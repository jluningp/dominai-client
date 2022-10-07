open Core
open Import

module Request = struct
  module Util = struct
    let parse_params params ~of_yojson =
      params |> Jsonrpc.Structured.yojson_of_t |> of_yojson
  end

  type t = Play of Play.t | Buy of Request_card.t | End_turn

  let method_ = function
    | Play _ -> "Play"
    | Buy _ -> "Buy"
    | End_turn -> "EndTurn"

  let params t =
    let%map.Option yojson =
      match t with
      | Play data -> Some (Play.yojson_of_t data)
      | Buy data -> Some (Request_card.yojson_of_t data)
      | End_turn -> None
    in
    Jsonrpc.Structured.t_of_yojson yojson

  let create t id =
    Jsonrpc.Request.create ~id ~method_:(method_ t) ?params:(params t) ()

  let dispatch t ~conn = Conn.dispatch conn ~with_request:(create t)
end

module Response = struct
  let create_ok id = Jsonrpc.Response.ok id (yojson_of_unit ())
end

type t = {
  conn : Conn.t;
  play_request : Jsonrpc.Request.t Lwt_mvar.t;
  play_response : Jsonrpc.Response.t Lwt_mvar.t;
  ai : Ai.t;
  mutable current_hand : Card.t list;
}

let create ~conn ~ai =
  {
    conn;
    ai;
    play_request = Lwt_mvar.create_empty ();
    play_response = Lwt_mvar.create_empty ();
    current_hand = [];
  }

module Play_requests = struct
  module Harbinger = struct
    type t = { discard : Card.t list }
    [@@deriving yojson] [@@yojson.allow_extra_fields]
  end

  module Vassal = struct
    type t = { card : Card.t } [@@deriving yojson] [@@yojson.allow_extra_fields]
  end

  module Poacher = struct
    type t = { hand : Card.t list; empty_supply_piles : int }
    [@@deriving yojson] [@@yojson.allow_extra_fields]
  end

  module Library = struct
    type t = { card : Card.t; hand : Card.t list }
    [@@deriving yojson] [@@yojson.allow_extra_fields]
  end

  module Sentry = struct
    type t = { hand : Card.t list; cards : Card.t list }
    [@@deriving yojson] [@@yojson.allow_extra_fields]
  end
end

let end_turn (t : t) : unit Lwt.t =
  print_endline "Ending turn...";
  let%lwt response = Request.dispatch ~conn:t.conn End_turn in
  match response.result with
  | Error _ -> Lwt.return ()
  | Ok data ->
      let data = Game_state.End_of_turn.t_of_yojson data in
      t.current_hand <- data.hand;
      Lwt.return ()

let on_harbinger t ~(game_state : Game_state.t) ~card_to_topdeck =
  if game_state.discard <= 0 then Lwt.return ()
  else
    let%lwt request = Lwt_mvar.take t.play_request in
    let { Jsonrpc.Request.params; id; _ } = request in
    let params = Option.value_exn params in
    let { Play_requests.Harbinger.discard } =
      Request.Util.parse_params params
        ~of_yojson:Play_requests.Harbinger.t_of_yojson
    in
    let topdeck = card_to_topdeck ~discard in
    let response =
      Jsonrpc.Response.ok id
        (Request_card.yojson_of_t { Request_card.card = topdeck })
    in
    Lwt_mvar.put t.play_response response

module Resp = struct
  type t = { play : bool; data : Play.just_data } [@@deriving yojson]
  type no_play = { play : bool; data : string } [@@deriving yojson]
  type discard = { discard : Card.t list } [@@deriving yojson]
  type skip = { skip : bool } [@@deriving yojson]

  (* placement = "trash" | "discard" | "topdeck" *)
  type sentry_card = { card : Card.t; placement : string } [@@deriving yojson]
  type sentry = sentry_card list [@@deriving yojson]
  type rematch = { rematch : bool } [@@deriving yojson]
end

let take_request_after t ~seconds =
  let promise, resolver = Lwt.wait () in
  Lwt_timeout.create seconds (fun () ->
      Lwt.wakeup resolver (Lwt_mvar.take_available t.play_request))
  |> Lwt_timeout.start;
  promise

let on_poacher t ~(game_state : Game_state.t) ~cards_to_discard =
  let has_empty_supply_piles =
    Map.exists game_state.supply ~f:(fun n -> n > 0)
  in
  if not has_empty_supply_piles then Lwt.return ()
  else
    let%lwt request = Lwt_mvar.take t.play_request in
    let { Jsonrpc.Request.params; id; _ } = request in
    let params = Option.value_exn params in
    let { Play_requests.Poacher.hand; empty_supply_piles } =
      Request.Util.parse_params params
        ~of_yojson:Play_requests.Poacher.t_of_yojson
    in
    let discard =
      cards_to_discard ~number_to_discard:empty_supply_piles ~hand
    in
    let response =
      Jsonrpc.Response.ok id (Resp.yojson_of_discard { discard })
    in
    Lwt_mvar.put t.play_response response

let on_throne_room t ~game_state:_ ~card_to_play =
  let%lwt request = Lwt_mvar.take t.play_request in
  let { Jsonrpc.Request.id; _ } = request in
  let response =
    Jsonrpc.Response.ok id (Play.yojson_of_just_data card_to_play)
  in
  Lwt_mvar.put t.play_response response

let on_sentry t ~game_state:_ ~what_to_do =
  let%lwt request = Lwt_mvar.take t.play_request in
  let { Jsonrpc.Request.params; id; _ } = request in
  let params = Option.value_exn params in
  let { Play_requests.Sentry.hand; cards } =
    Request.Util.parse_params params ~of_yojson:Play_requests.Sentry.t_of_yojson
  in
  let to_do = what_to_do ~hand ~top_two_cards:cards in
  let response =
    Jsonrpc.Response.ok id
      (Resp.yojson_of_sentry
         (List.map to_do ~f:(fun (card, to_do) ->
              match to_do with
              | `Trash -> { Resp.card; placement = "trash" }
              | `Topdeck -> { Resp.card; placement = "topdeck" }
              | `Discard -> { Resp.card; placement = "discard" })))
  in
  Lwt_mvar.put t.play_response response

let rec on_library t ~game_state ~skip_action_card =
  let%lwt request = take_request_after t ~seconds:1 in
  match request with
  | None -> Lwt.return ()
  | Some request ->
      let { Jsonrpc.Request.params; id; _ } = request in
      let params = Option.value_exn params in
      let { Play_requests.Library.hand; card } =
        Request.Util.parse_params params
          ~of_yojson:Play_requests.Library.t_of_yojson
      in
      let skip =
        if Card.is_treasure card then false else skip_action_card ~hand card
      in
      let response = Jsonrpc.Response.ok id (Resp.yojson_of_skip { skip }) in
      let%lwt () = Lwt_mvar.put t.play_response response in
      on_library t ~game_state ~skip_action_card

let rec on_vassal t ~game_state ~play_card_from_topdeck =
  let%lwt request = take_request_after t ~seconds:1 in
  match request with
  | None -> Lwt.return ()
  | Some request -> (
      let { Jsonrpc.Request.params; id; _ } = request in
      let params = Option.value_exn params in
      let { Play_requests.Vassal.card } =
        Request.Util.parse_params params
          ~of_yojson:Play_requests.Vassal.t_of_yojson
      in
      let play = play_card_from_topdeck game_state card in
      let response =
        match play with
        | None -> Resp.yojson_of_no_play { play = false; data = "null" }
        | Some play -> Resp.yojson_of_t { play = true; data = play }
      in
      let response = Jsonrpc.Response.ok id response in
      let%lwt () = Lwt_mvar.put t.play_response response in
      match play with
      | None -> Lwt.return ()
      | Some play -> handle_card_specific_request t play ~game_state)

and handle_card_specific_request (t : t) card ~game_state =
  match card with
  | Play.Harbinger { card_to_topdeck } ->
      on_harbinger t ~game_state ~card_to_topdeck
  | Vassal { play_card_from_topdeck } ->
      on_vassal t ~game_state ~play_card_from_topdeck
  | Poacher { cards_to_discard } -> on_poacher t ~game_state ~cards_to_discard
  | ThroneRoom { data; _ } -> on_throne_room t ~game_state ~card_to_play:data
  | Library { skip_action_card } -> on_library t ~game_state ~skip_action_card
  | Sentry { what_to_do } -> on_sentry t ~game_state ~what_to_do
  | _ -> Lwt.return ()

let play_card (t : t) card ~(game_state : Game_state.t) : Game_state.t Lwt.t =
  ignore game_state;
  printf "Playing a card: %s\n" (Card.to_string (Play.to_card card));
  let response = Request.dispatch ~conn:t.conn (Play card) in
  let%lwt () = handle_card_specific_request t card ~game_state in
  let%lwt response = response in
  match response.result with
  | Error err -> Jsonrpc.Response.Error.raise err
  | Ok game_state -> Game_state.t_of_yojson game_state |> Lwt.return

let buy_card (t : t) card : Game_state.t Lwt.t =
  printf "Buying a card: %s\n" (Card.to_string card);
  let%lwt (response : Jsonrpc.Response.t) =
    Request.dispatch ~conn:t.conn (Buy { card })
  in
  match response.result with
  | Error err -> Jsonrpc.Response.Error.raise err
  | Ok game_state -> Game_state.t_of_yojson game_state |> Lwt.return

let rec play_cards t ~game_state =
  match Ai.next_play t.ai ~game_state with
  | None -> Lwt.return game_state
  | Some play ->
      let%lwt new_game_state = play_card t play ~game_state in
      play_cards t ~game_state:new_game_state

let rec buy_cards t ~game_state =
  match Ai.next_buy t.ai ~game_state with
  | None -> Lwt.return game_state
  | Some card ->
      let%lwt new_game_state = buy_card t card in
      buy_cards t ~game_state:new_game_state

let play_turn t ~(game_state : Game_state.t) : unit Lwt.t =
  let%lwt game_state = play_cards t ~game_state in
  let%lwt (_ : Game_state.t) = buy_cards t ~game_state in
  let%lwt () = end_turn t in
  Lwt.return ()

let on_notification (t : t) (notification : Jsonrpc.Notification.t) ~game_over =
  let { Jsonrpc.Notification.method_; params; _ } = notification in
  match method_ with
  | "StartTurn" ->
      print_endline "It's my turn! Deciding what to do...";
      let params = Option.value_exn params in
      let game_state =
        Request.Util.parse_params params ~of_yojson:Game_state.t_of_yojson
      in
      Lwt.async (fun () ->
          let%lwt () = play_turn t ~game_state in
          print_endline "Waiting for my turn to come around...";
          Lwt.return ())
  | "GameOver" ->
      print_endline "Game Over!";
      printf "%s\n"
        (notification |> Jsonrpc.Notification.yojson_of_t
       |> Yojson.Safe.to_string);
      Lwt.wakeup game_over ()
  | _ -> printf "Unknown notification: %s\n" method_

let on_attack (t : t) (id : Jsonrpc.Id.t) (attack : Attack.Request.t) :
    Jsonrpc.Response.t =
  let { Attack.Request.card; data } = attack in
  printf "Attacked! %s\n" (Card.to_string card);
  let ok ?reaction ?data () =
    let body =
      match (reaction, data) with
      | Some reaction, _ -> Attack.Response.Reaction.yojson_of_t { reaction }
      | _, Some data -> Attack.Response.Attack_succeeded.yojson_of_t { data }
      | None, None -> yojson_of_unit ()
    in
    Jsonrpc.Response.ok id body
  in
  match List.find t.current_hand ~f:(Card.equal Card.Moat) with
  | Some _ -> ok ~reaction:Moat ()
  | None -> (
      match card with
      | Bureaucrat -> (
          match Ai.on_bureaucrat t.ai ~current_hand:t.current_hand with
          | None -> ok ~data:(Bureaucrat Reveal) ()
          | Some victory ->
              t.current_hand <- List.diff t.current_hand [ victory ];
              ok ~data:(Bureaucrat (Card victory)) ())
      | Militia ->
          let discard = Ai.on_militia t.ai ~current_hand:t.current_hand in
          t.current_hand <- List.diff t.current_hand discard;
          ok ~data:(Militia discard) ()
      | Witch -> ok ()
      | Bandit ->
          let (Bandit top_two_cards) = Option.value_exn data in
          let trash = Ai.on_bandit t.ai ~top_two_cards in
          ok ~data:(Bandit trash) ()
      | card ->
          printf "Received Attack request with non-attack card: %s"
            (Card.to_string card);
          Jsonrpc.Response.error id
            (Jsonrpc.Response.Error.make ~code:InvalidRequest
               ~message:"Attacked with unknown attack card" ()))

let on_request (t : t) (request : Jsonrpc.Request.t) ~game_over :
    Jsonrpc.Response.t Lwt.t =
  let { Jsonrpc.Request.method_; params; id; _ } = request in
  match method_ with
  | "StartGame" ->
      let params = Option.value_exn params in
      let { Start_game.kingdom; order } =
        Request.Util.parse_params params ~of_yojson:Start_game.t_of_yojson
      in
      print_endline "The game has begun!";
      printf "Kingdom : %s\n"
        (Sexp.to_string (sexp_of_list Card.sexp_of_t kingdom));
      printf "Turn Order: %s\n"
        (Sexp.to_string (sexp_of_list Sexp.of_string order));
      printf "%!";
      Response.create_ok id |> Lwt.return
  | "Harbinger" | "Vassal" | "Poacher" | "ThroneRoom" | "Library" | "Sentry" ->
      let%lwt () = Lwt_mvar.put t.play_request request in
      Lwt_mvar.take t.play_response
  | "Attack" ->
      let params = Option.value_exn params in
      let attack =
        Request.Util.parse_params params ~of_yojson:Attack.Request.t_of_yojson
      in
      Lwt.return (on_attack t id attack)
  | "GameOver" ->
      print_endline "Game Over!";
      printf "%s\n"
        (request |> Jsonrpc.Request.yojson_of_t |> Yojson.Safe.to_string);
      let rematch = false in
      Lwt.wakeup game_over ();
      Jsonrpc.Response.ok id (Resp.yojson_of_rematch { rematch }) |> Lwt.return
  (* Add requests for attacks and cards here. *)
  | _ -> failwith "Unknown request"

let main (url : string) (log_traffic : bool) (always_militia : bool) :
    unit Lwt.t =
  let run_until, game_over = Lwt.wait () in
  let tref = ref None in
  let%lwt conn =
    Conn.create ~url
      ~on_request:(fun req ->
        on_request (Option.value_exn !tref) req ~game_over)
      ~on_notification:(fun notif ->
        on_notification (Option.value_exn !tref) notif ~game_over)
      ~log_traffic
  in
  let ai = Ai.create ~always_militia in
  let t = create ~conn ~ai in
  tref := Some t;
  run_until

let params : (unit -> unit) Command.Param.t =
  let open Command.Param in
  let open Command.Let_syntax in
  let%map url = flag "-url" (required string) ~doc:""
  and always_militia = flag "-always-militia" no_arg ~doc:""
  and log_traffic = flag "-log-traffic" no_arg ~doc:"" in
  fun () -> Lwt_main.run (main url log_traffic always_militia)

let command =
  Command.basic
    ~summary:"Run a simple Dominion AI against the specified game server" params
