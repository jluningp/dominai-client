open Core

module List = struct
  include List

  let diff l1 l2 =
    let rec remove_elt l v =
      match l with
      | [] -> []
      | e :: ls -> if Card.equal e v then ls else e :: remove_elt ls v
    in
    List.fold l2 ~init:l1 ~f:remove_elt
end

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

(* Harbinger:
      let%lwt () =
     match card with
     | Harbinger { card_to_topdeck } when game_state.discard > 0 ->
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
     | Harbinger _ ->
         print_endline "I don't think my discard has cards in it...";
         Lwt.return ()
     | _ -> Lwt.return ()
   in
*)

let play_card (t : t) card ~(game_state : Game_state.t) : Game_state.t Lwt.t =
  ignore game_state;
  printf "Playing a card: %s\n" (Card.to_string (Play.to_card card));
  let%lwt response = Request.dispatch ~conn:t.conn (Play card) in
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
  let response =
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
            match List.find t.current_hand ~f:Card.is_victory with
            | None -> ok ~data:(Bureaucrat Reveal) ()
            | Some victory ->
                t.current_hand <- List.diff t.current_hand [ victory ];
                ok ~data:(Bureaucrat (Card victory)) ())
        | Militia ->
            print_endline "Received militia attack request";
            let sorted_hand =
              List.sort t.current_hand
                ~compare:(Comparable.lift Int.compare ~f:Card.cost)
              |> List.rev
            in
            let last_3, first_n = List.split_n sorted_hand 3 in
            t.current_hand <- last_3;
            ok ~data:(Militia first_n) ()
        | Witch -> ok ()
        | Bandit ->
            let (Bandit top_two_cards) = Option.value_exn data in
            let sorted_top =
              top_two_cards
              |> List.filter ~f:(function
                   | Card.Silver | Gold -> true
                   | _ -> false)
              |> List.sort ~compare:(Comparable.lift Int.compare ~f:Card.cost)
            in
            let bandit =
              match sorted_top with least :: _ -> Some least | [] -> None
            in
            ok ~data:(Bandit bandit) ()
        | card ->
            printf "Received Attack request with non-attack card: %s"
              (Card.to_string card);
            Jsonrpc.Response.error id
              (Jsonrpc.Response.Error.make ~code:InvalidRequest
                 ~message:"Attacked with unknown attack card" ()))
  in
  printf "%s\n" (Jsonrpc.Response.yojson_of_t response |> Yojson.Safe.to_string);
  printf "%!";
  response

let on_request (t : t) (request : Jsonrpc.Request.t) : Jsonrpc.Response.t Lwt.t
    =
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
  | "Harbinger" ->
      print_endline "Received a Harbinger query...";
      let%lwt () = Lwt_mvar.put t.play_request request in
      Lwt_mvar.take t.play_response
  | "Attack" ->
      let params = Option.value_exn params in
      let attack =
        Request.Util.parse_params params ~of_yojson:Attack.Request.t_of_yojson
      in
      Lwt.return (on_attack t id attack)
  (* Add requests for attacks and cards here. *)
  | _ -> failwith "Unknown request"

let main (url : string) (always_militia : bool) : unit Lwt.t =
  let run_until, game_over = Lwt.wait () in
  let tref = ref None in
  let%lwt conn =
    Conn.create ~url
      ~on_request:(fun req -> on_request (Option.value_exn !tref) req)
      ~on_notification:(fun notif ->
        on_notification (Option.value_exn !tref) notif ~game_over)
  in
  let ai = Ai.create ~always_militia in
  let t = create ~conn ~ai in
  tref := Some t;
  run_until

let params : (unit -> unit) Command.Param.t =
  let open Command.Param in
  let open Command.Let_syntax in
  let%map url = flag "-url" (required string) ~doc:""
  and always_militia = flag "-always-militia" no_arg ~doc:"" in
  fun () -> Lwt_main.run (main url always_militia)

let command =
  Command.basic
    ~summary:"Run a simple Dominion AI against the specified game server" params
