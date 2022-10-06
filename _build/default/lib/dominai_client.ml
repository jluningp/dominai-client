open Core

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
  buy_coppers : bool;
  mutable deck : Card.t list;
  mutable discard : Card.t list;
  mutable playing_card : (Play.t * unit Lwt.u) option;
  mutable hand : Card.t list;
}

let create ~conn ~buy_coppers =
  {
    conn;
    deck =
      [
        Card.Copper;
        Copper;
        Copper;
        Copper;
        Copper;
        Copper;
        Copper;
        Estate;
        Estate;
        Estate;
      ];
    discard = [];
    buy_coppers;
    playing_card = None;
    hand = [];
  }

let have_card ?(count = 1) t ~card =
  List.count (t.deck @ t.discard) ~f:(Card.equal card) >= count

let make_decision t ~hand ~supply:_ =
  print_s [%sexp (hand : Card.t list)];
  let money = List.sum (module Int) hand ~f:Card.value_as_treasure in
  let cards_to_play =
    List.filter_map hand ~f:(function
      | Gold -> Some Play.Gold
      | Silver -> Some Silver
      | Copper -> Some Copper
      | _ -> None)
  in
  let cards_to_play =
    if List.exists hand ~f:(function Card.Harbinger -> true | _ -> false) then
      let card_to_topdeck ~discard = List.hd_exn discard in
      Play.Harbinger { card_to_topdeck } :: cards_to_play
    else cards_to_play
  in
  let cards_to_buy =
    if money >= 8 then [ Card.Province ]
    else if money >= 6 then [ Card.Gold ]
    else if money >= 3 then
      if have_card t ~count:15 ~card:Harbinger then [ Card.Silver ]
      else [ Card.Harbinger ]
    else if t.buy_coppers then [ Card.Copper ]
    else []
  in
  (cards_to_play, cards_to_buy)

(*
let request_and_await_response ~conn ~create_request ~of_response_data =
  let%lwt id = Request.send ~conn ~create_request in
  Response.read_with_id ~conn ~id ~of_data:of_response_data
*)

module Play_requests = struct
  module Harbinger = struct
    type t = { discard : Card.t list }
    [@@deriving yojson] [@@yojson.allow_extra_fields]
  end
end

let end_turn (t : t) : Game_state.t Lwt.t =
  print_endline "Ending turn...";
  let%lwt response = Request.dispatch ~conn:t.conn End_turn in
  match response.result with
  | Error _ -> failwith "Error code from end turn"
  | Ok data ->
      let game_state = Game_state.t_of_yojson data in
      Lwt.return game_state

let play_card (t : t) card : unit Lwt.t =
  printf "Playing a card: %s\n" (Card.to_string (Play.to_card card));
  let promise, resolver = Lwt.wait () in
  let%lwt (_ : Jsonrpc.Response.t) =
    Request.dispatch ~conn:t.conn (Play card)
  in
  (match card with
  | Harbinger _ when not (List.is_empty t.discard) ->
      t.playing_card <- Some (card, resolver)
  | _ -> Lwt.wakeup resolver ());
  promise

let buy_card (t : t) card : unit Lwt.t =
  printf "Buying a card: %s\n" (Card.to_string card);
  let%lwt (_ : Jsonrpc.Response.t) =
    Request.dispatch ~conn:t.conn (Buy { card })
  in
  t.discard <- card :: t.discard;
  Lwt.return ()

let diff l1 l2 =
  let rec remove_elt l v =
    match l with
    | [] -> []
    | e :: ls -> if Card.equal e v then ls else e :: remove_elt ls v
  in
  List.fold l2 ~init:l1 ~f:remove_elt

let cleanup t ~old_hand ~new_hand =
  t.discard <- old_hand @ t.discard;
  if List.length t.deck < 5 then (
    t.deck <- t.deck @ t.discard;
    t.discard <- []);
  t.deck <- diff t.deck new_hand;
  t.hand <- new_hand;
  let { deck; discard; hand; _ } = t in
  print_s
    [%sexp { deck : Card.t list; discard : Card.t list; hand : Card.t list }]

let play_turn t ~(game_state : Game_state.t) : unit Lwt.t =
  let hand = game_state.hand in
  let cards_to_play, cards_to_buy =
    make_decision t ~hand ~supply:game_state.supply
  in
  print_s
    [%sexp Decision { cards_to_play : Play.t list; cards_to_buy : Card.t list }];
  let%lwt () = Lwt_list.iter_s (play_card t) cards_to_play in
  let%lwt () = Lwt_list.iter_s (buy_card t) cards_to_buy in
  let%lwt (game_state : Game_state.t) = end_turn t in
  cleanup t ~old_hand:hand ~new_hand:game_state.hand;
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
      ignore
        (let%lwt () = play_turn t ~game_state in
         print_endline "Waiting for my turn to come around...";
         Lwt.return ())
  | "GameOver" ->
      print_endline "Game Over!";
      printf "%s\n"
        (notification |> Jsonrpc.Notification.yojson_of_t
       |> Yojson.Safe.to_string);
      Lwt.wakeup game_over ()
  | _ -> printf "Unknown notification: %s" method_

module Attack = struct
  module Bureaucrat_response = struct
    type t = { data : string } [@@deriving yojson]
  end

  module Militia_response = struct
    type t = { data : Card.t list } [@@deriving yojson]
  end

  module Cards = struct
    type t = Card.t list [@@deriving yojson]
  end

  type yojson = Yojson.Safe.t

  let yojson_of_yojson = Fn.id

  type t = { card : Card.t; data : yojson option [@yojson.option] }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

let on_request (t : t) (request : Jsonrpc.Request.t) : Jsonrpc.Response.t =
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
      Response.create_ok id
  | "Harbinger" -> (
      print_endline "Received a Harbinger query...";
      let params = Option.value_exn params in
      let { Play_requests.Harbinger.discard } =
        Request.Util.parse_params params
          ~of_yojson:Play_requests.Harbinger.t_of_yojson
      in
      match t.playing_card with
      | Some (Harbinger { card_to_topdeck }, resolver) ->
          let topdeck = card_to_topdeck ~discard in
          Lwt.wakeup resolver ();
          t.playing_card <- None;
          Jsonrpc.Response.ok id
            (Request_card.yojson_of_t { Request_card.card = topdeck })
      | _ -> failwith "Got Harbinger request while not playing harbinger")
  | "Attack" -> (
      let params = Option.value_exn params in
      let { Attack.card; data } =
        Request.Util.parse_params params ~of_yojson:Attack.t_of_yojson
      in
      match card with
      | Bureaucrat ->
          (* Need to keep track of my current deck. *)
          Jsonrpc.Response.ok id
            (Attack.Bureaucrat_response.yojson_of_t { data = "reveal" })
      | Militia ->
          Jsonrpc.Response.ok id
            (Attack.Militia_response.yojson_of_t
               { data = [ Copper; Copper; Copper ] })
      | Witch ->
          t.discard <- Witch :: t.discard;
          Jsonrpc.Response.ok id (yojson_of_unit ())
      | Bandit ->
          let data = Option.value_exn data in
          let _top_two_cards = Attack.Cards.t_of_yojson data in
          Jsonrpc.Response.ok id
            (Attack.Bureaucrat_response.yojson_of_t { data = "null" })
      | card ->
          printf "Received Attack request with non-attack card: %s"
            (Card.to_string card);
          Jsonrpc.Response.error id
            (Jsonrpc.Response.Error.make ~code:InvalidRequest
               ~message:"Attacked with unknown attack card" ()))
  (* Add requests for attacks and cards here. *)
  | _ -> failwith "Unsupported"

let main (url : string) ~buy_coppers : unit Lwt.t =
  let run_until, game_over = Lwt.wait () in
  let tref = ref None in
  let%lwt conn =
    Conn.create ~url
      ~on_request:(fun req -> on_request (Option.value_exn !tref) req)
      ~on_notification:(fun notif ->
        on_notification (Option.value_exn !tref) notif ~game_over)
  in
  let t = create ~conn ~buy_coppers in
  tref := Some t;
  run_until

let params : (unit -> unit) Command.Param.t =
  let open Command.Param in
  let open Command.Let_syntax in
  let%map url = flag "-url" (required string) ~doc:""
  and buy_coppers = flag "-buy-coppers" no_arg ~doc:"" in
  fun () -> Lwt_main.run (main url ~buy_coppers)

let command =
  Command.basic
    ~summary:"Run a simple Dominion AI against the specified game server" params
