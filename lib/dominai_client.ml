open Core

module Conn = struct
  type t = Websocket_lwt_unix.conn

  let connect ~(url : string) : t Lwt.t =
    let uri : Uri.t = Uri.of_string url in
    let%lwt (endp : Conduit.endp) =
      Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system
    in
    let ctx : Conduit_lwt_unix.ctx = Lazy.force Conduit_lwt_unix.default_ctx in
    let%lwt (client : Conduit_lwt_unix.client) =
      Conduit_lwt_unix.endp_to_client ~ctx endp
    in
    Websocket_lwt_unix.connect client uri

  let read (conn : t) : Jsonrpc.Packet.t Lwt.t =
    let%lwt (frame : Websocket.Frame.t) = Websocket_lwt_unix.read conn in
    Lwt.return
      (frame.Websocket.Frame.content |> Yojson.Safe.from_string
     |> Jsonrpc.Packet.t_of_yojson)

  let respond (conn : t) (packet : Jsonrpc.Response.t) : unit Lwt.t =
    let content = Yojson.Safe.to_string (Jsonrpc.Response.yojson_of_t packet) in
    let frame = Websocket.Frame.create ~content () in
    Websocket_lwt_unix.write conn frame

  let request (conn : t) (packet : Jsonrpc.Request.t) : unit Lwt.t =
    let content = Yojson.Safe.to_string (Jsonrpc.Request.yojson_of_t packet) in
    let frame = Websocket.Frame.create ~content () in
    Websocket_lwt_unix.write conn frame
end

type start_game = { kingdom : Card.t list; order : string list }
[@@deriving yojson]

module Supply = struct
  type t = int Card.Map.t [@@deriving sexp]

  let yojson_of_t supply : Yojson.Safe.t =
    `Assoc
      (Map.fold supply ~init:[] ~f:(fun ~key ~data acc ->
           (Card.yojson_of_t key |> Yojson.Safe.Util.to_string, `Int data)
           :: acc))

  let t_of_yojson (y : Yojson.Safe.t) : t =
    match y with
    | `Assoc alist ->
        List.map alist ~f:(fun (card, data) ->
            ( Card.of_string card,
              match data with
              | `Int data -> data
              | _ -> failwith "Malformed supply" ))
        |> Card.Map.of_alist_exn
    | _ -> failwith "Malformed supply"
end

type game_state = {
  hand : Card.t list;
  discard : int;
  deck : int;
  supply : Supply.t;
  trash : Card.t list;
  buys : int;
  actions : int;
  treasure : int;
  in_play : Card.t list;
  phase : string list;
}
[@@deriving yojson, sexp] [@@yojson.allow_extra_fields]

module Play_card = struct
  module Request = struct
    type without_data = { card : Card.t } [@@deriving yojson]
  end

  module Response = struct
    type t = game_state [@@deriving yojson, sexp]
  end
end

type end_turn = {
  hand : Card.t list;
  discard : int;
  deck : int;
  supply : Supply.t;
}
[@@deriving yojson, sexp] [@@yojson.allow_extra_fields]

type start_turn = game_state [@@deriving yojson, sexp]

let create_id () =
  let rand = List.permute (List.init 20 ~f:Fn.id) in
  String.concat (List.map rand ~f:Int.to_string)

let ok_response id = Jsonrpc.Response.ok id (yojson_of_unit ())

let read_response_exn ~conn ~id ~of_data =
  match%lwt Conn.read conn with
  | Response { result = Ok data; id = `String resp_id; _ }
    when String.equal id resp_id ->
      data |> of_data |> Lwt.return
  | _ -> failwith "Expected Ok response for most recent request"

let request_with_id ~conn ~request =
  let id = create_id () in
  let%lwt () = Conn.request conn (request ~id:(`String id)) in
  Lwt.return id

let _request_and_await_response ~conn ~request ~of_response_data =
  let%lwt id = request_with_id ~conn ~request in
  read_response_exn ~conn ~id ~of_data:of_response_data

let end_turn ~(conn : Conn.t) : unit Lwt.t =
  print_endline "Ending turn...";
  let%lwt _id =
    request_with_id ~conn ~request:(fun ~id ->
        Jsonrpc.Request.create ~id ~method_:"EndTurn" ())
    (* ~of_response_data:end_turn_of_yojson *)
  in
  Lwt.return ()

let make_decision ~hand =
  let (money : int) =
    List.sum
      (module Int)
      hand
      ~f:(function Card.Copper -> 1 | Silver -> 2 | Gold -> 3 | _ -> 0)
  in
  let cards_to_play = List.filter hand ~f:Card.is_treasure in
  let card_to_buy =
    if money > 6 then Card.Gold
    else if money > 3 then Card.Silver
    else Card.Copper
  in
  (cards_to_play, card_to_buy)

let play_card ~conn card : unit Lwt.t =
  printf "Playing a card: %s\n" (Card.to_string card);
  let%lwt _id =
    request_with_id ~conn ~request:(fun ~id ->
        let request = { Play_card.Request.card } in
        Jsonrpc.Request.create ~id ~method_:"Play"
          ~params:
            (Jsonrpc.Structured.t_of_yojson
               (Play_card.Request.yojson_of_without_data request))
          ())
  in
  Lwt.return ()
(*     ~of_response_data:Play_card.Response.t_of_yojson *)

let play_card_unit ~conn card =
  let%lwt _response = play_card ~conn card in
  Lwt.return ()

let _buy_card ~conn card : unit Lwt.t =
  printf "Buying a card: %s\n" (Card.to_string card);
  let%lwt _id =
    request_with_id ~conn ~request:(fun ~id ->
        let request = { Play_card.Request.card } in
        Jsonrpc.Request.create ~id ~method_:"Buy"
          ~params:
            (Jsonrpc.Structured.t_of_yojson
               (Play_card.Request.yojson_of_without_data request))
          ())
    (*     ~of_response_data:game_state_of_yojson *)
  in
  Lwt.return ()

let rec play_cards ~conn ~cards =
  match cards with
  | [] -> Lwt.return ()
  | card :: cards ->
      print_endline "I got here???";
      let%lwt () = play_card_unit ~conn card in
      play_cards ~conn ~cards

let play_turn ~(conn : Conn.t) ~(start_turn : start_turn) : unit Lwt.t =
  let cards_to_play, card_to_buy = make_decision ~hand:start_turn.hand in
  print_s [%sexp Decision { cards_to_play : Card.t list; card_to_buy : Card.t }];
  let%lwt () = play_cards ~conn ~cards:cards_to_play in
  (* let%lwt _response = buy_card ~conn card_to_buy in *)
  let%lwt _response = end_turn ~conn in
  Lwt.return ()

let rec play_turns ~(conn : Conn.t) : unit Lwt.t =
  match%lwt Conn.read conn with
  | Jsonrpc.Packet.Notification
      { method_ = "StartTurn"; params = Some params; _ } ->
      print_endline "It's my turn! Deciding what to do...";
      let start_turn =
        params |> Jsonrpc.Structured.yojson_of_t |> start_turn_of_yojson
      in
      let%lwt () = play_turn ~conn ~start_turn in
      print_endline "Waiting for my turn to come around...";
      play_turns ~conn
  | Notification { method_ = "GameOver"; _ } ->
      print_endline "Game Over!";
      Lwt.return ()
  | malformed ->
      Printf.eprintf "Was expecting StartTurn or GameOver but received %s"
        (malformed |> Jsonrpc.Packet.yojson_of_t |> Yojson.Safe.to_string);
      failwith "Malformed"

let play ~(conn : Conn.t) : unit Lwt.t =
  print_endline "Connected to server. Waiting for game to start ...";
  let%lwt { kingdom; order } =
    match%lwt Conn.read conn with
    | Jsonrpc.Packet.Request
        Jsonrpc.Request.{ method_ = "StartGame"; params = Some params; id; _ }
      ->
        let start_game =
          params |> Jsonrpc.Structured.yojson_of_t |> start_game_of_yojson
        in
        let%lwt () = Conn.respond conn (ok_response id) in
        Lwt.return start_game
    | malformed ->
        Printf.eprintf "Was expecting StartGame but received %s"
          (malformed |> Jsonrpc.Packet.yojson_of_t |> Yojson.Safe.to_string);
        failwith "Malformed"
  in
  print_endline "The game has begun!";
  printf "Kingdom : %s\n" (Sexp.to_string (sexp_of_list Card.sexp_of_t kingdom));
  printf "Turn Order: %s\n" (Sexp.to_string (sexp_of_list Sexp.of_string order));
  let%lwt () = play_turns ~conn in
  Lwt.return ()

let main (url : string) : unit Lwt.t =
  let%lwt conn = Conn.connect ~url in
  play ~conn

let params : (unit -> unit) Command.Param.t =
  let open Command.Param in
  let open Command.Let_syntax in
  let%map url = "url" %: string |> anon in
  fun () -> Lwt_main.run (main url)

let command =
  Command.basic
    ~summary:"Run a simple Dominion AI against the specified game server" params
