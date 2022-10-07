open Core

type t = { mutable cards : Card.t list; militia : bool }

let create ~always_militia = { cards = []; militia = always_militia }

let next_money ~(game_state : Game_state.t) =
  List.find_map game_state.hand ~f:(function
    | Copper -> Some Play.Copper
    | Gold -> Some Gold
    | Silver -> Some Silver
    | _ -> None)

let next_action ~(game_state : Game_state.t) =
  if game_state.actions <= 0 then None
  else
    List.find_map game_state.hand ~f:(function
      | Militia -> Some Play.Militia
      | _ -> None)

let next_play (_ : t) ~(game_state : Game_state.t) =
  match next_action ~game_state with
  | Some action -> Some action
  | None -> next_money ~game_state

let can_buy card ~(game_state : Game_state.t) =
  match Map.find game_state.supply card with
  | None -> false
  | Some count -> count > 0

let militias t = List.count t.cards ~f:(function Militia -> true | _ -> false)

let next_buy t ~(game_state : Game_state.t) =
  let buy =
    if game_state.buys <= 0 then None
    else
      let c (card : Card.t) = can_buy card ~game_state in
      match game_state.treasure with
      | n when n >= 8 && c Province -> Some Card.Province
      | n when n >= 6 && c Gold -> Some Gold
      | n when n >= 4 && c Militia && t.militia && militias t < 2 ->
          Some Militia
      | n when n >= 3 && c Silver -> Some Silver
      | _ -> None
  in
  match buy with
  | None -> None
  | Some buy ->
      t.cards <- buy :: t.cards;
      Some buy
