open Core
open Import

type t = { mutable cards : Card.t list }

let create () = { cards = [] }

(* "Cellar","Market","Merchant","Militia","Mine","Moat","Remodel","Smithy","Village","Workshop" *)

(* The single action card to play *)
let action_play (_ : t) ~(game_state : Game_state.t) =
  ignore game_state;
  Some Play.Smithy

let action_card = Card.Smithy

(* Card tracking *)
let add_card t card = t.cards <- card :: t.cards
let trash_card t card = t.cards <- List.diff t.cards [ card ]

let next_action (t : t) ~(game_state : Game_state.t) =
  if game_state.actions > 0 then
    Option.some_if
      (List.exists game_state.hand ~f:(Card.equal action_card))
      (action_play t ~game_state)
    |> Option.join
  else None

let next_treasure (_ : t) ~(game_state : Game_state.t) =
  List.find_map game_state.hand ~f:(function
    | Copper -> Some Play.Copper
    | Gold -> Some Gold
    | Silver -> Some Silver
    | _ -> None)

let next_play t ~game_state =
  match next_action t ~game_state with
  | None -> next_treasure t ~game_state
  | Some action -> Some action

let can_buy card ~(game_state : Game_state.t) =
  match Map.find game_state.supply card with
  | None -> false
  | Some count -> count > 0

let next_buy t ~(game_state : Game_state.t) =
  let buy =
    if game_state.buys <= 0 then None
    else
      let c (card : Card.t) = can_buy card ~game_state in
      match game_state.treasure with
      | n when n >= 8 && c Province -> Some Card.Province
      | n when n >= 6 && c Gold -> Some Gold
      (* This needs to change if we pick a more expensive action card. *)
      | n when n >= 4 && c action_card -> Some action_card
      | n when n >= 3 && c Silver -> Some Silver
      | _ -> None
  in
  Option.iter buy ~f:(add_card t);
  buy

let on_militia (_ : t) ~current_hand =
  let hand_highest_to_lowest =
    List.sort current_hand ~compare:(Comparable.lift Int.compare ~f:Card.cost)
    |> List.rev
  in
  (* Discard whichever cards are not the three highest value cards. *)
  List.drop hand_highest_to_lowest 3

let on_bureaucrat (_ : t) ~current_hand =
  (* Topdeck an arbitrary victory card, if it exists. *)
  List.find current_hand ~f:Card.is_victory

let on_bandit (t : t) ~top_two_cards =
  (* The only base cards that can be bandited are Silver and Gold.
     If we have a silver, trash that. Otherwise, trash the Gold. *)
  let trash =
    match List.find top_two_cards ~f:(Card.equal Card.Silver) with
    | None -> List.find top_two_cards ~f:(Card.equal Card.Gold)
    | Some silver -> Some silver
  in
  (* Track which cards exist in the deck correctly. *)
  Option.iter trash ~f:(trash_card t);
  trash
