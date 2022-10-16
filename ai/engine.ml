(*
Potential cards: 

Chapel: 2
Moat: 2 
Vassal, Village: 3
Militia: 4
Smithy: 4
Festival: 5

Buys:

On the first turn, buy a chapel and a militia.
Then:
- If you have 2, buy one moat. Otherwise, nothing. 
- If you have 3, alternate vassal/village, starting with vassal.
- If you have 4, buy one smithy, otherwise act like you have 3.
- If you have 5/6/7 and 1 buy, buy a festival
- If you have 6/7 and 2 buys, buy a vassal and village
- If you have 8, buy a province. 

Plays:
- Play any festivals
- Play any villages
- If >= 2 actions, play smithy or moat. 
- Play any vassals
- Play militia
- Play chapel (chapel estats always, chapel all coppers after 2nd reshuffle)

 *)

open Core
open Import

type t = { mutable cards : Card.t list; mutable total_buys : int }

let create () =
  {
    cards =
      List.init 7 ~f:(fun _ -> Card.Copper)
      @ List.init 3 ~f:(fun _ -> Card.Estate);
    total_buys = 0;
  }

(* Card tracking *)
let add_card t card = t.cards <- card :: t.cards
let trash_card t card = t.cards <- List.diff t.cards [ card ]

(* Play cards *)
let card_rank (card : Card.t) ~actions =
  match card with
  | Festival -> 0
  | Village -> 1
  | Smithy when actions >= 2 -> 2
  | Moat when actions >= 2 -> 3
  | Vassal -> 4
  | Militia -> 5
  | Chapel -> 6
  | Smithy -> 7
  | Moat -> 8
  | Copper -> 9
  | _ -> 10

let compare_rank c1 c2 ~actions =
  Int.compare (card_rank c1 ~actions) (card_rank c2 ~actions)

let play_chapel (t : t) (game_state : Game_state.t) =
  let estates = List.filter game_state.hand ~f:(Card.equal Estate) in
  let coppers = List.filter game_state.hand ~f:(Card.equal Copper) in
  let cards_to_trash =
    if t.total_buys >= 4 then estates @ coppers else estates
  in
  if List.is_empty cards_to_trash then None
  else Some (Play.Chapel (List.take cards_to_trash 4))

let rec play_from_topdeck t (game_state : Game_state.t) card =
  match card with Card.Chapel -> None | _ -> play_card t game_state card

and play_card (t : t) (game_state : Game_state.t) (card : Card.t) =
  let play (play : Play.t) = Some play in
  let do_not_play = None in
  match card with
  | Festival -> play Festival
  | Village -> play Village
  | Smithy -> play Smithy
  | Moat -> play Moat
  | Vassal -> play (Vassal { play_card_from_topdeck = play_from_topdeck t })
  | Militia -> play Militia
  | Chapel -> play_chapel t game_state
  | Copper -> play Copper
  | _ -> do_not_play

let can_play card ~(game_state : Game_state.t) =
  not (game_state.actions <= 0 && Card.is_action card)

let next_play (t : t) ~(game_state : Game_state.t) =
  let play =
    let ranked_hand =
      List.sort game_state.hand
        ~compare:(compare_rank ~actions:game_state.actions)
    in
    List.find_map ranked_hand ~f:(fun card ->
        if can_play card ~game_state then play_card t game_state card else None)
  in
  Option.iter play ~f:(function
    | Chapel trash -> List.iter trash ~f:(trash_card t)
    | _ -> ());
  play

let can_buy card ~(game_state : Game_state.t) =
  match Map.find game_state.supply card with
  | None -> false
  | Some count -> count > 0

let count_card t card = List.count t.cards ~f:(Card.equal card)

let buy_village_or_vassal t game_state =
  match (can_buy Village ~game_state, can_buy Vassal ~game_state) with
  | true, false -> Some Card.Village
  | false, true -> Some Card.Vassal
  | false, false ->
      failwith "BUG! Trying to buy village/vassal, but neither exist."
  | true, true ->
      if count_card t Vassal > count_card t Village then Some Card.Village
      else Some Card.Vassal

let next_buy t ~(game_state : Game_state.t) =
  let buy =
    if game_state.buys <= 0 then None
    else if t.total_buys < 2 then
      (* Hard-code the first two turns *)
      if game_state.treasure < 4 then Some Card.Chapel else Some Card.Militia
    else
      let c (card : Card.t) = can_buy card ~game_state in
      let v_v_strat = game_state.buys >= 2 && c Village && c Vassal in
      let v_strat = c Vassal || c Village in
      match game_state.treasure with
      | n when n >= 8 -> Some Card.Province
      | n when n >= 6 && v_v_strat -> buy_village_or_vassal t game_state
      | n when n >= 5 && c Festival -> Some Card.Festival
      | n when n >= 4 && count_card t Smithy < 1 && c Smithy -> Some Card.Smithy
      | n when n >= 3 && v_strat -> buy_village_or_vassal t game_state
      | n when n >= 2 && count_card t Moat < 1 && c Moat -> Some Card.Moat
      | _ -> None
  in
  Option.iter buy ~f:(add_card t);
  Option.iter buy ~f:(fun _ -> t.total_buys <- t.total_buys + 1);
  buy

let on_militia (_ : t) ~current_hand =
  let ranked_hand = List.sort current_hand ~compare:(compare_rank ~actions:1) in
  (* Discard whichever cards are not the three cards I'd play first. *)
  List.drop ranked_hand 3

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

let on_witch t = t.cards <- Card.Curse :: t.cards
