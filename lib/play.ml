open Core

module Cellar = struct
  type t = { card : Card.t; data : Card.t list } [@@deriving yojson, sexp]
end

module Chapel = struct
  type t = { card : Card.t; data : Card.t list } [@@deriving yojson, sexp]
end

module Workshop = struct
  type t = { card : Card.t; data : Card.t } [@@deriving yojson, sexp]
end

module Moneylender = struct
  type t = { card : Card.t; data : bool } [@@deriving yojson, sexp]
end

module Remodel = struct
  module Data = struct
    type t = { trash : Card.t; gain : Card.t } [@@deriving yojson, sexp]
  end

  type t = { card : Card.t; data : Data.t } [@@deriving yojson, sexp]
end

module Throne_room = struct
  module Data = struct
    type yojson = Yojson.Safe.t

    let yojson_of_yojson = Fn.id
    let sexp_of_yojson t = Sexp.Atom (Yojson.Safe.to_string t)

    let yojson_of_sexp = function
      | Sexp.Atom str -> Yojson.Safe.from_string str
      | _ -> failwith "Invalid yojson sexp"

    type t = { card : Card.t; data : yojson (* Data for playing the card. *) }
    [@@deriving yojson, sexp]
  end

  type t = { card : Card.t; data : Data.t } [@@deriving yojson, sexp]
end

module Mine = struct
  module Data = struct
    type t = { trash : Card.t; gain : Card.t } [@@deriving yojson, sexp]
  end

  type t = { card : Card.t; data : Data.t } [@@deriving yojson, sexp]
end

module Artisan = struct
  module Data = struct
    type t = { trash : Card.t; topdeck : Card.t } [@@deriving yojson, sexp]
  end

  type t = { card : Card.t; data : Data.t } [@@deriving yojson, sexp]
end

module Harbinger = struct
  type t = { card_to_topdeck : discard:Card.t list -> Card.t [@sexp.opaque] }
  [@@deriving sexp]
end

type t =
  (* TREASURE CARDS *)
  | Copper
  | Silver
  | Gold
  (* ACTION CARDS *)
  | Cellar of Cellar.t
  | Chapel of Chapel.t
  | Moat
  | Merchant
  | Village
  | Workshop of Workshop.t
  | Bureaucrat
  | Militia
  | Moneylender of Moneylender.t
  | Remodel of Remodel.t
  | Smithy
  | Bandit
  | CouncilRoom
  | Festival
  | Laboratory
  | Market
  | Mine of Mine.t
  | Witch
  | Artisan of Artisan.t
  (* Require extra rpc calls *)
  | Harbinger of Harbinger.t
  | Vassal
  | Poacher
  | ThroneRoom of Throne_room.t
  | Library
  | Sentry
[@@deriving sexp]

let to_card = function
  | Copper -> Card.Copper
  | Silver -> Silver
  | Gold -> Gold
  | Cellar _ -> Cellar
  | Chapel _ -> Chapel
  | Moat -> Moat
  | Harbinger _ -> Harbinger
  | Merchant -> Merchant
  | Vassal -> Vassal
  | Village -> Village
  | Workshop _ -> Workshop
  | Bureaucrat -> Bureaucrat
  | Militia -> Militia
  | Moneylender _ -> Moneylender
  | Poacher -> Poacher
  | Remodel _ -> Remodel
  | Smithy -> Smithy
  | ThroneRoom _ -> ThroneRoom
  | Bandit -> Bandit
  | CouncilRoom -> CouncilRoom
  | Festival -> Festival
  | Laboratory -> Laboratory
  | Library -> Library
  | Market -> Market
  | Mine _ -> Mine
  | Sentry -> Sentry
  | Witch -> Witch
  | Artisan _ -> Artisan

let yojson_of_t = function
  | ( Copper | Silver | Gold | Moat | Harbinger _ | Merchant | Vassal | Village
    | Bureaucrat | Militia | Poacher | Smithy | Bandit | CouncilRoom | Festival
    | Laboratory | Library | Market | Sentry | Witch ) as card ->
      Request_card.yojson_of_t { Request_card.card = to_card card }
  | Cellar request -> Cellar.yojson_of_t request
  | Chapel request -> Chapel.yojson_of_t request
  | Workshop request -> Workshop.yojson_of_t request
  | Moneylender request -> Moneylender.yojson_of_t request
  | Remodel request -> Remodel.yojson_of_t request
  | ThroneRoom request -> Throne_room.yojson_of_t request
  | Mine request -> Mine.yojson_of_t request
  | Artisan request -> Artisan.yojson_of_t request
