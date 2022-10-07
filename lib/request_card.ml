open Core

type t = { card : Card.t } [@@deriving yojson]
type with_data = { card : Card.t; data : string } [@@deriving yojson]
