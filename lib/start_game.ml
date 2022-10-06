open Core

type t = { kingdom : Card.t list; order : string list } [@@deriving yojson]
