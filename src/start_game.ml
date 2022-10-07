open Core
open Import

type t = { kingdom : Card.t list; order : string list } [@@deriving yojson]
