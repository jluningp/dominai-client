open Core
open Import

type t = { hand : Card.t list; discard : int; deck : int; supply : Supply.t }
[@@deriving yojson, sexp] [@@yojson.allow_extra_fields]
