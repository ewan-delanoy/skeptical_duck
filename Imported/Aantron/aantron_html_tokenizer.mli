(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Aantron_markup_common

type token =
  [ `Doctype of doctype
  | `Start of Token_tag.t
  | `End of Token_tag.t
  | `Char of int
  | `Comment of string
  | `EOF ]

type state = [ `Data | `RCDATA | `RAWTEXT | `Script_data | `PLAINTEXT ]

val tokenize :
   Aantron_markup_error.parse_handler ->
  (location * int)  Aantron_kstream.t * (unit -> location) ->
    (location * token)  Aantron_kstream.t *
    (state -> unit) *
    ((unit -> bool) -> unit)
