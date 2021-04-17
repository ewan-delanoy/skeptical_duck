(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Aantron_markup_common

val write :
  ?escape_attribute:(string -> string) ->
  ?escape_text:(string -> string) ->
  [< signal ]  Aantron_kstream.t -> string  Aantron_kstream.t
