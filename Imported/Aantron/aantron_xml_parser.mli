(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Aantron_markup_common

val parse :
  [< `Document | `Fragment ] option ->
  (string -> string option) ->
   Aantron_markup_error.parse_handler ->
  (location *  Aantron_xml_tokenizer.token)  Aantron_kstream.t ->
    (location * signal)  Aantron_kstream.t
