(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Aantron_markup_common

val parse :
  [< `Document | `Fragment of string ] option ->
   Aantron_markup_error.parse_handler ->
  (location *  Aantron_html_tokenizer.token)  Aantron_kstream.t *
  ( Aantron_html_tokenizer.state -> unit) *
  ((unit -> bool) -> unit) ->
    (location * signal)  Aantron_kstream.t
