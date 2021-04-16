(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Aantron_markup_common

module Parsing :
sig
  type context

  val init : (string -> string option) -> context
  val push :
    unit  Aantron_markup_error.handler ->
    context ->
    string -> (string * string) list ->
      (name * (name * string) list) cps
  val pop : context -> unit
  val expand_element : unit  Aantron_markup_error.handler -> context -> string -> name cps

  val parse : string -> string * string
end

module Writing :
sig
  type context

  val init : (string -> string option) -> context
  val push :
    unit  Aantron_markup_error.handler ->
    context ->
    name -> (name * string) list ->
      (string * (string * string) list) cps
  val pop : context -> unit
end
