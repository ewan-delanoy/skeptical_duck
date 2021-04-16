(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

   open Aantron_markup_common

   type token =
     [ `Xml of xml_declaration
     | `Doctype of doctype
     | `Start of Token_tag.t
     | `End of Token_tag.t
     | `Chars of string list
     | `PI of string * string
     | `Comment of string
     | `EOF ]
   
   val tokenize :
   Aantron_markup_error.parse_handler ->
     (string -> string option) ->
     (location * int) Aantron_kstream.t * (unit -> location) ->
       (location * token) Aantron_kstream.t