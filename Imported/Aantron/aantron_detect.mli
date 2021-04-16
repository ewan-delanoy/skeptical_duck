(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Aantron_markup_common

val select_html : ?limit:int -> char  Aantron_kstream.t ->  Aantron_encoding.t cps
val select_xml : char  Aantron_kstream.t ->  Aantron_encoding.t cps

(* The following values are exposed for testing. They are not used outside the
   module. *)

val normalize_name : bool -> string -> string
val guess_from_bom_html : char  Aantron_kstream.t -> string option cps
val guess_from_bom_xml : char  Aantron_kstream.t -> string option cps
val guess_family_xml : char  Aantron_kstream.t -> string option cps
val meta_tag_prescan :
  ?supported:(string -> bool cont -> unit) ->
  ?limit:int ->
  char  Aantron_kstream.t ->
    string option cps
val read_xml_encoding_declaration :
  char  Aantron_kstream.t ->  Aantron_encoding.t -> string option cps
