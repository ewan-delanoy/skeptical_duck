(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

   open Aantron_markup_common

   val preprocess :
     (int -> bool) -> Aantron_markup_error.parse_handler -> int Aantron_kstream.t ->
       (location * int) Aantron_kstream.t * (unit -> location)