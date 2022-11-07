(*

#use"lib/Padioleau/yp_flag_parsing.ml";;

*)

let verbose_lexing = ref false ;;

(* will lexer $X and '...' tokens, and allow certain grammar extension
 * see sgrep_guard() below.
*)
let sgrep_mode = ref false ;;

let sgrep_guard v =
  if !sgrep_mode
  then v
  else raise Parsing.Parse_error ;;           