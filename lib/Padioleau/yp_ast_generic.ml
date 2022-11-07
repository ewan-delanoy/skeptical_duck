(*

#use"lib/Padioleau/yp_ast_generic.ml";;

*)

let (=~) = Yp_common.(=~) ;;    

let is_metavar_name s =
  s =~ "^\\(\\$[A-Z_][A-Z_0-9]*\\)$" ;; 