(*

#use"lib/Cee_language/cee_common.ml";;

*)

let add_extra_ending_in_filename ~extra fn = 
  let (basename,ending) = Cull_string.split_wrt_rightmost fn '.'  in 
  (basename^"_"^extra^"."^ending) ;;

