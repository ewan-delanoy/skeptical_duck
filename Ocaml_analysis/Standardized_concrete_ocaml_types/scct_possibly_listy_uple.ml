(* 

#use"Ocaml_analysis/Standardized_concrete_ocaml_types/scct_possibly_listy_uple.ml";;


*)



module Private = struct

let dimension (Scct_possibly_listy_uple_t.U(item_name,is_a_list, l)) = 
   List.length l ;;  

end ;;

let dimension = Private.dimension ;;

