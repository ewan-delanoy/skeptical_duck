(*

#use"lib/Szemeredi/sz_preprocessing_for_third_stab.ml";;

*)

let ref_for_divisions_successively_made = ref [] ;; 

let add_one_more_division node division new_nodes =
   (ref_for_divisions_successively_made:=
     (node,division,new_nodes)::(!ref_for_divisions_successively_made)
   ) ;;
