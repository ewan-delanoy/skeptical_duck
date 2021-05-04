(* 

#use"Ocaml_analysis/Standardized_concrete_ocaml_types/scct_common.ml";;


*)

let wrap_in_parentheses_if_needed typename =
    if String.contains typename ' '
    then "( "^typename^" )"
    else  typename ;;    