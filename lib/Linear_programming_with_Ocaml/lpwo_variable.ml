(*

#use"lib/Linear_programming_with_Ocaml/lpwo_variable.ml";;

*)

let order = ((fun (Lpwo_variable_t.V s1) (Lpwo_variable_t.V s2)  -> 
  Total_ordering.silex_for_strings s1 s2) : Lpwo_variable_t.t 
  Total_ordering_t.t
  ) ;;