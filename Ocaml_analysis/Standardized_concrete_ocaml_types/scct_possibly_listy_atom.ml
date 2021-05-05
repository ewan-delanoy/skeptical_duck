(* 

#use"Ocaml_analysis/Standardized_concrete_ocaml_types/scct_possibly_listy_atom.ml";;


*)

type t = T of  string * bool * Scct_atomic_type_t.t ;;

module Private = struct
    
    let c= "Concrete_"^"object_field.";;
    let wrap = Scct_common.wrap_in_parentheses_if_needed ;;
    
    let converters (T(_,is_listy,atm)) =
          let cv_of_crobj =  Scct_atomic_type.converter_from_crobj atm 
          and cv_to_crobj =  Scct_atomic_type.converter_from_crobj atm in 
          if is_listy 
          then ("( "^c^"to_list"^" "^cv_of_crobj^" )","( "^c^"of_list"^" "^cv_to_crobj^" )")  
          else (cv_of_crobj,cv_to_crobj) ;;
    
    let write_in_ocaml (T(_,is_listy,atm)) =     
       let default = Scct_atomic_type.write_in_ocaml atm in 
       if is_listy 
       then (wrap default)^" list" 
       else default ;;     


end ;;
    
let converter_from_crobj pl_atm = fst (Private.converters  pl_atm) ;;
let converter_to_crobj   pl_atm  = snd (Private.converters pl_atm) ;;
let write_in_ocaml = Private.write_in_ocaml ;;    