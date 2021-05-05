(* 

#use"Ocaml_analysis/Standardized_concrete_ocaml_types/scct_inner_uple.ml";;


*)


type t = IU of (string * bool * Scct_atomic_type_t.t ) list


module Private = struct

  
  let write_type (IU(l)) = 
        String.concat " * "  (Image.image (
           fun  pl_atm ->Scct_possibly_listy_atom.write_in_ocaml pl_atm
        ) l) 
       ;;
  
  let vertical_homogeneous_from_crobj ~tab_width ~separator argname (IU l)=
     let temp1 = Ennig.index_everything l in  
     let n = List.length(l) in 
     Image.image (
       fun (k,pl_atm) ->
          let comma_or_not = (if (k=n)||(n=1) then "" else separator) in
          (String.make  tab_width ' ')^(Scct_possibly_listy_atom.converter_from_crobj pl_atm)
          ^" "^argname^(string_of_int k)^comma_or_not
     ) temp1 ;;


  

end ;;

let vertical_homogeneous_from_crobj = Private.vertical_homogeneous_from_crobj ;;
let write_type = Private.write_type ;;