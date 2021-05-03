(* 

#use"Ocaml_analysis/Standardized_concrete_ocaml_types/scct_atomic_type.ml";;


*)

module Private = struct

    let c= "Concrete_"^"object_field.";;
    
    let table_for_names_inside_converters = 
        [
          Scct_atomic_type_t.Bool,"bool";
          Scct_atomic_type_t.Int_List,"int_list";
          Scct_atomic_type_t.Int_Pair,"int_pair";
          Scct_atomic_type_t.Int_Triple,"int_triple";
          Scct_atomic_type_t.String_List,"string_list";
          Scct_atomic_type_t.String_Pair,"string_pair";
          Scct_atomic_type_t.String_Pair_List,"string_pair_list";
          Scct_atomic_type_t.String_Triple,"string_triple"; 
          Scct_atomic_type_t.String_List_List,"string_list_list"
        ];;
    
    let converters atm = match atm with 
        Scct_atomic_type_t.Modular(modulename) -> let prefix = (String.capitalize_ascii modulename)^"." in 
                              (prefix^"of_concrete_object",prefix^"to_concrete_object") 
       |Int    -> (c^"unwrap_int",c^"wrap_int")
       |String -> (c^"unwrap_string",c^"wrap_string")
       | _ -> let name = List.assoc atm table_for_names_inside_converters in 
              (c^"to_"^name,c^"of_"^name) ;;
    
    let table_for_typenames = 
        [
          Scct_atomic_type_t.Bool,"bool";
          Scct_atomic_type_t.Int,"int";
          Scct_atomic_type_t.Int_List,"int list";
          Scct_atomic_type_t.Int_Pair,"int * int";
          Scct_atomic_type_t.Int_Triple,"int * int *int";
          Scct_atomic_type_t.String,"string";
          Scct_atomic_type_t.String_List,"string list";
          Scct_atomic_type_t.String_Pair,"string * string";
          Scct_atomic_type_t.String_Pair_List,"(string * string) list";
          Scct_atomic_type_t.String_Triple,"string * string * string"; 
          Scct_atomic_type_t.String_List_List,"string list list"
         ];;
    
    let write_in_ocaml atm = match atm with 
         Scct_atomic_type_t.Modular(modulename) -> 
               (String.capitalize_ascii modulename)^"_t.t" 
        | _ -> List.assoc atm table_for_typenames ;;
    
    end ;;
    
let converter_from_crobj atm = fst (Private.converters atm) ;;
let converter_to_crobj atm   = snd (Private.converters atm) ;;
let write_in_ocaml = Private.write_in_ocaml ;;    