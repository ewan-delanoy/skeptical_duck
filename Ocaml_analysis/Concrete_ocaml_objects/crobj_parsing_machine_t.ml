(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_parsing_machine_t.ml";;


*)

type t= 
    {
       parsed_one    : string;
       current_index : int;
       data : Double_partial_crobj_t.t;
    };;

