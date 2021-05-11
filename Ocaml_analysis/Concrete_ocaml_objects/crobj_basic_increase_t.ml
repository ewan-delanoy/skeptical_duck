(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_basic_increase_t.ml";;


*)


type t= 
     Push_int of int 
    |Push_string of Encoded_string_t.t 
    |Push_field_name  of string 
    |Open of Crobj_opening_t.t 
    |Separate of Crobj_category_t.t
    |Close of Crobj_category_t.t;;
    


        



