(* 

#use"Concrete_ocaml_objects/crobj_basic_increase_t.ml";;


*)


type t= 
     Push_int of int 
    |Push_string of string 
    |Push_record_name  of string 
    |Usual of Crobj_category_t.t * Crobj_parenthesing_tool_t.t ;;
    


        



