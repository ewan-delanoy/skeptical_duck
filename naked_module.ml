(*

#use"naked_module.ml";;

A module name, or a candidate for one. Uncapitalized. Should contain no slashes.

*)


let of_string s=Naked_module_t.N (String.uncapitalize_ascii s);; 
let to_string (Naked_module_t.N s)=s;;

let add_prefix_and_capitalize prefix (Naked_module_t.N name)=
  Naked_module_t.N(String.capitalize_ascii(prefix^name));;

let to_concrete_object (Naked_module_t.N(s))=
    Concrete_object_t.Variant("Naked_module_t.N",[Concrete_object_t.String(s)]);;

let of_concrete_object ccrt_obj =
   let (_,(arg1,_,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant ccrt_obj in 
   Naked_module_t.N(Concrete_object_field.unwrap_string arg1);;



           