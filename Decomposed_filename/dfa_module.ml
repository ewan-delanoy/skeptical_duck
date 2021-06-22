(*

#use"Decomposed_filename/dfa_module.ml";;

A module name, or a candidate for one. Uncapitalized. Should contain no slashes.

*)


let of_line s=Dfa_module_t.M (String.uncapitalize_ascii s);; 
let to_line (Dfa_module_t.M s)=s;;

let add_prefix_and_capitalize prefix (Dfa_module_t.M name)=
  Dfa_module_t.M(String.capitalize_ascii(prefix^name));;

  
let capitalized_form (Dfa_module_t.M name)= String.capitalize_ascii name;;

let to_concrete_object (Dfa_module_t.M(s))=
    Concrete_object_t.Variant("Dfa_"^"module_t.M",
     [Crobj_converter.string_to_concrete_object(s)]);;

let of_concrete_object ccrt_obj =
   let (_,(arg1,_,_,_,_,_,_))=Concrete_object.unwrap_bounded_variant ccrt_obj in 
   Dfa_module_t.M(Crobj_converter.string_of_concrete_object arg1);;
