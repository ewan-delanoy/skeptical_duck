(*

#use"Decomposed_filename/dfn_rootless.ml";;


*)


let of_line line = Dfn_common.string_to_rootless line;;


let to_line (Dfn_rootless_t.J(s,m,e))=
   (Dfa_subdirectory.connectable_to_subpath s)^
   (Dfa_module.to_line m)^(Dfa_ending.connectable_to_modulename e);;

let to_module (Dfn_rootless_t.J(s,m,e))=m;;

let relocate_to (Dfn_rootless_t.J(old_subdir,m,e)) new_subdir=Dfn_rootless_t.J(new_subdir,m,e);;
     

let to_concrete_object (Dfn_rootless_t.J(s,m,e))=
   Concrete_object_t.Variant("Dfn_"^"rootless.J",
     [
        
        Dfa_subdirectory.to_concrete_object s;
        Dfa_module.to_concrete_object m;
        Dfa_ending.to_concrete_object e;
     ]
   ) ;;
    
let of_concrete_object crobj =
   let (_,(arg1,arg2,arg3,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
   Dfn_rootless_t.J(
      Dfa_subdirectory.of_concrete_object arg1,
      Dfa_module.of_concrete_object arg2,
      Dfa_ending.of_concrete_object arg3
   );;