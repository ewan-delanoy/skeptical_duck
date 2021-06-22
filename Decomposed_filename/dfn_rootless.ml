(*

#use"Decomposed_filename/dfn_rootless.ml";;


*)

module Private = struct 

let of_concrete_object crobj =
   let (_,(arg1,arg2,arg3,_,_,_,_))=Concrete_object.unwrap_bounded_variant crobj in 
   Dfn_rootless_t.J(
      Dfa_subdirectory.of_concrete_object arg1,
      Dfa_module.of_concrete_object arg2,
      Dfa_ending.of_concrete_object arg3
   );;

let to_concrete_object (Dfn_rootless_t.J(s,m,e))=
   Concrete_object_t.Variant("Dfn_"^"rootless.J",
     [
        
        Dfa_subdirectory.to_concrete_object s;
        Dfa_module.to_concrete_object m;
        Dfa_ending.to_concrete_object e;
     ]
   ) ;;



end ;; 


let is_compilable (Dfn_rootless_t.J(s,m,e))= Dfa_ending.is_compilable e;;

let is_in (Dfn_rootless_t.J(s,m,e)) sd = Dfa_subdirectory.begins_with s sd;;

let list_of_concrete_object crobj=
  Crobj_converter_combinator.to_list Private.of_concrete_object crobj ;;

let list_to_concrete_object l=
   Crobj_converter_combinator.of_list Private.to_concrete_object l;;


let of_concrete_object = Private.of_concrete_object ;;

let of_line line = Dfn_common.string_to_rootless line;;

let pair_list_of_concrete_object crobj=
  Crobj_converter_combinator.to_pair_list Private.of_concrete_object Private.of_concrete_object crobj ;;

let pair_list_to_concrete_object l=
  Crobj_converter_combinator.of_pair_list Private.to_concrete_object Private.to_concrete_object l;;


let relocate_to (Dfn_rootless_t.J(old_subdir,m,e)) new_subdir=Dfn_rootless_t.J(new_subdir,m,e);;
     
let rename_subdirectory_as  (old_subdir,new_subdir) old_path=
   let (Dfn_rootless_t.J(s,m,e))=old_path in 
   if s=old_subdir
   then Dfn_rootless_t.J(new_subdir,m,e)
   else old_path;;

let soak (old_subdir,new_subdir) (Dfn_rootless_t.J(s,m,e)) =
   match Dfa_subdirectory.soak (old_subdir,new_subdir) s with 
   Some(new_s)->Some(Dfn_rootless_t.J(new_s,m,e))
   |None -> None ;;

let to_concrete_object = Private.to_concrete_object ;;

let to_ending (Dfn_rootless_t.J(s,m,e))=e;;

let to_line (Dfn_rootless_t.J(s,m,e))=
   (Dfa_subdirectory.connectable_to_subpath s)^
   (Dfa_module.to_line m)^(Dfa_ending.connectable_to_modulename e);;

let to_middle (Dfn_rootless_t.J(s,m,e))=Dfn_middle_t.J(s,m);;

let to_module (Dfn_rootless_t.J(s,m,e))=m;;

let to_subdirectory (Dfn_rootless_t.J(s,m,e))=s;;





    
