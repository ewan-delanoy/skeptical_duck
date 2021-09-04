(*

#use"Decomposed_filename/dfn_middle.ml";;

*)

module Private = struct 

  let of_concrete_object crobj =
     let (_,(arg1,arg2,_,_,_,_,_))=Concrete_object.unwrap_bounded_variant crobj in 
     Dfn_middle_t.J(
        Dfa_subdirectory.of_concrete_object arg1,
        Dfa_module.of_concrete_object arg2
     );;
  
  let to_concrete_object (Dfn_middle_t.J(s,m))=
     Concrete_object_t.Variant("Dfn_"^"middle_t.J",
       [
         Dfa_subdirectory.to_concrete_object s;
         Dfa_module.to_concrete_object m;
       ]
     ) ;;
end ;; 
  
let of_concrete_object = Private.of_concrete_object ;; 
let rename_endsubdirectory 
   (old_subdir,new_subdirname) 
      (Dfn_middle_t.J(s,m))=
   Dfn_middle_t.J(
   		(Dfa_subdirectory.rename_endsubdirectory (old_subdir,new_subdirname) s),
         m
	    );; 

let rename_module (m1,m2) middle =
  let (Dfn_middle_t.J(s,m)) = middle in 
  if m = m1 
  then Dfn_middle_t.J(s,m2)
  else middle;;       
let to_concrete_object = Private.to_concrete_object ;;   
let to_line (Dfn_middle_t.J(s,m)) = (Dfa_subdirectory.connectable_to_subpath s)^ (Dfa_module.to_line m);;
let to_module (Dfn_middle_t.J(s,m))=m;;