(*

#use"Decomposed_filename/dfn_endingless.ml";;


*)



let to_root (Dfn_endingless_t.J(r,s,m))=r;;
let to_subdirectory  (Dfn_endingless_t.J(r,s,m))=s;;
let to_module  (Dfn_endingless_t.J(r,s,m))=m;;
   
let to_line (Dfn_endingless_t.J(r,s,m))=
   (Dfa_root.connectable_to_subpath r)^
   (Dfa_subdirectory.connectable_to_subpath s)^
   (Dfa_module.to_line m);;

let to_middle (Dfn_endingless_t.J(r,s,m)) = Dfn_middle_t.J(s,m) ;;

let rename_endsubdirectory 
   (old_subdir,new_subdirname) 
      (Dfn_endingless_t.J(r,s,m))=
   Dfn_endingless_t.J(
	      r,
   		(Dfa_subdirectory.rename_endsubdirectory (old_subdir,new_subdirname) s),
         m
	    );;  

let rename_module
   (old_module,new_module) 
      (Dfn_endingless_t.J(r,s,m))=
        if m =old_module 
        then Dfn_endingless_t.J(r,s,new_module) 
        else Dfn_endingless_t.J(r,s,m) ;;     

let replace_subdirectory (old_subdir,new_subdir) eless = 
    match eless with  
      (Dfn_endingless_t.J(r,s,m)) -> 
       if s <> old_subdir 
       then eless 
       else 
   Dfn_endingless_t.J(r,new_subdir,m);;  
   



let to_concrete_object (Dfn_endingless_t.J(r,s,m))=
   Concrete_object_t.Variant("Dfn_"^"endingless.J",
     [
        Dfa_root.to_concrete_object r;
        Dfa_subdirectory.to_concrete_object s;
        Dfa_module.to_concrete_object m;
     ]
   ) ;;

let of_concrete_object crobj =
   let (_,(arg1,arg2,arg3,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
   Dfn_endingless_t.J(
      Dfa_root.of_concrete_object arg1,
      Dfa_subdirectory.of_concrete_object arg2,
      Dfa_module.of_concrete_object arg3
   );;
    



