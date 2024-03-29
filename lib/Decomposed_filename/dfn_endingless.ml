(*

#use"lib/Decomposed_filename/dfn_endingless.ml";;


*)

let begins_with (Dfn_endingless_t.J(_r,s,_m)) subdir=
   Dfa_subdirectory.begins_with s subdir;;

let to_root (Dfn_endingless_t.J(r,_s,_m))=r;;
let to_subdirectory  (Dfn_endingless_t.J(_r,s,_m))=s;;
let to_module  (Dfn_endingless_t.J(_r,_s,m))=m;;
   
let to_line (Dfn_endingless_t.J(r,s,m))=
   (Dfa_root.connectable_to_subpath r)^
   (Dfa_subdirectory.connectable_to_subpath s)^
   (Dfa_module.to_line m);;

let to_middle (Dfn_endingless_t.J(_r,s,m)) = Dfn_middle_t.J(s,m) ;;

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
   
let soak (old_subdir,new_subdir) eless =
   let (Dfn_endingless_t.J(r,s,m)) = eless in 
   match Dfa_subdirectory.soak (old_subdir,new_subdir) s with 
   Some(new_s)->Some(Dfn_endingless_t.J(r,new_s,m))
   |None -> None ;;


let to_concrete_object (Dfn_endingless_t.J(r,s,m))=
   Concrete_object_t.Variant("Dfn_"^"endingless.J",
     [
        Dfa_root.to_concrete_object r;
        Dfa_subdirectory.to_concrete_object s;
        Dfa_module.to_concrete_object m;
     ]
   ) ;;

let of_concrete_object crobj =
   let (_,(arg1,arg2,arg3,_,_,_,_))=Concrete_object.unwrap_bounded_variant crobj in 
   Dfn_endingless_t.J(
      Dfa_root.of_concrete_object arg1,
      Dfa_subdirectory.of_concrete_object arg2,
      Dfa_module.of_concrete_object arg3
   );;
    



