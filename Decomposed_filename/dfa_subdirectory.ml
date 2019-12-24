(*

Subdirectories name, with the trailing slash removed.

#use"Decomposed_filename/dfa_subdirectory.ml";;


*)


let without_trailing_slash (Dfa_subdirectory_t.SD s)=s;;

let connectable_to_subpath (Dfa_subdirectory_t.SD s)=
  if s="" 
  then "" 
  else s^"/";;

let of_line s=Dfa_subdirectory_t.SD s;;

let main = Dfa_subdirectory_t.SD "";;

let rename_endsubdirectory (Dfa_subdirectory_t.SD(old_subdir),new_esdname) 
   (Dfa_subdirectory_t.SD s)=
   if Supstring.begins_with s old_subdir
   then let sub_s=Cull_string.cobeginning (String.length old_subdir) s in
        let t=Cull_string.before_rightmost old_subdir '/' in
        let new_t=(if t="" then "" else t^"/") in
        Dfa_subdirectory_t.SD(new_t^new_esdname^sub_s)
   else Dfa_subdirectory_t.SD(s);;
   
(*

rename_endsubdirectory (SD("Haag/Huug"),"Java") (SD "Haag/Huug/King/Jordan");;
rename_endsubdirectory (SD("Haag"),"Java") (SD "Haag/Huug/King/Jordan");;

*)              

let to_concrete_object (Dfa_subdirectory_t.SD(s))=
    Concrete_object_t.Variant("Dfa_"^"subdirectory_t.SD",[Concrete_object_field.wrap_string(s)]);;

let of_concrete_object ccrt_obj =
   let (_,(arg1,_,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant ccrt_obj in 
   Dfa_subdirectory_t.SD(Concrete_object_field.unwrap_string arg1);;




