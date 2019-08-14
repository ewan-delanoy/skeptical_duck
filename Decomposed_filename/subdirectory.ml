(*

Subdirectories name, with the trailing slash removed.

#use"subdirectory.ml";;

*)


let without_trailing_slash (Subdirectory_t.SD s)=s;;


let of_string s=Subdirectory_t.SD s;;

let depth (Subdirectory_t.SD s)=
 if s="" then 0 else
 (List.length(Substring.occurrences_of_in "/" s))+1;;

let connectable_to_subpath (Subdirectory_t.SD s)=
  if s="" 
  then "" 
  else s^"/";;

let rename_endsubdirectory (Subdirectory_t.SD(old_subdir),new_esdname) 
   (Subdirectory_t.SD s)=
   if Supstring.begins_with s old_subdir
   then let sub_s=Cull_string.cobeginning (String.length old_subdir) s in
        let t=Cull_string.before_rightmost old_subdir '/' in
        let new_t=(if t="" then "" else t^"/") in
        Subdirectory_t.SD(new_t^new_esdname^sub_s)
   else Subdirectory_t.SD(s);;
   
(*

rename_endsubdirectory (SD("Haag/Huug"),"Java") (SD "Haag/Huug/King/Jordan");;
rename_endsubdirectory (SD("Haag"),"Java") (SD "Haag/Huug/King/Jordan");;

*)              

let to_concrete_object (Subdirectory_t.SD(s))=
    Concrete_object_t.Variant("Subdirectory_t.SD",[Concrete_object_t.String(s)]);;

let of_concrete_object ccrt_obj =
   let (_,(arg1,_,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant ccrt_obj in 
   Subdirectory_t.SD(Concrete_object_field.unwrap_string arg1);;

let list_to_concrete_object l=
   Concrete_object_t.List(Image.image to_concrete_object l);;

let list_of_concrete_object ccrt_obj=
    Image.image of_concrete_object (Concrete_object_field.unwrap_list ccrt_obj);;   

    


