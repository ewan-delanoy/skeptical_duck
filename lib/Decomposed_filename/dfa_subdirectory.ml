(*

Subdirectories name, with the trailing slash removed.

#use"lib/Decomposed_filename/dfa_subdirectory.ml";;


*)

module Private = struct 

   let of_line s=
      let n = String.length s in 
      let indices = List.rev(Int_range.range 1 n) in 
      let limit_idx=(match List.find_opt(fun j->(Strung.get s j)<>'/')(indices) with 
         None -> 0 |Some(j)->j
      ) in 
      Dfa_subdirectory_t.SD (Cull_string.beginning limit_idx s);;   
   
   let without_trailing_slash (Dfa_subdirectory_t.SD s)=s;;
   
   end ;;   
   
   
   
   let compute_long_subdir_name old_subdir new_subdir_short_name =
      let temp1 =  Cull_string.trim_slashes_on_the_right new_subdir_short_name in
      let long_name = (
      if String.contains temp1 '/'
      then temp1 
      else let old_subdir_name = Private.without_trailing_slash old_subdir in 
           let father_name = Cull_string.before_rightmost old_subdir_name '/' in 
           if father_name = ""
           then temp1
           else father_name^"/"^temp1 ) in 
      Private.of_line long_name ;;    
   
   let connectable_to_subpath (Dfa_subdirectory_t.SD s)=
     if s="" 
     then "" 
     else s^"/";;
   
   let begins_with (Dfa_subdirectory_t.SD s1) (Dfa_subdirectory_t.SD s2)=
      Supstring.begins_with s1 s2;;
       
   let extend (Dfa_subdirectory_t.SD s) subsub = Dfa_subdirectory_t.SD (s^"/"^subsub);;
   
   let main = Dfa_subdirectory_t.SD "";;
   
   let of_concrete_object ccrt_obj =
      let (_,(arg1,_,_,_,_,_,_))=Concrete_object.unwrap_bounded_variant ccrt_obj in 
      Dfa_subdirectory_t.SD(Crobj_converter.string_of_concrete_object arg1);;
   
   let of_line = Private.of_line ;;
   
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
   
   
   let soak (Dfa_subdirectory_t.SD(s1),Dfa_subdirectory_t.SD(s2)) (Dfa_subdirectory_t.SD(s))=
      match Strung.soak (s1,s2) s with 
      Some(t)->Some(Dfa_subdirectory_t.SD(t))
      |None -> None ;;
   
   let to_concrete_object (Dfa_subdirectory_t.SD(s))=
       Concrete_object_t.Variant("Dfa_"^"subdirectory_t.SD",
       [Crobj_converter.string_to_concrete_object(s)]);;
   
   
   
   let without_trailing_slash= Private.without_trailing_slash ;;
   
   
   