(*

#use"Decomposed_filename/dfn_endingless_t.ml";;

A module name, or a candidate for one. Can contain  slashes.
Should not contain dots.
Starts with an uncapitalized letter.
Designates a relative path.

*)

(*

let bundle_main_dir x=Dfa_root.of_string(x.Dfn_endingless_t.bundle_main_dir);;
let subdirectory x=Dfa_subdirectory.of_string(x.Dfn_endingless_t.subdirectory);;
let naked_module  x=Dfa_module.of_string(x.Dfn_endingless_t.naked_module);;


exception Inexistent_module of string;;
 
let of_string_and_root old_s dir=
        let s=Cull_string.before_rightmost_possibly_all old_s '.' in
        let s_dir=Dfa_root.without_trailing_slash dir in
      if List.for_all (fun edg->not(Sys.file_exists(s_dir^"/"^s^edg)) ) 
           Dfa_ending.all_string_endings
	    then raise(Inexistent_module(s_dir^s))
	    else
	    {
	      Dfn_endingless_t.bundle_main_dir = s_dir;
   		Dfn_endingless_t.subdirectory    =Cull_string.before_rightmost s '/';
         Dfn_endingless_t.naked_module     =Cull_string.after_rightmost s '/';
	    };;  
   
let uprooted_version x=
   let sub=x.Dfn_endingless_t.subdirectory in
   if sub=""
   then x.Dfn_endingless_t.naked_module
   else sub^"/"^(x.Dfn_endingless_t.naked_module);;

 

let to_shortened_string x=x.Dfn_endingless_t.naked_module;;   

let unveil x=(uprooted_version x,bundle_main_dir x);;

exception FileOutsideDirectory of Absolute_path.t*Dfa_root_t.t;;


let of_path_and_root ap dir=
   if (not(Supstring.begins_with (Absolute_path.to_string ap)
         (Dfa_root.connectable_to_subpath dir)))
    then raise(FileOutsideDirectory(ap,dir))
    else 
    let s_dir=Dfa_root.without_trailing_slash dir in
    let n_dir=(String.length s_dir)+1 in
    let subpath=Cull_string.cobeginning n_dir (Absolute_path.to_string ap) in
    let s=Cull_string.before_rightmost_possibly_all subpath '.' in
    {
	   Dfn_endingless_t.bundle_main_dir = s_dir;
   	Dfn_endingless_t.subdirectory    =Cull_string.before_rightmost s '/';
      Dfn_endingless_t.naked_module     =Cull_string.after_rightmost s '/';
    }  ;;    


let is_executable x=
  let s=uprooted_version x in 
  let n=String.length s in
  if String.length(s)<10 then false else
  String.sub s (n-10) 10="executable";;

let capitalized_module_name x=
  (String.capitalize_ascii x.Dfn_endingless_t.naked_module);;
  
let rename_endsubdirectory (old_subdir,new_subdirname) x=
   {
	   Dfn_endingless_t.bundle_main_dir = x.Dfn_endingless_t.bundle_main_dir;
   	Dfn_endingless_t.subdirectory    = Dfa_subdirectory.without_trailing_slash(
   		                    Dfa_subdirectory.rename_endsubdirectory
   		                       (old_subdir,new_subdirname) 
   		                       (Dfa_subdirectory.of_string(x.Dfn_endingless_t.subdirectory)));
      Dfn_endingless_t.naked_module    = x.Dfn_endingless_t.naked_module;
    }  ;;    
   

   
module Private = struct 

let salt = "Half_"^"dressed_module.";;

let bundle_main_dir_label = salt ^ "bundle_main_dir";;
let subdirectory_label = salt ^ "subdirectory";;
let naked_module_label = salt ^ "naked_module";;

let to_concrete_object hm=
   let items=Image.image (
      fun (constructor,content)->
        (constructor,Concrete_object_t.String(content))
   ) [
      bundle_main_dir_label,hm.Dfn_endingless_t.bundle_main_dir;
      subdirectory_label,hm.Dfn_endingless_t.subdirectory;
      naked_module_label,hm.Dfn_endingless_t.naked_module;
     ] 
   in
   Concrete_object_t.Record items;;
    
    let of_concrete_object ccrt_obj =
     let g=(fun field->Concrete_object_field.unwrap_string(Concrete_object_field.get_record ccrt_obj field)) in 
     {
	      Dfn_endingless_t.bundle_main_dir = g bundle_main_dir_label;
   		Dfn_endingless_t.subdirectory    = g subdirectory_label;
         Dfn_endingless_t.naked_module    = g naked_module_label;
      };;

end ;;

let to_concrete_object = Private.to_concrete_object;;

let of_concrete_object = Private.of_concrete_object;;

*)


