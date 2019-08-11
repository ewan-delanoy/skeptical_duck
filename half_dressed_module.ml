(*

#use"half_dressed_module.ml";;

A module name, or a candidate for one. Can contain  slashes.
Should not contain dots.
Starts with an uncapitalized letter.
Designates a relative path.

*)


          
type t={
   bundle_main_dir : string;
   subdirectory    : string;
   naked_module     : string;
};;

let bundle_main_dir x=Root_directory.of_string(x.bundle_main_dir);;
let subdirectory x=Subdirectory.of_string(x.subdirectory);;
let naked_module  x=Naked_module.of_string(x.naked_module);;


exception Inexistent_module of string;;
 
let of_string_and_root old_s dir=
        let s=Cull_string.before_rightmost_possibly_all old_s '.' in
        let s_dir=Root_directory.without_trailing_slash dir in
      if List.for_all (fun edg->not(Sys.file_exists(s_dir^"/"^s^edg)) ) 
           Ocaml_ending.all_string_endings
	    then raise(Inexistent_module(s_dir^s))
	    else
	    {
	      bundle_main_dir = s_dir;
   		  subdirectory    =Cull_string.before_rightmost s '/';
          naked_module     =Cull_string.after_rightmost s '/';
	    };;  
   
let uprooted_version x=
   let sub=x.subdirectory in
   if sub=""
   then x.naked_module
   else sub^"/"^(x.naked_module);;

 

let to_shortened_string x=x.naked_module;;   

let unveil x=(uprooted_version x,bundle_main_dir x);;

exception FileOutsideDirectory of Absolute_path.t*Root_directory_t.t;;


let of_path_and_root ap dir=
   if (not(Supstring.begins_with (Absolute_path.to_string ap)
         (Root_directory.connectable_to_subpath dir)))
    then raise(FileOutsideDirectory(ap,dir))
    else 
    let s_dir=Root_directory.without_trailing_slash dir in
    let n_dir=(String.length s_dir)+1 in
    let subpath=Cull_string.cobeginning n_dir (Absolute_path.to_string ap) in
    let s=Cull_string.before_rightmost_possibly_all subpath '.' in
    {
	      bundle_main_dir = s_dir;
   		  subdirectory    =Cull_string.before_rightmost s '/';
          naked_module     =Cull_string.after_rightmost s '/';
    }  ;;    


let is_executable x=
  let s=uprooted_version x in 
  let n=String.length s in
  if String.length(s)<10 then false else
  String.sub s (n-10) 10="executable";;

let capitalized_module_name x=
  (String.capitalize_ascii x.naked_module);;
  
let rename_endsubdirectory (old_subdir,new_subdirname) x=
   {
	      bundle_main_dir = x.bundle_main_dir;
   		  subdirectory    = Subdirectory.without_trailing_slash(
   		                    Subdirectory.rename_endsubdirectory
   		                       (old_subdir,new_subdirname) 
   		                       (Subdirectory.of_string(x.subdirectory)));
          naked_module    = x.naked_module;
    }  ;;    
   

let industrial_separator=Industrial_separator.half_dressed_module;;  
let industrial_separator2=Industrial_separator.half_dressed_module_times_boolean;;  

let archive x=
   String.concat industrial_separator 
    [x.bundle_main_dir;x.subdirectory;x.naked_module];;
  
let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator) s in
   {
	      bundle_main_dir = List.nth l1 0;
   		  subdirectory    = List.nth l1 1;
          naked_module    = List.nth l1 2;
    };;  
   
let archive_pair (x,is_compiled_correctly)=
  String.concat industrial_separator2 
  [archive x;string_of_bool is_compiled_correctly];;

let unarchive_pair s=
    let l1=Str.split (Str.regexp_string industrial_separator2) s in
    (unarchive(List.nth l1 0),
     bool_of_string(List.nth l1 1));;
   
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
      bundle_main_dir_label,hm.bundle_main_dir;
      subdirectory_label,hm.subdirectory;
      naked_module_label,hm.naked_module;
     ] 
   in
   Concrete_object_t.Record items;;
    
    let of_concrete_object ccrt_obj = 
     {
	      bundle_main_dir = Concrete_object_field.get_str_record ccrt_obj bundle_main_dir_label;
   		subdirectory    = Concrete_object_field.get_str_record ccrt_obj subdirectory_label;
         naked_module    = Concrete_object_field.get_str_record ccrt_obj naked_module_label;
      };;

end ;;

let to_concrete_object = Private.to_concrete_object;;

let of_concrete_object = Private.of_concrete_object;;

let pair_to_concrete_object (hm,is_compiled_correctly)=
    Concrete_object_t.Uple [to_concrete_object hm;Concrete_object_field.of_bool is_compiled_correctly];;
   
let pair_of_concrete_object ccrt_obj =
    let (hm_o,is_compiled_correctly_o)=Concrete_object_field.get_pair ccrt_obj in 
    (of_concrete_object hm_o,Concrete_object_field.to_bool is_compiled_correctly_o);;
    

let list_of_pairs_to_concrete_object l=
   Concrete_object_t.List(Image.image pair_to_concrete_object l);;

let list_of_pairs_of_concrete_object ccrt_obj=
    Image.image pair_of_concrete_object (Concrete_object_field.unwrap_list ccrt_obj);;  





