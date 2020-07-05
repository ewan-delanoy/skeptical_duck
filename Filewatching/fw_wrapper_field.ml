(*

#use"Filewatching/fw_wrapper_field.ml";;

Acts on the surrounding physical Unix world, within the limits
defined in  the configuration parameter

*)

exception Rootless_not_found of Dfn_rootless_t.t;;


module Private = struct 

let pair_of_crobj crobj=
   let (_,(arg1,arg2,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
  (
    Dfn_rootless.of_concrete_object arg1,
    Concrete_object_field.unwrap_string arg2
  );;

let pair_to_crobj (watched_file,modif_date)=
  Concrete_object_t.Variant("Dfn_"^"rootless.J",
     [
        
        Dfn_rootless.to_concrete_object watched_file;
        Concrete_object_field.wrap_string(modif_date);
     ]
   ) ;;

let salt = "Fw_"^"wrapper_t.";;

let configuration_label        = salt ^ "configuration";;
let compilable_files_label     = salt ^ "compilable_files";;
let noncompilable_files_label  = salt ^ "noncompilable_files";;
let last_noticed_changes_label = salt ^ "last_noticed_changes";;

let of_concrete_object ccrt_obj = 
   let g=Concrete_object_field.get_record ccrt_obj in
   {
      Fw_wrapper_t.configuration = Fw_configuration.of_concrete_object(g configuration_label);
      compilable_files = Concrete_object_field.to_list pair_of_crobj (g compilable_files_label);
      noncompilable_files = Concrete_object_field.to_list pair_of_crobj (g noncompilable_files_label);
      last_noticed_changes = Dircopy_diff.of_concrete_object (g last_noticed_changes_label);
   };; 

let to_concrete_object fw=
   let items= 
   [
    configuration_label, Fw_configuration.to_concrete_object fw.Fw_wrapper_t.configuration;
    compilable_files_label, Concrete_object_field.of_list pair_to_crobj fw.Fw_wrapper_t.compilable_files;
    noncompilable_files_label, Concrete_object_field.of_list pair_to_crobj fw.Fw_wrapper_t.noncompilable_files;
    last_noticed_changes_label, Dircopy_diff.to_concrete_object fw.Fw_wrapper_t.last_noticed_changes
   ]  in
   Concrete_object_t.Record items;;


end ;;

let reflect_changes_in_diff fw l= {
   fw with 
   Fw_wrapper_t.last_noticed_changes = 
     Dircopy_diff.add_changes 
       (fw.Fw_wrapper_t.last_noticed_changes) l
} ;;

let get_content fw rootless = 
    let root = Fw_configuration.root (fw.Fw_wrapper_t.configuration) in 
    let s_ap = Dfn_common.recompose_potential_absolute_path root rootless in 
    Io.read_whole_file(Absolute_path.of_string s_ap);;     
        
let get_mtime_or_zero_if_file_is_nonregistered fw rootless =
   match Option.seek (fun (rootless1,_)->rootless1=rootless) 
    ((fw.Fw_wrapper_t.compilable_files)@(fw.Fw_wrapper_t.noncompilable_files)) with 
   None -> "0."
  |Some(_,mtime)-> mtime  ;; 


let get_mtime fw rootless  =
  match Option.seek (fun (rootless1,_)->rootless1=rootless) 
    ((fw.Fw_wrapper_t.compilable_files)@(fw.Fw_wrapper_t.noncompilable_files)) with 
   None -> raise (Rootless_not_found(rootless))
  |Some(_,mtime)-> mtime  ;; 

let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;

let reflect_creations_in_diff fw created_ones= {
   fw with 
   Fw_wrapper_t.last_noticed_changes = 
     Dircopy_diff.create 
       (fw.Fw_wrapper_t.last_noticed_changes) created_ones
} ;;


let reflect_destructions_in_diff fw destroyed_ones = {
   fw with 
   Fw_wrapper_t.last_noticed_changes = 
     Dircopy_diff.destroy  
       (fw.Fw_wrapper_t.last_noticed_changes) destroyed_ones 
} ;;


let reflect_replacements_in_diff fw reps= {
   fw with 
   Fw_wrapper_t.last_noticed_changes = 
     Dircopy_diff.replace 
       (fw.Fw_wrapper_t.last_noticed_changes) reps
} ;;

let root fw = Fw_configuration.root (fw.Fw_wrapper_t.configuration);;

