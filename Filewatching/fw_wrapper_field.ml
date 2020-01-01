(*

#use"Filewatching/fw_wrapper.ml";;

Acts on the physical Unix world around, within the limits
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

let configuration_label         = salt ^ "configuration";;
let watched_files_label         = salt ^ "watched_files";;
let special_watched_files_label = salt ^ "special_watched_files";;

let of_concrete_object ccrt_obj = 
   let g=Concrete_object_field.get_record ccrt_obj in
   {
      Fw_wrapper_t.configuration = Fw_configuration.of_concrete_object(g configuration_label);
      watched_files = Concrete_object_field.to_list pair_of_crobj (g watched_files_label);
      special_watched_files = Concrete_object_field.to_list pair_of_crobj (g special_watched_files_label);
   };; 

let to_concrete_object fw=
   let items= 
   [
    configuration_label, Fw_configuration.to_concrete_object fw.Fw_wrapper_t.configuration;
    watched_files_label, Concrete_object_field.of_list pair_to_crobj fw.Fw_wrapper_t.watched_files;
    special_watched_files_label, Concrete_object_field.of_list pair_to_crobj fw.Fw_wrapper_t.special_watched_files;
   ]  in
   Concrete_object_t.Record items;;


end ;;


let get_content fw rootless = 
    let root = Fw_configuration.root (fw.Fw_wrapper_t.configuration) in 
    let s_ap = Dfn_common.recompose_potential_absolute_path root rootless in 
    Io.read_whole_file(Absolute_path.of_string s_ap);;     
        
let get_mtime_or_zero_if_file_is_nonregistered fw rootless =
   match Option.seek (fun (rootless1,_)->rootless1=rootless) 
    ((fw.Fw_wrapper_t.watched_files)@(fw.Fw_wrapper_t.special_watched_files)) with 
   None -> "0."
  |Some(_,mtime)-> mtime  ;; 

let get_mtime fw rootless  =
  match Option.seek (fun (rootless1,_)->rootless1=rootless) 
    ((fw.Fw_wrapper_t.watched_files)@(fw.Fw_wrapper_t.special_watched_files)) with 
   None -> raise (Rootless_not_found(rootless))
  |Some(_,mtime)-> mtime  ;; 

let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;


let root fw = Fw_configuration.root (fw.Fw_wrapper_t.configuration);;

