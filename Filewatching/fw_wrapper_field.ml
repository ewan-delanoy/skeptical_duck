(*

#use"Filewatching/fw_wrapper.ml";;

Acts on the physical Unix world around, within the limits
defined in  the configuration parameter

*)

module Private = struct 

let triple_of_crobj crobj=
   let (_,(arg1,arg2,arg3,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
  (
    Dfn_rootless.of_concrete_object arg1,
    Concrete_object_field.unwrap_string arg2,
    Concrete_object_field.unwrap_string arg3
  );;

let triple_to_crobj (watched_file,modif_date,content)=
  Concrete_object_t.Variant("Dfn_"^"rootless.J",
     [
        
        Dfn_rootless.to_concrete_object watched_file;
        Concrete_object_t.String(modif_date);
        Concrete_object_t.String(content);
     ]
   ) ;;

let salt = "Fw_"^"wrapper.";;

let configuration_label         = salt ^ "configuration";;
let watched_files_label         = salt ^ "watched_files";;
let special_watched_files_label = salt ^ "special_watched_files";;

let of_concrete_object ccrt_obj = 
   let g=Concrete_object_field.get_record ccrt_obj in
   {
      Fw_wrapper_t.configuration = Fw_configuration.of_concrete_object(g configuration_label);
      watched_files = Concrete_object_field.to_list triple_of_crobj (g watched_files_label);
      special_watched_files = Concrete_object_field.to_list triple_of_crobj (g special_watched_files_label);
   };; 

let to_concrete_object cs=
   let items= 
   [
    configuration_label, Fw_configuration.to_concrete_object cs.Fw_wrapper_t.configuration;
    watched_files_label, Concrete_object_field.of_list triple_to_crobj cs.Fw_wrapper_t.watched_files;
    special_watched_files_label, Concrete_object_field.of_list triple_to_crobj cs.Fw_wrapper_t.special_watched_files;
   ]  in
   Concrete_object_t.Record items;;

end ;;

let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;


