(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_types.ml";;

*)

type field_t = {
      space_name_for_field : string ;
      field_name : string ;
      field_type : string ;
      var_name : string ;
      default_value : string ;
      crobj_converters : (string * string) option ;
} ;;
   
type record_t = {
      space_name_for_record : string ;
      record_name : string ;
      record_fields : field_t list ;
} ;;

type dependency_t = {
     space_name_for_dependency : string ; 
     record_for_dependency : record_t ;
     dependency_name : string ;
     names_for_dependencies_before : string list ;
} ;; 

type self_modifier_t = {
     space_name_for_self_modifier : string ; 
     record_for_self_modifier : record_t ;
     self_modifier_name : string ;
} ;;    


type space_t = {
      space_name : string ;
      fields : field_t list ;
      records : record_t list ;
      type_signature_file : Absolute_path.t ;
      implementation_file : Absolute_path.t ;
      has_crobj_conversion : bool ;
} ;;
   
