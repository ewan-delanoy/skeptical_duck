(*

#use"lib/Ocaml_preprocessing/old_polymorphic_ocaml_record_t.ml";;

*)

type field_t = {
      field_name : string ;
      field_type : string ;
      var_name : string ;
      default_value : string ;
      crobj_converters : (string * string) option ;
} ;;
   
type instance_t = {
      instance_name : string ;
      instance_fields : string list ;
} ;;
   

type t = {
      main_type_name : string ;
      module_name : string ;
      fields : field_t list ;
      instances : instance_t list ;
      type_signature_file : Absolute_path.t ;
      implementation_file : Absolute_path.t ;
      has_crobj_conversion : bool ;
      extensions : (string * string) list ;
      restrictions : string list ; 
      constructors : string list ;
      designated_parents : (string * string) list ; 
} ;;
   
