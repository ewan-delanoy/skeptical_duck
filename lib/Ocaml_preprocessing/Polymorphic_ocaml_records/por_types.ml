(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_types.ml";;

*)

type field_t = {
      field_name : string;
      field_type : string;
      var_name : string;
      default_value : string;
      crobj_converters : (string * string) option;
} ;;
    
type subclass_t = {
      subclass_name : string;
      subclass_fields : string list;
} ;;

type t = {
      main_type_name : string;
      module_name : string;
      fields : field_t list;
      subclasses : subclass_t list;
      type_signature_file : Absolute_path.t;
      implementation_file : Absolute_path.t;
      has_crobj_conversion : bool;
      extensions : (string * string) list;
      restrictions : string list;
      constructors : string list;
      designated_parents : (string * string) list;
} ;; 

