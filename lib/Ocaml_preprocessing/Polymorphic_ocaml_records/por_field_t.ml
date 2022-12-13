(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_field_t.ml";;

*)

type field_t = {
      field_name : string;
      field_type : string;
      var_name : string;
      default_value : string;
      crobj_converters : (string * string) option;
} ;;
