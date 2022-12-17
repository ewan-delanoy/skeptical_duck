(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_field_t.ml";;

*)

type t = {
      field_name : string;
      field_type : string;
      var_name : string;
      default_value : string;
      crobj_converters : (string * string) option;
      dependency_data : Por_dependency_data_t.t option;
} ;;
