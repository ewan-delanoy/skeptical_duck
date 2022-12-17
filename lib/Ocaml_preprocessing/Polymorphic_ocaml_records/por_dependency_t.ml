(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_dependency_t.ml";;

*)

type t = {
      dependency_name : string;
      dependency_type : string;
      var_name : string;
      crobj_converters : (string * string) option;
      dependency_definition : string;
      shortcuts : (string * string) list;
} ;;
