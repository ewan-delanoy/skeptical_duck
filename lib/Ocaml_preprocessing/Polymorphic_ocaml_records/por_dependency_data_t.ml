(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_dependency_data_t.ml";;

In a string * string pair, the first string represents the function's name, and the second
its content.

*)

type t = {
      definition : string * string;
      shortcuts : (string * string) list;
} ;;
