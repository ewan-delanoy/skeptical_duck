(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_subclass_t.ml";;

*)


type t = {
      subclass_name : string;
      subclass_fields : Por_field_t.t list;
      subclass_dependencies : Por_dependency_t.t list;
      parent : string option;
      extensions_leading_here : string list;
      has_restriction : bool;
      has_constructor : bool;
} ;;


