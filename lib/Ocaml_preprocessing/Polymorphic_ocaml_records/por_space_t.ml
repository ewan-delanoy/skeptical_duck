(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_space_t.ml";;

*)

type t = {
      main_type_name : string;
      module_name : string;
      type_signature_file : Absolute_path.t;
      implementation_file : Absolute_path.t;
      has_crobj_conversion : bool;
      subclasses : Por_subclass_t.t list;
      dependencies : Por_dependency_t.t list;
      incomplete_extensions : (string * Por_subclass_t.t) list;
} ;; 

