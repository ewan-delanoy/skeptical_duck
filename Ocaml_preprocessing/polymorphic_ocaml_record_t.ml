(*

#use"Ocaml_preprocessing/polymorphic_ocaml_record_t.ml";;

*)

type field_t = {
   field_name : string ;
   field_type : string ;
   var_name : string ;

} ;;

type instance_t = {
   fields : string list ;
} ;;

type t = {
   fields : field_t list ;
   instances : instance_t list ;
   interface_file : Absolute_path.t ;
   implementation_file : Absolute_path.t ;
} ;;

