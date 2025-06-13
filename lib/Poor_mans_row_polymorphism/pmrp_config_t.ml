(*

#use"lib/Poor_mans_row_polymorphism/pmrp_config_t.ml";;

*)


type field_t = Field of string ;;

type t = {
  fields_with_their_types : (Pmrp_field_t.t * string) list;
  fieldsets_with_their_names : ((Pmrp_field_t.t list) * string) list;
  mutable_fields : Pmrp_field_t.t list;
  receiving_file : Absolute_path.t;
} ;; 



