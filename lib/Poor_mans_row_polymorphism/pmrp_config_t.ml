(*

#use"lib/Poor_mans_row_polymorphism/pmrp_config_t.ml";;

*)


type t = {
  fields_with_their_types : (Pmrp_field_t.t * string) list;
  fieldsets : Pmrp_field_set_t.t  list;
  mutable_fields : Pmrp_field_t.t list;
  receiving_file : Absolute_path.t;
} ;; 



