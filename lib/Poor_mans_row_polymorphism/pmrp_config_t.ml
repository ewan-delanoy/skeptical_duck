(*

#use"lib/Poor_mans_row_polymorphism/pmrp_config_t.ml";;

*)


type t = {
  fields_with_their_types : (Pmrp_types.field * string) list;
  fieldsets : Pmrp_types.field_set  list;
  mutable_fields : Pmrp_types.field list;
  receiving_file : Absolute_path.t;
} ;; 



