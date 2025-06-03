(*

#use"lib/Poor_mans_row_polymorphism/pmrp_config_t.ml";;

*)


type field_t = Field of string ;;

type t = {
  fields_with_their_types : (field_t * string) list;
  fieldsets_with_their_names : ((field_t list) * string) list;
} ;; 



