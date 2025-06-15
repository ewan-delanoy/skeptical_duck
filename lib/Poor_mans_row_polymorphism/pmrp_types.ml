(*

#use"lib/Poor_mans_row_polymorphism/pmrp_types.ml";;

*)


type field = F of string ;;

type field_set = {
  field_set_name : string;
  fields : field list;
} ;;




