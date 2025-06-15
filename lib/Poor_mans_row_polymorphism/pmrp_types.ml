(*

#use"lib/Poor_mans_row_polymorphism/pmrp_types.ml";;

*)


type field = F of string ;;

type field_set = {
  field_set_name : string;
  fields : field list;
} ;;

type involved_or_not =  
   Involved of  field_set
 | Not_involved of string ;;

type config = {
  fields_with_their_types : (field * string) list;
  fieldsets : field_set  list;
  mutable_fields : field list;
  receiving_file : Absolute_path.t;
} ;; 



