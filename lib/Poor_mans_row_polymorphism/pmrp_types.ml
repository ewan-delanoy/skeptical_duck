(*

#use"lib/Poor_mans_row_polymorphism/pmrp_types.ml";;

*)

type inhabitated_type = {
   type_name : string;
   inhabitant : string;
} ;;

type field = {
  field_name : string ;
  field_type : inhabitated_type ;
} ;;

type field_set = {
  field_set_name : string;
  fields : field list;
} ;;

type involved_or_not =  
   Involved of  field_set
 | Not_involved of inhabitated_type ;;

type config = {
  fields_in_config : field list;
  field_sets : field_set  list;
  mutable_fields : field list;
  receiving_file : Absolute_path.t;
} ;; 



