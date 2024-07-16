(*

#use"lib/makefile_t.ml";;

*)

type variable_assignment = {
   va_line_number : int ;
   variable_name : string ;
   content : string list
} ;;

type rule = {
   ru_line_number : int ;
   targets : string list ;
   prerequisites : string list ;
   commands : string list
} ;;


type t = {
   assignments : variable_assignment list;
   rules : rule list
};; 
