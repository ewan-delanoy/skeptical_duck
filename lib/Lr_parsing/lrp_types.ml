(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_types.ml";;

*)

type production = Prod of string * string list ;;

type grammar = {
  grammar_serial_number : int ;
  productions : production list ; 
} ;;

type item = Item of string * (string list) ;;

type action = 
   Shift of int 
  |Reduce of production 
  |Accept ;;

type lr_pre_table = {
   action_pre_data :  ((int * string) * (action list)) list ;
   goto_pre_data :    ((int * string) * (int list)) list ;
}  ;; 

type lr_table = {
   table_serial_number : int ;
   action_data :  (int * ((string * action) list)) list ;
   goto_data :  (int * ((string * int) list)) list ;
}  ;; 




