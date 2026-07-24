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

type lr_table = {
   action_data :  (int * ((string * action) list)) list ;
   action_getter : (int * string, action option) Hashtbl.t ;
   goto_data :  (int * ((string * int) list)) list ;
   goto_getter : (int * string, int option) Hashtbl.t ;
}  ;; 




