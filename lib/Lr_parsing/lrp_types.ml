(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_types.ml";;

*)

type production = Prod of string * string list ;;

type bare_grammar = BG of production list ;;

type item = Item of string * (string list) ;;

type lr0_state = St of item list ;;

type registered_lr0_state = RSt of int * (item list) ;;

type registry_for_lr0_states = Rg of ( lr0_state * ((string list) list) ) list ;;

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

type data_for_actions =  (int * (string * action) list) list ;;

type data_for_gotos = (int * (string * int) list) list ;;

type shortnamer = Shn of (registered_lr0_state * string) list ;;

type grammar = {
   core : bare_grammar ;
   mutable symbols : (string list) option;
   mutable terminals : (string list) option;
   mutable nonterminals : (string list) option;
   mutable registry : registry_for_lr0_states ;
   hashtbl_for_ghettoes : (int * string, registered_lr0_state) Hashtbl.t ;
   mutable all_lr0_states : (registered_lr0_state list) option; 
   hashtbl_for_emptiability : (string, bool) Hashtbl.t ;
   mutable emptiable_nonterminals : (string list) option ;
   hashtbl_for_furst_sets : (string, string list) Hashtbl.t ;
   hashtbl_for_rightmost_ancestors : (string, string list) Hashtbl.t ;
   hashtbl_for_follow_sets : (string, string list) Hashtbl.t ;
   mutable data_for_simple_lr_table : ( data_for_actions * data_for_gotos ) option ;
   mutable usual_names_for_lr0_states : shortnamer option;
} ;;


