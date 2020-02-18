(* 

#use"Hex_analysis/hex_enemy_contact_result_t.ml";;

This type enumerates the three possible outcomes. No such new enum type is needed 
for ally contacts, where there are only two outcomes and an option suffices to
describe the situation.

*)

type t= 
   No_change
  |Weakening
  |Destruction;;