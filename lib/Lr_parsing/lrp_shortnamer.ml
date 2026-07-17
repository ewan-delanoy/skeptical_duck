(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_shortnamer.ml";;

*)

open Lrp_types ;;


let on_index (Shn names_for_states) idx0=
 snd(List.find (fun (RSt(idx,_),_)->idx=idx0) names_for_states) ;;   

let on_action shn = function 
  (Shift j)->  on_index shn j 
  |Reduce(Prod(a,b)) -> a^" -> "^(String.concat "" b) 
  |Accept -> "Accept" ;;

let on_action_data shn l =
   Image.image (fun (idx,transitions)->
    (on_index shn idx,Image.image (
      fun (mover,result) -> (mover,on_action shn result) 
    ) transitions)   
   ) l;;  

let on_goto_data names_for_states l =
   Image.image (fun (idx,transitions)->
    (on_index names_for_states idx,Image.image (
      fun (mover,result) -> (mover,on_index names_for_states result) 
    ) transitions)   
   ) l;;     

let on_table names_for_states tbl =
   (
      on_action_data names_for_states tbl.action_data,
      on_goto_data names_for_states tbl.goto_data
   ) ;; 
 

let on_parsing_details names_for_states l =
   List.rev_map (
    fun (state_stack,symbol_stack,next_action) ->
      (
         List.rev_map (on_index names_for_states) state_stack,
         symbol_stack,
         on_action names_for_states next_action
      )
   ) l ;;


   