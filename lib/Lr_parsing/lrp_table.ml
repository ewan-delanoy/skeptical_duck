(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_table.ml";;

*)

open Lrp_types ;;
open Lrp_constant ;;

exception Goto_error_exn of int * string ;;
exception Transition_error_exn of int * string ;;
exception No_steps_after_acceptance_exn ;;
exception Nothing_to_parse_exn ;;

module Private = struct 

let instance_counter = ref(0) ;;

let make l_action l_goto =
   let new_val = (!instance_counter)+1 in 
   let _ = (instance_counter:=new_val) in    
   {
     table_serial_number = new_val ;
     action_data = l_action ;
     goto_data = l_goto ;
   } ;;

let i_sort = Ordered.sort Total_ordering.for_integers ;;  
let str_sort = Ordered.sort Total_ordering.lex_for_strings ;;  

let from_pre_table_opt pre_tbl=
   let (bad_actions,good_actions)=List.partition (fun (_pair,actions)->
           List.length(actions)<>1
   ) pre_tbl.action_pre_data 
   and (bad_gotos,good_gotos)=List.partition (fun (_pair,gotos)->
           List.length(gotos)<>1
   ) pre_tbl.goto_pre_data in 
   if (bad_actions<>[])||(bad_gotos<>[])
   then (None,Some(bad_actions,bad_gotos))
   else   
   let actions1 = Image.image (fun (pair,actions)->(pair,List.hd actions)) good_actions 
   and gotos1 = Image.image (fun (pair,actions)->(pair,List.hd actions)) good_gotos in 
   let indices_from_actions = i_sort(Image.image (fun ((idx,_),_) -> idx) actions1) 
   and indices_from_gotos = i_sort(Image.image (fun ((idx,_),_) -> idx) gotos1) in 
   let action_data = Image.image (
     fun idx -> 
      let pairs = List.filter_map(fun ((idx2,mover),act) -> if idx2=idx then Some(mover,act) else None ) actions1 in
      let movers =  str_sort(Image.image fst pairs) in 
      (idx,Image.image (fun mover ->(mover,List.assoc mover pairs)) movers)
   ) indices_from_actions 
   and goto_data = Image.image (
     fun idx -> 
      let pairs = List.filter_map(fun ((idx2,mover),dest) -> if idx2=idx then Some(mover,dest) else None ) gotos1 in
      let movers =  str_sort(Image.image fst pairs) in 
      (idx,Image.image (fun mover ->(mover,List.assoc mover pairs)) movers)
   ) indices_from_gotos 
   in
   (Some(make action_data goto_data),None) ;;   
   

   

let hashtbl_for_actions = Hashtbl.create 100 ;;
 

let compute_action_naively tbl state_idx symb = 
   let temp = List.assoc state_idx tbl.action_data  in 
  List.assoc_opt symb temp ;;

let get_action tbl state_idx symb =
   let wrapped_answer = (
   match Hashtbl.find_opt hashtbl_for_actions (tbl.table_serial_number,state_idx,symb) with 
   Some old_answer -> old_answer 
   |None ->
      let answer = compute_action_naively tbl state_idx symb in 
      let _ = Hashtbl.replace hashtbl_for_actions (tbl.table_serial_number,state_idx,symb) answer in 
      answer 
   ) in 
   match wrapped_answer with 
   None -> raise(Transition_error_exn(state_idx,symb))
   |Some(action)->action ;;

let hashtbl_for_gotos = Hashtbl.create 100 ;;   

let compute_goto_naively tbl state_idx symb = 
   let temp = List.assoc state_idx tbl.goto_data  in 
  List.assoc_opt symb temp ;;

let get_goto tbl state_idx symb =
   let wrapped_answer = (
   match Hashtbl.find_opt hashtbl_for_gotos (tbl.table_serial_number,state_idx,symb) with 
   Some old_answer -> old_answer 
   |None ->
      let answer = compute_goto_naively tbl state_idx symb in 
      let _ = Hashtbl.replace hashtbl_for_gotos (tbl.table_serial_number,state_idx,symb) answer in 
      answer 
   ) in 
   match wrapped_answer with 
   None -> raise(Goto_error_exn(state_idx,symb))
   |Some(goto)->goto ;;

let initial_configuration text_to_be_parsed = ([0],text_to_be_parsed@[end_marker]) ;;  

let compute_next_action tbl (state_stack,symbol_stack) =
   (state_stack,symbol_stack,get_action tbl (List.hd state_stack) (List.hd symbol_stack)) ;;

let initial_configuration tbl text_to_be_parsed = 
   compute_next_action tbl ([0],text_to_be_parsed@[end_marker]) ;;     

let step tbl steps = 
   match steps with 
   [] -> []
   |(state_stack,symbol_stack,next_action)::_ ->
      let next_pair = (
       match next_action with 
      Accept -> raise No_steps_after_acceptance_exn 
      |Shift(j) -> (j::state_stack,List.tl symbol_stack)
      |Reduce(Prod(aa,omega)) ->
         let remaining_stack = List_again.long_tail (List.length omega) state_stack in 
         let remaining_head = List.hd remaining_stack in 
         let k = get_goto tbl remaining_head aa in 
         (k::remaining_stack,symbol_stack)

      ) in 
      (compute_next_action tbl next_pair)::steps ;;

let rec iterator tbl steps = 
   match steps with 
   [] -> raise(Nothing_to_parse_exn)
   |(_state_stack,_symbol_stack,next_action)::_ ->
      if next_action = Accept 
      then steps   
      else iterator tbl (step tbl steps) ;;      


end ;;   


let from_pre_table_opt = Private.from_pre_table_opt ;;

let make = Private.make ;;

let parsing_details tbl text_to_be_parsed=    
  Private.iterator tbl [Private.initial_configuration tbl text_to_be_parsed] ;;

