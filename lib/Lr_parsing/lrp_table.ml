(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_table.ml";;

*)

open Lrp_types ;;

exception Goto_error_exn of int * string ;;
exception Transition_error_exn of int * string ;;
exception No_steps_after_acceptance_exn ;;
exception Nothing_to_parse_exn ;;

module Private = struct 

let compute_action_naively tbl state_idx symb = 
   let temp = List.assoc state_idx tbl.action_data  in 
  List.assoc_opt symb temp ;;

let get_action tbl state_idx symb =
   let wrapped_answer = (
   match Hashtbl.find_opt tbl.action_getter (state_idx,symb) with 
   Some old_answer -> old_answer 
   |None ->
      let answer = compute_action_naively tbl state_idx symb in 
      let _ = Hashtbl.add tbl.action_getter (state_idx,symb) answer in 
      answer 
   ) in 
   match wrapped_answer with 
   None -> raise(Transition_error_exn(state_idx,symb))
   |Some(action)->action ;;

let compute_goto_naively tbl state_idx symb = 
   let temp = List.assoc state_idx tbl.goto_data  in 
  List.assoc_opt symb temp ;;

let get_goto tbl state_idx symb =
   let wrapped_answer = (
   match Hashtbl.find_opt tbl.goto_getter (state_idx,symb) with 
   Some old_answer -> old_answer 
   |None ->
      let answer = compute_goto_naively tbl state_idx symb in 
      let _ = Hashtbl.add tbl.goto_getter (state_idx,symb) answer in 
      answer 
   ) in 
   match wrapped_answer with 
   None -> raise(Goto_error_exn(state_idx,symb))
   |Some(goto)->goto ;;

let initial_configuration text_to_be_parsed = ([0],text_to_be_parsed@["Endmarker"]) ;;  

let compute_next_action tbl (state_stack,symbol_stack) =
   (state_stack,symbol_stack,get_action tbl (List.hd state_stack) (List.hd symbol_stack)) ;;

let initial_configuration tbl text_to_be_parsed = 
   compute_next_action tbl ([0],text_to_be_parsed@["Endmarker"]) ;;     

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

let get_action = Private.get_action ;;

let get_goto = Private.get_goto ;;

let make l_action l_goto =
   {
     action_data = l_action ;
     action_getter = Hashtbl.create 100;
     goto_data = l_goto ;
     goto_getter = Hashtbl.create 100;
   } ;;

let parsing_details tbl text_to_be_parsed=    
  Private.iterator tbl [Private.initial_configuration tbl text_to_be_parsed] ;;