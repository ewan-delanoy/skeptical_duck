(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_lalr_computations.ml";;

Here, LALR means LALR(1).

*)

open Lrp_types ;; 
open Lrp_constant ;;

exception Conflict_in_Lalr_parser_exn ;;

module Private = struct 

module Lr0_fruit = Lrp_lr_computations.Make(Lr0_seed) ;;
module Lr1_fruit = Lrp_lr_computations.Make(Lr1_seed) ;;

module type WRAPPED_GRAMMAR = sig
  
val current_grammar : grammar 

end ;;

module ComputePreTable = functor (WG:WRAPPED_GRAMMAR) -> struct 

let current_grammar = WG.current_grammar ;;

let str_order = Total_ordering.lex_for_strings ;;

let str_insert = Ordered.insert str_order ;; 


let current_productions = Lrp_grammar.productions current_grammar ;;

let index_of_production_in_current_grammar prod =
  List_again.index_of_in prod current_productions ;;

let order_on_productions = ((fun prod1 prod2 ->
  Total_ordering.for_integers 
    (index_of_production_in_current_grammar prod1) 
      (index_of_production_in_current_grammar prod2)  
): production Total_ordering_t.t);;   



(* A convenience for correct reasoning *)


type lr0_index = LR0 of int ;;
type lr1_index = LR1 of int ;;
type lr1_action = 
  LR1_Shift of lr1_index | LR1_Reduce of production | LR1_Accept ;;
type lalr_action = 
  LALR_Shift of lr0_index | LALR_Reduce of production | LALR_Accept ;;  

module AdjustTypes = struct 

let declare_as_lr0 idx = LR0 idx ;;
let declare_as_lr1 idx = LR1 idx ;;
let declare_action_as_lr1 = function 
   (Shift j)-> LR1_Shift (LR1 j)
  |(Reduce prod) ->  LR1_Reduce prod 
  |Accept -> LR1_Accept ;;
let lalr_to_usual = function 
   (LALR_Shift (LR0 j))-> Shift (j)
  |(LALR_Reduce prod) ->  Reduce prod 
  |LALR_Accept -> Accept ;;  

let unveil_action_data l = Image.image (fun ((LR0 j,mover),l2)->((j,mover),Image.image lalr_to_usual l2)) l;; 
let unveil_goto_data l = Image.image (fun ((LR0 j,mover),l2)->((j,mover),Image.image (fun (LR0 i)->i) l2)) l;; 

end ;;   

let order_on_lr0_indices= ((fun (LR0 i1) (LR0 i2) ->
  Total_ordering.for_integers i1 i2
): lr0_index Total_ordering_t.t);;  

let compare_lalr_action_to_shift action1 j2 = match action1 with 
  (LALR_Shift (LR0 j1))-> Total_ordering.for_integers j1 j2 
  |LALR_Reduce _ 
  |LALR_Accept -> Total_ordering_result_t.Greater ;;

let compare_lalr_action_to_reduce action1 prod2 = match action1 with
  (LALR_Shift _)-> Total_ordering_result_t.Lower 
  |(LALR_Reduce prod1) ->  order_on_productions prod1 prod2
  |LALR_Accept -> Total_ordering_result_t.Greater ;;

let compare_lalr_action_to_accept = function 
   (LALR_Shift _)
  |(LALR_Reduce _) ->  Total_ordering_result_t.Lower
  |LALR_Accept -> Total_ordering_result_t.Equal ;;

let order_on_lalr_actions= ((fun action1 action2 ->
   match action2 with 
   (LALR_Shift (LR0 j2))-> compare_lalr_action_to_shift action1 j2 
  |(LALR_Reduce prod2) ->  compare_lalr_action_to_reduce action1 prod2 
  |LALR_Accept -> compare_lalr_action_to_accept action1
): lalr_action Total_ordering_t.t);;  

let raw_lr0_cores = 
   Image.image (fun (idx,molecule)->(AdjustTypes.declare_as_lr0 idx,molecule))
   (List.tl(Lr0_fruit.all_indexed_lrk_molecules current_grammar));; 

let indices_for_lr0_cores = Image.image (fun (lr0_idx,molecule)->
(Image.image  Lr0_seed.item_component (Lr0_seed.atoms_inside molecule),lr0_idx)  
) raw_lr0_cores ;;

let raw_lr1_states = 
   Image.image (fun (idx,molecule)->(AdjustTypes.declare_as_lr1 idx,molecule))
   (List.tl(Lr1_fruit.all_indexed_lrk_molecules current_grammar)) ;;


let lr1_states = Image.image (fun (idx,molecule)->
(idx,Image.image  (fun atm->(Lr1_seed.item_component atm,Lr1_seed.lookahead_component atm)) 
(Lr1_seed.atoms_inside molecule))  
) raw_lr1_states ;;

let cores_for_lr1_states = Image.image (
  fun (idx,lr1_atoms) ->
   let unordered_lr0_core = Image.image fst lr1_atoms in 
   let lr0_core = Ordered.sort (Lrp_grammar.order_on_items current_grammar) unordered_lr0_core in 
   (idx,List.assoc lr0_core indices_for_lr0_cores)
) lr1_states ;;

let equivalence_classes = Image.image (fun (_,lr0_idx)->
 (lr0_idx,List.filter_map (fun (lr1_idx,core)->if core=lr0_idx then Some lr1_idx else None) cores_for_lr1_states)   
) indices_for_lr0_cores ;;

let lr0_index_of_lr1_index = (fun lr1_index->List.assoc lr1_index cores_for_lr1_states) ;;
let lr1_indices_of_lr0_index = (fun lr0_idx -> List.assoc lr0_idx equivalence_classes) ;;
let lr1_table = Lr1_fruit.table current_grammar ;;

let lr1_action_data = Image.image (fun (idx,transitions)->
   ( AdjustTypes.declare_as_lr1 idx, Image.image (fun (mover,action)->
        (mover,AdjustTypes.declare_action_as_lr1 action)
   ) transitions)
   ) lr1_table.action_data ;;

let lr1_goto_data = Image.image (fun (idx,transitions)->
   ( AdjustTypes.declare_as_lr1 idx, Image.image (fun (mover,destination)->
        (mover,AdjustTypes.declare_as_lr1 destination)
   ) transitions)
   ) lr1_table.goto_data ;;

let lalr_action_of_lr1_action =(fun action -> match action with 
    LR1_Shift(lr1_index) -> LALR_Shift(lr0_index_of_lr1_index lr1_index)
   |LR1_Reduce prod -> LALR_Reduce prod
   |LR1_Accept -> LALR_Accept );;

let termies = str_insert end_marker (Lrp_grammar.terminals current_grammar) ;;

let contribution_of_lr1_index_to_lalr_actions lr1_index =
   let temp = List.assoc lr1_index lr1_action_data in 
   Image.image (fun (mover,action)->(mover,lalr_action_of_lr1_action action)) temp ;;

let contribution_of_lr0_index_to_lalr_actions lr0_index =
   let temp = List.flatten (Image.image (contribution_of_lr1_index_to_lalr_actions) (lr1_indices_of_lr0_index lr0_index)) in 
   List.filter_map (fun term->
    let outlets = List.filter_map (fun (term2,action)->if term2=term then Some action else None) temp in 
    if outlets = []
    then None 
   else Some((lr0_index,term),Ordered.sort order_on_lalr_actions outlets)  
   ) termies ;;

let positive_lr0_indices = Image.image fst raw_lr0_cores ;;   

let pre_data_for_actions = List.flatten (Image.image contribution_of_lr0_index_to_lalr_actions positive_lr0_indices) ;; 

let nontermies = Lrp_grammar.nonterminals current_grammar ;;

let contribution_of_lr1_index_to_lalr_gotos lr1_index =
   match List.assoc_opt lr1_index lr1_goto_data with 
   None -> [] 
   |Some temp ->Image.image (fun (mover,destination)->(mover,lr0_index_of_lr1_index destination)) temp ;;

let contribution_of_lr0_index_to_lalr_gotos lr0_index =
   let temp = List.flatten (Image.image (contribution_of_lr1_index_to_lalr_gotos) (lr1_indices_of_lr0_index lr0_index)) in 
   List.filter_map (fun nonterm->
    let outlets = List.filter_map (fun (nonterm2,destination)->if nonterm2=nonterm then Some destination else None) temp in 
    if outlets = []
    then None 
   else Some((lr0_index,nonterm),Ordered.sort order_on_lr0_indices outlets)  
   ) nontermies ;;

let pre_data_for_gotos = List.flatten (Image.image contribution_of_lr0_index_to_lalr_gotos positive_lr0_indices) ;; 

let pre_table = {
    action_pre_data = AdjustTypes.unveil_action_data pre_data_for_actions;
    goto_pre_data = AdjustTypes.unveil_goto_data pre_data_for_gotos;

};;

end ;;



let compute_pre_table_naively gram =
   let module Seed = struct let current_grammar= gram  end in 
   let module Fruit = ComputePreTable(Seed) in 
   Fruit.pre_table ;;

let hashtbl_for_pre_table = Hashtbl.create 100 ;;   

    let pre_table gram = 
      match Hashtbl.find_opt hashtbl_for_pre_table gram.grammar_serial_number  with 
      Some old_answer -> old_answer 
    | None ->
    let new_answer = compute_pre_table_naively gram in 
    let _ = (Hashtbl.replace hashtbl_for_pre_table gram.grammar_serial_number new_answer) in 
     new_answer ;;

let towards_table gram =
      let pre_tbl = pre_table gram in 
      let (tbl_opt,conflicts_opt)=Lrp_table.from_pre_table_opt pre_tbl in 
      match tbl_opt with 
      Some tbl -> tbl 
      | None ->
       let names_for_states = Lr0_fruit.usual_names_for_lrk_molecules gram
       and (action_conflicts,goto_conflicts) = Option.get conflicts_opt in 
       let msg ="\n\nThis grammar is not LALR.\n"^
       (Lrp_pretty_printing.on_both_conflicts names_for_states (action_conflicts,goto_conflicts)) in 
       let _ = print_string msg in 
       raise Conflict_in_Lalr_parser_exn
      ;;

    
   let table gram = 
      let names_for_states = Lr0_fruit.usual_names_for_lrk_molecules gram in 
      let tbl = towards_table gram in 
      let _ = print_string(Lrp_pretty_printing.on_table names_for_states tbl) in 
      tbl ;; 



end ;;  


let table = Private.table ;;

