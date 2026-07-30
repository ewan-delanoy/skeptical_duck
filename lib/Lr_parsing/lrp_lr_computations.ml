(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_lr_computations.ml";;

*)

open Lrp_types ;;
open Lrp_constant ;;

exception Conflict_in_Lr_parser_exn ;;

module type LRK_SEED  =  
sig
    
    type atom 
    type molecule 
    val atoms_inside : molecule -> atom list
    val empty_one : molecule
    val ender_atom : grammar -> atom
    val immediate_closure : grammar -> atom -> atom list
    val item_component : atom -> item
    val molecule : atom list -> molecule
    val name : string
    val order_on_atoms : grammar -> atom Total_ordering_t.t
    val push_dot_one_symbol : string -> atom -> atom option
    val starter_atom : grammar -> atom
    val test_for_allowing_reduction : grammar -> atom -> head_of_production:string -> terminal:string -> bool
    val visualize_atom : atom -> string
end ;;

module Make = functor(Seed:LRK_SEED) -> struct


module Private = struct 

   let str_order = Total_ordering.lex_for_strings ;;

   let str_fold_merge = Ordered.fold_merge str_order ;; 
   let str_insert = Ordered.insert str_order ;; 
   let str_intersect = Ordered.intersect str_order ;; 
   let str_mem = Ordered.mem str_order ;; 
   let str_merge = Ordered.merge str_order ;; 
   let str_setminus = Ordered.setminus str_order ;; 
   let str_sort = Ordered.sort str_order ;; 


   module Registration = struct 

   let path_order = Total_ordering.silex_compare Total_ordering.silex_for_strings ;;

   let path_merge = Ordered.merge path_order ;;
   let path_sort = Ordered.sort path_order ;;

   let hashtbl_for_counting_registered_molecules = Hashtbl.create 100 ;;

   let hashtbl_for_indices = Hashtbl.create 100 ;;
   let hashtbl_for_paths = Hashtbl.create 100 ;;
   
   let number_of_registered_molecules gram =
      match Hashtbl.find_opt hashtbl_for_counting_registered_molecules gram.grammar_serial_number with 
      Some old_count -> old_count 
      |None -> let _ =(Hashtbl.replace hashtbl_for_counting_registered_molecules gram.grammar_serial_number 0) in 
               0 ;;

   let get_index gram molecule=
      match Hashtbl.find_opt hashtbl_for_indices (gram.grammar_serial_number,molecule) with 
       Some old_idx -> old_idx
      |None ->
        let new_idx = (number_of_registered_molecules gram)-1 in 
        let _ = (
         Hashtbl.replace hashtbl_for_counting_registered_molecules gram.grammar_serial_number (new_idx+2);
         Hashtbl.replace hashtbl_for_indices (gram.grammar_serial_number,molecule) new_idx;
         Hashtbl.replace hashtbl_for_paths (gram.grammar_serial_number,new_idx) [];
      ) in  
      new_idx ;;      
      
   let get_paths gram molecule=
      let idx = get_index gram molecule in 
      Hashtbl.find hashtbl_for_paths (gram.grammar_serial_number,idx) ;;

   let add_paths gram molecule paths_to_be_added =
       let idx = get_index gram molecule in 
       let old_paths = Hashtbl.find hashtbl_for_paths (gram.grammar_serial_number,idx) in 
       let new_paths = path_merge old_paths (path_sort paths_to_be_added) in 
       Hashtbl.replace hashtbl_for_paths (gram.grammar_serial_number,idx) new_paths ;;
     

   end ;;   
   

   let add_new_paths_to_lrk_molecule gram lrk_molecule paths_to_be_added =
     Registration.add_paths gram lrk_molecule paths_to_be_added ;;

   let register_lrk_molecule gram lrk_molecule = 
     let idx = Registration.get_index gram lrk_molecule in 
     (idx,lrk_molecule);; 

   let immediate_closure_for_several gram atoms = 
      Ordered.fold_merge (Seed.order_on_atoms gram) 
      (Image.image (Seed.immediate_closure gram) atoms) ;; 

   let rec towards_closure gram (whole,_treated,to_be_treated) = 
    if to_be_treated = [] then Seed.molecule(whole) else 
    let temp = immediate_closure_for_several gram to_be_treated in 
    let new_whole = Ordered.merge (Seed.order_on_atoms gram) temp whole 
    and yet_untreated = Ordered.setminus (Seed.order_on_atoms gram) temp whole in   
    towards_closure gram (new_whole,whole,yet_untreated) ;;

   let closure gram items = towards_closure gram (items,[],items) ;;  

   let push_dots_one_symbol gram symb lrk_molecule =
      let old_atoms = Seed.atoms_inside lrk_molecule in 
      let unordered_new_atoms = List.filter_map (Seed.push_dot_one_symbol symb) old_atoms in
      let new_atoms = Ordered.sort (Seed.order_on_atoms gram) unordered_new_atoms in 
      Seed.molecule new_atoms ;;

   let ghetto_for_jterm gram lrk_molecule symb = closure gram 
   (Seed.atoms_inside(push_dots_one_symbol gram symb lrk_molecule));; 

let compute_ghetto_naively gram ( (_idx,old_lrk_molecule)) symb = 
  let new_lrk_molecule = ghetto_for_jterm gram old_lrk_molecule symb in 
  let new_rlrk_molecule = register_lrk_molecule gram new_lrk_molecule in 
  let older_paths = Registration.get_paths gram old_lrk_molecule in 
  let paths_to_be_added= Image.image (fun p->p@[symb]) older_paths in 
  let _ = add_new_paths_to_lrk_molecule gram new_lrk_molecule paths_to_be_added in 
  new_rlrk_molecule  
  ;;

let hashtbl_for_ghettoes = Hashtbl.create 100 ;;

let compute_ghetto gram rlr_state symb =
  let ( (idx,_items)) = rlr_state in  
  let key = (gram.grammar_serial_number,idx,symb) in 
  match Hashtbl.find_opt hashtbl_for_ghettoes key with 
  Some old_answer -> old_answer 
  | None ->
   let new_answer = compute_ghetto_naively gram rlr_state symb in 
   let _ = Hashtbl.replace hashtbl_for_ghettoes key new_answer in 
   new_answer
  ;;

let rlrk_molecule_order = ((fun ( (i1,_))  ( (i2,_))->Total_ordering.for_integers i1 i2): (int * Seed.molecule)  Total_ordering_t.t) ;;
let rlrk_molecule_fold_merge = Ordered.fold_merge rlrk_molecule_order ;;
let rlrk_molecule_merge = Ordered.merge rlrk_molecule_order ;;
let rlrk_molecule_setminus = Ordered.setminus rlrk_molecule_order ;;
let rlrk_molecule_sort = Ordered.sort rlrk_molecule_order ;;


let ghetto_neighbors_for_one gram rlrk_molecule = 
   let all_symbols = Lrp_grammar.all_symbols gram in 
   rlrk_molecule_sort(Image.image (compute_ghetto gram rlrk_molecule) all_symbols) ;;

let ghetto_neighbors_for_several gram lrk_molecules = rlrk_molecule_fold_merge
 (Image.image (ghetto_neighbors_for_one gram) lrk_molecules) ;;

let rec towards_ghetto_neighborhood gram (whole,_treated,to_be_treated) = 
  if to_be_treated = [] then whole else 
  let temp = ghetto_neighbors_for_several gram to_be_treated in 
  let new_whole = rlrk_molecule_merge temp whole 
  and yet_untreated = rlrk_molecule_setminus temp whole  in 
 towards_ghetto_neighborhood gram (new_whole,whole,yet_untreated) ;;

let ghetto_neighborhood gram lrk_molecules = towards_ghetto_neighborhood gram (lrk_molecules,[],lrk_molecules) ;; 

let starter_lrk_molecule gram = 
   closure gram [Seed.starter_atom gram];; 

let starter_rlrk_molecule gram = 
   let starter_lrk_molecule = starter_lrk_molecule gram in
   let answer = register_lrk_molecule gram starter_lrk_molecule in 
   let _ = add_new_paths_to_lrk_molecule gram starter_lrk_molecule [[]] in 
   answer;;
;;

let compute_all_indexed_lrk_molecules_naively gram = 
   let _ = (register_lrk_molecule gram Seed.empty_one) in 
   let strtr=starter_rlrk_molecule gram in
   ghetto_neighborhood gram [strtr] ;;

let hashtbl_for_all_indexed_lrk_molecules = Hashtbl.create 100 ;;  

let all_indexed_lrk_molecules gram =
  let key = (gram.grammar_serial_number) in 
  match Hashtbl.find_opt hashtbl_for_all_indexed_lrk_molecules key with 
  Some old_answer -> old_answer 
  | None ->
   let new_answer = compute_all_indexed_lrk_molecules_naively gram in 
   let _ = Hashtbl.replace hashtbl_for_all_indexed_lrk_molecules key new_answer in 
   new_answer
  ;;


module Pretty_printing = struct 


let on_molecule molecule =
      let atoms = Seed.atoms_inside molecule in 
      "\n\n"^(String.concat "\n" (Image.image (fun atom-> (String.make 5 ' ')^(Seed.visualize_atom atom)) atoms))^"\n\n" ;;
      
let on_name_giver ((idx,molecule),surname) =
       "State number "^(string_of_int idx)^", aka "^surname^":\n"^
          (on_molecule molecule) ;;

let on_name_givers triples = "\n\n"^(String.concat "\n" (Image.image on_name_giver triples))^"\n\n" ;;   
      

end ;;   

module Pretty_display = struct 

   let on_parsing_details names_for_states l =
   List.rev_map (
    fun (state_stack,symbol_stack,next_action) ->
      (
         List.rev_map (Lrp_pretty_printing.on_index names_for_states) state_stack,
         symbol_stack,
         Lrp_pretty_printing.on_action names_for_states next_action
      )
   ) l ;;

end ;;   


module Usual_names_for_States = struct 

let compute_usual_names_for_lrk_molecules_naively gram = 
      let temp0 = all_indexed_lrk_molecules gram in 
      let temp1 = List.tl(List.tl(temp0)) in 
      let temp2 = Image.image (
        fun state ->
         let ((_idx,lrk_molecule)) = state in 
         let paths = Registration.get_paths gram lrk_molecule in
         (state,List.hd(List.rev(List.hd paths))) 
      ) temp1 in 
      (
      (List.nth temp0 0,"Death")::
      (List.nth temp0 1,"Birth")::
      (List_again.rename_according_to_occurrence_rank temp2)
       ) ;;

 let hashtbl_for_usual_names_for_lrk_molecules = Hashtbl.create 100 ;;   

    let usual_names_for_lrk_molecules_without_visualizing gram = 
      match Hashtbl.find_opt hashtbl_for_usual_names_for_lrk_molecules gram.grammar_serial_number  with 
      Some old_answer -> old_answer 
    | None ->
    let new_answer = compute_usual_names_for_lrk_molecules_naively gram in 
    let _ = (Hashtbl.replace hashtbl_for_usual_names_for_lrk_molecules gram.grammar_serial_number new_answer) in 
     new_answer ;;

   
   
   let usual_names_for_lrk_molecules gram =
      let answer =usual_names_for_lrk_molecules_without_visualizing gram in 
      let msg = Pretty_printing.on_name_givers answer in 
      let _ = print_string ("\n\n"^msg^"\n\n") in 
      answer;;

end ;;   


module Table = struct 

 let terminals_after_a_dot_in_lrk_molecule gram lrk_molecule =
    let atoms= Seed.atoms_inside lrk_molecule in 
    let items = Image.image Seed.item_component atoms in 
    let symbols_after_a_dot = 
      str_sort(List.filter_map Lrp_item.symbol_after_dot_opt items) in  
    let termies = Lrp_grammar.terminals gram in 
    str_intersect termies symbols_after_a_dot ;; 

   let shifts_from_lrk_molecule gram lrk_molecule =
      let idx = Registration.get_index gram lrk_molecule  in 
      let terms = terminals_after_a_dot_in_lrk_molecule gram lrk_molecule in 
      Image.image (fun term->
        let  ((new_idx,_))= compute_ghetto gram ((idx,lrk_molecule)) term in 
        (term,Shift(new_idx))
      ) terms ;;

    let full_test_for_allowing_reduction gram atom ~head_of_production ~terminal =
      let productions = Lrp_grammar.productions gram in 
      let (Prod(early_start,_old_start)) = List.hd(productions)   in 
      if head_of_production = early_start then false else  
      Seed.test_for_allowing_reduction gram atom ~head_of_production ~terminal ;;   

   let reduction_from_terminal_and_atom_opt gram terminal atom =
      match Lrp_item.almost_finished_production_opt (Seed.item_component atom) with
      None -> None
      |Some(production) ->
         let (Prod(head_of_production,_)) = production in 
         if full_test_for_allowing_reduction gram atom ~head_of_production ~terminal 
         then Some(terminal,Reduce(production))
         else None;;  
         
   let reduction_from_molecule_and_terminal_opt gram lrk_molecule term = 
      let atoms= Seed.atoms_inside lrk_molecule in 
      List.find_map (reduction_from_terminal_and_atom_opt gram term) atoms;;  

   let reductions_from_lrk_molecule gram lrk_molecule = 
      let termies = str_insert end_marker (Lrp_grammar.terminals gram) in 
      List.filter_map (reduction_from_molecule_and_terminal_opt gram lrk_molecule) termies ;;
  

   let acceptations_from_lrk_molecule gram lrk_molecule =
      let atoms= Seed.atoms_inside lrk_molecule in 
      if List.mem (Seed.ender_atom gram) atoms 
      then  [(end_marker,Accept)]
      else [] ;; 
     
   let actions_from_lrk_molecule gram lrk_molecule =
      (shifts_from_lrk_molecule gram lrk_molecule)@
      (reductions_from_lrk_molecule gram lrk_molecule)@
      (acceptations_from_lrk_molecule gram lrk_molecule) ;; 
  
   let action_pairs_from_indexed_lrk_molecule gram (idx,lrk_molecule) = 
      let  termies = str_insert end_marker (Lrp_grammar.terminals gram) 
      and raw_data = actions_from_lrk_molecule gram lrk_molecule in 
      List.filter_map (fun term->
         let results = List.filter_map (fun (term2,act)->if term2=term then Some act else None ) raw_data in 
         if results=[]
         then None 
         else Some((idx,term),results)
      ) termies ;;

   let all_action_pairs gram =
      let indexed_molecules = List.tl(all_indexed_lrk_molecules gram)  in 
      List.flatten(Image.image (action_pairs_from_indexed_lrk_molecule gram) indexed_molecules );;

   let all_goto_pairs gram = 
      let states = List.tl(all_indexed_lrk_molecules gram) 
      and nonterminals = Lrp_grammar.nonterminals gram  in 
      let base = Cartesian.product states nonterminals in 
      List.filter_map (
         fun pair ->
            let (state,nonterminal) = pair in 
            let ((idx,_items)) = state in 
            let ((new_idx,_new_items)) = compute_ghetto gram state nonterminal in 
            if new_idx>=0 
            then Some((idx,nonterminal),[new_idx])
            else None
      ) base ;;

   let compute_pre_table_naively gram= {
      action_pre_data = all_action_pairs gram;
      goto_pre_data = all_goto_pairs gram;
   } ;; 


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
       let names_for_states = Usual_names_for_States.usual_names_for_lrk_molecules_without_visualizing gram 
       and (action_conflicts,goto_conflicts) = Option.get conflicts_opt in 
       let msg ="\n\nThis grammar is not "^ Seed.name ^".\n"^
       (Lrp_pretty_printing.on_both_conflicts names_for_states (action_conflicts,goto_conflicts)) in 
       let _ = print_string msg in 
       raise Conflict_in_Lr_parser_exn
      ;;

    
   let table gram = 
      let names_for_states = Usual_names_for_States.usual_names_for_lrk_molecules_without_visualizing gram in 
      let tbl = towards_table gram in 
      let _ = print_string(Lrp_pretty_printing.on_table names_for_states tbl) in 
      tbl ;; 

end ;;   



end ;;   


let all_indexed_lrk_molecules = Private.all_indexed_lrk_molecules ;;


let parsing_details gram text_to_be_parsed = 
   let names_for_states = Private.Usual_names_for_States.usual_names_for_lrk_molecules gram in 
   let lr_table = Private.Table.table gram in 
   let parse_example = Lrp_table.parsing_details lr_table text_to_be_parsed in 
   Private.Pretty_display.on_parsing_details names_for_states parse_example ;;   


let table = Private.Table.table ;;   


let usual_names_for_lrk_molecules = Private.Usual_names_for_States.usual_names_for_lrk_molecules ;;

end ;;