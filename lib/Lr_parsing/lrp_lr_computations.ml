(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_lr_computations.ml";;

*)

open Lrp_types ;;

exception Conflict_in_Lr_parser_exn ;;

module Private = struct 

   let str_order = Total_ordering.lex_for_strings ;;

   let str_fold_merge = Ordered.fold_merge str_order ;; 
   let str_insert = Ordered.insert str_order ;; 
   let str_intersect = Ordered.intersect str_order ;; 
   let str_mem = Ordered.mem str_order ;; 
   let str_merge = Ordered.merge str_order ;; 
   let str_setminus = Ordered.setminus str_order ;; 
   let str_sort = Ordered.sort str_order ;; 

   let end_marker = "Endmarker" ;;

   module Registration = struct 

   let path_order = Total_ordering.silex_compare Total_ordering.silex_for_strings ;;

   let path_merge = Ordered.merge path_order ;;
   let path_sort = Ordered.sort path_order ;;

   let counter_for_registered_molecules = ref (0) ;;

   let hashtbl_for_indices = Hashtbl.create 100 ;;
   let hashtbl_for_paths = Hashtbl.create 100 ;;
   
   let get_index gram molecule=
      match Hashtbl.find_opt hashtbl_for_indices (gram.grammar_serial_number,molecule) with 
       Some old_idx -> old_idx
      |None ->
        let new_idx = (!counter_for_registered_molecules)-1 in 
        let _ = (
         counter_for_registered_molecules:=new_idx+2;
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
     RSt(idx,lrk_molecule);; 

   let immediate_closure_for_several gram atoms = 
      Ordered.fold_merge (Lrk_core_methods.order_on_atoms gram) 
      (Image.image (Lrk_core_methods.immediate_closure gram) atoms) ;; 

   let rec towards_closure gram (whole,_treated,to_be_treated) = 
    if to_be_treated = [] then Lrk_core_methods.molecule(whole) else 
    let temp = immediate_closure_for_several gram to_be_treated in 
    let new_whole = Ordered.merge (Lrk_core_methods.order_on_atoms gram) temp whole 
    and yet_untreated = Ordered.setminus (Lrk_core_methods.order_on_atoms gram) temp whole in   
    towards_closure gram (new_whole,whole,yet_untreated) ;;

   let closure gram items = towards_closure gram (items,[],items) ;;  

   let push_dots_one_symbol gram symb lrk_molecule =
      let old_atoms = Lrk_core_methods.atoms_inside lrk_molecule in 
      let unordered_new_atoms = List.filter_map (Lrk_core_methods.push_dot_one_symbol symb) old_atoms in
      let new_atoms = Ordered.sort (Lrk_core_methods.order_on_atoms gram) unordered_new_atoms in 
      Lrk_core_methods.molecule new_atoms ;;

   let ghetto_for_jterm gram lrk_molecule symb = closure gram 
   (Lrk_core_methods.atoms_inside(push_dots_one_symbol gram symb lrk_molecule));; 

let compute_ghetto_naively gram (RSt (_idx,old_lrk_molecule)) symb = 
  let new_lrk_molecule = ghetto_for_jterm gram old_lrk_molecule symb in 
  let new_rlrk_molecule = register_lrk_molecule gram new_lrk_molecule in 
  let older_paths = Registration.get_paths gram old_lrk_molecule in 
  let paths_to_be_added= Image.image (fun p->p@[symb]) older_paths in 
  let _ = add_new_paths_to_lrk_molecule gram new_lrk_molecule paths_to_be_added in 
  new_rlrk_molecule  
  ;;

let hashtbl_for_ghettoes = Hashtbl.create 100 ;;

let compute_ghetto gram rlr_state symb =
  let (RSt (idx,_items)) = rlr_state in  
  let key = (gram.grammar_serial_number,idx,symb) in 
  match Hashtbl.find_opt hashtbl_for_ghettoes key with 
  Some old_answer -> old_answer 
  | None ->
   let new_answer = compute_ghetto_naively gram rlr_state symb in 
   let _ = Hashtbl.replace hashtbl_for_ghettoes key new_answer in 
   new_answer
  ;;

let rlrk_molecule_order = ((fun (RSt (i1,_))  (RSt (i2,_))->Total_ordering.for_integers i1 i2): registered_lr0_molecule Total_ordering_t.t) ;;
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
   closure gram [Lrk_core_methods.starter_atom gram];; 

let starter_rlrk_molecule gram = 
   let starter_lrk_molecule = starter_lrk_molecule gram in
   let answer = register_lrk_molecule gram starter_lrk_molecule in 
   let _ = add_new_paths_to_lrk_molecule gram starter_lrk_molecule [[]] in 
   answer;;
;;

let compute_all_lrk_molecules_naively gram = 
   let _ = (register_lrk_molecule gram Lrk_core_methods.empty_one) in 
   let strtr=starter_rlrk_molecule gram in
   ghetto_neighborhood gram [strtr] ;;

let hashtbl_for_all_lrk_molecules = Hashtbl.create 100 ;;  

let all_lrk_molecules gram =
  let key = (gram.grammar_serial_number) in 
  match Hashtbl.find_opt hashtbl_for_all_lrk_molecules key with 
  Some old_answer -> old_answer 
  | None ->
   let new_answer = compute_all_lrk_molecules_naively gram in 
   let _ = Hashtbl.replace hashtbl_for_all_lrk_molecules key new_answer in 
   new_answer
  ;;


module Simple_Lr = struct 

 let terminals_after_a_dot_in_lrk_molecule gram lrk_molecule =
    let atoms= Lrk_core_methods.atoms_inside lrk_molecule in 
    let items = Image.image Lrk_core_methods.item_component atoms in 
    let symbols_after_a_dot = 
      str_sort(List.filter_map Lrp_item.symbol_after_dot_opt items) in  
    let termies = Lrp_grammar.terminals gram in 
    str_intersect termies symbols_after_a_dot ;; 

   let shifts_from_lrk_molecule gram lrk_molecule =
      let idx = Registration.get_index gram lrk_molecule  in 
      let terms = terminals_after_a_dot_in_lrk_molecule gram lrk_molecule in 
      Image.image (fun term->
        let  (RSt(new_idx,_))= compute_ghetto gram (RSt(idx,lrk_molecule)) term in 
        (term,Shift(new_idx))
      ) terms ;;

   let reduction_from_terminal_and_atom_opt gram terminal atom =
      match Lrp_item.almost_finished_production_opt (Lrk_core_methods.item_component atom) with
      None -> None
      |Some(production) ->
         let (Prod(head_of_production,_)) = production in 
         if Lrk_core_methods.test_for_allowing_reduction gram atom ~head_of_production ~terminal 
         then Some(terminal,Reduce(production))
         else None;;  
         
   let reduction_from_molecule_and_terminal_opt gram lrk_molecule term = 
      let atoms= Lrk_core_methods.atoms_inside lrk_molecule in 
      List.find_map (reduction_from_terminal_and_atom_opt gram term) atoms;;  

   let reductions_from_lrk_molecule gram lrk_molecule = 
      let termies = str_insert end_marker (Lrp_grammar.terminals gram) in 
      List.filter_map (reduction_from_molecule_and_terminal_opt gram lrk_molecule) termies ;;
  

   let acceptations_from_lrk_molecule gram lrk_molecule =
      let atoms= Lrk_core_methods.atoms_inside lrk_molecule in 
      if List.mem (Lrk_core_methods.ender_atom gram) atoms 
      then  [(end_marker,Accept)]
      else [] ;; 
     
   let actions_from_lrk_molecule gram lrk_molecule =
      (shifts_from_lrk_molecule gram lrk_molecule)@
      (reductions_from_lrk_molecule gram lrk_molecule)@
      (acceptations_from_lrk_molecule gram lrk_molecule) ;; 
  
   let preliminary_data_for_simple_lr_actions gram =
      let states = List.tl(all_lrk_molecules gram) 
      and termies = (Lrp_grammar.terminals gram)@["Endmarker"] in 
      let base = Cartesian.product states termies in 
      let initial_data = List.filter_map (
         fun pair ->
            let (state,terminal) = pair in 
            let (RSt(idx,lrk_molecule)) = state in 
            let all_actions = actions_from_lrk_molecule gram (lrk_molecule) in 
            let suitable_actions = List.filter_map (
            fun (symb,action)->if symb<>terminal then None else Some(action)
            )  all_actions in 
            if suitable_actions = []
            then None    
            else Some((idx,terminal),suitable_actions)
      ) base in  
      let (bad,good)=List.partition (fun (_pair,actions)->
           List.length(actions)>1
         ) initial_data in 
      if bad<>[]
      then (Some bad,None)
      else 
      let temp1 = Image.image (fun (pair,actions)->(pair,List.hd actions)) good in 
      let m = List.length(states)-1 in 
      let temp2 = Int_range.scale (fun idx->
        (idx,List.filter_map(fun ((idx2,terminal),action)->
          if idx2=idx then Some(terminal,action) else None
         ) temp1)   
      ) 0 m in 
     (None,Some(temp2));;

   let ref_for_conflicts_in_slr_parser = ref [] ;;

   let data_for_simple_lr_actions gram = 
      let (bad_opt,good_opt) = preliminary_data_for_simple_lr_actions gram in 
      if bad_opt<>None
      then let _ = (ref_for_conflicts_in_slr_parser:=(Option.get bad_opt)) in 
           raise(Conflict_in_Lr_parser_exn)
      else Option.get good_opt ;;     

   let data_for_simple_lr_gotos gram = 
      let states = List.tl(all_lrk_molecules gram) 
      and nonterminals = Lrp_grammar.nonterminals gram  in 
      let base = Cartesian.product states nonterminals in 
      let initial_data = List.filter_map (
         fun pair ->
            let (state,nonterminal) = pair in 
            let (RSt(idx,_items)) = state in 
            let (RSt(new_idx,_new_items)) = compute_ghetto gram state nonterminal in 
            if new_idx>=0 
            then Some(idx,(nonterminal,new_idx))
            else None
      ) base in  
      let m = List.length(states)-1 in 
      Int_range.scale (fun idx->
        (idx,List.filter_map(fun (idx2,pair)->
          if idx2=idx then Some(pair) else None
         ) initial_data)   
      ) 0 m ;;

    let compute_data_for_simple_lr_table_naively gram =
      (
          data_for_simple_lr_actions gram,
          data_for_simple_lr_gotos gram
      ) ;;

    let hashtbl_for_data_for_simple_lr_table = Hashtbl.create 100 ;;   

    let data_for_simple_lr_table gram = 
      match Hashtbl.find_opt hashtbl_for_data_for_simple_lr_table gram.grammar_serial_number  with 
      Some old_answer -> old_answer 
    | None ->
    let new_answer = compute_data_for_simple_lr_table_naively gram in 
    let _ = (Hashtbl.replace hashtbl_for_data_for_simple_lr_table gram.grammar_serial_number new_answer) in 
     new_answer ;;

   let table gram =
       let (l_actions,l_gotos) = data_for_simple_lr_table gram in 
       Lrp_table.make l_actions l_gotos ;;


end ;;   

module Usual_names_for_Lr0_states = struct 

let compute_usual_names_for_lrk_molecules_naively gram = 
      let temp0 = all_lrk_molecules gram in 
      let temp1 = List.tl(List.tl(temp0)) in 
      let temp2 = Image.image (
        fun state ->
         let (RSt(_idx,lrk_molecule)) = state in 
         let paths = Registration.get_paths gram lrk_molecule in
         (state,List.hd(List.rev(List.hd paths))) 
      ) temp1 in 
      (
      (List.nth temp0 0,"Death")::
      (List.nth temp0 1,"Birth")::
      (List_again.rename_according_to_occurrence_rank temp2)
       ) ;;

 let hashtbl_for_usual_names_for_lrk_molecules = Hashtbl.create 100 ;;   

    let usual_names_for_lrk_molecules gram = 
      match Hashtbl.find_opt hashtbl_for_usual_names_for_lrk_molecules gram.grammar_serial_number  with 
      Some old_answer -> old_answer 
    | None ->
    let new_answer = compute_usual_names_for_lrk_molecules_naively gram in 
    let _ = (Hashtbl.replace hashtbl_for_usual_names_for_lrk_molecules gram.grammar_serial_number new_answer) in 
     new_answer ;;

end ;;   

module Shortnamer = struct 

let on_index names_for_states idx0=
 snd(List.find (fun (RSt(idx,_),_)->idx=idx0) names_for_states) ;;   

let on_action names_for_states = function 
  (Shift j)->  on_index names_for_states j 
  |Reduce(Prod(a,b)) -> a^" -> "^(String.concat "" b) 
  |Accept -> "Accept" ;;

let on_action_data names_for_states l =
   Image.image (fun (idx,transitions)->
    (on_index names_for_states idx,Image.image (
      fun (mover,result) -> (mover,on_action names_for_states result) 
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

let on_conflict_element names_for_states ((idx,mover),actions)=
  ((on_index names_for_states idx,mover),Image.image (on_action names_for_states) actions) ;;

let on_conflicts  names_for_states elements = Image.image (on_conflict_element names_for_states)  elements ;;
      

end ;;   

end ;;   


let all_lrk_molecules = Private.all_lrk_molecules ;;


let conflicts_in_lr_parser gram = 
   try (let _ =Private.Simple_Lr.table gram in []) with
   _ -> 
   let conflicts = (!(Private.Simple_Lr.ref_for_conflicts_in_slr_parser)) in 
   let names_for_states = Private.Usual_names_for_Lr0_states.usual_names_for_lrk_molecules gram in 
   Private.Shortnamer.on_conflicts names_for_states conflicts;;


let parsing_details gram text_to_be_parsed = 
   let names_for_states = Private.Usual_names_for_Lr0_states.usual_names_for_lrk_molecules gram in 
   let lr_table = Private.Simple_Lr.table gram in 
   let parse_example = Lrp_table.parsing_details lr_table text_to_be_parsed in 
   Private.Shortnamer.on_parsing_details names_for_states parse_example ;;   


let simple_lr_table gram = Private.Simple_Lr.table gram ;; 




let usual_names_for_lrk_molecules = Private.Usual_names_for_Lr0_states.usual_names_for_lrk_molecules ;;