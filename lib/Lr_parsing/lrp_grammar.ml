(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_grammar.ml";;

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

   let add_new_paths_to_lr0_molecule gram lr0_molecule paths_to_be_added =
   let new_registry = Lrp_registry.add_new_paths_to_lr0_molecule gram.registry lr0_molecule paths_to_be_added in 
   (gram.registry<- new_registry) ;;

   let register_lr0_molecule gram lr0_molecule = 
   let (new_registry,registered_state) = Lrp_registry.register_lr0_molecule gram.registry lr0_molecule in 
   let _ = (gram.registry<- new_registry) in  
   registered_state;; 

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

   let push_dots_one_symbol bare_gram symb lr0_molecule =
      let old_atoms = Lrk_core_methods.atoms_inside lr0_molecule in 
      let unordered_new_atoms = List.filter_map (Lrk_core_methods.push_dot_one_symbol symb) old_atoms in
      let new_atoms = Ordered.sort (Lrk_core_methods.order_on_atoms bare_gram) unordered_new_atoms in 
      Lrk_core_methods.molecule new_atoms ;;

   let ghetto_for_jterm gram lr0_molecule symb = closure gram.core 
   (Lrk_core_methods.atoms_inside(push_dots_one_symbol gram.core symb lr0_molecule));; 

let compute_ghetto_naively gram (RSt (_idx,old_lr0_molecule)) symb = 
  let new_lr0_molecule = ghetto_for_jterm gram old_lr0_molecule symb in 
  let new_rlr0_molecule = register_lr0_molecule gram new_lr0_molecule in 
  let (Rg regy) = gram.registry in 
  let older_paths = List.assoc old_lr0_molecule regy in 
  let paths_to_be_added= Image.image (fun p->p@[symb]) older_paths in 
  let _ = add_new_paths_to_lr0_molecule gram new_lr0_molecule paths_to_be_added in 
  new_rlr0_molecule  
  ;;


let compute_ghetto gram rlr_state symb =
  let (RSt (idx,_items)) = rlr_state in  
  match Hashtbl.find_opt gram.hashtbl_for_ghettoes (idx,symb) with 
  Some old_answer -> old_answer 
  | None ->
   let new_answer = compute_ghetto_naively gram rlr_state symb in 
   let _ = Hashtbl.add gram.hashtbl_for_ghettoes (idx,symb) new_answer in 
   new_answer
  ;;

let rlr0_molecule_order = ((fun (RSt (i1,_))  (RSt (i2,_))->Total_ordering.for_integers i1 i2): registered_lr0_molecule Total_ordering_t.t) ;;
let rlr0_molecule_fold_merge = Ordered.fold_merge rlr0_molecule_order ;;
let rlr0_molecule_merge = Ordered.merge rlr0_molecule_order ;;
let rlr0_molecule_setminus = Ordered.setminus rlr0_molecule_order ;;
let rlr0_molecule_sort = Ordered.sort rlr0_molecule_order ;;

let all_symbols gram = Lrp_bare_grammar.all_symbols gram.core ;;


let ghetto_neighbors_for_one gram rlr0_molecule = 
   let all_symbols = all_symbols gram in 
   rlr0_molecule_sort(Image.image (compute_ghetto gram rlr0_molecule) all_symbols) ;;

let ghetto_neighbors_for_several gram lr0_molecules = rlr0_molecule_fold_merge
 (Image.image (ghetto_neighbors_for_one gram) lr0_molecules) ;;

let rec towards_ghetto_neighborhood gram (whole,_treated,to_be_treated) = 
  if to_be_treated = [] then whole else 
  let temp = ghetto_neighbors_for_several gram to_be_treated in 
  let new_whole = rlr0_molecule_merge temp whole 
  and yet_untreated = rlr0_molecule_setminus temp whole  in 
 towards_ghetto_neighborhood gram (new_whole,whole,yet_untreated) ;;

let ghetto_neighborhood gram lr0_molecules = towards_ghetto_neighborhood gram (lr0_molecules,[],lr0_molecules) ;; 

let starter_lr0_molecule bare_gram = 
   closure bare_gram [Lrk_core_methods.starter_atom bare_gram];; 

let starter_rlr0_molecule gram = 
   let starter_lr0_molecule = starter_lr0_molecule gram.core in
   let answer = register_lr0_molecule gram starter_lr0_molecule in 
   let _ = add_new_paths_to_lr0_molecule gram starter_lr0_molecule [[]] in 
   answer;;
;;

let compute_all_lr0_molecules_naively gram = ghetto_neighborhood gram [starter_rlr0_molecule gram] ;;

let all_lr0_molecules gram = 
   match gram.all_lr0_molecules with 
   Some old_answer -> old_answer 
   | None -> 
      let answer = compute_all_lr0_molecules_naively gram in 
      let _ = (gram.all_lr0_molecules <- (Some answer)
      ) in 
      answer;;


let make_from_bare_grammar bg= {
   core = bg ;
   registry = Lrp_registry.default ;
   hashtbl_for_ghettoes = Hashtbl.create 100;
   all_lr0_molecules = None ;
   hashtbl_for_follow_sets = Hashtbl.create 100;
   data_for_simple_lr_table = None ;
   usual_names_for_lr0_molecules = None ;
} ;;


let make l= make_from_bare_grammar (Lrp_bare_grammar.make l);;

let first_production gram = List.hd(Lrp_bare_grammar.productions gram.core);;


let terminals gram = Lrp_bare_grammar.terminals gram.core ;;
   

let nonterminals gram = Lrp_bare_grammar.terminals gram.core ;;

let is_emptiable gram symb = Lrp_bare_grammar.is_emptiable gram.core symb ;;




module Follow_set = struct 

   let completions_on_the_right_for_in_production symb (Prod(_a,b)) =
      let temp1 = Two_winged_bird_on_plank.generic b in 
      List.filter_map (
        fun (_rev_left,center,right) ->
          if center <> symb then None else 
          Some right   
      ) temp1 ;;

    let completions_on_the_right_for_in_productions symb productions =
     List.flatten (Image.image (completions_on_the_right_for_in_production symb) productions) ;;   


let direct_follow_set gram symb =
   let productions = Lrp_bare_grammar.productions gram.core in 
   let completions = completions_on_the_right_for_in_productions symb productions in 
   let (empty_completions,nonempty_completions) = List.partition (fun x->x=[]) completions in
   let rightmost_contribution = (if empty_completions=[] then [] else [end_marker]) in 
   str_fold_merge (rightmost_contribution::
   (Image.image (Lrp_bare_grammar.furst_set_for_form gram.core) nonempty_completions)) ;;

let compute_naively gram symb =
   str_fold_merge (Image.image (direct_follow_set gram)
    (symb::(Lrp_bare_grammar.rightmost_ancestors gram.core symb))) ;;


let follow_set gram symb = 
  match Hashtbl.find_opt gram.hashtbl_for_follow_sets symb with 
  Some old_answer -> old_answer 
  | None ->
   let new_answer = compute_naively gram symb in 
   let _ = Hashtbl.add gram.hashtbl_for_follow_sets symb new_answer in 
   new_answer
  ;;

end ;;   

module Simple_Lr = struct 

 let terminals_after_a_dot_in_lr0_molecule gram lr0_molecule =
    let atoms= Lrk_core_methods.atoms_inside lr0_molecule in 
    let items = Image.image Lrk_core_methods.item_component atoms in 
    let symbols_after_a_dot = 
      str_sort(List.filter_map Lrp_item.symbol_after_dot_opt items) in  
    let termies = terminals gram in 
    str_intersect termies symbols_after_a_dot ;; 

   let shifts_from_lr0_molecule gram lr0_molecule =
      let idx = Lrp_registry.index_of_in lr0_molecule gram.registry in 
      let terms = terminals_after_a_dot_in_lr0_molecule gram lr0_molecule in 
      Image.image (fun term->
        let  (RSt(new_idx,_))= compute_ghetto gram (RSt(idx,lr0_molecule)) term in 
        (term,Shift(new_idx))
      ) terms ;;

   let test_for_allowing_reduction gram _atom ~head_of_production ~terminal =
      let productions = Lrp_bare_grammar.productions gram.core in 
      let (Prod(early_start,_old_start)) = List.hd(productions)   in 
      if head_of_production = early_start then false else  
      List.mem terminal (Follow_set.follow_set gram head_of_production) ;;

   let reduction_from_terminal_and_atom_opt gram terminal atom =
      match Lrp_item.almost_finished_production_opt (Lrk_core_methods.item_component atom) with
      None -> None
      |Some(production) ->
         let (Prod(head_of_production,_)) = production in 
         if test_for_allowing_reduction gram atom ~head_of_production ~terminal 
         then Some(terminal,Reduce(production))
         else None;;  
         
   let reduction_from_molecule_and_terminal_opt gram lr0_molecule term = 
      let atoms= Lrk_core_methods.atoms_inside lr0_molecule in 
      List.find_map (reduction_from_terminal_and_atom_opt gram term) atoms;;  

   let reductions_from_lr0_molecule gram lr0_molecule = 
      let termies = terminals gram in 
      List.filter_map (reduction_from_molecule_and_terminal_opt gram lr0_molecule) termies ;;
  

   let acceptations_from_lr0_molecule gram lr0_molecule =
      let (Prod(early_start,old_start)) = first_production gram 
     and (St atoms) = lr0_molecule in 
      let items = Image.image (fun ( Atom  item) -> item) atoms in 
      let the_item = Item(early_start,old_start@["."]) in 
      if List.mem the_item items 
      then  [(end_marker,Accept)]
      else [] ;; 
     
   let actions_from_lr0_molecule gram lr0_molecule =
      (shifts_from_lr0_molecule gram lr0_molecule)@
      (reductions_from_lr0_molecule gram lr0_molecule)@
      (acceptations_from_lr0_molecule gram lr0_molecule) ;; 
  
   let preliminary_data_for_simple_lr_actions gram =
      let states = List.tl(all_lr0_molecules gram) 
      and termies = (terminals gram)@["Endmarker"] in 
      let base = Cartesian.product states termies in 
      let initial_data = List.filter_map (
         fun pair ->
            let (state,terminal) = pair in 
            let (RSt(idx,lr0_molecule)) = state in 
            let all_actions = actions_from_lr0_molecule gram (lr0_molecule) in 
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
      let states = List.tl(all_lr0_molecules gram) 
      and nonterminals = nonterminals gram  in 
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

    let data_for_simple_lr_table gram = 
      match gram.data_for_simple_lr_table with 
      Some old_answer -> old_answer 
    | None ->
    let new_answer = compute_data_for_simple_lr_table_naively gram in 
    let _ = (gram.data_for_simple_lr_table <- Some new_answer) in 
     new_answer ;;

   let table gram =
       let (l_actions,l_gotos) = data_for_simple_lr_table gram in 
       Lrp_table.make l_actions l_gotos ;;


end ;;   

module Usual_names_for_Lr0_states = struct 

let compute_naively gram = 
      let (Rg rgy)=gram.registry 
      and temp0 = all_lr0_molecules gram in 
      let temp1 = List.tl(List.tl(temp0)) in 
      let temp2 = Image.image (
        fun state ->
         let (RSt(_idx,lr0_molecule)) = state in 
         let paths = List.assoc (lr0_molecule) rgy in
         (state,List.hd(List.rev(List.hd paths))) 
      ) temp1 in 
      Shn(
      (List.nth temp0 0,"Death")::
      (List.nth temp0 1,"Birth")::
      (List_again.rename_according_to_occurrence_rank temp2)
       ) ;;

let usual_names_for_lr0_molecules gram = 
      match gram.usual_names_for_lr0_molecules with 
      Some old_answer -> old_answer 
    | None ->
    let new_answer = compute_naively gram in 
    let _ = (gram.usual_names_for_lr0_molecules <- Some new_answer) in 
     new_answer ;;

end ;;   

end ;;   

let all_symbols = Private.all_symbols  ;; 

let augment ~earlier_start ~new_name_for_old_start l=
  let old_gram = Lrp_bare_grammar.make l in 
  let new_gram = Lrp_bare_grammar.augment ~earlier_start ~new_name_for_old_start old_gram in 
  Private.make_from_bare_grammar new_gram ;;

let add_new_paths_to_lr0_molecule = Private.add_new_paths_to_lr0_molecule ;;

let all_lr0_molecules = Private.all_lr0_molecules ;;


let conflicts_in_simple_lr_parser () = (!(Private.Simple_Lr.ref_for_conflicts_in_slr_parser)) ;;

let follow_set = Private.Follow_set.follow_set ;;

let furst_set gram symb = Lrp_bare_grammar.furst_set_for_symbol gram.core symb;;

let items gram = Lrp_bare_grammar.items gram.core ;; 

let make = Private.make ;;



let nonterminals = Private.nonterminals ;;

let register_lr0_molecule = Private.register_lr0_molecule ;;

let simple_lr_table gram = Private.Simple_Lr.table gram ;; 

let start_symbol gram = Lrp_bare_grammar.start_symbol gram.core ;; 

let terminals = Private.terminals;;

let usual_names_for_lr0_molecules = Private.Usual_names_for_Lr0_states.usual_names_for_lr0_molecules ;;