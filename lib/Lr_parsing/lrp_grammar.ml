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

   module DirectRegistration = struct 

   let path_order = Total_ordering.silex_compare Total_ordering.silex_for_strings ;;

   let path_merge = Ordered.merge path_order ;;
   let path_sort = Ordered.sort path_order ;;

   let counter_for_registered_molecules = ref (0) ;;

   let hashtbl_for_indices = Hashtbl.create 100 ;;
   let hashtbl_for_paths = Hashtbl.create 100 ;;
   
   let get_index gram molecule=
      match Hashtbl.find_opt hashtbl_for_indices (gram.core.grammar_serial_number,molecule) with 
       Some old_idx -> old_idx
      |None ->
        let new_idx = (!counter_for_registered_molecules)-1 in 
        let _ = (
         counter_for_registered_molecules:=new_idx+2;
         Hashtbl.replace hashtbl_for_indices (gram.core.grammar_serial_number,molecule) new_idx;
         Hashtbl.replace hashtbl_for_paths (gram.core.grammar_serial_number,new_idx) [];
      ) in  
      new_idx ;;      
      
   let get_paths gram molecule=
      let idx = get_index gram molecule in 
      Hashtbl.find hashtbl_for_paths (gram.core.grammar_serial_number,idx) ;;

   let add_paths gram molecule paths_to_be_added =
       let idx = get_index gram molecule in 
       let old_paths = Hashtbl.find hashtbl_for_paths (gram.core.grammar_serial_number,idx) in 
       let new_paths = path_merge old_paths (path_sort paths_to_be_added) in 
       Hashtbl.replace hashtbl_for_paths (gram.core.grammar_serial_number,idx) new_paths ;;
     

   end ;;   

   module IndirectRegistration = struct 
      
   let get_index gram lrk_molecule = DirectRegistration.get_index gram (Lrk_core_methods.to_uniform_representation lrk_molecule) ;;

   let get_paths gram lrk_molecule = DirectRegistration.get_paths gram (Lrk_core_methods.to_uniform_representation lrk_molecule) ;;
   
   let add_paths gram lrk_molecule paths_to_be_added =
         DirectRegistration.add_paths gram (Lrk_core_methods.to_uniform_representation lrk_molecule) paths_to_be_added ;;

   end ;;   
   

   let add_new_paths_to_lr0_molecule gram lr0_molecule paths_to_be_added =
     IndirectRegistration.add_paths gram lr0_molecule paths_to_be_added ;;

   let register_lr0_molecule gram lr0_molecule = 
     let idx = IndirectRegistration.get_index gram lr0_molecule in 
     RSt(idx,lr0_molecule);; 

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
  let older_paths = IndirectRegistration.get_paths gram old_lr0_molecule in 
  let paths_to_be_added= Image.image (fun p->p@[symb]) older_paths in 
  let _ = add_new_paths_to_lr0_molecule gram new_lr0_molecule paths_to_be_added in 
  new_rlr0_molecule  
  ;;

let rglr_to_uniform_representation (RSt(reg_idx,lr)) = (reg_idx,Lrk_core_methods.to_uniform_representation lr) ;; 

let rglr_of_uniform_representation (reg_idx,uniform_rep)= (RSt(reg_idx,Lrk_core_methods.of_uniform_representation uniform_rep)) ;; 

let hashtbl_for_ghettoes = Hashtbl.create 100 ;;

let compute_ghetto gram rlr_state symb =
  let (RSt (idx,_items)) = rlr_state in  
  let key = (gram.core.grammar_serial_number,idx,symb) in 
  match Hashtbl.find_opt hashtbl_for_ghettoes key with 
  Some old_answer -> rglr_of_uniform_representation old_answer 
  | None ->
   let new_answer = compute_ghetto_naively gram rlr_state symb in 
   let _ = Hashtbl.replace hashtbl_for_ghettoes key (rglr_to_uniform_representation new_answer) in 
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

let hashtbl_for_all_lr0_molecules = Hashtbl.create 100 ;;  

let rglr_list_to_uniform_representation = Image.image rglr_to_uniform_representation ;; 

let rglr_list_of_uniform_representation = Image.image rglr_of_uniform_representation ;;  ;; 


let all_lr0_molecules gram =
  let key = (gram.core.grammar_serial_number) in 
  match Hashtbl.find_opt hashtbl_for_all_lr0_molecules key with 
  Some old_answer -> rglr_list_of_uniform_representation old_answer 
  | None ->
   let new_answer = compute_all_lr0_molecules_naively gram in 
   let _ = Hashtbl.replace hashtbl_for_all_lr0_molecules key (rglr_list_to_uniform_representation new_answer) in 
   new_answer
  ;;


let make_from_bare_grammar bg= {
   core = bg ;
   usual_names_for_lr0_molecules = None ;
} ;;


let make l= make_from_bare_grammar (Lrp_bare_grammar.make l);;

let first_production gram = List.hd(Lrp_bare_grammar.productions gram.core);;


let terminals gram = Lrp_bare_grammar.terminals gram.core ;;
   

let nonterminals gram = Lrp_bare_grammar.terminals gram.core ;;

let is_emptiable gram symb = Lrp_bare_grammar.is_emptiable gram.core symb ;;




module Simple_Lr = struct 

 let terminals_after_a_dot_in_lr0_molecule gram lr0_molecule =
    let atoms= Lrk_core_methods.atoms_inside lr0_molecule in 
    let items = Image.image Lrk_core_methods.item_component atoms in 
    let symbols_after_a_dot = 
      str_sort(List.filter_map Lrp_item.symbol_after_dot_opt items) in  
    let termies = terminals gram in 
    str_intersect termies symbols_after_a_dot ;; 

   let shifts_from_lr0_molecule gram lr0_molecule =
      let idx = IndirectRegistration.get_index gram lr0_molecule  in 
      let terms = terminals_after_a_dot_in_lr0_molecule gram lr0_molecule in 
      Image.image (fun term->
        let  (RSt(new_idx,_))= compute_ghetto gram (RSt(idx,lr0_molecule)) term in 
        (term,Shift(new_idx))
      ) terms ;;

   let test_for_allowing_reduction gram _atom ~head_of_production ~terminal =
      let productions = Lrp_bare_grammar.productions gram.core in 
      let (Prod(early_start,_old_start)) = List.hd(productions)   in 
      if head_of_production = early_start then false else  
      List.mem terminal (Lrp_bare_grammar.follow_set gram.core head_of_production) ;;

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

    let hashtbl_for_data_for_simple_lr_table = Hashtbl.create 100 ;;   

    let data_for_simple_lr_table gram = 
      match Hashtbl.find_opt hashtbl_for_data_for_simple_lr_table gram.core.grammar_serial_number  with 
      Some old_answer -> old_answer 
    | None ->
    let new_answer = compute_data_for_simple_lr_table_naively gram in 
    let _ = (Hashtbl.replace hashtbl_for_data_for_simple_lr_table gram.core.grammar_serial_number new_answer) in 
     new_answer ;;

   let table gram =
       let (l_actions,l_gotos) = data_for_simple_lr_table gram in 
       Lrp_table.make l_actions l_gotos ;;


end ;;   

module Usual_names_for_Lr0_states = struct 

let compute_naively gram = 
      let temp0 = all_lr0_molecules gram in 
      let temp1 = List.tl(List.tl(temp0)) in 
      let temp2 = Image.image (
        fun state ->
         let (RSt(_idx,lr0_molecule)) = state in 
         let paths = IndirectRegistration.get_paths gram lr0_molecule in
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


let all_lr0_molecules = Private.all_lr0_molecules ;;


let conflicts_in_simple_lr_parser () = (!(Private.Simple_Lr.ref_for_conflicts_in_slr_parser)) ;;



let make = Private.make ;;






let simple_lr_table gram = Private.Simple_Lr.table gram ;; 




let usual_names_for_lr0_molecules = Private.Usual_names_for_Lr0_states.usual_names_for_lr0_molecules ;;