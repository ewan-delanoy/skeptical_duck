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

   let add_new_paths_to_lr0_state gram lr0_state paths_to_be_added =
   let new_registry = Lrp_registry.add_new_paths_to_lr0_state gram.registry lr0_state paths_to_be_added in 
   (gram.registry<- new_registry) ;;

   let register_lr0_state gram lr0_state = 
   let (new_registry,registered_state) = Lrp_registry.register_lr0_state gram.registry lr0_state in 
   let _ = (gram.registry<- new_registry) in  
   registered_state;; 

   let ghetto_for_jterm gram (St items) symb = Lrp_bare_grammar.closure gram.core 
(Lrp_item.push_dots_one_symbol gram.core symb items);;

let compute_ghetto_naively gram (RSt (_idx,old_lr0_state)) symb = 
  let new_lr0_state = ghetto_for_jterm gram old_lr0_state symb in 
  let new_rlr0_state = register_lr0_state gram new_lr0_state in 
  let (Rg regy) = gram.registry in 
  let older_paths = List.assoc old_lr0_state regy in 
  let paths_to_be_added= Image.image (fun p->p@[symb]) older_paths in 
  let _ = add_new_paths_to_lr0_state gram new_lr0_state paths_to_be_added in 
  new_rlr0_state  
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

let rlr0_state_order = ((fun (RSt (i1,_))  (RSt (i2,_))->Total_ordering.for_integers i1 i2): registered_lr0_state Total_ordering_t.t) ;;
let rlr0_state_fold_merge = Ordered.fold_merge rlr0_state_order ;;
let rlr0_state_merge = Ordered.merge rlr0_state_order ;;
let rlr0_state_setminus = Ordered.setminus rlr0_state_order ;;
let rlr0_state_sort = Ordered.sort rlr0_state_order ;;

let all_symbols gram = 
   match gram.symbols with 
   Some old_answer -> old_answer 
   | None -> 
      let answer = Lrp_bare_grammar.symbols gram.core in 
      let _ = (
          gram.symbols <- (Some answer);
      ) in 
      answer;;


let ghetto_neighbors_for_one gram rlr0_state = 
   let all_symbols = all_symbols gram in 
   rlr0_state_sort(Image.image (compute_ghetto gram rlr0_state) all_symbols) ;;

let ghetto_neighbors_for_several gram lr0_states = rlr0_state_fold_merge
 (Image.image (ghetto_neighbors_for_one gram) lr0_states) ;;

let rec towards_ghetto_neighborhood gram (whole,_treated,to_be_treated) = 
  if to_be_treated = [] then whole else 
  let temp = ghetto_neighbors_for_several gram to_be_treated in 
  let new_whole = rlr0_state_merge temp whole 
  and yet_untreated = rlr0_state_setminus temp whole  in 
 towards_ghetto_neighborhood gram (new_whole,whole,yet_untreated) ;;

let ghetto_neighborhood gram lr0_states = towards_ghetto_neighborhood gram (lr0_states,[],lr0_states) ;; 

let starter_rlr0_state gram = 
   let starter_lr0_state = Lrp_bare_grammar.starter_lr0_state gram.core in
   let answer = register_lr0_state gram starter_lr0_state in 
   let _ = add_new_paths_to_lr0_state gram starter_lr0_state [[]] in 
   answer;;
;;

let compute_all_lr0_states_naively gram = ghetto_neighborhood gram [starter_rlr0_state gram] ;;

let all_lr0_states gram = 
   match gram.all_lr0_states with 
   Some old_answer -> old_answer 
   | None -> 
      let answer = compute_all_lr0_states_naively gram in 
      let _ = (gram.all_lr0_states <- (Some answer)
      ) in 
      answer;;



let make l= {
   core = BG l ;
   symbols = None ;
   terminals = None ;
   nonterminals = None ;
   registry = Lrp_registry.default ;
   hashtbl_for_ghettoes = Hashtbl.create 100;
   all_lr0_states = None ;
   hashtbl_for_emptiability = Hashtbl.create 100;
   emptiable_nonterminals = None ;
   hashtbl_for_furst_sets = Hashtbl.create 100;
   hashtbl_for_rightmost_ancestors = Hashtbl.create 100;
   hashtbl_for_follow_sets = Hashtbl.create 100;
   data_for_simple_lr_table = None ;
   usual_names_for_lr0_states = None ;
} ;;

let first_production gram =
   let (BG l)=gram.core in List.hd l;;




let terminals gram = 
   match gram.terminals with 
   Some old_answer -> old_answer 
   | None -> 
      let answer = Lrp_bare_grammar.terminals gram.core in 
      let _ = (
          gram.terminals <- (Some answer);
          List.iter (fun terminal -> Hashtbl.add gram.hashtbl_for_furst_sets terminal [terminal]) answer
      ) in 
      answer;;

let nonterminals gram = 
   match gram.nonterminals with 
   Some old_answer -> old_answer 
   | None -> 
      let answer = Lrp_bare_grammar.nonterminals gram.core in 
      let _ = (
          gram.nonterminals <- (Some answer);
      ) in 
      answer;;
   

module Emptiable_nonterminals = struct

let initial_data gram =
   let (BG prods1) = gram.core in 
   let prods2= Image.image (fun (Prod(h,l))->(h,str_sort l)) prods1 in
   let unordered_nonterminals = Image.image fst prods2 in 
   let nonterminals = str_sort unordered_nonterminals in 
   Image.image (fun h->(h,List.assoc h prods2)) nonterminals ;; 


let pusher (simplified_productions,level) =
    let next_simplified_productions = Image.image (fun (h,l)->
        (h,str_setminus l level)
      ) simplified_productions in 
    let next_level = List.filter_map(fun (h,l)->if l=[] then Some h else None) next_simplified_productions in 
    (next_simplified_productions,next_level)  ;;

let rec iterator (preceding_pair,pair) =
   let (_,preceding_level) = preceding_pair 
   and (_,level) = pair in 
   if List.length preceding_level = List.length level 
   then level 
   else iterator(pair,pusher pair) ;;   

let all gram = 
   match gram.emptiable_nonterminals with 
   Some old_answer -> old_answer 
   | None ->
   let first_pair = (initial_data gram,[]) in 
   let final_level = iterator (first_pair,pusher first_pair) in 
   let symbols = all_symbols gram in 
   let _ = (List.iter (fun x->
     Hashtbl.add gram.hashtbl_for_emptiability x (List.mem x final_level)   
   ) symbols;
     gram.emptiable_nonterminals <- (Some final_level);
   ) in 
   final_level ;;



end ;;

module Furst_set = struct 

(*

We compute so-called "FIRST" sets (here renamed "Furst" sets for convenience) 

*)

let elements_having_a_wholly_emptiable_left gram form =
   let _ = Emptiable_nonterminals.all gram in 
   (* When we get here, gram.hashtbl_for_emptiability has already been filled *)
   let rec tempf = (
     fun (treated,to_be_treated) -> 
      match to_be_treated with 
      [] -> List.rev(treated)
      |symb::other_symbs ->
         if Hashtbl.find gram.hashtbl_for_emptiability symb 
         then tempf(symb::treated,other_symbs)
         else List.rev(symb::treated)  
   ) in 
   tempf([],form) ;; 

let expand gram already_found_prefixes (older_heads,current_head) = 
   let termies = terminals gram in 
   let updated_heads = str_insert current_head older_heads in 
   let (BG productions) = gram.core in 
   let temp1 = List.flatten(List.filter_map (fun (Prod(a,b))->
      if a<>current_head then None else Some( elements_having_a_wholly_emptiable_left gram b)) productions) in 
   let candidates =  str_setminus (str_sort temp1) updated_heads in 
   let (new_prefixes1,nonterminal_candidates) = List.partition (fun symb->str_mem symb termies) candidates in 
   let using_precedent_computations = Image.image (fun symb->
      (symb,Hashtbl.find_opt gram.hashtbl_for_furst_sets symb)) nonterminal_candidates in 
   let (to_be_treated_next,already_treated)  = List.partition (fun (_,opt)->opt=None) 
       using_precedent_computations in 
   let new_prefixes2 = str_fold_merge  (Image.image (fun (_,opt)->Option.get opt) already_treated) in 
   let new_prefixes = str_merge new_prefixes1 new_prefixes2 in 
   (str_merge new_prefixes already_found_prefixes,updated_heads,Image.image fst to_be_treated_next) ;;

let rec iterator gram (already_found_prefixes,to_be_treated) = 
   match to_be_treated with 
   [] -> already_found_prefixes 
   |pair :: other_pairs -> 
     let (new_set_of_prefixes,new_pairs) = (
        match Hashtbl.find_opt gram.hashtbl_for_furst_sets (snd pair) with 
        Some old_answer -> (str_merge old_answer already_found_prefixes,[])
        |None -> 
         let (new_set_of_prefixes2,updated_heads,to_be_treated_next) = expand gram already_found_prefixes pair in 
         let new_pairs2 = Image.image (fun candidate ->(updated_heads,candidate)) to_be_treated_next  in 
         (new_set_of_prefixes2,new_pairs2)
     )  in 
     iterator gram (new_set_of_prefixes,new_pairs@other_pairs) ;;
  
let compute_furst_set_naively gram symb= iterator  gram ([],[[],symb]) ;;

let furst_set_for_symbol gram symb = 
  match Hashtbl.find_opt gram.hashtbl_for_furst_sets symb with 
  Some old_answer -> old_answer 
  | None ->
   let new_answer = compute_furst_set_naively gram symb in 
   let _ = Hashtbl.add gram.hashtbl_for_furst_sets symb new_answer in 
   new_answer
  ;;

let furst_set_for_form gram form = 
    let symbols = elements_having_a_wholly_emptiable_left gram form in 
    iterator  gram ([],Image.image(fun symb -> ([],symb)) symbols) ;;



end ;;  

module Rightmost_ancestors = struct 

let direct_rightmost_ancestors gram symb =
   let (BG productions) = gram.core in 
   str_sort(List.filter_map (fun (Prod(a,b))->if List.hd(List.rev b)=symb then Some a else None)  productions) ;;

let direct_rightmost_ancestors_for_several gram symbs =
  str_fold_merge (Image.image (direct_rightmost_ancestors gram) symbs) ;;

let rec helper gram (current_whole,to_be_treated) =
   let possibly_new = direct_rightmost_ancestors_for_several gram to_be_treated in 
   let really_new = str_setminus possibly_new current_whole in 
   if really_new = []
   then current_whole 
   else helper gram (str_merge current_whole really_new,really_new) ;;   

let compute_naively gram symb = helper gram ([],[symb]) ;;

let rightmost_ancestors gram symb = 
  match Hashtbl.find_opt gram.hashtbl_for_rightmost_ancestors symb with 
  Some old_answer -> old_answer 
  | None ->
   let new_answer = compute_naively gram symb in 
   let _ = Hashtbl.add gram.hashtbl_for_rightmost_ancestors symb new_answer in 
   new_answer
  ;;

end ;;   

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
   let (BG productions) = gram.core in 
   let completions = completions_on_the_right_for_in_productions symb productions in 
   let (empty_completions,nonempty_completions) = List.partition (fun x->x=[]) completions in
   let rightmost_contribution = (if empty_completions=[] then [] else [end_marker]) in 
   str_fold_merge (rightmost_contribution::
   (Image.image (Furst_set.furst_set_for_form gram) nonempty_completions)) ;;

let compute_naively gram symb =
   str_fold_merge (Image.image (direct_follow_set gram)(symb::(Rightmost_ancestors.rightmost_ancestors gram symb))) ;;


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

   let terminals_after_a_dot_in_lr0_state gram (St items)=
    let symbols_after_a_dot = 
      str_sort(List.filter_map Lrp_item.symbol_after_dot_opt items) in  
    let termies = terminals gram in 
    str_intersect termies symbols_after_a_dot ;; 

   let shifts_from_lr0_state gram lr0_state =
      let idx = Lrp_registry.index_of_in lr0_state gram.registry in 
      let terms = terminals_after_a_dot_in_lr0_state gram lr0_state in 
      Image.image (fun term->
        let  (RSt(new_idx,_))= compute_ghetto gram (RSt(idx,lr0_state)) term in 
        (term,Shift(new_idx))
      ) terms ;;

   let almost_finished_productions_in_lr0_state (St items)=
    List.filter_map (fun item->Lrp_item.almost_finished_production_opt item) items ;;    
   
   let reductions_from_lr0_state gram lr0_state =
      let (Prod(early_start,_old_start)) = first_production gram   in 
      let prods = almost_finished_productions_in_lr0_state lr0_state in 
      List.flatten(Image.image (fun production->
        let (Prod(p,_)) = production in 
        if p = early_start then [] else  
        let followers = Follow_set.follow_set gram p in 
        Image.image (fun follower -> (follower,Reduce(production))) followers
      ) prods );; 

   let acceptations_from_lr0_state gram lr0_state =
      let (Prod(early_start,old_start)) = first_production gram 
      and (St items) = lr0_state in 
      let the_item = Item(early_start,old_start@["."]) in 
      if List.mem the_item items 
      then  [(end_marker,Accept)]
      else [] ;; 
     
   let actions_from_lr0_state gram lr0_state =
      (shifts_from_lr0_state gram lr0_state)@
      (reductions_from_lr0_state gram lr0_state)@
      (acceptations_from_lr0_state gram lr0_state) ;; 
  
   let preliminary_data_for_simple_lr_actions gram =
      let states = List.tl(all_lr0_states gram) 
      and termies = (terminals gram)@["Endmarker"] in 
      let base = Cartesian.product states termies in 
      let initial_data = List.filter_map (
         fun pair ->
            let (state,terminal) = pair in 
            let (RSt(idx,lr0_state)) = state in 
            let all_actions = actions_from_lr0_state gram (lr0_state) in 
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
      let states = List.tl(all_lr0_states gram) 
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
      and temp0 = all_lr0_states gram in 
      let temp1 = List.tl(List.tl(temp0)) in 
      let temp2 = Image.image (
        fun state ->
         let (RSt(_idx,lr0_state)) = state in 
         let paths = List.assoc (lr0_state) rgy in
         (state,List.hd(List.rev(List.hd paths))) 
      ) temp1 in 
      Shn(
      (List.nth temp0 0,"Death")::
      (List.nth temp0 1,"Birth")::
      (List_again.rename_according_to_occurrence_rank temp2)
       ) ;;

let usual_names_for_lr0_states gram = 
      match gram.usual_names_for_lr0_states with 
      Some old_answer -> old_answer 
    | None ->
    let new_answer = compute_naively gram in 
    let _ = (gram.usual_names_for_lr0_states <- Some new_answer) in 
     new_answer ;;

end ;;   

end ;;   

let all_symbols = Private.all_symbols  ;; 

let augment ~earlier_start ~new_name_for_old_start l=
   let (BG core) = Lrp_bare_grammar.augment ~earlier_start ~new_name_for_old_start (BG l) in 
   Private.make core ;;

let add_new_paths_to_lr0_state = Private.add_new_paths_to_lr0_state ;;

let all_lr0_states = Private.all_lr0_states ;;

let emptiable_nonterminals = Private.Emptiable_nonterminals.all ;;

let conflicts_in_simple_lr_parser () = (!(Private.Simple_Lr.ref_for_conflicts_in_slr_parser)) ;;

let follow_set = Private.Follow_set.follow_set ;;

let furst_set = Private.Furst_set.furst_set_for_symbol ;;

let items gram = Lrp_bare_grammar.items gram.core ;; 

let make = Private.make ;;



let nonterminals = Private.nonterminals ;;

let register_lr0_state = Private.register_lr0_state ;;

let rightmost_ancestors = Private.Rightmost_ancestors.rightmost_ancestors ;;

let simple_lr_table gram = Private.Simple_Lr.table gram ;; 

let start_symbol gram = Lrp_bare_grammar.start_symbol gram.core ;; 

let starter_lr0_state gram = Lrp_bare_grammar.starter_lr0_state gram.core ;; 
let terminals = Private.terminals;;

let usual_names_for_lr0_states = Private.Usual_names_for_Lr0_states.usual_names_for_lr0_states ;;