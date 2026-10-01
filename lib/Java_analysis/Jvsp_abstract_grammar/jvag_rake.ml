(*

#use"lib/Java_analysis/Jvsp_abstract_grammar/jvag_rake.ml";;

*)

open Jvag_types ;;

let str_order = Total_ordering.lex_for_strings ;;
let str_sort = Ordered.sort str_order ;;

let indices_in_four_parts cfg leaves = 
  let main_name =cfg.main_name in 
  let indexed_leaves = Int_range.index_everything leaves in
  let (outer_left_recursions,temp1) = List.partition (fun (_idx,(_path,form))->
    List.hd(form)=main_name
    ) indexed_leaves in 
  let (unordered_inner_left_recursions,nonrecursions) = List.partition (fun (_idx,(path,_form))->
    List.mem (List.hd path) (List.tl path)
    ) temp1 in     
  let pivots_for_inner_left_recursions=str_sort(
    Image.image(fun (_idx,(path,_form))->List.hd path) unordered_inner_left_recursions) in 
  let inner_left_recursions = List.flatten (Image.image (
    fun pivot -> List.filter (fun (_idx,(path,_form))->(List.hd path)=pivot) temp1
  ) pivots_for_inner_left_recursions)   in
  let (expandables,terminal_forms) = List.partition (fun (_idx,(_path,form))->
    List.mem (List.hd form) cfg.irreducibles
    ) nonrecursions in 
  let i_terminal_forms = Image.image fst terminal_forms 
  and i_expandables = Image.image fst expandables
  and i_inner_left_recursions = Image.image fst inner_left_recursions 
  and i_outer_left_recursions = Image.image fst outer_left_recursions in 
  (i_terminal_forms,i_expandables,i_inner_left_recursions,i_outer_left_recursions) ;;  


let make_from_not_yet_ordered_grammar cfg unordered_gram= 
  let dis1 = Jvag_form.disjunction_content (Jvag_grammar.get unordered_gram cfg.main_name) in 
  let initial_expansion = Image.image 
    (fun nm->([nm],Jvag_form.concat_content (Jvag_grammar.get unordered_gram nm))) dis1 in 
  let (i_terminal_forms,i_expandables,i_inner_left_recursions,i_outer_left_recursions) = 
     indices_in_four_parts cfg initial_expansion in 
  let ordered_gram = Jvag_grammar.modify unordered_gram [Local(cfg.main_name,
    [Lm_permute_lines(i_terminal_forms@i_expandables@i_inner_left_recursions@i_outer_left_recursions)])] in 
  {
    config = cfg ;
    current_grammar = ordered_gram ;
    terminal_forms = Image.image (fun k->List.nth initial_expansion (k-1)) i_terminal_forms ;
    expandables = Image.image (fun k->List.nth initial_expansion (k-1)) i_expandables ;
    inner_left_recursions = Image.image (fun k->List.nth initial_expansion (k-1)) i_inner_left_recursions ;
    outer_left_recursions = Image.image (fun k->List.nth initial_expansion (k-1)) i_outer_left_recursions ;  
  } ;;  


let make cfg = 
  let unordered_gram = Jvag_grammar.modify cfg.original_grammar [Local(cfg.main_name,[Lm_expand_all_disjunctions])] in 
  make_from_not_yet_ordered_grammar cfg unordered_gram;;  

let expand rake =
   let number_of_terminal_forms = List.length rake.terminal_forms 
   and number_of_expandables = List.length rake.expandables in 
   let i_expandables = Int_range.range (number_of_terminal_forms+1) (number_of_terminal_forms+number_of_expandables) in 
   let new_gram = Jvag_grammar.modify rake.current_grammar [Local(rake.config.main_name,
    [Lm_expand_heads_at_lines(i_expandables)])] in
   make_from_not_yet_ordered_grammar rake.config new_gram;;   
       