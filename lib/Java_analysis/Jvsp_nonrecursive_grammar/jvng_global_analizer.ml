(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_global_analizer.ml";;

*)

module T = Jvsp_types ;;
open Jvng_types ;;

exception End_reached ;;

exception Blocked_state_exn of global_analizer;;

module Private =struct


let put_new_grammar_if_needed old_global new_grammar_opt should_update_grammar= 
  match new_grammar_opt with
  None -> old_global
  |Some new_grammar -> 
    if should_update_grammar 
    then {old_global with managed_grammar=new_grammar;}
    else old_global;;  

let change_head old_global new_head= 
     {
      old_global with 
      head = new_head ;
     } ;;




let pass_to_tail old_global = 
   match old_global.tail with 
   [] -> raise End_reached 
   |prod::other_prods ->
     {
      old_global with 
      head = prod ;
      tail = other_prods ;
     } ;;

let parse_token_type_sequence_opt dname =
  if Jvng_duplicated_name.index dname <>1 then None else 
  let name = Jvng_duplicated_name.name dname in 
  try Some(Jvsp_util.token_type_sequence_from_codes_in_production_names name) with 
  _ -> None ;;      

let possible_first_tokens_opt global dname = 
  let trial1 = parse_token_type_sequence_opt dname in 
  if trial1<>None then trial1 else
  List.assoc_opt dname global.battery.precomputed_first_tokens ;;



let easy_decision_that_analizer_head_is_used1 old_global = 
  match old_global.tail with 
  [] -> false 
  | dname2 :: _others ->
    match parse_token_type_sequence_opt dname2 with 
    None -> false
    |(Some l) -> 
      (List.hd l)<>(List.hd(Jvsp_token_types_list.unveil(old_global.consumable.remaining_list))) 
    ;;

let easy_decision_that_analizer_head_is_used2 old_global = 
  match old_global.tail with 
  [] -> false 
  | dname2 :: _others ->
    match possible_first_tokens_opt old_global dname2 with 
    None -> false
    |(Some first_toks) -> 
      let stream_head = List.hd(Jvsp_token_types_list.unveil(old_global.consumable.remaining_list)) in 
      not(List.mem stream_head first_toks) 
    ;;

let easy_decision_that_analizer_head_is_not_used old_global nm= 
    match possible_first_tokens_opt old_global nm with 
    None -> false
    |(Some first_toks) -> 
      let stream_head = List.hd(Jvsp_token_types_list.unveil(old_global.consumable.remaining_list)) in 
      not(List.mem stream_head first_toks) 
    ;;


let easy_decision_that_analizer_head_is_used old_global =
   (easy_decision_that_analizer_head_is_used1 old_global) ||
   (easy_decision_that_analizer_head_is_used2 old_global)  ;;




let step old_global = 
   let old_man = old_global.managed_grammar in 
   let (production,new_man_opt) = Jvng_with_ancestry_manager.get old_man old_global.head in 
   let decomposition_opt = Jvng_jvag_form.uniform_decomposition_opt production in 

   let (before_updating_man,should_update_man) = (
    match decomposition_opt with 
    None ->
        let name = Jvng_duplicated_name.name old_global.head in 
        let toktypes = Jvsp_util.token_type_sequence_from_codes_in_production_names name in 
        let remaining = Jvsp_token_types_list.unveil old_global.consumable.remaining_list in 
       let (common,left,_right) = List_again.common_initial_sublist toktypes remaining in 
       if left<>[]
       then raise(Blocked_state_exn(old_global))
       else let k = List.length common in 
            (pass_to_tail({
              old_global with 
              consumable = Jvng_stream.consume old_global.consumable k; 
            }),true) 
    |Some(link,coatoms) ->  
   match link with 
     Jvag_types.Optional_L -> 
       let nm = List.hd coatoms in 
       let is_used = (
        if (old_global.tail=[])||(easy_decision_that_analizer_head_is_used old_global)
        then true
        else 
        if easy_decision_that_analizer_head_is_not_used old_global nm 
        then false
        else    
        Jvng_battery_of_analizers.decide old_global.battery nm old_global.consumable) in  
       if is_used 
       then (change_head old_global nm,true)
       else (pass_to_tail old_global,false)  
  |Jvag_types.Concat_L -> 
       ({
              old_global with 
              head = (List.hd coatoms);
              tail = (List.tl coatoms)@old_global.tail ;
        },true)
  |Jvag_types.Disjunction_L ->  
       let choice = Jvng_battery_of_analizers.choose old_global.battery old_global.head old_global.consumable in 
       (change_head old_global choice, true)
  |Jvag_types.Star_L ->  
       let nm = List.hd coatoms in 
       let is_used = (
        if (old_global.tail=[])||(easy_decision_that_analizer_head_is_used old_global)
        then true
        else 
        if easy_decision_that_analizer_head_is_not_used old_global nm 
        then false
        else      
        Jvng_battery_of_analizers.decide old_global.battery  nm old_global.consumable) in  
       if is_used 
       then ({
              old_global with 
              head = nm;
              tail = old_global.head :: old_global.tail ;
            },true)
       else (pass_to_tail old_global,false)  
  |Jvag_types.Synonym_L -> 
       let nm = List.hd coatoms in 
      (change_head old_global nm, true)) in 
  put_new_grammar_if_needed before_updating_man new_man_opt should_update_man;;
    
let make source origin batt tok_types=
    let man = Jvng_with_ancestry_manager.make source origin in 
    let form =  Jvag_grammar.get source origin in 
    let coats = Image.image Jvng_duplicated_name.of_string (Jvag_form.unordered_coatoms form) in 
    {
      managed_grammar =man ;
      battery = batt;
      head = List.hd(coats) ;
      tail = List.tl(coats);
      consumable =  {
        cursor = 1 ;
       remaining_list = Jvsp_token_types_list.construct  tok_types ;
      };
    } ;;


let enclose dname = "\""^(Jvng_duplicated_name.to_string dname)^"\"" ;;


let to_string global =
  let head = enclose global.head 
  and tail = "["^(String.concat ";" (Image.image enclose global.tail))^"]" in
   "(...)"^head^","^tail^",\n\n\n"^(string_of_int(global.consumable.cursor))^",\n\n"^(Jvsp_token_types_list.to_string global.consumable.remaining_list);;    
 
end ;;   

let make = Private.make ;;

(* This is a registered printer : print_out *)
let print_out (fmt:Format.formatter) global=
   Format.fprintf fmt "@[%s@]" (Private.to_string global);;

let step = Private.step ;;