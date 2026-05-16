(*

#use"lib/Java_analysis/jvsp_abstract_language.ml";;

*)


     
type form =  Jvsp_abstract_language_t.form = 
    Just_an_optional of string
   |Just_atomic of Jvsp_types.token_type list
   |Just_a_concat of string list  
   |Just_a_disjunction of string list 
   |Just_a_star of string 
   |Synonym of string;;

type t =  Jvsp_abstract_language_t.t = AL of (string * form) list ;; 

type modification = Jvsp_abstract_language_t.modification = 
   Set_production of string * form 
  |Rename of string * string 
  |Remove_productions of string list;;


module Private = struct 

  let str_order = Total_ordering.lex_for_strings ;;
  let str_fold_merge = Ordered.fold_merge str_order ;;
  let str_intersect= Ordered.intersect str_order ;;
  let str_merge= Ordered.merge str_order ;;
  let str_setminus = Ordered.setminus str_order ;;
  let str_sort = Ordered.sort str_order ;;

  

let ocaml_name_of_form = function 
   Just_an_optional(nm) -> "Just_an_optional(\""^nm^"\")"
  |Just_atomic(l) -> "Just_atomic(["^(String.concat ";" (Image.image Jvsp_util.ocaml_name_for_token_type l))^"])"
  |Just_a_concat(l) -> "Just_a_concat(["^(String.concat ";" (Image.image (fun s->"\""^s^"\"") l))^"])"
  |Just_a_disjunction(l) -> "Just_a_disjunction(["^(String.concat ";" (Image.image (fun s->"\""^s^"\"") l))^"])"
  |Just_a_star(nm) -> "Just_a_star(\""^nm^"\")"
  |Synonym(nm) -> "Synonym(\""^nm^"\")";;

let ocaml_name_of_sf (name,frm) =
    "(\""^name^"\","^(ocaml_name_of_form frm)^")";;

let ocaml_name (AL l)=
  let lines = Image.image (fun sf->(String.make 3 ' ')^(ocaml_name_of_sf sf)^";") l in 
"AL ([\n\n"^
(String.concat "\n" lines)^
"\n\n])" ;; 

let get (AL l) name = List.assoc name l ;;

let display_optional nm = "\u{3010}"^nm^"\u{3011}"  ;;
let display_star nm = nm ^ "\u{2605}" ;;
let form_to_string = function 
   Just_an_optional(nm) -> display_optional nm   
  |Just_atomic(l) -> (String.concat " " (Image.image Jvsp_util.summary_of_token_type l))   
  |Just_a_concat(l) ->  String.concat " " l
  |Just_a_disjunction(l) ->
     "\n"^(String.concat "\n" (Image.image (fun elt->
      "|"^elt) l))^"\n"
  |(Just_a_star nm) -> display_star nm 
  |Synonym(nm) -> nm;;

let print_out_form (fmt:Format.formatter) form=
   Format.fprintf fmt "@[%s@]" (form_to_string form);;

let concat_element_to_enhanced_string name form = match form with
   Just_a_disjunction(_) -> name
  |Just_an_optional(_) 
  |Just_atomic(_)    
  |(Just_a_concat _) 
  |(Just_a_star _) 
  |Synonym(_) -> form_to_string form;;

let concat_to_enhanced_string gram l = 
    String.concat " " (Image.image (fun nm->concat_element_to_enhanced_string nm (get gram nm)) l) ;;



let form_to_enhanced1_string gram form = match form with
   (Just_a_concat l) -> concat_to_enhanced_string gram l
  |Just_an_optional(_)   
  |Just_atomic(_)    
  |Just_a_disjunction(_) 
  |(Just_a_star _) 
  |Synonym(_) -> form_to_string form;;

let form_to_enhanced2_string gram form= match form with
   Just_a_disjunction(l) ->
     "\n"^(String.concat "\n" (Image.image (fun elt->
      let expanded_elt = get gram elt in 
      "|"^elt^" : "^(form_to_enhanced1_string gram expanded_elt)) l))^"\n"  
  |Just_a_concat(l) ->  concat_to_enhanced_string gram l     
  |Just_an_optional(_) 
  |Just_atomic(_)    
  |(Just_a_star _) 
  |Synonym(_) -> form_to_string form;;

let needs_extra_display form= match form with
   Just_a_disjunction(_) 
  |Just_a_concat(_) ->  true    
  |Just_an_optional(_) 
  |Just_atomic(_)    
  |(Just_a_star _) 
  |Synonym(_) -> false;;  

let get_and_display gram name =
   let form = get gram name in 
   let _ = (
      if needs_extra_display form 
      then let msg = "\n\n\n" ^ (form_to_enhanced2_string gram form) ^ "\n\n\n" in 
           print_string msg;flush stdout
   ) in 
   form ;;
   
let ocaml_name_of_modification = function 
    Set_production(name,form) -> "Set_production(\""^name^"\","^(ocaml_name_of_form form)^")" 
  |Rename(old_name,new_name) -> "Rename(\""^old_name^"\",\""^new_name^"\")"     
  |Remove_productions(l) -> "Remove_productions(["^(String.concat ";" (Image.image (fun s->"\""^s^"\"") l))^"])"   ;;

let ocaml_name_of_modification_list l = 
  "[\n"^
  (String.concat "\n" (Image.image (fun modif->"   "^(ocaml_name_of_modification modif)^";") l))^
  "\n]" ;;
     

let order_on_forms = (
   (fun form1 form2 ->Total_ordering.standard form1 form2): 
     form Total_ordering_t.t ) ;; 

let order_on_pairs = Total_ordering.product str_order order_on_forms ;;

let is_contained_in_form nm form = match form with
    Just_a_concat l -> List.mem nm l
   |Just_atomic _ -> false
   |Just_a_disjunction l -> List.mem nm l
   |Just_a_star nm2 -> nm2 = nm
   |Just_an_optional nm2 -> nm2 = nm
   |Synonym nm2 -> nm2 = nm ;;

let is_contained_in_pair nm (_name,form) = is_contained_in_form nm form;;

let unordered_coatoms form = match form with
    Just_a_concat l ->  l
   |Just_atomic _ -> []
   |Just_a_disjunction l -> l
   |Just_a_star nm -> [nm]
   |Just_an_optional nm -> [nm]
   |Synonym nm -> [nm] ;;

let coatoms form = str_sort (unordered_coatoms form) ;;

let containing nm (AL l) = List.filter(is_contained_in_pair nm) l;;

exception Find_acyclic_ordering_exn of string * (string list list) ;;

let find_acyclic_ordering unordered_l =
  let l = Ordered.sort order_on_pairs unordered_l in  
  let names = Image.image fst l in 
  let relative_coatoms = Memoized.make(fun name ->
     let form = List.assoc name l in 
     str_intersect(coatoms form) names
  ) in  
  let (cycles,acyclic_ordering) = 
     Reconstruct_linear_poset.reconstruct_linear_poset relative_coatoms names in 
  if cycles<>[]
  then raise(Find_acyclic_ordering_exn("Cycles found : ",cycles)) 
  else       
  let names1 = Image.image fst acyclic_ordering in 
  let (ghosts,nonghosts) = List.partition (fun name->relative_coatoms name=[]) names1 in 
  let names2 = ghosts @ nonghosts in 
  (AL(Image.image (fun name -> (name,List.assoc name l)) names2));; 

let extract_at_names (AL l) names = 
   (AL(List.filter (fun (name,_)->List.mem name names) l)) 


module Modify = struct
  
let add_pair pair (AL l) = 
  let (name,_form) = pair in 
  let new_l = (
    match List.assoc_opt name l with 
    None -> pair :: l 
    |Some _ -> Image.image (fun pair2->
      if (fst pair2)=name then pair else pair2 ) l
  )  in 
  AL(Ordered.sort order_on_pairs new_l);; 

let rename_on_name (old_name,new_name) name =
  if name = old_name then new_name else name ;; 
  
let rename_on_form renaming_data form = 
  let rename = rename_on_name renaming_data in 
  match form with
    Just_a_concat l -> Just_a_concat(Image.image rename l)
   |Just_atomic _  -> form
   |Just_a_disjunction l -> Just_a_disjunction(Image.image rename l) 
   |Just_a_star nm -> Just_a_star (rename nm)
   |Just_an_optional nm -> Just_an_optional (rename nm)
   |Synonym nm -> Synonym (rename nm) ;;  

let rename_on_pair renaming_data (name,form) =
   (rename_on_name renaming_data name,rename_on_form renaming_data form) ;;    

let rename_on_grammar renaming_data (AL l)=
 let unordered_new_l = Image.image (rename_on_pair renaming_data) l in 
(AL (Ordered.sort order_on_pairs (unordered_new_l))) ;;

let remove_productions to_be_removed (AL l) = 
   AL(List.filter (fun (name,_)->not(List.mem name to_be_removed)) l) ;;

let apply gram = function 
   (Set_production(name,form)) -> add_pair (name,form) gram 
  |Rename(old_name,new_name) -> rename_on_grammar (old_name,new_name) gram
  |Remove_productions(to_be_removed) -> remove_productions to_be_removed gram ;;

let apply_several gram modifications = 
   List.fold_left apply gram modifications ;;

end ;; 

module Redundant_concats = struct
let get_concat_content_opt = function 
   (Just_a_concat cc) -> Some cc
   |Just_atomic _
   |Just_a_disjunction _ 
   |Just_a_star _
   |Just_an_optional _
   |Synonym _ -> None ;;

let concat_parts_inside_opt form = 
    match form with
    Just_a_concat l -> Some(l)
   |Just_atomic _
   |Just_a_disjunction _ 
   |Just_a_star _
   |Just_an_optional _
   |Synonym _ -> None ;;

let redundant_concats_inside gram (name,form) = 
  match concat_parts_inside_opt form with
  None -> None 
  |Some l -> 
    let culprits = List.filter_map (fun nm->
      Option.map (fun inner_concat ->(nm,inner_concat))
      (get_concat_content_opt(get gram nm))) l in 
    if culprits<>[]  
    then Some((name,l),culprits) 
    else None;;

let all_redundant_concats =Memoized.make (fun gram->
  let (AL pairs) = gram in   
  List.filter_map (redundant_concats_inside gram)  pairs);; 

let replacements_by_name =Memoized.make (fun gram -> Image.image (
  fun ((name,initial_list),culprits) -> 
    let final_list = List.flatten(Image.image (fun nm->
      match List.assoc_opt nm culprits with 
      None -> [nm] 
      |Some expansion -> expansion) initial_list) in 
    (name,(initial_list,final_list))
) (all_redundant_concats gram)) ;;  

let names_involved_in_replacements = Memoized.make(fun gram -> 
  str_sort (Image.image fst (replacements_by_name gram))) ;;

let corrections_needed_for_redundant_concats = Memoized.make(fun gram -> Image.image (
  fun (name,(_initial_list,final_list)) -> Set_production(name,Just_a_concat(final_list))
) (replacements_by_name gram) );;  

let display_corrections_needed_for_redundant_concats gram = 
   let corrections = corrections_needed_for_redundant_concats gram in 
   let _ = (
     if corrections <> []
     then let msg = "\n\n\n" ^ (ocaml_name_of_modification_list corrections) ^ "\n\n\n" in 
          print_string msg;flush stdout  
   ) in 
   corrections;;  

let remove_immediate_redundant_concats gram = 
   Modify.apply_several gram (corrections_needed_for_redundant_concats gram) ;;

end ;;

module Mergeable_token_sequences = struct

   let get_atomic_content_opt = function 
   (Just_atomic ac) -> Some ac
   |Just_a_concat _
   |Just_a_disjunction _ 
   |Just_a_star _
   |Just_an_optional _
   |Synonym _ -> None ;;

let find_realizing_pair_opt toktypes (name,form) = match form with  
   (Just_atomic ac) -> if ac = toktypes then Some name else None
   |Just_a_concat _
   |Just_a_disjunction _ 
   |Just_a_star _
   |Just_an_optional _
   |Synonym _ -> None ;;

let find_realization_opt toktypes (AL l) = 
    List.find_map (find_realizing_pair_opt toktypes) l ;;      


let is_a_token_sequence form = ((get_atomic_content_opt form)<>None) ;;   

let mergeable_tl_subsequences_in_concat_list gram l= 
   let temp1 = Image.image (get gram) l in 
   let temp2 = List_again.connected_fibers is_a_token_sequence temp1 in 
   List.filter_map (
    fun (_range,seq,is_a_tl_seq) ->
      if is_a_tl_seq && (List.length(seq)>1)
      then  Some (List.flatten(Image.image (fun z->Option.get(get_atomic_content_opt z)) seq)) 
      else None
   ) temp2;;

let mergeable_tl_subsequences_in_pair gram (name,form) = 
   match form with
    Just_a_concat l ->
      let temp = mergeable_tl_subsequences_in_concat_list gram l in 
      if temp = []
      then None  
      else Some((name,l),temp)
   |Just_atomic _
   |Just_a_disjunction _ 
   |Just_a_star _
   |Just_an_optional _
   |Synonym _ -> None ;;


let all_mergeable_tl_subsequences1 =Memoized.make (fun gram->
  let (AL pairs) = gram in   
  List.filter_map (mergeable_tl_subsequences_in_pair gram) pairs);; 

let order_on_token_types = 
  ((fun tt1 tt2 ->Total_ordering.lex_for_strings 
     (Jvsp_util.ocaml_name_for_token_type tt1)
      (Jvsp_util.ocaml_name_for_token_type tt2)
     ) : 
     Jvsp_types.token_type  Total_ordering_t.t );; 

let order_on_tl_sequences = Total_ordering.silex_compare order_on_token_types ;; 

let all_mergeable_tl_subsequences2 = Memoized.make(fun gram -> 
  let temp1 = Image.image (fun (_,seqs)->seqs) (all_mergeable_tl_subsequences1 gram) in 
  let temp2 = List.flatten temp1 in 
  Ordered.sort order_on_tl_sequences temp2) ;;

let not_yet_registered_mergeable_tl_subsequences = Memoized.make(fun gram -> 
  let temp1 = all_mergeable_tl_subsequences2 gram in 
  let temp2 = Image.image(fun seq->(seq,find_realization_opt seq gram)) temp1 in 
  let (good,bad) = List.partition (fun (_seq,realizers)->realizers<>None) temp2 in 
  (Image.image fst bad,Image.image (fun (seq,opt)->(seq,Option.get opt)) good)
  ) ;;
let names_involving_mergeable_tl_sequences = Memoized.make(fun gram -> 
  str_sort (Image.image (fun ((name,_),_)->name) (all_mergeable_tl_subsequences1 gram))) ;;


let new_pairs_merging_tl_sequences = Memoized.make(fun gram -> 
  let (seqs,_) = not_yet_registered_mergeable_tl_subsequences gram in 
  Image.image (fun seq->(Jvsp_util.code_for_tokentype_sequence_in_production_names seq,Just_atomic seq)) seqs
  ) ;; 

let data_for_merging_tl_sequences = Memoized.make(fun gram -> 
  let involved_names = names_involving_mergeable_tl_sequences gram
  and  (seqs,_) = not_yet_registered_mergeable_tl_subsequences gram in 
  (involved_names,Image.image (fun seq->(seq,Jvsp_util.code_for_tokentype_sequence_in_production_names seq)) seqs)
  ) ;; 

exception Merge_tl_sequence_in_concat_exn of Jvsp_types.token_type list;;

let merge_tl_sequence_in_concat gram associator l = 
  let temp1 = Image.image (fun nm->(nm,get gram nm)) l in 
  let temp2 = List_again.connected_fibers (fun (_,form)->is_a_token_sequence form) temp1 in 
  let temp3 = Image.image (
    fun (_range,segment,is_a_tl_segment) ->
      if is_a_tl_segment && (List.length(segment)>1)
      then let seq = List.flatten(Image.image (fun (_,z)->Option.get(get_atomic_content_opt z)) segment) in  
           let seq_name_opt = List.assoc_opt seq associator in 
           (
             match seq_name_opt with 
             None -> raise(Merge_tl_sequence_in_concat_exn(seq)) 
             |Some name_seq -> [name_seq]
           )
      else Image.image fst segment
  ) temp2 in 
  List.flatten temp3 ;; 


let merge_tl_sequence_in_form gram associator form =
  match form with
    Just_a_concat l ->Just_a_concat (merge_tl_sequence_in_concat gram associator l)
   |Just_atomic _   
   |Just_a_disjunction _ 
   |Just_a_star _
   |Just_an_optional _
   |Synonym _ -> form ;;

let merge_tl_sequence_in_pair gram data_for_merging pair =
    let (involved_names,associator) = data_for_merging in
    let (name,form) = pair in 
    if not(List.mem name involved_names)
    then pair 
    else (name, merge_tl_sequence_in_form gram associator form) ;; 

let corrections_needed_for_merging_tl_sequences = Memoized.make(fun gram->
   let data_for_merging = data_for_merging_tl_sequences gram 
   and new_pairs = Image.image (fun (a,b)->Set_production(a,b)) (new_pairs_merging_tl_sequences gram) in 
   let (involved_names,associator) = data_for_merging in  
   let modified_old_pairs = Image.image(
     fun name -> let old_form = get gram name in 
     let new_form = merge_tl_sequence_in_form gram associator old_form in 
     Set_production(name,new_form)
   ) involved_names in 
   (new_pairs,modified_old_pairs)
) ;;

let display_corrections_needed_for_merging_tl_sequences gram = 
   let (new_pairs,modified_old_pairs) = corrections_needed_for_merging_tl_sequences gram in 
   let corrections = new_pairs@modified_old_pairs in 
   let _ = (
     if corrections <> []
     then let msg = "\n\n\n" ^ (ocaml_name_of_modification_list corrections) ^ "\n\n\n" in 
          print_string msg;flush stdout  
   ) in 
    (new_pairs,modified_old_pairs);;  

let merge_tl_sequences_in_grammar gram = 
  let (new_pairs,modified_old_pairs) = corrections_needed_for_merging_tl_sequences gram in 
   Modify.apply_several gram (new_pairs @ modified_old_pairs) ;;


end ;;   

module Name_usage = struct 

let unordered_used_names_in_form form =
  match form with
    Just_a_concat l -> l
   |Just_atomic _  -> [] 
   |Just_a_disjunction l -> l 
   |Just_a_star nm -> [nm]
   |Just_an_optional nm -> [nm]
   |Synonym nm -> [nm] ;;

let used_names_in_form form =
   str_sort (unordered_used_names_in_form form) ;;

let used_names_in_grammar (AL l)=
  let temp = Image.image (fun (_,form)->used_names_in_form form) l in 
  str_fold_merge temp ;;
  
let immediately_unused_names_in_grammar gram =
  let (AL l)= gram in
  let names = Image.image fst l in 
  str_setminus names (used_names_in_grammar gram) ;;  


let remove_immediately_unused_names gram ~exceptions=
  let unused_names = immediately_unused_names_in_grammar gram in 
  let to_be_removed =  str_setminus unused_names exceptions in 
  let (AL l)= gram in 
  (to_be_removed,AL(List.filter (fun (name,_)->
   not(Ordered.mem  Total_ordering.lex_for_strings name to_be_removed)) l));;

let rec helper_for_removing_unused_names exceptions (unused_names,gram1,gram2)=
   if gram1 = gram2  
   then (unused_names,gram1)
   else let (new_unused_names,gram3) = remove_immediately_unused_names gram2 ~exceptions in 
        helper_for_removing_unused_names exceptions (
          str_merge unused_names new_unused_names,gram2,gram3) ;; 

let remove_unused_names gram ~exceptions = 
  let (first_unused_names,gram2) = remove_immediately_unused_names gram ~exceptions in 
  helper_for_removing_unused_names exceptions 
  (first_unused_names,gram,gram2) ;; 

end ;;   



end ;; 


module Preliminary_normalizations = struct

let mergeable_token_sequences = 
      Private.Mergeable_token_sequences.display_corrections_needed_for_merging_tl_sequences ;;
let merge_token_sequences = 
      Private.Mergeable_token_sequences.merge_tl_sequences_in_grammar ;;  
      
let redundant_concats = Private.Redundant_concats.display_corrections_needed_for_redundant_concats ;;
let remove_immediate_redundant_concats = Private.Redundant_concats.remove_immediate_redundant_concats ;;

let remove_unused_names = Private.Name_usage.remove_unused_names ;;

let all gram = 
  let (new_pairs,modified_old_pairs) = mergeable_token_sequences gram in 
  (List.length(new_pairs),List.length(modified_old_pairs),List.length(redundant_concats gram),
 fst(remove_unused_names gram ~exceptions:["OrdinaryCompilationUnit"])) ;; 

end ;;

 

let containing = Private.containing ;;
let get = Private.get ;;

let get_and_display = Private.get_and_display ;;

let modify = Private.Modify.apply_several ;;
let ocaml_name = Private.ocaml_name ;;
let order_on_pairs = Private.order_on_pairs ;;

(* This is a registered printer : print_out_form *)
let print_out_form = Private.print_out_form ;;