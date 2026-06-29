(*

#use"lib/Java_analysis/Jvsp_abstract_grammar/jvag_grammar.ml";;

*)

open Jvag_types ;;

exception Get_exn of string ;;
exception Circularity of string * (string list) ;; 
exception Name_already_in_use of string ;;
exception Flatten_triangle_exn of string ;;
exception Flatten_tetris1_exn of string ;;

module Private = struct 

  let str_order = Total_ordering.lex_for_strings ;;
  let str_fold_merge = Ordered.fold_merge str_order ;;
  let str_insert= Ordered.insert str_order ;;
  let str_intersect= Ordered.intersect str_order ;;
  let str_mem= Ordered.mem str_order ;;
  let str_merge= Ordered.merge str_order ;;
  let str_setminus = Ordered.setminus str_order ;;
  let str_sort = Ordered.sort str_order ;;

let ocaml_name_of_sf (name,frm) =
    "(\""^name^"\","^(Jvag_form.ocaml_name frm)^")";;

let ocaml_name (AL l)=
  let lines = Image.image (fun sf->(String.make 3 ' ')^(ocaml_name_of_sf sf)^";") l in 
"AL ([\n\n"^
(String.concat "\n" lines)^
"\n\n])" ;; 

let get_opt (AL l) name = List.assoc_opt name l ;;

let name_for_form_opt (AL l) form = List_again.assoc_right_opt form l ;;

let get gram name = match get_opt gram name  with 
  None -> raise(Get_exn(name))
 |Some answer -> answer ;;

let concat_element_to_enhanced_string name form = match form with
   Disjunction(_) -> name
  |Optional(_) 
  |Molecular(_)    
  |(Concat _) 
  |(Star _) 
  |Synonym(_) -> Jvag_form.to_string form;;

let concat_to_enhanced_string gram l = 
    String.concat " " (Image.image (fun nm->concat_element_to_enhanced_string nm (get gram nm)) l) ;;

let adhoc_disjunction_to_string l = 
    "\n"^(String.concat "\n" (Image.image (fun elt->
      "    |"^elt) l)) ;; 

let form_to_enhanced1_string gram form = match form with
   (Concat l) -> concat_to_enhanced_string gram l
   |Disjunction(l) -> adhoc_disjunction_to_string l
  |Optional(_)   
  |Molecular(_)    
  |(Star _) 
  |Synonym(_) -> Jvag_form.to_string form;;

let form_to_enhanced2_string gram form= match form with
   Disjunction(l) ->
     "\n"^(String.concat "\n" (Image.image (fun elt->
      let expanded_elt = get gram elt in 
      elt^" : "^(form_to_enhanced1_string gram expanded_elt)) l))^"\n"  
  |Concat(l) ->  concat_to_enhanced_string gram l     
  |Optional(_) 
  |Molecular(_)    
  |(Star _) 
  |Synonym(_) -> Jvag_form.to_string form;;


let get_and_display gram name =
   let form = get gram name in 
   let _ = (
      if Jvag_form.needs_extra_display form 
      then let msg = "\n\n\n" ^ (form_to_enhanced2_string gram form) ^ "\n\n\n" in 
           print_string msg;flush stdout
   ) in 
   form ;;
   
let ocaml_name_of_local_modification lmod=
  let soi =string_of_int in 
  match lmod with 
  (Lm_expand_disjunction(index_in_disj,index_in_concat)) ->
    "Lm_expand_disjunction("^(soi index_in_disj)^","^(soi index_in_concat)^")" 
 |(Lm_expand_synonym(index_in_disj,index_in_concat)) ->
    "Lm_expand_synonym("^(soi index_in_disj)^","^(soi index_in_concat)^")"
 |(Lm_expand_concat(index_in_disj,index_in_concat)) ->
   "Lm_expand_concat("^(soi index_in_disj)^","^(soi index_in_concat)^")"
 |(Lm_implode_molecule(index_in_disj,(range_start,range_end))) ->  
  "Lm_implode_molecule("^(soi index_in_disj)^",("^(soi range_start)^","^(soi range_end)^"))"
 |(Lm_explode_molecule(index_in_disj,index_in_concat)) ->
   "Lm_explode_molecule("^(soi index_in_disj)^","^(soi index_in_concat)^")" 
 |(Lm_reunite_star(index_in_disj,(length_before,length_after))) ->  
  "Lm_reunite_star("^(soi index_in_disj)^",("^(soi length_before)^","^(soi length_after)^"))"
 |(Lm_detect_optional(index_in_disj,(length_before,length_after))) ->  
  "Lm_detect_optional("^(soi index_in_disj)^",("^(soi length_before)^","^(soi length_after)^"))"  
 |(Lm_reunite_disjunction((disj_range_start,disj_range_end),index_in_concat)) ->
  "Lm_reunite_disjunction(("^(soi disj_range_start)^","^(soi disj_range_end)^"),"^(soi index_in_concat)^")"
 |(Lm_implode_concat(index_in_disj,(range_start,range_end))) ->  
  "Lm_implode_concat("^(soi index_in_disj)^",("^(soi range_start)^","^(soi range_end)^"))" 
 |(Lm_pumping_lemma(original_name,index_in_disj)) ->  
   "Lm_pumping_lemma(\""^original_name^"\","^(soi index_in_disj)^")"
 |(Lm_collapse_synonym(index_in_disj)) ->
    "Lm_collapse_synonym("^(soi index_in_disj)^")"  
;;

let ocaml_name_of_modification = function 
    Set_production(name,form) -> "Set_production(\""^name^"\","^(Jvag_form.ocaml_name form)^")" 
  |Create_production(name,form) -> "Set_production(\""^name^"\","^(Jvag_form.ocaml_name form)^")"   
  |Rename(old_name,new_name) -> "Rename(\""^old_name^"\",\""^new_name^"\")"     
  |Remove_productions(l) -> "Remove_productions(["^(String.concat ";" (Image.image (fun s->"\""^s^"\"") l))^"])"   
  |Register_with_standardized_name(form) -> "Register__with_standardized_name("^(
    Jvag_form.ocaml_name form)^")"
  |Expand_in_disjunction(contained,container) -> "Expand_in_disjunction(\""^contained^"\",\""^container^"\")"
  |Expand_in_synonym(name_for_content,container) -> "Expand_in_synonym(\""^name_for_content^"\",\""^container^"\")"
  |Collapse_synonym_locally(newer_synonym,container) -> "Collapse_synonym_locally(\""^newer_synonym^"\",\""^container^"\")"
  |Collapse_synonym_globally(newer_synonym) -> "Collapse_synonym_globally(\""^newer_synonym^"\")" 
  |Local(name,mods)->  "Local(\""^name^"\",["^(String.concat ";" 
     (Image.image ocaml_name_of_local_modification mods))^"])"
;;

let ocaml_name_of_modification_list l = 
  "[\n"^
  (String.concat "\n" (Image.image (fun modif->"   "^(ocaml_name_of_modification modif)^";") l))^
  "\n]" ;;
     

let order_on_pairs = Total_ordering.product str_order Jvag_form.order ;;


let is_contained_in_pair nm (_name,form) = Jvag_form.is_contained_in nm form;;

let containing nm (AL l) = List.filter(is_contained_in_pair nm) l;;


let rec helper_for_lower_interval_below (gram,treated,to_be_treated) = 
  match to_be_treated with 
  [] -> treated 
  |name :: other_names ->
     let coats = Jvag_form.coatoms (get gram name) in 
     let new_coats = str_setminus coats treated in 
     helper_for_lower_interval_below (gram,str_insert name treated,str_merge new_coats other_names) 
  ;;

let lower_interval_below gram name = helper_for_lower_interval_below (gram,[name],[name]) ;;

let just_below gram name = (Jvag_form.unordered_coatoms (get gram name)) ;;

exception Bad_step_in_ladder_exn of string ;;
exception Strange_step_in_ladder_exn of string ;;
exception Bad_link_in_ladder_exn of string * string * (string list) ;;

let test_disjunction_ladder_link gram (x,y) =
  match get_opt gram x with 
  None -> raise(Bad_step_in_ladder_exn(x))
  |Some form -> 
       match Jvag_form.disjunction_content_opt form with 
       None -> raise(Strange_step_in_ladder_exn(x))
       |Some l ->
       if not(List.mem y l) 
       then raise(Bad_link_in_ladder_exn(x,y,l))
       else () ;;  


let check_disjunction_ladder gram ladder = 
   let temp1 = List_again.universal_delta_list ladder in   
   List.iter (test_disjunction_ladder_link gram) temp1 ;;

let differences (AL l1) (AL l2) =
  let names = str_merge (Image.image fst l1) (Image.image fst l2) in 
  let data = Image.image (
    fun name ->(name,(List.assoc_opt name l1,List.assoc_opt name l2))
  ) names in 
  List.filter (fun (_name,(opt1,opt2))->opt1<>opt2) data ;;

let add_pair_naively pair (AL l) = 
  let (name,_form) = pair in 
  let new_l = (
    match List.assoc_opt name l with 
    None -> pair :: l 
    |Some _ -> Image.image (fun pair2->
      if (fst pair2)=name then pair else pair2 ) l
  )  in 
  AL(Ordered.sort order_on_pairs new_l);; 

exception Name_is_not_new_exn of string ;;

let create_new_pair pair (AL l) = 
  let (name,_form) = pair in 
  let new_l = (
    match List.assoc_opt name l with 
    None -> pair :: l 
    |Some _ -> raise(Name_is_not_new_exn(name))
  )  in 
  AL(Ordered.sort order_on_pairs new_l);; 


exception Standardized_name_exn of string ;;   

let standardized_name = function 
   Concat l -> "CCCCC"^(String.concat "NNNNN" l)^"TTTTT"
   |Molecular token_types  -> Jvsp_util.code_for_tokentype_sequence_in_production_names token_types
   |Disjunction _ -> raise(Standardized_name_exn("Disjunction"))
   |Star nm -> "Starred"^ nm
   |Optional nm -> "Optional"^ nm
   |Synonym _ -> raise(Standardized_name_exn("Synonym")) ;;  

let register_if_needed gram form = 
   match name_for_form_opt gram form with 
  Some old_name ->(gram,old_name)
  |None -> 
  let new_name = standardized_name form in 
  if get_opt gram new_name <> None then raise(Name_already_in_use(new_name)) else  
  let new_ag = add_pair_naively (new_name,form) gram in  
  (new_ag,new_name) ;;  

let rec helper_for_multiple_registration (treated,gram,to_be_treated) =
  match to_be_treated with 
  [] -> (gram,List.rev treated)
  |form1 :: other_forms ->
    let (gram1,name1) = register_if_needed gram form1 in 
    helper_for_multiple_registration (name1::treated,gram1,other_forms)
  ;;

let register_several_if_needed gram forms = 
      helper_for_multiple_registration ([],gram,forms) ;;


module Local_Modification = struct 
  
let lm_get gram name = 
  let form1 = get gram name in 
  let l =Jvag_form.disjunction_content form1 in 
  Image.image (get gram) l;;

exception Bad_index_exn of string * int * string ;;
exception Bad_range_exn of string * int * int * string ;;
exception Bad_form_exn of  string * int * string * form  * string ;;
exception Bad_sides_in_two_sided_cutting_exn of string * int * int * string * int * int * string ;;
exception Nonuniform_left_in_two_sided_cutting_exn of string * int * int * string * int * int * (string list) * (string list) * string ;;
exception Nonuniform_right_in_two_sided_cutting_exn of string * int * int * string * int * int * (string list) * (string list) * string ;;
exception Nonuniform_sizes_in_extraction_exn of string * int * int *  (string list) * (string list) * string ;;  


let match_concat form (text_for_index,index,caller_name)= 
    match Jvag_form.concat_content_opt form with 
  None -> raise (Bad_form_exn(text_for_index,index,"concat expected",form,caller_name))
  |Some(chain)-> chain ;;

let match_disjunction form (text_for_index,index,caller_name)= 
    match Jvag_form.disjunction_content_opt form with 
  None -> raise (Bad_form_exn(text_for_index,index,"disjunction expected",form,caller_name))
  |Some(cases)-> cases ;;

let match_synonym form (text_for_index,index,caller_name)= 
    match Jvag_form.synonym_content_opt form with 
  None -> raise (Bad_form_exn(text_for_index,index,"synonym expected",form,caller_name))
  |Some(older_synonym)-> older_synonym ;;  

let match_molecular form (text_for_index,index,caller_name)= 
    match Jvag_form.molecular_content_opt form with 
  None -> raise (Bad_form_exn(text_for_index,index,"molecular expected",form,caller_name))
  |Some(older_synonym)-> older_synonym ;;    

let match_concats forms (text_for_index,caller_name)= 
   let indexed_forms = Int_range.index_everything forms in 
   Image.image (fun (index,form)-> match_concat form (text_for_index,index,caller_name)) indexed_forms;; 


let match_moleculars forms (text_for_index,caller_name)= 
   let indexed_forms = Int_range.index_everything forms in 
   Image.image (fun (index,form)-> match_molecular form (text_for_index,index,caller_name)) indexed_forms;;  

let match_concat_opt form (text_for_index,index,caller_name)= 
   try (Some(match_concat form (text_for_index,index,caller_name))) with 
   _ -> None ;;

let extract_element_from_disjunction caller_name forms index_in_disj = 
   if (index_in_disj<0)||(index_in_disj>List.length(forms))
  then raise (Bad_index_exn("index in disjunction",index_in_disj,caller_name))
  else
  let (rev_before,temp) = List_again.long_head_with_tail (index_in_disj-1) forms in 
  let before = List.rev rev_before in 
  let (pivot,after) = List_again.head_with_tail temp in   
  (before,pivot,after) ;;

let extract_element_from_concat caller_name chain index_in_concat = 
   if (index_in_concat<0)||(index_in_concat>List.length(chain))
  then raise (Bad_index_exn("index in concat",index_in_concat,caller_name))
  else
  let (rev_before,temp) = List_again.long_head_with_tail (index_in_concat-1) chain in 
  let before = List.rev rev_before in 
  let (pivot,after) = List_again.head_with_tail temp in 
  (before,pivot,after) ;;

let extract_range_from_concat caller_name chain (range_start,range_end) =  
   if (range_start<0)||(range_end<range_start)||(range_end>List.length(chain))
  then raise (Bad_range_exn("range in concat",range_start,range_end,caller_name))
  else
  let (rev_before,temp) = List_again.long_head_with_tail (range_start-1) chain in 
  let before = List.rev rev_before in 
  let d = range_end - range_start +1 in 
  let (rev_between,after) = List_again.long_head_with_tail d temp in
  (before,List.rev rev_between,after) ;; 

let extract_range_from_disjunction caller_name forms (range_start,range_end) =  
   if (range_start<0)||(range_end<range_start)||(range_end>List.length(forms))
  then raise (Bad_range_exn("range in disjunction",range_start,range_end,caller_name))
  else
  let (rev_before,temp) = List_again.long_head_with_tail (range_start-1) forms in 
  let before = List.rev rev_before in 
  let d = range_end - range_start +1 in 
  let (rev_between,after) = List_again.long_head_with_tail d temp in
  (before,List.rev rev_between,after) ;; 

let uniform_two_sided_cutting (caller_name,range_start,range_end) bars_to_be_cut (length_before,length_after) = 
  let total_length = length_before + length_after 
  and min_length = Min.list(Image.image List.length bars_to_be_cut) in 
  if (length_before<0)||(length_after<0)||(total_length>min_length)
  then raise(Bad_sides_in_two_sided_cutting_exn("range in disjunction",range_start,range_end,"sides",length_before,length_after,caller_name))  
  else 
  let triples= Image.image (List_again.two_sided_cutting (length_before,length_after)) bars_to_be_cut in 
  let left_parts = Image.image (fun (left,_,_)->left) triples 
  and centers = Image.image (fun (_,center,_)->center) triples 
  and right_parts = Image.image (fun (_,_,right)->right) triples in 
  let left0 = List.hd left_parts 
  and right0 = List.hd right_parts in 
  let left_mismatch_opt = List.find_opt (fun l->l<>left0) left_parts in 
  if left_mismatch_opt<>None 
  then raise(Nonuniform_left_in_two_sided_cutting_exn("range in disjunction",range_start,range_end,"sides",length_before,length_after,left0,Option.get left_mismatch_opt,caller_name))  
  else     
  let right_mismatch_opt = List.find_opt (fun r->r<>right0) right_parts in 
  if right_mismatch_opt<>None 
  then raise(Nonuniform_right_in_two_sided_cutting_exn("range in disjunction",range_start,range_end,"sides",length_before,length_after,right0,Option.get right_mismatch_opt,caller_name))  
  else (left0,centers,right0)  ;; 

let uniform_extraction (caller_name,range_start,range_end) bars_to_be_cut index =
  let bar0 = List.hd(bars_to_be_cut) in 
  let n = List.length(bar0) in 
  let bad_length_opt= List.find_opt (fun bar->List.length(bar)<>n) bars_to_be_cut in 
  if bad_length_opt<>None 
  then raise(Nonuniform_sizes_in_extraction_exn("range in disjunction",range_start,range_end,bar0,Option.get bad_length_opt,caller_name))  
  else    
  let (left0,centers,right0) =  
  uniform_two_sided_cutting (caller_name,range_start,range_end) bars_to_be_cut (index-1,n-index) in 
  (left0,Image.image List.hd centers,right0);;  

let expand_disjunction (gram,forms) (index_in_disj,index_in_concat) =
  let (before,old_pivot,after) = extract_element_from_disjunction "expand_disjunction" forms index_in_disj in 
  let chain = match_concat old_pivot ("index in disjunction",index_in_disj,"expand_disjunction") in 
  let (before2,pivot2_name,after2) = extract_element_from_concat "expand_disjunction" chain index_in_concat in  
  let pivot2 = get gram pivot2_name in 
  let inner_disjunction = match_disjunction pivot2 ("index in concat",index_in_concat,"expand_disjunction") in 
  (gram,before @ (Image.image (fun elt->
      Jvag_types.Concat(before2@[elt]@after2)) inner_disjunction) @ after);;

let expand_synonym (gram,forms) (index_in_disj,index_in_concat) =
  let (before,old_pivot,after) = extract_element_from_disjunction "expand_synonym" forms index_in_disj in 
  let chain = match_concat old_pivot ("index in disjunction",index_in_disj,"expand_synonym") in
  let (before2,pivot2_name,after2) = extract_element_from_concat "expand_synonym" chain index_in_concat in   
  let pivot2 = get gram pivot2_name in 
  let older_synonym = match_synonym pivot2 ("index in concat",index_in_concat,"expand_synonym") in 
  (gram,before @ [Jvag_types.Concat(before2@[older_synonym]@after2)]  @ after);;

let expand_concat (gram,forms) (index_in_disj,index_in_concat) =
  let (before,old_pivot,after) = extract_element_from_disjunction "expand_concat" forms index_in_disj in  
  let chain = match_concat old_pivot ("index in disjunction",index_in_disj,"expand_concat") in
  let (before2,pivot2_name,after2) = extract_element_from_concat "expand_concat" chain index_in_concat in   
  let pivot2 = get gram pivot2_name in 
  let chain2 = match_concat pivot2 ("index in concat",index_in_concat,"expand_concat") in
  (gram,before @ [Jvag_types.Concat(before2@chain2@after2)]  @ after);;

let implode_molecule (gram,forms) (index_in_disj,(range_start,range_end)) = 
  let (before,old_pivot,after) = extract_element_from_disjunction "implode_molecule" forms index_in_disj in  
  let chain = match_concat old_pivot ("index in disjunction",index_in_disj,"implode_molecule") in
  let (before2,between2,after2) = extract_range_from_concat "implode_molecule" chain (range_start,range_end) in 
  let forms_between = Image.image(get gram) between2 in 
  let tokens_between = match_moleculars forms_between ("index in range in concat","implode_molecule") in 
  let tokens = List.flatten tokens_between in 
  let molecular = Jvag_types.Molecular tokens in 
  let (gram2,name_for_molecular) = register_if_needed gram molecular in 
  (gram2,before @ [Jvag_types.Concat(before2@[name_for_molecular]@after2)]  @ after);;      

let explode_molecule (gram,forms) (index_in_disj,index_in_concat) = 
  let (before,old_pivot,after) = extract_element_from_disjunction "explode_molecule" forms index_in_disj in  
  let chain = match_concat old_pivot ("index in disjunction",index_in_disj,"explode_molecule") in
  let (before2,pivot2_name,after2) = extract_element_from_concat "explode_molecule" chain index_in_concat in   
  let pivot2 = get gram pivot2_name in 
  let tokens = match_molecular pivot2 ("index in concat",index_in_concat,"explode_molecule") in
  let atoms = Image.image (fun tok->Jvag_types.Molecular [tok]) tokens in 
  let (gram2,names_for_the_atoms) = register_several_if_needed gram atoms in 
  (gram2,before @ [Jvag_types.Concat(before2@names_for_the_atoms@after2)]  @ after);;

let extract_main_name_from_list_during_star_reuniting between =
   if List.length(between) <> 2 then None else 
   let a = List.nth between 0 and b = List.nth between 1 in 
   if b="Starred"^a then Some a else 
   if a="Starred"^b then Some b else 
   None;;


let extract_main_name_from_pair_during_star_reuniting between1 between2 =
   if between1 = [] then extract_main_name_from_list_during_star_reuniting between2 else 
   if between2 = [] then extract_main_name_from_list_during_star_reuniting between1 else  
   None ;;
   

exception Both_are_nonconcats_in_reunite_star_exn of form * form ;;   
exception Reunite_star_exn of (string list) * (string list) ;;
exception Misfit_in_reunite_star_exn of form * (string list) ;;

let deal_with_narrowed_case_in_star_reuniting gram pivot other_chain =
  let common_name = List.hd other_chain in 
  if get gram common_name = pivot 
  then ([common_name],other_chain)
  else raise(Misfit_in_reunite_star_exn(pivot,other_chain));;    

let deal_with_hard_case_in_star_reuniting gram (pivot1,chain1_opt) (pivot2,chain2_opt) =
  (* we assume that chain1_opt and chain2_opt are not both equal to None *)
  if chain1_opt = None then deal_with_narrowed_case_in_star_reuniting gram pivot1 (Option.get chain2_opt) else 
  if chain2_opt = None then deal_with_narrowed_case_in_star_reuniting gram pivot2 (Option.get chain1_opt) else   
  (Option.get chain1_opt,Option.get chain2_opt) ;;


let reunite_star (gram,forms) (index_in_disj,(length_before,length_after)) = 
  let (before,pivot1,almost_after) = extract_element_from_disjunction "reunite_star" forms index_in_disj in  
  let (pivot2,after) = List_again.head_with_tail almost_after in 
  let chain1_opt = match_concat_opt pivot1 ("index in disjunction",index_in_disj,"reunite_star") 
  and chain2_opt = match_concat_opt pivot2 ("index in disjunction",index_in_disj+1,"reunite_star") in
  if (chain1_opt,chain2_opt) = (None,None) 
  then raise(Both_are_nonconcats_in_reunite_star_exn(pivot1,pivot2))  
  else 
  let (chain1,chain2) = deal_with_hard_case_in_star_reuniting gram (pivot1,chain1_opt) (pivot2,chain2_opt) in 
  let (left,between,right) = uniform_two_sided_cutting ("reunite_star",index_in_disj,index_in_disj+1) [chain1;chain2] (length_before,length_after) in 
  let between1 = List.nth between 0 and between2 = List.nth between 1 in 
  match extract_main_name_from_pair_during_star_reuniting between1 between2 with 
  None -> raise (Reunite_star_exn(between1,between2))
  |Some(main_name) ->
  (gram,before @ [Jvag_types.Concat(left@["Starred"^main_name]@right)]  @ after);;      

let extract_main_name_from_list_during_option_detection between =
   if List.length(between) <> 1 
   then None 
   else Some(List.hd between);;

let extract_main_name_from_pair_during_option_detection between1 between2 = 
   if between1 = [] then extract_main_name_from_list_during_option_detection between2 else 
   if between2 = [] then extract_main_name_from_list_during_option_detection between1 else  
   None ;;

exception Detect_optional_exn of (string list) * (string list) ;;   

let detect_optional (gram,forms) (index_in_disj,(length_before,length_after)) =
  let (before,pivot1,almost_after) = extract_element_from_disjunction "detect_optional" forms index_in_disj in  
  let (pivot2,after) = List_again.head_with_tail almost_after in 
  let chain1 = match_concat pivot1 ("index in disjunction",index_in_disj,"detect_optional") 
  and chain2 = match_concat pivot2 ("index in disjunction",index_in_disj+1,"detect_optional") in
  let (left,between,right) = uniform_two_sided_cutting ("detect_optional",index_in_disj,index_in_disj+1) [chain1;chain2] (length_before,length_after) in 
  let between1 = List.nth between 0 and between2 = List.nth between 1 in 
  match extract_main_name_from_pair_during_option_detection between1 between2 with 
  None -> raise (Detect_optional_exn(between1,between2))
  |Some(main_name) ->
   let final_form = Jvag_types.Optional main_name in 
   let (gram2,name_for_final_form) = register_if_needed gram final_form in 
  (gram2,before @ [Jvag_types.Concat(left@[name_for_final_form]@right)]  @ after);;      

let reunite_disjunction (gram,forms) ((disj_range_start,disj_range_end),index_in_concat) =
  let (before,forms_between,after)=extract_range_from_disjunction "reunite_disjunction" forms (disj_range_start,disj_range_end) in 
  let chains_between = match_concats forms_between ("index in range in disjunction","reunite_disjunction") in 
  let (left,centers,right) = uniform_extraction ("reunite_disjunction",disj_range_start,disj_range_end) chains_between index_in_concat in 
  let final_form = Jvag_types.Disjunction centers in 
   let (gram2,name_for_final_form) = register_if_needed gram final_form in 
  (gram2,before @ [Jvag_types.Concat(left@[name_for_final_form]@right)]  @ after);;      

let implode_concat (gram,forms) (index_in_disj,(range_start,range_end)) = 
  let (before,old_pivot,after) = extract_element_from_disjunction "implode_concat" forms index_in_disj in  
  let chain = match_concat old_pivot ("index in disjunction",index_in_disj,"implode_concat") in
  let (before2,between2,after2) = extract_range_from_concat "implode_concat" chain (range_start,range_end) in 
  let final_form = Jvag_types.Concat between2 in 
  let (gram2,name_for_final_form) = register_if_needed gram final_form in 
  (gram2,before @ [Jvag_types.Concat(before2@[name_for_final_form]@after2)]  @ after);;  

exception Pumping_lemma_exn of string * string ;;

let pumping_lemma (gram,forms) original_name index_in_disj = 
  let (before,old_pivot,after) = extract_element_from_disjunction "pumping_lemma" forms index_in_disj in  
  let chain = match_concat old_pivot ("index in disjunction",index_in_disj,"pumping_lemma") in
  let (head,tail) = List_again.head_with_tail chain in 
  if head<>original_name 
  then raise(Pumping_lemma_exn(original_name,head))
  else 
  let (gram2,name_for_form1) = (
    if List.length(tail)=1 
    then (gram,List.hd tail)
    else 
    let form1 = Jvag_types.Concat(tail) in  
    register_if_needed gram form1 
  ) in  
  let form2 = Jvag_types.Star(name_for_form1) in 
  let (gram3,name_for_form2) = register_if_needed gram2 form2 in 
  let (gram4,names_for_others) = register_several_if_needed gram3 (before@after) in 
  let (gram5,name_for_form3) = (
    if List.length(names_for_others)=1 
    then (gram4,List.hd names_for_others)
    else  
    let form3 = Jvag_types.Disjunction names_for_others in   
    register_if_needed gram4 form3
  ) in 
  (gram5,[Jvag_types.Concat([name_for_form3;name_for_form2])]) ;;

let collapse_synonym (gram,forms) index_in_disj =
  let (before,old_pivot,after) = extract_element_from_disjunction "collapse_synonym" forms index_in_disj in 
  let older_name = match_synonym old_pivot ("index in disjunction",index_in_disj,"collapse_synonym") in
  (gram,before @ [Jvag_types.Concat([older_name])]  @ after);;


let apply gf =function 
 (Lm_expand_disjunction(index_in_disj,index_in_concat)) ->
    expand_disjunction gf (index_in_disj,index_in_concat) 
 |(Lm_expand_synonym(index_in_disj,index_in_concat)) ->
   expand_synonym gf (index_in_disj,index_in_concat)
 |(Lm_expand_concat(index_in_disj,index_in_concat)) ->
   expand_concat gf (index_in_disj,index_in_concat)
 |(Lm_implode_molecule(index_in_disj,(range_start,range_end))) ->
   implode_molecule gf (index_in_disj,(range_start,range_end)) 
 |(Lm_explode_molecule(index_in_disj,index_in_concat)) ->
   explode_molecule gf (index_in_disj,index_in_concat)
 |(Lm_reunite_star(index_in_disj,(length_before,length_after))) -> 
    reunite_star gf (index_in_disj,(length_before,length_after)) 
 |(Lm_detect_optional(index_in_disj,(length_before,length_after))) -> 
    detect_optional gf (index_in_disj,(length_before,length_after)) 
 |(Lm_reunite_disjunction((disj_range_start,disj_range_end),index_in_concat)) ->  
   reunite_disjunction gf ((disj_range_start,disj_range_end),index_in_concat)
 |(Lm_implode_concat(index_in_disj,(range_start,range_end))) ->   
    implode_concat gf (index_in_disj,(range_start,range_end))
 |(Lm_pumping_lemma(original_name,index_in_disj)) ->   
    pumping_lemma gf original_name index_in_disj 
 |(Lm_collapse_synonym(index_in_disj)) ->
    collapse_synonym gf index_in_disj   
  ;;


let apply_several gf mods = List.fold_left apply gf mods ;;

end ;;  

module Modify = struct
  
let expand_form_using_concat (name,chain) form = match form with
  (Concat l) -> Concat(List.flatten(Image.image (fun name2->if name2=name then chain else [name2]) l))
   |Disjunction _
   |Molecular  _
   |Star _
   |Optional _ 
   |Synonym _ -> form;;   

let expand_pair_using_concat data (name,form) =
    (name,expand_form_using_concat data form) ;;

let expand_grammar_using_concat data (AL l) =
  AL(Image.image (expand_pair_using_concat data) l);;


let heavy_add_pair pair gram = 
  let gram2 = add_pair_naively pair gram in 
  match snd pair with 
  (Concat l) -> expand_grammar_using_concat (fst pair,l) gram2    
   |Disjunction _
   |Molecular  _
   |Star _
   |Optional _ 
   |Synonym _ -> gram2;;   

let rename_on_name (old_name,new_name) name =
  if name = old_name then new_name else name ;; 
  
let rename_on_form renaming_data form = 
  let rename = rename_on_name renaming_data in 
  match form with
    Concat l -> Concat(Image.image rename l)
   |Molecular _  -> form
   |Disjunction l -> Disjunction(Image.image rename l) 
   |Star nm -> Star (rename nm)
   |Optional nm -> Optional (rename nm)
   |Synonym nm -> Synonym (rename nm) ;;  

let rename_on_pair renaming_data (name,form) =
   (rename_on_name renaming_data name,rename_on_form renaming_data form) ;;    

let rename_on_grammar renaming_data (AL l)=
 let unordered_new_l = Image.image (rename_on_pair renaming_data) l in 
(AL (Ordered.sort order_on_pairs (unordered_new_l))) ;;

let remove_productions to_be_removed (AL l) = 
   AL(List.filter (fun (name,_)->not(List.mem name to_be_removed)) l) ;;

let register_molecular token_types gram =
   let name = Jvsp_util.code_for_tokentype_sequence_in_production_names token_types in 
   add_pair_naively (name,Molecular(token_types)) gram ;;



let register_with_standardized_name form gram= 
   let name = standardized_name form in 
   add_pair_naively (name,form) gram ;;



let eid_in_dijsunction (contained,replacement) l = 
  Disjunction (List_again.nonredundant_version(List.flatten(Image.image(
                                 fun nm -> if nm = contained then replacement else [nm]
                            ) l))) ;;     

let eid_in_named_form (contained,container,replacement) (name,form) = match form with 
   (Disjunction l) -> (if name=container 
                            then eid_in_dijsunction (contained,replacement) l
                            else form)     
   |Concat _
   |Molecular  _
   |Star _
   |Optional _ 
   |Synonym _ -> form;;   

let eid_in_pair triple (name,form) = (name,eid_in_named_form triple(name,form) ) ;;
   
let eid_in_grammar (contained,container) gram =
   let (AL l) = gram 
   and replacement = Jvag_form.disjunction_content (get gram contained) in 
   AL(Image.image(eid_in_pair (contained,container,replacement)) l);;
   
exception Bad_substitution_in_synonym_exn of string * string ;;
let eis_in_named_form (name_for_content,container,actual_content) (name,form) = match form with 
   (Synonym name2_for_content) -> (if name=container 
                   then if name_for_content <> name2_for_content 
                         then raise(Bad_substitution_in_synonym_exn(name_for_content,name2_for_content)) 
                         else actual_content
                  else form)     
   |Disjunction _
   |Concat _
   |Molecular  _
   |Star _
   |Optional _ -> form;;   

let eis_in_pair triple (name,form) = (name,eis_in_named_form triple(name,form) ) ;;   
let eis_in_grammar (name_for_content,container) gram =
   let (AL l) = gram 
   and actual_content = get gram name_for_content in 
   AL(Image.image(eis_in_pair (name_for_content,container,actual_content)) l);;   

let csg_in_form rep_pair form = 
   let (newer_synonym,older_synonym)= rep_pair in 
   let replacer = List_again.replace_if_proposed [rep_pair] in 
   match form with 
   (Synonym nm) -> Synonym(replacer nm)    
   |Disjunction l ->
        let new_l=(
          if List.mem older_synonym l 
          then List.filter (fun x->x<>newer_synonym) l
          else Image.image replacer l) in 
        if List.length(new_l)=1 
        then Synonym older_synonym 
        else Disjunction new_l    
   |Concat l -> Concat(Image.image replacer l)
   |Star nm -> Star(replacer nm)
   |Optional nm -> Optional (replacer nm)
   |Molecular  _ -> form;;   


let csl_in_named_form (newer_synonym,container,older_synonym) (name,form) = 
   if name <> container 
   then form 
   else csg_in_form (newer_synonym,older_synonym) form;;   

let csl_in_pair triple (name,form) = (name,csl_in_named_form triple (name,form) ) ;; 


let csl_in_grammar (newer_synonym,container) gram =
   let (AL l) = gram 
   and older_synonym = Jvag_form.synonym_content(get gram newer_synonym) in 
   AL(Image.image(csl_in_pair (newer_synonym,container,older_synonym)) l);;

let csg_in_pair_opt rep_pair (name,form) = 
  if name = fst(rep_pair)
  then None 
  else Some(name,csg_in_form rep_pair form ) ;; 


let csg_in_grammar newer_synonym gram =
   let (AL l) = gram 
   and older_synonym = Jvag_form.synonym_content(get gram newer_synonym) in 
   AL(List.filter_map(csg_in_pair_opt (newer_synonym,older_synonym)) l);;

let apply_local_modifications gram name mods =
   let start_dis = Local_Modification.lm_get gram name in 
   let (end_gram,end_dis) =  
     Local_Modification.apply_several (gram,start_dis) mods in 
   let (gram2,end_form) = (
      if List.length(end_dis)=1
      then (end_gram,List.hd end_dis)
      else let (temp_gram,end_names) = register_several_if_needed end_gram end_dis in 
           (temp_gram,Jvag_types.Disjunction end_names)     
   ) in   
   heavy_add_pair (name,end_form) gram2 ;;

let apply gram = function 
   (Set_production(name,form)) -> add_pair_naively (name,form) gram 
  |Create_production(name,form) ->  create_new_pair (name,form) gram 
  |Rename(old_name,new_name) -> rename_on_grammar (old_name,new_name) gram
  |Remove_productions(to_be_removed) -> remove_productions to_be_removed gram
  |Register_with_standardized_name(form) -> register_with_standardized_name form gram 
  |Expand_in_disjunction(contained,container) -> eid_in_grammar (contained,container) gram
  |Expand_in_synonym(name_for_content,container) -> eis_in_grammar (name_for_content,container) gram
  |Collapse_synonym_locally(newer_synonym,container) -> csl_in_grammar (newer_synonym,container) gram
  |Collapse_synonym_globally(newer_synonym) -> csg_in_grammar newer_synonym gram
  |Local(name,mods)->apply_local_modifications gram name mods;;
 

let apply_several gram modifications = 
   List.fold_left apply gram modifications ;;

end ;; 

module Redundant_concats = struct
let get_concat_content_opt = function 
   (Concat cc) -> Some cc
   |Molecular _
   |Disjunction _ 
   |Star _
   |Optional _
   |Synonym _ -> None ;;

let concat_parts_inside_opt form = 
    match form with
    Concat l -> Some(l)
   |Molecular _
   |Disjunction _ 
   |Star _
   |Optional _
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
  fun (name,(_initial_list,final_list)) -> Set_production(name,Concat(final_list))
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


let find_realizing_pair_opt toktypes (name,form) = match form with  
   (Molecular ac) -> if ac = toktypes then Some name else None
   |Concat _
   |Disjunction _ 
   |Star _
   |Optional _
   |Synonym _ -> None ;;

let find_realization_opt toktypes (AL l) = 
    List.find_map (find_realizing_pair_opt toktypes) l ;;      
   

let mergeable_tl_subsequences_in_concat_list gram l= 
   let temp1 = Image.image (get gram) l in 
   let temp2 = List_again.connected_fibers Jvag_form.is_a_token_sequence temp1 in 
   List.filter_map (
    fun (_range,seq,is_a_tl_seq) ->
      if is_a_tl_seq && (List.length(seq)>1)
      then  Some (List.flatten(Image.image (fun z->Option.get(Jvag_form.molecular_content_opt z)) seq)) 
      else None
   ) temp2;;

let mergeable_tl_subsequences_in_pair gram (name,form) = 
   match form with
    Concat l ->
      let temp = mergeable_tl_subsequences_in_concat_list gram l in 
      if temp = []
      then None  
      else Some((name,l),temp)
   |Molecular _
   |Disjunction _ 
   |Star _
   |Optional _
   |Synonym _ -> None ;;


let initial_complete_data_on_mergeable_tl_subsequences =Memoized.make (fun gram->
  let (AL pairs) = gram in   
  List.filter_map (mergeable_tl_subsequences_in_pair gram) pairs);; 

let order_on_token_types = 
  ((fun tt1 tt2 ->Total_ordering.lex_for_strings 
     (Jvsp_util.ocaml_name_for_token_type tt1)
      (Jvsp_util.ocaml_name_for_token_type tt2)
     ) : 
     Jvsp_types.token_type  Total_ordering_t.t );; 

let order_on_tl_sequences = Total_ordering.silex_compare order_on_token_types ;; 

let all_mergeable_tl_subsequences = Memoized.make(fun gram -> 
  let temp1 = Image.image (fun (_,seqs)->seqs) (initial_complete_data_on_mergeable_tl_subsequences gram) in 
  let temp2 = List.flatten temp1 in 
  Ordered.sort order_on_tl_sequences temp2) ;;

let data_to_start_naming_all_mergeable_tl_sequences = Memoized.make(fun gram -> 
  let temp1 = all_mergeable_tl_subsequences gram in 
  let temp2 = Image.image(fun seq->(seq,find_realization_opt seq gram)) temp1 in 
  let (found,not_found) = List.partition (fun (_,opt)->opt<>None) temp2 in 
    (Image.image (fun (seq,opt)->(seq,Option.get opt)) found,
     Image.image fst not_found 
    )  
  ) ;; 

let namings_for_registered_mergeable_tl_sequences gram = fst(data_to_start_naming_all_mergeable_tl_sequences gram) ;;

let namings_for_unregistered_mergeable_tl_sequences = Memoized.make(fun gram -> 
  Image.image(fun seq->(seq,Jvsp_util.code_for_tokentype_sequence_in_production_names seq)) 
    (snd(data_to_start_naming_all_mergeable_tl_sequences gram))
  ) ;; 
  

let namings_for_all_mergeable_tl_sequences = Memoized.make(fun gram -> 
  (namings_for_registered_mergeable_tl_sequences gram) @
  (namings_for_unregistered_mergeable_tl_sequences gram)  
) ;; 
let names_involving_mergeable_tl_sequences = Memoized.make(fun gram -> 
  str_sort (Image.image (fun ((name,_),_)->name) (initial_complete_data_on_mergeable_tl_subsequences gram))) ;;

let data_for_merging_tl_sequences = Memoized.make(fun gram -> 
  (names_involving_mergeable_tl_sequences gram,
  namings_for_all_mergeable_tl_sequences gram
  )
  ) ;; 

exception Merge_tl_sequence_in_concat_exn of Jvsp_types.token_type list;;

let merge_tl_sequence_in_concat gram associator l = 
  let temp1 = Image.image (fun nm->(nm,get gram nm)) l in 
  let temp2 = List_again.connected_fibers (fun (_,form)->Jvag_form.is_a_token_sequence form) temp1 in 
  let temp3 = Image.image (
    fun (_range,segment,is_a_tl_segment) ->
      if is_a_tl_segment && (List.length(segment)>1)
      then let seq = List.flatten(Image.image (fun (_,z)->Option.get(Jvag_form.molecular_content_opt z)) segment) in  
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
    Concat l ->Concat (merge_tl_sequence_in_concat gram associator l)
   |Molecular _   
   |Disjunction _ 
   |Star _
   |Optional _
   |Synonym _ -> form ;;

let merge_tl_sequence_in_pair gram data_for_merging pair =
    let (involved_names,associator) = data_for_merging in
    let (name,form) = pair in 
    if not(List.mem name involved_names)
    then pair 
    else (name, merge_tl_sequence_in_form gram associator form) ;; 

let corrections_needed_for_merging_tl_sequences = Memoized.make(fun gram->
   let data_for_merging = data_for_merging_tl_sequences gram 
   and new_pairs = Image.image (fun (a,b)->Set_production(b,Molecular a)) (namings_for_unregistered_mergeable_tl_sequences gram) in 
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
    Concat l -> l
   |Molecular _  -> [] 
   |Disjunction l -> l 
   |Star nm -> [nm]
   |Optional nm -> [nm]
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

module WriteParser = struct 




exception Find_acyclic_ordering_exn of string * (string list list) ;;

let find_acyclic_ordering unordered_l =
  let l = Ordered.sort order_on_pairs unordered_l in  
  let defined_names = str_sort(Image.image fst l) 
  and referenced_names = str_fold_merge(Image.image (fun (_,form)->Jvag_form.coatoms form) l) in 
  let undefined_names = str_setminus referenced_names defined_names in 
  let relative_coatoms = Memoized.make(fun name ->
     let form = List.assoc name l in 
     str_intersect(Jvag_form.coatoms form) defined_names
  ) in  
  let (cycles,acyclic_ordering) = 
     Reconstruct_linear_poset.reconstruct_linear_poset relative_coatoms defined_names in 
  if cycles<>[]
  then raise(Find_acyclic_ordering_exn("Cycles found : ",cycles)) 
  else       
  let defined_names_in_acylic_order = Image.image fst acyclic_ordering in 
  let (ghosts,nonghosts) = List.partition (fun name->relative_coatoms name=[]) defined_names_in_acylic_order in 
  let part1 = Image.image (fun name -> (name,None)) undefined_names
  and part2 = Image.image (fun name -> (name,List.assoc_opt name l)) (ghosts@nonghosts) in 
  part1 @ part2 ;; 

let extract_at_names (AL l) names = 
   List.filter (fun (name,_)->List.mem name names) l ;;  

let uppercase_min = int_of_char 'A' ;;
let uppercase_max = int_of_char 'Z' ;;

let is_uppercase c = 
   let i = int_of_char c in 
   (uppercase_min<=i) && (i<=uppercase_max) ;;

let snake_case_from_camel_case camel_case =
    let n = String.length camel_case in
    let exploded = Int_range.scale (fun j->(j>1,String.get camel_case (j-1)) ) 1 n in 
    let temp = Image.image (
      fun (is_not_the_first_char,c) -> 
        let s = String.make 1 c in 
        if (is_uppercase c) && is_not_the_first_char then "_"^s else s 
    ) exploded in 
    let temp2 =String.lowercase_ascii (String.concat "" temp) in 
    Replace_inside.replace_inside_text ~display_number_of_matches:false ("__","_") temp2;;

(*
    snake_case_from_camel_case "TopLevelClassOrInterfaceDeclaration" ;;
*)
    
let parser_name camel_case = (snake_case_from_camel_case camel_case)^"_prsr" ;;

let prsrtxt_for_concat name l = 
  let sn = string_of_int(List.length l) in 
  "let "^(parser_name name)^" = \n"^
  "   Jvsp"^"_parser.concat"^sn^" \n"^
  "   "^(String.concat " " (Image.image parser_name l))^" ;;" ;;

let prsrtxt_for_atomic name l = 
  "let "^(parser_name name)^" = \n"^
  "   Jvsp"^"_parser.molecular \n"^
  "   ["^(String.concat ";" (Image.image 
    (fun tt->"T."^(Jvsp_util.ocaml_name_for_token_type tt) ) l))^"] ;;" ;;

let prsrtxt_for_disjunction name l = 
  let sn = string_of_int(List.length l) in 
  "let "^(parser_name name)^" = \n"^
  "   Jvsp"^"_parser.dis"^sn^" \n"^
  "   "^(String.concat " " (Image.image parser_name l))^" ;;" ;;

let prsrtxt_for_star name nm = 
  "let "^(parser_name name)^" = \n"^
  "   Jvsp"^"_parser.star "^(parser_name nm)^" ;;" ;;

let prsrtxt_for_optional name nm = 
  "let "^(parser_name name)^" = \n"^
  "   Jvsp"^"_parser.optional "^(parser_name nm)^" ;;" ;;

let prsrtxt_for_synonym name nm = 
  "let "^(parser_name name)^" = "^(parser_name nm)^" ;;" ;;

let prsrtxt_for_undefined name = 
  "let "^(parser_name name)^" = Jvsp_"^"parser.always_fails ;;" ;;  

let prsrtxt_for_pair (name,form_opt) = match form_opt with 
  None -> prsrtxt_for_undefined name 
  |(Some form) ->
  match form with 
   (Concat l) ->  prsrtxt_for_concat name l
   |Molecular l -> prsrtxt_for_atomic name l
   |Disjunction l -> prsrtxt_for_disjunction name l
   |Star nm -> prsrtxt_for_star name nm
   |Optional nm -> prsrtxt_for_optional name nm
   |Synonym nm -> prsrtxt_for_synonym name nm ;;

let prsrtxt_for_pair_list l = 
  "\n\n\n module T = Jvsp_types ;; \n\n"^(String.concat "\n" (Image.image prsrtxt_for_pair l))^"\n\n\n" ;;

let ap_for_prsrtxt = Absolute_path.of_string 
((Sys.getenv "HOME") ^"/Teuliou/OCaml/skeptical_duck/watched/watched_not_githubbed/preparatory_jvsp_parser.ml");;   

let write_prsrtxt l = 
   let acyclic_l =find_acyclic_ordering l in 
   let text = prsrtxt_for_pair_list acyclic_l in 
   Replace_inside.overwrite_between_markers_inside_file 
   ~overwriter:text  ("(* OCaml-generated parser begins here *)","(* OCaml-generated parser ends here *)") 
   ap_for_prsrtxt ;;

end ;;  

module Nonrecursive_grammar = struct 

let order_on_string_pairs = Total_ordering.product str_order str_order ;;

let auxiliary_order2 = (
   (fun pair1 pair2 ->Total_ordering.standard pair1 pair2): 
     (form *(string list)) Total_ordering_t.t ) ;; 

let order_on_nrg_pairs = Total_ordering.product str_order auxiliary_order2 ;;

exception Fatherless of string ;;
exception Unproductive_son of string * string;;

let insert_new old_grammar son son_form = 
  match List.assoc_opt son old_grammar.sons_and_fathers with 
  (Some father) -> raise(Unproductive_son(son,father))
  | None ->
  match List.find_opt (fun (_,(form,_)) ->str_mem son (Jvag_form. coatoms form)) old_grammar.productions with 
  None -> raise(Fatherless(son))
  |(Some (father,(_form,ancestry))) ->
    if List.mem son ancestry 
    then raise(Circularity(son,father::ancestry))
    else     
    let new_item = (son,(son_form,father::ancestry)) in   
  {
   sons_and_fathers  = Ordered.insert order_on_string_pairs (son,father) old_grammar.sons_and_fathers;
   productions = Ordered.insert order_on_nrg_pairs new_item old_grammar.productions;
  };; 



exception Expand_a_second_time_exn2 of string * form;;
exception Expand_a_second_time_exn3 of string * string * (string list);;

let get_from_nonrecursive_grammar provider old_grammar name = 
  match List.assoc_opt name old_grammar.productions with 
   (Some (form,_)) -> (form,None) 
  | None->    
  let form = get provider name in 
  let new_grammar = insert_new old_grammar name form in 
  (form, Some new_grammar) ;;

let singleton provider origin = 
  let original_form = get provider origin in 
  {
   sons_and_fathers  = [];
   productions = [origin,(original_form,[])];
  };; 

  

end ;;  

module HeavyModify = struct 

let expand_form_using_concat (name,chain) form = match form with
  (Concat l) -> Concat(List.flatten(Image.image (fun name2->if name2=name then chain else [name2]) l))
   |Disjunction _
   |Molecular  _
   |Star _
   |Optional _ 
   |Synonym _ -> form;;   

let expand_pair_using_concat data (name,form) =
    (name,expand_form_using_concat data form) ;;

let expand_grammar_using_concat data (AL l) =
  AL(Image.image (expand_pair_using_concat data) l);;

let heavy_add_pair pair gram = 
  let gram2 = add_pair_naively pair gram in 
  let gram3 =(match snd pair with 
  (Concat l) -> expand_grammar_using_concat (fst pair,l) gram2    
   |Disjunction _
   |Molecular  _
   |Star _
   |Optional _ 
   |Synonym _ -> gram2) in 
  Mergeable_token_sequences.merge_tl_sequences_in_grammar gram3;;   


let apply gram modification = match modification with
   (Set_production(name,form)) -> heavy_add_pair (name,form) gram 
  |Create_production(_,_)
  |Rename(_,_) 
  |Remove_productions(_) 
  |Register_with_standardized_name(_) 
  |Expand_in_disjunction(_,_) 
  |Expand_in_synonym(_,_) 
  |Collapse_synonym_locally(_,_) 
  |Collapse_synonym_globally(_) 
  |Local(_,_)-> Modify.apply gram modification;;
 

let apply_several gram modifications = 
   List.fold_left apply gram modifications ;;


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

module Nonrecursive_grammar = struct 

let get = Private.Nonrecursive_grammar.get_from_nonrecursive_grammar ;;  
let singleton = Private.Nonrecursive_grammar.singleton ;;

end ;;  

let add_pair_naively = Private.add_pair_naively ;;

let check_disjunction_ladder = Private.check_disjunction_ladder ;; 
let containing = Private.containing ;;

let debug_bad_list_of_local_modifications gram name local_modifs=
  let orig = Private.Local_Modification.lm_get gram name in 
  let (count_before_bug,the_problematic_local_modif) = 
    Tools_for_debugging.extract_from_fold_left Private.Local_Modification.apply (gram,orig) local_modifs in
  let local_modifs_before_bug = List_again.long_head count_before_bug local_modifs in 
  (count_before_bug,local_modifs_before_bug,
  Private.Local_Modification.apply_several (gram,orig) local_modifs_before_bug,
  the_problematic_local_modif) ;;


let debug_bad_list_of_modifications gram modifs=
  let (count_before_bug,the_problematic_modif) = Tools_for_debugging.extract_from_fold_left Private.HeavyModify.apply gram modifs in 
  let modifs_before_bug = List_again.long_head count_before_bug modifs in 
  (count_before_bug,modifs_before_bug,Private.HeavyModify.apply_several gram modifs_before_bug,the_problematic_modif) ;;



let differences = Private.differences ;;
let extract_at_names = Private.WriteParser.extract_at_names ;;
let get = Private.get ;;

let get_opt (AL l) name = List.assoc_opt name l ;;
let get_and_display = Private.get_and_display ;;

let just_below = Private.just_below ;;
let lower_interval_below = Private.lower_interval_below ;;

let modify = Private.HeavyModify.apply_several ;;
let name_for_form_opt = Private.name_for_form_opt ;; 
let ocaml_name = Private.ocaml_name ;;
let order_on_pairs = Private.order_on_pairs ;;

let singleton name form = AL [name,form] ;;     
let write_parser = Private.WriteParser.write_prsrtxt ;;