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
   
let ocaml_name_of_modification = function 
    Set_production(name,form) -> "Set_production(\""^name^"\","^(Jvag_form.ocaml_name form)^")" 
  |Rename(old_name,new_name) -> "Rename(\""^old_name^"\",\""^new_name^"\")"     
  |Remove_productions(l) -> "Remove_productions(["^(String.concat ";" (Image.image (fun s->"\""^s^"\"") l))^"])"   
  |Register_with_standardized_name(form) -> "Register__with_standardized_name("^(
    Jvag_form.ocaml_name form)^")"
  |Expand_in_disjunction(contained,container) -> "Expand_in_disjunction(\""^contained^"\",\""^container^"\")"
  |Expand_in_synonym(name_for_content,container) -> "Expand_in_synonym(\""^name_for_content^"\",\""^container^"\")"
  |Collapse_synonym_locally(newer_synonym,container) -> "Collapse_synonym_locally(\""^newer_synonym^"\",\""^container^"\")"
  |Collapse_synonym_globally(newer_synonym) -> "Collapse_synonym_globally(\""^newer_synonym^"\")" 
  |Flatten_triangle(triangle_name) ->  "Flatten_triangle(\""^triangle_name^"\")"
  |Flatten_tetris1(tetris1_name) -> "Flatten_tetris1(\""^tetris1_name^"\")"
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

let add_pair_naively pair (AL l) = 
  let (name,_form) = pair in 
  let new_l = (
    match List.assoc_opt name l with 
    None -> pair :: l 
    |Some _ -> Image.image (fun pair2->
      if (fst pair2)=name then pair else pair2 ) l
  )  in 
  AL(Ordered.sort order_on_pairs new_l);; 

let name_form_if_needed gram form new_name =
  match name_for_form_opt gram form with 
  Some old_name ->(None,old_name)
  |None -> 
     if get_opt gram new_name <> None then raise(Name_already_in_use(new_name)) else  
     let new_ag = add_pair_naively (new_name,form) gram in 
     (Some new_ag,new_name) ;;

let rec helper_for_naming_several_forms (treated,change_made,gram,to_be_treated) =
  match to_be_treated with 
  []->((if change_made then Some gram else None),List.rev treated)
  |(form1,name1) :: other_pairs ->
     let (new_gram_opt,final_name) = name_form_if_needed gram form1 name1 in 
     let treated2 = final_name :: treated in 
     match new_gram_opt with 
     None ->  helper_for_naming_several_forms (treated2,change_made,gram,other_pairs)
     |(Some new_gram) ->  helper_for_naming_several_forms (treated2,true,new_gram,other_pairs) ;;
           
let name_several_forms_as_needed gram pairs = 
   helper_for_naming_several_forms ([],false,gram,pairs) ;; 


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

exception Standardized_name_exn of string ;;   

let standardized_name = function 
   Concat l -> "CCCCC"^(String.concat "NNNNN" l)^"TTTTT"
   |Molecular token_types  -> Jvsp_util.code_for_tokentype_sequence_in_production_names token_types
   |Disjunction _ -> raise(Standardized_name_exn("Disjunction"))
   |Star nm -> "Starred"^ nm
   |Optional nm -> "Optional"^ nm
   |Synonym _ -> raise(Standardized_name_exn("Synonym")) ;;  

let register_with_standardized_name form gram= 
   let name = standardized_name form in 
   add_pair_naively (name,form) gram ;;

let give_standardized_name_if_needed gram form = 
  name_form_if_needed gram form (standardized_name form) ;;

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

let triangle_parameters gram triangle_name =
   let form1 = get gram triangle_name in 
   match Jvag_form.disjunction_content_opt form1 with 
   None -> None 
   |Some li1 -> 
     let temp1 = Image.image (fun n->
       let f = get gram n in 
      (n,f,Jvag_form.concat_content_opt f)) li1 in 
      let (temp2,temp3) = List.partition (fun (_n,_f,l_opt)->l_opt=None) temp1 in 
      if (List.length(temp2),List.length(temp3))<>(1,1)
      then None
      else 
      let (name_for_core,_,_) = List.hd temp2  
      and (name_for_compound,_,li3_opt) = List.hd temp3 in 
      let li3 = Option.get li3_opt in
      if List.hd(li3)<>triangle_name then None else 
      let li4 = List.tl li3 in 
      let (new_gram_opt,name_for_extender) = (
         if List.length(li4)=1 then (None,List.hd li4) else 
         let form4 = Jvag_types.Concat li4 in 
         give_standardized_name_if_needed gram form4
      ) in 
      let gram2 = (match new_gram_opt with Some gramm->gramm |None ->gram) in
      let starred_extender = Jvag_types.Star name_for_extender in 
      let (new_gram_opt2,name_for_starred_extender) = give_standardized_name_if_needed gram2 starred_extender in
      let gram3 = (match new_gram_opt2 with Some gramm->gramm |None ->gram2) in
      let final_gram_opt = (if (new_gram_opt,new_gram_opt2)=(None,None) then None else Some gram3) in  
      Some(triangle_name,name_for_core,name_for_compound,
      name_for_extender,name_for_starred_extender,final_gram_opt) ;;


let flatten_triangle gram triangle_name = 
   match triangle_parameters gram triangle_name with 
   None -> raise(Flatten_triangle_exn(triangle_name))
   |Some(triangle_name,name_for_core,name_for_compound,
      name_for_extender,name_for_starred_extender,final_gram_opt) ->
   let gram2 = (match final_gram_opt with Some gramm->gramm |None ->gram) in    
   let gram3 = heavy_add_pair (triangle_name,Jvag_types.Concat([name_for_core;name_for_starred_extender])) gram2 in 
   let gram4 = heavy_add_pair (name_for_compound,Jvag_types.Concat([name_for_core;name_for_extender;name_for_starred_extender])) gram3 in 
   let gram5 = (
      if containing name_for_compound gram4 = []
      then remove_productions [name_for_compound] gram4 
      else gram4 
   ) in 
  gram5;; 

let tetris1_parameters gram tetris1_name =
   let form1 = get gram tetris1_name in 
   match Jvag_form.disjunction_content_opt form1 with 
   None -> None 
   |Some li1 -> 
     let temp1 = Image.image (fun n->
       let f = get gram n in 
      (n,f,Jvag_form.concat_content_opt f)) li1 in 
      let (temp2,temp3) = List.partition (fun (_n,_f,l_opt)->l_opt=None) temp1 in 
      if (List.length(temp2),List.length(temp3))<>(1,1)
      then None
      else 
      let (name_for_core,_,_) = List.hd temp2  
      and (_,_,li3_opt) = List.hd temp3 in 
      let li3 = Option.get li3_opt in
      if List.length(li3)<>3 then None else 
      let ext1 = List.nth li3 1 
      and ext2 = List.nth li3 2 in 
      if (List.hd(li3)<>name_for_core)||(ext2<>"Starred"^ext1) then None else 
      Some(name_for_core,ext1,ext2) ;;   

let flatten_tetris1 gram tetris1_name = 
   match tetris1_parameters gram tetris1_name with 
   None -> raise(Flatten_tetris1_exn(tetris1_name))
   |Some(name_for_core,_ext,starred_ext) ->
   heavy_add_pair (tetris1_name,Jvag_types.Concat([name_for_core;starred_ext])) gram ;; 

let apply gram = function 
   (Set_production(name,form)) -> add_pair_naively (name,form) gram 
  |Rename(old_name,new_name) -> rename_on_grammar (old_name,new_name) gram
  |Remove_productions(to_be_removed) -> remove_productions to_be_removed gram
  |Register_with_standardized_name(form) -> register_with_standardized_name form gram 
  |Expand_in_disjunction(contained,container) -> eid_in_grammar (contained,container) gram
  |Expand_in_synonym(name_for_content,container) -> eis_in_grammar (name_for_content,container) gram
  |Collapse_synonym_locally(newer_synonym,container) -> csl_in_grammar (newer_synonym,container) gram
  |Collapse_synonym_globally(newer_synonym) -> csg_in_grammar newer_synonym gram
  |Flatten_triangle(triangle_name) -> flatten_triangle gram triangle_name
  |Flatten_tetris1(tetris1_name) -> flatten_tetris1 gram tetris1_name;;
 

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
  let gram2 = Modify.add_pair_naively pair gram in 
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
  |Rename(_,_) 
  |Remove_productions(_) 
  |Register_with_standardized_name(_) 
  |Expand_in_disjunction(_,_) 
  |Expand_in_synonym(_,_) 
  |Collapse_synonym_locally(_,_) 
  |Collapse_synonym_globally(_) 
  |Flatten_triangle(_)
  |Flatten_tetris1(_) -> Modify.apply gram modification;;
 

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

let add_pair_naively = Private.Modify.add_pair_naively ;;

let check_disjunction_ladder = Private.check_disjunction_ladder ;; 
let containing = Private.containing ;;
let differences = Private.differences ;;
let extract_at_names = Private.WriteParser.extract_at_names ;;
let get = Private.get ;;

let get_opt (AL l) name = List.assoc_opt name l ;;
let get_and_display = Private.get_and_display ;;

let just_below = Private.just_below ;;
let lower_interval_below = Private.lower_interval_below ;;

let modify = Private.HeavyModify.apply_several ;;
let name_for_form_opt = Private.name_for_form_opt ;; 
let name_several_forms_as_needed = Private.Modify.name_several_forms_as_needed ;;
let ocaml_name = Private.ocaml_name ;;
let order_on_pairs = Private.order_on_pairs ;;

let registration_opt (AL l) form = 
    Option.map fst (List.find_opt (fun (_,form2)->form2=form) l) ;;

let singleton name form = AL [name,form] ;;     
let write_parser = Private.WriteParser.write_prsrtxt ;;