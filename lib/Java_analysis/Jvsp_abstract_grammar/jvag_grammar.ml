(*

#use"lib/Java_analysis/Jvsp_abstract_grammar/jvag_grammar.ml";;

*)

open Jvag_types ;;

exception Get_exn of string ;;
exception Circularity of string * (string list) ;; 
exception Name_already_in_use of string ;;

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

let get_opt (AL l) name = 
  match Jvsp_util.token_type_sequence_from_codes_in_production_names_opt name with 
  Some answer -> Some (Molecular answer) 
  | None -> 
    if String.starts_with name ~prefix:"Optional"
    then Some(Optional(Cull_string.two_sided_cutting ("Optional","") name))  
    else
     if String.starts_with name ~prefix:"Starred"
    then Some(Star(Cull_string.two_sided_cutting ("Starred","") name))  
    else  
    List.assoc_opt name l ;;

let automatic_name_for_molecular_opt = function 
   (Molecular l) -> Some(Jvsp_util.code_for_tokentype_sequence_in_production_names l)
  |Disjunction(_)
  |Optional(_) 
  |(Concat _) 
  |(Star _) 
  |Synonym(_) -> None;;


let name_for_form_opt (AL l) form = match form with 
  (Molecular l) -> Some(Jvsp_util.code_for_tokentype_sequence_in_production_names l)
  |(Optional nm) -> Some("Optional"^nm) 
  |(Star nm) -> Some("Starred"^nm)
  |Disjunction(_) 
  |Concat _ 
  |Synonym _ -> List_again.assoc_right_opt  form l ;;

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

let form_to_enhanced3_string gram name form= 
  "\n\n "^name^" = "^(Jvag_form.ocaml_name form)^"\n\n"^
  (form_to_enhanced2_string gram form);;


let get_and_display gram name =
   let form = get gram name in 
   let msg = "\n\n\n" ^ (form_to_enhanced3_string gram name form) ^ "\n\n\n" in 
  let _ =(print_string msg;flush stdout) in 
   form ;;
   
let ocaml_name_of_lid = function 
  I(i)->"I "^(string_of_int i)
  |N(s)->"N \""^s^"\"" ;;   
   
let ocaml_name_of_local_modification lmod=
  let soi =string_of_int in 
  match lmod with 
  (Lm_expand_disjunction(index_in_disj,index_in_concat)) ->
    "Lm_expand_disjunction("^(soi index_in_disj)^","^(soi index_in_concat)^")" 
 |(Lm_remove_left_recursive_line_in_disjunction(original_name,index_in_disj)) ->  
   "Lm_remove_left_recursive_line_in_disjunction(\""^original_name^"\","^(soi index_in_disj)^")"
 |(Lm_collapse_synonym(index_in_disj)) ->
    "Lm_collapse_synonym("^(soi index_in_disj)^")"  
 |(Lm_expand_point_in_line(lid,index_in_concat)) ->  
  "Lm_expand_point_in_line("^(ocaml_name_of_lid lid)^","^(soi index_in_concat)^")"
 |(Lm_reunite_in_concatenation(lid,(range_start,range_end))) ->  
  "Lm_reunite_in_concatenation("^(ocaml_name_of_lid lid)^",("^(soi range_start)^","^(soi range_end)^"))"    
 |(Lm_reunite_in_disjunction(lid_start,lid_end)) ->
  "Lm_reunite_in_disjunction("^(ocaml_name_of_lid lid_start)^","^(ocaml_name_of_lid lid_end)^")"   
;;

let ocaml_name_of_modification = function 
    Set_production(name,form) -> "Set_production(\""^name^"\","^(Jvag_form.ocaml_name form)^")" 
  |Create_production(name,form) -> "Set_production(\""^name^"\","^(Jvag_form.ocaml_name form)^")"   
  |Rename(old_name,new_name) -> "Rename(\""^old_name^"\",\""^new_name^"\")"     
  |Remove_productions(l) -> "Remove_productions(["^(String.concat ";" (Image.image (fun s->"\""^s^"\"") l))^"])"   
  |Expand_in_disjunction(contained,container) -> "Expand_in_disjunction(\""^contained^"\",\""^container^"\")"
  |Expand_in_synonym(name_for_content,container) -> "Expand_in_synonym(\""^name_for_content^"\",\""^container^"\")"
  |Collapse_synonym_locally(newer_synonym,container) -> "Collapse_synonym_locally(\""^newer_synonym^"\",\""^container^"\")"
  |Collapse_synonym_globally(newer_synonym) -> "Collapse_synonym_globally(\""^newer_synonym^"\")" 
  |Local(name,mods)->  "Local(\""^name^"\",["^(String.concat ";" 
     (Image.image ocaml_name_of_local_modification mods))^"])"
;;

let ocaml_name_of_local_modification_list l = 
  "[\n"^
  (String.concat "\n" (Image.image (fun modif->"   "^(ocaml_name_of_local_modification modif)^";") l))^
  "\n]" ;;
     

let ocaml_name_of_modification_list l = 
  "[\n"^
  (String.concat "\n" (Image.image (fun modif->"   "^(ocaml_name_of_modification modif)^";") l))^
  "\n]" ;;
     
let display_local_modification_list l = print_string("\n\n\n"^(ocaml_name_of_local_modification_list l)^"\n\n\n");;  

let display_modification_list l = print_string("\n\n\n"^(ocaml_name_of_modification_list l)^"\n\n\n");;  

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

let replace_pair_or_add_if_absent pair (AL l) = 
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
 

module Dwarf_count = struct 

let i0 = int_of_char '0'
and i9 = int_of_char '9' ;;

let dwarf_marker = "Dwarf" ;;
let dwarf_marker_length = String.length dwarf_marker ;;

let dwarf_number_in_name name = 
  if not(String.starts_with name ~prefix:dwarf_marker)
  then 0 
  else 
  let n = String.length name in   
  let last_digit_position =(
  match String_find_char.from_inclusive_opt (fun c->
    let i=int_of_char c in (i<i0)||(i>i9)
  ) name (dwarf_marker_length+1) with 
  None -> n 
  |Some idx ->idx-1
  )  in
  int_of_string(Cull_string.interval name (dwarf_marker_length+1) last_digit_position) ;;   


(* dwarf_number_in_name "Dwarf273abc" ;; *)

let recompute_dwarf_count_from_scratch (AL l)=
   snd(Max.maximize_it (fun (name,_)->dwarf_number_in_name name) l) ;;

let dwarfy_name ~suffix dwarf_number= function 
    Molecular token_types  -> Jvsp_util.code_for_tokentype_sequence_in_production_names token_types
   |Star nm -> "Starred"^ nm
   |Optional nm -> "Optional"^ nm
   |Concat _ 
   |Disjunction _ 
   |Synonym _  -> dwarf_marker^(string_of_int dwarf_number)^"_"^suffix ;; 

end ;;  



module Mergeable_token_sequences = struct

let merge_tl_sequences_in_concat_perhaps gram l = 
  let temp1 = Image.image (fun nm->(nm,get gram nm)) l in 
  let temp2 = List_again.connected_fibers (fun (_,form)->Jvag_form.is_a_token_sequence form) temp1 in 
  let watcher = ref(false) in 
  let temp3 = Image.image (
    fun (_range,segment,is_a_tl_segment) ->
      if is_a_tl_segment && (List.length(segment)>1)
      then let _= (watcher:=true) in
           let seq = List.flatten(Image.image (fun (_,z)->Option.get(Jvag_form.molecular_content_opt z)) segment) in
           let seq_name = Jvsp_util.code_for_tokentype_sequence_in_production_names seq in   
           [seq_name]
      else Image.image fst segment
  ) temp2 in 
  (List.flatten temp3,!watcher) ;; 


let merge_tl_sequences_in_form_perhaps gram form =
  match form with
    Concat l ->
          let (new_l,action_present)=merge_tl_sequences_in_concat_perhaps gram l in 
            (Concat new_l,action_present)
   |Molecular _   
   |Disjunction _ 
   |Star _
   |Optional _
   |Synonym _ -> (form,false) ;;

let merge_tl_sequences_in_pair_perhaps gram pair =
    let (name,form) = pair in 
    let (new_form,action_present)=merge_tl_sequences_in_form_perhaps gram form in 
    ((name,new_form), action_present) ;; 

let data_about_mergeing_tl_sequences_in_grammar gram =
    let (AL l) = gram in 
    let main = Image.image  (merge_tl_sequences_in_pair_perhaps gram) l in 
    let new_gram = AL(Image.image fst main) 
    and needed_modifications = List.filter_map (fun 
      ((name,form),action_present) -> 
        if action_present 
        then Some(Set_production(name,form))
        else None
      ) main in 
    (new_gram,needed_modifications );; 




end ;;   



module Redundant_concats = struct

(* old version ends here *)

let remove_redundant_concats_in_concat_perhaps gram names = 
  let temp1 = Image.image (fun name->(name,get gram name)) names in 
  let watcher = ref(false) in 
  let new_disjunction = List.flatten(Image.image (
    fun  (name,form) ->
      match Jvag_form.concat_content_opt form with 
      None -> [name]
      |Some l -> 
         let _= (watcher:=true) in
          l
  ) temp1) in 
  (new_disjunction,!watcher) ;; 


let remove_redundant_concats_in_form_perhaps gram form =
  match form with
    Concat l ->
          let (new_l,action_present)=remove_redundant_concats_in_concat_perhaps gram l in 
            (Concat new_l,action_present)
   |Molecular _   
   |Disjunction _ 
   |Star _
   |Optional _
   |Synonym _ -> (form,false) ;;

let remove_redundant_concats_in_pair_perhaps gram pair =
    let (name,form) = pair in 
    let (new_form,action_present)=remove_redundant_concats_in_form_perhaps gram form in 
    ((name,new_form), action_present) ;; 

let data_about_removing_redundant_concats_in_grammar gram =
    let (AL l) = gram in 
    let main = Image.image  (remove_redundant_concats_in_pair_perhaps gram) l in 
    let new_gram = AL(Image.image fst main) 
    and needed_modifications = List.filter_map (fun 
      ((name,form),action_present) -> 
        if action_present 
        then Some(Set_production(name,form))
        else None
      ) main in 
    (new_gram,needed_modifications );; 


end ;;



module Unused_names = struct 

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


let data_about_removing_immediately_unused_names_in_grammar gram ~exceptions=
  let unused_names = immediately_unused_names_in_grammar gram in 
  let to_be_removed =  str_setminus unused_names exceptions in 
  let (AL l)= gram in 
  (AL(List.filter (fun (name,_)->
   not(Ordered.mem  Total_ordering.lex_for_strings name to_be_removed)) l),to_be_removed);;


end ;;   

module Sanitize = struct 

let sanitize_superficially gram =
  let (gram2,unused_names) = Unused_names.data_about_removing_immediately_unused_names_in_grammar 
                                    gram ~exceptions:["OrdinaryCompilationUnit"] in 
  let (gram3,mergeable_tokseqs) = Mergeable_token_sequences.data_about_mergeing_tl_sequences_in_grammar gram2 in 
  let (gram4,redundant_concats) = Redundant_concats.data_about_removing_redundant_concats_in_grammar gram3 in 
  ((unused_names,mergeable_tokseqs,redundant_concats),gram4) ;;

let rec helper_for_sanitization (older_data,gram) =
   let (new_data,new_gram) = sanitize_superficially gram in 
   if new_data=([],[],[]) 
   then (older_data,gram) 
   else helper_for_sanitization (new_data::older_data,new_gram) ;;

let sanitize_as_many_times_as_needed gram = helper_for_sanitization ([],gram) ;; 


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
     Lower_acyclic_subposet.compute relative_coatoms defined_names in 
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


module With_dwarf_count = struct
  
type t = WDC of int * grammar ;;

let make gram =
  let dwarf_count = Dwarf_count.recompute_dwarf_count_from_scratch gram in 
  WDC(dwarf_count,gram) ;;

module Common = struct

let register_with_dwarfy_name_if_needed gram_with_dwc ~suffix form = 
  let (WDC(old_dwarf_count,gram)) = gram_with_dwc in 
   match name_for_form_opt gram form with 
  Some old_name ->(gram_with_dwc,old_name)
  |None -> 
  let new_dwarf_count = old_dwarf_count +1 in 
  let new_name = Dwarf_count.dwarfy_name ~suffix new_dwarf_count form in 
  if get_opt gram new_name <> None then raise(Name_already_in_use(new_name)) else  
  let new_ag = replace_pair_or_add_if_absent (new_name,form) gram in  
  (WDC(new_dwarf_count,new_ag),new_name) ;;  

let rec helper_for_multiple_registration (treated,gram_with_dwc,suffix,to_be_treated) =
  match to_be_treated with 
  [] -> (gram_with_dwc,List.rev treated)
  |form1 :: other_forms ->
    let (gram1_with_dwc,name1) = register_with_dwarfy_name_if_needed gram_with_dwc ~suffix form1 in 
    helper_for_multiple_registration (name1::treated,gram1_with_dwc,suffix,other_forms)
  ;;

let register_several_with_dwarfy_name_if_needed gram ~suffix forms = 
      helper_for_multiple_registration ([],gram,suffix,forms) ;;  

let get (WDC(_dwarf_count,gram)) name = get gram name ;;

let replace_pair_or_add_if_absent pair (WDC(old_dwarf_count,AL l)) = 
  let (name,_form) = pair in 
  let (name_was_already_there,new_l) = (
    match List.assoc_opt name l with 
    None -> (false,pair :: l) 
    |Some _ -> (true,Image.image (fun pair2->
      if (fst pair2)=name then pair else pair2 ) l)
  )  in 
  let new_dwarf_count = (
    if name_was_already_there 
    then old_dwarf_count 
    else max old_dwarf_count (Dwarf_count.dwarf_number_in_name name)  
  ) in
  WDC(new_dwarf_count,AL(Ordered.sort order_on_pairs new_l));; 


let create_new_pair pair (WDC(old_dwarf_count,AL l)) = 
  let (name,_form) = pair in 
  let new_l = (
    match List.assoc_opt name l with 
    None -> pair :: l 
    |Some _ -> raise(Name_is_not_new_exn(name))
  )  
  and new_dwarf_count = max old_dwarf_count (Dwarf_count.dwarf_number_in_name name) in 
  WDC(new_dwarf_count,AL(Ordered.sort order_on_pairs new_l));; 

end ;;

module Local_Modification = struct 

exception Lm_get_exn of form ;;
exception Bad_index_exn of string * int * string ;;
exception Bad_range_exn of string * int * int * string ;;
exception Bad_form_exn of  string * int * string * form  * string ;;
exception Bad_lid_form_exn of  string * location_in_disjunction * string * form  * string ;;

let lm_get gram name = 
  let form = Common.get gram name in 
  match form with 
  Disjunction(l) -> Image.image (fun nm->(nm,Common.get gram nm)) l 
  |Concat(_)->[name,form]
  |Molecular  _
  |Star _
  |Optional _ 
  |Synonym _ -> raise(Lm_get_exn(form));;  
  
let match_named_concat name form = 
    match Jvag_form.concat_content_opt form with 
  None -> [name]
  |Some(chain)-> chain ;;  

let match_named_concats named_forms = 
   Image.image (fun (name,form)-> match_named_concat name form) named_forms;; 

let match_lid_concat form (text_for_index,index,caller_name)= 
    match Jvag_form.concat_content_opt form with 
  None -> raise (Bad_lid_form_exn(text_for_index,index,"concat expected",form,caller_name))
  |Some(chain)-> chain ;;

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

exception Index_for_lid_exn of location_in_disjunction;;

let index_for_lid indexed_forms lid = match lid with   
  (I i)-> i
  |N name -> match List.find_opt(fun (_,(nm,_)) ->nm=name) indexed_forms with 
     None -> raise(Index_for_lid_exn(lid))
     |Some(idx,_) -> idx
   ;;
   

let extract_lid_range_from_disjunction caller_name named_forms (lid_start,lid_end) =  
  let indexed_forms = Int_range.index_everything named_forms in 
  let range_start = index_for_lid indexed_forms lid_start 
  and range_end =  index_for_lid indexed_forms lid_end 
  and n=List.length(named_forms) in 
   if (range_start<0)||(range_end<range_start)||(range_end>n)
  then raise (Bad_range_exn("range in disjunction",range_start,range_end,caller_name))
  else List_again.two_sided_cutting (range_start-1,n-range_end) named_forms ;;    

let extract_lid_from_disjunction caller_name named_forms lid =  
  let (left,center,right) = extract_lid_range_from_disjunction caller_name named_forms (lid,lid) in 
  (left,List.hd center,right) ;;  

let extract_element_from_disjunction caller_name named_forms index_in_disj = 
   if (index_in_disj<0)||(index_in_disj>List.length(named_forms))
  then raise (Bad_index_exn("index in disjunction",index_in_disj,caller_name))
  else
  let (rev_before,temp) = List_again.long_head_with_tail (index_in_disj-1) named_forms in 
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


exception Remove_left_recursive_line_in_disjunction_exn of string * string ;;

exception Compute_lump_optional_for_concatenation_of_two_exn ;;
let compute_lump_optional_for_concatenation_of_two a b =
  if b="Starred"^a then Star(a) else 
  if a="Starred"^b then Star(b) else
  raise Compute_lump_optional_for_concatenation_of_two_exn ;;
    

exception Compute_lump_optional_for_a_concatenation_exn ;;
let rewrite_optional_of_a_concatenation l =
    if List.length(l)=1 
    then Optional(List.hd l)
    else 
    if List.length(l)=2
    then compute_lump_optional_for_concatenation_of_two (List.nth l 0) (List.nth l 1)
    else raise Compute_lump_optional_for_a_concatenation_exn ;;  

exception Compute_lump_in_disjunction_in_binary_case_exn ;;

let compute_lump_in_disjunction_in_binary_case possibly_large_centers = 
   let l1 = List.nth  possibly_large_centers 0 
   and l2 = List.nth  possibly_large_centers 1 in 
   if l1=[] then rewrite_optional_of_a_concatenation l2 else 
   if l2=[] then rewrite_optional_of_a_concatenation l1 else  
   raise Compute_lump_in_disjunction_in_binary_case_exn ;;


exception Compute_lump_in_disjunction_exn ;;

let compute_lump_in_disjunction possibly_large_centers = 
  if List.for_all (fun x->List.length(x)=1) possibly_large_centers 
  then let centers = Image.image List.hd possibly_large_centers in 
       Jvag_types.Disjunction centers
  else 
  if List.length(possibly_large_centers)<>2
  then raise Compute_lump_in_disjunction_exn
  else compute_lump_in_disjunction_in_binary_case possibly_large_centers

let compute_lump_in_concatenation names_between forms_between = 
   let moleculars_opt = Image.image (Jvag_form.molecular_content_opt) forms_between in 
   if List.for_all (fun opt->opt<>None) moleculars_opt
   then Molecular(List.flatten(Image.image Option.get moleculars_opt))
   else Concat names_between ;;

exception Compute_lump_in_nondisjunction_point_expansion_exn of form ;;  

let compute_lump_in_nondisjunction_point_expansion inner_pivot = 
  match inner_pivot with 
   Concat(l)->l
  |Molecular(l)->Image.image (fun tok->Jvsp_util.code_for_tokentype_sequence_in_production_names [tok]) l
  |Synonym (older_name)->[older_name]
  |Star _
  |Optional _ 
  |Disjunction _ -> raise(Compute_lump_in_nondisjunction_point_expansion_exn(inner_pivot));; 

let expand_disjunction (gram,(name,named_forms)) (index_in_disj,index_in_concat) =
  let (before,old_pivot,after) = extract_element_from_disjunction "expand_disjunction" named_forms index_in_disj in 
  let chain = match_concat (snd old_pivot) ("index in disjunction",index_in_disj,"expand_disjunction") in 
  let (before2,pivot2_name,after2) = extract_element_from_concat "expand_disjunction" chain index_in_concat in  
  let pivot2 = Common.get gram pivot2_name in 
  let inner_disjunction = match_disjunction pivot2 ("index in concat",index_in_concat,"expand_disjunction") in 
  let new_elements = Image.image (fun elt-> Jvag_types.Concat(before2@[elt]@after2) ) inner_disjunction in 
  let (gram2,names_for_new_elements) = Common.register_several_with_dwarfy_name_if_needed gram ~suffix:name new_elements in 
  let named_new_elements = List.combine names_for_new_elements new_elements in 
  (gram2,before @ named_new_elements @ after);;  
  

let remove_left_recursive_line_in_disjunction (gram,(name,named_forms)) original_name index_in_disj = 
  let (before,old_pivot,after) = extract_element_from_disjunction "remove_left_recursive_line_in_disjunction" named_forms index_in_disj in  
  let chain = match_concat (snd old_pivot) ("index in disjunction",index_in_disj,"remove_left_recursive_line_in_disjunction") in
  let (head,tail) = List_again.head_with_tail chain in 
  if head<>original_name 
  then raise(Remove_left_recursive_line_in_disjunction_exn(original_name,head))
  else 
  let (gram2,name_for_form1) = (
    if List.length(tail)=1 
    then (gram,List.hd tail)
    else 
    let form1 = Jvag_types.Concat(tail) in  
    Common.register_with_dwarfy_name_if_needed gram ~suffix:(name^"Extender") form1 
  ) in  
  let form2 = Jvag_types.Star(name_for_form1) in 
  let (gram3,name_for_form2) = Common.register_with_dwarfy_name_if_needed gram2 ~suffix:"" form2 in 
  let other_forms = Image.image snd (before@after) in 
  let (gram4,names_for_others) = Common.register_several_with_dwarfy_name_if_needed gram3 ~suffix:"" other_forms in 
  let (gram5,name_for_form3) = (
    if List.length(names_for_others)=1 
    then (gram4,List.hd names_for_others)
    else  
    let form3 = Jvag_types.Disjunction names_for_others in   
    Common.register_with_dwarfy_name_if_needed gram4 ~suffix:"" form3
  ) in 
  (gram5,[name,Jvag_types.Concat([name_for_form3;name_for_form2])]) ;;

let collapse_synonym (gram,(name,named_forms)) index_in_disj = 
  let (before,old_pivot,after) = extract_element_from_disjunction "collapse_synonym" named_forms index_in_disj in 
  let older_name = match_synonym (snd old_pivot) ("index in disjunction",index_in_disj,"collapse_synonym") in
  let new_element = Jvag_types.Concat([older_name]) in 
  let (gram2,name_for_new_element) = Common.register_with_dwarfy_name_if_needed gram ~suffix:name new_element in 
  (gram2,before @ [(name_for_new_element,new_element)]  @ after);;  

let expand_nondisjunction_point_in_line (gram,name) (before,after) (names_before,names_after) inner_pivot=
  let lump = compute_lump_in_nondisjunction_point_expansion inner_pivot in 
  let new_conc_in_disj = Jvag_types.Concat(names_before@lump@names_after) in 
  let (gram2,name_for_new_conc_in_disj) = Common.register_with_dwarfy_name_if_needed gram ~suffix:name new_conc_in_disj in 
  (gram2,before @ [(name_for_new_conc_in_disj,new_conc_in_disj)]  @ after);;  

let expand_disjunction_point_in_line (gram,name) (before,after) (names_before,names_after) inner_disjunction=
 let new_elements = Image.image (fun elt-> Jvag_types.Concat(names_before@[elt]@names_after) ) inner_disjunction in 
 let (gram2,names_for_new_elements) = Common.register_several_with_dwarfy_name_if_needed gram ~suffix:name new_elements in 
 let named_new_elements = List.combine names_for_new_elements new_elements in 
 (gram2,before @ named_new_elements @ after);;  

let expand_point_in_line (gram,(name,named_forms)) (lid,index_in_concat) = 
  let (before,outer_pivot,after) = extract_lid_from_disjunction "expand_point_in_line" named_forms lid in  
  let chain = match_lid_concat (snd outer_pivot) ("index in disjunction",lid,"expand_point_in_line") in
  let (names_before,name_there,names_after) = extract_element_from_concat "expand_concat" chain index_in_concat in   
  let inner_pivot = Common.get gram name_there in 
  match Jvag_form.disjunction_content_opt inner_pivot with 
  None -> expand_nondisjunction_point_in_line (gram,name) (before,after) (names_before,names_after) inner_pivot
  |Some(l) -> expand_disjunction_point_in_line (gram,name) (before,after) (names_before,names_after) l;;
  

let reunite_in_concatenation (gram,(name,named_forms)) (lid,(range_start,range_end)) = 
  let (before,old_pivot,after) = extract_lid_from_disjunction "reunite_in_concatenation" named_forms lid in  
  let chain = match_lid_concat (snd old_pivot) ("index in disjunction",lid,"reunite_in_concatenation") in
  let (names_before,names_between,names_after) = extract_range_from_concat "reunite_in_concatenation" chain (range_start,range_end) in 
  let forms_between = Image.image(Common.get gram) names_between in 
  let lump_form = compute_lump_in_concatenation names_between forms_between in 
  let (gram2,name_for_lump_form) = Common.register_with_dwarfy_name_if_needed gram ~suffix:"" lump_form in
  let new_conc_in_disj = Jvag_types.Concat(names_before@[name_for_lump_form]@names_after) in 
  let (gram3,name_for_new_conc_in_disj) = Common.register_with_dwarfy_name_if_needed gram2 ~suffix:name new_conc_in_disj in 
  (gram3,before @ [(name_for_new_conc_in_disj,new_conc_in_disj)]  @ after);;  

let reunite_in_disjunction (gram,(name,named_forms)) (lid_start,lid_end) = 
  let (before,named_forms_between,after)=extract_lid_range_from_disjunction "reunite_in_disjunction" named_forms (lid_start,lid_end) in 
  let chains_between = match_named_concats named_forms_between  in 
  let (left,centers,right) = List_again.two_sided_common_parts chains_between  in 
  let lump_form = compute_lump_in_disjunction centers in 
   let (gram2,name_for_lump_form) = Common.register_with_dwarfy_name_if_needed gram ~suffix:"" lump_form in 
  let new_conc_in_disj = Jvag_types.Concat(left@[name_for_lump_form]@right) in 
  let (gram3,name_for_conc_in_disj) = Common.register_with_dwarfy_name_if_needed gram2 ~suffix:name new_conc_in_disj in 
  (gram3,before @ [(name_for_conc_in_disj,new_conc_in_disj)]  @ after);;


let apply name (gram,named_forms) modif=
  let gf = (gram,(name,named_forms)) in 
  match modif with 
 (Lm_expand_disjunction(index_in_disj,index_in_concat)) ->
    expand_disjunction gf (index_in_disj,index_in_concat) 
 |(Lm_remove_left_recursive_line_in_disjunction(original_name,index_in_disj)) ->   
    remove_left_recursive_line_in_disjunction gf original_name index_in_disj 
 |(Lm_collapse_synonym(index_in_disj)) ->
    collapse_synonym gf index_in_disj   
  |(Lm_expand_point_in_line(lid,index_in_concat)) ->  
     expand_point_in_line gf (lid,index_in_concat)  
  |(Lm_reunite_in_concatenation(lid,(range_start,range_end))) -> 
    reunite_in_concatenation gf  (lid,(range_start,range_end))  
  |(Lm_reunite_in_disjunction(lid_start,lid_end)) ->
    reunite_in_disjunction gf  (lid_start,lid_end)  
  ;;

let apply_several name gf mods = List.fold_left (apply name) gf mods ;;

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


let add_pair_and_simplify_some_concats_on_bare_grammar pair gram = 
  let gram2 = replace_pair_or_add_if_absent pair gram in 
  match snd pair with 
  (Concat l) -> expand_grammar_using_concat (fst pair,l) gram2    
   |Disjunction _
   |Molecular  _
   |Star _
   |Optional _ 
   |Synonym _ -> gram2;;   

let add_pair_and_simplify_some_concats pair  (WDC(old_dwarf_count,gram)) = 
   WDC(old_dwarf_count,add_pair_and_simplify_some_concats_on_bare_grammar pair gram) ;;  

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

let rename_on_bare_grammar renaming_data (AL l)=
 let unordered_new_l = Image.image (rename_on_pair renaming_data) l in 
(AL (Ordered.sort order_on_pairs (unordered_new_l))) ;;


let rename_on_grammar renaming_data (WDC(old_dwarf_count,gram)) = 
  let new_gram = rename_on_bare_grammar renaming_data gram in 
  let (old_name,new_name) = renaming_data in 
  let old_v=Dwarf_count.dwarf_number_in_name old_name
  and new_v=Dwarf_count.dwarf_number_in_name new_name in 
  let new_dwarf_count = (
     if (old_v<old_dwarf_count) && (new_v<= old_dwarf_count)  
     then old_dwarf_count
     else Dwarf_count.recompute_dwarf_count_from_scratch new_gram
  ) in 
  WDC(new_dwarf_count,new_gram) ;;

let remove_productions to_be_removed gram_with_dwc =
   let (WDC(old_dwarf_count,AL l)) = gram_with_dwc in 
   WDC(old_dwarf_count,AL(List.filter (fun (name,_)->not(List.mem name to_be_removed)) l)) ;;


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
   
let eid_in_grammar (contained,container) gram_with_dwc =
   let (WDC(old_dwarf_count,AL l)) = gram_with_dwc 
   and replacement = Jvag_form.disjunction_content (Common.get gram_with_dwc contained) in 
   WDC(old_dwarf_count,AL(Image.image(eid_in_pair (contained,container,replacement)) l));;
    
   
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
let eis_in_grammar (name_for_content,container) gram_with_dwc =
   let (WDC(old_dwarf_count,AL l)) = gram_with_dwc 
   and actual_content = Common.get gram_with_dwc name_for_content in 
   WDC(old_dwarf_count,AL(Image.image(eis_in_pair (name_for_content,container,actual_content)) l));;    

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


let csl_in_grammar (newer_synonym,container) gram_with_dwc =
   let (WDC(old_dwarf_count,AL l)) = gram_with_dwc 
   and older_synonym = Jvag_form.synonym_content(Common.get gram_with_dwc newer_synonym) in 
   WDC(old_dwarf_count,AL(Image.image(csl_in_pair (newer_synonym,container,older_synonym)) l));;


let csg_in_pair_opt rep_pair (name,form) = 
  if name = fst(rep_pair)
  then None 
  else Some(name,csg_in_form rep_pair form ) ;; 


let csg_in_grammar newer_synonym gram_with_dwc =
   let (WDC(old_dwarf_count,AL l)) = gram_with_dwc 
   and older_synonym = Jvag_form.synonym_content(Common.get gram_with_dwc newer_synonym) in 
   WDC(old_dwarf_count,AL(List.filter_map(csg_in_pair_opt (newer_synonym,older_synonym)) l));;   
 

let apply_local_modifications gram name mods =
   let start_dis = Local_Modification.lm_get gram name in 
   let (gram2,named_forms) =  
     Local_Modification.apply_several name (gram,start_dis) mods in 
   let final_form = (
      if List.length(named_forms)=1
      then snd(List.hd named_forms)
      else Jvag_types.Disjunction (Image.image fst named_forms)    
   ) in   
   add_pair_and_simplify_some_concats (name,final_form) gram2 ;;

let apply gram = function 
   (Set_production(name,form)) -> Common.replace_pair_or_add_if_absent (name,form) gram 
  |Create_production(name,form) ->  Common.create_new_pair (name,form) gram 
  |Rename(old_name,new_name) -> rename_on_grammar (old_name,new_name) gram
  |Remove_productions(to_be_removed) -> remove_productions to_be_removed gram
  |Expand_in_disjunction(contained,container) -> eid_in_grammar (contained,container) gram
  |Expand_in_synonym(name_for_content,container) -> eis_in_grammar (name_for_content,container) gram
  |Collapse_synonym_locally(newer_synonym,container) -> csl_in_grammar (newer_synonym,container) gram
  |Collapse_synonym_globally(newer_synonym) -> csg_in_grammar newer_synonym gram
  |Local(name,mods)->apply_local_modifications gram name mods;;
 

let apply_several gram modifications = 
   List.fold_left apply gram modifications ;;

let debug_bad_list_of_local_modifications gram name local_modifs=
  let orig = Local_Modification.lm_get gram name in 
  let (count_before_bug,the_problematic_local_modif) = 
    Tools_for_debugging.extract_from_fold_left (Local_Modification.apply name) (gram,orig) local_modifs in
  let local_modifs_before_bug = List_again.long_head count_before_bug local_modifs in 
  (count_before_bug,local_modifs_before_bug,
  (Local_Modification.apply_several name) (gram,orig) local_modifs_before_bug,
  the_problematic_local_modif) ;;


let debug_bad_list_of_modifications gram modifs=
  let (count_before_bug,the_problematic_modif) = Tools_for_debugging.extract_from_fold_left apply gram modifs in 
  let modifs_before_bug = List_again.long_head count_before_bug modifs in 
  (count_before_bug,modifs_before_bug,apply_several gram modifs_before_bug,the_problematic_modif) ;;



end ;;



end ;; 

end ;; 


module Nonrecursive_grammar = struct 

let get = Private.Nonrecursive_grammar.get_from_nonrecursive_grammar ;;  
let singleton = Private.Nonrecursive_grammar.singleton ;;

end ;;  

let replace_pair_or_add_if_absent = Private.replace_pair_or_add_if_absent ;;

let check_disjunction_ladder = Private.check_disjunction_ladder ;; 
let containing = Private.containing ;;

let debug_bad_list_of_local_modifications gram name local_modifs=
 let gram_with_dwc = Private.With_dwarf_count.make gram in 
 let  (count_before_bug,local_modifs_before_bug,(gram_with_dwc_before_bug,dis_before_bug),the_problematic_local_modif) =
    Private.With_dwarf_count.Modify.debug_bad_list_of_local_modifications gram_with_dwc name local_modifs in 
 let (Private.With_dwarf_count.WDC(_,gram_before_bug)) = gram_with_dwc_before_bug in 
 (count_before_bug,local_modifs_before_bug,(gram_before_bug,dis_before_bug),the_problematic_local_modif) ;;


let debug_bad_list_of_modifications gram modifs=
 let gram_with_dwc = Private.With_dwarf_count.make gram in 
 let  (count_before_bug,modifs_before_bug,gram_with_dwc_before_bug,the_problematic_modif) =
    Private.With_dwarf_count.Modify.debug_bad_list_of_modifications gram_with_dwc modifs in 
 let (Private.With_dwarf_count.WDC(_,gram_before_bug)) = gram_with_dwc_before_bug in 
 (count_before_bug,modifs_before_bug,gram_before_bug,the_problematic_modif) ;;


let differences = Private.differences ;;
let extract_at_names = Private.WriteParser.extract_at_names ;;
let get = Private.get ;;

let get_opt = Private.get_opt ;;
let get_and_display = Private.get_and_display ;;

let just_below = Private.just_below ;;
let lower_interval_below = Private.lower_interval_below ;;

let modify gram modifs = 
  let gram_with_dwc = Private.With_dwarf_count.make gram in 
  let  new_gram_with_dwc = Private.With_dwarf_count.Modify.apply_several gram_with_dwc modifs in 
  let (Private.With_dwarf_count.WDC(_,new_gram)) = new_gram_with_dwc in 
  new_gram ;;
  
let name_for_form_opt = Private.name_for_form_opt ;; 
let ocaml_name = Private.ocaml_name ;;
let order_on_pairs = Private.order_on_pairs ;;

let sanitize = Private.Sanitize.sanitize_as_many_times_as_needed ;;
let singleton name form = AL [name,form] ;;     
let write_parser = Private.WriteParser.write_prsrtxt ;;