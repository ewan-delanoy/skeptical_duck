(*

#use"lib/Java_analysis/jvag_magnifying_glass.ml";;

*)

open Jvag_types ;;

exception Missing_element_in_list_exn of   string * (string list) ;;
exception Unspecified_index_in_list_exn of string * (string list) ;;
exception Bad_index_in_list_exn of int * string * (string list) ;; 

module Private = struct 


let maximal_name_size (MG l)= snd(Max.maximize_it 
(fun (MGL(name,_))->String.length name) l) ;;
  


let link_to_string (name,form) = match form with 
   Optional(nm) -> Jvsp_util.display_optional nm   
  |Molecular(l) -> (String.concat " " (Image.image Jvsp_util.summary_of_token_type l))   
  |Concat(_) ->  name
  |Disjunction(_) -> name^"(DIS)"
  |(Star nm) -> Jvsp_util.display_star nm 
  |Synonym(_) -> name
;;


let line_to_string max_name_size (MGL(name,concatenation)) = 
(Strung.insert_repetitive_offset_on_the_left ' ' max_name_size name)^" : "^
(String.concat " " (Image.image link_to_string concatenation)) ;;

let to_string mg =
   let m = maximal_name_size mg and (MG l)=mg in 
   "\n\n\n" ^ (String.concat "\n" (Image.image (line_to_string m) l)) ^ "\n\n\n" ;;

let print_out (fmt:Format.formatter) mg=
   Format.fprintf fmt "@[%s@]" (to_string mg);;

let concatify gram name =
   let form = Jvag_grammar.get gram name in 
   match form with 
   Concat(l) ->  
     Image.image (fun name2->(name2,Jvag_grammar.get gram name2)) l
  |Optional(_)  
  |Disjunction(_)
  |Molecular(_)  
  |Star _  
  |Synonym(_) -> [name,form] ;;



let get gram name =
   let form = Jvag_grammar.get gram name in 
   match form with 
   Disjunction(l) -> 
     MG(Image.image (fun name2->MGL(name2,concatify gram name2)) l)
  |Concat(_) ->
     MG[MGL(name,concatify gram name)]    
  |Optional(_)  
  |Molecular(_)  
  |Star _  
  |Synonym(_) -> MG([MGL(name,[name,form])]) ;;


let extract_element_in_indexed_list (name,idx_opt) l =
   let indexed_l=Int_range.index_everything l in 
   let sols = List.filter (fun (_,(name2,_))->name2 = name) indexed_l in 
   let n = List.length sols in 
   if n=0 then raise(Missing_element_in_list_exn(name,Image.image fst l)) else 
   if n=1 then fst(List.hd sols) else 
    match idx_opt with 
    None -> raise(Unspecified_index_in_list_exn(name,Image.image fst l))
    |Some idx -> 
      if List.assoc_opt idx sols = None 
      then raise(Bad_index_in_list_exn(idx,name,Image.image fst l))
      else idx ;;  

let rec assoc_opt name0 = function 
  [] -> None 
  | (MGL(name,concatention))::others ->
     if name = name0 
     then Some concatention 
     else assoc_opt name0 others;;  


let inner_expansion_of_inner_node gram form = 
   let (temp,expansion_should_occur)=
   (match form with 
    (Disjunction l) -> (Image.image (fun x->[x]) l,true)    
   |Concat l->([l],true)
   |Molecular  _ -> ([],false)
   |Star nm -> ([[];[nm;"Starred"^nm]],true)
   |Optional nm -> ([[];[nm]],true)
   |Synonym nm -> ([[nm]],true)) in 
   (Image.image (Image.image (fun name->(name,Jvag_grammar.get gram name))) temp,
   expansion_should_occur);; 

let append_index_after_sharp name idx =
   let s = string_of_int idx in 
   if String.contains name '#'
   then name ^ "." ^ s 
   else name ^ "#" ^ s ;;      

let sharpless_core str = Cull_string.before_rightmost_possibly_all str '#' ;;


(*

sharpless_core "Gabriel" ;;
sharpless_core "Gabriel#7.21.53" ;;
append_index_after_sharp "Gabriel#7.21.53" 64 ;;

*)      

let expand_node_in_line_at_index gram idx line_in_mg  =
   let (MGL(main_name,concatenation)) =line_in_mg in 
   let indexed_concatenation = Int_range.index_everything concatenation in 
   let (node_name,old_node) = List.nth concatenation (idx-1) in 
   let (inner_l,expansion_should_occur) = inner_expansion_of_inner_node gram old_node in 
   if not(expansion_should_occur)
   then ([line_in_mg],None) 
   else  
   let before = List.filter_map (fun (i,pair)->if i<idx then Some pair else None) indexed_concatenation 
   and after = List.filter_map (fun (i,pair)->if i>idx then Some pair else None) indexed_concatenation  
   and indexed_inner_l=Int_range.index_everything inner_l in 
   (Image.image (
       fun (idx2,data2) -> 
         MGL(append_index_after_sharp main_name idx2,
         before@(data2)@after)
   ) indexed_inner_l,Some node_name) ;;

let expand_node_in_line_according_to_data gram line_in_mg data =
   let (MGL(_main_name,concatenation)) =line_in_mg in 
   let idx = extract_element_in_indexed_list data concatenation in
   expand_node_in_line_at_index gram idx line_in_mg   ;;

let expand_all_heads gram ?(verbose=false) (MG l) =
   let temp = Image.image (expand_node_in_line_at_index gram 1) l in 
   let answer = MG(List.flatten(Image.image fst temp)) in
   let expanded_heads = List.filter_map snd temp in 
   let m = List.length expanded_heads in 
   let _ = (
     if verbose then 
     let msg = (
       if m=0 then "All the heads are already expanded." else 
       if m=1 then "A single head has been expanded : "^(List.hd expanded_heads) else 
       "The following "^(string_of_int m)^" heads have been expanded : "^
       (String.concat ", " expanded_heads)  
     ) in 
     print_string ("\n\n"^msg^"\n\n");flush stdout
   ) in 
   (answer,m) ;;

let rec helper_for_first_token_determination gram (mg,number_of_recent_actions) = 
   if number_of_recent_actions = 0 
   then mg 
   else helper_for_first_token_determination gram (expand_all_heads gram mg) ;; 
    
exception Bad_head_exn of string * form ;; 

let head_with_tail concatenation = 
   let (head_node,other_nodes) = List_again.head_with_tail concatenation in 
   let (head_name,head_form) = head_node in 
   let l_opt = Jvag_form.molecular_content_opt head_form in 
   if l_opt= None 
   then raise(Bad_head_exn(head_name,head_form))
   else 
   let l = Option.get l_opt in 
   let (h,t) = List_again.head_with_tail l in 
   let final_tail = (
      if t=[]
      then other_nodes 
      else (Jvsp_util.code_for_tokentype_sequence_in_production_names t,Molecular(t)) :: other_nodes   
   ) in 
   (h,final_tail) ;;

let mg_length (MG l) = List.length l;;   



let str_order = Total_ordering.lex_for_strings ;;
let str_sort = Ordered.sort str_order ;;

let numberless_versions (MG l)=
   str_sort (Image.image (fun (MGL(name,_))->sharpless_core name) l) ;;

let group_by_first_token (MG l)= 
  let temp1 = Image.image (fun (MGL(name,concatenation)) -> 
    let (h,final_tail) = head_with_tail concatenation in 
    (h,MGL(name,final_tail))
   ) l in 
  let tokens = Ordered.sort Total_ordering.standard (Image.image fst temp1) in   
  Image.image (
    fun h->
      let for_h = List.filter_map (fun pair->if fst(pair)=h then Some(snd pair) else None) temp1 in 
      (h,MG(for_h))
  ) tokens ;; 



let determine_first_token gram mg =
   let mg2 = helper_for_first_token_determination gram (expand_all_heads gram mg) in 
   let grouped_by_first_tok = group_by_first_token mg2 in 
   Image.image (
    fun (h,mg_for_h)->
      let versions = numberless_versions mg_for_h in 
      (h,(versions,mg_for_h))
  ) grouped_by_first_tok;;

let determine_first_token_and_partition gram mg =
   List.partition (fun (_h,(versions,_realizers))->List.length(versions)=1) 
      (determine_first_token gram mg);;

let react_to_imposed_first_token gram mg imposed_first_token=
   let list_of_cases = determine_first_token gram mg in 
   List.assoc imposed_first_token list_of_cases ;;

exception Prefix_too_short_exn of Jvsp_types.token_type list ;;

let rec helper_for_shortest_determinative_prefix gram (giver,taker,(versions,mg)) = 
   if List.length(versions)=1
   then (List.rev taker,List.hd versions,mg)
   else  
   match giver with 
   [] -> raise(Prefix_too_short_exn(List.rev taker))     
   |toktype::others ->
      helper_for_shortest_determinative_prefix gram (others,toktype::taker,
       react_to_imposed_first_token gram mg toktype
      ) ;; 

let shortest_determinative_prefix gram mg toklist =
   helper_for_shortest_determinative_prefix gram (Jvsp_token_types_list.unveil toklist,[],(numberless_versions mg,mg)) ;;

let behead_each_one (MG l) =
   MG(Image.image (fun (MGL(name,concatenation))->MGL(name,List.tl concatenation)) l);;


let select (MG l) names = 
   MG(List.filter (fun (MGL(name,_))->List.mem name names) l);;

let determined_or_not (MG l) = 
   let (a,b)=List.partition (fun (MGL(_,concatenation))->
      let (_,form) = List.hd concatenation in 
      Jvag_form.molecular_content_opt form=None) l in 
   (MG(a),MG(b));;



end ;; 

let behead_each_one = Private.behead_each_one ;;
let determined_or_not = Private.determined_or_not ;;
let determine_first_token = Private.determine_first_token ;;
let expand_all_heads = Private.expand_all_heads ;;
let get = Private.get ;; 
(* This is a registered printer : print_out *)
let print_out = Private.print_out ;;
let select = Private.select ;;
let shortest_determinative_prefix  = Private.shortest_determinative_prefix ;;

