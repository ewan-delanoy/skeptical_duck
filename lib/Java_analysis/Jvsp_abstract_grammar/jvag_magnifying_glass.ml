(*

#use"lib/Java_analysis/Jvsp_abstract_grammar/jvag_magnifying_glass.ml";;

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


let line_to_string max_name_size (MGL(name,(concatenation,_path))) = 
(Strung.insert_repetitive_offset_on_the_left ' ' max_name_size name)^" : "^
(String.concat " " (Image.image link_to_string concatenation)) ;;

let to_string mg =
   let (MG l)=mg in 
   if l=[] then "{}" else
   let m = maximal_name_size mg in 
   "\n\n\n" ^ (String.concat "\n" (Image.image (line_to_string m) l)) ^ "\n\n\n" ;;

let print_out (fmt:Format.formatter) mg=
   Format.fprintf fmt "@[%s@]" (to_string mg);;

let line_to_pathful_string max_name_size (MGL(name,(concatenation,path))) = 
(Strung.insert_repetitive_offset_on_the_left ' ' max_name_size name)^" : "^
(String.concat " " (Image.image link_to_string concatenation))^", "^
(String.concat "<" path) ;;

let to_pathful_string mg =
   let (MG l)=mg in 
   if l=[] then "{}" else
   let m = maximal_name_size mg in 
   "\n\n\n" ^ (String.concat "\n" (Image.image (line_to_pathful_string m) l)) ^ "\n\n\n" ;;


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
     MG(Image.image (fun name2->MGL(name2,(concatify gram name2,[name]))) l)
  |Concat(_) ->
     MG[MGL(name,(concatify gram name,[name]))]    
  |Optional(_)  
  |Molecular(_)  
  |Star _  
  |Synonym(_) -> MG([MGL(name,([name,form],[name]))]) ;;


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
    (Disjunction l) -> (Image.image (fun x->([x],Some x)) l,true)    
   |Concat l->([l,None],true)
   |Molecular  _ -> ([],false)
   |Star nm -> ([([],None);([nm;"Starred"^nm],None)],true)
   |Optional nm -> ([([],None);([nm],Some nm)],true)
   |Synonym nm -> ([[nm],Some nm],true)) in 
   (Image.image (fun (ll,path_data_opt) ->
      (Image.image (fun name->(name,Jvag_grammar.get gram name)) ll,path_data_opt) ) temp,
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

let cons_opt added_item_opt l = match added_item_opt with
 None -> l 
 |Some x -> x::l;;

let expand_node_in_line_at_index gram idx line_in_mg  =
   let (MGL(main_name,(concatenation,path))) =line_in_mg in 
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
       fun (idx2,(data2,path_data_opt)) -> 
         MGL(append_index_after_sharp main_name idx2,
         (before@(data2)@after,cons_opt path_data_opt path))
   ) indexed_inner_l,Some node_name) ;;

let expand_node_in_line_according_to_data gram line_in_mg data =
   let (MGL(_main_name,(concatenation,_path))) =line_in_mg in 
   let idx = extract_element_in_indexed_list data concatenation in
   expand_node_in_line_at_index gram idx line_in_mg   ;;

let expand_all_heads_and_remember_count gram (MG l) =
   let temp = Image.image (expand_node_in_line_at_index gram 1) l in 
   let answer = MG(List.flatten(Image.image fst temp)) in
   let expanded_heads = List.filter_map snd temp in 
   let m = List.length expanded_heads in 
   (answer,m) ;;

let expand_all_heads gram mg = fst(expand_all_heads_and_remember_count gram mg) ;;

let expand_some_heads gram (MG l) names=
   let temp = Image.image (fun mgl ->
      let (MGL(name,_)) = mgl in 
      if List.mem name names
      then fst(expand_node_in_line_at_index gram 1 mgl)
      else [mgl]) l in 
   MG(List.flatten(temp));;


let rec helper_for_first_token_determination gram (mg,number_of_recent_actions) = 
   if number_of_recent_actions = 0 
   then mg 
   else helper_for_first_token_determination gram (expand_all_heads_and_remember_count gram mg) ;; 
    
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
  let temp1 = Image.image (fun (MGL(name,(concatenation,path))) -> 
    let (h,final_tail) = head_with_tail concatenation in 
    (h,MGL(name,(final_tail,path)))
   ) l in 
  let tokens = Ordered.sort Total_ordering.standard (Image.image fst temp1) in   
  Image.image (
    fun h->
      let for_h = List.filter_map (fun pair->if fst(pair)=h then Some(snd pair) else None) temp1 in 
      (h,MG(for_h))
  ) tokens ;; 
let determine_first_token gram mg =
   let mg2 = helper_for_first_token_determination gram (expand_all_heads_and_remember_count gram mg) in 
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
   MG(Image.image (fun (MGL(name,(concatenation,path)))->MGL(name,(List.tl concatenation,path))) l);;


let select (MG l) names = 
   MG(List.filter (fun (MGL(name,_))->List.mem name names) l);;

let determined_or_not (MG l) = 
   let (a,b)=List.partition (fun (MGL(_,(concatenation,_)))->
      let (_,form) = List.hd concatenation in 
      Jvag_form.molecular_content_opt form=None) l in 
   (MG(a),MG(b));;

let possible_first_tokens gram name =
   let mg = get gram name in 
   let complete_info = determine_first_token gram mg in 
   let unordered_tokens = Image.image fst complete_info in 
   let tokens = Ordered.sort Jvsp_util.order_on_token_types unordered_tokens in 
   (tokens,complete_info) ;;

let nonempty_lines (MG l) = 
   MG(List.filter (fun (MGL(_name,(concatenation,_)))->concatenation<>[]) l);;

let names (MG l) = 
   Image.image (fun (MGL(name,_form))->name) l;;   

let paths (MG l)=Image.image (fun (MGL(_,(_,path))) -> path) l;;    

let path_is_circular = function 
 [] -> false 
 |a::b -> List.mem a b ;;

let is_nonempty (MG l)=(l<>[]) ;;

let circularities (MG l) = 
   let new_mg = MG(List.filter (fun (MGL(_name,(_concatenation,path)))->path_is_circular path) l) in 
   let _ = (if is_nonempty new_mg then print_string(to_pathful_string new_mg)) in 
   new_mg;;

let partition_according_to_whether_first_name_in_chain_equals (MG l) name0= 
   let (a,b)=List.partition (fun (MGL(_,(concatenation,_)))->
      let (name,_) = List.hd concatenation in 
      name=name0) l in  
   (MG(a),MG(b));;   

let expand_concat_in_line gram mgl =
    let (MGL(name,(concatenation,path))) =mgl in 
    if List.length(concatenation)<>1 then mgl else 
    let (_,form1)  = List.hd concatenation in 
    match Jvag_form.concat_content_opt form1 with 
    None -> mgl 
    |Some(l)->
      let new_concatenation = Image.image (fun name2->(name2,Jvag_grammar.get gram name2)) l in 
      MGL(name,(new_concatenation,path)) ;;

let expand_some_concats gram (MG l) names =
   let new_l = Image.image (fun mgl ->
      let (MGL(name,_)) = mgl in 
      if List.mem name names
      then expand_concat_in_line gram mgl
      else mgl) l in
   MG(new_l);;


end ;; 

let behead_each_one = Private.behead_each_one ;;
let circularities = Private.circularities ;;
let determined_or_not = Private.determined_or_not ;;
let determine_first_token = Private.determine_first_token ;;
let expand_all_heads = Private.expand_all_heads ;;
let expand_some_concats = Private.expand_some_concats ;;
let expand_some_heads = Private.expand_some_heads ;;
let get = Private.get ;; 
let names = Private.names ;;
let nonempty_lines = Private.nonempty_lines ;;
let possible_first_tokens = Private.possible_first_tokens ;;
let paths = Private.paths ;;
let partition_according_to_whether_first_name_in_chain_equals = Private.partition_according_to_whether_first_name_in_chain_equals ;;
(* This is a registered printer : print_out *)
let print_out = Private.print_out ;;
let select = Private.select ;;
let shortest_determinative_prefix  = Private.shortest_determinative_prefix ;;

