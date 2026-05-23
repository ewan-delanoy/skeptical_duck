(*

#use"lib/Java_analysis/jvsp_magnifying_glass.ml";;

*)

type form = Jvsp_abstract_grammar_t.form = 
    Just_an_optional of string 
   |Just_atomic of Jvsp_types.token_type list
   |Just_a_concat of string list 
   |Just_a_disjunction of string list 
   |Just_a_star of string 
   |Synonym of string
  ;;

type magnifying_glass = Jvsp_abstract_grammar_t.magnifying_glass = MG of 
  (string * ((string * form) list)) list 
 ;;

exception Missing_element_in_list_exn of   string * (string list) ;;
exception Unspecified_index_in_list_exn of string * (string list) ;;
exception Bad_index_in_list_exn of int * string * (string list) ;; 

module Private = struct 


let maximal_name_size (MG l)= snd(Max.maximize_it (fun (name,_)->String.length name) l) ;;
  


let link_to_string (name,form) = match form with 
   Just_an_optional(nm) -> Jvsp_util.display_optional nm   
  |Just_atomic(l) -> (String.concat " " (Image.image Jvsp_util.summary_of_token_type l))   
  |Just_a_concat(_) ->  name
  |Just_a_disjunction(_) -> name^"(DIS)"
  |(Just_a_star nm) -> Jvsp_util.display_star nm 
  |Synonym(_) -> name
;;


let element_to_string max_name_size (name,concatenation) = 
(Strung.insert_repetitive_offset_on_the_left ' ' max_name_size name)^" : "^
(String.concat " " (Image.image link_to_string concatenation)) ;;

let to_string mg =
   let m = maximal_name_size mg and (MG l)=mg in 
   "\n\n\n" ^ (String.concat "\n" (Image.image (element_to_string m) l)) ^ "\n\n\n" ;;

let print_out (fmt:Format.formatter) mg=
   Format.fprintf fmt "@[%s@]" (to_string mg);;

let concatify gram name =
   let form = Jvsp_abstract_grammar.get gram name in 
   match form with 
   Just_a_concat(l) ->  
     Image.image (fun name2->(name2,Jvsp_abstract_grammar.get gram name2)) l
  |Just_an_optional(_)  
  |Just_a_disjunction(_)
  |Just_atomic(_)  
  |Just_a_star _  
  |Synonym(_) -> [name,form] ;;



let get gram name =
   let form = Jvsp_abstract_grammar.get gram name in 
   match form with 
   Just_a_disjunction(l) -> 
     MG(Image.image (fun name2->(name2,concatify gram name2)) l)
  |Just_a_concat(_) ->
     MG[name,concatify gram name]    
  |Just_an_optional(_)  
  |Just_atomic(_)  
  |Just_a_star _  
  |Synonym(_) -> MG([name,[name,form]]) ;;


let extract_element_in_indexed_list name idx_opt l indexed_l=
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

let disjunction_content_opt form = match form with 
   (Just_a_disjunction l) -> Some l    
   |Just_a_concat _
   |Just_atomic  _
   |Just_a_star _
   |Just_an_optional _ 
   |Synonym _ -> None;;   

exception Wrong_main_name_exn of string ;; 
exception Name_does_not_indicate_subdisjunction_exn of string * form ;;

let expand_subdisjunction gram ?(idx_opt=None) mg ~main_name  ~subdis_name =
   let (MG outer_l) =mg in 
   match List.assoc_opt main_name outer_l with 
   None -> raise(Wrong_main_name_exn(main_name))
   |Some l ->
    let indexed_l = Int_range.index_everything l in 
    let idx = extract_element_in_indexed_list subdis_name idx_opt l indexed_l in
    let (_,old_subdis) = List.assoc idx indexed_l in 
    match disjunction_content_opt old_subdis with 
     None -> raise(Name_does_not_indicate_subdisjunction_exn(subdis_name,old_subdis))
     |Some inner_l ->
      let before = List.filter_map (fun (i,pair)->if i<idx then Some pair else None) indexed_l 
      and after = List.filter_map (fun (i,pair)->if i>idx then Some pair else None) indexed_l  
      and indexed_inner_l=Int_range.index_everything inner_l in 
      let replacement = Image.image (
       fun (idx2,name2) -> (main_name^"."^(string_of_int idx2),before@(concatify gram name2)@after)
      ) indexed_inner_l in 
      let new_outer_l = List.flatten(Image.image (
       fun pair->
         if fst(pair)=main_name then replacement else [pair]
      ) outer_l) in 
      MG new_outer_l ;;

let select (MG l) names = 
   MG(List.filter (fun (name,_)->List.mem name names) l);;

let take_tails (MG outer_l) = 
   MG(Image.image (fun (name,l)->(name,List.tl l)) outer_l);;   


end ;; 

let expand_subdisjunction = Private.expand_subdisjunction ;;

let get = Private.get ;; 
(* This is a registered printer : print_out *)
let print_out = Private.print_out ;;
let select = Private.select ;;
let take_tails = Private.take_tails ;; 