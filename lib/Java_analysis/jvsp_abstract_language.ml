(*

#use"lib/Java_analysis/jvsp_abstract_language.ml";;

*)


type element_in_disjunction = Jvsp_abstract_language_t.element_in_disjunction = 
   Concat of string list ;;
     
type form =  Jvsp_abstract_language_t.form = 
   Disjunction of element_in_disjunction list 
   |Just_an_optional of string
   |Just_atomic of Jvsp_types.token_type list
   |Just_a_concat of string list  
   |Just_a_disjunction of string list 
   |Just_a_star of string 
   |Synonym of string;;

type t =  Jvsp_abstract_language_t.t = AL of (string * form) list ;; 


module Private = struct 

let ocaml_name_of_element_in_disjunction (Concat l) =
    "Concat(["^(String.concat ";" (Image.image (fun s->"\""^s^"\"") l))^"])";;

let ocaml_name_of_form = function 
  (Disjunction l) ->
   "Disjunction(["^(String.concat ";" (Image.image ocaml_name_of_element_in_disjunction l))^"])"
  |Just_an_optional(nm) -> "Just_an_optional(\""^nm^"\")"
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

let element_in_disjunction_to_string (Concat l) =String.concat " " l;;

let form_to_string = function 
   (Disjunction l) -> 
   if List.length(l) = 1 
   then element_in_disjunction_to_string(List.hd l)
   else       
   "\n"^(String.concat "\n" (Image.image (fun elt->
      "|"^(element_in_disjunction_to_string elt)) l))^"\n" 
  |Just_an_optional(nm) -> "\u{3010}"^nm^"\u{3011}"    
  |Just_atomic(l) -> (String.concat " " (Image.image Jvsp_util.summary_of_token_type l))   
  |Just_a_concat(l) ->  String.concat " " l
  |Just_a_disjunction(l) ->
     "\n"^(String.concat "\n" (Image.image (fun elt->
      "|"^elt) l))^"\n"
  |(Just_a_star nm) -> nm^"\u{2605}"  
  |Synonym(nm) -> nm;;

let print_out_form (fmt:Format.formatter) form=
   Format.fprintf fmt "@[%s@]" (form_to_string form);;

let order_on_forms = (
   (fun form1 form2 ->Total_ordering.standard form1 form2): 
     form Total_ordering_t.t ) ;; 

let order_on_pairs = Total_ordering.product 
    Total_ordering.lex_for_strings order_on_forms ;;

module Redundant_concats = struct

let apply_replacements_to_list reps li = 
   match List.assoc_opt li reps with
   (Some other_li) -> other_li
   |None -> li ;;


let apply_replacements_to_form reps form = match form with
   (Disjunction ll) -> Disjunction(Image.image (fun (Concat l)->Concat(apply_replacements_to_list reps l))  ll)
   |Just_a_concat l -> Just_a_concat(apply_replacements_to_list reps l)
   |Just_atomic _
   |Just_a_disjunction _ 
   |Just_a_star _
   |Just_an_optional _
   |Synonym _ -> form ;;

let apply_replacements_to_pair named_reps pair =
  let (name,form) = pair in 
  match List.assoc_opt name named_reps with 
  None -> pair 
  |Some(reps)->(name,apply_replacements_to_form reps form) ;; 

let apply_replacements_to_grammar reps (AL l) = AL(Image.image (apply_replacements_to_pair reps) l) ;;
let get_concat_content_opt = function 
   (Just_a_concat cc) -> Some cc
   |Disjunction _ 
   |Just_atomic _
   |Just_a_disjunction _ 
   |Just_a_star _
   |Just_an_optional _
   |Synonym _ -> None ;;

let form_is_a_concat form = (get_concat_content_opt(form)<>None);;

let concat_parts_inside name form = 
   let specify_name = (fun l->Image.image (fun z->(name,(l,z))) l) in 
    match form with
   (Disjunction ll) -> List.flatten(Image.image (fun (Concat l)->specify_name l) ll)
   |Just_a_concat l -> specify_name l
   |Just_atomic _
   |Just_a_disjunction _ 
   |Just_a_star _
   |Just_an_optional _
   |Synonym _ -> [] ;;

let redundant_concats_inside gram (name,form) =
   List.filter (fun (_,(_,z))->
      form_is_a_concat(get gram z))  (concat_parts_inside name form) ;;

let all_redundant_concats =Memoized.make (fun gram->
  let (AL pairs) = gram in   
   List.flatten(Image.image (redundant_concats_inside gram) pairs));; 

let replacements_by_name1 =Memoized.make (fun gram -> Image.image (
  fun (name,(initial_list,culprit)) ->
    let inner_list = Option.get(get_concat_content_opt(get gram culprit)) in 
    let final_list = List.flatten(Image.image (fun t->if t=culprit then inner_list else [t]) initial_list) in 
    (name,(initial_list,final_list))
) (all_redundant_concats gram)) ;;  

let names_involved_in_replacements = Memoized.make(fun gram -> 
  Ordered.sort Total_ordering.lex_for_strings (Image.image fst (replacements_by_name1 gram))) ;;

let replacements_by_name2 = Memoized.make(fun gram -> Image.image (
  fun name -> (name,List.filter_map 
  (fun pair->if fst(pair)=name then Some(snd pair) else None) 
    (replacements_by_name1 gram))
) (names_involved_in_replacements gram) );;  

let remove_immediate_redundant_concats gram = 
   apply_replacements_to_grammar (replacements_by_name2 gram) gram ;;

end ;;

module Mergable_token_sequences = struct



end ;;   

end ;; 


module Preliminary_normalizations = struct
  
let redundant_concats = Private.Redundant_concats.replacements_by_name2 ;;
let remove_immediate_redundant_concats = Private.Redundant_concats.remove_immediate_redundant_concats ;;

end ;;
let get = Private.get ;;
let ocaml_name = Private.ocaml_name ;;
let order_on_pairs = Private.order_on_pairs ;;

(* This is a registered printer : print_out_form *)
let print_out_form = Private.print_out_form ;;