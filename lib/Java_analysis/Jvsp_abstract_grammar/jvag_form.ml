(*

#use"lib/Java_analysis/Jvsp_abstract_grammar/jvag_form.ml";;

*)

open Jvag_types ;;

exception Get_exn of string ;;
exception Circularity of string * (string list) ;; 
exception Concat_content_exn of form ;;
exception Disjunction_content_exn of form ;;
exception Synonym_content_exn of form ;;

module Private = struct 

  let str_order = Total_ordering.lex_for_strings ;;
  let str_sort = Ordered.sort str_order ;;


let to_string = function 
   Optional(nm) -> Jvsp_util.display_optional nm   
  |Molecular(l) -> (String.concat " " (Image.image Jvsp_util.summary_of_token_type l))   
  |Concat(l) ->  String.concat " " l
  |Disjunction(l) ->
     "\n"^(String.concat "\n" (Image.image (fun elt->
      "|"^elt) l))^"\n"
  |(Star nm) -> Jvsp_util.display_star nm 
  |Synonym(nm) -> nm;;

let unordered_coatoms form = match form with
    Concat l ->  l
   |Molecular _ -> []
   |Disjunction l -> l
   |Star nm -> [nm]
   |Optional nm -> [nm]
   |Synonym nm -> [nm] ;;


let molecular_content_opt form = match form with 
   (Molecular l) -> Some l    
   |Concat _
   |Disjunction _
   |Star _
   |Optional _ 
   |Synonym _ -> None;;  

end ;;

let coatoms form = Private.str_sort (Private.unordered_coatoms form) ;;

let concat_content form = match form with 
   (Concat l) -> l    
   |Disjunction _
   |Molecular  _
   |Star _
   |Optional _ 
   |Synonym _ -> raise(Concat_content_exn(form));;   
 
let concat_content_opt form = match form with 
   (Concat l) -> Some l    
   |Disjunction _
   |Molecular  _
   |Star _
   |Optional _ 
   |Synonym _ -> None;;   

let disjunction_content form = match form with 
   (Disjunction l) -> l    
   |Concat _
   |Molecular  _
   |Star _
   |Optional _ 
   |Synonym _ -> raise(Disjunction_content_exn(form));;   

let disjunction_content_opt form = match form with 
   (Disjunction l) -> Some l    
   |Concat _
   |Molecular  _
   |Star _
   |Optional _ 
   |Synonym _ -> None;;  



let is_a_token_sequence form = ((Private.molecular_content_opt form)<>None) ;;

let is_contained_in nm form = match form with
    Concat l -> List.mem nm l
   |Molecular _ -> false
   |Disjunction l -> List.mem nm l
   |Star nm2 -> nm2 = nm
   |Optional nm2 -> nm2 = nm
   |Synonym nm2 -> nm2 = nm ;;

let molecular_content_opt = Private.molecular_content_opt ;;   
let needs_extra_display form= match form with
   Disjunction(_) 
  |Concat(_) ->  true    
  |Optional(_) 
  |Molecular(_)    
  |(Star _) 
  |Synonym(_) -> false;;  

let ocaml_name = function 
   Optional(nm) -> "Optional(\""^nm^"\")"
  |Molecular(l) -> "Molecular(["^(String.concat ";" (Image.image Jvsp_util.ocaml_name_for_token_type l))^"])"
  |Concat(l) -> "Concat(["^(String.concat ";" (Image.image (fun s->"\""^s^"\"") l))^"])"
  |Disjunction(l) -> "Disjunction(["^(String.concat ";" (Image.image (fun s->"\""^s^"\"") l))^"])"
  |Star(nm) -> "Star(\""^nm^"\")"
  |Synonym(nm) -> "Synonym(\""^nm^"\")";;  

let order = (
   (fun form1 form2 ->Total_ordering.standard form1 form2): 
     form Total_ordering_t.t ) ;; 

(* This is a registered printer : print_out *)
let print_out (fmt:Format.formatter) form=
   Format.fprintf fmt "@[%s@]" (Strung.with_size_limit ~size_limit:250 (Private.to_string form));;

let starred_content_opt form = match form with 
   |Star nm -> Some nm
   |Molecular _    
   |Concat _
   |Disjunction _
   |Optional _ 
   |Synonym _ -> None;;  

let synonym_content form = match form with 
   (Synonym nm) -> nm   
   |Disjunction _
   |Concat _
   |Molecular  _
   |Star _
   |Optional _ -> raise(Synonym_content_exn(form));;

let to_string = Private.to_string ;; 

let uniform_composition link l = match link with 
    Optional_L -> Optional(List.hd l)
   |Concat_L -> Concat(l)
   |Disjunction_L -> Disjunction(l)
   |Star_L -> Star(List.hd l)
   |Synonym_L -> Synonym(List.hd l);;

let uniform_decomposition_opt form = match form with
   |Molecular _ -> None    
   |Concat l ->  Some(Concat_L,l)
   |Disjunction l -> Some(Disjunction_L,l)
   |Star nm -> Some(Star_L,[nm])
   |Optional nm -> Some(Optional_L,[nm])
   |Synonym nm -> Some(Synonym_L,[nm]) ;;

let unordered_coatoms = Private.unordered_coatoms ;;   