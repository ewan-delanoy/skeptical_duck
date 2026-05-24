(*

#use"lib/Java_analysis/jvag_form.ml";;

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
   Just_an_optional(nm) -> Jvsp_util.display_optional nm   
  |Just_atomic(l) -> (String.concat " " (Image.image Jvsp_util.summary_of_token_type l))   
  |Just_a_concat(l) ->  String.concat " " l
  |Just_a_disjunction(l) ->
     "\n"^(String.concat "\n" (Image.image (fun elt->
      "|"^elt) l))^"\n"
  |(Just_a_star nm) -> Jvsp_util.display_star nm 
  |Synonym(nm) -> nm;;

let unordered_coatoms form = match form with
    Just_a_concat l ->  l
   |Just_atomic _ -> []
   |Just_a_disjunction l -> l
   |Just_a_star nm -> [nm]
   |Just_an_optional nm -> [nm]
   |Synonym nm -> [nm] ;;


end ;;

let coatoms form = Private.str_sort (Private.unordered_coatoms form) ;;

let concat_content form = match form with 
   (Just_a_concat l) -> l    
   |Just_a_disjunction _
   |Just_atomic  _
   |Just_a_star _
   |Just_an_optional _ 
   |Synonym _ -> raise(Concat_content_exn(form));;   
 
let disjunction_content form = match form with 
   (Just_a_disjunction l) -> l    
   |Just_a_concat _
   |Just_atomic  _
   |Just_a_star _
   |Just_an_optional _ 
   |Synonym _ -> raise(Disjunction_content_exn(form));;   

let is_contained_in nm form = match form with
    Just_a_concat l -> List.mem nm l
   |Just_atomic _ -> false
   |Just_a_disjunction l -> List.mem nm l
   |Just_a_star nm2 -> nm2 = nm
   |Just_an_optional nm2 -> nm2 = nm
   |Synonym nm2 -> nm2 = nm ;;

let needs_extra_display form= match form with
   Just_a_disjunction(_) 
  |Just_a_concat(_) ->  true    
  |Just_an_optional(_) 
  |Just_atomic(_)    
  |(Just_a_star _) 
  |Synonym(_) -> false;;  

let ocaml_name = function 
   Just_an_optional(nm) -> "Just_an_optional(\""^nm^"\")"
  |Just_atomic(l) -> "Just_atomic(["^(String.concat ";" (Image.image Jvsp_util.ocaml_name_for_token_type l))^"])"
  |Just_a_concat(l) -> "Just_a_concat(["^(String.concat ";" (Image.image (fun s->"\""^s^"\"") l))^"])"
  |Just_a_disjunction(l) -> "Just_a_disjunction(["^(String.concat ";" (Image.image (fun s->"\""^s^"\"") l))^"])"
  |Just_a_star(nm) -> "Just_a_star(\""^nm^"\")"
  |Synonym(nm) -> "Synonym(\""^nm^"\")";;  

let order = (
   (fun form1 form2 ->Total_ordering.standard form1 form2): 
     form Total_ordering_t.t ) ;; 

(* This is a registered printer : print_out *)
let print_out (fmt:Format.formatter) form=
   Format.fprintf fmt "@[%s@]" (Strung.with_size_limit ~size_limit:250 (Private.to_string form));;


let synonym_content form = match form with 
   (Synonym nm) -> nm   
   |Just_a_disjunction _
   |Just_a_concat _
   |Just_atomic  _
   |Just_a_star _
   |Just_an_optional _ -> raise(Synonym_content_exn(form));;

let to_string = Private.to_string ;; 

let unordered_coatoms = Private.unordered_coatoms ;;   