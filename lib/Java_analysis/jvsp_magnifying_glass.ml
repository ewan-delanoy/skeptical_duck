(*

#use"lib/Java_analysis/jvsp_magnifying_glass.ml";;

*)

module Private = struct 

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


end ;; 

(* This is a registered printer : print_out *)
let print_out = Private.print_out ;;