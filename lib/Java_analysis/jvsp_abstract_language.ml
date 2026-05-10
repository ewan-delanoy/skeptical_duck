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

end ;; 

let get = Private.get ;;
let ocaml_name = Private.ocaml_name ;;
let order_on_pairs = Private.order_on_pairs ;;

(* This is a registered printer : print_out_form *)
let print_out_form = Private.print_out_form ;;