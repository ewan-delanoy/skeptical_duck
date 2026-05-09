(*

#use"lib/Java_analysis/jvsp_abstract_language.ml";;

*)

type element_in_concat = Jvsp_abstract_language_t.element_in_concat = 
   Ref of string |Atomic of Jvsp_types.token_type | Star of string ;;

type element_in_disjunction = Jvsp_abstract_language_t.element_in_disjunction = 
   Concat of element_in_concat list ;;
     
type form =  Jvsp_abstract_language_t.form = Disjunction of element_in_disjunction list ;;

type t =  Jvsp_abstract_language_t.t = AL of (string * form) list ;; 


module Private = struct 

let ocaml_name_of_element_in_concat = function 
  (Ref nm) -> "Ref(\""^nm^"\")"
  |(Atomic tok) -> "Atomic("^(Jvsp_util.summary_of_token_type tok)^")"
  |(Star nm) -> "Star(\""^nm^"\")"  ;;

let ocaml_name_of_element_in_disjunction (Concat l) =
   "Concat(["^(String.concat ";" (Image.image ocaml_name_of_element_in_concat l))^"])";;

let ocaml_name_of_form (Disjunction l) =
   "Disjunction(["^(String.concat ";" (Image.image ocaml_name_of_element_in_disjunction l))^"])";;

let ocaml_name_of_sf (name,frm) =
    "(\""^name^"\","^(ocaml_name_of_form frm)^")";;

let ocaml_name (AL l)=
  let lines = Image.image (fun sf->(String.make 3 ' ')^(ocaml_name_of_sf sf)^";") l in 
"AL ([\n\n"^
(String.concat "\n" lines)^
"\n\n])" ;; 

let get (AL l) name = List.assoc name l ;;

let element_in_concat_to_string = function
   (Ref nm) -> nm
  |(Atomic tok) -> "Atomic("^(Jvsp_util.ocaml_name_for_token_type tok)^")"
  |(Star nm) -> "("^nm^")*"  ;;

let element_in_disjunction_to_string (Concat l) =
   String.concat " " (Image.image element_in_concat_to_string l) ;;

let form_to_string (Disjunction l) =
   "\n"^(String.concat "\n" (Image.image (fun elt->
      "|"^(element_in_disjunction_to_string elt)) l))^"\n" ;;

let print_out_form (fmt:Format.formatter) form=
   Format.fprintf fmt "@[%s@]" (form_to_string form);;

end ;; 

let get = Private.get ;;
let ocaml_name = Private.ocaml_name ;;

(* This is a registered printer : print_out_form *)
let print_out_form = Private.print_out_form ;;