(*

#use"lib/Java_analysis/jvsp_abstract_language.ml";;

*)

type element_in_concat = Jvsp_abstract_language_t.element_in_concat = 
   Ref of string |Atomic of Jvsp_types.token_type | Star of string ;;

type element_in_disjunction = Jvsp_abstract_language_t.element_in_disjunction = 
   Concat of element_in_concat list ;;
     
type form =  Jvsp_abstract_language_t.form = Disjunction of element_in_disjunction list ;;

type t =  Jvsp_abstract_language_t.t = AL of (string * form) list ;; 

(*
let ocaml_name_of_element_in_concat = function 
  (Ref nm) -> "Ref(\""^nm^"\")"
  |(Atomic tok) -> 
   "Concat(["^(String.concat ";" (Image.image ocaml_name_of_element_in_concat l))^"])";;

let ocaml_name_of_element_in_disjunction (Disjunction l) =
   "Concat(["^(String.concat ";" (Image.image ocaml_name_of_element_in_concat l))^"])";;

let ocaml_name_of_form (Disjunction l) =
   "Disjunction(["^(String.concat ";" (Image.image ocaml_name_of_element_in_disjunction l))^"])";;

let ocaml_name_of_sf (name,frm) =
    "(\""^name^"\","^(ocaml_name_of_form frm)^")";;

let ocaml_name (AL l)=
  let lines = Image.image (fun sf->(String.make 3 ' ')^(ocaml_name_of_sf sf)^";") l in 
"AL ([\\n"^
(String.concat "\n" lines)^
"\n\n])" ;; 

*)