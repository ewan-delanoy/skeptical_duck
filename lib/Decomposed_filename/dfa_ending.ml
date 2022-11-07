(*

#use"lib/Decomposed_filename/dfa_ending.ml";;

*)



exception Dot_inside_ending of string;;
exception Not_an_ocaml_ending of string;;
exception Unknown_ending of Dfa_ending_t.t ;;

let of_line e =
  if String.contains e '.'
  then raise(Dot_inside_ending(e))
  else Dfa_ending_t.E(e);;

let connectable_to_modulename (Dfa_ending_t.E(e)) = "." ^ e ;;



let mll =  Dfa_ending_t.E "mll"
and mly =  Dfa_ending_t.E "mly"
and ml  =  Dfa_ending_t.E "ml"
and mli =  Dfa_ending_t.E "mli" ;; 

let all_ocaml_endings= [mll;mly;ml;mli];;

let all_cee_endings = Image.image (fun s->Dfa_ending_t.E s) ["h";"c"];;

let compute_on_all_ocaml_endings f=(f ml,f mli,f mll,f mly);;

let endings_for_compilable_files = 
   (all_ocaml_endings) @ all_cee_endings ;;

let endings_for_noncompilable_readable_files = 
     Image.image (fun s->Dfa_ending_t.E s) ["txt";"html";"php";"js";"ejs";"json"];; 

let endings_for_readable_files = 
     endings_for_compilable_files @ endings_for_noncompilable_readable_files ;;

let is_compilable edg =
   if List.mem edg endings_for_compilable_files 
   then true 
   else 
   if List.mem edg endings_for_noncompilable_readable_files 
   then false 
   else raise(Unknown_ending(edg));;



let to_concrete_object (Dfa_ending_t.E(e)) =
    Concrete_object_t.Variant ("Dfa_"^"ending_t.E",
    [Crobj_converter.string_to_concrete_object(e)]);;

let of_concrete_object crobj =
   let (_,(arg1,_,_,_,_,_,_))=Concrete_object.unwrap_bounded_variant crobj in 
   Dfa_ending_t.E(
      Crobj_converter.string_of_concrete_object arg1
   );;

