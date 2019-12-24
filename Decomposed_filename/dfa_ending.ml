(*

#use"Decomposed_filename/dfa_ending_t.ml";;

*)



exception Dot_inside_ending of string;;
exception Not_an_ocaml_ending of string;;

let of_line e =
  if String.contains e '.'
  then raise(Dot_inside_ending(e))
  else Dfa_ending_t.E(e);;

let connectable_to_modulename (Dfa_ending_t.E(e)) = "." ^ e ;;

let restrict_to_ocaml_ending (Dfa_ending_t.E(e)) =
   if not(Supstring.begins_with e "ml")
   then raise(Not_an_ocaml_ending(e))
   else let n=String.length(e) in 
        if n=2 then Dfa_ocaml_ending_t.Ml else 
        if n>3 then raise(Not_an_ocaml_ending(e)) else 
        match String.get e 2 with 
         'i'->Dfa_ocaml_ending_t.Mli
        |'l'->Dfa_ocaml_ending_t.Mll
        |'y'->Dfa_ocaml_ending_t.Mly
        | _ -> raise(Not_an_ocaml_ending(e));;

let mll =  Dfa_ending_t.E "mll"
and mly =  Dfa_ending_t.E "mly"
and ml  =  Dfa_ending_t.E "ml"
and mli =  Dfa_ending_t.E "mli" ;; 

let all_ocaml_endings= [mll;mly;ml;mli];;

let compute_on_all_ocaml_endings f=(f ml,f mli,f mll,f mly);;

let to_concrete_object (Dfa_ending_t.E(e)) =
    Concrete_object_t.Variant ("Dfa_"^"ending_t.E",[Concrete_object_field.wrap_string(e)]);;

let of_concrete_object crobj =
   let (_,(arg1,_,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
   Dfa_ending_t.E(
      Concrete_object_field.unwrap_string arg1
   );;

