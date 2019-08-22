(*

#use"Decomposed_filename/dfa_ending_t.ml";;

*)


(*
let ml=Dfa_ending_t.Ml 
and mli=Dfa_ending_t.Mli 
and mll=Dfa_ending_t.Mll 
and mly=Dfa_ending_t.Mly;;



let exhaustive_uple f=(f ml,f mli,f mll,f mly);;

(*
Caution! The order is important in the all_endings list below.
It says in which order of priority one should look for
a file to read in order to extract dependency information
about the module. See the find_suitable_ending below.
*)


let all_endings=[mll;mly;ml;mli];;
let all_string_endings=[".mll";".mly";".ml";".mli"];;
let correspondances=List.combine all_endings all_string_endings;;

exception Unknown_ending of string;;

let of_string s=
  try (fst(Option.find (fun (x,y)->y=s) correspondances))
  with _->raise(Unknown_ending(s));;

let to_string edg=snd(Option.find (fun (x,y)->x=edg) correspondances);;  


let crobj_correspondances= 
  List.combine [mll;mly;ml;mli] 
  (Image.image (fun s->"Dfa_"^"ending_t."^s) ["Mll";"Mly";"Ml";"Mli"]);;

let of_concrete_object =Concrete_object_field.unwrap_lonely_variant 
  crobj_correspondances;;
          
let to_concrete_object =Concrete_object_field.wrap_lonely_variant 
  crobj_correspondances;;           
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
    Concrete_object_t.Variant ("Dfa_"^"ending_t.E",[Concrete_object_t.String(e)]);;

let of_concrete_object crobj =
   let (_,(arg1,_,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
   Dfa_ending_t.E(
      Concrete_object_field.unwrap_string arg1
   );;

