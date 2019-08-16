(*

#use"Decomposed_filename/dfa_ending_t.ml";;

*)



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
