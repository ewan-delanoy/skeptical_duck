(*

#use"ocaml_ending.ml";;

*)

type t=Ml |Mli |Mll |Mly;;

let ml=Ml and mli=Mli and mll=Mll and mly=Mly;;



let exhaustive_uple f=(f Ml,f Mli,f Mll,f Mly);;

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


let of_concrete_object =Concrete_object_field.unwrap_lonely_variant 
  ["Ml",Ml;"Mli",Mli;"Mll",Mll;"Mly",Mly];;

(*           
let to_concrete_object =Concrete_object_field.unwrap_lonely_variant 
  ["Ml",Ml;"Mli",Mli;"Mll",Mll;"Mly",Mly];;           
*)  