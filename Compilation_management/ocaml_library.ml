
(* 

#use"Compilation_management/ocaml_library.ml";;

*)


type t=NumLib |StrLib |UnixLib;;

let correspondances=[NumLib,"num";StrLib,"str";UnixLib,"unix"];;
let capitalized_correspondances =Image.image (
   fun (x,y)->(x,"Ocaml"^"_library."^y)
) correspondances;;

exception Unknown_lib of string;;

let of_string s=
  try (fst(Listennou.force_find (fun (x,y)->y=s) correspondances))
  with _->raise(Unknown_lib(s));;

let to_string lib=snd(Listennou.force_find (fun (x,y)->x=lib) correspondances);;  


let short_name=function
   NumLib->"NumLib" 
  |StrLib->"StrLib" 
  |UnixLib->"UnixLib";;

let ocaml_name lib=
  (*cutting the name as always, to avoid a circular definition *)
  "Ocaml"^"_library."^(short_name lib);;

let file_for_library=function 
  NumLib->"nums" |StrLib->"str" |UnixLib->"unix";;  

let modules_telling_a_library_away=function
NumLib->["num";"big_int";"arith_status"] 
|StrLib->["str"] 
|UnixLib->["unix"];;    


let all_libraries=[NumLib;StrLib;UnixLib];;  

let compute_needed_libraries_from_uncapitalized_modules_list l=
   List.filter (
      fun lib->List.exists(
        fun z->List.mem z (modules_telling_a_library_away lib)
      ) l
   ) all_libraries;;
           
let of_concrete_object =Concrete_object_field.unwrap_lonely_variant 
  capitalized_correspondances;;
          
let to_concrete_object =Concrete_object_field.wrap_lonely_variant 
  capitalized_correspondances;;    
