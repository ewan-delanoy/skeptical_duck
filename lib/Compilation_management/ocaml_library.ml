(* 

#use"lib/Compilation_management/ocaml_library.ml";;

*)


let correspondances=[
   Ocaml_library_t.ZarithLib,"zarith";
   Ocaml_library_t.StrLib,"str";
   Ocaml_library_t.UnixLib,"unix"];;
let capitalized_correspondances =Image.image (
   fun (x,y)->(x,"Ocaml"^"_library_t."^y)
) correspondances;;

exception Unknown_lib of string;;

let of_string s=
  try (fst(List.find (fun (_x,y)->y=s) correspondances))
  with _->raise(Unknown_lib(s));;

let to_string lib=snd(List.find (fun (x,_y)->x=lib) correspondances);;  


let short_name lib =
  (String.capitalize_ascii (to_string lib))^"Lib" ;;
  

let ocaml_name lib=
  (*cutting the name as always, to avoid a circular definition *)
  "Ocaml"^"_library."^(short_name lib);;

let file_for_library lib =to_string lib;;  

let modules_telling_a_library_away =function
Ocaml_library_t.ZarithLib->["z";"q"] 
|Ocaml_library_t.StrLib->["str"] 
|Ocaml_library_t.UnixLib->["unix"];;    


let all_libraries=[
  Ocaml_library_t.ZarithLib;
  Ocaml_library_t.StrLib;
  Ocaml_library_t.UnixLib];;  

let compute_needed_libraries_from_uncapitalized_modules_list l=
   List.filter (
      fun lib->List.exists(
        fun z->List.mem z (modules_telling_a_library_away lib)
      ) l
   ) all_libraries;;
           
let of_concrete_object =Concrete_object.unwrap_lonely_variant 
  capitalized_correspondances;;
          
let to_concrete_object =Concrete_object.wrap_lonely_variant 
  capitalized_correspondances;;    
