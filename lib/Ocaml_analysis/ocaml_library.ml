(* 

#use"lib/Ocaml_analysis/ocaml_library.ml";;

*)

module Private = struct
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

let modules_telling_a_library_away =function
Ocaml_library_t.ZarithLib->["z";"q"] 
|Ocaml_library_t.StrLib->["str"] 
|Ocaml_library_t.UnixLib->["unix"];;    


let all_libraries=[
  Ocaml_library_t.ZarithLib;
  Ocaml_library_t.StrLib;
  Ocaml_library_t.UnixLib];;  

let containing_dir lib=
 let s = (Unix_again.get_command_output("ocamlfind query "^(to_string lib))) in 
 let n = String.length s in 
 if String.get s (n-1)='\n'
 then String.sub s 0 (n-1) 
 else s;;

end ;;  

let compute_needed_libraries_from_uncapitalized_modules_list l=
   List.filter (
      fun lib->List.exists(
        fun z->List.mem z (Private.modules_telling_a_library_away lib)
      ) l
   ) Private.all_libraries;;

let containing_dir = Private.containing_dir ;;

let exact_location lib = (Private.containing_dir lib)^"/"^(Private.to_string lib)^".cma" ;;


let of_concrete_object =Concrete_object.unwrap_lonely_variant 
  Private.capitalized_correspondances;;
          
let to_concrete_object =Concrete_object.wrap_lonely_variant 
  Private.capitalized_correspondances;;    
