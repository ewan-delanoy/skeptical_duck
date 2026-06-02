(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_disjunction_ladder_finder.ml";;

*)


module Private = struct 


open Jvng_types ;;


let apply (DLF atomic_finders) production_name token_types =
   match List.find_map (fun (DLAF tester) ->tester token_types) atomic_finders with 
   None -> None
   |Some ladder ->
     List_again.next_in_line_opt production_name ladder ;;
   
let make l = Jvng_types.DLF(Image.image (fun f->DLAF f) l);;

end ;; 

let apply = Private.apply ;;

let make = Private.make ;;


