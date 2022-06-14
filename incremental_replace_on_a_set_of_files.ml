(*

#use"incremental_replace_on_a_set_of_files.ml";;

*)

exception Replacement_file_undefined ;;
exception Markers_undefined ;;

module Private = struct 

let ocamlese_for_rep (a,b) = "(\""^(String.escaped a)^"\",\""^(String.escaped b)^"\")" ;; 

let ocamlese_for_replist l = 
  let lines = Image.image (fun rep->(ocamlese_for_rep rep)) l in 
  "\n\n\nlet replacements = [\n"^(String.concat "\n" lines)^"\n];;\n\n\n" ;; 

let string_order = Total_ordering.silex_for_strings ;;   
let rep_order = Total_ordering.product string_order string_order ;;

let the_replacement_datafile = ref None ;;
let the_markers = ref None ;; 

let the_receiving_files = ref [] ;;
let the_replacements = ref [] ;;

let get_replacements_datafile () = match (!the_replacement_datafile) with 
   Some answer -> answer 
   | None -> raise (Replacement_file_undefined) ;; 

let get_markers () = match (!the_markers) with 
   Some answer -> answer 
   | None -> raise (Markers_undefined) ;;    

let persist_onto_datafile () = 
    let reps_in_ocamlese = ocamlese_for_replist (!the_replacements) in 
    Replace_inside.overwrite_between_markers_inside_file
      (Overwriter.of_string reps_in_ocamlese) (get_markers()) (get_replacements_datafile ()) ;;

let set_replacements_datafile filename = ( the_replacement_datafile:=Some filename) ;;
let set_markers a b = ( the_markers:=Some (a,b)) ;;
let set_receiving_files l = (the_receiving_files:=l) ;;



let initialize_replacements l = (the_replacements:=l; persist_onto_datafile ()) ;; 


let apply_all () = 
    let pairs = Cartesian.product (!the_replacements) (!the_receiving_files) in 
    List.iter (fun (rep,file)->
      Replace_inside.silently_replace_inside_file rep file ) pairs ;;

let add_new_replacement new_rep=      
    let old_replist = !the_replacements in 
    let new_replist = Ordered.insert rep_order new_rep old_replist in 
    (the_replacements:=new_replist; 
     persist_onto_datafile ();
     List.iter (fun file->
      Replace_inside.replace_inside_file new_rep file) (!the_receiving_files)
     ) ;; 

end ;;
   
let add_new_replacement  = Private.add_new_replacement ;; 
let apply_all  = Private.apply_all ;; 
let initialize_replacements = Private.initialize_replacements ;;
let set_markers = Private.set_markers ;; 
let set_receiving_files = Private.set_receiving_files ;;
let set_replacements_datafile = Private.set_replacements_datafile ;; 

