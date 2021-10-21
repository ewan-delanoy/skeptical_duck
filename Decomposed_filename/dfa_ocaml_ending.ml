(*

#use"Decomposed_filename/dfa_ocaml_ending.ml";;

*)




exception Not_an_ocaml_ending of string;;

module Private = struct

let to_string = function 
   Dfa_ocaml_ending_t.Mll ->  "mll"
  | Dfa_ocaml_ending_t.Mly -> "mly"
  | Dfa_ocaml_ending_t.Ml ->  "ml"
  | Dfa_ocaml_ending_t.Mli ->  "mli" ;; 


let capitalized_correspondances= Image.image (fun cml_edg->
    (cml_edg,to_string cml_edg)
   ) [
   Dfa_ocaml_ending_t.Mll ;  
   Dfa_ocaml_ending_t.Mly ; 
   Dfa_ocaml_ending_t.Ml  ; 
   Dfa_ocaml_ending_t.Mli 
 ];;

end ;;

let of_ending (Dfa_ending_t.E(e)) = 
   match Option.seek (fun (cml_edg,e2)->e2=e) Private.capitalized_correspondances with 
   None -> raise(Not_an_ocaml_ending(e)) 
   |(Some(cml_edg,_)) -> cml_edg ;; 
   
   
let of_concrete_object =Concrete_object.unwrap_lonely_variant 
   Private.capitalized_correspondances;;
           
let to_concrete_object =Concrete_object.wrap_lonely_variant 
   Private.capitalized_correspondances;;    
 
let to_ending cml_edg = Dfa_ending_t.E(Private.to_string cml_edg) ;;   