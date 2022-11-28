(* 

#use"lib/Ocaml_analysis/Concrete_ocaml_objects/encoded_string.ml";;

This is a leaky abstraction, as witnessed by the "retrieve" and "store" functions above.


*)

exception Forbidden_substring;;
exception Forbidden_recombination;;

let salt = 
  String.concat "" ["a"; "Y"; "2"; "u"; "k"; "k"; "w"; "D"; "z"; "y"; "K"; "d"];;
let replacement_salt = 
  String.concat "" ["b"; "Z"; "3"; "v"; "l"; "m"; "x"; "E"; "A"; "z"; "L"; "e"];;

let decode (Encoded_string_t.E(encoded_s))=
   Replace_inside.replace_inside_string ~display_number_of_matches:false (replacement_salt,salt) encoded_s;;

let encode s=
   if Substring.is_a_substring_of replacement_salt s 
   then raise(Forbidden_substring)
   else let encoded_s=Replace_inside.replace_inside_string ~display_number_of_matches:false (salt,replacement_salt) s in 
        if  Substring.is_a_substring_of salt encoded_s 
        then raise(Forbidden_substring)
        else Encoded_string_t.E(encoded_s);;

let retrieve encoded_s = (Encoded_string_t.E(encoded_s));;
let store (Encoded_string_t.E(encoded_s))= encoded_s;;
