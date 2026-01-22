(*

#use"lib/Cee_language/cee_data_archiver.ml";;

*)

module Private = struct 


let string_of_int_list l = 
  "["^(String.concat ";" (Image.image string_of_int l))^"]" ;;

let string_of_fiftuple (s1,ii,s2,n,l) = 
  "("^(Strung.enclose s1)^","^
      (string_of_int ii)^","^
      (Strung.enclose s2)^","^ 
      (string_of_int n)^","^ 
      (string_of_int_list l)^")" ;;
 
let string_of_sixtuple (i1,s1,ii,s2,n,l) = 
  "("^(string_of_int i1)^","^
      (Strung.enclose s1)^","^
      (string_of_int ii)^","^
      (Strung.enclose s2)^","^ 
      (string_of_int n)^","^ 
      (string_of_int_list l)^")" ;;

let string_of_fiftuple_list l=        
 "\n\n[\n"^(String.concat ";\n" 
 (Image.image (fun f->"   "^(string_of_fiftuple f)) l))^"\n]\n\n" ;;

let string_of_sixtuple_list l=        
 "\n\n[\n"^(String.concat ";\n" 
 (Image.image (fun f->"   "^(string_of_fiftuple f)) l))^"\n]\n\n" ;;

let make_persistent l = 
let ap = Absolute_path.of_string "watched/watched_not_githubbed/fill_cee_cache.ml" 
and markers = (
"(* D"^"efinition of pre_wardrobe1 begins here *)",
"(* D"^"efinition of pre_wardrobe1 ends here *)"
)  in   
Replace_inside.overwrite_between_markers_inside_file 
 ~overwriter:(string_of_fiftuple_list l) markers ap 
;;  

let shadow_to_pair = function 
 (Cee_shadow_t.Sh(n,Cee_prawn_t.P l2)) -> (n,l2) ;; 

let shadow_of_pair (n,l2) = 
  Cee_shadow_t.Sh(n,Cee_prawn_t.P l2) ;; 


end ;;

 module Cache_Content = struct 

      (* 
       Those values will be set in another place
        to avoid lengthening the utop launch
      *)
    let list28_ref = ref ([]: (string list)) ;;

    

end ;;

