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
let ap = Absolute_path.of_string "watched/watched_not_githubbed/large_data.ml" 
and markers = (
"(* D"^"efinition of pre_wardrobe1 begins here *)",
"(* D"^"efinition of pre_wardrobe1 ends here *)"
)  in   
Replace_inside.overwrite_between_markers_inside_file 
 ~overwriter:(string_of_fiftuple_list l) markers ap 
;;  

let yamamoto_make_persistent l = 
let ap = Absolute_path.of_string "watched/watched_not_githubbed/large_data.ml" 
and markers = (
"(* D"^"efinition of pre_wardrobe1 begins here *)",
"(* D"^"efinition of pre_wardrobe1 ends here *)"
)  in   
Replace_inside.overwrite_between_markers_inside_file 
 ~overwriter:(string_of_fiftuple_list l) markers ap 
;;  

let shadow_to_pair = function 
 (Cee_shadow_t.Sh(n,l2)) -> (n,l2) ;; 

let shadow_of_pair (n,l2) = 
  Cee_shadow_t.Sh(n,l2) ;; 

let dummy_location = (0,"") ;;  

let transform1  l = 
  Image.image (
     fun (includer_fn,Cee_wardrobe_t.Wr(inclusions)) -> 
       if inclusions = []
       then (includer_fn,[dummy_location,Cee_shadow_t.Sh(0,[])]) 
       else (includer_fn,inclusions)
  ) l ;; 

exception Rev_transform1_exn ;;

let rev_transform1 l = 
  Image.image (
     fun (includer_fn,inclusions) -> 
       match inclusions with 
       [] -> raise Rev_transform1_exn
       | (included_fn,_) :: _ ->
        if included_fn = dummy_location    
       then (includer_fn,Cee_wardrobe_t.Wr []) 
       else (includer_fn,Cee_wardrobe_t.Wr inclusions)
  ) l ;; 
  
let transform2  ll = List.flatten(Image.image (
   fun (includer_fn,l)->Image.image (fun 
   ((inclusion_index,included_fn),sh)-> 
      let (n,l2) = shadow_to_pair sh in 
      (includer_fn,inclusion_index,included_fn,n,l2)) l
) ll);;   

let rev_transform2 l = Hurried.reaggregate (
  (fun (includer_fn,inclusion_index,included_fn,n,l2) ->
    (includer_fn,((inclusion_index,included_fn),shadow_of_pair (n,l2)))
  )
) l ;;

let encode_wardrobe l = transform2 (transform1 l) ;;

let decode_wardrobe l = rev_transform1 (rev_transform2 l) ;;

end ;;

let decode_wardrobe = Private.decode_wardrobe ;;

let encode_wardrobe = Private.encode_wardrobe ;;

let store_as_wardrobe1 l = 
    Private.make_persistent (Private.encode_wardrobe l) ;;