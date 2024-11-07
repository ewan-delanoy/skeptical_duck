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
 
let string_of_fiftuple_list l=        
 "\n\n[\n"^(String.concat ";\n" 
 (Image.image (fun f->"   "^(string_of_fiftuple f)) l))^"\n]\n\n" ;;

let mathusalem_make_persistent l = 
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

let mathusalem_transform1  l = 
  Image.image (
     fun (includer_fn,Cee_wardrobe_t.Wr(inclusions)) -> 
       if inclusions = []
       then (includer_fn,[(0,""),Cee_shadow_t.Sh(0,[])]) 
       else (includer_fn,inclusions)
  ) l ;; 

exception Rev_transform1_exn ;;

let mathusalem_rev_transform1 l = 
  Image.image (
     fun (includer_fn,inclusions) -> 
       match inclusions with 
       [] -> raise Rev_transform1_exn
       | (included_fn,_) :: _ ->
        if included_fn = (0,"")    
       then (includer_fn,Cee_wardrobe_t.Wr []) 
       else (includer_fn,Cee_wardrobe_t.Wr inclusions)
  ) l ;; 
  
let mathusalem_transform2  ll = List.flatten(Image.image (
   fun (includer_fn,l)->Image.image (fun 
   ((inclusion_index,included_fn),sh)-> 
      let (n,l2) = shadow_to_pair sh in 
      (includer_fn,inclusion_index,included_fn,n,l2)) l
) ll);;   

let mathusalem_rev_transform2 l = Hurried.reaggregate (
  (fun (includer_fn,inclusion_index,included_fn,n,l2) ->
    (includer_fn,((inclusion_index,included_fn),shadow_of_pair (n,l2)))
  )
) l ;;

let mathusalem_encode_wardrobe l = mathusalem_transform2 (mathusalem_transform1 l) ;;

let mathusalem_decode_wardrobe l = mathusalem_rev_transform1 (mathusalem_rev_transform2 l) ;;

end ;;

let mathusalem_decode_wardrobe = Private.mathusalem_decode_wardrobe ;;

let mathusalem_encode_wardrobe = Private.mathusalem_encode_wardrobe ;;

let mathusalem_store_as_wardrobe1 l = 
    Private.mathusalem_make_persistent (Private.mathusalem_encode_wardrobe l) ;;