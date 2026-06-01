(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_disjunction_ladder.ml";;

*)


module Private = struct 


open Jvng_types ;;


let prepared_data_from_one_item (tester,sequence) =
   let deltaed_sequence = List_again.universal_delta_list sequence in 
   Image.image (fun (predecessor,successor)->(predecessor,
      (fun l-> if tester l then Some successor else None))) deltaed_sequence

let prepared_data_for_several_items items = 
   let all_data = List.flatten (Image.image prepared_data_from_one_item items) in
   let unordered_names = Image.image fst all_data in 
   let names = Ordered.sort Jvng_duplicated_name.order unordered_names in 
   Image.image (fun name->
     (name,List.filter_map (fun p->if fst(p)=name then Some(snd p) else None) all_data) 
   ) names ;;

let make pre_l =
  let l = Image.image (fun (f,z)->(f,Image.image Jvng_duplicated_name.of_string z)) pre_l in 
  {
    initial_data = l;
    computed_once_data = prepared_data_for_several_items l; 
  } ;;   

end ;; 

let make = Private.make ;;