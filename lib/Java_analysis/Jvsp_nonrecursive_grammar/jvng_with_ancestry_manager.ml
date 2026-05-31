(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_with_ancestry_manager.ml";;

*)

open Jvng_types ;;

exception Get_ancestry_data_for_dname_exn of Jvng_duplicated_name.t ;;

module Private = struct

let get_ancestry_data_for_dname (AM l) dname = match List.assoc_opt dname l with 
 None -> raise(Get_ancestry_data_for_dname_exn(dname))
 |Some data -> data;;

let order_for_pairs = ((
   Total_ordering.product Jvng_duplicated_name.order Total_ordering.standard
   ): 
   (Jvng_duplicated_name.t * ancestry_data) Total_ordering_t.t
) ;; 
let insert_in_ancestry_manager (AM l) pair =
    AM(Ordered.insert order_for_pairs pair l);;

let insert_several_in_ancestry_manager man pairs =
   List.fold_left insert_in_ancestry_manager man pairs ;;
 
let lower_bound_according_to_ancestry name ancestry =
   let temp = List.filter_map (fun dname ->
      if Jvng_duplicated_name.name dname = name 
      then Some (Jvng_duplicated_name.index dname)
      else None) ancestry in 
   if temp = []
   then 1
   else 1+(Max.list temp) ;;  

let rec compute_final_index indices_to_be_avoided lower_bound =
   if List.mem lower_bound indices_to_be_avoided 
   then compute_final_index indices_to_be_avoided (lower_bound+1)
   else lower_bound ;;   

let compute_suitable_duplicate common_ancestry (treated_coatoms,old_manager) (coatom_idx,coatom) =
   let (AM old_ancestry_data)=old_manager in 
   let similar_items = List.filter (fun (nm,data)->
      (data.ancestry = common_ancestry) && ((Jvng_duplicated_name.name nm)=coatom) 
   ) old_ancestry_data in 
   match List.find_opt ( fun (_,data)->data.position_in_birth_list = coatom_idx) similar_items with 
   Some(old_answer,_) -> (old_answer::treated_coatoms,AM old_ancestry_data)
   |None ->
   let current_location = {
     ancestry = common_ancestry ;
     position_in_birth_list = coatom_idx ;
   } in 
   let indices_to_be_avoided = Image.image ( fun (_,data)->data.position_in_birth_list) similar_items 
   and lower_bound = lower_bound_according_to_ancestry coatom common_ancestry  in 
   let final_idx = compute_final_index indices_to_be_avoided lower_bound in 
   let final_dname = Jvng_duplicated_name.make coatom final_idx in 
   let new_pair = (final_dname,current_location) in 
   (final_dname::treated_coatoms,insert_in_ancestry_manager old_manager new_pair ) ;;    


let duplicate_in_list old_ancestry_data common_ancestry coatoms = 
   List.fold_left (compute_suitable_duplicate common_ancestry) ([],old_ancestry_data) 
     (Int_range.index_everything coatoms) 
    ;;


let duplicate_in_form gram_with_am dname original_form = 
   match Jvag_form.uniform_decomposition_opt original_form with 
   None -> (Jvng_jvag_types.Molecular(
      Option.get(Jvag_form.molecular_content_opt original_form)
   ),gram_with_am.Jvng_types.manager)
   |Some(link,coatoms) ->
      let old_ancestry_data = gram_with_am.Jvng_types.manager in 
      let common_ancestry = dname :: ((get_ancestry_data_for_dname old_ancestry_data dname).Jvng_types.ancestry) in 
      let (new_coatoms,new_ancestry_data) = duplicate_in_list old_ancestry_data common_ancestry coatoms in 
      (Jvng_jvag_form.uniform_composition link (List.rev new_coatoms),new_ancestry_data) ;;

let get gram_with_am dname =
   let src = gram_with_am.Jvng_types.source 
   and rcvr = gram_with_am.Jvng_types.receiver  in 
   match Jvng_jvag_grammar.get_opt rcvr dname with 
   (Some answer) -> (answer,None)
   | None -> 
      let name = Jvng_duplicated_name.name dname in 
      let original_form = Jvag_grammar.get src name in 
      let (adjusted_form,new_ancestry_data) = duplicate_in_form gram_with_am dname original_form in 
      let final_receiver = 
           Jvng_jvag_grammar.add_pair_naively (dname,adjusted_form) rcvr in 
      let final_gram = {
          gram_with_am with 
          receiver = final_receiver ;
          manager = new_ancestry_data 
      } in 
      (adjusted_form,Some final_gram) ;;


let make src origin =
   let first_form_v1 = Jvag_grammar.get src origin 
   and first_dname = Jvng_duplicated_name.make origin 1 in 
   let (link,coatoms) = Option.get(Jvag_form.uniform_decomposition_opt first_form_v1) in 
   let first_coatoms = Image.image (fun nm -> Jvng_duplicated_name.make nm 1) coatoms in 
   let first_form_v2 = Jvng_jvag_form.uniform_composition link (first_coatoms) in 
   let first_nonrec_grammar =  Jvng_jvag_grammar.singleton first_dname first_form_v2 in 
   let first_data = { ancestry = [] ; position_in_birth_list = 0 } in  
   let indexed_first_coatoms = Int_range.index_everything first_coatoms in 
   let pairs_from_coatoms = Image.image ( 
     fun (idx,coatom) -> (coatom, { ancestry = [first_dname] ; position_in_birth_list = idx }) 
   ) indexed_first_coatoms in 
   let first_pairs = (first_dname,first_data) :: pairs_from_coatoms in 
   let first_manager = AM (Ordered.sort order_for_pairs first_pairs) in 
   {
      source =src;   
      receiver = first_nonrec_grammar ;
      manager = first_manager 
   } ;;
     
end ;;

let get = Private.get ;;

let make = Private.make ;; 