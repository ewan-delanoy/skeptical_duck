(*

#use"Filewatching/fw_determine_order.ml";;

*)



exception Circular_dependencies_detected ;;

module Private = struct 

let treat_circular_dependencies m_cycles= 
      if m_cycles=[]
      then ()
      else
      let cycles = Image.image (Image.image Dfa_module.to_line) m_cycles in    
      let temp1=Image.image(String.concat " -> ") cycles in
      let temp2="\n\n The following cycles have been detected : "^
        (String.concat "\n\n" temp1) in
      let _ = (print_string temp2;flush stdout) in 
      raise Circular_dependencies_detected ;;

let lex_order = ((fun (Dfa_module_t.M m1) (Dfa_module_t.M m2)->
   Total_ordering.lex_for_strings m1 m2) : Dfa_module_t.t Total_ordering_t.t);;

let compute_dependencies  l =
  let lex_sort = Ordered.sort lex_order in 
  let modules = Image.image fst l in 
  let modules_in_lex_order = lex_sort modules in 
  let coatoms_in_lex_order = Memoized.make (fun mname ->
     let details = List.assoc mname  l in 
     let temp1 = lex_sort(details.Fw_module_small_details_t.used_modules) in 
     Ordered.intersect lex_order  modules_in_lex_order temp1
  )     in 
  let (cycles,good_list) = Reconstruct_linear_poset.reconstruct_linear_poset coatoms_in_lex_order  
    modules in 
  let _ = treat_circular_dependencies cycles in
  let coatoms = Memoized.make (fun mname ->
    let old_order = coatoms_in_lex_order mname in 
    Option.filter_and_unpack (fun (mn,_)->
       if Ordered.mem lex_order mn old_order 
       then Some mn 
       else None  
      ) good_list
   ) in 
  Image.image (fun (mn,ancestors)->(mn,(coatoms mn,ancestors))) good_list ;; 

let compute_coatoms_in_small_extension older_modules extension =
   let modules_in_correct_order = 
       (Image.image fst older_modules) @ (Image.image fst extension) in 
   let lex_sort = Ordered.sort lex_order in 
   let coatoms = (fun mname ->
      let details = List.assoc mname  extension in 
      let temp1 = lex_sort(details.Fw_module_small_details_t.used_modules) in 
      List.filter  (fun mn-> Ordered.mem lex_order mn temp1) modules_in_correct_order
       )  in
   Image.image (fun (mname,_)->(mname,coatoms mname)) extension ;;           

let rec iterator_for_ancestor_computation (treated,to_be_treated) =
   match to_be_treated with 
   [] -> treated 
   | (mn,coat_mn) :: others ->
      let temp1 = (coat_mn) :: (Image.image (fun mn2->snd(List.assoc mn2 treated)) coat_mn) in 
      let ancestors_in_lex_order = Ordered.sort lex_order (List.flatten temp1) in 
      let ancestors_mn = Option.filter_and_unpack (
         fun (mn3,_) ->
            if Ordered.mem lex_order mn3  ancestors_in_lex_order 
            then Some mn3 
            else None   
      )  treated in 
      iterator_for_ancestor_computation (treated@[(mn,(coat_mn,ancestors_mn))],others) ;;  

let compute_coatoms_and_ancestors_in_small_extension older_modules extension =
   let ext_with_coatoms = compute_coatoms_in_small_extension older_modules extension in 
   iterator_for_ancestor_computation (older_modules,ext_with_coatoms) ;;

end ;;   

let compute_coatoms_and_ancestors_in_small_extension = Private.compute_coatoms_and_ancestors_in_small_extension ;;
let main = Private.compute_dependencies ;;


