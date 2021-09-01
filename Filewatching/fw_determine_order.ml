(*

#use"Filewatching/fw_determine_order.ml";;

*)


exception Nonadmissible_acolytes_list of Dfn_rootless_t.t list ;;
exception Several_locations_for_one_ending of (Dfa_ending_t.t * Dfn_rootless_t.t list) list;;
exception Circular_dependencies_detected ;;

module Private = struct 


let check_admissibility_of_acolytes_list l=
   let temp1 = Image.image (fun (rl,details)->(Dfn_rootless.to_ending rl,(rl,details))) l in 
   let temp2 = Listennou.partition_according_to_fst temp1 in 
   let temp3 = List.filter (fun (edg,l_rl)->List.length(l_rl)>1) temp2 in 
   if temp3<>[]
   then let temp5 = Image.image (fun (edg,detailed_l) -> (edg,Image.image fst detailed_l) ) temp3 in 
        raise(Several_locations_for_one_ending(temp5))
   else 
   let temp4 = Image.image (fun (edg,l_rl)->
      (Dfa_ending.convert_to_ocaml_ending edg,List.hd l_rl)
      ) temp3   in 
   let (temp5,temp6) = List.partition 
    (function (edg,rl)->edg=Dfa_ocaml_ending_t.Mli) temp4 in 
   if (temp4=[]) || (List.length(temp6)>1)
   then raise(Nonadmissible_acolytes_list(Image.image (fun (_,(rl,_))->rl) temp4))
   else        
   let opt_mli_detailed_rless = (
      if temp5=[] 
      then None 
      else Some(snd(List.hd temp5))) in
   let principal_detailed_rless = (
      if temp6=[]
      then Option.unpack opt_mli_detailed_rless 
      else snd(List.hd temp6)    
   ) in     
   (opt_mli_detailed_rless,principal_detailed_rless) ;;

let classify_according_to_module compilable_files =
    let temp1 = Image.image (fun (rless,details)->
       (Dfn_rootless.to_module rless,(rless,details))  
    ) compilable_files in 
    let temp2 = Listennou.partition_according_to_fst temp1 in 
    Image.image (fun (mn,l)->
      let (opt_mli,principal)=check_admissibility_of_acolytes_list l in 
      (mn,(opt_mli,principal))
      ) temp2 ;;

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

let compute_dependencies  prepared_list_of_modules =
  let lex_sort = Ordered.sort lex_order in 
  let modules_in_lex_order = lex_sort (Image.image fst prepared_list_of_modules) in 
  let coatoms = Memoized.make (fun mname ->
     let (opt_mli,(pr_rless,pr_details)) = List.assoc mname  prepared_list_of_modules in 
     let mli_part = (match opt_mli with 
       None->[] 
       |(Some (mli_rless,mli_details))->Fw_file_small_details.used_modules mli_details)
     and pr_part = Fw_file_small_details.used_modules pr_details in 
     let temp1 = lex_sort(mli_part@pr_part) in 
     Ordered.intersect lex_order  modules_in_lex_order temp1
  )     in 
  let (cycles,good_list) = Reconstruct_linear_poset.reconstruct_linear_poset coatoms  modules_in_lex_order in 
  let _ = treat_circular_dependencies cycles in
  (good_list,coatoms) ;; 

end ;;   


let main fw  = 
   let u_files=Fw_with_small_details.usual_compilable_files fw in 
   let temp2=List.filter (fun (rl,_)->List.mem rl u_files)
      (fw.Fw_with_small_details_t.small_details_in_files)  in
   let temp3= Private.classify_according_to_module temp2 in
   let (ancestors_for_module,old_coatom_map)= Private.compute_dependencies temp3 in
   let dep_ordered_list_of_modules = Image.image fst ancestors_for_module in  
   Image.image (
     fun (mn,ancestors)->
      let (opt_mli_rless,(pr_rless,pr_details)) = List.assoc mn temp3 in 
      let unordered_fathers  = old_coatom_map mn in 
      let fathers = List.filter (fun mn2->
         Ordered.mem Private.lex_order mn2 unordered_fathers
         ) dep_ordered_list_of_modules in 
      (mn,Dfn_rootless.to_subdirectory pr_rless,
       Dfn_rootless.to_ending pr_rless,pr_details,opt_mli_rless,
       fathers,ancestors)  
   ) ancestors_for_module ;;


(*   
    modules : Dfa_module_t.t list;
    subdir_for_module : (Dfa_module_t.t * Dfa_subdirectory_t.t) list;
    principal_ending_for_module : (Dfa_module_t.t * Dfa_ending_t.t) list;
    mli_presence_for_module : (Dfa_module_t.t * bool) list;
    
    principal_mt_for_module : (Dfa_module_t.t * string) list;
    mli_mt_for_module : (Dfa_module_t.t * string) list;
    direct_fathers_for_module : (Dfa_module_t.t * Dfa_module_t.t list) list;
    ancestors_for_module : (Dfa_module_t.t * Dfa_module_t.t list) list;

      
   needed_libs_for_module : (Dfa_module_t.t * Ocaml_library_t.t list) list;
   needed_dirs_for_module : (Dfa_module_t.t * Dfa_subdirectory_t.t list) list;
   directories : Dfa_subdirectory_t.t list;

   product_up_to_date_for_module : (Dfa_module_t.t * bool) list;
   printer_equipped_types : (Dfn_endingless_t.t * bool) list;

*)   
