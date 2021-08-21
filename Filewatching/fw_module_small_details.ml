(*

#use"Filewatching/fw_module_small_details.ml";;

From an associative list associating to each Dfn_rootless_t.t its small deatils,
we create a new list associating to each Dfa_module_t.t its small deatils.

*)


exception Nonadmissible_acolytes_list of Dfn_rootless_t.t list ;;
exception Several_locations_for_one_ending of (Dfa_ending_t.t * Dfn_rootless_t.t list) list;;
exception Several_subdirectories_for_one_ending of Dfn_rootless_t.t * Dfn_rootless_t.t ;;

module Private = struct 

let lex_order = ((fun (Dfa_module_t.M m1) (Dfa_module_t.M m2)->
   Total_ordering.lex_for_strings m1 m2) : Dfa_module_t.t Total_ordering_t.t);;

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
   let (principal_rless,principal_details) = principal_detailed_rless in 
   let principal_subdir = Dfn_rootless.to_subdirectory principal_rless in
   let _ = (
      match opt_mli_detailed_rless with 
      None -> () 
      |Some(mli_rless,_) ->
         let mli_subdir = Dfn_rootless.to_subdirectory mli_rless in 
         if mli_subdir <> principal_subdir 
         then raise(Several_subdirectories_for_one_ending(principal_rless,mli_rless))    
   ) in
   let principal_coatoms = Fw_file_small_details.used_modules principal_details in 
   let (all_coatoms,opt_mli_mt) = (
      match opt_mli_detailed_rless with 
      None -> (principal_coatoms,None)
      |Some(_,mli_details) ->
         let mli_coatoms = Fw_file_small_details.used_modules mli_details in 
         (principal_coatoms @ mli_coatoms,
         Some(Fw_file_small_details.modification_time mli_details))    
   ) in
   let all_coatoms_in_order = Ordered.sort lex_order all_coatoms in 
   {
      Fw_module_small_details_t.used_modules = all_coatoms_in_order ;
      used_libraries = Fw_file_small_details.used_libraries principal_details ;
      has_printer = Fw_file_small_details.has_printer principal_details ;
      subdirectory = principal_subdir ;
      principal_ending = Dfa_ending.convert_to_ocaml_ending (Dfn_rootless.to_ending principal_rless) ;
      mli_present = (opt_mli_detailed_rless <> None) ;
      principal_modification_time = Fw_file_small_details.modification_time principal_details ;
      mli_modification_time = opt_mli_mt ;
   };;

   
let classify_according_to_module compilable_files =
    let temp1 = Image.image (fun (rless,details)->
       (Dfn_rootless.to_module rless,(rless,details))  
    ) compilable_files in 
    let temp2 = Listennou.partition_according_to_fst temp1 in 
    Image.image (fun (mn,l)->
      (mn,check_admissibility_of_acolytes_list l)
      ) temp2 ;;


end ;;   

(*
let has_printer fw = fw.Fw_module_small_details_t.has_printer ;;  
*)

let modularize_details fw  = 
   let u_files=Fw_with_small_details.usual_compilable_files fw in 
   let temp1=List.filter (fun (rl,_)->List.mem rl u_files)
      (fw.Fw_with_small_details_t.small_details_in_files)  in
   Private.classify_according_to_module temp1 ;;

(*   
let mli_present fw = fw.Fw_module_small_details_t.mli_present ;; 
let opt_mli_modification_time fw = fw.Fw_module_small_details_t.mli_modification_time ;;      
let principal_ending fw = fw.Fw_module_small_details_t.principal_ending ;; 
let principal_modification_time fw = fw.Fw_module_small_details_t.principal_modification_time ;;     
*)

let recompute_details_for_module fw mod_name =  
   let temp1=List.filter (fun (rl,_)->(Dfn_rootless.to_module rl)=mod_name)
      (fw.Fw_with_small_details_t.small_details_in_files)  in
   Private.classify_according_to_module temp1 ;;

(*
let subdirectory fw = fw.Fw_module_small_details_t.subdirectory ;;  
let used_libraries fw = fw.Fw_module_small_details_t.used_libraries ;;    
let used_modules fw = fw.Fw_module_small_details_t.used_modules ;;  
*)
    
