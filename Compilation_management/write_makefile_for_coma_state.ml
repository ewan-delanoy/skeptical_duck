
(* 

#use"Compilation_management/write_makefile_for_coma_state.ml";;

*)

module Private=struct

  module Located_module = struct 
  
  type t={
   bundle_main_dir : string;
   subdirectory    : string;
   naked_module     : string;
  };;  

  let naked_module  x=Dfa_module.of_line(x.naked_module);;

  let uprooted_version x=
   let sub=x.subdirectory in
   if sub=""
   then x.naked_module
   else sub^"/"^(x.naked_module);;

  let unveil x=(uprooted_version x,Dfa_root.of_line(x.bundle_main_dir));;

  exception Inexistent_module of string;;
 
  let of_string_and_root old_s dir=
        let s=Cull_string.before_rightmost_possibly_all old_s '.' in
        let s_dir=Dfa_root.without_trailing_slash dir in
      if List.for_all (fun edg->
         let t = s_dir^"/"^s^(Dfa_ending.connectable_to_modulename edg) in 
        not(Sys.file_exists t )) 
           Dfa_ending.all_ocaml_endings
	    then raise(Inexistent_module(s_dir^s))
	    else
	    {
	      bundle_main_dir = s_dir;
   		  subdirectory    =Cull_string.before_rightmost s '/';
        naked_module     =Cull_string.after_rightmost s '/';
	    };;   

  let to_shortened_string x=x.naked_module;;   

  end;;

  let lm_at_idx cs k=
    let (Dfa_root_t.R r)= Coma_state.root cs in
    {
      Located_module.bundle_main_dir = r;
      subdirectory = (Dfa_subdirectory.without_trailing_slash(Coma_state.subdir_at_idx cs k));
      naked_module = (Dfa_module.to_line(Coma_state.module_at_idx cs k));
    } ;;

  let all_located_modules cs=
    let n=Small_array.size((Coma_state.modules cs)) in
    Ennig.doyle (lm_at_idx cs) 1 n;; 

  module Mlx_path=struct 

   type t=MLX of Dfa_ending_t.t*string*Dfa_root_t.t;;

   let short_path (MLX(edg,s,_))=match edg with
     Dfa_ending_t.E(e)-> s ^ "." ^ e;;

   let to_string=short_path;; 

   let join hs ending=
    let (s,dir)=Located_module.unveil hs in
    MLX(ending,s,dir);;

  let decompose (MLX(edg,s,dir))=
  (Located_module.of_string_and_root s dir,edg);;

  let root (MLX(_,_,dir))=dir;;

  let to_path mlx=
      let (hm,edg)=decompose mlx in
      let dir=root mlx in
      let s_hm=Located_module.uprooted_version hm 
      and s_dir=Dfa_root.connectable_to_subpath dir in
      Absolute_path.of_string( s_dir^s_hm^(Dfa_ending.connectable_to_modulename edg) );; 

  let half_dressed_core mlx=fst(decompose mlx);;
  let ending mlx=snd(decompose mlx);;

  end;;


  let find_needed_data_for_file cs fn=
      let temp1=Look_for_module_names.names_in_ml_file fn in
      Small_array.indices_of_property_in 
      (fun nm->List.mem nm temp1)
      (Coma_state.modules cs);; 

  let find_needed_data cs mlx=
      let fn=Mlx_path.to_path mlx in
      find_needed_data_for_file cs fn;;      

   module Ocaml_target = struct
   
   type t=
     NO_DEPENDENCIES of Mlx_path.t
    |ML_FROM_MLL of Located_module.t
    |ML_FROM_MLY of Located_module.t 
    |CMI of Located_module.t
    |CMO of Located_module.t
    |DCMO of Located_module.t
    |CMA of Located_module.t
    |CMX of Located_module.t
    |EXECUTABLE of Located_module.t
    |DEBUGGABLE of Located_module.t;;


  let to_string =function
     NO_DEPENDENCIES(mlx)->Mlx_path.to_string mlx
    |ML_FROM_MLL(hm)->(Located_module.uprooted_version hm)^".ml"
    |ML_FROM_MLY(hm)->(Located_module.uprooted_version hm)^".ml" 
    |CMI(hm)->(Located_module.uprooted_version hm)^".cmi"
    |CMO(hm)->(Located_module.uprooted_version hm)^".cmo"
    |DCMO(hm)->(Located_module.uprooted_version hm)^".d.cmo"
    |CMA(hm)->(Located_module.uprooted_version hm)^".cma"
    |CMX(hm)->(Located_module.uprooted_version hm)^".cmx"
    |EXECUTABLE(hm)->(Located_module.uprooted_version hm)^".caml_executable"
    |DEBUGGABLE(hm)->(Located_module.uprooted_version hm)^".caml_debuggable";;

  let to_shortened_string =function
    NO_DEPENDENCIES(mlx)->
   (*
     we do not shorten those because the makefile will
     see them as file targets, and will need to know
     their precise location in order to know if they are
     up-to-date or not.
   *)
    Mlx_path.to_string mlx
   |ML_FROM_MLL(hm)->(Located_module.to_shortened_string hm)^".ml"
   |ML_FROM_MLY(hm)->(Located_module.to_shortened_string hm)^".ml" 
   |CMI(hm)->(Located_module.to_shortened_string hm)^".cmi"
   |CMO(hm)->(Located_module.to_shortened_string hm)^".cmo"
   |DCMO(hm)->(Located_module.to_shortened_string hm)^".d.cmo"
   |CMA(hm)->(Located_module.to_shortened_string hm)^".cma"
   |CMX(hm)->(Located_module.to_shortened_string hm)^".cmx"
   |EXECUTABLE(hm)->(Located_module.to_shortened_string hm)^".caml_executable"
   |DEBUGGABLE(hm)->(Located_module.to_shortened_string hm)^".caml_debuggable";;

    let no_dependencies mlx=NO_DEPENDENCIES(mlx);;
    let ml_from_mll hm=ML_FROM_MLL(hm);; 
    let ml_from_mly hm=ML_FROM_MLY(hm);;
    let cmi hm=CMI(hm);;
    let cmo hm=CMO(hm);;
    let dcmo hm=DCMO(hm);;
    let cma hm=CMA(hm);; 
    let cmx hm=CMX(hm);;
    let executable hm=EXECUTABLE(hm);; 
    let debuggable hm=DEBUGGABLE(hm);; 



  end;;
 
  module Debbie=struct

  let debuggable_targets_from_ancestor_data pr_end hm=
    match Dfa_ending.restrict_to_ocaml_ending pr_end with
     Dfa_ocaml_ending_t.Mll-> 
        let mll_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.mll) in
             [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
    |Mly-> 
        let mly_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.mly) in
        [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
    |Ml-> 
             let ml_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.ml) in
             [ml_target;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
    |Mli-> 
             let mli_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.mli) in
             [mli_target;Ocaml_target.cmi hm];;    
    
    let immediate_ingredients_for_debuggable hm=
        [Ocaml_target.dcmo hm;Ocaml_target.debuggable hm];;  
    
 

    let debuggable_targets_from_ancestors cs ancestors=
    let temp1=Image.image (fun hm2->
           let idx2=Coma_state.find_module_index cs hm2 in
           let pr_end2=Coma_state.principal_ending_at_idx cs idx2 
           and hm2=lm_at_idx cs idx2 in
           debuggable_targets_from_ancestor_data pr_end2 hm2
         ) ancestors in
    Preserve_initial_ordering.preserve_initial_ordering temp1;;

    let ingredients_for_debuggable cs hm=
      let mlfile=Mlx_path.join hm Dfa_ending.ml in
      let genealogy=find_needed_data cs mlfile in
      let dirfath=Image.image (Coma_state.module_at_idx cs) genealogy in
      let temp1=Image.image 
             (fun idx->
             Tidel.diforchan(Coma_state.ancestors_at_idx cs idx) 
             ) 
             genealogy in
       let temp2=Tidel.big_teuzin ((Tidel.diforchan(dirfath) )::temp1) in
       let temp3=Small_array.indices_of_property_in (
            fun nm->Tidel.elfenn nm temp2
       ) (Coma_state.modules cs) in
       let allanc=Image.image (Coma_state.module_at_idx cs) temp3 in
      (debuggable_targets_from_ancestors cs allanc)
      @(immediate_ingredients_for_debuggable hm);; 

   end;;   

  module Ingredients_for_ocaml_target=struct

  exception Unregistered_cmo  of Located_module.t;;
  exception Unregistered_dcmo of Located_module.t;;
  exception Unregistered_cmi  of Located_module.t;;
  exception Unregistered_cma  of Located_module.t;;
  exception Unregistered_cmx  of Located_module.t;;
  exception Unregistered_ml_from_mll of Located_module.t;;
  exception Unregistered_ml_from_mly of Located_module.t;;
  exception Unregistered_executable of Located_module.t;;
  exception Unregistered_debuggable of Located_module.t;;
  exception Unregistered_module of (Located_module.t);;
  exception NonMarkedIngredientsForToplevel of string;;
  
  
  let targets_from_ancestor_data cs idx=
    let hm=lm_at_idx cs idx in
    if Coma_state.check_ending_in_at_idx Dfa_ending.mll cs idx
    then let mll_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
    else 
    if Coma_state.check_ending_in_at_idx Dfa_ending.mly cs idx
    then let mly_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
    else
    if Coma_state.check_ending_in_at_idx Dfa_ending.ml cs idx
    then 
         let ml_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.ml) in
         [ml_target;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
    else let mli_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.mli) in
         [mli_target;Ocaml_target.cmi hm];;  
  
  let targets_from_ancestors cs idx=
       let ancestors=Coma_state.ancestors_at_idx cs idx in
       let temp1=Image.image (fun nm2->
              let idx2=Coma_state.find_module_index cs nm2 in
              targets_from_ancestor_data cs idx2
            ) ancestors in
       Preserve_initial_ordering.preserve_initial_ordering temp1;;
  
  let optimized_targets_from_ancestor_data cs idx=
    let hm=lm_at_idx cs idx in
    if Coma_state.check_ending_in_at_idx Dfa_ending.mll cs idx
    then let mll_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
    else 
    if Coma_state.check_ending_in_at_idx Dfa_ending.mly cs idx
    then let mly_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
    else
    if Coma_state.check_ending_in_at_idx Dfa_ending.ml cs idx
    then 
         let ml_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.ml) in
         [ml_target;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
    else let mli_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.mli) in
         [mli_target;Ocaml_target.cmi hm];;  
  
  let optimized_targets_from_ancestors cs idx=
       let ancestors=Coma_state.ancestors_at_idx cs idx in
       let temp1=Image.image (fun nm2->
            let idx2=Coma_state.find_module_index cs nm2 in
            optimized_targets_from_ancestor_data cs idx2
            ) ancestors in
       Preserve_initial_ordering.preserve_initial_ordering temp1;;
  
  let immediate_ingredients_for_ml_from_mll hm=
    let mll_target=Ocaml_target.no_dependencies
       (Mlx_path.join hm Dfa_ending.mll) in
    [mll_target];;
  
  let immediate_ingredients_for_ml_from_mly hm=
    let mly_target=Ocaml_target.no_dependencies
      (Mlx_path.join hm Dfa_ending.mly) in
    [mly_target];;
  
  let immediate_ingredients_for_cmi cs idx hm=
      if Coma_state.check_ending_in_at_idx Dfa_ending.mll cs idx
      then let mll_target=Ocaml_target.no_dependencies
             (Mlx_path.join hm Dfa_ending.mll) in
           [mll_target;Ocaml_target.ml_from_mll hm]
      else 
      if Coma_state.check_ending_in_at_idx Dfa_ending.mly cs idx
      then let mly_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.mly) in
           [mly_target;Ocaml_target.ml_from_mly hm]
      else
    if Coma_state.check_ending_in_at_idx Dfa_ending.mli cs idx
    then let mli_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.mli) in
         [mli_target]
    else let ml_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.ml) in
         [ml_target];; 
  
  let immediate_ingredients_for_cmo cs idx hm=
      if Coma_state.check_ending_in_at_idx Dfa_ending.mll cs idx
      then let mll_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.mll) in
           [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm]
      else 
      if Coma_state.check_ending_in_at_idx Dfa_ending.mly cs idx
      then let mly_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.mly) in
           [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm]
      else
    if Coma_state.check_ending_in_at_idx Dfa_ending.ml cs idx
    then let ml_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.ml) in
         [ml_target;Ocaml_target.cmi hm]
    else let mli_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.mli) in
         [mli_target;Ocaml_target.cmi hm];;  
  
  
  let immediate_ingredients_for_dcmo=immediate_ingredients_for_cmo;;
  
  let immediate_ingredients_for_cma=immediate_ingredients_for_cmo;;
  
  let immediate_ingredients_for_cmx cs idx hm=
      if Coma_state.check_ending_in_at_idx Dfa_ending.mll cs idx
      then let mll_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.mll) in
           [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm]
      else 
      if Coma_state.check_ending_in_at_idx Dfa_ending.mly cs idx
      then let mly_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.mly) in
           [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm]
      else
    if Coma_state.check_ending_in_at_idx Dfa_ending.ml cs idx
    then let ml_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.ml) in
         [ml_target;Ocaml_target.cmi hm]
    else let mli_target=Ocaml_target.no_dependencies(Mlx_path.join hm Dfa_ending.mli) in
         [mli_target;Ocaml_target.cmi hm];;  
  
  
  let immediate_ingredients_for_executable hm=
   [Ocaml_target.cmx hm;Ocaml_target.executable hm];;  
  
  
  let ingredients_for_nodep mlx=[];;
  
  let ingredients_for_ml_from_mll cs hm=
    let nm=Located_module.naked_module hm in
    let opt_idx=Coma_state.seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_ml_from_mll(hm)) else 
    let idx=Option.unpack opt_idx in
    (targets_from_ancestors cs idx)@(immediate_ingredients_for_ml_from_mll hm);;
  
  let ingredients_for_ml_from_mly cs hm=
      let nm=Located_module.naked_module hm in
      let opt_idx=Coma_state.seek_module_index cs nm in
      if opt_idx=None then raise(Unregistered_ml_from_mly(hm)) else 
      let idx=Option.unpack opt_idx in
      (targets_from_ancestors cs idx)@(immediate_ingredients_for_ml_from_mly hm);;
  
  
  let ingredients_for_cmi cs hm=
    let nm=Located_module.naked_module hm in
    let opt_idx=Coma_state.seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_cmi(hm)) else 
    let idx=Option.unpack opt_idx in
    (targets_from_ancestors cs idx)@(immediate_ingredients_for_cmi cs idx hm);;
  
  let ingredients_for_cmo cs hm=
      let nm=Located_module.naked_module hm in
      let opt_idx=Coma_state.seek_module_index cs nm in
      if opt_idx=None then raise(Unregistered_cmo(hm)) else 
      let idx=Option.unpack opt_idx in
      (targets_from_ancestors cs idx)@
      (immediate_ingredients_for_cmo cs idx hm);;
  
  let ingredients_for_dcmo cs hm=
    let nm=Located_module.naked_module hm in
    let opt_idx=Coma_state.seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_dcmo(hm)) else 
    let idx=Option.unpack opt_idx in
    let ancestors=Coma_state.ancestors_at_idx cs idx  in
    (Debbie.debuggable_targets_from_ancestors cs ancestors)@
    (immediate_ingredients_for_dcmo cs idx hm);;
  
  let ingredients_for_cma cs hm=
      let nm=Located_module.naked_module hm in
      let opt_idx=Coma_state.seek_module_index cs nm in
      if opt_idx=None then raise(Unregistered_cma(hm)) else 
      let idx=Option.unpack opt_idx in
      (targets_from_ancestors cs idx)@
      (immediate_ingredients_for_cma cs idx hm);;
  
  let ingredients_for_cmx cs hm=
      let nm=Located_module.naked_module hm in
      let opt_idx=Coma_state.seek_module_index cs nm in
      if opt_idx=None then raise(Unregistered_cma(hm)) else 
      let idx=Option.unpack opt_idx in
      (optimized_targets_from_ancestors cs idx)@
      (immediate_ingredients_for_cmx cs idx hm);;    
  
  let ingredients_for_executable cs hm=
      let nm=Located_module.naked_module hm in
      let opt_idx=Coma_state.seek_module_index cs nm in
      if opt_idx=None then raise(Unregistered_executable(hm)) else 
      let idx=Option.unpack opt_idx in
      (optimized_targets_from_ancestors cs idx)@
      (immediate_ingredients_for_executable  hm);;   
  
  
  let ingredients_for_usual_element cs hm=
    let nm=Located_module.naked_module hm in
    let opt_idx=Coma_state.seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_executable(hm)) else 
    let idx=Option.unpack opt_idx in
    let mli_reg=Coma_state.check_ending_in_at_idx Dfa_ending.mli cs idx
    and ml_reg=Coma_state.check_ending_in_at_idx Dfa_ending.mli cs idx in
    if mli_reg&&(not ml_reg)
    then (ingredients_for_cmi cs hm)@[Ocaml_target.cmi hm]
    else (ingredients_for_cmo cs hm)@[Ocaml_target.cmo hm];;  


  let ingredients_for_ocaml_target cs=function
    Ocaml_target.NO_DEPENDENCIES(mlx)->[]
   |Ocaml_target.ML_FROM_MLL(hm)->ingredients_for_ml_from_mll cs hm
   |Ocaml_target.ML_FROM_MLY(hm)->ingredients_for_ml_from_mly cs hm
   |Ocaml_target.CMI(hm)->ingredients_for_cmi cs hm
   |Ocaml_target.CMO(hm)->ingredients_for_cmo cs hm
   |Ocaml_target.DCMO(hm)->ingredients_for_dcmo cs hm
   |Ocaml_target.CMA(hm)->ingredients_for_cma cs hm
   |Ocaml_target.CMX(hm)->ingredients_for_cmx cs hm
   |Ocaml_target.EXECUTABLE(hm)->ingredients_for_executable cs hm
   |Ocaml_target.DEBUGGABLE(hm)->Debbie.ingredients_for_debuggable cs hm;;      
  
  
  
  let module_dependency_for_nodep mlx=false;;
  let module_dependency_for_ml_from_mll cs l_hm hm1=
         if List.mem hm1 l_hm
         then true
         else  
         let nm1=Located_module.naked_module hm1 in
         let idx1=Coma_state.find_module_index cs nm1 in
         let anc1=Coma_state.ancestors_at_idx cs idx1 in
         List.exists 
          (fun z->List.mem (Located_module.naked_module z) anc1 ) 
          l_hm;;
  
  
  let module_dependency_for_ml_from_mly=module_dependency_for_ml_from_mll;; 
  let module_dependency_for_cmi=module_dependency_for_ml_from_mll;;
  let module_dependency_for_cmo=module_dependency_for_ml_from_mll;;
  let module_dependency_for_dcmo=module_dependency_for_ml_from_mll;;
  let module_dependency_for_cma=module_dependency_for_ml_from_mll;;                 
  let module_dependency_for_cmx=module_dependency_for_ml_from_mll;;  
  let module_dependency_for_executable=module_dependency_for_ml_from_mll;;  
  let module_dependency_for_debuggable=module_dependency_for_ml_from_mll;;  
  let module_dependency_for_toplevel cs l_hm name l_hm2=
    List.exists(fun hm2->
    (module_dependency_for_cmo cs l_hm hm2)||(List.mem hm2 l_hm)
    ) l_hm2;;
  
  
  let module_dependency_for_ocaml_target cs l_hm =function
    Ocaml_target.NO_DEPENDENCIES(mlx)->false
   |Ocaml_target.ML_FROM_MLL(hm)->module_dependency_for_ml_from_mll cs l_hm hm
   |Ocaml_target.ML_FROM_MLY(hm)->module_dependency_for_ml_from_mly cs l_hm hm
   |Ocaml_target.CMI(hm)->module_dependency_for_cmi cs l_hm hm
   |Ocaml_target.CMO(hm)->module_dependency_for_cmo cs l_hm hm
   |Ocaml_target.DCMO(hm)->module_dependency_for_dcmo cs l_hm hm
   |Ocaml_target.CMA(hm)->module_dependency_for_cma cs l_hm hm
   |Ocaml_target.CMX(hm)->module_dependency_for_cmx cs l_hm hm
   |Ocaml_target.EXECUTABLE(hm)->module_dependency_for_executable cs l_hm hm
   |Ocaml_target.DEBUGGABLE(hm)->module_dependency_for_debuggable cs l_hm hm;;
  
         
  
  
  
  let mlx_dependency_for_ocaml_target cs mlx tgt=
    let hm=Mlx_path.half_dressed_core mlx in
    module_dependency_for_ocaml_target cs [hm] tgt;;
  
  let mlx_list_dependency_for_ocaml_target cs l_mlx tgt=
   List.exists (fun mlx->mlx_dependency_for_ocaml_target cs mlx tgt) l_mlx;;
  
  

end;;  

module Command_for_ocaml_target=struct

  let ocamlc="ocamlc  -bin-annot ";;
  let ocamlopt="ocamlopt  -bin-annot ";;
  let cee=" -c ";;
  
  exception Command_called_on_nodep of Mlx_path.t;;
  exception Unregistered_cmo  of Located_module.t;;
  exception Unregistered_dcmo of Located_module.t;;
  exception Unregistered_cmi  of Located_module.t;;
  exception Unregistered_cma  of Located_module.t;;
  exception Unregistered_cmx  of Located_module.t;;
  exception Unregistered_ml_from_mll of Located_module.t;;
  exception Unregistered_ml_from_mly of Located_module.t;;
  exception Unregistered_executable of Located_module.t;;
  exception Unregistered_debuggable of Located_module.t;;
  exception Unregistered_modules_in_toplevel of string*(Located_module.t list);;  
  
  let ingr=Ingredients_for_ocaml_target.ingredients_for_ocaml_target;;
  
  let cmx_manager=function
   Ocaml_target.CMX(hm2)->
      let s_hm2=Located_module.to_shortened_string hm2 in
      Some((Dfa_subdirectory.connectable_to_subpath Coma_constant.exec_build_subdir)^s_hm2^".cmx")
   |_->None;;
  
  let dcmo_manager=function
   Ocaml_target.DCMO(hm2)->
      let s_hm2=Located_module.to_shortened_string hm2 in
      Some((Dfa_subdirectory.connectable_to_subpath Coma_constant.debug_build_subdir)^s_hm2^".d.cmo")
   |_->None;;
  
  let command_for_nodep mlx=[];;
  
  let command_for_ml_from_mll dir hm=
            let s_hm=Located_module.uprooted_version hm in
            let s_root=Dfa_root.connectable_to_subpath dir in
            let s_fhm=s_root^s_hm in
            [
              "ocamllex  -o "^s_fhm^".ml "^s_fhm^".mll";
            ];;
   
  let command_for_ml_from_mly dir hm=
            let s_hm=Located_module.uprooted_version hm in
            let s_root=Dfa_root.connectable_to_subpath dir in
            let s_fhm=s_root^s_hm in
            [
              "ocamlyacc "^s_fhm^".mly"
            ];;  

let command_for_cmi dir cs hm=
    let nm=Located_module.naked_module hm in
    let opt_idx=Coma_state.seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_cmi(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_root=Dfa_root.connectable_to_subpath(dir) in
    let s_hm=Located_module.uprooted_version hm in
    let s_fhm=s_root^s_hm in
    let mli_reg=Coma_state.check_ending_in_at_idx Dfa_ending.mli cs idx in
    let ending=(if mli_reg then ".mli" else ".ml") in
    let central_cmd=
        "ocamlc  -bin-annot "^
        (Coma_state.needed_dirs_and_libs_in_command Compilation_mode_t.Usual cs idx)^
            " -c "^s_fhm^ending in
            let full_mli=s_root^s_hm^".mli" in
            if (not mli_reg)
               &&(Sys.file_exists(full_mli))
            then (* 
                   in this situation the mli file exists but is not registered.
                   So the modulesystem manager must treat it as though it didn't
                   exist. We temporarily rename it so that ocamlc will ignore it.
                  *)
                  let dummy_mli=s_root^"uvueaoqhkt" in
                  [
                   "mv "^full_mli^" "^dummy_mli;
                   central_cmd;
                   "mv "^s_fhm^".cm* "^s_root^(Dfa_subdirectory.connectable_to_subpath Coma_constant.build_subdir);
                   "mv "^dummy_mli^" "^full_mli
                  ] 
            else  [
                     central_cmd;
                     "mv "^s_fhm^".cm* "^s_root^(Dfa_subdirectory.connectable_to_subpath Coma_constant.build_subdir)
                   ];;
  
  let command_for_cmo dir cs hm=
    let nm=Located_module.naked_module hm in
    let opt_idx=Coma_state.seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_cmo(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_hm=Located_module.uprooted_version hm in
    let s_root=Dfa_root.connectable_to_subpath(dir) in
    let s_fhm=s_root^s_hm in
    let dirs_and_libs=Coma_state.needed_dirs_and_libs_in_command Compilation_mode_t.Usual cs idx in
    let mli_reg=Coma_state.check_ending_in_at_idx Dfa_ending.mli cs idx in 
    let full_mli=s_fhm^".mli" in
    let central_cmds=
    [ 
      "ocamlc -bin-annot "^dirs_and_libs^" -o "^s_fhm^".cmo -c "^s_fhm^".ml";
      "mv "^s_fhm^".cm* "^s_root^(Dfa_subdirectory.connectable_to_subpath Coma_constant.build_subdir)
    ] in 
    if (not mli_reg) &&(Sys.file_exists(full_mli))
    then 
          (* 
                   in this situation the mli file exists but is not registered.
                   So the modulesystem manager must treat it as though it didn't
                   exist. We temporarily rename it so that ocamlc will ignore it.
          *)
                  let dummy_mli=s_root^"uvueaoqhkt" in
                  [
                   "mv "^full_mli^" "^dummy_mli
                  ]
                  @ 
                   central_cmds
                  @ 
                  [ 
                   "mv "^dummy_mli^" "^full_mli
                  ] 
    else central_cmds;; 
 
 
  let command_for_dcmo dir cs hm=
    let nm=Located_module.naked_module hm in
    let opt_idx=Coma_state.seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_cmo(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_hm=Located_module.uprooted_version hm in
    let s_root=Dfa_root.connectable_to_subpath(dir) in
    let s_fhm=s_root^s_hm in
    let dirs_and_libs=Coma_state.needed_dirs_and_libs_in_command Compilation_mode_t.Debug cs idx in
    [ 
      "ocamlc -g "^dirs_and_libs^" -o "^s_fhm^".d.cmo -c "^s_fhm^".ml";
      "mv "^s_fhm^".d.cm* "^s_root^
       (Dfa_subdirectory.connectable_to_subpath Coma_constant.debug_build_subdir)
    ];;
  
  let command_for_cma dir cs hm=
      let nm=Located_module.naked_module hm in
      let opt_idx=Coma_state.seek_module_index cs nm in
      if opt_idx=None then raise(Unregistered_cma(hm)) else 
      let idx=Option.unpack opt_idx in
      let s_hm=Located_module.uprooted_version hm in
      let s_root=Dfa_root.connectable_to_subpath(dir) in
      let s_fhm=s_root^s_hm in
      let dirs_and_libs=Coma_state.needed_dirs_and_libs_in_command Compilation_mode_t.Executable cs idx in
      [ 
        "ocamlopt -bin-annot -a "^dirs_and_libs^" -o "^s_fhm^".cma -c "^s_fhm^".ml";
        "mv "^s_fhm^".cm* "^s_root^(Dfa_subdirectory.connectable_to_subpath Coma_constant.build_subdir)
      ];;  
  
  let command_for_cmx dir cs hm=
      let nm=Located_module.naked_module hm in
      let opt_idx=Coma_state.seek_module_index cs nm in
      if opt_idx=None then raise(Unregistered_cmx(hm)) else 
      let idx=Option.unpack opt_idx in
      let s_hm=Located_module.uprooted_version hm in
      let s_root=Dfa_root.connectable_to_subpath(dir) in
      let s_fhm=s_root^s_hm in
      let dirs_and_libs=Coma_state.needed_dirs_and_libs_in_command Compilation_mode_t.Executable cs idx in
      let dir_for_building=
        (Dfa_subdirectory.connectable_to_subpath Coma_constant.exec_build_subdir) in
      [ 
        "ocamlopt "^dirs_and_libs^" -o "^s_fhm^".cmx -c "^s_fhm^".ml";
        "mv "^s_fhm^".cm* "^s_root^dir_for_building;
        "mv "^s_fhm^".o "^s_root^dir_for_building
      ];;      
            
  let command_for_executable dir cs hm=
    let nm=Located_module.naked_module hm in
    let opt_idx=Coma_state.seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_executable(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_hm=Located_module.uprooted_version hm in
    let s_root=Dfa_root.connectable_to_subpath(dir) in
    let s_fhm=s_root^s_hm in
    let temp1=ingr cs (Ocaml_target.EXECUTABLE(hm)) in
    let temp2=Option.filter_and_unpack cmx_manager temp1 in
    let long_temp2=Image.image (fun t->s_root^t) temp2 in
    let dirs_and_libs=Coma_state.needed_dirs_and_libs_in_command Compilation_mode_t.Executable  cs idx  in
    let dir_for_building=
        (Dfa_subdirectory.connectable_to_subpath Coma_constant.exec_build_subdir) in
    [ 
      "ocamlopt "^dirs_and_libs^" -o "^s_fhm^".ocaml_executable "^
        (String.concat " " long_temp2);
      "mv "^s_fhm^".ocaml_executable "^s_root^dir_for_building
    ];;
            
  let command_for_debuggable dir cs hm=
    let nm=Located_module.naked_module hm in
    let opt_idx=Coma_state.seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_debuggable(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_hm=Located_module.uprooted_version hm in
    let s_root=Dfa_root.connectable_to_subpath(dir) in
    let s_fhm=s_root^s_hm in
    let temp1=ingr cs (Ocaml_target.DEBUGGABLE(hm)) in
    let temp2=Option.filter_and_unpack dcmo_manager temp1 in
    let long_temp2=Image.image (fun t->s_root^t) temp2 in
    let dirs_and_libs=Coma_state.needed_dirs_and_libs_in_command Compilation_mode_t.Debug  cs idx in
    let dir_for_building=
      (Dfa_subdirectory.connectable_to_subpath Coma_constant.debug_build_subdir) in
    [ 
      "ocamlc -g "^dirs_and_libs^" -o "^s_fhm^".ocaml_debuggable "^
        (String.concat " " long_temp2);
      "mv "^s_fhm^".ocaml_debuggable "^s_root^dir_for_building
    ];;          
    
  

  let command_for_ocaml_target dir cs tgt=
     match tgt with
    Ocaml_target.NO_DEPENDENCIES(mlx)->command_for_nodep mlx 
   |Ocaml_target.ML_FROM_MLL(hm)->command_for_ml_from_mll dir hm
   |Ocaml_target.ML_FROM_MLY(hm)->command_for_ml_from_mly dir hm
   |Ocaml_target.CMI(hm)->command_for_cmi  dir cs hm
   |Ocaml_target.CMO(hm)->command_for_cmo dir cs hm
   |Ocaml_target.DCMO(hm)->command_for_dcmo dir cs hm
   |Ocaml_target.CMA(hm)->command_for_cma dir cs hm
   |Ocaml_target.CMX(hm)->command_for_cmx dir cs hm
   |Ocaml_target.EXECUTABLE(hm)->command_for_executable dir cs hm
   |Ocaml_target.DEBUGGABLE(hm)->command_for_debuggable dir cs hm;;

  
end;;

   let target_at_idx cs idx=
    let hm=lm_at_idx cs idx 
    and mlp =Coma_state.check_ending_in_at_idx Dfa_ending.ml cs idx
    and mlip=Coma_state.check_ending_in_at_idx Dfa_ending.mli cs idx
    and mllp=Coma_state.check_ending_in_at_idx Dfa_ending.mll cs idx
    and mlyp=Coma_state.check_ending_in_at_idx Dfa_ending.mly cs idx in
    let temp1=[
                mllp,Ocaml_target.ml_from_mll hm;
                mlyp,Ocaml_target.ml_from_mly hm;
           mlp||mlip,Ocaml_target.cmi hm;
           mlp||mlip,Ocaml_target.cmo hm;
           mlp||mlip,Ocaml_target.cma hm;
           mlp||mlip,Ocaml_target.cmx hm;
                 mlp,Ocaml_target.executable hm;
    ] in
    Option.filter_and_unpack 
      (fun x->if fst x 
              then Some(snd x) 
              else None) temp1;;  


let usual_targets cs=
  let n=Small_array.size((Coma_state.modules cs)) in
  let temp1=Option.filter_and_unpack 
   (fun idx->
      if Coma_state.product_up_to_date_at_idx cs idx 
      then Some(target_at_idx cs idx)
      else None) (Ennig.ennig 1 n) in
  List.flatten temp1;;


  let slice_targets tgts=
    let temp1=Sliced_string.make_aggregates_if_possible 
                     (Separator.of_string " ") 
                     (Image.image Ocaml_target.to_string tgts) in
    Sliced_string.to_string_list temp1;;
  
  let slice_shortened_targets tgts=
    let temp1=Sliced_string.make_aggregates_if_possible 
                     (Separator.of_string " ") 
                     (Image.image Ocaml_target.to_shortened_string tgts) in
    Sliced_string.to_string_list temp1;;
  
  let write_usual_makefile_element cs tgt=
   let main_root = Coma_state.root cs in 
   let ingrs=Ingredients_for_ocaml_target.ingredients_for_ocaml_target 
      cs tgt in
   let sliced_ingrs=slice_shortened_targets ingrs in
   let cmds=Command_for_ocaml_target.command_for_ocaml_target 
                         main_root cs tgt in
   let s1=(Ocaml_target.to_shortened_string tgt)^" : " 
   and s2=String.concat " \\\n" sliced_ingrs
   and s3="\n\t"
   and s4=String.concat "\n\t" cmds in
   String.concat "" [s1;s2;s3;s4];;

   
   
  let write_full_compilation_makefile_element  cs=
    let main_root=Coma_state.root cs in
    let l=all_located_modules cs in
    let temp1=Image.image 
    (Ingredients_for_ocaml_target.ingredients_for_usual_element cs) l in
    let ingrs=Preserve_initial_ordering.preserve_initial_ordering temp1 in
    let sliced_ingrs=slice_shortened_targets ingrs in
    let l_idx=Image.image (fun hm->
      let nm=Located_module.naked_module hm in
      Coma_state.find_module_index cs nm) l  in
    let s_root=Dfa_root.connectable_to_subpath(main_root) in
    let long_temp4=Image.image (fun idx->
               let hm=lm_at_idx cs idx in
               let s_hm=(Located_module.uprooted_version hm) in
               let short_s_hm=Cull_string.after_rightmost s_hm '/' in
               let ml_reg=Coma_state.check_ending_in_at_idx Dfa_ending.ml cs idx in
               if ml_reg
               then s_root^"_build/"^short_s_hm^".cmo"
               else " "
    ) l_idx in   
    let long_s_lhm=String.concat " " long_temp4 in
    let dirs_and_libs=Coma_state.needed_dirs_and_libs_for_several Compilation_mode_t.Usual cs l_idx in
    let cmds=[ "ocamlmktop "^dirs_and_libs^" -o "^s_root^"ecaml "^long_s_lhm^" ";
            "mv "^s_root^"ecaml "^s_root^"_build/"] in
    let s1="ecaml : " 
    and s2=String.concat " \\\n" sliced_ingrs
    and s3="\n\t"
    and s4=String.concat "\n\t" cmds in
    String.concat "" [s1;s2;s3;s4];; 
  
    let write_makefile cs=
      let temp1=usual_targets cs in
      let temp2=Image.image (write_usual_makefile_element cs) temp1 in
      let temp3=temp2@[write_full_compilation_makefile_element cs] in
      let temp5=slice_targets  temp1 in
      let temp6=String.concat " \\\n" temp5 in
      let temp7="clean :\n\trm -r -f "^temp6^"\n\n" in
      String.concat "\n\n" (temp3@[temp7]);;
  
let force_join (Dfa_root_t.R s) w=
      let t=s^"/"^w in
      if Sys.file_exists t
      then t
      else let _=Unix_command.uc("touch "^t) in
           t;;

let save_makefile  cs=
      let s1="# This makefile was automatocally written by\n"^
      "# the write_makefile function in the ml_manager module. \n\n"^
      (write_makefile cs) in
      let lm=force_join (Coma_state.root cs) Coma_constant.bare_name_for_makefile in
      Io.overwrite_with (Absolute_path.of_string lm) s1;;     

end;;


let save_makefile=Private.save_makefile;;

