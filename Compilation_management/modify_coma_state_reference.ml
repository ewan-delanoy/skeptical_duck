
(* 

#use"Compilation_management/modify_coma_state_reference.ml";;

*)

let forget pcs x=
  let new_cs = Modify_coma_state_and_save.forget (!pcs) x in 
  pcs:=new_cs;;


let initialize pcs =
  let new_cs = Coma_state.read_persistent_version (!pcs) in 
  pcs:=new_cs;;

let initialize_if_empty pcs =
   if Coma_state.system_size (!pcs)  = 0 
   then initialize pcs;;

let recompile pcs opt_comment=
  let new_cs = Modify_coma_state_and_save.recompile (!pcs) opt_comment in 
  pcs:=new_cs;;


let refresh pcs =
  let new_cs = Modify_coma_state_and_save.refresh (!pcs)  in 
  pcs:=new_cs;;

let register_short_path pcs short_path=
  let new_cs = Modify_coma_state_and_save.register_short_path (!pcs) short_path in 
  pcs:=new_cs;;


let relocate_module pcs old_hm_name new_subdir=
   let new_cs = Modify_coma_state_and_save.relocate_module (!pcs) old_hm_name new_subdir in 
  pcs:=new_cs;;  


let rename_directory pcs old_subdir new_subdirname=
    let new_cs = Modify_coma_state_and_save.rename_directory (!pcs) old_subdir new_subdirname in 
    pcs:=new_cs;;
    

let rename_module pcs old_name new_name=
   let new_cs = Modify_coma_state_and_save.rename_module (!pcs) old_name new_name in 
  pcs:=new_cs;;


let rename_string_or_value pcs old_name new_name=
   let new_cs = Coma_state.Almost_concrete.rename_string_or_value (!pcs) old_name new_name in 
  pcs:=new_cs;;

