
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

let recompile_without_githubbing pcs =
  let (new_cs,_) = Coma_state.Almost_concrete.recompile_without_githubbing (!pcs) in 
  pcs:=new_cs;;

let refresh pcs =
  let (new_cs,_) = Coma_state.Almost_concrete.local_refresh (!pcs)  in 
  pcs:=new_cs;;

let register_short_path pcs x=
  let new_cs = Coma_state.Almost_concrete.register_short_path (!pcs) x in 
  pcs:=new_cs;;

let relocate_module pcs old_name new_name=
   let new_cs = Coma_state.Almost_concrete.relocate_module (!pcs) old_name new_name in 
  pcs:=new_cs;;

let rename_directory pcs (old_subdir,new_subdirname)=
    let new_cs = Coma_state.Almost_concrete.rename_directory (!pcs) (old_subdir,new_subdirname) in 
    pcs:=new_cs;;
    
let rename_module pcs old_hm_name new_subdir=
   let new_cs = Coma_state.Almost_concrete.rename_module (!pcs) old_hm_name new_subdir in 
  pcs:=new_cs;;  

let rename_string_or_value pcs old_name new_name=
   let new_cs = Coma_state.Almost_concrete.rename_string_or_value (!pcs) old_name new_name in 
  pcs:=new_cs;;

