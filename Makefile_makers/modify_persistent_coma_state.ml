
(* 

#use"Makefile_makers/modify_persistent_coma_state.ml";;

*)

let forget_with_backup pcs x=
  let new_cs = Coma_state.Almost_concrete.forget_with_backup (!pcs) x in 
  pcs:=new_cs;;

let forget_without_backup pcs x=
  let new_cs = Coma_state.Almost_concrete.forget_with_backup (!pcs) x in 
  pcs:=new_cs;;

let initialize pcs =
  let new_cs = Coma_state.read_persistent_version (!pcs) in 
  pcs:=new_cs;;

let initialize_if_empty pcs =
   if Coma_state.system_size (!pcs)  = 0 
   then initialize pcs;;

let recompile pcs opt_comment=
  let new_cs = Coma_state.Almost_concrete.recompile (!pcs) opt_comment in 
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





