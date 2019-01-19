(*

#use"rare_values.ml";;

*)



let needed_modules_for_compilation_management ()=
  let mr=Usual_coma_state.main_ref in
  let mod1=German_pervasives.nmx "german_pervasives" in
  let mod2=German_pervasives.nmx "usual_coma_state" in
  let mod3=German_pervasives.nmx "needed_values" in
  let mods = Coma_state.above_one_in_several_or_inside mr
  [mod1;mod2;mod3] in
  let paths=Option.filter_and_unpack (fun nm->
 let idx=Coma_state.find_module_index mr nm in
 if Coma_state.check_ending_in_at_idx
     Ocaml_ending.ml mr idx 
 then let hm=Coma_state.hm_at_idx mr idx in
      let mlx=Mlx_ended_absolute_path.join
            hm Ocaml_ending.ml in
      let ap=Mlx_ended_absolute_path.to_absolute_path mlx in 
      Some(ap)
 else None
 ) mods in
 (mods,paths);;