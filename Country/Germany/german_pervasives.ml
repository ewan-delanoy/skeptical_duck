(*

#use"Country/Germany/german_pervasives.ml";;

*)


let cdir=Coma_big_constant.This_World.root;;

let s_cdir=Root_directory.connectable_to_subpath cdir;;
let current_registered_directories ()=Coma_state.directories(!Usual_coma_state.main_ref);;

let current_directories()=
  let temp1=List.filter (
    fun x->x<>Subdirectory_t.SD ""
  ) (current_registered_directories()) in
  (Subdirectory_t.SD "")::(temp1@
  [
    Coma_constant.automatically_generated_subdir;
    Coma_constant.left_out_of_updating;
    Coma_constant.old_and_hardly_reusable
  ]);;


let nmx x=Half_dressed_module.naked_module (Usual_coma_state.find_half_dressed_module x);;

 
  
let sd ()=
  let _=Coma_state.recompile (!(Usual_coma_state.main_ref)) in 
  let _=Coma_state.start_debugging (!(Usual_coma_state.main_ref)) in 
    Usual_coma_state.save_all();;

let muv x=Coma_state.modules_using_value (!Usual_coma_state.main_ref) x;;

let ed ()=
  let sbuild=(Root_directory.connectable_to_subpath Coma_big_constant.This_World.root)
     ^"_debug_build/" in
  let _=Unix_command.uc("rm -f "^sbuild^"*.d.cm*"^" "^
     sbuild^"*.ocaml_debuggable")  in
  ();;     


let fvd a=Find_value_descendants.fvd 
  (Compute_all_ocaml_items.caoi(!Usual_coma_state.main_ref)) a ;;


let am ()=Coma_state.all_naked_modules (!Usual_coma_state.main_ref);;
  



           