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



let ureg_without_backup x=
  if List.exists (fun edg->Supstring.ends_with x edg) [".ml";".mli";".mll";".mly"] 
  then let path=Absolute_path.of_string(Root_directory.join cdir x) in
       let mlx=Mlx_ended_absolute_path.of_path_and_root path cdir in
       German_wrapper.unregister_mlx_file mlx 
  else German_wrapper.unregister_module (Usual_coma_state.find_half_dressed_module x);;

let double_semicolon=";"^";";;



let init ()=Usual_coma_state.initialize ();;


let rd ()=Alaskan_remove_debuggables.rd cdir (Usual_coma_state.main_ref);;
let sd=German_wrapper.start_debugging;;



let rv_without_backup x y=Usual_coma_state.rename_string_or_value  x y;;
let srv_without_backup x y=Usual_coma_state.replace_string x y;;



let muv x=Coma_state.modules_using_value (!Usual_coma_state.main_ref) x;;

let ed =German_wrapper.end_debugging;;


let vd=German_wrapper.view_definition;;
let fvd a=Find_value_descendants.fvd 
  (Compute_all_ocaml_items.caoi(!Usual_coma_state.main_ref)) a ;;


let am ()=Coma_state.all_naked_modules (!Usual_coma_state.main_ref);;
  
    
let tw x=
  let hm=Usual_coma_state.find_half_dressed_module x in
  let s_hm=Half_dressed_module.uprooted_version hm in
  let fn=(Root_directory.connectable_to_subpath(cdir))^s_hm in    
  Sys.command ("open -a /Applications/TextWrangler.app "^fn^".ml");;


let rndir p=(German_wrapper.rename_directory p;Usual_coma_state.recompile None);;


let rv x y=(rv_without_backup x y;Usual_coma_state.recompile None);;
let srv x y=(srv_without_backup x y;Usual_coma_state.recompile None);;
let ureg x=(ureg_without_backup x;Usual_coma_state.recompile None);;



           