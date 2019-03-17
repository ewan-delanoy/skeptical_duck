(*

#use"Country/Germany/german_pervasives.ml";;

*)


let cdir=Coma_big_constant.this_world;;

let s_cdir=Root_directory.connectable_to_subpath cdir;;
let current_registered_directories ()=German_wrapper.directories();;

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

let fl=German_vague_string.to_path cdir;; 

let fmr x=
  let uncapitalized_x=
    Naked_module.of_string(String.uncapitalize_ascii x) in
  Coma_state.seek_module_index
  (Usual_coma_state.main_ref) uncapitalized_x;;

exception No_module_with_name of string;;

let hmx x=
   match fmr x
   with 
   Some(idx)->Coma_state.hm_at_idx (Usual_coma_state.main_ref) idx
   |None->raise(No_module_with_name(x));;  

let nmx x=Half_dressed_module.naked_module (hmx x);;

let abo x=
  let wmdata=Usual_coma_state.main_ref in
  Image.image (fun nm->
   Half_dressed_module.uprooted_version(
    Coma_state.hm_from_nm wmdata nm
   )) 
  (Coma_state.above wmdata (hmx x));;

let bel x=
  let wmdata=Usual_coma_state.main_ref in
  Image.image (fun nm->
   Half_dressed_module.uprooted_version(
    Coma_state.hm_from_nm wmdata nm
   )) 
  (Coma_state.below wmdata (hmx x));;
  
let dbel x=
  let wmdata=Usual_coma_state.main_ref in
  Image.image (fun nm->
   Half_dressed_module.uprooted_version(
    Coma_state.hm_from_nm wmdata nm
   )) 
  (Coma_state.directly_below wmdata (hmx x));;


let ren_without_backup x y=German_wrapper.rename_module (hmx x) (No_slashes.of_string y);;
let relo_without_backup x y=German_wrapper.relocate_module (hmx x) y;;

let fg_without_backup x=
   if String.contains x '.'
   then German_wrapper.forget_file (fl x)
   else let _=German_wrapper.forget_module (hmx x) in ();;

let regi_without_backup x= 
  let path=Absolute_path.of_string(Root_directory.join cdir x) in
  let mlx=Mlx_ended_absolute_path.of_path_and_root path cdir in
  Usual_coma_state.register_mlx_file mlx;;



let ureg_without_backup x=
  if List.exists (fun edg->Supstring.ends_with x edg) [".ml";".mli";".mll";".mly"] 
  then let path=Absolute_path.of_string(Root_directory.join cdir x) in
       let mlx=Mlx_ended_absolute_path.of_path_and_root path cdir in
       German_wrapper.unregister_mlx_file mlx 
  else German_wrapper.unregister_module (hmx x);;

let double_semicolon=";"^";";;

let cf old_t1 old_t2=
   let t1=String.uncapitalize_ascii old_t1
   and t2=String.uncapitalize_ascii old_t2 in 
   let ap1=fl t1 in
   let s_ap1=Absolute_path.to_string ap1 in
   let s_ap2=(Father_and_son.invasive_father s_ap1 '/')^"/"^t2^".ml" in
   let _=Unix_command.uc ("cp "^s_ap1^" "^s_ap2) in
   let ap2=Absolute_path.of_string s_ap2 in
   let s1=Cull_string.cobeginning (String.length s_cdir) s_ap1
   and s2=Cull_string.cobeginning (String.length s_cdir) s_ap2 in
   let txt1="\""^s1^"\""^double_semicolon
   and txt2="\""^s2^"\""^double_semicolon in
   let _=Replace_inside.replace_inside_file 
    (txt1,txt2) ap2  in 
   Unix_command.uc ("open -a \"/Applications/Visual Studio Code.app\" "^s_ap2);;   

let vo s=
  let temp1=Find_suitable_ending.find_file_location cdir (current_directories()) s in
  let s1=Absolute_path.to_string temp1 in
  Unix_command.uc ("open -a \"/Applications/Visual Studio Code.app\" "^s1);;


let syz()=Coma_state.system_size (Usual_coma_state.main_ref);;

let init=German_wrapper.initialize;;


let rd ()=Alaskan_remove_debuggables.rd cdir (Usual_coma_state.main_ref);;
let sd=German_wrapper.start_debugging;;



let rv_without_backup x y=German_values_in_modules.rename_string_or_value cdir (Usual_coma_state.main_ref) x y;;
let srv_without_backup x y=German_values_in_modules.replace_string cdir (Usual_coma_state.main_ref) x y;;


let sv wal=German_values_in_modules.show_value_occurrences_in_modulesystem 
  cdir wal (Usual_coma_state.main_ref) ;;
let vfm modname =German_values_in_modules.list_values_from_module_in_modulesystem 
    modname (Usual_coma_state.main_ref) ;;
let muv x=Coma_state.modules_using_value (Usual_coma_state.main_ref) x;;

let ed =German_wrapper.end_debugging;;


let vd=German_wrapper.view_definition;;
let fvd a=Find_value_descendants.fvd 
  (Compute_all_ocaml_items.caoi(Usual_coma_state.main_ref)) a ;;

let rsh_without_backup ()=let _=German_wrapper.refresh() in ();;


let am ()=Coma_state.all_naked_modules (Usual_coma_state.main_ref);;
  
    
let tw x=
  let hm=hmx x in
  let s_hm=Half_dressed_module.uprooted_version hm in
  let fn=(Root_directory.connectable_to_subpath(cdir))^s_hm in    
  Sys.command ("open -a /Applications/TextWrangler.app "^fn^".ml");;



let ucc ()=Coma_state.Create_or_update_copied_compiler.ucc 
  Usual_coma_state.main_ref
 (Coma_big_constant.next_world,
  Coma_big_constant.dummy_backup_dir);;



let forget_file_with_backup x=
   let ap=fl x in
   let s_ap=Absolute_path.to_string ap in  
   let cut_ap=Root_directory.cut_beginning cdir s_ap in
   let diff=
    Dircopy_diff.veil
    (Recently_deleted.of_string_list [cut_ap])
    (Recently_changed.of_string_list [])
    (Recently_created.of_string_list []) in
   (
    German_wrapper.forget_file ap;
    Usual_coma_state.backup diff None;
    Usual_coma_state.save_all() 
   ) ;; 

let forget_module_with_backup x=
    let short_paths=German_wrapper.forget_module (hmx x) in
    let ordered_paths=Ordered_string.forget_order(Ordered_string.safe_set(short_paths)) in
    let diff=
      Dircopy_diff.veil
      (Recently_deleted.of_string_list ordered_paths)
      (Recently_changed.of_string_list [])
      (Recently_created.of_string_list []) in
     (
      Usual_coma_state.backup diff None; 
      Usual_coma_state.save_all() 
     );; 
 
let fg x=
      if String.contains x '.'
      then forget_file_with_backup x
      else forget_module_with_backup x;;

let rndir p=(German_wrapper.rename_directory p;Usual_coma_state.recompile None);;

let relo x y=(relo_without_backup x y;Usual_coma_state.recompile None);;
let ren  x y=(ren_without_backup  x y;Usual_coma_state.recompile None);;
let rsh ()=
  let diff=German_wrapper.refresh () in
  (
    Usual_coma_state.backup diff None;
    Usual_coma_state.save_all() 
   );;


let  cod ()=Check_ocaml_dircopy.check cdir;;


let rv x y=(rv_without_backup x y;Usual_coma_state.recompile None);;
let srv x y=(srv_without_backup x y;Usual_coma_state.recompile None);;
let ureg x=(ureg_without_backup x;Usual_coma_state.recompile None);;



           