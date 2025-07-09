(*

#use"lib/Filewatching/Fw_classes/coming_soon_fwc_configuration.ml";;

*)

type t = Fwg_configuration.t ;;

module Inherited = struct
  
let set_root fw new_root =   
  Fwg_configuration.make new_root 
  (Fwg_configuration.ignored_files fw)  (Fwg_configuration.ignored_subdirectories fw) ;;

end ;;

module Crobj = struct 


  let salt = "Fw_poly_t." ;;
  let label_for_ignored_files                      = salt ^ "ignored_files" ;;
  let label_for_ignored_subdirectories             = salt ^ "ignored_subdirectories" ;;
  let label_for_root                               = salt ^ "root" ;;
  
  let of_concrete_object ccrt_obj = 
   let g=Concrete_object.get_record ccrt_obj in 
   let v_root = Dfa_root.of_concrete_object (g label_for_root) 
   and v_ignored_files = Crobj_converter_combinator.to_list Dfn_rootless.of_concrete_object (g label_for_ignored_files)
   and v_ignored_subdirectories = Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object (g label_for_ignored_subdirectories) in 
   Fwg_configuration.make v_root v_ignored_files v_ignored_subdirectories ;;
  
  let to_concrete_object fw = 
    let v_root = Fwg_configuration.root fw
    and v_ignored_files = Fwg_configuration.ignored_files fw
    and v_ignored_subdirectories = Fwg_configuration.ignored_subdirectories fw in  
    let items =  
    [
     label_for_root, Dfa_root.to_concrete_object ( v_root ) ;
     label_for_ignored_files, Crobj_converter_combinator.of_list Dfn_rootless.to_concrete_object ( v_ignored_files ) ;
     label_for_ignored_subdirectories, Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object ( v_ignored_subdirectories ) ;
    ] in 
    Concrete_object_t.Record items ;;
  


end ;;  

let ignored_files fw = Fwg_configuration.ignored_files fw ;;
let ignored_subdirectories fw = Fwg_configuration.ignored_subdirectories fw ;;

let of_root root_dir = Fwg_configuration.make root_dir [] Coma_constant.git_ignored_subdirectories ;; 


let root = Fwg_configuration.root ;;

let test_for_admissibility data rl=
  (List.mem (
    (Dfn_rootless.to_ending rl)
  ) Dfa_ending.endings_for_readable_files)
  &&
   (List.for_all (
     fun sd->not(Dfn_rootless.is_in rl sd)
  ) (Fwg_configuration.ignored_subdirectories data)
  )  
  &&
  (
    not(List.mem rl (Fwg_configuration.ignored_files data))
  )
  ;;

let test_equality fw1 fw2 = 
    let temp =
    [
      ("root",((Fwg_configuration.root fw1)=(Fwg_configuration.root fw2)));
      ("ignored_subdirectories",((Fwg_configuration.ignored_subdirectories fw1)=(Fwg_configuration.ignored_subdirectories fw2)));
      ("ignored_files",((Fwg_configuration.ignored_files fw1)=(Fwg_configuration.ignored_files fw2)));
    ] in 
    List.filter_map (fun (fld,is_ok)->if is_ok then None else Some fld) temp;;
  
