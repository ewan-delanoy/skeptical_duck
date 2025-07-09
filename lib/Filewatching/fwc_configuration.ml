(*

#use"lib/Filewatching/fwc_configuration.ml";;

*)


let ignored_files fw = fw.Fw_flattened_poly_t.ignored_files ;;
let ignored_subdirectories fw = fw.Fw_flattened_poly_t.ignored_subdirectories ;;



let of_concrete_object = Fw_poly.of_concrete_object ;;


let of_root root_dir = 
    Fw_poly.construct_fw_configuration 
      ~root:root_dir
      ~ignored_subdirectories:Coma_constant.git_ignored_subdirectories
      ~ignored_files:[]
    ;; 


let root = Fw_poly.root ;;

let test_for_admissibility data rl=
  (List.mem (
    (Dfn_rootless.to_ending rl)
  ) Dfa_ending.endings_for_readable_files)
  &&
   (List.for_all (
     fun sd->not(Dfn_rootless.is_in rl sd)
  ) (Fw_poly.ignored_subdirectories data)
  )  
  &&
  (
    not(List.mem rl (Fw_poly.ignored_files data))
  )
  ;;

let test_equality fw1 fw2 = 
    let temp =
    [
      ("root",(fw1.Fw_flattened_poly_t.root=fw2.Fw_flattened_poly_t.root));
      ("ignored_subdirectories",(fw1.Fw_flattened_poly_t.ignored_subdirectories=fw2.Fw_flattened_poly_t.ignored_subdirectories));
      ("ignored_files",(fw1.Fw_flattened_poly_t.ignored_files=fw2.Fw_flattened_poly_t.ignored_files));
    ] in 
    List.filter_map (fun (fld,is_ok)->if is_ok then None else Some fld) temp;;
  


let to_concrete_object = Fw_poly.to_concrete_object ;;
