(*

#use"Filewatching/fw_configuration.ml";;

*)

let root config = config.Fw_configuration_t.root;;

let of_root root_dir = 
    Fw_poly.construct_fw_configuration 
      ~root:root_dir
      ~ignored_subdirectories:Coma_constant.git_ignored_subdirectories
      ~ignored_files:[]
    ;; 



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

