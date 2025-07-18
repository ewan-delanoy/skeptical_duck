(*

#use"lib/needed_values.ml";;

*)

let abo = Fw_usual_manager.sugared_above;; 

let afs = Manage_diary.append_fresh_snippet ;;

let ae = Fw_usual_manager.all_endinglesses;;

let bel = Fw_usual_manager.sugared_below;;

let cod ()=
   let ucs = Fw_usual_manager.current_state() in 
   Check_ocaml_dircopy.check 
     (Fwc_with_githubbing.Inherited.to_fw_configuration ucs) 
        (Fwc_with_githubbing.github_configuration ucs);;

        
        
let dabo = Fw_usual_manager.sugared_directly_above;;

let dbel = Fw_usual_manager.sugared_directly_below;;

let dm = Fw_usual_manager.duplicate_module;;

let fg = Fw_usual_manager.forget_one;;

let fgs = Fw_usual_manager.forget_several;;

let hi = List.length ;; 

let home = Sys.getenv "HOME" ;;

let hmx = Fw_usual_manager.find_endingless;;

let hod = Printf.sprintf "0x%08x";;

let ia = Fw_usual_manager.set_internet_access;;

let image = Image.image ;;

let initialize_toplevel=Fw_usual_manager.load_persisted_version;;

let lc () = let _ = Fw_usual_manager.latest_changes () in ();; 

let muv=Fw_usual_manager.modules_using_value;;

let save msg=Fw_usual_manager.save_latest_changes (Some msg);; 

let regi rootless_line=Fw_usual_manager.register_rootless_line rootless_line;;

let relo = Fw_usual_manager.relocate_module_to;;

let ren = Fw_usual_manager.rename_module;;

let rensub = Fw_usual_manager.rename_subdirectory;;

let rf x=Io.read_whole_file (Absolute_path.of_string x);;


let rsh = Fw_usual_manager.refresh;;

let rv=Fw_usual_manager.rename_string_or_value;;

let sd ()=Fw_usual_manager.start_debugging ();;

let sv=Fw_usual_manager.show_value_occurrences_in_modulesystem;;  

let vfm modname =Fw_usual_manager.list_values_from_module_in_modulesystem modname ;;

