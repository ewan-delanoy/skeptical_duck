(*

#use"lib/Mad_world/mw_needed_values.ml";;

*)

let abo = Mw_usual_coma_state.sugared_above;; 

let ae = Mw_usual_coma_state.all_endinglesses;;

let bel = Mw_usual_coma_state.sugared_below;;

let cod ()=
   let ucs = (!(Mw_usual_coma_state.main_ref)) in 
   Mw_check_ocaml_dircopy.check 
     (Mw_poly.to_fw_configuration ucs) 
        (Mw_poly.to_github_configuration ucs);;

let dabo = Mw_usual_coma_state.sugared_directly_above;;

let dbel = Mw_usual_coma_state.sugared_directly_below;;

let dm = Mw_usual_coma_state.duplicate_module;;

let fg = Mw_usual_coma_state.forget_one;;

let fgs = Mw_usual_coma_state.forget_several;;

let hi = List.length ;; 

let home = Sys.getenv "HOME" ;;

let hmx = Mw_usual_coma_state.find_endingless;;

let hod = Printf.sprintf "0x%08x";;

let ia = Mw_usual_coma_state.set_internet_access;;

let image = Image.image ;;

let initialize_toplevel=Mw_usual_coma_state.initialize_if_empty;;

let lc () = let _ = Mw_usual_coma_state.latest_changes () in ();; 

let muv=Mw_usual_coma_state.modules_using_value;;

let ocs=Mw_other_coma_state.main_ref;;

let osv=Mw_other_coma_state.show_value_occurrences_in_modulesystem;;  

let ovfm modname =Mw_other_coma_state.list_values_from_module_in_modulesystem modname ;;

let reco msg=Mw_usual_coma_state.recompile (Some msg);; 

let rsp msg = Mw_usual_coma_state.recompile (Some (msg^ " in Sz3_preliminaries"));; 

let regi rootless_line=Mw_usual_coma_state.register_rootless_line rootless_line;;

let regis rootless_lines=Mw_usual_coma_state.register_rootless_lines rootless_lines;;

let relo = Mw_usual_coma_state.relocate_module_to;;

let ren = Mw_usual_coma_state.rename_module;;

let rensub = Mw_usual_coma_state.rename_subdirectory;;

let ruco () = Mw_other_coma_state.recompile None ;; 

let rf x=Io.read_whole_file (Absolute_path.of_string x);;


let rsh = Mw_usual_coma_state.refresh;;

let rv=Mw_usual_coma_state.rename_string_or_value;;

let sd ()=Mw_usual_coma_state.start_debugging ();;
let se =Mw_usual_coma_state.start_executing;;

let sv=Mw_usual_coma_state.show_value_occurrences_in_modulesystem;;  

let ucs=Mw_usual_coma_state.main_ref;;   

let vfm modname =Mw_usual_coma_state.list_values_from_module_in_modulesystem modname ;;
