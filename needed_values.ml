(*

#use"needed_values.ml";;

*)

let abo = Usual_coma_state.sugared_above;; 

let ae = Usual_coma_state.all_endinglesses;;

let bel = Usual_coma_state.sugared_below;;

let cod ()=
   let ucs = (!(Usual_coma_state.main_ref)) in 
   Check_ocaml_dircopy.check 
     (Fw_poly.to_fw_configuration ucs) 
        (Fw_poly.to_github_configuration ucs);;

let dabo = Usual_coma_state.sugared_directly_above;;

let dbel = Usual_coma_state.sugared_directly_below;;

let dm = Usual_coma_state.duplicate_module;;

let fg = Usual_coma_state.forget_one;;

let fgs = Usual_coma_state.forget_several;;

let hi = List.length ;; 

let home = Sys.getenv "HOME" ;;

let hmx = Usual_coma_state.find_endingless;;

let hod = Printf.sprintf "0x%08x";;

let ia = Usual_coma_state.set_internet_access;;

let image = Image.image ;;

let initialize_toplevel=Usual_coma_state.initialize_if_empty;;

let lc = Usual_coma_state.latest_changes;; 

let muv=Usual_coma_state.modules_using_value;;

let ocs=Other_coma_state.main_ref;;

let osv=Other_coma_state.show_value_occurrences_in_modulesystem;;  

let ovfm modname =Other_coma_state.list_values_from_module_in_modulesystem modname ;;

let reco msg=Usual_coma_state.recompile (Some msg);; 

let rsp msg = Usual_coma_state.recompile (Some (msg^ " in Szemeredi problem"));; 

let regi rootless_line=Usual_coma_state.register_rootless_line rootless_line;;

let regis rootless_lines=Usual_coma_state.register_rootless_lines rootless_lines;;

let relo = Usual_coma_state.relocate_module_to;;

let ren = Usual_coma_state.rename_module;;

let rensub = Usual_coma_state.rename_subdirectory;;

let ruco () = Other_coma_state.recompile None ;; 

let rf x=Io.read_whole_file (Absolute_path.of_string x);;


let rsh = Usual_coma_state.refresh;;

let rv=Usual_coma_state.rename_string_or_value;;

let sd ()=Usual_coma_state.start_debugging ();;
let se =Usual_coma_state.start_executing;;

let sv=Usual_coma_state.show_value_occurrences_in_modulesystem;;  

let ucs=Usual_coma_state.main_ref;;   

let vfm modname =Usual_coma_state.list_values_from_module_in_modulesystem modname ;;
