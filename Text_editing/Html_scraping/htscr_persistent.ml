(*

#use"Text_editing/Html_scraping/htscr_persistent.ml";;

*)

module Private = struct 

      let main_ref = ref Htscr_state.empty_one ;;
      
      let ap_for_persistence = Absolute_path.of_string 
         ((Sys.getenv "HOME")^"/Teuliou/OCaml/Ordinary/Text_editing/Html_scraping/htscr_state.txt");;
      
      let initialization_already_done = ref false ;;
      
      let save_to_disk () =
          let stringified_crobj = Crobj_parsing.unparse (Htscr_state.to_concrete_object (!main_ref)) in 
          Io.overwrite_with ap_for_persistence stringified_crobj ;;
      
      let read_from_disk () =
          let crobj = Crobj_parsing.parse (Io.read_whole_file ap_for_persistence) in    
          main_ref := (Htscr_state.of_concrete_object crobj);;    
      
      let initialize () = 
         if (!initialization_already_done) 
         then () 
         else (read_from_disk();initialization_already_done:=true) ;;  
      
      
      let read_html config ap =
         let text = Io.read_whole_file ap in 
         let (new_items,new_state)= Htscr_state.read_text config (!main_ref) text in  
         let (commands,reps) = Htscr_item.commands_and_replacements config new_items in 
         let _ = Unix_command.conditional_multiple_uc commands in 
         let _ = (main_ref:=new_state) in 
      reps;;   

end ;;      

let initialize = Private.initialize ;;
let read_html = Private.read_html ;;      