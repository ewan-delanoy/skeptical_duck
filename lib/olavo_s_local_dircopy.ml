(*

#use"lib/olavo_s_local_dircopy.ml";;

If you wish to reinitialize the object from scratch, you can do 

Pri.main_ref := Local_dircopy.initialize Pri.config ;;

where Pri is <ThisModule>.Private 

*)


module Private = struct 

let config = {
   Local_dircopy_config_t.allowed_number_of_digits = 3;
   remote_dir = "/media/" ^ (Sys.getenv "USER") ^ "/HEAVY/Other/OC/" ;
   frontier_dir = (Sys.getenv "HOME") ^ "/Downloads/OC/Lennet/"; 
   persisted_file_listing_dir_content = "nonml_files/nongithubbed_nonml_files/videos_with_olavo.txt";
} ;; 

let main_ref = ref (Local_dircopy.reload config) ;;



end ;;


let show_files () = Local_dircopy.show_files (!Private.main_ref) ;;
let update () = (Private.main_ref:= Local_dircopy.update (!Private.main_ref)) ;;









