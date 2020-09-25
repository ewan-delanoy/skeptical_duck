(* 

#use"Hex_analysis/hex_readable_and_writable_sheet.ml";;

*)

module Private =struct 

let path_for_sheet = Absolute_path.of_string "Hex_analysis/Hex_gitignored_text_files/hex_config_sheet.txt";;

end ;; 

let read () = Io.read_whole_file Private.path_for_sheet ;;
let write text = Io.overwrite_with Private.path_for_sheet text;;


