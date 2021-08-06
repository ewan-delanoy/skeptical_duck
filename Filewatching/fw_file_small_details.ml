(*

#use"Filewatching/fw_file_small_details.ml";;

*)


module Private = struct 

let salt = "Fw_"^"file_simple_details_t.";;
      
let used_modules_label   = salt ^ "used_modules";;
let used_libraries_label = salt ^ "used_libraries";; 
let has_printer_label    = salt ^ "has_printer";;

let of_concrete_object ccrt_obj = 
  let g=Concrete_object.get_record ccrt_obj in
  {
     Fw_file_small_details_t.used_modules = Crobj_converter_combinator.to_list Dfa_module.of_concrete_object (g used_modules_label);
     used_libraries = Crobj_converter_combinator.to_list Ocaml_library.of_concrete_object (g used_libraries_label);
     has_printer = Crobj_converter.bool_of_concrete_object (g has_printer_label);
  };; 

let to_concrete_object fsd=
  let items= 
  [
    used_modules_label, Crobj_converter_combinator.of_list Dfa_module.to_concrete_object fsd.Fw_file_small_details_t.used_modules;
    used_libraries_label, Crobj_converter_combinator.of_list Ocaml_library.to_concrete_object fsd.Fw_file_small_details_t.used_libraries;
    has_printer_label, Crobj_converter.bool_to_concrete_object fsd.Fw_file_small_details_t.has_printer;
  
  ]  in
  Concrete_object_t.Record items;;

end ;;  

let compute ap =
    let full_text = Io.read_whole_file ap in 
    let used_mods = Look_for_module_names.names_in_mlx_file ap in 
    let snippets = Outside_comments_and_strings.good_substrings full_text in 
    let printer_exists = List.exists (fun (i,j,subtext)->
           (Detect_printer_declaration_in_text.detect subtext)<>None 
         ) snippets  in 
    let used_libs = Ocaml_library.compute_needed_libraries_from_uncapitalized_modules_list
       (Image.image Dfa_module.to_line used_mods) in      
    {
      Fw_file_small_details_t.used_modules = used_mods;
      used_libraries = used_libs ;
      has_printer = printer_exists;
    } ;;
    

let has_printer fsd = fsd.Fw_file_small_details_t.has_printer ;;
let of_concrete_object = Private.of_concrete_object ;;
let to_concrete_object = Private.to_concrete_object ;;
let used_libraries fsd = fsd.Fw_file_small_details_t.used_libraries ;;
let used_modules fsd = fsd.Fw_file_small_details_t.used_modules ;;