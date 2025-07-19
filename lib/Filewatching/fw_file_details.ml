(*

#use"lib/Filewatching/fw_file_details.ml";;

*)


module Private = struct 

let istr_of_concrete_object =
    Crobj_converter_combinator.to_pair 
    Crobj_converter.int_of_concrete_object
    Crobj_converter.string_of_concrete_object ;;

let istr_to_concrete_object =
    Crobj_converter_combinator.of_pair 
    Crobj_converter.int_to_concrete_object
    Crobj_converter.string_to_concrete_object ;;    

let istrl_of_concrete_object =
  Crobj_converter_combinator.to_list istr_of_concrete_object ;;

let istrl_to_concrete_object =
  Crobj_converter_combinator.of_list istr_to_concrete_object ;;  

let salt = "Fw_"^"file_simple_details_t.";;
      
let used_modules_label        = salt ^ "used_modules";;
let used_libraries_label      = salt ^ "used_libraries";; 
let has_printer_label         = salt ^ "has_printer";;
let registered_printers_label = salt ^ "registered_printers";;
let modification_time_label   = salt ^ "modification_time";;

let of_concrete_object ccrt_obj = 
  let g=Concrete_object.get_record ccrt_obj in
  {
     Fw_file_details_t.used_modules = Crobj_converter_combinator.to_list Dfa_module.of_concrete_object (g used_modules_label);
     used_libraries = Crobj_converter_combinator.to_list Ocaml_library.of_concrete_object (g used_libraries_label);
     has_printer = Crobj_converter.bool_of_concrete_object (g has_printer_label);
     registered_printers = istrl_of_concrete_object (g registered_printers_label); 
     modification_time = Crobj_converter.string_of_concrete_object (g modification_time_label);
  };; 

let to_concrete_object fsd=
  let items= 
  [
    used_modules_label, Crobj_converter_combinator.of_list Dfa_module.to_concrete_object fsd.Fw_file_details_t.used_modules;
    used_libraries_label, Crobj_converter_combinator.of_list Ocaml_library.to_concrete_object fsd.Fw_file_details_t.used_libraries;
    has_printer_label, Crobj_converter.bool_to_concrete_object fsd.Fw_file_details_t.has_printer;
    registered_printers_label, istrl_to_concrete_object fsd.Fw_file_details_t.registered_printers;
    modification_time_label, Crobj_converter.string_to_concrete_object (fsd.Fw_file_details_t.modification_time);
  ]  in
  Concrete_object_t.Record items;;

end ;;  

let compute ap =
    let full_text = Io.read_whole_file ap in 
    let used_mods = Look_for_module_names.names_in_mlx_file ap in 
    let snippets = Outside_ocaml_comments_and_strings.good_substrings full_text in 
    let printer_exists = List.exists (fun (_i,_j,subtext,_linenbr)->
           (Detect_printer_declaration_in_text.detect subtext)<>None 
         ) snippets  in 
    let used_libs = Ocaml_library.compute_needed_libraries_from_uncapitalized_modules_list
       (Image.image Dfa_module.to_line used_mods) in  
    let s_ap = Absolute_path.to_string ap in       
    let mtime = string_of_float((Unix.stat s_ap).Unix.st_mtime) 
    and printers = Detect_printer_registration_in_text.detect ap in    
    {
      Fw_file_details_t.used_modules = used_mods;
      used_libraries = used_libs ;
      has_printer = printer_exists;
      registered_printers = printers ;
      modification_time = mtime ;
    } ;;
    

let has_printer fsd = fsd.Fw_file_details_t.has_printer ;;
let modification_time fsd = fsd.Fw_file_details_t.modification_time ;;
let of_concrete_object = Private.of_concrete_object ;;
let registered_printers fsd = fsd.Fw_file_details_t.registered_printers ;;
let to_concrete_object = Private.to_concrete_object ;;
let used_libraries fsd = fsd.Fw_file_details_t.used_libraries ;;
let used_modules fsd = fsd.Fw_file_details_t.used_modules ;;