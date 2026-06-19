(*

#use"lib/Text_editing/executable_coherent_pdf.ml";;

*)



module Private = struct 

let denounce msg =   let _ = Basic.display_message ( msg) in failwith("Error") ;; 

let remove_pdf_suffix fn =
   if String.ends_with fn ~suffix:".pdf"
   then Cull_string.two_sided_cutting ("",".pdf") fn 
   else fn ;;

let find_rectifications_in_directory dir prefix =
   let files = Unix_again.beheaded_simple_ls (Directory_name.of_string dir) in 
   let unordered_pairs = List.filter_map (
     fun fn -> 
      if (not(String.starts_with fn ~prefix))||(not(String.ends_with fn ~suffix:".pdf"))
      then None 
      else let core = Cull_string.two_sided_cutting (prefix,".pdf") fn in 
           match int_of_string_opt core with 
           None -> None 
           |Some i->Some(i,remove_pdf_suffix fn)   
   ) files in
   let indices = Ordered.sort Total_ordering.for_integers (Image.image fst unordered_pairs) in 
   Image.image (fun i->(i,List.assoc i unordered_pairs)) indices ;;

let find_file dir filename = 
   try Absolute_path.of_string (dir^"/"^filename) with 
   _ -> denounce("There is no file called "^filename^" in "^dir) ;;


let apply_pqyz_renaming _command_parameters =
   let dir= Directory_name.of_string (Sys.getcwd()) in  
   let _ =Coherent_pdf.apply_pqyz_renaming dir in 
   ();;  


let replace_pages_inside command_parameters = 
   if Array.length (command_parameters) < 4
   then denounce ("Some parameters are missing in your command. The format is cepdf -replace <patient-name> <prefix-for-replacers>,\n"^
        "for example : cepdf -replace corrupted.pdf rep, if your replacer files are rep3.pdf, rep27.pdf etc")  
   else
   let dir = Sys.getcwd () in 
   let filename = Array.get command_parameters 2
   and prefix = Array.get command_parameters 3 in 
   let patient_ap = find_file dir filename
   and rectifs = find_rectifications_in_directory dir prefix in 
   let _ =Coherent_pdf.replace_pages_inside 
    ~patient:patient_ap ~page_numbers_with_their_replacements:rectifs 
     ~outputfile_name:(remove_pdf_suffix filename) in 
   ();;

let options_for_main_command =
   [
     "-pqyz-rename",apply_pqyz_renaming;
     "-replace",replace_pages_inside;
   ] ;;

let description_of_options_for_main_command=
  "Currently, the available options are : \n" ^ (
    String.concat ",\n" (Image.image fst options_for_main_command)
  ) ^
  ".";;   

let denounce_bad_user_option explanation = 
    denounce (explanation^"\n"^description_of_options_for_main_command) ;;

let main command_parameters = 
   if Array.length (command_parameters) < 2
   then denounce_bad_user_option ("You need to provide an option to cepdf.")  
   else
   let user_option = Array.get command_parameters 1 in 
   match List.assoc_opt user_option options_for_main_command with 
   None -> denounce_bad_user_option ("Unknown option "^user_option^".")  
   |Some(proc) -> proc command_parameters ;;


end ;;

let main = Private.main ;;