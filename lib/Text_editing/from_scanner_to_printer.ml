(*

#use"lib/Text_editing/from_scanner_to_printer.ml";;

*)

exception Disconnected of (int * int) list ;;

module Private = struct 

let home = Sys.getenv "HOME" ;;
let master_work_path = home ^ "/Teuliou/Heavy/Scanning/" ;;

module Common = struct 


  let i_order = Total_ordering.for_integers ;;
  let i_sort = Ordered.sort i_order ;;
 
 let is_a_nondigit c = 
     let i = int_of_char c in
     (i<48)||(i>57)  ;;

  let earliest_nondigit_from s starting_idx = 
    let n = String.length s in 
    let rec tempf=(
      fun k->
        if k>n then n+1 else 
        if is_a_nondigit(String.get s (k-1))  
        then k 
        else tempf(k+1)  
    ) in 
    tempf starting_idx ;;

  let read_number_starting_at s starting_idx =
    if is_a_nondigit(String.get s (starting_idx-1))  
    then None 
    else
    let m = earliest_nondigit_from s starting_idx in 
    Some(int_of_string(Cull_string.interval s starting_idx (m-1)));;


end ;;  
 

let step4_dir = Directory_name.of_string 
    (master_work_path^"Step_4_Same_sized_pages_book_pdfs/") ;; 
    
let step5_dir = Directory_name.of_string 
    (master_work_path^"Step_5_Printable_pdfs/") ;;     

module Step1 = struct 

  let work_path = 
    (master_work_path^"Step_1_Raw_scan_pngs_or_pdfs/") ;;

let screenshots_dir =  Directory_name.of_string 
    (home ^ "/Pictures/Screenshots") ;; 

let receive_raw_data  ?(original_source=screenshots_dir) project_name =
  let step1_workpath = work_path^
             (String.capitalize_ascii project_name) in 
  let cmd1 = "mkdir -p "^step1_workpath in  
  let s_dir = Directory_name.connectable_to_subpath original_source in
  let list1 = Unix_again.beheaded_simple_ls original_source in  
  let cmd2_maybe = (
     List.exists (fun s->String.ends_with s ~suffix:".png") list1,
     "mv "^s_dir^"*.png "^step1_workpath) 
  and cmd3_maybe = (
     List.exists (fun s->String.ends_with s ~suffix:".pdf") list1, 
     "mv "^s_dir^"*.pdf "^step1_workpath) in 
  let cmd23 = 
    List.filter_map (fun (t,cmd)->if t then Some cmd else None)
    [cmd2_maybe;cmd3_maybe] in    
  Unix_command.conditional_multiple_uc (cmd1::cmd23) ;;

  end ;;

module Step2 = struct 
  
  let work_path = master_work_path^"Step_2_Page_pdfs/" ;;
  
  let hundreds_in_first_char c =
    (*
      p means 0*100, q means 1*100 etc 
    *)
    let i = int_of_char c in 
    if (i<112)||(i>122)
    then None 
    else Some(i-112);; 
  
  type allowed_ending = Png | Pdf ;;

  type page_kind = Illustration | Not_an_illustration ;;

  let determine_ending fn =
    if String.ends_with fn ~suffix:".png"   
    then Some Png 
    else 
    if String.ends_with fn ~suffix:".pdf"   
    then Some Pdf
    else None;;

  let illustration_prefix =   "illustration_" ;;
  let determine_page_kind fn =
    if String.starts_with fn ~prefix:illustration_prefix
    then Illustration 
    else Not_an_illustration ;;

  

  let analize_filename fn =
    match determine_ending fn with 
    None -> None 
    |Some ending ->
    let kind = determine_page_kind fn in 
    let starting_idx = (
       match kind with 
         Illustration -> (String.length illustration_prefix) +1
        | Not_an_illustration -> 2
    ) in 
    match Common.read_number_starting_at fn starting_idx with 
    None -> None 
    |Some number ->
    let final_page_number_opt = (
        match kind with 
         Illustration -> Some number
        | Not_an_illustration -> 
          (
            match hundreds_in_first_char (String.get fn 0) with 
            None -> None 
            |Some hundreds -> Some(100*hundreds+number) 
          )
    ) in 
    match final_page_number_opt with 
    None -> None 
    |Some final_page_number ->
    let initial_in_final_name = (
       match kind with 
         Illustration -> "i"
        | Not_an_illustration -> "p"
    ) in 
    let final_name = initial_in_final_name^
        (string_of_int final_page_number)^".pdf" in 
    Some(fn,(ending,final_name));;

  

  let conversion_commands_for_analized_filename proj_name uple=
   let (fn,(ending,final_name)) = uple in 
   let engine = (match ending with 
      Pdf -> "cp"
    | Png -> "convert"
   ) in
   [
    engine^" "^fn^" produced_"^final_name;
    "mv produced_"^final_name^" "
    ^work_path^(String.capitalize_ascii proj_name)^"/";

   ] ;;

  let renaming_command_for_analized_filename uple=
   let (_fn,(_ending,final_name)) = uple in 
    "mv produced_"^final_name^" "^final_name
   ;; 

  let complete_set_of_commands proj_name =
    let source = Step1.work_path ^ (String.capitalize_ascii proj_name) 
    and destination = work_path ^ (String.capitalize_ascii proj_name)in 
    let all_files = Unix_again.beheaded_simple_ls 
    (Directory_name.of_string source) in 
    let analized_files = List.filter_map analize_filename all_files in 
    let conversion_commands =
      List.flatten (Image.image 
      (conversion_commands_for_analized_filename proj_name) analized_files) 
    and renaming_commands =
      Image.image renaming_command_for_analized_filename analized_files in
     let current_dir = Sys.getcwd () in 
   ("mkdir -p "^ destination) :: 
   ("cd "^ source) :: 
   conversion_commands @
   (
    ("cd "^ destination) :: 
   renaming_commands @
    ["cd "^current_dir]
   );;  

  let act proj_name = Unix_command.indexed_multiple_uc 
     (complete_set_of_commands proj_name) ;;  
  
  
end ;;  

module Step3 = struct 

let work_path =  master_work_path^"Step_3_Book_pdfs/" ;;    

let extract_file_with_prefix prefix fn =
  if not(String.starts_with fn ~prefix) 
  then None 
  else 
  Common.read_number_starting_at fn (String.length(prefix)+1) ;;

let extract_files_with_prefix files prefix =
  let temp = Common.i_sort(List.filter_map (extract_file_with_prefix prefix) files) in 
  if temp=[] then None else
  let temp2 = Arithmetic_list.decompose_into_connected_components temp in 
  if List.length(temp2)>1 
  then raise(Disconnected(temp2))
  else Some(prefix,List.hd temp2);;  
let analize_files proj_name= 
    let source = Step2.work_path ^ (String.capitalize_ascii proj_name) in 
    let all_files = Unix_again.beheaded_simple_ls 
    (Directory_name.of_string source) in 
    List.filter_map (extract_files_with_prefix all_files) ["p";"i"] ;;

let filelist_for_prefixed_range (prefix,(i,j)) =
  String.concat " " (Int_range.scale (fun k->prefix^(string_of_int k)) 
  i j) ;;

let filelist_for_prefixed_ranges prefixed_ranges =
  String.concat " " (Image.image filelist_for_prefixed_range prefixed_ranges) ;;  



let commands proj_name =
    let source = Step1.work_path ^ (String.capitalize_ascii proj_name) 
    and destination = work_path ^ (String.capitalize_ascii proj_name)in 
    let prefixed_ranges = analize_files proj_name in 
    let ingredients = filelist_for_prefixed_ranges prefixed_ranges in 
    let current_dir = Sys.getcwd () in 
   [
      ("mkdir -p "^ destination);
       "cd "^ source;
      "cpdf "^ingredients^" - o "^proj_name^".pdf";
      "mv "^proj_name^".pdf "^destination^"/";
      "cd "^current_dir 
   ] ;;  
   
 let act proj_name = Unix_command.indexed_multiple_uc 
     (commands proj_name) ;;  
  


end ;;  


end ;;  

let step1_receive_raw_data = Private.Step1.receive_raw_data ;;
let step2_convert_to_pdf = Private.Step2.act ;;
let step3_merge_into_book = Private.Step3.act ;;