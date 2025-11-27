(*

#use"lib/Text_editing/from_scanner_to_printer.ml";;

*)

module Private = struct 

let home = Sys.getenv "HOME" ;;
let work_path = home ^ "/Teuliou/Heavy/Scanning/" ;;

let step1_dir = Directory_name.of_string 
    (work_path^"Step_1_Raw_scan_pngs_or_pdfs/") ;;

let step2_dir = Directory_name.of_string 
    (work_path^"Step_2_Page_pdfs/") ;;

let step3_dir = Directory_name.of_string 
    (work_path^"Step_3_Book_pdfs/") ;;    

let step4_dir = Directory_name.of_string 
    (work_path^"Step_4_Same_sized_pages_book_pdfs/") ;; 
    
let step5_dir = Directory_name.of_string 
    (work_path^"Step_5_Printable_pdfs/") ;;     

let screenshots_dir =  Directory_name.of_string 
    (home ^ "/Pictures/Screenshots") ;; 

let step1_receive_raw_data  ?(original_source=screenshots_dir) project_name =
  let step1_workpath = (Directory_name.connectable_to_subpath step1_dir)^
             (String.capitalize_ascii project_name) in 
  let cmd1 = "mkdir -p "^step1_workpath in  
  let s_dir = Directory_name.connectable_to_subpath original_source in 
  let cmd2 = "mv "^s_dir^"*.png "^s_dir^"*.pdf "^step1_workpath in 
  Unix_command.conditional_multiple_uc [cmd1;cmd2] ;;


end ;;  

let step1_receive_raw_data = Private.step1_receive_raw_data ;;