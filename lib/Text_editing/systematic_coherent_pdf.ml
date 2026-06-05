(*

#use"lib/Text_editing/systematic_coherent_pdf.ml";;

*)

module Private = struct 

let root_dir = "~/Teuliou/Heavy/Scanning/" ;;
let step3_dir = root_dir ^ "Step_3_Book_pdfs/" ;;
let step4_dir = root_dir ^ "Step_4_Same_sized_pages_book_pdfs/" ;;

end ;;


module Command = struct 

  let force_same_size_for_all_pages filename = 
    let ap = Absolute_path.of_string (Private.step3_dir ^ filename ^ ".pdf") in 
    let (avg_width,avg_height) = Coherent_pdf.Private.average_page_width_and_height ap in 
    let intermediary_name = "YGQSCwoSZgQhzFTSnAQA" in 
    let current_dir = Sys.getcwd ()  in 
   ("cd "^ Coherent_pdf.work_path) :: 
   ("cp "^(Absolute_path.to_string ap)^" initial_copy.pdf") ::
   (Coherent_pdf.OnSiteCommand.force_same_size_for_all_pages 
   "initial_copy" intermediary_name ~forced_width:avg_width ~forced_height:avg_height) @
    ["mv "^intermediary_name^".pdf "^Private.step4_dir^filename^".pdf";
      "cd "^current_dir];; 



end ;;  

let force_same_size_for_all_pages filename= 
   Unix_command.indexed_multiple_uc (Command.force_same_size_for_all_pages filename) ;;