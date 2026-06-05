(*

#use"lib/Text_editing/systematic_coherent_pdf.ml";;

*)

module Private = struct 

let root_dir = "~/Teuliou/Heavy/Scanning/" ;;
let step3_dir = root_dir ^ "Step_3_Book_pdfs/" ;;
let step4_dir = root_dir ^ "Step_4_Same_sized_pages_book_pdfs/" ;;
let step5_dir = root_dir ^ "Step_5_Adbridged_pdfs/" ;;
let step6_dir = root_dir ^ "Step_6_Printable_pdfs/" ;;

end ;;


module Command = struct 

  let corep_cuttable_transform filename = 
    let ap = Absolute_path.of_string (Private.step5_dir ^ filename ^ ".pdf") in 
    let original_nbr = Coherent_pdf.number_of_pages_in_pdf ap in 
    let padded_nbr = (Basic.frac_ceiling original_nbr 8)*8 in 
    let intermediary_name = "YGQSCwoSZgQhzFTSnAQA" in 
    let current_dir = Sys.getcwd ()  in 
   ("cd "^ Coherent_pdf.work_path) :: 
   ("cp "^(Absolute_path.to_string ap)^" initial_copy.pdf") ::
   (Coherent_pdf.OnSiteCommand.corep_cuttable_transform "initial_copy" intermediary_name padded_nbr) @
    ["mv "^intermediary_name^".pdf "^Private.step6_dir^filename^".pdf";
     "cd "^current_dir];;

  let corep_foldable_transform filename = 
    let ap = Absolute_path.of_string (Private.step5_dir ^ filename ^ ".pdf") in 
    let original_nbr = Coherent_pdf.number_of_pages_in_pdf ap in 
    let padded_nbr = (Basic.frac_ceiling original_nbr 8)*8 in 
    let intermediary_name = "YGQSCwoSZgQhzFTSnAQA" in 
    let current_dir = Sys.getcwd ()  in 
   ("cd "^ Coherent_pdf.work_path) :: 
   ("cp "^(Absolute_path.to_string ap)^" initial_copy.pdf") ::
   (Coherent_pdf.OnSiteCommand.corep_foldable_transform "initial_copy" intermediary_name padded_nbr) @
    ["mv "^intermediary_name^".pdf "^Private.step6_dir^filename^".pdf";
     "cd "^current_dir];;

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

let corep_cuttable_transform filename= 
   Unix_command.indexed_multiple_uc (Command.corep_cuttable_transform filename) ;;

let corep_foldable_transform filename= 
   Unix_command.indexed_multiple_uc (Command.corep_foldable_transform filename) ;;

let force_same_size_for_all_pages filename= 
   Unix_command.indexed_multiple_uc (Command.force_same_size_for_all_pages filename) ;;