(*

#use"lib/Text_editing/systematic_coherent_pdf.ml";;

*)

module Private = struct 

let root_dir = "~/Teuliou/Heavy/Scanning/" ;;
let step3_dir = root_dir ^ "Step_3_Book_pdfs/" ;;
let step4_dir = root_dir ^ "Step_4_Same_sized_pages_book_pdfs/" ;;
let step5_dir = root_dir ^ "Step_5_Adbridged_pdfs/" ;;
let step6_dir = root_dir ^ "Step_6_Printable_pdfs/" ;;

let i0 = int_of_char '0' ;;
let i9 = int_of_char '9' ;;
let is_a_digit c = 
   let i = int_of_char c in 
   (i0<=i) && (i<=i9) ;;

let is_not_a_digit c = not(is_a_digit c) ;;

let extract_rightmost_number name =
   let n = String.length name in 
   match String_find_char.backwards_from_inclusive_opt is_not_a_digit name n with 
   None -> ("",int_of_string name)
   |Some(k) ->
     if k =n 
     then (name,0)
     else (Cull_string.beginning k name,int_of_string(Cull_string.cobeginning k name)) ;;

(*

extract_rightmost_number "peggy" ;;
extract_rightmost_number "peggy567" ;;

*)

let extract_basename_and_rightmost_number name =
   let basename = Cull_string.before_rightmost_possibly_all  name '.' in 
   extract_rightmost_number basename ;;

(*

extract_basename_and_rightmost_number "peggy.txt" ;;
extract_basename_and_rightmost_number "peggy567.txt" ;;
extract_basename_and_rightmost_number "peggy567" ;;

*)

let all_numbered_files () =
   let dir = Directory_name.of_string Coherent_pdf.work_path in 
   let all = Unix_again.beheaded_simple_ls dir in 
   let pdfs = List.filter (String.ends_with ~suffix:".pdf") all in 
   List.filter_map (
    fun pdf -> let (name,nbr) = extract_basename_and_rightmost_number pdf in 
     if nbr<>0 then Some(nbr,name) else None 
   ) pdfs  ;; 

exception Missing_file_for_index of int ;;

let get_files_from_indices indices = 
   let associator = all_numbered_files () in 
   Image.image (
    fun i -> match List.assoc_opt i associator with 
    None -> raise(Missing_file_for_index(i))
    |Some file ->(i,file)
   ) indices ;;



end ;;


module Command = struct 

   let corep_bigger_cuttable_transform filename = 
    let ap = Absolute_path.of_string (Private.step5_dir ^ filename ^ ".pdf") in 
    let original_nbr = Coherent_pdf.number_of_pages_in_pdf ap in 
    let padded_nbr = (Basic.frac_ceiling original_nbr 8)*8 in 
    let intermediary_name = "YGQSCwoSZgQhzFTSnAQA" in 
    let current_dir = Sys.getcwd ()  in 
   ("cd "^ Coherent_pdf.work_path) :: 
   ("cp "^(Absolute_path.to_string ap)^" initial_copy.pdf") ::
   (Coherent_pdf.OnSiteCommand.corep_bigger_cuttable_transform "initial_copy" intermediary_name padded_nbr) @
    ["mv "^intermediary_name^".pdf "^Private.step6_dir^"printable_"^filename^".pdf";
     "cd "^current_dir];;

  let corep_cuttable_transform filename = 
    let ap = Absolute_path.of_string (Private.step5_dir ^ filename ^ ".pdf") in 
    let original_nbr = Coherent_pdf.number_of_pages_in_pdf ap in 
    let padded_nbr = (Basic.frac_ceiling original_nbr 8)*8 in 
    let intermediary_name = "YGQSCwoSZgQhzFTSnAQA" in 
    let current_dir = Sys.getcwd ()  in 
   ("cd "^ Coherent_pdf.work_path) :: 
   ("cp "^(Absolute_path.to_string ap)^" initial_copy.pdf") ::
   (Coherent_pdf.OnSiteCommand.corep_cuttable_transform "initial_copy" intermediary_name padded_nbr) @
    ["mv "^intermediary_name^".pdf "^Private.step6_dir^"printable_"^filename^".pdf";
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
    ["mv "^intermediary_name^".pdf "^Private.step6_dir^"printable_"^filename^".pdf";
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

  let replace_pages_inside filename indices_to_be_replaced= 
    let ap = Absolute_path.of_string (Coherent_pdf.work_path ^ filename ^ ".pdf") in 
    let intermediary_name = "YGQSCwoSZgQhzFTSnAQA" in 
    let current_dir = Sys.getcwd ()  in 
    let files = Private.get_files_from_indices indices_to_be_replaced in 
    let commands_for_copying_replacers = Image.image (fun (i,file)->
      let si = string_of_int i in 
      "cp "^file^si^".pdf replacer"^si^".pdf"  
    ) files in 
    let total_nbr_of_pages = Coherent_pdf.number_of_pages_in_pdf ap in 
   ("cd "^ Coherent_pdf.work_path) :: 
   ("cp "^(Absolute_path.to_string ap)^" initial_copy.pdf") ::
   (commands_for_copying_replacers@(
   (Coherent_pdf.OnSiteCommand.replace_pages_inside total_nbr_of_pages indices_to_be_replaced intermediary_name) @
    ["mv "^intermediary_name^".pdf "^filename^ ".pdf";
     "cd "^current_dir]));;     


end ;;  

let corep_bigger_cuttable_transform filename= 
   Unix_command.indexed_multiple_uc (Command.corep_bigger_cuttable_transform filename) ;;

let corep_cuttable_transform filename= 
   Unix_command.indexed_multiple_uc (Command.corep_cuttable_transform filename) ;;

let corep_foldable_transform filename= 
   Unix_command.indexed_multiple_uc (Command.corep_foldable_transform filename) ;;

let force_same_size_for_all_pages filename= 
   Unix_command.indexed_multiple_uc (Command.force_same_size_for_all_pages filename) ;;

let replace_pages_inside filename indices_to_be_replaced =
   Unix_command.indexed_multiple_uc (Command.replace_pages_inside filename indices_to_be_replaced) ;;    