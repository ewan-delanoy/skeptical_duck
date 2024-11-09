(*

#use"lib/Text_editing/coherent_pdf.ml";;

*)


module Private = struct 
  let home = Sys.getenv "HOME" ;;

  let work_path = 
          home ^ 
        "/Teuliou/OCaml/skeptical_duck/nonml_files/"^
        "nongithubbed_nonml_files/pdf_workspace/" ;;

  let commands_for_number_of_pages_in_pdf ap = 
    let temp_file = work_path ^ "temp.txt" in 
    (
      temp_file,  
    [
       "touch "^temp_file;
       "cpdf -info "^(Absolute_path.to_string ap)^" > "^temp_file
    ],
     [
       "rm "^temp_file  
     ]);;      

   let number_of_pages_in_pdf ap = 
    let (temp_file,before,after) = 
      commands_for_number_of_pages_in_pdf ap in 
    let _ = Unix_command.conditional_multiple_uc before in 
    let data = Io.read_whole_file (Absolute_path.of_string temp_file) in 
    let _ = Unix_command.conditional_multiple_uc after in 
    let temp1 = Lines_in_string.lines data in 
    let prefix = "Pages:" in 
    let temp2 = List.find (String.starts_with ~prefix) temp1 in
    let temp3 = Cull_string.two_sided_cutting (prefix,"") temp2 in
    int_of_string (Cull_string.trim_spaces temp3)  ;;

    let partial prefix idx = 
         prefix^
        (Strung.insert_repetitive_offset_on_the_left '0' 3 
        (string_of_int idx)) ;; 


  end ;;  

module OnSiteCommand = struct 

  module OnSiteCommandPrivate = struct 
  
    let number_of_pages_in_pdf onsite_file = 
      let ap = Absolute_path.of_string(
          Private.work_path ^ onsite_file ^ ".pdf"
      )  in 
      Private.number_of_pages_in_pdf ap ;;

    
  end ;;  
  let explode onsite_input output_prefix= 
    "cpdf -split "^onsite_input^".pdf "^
    "-o "^output_prefix^"%%%.pdf -chunk 1" ;;

  let implode prefix name_for_whole indices=
    let all_pages=String.concat " " 
    (Image.image (fun idx->(Private.partial prefix idx)^".pdf") indices) in 
    " cpdf "^all_pages^" -o "^name_for_whole^".pdf";;

  let pad_up_to_multiple onsite_input m output_name=
    "cpdf "^onsite_input^".pdf -pad-multiple "^(string_of_int m)^
    " -o "^output_name^".pdf";; 

  let reverse_one onsite_input = 
    ["cpdf -rotate-contents 180 "^onsite_input^".pdf -o temp.pdf";
     "cp temp.pdf "^onsite_input^".pdf" ;
     "rm temp.pdf"
    ] ;;  

  let reverse_several prefix indices = 
    List.flatten ( Image.image
      (fun idx ->
        let name = Private.partial prefix idx in 
        reverse_one name) 
      indices );;   

  let reverse_pages_in_corep_subset page_prefix ~number_of_pages= 
    let corep_subset = List.filter (
      fun idx -> List.mem(idx mod 4)[2;3]
    ) (Int_range.range 1 number_of_pages) in 
    reverse_several page_prefix corep_subset ;;

  let corep_transform onsite_input outputfile_name padded_nbr= 
    let corep_order = List.flatten (Int_range.scale (fun q->
        [4*q;4*q-3;4*q-1;4*q-2]
      ) 1 (padded_nbr/4)) in 
    (pad_up_to_multiple onsite_input  4 "padded")::
    (explode  "padded" "page")::
    (reverse_pages_in_corep_subset  "page" ~number_of_pages:padded_nbr)@
    (
     [
       (implode "page" outputfile_name corep_order);
       "rm page*.pdf padded.pdf";
     ]
    );; 

end ;;  


module Command = struct 
  let corep_transform ap = 
    let original_nbr = Private.number_of_pages_in_pdf ap in 
    let padded_nbr = (Basic.frac_ceiling original_nbr 4)*4 in 
    let current_dir = Sys.getcwd () in 
   ("cd "^ Private.work_path) :: 
   ("cp "^(Absolute_path.to_string ap)^" initial_copy.pdf") ::
   (OnSiteCommand.corep_transform "initial_copy" "corepped" padded_nbr) @
    ["cd "^current_dir];;

end ;;  

let corep_transform ap = 
   Unix_command.conditional_multiple_uc 
    (Command.corep_transform ap) ;;

let number_of_pages_in_pdf = 
    Private.number_of_pages_in_pdf ;;
      