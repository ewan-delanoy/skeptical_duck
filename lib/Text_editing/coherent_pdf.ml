(*

#use"lib/Text_editing/coherent_pdf.ml";;

*)


module Private = struct 
  let home = Sys.getenv "HOME" ;;

  let work_path = 
          home ^ 
        "/Teuliou/Printable/" ;;

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

    let corep_transform onsite_input outputfile_name padded_nbr= 
    let q = (padded_nbr/8) in
    let corep_order = List.flatten (Int_range.scale (fun j->
        [2*j-1;2*q+2*j-1;2*j;2*q+2*j]
      ) 1 q) in 
    (pad_up_to_multiple onsite_input  8 "padded")::
    (explode  "padded" "page")::
    (
     [
       (implode "page" "reaggregated" corep_order);
       ("cpdf -impose-pdf \"2 2\" reaggregated.pdf -o "^outputfile_name^".pdf");
       "rm initial_copy.pdf page*.pdf padded.pdf reaggregated.pdf";
     ]
    );; 

end ;;  


module Command = struct 
  let corep_transform ap outputfile_name = 
    let original_nbr = Private.number_of_pages_in_pdf ap in 
    let padded_nbr = (Basic.frac_ceiling original_nbr 4)*4 in 
    let current_dir = Sys.getcwd () in 
   ("cd "^ Private.work_path) :: 
   ("cp "^(Absolute_path.to_string ap)^" initial_copy.pdf") ::
   (OnSiteCommand.corep_transform "initial_copy" outputfile_name padded_nbr) @
    ["cd "^current_dir];;

end ;;  

let corep_transform ap ~outputfile_name= 
   Unix_command.conditional_multiple_uc 
    (Command.corep_transform ap outputfile_name) ;;

let number_of_pages_in_pdf = 
    Private.number_of_pages_in_pdf ;;
      