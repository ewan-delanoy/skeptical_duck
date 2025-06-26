(*

#use"lib/Text_editing/coherent_pdf.ml";;

*)


module Private = struct 

  exception Shifted_mediabox 
    of string * string * string * string ;;  
  let home = Sys.getenv "HOME" ;;

  let work_path = home ^ "/Teuliou/Heavy/Scanning/Workshop/" ;;

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
    let temp1 = Lines_in_text.lines data in 
    let prefix = "Pages:" in 
    let temp2 = List.find (String.starts_with ~prefix) temp1 in
    let temp3 = Cull_string.two_sided_cutting (prefix,"") temp2 in
    int_of_string (Cull_string.trim_spaces temp3)  ;;

    let partial prefix idx = 
         prefix^
        (Strung.insert_repetitive_offset_on_the_left '0' 3 
        (string_of_int idx)) ;; 

   let commands_for_mediabox_in_pdf ap page_nbr = 
    let temp_file = work_path ^ "temp.txt" in 
    (
      temp_file,  
    [
       "touch "^temp_file;
       "cpdf "^(Absolute_path.to_string ap)^
       " -page-info "^(string_of_int page_nbr)^" > "^temp_file
    ],
     [
       "rm "^temp_file  
     ]);;           

  let mediabox_in_pdf ap page_nbr = 
    let (temp_file,before,after) = 
      commands_for_mediabox_in_pdf ap page_nbr in 
    let _ = Unix_command.conditional_multiple_uc before in 
    let data = Io.read_whole_file (Absolute_path.of_string temp_file) in 
    let _ = Unix_command.conditional_multiple_uc after in 
    let temp1 = Lines_in_text.lines data in 
    let prefix = "MediaBox:" in 
    let temp2 = List.find (String.starts_with ~prefix) temp1 in
    let temp3 = Cull_string.two_sided_cutting (prefix,"") temp2 in
    Str.split (Str.regexp "[ \t\r]+") temp3  ;;

  let sizes_for_each_page ap = 
    let n = number_of_pages_in_pdf ap in 
     let data =  Int_range.scale (mediabox_in_pdf ap) 1 n in 
     let zero = "0.000000" in
     match List.find_opt (
      fun l->(List.nth l 0,List.nth l 1)<>(zero,zero)
     ) data with 
     (Some l)->
       let nt = List.nth l in  
       raise(Shifted_mediabox(nt 0,nt 1,nt 2,nt 3))
     |None ->
      Image.image 
        (fun l->float_of_string(List.nth l 2),float_of_string(List.nth l 3)) data  
       ;; 

  let average_page_width_and_height ap =
     let n = number_of_pages_in_pdf ap in 
     let data =  Int_range.scale (mediabox_in_pdf ap) 1 n in 
     let zero = "0.000000" in
     match List.find_opt (
      fun l->(List.nth l 0,List.nth l 1)<>(zero,zero)
     ) data with 
     (Some l)->
       let nt = List.nth l in  
       raise(Shifted_mediabox(nt 0,nt 1,nt 2,nt 3))
     |None ->
      let widths=Image.image 
        (fun l->float_of_string(List.nth l 2)) data  
      and heights=Image.image 
        (fun l->float_of_string(List.nth l 3)) data in 
      let float_n=float_of_int n in 
      let take_average = (fun l->
        (List.fold_left (+.) 0. l)/.(float_n)
      ) in   
      (take_average widths,take_average heights)    ;;

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

    let corep_cuttable_transform onsite_input outputfile_name padded_nbr= 
    let q = (padded_nbr/8) in
    let corep_cuttable_order = List.flatten (Int_range.scale (fun j->
        Image.image (fun r->2*j+r) 
        [2*q-1;-1;6*q-1;4*q-1;
         0;2*q;4*q;6*q]
      ) 1 q) in 
    (pad_up_to_multiple onsite_input  8 "padded")::
    (explode  "padded" "page")::
    (
     [
       (implode "page" "reaggregated" corep_cuttable_order);
       ("cpdf -impose-xy \"2 2\" -impose-margin 15 reaggregated.pdf -o "^outputfile_name^".pdf");
       "rm initial_copy.pdf page*.pdf padded.pdf reaggregated.pdf";
     ]
    );; 


    let corep_foldable_transform onsite_input outputfile_name padded_nbr= 
    let q = (padded_nbr/8) in
    let corep_order = List.flatten (Int_range.scale (fun j->
        Image.image (fun r->8*j+r) [2;3;6;7;4;1;8;5]
      ) 0 (q-1)) in 
    (pad_up_to_multiple onsite_input  8 "padded")::
    (explode  "padded" "page")::
    (
     [
       (implode "page" "reaggregated" corep_order);
       ("cpdf -impose-xy \"2 2\" -impose-margin 15 reaggregated.pdf -o "^outputfile_name^".pdf");
       "rm initial_copy.pdf page*.pdf padded.pdf reaggregated.pdf";
     ]
    );; 

   let force_same_size_for_all_pages 
     onsite_input outputfile_name ~forced_width ~forced_height=  
    let sizes = "\""^(string_of_int forced_width)^"pt " 
                     ^(string_of_int forced_height)^"pt\"" in 

     [
       
       ("cpdf -scale-to-fit "^sizes^" "^onsite_input^".pdf -o "
            ^outputfile_name^".pdf");
       "rm initial_copy.pdf";
     ]
    ;; 

   let replace_inside total_nbr_of_pages left_of_cut right_of_cut outputfile_name= 
      let left_part = (
         if (left_of_cut<0) then ("",[]) else 
          ("left.pdf",
          ["cpdf initial_copy.pdf 1-"^(string_of_int left_of_cut)^
          " -o left.pdf"]
          )
      ) 
      and right_part = (
         if (right_of_cut>total_nbr_of_pages) then ("",[]) else 
          ("right.pdf",["cpdf initial_copy.pdf "^
          (string_of_int right_of_cut)^"-"^
          (string_of_int total_nbr_of_pages)^
          " -o right.pdf"])
      ) in
      
      (snd left_part)@
      (snd right_part)@
   [
    "cpdf "^(fst left_part)^" replacer_copy.pdf "^(fst right_part)^" -o "^outputfile_name^".pdf";
    "rm "^(fst left_part)^" "^(fst right_part)^" initial_copy.pdf replacer_copy.pdf"
    ] ;;

end ;;  


module Command = struct 
  
  let corep_cuttable_transform ap outputfile_name = 
    let original_nbr = Private.number_of_pages_in_pdf ap in 
    let padded_nbr = (Basic.frac_ceiling original_nbr 8)*8 in 
    let current_dir = Sys.getcwd () in 
   ("cd "^ Private.work_path) :: 
   ("cp "^(Absolute_path.to_string ap)^" initial_copy.pdf") ::
   (OnSiteCommand.corep_cuttable_transform "initial_copy" outputfile_name padded_nbr) @
    ["cd "^current_dir];;
  let corep_foldable_transform ap outputfile_name = 
    let original_nbr = Private.number_of_pages_in_pdf ap in 
    let padded_nbr = (Basic.frac_ceiling original_nbr 8)*8 in 
    let current_dir = Sys.getcwd () in 
   ("cd "^ Private.work_path) :: 
   ("cp "^(Absolute_path.to_string ap)^" initial_copy.pdf") ::
   (OnSiteCommand.corep_foldable_transform "initial_copy" outputfile_name padded_nbr) @
    ["cd "^current_dir];;

  let force_same_size_for_all_pages ap outputfile_name 
         ~forced_width ~forced_height=  
    
    let current_dir = Sys.getcwd () in 
   ("cd "^ Private.work_path) :: 
   ("cp "^(Absolute_path.to_string ap)^" initial_copy.pdf") ::
   (OnSiteCommand.force_same_size_for_all_pages "initial_copy" outputfile_name ~forced_width ~forced_height) @
    ["cd "^current_dir];; 

  let replace_inside 
    ~patient:patient_ap ~replacer:replacer_ap
   ~left_of_cut ~right_of_cut outputfile_name = 
   let current_dir = Sys.getcwd () in 
   let total_nbr_of_pages = Private.number_of_pages_in_pdf patient_ap in 
   ("cd "^ Private.work_path) :: 
   ("cp "^(Absolute_path.to_string patient_ap)^" initial_copy.pdf") ::
   ("cp "^(Absolute_path.to_string replacer_ap)^" replacer_copy.pdf") ::
   (OnSiteCommand.replace_inside total_nbr_of_pages left_of_cut right_of_cut outputfile_name) @
    ["cd "^current_dir];; 

end ;;  

let average_page_width_and_height = 
    Private.average_page_width_and_height ;;

let corep_cuttable_transform ap ~outputfile_name= 
    Unix_command.conditional_multiple_uc 
     (Command.corep_cuttable_transform ap outputfile_name) ;;    
let corep_foldable_transform ap ~outputfile_name= 
   Unix_command.conditional_multiple_uc 
    (Command.corep_foldable_transform ap outputfile_name) ;;

let force_same_size_for_all_pages ap ~outputfile_name
    ~forced_width ~forced_height= 
   Unix_command.conditional_multiple_uc 
    (Command.force_same_size_for_all_pages ap outputfile_name
        ~forced_width ~forced_height) ;;

let number_of_pages_in_pdf = 
    Private.number_of_pages_in_pdf ;;
      
let replace_inside 
    ~patient:patient_ap ~replacer:replacer_ap
   ~left_of_cut ~right_of_cut ~outputfile_name = 
    Unix_command.conditional_multiple_uc  (
     Command.replace_inside 
    ~patient:patient_ap ~replacer:replacer_ap
   ~left_of_cut ~right_of_cut outputfile_name
    ) ;;

let sizes_for_each_page = Private.sizes_for_each_page ;;    