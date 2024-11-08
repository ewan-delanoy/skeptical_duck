(*

#use"lib/Text_editing/coherent_pdf.ml";;

*)

module Command = struct 

  module Private = struct 
        let home = Sys.getenv "HOME" ;;

        let work_path = 
          home ^ 
        "/Teuliou/OCaml/skeptical_duck/nonml_files/"^
        "nongithubbed_nonml_files/pdf_workspace/" ;;


  end ;;  

  



  let number_of_pages_in_pdf ap = 
    let temp_file = Private.work_path ^ "temp.txt" in 
    (
      temp_file,  
    [
       "touch "^temp_file;
       "cpdf -info "^(Absolute_path.to_string ap)^" > "^temp_file
    ],
     [
       "rm "^temp_file  
     ]);;
  
  let noncommand_number_of_pages_in_pdf ap = 
    let (temp_file,before,after) = 
      number_of_pages_in_pdf ap in 
    let _ = Unix_command.conditional_multiple_uc before in 
    let data = Io.read_whole_file (Absolute_path.of_string temp_file) in 
    let _ = Unix_command.conditional_multiple_uc after in 
    let temp1 = Lines_in_string.lines data in 
    let prefix = "Pages:" in 
    let temp2 = List.find (String.starts_with ~prefix) temp1 in
    let temp3 = Cull_string.two_sided_cutting (prefix,"") temp2 in
    int_of_string (Cull_string.trim_spaces temp3)  ;;    
  
  let explode_range ap i j= 
    let extract = (function k->
      let sk = string_of_int k in 
      "cpdf whole.pdf "^sk^"-"^sk^" -o p"^sk^".pdf" 
    ) in 
    let current_dir = Sys.getcwd () in 
   ("cd "^ Private.work_path) :: 
   ("cp "^(Absolute_path.to_string ap)^" whole.pdf") :: 
   (Int_range.scale extract i j) @
    ["cd "^current_dir];;
     

  end ;;  

let explode_range ap i j = 
  Unix_command.conditional_multiple_uc 
   (Command.explode_range ap i j) ;; 

let number_of_pages_in_pdf = 
    Command.noncommand_number_of_pages_in_pdf ;;
      