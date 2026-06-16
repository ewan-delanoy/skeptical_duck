(*

#use"lib/Text_editing/executable_coherent_pdf.ml";;

*)



module Private = struct 

let denounce msg =   let _ = Basic.display_message ( msg) in failwith("Error") ;; 


let denounce_bad_integer_list str bad_parts =
   let b = List.length bad_parts in 
   if b=0 then () else 
   let msg1 = "\""^str ^ "\" is not a valid list of integers : " in 
   let msg2 = (
      if b=1 
      then (List.hd bad_parts)^" is not a valid integer"
      else let (last_part,other_parts) = List_again.head_with_tail (List.rev bad_parts)  in 
           let first_parts = List.rev other_parts in 
           (String.concat "," first_parts)^" and "^(last_part)^" are not valid integers"
   ) in 
   denounce ( msg1 ^ msg2) ;; 


let message_for_missing_rectifications missing_rectifs =
   let m = List.length missing_rectifs in 
   if m=0 then "" else 
   if m=1 
   then "There is no rectification for page number "^(string_of_int(List.hd missing_rectifs))
   else let (last_part,other_parts) = List_again.head_with_tail (List.rev missing_rectifs)  in 
        let first_parts = List.rev other_parts in 
        "There are no rectifications for pages number "^
        (String.concat "," (Image.image string_of_int first_parts))^" and "^(string_of_int(last_part))^"." ;;
   
let message_for_unclear_rectification (i,realizers) = 
   let (last_part,other_parts) = List_again.head_with_tail (List.rev realizers)  in 
   let first_parts = List.rev other_parts in 
   "There are several proposed rectifications for page number "^(string_of_int i)^" : "^
   (String.concat ", " first_parts)^" and "^(last_part)^"." ;;

let message_for_unclear_rectifications unclear_rectifs =
   (String.concat "\n" (Image.image message_for_unclear_rectification unclear_rectifs)) ;;


let denounce_rectification_problems missing_rectifs unclear_rectifs = 
   let msg1 = message_for_missing_rectifications missing_rectifs
   and msg2 = message_for_unclear_rectifications unclear_rectifs in 
   if (msg1,msg2)=("","") then () else
   let msg = "The follwing problems were encountered with rectification files :\n"^msg1^"\n"^msg2 in 
   denounce msg  ;;     


(*

denounce_rectification_problems [2;7;50] [3,["peggy";"lee"];20,["george";"walker";"bush"];40,["hannah";"pearl";"davis"]]

*)

let parse_list_of_integers str =
   let parts = Str.split (Str.regexp ",") str in 
   let analysis_result = Image.image (fun part->(part,int_of_string_opt part)) parts in 
   let bad_parts = List.filter_map (fun (part,i_opt)->if i_opt=None then Some part else None) analysis_result in 
   let _ = denounce_bad_integer_list  str bad_parts in 
   Image.image (fun p->Option.get(snd p)) analysis_result;;

(*

parse_list_of_integers "2,abc,13,d,47,efg,58,jkl,69"

*)

let decompose_according_to_number_ending_opt filename = 
   if not(String.ends_with filename ~suffix:".pdf") then None else 
   let n = String.length filename in 
   let m = n -4 in 
   if Charset.is_not_a_digit (Strung.get filename m) then None else 
   match String_find_char.backwards_from_inclusive_opt Charset.is_not_a_digit filename m with 
   None -> None 
   |Some j0 -> Some (
      (Cull_string.beginning j0 filename,int_of_string(Cull_string.interval filename (j0+1) m)),
      filename) ;;      
 ;;

(*

decompose_according_to_number_ending_opt "amy347" ;;
decompose_according_to_number_ending_opt "amy347.pdf" ;;

*)

let find_rectifications_in_directory dir indices =
   let files = Unix_again.beheaded_simple_ls (Directory_name.of_string dir) in 
   let numbered_files = List.filter_map decompose_according_to_number_ending_opt files in 
   let search_result = Image.image (
     fun i->(i,List.filter_map ( fun ((_,i2),fn)->if i2=i then Some fn else None ) numbered_files)
   ) indices in 
   let missing_rectifs = List.filter (fun (_i,l)->(List.length l)<1) search_result
   and unclear_rectifs = List.filter (fun (_i,l)->(List.length l)>1) search_result in 
   let _ = denounce_rectification_problems (Image.image fst missing_rectifs) unclear_rectifs in 
   Image.image (fun (i,l)->let fn=List.hd l in (i,Cull_string.two_sided_cutting ("",".pdf") fn)) search_result ;;

let find_file dir filename = 
   try Absolute_path.of_string (dir^"/"^filename) with 
   _ -> denounce("There is no file called "^filename^" in "^dir) ;;

let replace_pages_inside command_parameters = 
   if Array.length (command_parameters) < 4
   then denounce ("Some parameters are missing in your command. The format is cepdf -replace <patient-name> <list-of-page-numbers>,\n"^
        "for example : cepdf -replace corrupted.pdf 3,4,21")  
   else
   let dir = Sys.getcwd () in 
   let filename = Array.get command_parameters 2
   and description_of_list_of_indices = Array.get command_parameters 3 in 
   let patient_ap = find_file dir filename
   and indices = parse_list_of_integers description_of_list_of_indices in 
   let rectifs = find_rectifications_in_directory dir indices in 
   let _ =Coherent_pdf.replace_pages_inside ~patient:patient_ap ~page_numbers_with_their_replacements:rectifs ~outputfile_name:filename in 
   ();;

let options_for_main_command =
   [
     "-replace",replace_pages_inside
   ] ;;

let description_of_options_for_main_command=
  "Currently, the available options are : \n" ^ (
    String.concat "," (Image.image fst options_for_main_command)
  ) ^
  ".";;   

let denounce_bad_user_option explanation = 
    denounce (explanation^"\n"^description_of_options_for_main_command) ;;

let main command_parameters = 
   if Array.length (command_parameters) < 4
   then denounce_bad_user_option ("You need to provide an option to cepdf.")  
   else
   let user_option = Array.get command_parameters 1 in 
   match List.assoc_opt user_option options_for_main_command with 
   None -> denounce_bad_user_option ("Unknown option "^user_option^".")  
   |Some(proc) -> proc command_parameters ;;


end ;;

let main = Private.main ;;