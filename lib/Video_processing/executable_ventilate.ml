(*

#use"lib/Video_processing/executable_ventilate.ml";;

*)



module Private = struct 

type use_case =
   Clean of string list
  |Fix_one of string * int * int
  |Fix_several of string * ((int * (int * int)) list)
  |Help
  |Usual of string * ((int * (int * int)) list)
 ;;

let summary_of_options = String.concat "\n" [
  "Possible formats : ";
  "ventilate movieName start1 end1 ... startn endn (n>=1) ";
  "ventilate -clean movieName1 movieName2 ... movieNamen (n>=1)";
  "ventilate -help";
  "ventilate -fix movieName start end";
  "ventilate -fix movieName index1 start1 end1 indexn startn endn (n>=2)";
] ;;

let denounce msg =   let _ = Basic.display_message ( msg) in failwith("Error") ;; 

let is_a_digit c = let i = int_of_char c in (48<=i)&&(i<=57) ;;

let extract_numbers text =
   let all_chars = Strung.explode text in 
   let temp = List_again.connected_fibers is_a_digit all_chars in 
   let temp2 = List.filter_map (fun (_,chars,is_a_number) -> if is_a_number then Some chars else None) temp in 
   if temp2 = []
   then  denounce (" Badly formatted time : "^text)
   else   
   let temp3 = Image.image (fun chars->int_of_string(Strung.implode chars)) temp2 in 
   temp3;;

(*

extract_numbers "23a678b3ghij71" ;;

*)

let parse_time text = Basic.horner_algorithm (extract_numbers text) 60 ;;

(*

parse_time "2h5m50s" ;;

*)

let name_for_raw_version movie_name       = movie_name^"_raw.mp4" ;;
let name_for_part movie_name idx= 
  if idx=0
  then movie_name^"_shortened.mp4" 
  else movie_name^"_part"^(string_of_int idx)^"_shortened.mp4" ;;
let name_for_compressed_version movie_name = movie_name^".mp4" ;;


let commands_for_single_extraction movie_name (idx,(time_start,time_end))  =
  let s_start= string_of_int(time_start) 
  and s_end = string_of_int(time_end) in 
  let output_name = name_for_part movie_name idx in 
  [
    "ffmpeg -ss "^s_start^" -to "^s_end^" -i "^(name_for_raw_version movie_name)^" -c copy "^output_name;
    "mv "^output_name^" ../Step2_Shortened/";
  ] ;;

let commands_for_multiple_extraction movie_name indexed_time_ranges =
   List.flatten(Image.image (
          commands_for_single_extraction movie_name    
   ) indexed_time_ranges);;

let maximum_number_of_parts = 20 ;;

let commands_for_reaggregation reaggregation_needed movie_name =
   if not reaggregation_needed 
   then []
   else   
   let dir = (Sys.getenv "HOME") ^ "/Downloads/Video_recording/Step2_Shortened/" in 
   let unjoined_parts = List.filter_map (
      fun idx -> 
         let part_name = name_for_part movie_name idx in 
         if Sys.file_exists(dir^part_name)
         then Some("\"$PWD/"^(part_name)^"\"")
         else None
   ) (Int_range.range 1 maximum_number_of_parts) in  
   let parts = String.concat " " unjoined_parts in 
   [
      "cd "^dir;
      "printf \"file '%s'\n\" " ^parts^ "  > /tmp/concat_"^movie_name^".txt";
      "ffmpeg -f concat -safe 0 -i /tmp/concat_"^movie_name^".txt -c copy "^(name_for_part movie_name 0);
      "rm /tmp/concat_"^movie_name^".txt"
   ]
   ;;

let commands_for_compression old_dir movie_name =
   let output_name = name_for_compressed_version movie_name in 
   [
      "cd "^(Sys.getenv "HOME") ^ "/Downloads/Video_recording/Step2_Shortened";
      "ffmpeg -i "^movie_name^"_shortened.mp4 -b:v 1000000 "^output_name;
      "mv "^output_name^" ../Step3_Compressed/";
      "cd "^old_dir;
   ] 
;;


let commands_for_usual_case old_dir movie_name indexed_time_ranges =
   let reaggregation_needed = (fst(List.hd(indexed_time_ranges))>0) in 
   [
     "cd "^(Sys.getenv "HOME") ^ "/Downloads/Video_recording/Step1_Raw";
   ] 
   @
    (commands_for_multiple_extraction movie_name indexed_time_ranges)
   @
    (commands_for_reaggregation reaggregation_needed movie_name)
   @
    (commands_for_compression old_dir movie_name)
;;

let commands_for_single_cleanup movie_name =
   [
     "rm -f "^(Sys.getenv "HOME") ^ "/Downloads/Video_recording/Step3_Compressed/"^(name_for_compressed_version movie_name);
     "rm "^(Sys.getenv "HOME") ^ "/Downloads/Video_recording/Step2_Shortened/"^ movie_name ^ "*.mp4";
     "rm "^(Sys.getenv "HOME") ^ "/Downloads/Video_recording/Step1_Raw/"^(name_for_raw_version movie_name);
   ] 
;;

let commands_for_cleanup movie_names=List.flatten(Image.image commands_for_single_cleanup  movie_names) ;;  

let commands_for_fixing old_dir movie_name indexed_time_ranges =
   let reaggregation_needed = (fst(List.hd(indexed_time_ranges))>0) in 
   [
     "rm "^(Sys.getenv "HOME") ^ "/Downloads/Video_recording/Step3_Compressed/"^(name_for_compressed_version movie_name);
   ] 
   @
   (
     Image.image(fun (idx,_)-> "rm "^(Sys.getenv "HOME") ^ 
     "/Downloads/Video_recording/Step2_Shortened/"^ (name_for_part movie_name idx)) indexed_time_ranges 
   )
   @
    (commands_for_multiple_extraction movie_name indexed_time_ranges)
   @
    (commands_for_reaggregation reaggregation_needed movie_name)
   @
    (commands_for_compression old_dir movie_name)
;;



   
let parse_command_parameters parameters = 
   let n = Array.length (parameters) in 
   if n<2 
   then denounce ("Too few arguments. Try ventilate -help for more information.")  
   else       
   let option_name = Array.get parameters 1 in 
   if option_name = "-help" then Help else 
   if option_name = "-clean" then Clean(Int_range.scale (Array.get parameters) 2 (n-1)) else 
   let get = (fun t->Array.get parameters t) in   
   if option_name = "-fix" 
   then if n = 5 
        then Fix_one(get 2,parse_time(get 3),parse_time(get 4))
        else 
        if (n mod 3)<>0
        then denounce ("Wrong number of arguments for fix mode. Try ventilate -help for more information.")  
        else 
            let q=(n/3)-1 in 
            Fix_several(get 2,Int_range.scale (fun k->(int_of_string(get (3*k)),
            (parse_time(get (3*k+1)),parse_time(get (3*k+2))) )) 1 q) 
   else if n = 4 
        then Usual(get 1,[0,(parse_time(get 2),parse_time(get 3))])
        else 
        if (n mod 2)<>0
        then denounce ("Wrong number of arguments for usual mode. Try ventilate -help for more information.")  
        else 
            let q=(n/2)-1 in 
            Usual(get 1,Int_range.scale (fun k->(k,
            (parse_time(get (2*k)),parse_time(get (2*k+1))) )) 1 q) ;;

let commands_for_use_case_opt old_dir = function 
   Clean (movie_names) -> Some(commands_for_cleanup movie_names)
  |Fix_one (movie_name,t_start,t_end) -> Some(commands_for_fixing old_dir movie_name [0,(t_start,t_end)])
  |Fix_several (movie_name,indexed_time_ranges) -> Some(commands_for_fixing old_dir movie_name indexed_time_ranges)
  |Usual (movie_name,indexed_time_ranges) -> Some(commands_for_usual_case old_dir movie_name indexed_time_ranges)
  |Help -> None  ;;

(* this function is for debugging purposes *)
let commands_from_parameters old_dir parameters= 
    Option.get(commands_for_use_case_opt old_dir (parse_command_parameters parameters)) ;;

let main args = 
   match commands_for_use_case_opt (Sys.getcwd()) (parse_command_parameters args) with 
   None -> Basic.display_message summary_of_options 
   |Some commands ->
   let _ = Chronometer.it (Unix_command.conditional_multiple_uc ~display_mode:true) commands  in 
   ();;

end ;;

let main = Private.main ;;

