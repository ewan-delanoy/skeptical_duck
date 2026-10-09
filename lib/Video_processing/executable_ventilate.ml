(*

#use"lib/Video_processing/executable_ventilate.ml";;

*)



module Private = struct 

let denounce msg =   let _ = Basic.display_message ( msg) in failwith("Error") ;; 

let is_a_digit c = let i = int_of_char c in (48<=i)&&(i<=57) ;;

let extract_numbers text =
   let all_chars = Strung.explode text in 
   let temp = List_again.connected_fibers is_a_digit all_chars in 
   let temp2 = List.filter_map (fun (_,chars,is_a_number) -> if is_a_number then Some chars else None) temp in 
   let temp3 = Image.image (fun chars->int_of_string(Strung.implode chars)) temp2 in 
   temp3;;

(*

extract_numbers "23a678b3ghij71" ;;

*)

let parse_time_naively text = Basic.horner_algorithm (extract_numbers text) 60 ;;

let parse_time text = 
   try parse_time_naively text with 
   _ -> denounce (" Badly formatted time : "^text) ;;

(*

parse_time "2h5m50s" ;;

*)


let commands_for_single_extraction movie_name extra_suffix (time_start,time_end)  =
  let s_start= string_of_int(time_start) 
  and s_end = string_of_int(time_end) in 
  let output_name = movie_name^extra_suffix^"_shortened.mp4" in 
  [
    "ffmpeg -ss "^s_start^" -to "^s_end^" -i "^movie_name^"_raw.mp4 -c copy "^output_name;
    "mv "^output_name^" ../Step2_Shortened/";
  ] ;;

let commands_for_multiple_extraction movie_name time_ranges =
   if List.length(time_ranges) > 1 
   then commands_for_single_extraction movie_name "" (List.hd time_ranges)
   else let indexed_intervals= Int_range.index_everything time_ranges in
        let unjoined_parts = Image.image (
          fun (idx,_time_range) -> "\"$PWD/"^movie_name^"_part"^(string_of_int idx)^"_shortened.mp4\""
        ) indexed_intervals in  
        let parts = String.concat " " unjoined_parts in 
        List.flatten(Image.image (
         fun (idx,time_range) ->
          commands_for_single_extraction movie_name ("_part"^(string_of_int idx)) time_range   
        ) indexed_intervals)
        @
        [
          "cd "^(Sys.getenv "HOME") ^ "/Downloads/Video_recording/Step2_Shortened";
          "printf \"file '%s'\n\" " ^parts^ "  > /tmp/concat_"^movie_name^".txt";
          "ffmpeg -f concat -safe 0 -i /tmp/concat_"^movie_name^".txt -c copy "^movie_name^"_shortened.mp4";
          "rm /tmp/concat_"^movie_name^".txt"
        ]
   ;;


let commands_for_main movie_name time_ranges =
   let old_dir = Sys.getcwd() in 
   let output_name = movie_name^"_compressed.mp4" in 
   [
     "cd "^(Sys.getenv "HOME") ^ "/Downloads/Video_recording/Step1_Raw";
   ] 
   @
    (commands_for_multiple_extraction movie_name time_ranges)
   @
   [
      "cd "^(Sys.getenv "HOME") ^ "/Downloads/Video_recording/Step2_Shortened";
      "ffmpeg -i "^movie_name^"_shortened.mp4 -b:v 1000000 "^output_name;
      "mv "^output_name^" ../Step3_Compressed/";
      "cd "^old_dir;
   ] 
;;
   
let parse_command_parameters parameters = 
   let n = Array.length (parameters) in 
   if (n<3)||(n mod 2=1)
   then denounce ("You need to provide a movie name, followed by several (and at least one) time ranges "^
         "(each time range takes two arguments).")  
   else let q = (n-1)/2 
      and get = (fun t->parse_time(Array.get parameters t)) in 
        (Array.get parameters 0,Int_range.scale(fun j->(get (2*j-1),get(2*j))) 1 q) ;;

let commands_from_parameters parameters= 
    let (movie_name,time_ranges) = parse_command_parameters parameters in 
    commands_for_main movie_name time_ranges ;;

let main args = 
   let commands = commands_from_parameters args in 
   Chronometer.it Unix_command.conditional_multiple_uc commands ;;

end ;;

let main = Private.main ;;

