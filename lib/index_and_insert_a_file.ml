(*

#use"lib/index_and_insert_a_file.ml";;

*)


exception Detox_exn of string list ;; 

exception  Unique_olavian_file_inside of string list ;; 

module Private = struct 

   type short_or_long = Short | Long ;;
    

   let unique_olavian_file_inside dir = 
      let files_inside = Unix_again.beheaded_simple_ls dir in 
      let temp = List.filter (
        fun fn -> (Supstring.contains fn "Olavo")
              &&
              (List.exists (fun edg->Supstring.ends_with fn edg) [".mkv";".mp4";".webm"])
        ) files_inside in 
      if List.length(temp)<>1 
      then  raise(Unique_olavian_file_inside(temp))  
      else List.hd temp;;    
    
   let dry_detox_result dir filename = 
      let full_filename = (Directory_name.connectable_to_subpath dir)^filename in 
      let temp_ap = Absolute_path.create_file_if_absent "frimframsauce.txt" in
      let s_temp_ap = Absolute_path.to_string temp_ap in  
      let cmd2 = Filename.quote_command "detox" ["-n"; full_filename] ~stdout:s_temp_ap in
      let _ = Sys.command cmd2 in 
      let output = Io.read_whole_file temp_ap in
      let _ = Sys.command (Filename.quote_command "rm" [s_temp_ap]) in 
      let opt1 = Cull_string.before_and_after " -> " output in  
      let new_full_name =Cull_string.trim_spaces(snd(Option.get opt1)) in 
      Cull_string.after_rightmost new_full_name '/' ;;

   let detox_related dir =
        let wild_fn = unique_olavian_file_inside dir in 
        let tamed_fn = dry_detox_result dir wild_fn in 
        (wild_fn,tamed_fn) ;; 
     


   let downloads_dir = Directory_name.of_string ("~/Downloads");;     
   let dir_for_short_files () = Directory_name.of_string "/Volumes/Matroska/Video/Olavo/";; 
   let dir_for_long_files () = Directory_name.of_string "/Volumes/Matroska/Video/Olavo/Longer_videos";; 
   
  let directory_for_size = function 
      Short -> dir_for_short_files () 
      |Long -> dir_for_long_files () ;; 

   let char_is_not_a_digit c = 
      let i = int_of_char c in 
      (i<48)||(i>57) ;; 
  
   let sl_of_char_opt c = List.assoc_opt c 
    ['S',Short;'L',Long;] ;;
      
  let detect_indexed_file fn =
     if String.length(fn)<6 then None else 
     if (List.exists (fun k->char_is_not_a_digit(Strung.get fn k)) [2;3;4;5])
        ||
        ((Strung.get fn 6)<>'_')
     then None else 
      match sl_of_char_opt (Strung.get fn 1) with 
      None -> None 
      |Some s_or_l -> 
     Some(s_or_l,
      int_of_string(Cull_string.interval fn 2 5),
      Cull_string.cobeginning 6 fn) ;;  
  
   let order_for_intstring_pairs =
        Total_ordering.product Total_ordering.for_integers Total_ordering.lex_for_strings ;; 


   let ref_for_mistakes = ref ([],[]) ;; 

  let warn found_files indexed_data msg = 
     let _ = (
        ref_for_mistakes:=(found_files,indexed_data);
        print_string ("\n\n\n"^msg^"\n"^
        " See In"^"dex_and_insert_a_file.Private.ref_for_mistakes for more details\n\n\n");
        flush stdout) in None ;; 
  
     let index_analysis_before_insertion s_or_l =
      let dir = directory_for_size s_or_l in  
      let temp1 = Unix_again.beheaded_simple_ls dir in 
      let indexed_data = List.filter_map detect_indexed_file temp1 in 
      if indexed_data = [] 
      then warn temp1 indexed_data "Failure : No indexed files" 
      else 
      let unordered_indices = Image.image (fun (_s_or_l,idx,_end_fn)->idx ) indexed_data in 
      let indices = Ordered.sort Total_ordering.for_integers unordered_indices in 
      let m = List.length indices in 
      if (indices<>Int_range.range 1 m)||(List.length(unordered_indices)<>m) 
      then warn temp1 indexed_data "Failure: Indexes do not form an initial segment"
      else   
      if List.exists(fun (s_or_l1,_idx,_end_fn)->s_or_l1<>s_or_l) indexed_data 
      then warn temp1 indexed_data  "Failure : Short files and long files are mixed" 
      else 
      let unordered_pairs = Image.image (fun (_s_or_l,idx,end_fn)->(idx,end_fn)) indexed_data in 
      Some (Ordered.sort order_for_intstring_pairs unordered_pairs) ;;  
  
   let basename_of_dir dir =
       let s_dir = Directory_name.connectable_to_subpath dir in 
       let without_the_trailing_slash = Cull_string.coending 1 s_dir in  
       without_the_trailing_slash ;; 

   let conventional_name (s_or_l,idx,end_fn) = 
     let dir = directory_for_size s_or_l 
     and s_or_l_str = List.assoc s_or_l 
       [Short,"S";Long,"L"] in  
      (Directory_name.connectable_to_subpath dir)^s_or_l_str^(
      Strung.insert_repetitive_offset_on_the_left '0' 4   
      (string_of_int idx))^"_"^end_fn ;;   

   

   let commands_for_insertion_of_old_files extraction_dir s_or_l = 
      let (wild_fn,tamed_fn) = detox_related extraction_dir in 
      match index_analysis_before_insertion s_or_l with 
       None -> None 
      |Some indexed_data ->  
      let s_source = Directory_name.connectable_to_subpath extraction_dir  in 
      let fst_command = 
         Filename.quote_command "mv" [s_source^wild_fn;
         conventional_name (s_or_l,1,tamed_fn)]
      and other_commands= Image.image(fun (idx,end_fn)->
           let tr= (fun j->conventional_name (s_or_l,j,end_fn)) in 
           "mv "^(tr idx)^" "^(tr (idx+1))  
         ) indexed_data in 
         Some(fst_command ::other_commands) ;;    
  
     
     let commands_for_insertion_of_recent_files extraction_dir s_or_l = 
      let (wild_fn,tamed_fn) = detox_related extraction_dir in 
      match index_analysis_before_insertion s_or_l with 
       None -> None 
      |Some indexed_data ->  
      let m = List.length(indexed_data)+1  in 
      let s_dir = Directory_name.connectable_to_subpath extraction_dir  in 
      let fst_command = Filename.quote_command "mv" 
         [s_dir^wild_fn;conventional_name (s_or_l,m,tamed_fn)]  in 
      Some([fst_command]) ;;    
    
      let do_insertion_of_old_files extraction_dir s_or_l = 
         match  commands_for_insertion_of_old_files extraction_dir s_or_l with 
          None -> []
         |Some l -> Image.image Unix_command.hardcore_uc l ;;   
     
    let do_insertion_of_recent_files extraction_dir s_or_l = 
     match  commands_for_insertion_of_recent_files extraction_dir s_or_l with 
      None -> []
     |Some l -> Image.image Unix_command.hardcore_uc l ;; 
  

     let dict_order=(fun (i1,str1) (i2,str2) ->
      let trial1 = Total_ordering.lex_for_strings str1 str2 in 
      if trial1<>Total_ordering_result_t.Equal then trial1 else
      Total_ordering.for_integers i1 i2  
  ) ;; 
  
  let list_files_in_alphabetical_order  s_or_l = 
    let temp1 = Option.get(index_analysis_before_insertion s_or_l) in 
    let temp2 = Ordered.sort dict_order temp1 in 
    let lines = Image.image (
      fun (i,str)-> (String.make 4 ' ')^str^"  ["^(string_of_int i)^"]"
    ) temp2 in 
    let full_text = "\n\n\n"^(String.concat "\n" lines)^"\n\n\n" in 
    print_string full_text ;; 
  
let list_long_files () = list_files_in_alphabetical_order Long ;;   
let list_short_files () = list_files_in_alphabetical_order Short ;; 
let long_file  () = 
   let _ = (print_string"\n Long files are always old ... \n";flush stdout) in
   do_insertion_of_old_files downloads_dir Long ;; 
 
let short_old_file   () = do_insertion_of_old_files downloads_dir Short ;; 
let short_recent_file () = do_insertion_of_recent_files downloads_dir Short ;;



end ;;   

let list_long_files = Private.list_long_files ;; 
let list_short_files = Private.list_short_files ;; 
let long_file = Private.long_file ;; 
let short_old_file = Private.short_old_file ;; 
let short_recent_file = Private.short_recent_file ;; 