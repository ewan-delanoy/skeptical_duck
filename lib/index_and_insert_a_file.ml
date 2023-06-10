(*

#use"lib/index_and_insert_a_file.ml";;

*)

type short_or_long = Short | Long ;;

exception Detox_exn of string list ;; 

exception  Unique_olavian_file_inside of string list ;; 

module Private = struct 

     

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
  
  let warn msg =
     let _ = (print_string ("\n\n\n"^msg^"\n\n\n");flush stdout) in None ;; 
  
     let index_analysis_before_insertion s_or_l =
      let dir = directory_for_size s_or_l in  
      let temp1 = Unix_again.beheaded_simple_ls dir in 
      let indexed_data = List.filter_map detect_indexed_file temp1 in 
      if indexed_data = [] then warn "Failure : No indexed files" else 
      let indices = Image.image (fun (_s_or_l,idx,_end_fn)->idx ) indexed_data in 
      let m = List.length indices in 
      if indices<>Int_range.range 1 m 
      then warn "Failure: Indexes do not form an initial segment"
      else   
      if List.exists(fun (s_or_l1,_idx,_end_fn)->s_or_l1<>s_or_l) indexed_data 
      then warn "Failure : Short files and long files are mixed" 
      else Some (Image.image (fun (_s_or_l,idx,end_fn)->(idx,end_fn)) indexed_data) ;;  
  
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

   

   let commands_for_upwards_insertion extraction_dir s_or_l = 
      let (wild_fn,tamed_fn) = detox_related extraction_dir in 
      match index_analysis_before_insertion s_or_l with 
       None -> None 
      |Some indexed_data ->  
      let s_source = Directory_name.connectable_to_subpath extraction_dir  
      and s_dest = Directory_name.connectable_to_subpath (directory_for_size s_or_l) in 
      let fst_command = 
         Filename.quote_command "mv" [s_source^wild_fn;s_dest^tamed_fn]
      and other_commands= Image.image(fun (idx,end_fn)->
           let tr= (fun j->conventional_name (s_or_l,j,end_fn)) in 
           "mv "^(tr idx)^" "^(tr (idx+1))  
         ) indexed_data in 
         Some(fst_command ::other_commands) ;;    
  
     
     let commands_for_downwards_insertion extraction_dir s_or_l = 
      let (wild_fn,tamed_fn) = detox_related extraction_dir in 
      match index_analysis_before_insertion s_or_l with 
       None -> None 
      |Some indexed_data ->  
      let m = List.length(indexed_data)+1  in 
      let s_dir = Directory_name.connectable_to_subpath extraction_dir  in 
      let fst_command = Filename.quote_command "mv" 
         [s_dir^wild_fn;conventional_name (s_or_l,m,tamed_fn)]  in 
      Some([fst_command]) ;;    
    
      let do_upwards_insertion extraction_dir s_or_l = 
         match  commands_for_upwards_insertion extraction_dir s_or_l with 
          None -> []
         |Some l -> Image.image Unix_command.hardcore_uc l ;;   
     
    let do_downwards_insertion extraction_dir s_or_l = 
     match  commands_for_downwards_insertion extraction_dir s_or_l with 
      None -> []
     |Some l -> Image.image Unix_command.hardcore_uc l ;; 
  



let long  () = do_upwards_insertion downloads_dir Long ;; 
let short_downwards () = do_downwards_insertion downloads_dir Short ;; 
let short_upwards   () = do_upwards_insertion downloads_dir Short ;; 


end ;;   



let long = Private.long ;; 
let short_downwards = Private.short_downwards ;; 
let short_upwards = Private.short_upwards ;; 