(*

#use"lib/index_and_insert_a_file.ml";;

*)

type short_or_long = Short | Long ;;

module Private = struct 

   let is_a_harmless_ascii_character c = 
      let i = int_of_char c in 
     if  (i<32)||(i>126) then false else 
     not(List.mem c [' ';'"';'$';'%'; '&';'\'';'?';'^';'`';'|';]) ;; 
  
   let is_a_harmless_filename fn =
       let n = String.length fn in 
       List.for_all (fun k->
         is_a_harmless_ascii_character(String.get fn (k-1))) 
            (Int_range.range 1 n) ;;

   let admissible_files_inside dir =
       let files_inside = Unix_again.beheaded_simple_ls dir in 
       List.filter (
         fun fn ->
            if (not (is_a_harmless_filename fn))
               ||
               (not(String.contains fn '.'))
            then false    
            else
              let ending = Cull_string.after_rightmost fn '.' in 
              List.mem ending ["mkv";"mp4";"webm"] 
       ) files_inside ;; 

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

   let commands_for_upwards_insertion_for_inserted_file inserted_one s_or_l = 
      match index_analysis_before_insertion s_or_l with 
       None -> None 
      |Some indexed_data ->  
      let short_fn = Cull_string.after_rightmost inserted_one '/'   in 
      let fst_command = "mv "^inserted_one^" "^(conventional_name (s_or_l,1,short_fn)) 
      and other_commands= Image.image(fun (idx,end_fn)->
           let tr= (fun j->conventional_name (s_or_l,j,end_fn)) in 
           "mv "^(tr idx)^" "^(tr (idx+1))  
         ) indexed_data in 
         Some(fst_command :: other_commands) ;;    

  
  let commands_for_upwards_insertion extraction_dir s_or_l = 
    let admissible_files = admissible_files_inside extraction_dir in 
    let nbr_of_admissible_files = List.length  admissible_files in 
    if nbr_of_admissible_files < 1 then warn "Failure : No admissible files" else 
    if nbr_of_admissible_files > 1 then warn "Failure : Nonunique admissible file" else   
   let inserted_one = (Directory_name.connectable_to_subpath extraction_dir)^(List.hd admissible_files) in 
   commands_for_upwards_insertion_for_inserted_file inserted_one s_or_l ;; 
     
   let do_upwards_insertion extraction_dir s_or_l = 
     match  commands_for_upwards_insertion extraction_dir s_or_l with 
      None -> []
     |Some l -> Image.image Unix_command.hardcore_uc l ;; 
  
     
     let commands_for_downwards_insertion_for_inserted_file inserted_one s_or_l = 
      match index_analysis_before_insertion s_or_l with 
       None -> None 
      |Some indexed_data ->  
      let m = List.length(indexed_data)+1   
      and short_fn = Cull_string.after_rightmost inserted_one '/'   in 
      let fst_command = "mv "^inserted_one^" "^(conventional_name (s_or_l,m,short_fn)) in 
      Some([fst_command]) ;;    
    
    
    let commands_for_downwards_insertion extraction_dir s_or_l = 
    let admissible_files = admissible_files_inside extraction_dir in 
    let nbr_of_admissible_files = List.length  admissible_files in 
    if nbr_of_admissible_files < 1 then warn "Failure : No admissible files" else 
    if nbr_of_admissible_files > 1 then warn "Failure : Nonunique admissible file" else   
    let inserted_one = (Directory_name.connectable_to_subpath extraction_dir)^(List.hd admissible_files) in 
    commands_for_downwards_insertion_for_inserted_file inserted_one s_or_l ;; 
     
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