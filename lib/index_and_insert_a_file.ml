(*

#use"lib/index_and_insert_a_file.ml";;

*)

module Private = struct 

   let char_is_not_a_digit c = 
      let i = int_of_char c in 
      (i<48)||(i>57) ;; 
  
  let detect_indexed_file fn =
     if String.length(fn)<6 then None else 
     if (List.exists (fun k->char_is_not_a_digit(Strung.get fn k)) [2;3;4;5])
        ||
        (not(List.mem (Strung.get fn 1) ['S';'L']))
        ||
        ((Strung.get fn 6)<>'_')
     then None else 
     Some(
      Cull_string.interval fn 1 1,
      int_of_string(Cull_string.interval fn 2 5),
      Cull_string.cobeginning 6 fn) ;;  
  
  let warn msg =
     let _ = (print_string ("\n\n\n"^msg^"\n\n\n");flush stdout) in None ;; 
  
  let common_analysis_before_insertion dir = 
      let temp1 = More_unix.simple_ls dir in 
      let temp2 = List.filter (fun ap->not(More_unix.is_a_directory ap)) temp1 in 
      let temp3 = Image.image (fun ap->
        let s_ap = Absolute_path.to_string ap in 
        let (basename,fn) = Cull_string.split_wrt_rightmost s_ap '/' in 
        (basename,fn,detect_indexed_file fn)  
      ) temp2 in
      let temp4 = List.filter (
        fun (_basename,fn,_opt)->fn <> ".DS_Store"
      ) temp3 in 
      let (nonindexed,pre_indexed) = List.partition (
          fun (_basename,_fn,opt)->opt = None
      ) temp4 in 
      if nonindexed = [] then warn "Failure : All files are indexed" else 
      if List.length(nonindexed)>1 then warn "Failure : Too many non-indexed files" else 
      let indexed = Image.image (
        fun (basename,_,opt)-> 
           let (s_or_l,idx,end_fn) = Option.get opt in 
          (basename,s_or_l,idx,end_fn)
      ) pre_indexed in 
      let temp5 = Image.image (fun (_basename,_s_or_l,idx,_end_fn)->idx ) indexed in 
      let m = List.length temp5 in 
      if temp5<>Int_range.range 1 m 
      then warn "Failure: Indexes do not form an initial segment"
      else   
      let temp6 = Image.image (fun (_basename,s_or_l,_idx,_end_fn)->s_or_l ) indexed in 
      let s_or_l0=List.hd temp6 in 
      if List.exists(fun t->t<>s_or_l0) temp6 then warn "Failure : Short files and long files are mixed" else 
      let (basename0,fn0,_) = List.hd nonindexed in 
      Some(basename0,fn0,s_or_l0,Image.image (
        fun (basename,_s_or_l,idx,end_fn)->(basename,idx,end_fn)
      ) indexed) ;;  
  
  let conventional_name (basename,s_or_l,end_fn) idx=
  basename^"/"^s_or_l^(
    Strung.insert_repetitive_offset_on_the_left '0' 4   
  (string_of_int idx))^"_"^end_fn ;;   
  
  
  let commands_for_upwards_insertion dir =
    match common_analysis_before_insertion dir with 
    None -> None 
    |Some  (basename0,fn0,s_or_l0,indexed) ->  
     let fst_command = "mv "^basename0^"/"^fn0^" "^(conventional_name (basename0,s_or_l0,fn0) 1) 
     and other_commands= Image.image(fun (basename,idx,end_fn)->
      let tr= (basename,s_or_l0,end_fn) in 
      "mv "^(conventional_name tr idx)^" "^(conventional_name tr (idx+1))  
    ) indexed in 
    Some(fst_command :: other_commands) ;; 
  
  let do_upwards_insertion dir =
     match  commands_for_upwards_insertion dir with 
      None -> []
     |Some l -> Image.image Unix_command.hardcore_uc l ;; 
  
  let command_for_downwards_insertion dir =
      match common_analysis_before_insertion dir with 
      None -> None 
      |Some  (basename0,fn0,s_or_l0,indexed) ->  
        let m = List.length(indexed)+1 in 
       let command 
        = "mv "^basename0^"/"^fn0^" "^(conventional_name (basename0,s_or_l0,fn0) m) in
      Some(command) ;; 
  
  let do_downwards_insertion dir =
        match  command_for_downwards_insertion dir with 
         None -> (-2023)
        |Some cmd -> Unix_command.hardcore_uc cmd ;;     
  
  let usual_dir_for_short_files () = Directory_name.of_string "/Volumes/Matroska/Video/Olavo/";; 
  let usual_dir_for_long_files () = Directory_name.of_string "/Volumes/Matroska/Video/Olavo/Longer_videos";; 
  
let long_downwards  () = do_downwards_insertion (usual_dir_for_long_files()) ;; 
let long_upwards    () = do_upwards_insertion (usual_dir_for_long_files()) ;; 
let short_downwards () = do_downwards_insertion (usual_dir_for_short_files()) ;; 
let short_upwards   () = do_upwards_insertion (usual_dir_for_short_files()) ;; 

end ;;   

let long_downwards = Private.long_downwards ;; 
let long_upwards = Private.long_upwards ;; 
let short_downwards = Private.short_downwards ;; 
let short_upwards = Private.short_upwards ;; 