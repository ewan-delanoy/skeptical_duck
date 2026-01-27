(*
#use"lib/Cee_language/cee_snapshot.ml";;
*)


    type t = Sn of Cee_snapshot_parameters_t.t ;;

    let str_order = Total_ordering.lex_for_strings ;; 
    let str_sort = Ordered.sort str_order ;;
    let str_setminus = Ordered.setminus str_order ;;

    let conventional_snapshot_location root idx suffix=
    let s_idx = Strung.insert_repetitive_offset_on_the_left
      '0' 3 (string_of_int idx) in 
     (Directory_name.connectable_to_subpath root)^
     "snapshot-"^s_idx^"-"^suffix ;;

    let root (Sn snap) = snap.Cee_snapshot_parameters_t.root ;;
    let suffix (Sn snap) = snap.Cee_snapshot_parameters_t.suffix_for_snapshots ;;
    let index (Sn snap) = snap.Cee_snapshot_parameters_t.index ;;
    
    let source snap = 
       Directory_name.of_string(
        conventional_snapshot_location (root snap) (index snap) (suffix snap)
      );;
     
    let destination snap = 
      let s_dir = conventional_snapshot_location (root snap) ((index snap)+1) (suffix snap) in 
      let _ = (if not(Sys.file_exists s_dir)
      then   Sys.command ("mkdir -p "^s_dir) else 0) in 
      Directory_name.of_string(
       s_dir
      ) ;;
    
     
    let hashtbl_for_file_reading = 
      (Hashtbl.create 4000: (t * string,string) Hashtbl.t) ;; 

    let read_file snap fn =
      match Hashtbl.find_opt hashtbl_for_file_reading (snap,fn) with
      | Some old_answer -> old_answer
      | None ->
        let src_dir = Directory_name.connectable_to_subpath (source snap) in
        let ap = Absolute_path.of_string (src_dir ^ fn) in
        let text = Io.read_whole_file ap in
        let _ = Hashtbl.add hashtbl_for_file_reading (snap,fn) text in
        text
    ;;


    
let hashtbl_for_commands = 
  (Hashtbl.create 20: (t,Cee_compilation_command_t.t list) Hashtbl.t) ;; 

let commands snap = 
    match Hashtbl.find_opt hashtbl_for_commands snap with 
  (Some old_answer) -> old_answer 
  | None ->
   let temp1 = read_file snap "Makefile" in 
   let mkf1 = Makefile.parse (Makefile_t.MT temp1) in 
   let (_,cmds_and_echoes) = Makefile.prerequisites_and_commands_for_target 
   (ref mkf1) "all" in
   let cmds = List.filter (fun line->
    not(String.starts_with line ~prefix:"@echo")) cmds_and_echoes in 
   let answer = Image.image (Cee_compilation_command.parse (source snap)) cmds in 
   let _ = Hashtbl.add hashtbl_for_commands snap answer in 
      answer ;; 


let hashtbl_for_all_h_or_c_files = 
  (Hashtbl.create 20: (t,string list) Hashtbl.t) ;; 

let all_h_or_c_files snap = 
    match Hashtbl.find_opt hashtbl_for_all_h_or_c_files snap with 
  (Some old_answer) -> old_answer 
  | None ->
    let src = Directory_name.connectable_to_subpath (source snap) in
      let temp1 = Unix_again.quick_beheaded_complete_ls src in
      let answer =  str_sort
        (List.filter
           (fun fn ->
             List.exists
               (fun edg -> String.ends_with fn ~suffix:edg)
               [ ".h"; ".c"; ".macros" ])
           temp1) in 
    let _ = Hashtbl.add hashtbl_for_all_h_or_c_files snap answer in 
      answer ;; 
let hashtbl_for_separate_commands = 
  (Hashtbl.create 20: (t,Cee_compilation_command_t.separate_t list) Hashtbl.t) ;; 

let separate_commands snap = 
    match Hashtbl.find_opt hashtbl_for_separate_commands snap with 
  (Some old_answer) -> old_answer 
  | None ->
    let answer =  List.filter_map
        (function
          | Cee_compilation_command_t.Batch _ -> None
          | Cee_compilation_command_t.Separate s -> Some s)
    (commands snap) in 
    let _ = Hashtbl.add hashtbl_for_separate_commands snap answer in 
    answer ;; 

    let hashtbl_for_directly_compiled_files = 
      (Hashtbl.create 20: (t,string list) Hashtbl.t) ;; 

    let directly_compiled_files snap = 
       match Hashtbl.find_opt hashtbl_for_directly_compiled_files snap with 
      (Some old_answer) -> old_answer 
      | None ->
      let answer = str_sort
        (Image.image
           Cee_compilation_command.short_name_from_separate
           (separate_commands snap)) in 
      let _ = Hashtbl.add hashtbl_for_directly_compiled_files snap answer in 
      answer ;; 

   
    let modify_file snap fn new_content =
      let dest_dir = Directory_name.connectable_to_subpath (destination snap) in
      let ap = Absolute_path.of_string (dest_dir ^ fn) in
      Io.overwrite_with ap new_content
    ;;

    let create_file_in_a_list snap fn 
      ?new_content_description ~is_temporary new_content index_msg=
      let containing_dir = ( 
         if is_temporary 
         then Directory_name.connectable_to_subpath (source snap) 
         else Directory_name.connectable_to_subpath (destination snap) 
      ) in
      let ap = Absolute_path.create_file_if_absent (containing_dir ^ fn) in
      let _ = Io.overwrite_with ap new_content in
      let end_of_msg =
        match new_content_description with
        | None -> ""
        | Some descr -> ", with content " ^ descr
      in
      let durability =(
         if is_temporary then "temporary" else "persistent") in 
      Basic.announce ("Created "^durability^" file  " ^ fn ^ end_of_msg ^ index_msg)
    ;;

    let create_file snap fn ?new_content_description ~is_temporary new_content =
     create_file_in_a_list snap fn 
      ?new_content_description ~is_temporary new_content ""
    ;;
   
  
    


  
   
 
  let reinitialize_destination_directory snap =
    let src = Directory_name.connectable_to_subpath (source snap)
    and slashed_dest = Directory_name.connectable_to_subpath (destination snap) in
    let dest = Cull_string.coending 1 slashed_dest in
    let _ = Unix_command.conditional_multiple_uc
      [ "rm -rf " ^ dest ^ "/*"
      ; "cp -R " ^ src ^ "/* " ^ dest ^ "/"
      ; "cp " ^ src ^ "/.gdbinit " ^ dest ^ "/"
      ] in 
    ()
    ;;
  
  
  let parameters (Sn params) = params ;;

  let take_possession params = Sn params ;;

