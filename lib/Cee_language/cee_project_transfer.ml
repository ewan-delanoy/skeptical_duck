(*
   #use"lib/Cee_language/cee_project_transfer.ml";;
*)



module Private2 = struct
  let str_order = Total_ordering.lex_for_strings ;; 
  let str_mem = Ordered.mem str_order ;;

  let il_order = Total_ordering.lex_compare Total_ordering.for_integers;;
  let il_sort = Ordered.sort il_order ;;


   

   module PreSnapshot = struct
    type t = Sn of Cee_snapshot_parameters_t.t ;;
    let str_sort = Ordered.sort str_order
    let str_setminus = Ordered.setminus str_order

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
      let dest_dir = Directory_name.connectable_to_subpath (destination snap) in
      let ap = Absolute_path.create_file_if_absent (dest_dir ^ fn) in
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

  end ;;
end ;;

module type SNAPSHOT_INTERFACE = sig
  type t 

  val commands : t -> Cee_compilation_command_t.t list
  val destination : t -> Directory_name_t.t
  val parameters : t -> Cee_snapshot_parameters_t.t 
  val source : t -> Directory_name_t.t
  val take_possession : Cee_snapshot_parameters_t.t -> t  

  val all_h_or_c_files : t -> string list
  val directly_compiled_files : t -> string list
  val separate_commands : t -> Cee_compilation_command_t.separate_t list
  
  val create_file : t -> string -> ?new_content_description:string -> is_temporary:bool -> string -> unit
  val read_file : t -> string -> string
  val modify_file : t -> string -> string -> unit
  val reinitialize_destination_directory : t -> unit  

end ;;

module Snapshot : SNAPSHOT_INTERFACE = Private2.PreSnapshot

module Private = struct
  let str_order = Total_ordering.lex_for_strings
  let str_mem = Ordered.mem str_order
  let str_sort = Ordered.sort str_order

  let rec parse_double_points_in_filename container_dir included_fn =
    if not (String.starts_with included_fn ~prefix:"../")
    then container_dir ^ "/" ^ included_fn
    else (
      let container_dir2 = Cull_string.before_rightmost container_dir '/'
      and included_fn2 = Cull_string.cobeginning 3 included_fn in
      parse_double_points_in_filename container_dir2 included_fn2)
  ;;

  module Individual_inclusion_analysis = struct
    type result = R of string option * string list

    let make str_opt l = R (str_opt, l)
    let read (R (str_opt, _)) = str_opt
    let possibilities (R (_, l)) = l
  end
 


  let analize_double_pointed_included_filename
    snap
    includer_fn
    original_included_fn
    =
    let includer_dir = Cull_string.before_rightmost includer_fn '/' in
    let included_fn = parse_double_points_in_filename includer_dir original_included_fn in
    let final_result, l =
      if str_mem included_fn (Snapshot.all_h_or_c_files snap)
      then Some included_fn, [ included_fn ]
      else None, []
    in
    Individual_inclusion_analysis.make final_result l
  ;;


   let choose_candidate_from_list includer_fn included_fn l inc_source_dirs =
    if List.length l = 1
    then Some (List.hd l)
    else if str_mem included_fn l
    then Some included_fn
    else (
      let includer_dir = Cull_string.before_rightmost includer_fn '/' in
      let neighbor = includer_dir ^ "/" ^ included_fn in
      if str_mem neighbor l
      then Some neighbor
      else (
        let indicated_ones =
          List.filter_map
            (fun indicated_dir ->
              let indicated_fn = indicated_dir ^ included_fn in
              if List.mem indicated_fn l then Some indicated_fn else None)
            inc_source_dirs
        in
        if List.length indicated_ones = 1 then Some (List.hd indicated_ones) else None))
  ;;

   let analize_slashed_nonpointed_included_filename
    snap
    includer_fn
    included_fn
    inc_source_dirs
    =
    let current_dir = Cull_string.before_rightmost includer_fn '/' in
    let candidates = List.filter (
      fun fn -> (fn = included_fn) || List.exists (fun 
        source_dir -> (String.starts_with 
        ~prefix:source_dir  fn)
      ) (current_dir::inc_source_dirs)
    )(Snapshot.all_h_or_c_files snap) in  
    let l =
      List.filter
        (fun nfn -> String.ends_with nfn ~suffix:included_fn)
        candidates
    in
    let final_result =
      choose_candidate_from_list includer_fn included_fn l inc_source_dirs
    in
    Individual_inclusion_analysis.make final_result l
  ;;

  let analize_slashed_included_filename
    snap
    includer_fn
    included_fn
    inc_source_dirs
    =
    if String.starts_with included_fn ~prefix:"../"
    then
      analize_double_pointed_included_filename
        snap
        includer_fn
        included_fn
    else
      analize_slashed_nonpointed_included_filename
        snap
        includer_fn
        included_fn
        inc_source_dirs
  ;;

  let analize_nonslashed_included_filename
    snap includer_fn included_fn inc_source_dirs =
    let current_dir = Cull_string.before_rightmost includer_fn '/' in
    let candidates = List.filter (
      fun fn -> List.exists (fun 
        source_dir -> String.starts_with 
        ~prefix:source_dir  fn
      ) (current_dir::inc_source_dirs)
    )(Snapshot.all_h_or_c_files snap) in  
    let l =
      List.filter
        (fun nfn -> String.ends_with nfn ~suffix:("/" ^ included_fn))
        candidates
    in
    let final_result =
      choose_candidate_from_list includer_fn included_fn l inc_source_dirs
    in
    Individual_inclusion_analysis.make final_result l
  ;;



 
    
    let parse_cee_inclusion_line
     snap includer_fn included_fn inc_source_dirs =
    if String.contains included_fn '/'
    then
      analize_slashed_included_filename
        
        snap
        includer_fn
        included_fn
        inc_source_dirs
    else
      analize_nonslashed_included_filename
        snap
        includer_fn
        included_fn
        inc_source_dirs
  ;;

    let included_source_dirs_for_file snap includer_fn =
    let cmds =Snapshot.separate_commands snap in
    match
      List.find_opt
        (fun cmd -> Cee_compilation_command.short_name_from_separate cmd = 
        includer_fn)
        cmds
    with
    | None -> []
    | Some cmd -> 
      Image.image(fun dir->
         if dir=""
         then Cull_string.before_rightmost includer_fn '/' 
         else dir 
        )
      (cmd.Cee_compilation_command_t.included_source_dirs)
  ;;


   exception Included_files_exn of string * int * string ;;
    let included_files_in_single_file snap includer_fn =
    let inc_source_files =
      included_source_dirs_for_file snap includer_fn
    in
    let temp1 = Cee_text.included_local_files_in_text (Snapshot.read_file snap includer_fn) in
    Image.image
      (fun (line_nbr, included_fn) ->
        let iar =
          parse_cee_inclusion_line
            snap
            includer_fn
            included_fn
            inc_source_files
        in
        match Individual_inclusion_analysis.read iar with
        | None -> raise (Included_files_exn (includer_fn, line_nbr, included_fn))
        | Some clean_included_fn -> line_nbr, clean_included_fn)
      temp1
  ;;


   let included_files_in_several_files
    snap
    includers
    =
    Image.image
         (fun includer_fn ->
             (includer_fn,
             included_files_in_single_file snap includer_fn))
         includers
  ;;
  
  let keep_temporary_files_mode = ref false ;;

  let main_preprocessing_command_for_separate_shadow
    snap
    old_separate_cmd
    =
    let dest_cmd =
      { old_separate_cmd with 
      Cee_compilation_command_t.root = Snapshot.destination snap }
    in
    Cee_compilation_command.preprocess_only_version dest_cmd
  ;;

 
  

   let compute_preprocessing_output_for_separate_shadow
    snap
    separate_cmd
    text_to_be_preprocessed
    =
    let dest_dir = Directory_name.connectable_to_subpath (Snapshot.destination snap) in
    let short_separate = Cee_compilation_command.short_name_from_separate separate_cmd in
    let short_name_for_preprocessable_file =
      Cee_common.add_extra_ending_in_filename ~extra:"preprocessable" short_separate
    and short_name_for_preprocessed_file =
      Cee_common.add_extra_ending_in_filename ~extra:"preprocessed" short_separate
    in
    let name_for_preprocessable_file = dest_dir ^ short_name_for_preprocessable_file
    and name_for_preprocessed_file = dest_dir ^ short_name_for_preprocessed_file in
    let msg = "(watermark  " ^ short_separate ^ ")" in
    let _ =
      Snapshot.create_file
        snap
        short_name_for_preprocessable_file
        ?new_content_description:(Some msg)
        ~is_temporary:true
        text_to_be_preprocessed
    in
    let cmd2 =
      main_preprocessing_command_for_separate_shadow snap separate_cmd
    in
    let _ = Basic.announce_execution cmd2 in
    let _ = Unix_command.uc cmd2 in
    let preprocessed_file = Absolute_path.of_string name_for_preprocessed_file in
    let answer = Io.read_whole_file preprocessed_file in
    let _ =
      if not !keep_temporary_files_mode
      then (
        let _ =
          Unix_command.conditional_multiple_uc
            [ "rm -f " ^ name_for_preprocessable_file
            ; "rm -f " ^ name_for_preprocessed_file
            ]
        in
        ())
    in
    answer
  ;;


  let shadow_for_separate_command snap separate_cmd =
    let name_for_included_file =
      Cee_compilation_command.short_name_from_separate separate_cmd
    in
    let old_text = Snapshot.read_file snap name_for_included_file in
    let text_to_be_preprocessed =
      Cee_text.tattoo_regions_between_conditional_directives 
      ~name_for_included_file old_text
    in
    let preprocessed_includer_text =
      compute_preprocessing_output_for_separate_shadow
        snap
        separate_cmd
        text_to_be_preprocessed
    in
    Cee_text.compute_shadow
      old_text
      ~inclusion_index_opt:None
      ~name_for_included_file
      ~preprocessed_includer_text
  ;;

  module Memoized = struct 

  let hashtbl_for_inclusions_in_dc_files = 
      (Hashtbl.create 20: (Cee_snapshot_parameters_t.t,(string * (int * string) list) list) Hashtbl.t) ;; 


    let inclusions_in_dc_files snap = 
       match Hashtbl.find_opt hashtbl_for_inclusions_in_dc_files 
        (Snapshot.parameters snap) with 
      (Some old_answer) -> old_answer 
      | None ->
      let answer = included_files_in_several_files snap (Snapshot.directly_compiled_files snap) in 
      let _ = Hashtbl.add hashtbl_for_inclusions_in_dc_files 
        (Snapshot.parameters snap) answer in 
      answer ;; 

     let hashtbl_for_shadows_for_dc_files = 
      (Hashtbl.create 20: (Cee_snapshot_parameters_t.t,(string * Cee_shadow_t.t) list) Hashtbl.t) ;;  

    let shadows_for_dc_files snap = 
      match Hashtbl.find_opt hashtbl_for_shadows_for_dc_files 
       (Snapshot.parameters snap) with 
      (Some old_answer) -> old_answer 
      | None ->
      let cmds = Snapshot.separate_commands snap in
      let answer = Image.image
        (fun cmd ->
          ( Cee_compilation_command.short_name_from_separate cmd
          , shadow_for_separate_command snap cmd
          ))
        cmds in 
      let _ = Hashtbl.add hashtbl_for_shadows_for_dc_files 
       (Snapshot.parameters snap) answer in 
      answer ;; 
        

  end ;;  

 
 

  let remove_cds_in_file snap ~name_for_container_file separate_cmd =
    let dest_dir = Directory_name.connectable_to_subpath (Snapshot.destination snap) in
    let dest_last =
      Cull_string.after_rightmost (Cull_string.coending 1 dest_dir) '/' ^ "/"
    in
    let old_text =
      Snapshot.read_file
        snap
        (separate_cmd.Cee_compilation_command_t.short_path
         ^ separate_cmd.Cee_compilation_command_t.ending)
    in
    let shadow = List.assoc name_for_container_file (Memoized.shadows_for_dc_files snap) in
    let (Cee_shadow_t.Sh(_,prawn)) = shadow in 
    let new_text = Cee_text.crop_using_prawn old_text prawn in
    let target_filename = dest_dir ^ name_for_container_file in
    let target_file = Absolute_path.create_file_if_absent target_filename in
    let _ =
      Basic.announce_execution
        ("(unifdeffed  "
         ^ name_for_container_file
         ^ ") > "
         ^ (dest_last ^ name_for_container_file)
         ^ ")")
    in
    Io.overwrite_with target_file new_text
  ;;

  let fiamengize_file snap ~fiamengo_depth ~name_for_container_file separate_cmd =
    let dest_dir = Directory_name.connectable_to_subpath (Snapshot.destination snap) in
    let dest_last =
      Cull_string.after_rightmost (Cull_string.coending 1 dest_dir) '/' ^ "/"
    in
    let old_text =
      Snapshot.read_file
        snap
        (separate_cmd.Cee_compilation_command_t.short_path
         ^ separate_cmd.Cee_compilation_command_t.ending)
    in
   
    let new_text = Cee_text.fiamengize_text ~fiamengo_depth (Snapshot.read_file snap) old_text in
    let target_filename = dest_dir ^ name_for_container_file in
    let target_file = Absolute_path.create_file_if_absent target_filename in
    let _ =
      Basic.announce_execution
        ("(fiamengized  "
         ^ name_for_container_file
         ^ ") > "
         ^ (dest_last ^ name_for_container_file)
         ^ ")")
    in
    Io.overwrite_with target_file new_text
  ;;

  let remove_cds_in_directly_compiled_file snap separate_cmd =
    let name_for_container_file =
      Cee_compilation_command.short_name_from_separate separate_cmd
    in
    remove_cds_in_file snap ~name_for_container_file separate_cmd
  ;;

  let fiamengize_directly_compiled_file snap separate_cmd =
    let name_for_container_file =
      Cee_compilation_command.short_name_from_separate separate_cmd
    in
    fiamengize_file snap ~name_for_container_file separate_cmd
  ;; 

  let ref_for_debugging_multiple_cd_removal = ref 0;;

  let remove_cds_in_directly_compiled_files snap separate_cmds =
    let temp1 = Int_range.index_everything separate_cmds
    and sn = string_of_int (List.length separate_cmds) in 
    let _ = (ref_for_debugging_multiple_cd_removal:=0) in 
    List.iter
      (fun (idx, separate_cmd) ->
        let msg1 =
          " Step "
          ^ string_of_int idx
          ^ " of "
          ^ sn
          ^ " : "
          ^ "removing cds in "
          ^ separate_cmd.Cee_compilation_command_t.short_path
          ^ separate_cmd.Cee_compilation_command_t.ending
          ^ "\n\n"
        and msg2 = " Finished step " ^ string_of_int idx ^ " of " ^ sn ^ ".\n" in
        print_string msg1;
        flush stdout;
        remove_cds_in_directly_compiled_file snap separate_cmd;
        ref_for_debugging_multiple_cd_removal:=idx;
        print_string msg2;
        flush stdout)
      temp1
  ;;

  let fiamengize_directly_compiled_files snap ~fiamengo_depth separate_cmds =
    let temp1 = Int_range.index_everything separate_cmds
    and sn = string_of_int (List.length separate_cmds) in 
    let _ = (ref_for_debugging_multiple_cd_removal:=0) in 
    List.iter
      (fun (idx, separate_cmd) ->
        let msg1 =
          " Step "
          ^ string_of_int idx
          ^ " of "
          ^ sn
          ^ " : "
          ^ "Fiamengizing "
          ^ separate_cmd.Cee_compilation_command_t.short_path
          ^ separate_cmd.Cee_compilation_command_t.ending
          ^ "\n\n"
        and msg2 = " Finished step " ^ string_of_int idx ^ " of " ^ sn ^ ".\n" in
        print_string msg1;
        flush stdout;
        fiamengize_directly_compiled_file snap ~fiamengo_depth separate_cmd;
        ref_for_debugging_multiple_cd_removal:=idx;
        print_string msg2;
        flush stdout)
      temp1
  ;;
 

  let remove_cds_in_all_directly_compiled_files snap =
    remove_cds_in_directly_compiled_files snap (Snapshot.separate_commands snap)
  ;;

  let fiamengize_all_directly_compiled_files snap ~fiamengo_depth= 
    fiamengize_directly_compiled_files snap ~fiamengo_depth (Snapshot.separate_commands snap)
  ;;  

  
  let is_an_alphabetic_lowercase_char c = 
    let i= int_of_char c in 
    (97<=i)&&(i<=122);;
  let is_an_alphabetic_uppercase_char c = 
    let i= int_of_char c in 
    (65<=i)&&(i<=90);;

  let is_an_alphabetic_char c =
     (is_an_alphabetic_lowercase_char c) 
     ||
     (is_an_alphabetic_uppercase_char c) ;;

  let ambiguous_nonstandard_inclusions_in_individual_includer 
      snap includer_fn = 
     let text = Snapshot.read_file snap includer_fn in
     let temp1 = Cee_text.included_nonlocal_files_in_text text 
     and all_files = Snapshot.all_h_or_c_files snap in 
     List.filter (
       fun (_line_nbr, included_fn) -> 
         let c=String.get included_fn 0 in
         (not(is_an_alphabetic_char c))
         ||
         (List.exists (fun fn->String.ends_with
           fn ~suffix:("/"^included_fn) 
         ) all_files) 
     ) temp1 ;;

     let unusual_header_inclusion_formats_in_individual_includer snap includer_fn =
    let inc_source_dirs =
      included_source_dirs_for_file snap includer_fn
    in
    let text = Snapshot.read_file snap includer_fn in
    let lines = Lines_in_text.indexed_lines text in
    let temp5 = Cee_text.included_nonlocal_files_in_text text in
    let temp6 =
      List.filter_map
        (fun (line_nbr, included_fn) ->
          let iar =
            parse_cee_inclusion_line
              snap
              includer_fn
              included_fn
              inc_source_dirs
          in
          match Individual_inclusion_analysis.read iar with
          | None -> None
          | Some answer -> Some (List.assoc line_nbr lines, "#include \"" ^ answer ^ "\""))
        temp5
    in
    temp6
  ;;

  let unusual_header_inclusion_formats_in_includers snap includers =
    List.filter_map
      (fun includer ->
        let l = unusual_header_inclusion_formats_in_individual_includer snap includer in
        if l = [] then None else Some (includer, l))
      includers
  ;;
     

  let ambiguous_nonstandard_inclusions_in_files 
      snap includer_fns =
    List.filter_map ( fun includer_fn ->
      let l =  ambiguous_nonstandard_inclusions_in_individual_includer 
      snap includer_fn in  
      if l= []
      then None 
      else  Some(includer_fn,l) 
      ) includer_fns;;  

  let nonstandard_inclusion_formats_in_individual_includer snap includer_fn =
    let inc_source_dirs =
      included_source_dirs_for_file snap includer_fn
    in
    let text = Snapshot.read_file snap includer_fn in
    let temp1 = Cee_text.included_local_files_in_text text
    and lines = Lines_in_text.indexed_lines text in
    let temp2 =
      Image.image
        (fun (line_nbr, vague_included_fn) ->
          let iar =
            parse_cee_inclusion_line
              snap
              includer_fn
              vague_included_fn
              inc_source_dirs
          in
          ( includer_fn
          , vague_included_fn
          , List.assoc line_nbr lines
          , Individual_inclusion_analysis.read iar ))
        temp1
    in
    let temp3, temp4 =
      List.partition
        (fun (_line_nbr, _vague_included_fn, _line, solution_opt) -> solution_opt = None)
        temp2
    in
    let temp5 = Cee_text.included_nonlocal_files_in_text text in
    let part1 =
      Image.image
        (fun (_line_nbr, vague_included_fn, original_line, _) ->
          original_line, "#include <" ^ vague_included_fn ^ ">")
        temp3
    and part2 =
      List.filter_map
        (fun (_line_nbr, vague_included_fn, original_line, solution_opt) ->
          let included_fn = Option.get solution_opt in
          if included_fn <> vague_included_fn
          then Some (original_line, "#include \"" ^ included_fn ^ "\"")
          else None)
        temp4
    and part3 =
      List.filter_map
        (fun (line_nbr, included_fn) ->
          let iar =
            parse_cee_inclusion_line
              snap
              includer_fn
              included_fn
              inc_source_dirs
          in
          match Individual_inclusion_analysis.read iar with
          | None -> None
          | Some answer -> Some (List.assoc line_nbr lines, "#include \"" ^ answer ^ "\""))
        temp5
    in
    part1 @ part2 @ part3
  ;;

  let nonstandard_inclusion_formats_in_includers snap includers =
    List.filter_map
      (fun includer ->
        let l = nonstandard_inclusion_formats_in_individual_includer snap includer in
        if l = [] then None else Some (includer, l))
      includers
  ;;

  let standardize_inclusions_in_files snap includers ~dry_run =
    let replacements_to_be_made =
      nonstandard_inclusion_formats_in_includers snap includers
    in
    let _ =
      if not dry_run
      then
        List.iter
          (fun (fn, replacements) ->
            let old_text = Snapshot.read_file snap fn in
            let new_text =
              Replace_inside.replace_several_inside_text
                ~display_number_of_matches:true
                replacements
                old_text
            in
            Snapshot.modify_file snap fn new_text)
          replacements_to_be_made;
    in
    replacements_to_be_made
  ;;

  let standardize_guards_in_files snap files ~dry_run =
    List.filter_map
      (fun fn ->
        let old_text = Snapshot.read_file snap fn in
        match Cee_text.standardize_guard_in_text_opt old_text with
        | None -> None
        | Some new_text ->
          let _ = if not dry_run then Snapshot.modify_file snap fn new_text in
          Some fn)
      files
  ;;

  let next snap = 
      let old_params = Snapshot.parameters snap in 
      let new_params = {
         old_params with 
          Cee_snapshot_parameters_t.index =  
          ((old_params.Cee_snapshot_parameters_t.index)+1) ;
      } in 
      Snapshot.take_possession new_params ;;


  let reinit_and_fiamengize_all_directly_compiled_files snap ~fiamengo_depth= 
    let _ = (Snapshot.reinitialize_destination_directory snap;
    fiamengize_all_directly_compiled_files snap ~fiamengo_depth) in 
    next snap;;
  
  let reinit_and_remove_cds_in_all_directly_compiled_files snap = 
    let _ = (Snapshot.reinitialize_destination_directory snap;  
    remove_cds_in_all_directly_compiled_files snap) in 
    next snap;;

  let reinit_and_standardize_guards_in_directly_compiled_files snap ~dry_run= 
    let _ = (if not dry_run then Snapshot.reinitialize_destination_directory snap) in 
    let data = standardize_guards_in_files snap (Snapshot.directly_compiled_files snap) ~dry_run in 
    (data,next snap);;

  let reinit_and_standardize_inclusions_in_directly_compiled_files snap ~dry_run= 
    let _ = (if not dry_run then Snapshot.reinitialize_destination_directory snap) in 
    let data = standardize_inclusions_in_files snap (Snapshot.directly_compiled_files snap) ~dry_run in 
    (data,next snap);;

end ;; 

let fiamengize_all_directly_compiled_files =
    Private.reinit_and_fiamengize_all_directly_compiled_files ;;

let remove_conditional_directives_in_directly_compiled_files =
  Private.remove_cds_in_all_directly_compiled_files
;;

let standardize_guards_in_directly_compiled_files = 
  Private.reinit_and_standardize_guards_in_directly_compiled_files ;;
let standardize_inclusions_in_files = 
   Private.reinit_and_standardize_guards_in_directly_compiled_files ;;

