(*
#use"lib/Cee_language/cee_produce_next_snapshot.ml";;
*)


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
      if str_mem included_fn (Cee_snapshot.all_h_or_c_files snap)
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
    )(Cee_snapshot.all_h_or_c_files snap) in  
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
    )(Cee_snapshot.all_h_or_c_files snap) in  
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
    let cmds =Cee_snapshot.separate_commands snap in
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
    let temp1 = Cee_text.included_local_files_in_text (Cee_snapshot.read_file snap includer_fn) in
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
      Cee_compilation_command_t.root = Cee_snapshot.source snap }
    in
    Cee_compilation_command.preprocess_only_version dest_cmd
  ;;

 
  

   let compute_preprocessing_output_for_separate_shadow
    snap
    separate_cmd
    text_to_be_preprocessed
    =
    let source_dir = Directory_name.connectable_to_subpath (Cee_snapshot.source snap) in
    let short_separate = Cee_compilation_command.short_name_from_separate separate_cmd in
    let short_name_for_preprocessable_file =
      Cee_common.add_extra_ending_in_filename ~extra:"preprocessable" short_separate
    and short_name_for_preprocessed_file =
      Cee_common.add_extra_ending_in_filename ~extra:"preprocessed" short_separate
    in
    let name_for_preprocessable_file = source_dir ^ short_name_for_preprocessable_file
    and name_for_preprocessed_file = source_dir ^ short_name_for_preprocessed_file in
    let msg = "(watermark  " ^ short_separate ^ ")" in
    let _ =
      Cee_snapshot.create_file
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
    let old_text = Cee_snapshot.read_file snap name_for_included_file in
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
        (Cee_snapshot.parameters snap) with 
      (Some old_answer) -> old_answer 
      | None ->
      let answer = included_files_in_several_files snap (Cee_snapshot.directly_compiled_files snap) in 
      let _ = Hashtbl.add hashtbl_for_inclusions_in_dc_files 
        (Cee_snapshot.parameters snap) answer in 
      answer ;; 

     let hashtbl_for_shadows_for_dc_files = 
      (Hashtbl.create 20: (Cee_snapshot_parameters_t.t,(string * Cee_shadow_t.t) list) Hashtbl.t) ;;  

    let shadows_for_dc_files snap = 
      match Hashtbl.find_opt hashtbl_for_shadows_for_dc_files 
       (Cee_snapshot.parameters snap) with 
      (Some old_answer) -> old_answer 
      | None ->
      let cmds = Cee_snapshot.separate_commands snap in
      let answer = Image.image
        (fun cmd ->
          ( Cee_compilation_command.short_name_from_separate cmd
          , shadow_for_separate_command snap cmd
          ))
        cmds in 
      let _ = Hashtbl.add hashtbl_for_shadows_for_dc_files 
       (Cee_snapshot.parameters snap) answer in 
      answer ;; 
        

  end ;;  

 
 

  let remove_cds_in_file snap ~name_for_container_file separate_cmd =
    let dest_dir = Directory_name.connectable_to_subpath (Cee_snapshot.destination snap) in
    let dest_last =
      Cull_string.after_rightmost (Cull_string.coending 1 dest_dir) '/' ^ "/"
    in
    let old_text =
      Cee_snapshot.read_file
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
    let dest_dir = Directory_name.connectable_to_subpath (Cee_snapshot.destination snap) in
    let dest_last =
      Cull_string.after_rightmost (Cull_string.coending 1 dest_dir) '/' ^ "/"
    in
    let old_text =
      Cee_snapshot.read_file
        snap
        (separate_cmd.Cee_compilation_command_t.short_path
         ^ separate_cmd.Cee_compilation_command_t.ending)
    in
   
    let new_text = Cee_text.fiamengize_text ~fiamengo_depth (Cee_snapshot.read_file snap) old_text in
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
    remove_cds_in_directly_compiled_files snap (Cee_snapshot.separate_commands snap)
  ;;

  let fiamengize_all_directly_compiled_files snap ~fiamengo_depth= 
    fiamengize_directly_compiled_files snap ~fiamengo_depth (Cee_snapshot.separate_commands snap)
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
     let text = Cee_snapshot.read_file snap includer_fn in
     let temp1 = Cee_text.included_nonlocal_files_in_text text 
     and all_files = Cee_snapshot.all_h_or_c_files snap in 
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
    let text = Cee_snapshot.read_file snap includer_fn in
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
    let text = Cee_snapshot.read_file snap includer_fn in
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
            let old_text = Cee_snapshot.read_file snap fn in
            let new_text =
              Replace_inside.replace_several_inside_text
                ~display_number_of_matches:true
                replacements
                old_text
            in
            Cee_snapshot.modify_file snap fn new_text)
          replacements_to_be_made;
    in
    replacements_to_be_made
  ;;

  let standardize_guards_in_files snap files ~dry_run =
    List.filter_map
      (fun fn ->
        let old_text = Cee_snapshot.read_file snap fn in
        match Cee_text.standardize_guard_in_text_opt old_text with
        | None -> None
        | Some new_text ->
          let _ = if not dry_run then Cee_snapshot.modify_file snap fn new_text in
          Some fn)
      files
  ;;

  let next snap = 
      let old_params = Cee_snapshot.parameters snap in 
      let new_params = {
         old_params with 
          Cee_snapshot_parameters_t.index =  
          ((old_params.Cee_snapshot_parameters_t.index)+1) ;
      } in 
      Cee_snapshot.take_possession new_params ;;


  let reinit_and_fiamengize_all_directly_compiled_files snap ~fiamengo_depth= 
    let _ = (Cee_snapshot.reinitialize_destination_directory snap;
    fiamengize_all_directly_compiled_files snap ~fiamengo_depth) in 
    next snap;;
  
  let reinit_and_remove_cds_in_all_directly_compiled_files snap = 
    let _ = (Cee_snapshot.reinitialize_destination_directory snap;  
    remove_cds_in_all_directly_compiled_files snap) in 
    next snap;;

  let reinit_and_standardize_guards_in_directly_compiled_files snap ~dry_run= 
    let _ = (if not dry_run then Cee_snapshot.reinitialize_destination_directory snap) in 
    let data = standardize_guards_in_files snap (Cee_snapshot.directly_compiled_files snap) ~dry_run in 
    (data,next snap);;

  let reinit_and_standardize_inclusions_in_directly_compiled_files snap ~dry_run= 
    let _ = (if not dry_run then Cee_snapshot.reinitialize_destination_directory snap) in 
    let data = standardize_inclusions_in_files snap (Cee_snapshot.directly_compiled_files snap) ~dry_run in 
    (data,next snap);;

end ;; 

let compute_zones_between_conditional_directives_in_cd_files =
    Private.Memoized.shadows_for_dc_files ;;

let fiamengize_all_directly_compiled_files =
    Private.reinit_and_fiamengize_all_directly_compiled_files ;;

let remove_conditional_directives_in_directly_compiled_files =
  Private.reinit_and_remove_cds_in_all_directly_compiled_files
;;

let standardize_guards_in_directly_compiled_files = 
  Private.reinit_and_standardize_guards_in_directly_compiled_files ;;
let standardize_inclusions_in_files = 
   Private.reinit_and_standardize_guards_in_directly_compiled_files ;;

