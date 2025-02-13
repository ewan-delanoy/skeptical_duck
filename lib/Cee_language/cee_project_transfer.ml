(*
   #use"lib/Cee_language/cee_project_transfer.ml";;
*)

module Private2 = struct
  let str_order = Total_ordering.lex_for_strings ;; 
  let str_mem = Ordered.mem str_order ;;

  let il_order = Total_ordering.lex_compare Total_ordering.for_integers;;
  let il_sort = Ordered.sort il_order ;;

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
    cpsl_all_h_or_c_files
    cpsl
    includer_fn
    original_included_fn
    =
    let includer_dir = Cull_string.before_rightmost includer_fn '/' in
    let included_fn = parse_double_points_in_filename includer_dir original_included_fn in
    let final_result, l =
      if str_mem included_fn (cpsl_all_h_or_c_files cpsl)
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
    cpsl_all_h_or_c_files
    cpsl
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
    )(cpsl_all_h_or_c_files cpsl) in  
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
    cpsl_all_h_or_c_files
    cpsl
    includer_fn
    included_fn
    inc_source_dirs
    =
    if String.starts_with included_fn ~prefix:"../"
    then
      analize_double_pointed_included_filename
        cpsl_all_h_or_c_files
        cpsl
        includer_fn
        included_fn
    else
      analize_slashed_nonpointed_included_filename
        cpsl_all_h_or_c_files
        cpsl
        includer_fn
        included_fn
        inc_source_dirs
  ;;

  let analize_nonslashed_included_filename
    cpsl_all_h_or_c_files
    cpsl
    includer_fn
    included_fn
    inc_source_dirs
    =
    let current_dir = Cull_string.before_rightmost includer_fn '/' in
    let candidates = List.filter (
      fun fn -> List.exists (fun 
        source_dir -> String.starts_with 
        ~prefix:source_dir  fn
      ) (current_dir::inc_source_dirs)
    )(cpsl_all_h_or_c_files cpsl) in  
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
    cpsl_all_h_or_c_files
    cpsl
    includer_fn
    included_fn
    inc_source_dirs
    =
    if String.contains included_fn '/'
    then
      analize_slashed_included_filename
        cpsl_all_h_or_c_files
        cpsl
        includer_fn
        included_fn
        inc_source_dirs
    else
      analize_nonslashed_included_filename
        cpsl_all_h_or_c_files
        cpsl
        includer_fn
        included_fn
        inc_source_dirs
  ;;

  let included_source_dirs_for_file cpsl_separate_commands cpsl includer_fn =
    let cmds = cpsl_separate_commands cpsl in
    match
      List.find_opt
        (fun cmd -> Cee_compilation_command.short_name_from_separate cmd = includer_fn)
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

  let included_files_in_single_file
    (cpsl_separate_commands, cpsl_all_h_or_c_files, cpsl_read_file)
    cpsl
    includer_fn
    =
    let inc_source_files =
      included_source_dirs_for_file cpsl_separate_commands cpsl includer_fn
    in
    let temp1 = Cee_text.included_local_files_in_text (cpsl_read_file cpsl includer_fn) in
    Image.image
      (fun (line_nbr, included_fn) ->
        let iar =
          parse_cee_inclusion_line
            cpsl_all_h_or_c_files
            cpsl
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
    (cpsl_separate_commands, cpsl_all_h_or_c_files, cpsl_read_file)
    cpsl
    includers
    =
    Image.image
         (fun includer_fn ->
             (includer_fn,included_files_in_single_file
                (cpsl_separate_commands, cpsl_all_h_or_c_files, cpsl_read_file)
                cpsl
                includer_fn))
         includers
  ;;

  let announce cmd =
    print_string (cmd ^ " ...\n\n");
    flush stdout
  ;;

  let announce_execution cmd = announce ("Executing " ^ cmd)
  let keep_temporary_files_mode = ref false

  let main_preprocessing_command_for_separate_shadow
    cpsl_destination
    cpsl
    old_separate_cmd
    =
    let dest_cmd =
      { old_separate_cmd with Cee_compilation_command_t.root = cpsl_destination cpsl }
    in
    Cee_compilation_command.preprocess_only_version dest_cmd
  ;;

  let compute_preprocessing_output_for_separate_shadow
    (cpsl_destination, cpsl_create_file)
    cpsl
    separate_cmd
    text_to_be_preprocessed
    =
    let dest_dir = Directory_name.connectable_to_subpath (cpsl_destination cpsl) in
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
      cpsl_create_file
        cpsl
        short_name_for_preprocessable_file
        ?new_content_description:(Some msg)
        ~is_temporary:true
        text_to_be_preprocessed
    in
    let cmd2 =
      main_preprocessing_command_for_separate_shadow cpsl_destination cpsl separate_cmd
    in
    let _ = announce_execution cmd2 in
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

  let shadow_for_separate_command
    (cpsl_destination, cpsl_read_file, cpsl_create_file)
    cpsl
    separate_cmd
    =
    let name_for_included_file =
      Cee_compilation_command.short_name_from_separate separate_cmd
    in
    let old_text = cpsl_read_file cpsl name_for_included_file in
    let text_to_be_preprocessed =
      Cee_text.tattoo_regions_between_conditional_directives ~name_for_included_file old_text
    in
    let preprocessed_includer_text =
      compute_preprocessing_output_for_separate_shadow
        (cpsl_destination, cpsl_create_file)
        cpsl
        separate_cmd
        text_to_be_preprocessed
    in
    Cee_text.compute_shadow
      old_text
      ~inclusion_index_opt:None
      ~name_for_included_file
      ~preprocessed_includer_text
  ;;

  let create_copies_of_included_files_for_wardrobe
    (cpsl_inclusions_in_dc_files, cpsl_read_file, cpsl_create_file)
    cpsl
    short_filename
    =
    (* returns the list of the filenames created*)
    let temp1 = cpsl_inclusions_in_dc_files cpsl in
    let temp2 = List.assoc short_filename temp1 in
    let indexed_inclusions = Int_range.index_everything temp2 in 
    Image.image
      (fun (inclusion_idx,(_,fn)) ->
        let new_fn = Cee_common.add_extra_ending_in_filename ~extra:"includable" fn in
        let old_content = cpsl_read_file cpsl fn in
        let new_content =
          Cee_text.tattoo_regions_between_conditional_directives ~name_for_included_file:new_fn old_content
        in
        let msg = "(tattoo_regions  " ^ fn ^ ")" in
        let _ =
          cpsl_create_file cpsl new_fn ?new_content_description:(Some msg) ~is_temporary:true new_content
        in
        fn, old_content, new_fn,inclusion_idx)
      indexed_inclusions
  ;;

  let wardrobe_for_indexed_separate_command
    (cpsl_destination, cpsl_read_file, cpsl_create_file, cpsl_inclusions_in_dc_files)
    cpsl
    (idx, separate_cmd)
    s_num_of_cmds
    =
    let short_name = Cee_compilation_command.short_name_from_separate separate_cmd
    and s_idx = string_of_int idx in
    let indexed_name = short_name ^ " (" ^ s_idx ^ " of " ^ s_num_of_cmds ^ ")" in
    let _ = announce ("Computing the wardrobe for " ^ indexed_name) in
    let copied_includable_files =
      create_copies_of_included_files_for_wardrobe
        (cpsl_inclusions_in_dc_files, cpsl_read_file, cpsl_create_file)
        cpsl
        short_name
    in
    let dest_dir = Directory_name.connectable_to_subpath (cpsl_destination cpsl) in
    let old_text = cpsl_read_file cpsl short_name in
    let text_to_be_preprocessed, _nbr_of_inclusions =
      Cee_text.highlight_and_add_extra_ending_in_inclusions_inside_text 
        ~extra:"includable" old_text
    in
    let preprocessed_includer_text =
      compute_preprocessing_output_for_separate_shadow
        (cpsl_destination, cpsl_create_file)
        cpsl
        separate_cmd
        text_to_be_preprocessed
    in
    let answer =
      Cee_text.compute_wardrobe
        ~preprocessed_includer_text
        copied_includable_files
    in
    let _ =
      if not !keep_temporary_files_mode
      then (
        let _ =
          Image.image
            (fun (_, _, fn,_) -> Unix_command.uc ("rm -f " ^ dest_dir ^ fn))
            copied_includable_files
        in
        ())
    in
    let _ = announce ("Computation of wardrobe finished for " ^ indexed_name ^ ".") in
    answer
  ;;

  let marker_for_shadowed_partial_copies = "_QhzFTSnAQA_" ;; 

    let shadowed_partial_copy_name 
      ~filepath ~copy_level ~prawn_index ~number_of_prawns = 
      let (basename,extension) =
          Cull_string.split_wrt_rightmost filepath '.' in 
      basename ^ marker_for_shadowed_partial_copies ^
      "level_"^(string_of_int copy_level)^
      "_prawn_"^(string_of_int prawn_index)^
      "_of_"^(string_of_int number_of_prawns)^"."^extension ;;

   module PreCapsule = struct
    type immutable_t =
      { source_envname : string
      ; destination_envname : string
      ; source_opt : Directory_name_t.t option
      ; destination_opt : Directory_name_t.t option
      ; commands : Cee_compilation_command_t.t list
      ; all_h_or_c_files_opt : string list option
      ; separate_commands_opt : Cee_compilation_command_t.separate_t list option
      ; filecontents : (string, string) Hashtbl.t
      ; directly_compiled_files_opt : string list option
      ; inclusions_in_dc_files_opt : (string * ((int * string) list)) list option
      ; shadows_for_dc_files_opt : (string * Cee_shadow_t.t) list option
      ; wardrobes_for_dc_files_opt : (string * Cee_wardrobe_t.t) list option
      ; directly_included_files_opt : string list option
      ; inclusions_for_di_files : (string, (string * int) list) Hashtbl.t
      ; wardrobes_for_di_files_opt : (string * Cee_wardrobe_t.t) list option
      ; prawn_algebras_for_di_files_opt : ( (string * Cee_prawn_algebra_t.t) list) option
      } ;;

    type t = immutable_t ref

    let str_sort = Ordered.sort str_order
    let str_setminus = Ordered.setminus str_order
    let source_envname cpsl_ref = !cpsl_ref.source_envname
    let destination_envname cpsl_ref = !cpsl_ref.destination_envname
    let compute_source cpsl = Directory_name.of_string (Sys.getenv cpsl.source_envname)

    let source cpsl_ref =
      let old_cpsl = !cpsl_ref in
      match old_cpsl.source_opt with
      | Some old_answer -> old_answer
      | None ->
        let answer = compute_source old_cpsl in
        let new_cpsl = { old_cpsl with source_opt = Some answer } in
        let _ = cpsl_ref := new_cpsl in
        answer
    ;;

    let compute_destination cpsl =
      Directory_name.of_string (Sys.getenv cpsl.destination_envname)
    ;;

    let destination cpsl_ref =
      let old_cpsl = !cpsl_ref in
      match old_cpsl.destination_opt with
      | Some old_answer -> old_answer
      | None ->
        let answer = compute_destination old_cpsl in
        let new_cpsl = { old_cpsl with destination_opt = Some answer } in
        let _ = cpsl_ref := new_cpsl in
        answer
    ;;

    let commands cpsl = !cpsl.commands

        let compute_all_h_or_c_files cpsl_ref =
      let src = Directory_name.connectable_to_subpath (source cpsl_ref) in
      let temp1 = Unix_again.quick_beheaded_complete_ls src in
      str_sort
        (List.filter
           (fun fn ->
             List.exists
               (fun edg -> String.ends_with fn ~suffix:edg)
               [ ".h"; ".c"; ".macros" ])
           temp1)
    ;;

    let all_h_or_c_files cpsl_ref =
      let old_cpsl = !cpsl_ref in
      match old_cpsl.all_h_or_c_files_opt with
      | Some old_answer -> old_answer
      | None ->
        let answer = compute_all_h_or_c_files cpsl_ref in
        let new_cpsl = { old_cpsl with all_h_or_c_files_opt = Some answer } in
        let _ = cpsl_ref := new_cpsl in
        answer
    ;;

    let compute_separate_commands cpsl =
      List.filter_map
        (function
          | Cee_compilation_command_t.Batch _ -> None
          | Cee_compilation_command_t.Separate s -> Some s)
        cpsl.commands
    ;;

    let separate_commands cpsl_ref =
      let old_cpsl = !cpsl_ref in
      match old_cpsl.separate_commands_opt with
      | Some old_answer -> old_answer
      | None ->
        let answer = compute_separate_commands old_cpsl in
        let new_cpsl = { old_cpsl with separate_commands_opt = Some answer } in
        let _ = cpsl_ref := new_cpsl in
        answer
    ;;

    let compute_directly_compiled_files cpsl_ref =
      str_sort
        (Image.image
           Cee_compilation_command.short_name_from_separate
           (separate_commands cpsl_ref))
    ;;

    let directly_compiled_files cpsl_ref =
      match !cpsl_ref.directly_compiled_files_opt with
      | Some old_answer -> old_answer
      | None ->
        let answer = compute_directly_compiled_files cpsl_ref in
        let new_cpsl = { !cpsl_ref with directly_compiled_files_opt = Some answer } in
        let _ = cpsl_ref := new_cpsl in
        answer
    ;;

    let read_file cpsl_ref fn =
      let cpsl = !cpsl_ref in
      match Hashtbl.find_opt cpsl.filecontents fn with
      | Some old_answer -> old_answer
      | None ->
        let src_dir = Directory_name.connectable_to_subpath (source cpsl_ref) in
        let ap = Absolute_path.of_string (src_dir ^ fn) in
        let text = Io.read_whole_file ap in
        let _ = Hashtbl.add cpsl.filecontents fn text in
        text
    ;;

    let modify_file cpsl_ref fn new_content =
      let dest_dir = Directory_name.connectable_to_subpath (destination cpsl_ref) in
      let ap = Absolute_path.of_string (dest_dir ^ fn) in
      Io.overwrite_with ap new_content
    ;;

    let create_file_in_a_list cpsl_ref fn 
      ?new_content_description ~is_temporary new_content index_msg=
      let dest_dir = Directory_name.connectable_to_subpath (destination cpsl_ref) in
      let ap = Absolute_path.create_file_if_absent (dest_dir ^ fn) in
      let _ = Io.overwrite_with ap new_content in
      let end_of_msg =
        match new_content_description with
        | None -> ""
        | Some descr -> ", with content " ^ descr
      in
      let durability =(
         if is_temporary then "temporary" else "persistent") in 
      announce ("Created "^durability^" file  " ^ fn ^ end_of_msg ^ index_msg)
    ;;

    let create_file cpsl_ref fn ?new_content_description ~is_temporary new_content =
     create_file_in_a_list cpsl_ref fn 
      ?new_content_description ~is_temporary new_content ""
    ;;

    let create_shadowed_partial_copy 
      cpsl_ref fn prawn ~copy_level ~prawn_index ~number_of_prawns index_msg = 
      
      let copy_name = shadowed_partial_copy_name 
      ~filepath:fn ~copy_level ~prawn_index ~number_of_prawns in 
      let old_content = read_file cpsl_ref fn in 
      let new_content = 
         Cee_text.crop_using_prawn old_content prawn in   
      create_file_in_a_list cpsl_ref copy_name ~is_temporary:false new_content index_msg;;

    let text_for_makefile cpsl =
      let temp1 = Int_range.index_everything cpsl.commands in
      let temp2 =
        Image.image
          (fun (cmd_idx, cmd) ->
            let s_idx = string_of_int cmd_idx in
            [ "\t@echo \"************************************************ Step "
              ^ s_idx
              ^ ":\""
            ; "\t" ^ Cee_compilation_command.write cmd
            ])
          temp1
      in
      let temp3 = "make all:" :: List.flatten temp2 in
      String.concat "\n" temp3
    ;;

    let write_makefile cpsl_ref =
      let cpsl = !cpsl_ref in
      let dest_dir = Directory_name.connectable_to_subpath (destination cpsl_ref) in
      let path_for_makefile = Absolute_path.create_file_if_absent (dest_dir ^ "Makefile") in
      Io.overwrite_with path_for_makefile (text_for_makefile cpsl)
    ;;

    let text_for_upper_makefile cpsl =
      let dest = destination_envname cpsl in
      String.concat
        "\n"
        [ "diverka:"
        ; "\tcat /dev/null > ${EXPHP}/what_make_did.txt "
        ; "\trm -rf ${PHPSRC}/* "
        ; "\tcp -R ${" ^ dest ^ "}/* ${PHPSRC}/ "
        ; "\tcp ${" ^ dest ^ "}/.gdbinit ${PHPSRC}/"
        ; "\tcp ${EXPHP}/copiableMakefile ${PHPSRC}/Makefile "
        ; ""
        ; "adober:"
        ; "\tmake -C ${PHPSRC} -f ${PHPSRC}/Makefile all -j4 | "
          ^ "tee ${EXPHP}/what_make_did.txt"
        ; "#\tmake -C ${PHPSRC} -f ${PHPSRC}/Makefile install -j4 | "
          ^ "tee ${EXPHP}/what_make_install_did.txt "
        ]
    ;;

    let write_to_upper_makefile cpsl_ref =
      let dest_dir = Directory_name.connectable_to_subpath (destination cpsl_ref) in
      let upper_dir =
        Cull_string.before_rightmost (Cull_string.coending 1 dest_dir) '/'
      in
      let path_for_upper_makefile = Absolute_path.of_string (upper_dir ^ "/myMakefile") in
      Io.overwrite_with path_for_upper_makefile (text_for_upper_makefile cpsl_ref)
    ;;

    let included_files_in_several_files =
      included_files_in_several_files (separate_commands, all_h_or_c_files, read_file)
    ;;

    let compute_inclusions_in_dc_files cpsl_ref =
      included_files_in_several_files cpsl_ref (directly_compiled_files cpsl_ref)
    ;;

    let inclusions_in_dc_files cpsl_ref =
      match !cpsl_ref.inclusions_in_dc_files_opt with
      | Some old_answer -> old_answer
      | None ->
        let answer = compute_inclusions_in_dc_files cpsl_ref in
        let new_cpsl = { !cpsl_ref with inclusions_in_dc_files_opt = Some answer } in
        let _ = cpsl_ref := new_cpsl in
        answer
    ;;

    let compute_shadows_for_dc_files cpsl_ref =
      let cmds = separate_commands cpsl_ref in
      Image.image
        (fun cmd ->
          ( Cee_compilation_command.short_name_from_separate cmd
          , shadow_for_separate_command (destination, read_file, create_file) cpsl_ref cmd
          ))
        cmds
    ;;

    let shadows_for_dc_files cpsl_ref =
      match !cpsl_ref.shadows_for_dc_files_opt with
      | Some old_answer -> old_answer
      | None ->
        let answer = compute_shadows_for_dc_files cpsl_ref in
        let new_cpsl = { !cpsl_ref with shadows_for_dc_files_opt = Some answer } in
        let _ = cpsl_ref := new_cpsl in
        answer
    ;;

let compute_wardrobes_for_dc_files_in_execution_order cpsl_ref =
      let cmds = separate_commands cpsl_ref in
      let indexed_cmds = Int_range.index_everything cmds
      and s_num_of_cmds = string_of_int (List.length cmds) in
      Image.image
        (fun (idx, cmd) ->
          ( Cee_compilation_command.short_name_from_separate cmd
          , wardrobe_for_indexed_separate_command
              (destination, read_file, create_file, inclusions_in_dc_files)
              cpsl_ref
              (idx, cmd)
              s_num_of_cmds ))
        indexed_cmds
    ;;

let compute_wardrobes_for_dc_files cpsl_ref = 
  let dc_files = directly_compiled_files cpsl_ref 
  and execution_order = compute_wardrobes_for_dc_files_in_execution_order cpsl_ref in 
  Image.image (
    fun dc_file ->(dc_file,List.assoc dc_file execution_order)
  ) dc_files ;;

   let wardrobes_for_dc_files cpsl_ref =
      match !cpsl_ref.wardrobes_for_dc_files_opt with
      | Some old_answer -> old_answer
      | None ->
        let answer = compute_wardrobes_for_dc_files cpsl_ref in
        let new_cpsl = { !cpsl_ref with wardrobes_for_dc_files_opt = Some answer } in
        let _ = cpsl_ref := new_cpsl in
        answer
    ;;

    let compute_directly_included_files cpsl_ref =
      let temp1 = inclusions_in_dc_files cpsl_ref in
      let temp2 = Image.image(fun (_,l)->Image.image snd l) temp1 in 
      let temp3 = str_sort (List.flatten temp2) in
      str_setminus temp3 (directly_compiled_files cpsl_ref)
    ;;

    let directly_included_files cpsl_ref =
      match !cpsl_ref.directly_included_files_opt with
      | Some old_answer -> old_answer
      | None ->
        let answer = compute_directly_included_files cpsl_ref in
        let new_cpsl = { !cpsl_ref with directly_included_files_opt = Some answer } in
        let _ = cpsl_ref := new_cpsl in
        answer
    ;;

    let inclusions_for_di_file cpsl_ref fn =
      let cpsl = !cpsl_ref in
      match Hashtbl.find_opt cpsl.inclusions_for_di_files fn with
      | Some old_answer -> old_answer
      | None ->
        let temp1 = inclusions_in_dc_files cpsl_ref in
        
        
        let temp2 = List.filter_map(fun (includer,l)->
         List.find_map (fun (line_nbr,other_fn)->
             if other_fn = fn then Some(includer,line_nbr) else None  
          ) l ) temp1 in 
      let temp3 = str_sort (Image.image fst temp2) in
      let answer = Image.image (fun
        includer ->(includer,List.assoc includer temp2)
      ) temp3 in 
        let _ = Hashtbl.add cpsl.inclusions_for_di_files fn answer in
        answer
    ;;

    let symmetric_version included_one wardrobe_for_includers = 
      let temp1 = List.filter_map (
        fun (includer, (Cee_wardrobe_t.Wr data)) ->
          let list_form = List.filter_map (
            fun ((inclusion_idx,included_one2),shadow) ->
              if included_one = included_one2
              then Some((inclusion_idx,includer),shadow) 
              else None 
          ) data in 
          if list_form = []
          then None 
          else Some list_form
      ) wardrobe_for_includers in 
      Cee_wardrobe_t.Wr (List.flatten temp1) ;;


    let compute_wardrobes_for_di_files cpsl_ref =
      let wardrobe_for_includers = wardrobes_for_dc_files cpsl_ref in
      let di_files = directly_included_files cpsl_ref in
      Image.image
        (fun included_one ->
          ( included_one
          , symmetric_version included_one
              wardrobe_for_includers ))
        di_files
    ;;

    let wardrobes_for_di_files cpsl_ref =
      match !cpsl_ref.wardrobes_for_di_files_opt with
      | Some old_answer -> old_answer
      | None ->
        let answer = compute_wardrobes_for_di_files cpsl_ref in
        let new_cpsl = { !cpsl_ref with wardrobes_for_di_files_opt = Some answer } in
        let _ = cpsl_ref := new_cpsl in
        answer
    ;;

  let connected_components l =
     let temp1 = Arithmetic_list.decompose_into_connected_components l in 
     Image.image (fun (i,j)->Int_range.range i j) temp1 ;; 

    let compute_prawn_algebras_for_di_files cpsl_ref =
      let wardrobes = wardrobes_for_di_files cpsl_ref in
      Image.image 
      (fun (included_one,wardrobe)->
        (included_one,Cee_prawn_algebra.of_wardrobe wardrobe)  
      ) wardrobes;;

    let prawn_algebras_for_di_files cpsl_ref =
      match !cpsl_ref.prawn_algebras_for_di_files_opt with
      | Some old_answer -> old_answer
      | None ->
        let answer = compute_prawn_algebras_for_di_files cpsl_ref in
        let new_cpsl = { !cpsl_ref with prawn_algebras_for_di_files_opt = Some answer } in
        let _ = cpsl_ref := new_cpsl in
        answer
    ;;
 
  let reinitialize_destination_directory cpsl =
    let src = Directory_name.connectable_to_subpath (source cpsl)
    and slashed_dest = Directory_name.connectable_to_subpath (destination cpsl) in
    let dest = Cull_string.coending 1 slashed_dest in
    let _ = Unix_command.conditional_multiple_uc
      [ "rm -rf " ^ dest ^ "/*"
      ; "cp -R " ^ src ^ "/* " ^ dest ^ "/"
      ; "cp " ^ src ^ "/.gdbinit " ^ dest ^ "/"
      ] in 
    ()
    ;;
  
  let first_constructor ~source_envname:src_envname ~destination_envname:dest_envname 
    ~reinitialize_destination
    processed_commands =
      let dest = Directory_name.of_string (Sys.getenv dest_envname) in
      let new_cpsl = ref
        { source_envname = src_envname
        ; destination_envname = dest_envname
        ; commands = processed_commands
        ; source_opt = None
        ; destination_opt = Some dest
        ; all_h_or_c_files_opt = None
        ; separate_commands_opt = None
        ; filecontents = Hashtbl.create 3000
        ; directly_compiled_files_opt = None
        ; inclusions_in_dc_files_opt = None
        ; shadows_for_dc_files_opt = None
        ; wardrobes_for_dc_files_opt = None
        ; directly_included_files_opt = None
        ; inclusions_for_di_files = Hashtbl.create 600
        ; wardrobes_for_di_files_opt = None
        ; prawn_algebras_for_di_files_opt = None
        } in 
      let _ = (
         if reinitialize_destination 
         then  
          (write_makefile new_cpsl;
     write_to_upper_makefile new_cpsl;
     reinitialize_destination_directory new_cpsl;)
      )  in  
      new_cpsl
    ;;

    let make ?(reinitialize_destination=false) ~source_envname:src_envname ~destination_envname:dest_envname 
        raw_commands =
      let dest = Directory_name.of_string (Sys.getenv dest_envname) in 
      let processed_commands = Image.image (Cee_compilation_command.parse dest) raw_commands in 
      first_constructor ~source_envname:src_envname ~destination_envname:dest_envname ~reinitialize_destination processed_commands ;;

  let replicate ?(reinitialize_destination=false) ~next_envname cpsl  =      
       first_constructor
   ~source_envname:(((!cpsl).destination_envname))
   ~destination_envname:next_envname
   ~reinitialize_destination 
    ((!cpsl).commands) 
   ;;

    let unsafe_set_wardrobe_for_dc_files 
       cpsl_ref precomputed_wardrobes_for_dc_files = 
      let old_cpsl = (!cpsl_ref) in 
      let new_cpsl = {
        old_cpsl with 
        wardrobes_for_dc_files_opt = Some precomputed_wardrobes_for_dc_files
      } in  
      ref new_cpsl;;

  end ;;
end ;;

module type CAPSULE_INTERFACE = sig
  type t

  val source_envname : t -> string
  val destination_envname : t -> string
  val source : t -> Directory_name_t.t
  val destination : t -> Directory_name_t.t
  val commands : t -> Cee_compilation_command_t.t list
  
  val all_h_or_c_files : t -> string list
  val separate_commands : t -> Cee_compilation_command_t.separate_t list
  val directly_compiled_files : t -> string list
  val inclusions_in_dc_files : t -> (string * ((int * string) list)) list
  val shadows_for_dc_files : t -> (string * Cee_shadow_t.t) list
  val wardrobes_for_dc_files : t -> (string * Cee_wardrobe_t.t) list
  val directly_included_files : t -> string list
  val inclusions_for_di_file : t -> string -> (string * int) list
  val wardrobes_for_di_files : t -> (string * Cee_wardrobe_t.t) list
  val prawn_algebras_for_di_files : t -> (string * Cee_prawn_algebra_t.t) list
  val read_file : t -> string -> string
  val modify_file : t -> string -> string -> unit
  val create_file : t -> string -> ?new_content_description:string -> is_temporary:bool -> string -> unit
  val create_shadowed_partial_copy :
      t ->
      string -> Cee_prawn_t.t -> copy_level:int -> prawn_index:int -> number_of_prawns:int -> string -> unit

   val make :
      ?reinitialize_destination:bool ->
      source_envname:string ->
      destination_envname:string ->
       string list -> t
    val replicate :
      ?reinitialize_destination:bool ->
      next_envname:string -> t -> t



  val unsafe_set_wardrobe_for_dc_files :
      t -> (string * Cee_wardrobe_t.t) list -> t

  val  reinitialize_destination_directory : t -> unit  
  
end ;;

module Capsule : CAPSULE_INTERFACE = Private2.PreCapsule

module Private = struct
  let str_order = Total_ordering.lex_for_strings
  let str_mem = Ordered.mem str_order
  let str_sort = Ordered.sort str_order

  let remove_cds_in_file cpsl ~name_for_container_file separate_cmd =
    let dest_dir = Directory_name.connectable_to_subpath (Capsule.destination cpsl) in
    let dest_last =
      Cull_string.after_rightmost (Cull_string.coending 1 dest_dir) '/' ^ "/"
    in
    let old_text =
      Capsule.read_file
        cpsl
        (separate_cmd.Cee_compilation_command_t.short_path
         ^ separate_cmd.Cee_compilation_command_t.ending)
    in
    let shadow = List.assoc name_for_container_file (Capsule.shadows_for_dc_files cpsl) in
    let (Cee_shadow_t.Sh(_,prawn)) = shadow in 
    let new_text = Cee_text.crop_using_prawn old_text prawn in
    let target_filename = dest_dir ^ name_for_container_file in
    let target_file = Absolute_path.create_file_if_absent target_filename in
    let _ =
      Private2.announce_execution
        ("(unifdeffed  "
         ^ name_for_container_file
         ^ ") > "
         ^ (dest_last ^ name_for_container_file)
         ^ ")")
    in
    Io.overwrite_with target_file new_text
  ;;

  let remove_cds_in_directly_compiled_file cpsl separate_cmd =
    let name_for_container_file =
      Cee_compilation_command.short_name_from_separate separate_cmd
    in
    remove_cds_in_file cpsl ~name_for_container_file separate_cmd
  ;;

  let remove_cds_in_directly_compiled_files cpsl separate_cmds =
    let temp1 = Int_range.index_everything separate_cmds
    and sn = string_of_int (List.length separate_cmds) in
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
        remove_cds_in_directly_compiled_file cpsl separate_cmd;
        print_string msg2;
        flush stdout)
      temp1
  ;;

  let remove_cds_in_all_directly_compiled_files cpsl =
    remove_cds_in_directly_compiled_files cpsl (Capsule.separate_commands cpsl)
  ;;

  let unusual_header_inclusion_formats_in_individual_includer cpsl includer_fn =
    let inc_source_dirs =
      Private2.included_source_dirs_for_file Capsule.separate_commands cpsl includer_fn
    in
    let text = Capsule.read_file cpsl includer_fn in
    let lines = Lines_in_string.indexed_lines text in
    let temp5 = Cee_text.included_nonlocal_files_in_text text in
    let temp6 =
      List.filter_map
        (fun (line_nbr, included_fn) ->
          let iar =
            Private2.parse_cee_inclusion_line
              Capsule.all_h_or_c_files
              cpsl
              includer_fn
              included_fn
              inc_source_dirs
          in
          match Private2.Individual_inclusion_analysis.read iar with
          | None -> None
          | Some answer -> Some (List.assoc line_nbr lines, "#include \"" ^ answer ^ "\""))
        temp5
    in
    temp6
  ;;

  let unusual_header_inclusion_formats_in_includers cpsl includers =
    List.filter_map
      (fun includer ->
        let l = unusual_header_inclusion_formats_in_individual_includer cpsl includer in
        if l = [] then None else Some (includer, l))
      includers
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
      cpsl includer_fn = 
     let text = Capsule.read_file cpsl includer_fn in
     let temp1 = Cee_text.included_nonlocal_files_in_text text 
     and all_files = Capsule.all_h_or_c_files cpsl in 
     List.filter (
       fun (_line_nbr, included_fn) -> 
         let c=String.get included_fn 0 in
         (not(is_an_alphabetic_char c))
         ||
         (List.exists (fun fn->String.ends_with
           fn ~suffix:("/"^included_fn) 
         ) all_files) 
     ) temp1 ;;

  let ambiguous_nonstandard_inclusions_in_files 
      cpsl includer_fns =
    List.filter_map ( fun includer_fn ->
      let l =  ambiguous_nonstandard_inclusions_in_individual_includer 
      cpsl includer_fn in  
      if l= []
      then None 
      else  Some(includer_fn,l) 
      ) includer_fns;;  

  let nonstandard_inclusion_formats_in_individual_includer cpsl includer_fn =
    let inc_source_dirs =
      Private2.included_source_dirs_for_file Capsule.separate_commands cpsl includer_fn
    in
    let text = Capsule.read_file cpsl includer_fn in
    let temp1 = Cee_text.included_local_files_in_text text
    and lines = Lines_in_string.indexed_lines text in
    let temp2 =
      Image.image
        (fun (line_nbr, vague_included_fn) ->
          let iar =
            Private2.parse_cee_inclusion_line
              Capsule.all_h_or_c_files
              cpsl
              includer_fn
              vague_included_fn
              inc_source_dirs
          in
          ( includer_fn
          , vague_included_fn
          , List.assoc line_nbr lines
          , Private2.Individual_inclusion_analysis.read iar ))
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
            Private2.parse_cee_inclusion_line
              Capsule.all_h_or_c_files
              cpsl
              includer_fn
              included_fn
              inc_source_dirs
          in
          match Private2.Individual_inclusion_analysis.read iar with
          | None -> None
          | Some answer -> Some (List.assoc line_nbr lines, "#include \"" ^ answer ^ "\""))
        temp5
    in
    part1 @ part2 @ part3
  ;;

  let nonstandard_inclusion_formats_in_includers cpsl includers =
    List.filter_map
      (fun includer ->
        let l = nonstandard_inclusion_formats_in_individual_includer cpsl includer in
        if l = [] then None else Some (includer, l))
      includers
  ;;

  let standardize_inclusions_in_files cpsl includers ~dry_run =
    let replacements_to_be_made =
      nonstandard_inclusion_formats_in_includers cpsl includers
    in
    let _ =
      if not dry_run
      then
        List.iter
          (fun (fn, replacements) ->
            let old_text = Capsule.read_file cpsl fn in
            let new_text =
              Replace_inside.replace_several_inside_string
                ~display_number_of_matches:true
                replacements
                old_text
            in
            Capsule.modify_file cpsl fn new_text)
          replacements_to_be_made;
    in
    replacements_to_be_made
  ;;

  let standardize_guards_in_files cpsl files ~dry_run =
    List.filter_map
      (fun fn ->
        let old_text = Capsule.read_file cpsl fn in
        match Cee_text.standardize_guard_in_text_opt old_text with
        | None -> None
        | Some new_text ->
          let _ = if not dry_run then Capsule.modify_file cpsl fn new_text in
          Some fn)
      files
  ;;

  

  let create_level_1_copies cpsl = 
    let temp1 = Capsule.prawn_algebras_for_di_files cpsl in 
    let temp2 = List.flatten(Image.image (
     fun (fn,Cee_prawn_algebra_t.A(prawns)) ->
        Image.image (fun (prawn_idx,Cee_prawn_t.P l)->
          (fn,Cee_prawn_t.P l,
           prawn_idx,List.length prawns)
        ) prawns
    ) temp1) in 
    let temp4 = Int_range.index_everything temp2  in 
    let s_total = string_of_int(List.length temp2) in 
    List.iter 
    (fun (global_idx,(fn,shadow,prawn_index,number_of_prawns))->
       let idx_msg = " ("^(string_of_int global_idx)^" of "^s_total^")" in 
      Capsule.create_shadowed_partial_copy 
       cpsl fn shadow ~copy_level:1 ~prawn_index ~number_of_prawns idx_msg
      ) temp4;; 

   let inclusion_instructions_for_prawn 
   included_file copy_level number_of_prawns prawn_index =
   let name = Private2.shadowed_partial_copy_name
     ~filepath:included_file ~copy_level ~prawn_index ~number_of_prawns in 
   "#include\""^name^"\"" ;;  

  let inclusion_instructions_for_list_of_prawns 
    included_file ~copy_level prawn_indices total_nbr_of_prawns =
   let temp1 = Image.image (
      inclusion_instructions_for_prawn 
      included_file copy_level total_nbr_of_prawns
   ) prawn_indices 
  in 
  String.concat "\n" temp1 ;;   

  exception Distinct_filenames of int * string * string ;;  

   let data_for_inclusion_prawning cpsl = 
    let inclusions_for_dcs = Capsule.inclusions_in_dc_files cpsl in 
    let wardrobes_for_dcs = Capsule.wardrobes_for_dc_files cpsl in 
    let algebras_for_dis = Capsule.prawn_algebras_for_di_files cpsl in 
    let pairs1_for_dcs  = 
      List.combine inclusions_for_dcs wardrobes_for_dcs in 
    let pairs2_for_dcs  = 
      Image.image (
        fun ((includer_file1,li1),(includer_file2,Cee_wardrobe_t.Wr li2)) ->
         if includer_file1<>includer_file2
         then raise (Distinct_filenames(1,includer_file1,includer_file2))   
         else 
         let comb1 = List.combine li1 li2 in 
         let comb2 = Image.image (
         fun ((line_nbr,included_file1),
         ((_inc_idx,included_file2),sha)) ->
            if included_file1 <> included_file2 
            then raise (Distinct_filenames(2,included_file1,included_file2))
            else (line_nbr,included_file1,sha)  
          ) comb1 in   
          (includer_file1,comb2)
      ) pairs1_for_dcs in
      Image.image (
      fun (includer_file,li1) ->
      (includer_file,Image.image (
         fun (line_nbr,included_file,sha) ->
            let alg = List.assoc included_file algebras_for_dis in 
            let (prawn_indices,total_nbr_of_prawns) =
             Cee_prawn_algebra.decompose_shadow alg sha in 
           (line_nbr,(included_file,prawn_indices,total_nbr_of_prawns))  
      ) li1) 
      ) pairs2_for_dcs ;; 

     let prawn_inclusions_in_several_dc_files cpsl dc_files= 
        let sn = string_of_int(List.length dc_files) 
        and indexed_dc_files = Int_range.index_everything dc_files in  
        List.iter (
          fun (idx,(includer_file,data_for_includer_file)) ->
            let filecontent = Capsule.read_file cpsl includer_file in 
            let indexed_lines = Lines_in_string.indexed_lines filecontent in 
            let modified_lines = Image.image (
              fun (line_idx,old_line) ->
             match List.assoc_opt line_idx data_for_includer_file with 
             None -> old_line 
             |Some(included_file,prawn_indices,total_nbr_of_prawns)->
             inclusion_instructions_for_list_of_prawns 
                included_file ~copy_level:1 prawn_indices total_nbr_of_prawns
            ) indexed_lines in 
            let new_filecontent = String.concat "\n" modified_lines in 
            let _ = Capsule.modify_file cpsl includer_file new_filecontent in 
            let msg= "Inclusions have been prawned in "^includer_file^
            " ("^(string_of_int idx)^" of "^sn^")" in 
            Private2.announce msg
          ) indexed_dc_files ;;

     let prawn_inclusions_in_directly_compiled_files cpsl =
        prawn_inclusions_in_several_dc_files cpsl 
         (data_for_inclusion_prawning cpsl) ;;

end ;; 

let create_level_1_copies = Private.create_level_1_copies ;;

let prawn_inclusions_in_directly_compiled_files = Private.prawn_inclusions_in_directly_compiled_files ;;
let remove_conditional_directives_in_directly_compiled_files =
  Private.remove_cds_in_all_directly_compiled_files
;;

let standardize_guards_in_files = Private.standardize_guards_in_files
let standardize_inclusions_in_files = Private.standardize_inclusions_in_files
