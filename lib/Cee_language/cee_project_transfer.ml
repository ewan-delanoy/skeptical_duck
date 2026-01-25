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
  let marker_for_shadowed_partial_copies = "_QhzFTSnAQA_" ;; 

    let shadowed_partial_copy_name 
      ~filepath ~copy_level ~prawn_index ~number_of_prawns = 
      let (basename,extension) =
          Cull_string.split_wrt_rightmost filepath '.' in 
      basename ^ marker_for_shadowed_partial_copies ^
      "level_"^(string_of_int copy_level)^
      "_prawn_"^(string_of_int prawn_index)^
      "_of_"^(string_of_int number_of_prawns)^"."^extension ;;

   let conventional_snapshot_location root idx suffix=
    let s_idx = Strung.insert_repetitive_offset_on_the_left
      '0' 3 (string_of_int idx) in 
     (Directory_name.connectable_to_subpath root)^
     "snapshot-"^s_idx^"-"^suffix ;;

   module PreCapsule = struct
    type t =
      { 
        snapshot : Cee_snapshot_t.t
      } ;;


    let str_sort = Ordered.sort str_order
    let str_setminus = Ordered.setminus str_order

  
    let root cpsl = (cpsl.snapshot.Cee_snapshot_t.project).Cee_project_t.root ;;
    let suffix cpsl = (cpsl.snapshot.Cee_snapshot_t.project).Cee_project_t.suffix_for_snapshots ;;
    
    let index cpsl = cpsl.snapshot.Cee_snapshot_t.index ;;
    
    let source cpsl = 
       Directory_name.of_string(
        conventional_snapshot_location (root cpsl) (index cpsl) (suffix cpsl)
      );;
     
    let destination cpsl = 
      Directory_name.of_string(
       conventional_snapshot_location (root cpsl) ((index cpsl)+1) (suffix cpsl)
      ) ;;
    
     
    let hashtbl_for_file_reading = 
      (Hashtbl.create 4000: (t * string,string) Hashtbl.t) ;; 

    let read_file cpsl fn =
      match Hashtbl.find_opt hashtbl_for_file_reading (cpsl,fn) with
      | Some old_answer -> old_answer
      | None ->
        let src_dir = Directory_name.connectable_to_subpath (source cpsl) in
        let ap = Absolute_path.of_string (src_dir ^ fn) in
        let text = Io.read_whole_file ap in
        let _ = Hashtbl.add hashtbl_for_file_reading (cpsl,fn) text in
        text
    ;;


    
let hashtbl_for_commands = 
  (Hashtbl.create 20: (t,Cee_compilation_command_t.t list) Hashtbl.t) ;; 

let commands cpsl = 
    match Hashtbl.find_opt hashtbl_for_commands cpsl with 
  (Some old_answer) -> old_answer 
  | None ->
   let temp1 = read_file cpsl "Makefile" in 
   let mkf1 = Makefile.parse (Makefile_t.MT temp1) in 
   let (_,cmds_and_echoes) = Makefile.prerequisites_and_commands_for_target 
   (ref mkf1) "all" in
   let cmds = List.filter (fun line->
    not(String.starts_with line ~prefix:"@echo")) cmds_and_echoes in 
   let answer = Image.image (Cee_compilation_command.parse (source cpsl)) cmds in 
   let _ = Hashtbl.add hashtbl_for_commands cpsl answer in 
      answer ;; 


let hashtbl_for_all_h_or_c_files = 
  (Hashtbl.create 20: (t,string list) Hashtbl.t) ;; 

let all_h_or_c_files cpsl = 
    match Hashtbl.find_opt hashtbl_for_all_h_or_c_files cpsl with 
  (Some old_answer) -> old_answer 
  | None ->
    let src = Directory_name.connectable_to_subpath (source cpsl) in
      let temp1 = Unix_again.quick_beheaded_complete_ls src in
      let answer =  str_sort
        (List.filter
           (fun fn ->
             List.exists
               (fun edg -> String.ends_with fn ~suffix:edg)
               [ ".h"; ".c"; ".macros" ])
           temp1) in 
    let _ = Hashtbl.add hashtbl_for_all_h_or_c_files cpsl answer in 
      answer ;; 
let hashtbl_for_separate_commands = 
  (Hashtbl.create 20: (t,Cee_compilation_command_t.separate_t list) Hashtbl.t) ;; 

let separate_commands cpsl = 
    match Hashtbl.find_opt hashtbl_for_separate_commands cpsl with 
  (Some old_answer) -> old_answer 
  | None ->
    let answer =  List.filter_map
        (function
          | Cee_compilation_command_t.Batch _ -> None
          | Cee_compilation_command_t.Separate s -> Some s)
    (commands cpsl) in 
    let _ = Hashtbl.add hashtbl_for_separate_commands cpsl answer in 
    answer ;; 

    let hashtbl_for_directly_compiled_files = 
      (Hashtbl.create 20: (t,string list) Hashtbl.t) ;; 

    let directly_compiled_files cpsl = 
       match Hashtbl.find_opt hashtbl_for_directly_compiled_files cpsl with 
      (Some old_answer) -> old_answer 
      | None ->
      let answer = str_sort
        (Image.image
           Cee_compilation_command.short_name_from_separate
           (separate_commands cpsl)) in 
      let _ = Hashtbl.add hashtbl_for_directly_compiled_files cpsl answer in 
      answer ;; 

   
    let modify_file cpsl fn new_content =
      let dest_dir = Directory_name.connectable_to_subpath (destination cpsl) in
      let ap = Absolute_path.of_string (dest_dir ^ fn) in
      Io.overwrite_with ap new_content
    ;;

    let create_file_in_a_list cpsl fn 
      ?new_content_description ~is_temporary new_content index_msg=
      let dest_dir = Directory_name.connectable_to_subpath (destination cpsl) in
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

    let create_file cpsl fn ?new_content_description ~is_temporary new_content =
     create_file_in_a_list cpsl fn 
      ?new_content_description ~is_temporary new_content ""
    ;;

    let create_shadowed_partial_copy 
      cpsl fn prawn ~copy_level ~prawn_index ~number_of_prawns index_msg = 
      
      let copy_name = shadowed_partial_copy_name 
      ~filepath:fn ~copy_level ~prawn_index ~number_of_prawns in 
      let old_content = read_file cpsl fn in 
      let new_content = 
         Cee_text.crop_using_prawn old_content prawn in   
      create_file_in_a_list cpsl copy_name ~is_temporary:false new_content index_msg;;

    let text_for_makefile cpsl =
      let temp1 = Int_range.index_everything (commands cpsl) in
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

    let write_makefile cpsl =
      let source_dir = Directory_name.connectable_to_subpath (source cpsl) in
      let path_for_makefile = Absolute_path.create_file_if_absent (source_dir ^ "Makefile") in
      Io.overwrite_with path_for_makefile (text_for_makefile cpsl)
    ;;

    let included_files_in_several_files =
      included_files_in_several_files (separate_commands, all_h_or_c_files, read_file)
    ;;
 
    let hashtbl_for_inclusions_for_dc_files = 
      (Hashtbl.create 20: (t,(string * (int * string) list) list) Hashtbl.t) ;; 

    let inclusions_in_dc_files cpsl = 
       match Hashtbl.find_opt hashtbl_for_inclusions_for_dc_files cpsl with 
      (Some old_answer) -> old_answer 
      | None ->
      let answer = included_files_in_several_files cpsl (directly_compiled_files cpsl) in 
      let _ = Hashtbl.add hashtbl_for_inclusions_for_dc_files cpsl answer in 
      answer ;; 
      
    let hashtbl_for_shadows_for_dc_files = 
      (Hashtbl.create 20: (t,(string * Cee_shadow_t.t) list) Hashtbl.t) ;;  

    let shadows_for_dc_files cpsl = 
      match Hashtbl.find_opt hashtbl_for_shadows_for_dc_files cpsl with 
      (Some old_answer) -> old_answer 
      | None ->
      let cmds = separate_commands cpsl in
      let answer = Image.image
        (fun cmd ->
          ( Cee_compilation_command.short_name_from_separate cmd
          , shadow_for_separate_command (destination, read_file, create_file) cpsl cmd
          ))
        cmds in 
      let _ = Hashtbl.add hashtbl_for_shadows_for_dc_files cpsl answer in 
      answer ;; 


  let connected_components l =
     let temp1 = Arithmetic_list.decompose_into_connected_components l in 
     Image.image (fun (i,j)->Int_range.range i j) temp1 ;; 
 
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
  
  let first_constructor ~snapshot:snap  
    ~reinitialize_destination =
      let new_cpsl = 
        {  snapshot =snap 
        } in 
      let _ = (
         if reinitialize_destination 
         then  
          (
         reinitialize_destination_directory new_cpsl;)
      )  in  
      new_cpsl
    ;;

    let make ~snapshot ?(reinitialize_destination=false)  =
      first_constructor ~snapshot  ~reinitialize_destination ;;

  let replicate ?(reinitialize_destination=false) cpsl  =      
       first_constructor
   ~snapshot:((cpsl).snapshot)
   ~reinitialize_destination 
   ;;

     let unsafe_unveil cpsl = (cpsl:t) ;;   

  end ;;
end ;;

module type CAPSULE_INTERFACE = sig
  type t

  val source : t -> Directory_name_t.t
  val destination : t -> Directory_name_t.t
  val commands : t -> Cee_compilation_command_t.t list
  
  val all_h_or_c_files : t -> string list
  val separate_commands : t -> Cee_compilation_command_t.separate_t list
  val directly_compiled_files : t -> string list
  val inclusions_in_dc_files : t -> (string * ((int * string) list)) list
  val shadows_for_dc_files : t -> (string * Cee_shadow_t.t) list
  val read_file : t -> string -> string
  val modify_file : t -> string -> string -> unit
  val create_file : t -> string -> ?new_content_description:string -> is_temporary:bool -> string -> unit
  val create_shadowed_partial_copy :
      t ->
      string -> Cee_prawn_t.t -> copy_level:int -> prawn_index:int -> number_of_prawns:int -> string -> unit

   val make :
      snapshot:Cee_snapshot_t.t ->
      ?reinitialize_destination:bool -> t
    val replicate :
      ?reinitialize_destination:bool -> t -> t

  val unsafe_unveil : t -> t   
  val  reinitialize_destination_directory : t -> unit  

  val write_makefile : t -> unit
  
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

  let fiamengize_file cpsl ~fiamengo_depth ~name_for_container_file separate_cmd =
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
   
    let new_text = Cee_text.fiamengize_text ~fiamengo_depth (Capsule.read_file cpsl) old_text in
    let target_filename = dest_dir ^ name_for_container_file in
    let target_file = Absolute_path.create_file_if_absent target_filename in
    let _ =
      Private2.announce_execution
        ("(fiamengized  "
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

  let fiamengize_directly_compiled_file cpsl separate_cmd =
    let name_for_container_file =
      Cee_compilation_command.short_name_from_separate separate_cmd
    in
    fiamengize_file cpsl ~name_for_container_file separate_cmd
  ;; 

  let ref_for_debugging_multiple_cd_removal = ref 0;;

  let remove_cds_in_directly_compiled_files cpsl separate_cmds =
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
        remove_cds_in_directly_compiled_file cpsl separate_cmd;
        ref_for_debugging_multiple_cd_removal:=idx;
        print_string msg2;
        flush stdout)
      temp1
  ;;

  let fiamengize_directly_compiled_files cpsl ~fiamengo_depth separate_cmds =
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
        fiamengize_directly_compiled_file cpsl ~fiamengo_depth separate_cmd;
        ref_for_debugging_multiple_cd_removal:=idx;
        print_string msg2;
        flush stdout)
      temp1
  ;;
 

  let remove_cds_in_all_directly_compiled_files cpsl =
    remove_cds_in_directly_compiled_files cpsl (Capsule.separate_commands cpsl)
  ;;

  let fiamengize_all_directly_compiled_files cpsl ~fiamengo_depth= 
    fiamengize_directly_compiled_files cpsl ~fiamengo_depth (Capsule.separate_commands cpsl)
  ;;  

  let unusual_header_inclusion_formats_in_individual_includer cpsl includer_fn =
    let inc_source_dirs =
      Private2.included_source_dirs_for_file Capsule.separate_commands cpsl includer_fn
    in
    let text = Capsule.read_file cpsl includer_fn in
    let lines = Lines_in_text.indexed_lines text in
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
    and lines = Lines_in_text.indexed_lines text in
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
              Replace_inside.replace_several_inside_text
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

end ;; 

let fiamengize_all_directly_compiled_files = 
    Private.fiamengize_all_directly_compiled_files ;;

let remove_conditional_directives_in_directly_compiled_files =
  Private.remove_cds_in_all_directly_compiled_files
;;

let standardize_guards_in_files = Private.standardize_guards_in_files
let standardize_inclusions_in_files = Private.standardize_inclusions_in_files
