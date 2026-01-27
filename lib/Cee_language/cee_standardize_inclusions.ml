(*
#use"lib/Cee_language/cee_standardize_inclusions.ml";;
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

end ;;  

let nonstandard_inclusion_formats_in_includers = 
   Private.nonstandard_inclusion_formats_in_includers ;;