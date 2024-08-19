(*

#use"lib/Cee_language/cee_project_transfer.ml";;

*)

exception Elif_in_ether of int ;;
exception Elif_after_else of int ;;
exception Else_in_ether of int ;;
exception Endif_in_ether of int ;;

module type CAPSULE_TYPE = sig
   
  

   type t 
   val source : t -> Directory_name_t.t
   val destination : t -> Directory_name_t.t
   val commands : t -> Cee_compilation_command_t.t list
   val make :
     source:Directory_name_t.t ->
     destination:Directory_name_t.t -> string list -> t
   val all_h_or_c_files : t -> string list   

   val separate_commands : t -> Cee_compilation_command_t.separate_t list
   val directly_compiled_files : t -> string list
 
   val read_file : t -> string -> string  

   val modify_file : t -> string -> string -> unit
   
   val write_makefile : t -> unit

  end ;;


module Private = struct 

  type immutable_capsule_t = {
    source : Directory_name_t.t ;
    destination : Directory_name_t.t ;
    commands : Cee_compilation_command_t.t list;
    all_h_or_c_files_opt : (string list) option ;
    separate_commands_opt : (Cee_compilation_command_t.separate_t list) option;
    directly_compiled_files_opt : (string list) option ;
    filecontents : (string, string) Hashtbl.t ;
  } ;;
  
  type capsule_t = immutable_capsule_t ref ;;
  let str_order = Total_ordering.lex_for_strings ;;
  let str_sort = Ordered.sort str_order ;;  
  
  let str_mem = Ordered.mem str_order ;; 
  let capsule_source cpsl = (!cpsl).source ;;
  
  let capsule_destination cpsl = (!cpsl).destination ;; 
  
  let capsule_commands cpsl = (!cpsl).commands ;;
  
  
  let make_capsule 
  ~source:src ~destination:dest raw_commands = 
  ref({
   source = src ;
   destination = dest ;
   commands = Image.image Cee_compilation_command.parse raw_commands;
   all_h_or_c_files_opt = None ;
   separate_commands_opt = None ;
   directly_compiled_files_opt = None ;
   filecontents = Hashtbl.create 3000
  }) ;;
  
  let compute_all_h_or_c_files_in_capsule cpt = 
  let src = Directory_name.connectable_to_subpath cpt.source in 
  let temp1 = Unix_again.quick_beheaded_complete_ls src in
  str_sort(List.filter (
    fun fn -> List.exists (fun edg ->
       String.ends_with fn ~suffix:edg) [".h";".c";".macros"]
  ) temp1 );;  
  
  let all_h_or_c_files_in_capsule cpsl_ref = 
   let old_cpsl = (!cpsl_ref) in
   match old_cpsl.all_h_or_c_files_opt with 
  (Some old_answer) -> old_answer 
  |None ->
    let answer = compute_all_h_or_c_files_in_capsule old_cpsl in 
    let new_cpsl = {old_cpsl with 
      all_h_or_c_files_opt = Some answer 
    } in 
    let _ = (cpsl_ref:=new_cpsl) in 
    answer ;;
  
  let compute_separate_commands_in_capsule cpsl = 
    List.filter_map (function 
      (Cee_compilation_command_t.Batch(_)) -> None 
      |Cee_compilation_command_t.Separate(s) -> Some s
    ) cpsl.commands ;;
      
  let separate_commands_in_capsule cpsl_ref = 
    let old_cpsl = (!cpsl_ref) in
       match old_cpsl.separate_commands_opt with 
      (Some old_answer) -> old_answer 
      |None ->
        let answer = compute_separate_commands_in_capsule old_cpsl in 
        let new_cpsl = {old_cpsl with 
        separate_commands_opt = Some answer 
        } in 
        let _ = (cpsl_ref:=new_cpsl) in 
        answer ;;
  
  let compute_directly_compiled_files_in_capsule cpsl_ref = 
    str_sort(Image.image (
      fun cmd ->
       (cmd.Cee_compilation_command_t.short_path) ^ 
       (cmd.Cee_compilation_command_t.ending)
    ) (separate_commands_in_capsule cpsl_ref)) ;;    
    
  let directly_compiled_files_in_capsule cpsl_ref = 
       match (!cpsl_ref).directly_compiled_files_opt with 
      (Some old_answer) -> old_answer 
      |None ->
        let answer = compute_directly_compiled_files_in_capsule cpsl_ref in 
        let new_cpsl = {(!cpsl_ref) with 
          directly_compiled_files_opt = Some answer 
        } in 
        let _ = (cpsl_ref:=new_cpsl) in 
        answer ;;
   
  let read_file_in_capsule cpsl_ref fn =
    let cpsl = (!cpsl_ref) in 
    match Hashtbl.find_opt cpsl.filecontents fn with 
    (Some old_answer) -> old_answer 
    | None ->
      let src_dir = Directory_name.connectable_to_subpath cpsl.source in 
      let ap = Absolute_path.of_string (src_dir ^ fn) in 
      let text = Io.read_whole_file ap in 
      let _ = Hashtbl.add cpsl.filecontents fn text in 
      text ;;
      
  let modify_file_in_capsule cpsl_ref fn new_content=
    let cpsl = (!cpsl_ref) in 
    let dest_dir = Directory_name.connectable_to_subpath cpsl.destination in 
    let ap = Absolute_path.of_string (dest_dir ^ fn) in
    Io.overwrite_with ap new_content;;
  
  let text_for_makefile_in_capsule cpsl =
    let temp1 = Int_range.index_everything cpsl.commands in 
    let temp2 = Image.image (fun (cmd_idx,cmd)->
      let s_idx = string_of_int cmd_idx in 
    [
    "\t@echo \"************************************************ Step "^s_idx^":\"";
    "\t"^(Cee_compilation_command.write cmd)  
    ]) temp1 in
    let temp3 = ("make all:")::(List.flatten temp2) in 
    String.concat "\n" temp3 ;; 
    
  let write_capsule_makefile cpsl_ref =
    let cpsl = (!cpsl_ref) in 
    let dest_dir = Directory_name.connectable_to_subpath cpsl.destination in  
    let path_for_makefile = Absolute_path.of_string (dest_dir ^ "Makefile" ) in 
    Io.overwrite_with path_for_makefile (text_for_makefile_in_capsule cpsl) ;;
  ;; 
   

let names_for_temporary_files_during_preprocessing cpsl separate_cmd  = 
  let dest_dir = Directory_name.connectable_to_subpath (capsule_destination cpsl)  in  
  let endingless = dest_dir ^ separate_cmd.Cee_compilation_command_t.short_path  in 
  let (short_preprocessable,short_preprocessed) = 
     Cee_compilation_command.short_names_for_temporary_files_during_preprocessing separate_cmd in 
  let file_to_be_preprocessed = 
       (endingless^"_"^short_preprocessable) 
  and preprocessed_file = 
      (endingless^"_"^short_preprocessed) in 
  (file_to_be_preprocessed,preprocessed_file) ;;

let main_preprocessing_command cpsl separate_cmd = 
  let dest_dir = Directory_name.connectable_to_subpath (capsule_destination cpsl) in  
  let core_of_command = Cee_compilation_command.adapt_command 
    ~root_dir:dest_dir separate_cmd.Cee_compilation_command_t.core_of_command in 
  let (name_for_preprocessable_file,name_for_preprocessed_file) = 
     names_for_temporary_files_during_preprocessing cpsl separate_cmd in 
  core_of_command^" -E "^name_for_preprocessable_file^" -o "^name_for_preprocessed_file  ;;

let announce cmd = 
  (print_string("Executing "^cmd^" ...\n\n");
  flush stdout) ;;

let keep_temporary_files_mode = ref false ;;  

  let compute_preprocessing_output cpsl separate_cmd text_to_be_preprocessed = 
    let dest_dir = Directory_name.connectable_to_subpath (capsule_destination cpsl) in  
    let dest_last = (Cull_string.after_rightmost (Cull_string.coending 1 dest_dir) '/' ) ^ "/" in
    let (short_preprocessable,short_preprocessed) = 
     Cee_compilation_command.short_names_for_temporary_files_during_preprocessing separate_cmd in 
    let endingless = dest_dir ^ separate_cmd.Cee_compilation_command_t.short_path  in 
    let name_for_preprocessable_file = endingless^"_"^short_preprocessable
    and name_for_preprocessed_file = endingless^"_"^short_preprocessed in 
    let preprocessable_file = Absolute_path.create_file_if_absent name_for_preprocessable_file in
    let _ = announce("(watermark  "^
       (separate_cmd.Cee_compilation_command_t.short_path ^ separate_cmd.Cee_compilation_command_t.ending)^") > "^
       (dest_last ^ separate_cmd.Cee_compilation_command_t.short_path ^"_"^short_preprocessable)^")") in 
    let _ = Io.overwrite_with preprocessable_file text_to_be_preprocessed in  
  
   let cmd1 = main_preprocessing_command cpsl separate_cmd in 
   let _ = announce(cmd1) in 
   let _ = Unix_command.uc cmd1 in 
   let preprocessed_file = Absolute_path.of_string name_for_preprocessed_file in 
   let answer = Io.read_whole_file preprocessed_file in 
   let _ = (
    if (not(!keep_temporary_files_mode)) 
    then let _ = Unix_command.conditional_multiple_uc [
      "rm -f "^name_for_preprocessable_file;
      "rm -f "^name_for_preprocessed_file
    ] in ()
   ) in 
   answer;;

let remove_cds_in_file cpsl ~name_for_watermarkable_file separate_cmd  = 
  let dest_dir = Directory_name.connectable_to_subpath (capsule_destination cpsl) in  
  let dest_last = (Cull_string.after_rightmost (Cull_string.coending 1 dest_dir) '/' ) ^ "/" in 
  let old_text = read_file_in_capsule cpsl (separate_cmd.Cee_compilation_command_t.short_path ^ separate_cmd.Cee_compilation_command_t.ending) in 
  let text_to_be_preprocessed = Cee_text.watermark_text ~name_for_watermarkable_file old_text in
  let preprocessed_text = compute_preprocessing_output cpsl separate_cmd text_to_be_preprocessed in 
  let new_text = Cee_text.rewrite_using_watermarks old_text ~name_for_watermarkable_file ~watermarked_text:preprocessed_text in 
  let target_filename = dest_dir ^ name_for_watermarkable_file in 
  let target_file = Absolute_path.create_file_if_absent target_filename in  
  let _ = announce("(unifdeffed  "^ name_for_watermarkable_file^") > "^
     (dest_last ^ name_for_watermarkable_file)^")") in 
  Io.overwrite_with target_file new_text ;;

let remove_cds_in_directly_compiled_file cpsl  separate_cmd  = 
 let name_for_watermarkable_file = separate_cmd.Cee_compilation_command_t.short_path ^ separate_cmd.Cee_compilation_command_t.ending in 
 remove_cds_in_file cpsl ~name_for_watermarkable_file separate_cmd ;;

let remove_cds_in_directly_compiled_files cpsl separate_cmds = 
  let temp1 = Int_range.index_everything separate_cmds 
  and sn = string_of_int(List.length separate_cmds) in 
  List.iter (fun (idx,separate_cmd) ->
    let msg1 = " Step "^(string_of_int idx)^" of "^sn^" : "^
    "removing cds in "^separate_cmd.Cee_compilation_command_t.short_path^separate_cmd.Cee_compilation_command_t.ending^"\n\n"
    and msg2 = " Finished step "^(string_of_int idx)^" of "^sn^".\n" in 
    print_string msg1;
    flush stdout;
    remove_cds_in_directly_compiled_file cpsl separate_cmd;
    print_string msg2;
    flush stdout;
    ) temp1 ;;

let remove_cds_in_all_directly_compiled_files cpsl = 
  remove_cds_in_directly_compiled_files cpsl (separate_commands_in_capsule cpsl) ;; 

let cleanup_temporary_data_for_file cpsl separate_cmd  = 
  let dest_dir = Directory_name.connectable_to_subpath (capsule_destination cpsl)  in  
  let s_ap = dest_dir ^ separate_cmd.Cee_compilation_command_t.short_path ^ separate_cmd.Cee_compilation_command_t.ending in 
  let short_s_ap = Cull_string.coending 2 s_ap 
  and ending = separate_cmd.Cee_compilation_command_t.ending in
  let second_filename = 
       (short_s_ap^"_"^Cee_text.random_marker^"_second"^ending) in
  let third_filename = 
      (short_s_ap^"_"^Cee_text.random_marker^"_third"^ending) in 
  Unix_command.conditional_multiple_uc [
       "rm -f "^second_filename;
       "rm -f "^third_filename
  ]  ;;
  
let cleanup_temporary_files_after_cds_removal cpsl =   
  Image.image (cleanup_temporary_data_for_file cpsl) (separate_commands_in_capsule cpsl) ;;

   
  exception Check_presence_in_project_exn of string ;;

  let check_presence_in_project cpsl fn =
     if str_mem fn (all_h_or_c_files_in_capsule cpsl) 
     then fn 
     else raise(Check_presence_in_project_exn(fn));;  
   

  
exception Normalize_included_filename_exn of string * string ;;
let normalize_nonpointed_included_filename cpsl includer_fn included_fn =
   match List.find_opt (fun nfn->
     String.ends_with nfn ~suffix:included_fn   
   ) (all_h_or_c_files_in_capsule cpsl) with 
  None -> raise (Normalize_included_filename_exn(includer_fn,included_fn))
  |(Some nfn) -> nfn;;   

  let rec normalize_pointed_included_filename cpsl includer_fn included_fn = 
    if not(String.starts_with included_fn ~prefix:"../") 
    then check_presence_in_project cpsl (includer_fn ^ "/" ^ included_fn) 
    else 
      let includer_fn2 = Cull_string.before_rightmost includer_fn '/'
      and included_fn2 = Cull_string.cobeginning 3 included_fn in 
      normalize_pointed_included_filename cpsl includer_fn2 included_fn2 ;;  
       
 let normalize_included_filename cpsl includer_dir included_fn = 
    if String.starts_with included_fn ~prefix:"../" 
    then normalize_pointed_included_filename cpsl includer_dir included_fn 
    else normalize_nonpointed_included_filename cpsl includer_dir included_fn ;;   


    exception Included_files_exn of string * string * string ;;

    let included_files cpsl includer_fn=
      let temp1 = Cee_text.included_local_files_in_text(read_file_in_capsule cpsl includer_fn) 
      and includer_dir = Cull_string.before_rightmost includer_fn '/' in 
      Image.image (fun (included_fn,indices)->
            try (normalize_included_filename cpsl includer_dir included_fn,indices) with
            Normalize_included_filename_exn(x,y) -> raise(Included_files_exn(includer_fn,x,y))
            ) temp1;;
      
    let nonstandard_inclusion_formats_in_individual_includer cpsl includer_fn = 
      let text = read_file_in_capsule cpsl includer_fn in 
      let temp1 = Cee_text.included_local_files_in_text text
      and includer_dir = Cull_string.before_rightmost includer_fn '/' in 
      let lines = Lines_in_string.indexed_lines text in 
       List.flatten( List.filter_map (fun (included_fn,indices)->
          try (fun _->None)(normalize_included_filename cpsl includer_dir included_fn) with
               Normalize_included_filename_exn(_,_) -> 
                  Some(Image.image(fun idx->(includer_fn,List.assoc idx lines)) indices)
               ) temp1) ;;
      
    let nonstandard_inclusion_formats_in_includers cpsl includers =
      List.flatten(
        Image.image (nonstandard_inclusion_formats_in_individual_includer cpsl) includers
      ) ;;

   

   let standardize_inclusion_in_files cpsl includers ~dry_run= 
     let temp1 = nonstandard_inclusion_formats_in_includers cpsl includers in 
     let replacements_to_be_made = Image.image (
       fun (includer,line)->(includer,line,Cee_text.standardize_inclusion_line line)
     ) temp1 in 
    let _ =(
      if not(dry_run)
      then List.iter (fun (fn,ab,ba)->
              let old_text = read_file_in_capsule cpsl fn in 
              let new_text = Replace_inside.replace_inside_string (ab,ba) old_text in 
              modify_file_in_capsule cpsl fn new_text) replacements_to_be_made
    ) in 
    replacements_to_be_made;; 

    let standardize_guards_in_files cpsl files = 
      List.filter_map (fun 
         fn ->
          let old_text = read_file_in_capsule cpsl fn in 
          match Cee_text.standardize_guard_in_text old_text with 
          None -> None 
          |Some new_text ->
            let _ = modify_file_in_capsule cpsl fn new_text in 
            Some fn 
      ) files ;;


end ;;

module Capsule = (struct
   
  type t = Private.capsule_t 
  let source = Private.capsule_source ;;

  let destination = Private.capsule_destination ;;

  let commands = Private.capsule_commands ;;
  
  let make = Private.make_capsule ;;
  
  let all_h_or_c_files = Private.all_h_or_c_files_in_capsule ;;  

  let separate_commands = Private.separate_commands_in_capsule ;;

  let directly_compiled_files = Private.directly_compiled_files_in_capsule ;;

  let read_file = Private.read_file_in_capsule ;; 

  let modify_file = Private.modify_file_in_capsule ;;
  
  let write_makefile = Private.write_capsule_makefile ;;

 end : CAPSULE_TYPE);;


let remove_conditional_directives_in_directly_compiled_files = Private.remove_cds_in_all_directly_compiled_files ;; 

let standardize_guards_in_files = Private.standardize_guards_in_files ;; 

let standardize_inclusion_in_files = Private.standardize_inclusion_in_files ;;
