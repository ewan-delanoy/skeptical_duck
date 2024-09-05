(*

#use"lib/Cee_language/cee_project_transfer.ml";;

*)


module Private2 = struct 

  let str_order = Total_ordering.lex_for_strings ;;
  let str_mem = Ordered.mem str_order ;;  
 

  exception Check_presence_in_project_exn of string ;;

  let check_presence_in_project cpsl_all_h_or_c_files cpsl fn =
     if str_mem fn (cpsl_all_h_or_c_files cpsl) 
     then fn 
     else raise(Check_presence_in_project_exn(fn));;  

  exception Normalize_included_filename_exn of string * string ;;

let normalize_nonpointed_included_filename cpsl_all_h_or_c_files cpsl includer_fn included_fn =
   match List.find_opt (fun nfn->
     String.ends_with nfn ~suffix:included_fn   
   ) (cpsl_all_h_or_c_files cpsl) with 
  None -> raise (Normalize_included_filename_exn(includer_fn,included_fn))
  |(Some nfn) -> nfn;;   

  let rec normalize_pointed_included_filename cpsl_all_h_or_c_files  cpsl includer_fn included_fn = 
    if not(String.starts_with included_fn ~prefix:"../") 
    then 
        let joined_form = (
          if String.starts_with included_fn ~prefix:"/"
          then includer_fn       ^ included_fn
          else includer_fn ^ "/" ^ included_fn
        ) in 
        check_presence_in_project cpsl_all_h_or_c_files  cpsl joined_form 
    else 
      let includer_fn2 = Cull_string.before_rightmost includer_fn '/'
      and included_fn2 = Cull_string.cobeginning 3 included_fn in 
      normalize_pointed_included_filename cpsl_all_h_or_c_files cpsl includer_fn2 included_fn2 ;;  
       
 let normalize_included_filename cpsl_all_h_or_c_files cpsl includer_dir initial_included_fn = 
    (*
       The following hack is necessary to deal with the
       case when some filename is a prefix of another,
       e.g. with ir_php.h and php.h
    *)
    let included_fn = (
      if String.contains initial_included_fn '/'
      then initial_included_fn
      else "/"^initial_included_fn 
    ) in 
    if String.starts_with included_fn ~prefix:"../" 
    then normalize_pointed_included_filename cpsl_all_h_or_c_files cpsl includer_dir included_fn 
    else normalize_nonpointed_included_filename cpsl_all_h_or_c_files cpsl includer_dir included_fn ;;   



  exception Included_files_exn of string * string * string ;;

  let included_files_in_single_file (cpsl_all_h_or_c_files,cpsl_read_file) cpsl includer_fn=
      let temp1 = Cee_text.included_local_files_in_text(cpsl_read_file cpsl includer_fn) 
      and includer_dir = Cull_string.before_rightmost includer_fn '/' in 
      Image.image (fun (idx,included_fn)->
            try 
            (idx,normalize_included_filename cpsl_all_h_or_c_files cpsl includer_dir included_fn) with
            Normalize_included_filename_exn(x,y) -> raise(Included_files_exn(includer_fn,x,y))
            ) temp1;;

  let included_files_in_several_files 
    (cpsl_all_h_or_c_files,cpsl_read_file) cpsl includers =
    List.flatten(Image.image (fun includer_fn ->
      Image.image (fun 
      (idx,included_fn) -> (includer_fn,idx,included_fn)
      ) (included_files_in_single_file (cpsl_all_h_or_c_files,cpsl_read_file) cpsl includer_fn)
    ) includers);; 

  let announce cmd = 
      (print_string(cmd^" ...\n\n");
      flush stdout) ;;

  let announce_execution cmd = 
    announce("Executing "^cmd) ;;    
    
  let keep_temporary_files_mode = ref false ;;  

let main_preprocessing_command_for_separate_shadow 
  (cpsl_destination) cpsl separate_cmd = 
  let dest_dir = Directory_name.connectable_to_subpath (cpsl_destination cpsl) in  
  Cee_compilation_command.preprocess_only_version
     ~root_dir:dest_dir separate_cmd ;; 

 let compute_preprocessing_output_for_separate_shadow 
   (cpsl_destination,cpsl_create_file) 
   cpsl separate_cmd text_to_be_preprocessed = 
  let dest_dir = Directory_name.connectable_to_subpath (cpsl_destination cpsl) in 
  let short_separate = Cee_compilation_command.short_name_from_separate separate_cmd in 
  let short_name_for_preprocessable_file =  
    Cee_common.add_extra_ending_in_filename
    ~extra:"preprocessable" short_separate 
  and short_name_for_preprocessed_file =  
    Cee_common.add_extra_ending_in_filename
    ~extra:"preprocessed" short_separate in   
  let name_for_preprocessable_file = dest_dir ^ short_name_for_preprocessable_file 
  and name_for_preprocessed_file = dest_dir ^ short_name_for_preprocessed_file in 
  let msg = "(watermark  "^short_separate^")" in 
  let _ = cpsl_create_file  cpsl short_name_for_preprocessable_file 
  ?new_content_description:(Some msg)
  text_to_be_preprocessed in 
  let cmd2 = main_preprocessing_command_for_separate_shadow
     (cpsl_destination) cpsl separate_cmd in 
  let _ = announce_execution(cmd2) in 
  let _ = Unix_command.uc cmd2 in 
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


let shadow_for_separate_command (cpsl_destination,cpsl_read_file,cpsl_create_file) cpsl separate_cmd  = 
 let name_for_container_file = 
  Cee_compilation_command.short_name_from_separate separate_cmd in 
 let old_text = cpsl_read_file cpsl name_for_container_file in 
 let text_to_be_preprocessed = Cee_text.watermark_text ~name_for_container_file old_text in
 let preprocessed_text = compute_preprocessing_output_for_separate_shadow 
    (cpsl_destination,cpsl_create_file) cpsl separate_cmd text_to_be_preprocessed in 
 Cee_text.compute_shadow old_text ~name_for_container_file ~watermarked_text:preprocessed_text  ;;

let create_copies_of_included_files_for_wardrobe 
(cpsl_inclusions_in_dc_files,cpsl_read_file,cpsl_create_file)
  cpsl short_filename =
  (* returns the list of the filenames created*) 
  let temp1 = cpsl_inclusions_in_dc_files cpsl in 
  let included_files = List.filter_map (
  fun (includer,_line_nbr,included_one) ->
  if includer = short_filename
  then Some(included_one)
  else None   
  ) temp1 in 
  Image.image (
    fun fn -> 
      let new_fn = Cee_common.add_extra_ending_in_filename
       ~extra:"includable" fn in 
      let old_content = cpsl_read_file cpsl fn in
      let new_content = Cee_text.watermark_text
        ~name_for_container_file:new_fn old_content in 
      let msg = "(watermark  "^fn^")" in 
      let _ = cpsl_create_file cpsl new_fn 
      ?new_content_description:(Some msg)
      new_content in 
      (fn,old_content,new_fn)  
  ) included_files;;
    
let wardrobe_for_separate_command 
 (cpsl_destination,cpsl_read_file,cpsl_create_file,
  cpsl_inclusions_in_dc_files) cpsl separate_cmd  = 
 
 let short_name = 
    Cee_compilation_command.short_name_from_separate separate_cmd in  
 let _ = announce("Computing the wardrobe for "^short_name)  in
  let copied_includable_files = 
  create_copies_of_included_files_for_wardrobe 
  (cpsl_inclusions_in_dc_files,cpsl_read_file,cpsl_create_file) cpsl short_name in  
  let dest_dir = Directory_name.connectable_to_subpath (cpsl_destination cpsl) in 
  let old_text = cpsl_read_file cpsl short_name in 
 let (text_to_be_preprocessed,_nbr_of_inclusions) = 
    Cee_text.add_extra_ending_in_inclusions_inside_text 
    ~extra:"includable" old_text in
 let preprocessed_text = compute_preprocessing_output_for_separate_shadow 
    (cpsl_destination,cpsl_create_file) cpsl separate_cmd text_to_be_preprocessed in 
 let answer = Cee_text.compute_wardrobe  
   ~watermarked_text:preprocessed_text copied_includable_files in 
 let _ = (
  if (not(!keep_temporary_files_mode)) 
  then let _ = Image.image 
   (fun (_,_,fn) -> Unix_command.uc("rm -f "^dest_dir^fn)) 
      copied_includable_files
    in ()
 ) in 
 let _ = announce("Computation of wardrobe finished for "^
      separate_cmd.Cee_compilation_command_t.short_path ^ ".")  in
 answer;; 



module PreCapsule = struct 


 type immutable_t = {
  source : Directory_name_t.t ;
  destination : Directory_name_t.t ;
  commands : Cee_compilation_command_t.t list;
  all_h_or_c_files_opt : (string list) option ;
  separate_commands_opt : (Cee_compilation_command_t.separate_t list) option;
  filecontents : (string, string) Hashtbl.t ;
  directly_compiled_files_opt : (string list) option ;
  inclusions_in_dc_files_opt : ((string * int * string) list) option;
  shadows_for_dc_files_opt : ((string * Cee_shadow_t.t) list) option;
  wardrobes_for_dc_files_opt : ((string * ((string * Cee_shadow_t.t) list)) list) option;
  directly_included_files_opt : (string list) option ;
  inclusions_for_di_files : (string, (string * int) list) Hashtbl.t;
  wardrobes_for_di_files_opt : ((string * ((string * Cee_shadow_t.t) list)) list) option;
} ;;

type t = immutable_t ref ;;
let str_sort = Ordered.sort str_order ;;

let str_setminus = Ordered.setminus str_order ;;
let source cpsl = (!cpsl).source ;;

let destination cpsl = (!cpsl).destination ;; 

let commands cpsl = (!cpsl).commands ;;


let make 
~source:src ~destination:dest raw_commands = 
ref({
 source = src ;
 destination = dest ;
 commands = Image.image Cee_compilation_command.parse raw_commands;
 all_h_or_c_files_opt = None ;
 separate_commands_opt = None ;
 filecontents = Hashtbl.create 3000;
 directly_compiled_files_opt = None ;
 inclusions_in_dc_files_opt = None;
 shadows_for_dc_files_opt = None;
 wardrobes_for_dc_files_opt = None;
 directly_included_files_opt = None ;
 inclusions_for_di_files = Hashtbl.create 600;
 wardrobes_for_di_files_opt = None;
}) ;;

let compute_all_h_or_c_files cpt = 
let src = Directory_name.connectable_to_subpath cpt.source in 
let temp1 = Unix_again.quick_beheaded_complete_ls src in
str_sort(List.filter (
  fun fn -> List.exists (fun edg ->
     String.ends_with fn ~suffix:edg) [".h";".c";".macros"]
) temp1 );;  

let all_h_or_c_files cpsl_ref = 
 let old_cpsl = (!cpsl_ref) in
 match old_cpsl.all_h_or_c_files_opt with 
(Some old_answer) -> old_answer 
|None ->
  let answer = compute_all_h_or_c_files old_cpsl in 
  let new_cpsl = {old_cpsl with 
    all_h_or_c_files_opt = Some answer 
  } in 
  let _ = (cpsl_ref:=new_cpsl) in 
  answer ;;

let compute_separate_commands cpsl = 
  List.filter_map (function 
    (Cee_compilation_command_t.Batch(_)) -> None 
    |Cee_compilation_command_t.Separate(s) -> Some s
  ) cpsl.commands ;;
    
let separate_commands cpsl_ref = 
  let old_cpsl = (!cpsl_ref) in
     match old_cpsl.separate_commands_opt with 
    (Some old_answer) -> old_answer 
    |None ->
      let answer = compute_separate_commands old_cpsl in 
      let new_cpsl = {old_cpsl with 
      separate_commands_opt = Some answer 
      } in 
      let _ = (cpsl_ref:=new_cpsl) in 
      answer ;;

let compute_directly_compiled_files cpsl_ref = 
  str_sort(Image.image (
    fun cmd ->
     (cmd.Cee_compilation_command_t.short_path) ^ 
     (cmd.Cee_compilation_command_t.ending)
  ) (separate_commands cpsl_ref)) ;;    
  
let directly_compiled_files cpsl_ref = 
     match (!cpsl_ref).directly_compiled_files_opt with 
    (Some old_answer) -> old_answer 
    |None ->
      let answer = compute_directly_compiled_files cpsl_ref in 
      let new_cpsl = {(!cpsl_ref) with 
        directly_compiled_files_opt = Some answer 
      } in 
      let _ = (cpsl_ref:=new_cpsl) in 
      answer ;;
 
let read_file cpsl_ref fn =
  let cpsl = (!cpsl_ref) in 
  match Hashtbl.find_opt cpsl.filecontents fn with 
  (Some old_answer) -> old_answer 
  | None ->
    let src_dir = Directory_name.connectable_to_subpath cpsl.source in 
    let ap = Absolute_path.of_string (src_dir ^ fn) in 
    let text = Io.read_whole_file ap in 
    let _ = Hashtbl.add cpsl.filecontents fn text in 
    text ;;
    
let modify_file cpsl_ref fn new_content=
  let cpsl = (!cpsl_ref) in 
  let dest_dir = Directory_name.connectable_to_subpath cpsl.destination in 
  let ap = Absolute_path.of_string (dest_dir ^ fn) in
  Io.overwrite_with ap new_content;;

let create_file cpsl_ref fn ?new_content_description new_content =
  let cpsl = (!cpsl_ref) in 
  let dest_dir = Directory_name.connectable_to_subpath cpsl.destination in 
  let ap = Absolute_path.create_file_if_absent (dest_dir ^ fn) in
  let _ = Io.overwrite_with ap new_content in
  let end_of_msg = (
    match new_content_description with 
    None -> ""
    |Some (descr) -> ", with content "^descr
  )  in 
  announce("Created file  "^fn^end_of_msg) ;;

let text_for_makefile cpsl =
  let temp1 = Int_range.index_everything cpsl.commands in 
  let temp2 = Image.image (fun (cmd_idx,cmd)->
    let s_idx = string_of_int cmd_idx in 
  [
  "\t@echo \"************************************************ Step "^s_idx^":\"";
  "\t"^(Cee_compilation_command.write cmd)  
  ]) temp1 in
  let temp3 = ("make all:")::(List.flatten temp2) in 
  String.concat "\n" temp3 ;; 
  
let write_makefile cpsl_ref =
  let cpsl = (!cpsl_ref) in 
  let dest_dir = Directory_name.connectable_to_subpath cpsl.destination in  
  let path_for_makefile = Absolute_path.of_string (dest_dir ^ "Makefile" ) in 
  Io.overwrite_with path_for_makefile (text_for_makefile cpsl) ;;

  let included_files_in_several_files = 
     included_files_in_several_files 
  (all_h_or_c_files,read_file) ;;

  let compute_inclusions_in_dc_files cpsl_ref = 
    included_files_in_several_files cpsl_ref
      (directly_compiled_files cpsl_ref) ;;    
    
  let inclusions_in_dc_files cpsl_ref = 
       match (!cpsl_ref).inclusions_in_dc_files_opt with 
      (Some old_answer) -> old_answer 
      |None ->
        let answer = compute_inclusions_in_dc_files cpsl_ref in 
        let new_cpsl = {(!cpsl_ref) with 
        inclusions_in_dc_files_opt = Some answer 
        } in 
        let _ = (cpsl_ref:=new_cpsl) in 
        answer ;;

let compute_shadows_for_dc_files cpsl_ref = 
  let cmds = separate_commands cpsl_ref in 
  Image.image (
    fun cmd -> (Cee_compilation_command.short_name_from_separate cmd,    
    shadow_for_separate_command 
    (destination,read_file,create_file) cpsl_ref cmd 
    )
  ) cmds ;;    
          
let shadows_for_dc_files cpsl_ref = 
  match (!cpsl_ref).shadows_for_dc_files_opt with 
  (Some old_answer) -> old_answer 
  |None ->
   let answer = compute_shadows_for_dc_files cpsl_ref in 
   let new_cpsl = {(!cpsl_ref) with 
       shadows_for_dc_files_opt = Some answer 
   } in 
   let _ = (cpsl_ref:=new_cpsl) in 
   answer ;;

let compute_wardrobes_for_dc_files cpsl_ref = 
  let cmds = separate_commands cpsl_ref in 
  Image.image (
      fun cmd -> (Cee_compilation_command.short_name_from_separate cmd,    
      wardrobe_for_separate_command 
      (destination,read_file,create_file,
       inclusions_in_dc_files) cpsl_ref cmd 
      )
  ) cmds ;;    
            
let wardrobes_for_dc_files cpsl_ref = 
  match (!cpsl_ref).wardrobes_for_dc_files_opt with 
  (Some old_answer) -> old_answer 
  |None ->
    let answer = compute_wardrobes_for_dc_files cpsl_ref in 
    let new_cpsl = {(!cpsl_ref) with 
      wardrobes_for_dc_files_opt = Some answer 
    } in 
    let _ = (cpsl_ref:=new_cpsl) in 
    answer ;;

  let compute_directly_included_files cpsl_ref = 
    let temp1 = inclusions_in_dc_files cpsl_ref in 
    let temp2 = str_sort(Image.image ( fun 
      (_includer,_line_number,included_one) -> included_one
    ) temp1) in       
    str_setminus temp2 (directly_compiled_files cpsl_ref) ;;    
              
  let directly_included_files cpsl_ref = 
    match (!cpsl_ref).directly_included_files_opt with 
    (Some old_answer) -> old_answer 
    |None ->
      let answer = compute_directly_included_files cpsl_ref in 
      let new_cpsl = {(!cpsl_ref) with 
                    directly_included_files_opt = Some answer 
      } in 
      let _ = (cpsl_ref:=new_cpsl) in 
      answer ;;    
      
let inclusions_for_di_file cpsl_ref fn =
  let cpsl = (!cpsl_ref) in 
  match Hashtbl.find_opt cpsl.inclusions_for_di_files fn with 
  (Some old_answer) -> old_answer 
  | None ->
    let temp1 = inclusions_in_dc_files cpsl_ref in 
    let answer = List.filter_map (
      fun (includer,line_number,included_one) -> 
        if included_one = fn 
        then Some(includer,line_number) 
        else None   
       ) temp1 in 
    let _ = Hashtbl.add cpsl.inclusions_for_di_files fn answer in 
    answer ;;


let compute_wardrobes_for_di_files cpsl_ref = 
  let temp1 = wardrobes_for_dc_files cpsl_ref in 
  let di_files = directly_included_files cpsl_ref in 
  Image.image (
    fun included_one ->
      (included_one,List.filter_map (
        fun (includer,data) -> 
          Option.map (fun shadow->(includer,shadow))
          (List.assoc_opt included_one data)
      ) temp1) 
  ) di_files;;
  ;;    
                
let wardrobes_for_di_files cpsl_ref = 
  match (!cpsl_ref).wardrobes_for_di_files_opt with 
  (Some old_answer) -> old_answer 
  |None ->
    let answer = compute_wardrobes_for_di_files cpsl_ref in 
    let new_cpsl = {(!cpsl_ref) with 
          wardrobes_for_di_files_opt = Some answer 
    } in 
    let _ = (cpsl_ref:=new_cpsl) in 
    answer ;;



end ;;

end ;;  


module type CAPSULE_INTERFACE = sig
   
  

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
 
   val inclusions_in_dc_files : t -> ((string * int * string) list)
   
   val shadows_for_dc_files : t -> ((string * Cee_shadow_t.t) list)

   val wardrobes_for_dc_files : t -> (string * (string * Cee_shadow_t.t) list) list
   val directly_included_files : t -> string list
   val inclusions_for_di_file : t -> string -> (string * int) list

   val wardrobes_for_di_files : t -> (string * (string * Cee_shadow_t.t) list) list
   val read_file : t -> string -> string  

   val modify_file : t -> string -> string -> unit

   val create_file : t -> string -> ?new_content_description:string -> string -> unit
   
   val write_makefile : t -> unit

  end ;;



 
module Capsule = ( Private2.PreCapsule :CAPSULE_INTERFACE );; 

module Private = struct 


    let str_order = Total_ordering.lex_for_strings ;;
    let str_mem = Ordered.mem str_order ;; 
    let str_sort = Ordered.sort str_order ;; 



let remove_cds_in_file cpsl ~name_for_container_file separate_cmd  = 
  let dest_dir = Directory_name.connectable_to_subpath (Capsule.destination cpsl) in  
  let dest_last = (Cull_string.after_rightmost (Cull_string.coending 1 dest_dir) '/' ) ^ "/" in 
  let old_text = Capsule.read_file cpsl (separate_cmd.Cee_compilation_command_t.short_path ^ separate_cmd.Cee_compilation_command_t.ending) in 
  let shadow = List.assoc name_for_container_file (Capsule.shadows_for_dc_files cpsl) in 
  let new_text = Cee_text.rewrite_using_shadow old_text shadow in 
  let target_filename = dest_dir ^ name_for_container_file in 
  let target_file = Absolute_path.create_file_if_absent target_filename in  
  let _ = Private2.announce_execution("(unifdeffed  "^ name_for_container_file^") > "^
     (dest_last ^ name_for_container_file)^")") in 
  Io.overwrite_with target_file new_text ;;

let remove_cds_in_directly_compiled_file cpsl  separate_cmd  = 
 let name_for_container_file = 
  Cee_compilation_command.short_name_from_separate separate_cmd in 
 remove_cds_in_file cpsl ~name_for_container_file separate_cmd ;;

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
  remove_cds_in_directly_compiled_files cpsl (Capsule.separate_commands cpsl) ;; 

 let normalize_included_filename cpsl includer_dir included_fn = 
   Private2.normalize_included_filename Capsule.all_h_or_c_files
   cpsl includer_dir included_fn;;


    let nonstandard_inclusion_formats_in_individual_includer cpsl includer_fn = 
      let text = Capsule.read_file cpsl includer_fn in 
      let temp1 = Cee_text.included_local_files_in_text text
      and includer_dir = Cull_string.before_rightmost includer_fn '/' in 
      let lines = Lines_in_string.indexed_lines text in 
        List.filter_map (fun (idx,included_fn)->
          try (fun _->None)(normalize_included_filename cpsl includer_dir included_fn) with
               Private2.Normalize_included_filename_exn(_,_) -> 
                  Some(includer_fn,List.assoc idx lines))
                temp1 ;;
      
    let nonstandard_inclusion_formats_in_includers cpsl includers =
      List.flatten(
        Image.image (nonstandard_inclusion_formats_in_individual_includer cpsl) includers
      ) ;;   

   let standardize_inclusions_in_files cpsl includers ~dry_run= 
     let temp1 = nonstandard_inclusion_formats_in_includers cpsl includers in 
     let replacements_to_be_made1 = Image.image (
       fun (includer,line)->(includer,(line,Cee_text.standardize_inclusion_line line))
     ) temp1 in 
     let includers_involved = str_sort(Image.image fst replacements_to_be_made1) in 
     let replacements_to_be_made2 = Image.image (
      fun includer ->
        (includer,
        List.filter_map (fun (includer2,pair)
          -> if includer2 = includer 
             then Some pair 
             else None  
        ) replacements_to_be_made1)
    ) includers_involved in 
    let _ =(
      if not(dry_run)
      then List.iter (fun (fn,replacements)->
              let old_text = Capsule.read_file cpsl fn in 
              let new_text = Replace_inside.replace_several_inside_string 
               ~display_number_of_matches:true 
               replacements old_text in 
              Capsule.modify_file cpsl fn new_text) replacements_to_be_made2
    ) in 
    replacements_to_be_made2;; 

    let standardize_guards_in_files cpsl files  ~dry_run= 
      List.filter_map (fun 
         fn ->
          let old_text = Capsule.read_file cpsl fn in 
          match Cee_text.standardize_guard_in_text old_text with 
          None -> None 
          |Some new_text ->
            let _ = (
              if not(dry_run) 
              then Capsule.modify_file cpsl fn new_text
            ) in 
            Some fn 
      ) files ;;
   
  

end ;;

let make_capsule = Capsule.make ;;

let remove_conditional_directives_in_directly_compiled_files = Private.remove_cds_in_all_directly_compiled_files ;; 

let standardize_guards_in_files = Private.standardize_guards_in_files ;; 

let standardize_inclusions_in_files = Private.standardize_inclusions_in_files ;;
