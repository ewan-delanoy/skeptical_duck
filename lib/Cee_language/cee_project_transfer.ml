(*

#use"lib/Cee_language/cee_project_transfer.ml";;

*)

exception Elif_in_ether of int ;;
exception Elif_after_else of int ;;
exception Else_in_ether of int ;;
exception Endif_in_ether of int ;;

module type CAPSULE_TYPE = sig
   
  type initial_command = {
    short_path : string;
    ending : string;
    core_of_command : string;
  } ;;

   type t 
   val source : t -> Directory_name_t.t
   val destination : t -> Directory_name_t.t
   val commands : t -> initial_command list
   val make :
     source:Directory_name_t.t ->
     destination:Directory_name_t.t -> string list -> t
   val all_h_or_c_files : t -> string list   
   val directly_compiled_files : t -> string list
 
  end ;;



 
module Capsule = (struct 

  type initial_command = {
  short_path : string;
  ending : string;
  core_of_command : string;
  } ;;

 type immutable_t = {
    source : Directory_name_t.t ;
    destination : Directory_name_t.t ;
    commands : initial_command list;
    all_h_or_c_files_opt : (string list) option ;
    directly_compiled_files_opt : (string list) option ;
 } ;;

 type t = immutable_t ref ;;
 let str_order = Total_ordering.lex_for_strings ;;
 let str_sort = Ordered.sort str_order ;;  
 let source cpsl = (!cpsl).source ;;

 let destination cpsl = (!cpsl).destination ;; 

 let commands cpsl = (!cpsl).commands ;;

 let make_initial_command raw_command = 
  let i1 = Option.get(Substring.leftmost_index_of_in_from_opt " -c " raw_command 1) in 
  let i2 = Option.get(Substring.leftmost_index_of_in_from_opt " " raw_command (i1+4)) in
  let short_filename = Cull_string.interval raw_command (i1+4) (i2-1) in 
  {
    short_path = Cull_string.coending 2 short_filename ;
    ending = Cull_string.ending 2 short_filename ;
    core_of_command =Cull_string.beginning (i1-1) raw_command ;
  }  ;;

 let make 
 ~source:src ~destination:dest raw_commands = 
 ref({
   source = src ;
   destination = dest ;
   commands = Image.image make_initial_command raw_commands;
   all_h_or_c_files_opt = None ;
   directly_compiled_files_opt = None ;
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

  let compute_directly_compiled_files cpsl = 
    str_sort(Image.image (
      fun cmd ->
       (cmd.short_path) ^ 
       (cmd.ending)
    ) cpsl.commands) ;;    
    
  let directly_compiled_files cpsl_ref = 
       let old_cpsl = (!cpsl_ref) in
       match old_cpsl.directly_compiled_files_opt with 
      (Some old_answer) -> old_answer 
      |None ->
        let answer = compute_directly_compiled_files old_cpsl in 
        let new_cpsl = {old_cpsl with 
          directly_compiled_files_opt = Some answer 
        } in 
        let _ = (cpsl_ref:=new_cpsl) in 
        answer ;;
   

end :CAPSULE_TYPE);; 

module Private = struct 

  type initial_command = Capsule.initial_command = {
    short_path : string;
    ending : string;
    core_of_command : string;
    } ;;

    let str_order = Total_ordering.lex_for_strings ;;
    let str_mem = Ordered.mem str_order ;;  
  
let adapt_ii_element root_dir i_elt = 
  if i_elt="-I." then "-I"^root_dir else 
  if (String.get i_elt 2)='/' then i_elt else 
  "-I"^root_dir^(Cull_string.cobeginning 2 i_elt) ;;    

let adapt_element root_dir (preceding_elt_opt,elt) = 
  if String.starts_with elt ~prefix:"-I"
  then adapt_ii_element root_dir elt
  else 
  match preceding_elt_opt with 
  None -> elt 
  |Some preceding_elt ->
    if List.mem preceding_elt ["-MF";"-MT"]
    then root_dir^elt
    else elt;;

let adapt_command root_dir cmd =
  let temp1 = Str.split (Str.regexp "[ \t\r]+") cmd in 
  let temp2 = List_again.universal_delta_list temp1 in 
  let temp3 = (None,List.hd temp1):: (Image.image (fun (e1,e2)->(Some e1,e2) ) temp2) in 
  let temp4 = Image.image (adapt_element root_dir) temp3 in 
  String.concat " " temp4 ;;




let main_preprocessing_command cpsl init_cmd = 
  let src_dir = Directory_name.connectable_to_subpath (Capsule.source cpsl) in  
  let core_of_command = adapt_command src_dir init_cmd.core_of_command in 
  let s_ap = src_dir ^ init_cmd.short_path ^ init_cmd.ending in 
  let short_s_ap = Cull_string.coending 2 s_ap 
  and ending = Cull_string.ending 2 s_ap in
  let second_filename = 
       (short_s_ap^"_"^Cee_text.random_marker^"_second"^ending) in
  let third_filename = 
      (short_s_ap^"_"^Cee_text.random_marker^"_third"^ending) in 
  core_of_command^" -E "^second_filename^" -o "^third_filename  ;;

let announce cmd = 
  (print_string("Executing "^cmd^" ...\n\n");
  flush stdout) ;;

let remove_cds_in_file cpsl init_cmd  = 
  let src_dir = Directory_name.connectable_to_subpath (Capsule.source cpsl) 
  and dest_dir = Directory_name.connectable_to_subpath (Capsule.destination cpsl) in  
  let src_last = (Cull_string.after_rightmost (Cull_string.coending 1 src_dir) '/' ) ^ "/"
  and dest_last = (Cull_string.after_rightmost (Cull_string.coending 1 dest_dir) '/' ) ^ "/" in 
  let s_ap = src_dir ^ init_cmd.short_path ^ init_cmd.ending in 
  let short_s_ap = Cull_string.coending 2 s_ap 
  and ending = init_cmd.ending in
  let ap = Absolute_path.of_string s_ap in  
  let old_text = Io.read_whole_file ap in 
  let second_text = Cee_text.watermark_text old_text 
  and second_filename = 
       (short_s_ap^"_"^Cee_text.random_marker^"_second"^ending) in
  let second_file = Absolute_path.create_file_if_absent second_filename in
  let _ = announce("(watermark  "^
     (init_cmd.short_path ^ init_cmd.ending)^") > "^
     (src_last ^ init_cmd.short_path ^"_"^Cee_text.random_marker^"_second"^ending)^")") in 
  let _ = Io.overwrite_with second_file second_text in 
  let third_filename = 
      (short_s_ap^"_"^Cee_text.random_marker^"_third"^ending) in 
  let cmd1 = main_preprocessing_command cpsl init_cmd in 
  let _ = announce(cmd1) in 
  let _ = Unix_command.uc cmd1 in 
  let third_file = Absolute_path.of_string third_filename in 
  let third_text =Io.read_whole_file third_file in 
  let new_text = Cee_text.rewrite_using_watermarks old_text third_text in 
  let fourth_filename = 
      (dest_dir ^ init_cmd.short_path ^ending) in 
  let fourth_file = Absolute_path.create_file_if_absent fourth_filename in  
  let _ = announce("(unifdeffed  "^
     (init_cmd.short_path ^ init_cmd.ending)^") > "^
     (dest_last ^ init_cmd.short_path ^ending)^")") in 
  Io.overwrite_with fourth_file new_text ;;

let remove_cds_in_files cpsl init_cmds = 
  let temp1 = Int_range.index_everything init_cmds 
  and sn = string_of_int(List.length init_cmds) in 
  List.iter (fun (idx,init_cmd) ->
    let msg1 = " Step "^(string_of_int idx)^" of "^sn^" : "^
    "removing cds in "^init_cmd.short_path^init_cmd.ending^"\n\n"
    and msg2 = " Finished step "^(string_of_int idx)^" of "^sn^".\n" in 
    print_string msg1;
    flush stdout;
    remove_cds_in_file cpsl init_cmd;
    print_string msg2;
    flush stdout;
    ) temp1 ;;

let remove_cds cpsl = remove_cds_in_files cpsl (Capsule.commands cpsl) ;; 

let cleanup_temporary_data_for_file cpsl init_cmd  = 
  let src_dir = Directory_name.connectable_to_subpath (Capsule.source cpsl)  in  
  let s_ap = src_dir ^ init_cmd.short_path ^ init_cmd.ending in 
  let short_s_ap = Cull_string.coending 2 s_ap 
  and ending = init_cmd.ending in
  let second_filename = 
       (short_s_ap^"_"^Cee_text.random_marker^"_second"^ending) in
  let third_filename = 
      (short_s_ap^"_"^Cee_text.random_marker^"_third"^ending) in 
  Unix_command.conditional_multiple_uc [
       "rm -f "^second_filename;
       "rm -f "^third_filename
  ]  ;;
  
let cleanup_temporary_files_after_cds_removal cpsl =   
  Image.image (cleanup_temporary_data_for_file cpsl) (Capsule.commands cpsl) ;;

let remove_cds_and_cleanup cpsl =
   let _ = (remove_cds cpsl;
   cleanup_temporary_files_after_cds_removal cpsl) in 
  ();;
   
  exception Check_presence_in_project_exn of string ;;

  let check_presence_in_project cpsl fn =
     if str_mem fn (Capsule.all_h_or_c_files cpsl) 
     then fn 
     else raise(Check_presence_in_project_exn(fn));;  
   

  
exception Normalize_included_filename_exn of string * string ;;
let normalize_nonpointed_included_filename cpsl includer_fn included_fn =
   match List.find_opt (fun nfn->
     String.ends_with nfn ~suffix:included_fn   
   ) (Capsule.all_h_or_c_files cpsl) with 
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
      let dest_dir = Directory_name.connectable_to_subpath (Capsule.destination cpsl) in 
      let ap = Absolute_path.of_string (dest_dir ^ includer_fn) in 
      let temp1 = Cee_text.included_local_files_in_file ap 
      and includer_dir = Cull_string.before_rightmost includer_fn '/' in 
      Image.image (fun (included_fn,indices)->
            try (normalize_included_filename cpsl includer_dir included_fn,indices) with
            Normalize_included_filename_exn(x,y) -> raise(Included_files_exn(includer_fn,x,y))
            ) temp1;;
      
    let nonstandard_inclusion_formats_in_individual_includer cpsl includer_fn = 
      let dest_dir = Directory_name.connectable_to_subpath (Capsule.destination cpsl) in 
      let ap = Absolute_path.of_string (dest_dir ^ includer_fn) in 
      let temp1 = Cee_text.included_local_files_in_file ap 
      and includer_dir = Cull_string.before_rightmost includer_fn '/' in 
      let text = Io.read_whole_file ap in 
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
      then  let dest_dir = Directory_name.connectable_to_subpath (Capsule.destination cpsl) in 
            List.iter (fun (fn,ab,ba)->
                let ap =  Absolute_path.of_string (dest_dir ^ fn) in 
                Replace_inside.replace_inside_file (ab,ba) ap) replacements_to_be_made
    ) in 
    replacements_to_be_made;; 



end ;;

let make = Capsule.make ;;

let remove_conditional_directives_in_directly_compiled_files = Private.remove_cds_and_cleanup ;; 

let standardize_inclusion_in_files = Private.standardize_inclusion_in_files ;;
