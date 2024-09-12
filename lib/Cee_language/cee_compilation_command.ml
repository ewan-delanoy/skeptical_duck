(*

#use"lib/Cee_language/cee_compilation_command.ml";;

*)

type separate_t = Cee_compilation_command_t.separate_t = {
    root : Directory_name_t.t;
    included_header_dirs : Directory_name_t.t list;
    included_source_dirs : string list;
    deffers : (string * (string option)) list;
    undeffers : string list;
    dep_file : string option;
    libobj_file : string option;
    short_path : string;
    ending : string;
    core_of_command : string;
    } ;;
  
type batch_t = Cee_compilation_command_t.batch_t = {
    beginning_of_command : string ;
    litany: string list ;
    end_of_command : string
  } ;; 
  
type t = Cee_compilation_command_t.t = 
    Separate of separate_t 
   |Batch of batch_t ;;


module Private = struct

let deffer_extractor = Image.image (fun (_,elt) -> 
  let y = Cull_string.cobeginning 2 elt in 
  match String.index_opt y '=' with 
  None -> (y,None)
  |Some i ->
     (Cull_string.beginning i y,Some(Cull_string.cobeginning (i+1) y))
  ) ;;

let extract_and_classify_data_from_command raw_command = 
  let c2 = Cull_string.cobeginning 2 in 
  let starts_with = (fun pref l ->
    List.partition (fun (_,elt) -> String.starts_with
    elt ~prefix:pref) l
  ) 
  and is_preceded_by=(fun prec l ->
    let (a,b)=List.partition (fun (opt,_elt) -> opt=Some prec) l in
    if a= [] then (None,b) else (Some(snd(List.hd a)),b)
  ) in
  let temp1 = Str.split (Str.regexp "[ \t\r]+") raw_command in 
  let temp2 = List_again.universal_delta_list temp1 in 
  let temp3 = (None,List.hd temp1):: (Image.image (fun (e1,e2)->(Some e1,e2) ) temp2) in 
  let (header_dirs1,others1) = starts_with "-I/" temp3 in 
  let header_dirs = Image.image (fun (_,elt) -> 
       Directory_name.of_string(c2 elt)
    ) header_dirs1 in
  let (source_dirs1,others2) = starts_with "-I" others1 in 
  let source_dirs = Image.image (fun (_,elt) -> 
     let y = c2 elt in if y="." then "" else y) source_dirs1 in 
  let (deffings1,others3) = starts_with "-D" others2 in
  let deffings = deffer_extractor deffings1 in 
  let (undeffings1,others4) = starts_with "-U" others3 in
  let undeffings = Image.image (fun (_,elt) -> c2 elt ) undeffings1 in   
  let (dep_file_opt,others5) = is_preceded_by "-MF" others4 in
  let (libobj_file_opt,others6) = is_preceded_by "-MT" others5 in 
  let others7 = List.filter (fun (_,elt)->not(List.mem elt ["-MMD";"-MF";"-MT"])) others6 in  
  let shortened_command = String.concat " " (Image.image snd others7) in    
  let i1 = Option.get(Substring.leftmost_index_of_in_from_opt " -c " shortened_command 1) in 
  let i2 = Option.get(Substring.leftmost_index_of_in_from_opt " " shortened_command (i1+4)) in
  let short_filename = Cull_string.interval shortened_command (i1+4) (i2-1) in 
  let gutted_command = Cull_string.beginning (i1-1) shortened_command in 
  (header_dirs,source_dirs,deffings,undeffings,
     dep_file_opt,libobj_file_opt,short_filename,gutted_command) ;;

let parse_separate root_dir raw_command = 
  let (header_dirs,source_dirs,deffings,undeffings,
     dep_file_opt,libobj_file_opt,short_filename,gutted_command) = 
     extract_and_classify_data_from_command raw_command in 
  {
    root = root_dir;
    included_header_dirs = header_dirs;
    included_source_dirs = source_dirs;
    deffers = deffings;
    undeffers = undeffings;
    dep_file = dep_file_opt;
    libobj_file = libobj_file_opt ;
    short_path = Cull_string.coending 2 short_filename ;
    ending = Cull_string.ending 2 short_filename ;
    core_of_command = gutted_command ;
  }  ;;
 
let parse_batch raw_command = 
  let elts = Str.split (Str.regexp "[ \t\r]+") raw_command in 
  let i1 = (Option.get(List.find_index(fun elt->String.ends_with elt ~suffix:".o") elts))+1 in 
  let (rev_before,rest) = List_again.long_head_with_tail (i1-1) elts in 
  let before = List.rev rev_before in 
  let rev_rest = List.rev rest in 
  let i2 = (Option.get(List.find_index(fun elt->String.ends_with elt ~suffix:".o") rev_rest))+1 in 
  let (after,rev_middle) = List_again.long_head_with_tail (i2-1) rev_rest in 
  let middle = List.rev rev_middle  in  
  {
      beginning_of_command = String.concat " " before ;
      litany =Image.image (Cull_string.coending 2) middle ;
      end_of_command = String.concat " " after
  }  ;;

(*  
parse_batch "The brown and astute fox.o jumped.o over.o the lazy dog" ;;
*)
let parse root_dir raw_command = 
  if Substring.is_a_substring_of " -c " raw_command 
  then Separate(parse_separate root_dir raw_command)
  else Batch(parse_batch raw_command) ;;


let short_name_from_separate separate_cmd = 
  separate_cmd.Cee_compilation_command_t.short_path ^ 
  separate_cmd.Cee_compilation_command_t.ending ;; 

let short_name_for_preprocessable separate_cmd = 
  Cee_common.add_extra_ending_in_filename
    ~extra:"preprocessable" (short_name_from_separate separate_cmd) ;; 
let short_name_for_preprocessed separate_cmd = 
  Cee_common.add_extra_ending_in_filename
    ~extra:"preprocessed" (short_name_from_separate separate_cmd) ;; 

let needed_header_dirs separate_cmd =
  let temp1 = separate_cmd.Cee_compilation_command_t.included_header_dirs in 
  let temp2 = Image.image (
        fun dir -> 
          "-I"^(Directory_name.connectable_to_subpath dir)
  ) temp1 in 
  String.concat " " temp2 ;;     

let short_version_for_needed_source_dirs separate_cmd =
   let temp1 = separate_cmd.Cee_compilation_command_t.included_source_dirs in 
   let temp2 = Image.image (
     fun dir -> 
       let adjusted_dir = (if dir="" then "." else dir) in 
       "-I"^adjusted_dir
   ) temp1 in 
   String.concat " " temp2 ;; 
       
let long_version_for_needed_source_dirs separate_cmd =
  let root_dir = Directory_name.connectable_to_subpath separate_cmd.root in 
  let temp1 = separate_cmd.Cee_compilation_command_t.included_source_dirs in 
  let temp2 = Image.image (
      fun dir -> 
        "-I"^root_dir^dir
  ) temp1 in 
  String.concat " " temp2 ;;    

let needed_dirs ~write_out_root_in_source_inclusions separate_cmd =
  let header_dirs = needed_header_dirs separate_cmd 
  and source_dirs = (
     if write_out_root_in_source_inclusions 
     then long_version_for_needed_source_dirs separate_cmd 
     else short_version_for_needed_source_dirs separate_cmd 
  ) in 
  header_dirs ^ " " ^ source_dirs ;;

let deffers separate_cmd =
  let temp1 = separate_cmd.Cee_compilation_command_t.deffers in 
  let temp2 = Image.image (
      fun (vaar,vaal_opt) ->
        let vaal_part =(match vaal_opt with 
          None -> ""
          |Some vaal -> "="^vaal
        ) in 
        "-D"^vaar ^ vaal_part
  ) temp1 in 
  String.concat " " temp2 ;;  

let undeffers separate_cmd =
  let temp1 = separate_cmd.Cee_compilation_command_t.undeffers in 
  let temp2 = Image.image (fun vaar ->"-U"^vaar) temp1 in 
  String.concat " " temp2 ;;

let preprocess_only_version separate_cmd = 
  let root_dir = Directory_name.connectable_to_subpath separate_cmd.root in 
  let core_of_command = separate_cmd.Cee_compilation_command_t.core_of_command in 
  let short_separate = short_name_from_separate separate_cmd in 
  let short_name_for_preprocessable_file =  
        Cee_common.add_extra_ending_in_filename
        ~extra:"preprocessable" short_separate 
  and short_name_for_preprocessed_file =  
        Cee_common.add_extra_ending_in_filename
        ~extra:"preprocessed" short_separate in   
  let name_for_preprocessable_file = root_dir ^ short_name_for_preprocessable_file 
  and name_for_preprocessed_file = root_dir ^ short_name_for_preprocessed_file in 
  let included_dirs = 
    needed_dirs ~write_out_root_in_source_inclusions:true separate_cmd in
  core_of_command^" "^included_dirs^" "^(deffers separate_cmd)^" "^(undeffers separate_cmd)^
  " -E "^name_for_preprocessable_file^" -o "^name_for_preprocessed_file  ;;
  
(*

let cmd1 = parse_separate 
(Directory_name.of_string(home^"/Teuliou/Experimenting_with_php/php-src"))
"/usr/bin/clang -IZend/ -Imain/ -I. -Iext/date/lib/ -I/usr/include/libxml2/ -ITSRM/ -D_GNU_SOURCE -fno-common -Wstrict-prototypes -Wformat-truncation -Wall -Wextra -Wno-unused-parameter -Wno-sign-compare -g -ffp-contract=off -fvisibility=hidden -O0 -DZEND_SIGNALS -DZEND_ENABLE_STATIC_TSRMLS_CACHE=1 -DSHADOW_STACK_SYSCALL=0 -MMD -MF Zend/zend_exceptions.dep -MT Zend/zend_exceptions.lo -c Zend/zend_exceptions.c -o Zend/zend_exceptions.o" 
;;

let pp1 = preprocess_only_version cmd1 ;;

*)

let write_separate cmd = 
  let core_of_command = cmd.Cee_compilation_command_t.core_of_command in 
  let included_dirs = 
    needed_dirs ~write_out_root_in_source_inclusions:false cmd in
  core_of_command^" "^included_dirs^" "^(deffers cmd)^" "^(undeffers cmd)^
  " -c " ^ cmd.short_path ^ cmd.ending ^ 
  " -o " ^ cmd.short_path ^ ".o"

(*

let cmd1 = parse_separate 
(Directory_name.of_string(home^"/Teuliou/Experimenting_with_php/php-src"))
"/usr/bin/clang -IZend/ -Imain/ -I. -Iext/date/lib/ -I/usr/include/libxml2/ -ITSRM/ -D_GNU_SOURCE -fno-common -Wstrict-prototypes -Wformat-truncation -Wall -Wextra -Wno-unused-parameter -Wno-sign-compare -g -ffp-contract=off -fvisibility=hidden -O0 -DZEND_SIGNALS -DZEND_ENABLE_STATIC_TSRMLS_CACHE=1 -DSHADOW_STACK_SYSCALL=0 -MMD -MF Zend/zend_exceptions.dep -MT Zend/zend_exceptions.lo -c Zend/zend_exceptions.c -o Zend/zend_exceptions.o" 
;;

let ws1 = write_separate cmd1 ;; 

*)  


let write_batch cmd = 
  let middle = String.concat " " (Image.image (fun elt->elt^".o") cmd.litany) in 
  cmd.beginning_of_command ^ middle ^ cmd.end_of_command ;;

let write = function 
  (Separate s) -> write_separate s 
  |(Batch b) -> write_batch b ;;

end ;;

let parse = Private.parse ;;
let parse_separate = Private.parse_separate ;;

let preprocess_only_version = Private.preprocess_only_version ;; 
let short_name_for_preprocessable = Private.short_name_for_preprocessable ;;

let short_name_for_preprocessed = Private.short_name_for_preprocessed ;;

let short_name_from_separate = Private.short_name_from_separate ;;
let write = Private.write ;;