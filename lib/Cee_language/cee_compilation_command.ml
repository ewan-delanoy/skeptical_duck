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
  let deffings = Image.image (fun (_,elt) -> 
    let y = c2 elt in 
    match String.index_opt y '=' with 
    None -> (y,None)
    |Some i ->
       (Cull_string.beginning i y,Some(Cull_string.cobeginning (i+1) y))
    ) deffings1 in 
    let (undeffings1,others4) = starts_with "-U" others3 in
    let undeffings = Image.image (fun (_,elt) -> 
      c2 elt ) undeffings1 in   
    let (dep_file_opt,others5) = is_preceded_by "-MF" others4 in
    let (libobj_file_opt,others6) = is_preceded_by "-MT" others5 in
    let gutted_command = String.concat " " (Image.image snd others6) in    
    (header_dirs,source_dirs,deffings,undeffings,
     dep_file_opt,libobj_file_opt,gutted_command) ;;

let parse_separate root_dir raw_command = 
  let i1 = Option.get(Substring.leftmost_index_of_in_from_opt " -c " raw_command 1) in 
  let i2 = Option.get(Substring.leftmost_index_of_in_from_opt " " raw_command (i1+4)) in
  let short_filename = Cull_string.interval raw_command (i1+4) (i2-1) in 
  let (header_dirs,source_dirs,deffings,undeffings,
     dep_file_opt,libobj_file_opt,gutted_command) = 
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

let adapt_command ~root_dir cmd =
  let temp1 = Str.split (Str.regexp "[ \t\r]+") cmd in 
  let temp2 = List_again.universal_delta_list temp1 in 
  let temp3 = (None,List.hd temp1):: (Image.image (fun (e1,e2)->(Some e1,e2) ) temp2) in 
  let temp4 = Image.image (adapt_element root_dir) temp3 in 
  String.concat " " temp4 ;;

let short_name_from_separate separate_cmd = 
  separate_cmd.Cee_compilation_command_t.short_path ^ 
  separate_cmd.Cee_compilation_command_t.ending ;; 

let short_name_for_preprocessable separate_cmd = 
  Cee_common.add_extra_ending_in_filename
    ~extra:"preprocessable" (short_name_from_separate separate_cmd) ;; 
let short_name_for_preprocessed separate_cmd = 
  Cee_common.add_extra_ending_in_filename
    ~extra:"preprocessed" (short_name_from_separate separate_cmd) ;; 
          
    



let preprocess_only_version separate_cmd = 
  let root_dir = Directory_name.connectable_to_subpath separate_cmd.root in 
  let core_of_command = adapt_command 
       ~root_dir separate_cmd.Cee_compilation_command_t.core_of_command in 
  let short_separate = short_name_from_separate separate_cmd in 
  let short_name_for_preprocessable_file =  
        Cee_common.add_extra_ending_in_filename
        ~extra:"preprocessable" short_separate 
  and short_name_for_preprocessed_file =  
        Cee_common.add_extra_ending_in_filename
        ~extra:"preprocessed" short_separate in   
  let name_for_preprocessable_file = root_dir ^ short_name_for_preprocessable_file 
  and name_for_preprocessed_file = root_dir ^ short_name_for_preprocessed_file in 
  core_of_command^" -E "^name_for_preprocessable_file^" -o "^name_for_preprocessed_file  ;;
  




let write_separate cmd = 
    cmd.core_of_command ^ 
    " -c " ^ cmd.short_path ^ cmd.ending ^ 
    " -o " ^ cmd.short_path ^ ".o" ;;

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