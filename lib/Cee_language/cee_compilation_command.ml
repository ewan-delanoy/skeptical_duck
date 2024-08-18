(*

#use"lib/Cee_language/cee_compilation_command.ml";;

*)

module Private = struct

let parse_separate raw_command = 
  let i1 = Option.get(Substring.leftmost_index_of_in_from_opt " -c " raw_command 1) in 
  let i2 = Option.get(Substring.leftmost_index_of_in_from_opt " " raw_command (i1+4)) in
  let short_filename = Cull_string.interval raw_command (i1+4) (i2-1) in 
  {
    Cee_compilation_command_t.short_path = Cull_string.coending 2 short_filename ;
    ending = Cull_string.ending 2 short_filename ;
    core_of_command =Cull_string.beginning (i1-1) raw_command ;
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
      Cee_compilation_command_t.beginning_of_command = String.concat " " before ;
      litany =Image.image (Cull_string.coending 2) middle ;
      end_of_command = String.concat " " after
  }  ;;

(*  
parse_batch "The brown and astute fox.o jumped.o over.o the lazy dog" ;;
*)
let parse raw_command = 
  if Substring.is_a_substring_of " -c " raw_command 
  then Cee_compilation_command_t.Separate(parse_separate raw_command)
  else Cee_compilation_command_t.Batch(parse_batch raw_command) ;;


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

let short_names_for_temporary_files_during_preprocessing separate_cmd  =  
  let ending = separate_cmd.Cee_compilation_command_t.ending in
  (Cee_text.random_marker^"_second"^ending,Cee_text.random_marker^"_third"^ending) ;;
 
end ;;

let adapt_command = Private.adapt_command ;;

let parse = Private.parse ;;
let parse_separate = Private.parse_separate ;;

let short_names_for_temporary_files_during_preprocessing = Private.short_names_for_temporary_files_during_preprocessing ;; 
