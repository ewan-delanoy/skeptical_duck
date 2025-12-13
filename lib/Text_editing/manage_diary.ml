(*

#use"lib/Text_editing/manage_diary.ml";;

This module manages "diaries", i.e. files consisting of entries 
of code indexed in increasing order.
The format for each entry is : 
(**** ... Entry idx : title ****) entry_content

Note that in the internal representation, the entries are stored in reverse order.


*)

module Private = struct

type unindexed_entry = {
   unindexed_summary : string ;
   unindexed_content : string ;
} ;; 

type entry = {
   index : int ;
   summary : string ;
   content : string ;
} ;; 

type diary = {
   global_prologue : string option ;
   entries : entry list ;
} ;;

module Common = struct 

let long_opener = "("^(String.make 120 '*') ;;
let long_closer = (String.make 120 '*')^")" ;;  

let index_entries_naturally unindexed_entries = 
  let temp = Int_range.index_everything unindexed_entries in 
  List.rev_map (
    fun (idx,uent)-> {
   index =idx ;
   summary = uent.unindexed_summary ;
   content = uent.unindexed_content ;
   } 
  ) temp;;

let usual_container = 
  Absolute_path.of_string(
   Dfn_common.recompose_potential_absolute_path 
    Fw_big_constant.This_World.root   
      Fw_constant.rootless_path_for_diary_file);;


end ;;  

module Modify = struct

let modify_at_index dy f k =
  let new_entries = Image.image (
    fun ent ->
      if ent.index = k 
      then {ent with content = f(ent.content)}
     else ent  
  ) dy.entries in 
  {
    dy with entries = new_entries
  } ;;

let replace_at_index_with_content dy k new_content=
  modify_at_index dy (fun _->new_content) k;;

let unindexed_version ent = {
   unindexed_summary = ent.summary;
   unindexed_content = ent.content ;
} ;; 

let remove_at_indices dy indices= 
  let unindexed_retained_ones = List.filter_map (
    fun ent -> 
       if List.mem ent.index indices 
       then None 
       else Some(unindexed_version ent) 
  ) dy.entries in 
  let retained_ones = Common.index_entries_naturally unindexed_retained_ones in 
  { dy with entries=retained_ones} ;;  

let diary_size dy = match dy.entries with 
  [] -> 0 
  | last_entry :: _ -> last_entry.index ;;

let add_fresh_entry dy ~summary_ ~content_=
   let new_entry = {
    index = (diary_size dy)+1;
    summary=summary_;
    content=content_;
   } in 
   {
     dy with 
     entries = new_entry :: dy.entries 
   } ;;  
  
end ;;

module Parse = struct

  

let find_line_starting_with prefix lines =
  List_again.find_and_remember_opt (String.starts_with ~prefix) lines ;; 

let find_opener = find_line_starting_with Common.long_opener ;;
let find_closer = find_line_starting_with Common.long_closer ;;

let enforce_nonempty short_text =
    let culled_text = Cull_string.trim_spaces short_text in 
    if culled_text = "" then None else Some culled_text ;;

let parse_summary unparsed_summary = 
  match String.index_opt unparsed_summary ':' with 
  None -> unparsed_summary 
  |Some j -> 
    let k=(
      if String.get unparsed_summary (j+1)=' '
      then j+2
      else j+1) in 
    Cull_string.cobeginning k unparsed_summary;;

let remove_module_wrapper possibly_wrapped_text = 
  let indexed_lines = Lines_in_text.indexed_lines possibly_wrapped_text in 
  let first_nonblank_line_opt = List.find_opt (
    fun  (_,line)->(Cull_string.trim_spaces line)<>""
  ) indexed_lines
  and last_nonblank_line_opt = List.find_opt (
    fun   (_,line)->(Cull_string.trim_spaces line)<>""
  ) (List.rev indexed_lines) in 
  if (first_nonblank_line_opt=None)||(last_nonblank_line_opt=None)
  then possibly_wrapped_text
  else
  let (idx1,line1) = Option.get first_nonblank_line_opt
  and (idx2,line2) = Option.get last_nonblank_line_opt in 
  if (not(String.starts_with line1 ~prefix:"module "))
     ||(not(List.mem (Cull_string.trim_spaces line2) ["end;;";"end ;;"]))
  then possibly_wrapped_text
  else 
    let retained_lines = List.filter_map (
      fun (idx,line) -> 
        if (idx1<idx)&&(idx<idx2)
        then Some line 
        else None   
    ) indexed_lines in 
    String.concat "\n" retained_lines ;;      
 
let rec helper_for_whole_diary_parsing (treated,lines_after_first_opener) =
   match find_closer lines_after_first_opener with 
   None -> 
    {
      unindexed_summary ="" ;
      unindexed_content =remove_module_wrapper(String.concat "\n" lines_after_first_opener) ;
    } :: treated 
  |Some(before,_,after) ->
    let smry = parse_summary (String.concat "\n" before) in 
     match find_opener after with 
   None -> 
    {
      unindexed_summary =smry ;
      unindexed_content =remove_module_wrapper(String.concat "\n" after) ;
    } :: treated
  |Some(before2,_,after2) ->
    let treated2 = {
      unindexed_summary =smry ;
      unindexed_content =remove_module_wrapper(String.concat "\n" before2) ;
    } :: treated in 
    helper_for_whole_diary_parsing (treated2,after2);;       


let parse_whole_diary diary_text =
   let lines = Lines_in_text.lines diary_text in 
   match find_opener lines with 
   None -> {
              global_prologue = enforce_nonempty diary_text ;
              entries = [] ;
           }
  |Some(before,_,after) ->
    {
        global_prologue = enforce_nonempty (String.concat "\n" before) ;
        entries = Common.index_entries_naturally(
          helper_for_whole_diary_parsing ([],after)) ;
    } ;;       

  end ;;

module Write = struct 

let write_prologue = function 
  None -> ""
  |Some prologue -> prologue^"\n" ;;

let write_entry ent =
  Common.long_opener ^ "\n" ^
  " Entry "^(string_of_int ent.index)^" : "^ent.summary^
  "\n" ^ Common.long_closer ^ 
  "\nmodule Snip"^(string_of_int ent.index)^" = struct \n"^
  ent.content^
  "\nend;;\n" ;;

let write_diary dy =
  (write_prologue dy.global_prologue)^
  (String.concat "\n" (Image.image write_entry dy.entries)) ;;  

end ;;

module Give_and_Receive = struct


   let starter_for_snippet_origin_mention =
     "(* The first draft of this was initially extracted "^
    "from entry " ;;

    let rec first_nonblank_line_with_rest indexed_lines = 
   match indexed_lines with 
   [] -> None
   |(idx1,line1) :: others ->
      let culled1 = Cull_string.trim_spaces line1 in 
      if culled1<>""
      then Some((idx1,culled1),others)
      else first_nonblank_line_with_rest others ;; 

let last_index_in_header indexed_lines =
   match first_nonblank_line_with_rest indexed_lines with 
   None -> 0
   |Some((_,culled1),indexed_lines2)->
     if culled1<>"(*" then 0 else 
   match first_nonblank_line_with_rest indexed_lines2 with 
   None -> 0
   |Some((_,culled2),indexed_lines3)->
     if not(String.starts_with culled2 ~prefix:"#use\"") then 0 else  
    match first_nonblank_line_with_rest indexed_lines3 with 
   None -> 0
   |Some((idx3,culled3),indexed_lines4)->
     if culled3<>"*)" then 0 else     
       match first_nonblank_line_with_rest indexed_lines4 with 
   None -> idx3
   |Some((idx4,culled4),indexed_lines5)->
     if not(String.starts_with culled4 ~prefix:"open Skeptical_duck_lib ") 
     then idx3 
     else
    match indexed_lines5 with 
    [] -> idx4 
    |(idx5,line5) :: _ -> 
       if not(String.starts_with line5 ~prefix:"open Needed_values ") 
      then idx4 
      else idx5 ;;


let remove_header_and_mention_of_snippet_origin indexed_lines last_idx_in_header=
  let retained_lines = List.filter_map (
    fun (idx,line) ->
      if (idx<=last_idx_in_header) || 
        (String.starts_with line ~prefix:starter_for_snippet_origin_mention)
      then None 
      else Some line
  ) indexed_lines in 
  String.concat "\n" retained_lines ;;

let extract_header indexed_lines last_idx_in_header=
   let retained_lines = List.filter_map (
    fun (idx,line) ->
      if (idx>last_idx_in_header)
      then None 
      else Some line
  ) indexed_lines in 
  String.concat "\n" retained_lines ;;

let extract_at_index_and_append_to_file dy k ap = 
  let ent = List.find (fun ent->ent.index = k) dy.entries in 
  let decorated_content =
  "\n"^starter_for_snippet_origin_mention^(string_of_int k)^
  " of diary *)"^ent.content in     
  Io.append_string_to_file decorated_content ap ;;

let clean_filecontent raw_file_content = 
  let raw_indexed_lines = Lines_in_text.indexed_lines raw_file_content in 
  let idx0 = last_index_in_header raw_indexed_lines in 
  let cleaned_filecontent = remove_header_and_mention_of_snippet_origin raw_indexed_lines idx0 
      and header = extract_header raw_indexed_lines idx0 in 
  (header,cleaned_filecontent) ;;    

let transfer_file_content_to_fresh_entry dy ?(summary="") ap ~erase_original=
  let raw_file_content = Io.read_whole_file ap in 
  let (header,cleaned_content) = clean_filecontent raw_file_content in 
  let _=(if erase_original then Io.overwrite_with ap header) in 
  Modify.add_fresh_entry dy ~summary_:summary ~content_:cleaned_content;;

 let replace_at_index_with_file_content dy k ap ~erase_original=   
   let raw_file_content = Io.read_whole_file ap in 
  let (header,cleaned_content) = clean_filecontent raw_file_content in 
  let _=(if erase_original then Io.overwrite_with ap header) in 
  Modify.replace_at_index_with_content dy k cleaned_content;;


end ;;

module With_container = struct 

let extract_at_index_and_append_to_file fn k ap = 
  let dy = Parse.parse_whole_diary(Io.read_whole_file fn) in 
  Give_and_Receive.extract_at_index_and_append_to_file dy k ap ;;

let remove_at_indices fn indices= 
  let old_dy = Parse.parse_whole_diary(Io.read_whole_file fn) in 
  let new_dy = Modify.remove_at_indices old_dy indices in 
  Io.overwrite_with fn (Write.write_diary new_dy) ;;  

let replace_at_index_with_file_content fn k ap ~erase_original=   
   let old_dy = Parse.parse_whole_diary(Io.read_whole_file fn) in 
  let new_dy = Give_and_Receive.replace_at_index_with_file_content old_dy k ap ~erase_original in 
  Io.overwrite_with fn (Write.write_diary new_dy) ;; 
  
let transfer_file_content_to_fresh_entry fn ?(summary="") ap ~erase_original=    
   let old_dy = Parse.parse_whole_diary(Io.read_whole_file fn) in 
  let new_dy = Give_and_Receive.transfer_file_content_to_fresh_entry old_dy ~summary ap ~erase_original in 
  Io.overwrite_with fn (Write.write_diary new_dy) ;; 


end ;;  

module For_Nongithubbed_files = struct 

let prefix_for_nongithubbed_files =  "watched/watched_not_githubbed/";;

let expand short_path = Absolute_path.of_string
  (prefix_for_nongithubbed_files^ short_path ^ ".ml");;

let extract_at_index_and_append_to_file fn k ~nongithubbed_path = 
  With_container.extract_at_index_and_append_to_file fn k 
    (expand nongithubbed_path) ;;

let replace_at_index_with_file_content fn k ~nongithubbed_path ~erase_original=   
  With_container.replace_at_index_with_file_content fn k 
    (expand nongithubbed_path) ~erase_original;; 
  
let transfer_file_content_to_fresh_entry ?(summary="") ~nongithubbed_path fn ~erase_original=    
  With_container.transfer_file_content_to_fresh_entry fn ~summary 
    (expand nongithubbed_path) ~erase_original;; 
  
end ;;  

end ;;


let extract_at_index_and_append_to_file k ~nongithubbed_path = 
 Private.For_Nongithubbed_files.extract_at_index_and_append_to_file 
  Private.Common.usual_container k ~nongithubbed_path ;;

let remove_at_indices indices= 
  Private.With_container.remove_at_indices Private.Common.usual_container indices ;;  

let replace_at_index_with_file_content ?(erase_original=true) k  ~nongithubbed_path =   
   Private.For_Nongithubbed_files.replace_at_index_with_file_content 
    Private.Common.usual_container k ~nongithubbed_path ~erase_original ;;
  
let transfer_file_content_to_fresh_entry ?(summary="") ?(erase_original=true) ~nongithubbed_path () =    
  Private.For_Nongithubbed_files.transfer_file_content_to_fresh_entry
    ~summary ~nongithubbed_path Private.Common.usual_container ~erase_original;; 




(*
let entry1 = {
  index=1;
  summary="abcdef";
  content="abc\ndef"
} ;;

let entry2 = {
  index=2;
  summary="ghij";
  content="\ngh\nij\n"
} ;;

let entry3 = {
  index=3;
  summary="klmno";
  content="\nklmno"
} ;;

let entry4 = {
  index=4;
  summary="pqr";
  content="pqr\n"
} ;;

let dy1 = {
  global_prologue = Some "Here is my prologue";
  entries = [entry4;entry3;entry2;entry1]
} ;;

let text1 = write_diary dy1 ;;

let dy2 = parse_whole_diary text1 ;;

*)


(*

let lines = Lines_in_text.lines text1 ;;

let (before,_,after) = Option.get (find_opener lines);;

let temp = helper_for_whole_diary_parsing ([],after) ;;

let treated = []
and lines_after_first_opener = after ;;

let (before,_,after) = Option.get (find_closer lines_after_first_opener);;

let (before2,_,after2) = Option.get (find_opener after);;

let possibly_wrapped_text = String.concat "\n" before2 ;;

let bad1 = remove_module_wrapper possibly_wrapped_text ;;

let indexed_lines = Lines_in_text.indexed_lines possibly_wrapped_text ;;

let first_nonblank_line_opt = List.find_opt (
    fun  (_,line)->(Cull_string.trim_spaces line)<>""
  ) indexed_lines
and last_nonblank_line_opt = List.find_opt (
    fun   (_,line)->(Cull_string.trim_spaces line)<>""
  ) (List.rev indexed_lines) ;;

let (idx1,line1) = Option.get first_nonblank_line_opt
  and (idx2,line2) = Option.get first_nonblank_line_opt



*)

