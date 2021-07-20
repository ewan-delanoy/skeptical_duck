(*

#use"Text_editing/manage_diary.ml";;

This module manages "diaries", i.e. files consisting of snippets 
of code indexed in increasing order.
The format for each snippet is : 
(**** ... Snippet idx : title ****) snippet_content

Note that in the internal representation, the snippets are stored in reverse order.


*)

module Private = struct

  (* Note that the first letter of the word <<snippet>> can be capitalized or uncapitalized. *)
  let snippet_keyword = "nippet ";;
  
  let blanks =  [' ';'\n';'\r';'\t'] ;;
  let digits = ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9'];;
  
  let snippet_analysis_at_index text idx=
    let i1 = idx + (String.length snippet_keyword) in 
    let i2 = Strung.char_finder_from (fun c->not(List.mem c blanks)) text i1 in 
    if i2 = 0 then None else 
    if not(List.mem (Strung.get text i2) digits) then None else 
    let extended_text = text ^ " " in   
    let i3 = Strung.char_finder_from (fun c->not(List.mem c digits)) extended_text i2 in 
    let i4 = i3 -1 in 
    Some(i2,i4,int_of_string(Cull_string.interval text i2 i4)) ;;
  
  
  let snippet_analysis text =
     let temp1 = Substring.occurrences_of_in snippet_keyword text in 
     Option.filter_and_unpack (snippet_analysis_at_index text) temp1 ;;
  
  let apply_replacements_to_snippet replacements absolute_idx sn_descr sn_indices = 
      if sn_indices = []  
      then ("Snippet ")^(string_of_int absolute_idx)^" : "^sn_descr 
      else
        let changes_to_be_made = Option.filter_and_unpack (
           fun (i_start,i_end,snippet_idx) ->
            match List.assoc_opt snippet_idx replacements with 
            None -> None 
            |Some new_idx -> 
              Some((i_start,i_end),string_of_int(new_idx))
        ) sn_indices in 
        Strung.replace_ranges_in changes_to_be_made sn_descr ;;
  
  let message_to_describe_replacements = function 
    [] -> "No replacement to be made."
    | l -> 
        let temp1 = Image.image (
          fun (x,y) -> (string_of_int x) ^" -> "^(string_of_int y)
        ) l in 
        "The following replacements have been made : \n"^
        (String.concat " , " temp1);;
  
  let announce_replacements replacements =
      let msg = "\n\n"^(message_to_describe_replacements replacements)^"\n\n" in 
      (print_string msg; flush stdout) ;;
  
  
  let beginning_of_opener = "(*******";;
  let beginning_of_closer = "********";;
  
  let long_opener = "("^(String.make 120 '*')^"\n" ;;
  let long_closer = "\n"^(String.make 120 '*')^")" ;;
  
  type line_kind = Opener | Closer | Ordinary_line ;;
  
  let compute_kind line =
    if Supstring.begins_with line beginning_of_opener then Opener else 
    if Supstring.begins_with line beginning_of_closer then Closer else 
    Ordinary_line ;;     
  
  let rec get_next_ordinary_lines (treated,to_be_treated) =
      match to_be_treated with 
      [] -> (treated,[])
      | ((linedex,line),lk) :: other_lines -> match lk with 
      Ordinary_line -> let next_content = (if treated="" then line else treated^"\n"^line) in 
                        get_next_ordinary_lines (next_content,other_lines)
     |_ ->  (treated,to_be_treated) ;; 
  
  exception Get_next_end_of_chunk_exn of int * string ;;
  exception Empty_arg_in_get_next_end_of_chunk_exn ;;
  
  let get_next_end_of_chunk lines =
         match lines with 
         [] -> raise (Empty_arg_in_get_next_end_of_chunk_exn)
         | ((linedex,line),lk) :: other_lines -> match lk with 
         Closer -> get_next_ordinary_lines ("",other_lines) 
        |Opener | Ordinary_line->  raise(Get_next_end_of_chunk_exn(linedex,line)) ;; 
    
  
  exception Get_next_chunk_exn of int * string ;;
  
  let rec get_next_chunk lines =
      match lines with 
      [] -> None
      | ((linedex,line),lk) :: other_lines -> match lk with 
      Opener -> let (text1,other_lines2) = get_next_ordinary_lines ("",other_lines) in 
                let (text2,other_lines3) = get_next_end_of_chunk other_lines2 in 
                Some(text1,text2,other_lines3)
     |Closer | Ordinary_line->  raise(Get_next_chunk_exn(linedex,line)) ;; 
  
  let rec get_all_chunks (treated_chunks,lines) =
      match get_next_chunk lines with 
       None -> treated_chunks 
      |Some(text1,text2,other_lines) -> get_all_chunks ((text1,text2)::treated_chunks,other_lines) ;; 
  
  
  let parse text =
    let lines = Lines_in_string.core text in 
    let lines2 = Image.image (fun (linedex,line)->((linedex,line),compute_kind line)) lines in 
    let (prologue,lines3) = get_next_ordinary_lines ("",lines2) in 
    (prologue,get_all_chunks ([],lines3));;
  
  let unparse pairs =
      let chunks = List.rev_map (fun
        (snippet_description,snippet_content)->
        long_opener ^ snippet_description ^ long_closer ^ "\n" ^ 
        snippet_content 
      ) pairs in 
      String.concat "\n" chunks ;;
  
  let read_and_parse fn = parse (Io.read_whole_file fn) ;;
  let unparse_and_write_to pairs fn = Io.overwrite_with fn (unparse pairs) ;;
  
  let fix_indexation pairs display_reps=
      let temp1 = Image.image (fun 
      (sn_descr,sn_content)->
        (sn_descr,sn_content,snippet_analysis sn_descr)
      ) pairs in 
      let temp2 = Ennig.index_everything temp1 in 
      let replacements = Option.filter_and_unpack (
        fun (absolute_idx,(sn_descr,sn_content,sn_indices)) ->
           match sn_indices with 
           [] -> None 
           | (_,_,idx) :: _ ->
             if idx = absolute_idx then None else Some(idx,absolute_idx)
      ) temp2 in 
      let _ = (if display_reps then announce_replacements replacements) in 
      Image.image (
        fun (absolute_idx,(sn_descr,sn_content,sn_indices)) ->
           let new_sn_descr = 
            apply_replacements_to_snippet replacements absolute_idx sn_descr sn_indices in 
            (new_sn_descr,sn_content)
      ) temp2 ;;
  
  let remove_snippets pairs indices=
    let temp1 = Ennig.index_everything pairs in 
    let pairs2 = Option.filter_and_unpack (fun 
       (idx,pair) -> 
        if List.mem idx indices then None else Some pair
    ) temp1 in 
    fix_indexation pairs2 false ;;
  
  let absorb_new_snippet (prologue,older_snippets) = 
     let n = List.length(older_snippets) + 1 in 
     let sn_descr = "Snippet "^(string_of_int n)^" : " in 
     older_snippets @ [sn_descr,prologue];; 
  
  let fix_indexation_in_file fn =
    let (_,old_pairs) = read_and_parse fn in 
    let new_pairs = fix_indexation old_pairs true in 
    unparse_and_write_to new_pairs fn ;;
      
  let remove_snippets_in_file fn indices =
      let _ = fix_indexation_in_file fn in  
      let (_,old_pairs) = read_and_parse fn in 
      let new_pairs = remove_snippets old_pairs indices in 
      unparse_and_write_to new_pairs fn ;;  
  
  let absorb_new_snippet_in_file fn =
    let (prologue,old_pairs) = read_and_parse fn in 
    let new_pairs = absorb_new_snippet (prologue,old_pairs) in 
    unparse_and_write_to new_pairs fn ;;
  
  exception Nonindexed_empty_snippet of string ;;  

  let empty_snippets pairs =
    Option.filter_and_unpack (fun 
    (sn_descr,sn_content)->
      if (Cull_string.trim_spaces sn_content) <> "" 
      then None  
      else match snippet_analysis sn_descr with 
           [] -> raise ( Nonindexed_empty_snippet sn_descr)
           |(_,_,idx) :: _ -> Some idx
    ) pairs ;;  
  
  let empty_snippets_in_file fn = 
      let (prologue,pairs) = read_and_parse fn in 
      empty_snippets pairs ;;

  end ;; 
  
  let absorb_new_snippet = Private.absorb_new_snippet_in_file ;;
  let empty_snippets = Private.empty_snippets_in_file ;;
  let fix_indexation = Private.fix_indexation_in_file ;;
  let remove_snippets = Private.remove_snippets_in_file ;;
  
  
  
  (*    
  let z4 = 
    [
       "Snippet 2 : A ","aaa";
       "Snippet 3 : B ","bbb";
       "Snippet 4 : C from snippet 2 ","ccc";
       "Snippet 5 : D from snippet 2 and snippet 3","ddd";
       "Snippet 6 : E from snippet 3 and snippet 4 ","eee";
    ];;
  let z5 = fix_indexation z4 true;;
  let z6 = unparse z5;;
  let (z7,z8) = parse z6;;
  let check_invariance = (z8=z5);;
  *)
