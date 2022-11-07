(*
   
#use"watched/watched_and_githubbed/mass_editing_of_php_topic.ml";;

*)

(*

CHAPTER 1 : Call mysql from ocaml

*)

Sys.command "echo $MYSQL";;


Unix.putenv "CTL" "/Applications/mampstack-7.3.13-0/ctlscript.sh";;
Unix.putenv "MYSQL" "/Applications/mampstack-7.3.13-0/mysql/bin/mysql";;

let usual_dir = Dfa_root.without_trailing_slash 
Coma_big_constant.This_World.root ;; 

let main_dir = usual_dir ^ 
   "/nonml_files/nongithubbed_nonml_files/Mass_editing_of_phpbb_topic/";;

let read_file fn=
  Io.read_whole_file(Absolute_path.of_string(main_dir^fn));;

let write_to_file txt fn=
  Io.overwrite_with (Absolute_path.of_string(main_dir^fn)) txt;;

let put_mysql_output_in_file mysql_cmd fn=
   let c=Unix_command.hardcore_uc and ch=(fun s->let _=Sys.chdir s in 0) in 
   let i1=c ("touch "^main_dir^fn) in 
   let i2=ch main_dir in 
   let i3=c ("$MYSQL  --login-path=local -e \""^mysql_cmd^"\" local_micael_database > "^fn) in 
   let i4=ch usual_dir in 
   [
       i1;i2;i3;i4
   ];;

let ask_mysql_for_output mysql_cmd =
   let fn = "carrier.txt" in 
   let _=put_mysql_output_in_file mysql_cmd fn in 
   read_file fn;;

let absorb_mysql_file fn=
   let c=Sys.command and ch=(fun s->let _=Sys.chdir s in 0) in 
   let i1=ch main_dir in 
   let i2=c ("$MYSQL  --login-path=local local_micael_database < "^fn) in 
   let i3=ch main_dir in 
   [
       i1;i2;i3
   ];;

(*

CHAPTER 2 : Implement emptying of database

*)


let droplist = 
  "DROP TABLE IF EXISTS mysql_table_acl_groups, mysql_table_acl_options, mysql_table_acl_roles,"^
  " mysql_table_acl_roles_data, mysql_table_acl_users, mysql_table_attachments, "^
  "mysql_table_banlist, mysql_table_bbcodes, mysql_table_bookmarks, "^
  "mysql_table_bots, mysql_table_captcha_answers, mysql_table_captcha_questions,"^
  " mysql_table_config, mysql_table_config_text, mysql_table_confirm, "^
  "mysql_table_disallow, mysql_table_drafts, mysql_table_ext, mysql_table_extensions, "^
  "mysql_table_extension_groups, mysql_table_forums, mysql_table_forums_access, "^
  "mysql_table_forums_track, mysql_table_forums_watch, mysql_table_groups, "^
  "mysql_table_icons, mysql_table_lang, mysql_table_log, mysql_table_login_attempts, "^
  "mysql_table_migrations, mysql_table_moderator_cache, mysql_table_modules, "^
  "mysql_table_notifications, mysql_table_notification_types, "^
  "mysql_table_oauth_accounts, mysql_table_oauth_states, mysql_table_oauth_tokens, "^
  "mysql_table_poll_options, mysql_table_poll_votes, mysql_table_posts, "^
  "mysql_table_privmsgs, mysql_table_privmsgs_folder, mysql_table_privmsgs_rules, "^
  "mysql_table_privmsgs_to, mysql_table_profile_fields, mysql_table_profile_fields_data, "^
  "mysql_table_profile_fields_lang, mysql_table_profile_lang, mysql_table_qa_confirm, "^
  "mysql_table_ranks, mysql_table_reports, mysql_table_reports_reasons, "^
  "mysql_table_search_results, mysql_table_search_wordlist, mysql_table_search_wordmatch, "^
  "mysql_table_sessions, mysql_table_sessions_keys, mysql_table_sitelist, "^
  "mysql_table_smilies, mysql_table_sphinx, mysql_table_styles, mysql_table_teampage, "^
  "mysql_table_topics, mysql_table_topics_posted, mysql_table_topics_track, "^
  "mysql_table_topics_watch, mysql_table_users, mysql_table_user_group, "^
  "mysql_table_user_notifications, mysql_table_warnings, mysql_table_words, "^
  "mysql_table_zebra;" ;;
  
  
  let drop_all () = ask_mysql_for_output droplist;;
  
(*

CHAPTER 3 : Implement database reinitialization

The "restarter.sql" file comes from a backup_larchang operation.

Note that restarting is slow (18 secs last time I checked)

*)
  
let current_restarter = "restarter.sql";;

let restart ()= absorb_mysql_file current_restarter;;

(*

CHAPTER 4 : Post indices in topic

*)

let post_indices_in_topic =Memoized.make(fun topic_idx ->
    let s_topic_idx = string_of_int topic_idx in 
    let temp1 = ask_mysql_for_output 
    ("SELECT post_id FROM mysql_table_posts WHERE topic_id = "
      ^s_topic_idx^" ;") in
    let temp2 = Cull_string.cobeginning 8 temp1 in   
    let temp3 = Str.split (Str.regexp_string "\n") temp2 in 
    Ordered.sort Total_ordering.standard 
    (Image.image int_of_string temp3));;

(*    
let indices = post_indices_in_topic 1149 ;; 
*)

(*

CHAPTER 5 : Getters and setters 

*)

let number_of_posts topic_idx = List.length (post_indices_in_topic topic_idx) ;;

let get_post =Memoized.make(fun (topic_idx,idx) ->
  let indices =  post_indices_in_topic topic_idx in 
  let post_idx = List.nth indices (idx-1) in 
   Cull_string.cobeginning 10 (ask_mysql_for_output 
   ("SELECT post_text FROM mysql_table_posts WHERE "^
   "post_id = "^(string_of_int (post_idx))^" ;")));;

let set_post topic_idx idx replacement = 
  let indices =  post_indices_in_topic topic_idx in 
   let post_idx = List.nth indices (idx-1) in 
   ask_mysql_for_output 
   ("UPDATE mysql_table_posts SET post_text='"^
    replacement^"' WHERE post_id = "^(string_of_int (post_idx))^" ;");;

let see_post topic_idx idx = 
  print_string("\n\n\n"^(get_post (topic_idx,idx))^"\n\n\n");;

(*

CHAPTER 6 : Read table of contents 

*)

let url_starter = "<s>[url=" ;;

let detect_phpbb_url_start_at_index s idx =
   if not(Substring.is_a_substring_located_at url_starter s idx)
   then None 
   else 
   let idx2=Substring.leftmost_index_of_in_from "]" s (idx+(String.length url_starter)) in 
   Some(idx2-idx+1,(idx,idx2));;

let url_ender = "[/url]";;

let detect_phpbb_url_end_at_index s idx =
   if Substring.is_a_substring_located_at url_ender s idx
   then let e = String.length url_ender in 
        Some(e,(idx,idx+e-1))
   else None ;;

let beautify_section_path x= 
   if x="" then "Pr\195\169face, Cardinal Manning " else 
   if x="PREMIÈRE PARTIE" then "I, " else 
   if x="DEUXIÈME PARTIE" then "II, " else
   x^", ";;

let extract_post_index_in_topic s topic_idx = 
  let indices =  post_indices_in_topic topic_idx in 
   let i1=Substring.leftmost_index_of_in "p=" s 
   and i2=Substring.leftmost_index_of_in "#" s  in 
   Listennou.find_index (int_of_string(Cull_string.interval s (i1+2) (i2-1))) indices;;

let extract_section_path s = 
   let i1=Substring.leftmost_index_of_in "." s in 
   beautify_section_path(Cull_string.beginning  (i1-1) s);;

let dissect topic_idx idx  (opt,content) = 
  let temp1 = get_post (topic_idx,idx) in
  match opt with  
  None -> None 
  |Some(label,pair)->
     let (i,j) = fst pair in
     Some(extract_post_index_in_topic 
     (Cull_string.interval temp1 i j) topic_idx,
      extract_section_path(Cull_string.two_sided_cutting ("</s>","<e>") content));;

let prepare_sections_for_posts topic_idx idx = 
  let indices =  post_indices_in_topic topic_idx in 
  let temp1 = get_post (topic_idx,idx) in   
  let temp2 = Functional_parenthesed_block.decompose_without_taking_blanks_into_account 
      [("php","bb"),detect_phpbb_url_start_at_index,detect_phpbb_url_end_at_index] temp1 in 
  let temp3 = Option.filter_and_unpack (dissect topic_idx idx) temp2 in 
  let (last_mentioned_idx,last_section) = List.hd (List.rev temp3) in 
  let last_range = Int_range.scale (fun i->(i,last_section)) last_mentioned_idx (List.length indices)
  and temp4 = Listennou.universal_delta_list temp3 in 
  (Image.image (
   fun ((i1,section1),(i2,_)) ->
     Int_range.scale (fun i->(i,section1)) (i1) (i2-1)
) temp4) @ [last_range] ;;

let sections_for_posts topic_idx idx = 
  List.flatten (prepare_sections_for_posts topic_idx idx);;

(*

CHAPTER 7 : Extract sections from quotes

*)

exception Substring_not_found of int * string ;;

let forced_leftmost_index_from helper pattern whole_string start =
   let j= Substring.leftmost_index_of_in_from pattern whole_string start in 
   if j<0 then raise(Substring_not_found(helper,pattern)) else j;;

let part1 = "<QUOTE author=\"";;
let part2 = "\"><s>[quote=\"";;
let part3 = "\"]</s>";;

let analize post_idx post=
   let i1 = forced_leftmost_index_from post_idx  part1 post 1 in 
   let j1 = i1 + (String.length part1) -1 in 
   let i2 = forced_leftmost_index_from post_idx  part2 post (j1+1) in 
   let j2 = i2 + (String.length part2) -1 in
   let i3 = forced_leftmost_index_from post_idx  part3 post (j2+1) in 
   (i1,j1,i2,j2,i3) ;;

let az_test post_idx post =
    let  (i1,j1,i2,j2,i3) = analize post_idx post in 
    (Cull_string.interval post (j1+1) (i2-1)) <>  
    (Cull_string.interval post (j2+1) (i3-1));; 

let old_posts =Memoized.make(fun (topic_idx,fst_idx,last_idx) -> 
  let mapper = (fun post_idx -> (post_idx,get_post (topic_idx,post_idx))) in 
  Chronometer.it Explicit.image mapper (Int_range.range fst_idx last_idx));;

let should_be_empty1 fst_idx last_idx= 
    List.filter (fun (post_idx,post) -> az_test post_idx post) 
    (fst_idx last_idx) ;;


let change_section post_idx post new_section=
   let n = String.length post in 
   let  (i1,j1,i2,j2,i3) = analize post_idx post in 
   (Cull_string.interval post 1 j1)^new_section^
   (Cull_string.interval post i2 j2)^new_section^
   (Cull_string.interval post i3 n);;

let new_posts topic_idx idx fst_idx last_idx = 
   let sfp = sections_for_posts topic_idx idx in 
  Image.image (
   fun (post_idx,old_post) ->
       let new_section = "SWS, Livre I, "^(List.assoc post_idx sfp)^"traduit par le chartreux" in 
       let new_post = change_section post_idx old_post new_section in 
       (post_idx,new_post)
) (old_posts (topic_idx,fst_idx,last_idx)) ;;

(*

CHAPTER 8 : Aggregate mysql commands

*)

let escape_single_quote = Replace_inside.replace_inside_string ("'","\\'");;

let command_for_new_post topic_idx (post_idx,new_post) =
  let indices =  post_indices_in_topic topic_idx in 
    let effective_idx = List.nth indices (post_idx-1) in 
    "UPDATE mysql_table_posts SET post_text = '"^
    (escape_single_quote new_post)^
    "' WHERE mysql_table_posts.post_id = "^(string_of_int effective_idx)^" ;" ;;

let mysql_commands topic_idx idx fst_idx last_idx = Image.image 
 (command_for_new_post topic_idx) 
  (new_posts topic_idx idx fst_idx last_idx);;

let aggregate topic_idx idx fst_idx last_idx= 
   let temp1 = String.concat "\n" (""::(mysql_commands topic_idx idx fst_idx last_idx)@[""]) in 
    Replace_inside.replace_inside_string ("\\n","\n") temp1 ;;

let current_fixer = "fixer.sql";;

let store_commands topic_idx idx fst_idx last_idx = 
  write_to_file (aggregate topic_idx idx fst_idx last_idx) current_fixer;;

(*

Note that fixer.sql may be compressed by : gzip --best -c fixer.sql > fixer.sql.zip 

*)

(*

CHAPTER 9 : Remove phpbb annotations

*)

let rec to_simplified_string = function 
   Phpbb_text_with_quotes_t.Atom(text) -> text 
   |Concatenated(l) -> String.concat "" (Image.image to_simplified_string l) 
   |Quoted(_,content) -> to_simplified_string  content ;; 

let remove_quote_related_annotations text =
   to_simplified_string(Phpbb_text_with_quotes.parse text);;

let yrl_starter = "<URL" ;;

let detect_phpbb_yrl_start_at_index s idx =
   if not(Substring.is_a_substring_located_at yrl_starter s idx)
   then None 
   else 
   let idx2=Substring.leftmost_index_of_in_from ">" s (idx+(String.length yrl_starter)) in 
   Some(idx2-idx+1,(idx,idx2));;

let yrl_ender = "</URL>";;

let detect_phpbb_yrl_end_at_index s idx =
   if Substring.is_a_substring_located_at yrl_ender s idx
   then let e = String.length yrl_ender in 
        Some(e,(idx,idx+e-1))
   else None ;;

let remove_yrl_related_annotations text =
   let ttemp1 = Functional_parenthesed_block.decompose_without_taking_blanks_into_account 
    [("php","bb"),detect_phpbb_yrl_start_at_index,detect_phpbb_yrl_end_at_index] text in 
   String.concat "" (Image.image snd ttemp1)
;;

let remove_annotations text =
   let text1 = remove_quote_related_annotations text in 
   let text2 = remove_yrl_related_annotations text1 in 
   Replace_inside.replace_several_inside_string 
   [
       "<s>","";"</s>","";"<e>","";"</e>","";"\\n","\n";
       "<r>","";"</r>","";"<I>","";"</I>","";"(\195\160 suivre)","";
   ] text2 ;; 

let shortened_posts topic_idx = Explicit.image 
  (fun k-> remove_annotations(get_post (topic_idx,k))) 
    (Int_range.range 1 (number_of_posts topic_idx)) ;;

let shortened_text topic_idx = String.concat "\n" (shortened_posts topic_idx) ;;

let store_shortened_text topic_idx = 
    write_to_file (shortened_text topic_idx) "part_ws.txt" ;;


    