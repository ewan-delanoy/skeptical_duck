(*

#use"lib/makefile.ml";;

*)

exception Check_all_are_empty_but_last_exn  ;;

exception List_value_exn of string * ( Makefile_t.variable_assignment list) ;; 

exception Parse_next_instruction_exn of int ;;

exception Prerequisites_and_commands_for_target_exn of string * ( Makefile_t.rule list) ;; 

exception Single_value_exn of string * (string list) ;; 

type walking_index = WI of int * int ;; (* first is line index, second is char index *)

let wi_to_char_index (WI(_,c_idx)) = c_idx ;;
let wi_to_line_index (WI(l_idx,_)) = l_idx ;;
let wi_minus_one text (WI(l_idx,c_idx)) = 
   let new_l_idx = (if Strung.get text (c_idx-1) = '\n' then  l_idx-1 else l_idx) in 
   WI(new_l_idx,c_idx-1) ;;

let wi_plus_one text (WI(l_idx,c_idx)) = 
   let new_l_idx = (if Strung.get text (c_idx+1) = '\n' then  l_idx+1 else l_idx) in 
   WI(new_l_idx,c_idx+1) ;;

let wi_plus_two text wi = wi_plus_one text (wi_plus_one text wi) ;;

let wi_starter text =
   let fst_c_idx = (if String.get text 0 ='\n' then 1 else 0) in 
   WI(fst_c_idx,1)

let rec helper_for_wi_finding (text,text_length,f) walking_idx = 
   let idx = wi_to_char_index walking_idx in 
   if idx > text_length 
   then None 
   else   
   if f (Strung.get text idx) 
   then Some walking_idx 
   else     
   let next_walking_idx = wi_plus_one text walking_idx in    
   helper_for_wi_finding (text,text_length,f) next_walking_idx ;;
            
let wi_to_char_index_finder_from_inclusive_opt mkf_text f walking_idx = 
   let (Makefile_t.MT text) = mkf_text in 
   helper_for_wi_finding (text,String.length text,f) walking_idx ;;
   


let check_all_are_empty_but_last l =
   let (h,t) = List_again.head_with_tail(List.rev l) in
   if List.for_all (fun commands->commands=[]) t 
   then h
   else raise Check_all_are_empty_but_last_exn ;;  

let prerequisites_and_commands_for_target mkf target_name = 
   let rules1 = List.filter (
      fun rule ->
         List.mem target_name rule.Makefile_t.targets
   ) mkf.Makefile_t.rules in 
   try (
   let prerequisites1 = Image.image (fun rule -> rule.Makefile_t.prerequisites) rules1 
   and commands1 = Image.image (fun rule -> rule.Makefile_t.commands) rules1 in 
   let prerequisites = List.flatten prerequisites1
   and commands = check_all_are_empty_but_last commands1  in 
   (prerequisites,commands) 
   ) with 
   Check_all_are_empty_but_last_exn ->
   raise(Prerequisites_and_commands_for_target_exn(target_name,rules1));;
  
let prerequisites_for_target mkf target_name = 
    fst(prerequisites_and_commands_for_target mkf target_name) ;; 


let list_value mkf ~variable_name = 
   let temp1 = List.filter (
         fun assg-> assg.Makefile_t.variable_name = variable_name
   ) mkf.Makefile_t.assignments in 
   if List.length(temp1)>1
   then  raise(List_value_exn(variable_name,temp1))
   else  
   if temp1=[]
   then [""]      
   else
   let assg = List.hd temp1 in 
   assg.Makefile_t.content ;;
let single_value mkf ~variable_name = 
   let vals= list_value mkf ~variable_name in 
   if List.length(vals)<>1 
   then raise(Single_value_exn(variable_name,vals))   
   else List.hd vals ;;


let rec helper_for_target_list_expansion mkf (treated,terminals,to_be_treated) = 
  match to_be_treated with 
  [] -> List.rev treated 
 |(target,already_visited) :: others ->
    if already_visited 
    then  helper_for_target_list_expansion mkf (target::treated,terminals,others)
    else 
    let (prerequisites,commands)  = prerequisites_and_commands_for_target mkf target in 
    if (prerequisites,commands)  = ([],[])
    then  helper_for_target_list_expansion mkf (treated,target::terminals,others) 
    else 
    let old_prerequisites = treated @ terminals in 
    let new_prerequisites = List.filter (fun tgt ->not(List.mem tgt old_prerequisites)) prerequisites in 
    if new_prerequisites = []
    then helper_for_target_list_expansion mkf (target::treated,terminals,others)
    else
    let new_goal = (Image.image (fun x->(x,false)) new_prerequisites)
                    @( (target,true) :: others) in 
    helper_for_target_list_expansion mkf (treated,terminals,new_goal) ;;                    
   
let expand_target_list mkf l = 
  helper_for_target_list_expansion mkf ([],[],Image.image (fun x->(x,false)) l) ;;


let makefile_lines_for_indexed_command (cmd_idx,(cmd_content,comment)) = 
   let s_idx = string_of_int cmd_idx in 
   [
   "\t@echo \"************************************************ Step "^s_idx^":"^comment^"\"";
   "\t"^cmd_content   
   ] ;; 
 
 let write_rule_without_prerequisites ~target_name ~commands = 
  let indexed_commands = Int_range.index_everything commands in 
  "\n"^target_name^":\n" ^
  (String.concat "\n"
  (List.flatten (Image.image makefile_lines_for_indexed_command indexed_commands ))) ^ "\n";;   
 
type instruction = 
     Comment 
    |Rule of Makefile_t.rule 
    |Assignment of Makefile_t.variable_assignment ;;

let rec helper_for_noncancelled_linebreak_finder (text,fst_idx,last_idx) walking_idx= 
 let idx = wi_to_char_index walking_idx in  
 if idx > last_idx 
 then None 
 else    
 if (Strung.get text idx)<>'\n'
 then helper_for_noncancelled_linebreak_finder (text,fst_idx,last_idx) (wi_plus_one text walking_idx)
 else  
 if (idx = fst_idx) 
 then Some walking_idx 
 else
 if (Strung.get text (idx-1))='\\'
 then helper_for_noncancelled_linebreak_finder (text,fst_idx,last_idx) (wi_plus_one text walking_idx)
 else Some walking_idx ;;

let next_noncancelled_linebreak_opt mkf_text start_idx = 
   let (Makefile_t.MT text) = mkf_text in 
   helper_for_noncancelled_linebreak_finder (text,wi_to_char_index start_idx,String.length text) start_idx ;;

(*

let z1 = next_noncancelled_linebreak_opt (Makefile_t.MT "123\\\n6\\\n9\n12") 1 ;; 

*)

let add_interval_if_nonempty  treated text current_start idx=
 if current_start > idx 
 then treated 
 else (Cull_string.interval text current_start idx) :: treated ;;   

let rec helper_for_long_line_parser (text,start_idx,end_idx) (treated,current_start,idx)= 
 let treated2 =add_interval_if_nonempty  treated text current_start (idx-1) in 
 if idx > end_idx 
 then List.rev (treated2)
 else    
 let c = Strung.get text idx in 
 if List.mem c [' ';'\t';'\r']
 then helper_for_long_line_parser (text,start_idx,end_idx) (treated2,idx+1,idx+1)
 else
 if c <> '\n'
 then helper_for_long_line_parser (text,start_idx,end_idx) (treated,current_start,idx+1)
 else  
 if (idx = start_idx) 
 then helper_for_long_line_parser (text,start_idx,end_idx) (treated2,idx+1,idx+1) 
 else
 if (Strung.get text (idx-1))='\\'
 then let treated3 =add_interval_if_nonempty  treated text current_start (idx-2) in 
      helper_for_long_line_parser (text,start_idx,end_idx) (treated3,idx+1,idx+1) 
 else helper_for_long_line_parser (text,start_idx,end_idx) (treated,current_start,idx+1) ;;

let parse_long_line mkf_text start_idx end_idx= 
   let (Makefile_t.MT text) = mkf_text in 
   helper_for_long_line_parser (text,start_idx,end_idx) ([],start_idx,start_idx) ;;

(*
let z2 = parse_long_line (Makefile_t.MT "123\\\n6\\\n9\t12") 1 12;; 
*)

let last_index_before_end_of_line mkf_text start_idx = 
   let (Makefile_t.MT text) = mkf_text in 
   match next_noncancelled_linebreak_opt mkf_text start_idx with 
   (Some walking_idx) -> wi_minus_one text walking_idx 
   | None ->
   let (Makefile_t.MT text) = mkf_text in 
   WI(Strung.number_of_linebreaks text,String.length text);;



let rec helper_for_command_list_parsing (text,text_length) (treated,walking_idx) = 
   let idx = wi_to_char_index walking_idx in  
   if idx > text_length 
   then (List.rev treated,None) 
   else   
   if (Strung.get text idx)<>'\t'
   then (List.rev treated,Some walking_idx) 
   else     
   let next_idx = last_index_before_end_of_line (Makefile_t.MT text)  (wi_plus_one text walking_idx) in 
   let next_cmd = Cull_string.interval text (idx+1) (wi_to_char_index next_idx) in 
   let treated2 = next_cmd::treated in 
   if (wi_to_char_index next_idx)+2 > text_length 
   then (List.rev treated2, None)
   else   
   helper_for_command_list_parsing (text,text_length) (next_cmd::treated,wi_plus_two text next_idx) ;;


let parse_list_of_commands mkf_text start_idx = 
   let (Makefile_t.MT text) = mkf_text in 
   helper_for_command_list_parsing (text,String.length text) ([],start_idx) ;; 


let parse_next_rule mkf_text start_idx sep_idx = 
   let (Makefile_t.MT text) = mkf_text in 
   let tgts = parse_long_line mkf_text (wi_to_char_index start_idx) ((wi_to_char_index sep_idx)-1) in 
   let sep2_idx = last_index_before_end_of_line mkf_text (wi_plus_one text sep_idx) in       
   let prereqs = parse_long_line mkf_text ((wi_to_char_index sep_idx)+1) (wi_to_char_index sep2_idx) in 
   let (cmds,next_idx_opt) = parse_list_of_commands mkf_text (wi_plus_two text sep2_idx)  in 
   ({
      Makefile_t.ru_line_number = wi_to_line_index start_idx;
      targets = tgts ;
      prerequisites = prereqs ;
      commands =cmds ;
   }, next_idx_opt) ;; 


let parse_next_assignment mkf_text start_idx sep_idx = 
   let (Makefile_t.MT text) = mkf_text in 
   let vname = Cull_string.trim_spaces (Cull_string.interval text (wi_to_line_index start_idx) ((wi_to_char_index sep_idx)-1)) in 
   let next_idx_opt = next_noncancelled_linebreak_opt mkf_text sep_idx in 
   let last_idx_before_end_of_line  = 
     ( match next_idx_opt with 
     (Some walking_idx) -> wi_minus_one text walking_idx 
     | None ->
     let (Makefile_t.MT text) = mkf_text in 
     WI(Strung.number_of_linebreaks text,String.length text) ) in    
   let ctnt = parse_long_line mkf_text ((wi_to_char_index sep_idx)+1) (wi_to_char_index last_idx_before_end_of_line) in 
   ({
         Makefile_t.va_line_number = wi_to_line_index start_idx;
         variable_name = vname;
         content = ctnt;
       
   }, next_idx_opt) ;;    

let parse_next_instruction mkf_text nonblank_idx = 
   let (Makefile_t.MT text) = mkf_text in 
   if (Strung.get text (wi_to_char_index nonblank_idx)) = '#'
   then (Comment,next_noncancelled_linebreak_opt mkf_text nonblank_idx)
   else  
   match wi_to_char_index_finder_from_inclusive_opt mkf_text (fun c->
          List.mem c [':';'='])  (wi_plus_one text nonblank_idx) with 
    None -> raise (Parse_next_instruction_exn(wi_to_char_index nonblank_idx))
   |Some sep_idx ->
      if (Strung.get text (wi_to_char_index sep_idx)) = ':'
      then let (rule,next_idx_opt) = parse_next_rule mkf_text nonblank_idx sep_idx in 
           (Rule rule,next_idx_opt)
      else let (assg,next_idx_opt) = parse_next_assignment mkf_text nonblank_idx sep_idx in 
           (Assignment assg,next_idx_opt) ;;



let parse_next_instruction_if_there_is_one mkf_text first_c_idx_in_line = 
  match wi_to_char_index_finder_from_inclusive_opt mkf_text ( 
     fun c-> not(List.mem (c) [' ';'\t';'\r';'\n'])
  ) first_c_idx_in_line with 
  None -> (None,None)
  |Some nonblank_idx -> 
     let (instr,next_idx_opt)=parse_next_instruction mkf_text nonblank_idx in 
     (Some instr,next_idx_opt) ;;

let empty_one = { 
      Makefile_t.assignments = []; 
      Makefile_t.rules = []
   } ;;     

let rev_both mkf = { 
   Makefile_t.assignments = List.rev(mkf.Makefile_t.assignments); 
   Makefile_t.rules = List.rev(mkf.Makefile_t.rules)
} ;;

let add_assignment assg mkf = { 
  mkf with    
   Makefile_t.assignments = assg :: (mkf.Makefile_t.assignments); 
} ;;

let add_rule rule mkf = { 
   mkf with 
   Makefile_t.rules = rule :: (mkf.Makefile_t.rules)
} ;;


let rec helper_for_makefile_parsing (text,text_length) (treated,walking_idx) = 
   let idx = wi_to_char_index walking_idx in  
   if idx > text_length 
   then rev_both treated
   else 
   let (instr_opt,next_idx_opt) =  parse_next_instruction_if_there_is_one (Makefile_t.MT text) walking_idx in 
   if instr_opt = None 
   then rev_both treated 
   else     
   let treated2 = (   
      match Option.get instr_opt with 
       Comment -> treated 
       |Rule rule -> add_rule rule treated 
       |Assignment assg -> add_assignment assg treated   
   ) in 
   match next_idx_opt with 
    None -> rev_both treated2 
   |Some next_idx -> helper_for_makefile_parsing (text,text_length) (treated2,next_idx) ;;


let parse_makefile mkf_text = 
   let (Makefile_t.MT text) = mkf_text in 
   helper_for_makefile_parsing (text,String.length text) (empty_one,wi_starter text) ;;

   

(*

let s_mt1 = 
   (String.concat "\n"
   ["arthur: a.txt"; "\t@echo \"This is Arthur 1\""; "belinda: b.txt";
   "\t@echo \"This is Belinda\""; "arthur: c.txt";
   "\t@echo \"This is Arthur 2\""; "arthur: d.txt";
   "\t@echo \"This is Arthur 3\"\t"; "a.txt:"; "\tcp origin.txt a.txt"; "b.txt:";
   "\tcp origin.txt b.txt\t"; "c.txt:"; "\tcp origin.txt c.txt\t"; "d.txt:";
   "\tcp origin.txt d.txt\t\t\t"; "clean:"; "\trm -f a.txt b.txt c.txt d.txt";
   "\ttouch what_gnu_make_did.txt \t"; ""] 
   ) ;; 

let mt1 = Makefile_t.MT s_mt1;; 

let ap2= Absolute_path.of_string "~/Teuliou/Experimenting_with_php/copiableMakefile";;

let mt2  =  Makefile_t.MT(Io.read_whole_file ap2) ;; 

let see2 = parse_makefile mt2 ;;

let assgs2 = see2.Makefile_t.assignments ;; 

let assg = List.nth assgs2 50;;

let rules2 = see2.Makefile_t.rules ;; 

let rule = List.nth rules2 340;;

*)

