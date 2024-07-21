(*

#use"lib/makefile.ml";;

*)


exception List_value_exn of string * ( Makefile_t.variable_assignment list) ;; 

exception Parse_next_instruction_exn of int ;;

exception Prerequisites_and_commands_for_target_exn of string * ( Makefile_t.rule list) ;; 

exception Single_value_exn of string * (string list) ;; 


module Private = struct 

exception Check_all_are_empty_but_last_exn  ;;

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
   if l = [] then [] else
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
   raise(Prerequisites_and_commands_for_target_exn(target_name,rules1))  ;;
  
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
     Rule of Makefile_t.rule 
    |Assignment of Makefile_t.variable_assignment 
    |Inclusion of Makefile_t.inclusion ;;



let parse_next_rule first_eline next_elines sep_idx = 
   let (_c_idx,l_idx,line1) = first_eline in 
   let tgts = Str.split (Str.regexp "[ \t\r]+") (Cull_string.beginning (sep_idx-1) line1) 
   and prereqs = Str.split (Str.regexp "[ \t\r]+") (Cull_string.cobeginning sep_idx line1 ) in 
   let (tabbed_elines,further_elines) = 
    Hurried.partition_in_two_parts (fun (_,_,line)->String.starts_with ~prefix:"\t" line)  next_elines in 
   let cmds= Image.image (fun (_,_,line) -> Cull_string.cobeginning 1 line) tabbed_elines in 
   ({
      Makefile_t.ru_line_number = l_idx;
      targets = tgts ;
      prerequisites = prereqs ;
      commands =cmds ;
   }, further_elines) ;; 


let parse_next_assignment first_eline next_elines sep_idx = 
   let (_c_idx,l_idx,line1) = first_eline in 
   let vname = Cull_string.trim_spaces (Cull_string.interval line1 1 (sep_idx-1)) 
   and vcontent = Str.split (Str.regexp "[ \t\r]+") (Cull_string.cobeginning sep_idx line1 )  in 

   ({
         Makefile_t.va_line_number = l_idx;
         variable_name = vname;
         content = vcontent;
       
   }, next_elines) ;;    

let prefix_for_inclusion = "-include ";;

let parse_next_inclusion first_eline next_elines = 
   let (_c_idx,l_idx,line1) = first_eline in 
   let line2 = Cull_string.two_sided_cutting (prefix_for_inclusion,"") line1 in 
({
   Makefile_t.inc_line_number = l_idx;
   included_ones = Str.split (Str.regexp "[ \t\r]+") line2;
 
}, next_elines) ;;    


let parse_next_instruction first_eline next_elines = 
   let (_c_idx,l_idx,line1) = first_eline in 
   if String.starts_with ~prefix:prefix_for_inclusion line1
   then   let (incl,remaining_lines) = parse_next_inclusion first_eline next_elines  in 
         (Inclusion incl,remaining_lines)      
   else     
   match Strung.char_finder_from_inclusive_opt  (fun c->
          List.mem c [':';'=']) line1 1 with 
    None -> raise (Parse_next_instruction_exn(l_idx))
   |Some sep_idx ->
      if (Strung.get line1 sep_idx) = ':'
      then let (rule,remaining_lines) = parse_next_rule first_eline next_elines sep_idx in 
           (Rule rule,remaining_lines)
      else let (assg,remaining_lines) = parse_next_assignment first_eline next_elines sep_idx in 
           (Assignment assg,remaining_lines) ;;


let empty_one = { 
      Makefile_t.assignments = []; 
      Makefile_t.rules = [];
      Makefile_t.inclusions = [];
   } ;;     

let rev_all mkf = { 
   Makefile_t.assignments = List.rev(mkf.Makefile_t.assignments); 
   Makefile_t.rules = List.rev(mkf.Makefile_t.rules);
   Makefile_t.inclusions = List.rev(mkf.Makefile_t.inclusions);
   
} ;;

let add_assignment assg mkf = { 
  mkf with    
   Makefile_t.assignments = assg :: (mkf.Makefile_t.assignments); 
} ;;

let add_rule rule mkf = { 
   mkf with 
   Makefile_t.rules = rule :: (mkf.Makefile_t.rules)
} ;;

let add_inclusion inclusion mkf = { 
   mkf with 
   Makefile_t.inclusions = inclusion :: (mkf.Makefile_t.inclusions)
} ;;

let add_instruction instr mkf = match instr with 
  Rule rule -> add_rule rule mkf
 |Assignment assg -> add_assignment assg mkf   
 |Inclusion incl -> add_inclusion incl mkf ;;


let rec helper_for_makefile_parsing (treated,remaining_elines) = 
   match remaining_elines with 
   [] -> rev_all treated 
  | first_eline :: other_elines ->
   let (instr,further_elines) =  parse_next_instruction first_eline other_elines in 
   helper_for_makefile_parsing (add_instruction instr treated,further_elines) ;;


let rec helper_for_gluing (treated,current_head,current_tail) =
    match current_tail with 
    [] -> List.rev (current_head :: treated)
    | next_head :: further_tail ->
       let (c_idx1,l_idx1,line1) = current_head 
       and (_c_idx2,_l_idx2,line2) = next_head in 
       if String.ends_with ~suffix:"\\" line1 
       then  let glued_line = (Cull_string.coending 1 line1)^" "^line2 in  
             helper_for_gluing (treated,(c_idx1,l_idx1,glued_line),further_tail)
       else  helper_for_gluing (current_head::treated,next_head,further_tail) ;; 

let glue_lines = function 
  [] -> [] 
  | head :: tail -> helper_for_gluing ([],head,tail) ;;   

let parse_makefile mkf_text = 
   let (Makefile_t.MT text) = mkf_text in 
   let lines1 = Lines_in_string.enhanced_indexed_lines text in 
   let lines2 = List.filter (fun (_c_idx,_l_idx,line)->
        not(
          (String.starts_with ~prefix:"#" line ) ||
          (Cull_string.trim_spaces(line)="" )
        )
      ) lines1 in 
   let lines3 = glue_lines lines2 in    
   helper_for_makefile_parsing (empty_one,lines3) ;;


let all_prerequisites mkf = 
    let temp1 = Image.image (fun ru -> ru.Makefile_t.prerequisites) mkf.Makefile_t.rules in 
    let temp2 = List.flatten temp1 in 
    Ordered.sort Total_ordering.lex_for_strings temp2 ;;

type list_of_prereq_aliases = LPA of ( string * string )   ;;



end ;;

let all_prerequisites = Private.all_prerequisites ;;

let expand_target_list = Private.expand_target_list ;;

let list_value = Private.list_value;;

let parse = Private.parse_makefile ;; 

let prerequisites_and_commands_for_target = Private.prerequisites_and_commands_for_target ;; 

let prerequisites_for_target = Private.prerequisites_for_target ;; 

let single_value = Private.single_value ;;

let write_rule_without_prerequisites = Private.write_rule_without_prerequisites ;; 

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

