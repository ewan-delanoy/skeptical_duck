(*

#use"lib/makefile.ml";;

*)


exception List_value_exn of string * ( Makefile_t.variable_assignment list) ;; 

exception Parse_next_instruction_exn of int ;;

exception Prerequisites_and_commands_for_target_exn of string * ( Makefile_t.rule list) ;; 

exception Missing_variable_close_tag of string * int ;;

module Private = struct 

exception Check_all_are_empty_but_last_exn  ;;

type text_or_dollar_var 
   = Txt of string 
    |DVar of string * ((string * string) list) ;; 

type t = {
   assignments : Makefile_t.variable_assignment list;
   rules : Makefile_t.rule list;
   inclusions : Makefile_t.inclusion list;
   all_targets: (string list) option ;
   all_prerequisites: (string list) option ;
   all_variables: ((string * (text_or_dollar_var list)) list) option ;
};; 


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
   ) mkf.rules in 
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
   ) mkf.assignments in 
   if List.length(temp1)>1
   then  raise(List_value_exn(variable_name,temp1))
   else  
   if temp1=[]
   then [""]      
   else
   let assg = List.hd temp1 in 
   assg.Makefile_t.content ;;
let single_value mkf ~variable_name = 
   String.concat " " (list_value mkf ~variable_name);;


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
      assignments = []; 
      rules = [];
      inclusions = [];
      all_targets = None ;
      all_prerequisites = None  ;
      all_variables = None  ;
   } ;;     

let rev_all mkf = { 
   mkf with 
   assignments = List.rev(mkf.assignments); 
   rules = List.rev(mkf.rules);
   inclusions = List.rev(mkf.inclusions);
   
} ;;

let add_assignment assg mkf = { 
  mkf with    
   assignments = assg :: (mkf.assignments); 
} ;;

let add_rule rule mkf = { 
   mkf with 
   rules = rule :: (mkf.rules)
} ;;

let add_inclusion inclusion mkf = { 
   mkf with 
   inclusions = inclusion :: (mkf.inclusions)
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


let all_elements mkf = 
    let temp1 = Image.image (fun ru -> ru.Makefile_t.targets) mkf.rules 
    and temp2 = Image.image (fun ru -> ru.Makefile_t.prerequisites) mkf.rules in 
    let temp3 = List.flatten (temp1@temp2) in 
    Ordered.sort Total_ordering.lex_for_strings temp3 ;;


let sl_order = Total_ordering.lex_for_strings ;;
let sl_insert = Ordered.insert sl_order ;; 
let sl_sort = Ordered.sort sl_order ;; 

  


let parse_dollar_var varcontent = 
   let opt1 = String.index_from_opt varcontent 0 ':' in 
   if opt1 = None 
   then DVar(varcontent,[]) 
   else 
   let i1 = (Option.get opt1) + 1 in 
   let opt2 = String.index_from_opt varcontent i1 '=' in  
   if opt2 = None 
   then DVar(varcontent,[]) 
   else 
   let i2 = (Option.get opt2) + 1 in 
   let itv = Cull_string.interval varcontent in 
   DVar(itv 1 (i1-1),[itv (i1+1) (i2-1),itv (i2+1) (String.length varcontent)]) ;;   
    
(*

parse_dollar_var "The:brown=cat" ;; 
parse_dollar_var "Again" ;; 

*)

let add_if_nonempty line i j treated = 
   if i<=j 
   then (Txt(Cull_string.interval line i j)) :: treated 
   else treated ;;   

let rec helper_for_todv_decomposition (line,line_length) (treated,idx) = 
    if idx>line_length 
    then List.rev treated 
    else 
    let itv = Cull_string.interval line in   
    match Substring.leftmost_index_of_pattern_among_in_from_opt ["${";"$("] line idx with 
      None ->  List.rev ( (Txt(itv idx line_length)) :: treated )
    |Some(_,idx2) -> 
      let treated2 = add_if_nonempty line idx (idx2-1) treated in 
      let opening_par = Strung.get line (idx2+1) in 
      let closing_par = List.assoc opening_par ['(',")";'{',"}"] in 
      let opt3 = Substring.leftmost_index_of_in_from_opt closing_par line idx2 in 
      if opt3 = None 
      then raise(Missing_variable_close_tag(line,idx))
      else 
      let idx3 = Option.get opt3 in 
      let vrange = itv (idx2+2) (idx3-1) in 
      helper_for_todv_decomposition (line,line_length) ((parse_dollar_var vrange) ::treated2,idx3+1) ;; 

        
let todv_decompose line = helper_for_todv_decomposition (line,String.length line) ([],1) ;; 

(*
   
todv_decompose "When $(the) ${saints}\t\t${go} marching $(in) ..." ;; 

*)

let rec helper_for_regluing (treated,todv,to_be_treated) = 
   match to_be_treated with 
   [] -> List.rev(todv::treated)
   |todv2 :: others ->
      (
         match todv with 
         DVar(_,_) ->  helper_for_regluing (todv::treated,todv2,others)
         |Txt(txt1) ->
            (
              match todv2 with 
             DVar(_,_) ->  helper_for_regluing (todv::treated,todv2,others)
            |Txt(txt2) ->  helper_for_regluing (treated,Txt(txt1^txt2),others)
      )
      ) ;;

let reglue = function 
   [] -> [] 
  |todv :: others ->  helper_for_regluing ([],todv,others) ;;    
  
(*

reglue [DVar("a",[]);Txt("1");Txt("2");Txt("3");DVar("b",[]);Txt("4");Txt("5");DVar("c",[])] ;;

*)  

let apply_low_level_substitution_to_todv (ab,ba) = function 
  Txt(txt) -> Txt(Replace_inside.replace_inside_string (ab,ba) txt)
  | DVar(vname,replacements) -> DVar(vname,replacements@[ab,ba]) ;;

let apply_low_level_substitution_to_todv_list  l pair = 
   Image.image (apply_low_level_substitution_to_todv pair) l ;;
let apply_low_level_substitutions_to_todv_list  l pairs = 
  List.fold_left apply_low_level_substitution_to_todv_list  l pairs ;;

let expand_in_todv  (varname,new_varcontent) todv = 
  match todv with  
  Txt(_) -> (0,[todv])
  | DVar(vname,replacements) ->
     if vname<>varname 
     then (0,[todv])
     else (1,apply_low_level_substitutions_to_todv_list new_varcontent replacements) ;;

let expand_in_todv_list (old_count,l) pair = 
   let temp1 = Image.image (expand_in_todv pair) l in 
   let offset = Basic.fold_sum (Image.image fst temp1)
   and unglued = List.flatten(Image.image snd temp1) in 
   (old_count + offset, reglue unglued);;

let expand_several_in_todv_list (old_count,l) pairs = 
   List.fold_left expand_in_todv_list (old_count,l) pairs ;;

let compute_all_targets mkf = 
   let tgts1 = List.flatten (Image.image (fun ru -> ru.Makefile_t.targets ) mkf.rules) in 
   Ordered.sort Total_ordering.lex_for_strings tgts1 ;;

let all_targets mkf_ref = 
   let old_mkf = (!mkf_ref) in 
   match old_mkf.all_targets with 
 (Some already_computed) -> already_computed 
 | None -> 
    let tgts = compute_all_targets old_mkf in 
    let _ = (mkf_ref:= {old_mkf with all_targets = Some tgts} ) in 
    tgts ;; 


let compute_all_prerequisites mkf = 
   let tgts1 = List.flatten (Image.image (fun ru -> ru.Makefile_t.prerequisites ) mkf.rules) in 
   Ordered.sort Total_ordering.lex_for_strings tgts1 ;;
    
let all_prerequisites mkf_ref = 
   let old_mkf = (!mkf_ref) in 
      match old_mkf.all_prerequisites with 
   (Some already_computed) -> already_computed 
   | None -> 
       let prereqs = compute_all_prerequisites old_mkf in 
       let _ = (mkf_ref:= {old_mkf with all_prerequisites = Some prereqs} ) in 
       prereqs ;; 
    
let compute_all_variables mkf = 
         let vars1 = Image.image (fun va -> va.Makefile_t.variable_name ) mkf.assignments in 
         let vars = Ordered.sort Total_ordering.lex_for_strings vars1 in 
         Image.image (fun v->(v,todv_decompose(single_value mkf ~variable_name:v))) vars ;;

let all_variables mkf_ref = 
   let old_mkf = (!mkf_ref) in 
            match old_mkf.all_variables with 
    (Some already_computed) -> already_computed 
   | None -> 
     let vars = compute_all_variables old_mkf in 
     let _ = (mkf_ref:= {old_mkf with all_variables = Some vars} ) in 
     vars ;; 

let constant_for_todv_list_opt l = 
   if List.length(l)<>1
   then None
   else 
   match List.hd(l) with 
   Txt txt ->(Some txt)
   |DVar(_,_) -> None ;;     

type full_expander = FE of ( string * string ) list ;;
type partial_expander = PE of (string * (text_or_dollar_var list)) list ;;
type mixed_expander = Mx of full_expander * partial_expander ;; 

exception Unknown_variable_exn of string ;;

let register_new_full_expansion (Mx(FE l1,PE l2)) (vname,vcontent) = 
    let new_l1 = (vname,vcontent) :: l1 
    and new_l2 = List.filter (fun (vname2,_) -> vname2 <> vname) l2 in 
    Mx(FE new_l1,PE new_l2) ;;

let eval (Mx(FE l1,PE l2)) vname=
  match List.assoc_opt vname l1 with 
  (Some text1) -> [Txt text1]
  | None ->
   (
      match List.assoc_opt vname l2 with 
      (Some expr2) -> expr2
      |  None ->
         (
            match Sys.getenv_opt vname with 
             Some env_value -> [Txt env_value]
             |None -> raise (Unknown_variable_exn(vname)) 
         )
   ) ;;

    
let decision_for_todv mixed = function 
  (Txt txt) -> (Some txt,None)
  |DVar(vname,_reps) ->
     match constant_for_todv_list_opt (eval mixed vname) with 
      (Some cst) ->  (Some cst,None)
      |None -> (None,Some vname) ;; 

let decision_for_todv_list mixed l = 
   let temp1 = Image.image (fun todv ->(todv,decision_for_todv mixed todv)) l in       
   match List.find_opt (fun (_,(_,bad_opt)) -> bad_opt<>None) temp1 with 
   None ->
         let texts = Image.image (fun (_,(good_opt,_)) ->Option.get good_opt) temp1 in 
         (Some(String.concat "" texts), None)
   |(Some (_,(_,bad_opt))) ->(None, bad_opt)   ;;


(* let rec helper_for_full_expansion (mixed,chain_head,chain_tail) =
   let (vname1,todv_list1) = chain_head in 
   let (good_opt,bad_opt) = decision_for_todv_list mixed todv_list1 in 
   if good_opt<>None 
   then let new_mixed = register_new_full_expansion mixed (vname1, Option.get good_opt) in 
        (
         match chain_tail with 
          next_pair :: other_pairs -> helper_for_full_expansion (new_mixed,next_pair,other_pairs)
          |[] -> 
             let (Mx(FE l1,PE l2)) = new_mixed in
             (
               match l2 with 
               [] -> FE l1
               |next_pair2 :: other_pairs2 -> helper_for_full_expansion (new_mixed,next_pair2,other_pairs2)
             ) 
        )   
    else
    let vname2 = Option.get bad_opt in 
    let full_chain = chain_head :: chain_tail in 
    let names = Image.image fst full_chain in 
    let i_opt = List_again.find_index_of_in *)


end ;;


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

let assgs2 = see2.assignments ;; 

let assg = List.nth assgs2 50;;

let rules2 = see2.rules ;; 

let rule = List.nth rules2 340;;

*)

