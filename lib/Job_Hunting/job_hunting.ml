(*

#use"lib/Job_Hunting/job_hunting.ml";;


*)

exception Uneven_list of (int * int) ;;
exception Bad_effective_date_format of int ;;

module Private = struct

let extremal_line_indices l =
   (fst(List.hd l),fst(List.hd(List.rev l))) ;;  
 
let convert_even_sized_list_to_list_of_pairs l =
  let m = List.length l in 
  if (m mod 2)<>0
  then raise(Uneven_list(extremal_line_indices l))
  else 
    let nt = (fun j->List.nth l (j-1)) in 
    Int_range.scale (fun k-> (nt(2*k-1),nt(2*k)))  1 (m/2);; 

let get_effective_date_for_pair default_date (line_idx,company_name)=
  match Substring.leftmost_index_of_in_from_opt "(g: " company_name 1 with 
  None -> default_date  
  |Some (t) ->
     let yearless = Jh_date.yearless_parser default_date.Jh_date_t.year in 
     match Naive_parser.try_parse_at_index yearless company_name (t+4) with 
     None -> raise(Bad_effective_date_format(line_idx))
     |Some(effective_date,_) -> effective_date

let extract_company_name company_data = 
    match Cull_string.before_and_after "," company_data with 
    None -> company_data
    |Some(before,_after) -> before ;; 

let order_on_dated_items = 
  Total_ordering.product Jh_date.order Total_ordering.lex_for_strings ;;    

let rec helper_towards_unique_dates_for_each_company (treated,to_be_treated)= match to_be_treated with 
  [] -> treated 
  | pair :: other_pairs ->
      let (date,company_name) = pair in 
      let (filtered_list,(opt_counterexample,_opt_example)) = List_again.filter_and_remember (fun p->snd(p)<>company_name) other_pairs in 
      let has_multiple_applications = (opt_counterexample<>None) in 
      let translated_date = Jh_date.three_months_later date in 
      let remaining_before_release = Jh_date.number_of_days_between (Jh_date.today()) translated_date in 
      helper_towards_unique_dates_for_each_company
      ((date,(company_name,has_multiple_applications,remaining_before_release))::treated,filtered_list) ;;

let shorten_list_to_have_unique_dates_for_each_company l = 
  helper_towards_unique_dates_for_each_company ([],List.rev l) ;;

let extract_list_of_dated_items text = 
  let indexed_lines = Lines_in_text.indexed_lines text in 
   let nonempty_lines = List.filter (fun (_line_idx,line)->line<>"") indexed_lines in 
  let temp1 = Image.image (fun (line_idx,line)->
    ((line_idx,line),Naive_parser.try_parse_at_index Jh_date.parser line 1)) nonempty_lines in 
  let (_chaotic_beginning_of_list,normalized_end_of_list) = List_again.leaders_and_followers 
   (fun (_,date_at_beginning_opt) -> date_at_beginning_opt<>None)
  temp1 in
  let temp2 = Image.image (fun (leader,followers)->
    (fst(Option.get(snd leader)),Image.image fst followers)  
  ) normalized_end_of_list in 
  Image.image (
    fun (official_date,followers)->
    (official_date,convert_even_sized_list_to_list_of_pairs followers)
  ) temp2 ;;

let ref_for_current_list_of_dated_items = ref None ;;

let current_list_of_dated_items () = 
    match !ref_for_current_list_of_dated_items with 
   (Some old_answer) -> old_answer 
    |None ->
       let ap = Absolute_path.of_string ((Sys.getenv "HOME")^"//Teuliou/html_files/Text_files/candidatures.txt") in 
      let text = Io.read_whole_file ap in 
      let new_answer = extract_list_of_dated_items text in 
      ref_for_current_list_of_dated_items:=Some(new_answer);
      new_answer ;;

let compute_effective_dates_and_shorten dated_items = 
  let unordered = List.flatten(Image.image (
  fun  (official_date,jas)-> Image.image (fun (company_data,_)->
    (get_effective_date_for_pair official_date company_data,
    extract_company_name (snd company_data))) jas
  ) dated_items) in 
  let all_pairs = Ordered.sort order_on_dated_items unordered in 
  shorten_list_to_have_unique_dates_for_each_company all_pairs ;;

let recompute_initial_list_from_scratch () = 
  let dated_items = current_list_of_dated_items () in 
  compute_effective_dates_and_shorten dated_items ;;

let ref_for_initial_list = ref None ;;

let initial_list_ordered_by_date () =
   match !ref_for_initial_list with 
   (Some old_answer) -> old_answer 
    |None ->
      let new_answer = recompute_initial_list_from_scratch () in 
      ref_for_initial_list:=Some(new_answer);
      new_answer ;;


let reorder_by_company_name l =
     let companies = Ordered.sort Total_ordering.lex_for_strings (Image.image 
   (fun (_date,(company_name,_has_multiple_applications,_remaining_before_release))
   ->company_name) l) in 
     Image.image (fun company_name->
     Option.get(List.find_map (
       fun (date,(company_name2,has_multiple_applications,remaining_before_release)) ->
        if company_name2 = company_name 
        then Some(company_name,(date,has_multiple_applications,remaining_before_release))  
        else None  
     ) l)
  ) companies ;;


let initial_list_ordered_by_company_name () =
     reorder_by_company_name( initial_list_ordered_by_date()) ;;


let available_now_ordered_by_company_name () =
   let all_job_applications = initial_list_ordered_by_company_name() in 
  List.filter (fun (_company_name,(_date,_has_multiple_applications,remaining_before_release))->
    remaining_before_release < 0
  ) all_job_applications ;;
   

let available_later_ordered_by_date () =
  let all_job_applications = initial_list_ordered_by_date() in 
  List.filter (fun (_date,(_company_name,_has_multiple_applications,remaining_before_release))->
    remaining_before_release >= 0
  ) all_job_applications ;;

(*
let display_dated_item (date,line_pairs) =
*)


  end ;;  

let all_ordered_by_company_name = Private.initial_list_ordered_by_company_name ;;

let available_now_ordered_by_company_name = Private.available_now_ordered_by_company_name ;;  

let available_later_ordered_by_date = Private.available_later_ordered_by_date ;;  

