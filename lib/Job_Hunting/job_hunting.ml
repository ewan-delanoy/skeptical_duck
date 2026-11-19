(*

#use"lib/Job_Hunting/job_hunting.ml";;


*)

exception Uneven_list ;;
exception Bad_effective_date_format of int ;;

module Private = struct

let global_ap = Absolute_path.of_string ((Sys.getenv "HOME")^"/Teuliou/html_files/Text_files/candidatures.txt") ;;

let extremal_line_indices l =
   (fst(List.hd l),fst(List.hd(List.rev l))) ;;  
 
let convert_even_sized_list_to_list_of_pairs l =
  let m = List.length l in 
  if (m mod 2)<>0
  then raise(Uneven_list)
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

exception Pusher_towards_unique_dates_for_each_company_exn ;;

let pusher_towards_unique_dates_for_each_company (treated,to_be_treated)= match to_be_treated with 
  [] -> raise Pusher_towards_unique_dates_for_each_company_exn
  | pair :: other_pairs -> 
      let (date,company_name) = pair in 
      let (filtered_list,(opt_counterexample,_opt_example)) = 
        List_again.filter_and_remember (fun p->snd(p)<>company_name) other_pairs in 
      let has_multiple_applications = (opt_counterexample<>None) in 
      let translated_date = Jh_date.three_months_later date in 
      let remaining_before_release = Jh_date.number_of_days_between (Jh_date.today()) translated_date in 
      ((date,(company_name,has_multiple_applications,remaining_before_release))::treated,filtered_list) ;;

let rec helper_towards_unique_dates_for_each_company pair =
  let (treated,to_be_treated)= pair in 
  match to_be_treated with 
  [] -> treated 
  | _ ->
      helper_towards_unique_dates_for_each_company
      (pusher_towards_unique_dates_for_each_company pair) ;;

let shorten_list_to_have_unique_dates_for_each_company l = 
  helper_towards_unique_dates_for_each_company ([],List.rev l) ;;

let regulate_list_of_dated_items irregular_end_of_list = 
  let the_dates = Image.image fst irregular_end_of_list 
  and the_items = List.flatten(Image.image snd irregular_end_of_list) in 
  let items1 = Image.image (fun x->Some x) the_items in 
  let items2 = (
    if (List.length items1) mod 2 = 0 
    then items1
    else (items1)@[None] 
  ) in 
  let pairs_of_items = convert_even_sized_list_to_list_of_pairs items2 in 
  let m = List.length pairs_of_items in 
  if List.length(the_dates)<m
  then failwith("There are not enough dates to store all the job applications. Please add more dates")
  else
  let core =  List.combine (List.take m the_dates)  pairs_of_items in 
  let basic_form = Image.image (
   fun (date,(opt1,opt2))->
      let data1 = Option.get opt1 in 
      let final_data =(match opt2 with 
      None -> [data1]
      |Some data2 -> [data1;data2]) in 
      (date,final_data)
  ) core in 
  let writable_form =  core @ (Image.image (fun x->(x,(None,None))) (List.drop m the_dates)) in 
  (basic_form,writable_form) ;;

type company_item = CItem of (int * string) * (int * string) ;;


let citem_constructor (x,y) = CItem(x,y) ;;

let extract_naive_list_of_dated_items text = 
  let indexed_lines = Lines_in_text.indexed_lines text in 
  let nonempty_lines = List.filter (fun (_line_idx,line)->line<>"") indexed_lines in 
  let temp1 = Image.image (fun (line_idx,line)->
    ((line_idx,line),Naive_parser.try_parse_at_index Jh_date.parser line 1)) nonempty_lines in 
  let (chaotic_beginning_of_list,normalized_end_of_list) = List_again.leaders_and_followers 
   (fun (_,date_at_beginning_opt) -> date_at_beginning_opt<>None)
  temp1 in
  let temp2 = Image.image (fun (leader,followers)->
    (fst(Option.get(snd leader)),Image.image fst followers)  
  ) normalized_end_of_list in 
  (Image.image fst chaotic_beginning_of_list,Image.image (
    fun (official_date,followers)->
    let paired_followers = convert_even_sized_list_to_list_of_pairs followers in 
    (official_date,Image.image citem_constructor paired_followers)
  ) temp2 );;

let reconstruct_from_indexed_nonempty_lines indexed_nonempty_lines =
   let start_idx = fst(List.hd indexed_nonempty_lines)
   and end_idx =  fst(List.hd (List.rev indexed_nonempty_lines)) in 
   let reconstructed_lines = Int_range.scale (fun 
     line_idx -> match List.assoc_opt line_idx indexed_nonempty_lines with 
     Some line -> line 
     |None -> ""
   ) start_idx end_idx in 
   String.concat "\n" reconstructed_lines ;;  

let write_company_item (CItem((idx1,line1),(idx2,line2)))= line1^(String.make (idx2-idx1) '\n')^line2;;  

let write_list_of_dated_strokes l =
  let paragraphs = Image.image (fun 
    (date,company_items) -> 
      String.concat "\n\n" ((Jh_date.to_string date) :: 
      (Image.image write_company_item company_items))
  ) l in 
   String.concat "\n\n" paragraphs ;;

let write_obo_pair obo =
  if obo=(None,None) 
  then "" 
  else 
  let (opt1,opt2) = obo in 
  let part1 =  "\n\n"^write_company_item(Option.get opt1)
  and part2 = (
    match opt2 with 
    None -> ""
    |Some(item2) -> "\n\n"^(write_company_item item2)
  ) in 
  part1^part2 ;;


let write_dated_obo_pair (date,obo_pair) = (Jh_date.to_string date)^(write_obo_pair obo_pair) ;;
 
let write_list_of_dated_obo_pairs l = 
   String.concat "\n\n" (Image.image write_dated_obo_pair l) ;;


let write_main_triple chaotic_beginning_of_list regular_start_of_list writable_form_for_end_of_list =
   (reconstruct_from_indexed_nonempty_lines chaotic_beginning_of_list)^"\n\n"^
   (write_list_of_dated_strokes regular_start_of_list)^"\n\n"^
   (write_list_of_dated_obo_pairs writable_form_for_end_of_list) ;; 

let rectificative_message irregular_end_of_list = 
  "Excess at "^(Jh_date.to_string(fst(List.hd irregular_end_of_list)))^". The list will be rewritten."

let extract_list_of_dated_items text = 
  let (chaotic_beginning_of_list,naive_list) = extract_naive_list_of_dated_items text in 
  let regular_start_of_list = List.take_while (fun (_date,company_items)->List.length(company_items)<=2) naive_list
  and irregular_end_of_list = List.drop_while (fun (_date,company_items)->List.length(company_items)<=2) naive_list in 
  if irregular_end_of_list=[]
  then naive_list
  else
  let _ = Basic.display_message (rectificative_message irregular_end_of_list) in  
  let (basic_form_for_end_of_list,writable_form_for_end_of_list) = regulate_list_of_dated_items irregular_end_of_list  in 
  let fixed_text = write_main_triple chaotic_beginning_of_list regular_start_of_list writable_form_for_end_of_list  in 
  let _ = Io.overwrite_with global_ap fixed_text in 
  regular_start_of_list @ basic_form_for_end_of_list;;


let ref_for_current_list_of_dated_items = ref None ;;

let current_list_of_dated_items () = 
    match !ref_for_current_list_of_dated_items with 
   (Some old_answer) -> old_answer 
    |None ->
      let text = Io.read_whole_file global_ap in 
      let new_answer = extract_list_of_dated_items text in 
      ref_for_current_list_of_dated_items:=Some(new_answer);
      new_answer ;;

let compute_effective_dates_and_shorten dated_items = 
  let unordered = List.flatten(Image.image (
  fun  (official_date,jas)-> Image.image (fun (CItem(company_data,_))->
    (get_effective_date_for_pair official_date company_data,
    extract_company_name (snd company_data))) jas
  ) dated_items) in 
  let all_pairs = Ordered.sort order_on_dated_items unordered in 
  shorten_list_to_have_unique_dates_for_each_company (Ordered.to_list all_pairs) ;;

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
     Ordered.image (fun company_name->
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
    remaining_before_release <= 0
  ) all_job_applications ;;
   

let available_later_ordered_by_date () =
  let all_job_applications = initial_list_ordered_by_date() in 
  List.filter (fun (_date,(_company_name,_has_multiple_applications,remaining_before_release))->
    remaining_before_release > 0
  ) all_job_applications ;;

let available_later_ordered_by_company_name () =
     reorder_by_company_name( available_later_ordered_by_date()) ;;



let display_optional_line_pairs = function 
  None -> "No data found for today (perhaps it's the weekend ?)" 
 |Some line_pairs ->
  (String.concat "\n\n" (Image.image write_company_item line_pairs)) ;;

let display_dated_item date line_pairs_opt = 
  "Data for today, "^(Jh_date.to_string date)^" :\n\n"^
  (display_optional_line_pairs line_pairs_opt) ;;

let data_for_today () =
   let today = Jh_date.today () in 
   let line_pairs_opt = List.assoc_opt today (current_list_of_dated_items()) in 
   display_dated_item today line_pairs_opt ;;

end ;;  


let available_later_ordered_by_company_name = Private.available_later_ordered_by_company_name ;;
let available_later_ordered_by_date = Private.available_later_ordered_by_date ;; 
let available_now_ordered_by_company_name = Private.available_now_ordered_by_company_name ;;  

 
let data_for_today = Private.data_for_today  ;;