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

let read_list text = 
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
  let temp3 = Image.image (
    fun (official_date,followers)->
    (official_date,convert_even_sized_list_to_list_of_pairs followers)
  ) temp2 in 
  let unordered = List.flatten(Image.image (
  fun  (official_date,jas)-> Image.image (fun (company_data,_)->
    (get_effective_date_for_pair official_date company_data,
    extract_company_name (snd company_data))) jas
  ) temp3) in 
  Ordered.sort order_on_dated_items unordered;;

let recompute_usual_list_from_scratch () = 
  let ap = Absolute_path.of_string ((Sys.getenv "HOME")^"//Teuliou/html_files/Text_files/candidatures.txt") in 
  let text = Io.read_whole_file ap in 
  read_list text ;;

let ref_for_usual_list = ref None ;;

let usual_list () =
   match !ref_for_usual_list with 
   (Some old_answer) -> old_answer 
    |None ->
      let new_answer = recompute_usual_list_from_scratch () in 
      ref_for_usual_list:=Some(new_answer);
      new_answer ;;


let all_companies () = Ordered.sort Total_ordering.lex_for_strings (Image.image snd (usual_list())) ;;

let three_months_duration = 30+2*31 ;;

let recent_companies () =
  let all_job_applications = usual_list() in 
  let recent_job_applications = List.filter (fun (date,_company_name)->
    (Jh_date.number_of_days_between (Jh_date.today ()) date) <= three_months_duration
  ) all_job_applications in 
  let answer = Ordered.sort Total_ordering.lex_for_strings (Image.image snd recent_job_applications) in 
  (answer,(fun c->List.filter (fun s->(String.get s 0)=c) answer));;

  end ;;  