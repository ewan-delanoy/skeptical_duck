(*

#use"lib/Job_Hunting/jh_date.ml";;


*)

module Private = struct 

let is_a_leap_year  year = 
  if (year mod 4)<>0 then false else 
  if (year mod 400)=0 then true else   
  (year mod 100 <> 0) ;;

let list_for_number_of_days_in_month =
   [31;28;31;30;31;30;
    31;31;30;31;30;31] ;;

let number_of_days_in_month year m = 
  let default = List.nth list_for_number_of_days_in_month (m-1) in 
  if m<>2 then default else 
  default+(if is_a_leap_year  year then 1 else 0) ;;  

let number_of_days_in_month_initial_range year month_max =
    let accu = ref 0 in 
    for month = 1 to month_max do accu:=(!accu) +  (number_of_days_in_month year month) done;
    !accu ;;  

let number_of_days_in_month_final_range year month_min =
    let accu = ref 0 in 
    for month = month_min to 12 do accu:=(!accu) +  (number_of_days_in_month year month) done;
    !accu ;;  

let number_of_days_in_year year = if is_a_leap_year  year then 365 else 366 ;;
   
let number_of_days_in_year_range year_min year_max =
    let accu = ref 0 in 
    for year = year_min to year_max do accu:=(!accu) +  (number_of_days_in_year year) done;
    !accu ;;

let number_of_days_between date1 date2 = 
   let first_year = date1.Jh_date_t.year 
   and first_month = date1.Jh_date_t.month 
   and first_day = date1.Jh_date_t.day 
   and last_year = date2.Jh_date_t.year 
   and last_month = date2.Jh_date_t.month 
   and last_day = date2.Jh_date_t.day in
   let end_of_first_month = (number_of_days_in_month first_year first_month) - first_day
   and end_of_first_year = number_of_days_in_month_final_range first_year (first_month+1)
   and year_diff = number_of_days_in_year_range (first_year+1) (last_year-1)
   and beginning_of_last_year = number_of_days_in_month_initial_range last_year (last_month-1)
   and beginning_of_last_month = last_day  in 
   end_of_first_month + end_of_first_year + year_diff + beginning_of_last_year + beginning_of_last_month ;;

let digit_followed_by_optional_digit = 
  let d= Naive_parser_example.digit in
  Naive_parser.concat_mandatory_with_optional d d ;;

let one_or_two_digits = 
  Naive_parser.map (
    fun (c1,opt) -> 
      let i1 = int_of_char(c1)-48 in 
      match opt with 
     None -> i1
     |Some(c2)->10*i1+(int_of_char(c2)-48)
  ) digit_followed_by_optional_digit ;;

(*     
Naive_parser.try_parse_at_index one_or_two_digits "78abc" 1 ;;
Naive_parser.try_parse_at_index one_or_two_digits "9abc" 1 ;;
*)

let slash= Naive_parser_example.fixed_string "/" ;;

let one_or_two_digits_followed_by_slash = 
  Naive_parser.map fst (Naive_parser.concat2 one_or_two_digits slash) ;;

(*     
Naive_parser.try_parse_at_index one_or_two_digits_followed_by_slash "78/abc" 1 ;;
Naive_parser.try_parse_at_index one_or_two_digits_followed_by_slash "9/abc" 1 ;;
*)   

let doubled_digit =  Naive_parser.concat2 Naive_parser_example.digit Naive_parser_example.digit ;;

let number_with_two_digits = Naive_parser.map (
   fun (c1,c2) ->  10*(int_of_char(c1)-48)+(int_of_char(c2)-48)
) doubled_digit ;;


(*     
Naive_parser.try_parse_at_index number_with_two_digits "543/abc" 1 ;;
Naive_parser.try_parse_at_index number_with_two_digits "78/abc" 1 ;;
Naive_parser.try_parse_at_index number_with_two_digits "9/abc" 1 ;;
*)   

let number_with_two_digits_possibly_doubled = 
   Naive_parser.concat_mandatory_with_optional
     number_with_two_digits number_with_two_digits ;;

(*     
Naive_parser.try_parse_at_index number_with_two_digits_possibly_doubled "5432/abc" 1 ;;
Naive_parser.try_parse_at_index number_with_two_digits_possibly_doubled "781/abc" 1 ;;
*)

let date_beginning = 
   Naive_parser.concat2 one_or_two_digits_followed_by_slash one_or_two_digits_followed_by_slash ;;
let date_ending = Naive_parser.map 
 (
   fun (i1,opt) ->
     match opt with 
     None -> 2000+i1
     |Some(i2)->100*i1+i2
 ) number_with_two_digits_possibly_doubled ;;
 


exception Unfinished_date of int * (int * int) * int ;;

let parser = Naive_parser_t.NP(fun
  text idx  ->
  match Naive_parser.try_parse_at_index date_beginning text idx with 
  None -> None 
  |Some((d,m),idx1)->
    match Naive_parser.try_parse_at_index date_ending text idx1 with 
  None -> raise(Unfinished_date(idx,(d,m),idx1))
  |Some(y,final_idx)->
    Some({Jh_date_t.day=d; month=m; year=y},final_idx)
   ) ;;

(*     
Naive_parser.try_parse_at_index parser "54/3/3006abc" 1 ;;
Naive_parser.try_parse_at_index parser "2/3/14abc" 1 ;;
Naive_parser.try_parse_at_index parser "781/abc" 1 ;;
*)

let today () =
   let gm = Unix.localtime (Unix.time()) in 
   {Jh_date_t.day=gm.Unix.tm_mday; month=gm.Unix.tm_mon+1; year=gm.Unix.tm_year+1900} ;;

end ;;  

let  number_of_days_between = Private.number_of_days_between ;; 
let parser = Private.parser ;;

let today = Private.today ;;