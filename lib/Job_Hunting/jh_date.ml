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


end ;;  

let  number_of_days_between = Private.number_of_days_between ;; 