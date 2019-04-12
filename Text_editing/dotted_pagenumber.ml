(*


#use"Text_editing/dotted_pagenumber.ml";;

*)


let check_for_dotted_pagenumber_at_index s start_idx =
  let n=String.length s in 
  if (Strung.get s start_idx)<>'-'
  then None
  else 
  let opt1=Option.seek (
     fun k->let c=Strung.get s k in 
     not(List.mem c [' ';'\n';'\t';'r'])
  ) (Ennig.ennig (start_idx+1) n) in 
  if opt1=None then None else 
  let idx1=Option.unpack opt1 in
  let opt2=Option.seek (
     fun k->let c=Strung.get s k in 
     not(List.mem c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'])
  ) (Ennig.ennig (idx1) n) in 
  if (opt2=None)||(opt2=Some(idx1)) then None else
  let idx2=Option.unpack opt2 in 
  let opt3=Option.seek (
     fun k->let c=Strung.get s k in 
     not(List.mem c [' ';'\n';'\t';'r'])
  ) (Ennig.ennig (idx2) n) in
  if (opt3=None) then None else
  let idx3=Option.unpack opt3 in
  if (Strung.get s idx3)<>'-'
  then None
  else   
  let pagenumber_description=Cull_string.interval s idx1 (idx2-1) in 
  let pagenumber=int_of_string pagenumber_description in 
  Some(pagenumber,(start_idx,idx3));;

(*

check_for_dotted_pagenumber_at_index "123- 67 -012" 4;;
check_for_dotted_pagenumber_at_index "123-    -012" 4;;

*)


let extract_dotted_pagenumbers main_text =
   let n=String.length main_text in 
   let rec tempf=(
     fun (treated,idx_to_be_treated)->
       if idx_to_be_treated > n 
       then List.rev treated
       else 
       let opt1=Option.find_and_stop 
         (check_for_dotted_pagenumber_at_index main_text) 
           (Ennig.ennig idx_to_be_treated n) in 
       if opt1=None 
       then List.rev ((None,Cull_string.interval main_text idx_to_be_treated n)::treated) 
       else 
       let (pg_nbr,(i_start,i_end))=Option.unpack opt1 in 
       let temp1=(
         if i_start=idx_to_be_treated 
         then  treated
         else  (None,Cull_string.interval main_text idx_to_be_treated (i_start-1))::treated
       )  in 
       tempf((opt1,Cull_string.interval main_text i_start i_end)::temp1,i_end+1)
   ) in 
   tempf([],1);;

(*

extract_dotted_pagenumbers "abc- 37 -de\n-38-fgh-\t93-ij\n";;

*)

let standardize_dotted_pagenumbers_in_string main_text =
   let temp1=extract_dotted_pagenumbers main_text in 
   let temp2=Image.image (
      fun (opt,snippet)->match opt with 
      None->snippet
      |Some(pg_nbr,_)->
         "\n\np"^(string_of_int pg_nbr)^"\n\n"
   ) temp1 in 
   String.concat "" temp2;;

let standardize_dotted_pagenumbers_in_in_file fn=
   let old_text=Io.read_whole_file fn in
   let new_text=standardize_dotted_pagenumbers_in_string old_text in
   Io.overwrite_with fn new_text;;






