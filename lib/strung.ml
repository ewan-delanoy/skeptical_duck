(*

#use"lib/strung.ml";;

*)


let get s i=String.get s (i-1);;
 
let set s i c=Bytes.set s (i-1) c;;

let enclose s=
  let encloser="\"" in
  encloser^(String.escaped s)^encloser;;


let implode l=
   let n=List.length(l) in
   let by=Bytes.make n ' ' in
   let _=(for i=0 to (n-1) do Bytes.set by i (List.nth l i) done;) in
   Bytes.to_string by;;
  
    
let explode s=
    let n=String.length s in
    Int_range.scale (String.get s) 0 (n-1);;
    
 
let char_finder_from_inclusive_opt f s w0=
   let n=(String.length s) in
   let rec tempf=(fun j->
     if j>n then None else
     if f(String.get s  (j-1)) then Some j else
     tempf(j+1)
   ) in
   tempf(w0);;

let backwards_char_finder f s =
    let rec tempf=(fun j->
      if j<0 then 0 else
      if f(String.get s  j) then j+1 else
      tempf(j-1)
    ) in
    tempf((String.length s)-1);;   
 
let show_indices s=
  let n=String.length s in
  Int_range.scale (fun i->(i,String.get s (i-1)) ) 1 n;;   
   
let number_of_lines_before s i=
  if i<1 then 0 else
  let m=min i (String.length s) in
  List.length(List.filter(fun j->(String.get s (j-1))='\n')(Int_range.range 1 m));;


let number_of_linebreaks s =
    let n = String.length s 
    and counter = ref 0 in
    for k = 0 to (n-1) 
    do 
       if (String.get s k='\n') then counter:=(!counter)+1
    done; (!counter) ;; 

(* number_of_linebreaks "3\n\n\n4\n\n\n\nhum";; *)    

exception Negative_offset_for_string;; 

let insert_repetitive_offset_on_the_left c l_max str=
   let d=l_max-(String.length str) in
   if d<0
   then raise(Negative_offset_for_string)
   else
   (String.make d c)^str;;


     
let reverse s=
   implode(List.rev(explode s));; 

let find_one_of_several_in_at_idx candidates s idx =
   let tester=(
      fun candidate ->
       let j=idx-1 and l_cand=String.length(candidate) in 
       if (String.length(s)<j+l_cand)||(j<0)
      then None
      else if (String.sub s j l_cand)=candidate 
           then Some(idx,candidate)
           else None
   ) in 
   List.find_map tester candidates;;
  
(*

find_one_of_several_in_at_idx ["ba";"ab"] "123ab67" 4;;

*)

let find_one_of_several_in_from_idx candidates s idx =
  let n=String.length s in 
  List.find_map (
    find_one_of_several_in_at_idx candidates s
  ) (Int_range.range idx n);;

(*

find_one_of_several_in_from_idx ["ba";"ab"] "123ab67" 1;;

*)

let rec 
helper_for_dec_according_to_occ 
(whole,candidates,last_idx) (treated,idx) =
if idx > last_idx 
then List.rev treated 
else
match find_one_of_several_in_from_idx candidates whole idx with 
  None -> List.rev (((idx,last_idx),false,Cull_string.interval whole idx last_idx) 
                       :: treated)
 |Some(idx2,found_word) ->
    let treated2 = (
       if idx2=idx 
       then treated
       else ((idx,idx2-1),false,Cull_string.interval whole idx (idx2-1)) ::treated
    ) in 
    let new_idx = idx +(String.length found_word)  in 
    let pair2 = ((idx,new_idx-1),true,found_word)  in 
    helper_for_dec_according_to_occ    
     (whole,candidates,last_idx) (pair2::treated2,new_idx);;
  

let decomposition_according_to_occurrences_from_several whole candidates = 
  helper_for_dec_according_to_occ 
  (whole,candidates,String.length whole) ([],1);;


(*

decomposition_according_to_occurrences_from_several 
  "12abc3def4ababab5c" ["ab";"c";"def"] ;;


*)


let remove_newlines s=
   let temp1=List.filter (fun c->c<>'\n') (explode s) in 
   implode temp1;;

exception Not_found_during_succession;;

let find_successively_in_from patterns_in_order s start_idx=
  let rec tempf=(fun 
     (treated,to_be_treated,idx,line_idx)->
       match to_be_treated with 
       []->List.rev treated
       |patt::other_patts->
         match  find_one_of_several_in_from_idx patt s idx with 
         None->raise(Not_found_during_succession)
         |Some(idx2,candidate)->
          let temp1=List.filter(fun k->(get s k)='\n')(Int_range.range idx (idx2-1)) in 
          let line_idx_for_idx2=line_idx+List.length(temp1) in 
          let msg="Found "^(remove_newlines candidate)^" at line number "^(string_of_int line_idx_for_idx2)^"\n" in 
          let _=(print_string msg;flush stdout) in 
          let idx3=idx2+(String.length candidate) in  
          let temp2=List.filter(fun k->(get s k)='\n')(Int_range.range idx2 (idx3-1)) in 
          let line_idx_for_idx3=line_idx_for_idx2+List.length(temp2) in 
          tempf((idx2,idx3-1)::treated,other_patts,idx3,line_idx_for_idx3)    
  ) in 
  let temp3=List.filter(fun k->(get s k)='\n')(Int_range.range 1 (start_idx-1)) in 
  let start_line_idx = 1+(List.length(temp3)) in 
  tempf([],patterns_in_order,start_idx,start_line_idx);;

(*

find_successively_in [["ba";"ab"];["cde";"edc"]] "12\n\n\n\n\n8ab123\n\n67cde12";;

*)



let replace_ranges_in l s=
    if l=[] then s else
    let n=String.length s in
    let ranges=Image.image fst l in
    let partition= Partition_list.from_set_of_ranges ranges n in 
    let temp1=Image.image (
      fun (i,j,will_be_replaced)->
        if will_be_replaced 
        then List.assoc (i,j) l
        else String.sub s (i-1) (j-i+1)
    ) partition in
    String.concat "" temp1;;

(*

replace_ranges_in [((3,5),"A");((8,12),"B")] "12345678901234567890";;

*)




let insert_prefixes_at_indices l s=
    if l=[] then s else
    let n=String.length s in
    let temp1=Image.image (fun (pref,idx)->(idx,pref)) l in
    let temp2=Image.image fst temp1 in
    let temp3=Ordered.sort Total_ordering.standard ((n+1)::temp2) in
    let temp4=List_again.universal_delta_list temp3 in
    let temp5=Image.image(fun (i,j)->
       (List.assoc i temp1)^(String.sub s (i-1) (j-i)) ) temp4 in
    let i1=List.hd temp3 in
    let temp6=(
       if i1=1 then temp5 else (String.sub s 0 (i1-1))::temp5
    )  in 
    String.concat "" temp6;;

(*

insert_prefixes_at_indices ["hap",4;"na",12] "123py678901tion6";;

*)

exception Largest_common_prefix_exn;;

let largest_common_prefix l=
   if l=[] then raise(Largest_common_prefix_exn) else
   let lengths=Image.image String.length l in
   let m=Min.list lengths in
   let tester=(fun k->
     let temp1=Image.image (fun s->String.get s k) l in
     let v=List.hd temp1 in
     List.for_all (fun x->x=v) temp1
   ) in
   let rec tempf=(fun j->
     if j=m then j else 
     if tester(j) then tempf(j+1) else j
   ) in
   let j0=tempf 0 in
   String.sub (List.hd l) 0 j0;;

(*

largest_common_prefix ["abby";"abnormal"];;
largest_common_prefix ["";"call"];;
largest_common_prefix ["sad";"again"];;


*)

let leftmost_difference s1 s2=
   let n1=String.length s1 
   and n2=String.length s2 in
   let n=min(n1)(n2) in 
   match List.find_opt(fun j->
      (get s1 j)<>(get s2 j)
   )(Int_range.range 1 n) with 
   None->None 
   |Some(j0)->
      let common_part=String.sub s1 0 (j0-1) 
      and s1_part=String.sub s1 j0 (n1-j0)
      and s2_part=String.sub s2 j0 (n2-j0) in 
      Some(common_part,get s1 j0,get s2 j0,s1_part,s2_part);;

(*
leftmost_difference "abc1def" "abc257";;
*)




exception Unfinished_expression of int*string;;
exception Unexpected_case_in_triune_analysis;;


module Private = struct

let pusher_inside_nested_parentheses_parsing
    (s,joiners,seeker) state =
     let (opt_result,nbr_of_openers_so_far,items_so_far,current_item_start,world_start,idx)=state in 
     if opt_result<>None then state else 
     let opt = seeker idx in 
     if opt = None 
     then raise(Unfinished_expression(idx,s))
     else 
     let (case,new_idx)=Option.get opt in
     let joiner=List.nth joiners (case-1) in 
     let idx2=new_idx+String.length(joiner) in 
     if case=1
     then (None,nbr_of_openers_so_far+1,items_so_far,current_item_start,world_start,idx2)
     else    
     if case=2
     then (
            if nbr_of_openers_so_far=1
            then let new_item = Cull_string.interval s current_item_start (new_idx-1) in 
                 (None,nbr_of_openers_so_far,new_item::items_so_far,idx2,world_start,idx2)
            else (None,nbr_of_openers_so_far,items_so_far,current_item_start,world_start,idx2)
          )
     else 
     if case=3
     then (
            if nbr_of_openers_so_far=1
            then let whole_interval=(world_start,idx2-1) in 
                 let last_item = Cull_string.interval s current_item_start (new_idx-1) in 
                 let items=List.rev(last_item::items_so_far) in
                 let answer=Some(items,whole_interval) in  
                 (answer,0,[],0,0,0)
            else (None,nbr_of_openers_so_far-1,items_so_far,current_item_start,world_start,idx2)
          )
     else raise(Unexpected_case_in_triune_analysis);;
     
let iterator_inside_nested_parentheses_parsing 
    (s,joiners,seeker) =
     let rec tempf=(fun state ->
     let (opt_result,_nbr_of_openers_so_far,_items_so_far,_current_item_start,_world_start,_idx)=state in 
     match opt_result with 
     Some(result)->result 
     |None -> tempf(pusher_inside_nested_parentheses_parsing (s,joiners,seeker) state )) in 
   tempf;;

end ;;


exception Missing_opener of string*string;;
exception Started_by_nonopener of int*string;;


let parse_nested_parentheses 
  (openr,separatr,closr) s=
    let joiners = [openr;separatr;closr]  in  
    let seeker = Substring.leftmost_index_of_pattern_among_in_from_opt 
       [openr;separatr;closr] s in 
    
    let opt1=seeker 1 in 
    if opt1=None 
    then raise(Missing_opener(openr,s))
    else  
    let (case1,idx1)=Option.get opt1 in
    if case1<>1
    then raise(Started_by_nonopener(case1,s))
    else 
    let idx2=idx1+(String.length openr) in 
    let initial_vals=(None,1,[],idx2,idx1,idx2) in 
    Private.iterator_inside_nested_parentheses_parsing 
    (s,joiners,seeker) initial_vals;;
    
(*

parse_nested_parentheses ("(",",",")") "f(ab,cde,gh)ijk" ;;

parse_nested_parentheses ("(",",",")") "g(1,f(ab,cde,gh)ijk,2,h(k(u(6,7),v)),3)" ;;

*)  


   
let to_intlist enclosed_s =
   let n=String.length enclosed_s in 
   let s=Cull_string.interval enclosed_s 2 (n-1) in 
   let temp1=Cull_string.extract_intervals_in_wrt_separator s ";" in 
   Image.image int_of_string temp1;;

let of_intlist l=
  let temp1=Image.image string_of_int l in 
  "["^(String.concat ";" temp1)^"]";;

(*


let z1=[2;7;3;51];;
let z2=of_intlist z1;;
let check =(to_intlist(z2)=z1);;

*)  

let soak (replacee,replacer) s=
   if String.starts_with ~prefix:replacee s 
   then Some(replacer^(Cull_string.two_sided_cutting (replacee,"") s))
   else None ;;

(*

soak ("abc/def","DEF/GHI") "abc/def/klm/pqr" ;;
soak ("abc/def","DEF/GHI") "azc/def/klm/pqr" ;;
soak ("abc/def","DEF/GHI") "azc/def" ;;

*)


let escaped_and_quoted text =
   match Str.split (Str.regexp_string "\n") text with 
   [] -> "\"\""
   |first_line :: other_lines ->
    let quoted_lines = (enclose first_line)::
     (Image.image (fun line->enclose("\n"^line)) other_lines) in 
    String.concat "^\n" quoted_lines;;


(*

let z1 = "abc\nde\nfghi\njkl";;
print_string(escaped_and_quoted z1);;



*)

let reposition_whole_according_to_separator separator lines =
      let temp1 = Image.image (fun line->(line,Option.get(Substring.leftmost_index_of_in_from_opt separator line 1))) lines in 
      let max_idx = snd(Max.maximize_it snd temp1) in 
      Image.image (fun (line,idx)->
          let offset = max_idx-idx in 
          (String.make offset ' ')^line
      ) temp1;;

let reposition_left_hand_side_according_to_separator separator lines =
         let temp1 = Image.image (fun line->
              let j = Option.get(Substring.leftmost_index_of_in_from_opt separator line 1) in 
              ((Cull_string.beginning (j-1) line,Cull_string.cobeginning (j-1) line),j)) lines in 
         let max_idx = snd(Max.maximize_it snd temp1) in 
         Image.image (fun ((left,right),idx)->
             let offset = max_idx-idx in 
             left^(String.make offset ' ')^right
         ) temp1;;      

let announce ~trailer ~printer ~items ~separator=
   if items = []
   then ()
   else   
   let temp1 = Image.image printer items in
   let msg = "\n\n"^trailer^"\n\n"^(String.concat separator temp1)^"\n\n" in 
   (print_string msg;flush stdout) ;;    
