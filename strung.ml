(*

#use"strung.ml";;

*)


let get s i=String.get s (i-1);;
 
let set s i c=Bytes.set s (i-1) c;;

let enclose s=
  let encloser="\"" in
  let n=String.length s in 
  let accu=ref[] in
  let _=(for k=n downto 1 do
     let c=String.get s (k-1) in 
     let element=(
        if c='"'
        then "\\\""
        else String.make 1 c
     ) in 
     accu:=element::(!accu)
  done) in 
  encloser^(String.concat "" (!accu))^encloser;;


let implode l=
   let n=List.length(l) in
   let by=Bytes.make n ' ' in
   let _=(for i=0 to (n-1) do Bytes.set by i (List.nth l i) done;) in
   Bytes.to_string by;;
  
    
let explode s=
    let n=String.length s in
    Ennig.doyle (String.get s) 0 (n-1);;
    
 
let char_finder f s w0=
   let n=(String.length s) in
   let rec tempf=(fun j->
     if j>=n then 0 else
     if f(String.get s  j) then j+1 else
     tempf(j+1)
   ) in
   tempf(w0-1);;

let backwards_char_finder f s w0=
    let rec tempf=(fun j->
      if j<0 then 0 else
      if f(String.get s  j) then j+1 else
      tempf(j-1)
    ) in
    tempf(w0-1);;   
 
let show_indices s=
  let n=String.length s in
  Ennig.doyle (fun i->(i,String.get s (i-1)) ) 1 n;;   
   
let number_of_lines_before = Substring.Friend.number_of_lines_before;;

exception Integer_too_big_for_string_of_int;; 

let left_completed_string_of_int l_max m=
   let s1=string_of_int(m) in
   let d=l_max-(String.length s1) in
   if d<0
   then raise(Integer_too_big_for_string_of_int)
   else
   (String.make d '0')^s1;;


     
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
   Option.find_and_stop tester candidates;;
  
(*

find_one_of_several_in_at_idx ["ba";"ab"] "123ab67" 4;;

*)

let find_one_of_several_in_from_idx candidates s idx =
  let n=String.length s in 
  Option.find_and_stop (
    find_one_of_several_in_at_idx candidates s
  ) (Ennig.ennig idx n);;

(*

find_one_of_several_in_from_idx ["ba";"ab"] "123ab67" 1;;

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
          let temp1=List.filter(fun k->(get s k)='\n')(Ennig.ennig idx (idx2-1)) in 
          let line_idx_for_idx2=line_idx+List.length(temp1) in 
          let msg="Found "^(remove_newlines candidate)^" at line number "^(string_of_int line_idx_for_idx2)^"\n" in 
          let _=(print_string msg;flush stdout) in 
          let idx3=idx2+(String.length candidate) in  
          let temp2=List.filter(fun k->(get s k)='\n')(Ennig.ennig idx2 (idx3-1)) in 
          let line_idx_for_idx3=line_idx_for_idx2+List.length(temp2) in 
          tempf((idx2,idx3-1)::treated,other_patts,idx3,line_idx_for_idx3)    
  ) in 
  let temp3=List.filter(fun k->(get s k)='\n')(Ennig.ennig 1 (start_idx-1)) in 
  let start_line_idx = 1+(List.length(temp3)) in 
  tempf([],patterns_in_order,start_idx,start_line_idx);;

(*

find_successively_in [["ba";"ab"];["cde";"edc"]] "12\n\n\n\n\n8ab123\n\n67cde12";;

*)

let partition_from_set_of_ranges l n=
    if l=[] then [1,n,false] else 
    let (last_i,last_j)=List.hd(List.rev l) 
    and (first_i,_)=List.hd l in
    let temp2=Listennou.universal_delta_list l in  
    let temp3=Image.image (fun ((i1,j1),(i2,j2))->
      [(i1,j1,true);(j1+1,i2-1,false)]
    ) temp2 in 
    let middle_part=List.flatten temp3 in
    let first_part=(if first_i>1 then [(1,first_i-1,false)] else []) 
    and last_part=(if last_j<n then [(last_j+1,n,false)] else []) in 
    first_part@middle_part@[(last_i,last_j,true)]@last_part;;

(*

partition_from_set_of_ranges [(3,7);(41,52)] 100;;
partition_from_set_of_ranges [(1,7);(41,52)] 100;;

*)


let replace_ranges_in l s=
    if l=[] then s else
    let n=String.length s in
    let ranges=Image.image fst l in
    let partition=partition_from_set_of_ranges ranges n in 
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
    let temp3=Ordered.forget_order(Tidel.diforchan((n+1)::temp2)) in
    let temp4=Listennou.universal_delta_list temp3 in
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
   match Option.seek(fun j->
      (get s1 j)<>(get s2 j)
   )(Ennig.ennig 1 n) with 
   None->None 
   |Some(j0)->
      let common_part=String.sub s1 0 (j0-1) 
      and s1_part=String.sub s1 j0 (n1-j0)
      and s2_part=String.sub s2 j0 (n2-j0) in 
      Some(common_part,get s1 j0,get s2 j0,s1_part,s2_part);;

(*
leftmost_difference "abc1def" "abc257";;
*)

(*
module Private = struct 

let is_a_substring_located_at y x old_j =
    let j=old_j-1 in
    let ly=String.length(y) in
      if (String.length(x)<j+ly)||(j<0)
      then false
      else (String.sub x j ly)=y;;

end;;


exception Missing_opener_in_triune_analysis of string*string;;

let triune_analysis 
  openr separatr closr s=
    let n=String.length s in 
    let tester=(fun j->
       if Private.is_a_substring_located_at openr s j then Some(j,1) else 
       if Private.is_a_substring_located_at separatr s j then Some(j,2) else
       if Private.is_a_substring_located_at closr s j then Some(j,3) else
       None
    ) in 
    let seeker = (fun k-> Option.find_and_stop tester (Ennig.ennig k n) in 
    let opt1=seeker 1 in 
    if opt1=None 
    then raise(Missing_opener_in_triune_analysis(openr,s))
    else  
    let (idx1,case1)=(Option.unpack opt1) in 
    
  
  Substring.leftmost_index_of_in
           
let leftmost_index_of_in_from x y i=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      match Ennig.find_it tester (i-1) (String.length(y)-lx) with
         None->(-1)
        |Some(k)->k+1;;

*)