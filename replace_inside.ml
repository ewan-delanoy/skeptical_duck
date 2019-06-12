(*

#use"replace_inside.ml";;

*)


let replace_inside_string (a,b) s=
  Alternative_global_replace.my_global_replace (a,b) s;;
 
let replace_several_inside_string l t=List.fold_left 
(fun s (a,b)->replace_inside_string (a,b) s) t l;;  
 
let replace_inside_file (a,b) fn=
    let s1=Io.read_whole_file fn in
    let la=String.length(a) in
    if List.exists (fun j->(String.sub s1 j la)=a) (Ennig.ennig 0 ((String.length s1)-la))
    then let s2=replace_inside_string (a,b) s1 in
         Io.overwrite_with fn s2
    else ();; 
    
let replace_several_inside_file l fn=
    let s1=Io.read_whole_file fn in
    let s2=replace_several_inside_string l s1  in
    Io.overwrite_with fn s2;; 

exception Absent_beginning_marker of string;;
exception Absent_ending_marker of string;; 
 
let overwrite_between_markers_inside_string ovw_b (bm,em)
   s1=
     let b=Overwriter.to_string ovw_b in
     if (bm,em)=("","") then b else
     let substring_leftmost_index_from=(fun x y i0->
      let lx=String.length(x) and ly=String.length(y) in
      let rec tempf=(fun j->
        if j>ly-lx then (-1) else 
        if (String.sub y j lx)=x then j else (tempf(j+1))
      ) in
      tempf i0) in
     let i1=substring_leftmost_index_from bm s1 0 in
     if i1=(-1) then raise(Absent_beginning_marker(bm)) else
     let j1=i1+(String.length bm)-1 in
     let i2=substring_leftmost_index_from em s1 (j1+1) in
     if i2=(-1) then raise(Absent_ending_marker(em)) else
     let before=String.sub s1 0 (j1+1)
     and after=String.sub s1 i2 (String.length(s1)-i2) 
     in
     before^b^after ;; 
     
let overwrite_between_markers_inside_file 
   ovw_b (bm,em)
   fn =
    let s1=Io.read_whole_file fn in
    let s2=overwrite_between_markers_inside_string ovw_b (bm,em) s1 in
    Io.overwrite_with fn s2;;      


let overwrite_and_dump_markers_inside_string ovw_b (bm,em)
   s1=
     let b=Overwriter.to_string ovw_b in
     if (bm,em)=("","") then b else
     let substring_leftmost_index_from=(fun x y i0->
      let lx=String.length(x) and ly=String.length(y) in
      let rec tempf=(fun j->
        if j>ly-lx then (-1) else 
        if (String.sub y j lx)=x then j else (tempf(j+1))
      ) in
      tempf i0) in
     let i1=substring_leftmost_index_from bm s1 0 in
     if i1=(-1) then raise(Absent_beginning_marker(bm)) else
     let j1=i1+(String.length bm)-1 in
     let i2=substring_leftmost_index_from em s1 (j1+1) in
     if i2=(-1) then raise(Absent_ending_marker(bm)) else
     let corrected_i2=i2+(String.length bm)-1 in
     let before=String.sub s1 0 i1
     and after=String.sub s1 corrected_i2 (String.length(s1)-corrected_i2) 
     in
     before^b^after ;; 
     
let overwrite_and_dump_markers_inside_file 
   ovw_b (bm,em)
   fn =
    let s1=Io.read_whole_file fn in
    let s2=overwrite_and_dump_markers_inside_string ovw_b (bm,em) s1 in
    Io.overwrite_with fn s2;;      
 
(* 


 overwrite_between_markers_inside_string
  (Overwriter.of_string "456")
  ("aaa","bb")
   "123aaa5678bb78910" ;;    
   
overwrite_and_dump_markers_inside_string
  (Overwriter.of_string "456")
  ("aaa","bb")
   "123aaa5678bb78910" ;;       
   
     
*)

let at_intervals_inside_string s l=
  if l=[] then s else
  let n=String.length s in
  let temp1=Listennou.universal_delta_list l 
  and ((i_first,_),_)=List.hd(l)
  and ((i_last,j_last),rep_last)=List.hd(List.rev l) in
  let temp2=Image.image (fun (((i1,j1),rep1),((i2,j2),rep2))->
      rep1^(String.sub s j1 (i2-j1-1))
  ) temp1 in
  let first_part=(String.sub s 0 (i_first-1))
  and last_part=rep_last^(String.sub s j_last (n-j_last)) in
  first_part^(String.concat "" temp2)^last_part;;

let at_intervals_inside_file 
  fn l=
   let s1=Io.read_whole_file fn in
   let s2=at_intervals_inside_string s1 l in
   Io.overwrite_with fn s2;;     

(*    

at_intervals_inside_string "12345678901234567890" [(3,5),"right";(12,17),"again"];;

*)         

let choose_once_inside_string_from (candidates,replacement) s (idx0,idx0_in_old_s)=
  match Option.find_and_stop (
    fun candy ->
       let idx=Substring.leftmost_index_of_in_from candy s idx0 in 
       if idx<0 then None else Some(idx,candy)
  ) candidates with 
  None->None
  |Some(idx1,candidate)->
  let idx1=Substring.leftmost_index_of_in candidate s in 
  if idx1<0 then None else 
  let idx2_in_s=idx1+(String.length candidate)-1 in 
  let left_side = Cull_string.beginning (idx1-1) s 
  and right_side = Cull_string.cobeginning idx2_in_s s in
  let idx2_in_old_s= 
  Some(left_side^replacement^right_side,idx2);;      

(*
choose_once_inside_string_from (["17";"12";"19"],"348") "abc12def" 1;;
*)

exception No_equivalent_found_for of string;;

let successively_inside_string_from l_reps s idx0=
  let rec tempf=(fun (to_be_treated,current_s,idx)->
      match to_be_treated with 
      []->current_s
      |rep::other_reps ->
        (
          match choose_once_inside_string_from rep s idx with
          None->raise(No_equivalent_found_for(snd rep))
          |Some(new_s,new_idx)->
            let name=Str.global_replace (Str.regexp_string "\n") " " (snd rep) in 
            let _=(print_string("Treated "^name^"\n");flush stdout) in 
            tempf(other_reps,new_s,new_idx)  
        )
  ) in 
  tempf(l_reps,s,idx0);;

(*
successively_inside_string_from [["q1"],"p1";["q2"],"p2";["q3"],"p3"] "ABq1CDEq2FGq3" 1;;
successively_inside_string_from [["q1"],"p1";["q7"],"p2";["q3"],"p3"] "ABq1CDEq2FGq3" 1;;
*)


