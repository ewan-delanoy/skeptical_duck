(*

#use"cull_string.ml";;

*)




let interval s a b=String.sub s (a-1) (b-a+1);;

let neighborhood_with_center_and_size s i d=
   let a=max(1)(i-d)
   and b=min(String.length s)(i+d) in
   interval s a b;;

exception Beginning_failure;;

let beginning k s=
   if k<1 then "" else
   let n=String.length(s) in
   if (k>n)
   then raise(Beginning_failure)
   else String.sub s 0 k;;
   
exception Ending_failure;;   
   
 let ending k s=
   if k<1 then "" else
   let n=String.length(s) in
   if (k>n)
   then raise(Ending_failure)
   else String.sub s (n-k) k;;
    
 let cobeginning k s=ending (String.length(s)-k) s;; 
 
 let coending k s=beginning (String.length(s)-k) s;; 
 
 let resize_from_left s p c=
   let d=p-String.length(s) in
   if d>0
   then s^(String.make d c)
   else beginning p s;;
   
  let resize_from_right s p c=
   let d=p-String.length(s) in
   if d>0
   then (String.make d c)^s
   else ending p s;;  
     
 type leftwing_length=int;;
 type rightwing_length=int;;  
   
 let without_the_lid  (a:leftwing_length) s (b:rightwing_length)=
   String.sub s a (String.length(s)-b-a);;
 
 
let before_and_after w x=
  let j=Substring.leftmost_index_of_in(w)(x) in
  if j=(-1) then None else 
   Some(  beginning (j-1) x,
    cobeginning (j+String.length(w)-1) x);;

(*
let extract_intervals 
*)

let trim_spaces_on_the_left s=
      let n=String.length s in
      match Option.seek(fun j->
          not(List.mem (String.get s (j-1)) [' ';'\t';'\r';'\n'])
      )(Ennig.ennig 1 n) with
      None->""
      |Some(d)->cobeginning (d-1) s;;

let trim_spaces_on_the_right s=
      let n=String.length s in
      match Option.seek(fun j->
          not(List.mem (String.get s (n-j)) [' ';'\t';'\r';'\n'])
      )(Ennig.ennig 1 n) with
      None->""
      |Some(d)->coending (d-1) s;;
              

 let trim_spaces s=
   let n=String.length s in
   let opt1=Option.seek(fun j->not(List.mem(String.get s (j-1)) [' ';'\r';'\t';'\n']))(Ennig.ennig 1 n) in
   if opt1=None then "" else
   let i1=Option.unpack opt1 in
   let k1=Option.find(fun j->not(List.mem(String.get s (n-j)) [' ';'\r';'\t';'\n']))(Ennig.ennig 1 n) in 
   let j1=(n+1)-k1 in
   interval s i1 j1;;

exception Two_sided_cutting_exn of int*int*int;;

let two_sided_cutting (left_part,right_part) s=
   let n=String.length s 
   and l=String.length left_part 
   and r=String.length right_part in 
   let d=n-(l+r) in 
   if n<l+r
   then raise(Two_sided_cutting_exn(n,l,r)) 
   else String.sub s l d;;

(*

two_sided_cutting ("ab","efg") "abcdefg";;

*)      

 let closeup_around_index s j=
   let n=String.length s in
   let temp1=List.filter(fun j->(String.get s (j-1))='\n')(Ennig.ennig 1 n) in
   let (temp2,temp3)=Prepared.partition_in_two_parts(fun k->k<j) temp1 in
   let a=(if List.length(temp2)<6 then 1 else List.nth(List.rev temp2)(5))
   and b=(if List.length(temp3)<6 then n else List.nth(temp3)(5)) in
   String.sub s (a-1) (b-a);;
   
exception Absent_beginning_marker of string;;
exception Absent_ending_marker of string;; 
 
let between_markers (bm,em) s=
     if (bm,em)=("","") then s else
     let i1=Substring.leftmost_index_of_in_from bm s 1  in
     if i1<1 then raise(Absent_beginning_marker(bm)) else
     let j1=i1+(String.length bm) in
     let i2=Substring.leftmost_index_of_in_from em s (j1+1) in
     if i2<1 then raise(Absent_ending_marker(bm)) else
     interval s j1 (i2-1);; 
 
let optional_between_markers p s=
   try Some(between_markers p s) with _->None;; 
   
(*

between_markers ("aaa","bb") "123aaa45bb678";;

*)     
   
let split_wrt_rightmost s c=
   let i=(try String.rindex(s)(c) with _->(-1)) in
   if i<0
   then ("",s)
   else (String.sub s 0 i,String.sub s (i+1) ((String.length s)-i-1) );;

let before_rightmost s c=fst(split_wrt_rightmost s c);;
let after_rightmost s c=snd(split_wrt_rightmost s c);;

let before_rightmost_possibly_all s c=
   let i=(try String.rindex(s)(c) with _->(-1)) in
   if i<0
   then s
   else String.sub s 0 i;;



  
             