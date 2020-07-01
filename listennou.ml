
(*

#use"listennou.ml";;

*)



exception Ht_exn;;
exception Reposition_first_key_not_found;;
exception Reposition_second_key_not_found;;
exception Push_immediately_after_exn;;


let ht x=match x with
    []->raise(Ht_exn)
    |a::b->(a,b);;

let rec uncurrified_rev_append (x,y)=match x with
[]->y
|a::peurrest->uncurrified_rev_append (peurrest,a::y);;

let rec uncurrified_append (x,y)=uncurrified_rev_append (List.rev x,y);;

let factor (x,y)=
    let rec factor0=(fun
       (graet,da_ober1,da_ober2)->
       if (da_ober1=[])||(da_ober2=[])
       then (List.rev graet,da_ober1,da_ober2)
       else let (a1,peurrest1)=ht da_ober1
            and (a2,peurrest2)=ht da_ober2 in
            if a1=a2
            then factor0(a1::graet,peurrest1,peurrest2)
            else (List.rev graet,da_ober1,da_ober2)
    ) in
    factor0([],x,y);;

let comparable_for_prefix_order  a b=
    let (_,a1,b1)=factor(a,b) in (a1=[])||(b1=[]);;

let extends l1 l2=
   let (_,_,r2)=factor (l1,l2) in r2=[];;


let didrochan x=
let rec didrochan0=
(function (u,accu1,accu2,bowl)->match u with
 []->(accu1,accu2)
 |a::b->if bowl
        then didrochan0(b,a::accu1,accu2,false)
        else didrochan0(b,accu1,a::accu2,true))  
in
didrochan0(x,[],[],true);;

let find_index x ll=
let rec sub_f=
(function (j,l)->match l with
[]->(-1)      
|u::v->if u=x then j else sub_f(j+1,v)) in
sub_f(1,ll);;

exception Force_find_exn ;;

let rec force_find f x=
   match x with 
   [] -> raise(Force_find_exn)
   |a::others -> if f a 
                 then a 
                 else force_find f others ;; 

let morzholan f x=
let rec sub_f=(function (u,v)->if u=v then u else sub_f(v,f(v)))
in sub_f(x,f(x));;

let rec morzhol_bihan f k x=
if k=0 then x else morzhol_bihan f (k-1) (f(x));;

exception Big_rht_exn of int*int;;

let big_rht r l=let rec tempf=
(function (j,kleiz,dehou)->
if j=0 then (kleiz,dehou) else 
match dehou with
[]->raise(Big_rht_exn(r,List.length l))
|a::peurrest->tempf(j-1,a::kleiz,peurrest)
) in
tempf(r,[],l);;

let big_head r l=if (r>(List.length l)) then l else List.rev(fst(big_rht(r)(l)));;

let big_tail r l=if (r>(List.length l)) then [] else snd(big_rht(r)(l));;

let remove_element_at_idx l k=
   let (kleiz,dehou)=big_rht k l in 
   List.rev_append (List.tl kleiz) dehou;;

(* remove_element_at_idx [1; 2; 3; 4; 5; 6; 7] 3;; *)   

let decompose_wrt_two_indices l i j=
   let (r_part1,temp1)=big_rht (i-1) l in 
   let (ei,temp2)=ht temp1 in 
   let (r_part2,temp3)=big_rht (j-i-1) temp2 in 
   let (ej,part3)=ht temp3 in 
   (List.rev r_part1,ei,List.rev r_part2,ej,part3);;

(* decompose_wrt_two_indices [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12] 3 7;; *)

let extract_interval l i j=
  let (r_part1,temp1)=big_rht (i-1) l in 
  let (r_part2,part3)=big_rht (j-i+1) temp1 in 
  (List.rev r_part1,List.rev r_part2,part3);;

(* extract_interval [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12] 3 7;; *)

let decompose_wrt_element l elt1=
  let rec tempf=(
     fun (treated,to_be_treated)->match to_be_treated with 
     []->(List.rev treated,false,[])
    |elt::other_elts ->
       if elt=elt1
      then (List.rev(treated),true,other_elts)
      else tempf(elt::treated,other_elts)
  ) in 
  tempf([],l);; 

(* decompose_wrt_element [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12] 3;; *)



let reposition_by_putting_snd_immediately_after_fst l elt_i elt_j=
    let (left1,found1,right1)=decompose_wrt_element l elt_i in 
    if not found1 then raise(Reposition_first_key_not_found) else 
    let (left2,found2,right2)=decompose_wrt_element right1 elt_j in 
    if not found2 then raise(Reposition_second_key_not_found) else
    left1@(elt_i::elt_j::(left2@right2));; 
  
(* reposition_by_putting_snd_immediately_after_fst [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12] 3 7;; *)  


let power_set l=
let rec tempf=
(function (da_ober,graet)->match da_ober with
[]->graet
|a::peurrest->tempf(peurrest,graet@(Image.vorstellung(function y->a::y)(graet)))
) in
tempf(List.rev(l),[[]]);;


let fold_right f x0 l=List.fold_left(function x->(function a->f a x)) x0 l;;



let universal_delta_list l=
let rec sub_f=
(function (accu,a,rl)->match rl with
[]->List.rev(accu)
|b::x->sub_f((a,b)::accu,b,x)
) in
match l with
[]->[]
|u::v->sub_f([],u,v);;

 
let delete_redundancies r l=
 let rec tempf=(function
   (graet,da_ober)->match da_ober with
   []->List.rev(graet)
   |x::peurrest->
     if List.exists(function y->r y x)(peurrest)
     then tempf(graet,peurrest)
     else let temp1=List.filter(function y->not(r x y))(peurrest) in
          tempf(x::graet,temp1)
 ) in
 tempf([],l);;

let nonredundant_version l=
  let rec tempf=(
    fun (graet,da_ober)->
      match da_ober with
      []->List.rev graet
      |a::peurrest->if List.mem a graet
                    then tempf(graet,peurrest)
                    else tempf(a::graet,peurrest)
  ) in
  tempf([],l);;

let rev_map f l=
   let rec tempf=(
     fun (graet,da_ober)->match da_ober with
     []->graet
     |a::peurrest->tempf((f a)::graet,peurrest)
   ) in
   tempf([],l);;
   
let redundant_indices l=
  let rec tempf=(
    fun (counter,already_known,bad_indices,to_be_treated)->
      match to_be_treated with
      []->List.rev bad_indices
      |a::others->
        let idx=counter+1 in  
        if List.mem a already_known
        then tempf(idx,already_known,idx::bad_indices,others)
        else tempf(idx,a::already_known,bad_indices,others)
  ) in
  tempf(0,[],[],l);;

(*
redundant_indices [1; 2; 1; 4; 5; 6; 3; 8; 9; 10; 11; 12; 13; 6; 15];;
*)

let divide_by_two l=
   let rec tempf=(
     fun (treated,to_be_treated)->match to_be_treated with 
     []->(List.rev treated,None)
     |a1::others1->(
         match others1 with 
         []->(List.rev treated,Some(a1))
         |a2::others->tempf((a1,a2)::treated,others)
      )
   ) in 
   tempf ([],l);;

let push_immediately_after l elt2 elt1 =
  let rec tempf=(
    fun (treated,to_be_treated)->match to_be_treated with 
     []->raise(Push_immediately_after_exn)
    |elt::others ->
      if elt=elt1
      then List.rev_append treated (elt::elt2::others)
      else tempf(elt::treated,others)
  ) in 
  tempf([],l);; 

let hi=List.length;;
let rev=List.rev;;

(*

push_immediately_after [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12] 145 3;;

*)

let partition_from_set_of_ranges l n=
    if l=[] then [1,n,false] else 
    let (last_i,last_j)=List.hd(List.rev l) 
    and (first_i,_)=List.hd l in
    let temp2=universal_delta_list l in  
    let temp3=Image.vorstellung (fun ((i1,j1),(i2,j2))->
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

let extract_intervals_in_complement l n =
   let enhanced_l = 0::(l@[n+1]) in 
   let temp1=universal_delta_list enhanced_l in
   let temp2= Image.vorstellung (fun (x,y)->(x+1,y-1)) temp1 in 
   List.filter (fun (a,b)->a<=b) temp2;;

(*

extract_intervals_in_complement [3;7;8;20] 30;;
extract_intervals_in_complement [1;7;8;20] 30;;
extract_intervals_in_complement [1;7;8;30] 30;;

*)   

let complement_union_of_ranges ranges n=
   let rec tempf=(fun 
     (already_treated,a,b,to_be_treated)->
       match to_be_treated with 
       []->List.rev((a,b)::already_treated)
       |(x1,y1)::other_ranges->
         tempf((a,x1-1)::already_treated,y1+1,b,other_ranges)
   ) in 
   let temp1=tempf([],1,n,ranges) in 
   List.filter (fun (x,y)->x<=y) temp1;;

(*

complement_union_of_ranges [3,7;8,20] 30;;
complement_union_of_ranges [3,7;9,20] 30;;
complement_union_of_ranges [1,7;9,20] 30;;
complement_union_of_ranges [1,7;9,30] 30;;
complement_union_of_ranges [1,7;8,30] 30;;

*)


let split_list_in_half l=
   let temp1=Ennig.index_everything(l) in 
   let (temp2,temp3)=List.partition (fun (j,_)->(j mod 2)=1) temp1 in 
   (Image.vorstellung snd temp2,Image.vorstellung snd temp3);;

(*

split_list_in_half [1; 2; 3; 4; 5; 6; 7];;
split_list_in_half [1; 2; 3; 4; 5; 6; 7; 8];;

*)   



let unequal_combine l1 l2 =
   let rec tempf=(fun
     (treated,to_be_treated1,to_be_treated2)->
       match to_be_treated1 with 
       []->List.rev(treated)
       |a1::others1->(
                       match to_be_treated2 with 
                        []->List.rev(treated)
                        |a2::others2 -> tempf((a1,a2)::treated,others1,others2)
                     )
   ) in 
   tempf([],l1,l2);;

exception Fst_is_largest of int * int;;
  

let unequal_combine_where_fst_is_smallest l1 l2 =
   let n1=List.length(l1) and n2=List.length(l2) in 
   if n1>n2 then raise(Fst_is_largest(n1,n2)) else   
   unequal_combine l1 l2;;


exception  Extract_successive_pairs_exn of int;;

let extract_successive_pairs_from_even_list l=
   let m1 =(List.length l) in 
   if (m1 mod 2)<>0 then raise(Extract_successive_pairs_exn(m1)) else 
   let m2=m1/2 in 
   Ennig.doyle (fun j->
      (List.nth l (2*j-2),List.nth l (2*j-1)) 
   ) 1 m2;;
