(*


In an emergency situation, 
go up one directory, open a fresh terminal, load this 
file with the above command and call 
Assistance_usual_coma_state.refresh() )

#load"unix.cma";;
#load"str.cma";;
#use"Ordinary/watched/watched_and_githubbed/assistance.ml";;

*)


module Assistance_hurried=struct

(*

#use"hurried.ml";;

*)

module Private = struct

let partition_in_two_parts f l=
  let rec tempf=(fun
   (graet,da_ober)->match da_ober with
     []->(List.rev graet,[])
     |a::peurrest->
        if f(a)
        then tempf(a::graet,peurrest)
        else (List.rev graet,da_ober)
  ) in
  tempf([],l);; 

(* partition_in_two_parts (fun x->(x mod 3)<>0) (Ennig.ennig 1 21) *)

end ;; 




let connected_components f l = 
  let rec tempf = (fun 
    (treated,to_be_treated) -> match to_be_treated with 
       [] -> List.rev treated 
     | a :: others ->
        let fa = f a in 
        let (left,right) = Private.partition_in_two_parts (fun x-> f x=fa) others in 
        tempf((a::left)::treated,right)
  ) in 
  tempf([],l) ;;

(* connected_components (fun x->(x mod 3)<>0) (Ennig.ennig 1 21) *)  

let partition_in_two_parts = Private.partition_in_two_parts ;;    

end;;






module Assistance_image=struct

(*

#use"image.ml";;
The most used function in all those modules !


*)


let image f l=
  let rec tempf=(fun
   (graet,da_ober)->match da_ober with
   []->List.rev graet
   |a::peurrest->tempf(f(a)::graet,peurrest)
  ) in
  tempf([],l);;



end;;






module Assistance_int_range=struct

(*

#use"int_range.ml";;

*) 
let scale f a b=
let accu=ref([]) in
let rec doyle0=(function
j->if j<a
   then (!accu)
 else let _=(accu:=f(j)::(!accu)) in doyle0(j-1)
) in
doyle0 b;;

let descending_scale f b a =
  scale (fun x->f(a+b-x)) a b ;; 

let slow_scale f a b=
let accu=ref([]) in
let rec slow_doyle0=(function
j->if j>b
   then List.rev(!accu)
 else let _=(accu:=f(j)::(!accu)) in slow_doyle0(j+1)
) in
slow_doyle0 a;;


let scale_for_delta f n u0=
let accu=ref([u0]) and traveler=ref(u0) in
let rec doyle0=(function
da_ober->if da_ober<1
         then List.rev(!accu)
       else let _=(traveler:=f(!traveler);accu:=(!traveler)::(!accu)) in 
            doyle0(da_ober-1)
) in
doyle0 n;;
 

let range a b=scale (function x->x) a b;; 

let index_everything l=
 let rec tempf=
  (function (j,graet,da_ober)->
    match da_ober with
     []->graet
    |a::b->tempf(j-1,(j,a)::graet,b)
   )    in
   tempf(List.length(l),[],List.rev(l));;

let for_all f a b=
let rec for_all0=(function
j->if j>b
   then true
 else if f(j)
      then for_all0(j+1)
    else false
) in
for_all0 a;;

let rec exists f a b=
if (a>b) 
then false
else if f(a)
  then true
  else exists f (a+1) b;;	 

let rec find_it f a b=
if (a>b) 
then None
else if f(a)
  then Some(a)
  else find_it f (a+1) b;;	  

let rec find_and_stop f a b=
let rec find_and_stop0=(function
 j->if (j>b)
    then None
  else match f(j) with
   None->find_and_stop0(j+1)
   |Some(x)->Some(x)
) in
find_and_stop0 a;;

let constant_list n x=scale (function j->x) 1 n;;

let describe_fibers_as_intervals f a b=
 if (a>b) then [] else
 let rec tempf=(function
   (graet,x1,x2,y0)->
      if (x2>=b) then List.rev((x1,x2,y0)::graet) else
      let x3=x2+1 in
      let y3=f(x3) in
      if (y3=y0)
      then tempf(graet,x1,x3,y0)
      else tempf((x1,x2,y0)::graet,x3,x3,y3)
 
 ) in
 tempf([],a,a,f(a));;

let test_for_interval l=
 match l with 
  [] -> Some(1,0) 
 |a :: others ->
    (
      match List.rev others with 
       [] -> Some(a,a)
       | b :: _-> if l = range a b 
                  then Some(a,b)
                  else None 
    )  ;;

(* test_for_interval [2;3;4;5] ;; *)


let reposition_by_putting_snd_immediately_after_fst i j t=
    if t<=i then t else
    if t=i+1  then j else
    if t<=j  then t-1 else t;;

let repeat n x = scale (fun _->x) 1 n ;;

(* repeat 3 "a" ;; *)          
  

end;;






module Assistance_memoized=struct

(*

#use"memoized.ml";;

*) 
type ('a,'b) map=('a->'b);;

let make_from (f:'a->'b) (a_hashtbl_for_f:('a,'b) Hashtbl.t)=
  let memoized_f=(fun x->
     if Hashtbl.mem(a_hashtbl_for_f)(x)
     then Hashtbl.find(a_hashtbl_for_f)(x)
     else let y=f(x) in
          let ()=(Hashtbl.add(a_hashtbl_for_f) x y) in
          y
  ) in
  (memoized_f:>('a,'b) map);;

let make (f:'a->'b)=
  let a_hashtbl_for_f=Hashtbl.create(100) in
  make_from f a_hashtbl_for_f;;
  
let recursive_from=((fun (big_f:('a->'b)->'a->'b) (a_hashtbl_for_f:('a,'b) Hashtbl.t)->
  let rec memoized_f=(fun x->
     if Hashtbl.mem(a_hashtbl_for_f)(x)
     then Hashtbl.find(a_hashtbl_for_f)(x)
     else let mf=(memoized_f:>('a->'b)) in
          let y=big_f(mf)(x) in
          let ()=(Hashtbl.add(a_hashtbl_for_f) x y) in
          y
  ) in
  memoized_f):>(('a->'b)-> 'a -> 'b) -> (('a,'b) Hashtbl.t) -> ('a, 'b) map);;

let recursive (big_f:('a->'b)->'a->'b)=
  let a_hashtbl_for_f=Hashtbl.create(100) in
  recursive_from big_f a_hashtbl_for_f;;

let small f initial_value=
  recursive(fun old_f k->if k<1 then initial_value else f(old_f(k-1)));;
  
let reversible (f:'a->'b)=
  let a_hashtbl_for_f=Hashtbl.create(100) 
  and a_hashtbl_for_the_inverse_of_f=Hashtbl.create(100)
  and a_hashtbl_for_the_second_inverse_of_f=Hashtbl.create(100)
  and a_hashtbl_for_the_projector=Hashtbl.create(50) 
  and irreducibles=ref([]) 
  and minimal_reductions=ref([]) in
  let compute_f=(fun x accu->
     let y=f(x) in
     let ()=(Hashtbl.add(a_hashtbl_for_f) x y;accu:=[y]) in
      if Hashtbl.mem(a_hashtbl_for_the_second_inverse_of_f)(y)
     then let old_x=Hashtbl.find(a_hashtbl_for_the_inverse_of_f)(y) in
          Hashtbl.add(a_hashtbl_for_the_projector)(x)(old_x)
     else     
     if Hashtbl.mem(a_hashtbl_for_the_inverse_of_f)(y)
     then let old_x=Hashtbl.find(a_hashtbl_for_the_inverse_of_f)(y) in
          (Hashtbl.add(a_hashtbl_for_the_projector)(x)(old_x);
          Hashtbl.add(a_hashtbl_for_the_second_inverse_of_f)(y)(x);
          minimal_reductions:=(x,old_x)::(!minimal_reductions))
     else (Hashtbl.add(a_hashtbl_for_the_inverse_of_f)(y)(x);
            irreducibles:=x::(!irreducibles))
     
  ) in
  let memoized_f=(fun x->
     if Hashtbl.mem(a_hashtbl_for_f)(x)
     then Hashtbl.find(a_hashtbl_for_f)(x)
     else let accu=ref([]) in
          let _=compute_f(x)(accu) in
          List.hd(!accu)
  ) 
  and memoized_inverse_of_f=Hashtbl.find(a_hashtbl_for_the_inverse_of_f) in
  let memoized_projector=(fun x->
    let ()=compute_f(x)(ref[]) in
    if Hashtbl.mem(a_hashtbl_for_the_projector)(x)
    then Hashtbl.find(a_hashtbl_for_the_projector)(x)
    else x
    ) in
  (memoized_f,memoized_inverse_of_f,memoized_projector,irreducibles,minimal_reductions);;
           

end;;






module Assistance_int_uple=struct

(*

#use"int_uple.ml";;

*) 
let translate_pair a (x1,x2)=(a+x1,a+x2);;
let translate_triple a (x1,x2,x3)=(a+x1,a+x2,a+x3);;
let translate_fourtuple a (x1,x2,x3,x4)=(a+x1,a+x2,a+x3,a+x4);;
let translate_fiftuple a (x1,x2,x3,x4,x5)=(a+x1,a+x2,a+x3,a+x4,a+x5);;
let translate_sixtuple a (x1,x2,x3,x4,x5,x6)=(a+x1,a+x2,a+x3,a+x4,a+x5,a+x6);;

let next_pair (i,j)=
if i+1<j then (i+1,j) else
(1,i+2);;

let next_inclusive_pair (i,j)=
if i+1<=j then (i+1,j) else
(1,j+1);;


let next_triple (i,j,k)=
if i+1<j then (i+1,j,k) else
if i+2<k then (1,i+2,k) else
(1,2,i+3);;

let next_fourtuple (x1,x2,x3,x4)=
if x1+1<x2 then (x1+1,x2,x3,x4) else
if x2+1<x3 then (1, x2+1,x3,x4) else
if x3+1<x4 then (1,2,  x3+1,x4) else
(1,2,3,x4+1);;

let next_fiftuple (x1,x2,x3,x4,x5)=
if x1+1<x2 then (x1+1,x2,x3,x4,x5) else
if x2+1<x3 then (1, x2+1,x3,x4,x5) else
if x3+1<x4 then (1,2,  x3+1,x4,x5) else
if x4+1<x5 then (1,2,3,   x4+1,x5) else
(1,2,3,4,x5+1);;

let next_sixtuple (x1,x2,x3,x4,x5,x6)=
if x1+1<x2 then (x1+1,x2,x3,x4,x5,x6) else
if x2+1<x3 then (1, x2+1,x3,x4,x5,x6) else
if x3+1<x4 then (1,2,  x3+1,x4,x5,x6) else
if x4+1<x5 then (1,2,3,   x4+1,x5,x6) else
if x5+1<x6 then (1,2,3,4,    x5+1,x6) else
(1,2,3,4,5,x6+1);;


let inclusive_list_of_pairs=Assistance_memoized.make(function n->
        if n<1 then [] else
        if n=1 then [1,1] else        
        let accu=ref([],(1,1))
        and number_of_iterations=(n*(n+1))/2 
        and iterator=(function (l,c)->(c::l,next_inclusive_pair(c)) ) in
        let _=(for k=1 to number_of_iterations do
        accu:=iterator(!accu)
        done) in
        List.rev(fst (!accu)));;

let list_of_pairs=Assistance_memoized.make(function n->
if n<2 then [] else
let accu=ref([],(1,2))
and number_of_iterations=(n*(n-1))/2 
and iterator=(function (l,c)->(c::l,next_pair(c)) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
List.rev(fst (!accu)));;

let list_of_triples=Assistance_memoized.make(function n->
if n<3 then [] else
let accu=ref([],(1,2,3))
and number_of_iterations=(n*(n-1)*(n-2))/6 
and iterator=(function (l,c)->(c::l,next_triple(c)) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
List.rev(fst (!accu)));;

let list_of_fourtuples=Assistance_memoized.make(function n->
if n<4 then [] else
let accu=ref([],(1,2,3,4))
and number_of_iterations=(n*(n-1)*(n-2)*(n-3))/24 
and iterator=(function (l,c)->(c::l,next_fourtuple(c)) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
List.rev(fst (!accu)));;


let list_of_fiftuples=Assistance_memoized.make(function n->
if n<5 then [] else
let accu=ref([],(1,2,3,4,5))
and number_of_iterations=(n*(n-1)*(n-2)*(n-3)*(n-4))/120 
and iterator=(function (l,c)->(c::l,next_fiftuple(c)) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
List.rev(fst (!accu)));;


let list_of_sixtuples=Assistance_memoized.make(function n->
if n<6 then [] else
let accu=ref([],(1,2,3,4,5,6))
and number_of_iterations=(n*(n-1)*(n-2)*(n-3)*(n-4)*(n-5))/720 
and iterator=(function (l,c)->(c::l,next_sixtuple(c)) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
List.rev(fst (!accu)));;

let maximize_on_pairs f a b=
let n=(b-a)+1 and aa=(a-1) in
let accu=ref(f(a,a+1),[],(1,2))
and number_of_iterations=(n*(n-1))/2 
and iterator=(function (y0,lx,c)->
   let tc=translate_pair(aa)(c) and nc=next_pair(c) in
   let y=f(tc) in
   if y<y0 
   then (y0,lx,nc)
   else if y=y0
        then (y0,tc::lx,nc)
        else (y,[],nc) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
let last_term=(!accu) in
let final_y=(function (y,lx,c)->y)(last_term)
and final_list=(function (y,lx,c)->List.rev lx)(last_term) in
(final_y,final_list);;
  
let maximize_on_triples f a b=
let n=(b-a)+1 and aa=(a-1) in
let accu=ref(f(a,a+1,a+2),[],(1,2,3))
and number_of_iterations=(n*(n-1)*(n-2))/6 
and iterator=(function (y0,lx,c)->
   let tc=translate_triple(aa)(c) and nc=next_triple(c) in
   let y=f(tc) in
   if y<y0 
   then (y0,lx,nc)
   else if y=y0
        then (y0,tc::lx,nc)
        else (y,[],nc) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
let last_term=(!accu) in
let final_y=(function (y,lx,c)->y)(last_term)
and final_list=(function (y,lx,c)->List.rev lx)(last_term) in
(final_y,final_list);;  
  
let maximize_on_fourtuples f a b=
let n=(b-a)+1 and aa=(a-1) in
let accu=ref(f(a,a+1,a+2,a+3),[],(1,2,3,4))
and number_of_iterations=(n*(n-1)*(n-2)*(n-3))/24 
and iterator=(function (y0,lx,c)->
   let tc=translate_fourtuple(aa)(c) and nc=next_fourtuple(c) in
   let y=f(tc) in
   if y<y0 
   then (y0,lx,nc)
   else if y=y0
        then (y0,tc::lx,nc)
        else (y,[],nc) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
let last_term=(!accu) in
let final_y=(function (y,lx,c)->y)(last_term)
and final_list=(function (y,lx,c)->List.rev lx)(last_term) in
(final_y,final_list);;

let maximize_on_fiftuples f a b=
let n=(b-a)+1 and aa=(a-1) in
let accu=ref(f(a,a+1,a+2,a+3,a+4),[],(1,2,3,4,5))
and number_of_iterations=(n*(n-1)*(n-2)*(n-3)*(n-4))/120
and iterator=(function (y0,lx,c)->
   let tc=translate_fiftuple(aa)(c) and nc=next_fiftuple(c) in
   let y=f(tc) in
   if y<y0 
   then (y0,lx,nc)
   else if y=y0
        then (y0,tc::lx,nc)
        else (y,[],nc) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
let last_term=(!accu) in
let final_y=(function (y,lx,c)->y)(last_term)
and final_list=(function (y,lx,c)->List.rev lx)(last_term) in
(final_y,final_list);;
  
let maximize_on_sixtuples f a b=
let n=(b-a)+1 and aa=(a-1) in
let accu=ref(f(a,a+1,a+2,a+3,a+4,a+5),[],(1,2,3,4,5,6))
and number_of_iterations=(n*(n-1)*(n-2)*(n-3)*(n-4)*(n-5))/720
and iterator=(function (y0,lx,c)->
   let tc=translate_sixtuple(aa)(c) and nc=next_sixtuple(c) in
   let y=f(tc) in
   if y<y0 
   then (y0,lx,nc)
   else if y=y0
        then (y0,tc::lx,nc)
        else (y,[],nc) ) in
let _=(for k=1 to number_of_iterations do
accu:=iterator(!accu)
done) in
let last_term=(!accu) in
let final_y=(function (y,lx,c)->y)(last_term)
and final_list=(function (y,lx,c)->List.rev lx)(last_term) in
(final_y,final_list);;
    
  
  


end;;






module Assistance_option=struct

(*

#use"option.ml";;

*) 

exception Unpackable of string;;

module Private = struct 

let unpack_with_error_message s=function
None->raise(Unpackable(s))
|Some(x)->x;;

end ;;


let add_element_on_the_right l x=match x with
  None->l
  |Some(a)->l@[a];;
 
let rec filter_and_unpack f l=
 let rec filter0=(function
  (graet,da_ober)->match da_ober with
   []->List.rev(graet)
   |x::peurrest->match f(x) with
		None->filter0(graet,peurrest)
		|Some(y)->filter0(y::graet,peurrest)
 ) in
 filter0([],l);;

let  find_and_stop f l=
 let rec find_and_stop0=(function
  da_ober->match da_ober with
   []->None
   |a::peurrest->match f(a) with
		None->find_and_stop0(peurrest)
		|Some(x)->Some(x)
 ) in
 find_and_stop0(l);;

let propagate f=function
None->None
|Some(x)->Some(f(x));;

let rec seek f =function
[]->None
|a::b->if f(a) then Some(a) else seek(f)(b);;

let unpack x =Private.unpack_with_error_message "void is not unpackable" x;;





 


end;;






module Assistance_listennou=struct

(*

#use"Listy/listennou.ml";;

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
|a::peurrest->tempf(peurrest,graet@(Assistance_image.image(function y->a::y)(graet)))
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
    let temp3=Assistance_image.image (fun ((i1,j1),(i2,j2))->
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
   let temp2= Assistance_image.image (fun (x,y)->(x+1,y-1)) temp1 in 
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
   let temp1=Assistance_int_range.index_everything(l) in 
   let (temp2,temp3)=List.partition (fun (j,_)->(j mod 2)=1) temp1 in 
   (Assistance_image.image snd temp2,Assistance_image.image snd temp3);;

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
   Assistance_int_range.scale (fun j->
      (List.nth l (2*j-2),List.nth l (2*j-1)) 
   ) 1 m2;;

let remove_initial_contaminated_elements contamination_test all_elts =
      let rec tempf =(
         fun (beginning,l)-> match l with 
         [] -> (beginning,[])
         |a :: b -> if  contamination_test a 
                    then tempf (a::beginning,b) 
                    else (beginning,l)
      ) in 
      tempf ([],all_elts) ;;

(*

remove_initial_contaminated_elements (fun x->x<=100) [2;3;507;1;4;30];;

*)

let start_separating is_sep is_not_sep elts =
     let (_,temp1) = remove_initial_contaminated_elements is_sep elts in 
     remove_initial_contaminated_elements is_not_sep temp1;; 
      
let separate_according_to elts separators =      
   let is_sep  = (fun x->List.mem x separators) 
   and is_not_sep = (fun x->not(List.mem x separators))  in 
   let rec tempf = (fun (treated,to_be_treated)-> 
       if to_be_treated=[]
       then List.rev treated 
      else let (half1,half2)= start_separating is_sep is_not_sep to_be_treated in 
           let treated2 =(
                if half1=[] 
                then treated 
                else (List.rev half1)::treated 
           ) in 
           tempf(treated2,half2)          
   ) in 
   tempf([],elts);;

(*

separate_according_to  [1;2;3;0;4;0;0;5;6;0;0;0;7;0;8;0] [0];;
separate_according_to  [0;0;1;2;3;0;4;0;0;5;6;0;0;0;7;0;8;0] [0];;
*)


let partition_according_to_fst pairs=
  let rec tempf = (fun (already_treated,to_be_treated)->
       match to_be_treated with 
        [] -> List.rev already_treated 
       |(a0,_) :: _ ->
         let (part1,part2) = List.partition (fun (a,b)->a=a0) to_be_treated in 
         tempf ((a0,Assistance_image.image snd part1)::already_treated,part2)     
   ) in 
   tempf ([],pairs) ;;


let replace_if_possible l x=
  match List.assoc_opt x l with 
  None -> x 
  |Some y -> y ;;

let complement_of_singleton l k = 
     let temp1 = Assistance_int_range.index_everything l in 
     Assistance_option.filter_and_unpack (fun (j,x)->if j=k then None else Some x) temp1 ;;

(* complement_of_singleton (Ennig.ennig 1 7) 3 ;; *)

let minimal_element_in_unpwards_filter f l =
     let rec tempf = (
        fun (treated,to_be_treated) -> match to_be_treated with 
          [] -> List.rev treated 
          | x :: others ->
          if f(List.rev_append treated others)
          then tempf(treated,others)
          else tempf(x::treated,others)
     ) in 
     tempf([],l) ;;

(*  (minimal_element_in_unpwards_filter (fun x->Basic.fold_sum(x)>=10) [6;2;5;1;1]) = [6;5] ;; *)

let cut_into_small_parts  l ~max_part_size =
  let rec tempf = (
      fun (treated,to_be_treated,remaining_size) -> 
           if remaining_size <= max_part_size 
           then List.rev(to_be_treated::treated) 
           else let (reversed_left,right) = big_rht max_part_size to_be_treated in 
                let left = List.rev reversed_left in 
                tempf(left::treated,right,remaining_size-max_part_size)
  ) in 
  tempf ([],l,List.length l) ;;

(* cut_into_small_parts (Ennig.ennig 1 7) ~max_part_size:3 ;; *)

let project l indices = Assistance_image.image (fun k->List.nth l (k-1)) indices ;;

(* project  ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"] [2;3;7] ;; *)

let insert_two_elements_at_indices l (elt1,elt2) (idx1,idx2) = 
  let (part1,temp1) = big_rht (idx1-1) l in 
  let (part2,part3) = big_rht (idx2-idx1) temp1 in 
  List.rev_append part1  (elt1 :: (List.rev_append part2  (elt2 :: part3))) ;;  
  
(* insert_two_elements_at_indices [1; 2; 3; 4; 5; 6] (25,35) (3,4) ;;  *)


let extend_total_ordering_by_adding_two_elements old_total_order elt1 elt2 = 
  let n = (List.length old_total_order)+1 in 
  Assistance_image.image (
   insert_two_elements_at_indices old_total_order (elt1,elt2)
  ) (Assistance_int_uple.inclusive_list_of_pairs n) ;; 


(* extend_total_ordering_by_adding_two_elements  [1; 2; 3; 4; 5; 6] 25 35 ;; *)



end;;






module Assistance_supstring=struct

(*

#use"supstring.ml";;

*)



let begins_with y x=
      let lx=String.length(x) in
      if String.length(y)<lx
      then false
      else (String.sub y 0 lx)=x;;  
   
 let ends_with y x=
      let lx=String.length(x) in
      if String.length(y)<lx
      then false
      else (String.sub y ((String.length y)-lx) lx)=x;;  
   
 
let contains y x=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      Assistance_int_range.exists tester 0 (String.length(y)-lx);;               

end;;






module Assistance_substring=struct

(*

#use"substring.ml";;

*)

 let is_the_beginning_of y x=Assistance_supstring.begins_with x y;;     

   
 let is_the_ending_of y x=Assistance_supstring.ends_with x y;;  

 let is_a_substring_located_at y x old_j =
    let j=old_j-1 in
    let ly=String.length(y) in
      if (String.length(x)<j+ly)||(j<0)
      then false
      else (String.sub x j ly)=y;;
 
  let is_a_substring_of x y=Assistance_supstring.contains y x;; 
      
  let leftmost_index_of_in x y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      try (Assistance_option.unpack(Assistance_int_range.find_it tester 0 (String.length(y)-lx))+1) with
      _->(-1);;
  
  let rightmost_index_of_in x y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) 
      and temp1=List.rev(Assistance_int_range.range(0)(String.length(y)-lx)) in
      try ((Assistance_listennou.force_find tester temp1)+1) with
      _->(-1);;
  
   let leftmost_index_of_in_from x y i=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      match Assistance_int_range.find_it tester (i-1) (String.length(y)-lx) with
         None->(-1)
        |Some(k)->k+1;;
  
module Friend = struct

let number_of_lines_before s i=
   if i<1 then 0 else
   let m=min i (String.length s) in
   List.length(List.filter(fun j->(String.get s (j-1))='\n')(Assistance_int_range.range 1 m));;


end;;

let leftmost_linedex_of_in x y=
    let j=leftmost_index_of_in x y in
    if j<0 then (-1) else
    Friend.number_of_lines_before y j;;



let leftmost_linedex_of_in_from x y i=
        let j=leftmost_index_of_in_from x y i in
        if j<0 then (-1) else
        Friend.number_of_lines_before y j;;    



let leftmost_index_of_pattern_among_in_from patterns whole_string start_idx=  
    let n=String.length(whole_string) in
    let temp1=Assistance_int_range.index_everything patterns in 
    let tester =(fun idx->Assistance_option.find_and_stop (
         fun (patt_nbr,patt)->
           if is_a_substring_located_at patt whole_string idx 
           then Some(patt_nbr,idx)
           else None
       ) temp1) in
    Assistance_option.find_and_stop tester (Assistance_int_range.range start_idx n);;          
      
(*

leftmost_index_of_pattern_among_in_from ["uv";"abc";"abcde"] "123abcde90" 1;;

*)

let occurrences_of_in x y=
   let n=String.length y in
   let rec tempf=(fun (j,accu)->
      if j>n then List.rev(accu) else
      let k=leftmost_index_of_in_from x y j in
      if k<0 then List.rev(accu) else
      tempf(k+1,k::accu)
   )  in
   tempf (1,[]);;

let ranges_for_occurrences_of_in x y=
   let m=String.length x in
   let temp1 = occurrences_of_in x y in 
   Assistance_image.image (fun i->(i,i+m-1)) temp1;;   

end;;






module Assistance_cull_string=struct

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
     

let before_and_after w x=
  let j=Assistance_substring.leftmost_index_of_in(w)(x) in
  if j=(-1) then None else 
   Some(  beginning (j-1) x,
    cobeginning (j+String.length(w)-1) x);;

let complement_union_of_ranges ranges s=
   let n=String.length s in 
   let temp1=Assistance_listennou.complement_union_of_ranges ranges n in 
   Assistance_image.image (fun (u,v)->interval s u v) temp1;;

let extract_intervals_in_wrt_separator s sep =
  let d=String.length(sep)-1 in 
  let occurrences = Assistance_substring.occurrences_of_in sep s in 
  let ranges = Assistance_image.image ( fun start ->(start,start + d)) occurrences in 
  complement_union_of_ranges ranges s;;    

(*
extract_intervals_in_wrt_separator "123+ab+++c+d+45+678+" "+" ;;

extract_intervals_in_wrt_separator "123a4ab56ab789ab" "ab" ;;

*)

let remove_chars_in_set_on_the_left l s=
      let n=String.length s in
      match Assistance_option.seek(fun j->
          not(List.mem (String.get s (j-1)) l)
      )(Assistance_int_range.range 1 n) with
      None->""
      |Some(d)->cobeginning (d-1) s;;

let remove_chars_in_set_on_the_right l s=
      let n=String.length s in
      match Assistance_option.seek(fun j->
          not(List.mem (String.get s (n-j)) l)
      )(Assistance_int_range.range 1 n) with
      None->""
      |Some(d)->coending (d-1) s;;

let trim_spaces_on_the_left =remove_chars_in_set_on_the_left [' ';'\t';'\r';'\n'];;

let trim_spaces_on_the_right = remove_chars_in_set_on_the_right [' ';'\t';'\r';'\n'] ;;

let trim_slashes_on_the_right =remove_chars_in_set_on_the_right ['/'];;
   
              

 let trim_spaces s=
   let n=String.length s in
   let opt1=Assistance_option.seek(fun j->not(List.mem(String.get s (j-1)) [' ';'\r';'\t';'\n']))(Assistance_int_range.range 1 n) in
   if opt1=None then "" else
   let i1=Assistance_option.unpack opt1 in
   let k1=Assistance_listennou.force_find(fun j->not(List.mem(String.get s (n-j)) [' ';'\r';'\t';'\n']))(Assistance_int_range.range 1 n) in 
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
   let temp1=List.filter(fun j->(String.get s (j-1))='\n')(Assistance_int_range.range 1 n) in
   let (temp2,temp3)=Assistance_hurried.partition_in_two_parts(fun k->k<j) temp1 in
   let a=(if List.length(temp2)<6 then 1 else List.nth(List.rev temp2)(5))
   and b=(if List.length(temp3)<6 then n else List.nth(temp3)(5)) in
   String.sub s (a-1) (b-a);;
   
exception Absent_beginning_marker of string;;
exception Absent_ending_marker of string;; 
 
let between_markers (bm,em) s=
     if (bm,em)=("","") then s else
     let i1=Assistance_substring.leftmost_index_of_in_from bm s 1  in
     if i1<1 then raise(Absent_beginning_marker(bm)) else
     let j1=i1+(String.length bm) in
     let i2=Assistance_substring.leftmost_index_of_in_from em s (j1+1) in
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

let shorten_blanks s=
   let blanks = [' ';'\n';'\r';'\t'] in 
   let n = String.length s in 
   let test_idx = (fun j->
       if j=1 then true else 
       let c = String.get s (j-1)  in 
       if not(List.mem c blanks) then true else 
       let d = String.get s (j-2)  in  
       not(List.mem d blanks) 
      ) in 
   let temp1 = List.filter test_idx (Assistance_int_range.range 1 n) in 
   let temp2 = Assistance_image.image (fun j->String.make 1 (String.get s (j-1))) temp1 in 
   let temp3 = String.concat "" temp2 in 
   trim_spaces temp3 ;;

(*

shorten_blanks " \n 123\r \n45 \n\n6\n  7\t 89\n";;

*)    

  
             

end;;






module Assistance_encoded_string_t=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/encoded_string_t.ml";;


*)


type t= E of string;;



end;;






module Assistance_concrete_object_t=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/concrete_object_t.ml";;


*)

type t= 
    Int of int 
   |String of Assistance_encoded_string_t.t 
   |Uple of t list
   |List of t list
   |Array of t list
   |Record of (string*t) list
   |Variant of string*(t list);;



end;;






module Assistance_concrete_object=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/concrete_object.ml";;

*)

module Exn = struct

   exception Get_record_absent_key_exn of string;;
   exception Get_record_bad_type_exn of Assistance_concrete_object_t.t;;
   exception Get_pair_exn of Assistance_concrete_object_t.t;;
   exception Unwrap_array_exn of Assistance_concrete_object_t.t;;
   exception Unwrap_int_exn of Assistance_concrete_object_t.t;;
   exception Unwrap_list_exn of Assistance_concrete_object_t.t;;
   exception Unwrap_string_exn of Assistance_concrete_object_t.t;;
   exception Wrap_lonely_variant_exn;;
   exception Unwrap_lonely_variant_exn of Assistance_concrete_object_t.t;;
   
   exception Uple_too_big of Assistance_concrete_object_t.t;;
   exception Uple_too_small of Assistance_concrete_object_t.t;;
   exception Unwrap_bounded_uple_exn of Assistance_concrete_object_t.t;;
   
   exception Variant_too_big of Assistance_concrete_object_t.t;;
   exception Variant_too_small of Assistance_concrete_object_t.t;;
   exception Unwrap_bounded_variant_exn of Assistance_concrete_object_t.t;;
   
   end;;
   
   let wrap_encoded_string encoded_s=Assistance_concrete_object_t.String(encoded_s);;
   
   
   let unwrap_list ccrt_obj=
      match ccrt_obj with 
      Assistance_concrete_object_t.List(l)->l 
      |_->raise(Exn.Unwrap_list_exn(ccrt_obj));;
   
   let unwrap_array ccrt_obj=
      match ccrt_obj with 
      Assistance_concrete_object_t.Array(l)->Array.of_list l 
      |_->raise(Exn.Unwrap_array_exn(ccrt_obj));;
   
   let get_record ccrt_obj field =
      match ccrt_obj with 
      Assistance_concrete_object_t.Record(l)->
           (try List.assoc field l with 
           _ ->raise(Exn.Get_record_absent_key_exn(field)))
      |_->raise(Exn.Get_record_bad_type_exn(ccrt_obj));;
   
   let unwrap_bounded_uple ccrt_obj=
     match ccrt_obj with 
      Assistance_concrete_object_t.Uple(l)->
         let n=List.length(l) in 
         if  n<2 then raise(Exn.Uple_too_small(ccrt_obj)) else 
         if  n>7 then raise(Exn.Uple_too_big(ccrt_obj)) else 
         let i3=(if n<3 then 1 else 3)
         and i4=(if n<4 then 1 else 4)
         and i5=(if n<5 then 1 else 5)
         and i6=(if n<6 then 1 else 6)
         and i7=(if n<7 then 1 else 7) in
         let get=(fun k->List.nth l (k-1)) in 
         (get 1,get 2,get i3,get i4,get i5,get i6,get i7)
      | _-> raise(Exn.Unwrap_bounded_uple_exn(ccrt_obj));;
   
   
   
   let unwrap_bounded_variant ccrt_obj=
     match ccrt_obj with 
      Assistance_concrete_object_t.Variant(constructor,l)->
         let n=List.length(l) in 
         if  n<1 then raise(Exn.Variant_too_small(ccrt_obj)) else 
         if  n>7 then raise(Exn.Variant_too_big(ccrt_obj)) else 
         let i2=(if n<2 then 1 else 2) 
         and i3=(if n<3 then 1 else 3)
         and i4=(if n<4 then 1 else 4)
         and i5=(if n<5 then 1 else 5)
         and i6=(if n<6 then 1 else 6)
         and i7=(if n<7 then 1 else 7) in
         let get=(fun k->List.nth l (k-1)) in 
         (constructor,(get 1,get i2,get i3,get i4,get i5,get i6,get i7))
      | _-> raise(Exn.Unwrap_bounded_variant_exn(ccrt_obj));;
   
   
   let wrap_lonely_variant l_pairs unwrapped=
      match Assistance_option.seek(fun (key,vaal)->key=unwrapped) l_pairs with
         None->raise(Exn.Wrap_lonely_variant_exn)
        |Some(_,constructor)->Assistance_concrete_object_t.Variant(constructor,[]) ;;
   
   
   let unwrap_lonely_variant l_pairs ccrt_obj=
      match ccrt_obj with 
      Assistance_concrete_object_t.Variant(constructor,l)->
         if  l<>[] then raise(Exn.Unwrap_lonely_variant_exn(ccrt_obj)) else 
         (match Assistance_option.seek(fun (_,key)->key=constructor) l_pairs with
         None->raise(Exn.Unwrap_lonely_variant_exn(ccrt_obj))
        |Some(vaal,_)->vaal) 
      |_->raise(Exn.Unwrap_lonely_variant_exn(ccrt_obj));;
   

end;;






module Assistance_tools_for_absolute_path=struct

(*

#use"tools_for_absolute_path.ml";;

Standardize filename path. Non-directories never 
end with /, directories always do (exceptions : the iterated_container 
functions below, note that 
Sys.getcwd() does not follow this convention ).


*)

let remove_trailing_slash s=
    let n=String.length(s) in
    if ((String.get s (n-1))='/')
    then String.sub s 0 (n-1)
    else s;;

exception Number_of_double_points_exn;;

let number_of_double_points s=
  let n=String.length(s) in
  let rec tempf=(fun j->
     let k=(3*j) in
     if (n<k+2) then j else
     if (String.get s k='.')&&(String.get s (k+1)='.')
     then if n=(k+2) then j+1 else
          if (String.get s (k+2)='/') 
          then tempf(j+1)
          else raise(Number_of_double_points_exn)
     else j
  ) in
  tempf(0);;
  
  
let helper_for_iterated_container j0 s=
   let rec tempf=(fun (j,k)->
     if j<1 then (String.sub s 0 k) else
     let i=String.rindex_from(s)(k-1)('/') in
     tempf(j-1,i)
     ) in
    tempf (j0,String.length s);;
 
exception Too_many_double_points;;  
 
 let iterated_container j0 s=try helper_for_iterated_container j0 s with
   any_exn->raise(Too_many_double_points);;

exception Blank_filename;;

let delete_left_blanks s=
  let n=String.length(s) in
  let rec tempf=(fun j->
    if j>=n then raise(Blank_filename) else
    if String.get(s)(j)=' '
    then tempf(j+1)
    else j
  ) in
  let j0=tempf 0 in
  String.sub s j0 (n-j0);;

let parse_unix_filename_shortcuts_from_dir dir s0=
  let dir_without_the_slash = remove_trailing_slash dir in  
  let s1=delete_left_blanks(s0) in
  let dp1=number_of_double_points(s1) in
  if (dp1>0) 
  then  let smaller_pwd=iterated_container dp1 dir_without_the_slash in
        let j1=(3*dp1)-1 in 
         smaller_pwd^(String.sub s1 j1 ((String.length s1)-j1) )    
  else
  if s1="/" then "/" else
  match String.get(s1)(0) with
  '/'->s1
  |'~'->(Sys.getenv "HOME")^(String.sub s1 1 (String.length(s1)-1))
  |'.'->if s1="." 
        then dir_without_the_slash
        else dir^(String.sub s1 2 (String.length(s1)-2))
  |arall->dir^s1;;

let parse_unix_filename_shortcuts =
  parse_unix_filename_shortcuts_from_dir ((Sys.getcwd())^"/");;
  
 
  
 exception Inexistent_file of string;; 
  
 let opt_of_string s=
  let s0=parse_unix_filename_shortcuts(s) in
  if Sys.file_exists(s0)
  then if s0="/" then Some s0 else
       let s1=remove_trailing_slash s0 in
       if Sys.is_directory s1
       then Some(s1^"/")
       else Some s1
  else None;;
  
 let of_string s=
   match opt_of_string s with 
   Some result -> result  
   |None -> raise(Inexistent_file(s));; 



end;;






module Assistance_absolute_path=struct

(*

#use"absolute_path.ml";;

*)

type t=AP of string;;

let of_string s=AP(Assistance_tools_for_absolute_path.of_string s);;


let to_string (AP s)=s;;

let ocaml_name ap=
 let s=to_string ap in
"Absolute"^"_path"^"."^"of_string(\""^s^"\"";;

let test_equal_paths s1 s2=
((of_string s1)=(of_string s2));;

exception Error_during_file_creation;;
exception Error_during_unix_command of string;;

let uc cmd = 
   let i= Sys.command cmd in 
   if i<>0 then raise(Error_during_unix_command cmd) else ();;


let create_file_if_absent w=
    let cr=(fun w->
      let ld=Unix.openfile w [Unix.O_RDWR;Unix.O_CREAT;Unix.O_EXCL] 0o666 in
       Unix.close ld
    ) in
    if Sys.file_exists w then of_string w else
    if (not(String.contains w '/'))
    then (cr w;of_string w)
    else 
    let i=String.rindex w '/' in
    let basedir=String.sub w 0 i
    and filename=String.sub w (i+1) ((String.length w)-(i+1)) in
    let g1="jnoxgghg_"^filename in
    let _=(uc ("mkdir -p "^basedir);
           uc ("touch "^g1); 
           uc ("mv "^g1^" "^w);
           uc ("rm -f "^g1)) in 
    of_string w;;
    
let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;

           

end;;






module Assistance_io=struct

(*

#use"io.ml";;

*)


exception Open_in_exn of string ;;
exception Open_out_exn of string ;;
exception Dangerous_command_reading of string ;;

let max_size_for_reasonable_in_channel = ref(1000000);;

module Private = struct 

let make_filename_complete s=
  let home=Sys.getenv("HOME") in
  if s="" then Assistance_absolute_path.of_string(home) else
  let c=String.get s 0 in
  if c='/' then Assistance_absolute_path.of_string(s) else
  if c='~' then Assistance_absolute_path.of_string(home^(String.sub s 1 (String.length(s)-1))) else
  Assistance_absolute_path.of_string((Sys.getcwd ())^"/"^s);;

let open_in_locally x=try open_in_bin(x) with 
_->raise(Open_in_exn(x));;

let open_out_locally x=try open_out_bin(x) with 
_->raise(Open_out_exn(x));;  

let put_whole_content_of_file_in_buffer s=
  let x=Assistance_absolute_path.to_string(make_filename_complete(s)) in
  let janet=open_in_locally(x) in
  let n=in_channel_length(janet) in
  let b=Buffer.create(n) in
  let _=Buffer.add_channel(b)(janet)(n) in
  let _=close_in janet in
  b;;
  
  
type filename=string;;
  
let erase_file_and_fill_it_with_contents_of_buffer (fn:filename) b=
   let x=Assistance_absolute_path.to_string(make_filename_complete(fn)) in
  let john=open_out_locally(x) in
  (Buffer.output_buffer(john)(b);close_out john);;
  
let overwrite_with ap s=
   let fn=Assistance_absolute_path.to_string ap in
   let n=String.length(s) in
   let b=Buffer.create(n) in
   let _=Buffer.add_string b s in
   erase_file_and_fill_it_with_contents_of_buffer fn b;;
   
let read_whole_file ap=   
   let s=Assistance_absolute_path.to_string ap in
   let b=put_whole_content_of_file_in_buffer(s) in
   Buffer.contents b;;

let append_string_to_file s ap=
  let new_content=(read_whole_file ap)^s in
  overwrite_with ap new_content;; 
   
let read_reasonable_command cmd =
   let chan = Unix.open_process_in cmd in 
   let max_reasonable_size = (!max_size_for_reasonable_in_channel)+1 in 
   let buf = Bytes.create max_reasonable_size  in 
   let final_size = input chan buf 0 max_reasonable_size  in 
   let _ = Unix.close_process_in chan in 
   if (final_size=0)||(final_size >= max_reasonable_size) 
   then raise(Dangerous_command_reading(cmd))
   else 
   Bytes.sub_string buf 0 final_size ;;

end ;; 

let append_string_to_file = Private.append_string_to_file ;;
let overwrite_with = Private.overwrite_with ;;
let read_reasonable_command = Private.read_reasonable_command ;;
let read_whole_file = Private.read_whole_file ;;
   
  
             

end;;






module Assistance_overwriter=struct

(*

#use"overwriter.ml";;

*)


type t=Ovw of string;;

let of_string s=Ovw(s);;
let to_string (Ovw s)=s;;
             

end;;






module Assistance_replace_inside=struct

(*

#use"replace_inside.ml";;

*)

exception Ambiguity of string*int*int;;

module Private = struct 

(*

The my_global_replace function below is a replacement for Ocaml's Str.global_replace which has
the disadvantage of applying certain transforms to the replacement string.

*)

  let single_char_special_case (single_c,b) s=
  let n=String.length(s) and counter=ref(0) in
  let temp1=Assistance_int_range.scale (
     fun j->let c=String.get s j in
            if c=single_c
            then let _=(counter:=(!counter)+1) in b
            else String.make 1 c 
  ) 0 (n-1) in
  (String.concat "" temp1,!counter);;

let global_replace_with_number_of_matches (a,b) s=
let n=String.length(s) and na=String.length(a) in
if na=1 then single_char_special_case (String.get a 0,b) s else
let indices=Assistance_substring.occurrences_of_in a s in
if indices=[] then (s,0) else
let delta_indices = Assistance_listennou.universal_delta_list indices in 
let opt_ambiguity=Assistance_option.seek (fun (start1,start2)->start2<start1+na) delta_indices in 
if  opt_ambiguity<>None
then let (start1,start2)=Assistance_option.unpack opt_ambiguity in 
     raise(Ambiguity(a,start1,start2))
else  
let m=List.length indices in 
let lower_end=(fun j->if j=0 then 1 else List.nth indices (j-1)+na) 
and upper_end=(fun j->if j=m then n else (List.nth indices (j))-1) in 
let unchanged_intervals = Assistance_int_range.scale (fun j->(lower_end j,upper_end j)) 0 m in 
let unchanged_substrings=Assistance_image.image 
   (fun (x,y)->if x>y then "" else Assistance_cull_string.interval s x y) unchanged_intervals in
(String.concat b unchanged_substrings,m);;

let text_for_number_of_replacements k=
  if k = 0 then "No replacement made" else
  if k = 1 then "1 replacement made" else 
  (string_of_int k)^" replacements made" ;;   

let my_global_replace ?(display_number_of_matches=true) (a,b) old_s  =
   let (new_s,count) = global_replace_with_number_of_matches (a,b) old_s in 
   let _ =(
      if display_number_of_matches 
      then print_string("\n"^(text_for_number_of_replacements count)^" for "^a^" -> "^b^"\n"); 
           flush stdout 
   ) in 
   new_s ;; 

(*  
my_global_replace ("ab","cd") "12345ab6ab78cd91234ab679";; 
my_global_replace ("ab","cd") "ab12345ab6ab78cd91234ab679";; 
my_global_replace ("ab","cd") "12345ab6ab78cd91234ab679ab";;
my_global_replace ("1111","") "abc1111111111def";;
my_global_replace ("ab","cd") "xyz";;
my_global_replace ("a","b") "1aa2";; 
my_global_replace ("uv","w") "1uvuv2";; 

*)  

end ;;



let replace_inside_string (a,b) s=
  Private.my_global_replace (a,b) s ~display_number_of_matches:true;;
 
let silently_replace_inside_string (a,b) s=
  Private.my_global_replace (a,b) s ~display_number_of_matches:false;;

let replace_several_inside_string l t=List.fold_left 
(fun s (a,b)->Private.my_global_replace (a,b) s  ~display_number_of_matches:false) t l;;  
 
let replace_inside_file (a,b) fn=
    let s1=Assistance_io.read_whole_file fn in
    let la=String.length(a) in
    if List.exists (fun j->(String.sub s1 j la)=a) (Assistance_int_range.range 0 ((String.length s1)-la))
    then let s2=replace_inside_string (a,b) s1 in
         Assistance_io.overwrite_with fn s2
    else ();; 
    
let silently_replace_inside_file (a,b) fn=
    let s1=Assistance_io.read_whole_file fn in
    let la=String.length(a) in
    if List.exists (fun j->(String.sub s1 j la)=a) (Assistance_int_range.range 0 ((String.length s1)-la))
    then let s2=silently_replace_inside_string (a,b) s1 in
         Assistance_io.overwrite_with fn s2
    else ();; 


let replace_several_inside_file l fn=
    let s1=Assistance_io.read_whole_file fn in
    let s2=replace_several_inside_string l s1  in
    Assistance_io.overwrite_with fn s2;; 

exception Absent_beginning_marker of string;;
exception Absent_ending_marker of string;; 
 
let overwrite_between_markers_inside_string ovw_b (bm,em)
   s1=
     let b=Assistance_overwriter.to_string ovw_b in
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
    let s1=Assistance_io.read_whole_file fn in
    let s2=overwrite_between_markers_inside_string ovw_b (bm,em) s1 in
    Assistance_io.overwrite_with fn s2;;      


let overwrite_and_dump_markers_inside_string ovw_b (bm,em)
   s1=
     let b=Assistance_overwriter.to_string ovw_b in
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
    let s1=Assistance_io.read_whole_file fn in
    let s2=overwrite_and_dump_markers_inside_string ovw_b (bm,em) s1 in
    Assistance_io.overwrite_with fn s2;;      
 
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

let at_char_intervals_inside_string s l=
  if l=[] then s else
  let n=String.length s in
  let temp1=Assistance_listennou.universal_delta_list l 
  and ((i_first,_),_)=List.hd(l)
  and ((i_last,j_last),rep_last)=List.hd(List.rev l) in
  let temp2=Assistance_image.image (fun (((i1,j1),rep1),((i2,j2),rep2))->
      rep1^(String.sub s j1 (i2-j1-1))
  ) temp1 in
  let first_part=(String.sub s 0 (i_first-1))
  and last_part=rep_last^(String.sub s j_last (n-j_last)) in
  first_part^(String.concat "" temp2)^last_part;;

let at_char_intervals_inside_file 
  fn l=
   let s1=Assistance_io.read_whole_file fn in
   let s2=at_char_intervals_inside_string s1 l in
   Assistance_io.overwrite_with fn s2;;     

(*    

at_char_intervals_inside_string "12345678901234567890" [(3,5),"right";(12,17),"again"];;

*)         






end;;






module Assistance_encoded_string=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/encoded_string.ml";;

This is a leaky abstraction, as witnessed by the "retrieve" and "store" functions above.


*)

exception Forbidden_substring;;
exception Forbidden_recombination;;

let salt = 
  String.concat "" ["a"; "Y"; "2"; "u"; "k"; "k"; "w"; "D"; "z"; "y"; "K"; "d"];;
let replacement_salt = 
  String.concat "" ["b"; "Z"; "3"; "v"; "l"; "m"; "x"; "E"; "A"; "z"; "L"; "e"];;

let decode (Assistance_encoded_string_t.E(encoded_s))=
   Assistance_replace_inside.silently_replace_inside_string (replacement_salt,salt) encoded_s;;

let encode s=
   if Assistance_substring.is_a_substring_of replacement_salt s 
   then raise(Forbidden_substring)
   else let encoded_s=Assistance_replace_inside.silently_replace_inside_string (salt,replacement_salt) s in 
        if  Assistance_substring.is_a_substring_of salt encoded_s 
        then raise(Forbidden_substring)
        else Assistance_encoded_string_t.E(encoded_s);;

let retrieve encoded_s = (Assistance_encoded_string_t.E(encoded_s));;
let store (Assistance_encoded_string_t.E(encoded_s))= encoded_s;;


end;;






module Assistance_crobj_converter=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_converter.ml";;

*)



exception Unwrap_int_exn of Assistance_concrete_object_t.t;;
exception Unwrap_string_exn of Assistance_concrete_object_t.t;;


let bool_of_concrete_object = Assistance_concrete_object.unwrap_lonely_variant [true,"True";false,"False"];;
let bool_to_concrete_object bowl = 
    if bowl 
    then Assistance_concrete_object_t.Variant("True",[]) 
    else Assistance_concrete_object_t.Variant("False",[]);;  

let int_of_concrete_object ccrt_obj =
    match ccrt_obj with 
     Assistance_concrete_object_t.Int(i)->i 
    |_->raise(Unwrap_int_exn(ccrt_obj)) ;;
let int_to_concrete_object i = Assistance_concrete_object_t.Int i ;;       

let string_of_concrete_object ccrt_obj =
       match ccrt_obj with 
       Assistance_concrete_object_t.String(encoded_s)->Assistance_encoded_string.decode encoded_s 
       |_->raise(Unwrap_string_exn(ccrt_obj)) ;;
let string_to_concrete_object s = Assistance_concrete_object_t.String(Assistance_encoded_string.encode s) ;;       

end;;






module Assistance_dfa_root_t=struct

(*

Does not end with a slash.

#use"Decomposed_filename/dfa_root_t.ml";;

*)

type t=R of string;;           

end;;






module Assistance_dfa_root=struct

(*

The rightmost trailing slash is removed.

#use"Decomposed_filename/dfa_root.ml";;

*)

let without_trailing_slash (Assistance_dfa_root_t.R s)=s;;
let connectable_to_subpath (Assistance_dfa_root_t.R s)=s^"/";;  
let dummy = Assistance_dfa_root_t.R "" ;;

let of_line line = Assistance_dfa_root_t.R(Assistance_tools_for_absolute_path.remove_trailing_slash line);;


let to_concrete_object (Assistance_dfa_root_t.R(line))=
    Assistance_concrete_object_t.Variant("Dfa_"^"root.R",[Assistance_crobj_converter.string_to_concrete_object(line)]);;

let of_concrete_object ccrt_obj =
   let (_,(arg1,_,_,_,_,_,_))=Assistance_concrete_object.unwrap_bounded_variant ccrt_obj in 
   Assistance_dfa_root_t.R(Assistance_crobj_converter.string_of_concrete_object arg1);;



end;;






module Assistance_crobj_converter_combinator=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_converter_combinator.ml";;

*)

let of_array of_a arr= Assistance_concrete_object_t.Array(Array.to_list(Array.map of_a arr));;
let to_array to_a crobj= Array.map to_a (Assistance_concrete_object.unwrap_array crobj);;

let of_list of_a l= Assistance_concrete_object_t.List(Assistance_image.image of_a l);;
let to_list to_a crobj= Assistance_image.image to_a (Assistance_concrete_object.unwrap_list crobj);;
   
   
let of_pair of_a of_b (a,b)=Assistance_concrete_object_t.Uple[of_a a;of_b b];;
let to_pair to_a to_b crobj=
       let (arg1,arg2,_,_,_,_,_)=Assistance_concrete_object.unwrap_bounded_uple crobj in
       (to_a arg1,to_b arg2);;
   
let of_triple of_a of_b of_c (a,b,c)=Assistance_concrete_object_t.Uple[of_a a;of_b b;of_c c];;
let to_triple to_a to_b to_c crobj=
           let (arg1,arg2,arg3,_,_,_,_)=Assistance_concrete_object.unwrap_bounded_uple crobj in
           (to_a arg1,to_b arg2,to_c arg3);;
   

let of_pair_list of_a of_b l=of_list (of_pair of_a of_b) l;;
let to_pair_list to_a to_b crobj = to_list (to_pair to_a to_b) crobj;;
   
   


end;;






module Assistance_dfa_ending_t=struct

(*

#use"Decomposed_filename/dfa_ending_t.ml";;

Does not contain a dot.

*)

type t=E of string;;



end;;






module Assistance_dfa_ending=struct

(*

#use"Decomposed_filename/dfa_ending.ml";;

*)



exception Dot_inside_ending of string;;
exception Not_an_ocaml_ending of string;;
exception Unknown_ending of Assistance_dfa_ending_t.t ;;

let of_line e =
  if String.contains e '.'
  then raise(Dot_inside_ending(e))
  else Assistance_dfa_ending_t.E(e);;

let connectable_to_modulename (Assistance_dfa_ending_t.E(e)) = "." ^ e ;;



let mll =  Assistance_dfa_ending_t.E "mll"
and mly =  Assistance_dfa_ending_t.E "mly"
and ml  =  Assistance_dfa_ending_t.E "ml"
and mli =  Assistance_dfa_ending_t.E "mli" ;; 

let all_ocaml_endings= [mll;mly;ml;mli];;

let all_cee_endings = Assistance_image.image (fun s->Assistance_dfa_ending_t.E s) ["h";"c"];;

let compute_on_all_ocaml_endings f=(f ml,f mli,f mll,f mly);;

let endings_for_compilable_files = 
   (all_ocaml_endings) @ all_cee_endings ;;

let endings_for_noncompilable_readable_files = 
     Assistance_image.image (fun s->Assistance_dfa_ending_t.E s) ["txt";"html";"php";"js";"ejs";"json"];; 

let endings_for_readable_files = 
     endings_for_compilable_files @ endings_for_noncompilable_readable_files ;;

let is_compilable edg =
   if List.mem edg endings_for_compilable_files 
   then true 
   else 
   if List.mem edg endings_for_noncompilable_readable_files 
   then false 
   else raise(Unknown_ending(edg));;



let to_concrete_object (Assistance_dfa_ending_t.E(e)) =
    Assistance_concrete_object_t.Variant ("Dfa_"^"ending_t.E",
    [Assistance_crobj_converter.string_to_concrete_object(e)]);;

let of_concrete_object crobj =
   let (_,(arg1,_,_,_,_,_,_))=Assistance_concrete_object.unwrap_bounded_variant crobj in 
   Assistance_dfa_ending_t.E(
      Assistance_crobj_converter.string_of_concrete_object arg1
   );;



end;;






module Assistance_dfa_module_t=struct

(*

#use"Decomposed_filename/dfa_module_t.ml";;

A module name, or a candidate for one. Uncapitalized. 
Should contain no slashes.

*)

type t=M of string;;

           

end;;






module Assistance_dfa_module=struct

(*

#use"Decomposed_filename/dfa_module.ml";;

A module name, or a candidate for one. Uncapitalized. Should contain no slashes.

*)


let of_line s=Assistance_dfa_module_t.M (String.uncapitalize_ascii s);; 
let to_line (Assistance_dfa_module_t.M s)=s;;

let add_prefix_and_capitalize prefix (Assistance_dfa_module_t.M name)=
  Assistance_dfa_module_t.M(String.capitalize_ascii(prefix^name));;

  
let capitalized_form (Assistance_dfa_module_t.M name)= String.capitalize_ascii name;;

let to_concrete_object (Assistance_dfa_module_t.M(s))=
    Assistance_concrete_object_t.Variant("Dfa_"^"module_t.M",
     [Assistance_crobj_converter.string_to_concrete_object(s)]);;

let of_concrete_object ccrt_obj =
   let (_,(arg1,_,_,_,_,_,_))=Assistance_concrete_object.unwrap_bounded_variant ccrt_obj in 
   Assistance_dfa_module_t.M(Assistance_crobj_converter.string_of_concrete_object arg1);;


end;;






module Assistance_dfa_subdirectory_t=struct

(*

Subdirectories name, with the trailing slash removed.

#use"Decomposed_filename/dfa_subdirectory_t.ml";;

*)

type t=SD of string;;



end;;






module Assistance_max=struct

(*

#use"max.ml";;

*) 


let list=function 
[]->failwith("max of empty set undefined according to Garfield")
|a::b->List.fold_left(max)(a)(b);;

let maximize_it f=function
[]->failwith("max on empty set undefined")
|x::y->
 let rec maximize_it0=(function
  (current_candidate,current_value,da_ober)->match da_ober with
  []->(current_candidate,current_value)
  |a::peurrest->let va=f(a) in
                if (va>current_value)
				then maximize_it0(a,va,peurrest)
				else maximize_it0(current_candidate,current_value,peurrest)
 ) 
in
 maximize_it0(x,f(x),y);;
 
let maximize_it_if_possible f l=
   let temp1=Assistance_option.filter_and_unpack (function 
     None->None
    |Some(x)->Some(x,f x) ) l in
   if temp1=[]
   then None
   else Some(fst(maximize_it(snd) temp1));;
 

let maximize_it_with_care f=function
[]->failwith("careful max on empty set undefined")
|x::y->
 let rec maximize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                if (va>current_value)
				then maximize_it_with_care0([a],va,peurrest)
				else if (va=current_value)
				     then maximize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else maximize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 maximize_it_with_care0([x],f(x),y);;
           

end;;






module Assistance_min=struct

(*

#use"min.ml";;

*) 


let list=function 
[]->failwith("min of empty set undefined")
|a::b->List.fold_left(min)(a)(b);;

let minimize_it f=function
[]->failwith("min on empty set undefined")
|x::y->
 let rec minimize_it0=(function
  (current_candidate,current_value,da_ober)->match da_ober with
  []->(current_candidate,current_value)
  |a::peurrest->let va=f(a) in
                if (va<current_value)
				then minimize_it0(a,va,peurrest)
				else minimize_it0(current_candidate,current_value,peurrest)
 ) 
in
 minimize_it0(x,f(x),y);;

let minimize_it_if_possible f l=
   let temp1=Assistance_option.filter_and_unpack (function 
     None->None
    |Some(x)->Some(x,f x) ) l in
   if temp1=[]
   then None
   else Some(fst(minimize_it(snd) temp1));;

let minimize_it_with_care f=function
[]->failwith("careful min on empty set undefined")
|x::y->
 let rec minimize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                if (va<current_value)
				then minimize_it_with_care0([a],va,peurrest)
				else if (va=current_value)
				     then minimize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else minimize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 minimize_it_with_care0([x],f(x),y);;
           

end;;






module Assistance_total_ordering_result_t=struct

(*

#use"Ordered_Lists/total_ordering_result_t.ml";;

*)

type t =
   Lower 
 | Equal 
 | Greater ;;


 
           

end;;






module Assistance_total_ordering_t=struct

(*

#use"Ordered_Lists/total_ordering_t.ml";;

*)



type 'a t = ( 'a -> 'a ->Assistance_total_ordering_result_t.t ) ;;



end;;






module Assistance_ordered=struct

(*
 
#use"ordered.ml";;

*)

module Private = struct 

  let intersect (cmpr:'a Assistance_total_ordering_t.t) ox oy=
      let rec tempf=(function (u,v,accu)->
        if u=[] then (List.rev(accu)) else
        if v=[] then (List.rev(accu)) else
        let xu=List.hd(u) and yu=List.tl(u) 
        and xv=List.hd(v) and yv=List.tl(v) in
        match cmpr(xu)(xv) with
         Assistance_total_ordering_result_t.Lower->tempf(yu,v,accu)
        |Assistance_total_ordering_result_t.Equal->tempf(yu,yv,xu::accu)
        |Assistance_total_ordering_result_t.Greater->tempf(u,yv,accu)
      ) in
      tempf(ox,oy,[]);;
  
  let is_increasing (cmpr:'a Assistance_total_ordering_t.t) l=
    if List.length(l)<2 then true else
    let rec tempf=(function
    (a,to_be_treated)->match to_be_treated with
     []->true
     |b::others->if (cmpr(a)(b)=Assistance_total_ordering_result_t.Lower)
                   then tempf(b,others)
                   else false
    ) in
    tempf(List.hd l,List.tl l);;
    
  
  let merge (cmpr:'a Assistance_total_ordering_t.t) ox oy=
      let rec tempf=(function (u,v,accu)->
        if u=[] then (List.rev_append(accu)(v)) else
        if v=[] then (List.rev_append(accu)(u)) else
        let xu=List.hd(u) and yu=List.tl(u) 
        and xv=List.hd(v) and yv=List.tl(v) in
      match cmpr(xu)(xv) with
        Assistance_total_ordering_result_t.Lower->tempf(yu,v,xu::accu)
      |Assistance_total_ordering_result_t.Equal->tempf(yu,yv,xu::accu)
      |Assistance_total_ordering_result_t.Greater->tempf(u,yv,xv::accu)
      ) in
      tempf(ox,oy,[]);;
  
  
  let setminus (cmpr:'a Assistance_total_ordering_t.t) ox oy=
      let rec tempf=
      (function (u,v,accu)->
        if u=[] then (List.rev(accu)) else
        if v=[] then (List.rev_append(accu)(u)) else
        let xu=List.hd(u) and yu=List.tl(u) 
        and xv=List.hd(v) and yv=List.tl(v) in
        match cmpr(xu)(xv) with
           Assistance_total_ordering_result_t.Lower->tempf(yu,v,xu::accu)
          |Assistance_total_ordering_result_t.Equal->tempf(yu,yv,accu)
          |Assistance_total_ordering_result_t.Greater->tempf(u,yv,accu)
     ) in
     tempf(ox,oy,[]);;
  
  let rec sort (cmpr:'a Assistance_total_ordering_t.t) x=
    if List.length(x)<2
    then x
    else let temp1=Assistance_listennou.split_list_in_half(x) in
         let y1=sort(cmpr)(fst temp1)
         and y2=sort(cmpr)(snd temp1) in
         merge cmpr y1 y2;;
  
  let is_included_in (cmpr:'a Assistance_total_ordering_t.t) ox oy=
         let rec tempf=(function (u,v)->
           if u=[] then true else
           if v=[] then false else
           let xu=List.hd(u) and yu=List.tl(u) 
           and xv=List.hd(v) and yv=List.tl(v) in
           match cmpr(xu)(xv) with
             Assistance_total_ordering_result_t.Lower->false
           |Assistance_total_ordering_result_t.Equal->tempf(yu,yv)
           |Assistance_total_ordering_result_t.Greater->tempf(u,yv)
         ) in
         tempf(ox,oy);;       
  
  let helper1_for_minimal_elements_selection (cmpr:'a Assistance_total_ordering_t.t)  
         comparator =
         let rec tempf = (fun
         (treated,to_be_treated) -> match to_be_treated with 
         [] -> (None,List.rev treated) 
        |new_item :: others ->
           if new_item = comparator 
           then (* ignore and continue *) tempf (treated,others)
           else       
           if is_included_in cmpr new_item comparator
           then (* finish *)  (Some new_item,[])
           else 
           if is_included_in cmpr comparator new_item
           then tempf (treated,others)
           else tempf (new_item::treated,others)  
         ) in tempf ;;    
       
  let rec helper2_for_minimal_elements_selection (cmpr:'a Assistance_total_ordering_t.t)  =
          let rec tempf = (fun 
         (treated,to_be_treated) -> match to_be_treated with 
          [] -> List.rev treated 
         |new_item :: others ->
           let (opt,checked_subset) = 
             helper1_for_minimal_elements_selection cmpr new_item ([],others) in 
           if opt<>None 
           then tempf(treated,others) 
           else tempf(new_item::treated,checked_subset)) in   
         tempf ;;
           
  let select_minimal_elements_for_inclusion tr ll=
         helper2_for_minimal_elements_selection tr ([],ll) ;;
    
  
  end;;
  
  
  let diff (cmpr: 'a Assistance_total_ordering_t.t) =
            let rec tempf=(fun
              (treated_bc,treated_b,treated_c,to_be_treated1,to_be_treated2)->
                match to_be_treated1 with
                []->(treated_bc,treated_b,List.rev_append treated_c to_be_treated2)
                |(a1,b1)::others1->
                (
                  match to_be_treated2 with
                []->(treated_bc,List.rev_append treated_b to_be_treated1,treated_c)     
                |(a2,c2)::others2->
                  (
                    match cmpr a1 a2 with
                    Assistance_total_ordering_result_t.Lower->
                      tempf(treated_bc,(a1,b1)::treated_b,treated_c,others1,to_be_treated2)
                    |Assistance_total_ordering_result_t.Greater->
                    tempf(treated_bc,treated_b,(a2,c2)::treated_c,to_be_treated1,others2)
                    |Assistance_total_ordering_result_t.Equal->
                    tempf((a1,b1,c2)::treated_bc,treated_b,treated_c,others1,others2)  
                  )
                )      
            ) in
            tempf;;   
  
  let does_not_intersect (cmpr:'a Assistance_total_ordering_t.t) ox oy=
      let rec tempf=(function (u,v)->
          if (u=[])||(v=[]) then true else
          let xu=List.hd(u) and yu=List.tl(u) 
          and xv=List.hd(v) and yv=List.tl(v) in
          match cmpr(xu)(xv) with
            Assistance_total_ordering_result_t.Lower->tempf(yu,v)
          |Assistance_total_ordering_result_t.Equal->false
          |Assistance_total_ordering_result_t.Greater->tempf(u,yv)
      ) in
      tempf(ox,oy);;
  
  exception Empty_intersection_undefined;;    
  
  let fold_intersect cmpr=function
     []->raise(Empty_intersection_undefined)
    |a::b->List.fold_left(Private.intersect cmpr)(a)(b);;
  
  let fold_merge cmpr l=
     let rec tempf=(function
        (already_treated,to_be_treated)->match to_be_treated with 
        []->already_treated
        |a::b->tempf(Private.merge cmpr a already_treated,b)
     ) in 
     tempf([],l);;    
  
  let insert cmpr x oy=Private.merge cmpr [x] oy;; 
  
  let intersect = Private.intersect;;
  
  let intersects cmpr ox oy = not(does_not_intersect cmpr ox oy);;
  
  let is_included_in = Private.is_included_in ;;
  
  let rec mem (cmpr:'a Assistance_total_ordering_t.t) x ol=
     let rec tempf=(function
      []->false
      |a::others->match cmpr(x)(a) with
         Assistance_total_ordering_result_t.Lower->false
         |Assistance_total_ordering_result_t.Equal->true
         |Assistance_total_ordering_result_t.Greater->tempf others
     )  in
     tempf ol;;    
  
  let merge = Private.merge;;
  
  let outsert cmpr x oy=Private.setminus cmpr oy [x];;
  
  let safe_set cmpr ox=if Private.is_increasing(cmpr)(ox) 
                       then ox 
                       else Private.sort cmpr ox;;
  
  let select_minimal_elements_for_inclusion = Private.select_minimal_elements_for_inclusion;;
  
  let setminus = Private.setminus;;
  
  let sort = Private.sort;;

  let symmetric_difference cmpr x y =
    merge cmpr (setminus cmpr x y) (setminus cmpr y x) ;;
  
  
  

end;;






module Assistance_total_ordering=struct

(*

#use"Ordered_Lists/total_ordering.ml";;

*)


module Private = struct
let leq (computer:'a Assistance_total_ordering_t.t) x y=
   let v=computer(x)(y) in
   (v=Assistance_total_ordering_result_t.Lower)||(v=Assistance_total_ordering_result_t.Equal);;
   
 let lt (computer:'a Assistance_total_ordering_t.t) x y=(computer(x)(y)=Assistance_total_ordering_result_t.Lower);;   
 
 let geq (computer:'a Assistance_total_ordering_t.t) x y=
   let v=computer(x)(y) in
   (v=Assistance_total_ordering_result_t.Lower)||(v=Assistance_total_ordering_result_t.Equal);;
   
 let gt (computer:'a Assistance_total_ordering_t.t) x y=(computer(x)(y)=Assistance_total_ordering_result_t.Greater);;   
 
 let from_lt f=
   let temp1=(fun x y->
     if f(x)(y)
     then Assistance_total_ordering_result_t.Lower
     else if f(y)(x)
          then Assistance_total_ordering_result_t.Greater
          else Assistance_total_ordering_result_t.Equal
   ) in
   (temp1:'a Assistance_total_ordering_t.t);;
 
 let standard_completion f g=
  let answer=(fun x y->
   if f(y)(x)
   then Assistance_total_ordering_result_t.Greater
   else if f(x)(y)
        then Assistance_total_ordering_result_t.Lower
        else if g(x)(y)
             then Assistance_total_ordering_result_t.Equal
             else if x<y
                  then Assistance_total_ordering_result_t.Lower
                  else Assistance_total_ordering_result_t.Greater
  ) in
  (answer: 'a Assistance_total_ordering_t.t);;
 
 let standard=((fun x y->
    if x=y
    then Assistance_total_ordering_result_t.Equal
    else if x<y
         then Assistance_total_ordering_result_t.Lower
         else Assistance_total_ordering_result_t.Greater
 ): 'a Assistance_total_ordering_t.t);;
 
let standard2=((fun (x1,y1) (x2,y2)->
    let t1=standard x1 x2 in 
    if t1<> Assistance_total_ordering_result_t.Equal 
    then t1
    else standard y1 y2
 ): ('a * 'b) Assistance_total_ordering_t.t);;

 let completion f (g:'a Assistance_total_ordering_t.t)=
  let answer=(fun x y->
   if f(y)(x)
   then Assistance_total_ordering_result_t.Greater
   else if f(x)(y)
        then Assistance_total_ordering_result_t.Lower
         else g(x)(y)
  ) in
  (answer: 'a Assistance_total_ordering_t.t);;
 
let combine=((fun ~tried_first ~tried_second->
  (fun x y->
   let first_trial = tried_first x y in 
   if first_trial <> Assistance_total_ordering_result_t.Equal 
   then first_trial
   else tried_second x y
  ) ): 
    tried_first:('a Assistance_total_ordering_t.t) -> tried_second:('a Assistance_total_ordering_t.t) -> ('a Assistance_total_ordering_t.t)
  );;

 let product (f:'a Assistance_total_ordering_t.t) (g:'b Assistance_total_ordering_t.t)=
  ((fun (x1,y1) (x2,y2)->
     let t=f(x1)(x2) in
     if t<>Assistance_total_ordering_result_t.Equal 
     then t
     else g y1 y2
 ): ('a*'b) Assistance_total_ordering_t.t);;
 
 let triple_product (f:'a Assistance_total_ordering_t.t) (g:'b Assistance_total_ordering_t.t) (h:'c Assistance_total_ordering_t.t)=
  ((fun (x1,y1,z1) (x2,y2,z2)->
     let tx=f(x1)(x2) in
     if tx<>Assistance_total_ordering_result_t.Equal 
     then tx
     else let ty=g(y1)(y2) in
          if ty<>Assistance_total_ordering_result_t.Equal 
          then ty
          else h z1 z2
 ): ('a*'b*'c) Assistance_total_ordering_t.t);;
 
 let rec lex_compare (f:'a Assistance_total_ordering_t.t)=
  let rec tempf=(
    fun l1 l2->
     match l1 with 
     []->(if l2=[] then Assistance_total_ordering_result_t.Equal else Assistance_total_ordering_result_t.Lower)
     |a1::b1->
      (
        match l2 with 
        []->Assistance_total_ordering_result_t.Greater
        |a2::b2->
          let t=f(a1)(a2) in
           if t<>Assistance_total_ordering_result_t.Equal then t else
           tempf b1 b2
      )) in
     (tempf:>( ('a list) Assistance_total_ordering_t.t));;
 


let silex_compare (f:'a Assistance_total_ordering_t.t)=
  let tempf=(
    fun l1 l2->
     let t=standard(List.length l1)(List.length l2) in
     if t<>Assistance_total_ordering_result_t.Equal then t else
     lex_compare f l1 l2
  ) in
   (tempf:>( ('a list) Assistance_total_ordering_t.t));;
 

let from_list (l:'a list)=
  let tempc=(fun x y->
  let rec tempf=(function
   []->(x<y)
   |u::peurrest->if u=x then List.mem(y)(peurrest)
                 else if u=y then false
                 else tempf(peurrest)
  ) in
  tempf l) in
  from_lt tempc;;

let min (f:'a Assistance_total_ordering_t.t)=function
 []->failwith("Min of the empty set is undefined")
 |a::b->
   let rec tempf=(fun
    (candidate,l)->match l with
      []->candidate
      |c::peurrest->if f(c)(candidate)=Assistance_total_ordering_result_t.Lower
                    then tempf(c,peurrest)
                    else tempf(candidate,peurrest)
   ) in
   tempf(a,b);;

let max (f:'a Assistance_total_ordering_t.t)=function
 []->failwith("Max of the empty set is undefined")
 |a::b->
   let rec tempf=(fun
    (candidate,l)->match l with
      []->candidate
      |c::peurrest->if f(c)(candidate)=Assistance_total_ordering_result_t.Greater
                    then tempf(c,peurrest)
                    else tempf(candidate,peurrest)
   ) in
   tempf(a,b);;
   
let minimize_it_with_care (cf:'a Assistance_total_ordering_t.t) 
   f=function
[]->failwith("careful min on empty set undefined")
|x::y->
 let rec minimize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                let howl=cf(va)(current_value) in
                if howl=Assistance_total_ordering_result_t.Lower
				then minimize_it_with_care0([a],va,peurrest)
				else if howl=Assistance_total_ordering_result_t.Equal
				     then minimize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else minimize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 minimize_it_with_care0([x],f(x),y);;


let maximize_it_with_care (cf:'a Assistance_total_ordering_t.t) 
   f=function
[]->failwith("careful max on empty set undefined")
|x::y->
 let rec maximize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                let howl=cf(va)(current_value) in
                if howl=Assistance_total_ordering_result_t.Greater
				then maximize_it_with_care0([a],va,peurrest)
				else if howl=Assistance_total_ordering_result_t.Equal
				     then maximize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else maximize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 maximize_it_with_care0([x],f(x),y);;

let modify_locally (f:'a Assistance_total_ordering_t.t) l=
  let big_m=max(f)(l) in
  let tempf=(fun x y->
    if List.mem(x)(l)
    then if List.mem(y)(l)
         then if x=y
              then Assistance_total_ordering_result_t.Equal
              else (from_list l x y)
         else f big_m y
    else if List.mem(y)(l)
         then f x big_m
         else f x y
  
  ) in
  (tempf:>( 'a Assistance_total_ordering_t.t));;

let list_for_dictionary_order=
  [97; 65; 98; 66; 99; 67; 100; 68; 101; 69; 102; 70; 103; 71; 104; 72; 105;
  73; 106; 74; 107; 75; 108; 76; 109; 77; 110; 78; 111; 79; 112; 80; 113; 81;
  114; 82; 115; 83; 116; 84; 117; 85; 118; 86; 119; 87; 120; 88; 121; 89;
  122; 90; 91; 92; 93; 94; 95; 96];;  

let reindexer_for_dictionary_order i=
    if (i<65)||(i>122) 
    then i 
    else 64+(Assistance_listennou.find_index i list_for_dictionary_order);;


let for_characters=let tempf=(fun x y->
  standard 
        (reindexer_for_dictionary_order(int_of_char x))
        (reindexer_for_dictionary_order(int_of_char y))
  ) in (tempf:>char Assistance_total_ordering_t.t);;

let for_integers=let tempf=(fun (x:int) (y:int)-> standard x y 
    ) in (tempf:>int Assistance_total_ordering_t.t);;  

let lex_for_strings=
    ((fun s1 s2->
      let m1=String.length s1
      and m2=String.length s2
      in
      let m=Stdlib.min(m1)(m2) in
      match Assistance_option.seek (fun j->(String.get s1 j)<>(String.get s2 j)) (Assistance_int_range.range 0 (m-1)) with
      None->standard m1 m2
      |Some(j)->for_characters (String.get s1 j) (String.get s2 j) 
    ) : string Assistance_total_ordering_t.t);;

let silex_for_strings=
      ((fun s1 s2->
        let m1=String.length s1
        and m2=String.length s2
        in
        let first_try=standard(m1)(m2) in
        if first_try<>Assistance_total_ordering_result_t.Equal
        then first_try
        else lex_for_strings s1 s2
      ) : string Assistance_total_ordering_t.t);;    


let lex_for_string_lists=
  ((fun l1 l2->
      let (_,left_part,right_part)=Assistance_listennou.factor (l1,l2) in
      if left_part=[] 
      then (if right_part=[] 
           then Assistance_total_ordering_result_t.Equal 
           else Assistance_total_ordering_result_t.Lower)
      else if right_part=[] 
           then Assistance_total_ordering_result_t.Greater 
           else lex_for_strings (List.hd left_part) (List.hd right_part)  
  ) : (string list) Assistance_total_ordering_t.t);;

let for_longest_match=  
    ((fun s1 s2->
      let m1=String.length s1
      and m2=String.length s2 in
      if (
          if m1>m2 then false else
          (String.sub s2 0 m1)=s1
      ) then Assistance_total_ordering_result_t.Greater else
      if (
          if m2>m1 then false else
          (String.sub s1 0 m2)=s2
      ) then Assistance_total_ordering_result_t.Lower else
      lex_for_strings s1 s2
     ): string Assistance_total_ordering_t.t);;


let for_longest_match_pairs=  
((fun (s1,v1) (s2,v2)->
  let first_try=silex_for_strings(s2)(s1) in
  if first_try<>Assistance_total_ordering_result_t.Equal 
  then first_try
  else standard v1 v2
 ): (string*'b) Assistance_total_ordering_t.t);;
 
let from_snd (f:'b Assistance_total_ordering_t.t)=((fun (x1,y1) (x2,y2)->
  let first_try=f y1 y2 in
  if first_try<>Assistance_total_ordering_result_t.Equal 
  then first_try
  else standard x1 x2
): ('a*'b) Assistance_total_ordering_t.t );;

let cardinality_then_diameter =((fun l1 l2->
  let first_try=standard (List.length l1) (List.length l2) in
  if first_try<>Assistance_total_ordering_result_t.Equal 
  then first_try
  else 
  let diam1 = List.hd(List.rev l1) - (List.hd l1)
  and diam2 = List.hd(List.rev l2) - (List.hd l2) in 
  let second_try=standard diam1 diam2 in
  if second_try<>Assistance_total_ordering_result_t.Equal 
  then second_try
  else lex_compare for_integers l1 l2
): (int list) Assistance_total_ordering_t.t );;

end ;;

let cardinality_then_diameter = Private.cardinality_then_diameter ;;
let for_integers = Private.for_integers ;;
let lex_compare = Private.lex_compare ;;
let lex_for_strings = Private.lex_for_strings ;;
let product = Private.product ;;
let silex_compare = Private.silex_compare ;;
let silex_for_strings = Private.silex_for_strings ;;
let silex_for_intlists = Private.silex_compare for_integers ;;
let standard = Private.standard ;;
let standard2 = Private.standard2 ;;
let triple_product = Private.triple_product ;;
 
           

end;;






module Assistance_strung=struct

(*

#use"strung.ml";;

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
    Assistance_int_range.scale (String.get s) 0 (n-1);;
    
 
let char_finder_from f s w0=
   let n=(String.length s) in
   let rec tempf=(fun j->
     if j>=n then 0 else
     if f(String.get s  j) then j+1 else
     tempf(j+1)
   ) in
   tempf(w0-1);;

let backwards_char_finder f s =
    let rec tempf=(fun j->
      if j<0 then 0 else
      if f(String.get s  j) then j+1 else
      tempf(j-1)
    ) in
    tempf((String.length s)-1);;   
 
let show_indices s=
  let n=String.length s in
  Assistance_int_range.scale (fun i->(i,String.get s (i-1)) ) 1 n;;   
   
let number_of_lines_before = Assistance_substring.Friend.number_of_lines_before;;

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
   Assistance_option.find_and_stop tester candidates;;
  
(*

find_one_of_several_in_at_idx ["ba";"ab"] "123ab67" 4;;

*)

let find_one_of_several_in_from_idx candidates s idx =
  let n=String.length s in 
  Assistance_option.find_and_stop (
    find_one_of_several_in_at_idx candidates s
  ) (Assistance_int_range.range idx n);;

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
          let temp1=List.filter(fun k->(get s k)='\n')(Assistance_int_range.range idx (idx2-1)) in 
          let line_idx_for_idx2=line_idx+List.length(temp1) in 
          let msg="Found "^(remove_newlines candidate)^" at line number "^(string_of_int line_idx_for_idx2)^"\n" in 
          let _=(print_string msg;flush stdout) in 
          let idx3=idx2+(String.length candidate) in  
          let temp2=List.filter(fun k->(get s k)='\n')(Assistance_int_range.range idx2 (idx3-1)) in 
          let line_idx_for_idx3=line_idx_for_idx2+List.length(temp2) in 
          tempf((idx2,idx3-1)::treated,other_patts,idx3,line_idx_for_idx3)    
  ) in 
  let temp3=List.filter(fun k->(get s k)='\n')(Assistance_int_range.range 1 (start_idx-1)) in 
  let start_line_idx = 1+(List.length(temp3)) in 
  tempf([],patterns_in_order,start_idx,start_line_idx);;

(*

find_successively_in [["ba";"ab"];["cde";"edc"]] "12\n\n\n\n\n8ab123\n\n67cde12";;

*)



let replace_ranges_in l s=
    if l=[] then s else
    let n=String.length s in
    let ranges=Assistance_image.image fst l in
    let partition=Assistance_listennou.partition_from_set_of_ranges ranges n in 
    let temp1=Assistance_image.image (
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
    let temp1=Assistance_image.image (fun (pref,idx)->(idx,pref)) l in
    let temp2=Assistance_image.image fst temp1 in
    let temp3=Assistance_ordered.sort Assistance_total_ordering.standard ((n+1)::temp2) in
    let temp4=Assistance_listennou.universal_delta_list temp3 in
    let temp5=Assistance_image.image(fun (i,j)->
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
   let lengths=Assistance_image.image String.length l in
   let m=Assistance_min.list lengths in
   let tester=(fun k->
     let temp1=Assistance_image.image (fun s->String.get s k) l in
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
   match Assistance_option.seek(fun j->
      (get s1 j)<>(get s2 j)
   )(Assistance_int_range.range 1 n) with 
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
     let (case,new_idx)=Assistance_option.unpack opt in
     let joiner=List.nth joiners (case-1) in 
     let idx2=new_idx+String.length(joiner) in 
     if case=1
     then (None,nbr_of_openers_so_far+1,items_so_far,current_item_start,world_start,idx2)
     else    
     if case=2
     then (
            if nbr_of_openers_so_far=1
            then let new_item = Assistance_cull_string.interval s current_item_start (new_idx-1) in 
                 (None,nbr_of_openers_so_far,new_item::items_so_far,idx2,world_start,idx2)
            else (None,nbr_of_openers_so_far,items_so_far,current_item_start,world_start,idx2)
          )
     else 
     if case=3
     then (
            if nbr_of_openers_so_far=1
            then let whole_interval=(world_start,idx2-1) in 
                 let last_item = Assistance_cull_string.interval s current_item_start (new_idx-1) in 
                 let items=List.rev(last_item::items_so_far) in
                 let answer=Some(items,whole_interval) in  
                 (answer,0,[],0,0,0)
            else (None,nbr_of_openers_so_far-1,items_so_far,current_item_start,world_start,idx2)
          )
     else raise(Unexpected_case_in_triune_analysis);;
     
let iterator_inside_nested_parentheses_parsing 
    (s,joiners,seeker) =
     let rec tempf=(fun state ->
     let (opt_result,nbr_of_openers_so_far,items_so_far,current_item_start,world_start,idx)=state in 
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
    let seeker = Assistance_substring.leftmost_index_of_pattern_among_in_from 
       [openr;separatr;closr] s in 
    
    let opt1=seeker 1 in 
    if opt1=None 
    then raise(Missing_opener(openr,s))
    else  
    let (case1,idx1)=Assistance_option.unpack opt1 in
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
   let s=Assistance_cull_string.interval enclosed_s 2 (n-1) in 
   let temp1=Assistance_cull_string.extract_intervals_in_wrt_separator s ";" in 
   Assistance_image.image int_of_string temp1;;

let of_intlist l=
  let temp1=Assistance_image.image string_of_int l in 
  "["^(String.concat ";" temp1)^"]";;

(*


let z1=[2;7;3;51];;
let z2=of_intlist z1;;
let check =(to_intlist(z2)=z1);;

*)  

let soak (replacee,replacer) s=
   if Assistance_substring.is_the_beginning_of replacee s 
   then Some(replacer^(Assistance_cull_string.two_sided_cutting (replacee,"") s))
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
     (Assistance_image.image (fun line->enclose("\n"^line)) other_lines) in 
    String.concat "^\n" quoted_lines;;


(*

let z1 = "abc\nde\nfghi\njkl";;
print_string(escaped_and_quoted z1);;



*)

let reposition_whole_according_to_separator separator lines =
      let temp1 = Assistance_image.image (fun line->(line,Assistance_substring.leftmost_index_of_in separator line)) lines in 
      let max_idx = snd(Assistance_max.maximize_it snd temp1) in 
      Assistance_image.image (fun (line,idx)->
          let offset = max_idx-idx in 
          (String.make offset ' ')^line
      ) temp1;;

let reposition_left_hand_side_according_to_separator separator lines =
         let temp1 = Assistance_image.image (fun line->
              let j = Assistance_substring.leftmost_index_of_in separator line in 
              ((Assistance_cull_string.beginning (j-1) line,Assistance_cull_string.cobeginning (j-1) line),j)) lines in 
         let max_idx = snd(Assistance_max.maximize_it snd temp1) in 
         Assistance_image.image (fun ((left,right),idx)->
             let offset = max_idx-idx in 
             left^(String.make offset ' ')^right
         ) temp1;;      

let announce ~trailer ~printer ~items ~separator=
   if items = []
   then ()
   else   
   let temp1 = Assistance_image.image printer items in
   let msg = "\n\n"^trailer^"\n\n"^(String.concat separator temp1)^"\n\n" in 
   (print_string msg;flush stdout) ;;    


end;;






module Assistance_dfa_subdirectory=struct

(*

Subdirectories name, with the trailing slash removed.

#use"Decomposed_filename/dfa_subdirectory.ml";;


*)

module Private = struct 

   let of_line s=
      let n = String.length s in 
      let indices = List.rev(Assistance_int_range.range 1 n) in 
      let limit_idx=(match Assistance_option.seek(fun j->(Assistance_strung.get s j)<>'/')(indices) with 
         None -> 0 |Some(j)->j
      ) in 
      Assistance_dfa_subdirectory_t.SD (Assistance_cull_string.beginning limit_idx s);;   
   
   let without_trailing_slash (Assistance_dfa_subdirectory_t.SD s)=s;;
   
   end ;;   
   
   
   
   let compute_long_subdir_name old_subdir new_subdir_short_name =
      let temp1 =  Assistance_cull_string.trim_slashes_on_the_right new_subdir_short_name in
      let long_name = (
      if String.contains temp1 '/'
      then temp1 
      else let old_subdir_name = Private.without_trailing_slash old_subdir in 
           let father_name = Assistance_cull_string.before_rightmost old_subdir_name '/' in 
           if father_name = ""
           then temp1
           else father_name^"/"^temp1 ) in 
      Private.of_line long_name ;;    
   
   let connectable_to_subpath (Assistance_dfa_subdirectory_t.SD s)=
     if s="" 
     then "" 
     else s^"/";;
   
   let begins_with (Assistance_dfa_subdirectory_t.SD s1) (Assistance_dfa_subdirectory_t.SD s2)=
      Assistance_supstring.begins_with s1 s2;;
       
   let extend (Assistance_dfa_subdirectory_t.SD s) subsub = Assistance_dfa_subdirectory_t.SD (s^"/"^subsub);;
   
   let main = Assistance_dfa_subdirectory_t.SD "";;
   
   let of_concrete_object ccrt_obj =
      let (_,(arg1,_,_,_,_,_,_))=Assistance_concrete_object.unwrap_bounded_variant ccrt_obj in 
      Assistance_dfa_subdirectory_t.SD(Assistance_crobj_converter.string_of_concrete_object arg1);;
   
   let of_line = Private.of_line ;;
   
   let rename_endsubdirectory (Assistance_dfa_subdirectory_t.SD(old_subdir),new_esdname) 
      (Assistance_dfa_subdirectory_t.SD s)=
      if Assistance_supstring.begins_with s old_subdir
      then let sub_s=Assistance_cull_string.cobeginning (String.length old_subdir) s in
           let t=Assistance_cull_string.before_rightmost old_subdir '/' in
           let new_t=(if t="" then "" else t^"/") in
           Assistance_dfa_subdirectory_t.SD(new_t^new_esdname^sub_s)
      else Assistance_dfa_subdirectory_t.SD(s);;
      
   (*
   
   rename_endsubdirectory (SD("Haag/Huug"),"Java") (SD "Haag/Huug/King/Jordan");;
   rename_endsubdirectory (SD("Haag"),"Java") (SD "Haag/Huug/King/Jordan");;
   
   *)              
   
   
   let soak (Assistance_dfa_subdirectory_t.SD(s1),Assistance_dfa_subdirectory_t.SD(s2)) (Assistance_dfa_subdirectory_t.SD(s))=
      match Assistance_strung.soak (s1,s2) s with 
      Some(t)->Some(Assistance_dfa_subdirectory_t.SD(t))
      |None -> None ;;
   
   let to_concrete_object (Assistance_dfa_subdirectory_t.SD(s))=
       Assistance_concrete_object_t.Variant("Dfa_"^"subdirectory_t.SD",
       [Assistance_crobj_converter.string_to_concrete_object(s)]);;
   
   
   
   let without_trailing_slash= Private.without_trailing_slash ;;
   
   
   

end;;






module Assistance_dfn_rootless_t=struct

(*

#use"Decomposed_filename/dfn_rootless_t.ml";;

*)

type t = J of Assistance_dfa_subdirectory_t.t * Assistance_dfa_module_t.t * Assistance_dfa_ending_t.t ;;
          



end;;






module Assistance_dfn_common=struct

(*

#use"Decomposed_filename/dfn_common.ml";;

*)

exception No_dot_in_string_to_rootless of string;;
exception Decompose_absolute_path_using_root_exn of Assistance_absolute_path.t * Assistance_dfa_root_t.t;;

let string_of_sm (s,m)=
   let (Assistance_dfa_subdirectory_t.SD sub)=s 
   and (Assistance_dfa_module_t.M mn)=m in 
   if sub=""
   then mn
   else sub^"/"^mn;;

let string_to_sm s=
   let (sub,mn)= Assistance_cull_string.split_wrt_rightmost s '/' in 
   (Assistance_dfa_subdirectory_t.SD sub,Assistance_dfa_module_t.M mn);;     

let string_to_rootless line=
  if not(String.contains line '.')
  then raise(No_dot_in_string_to_rootless(line)) 
  else 
  let (rest,ending) = Assistance_cull_string.split_wrt_rightmost line '.' in 
  let (s,m) = string_to_sm rest in 
  Assistance_dfn_rootless_t.J(s,m,Assistance_dfa_ending_t.E ending);;

   
let decompose_absolute_path_using_root ap root=
  let s_root=Assistance_dfa_root.without_trailing_slash root  
  and s_ap = Assistance_absolute_path.to_string ap in 
  let ns=String.length(s_root)
  and nw=String.length(s_ap) in
  if (ns+1)>nw then raise(Decompose_absolute_path_using_root_exn(ap,root)) else
  if (String.sub s_ap 0 (ns+1))<>(s_root^"/") then raise(Decompose_absolute_path_using_root_exn(ap,root)) else
  string_to_rootless(String.sub s_ap (ns+1) (nw-ns-1));;

let recompose_potential_absolute_path root 
      (Assistance_dfn_rootless_t.J(Assistance_dfa_subdirectory_t.SD(s),Assistance_dfa_module_t.M(m),Assistance_dfa_ending_t.E(e))) =
    let s_subdir = (if s="" then "" else s^"/") in 
    (Assistance_dfa_root.connectable_to_subpath root)^s_subdir^m^"."^e;;


end;;






module Assistance_dfn_middle_t=struct

(*

#use"Decomposed_filename/dfn_middle_t.ml";;

*)

type t = J of Assistance_dfa_subdirectory_t.t * Assistance_dfa_module_t.t ;;
          



end;;






module Assistance_dfn_rootless=struct

(*

#use"Decomposed_filename/dfn_rootless.ml";;


*)

module Private = struct 

let of_concrete_object crobj =
   let (_,(arg1,arg2,arg3,_,_,_,_))=Assistance_concrete_object.unwrap_bounded_variant crobj in 
   Assistance_dfn_rootless_t.J(
      Assistance_dfa_subdirectory.of_concrete_object arg1,
      Assistance_dfa_module.of_concrete_object arg2,
      Assistance_dfa_ending.of_concrete_object arg3
   );;

let to_concrete_object (Assistance_dfn_rootless_t.J(s,m,e))=
   Assistance_concrete_object_t.Variant("Dfn_"^"rootless.J",
     [
        
        Assistance_dfa_subdirectory.to_concrete_object s;
        Assistance_dfa_module.to_concrete_object m;
        Assistance_dfa_ending.to_concrete_object e;
     ]
   ) ;;



end ;; 


let is_compilable (Assistance_dfn_rootless_t.J(s,m,e))= Assistance_dfa_ending.is_compilable e;;

let is_in (Assistance_dfn_rootless_t.J(s,m,e)) sd = Assistance_dfa_subdirectory.begins_with s sd;;

let list_of_concrete_object crobj=
  Assistance_crobj_converter_combinator.to_list Private.of_concrete_object crobj ;;

let list_to_concrete_object l=
   Assistance_crobj_converter_combinator.of_list Private.to_concrete_object l;;


let of_concrete_object = Private.of_concrete_object ;;

let of_line line = Assistance_dfn_common.string_to_rootless line;;

let pair_list_of_concrete_object crobj=
  Assistance_crobj_converter_combinator.to_pair_list Private.of_concrete_object Private.of_concrete_object crobj ;;

let pair_list_to_concrete_object l=
  Assistance_crobj_converter_combinator.of_pair_list Private.to_concrete_object Private.to_concrete_object l;;


let relocate_to (Assistance_dfn_rootless_t.J(old_subdir,m,e)) new_subdir=Assistance_dfn_rootless_t.J(new_subdir,m,e);;

let rename_module_as  (old_m,new_m) old_path=
   let (Assistance_dfn_rootless_t.J(s,m,e))=old_path in 
   if m=old_m
   then Assistance_dfn_rootless_t.J(s,new_m,e)
   else old_path;;

let rename_subdirectory_as  (old_subdir,new_subdir) old_path=
   let (Assistance_dfn_rootless_t.J(s,m,e))=old_path in 
   if s=old_subdir
   then Assistance_dfn_rootless_t.J(new_subdir,m,e)
   else old_path;;

let soak (old_subdir,new_subdir) (Assistance_dfn_rootless_t.J(s,m,e)) =
   match Assistance_dfa_subdirectory.soak (old_subdir,new_subdir) s with 
   Some(new_s)->Some(Assistance_dfn_rootless_t.J(new_s,m,e))
   |None -> None ;;

let to_concrete_object = Private.to_concrete_object ;;

let to_ending (Assistance_dfn_rootless_t.J(s,m,e))=e;;

let to_line (Assistance_dfn_rootless_t.J(s,m,e))=
   (Assistance_dfa_subdirectory.connectable_to_subpath s)^
   (Assistance_dfa_module.to_line m)^(Assistance_dfa_ending.connectable_to_modulename e);;

let to_middle (Assistance_dfn_rootless_t.J(s,m,e))=Assistance_dfn_middle_t.J(s,m);;

let to_module (Assistance_dfn_rootless_t.J(s,m,e))=m;;

let to_subdirectory (Assistance_dfn_rootless_t.J(s,m,e))=s;;





    


end;;






module Assistance_dircopy_diff_t=struct

(*

#use"dircopy_diff_t.ml";;

*)

type t={
   recently_deleted : Assistance_dfn_rootless_t.t list;
   recently_changed : Assistance_dfn_rootless_t.t list;
   recently_created : Assistance_dfn_rootless_t.t list;
};;



end;;






module Assistance_dircopy_diff=struct

(*

#use"dircopy_diff.ml";;

*)

module Private=struct

let summarize_rootless_path rl=
   if List.mem (Assistance_dfn_rootless.to_ending rl) Assistance_dfa_ending.endings_for_compilable_files
   then String.capitalize_ascii(Assistance_cull_string.after_rightmost 
   (Assistance_cull_string.before_rightmost_possibly_all (Assistance_dfn_rootless.to_line rl) '.') '/')
   else Assistance_dfn_rootless.to_line rl;;
 
let summarize_rootless_path_list  l=
    let temp1=Assistance_image.image summarize_rootless_path l in
    Assistance_ordered.sort Assistance_total_ordering.silex_for_strings temp1;;

    

let salt = "Dircopy_"^"diff_t.";;

let recently_deleted_label = salt ^ "recently_deleted";;
let recently_changed_label = salt ^ "recently_changed";;
let recently_created_label = salt ^ "recently_created";;

let of_concrete_object ccrt_obj = 
   let g=Assistance_concrete_object.get_record ccrt_obj in
   {
      Assistance_dircopy_diff_t.recently_deleted = Assistance_dfn_rootless.list_of_concrete_object (g recently_deleted_label);
      recently_changed = Assistance_dfn_rootless.list_of_concrete_object (g recently_changed_label);
      recently_created = Assistance_dfn_rootless.list_of_concrete_object (g recently_created_label);
   };; 

let to_concrete_object dirdiff=
   let items= 
   [
    recently_deleted_label, Assistance_dfn_rootless.list_to_concrete_object dirdiff.Assistance_dircopy_diff_t.recently_deleted;
    recently_changed_label, Assistance_dfn_rootless.list_to_concrete_object dirdiff.Assistance_dircopy_diff_t.recently_changed;
    recently_created_label, Assistance_dfn_rootless.list_to_concrete_object dirdiff.Assistance_dircopy_diff_t.recently_created;
   ]  in
   Assistance_concrete_object_t.Record items;;

let is_empty x=
  (x.Assistance_dircopy_diff_t.recently_deleted,x.Assistance_dircopy_diff_t.recently_created,x.Assistance_dircopy_diff_t.recently_changed)=
   ([],[],[]);; 

let to_string x=
   if is_empty x then "{}" else 
   let tempf=(fun msg l->
   "\n"::msg::(Assistance_image.image(fun w->"\t\t"^(Assistance_dfn_rootless.to_line w)) l)
   ) in
   let temp1=tempf "Deleted : " (x.Assistance_dircopy_diff_t.recently_deleted)
   and temp2=tempf "Created : " (x.Assistance_dircopy_diff_t.recently_created)
   and temp3=tempf "Changed : " (x.Assistance_dircopy_diff_t.recently_changed) in
   String.concat "\n" (temp1@temp2@temp3) ;;

end;;



let add_changes diff l= 
  {
      diff with 
      Assistance_dircopy_diff_t.recently_changed = (diff.Assistance_dircopy_diff_t.recently_changed)@ l;
   };; 

let constructor a b c={
   Assistance_dircopy_diff_t.recently_deleted =a;
   Assistance_dircopy_diff_t.recently_changed =b;
   Assistance_dircopy_diff_t.recently_created =c;
};;


let create diff created_ones= 
  {
      diff with 
      Assistance_dircopy_diff_t.recently_created = (diff.Assistance_dircopy_diff_t.recently_created)@ created_ones;
   };; 

let destroy diff destroyed_ones= 
  {
      diff with 
      Assistance_dircopy_diff_t.recently_deleted = (diff.Assistance_dircopy_diff_t.recently_deleted)@ destroyed_ones;
   };; 


let empty_one  = 
   {
      Assistance_dircopy_diff_t.recently_deleted = [];
      recently_changed = [];
      recently_created = [];
   };; 


let explain  x=
   let tempf=(fun (msg,l)->
     if l=[]
     then None
     else Some(msg^" "^(String.concat "," l)^".")
   ) in
   let temp1=Assistance_option.filter_and_unpack tempf
   (* we use infinitives for github format *)
   [
     "Delete",Private.summarize_rootless_path_list  (x.Assistance_dircopy_diff_t.recently_deleted);
     "Create",Private.summarize_rootless_path_list  (x.Assistance_dircopy_diff_t.recently_created);
     "Modify",Private.summarize_rootless_path_list  (x.Assistance_dircopy_diff_t.recently_changed);
   ] in
   if temp1=[] then "" else
   let temp2=(String.uncapitalize_ascii (List.hd temp1))::(List.tl temp1) in
   String.concat " " temp2;; 
   


let is_empty = Private.is_empty ;;

let of_concrete_object = Private.of_concrete_object ;;

let print_out (fmt:Format.formatter) x=
   Format.fprintf fmt "@[%s@]" (Private.to_string x);;     


let recently_deleted x=x.Assistance_dircopy_diff_t.recently_deleted;;
let recently_created x=x.Assistance_dircopy_diff_t.recently_created;;
let recently_changed x=x.Assistance_dircopy_diff_t.recently_changed;;



let replace diff replacements= 
   let l_deleted = Assistance_image.image fst replacements 
   and l_created = Assistance_image.image snd replacements  in
  {
      diff with 
      Assistance_dircopy_diff_t.recently_created = (diff.Assistance_dircopy_diff_t.recently_created)@ l_created;
      Assistance_dircopy_diff_t.recently_deleted = (diff.Assistance_dircopy_diff_t.recently_deleted)@ l_deleted;
   };; 


let to_concrete_object = Private.to_concrete_object ;;   
   
   
   
              

end;;






module Assistance_detect_printer_declaration_in_text=struct

(*

#use"Text_editing/detect_printer_declaration_in_text.ml";;

*)

module Private = struct

let let_keyword = "let" ;;

let po_keyword  = "print_out" ;;

let blanks = [' ';'\n';'\r';'\t'];;

let after_blanks text =
  let n=String.length text in
  let rec tempf=(
    fun j->
      if j>n then None else
      if  List.mem (String.get text (j-1)) blanks
      then tempf(j+1)
      else Some(j)
  ) in
  tempf;;

let detect_printer_declaration_at_index text idx =
  if not (Assistance_substring.is_a_substring_located_at let_keyword text idx )
  then None 
  else 
  match after_blanks text (idx+(String.length let_keyword)) with 
   None -> None 
   |Some(idx2) ->
  if not (Assistance_substring.is_a_substring_located_at po_keyword text idx2 )
  then None 
  else  
  let idx3 = idx2+(String.length po_keyword) in 
  if List.mem (Assistance_strung.get text idx3) blanks 
  then Some(idx2)
  else None;;
  
let rec detect_printer_declaration_from_index text idx = 
   if idx>(String.length text)
   then None 
   else
   match  detect_printer_declaration_at_index text idx with 
   Some(idx2) -> Some(idx,idx2)
   |None -> detect_printer_declaration_from_index text (idx+1) ;;    

end ;;   

let detect text = Private.detect_printer_declaration_from_index text 0;;

(*

detect "123 l"^"et print_out = 54" ;;

*)

end;;






module Assistance_ocaml_library_t=struct

(* 

#use"Compilation_management/ocaml_library_t.ml";;

*)


type t=NumLib |StrLib |UnixLib;;

 


end;;






module Assistance_fw_file_small_details_t=struct

(*

#use"Filewatching/fw_file_small_details_t.ml";;


*)

type t ={
  used_modules : Assistance_dfa_module_t.t list ;
  used_libraries : Assistance_ocaml_library_t.t list ;
  has_printer : bool ;
  modification_time : string ;
};;


end;;






module Assistance_characters_in_namespace_name=struct

(*

#use"characters_in_namespace_name.ml";;

*)


let chars=
  (Assistance_int_range.scale char_of_int 65 90)@
  (Assistance_int_range.scale char_of_int 97 122)@
  (Assistance_int_range.scale char_of_int 48 57)@
  ['\\';'_'];;
           

end;;






module Assistance_charset=struct

(*

#use"charset.ml";;

*)

let lowercase_letters=    
  ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
   'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
   'u';'v';'w';'x';'y';'z'];;

    
let uppercase_letters= 
   ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
    'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';
    'U';'V';'W';'X';'Y';'Z'];;
    
let anycase_letters=
    lowercase_letters@uppercase_letters;;

let lowercase_identifier_elements=    
    ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
     'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
     'u';'v';'w';'x';'y';'z';'_';'+';'-';'*';
     '0';'1';'2';'3';'4';'5';'6';'7';'8';'9']@uppercase_letters;;
     
let php_label_first_letters =
  [
    'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
    'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
    'u';'v';'w';'x';'y';'z';
    'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
    'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';
    'U';'V';'W';'X';'Y';'Z';
    '_';
    ];;  

 let php_label_nonfirst_letters =
  php_label_first_letters
  @
  [
   '0';'1';'2';'3';'4';'5';'6';'7';'8';'9'
  ];;   

let ocaml_modulename_nonfirst_letters=
  ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o';
 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'; 'A'; 'B'; 'C'; 'D';
 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S';
 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'; '_'; '0'; '1'; '2'; '3'; '4'; '5'; '6';
 '7'; '8'; '9'];;

let alphanumeric_characters =
  php_label_nonfirst_letters @
  [
   '.';'\''
  ];;    

let unix_filename_admissible_characters =
  php_label_nonfirst_letters @
  [
   '.';'/';'!';'~';
  ];;        
    
let list_of_whites=[' ';'\n';'\r';'\t'];; 
  
let classlike_declaration_chars=
    list_of_whites@Assistance_characters_in_namespace_name.chars;;  

let enclosers=[
      '(',')';
      '[',']';
      '{','}';
];;
           

end;;






module Assistance_after=struct

(*

#use"after.ml";;

*)

let after_star l s =
  let n=String.length s in
  let rec tempf=(
    fun j->
      if j>n then None else
      if  List.mem (String.get s (j-1)) l
      then tempf(j+1)
      else Some(j)
  ) in
  tempf;;


let after_whites s =after_star Assistance_charset.list_of_whites s;;

  let after_whites_and_comments s=
    let n=String.length s in
    let rec tempf=(
      fun j->
        if j>n then None else
        if List.mem (String.get s (j-1)) Assistance_charset.list_of_whites
        then tempf(j+1)
        else 
        if Assistance_substring.is_a_substring_located_at "/*" s j
        then let k=Assistance_substring.leftmost_index_of_in_from "*/" s (j+2) in
             if k<0
             then None
             else tempf(k+2)
        else Some(j)
    ) in
    tempf;;
  
  (*    
  after_whites_and_comments "\n/* 567 */\t\r\n\n/* 89 ** // 78*/123";;    
  *)
  
exception Unfinished_simple_quoted_string of int;;  

let after_simple_quoted_string s k0=
    let n=String.length s in
    if (Assistance_strung.get s k0)<>'\''
    then k0
    else 
    let rec tempf=(fun k->
       if k>n
       then raise(Unfinished_simple_quoted_string(k0))
       else 
       let c=String.get s (k-1) in
       if c='\\'
       then tempf(k+2)
       else 
       if c='\''
       then k+1
       else tempf(k+1)
    ) in
    tempf (k0+1);;

(*

after_simple_quoted_string "'abc'67" 1;; 

*)    

exception Unfinished_double_quoted_string of int;;  
    
let after_double_quoted_string s k0=
        let n=String.length s in
        if (Assistance_strung.get s k0)<>'"'
        then k0
        else 
        let rec tempf=(fun k->
           if k>n
           then raise(Unfinished_double_quoted_string(k0))
           else 
           let c=String.get s (k-1) in
           if c='\\'
           then tempf(k+2)
           else 
           if c='"'
           then k+1
           else tempf(k+1)
        ) in
        tempf (k0+1);;     



exception Unbalanced_expression of char*char;;

let after_closing_character (lchar,rchar) s=
  let n=String.length s in
  let rec tempf=(
    fun (k,count)->
      if k>n
      then raise(Unbalanced_expression(lchar,rchar))
      else 
      if Assistance_substring.is_a_substring_located_at "/*" s k
      then let j=Assistance_substring.leftmost_index_of_in_from "*/" s (k+2) in
           tempf(j+2,count)
      else 
      if Assistance_substring.is_a_substring_located_at "//" s k
      then let j=Assistance_substring.leftmost_index_of_in_from "\n" s (k+2) in
           tempf(j+1,count)
      else 
      if (Assistance_substring.is_a_substring_located_at "<<<EOF\n" s k)
         ||
         (Assistance_substring.is_a_substring_located_at "<<<'EOF'\n" s k) 
      then let j=Assistance_substring.leftmost_index_of_in_from "\nEOF;\n" s (k+7) in
           tempf(j+6,count)
      else 
      let c=String.get s (k-1) in
      if c=lchar
      then tempf(k+1,count+1)
      else 
      if c='\''
      then let j=after_simple_quoted_string s k in
           tempf(j,count)
      else
      if c='"'
      then let j=after_double_quoted_string s k in
           tempf(j,count)
      else     
      if c<>rchar
      then tempf(k+1,count)
      else 
        if count=1
        then k+1
        else tempf(k+1,count-1)
  ) in
  tempf;;

(*

after_closing_character ('{','}') "{ 345 }89" (1,0);;
after_closing_character ('{','}') "{2{4}6{8{0}2}4}67" (1,0);;
after_closing_character ('{','}') "{\"3}5\"}89" (1,0);;
after_closing_character ('{','}') "{'3}5'}89" (1,0);;
after_closing_character ('{','}') "{/*4}6*/}01" (1,0);;
after_closing_character ('{','}') "{<<<EOF\n}\nEOF;\n}78" (1,0);;
after_closing_character ('{','}') "{<<<'EOF'\n}\nEOF;\n}90" (1,0);;

*)

let next_in_list l s=
  let n=String.length s in
  let rec tempf=(
    fun k->
      if k>n
      then None
      else 
      if Assistance_substring.is_a_substring_located_at "/*" s k
      then let j=Assistance_substring.leftmost_index_of_in_from "*/" s (k+2) in
           tempf(j+2)
      else 
      if Assistance_substring.is_a_substring_located_at "//" s k
      then let j=Assistance_substring.leftmost_index_of_in_from "\n" s (k+2) in
           tempf(j+1)
      else 
      if (Assistance_substring.is_a_substring_located_at "<<<EOF\n" s k)
         ||
         (Assistance_substring.is_a_substring_located_at "<<<'EOF'\n" s k) 
      then let j=Assistance_substring.leftmost_index_of_in_from "\nEOF;\n" s (k+7) in
           tempf(j+6)
      else 
      let c=String.get s (k-1) in
      if c='\''
      then let j=after_simple_quoted_string s k in
           tempf(j)
      else
      if c='"'
      then let j=after_double_quoted_string s k in
           tempf(j)
      else     
      if List.mem c l
      then Some(k)
      else tempf(k+1)
  ) in
  tempf;;


let after_classlike_declaration s i=
    Assistance_option.seek(
     fun j->not(List.mem 
         (String.get s (j-1)) Assistance_charset.classlike_declaration_chars
     )
    )(Assistance_int_range.range i (String.length s));;


let after_abstract_class s i0=
  if not(Assistance_substring.is_a_substring_located_at "abstract" s i0)
  then None
  else
  let opt1=after_whites s (i0+8) in
  if opt1=None then None else
  let i1=Assistance_option.unpack opt1 in
  if not(Assistance_substring.is_a_substring_located_at "class" s i1)
  then None
  else 
  let opt2=after_classlike_declaration s (i1+5) in
  if opt2=None then None else
  let i2=Assistance_option.unpack opt2 in
  if (Assistance_strung.get s i2)<>'{' then None else 
  Some(after_closing_character ('{','}') s (i2+1,1));;

(*

after_abstract_class "abstract  class {u\nv}234" 1;;

*)

let after_final_class s i0=
  if not(Assistance_substring.is_a_substring_located_at "final" s i0)
  then None
  else
  let opt1=after_whites s (i0+5) in
  if opt1=None then None else
  let i1=Assistance_option.unpack opt1 in
  if not(Assistance_substring.is_a_substring_located_at "class" s i1)
  then None
  else 
  let opt2=after_classlike_declaration s (i1+5) in
  if opt2=None then None else
  let i2=Assistance_option.unpack opt2 in
  if (Assistance_strung.get s i2)<>'{' then None else 
  Some(after_closing_character ('{','}') s (i2+1,1));;     

(*

after_final_class "final  class {u\nv}901" 1;;

*)

let after_usual_class s i0=
  if not(Assistance_substring.is_a_substring_located_at "class" s i0)
  then None
  else 
  let opt2=after_classlike_declaration s (i0+5) in
  if opt2=None then None else
  let i2=Assistance_option.unpack opt2 in
  if (Assistance_strung.get s i2)<>'{' then None else 
  Some(after_closing_character ('{','}') s (i2+1,1));;     

(*

after_usual_class "class {u\nv}234" 1;;
after_usual_class "class_loader { }" 1;;

*)

let after_interface s i0=
  if not(Assistance_substring.is_a_substring_located_at "interface" s i0)
  then None
  else 
  let opt2=after_classlike_declaration s (i0+5) in
  if opt2=None then None else
  let i2=Assistance_option.unpack opt2 in
  if (Assistance_strung.get s i2)<>'{' then None else 
  Some(after_closing_character ('{','}') s (i2+1,1));;  

(*

after_interface "interface {u\nv}678" 1;;

*)

let after_classlike_block s i=
   Assistance_option.find_and_stop(
     fun f->f s i
   )[
       after_abstract_class;
       after_final_class;
       after_usual_class;
       after_interface;
    ];;


(*

after_classlike_block "abstract  class {u\nv}234" 1;;
after_classlike_block "final  class {u\nv}901" 1;;
after_classlike_block "class {u\nv}234" 1;;
after_classlike_block "interface {u\nv}678" 1;;

*)    

let after_classlike_block_with_linebreak s i=
  let n=String.length s in
  let opt1=after_classlike_block s i in
  if opt1=None then None else
  let i1=Assistance_option.unpack opt1 in
  let opt2=Assistance_option.seek(fun j->
     not(List.mem (Assistance_strung.get s j) [' ';'\r';'\t']) )
  (Assistance_int_range.range i1 n) in
  if opt2=None then None else
  let i2=Assistance_option.unpack opt2 in
  if Assistance_strung.get s i2='\n'
  then Some(i2+1)
  else None;;
    
(*

after_classlike_block_with_linebreak "abstract  class {u\nv}  \t \n7" 1;;
after_classlike_block_with_linebreak "final  class {u\nv} \t\t\n3" 1;;
after_classlike_block_with_linebreak "class {u\nv}\n3" 1;;
after_classlike_block_with_linebreak "interface {u\nv} \t\n9" 1;;

*)    

exception End_of_div_not_found;;

let rec main_helper_for_div (s,n,div_count,idx)=
    if idx>n
    then raise(End_of_div_not_found)
    else
    if Assistance_substring.is_a_substring_located_at "</div>" s idx
    then if div_count=1
         then idx+6
         else main_helper_for_div(s,n,div_count-1,idx+6)
    else 
    if not(Assistance_substring.is_a_substring_located_at "<div " s idx)
    then main_helper_for_div(s,n,div_count,idx+1)
    else  
    let jdx=Assistance_substring.leftmost_index_of_in_from ">" s (idx+5) in
    main_helper_for_div(s,n,div_count+1,jdx);;

let after_div s idx=main_helper_for_div(s,String.length s,0,idx);;

(*

after_div "<div val=\"abc\"> xyz </div>789" 1;;

*)

let after_one pattern s idx=
  if Assistance_substring.is_a_substring_located_at pattern s idx
  then Some(idx+String.length pattern)
  else None;;

let after_one_among_several l_patterns s idx=
   Assistance_option.find_and_stop (
     fun pattern->after_one pattern s idx
   ) l_patterns;;

let  after_php_label s idx=
   if not(List.mem (Assistance_strung.get s idx) Assistance_charset.php_label_first_letters)
   then None
   else
   after_star 
     Assistance_charset.php_label_nonfirst_letters s (idx+1);;
     



           

end;;






module Assistance_alternative_str=struct

(*

An adptation of OCaml's Str module :

String indices are now from 1 to n instead of 0 to (n-1).
Operations on regexps are encoded as functions in the module.

#use"alternative_str.ml";;

*)

type backtrack_length=int;;

type regexp=M of string*(Str.regexp)*backtrack_length;;

let unveil (M(s,_,b))=s;;
let veil s=M(s,Str.regexp s,0);;

let set_backtrack (b:backtrack_length) (M(s,rgxp,_))=M(s,rgxp,b);;

let regexp_string s=let quote=Str.quote s in veil quote;;

let plus (M (s,_,_))=let new_s="\\("^s^"\\)+" in veil new_s;;
let star (M (s,_,_))=let new_s="\\("^s^"\\)*" in veil new_s;;

let concat (M(s1,_,_)) (M(s2,_,_))=veil (s1^s2);;
   
let big_concat l=
  if l=[]
  then veil ""
  else let temp1=List.map (fun w->"\\("^(unveil w)^"\\)") l in
       let new_s=String.concat "" temp1 in
       veil new_s;;   

let big_or l=
  let temp1=List.map (fun (M(s,_,_))->"\\("^s^"\\)") l in
  let new_s=String.concat "\\|" temp1 in
  veil new_s;;

let ore a b=big_or [a;b];;

let string_match (M(_,rgxp,b)) s i0=
   let bowl=(Str.string_match rgxp s (i0-1)) in
   if bowl
   then Some(i0+(String.length(Str.matched_string s))-b-1)
   else None;;
   
(* Functions from the option or ennig s *)  

let index_everything l=
  let rec tempf=
   (function (j,graet,da_ober)->
     match da_ober with
      []->graet
     |a::b->tempf(j-1,(j,a)::graet,b)
    )    in
    tempf(List.length(l),[],List.rev(l));;

let unpack=function
None->failwith("Void is not unpackable")
|Some(x)->x;;   
   
let rec find_and_stop f l=
 let rec find_and_stop0=(function
  da_ober->match da_ober with
   []->None
   |a::peurrest->match f(a) with
		None->find_and_stop0(peurrest)
		|Some(x)->Some(x)
 ) in
 find_and_stop0(l);;
   
(* End of functions from the option or ennig modules *)     
      
   
type left_regexp=regexp;;
type center_regexp=regexp;;   
type right_regexp=regexp;;   
   
type centered_regexp=left_regexp*center_regexp*right_regexp;;   
 
let create_centered_regexp a b c=((a,b,c):centered_regexp);;   
   
let centered_regexp_match ((a,b,c):centered_regexp) s i0=
  let opt1=string_match a s i0 in
  if opt1=None then None else
  let i1=unpack(opt1)+1 in
  let opt2=string_match b s i1 in
  if opt2=None then None else   
  let i2=unpack(opt2)+1 in 
  let opt3=string_match c s i2 in
  if opt3=None then None else   
  Some(i1,i2-1);;
   
 
let centered_regexp_list_match l s i0=
  let temp1=index_everything l in
  find_and_stop(fun (pattern_idx,rgxp)->
    match centered_regexp_match rgxp s i0 with
     Some(i_start,i_end)->Some(pattern_idx,(i_start,i_end))
    |None->None
  ) temp1;;   
   
 let find_all_occurrences l s i0=
  let n=String.length s in
  let rec tempf=(fun (graet,j)->
    if j>n then List.rev(graet) else
    let opt=centered_regexp_list_match l s j in
    if opt=None then tempf(graet,j+1) else
    let (pattern_idx,(i_start,i_end))=unpack(opt) in
    tempf((pattern_idx,(i_start,i_end))::graet,i_end+1)
  ) in
  tempf([],i0);;

   
   
 (*  
 
 centered_regexp_match (veil "12.",veil"4.6",veil"78") "123456789abcdef" 1;;
 
 let s1="123456789abcdef";;
 let a1=veil "12." and b1=veil"4.6" and c1=veil"78";;
 
 let w1=string_match a1 s1 1;;
 
 
 
 string_match (regexp_string "456") "123456789abcdef" 4;; 

let test_centered_regexp  
 
let search_forward (M(_,rgxp,b)) s i0=
   let j1=(Str.search_forward rgxp s (i0-1))+1 in
   let j2=j1+(String.length(Str.matched_string s))-b-1 in
   (j1,j2);;
  *)            

end;;






module Assistance_alternative_str_example=struct

(*

Concrete values of type My_str.regexp.

#use"alternative_str_example.ml";;

*)

let capital_letter=Assistance_alternative_str.veil "[A-Z]";;
let letters=Assistance_alternative_str.veil "[A-Za-z1-9_']*";;
let nonletter=Assistance_alternative_str.veil "[^A-Za-z1-9_']";;
let white=Assistance_alternative_str.veil "[ \n\r\t]";;
let maybe_whites=Assistance_alternative_str.star white;;
let some_whites=Assistance_alternative_str.plus white;;
let backtracking_nonletter=Assistance_alternative_str.set_backtrack 1 nonletter;;  

let delimited_module_name=Assistance_alternative_str.big_concat
  [
    nonletter;capital_letter;letters;nonletter
  ];;

let bare_module_name=Assistance_alternative_str.big_concat
  [
    capital_letter;letters
  ];;

let include_case=
  let left_part=Assistance_alternative_str.veil "[ \n\r\t]+include[ \n\r\t(]+"
  and center_part=bare_module_name 
  and right_part=backtracking_nonletter in
  Assistance_alternative_str.create_centered_regexp left_part center_part right_part;; 

let open_case=
  let left_part=Assistance_alternative_str.veil "[ \n\r\t]+open[ \n\r\t(]+"
  and center_part=bare_module_name 
  and right_part=backtracking_nonletter in
  Assistance_alternative_str.create_centered_regexp left_part center_part right_part;; 

let moodle_case=
  let left_part=Assistance_alternative_str.big_concat 
  [white;Assistance_alternative_str.veil"module";some_whites;
   bare_module_name;maybe_whites;Assistance_alternative_str.veil"=";maybe_whites]
  and center_part=bare_module_name 
  and right_part=backtracking_nonletter in
  Assistance_alternative_str.create_centered_regexp left_part center_part right_part;; 

let pointed_case=
  let left_part=nonletter
  and center_part=bare_module_name 
  and right_part=Assistance_alternative_str.regexp_string "." in
  Assistance_alternative_str.create_centered_regexp left_part center_part right_part;; 

let moodle_cases=[include_case;open_case;moodle_case;pointed_case];;
let index_for_include_case=1;;
let index_for_open_case=2;;
let index_for_moodle_case=3;;
let index_for_pointed_case=4;;


(*

let f case s=let (i,j)=Option.unpack(Alternative_str.centered_regexp_match case s 1) in 
(i,j,Cull_string.interval s i j);;

f include_case " include Peggy;; ";;
f include_case " include_once;; ";;
f moodle_case " module Amy = Lawson ";;
f pointed_case " 57+Everybody.talking-78 ";;

*)

 let capital_letter=Assistance_alternative_str.veil "[A-Z]";;
 
 let alphanumeric=
    Assistance_alternative_str.big_or
      [ 
     	Assistance_alternative_str.veil "[a-z]";
     	Assistance_alternative_str.veil "[A-Z]";
     	Assistance_alternative_str.veil "[0-9]";
     	Assistance_alternative_str.regexp_string "_";
      ];;
 
 let alphanumerics=Assistance_alternative_str.plus alphanumeric;;
 
 let beginning_of_module_definition=
    Assistance_alternative_str.set_backtrack 1
    (Assistance_alternative_str.big_concat
      [
         white;
         Assistance_alternative_str.regexp_string "module";
         some_whites;
         capital_letter;
         alphanumerics;
         some_whites;
         Assistance_alternative_str.regexp_string "=";
         some_whites;
         Assistance_alternative_str.regexp_string "struct";
         white;
          
      ]);;
      
 let beginning_of_module_reminder=
    Assistance_alternative_str.set_backtrack 1
    (Assistance_alternative_str.big_concat
      [
         white;
         Assistance_alternative_str.regexp_string "module";
         some_whites;
         capital_letter;
         alphanumerics;
         some_whites;
         Assistance_alternative_str.regexp_string ":";
         some_whites;
         Assistance_alternative_str.regexp_string "sig";
         white;
          
      ]);;
      
 let beginning_of_module_type_definition=
    Assistance_alternative_str.set_backtrack 1
    (Assistance_alternative_str.big_concat
      [
         white;
         Assistance_alternative_str.regexp_string "module";
         some_whites;
         Assistance_alternative_str.regexp_string "type";
         some_whites;
         capital_letter;
         alphanumerics;
         some_whites;
         Assistance_alternative_str.regexp_string "=";
         some_whites;
         Assistance_alternative_str.regexp_string "sig";
         white;
          
      ]);;           
      
      
 let the_end=
    Assistance_alternative_str.set_backtrack 1
    (Assistance_alternative_str.big_concat
      [
         white;
         Assistance_alternative_str.regexp_string "end";
         white;
          
      ]);;       
                 

end;;






module Assistance_outside_comments_and_strings=struct

(*

#use"outside_comments_and_strings.ml";;

Detect in a text the parts which can possibly contain module
names, i.e. those parts which are outside comments and outside
strings.

Comments are a little more complicated than strings because they
can be nested. Also, note that we can have strings inside comments :
for example (* a " ( * " b *) is a valid OCaml code snippet.


To keep the automaton simple, changes are notified as soon as
possible. Thus, the automaton toggles string_mode or changes
nesting comment depth as soon as the terminating character is
encountered.

*)

type state={
    depth : int;
    string_mode         : bool;
    lastchar_is_a_left_paren          : bool;
    lastchar_is_a_star                : bool;
    lastchar_is_a_backslash           : bool;
    lastchar_is_a_tick           : bool;
    rightmost_backslash_count_is_even : bool;
    penultchar_is_a_left_paren        : bool;
    interval_start : int;
    accumulator : (int*int*string) list;
};;


let one_more_step s n j c x=
   let d=x.depth in 
   let comment_opened_now=(x.lastchar_is_a_left_paren)&&(c='*')&&(not(x.string_mode))
   and comment_closed_now=
            (not(x.penultchar_is_a_left_paren))
            &&(x.lastchar_is_a_star)
            &&(c=')')
            &&(not(x.string_mode)) in
   let new_depth=(
   		if x.string_mode
        then d
        else
        if comment_opened_now
        then d+1
        else  
        if comment_closed_now
        then max 0 (d-1)
        else  d
   
   ) in
   let string_opened_now=(c='"')&&(not(x.string_mode))&&
       (not(x.lastchar_is_a_backslash))&&
       (not(x.lastchar_is_a_tick))
   and string_closed_now=(c='"')&&(x.string_mode)&&(
      if x.lastchar_is_a_backslash
      then x.rightmost_backslash_count_is_even
      else true
   ) in
   let new_start=
      ((x.depth=0)&&string_closed_now)
      ||
      ((x.depth=1)&&comment_closed_now)
      ||
      ((x.depth=0)&&comment_opened_now) in
    let optional_last_index_for_interval=(
       if x.depth>0
       then None
       else
       if string_opened_now
       then Some(j-1)
       else
       if comment_opened_now
       then Some(j-2)
       else 
       if (j=n)&&(not(x.string_mode))
       then Some(j)
       else None
    ) in
    let old_accu=x.accumulator in  
    let new_accu=(
       match optional_last_index_for_interval with
       None->old_accu
       |Some(upper_bound)->
           let lower_bound=x.interval_start in
           if lower_bound>upper_bound
           then old_accu
           else let new_itv=Assistance_cull_string.interval s lower_bound upper_bound in 
               (lower_bound,upper_bound,new_itv)::old_accu
    ) in  
      
  {
    depth =new_depth;
    string_mode    =(if string_opened_now then true else
                     if string_closed_now then false else
                     x.string_mode);
    lastchar_is_a_left_paren   =(c='(');
    lastchar_is_a_star         =(c='*');
    lastchar_is_a_backslash    =(c='\\');
    lastchar_is_a_tick    =(c='\'');
    rightmost_backslash_count_is_even=(if c<>'\\' 
                                       then true 
                                       else not(x.rightmost_backslash_count_is_even) );
    penultchar_is_a_left_paren =x.lastchar_is_a_left_paren;
    interval_start=(if new_start then j+1 else x.interval_start);
    accumulator=new_accu;
};;

let initial_state=  
 {
    depth =0;
    string_mode    =false;
    lastchar_is_a_left_paren   =false;
    lastchar_is_a_star         =false;
    lastchar_is_a_backslash    =false;
    lastchar_is_a_tick    =false;
    rightmost_backslash_count_is_even=true;
    penultchar_is_a_left_paren =false;
    interval_start=1;
    accumulator=[];
};;

let rec iterator (s,n,j,st)=
    if j>n
    then List.rev(st.accumulator)
    else iterator(s,n,j+1,one_more_step s n j (String.get s (j-1)) st);;
    
let good_substrings s=iterator(s,String.length s,1,initial_state);;    

(*

[
((good_substrings "abcdef")=    [1, 6, "abcdef"]);
((good_substrings "(*abc*)def")=[8, 10, "def"]);
((good_substrings "ab(*cde*)f")=[1, 2, "ab"; 10, 10, "f"]);
((good_substrings "let a=\"\\\"\" in a+1;;")=[1, 6, "let a="; 11, 19, " in a+1;;"] );
((good_substrings "let a='\\\"' in a+2;;")=[1, 19, "let a='\\\"' in a+2;;"]  );
((good_substrings "let a=\"\\\\\" in a+3;;")=[1, 6, "let a="; 11, 19, " in a+3;;"]  );
((good_substrings "let a=\"\\\\\\\" in a+3;;")=[1, 6, "let a="]  );
];;

good_substrings "ab\"cde\"f";;
good_substrings "\"abc\"def";;
good_substrings "ghi(*a(*b*)c*)def";;
good_substrings "ghi(**a(*b*)c**)def";;
good_substrings "ghi(**a\"b\"c**)def";;
good_substrings "123\"(*\"890\"*)\"567";;
good_substrings "123(*67\"90\"23*)67";;
good_substrings "let a=7;; let b='\"';; let c=8;;";;
good_substrings "let a=7;; let b='\"';; (* ahem *) let c=8;;";;

let nachste (s,n,j,st)=(s,n,j+1,one_more_step s n j (String.get s (j-1)) st);;
let s0="123456\"\\\\\"123456789";;
let n0=String.length s0;;
let v0=(s0,n0,1,initial_state);;
let ff=Memoized.small nachste v0;;
let gg n=match ff n with (_,_,_,st)->st;;


*)      

           

end;;






module Assistance_functor_for_sets=struct

(*
 
#use"Ordered_Lists/functor_for_sets.ml";;

Here all the possible dependencies are defined. Each particular
instance defines only the value it needs.

*)


type ('a,'b) parameter = (('a list) -> 'b) * ('b -> ('a list)) * ('a Assistance_total_ordering_t.t);;


let does_not_intersect ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= Assistance_ordered.does_not_intersect cmpr (deco ox) (deco oy);;
    
let empty_set ((co,deco,cmpr):('a,'b) parameter) = co [];;

let fold_merge ((co,deco,cmpr):('a,'b) parameter) 
     l=co (Assistance_ordered.fold_merge cmpr (Assistance_image.image deco l));;

let fold_intersect ((co,deco,cmpr):('a,'b) parameter) 
     l=co (Assistance_ordered.fold_intersect cmpr (Assistance_image.image deco l));;
    
let forget_order ((co,deco,cmpr):('a,'b) parameter) =deco;;

let hd ((co,deco,cmpr):('a,'b) parameter) ox= List.hd(deco ox);;

let image ((co,deco,cmpr):('a,'b) parameter) f ox= Assistance_image.image f (deco ox);;

let insert ((co,deco,cmpr):('a,'b) parameter) 
     x oy= co(Assistance_ordered.insert cmpr x (deco oy));;

let intersect ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= co(Assistance_ordered.intersect cmpr (deco ox) (deco oy));;

let intersects ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= Assistance_ordered.intersects cmpr (deco ox) (deco oy);;

let is_included_in ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= Assistance_ordered.is_included_in cmpr (deco ox) (deco oy);;

let length ((co,deco,cmpr):('a,'b) parameter) ox= List.length(deco ox);;

let max ((co,deco,cmpr):('a,'b) parameter) ox= List.hd(List.rev (deco ox));;

let mem ((co,deco,cmpr):('a,'b) parameter) 
     x oy= Assistance_ordered.mem cmpr x (deco oy);;

let merge ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= co(Assistance_ordered.merge cmpr (deco ox) (deco oy));;

let min ((co,deco,cmpr):('a,'b) parameter) ox= List.hd(deco ox);;

let nmem ((co,deco,cmpr):('a,'b) parameter) 
     x oy= not(Assistance_ordered.mem cmpr x (deco oy));;

let outsert ((co,deco,cmpr):('a,'b) parameter) 
     x oy= co(Assistance_ordered.outsert cmpr x (deco oy));;

let safe_set ((co,deco,cmpr):('a,'b) parameter) 
     l= co(Assistance_ordered.safe_set cmpr l);;

let select_minimal_elements_for_inclusion 
  ((co,deco,cmpr):('a,'b) parameter) ll  
   = 
     let old_form = Assistance_image.image deco ll in
     let new_form = Assistance_ordered.select_minimal_elements_for_inclusion cmpr old_form in 
     Assistance_image.image co new_form;;

let setminus ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= co(Assistance_ordered.setminus cmpr (deco ox) (deco oy));;

let singleton ((co,deco,cmpr):('a,'b) parameter)  x=co[x];;

let size_of_intersection ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= List.length(Assistance_ordered.intersect cmpr (deco ox) (deco oy));;

let sort ((co,deco,cmpr):('a,'b) parameter) 
     l= co(Assistance_ordered.sort cmpr l);;

let tl ((co,deco,cmpr):('a,'b) parameter) ox= co(List.tl(deco ox));;

let unsafe_set ((co,deco,cmpr):('a,'b) parameter) 
     l= co l;;



end;;






module Assistance_set_of_strings_t=struct

(* 

#use"Ordered_Lists/set_of_strings_t.ml";;

*)

type t=S of string list;;


end;;






module Assistance_set_of_strings=struct

(* 

#use"Ordered_Lists/set_of_strings.ml";;

*)

let tr = ((fun x->Assistance_set_of_strings_t.S(x)),(fun (Assistance_set_of_strings_t.S(x))->x),Assistance_total_ordering.silex_for_strings);;


let forget_order x= Assistance_functor_for_sets.forget_order tr x;;
let image f x= Assistance_functor_for_sets.image tr f x;;
let safe_set l= Assistance_functor_for_sets.safe_set tr l;;
let sort l= Assistance_functor_for_sets.sort tr l;;





end;;






module Assistance_three_parts=struct

(*

#use"three_parts.ml";;

*)




let generic=function
[]->[]
|u::v->
let rec tempf=
(function
((graet,x,da_ober),accu)->
let accu2=(graet,x,da_ober)::accu in
match da_ober with
[]->accu2
|a::b->tempf((x::graet,a,b),accu2)
) in
tempf(([],u,v),[]);;

let complemented_points l=List.rev_map(function (kleiz,x,dehou)->
(x,List.rev_append(kleiz)(dehou)))
(generic l);;

let beheaded_tails l=List.rev_map (function (kleiz,x,dehou)->(x,dehou) )(generic l);;

let select_center_element_and_reverse_left f l=
  let rec tempf=(fun (graet,da_ober)->match da_ober with
  []->(graet,None,[])
  |x::peurrest->if f x 
                then (graet,Some(x),peurrest)
                else tempf(x::graet,peurrest)
  ) in
  tempf([],l);;

let select_center_element f l=
  let (temp1,opt,after)=select_center_element_and_reverse_left f l in 
  (List.rev temp1,opt,after);;

let decompose_according_to_end_markers f l =
  let rec tempf=(
     fun (treated,to_be_treated)->
       let (before,opt,after)=select_center_element f to_be_treated in 
       if opt=None then (List.rev treated,before) else 
       tempf((before,Assistance_option.unpack opt)::treated,after)
  ) in 
  tempf([],l);;


let decompose_according_to_beginning_markers f l=
  let (nonlast_ones,last_one)=decompose_according_to_end_markers f (List.rev l) in
  (List.rev last_one,
    List.rev_map (fun (x,marker)->
      (marker,List.rev x)
    ) nonlast_ones
  );; 
  

(*

decompose_according_to_beginning_markers (fun x->List.mem x [3;7;11]) 
[1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20];;

decompose_according_to_end_markers (fun x->List.mem x [3;7;11]) 
[1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20];;

*)  

let select_left_interval f l=
  (* note that the "interval" is returned in reverse form *)
  let rec tempf=(fun (graet,da_ober)->match da_ober with
  []->(graet,[])
  |x::peurrest->if f x 
                then tempf(x::graet,peurrest) 
                else (graet,da_ober)
  ) in
  tempf([],l);;

let select_center_interval f l=
  let rec tempf=(fun (graet,da_ober)->match da_ober with
  []->(List.rev graet,[],[])
  |x::peurrest->if f x 
                then let (temp1,temp2)=select_left_interval f da_ober in
                     (List.rev graet,List.rev(temp1),temp2)
                else tempf(x::graet,peurrest)
  ) in
  tempf([],l);;


let replace_in_list replacee replacer l=
  let (temp1,opt,temp2)=select_center_element (fun t->t=replacee) l in
  if opt=None then l else List.rev_append temp1 (replacer@temp2);;
   
           

end;;






module Assistance_look_for_module_names=struct

(*

#use"Ocaml_analysis/look_for_module_names.ml";;

*)

exception Unknown_ending_during_modulename_changing of string ;;
exception Change_not_implemented of string ;;

exception Unknown_ending_during_modulename_reading of string ;;
exception Reading_not_implemented of string ;;

module Private = struct 


let indices_in_ml_ocamlcode code=
  let temp1=Assistance_outside_comments_and_strings.good_substrings code in
  let temp2=Assistance_image.image (fun (a,b,t)->
     let ttemp3=Assistance_alternative_str.find_all_occurrences Assistance_alternative_str_example.moodle_cases t 1 in
     Assistance_image.image (fun (case_index,(u,v))->
        (case_index,(u+a-1,v+a-1))
     ) ttemp3
  ) temp1 in
  List.flatten temp2;;

let indices_in_mli_ocamlcode code=  indices_in_ml_ocamlcode code ;; 
let indices_in_mll_ocamlcode code=  indices_in_ml_ocamlcode code ;; 
let indices_in_mly_ocamlcode code=  indices_in_ml_ocamlcode code ;; 
  
let indices_in_mlx_file ap=  
    let s_ap = Assistance_absolute_path.to_string ap in 
    let ending = Assistance_cull_string.after_rightmost s_ap '.' in 
    if ending = "ml"  then indices_in_ml_ocamlcode (Assistance_io.read_whole_file ap) else 
    if ending = "mli" then indices_in_mli_ocamlcode (Assistance_io.read_whole_file ap) else   
    if ending = "mll" then indices_in_mll_ocamlcode (Assistance_io.read_whole_file ap) else 
    if ending = "mly" then indices_in_mly_ocamlcode (Assistance_io.read_whole_file ap) else   
    raise(Unknown_ending_during_modulename_reading s_ap);;  


let indices_in_ml_file file=indices_in_ml_ocamlcode(Assistance_io.read_whole_file file);;  

let names_in_mlx_file ap=
  let temp1=indices_in_mlx_file ap in
  let text = Assistance_io.read_whole_file ap in 
  let temp2=Assistance_image.image (fun (_,(a,b))->String.sub text (a-1) (b-a+1) ) temp1 in
  let temp3=Assistance_three_parts.generic temp2 in
  let temp4=List.filter (fun (x,y,z)->not(List.mem y x)) temp3 in
  let temp5=Assistance_image.image (fun (x,y,z)->Assistance_dfa_module.of_line 
      (String.uncapitalize_ascii  y)) temp4 in
  temp5;;


let change_module_name_in_ml_ocamlcode
   old_naked_name
   new_naked_name old_code=
   let old_name=String.capitalize_ascii(Assistance_dfa_module.to_line(old_naked_name))
   and new_name=String.capitalize_ascii(Assistance_dfa_module.to_line(new_naked_name)) in
   let itv=(fun a b->String.sub old_code (a-1) (b-a+1)) in
   let temp1=indices_in_ml_ocamlcode old_code in
   let temp2=List.filter (fun (j,(a,b))->(itv a b)=old_name ) temp1 in
   if temp2=[]
   then old_code
   else
   let temp3 = Assistance_image.image (fun (j,(a,b))->((a,b),new_name) ) temp2 in  
   Assistance_strung.replace_ranges_in temp3 old_code;;
 
  

 let change_module_name_in_ml_file old_name new_name file=
   let s=Assistance_io.read_whole_file file in
   let new_s=change_module_name_in_ml_ocamlcode old_name new_name s in
   Assistance_io.overwrite_with file new_s;;  

 let change_module_name_in_mli_file old_name new_name file=
 change_module_name_in_ml_file old_name new_name file ;;

  let change_module_name_in_mlx_file old_name new_name ap=  
    let s_ap = Assistance_absolute_path.to_string ap in 
    let ending = Assistance_cull_string.after_rightmost s_ap '.' in 
    if ending = "ml"  then change_module_name_in_ml_file old_name new_name ap else 
    if ending = "mli" then change_module_name_in_mli_file old_name new_name ap else   
    if ending = "mll" then raise(Change_not_implemented s_ap) else 
    if ending = "mly" then raise(Change_not_implemented s_ap) else   
    raise(Unknown_ending_during_modulename_changing s_ap);;

let change_several_module_names_in_ml_ocamlcode l_changes s=
    List.fold_left(fun t (u,v)->change_module_name_in_ml_ocamlcode u v t) s l_changes;;

let change_several_module_names_in_ml_file l_changes file=
   let s=Assistance_io.read_whole_file file in
   let new_s=change_several_module_names_in_ml_ocamlcode l_changes s in
   Assistance_io.overwrite_with file new_s;;  

let list_values_from_module_in_file module_name file=
   let s=Assistance_io.read_whole_file file in
   let temp1=indices_in_mlx_file file in
   let temp2=List.filter (fun (t,(i,j))->
     (t=Assistance_alternative_str_example.index_for_pointed_case)&&
     (Assistance_cull_string.interval s i j=(String.capitalize_ascii module_name))
   ) temp1 in
   let temp3=Assistance_image.image(fun (t,(i,j))->
    let opt=Assistance_after.after_star 
     Assistance_charset.ocaml_modulename_nonfirst_letters
     s (j+2) in
    let end_idx=(match opt with Some(k)->k-1 |None->String.length s) in
     Assistance_cull_string.interval s (j+2) end_idx
   ) temp2 in
   Assistance_set_of_strings.sort temp3;;

end ;;

let change_module_name_in_mlx_file = Private.change_module_name_in_mlx_file ;;
 let change_module_name_in_ml_ocamlcode = Private.change_module_name_in_ml_ocamlcode ;;
 let change_several_module_names_in_ml_ocamlcode = Private.change_several_module_names_in_ml_ocamlcode ;;
 let indices_in_mlx_file = Private.indices_in_mlx_file ;;
 let list_values_from_module_in_file = Private.list_values_from_module_in_file ;;
 let names_in_mlx_file = Private.names_in_mlx_file ;;
 

(*   
   
indices_in_string "123 Haag.012 open Garfield;8";;

indices_in_string "(* Haag. *)234 Dog.\"open Garfield;\"67 Corn.4";;

let example = String.concat "\n" [
""; "open Aantron_markup_common"; ""; "module Aantron_peggy = Aantron_kstream";""; "include Aantron_kstream.Foo";"";"val parse :";
"  [< `Document | `Fragment of string ] option ->";
"   Aantron_markup_error.parse_handler ->";
"  (location *  Aantron_html_tokenizer.token)  Aantron_kstream.t *";
"  ( Aantron_html_tokenizer.state -> unit) *";
"  ((unit -> bool) -> unit) ->";
"    (location * signal)  Aantron_kstream.t"; ""] ;;

let see_example = change_module_name_in_ml_ocamlcode
   (Dfa_module_t.M "aantron_kstream") (Dfa_module_t.M "other_kstream") z1 ;;
   
*)              

end;;






module Assistance_ocaml_library=struct

(* 

#use"Compilation_management/ocaml_library.ml";;

*)


let correspondances=[
   Assistance_ocaml_library_t.NumLib,"num";
   Assistance_ocaml_library_t.StrLib,"str";
   Assistance_ocaml_library_t.UnixLib,"unix"];;
let capitalized_correspondances =Assistance_image.image (
   fun (x,y)->(x,"Ocaml"^"_library_t."^y)
) correspondances;;

exception Unknown_lib of string;;

let of_string s=
  try (fst(Assistance_listennou.force_find (fun (x,y)->y=s) correspondances))
  with _->raise(Unknown_lib(s));;

let to_string lib=snd(Assistance_listennou.force_find (fun (x,y)->x=lib) correspondances);;  


let short_name=function
   Assistance_ocaml_library_t.NumLib->"NumLib" 
  |StrLib->"StrLib" 
  |UnixLib->"UnixLib";;

let ocaml_name lib=
  (*cutting the name as always, to avoid a circular definition *)
  "Ocaml"^"_library."^(short_name lib);;

let file_for_library=function 
  Assistance_ocaml_library_t.NumLib->"nums" |StrLib->"str" |UnixLib->"unix";;  

let modules_telling_a_library_away=function
Assistance_ocaml_library_t.NumLib->["num";"big_int";"arith_status"] 
|StrLib->["str"] 
|UnixLib->["unix"];;    


let all_libraries=[Assistance_ocaml_library_t.NumLib;Assistance_ocaml_library_t.StrLib;Assistance_ocaml_library_t.UnixLib];;  

let compute_needed_libraries_from_uncapitalized_modules_list l=
   List.filter (
      fun lib->List.exists(
        fun z->List.mem z (modules_telling_a_library_away lib)
      ) l
   ) all_libraries;;
           
let of_concrete_object =Assistance_concrete_object.unwrap_lonely_variant 
  capitalized_correspondances;;
          
let to_concrete_object =Assistance_concrete_object.wrap_lonely_variant 
  capitalized_correspondances;;    


end;;






module Assistance_fw_file_small_details=struct

(*

#use"Filewatching/fw_file_small_details.ml";;

*)


module Private = struct 

let salt = "Fw_"^"file_simple_details_t.";;
      
let used_modules_label      = salt ^ "used_modules";;
let used_libraries_label    = salt ^ "used_libraries";; 
let has_printer_label       = salt ^ "has_printer";;
let modification_time_label = salt ^ "modification_time";;

let of_concrete_object ccrt_obj = 
  let g=Assistance_concrete_object.get_record ccrt_obj in
  {
     Assistance_fw_file_small_details_t.used_modules = Assistance_crobj_converter_combinator.to_list Assistance_dfa_module.of_concrete_object (g used_modules_label);
     used_libraries = Assistance_crobj_converter_combinator.to_list Assistance_ocaml_library.of_concrete_object (g used_libraries_label);
     has_printer = Assistance_crobj_converter.bool_of_concrete_object (g has_printer_label);
     modification_time = Assistance_crobj_converter.string_of_concrete_object (g modification_time_label);
  };; 

let to_concrete_object fsd=
  let items= 
  [
    used_modules_label, Assistance_crobj_converter_combinator.of_list Assistance_dfa_module.to_concrete_object fsd.Assistance_fw_file_small_details_t.used_modules;
    used_libraries_label, Assistance_crobj_converter_combinator.of_list Assistance_ocaml_library.to_concrete_object fsd.Assistance_fw_file_small_details_t.used_libraries;
    has_printer_label, Assistance_crobj_converter.bool_to_concrete_object fsd.Assistance_fw_file_small_details_t.has_printer;
    modification_time_label, Assistance_crobj_converter.string_to_concrete_object (fsd.Assistance_fw_file_small_details_t.modification_time);
  ]  in
  Assistance_concrete_object_t.Record items;;

end ;;  

let compute ap =
    let full_text = Assistance_io.read_whole_file ap in 
    let used_mods = Assistance_look_for_module_names.names_in_mlx_file ap in 
    let snippets = Assistance_outside_comments_and_strings.good_substrings full_text in 
    let printer_exists = List.exists (fun (i,j,subtext)->
           (Assistance_detect_printer_declaration_in_text.detect subtext)<>None 
         ) snippets  in 
    let used_libs = Assistance_ocaml_library.compute_needed_libraries_from_uncapitalized_modules_list
       (Assistance_image.image Assistance_dfa_module.to_line used_mods) in  
    let s_ap = Assistance_absolute_path.to_string ap in       
    let mtime = string_of_float((Unix.stat s_ap).Unix.st_mtime) in    
    {
      Assistance_fw_file_small_details_t.used_modules = used_mods;
      used_libraries = used_libs ;
      has_printer = printer_exists;
      modification_time = mtime ;
    } ;;
    

let has_printer fsd = fsd.Assistance_fw_file_small_details_t.has_printer ;;
let modification_time fsd = fsd.Assistance_fw_file_small_details_t.modification_time ;;
let of_concrete_object = Private.of_concrete_object ;;
let to_concrete_object = Private.to_concrete_object ;;
let used_libraries fsd = fsd.Assistance_fw_file_small_details_t.used_libraries ;;
let used_modules fsd = fsd.Assistance_fw_file_small_details_t.used_modules ;;

end;;






module Assistance_fw_instance_index_t=struct

(*

#use"Filewatching/fw_instance_index_t.ml";;

*)

type t = I of int ;;

end;;






module Assistance_fw_state_index_t=struct

(*

#use"Filewatching/fw_state_index_t.ml";;

*)

type t = I of int ;;

end;;






module Assistance_fw_indexer=struct

(*

#use"Filewatching/fw_indexer.ml";;



*)

exception Invalid_instance_index of int ;;

module Private = struct 

let main_ref = ref ([]: Assistance_fw_state_index_t.t list) ;;
   


end ;;  

let create_new_instance () =
  let raf = Private.main_ref in 
  let n = List.length (!raf) 
  and starter = Assistance_fw_state_index_t.I 0 in 
  let _ = (raf := (!raf) @ [starter]) in 
  Assistance_fw_instance_index_t.I(n+1) ;;

let get_state (Assistance_fw_instance_index_t.I ii) =
  try List.nth (!(Private.main_ref)) (ii-1) with 
  _ -> raise (Invalid_instance_index(ii)) ;;
  
let make_full_instance () = 
   let idx = create_new_instance () in 
   (idx,get_state idx) ;;

let push_state instance =
  let raf = Private.main_ref in 
  let (Assistance_fw_state_index_t.I old_state) = get_state instance 
  and indexed_old_list = Assistance_int_range.index_everything (!raf) in 
  let new_state = (Assistance_fw_state_index_t.I (old_state+1)) in 
  let (Assistance_fw_instance_index_t.I ii) = instance in 
  let new_list = Assistance_image.image 
       (fun (k,st)->if k=ii then new_state else st) indexed_old_list in 
  let _ = (raf := new_list) in 
  () ;;

   


end;;






module Assistance_fw_poly_t=struct

(*

#use"Filewatching/fw_poly_t.ml";;

*)


type t = { 
   type_name : string ;
   root : Assistance_dfa_root_t.t ;
   ignored_subdirectories : Assistance_dfa_subdirectory_t.t list ;
   ignored_files : Assistance_dfn_rootless_t.t list ;
   watched_files : (Assistance_dfn_rootless_t.t * string) list ;
   subdirs_for_archived_mlx_files : Assistance_dfa_subdirectory_t.t list ;
   small_details_in_files : (Assistance_dfn_rootless_t.t * Assistance_fw_file_small_details_t.t) list ;
   index_for_caching : Assistance_fw_instance_index_t.t * Assistance_fw_state_index_t.t ;
   last_compilation_result_for_module : (Assistance_dfa_module_t.t * bool) list ;
   dir_for_backup : Assistance_dfa_root_t.t ;
   gitpush_after_backup : bool ;
   github_url : string ;
   encoding_protected_files : (Assistance_dfn_rootless_t.t * Assistance_dfn_rootless_t.t) list ;
} ;;

end;;






module Assistance_fw_poly=struct

(*

#use"Filewatching/fw_poly.ml";;

*)


module Private = struct 

module Crobj = struct 
let salt = "Fw_poly_t." ;;
let label_for_type_name                          = salt ^ "type_name" ;;
let label_for_dir_for_backup                     = salt ^ "dir_for_backup" ;;
let label_for_encoding_protected_files           = salt ^ "encoding_protected_files" ;;
let label_for_github_url                         = salt ^ "github_url" ;;
let label_for_gitpush_after_backup               = salt ^ "gitpush_after_backup" ;;
let label_for_ignored_files                      = salt ^ "ignored_files" ;;
let label_for_ignored_subdirectories             = salt ^ "ignored_subdirectories" ;;
let label_for_last_compilation_result_for_module = salt ^ "last_compilation_result_for_module" ;;
let label_for_root                               = salt ^ "root" ;;
let label_for_small_details_in_files             = salt ^ "small_details_in_files" ;;
let label_for_subdirs_for_archived_mlx_files     = salt ^ "subdirs_for_archived_mlx_files" ;;
let label_for_watched_files                      = salt ^ "watched_files" ;;

let of_concrete_object ccrt_obj = 
 let g=Assistance_concrete_object.get_record ccrt_obj in 
 {
   Assistance_fw_poly_t.type_name = Assistance_crobj_converter.string_of_concrete_object (g label_for_type_name) ;
   dir_for_backup = Assistance_dfa_root.of_concrete_object (g label_for_dir_for_backup)  ;
   encoding_protected_files = Assistance_crobj_converter_combinator.to_pair_list Assistance_dfn_rootless.of_concrete_object Assistance_dfn_rootless.of_concrete_object (g label_for_encoding_protected_files)  ;
   github_url = Assistance_crobj_converter.string_of_concrete_object (g label_for_github_url)  ;
   gitpush_after_backup = Assistance_crobj_converter.bool_of_concrete_object (g label_for_gitpush_after_backup)  ;
   ignored_files = Assistance_crobj_converter_combinator.to_list Assistance_dfn_rootless.of_concrete_object (g label_for_ignored_files)  ;
   ignored_subdirectories = Assistance_crobj_converter_combinator.to_list Assistance_dfa_subdirectory.of_concrete_object (g label_for_ignored_subdirectories)  ;
   index_for_caching = (Assistance_fw_indexer.make_full_instance ()) ;
   last_compilation_result_for_module = Assistance_crobj_converter_combinator.to_pair_list Assistance_dfa_module.of_concrete_object Assistance_crobj_converter.bool_of_concrete_object (g label_for_last_compilation_result_for_module)  ;
   root = Assistance_dfa_root.of_concrete_object (g label_for_root)  ;
   small_details_in_files = Assistance_crobj_converter_combinator.to_pair_list Assistance_dfn_rootless.of_concrete_object Assistance_fw_file_small_details.of_concrete_object (g label_for_small_details_in_files)  ;
   subdirs_for_archived_mlx_files = Assistance_crobj_converter_combinator.to_list Assistance_dfa_subdirectory.of_concrete_object (g label_for_subdirs_for_archived_mlx_files)  ;
   watched_files = Assistance_crobj_converter_combinator.to_pair_list Assistance_dfn_rootless.of_concrete_object Assistance_crobj_converter.string_of_concrete_object (g label_for_watched_files)  ;
} ;;

let to_concrete_object fw = 
 let items =  
 [
     label_for_type_name, Assistance_crobj_converter.string_to_concrete_object fw.Assistance_fw_poly_t.type_name ;
     label_for_dir_for_backup, Assistance_dfa_root.to_concrete_object fw.Assistance_fw_poly_t.dir_for_backup ;
     label_for_encoding_protected_files, Assistance_crobj_converter_combinator.of_pair_list Assistance_dfn_rootless.to_concrete_object Assistance_dfn_rootless.to_concrete_object fw.Assistance_fw_poly_t.encoding_protected_files ;
     label_for_github_url, Assistance_crobj_converter.string_to_concrete_object fw.Assistance_fw_poly_t.github_url ;
     label_for_gitpush_after_backup, Assistance_crobj_converter.bool_to_concrete_object fw.Assistance_fw_poly_t.gitpush_after_backup ;
     label_for_ignored_files, Assistance_crobj_converter_combinator.of_list Assistance_dfn_rootless.to_concrete_object fw.Assistance_fw_poly_t.ignored_files ;
     label_for_ignored_subdirectories, Assistance_crobj_converter_combinator.of_list Assistance_dfa_subdirectory.to_concrete_object fw.Assistance_fw_poly_t.ignored_subdirectories ;
     label_for_last_compilation_result_for_module, Assistance_crobj_converter_combinator.of_pair_list Assistance_dfa_module.to_concrete_object Assistance_crobj_converter.bool_to_concrete_object fw.Assistance_fw_poly_t.last_compilation_result_for_module ;
     label_for_root, Assistance_dfa_root.to_concrete_object fw.Assistance_fw_poly_t.root ;
     label_for_small_details_in_files, Assistance_crobj_converter_combinator.of_pair_list Assistance_dfn_rootless.to_concrete_object Assistance_fw_file_small_details.to_concrete_object fw.Assistance_fw_poly_t.small_details_in_files ;
     label_for_subdirs_for_archived_mlx_files, Assistance_crobj_converter_combinator.of_list Assistance_dfa_subdirectory.to_concrete_object fw.Assistance_fw_poly_t.subdirs_for_archived_mlx_files ;
     label_for_watched_files, Assistance_crobj_converter_combinator.of_pair_list Assistance_dfn_rootless.to_concrete_object Assistance_crobj_converter.string_to_concrete_object fw.Assistance_fw_poly_t.watched_files ;
 ] in 
 Assistance_concrete_object_t.Record items ;;


end;; 




module Extender = struct 

let file_watcher_to_fw_with_archives fw ~subdirs_for_archived_mlx_files:v1_archives_subdirs = {
   fw with 
   Assistance_fw_poly_t.type_name = "Fw_with_archives" ;
   subdirs_for_archived_mlx_files = v1_archives_subdirs ;
} ;;
let fw_configuration_to_file_watcher fw ~watched_files:v1_files = {
   fw with 
   Assistance_fw_poly_t.type_name = "File_watcher" ;
   watched_files = v1_files ;
} ;;
let fw_with_archives_to_fw_with_small_details fw ~small_details_in_files:v1_small_details = {
   fw with 
   Assistance_fw_poly_t.type_name = "Fw_with_small_details" ;
   small_details_in_files = v1_small_details ;
} ;;
let fw_with_batch_compilation_to_fw_with_githubbing fw ~dir_for_backup:v1_backup_dir ~gitpush_after_backup:v2_gab ~github_url:v3_url ~encoding_protected_files:v4_protected_pairs = {
   fw with 
   Assistance_fw_poly_t.type_name = "Fw_with_githubbing" ;
   dir_for_backup = v1_backup_dir ;
   gitpush_after_backup = v2_gab ;
   github_url = v3_url ;
   encoding_protected_files = v4_protected_pairs ;
} ;;
let fw_with_dependencies_to_fw_with_batch_compilation fw ~last_compilation_result_for_module:v1_compilation_results = {
   fw with 
   Assistance_fw_poly_t.type_name = "Fw_with_batch_compilation" ;
   last_compilation_result_for_module = v1_compilation_results ;
} ;;
let fw_with_small_details_to_fw_with_dependencies fw ~index_for_caching:v1_cache_idx = {
   fw with 
   Assistance_fw_poly_t.type_name = "Fw_with_dependencies" ;
   index_for_caching = v1_cache_idx ;
} ;;
end;;

module Parent = struct 
let designated_parents = [
    "Fw_with_archives" , "File_watcher" ;
    "Fw_with_small_details" , "Fw_with_archives" ;
    "Fw_with_dependencies" , "Fw_with_small_details" ;
    "Fw_with_batch_compilation" , "Fw_with_dependencies" ;
    "Fw_with_githubbing" , "Fw_with_batch_compilation" ;
] ;;

exception No_designated_parent of string ;; 
exception Set_parent_exn of string ;; 

let get_parent_name fw = 
 let name = fw.Assistance_fw_poly_t.type_name in 
 match List.assoc_opt name designated_parents with 
  Some(answer) ->answer
 |None -> raise (No_designated_parent(name)) ;;

let sp_for_fw_with_archives child new_parent = 
 Extender.file_watcher_to_fw_with_archives new_parent 
   ~subdirs_for_archived_mlx_files:(child.Assistance_fw_poly_t.subdirs_for_archived_mlx_files)
 ;;
let sp_for_fw_with_small_details child new_parent = 
 Extender.fw_with_archives_to_fw_with_small_details new_parent 
   ~small_details_in_files:(child.Assistance_fw_poly_t.small_details_in_files)
 ;;
let sp_for_fw_with_dependencies child new_parent = 
 Extender.fw_with_small_details_to_fw_with_dependencies new_parent 
   ~index_for_caching:(child.Assistance_fw_poly_t.index_for_caching)
 ;;
let sp_for_fw_with_batch_compilation child new_parent = 
 Extender.fw_with_dependencies_to_fw_with_batch_compilation new_parent 
   ~last_compilation_result_for_module:(child.Assistance_fw_poly_t.last_compilation_result_for_module)
 ;;
let sp_for_fw_with_githubbing child new_parent = 
 Extender.fw_with_batch_compilation_to_fw_with_githubbing new_parent 
   ~dir_for_backup:(child.Assistance_fw_poly_t.dir_for_backup)
   ~gitpush_after_backup:(child.Assistance_fw_poly_t.gitpush_after_backup)
   ~github_url:(child.Assistance_fw_poly_t.github_url)
   ~encoding_protected_files:(child.Assistance_fw_poly_t.encoding_protected_files)
 ;;

let set ~child ~new_parent = 
 let name = child.Assistance_fw_poly_t.type_name in 
 match List.assoc_opt name [
   "Fw_with_archives" , sp_for_fw_with_archives child new_parent ;
   "Fw_with_small_details" , sp_for_fw_with_small_details child new_parent ;
   "Fw_with_dependencies" , sp_for_fw_with_dependencies child new_parent ;
   "Fw_with_batch_compilation" , sp_for_fw_with_batch_compilation child new_parent ;
   "Fw_with_githubbing" , sp_for_fw_with_githubbing child new_parent ;
 ] with 
  Some(answer) ->answer
 |None -> raise (Set_parent_exn(name)) ;;

let get child = 
 let parent_name = get_parent_name child in 
 { child with Assistance_fw_poly_t.type_name = parent_name } ;;

end;; 




let origin = {
   Assistance_fw_poly_t.type_name = "" ;
   dir_for_backup = Assistance_dfa_root.of_line "dummy" ;
   encoding_protected_files = [] ;
   github_url = "" ;
   gitpush_after_backup = false ;
   ignored_files = [] ;
   ignored_subdirectories = [] ;
   index_for_caching = (Assistance_fw_indexer.make_full_instance ()) ;
   last_compilation_result_for_module = [] ;
   root = Assistance_dfa_root.of_line "dummy" ;
   small_details_in_files = [] ;
   subdirs_for_archived_mlx_files = [] ;
   watched_files = [] ;
} ;;

module Type_information = struct 
let fields_for_instances = [
"Fw_configuration" , ["root";"ignored_subdirectories";"ignored_files"];
"File_watcher" , ["root";"ignored_subdirectories";"ignored_files";"watched_files"];
"Fw_with_archives" , ["root";"ignored_subdirectories";"ignored_files";"watched_files";"subdirs_for_archived_mlx_files"];
"Fw_with_small_details" , ["root";"ignored_subdirectories";"ignored_files";"watched_files";"subdirs_for_archived_mlx_files";"small_details_in_files"];
"Fw_with_dependencies" , ["root";"ignored_subdirectories";"ignored_files";"watched_files";"subdirs_for_archived_mlx_files";"small_details_in_files";"index_for_caching"];
"Fw_with_batch_compilation" , ["root";"ignored_subdirectories";"ignored_files";"watched_files";"subdirs_for_archived_mlx_files";"small_details_in_files";"index_for_caching";"last_compilation_result_for_module"];
"Fw_with_githubbing" , ["root";"ignored_subdirectories";"ignored_files";"watched_files";"subdirs_for_archived_mlx_files";"small_details_in_files";"index_for_caching";"last_compilation_result_for_module";"dir_for_backup";"gitpush_after_backup";"github_url";"encoding_protected_files"];
"Github_configuration" , ["root";"dir_for_backup";"gitpush_after_backup";"github_url";"encoding_protected_files"]
] ;;

exception Get_fields_exn of string ;;

let get_fields_from_name tname = 
   try List.assoc tname fields_for_instances with
    _ -> raise(Get_fields_exn(tname)) ;;

let get_fields fw = get_fields_from_name fw.Assistance_fw_poly_t.type_name ;; 

let data_for_fields = [
   "dir_for_backup" , "Dfa_root_t.t";
   "encoding_protected_files" , "(Dfn_rootless_t.t * Dfn_rootless_t.t) list";
   "github_url" , "string";
   "gitpush_after_backup" , "bool";
   "ignored_files" , "Dfn_rootless_t.t list";
   "ignored_subdirectories" , "Dfa_subdirectory_t.t list";
   "index_for_caching" , "Fw_instance_index_t.t * Fw_state_index_t.t";
   "last_compilation_result_for_module" , "(Dfa_module_t.t * bool) list";
   "root" , "Dfa_root_t.t";
   "small_details_in_files" , "(Dfn_rootless_t.t * Fw_file_small_details_t.t) list";
   "subdirs_for_archived_mlx_files" , "Dfa_subdirectory_t.t list";
   "watched_files" , "(Dfn_rootless_t.t * string) list"
] ;;

exception Get_field_data_exn of string ;;

let get_field_data field_name = 
   try (field_name,List.assoc field_name data_for_fields) with
    _ -> raise(Get_field_data_exn(field_name)) ;;

let element_in_show_fields (fd_name,fd_type) = (String.make 3 ' ') ^ fd_name ^ " : " ^ fd_type ;;

let show_fields fw = 
 let fields = get_fields fw in 
 let data = Assistance_image.image get_field_data fields in 
 let msg = " "^ (fw.Assistance_fw_poly_t.type_name) ^ " : {\n" ^ 
 (String.concat "\n" (Assistance_image.image element_in_show_fields data))
 ^ " \n } " in
 print_string ("\n\n"^msg^"\n\n");;

let check_inclusion inst1 inst2 = 
   let cinst1 = String.capitalize_ascii inst1
   and cinst2 = String.capitalize_ascii inst2 in
   let fields1 = get_fields_from_name cinst1
   and fields2 = get_fields_from_name cinst2 in
   let nonincluded_fields = List.filter (fun x->not(List.mem x fields2)) fields1 in
   let n = List.length nonincluded_fields in
   if n = 0
   then true
   else
   let (left,right)= (if n>1 then ("s"," are ") else (""," is ") ) in
   let fld_list = String.concat " , " nonincluded_fields in
   let msg = " The field " ^ left ^ fld_list ^ right ^ " in " ^
   cinst1 ^ "but not in " ^
   cinst2 ^ "." in
   let _ = print_string ("\n\n"^msg^"\n\n") in
   false ;;
exception Check_inclusion_exn ;;

let check_inclusion_forcefully inst1 inst2 = 
   if (not(check_inclusion inst1 inst2)) then raise(Check_inclusion_exn) ;;

end;; 





end;; 




let construct_fw_configuration ~root:v1_r ~ignored_subdirectories:v2_ign_subdirs ~ignored_files:v3_ign_files = {
   Private.origin with 
   Assistance_fw_poly_t.type_name = "Fw_configuration" ;
   root = v1_r ;
   ignored_subdirectories = v2_ign_subdirs ;
   ignored_files = v3_ign_files ;
} ;;
let construct_github_configuration ~root:v1_r ~dir_for_backup:v2_backup_dir ~gitpush_after_backup:v3_gab ~github_url:v4_url ~encoding_protected_files:v5_protected_pairs = {
   Private.origin with 
   Assistance_fw_poly_t.type_name = "Github_configuration" ;
   root = v1_r ;
   dir_for_backup = v2_backup_dir ;
   gitpush_after_backup = v3_gab ;
   github_url = v4_url ;
   encoding_protected_files = v5_protected_pairs ;
} ;;
let dir_for_backup x = x.Assistance_fw_poly_t.dir_for_backup ;;
let encoding_protected_files x = x.Assistance_fw_poly_t.encoding_protected_files ;;
let extend_file_watcher_to_fw_with_archives  = Private.Extender.file_watcher_to_fw_with_archives ;;
let extend_fw_configuration_to_file_watcher  = Private.Extender.fw_configuration_to_file_watcher ;;
let extend_fw_with_archives_to_fw_with_small_details  = Private.Extender.fw_with_archives_to_fw_with_small_details ;;
let extend_fw_with_batch_compilation_to_fw_with_githubbing  = Private.Extender.fw_with_batch_compilation_to_fw_with_githubbing ;;
let extend_fw_with_dependencies_to_fw_with_batch_compilation  = Private.Extender.fw_with_dependencies_to_fw_with_batch_compilation ;;
let extend_fw_with_small_details_to_fw_with_dependencies  = Private.Extender.fw_with_small_details_to_fw_with_dependencies ;;
let github_url x = x.Assistance_fw_poly_t.github_url ;;
let gitpush_after_backup x = x.Assistance_fw_poly_t.gitpush_after_backup ;;
let ignored_files x = x.Assistance_fw_poly_t.ignored_files ;;
let ignored_subdirectories x = x.Assistance_fw_poly_t.ignored_subdirectories ;;
let index_for_caching x = x.Assistance_fw_poly_t.index_for_caching ;;
let last_compilation_result_for_module x = x.Assistance_fw_poly_t.last_compilation_result_for_module ;;
let of_concrete_object = Private.Crobj.of_concrete_object ;;
let parent  = Private.Parent.get ;;
let print_out (fmt:Format.formatter) fw  = Format.fprintf fmt "@[%s@]" ("< "^(fw.Assistance_fw_poly_t.type_name)^" >") ;;
let root x = x.Assistance_fw_poly_t.root ;;
let set_dir_for_backup x backup_dir = { x with Assistance_fw_poly_t.dir_for_backup = backup_dir} ;;
let set_encoding_protected_files x protected_pairs = { x with Assistance_fw_poly_t.encoding_protected_files = protected_pairs} ;;
let set_github_url x url = { x with Assistance_fw_poly_t.github_url = url} ;;
let set_gitpush_after_backup x gab = { x with Assistance_fw_poly_t.gitpush_after_backup = gab} ;;
let set_ignored_files x ign_files = { x with Assistance_fw_poly_t.ignored_files = ign_files} ;;
let set_ignored_subdirectories x ign_subdirs = { x with Assistance_fw_poly_t.ignored_subdirectories = ign_subdirs} ;;
let set_index_for_caching x cache_idx = { x with Assistance_fw_poly_t.index_for_caching = cache_idx} ;;
let set_last_compilation_result_for_module x compilation_results = { x with Assistance_fw_poly_t.last_compilation_result_for_module = compilation_results} ;;
let set_parent  = Private.Parent.set ;;
let set_root x r = { x with Assistance_fw_poly_t.root = r} ;;
let set_small_details_in_files x small_details = { x with Assistance_fw_poly_t.small_details_in_files = small_details} ;;
let set_subdirs_for_archived_mlx_files x archives_subdirs = { x with Assistance_fw_poly_t.subdirs_for_archived_mlx_files = archives_subdirs} ;;
let set_watched_files x files = { x with Assistance_fw_poly_t.watched_files = files} ;;
let show_fields  = Private.Type_information.show_fields ;;
let small_details_in_files x = x.Assistance_fw_poly_t.small_details_in_files ;;
let subdirs_for_archived_mlx_files x = x.Assistance_fw_poly_t.subdirs_for_archived_mlx_files ;;
let to_concrete_object = Private.Crobj.to_concrete_object ;;
let to_fw_configuration fw  = 
  let tname = fw.Assistance_fw_poly_t.type_name in 
  let _ = Private.Type_information.check_inclusion "fw_configuration" tname in 
   {
   fw with 
   Assistance_fw_poly_t.type_name = "Fw_configuration" ;
} ;;
let to_github_configuration fw  = 
  let tname = fw.Assistance_fw_poly_t.type_name in 
  let _ = Private.Type_information.check_inclusion "github_configuration" tname in 
   {
   fw with 
   Assistance_fw_poly_t.type_name = "Github_configuration" ;
} ;;
let watched_files x = x.Assistance_fw_poly_t.watched_files ;;

end;;






module Assistance_compact_replacer_t=struct

(*

#use"Text_editing/compact_replacer_t.ml";;

*)

type t= CR of (string*string) list ;;


end;;






module Assistance_compact_replacer=struct

(*

#use"Text_editing/compact_replacer.ml";;

*)


let separator = " \205\140 ";;

let unparse (Assistance_compact_replacer_t.CR(l))=
   let temp1 = List.flatten (Assistance_image.image (fun (x,y)->[x;y]) l)  in 
   String.concat separator temp1 ;;

let parse descr =
   let temp1 = Str.split (Str.regexp_string separator) descr in 
   let m = (List.length temp1)/2 in
   let tg =(fun k->List.nth temp1 (k-1)) in  
   Assistance_compact_replacer_t.CR(Assistance_int_range.scale (fun j->(tg (2*j-1),tg (2*j)) ) 1 m );;

let replace_inside_string (Assistance_compact_replacer_t.CR(l)) old_text =
   Assistance_replace_inside.replace_several_inside_string l old_text ;;

let replace_inside_file (Assistance_compact_replacer_t.CR(l)) fn =
   Assistance_replace_inside.replace_several_inside_file l fn ;;

let reverse_replace_inside_string (Assistance_compact_replacer_t.CR(old_l)) old_text =
   let l = Assistance_image.image (fun (x,y)->(y,x)) old_l in 
   Assistance_replace_inside.replace_several_inside_string l old_text ;;

let reverse_replace_inside_file (Assistance_compact_replacer_t.CR(old_l)) fn =
   let l = Assistance_image.image (fun (x,y)->(y,x)) old_l in 
   Assistance_replace_inside.replace_several_inside_file l fn ;;

let execute s=
   let temp1 = Str.split (Str.regexp "[ \t]+") s in 
   let temp2 = Assistance_image.image Assistance_absolute_path.of_string  temp1 in 
   let replacements = Assistance_io.read_whole_file (List.nth temp2 0) 
   and recipient = (List.nth temp2 1) in 
   replace_inside_file (parse replacements) recipient ;;

let reverse_execute s=
   let temp1 = Str.split (Str.regexp "[ \t]+") s in 
   let temp2 = Assistance_image.image Assistance_absolute_path.of_string  temp1 in 
   let replacements = Assistance_io.read_whole_file (List.nth temp2 0) 
   and recipient = (List.nth temp2 1) in 
   reverse_replace_inside_file (parse replacements) recipient ;;   

(*

let z1 =  Compact_replacer_t.CR(["abc","def";"12","34"]) ;;  
let z2 = unparse z1;;
let z3 = parse z2;;
let check = (z3=z1);;

*)

end;;






module Assistance_unix_command=struct

(*

Wrapper on the Sys dot command function.

#use"unix_command.ml";;

*)


exception Command_failed of string;;
exception Command_failed_just_now ;;

module Private = struct

let prefix_for_changing_directories         = "cd ";;
let prefix_for_replacing_patterns           = "rp ";;
let prefix_for_reverse_replacing_patterns   = "rvp ";;

let accu=ref([]:string list);;
let remember_commands_mode=ref(false);;
let hardcore_mode=ref(false);;

let command cmd=
   let cd_prefix =prefix_for_changing_directories 
   and rp_prefix =prefix_for_replacing_patterns 
   and rvp_prefix =prefix_for_reverse_replacing_patterns in 
   if Assistance_supstring.begins_with cmd cd_prefix 
   then let  _=Sys.chdir(Assistance_cull_string.cobeginning (String.length cd_prefix) cmd) in 0
   else 
   if Assistance_supstring.begins_with cmd rp_prefix 
   then let  _= Assistance_compact_replacer.execute(Assistance_cull_string.cobeginning (String.length rp_prefix) cmd) in 0 
   else 
   if Assistance_supstring.begins_with cmd rvp_prefix 
   then let  _= Assistance_compact_replacer.reverse_execute(Assistance_cull_string.cobeginning (String.length rvp_prefix) cmd) in 0 
   else 
   Sys.command cmd;;


let mild_uc s=
   let i=command s in
   let _=(
   if i<>0
   then (print_string ("Failed during "^s^"\n");flush stdout)
   else (if (!remember_commands_mode) 
               then accu:=s::(!accu))
   ) in
   i;;

let hardcore_uc s=
   let i=command s in
   if i<>0
   then raise(Command_failed(s))
   else let _=(if (!remember_commands_mode) 
               then accu:=s::(!accu)) in 
        i;;

let uc s=
   if (!hardcore_mode)
   then hardcore_uc s
   else mild_uc s;;

let debug_individual_uc (j,s) =
    let msg = "Trying command number "^(string_of_int j)^" : "^s^"\n" in 
    let _=(print_string msg;flush stdout) in
    let i = command s in 
    if i<>0 
    then raise(Command_failed_just_now)
    else (print_string "Successful.\n";flush stdout);;

let rec helper_for_debug_multiple_uc (j,l)=
   match l with 
   [] -> () 
   |cmd :: other_cmds ->
     let _ = debug_individual_uc (j,cmd) in 
     helper_for_debug_multiple_uc (j+1,other_cmds) ;; 


end;;

let cd dirname = (Private.prefix_for_changing_directories)^dirname;;

let rec conditional_multiple_uc commands=match commands with
  []->true
  |cmd1::other_commands ->
    if (Private.uc cmd1)=0
    then conditional_multiple_uc other_commands 
    else false;;
           
let debug_multiple_uc l = Private.helper_for_debug_multiple_uc (1,l);;           

let hardcore_uc = Private.hardcore_uc ;;

let mv full_path new_location =
   let destination_equals_source=(
     if not(Assistance_supstring.begins_with full_path new_location) then false else 
     let naked_name=Assistance_cull_string.two_sided_cutting (new_location,"") full_path in 
     not(String.contains naked_name '/') 
   ) in 
   if destination_equals_source 
   then None 
   else Some("mv "^full_path^" "^new_location);;

let prefix_for_replacing_patterns           = Private.prefix_for_replacing_patterns;;
let prefix_for_reverse_replacing_patterns   = Private.prefix_for_reverse_replacing_patterns;;


let uc = Private.uc;;



           

end;;






module Assistance_transmit_change_to_github=struct

(* 

#use"Githubbing/transmit_change_to_github.ml";;

*)

module Private = struct

let commands_for_backup config diff=
   if Assistance_dircopy_diff.is_empty diff
   then ([],[])
   else 
   let source_dir = Assistance_fw_poly.root config 
   and destination_dir = Assistance_fw_poly.dir_for_backup config in 
   let s_destination=Assistance_dfa_root.connectable_to_subpath destination_dir in
   let created_ones=Assistance_image.image Assistance_dfn_rootless.to_line (Assistance_dircopy_diff.recently_created diff) in
   let temp2=Assistance_option.filter_and_unpack
   (fun fn->
     if String.contains fn '/'
     then let dn=Assistance_cull_string.before_rightmost fn '/' in
          Some("mkdir -p "^s_destination^dn)
     else None 
    ) created_ones in
   let temp3=Assistance_ordered.sort Assistance_total_ordering.silex_for_strings temp2 in
   let s_source=Assistance_dfa_root.connectable_to_subpath source_dir in
   let temp4=Assistance_image.image(
      fun fn->
      "cp "^s_source^fn^" "^s_destination^(Assistance_cull_string.before_rightmost fn '/')
   ) created_ones in
   let changed_ones=Assistance_image.image Assistance_dfn_rootless.to_line (Assistance_dircopy_diff.recently_changed diff) in
   let temp5=Assistance_image.image(
      fun fn->
      "cp "^s_source^fn^" "^s_destination^fn
   ) changed_ones in
   let temp6=Assistance_image.image(
      fun fn->
      "git -C "^s_destination^" add "^fn
   ) (created_ones@changed_ones) in
   let temp7=Assistance_image.image(
      fun rl->
      let fn = Assistance_dfn_rootless.to_line rl in 
      "git -C "^s_destination^" rm "^fn
   ) (Assistance_dircopy_diff.recently_deleted diff) in
   let temp8= Assistance_image.image (
     fun (replacer,replacee) ->
       let s_replacer = Assistance_dfn_rootless.to_line  replacer 
       and s_backup_dir = Assistance_dfa_root.connectable_to_subpath destination_dir in 
       let s_full_path = s_backup_dir^(Assistance_dfn_rootless.to_line replacee) in 
       Assistance_unix_command.prefix_for_replacing_patterns^s_replacer^" "^s_full_path
   ) (Assistance_fw_poly.encoding_protected_files config) in 
   (temp3@temp4@temp5@temp8,temp6@temp7);;

let backup_with_message config  diff msg=
  let destination_dir = Assistance_fw_poly.dir_for_backup config in 
  let (nongit_cmds,git_cmds)=commands_for_backup config diff in
  let s_destination=Assistance_dfa_root.connectable_to_subpath destination_dir in
  let _=Assistance_image.image Assistance_unix_command.uc nongit_cmds in
  let _=(
  if Assistance_fw_poly.gitpush_after_backup config
  then let cwd=Sys.getcwd() in
       Assistance_image.image Assistance_unix_command.uc
       (
       [Assistance_unix_command.cd s_destination]@   
       git_cmds@   
       [
         "git -C "^s_destination^" commit -m \""^msg^"\"";
         "git -C "^s_destination^" push"
       ]@
       [Assistance_unix_command.cd cwd]
       ) 
  else let cwd=Sys.getcwd() in
       Assistance_image.image Assistance_unix_command.uc
       (
       [Assistance_unix_command.cd s_destination]@   
       git_cmds@   
       [
         "git -C "^s_destination^" commit -m \""^msg^"\""
       ]@
       [Assistance_unix_command.cd cwd]
       ) 
  ) in
  ();;

let backup config diff opt_msg=
  if Assistance_dircopy_diff.is_empty diff
  then (print_string "No recent changes to commit ...";flush stdout) 
  else 
  let msg=(
   match opt_msg with
    None->Assistance_dircopy_diff.explain diff
   |Some(msg0)->msg0) in
  backup_with_message config diff msg;;
  
end ;; 

let backup config diff opt_msg=
  Private.backup config diff opt_msg;;

  
  

end;;






module Assistance_cartesian=struct

(*

#use"cartesian.ml";;

*) 

let product a b=
if (a=[])||(b=[]) then [] else
let rec sub_f=(function
(accu,variable_a,constant_b)->match variable_a with
[]->List.rev(accu)
|u::v->sub_f(List.rev_append(List.rev(List.rev_map(function t->(u,t))(constant_b)))
(accu),v,constant_b)
) in
sub_f([],a,b);;

let square x=product x x;;

let tproduct a b c=List.rev_map(function ((x,y),z)->(x,y,z))
(List.rev(product(product(a)(b))(c)));;

let pproduct a b c d=List.rev_map(function ((x,y,z),t)->(x,y,z,t))
(List.rev(product(tproduct a b c)(d)));;

let qproduct a b c d e=List.rev_map(function ((x,y,z,t),u)->(x,y,z,t,u))
(List.rev(product(pproduct a b c d)(e)));;

let cube x=tproduct x x x;;

let fourth_power x=pproduct x x x x;;

let fifth_power x=qproduct x x x x x;;

let general_product x=
let rec sub_f=(function
([],accu)->accu
|(a::b,accu)->sub_f(b,List.rev_map(function (x,l)->x::l)(List.rev(product(a)(accu)))))
in
sub_f(List.rev(x),[[]]);;

let power x n=general_product (Assistance_int_range.scale (fun j->x) 1 n);;
             

end;;






module Assistance_ordered_misc=struct

(* 

#use"Ordered_Lists/ordered_misc.ml";;

*)

module Private = struct 

  let oi = Assistance_total_ordering.for_integers ;;

  let rec helper_for_maximal_elts_wrt_inclusion (already_treated,to_be_treated) =
    match to_be_treated with 
    [] -> List.rev already_treated 
    | a :: others ->
       if List.exists (fun b->
         (b<>a) && ( Assistance_ordered.is_included_in oi a b) ) others
       then helper_for_maximal_elts_wrt_inclusion (already_treated,others)
       else 
        let temp1 = List.filter (fun b->
           not(Assistance_ordered.is_included_in oi b a)) others in
        helper_for_maximal_elts_wrt_inclusion (a :: already_treated,temp1) ;;  

  let rec helper_for_minimal_elts_wrt_inclusion (already_treated,to_be_treated) =
    match to_be_treated with 
    [] -> List.rev already_treated 
    | a :: others ->
       if List.exists (fun b->
         (b<>a) && ( Assistance_ordered.is_included_in oi b a) ) others
       then helper_for_minimal_elts_wrt_inclusion (already_treated,others)
       else 
        let temp1 = List.filter (fun b->
           not(Assistance_ordered.is_included_in oi a b)) others in
        helper_for_minimal_elts_wrt_inclusion (a :: already_treated,temp1) ;;  

  let rec helper_for_minimal_transversals (already_treated,to_be_treated) =
    match to_be_treated with 
    [] -> List.rev already_treated 
    | a :: others ->
      let temp1 = Assistance_cartesian.product a already_treated in 
      let temp2 = Assistance_image.image (fun (x,y)->Assistance_ordered.insert oi x y) temp1 in 
      let temp3 = helper_for_minimal_elts_wrt_inclusion ([],temp2) in 
      helper_for_minimal_transversals (temp3,others) ;;        

  let power_set_for_intset x =
     Assistance_ordered.sort (Assistance_total_ordering.silex_compare oi) (Assistance_listennou.power_set x) ;; 

  let rec helper_for_int_upwards_filter (f,treated,to_be_treated) = 
     match to_be_treated with 
     [] -> List.rev treated 
     | a::others ->
         if f a 
         then let remaining_ones = List.filter (
                fun b->not(Assistance_ordered.is_included_in oi a b)
              ) to_be_treated in 
              helper_for_int_upwards_filter (f,a::treated,remaining_ones)
         else helper_for_int_upwards_filter (f,treated,others) ;;    

  let naive_minimal_elts_in_int_upwards_filter f base =
    let pb = power_set_for_intset base in 
    helper_for_int_upwards_filter (f,[],pb) ;;     

  let minimal_elts_in_int_upwards_filter f base =
     let indispensable_ones = List.filter (
       fun x->(not(f(Assistance_ordered.outsert oi x base)))
     ) base in 
     let dispensable_ones = Assistance_ordered.setminus oi base indispensable_ones in 
     let pullbacked_f = (fun y->f(Assistance_ordered.merge oi indispensable_ones y)) in 
     let temp1 = naive_minimal_elts_in_int_upwards_filter pullbacked_f dispensable_ones in 
     Assistance_image.image (Assistance_ordered.merge oi indispensable_ones) temp1 ;;

  (*   
  let base0 = Ennig.ennig 1 5 ;;
  let f0 y= List.exists (fun x->Ordered.is_included_in oi x y) [[1;2;3];[3;4;5]] ;;
  let res0 = minimal_elts_in_int_upwards_filter f0 base0 ;;
  *)

  let minimal_elts_in_upwards_filter f base =
     let get_one = (fun k->List.nth base (k-1)) in 
     let get_several = Assistance_image.image get_one in 
     let n = List.length base 
     and normalized_f = (fun indices ->f(get_several indices)) in 
     let temp1 = minimal_elts_in_int_upwards_filter normalized_f (Assistance_int_range.range 1 n) in 
     Assistance_image.image get_several temp1 ;;

let commonest_elements ll = 
  let temp1 = Assistance_ordered.fold_merge oi ll in 
  let (_,temp2) = Assistance_max.maximize_it_with_care (
    fun y->List.length(List.filter (Assistance_ordered.mem oi y) ll)
  ) temp1 in 
  temp2;; 

let rec helper_for_greedy_transversal (history,treated,to_be_treated) =
   if to_be_treated = [] 
   then (List.rev history,treated)
   else
   let temp1 = commonest_elements to_be_treated in 
   let a = List.hd temp1 in 
   helper_for_greedy_transversal(temp1::history,Assistance_ordered.insert oi a treated,
     List.filter (fun x->not(Assistance_ordered.mem oi a x)) to_be_treated) ;;  

 let greedy_transversal ll =
  helper_for_greedy_transversal ([],[],ll) ;;
   
end ;;        


let commonest_elements = Private.commonest_elements ;;

let greedy_transversal = Private.greedy_transversal ;;

let maximal_elts_wrt_inclusion l= 
  Private.helper_for_maximal_elts_wrt_inclusion ([],l) ;;

let minimal_elts_in_upwards_filter = Private.minimal_elts_in_upwards_filter ;;

let minimal_elts_wrt_inclusion l= 
  Private.helper_for_minimal_elts_wrt_inclusion ([],l) ;;

  
let minimal_transversals l= 
  match l with 
  [] -> []
  | a:: others ->
    let starter = Assistance_image.image (fun x->[x]) a in    
  Private.helper_for_minimal_transversals (starter,others) ;;  
  

let reorder_list_of_pairs_using_list_of_singles pairs singles =
  let idx=Assistance_memoized.make(fun x->
     Assistance_listennou.find_index x singles   
  ) in 
  let ordr = (fun x1 x2 -> Assistance_total_ordering.for_integers (idx x1) (idx x2)) in 
  let ordr2 = Assistance_total_ordering.product ordr Assistance_total_ordering.standard in 
  Assistance_ordered.sort ordr2 pairs ;;

(*

reorder_list_of_pairs_using_list_of_singles (Ennig.doyle( fun t->(t,t+100)) 1 8)
[2;5;3;6;4;7;1;8] ;;
*)


let translate_at_level_two ll translation=
  Assistance_image.image (
    fun l->Assistance_ordered.merge Assistance_total_ordering.for_integers l translation
  ) ll ;;


let underline_new_elements ord old_set possibly_new_elts =
  let new_elts = Assistance_ordered.setminus ord possibly_new_elts old_set in 
  let new_whole = Assistance_ordered.merge ord old_set new_elts in 
  let temp1 = Assistance_image.image (fun x->(x,List.mem x new_elts)) new_whole in 
  let temp2 = Assistance_hurried.connected_components snd temp1 in 
  Assistance_image.image (fun part -> (Assistance_image.image fst part,snd(List.hd part)) ) temp2;;

(*

underline_new_elements Total_ordering.standard 
 [1; 2;  5; 7; 8;  11; 13; 14; 16; 17; 19; 20]
 [3;4; 6; 9;10; 12; 15; 18; 21] ;;

*)




end;;






module Assistance_dfa_ocaml_ending_t=struct

(*

#use"Decomposed_filename/dfa_ocaml_ending_t.ml";;

*)

type t=Ml |Mli |Mll |Mly;;



end;;






module Assistance_dfn_full_t=struct

(*

#use"Decomposed_filename/dfn_full_t.ml";;

*)


type t = J of Assistance_dfa_root_t.t * Assistance_dfa_subdirectory_t.t * Assistance_dfa_module_t.t * Assistance_dfa_ending_t.t;;



end;;






module Assistance_dfn_endingless_t=struct

(*

#use"Decomposed_filename/dfn_endingless_t.ml";;

*)

type t = J of Assistance_dfa_root_t.t * Assistance_dfa_subdirectory_t.t * Assistance_dfa_module_t.t  ;;



end;;






module Assistance_dfn_short_t=struct

(*

#use"Decomposed_filename/dfn_short_t.ml";;

*)

type t = J of Assistance_dfa_module_t.t * Assistance_dfa_ending_t.t;;
          



end;;






module Assistance_dfn_join=struct

(*

#use"Decomposed_filename/dfn_join.ml";;

*)

let middle_to_ending (Assistance_dfn_middle_t.J(s,m)) e= (Assistance_dfn_rootless_t.J(s,m,e));;
let root_to_middle r (Assistance_dfn_middle_t.J(s,m))=Assistance_dfn_endingless_t.J(r,s,m);; 
let root_to_rootless r (Assistance_dfn_rootless_t.J(s,m,e))=Assistance_dfn_full_t.J(r,s,m,e);;
let root_to_subdirectory (Assistance_dfa_root_t.R(r)) (Assistance_dfa_subdirectory_t.SD(s))=Assistance_dfa_root_t.R(r^"/"^s);;
let subdirectory_to_module s m=(Assistance_dfn_middle_t.J(s,m));;  
let subdirectory_to_short s (Assistance_dfn_short_t.J(m,e))=(Assistance_dfn_rootless_t.J(s,m,e));;  
let to_ending (Assistance_dfn_endingless_t.J(r,s,m)) e = Assistance_dfn_full_t.J(r,s,m,e);;



end;;






module Assistance_dfn_short=struct

(*

#use"Decomposed_filename/dfn_short.ml";;

*)

exception Of_line_exn of string;;

let of_line line = 
   let (mn,e)=Assistance_cull_string.split_wrt_rightmost line '.' in 
   if mn=""
   then raise(Of_line_exn(line))
   else Assistance_dfn_short_t.J(Assistance_dfa_module.of_line mn,Assistance_dfa_ending.of_line(e));;


let to_line (Assistance_dfn_short_t.J(m,e))=
   (Assistance_dfa_module.to_line m)^(Assistance_dfa_ending.connectable_to_modulename e);;

let to_module (Assistance_dfn_short_t.J(m,e))=m;;

end;;






module Assistance_dfn_full=struct

(*

#use"Decomposed_filename/dfn_full.ml";;

*)


let to_subdirectory  (Assistance_dfn_full_t.J(r,s,m,e))=s;;
let to_module  (Assistance_dfn_full_t.J(r,s,m,e))=m;;
let to_ending (Assistance_dfn_full_t.J(r,s,m,e))=e;;

let to_rootless (Assistance_dfn_full_t.J(r,s,m,e))=(Assistance_dfn_rootless_t.J(s,m,e));; 
let to_endingless (Assistance_dfn_full_t.J(r,s,m,e))=(Assistance_dfn_endingless_t.J(r,s,m));; 

   
let to_rootless_line (Assistance_dfn_full_t.J(r,s,m,e))=
    (Assistance_dfa_subdirectory.connectable_to_subpath s)^
   (Assistance_dfa_module.to_line m)^(Assistance_dfa_ending.connectable_to_modulename e);;

let to_line (Assistance_dfn_full_t.J(r,s,m,e))=
   (Assistance_dfa_root.connectable_to_subpath r)^
   (Assistance_dfa_subdirectory.connectable_to_subpath s)^
   (Assistance_dfa_module.to_line m)^(Assistance_dfa_ending.connectable_to_modulename e);;

let to_absolute_path mlx=Assistance_absolute_path.of_string(to_line mlx);;  

let from_absolute_path_with_root ap dir=
  let rless = Assistance_dfn_common.decompose_absolute_path_using_root ap dir in 
  Assistance_dfn_join.root_to_rootless dir rless;;

  
let relocate (Assistance_dfn_full_t.J(r,old_subdir,m,e)) new_subdir=
  (Assistance_dfn_full_t.J(r,new_subdir,m,e));;  
 




end;;






module Assistance_dfn_endingless=struct

(*

#use"Decomposed_filename/dfn_endingless.ml";;


*)

let begins_with (Assistance_dfn_endingless_t.J(r,s,m)) subdir=
   Assistance_dfa_subdirectory.begins_with s subdir;;

let to_root (Assistance_dfn_endingless_t.J(r,s,m))=r;;
let to_subdirectory  (Assistance_dfn_endingless_t.J(r,s,m))=s;;
let to_module  (Assistance_dfn_endingless_t.J(r,s,m))=m;;
   
let to_line (Assistance_dfn_endingless_t.J(r,s,m))=
   (Assistance_dfa_root.connectable_to_subpath r)^
   (Assistance_dfa_subdirectory.connectable_to_subpath s)^
   (Assistance_dfa_module.to_line m);;

let to_middle (Assistance_dfn_endingless_t.J(r,s,m)) = Assistance_dfn_middle_t.J(s,m) ;;

let rename_endsubdirectory 
   (old_subdir,new_subdirname) 
      (Assistance_dfn_endingless_t.J(r,s,m))=
   Assistance_dfn_endingless_t.J(
	      r,
   		(Assistance_dfa_subdirectory.rename_endsubdirectory (old_subdir,new_subdirname) s),
         m
	    );;  

let rename_module
   (old_module,new_module) 
      (Assistance_dfn_endingless_t.J(r,s,m))=
        if m =old_module 
        then Assistance_dfn_endingless_t.J(r,s,new_module) 
        else Assistance_dfn_endingless_t.J(r,s,m) ;;     

let replace_subdirectory (old_subdir,new_subdir) eless = 
    match eless with  
      (Assistance_dfn_endingless_t.J(r,s,m)) -> 
       if s <> old_subdir 
       then eless 
       else 
   Assistance_dfn_endingless_t.J(r,new_subdir,m);;  
   
let soak (old_subdir,new_subdir) eless =
   let (Assistance_dfn_endingless_t.J(r,s,m)) = eless in 
   match Assistance_dfa_subdirectory.soak (old_subdir,new_subdir) s with 
   Some(new_s)->Some(Assistance_dfn_endingless_t.J(r,new_s,m))
   |None -> None ;;


let to_concrete_object (Assistance_dfn_endingless_t.J(r,s,m))=
   Assistance_concrete_object_t.Variant("Dfn_"^"endingless.J",
     [
        Assistance_dfa_root.to_concrete_object r;
        Assistance_dfa_subdirectory.to_concrete_object s;
        Assistance_dfa_module.to_concrete_object m;
     ]
   ) ;;

let of_concrete_object crobj =
   let (_,(arg1,arg2,arg3,_,_,_,_))=Assistance_concrete_object.unwrap_bounded_variant crobj in 
   Assistance_dfn_endingless_t.J(
      Assistance_dfa_root.of_concrete_object arg1,
      Assistance_dfa_subdirectory.of_concrete_object arg2,
      Assistance_dfa_module.of_concrete_object arg3
   );;
    





end;;






module Assistance_dfn_middle=struct

(*

#use"Decomposed_filename/dfn_middle.ml";;

*)

module Private = struct 

  let of_concrete_object crobj =
     let (_,(arg1,arg2,_,_,_,_,_))=Assistance_concrete_object.unwrap_bounded_variant crobj in 
     Assistance_dfn_middle_t.J(
        Assistance_dfa_subdirectory.of_concrete_object arg1,
        Assistance_dfa_module.of_concrete_object arg2
     );;
  
  let to_concrete_object (Assistance_dfn_middle_t.J(s,m))=
     Assistance_concrete_object_t.Variant("Dfn_"^"middle_t.J",
       [
         Assistance_dfa_subdirectory.to_concrete_object s;
         Assistance_dfa_module.to_concrete_object m;
       ]
     ) ;;
end ;; 
  
let of_concrete_object = Private.of_concrete_object ;; 
let rename_endsubdirectory 
   (old_subdir,new_subdirname) 
      (Assistance_dfn_middle_t.J(s,m))=
   Assistance_dfn_middle_t.J(
   		(Assistance_dfa_subdirectory.rename_endsubdirectory (old_subdir,new_subdirname) s),
         m
	    );; 

let rename_module (m1,m2) middle =
  let (Assistance_dfn_middle_t.J(s,m)) = middle in 
  if m = m1 
  then Assistance_dfn_middle_t.J(s,m2)
  else middle;;       
let to_concrete_object = Private.to_concrete_object ;;   
let to_line (Assistance_dfn_middle_t.J(s,m)) = (Assistance_dfa_subdirectory.connectable_to_subpath s)^ (Assistance_dfa_module.to_line m);;
let to_module (Assistance_dfn_middle_t.J(s,m))=m;;

end;;






module Assistance_dfa_ocaml_ending=struct

(*

#use"Decomposed_filename/dfa_ocaml_ending.ml";;

*)




exception Not_an_ocaml_ending of string;;

module Private = struct

let to_string = function 
   Assistance_dfa_ocaml_ending_t.Mll ->  "mll"
  | Assistance_dfa_ocaml_ending_t.Mly -> "mly"
  | Assistance_dfa_ocaml_ending_t.Ml ->  "ml"
  | Assistance_dfa_ocaml_ending_t.Mli ->  "mli" ;; 


let capitalized_correspondances = Assistance_image.image (fun cml_edg->
    (cml_edg,to_string cml_edg)
   ) [
   Assistance_dfa_ocaml_ending_t.Mll ;  
   Assistance_dfa_ocaml_ending_t.Mly ; 
   Assistance_dfa_ocaml_ending_t.Ml  ; 
   Assistance_dfa_ocaml_ending_t.Mli 
 ];;

end ;;

let all = Assistance_image.image fst Private.capitalized_correspondances ;;

let of_ending (Assistance_dfa_ending_t.E(e)) = 
   match Assistance_option.seek (fun (cml_edg,e2)->e2=e) Private.capitalized_correspondances with 
   None -> raise(Not_an_ocaml_ending(e)) 
   |(Some(cml_edg,_)) -> cml_edg ;; 
   
   
let of_concrete_object =Assistance_concrete_object.unwrap_lonely_variant 
   Private.capitalized_correspondances;;
           
let to_concrete_object =Assistance_concrete_object.wrap_lonely_variant 
   Private.capitalized_correspondances;;    
 
let to_ending cml_edg = Assistance_dfa_ending_t.E(Private.to_string cml_edg) ;;   

end;;






module Assistance_chronometer=struct

(*

#use"chronometer.ml";;

*) 

let rewrite_days=function
0->""
|1->"1 day,"
|x->string_of_int(x)^" days,";;

let rewrite_hours=function
0->""
|1->"1 hour,"
|x->string_of_int(x)^" hours,";;

let rewrite_minutes=function
0->""
|1->"1 minute,"
|x->string_of_int(x)^" minutes,";;

let rewrite_seconds=function
0->""
|1->"1 second."
|x->string_of_int(x)^" seconds.";;

let rewrite_float x=
   let i=int_of_float(x) in
   let v_sec=(i mod 60) and q_sec=(i/60) in
   let v_min=(q_sec mod 60) and q_min=(q_sec/60) in
   let v_hour=(q_min mod 24) and q_hour=(q_min/24) in
   let s_day=rewrite_days(q_hour)
   and s_hour=rewrite_hours(v_hour)
   and s_min=rewrite_minutes(v_min)
   and s_sec=rewrite_seconds(v_sec) in
   s_day^s_hour^s_min^s_sec;;
  
let rewrite_duration x=
   if x=0. 
   then "Computation was quick.\n"
   else "Computation lasted "^(rewrite_float x)^"\n";;

 let timer=ref(0.000);;  
 
 let duration_of_computation f x=
   let t0=Unix.time() in
   let _=f(x) in
   let _=(timer:=Unix.time()-.t0) in
   (print_string(rewrite_duration (!timer));flush stdout);;
 
 let duration_of_last_computation ()=
  (print_string(rewrite_duration (!timer));flush stdout);;
   
   
 let  it f x=
  let t0=Unix.time() in
   let y=f(x) in
   let _=(timer:=Unix.time()-.t0) in
   let _=(print_string(rewrite_duration (!timer));flush stdout) in
   y;;
 
   
           

end;;






module Assistance_coma_big_constant=struct

(* 
#use"Compilation_management/coma_big_constant.ml";;
*)

let github_url = "https://github.com/ewan-delanoy/skeptical_duck";;
let home = Sys.getenv "HOME" ;;

module This_World=struct

let root=Assistance_dfa_root.of_line (home^"/Teuliou/OCaml/Ordinary");;
let backup_dir=Assistance_dfa_root.of_line (home^"/Teuliou/OCaml/Githubbed_ocaml");;
let githubbing=false;;
let triple = (root,backup_dir,githubbing);;

end;;
module Next_World=struct

let root=Assistance_dfa_root.of_line (home^"/Teuliou/OCaml/Idaho");;
let backup_dir=Assistance_dfa_root.of_line (home^"/Teuliou/OCaml/Idaho_backup") ;;
let githubbing=false;;
let triple = (root,backup_dir,githubbing);;

end;;
module Third_World=struct

let root=Assistance_dfa_root.of_line (home^"/Teuliou/OCaml/Cherokee") ;;
let backup_dir=Assistance_dfa_root.of_line (home^"/Teuliou/OCaml/Cherokee_backup") ;;
let githubbing=false;;
let triple = (root,backup_dir,githubbing);;

end;;





end;;






module Assistance_explicit=struct

(*

#use"explicit.ml";;

*)

module Private = struct 

let iter0 (f:'a->unit) l addenda=
  let n=List.length(l)
  and accu=ref(l) in
  let s0=" of "^string_of_int(n)^" "^addenda^"\n" in
  for j=1 to n
               do
               ( f(List.hd(!accu));
                 accu:=List.tl(!accu);
                 print_string(string_of_int(j)^s0);
                 flush stdout)
               done;;

let iter2 (f:'a->'a) initial_value tester (shower:'a->string)  addenda=
  let accu=ref(initial_value) in
  let _=(while tester(!accu)
               do
               ( 
                 accu:=f(!accu);
                 print_string((shower (!accu))^addenda);
                 flush stdout;
               )
               done) in
  !accu;;

let e_rev l=
   let accu=ref([]) in 
   let f=(fun x->accu:=x::(!accu)) in
   let _=iter0(f)(l)(" (rev part)") in
   !accu;;    

let unchronometered_explore_tree f l=
    let g=(fun (graet,p,q,da_ober)->
       match da_ober with
       []->(graet,0,0,[])
       |a::peurrest->
         let temp1=f a in
         if temp1=[]
         then (a::graet,p-1,q+1,peurrest)
         else (a::graet,p-1+List.length(temp1),q+1,temp1@peurrest)
    ) and 
    tester=(fun (graet,p,q,da_ober)->da_ober<>[]) 
    and
    shower=(
     fun (graet,p,q,da_ober)->
       (string_of_int p)^" to be explored ; "^
       (string_of_int q)^" already explored\n"
    )
    in
    let initial_value=([],List.length l,0,l) in
    let _=(print_string(shower(initial_value));flush stdout) in
    let (ans,_,_,_)=iter2 g initial_value tester shower  "" in
    ans;;



let unchronometered_filter f l=
   let accu=ref([]) in 
   let g=(fun x->if f(x) then accu:=x::(!accu) else ()) in
   let _=iter0(g)(l)(" (filter part)") in
   e_rev(!accu);;    
 
 
 let unchronometered_image f l=
   let accu=ref([]) in 
   let g=(fun x->accu:=f(x)::(!accu)) in
   let _=iter0(g)(l)(" (rev_image part)") in
   e_rev(!accu);;  
   

let unchronometered_image_computed_backwards f l=
   let temp1=e_rev(l) in
    let accu=ref([]) in 
   let g=(fun x->accu:=f(x)::(!accu)) in
   let _=iter0(g)(temp1)(" (image part)") in
   (!accu);;     
 
  

exception Force_find_exn ;;

let rec helper_for_opt_finding (f,sn) (j,x)=
   match x with 
   [] -> None
   |a::others -> if f a 
                 then Some a
                 else let _=(
                        print_string("Item number "^string_of_int(j)^" of "^sn^" found wanting \n");
                        flush stdout) in 
                      helper_for_opt_finding (f,sn) (j+1,others) ;; 

end ;; 

let explore_tree f l=Assistance_chronometer.it (Private.unchronometered_explore_tree f) l;; 
           
let filter f l=Assistance_chronometer.it (Private.unchronometered_filter f) l;; 


let image f l=Assistance_chronometer.it (Private.unchronometered_image f) l;;  

let image_computed_backwards f l=Assistance_chronometer.it 
   	(Private.unchronometered_image_computed_backwards f) l;;               

let opt_find f x = Private.helper_for_opt_finding (f,string_of_int(List.length x)) (1,x) ;;

(* opt_find (fun t->t>4) (Ennig.ennig 1 7);; *)


end;;






module Assistance_lines_in_string=struct

(*

#use"lines_in_string.ml";;

*)


module Private = struct 

  let lines old_s=
     let left_offset=(if Assistance_supstring.begins_with old_s "\n" then "\n" else "")
     and right_offset=(if Assistance_supstring.ends_with old_s "\n" then "\n" else "") in
     let s=left_offset^old_s^right_offset in
     Str.split (Str.regexp_string "\n") s ;;

  let indexed_lines text=
     Assistance_int_range.index_everything (lines text);;
  
  let rec iterator_for_enchancement (num_of_treated_chars,treated_lines,lines) =
       match lines with 
       [] -> List.rev treated_lines 
       |(line_idx,line) :: other_lines ->
        iterator_for_enchancement 
        (num_of_treated_chars+(String.length line)+1,
         (num_of_treated_chars,line_idx,line)::treated_lines,other_lines)   ;;
        
  let enhance indexed_lines =  iterator_for_enchancement (0,[],indexed_lines );;      
  
  let adjust_num_of_lines_upwards_in_string ~required_size text =
      let temp1 = lines text in  
      let d = required_size - (List.length temp1) in 
      if d<=0 
      then text 
      else text ^ (String.make d '\n') ;;   

  let adjust_num_of_lines_upwards_in_file ~required_size file =
      let old_text = Assistance_io.read_whole_file file in
      let new_text = adjust_num_of_lines_upwards_in_string ~required_size old_text  in
      Assistance_io.overwrite_with file new_text ;;   

  let tripartition_associated_to_interval s i j=
      let temp2=lines s in 
      let (temp3,temp4)=Assistance_listennou.big_rht (i-1) temp2 in 
      let part1=String.concat "\n" (List.rev temp3) in 
      let (temp5,temp6)=Assistance_listennou.big_rht (j-i+1) temp4 in 
      let part2=String.concat "\n" (List.rev temp5) in 
      let part3=String.concat "\n" temp6 in 
      (part1^"\n",part2,"\n"^part3);;
   
   (* tripartition_associated_to_interval "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)
       
  let interval text i j=
    let temp1=indexed_lines text in
    let temp2=List.filter (fun (k,_)->(i<=k)&&(k<=j)) temp1  in
    let temp3=Assistance_image.image snd temp2 in
    String.concat "\n" temp3;;  
      
  (* interval "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)
    
  let copy_interval_from_string_to_string (i,j)  src dest =
     let src_linelength = List.length (lines src) in 
     let temp1 = adjust_num_of_lines_upwards_in_string ~required_size:src_linelength dest in
     let (before,in_between,after) = tripartition_associated_to_interval temp1 i j in 
     before^(interval src i j)^after;;

      
  (* copy_interval_from_string_to_string (2,5) "1\n2\n3\n4\n5\n6\n7\n" "a\nb\nc";; *)

  let copy_interval_from_file_to_file (i,j) src_file  dest_file =
     let src = Assistance_io.read_whole_file src_file 
     and old_text = Assistance_io.read_whole_file dest_file  in 
     let new_text = copy_interval_from_string_to_string (i,j) src old_text in 
     Assistance_io.overwrite_with dest_file new_text ;; 
     
   exception Lines_in_char_range_exn of int*int;;

   let number_of_lines_in_char_interval s  i j=
     try (List.length(List.filter (fun k->
         String.get s (k-1)='\n'
     ) (Assistance_int_range.range i j))) with
     _->raise(Lines_in_char_range_exn(i,j));;    

   let duplicate_interval_in_string (i,j) s = 
     let (before,itv,after) = tripartition_associated_to_interval s i j in 
     before^itv^"\n"^itv^after ;;

  (* duplicate_interval_in_string (2,4) "1\n2\n3\n4\n5\n";; *)

   let duplicate_interval_in_file (i,j) src_file  =
     let old_text = Assistance_io.read_whole_file src_file  in 
     let new_text = duplicate_interval_in_string (i,j) old_text in 
     Assistance_io.overwrite_with src_file new_text ;; 

  
  end ;;   
  
  let copy_interval_from_file_to_file = Private.copy_interval_from_file_to_file ;;
  let copy_interval_from_string_to_string = Private.copy_interval_from_string_to_string ;; 

  let duplicate_interval_in_file = Private.duplicate_interval_in_file ;;
  let duplicate_interval_in_string = Private.duplicate_interval_in_string ;;

  let indexed_lines = Private.indexed_lines ;;
  
  (*
  
  indexed_lines "a\nb";;
  indexed_lines "\na\nb";;
  indexed_lines "a\nb\n";;
  
  *)

  let enhanced_indexed_lines s= Private.enhance (Private.indexed_lines s);;
  
  (*
  
  enhanced_indexed_lines "a\nb";;
  enhanced_indexed_lines "\na\nb";;
  enhanced_indexed_lines "a\nb\n";;
  
  *)

  let indent_interval_in_string_with (i,j) ~text ~tab_width =
    let old_lines = indexed_lines text 
    and tab = String.make tab_width ' ' in 
    let new_lines = Assistance_image.image (
        fun (k,line) -> 
          if (k<i)||(k>j)
          then line
         else tab^line
    ) old_lines in 
    String.concat "\n" new_lines ;;

(* ident_interval_in_string_with (2,5) ~text:"1\n2\n3\n4\n5\n6\n7\n" ~tab_width:3;; *)

let indent_interval_in_file_with (i,j) fn ~tab_width=
   let old_text=Assistance_io.read_whole_file fn in
   let new_text=indent_interval_in_string_with (i,j) ~text:old_text ~tab_width  in
   Assistance_io.overwrite_with fn new_text;;   

let interval = Private.interval ;;

   let line_index_from_char_index s char_idx=
      1+(Private.number_of_lines_in_char_interval s 1 char_idx);;

  let lines s= Assistance_image.image snd (indexed_lines s);;

  let remove_interval s i j=
    let temp1=indexed_lines s in
    let temp2=List.filter (fun (k,_)->(i>k)||(k>j)) temp1  in
    let temp3=Assistance_image.image snd temp2 in
    String.concat "\n" temp3;; 
  
  let remove_interval_in_file fn i j=
      let s1=Assistance_io.read_whole_file fn in
      let s2=remove_interval s1 i j  in
     Assistance_io.overwrite_with fn s2;;   
  
  let remove_lines_containing_substring_in_string pattern text =
     let temp1=indexed_lines text in
     let temp2=List.filter (fun (_,line)->not(Assistance_substring.is_a_substring_of pattern line)) temp1  in
     let temp3=Assistance_image.image snd temp2 in
     String.concat "\n" temp3;; 
   
   let remove_lines_containing_substring_in_file pattern fn=
       let old_text=Assistance_io.read_whole_file fn in
       let new_text=remove_lines_containing_substring_in_string pattern old_text  in
      Assistance_io.overwrite_with fn new_text;;   
  
let findreplace_in_interval (x,y) s i j=
      let (part1,old_part2,part3) = Private.tripartition_associated_to_interval s i j in 
      let new_part2 = Assistance_replace_inside.replace_inside_string (x,y) old_part2 in 
      part1^new_part2^part3 ;; 

let findreplace_in_interval_in_file (x,y) fn i j=
      let s1=Assistance_io.read_whole_file fn in
      let s2=findreplace_in_interval (x,y) s1 i j  in
      Assistance_io.overwrite_with fn s2;;     
  

(* replace_in_interval ("\n"," ") "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)

let suppress_linebreaks_in_interval s i j=
    let (part1,old_part2,part3) = Private.tripartition_associated_to_interval s i j in 
    let new_part2 = String.concat "" (lines old_part2) in 
    part1^new_part2^part3 ;; 
  
  (* suppress_linebreaks_in_interval "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)
  
let suppress_linebreaks_in_interval_in_file fn i j=
    let s1=Assistance_io.read_whole_file fn in
    let s2=suppress_linebreaks_in_interval s1 i j  in
    Assistance_io.overwrite_with fn s2;;     

let tripartition_associated_to_interval = Private.tripartition_associated_to_interval ;;    





end;;






module Assistance_directory_name_t=struct

(*

Directories name, with the trailing slash removed.

#use"directory_name_t.ml";;

*)

type t=D of string;;

           

end;;






module Assistance_directory_name=struct

(*

Directories name, with the trailing slash removed.

#use"directory_name.ml";;

*)



exception Non_directory of string;;
exception File_not_found of string * (Assistance_directory_name_t.t list);;

let find_file_with_directory_list fname l=
  match Assistance_option.find_and_stop (
     fun (Assistance_directory_name_t.D s_dir) ->
      let full_path = s_dir^"/"^fname in 
      if Sys.file_exists full_path 
      then Some(Assistance_absolute_path.of_string full_path)
      else None
  ) l with 
  None -> raise(File_not_found(fname,l))
  |Some(ap) -> ap;;


let of_string s=
  let temp1=Assistance_tools_for_absolute_path.of_string s in
  if Sys.is_directory temp1
  then Assistance_directory_name_t.D(Assistance_tools_for_absolute_path.remove_trailing_slash temp1)
  else raise(Non_directory(s));;

let connectable_to_subpath (Assistance_directory_name_t.D s)=s^"/";;



end;;






module Assistance_set_of_polys_t=struct

(* 

#use"Ordered_Lists/set_of_polys_t.ml";;

*)

type 'a t=S of 'a list;;


end;;






module Assistance_set_of_polys=struct

(* 

#use"Ordered_Lists/set_of_polys.ml";;

*)

let tr = ((fun x->Assistance_set_of_polys_t.S(x)),(fun (Assistance_set_of_polys_t.S(x))->x),Assistance_total_ordering.standard);;

let does_not_intersect x y= Assistance_functor_for_sets.does_not_intersect tr x y;;
let empty_set = Assistance_functor_for_sets.empty_set tr;;
let fold_merge l= Assistance_functor_for_sets.fold_merge tr l;;
let forget_order x= Assistance_functor_for_sets.forget_order tr x;;
let hd x = Assistance_functor_for_sets.hd tr x;;
let image f x= Assistance_functor_for_sets.image tr f x;;
let insert a x= Assistance_functor_for_sets.insert tr a x;;
let is_included_in x y= Assistance_functor_for_sets.is_included_in tr x y;;
let length x= Assistance_functor_for_sets.length tr x;;
let mem a x= Assistance_functor_for_sets.mem tr a x;;
let merge l= Assistance_functor_for_sets.merge tr l;;
let nmem a x= Assistance_functor_for_sets.nmem tr a x;;
let outsert a x= Assistance_functor_for_sets.outsert tr a x;;
let safe_set l= Assistance_functor_for_sets.safe_set tr l;;
let setminus x y= Assistance_functor_for_sets.setminus tr x y;;
let singleton a= Assistance_functor_for_sets.singleton tr a;;
let sort l= Assistance_functor_for_sets.sort tr l;;
let unsafe_set l= Assistance_functor_for_sets.unsafe_set tr l;;




end;;






module Assistance_stabilize=struct

(*

#use"stabilize.ml";;

*) 

let one_more_time f (an_holl,da_ober)=
 let l_da_ober=Assistance_set_of_polys.forget_order(da_ober) in
 let temp1=List.flatten(List.rev_map(f)(l_da_ober)) in
 let temp2=Assistance_set_of_polys.sort(temp1) in
 let re_nevez=Assistance_set_of_polys.setminus(temp2)(an_holl) in
 let hollad_nevez=Assistance_set_of_polys.merge(an_holl)(re_nevez) in
 (hollad_nevez,re_nevez);; 
  
let rec morzholan f (an_holl,da_ober)=
  if da_ober=Assistance_set_of_polys.empty_set
  then an_holl
  else morzholan f (one_more_time f (an_holl,da_ober));;
  
let father f l=let tl=Assistance_set_of_polys.sort(l) in morzholan f (tl,tl);;

let one_more_time2 f (an_holl,graet,da_ober)=
 let l_graet=Assistance_set_of_polys.forget_order(graet) 
 and l_da_ober=Assistance_set_of_polys.forget_order(da_ober) in
 let zz1=Assistance_cartesian.product(l_graet)(l_da_ober)
 and zz2=Assistance_cartesian.product(l_da_ober)(l_graet) 
 and zz3=Assistance_cartesian.product(l_da_ober)(l_da_ober) in
 let zz=List.flatten [zz1;zz2;zz3] in
 let temp1=List.flatten(List.rev_map (function (x,y)->[f x y]) zz ) in
 let temp2=Assistance_set_of_polys.sort(temp1) in
 let re_nevez=Assistance_set_of_polys.setminus(temp2)(an_holl) in
 let hollad_nevez=Assistance_set_of_polys.merge(an_holl)(re_nevez) in
 (hollad_nevez,an_holl,re_nevez);; 
  
let rec morzholan2 f (an_holl,graet,da_ober)=
  if da_ober=Assistance_set_of_polys.empty_set
  then an_holl
  else morzholan2 f (one_more_time2 f (an_holl,graet,da_ober));; 
  
let binary_operation f l=let tl=Assistance_set_of_polys.sort(l) in morzholan2 f (tl,Assistance_set_of_polys.empty_set,tl);;  

let explore_tree f l0=
 let modified_l0=List.rev_map(function x->(x,0))(l0) in
 let modified_f=(function (x,j)->
   List.rev_map(function y->(y,j+1))(f x)
 ) in
 let rec tempf=(function (j,graet,da_ober)->
 match da_ober with
    []->graet
    |(xa,ia)::peurrest->let temp1=modified_f(xa,ia) in
                  let temp2=(j+1,xa::graet,List.rev_append temp1 peurrest) in
                  tempf(temp2)
 ) in
 tempf(0,[],modified_l0);;
 
 let explore_tree_explicitly f l0=
 let modified_l0=List.rev_map(function x->(x,0))(l0) in
 let modified_f=(function (x,j)->
   List.rev_map(function y->(y,j+1))(f x)
 ) in
 let rec tempf=(function (j,graet,da_ober)->
 match da_ober with
    []->graet
    |(xa,ia)::peurrest->let temp1=modified_f(xa,ia) in
                  let temp2=(j+1,xa::graet,List.rev_append temp1 peurrest) in
                  let _=(print_string("("^string_of_int(ia)^","^string_of_int(j+1)^")\n");
                  flush stdout) in
                  tempf(temp2)
 ) in
 tempf(0,[],modified_l0);;
 
type 'a hierarchize_data=
   Normal of  'a Assistance_set_of_polys_t.t list * 'a Assistance_set_of_polys_t.t * ('a * 'a Assistance_set_of_polys_t.t) list
  |Failed of  'a Assistance_set_of_polys_t.t list * 'a Assistance_set_of_polys_t.t * ('a * 'a Assistance_set_of_polys_t.t) list
  |Succeeded of 'a list list;;
   

let pusher_for_hierarchization (graet,hollad,da_ober)=
  if da_ober=[]
  then Succeeded(List.rev_map Assistance_set_of_polys.forget_order graet)
  else 
  let temp1=Assistance_image.image (fun (x,y)->(x,Assistance_set_of_polys.setminus y hollad)) da_ober in
  let (temp2,temp3)=List.partition (fun (x,z)->Assistance_set_of_polys.length z=0) temp1 in
  if temp2=[]
  then Failed(graet,hollad,da_ober)
  else
  let temp4=Assistance_image.image fst temp2 in
  let o_temp4=Assistance_set_of_polys.sort(temp4) in
  let temp5=Assistance_image.image (fun (x,z)->(x,Assistance_set_of_polys.setminus z o_temp4)) temp3 in
  (Normal(o_temp4::graet,Assistance_set_of_polys.merge hollad o_temp4,temp5));;
  
type 'a list_of_ancestors_map=('a -> 'a list);;  
  
let try_hierarchizing (f: 'a list_of_ancestors_map) l=
  let temp1=Assistance_image.image (fun t->(t,Assistance_set_of_polys.sort(f t))) l in
  let rec tempf=(fun x->
    let y=pusher_for_hierarchization x in
    match y  with
    Normal(a,b,c)->tempf(a,b,c)
    |_->y) in
  tempf ([],Assistance_set_of_polys.empty_set,temp1);;
  
let hierarchize f l=
  match try_hierarchizing f l with
   Succeeded(l)->l
  |_->failwith("Direct hierarchizing fails, there is a cycle. Try hierarchize instead");;
  
  

           

end;;






module Assistance_more_unix=struct

(*

#use"more_unix.ml";;

*)


module Private=struct
 
let naive_extension ap=
   let s=Assistance_absolute_path.to_string ap in
   let i=String.rindex s '.' in
   (Assistance_cull_string.cobeginning (i+1) s);; 
   
let extension x=try (naive_extension x) with 
  any_exception->"";;
  
 let is_a_directory ap=
   let s=Assistance_absolute_path.to_string ap in
   try (function x->true)(Sys.readdir s) with any_exception->false;;
 
 let father ap=
   let s=Assistance_absolute_path.to_string ap in
   let i=String.rindex s '/' in
   if i=0 then Assistance_directory_name.of_string"/" else
   Assistance_directory_name.of_string (Assistance_cull_string.beginning i s);; 
   
 let son dir=
   let s=Assistance_directory_name.connectable_to_subpath dir in
   let i=String.rindex s '/' in
   if i=0 then "" else
   (Assistance_cull_string.cobeginning (i+1) s);; 
  
 let is_a_nondirectory_or_a_nib x=
  if is_a_directory(x)
  then extension(x)="nib"
  else not(Assistance_substring.is_a_substring_of(".nib/")(Assistance_absolute_path.to_string x));;
  
 let naive_ls dir=
   let s=Assistance_directory_name.connectable_to_subpath dir in
   let s_with_slash=(function ()->
    if String.get(s)(String.length(s)-1)='/'
    then s
    else s^"/"
   )() in
   let temp1=Array.to_list(Sys.readdir(s)) in
   let tempf=(function w->try (Some(Assistance_absolute_path.of_string(s_with_slash^w))) with
   any_exception->None) in
   Assistance_option.filter_and_unpack tempf temp1;;
   
 let ls x=try (naive_ls x) with any_exception->[];;  
 
 let test_for_cleaniness=function ap->
  let s=Assistance_absolute_path.to_string ap in
  Assistance_cull_string.after_rightmost(s)('/')<>".DS_Store";;
 
 let cleaned_ls x=
   List.filter test_for_cleaniness (ls x);;
   
let select_by_prefix subdir forbidden_subdirs =
  Assistance_option.filter_and_unpack (
     fun forb_subdir -> 
        if Assistance_supstring.begins_with forb_subdir subdir 
        then Some(Assistance_cull_string.two_sided_cutting (subdir,"") forb_subdir)
        else None
  ) forbidden_subdirs ;; 

let ls_with_ignored_subdirs (dir,forbidden_subdirs)=
   let temp1 = Array.to_list (Sys.readdir dir) in
   let temp2 = Assistance_option.filter_and_unpack (
      fun fname -> if List.for_all (
          fun forb_subdir -> 
           not(Assistance_supstring.begins_with fname forb_subdir)
        )  forbidden_subdirs
           then Some(dir^fname)
           else None
   ) temp1 in 
   let is_a_dir  = (fun s->is_a_directory(Assistance_absolute_path.AP(s))) in 
   let (found_dirs,found_nondirs) = List.partition is_a_dir temp2 in 
   let new_constraints = Assistance_image.image (
     fun full_subdir_path ->
        let subdir = Assistance_cull_string.two_sided_cutting (dir,"") full_subdir_path in 
       (full_subdir_path^"/",select_by_prefix subdir forbidden_subdirs)
   ) found_dirs in 
   (found_nondirs,found_dirs,new_constraints);;

let rec helper_for_complete_ls_with_ignored_subdirs 
  (verbose,treated_nondirs,treated_dirs,to_be_treated) = match to_be_treated with 
  [] -> (Assistance_image.image Assistance_absolute_path.of_string treated_nondirs,treated_dirs)
  |(dir,forbidden_subdirs) :: others -> 
    let (found_nondirs,found_dirs,new_constraints) = 
        ls_with_ignored_subdirs (dir,forbidden_subdirs) in 
    let new_treated_nondirs = List.rev_append found_nondirs treated_nondirs 
    and new_treated_dirs =  List.rev_append found_dirs treated_dirs 
    and new_to_be_treated = List.rev_append new_constraints others in 
    let n = string_of_int(List.length new_to_be_treated) in 
    let msg = " "^n^" to go ...\n" in 
    let _= (if verbose then (print_string msg;flush stdout)) in 
    helper_for_complete_ls_with_ignored_subdirs 
    (verbose,new_treated_nondirs,new_treated_dirs,new_to_be_treated) ;;

let complete_ls_with_ignored_subdirs dir forbidden_subdirs verbose= 
   let s_dir = Assistance_directory_name.connectable_to_subpath dir in 
   let _= (if not verbose then (print_string "Enumerating files ...";flush stdout)) in 
   let (treated_nondirs,treated_dirs) = 
   helper_for_complete_ls_with_ignored_subdirs 
  (verbose,[],[],[s_dir,
         Assistance_image.image Assistance_dfa_subdirectory.without_trailing_slash forbidden_subdirs]) in 
   let m = string_of_int(List.length treated_nondirs) in 
   let msg = " "^m^" files found.\n" in 
   let _= (if not verbose then (print_string msg;flush stdout)) in          
   (treated_nondirs,Assistance_image.image 
     (fun x->Assistance_dfa_subdirectory.of_line(Assistance_cull_string.two_sided_cutting (s_dir,"") x)) 
   treated_dirs);;

let ls_with_directories_only dir=
   let temp1 = cleaned_ls dir in 
   Assistance_option.filter_and_unpack (
     fun ap -> 
       if is_a_directory ap 
       then let s_ap = Assistance_absolute_path.to_string ap in 
            Some(Assistance_directory_name.of_string s_ap)
       else None
   )  temp1 ;;

 let dirty_ones_in_ls x=
   List.filter (function u->not(test_for_cleaniness u) )(ls x);; 
 
 let adhoc_ls ap=
   let s=Assistance_absolute_path.to_string ap in
   if not(is_a_directory ap) 
   then []
   else 
   let dir=Assistance_directory_name.of_string s in
   ls dir;;
 

 
let complete_ls dir=
   let s_dir=Assistance_directory_name.connectable_to_subpath dir in
   let x=Assistance_absolute_path.of_string s_dir in
   Assistance_explicit.explore_tree adhoc_ls [x];;   

let adhoc_ls_with_ignored_subdirs ap=
   let s=Assistance_absolute_path.to_string ap in
   if not(is_a_directory ap) 
   then []
   else 
   let dir=Assistance_directory_name.of_string s in
   ls dir;;

let complete_ls_with_directories_only x=
  Assistance_explicit.explore_tree ls_with_directories_only [x];;
  

 let complete_ls_with_nondirectories_only x=
  List.filter(is_a_nondirectory_or_a_nib)(complete_ls x);;
  
  
 let beheaded_ls_with_nondirectories_only x=
  let n0=String.length(Assistance_absolute_path.to_string x) in
  let temp1=List.filter(is_a_nondirectory_or_a_nib)(adhoc_ls x) in
  let temp2=Assistance_image.image (fun ap->Assistance_cull_string.cobeginning n0 (Assistance_absolute_path.to_string ap)) temp1 in
  temp2;; 
 
 let dir_substructure x=
    let n0=String.length(Assistance_absolute_path.to_string x) in
    let temp1=(Assistance_stabilize.explore_tree adhoc_ls (adhoc_ls x)) in
    let temp2=List.filter(function x->extension(x)<>"nib")(temp1) in
    List.rev_map(function ap->Assistance_cull_string.cobeginning n0 (Assistance_absolute_path.to_string ap))(temp2);;
  
 let endfiles x=
    let n0=String.length(Assistance_absolute_path.to_string x)+1(*because of the slash!*) in
    let temp1=(Assistance_stabilize.explore_tree adhoc_ls (adhoc_ls x)) in
    let temp2=List.filter(is_a_nondirectory_or_a_nib)(temp1) in
    List.rev_map(function ap->Assistance_cull_string.cobeginning n0 (Assistance_absolute_path.to_string ap))(temp2);;
    
let quick_complete_ls s=
  let x=Assistance_directory_name.of_string s in
  let temp1=complete_ls x in
  Assistance_image.image Assistance_absolute_path.to_string temp1;;  
  
 

let quick_beheaded_complete_ls s=
  let x=Assistance_directory_name.of_string s in
  let n=String.length(Assistance_directory_name.connectable_to_subpath x) in
  let temp1=complete_ls x in
  Assistance_image.image (fun ap->Assistance_cull_string.cobeginning n (Assistance_absolute_path.to_string ap)) temp1;; 
  
let beheaded_simple_ls dir=
  let n=String.length(Assistance_directory_name.connectable_to_subpath dir) in
  let temp1=ls dir in
  Assistance_image.image (fun ap->
   Assistance_cull_string.cobeginning n (Assistance_absolute_path.to_string ap)) temp1;; 


let clear_directory_contents root =
    let s_root = Assistance_dfa_root.connectable_to_subpath root in 
    let cmd = "rm -rf "^s_root^"*" in 
    Sys.command cmd;;


let create_subdirs_and_fill_files_if_necessary root subdirs files_with_content =
   let s_root = Assistance_dfa_root.connectable_to_subpath root in 
   let cmds1=Assistance_image.image (
      fun subdir -> 
         "mkdir -p "^s_root^(Assistance_dfa_subdirectory.without_trailing_slash subdir)
   ) subdirs in 
   let _=Assistance_image.image Sys.command cmds1 in 
   let temp1=Assistance_option.filter_and_unpack (
     fun (rootless,content)->
        let full_path = Assistance_dfn_full.to_line(Assistance_dfn_join.root_to_rootless root rootless) in 
        if Sys.file_exists full_path 
        then None 
        else Some(full_path,content)
   ) files_with_content in 
   Assistance_image.image (
      fun (full_path,content) ->
         let _=Sys.command("touch "^full_path) in 
         Assistance_io.overwrite_with (Assistance_absolute_path.of_string full_path) content
   )  temp1;;


let create_subdirs_and_fill_files root subdirs files_with_content =
   let s_root = Assistance_dfa_root.connectable_to_subpath root in 
   let cmds1=Assistance_image.image (
      fun subdir -> 
         "mkdir -p "^s_root^(Assistance_dfa_subdirectory.without_trailing_slash subdir)
   ) subdirs in 
   let _=Assistance_image.image Sys.command cmds1 in 
   Assistance_image.image (
     fun (rootless,content)->
        let full_path = Assistance_dfn_full.to_line(Assistance_dfn_join.root_to_rootless root rootless) in 
         let _=Sys.command("touch "^full_path) in 
         Assistance_io.overwrite_with (Assistance_absolute_path.of_string full_path) content
   ) files_with_content;;

end;;    


let all_files_with_endings dir l_endings=
   let temp1=Private.complete_ls dir in
   let temp2=List.filter(
   fun ap->
     let s_ap=Assistance_absolute_path.to_string ap in
     List.exists( fun ending->
       Assistance_supstring.ends_with s_ap ending)
     l_endings  
   ) temp1 in
   temp2;;  
let beheaded_simple_ls=Private.beheaded_simple_ls;;
let complete_ls=Private.complete_ls;;
let complete_ls_with_directories_only=Private.complete_ls_with_directories_only;;
let complete_ls_with_ignored_subdirs=Private.complete_ls_with_ignored_subdirs;;
let complete_ls_with_nondirectories_only=Private.complete_ls_with_nondirectories_only;;
let clear_directory_contents = Private.clear_directory_contents;;
let create_subdirs_and_fill_files = Private.create_subdirs_and_fill_files;;
let create_subdirs_and_fill_files_if_necessary = Private.create_subdirs_and_fill_files_if_necessary;;
let is_a_directory=Private.is_a_directory;;   
let quick_beheaded_complete_ls=Private.quick_beheaded_complete_ls;;           
let simple_ls=Private.ls;;

end;;






module Assistance_put_use_directive_in_initial_comment=struct

(*

#use"Text_editing/put_use_directive_in_initial_comment.ml";;

*)

let detect_initial_comment_in_text text = 
  let lines = Assistance_lines_in_string.indexed_lines text in 
  let first_line = snd (List.hd lines) in 
  if (Assistance_cull_string.trim_spaces first_line) <> "(*" then None else 
  match Assistance_option.seek (fun (line_idx,line)->
     if (line_idx<=1) then false else
     let trimmed_line = Assistance_cull_string.trim_spaces line in 
     Assistance_supstring.begins_with trimmed_line "#use"
  ) lines with 
  None -> None 
  |Some(i1,line1) ->
 (
  match Assistance_option.seek (fun (line_idx,line)->
    if (line_idx<=i1) then false else
    let trimmed_line = Assistance_cull_string.trim_spaces line in 
    trimmed_line = "*)"
 ) lines with 
  None -> None 
 |Some(i2,_) -> Some(i1,Assistance_cull_string.trim_spaces line1,i2) );;  

let detect_initial_comment_in_file  fn =  detect_initial_comment_in_text  (Assistance_io.read_whole_file fn) ;;

let in_text ~new_directive text =
  match detect_initial_comment_in_text text  with 
  None -> text 
  |Some(i1,line1,i2) ->
    let old_lines = Assistance_lines_in_string.indexed_lines text in 
    let new_lines = Assistance_image.image (fun (line_idx,line)->
        if line_idx = i1 then new_directive else line
      ) old_lines in 
   String.concat "\n" new_lines ;;   

let in_file ~new_directive fn =   
   let text = Assistance_io.read_whole_file fn in 
   match detect_initial_comment_in_text text  with 
  None -> () 
  |Some(i1,line1,i2) ->
    let old_lines = Assistance_lines_in_string.indexed_lines text in 
    let new_lines = Assistance_image.image (fun (line_idx,line)->
        if line_idx = i1 then new_directive else line
      ) old_lines in 
    let new_text = String.concat "\n" new_lines in 
    Assistance_io.overwrite_with fn new_text;;

let usual root ap =
    let s_ap=Assistance_absolute_path.to_string ap in 
    let s_cdir=Assistance_dfa_root.connectable_to_subpath root in 
    let shortened_path=Assistance_cull_string.cobeginning (String.length s_cdir) s_ap in 
    "#use\""^shortened_path^"\";"^";" ;;
    
let put_usual root ap =
    let new_directive = usual root ap in 
    in_file ~new_directive ap;;    

end;;






module Assistance_particular_string=struct

(*

#use"particular_string.ml";;


*)



let double_semicolon=String.make 2 (char_of_int 59);; (* to make life easier for the OCaml mini-parser *)

let triple_blank = String.make 3 ' ';;
           

end;;






module Assistance_coma_constant=struct

(* 

#use"Compilation_management/coma_constant.ml";;

*)

module Private = struct

let directives_subdir=
  Assistance_dfa_subdirectory.of_line "directives";;

let debugging_subdir=
  Assistance_dfa_subdirectory.of_line "debugging";;

let watched_subdir=
  Assistance_dfa_subdirectory.of_line "watched";;

let nonml_files_subdir=
  Assistance_dfa_subdirectory.of_line "nonml_files";;

let watched_and_githubbed_subdir=
  Assistance_dfa_subdirectory.extend watched_subdir "watched_and_githubbed";;

let watched_not_githubbed_subdir=
  Assistance_dfa_subdirectory.extend watched_subdir "watched_not_githubbed";;

let githubbed_nonml_files_subdir=
  Assistance_dfa_subdirectory.extend nonml_files_subdir "githubbed_nonml_files";;

let nongithubbed_nonml_files_subdir=
  Assistance_dfa_subdirectory.extend nonml_files_subdir "nongithubbed_nonml_files";;  

let build_subdir =   Assistance_dfa_subdirectory.extend nongithubbed_nonml_files_subdir "_build";;
let usual_build_subdir= Assistance_dfa_subdirectory.extend build_subdir "_usual_build";;
let debug_build_subdir= Assistance_dfa_subdirectory.extend build_subdir "_debug_build";;  
let exec_build_subdir=  Assistance_dfa_subdirectory.extend build_subdir "_exec_build";;  
let parameters_subdir= Assistance_dfa_subdirectory.of_line "Compilation_management";;

let short_path_for_diary_file= Assistance_dfn_short.of_line"diary_archive.ml";;
let short_path_for_loadingsfile= Assistance_dfn_short.of_line"my_loadings.ml";;
let short_path_for_painful_debugging_file=Assistance_dfn_short.of_line"painful_debugging.ml";;
let short_path_for_parametersfile= Assistance_dfn_short.of_line "coma_big_constant.ml";;
let short_path_for_printersfile= Assistance_dfn_short.of_line "my_printers.ml";;
let short_path_for_targetfile= Assistance_dfn_short.of_line "targetfile.ocaml_made";;
 
let rootless_path_for_diary_file=
  Assistance_dfn_join.subdirectory_to_short  watched_and_githubbed_subdir short_path_for_diary_file;;
let rootless_path_for_loadingsfile=
  Assistance_dfn_join.subdirectory_to_short  directives_subdir short_path_for_loadingsfile;;
let rootless_path_for_painful_debugging_file=
  Assistance_dfn_join.subdirectory_to_short  watched_not_githubbed_subdir short_path_for_painful_debugging_file;;
let rootless_path_for_parametersfile=
  Assistance_dfn_join.subdirectory_to_short  parameters_subdir short_path_for_parametersfile;;
let rootless_path_for_printersfile=
  Assistance_dfn_join.subdirectory_to_short  directives_subdir short_path_for_printersfile;;
let rootless_path_for_targetfile=
  Assistance_dfn_join.subdirectory_to_short  nongithubbed_nonml_files_subdir short_path_for_targetfile;;     

let rootless_path_for_ocamlinit = Assistance_dfn_rootless.of_line ".ocamlinit";;


let git_ignored_subdirectories =
  [
     build_subdir;
     watched_not_githubbed_subdir;
     nongithubbed_nonml_files_subdir;
     directives_subdir;
     debugging_subdir;
  ];;


let minimalist_text_for_ocamlinit =
   "\n#use\""^(Assistance_dfn_rootless.to_line rootless_path_for_loadingsfile)^"\""^Assistance_particular_string.double_semicolon^
  "\n#use\""^(Assistance_dfn_rootless.to_line rootless_path_for_printersfile)^"\""^Assistance_particular_string.double_semicolon;;

 let full_text_for_ocamlinit = (
      minimalist_text_for_ocamlinit^
      "\nopen Needed_values"^Assistance_particular_string.double_semicolon^
      "\ninitialize_toplevel()"^Assistance_particular_string.double_semicolon
       ) ;; 

let text_for_printersfile = "\n\n (*Registered printers start here *) \n\n (*Registered printers end here *) \n\n" ;;
let text_for_painful_debugging_file  = "\n\n(*\n\n#use\"Fads/painful_debugging.ml\""^Assistance_particular_string.double_semicolon^"\n\n*)\n\n" ;;

let common_part_in_conventional_files = 
   [
     rootless_path_for_printersfile, text_for_printersfile ; 
     rootless_path_for_loadingsfile, "" ;
     rootless_path_for_targetfile, "";
     rootless_path_for_diary_file, "";
   ] ;;     


let conventional_files_with_full_content =  
   [
     rootless_path_for_ocamlinit, full_text_for_ocamlinit ;
     rootless_path_for_painful_debugging_file, text_for_painful_debugging_file;
   ] @ common_part_in_conventional_files ;;      

let conventional_files_with_minimal_content =    
   [
     rootless_path_for_ocamlinit, minimalist_text_for_ocamlinit ;
   ] @ common_part_in_conventional_files ;;      


let minimal_set_of_needed_dirs = 
  [
    usual_build_subdir ;
    watched_not_githubbed_subdir;
    watched_and_githubbed_subdir;
    githubbed_nonml_files_subdir;
    nongithubbed_nonml_files_subdir;
  ] ;;  

let full_set_of_needed_dirs = 
  minimal_set_of_needed_dirs @
    [
    ] ;;  

end ;;

 let conventional_files_with_full_content = Private.conventional_files_with_full_content ;;
 let conventional_files_with_minimal_content = Private.conventional_files_with_minimal_content ;;
 let debug_build_subdir = Private.debug_build_subdir ;;
 let debugging_subdir = Private.debugging_subdir ;;
 let exec_build_subdir = Private.exec_build_subdir ;;
 let full_set_of_needed_dirs = Private.full_set_of_needed_dirs ;;
 let git_ignored_subdirectories = Private.git_ignored_subdirectories ;;
 let watched_and_githubbed_subdir = Private.watched_and_githubbed_subdir ;;
 let minimal_set_of_needed_dirs = Private.minimal_set_of_needed_dirs ;;
 let nongithubbed_nonml_files_subdir = Private.nongithubbed_nonml_files_subdir ;;
 let rootless_path_for_diary_file = Private.rootless_path_for_diary_file ;;
 let rootless_path_for_loadingsfile = Private.rootless_path_for_loadingsfile ;;
 let rootless_path_for_parametersfile = Private.rootless_path_for_parametersfile ;;
 let rootless_path_for_printersfile = Private.rootless_path_for_printersfile ;;
 let rootless_path_for_targetfile = Private.rootless_path_for_targetfile ;;
 let usual_build_subdir = Private.usual_build_subdir ;;




end;;






module Assistance_partial_crobj_t=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/partial_crobj_t.ml";;

For convenience, the list is reversed when the partial object is closed and becomes full
(new objects are therefore appended directly to the left on a partial object).


*)


type t= 
   |Uple of Assistance_concrete_object_t.t list
   |List of Assistance_concrete_object_t.t list
   |Array of Assistance_concrete_object_t.t list
   |Record of ((string*Assistance_concrete_object_t.t) list)
   |RecordPlusFieldName of ((string*Assistance_concrete_object_t.t) list)*string
   |Variant of string*(Assistance_concrete_object_t.t list);;



end;;






module Assistance_double_partial_crobj_t=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/double_partial_crobj_t.ml";;

First argument says if a comma appears last, waiting for another item.

The two last arguments are t * (t list) rather than just t list, to enforce
a non-empty list. 

*)


type t= Double of bool * Assistance_partial_crobj_t.t * (Assistance_partial_crobj_t.t list) ;;



end;;






module Assistance_crobj_parsing_machine_t=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_parsing_machine_t.ml";;


*)

type t= 
    {
       parsed_one    : string;
       current_index : int;
       data : Assistance_double_partial_crobj_t.t;
    };;



end;;






module Assistance_crobj_category_t=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_category_t.ml";;


*)


type t= 
    Uple 
   |List 
   |Array 
   |Record
   |Variant;;



end;;






module Assistance_crobj_opening_t=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_opening_t.ml";;



*)


type t= 
    Uple 
   |List 
   |Array 
   |Record 
   |Variant of string;;



end;;






module Assistance_crobj_basic_increase_t=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_basic_increase_t.ml";;


*)


type t= 
     Push_int of int 
    |Push_string of Assistance_encoded_string_t.t 
    |Push_field_name  of string 
    |Open of Assistance_crobj_opening_t.t 
    |Separate of Assistance_crobj_category_t.t
    |Close of Assistance_crobj_category_t.t;;
    


        





end;;






module Assistance_partial_crobj=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/partial_crobj.ml";;


*)

exception Field_With_No_Name of Assistance_concrete_object_t.t;;
exception Unused_Field_Name of string;;
exception Misapplied_Field_Name of string;;
exception Category_Mismatch of Assistance_crobj_category_t.t * Assistance_partial_crobj_t.t;;

let initialize=function 
    Assistance_crobj_opening_t.Uple -> Assistance_partial_crobj_t.Uple[]
   |List -> Assistance_partial_crobj_t.List[]
   |Array -> Assistance_partial_crobj_t.Array[]
   |Record->Assistance_partial_crobj_t.Record([])
   |Variant(constructor)->Assistance_partial_crobj_t.Variant(constructor,[]);;

let category = function 
    Assistance_partial_crobj_t.Uple(l)->Assistance_crobj_category_t.Uple
   |List(_)->Assistance_crobj_category_t.List
   |Array(_)->Assistance_crobj_category_t.Array
   |Record(_)->Assistance_crobj_category_t.Record
   |RecordPlusFieldName(l,rcdname)->Assistance_crobj_category_t.Record
   |Variant(constructor,l)->Assistance_crobj_category_t.Variant;;
 

let push_one_more_item item =function 
    Assistance_partial_crobj_t.Uple(l)->Assistance_partial_crobj_t.Uple(item::l)
   |List(l)->Assistance_partial_crobj_t.List(item::l)
   |Array(l)->Assistance_partial_crobj_t.Array(item::l)
   |Record(_)->raise(Field_With_No_Name(item))
   |RecordPlusFieldName(l,rcdname)->Assistance_partial_crobj_t.Record((rcdname,item)::l)
   |Variant(constructor,l)->Assistance_partial_crobj_t.Variant(constructor,item :: l);;

let push_int i = push_one_more_item (Assistance_concrete_object_t.Int(i));;
let push_string encoded_s = push_one_more_item (Assistance_concrete_object.wrap_encoded_string encoded_s);;

let push_field_name recdname=function 
    Assistance_partial_crobj_t.Record(l)->Assistance_partial_crobj_t.RecordPlusFieldName(l,recdname)
   |_->raise(Misapplied_Field_Name(recdname));;



let close =function 
    Assistance_partial_crobj_t.Uple(l)->Assistance_concrete_object_t.Uple(List.rev l)
   |List(l)->Assistance_concrete_object_t.List(List.rev l)
   |Array(l)->Assistance_concrete_object_t.Array(List.rev l)
   |Record(l)->Assistance_concrete_object_t.Record(List.rev l)
   |RecordPlusFieldName(_,rcdname)->raise(Unused_Field_Name(rcdname))
   |Variant(constructor,l)->Assistance_concrete_object_t.Variant(constructor,List.rev l);;


let check_category_and_close ctgr pcrobj=
   if category(pcrobj)<>ctgr 
   then raise(Category_Mismatch(ctgr,pcrobj))
   else 
   close pcrobj;;

end;;






module Assistance_double_partial_crobj=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/double_partial_crobj.ml";;

*)

exception Close_on_separator;;
exception Redundant_separator;;
exception Category_Mismatch of Assistance_crobj_category_t.t * Assistance_partial_crobj_t.t;;
exception End_reached of Assistance_concrete_object_t.t ;;

module Private = struct 

let push_int i (Assistance_double_partial_crobj_t.Double(_,last_opened,opened_before))=
  (Assistance_double_partial_crobj_t.Double(false,
    Assistance_partial_crobj.push_int i last_opened,opened_before));;

let push_string encoded_s (Assistance_double_partial_crobj_t.Double(_,last_opened,opened_before))=
  (Assistance_double_partial_crobj_t.Double(false,
    Assistance_partial_crobj.push_string encoded_s last_opened,opened_before));;    

let push_separator ctgr (Assistance_double_partial_crobj_t.Double(separator_present,last_opened,opened_before))=
  if separator_present
  then raise(Redundant_separator)
  else 
        let ctgr2 = Assistance_partial_crobj.category last_opened in 
        if ctgr <> ctgr2
        then raise(Category_Mismatch(ctgr,last_opened))
        else  (Assistance_double_partial_crobj_t.Double(true,last_opened,opened_before));;

let push_field_name record_name (Assistance_double_partial_crobj_t.Double(_,last_opened,opened_before))=
  (Assistance_double_partial_crobj_t.Double(false,
    Assistance_partial_crobj.push_field_name record_name last_opened,opened_before));;    

let open_new opening 
   (Assistance_double_partial_crobj_t.Double(_,last_opened,opened_before))=
    Assistance_double_partial_crobj_t.Double(false,
      Assistance_partial_crobj.initialize opening,last_opened::opened_before);;



let close ctgr
    (Assistance_double_partial_crobj_t.Double(separator_present,last_opened,opened_before))=
    if separator_present 
    then raise(Close_on_separator)
    else 
    let newfound=Assistance_partial_crobj.check_category_and_close ctgr last_opened in 
    match opened_before with 
    []->raise(End_reached(newfound))
    |next_opened_one::others ->
      let new_frontier = Assistance_partial_crobj.push_one_more_item newfound next_opened_one in 
      Assistance_double_partial_crobj_t.Double(false,new_frontier,others);;

end ;; 

let initialize opening = 
    Assistance_double_partial_crobj_t.Double(false,Assistance_partial_crobj.initialize opening,[]);;

let increase = function 
   Assistance_crobj_basic_increase_t.Push_int(i)->Private.push_int i 
    |Push_string(encoded_s)->Private.push_string encoded_s 
    |Push_field_name(rcdname)->Private.push_field_name rcdname
    |Open(opening) -> Private.open_new opening
    |Separate(cat) -> Private.push_separator cat 
    |Close(cat) -> Private.close cat;;
        






end;;






module Assistance_crobj_parsing=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_parsing.ml";;


*)

exception Unreadable of int * string ;;

module Private = struct

let salt = Assistance_encoded_string.salt ;; 

let array_opener = salt ^ "ao";;
let list_opener = salt ^ "lo";;
let record_opener = salt ^ "ro";;
let string_opener = salt ^ "so";;
let uple_opener = salt ^ "uo";; 
let variant_opener = salt ^ "vo";; 

let array_separator = salt ^ "as";;
let list_separator = salt ^ "ls";;
let record_separator = salt ^ "rs";;
let uple_separator = salt ^ "us";; 
let variant_separator = salt ^ "vs";; 

let array_closer = salt ^ "ac";;
let list_closer = salt ^ "lc";;
let record_closer = salt ^ "rc";;
let string_closer = salt ^ "sc";;
let uple_closer = salt ^ "uc";; 
let variant_closer = salt ^ "vc";; 

let record_arrow = salt ^ "ra";;


let array_cat = Assistance_crobj_category_t.Array
and list_cat  = Assistance_crobj_category_t.List
and record_cat = Assistance_crobj_category_t.Record 
and uple_cat = Assistance_crobj_category_t.Uple 
and variant_cat = Assistance_crobj_category_t.Variant;;

let list_for_category_of_lexeme=[
    (array_opener,array_cat); 
    (list_opener,list_cat); 
    (record_opener,record_cat); 
    (uple_opener,uple_cat); 
    (variant_opener,variant_cat); 

    (array_separator,array_cat); 
    (list_separator,list_cat); 
    (record_separator,record_cat); 
    (uple_separator,uple_cat); 
    (variant_separator,variant_cat); 

    (array_closer,array_cat); 
    (list_closer,list_cat); 
    (record_closer,record_cat); 
    (uple_closer,uple_cat); 
    (variant_closer,variant_cat); 

    (record_arrow,record_cat); (* a little bit of convenient convention here *)
];;

let category_of_lexeme lexeme=List.assoc lexeme list_for_category_of_lexeme;;

let list_for_preludeless_increasers=[
    (array_opener,Assistance_crobj_basic_increase_t.Open(Assistance_crobj_opening_t.Array)); 
    (list_opener,Assistance_crobj_basic_increase_t.Open(Assistance_crobj_opening_t.List)); 
    (record_opener,Assistance_crobj_basic_increase_t.Open(Assistance_crobj_opening_t.Record)); 
    (uple_opener,Assistance_crobj_basic_increase_t.Open(Assistance_crobj_opening_t.Uple)); 

    (array_separator,Assistance_crobj_basic_increase_t.Separate(Assistance_crobj_category_t.Array)); 
    (list_separator,Assistance_crobj_basic_increase_t.Separate(Assistance_crobj_category_t.List)); 
    (record_separator,Assistance_crobj_basic_increase_t.Separate(Assistance_crobj_category_t.Record)); 
    (uple_separator,Assistance_crobj_basic_increase_t.Separate(Assistance_crobj_category_t.Uple)); 
    (variant_separator,Assistance_crobj_basic_increase_t.Separate(Assistance_crobj_category_t.Variant)); 

    (array_closer,Assistance_crobj_basic_increase_t.Close(Assistance_crobj_category_t.Array)); 
    (list_closer,Assistance_crobj_basic_increase_t.Close(Assistance_crobj_category_t.List)); 
    (record_closer,Assistance_crobj_basic_increase_t.Close(Assistance_crobj_category_t.Record)); 
    (uple_closer,Assistance_crobj_basic_increase_t.Close(Assistance_crobj_category_t.Uple)); 
    (variant_closer,Assistance_crobj_basic_increase_t.Close(Assistance_crobj_category_t.Variant)); 

];;

exception Unreadable_int of string;;

let parse_int s=try int_of_string s with _->raise(Unreadable_int(s));;

let next_basic_increase_in_variant_opening_case s idx idx1=
   let opening = Assistance_crobj_opening_t.Variant (Assistance_cull_string.interval s idx (idx1-1)) in 
   (Assistance_crobj_basic_increase_t.Open(opening),idx1+(String.length variant_opener));;

let next_basic_increase_in_field_naming_case s idx idx1=
   let name = Assistance_cull_string.interval s idx (idx1-1) in 
   (Assistance_crobj_basic_increase_t.Push_field_name(name),idx1+(String.length record_arrow));;

let next_basic_increase_in_preludy_case s idx idx1=
   if Assistance_substring.is_a_substring_located_at variant_opener s idx1 
   then next_basic_increase_in_variant_opening_case s idx idx1
   else 
   if Assistance_substring.is_a_substring_located_at record_arrow s idx1 
   then next_basic_increase_in_field_naming_case s idx idx1
   else let i=parse_int(Assistance_cull_string.interval s idx (idx1-1)) in 
       (Assistance_crobj_basic_increase_t.Push_int(i),idx1);;


exception Missing_string_closer of int * string;;

let next_basic_increase_in_push_string_case s idx=
   let idx1=idx+(String.length string_opener) in 
   let idx2=Assistance_substring.leftmost_index_of_in_from string_closer s idx1 in 
   if idx2<0
   then raise(Missing_string_closer(idx1,s))
   else
   (* we know that the string is already encoded *)
   let encoded_s = Assistance_encoded_string.retrieve (Assistance_cull_string.interval s idx1 (idx2-1)) in 
   (Assistance_crobj_basic_increase_t.Push_string(encoded_s),idx2+(String.length string_closer));;




exception Unreadable_increase of int * string ;;

let next_basic_increase  s idx=
   let idx1= Assistance_substring.leftmost_index_of_in_from salt s idx in 
   if idx1<0 
   then let i=parse_int (Assistance_cull_string.cobeginning (idx-1) s) in
        (Assistance_crobj_basic_increase_t.Push_int(i),String.length(s)+1)
   else      
   if idx1>idx 
   then next_basic_increase_in_preludy_case s idx idx1
   else 
   if Assistance_substring.is_a_substring_located_at string_opener s idx 
   then next_basic_increase_in_push_string_case s idx
   else 
   match Assistance_option.seek (fun 
      (text,action)->Assistance_substring.is_a_substring_located_at text s idx
   ) list_for_preludeless_increasers with 
   None -> raise(Unreadable_increase(idx,s))
   |Some(text,action)->(action,idx+(String.length text));;

let one_step_more machine =
   let (action,next_idx) =
      next_basic_increase machine.Assistance_crobj_parsing_machine_t.parsed_one 
                            machine.Assistance_crobj_parsing_machine_t.current_index in 
   {
      machine with 
      Assistance_crobj_parsing_machine_t.current_index = next_idx ;
      Assistance_crobj_parsing_machine_t.data = Assistance_double_partial_crobj.increase action machine.Assistance_crobj_parsing_machine_t.data;
   }  ;;

let prudent_push machine = try (None,Some(one_step_more machine)) with 
   Assistance_double_partial_crobj.End_reached(solution) -> (Some solution,None);;

exception First_step_exn of Assistance_crobj_basic_increase_t.t ;; 

let first_step s =
   let (action,next_idx) = next_basic_increase s 1 in 
   match action with 
    Assistance_crobj_basic_increase_t.Push_int(i)->(Some(Assistance_concrete_object_t.Int(i)),None,next_idx)
   |Assistance_crobj_basic_increase_t.Push_string(encoded_s)->(Some(Assistance_concrete_object.wrap_encoded_string(encoded_s)),None,next_idx)
   |Assistance_crobj_basic_increase_t.Open(opening)->(None,Some(Assistance_double_partial_crobj.initialize(opening)),next_idx)
   |_->raise(First_step_exn(action));;

exception Ends_too_soon of Assistance_concrete_object_t.t * string ;; 

let parse s =
    let (opt_quick_result,opt_start,next_idx) = first_step s in 
    match opt_quick_result with 
    Some (res)-> if next_idx < (String.length s)
                 then raise(Ends_too_soon(res,s)) 
                 else res 
    |None -> let start_partial_obj = Assistance_option.unpack opt_start in   
             let machine = {
                Assistance_crobj_parsing_machine_t.parsed_one = s ;
                Assistance_crobj_parsing_machine_t.current_index = next_idx ;
                Assistance_crobj_parsing_machine_t.data = start_partial_obj;
             } in 
             let rec iterator = (fun mach ->
                let (opt_sol,opt_term) = prudent_push mach in 
                match opt_term with 
                None -> Assistance_option.unpack opt_sol 
                |Some(term)->iterator(term) 
             ) in 
             iterator machine;;

let rec unparse = function 
   Assistance_concrete_object_t.Int(i)->string_of_int i 
   |String(t)->string_opener^(Assistance_encoded_string.store t)^string_closer
   |Uple(l)->let temp1=Assistance_image.image unparse l in 
             uple_opener^(String.concat uple_separator temp1)^uple_closer
   |List(l)->let temp1=Assistance_image.image unparse l in 
             list_opener^(String.concat list_separator temp1)^list_closer 
   |Array(l)->let temp1=Assistance_image.image unparse l in 
             array_opener^(String.concat array_separator temp1)^array_closer
   |Record(l)->let temp1=Assistance_image.image (fun (key,vaal)->key ^ record_arrow ^ (unparse vaal))  l in 
             record_opener^(String.concat record_separator temp1)^record_closer          
   |Variant(constructor,l)->let temp1=Assistance_image.image unparse l in 
             constructor^variant_opener^(String.concat variant_separator temp1)^variant_closer ;; 

end;;

let parse = Private.parse;;
let unparse = Private.unparse;;

end;;






module Assistance_isolated_occurrences=struct

(*

#use"isolated_occurrences.ml";;

Used to detect mentions of previously defined names in
the same OCaml module.

An occurrence of a substring is isolated when it 
cannot be extended to a meaningful Ocaml name. So we look at
the surrounding characters, on the left and on the right.


*)

module Private=struct

exception Unclear_left_char of char;;
exception Unclear_right_char of char;;

let rejected_left_chars=
  [
   	'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
    'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
    'u';'v';'w';'x';'y';'z';
    'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
    'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';
    'U';'V';'W';'X';'Y';'Z';
    '0';'1';'2';'3';'4';'5';'6';'7';'8';'9';
    '_';
  ];;

let admitted_left_chars=
  [
   	'(' ; ')' ; ';' ; ' ' ;'\n';'\r';'=';'<';'>';'+';'*';'/';'-'; '.'; ',';
  ];;

let rejected_right_chars=
  [
   	'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
    'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
    'u';'v';'w';'x';'y';'z';
    'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
    'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';
    'U';'V';'W';'X';'Y';'Z';
    '0';'1';'2';'3';'4';'5';'6';'7';'8';'9';
    '_';
  ];;

let admitted_right_chars=
  [
   	'(' ; ')' ; ';' ; ' ' ;'\n';'\r';'=';'<';'>';'+';'*';'/';'-'; '.'; ',';
  ];;

let test_for_left_admissiblity c=
   if List.mem c rejected_left_chars then false else
   if List.mem c admitted_left_chars then true else
   raise(Unclear_left_char(c));;
   
let test_for_right_admissiblity c=
   if List.mem c rejected_right_chars then false else
   if List.mem c admitted_right_chars then true else
   raise(Unclear_right_char(c));;   
   
let leftmost_small_test  s j=
   if j=0 
   then true 
   else test_for_left_admissiblity (String.get s (j-1));;

let rightmost_small_test  s j=
   if j=((String.length s)+1) 
   then true 
   else test_for_right_admissiblity (String.get s (j-1));;   
   

end;;

let of_in substr s=
  let l_substr=String.length substr 
  and n=String.length(s) in
  let main_test= (
    fun k->
      if ((String.sub s (k-1) l_substr)<>substr)
      then false
      else 
      ( Private.leftmost_small_test s (k-1) )
      &&
      ( Private.rightmost_small_test s (k+l_substr) )
      
  ) in
  Assistance_option.filter_and_unpack(
     fun k->
       if main_test k
       then Some(k,k+l_substr-1)
       else None
  ) (Assistance_int_range.range 1 (n+1-l_substr));;

   
(*   
   
of_in "garfield" 
"let x=garfield in let y=subgarfield and z=garfield2 in";;

of_in "garfield" "garfield is a cat";;

of_in "Boogie.Woogie.c" "48+Boogie.Woogie.c";;


*)              

end;;






module Assistance_ocaml_gsyntax_category=struct

(*

#use"Ocaml_analysis/ocaml_gsyntax_category.ml";;

*)

type t=
     Value
    |Type
    |Exception 
    |Module_opener
    |Module_closer 
    |Module_inclusion;;
    
               

end;;






module Assistance_ocaml_gsyntax_item=struct

(*

#use"Ocaml_analysis/ocaml_gsyntax_item.ml";;

*)

type t={
  category : Assistance_ocaml_gsyntax_category.t;
  name : string;
  interval_for_name : int*int;
  whole : string;
  content : string;
  interval_for_content : int*int;  
  is_an_included_item : bool;
};;

let name x=x.name;;
let content x=x.content;;
let whole x=x.whole;;

let make cat nm nm_itv intr ctnt ctnt_itv incldd_or_not=
    {
  		category =cat;
        name =nm;
        interval_for_name =nm_itv;
        whole =intr;
        content =ctnt;
        interval_for_content =ctnt_itv;  
        is_an_included_item =incldd_or_not;
    };;

let prepend_prefix prefix x=
    {
  		category =x.category;
        name =prefix^"."^x.name;
        interval_for_name =x.interval_for_name;
        whole =x.whole;
        content =x.content;
        interval_for_content =x.interval_for_content;  
        is_an_included_item =x.is_an_included_item;
    };;
    
let include_in_new_scope new_scope x=
    {
  		category =x.category;
        name =new_scope^(Assistance_cull_string.before_rightmost_possibly_all x.name '.');
        interval_for_name =x.interval_for_name;
        whole =x.whole;
        content =x.content;
        interval_for_content =x.interval_for_content;  
        is_an_included_item =true;
    };;    
    
let make_name_coincide_with_content x=
        {
            category =x.category;
            name =x.content;
            interval_for_name =x.interval_for_name;
            whole =x.whole;
            content =x.content;
            interval_for_content =x.interval_for_content;  
            is_an_included_item =x.is_an_included_item;
        };;    
    
    
    
    
    
    
               

end;;






module Assistance_overwrite_at_intervals=struct

(*

#use"overwrite_at_intervals.ml";;

*)



let inside_string replacings s=
  let n=String.length s
  and r=List.length replacings in
  let x_coord=(fun j->
    if j=1 then 1 else
    snd(fst(List.nth replacings ((j-3)/2)))+1
  ) and y_coord=(fun j->
   if j=2*r+1 then n else
    fst(fst(List.nth replacings ((j-1)/2)))-1
  ) in
  let xy_substring=(fun j->
    Assistance_cull_string.interval s (x_coord j) (y_coord j)
  ) in
  let all_parts=Assistance_int_range.scale (
    fun j->
      if (j mod 2)=1
      then xy_substring j
      else Assistance_overwriter.to_string(snd(List.nth replacings ((j-2)/2)))
  ) 1 (2*r+1) in
  String.concat "" all_parts;;

(*

inside_string
 [(7,12),"garfield";(23,24),"jack";(30,30),"gas"]
 "12345678901234567890123456789012345678901234567890";;
 

*)

let inside_file replacings fn=
  let old_t=Assistance_io.read_whole_file fn in
  let new_t=inside_string replacings old_t in
  Assistance_io.overwrite_with fn new_t;;  
  





           

end;;






module Assistance_gparser=struct

(*

#use"GParser/gparser.ml";;

*)

type t=
     Constant of string
    |Enclosure of string*string
    |Footless_constant of string
    |Sample_char of string
    |Sample_neg of string
    |Sample_star of string
    |Sample_negstar of string
    |Sample_plus of string
    |Race of string*string
    |Comment of string*string*string*string
    |House_with_doors of string*string*((string*string) list)
    |Chain of t list
    |Disjunction of t list
    |Star of t
    |Detailed_star of t
    |One_or_more of t
    |Optional of t
    |Recoiling_ending of t*t
    |Detailed_chain of t list
;;
           

end;;






module Assistance_gparser_result=struct

(*

#use"GParser/gparser_result.ml";;

*)


type t={
   whole_range : int*int ;
   important_ranges : (int*int) list;
   final_cursor_position : int; 
   disjunction_index : int option;
};;

let whole_range x=x.whole_range;;
let important_ranges x=x.important_ranges;;
let final_cursor_position x=x.final_cursor_position;;
let disjunction_index x=x.disjunction_index;;

let veil b c d e={
   whole_range =b;
   important_ranges =c;
   final_cursor_position =d; 
   disjunction_index=e;
};;


           

end;;






module Assistance_gparser_fun=struct

(*

#use"GParser/gparser_fun.ml";;

*)

type t=(string->int->(Assistance_gparser_result.t option));;
           

end;;






module Assistance_gparser_house_with_doors=struct

(*

#use"GParser/gparser_house_with_doors.ml";;

*)

module Private=struct
type mistletoe={
     main_opener:string;
     main_closer:string;
     other_enclosers: (string*string) list;
     processed_argument: string;
     initial_index: int;
};;

type walker={
    current_index : int;
    current_depth : int;
    awaited_closer : string option;
    answer : Assistance_gparser_result.t option;
};;


let new_walker_in_first_case_in_hwd (m,wlkr)=
   let opt1=Assistance_option.seek(fun (opener,closer)->
     Assistance_substring.is_a_substring_located_at opener 
        m.processed_argument wlkr.current_index
   ) m.other_enclosers in
   if opt1<>None
   then let (op1,cl1)=Assistance_option.unpack opt1 in
        {
        	current_index =wlkr.current_index+(String.length op1);
            current_depth =wlkr.current_depth;
            awaited_closer=Some(cl1);
            answer =None;
        }
   else
   if Assistance_substring.is_a_substring_located_at m.main_opener 
        m.processed_argument wlkr.current_index
   then {
        	current_index =wlkr.current_index+(String.length m.main_opener);
            current_depth =wlkr.current_depth+1;
            awaited_closer=None;
            answer =None;
        }
   else   
   if not(Assistance_substring.is_a_substring_located_at m.main_closer 
      m.processed_argument wlkr.current_index)
   then {
        	current_index =wlkr.current_index+1;
            current_depth =wlkr.current_depth;
            awaited_closer=None;
            answer =None;
        }
   else    
   if wlkr.current_depth>1
   then {
        	current_index =wlkr.current_index+(String.length m.main_closer);
            current_depth =wlkr.current_depth-1;
            awaited_closer=None;
            answer =None;
        }
   else  
   let j1=wlkr.current_index+(String.length m.main_closer) in
   let res=Assistance_gparser_result.veil
               (m.initial_index,j1-1)
               []
               j1
               None        in
        {
        	current_index =wlkr.current_index+(String.length m.main_closer);
            current_depth =wlkr.current_depth-1;
            awaited_closer=None;
            answer =Some(res);
        };;
   

let first_case_in_hwd 
   (m,wlkr)=(m,new_walker_in_first_case_in_hwd (m,wlkr));;
  
let second_case_in_hwd (m,wlkr)=
  let rparen=Assistance_option.unpack wlkr.awaited_closer in
  if Assistance_substring.is_a_substring_located_at rparen 
      m.processed_argument wlkr.current_index
  then (m,{
        	current_index =wlkr.current_index+(String.length rparen);
            current_depth =wlkr.current_depth;
            awaited_closer=None;
            answer =None;
        })
  else (m,{
        	current_index =wlkr.current_index+1;
            current_depth =wlkr.current_depth;
            awaited_closer=wlkr.awaited_closer;
            answer =None;
        });;

let pusher_for_hwd w=
  let (m,wlkr)=w in
  if wlkr.answer=None
  then 
       (
         if wlkr.awaited_closer=None
         then first_case_in_hwd w
         else second_case_in_hwd w       
        )
  else w;;  
   
let rec iterator_for_hwd w=
   let (m,wlkr)=w in
   if wlkr.answer<>None
   then wlkr.answer
   else
   if wlkr.current_index>(String.length m.processed_argument)
   then None
   else iterator_for_hwd (pusher_for_hwd w);; 
  
let starter_for_hwd (main_opener,main_closer) other_enclosers s i=
  (
    {
     main_opener=main_opener;
     main_closer=main_closer;
     other_enclosers=other_enclosers;
     processed_argument=s;
     initial_index=i;
    }
  ,
    {
      current_index =i+(String.length main_opener);
      current_depth =1;
      awaited_closer=None;
      answer =None;
    }
  );;
end;;  

let hwd
   (main_opener,main_closer)
     other_enclosers=
   let rec tempf=(fun s i->
        if not(Assistance_substring.is_a_substring_located_at main_opener s i)
        then None 
        else 
          
          Private.iterator_for_hwd 
         (Private.starter_for_hwd (main_opener,main_closer) other_enclosers s i)
   ) in
   (tempf:Assistance_gparser_fun.t);;      
      
(*

hwd ("(*","*)") ["\"","\""] "(* Bye \"*)\" bye bird *)456" 1;;

*)



         
   
           

end;;






module Assistance_gparser_ocaml_comment=struct

(*

#use"GParser/gparser_ocaml_comment.ml";;

*)

module Private=struct
type mistletoe={
     comment_opener:string;
     comment_closer:string;
     quote_opener:string;
     quote_closer:string;
     processed_argument: string;
     initial_index: int;
};;

type walker={
    current_index : int;
    current_depth : int;
    quote_mode : bool;
    answer : Assistance_gparser_result.t option;
    length_of_preceding_backslash_wall :int;
};;

let update_backslash_wall_length (m,wlkr)=
if (Assistance_strung.get m.processed_argument wlkr.current_index)='\\'
then (wlkr.length_of_preceding_backslash_wall)+1
else 0;;


let new_walker_in_first_case_in_hwd (m,wlkr)=
   if Assistance_substring.is_a_substring_located_at
     m.quote_opener m.processed_argument wlkr.current_index
   then {
        	current_index =wlkr.current_index+(String.length m.quote_opener);
            current_depth =wlkr.current_depth;
            quote_mode=true;
            answer =None;
            length_of_preceding_backslash_wall=0;
        }
   else
   if Assistance_substring.is_a_substring_located_at m.comment_opener 
        m.processed_argument wlkr.current_index
   then {
        	current_index =wlkr.current_index+(String.length m.comment_opener);
            current_depth =wlkr.current_depth+1;
            quote_mode=false;
            answer =None;
            length_of_preceding_backslash_wall=0;
        }
   else   
   if not(Assistance_substring.is_a_substring_located_at m.comment_closer 
      m.processed_argument wlkr.current_index)
   then {
        	current_index =wlkr.current_index+1;
            current_depth =wlkr.current_depth;
            quote_mode=false;
            answer =None;
            length_of_preceding_backslash_wall=
               update_backslash_wall_length(m,wlkr);
        }
   else    
   if wlkr.current_depth>1
   then {
        	current_index =wlkr.current_index+(String.length m.comment_closer);
            current_depth =wlkr.current_depth-1;
            quote_mode=false;
            answer =None;
            length_of_preceding_backslash_wall=0;
        }
   else  
   let j1=wlkr.current_index+(String.length m.comment_closer) in
   let res=Assistance_gparser_result.veil
               (m.initial_index,j1-1)
               []
               j1
               None        in
        {
        	current_index =wlkr.current_index+(String.length m.comment_closer);
            current_depth =wlkr.current_depth-1;
            quote_mode=false;
            answer =Some(res);
            length_of_preceding_backslash_wall=0;
        };;
   

let first_case_in_hwd 
   (m,wlkr)=(m,new_walker_in_first_case_in_hwd (m,wlkr));;
  
let second_case_in_hwd (m,wlkr)=
  if (Assistance_substring.is_a_substring_located_at m.quote_closer
      m.processed_argument wlkr.current_index)
     &&
     ((wlkr.length_of_preceding_backslash_wall mod 2)=0) 
  then (m,{
        	current_index =wlkr.current_index+(String.length m.quote_closer);
            current_depth =wlkr.current_depth;
            quote_mode=false;
            answer =None;
            length_of_preceding_backslash_wall=0;
        })
  else (m,{
        	current_index =wlkr.current_index+1;
            current_depth =wlkr.current_depth;
            quote_mode=true;
            answer =None;
            length_of_preceding_backslash_wall=
               update_backslash_wall_length(m,wlkr);
        });;

let pusher_for_hwd w=
  let (m,wlkr)=w in
  if wlkr.answer=None
  then 
       (
         if not(wlkr.quote_mode)
         then first_case_in_hwd w
         else second_case_in_hwd w       
        )
  else w;;  
   
let rec iterator_for_main_prsr w=
   let (m,wlkr)=w in
   if wlkr.answer<>None
   then wlkr.answer
   else
   if wlkr.current_index>(String.length m.processed_argument)
   then None
   else iterator_for_main_prsr (pusher_for_hwd w);; 
  
let starter_for_main_prsr 
(comment_opener,comment_closer) 
 (quote_opener,quote_closer) s i=
  (
    {
     comment_opener=comment_opener;
     comment_closer=comment_closer;
     quote_opener=quote_opener;
     quote_closer=quote_closer;
     processed_argument=s;
     initial_index=i;
    }
  ,
    {
      current_index =i+(String.length comment_opener);
      current_depth =1;
      quote_mode=false;
      answer =None;
      length_of_preceding_backslash_wall=0;
    }
  );;
end;;  

let main_prsr
   (comment_opener,comment_closer)
     (quote_opener,quote_closer)=
   let rec tempf=(fun s i->
        if not(Assistance_substring.is_a_substring_located_at comment_opener s i)
        then None 
        else 
          
          Private.iterator_for_main_prsr 
         (Private.starter_for_main_prsr (comment_opener,comment_closer) 
            (quote_opener,quote_closer) s i)
   ) in
   (tempf:Assistance_gparser_fun.t);;      
      
(*

main_prsr ("(*","*)") ("\"","\"") "(* Bye \"*)\" bye bird *)456" 1;;
main_prsr ("(*","*)") ("\"","\"") "(* Bye \"uu\" bye bird *)456" 1;;
main_prsr ("(*","*)") ("\"","\"") "(* Bye \"uu\\\" *) \"  *)234" 1;;


*)



         
   
           

end;;






module Assistance_gparser_apply=struct

(*

#use"GParser/gparser_apply.ml";;

*)

module Private=struct

let enclosure (left_encloser,right_encloser)=
   let tempf=(fun s i1->
   if (not(Assistance_substring.is_a_substring_located_at left_encloser s i1))
   then None
   else 
   let i2=i1+(String.length left_encloser) in
   let i3=Assistance_substring.leftmost_index_of_in_from right_encloser s i2 in
   if i3<1
   then None 
   else
   let i4=i3+(String.length right_encloser)-1 in
   let res= Assistance_gparser_result.veil
               (i1,i4)
               [i2,i3-1]
               (i4+1)
               None in
   Some(res)) in
   (tempf: Assistance_gparser_fun.t);;
   
let constant t=
   let tempf=(fun s i1->
   if (not(Assistance_substring.is_a_substring_located_at t s i1))
   then None
   else 
   let i2=i1+(String.length t) in
   let res= Assistance_gparser_result.veil
               (i1,i2-1)
               []
               i2
               None in
   Some(res)) in
   (tempf: Assistance_gparser_fun.t);;


let footless_constant t=
   let tempf=(fun s i1->
   if (not(Assistance_substring.is_a_substring_located_at t s i1))
   then None
   else 
   let i2=i1+(String.length t) in
   let res= Assistance_gparser_result.veil
               (i1,i2-1)
               []
               (i2-1)
               None in
   Some(res)) in
   (tempf:Assistance_gparser_fun.t);;

let sample_char t=
   let lc=Assistance_strung.explode t in
   let tempf=(fun s i->
        let c=Assistance_strung.get s i in
        if List.mem c lc
        then Some(Assistance_gparser_result.veil
               (i,i)
               []
               (i+1)
               None)
        else None) in
   (tempf:Assistance_gparser_fun.t);;

let sample_neg t=
   let lc=Assistance_strung.explode t in
   let tempf=(fun s i->
        let c=Assistance_strung.get s i in
        if not(List.mem c lc)
        then Some(Assistance_gparser_result.veil
               (i,i)
               []
               (i+1)
               None)
        else None) in
   (tempf:Assistance_gparser_fun.t);;

let sample_star t=
   let lc=Assistance_strung.explode t in
   let tempf=(fun s i1->
        let j=Assistance_strung.char_finder_from (fun c->not(List.mem c lc)) s i1 in
        let better_j=(if j<1 then (String.length s)+1 else j) in
        let res=Assistance_gparser_result.veil
               (i1,better_j-1)
               []
               better_j
               None in
   Some(res)) in
   (tempf:Assistance_gparser_fun.t);;

let sample_negstar t=
   let lc=Assistance_strung.explode t in
   let tempf=(fun s i1->
        let j=Assistance_strung.char_finder_from (fun c->List.mem c lc) s i1 in
        let better_j=(if j<1 then (String.length s)+1 else j) in
        let res=Assistance_gparser_result.veil
               (i1,better_j-1)
               []
               better_j
               None in
   Some(res)) in
   (tempf:Assistance_gparser_fun.t);;

let sample_plus t=
   let lc=Assistance_strung.explode t in
   let tempf=(fun s i1->
        if i1>(String.length s) then None else
        if (not(List.mem (Assistance_strung.get s i1 ) lc)) then None else
        let j=Assistance_strung.char_finder_from (fun c->not(List.mem c lc)) s i1 in
        let better_j=(if j<1 then (String.length s)+1 else j) in
        let res=Assistance_gparser_result.veil
               (i1,better_j-1)
               []
               better_j
               None in
   Some(res)) in
   (tempf:Assistance_gparser_fun.t);;
   

let race (continuer,finalizer)=
   let rec tempf=(fun (s,i1,k)->
        if k>(String.length s)
        then None
        else
        if Assistance_substring.is_a_substring_located_at continuer s k
        then tempf(s,i1,k+(String.length continuer))
        else
        if (not(Assistance_substring.is_a_substring_located_at finalizer s k))
        then tempf(s,i1,k+1)
        else
        let j1=k+(String.length finalizer) in
        let res=Assistance_gparser_result.veil
               (i1,j1-1)
               []
               (j1-1)
               None in
        Some(res)) in
   ((fun s i->tempf(s,i,i)):Assistance_gparser_fun.t);;   
      
let house_with_doors=Assistance_gparser_house_with_doors.hwd;;


type chain_artefact=
     Usual of (int * int) list * Assistance_gparser_fun.t list * bytes * int * int 
    |Result_found of Assistance_gparser_result.t
    |Failure_found;;

let chain l=
  let main_f=
  	(fun s i->
   		let rec tempf=
   		(
         	fun (imp_ranges,da_ober,s,i0,k)->
      		match da_ober with
      		[]->Some(
           		    	Assistance_gparser_result.veil
               			(i0,k-1)
               			imp_ranges
               			k
               			None
          			)
       		|prsr::rest->   
         		(
           			match prsr s k with
            		None->None
           		  |Some(res)->tempf(
           		       imp_ranges@(Assistance_gparser_result.important_ranges res),
                       rest,s,i0,Assistance_gparser_result.final_cursor_position res)
                )
         )  
    in tempf([],l,s,i,i)
    ) in
  (main_f:Assistance_gparser_fun.t);;

let detailed_chain l=
  let main_f=
  	(fun s i->
   		let rec tempf=
   		(
         	fun (imp_ranges,da_ober,s,i0,k)->
      		match da_ober with
      		[]->Some(
           		    	Assistance_gparser_result.veil
               			(i0,k-1)
               			(List.rev imp_ranges)
               			k
               			None
          			)
       		|prsr::rest->   
         		(
           			match prsr s k with
            		None->None
           		  |Some(res)->tempf(
           		       (Assistance_gparser_result.whole_range res)::imp_ranges,
                       rest,s,i0,Assistance_gparser_result.final_cursor_position res)
                )
         )  
    in tempf([],l,s,i,i)
    ) in
  (main_f:Assistance_gparser_fun.t);;

let debugful_detailed_chain l=
  let main_f=
  	(fun s i->
   		let rec tempf=
   		(
         	fun (imp_ranges,da_ober,s,i0,k,opt)->
      		match da_ober with
      		[]->let sol=Some(
           		    	Assistance_gparser_result.veil
               			(i0,k-1)
               			(List.rev imp_ranges)
               			k
               			None
          			) in
          	     (imp_ranges,da_ober,s,i0,k,sol) 		
       		|prsr::rest->   
         		(
           			match prsr s k with
            		None->(imp_ranges,da_ober,s,i0,k,opt)
           		  |Some(res)->tempf(
           		       (Assistance_gparser_result.whole_range res)::imp_ranges,
                       rest,s,i0,Assistance_gparser_result.final_cursor_position res,None)
                )
         )  
    in tempf([],l,s,i,i,None)
    ) in
  main_f;;

let disjunction l=
   let indexed_l=Assistance_int_range.index_everything l in   
   let rec tempf=(fun
   (da_ober,s,i0)->
      match da_ober with
      []->None 
      |(j,prsr)::rest->
         (
           match prsr s i0 with
             None->tempf(rest,s,i0)
           |Some(res)->
          Some(
             Assistance_gparser_result.veil
               (Assistance_gparser_result.whole_range res)
               (Assistance_gparser_result.important_ranges res)
               (Assistance_gparser_result.final_cursor_position res)
               (Some j)
           )
         )   
   ) in
   ((fun s i->tempf (indexed_l,s,i)):Assistance_gparser_fun.t);;

let star prsr=
   let rec tempf=(fun
   (imp_ranges,s,i0,k)->
      match prsr s k with
       None->Some(
             Assistance_gparser_result.veil
               (i0,k-1)
               (imp_ranges)
               k
               None
            )
      |Some(res)->tempf(imp_ranges@(Assistance_gparser_result.important_ranges res),
                       s,i0,Assistance_gparser_result.final_cursor_position res)
   
   ) in
   ((fun s i->tempf ([],s,i,i)):Assistance_gparser_fun.t);;

let detailed_star prsr=
   let rec tempf=(fun
   (imp_ranges,s,i0,k)->
      match prsr s k with
       None->Some(
             Assistance_gparser_result.veil
               (i0,k-1)
               (List.rev(imp_ranges))
               k
               None
            )
      |Some(res)->tempf((Assistance_gparser_result.whole_range res)::imp_ranges,
                       s,i0,Assistance_gparser_result.final_cursor_position res)
   
   ) in
   ((fun s i->tempf ([],s,i,i)):Assistance_gparser_fun.t);;   
   
   
let one_or_more prsr=chain [prsr;star prsr];;

let optional prsr=
   let rec tempf=(fun s i->
      match prsr s i with
       Some(res)->Some(
            Assistance_gparser_result.veil
               (Assistance_gparser_result.whole_range res)
               (Assistance_gparser_result.important_ranges res)
               (Assistance_gparser_result.final_cursor_position res)
               None
            )
      |None->Some(
            Assistance_gparser_result.veil
               (i,i-1)
               []
               i
               None
            )
   
   ) in
   (tempf:Assistance_gparser_fun.t);;


let recoiling_ending x y=
   let tempf=(fun s i->
      match x s i with
       None->None
      |Some(res)->
                  
                  let j=Assistance_gparser_result.final_cursor_position res in
                  if y s j=None then None else
                  Some(
                  Assistance_gparser_result.veil
                  (i,j-1)
                  (Assistance_gparser_result.important_ranges res)
                  j
                  None
                  )
   ) in
   (tempf:Assistance_gparser_fun.t);;
     
let rec apply=function        
     Assistance_gparser.Constant(s)->constant s
    |Assistance_gparser.Enclosure(s1,s2)->enclosure (s1,s2)
    |Assistance_gparser.Footless_constant(s)->footless_constant s
    |Assistance_gparser.Sample_char(s)->sample_char s
    |Assistance_gparser.Sample_neg(s)->sample_neg s
    |Assistance_gparser.Sample_star(s)->sample_star s
    |Assistance_gparser.Sample_negstar(s)->sample_negstar s
    |Assistance_gparser.Sample_plus(s)->sample_plus s
    |Assistance_gparser.Race(s1,s2)->race(s1,s2)
    |Assistance_gparser.Comment(s1,s2,s3,s4)->Assistance_gparser_ocaml_comment.main_prsr(s1,s2)(s3,s4)
    |Assistance_gparser.House_with_doors(s1,s2,l)->house_with_doors (s1,s2) l
    |Assistance_gparser.Chain(l)->chain(Assistance_image.image apply l)
    |Assistance_gparser.Disjunction(l)->disjunction(Assistance_image.image apply l)
    |Assistance_gparser.Star(x)->star(apply x)
    |Assistance_gparser.Detailed_star(x)->detailed_star(apply x)
    |Assistance_gparser.One_or_more(x)->one_or_more(apply x)
    |Assistance_gparser.Optional(x)->optional(apply x)
    |Assistance_gparser.Recoiling_ending(x,y)->recoiling_ending (apply x) (apply y)
    |Assistance_gparser.Detailed_chain(l)->detailed_chain(Assistance_image.image apply l);;
   
end;;   
   
let apply=Private.apply;;   
   
(*


*)   
   
           

end;;






module Assistance_list_with_indices=struct

(*

#use"list_with_indices.ml";;

*)

exception Bad_set_of_indices;;

let list_with_indices l=
  let n=List.length l in
  let temp1=Assistance_int_range.scale (fun i->Assistance_option.seek(fun p->fst(p)=i) l) 1 n in
  if List.mem None temp1
  then raise(Bad_set_of_indices)
  else
  Assistance_int_range.scale (fun
     i->snd(Assistance_listennou.force_find(fun p->fst(p)=i) l)
  ) 1 n;;

(*

list_with_indices [3,"a";1,"b";2,"c"];;

*)  
           

end;;






module Assistance_gparser_for_ocaml_language=struct

(*

#use"GParser/gparser_for_ocaml_language.ml";;

*)

let double_semicolon=Assistance_particular_string.double_semicolon;;

let prsr_for_comment=
  Assistance_gparser.Comment ("(*","*)","\"","\"");;


let prsr_for_sharp_comment=Assistance_gparser.Enclosure ("\n#","\n");;

let prsr_for_space=Assistance_gparser.Constant " ";;
let prsr_for_tab=Assistance_gparser.Constant "\t";;


let prsr_for_space_or_tab=Assistance_gparser.Disjunction [prsr_for_space;prsr_for_tab];;
let prsr_for_linebreak=Assistance_gparser.Constant "\n";;
let prsr_for_newline=Assistance_gparser.Constant "\012";;
let prsr_for_windows_newline=Assistance_gparser.Constant "\r";;
let prsr_for_individual_white=Assistance_gparser.Disjunction 
[prsr_for_space;prsr_for_tab;prsr_for_linebreak;prsr_for_newline;prsr_for_windows_newline];;

let prsr_for_inline_white_maybe=Assistance_gparser.Star prsr_for_space_or_tab;;
let prsr_for_white_maybe=Assistance_gparser.Star prsr_for_individual_white;;
let prsr_for_white=Assistance_gparser.One_or_more prsr_for_individual_white;;

let prsr_for_special_sharp=Assistance_gparser.Chain
   [
     Assistance_gparser.Constant "#";
     prsr_for_inline_white_maybe;
     Assistance_gparser.Sample_star "0123456789";
     prsr_for_inline_white_maybe;
     Assistance_gparser.Constant "\"";
     Assistance_gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ/.";
     Assistance_gparser.Constant "\"";
     prsr_for_inline_white_maybe;
   ];;

let prsr_for_uncapitalized_word=Assistance_gparser.Chain
   [
     Assistance_gparser.Sample_char "abcdefghijklmnopqrstuvwxyz_";
     Assistance_gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
   ];;

let prsr_for_capitalized_word=Assistance_gparser.Chain
   [
     Assistance_gparser.Sample_char "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
     Assistance_gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ012356789";
   ];;

let prsr_for_pointing_module=Assistance_gparser.Chain
   [
     prsr_for_capitalized_word;
     Assistance_gparser.Constant ".";
   ];;

let prsr_for_wholly_lowercase_name=
   Assistance_gparser.Chain
   [
     Assistance_gparser.Sample_char "abcdefghijklmnopqrstuvwxyz_";
     Assistance_gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_";
   ];;


let prsr_for_element_in_uple_in_typedef=
   Assistance_gparser.Chain
   [
     Assistance_gparser.Constant "'";
      prsr_for_uncapitalized_word; 
     prsr_for_white_maybe; 
     Assistance_gparser.Constant ",";
     prsr_for_white_maybe; 
   ];;

let prsr_for_parameters1_in_type=
   Assistance_gparser.Chain
   [
     Assistance_gparser.Constant "'";
      prsr_for_uncapitalized_word; 
     prsr_for_white_maybe; 
   ];;

let prsr_for_parameters2_in_type=
   Assistance_gparser.Chain
   [
     Assistance_gparser.Constant "(";
     prsr_for_white_maybe; 
     Assistance_gparser.Star(prsr_for_element_in_uple_in_typedef);
     prsr_for_white_maybe; 
     Assistance_gparser.Constant "'";
     prsr_for_uncapitalized_word; 
     prsr_for_white_maybe; 
     Assistance_gparser.Constant ")";
     prsr_for_white_maybe; 
   ];;

   

let prsr_for_parameters_in_type=
   Assistance_gparser.Disjunction
   [
     prsr_for_parameters1_in_type;
     prsr_for_parameters2_in_type;
   ];;

let prsr_for_rec_followed_by_white=Assistance_gparser.Chain
   [
     Assistance_gparser.Optional(Assistance_gparser.Constant "rec");
     prsr_for_white;
   ];;
  

module Private=struct
let list_for_value_making=
   [
     Assistance_gparser.Constant "let";
     prsr_for_white;
     Assistance_gparser.Optional(prsr_for_rec_followed_by_white);
     prsr_for_uncapitalized_word;
     prsr_for_white_maybe;
     Assistance_gparser.Enclosure ("","=");
     Assistance_gparser.Enclosure ("",double_semicolon);
   ];;
  
end;;

let index_for_name_in_value_parser=Assistance_listennou.find_index
   prsr_for_uncapitalized_word Private.list_for_value_making;;

let index_for_content_in_value_parser=Assistance_listennou.find_index
   (Assistance_gparser.Enclosure ("",double_semicolon)) Private.list_for_value_making;; 
   

let prsr_for_value_making=Assistance_gparser.Detailed_chain
   Private.list_for_value_making;;

let prsr_for_type_making=Assistance_gparser.Detailed_chain
   [
     Assistance_gparser.Constant "type";
     prsr_for_white;
     Assistance_gparser.Optional(prsr_for_parameters_in_type);
     prsr_for_uncapitalized_word;
     prsr_for_white_maybe;
     Assistance_gparser.Enclosure ("","=");
     Assistance_gparser.Enclosure ("",double_semicolon);
   ];;



let prsr_for_exception_making=Assistance_gparser.Detailed_chain
     [
     Assistance_gparser.Constant "exception";
     prsr_for_white;
     prsr_for_capitalized_word;
     Assistance_gparser.Enclosure ("",double_semicolon);
   ];;

let prsr_for_module_opener=
   Assistance_gparser.Detailed_chain
   [
     Assistance_gparser.Constant "module";
     prsr_for_white;
     prsr_for_capitalized_word;
     prsr_for_white_maybe;
     Assistance_gparser.Constant "=";
     prsr_for_white_maybe;
     Assistance_gparser.Constant "struct";
   ];;

let prsr_for_module_closer=
   Assistance_gparser.Chain
   [
     Assistance_gparser.Constant "end";
     prsr_for_white_maybe;
     Assistance_gparser.Constant double_semicolon;
   ];;

let prsr_for_module_inclusion=
   Assistance_gparser.Detailed_chain
   [
     Assistance_gparser.Constant "include ";
     prsr_for_white_maybe;
     prsr_for_capitalized_word;
     prsr_for_white_maybe;
     Assistance_gparser.Constant double_semicolon;
   ];;

let prsr_for_special_names=
   Assistance_gparser.Disjunction
     [
       Assistance_gparser.Constant "add_to_vvv ";
       Assistance_gparser.Constant "add_data ";
       Assistance_gparser.Constant "add_data\n";
       Assistance_gparser.Constant "add_label ";
       Assistance_gparser.Constant "add_recognizer ";
       Assistance_gparser.Constant "add_shortcut ";
       Assistance_gparser.Constant "define_precedence_set ";
       Assistance_gparser.Constant "get_name_for_set ";
     ];;   
   
let prsr_for_specialities=Assistance_gparser.Chain
   [
     prsr_for_special_names;
     Assistance_gparser.Enclosure ("",double_semicolon);
   ];;   

let index_for_value=1;;
let index_for_type=2;;
let index_for_exception=3;;
let index_for_comment=4;;
let index_for_sharp_comment=5;;
let index_for_special_sharp=6;;
let index_for_module_opener=7;;
let index_for_module_closer=8;;
let index_for_module_inclusion=9;;
let index_for_specialities=10;;
let index_for_white=11;;


let elt_prsr=Assistance_gparser.Disjunction 
  (
     Assistance_list_with_indices.list_with_indices
     [
       index_for_value           ,prsr_for_value_making;
       index_for_type            ,prsr_for_type_making;
       index_for_exception       ,prsr_for_exception_making;
       index_for_comment         ,prsr_for_comment;
       index_for_sharp_comment   ,prsr_for_sharp_comment;
       index_for_special_sharp   ,prsr_for_special_sharp;
       index_for_module_opener   ,prsr_for_module_opener;
       index_for_module_closer   ,prsr_for_module_closer;
       index_for_module_inclusion,prsr_for_module_inclusion;
       index_for_specialities    ,prsr_for_specialities;
       index_for_white           ,prsr_for_white;
     ]
   )
;;


let main_prsr=
   Assistance_gparser.Detailed_star elt_prsr;;



   
           

end;;






module Assistance_pre_read_ocaml_files=struct

(*

#use"Ocaml_analysis/pre_read_ocaml_files.ml";;

Originated as the code shared by modules
read_ocaml_files and read_ocaml_files_without_expanding_inclusions.

*)

exception Pre_read_exn of string;;

module Private=struct
  exception Unreadable of string;;
  
  let accuse_final_excerpt s i=
    let j=min(String.length s)(i+100) in
    raise(Unreadable(Assistance_cull_string.interval s i j));;
  
  let uncatched_read1 s=
    let opt=Assistance_gparser_apply.apply Assistance_gparser_for_ocaml_language.main_prsr s 1 in
    if opt=None then accuse_final_excerpt s 1 else
    let res=Assistance_option.unpack opt in 
    let p=Assistance_gparser_result.final_cursor_position res in
    if p<=(String.length s) 
    then accuse_final_excerpt s p
    else 
    let temp1=Assistance_gparser_result.important_ranges res in
    Assistance_image.image (fun (i,j)->
      let opt=Assistance_gparser_apply.apply Assistance_gparser_for_ocaml_language.elt_prsr s i in
      let res=Assistance_option.unpack opt in
      ((i,j),Assistance_option.unpack(Assistance_gparser_result.disjunction_index res))
    ) temp1;;
  
  exception Read1_exn of string;;
  
  let read1 s= try uncatched_read1 s with Unreadable(t)->raise(Read1_exn(t));;
    
  let describe_value_item s (i,j)=
       let opt=Assistance_gparser_apply.apply Assistance_gparser_for_ocaml_language.prsr_for_value_making s i in
       let res=Assistance_option.unpack opt in
       let (i1,j1)=List.nth(Assistance_gparser_result.important_ranges res) 
            (Assistance_gparser_for_ocaml_language.index_for_name_in_value_parser-1)
       and (i2,j2)=List.nth(Assistance_gparser_result.important_ranges res) 
            (Assistance_gparser_for_ocaml_language.index_for_content_in_value_parser-1) 
       and (i3,j3)=Assistance_gparser_result.whole_range res in
         Assistance_ocaml_gsyntax_item.make
            Assistance_ocaml_gsyntax_category.Value
            (Assistance_cull_string.interval s i1 j1)
            (i1,j1)
            (Assistance_cull_string.interval s i3 j3)
            (* the -2 of because of the 2 characters in the double semicolon *)
            (Assistance_cull_string.interval s i2 (j2-2))
            (i2,j2-2)
            false;;
  
  let describe_type_item s (i,j)=
       let opt=Assistance_gparser_apply.apply Assistance_gparser_for_ocaml_language.prsr_for_type_making s i in
       let res=Assistance_option.unpack opt in
       let (i1,j1)=List.nth(Assistance_gparser_result.important_ranges res) 3
       and (i2,j2)=List.nth(Assistance_gparser_result.important_ranges res) 6 
       and (i3,j3)=Assistance_gparser_result.whole_range res in
         Assistance_ocaml_gsyntax_item.make
            Assistance_ocaml_gsyntax_category.Type
            (Assistance_cull_string.interval s i1 j1)
            (i1,j1)
            (Assistance_cull_string.interval s i3 j3)
            (* the -2 of because of the 2 characters in the double semicolon *)
            (Assistance_cull_string.interval s i2 (j2-2))
            (i2,j2-2)
            false;;
  
  let describe_exception_item s (i,j)=
       let opt=Assistance_gparser_apply.apply Assistance_gparser_for_ocaml_language.prsr_for_exception_making s i in
       let res=Assistance_option.unpack opt in
       let (i1,j1)=List.nth(Assistance_gparser_result.important_ranges res) 2
       and (i2,j2)=List.nth(Assistance_gparser_result.important_ranges res) 3 
       and (i3,j3)=Assistance_gparser_result.whole_range res in
         Assistance_ocaml_gsyntax_item.make
            Assistance_ocaml_gsyntax_category.Exception
            (Assistance_cull_string.interval s i1 j1)
            (i1,j1)
            (Assistance_cull_string.interval s i3 j3)
            (* the -2 of because of the 2 characters in the double semicolon *)
            (Assistance_cull_string.interval s i2 (j2-2))
            (i2,j2-2)
            false;;
  
  let describe_module_opener_item s (i,j)=
       let opt=Assistance_gparser_apply.apply Assistance_gparser_for_ocaml_language.prsr_for_module_opener s i in
       let res=Assistance_option.unpack opt in
       let (i1,j1)=List.nth(Assistance_gparser_result.important_ranges res) 2
       and (i3,j3)=Assistance_gparser_result.whole_range res in 
         Assistance_ocaml_gsyntax_item.make
            Assistance_ocaml_gsyntax_category.Module_opener
            (Assistance_cull_string.interval s i1 j1)
            (i1,j1)
            (Assistance_cull_string.interval s i3 j3)
            ""
            (0,0)
            false;;
  
  
  let describe_module_closer_item=
         Assistance_ocaml_gsyntax_item.make
            Assistance_ocaml_gsyntax_category.Module_closer
            ""
            (0,0)
            ""
            ""
            (0,0)
            false;;
  
  
  let describe_module_inclusion_item s (i,j)=
       let opt=Assistance_gparser_apply.apply Assistance_gparser_for_ocaml_language.prsr_for_module_inclusion s i in
       let res=Assistance_option.unpack opt in
       let (i1,j1)=List.nth(Assistance_gparser_result.important_ranges res) 2 
       and (i3,j3)=Assistance_gparser_result.whole_range res in 
         Assistance_ocaml_gsyntax_item.make
            Assistance_ocaml_gsyntax_category.Module_inclusion
            (Assistance_cull_string.interval s i1 j1)
            (i1,j1)
            (Assistance_cull_string.interval s i3 j3)
            ""
            (0,0)
            false;;
            
   let describe_item s ((i,j),idx)=
     if idx=Assistance_gparser_for_ocaml_language.index_for_value
     then Some(describe_value_item s (i,j))
     else
     if idx=Assistance_gparser_for_ocaml_language.index_for_type
     then Some(describe_type_item s (i,j))
     else
     if idx=Assistance_gparser_for_ocaml_language.index_for_exception
     then Some(describe_exception_item s (i,j))
     else
     if idx=Assistance_gparser_for_ocaml_language.index_for_module_opener
     then Some(describe_module_opener_item s (i,j))
     else
     if idx=Assistance_gparser_for_ocaml_language.index_for_module_closer
     then Some(describe_module_closer_item)
     else          
     if idx=Assistance_gparser_for_ocaml_language.index_for_module_inclusion
     then Some(describe_module_inclusion_item s (i,j))
     else None;;
     
  let uncatched_read2 s=
     Assistance_option.filter_and_unpack (describe_item s) (read1 s);;   
     
  
  
  let pre_read s= try uncatched_read2 s with Read1_exn(t)->raise(Pre_read_exn(t));;
  end;;

  let pre_read =Private.pre_read;;           

end;;






module Assistance_read_ocaml_files=struct

(*

#use"Ocaml_analysis/read_ocaml_files.ml";;

*)

module Private=struct
  

  
  let module_inclusion_in_pusher    
     (graet,current_full_scope,current_names) x=
      let included_module=x.Assistance_ocaml_gsyntax_item.name in
          let full_scope=current_full_scope^"."^included_module in
          let maybe_included_items=List.filter(
             fun y->let nm_y=y.Assistance_ocaml_gsyntax_item.name in
             (Assistance_supstring.begins_with nm_y full_scope)
             ||
             (Assistance_supstring.begins_with nm_y included_module)  
          ) graet in 
          (* local redifinition has priority over an outside definition *)
          let chosen_scope=(if
            List.exists(fun y->
              y.Assistance_ocaml_gsyntax_item.name=included_module
            ) maybe_included_items
            then included_module
            else full_scope
          ) in
           let included_items=List.filter(
             fun y->y.Assistance_ocaml_gsyntax_item.name=chosen_scope
           ) maybe_included_items in
           let renamed_included_items=Assistance_image.image 
           (Assistance_ocaml_gsyntax_item.include_in_new_scope full_scope )
           included_items in
           (List.rev_append renamed_included_items graet,current_full_scope,current_names);;
     
  let first_pusher_for_modulename_prepension_and_inclusion_expansion  
     walker_state x=
     let (graet,current_full_scope,current_names)=walker_state in
    match x.Assistance_ocaml_gsyntax_item.category with
      Assistance_ocaml_gsyntax_category.Value                                                                          
    | Assistance_ocaml_gsyntax_category.Type
    | Assistance_ocaml_gsyntax_category.Exception->
            let new_x=Assistance_ocaml_gsyntax_item.prepend_prefix current_full_scope x in
            (new_x::graet,current_full_scope,current_names)
    | Assistance_ocaml_gsyntax_category.Module_opener->
            let new_name=x.Assistance_ocaml_gsyntax_item.name in
            let new_names=current_names@[new_name] in
            let new_full_scope=String.concat "." new_names in
            (graet,new_full_scope,new_names)
    | Assistance_ocaml_gsyntax_category.Module_closer->
            let new_names=List.rev(List.tl(List.rev(current_names))) in
            let new_full_scope=String.concat "." new_names in
            (graet,new_full_scope,new_names)
    | Assistance_ocaml_gsyntax_category.Module_inclusion->
           module_inclusion_in_pusher (graet,current_full_scope,current_names) x;;
  
  exception Pusher23_exn;;
  
  let pusher_for_modulename_prepension_and_inclusion_expansion (walker_state,da_ober)=
     match da_ober with
     []->raise(Pusher23_exn)
     |x::peurrest->(first_pusher_for_modulename_prepension_and_inclusion_expansion 
     walker_state x,peurrest);;    
  
           
  let rec iterator_for_modulename_prepension_and_inclusion_expansion (walker_state,da_ober)=
     if da_ober=[] 
     then let  (graet,_,_)=walker_state in List.rev graet
     else iterator_for_modulename_prepension_and_inclusion_expansion(
       pusher_for_modulename_prepension_and_inclusion_expansion (walker_state,da_ober));; 
  
  
  let prepend_modulenames_and_expand_inclusions data_before (current_module,l)=
      iterator_for_modulename_prepension_and_inclusion_expansion 
        ((data_before,current_module,String.split_on_char '.' current_module),l);;
  
  end;;
  
  exception Reading_error of Assistance_absolute_path.t * string;;
  
  let read_ocaml_files l_ap=
     let temp1=Assistance_image.image( fun ap->
     let s_ap=Assistance_absolute_path.to_string ap
     and text=Assistance_io.read_whole_file ap in
     let unpointed=Assistance_cull_string.before_rightmost s_ap '.' in
     let module_name=String.capitalize_ascii (Assistance_cull_string.after_rightmost unpointed '/') in
     try (module_name,Assistance_pre_read_ocaml_files.pre_read text)  with
     Assistance_pre_read_ocaml_files.Pre_read_exn(t)->raise(Reading_error(ap,t)) 
     ) l_ap in 
     List.fold_left Private.prepend_modulenames_and_expand_inclusions [] temp1;;
     
     
  (*
  
  let g1=German_wrapper.data();;
  let g2=List.filter Modulesystem_data.ml_present g1;;
  let g3=List.flatten (image Modulesystem_data.acolytes g2);;
  let g4=List.filter (fun mlx->snd(Mlx_filename.decompose mlx)=Ocaml_ending.ml) g3;;
  let g5=image Mlx_filename.to_absolute_path g4;;
  
  let g6=read3 g5;;
  
  
  let g6=image (fun ap->let s=Io.read_whole_file ap in
    (-(String.length s),(ap,s))
  ) g5 ;;
  let g7=image snd (ofo(Tidel2.diforchan g6));;
  let g8=Explicit.image (fun (ap,s)->(ap,read2 s)) g7;;
  let g9=Explicit.image (fun (ap,l)->
    from_level2_to_level3 ([],"Moody") l
  ) g8;;
  
  *)
  
    
  (*  
  
  let s1="let jiving=234  ;;";;
  describe_value_item s1 (1,String.length s1);;
  
  let s2="type ('a,'b) sister=('a list)*'b*string;;";;
  describe_type_item s2 (1,String.length s2);;
  
  let s3="type sister=(int list)*float*string;;";;
  describe_type_item s3 (1,String.length s3);;
  
  let s4="exception Foobar of string*int;;";;
  describe_exception_item s4 (1,String.length s4);;
  
  let s5="exception Foobar;;";;
  describe_exception_item s5 (1,String.length s5);;
  
  let s6="module  Foobar=struct";;
  describe_module_opener_item s6 (1,String.length s6);;
  
  let s7="end\n;;";;
  describe_module_opener_item s7 (1,String.length s7);;
  
  let s8="include Leap\n;;";;
  describe_module_inclusion_item s8 (1,String.length s8);;
     
  *)   
     
       
                

end;;






module Assistance_rename_moduled_value_in_file=struct

(*

#use"Ocaml_analysis/rename_moduled_value_in_file.ml";;

*)

exception No_module_given of string;;
exception No_value_with_name of string;;

let rename_moduled_value_in_file preceding_files old_name new_name path=
   let j=Assistance_substring.leftmost_index_of_in "." old_name in
   if j<0 
   then raise(No_module_given(old_name))
   else 
   let module_name=Assistance_cull_string.beginning (j-1) old_name in
   let temp3=Assistance_read_ocaml_files.read_ocaml_files preceding_files in
   let opt_temp4=Assistance_option.seek (fun itm->
     (itm.Assistance_ocaml_gsyntax_item.name)=old_name
   ) temp3 in
   if opt_temp4=None
   then raise(No_value_with_name(old_name))
   else
   let temp4=Assistance_option.unpack(opt_temp4) in
   let (i1,j1)=temp4.Assistance_ocaml_gsyntax_item.interval_for_name in
   let _=Assistance_overwrite_at_intervals.inside_file [(i1,j1),new_name] path in
   let temp3_again=Assistance_read_ocaml_files.read_ocaml_files preceding_files in
   let beheaded_name=Assistance_cull_string.cobeginning j old_name in
   let s_new_beheaded_name=(fun (fa,nn)->if fa="" then nn else fa^"."^nn)
   (Assistance_cull_string.before_rightmost beheaded_name '.',Assistance_overwriter.to_string new_name) in
   let new_beheaded_name=Assistance_overwriter.of_string s_new_beheaded_name in
   let s_new_full_name=module_name^"."^s_new_beheaded_name in
   let temp4_again=Assistance_listennou.force_find (fun itm->
     (itm.Assistance_ocaml_gsyntax_item.name)=s_new_full_name
   ) temp3_again in
   let k1=Assistance_listennou.find_index temp4_again temp3_again in
   let temp5=Assistance_listennou.big_tail k1 temp3_again in
   let temp6=Assistance_option.filter_and_unpack(
      fun itm->
        let txt=itm.Assistance_ocaml_gsyntax_item.content in
        let ttemp7=Assistance_isolated_occurrences.of_in 
           beheaded_name txt in
        if ttemp7<>[]
        then  let isoc=Assistance_isolated_occurrences.of_in beheaded_name txt in
              let replacings=Assistance_image.image (fun p->(p,new_beheaded_name)) isoc in
              let new_txt=Assistance_overwrite_at_intervals.inside_string
                   replacings txt in
             Some(itm.Assistance_ocaml_gsyntax_item.interval_for_content,
                  Assistance_overwriter.of_string new_txt)
        else None   
   ) temp5 in
   Assistance_overwrite_at_intervals.inside_file temp6 path;;


end;;






module Assistance_find_suitable_ending=struct

(*

#use"find_suitable_ending.ml";;

*)

(*

Note that the order in Ocaml_ending.correspondances is important

*)

exception No_suitable_location of Assistance_dfa_root_t.t*(Assistance_dfa_subdirectory_t.t list)*string;;

let find_file_location dir l_subdir old_x=
  let x=String.uncapitalize_ascii old_x in
  let s_dir=Assistance_dfa_root.connectable_to_subpath(dir) in
  let original_endings=Assistance_image.image Assistance_dfa_ending.connectable_to_modulename Assistance_dfa_ending.all_ocaml_endings in
  let endings=(
     if List.exists (fun edg->Assistance_supstring.ends_with x edg) original_endings
     then [""]
     else original_endings
  ) in
  let temp1=Assistance_cartesian.product(l_subdir) endings in
  let tempf=(fun (sd,edg)->
  	let s1=s_dir^(Assistance_dfa_subdirectory.connectable_to_subpath sd)^x^edg in
  	if Sys.file_exists s1
  	then Some(Assistance_absolute_path.of_string s1)
  	else None
  ) in
  let opt=Assistance_option.find_and_stop tempf temp1 in
  if opt=None
  then raise(No_suitable_location(dir ,l_subdir,x))
  else  Assistance_option.unpack(opt);;           

end;;






module Assistance_fw_module_small_details_t=struct

(*

#use"Filewatching/fw_module_small_details_t.ml";;


*)

type t ={
  used_modules : Assistance_dfa_module_t.t list ;
  used_libraries : Assistance_ocaml_library_t.t list ;
  has_printer : bool ;
  subdirectory : Assistance_dfa_subdirectory_t.t ;
  principal_ending : Assistance_dfa_ocaml_ending_t.t ;
  mli_present : bool ;
  principal_modification_time : string ;
  mli_modification_time : string option ;
};;



end;;






module Assistance_fw_configuration=struct

(*

#use"Filewatching/fw_configuration.ml";;

*)


let of_root root_dir = 
    Assistance_fw_poly.construct_fw_configuration 
      ~root:root_dir
      ~ignored_subdirectories:Assistance_coma_constant.git_ignored_subdirectories
      ~ignored_files:[]
    ;; 



let test_for_admissibility data rl=
  (List.mem (
    (Assistance_dfn_rootless.to_ending rl)
  ) Assistance_dfa_ending.endings_for_readable_files)
  &&
   (List.for_all (
     fun sd->not(Assistance_dfn_rootless.is_in rl sd)
  ) (Assistance_fw_poly.ignored_subdirectories data)
  )  
  &&
  (
    not(List.mem rl (Assistance_fw_poly.ignored_files data))
  )
  ;;



end;;






module Assistance_file_watcher=struct

(*

#use"Filewatching/file_watcher.ml";;

*)

exception Register_rootless_path_exn of string list;;
exception Already_registered_rootless_paths_exn of string list;;
exception Change_has_occurred ;;


module Private = struct


(* Start of level 4 *)

  

(* End of level 4 *)

(* Start of level 3 *)

let message_about_missing_files missing_files=
   let temp1=Assistance_image.image Assistance_dfn_rootless.to_line missing_files in
   "\n\n"^
   "The following files have been deleted without warning :\n"^
   (String.concat "\n" temp1)^
   "\n\n"
 ;;    

let mtime file = string_of_float((Unix.stat file).Unix.st_mtime) ;;

let recompute_mtime fw path =
      let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_poly.root fw) 
      and s_path=Assistance_dfn_rootless.to_line path in 
      let file = s_root^s_path in 
      mtime file;;
 
let recompute_mtime_opt fw path =
     try Some(recompute_mtime fw path) with _ -> None ;;
 


(* End of level 3 *)


(* Start of level 2 *)


let announce_missing_files missing_files=
     if missing_files=[]
     then ()
     else (print_string(message_about_missing_files missing_files);flush stdout);;
            

let helper1_during_inspection fw accu (rl,old_mtime,new_mtime)=
   let _ = (if new_mtime <> old_mtime then accu:=rl::(!accu)) in 
   (rl,new_mtime);;

let helper2_during_inspection fw accu l_pairs =
   let temp1 = Assistance_image.image (fun (rl,old_mtime)->
      (rl,old_mtime,recompute_mtime_opt fw rl)
      ) l_pairs in 
   let (good_temp1,bad_temp1) =  List.partition (
       fun (_,_,opt) -> opt <> None 
   ) temp1 in
   let good_pairs = Assistance_image.image (fun 
     (rl,old_mtime,opt) -> (rl,old_mtime,Assistance_option.unpack opt)
   ) good_temp1 
   and missing_files = Assistance_image.image (fun (rl,_,_)->rl) bad_temp1 in
   let _ = announce_missing_files missing_files in 
   let new_l_pairs = Assistance_image.image (helper1_during_inspection fw accu) good_pairs in 
   (new_l_pairs,List.rev(!accu));;

  let recompute_all_info fw path =
    let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_poly.root fw) 
    and s_path=Assistance_dfn_rootless.to_line path in 
    let file = s_root^s_path in 
    (path,mtime file);;


(* End of level 2 *)


(* Start of level 1 *)

let compute_changes_and_announce_them fw ~verbose=
   let ref_for_files=ref[]  in 
   let (new_files,changed_files)=
       helper2_during_inspection fw ref_for_files (Assistance_fw_poly.watched_files fw) in 
   let _ = (
     if verbose 
     then Assistance_strung.announce 
            ~trailer:"The following files have been changed :"
               ~printer:Assistance_dfn_rootless.to_line ~items:changed_files 
               ~separator: ", "
   ) in
   (new_files,changed_files);;   

(* let configuration fw = fw.File_watcher_t.configuration ;; *)

let get_content fw rootless = 
  let root = Assistance_fw_poly.root fw in 
  let s_ap = Assistance_dfn_common.recompose_potential_absolute_path root rootless in 
  Assistance_io.read_whole_file(Assistance_absolute_path.of_string s_ap);;     
    

let of_configuration_and_list config to_be_watched =
  let the_root = Assistance_fw_poly.root config in  
  let compute_info=( fun path->
    let s_root = Assistance_dfa_root.connectable_to_subpath the_root
    and s_path=Assistance_dfn_rootless.to_line path in 
    let file = s_root^s_path in 
    let mtime = string_of_float((Unix.stat file).Unix.st_mtime) in 
    (path,mtime)
 ) in 
 Assistance_fw_poly.extend_fw_configuration_to_file_watcher config 
 ~watched_files:(Assistance_image.image compute_info to_be_watched) ;;
 

   let ref_for_subdirectory_renaming = ref [];;

   let remember_during_subdirectory_renaming pair =
      (ref_for_subdirectory_renaming := pair :: (!ref_for_subdirectory_renaming) );;
   
   let rename_subdirectory_on_pair fw (old_subdir,new_subdir) pair=
      let (rootless_path,_)=pair in 
      match Assistance_dfn_rootless.soak (old_subdir,new_subdir) rootless_path with 
      Some(new_rootless_path) -> 
           let _=(
              remember_during_subdirectory_renaming (rootless_path,new_rootless_path)
           ) in 
           recompute_all_info fw new_rootless_path
      |None -> pair;;
   
   let rename_subdirectory_on_pairs fw (old_subdir,new_subdir) l_pairs =
        let _=(ref_for_subdirectory_renaming := []) in 
        let comp=Assistance_image.image (rename_subdirectory_on_pair fw (old_subdir,new_subdir)) l_pairs in 
        (comp,List.rev(!ref_for_subdirectory_renaming));;
   
   

let update_in_list_of_pairs fw  to_be_updated pairs  =
Assistance_image.image (
   fun pair -> 
     let (rootless,mtime)=pair in 
     if List.mem rootless to_be_updated 
     then recompute_all_info fw rootless 
     else pair
) pairs;;

let update_some_files fw w_files = 
   let new_watched_files = update_in_list_of_pairs fw w_files 
   (Assistance_fw_poly.watched_files fw) in 
   Assistance_fw_poly.set_watched_files fw new_watched_files ;;

   

(* End of level 1 *)



let adhoc_membership path selected_files_opt=
   match selected_files_opt with 
   None -> true 
   |Some selected_files -> List.mem path selected_files ;;

let apply_text_transformation_on_pair fw tr changed_files_ref selected_files_opt pair=
   let (path,_) = pair in 
   if not(adhoc_membership path selected_files_opt)
   then pair 
   else    
   let old_content = get_content fw path in 
   let new_content = tr old_content in
   if new_content = old_content   
   then pair 
   else 
   let _=(changed_files_ref:= path:: (!changed_files_ref)) in 
   let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_poly.root fw) 
   and s_path=Assistance_dfn_rootless.to_line path in 
   let file = s_root^s_path in  
   let ap=Assistance_absolute_path.of_string file in 
   let _=(Assistance_io.overwrite_with ap new_content) in 
   recompute_all_info fw path ;;



let apply_text_transformation_on_some_files fw tr l=
   let changed_files_ref=ref[]  in 
   let new_files = Assistance_image.image (
      apply_text_transformation_on_pair fw tr changed_files_ref (Some l)
   )  (Assistance_fw_poly.watched_files fw)  in 
   let fw2 = Assistance_fw_poly.set_watched_files fw new_files in 
  (fw2,!changed_files_ref);;  



   
let check_that_no_change_has_occurred fw =
  let (new_files,changed_files)= compute_changes_and_announce_them fw ~verbose:true in
  if changed_files <> []
  then raise(Change_has_occurred)
  else () ;;       

  let deal_with_initial_comment_if_needed fw rless =
    if (Assistance_dfn_rootless.to_ending rless)<> Assistance_dfa_ending.ml 
    then ()
    else
       let root = Assistance_fw_poly.root fw in 
       let full = Assistance_dfn_join.root_to_rootless root rless in 
       let ap = Assistance_dfn_full.to_absolute_path full in 
       Assistance_put_use_directive_in_initial_comment.put_usual root ap
    ;;    
         

let first_init config =
   let the_root = Assistance_fw_poly.root config in 
   let the_dir =  Assistance_directory_name.of_string (Assistance_dfa_root.without_trailing_slash the_root) in 
   let (list1,_) = Assistance_more_unix.complete_ls_with_ignored_subdirs the_dir (Assistance_fw_poly.ignored_subdirectories config) false in 
   let list2 = Assistance_option.filter_and_unpack(
            fun ap-> try Some(Assistance_dfn_common.decompose_absolute_path_using_root ap the_root) with 
                     _->None 
   ) list1 in
   List.filter (Assistance_fw_configuration.test_for_admissibility config) list2 ;;
      
let inspect_and_update fw ~verbose = 
   let (new_files,changed_files)= compute_changes_and_announce_them fw ~verbose in 
   let fw2 = Assistance_fw_poly.set_watched_files fw new_files in
   (fw2,changed_files);;         
        
let latest_changes fw ~verbose =
   let (_,changed_files) = compute_changes_and_announce_them fw ~verbose  in 
   changed_files ;;
         
let of_configuration config = 
   let to_be_watched = first_init config in 
   of_configuration_and_list config to_be_watched ;;
        
         
let overwrite_file_if_it_exists fw rootless new_content =
   let root = Assistance_fw_poly.root fw in 
   if List.exists ( fun (r,_)->r=rootless ) (Assistance_fw_poly.watched_files fw)
   then let ap = Assistance_absolute_path.of_string (Assistance_dfn_common.recompose_potential_absolute_path root rootless) in 
         let _=Assistance_io.overwrite_with ap new_content in 
         let new_watched_files = update_in_list_of_pairs fw [rootless] (Assistance_fw_poly.watched_files fw) in 
         (Assistance_fw_poly.set_watched_files fw new_watched_files,true)
   else (fw,false);;


let register_rootless_paths fw rootless_paths= 
   let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_poly.root fw) in
   let nonexistent_paths = Assistance_option.filter_and_unpack (
      fun rp-> let s_full_path = s_root^(Assistance_dfn_rootless.to_line rp)  in 
      if not(Sys.file_exists s_full_path)
      then Some(s_full_path)
      else None
   ) rootless_paths in 
   if nonexistent_paths<>[]
   then raise(Register_rootless_path_exn(nonexistent_paths))
   else 
   let old_watched_files = Assistance_fw_poly.watched_files fw in    
   let redundant_paths = List.filter (
      fun rp-> List.exists (fun (rl,_)->rl = rp) old_watched_files
   ) rootless_paths in 
   if redundant_paths<>[]
   then raise(Already_registered_rootless_paths_exn
       (Assistance_image.image Assistance_dfn_rootless.to_line redundant_paths))
   else    
   Assistance_fw_poly.set_watched_files fw (
      old_watched_files@
       (Assistance_image.image (recompute_all_info fw) rootless_paths)
   ) ;;



let remove_files fw rootless_paths=
 let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_poly.root fw) in 
 let removals_to_be_made = Assistance_image.image (
   fun path->" rm -f "^s_root^(Assistance_dfn_rootless.to_line path) 
 ) rootless_paths in 
 let _=Assistance_unix_command.conditional_multiple_uc removals_to_be_made in 
 Assistance_fw_poly.set_watched_files fw (
   List.filter (fun (path,_)->
      not(List.mem path rootless_paths)
   ) (Assistance_fw_poly.watched_files fw)
 ) ;;

let rename_files fw renaming_schemes =
    let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_poly.root fw)  in 
    let displacements_to_be_made = Assistance_image.image (
      fun (path1,path2)->" mv "^s_root^(Assistance_dfn_rootless.to_line path1)^" "^
      s_root^(Assistance_dfn_rootless.to_line path2)
    ) renaming_schemes in 
    let _=Assistance_unix_command.conditional_multiple_uc displacements_to_be_made in 
    let new_watched_files = Assistance_image.image (fun pair->
      let (path,_)=pair in 
      (match List.assoc_opt path renaming_schemes with
      Some(new_path) -> 
           let _ = (
             if  (Assistance_dfn_rootless.to_ending new_path) = Assistance_dfa_ending.ml
             then deal_with_initial_comment_if_needed fw new_path
           ) in 
           (new_path,recompute_mtime fw new_path)
      | None -> pair)
   ) (Assistance_fw_poly.watched_files fw)   in 
   Assistance_fw_poly.set_watched_files fw  new_watched_files ;;

  let rename_subdirectory_as fw (old_subdir,new_subdir)=
  let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_poly.root fw)  in 
  let s_old_subdir = Assistance_dfa_subdirectory.without_trailing_slash old_subdir 
  and s_new_subdir = Assistance_dfa_subdirectory.without_trailing_slash new_subdir in 
  let old_full_path = s_root^s_old_subdir 
  and new_full_path = s_root^s_new_subdir in 
  let cmd=" mv "^old_full_path^" "^new_full_path in 
      let _=Assistance_unix_command.hardcore_uc cmd in 
  let (files,reps)   =  rename_subdirectory_on_pairs fw (old_subdir,new_subdir) 
     (Assistance_fw_poly.watched_files fw) in 
  (Assistance_fw_poly.set_watched_files fw files,reps);;   
          
     
let plunge_fw_configuration config= 
   Assistance_fw_poly.extend_fw_configuration_to_file_watcher config 
   ~watched_files:[] ;;
 
end;;


let apply_text_transformation_on_some_files = Private.apply_text_transformation_on_some_files;;
let check_that_no_change_has_occurred = Private.check_that_no_change_has_occurred ;;
let inspect_and_update = Private.inspect_and_update;; 
let latest_changes = Private.latest_changes ;;
let of_configuration = Private.of_configuration ;;
let of_configuration_and_list = Private.of_configuration_and_list ;;
let overwrite_file_if_it_exists = Private.overwrite_file_if_it_exists ;;
let plunge_fw_configuration = Private.plunge_fw_configuration ;;
let register_rootless_paths = Private.register_rootless_paths;;
let remove_files = Private.remove_files;;
let rename_files = Private.rename_files;;
let rename_subdirectory_as = Private.rename_subdirectory_as;;
let update_some_files = Private.update_some_files ;; 


end;;






module Assistance_fw_with_archives=struct

(*

#use"Filewatching/fw_with_archives.ml";;

*)

module Private = struct

   let parent fw = Assistance_fw_poly.parent fw ;;
   
   (* Inherited methods *)

   (* let configuration fw = File_watcher.configuration (parent fw) ;;*)

   let root fw = Assistance_fw_poly.root fw ;;
   let update_parent fw new_parent = Assistance_fw_poly.set_parent fw new_parent ;;
   let watched_files fw = Assistance_fw_poly.watched_files fw ;;
   
   (* End of inherited methods *)  
   (* Inherited constructors *)

   let constructor par opt_subdirs= 
      let subdirs = (match opt_subdirs with (Some l)->l |None -> [Assistance_coma_constant.watched_and_githubbed_subdir]) in    
      Assistance_fw_poly.extend_file_watcher_to_fw_with_archives
        par ~subdirs_for_archived_mlx_files:subdirs ;;
      
   let plunge_fw_configuration config = constructor(Assistance_file_watcher.plunge_fw_configuration config) None ;;
   let of_configuration config = constructor(Assistance_file_watcher.of_configuration config) None ;;
   let of_configuration_and_list config l = constructor (Assistance_file_watcher.of_configuration_and_list config l) None;;
   let overwrite_file_if_it_exists fw rl new_content = 
      let old_parent = parent fw in 
      let (new_parent,file_exists) = Assistance_file_watcher.overwrite_file_if_it_exists old_parent rl new_content in 
      (update_parent fw new_parent,file_exists);;
   let register_rootless_paths fw rls = 
      let old_parent = parent fw in 
      let new_parent = Assistance_file_watcher.register_rootless_paths old_parent rls in 
      update_parent fw new_parent ;;   
   let remove_files fw rls = 
      let old_parent = parent fw in 
      let new_parent = Assistance_file_watcher.remove_files old_parent rls in 
      update_parent fw new_parent ;;               
   let rename_subdirectory_as fw sd_pair = 
      let old_parent = parent fw in 
      let (new_parent,extra) = Assistance_file_watcher.rename_subdirectory_as old_parent sd_pair in 
      (update_parent fw new_parent,extra) ;;  
     

   (* End of inherited constructors *)  
      
   let canonical_tripartition fw all_files =
      let (c_files,nc_files) = List.partition (
                fun rl->
                  Assistance_dfa_ending.is_compilable (Assistance_dfn_rootless.to_ending rl)
      )  all_files in 
      let archived_subdirs = Assistance_fw_poly.subdirs_for_archived_mlx_files fw in 
      let is_archived = (fun rl->List.exists (Assistance_dfn_rootless.is_in rl) archived_subdirs) in 
      let (a_files,u_files) = List.partition is_archived  c_files in 
      (a_files,u_files,nc_files) ;;     
      
   let full_tripartition fw =
      let all_files = Assistance_image.image fst (watched_files fw) in 
      canonical_tripartition fw all_files ;;

   let compilable_files fw =
      Assistance_option.filter_and_unpack (
         fun (rl,_)->
            if Assistance_dfa_ending.is_compilable (Assistance_dfn_rootless.to_ending rl)
            then Some rl 
            else None   
      )  (watched_files fw) ;;
      
   let compute_small_details_on_one_file fw rl=
      let root = Assistance_fw_poly.root fw in 
      let s_ap = Assistance_dfn_common.recompose_potential_absolute_path root rl in 
      let ap = Assistance_absolute_path.of_string s_ap in 
      Assistance_fw_file_small_details.compute ap ;;

   let compute_all_small_details fw =
      let c_files = compilable_files fw in 
      Assistance_image.image (
            fun rl ->
               (rl,compute_small_details_on_one_file fw rl)
      ) c_files ;;
           

   let forget_modules fw mod_names =
      let all_files = Assistance_image.image fst (watched_files fw) in 
      let (_,u_files,_) = canonical_tripartition fw all_files in 
      let the_files = List.filter (
                 fun path-> List.mem (Assistance_dfn_rootless.to_module path) mod_names 
         ) u_files in  
      let old_parent = parent fw in    
      let new_parent = Assistance_file_watcher.remove_files old_parent the_files in 
      (update_parent fw new_parent,the_files) ;;      
   
   
   let announce_changes fw changed_files=    
         let (a_files,u_files,nc_files) = 
            canonical_tripartition fw changed_files in 
         let announce = (fun trail files ->
            Assistance_strung.announce 
               ~trailer: trail
                  ~printer:Assistance_dfn_rootless.to_line ~items:files 
                  ~separator: "\n"
         ) in
         let _ = (
            announce "The following noncompilables have been changed :" nc_files;
            announce "The following archived files have been changed :" a_files;
            announce "The following usual compilables have been changed :" u_files;
         ) in
         (a_files,u_files,nc_files) ;;   


   let inspect_and_update old_fw = 
      let old_parent = parent old_fw in    
      let (new_parent,changed_files) = 
          Assistance_file_watcher.inspect_and_update old_parent
           ~verbose:false in 
      let new_fw = update_parent old_fw new_parent in     
      let (a_files,u_files,nc_files) = announce_changes new_fw changed_files in 
      (new_fw,changed_files,(a_files,u_files)) ;;   

   let latest_changes fw = 
      let changed_files = Assistance_file_watcher.latest_changes (parent fw) ~verbose:false in 
      announce_changes fw changed_files ;;

   let noncompilable_files fw  =
      let all_files = Assistance_image.image fst (watched_files fw) in 
      let (_,_,nc_files) = canonical_tripartition fw all_files in 
      nc_files ;;
      
   let relocate_module_to fw mod_name new_subdir=
      let all_files = Assistance_image.image fst (watched_files fw) in 
      let (_,u_files,_) = canonical_tripartition fw all_files in 
      let the_files = List.filter (
                  fun path-> (Assistance_dfn_rootless.to_module path)=mod_name 
      ) u_files in 
      let old_parent = parent fw in    
      let replacements = Assistance_image.image (fun path->
         (path,Assistance_dfn_rootless.relocate_to path new_subdir)
       ) the_files in 
      let new_parent = Assistance_file_watcher.rename_files old_parent replacements in 
      (update_parent fw new_parent,replacements)  ;;
         
   let rename_module_on_filename_level old_fw (old_module,new_module) = 
      let all_files = Assistance_image.image fst (watched_files old_fw) in 
      let (a_files,u_files,nc_files) = canonical_tripartition old_fw all_files in 
      let acolytes = List.filter (
                   fun rl -> (Assistance_dfn_rootless.to_module rl) = old_module 
      ) u_files in
      let replacements = Assistance_image.image (fun old_rl->
                   (old_rl,Assistance_dfn_rootless.rename_module_as (old_module,new_module) old_rl )) acolytes in
      let old_parent = parent old_fw in    
      let new_parent = Assistance_file_watcher.rename_files old_parent replacements in                    
      let new_fw = update_parent old_fw new_parent in 
      (new_fw,replacements) ;;     
               
   let rename_module_on_content_level old_fw (old_module,new_module) files_to_be_rewritten =
      let apply = (fun par files->
         Assistance_file_watcher.apply_text_transformation_on_some_files par 
         (Assistance_look_for_module_names.change_module_name_in_ml_ocamlcode  
         old_module new_module) files
      ) in 
      let (all_a_files,_,_) = full_tripartition old_fw  in 
      let old_parent = parent old_fw in    
      let (par1,changed_u_files) = apply old_parent files_to_be_rewritten in 
      let (par2,changed_a_files) = apply par1 all_a_files in 
      let announce = (fun trail files ->
               Assistance_strung.announce 
                        ~trailer: trail
                           ~printer:Assistance_dfn_rootless.to_line ~items:files 
                           ~separator: "\n"
      ) in
      let _ = (
               announce "The following archived files have their content affected by the renaming :" changed_a_files;
               announce "The following usual compilables have their content affected by the renaming :" changed_u_files;
      ) in   
      (update_parent old_fw par2,changed_u_files,changed_a_files) ;;  
                  
   let rename_module_on_filename_level_and_in_files fw (old_module,new_module,files_to_be_rewritten)=
      let (fw2,file_renamings) = rename_module_on_filename_level fw (old_module,new_module) in 
      let (fw3,changed_u_files,changed_a_files) = rename_module_on_content_level fw2 (old_module,new_module) files_to_be_rewritten in 
      (fw3,file_renamings,changed_u_files,changed_a_files) ;;   
   
      
   let replace_string old_fw (replacee,replacer) = 
      let apply = (fun par files->
         Assistance_file_watcher.apply_text_transformation_on_some_files par 
         (Assistance_replace_inside.silently_replace_inside_string (replacee,replacer)) files
      ) in 
      let (all_a_files,all_u_files,_) = full_tripartition old_fw  in 
      let old_parent = parent old_fw in    
      let (par1,changed_u_files) = apply old_parent all_u_files in 
      let (par2,changed_a_files) = apply par1 all_a_files in 
      let announce = (fun trail files ->
         Assistance_strung.announce 
               ~trailer: trail
                  ~printer:Assistance_dfn_rootless.to_line ~items:files 
                  ~separator: "\n"
      ) in
      let _ = (
         announce "The following archived files have their content affected by the renaming :" changed_a_files;
         announce "The following usual compilables have their content affected by the renaming :" changed_u_files;
      ) in 
      (update_parent old_fw par2,(changed_a_files,changed_u_files)) ;;

   let replace_value old_fw (preceding_files,path) (replacee,pre_replacer) =
      let replacer=(Assistance_cull_string.before_rightmost replacee '.')^
          "."^pre_replacer in 
      let _=Assistance_rename_moduled_value_in_file.rename_moduled_value_in_file 
         preceding_files replacee (Assistance_overwriter.of_string pre_replacer) 
           path in 
      let old_parent = parent old_fw in   
      let rootless = Assistance_dfn_common.decompose_absolute_path_using_root path 
        (Assistance_fw_poly.root old_parent)  in 
      let par2= Assistance_file_watcher.update_some_files old_parent [rootless] in 
      let fw2 = update_parent old_fw par2 in 
      let (fw3,(changed_a_files,changed_u_files))=replace_string fw2 (replacee,replacer) in 
      let all_changes = changed_a_files@rootless::changed_u_files in       
      (fw3,(all_changes,rootless::changed_u_files));;

   let usual_compilable_files fw  =
      let all_files = Assistance_image.image fst (watched_files fw) in 
      let (_,u_files,_) = canonical_tripartition fw all_files in 
      u_files ;;        

     
   let check_that_no_change_has_occurred fw =
      Assistance_file_watcher.check_that_no_change_has_occurred (parent fw) ;; 
      
end ;;

let check_that_no_change_has_occurred = Private.check_that_no_change_has_occurred;;
let compute_all_small_details = Private.compute_all_small_details ;;
let compute_small_details_on_one_file = Private.compute_small_details_on_one_file ;;
let forget_modules = Private.forget_modules ;;
let inspect_and_update = Private.inspect_and_update ;;
let latest_changes = Private.latest_changes ;;
let noncompilable_files = Private.noncompilable_files ;;
let of_configuration = Private.of_configuration ;;
let of_configuration_and_list = Private.of_configuration_and_list ;;
let overwrite_file_if_it_exists = Private.overwrite_file_if_it_exists ;;
let partition_for_singles = Private.canonical_tripartition ;; 
let plunge_fw_configuration = Private.plunge_fw_configuration ;;
let register_rootless_paths = Private.register_rootless_paths ;; 
let relocate_module_to = Private.relocate_module_to ;;
let remove_files = Private.remove_files ;;  
let rename_module_on_filename_level_and_in_files = Private.rename_module_on_filename_level_and_in_files ;;
let rename_subdirectory_as = Private.rename_subdirectory_as ;;
let replace_string = Private.replace_string;;
let replace_value = Private.replace_value;;
let usual_compilable_files = Private.usual_compilable_files ;;




end;;






module Assistance_fw_with_small_details=struct

(*

#use"Filewatching/fw_with_small_details.ml";;

*)


module Private = struct

   (* Start of level 2 *)
   let parent fw = Assistance_fw_poly.parent fw ;;
   (* End of level 2 *)
 
   (* Start of level 1 *)
 
   
   let constructor mother =
     Assistance_fw_poly.extend_fw_with_archives_to_fw_with_small_details 
       mother  
       ~small_details_in_files:(Assistance_fw_with_archives.compute_all_small_details mother) ;;
   let root fw     = Assistance_fw_poly.root fw ;;  
   let small_details_in_files fw = Assistance_fw_poly.small_details_in_files fw ;;  
   
   (* End of level 1 *)
   

 
   let forget_modules fw mod_names =
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,removed_files) = Assistance_fw_with_archives.forget_modules old_parent mod_names in
      (
         Assistance_fw_poly.extend_fw_with_archives_to_fw_with_small_details 
         new_parent 
         ~small_details_in_files:(List.filter (
            fun (rl,_)->not(List.mem (Assistance_dfn_rootless.to_module rl) mod_names)
          ) old_details),removed_files) ;;  
   
   let inspect_and_update fw  =
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,changed_files,(a_files,u_files)) = Assistance_fw_with_archives.inspect_and_update old_parent in
      let changed_details_ref = ref [] in 
      let new_small_details = Assistance_image.image (
         fun old_pair->
          let rl = fst old_pair in
          if List.mem rl changed_files 
          then let new_pair = (rl,Assistance_fw_with_archives.compute_small_details_on_one_file new_parent rl) in 
               let _ = (changed_details_ref:=new_pair::(!changed_details_ref) ) in 
               new_pair 
          else old_pair  
       ) old_details in 
      (Assistance_fw_poly.extend_fw_with_archives_to_fw_with_small_details 
      new_parent 
      ~small_details_in_files:new_small_details,
      ((a_files,u_files),!changed_details_ref,changed_files));;
   
   let of_configuration config =   
       let mother = Assistance_fw_with_archives.of_configuration config in 
       constructor mother ;;
   
   let of_configuration_and_list (config,files)=   
      let mother = Assistance_fw_with_archives.of_configuration_and_list config files  in 
      constructor mother ;;
   
   let overwrite_file_if_it_exists fw (rootless,new_content) = 
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,change_made) = 
         Assistance_fw_with_archives.overwrite_file_if_it_exists 
           old_parent rootless new_content in 
      let accu = ref None in      
      let new_fw = (
      if change_made 
      then 
         let new_small_details = Assistance_image.image (
            fun old_pair->
            let rl = fst old_pair in
            if rl  = rootless 
            then let new_pair = (rl,Assistance_fw_with_archives.compute_small_details_on_one_file new_parent rl) in 
                 let _= (accu:=Some(rl,Some(new_pair))) in 
                 new_pair
            else old_pair  
         ) old_details in 
         Assistance_fw_poly.extend_fw_with_archives_to_fw_with_small_details 
         new_parent 
          ~small_details_in_files:new_small_details
      else fw ) in (new_fw,!accu);;           
   
   
   let register_rootless_paths fw rootless_paths= 
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let new_parent = Assistance_fw_with_archives.register_rootless_paths 
           old_parent rootless_paths in 
      let new_details =    (Assistance_image.image (fun rl->
         (rl,Assistance_fw_with_archives.compute_small_details_on_one_file new_parent rl)) 
         rootless_paths) in 
      ( Assistance_fw_poly.extend_fw_with_archives_to_fw_with_small_details 
      new_parent 
      ~small_details_in_files:(old_details @ new_details),
      (Assistance_fw_with_archives.partition_for_singles new_parent rootless_paths,new_details) ) ;;    
   
   let relocate_module_to fw (mod_name,new_subdir)=
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,replacements) = Assistance_fw_with_archives.relocate_module_to 
           old_parent mod_name new_subdir in 
      let accu = ref [] in      
      let new_small_details = Assistance_image.image (
         fun old_pair->
         let rl = fst old_pair in
         if (Assistance_dfn_rootless.to_module rl) = mod_name 
         then let new_rl = Assistance_dfn_rootless.relocate_to rl new_subdir in 
              let new_pair = (new_rl,snd old_pair) in 
              let _ = (accu := (rl,Some new_pair) :: (!accu)) in
              new_pair
         else old_pair        
      ) old_details in 
      let new_fw = Assistance_fw_poly.extend_fw_with_archives_to_fw_with_small_details 
      new_parent 
      ~small_details_in_files:new_small_details  in 
      (new_fw,(!accu,replacements));;  
   
   let remove_files fw removed_rootless_paths=   
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let new_parent = Assistance_fw_with_archives.remove_files 
         old_parent removed_rootless_paths in 
      ( Assistance_fw_poly.extend_fw_with_archives_to_fw_with_small_details 
      new_parent 
      ~small_details_in_files: (List.filter (
            fun (rl,_)->not(List.mem rl removed_rootless_paths)
         ) old_details),
         Assistance_image.image (fun rl->(rl,None)) removed_rootless_paths) ;;
   
   
      let rename_module_on_filename_level_and_in_files fw (old_module,new_module,files_to_be_rewritten) =
         let old_parent = parent fw 
         and old_details = small_details_in_files fw  in 
         let (new_parent,file_renamings,changed_u_files,changed_a_files) = 
             Assistance_fw_with_archives.rename_module_on_filename_level_and_in_files 
           old_parent (old_module,new_module,files_to_be_rewritten) in 
         let optional_new_rl = (fun rl->
            match List.assoc_opt rl file_renamings with 
               Some(new_rl) -> Some(new_rl) 
               | None ->
                  if List.mem rl changed_u_files
                  then Some rl 
                  else None
         ) in 
         let accu = ref [] in 
         let new_details = Assistance_image.image (
            fun old_pair->
              let rl = fst old_pair in 
              match optional_new_rl rl with 
               Some(new_rl) -> 
              let new_pair = (new_rl,Assistance_fw_with_archives.compute_small_details_on_one_file new_parent new_rl) in 
              let _ = (accu:=(rl,Some new_pair)::(!accu)) in 
              new_pair 
             | None -> old_pair  
          ) old_details in    
         (Assistance_fw_poly.extend_fw_with_archives_to_fw_with_small_details 
         new_parent 
         ~small_details_in_files:new_details,
         (List.rev(!accu),(file_renamings,changed_u_files@changed_a_files))) ;;  
   
   
   let rename_subdirectory_as fw (old_subdir,new_subdir)=   
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,original_reps) = Assistance_fw_with_archives.rename_subdirectory_as old_parent (old_subdir,new_subdir) in 
      let accu = ref [] in
      let new_details = Assistance_image.image (
         fun old_pair->
         let rl = fst old_pair in
         match Assistance_dfn_rootless.soak (old_subdir,new_subdir) rl with 
         (Some new_rl) -> 
            let new_pair = (new_rl,Assistance_fw_with_archives.compute_small_details_on_one_file new_parent new_rl) in 
                let _ = (accu:=(rl,Some new_pair)::(!accu)) in 
                new_pair 
         | None -> old_pair        
      ) old_details in 
      (Assistance_fw_poly.extend_fw_with_archives_to_fw_with_small_details 
      new_parent 
      ~small_details_in_files:new_details,(List.rev(!accu),original_reps)) ;;   
   
   let replace_string fw (replacee,replacer)=
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,(changed_a_files,changed_u_files)) = Assistance_fw_with_archives.replace_string old_parent (replacee,replacer) in
      let accu = ref [] in 
      let new_details = Assistance_image.image (
         fun old_pair->
         let rl = fst old_pair in
         if List.mem rl changed_u_files
         then let new_pair = (rl,Assistance_fw_with_archives.compute_small_details_on_one_file new_parent rl) in 
              let _ = (accu:=(rl,Some new_pair)::(!accu)) in 
              new_pair 
         else old_pair  
      ) old_details in
      (Assistance_fw_poly.extend_fw_with_archives_to_fw_with_small_details 
      new_parent 
      ~small_details_in_files:new_details,(List.rev(!accu),changed_a_files@changed_u_files));;   
   
   let replace_value fw ((preceding_files,path),(replacee,pre_replacer)) =
      let old_parent = parent fw 
      and old_details = small_details_in_files fw  in 
      let (new_parent,(all_changes,changed_files)) = 
           Assistance_fw_with_archives.replace_value 
            old_parent (preceding_files,path) (replacee,pre_replacer) in
      let accu = ref [] in 
      let new_details = Assistance_image.image (
         fun old_pair->
            let rl = fst old_pair in
            if List.mem rl changed_files
            then let new_pair = (rl,Assistance_fw_with_archives.compute_small_details_on_one_file new_parent rl) in 
                    let _ = (accu:=(rl,Some new_pair)::(!accu)) in 
                    new_pair 
            else old_pair  
         ) old_details in      
      (Assistance_fw_poly.extend_fw_with_archives_to_fw_with_small_details 
      new_parent 
      ~small_details_in_files:new_details,(List.rev(!accu),all_changes));;     
   
      let check_that_no_change_has_occurred fw =
         Assistance_fw_with_archives.check_that_no_change_has_occurred (parent fw) ;; 
   
      let latest_changes fw = Assistance_fw_with_archives.latest_changes (parent fw) ;;   
         
   end ;;
   
   
   
   let forget_modules = Private.forget_modules ;;
   let inspect_and_update = Private.inspect_and_update;;
   let of_configuration = Private.of_configuration ;;
   let of_configuration_and_list = Private.of_configuration_and_list ;;
   let overwrite_file_if_it_exists = Private.overwrite_file_if_it_exists ;;
   let plunge_fw_configuration config= Assistance_fw_poly.extend_fw_with_archives_to_fw_with_small_details (Assistance_fw_with_archives.plunge_fw_configuration config) ~small_details_in_files:[] ;; 
   let register_rootless_paths = Private.register_rootless_paths;;
   let relocate_module_to = Private.relocate_module_to;;
   let remove_files = Private.remove_files;;
   let rename_module_on_filename_level_and_in_files = Private.rename_module_on_filename_level_and_in_files ;;
   let rename_subdirectory_as = Private.rename_subdirectory_as;;
   let replace_string = Private.replace_string;;
   let replace_value = Private.replace_value;;
   let small_details_in_files = Private.small_details_in_files ;;
   let usual_compilable_files fw = Assistance_fw_with_archives.usual_compilable_files (Private.parent fw) ;;
   

end;;






module Assistance_fw_module_small_details=struct

(*

#use"Filewatching/fw_module_small_details.ml";;

From an associative list associating to each Dfn_rootless_t.t its small details,
we create a new list associating to each Dfa_module_t.t its small details.

*)


exception Nonadmissible_acolytes_list of Assistance_dfn_rootless_t.t list ;;
exception Several_locations_for_one_ending of (Assistance_dfa_ending_t.t * Assistance_dfn_rootless_t.t list) list;;
exception Several_subdirectories_for_one_ending of Assistance_dfn_rootless_t.t * Assistance_dfn_rootless_t.t ;;

module Private = struct 

let lex_order = ((fun (Assistance_dfa_module_t.M m1) (Assistance_dfa_module_t.M m2)->
   Assistance_total_ordering.lex_for_strings m1 m2) : Assistance_dfa_module_t.t Assistance_total_ordering_t.t);;



let compute_details_from_acolytes_list_for_one_module l=
   let temp1 = Assistance_image.image (fun (rl,details)->(Assistance_dfn_rootless.to_ending rl,(rl,details))) l in 
   let temp2 = Assistance_listennou.partition_according_to_fst temp1 in 
   let should_be_empty = List.filter (fun (edg,l_rl)->List.length(l_rl)>1) temp2 in 
   if should_be_empty<>[]
   then let clearer_picture = Assistance_image.image (fun (edg,detailed_l) -> (edg,Assistance_image.image fst detailed_l) ) 
                       should_be_empty in 
        raise(Several_locations_for_one_ending(clearer_picture))
   else 
   let temp3 = Assistance_image.image (fun (edg,l_rl)->
      (Assistance_dfa_ocaml_ending.of_ending edg,List.hd l_rl)
      ) temp2   in 
   let (temp4,temp5) = List.partition 
    (function (edg,rl)->edg=Assistance_dfa_ocaml_ending_t.Mli) temp3 in 
   if (temp3=[]) || (List.length(temp5)>1)
   then raise(Nonadmissible_acolytes_list(Assistance_image.image (fun (_,(rl,_))->rl) temp3))
   else        
   let opt_mli_detailed_rless = (
      if temp4=[] 
      then None 
      else Some(snd(List.hd temp4))) in
   let principal_detailed_rless = (
      if temp5=[]
      then Assistance_option.unpack opt_mli_detailed_rless 
      else snd(List.hd temp5)    
   ) in     
   let (principal_rless,principal_details) = principal_detailed_rless in 
   let principal_subdir = Assistance_dfn_rootless.to_subdirectory principal_rless in
   let _ = (
      match opt_mli_detailed_rless with 
      None -> () 
      |Some(mli_rless,_) ->
         let mli_subdir = Assistance_dfn_rootless.to_subdirectory mli_rless in 
         if mli_subdir <> principal_subdir 
         then raise(Several_subdirectories_for_one_ending(principal_rless,mli_rless))    
   ) in
   let principal_coatoms = Assistance_fw_file_small_details.used_modules principal_details in 
   let (all_coatoms,opt_mli_mt) = (
      match opt_mli_detailed_rless with 
      None -> (principal_coatoms,None)
      |Some(_,mli_details) ->
         let mli_coatoms = Assistance_fw_file_small_details.used_modules mli_details in 
         (principal_coatoms @ mli_coatoms,
         Some(Assistance_fw_file_small_details.modification_time mli_details))    
   ) in
   let all_coatoms_in_order = Assistance_ordered.sort lex_order all_coatoms in 
   {
      Assistance_fw_module_small_details_t.used_modules = all_coatoms_in_order ;
      used_libraries = Assistance_fw_file_small_details.used_libraries principal_details ;
      has_printer = Assistance_fw_file_small_details.has_printer principal_details ;
      subdirectory = principal_subdir ;
      principal_ending = Assistance_dfa_ocaml_ending.of_ending (Assistance_dfn_rootless.to_ending principal_rless) ;
      mli_present = (opt_mli_detailed_rless <> None) ;
      principal_modification_time = Assistance_fw_file_small_details.modification_time principal_details ;
      mli_modification_time = opt_mli_mt ;
   };;

   
let compute_details_from_acolytes_list_for_several_modules compilable_files =
    let temp1 = Assistance_image.image (fun (rless,details)->
       (Assistance_dfn_rootless.to_module rless,(rless,details))  
    ) compilable_files in 
    let temp2 = Assistance_listennou.partition_according_to_fst temp1 in 
    Assistance_image.image (fun (mn,l)->
      (mn,compute_details_from_acolytes_list_for_one_module l)
      ) temp2 ;;
 
let is_overriden_by_item rl (rl2,opt) =
    if rl2=rl then true else match opt with 
    None -> false 
    |Some(rl3,_) -> rl3 = rl ;;

let is_overriden_by_list rl l = List.exists (is_overriden_by_item rl) l ;;    

let recompute_module_details_from_list_of_changes fw mod_name unfiltered_l =
   let l = List.filter (fun (rl,_)->(Assistance_dfn_rootless.to_module rl) = mod_name ) unfiltered_l in 
   let extra_data = List.filter (
          fun (rl,_) ->
          ((Assistance_dfn_rootless.to_module rl) = mod_name) && (not(is_overriden_by_list rl l))
      ) ( Assistance_fw_with_small_details.small_details_in_files fw) in 
      compute_details_from_acolytes_list_for_one_module ((Assistance_option.filter_and_unpack snd l)@extra_data) ;;     


end ;;   


let has_printer fw = fw.Assistance_fw_module_small_details_t.has_printer ;;  


let compute_details_from_acolytes_list_for_one_module = Private.compute_details_from_acolytes_list_for_one_module ;;

let compute_details_from_acolytes_list_for_several_modules = Private.compute_details_from_acolytes_list_for_several_modules ;;

let modularize_details fw  = 
   let u_files=Assistance_fw_with_small_details.usual_compilable_files fw in 
   let temp1=List.filter (fun (rl,_)->List.mem rl u_files)
      (Assistance_fw_poly.small_details_in_files fw)  in
   Private.compute_details_from_acolytes_list_for_several_modules temp1 ;;


let mli_present fw = fw.Assistance_fw_module_small_details_t.mli_present ;; 
let opt_mli_modification_time fw = fw.Assistance_fw_module_small_details_t.mli_modification_time ;;      
let principal_ending fw = fw.Assistance_fw_module_small_details_t.principal_ending ;; 
let principal_modification_time fw = fw.Assistance_fw_module_small_details_t.principal_modification_time ;;     
let recompute_module_details_from_list_of_changes = Private.recompute_module_details_from_list_of_changes ;;
let subdirectory fw = fw.Assistance_fw_module_small_details_t.subdirectory ;;  
let used_libraries fw = fw.Assistance_fw_module_small_details_t.used_libraries ;;  
let used_modules fw = fw.Assistance_fw_module_small_details_t.used_modules ;;  



end;;






module Assistance_reconstruct_linear_poset=struct

(*

#use"reconstruct_linear_poset.ml";;

Computes the (canonical) maximal acyclic sub-poset of a given poset, returns
it as a list L where each element of L is a triple (a,anc_a,a_is_clean)
where anc_a is the list of all ancestors of a, ordered as in L, and a_is_clean
is a boolean indicating if a is the ancestor or a descendant of an "active"
element.

Also returns a (non-canonical,non-exhaustive) set of cycles.


*)


let iterator coat 
  (checked,checked_union,cycles,cycles_union,between,not_yet_checked,opt1)=
    (* 
    between is a "chained" list of pairs (x1,x2),(x2,x3), ...
    (stocked in reverse actually)
    that will possibly lead to a cycle
    *)
    if opt1<>None then ([],Assistance_set_of_polys.empty_set,[],Assistance_set_of_polys.empty_set,[],[],opt1) else
    if (between,not_yet_checked)=([],[]) 
    then ([],Assistance_set_of_polys.empty_set,[],Assistance_set_of_polys.empty_set,[],[],
          Some(cycles,Assistance_listennou.rev_map (fun (z,p)->(z,fst p)) checked)) 
    else
    let a=
    	  (if between=[] 
    	   then List.hd(not_yet_checked)
           else snd(List.hd(between))
          ) in
    let not_yet_checked2=List.filter (fun z->z<>a) not_yet_checked in
    let coat_a=coat(a) in
    let coatoms_of_a=Assistance_set_of_polys.safe_set(coat_a) in
    let temp1=Assistance_set_of_polys.setminus coatoms_of_a checked_union in
    if Assistance_set_of_polys.length(temp1)=0
    then let temp3=coatoms_of_a::(Assistance_image.image (fun z->snd(List.assoc z checked)) 
                      (coat_a)) in
         let ordered_set_version=Assistance_set_of_polys.fold_merge(temp3) in
         let temp4=Assistance_option.filter_and_unpack (
           fun (b,_)->if Assistance_set_of_polys.mem b ordered_set_version
             then Some(b)
             else None
         ) checked in
         let list_version=List.rev(temp4) in
         let data_for_a=(list_version,ordered_set_version) in
         ((a,data_for_a)::checked,Assistance_set_of_polys.insert a checked_union,
         cycles,cycles_union,[],not_yet_checked2,None)
    else
    if Assistance_set_of_polys.mem a temp1
    then ([],Assistance_set_of_polys.empty_set,[a]::cycles,Assistance_set_of_polys.insert a cycles_union,
         [],not_yet_checked2,None) 
    else 
    if (not(Assistance_set_of_polys.does_not_intersect temp1 cycles_union))
    then (checked,checked_union,cycles,Assistance_set_of_polys.insert a cycles_union,
         [],not_yet_checked2,None) 
    else 
    (*see if we can close the cycle *)
    match Assistance_option.seek(fun (x,y)->Assistance_set_of_polys.mem x temp1) between with
     None->(checked,checked_union,cycles,cycles_union,
     		(a,Assistance_set_of_polys.hd temp1)::between,not_yet_checked,None)
    |Some(p)->
        let (before,_,after)=Assistance_three_parts.select_center_element_and_reverse_left (fun x->x=p) between in
        let temp2=Assistance_image.image fst before in
        let new_cycle=(fst p)::(temp2@[a]) in
        let ordered_cycle=Assistance_set_of_polys.sort new_cycle in
        let not_yet_checked3=List.filter (fun z->Assistance_set_of_polys.nmem z ordered_cycle) not_yet_checked in
        (checked,checked_union,new_cycle::cycles,
        Assistance_set_of_polys.merge ordered_cycle cycles_union,
        [],not_yet_checked3,None);;

let reconstruct_linear_poset coat l=
  let rec tempf=(fun
  (checked,checked_union,cycles,cycles_union,between,not_yet_checked,opt)->
    if opt<>None
    then Assistance_option.unpack opt
    else tempf(iterator coat 
    (checked,checked_union,cycles,cycles_union,between,not_yet_checked,opt))
    ) in
    tempf([],Assistance_set_of_polys.empty_set,[],Assistance_set_of_polys.empty_set,[],l,None);;
    
(*

let sugar i=
   if (i<4)||(i>20) then [] else
   if i=11 then [5] else [i+1];;
    
reconstruct_linear_poset sugar (ennig 1 30);;  

let some_edges=
  [
    (1,4);(1,16);(2,6);(2,7);(3,16);(7,11);(7,19);(8,11);(8,15);(9,19);
    (10,1);(10,18);(11,2);(12,16);(13,5);(15,13);(16,17);(17,10);(17,14);(19,20);
    (20,21);(21,9)
  
  ];;

let brown j=image fst (List.filter (fun x->snd(x)=j) some_edges);;

reconstruct_linear_poset brown (ennig 1 21);;  


*)
 
    
    
    
               

end;;






module Assistance_fw_determine_order=struct

(*

#use"Filewatching/fw_determine_order.ml";;

*)



exception Circular_dependencies_detected ;;

module Private = struct 

let treat_circular_dependencies m_cycles= 
      if m_cycles=[]
      then ()
      else
      let cycles = Assistance_image.image (Assistance_image.image Assistance_dfa_module.to_line) m_cycles in    
      let temp1=Assistance_image.image(String.concat " -> ") cycles in
      let temp2="\n\n The following cycles have been detected : "^
        (String.concat "\n\n" temp1) in
      let _ = (print_string temp2;flush stdout) in 
      raise Circular_dependencies_detected ;;

let lex_order = ((fun (Assistance_dfa_module_t.M m1) (Assistance_dfa_module_t.M m2)->
   Assistance_total_ordering.lex_for_strings m1 m2) : Assistance_dfa_module_t.t Assistance_total_ordering_t.t);;

let compute_dependencies  l =
  let lex_sort = Assistance_ordered.sort lex_order in 
  let modules = Assistance_image.image fst l in 
  let modules_in_lex_order = lex_sort modules in 
  let coatoms_in_lex_order = Assistance_memoized.make (fun mname ->
     let details = List.assoc mname  l in 
     let temp1 = lex_sort(Assistance_fw_module_small_details.used_modules  details) in 
     Assistance_ordered.intersect lex_order  modules_in_lex_order temp1
  )     in 
  let (cycles,good_list) = Assistance_reconstruct_linear_poset.reconstruct_linear_poset coatoms_in_lex_order  
    modules in 
  let _ = treat_circular_dependencies cycles in
  let coatoms = Assistance_memoized.make (fun mname ->
    let old_order = coatoms_in_lex_order mname in 
    Assistance_option.filter_and_unpack (fun (mn,_)->
       if Assistance_ordered.mem lex_order mn old_order 
       then Some mn 
       else None  
      ) good_list
   ) in 
  Assistance_image.image (fun (mn,ancestors)->(mn,(coatoms mn,ancestors))) good_list ;; 

let compute_coatoms_in_small_extension older_modules extension =
   let modules_in_correct_order = 
       (Assistance_image.image fst older_modules) @ (Assistance_image.image fst extension) in 
   let lex_sort = Assistance_ordered.sort lex_order in 
   let coatoms = (fun mname ->
      let details = List.assoc mname  extension in 
      let temp1 = lex_sort(Assistance_fw_module_small_details.used_modules details) in 
      List.filter  (fun mn-> Assistance_ordered.mem lex_order mn temp1) modules_in_correct_order
       )  in
   Assistance_image.image (fun (mname,_)->(mname,coatoms mname)) extension ;;           

let rec iterator_for_ancestor_computation (treated,to_be_treated) =
   match to_be_treated with 
   [] -> treated 
   | (mn,coat_mn) :: others ->
      let temp1 = (coat_mn) :: (Assistance_image.image (fun mn2->snd(List.assoc mn2 treated)) coat_mn) in 
      let ancestors_in_lex_order = Assistance_ordered.sort lex_order (List.flatten temp1) in 
      let ancestors_mn = Assistance_option.filter_and_unpack (
         fun (mn3,_) ->
            if Assistance_ordered.mem lex_order mn3  ancestors_in_lex_order 
            then Some mn3 
            else None   
      )  treated in 
      iterator_for_ancestor_computation (treated@[(mn,(coat_mn,ancestors_mn))],others) ;;  

let compute_coatoms_and_ancestors_in_small_extension older_modules extension =
   let ext_with_coatoms = compute_coatoms_in_small_extension older_modules extension in 
   iterator_for_ancestor_computation (older_modules,ext_with_coatoms) ;;

end ;;   

let compute_coatoms_and_ancestors_in_small_extension = Private.compute_coatoms_and_ancestors_in_small_extension ;;
let main = Private.compute_dependencies ;;




end;;






module Assistance_fw_with_dependencies=struct

(*

#use"Filewatching/fw_with_dependencies.ml";;

*)

exception Absent_module of string;;
exception Duplicate_module_already_exists of string;;
exception Find_subdir_from_suffix_exn of string * (Assistance_dfa_subdirectory_t.t list) ;;

module Private = struct

 let expand_index idx = (idx,Assistance_fw_indexer.get_state idx) ;;
 let index fw = Assistance_fw_poly.index_for_caching fw ;; 
 let parent fw = Assistance_fw_poly.parent fw ;;
 let usual_extension fw_parent instance_idx = 
    Assistance_fw_poly.extend_fw_with_small_details_to_fw_with_dependencies 
      fw_parent 
       ~index_for_caching:(expand_index instance_idx) ;;


(* Pre-processed text starts here *)


module Entrance = struct 

let forget_modules = Assistance_fw_with_small_details.forget_modules ;;

let inspect_and_update = Assistance_fw_with_small_details.inspect_and_update ;;

let of_configuration = Assistance_fw_with_small_details.of_configuration ;;

let of_configuration_and_list = Assistance_fw_with_small_details.of_configuration_and_list ;;

let overwrite_file_if_it_exists = Assistance_fw_with_small_details.overwrite_file_if_it_exists ;;

let plunge_fw_configuration = Assistance_fw_with_small_details.plunge_fw_configuration ;;

let register_rootless_paths = Assistance_fw_with_small_details.register_rootless_paths ;;

let relocate_module_to = Assistance_fw_with_small_details.relocate_module_to ;;

let remove_files = Assistance_fw_with_small_details.remove_files ;;

let rename_module_on_filename_level_and_in_files = Assistance_fw_with_small_details.rename_module_on_filename_level_and_in_files ;;

let rename_subdirectory_as = Assistance_fw_with_small_details.rename_subdirectory_as ;;

let replace_string = Assistance_fw_with_small_details.replace_string ;;

let replace_value = Assistance_fw_with_small_details.replace_value ;;end ;;


module Cached = struct 

let forget_modules old_fw mods_to_be_erased =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Entrance.forget_modules old_parent mods_to_be_erased in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Assistance_fw_indexer.push_state instance_idx in 
 ( usual_extension new_parent instance_idx ,extra ) ;; 

let inspect_and_update old_fw  =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Entrance.inspect_and_update old_parent  in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Assistance_fw_indexer.push_state instance_idx in 
 ( usual_extension new_parent instance_idx ,extra ) ;; 

let of_configuration config =  
 let new_parent = Entrance.of_configuration config in 
 let instance_idx = Assistance_fw_indexer.create_new_instance () in 
  usual_extension new_parent instance_idx  ;; 

let of_configuration_and_list pair =  
 let new_parent = Entrance.of_configuration_and_list pair in 
 let instance_idx = Assistance_fw_indexer.create_new_instance () in 
  usual_extension new_parent instance_idx  ;; 

let overwrite_file_if_it_exists old_fw pair =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Entrance.overwrite_file_if_it_exists old_parent pair in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Assistance_fw_indexer.push_state instance_idx in 
 ( usual_extension new_parent instance_idx ,extra ) ;; 

let plunge_fw_configuration config =  
 let new_parent = Entrance.plunge_fw_configuration config in 
 let instance_idx = Assistance_fw_indexer.create_new_instance () in 
  usual_extension new_parent instance_idx  ;; 

let register_rootless_paths old_fw rootlesses =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Entrance.register_rootless_paths old_parent rootlesses in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Assistance_fw_indexer.push_state instance_idx in 
 ( usual_extension new_parent instance_idx ,extra ) ;; 

let relocate_module_to old_fw pair =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Entrance.relocate_module_to old_parent pair in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Assistance_fw_indexer.push_state instance_idx in 
 ( usual_extension new_parent instance_idx ,extra ) ;; 

let remove_files old_fw files_to_be_removed =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Entrance.remove_files old_parent files_to_be_removed in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Assistance_fw_indexer.push_state instance_idx in 
 ( usual_extension new_parent instance_idx ,extra ) ;; 

let rename_module_on_filename_level_and_in_files old_fw triple =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Entrance.rename_module_on_filename_level_and_in_files old_parent triple in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Assistance_fw_indexer.push_state instance_idx in 
 ( usual_extension new_parent instance_idx ,extra ) ;; 

let rename_subdirectory_as old_fw pair =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Entrance.rename_subdirectory_as old_parent pair in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Assistance_fw_indexer.push_state instance_idx in 
 ( usual_extension new_parent instance_idx ,extra ) ;; 

let replace_string old_fw pair =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Entrance.replace_string old_parent pair in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Assistance_fw_indexer.push_state instance_idx in 
 ( usual_extension new_parent instance_idx ,extra ) ;; 

let replace_value old_fw pair =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Entrance.replace_value old_parent pair in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Assistance_fw_indexer.push_state instance_idx in 
 ( usual_extension new_parent instance_idx ,extra ) ;; end ;;


module Modularized_details = struct 

 let the_hashtbl = ((Hashtbl.create 10)) ;; 
 let force_get fw = Assistance_fw_module_small_details.modularize_details (parent fw)
 let get fw = 
   let idx = index fw in 
   match Hashtbl.find_opt the_hashtbl idx with 
      Some(old_answer)-> old_answer 
     | None -> 
   let answer = force_get fw in 
   let _ = (Hashtbl.add the_hashtbl idx answer) in 
   answer ;; 

let forget_modules old_fw mods_to_be_erased =  
 let visible = Cached.forget_modules old_fw mods_to_be_erased in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let answer = List.filter (fun (mn,_)->not(List.mem mn mods_to_be_erased)) old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let inspect_and_update old_fw  =  
 let visible = Cached.inspect_and_update old_fw  in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let ((a_files,u_files),changed_u_files,changed_files) = extra in 
 let tempf = (
   fun old_pair ->
    let (mn,details) = old_pair in 
    let temp1 = List.filter (fun (rl,details2)->
       (Assistance_dfn_rootless.to_module rl)= mn
      ) changed_u_files in 
 if temp1 <> []
 then (mn, Assistance_fw_module_small_details.compute_details_from_acolytes_list_for_one_module temp1)
    else old_pair
 ) in 
 let answer = Assistance_image.image tempf old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let of_configuration config =  
 let new_fw = Cached.of_configuration config in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration_and_list pair =  
 let new_fw = Cached.of_configuration_and_list pair in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let overwrite_file_if_it_exists old_fw pair =  
 let visible = Cached.overwrite_file_if_it_exists old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let answer = ( match extra with 
      None -> old_val 
      |Some(change) ->
 let tempf = (
        fun old_pair -> 
          let (mn,details) = old_pair in 
          let temp1 = List.filter (fun (rl,details2)->
             (Assistance_dfn_rootless.to_module rl)= mn
            ) [change] in
          if temp1 <> []
          then let new_parent = parent new_fw in 
               (mn, Assistance_fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)
          else old_pair 
      ) in 
 Assistance_image.image tempf old_val) in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let plunge_fw_configuration config =  
 let new_fw = Cached.plunge_fw_configuration config in 
 let answer = [] in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let register_rootless_paths old_fw rootlesses =  
 let visible = Cached.register_rootless_paths old_fw rootlesses in 
 let (new_fw,extra) = visible in 
  let old_val = get old_fw in 
  let ((a_files,u_files,nc_files),new_details) = extra in 
  let old_mods = Assistance_image.image fst old_val in 
  let (overlapping,nonoverlapping) = List.partition (
     fun (rl,_) -> List.mem (Assistance_dfn_rootless.to_module rl) old_mods 
  ) new_details in 
  let tempf1 = (
    fun old_pair -> 
      let (mn,details) = old_pair in 
      let temp1 = Assistance_option.filter_and_unpack (fun (rl,details2)->
         if (Assistance_dfn_rootless.to_module rl)= mn
         then Some(rl,Some(rl,details2))
         else None 
        ) overlapping in
      if temp1 <> []
      then let new_parent = parent new_fw in 
           (mn, Assistance_fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)
      else old_pair 
  ) in 
  let answer = (Assistance_image.image tempf1 old_val)@
  (Assistance_fw_module_small_details.compute_details_from_acolytes_list_for_several_modules nonoverlapping) in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let relocate_module_to old_fw pair =  
 let visible = Cached.relocate_module_to old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let tempf = (
   fun old_pair -> 
     let (mn,details) = old_pair in 
     let temp1 = List.filter (fun (rl,new_pair_for_rl)->
        (Assistance_dfn_rootless.to_module rl)= mn
       ) (fst extra) in
     if temp1 <> []
     then let new_parent = parent new_fw in 
          (mn, Assistance_fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)
     else old_pair 
 ) in 
 let answer = Assistance_image.image tempf old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let remove_files old_fw files_to_be_removed =  
 let visible = Cached.remove_files old_fw files_to_be_removed in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let tempf = (
   fun old_pair -> 
     let (mn,details) = old_pair in 
     let temp1 = List.filter (fun (rl,new_pair_for_rl)->
        (Assistance_dfn_rootless.to_module rl)= mn
       ) extra in
     if temp1 <> []
     then let new_parent = parent new_fw in 
          (mn, Assistance_fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)
     else old_pair 
 ) in 
 let answer = Assistance_image.image tempf old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_module_on_filename_level_and_in_files old_fw triple =  
 let (new_fw,extra) = Cached.rename_module_on_filename_level_and_in_files old_fw triple in 
 let old_val = get old_fw in 
 let (old_mn,new_mn,_) = triple in 
 let tempf = (
   fun old_pair -> 
     let (pre_mn,details) = old_pair in 
     let temp1 = List.filter (fun (rl,new_pair_for_rl)->
        (Assistance_dfn_rootless.to_module rl)= pre_mn
       ) (fst extra) in
     if temp1 <> []
     then let new_parent = parent new_fw in 
          let mn = (if pre_mn = old_mn then new_mn else pre_mn) in 
          (mn, Assistance_fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)
     else old_pair 
 ) in 
 let answer = Assistance_image.image tempf old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 (new_fw,extra) ;;

let rename_subdirectory_as old_fw pair =  
 let visible = Cached.rename_subdirectory_as old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let tempf = (
   fun old_pair -> 
     let (mn,details) = old_pair in 
     let temp1 = List.filter (fun (rl,new_pair_for_rl)->
        (Assistance_dfn_rootless.to_module rl)= mn
       ) (fst extra) in
     if temp1 <> []
     then let new_parent = parent new_fw in 
          (mn, Assistance_fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)
     else old_pair 
 ) in 
 let answer = Assistance_image.image tempf old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_string old_fw pair =  
 let visible = Cached.replace_string old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let tempf = (
   fun old_pair -> 
     let (mn,details) = old_pair in 
     let temp1 = List.filter (fun (rl,new_pair_for_rl)->
        (Assistance_dfn_rootless.to_module rl)= mn
       ) (fst extra) in
     if temp1 <> []
     then let new_parent = parent new_fw in 
          (mn, Assistance_fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)
     else old_pair 
 ) in 
 let answer = Assistance_image.image tempf old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_value old_fw pair =  
 let visible = Cached.replace_value old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let tempf = (
   fun old_pair -> 
     let (mn,details) = old_pair in 
     let temp1 = List.filter (fun (rl,new_pair_for_rl)->
        (Assistance_dfn_rootless.to_module rl)= mn
       ) (fst extra) in
     if temp1 <> []
     then let new_parent = parent new_fw in 
          (mn, Assistance_fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)
     else old_pair 
 ) in 
 let answer = Assistance_image.image tempf old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

end ;;


module Order = struct 

 let the_hashtbl = ((Hashtbl.create 10)) ;; 
 let force_get fw = Assistance_fw_determine_order.main (Modularized_details.get fw)
 let get fw = 
   let idx = index fw in 
   match Hashtbl.find_opt the_hashtbl idx with 
      Some(old_answer)-> old_answer 
     | None -> 
   let answer = force_get fw in 
   let _ = (Hashtbl.add the_hashtbl idx answer) in 
   answer ;; 

let forget_modules old_fw mods_to_be_erased =  
 let visible = Modularized_details.forget_modules old_fw mods_to_be_erased in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let answer = List.filter (fun (mn,_)->not(List.mem mn mods_to_be_erased)) old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let inspect_and_update old_fw  =  
 let visible = Modularized_details.inspect_and_update old_fw  in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let modules_in_old_order = Assistance_image.image fst old_val in 
 let details_in_old_order = Assistance_ordered_misc.reorder_list_of_pairs_using_list_of_singles
 (Modularized_details.get new_fw) modules_in_old_order in 
 let answer = Assistance_fw_determine_order.main  details_in_old_order in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let of_configuration config =  
 let new_fw = Modularized_details.of_configuration config in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration_and_list pair =  
 let new_fw = Modularized_details.of_configuration_and_list pair in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let overwrite_file_if_it_exists old_fw pair =  
 let visible = Modularized_details.overwrite_file_if_it_exists old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let modules_in_old_order = Assistance_image.image fst old_val in 
 let details_in_old_order = Assistance_ordered_misc.reorder_list_of_pairs_using_list_of_singles
 (Modularized_details.get new_fw) modules_in_old_order in 
 let answer = Assistance_fw_determine_order.main  details_in_old_order in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let plunge_fw_configuration config =  
 let new_fw = Modularized_details.plunge_fw_configuration config in 
 let answer = [] in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let register_rootless_paths old_fw rootlesses =  
 let visible = Modularized_details.register_rootless_paths old_fw rootlesses in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let extended_details_list = Modularized_details.get new_fw in 
 let new_details = Assistance_listennou.big_tail (List.length old_val) extended_details_list in
 let new_modules_in_order = Assistance_image.image fst (Assistance_fw_determine_order.main new_details) in 
 let new_details_in_order = Assistance_ordered_misc.reorder_list_of_pairs_using_list_of_singles
     new_details new_modules_in_order in 
 let answer = Assistance_fw_determine_order.compute_coatoms_and_ancestors_in_small_extension
      old_val new_details_in_order in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let relocate_module_to old_fw pair =  
 let visible = Modularized_details.relocate_module_to old_fw pair in 
 let (new_fw,extra) = visible in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let remove_files old_fw files_to_be_removed =  
 let visible = Modularized_details.remove_files old_fw files_to_be_removed in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_module_on_filename_level_and_in_files old_fw triple =  
 let visible = Modularized_details.rename_module_on_filename_level_and_in_files old_fw triple in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let (old_mname,new_mname,_) = triple in
 let rep = (fun mn->if mn = old_mname then new_mname else mn) in  
 let answer = Assistance_image.image (fun (mn2,(coat_mn2,ancestors_mn2)) ->
     (rep mn2,(Assistance_image.image rep coat_mn2,Assistance_image.image rep ancestors_mn2))
 ) old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_subdirectory_as old_fw pair =  
 let visible = Modularized_details.rename_subdirectory_as old_fw pair in 
 let (new_fw,extra) = visible in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_string old_fw pair =  
 let visible = Modularized_details.replace_string old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let modules_in_old_order = Assistance_image.image fst old_val in 
 let details_in_old_order = Assistance_ordered_misc.reorder_list_of_pairs_using_list_of_singles
 (Modularized_details.get new_fw) modules_in_old_order in 
 let answer = Assistance_fw_determine_order.main  details_in_old_order in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_value old_fw pair =  
 let visible = Modularized_details.replace_value old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let modules_in_old_order = Assistance_image.image fst old_val in 
 let details_in_old_order = Assistance_ordered_misc.reorder_list_of_pairs_using_list_of_singles
 (Modularized_details.get new_fw) modules_in_old_order in 
 let answer = Assistance_fw_determine_order.main  details_in_old_order in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

end ;;


module Needed_dirs = struct 

 let the_hashtbl = ((Hashtbl.create 10)) ;; 
 let force_get fw = let details = Modularized_details.get fw in 
 let subdir_at_module = (fun mn->
   Assistance_fw_module_small_details.subdirectory(List.assoc mn details)
 ) in 
 Assistance_image.image (
  fun (mn,(_,ancestors)) ->
   let temp1 = Assistance_image.image subdir_at_module (mn::ancestors) in 
   (mn,Assistance_ordered.sort Assistance_total_ordering.standard temp1)
) (Order.get fw) ;;
 let get fw = 
   let idx = index fw in 
   match Hashtbl.find_opt the_hashtbl idx with 
      Some(old_answer)-> old_answer 
     | None -> 
   let answer = force_get fw in 
   let _ = (Hashtbl.add the_hashtbl idx answer) in 
   answer ;; 

let forget_modules old_fw mods_to_be_erased =  
 let visible = Order.forget_modules old_fw mods_to_be_erased in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let inspect_and_update old_fw  =  
 let visible = Order.inspect_and_update old_fw  in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let of_configuration config =  
 let new_fw = Order.of_configuration config in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration_and_list pair =  
 let new_fw = Order.of_configuration_and_list pair in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let overwrite_file_if_it_exists old_fw pair =  
 let visible = Order.overwrite_file_if_it_exists old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let plunge_fw_configuration config =  
 let new_fw = Order.plunge_fw_configuration config in 
 let answer = [] in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let register_rootless_paths old_fw rootlesses =  
 let visible = Order.register_rootless_paths old_fw rootlesses in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let relocate_module_to old_fw pair =  
 let visible = Order.relocate_module_to old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let remove_files old_fw files_to_be_removed =  
 let visible = Order.remove_files old_fw files_to_be_removed in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_module_on_filename_level_and_in_files old_fw triple =  
 let visible = Order.rename_module_on_filename_level_and_in_files old_fw triple in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let (old_mname,new_mname,_) = triple in
 let rep = (fun mn->if mn = old_mname then new_mname else mn) in 
 let answer = Assistance_image.image (fun (mn2,sdirs) -> (rep mn2,sdirs)) old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_subdirectory_as old_fw pair =  
 let visible = Order.rename_subdirectory_as old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let rep = (fun sdir ->
   match Assistance_dfa_subdirectory.soak pair sdir with 
   None -> sdir 
   |Some new_sdir -> new_sdir   
 ) in 
 let answer = Assistance_image.image (fun (mn,sdirs)->(mn,Assistance_image.image rep sdirs) ) old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_string old_fw pair =  
 let visible = Order.replace_string old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_value old_fw pair =  
 let visible = Order.replace_value old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

end ;;


module Needed_libs = struct 

 let the_hashtbl = ((Hashtbl.create 10)) ;; 
 let force_get fw = let details = Modularized_details.get fw in 
 let needed_libs_at_module = (fun mn->
   Assistance_fw_module_small_details.used_libraries(List.assoc mn details)
 ) in 
 Assistance_image.image (
  fun (mn,(_,ancestors)) ->
   let temp1 = List.flatten(Assistance_image.image needed_libs_at_module (mn::ancestors)) in 
   (mn,Assistance_ordered.sort Assistance_total_ordering.standard temp1)
) (Order.get fw) ;;
 let get fw = 
   let idx = index fw in 
   match Hashtbl.find_opt the_hashtbl idx with 
      Some(old_answer)-> old_answer 
     | None -> 
   let answer = force_get fw in 
   let _ = (Hashtbl.add the_hashtbl idx answer) in 
   answer ;; 

let forget_modules old_fw mods_to_be_erased =  
 let visible = Needed_dirs.forget_modules old_fw mods_to_be_erased in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let inspect_and_update old_fw  =  
 let visible = Needed_dirs.inspect_and_update old_fw  in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let of_configuration config =  
 let new_fw = Needed_dirs.of_configuration config in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration_and_list pair =  
 let new_fw = Needed_dirs.of_configuration_and_list pair in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let overwrite_file_if_it_exists old_fw pair =  
 let visible = Needed_dirs.overwrite_file_if_it_exists old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let plunge_fw_configuration config =  
 let new_fw = Needed_dirs.plunge_fw_configuration config in 
 let answer = [] in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let register_rootless_paths old_fw rootlesses =  
 let visible = Needed_dirs.register_rootless_paths old_fw rootlesses in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let relocate_module_to old_fw pair =  
 let visible = Needed_dirs.relocate_module_to old_fw pair in 
 let (new_fw,extra) = visible in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let remove_files old_fw files_to_be_removed =  
 let visible = Needed_dirs.remove_files old_fw files_to_be_removed in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_module_on_filename_level_and_in_files old_fw triple =  
 let visible = Needed_dirs.rename_module_on_filename_level_and_in_files old_fw triple in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let (old_mname,new_mname,_) = triple in
 let rep = (fun mn->if mn = old_mname then new_mname else mn) in 
 let answer = Assistance_image.image (fun (mn2,libs) -> (rep mn2,libs)) old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_subdirectory_as old_fw pair =  
 let visible = Needed_dirs.rename_subdirectory_as old_fw pair in 
 let (new_fw,extra) = visible in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_string old_fw pair =  
 let visible = Needed_dirs.replace_string old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_value old_fw pair =  
 let visible = Needed_dirs.replace_value old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

end ;;


module All_subdirectories = struct 

 let the_hashtbl = ((Hashtbl.create 10)) ;; 
 let force_get fw =  let details = Modularized_details.get fw in 
 Assistance_ordered.sort Assistance_total_ordering.standard (Assistance_image.image (
  fun (mn,details_on_mn) ->
  Assistance_fw_module_small_details.subdirectory(details_on_mn)
) details) ;;
 let get fw = 
   let idx = index fw in 
   match Hashtbl.find_opt the_hashtbl idx with 
      Some(old_answer)-> old_answer 
     | None -> 
   let answer = force_get fw in 
   let _ = (Hashtbl.add the_hashtbl idx answer) in 
   answer ;; 

let forget_modules old_fw mods_to_be_erased =  
 let visible = Needed_libs.forget_modules old_fw mods_to_be_erased in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let inspect_and_update old_fw  =  
 let visible = Needed_libs.inspect_and_update old_fw  in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let of_configuration config =  
 let new_fw = Needed_libs.of_configuration config in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration_and_list pair =  
 let new_fw = Needed_libs.of_configuration_and_list pair in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let overwrite_file_if_it_exists old_fw pair =  
 let visible = Needed_libs.overwrite_file_if_it_exists old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let plunge_fw_configuration config =  
 let new_fw = Needed_libs.plunge_fw_configuration config in 
 let answer = [] in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let register_rootless_paths old_fw rootlesses =  
 let visible = Needed_libs.register_rootless_paths old_fw rootlesses in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let (_,novelties) = extra in 
 let possibly_new = Assistance_ordered.sort Assistance_total_ordering.standard 
   (Assistance_image.image (fun (rl,dets)->Assistance_dfn_rootless.to_subdirectory rl  ) novelties) in 
 let answer = Assistance_ordered.merge Assistance_total_ordering.standard possibly_new old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let relocate_module_to old_fw pair =  
 let visible = Needed_libs.relocate_module_to old_fw pair in 
 let (new_fw,extra) = visible in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let remove_files old_fw files_to_be_removed =  
 let visible = Needed_libs.remove_files old_fw files_to_be_removed in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_module_on_filename_level_and_in_files old_fw triple =  
 let visible = Needed_libs.rename_module_on_filename_level_and_in_files old_fw triple in 
 let (new_fw,extra) = visible in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_subdirectory_as old_fw pair =  
 let visible = Needed_libs.rename_subdirectory_as old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let rep = (fun sdir ->
   match Assistance_dfa_subdirectory.soak pair sdir with 
   None -> sdir 
   |Some new_sdir -> new_sdir   
 ) in 
 let answer = Assistance_image.image rep old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_string old_fw pair =  
 let visible = Needed_libs.replace_string old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_value old_fw pair =  
 let visible = Needed_libs.replace_value old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

end ;;


module All_printables = struct 

 let the_hashtbl = ((Hashtbl.create 10)) ;; 
 let force_get fw =  let mods_without_subdirs = Assistance_option.filter_and_unpack (
  fun (mn,details) ->
 if Assistance_fw_module_small_details.has_printer details
  then Some mn
  else None
 ) (Modularized_details.get fw)
 and main_table = Modularized_details.get fw in 
 Assistance_image.image (
    fun mn ->
      let details = List.assoc mn main_table in 
      let subdir = Assistance_fw_module_small_details.subdirectory details in 
      Assistance_dfn_join.subdirectory_to_module subdir mn
 ) mods_without_subdirs ;;
 let get fw = 
   let idx = index fw in 
   match Hashtbl.find_opt the_hashtbl idx with 
      Some(old_answer)-> old_answer 
     | None -> 
   let answer = force_get fw in 
   let _ = (Hashtbl.add the_hashtbl idx answer) in 
   answer ;; 

let forget_modules old_fw mods_to_be_erased =  
 let visible = All_subdirectories.forget_modules old_fw mods_to_be_erased in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let answer = List.filter (fun middle->
    not(List.mem (Assistance_dfn_middle.to_module middle) mods_to_be_erased)) old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let inspect_and_update old_fw  =  
 let visible = All_subdirectories.inspect_and_update old_fw  in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let of_configuration config =  
 let new_fw = All_subdirectories.of_configuration config in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration_and_list pair =  
 let new_fw = All_subdirectories.of_configuration_and_list pair in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let overwrite_file_if_it_exists old_fw pair =  
 let visible = All_subdirectories.overwrite_file_if_it_exists old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let plunge_fw_configuration config =  
 let new_fw = All_subdirectories.plunge_fw_configuration config in 
 let answer = [] in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let register_rootless_paths old_fw rootlesses =  
 let visible = All_subdirectories.register_rootless_paths old_fw rootlesses in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let relocate_module_to old_fw pair =  
 let visible = All_subdirectories.relocate_module_to old_fw pair in 
 let (new_fw,extra) = visible in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let remove_files old_fw files_to_be_removed =  
 let visible = All_subdirectories.remove_files old_fw files_to_be_removed in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_module_on_filename_level_and_in_files old_fw triple =  
 let visible = All_subdirectories.rename_module_on_filename_level_and_in_files old_fw triple in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let (old_mname,new_mname,_) = triple in
 let rep = Assistance_dfn_middle.rename_module (old_mname,new_mname) in 
 let answer = Assistance_image.image rep old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_subdirectory_as old_fw pair =  
 let visible = All_subdirectories.rename_subdirectory_as old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let (old_sdir,new_sdir) = pair in
 let s_new_sdir = Assistance_dfa_subdirectory.without_trailing_slash new_sdir in 
 let rep = Assistance_dfn_middle.rename_endsubdirectory (old_sdir,s_new_sdir) in 
 let answer = Assistance_image.image rep old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_string old_fw pair =  
 let visible = All_subdirectories.replace_string old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_value old_fw pair =  
 let visible = All_subdirectories.replace_value old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

end ;;


(* Pre-processed text ends here *)




  
  module Exit = All_printables ;; 

  let details_for_module  fw mn = List.assoc mn (Modularized_details.get fw) ;;
  let check_ending_on_module fw edg  mn=
   if edg=Assistance_fw_module_small_details.principal_ending (details_for_module fw mn)
   then true 
   else 
   if edg=Assistance_dfa_ocaml_ending_t.Mli
   then Assistance_fw_module_small_details.mli_present (details_for_module fw mn) 
   else false;;
  let modules_with_their_ancestors fw l=
   let temp1=Assistance_option.filter_and_unpack (
     fun (nm,_)->if List.mem nm l then Some nm else None
     ) (Order.get fw )   in 
   let temp2=Assistance_image.image (
     fun nm->
       (snd (List.assoc nm (Order.get fw)))@[nm] 
   ) temp1 in 
   let temp3=List.flatten temp2 in 
   Assistance_listennou.nonredundant_version temp3;;
  
  let root fw = Assistance_fw_poly.root  (parent fw) ;;

  let subdir_for_module fw mn =Assistance_fw_module_small_details.subdirectory (details_for_module fw mn) ;;

   let endingless_at_module cs mn=
   Assistance_dfn_endingless_t.J(
        root cs,
        subdir_for_module cs mn,
        mn
    );;

  let check_ending_in_at_module edg fw mn=
    if edg= Assistance_fw_module_small_details.principal_ending (details_for_module fw mn)
    then true 
    else 
    if edg=Assistance_dfa_ocaml_ending_t.Mli
    then Assistance_fw_module_small_details.mli_present (details_for_module fw mn)
    else false;;


  let acolytes_at_module fw mn=
    let eless = endingless_at_module fw mn in
    Assistance_option.filter_and_unpack (fun 
    edg->
      if check_ending_in_at_module edg fw mn
      then Some(Assistance_dfn_join.to_ending eless (Assistance_dfa_ocaml_ending.to_ending edg))
      else None
    ) Assistance_dfa_ocaml_ending.all ;;

  let all_mlx_files fw=
   let mods=Assistance_image.image fst (Order.get fw) in
   List.flatten(Assistance_image.image(acolytes_at_module fw) mods);;                
       
 let all_mlx_paths cs=Assistance_image.image Assistance_dfn_full.to_absolute_path (all_mlx_files cs);;  

 let list_values_from_module fw module_name=
 let temp1=all_mlx_paths fw in
 let temp2=Assistance_image.image (fun ap->
  let ttemp1=Assistance_look_for_module_names.list_values_from_module_in_file module_name ap in
  Assistance_set_of_strings.image (fun x->(x,ap) ) ttemp1
  ) temp1 in
 let temp3=List.flatten temp2 in
 let temp4=Assistance_image.image fst temp3 in 
 let temp5=Assistance_ordered.sort Assistance_total_ordering.lex_for_strings temp4 in
 Assistance_image.image (
    fun x->(x,Assistance_option.filter_and_unpack(
      fun (y,ap)->if y=x then Some(ap) else None
    ) temp3)
 ) temp5 ;;


let show_value_occurrences fw t=
 let m=String.length(Assistance_dfa_root.connectable_to_subpath (root fw)) in
 let temp1=all_mlx_paths fw in
 let temp2=Assistance_image.image (fun ap->
    let text = Assistance_io.read_whole_file ap in   
    let temp3=Assistance_substring.occurrences_of_in t text in 
    let closeups = Assistance_image.image (fun j->Assistance_cull_string.closeup_around_index 
        text j
    ) temp3 in
    let mname=Assistance_cull_string.cobeginning(m)(Assistance_absolute_path.to_string ap) in
    Assistance_image.image (fun x->mname^":\n"^x ) closeups
 ) temp1 in
 let temp4=List.flatten temp2 in
 let temp5=String.concat "\n\n\n" (""::temp4@[""]) in 
 print_string temp5;; 

let number_of_modules fw = List.length (Order.get fw) ;;

let below fw eless=
  let mods_in_order = Order.get fw in 
  let mn0=Assistance_dfn_endingless.to_module eless  in
  Assistance_option.filter_and_unpack(fun (mn,_)->
    if List.mem mn0 (snd(List.assoc mn mods_in_order))
    then Some(mn)
    else None) mods_in_order;;

let below_several fw mods = 
  let all_mods_in_order = Assistance_image.image fst (Order.get fw) in 
  let below_module = (fun mn->below fw (endingless_at_module fw mn)) in 
  let temp1 = List.flatten(mods :: (Assistance_image.image below_module mods)) in
  let all_deps = List.filter (fun mn->List.mem mn temp1) all_mods_in_order in 
  let (mods_in_order,new_deps) = List.partition (fun mn->List.mem mn mods) all_deps in 
  (all_deps,new_deps,mods_in_order) ;;

let decipher_path fw x=Assistance_find_suitable_ending.find_file_location 
  (root fw) (All_subdirectories.get fw) x;;

let decipher_module fw capitalized_or_not_x=
  let x=String.uncapitalize_ascii capitalized_or_not_x in 
  let s=Assistance_cull_string.before_rightmost_possibly_all x '.' in
  match (Assistance_option.find_and_stop(
      fun edg->
      let t=s^(Assistance_dfa_ending.connectable_to_modulename edg) in 
      try(Some(decipher_path fw t)) with _->None
  ) Assistance_dfa_ending.all_ocaml_endings) with
  None->raise(Absent_module(x))
  |Some(ap)->
    let rootless_path = Assistance_dfn_common.decompose_absolute_path_using_root ap (root fw) in 
    let mlx = Assistance_dfn_join.root_to_rootless (root fw) rootless_path in 
    Assistance_dfn_full.to_endingless mlx ;;
  
  let above fw mn = snd (List.assoc mn (Order.get fw));;

  let below fw mn0 =
        let ordered_data = Order.get fw in 
        Assistance_option.filter_and_unpack(fun (mn,_)->
            let ancestors_for_mn = snd (List.assoc mn ordered_data) in 
            if List.mem mn0 ancestors_for_mn
            then Some(mn)
            else None) ordered_data;;  
   
  let directly_below fw mn0 =
    let ordered_data = Order.get fw in 
    Assistance_option.filter_and_unpack(fun (mn,_)->
      let fathers_for_mn = fst (List.assoc mn ordered_data) in 
        if List.mem mn0 fathers_for_mn
        then Some(mn)
        else None) ordered_data;;  


  let modules_using_value fw value_name =
    Assistance_option.filter_and_unpack (fun (mn,_)->
      let eless=endingless_at_module fw mn
      and pr_end=Assistance_fw_module_small_details.principal_ending (details_for_module fw mn) in
      let mlx=Assistance_dfn_join.to_ending eless (Assistance_dfa_ocaml_ending.to_ending pr_end) in
      let ap=Assistance_dfn_full.to_absolute_path mlx in
      if Assistance_substring.is_a_substring_of 
             value_name (Assistance_io.read_whole_file ap)
      then Some eless
      else None ) (Order.get fw);;
          
  
  let find_subdir_from_suffix fw possibly_slashed_suffix =
    let suffix = Assistance_cull_string.trim_slashes_on_the_right possibly_slashed_suffix  in
    let temp1 = List.filter (
    fun subdir -> Assistance_supstring.contains (Assistance_dfa_subdirectory.without_trailing_slash subdir) suffix
    ) (All_subdirectories.get fw) in 
    let test_for_minimality = (fun subdir1->
     List.for_all (fun subdir2 ->
        if subdir2 = subdir1 then true else 
        not(Assistance_dfa_subdirectory.begins_with subdir1 subdir2) 
     ) temp1
    ) in 
    let temp2 = List.filter test_for_minimality temp1 in 
    if List.length(temp2)<>1
    then raise(Find_subdir_from_suffix_exn(suffix,temp2))
    else let (Assistance_dfa_subdirectory_t.SD container) = List.hd temp2 in 
         let j1 = Assistance_substring.leftmost_index_of_in suffix container in 
         let j2 = j1 + (String.length suffix) -1 in 
        Assistance_dfa_subdirectory.of_line(Assistance_cull_string.beginning j2 container);;

    let duplicate_module fw old_t1 old_t2=
        let t1=String.uncapitalize_ascii old_t1
        and t2=String.uncapitalize_ascii old_t2 in 
        let ap1=decipher_path fw t1 in
        let s_ap1=Assistance_absolute_path.to_string ap1 in
        let s_ending = Assistance_cull_string.after_rightmost s_ap1 '.' in 
        let s_ap2=(Assistance_cull_string.before_rightmost_possibly_all s_ap1 '/')^"/"^t2^"."^s_ending in
        if Sys.file_exists s_ap2
        then raise(Duplicate_module_already_exists(t2))
        else 
        let _=Assistance_unix_command.uc ("cp "^s_ap1^" "^s_ap2) in
        let ap2=Assistance_absolute_path.of_string s_ap2 in
        let _ =  (
          if s_ending = "ml"
          then Assistance_put_use_directive_in_initial_comment.put_usual (root fw) ap2) in 
        Assistance_unix_command.uc ("open -a \"/Applications/Visual Studio Code.app\" "^s_ap2);;      

    let all_ml_absolute_paths fw =  
        Assistance_option.filter_and_unpack (fun (mn,_)->
          if not(check_ending_in_at_module Assistance_dfa_ocaml_ending_t.Ml fw mn)
          then None
          else 
          let hm=endingless_at_module fw mn in
          let mlx=Assistance_dfn_join.to_ending hm Assistance_dfa_ending.ml in
          Some(Assistance_dfn_full.to_absolute_path mlx)
        ) (Order.get fw);;

    let all_mlx_files fw=
        let mods=Assistance_image.image fst (Order.get fw) in
        List.flatten(Assistance_image.image(acolytes_at_module fw) mods);;    

    let all_endinglesses fw=
        Assistance_image.image (fun (mn,_)->endingless_at_module fw mn) (Order.get fw);;    
        
    
let test_for_foreign root ap =
  match (
    try Some(Assistance_dfn_common.decompose_absolute_path_using_root ap root) with 
             _->None 
  ) with 
  None -> true 
  |Some(rootless) ->
     (
      not(List.mem
         (Assistance_dfn_rootless.to_ending rootless) Assistance_dfa_ending.endings_for_readable_files)   
     )
     ;;


let check_module_sequence_for_forgettability fw l=
 let modules_below = Assistance_option.filter_and_unpack (
   fun (mn,(_,ancestors_for_mn)) -> 
    if List.exists (fun mn2->
       List.mem mn2 ancestors_for_mn
     ) l 
    then Some mn 
    else None 
 )(Order.get fw) in 
 List.filter (fun mn->not(List.mem mn l)) modules_below;;      

end ;;

let above = Private.above ;;
let acolytes_at_module = Private.acolytes_at_module ;;
let all_endinglesses = Private.all_endinglesses ;;
let all_ml_absolute_paths = Private.all_ml_absolute_paths ;;
let all_mlx_files = Private.all_mlx_files ;;
let all_subdirectories fw = Private.All_subdirectories.get fw;;
let ancestors_for_module fw mn = snd (List.assoc mn (Private.Order.get fw)) ;;
let below = Private.below ;;
let below_several = Private.below_several ;;
let check_ending_on_module = Private.check_ending_on_module ;;
let check_module_sequence_for_forgettability = Private.check_module_sequence_for_forgettability ;;
let decipher_module = Private.decipher_module ;;
let decipher_path = Private.decipher_path ;;
let dep_ordered_modules fw = Assistance_image.image fst (Private.Order.get fw);;
let directly_below = Private.directly_below ;;
let direct_fathers_for_module fw mn = fst (List.assoc mn (Private.Order.get fw)) ;;
let duplicate_module = Private.duplicate_module ;;
let endingless_at_module = Private.endingless_at_module ;;
let find_subdir_from_suffix = Private.find_subdir_from_suffix ;;
let forget_modules = Private.Exit.forget_modules ;;
let inspect_and_update = Private.Exit.inspect_and_update ;;
let list_values_from_module = Private.list_values_from_module ;; 
let modules_using_value = Private.modules_using_value ;;
let modules_with_their_ancestors = Private.modules_with_their_ancestors ;;
let needed_libs_for_module fw mn = List.assoc mn (Private.Needed_libs.get fw) ;;
let number_of_modules = Private.number_of_modules ;;
let of_concrete_object crobj = 
    let instance_idx = Assistance_fw_indexer.create_new_instance () in   
    Assistance_fw_poly.extend_fw_with_small_details_to_fw_with_dependencies 
      (Assistance_fw_poly.of_concrete_object crobj) 
      ~index_for_caching:(Private.expand_index instance_idx) ;;
let of_configuration = Private.Exit.of_configuration ;;
let of_configuration_and_list = Private.Exit.of_configuration_and_list ;;
let overwrite_file_if_it_exists = Private.Exit.overwrite_file_if_it_exists ;;
let plunge_fw_configuration = Private.Exit.plunge_fw_configuration ;;
let principal_ending_for_module fw mn = Assistance_fw_module_small_details.principal_ending (Private.details_for_module fw mn) ;;
let printer_equipped_types fw = Private.All_printables.get fw;;
let register_rootless_paths = Private.Exit.register_rootless_paths ;;
let relocate_module_to = Private.Exit.relocate_module_to ;;
let remove_files = Private.Exit.remove_files ;;
let rename_module_on_filename_level_and_in_files = Private.Exit.rename_module_on_filename_level_and_in_files ;;
let rename_subdirectory_as = Private.Exit.rename_subdirectory_as ;;
let replace_string = Private.Exit.replace_string ;;
let replace_value = Private.Exit.replace_value ;;
let root fw = Assistance_fw_poly.root (Private.parent fw) ;;
let show_value_occurrences = Private.show_value_occurrences ;;
let subdir_for_module fw mn = Assistance_fw_module_small_details.subdirectory (Private.details_for_module fw mn) ;;
let to_concrete_object fw = Assistance_fw_poly.to_concrete_object (Private.parent fw) ;;
let usual_compilable_files fw = Assistance_fw_with_small_details.usual_compilable_files (Private.parent fw) ;;


end;;






module Assistance_compilation_mode_t=struct

(* 

#use"Compilation_management/compilation_mode_t.ml";;

*)


type t=
   Usual
  |Debug
  |Executable;;

   

           

end;;






module Assistance_compilation_mode=struct

(* 

#use"Compilation_management/compilation_mode.ml";;

*)

exception Ending_for_last_module_exn ;; 
exception Ending_for_nonlast_module_exn ;; 

let workspace = function 
   Assistance_compilation_mode_t.Usual->Assistance_coma_constant.usual_build_subdir
                     |Debug->Assistance_coma_constant.debug_build_subdir
                     |Executable->Assistance_coma_constant.exec_build_subdir;;

let ending_for_last_module = function 
   Assistance_compilation_mode_t.Usual-> raise(Ending_for_last_module_exn)
                     |Debug->".cmo"
                     |Executable->".ml";;

let ending_for_nonlast_module = function 
   Assistance_compilation_mode_t.Usual-> raise(Ending_for_nonlast_module_exn)
                     |Debug->".cmo"
                     |Executable->".cmx";;                     

let executioner = function 
   Assistance_compilation_mode_t.Usual->"ocamlc -bin-annot "
                     |Debug->"ocamlc -g "
                     |Executable->"ocamlopt ";;

let ending_for_final_product = function 
   Assistance_compilation_mode_t.Usual->""
                     |Debug->".ocaml_debuggable "
                     |Executable->".ocaml_executable ";;   
           

end;;






module Assistance_commands_for_batch_compilation=struct

(* 

#use "Compilation_management/commands_for_batch_compilation.ml";;

*)


module Private = struct

  let needed_dirs_and_libs_in_command cmod fw mn=
    let extension=(if cmod=Assistance_compilation_mode_t.Executable then ".cmxa" else ".cma") in
    let s_root=Assistance_dfa_root.connectable_to_subpath(Assistance_fw_with_dependencies.root fw) in
    let dirs=
    "-I "^s_root^(Assistance_dfa_subdirectory.connectable_to_subpath(Assistance_compilation_mode.workspace cmod))
   and libs=String.concat(" ")
     (Assistance_image.image(fun z->Assistance_ocaml_library.file_for_library(z)^extension)
     (Assistance_fw_with_dependencies.needed_libs_for_module fw mn)) in
     String.concat " " ["";dirs;libs;""];;
  
  
  let command_for_cmi (cmod:Assistance_compilation_mode_t.t) dir fw hm=
      let nm=Assistance_dfn_endingless.to_module hm in
      let s_root=Assistance_dfa_root.connectable_to_subpath(dir) in
      let s_fhm=Assistance_dfn_endingless.to_line hm in
      let mli_reg=Assistance_fw_with_dependencies.check_ending_on_module fw Assistance_dfa_ocaml_ending_t.Mli  nm in
      let ending=(if mli_reg then ".mli" else ".ml") in
      let workdir = Assistance_dfa_subdirectory.connectable_to_subpath (Assistance_compilation_mode.workspace cmod ) in 
      let opt_exec_move=(if (cmod=Assistance_compilation_mode_t.Executable)&&(not(mli_reg)) 
                         then Some("mv "^s_fhm^".o "^s_root^workdir) 
                         else None) in 
      let central_cmd=
          (Assistance_compilation_mode.executioner cmod)^
          (needed_dirs_and_libs_in_command cmod fw nm)^
              " -c "^s_fhm^ending in
              let full_mli=s_fhm^".mli" in
              let almost_full_answer=(
              if (not mli_reg)
                 &&(Sys.file_exists(full_mli))
              then (* 
                     in this situation the mli file exists but is not registered.
                     So the compilation manager must treat it as though it didn't
                     exist. We temporarily rename it so that ocamlc will ignore it.
                    *)
                    let dummy_mli=s_root^"uvueaoqhkt" in
                    [
                     "mv "^full_mli^" "^dummy_mli;
                     central_cmd;
                     "mv "^s_fhm^".cm* "^s_root^workdir;
                     "mv "^dummy_mli^" "^full_mli
                    ] 
              else  [
                       central_cmd;
                       "mv "^s_fhm^".cm* "^s_root^workdir
                     ]
              ) in 
              Assistance_option.add_element_on_the_right almost_full_answer opt_exec_move;;
     
    let hack_to_ignore_present_but_unregistered_mli s_root full_mli central_cmds =
       (* 
          in this situation the mli file exists but is not registered.
          So the compilation manager must treat it as though it didn't
          exist. We temporarily rename it so that ocamlc will ignore it.
        *)
        let dummy_mli=s_root^"uvueaoqhkt" in
        [
          "mv "^full_mli^" "^dummy_mli
        ]
        @ 
          central_cmds
        @ 
        [ 
          "mv "^dummy_mli^" "^full_mli
        ] ;;



    let command_for_cmo_from_mll (cmod:Assistance_compilation_mode_t.t) dir fw eless=
      let nm=Assistance_dfn_endingless.to_module eless in
      let s_root=Assistance_dfa_root.connectable_to_subpath(dir) in
      let s_eless=Assistance_dfn_endingless.to_line eless in
      let dir_and_libs=needed_dirs_and_libs_in_command cmod fw nm in
      let mli_reg=Assistance_fw_with_dependencies.check_ending_on_module fw Assistance_dfa_ocaml_ending_t.Mli nm in 
      let full_mli=s_eless^".mli" in
      let workdir = Assistance_dfa_subdirectory.connectable_to_subpath (Assistance_compilation_mode.workspace cmod ) in 
      let opt_exec_move=(if cmod=Assistance_compilation_mode_t.Executable 
                         then Some("mv "^s_eless^".o "^s_root^workdir) 
                         else None) in 
      let ml_in_workplace = s_root^workdir ^ (Assistance_dfa_module.to_line nm) ^ ".ml" in                   
      let central_cmds=
      [ 
        "ocamllex "^s_eless^".mll";
        "mv "^s_eless^".ml "^s_root^workdir;
        (Assistance_compilation_mode.executioner cmod)^dir_and_libs^" -c "^ml_in_workplace;
      ] in 
      let almost_full_answer= 
      (if (not mli_reg) &&(Sys.file_exists(full_mli))
      then hack_to_ignore_present_but_unregistered_mli s_root full_mli central_cmds 
      else central_cmds)
      in Assistance_option.add_element_on_the_right almost_full_answer opt_exec_move;; 

    let command_for_cmo_from_mly (cmod:Assistance_compilation_mode_t.t) dir fw eless=
      let nm=Assistance_dfn_endingless.to_module eless in
      let s_root=Assistance_dfa_root.connectable_to_subpath(dir) in
      let s_eless=Assistance_dfn_endingless.to_line eless in
      let dir_and_libs=needed_dirs_and_libs_in_command cmod fw nm in
      let mli_reg=Assistance_fw_with_dependencies.check_ending_on_module fw Assistance_dfa_ocaml_ending_t.Mli nm in 
      let full_mli=s_eless^".mli" in
      let workdir = Assistance_dfa_subdirectory.connectable_to_subpath (Assistance_compilation_mode.workspace cmod ) in 
      let opt_exec_move=(if cmod=Assistance_compilation_mode_t.Executable 
                         then Some("mv "^s_eless^".o "^s_root^workdir) 
                         else None) in 
      let ml_in_workplace = s_root^workdir ^ (Assistance_dfa_module.to_line nm) ^ ".ml" in                   
      let central_cmds=
      [ 
        "ocamlyacc "^s_eless^".mly";
        "mv "^s_eless^".ml "^s_root^workdir;
        (Assistance_compilation_mode.executioner cmod)^dir_and_libs^" -c "^ml_in_workplace;
      ] in 
      let almost_full_answer= 
      (if (not mli_reg) &&(Sys.file_exists(full_mli))
      then hack_to_ignore_present_but_unregistered_mli s_root full_mli central_cmds 
      else central_cmds)
      in Assistance_option.add_element_on_the_right almost_full_answer opt_exec_move;; 



    let command_for_cmo (cmod:Assistance_compilation_mode_t.t) dir fw eless=
      let nm=Assistance_dfn_endingless.to_module eless in
      let s_root=Assistance_dfa_root.connectable_to_subpath(dir) in
      let s_eless=Assistance_dfn_endingless.to_line eless in
      let dir_and_libs=needed_dirs_and_libs_in_command cmod fw nm in
      let mli_reg=Assistance_fw_with_dependencies.check_ending_on_module fw Assistance_dfa_ocaml_ending_t.Mli nm in 
      let full_mli=s_eless^".mli" in
      let workdir = Assistance_dfa_subdirectory.connectable_to_subpath (Assistance_compilation_mode.workspace cmod ) in 
      let opt_exec_move=(if cmod=Assistance_compilation_mode_t.Executable 
                         then Some("mv "^s_eless^".o "^s_root^workdir) 
                         else None) in 
      let central_cmds=
      [ 
        (Assistance_compilation_mode.executioner cmod)^dir_and_libs^" -c "^s_eless^".ml";
        "mv "^s_eless^".cm* "^s_root^workdir
      ] in 
      let almost_full_answer= 
      (if (not mli_reg) &&(Sys.file_exists(full_mli))
      then hack_to_ignore_present_but_unregistered_mli s_root full_mli central_cmds
      else central_cmds)
      in Assistance_option.add_element_on_the_right almost_full_answer opt_exec_move;; 


  


  let command_for_mli_module_separate_compilation cmod fw eless =
      let dir = Assistance_fw_with_dependencies.root fw in 
      command_for_cmi cmod dir fw eless;;

  let command_for_ml_module_separate_compilation cmod fw eless =
      let dir = Assistance_fw_with_dependencies.root fw in 
      let nm=Assistance_dfn_endingless.to_module eless in
      let mli_reg=Assistance_fw_with_dependencies.check_ending_on_module fw Assistance_dfa_ocaml_ending_t.Mli nm in
      let temp2=(
      let co=command_for_cmo cmod dir fw eless in 
      if mli_reg
      then let ci=command_for_cmi cmod dir fw eless in 
           [ci;co]
      else [co]) in 
      List.flatten temp2;;

  let command_for_mll_module_separate_compilation cmod fw eless =
      let dir = Assistance_fw_with_dependencies.root fw in 
      let nm=Assistance_dfn_endingless.to_module eless in
      let mli_reg=Assistance_fw_with_dependencies.check_ending_on_module fw Assistance_dfa_ocaml_ending_t.Mli nm in
      let temp2=(
      let co=command_for_cmo_from_mll cmod dir fw eless in 
      if mli_reg
      then let ci=command_for_cmi cmod dir fw eless in 
           [ci;co]
      else [co]) in 
      List.flatten temp2;;

  let command_for_mly_module_separate_compilation cmod fw eless =
      let dir = Assistance_fw_with_dependencies.root fw in 
      let nm=Assistance_dfn_endingless.to_module eless in
      let mli_reg=Assistance_fw_with_dependencies.check_ending_on_module fw Assistance_dfa_ocaml_ending_t.Mli nm in
      let temp2=(
      let co=command_for_cmo_from_mly cmod dir fw eless in 
      if mli_reg
      then let ci=command_for_cmi cmod dir fw eless in 
           [ci;co]
      else [co]) in 
      List.flatten temp2;;

  let command_for_module_separate_compilation cmod fw eless pr_ending = 
     match pr_ending with 
      Assistance_dfa_ocaml_ending_t.Mli -> command_for_mli_module_separate_compilation cmod fw eless
     |Ml -> command_for_ml_module_separate_compilation cmod fw eless
     |Mll -> command_for_mll_module_separate_compilation cmod fw eless
     |Mly -> command_for_mly_module_separate_compilation cmod fw eless
    ;;


  
  exception  Command_for_predebuggable_or_preexecutable_exn;;
  
  let command_for_predebuggable fw short_path=
      let root = Assistance_fw_with_dependencies.root fw in 
      let s_root=Assistance_dfa_root.connectable_to_subpath root  in
      let cmod = Assistance_compilation_mode_t.Debug in 
      let full_path=Assistance_absolute_path.of_string(
          s_root^short_path) in 
      let nm_direct_deps = Assistance_look_for_module_names.names_in_mlx_file full_path in 
      let nm_deps = Assistance_fw_with_dependencies.modules_with_their_ancestors fw nm_direct_deps in 
      let nm_deps_with_subdirs = Assistance_image.image (
         fun nm->
                 let subdir=Assistance_fw_with_dependencies.subdir_for_module fw nm in 
          (subdir,nm)
      ) nm_deps in 
      let workdir=
        (Assistance_dfa_subdirectory.connectable_to_subpath (Assistance_compilation_mode.workspace cmod)) in
      let unpointed_short_path = Assistance_cull_string.before_rightmost short_path '.' in 
      let libs_for_prow = 
        Assistance_set_of_polys.sort(
        Assistance_ocaml_library.compute_needed_libraries_from_uncapitalized_modules_list
          (Assistance_image.image Assistance_dfa_module.to_line nm_direct_deps)) in 
      let pre_libs1=Assistance_image.image 
       (fun (_,nm) -> Assistance_set_of_polys.sort(Assistance_fw_with_dependencies.needed_libs_for_module fw nm)) nm_deps_with_subdirs in
      let pre_libs2=Assistance_set_of_polys.forget_order (Assistance_set_of_polys.fold_merge (libs_for_prow::pre_libs1)) in 
      let extension=".cma" in
      let libs=String.concat(" ")
        (Assistance_image.image(fun z->Assistance_ocaml_library.file_for_library(z)^extension) pre_libs2) in 
      Assistance_option.add_element_on_the_right   
      [ 
        (Assistance_compilation_mode.executioner cmod)^
        " -I "^s_root^workdir^" "^
        libs^" -c "^s_root^unpointed_short_path^".ml";
      ] 
      (Assistance_unix_command.mv (s_root^unpointed_short_path^".cm*") (s_root^workdir) )
      ;;          
  
  
  
  
  exception  Command_for_debuggable_or_executable_exn;;
  
  let command_for_debuggable_or_executable cmod fw rootless_path=
    let root = Assistance_fw_with_dependencies.root fw in 
    let s_root=Assistance_dfa_root.connectable_to_subpath root  in
      if cmod=Assistance_compilation_mode_t.Usual then raise(Command_for_debuggable_or_executable_exn) else 
      let full_path=Assistance_absolute_path.of_string(s_root^rootless_path) in 
      let nm_direct_deps = Assistance_look_for_module_names.names_in_mlx_file full_path in 
      let nm_deps =Assistance_fw_with_dependencies.modules_with_their_ancestors fw nm_direct_deps in 
      let nm_deps_with_subdirs = Assistance_image.image (
         fun nm->let subdir=Assistance_fw_with_dependencies.subdir_for_module fw nm in 
          (subdir,nm)
      ) nm_deps in 
      let workdir=
        (Assistance_dfa_subdirectory.connectable_to_subpath (Assistance_compilation_mode.workspace cmod)) 
      and ending=Assistance_compilation_mode.ending_for_nonlast_module cmod 
      and last_ending=Assistance_compilation_mode.ending_for_last_module cmod 
      and product_ending=Assistance_compilation_mode.ending_for_final_product cmod  in
      let cm_elements_but_the_last = Assistance_image.image (
        fun (subdir,nm)->(Assistance_dfa_module.to_line nm)^ending
      ) nm_deps_with_subdirs in 
      let unpointed_short_path = Assistance_cull_string.before_rightmost rootless_path '.' in 
      let nm_name = (Assistance_cull_string.after_rightmost unpointed_short_path '/') in 
      let last_cm_element=nm_name^last_ending in 
      let all_cm_elements= cm_elements_but_the_last @ [last_cm_element] in 
      let libs_for_prow = 
        Assistance_set_of_polys.sort(
        Assistance_ocaml_library.compute_needed_libraries_from_uncapitalized_modules_list
          (Assistance_image.image Assistance_dfa_module.to_line nm_direct_deps)) in 
      let pre_libs1=Assistance_image.image 
       (fun (_,nm) -> Assistance_set_of_polys.sort(Assistance_fw_with_dependencies.needed_libs_for_module fw nm)) nm_deps_with_subdirs in
      let pre_libs2=Assistance_set_of_polys.forget_order (Assistance_set_of_polys.fold_merge (libs_for_prow::pre_libs1)) in 
      let extension=(if cmod=Assistance_compilation_mode_t.Executable then ".cmxa" else ".cma") in
      let libs=String.concat(" ")
        (Assistance_image.image(fun z->Assistance_ocaml_library.file_for_library(z)^extension) pre_libs2) in 
      Assistance_option.add_element_on_the_right  
      [ 
        ((Assistance_compilation_mode.executioner cmod)^
         " -I "^s_root^workdir^" "^
         libs^" -o "^nm_name^product_ending^
          (String.concat " " all_cm_elements));
      ]
      (
        Assistance_unix_command.mv ((Sys.getcwd())^"/"^nm_name^product_ending) (s_root^workdir)
      )
      ;;          
  
  end ;; 
  
let module_separate_compilation = Private.command_for_module_separate_compilation ;;
     
let predebuggable = Private.command_for_predebuggable ;; 
     
let debuggable_or_executable  = Private.command_for_debuggable_or_executable ;;   
  
  

end;;






module Assistance_no_slashes=struct

(*

#use"no_slashes.ml";;

*)

type t=NS of string;;

exception Slash_at of string*int;;

let to_string(NS s)=s;;

let of_string s=
  let n=String.length s in
  let rec tempf=(fun i->
  if i>n
  then NS s
  else if (String.get s (i-1))='/'
       then raise(Slash_at(s,i))
       else tempf(i+1)
  ) in
  tempf 1;;
  
           

end;;






module Assistance_fw_with_batch_compilation=struct

(* 

#use"Filewatching/fw_with_batch_compilation.ml";;

*)

   
exception Rename_string_or_value_exn of string ;;

module Private = struct 
  
  let ocamldebug_printersfile_path root= 
             (Assistance_dfa_root.connectable_to_subpath root)^
             (Assistance_dfa_subdirectory.connectable_to_subpath
               (Assistance_coma_constant.nongithubbed_nonml_files_subdir)) ^
               "cmos_for_ocamldebug.txt";;
  let parent fw = Assistance_fw_poly.parent fw;;
  let get_cmpl_results fw = Assistance_fw_poly.last_compilation_result_for_module fw ;;
  let set_cmpl_results fw new_list = 
    Assistance_fw_poly.set_last_compilation_result_for_module fw new_list ;; 
  let set_parent fw new_parent = 
      Assistance_fw_poly.set_parent ~child:fw ~new_parent:new_parent ;;
  let usual_extension fw cmpl_results =
      Assistance_fw_poly.extend_fw_with_dependencies_to_fw_with_batch_compilation    
       fw ~last_compilation_result_for_module:cmpl_results ;;
     
  let last_compilation_result_for_module fw mn = 
    List.assoc mn (get_cmpl_results fw) ;;

  let modules_with_their_ancestors fw l=
    Assistance_fw_with_dependencies.modules_with_their_ancestors
     (parent fw) l ;;
  let root fw = Assistance_fw_poly.root (parent fw) ;;   
  let set_cmpl_result_at_module fw mn0 new_res = 
    let old_list_of_cmpl_results= get_cmpl_results fw in 
    let new_list_of_cmpl_results = Assistance_image.image (
      fun old_pair ->
        let mn = fst old_pair in  
        if mn = mn0 
        then (mn,new_res)
        else old_pair  
    ) old_list_of_cmpl_results in 
    set_cmpl_results fw new_list_of_cmpl_results ;;
  
  let preq_types_with_extra_info fw =
      let root = root fw  in 
      Assistance_image.image (fun middle->
       let mn = Assistance_dfn_middle.to_module middle in 
       (Assistance_dfn_join.root_to_middle root middle,last_compilation_result_for_module fw mn)
      ) (Assistance_fw_with_dependencies.printer_equipped_types fw) ;;  
  
  
  module Command = struct 
  
      let module_separate_compilation cmod fw eless pr_ending=
       Assistance_commands_for_batch_compilation.module_separate_compilation 
         cmod (parent fw) eless pr_ending;;
      
      let predebuggable fw short_path =
        Assistance_commands_for_batch_compilation.predebuggable 
          (parent fw) short_path ;; 
      
      let debuggable_or_executable cmod fw rootless_path =
        Assistance_commands_for_batch_compilation.debuggable_or_executable
          cmod (parent fw) rootless_path ;; 
  end;;
  
  
  module Ocaml_target_making=struct
  
  exception Failed_during_compilation of (Assistance_dfa_module_t.t*Assistance_dfn_endingless_t.t*string);;
  
  let rec helper_for_feydeau  (cmod:Assistance_compilation_mode_t.t) fw (rejected,treated,to_be_treated)=
       match to_be_treated with 
       []->(fw,rejected,List.rev treated)
       |triple::other_triples->
         let (nm,eless,cmd)=triple in
         if (Assistance_unix_command.uc cmd)=0
         then let fw2= set_cmpl_result_at_module fw nm true in 
              helper_for_feydeau cmod fw2 (rejected,(nm,eless)::treated,other_triples)
         else if (cmod<>Assistance_compilation_mode_t.Usual)
              then raise(Failed_during_compilation(triple))
              else 
              let triples_after=snd(Assistance_hurried.partition_in_two_parts (fun (nm2,_,_)->nm2<>nm) other_triples) in 
              let (rejected_siblings_as_triples,survivors)=List.partition
             (
                fun (nm2,_,_)->
                  List.mem nm (Assistance_fw_with_dependencies.ancestors_for_module fw nm2)
             ) triples_after in 
             let rejected_siblings_with_redundancies =  
                Assistance_image.image (fun (nm2,eless2,_)->(nm2,eless2) ) rejected_siblings_as_triples in 
             let rejected_siblings = Assistance_listennou.nonredundant_version rejected_siblings_with_redundancies in    
             let newly_rejected = (nm,eless)::rejected_siblings in 
             let newly_rejected_mods = Assistance_image.image fst newly_rejected in 
             let old_list_of_cmpl_results= get_cmpl_results fw in 
             let new_list_of_cmpl_results = Assistance_image.image (
                fun old_pair ->
                  let mn = fst old_pair in  
                  if List.mem mn newly_rejected_mods
                  then (mn,false)
                  else old_pair  
             ) old_list_of_cmpl_results in 
             let fw2 = set_cmpl_results fw new_list_of_cmpl_results in 
             helper_for_feydeau cmod fw2 (rejected@newly_rejected,treated,survivors) ;;
           
  
  let prepare_pretty_printers_for_ocamldebug fw deps = 
    let temp1 = "load_printer str.cma"::(Assistance_image.image (fun mname->
      let s= Assistance_dfa_module.to_line mname in 
      "load_printer "^s^".cmo"
    ) deps) 
    and printer_equipped_types = preq_types_with_extra_info fw  in 
    let printable_deps = List.filter (
      fun mn -> let eless = Assistance_fw_with_dependencies.endingless_at_module fw mn in 
      List.mem (eless,true) printer_equipped_types
    ) deps in 
    let temp2 = Assistance_image.image (fun mname->
      let s= Assistance_dfa_module.to_line mname in 
      "install_printer "^(String.capitalize_ascii s)^".print_out"
    ) printable_deps in 
    let full_text = String.concat "\n" (temp1@temp2) in 
    let ppodbg_path = ocamldebug_printersfile_path (root fw) in 
    Assistance_io.overwrite_with (Assistance_absolute_path.of_string ppodbg_path) full_text;;
  
  let dependencies_inside_shaft cmod fw (opt_modnames,opt_rootless_path)=
     match cmod with 
     Assistance_compilation_mode_t.Usual->Assistance_option.unpack opt_modnames
     |_->let rootless_path=Assistance_option.unpack opt_rootless_path in 
         let full_path=Assistance_absolute_path.of_string(
          (Assistance_dfa_root.connectable_to_subpath (root fw))^rootless_path) in 
         let nm_direct_deps = Assistance_look_for_module_names.names_in_mlx_file full_path in 
         let nm_deps=modules_with_their_ancestors fw nm_direct_deps in 
         let deps =List.filter (fun mn->List.mem mn nm_deps) (Assistance_fw_with_dependencies.dep_ordered_modules fw) in 
         let _=(if cmod = Assistance_compilation_mode_t.Debug 
                then prepare_pretty_printers_for_ocamldebug fw deps) in 
         deps;;
  
  let list_of_commands_for_shaft_part_of_feydeau cmod fw (opt_modulenames,opt_rootless_path)=
     let l=dependencies_inside_shaft cmod fw (opt_modulenames,opt_rootless_path) in 
     let temp1=Assistance_image.image (fun mn->
       let eless=Assistance_fw_with_dependencies.endingless_at_module fw mn 
       and pr_ending = Assistance_fw_with_dependencies.principal_ending_for_module fw mn in 
       let cmds=Command.module_separate_compilation cmod fw eless pr_ending in 
      Assistance_image.image (fun cmd->(mn,Assistance_fw_with_dependencies.endingless_at_module fw mn,cmd) ) cmds ) l in 
      List.flatten temp1;;
  
  let list_of_commands_for_connecting_part_of_feydeau cmod fw (_,opt_rootless_path)=
     let cmds=(
     match cmod with 
      Assistance_compilation_mode_t.Usual
     |Assistance_compilation_mode_t.Executable ->[] 
     |_->
        let rootless_path=Assistance_option.unpack opt_rootless_path in 
        Command.predebuggable fw rootless_path) in 
     cmds;;
  
  
  let list_of_commands_for_end_part_of_feydeau cmod fw (_,opt_rootless_path)= 
     let cmds=(
     match cmod with 
     Assistance_compilation_mode_t.Usual->[] 
     |_->
        let rootless_path=Assistance_option.unpack opt_rootless_path in 
        Command.debuggable_or_executable cmod fw rootless_path) in 
     cmds;;   
  
  let list_of_commands_for_ternary_feydeau cmod fw short_path=
     let pair = (None,Some(short_path)) in 
     let pre_cmds1=list_of_commands_for_shaft_part_of_feydeau cmod fw pair in 
     let cmds1=Assistance_image.image (fun (_,_,cmd)->cmd) pre_cmds1
     and cmds2=list_of_commands_for_connecting_part_of_feydeau cmod fw pair
     and cmds3=list_of_commands_for_end_part_of_feydeau cmod fw pair in 
     cmds1@cmds2@cmds3;;
  
  let shaft_part_of_feydeau cmod fw (opt_modnames,opt_rootless_path)=
    let cmds=list_of_commands_for_shaft_part_of_feydeau cmod fw (opt_modnames,opt_rootless_path) in  
    helper_for_feydeau cmod fw ([],[],cmds);; 
  
  let end_part_of_feydeau cmod fw (opt_modnames,opt_rootless_path)=
    match cmod with 
     Assistance_compilation_mode_t.Usual->()
     |_->
       let all_cmds=
         (list_of_commands_for_connecting_part_of_feydeau cmod fw (opt_modnames,opt_rootless_path))@
         (list_of_commands_for_end_part_of_feydeau cmod fw (opt_modnames,opt_rootless_path)) in 
       let _=Assistance_image.image  Assistance_unix_command.hardcore_uc all_cmds in 
       () ;;
  
  
  
  let feydeau cmod fw (opt_modnames,opt_rootless_path)=
    let answer=shaft_part_of_feydeau cmod fw (opt_modnames,opt_rootless_path) in 
    let _=end_part_of_feydeau cmod fw (opt_modnames,opt_rootless_path) in 
    answer;; 
  
  
  let usual_feydeau fw modnames = feydeau Assistance_compilation_mode_t.Usual fw (Some(modnames),None);;
  
  end;;  
  
  
  
  
  let clean_debug_dir fw=
    let s_root=Assistance_dfa_root.connectable_to_subpath(root fw) in
    let s_debug_dir=s_root^(Assistance_dfa_subdirectory.connectable_to_subpath
       (Assistance_coma_constant.debug_build_subdir)) in 
    Assistance_unix_command.uc("rm -f "^s_debug_dir^"*.cm*"^" "^s_debug_dir^"*.ocaml_debuggable");;
     
  let name_element_for_debugged_file = "debugged" ;;
  let debugged_file_path = 
    (Assistance_dfa_subdirectory.connectable_to_subpath(Assistance_coma_constant.debugging_subdir))
               ^ name_element_for_debugged_file ^ ".ml" ;;  
  
  let start_debugging fw=
    let  _=clean_debug_dir fw in
    let ppodbg_path = ocamldebug_printersfile_path (root fw) in 
    let _= Assistance_io.overwrite_with (Assistance_absolute_path.of_string ppodbg_path) "" in   
    let cmds=Ocaml_target_making.list_of_commands_for_ternary_feydeau Assistance_compilation_mode_t.Debug fw debugged_file_path in 
    let answer=Assistance_unix_command.conditional_multiple_uc cmds in 
    let dbgbuild_path =  Assistance_dfa_subdirectory.connectable_to_subpath(Assistance_coma_constant.debug_build_subdir) in 
    let msg=(
      if answer
      then "\n\n The debugging-friendly executable has been created. \n"^
          "Now, go to "^dbgbuild_path^" and start \n\nocamldebug "^name_element_for_debugged_file^
           ".ocaml_debuggable\n\nin another terminal.\n\n"^
           "If you need to use pretty printers, from inside ocamldebug do \n\n"^ 
           "source "^ppodbg_path^" \n\n"
      else "\n\n Something went wrong, see above. \n\n"
    ) in
    let _=(
      print_string msg;
      flush stdout
    ) in
    answer;;   
     
  let clean_exec_dir fw=
    let s_root=Assistance_dfa_root.connectable_to_subpath(root fw) in
    let s_exec_dir=s_root^(Assistance_dfa_subdirectory.connectable_to_subpath(Assistance_coma_constant.exec_build_subdir)) in 
    Assistance_unix_command.uc("rm -f "^s_exec_dir^"*.cm*"^" "^s_exec_dir^"*.ocaml_executable"^" "^s_exec_dir^"*.o");;
     
  
  let start_executing fw short_path=
    let  _=clean_exec_dir fw in
    let cmds=Ocaml_target_making.list_of_commands_for_ternary_feydeau 
      Assistance_compilation_mode_t.Executable fw short_path in 
    Assistance_unix_command.conditional_multiple_uc cmds;;   
  
  let list_values_from_module fw mn = Assistance_fw_with_dependencies.list_values_from_module (parent fw) mn ;;
  let show_value_occurrences fw mn = Assistance_fw_with_dependencies.show_value_occurrences (parent fw) mn ;;

  let up_to_date_elesses fw =
    Assistance_option.filter_and_unpack (
      fun mn->
        if last_compilation_result_for_module fw mn
        then Some(Assistance_fw_with_dependencies.endingless_at_module fw mn)
        else None
    )(Assistance_fw_with_dependencies.dep_ordered_modules fw);;

  let number_of_modules fw = Assistance_fw_with_dependencies.number_of_modules (parent fw) ;;  
  
  
  let modern_recompile fw changed_modules_in_any_order = 
      if changed_modules_in_any_order=[] then fw else
      let (all_deps,new_deps,changed_modules) = 
        Assistance_fw_with_dependencies.below_several (parent fw) changed_modules_in_any_order in     
      let _ = Assistance_strung.announce 
      ~trailer:("The following modules need to be recompiled \n"^
      "because they depend on directly changed modules :")
         ~printer:Assistance_dfa_module.to_line ~items:new_deps 
         ~separator: ", " in 
      let (fw2,rejected_pairs,accepted_pairs)=
        Ocaml_target_making.usual_feydeau fw all_deps in 
      fw2 ;;

   let forget_modules fw mod_names=
      let (new_parent,removed_files) = Assistance_fw_with_dependencies.forget_modules (parent fw) mod_names in  
     let temp1 = Assistance_image.image Assistance_dfa_module.to_line mod_names in 
     let temp2 = Assistance_cartesian.product temp1 [".cm*";".d.cm*";".caml_debuggable"] in 
     let s_root=Assistance_dfa_root.connectable_to_subpath(root fw) in
     let s_build_dir=s_root^(Assistance_dfa_subdirectory.connectable_to_subpath(Assistance_coma_constant.usual_build_subdir)) in 
     let _=Assistance_image.image
                      (fun (mname,edg)->
                       let cmd="rm -f "^s_build_dir^mname^edg in
                       Assistance_unix_command.uc(cmd))
                      temp2 in
      (set_parent fw new_parent,removed_files);;
   
   let remove_files fw rootless_paths=
      let (new_parent,_)=Assistance_fw_with_dependencies.remove_files (parent fw) rootless_paths in   
      set_parent fw new_parent ;;   
   
   let inspect_and_update fw =
      let (new_parent,((changed_archived_compilables,changed_usual_compilables),_,changed_files))
         =Assistance_fw_with_dependencies.inspect_and_update (parent fw) in   
      (set_parent fw new_parent,(changed_usual_compilables,changed_files));;

   let of_fw_with_dependencies fw = usual_extension fw 
    (Assistance_image.image (
        fun mn -> (mn,false)
    ) (Assistance_fw_with_dependencies.dep_ordered_modules fw));;   

   let of_configuration config =
      let root = Assistance_fw_poly.root config in 
      let _=(Assistance_more_unix.create_subdirs_and_fill_files_if_necessary root
       Assistance_coma_constant.minimal_set_of_needed_dirs 
           Assistance_coma_constant.conventional_files_with_minimal_content) in 
      let initial_parent = Assistance_fw_with_dependencies.of_configuration config in 
      let fw = of_fw_with_dependencies initial_parent in 
      let mods = Assistance_fw_with_dependencies.dep_ordered_modules initial_parent in 
      let (fw2,rejected_pairs,accepted_pairs) = Ocaml_target_making.usual_feydeau fw mods in 
        let cmpl_results = Assistance_image.image (
             fun mn -> (mn,List.exists (fun (mn2,_)->mn2 = mn) accepted_pairs)
           ) mods in 
      set_cmpl_results fw2 cmpl_results ;; 
   
   let register_rootless_paths fw rps=
      let (new_parent,((ac_paths,uc_paths,nc_paths),_))=
       Assistance_fw_with_dependencies.register_rootless_paths (parent fw) rps in   
      let old_list_of_cmpl_results= get_cmpl_results fw in 
     let new_list_of_cmpl_results = Assistance_image.image (
        fun mn -> 
          match List.assoc_opt mn old_list_of_cmpl_results with 
          None -> (mn,false)
          |Some(old_res) -> (mn,old_res)
     ) (Assistance_fw_with_dependencies.dep_ordered_modules new_parent) in 
     let fw2 = usual_extension new_parent new_list_of_cmpl_results in 
     let unordered_mods = Assistance_image.image Assistance_dfn_rootless.to_module uc_paths in  
     modern_recompile  fw2 unordered_mods;;
   
  
   let relocate_module_to fw mod_name new_subdir=
      let (new_parent,replacements)=Assistance_fw_with_dependencies.relocate_module_to (parent fw) (mod_name,new_subdir) in   
      (set_parent fw new_parent,replacements) ;;
   
   let rename_module fw old_middle_name new_nonslashed_name=
     let old_nm=Assistance_dfn_middle.to_module old_middle_name in
     let new_nm=Assistance_dfa_module.of_line (Assistance_no_slashes.to_string new_nonslashed_name) in  
     let old_parent = parent fw in 
     let separated_acolytes_below=Assistance_option.filter_and_unpack(
       fun mn->
        if List.mem old_nm (Assistance_fw_with_dependencies.ancestors_for_module old_parent mn)
       then Some(Assistance_image.image (Assistance_dfn_full.to_rootless) (Assistance_fw_with_dependencies.acolytes_at_module old_parent mn))
       else None
   ) (Assistance_fw_with_dependencies.dep_ordered_modules old_parent) in
     let all_acolytes_below=List.flatten separated_acolytes_below in
     let (new_parent,changes) = Assistance_fw_with_dependencies.rename_module_on_filename_level_and_in_files 
      old_parent (old_nm,new_nm,all_acolytes_below) in 
    let old_list_of_cmpl_results= get_cmpl_results fw in 
    let new_list_of_cmpl_results = Assistance_image.image (
         fun old_pair -> 
           let (mn,cmpl_result) = old_pair in 
           if mn = old_nm 
           then (new_nm,false)
           else old_pair    
      ) old_list_of_cmpl_results in   
      let fw2 = usual_extension new_parent new_list_of_cmpl_results in 
      let root_dir=root fw in 
      let s_root=Assistance_dfa_root.connectable_to_subpath root_dir in   
      let s_build_dir=Assistance_dfa_subdirectory.connectable_to_subpath (Assistance_coma_constant.usual_build_subdir) in  
      let _=Assistance_unix_command.uc
             ("rm -f "^s_root^s_build_dir^
             (Assistance_dfa_module.to_line old_nm)^
             ".cm* ") in            
      let fw3=modern_recompile fw2 [new_nm] in 
      (fw3,changes) ;;
      
   
   let rename_subdirectory_as fw (old_subdir,new_subdir)=
      let (new_parent,extra)=Assistance_fw_with_dependencies.rename_subdirectory_as 
         (parent fw) (old_subdir,new_subdir) in   
         (set_parent fw new_parent,extra) ;;
   
    let replace_string fw old_s new_s =
         let old_parent = parent fw in 
         let (new_parent,(changes1,all_changed_files)) = Assistance_fw_with_dependencies.replace_string old_parent (old_s,new_s) in 
         let changed_rootlesses = Assistance_image.image fst changes1 in 
         let changed_modules_in_any_order = Assistance_image.image Assistance_dfn_rootless.to_module changed_rootlesses in 
         (set_parent fw new_parent,(changed_modules_in_any_order,all_changed_files));; 
    
    
    let replace_value fw ((preceding_files,path),(old_v,new_v)) =
          let old_parent = parent fw in 
          let (new_parent,(u_changes,all_changes)) = Assistance_fw_with_dependencies.replace_value old_parent ((preceding_files,path),(old_v,new_v)) in 
          let changed_rootlesses = Assistance_image.image fst u_changes in 
          let changed_modules_in_any_order = Assistance_image.image Assistance_dfn_rootless.to_module changed_rootlesses in 
          (set_parent fw new_parent,(changed_modules_in_any_order,all_changes));;      


     

  let plunge_fw_configuration config = 
    usual_extension (Assistance_fw_with_dependencies.plunge_fw_configuration config) [];;

  let usual_recompile fw = 
    let (fw1,(changed_uc,changed_files)) = inspect_and_update fw  in 
    let unordered_mods = Assistance_image.image Assistance_dfn_rootless.to_module changed_uc in  
    let fw2 = modern_recompile fw1 unordered_mods  in 
    (fw2,(changed_uc,changed_files)) ;;   

  

  end ;;
  

let clean_debug_dir = Private.clean_debug_dir;;
let clean_exec_dir = Private.clean_exec_dir;;
let forget_modules = Private.forget_modules ;;
let list_values_from_module = Private.list_values_from_module ;;
let modern_recompile = Private.modern_recompile ;;
let number_of_modules = Private.number_of_modules ;;
let of_configuration = Private.of_configuration ;;
let of_fw_with_dependencies = Private.of_fw_with_dependencies ;;
let plunge_fw_configuration = Private.plunge_fw_configuration ;;
let preq_types_with_extra_info = Private.preq_types_with_extra_info ;;
let register_rootless_paths = Private.register_rootless_paths ;;
let relocate_module_to = Private.relocate_module_to ;;
let remove_files = Private.remove_files ;;
let rename_module = Private.rename_module ;;
let rename_subdirectory_as = Private.rename_subdirectory_as ;;
let replace_string = Private.replace_string ;;
let replace_value = Private.replace_value ;;
let root = Private.root ;;
let show_value_occurrences = Private.show_value_occurrences ;;
let start_debugging = Private.start_debugging;;
let start_executing = Private.start_executing ;;
let up_to_date_elesses = Private.up_to_date_elesses ;;
let usual_batch = Private.Ocaml_target_making.usual_feydeau ;; 
let usual_recompile = Private.usual_recompile ;;  
  
  
  

end;;






module Assistance_fw_with_githubbing=struct

(* 

#use"Filewatching/fw_with_githubbing.ml";;

*)


module Private = struct

  let parent = Assistance_fw_poly.parent ;; 
  
  let set_parent = Assistance_fw_poly.set_parent ;;
  
  
  let usual_batch fw modnames = 
    let (new_parent,rejected_ones,accepted_ones) = Assistance_fw_with_batch_compilation.usual_batch (parent fw) modnames in 
    (set_parent fw new_parent,rejected_ones,accepted_ones) ;; 
  
  let usual_extension fw_batch backup_dir gab git_url enc_files = 
      Assistance_fw_poly.extend_fw_with_batch_compilation_to_fw_with_githubbing 
      fw_batch
      ~dir_for_backup:backup_dir 
      ~gitpush_after_backup:gab
      ~github_url:git_url
      ~encoding_protected_files:enc_files ;;

  
  let of_fw_config_and_github_config fw_config github_config = usual_extension 
    (Assistance_fw_with_batch_compilation.of_configuration fw_config)
    (Assistance_fw_poly.dir_for_backup github_config) 
    (Assistance_fw_poly.gitpush_after_backup github_config) 
    (Assistance_fw_poly.github_url github_config)
    (Assistance_fw_poly.encoding_protected_files github_config);;


  let plunge_fw_config_with_github_config fw_config github_config= usual_extension 
      (Assistance_fw_with_batch_compilation.plunge_fw_configuration fw_config)
      (Assistance_fw_poly.dir_for_backup github_config) 
      (Assistance_fw_poly.gitpush_after_backup github_config) 
      (Assistance_fw_poly.github_url github_config)
      (Assistance_fw_poly.encoding_protected_files github_config);;
    
  exception Forget_modules_exn of Assistance_dfa_module_t.t  list ;;     

  let forget_modules fw mods = 
    let check = Assistance_fw_with_dependencies.check_module_sequence_for_forgettability (parent fw) mods in 
    if check <> []
    then raise(Forget_modules_exn(check))
    else
    let (new_parent,removed_files) = Assistance_fw_with_batch_compilation.forget_modules (parent fw) mods in 
    let descr = String.concat " , " (Assistance_image.image Assistance_dfa_module.to_line mods) in 
    let msg="delete "^descr in 
    let diff = Assistance_dircopy_diff.destroy Assistance_dircopy_diff.empty_one removed_files  in  
    let _ = Assistance_transmit_change_to_github.backup (Assistance_fw_poly.to_github_configuration fw) diff (Some msg) in     
    set_parent fw new_parent ;;     

  let forget_nonmodular_rootlesses fw rootless_paths=
      let new_parent = Assistance_fw_with_batch_compilation.remove_files (parent fw) rootless_paths in 
      let descr = String.concat " , " (Assistance_image.image Assistance_dfn_rootless.to_line rootless_paths) in 
      let msg="delete "^descr in 
      let diff = Assistance_dircopy_diff.destroy Assistance_dircopy_diff.empty_one rootless_paths  in  
      let _ = Assistance_transmit_change_to_github.backup (Assistance_fw_poly.to_github_configuration fw) diff (Some msg) in     
      set_parent fw new_parent ;;     
    
  
  let register_rootless_paths fw rootless_paths = 
      let new_parent = Assistance_fw_with_batch_compilation.register_rootless_paths (parent fw) rootless_paths in 
      let descr = String.concat " , " (Assistance_image.image Assistance_dfn_rootless.to_line rootless_paths) in 
      let msg="register "^descr in 
      let diff = Assistance_dircopy_diff.create Assistance_dircopy_diff.empty_one rootless_paths  in  
      let _ = Assistance_transmit_change_to_github.backup (Assistance_fw_poly.to_github_configuration fw) diff (Some msg) in     
      set_parent fw new_parent ;;  

   

  let relocate_module_to fw mod_name new_subdir = 
      let (new_parent,(_,replacements)) = Assistance_fw_with_batch_compilation.relocate_module_to (parent fw) mod_name new_subdir in 
      let msg="move "^(Assistance_dfa_module.to_line mod_name)^" to "^(Assistance_dfa_subdirectory.connectable_to_subpath new_subdir) in 
      let diff = Assistance_dircopy_diff.replace Assistance_dircopy_diff.empty_one replacements  in  
      let _ = Assistance_transmit_change_to_github.backup (Assistance_fw_poly.to_github_configuration fw) diff (Some msg) in     
      set_parent fw new_parent ;; 

  let rename_module fw old_middle_name new_nonslashed_name = 
      let (new_parent,(_,(file_renamings,changed_files))) = Assistance_fw_with_batch_compilation.rename_module (parent fw) old_middle_name new_nonslashed_name in 
      let msg="rename "^(Assistance_dfa_module.to_line(Assistance_dfn_middle.to_module old_middle_name))^
              " as "^(Assistance_no_slashes.to_string new_nonslashed_name) in       
      let diff1 = Assistance_dircopy_diff.replace Assistance_dircopy_diff.empty_one file_renamings  in  
      let diff2 = Assistance_dircopy_diff.add_changes diff1  changed_files  in  
      let _ = Assistance_transmit_change_to_github.backup (Assistance_fw_poly.to_github_configuration fw) diff2 (Some msg) in     
      set_parent fw new_parent ;;    

  let rename_subdirectory_as fw (old_subdir,new_subdir) = 
    let (new_parent,(_,original_reps)) = Assistance_fw_with_batch_compilation.rename_subdirectory_as 
          (parent fw) (old_subdir,new_subdir) in 
    let msg="rename "^(Assistance_dfa_subdirectory.connectable_to_subpath old_subdir)^
          " as "^(Assistance_dfa_subdirectory.connectable_to_subpath new_subdir) in 
    let diff = Assistance_dircopy_diff.replace Assistance_dircopy_diff.empty_one original_reps in   
    let _ = Assistance_transmit_change_to_github.backup (Assistance_fw_poly.to_github_configuration fw) diff (Some msg) in     
    set_parent fw new_parent ;; 

  
  let replace_string fw old_s new_s = 
      let (parent1,(changed_modules_in_any_order,all_changed_files)) = 
      Assistance_fw_with_batch_compilation.replace_string (parent fw) old_s new_s  in 
      let parent2 = Assistance_fw_with_batch_compilation.modern_recompile parent1 changed_modules_in_any_order in 
      let msg="rename "^old_s^" as "^new_s in 
      let diff = Assistance_dircopy_diff.add_changes Assistance_dircopy_diff.empty_one all_changed_files in 
      let _ = Assistance_transmit_change_to_github.backup (Assistance_fw_poly.to_github_configuration fw) diff (Some msg) in 
      set_parent fw parent2 ;;

  let replace_value fw ((preceding_files,path),(old_v,new_v)) = 
        let (parent1,(changed_modules_in_any_order,all_changes)) = 
        Assistance_fw_with_batch_compilation.replace_value (parent fw) ((preceding_files,path),(old_v,new_v))  in 
        let parent2 = Assistance_fw_with_batch_compilation.modern_recompile parent1 changed_modules_in_any_order in 
        let msg="rename "^old_v^" as "^new_v in 
        let diff = Assistance_dircopy_diff.add_changes Assistance_dircopy_diff.empty_one all_changes in 
        let _ = Assistance_transmit_change_to_github.backup (Assistance_fw_poly.to_github_configuration fw) diff (Some msg) in 
        set_parent fw parent2 ;; 
 
   
  let usual_recompile fw opt_comment = 
    let (new_parent,(changed_uc,changed_files)) = Assistance_fw_with_batch_compilation.usual_recompile (parent fw)  in 
    let diff = Assistance_dircopy_diff.add_changes Assistance_dircopy_diff.empty_one changed_files in 
    let _ = Assistance_transmit_change_to_github.backup (Assistance_fw_poly.to_github_configuration fw) diff opt_comment in 
    set_parent fw new_parent ;;
    

end;;  
      

let forget_modules = Private.forget_modules ;; 
let forget_nonmodular_rootlesses = Private.forget_nonmodular_rootlesses ;;  
let of_fw_with_batch_compilation =Private.usual_extension ;;
let of_fw_config_and_github_config = Private.of_fw_config_and_github_config ;;
let plunge_fw_config_with_github_config = Private.plunge_fw_config_with_github_config ;;
let register_rootless_paths = Private.register_rootless_paths ;;      
let relocate_module_to  = Private.relocate_module_to ;;         
let rename_module = Private.rename_module ;;   
let rename_subdirectory_as = Private.rename_subdirectory_as ;;     
let replace_string = Private.replace_string ;;  
let replace_value = Private.replace_value ;;    
let usual_recompile = Private.usual_recompile ;;



end;;






module Assistance_fw_with_persisting=struct

(* 

#use"Filewatching/fw_with_persisting.ml";;

*)

module Private=struct

  let building_site =  Assistance_coma_constant.usual_build_subdir ;;

  let loadings (main_root,rootless_path_for_loadingsfile) (dirs,hms)=
      let path_for_loadingsfile = Assistance_dfn_rootless.to_line rootless_path_for_loadingsfile in 
      let s_root=Assistance_dfa_root.connectable_to_subpath main_root in
      let part1="\n(*\n #use\""^s_root^(path_for_loadingsfile)^"\";"^";\n*)\n\n" in
      let temp5=Assistance_image.image (
        fun sd->
        "#directory\""^s_root^(Assistance_dfa_subdirectory.connectable_to_subpath sd)^"\";"^";"
      ) (building_site::dirs) in
      let part2=String.concat "\n" temp5 
      and part3="\n\n#load\"str.cma\";"^";\n#load\"unix.cma\";"^";\n\n\n" in
      let temp2=Assistance_image.image (
        function hm->
          let nm = Assistance_dfn_endingless.to_module hm in  
          let s=Assistance_cull_string.after_rightmost (Assistance_dfa_module.to_line nm) '/' in
          "#load\""^s^".cmo\";"^";"
      ) hms in
      let temp3="\n\n\n"::temp2 in
      let part4=String.concat "\n" temp3 
      and part5="\n\n\n" in
      part1^part2^part3^part4^part5;; 
          
    
    let instructions_for_merlinfile main_root dirs=
      let s_root=Assistance_dfa_root.connectable_to_subpath main_root in
      let temp1=Assistance_image.image 
        (fun sdir->"S "^s_root^(Assistance_dfa_subdirectory.connectable_to_subpath sdir) )
      dirs in
      let temp2=("B "^s_root^(Assistance_dfa_subdirectory.connectable_to_subpath building_site))::temp1 in
      "\n\n\n"^(String.concat "\n" temp2)^"\n\n\n";; 

    let instructions_for_printersfile printer_equipped_types=
        let temp2=List.rev_map (
          function (x,compiled_correctly)->
          if compiled_correctly 
          then let modname=Assistance_dfn_endingless.to_module x in 
               "#install_printer "^(Assistance_dfa_module.capitalized_form modname)^".print_out;"^";"
          else ""
        ) printer_equipped_types in
        let temp3="\n\n\n"::(List.rev ("\n\n\n"::temp2)) in
        let part2=String.concat "\n" temp3 in
        part2;;  

    let save_loadingsfile (root,rootless_path_for_loadingsfile) (dirs,hms)=
       let s=loadings (root,rootless_path_for_loadingsfile)
        (dirs,hms)
       and lm=Assistance_dfn_join.root_to_rootless root rootless_path_for_loadingsfile in
       Assistance_io.overwrite_with (Assistance_dfn_full.to_absolute_path lm) s;;
    
    let save_merlinfile (root,rootless_path_for_merlinfile) dirs=
        let s=instructions_for_merlinfile root dirs 
        and lm=Assistance_dfn_join.root_to_rootless root rootless_path_for_merlinfile in
        Assistance_io.overwrite_with (Assistance_dfn_full.to_absolute_path lm) s;;
  
    let save_printersfile (root,rootless_path_for_printersfile) printer_equipped_types=
       let s=instructions_for_printersfile printer_equipped_types
       and lm=Assistance_dfn_join.root_to_rootless root rootless_path_for_printersfile in
       let beg_mark="(*Registered printers start here *)"
       and end_mark="(*Registered printers end here *)" in
       Assistance_replace_inside.overwrite_between_markers_inside_file
       (Assistance_overwriter.of_string s)
       (beg_mark,end_mark)
       (Assistance_dfn_full.to_absolute_path lm);;
    
    
  
    let save_targetfile rootless_path_for_targetfile root_dir crobj_form=
      let s1=Assistance_crobj_parsing.unparse crobj_form in
      let lt=Assistance_dfn_join.root_to_rootless root_dir rootless_path_for_targetfile in
      Assistance_io.overwrite_with (Assistance_dfn_full.to_absolute_path lt) s1;;
    
    
    
    let write_all 
    (
      rootless_path_for_targetfile,
      rootless_path_for_loadingsfile,
      rootless_path_for_printersfile
      )
      (root_dir,elesses,crobj_form,directories,printer_equipped_types) = 
       (
        save_loadingsfile (root_dir,rootless_path_for_loadingsfile) (directories,elesses);
        save_targetfile rootless_path_for_targetfile root_dir crobj_form;
        save_printersfile (root_dir,rootless_path_for_printersfile) printer_equipped_types;
       );;

       
      

    let save_all cs=
      let root_dir = Assistance_fw_poly.root cs 
      and elesses = Assistance_fw_with_batch_compilation.up_to_date_elesses cs
      and crobj_form = Assistance_fw_poly.to_concrete_object cs 
      and directories = Assistance_fw_with_dependencies.all_subdirectories cs 
      and printer_equipped_types = Assistance_fw_with_batch_compilation.preq_types_with_extra_info cs 
        in
       write_all 
      (
        Assistance_coma_constant.rootless_path_for_targetfile,
        Assistance_coma_constant.rootless_path_for_loadingsfile,
        Assistance_coma_constant.rootless_path_for_printersfile
      )
      
	      (root_dir,elesses,crobj_form,directories,printer_equipped_types)
      ;;

    let read_persistent_version fw=
      let full_path=Assistance_dfn_join.root_to_rootless (Assistance_fw_poly.root fw)  Assistance_coma_constant.rootless_path_for_targetfile in
      let ap= Assistance_dfn_full.to_absolute_path full_path in
      let the_archive=Assistance_io.read_whole_file ap in
      let archived_object = Assistance_crobj_parsing.parse the_archive in 
      Assistance_fw_poly.of_concrete_object archived_object;;   

end;;  

let persist = Private.save_all;;
let read_persistent_version = Private.read_persistent_version ;;



end;;






module Assistance_modify_coma_state=struct

(* 

#use"Compilation_management/modify_coma_state.ml";;

*)


   
   
   module And_save = struct 
   
         let forget_modules cs mod_names=
            let _=Assistance_fw_with_archives.check_that_no_change_has_occurred cs in 
            let cs2 = Assistance_fw_with_githubbing.forget_modules cs mod_names in 
            let _=Assistance_fw_with_persisting.persist cs2 in 
            cs2;;
   
         let forget_nonmodular_rootlesses cs rootless_paths=
            let _=Assistance_fw_with_archives.check_that_no_change_has_occurred cs in 
            let cs2 = Assistance_fw_with_githubbing.forget_nonmodular_rootlesses cs rootless_paths in 
            let _=Assistance_fw_with_persisting.persist cs2 in 
            cs2;;
   
         let internet_access cs bowl=   
            let cs2=Assistance_fw_poly.set_gitpush_after_backup cs bowl in 
            let _=Assistance_fw_with_persisting.persist cs2 in 
            cs2;;
         
         let recompile cs opt_comment=
            let cs2= Assistance_fw_with_githubbing.usual_recompile cs opt_comment in 
            let _=Assistance_fw_with_persisting.persist cs2 in 
            cs2;;
         

         let refresh cs =
            let cs2= Assistance_fw_with_githubbing.of_fw_config_and_github_config 
            (Assistance_fw_poly.to_fw_configuration cs) 
            (Assistance_fw_poly.to_github_configuration cs)  in 
            let _=Assistance_fw_with_persisting.persist cs2 in 
            cs2;;       

         let register_rootless_paths cs rootless_path=
            let _=Assistance_fw_with_archives.check_that_no_change_has_occurred cs in 
            let cs2 = Assistance_fw_with_githubbing.register_rootless_paths cs rootless_path in 
            let _=Assistance_fw_with_persisting.persist cs2 in 
            cs2;;  
   
         let relocate_module_to cs old_module new_subdir=
            let _=Assistance_fw_with_archives.check_that_no_change_has_occurred cs in 
            let cs2 = Assistance_fw_with_githubbing.relocate_module_to cs old_module new_subdir in 
            let _=Assistance_fw_with_persisting.persist cs2 in 
            cs2;;   
   
         let rename_module cs old_middle_name new_nonslashed_name=
            let _=Assistance_fw_with_archives.check_that_no_change_has_occurred cs in 
            let cs2=Assistance_fw_with_githubbing.rename_module cs old_middle_name new_nonslashed_name in 
            let _=Assistance_fw_with_persisting.persist cs2 in 
            cs2;;  

         let rename_subdirectory cs old_subdir new_subdir=
            let _=Assistance_fw_with_archives.check_that_no_change_has_occurred cs in 
            let cs2=Assistance_fw_with_githubbing.rename_subdirectory_as cs (old_subdir,new_subdir) in 
            let _=Assistance_fw_with_persisting.persist cs2 in 
            cs2;;  


         let replace_string cs old_s new_s=
            let _=Assistance_fw_with_archives.check_that_no_change_has_occurred cs in 
            let cs2=Assistance_fw_with_githubbing.replace_string cs old_s new_s in 
            let _=Assistance_fw_with_persisting.persist cs2 in 
            cs2;;     
    
         
         let replace_value cs ((preceding_files,path),(old_v,new_v))=
            let _=Assistance_fw_with_archives.check_that_no_change_has_occurred cs in 
            let cs2= Assistance_fw_with_githubbing.replace_value cs ((preceding_files,path),(old_v,new_v)) in 
            let _=Assistance_fw_with_persisting.persist cs2 in 
            cs2;;        
   
   end ;;
   
   module Reference = struct 
   
         let forget_modules pcs mod_names=
            let new_cs = And_save.forget_modules (!pcs) mod_names in 
            pcs:=new_cs;;
   
         let forget_nonmodular_rootlesses pcs rootless_paths=
            let new_cs = And_save.forget_nonmodular_rootlesses (!pcs) rootless_paths in 
            pcs:=new_cs;; 
   
         let initialize pcs =
         let new_cs = Assistance_fw_with_persisting.read_persistent_version (!pcs) in 
         pcs:=new_cs;;

         let initialize_if_empty pcs =
            if Assistance_fw_with_dependencies.number_of_modules (!pcs) = 0 
            then initialize pcs;;
   
         let internet_access pcs bowl=
            let new_cs = And_save.internet_access (!pcs) bowl in 
             pcs:=new_cs;;
   
         let recompile pcs opt_comment=
            let new_cs = And_save.recompile (!pcs) opt_comment in 
            pcs:=new_cs;;
   
   
         let refresh pcs =
            let new_cs = And_save.refresh (!pcs)  in 
            pcs:=new_cs;;
   
         let register_rootless_paths pcs rootless_paths=
            let new_cs = And_save.register_rootless_paths (!pcs) rootless_paths in 
            pcs:=new_cs;;
   
   
         let relocate_module_to pcs old_module new_subdir=
            let new_cs = And_save.relocate_module_to (!pcs) old_module new_subdir in 
            pcs:=new_cs;;  
   
   
         let rename_subdirectory pcs old_subdir new_subdir=
            let new_cs = And_save.rename_subdirectory (!pcs) old_subdir new_subdir in 
            pcs:=new_cs;;
            
   
         let rename_module pcs old_middle_name new_nonslashed_name=
            let new_cs = And_save.rename_module (!pcs) old_middle_name new_nonslashed_name in 
            pcs:=new_cs;;
   
         let replace_string pcs old_s new_s=
            let new_cs = And_save.replace_string (!pcs) old_s new_s in 
            pcs:=new_cs;;   
         
         let replace_value pcs ((preceding_files,path),(old_v,new_v))=
            let new_cs = And_save.replace_value (!pcs) ((preceding_files,path),(old_v,new_v)) in 
            pcs:=new_cs;;      
   
   end ;;
   
   
   module Syntactic_sugar = struct 
   
   let display_number_of_modules cs_ref =
      let number_of_modules = List.length(Assistance_fw_with_dependencies.all_endinglesses (!cs_ref)) in 
      let msg = "\nThere are now "^(string_of_int number_of_modules)^" modules defined.\n" in 
      (print_string msg;flush stdout) ;; 

   let forget cs_ref data = 
      let ref_for_modules = ref []
      and ref_for_paths = ref [] in 
      let _=List.iter (
         fun descr ->
           if String.contains descr '.'
           then ref_for_paths:= (Assistance_dfn_rootless.of_line descr)::(!ref_for_paths)
           else ref_for_modules:= (Assistance_dfa_module.of_line descr) ::(!ref_for_modules)
      ) data in
      let all_paths = List.rev(!ref_for_paths) 
      and all_modules =  List.rev(!ref_for_modules) in 
      let _=(if all_paths=[] then () else Reference.forget_nonmodular_rootlesses cs_ref all_paths) in 
      let _=(if all_modules=[] then () else Reference.forget_modules cs_ref all_modules) in 
      display_number_of_modules cs_ref;;
   
   
   let register_several cs_ref lines =
      let rootless_paths = Assistance_image.image Assistance_dfn_rootless.of_line lines in 
      let _ = Reference.register_rootless_paths cs_ref  rootless_paths in 
      display_number_of_modules cs_ref ;;
   
   let register_one cs_ref line = 
      register_several cs_ref [line]  ;;
   
   let relocate_module_to cs_ref old_module_name new_subdir=
       let mn = Assistance_dfa_module.of_line(String.uncapitalize_ascii old_module_name) in
       Reference.relocate_module_to cs_ref mn new_subdir ;;
   
   let rename_module cs_ref old_module_name new_name=
      let mn = Assistance_dfa_module.of_line(String.uncapitalize_ascii old_module_name) in
      let old_eless = Assistance_fw_with_dependencies.endingless_at_module (!cs_ref) mn in
      let old_middle_name = Assistance_dfn_endingless.to_middle old_eless in    
      let new_nonslashed_name = Assistance_no_slashes.of_string (String.uncapitalize_ascii new_name) in 
      Reference.rename_module cs_ref old_middle_name new_nonslashed_name;; 
   
   
   exception Rename_string_or_value_exn of string ;;

   let rename_string_or_value cs_ref old_sov new_sov =
         if not(String.contains old_sov '.')
         then Reference.replace_string cs_ref old_sov new_sov 
         else 
         let j=Assistance_substring.leftmost_index_of_in "." old_sov in
         if j<0 
         then raise(Rename_string_or_value_exn(old_sov))
         else let module_name=Assistance_cull_string.beginning (j-1) old_sov in
         let endingless= Assistance_fw_with_dependencies.decipher_module (!cs_ref)  module_name 
         and path= Assistance_fw_with_dependencies.decipher_path (!cs_ref)  module_name in 
         let nm=Assistance_dfn_endingless.to_module endingless in
         let pre_temp2=(Assistance_fw_with_dependencies.ancestors_for_module (!cs_ref) nm)@[nm] in
         let temp2=Assistance_image.image (Assistance_fw_with_dependencies.endingless_at_module (!cs_ref)) pre_temp2 in
         let preceding_files=Assistance_image.image  (fun eless2->
                           Assistance_dfn_full.to_absolute_path(Assistance_dfn_join.to_ending eless2 Assistance_dfa_ending.ml)
         ) temp2 in
         Reference.replace_value cs_ref ((preceding_files,path),(old_sov,new_sov)) ;;       

   let rename_subdirectory cs_ref old_subdirname new_subdir_short_name=
       let old_subdir = Assistance_fw_with_dependencies.find_subdir_from_suffix (!cs_ref) old_subdirname  in
       let new_subdir = Assistance_dfa_subdirectory.compute_long_subdir_name old_subdir new_subdir_short_name  in 
       Reference.rename_subdirectory cs_ref old_subdir new_subdir ;;
   
   
   end;;

end;;






module Assistance_usual_coma_state=struct

(* 

#use"Compilation_management/usual_coma_state.ml";;

*)

exception No_module_with_name of string;;

module Private = struct 

let main_ref=
  let (root,backup_dir,githubbing)=Assistance_coma_big_constant.This_World.triple in 
  let fw_config = Assistance_fw_configuration.of_root root 
  and github_config = Assistance_fw_poly.construct_github_configuration 
  ~root:root
  ~dir_for_backup:backup_dir
  ~gitpush_after_backup:githubbing
  ~github_url:Assistance_coma_big_constant.github_url
  ~encoding_protected_files:[]
  in 
  ref(Assistance_fw_with_githubbing.plunge_fw_config_with_github_config  fw_config github_config);;
end;;

let all_endinglesses ()=Assistance_fw_with_dependencies.all_endinglesses (!(Private.main_ref)) ;; 

let clean_debug_dir ()=Assistance_fw_with_batch_compilation.clean_debug_dir (!(Private.main_ref));;
let clean_exec_dir ()=Assistance_fw_with_batch_compilation.clean_exec_dir (!(Private.main_ref));;

let duplicate_module old_t1 old_t2=
  Assistance_fw_with_dependencies.duplicate_module (!(Private.main_ref)) old_t1 old_t2;;

let find_endingless modname = 
  Assistance_fw_with_dependencies.endingless_at_module
   (!(Private.main_ref)) (Assistance_dfa_module.of_line (String.capitalize_ascii modname));;

let forget_one modname=Assistance_modify_coma_state.Syntactic_sugar.forget Private.main_ref [modname];;

let forget_several modnames=Assistance_modify_coma_state.Syntactic_sugar.forget Private.main_ref modnames;;

let initialize ()=Assistance_modify_coma_state.Reference.initialize Private.main_ref ;; 

let initialize_if_empty ()=Assistance_modify_coma_state.Reference.initialize_if_empty Private.main_ref ;;                       

let initialize ()=Assistance_modify_coma_state.Reference.initialize Private.main_ref ;; 

let internet_access () = Assistance_fw_poly.gitpush_after_backup (!(Private.main_ref)) ;;

let latest_changes ()=Assistance_fw_with_archives.latest_changes (!(Private.main_ref));;

let list_values_from_module_in_modulesystem module_name=
   Assistance_fw_with_dependencies.list_values_from_module (!(Private.main_ref)) module_name;;

let main_ref=Private.main_ref;;

let modules_using_value x=Assistance_fw_with_dependencies.modules_using_value (!(Private.main_ref)) x;;

let recompile opt=Assistance_modify_coma_state.Reference.recompile Private.main_ref opt;;
   

let refresh ()=Assistance_modify_coma_state.Reference.refresh Private.main_ref;;

let register_rootless_line x=Assistance_modify_coma_state.Syntactic_sugar.register_one Private.main_ref x;;
  
let register_rootless_lines x=Assistance_modify_coma_state.Syntactic_sugar.register_several Private.main_ref x;;

let relocate_module_to old_module_name new_subdir=
   Assistance_modify_coma_state.Syntactic_sugar.relocate_module_to Private.main_ref old_module_name new_subdir;;

let rename_subdirectory old_subdirname new_subdirname=
    Assistance_modify_coma_state.Syntactic_sugar.rename_subdirectory Private.main_ref old_subdirname new_subdirname;;

let rename_module old_name new_name=
   Assistance_modify_coma_state.Syntactic_sugar.rename_module Private.main_ref old_name new_name;;


let rename_string_or_value old_name new_name=
   Assistance_modify_coma_state.Syntactic_sugar.rename_string_or_value
   (Private.main_ref) old_name new_name;;

let set_internet_access bowl=Assistance_modify_coma_state.Reference.internet_access Private.main_ref bowl;;


let show_value_occurrences_in_modulesystem module_name=
   Assistance_fw_with_batch_compilation.show_value_occurrences (!(Private.main_ref)) module_name;;

let start_debugging ()=Assistance_fw_with_batch_compilation.start_debugging (!(Private.main_ref));;
let start_executing short_path= Assistance_fw_with_batch_compilation.start_executing (!(Private.main_ref)) short_path;;


let sugared_above capitalized_or_not_module_name=
  let mn0 = Assistance_dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  Assistance_image.image Assistance_dfa_module.to_line
  (Assistance_fw_with_dependencies.ancestors_for_module (!(Private.main_ref)) mn0);;

let sugared_below capitalized_or_not_module_name=
  let mn0 = Assistance_dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  Assistance_image.image Assistance_dfa_module.to_line
  (Assistance_fw_with_dependencies.below (!(Private.main_ref)) mn0);;

let sugared_directly_above capitalized_or_not_module_name=
  let mn0 = Assistance_dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  Assistance_image.image Assistance_dfa_module.to_line
  (Assistance_fw_with_dependencies.direct_fathers_for_module (!(Private.main_ref)) mn0);;

let sugared_directly_below capitalized_or_not_module_name=
let mn0 = Assistance_dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
Assistance_image.image Assistance_dfa_module.to_line
(Assistance_fw_with_dependencies.directly_below (!(Private.main_ref)) mn0);;


end;;

