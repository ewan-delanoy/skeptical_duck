(*

#directory "+str";;
#load "str.cma";;
#directory "+unix";;
#load "unix.cma";;
#use"Teuliou/OCaml/skeptical_duck/watched/watched_and_githubbed/Standalone_old_files/standalone_cee_analysis.ml";;

*)




module Stdl_image=struct

(*

#use"lib/image.ml";;
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






module Stdl_arithmetic_list=struct

(*

#use"lib/Listy/arithmetic_list.ml";;

*)

  
exception Compute_largest_connected_interval_on_the_left_exn ;;
exception Pusher_for_far_apart_components_exn ;;

module Private = struct 

let compute_largest_connected_interval_on_the_left initial_l =
  let rec tempf = (fun  (a,b,l)->
   match l with 
   [] -> ((a,b),[])
   |head :: tail -> if head = b+1 
                    then tempf(a,b+1,tail)
                    else ((a,b),l)  
  ) in  
  match initial_l with 
  [] -> raise Compute_largest_connected_interval_on_the_left_exn
  | head2 :: tail2 -> tempf (head2,head2,tail2) ;;



let pusher_for_far_apart_components 
   (d,treated,last_elt,in_progress,to_be_treated) = 
    match to_be_treated with 
    [] -> raise Pusher_for_far_apart_components_exn 
    | new_elt :: other_elts ->
        if (new_elt-last_elt > d) 
        then (d,(List.rev(last_elt::in_progress))::treated,new_elt,[],other_elts)
        else (d,treated,new_elt,last_elt::in_progress,other_elts)  ;;   

let rec iterator_for_far_apart_components uple =
    let (_d,treated,last_elt,in_progress,to_be_treated) = uple in        
    match to_be_treated with 
    [] ->  List.rev(List.rev(last_elt::in_progress)::treated)
    | _ -> iterator_for_far_apart_components(pusher_for_far_apart_components uple);;

let compress_small_block l = 
    List.flatten (
      Stdl_image.image (fun (i,j)->if i=j then [i] else [i;j]) l
    ) ;;

(* compress_small_block [(1,1);(3,4);(6,6);(8,9)] ;; *)

let rec find_large_block_opt (treated,to_be_treated) =
  match to_be_treated with 
    [] -> None
   |(i,j) :: others -> 
      if j-i>1
      then Some(List.rev(treated),(i,j),others) 
      else find_large_block_opt ((i,j)::treated,others) ;;

let rec helper_for_cc_writing (treated,to_be_treated) = 
   if to_be_treated = [] then List.rev(treated) else 
   match find_large_block_opt ([],to_be_treated) with
   None -> 
     let last_part = (false,0,0,compress_small_block to_be_treated) in 
     List.rev (last_part :: treated)
  |Some (before,(i,j),after) ->
     let increment1 = (
       if before = [] then treated else 
       (false,0,0,compress_small_block before) :: treated
     ) in 
     helper_for_cc_writing 
      ( (true,i,j,[]) :: increment1,after) ;;

(*
helper_for_cc_writing
([],[(1,1);(3,4);(6,6);(8,9);(11,15);(17,19);
  (22,22);(24,25);(27,27);(29,32);(36,37)
]) ;;
*)

let rec helper_for_connectedness_test i = function 
 [] -> true 
 |j :: others -> 
    if j = i+1 
    then helper_for_connectedness_test j others 
    else false ;;

let is_connected = function 
  [] -> true 
  |i :: others -> helper_for_connectedness_test i others ;;


let rec helper_for_cumulative_sums (treated,to_be_treated)=match to_be_treated with 
  [] -> treated 
  |x::others -> helper_for_cumulative_sums (x::(Stdl_image.image (fun t->x+t) treated),others) ;;

let cumulative_sums l = helper_for_cumulative_sums ([],List.rev l) ;;


end ;;


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

let cumulative_sums = Private.cumulative_sums ;;

(*

cumulative_sums [7;20;400;5000] ;;

*)


let decompose_into_connected_components l=
  let rec tempf = (fun 
     (treated,to_be_treated)->
     if to_be_treated = [] then List.rev treated else 
     let (interval,others) =  Private.compute_largest_connected_interval_on_the_left to_be_treated in 
     tempf (interval::treated,others)
  ) in 
  tempf ([],l);;

(*

decompose_into_connected_components [3; 4; 5; 6; 7; 10; 11; 12; 13; 14; 15; 31; 32; 33; 34; 35; 36; 37; 38; 39; 40;
 41; 42; 43; 44; 45; 46; 47; 48];;

*)  

let decompose_into_far_apart_components ~max_inner_distance = function 
  [] -> []
  |a :: b -> Private.iterator_for_far_apart_components
      (max_inner_distance,[],a,[],b) ;;

(*

decompose_into_far_apart_components ~max_inner_distance:5 [1;2;3;8;9;10;16;17;18] ;;
decompose_into_far_apart_components ~max_inner_distance:4 [1;2;3;8;9;10;16;17;18] ;;

*)



let delta l=
  let rec sub_f=
  (function (accu,a,rl)->match rl with
  []->List.rev(accu)
  |b::x->sub_f((b-a)::accu,b,x)
  ) in
  match l with
  []->[]
  |u::v->sub_f([],u,v);;
           
let is_connected = Private.is_connected ;;

let write_using_connected_components l = 
    let temp1 = decompose_into_connected_components l in 
    let temp2 = Private.helper_for_cc_writing ([],temp1) in 
    String.concat "" ( Stdl_image.image (
      fun (is_a_long_block,i,j,l) ->
        if is_a_long_block 
        then "["^(string_of_int i)^".."^(string_of_int j)^"]"
        else "{"^(String.concat "," (Stdl_image.image string_of_int l))^"}"
    ) temp2) ;;
(*

write_using_connected_components
[1; 3; 4; 6; 8; 9; 11; 12; 13; 14; 15; 17; 18; 19; 22; 24; 25; 27; 29; 30;
   31; 32; 36; 37] ;;

*)



end;;






module Stdl_int_range=struct

(*

#use"lib/int_range.ml";;

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

let rec find_opt f a b=
if (a>b) 
then None
else if f(a)
  then Some(a)
  else find_opt f (a+1) b;;	  

let find_and_stop f a b=
let rec find_and_stop0=(function
 j->if (j>b)
    then None
  else match f(j) with
   None->find_and_stop0(j+1)
   |Some(x)->Some(x)
) in
find_and_stop0 a;;

let constant_list n x=scale (function _j->x) 1 n;;

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






module Stdl_list_again=struct

(*

#use"lib/Listy/list_again.ml";;

*)

exception No_leader_found_exn ;; 

module Private = struct

let rec helper_for_common_initial_sublist (treated,to_be_treated1,to_be_treated2) =
  match to_be_treated1 with 
  [] -> (List.rev treated,to_be_treated1,to_be_treated2)
  | a1 :: others1 ->
    (
      match to_be_treated2 with 
      [] -> (List.rev treated,to_be_treated1,to_be_treated2)
      | a2 :: others2 ->
          if a1<>a2
          then (List.rev treated,to_be_treated1,to_be_treated2)
          else helper_for_common_initial_sublist (a1::treated,others1,others2)   
    ) ;;

let common_head_opt lists =
  match lists with 
  [] -> None
  | l1 :: _ ->
   match l1 with 
    [] -> None 
    |a :: _ ->
      if List.exists (function []->true |b::_->b<>a) lists 
      then None 
      else Some a  ;;
      
let rec iterator_for_common_initial_sublist (treated,to_be_treated) = match common_head_opt to_be_treated with 
   None -> (List.rev treated,to_be_treated)
   |Some a -> iterator_for_common_initial_sublist (a::treated, Stdl_image.image List.tl to_be_treated)

let common_initial_sublist ll = iterator_for_common_initial_sublist ([],ll) ;;

let common_final_sublist ll =
   let (rev_common,rev_core)= common_initial_sublist (Stdl_image.image List.rev ll) in 
   (Stdl_image.image List.rev rev_core,List.rev rev_common) ;;

(*

common_final_sublist [[7;1;2;3];[8;9;1;2;3];[10;11;12;1;2;3]] ;;

*)   

let two_sided_common_parts ll=
  let (temp,right) = common_final_sublist ll in 
  let (left,core) = common_initial_sublist temp in 
  (left,core,right) ;; 



let rec iterator_for_finding_and_remembering f (treated,to_be_treated) = 
  match to_be_treated with 
  [] -> None 
  |item :: next_items ->
    if f item 
    then Some(List.rev treated,item,next_items)
    else iterator_for_finding_and_remembering f (item::treated,next_items) ;;

let find_and_remember_opt f items = 
    iterator_for_finding_and_remembering f ([],items) ;;
  
let find_rightmost_and_remember_opt f items = 
  Option.map (fun (rev_right,item,rev_left)->
    (List.rev rev_left,item,List.rev rev_right)) (find_and_remember_opt f (List.rev items))  ;;

let rec helper_for_connected_fibers 
  (treated,start_of_current_fiber,end_of_current_fiber,fiber,fiber_image,to_be_treated) = 
  match to_be_treated with 
  [] -> List.rev (  ((start_of_current_fiber,end_of_current_fiber),List.rev fiber,fiber_image) :: treated)
  |(idx_for_b,b,image_of_b) :: others ->
      if image_of_b<>fiber_image 
     then let newly_treated = ((start_of_current_fiber,end_of_current_fiber),List.rev fiber,fiber_image) :: treated in 
          helper_for_connected_fibers  
          (newly_treated,idx_for_b,idx_for_b,[b],image_of_b,others)  
     else helper_for_connected_fibers 
         (treated,start_of_current_fiber,idx_for_b,b::fiber,fiber_image,others)   ;;

let connected_fibers f l =
   let indexed_l = Stdl_int_range.index_everything l in 
   let indexed_l_with_images = Stdl_image.image (fun (i,x)->(i,x,f x)) indexed_l in    
   match indexed_l_with_images with 
   [] -> []
   |(idx_for_a,a,image_of_a) :: others ->
      helper_for_connected_fibers  
          ([],idx_for_a,idx_for_a,[a],image_of_a,others)  ;;

let rec helper1_for_interval (accu,b,l) =
  if b =0
  then List.rev accu
  else helper1_for_interval ((List.hd l)::accu,b-1,List.tl l);;          

let rec helper2_for_interval (a,b,l) =
  if a =1
  then helper1_for_interval ([List.hd l],b-1,List.tl l)
  else helper2_for_interval (a-1,b-1,List.tl l);;

let interval l a b =
   let n = List.length l in 
   if (1>a) || (a>b) || (b>n)
   then []
   else helper2_for_interval (a,b,l) ;; 

let replace_if_proposed replacements x =
  match List.assoc_opt x replacements with 
   None -> x 
   |(Some y) -> y ;;

let decompose_using_prefix_opt l pref = 
  let (_common,left,right) = helper_for_common_initial_sublist ([],l,pref) in 
  if right=[]
  then Some left 
  else None ;;

(*

decompose_using_prefix_opt (Int_range.range 1 7) (Int_range.range 1 3);;
decompose_using_prefix_opt (Int_range.range 1 7) (Int_range.range 2 4);;
*)  

let decompose_using_pattern_opt l pattern = 
  List.find_map (fun pref ->Option.map (fun res->(pref,res)) 
  (decompose_using_prefix_opt l pref)) pattern ;;

(*

decompose_using_pattern_opt (Int_range.range 1 7) [[8];(Int_range.range 1 3);[9;10]];;
*)    

let rec iterator_for_finding_interval_sublist_and_remembering itvsub (treated,to_be_treated) = 
  match to_be_treated with 
  [] -> None 
  |item :: next_items ->
    match decompose_using_prefix_opt to_be_treated itvsub with 
    None -> iterator_for_finding_interval_sublist_and_remembering itvsub (item::treated,next_items) 
    |Some right ->Some(List.rev treated,right) ;;

let find_interval_sublist_and_remember_opt itvsub items = 
    iterator_for_finding_interval_sublist_and_remembering itvsub ([],items) ;;

let rec iterator_for_finding_pattern_and_remembering pattern (treated,to_be_treated) = 
  match to_be_treated with 
  [] -> None 
  |item :: next_items ->
    match decompose_using_pattern_opt to_be_treated pattern with 
    None -> iterator_for_finding_pattern_and_remembering pattern (item::treated,next_items) 
    |Some (middle,right) ->Some(List.rev treated,middle,right) ;;

let find_pattern_and_remember_opt pattern items = 
    iterator_for_finding_pattern_and_remembering pattern ([],items) ;;

let find_rightmost_pattern_and_remember_opt f items = 
  Option.map (fun (rev_right,item,rev_left)->
    (List.rev rev_left,item,List.rev rev_right)) (find_pattern_and_remember_opt f (List.rev items))  ;;


let rec helper_for_computing_leaders_and_followers f (treated,current_pair) =
    let (current_leader,after_current_leader) = current_pair in 
    match find_and_remember_opt f after_current_leader with 
    None -> List.rev ( (current_leader,after_current_leader)::treated)
    |Some(before_next_leader,next_leader,after_next_leader) ->
        let next_treated= (current_leader,before_next_leader) :: treated 
        and next_pair = (next_leader,after_next_leader) in 
        helper_for_computing_leaders_and_followers f (next_treated,next_pair) ;;



let leaders_and_followers f l=
   match find_and_remember_opt f l with 
    None -> raise No_leader_found_exn
    |Some(before_first_leader,first_leader,after_first_leader) ->
        (before_first_leader, helper_for_computing_leaders_and_followers f ([],(first_leader,after_first_leader))) ;;

let rec inner_helper_for_filtering_and_remembering f x_false x_true (treated,to_be_treated) = match to_be_treated with 
  []->(List.rev treated,(Some x_false,Some x_true))
  |z::others ->
     if f z 
     then inner_helper_for_filtering_and_remembering f x_false x_true (z::treated,others) 
     else inner_helper_for_filtering_and_remembering f x_false x_true (treated,others) ;;

let rec helper1_for_filtering_and_remembering f x_true (treated,to_be_treated) = match to_be_treated with 
  []->(List.rev treated,(None,Some x_true))
  |y::others ->
     if f y 
     then helper1_for_filtering_and_remembering f x_true (y::treated,others) 
     else inner_helper_for_filtering_and_remembering f y x_true (treated,others) ;;
   

let rec helper2_for_filtering_and_remembering f x_false (treated,to_be_treated) = match to_be_treated with 
  []->(List.rev treated,(Some x_false,None))
  |y::others ->
     if f y 
     then inner_helper_for_filtering_and_remembering f x_false y (y::treated,others)
     else helper2_for_filtering_and_remembering f x_false (treated,others) ;;     


let filter_and_remember f l = match l with 
  [] ->([],(None,None))
  |x::others ->
     if f x 
     then helper1_for_filtering_and_remembering f x ([x],others) 
     else helper2_for_filtering_and_remembering f x ([x],others) ;;
   
let rec helper_for_best_effort_combine (treated,l1,l2)=
  match l1 with 
  [] -> List.rev treated 
  |x1 :: others1 -> 
      match l2 with 
     [] -> List.rev treated 
    |x2 :: others2 -> helper_for_best_effort_combine ((x1,x2)::treated,others1,others2) ;;
    

end ;;    

let assoc_right_opt y l = 
    Option.map fst (List.find_opt (fun p->snd(p)=y) l);;

(*
assoc_right_opt 3 (Int_range.scale (fun t->(t*t,t)) 1 7) ;;
*)

let best_effort_combine l1 l2= Private.helper_for_best_effort_combine ([],l1,l2) ;;
(*
best_effort_combine [1; 2; 3; 4; 5; 6; 7] [83; 84; 85; 86; 87] ;;

*)
let common_initial_sublist ll = Private.iterator_for_common_initial_sublist ([],ll) ;;

(*
common_initial_sublist [[1;2;3;7;8];[1;2;3;9];[1;2;3;10;11]] ;; 
*)

let connected_fibers = Private.connected_fibers ;;
(*
connected_fibers (fun x->int_of_float(floor(sqrt(float_of_int x)))) (Int_range.range 1 50) ;;
*)

let cut k l = (List.take k l,List.drop k l) ;;
(*
cut 3 [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] ;;
*)


let filter_and_remember = Private.filter_and_remember ;;

(*

filter_and_remember (fun x->x<60) (Int_range.range 1 50) ;;
filter_and_remember (fun x->x>60) (Int_range.range 1 50) ;;
filter_and_remember (fun x->x<10) (Int_range.range 1 50) ;;

*)


let find_and_remember_opt = Private.find_and_remember_opt ;;

(*

find_and_remember_opt (fun x->x=7) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] ;;

*)


let find_interval_sublist_and_remember_opt = Private.find_interval_sublist_and_remember_opt ;;    

(*

find_interval_sublist_and_remember_opt (Int_range.range 3 7) (Int_range.range 1 10) ;;

*)

let find_pattern_and_remember_opt = Private.find_pattern_and_remember_opt ;;

(*

find_pattern_and_remember_opt [[8];(Int_range.range 4 7);[9]] (Int_range.range 1 30) ;;

*)

let find_rightmost_and_remember_opt = Private.find_rightmost_and_remember_opt ;;

(*

find_rightmost_and_remember_opt (fun x->List.mem x [3;7;20]) (Int_range.range 1 30) ;;

*)

let find_rightmost_pattern_and_remember_opt = Private.find_rightmost_pattern_and_remember_opt ;;

(*


find_pattern_and_remember_opt  [[8];(Int_range.range 4 7);[9]] ([4;5;6;7]@(Int_range.range 1 30)) ;;

find_rightmost_pattern_and_remember_opt  [[8];(Int_range.range 4 7);[9]] ([4;5;6;7]@(Int_range.range 1 30)) ;;

*)

exception Head_with_tail_exn ;; 

let head_with_tail x=match x with
    []->raise(Head_with_tail_exn)
    |a::b->(a,b);;

let index_of_in x ll=
    let rec sub_f=(function (j,l)->match l with
    []->(-1)      
    |u::v->if u=x then j else sub_f(j+1,v)) in
    sub_f(1,ll);;

let index_of_in_opt x ll=
    let rec sub_f=(function (j,l)->match l with
    []->None      
    |u::v->if u=x then Some j else sub_f(j+1,v)) in
    sub_f(1,ll);;


let interval = Private.interval ;;

(* interval (Int_range.range 1 7) 2 5 ;; *)

let leaders_and_followers = Private.leaders_and_followers ;;

(*

leaders_and_followers (fun x->List.mem x [4;9;16;25]) (Int_range.range 1 30) ;;

*)

exception Long_head_with_tail_exn of int*int;;

let long_head_with_tail r l=
       let rec tempf=(function (j,kleiz,dehou)->
        if j=0 
        then (kleiz,dehou) 
        else match dehou with
            []->raise(Long_head_with_tail_exn(r,List.length l))
            |a::others->tempf(j-1,a::kleiz,others)
        ) in
        tempf(r,[],l);;

(*

long_head_with_tail 3 (Int_range.range 1 7);;

*)        
    
let long_head r l=if (r>(List.length l)) then l else List.rev(fst(long_head_with_tail(r)(l)));;
    

let rec next_in_line_opt x0 l =
  match l with 
  [] -> None 
  |x::others ->
     if x<>x0 
     then next_in_line_opt x0 others
     else
     if others=[] 
     then None 
     else Some(List.hd others) ;;

(*

next_in_line_opt 26 [17;26;35;44] ;;

*)     

let nonredundant_version l=
  let rec tempf=(
    fun (treated,to_be_treated)->
      match to_be_treated with
      []->List.rev treated
      |a::others->if List.mem a treated
                    then tempf(treated,others)
                    else tempf(a::treated,others)
  ) in
  tempf([],l);;


let pepper separator l = 
  match l with 
  [] -> []
  |a :: others ->
     a @ (List.flatten(Stdl_image.image (fun y->separator@y) others)) ;; 

(*

pepper [800;809] [[1]; [2]; [3]; [4]; [5]; [6]; [7]] ;;

*)     

let power_set l=
  let rec tempf=(function 
     (treated,to_be_treated)->match to_be_treated with
     []->treated
    |a::others->tempf(treated@(Stdl_image.image(function y->a::y)(treated)),others)
  ) in
  tempf([[]],List.rev l );;

let replace_if_proposed = Private.replace_if_proposed ;;


let rev_map f l=
  let rec tempf=(
    fun (treated,to_be_treated)->match to_be_treated with
    []->treated
    |a::others->tempf((f a)::treated,others)
  ) in
  tempf([],l);;

 
let rename_according_to_occurrence_rank l =   
  let indexed_l = Stdl_int_range.index_everything l in  
  let nonunique_old_names = Stdl_image.image snd l in 
  let old_names = nonredundant_version nonunique_old_names in 
  let occurrences_for_old_names = Stdl_image.image (fun name ->(name,List.filter_map (fun (idx,pair)->
   if snd(pair)=name then Some idx else None) indexed_l)) old_names in 
  let compute_new_name = (fun (idx,(thing,old_name)) ->
   let occs = List.assoc old_name occurrences_for_old_names in 
   if List.length(occs)=1 then (thing,old_name) else 
   let rank_among_occurrences = index_of_in idx occs in 
   (thing,old_name^(string_of_int rank_among_occurrences))  
   ) in
  Stdl_image.image compute_new_name indexed_l ;;
  

(*

rename_according_to_occurrence_rank (Image.image (fun x->(x,x)) ["A";"B";"C";"D";"B";"E";"F";"D";"E"]) ;;


*)  

let starts_with l1 l2 = 
   let (_common,_left,right) = Private.helper_for_common_initial_sublist ([],l1,l2) in 
  right=[];;

(*

starts_with (Int_range.range 1 3) (Int_range.range 1 7) ;;
starts_with (Int_range.range 1 7) (Int_range.range 1 3) ;;

*)

let sublist_with_indices l indices = Stdl_image.image (fun k->List.nth l (k-1)) indices ;;

(* sublist_with_indices  ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"] [2;3;7] ;; *)
  

let two_sided_common_parts = Private.two_sided_common_parts ;;

(*

two_sided_common_parts [[107;24;7;1;2;3];[107;24;8;9;1;2;3];[107;24;10;11;12;1;2;3]] ;;

*)   

let two_sided_cutting (length_before,length_after) l = 
   let (left,temp) = (List.take length_before l,List.drop length_before l) in 
   let m = List.length(temp)-length_after in 
   let (center,right)= (List.take m temp,List.drop m temp) in 
   (left,center,right) ;;
  
(*

two_sided_cutting (2,3) (Int_range.range 1 9);;


*)   

let universal_delta_list l=
  let rec sub_f=(function (accu,a,rl)->match rl with
      []->List.rev(accu)
    |b::x->sub_f((a,b)::accu,b,x)
  ) in
  match l with
  []->[]
  |u::v->sub_f([],u,v);;

let universal_tridelta_list l=
  let rec sub_f=(function (accu,a,b,rl)->match rl with
      []->List.rev(accu)
    |c::others->sub_f((a,b,c)::accu,b,c,others)
  ) in
  match l with
  []->[]
  |u1::others1->
    match others1 with 
     [] -> []
     |u2::others2 ->sub_f([],u1,u2,others2);;


(*

universal_tridelta_list [1; 2; 3; 4; 5; 6; 7] ;;

*)

end;;






module Stdl_memoized=struct

(*

#use"lib/memoized.ml";;

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






(* Contents of ~/Teuliou/OCaml/skeptical_duck/lib/memoized.mli:

 type ('a, 'b) map = 'a -> 'b
val make_from : ('a -> 'b) -> ('a, 'b) Hashtbl.t -> ('a, 'b) map
val make : ('a -> 'b) -> ('a, 'b) map
val recursive_from : (('a, 'b) map -> 'a -> 'b) -> ('a, 'b) Hashtbl.t -> ('a, 'b) map
val recursive : (('a->'b) -> 'a -> 'b) -> ('a, 'b) map
val reversible : ('a -> 'b) -> (('a,'b) map) * (('b,'a) map) * (('a,'a) map) * (('a list) ref) * ((('a*'a) list) ref)
val small : ('a -> 'a) -> 'a -> ((int,'a) map)




*) 






module Stdl_partition_list=struct

(*

#use"lib/Listy/partition_list.ml";;

*)

module Private = struct 

let rec select_first_colleagues f (treated,img,to_be_treated) =   
    match to_be_treated with 
    [] -> (List.rev treated,to_be_treated)
    | item :: others -> 
        if f item = img 
        then select_first_colleagues f (item::treated,img,others) 
        else (List.rev treated,to_be_treated) ;; 

let according_to_map_assuming_connectedness pairs f=
    let rec tempf = (fun (already_treated,to_be_treated)->
      match to_be_treated with 
      [] -> List.rev already_treated 
    | pair :: others ->
      let img1 = f pair in  
      let (part1,part2) = select_first_colleagues f ([pair],img1,others) in 
               tempf ((img1,part1)::already_treated,part2)     
    ) in 
    tempf ([],pairs) ;;

(*

according_to_map_assuming_connectedness [1,7;1,8;2,9;3,10;3,11;1,12] fst ;; 

*)

let according_to_map_without_assuming_connectedness pairs f=
  let rec tempf = (fun (already_treated,to_be_treated)->
       match to_be_treated with 
        [] -> List.rev already_treated 
       | pair :: _ ->
         let img1 = f pair in  
         let (part1,part2) = List.partition (fun pair2->f pair2=img1) to_be_treated in 
         tempf ((img1,part1)::already_treated,part2)     
   ) in 
   tempf ([],pairs) ;;

(*

according_to_map_without_assuming_connectedness [1,7;1,8;2,9;3,10;3,11;1,12] fst ;; 

*)   

let according_to_map pairs ?(assume_connectedness=false) f  =
   if assume_connectedness 
   then according_to_map_assuming_connectedness pairs f
   else according_to_map_without_assuming_connectedness pairs f ;;  

end ;;  

let according_to_fst ?(assume_connectedness=false) l = 
    let temp = Private.according_to_map l ~assume_connectedness fst in 
    Stdl_image.image (fun (img,antecs)->(img,Stdl_image.image snd antecs)) temp;; 

let according_to_map = Private.according_to_map ;;

exception From_set_of_ranges_exn of string * ((int * int)*(int *int)) ;;

let from_set_of_ranges l n=
    if l=[] then [1,n,false] else 
    let (last_i,last_j)=List.hd(List.rev l) 
    and (first_i,_)=List.hd l in
    let temp2=Stdl_list_again.universal_delta_list l in  
    let mistake_opt=List.find_opt(
      fun ((_i1,j1),(i2,_j2)) -> j1>=i2
    ) temp2 in 
    if mistake_opt<>None 
    then raise(From_set_of_ranges_exn("Ranges are not well ordered",Option.get mistake_opt)) 
    else    
    let temp3=Stdl_image.image (fun ((i1,j1),(i2,_j2))->
      [(i1,j1,true);(j1+1,i2-1,false)]
    ) temp2 in 
    let middle_part=List.flatten temp3 in
    let first_part=(if first_i>1 then [(1,first_i-1,false)] else []) 
    and last_part=(if last_j<n then [(last_j+1,n,false)] else []) in 
    first_part@middle_part@[(last_i,last_j,true)]@last_part;;

(*

from_set_of_ranges [(3,7);(41,52)] 100;;
from_set_of_ranges [(1,7);(41,52)] 100;;

[(3, 7); (619, 665); (680, 691)] 700 ;; 

*)

let split_in_half l=
   let temp1=Stdl_int_range.index_everything(l) in 
   let (temp2,temp3)=List.partition (fun (j,_)->(j mod 2)=1) temp1 in 
   (Stdl_image.image snd temp2,Stdl_image.image snd temp3);;

(*

split_list_in_half [1; 2; 3; 4; 5; 6; 7];;
split_list_in_half [1; 2; 3; 4; 5; 6; 7; 8];;

*)   

end;;






module Stdl_total_ordering_result_t=struct

(*

#use"lib/Ordered_Lists/total_ordering_result_t.ml";;

*)

type t =
   Lower 
 | Equal 
 | Greater ;;


 
           

end;;






module Stdl_total_ordering_t=struct

(*

#use"lib/Ordered_Lists/total_ordering_t.ml";;

*)



type 'a t = ( 'a -> 'a ->Stdl_total_ordering_result_t.t ) ;;



end;;






module Stdl_ordered=struct

(*
 
#use"lib/tordered.ml";;

*)

exception Empty_intersection_undefined;;    
exception The_empty_set_has_no_min ;; 
exception The_empty_set_has_no_max ;; 

type 'a ordered_list = Se of 'a list ;;

module Private = struct 

   let to_list (Se l)= l ;;
   
   let unsafe_of_list l = Se l ;;


  let intersect (cmpr:'a Stdl_total_ordering_t.t) ox oy=
      let rec tempf=(function (u,v,accu)->
        if u=[] then (List.rev(accu)) else
        if v=[] then (List.rev(accu)) else
        let xu=List.hd(u) and yu=List.tl(u) 
        and xv=List.hd(v) and yv=List.tl(v) in
        match cmpr(xu)(xv) with
         Stdl_total_ordering_result_t.Lower->tempf(yu,v,accu)
        |Stdl_total_ordering_result_t.Equal->tempf(yu,yv,xu::accu)
        |Stdl_total_ordering_result_t.Greater->tempf(u,yv,accu)
      ) in
      tempf(ox,oy,[]);;
  
  let is_increasing (cmpr:'a Stdl_total_ordering_t.t) l=
    if List.length(l)<2 then true else
    let rec tempf=(function
    (a,to_be_treated)->match to_be_treated with
     []->true
     |b::others->if (cmpr(a)(b)=Stdl_total_ordering_result_t.Lower)
                   then tempf(b,others)
                   else false
    ) in
    tempf(List.hd l,List.tl l);;
    
  
  let merge (cmpr:'a Stdl_total_ordering_t.t) ox oy=
      let rec tempf=(function (u,v,accu)->
        if u=[] then (List.rev_append(accu)(v)) else
        if v=[] then (List.rev_append(accu)(u)) else
        let xu=List.hd(u) and yu=List.tl(u) 
        and xv=List.hd(v) and yv=List.tl(v) in
      match cmpr(xu)(xv) with
        Stdl_total_ordering_result_t.Lower->tempf(yu,v,xu::accu)
      |Stdl_total_ordering_result_t.Equal->tempf(yu,yv,xu::accu)
      |Stdl_total_ordering_result_t.Greater->tempf(u,yv,xv::accu)
      ) in
      tempf(ox,oy,[]);;
  
  let length_preserving_merge (cmpr:'a Stdl_total_ordering_t.t) ox oy=
      let rec tempf=(function (u,v,accu)->
        if u=[] then (List.rev_append(accu)(v)) else
        if v=[] then (List.rev_append(accu)(u)) else
        let xu=List.hd(u) and yu=List.tl(u) 
        and xv=List.hd(v) and yv=List.tl(v) in
      match cmpr(xu)(xv) with
        Stdl_total_ordering_result_t.Lower->tempf(yu,v,xu::accu)
      |Stdl_total_ordering_result_t.Equal->tempf(yu,yv,xu::xv::accu)
      |Stdl_total_ordering_result_t.Greater->tempf(u,yv,xv::accu)
      ) in
      tempf(ox,oy,[]);;    
  
  let setminus (cmpr:'a Stdl_total_ordering_t.t) ox oy=
      let rec tempf=
      (function (u,v,accu)->
        if u=[] then (List.rev(accu)) else
        if v=[] then (List.rev_append(accu)(u)) else
        let xu=List.hd(u) and yu=List.tl(u) 
        and xv=List.hd(v) and yv=List.tl(v) in
        match cmpr(xu)(xv) with
           Stdl_total_ordering_result_t.Lower->tempf(yu,v,xu::accu)
          |Stdl_total_ordering_result_t.Equal->tempf(yu,yv,accu)
          |Stdl_total_ordering_result_t.Greater->tempf(u,yv,accu)
     ) in
     tempf(ox,oy,[]);;
  
  let rec sort (cmpr:'a Stdl_total_ordering_t.t) x=
    if List.length(x)<2
    then x
    else let temp1=Stdl_partition_list.split_in_half(x) in
         let y1=sort(cmpr)(fst temp1)
         and y2=sort(cmpr)(snd temp1) in
         merge cmpr y1 y2;;      

  let rec length_preserving_sort (cmpr:'a Stdl_total_ordering_t.t) x=
         if List.length(x)<2
         then x
         else let temp1=Stdl_partition_list.split_in_half(x) in
              let y1=length_preserving_sort(cmpr)(fst temp1)
              and y2=length_preserving_sort(cmpr)(snd temp1) in
              length_preserving_merge cmpr y1 y2;;       
  
  let rec helper_for_max (cmpr:'a Stdl_total_ordering_t.t) (current_max,to_be_treated) =
    match to_be_treated with 
     [] -> current_max
    | elt :: others ->
    match cmpr current_max elt with
    Stdl_total_ordering_result_t.Greater
   |Stdl_total_ordering_result_t.Equal->helper_for_max cmpr (current_max,others)
   |Stdl_total_ordering_result_t.Lower->helper_for_max cmpr (elt,others) ;; 


  let max (cmpr:'a Stdl_total_ordering_t.t) = function 
       [] -> raise(The_empty_set_has_no_max)
      |elt :: others -> helper_for_max cmpr (elt,others) ;; 


  let rec helper_for_min (cmpr:'a Stdl_total_ordering_t.t) (current_min,to_be_treated) =
    match to_be_treated with 
     [] -> current_min
    | elt :: others ->
    match cmpr current_min elt with
    Stdl_total_ordering_result_t.Lower
   |Stdl_total_ordering_result_t.Equal->helper_for_min cmpr (current_min,others)
   |Stdl_total_ordering_result_t.Greater->helper_for_min cmpr (elt,others) ;; 


  let min (cmpr:'a Stdl_total_ordering_t.t) = function 
       [] -> raise(The_empty_set_has_no_min)
      |elt :: others -> helper_for_min cmpr (elt,others) ;; 

  let is_included_in (cmpr:'a Stdl_total_ordering_t.t) ox oy=
         let rec tempf=(function (u,v)->
           if u=[] then true else
           if v=[] then false else
           let xu=List.hd(u) and yu=List.tl(u) 
           and xv=List.hd(v) and yv=List.tl(v) in
           match cmpr(xu)(xv) with
             Stdl_total_ordering_result_t.Lower->false
           |Stdl_total_ordering_result_t.Equal->tempf(yu,yv)
           |Stdl_total_ordering_result_t.Greater->tempf(u,yv)
         ) in
         tempf(ox,oy);;       
  
  let helper1_for_minimal_elements_selection (cmpr:'a Stdl_total_ordering_t.t)  
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
       
  let helper2_for_minimal_elements_selection (cmpr:'a Stdl_total_ordering_t.t)  =
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
    
  let does_not_intersect (cmpr:'a Stdl_total_ordering_t.t) ox oy=
      let rec tempf=(function (u,v)->
          if (u=[])||(v=[]) then true else
          let xu=List.hd(u) and yu=List.tl(u) 
          and xv=List.hd(v) and yv=List.tl(v) in
          match cmpr(xu)(xv) with
            Stdl_total_ordering_result_t.Lower->tempf(yu,v)
          |Stdl_total_ordering_result_t.Equal->false
          |Stdl_total_ordering_result_t.Greater->tempf(u,yv)
      ) in
      tempf(ox,oy);;     
      
  let fold_intersect cmpr=function
     []->raise(Empty_intersection_undefined)
    |a::b->List.fold_left(intersect cmpr)(a)(b);;

  let fold_merge cmpr l=
     let rec tempf=(function
        (already_treated,to_be_treated)->match to_be_treated with 
        []->already_treated
        |a::b->tempf(merge cmpr a already_treated,b)
     ) in 
     tempf([],l);;  

  let insert cmpr x oy=merge cmpr [x] oy;; 
  
  let intersects cmpr ox oy = not(does_not_intersect cmpr ox oy);;

  let mem (cmpr:'a Stdl_total_ordering_t.t) x ol=
     let rec tempf=(function
      []->false
      |a::others->match cmpr(x)(a) with
         Stdl_total_ordering_result_t.Lower->false
         |Stdl_total_ordering_result_t.Equal->true
         |Stdl_total_ordering_result_t.Greater->tempf others
     )  in
     tempf ol;;   

  let outsert cmpr x oy=setminus cmpr oy [x];;
  
  let safe_set cmpr ox=if is_increasing(cmpr)(ox) 
                       then ox 
                       else sort cmpr ox;;   

  let symmetric_difference cmpr x y =
    merge cmpr (setminus cmpr x y) (setminus cmpr y x) ;;                     

  end;;
  
  module Forget_order_After = struct 

  let sort cmpr x= Private.sort cmpr x;;

  end ;;  
  
  let does_not_intersect cmpr ox oy = 
     Private.does_not_intersect cmpr (Private.to_list ox) (Private.to_list oy);;
  
  let empty_set = Private.unsafe_of_list [] ;;

  
  let filter f ox = Private.unsafe_of_list(List.filter f (Private.to_list ox));;

  let filter_map f ox = List.filter_map f (Private.to_list ox);;

  let fold_intersect cmpr oll = 
     Private.unsafe_of_list(Private.fold_intersect cmpr (Stdl_image.image Private.to_list oll));;   
  
  let fold_merge cmpr oll = 
     Private.unsafe_of_list(Private.fold_merge cmpr (Stdl_image.image Private.to_list oll));;     
  
  let image f ox = Stdl_image.image f (Private.to_list ox) ;;   

  let insert cmpr x oy=Private.unsafe_of_list(Private.insert cmpr x (Private.to_list oy));; 
  
  let intersect cmpr ox oy= 
  Private.unsafe_of_list(Private.intersect cmpr (Private.to_list ox) (Private.to_list oy));;
  
  let intersects cmpr ox oy = 
    Private.intersects cmpr (Private.to_list ox) (Private.to_list oy);;
  let is_included_in cmpr ox oy = 
    Private.is_included_in cmpr (Private.to_list ox) (Private.to_list oy);;

  let length ox = List.length (Private.to_list ox) ;;

  let length_preserving_sort 
     cmpr ox = Private.unsafe_of_list(Private.length_preserving_sort cmpr (Private.to_list ox) );;

  let max cmpr ox = Private.max cmpr (Private.to_list ox);; 
  let mem cmpr x oy= Private.mem cmpr x (Private.to_list oy);;   

  let merge cmpr ox oy= 
  Private.unsafe_of_list(Private.merge cmpr (Private.to_list ox) (Private.to_list oy));;
  
  
  let min cmpr ox = Private.min cmpr (Private.to_list ox);; 

  let outsert cmpr x oy=Private.unsafe_of_list(Private.outsert cmpr x (Private.to_list oy));; 
  
  let safe_set cmpr x=Private.unsafe_of_list(Private.safe_set cmpr x);;
  
  let select_minimal_elements_for_inclusion cmpr oll= 
   let temp = Stdl_image.image Private.to_list oll in 
   Stdl_image.image Private.unsafe_of_list (Private.select_minimal_elements_for_inclusion cmpr temp);;
  
  let setminus cmpr ox oy= 
  Private.unsafe_of_list(Private.setminus cmpr (Private.to_list ox) (Private.to_list oy));;
  
  let singleton x = Private.unsafe_of_list [x] ;;

  let sort cmpr x= Private.unsafe_of_list(Private.sort cmpr x);;

  let symmetric_difference cmpr ox oy =
    Private.unsafe_of_list(Private.symmetric_difference cmpr (Private.to_list ox) (Private.to_list oy));;

  let tl ox = Private.unsafe_of_list(List.tl (Private.to_list ox));;   

  let to_list = Private.to_list ;; 
  

end;;






module Stdl_hurried=struct

(*

#use"lib/hurried.ml";;

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
let rec helper_for_reaggregation 
      decomposer (treated,current_level,incomplete_item,to_be_treated) = 
    match to_be_treated with 
    [] -> List.rev((current_level,List.rev incomplete_item)::treated)  
    |pair :: other_pairs ->
        let (new_level,other_data) = decomposer pair in 
        if new_level = current_level 
        then helper_for_reaggregation 
        decomposer (treated,current_level,
                     other_data::incomplete_item,other_pairs)
        else helper_for_reaggregation 
        decomposer ((current_level,List.rev incomplete_item)::treated,
                      new_level,[other_data],other_pairs) ;;            

let reaggregate decomposer l = 
  match l with 
  [] -> []
  |pair :: other_pairs ->
    let (new_level,other_data) = decomposer pair in 
    helper_for_reaggregation 
    decomposer ([],new_level,[other_data],other_pairs) ;;

(*

reaggregate (fun x->(x/10,x mod 10)) (Int_range.range 1 50) ;; 

*)    


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

let reaggregate = Private.reaggregate ;;

end;;






module Stdl_max=struct

(*

#use"lib/max.ml";;

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
   let temp1=List.filter_map (function 
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






module Stdl_total_ordering=struct

(*

#use"lib/Ordered_Lists/total_ordering.ml";;

*)


module Private = struct
let leq (computer:'a Stdl_total_ordering_t.t) x y=
   let v=computer(x)(y) in
   (v=Stdl_total_ordering_result_t.Lower)||(v=Stdl_total_ordering_result_t.Equal);;
   
 let lt (computer:'a Stdl_total_ordering_t.t) x y=(computer(x)(y)=Stdl_total_ordering_result_t.Lower);;   
 
 let geq (computer:'a Stdl_total_ordering_t.t) x y=
   let v=computer(x)(y) in
   (v=Stdl_total_ordering_result_t.Lower)||(v=Stdl_total_ordering_result_t.Equal);;
   
 let gt (computer:'a Stdl_total_ordering_t.t) x y=(computer(x)(y)=Stdl_total_ordering_result_t.Greater);;   
 
 let from_lt f=
   let temp1=(fun x y->
     if f(x)(y)
     then Stdl_total_ordering_result_t.Lower
     else if f(y)(x)
          then Stdl_total_ordering_result_t.Greater
          else Stdl_total_ordering_result_t.Equal
   ) in
   (temp1:'a Stdl_total_ordering_t.t);;
 
 let standard_completion f g=
  let answer=(fun x y->
   if f(y)(x)
   then Stdl_total_ordering_result_t.Greater
   else if f(x)(y)
        then Stdl_total_ordering_result_t.Lower
        else if g(x)(y)
             then Stdl_total_ordering_result_t.Equal
             else if x<y
                  then Stdl_total_ordering_result_t.Lower
                  else Stdl_total_ordering_result_t.Greater
  ) in
  (answer: 'a Stdl_total_ordering_t.t);;
 
 let standard=((fun x y->
    if x=y
    then Stdl_total_ordering_result_t.Equal
    else if x<y
         then Stdl_total_ordering_result_t.Lower
         else Stdl_total_ordering_result_t.Greater
 ): 'a Stdl_total_ordering_t.t);;
 
let standard2=((fun (x1,y1) (x2,y2)->
    let t1=standard x1 x2 in 
    if t1<> Stdl_total_ordering_result_t.Equal 
    then t1
    else standard y1 y2
 ): ('a * 'b) Stdl_total_ordering_t.t);;

 let completion f (g:'a Stdl_total_ordering_t.t)=
  let answer=(fun x y->
   if f(y)(x)
   then Stdl_total_ordering_result_t.Greater
   else if f(x)(y)
        then Stdl_total_ordering_result_t.Lower
         else g(x)(y)
  ) in
  (answer: 'a Stdl_total_ordering_t.t);;
 
let combine=((fun ~tried_first ~tried_second->
  (fun x y->
   let first_trial = tried_first x y in 
   if first_trial <> Stdl_total_ordering_result_t.Equal 
   then first_trial
   else tried_second x y
  ) ): 
    tried_first:('a Stdl_total_ordering_t.t) -> tried_second:('a Stdl_total_ordering_t.t) -> ('a Stdl_total_ordering_t.t)
  );;

 let product (f:'a Stdl_total_ordering_t.t) (g:'b Stdl_total_ordering_t.t)=
  ((fun (x1,y1) (x2,y2)->
     let t=f(x1)(x2) in
     if t<>Stdl_total_ordering_result_t.Equal 
     then t
     else g y1 y2
 ): ('a*'b) Stdl_total_ordering_t.t);;
 
 let triple_product (f:'a Stdl_total_ordering_t.t) (g:'b Stdl_total_ordering_t.t) (h:'c Stdl_total_ordering_t.t)=
  ((fun (x1,y1,z1) (x2,y2,z2)->
     let tx=f(x1)(x2) in
     if tx<>Stdl_total_ordering_result_t.Equal 
     then tx
     else let ty=g(y1)(y2) in
          if ty<>Stdl_total_ordering_result_t.Equal 
          then ty
          else h z1 z2
 ): ('a*'b*'c) Stdl_total_ordering_t.t);;

 let quadruple_product (f:'a Stdl_total_ordering_t.t) (g:'b Stdl_total_ordering_t.t) (h:'c Stdl_total_ordering_t.t) (i:'d Stdl_total_ordering_t.t)=
  ((fun (x1,y1,z1,t1) (x2,y2,z2,t2)->
     let tx=f(x1)(x2) in
     if tx<>Stdl_total_ordering_result_t.Equal 
     then tx
     else let ty=g(y1)(y2) in
          if ty<>Stdl_total_ordering_result_t.Equal 
          then ty
          else let tz=h(z1)(z2) in
               if tz<>Stdl_total_ordering_result_t.Equal 
               then tz
               else i t1 t2
 ): ('a*'b*'c*'d) Stdl_total_ordering_t.t);;
 
 let lex_compare (f:'a Stdl_total_ordering_t.t)=
  let rec tempf=(
    fun l1 l2->
     match l1 with 
     []->(if l2=[] then Stdl_total_ordering_result_t.Equal else Stdl_total_ordering_result_t.Lower)
     |a1::b1->
      (
        match l2 with 
        []->Stdl_total_ordering_result_t.Greater
        |a2::b2->
          let t=f(a1)(a2) in
           if t<>Stdl_total_ordering_result_t.Equal then t else
           tempf b1 b2
      )) in
     (tempf:>( ('a list) Stdl_total_ordering_t.t));;
 


let silex_compare (f:'a Stdl_total_ordering_t.t)=
  let tempf=(
    fun l1 l2->
     let t=standard(List.length l1)(List.length l2) in
     if t<>Stdl_total_ordering_result_t.Equal then t else
     lex_compare f l1 l2
  ) in
   (tempf:>( ('a list) Stdl_total_ordering_t.t));;
 

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

let min (f:'a Stdl_total_ordering_t.t)=function
 []->failwith("Min of the empty set is undefined")
 |a::b->
   let rec tempf=(fun
    (candidate,l)->match l with
      []->candidate
      |c::peurrest->if f(c)(candidate)=Stdl_total_ordering_result_t.Lower
                    then tempf(c,peurrest)
                    else tempf(candidate,peurrest)
   ) in
   tempf(a,b);;

let max (f:'a Stdl_total_ordering_t.t)=function
 []->failwith("Max of the empty set is undefined")
 |a::b->
   let rec tempf=(fun
    (candidate,l)->match l with
      []->candidate
      |c::peurrest->if f(c)(candidate)=Stdl_total_ordering_result_t.Greater
                    then tempf(c,peurrest)
                    else tempf(candidate,peurrest)
   ) in
   tempf(a,b);;
   
let minimize_it_with_care (cf:'a Stdl_total_ordering_t.t) 
   f=function
[]->failwith("careful min on empty set undefined")
|x::y->
 let rec minimize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                let howl=cf(va)(current_value) in
                if howl=Stdl_total_ordering_result_t.Lower
				then minimize_it_with_care0([a],va,peurrest)
				else if howl=Stdl_total_ordering_result_t.Equal
				     then minimize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else minimize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 minimize_it_with_care0([x],f(x),y);;


let maximize_it_with_care (cf:'a Stdl_total_ordering_t.t) 
   f=function
[]->failwith("careful max on empty set undefined")
|x::y->
 let rec maximize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                let howl=cf(va)(current_value) in
                if howl=Stdl_total_ordering_result_t.Greater
				then maximize_it_with_care0([a],va,peurrest)
				else if howl=Stdl_total_ordering_result_t.Equal
				     then maximize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else maximize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 maximize_it_with_care0([x],f(x),y);;

let modify_locally (f:'a Stdl_total_ordering_t.t) l=
  let big_m=max(f)(l) in
  let tempf=(fun x y->
    if List.mem(x)(l)
    then if List.mem(y)(l)
         then if x=y
              then Stdl_total_ordering_result_t.Equal
              else (from_list l x y)
         else f big_m y
    else if List.mem(y)(l)
         then f x big_m
         else f x y
  
  ) in
  (tempf:>( 'a Stdl_total_ordering_t.t));;

let list_for_dictionary_order=
  [97; 65; 98; 66; 99; 67; 100; 68; 101; 69; 102; 70; 103; 71; 104; 72; 105;
  73; 106; 74; 107; 75; 108; 76; 109; 77; 110; 78; 111; 79; 112; 80; 113; 81;
  114; 82; 115; 83; 116; 84; 117; 85; 118; 86; 119; 87; 120; 88; 121; 89;
  122; 90; 91; 92; 93; 94; 95; 96];;  

let reindexer_for_dictionary_order i=
    if (i<65)||(i>122) 
    then i 
    else 64+(Stdl_list_again.index_of_in i list_for_dictionary_order);;


let for_characters=let tempf=(fun x y->
  standard 
        (reindexer_for_dictionary_order(int_of_char x))
        (reindexer_for_dictionary_order(int_of_char y))
  ) in (tempf:>char Stdl_total_ordering_t.t);;

let for_integers=let tempf=(fun (x:int) (y:int)-> standard x y 
    ) in (tempf:>int Stdl_total_ordering_t.t);;  

let lex_for_strings=
    ((fun s1 s2->
      let m1=String.length s1
      and m2=String.length s2
      in
      let m=Stdlib.min(m1)(m2) in
      match List.find_opt (fun j->(String.get s1 j)<>(String.get s2 j)) (Stdl_int_range.range 0 (m-1)) with
      None->standard m1 m2
      |Some(j)->for_characters (String.get s1 j) (String.get s2 j) 
    ) : string Stdl_total_ordering_t.t);;

let silex_for_strings=
      ((fun s1 s2->
        let m1=String.length s1
        and m2=String.length s2
        in
        let first_try=standard(m1)(m2) in
        if first_try<>Stdl_total_ordering_result_t.Equal
        then first_try
        else lex_for_strings s1 s2
      ) : string Stdl_total_ordering_t.t);;    

      let factor (x,y)=
      let rec factor0=(fun
         (graet,da_ober1,da_ober2)->
         if (da_ober1=[])||(da_ober2=[])
         then (List.rev graet,da_ober1,da_ober2)
         else let (a1,peurrest1)=Stdl_list_again.head_with_tail da_ober1
              and (a2,peurrest2)=Stdl_list_again.head_with_tail da_ober2 in
              if a1=a2
              then factor0(a1::graet,peurrest1,peurrest2)
              else (List.rev graet,da_ober1,da_ober2)
      ) in
      factor0([],x,y);;      

let lex_for_string_lists=
  ((fun l1 l2->
      let (_,left_part,right_part)= factor (l1,l2) in
      if left_part=[] 
      then (if right_part=[] 
           then Stdl_total_ordering_result_t.Equal 
           else Stdl_total_ordering_result_t.Lower)
      else if right_part=[] 
           then Stdl_total_ordering_result_t.Greater 
           else lex_for_strings (List.hd left_part) (List.hd right_part)  
  ) : (string list) Stdl_total_ordering_t.t);;

let for_longest_match=  
    ((fun s1 s2->
      let m1=String.length s1
      and m2=String.length s2 in
      if (
          if m1>m2 then false else
          (String.sub s2 0 m1)=s1
      ) then Stdl_total_ordering_result_t.Greater else
      if (
          if m2>m1 then false else
          (String.sub s1 0 m2)=s2
      ) then Stdl_total_ordering_result_t.Lower else
      lex_for_strings s1 s2
     ): string Stdl_total_ordering_t.t);;


let for_longest_match_pairs=  
((fun (s1,v1) (s2,v2)->
  let first_try=silex_for_strings(s2)(s1) in
  if first_try<>Stdl_total_ordering_result_t.Equal 
  then first_try
  else standard v1 v2
 ): (string*'b) Stdl_total_ordering_t.t);;
 
let from_snd (f:'b Stdl_total_ordering_t.t)=((fun (x1,y1) (x2,y2)->
  let first_try=f y1 y2 in
  if first_try<>Stdl_total_ordering_result_t.Equal 
  then first_try
  else standard x1 x2
): ('a*'b) Stdl_total_ordering_t.t );;

let cardinality_then_diameter =((fun l1 l2->
  let first_try=standard (List.length l1) (List.length l2) in
  if first_try<>Stdl_total_ordering_result_t.Equal 
  then first_try
  else 
  let diam1 = List.hd(List.rev l1) - (List.hd l1)
  and diam2 = List.hd(List.rev l2) - (List.hd l2) in 
  let second_try=standard diam1 diam2 in
  if second_try<>Stdl_total_ordering_result_t.Equal 
  then second_try
  else lex_compare for_integers l1 l2
): (int list) Stdl_total_ordering_t.t );;

let index_in_list x ll=
  let rec sub_f=
  (function (j,l)->match l with
  []->(-1)      
  |u::v->if u=x then j else sub_f(j+1,v)) in
  sub_f(1,ll);;


let from_list (l:'a list)=((fun x1 x2->
   for_integers (index_in_list x1 l) (index_in_list x2 l)   
) : 'a Stdl_total_ordering_t.t) ;; 

end ;;

let cardinality_then_diameter = Private.cardinality_then_diameter ;;
let for_characters = Private.for_characters ;;
let for_integers = Private.for_integers ;;
let from_list = Private.from_list ;; 
let lex_compare = Private.lex_compare ;;
let lex_for_strings = Private.lex_for_strings ;;
let product = Private.product ;;
let quadruple_product = Private.quadruple_product ;;
let silex_compare = Private.silex_compare ;;
let silex_for_strings = Private.silex_for_strings ;;
let silex_for_intlists = Private.silex_compare for_integers ;;
let standard = Private.standard ;;
let standard2 = Private.standard2 ;;
let triple_product = Private.triple_product ;;
 
           

end;;






module Stdl_encoded_string_t=struct

(* 

#use"lib/Ocaml_analysis/Concrete_ocaml_objects/encoded_string_t.ml";;


*)


type t= E of string;;



end;;






module Stdl_concrete_object_t=struct

(* 

#use"lib/Ocaml_analysis/Concrete_ocaml_objects/concrete_object_t.ml";;


*)

type t= 
    Int of int 
   |String of Stdl_encoded_string_t.t 
   |Uple of t list
   |List of t list
   |Array of t list
   |Record of (string*t) list
   |Variant of string*(t list);;



end;;






module Stdl_concrete_object=struct

(* 

#use"lib/Ocaml_analysis/Concrete_ocaml_objects/concrete_object.ml";;

*)

module Exn = struct

   exception Get_record_absent_key_exn of string;;
   exception Get_record_bad_type_exn of Stdl_concrete_object_t.t;;
   exception Get_pair_exn of Stdl_concrete_object_t.t;;
   exception Unwrap_array_exn of Stdl_concrete_object_t.t;;
   exception Unwrap_int_exn of Stdl_concrete_object_t.t;;
   exception Unwrap_list_exn of Stdl_concrete_object_t.t;;
   exception Unwrap_string_exn of Stdl_concrete_object_t.t;;
   exception Wrap_lonely_variant_exn;;
   exception Unwrap_lonely_variant_exn of Stdl_concrete_object_t.t;;
   
   exception Uple_too_big of Stdl_concrete_object_t.t;;
   exception Uple_too_small of Stdl_concrete_object_t.t;;
   exception Unwrap_bounded_uple_exn of Stdl_concrete_object_t.t;;
   
   exception Variant_too_big of Stdl_concrete_object_t.t;;
   exception Variant_too_small of Stdl_concrete_object_t.t;;
   exception Unwrap_bounded_variant_exn of Stdl_concrete_object_t.t;;
   
   end;;
   
   let wrap_encoded_string encoded_s=Stdl_concrete_object_t.String(encoded_s);;
   
   
   let unwrap_list ccrt_obj=
      match ccrt_obj with 
      Stdl_concrete_object_t.List(l)->l 
      |Stdl_concrete_object_t.Int(_)
         |Stdl_concrete_object_t.String (_)
         |Stdl_concrete_object_t.Uple (_)
         |Stdl_concrete_object_t.Array (_)
         |Stdl_concrete_object_t.Record (_)   
         |Stdl_concrete_object_t.Variant (_,_)  
      ->raise(Exn.Unwrap_list_exn(ccrt_obj));;
   
   let unwrap_array ccrt_obj=
      match ccrt_obj with 
      Stdl_concrete_object_t.Array(l)->Array.of_list l 
      |Stdl_concrete_object_t.Int(_)
         |Stdl_concrete_object_t.String (_)
         |Stdl_concrete_object_t.Uple (_)
         |Stdl_concrete_object_t.List (_)
         |Stdl_concrete_object_t.Record (_)  
         |Stdl_concrete_object_t.Variant (_,_)   
      ->raise(Exn.Unwrap_array_exn(ccrt_obj));;
   
   let get_record ccrt_obj field =
      match ccrt_obj with 
      Stdl_concrete_object_t.Record(l)->
           (try List.assoc field l with 
           _ ->raise(Exn.Get_record_absent_key_exn(field)))
           |Stdl_concrete_object_t.Int(_)
           |Stdl_concrete_object_t.String (_)
           |Stdl_concrete_object_t.Uple (_)
           |Stdl_concrete_object_t.List (_)
           |Stdl_concrete_object_t.Array (_) 
           |Stdl_concrete_object_t.Variant (_,_)    
      ->raise(Exn.Get_record_bad_type_exn(ccrt_obj));;
   
   let unwrap_bounded_uple ccrt_obj=
     match ccrt_obj with 
      Stdl_concrete_object_t.Uple(l)->
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
      |Stdl_concrete_object_t.Int(_)
      |Stdl_concrete_object_t.String (_)
      |Stdl_concrete_object_t.List (_)
      |Stdl_concrete_object_t.Array (_)
      |Stdl_concrete_object_t.Record (_)
      |Stdl_concrete_object_t.Variant (_,_)
      -> raise(Exn.Unwrap_bounded_uple_exn(ccrt_obj));;
   
   
   
   let unwrap_bounded_variant ccrt_obj=
     match ccrt_obj with 
      Stdl_concrete_object_t.Variant(constructor,l)->
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
         |Stdl_concrete_object_t.Int(_)
         |Stdl_concrete_object_t.String (_)
         |Stdl_concrete_object_t.Uple (_)
         |Stdl_concrete_object_t.List (_)
         |Stdl_concrete_object_t.Array (_)
         |Stdl_concrete_object_t.Record (_)   
            -> raise(Exn.Unwrap_bounded_variant_exn(ccrt_obj));;
   
   
   let wrap_lonely_variant l_pairs unwrapped=
      match List.find_opt(fun (key,_vaal)->key=unwrapped) l_pairs with
         None->raise(Exn.Wrap_lonely_variant_exn)
        |Some(_,constructor)->Stdl_concrete_object_t.Variant(constructor,[]) ;;
   
   
   let unwrap_lonely_variant l_pairs ccrt_obj=
      match ccrt_obj with 
      Stdl_concrete_object_t.Variant(constructor,l)->
         if  l<>[] then raise(Exn.Unwrap_lonely_variant_exn(ccrt_obj)) else 
         (match List.find_opt(fun (_,key)->key=constructor) l_pairs with
         None->raise(Exn.Unwrap_lonely_variant_exn(ccrt_obj))
        |Some(vaal,_)->vaal) 
      |Stdl_concrete_object_t.Int(_)
      |Stdl_concrete_object_t.String (_)
      |Stdl_concrete_object_t.Uple (_)
      |Stdl_concrete_object_t.List (_)
      |Stdl_concrete_object_t.Array (_)
      |Stdl_concrete_object_t.Record (_)
      ->raise(Exn.Unwrap_lonely_variant_exn(ccrt_obj));;
   

end;;






module Stdl_substring=struct

(*

#use"lib/substring.ml";;

*)


exception Completions_on_each_side_exn of string ;; 

module Private = struct 

   let leftmost_index_of_in_from_opt x y i=
   let lx=String.length(x) in
   let tester=(function j->(String.sub y j lx)=x) in
   match Stdl_int_range.find_opt tester (i-1) (String.length(y)-lx) with
      None->None
     |Some(k)->Some(k+1);;

let occurrences_of_in x y=
   let n=String.length y in
   let rec tempf=(fun (j,accu)->
      if j>n then List.rev(accu) else
      match leftmost_index_of_in_from_opt x y j with
      None -> List.rev(accu)
      | Some k -> tempf(k+1,k::accu)
   )  in
   tempf (1,[]);;

let ranges_for_occurrences_of_in x y=
   let m=String.length x in
   let temp1 = occurrences_of_in x y in 
   Stdl_image.image (fun i->(i,i+m-1)) temp1;;  



let completions_in_on_each_side_of container contained =
  match leftmost_index_of_in_from_opt contained container 1 with 
  None -> raise(Completions_on_each_side_exn(contained))
  | (Some k) -> 
   let n = String.length container and m = String.length contained in 
   (
   String.sub container 0 (k-1),String.sub container (k+m-1) (n+1-(k+m))) ;;

(*

completions_on_each_side "123456789" "45" ;;

*)


end ;;    


let completions_in_on_each_side_of = Private.completions_in_on_each_side_of ;;

let decorated_occurrences_of_in x y =
   let ny = String.length y 
   and ranges = Private.ranges_for_occurrences_of_in x y in  
   let arranged_ranges = Stdl_image.image (
     fun (old_i,old_j)->
        (max(1)(old_i-5),min(ny)(old_j+150))
   ) ranges in 
   Stdl_image.image (fun (a,b)->String.sub y (a-1) (b-a+1)) arranged_ranges ;;

 
 let is_a_substring_located_at y x old_j =
    let j=old_j-1 in
    let ly=String.length(y) in
      if (String.length(x)<j+ly)||(j<0)
      then false
      else (String.sub x j ly)=y;;
      
  let is_a_substring_of x y=
   let lx=String.length(x) in
   let tester=(function j->(String.sub y j lx)=x) in
   Stdl_int_range.exists tester 0 (String.length(y)-lx);;               

  let leftmost_index_of_in_from_opt = Private.leftmost_index_of_in_from_opt ;; 

   
  let rightmost_index_of_in_opt x y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) 
      and temp1=List.rev(Stdl_int_range.range(0)(String.length(y)-lx)) in
      try Some((List.find tester temp1)+1) with
      _->None;;




let leftmost_index_of_pattern_among_in_from_opt patterns whole_string start_idx=  
    let n=String.length(whole_string) in
    let temp1=Stdl_int_range.index_everything patterns in 
    let tester =(fun idx->List.find_map (
         fun (patt_nbr,patt)->
           if is_a_substring_located_at patt whole_string idx 
           then Some(patt_nbr,idx)
           else None
       ) temp1) in
    List.find_map tester (Stdl_int_range.range start_idx n);;          
      
(*

leftmost_index_of_pattern_among_in_from ["uv";"abc";"abcde"] "123abcde90" 1;;

*)

let occurrences_of_in = Private.occurrences_of_in ;;

let ranges_for_occurrences_of_in = Private.ranges_for_occurrences_of_in ;;   

end;;






module Stdl_cull_string=struct

(*

#use"lib/cull_string.ml";;

*)

exception Absent_beginning_marker of string;;
exception Absent_ending_marker of string;; 
exception Beginning_failure;;
exception Ending_failure;;   
exception Two_sided_cutting_exn of int*int*int;;

module Private = struct 

   let interval s a b=String.sub s (a-1) (b-a+1);;
   let leftmost_index_of_in_from_opt x y i=
   let lx=String.length(x) in
   let tester=(function j->(String.sub y j lx)=x) in
   match Stdl_int_range.find_opt tester (i-1) (String.length(y)-lx) with
      None->None
     |Some(k)->Some(k+1);;

    

let tripartition_using_markers (bm,em) text =
      let n = String.length text in 
      match leftmost_index_of_in_from_opt bm text 1 with 
      None -> raise(Absent_beginning_marker(bm)) 
      |Some i1 ->
      let j1=i1+(String.length bm)-1 in
      match leftmost_index_of_in_from_opt em text (j1+1) with 
      None -> raise(Absent_ending_marker(em)) 
      |Some i2 ->
      let j2=i2+(String.length em)-1 in
      (interval text 1 (i1-1),
       interval text (j1+1) (i2-1),
       interval text (j2+1) n) ;; 

(*

tripartition_using_markers ("backdoor","man") "123backdoor45man678" ;;

*)

 let trim_spaces s=
   let n=String.length s in
   let opt1=List.find_opt(fun j->not(List.mem(String.get s (j-1)) [' ';'\r';'\t';'\n']))(Stdl_int_range.range 1 n) in
   if opt1=None then "" else
   let i1=Option.get opt1 in
   let k1=List.find(fun j->not(List.mem(String.get s (n-j)) [' ';'\r';'\t';'\n']))(Stdl_int_range.range 1 n) in 
   let j1=(n+1)-k1 in
   interval s i1 j1;;


 let split_wrt_rightmost s c=
   let i=(try String.rindex(s)(c) with _->(-1)) in
   if i<0
   then ("",s)
   else (String.sub s 0 i,String.sub s (i+1) ((String.length s)-i-1) );;  

  let beginning k s=
   if k<1 then "" else
   let n=String.length(s) in
   if (k>n)
   then raise(Beginning_failure)
   else String.sub s 0 k;;
   
 let ending k s=
   if k<1 then "" else
   let n=String.length(s) in
   if (k>n)
   then raise(Ending_failure)
   else String.sub s (n-k) k;; 

 let cobeginning k s=ending (String.length(s)-k) s;; 
   
 

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
 

end ;;   

let before_and_after w x=
  let j_opt=Stdl_substring.leftmost_index_of_in_from_opt(w)(x) 1 in
  if j_opt=None then None else 
   let j = Option.get j_opt in 
   Some(  Private.beginning (j-1) x,
    Private.cobeginning (j+String.length(w)-1) x);;

(*

before_and_after "45" "123456789" ;;

*)

let after_rightmost s c=snd(Private.split_wrt_rightmost s c);;

let before_rightmost s c=fst(Private.split_wrt_rightmost s c);;

let before_rightmost_possibly_all s c=
   let i=(try String.rindex(s)(c) with _->(-1)) in
   if i<0
   then s
   else String.sub s 0 i;;



let beginning = Private.beginning;;
   

let between_markers markers text=
  let (_,middle,_) = Private.tripartition_using_markers markers text in 
  middle;; 
 
let between_markers_opt p s=
   try Some(between_markers p s) with _->None;; 
   
(*

between_markers ("aaa","bb") "123aaa45bb678";;

*)     

 let cobeginning k s=Private.cobeginning k s;; 
 
 let coending k s=beginning (String.length(s)-k) s;; 

 let ending = Private.ending ;;

let interval =Private.interval;;

let neighborhood_with_center_and_size s i d=
   let a=max(1)(i-d)
   and b=min(String.length s)(i+d) in
   interval s a b;;

 


let remove_chars_in_set_on_the_left l s=
      let n=String.length s in
      match List.find_opt(fun j->
          not(List.mem (String.get s (j-1)) l)
      )(Stdl_int_range.range 1 n) with
      None->""
      |Some(d)->cobeginning (d-1) s;;

let remove_chars_in_set_on_the_right l s=
      let n=String.length s in
      match List.find_opt(fun j->
          not(List.mem (String.get s (n-j)) l)
      )(Stdl_int_range.range 1 n) with
      None->""
      |Some(d)->coending (d-1) s;;

let rec remove_suffixes_from_list_on_the_right str suffixes =
   match List.find_opt (fun suffix->String.ends_with str ~suffix) suffixes with 
   None -> str 
   |Some suff ->
     let shorter_str = Private.two_sided_cutting ("",suff) str in 
     remove_suffixes_from_list_on_the_right shorter_str suffixes ;;

(*

remove_suffixes_from_list_on_the_right "A. I. V. C 2642.\\newline\n\n\\indent" 
 [" ";"\n";"\r";"\t";"\\newline";"\\linebreak";"\\indent";"\\noindent"];;

*)


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
     


let shortened_version max_length s = 
   let n = String.length s in 
   if n<=max_length 
   then s 
   else (beginning max_length s)^"(...)" ;;    

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
   let temp1 = List.filter test_idx (Stdl_int_range.range 1 n) in 
   let temp2 = Stdl_image.image (fun j->String.make 1 (String.get s (j-1))) temp1 in 
   let temp3 = String.concat "" temp2 in 
   Private.trim_spaces temp3 ;;

(*

shorten_blanks " \n 123\r \n45 \n\n6\n  7\t 89\n";;

*)    

let trim_spaces = Private.trim_spaces ;;

let trim_spaces_on_the_left =remove_chars_in_set_on_the_left [' ';'\t';'\r';'\n'];;

let trim_spaces_on_the_right = remove_chars_in_set_on_the_right [' ';'\t';'\r';'\n'] ;;

let trim_slashes_on_the_right =remove_chars_in_set_on_the_right ['/'];;
                 



let tripartition_using_markers = Private.tripartition_using_markers ;;

let two_sided_cutting = Private.two_sided_cutting ;;


 
   
let split_wrt_rightmost = Private.split_wrt_rightmost;;




  
             

end;;






module Stdl_tools_for_absolute_path=struct

(*

#use"lib/tools_for_absolute_path.ml";;

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
   _any_exn->raise(Too_many_double_points);;

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
  |_other->dir^s1;;

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






module Stdl_absolute_path=struct

(*

#use"lib/absolute_path.ml";;

*)

type t=AP of string;;

let of_string s=AP(Stdl_tools_for_absolute_path.of_string s);;


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
    
(* This is a registered printer : print_out *)

let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;

           

end;;






module Stdl_io=struct

(*

#use"lib/io.ml";;

*)


exception Open_in_exn of string ;;
exception Open_out_exn of string ;;
exception Dangerous_command_reading of string ;;

let max_size_for_reasonable_in_channel = ref(1000000);;

module Private = struct 

let make_filename_complete s=
  let home=Sys.getenv("HOME") in
  if s="" then Stdl_absolute_path.of_string(home) else
  let c=String.get s 0 in
  if c='/' then Stdl_absolute_path.of_string(s) else
  if c='~' then Stdl_absolute_path.of_string(home^(String.sub s 1 (String.length(s)-1))) else
  Stdl_absolute_path.of_string((Sys.getcwd ())^"/"^s);;

let open_in_locally x=try open_in_bin(x) with 
_->raise(Open_in_exn(x));;

let open_out_locally x=try open_out_bin(x) with 
_->raise(Open_out_exn(x));;  

let put_whole_content_of_file_in_buffer s=
  let x=Stdl_absolute_path.to_string(make_filename_complete(s)) in
  let janet=open_in_locally(x) in
  let n=in_channel_length(janet) in
  let b=Buffer.create(n) in
  let _=Buffer.add_channel(b)(janet)(n) in
  let _=close_in janet in
  b;;
  
  
type filename=string;;
  
let erase_file_and_fill_it_with_contents_of_buffer (fn:filename) b=
   let x=Stdl_absolute_path.to_string(make_filename_complete(fn)) in
  let john=open_out_locally(x) in
  (Buffer.output_buffer(john)(b);close_out john);;
  
let overwrite_with ap s=
   let fn=Stdl_absolute_path.to_string ap in
   let n=String.length(s) in
   let b=Buffer.create(n) in
   let _=Buffer.add_string b s in
   erase_file_and_fill_it_with_contents_of_buffer fn b;;
   
let read_whole_file ap=   
   let s=Stdl_absolute_path.to_string ap in
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






module Stdl_replace_inside=struct

(*

#use"lib/replace_inside.ml";;

*)

exception Ambiguity of string*int*int;;


module Private = struct 
 
  let substring_leftmost_index_from=(fun x y i0->
    let lx=String.length(x) and ly=String.length(y) in
    let rec tempf=(fun j->
      if j>ly-lx then (-1) else 
      if (String.sub y j lx)=x then j else (tempf(j+1))
    ) in
    tempf i0) ;;
(*

The my_global_replace function below is a replacement for Ocaml's Str.global_replace which has
the disadvantage of applying certain transforms to the replacement string.

*)

let single_char_special_case (single_c,b) s=
  let n=String.length(s) and counter=ref(0) in
  let temp1=Stdl_int_range.scale (
     fun j->let c=String.get s j in
            if c=single_c
            then let _=(counter:=(!counter)+1) in b
            else String.make 1 c 
  ) 0 (n-1) in
  (String.concat "" temp1,!counter);;

let global_replace_with_number_of_matches (a,b) s=
let n=String.length(s) and na=String.length(a) in
if na=1 then single_char_special_case (String.get a 0,b) s else
let indices=Stdl_substring.occurrences_of_in a s in
if indices=[] then (s,0) else
let delta_indices = Stdl_list_again.universal_delta_list indices in 
let opt_ambiguity=List.find_opt (fun (start1,start2)->start2<start1+na) delta_indices in 
if  opt_ambiguity<>None
then let (start1,start2)=Option.get opt_ambiguity in 
     raise(Ambiguity(a,start1,start2))
else  
let m=List.length indices in 
let lower_end=(fun j->if j=0 then 1 else List.nth indices (j-1)+na) 
and upper_end=(fun j->if j=m then n else (List.nth indices (j))-1) in 
let unchanged_intervals = Stdl_int_range.scale (fun j->(lower_end j,upper_end j)) 0 m in 
let unchanged_substrings=Stdl_image.image 
   (fun (x,y)->if x>y then "" else Stdl_cull_string.interval s x y) unchanged_intervals in
(String.concat b unchanged_substrings,m);;

let text_for_number_of_replacements k=
  if k = 0 then "No replacement made" else
  if k = 1 then "1 replacement made" else 
  (string_of_int k)^" replacements made" ;;   

let my_global_replace_with_raising display_number_of_matches (a,b) old_s  =
   let (new_s,count) = global_replace_with_number_of_matches (a,b) old_s in 
   let _ =(
      if display_number_of_matches 
      then print_string((text_for_number_of_replacements count)^" for "^a^" -> "^b^"\n"); 
           flush stdout 
   ) in 
   new_s ;; 

let my_global_replace_without_raising display_number_of_matches (a,b) old_s  =
   try my_global_replace_with_raising display_number_of_matches (a,b) old_s with 
   Ambiguity(_,_,_) -> old_s ;; 

let my_global_replace display_number_of_matches silent_on_ambiguity (a,b) old_s  = 
  if silent_on_ambiguity 
  then my_global_replace_without_raising display_number_of_matches (a,b) old_s 
  else my_global_replace_with_raising display_number_of_matches (a,b) old_s ;;   
  

(*  
my_global_replace ("ab","cd") "12345ab6ab78cd91234ab679";; 
my_global_replace ("ab","cd") "ab12345ab6ab78cd91234ab679";; 
my_global_replace ("ab","cd") "12345ab6ab78cd91234ab679ab";;
my_global_replace ("1111","") "abc1111111111def";;
my_global_replace ("ab","cd") "xyz";;
my_global_replace ("a","b") "1aa2";; 
my_global_replace ("uv","w") "1uvuv2";; 

*)  

let pair_for_commenting_or_uncommenting = 
  ("\n(*\n","\n*)\n") ;;

end ;;

let at_char_intervals_inside_text text l=
  if l=[] then text else
  let n=String.length text in
  let temp1=Stdl_list_again.universal_delta_list l 
  and ((i_first,_),_)=List.hd(l)
  and ((_i_last,j_last),rep_last)=List.hd(List.rev l) in
  let temp2=Stdl_image.image (fun (((_i1,j1),rep1),((i2,_j2),_rep2))->
      rep1^(String.sub text j1 (i2-j1-1))
  ) temp1 in
  let first_part=(String.sub text 0 (i_first-1))
  and last_part=rep_last^(String.sub text j_last (n-j_last)) in
  first_part^(String.concat "" temp2)^last_part;;

let at_char_intervals_inside_file 
  fn l=
   let s1=Stdl_io.read_whole_file fn in
   let s2=at_char_intervals_inside_text s1 l in
   Stdl_io.overwrite_with fn s2;;     

(*    

at_char_intervals_inside_text "12345678901234567890" [(3,5),"right";(12,17),"again"];;

*)         

let comment_out_between_markers_inside_text (bm,em) text=
     let (before,between,after) = Stdl_cull_string.tripartition_using_markers (bm,em) text in
     let (opener,closer) = Private.pair_for_commenting_or_uncommenting in 
     before^bm^opener^between^closer^em^after ;; 

let comment_out_between_markers_inside_file (bm,em) fn =
  let old_text=Stdl_io.read_whole_file fn in
  let new_text=comment_out_between_markers_inside_text (bm,em) old_text in
  Stdl_io.overwrite_with fn new_text;; 

let overwrite_and_dump_markers_inside_text ~overwriter:b (bm,em) text=
     if (bm,em)=("","") then b else
      let (before,_between,after) = Stdl_cull_string.tripartition_using_markers (bm,em) text in
      before^bm^b^em^after ;; 
     
let overwrite_and_dump_markers_inside_file 
~overwriter:b (bm,em)
   fn =
    let old_text=Stdl_io.read_whole_file fn in
    let new_text=overwrite_and_dump_markers_inside_text ~overwriter:b (bm,em) old_text in
    Stdl_io.overwrite_with fn new_text;;      
 
(* 


 overwrite_between_markers_inside_text
  (~overwriter:"456")
  ("aaa","bb")
   "123aaa5678bb78910" ;;    
   
overwrite_and_dump_markers_inside_text
  (~overwriter:"456")
  ("aaa","bb")
   "123aaa5678bb78910" ;;       
   
     
*)

let overwrite_at_intervals_inside_text replacings text=
  let n=String.length text
  and r=List.length replacings in
  let x_coord=(fun j->
    if j=1 then 1 else
    snd(fst(List.nth replacings ((j-3)/2)))+1
  ) and y_coord=(fun j->
   if j=2*r+1 then n else
    fst(fst(List.nth replacings ((j-1)/2)))-1
  ) in
  let xy_substring=(fun j->
    Stdl_cull_string.interval text (x_coord j) (y_coord j)
  ) in
  let all_parts=Stdl_int_range.scale (
    fun j->
      if (j mod 2)=1
      then xy_substring j
      else snd(List.nth replacings ((j-2)/2))
  ) 1 (2*r+1) in
  String.concat "" all_parts;;

(*

overwrite_at_intervals_inside_text
 [(7,12),"garfield";(23,24),"jack";(30,30),"gas"]
 "12345678901234567890123456789012345678901234567890";;
 

*)

let overwrite_at_intervals_inside_file replacings fn=
  let old_text=Stdl_io.read_whole_file fn in
  let new_text=overwrite_at_intervals_inside_text replacings old_text in
  Stdl_io.overwrite_with fn new_text;;  
  




let overwrite_between_markers_inside_text ~overwriter:b (bm,em) text=
     if (bm,em)=("","") then b else
     let (before,_between,after) = Stdl_cull_string.tripartition_using_markers (bm,em) text in
     before^bm^b^em^after ;; 
     
let overwrite_between_markers_inside_file 
   ~overwriter:b (bm,em)
   fn =
    let old_text=Stdl_io.read_whole_file fn in
    let new_text=overwrite_between_markers_inside_text ~overwriter:b (bm,em) old_text in
    Stdl_io.overwrite_with fn new_text;;      

let pair_for_commenting_or_uncommenting = 
     Private.pair_for_commenting_or_uncommenting ;;

let replace_inside_text ?(display_number_of_matches=true) ?(silent_on_ambiguity=false) (a,b) text=
  Private.my_global_replace display_number_of_matches silent_on_ambiguity (a,b) text ;;
 
let replace_several_inside_text ?(display_number_of_matches=false) ?(silent_on_ambiguity=false) l t=List.fold_left 
(fun s (a,b)->Private.my_global_replace display_number_of_matches silent_on_ambiguity (a,b) s ) t l;;  
 
let replace_inside_file ?(display_number_of_matches=true) ?(silent_on_ambiguity=false) (a,b) fn=
    let old_text=Stdl_io.read_whole_file fn in
    let la=String.length(a) in
    if List.exists (fun j->(String.sub old_text j la)=a) (Stdl_int_range.range 0 ((String.length old_text)-la))
    then let new_text=replace_inside_text ~display_number_of_matches ~silent_on_ambiguity (a,b) old_text in
         Stdl_io.overwrite_with fn new_text
    else ();; 


let replace_several_inside_file ?(display_number_of_matches=false) ?(silent_on_ambiguity=false) l fn=
    let old_text=Stdl_io.read_whole_file fn in
    let new_text=replace_several_inside_text ~display_number_of_matches ~silent_on_ambiguity l old_text  in
    Stdl_io.overwrite_with fn new_text;; 

let uncomment_between_markers_inside_text (bm,em) text=
      let (before,between,after) = Stdl_cull_string.tripartition_using_markers (bm,em) text in
      before^bm^(
       Stdl_cull_string.two_sided_cutting Private.pair_for_commenting_or_uncommenting  
      between)^em^after ;; 
 

 let uncomment_between_markers_inside_file (bm,em) fn =
   let old_text=Stdl_io.read_whole_file fn in
   let new_text=uncomment_between_markers_inside_text (bm,em) old_text in
   Stdl_io.overwrite_with fn new_text;; 





end;;






module Stdl_encoded_string=struct

(* 

#use"lib/Ocaml_analysis/Concrete_ocaml_objects/encoded_string.ml";;

This is a leaky abstraction, as witnessed by the "retrieve" and "store" functions above.


*)

exception Forbidden_substring;;
exception Forbidden_recombination;;

let salt = 
  String.concat "" ["a"; "Y"; "2"; "u"; "k"; "k"; "w"; "D"; "z"; "y"; "K"; "d"];;
let replacement_salt = 
  String.concat "" ["b"; "Z"; "3"; "v"; "l"; "m"; "x"; "E"; "A"; "z"; "L"; "e"];;

let decode (Stdl_encoded_string_t.E(encoded_s))=
   Stdl_replace_inside.replace_inside_text ~display_number_of_matches:false (replacement_salt,salt) encoded_s;;

let encode s=
   if Stdl_substring.is_a_substring_of replacement_salt s 
   then raise(Forbidden_substring)
   else let encoded_s=Stdl_replace_inside.replace_inside_text ~display_number_of_matches:false (salt,replacement_salt) s in 
        if  Stdl_substring.is_a_substring_of salt encoded_s 
        then raise(Forbidden_substring)
        else Stdl_encoded_string_t.E(encoded_s);;

let retrieve encoded_s = (Stdl_encoded_string_t.E(encoded_s));;
let store (Stdl_encoded_string_t.E(encoded_s))= encoded_s;;


end;;






module Stdl_crobj_converter=struct

(* 

#use"lib/Ocaml_analysis/Concrete_ocaml_objects/crobj_converter.ml";;

*)



exception Unwrap_int_exn of Stdl_concrete_object_t.t;;
exception Unwrap_string_exn of Stdl_concrete_object_t.t;;


let bool_of_concrete_object = Stdl_concrete_object.unwrap_lonely_variant [true,"True";false,"False"];;
let bool_to_concrete_object bowl = 
    if bowl 
    then Stdl_concrete_object_t.Variant("True",[]) 
    else Stdl_concrete_object_t.Variant("False",[]);;  

let int_of_concrete_object ccrt_obj =
    match ccrt_obj with 
     Stdl_concrete_object_t.Int(i)->i 
    |Stdl_concrete_object_t.String(_)
    |Stdl_concrete_object_t.Uple(_)
    |Stdl_concrete_object_t.List(_)
    |Stdl_concrete_object_t.Array(_)
    |Stdl_concrete_object_t.Record(_)
    |Stdl_concrete_object_t.Variant(_)
      ->raise(Unwrap_int_exn(ccrt_obj)) ;;
let int_to_concrete_object i = Stdl_concrete_object_t.Int i ;;       

let string_of_concrete_object ccrt_obj =
       match ccrt_obj with 
       Stdl_concrete_object_t.String(encoded_s)->Stdl_encoded_string.decode encoded_s 
       |Stdl_concrete_object_t.Int(_)
       |Stdl_concrete_object_t.Uple(_)
       |Stdl_concrete_object_t.List(_)
       |Stdl_concrete_object_t.Array(_)
       |Stdl_concrete_object_t.Record(_)
       |Stdl_concrete_object_t.Variant(_)
         ->raise(Unwrap_string_exn(ccrt_obj)) ;;
let string_to_concrete_object s = Stdl_concrete_object_t.String(Stdl_encoded_string.encode s) ;;       

end;;






module Stdl_dfa_subdirectory_t=struct

(*

Subdirectories name, with the trailing slash removed.

#use"lib/Decomposed_filename/dfa_subdirectory_t.ml";;

*)

type t=SD of string;;



end;;






module Stdl_min=struct

(*

#use"lib/min.ml";;

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
   let temp1=List.filter_map (function 
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






module Stdl_string_ranges=struct

(*

#use"lib/string_ranges.ml";;

*)

module Private = struct 

let complement_of_union_of_ranges text ranges =
  (* the ranges are assumed to be in increasing order *) 
  let n = String.length(text) in 
  let compl = Stdl_arithmetic_list.complement_union_of_ranges ranges n in 
  Stdl_image.image (fun (i,j)->String.sub text (i-1) (j-i+1)) compl ;;

let range_for_next_occurrence_of_in_from_opt patt text start_idx =
 match Stdl_substring.leftmost_index_of_in_from_opt patt text start_idx with 
 None -> None 
 |Some i1 -> Some(i1,i1+(String.length patt)-1);;

let ranges_for_next_ordered_occurrence_of_uple_in_from_opt patts original_text start_idx =
  let rec helper =(fun (text,remaining_patts,treated,current_idx) ->
  match remaining_patts with 
  [] -> Some(List.rev treated) 
  |patt::other_patts ->
   match range_for_next_occurrence_of_in_from_opt patt text current_idx with 
   None -> None 
   |Some(i1,i2) -> 
      helper (text,other_patts,(i1,i2)::treated,i2+1)) in 
   helper (original_text,patts,[],start_idx);;

let remove_next_ordered_occurrence_of_uple_in_from_opt patts text start_idx = 
   (*
   The convention is that in the returned result,
   we include the leftmost leftover but not the rightmost,
   so that we can iterate this function consistently later.
   For example, 
   remove_next_ordered_occurrence_of_uple_in_from_opt ["a";"b"] "1a2b3" 1
   returns  (["1"; "2"], 5) not (["1"; "2"; "3"], 5)

   *)
   let subtext = String.sub text (start_idx-1) (String.length(text)-start_idx+1) in 
   match ranges_for_next_ordered_occurrence_of_uple_in_from_opt patts subtext 1 with
   None -> ([],start_idx) 
   |Some ranges1 ->
      let rev_ranges1 = List.rev ranges1 in 
      let (a1,b1) = List.hd(rev_ranges1) in 
      let modified_revranges1= (a1,String.length text) :: (List.tl rev_ranges1) in 
      let ranges = List.rev modified_revranges1 
      and next_idx = b1+1 in 
      (complement_of_union_of_ranges subtext ranges,(start_idx-1)+next_idx) ;;

(*

let txt = "abcdefghijklmnop" ;;
let ranges = Option.get(ranges_for_next_ordered_occurrence_of_uple_in_from_opt ["bc";"f";"jk"] txt 1);;
let compl = complement_of_union_of_ranges txt ranges ;;
let remains = remove_next_ordered_occurrence_of_uple_in_from_opt ["bc";"f";"jk"] txt 1;;

let txt2 = "123" ^ txt ;;
let ranges = Option.get(ranges_for_next_ordered_occurrence_of_uple_in_from_opt ["bc";"f";"jk"] txt2 4);;
let compl = complement_of_union_of_ranges txt2 ranges ;;
let remains = remove_next_ordered_occurrence_of_uple_in_from_opt ["bc";"f";"jk"] txt2 4;;

*)

let rec helper_for_removing_all_occurrences patts (txt,treated,current_idx) =
  let (data,next_idx)= remove_next_ordered_occurrence_of_uple_in_from_opt patts txt current_idx in 
  if next_idx = current_idx 
  then let remainder = Stdl_cull_string.cobeginning (current_idx-1) txt in 
       (List.rev(treated),remainder)
  else helper_for_removing_all_occurrences patts (txt,data::treated,next_idx) ;;
    
end ;;

let remove_all_ordered_occurrences_of_uple_in patts txt = 
    Private.helper_for_removing_all_occurrences patts (txt,[],1) ;;

(*

remove_all_ordered_occurrences_of_uple_in ["bc";"f";"jk"] "123" ;;
remove_all_ordered_occurrences_of_uple_in ["bc";"f";"jk"] "abcdefghijklmnop" ;;
remove_all_ordered_occurrences_of_uple_in ["bc";"f";"jk"] "abcdefghijklmnop1bc23f456jk78" ;;

*)    

let remove_next_ordered_occurrence_of_uple_in_from_opt = Private.remove_next_ordered_occurrence_of_uple_in_from_opt ;; 

let replace_ranges_in l s=
    if l=[] then s else
    let n=String.length s in
    let ranges=Stdl_image.image fst l in
    let partition= Stdl_partition_list.from_set_of_ranges ranges n in 
    let temp1=Stdl_image.image (
      fun (i,j,will_be_replaced)->
        if will_be_replaced 
        then List.assoc (i,j) l
        else String.sub s (i-1) (j-i+1)
    ) partition in
    String.concat "" temp1;;

(*

replace_ranges_in [((3,5),"A");((8,12),"B")] "12345678901234567890";;

*)

let split_wrt_separator text separator =
    let (parts,remainder) = remove_all_ordered_occurrences_of_uple_in [separator] text in 
    let complement=(if remainder="" then [] else [remainder]) in 
    (List.flatten parts)@complement ;;
   
(*

split_wrt_separator "1abc23abc456abc78" "abc" ;;
split_wrt_separator "abc23abc456abc78abc" "abc" ;;

*)    

end;;






module Stdl_strung=struct

(*

#use"lib/strung.ml";;

*)


let get s i=String.get s (i-1);;
 
let set s i c=Bytes.set s (i-1) c;;

let enclose s=
  let encloser="\"" in
  encloser^(String.escaped s)^encloser;;

let sophisticated_enclose txt =
  let temp1 = Str.split_delim (Str.regexp_string "\n") txt in 
  let indexed_temp1 = Stdl_int_range.index_everything temp1 
  and m=List.length temp1 in 
  let encloser="\"" in
  let temp2 = Stdl_image.image (fun (idx,line)->
   let connector1_if_not_finished=(if idx=m then "" else "\\n") in
   (idx,encloser^(String.escaped line)^connector1_if_not_finished^
   encloser) 
  ) indexed_temp1 in
  let cleaned_temp2=List.filter (
    fun pair-> pair<>(m,"\"\"") 
  ) temp2 in 
  let cleaned_m=List.length cleaned_temp2 in 
  let temp3 = Stdl_image.image (fun (idx,line)->
   let connector2_if_not_finished=(if idx=cleaned_m then "" else "^") in
   line^connector2_if_not_finished 
  ) cleaned_temp2 in
  String.concat "\n" temp3;;
  
(*

print_string("\n\n\n"^(sophisticated_enclose "12\n\n56")^"\n\n\n") ;;
print_string("\n\n\n"^(sophisticated_enclose "123\n45\n678\n912")^"\n\n\n") ;;
print_string("\n\n\n"^(sophisticated_enclose "\n123\n45\n678\n912\n")^"\n\n\n") ;;

*)

let random chars m=
  let n = List.length chars in    
  let temp = Stdl_int_range.scale (fun _->List.nth chars (Random.int n)) 1 m in 
  String.concat "" (Stdl_image.image (String.make 1) temp) ;;
let implode l=
   let n=List.length(l) in
   let by=Bytes.make n ' ' in
   let _=(for i=0 to (n-1) do Bytes.set by i (List.nth l i) done;) in
   Bytes.to_string by;;
  
    
let explode s=
    let n=String.length s in
    Stdl_int_range.scale (String.get s) 0 (n-1);;
 
let show_indices s=
  let n=String.length s in
  Stdl_int_range.scale (fun i->(i,String.get s (i-1)) ) 1 n;;   
   
let number_of_lines_before s i=
  if i<1 then 0 else
  let m=min i (String.length s) in
  List.length(List.filter(fun j->(String.get s (j-1))='\n')(Stdl_int_range.range 1 m));;


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
  ) (Stdl_int_range.range idx n);;

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
  None -> List.rev (((idx,last_idx),false,Stdl_cull_string.interval whole idx last_idx) 
                       :: treated)
 |Some(idx2,found_word) ->
    let treated2 = (
       if idx2=idx 
       then treated
       else ((idx,idx2-1),false,Stdl_cull_string.interval whole idx (idx2-1)) ::treated
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



let insert_prefixes_at_indices l s=
    if l=[] then s else
    let n=String.length s in
    let temp1=Stdl_image.image (fun (pref,idx)->(idx,pref)) l in
    let temp2=Stdl_image.image fst temp1 in
    let temp3=Stdl_ordered.to_list(Stdl_ordered.sort Stdl_total_ordering.standard ((n+1)::temp2)) in
    let temp4=Stdl_list_again.universal_delta_list temp3 in
    let temp5=Stdl_image.image(fun (i,j)->
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
   let lengths=Stdl_image.image String.length l in
   let m=Stdl_min.list lengths in
   let tester=(fun k->
     let temp1=Stdl_image.image (fun s->String.get s k) l in
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
   )(Stdl_int_range.range 1 n) with 
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
            then let new_item = Stdl_cull_string.interval s current_item_start (new_idx-1) in 
                 (None,nbr_of_openers_so_far,new_item::items_so_far,idx2,world_start,idx2)
            else (None,nbr_of_openers_so_far,items_so_far,current_item_start,world_start,idx2)
          )
     else 
     if case=3
     then (
            if nbr_of_openers_so_far=1
            then let whole_interval=(world_start,idx2-1) in 
                 let last_item = Stdl_cull_string.interval s current_item_start (new_idx-1) in 
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
    let seeker = Stdl_substring.leftmost_index_of_pattern_among_in_from_opt 
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
   let s=Stdl_cull_string.interval enclosed_s 2 (n-1) in 
   let temp1=Stdl_string_ranges.split_wrt_separator s ";" in 
   Stdl_image.image int_of_string temp1;;

let of_intlist l=
  let temp1=Stdl_image.image string_of_int l in 
  "["^(String.concat ";" temp1)^"]";;

(*


let z1=[2;7;3;51];;
let z2=of_intlist z1;;
let check =(to_intlist(z2)=z1);;

*)  

let soak (replacee,replacer) s=
   if String.starts_with ~prefix:replacee s 
   then Some(replacer^(Stdl_cull_string.two_sided_cutting (replacee,"") s))
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
     (Stdl_image.image (fun line->enclose("\n"^line)) other_lines) in 
    String.concat "^\n" quoted_lines;;


(*

let z1 = "abc\nde\nfghi\njkl";;
print_string(escaped_and_quoted z1);;



*)

let reposition_whole_according_to_separator separator lines =
      let temp1 = Stdl_image.image (fun line->(line,Option.get(Stdl_substring.leftmost_index_of_in_from_opt separator line 1))) lines in 
      let max_idx = snd(Stdl_max.maximize_it snd temp1) in 
      Stdl_image.image (fun (line,idx)->
          let offset = max_idx-idx in 
          (String.make offset ' ')^line
      ) temp1;;

let reposition_left_hand_side_according_to_separator separator lines =
         let temp1 = Stdl_image.image (fun line->
              let j = Option.get(Stdl_substring.leftmost_index_of_in_from_opt separator line 1) in 
              ((Stdl_cull_string.beginning (j-1) line,Stdl_cull_string.cobeginning (j-1) line),j)) lines in 
         let max_idx = snd(Stdl_max.maximize_it snd temp1) in 
         Stdl_image.image (fun ((left,right),idx)->
             let offset = max_idx-idx in 
             left^(String.make offset ' ')^right
         ) temp1;;      

let announce ~trailer ~printer ~items ~separator=
   if items = []
   then ()
   else   
   let temp1 = Stdl_image.image printer items in
   let msg = "\n\n"^trailer^"\n\n"^(String.concat separator temp1)^"\n\n" in 
   (print_string msg;flush stdout) ;;    

let with_size_limit s ~size_limit =
    if String.length(s)>size_limit 
    then (String.sub s 0 size_limit)^"(...)"
    else s ;;      

end;;






module Stdl_dfa_subdirectory=struct

(*

Subdirectories name, with the trailing slash removed.

#use"lib/Decomposed_filename/dfa_subdirectory.ml";;


*)

module Private = struct 

   let of_line s=
      let n = String.length s in 
      let indices = List.rev(Stdl_int_range.range 1 n) in 
      let limit_idx=(match List.find_opt(fun j->(Stdl_strung.get s j)<>'/')(indices) with 
         None -> 0 |Some(j)->j
      ) in 
      Stdl_dfa_subdirectory_t.SD (Stdl_cull_string.beginning limit_idx s);;   
   
   let without_trailing_slash (Stdl_dfa_subdirectory_t.SD s)=s;;
   
   end ;;   
   
   
   
   let compute_long_subdir_name old_subdir new_subdir_short_name =
      let temp1 =  Stdl_cull_string.trim_slashes_on_the_right new_subdir_short_name in
      let long_name = (
      if String.contains temp1 '/'
      then temp1 
      else let old_subdir_name = Private.without_trailing_slash old_subdir in 
           let father_name = Stdl_cull_string.before_rightmost old_subdir_name '/' in 
           if father_name = ""
           then temp1
           else father_name^"/"^temp1 ) in 
      Private.of_line long_name ;;    
   
   let connectable_to_subpath (Stdl_dfa_subdirectory_t.SD s)=
     if s="" 
     then "" 
     else s^"/";;
   
   let begins_with (Stdl_dfa_subdirectory_t.SD s1) (Stdl_dfa_subdirectory_t.SD s2)=
      String.starts_with ~prefix:s2 s1 ;;
       
   let extend (Stdl_dfa_subdirectory_t.SD s) subsub = Stdl_dfa_subdirectory_t.SD (s^"/"^subsub);;
   
   let main = Stdl_dfa_subdirectory_t.SD "";;
   
   let of_concrete_object ccrt_obj =
      let (_,(arg1,_,_,_,_,_,_))=Stdl_concrete_object.unwrap_bounded_variant ccrt_obj in 
      Stdl_dfa_subdirectory_t.SD(Stdl_crobj_converter.string_of_concrete_object arg1);;
   
   let of_line = Private.of_line ;;

   let silex_order = ((fun (Stdl_dfa_subdirectory_t.SD subdir1) (Stdl_dfa_subdirectory_t.SD subdir2)->Stdl_total_ordering.silex_for_strings subdir1 subdir2) : 
     Stdl_dfa_subdirectory_t.t Stdl_total_ordering_t.t
   );;
   
   let rename_endsubdirectory (Stdl_dfa_subdirectory_t.SD(old_subdir),new_esdname) 
      (Stdl_dfa_subdirectory_t.SD s)=
      if String.starts_with ~prefix:old_subdir s 
      then let sub_s=Stdl_cull_string.cobeginning (String.length old_subdir) s in
           let t=Stdl_cull_string.before_rightmost old_subdir '/' in
           let new_t=(if t="" then "" else t^"/") in
           Stdl_dfa_subdirectory_t.SD(new_t^new_esdname^sub_s)
      else Stdl_dfa_subdirectory_t.SD(s);;
      
   (*
   
   rename_endsubdirectory (SD("Haag/Huug"),"Java") (SD "Haag/Huug/King/Jordan");;
   rename_endsubdirectory (SD("Haag"),"Java") (SD "Haag/Huug/King/Jordan");;
   
   *)              
   
   
   let soak (Stdl_dfa_subdirectory_t.SD(s1),Stdl_dfa_subdirectory_t.SD(s2)) (Stdl_dfa_subdirectory_t.SD(s))=
      match Stdl_strung.soak (s1,s2) s with 
      Some(t)->Some(Stdl_dfa_subdirectory_t.SD(t))
      |None -> None ;;
   
   let to_concrete_object (Stdl_dfa_subdirectory_t.SD(s))=
       Stdl_concrete_object_t.Variant("Dfa_"^"subdirectory_t.SD",
       [Stdl_crobj_converter.string_to_concrete_object(s)]);;
   
   
   
   let without_trailing_slash= Private.without_trailing_slash ;;
   
   
   

end;;






module Stdl_dfa_ending_t=struct

(*

#use"lib/Decomposed_filename/dfa_ending_t.ml";;

Does not contain a dot.

*)

type t=E of string;;



end;;






module Stdl_dfa_module_t=struct

(*

#use"lib/Decomposed_filename/dfa_module_t.ml";;

A module name, or a candidate for one. Uncapitalized. 
Should contain no slashes.

*)

type t=M of string;;

           

end;;






module Stdl_dfn_short_t=struct

(*

#use"lib/Decomposed_filename/dfn_short_t.ml";;

*)

type t = J of Stdl_dfa_module_t.t * Stdl_dfa_ending_t.t;;
          



end;;






module Stdl_dfa_module=struct

(*

#use"lib/Decomposed_filename/dfa_module.ml";;

A module name, or a candidate for one. Uncapitalized. Should contain no slashes.

*)


let of_line s=Stdl_dfa_module_t.M (String.uncapitalize_ascii s);; 
let to_line (Stdl_dfa_module_t.M s)=s;;

let add_prefix_and_capitalize prefix (Stdl_dfa_module_t.M name)=
  Stdl_dfa_module_t.M(String.capitalize_ascii(prefix^name));;

  
let capitalized_form (Stdl_dfa_module_t.M name)= String.capitalize_ascii name;;

let to_concrete_object (Stdl_dfa_module_t.M(s))=
    Stdl_concrete_object_t.Variant("Dfa_"^"module_t.M",
     [Stdl_crobj_converter.string_to_concrete_object(s)]);;

let of_concrete_object ccrt_obj =
   let (_,(arg1,_,_,_,_,_,_))=Stdl_concrete_object.unwrap_bounded_variant ccrt_obj in 
   Stdl_dfa_module_t.M(Stdl_crobj_converter.string_of_concrete_object arg1);;


end;;






module Stdl_dfn_middle_t=struct

(*

#use"lib/Decomposed_filename/dfn_middle_t.ml";;

*)

type t = J of Stdl_dfa_subdirectory_t.t * Stdl_dfa_module_t.t ;;
          



end;;






module Stdl_dfa_root_t=struct

(*

Does not end with a slash.

#use"lib/Decomposed_filename/dfa_root_t.ml";;

*)

type t=R of string;;           

end;;






module Stdl_dfa_root=struct

(*

The rightmost trailing slash is removed.

#use"lib/Decomposed_filename/dfa_root.ml";;

*)

let without_trailing_slash (Stdl_dfa_root_t.R s)=s;;
let connectable_to_subpath (Stdl_dfa_root_t.R s)=s^"/";;  
let dummy = Stdl_dfa_root_t.R "" ;;

let of_line line = Stdl_dfa_root_t.R(Stdl_tools_for_absolute_path.remove_trailing_slash line);;


let to_concrete_object (Stdl_dfa_root_t.R(line))=
    Stdl_concrete_object_t.Variant("Dfa_"^"root.R",[Stdl_crobj_converter.string_to_concrete_object(line)]);;

let of_concrete_object ccrt_obj =
   let (_,(arg1,_,_,_,_,_,_))=Stdl_concrete_object.unwrap_bounded_variant ccrt_obj in 
   Stdl_dfa_root_t.R(Stdl_crobj_converter.string_of_concrete_object arg1);;



end;;






module Stdl_dfn_rootless_t=struct

(*

#use"lib/Decomposed_filename/dfn_rootless_t.ml";;

*)

type t = J of Stdl_dfa_subdirectory_t.t * Stdl_dfa_module_t.t * Stdl_dfa_ending_t.t ;;
          



end;;






module Stdl_dfn_common=struct

(*

#use"lib/Decomposed_filename/dfn_common.ml";;

*)

exception No_dot_in_str_to_rootless of string;;
exception Decompose_absolute_path_using_root_exn of Stdl_absolute_path.t * Stdl_dfa_root_t.t;;

let string_of_sm (s,m)=
   let (Stdl_dfa_subdirectory_t.SD sub)=s 
   and (Stdl_dfa_module_t.M mn)=m in 
   if sub=""
   then mn
   else sub^"/"^mn;;

let string_to_sm s=
   let (sub,mn)= Stdl_cull_string.split_wrt_rightmost s '/' in 
   (Stdl_dfa_subdirectory_t.SD sub,Stdl_dfa_module_t.M mn);;     

let string_to_rootless line=
  if not(String.contains line '.')
  then raise(No_dot_in_str_to_rootless(line)) 
  else 
  let (rest,ending) = Stdl_cull_string.split_wrt_rightmost line '.' in 
  let (s,m) = string_to_sm rest in 
  Stdl_dfn_rootless_t.J(s,m,Stdl_dfa_ending_t.E ending);;

   
let decompose_absolute_path_using_root ap root=
  let s_root=Stdl_dfa_root.without_trailing_slash root  
  and s_ap = Stdl_absolute_path.to_string ap in 
  let ns=String.length(s_root)
  and nw=String.length(s_ap) in
  if (ns+1)>nw then raise(Decompose_absolute_path_using_root_exn(ap,root)) else
  if (String.sub s_ap 0 (ns+1))<>(s_root^"/") then raise(Decompose_absolute_path_using_root_exn(ap,root)) else
  string_to_rootless(String.sub s_ap (ns+1) (nw-ns-1));;

let recompose_potential_absolute_path root 
      (Stdl_dfn_rootless_t.J(Stdl_dfa_subdirectory_t.SD(s),Stdl_dfa_module_t.M(m),Stdl_dfa_ending_t.E(e))) =
    let s_subdir = (if s="" then "" else s^"/") in 
    (Stdl_dfa_root.connectable_to_subpath root)^s_subdir^m^"."^e;;


end;;






module Stdl_dfa_ending=struct

(*

#use"lib/Decomposed_filename/dfa_ending.ml";;

*)



exception Dot_inside_ending of string;;
exception Not_an_ocaml_ending of string;;
exception Unknown_ending of Stdl_dfa_ending_t.t ;;

let of_line e =
  if String.contains e '.'
  then raise(Dot_inside_ending(e))
  else Stdl_dfa_ending_t.E(e);;

let connectable_to_modulename (Stdl_dfa_ending_t.E(e)) = "." ^ e ;;



let mll =  Stdl_dfa_ending_t.E "mll"
and mly =  Stdl_dfa_ending_t.E "mly"
and ml  =  Stdl_dfa_ending_t.E "ml"
and mli =  Stdl_dfa_ending_t.E "mli" ;; 

let all_ocaml_endings= [mll;mly;ml;mli];;

let all_cee_endings = Stdl_image.image (fun s->Stdl_dfa_ending_t.E s) ["h";"c"];;

let compute_on_all_ocaml_endings f=(f ml,f mli,f mll,f mly);;

let endings_for_compilable_files = 
   (all_ocaml_endings) @ all_cee_endings ;;

let endings_for_noncompilable_readable_files = 
     Stdl_image.image (fun s->Stdl_dfa_ending_t.E s) ["txt";"html";"php";"js";"ejs";"json"];; 

let endings_for_readable_files = 
     endings_for_compilable_files @ endings_for_noncompilable_readable_files ;;

let is_compilable edg =
   if List.mem edg endings_for_compilable_files 
   then true 
   else 
   if List.mem edg endings_for_noncompilable_readable_files 
   then false 
   else raise(Unknown_ending(edg));;



let to_concrete_object (Stdl_dfa_ending_t.E(e)) =
    Stdl_concrete_object_t.Variant ("Dfa_"^"ending_t.E",
    [Stdl_crobj_converter.string_to_concrete_object(e)]);;

let of_concrete_object crobj =
   let (_,(arg1,_,_,_,_,_,_))=Stdl_concrete_object.unwrap_bounded_variant crobj in 
   Stdl_dfa_ending_t.E(
      Stdl_crobj_converter.string_of_concrete_object arg1
   );;



end;;






module Stdl_dfn_endingless_t=struct

(*

#use"lib/Decomposed_filename/dfn_endingless_t.ml";;

*)

type t = J of Stdl_dfa_root_t.t * Stdl_dfa_subdirectory_t.t * Stdl_dfa_module_t.t  ;;



end;;






module Stdl_dfn_full_t=struct

(*

#use"lib/Decomposed_filename/dfn_full_t.ml";;

*)


type t = J of Stdl_dfa_root_t.t * Stdl_dfa_subdirectory_t.t * Stdl_dfa_module_t.t * Stdl_dfa_ending_t.t;;



end;;






module Stdl_dfn_join=struct

(*

#use"lib/Decomposed_filename/dfn_join.ml";;

*)

let middle_to_ending (Stdl_dfn_middle_t.J(s,m)) e= (Stdl_dfn_rootless_t.J(s,m,e));;
let root_to_middle r (Stdl_dfn_middle_t.J(s,m))=Stdl_dfn_endingless_t.J(r,s,m);; 
let root_to_rootless r (Stdl_dfn_rootless_t.J(s,m,e))=Stdl_dfn_full_t.J(r,s,m,e);;
let root_to_subdirectory (Stdl_dfa_root_t.R(r)) (Stdl_dfa_subdirectory_t.SD(s))=Stdl_dfa_root_t.R(r^"/"^s);;
let subdirectory_to_module s m=(Stdl_dfn_middle_t.J(s,m));;  
let subdirectory_to_short s (Stdl_dfn_short_t.J(m,e))=(Stdl_dfn_rootless_t.J(s,m,e));;  
let to_ending (Stdl_dfn_endingless_t.J(r,s,m)) e = Stdl_dfn_full_t.J(r,s,m,e);;



end;;






module Stdl_dfn_full=struct

(*

#use"lib/Decomposed_filename/dfn_full.ml";;

*)


let to_subdirectory  (Stdl_dfn_full_t.J(_r,s,_m,_e))=s;;
let to_module  (Stdl_dfn_full_t.J(_r,_s,m,_e))=m;;
let to_ending (Stdl_dfn_full_t.J(_r,_s,_m,e))=e;;

let to_rootless (Stdl_dfn_full_t.J(_r,s,m,e))=(Stdl_dfn_rootless_t.J(s,m,e));; 
let to_endingless (Stdl_dfn_full_t.J(r,s,m,_e))=(Stdl_dfn_endingless_t.J(r,s,m));; 

   
let to_rootless_line (Stdl_dfn_full_t.J(_r,s,m,e))=
    (Stdl_dfa_subdirectory.connectable_to_subpath s)^
   (Stdl_dfa_module.to_line m)^(Stdl_dfa_ending.connectable_to_modulename e);;

let to_line (Stdl_dfn_full_t.J(r,s,m,e))=
   (Stdl_dfa_root.connectable_to_subpath r)^
   (Stdl_dfa_subdirectory.connectable_to_subpath s)^
   (Stdl_dfa_module.to_line m)^(Stdl_dfa_ending.connectable_to_modulename e);;

let to_absolute_path mlx=Stdl_absolute_path.of_string(to_line mlx);;  

let from_absolute_path_with_root ap dir=
  let rless = Stdl_dfn_common.decompose_absolute_path_using_root ap dir in 
  Stdl_dfn_join.root_to_rootless dir rless;;

  
let relocate (Stdl_dfn_full_t.J(r,_old_subdir,m,e)) new_subdir=
  (Stdl_dfn_full_t.J(r,new_subdir,m,e));;  
 




end;;






module Stdl_basic=struct

(*

#use"lib/basic.ml";;

*)

module Private = struct 

  let frac_ceiling_for_positive_denominator a b=
  (*we assume that b is positive *)
  if (a mod b)=0 
  then (a/b) 
  else if (a>=0) 
       then (a/b)+1
       else -(( abs a)/b);;
    
   let frac_floor_for_positive_denominator a b=
  (*we assume that b is positive *)
    if (a>=0) 
    then (a/b)
    else -((( abs a)/b)+1);;    

let rec helper_for_power (a,b,accu) =
    if b<1 then accu else 
    if b=1 then a*accu else 
    let multiplier = (if b mod 2=0 then 1 else a) in 
    helper_for_power (a*a,b/2,multiplier * accu) ;;  

let display_message msg =
    print_string ("\n\n\n" ^ msg ^ " \n\n\n");
    flush stdout
  ;;

let rec helper_for_horner_algorithm x accu coeffs = match coeffs with 
   [] -> accu 
   |a::other_coeffs -> helper_for_horner_algorithm x (a+x*accu) other_coeffs ;;

(*
helper_for_horner_algorithm 10 0 [6;7;8] ;;
*)

end ;;  

let announce cmd = Private.display_message (cmd ^ " ...") ;;
   
let announce_execution cmd = announce ("Executing " ^ cmd) ;;

let display_message = Private.display_message ;;
let fold_prod=function
[]->1
|a::b->List.fold_left( * )(a)(b);;

let fold_sum=function
[]->0
|a::b->List.fold_left(+)(a)(b);;

let frac_ceiling a b=
 if (b=0) 
 then failwith("division by zero in frac_ceiling")
 else if (b>0)
      then Private.frac_ceiling_for_positive_denominator a b
      else Private.frac_ceiling_for_positive_denominator (-a) (-b);;
let frac_floor a b=
 if (b=0) 
 then failwith("division by zero in frac_floor")
 else if (b>0)
      then Private.frac_floor_for_positive_denominator a b
      else Private.frac_floor_for_positive_denominator (-a) (-b);;  

let horner_algorithm coeffs x = Private.helper_for_horner_algorithm x 0 coeffs;;

(*

horner_algorithm [7;8;0;1] 10 ;;

*)

let rec largest_in_range_satisfying_opt (a,b) f =
   if a>b then None else 
   if f b then Some b else 
   largest_in_range_satisfying_opt (a,b-1) f;;     

let power a b = Private.helper_for_power (a,b,1) ;;

let sign x=if x<0 then -1 else if x=0 then 0 else 1;;

let rec smallest_in_range_satisfying_opt (a,b) f =
   if a>b then None else 
   if f a then Some a else 
   smallest_in_range_satisfying_opt (a+1,b) f;;  



end;;






module Stdl_chronometer=struct

(*

#use"lib/chronometer.ml";;

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






(* Contents of ~/Teuliou/OCaml/skeptical_duck/lib/chronometer.mli:

 
val rewrite_float : float -> string
val duration_of_computation : ('a -> 'b) -> 'a -> unit
val duration_of_last_computation : unit -> unit
val it : ('a -> 'b) -> 'a -> 'b



*) 






module Stdl_compact_replacer_t=struct

(*

#use"lib/Text_editing/compact_replacer_t.ml";;

*)

type t= CR of (string*string) list ;;


end;;






module Stdl_compact_replacer=struct

(*

#use"lib/Text_editing/compact_replacer.ml";;

*)

module Private = struct

let separator = " \205\140 ";;

let unparse (Stdl_compact_replacer_t.CR(l))=
   let temp1 = List.flatten (Stdl_image.image (fun (x,y)->[x;y]) l)  in 
   String.concat separator temp1 ;;

let parse descr =
   let temp1 = Str.split (Str.regexp_string separator) descr in 
   let m = (List.length temp1)/2 in
   let tg =(fun k->List.nth temp1 (k-1)) in  
   Stdl_compact_replacer_t.CR(Stdl_int_range.scale (fun j->(tg (2*j-1),tg (2*j)) ) 1 m );;

let replace_inside_text (Stdl_compact_replacer_t.CR(l)) old_text =
   Stdl_replace_inside.replace_several_inside_text l old_text ;;

let replace_inside_file (Stdl_compact_replacer_t.CR(l)) fn =
   Stdl_replace_inside.replace_several_inside_file l fn ;;

let reverse_replace_inside_text (Stdl_compact_replacer_t.CR(old_l)) old_text =
   let l = Stdl_image.image (fun (x,y)->(y,x)) old_l in 
   Stdl_replace_inside.replace_several_inside_text l old_text ;;

let reverse_replace_inside_file (Stdl_compact_replacer_t.CR(old_l)) fn =
   let l = Stdl_image.image (fun (x,y)->(y,x)) old_l in 
   Stdl_replace_inside.replace_several_inside_file l fn ;;

let execute s=
   let temp1 = Str.split (Str.regexp "[ \t]+") s in 
   let temp2 = Stdl_image.image Stdl_absolute_path.of_string  temp1 in 
   let replacements = Stdl_io.read_whole_file (List.nth temp2 0) 
   and recipient = (List.nth temp2 1) in 
   replace_inside_file (parse replacements) recipient ;;

let reverse_execute s=
   let temp1 = Str.split (Str.regexp "[ \t]+") s in 
   let temp2 = Stdl_image.image Stdl_absolute_path.of_string  temp1 in 
   let replacements = Stdl_io.read_whole_file (List.nth temp2 0) 
   and recipient = (List.nth temp2 1) in 
   reverse_replace_inside_file (parse replacements) recipient ;;   

(*

let z1 =  Compact_replacer_t.CR(["abc","def";"12","34"]) ;;  
let z2 = unparse z1;;
let z3 = parse z2;;
let check = (z3=z1);;

*)

end ;;

let execute = Private.execute ;;

let reverse_execute = Private.reverse_execute ;; 



end;;






module Stdl_unix_command=struct

(*

Wrapper on the Sys dot command function.

#use"lib/unix_command.ml";;

*)


exception Command_failed of string;;
exception Command_failed_just_now ;;

module Private = struct

let prefix_for_changing_directories         = "cd ";;
let prefix_for_replacing_patterns           = "rp ";;
let prefix_for_reverse_replacing_patterns   = "rvp ";;
let prefix_for_printing_text   = "print_outside ";;

let accu=ref([]:string list);;
let remember_commands_mode=ref(false);;
let hardcore_mode=ref(false);;

let command cmd=
   let cd_prefix =prefix_for_changing_directories 
   and rp_prefix =prefix_for_replacing_patterns 
   and rvp_prefix =prefix_for_reverse_replacing_patterns 
   and pt_prefix =prefix_for_printing_text in 
   if String.starts_with ~prefix:cd_prefix cmd 
   then let  _=Sys.chdir(Stdl_cull_string.cobeginning (String.length cd_prefix) cmd) in 0
   else 
   if String.starts_with ~prefix:rp_prefix cmd 
   then let  _= Stdl_compact_replacer.execute(Stdl_cull_string.cobeginning (String.length rp_prefix) cmd) in 0 
   else 
   if String.starts_with ~prefix:rvp_prefix cmd 
   then let  _= Stdl_compact_replacer.reverse_execute(Stdl_cull_string.cobeginning (String.length rvp_prefix) cmd) in 0 
   else 
   if String.starts_with ~prefix:pt_prefix cmd 
   then let  _= (print_string("\n"^(Stdl_cull_string.cobeginning (String.length pt_prefix) cmd));flush stdout) in 0 
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

let displayable_uc display_mode s=
   if display_mode 
   then let _ = Stdl_basic.display_message s in uc s 
   else uc s;;

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

let rec helper_for_indexed_multiple_uc sn indexed_commands=
  match indexed_commands with
 []->true
|(idx1,cmd1)::other_commands ->
   if (uc cmd1)=0
   then let msg = cmd1^"\nFinished command "^(string_of_int idx1)^" of "^sn^"\n\n" in 
        let _ = (print_string msg;flush stdout) in 
        helper_for_indexed_multiple_uc sn other_commands 
   else false;;
              
let indexed_multiple_uc commands=
  let sn = string_of_int(List.length commands) 
  and indexed_commands= Stdl_int_range.index_everything commands in 
  Stdl_chronometer.it (helper_for_indexed_multiple_uc sn) indexed_commands ;; 


end;;

let cd dirname = (Private.prefix_for_changing_directories)^dirname;;

let rec conditional_multiple_uc ?(display_mode=false) commands=match commands with
  []->true
  |cmd1::other_commands ->
    if (Private.displayable_uc display_mode cmd1)=0
    then conditional_multiple_uc ~display_mode other_commands 
    else false;;
           
let debug_multiple_uc l = Private.helper_for_debug_multiple_uc (1,l);;           

let hardcore_uc = Private.hardcore_uc ;;

let indexed_multiple_uc = Private.indexed_multiple_uc ;;
let mv full_path new_location =
   let destination_equals_source=(
     if not(String.starts_with ~prefix:new_location full_path) then false else 
     let naked_name=Stdl_cull_string.two_sided_cutting (new_location,"") full_path in 
     not(String.contains naked_name '/') 
   ) in 
   if destination_equals_source 
   then None 
   else Some("mv "^full_path^" "^new_location);;

let prefix_for_replacing_patterns           = Private.prefix_for_replacing_patterns;;
let prefix_for_reverse_replacing_patterns   = Private.prefix_for_reverse_replacing_patterns;;


let uc = Private.uc;;



           

end;;






module Stdl_situation_of_linebreak_inside_text_t=struct

(*

#use"lib/situation_of_linebreak_inside_text_t.ml";;

*)

type t = 
    Lbit_inside_a_long_double_quoted_string  
   |Lbit_inside_a_starry_comment
   |Lbit_outside_comments_or_strings ;;

 

end;;






module Stdl_string_find_char=struct

(*

#use"lib/string_find_char.ml";;

*)

 
let from_inclusive_opt f s initial_idx=
   let n=(String.length s) in
   let rec tempf=(fun j->
     if j>n then None else
     if f(String.get s  (j-1)) then Some j else
     tempf(j+1)
   ) in
   tempf(initial_idx);;

(*

from_inclusive_opt (fun c->c='3') "123456789" 1 ;;

*)   

let backwards_from_inclusive_opt f s initial_idx=
    let rec tempf=(fun j->
      if j<1 then None else
      if f(String.get s (j-1)) then Some(j) else
      tempf(j-1)
    ) in
    tempf(initial_idx);;   
 
(*

backwards_from_inclusive_opt (fun c->c='3') "123456789" 7 ;;
backwards_from_inclusive_opt (fun c->c='7') "123456789" 7 ;;

*)   

end;;






module Stdl_lines_in_text=struct

(*

#use"lib/lines_in_text.ml";;

*)

exception Shift_indentation_in_line_exn of int * string ;;

exception Put_line_first_bad_line_index_exn  of int * int ;;
exception Put_line_last_bad_line_index_exn  of int * int ;;

exception Unfinished_single_quoted_string ;;
exception Unfinished_double_quoted_string ;;

module Private = struct 

  let lines old_text=
     let left_offset=(if String.starts_with ~prefix:"\n" old_text  then "\n" else "")
     and right_offset=(if String.ends_with ~suffix:"\n" old_text  then "\n" else "") in
     let s=left_offset^old_text^right_offset in
     Str.split (Str.regexp_string "\n") s ;;

  let indexed_lines text=
     Stdl_int_range.index_everything (lines text);;
  
  let rec iterator_for_enchancement (num_of_treated_chars,treated_lines,lines) =
       match lines with 
       [] -> List.rev treated_lines 
       |(line_idx,line) :: other_lines ->
        iterator_for_enchancement 
        (num_of_treated_chars+(String.length line)+1,
         (num_of_treated_chars+1,line_idx,line)::treated_lines,other_lines)   ;;
        
  let enhance indexed_lines =  iterator_for_enchancement (0,[],indexed_lines );;      
  
  let adjust_num_of_lines_upwards_in_text ~required_size text =
      let temp1 = lines text in  
      let d = required_size - (List.length temp1) in 
      if d<=0 
      then text 
      else text ^ (String.make d '\n') ;;   

  let adjust_num_of_lines_upwards_in_file ~required_size file =
      let old_text = Stdl_io.read_whole_file file in
      let new_text = adjust_num_of_lines_upwards_in_text ~required_size old_text  in
      Stdl_io.overwrite_with file new_text ;;   

  let tripartition_associated_to_interval s i j=
      let temp2=lines s in 
      let (temp3,temp4)=Stdl_list_again.long_head_with_tail (i-1) temp2 in 
      let part1=String.concat "\n" (List.rev temp3) in 
      let (temp5,temp6)=Stdl_list_again.long_head_with_tail (j-i+1) temp4 in 
      let part2=String.concat "\n" (List.rev temp5) in 
      let part3=String.concat "\n" temp6 in 
      (part1^"\n",part2,"\n"^part3);;
   
   (* tripartition_associated_to_interval "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)
       
  let interval text i j=
    let temp1=indexed_lines text in
    let temp2=List.filter (fun (k,_)->(i<=k)&&(k<=j)) temp1  in
    let temp3=Stdl_image.image snd temp2 in
    String.concat "\n" temp3;;  
      
  (* interval "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)
    
  let copy_interval_from_text_to_text (i,j)  src dest =
     let src_linelength = List.length (lines src) in 
     let temp1 = adjust_num_of_lines_upwards_in_text ~required_size:src_linelength dest in
     let (before,_in_between,after) = tripartition_associated_to_interval temp1 i j in 
     before^(interval src i j)^after;;

      
  (* copy_interval_from_text_to_text (2,5) "1\n2\n3\n4\n5\n6\n7\n" "a\nb\nc";; *)

  let copy_interval_from_file_to_file (i,j) src_file  dest_file =
     let src = Stdl_io.read_whole_file src_file 
     and old_text = Stdl_io.read_whole_file dest_file  in 
     let new_text = copy_interval_from_text_to_text (i,j) src old_text in 
     Stdl_io.overwrite_with dest_file new_text ;; 
     
   exception Lines_in_char_range_exn of int*int;;

   let number_of_lines_in_char_interval text  i j=
     try (List.length(List.filter (fun k->
         String.get text (k-1)='\n'
     ) (Stdl_int_range.range i j))) with
     _->raise(Lines_in_char_range_exn(i,j));;    

   let duplicate_interval_in_text (i,j) text = 
     let (before,itv,after) = tripartition_associated_to_interval text i j in 
     before^itv^"\n"^itv^after ;;

  (* duplicate_interval_in_text (2,4) "1\n2\n3\n4\n5\n";; *)

   let duplicate_interval_in_file (i,j) src_file  =
     let old_text = Stdl_io.read_whole_file src_file  in 
     let new_text = duplicate_interval_in_text (i,j) old_text in 
     Stdl_io.overwrite_with src_file new_text ;; 

   let naive_closeup_around_index text j=
     let n=String.length text in
     let temp1=List.filter(fun j->(String.get text (j-1))='\n')(Stdl_int_range.range 1 n) in
     let (temp2,temp3)=Stdl_hurried.partition_in_two_parts(fun k->k<j) temp1 in
     let a=(if List.length(temp2)<6 then 1 else List.nth(List.rev temp2)(5))
     and b=(if List.length(temp3)<6 then n else List.nth(temp3)(5)) in
     (a,String.sub text a (b-a));;

  let closeup_around_index text idx =
     let (char_idx,subtext) = naive_closeup_around_index text idx in 
     let startline_idx = (Stdl_strung.number_of_lines_before text char_idx) in 
     let lines = indexed_lines subtext in 
     let decorated_lines = Stdl_image.image (
       fun (idx2,line)->
          let prefix = 
            Stdl_strung.insert_repetitive_offset_on_the_left ' ' 6 (string_of_int (idx2+startline_idx)) in 
          prefix^": "^line  
     ) lines in
     String.concat "\n" (""::decorated_lines) ;; 

     let change_indentation_in_interval_in_text ~indent (i,j) ~text  =
     let old_lines = indexed_lines text  in 
     let new_lines = Stdl_image.image (
         fun (k,line) -> 
           if (k<i)||(k>j)
           then line
          else (String.make indent ' ')^(Stdl_cull_string.trim_spaces_on_the_left line)
     ) old_lines in 
     String.concat "\n" new_lines ;;
   
   let impose_fixed_indentation_in_interval_in_file ~indent (i,j) fn =
     let old_text=Stdl_io.read_whole_file fn in
     let new_text=change_indentation_in_interval_in_text ~indent (i,j) ~text:old_text   in
     Stdl_io.overwrite_with fn new_text;;     
 
    let indentation_decomposition line =
        let n = String.length line in 
        match List.find_opt(fun j->
            not(List.mem (String.get line (j-1)) [' ';'\t']) 
          )(Stdl_int_range.range 1 n) with 
        None -> (line,"")
        |Some (j0) -> (Stdl_cull_string.beginning (j0-1) line,Stdl_cull_string.cobeginning (j0-1) line) ;;

    (*
    
    indentation_decomposition "abc";;
    indentation_decomposition "\t \tabc";;

    *)


    let shift_indentation_in_line line ~shift_amplitude ~forced =
        if shift_amplitude>=0 
        then (String.make shift_amplitude ' ')^line 
        else 
        let positive_amplitude = (-shift_amplitude) 
        and (indent,bare_text) = indentation_decomposition line in 
        let m = String.length indent in 
        if m>=positive_amplitude
        then (Stdl_cull_string.cobeginning positive_amplitude indent)^bare_text
        else     
        if forced 
        then bare_text 
        else raise(Shift_indentation_in_line_exn(shift_amplitude,line)) ;;  




    let shift_indentation_in_interval_in_text_with (i,j) ~text ~shift_amplitude ~forced =
      let old_lines = indexed_lines text  in 
      let new_lines = Stdl_image.image (
          fun (k,line) -> 
            if (k<i)||(k>j)
            then line
           else shift_indentation_in_line line ~shift_amplitude ~forced
      ) old_lines in 
      String.concat "\n" new_lines ;;
  
  (* ident_interval_in_text_with (2,5) ~text:"1\n2\n3\n4\n5\n6\n7\n" ~tab_width:3;; *)
  
  let shift_indentation_in_interval_in_file_with (i,j) fn ~shift_amplitude ~forced=
     let old_text=Stdl_io.read_whole_file fn in
     let new_text=shift_indentation_in_interval_in_text_with (i,j) ~text:old_text ~shift_amplitude ~forced  in
     Stdl_io.overwrite_with fn new_text;;     

  let occurrences_of_in_at_beginnings_of_lines patt text = 
    let temp1 = enhance (indexed_lines text) in 
    List.filter_map (
       fun (c_idx,l_idx,line) ->
          if String.starts_with ~prefix:patt line  
          then Some(c_idx,l_idx)
          else None
    ) temp1 ;;  

  (*

  let patt1 = "Bart" ;;

  let text1 = "Uv\nBart45\nJBart6\nBartAgain" ;;
     
  let z1 = occurrences_of_in_at_beginnings_of_lines patt1 text1 ;;

  let check_z1 = List.for_all (fun (c_idx,l_idx) -> 
        Substring.is_a_substring_located_at patt1 text1 c_idx
  ) z1 ;;


  *)  

  let put_line_first_in_text line_idx text = 
    if line_idx=1 then text else
    let lines = indexed_lines text in 
    match List.assoc_opt line_idx lines with 
    None -> raise (Put_line_first_bad_line_index_exn (line_idx,List.length lines))
    |Some(the_line) ->
    let lines2 = List.filter_map
       (fun (idx,old_line)->
        if idx=line_idx 
        then None 
        else Some old_line) lines in 
    String.concat "\n" (the_line::lines2) ;;

  (* put_line_first_in_text 4 "1\n2\n3\n4\n5" ;; *)

  let put_line_last_in_text line_idx text = 
    let lines = indexed_lines text in 
    let n = List.length lines in 
    if line_idx=n then text else
    match List.assoc_opt line_idx lines with 
    None -> raise (Put_line_last_bad_line_index_exn (line_idx,n))
    |Some(the_line) ->
    let lines2 = List.filter_map
      (fun (idx,old_line)->
       if idx=line_idx 
       then None 
       else Some old_line) lines in 
    String.concat "\n" (lines2@[the_line]) ;;
  
  (*  put_line_last_in_text 4 "1\n2\n3\n4\n5" ;; *)
    
  let put_line_first_in_file line_idx src_file  =
    let old_text = Stdl_io.read_whole_file src_file  in 
    let new_text = put_line_first_in_text line_idx  old_text in 
    Stdl_io.overwrite_with src_file new_text ;; 

  let put_line_last_in_file line_idx src_file  =
    let old_text = Stdl_io.read_whole_file src_file  in 
    let new_text = put_line_last_in_text line_idx  old_text in 
    Stdl_io.overwrite_with src_file new_text ;; 

  type situation_of_char_inside_line = 
    Inside_a_single_quoted_string 
   |After_backslash_in_double_quoted_string
   |Inside_a_double_quoted_string 
   |Inside_a_starry_comment
   |Inside_a_double_slash_comment 
   |Outside_comments_or_strings ;;  
  

  (* Data type to compute whether 
  the next  linebreak is in a comment, a long string, or neither. 
  The computation returns a pair made of
  the next linebreak's index, with a situation_of_line_inside_text
      indicating if the linebreak is in a comment.
  *)

  type walker = {
      answer_opt : (int * Stdl_situation_of_linebreak_inside_text_t.t) option ;
      next_idx : int ;
      current_state : situation_of_char_inside_line ;
      text : string ;
      text_length : int;
  } ;;

  let result_from_walker_opt w = 
     match w.answer_opt with 
     (Some answer) -> Some(Some answer) 
     | None -> 
     if w.next_idx > w.text_length
     then Some None 
     else None ;;

  let step w =
   let old_idx = w.next_idx in  
   let c = Stdl_strung.get w.text old_idx in 
   match w.current_state with 
    Inside_a_single_quoted_string -> 
       if c='\n' then raise(Unfinished_single_quoted_string) else
       if c = '\'' 
       then { w with 
              next_idx = old_idx +1;
              current_state = Outside_comments_or_strings;
            }
       else 
        let coming_idx = (
          if (Stdl_substring.is_a_substring_located_at "\\\\" w.text old_idx)
             || (Stdl_substring.is_a_substring_located_at "\\'" w.text old_idx) 
          then old_idx+2
          else old_idx+1  ) in  
           { w with 
              next_idx = coming_idx
            }       
     |After_backslash_in_double_quoted_string -> 
       let coming_idx = old_idx+1 in  
          { w with 
              answer_opt = (if c='\n' then Some (old_idx,Stdl_situation_of_linebreak_inside_text_t.Lbit_inside_a_long_double_quoted_string) else None);
              next_idx = coming_idx;
              current_state = Inside_a_double_quoted_string;
            } 
     |Inside_a_double_quoted_string -> 
       if c='\n' then raise(Unfinished_double_quoted_string) else
       if c = '"' 
       then { w with 
              next_idx = old_idx +1 ;
              current_state = Outside_comments_or_strings;
            }
       else 
       let state= (
          if Stdl_substring.is_a_substring_located_at "\\" w.text old_idx
          then After_backslash_in_double_quoted_string 
          else Inside_a_double_quoted_string
       ) in 
       { w with 
              next_idx = old_idx +1 ;
              current_state = state ;
       } 
          
   |Inside_a_starry_comment -> 
      if c = '\n' then {w with answer_opt = Some (old_idx,Stdl_situation_of_linebreak_inside_text_t.Lbit_inside_a_starry_comment)} else    
      (* here we use the fact that /* */-comments cannot be nested in C *)
      let (next_char_situation,coming_idx)=
       (if Stdl_substring.is_a_substring_located_at "*/" w.text old_idx 
      then (Outside_comments_or_strings,old_idx+2) 
      else (Inside_a_starry_comment,old_idx+1)) in   
          { w with 
              next_idx = coming_idx;
              current_state = next_char_situation;
            }  
   |Inside_a_double_slash_comment -> 
      if c = '\n' then {w with answer_opt = Some (old_idx,Stdl_situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings)} else 
      { w with next_idx = old_idx+1;}           
   |Outside_comments_or_strings ->
       if c = '\n' then {w with answer_opt = Some (old_idx,Stdl_situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings)} else 
       if c = '\'' 
       then { w with 
              next_idx = old_idx+1;
              current_state = Inside_a_single_quoted_string;
            }  
       else      
       if c = '"' 
       then { w with 
              next_idx = old_idx+1;
              current_state = Inside_a_double_quoted_string;
            }  
       else    
       if Stdl_substring.is_a_substring_located_at "/*" w.text old_idx
       then { w with 
              next_idx = old_idx+2;
              current_state = Inside_a_starry_comment;
            } 
       else     
       if Stdl_substring.is_a_substring_located_at "//" w.text old_idx
       then { w with 
              next_idx = old_idx+2;
              current_state = Inside_a_double_slash_comment;
            }  
       else { w with 
              next_idx = old_idx+1
            }        
      ;;      

let rec iterate w = 
   match result_from_walker_opt w with 
    Some res -> res 
    | None -> iterate (step w) ;;

 
let char_state_of_linebreak_state = function
    Stdl_situation_of_linebreak_inside_text_t.Lbit_inside_a_long_double_quoted_string -> Inside_a_double_quoted_string 
   |Stdl_situation_of_linebreak_inside_text_t.Lbit_inside_a_starry_comment -> Inside_a_starry_comment
   |Stdl_situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings -> Outside_comments_or_strings ;;

let initial_walker txt idx linebreak_state=
   {
      answer_opt = None ;
      next_idx = idx ;
      current_state = char_state_of_linebreak_state linebreak_state;
      text = txt ;
      text_length = String.length txt;
  } ;;

let next_newline_inside_or_outside_cee_comments_opt 
  txt idx unfinished_comment=
  let w = initial_walker txt idx unfinished_comment in 
  iterate w;;

let pusher_for_lines_inside_or_outside_cee_comments_or_dq_strings (_,walker) =
  let (whole_text,total_length,treated_lines,next_idx_to_be_treated,
  unfinished_comment_or_dqs)=walker in  
  if next_idx_to_be_treated > total_length 
  then (Some(List.rev treated_lines),walker) 
  else 
  match next_newline_inside_or_outside_cee_comments_opt  
        whole_text next_idx_to_be_treated unfinished_comment_or_dqs with
  None -> 
      let rest_of_text = 
        Stdl_cull_string.cobeginning (next_idx_to_be_treated-1) whole_text  in 
      (Some(List.rev ((rest_of_text,unfinished_comment_or_dqs) 
      :: treated_lines)),walker) 
  |Some(newline_idx,unfinished_comment2) ->
     let line= Stdl_cull_string.interval whole_text next_idx_to_be_treated (newline_idx-1) in 
     (None,
  (whole_text,total_length,(line,unfinished_comment_or_dqs)::treated_lines,newline_idx+1,unfinished_comment2));; 

let rec iterator_for_lines_inside_or_outside_cee_comments_or_dq_strings 
  outer_walker =
   let (end_result_opt,_inner_walker) = outer_walker in 
   match end_result_opt with 
   Some end_result -> end_result 
   | None -> iterator_for_lines_inside_or_outside_cee_comments_or_dq_strings 
    (pusher_for_lines_inside_or_outside_cee_comments_or_dq_strings outer_walker);; 


let lines_inside_or_outside_cee_comments_or_dq_strings text = 
  iterator_for_lines_inside_or_outside_cee_comments_or_dq_strings 
  (None,(text,String.length text,[],1,Stdl_situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings)) ;;

(*  


 let txt1 = String.concat "\n" [
   "1 When";"2 The "; "3 /* Saints"; "4 Go" ; "5 Marching */ In"; "6 Oh"
   ]  ;; 

lines_inside_or_outside_cee_comments_or_dq_strings txt1 ;; 

let txt2 = String.concat "\n" [
   "1 When";"2 The "; "3 '\\' /* Saints */"; "4 Go" ; "5 Marching In"; "6 Oh"
   ]  ;; 

lines_inside_or_outside_cee_comments_or_dq_strings txt2 ;;

let txt3 = String.concat "\n" [
   "1 When";"2 The "; "3 \"/*\" Saints"; "4 Go" ; "5 Marching \"*/\" In"; "6 Oh"
   ]  ;; 

lines_inside_or_outside_cee_comments_or_dq_strings txt3 ;; 

*)
  
  let modify_interval_inside_text f text i j =
     let (before,itv,after) = tripartition_associated_to_interval text i j in 
     let new_itv = f itv in 
     before ^ new_itv ^ after ;;

  let modify_interval_inside_file f src_file i j =
    let old_text = Stdl_io.read_whole_file src_file  in 
    let new_text = modify_interval_inside_text f old_text i j in 
    Stdl_io.overwrite_with src_file new_text ;;    

  let insert_after_line_inside_text text ~line_number ~inserted_snippet=
      let (before,itv,after) = tripartition_associated_to_interval text line_number line_number in 
      let new_itv = itv^"\n"^inserted_snippet in 
      before ^ new_itv ^ after ;;
 
  let insert_before_line_inside_text text ~line_number ~inserted_snippet=
      let (before,itv,after) = tripartition_associated_to_interval text line_number line_number in 
      let new_itv = inserted_snippet^"\n"^itv in 
      before ^ new_itv ^ after ;;

  let insert_after_line_inside_file src_file ~line_number ~inserted_snippet=
     let old_text = Stdl_io.read_whole_file src_file  in 
     let new_text = insert_after_line_inside_text old_text ~line_number ~inserted_snippet in 
     Stdl_io.overwrite_with src_file new_text ;;   

  let insert_before_line_inside_file src_file ~line_number ~inserted_snippet=
     let old_text = Stdl_io.read_whole_file src_file  in 
     let new_text = insert_before_line_inside_text old_text ~line_number ~inserted_snippet in 
     Stdl_io.overwrite_with src_file new_text ;;  

  let extract_ocaml_names_in_text full_text i j =
    let subtext = interval full_text i j in 
    let lines = lines subtext in
    List.filter_map(fun line ->
      if not (String.starts_with ~prefix:"let " line) 
      then None 
      else
      let s= Stdl_cull_string.two_sided_cutting ("let ","") line in 
      let i = String.index s ' ' in 
      Some(Stdl_cull_string.beginning i s)
    ) lines ;;

    let extract_ocaml_names_in_file ap i j =
      let full_text = Stdl_io.read_whole_file ap in 
      extract_ocaml_names_in_text full_text i j;;  

  let is_a_lowercase_letter = (
     fun c -> let i = int_of_char c in 
     (97<=i) && (i<=122)
  ) ;;

  let extract_field_names_from_type_definition_in_text full_text i j =
        let subtext = interval full_text i j in 
        let lines = lines subtext in
        List.filter_map(fun untrimmed_line ->
          let line = Stdl_cull_string.trim_spaces untrimmed_line in 
          if not (is_a_lowercase_letter(String.get line 0)) 
          then None 
          else
          match Stdl_string_find_char.from_inclusive_opt
          (fun c->List.mem c [' ';':']) line 1 with 
          None -> None
          |Some i->
          Some(Stdl_cull_string.beginning (i-1) line)
        ) lines ;;
        
  let extract_field_names_from_type_definition_in_file ap i j =
    let full_text = Stdl_io.read_whole_file ap in 
    extract_field_names_from_type_definition_in_text full_text i j;;  

  


  end ;;   


  let closeup_around_index = Private.closeup_around_index ;;
  let copy_interval_from_file_to_file = Private.copy_interval_from_file_to_file ;;
  let copy_interval_from_text_to_text = Private.copy_interval_from_text_to_text ;; 

  let duplicate_interval_in_file = Private.duplicate_interval_in_file ;;
  let duplicate_interval_in_text = Private.duplicate_interval_in_text ;;

  let enhanced_indexed_lines s= Private.enhance (Private.indexed_lines s);;
  
  (*
  
  enhanced_indexed_lines "a\nb";;
  enhanced_indexed_lines "\na\nb";;
  enhanced_indexed_lines "a\nb\n";;
  
  *)

  let extract_field_names_from_type_definition_in_file = 
    Private.extract_field_names_from_type_definition_in_file ;;

  let extract_field_names_from_type_definition_in_text = 
    Private.extract_field_names_from_type_definition_in_text ;;  

  let extract_ocaml_names_in_file = Private.extract_ocaml_names_in_file ;;

  let extract_ocaml_names_in_text = Private.extract_ocaml_names_in_text ;;
  
  

  let findreplace_in_interval (x,y) s i j=
      let (part1,old_part2,part3) = Private.tripartition_associated_to_interval s i j in 
      let new_part2 = Stdl_replace_inside.replace_inside_text (x,y) old_part2 in 
      part1^new_part2^part3 ;; 

let findreplace_in_interval_in_file (x,y) fn i j=
      let old_text=Stdl_io.read_whole_file fn in
      let new_text=findreplace_in_interval (x,y) old_text i j  in
      Stdl_io.overwrite_with fn new_text;;     
  

(* findreplace_in_interval ("\n"," ") "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)

  let impose_fixed_indentation_in_interval_in_file = Private.impose_fixed_indentation_in_interval_in_file ;;   


  let indexed_lines = Private.indexed_lines ;;
  
  (*
  
  indexed_lines "a\nb";;
  indexed_lines "\na\nb";;
  indexed_lines "a\nb\n";;
  
  *)

  let insert_after_line_inside_file = Private.insert_after_line_inside_file ;;
  let insert_after_line_inside_text = Private.insert_after_line_inside_text ;;

  let insert_before_line_inside_file = Private.insert_before_line_inside_file ;;
  let insert_before_line_inside_text = Private.insert_before_line_inside_text ;;


  let interval = Private.interval ;;

   let line_index_from_char_index text char_idx=
      1+(Private.number_of_lines_in_char_interval text 1 char_idx);;

  let lines text= Stdl_image.image snd (indexed_lines text);;

  let lines_inside_or_outside_cee_comments_or_dq_strings = Private.lines_inside_or_outside_cee_comments_or_dq_strings ;; 

  let modify_interval_inside_file = Private.modify_interval_inside_file ;;

  let modify_interval_inside_text = Private.modify_interval_inside_text ;;
  let occurrences_of_in_at_beginnings_of_lines = Private.occurrences_of_in_at_beginnings_of_lines ;; 

  let put_line_first_in_file = Private.put_line_first_in_file ;; 

  let put_line_first_in_text = Private.put_line_first_in_text ;; 

  let put_line_last_in_file = Private.put_line_last_in_file ;; 
  
  let put_line_last_in_text = Private.put_line_last_in_text ;; 

  let remove_lower_interval text i=
    let old_indexed_lines=indexed_lines text in
    let temp2=List.filter (fun (k,_)->(i>k)) old_indexed_lines  in
    let new_indexed_lines=Stdl_image.image snd temp2 in
    String.concat "\n" new_indexed_lines;; 

  let remove_lower_interval_in_file fn i =
      let old_text=Stdl_io.read_whole_file fn in
      let new_text=remove_lower_interval old_text i   in
     Stdl_io.overwrite_with fn new_text;;     

  let remove_interval text i j=
    let old_indexed_lines=indexed_lines text in
    let temp2=List.filter (fun (k,_)->(i>k)||(k>j)) old_indexed_lines  in
    let new_indexed_lines=Stdl_image.image snd temp2 in
    String.concat "\n" new_indexed_lines;; 
  
  let remove_interval_in_file fn i j=
      let old_text=Stdl_io.read_whole_file fn in
      let new_text=remove_interval old_text i j  in
     Stdl_io.overwrite_with fn new_text;;   
  
  let remove_lines_containing_substring_in_text pattern text =
     let old_indexed_lines=indexed_lines text in
     let temp2=List.filter (fun (_,line)->not(Stdl_substring.is_a_substring_of pattern line)) old_indexed_lines  in
     let new_indexed_lines=Stdl_image.image snd temp2 in
     String.concat "\n" new_indexed_lines;; 
   
   let remove_lines_containing_substring_in_file pattern fn=
       let old_text=Stdl_io.read_whole_file fn in
       let new_text=remove_lines_containing_substring_in_text pattern old_text  in
      Stdl_io.overwrite_with fn new_text;;   
  


let shift_indentation_in_interval_in_file_with = Private.shift_indentation_in_interval_in_file_with ;;
let shift_indentation_in_interval_in_text_with = Private.shift_indentation_in_interval_in_text_with ;;

let suppress_linebreaks_in_interval text i j=
    let (part1,old_part2,part3) = Private.tripartition_associated_to_interval text i j in 
    let new_part2 = String.concat "" (lines old_part2) in 
    part1^new_part2^part3 ;; 
  
  (* suppress_linebreaks_in_interval "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)
  
let suppress_linebreaks_in_interval_in_file fn i j=
    let old_text=Stdl_io.read_whole_file fn in
    let new_text=suppress_linebreaks_in_interval old_text i j  in
    Stdl_io.overwrite_with fn new_text;;     

let tripartition_associated_to_interval = Private.tripartition_associated_to_interval ;;    



end;;






module Stdl_cee_common=struct

(*

#use"lib/Cee_language/cee_common.ml";;

*)

module Private = struct
let marker = "uAiuoAUYyo" ;; 

end ;;

let add_extra_ending_in_filename ~extra fn = 
  let (basename,ending) = Stdl_cull_string.split_wrt_rightmost fn '.'  in 
  (basename^"_"^Private.marker^"_"^extra^"."^ending) ;;



end;;






module Stdl_cee_inclusion_item_t=struct

(*

#use"lib/Cee_language/cee_inclusion_item_t.ml";;

*)


type  t = {
 line_number : int ;
 inclusion_index : int ;
 included_file_opt : string option; 
} ;; 

end;;






module Stdl_cee_prawn_t=struct

(*

#use"lib/Cee_language/cee_prawn_t.ml";;


The prawn of a file is the set of "accepted" (after preprocessing the
conditional directives) zones 
without conditional directives in a C file.
The integers in the int list parameter are the indices of the zones.


*)


type  t = P of int list ;; 

end;;






module Stdl_cee_shadow_t=struct

(*

#use"lib/Cee_language/cee_shadow_t.ml";;

A shadow is a prawn, with an additional parameter
telling the total number of zones in the file.

*)

type  t = 
   Sh of int * Stdl_cee_prawn_t.t ;; 

end;;






module Stdl_directory_name_t=struct

(*

Directories name, with the trailing slash removed.

#use"lib/directory_name_t.ml";;

*)

type t=D of string;;

           

end;;






module Stdl_directory_name=struct

(*

Directories name, with the trailing slash removed.

#use"lib/directory_name.ml";;

*)



exception Non_directory of string;;
exception File_not_found of string * (Stdl_directory_name_t.t list);;

let find_file_with_directory_list fname l=
  match List.find_map (
     fun (Stdl_directory_name_t.D s_dir) ->
      let full_path = s_dir^"/"^fname in 
      if Sys.file_exists full_path 
      then Some(Stdl_absolute_path.of_string full_path)
      else None
  ) l with 
  None -> raise(File_not_found(fname,l))
  |Some(ap) -> ap;;


let of_string s=
  let temp1=Stdl_tools_for_absolute_path.of_string s in
  if Sys.is_directory temp1
  then Stdl_directory_name_t.D(Stdl_tools_for_absolute_path.remove_trailing_slash temp1)
  else raise(Non_directory(s));;

let connectable_to_subpath (Stdl_directory_name_t.D s)=s^"/";;



end;;






module Stdl_cee_text=struct

(*

#use"lib/Cee_language/cee_text.ml";;

*)

exception Lonely_Elif_exn of int ;;
exception Lonely_Else_exn of int ;;
exception Lonely_Endif_exn of int ;;
exception Double_Else_exn of int ;;
exception Standardize_inclusion_line_exn of string ;;  
exception Add_extra_inclusion_line_exn of string ;;
exception Unfinished_fiamengo_inclusion of string ;; 
exception Helper_for_computing_exact_locations_exn ;;
  
module Private = struct 

  let str_order = Stdl_total_ordering.lex_for_strings ;;
  let str_mem = Stdl_ordered.mem str_order ;;  

  let i_order = Stdl_total_ordering.for_integers ;;
   let i_merge = Stdl_ordered.merge i_order ;; 

  let intstr_order = Stdl_total_ordering.product i_order str_order ;;

  let intstr_sort = Stdl_ordered.sort intstr_order;;

  let lines_inside_or_outside_cee_comments_or_dq_strings = 
    Stdl_lines_in_text.lines_inside_or_outside_cee_comments_or_dq_strings ;;

  let indexed_lines_inside_or_outside_cee_comments_or_dq_strings text = 
     Stdl_int_range.index_everything (lines_inside_or_outside_cee_comments_or_dq_strings text) ;; 


   let sil_merge ox oy=
   let rec tempf=(function (u,v,accu)->
   if u=[] then (List.rev_append(accu)(v)) else
   if v=[] then (List.rev_append(accu)(u)) else
   let (xu,lxu)=List.hd(u) and yu=List.tl(u) 
   and (xv,lxv)=List.hd(v) and yv=List.tl(v) in
   match str_order(xu)(xv) with
     Stdl_total_ordering_result_t.Lower->tempf(yu,v,(xu,lxu)::accu)
   |Stdl_total_ordering_result_t.Equal->tempf(yu,yv,(xu,i_merge lxu lxv)::accu)
   |Stdl_total_ordering_result_t.Greater->tempf(u,yv,(xv,lxv)::accu)
   ) in
   tempf(ox,oy,[]);;
 
 let rec sil_sort  x=
   if List.length(x)<2
   then x
   else 
   let temp1=Stdl_partition_list.split_in_half(x) in
   let y1=sil_sort(fst temp1)
   and y2=sil_sort(snd temp1) in
   sil_merge y1 y2;;      
 

type line_beginning = 
  Lb_If
 |Lb_Elif
 |Lb_Else 
 |Lb_Endif 
 |Lb_Usual ;; 

   

 module Line_beginning = struct 

  let data = [
    "if", Lb_If;
    "ifdef", Lb_If; 
    "ifndef", Lb_If;    
    "elif", Lb_Elif;  
    "else", Lb_Else;
    "endif", Lb_Endif
]  ;;  

let compute line =
  if not(String.starts_with line ~prefix:"#")
  then Lb_Usual  
  else  
  match Stdl_string_find_char.from_inclusive_opt (fun c->
          not(List.mem c [' ';'\t';'\r'])
        ) line 2 with 
  None -> Lb_Usual
  |Some idx1 -> 
    (match List.find_opt (fun (lb_name,_lb)->
      Stdl_substring.is_a_substring_located_at lb_name line idx1
      ) data with 
    None -> Lb_Usual
    |Some (_,lb) -> lb 
    );;

end ;;  


 


type small_space_t = {
   namespace : int ;
   start_idx : int ;
   end_idx  : int ;
} ;;

type guard_pattern_indicators_t = {
    first_ivy_index_opt : int option ;
    elsie_or_elif_for_first_index_opt : int option;
    endif_index_for_first_ivy_opt : int option ;
    ivy_index_after_first_ivy_opt : int option;
} ;; 




module Guard_Pattern = struct
let origin = {
  first_ivy_index_opt = None;
  elsie_or_elif_for_first_index_opt = None;
  endif_index_for_first_ivy_opt = None ;
  ivy_index_after_first_ivy_opt = None;
} ;; 

let test gpi=
  (gpi.endif_index_for_first_ivy_opt <> None) &&
  (gpi.elsie_or_elif_for_first_index_opt = None) &&
  (gpi.ivy_index_after_first_ivy_opt = None)  ;;   
  
end ;;

module Walker = struct 

   module Walker_Object = struct 

    type t = {
      lines : (int *(string * Stdl_situation_of_linebreak_inside_text_t.t)) list ;
      current_namespace : int ;
      preceding_namespaces : int list ;
      smallest_unused_namespace_index : int ;
      small_space_start_index_opt : int option ;
      unfinished_condition : bool;
      last_directive_was_an_else : bool;
      treated : small_space_t list;
      gpi : guard_pattern_indicators_t ;
    } ;;  
  let make text = {
    lines = indexed_lines_inside_or_outside_cee_comments_or_dq_strings text ;
    current_namespace = 0 ;
    preceding_namespaces = [] ;
    smallest_unused_namespace_index = 1;
    small_space_start_index_opt = None ;
    unfinished_condition = false;
    last_directive_was_an_else = false;
    treated = [];
    gpi = Guard_Pattern.origin ;
  } ;; 

  let add_to_treated small_space w = {
       w with 
       treated = small_space :: (w.treated)   
    } ;; 

  let condition_is_unfinished w = w.unfinished_condition ;; 
  let get_current_namespace w = w.current_namespace ;; 
  let get_endif_index_for_first_ivy_opt w = w.gpi.endif_index_for_first_ivy_opt ;;
  let get_elsie_or_elif_for_first_index_opt w = w.gpi.elsie_or_elif_for_first_index_opt ;;
  let get_first_ivy_index_opt w = w.gpi.first_ivy_index_opt ;;
  let get_guard_pattern_indicators w = w.gpi ;;
  let get_ivy_index_after_first_ivy_opt w = w.gpi.ivy_index_after_first_ivy_opt ;;
  let get_lines w = w.lines ;;
  let get_preceding_namespaces w = w.preceding_namespaces ;;
  let get_small_space_start_index_opt w = w.small_space_start_index_opt ;;
  let get_smallest_unused_namespace_index w = w.smallest_unused_namespace_index ;;
  let get_treated w = w.treated ;;

  let last_directive_was_an_else w = w.last_directive_was_an_else ;;

  let register_endif_index_for_first_ivy w line_idx =
      let new_gpi = {w.gpi with endif_index_for_first_ivy_opt = Some line_idx} in 
      {w with gpi = new_gpi} ;;
  let register_elsie_or_elif_for_first_index w line_idx =
      let new_gpi = {w.gpi with elsie_or_elif_for_first_index_opt = Some line_idx} in 
      {w with gpi = new_gpi} ;;

  let register_first_ivy_index w line_idx =
    let new_gpi = {w.gpi with first_ivy_index_opt = Some line_idx} in 
    {w with gpi = new_gpi} ;;
  
  let register_ivy_index_after_first_ivy w line_idx =  
    let new_gpi = {w.gpi with ivy_index_after_first_ivy_opt = Some line_idx} in 
    {w with gpi = new_gpi} ;;

  let register_small_space_start_index w line_idx =    
      {
        w with 
        small_space_start_index_opt = Some line_idx 
      } ;; 

  let set_condition_to_finished w =    
      {
        w with 
        unfinished_condition = false
      }   ;;
 
  let set_lines w new_set_of_lines=    
      {
        w with 
        lines = new_set_of_lines
      }   ;;    

  let set_namespace_data w 
     ~v_current_namespace
     ~v_smallest_unused_namespace_index
     ~v_preceding_namespaces
      =  {
        w with 
        current_namespace = v_current_namespace ;
        smallest_unused_namespace_index = v_smallest_unused_namespace_index;
        preceding_namespaces = v_preceding_namespaces
      } ;;

  let set_unfinished_condition_data w 
      ~current_cond_is_unfinished
      ~current_line_is_an_elsie
    ={
      w with
        small_space_start_index_opt = None ;
        unfinished_condition = current_cond_is_unfinished;
        last_directive_was_an_else = current_line_is_an_elsie ;
     } ;;



   end ;;

  let register_new_small_space_if_needed old_w w line_idx= 
    match Walker_Object.get_small_space_start_index_opt old_w with 
    None -> w
    |Some start_index ->
      let small_space = {
       namespace = Walker_Object.get_current_namespace w ;
       start_idx = start_index ;
       end_idx  = line_idx -1 ;
      } in 
      Walker_Object.add_to_treated small_space w ;; 

  let deal_with_namespace_data old_w w line_beginning= 
     match line_beginning with 
     Lb_If ->
      let old_idx = Walker_Object.get_smallest_unused_namespace_index old_w in 
      let v_current_namespace=old_idx
      and v_smallest_unused_namespace_index=old_idx + 1
      and v_preceding_namespaces=(Walker_Object.get_current_namespace old_w) :: (Walker_Object.get_preceding_namespaces old_w) in 
      Walker_Object.set_namespace_data w 
        ~v_current_namespace
        ~v_smallest_unused_namespace_index
        ~v_preceding_namespaces
     
    | Lb_Endif ->
      let preceding = Walker_Object.get_preceding_namespaces old_w in 
      let v_current_namespace=List.hd preceding
      and v_smallest_unused_namespace_index=Walker_Object.get_smallest_unused_namespace_index old_w
      and v_preceding_namespaces= List.tl preceding in 
      Walker_Object.set_namespace_data w 
        ~v_current_namespace
        ~v_smallest_unused_namespace_index
        ~v_preceding_namespaces
   | Lb_Elif | Lb_Else | Lb_Usual -> w ;;

   let deal_with_guard_pattern_detection old_w w line_beginning line_idx = 
    match line_beginning with 
    Lb_If ->
      (if ((Walker_Object.get_first_ivy_index_opt old_w) = None)
       then Walker_Object.register_first_ivy_index w line_idx
       else 
       if ((Walker_Object.get_endif_index_for_first_ivy_opt old_w) <> None) &&
          ((Walker_Object.get_ivy_index_after_first_ivy_opt old_w) = None)
       then Walker_Object.register_ivy_index_after_first_ivy w line_idx  
       else  w
       )
   | Lb_Elif | Lb_Else ->
    (if ((Walker_Object.get_first_ivy_index_opt old_w) <> None) &&
         ((Walker_Object.get_current_namespace old_w)=1) &&
         ((Walker_Object.get_elsie_or_elif_for_first_index_opt old_w) = None)
      then Walker_Object.register_elsie_or_elif_for_first_index w line_idx 
      else w  
      )
     
   | Lb_Endif ->
    (if ((Walker_Object.get_first_ivy_index_opt old_w) <> None) &&
        ((Walker_Object.get_current_namespace old_w)=1) 
     then Walker_Object.register_endif_index_for_first_ivy w line_idx
     else w  
   )
   | Lb_Usual -> w ;;


  let directive_step old_w line_idx line line_beginning= 
    if ((Walker_Object.get_current_namespace old_w) = 0) && (line_beginning = Lb_Elif) then raise (Lonely_Elif_exn(line_idx)) else 
    if ((Walker_Object.get_current_namespace old_w) = 0) && (line_beginning = Lb_Else) then raise (Lonely_Else_exn(line_idx)) else
    if ((Walker_Object.get_current_namespace old_w) = 0) && (line_beginning = Lb_Endif) then raise (Lonely_Endif_exn(line_idx)) else
    if (Walker_Object.last_directive_was_an_else old_w) && (line_beginning = Lb_Else) then raise (Double_Else_exn(line_idx)) else
    let current_cond_is_unfinished = (
      if List.mem line_beginning [Lb_If;Lb_Elif]
      then String.ends_with line ~suffix:"\\"
      else false 
    ) in 
    let w1 =
      Walker_Object.set_unfinished_condition_data old_w 
       ~current_cond_is_unfinished
       ~current_line_is_an_elsie:(line_beginning = Lb_Else) in 
   let w2 = register_new_small_space_if_needed old_w w1 line_idx in 
   let w3 = deal_with_namespace_data old_w w2 line_beginning in 
   let w4 = deal_with_guard_pattern_detection old_w w3 line_beginning line_idx in
   w4 ;;
  let inner_usual_step w line_idx line = 
    match Walker_Object.get_small_space_start_index_opt w  with 
    (Some _) -> w 
    |None -> 
    if not(Walker_Object.condition_is_unfinished w)
    then Walker_Object.register_small_space_start_index w line_idx 
    else
    if String.ends_with line ~suffix:"\\"
    then w
    else Walker_Object.set_condition_to_finished w ;; 

  let usual_step w line_idx line =
    let w2 = inner_usual_step w line_idx line in 
    if (Walker_Object.get_lines w2) = []
    then register_new_small_space_if_needed w2 w2 (line_idx+1)
    else w2 ;;    
    

  let step old_w = 
    let (line_idx,(line,linebreak_state)) = List.hd (Walker_Object.get_lines old_w) 
    and w = Walker_Object.set_lines old_w  
          (List.tl(Walker_Object.get_lines old_w)) in 
    let line_beginning = Line_beginning.compute line in 
    if linebreak_state <> Stdl_situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings 
    then usual_step w line_idx line
    else     
    match line_beginning with 
    Lb_Usual -> usual_step w line_idx line 
   |Lb_If |Lb_Elif |Lb_Else |Lb_Endif -> 
     directive_step w line_idx line line_beginning ;;
  

  let rec iterate w =
    if (Walker_Object.get_lines w) = [] 
    then List.rev (Walker_Object.get_treated w) 
    else iterate (step w) ;;   

  
  let rec iterate_for_guard_pattern_detection w =
    if (Walker_Object.get_lines w) = [] 
    then Walker_Object.get_guard_pattern_indicators w 
    else iterate_for_guard_pattern_detection (step w) ;;   
    
let make = Walker_Object.make ;;

end ;;  


let compute_small_spaces_in_text text =
  Walker.iterate(Walker.make text) ;; 

(*

let text1 = String.concat "\n" 
[
  "#if 1";
  "2";
  "#elif 3";
  "4";
  "#if 5";
  "6";
  "#endif 7";
  "#endif 8";
  "9";
  "#if 10\\";
  "11\\";
  "12\\";
  "13";
  "14";
  "#endif 15"
] ;;

let check1 = compute_small_spaces_in_text text1 ;;

let text2 = String.concat "\n" 
[
  "1";
  "2";
  "#if 3\\";
  "4\\";
  "5";
  "#elif 6\\";
  "7";
  "#if 8";
  "9";
  "#endif 10";
  "11";
  "12";
  "#endif 13";
  "14";
] ;;

let check2 = compute_small_spaces_in_text text2 ;;

*)   

let compute_small_spaces_in_file ap = 
 compute_small_spaces_in_text (Stdl_io.read_whole_file ap)  ;;  

let test_text_for_guard_pattern text =
   Guard_Pattern.test 
   (Walker.iterate_for_guard_pattern_detection (Walker.make text)) ;; 

(*

let text1 = String.concat "\n" 
[
  "1";
  "#if 2";
  "3";
  "#endif 4";
  "#if 5";
  "6";
  "#endif 7";
  "8"
] ;;

let check1 = Walker.iterate_for_guard_pattern_detection (Walker.make text1) ;;
let chuck1 = test_text_for_guard_pattern text1 ;;

let text2 = String.concat "\n" 
[
  "1";
  "#if 2";
  "3";
  "4";
  "#elif 5";
  "6";
  "#endif 7";
  "8"
] ;;

let check2 = Walker.iterate_for_guard_pattern_detection (Walker.make text2) ;;
let chuck2 = test_text_for_guard_pattern text2 ;;

let text3 = String.concat "\n" 
[
  "1";
  "#if 2";
  "3";
  "#if 4";
  "5";
  "#elif 6";
  "7";
  "#endif 8";
  "9";
  "#if 10";
  "11";
  "#endif 12";
  "#endif 13";
  "14"
] ;;

let check3 = Walker.iterate_for_guard_pattern_detection (Walker.make text3) ;;
let chuck3 = test_text_for_guard_pattern text3 ;;


*)

let find_directive_from_list_opt line directives=
      if not(String.starts_with line ~prefix:"#")
      then None
      else  
      match Stdl_string_find_char.from_inclusive_opt (fun c->
              not(List.mem c [' ';'\t';'\r'])
            ) line 2 with 
      None -> None
      |Some idx1 -> 
        List.find_opt (fun drctv ->
          Stdl_substring.is_a_substring_located_at drctv line idx1
          ) directives ;;

let text_has_ivy text =
   let lines = lines_inside_or_outside_cee_comments_or_dq_strings text in 
   List.exists (
    fun (line,linebreak_state) -> 
      (linebreak_state = Stdl_situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings) &&
      (find_directive_from_list_opt line ["if"])<>None
   ) lines ;;          

exception First_ivy_in_text_exn ;;   
let first_ivy_in_text text =
   let lines = indexed_lines_inside_or_outside_cee_comments_or_dq_strings text in 
   match List.find_opt (fun (_line_idx,(line,linebreak_state))->
      (linebreak_state=Stdl_situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings) && 
      (find_directive_from_list_opt line ["if"])<>None
   ) lines with 
   None -> raise First_ivy_in_text_exn 
   |Some(line_idx,_) -> line_idx ;;
   
exception Last_endif_in_text_exn ;;   
let last_endif_in_text text =
  let lines = List.rev(indexed_lines_inside_or_outside_cee_comments_or_dq_strings text) in 
  match List.find_opt (fun (_line_idx,(line,linebreak_state))->
          (linebreak_state=Stdl_situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings) && 
         (find_directive_from_list_opt line ["endif"])<>None
  ) lines with 
   None -> raise Last_endif_in_text_exn 
  |Some(line_idx,_) -> line_idx ;;   
 
let put_first_ivy_on_first_line text = 
  let line_idx = first_ivy_in_text text in 
  Stdl_lines_in_text.put_line_first_in_text line_idx text ;;

let put_last_endif_on_last_line text = 
  let line_idx = last_endif_in_text text in 
  let temp_text = Stdl_lines_in_text.put_line_last_in_text line_idx text in 
  let temp_lines = List.rev(lines_inside_or_outside_cee_comments_or_dq_strings temp_text) in 
  let ((temp_last_line,_),temp_tl) = Stdl_list_again.head_with_tail temp_lines in 
  (* Any comments after the #endif must be moved before it *)
  let after= Stdl_cull_string.two_sided_cutting  ("#endif","") temp_last_line in 
  let all_lines_except_last = Stdl_image.image fst temp_tl in 
  let almost_all_lines=(
     match Stdl_string_find_char.from_inclusive_opt (fun c->
              not(List.mem c [' ';'\t';'\r'])
            ) after 1 with 
      None -> all_lines_except_last
      |Some idx -> (Stdl_cull_string.cobeginning (idx-1) after) :: all_lines_except_last
  )in
  let lines = List.rev("#endif" :: almost_all_lines) in 
  String.concat "\n" lines;;  

let emphatize_first_ivy_and_last_endif text = 
  let text2 = put_first_ivy_on_first_line text in 
  put_last_endif_on_last_line text2 ;;

let standardize_guard_in_text_opt text = 
  if test_text_for_guard_pattern text 
  then let new_text = emphatize_first_ivy_and_last_endif text in 
       if new_text = text 
       then None 
       else Some(new_text)
  else None ;;   

 let marker_for_cd_defined_region = "cgmvgtkcxvvxckt" ;;  
 let parametrized_marker_for_cd_defined_region name_for_watermarkable_file k =
   let sk = string_of_int k in 
   marker_for_cd_defined_region^name_for_watermarkable_file^sk^marker_for_cd_defined_region ;;
 
 let parametrized_line_for_cd_defined_region name_for_watermarkable_file cd_idx =  
   "char* unused_string"^(string_of_int cd_idx)^"=\""^
   (parametrized_marker_for_cd_defined_region name_for_watermarkable_file cd_idx)^"\";" ;;
 
 let is_in_interval x (a,b) = (a<=x) && (x<=b) ;; 
 
 let is_in_interval_union x intervals =
    List.exists (is_in_interval x) intervals ;;
 


 let crop_using_prawn old_text (Stdl_cee_prawn_t.P(accepted_indices)) =
   let lines = Stdl_lines_in_text.indexed_lines old_text 
   and ssps = compute_small_spaces_in_text old_text  in 
   let accepted_ssps = Stdl_image.image(
      fun ssp_idx ->
       (ssp_idx,List.nth ssps (ssp_idx-1)) 
   ) accepted_indices in 
   let accepted_intervals = Stdl_image.image (
     fun (_,ssp) -> (ssp.start_idx,ssp.end_idx)
   ) accepted_ssps in 
   let accepted_lines = List.filter_map (
      fun (line_idx,line) ->
        if is_in_interval_union line_idx accepted_intervals 
        then Some line 
       else None
   ) lines in 
   String.concat "\n" accepted_lines ;;
 

   let pairs_of_indices_for_watermarking indexed_ssps = 
    List.filter_map (
      fun (ssp_idx,ssp) ->
        if ssp.namespace = 0 then None else 
        Some(ssp.start_idx,ssp_idx) 
    ) indexed_ssps ;;
  
  let tattoo_regions_between_conditional_directives ~name_for_included_file text = 
     let lines = Stdl_lines_in_text.indexed_lines text 
     and ssps = compute_small_spaces_in_text text in 
     let indexed_ssps = Stdl_int_range.index_everything ssps in
     let pairs = pairs_of_indices_for_watermarking indexed_ssps in 
     let temp1 = Stdl_image.image (
        fun (line_idx,line) ->
          match List.assoc_opt line_idx pairs with 
          None -> [line]
          | (Some ssp_idx) ->
             [parametrized_line_for_cd_defined_region name_for_included_file ssp_idx;line]
     ) lines in  
     (String.concat "\n" (List.flatten temp1)) ;;

(*

let text1 = String.concat "\n" 
[
   "#if 1";
   "2";
   "#elif 3";
   "4";
   "#if 5";
   "6";
   "#endif 7";
   "#endif 8";
   "9";
   "#if 10\\";
   "11\\";
   "12\\";
   "13";
   "14";
   "#endif 15"
] ;;

let text2 = watermark_text text1 ;;

print_string(text2);;

print_string(rewrite_using_watermarks text1 text2);;

let text3 = parametrized_line_for_cd_defined_region 3;;

print_string(rewrite_using_watermarks text1 text3);;

*)

let generic_included_file_opt (opening_char,closing_char) line =
  if not(String.starts_with line ~prefix:"#")
  then None  
  else  
  match Stdl_string_find_char.from_inclusive_opt (fun c->
          not(List.mem c [' ';'\t';'\r'])
        ) line 2 with 
  None -> None
  |Some idx1 -> 
  if not(Stdl_substring.is_a_substring_located_at "include" line idx1)
  then None
  else   
  match Stdl_string_find_char.from_inclusive_opt (fun c->
        not(List.mem c [' ';'\t';'\r'])
  ) line (idx1+7) with 
  None -> None
  |Some idx2 -> 
  if (Stdl_strung.get line idx2)<>opening_char
  then None
  else    
  match Stdl_string_find_char.from_inclusive_opt (fun c->
           c = closing_char
  ) line (idx2+1) with 
  None -> None
  |Some idx3 ->      
    Some (Stdl_cull_string.interval line (idx2+1) (idx3-1))
    ;;

let included_local_file_opt = generic_included_file_opt ('"','"');;
let included_nonlocal_file_opt = generic_included_file_opt ('<','>');;

let is_an_inclusion_line line 
= ((included_local_file_opt line)<>None)||
  ((included_nonlocal_file_opt line)<>None) ;;


let marker_for_inclusion_highlighting = "cgmvgtkcxvvxckt" ;;  
let parametrized_marker_for_inclusion_highlighting inclusion_idx verb =
   let s_idx = string_of_int inclusion_idx in 
   "const char unused_"^s_idx^"_"^verb^"s []=\""^marker_for_inclusion_highlighting^" Inclusion number "^s_idx^
   " "^verb^"s here \";";;
    ;;
 
 let markers_for_inclusion_highlighting inclusion_idx =  
   (
     parametrized_marker_for_inclusion_highlighting inclusion_idx "start",
     parametrized_marker_for_inclusion_highlighting inclusion_idx "end"
   );;

 

 let rec helper_for_computing_exact_locations 
     (treated,counter,marker,to_be_treated) = 
       match to_be_treated with 
       [] -> List.rev treated
       |line::other_lines ->
        if line<>marker
        then  helper_for_computing_exact_locations 
             (treated,counter,marker,other_lines)
        else match other_lines with 
          [] -> List.rev treated
       |line2::other_lines2 ->
         let new_data = (
          if not(String.starts_with line2 ~prefix:"# 1 \"")    
          then None 
          else  
          match Stdl_string_find_char.from_inclusive_opt (fun c->c='\"') line2 6 with 
            None -> raise(Helper_for_computing_exact_locations_exn)
         |Some idx1 -> 
           Some(Stdl_cull_string.interval line2 6 (idx1-1)) 
         ) in 
         let new_marker = parametrized_marker_for_inclusion_highlighting 
         (counter+1) "start" in 
         helper_for_computing_exact_locations 
         ((counter,new_data)::treated,counter+1,new_marker,other_lines2)

let exact_locations_without_line_numbers ~preprocessed_includer_text = 
  let first_marker = parametrized_marker_for_inclusion_highlighting 
         1 "start" in 
 helper_for_computing_exact_locations 
     ([],1,first_marker,Stdl_lines_in_text.lines preprocessed_includer_text);;

let exact_locations old_text ~preprocessed_includer_text = 
  let indexed_lines = Stdl_lines_in_text.indexed_lines old_text in 
  let line_numbers_for_incls = List.filter_map (
   fun (line_idx,line) ->
    if is_an_inclusion_line line 
    then Some line_idx 
    else None
  ) indexed_lines in
  let temp =exact_locations_without_line_numbers ~preprocessed_includer_text in 
  let temp2 = List.combine line_numbers_for_incls temp in 
  Stdl_image.image (
    fun (line_nbr,(incl_idx,fn_opt)) -> {
     Stdl_cee_inclusion_item_t.line_number = line_nbr ;
      inclusion_index = incl_idx ;
      included_file_opt = fn_opt; 
    } 
  ) temp2;; 

  let compute_shadow old_text ~inclusion_index_opt ~name_for_included_file 
  ~preprocessed_includer_text =   
   let ssps = compute_small_spaces_in_text old_text  in 
   let indexed_ssps = Stdl_int_range.index_everything ssps in 
   let subtext = (
      match inclusion_index_opt with 
      None -> preprocessed_includer_text 
      |Some inclusion_idx -> 
        let markers = 
          markers_for_inclusion_highlighting inclusion_idx in 
        Stdl_cull_string.between_markers 
         markers preprocessed_includer_text
   ) in 
   let accepted_ssps = List.filter(
         fun (ssp_idx,ssp) ->
          if ssp.namespace = 0 then true else 
          Stdl_substring.is_a_substring_of 
          (parametrized_marker_for_cd_defined_region name_for_included_file ssp_idx) subtext 
   ) indexed_ssps in 
   Stdl_cee_shadow_t.Sh(List.length indexed_ssps,
   Stdl_cee_prawn_t.P(Stdl_image.image fst accepted_ssps)) ;;
 

  let rec helper_for_inclusion_highlighting (treated,counter,to_be_treated) =
    match to_be_treated with 
    [] -> String.concat "\n" (List.rev treated) 
    |line :: other_lines ->
      if is_an_inclusion_line line 
      then let (before,after) = markers_for_inclusion_highlighting (counter+1) in 
           let highlighted_line=before^"\n"^line^"\n"^after in 
           helper_for_inclusion_highlighting (highlighted_line::treated,counter+1,other_lines)
      else helper_for_inclusion_highlighting (line::treated,counter,other_lines) ;;         


  let highlight_inclusion_lines old_text = 
    helper_for_inclusion_highlighting ([],0,Stdl_lines_in_text.lines old_text) ;;  


let included_local_files_in_text text = 
  let temp1 = indexed_lines_inside_or_outside_cee_comments_or_dq_strings text in 
  let temp2 = List.filter_map (
    fun (line_idx,(line,linebreak_state)) ->
      if linebreak_state<>Stdl_situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings then None else
      Option.map (fun included_fn ->
          (line_idx,included_fn)
      ) (included_local_file_opt line)
  ) temp1 in 
  intstr_sort temp2 ;;

let included_nonlocal_files_in_text text = 
  let temp1 = indexed_lines_inside_or_outside_cee_comments_or_dq_strings text in 
  let temp2 = List.filter_map (
      fun (line_idx,(line,linebreak_state)) ->
        if linebreak_state<>Stdl_situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings then None else
        Option.map (fun included_fn ->
            (line_idx,included_fn)
        ) (included_nonlocal_file_opt line)
  ) temp1 in 
  intstr_sort temp2 ;;


let add_extra_ending_in_inclusion_line ~extra line = 
  let occs = Stdl_substring.occurrences_of_in "\"" line in 
  if List.length(occs)<>2
  then raise(Add_extra_inclusion_line_exn(line))
  else 
  let i1 = List.nth occs 0 
  and i2 = List.nth occs 1 in 
  let included_file = Stdl_cull_string.interval line (i1+1) (i2-1) in 
  (Stdl_cull_string.beginning i1 line)^
  (Stdl_cee_common.add_extra_ending_in_filename ~extra included_file)^
  (Stdl_cull_string.interval line i2 (String.length line)) ;;

(*  
  add_extra_ending_in_inclusion_line 
  ~extra:"fat" "# include \"The_brown.cat\"";;
*)

let add_extra_ending_in_inclusions_inside_text ~extra text =
  let lines_before = lines_inside_or_outside_cee_comments_or_dq_strings text 
  and counter=ref 0 in 
  let lines_after = Stdl_image.image(
    fun (line,linebreak_state) -> 
      if (linebreak_state=Stdl_situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings)
        &&
         ((included_local_file_opt line)<>None)
      then let _ =(counter:=(!counter)+1) in 
      add_extra_ending_in_inclusion_line ~extra line 
      else line    
  ) lines_before in 
  (String.concat "\n" lines_after,!counter);;
 let highlight_inclusions_in_text text = 
  let temp1 = indexed_lines_inside_or_outside_cee_comments_or_dq_strings text in 
  let all_lines = Stdl_image.image (
    fun (line_idx,(line,linebreak_state)) ->
      ((line_idx,line),
      (linebreak_state=Stdl_situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings)
      &&
      ((included_local_file_opt line)<>None))
  ) temp1 in 
  let inclusion_lines = List.filter snd all_lines in 
  let indexed_inclusion_lines = Stdl_int_range.index_everything inclusion_lines in 
  let line_idx_to_incl_idx = Stdl_image.image (
    fun (incl_idx,((line_idx,_),_)) -> (line_idx,incl_idx)
  ) indexed_inclusion_lines in 
  let modified_lines = Stdl_image.image (
    fun ((line_idx,line),line_is_an_inclusion) ->
      if not(line_is_an_inclusion) then line else 
      let incl_idx = List.assoc line_idx line_idx_to_incl_idx in 
      let (marker_before,marker_after) = 
       markers_for_inclusion_highlighting incl_idx in 
      "\n"^marker_before ^"\n"^line^"\n"^marker_after 
  ) all_lines in 
  String.concat "\n" modified_lines ;;

(*


let text1 = String.concat "\n" 
[
   "1";
   "2";
   "#include \"abc\"";
   "4";
   "#include \"def\"";
   "#include \"ghi\"";
   "7";
] ;;

let text2 = highlight_inclusions_in_text text1 ;;

print_string text2 ;;


*)


let marker_for_beginning_fiamengo_inclusions = "xvxBLjSGtxSfWtCVHNDS" ;;
let marker_for_ending_fiamengo_inclusions = "vQBknJWWtwvqDgSkVJms" ;;

let fiamengo_ends_here = " ends here */" ;;
let fiamengo_beginner_prefix ~fiamengo_depth =
  "/* "^marker_for_beginning_fiamengo_inclusions^" "^
    "Depth-"^(string_of_int fiamengo_depth)^" inclusion of ";;

let fiamengo_ender_prefix ~fiamengo_depth =
  "/* "^marker_for_ending_fiamengo_inclusions^" "^
    "Depth-"^(string_of_int fiamengo_depth)^" inclusion of ";;

let fiamengize_individual_inclusion reader ~fiamengo_depth 
   name_of_included_file =
    (fiamengo_beginner_prefix ~fiamengo_depth)^
    name_of_included_file^" starts here */\n"^
    (reader name_of_included_file)^
    "\n"^
    (fiamengo_ender_prefix ~fiamengo_depth)^
    name_of_included_file^fiamengo_ends_here;;

let rec helper_for_fiamengization reader ~fiamengo_depth= function
(treated,remaining_lines) -> 
  match remaining_lines with 
  [] -> treated
  |line :: other_lines ->
   (
     match included_local_file_opt  line  with 
      None -> helper_for_fiamengization reader ~fiamengo_depth 
            (treated^"\n"^line,other_lines)
    |Some name_of_included_file ->
      let fiamengized_line =  
      fiamengize_individual_inclusion reader ~fiamengo_depth 
   name_of_included_file in 
      helper_for_fiamengization reader ~fiamengo_depth 
            (treated^"\n"^fiamengized_line,other_lines)
   ) ;;

let fiamengize_whole_text  reader ~fiamengo_depth text= 
  helper_for_fiamengization reader ~fiamengo_depth
  ("",Stdl_lines_in_text.lines text) ;;

let extract_fiamengo_included_filename end_prefix line =
   Stdl_cull_string.two_sided_cutting (end_prefix,fiamengo_ends_here) line ;;
  

let pusher_for_fiamengo_parsing
  (opt_answer,walker) =
  match opt_answer with 
  (Some _) -> failwith("This should never happen in pusher_for_fiamengo_parsing")
  |None ->
  let (beg_prefix,end_prefix,treated,inclusion_pending,subtext,remaining_lines) = walker in 
    match remaining_lines with 
    [] -> (Some(List.rev ((false,(subtext,""))::treated)),walker) 
    |line :: other_lines ->
    (*
     simple two-state alternative : if an inclusion is currently
     pending we look for the end prefix, otherwise for the beginning prefix
    *)
    if inclusion_pending
   then (if String.starts_with line ~prefix:end_prefix 
          then  let filename = extract_fiamengo_included_filename end_prefix line in 
                 (None,(beg_prefix,end_prefix,(true,(filename,subtext))::treated,
                false,"",other_lines))
          else  (None,(beg_prefix,end_prefix,treated,
                true,subtext^"\n"^line,other_lines))
        )
   else if String.starts_with line ~prefix:beg_prefix 
          then  (*
                extra care must be taken in case the included subtext is 
                empty
               *)
               let treated2 = (false,(subtext,""))::treated in 
               (
                match other_lines with 
                [] -> raise(Unfinished_fiamengo_inclusion line)
                |line2 :: other_lines2 ->
                  if String.starts_with line2 ~prefix:end_prefix 
                  then let filename = extract_fiamengo_included_filename end_prefix line in 
                      (None,(beg_prefix,end_prefix,(true,(filename,""))::treated2,
                       false,"",other_lines2))
                  else (None,(beg_prefix,end_prefix,treated2,
                       true,line2,other_lines2))

               )
          else    
                (None,(beg_prefix,end_prefix,treated,
                false,subtext^"\n"^line,other_lines))  ;;    


let rec iterator_for_fiamengo_parsing  pair =
  match fst pair with 
  (Some answer) -> answer 
  | None -> 
    iterator_for_fiamengo_parsing 
    (pusher_for_fiamengo_parsing pair) ;;
  

let parse_fiamengized_text ~fiamengo_depth text = 
   iterator_for_fiamengo_parsing  
  (None,(fiamengo_beginner_prefix ~fiamengo_depth,
   fiamengo_ender_prefix ~fiamengo_depth,
   [],false,"",Stdl_lines_in_text.lines text)) ;;

exception Helper_for_inclusion_standardization_exn ;;
let rec helper_for_inclusion_standardization 
  (root_dir,counter,treated,current_incl_item,other_incl_items,to_be_treated) =
   match to_be_treated with 
   [] -> raise(Helper_for_inclusion_standardization_exn) 
   |(line_nbr,line) :: other_lines ->
     if line_nbr<>current_incl_item.Stdl_cee_inclusion_item_t.line_number 
     then helper_for_inclusion_standardization 
          (root_dir,counter,line::treated,current_incl_item,other_incl_items,other_lines)
     else 
     let standard_incl_line = (
      match current_incl_item.Stdl_cee_inclusion_item_t.included_file_opt with 
      None -> ""
      |Some fn ->
        let path = (
         if String.starts_with fn ~prefix:root_dir 
         then Stdl_cull_string.two_sided_cutting (root_dir,"") fn 
         else "header_files"^fn 
        )  in   
        "#include \""^path^"\""
     ) in 
     let treated2 = standard_incl_line :: treated 
     and counter2 = (
      if standard_incl_line = line 
      then counter 
      else counter+1  
     )in 
     match other_incl_items with 
     [] -> (counter2,String.concat "\n"
         (List.rev_append treated2 (Stdl_image.image snd other_lines)))
     |next_incl_item :: other_incl_items2 ->
        helper_for_inclusion_standardization 
          (root_dir,counter2,treated2,next_incl_item,other_incl_items2,other_lines)  
    ;; 

let standardize_inclusions_in_text_opt 
  root_dir incl_items old_text =
  match incl_items with 
  [] -> None 
  |incl_item::other_incl_items ->
  let s_root_dir = Stdl_directory_name.connectable_to_subpath root_dir 
  and indexed_lines = Stdl_lines_in_text.indexed_lines old_text in
  let (count,new_text) = helper_for_inclusion_standardization 
  (s_root_dir,0,[],incl_item,other_incl_items,indexed_lines) in 
  if count = 0 
  then None  
  else Some(count,new_text) ;;

end ;;  

let compute_shadow = Private.compute_shadow ;;
let crop_using_prawn = Private.crop_using_prawn ;;
let exact_locations_of_included_files = Private.exact_locations ;;
let fiamengize_text ~fiamengo_depth reader text = Private.fiamengize_whole_text ~fiamengo_depth reader text;;
let highlight_inclusions_inside_text = Private.highlight_inclusion_lines ;;
let included_local_files_in_text = Private.included_local_files_in_text ;;
let included_nonlocal_files_in_text = Private.included_nonlocal_files_in_text ;;
let parse_fiamengized_text ~fiamengo_depth text = Private.parse_fiamengized_text ~fiamengo_depth text;;
let standardize_guard_in_text_opt = Private.standardize_guard_in_text_opt ;;

let standardize_inclusions_in_text_opt = Private.standardize_inclusions_in_text_opt ;;
let tattoo_regions_between_conditional_directives= Private.tattoo_regions_between_conditional_directives;;




end;;






module Stdl_cee_compilation_command_t=struct

(*

#use"lib/Cee_language/cee_compilation_command_t.ml";;

*)

type separate_t = {
  root : Stdl_directory_name_t.t;
  included_header_dirs : Stdl_directory_name_t.t list;
  included_source_dirs : string list;
  deffers : (string * (string option)) list;
  undeffers : string list;
  dep_file : string option;
  libobj_file : string option;
  short_path : string;
  ending : string;
  core_of_command : string;
  } ;;

type batch_t = {
  beginning_of_command : string ;
  litany: string list ;
  end_of_command : string
} ;; 

type t = 
    Separate of separate_t 
   |Batch of batch_t ;;

end;;






module Stdl_cee_compilation_command=struct

(*

#use"lib/Cee_language/cee_compilation_command.ml";;

*)

type separate_t = Stdl_cee_compilation_command_t.separate_t = {
    root : Stdl_directory_name_t.t;
    included_header_dirs : Stdl_directory_name_t.t list;
    included_source_dirs : string list;
    deffers : (string * (string option)) list;
    undeffers : string list;
    dep_file : string option;
    libobj_file : string option;
    short_path : string;
    ending : string;
    core_of_command : string;
    } ;;
  
type batch_t = Stdl_cee_compilation_command_t.batch_t = {
    beginning_of_command : string ;
    litany: string list ;
    end_of_command : string
  } ;; 
  
type t = Stdl_cee_compilation_command_t.t = 
    Separate of separate_t 
   |Batch of batch_t ;;


module Private = struct

let deffer_extractor = Stdl_image.image (fun (_,elt) -> 
  let y = Stdl_cull_string.cobeginning 2 elt in 
  match String.index_opt y '=' with 
  None -> (y,None)
  |Some i ->
     (Stdl_cull_string.beginning i y,Some(Stdl_cull_string.cobeginning (i+1) y))
  ) ;;

let extract_and_classify_data_from_command raw_command = 
  let c2 = Stdl_cull_string.cobeginning 2 in 
  let starts_with = (fun pref l ->
    List.partition (fun (_,elt) -> String.starts_with
    elt ~prefix:pref) l
  ) 
  and is_preceded_by=(fun prec l ->
    let (a,b)=List.partition (fun (opt,_elt) -> opt=Some prec) l in
    if a= [] then (None,b) else (Some(snd(List.hd a)),b)
  ) in
  let temp1 = Str.split (Str.regexp "[ \t\r]+") raw_command in 
  let temp2 = Stdl_list_again.universal_delta_list temp1 in 
  let temp3 = (None,List.hd temp1):: (Stdl_image.image (fun (e1,e2)->(Some e1,e2) ) temp2) in 
  let (header_dirs1,others1) = starts_with "-I/" temp3 in 
  let header_dirs = Stdl_image.image (fun (_,elt) -> 
       Stdl_directory_name.of_string(c2 elt)
    ) header_dirs1 in
  let (source_dirs1,others2) = starts_with "-I" others1 in 
  let source_dirs = Stdl_image.image (fun (_,elt) -> 
     let y = c2 elt in if y="." then "" else y) source_dirs1 in 
  let (deffings1,others3) = starts_with "-D" others2 in
  let deffings = deffer_extractor deffings1 in 
  let (undeffings1,others4) = starts_with "-U" others3 in
  let undeffings = Stdl_image.image (fun (_,elt) -> c2 elt ) undeffings1 in   
  let (dep_file_opt,others5) = is_preceded_by "-MF" others4 in
  let (libobj_file_opt,others6) = is_preceded_by "-MT" others5 in 
  let others7 = List.filter (fun (_,elt)->not(List.mem elt ["-MMD";"-MF";"-MT"])) others6 in  
  let shortened_command = String.concat " " (Stdl_image.image snd others7) in    
  let i1 = Option.get(Stdl_substring.leftmost_index_of_in_from_opt " -c " shortened_command 1) in 
  let i2 = Option.get(Stdl_substring.leftmost_index_of_in_from_opt " " shortened_command (i1+4)) in
  let short_filename = Stdl_cull_string.interval shortened_command (i1+4) (i2-1) in 
  let gutted_command = Stdl_cull_string.beginning (i1-1) shortened_command in 
  (header_dirs,source_dirs,deffings,undeffings,
     dep_file_opt,libobj_file_opt,short_filename,gutted_command) ;;

let parse_separate root_dir raw_command = 
  let (header_dirs,source_dirs,deffings,undeffings,
     dep_file_opt,libobj_file_opt,short_filename,gutted_command) = 
     extract_and_classify_data_from_command raw_command in 
  {
    root = root_dir;
    included_header_dirs = header_dirs;
    included_source_dirs = source_dirs;
    deffers = deffings;
    undeffers = undeffings;
    dep_file = dep_file_opt;
    libobj_file = libobj_file_opt ;
    short_path = Stdl_cull_string.coending 2 short_filename ;
    ending = Stdl_cull_string.ending 2 short_filename ;
    core_of_command = gutted_command ;
  }  ;;
 
let parse_batch raw_command = 
  let elts = Str.split (Str.regexp "[ \t\r]+") raw_command in 
  let i1 = (Option.get(List.find_index(fun elt->String.ends_with elt ~suffix:".o") elts))+1 in 
  let (rev_before,rest) = Stdl_list_again.long_head_with_tail (i1-1) elts in 
  let before = List.rev rev_before in 
  let rev_rest = List.rev rest in 
  let i2 = (Option.get(List.find_index(fun elt->String.ends_with elt ~suffix:".o") rev_rest))+1 in 
  let (after,rev_middle) = Stdl_list_again.long_head_with_tail (i2-1) rev_rest in 
  let middle = List.rev rev_middle  in  
  {
      beginning_of_command = String.concat " " before ;
      litany =Stdl_image.image (Stdl_cull_string.coending 2) middle ;
      end_of_command = String.concat " " after
  }  ;;

(*  
parse_batch "The brown and astute fox.o jumped.o over.o the lazy dog" ;;
*)
let parse root_dir raw_command = 
  if Stdl_substring.is_a_substring_of " -c " raw_command 
  then Separate(parse_separate root_dir raw_command)
  else Batch(parse_batch raw_command) ;;


let short_name_from_separate separate_cmd = 
  separate_cmd.Stdl_cee_compilation_command_t.short_path ^ 
  separate_cmd.Stdl_cee_compilation_command_t.ending ;; 

let short_name_for_preprocessable separate_cmd = 
  Stdl_cee_common.add_extra_ending_in_filename
    ~extra:"preprocessable" (short_name_from_separate separate_cmd) ;; 
let short_name_for_preprocessed separate_cmd = 
  Stdl_cee_common.add_extra_ending_in_filename
    ~extra:"preprocessed" (short_name_from_separate separate_cmd) ;; 

let needed_header_dirs separate_cmd =
  let temp1 = separate_cmd.Stdl_cee_compilation_command_t.included_header_dirs in 
  let temp2 = Stdl_image.image (
        fun dir -> 
          "-I"^(Stdl_directory_name.connectable_to_subpath dir)
  ) temp1 in 
  String.concat " " temp2 ;;     

let short_version_for_needed_source_dirs separate_cmd =
   let temp1 = separate_cmd.Stdl_cee_compilation_command_t.included_source_dirs in 
   let temp2 = Stdl_image.image (
     fun dir -> 
       let adjusted_dir = (if dir="" then "." else dir) in 
       "-I"^adjusted_dir
   ) temp1 in 
   String.concat " " temp2 ;; 
       
let long_version_for_needed_source_dirs separate_cmd =
  let root_dir = Stdl_directory_name.connectable_to_subpath separate_cmd.root in 
  let temp1 = separate_cmd.Stdl_cee_compilation_command_t.included_source_dirs in 
  let temp2 = Stdl_image.image (
      fun dir -> 
        "-I"^root_dir^dir
  ) temp1 in 
  String.concat " " temp2 ;;    

let needed_dirs ~write_out_root_in_source_inclusions separate_cmd =
  let header_dirs = needed_header_dirs separate_cmd 
  and source_dirs = (
     if write_out_root_in_source_inclusions 
     then long_version_for_needed_source_dirs separate_cmd 
     else short_version_for_needed_source_dirs separate_cmd 
  ) in 
  header_dirs ^ " " ^ source_dirs ;;

let deffers separate_cmd =
  let temp1 = separate_cmd.Stdl_cee_compilation_command_t.deffers in 
  let temp2 = Stdl_image.image (
      fun (vaar,vaal_opt) ->
        let vaal_part =(match vaal_opt with 
          None -> ""
          |Some vaal -> "="^vaal
        ) in 
        "-D"^vaar ^ vaal_part
  ) temp1 in 
  String.concat " " temp2 ;;  

let undeffers separate_cmd =
  let temp1 = separate_cmd.Stdl_cee_compilation_command_t.undeffers in 
  let temp2 = Stdl_image.image (fun vaar ->"-U"^vaar) temp1 in 
  String.concat " " temp2 ;;

let preprocess_only_version separate_cmd = 
  let root_dir = Stdl_directory_name.connectable_to_subpath separate_cmd.root in 
  let core_of_command = separate_cmd.Stdl_cee_compilation_command_t.core_of_command in 
  let short_separate = short_name_from_separate separate_cmd in 
  let short_name_for_preprocessable_file =  
        Stdl_cee_common.add_extra_ending_in_filename
        ~extra:"preprocessable" short_separate 
  and short_name_for_preprocessed_file =  
        Stdl_cee_common.add_extra_ending_in_filename
        ~extra:"preprocessed" short_separate in   
  let name_for_preprocessable_file = root_dir ^ short_name_for_preprocessable_file 
  and name_for_preprocessed_file = root_dir ^ short_name_for_preprocessed_file in 
  let included_dirs = 
    needed_dirs ~write_out_root_in_source_inclusions:true separate_cmd in
  core_of_command^" "^included_dirs^" "^(deffers separate_cmd)^" "^(undeffers separate_cmd)^
  " -E "^name_for_preprocessable_file^" -o "^name_for_preprocessed_file  ;;
  
(*

let cmd1 = parse_separate 
(Directory_name.of_string(home^"/Teuliou/Experimenting_with_php/php-src"))
"/usr/bin/clang -IZend/ -Imain/ -I. -Iext/date/lib/ -I/usr/include/libxml2/ -ITSRM/ -D_GNU_SOURCE -fno-common -Wstrict-prototypes -Wformat-truncation -Wall -Wextra -Wno-unused-parameter -Wno-sign-compare -g -ffp-contract=off -fvisibility=hidden -O0 -DZEND_SIGNALS -DZEND_ENABLE_STATIC_TSRMLS_CACHE=1 -DSHADOW_STACK_SYSCALL=0 -MMD -MF Zend/zend_exceptions.dep -MT Zend/zend_exceptions.lo -c Zend/zend_exceptions.c -o Zend/zend_exceptions.o" 
;;

let pp1 = preprocess_only_version cmd1 ;;

*)

let write_separate cmd = 
  let core_of_command = cmd.Stdl_cee_compilation_command_t.core_of_command in 
  let included_dirs = 
    needed_dirs ~write_out_root_in_source_inclusions:false cmd in
  core_of_command^" "^included_dirs^" "^(deffers cmd)^" "^(undeffers cmd)^
  " -c " ^ cmd.short_path ^ cmd.ending ^ 
  " -o " ^ cmd.short_path ^ ".o" ;;

(*

let cmd1 = parse_separate 
(Directory_name.of_string(home^"/Teuliou/Experimenting_with_php/php-src"))
"/usr/bin/clang -IZend/ -Imain/ -I. -Iext/date/lib/ -I/usr/include/libxml2/ -ITSRM/ -D_GNU_SOURCE -fno-common -Wstrict-prototypes -Wformat-truncation -Wall -Wextra -Wno-unused-parameter -Wno-sign-compare -g -ffp-contract=off -fvisibility=hidden -O0 -DZEND_SIGNALS -DZEND_ENABLE_STATIC_TSRMLS_CACHE=1 -DSHADOW_STACK_SYSCALL=0 -MMD -MF Zend/zend_exceptions.dep -MT Zend/zend_exceptions.lo -c Zend/zend_exceptions.c -o Zend/zend_exceptions.o" 
;;

let ws1 = write_separate cmd1 ;; 

*)  


let write_batch cmd = 
  let middle = String.concat " " (Stdl_image.image (fun elt->elt^".o") cmd.litany) in 
  cmd.beginning_of_command ^ middle ^ cmd.end_of_command ;;

let write = function 
  (Separate s) -> write_separate s 
  |(Batch b) -> write_batch b ;;

end ;;

let parse = Private.parse ;;
let parse_separate = Private.parse_separate ;;

let preprocess_only_version = Private.preprocess_only_version ;; 
let short_name_for_preprocessable = Private.short_name_for_preprocessable ;;

let short_name_for_preprocessed = Private.short_name_for_preprocessed ;;

let short_name_from_separate = Private.short_name_from_separate ;;
let write = Private.write ;;

end;;






module Stdl_cee_snapshot_parameters_t=struct

(*

#use"lib/Cee_language/cee_snapshot_parameters_t.ml";;

*)

type  t = {
  root : Stdl_directory_name_t.t ;
  suffix_for_snapshots : string ;
  index : int ;
} ;; 

end;;






module Stdl_makefile_t=struct

(*

#use"lib/makefile_t.ml";;

*)

type variable_assignment = {
   va_line_number : int ;
   variable_name : string ;
   content : string list
} ;;

type rule = {
   ru_line_number : int ;
   targets : string list ;
   prerequisites : string list ;
   commands : string list
} ;;

type inclusion = {
   inc_line_number : int ;
   included_ones : string list ;
} ;;

type text = MT of string ;; 



end;;






module Stdl_makefile=struct

(*

#use"lib/makefile.ml";;

*)

exception Cyclic_dependency of string list ;;  

exception List_value_exn of string * ( Stdl_makefile_t.variable_assignment list) ;; 

exception Missing_variable_close_tag of string * int ;;

exception Parse_next_instruction_exn of int ;;

exception Prerequisites_and_commands_for_target_exn of string * ( Stdl_makefile_t.rule list) ;; 

exception Unknown_variable_exn of string ;;

module Private = struct 

exception Check_all_are_empty_but_last_exn  ;;

type text_or_dollar_var 
   = Txt of string 
    |DVar of string * ((string * string) option) ;; 

type t = {
   assignments : Stdl_makefile_t.variable_assignment list;
   rules : Stdl_makefile_t.rule list;
   inclusions : Stdl_makefile_t.inclusion list;
   unexpanded_targets: (string list) option ;
   unexpanded_prerequisites: (string list) option ;
   unexpanded_variables: ((string * (text_or_dollar_var list)) list) option ;
   expanded_variables: ((string * string) list) option;
   expanded_targets: ((string * string) list) option ;
   expanded_prerequisites: ((string * string) list) option ;
   hidden_equivalences : ((string * string list) list) option;
};; 


let check_all_are_empty_but_last l =
   if l = [] then [] else
   let (h,t) = Stdl_list_again.head_with_tail(List.rev l) in
   if List.for_all (fun commands->commands=[]) t 
   then h
   else raise Check_all_are_empty_but_last_exn ;;  
let prerequisites_and_commands_for_target mkf target_name = 
   let rules1 = List.filter (
      fun rule ->
         List.mem target_name rule.Stdl_makefile_t.targets
   ) mkf.rules in 
   try (
   let prerequisites1 = Stdl_image.image (fun rule -> rule.Stdl_makefile_t.prerequisites) rules1 
   and commands1 = Stdl_image.image (fun rule -> rule.Stdl_makefile_t.commands) rules1 in 
   let prerequisites = List.flatten prerequisites1
   and commands = check_all_are_empty_but_last commands1  in 
   (prerequisites,commands) 
   ) with 
   Check_all_are_empty_but_last_exn ->
   raise(Prerequisites_and_commands_for_target_exn(target_name,rules1))  ;;
  
let prerequisites_for_target mkf target_name = 
    fst(prerequisites_and_commands_for_target mkf target_name) ;; 


let list_value mkf ~variable_name = 
   let temp1 = List.filter (
         fun assg-> assg.Stdl_makefile_t.variable_name = variable_name
   ) mkf.assignments in 
   if List.length(temp1)>1
   then  raise(List_value_exn(variable_name,temp1))
   else  
   if temp1=[]
   then [""]      
   else
   let assg = List.hd temp1 in 
   assg.Stdl_makefile_t.content ;;
let single_value mkf ~variable_name = 
   String.concat " " (list_value mkf ~variable_name);;



let makefile_lines_for_indexed_command (cmd_idx,(cmd_content,comment)) = 
   let s_idx = string_of_int cmd_idx in 
   [
   "\t@echo \"************************************************ Step "^s_idx^":"^comment^"\"";
   "\t"^cmd_content   
   ] ;; 
 
 let write_rule_without_prerequisites ~target_name ~commands = 
  let indexed_commands = Stdl_int_range.index_everything commands in 
  "\n"^target_name^":\n" ^
  (String.concat "\n"
  (List.flatten (Stdl_image.image makefile_lines_for_indexed_command indexed_commands ))) ^ "\n";;   
 
type instruction = 
     Rule of Stdl_makefile_t.rule 
    |Assignment of Stdl_makefile_t.variable_assignment 
    |Inclusion of Stdl_makefile_t.inclusion ;;



let parse_next_rule first_eline next_elines sep_idx = 
   let (_c_idx,l_idx,line1) = first_eline in 
   let tgts = Str.split (Str.regexp "[ \t\r]+") (Stdl_cull_string.beginning (sep_idx-1) line1) 
   and prereqs = Str.split (Str.regexp "[ \t\r]+") (Stdl_cull_string.cobeginning sep_idx line1 ) in 
   let (tabbed_elines,further_elines) = 
    Stdl_hurried.partition_in_two_parts (fun (_,_,line)->String.starts_with ~prefix:"\t" line)  next_elines in 
   let cmds= Stdl_image.image (fun (_,_,line) -> Stdl_cull_string.cobeginning 1 line) tabbed_elines in 
   ({
      Stdl_makefile_t.ru_line_number = l_idx;
      targets = tgts ;
      prerequisites = prereqs ;
      commands =cmds ;
   }, further_elines) ;; 


let parse_next_assignment first_eline next_elines sep_idx = 
   let (_c_idx,l_idx,line1) = first_eline in 
   let vname = Stdl_cull_string.trim_spaces (Stdl_cull_string.interval line1 1 (sep_idx-1)) 
   and vcontent = Str.split (Str.regexp "[ \t\r]+") (Stdl_cull_string.cobeginning sep_idx line1 )  in 

   ({
         Stdl_makefile_t.va_line_number = l_idx;
         variable_name = vname;
         content = vcontent;
       
   }, next_elines) ;;    

let prefix_for_inclusion = "-include ";;

let parse_next_inclusion first_eline next_elines = 
   let (_c_idx,l_idx,line1) = first_eline in 
   let line2 = Stdl_cull_string.two_sided_cutting (prefix_for_inclusion,"") line1 in 
({
   Stdl_makefile_t.inc_line_number = l_idx;
   included_ones = Str.split (Str.regexp "[ \t\r]+") line2;
 
}, next_elines) ;;    


let parse_next_instruction first_eline next_elines = 
   let (_c_idx,l_idx,line1) = first_eline in 
   if String.starts_with ~prefix:prefix_for_inclusion line1
   then   let (incl,remaining_lines) = parse_next_inclusion first_eline next_elines  in 
         (Inclusion incl,remaining_lines)      
   else     
   match Stdl_string_find_char.from_inclusive_opt  (fun c->
          List.mem c [':';'=']) line1 1 with 
    None -> raise (Parse_next_instruction_exn(l_idx))
   |Some sep_idx ->
      if (Stdl_strung.get line1 sep_idx) = ':'
      then let (rule,remaining_lines) = parse_next_rule first_eline next_elines sep_idx in 
           (Rule rule,remaining_lines)
      else let (assg,remaining_lines) = parse_next_assignment first_eline next_elines sep_idx in 
           (Assignment assg,remaining_lines) ;;


let empty_one = { 
      assignments = []; 
      rules = [];
      inclusions = [];
      unexpanded_targets = None ;
      unexpanded_prerequisites = None  ;
      unexpanded_variables = None  ;
      expanded_variables = None  ;
      expanded_targets = None ;
      expanded_prerequisites = None  ;
      hidden_equivalences = None;
   } ;;     

let rev_all mkf = { 
   mkf with 
   assignments = List.rev(mkf.assignments); 
   rules = List.rev(mkf.rules);
   inclusions = List.rev(mkf.inclusions);
   
} ;;

let add_assignment assg mkf = { 
  mkf with    
   assignments = assg :: (mkf.assignments); 
} ;;

let add_rule rule mkf = { 
   mkf with 
   rules = rule :: (mkf.rules)
} ;;

let add_inclusion inclusion mkf = { 
   mkf with 
   inclusions = inclusion :: (mkf.inclusions)
} ;;

let add_instruction instr mkf = match instr with 
  Rule rule -> add_rule rule mkf
 |Assignment assg -> add_assignment assg mkf   
 |Inclusion incl -> add_inclusion incl mkf ;;


let rec helper_for_makefile_parsing (treated,remaining_elines) = 
   match remaining_elines with 
   [] -> rev_all treated 
  | first_eline :: other_elines ->
   let (instr,further_elines) =  parse_next_instruction first_eline other_elines in 
   helper_for_makefile_parsing (add_instruction instr treated,further_elines) ;;


let rec helper_for_gluing (treated,current_head,current_tail) =
    match current_tail with 
    [] -> List.rev (current_head :: treated)
    | next_head :: further_tail ->
       let (c_idx1,l_idx1,line1) = current_head 
       and (_c_idx2,_l_idx2,line2) = next_head in 
       if String.ends_with ~suffix:"\\" line1 
       then  let glued_line = (Stdl_cull_string.coending 1 line1)^" "^line2 in  
             helper_for_gluing (treated,(c_idx1,l_idx1,glued_line),further_tail)
       else  helper_for_gluing (current_head::treated,next_head,further_tail) ;; 

let glue_lines = function 
  [] -> [] 
  | head :: tail -> helper_for_gluing ([],head,tail) ;;   

let parse_makefile mkf_text = 
   let (Stdl_makefile_t.MT text) = mkf_text in 
   let lines1 = Stdl_lines_in_text.enhanced_indexed_lines text in 
   let lines2 = List.filter (fun (_c_idx,_l_idx,line)->
        not(
          (String.starts_with ~prefix:"#" line ) ||
          (Stdl_cull_string.trim_spaces(line)="" )
        )
      ) lines1 in 
   let lines3 = glue_lines lines2 in    
   helper_for_makefile_parsing (empty_one,lines3) ;;

let sl_order = Stdl_total_ordering.lex_for_strings ;;
let sl_insert = Stdl_ordered.insert sl_order ;; 
let sl_sort = Stdl_ordered.sort sl_order ;; 

  


let parse_dollar_var varcontent = 
   let opt1 = String.index_from_opt varcontent 0 ':' in 
   if opt1 = None 
   then DVar(varcontent,None) 
   else 
   let i1 = (Option.get opt1) + 1 in 
   let opt2 = String.index_from_opt varcontent i1 '=' in  
   if opt2 = None 
   then DVar(varcontent,None) 
   else 
   let i2 = (Option.get opt2) + 1 in 
   let itv = Stdl_cull_string.interval varcontent in 
   let ab = itv (i1+1) (i2-1) 
   and ba = itv (i2+1) (String.length varcontent) in 
   DVar(itv 1 (i1-1),Some(ab,ba)) ;;   
    
(*

parse_dollar_var "The:brown=cat" ;; 
parse_dollar_var "Again" ;; 

*)

let add_if_nonempty line i j treated = 
   if i<=j 
   then (Txt(Stdl_cull_string.interval line i j)) :: treated 
   else treated ;;   

let test_for_dollar_beginning line idx = 
   (
      List.exists (fun beg ->Stdl_substring.is_a_substring_located_at beg line idx) [
         "${";"$("
      ]
   ) &&
   (
     List.for_all (fun beg2 ->not(Stdl_substring.is_a_substring_located_at beg2 line (idx+2))) [
          "shell "
     ] 
   ) ;;

let rec next_dollar_beginning_opt (line,line_length) idx = 
   if idx > line_length 
   then None 
   else 
   if test_for_dollar_beginning line idx 
   then Some idx 
   else next_dollar_beginning_opt (line,line_length) (idx+1) ;;
   

let rec helper_for_todv_decomposition (line,line_length) (treated,idx) = 
    if idx>line_length 
    then List.rev treated 
    else 
    let itv = Stdl_cull_string.interval line in   
    match next_dollar_beginning_opt (line,line_length) idx with 
      None ->  List.rev ( (Txt(itv idx line_length)) :: treated )
    |Some(idx2) -> 
      let treated2 = add_if_nonempty line idx (idx2-1) treated in 
      let opening_par = Stdl_strung.get line (idx2+1) in 
      let closing_par = List.assoc opening_par ['(',")";'{',"}"] in 
      let opt3 = Stdl_substring.leftmost_index_of_in_from_opt closing_par line idx2 in 
      if opt3 = None 
      then raise(Missing_variable_close_tag(line,idx))
      else 
      let idx3 = Option.get opt3 in 
      let vrange = itv (idx2+2) (idx3-1) in 
      helper_for_todv_decomposition (line,line_length) ((parse_dollar_var vrange) ::treated2,idx3+1) ;; 

        
let todv_decompose line = helper_for_todv_decomposition (line,String.length line) ([],1) ;; 

(*
   
todv_decompose "When $(the) ${saints}\t\t${go} marching $(in) ..." ;; 

todv_decompose "When $(shell the) ${saints}\t\t${go} marching $(in) ..." ;; 

*)


let compute_unexpanded_targets mkf = 
   let tgts1 = List.flatten (Stdl_image.image (fun ru -> ru.Stdl_makefile_t.targets ) mkf.rules) in 
   Stdl_ordered.to_list(Stdl_ordered.sort Stdl_total_ordering.lex_for_strings tgts1) ;;

let unexpanded_targets mkf_ref = 
   let old_mkf = (!mkf_ref) in 
   match old_mkf.unexpanded_targets with 
 (Some already_computed) -> already_computed 
 | None -> 
    let tgts = compute_unexpanded_targets old_mkf in 
    let _ = (mkf_ref:= {old_mkf with unexpanded_targets = Some tgts} ) in 
    tgts ;; 


let compute_unexpanded_prerequisites mkf = 
   let tgts1 = List.flatten (Stdl_image.image (fun ru -> ru.Stdl_makefile_t.prerequisites ) mkf.rules) in 
   Stdl_ordered.to_list(Stdl_ordered.sort Stdl_total_ordering.lex_for_strings tgts1) ;;
    
let unexpanded_prerequisites mkf_ref = 
   let old_mkf = (!mkf_ref) in 
      match old_mkf.unexpanded_prerequisites with 
   (Some already_computed) -> already_computed 
   | None -> 
       let prereqs = compute_unexpanded_prerequisites old_mkf in 
       let _ = (mkf_ref:= {old_mkf with unexpanded_prerequisites = Some prereqs} ) in 
       prereqs ;; 
    
let compute_unexpanded_variables mkf = 
         let vars1 = Stdl_image.image (fun va -> va.Stdl_makefile_t.variable_name ) mkf.assignments in 
         let vars = Stdl_ordered.sort Stdl_total_ordering.lex_for_strings vars1 in 
         Stdl_ordered.image (fun v->(v,todv_decompose(single_value mkf ~variable_name:v))) vars ;;

let unexpanded_variables mkf_ref = 
   let old_mkf = (!mkf_ref) in 
            match old_mkf.unexpanded_variables with 
    (Some already_computed) -> already_computed 
   | None -> 
     let vars = compute_unexpanded_variables old_mkf in 
     let _ = (mkf_ref:= {old_mkf with unexpanded_variables = Some vars} ) in 
     vars ;; 

let constant_for_todv_list_opt l = 
   if List.length(l)<>1
   then None
   else 
   match List.hd(l) with 
   Txt txt ->(Some txt)
   |DVar(_,_) -> None ;;     

type full_expander = FE of ( string * string ) list ;;
type partial_expander = PE of (string * (text_or_dollar_var list)) list ;;
type mixed_expander = Mx of full_expander * partial_expander ;; 



let register_new_full_expansion (Mx(FE l1,PE l2)) (vname,vcontent) = 
    let new_l1 = (vname,vcontent) :: l1 
    and new_l2 = List.filter (fun (vname2,_) -> vname2 <> vname) l2 in 
    Mx(FE new_l1,PE new_l2) ;;

let eval (Mx(FE l1,PE l2)) vname=
  match List.assoc_opt vname l1 with 
  (Some text1) -> [Txt text1]
  | None ->
   (
      match List.assoc_opt vname l2 with 
      (Some expr2) -> expr2
      |  None ->
         (
            match Sys.getenv_opt vname with 
             Some env_value -> [Txt env_value]
             |None -> raise (Unknown_variable_exn(vname)) 
         )
   ) ;;

    
let decision_for_todv mixed = function 
  (Txt txt) -> (Some txt,None)
  |DVar(vname,replacement_opt) ->
     match constant_for_todv_list_opt (eval mixed vname) with 
      (Some cst) ->  
         let final_cst = (match replacement_opt with 
            None -> cst 
            |Some(ab,ba) -> Stdl_replace_inside.replace_inside_text (ab,ba) cst
         ) in 
         (Some final_cst,None)
      |None -> (None,Some vname) ;; 

let decision_for_todv_list mixed l = 
   let temp1 = Stdl_image.image (fun todv ->(todv,decision_for_todv mixed todv)) l in       
   match List.find_opt (fun (_,(_,bad_opt)) -> bad_opt<>None) temp1 with 
   None ->
         let texts = Stdl_image.image (fun (_,(good_opt,_)) ->Option.get good_opt) temp1 in 
         (Some(String.concat "" texts), None)
   |(Some (_,(_,bad_opt))) ->(None, bad_opt)   ;;



let rec helper_for_full_expansion (mixed,chain_head,chain_tail) =
   let (vname1,todv_list1) = chain_head in 
   let (good_opt,bad_opt) = decision_for_todv_list mixed todv_list1 in 
   if good_opt<>None 
   then let new_mixed = register_new_full_expansion mixed (vname1, Option.get good_opt) in 
        (
         match chain_tail with 
          next_pair :: other_pairs -> helper_for_full_expansion (new_mixed,next_pair,other_pairs)
          |[] -> 
             let (Mx(FE l1,PE l2)) = new_mixed in
             (
               match l2 with 
               [] -> FE l1
               |next_pair2 :: _-> helper_for_full_expansion (new_mixed,next_pair2,[])
             ) 
        )   
    else
    let vname2 = Option.get bad_opt in 
    let full_chain = chain_head :: chain_tail in 
    let names = Stdl_image.image fst full_chain in 
    match Stdl_list_again.index_of_in_opt vname2 names with  
    (Some i) -> raise(Cyclic_dependency(List.rev(vname2::(Stdl_list_again.long_head (i-1) names))))
    |None ->
      helper_for_full_expansion (mixed,(vname2,eval mixed vname2),full_chain) ;;  


let mixed_to_full mixed = 
   let (Mx(FE l1,PE l2)) = mixed in 
   match l2 with 
   [] -> FE l1 
   | pair :: _ -> helper_for_full_expansion (mixed,pair,[]) ;; 

let compute_expanded_variables mkf_ref = 
   let mixed = Mx(FE[],PE(unexpanded_variables mkf_ref)) in 
   let (FE expansions) = mixed_to_full mixed in 
   expansions ;;  


let expanded_variables mkf_ref = 
   let old_mkf = (!mkf_ref) in 
               match old_mkf.expanded_variables with 
       (Some already_computed) -> already_computed 
   | None -> 
        let vars = compute_expanded_variables mkf_ref in 
        let _ = (mkf_ref:= {old_mkf with expanded_variables = Some vars} ) in 
        vars ;; 

(*


let text1 = String.concat "\n"
[
  "CASTLE=$(WAITRESS) $(OXFORD) $(NANNY)"; 
  "MU=UM"; 
  "NANNY=$(NI) $(NI)";
  "NI=IN"; 
  "OXFORD =A C$(MU)"; 
  "WAITRESS =S$(MU)MER IS"
] ;; 


let text1 = String.concat "\n"
[
  "V1=$(V2)a$(V3)"; 
  "V2=$(V3)b$(V4)"; 
  "V3=$(V4)c$(V5)"; 
  "V4=d";
  "V5=e$(V1)"; 
] ;; 


let mkf1 = parse (MT text1) ;; 
let mkf1_ref = ref mkf1 ;; 

let check1 = expanded_variables mkf1_ref ;; 

*)

let expand_variables_in_expression mkf_ref expr = 
    let todv_list = todv_decompose expr in 
    let mixed = Mx(FE(expanded_variables mkf_ref),PE[]) in 
    let (good_opt,_) = decision_for_todv_list mixed todv_list in 
    Option.get good_opt;;

let compute_expanded_targets mkf_ref = 
   Stdl_image.image (fun tgt -> (tgt,expand_variables_in_expression mkf_ref tgt) ) (unexpanded_targets mkf_ref)  ;;
    
let expanded_targets mkf_ref = 
      let old_mkf = (!mkf_ref) in 
      match old_mkf.expanded_targets with 
    (Some already_computed) -> already_computed 
    | None -> 
       let tgts = compute_expanded_targets mkf_ref in 
       let _ = (mkf_ref:= {old_mkf with expanded_targets = Some tgts} ) in 
       tgts ;; 


let compute_expanded_prerequisites mkf_ref = 
   Stdl_image.image (fun prereq -> (prereq,expand_variables_in_expression mkf_ref prereq) ) (unexpanded_prerequisites mkf_ref) ;;
          
let expanded_prerequisites mkf_ref = 
   let old_mkf = (!mkf_ref) in 
      match old_mkf.expanded_prerequisites with 
   (Some already_computed) -> already_computed 
   | None -> 
      let prereqs = compute_expanded_prerequisites mkf_ref in 
      let _ = (mkf_ref:= {old_mkf with expanded_prerequisites = Some prereqs} ) in 
      prereqs ;; 

let expand_variables_in_target mkf_ref tgt = 
   List.assoc tgt (expanded_targets mkf_ref) ;;
   
let expand_variables_in_prerequisites mkf_ref prereq = 
   List.assoc prereq (expanded_prerequisites mkf_ref) ;;   

let involved_variables_in_todv_list l =
   let unordered = List.filter_map (
      function 
      Txt(_) -> None 
      |DVar(vname,_) -> Some vname
   )  l in 
   Stdl_ordered.sort Stdl_total_ordering.lex_for_strings unordered ;;

let all_involved_variables mkf_ref = 
   let temp1 = 
      (unexpanded_targets mkf_ref)
     @(unexpanded_prerequisites mkf_ref) in 
   let temp2 = (Stdl_image.image todv_decompose temp1)
     @(Stdl_image.image snd (unexpanded_variables mkf_ref)) in 
   let temp3 = Stdl_image.image involved_variables_in_todv_list temp2 in 
   Stdl_ordered.to_list(Stdl_ordered.fold_merge Stdl_total_ordering.lex_for_strings temp3) ;;

let unknown_variables mkf_ref =
   let uv = unexpanded_variables mkf_ref in 
   List.filter (
      fun v ->
         ((List.assoc_opt v uv) = None)
         && 
         (Sys.getenv_opt v=None)
    )(all_involved_variables mkf_ref) ;; 


let compute_hidden_equivalences_for_prerequisite mkf_ref (prereq,expanded_prereq)=
    let temp1 = List.filter_map (
       fun (tgt,expanded_tgt) -> 
           if (tgt<>prereq) && (expanded_tgt = expanded_prereq )
           then Some tgt 
           else None  
     )(expanded_targets mkf_ref) in 
    if temp1=[]
    then None 
    else Some(prereq,temp1) ;;     
       
let compute_hidden_equivalences mkf_ref = 
    List.filter_map (
       compute_hidden_equivalences_for_prerequisite mkf_ref
    ) (expanded_prerequisites mkf_ref) ;; 
 

let hidden_equivalences mkf_ref = 
      let old_mkf = (!mkf_ref) in 
                  match old_mkf.hidden_equivalences with 
          (Some already_computed) -> already_computed 
   | None -> 
      let vars = compute_hidden_equivalences mkf_ref in 
      let _ = (mkf_ref:= {old_mkf with hidden_equivalences = Some vars} ) in 
      vars ;; 
    
let enhanced_prerequisites_and_commands_for_target mkf_ref target =
   let eqvs = hidden_equivalences mkf_ref in
   let (old_prereqs,cmds) = prerequisites_and_commands_for_target (!mkf_ref) target in 
   let additional_prereqs = List.flatten(Stdl_image.image (
      fun prereq -> match List.assoc_opt prereq eqvs with 
      (Some others) -> others 
      | None -> []
   ) old_prereqs) in 
   (old_prereqs@additional_prereqs,cmds) ;;

let enhanced_prerequisites_for_target mkf_ref target = 
  fst(enhanced_prerequisites_and_commands_for_target mkf_ref target);;

let rec helper_for_target_list_enhancement mkf (treated,terminals,to_be_treated) = 
      match to_be_treated with 
      [] -> List.rev treated 
   |(target,already_visited) :: others ->
           if already_visited 
           then  helper_for_target_list_enhancement mkf (target::treated,terminals,others)
           else 
           let (prerequisites,commands)  = enhanced_prerequisites_and_commands_for_target mkf target in 
           if (prerequisites,commands)  = ([],[])
           then  helper_for_target_list_enhancement mkf (treated,target::terminals,others) 
           else 
           let old_prerequisites = treated @ terminals in 
           let new_prerequisites = List.filter (fun tgt ->not(List.mem tgt old_prerequisites)) prerequisites in 
           if new_prerequisites = []
           then helper_for_target_list_enhancement mkf (target::treated,terminals,others)
           else
           let new_goal = (Stdl_image.image (fun x->(x,false)) new_prerequisites)
                           @( (target,true) :: others) in 
           helper_for_target_list_enhancement mkf (treated,terminals,new_goal) ;;                    
          
let enhance_target_list mkf l = 
   helper_for_target_list_enhancement mkf ([],[],Stdl_image.image (fun x->(x,false)) l) ;;


end ;;

let enhance_target_list = Private.enhance_target_list ;;

let expand = Private.expand_variables_in_expression ;; 

let list_value = Private.list_value;;

let parse = Private.parse_makefile ;; 

let prerequisites_and_commands_for_target = Private.enhanced_prerequisites_and_commands_for_target ;; 

let prerequisites_for_target = Private.enhanced_prerequisites_for_target ;; 

let single_value = Private.single_value ;;

let write_rule_without_prerequisites = Private.write_rule_without_prerequisites ;; 

(*

let s_mt1 = 
   (String.concat "\n"
   ["arthur: a.txt"; "\t@echo \"This is Arthur 1\""; "belinda: b.txt";
   "\t@echo \"This is Belinda\""; "arthur: c.txt";
   "\t@echo \"This is Arthur 2\""; "arthur: d.txt";
   "\t@echo \"This is Arthur 3\"\t"; "a.txt:"; "\tcp origin.txt a.txt"; "b.txt:";
   "\tcp origin.txt b.txt\t"; "c.txt:"; "\tcp origin.txt c.txt\t"; "d.txt:";
   "\tcp origin.txt d.txt\t\t\t"; "clean:"; "\trm -f a.txt b.txt c.txt d.txt";
   "\ttouch what_gnu_make_did.txt \t"; ""] 
   ) ;; 

let mt1 = Makefile_t.MT s_mt1;; 

let ap2= Absolute_path.of_string "~/Teuliou/Experimenting_with_php/copiableMakefile";;

let mt2  =  Makefile_t.MT(Io.read_whole_file ap2) ;; 

let see2 = parse_makefile mt2 ;;

let assgs2 = see2.assignments ;; 

let assg = List.nth assgs2 50;;

let rules2 = see2.rules ;; 

let rule = List.nth rules2 340;;

*)



end;;






module Stdl_explicit=struct

(*

#use"lib/explicit.ml";;

*)

module Private = struct 

let iter0 (f:'a->unit) l (pre_left,right)=
  let left=(if pre_left="" then "" else pre_left^" : ") in 
  let n=List.length(l)
  and accu=ref(l) in
  let s0=" of "^string_of_int(n)^" "^right^"\n" in
  for j=1 to n
               do
               ( f(List.hd(!accu));
                 accu:=List.tl(!accu);
                 print_string(left^string_of_int(j)^s0);
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

let e_rev job_description l=
   let accu=ref([]) in 
   let f=(fun x->accu:=x::(!accu)) in
   let _=iter0(f)(l)(job_description," (rev part)") in
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
    tester=(fun (_graet,_p,_q,da_ober)->da_ober<>[]) 
    and
    shower=(
     fun (_graet,p,q,_da_ober)->
       (string_of_int p)^" to be explored ; "^
       (string_of_int q)^" already explored\n"
    )
    in
    let initial_value=([],List.length l,0,l) in
    let _=(print_string(shower(initial_value));flush stdout) in
    let (ans,_,_,_)=iter2 g initial_value tester shower  "" in
    ans;;



let unchronometered_filter job_description f l=
   let accu=ref([]) in 
   let g=(fun x->if f(x) then accu:=x::(!accu) else ()) in
   let _=iter0(g)(l)(job_description," (filter part)") in
   e_rev job_description (!accu);;    
 
 
 let unchronometered_image job_description f l=
   let accu=ref([]) in 
   let g=(fun x->accu:=f(x)::(!accu)) in
   let _=iter0(g)(l)(job_description," (rev_image part)") in
   e_rev job_description (!accu);;  
   

let unchronometered_image_computed_backwards job_description f l=
   let temp1=e_rev job_description l in
    let accu=ref([]) in 
   let g=(fun x->accu:=f(x)::(!accu)) in
   let _=iter0(g)(temp1)(job_description," (image part)") in
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

let explore_tree f l=Stdl_chronometer.it (Private.unchronometered_explore_tree f) l;; 
           
let filter ?(job_description="") f l=Stdl_chronometer.it (Private.unchronometered_filter job_description f) l;; 


let image ?(job_description="") f l=Stdl_chronometer.it (Private.unchronometered_image job_description f) l;;  

let image_computed_backwards ?(job_description="") f l=Stdl_chronometer.it 
   	(Private.unchronometered_image_computed_backwards job_description f) l;;               

let opt_find f x = Private.helper_for_opt_finding (f,string_of_int(List.length x)) (1,x) ;;

(* opt_find (fun t->t>4) (Ennig.ennig 1 7);; *)


end;;






module Stdl_stabilize=struct

(*

#use"lib/stabilize.ml";;

*) 


module Private = struct 

  exception Pusher_for_exploring_enhanced_tree_exn ;;

  let pusher_for_exploring_enhanced_tree f (treated,to_be_treated) =
     match to_be_treated with 
     [] -> raise(Pusher_for_exploring_enhanced_tree_exn)
    |item :: others ->
       let pair = f item in 
       let (_extra_info,descendants) = pair in 
       ((item,pair)::treated,descendants@others) ;;
       
  let rec iterator_for_exploring_enhanced_tree f walker =
     let  (treated,to_be_treated) = walker in 
     if to_be_treated = []
     then treated 
     else let next_walker = pusher_for_exploring_enhanced_tree f walker in 
          iterator_for_exploring_enhanced_tree f next_walker ;;   
       
  let explore_enhanced_tree f l = iterator_for_exploring_enhanced_tree f ([],l);;
  

end ;;  

let explore_enhanced_tree = Private.explore_enhanced_tree ;; 

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
 
 

end;;






module Stdl_unix_again=struct

(*

#use"lib/unix_again.ml";;

*)


exception Timeout ;;

module Private=struct
 
let naive_extension ap=
   let s=Stdl_absolute_path.to_string ap in
   let i=String.rindex s '.' in
   (Stdl_cull_string.cobeginning (i+1) s);; 
   
let extension x=try (naive_extension x) with 
  _any_exception->"";;
  
 let is_a_directory ap=
   let s=Stdl_absolute_path.to_string ap in
   try (function _x->true)(Sys.readdir s) with _any_exception->false;;
 
 let father ap=
   let s=Stdl_absolute_path.to_string ap in
   let i=String.rindex s '/' in
   if i=0 then Stdl_directory_name.of_string"/" else
   Stdl_directory_name.of_string (Stdl_cull_string.beginning i s);; 
   
 let son dir=
   let s=Stdl_directory_name.connectable_to_subpath dir in
   let i=String.rindex s '/' in
   if i=0 then "" else
   (Stdl_cull_string.cobeginning (i+1) s);; 
  
 let is_a_nondirectory_or_a_nib x=
  if is_a_directory(x)
  then extension(x)="nib"
  else not(Stdl_substring.is_a_substring_of(".nib/")(Stdl_absolute_path.to_string x));;
  
 let naive_ls dir=
   let s=Stdl_directory_name.connectable_to_subpath dir in
   let s_with_slash=(function ()->
    if String.get(s)(String.length(s)-1)='/'
    then s
    else s^"/"
   )() in
   let temp1=Array.to_list(Sys.readdir(s)) in
   let tempf=(function w->try (Some(Stdl_absolute_path.of_string(s_with_slash^w))) with
   _any_exception->None) in
   List.filter_map tempf temp1;;
   
 let ls x=try (naive_ls x) with _any_exception->[];;  
 
 let test_for_cleaniness=function ap->
  let s=Stdl_absolute_path.to_string ap in
  Stdl_cull_string.after_rightmost(s)('/')<>".DS_Store";;
 
 let cleaned_ls x=
   List.filter test_for_cleaniness (ls x);;
   
let select_by_prefix subdir forbidden_subdirs =
  List.filter_map (
     fun forb_subdir -> 
        if String.starts_with ~prefix:subdir  forb_subdir 
        then Some(Stdl_cull_string.two_sided_cutting (subdir,"") forb_subdir)
        else None
  ) forbidden_subdirs ;; 

let ls_with_ignored_subdirs (dir,forbidden_subdirs)=
   let temp1 = Array.to_list (Sys.readdir dir) in
   let temp2 = List.filter_map (
      fun fname -> if List.for_all (
          fun forb_subdir -> 
           not(String.starts_with ~prefix:forb_subdir fname)
        )  forbidden_subdirs
           then Some(dir^fname)
           else None
   ) temp1 in 
   let is_a_dir  = (fun s->is_a_directory(Stdl_absolute_path.AP(s))) in 
   let (found_dirs,found_nondirs) = List.partition is_a_dir temp2 in 
   let new_constraints = Stdl_image.image (
     fun full_subdir_path ->
        let subdir = Stdl_cull_string.two_sided_cutting (dir,"") full_subdir_path in 
       (full_subdir_path^"/",select_by_prefix subdir forbidden_subdirs)
   ) found_dirs in 
   (found_nondirs,found_dirs,new_constraints);;

let rec helper_for_complete_ls_with_ignored_subdirs 
  (verbose,treated_nondirs,treated_dirs,to_be_treated) = match to_be_treated with 
  [] -> (Stdl_image.image Stdl_absolute_path.of_string treated_nondirs,treated_dirs)
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
   let s_dir = Stdl_directory_name.connectable_to_subpath dir in 
   let _= (if not verbose then (print_string "Enumerating files ...";flush stdout)) in 
   let (treated_nondirs,treated_dirs) = 
   helper_for_complete_ls_with_ignored_subdirs 
  (verbose,[],[],[s_dir,
         Stdl_image.image Stdl_dfa_subdirectory.without_trailing_slash forbidden_subdirs]) in 
   let m = string_of_int(List.length treated_nondirs) in 
   let msg = " "^m^" files found.\n" in 
   let _= (if not verbose then (print_string msg;flush stdout)) in          
   (treated_nondirs,Stdl_image.image 
     (fun x->Stdl_dfa_subdirectory.of_line(Stdl_cull_string.two_sided_cutting (s_dir,"") x)) 
   treated_dirs);;

let ls_with_directories_only dir=
   let temp1 = cleaned_ls dir in 
   List.filter_map (
     fun ap -> 
       if is_a_directory ap 
       then let s_ap = Stdl_absolute_path.to_string ap in 
            Some(Stdl_directory_name.of_string s_ap)
       else None
   )  temp1 ;;

 let dirty_ones_in_ls x=
   List.filter (function u->not(test_for_cleaniness u) )(ls x);; 
 
 let adhoc_ls ap=
   let s=Stdl_absolute_path.to_string ap in
   if not(is_a_directory ap) 
   then []
   else 
   let dir=Stdl_directory_name.of_string s in
   ls dir;;
 

 
let complete_ls dir=
   let s_dir=Stdl_directory_name.connectable_to_subpath dir in
   let x=Stdl_absolute_path.of_string s_dir in
   Stdl_explicit.explore_tree adhoc_ls [x];;   

let adhoc_ls_with_ignored_subdirs ap=
   let s=Stdl_absolute_path.to_string ap in
   if not(is_a_directory ap) 
   then []
   else 
   let dir=Stdl_directory_name.of_string s in
   ls dir;;

let complete_ls_with_directories_only x=
  Stdl_explicit.explore_tree ls_with_directories_only [x];;
  

 let complete_ls_with_nondirectories_only x=
  List.filter(is_a_nondirectory_or_a_nib)(complete_ls x);;
  
  
 let beheaded_ls_with_nondirectories_only x=
  let n0=String.length(Stdl_absolute_path.to_string x) in
  let temp1=List.filter(is_a_nondirectory_or_a_nib)(adhoc_ls x) in
  let temp2=Stdl_image.image (fun ap->Stdl_cull_string.cobeginning n0 (Stdl_absolute_path.to_string ap)) temp1 in
  temp2;; 
 
 let dir_substructure x=
    let n0=String.length(Stdl_absolute_path.to_string x) in
    let temp1=(Stdl_stabilize.explore_tree adhoc_ls (adhoc_ls x)) in
    let temp2=List.filter(function x->extension(x)<>"nib")(temp1) in
    List.rev_map(function ap->Stdl_cull_string.cobeginning n0 (Stdl_absolute_path.to_string ap))(temp2);;
  
 let endfiles x=
    let n0=String.length(Stdl_absolute_path.to_string x)+1(*because of the slash!*) in
    let temp1=(Stdl_stabilize.explore_tree adhoc_ls (adhoc_ls x)) in
    let temp2=List.filter(is_a_nondirectory_or_a_nib)(temp1) in
    List.rev_map(function ap->Stdl_cull_string.cobeginning n0 (Stdl_absolute_path.to_string ap))(temp2);;
    
let quick_complete_ls s=
  let x=Stdl_directory_name.of_string s in
  let temp1=complete_ls x in
  Stdl_image.image Stdl_absolute_path.to_string temp1;;  
  
 

let quick_beheaded_complete_ls s=
  let x=Stdl_directory_name.of_string s in
  let n=String.length(Stdl_directory_name.connectable_to_subpath x) in
  let temp1=complete_ls x in
  Stdl_image.image (fun ap->Stdl_cull_string.cobeginning n (Stdl_absolute_path.to_string ap)) temp1;; 
  
let beheaded_simple_ls dir=
  let n=String.length(Stdl_directory_name.connectable_to_subpath dir) in
  let temp1=ls dir in
  Stdl_image.image (fun ap->
   Stdl_cull_string.cobeginning n (Stdl_absolute_path.to_string ap)) temp1;; 


let clear_directory_contents root =
    let s_root = Stdl_dfa_root.connectable_to_subpath root in 
    let cmd = "rm -rf "^s_root^"*" in 
    Sys.command cmd;;


let delete_directory root =
  let s_root = Stdl_dfa_root.connectable_to_subpath root in 
  let cmd = "rm -rf "^s_root in 
  Sys.command cmd;;
  
let empty_directory s_root =
   let cmd = "rm -rf "^s_root^"*" in 
   Sys.command cmd;;      

let create_subdirs_and_fill_files_if_necessary root subdirs 
  (pointed_files_with_content,nonpointed_files_with_content) =
   let s_root = Stdl_dfa_root.connectable_to_subpath root in 
   let cmds1=Stdl_image.image (
      fun subdir -> 
         "mkdir -p "^s_root^(Stdl_dfa_subdirectory.without_trailing_slash subdir)
   ) subdirs in 
   let _=Stdl_image.image Sys.command cmds1 in 
   let temp1=List.filter_map (
     fun (rootless,content)->
        let full_path = Stdl_dfn_full.to_line(Stdl_dfn_join.root_to_rootless root rootless) in 
        if Sys.file_exists full_path 
        then None 
        else Some(full_path,content)
   ) pointed_files_with_content 
   and temp2=List.filter_map (
    fun (nonpointed,content)->
       let full_path = s_root ^ nonpointed in 
       if Sys.file_exists full_path 
       then None 
       else Some(full_path,content)
  ) nonpointed_files_with_content in 
   Stdl_image.image (
      fun (full_path,content) ->
         let _=Sys.command("touch "^full_path) in 
         Stdl_io.overwrite_with (Stdl_absolute_path.of_string full_path) content
   )  (temp1 @ temp2);;


let create_subdirs_and_fill_files root subdirs 
  (pointed_files_with_content,nonpointed_files_with_content) =
   let s_root = Stdl_dfa_root.connectable_to_subpath root in 
   let cmds1=Stdl_image.image (
      fun subdir -> 
         "mkdir -p "^s_root^(Stdl_dfa_subdirectory.without_trailing_slash subdir)
   ) subdirs in 
   let _=Stdl_image.image Sys.command cmds1 in 
   (Stdl_image.image (
     fun (rootless,content)->
        let full_path = Stdl_dfn_full.to_line(Stdl_dfn_join.root_to_rootless root rootless) in 
         let _=Sys.command("touch "^full_path) in 
         Stdl_io.overwrite_with (Stdl_absolute_path.of_string full_path) content
   ) pointed_files_with_content,
   Stdl_image.image (
     fun (nonpointed,content)->
        let full_path = s_root ^ nonpointed in 
         let _=Sys.command("touch "^full_path) in 
         Stdl_io.overwrite_with (Stdl_absolute_path.of_string full_path) content
   ) nonpointed_files_with_content
   );;

let compute_with_time_constraint f x timeout =
      let _ =
        Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout))
      in
      ignore (Unix.alarm timeout);
      try
        let r = f x in
        ignore (Unix.alarm 0); r
      with
      | e  -> ignore (Unix.alarm 0); raise e ;; 

let get_command_output cmd =
  let ic = Unix.open_process_in cmd in
  let buffer = Buffer.create 1024 in
  (try
     while true do
       let line = input_line ic in
       Buffer.add_string buffer line;
       Buffer.add_char buffer '\n'
     done
   with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  Buffer.contents buffer ;;

end;;    


let all_files_with_endings dir l_endings=
   let temp1=Private.complete_ls dir in
   let temp2=List.filter(
   fun ap->
     let s_ap=Stdl_absolute_path.to_string ap in
     List.exists( fun ending->
      String.ends_with ~suffix:ending s_ap)
     l_endings  
   ) temp1 in
   temp2;;  
let beheaded_simple_ls=Private.beheaded_simple_ls;;
let complete_ls=Private.complete_ls;;
let complete_ls_with_directories_only=Private.complete_ls_with_directories_only;;
let complete_ls_with_ignored_subdirs=Private.complete_ls_with_ignored_subdirs;;
let complete_ls_with_nondirectories_only=Private.complete_ls_with_nondirectories_only;;
let compute_with_time_constraint = Private.compute_with_time_constraint ;; 
let clear_directory_contents = Private.clear_directory_contents;;
let create_subdirs_and_fill_files = Private.create_subdirs_and_fill_files;;
let create_subdirs_and_fill_files_if_necessary = Private.create_subdirs_and_fill_files_if_necessary;;
let delete_directory = Private.delete_directory ;;
let empty_directory = Private.empty_directory ;;
let get_command_output = Private.get_command_output ;;
let is_a_directory=Private.is_a_directory;;   
let quick_beheaded_complete_ls=Private.quick_beheaded_complete_ls;;           
let simple_ls=Private.ls;;

end;;






module Stdl_cee_snapshot=struct

(*
#use"lib/Cee_language/cee_snapshot.ml";;
*)


    type t = Sn of Stdl_cee_snapshot_parameters_t.t ;;

    let str_order = Stdl_total_ordering.lex_for_strings ;; 
    let str_sort = Stdl_ordered.sort str_order ;;
    let str_setminus = Stdl_ordered.setminus str_order ;;

    let conventional_snapshot_location root idx suffix=
    let s_idx = Stdl_strung.insert_repetitive_offset_on_the_left
      '0' 3 (string_of_int idx) in 
     (Stdl_directory_name.connectable_to_subpath root)^
     "snapshot-"^s_idx^"-"^suffix ;;

    let root (Sn snap) = snap.Stdl_cee_snapshot_parameters_t.root ;;
    let suffix (Sn snap) = snap.Stdl_cee_snapshot_parameters_t.suffix_for_snapshots ;;
    let index (Sn snap) = snap.Stdl_cee_snapshot_parameters_t.index ;;
    
    let source snap = 
       Stdl_directory_name.of_string(
        conventional_snapshot_location (root snap) (index snap) (suffix snap)
      );;
     
    let destination snap = 
      let s_dir = conventional_snapshot_location (root snap) ((index snap)+1) (suffix snap) in 
      let _ = (if not(Sys.file_exists s_dir)
      then   Sys.command ("mkdir -p "^s_dir) else 0) in 
      Stdl_directory_name.of_string(
       s_dir
      ) ;;
    
     
    let hashtbl_for_file_reading = 
      (Hashtbl.create 4000: (t * string,string) Hashtbl.t) ;; 

    let read_file snap fn =
      match Hashtbl.find_opt hashtbl_for_file_reading (snap,fn) with
      | Some old_answer -> old_answer
      | None ->
        let src_dir = Stdl_directory_name.connectable_to_subpath (source snap) in
        let ap = Stdl_absolute_path.of_string (src_dir ^ fn) in
        let text = Stdl_io.read_whole_file ap in
        let _ = Hashtbl.add hashtbl_for_file_reading (snap,fn) text in
        text
    ;;


    
let hashtbl_for_commands = 
  (Hashtbl.create 20: (t,Stdl_cee_compilation_command_t.t list) Hashtbl.t) ;; 

let commands snap = 
    match Hashtbl.find_opt hashtbl_for_commands snap with 
  (Some old_answer) -> old_answer 
  | None ->
   let temp1 = read_file snap "Makefile" in 
   let mkf1 = Stdl_makefile.parse (Stdl_makefile_t.MT temp1) in 
   let (_,cmds_and_echoes) = Stdl_makefile.prerequisites_and_commands_for_target 
   (ref mkf1) "all" in
   let cmds = List.filter (fun line->
    not(String.starts_with line ~prefix:"@echo")) cmds_and_echoes in 
   let answer = Stdl_image.image (Stdl_cee_compilation_command.parse (source snap)) cmds in 
   let _ = Hashtbl.add hashtbl_for_commands snap answer in 
      answer ;; 


let hashtbl_for_all_h_or_c_files = 
  (Hashtbl.create 20: (t,string list) Hashtbl.t) ;; 

let all_h_or_c_files snap = 
    match Hashtbl.find_opt hashtbl_for_all_h_or_c_files snap with 
  (Some old_answer) -> old_answer 
  | None ->
    let src = Stdl_directory_name.connectable_to_subpath (source snap) in
      let temp1 = Stdl_unix_again.quick_beheaded_complete_ls src in
      let answer =  Stdl_ordered.to_list(str_sort
        (List.filter
           (fun fn ->
             List.exists
               (fun edg -> String.ends_with fn ~suffix:edg)
               [ ".h"; ".c"; ".macros" ])
           temp1)) in 
    let _ = Hashtbl.add hashtbl_for_all_h_or_c_files snap answer in 
      answer ;; 
let hashtbl_for_separate_commands = 
  (Hashtbl.create 20: (t,Stdl_cee_compilation_command_t.separate_t list) Hashtbl.t) ;; 

let separate_commands snap = 
    match Hashtbl.find_opt hashtbl_for_separate_commands snap with 
  (Some old_answer) -> old_answer 
  | None ->
    let answer =  List.filter_map
        (function
          | Stdl_cee_compilation_command_t.Batch _ -> None
          | Stdl_cee_compilation_command_t.Separate s -> Some s)
    (commands snap) in 
    let _ = Hashtbl.add hashtbl_for_separate_commands snap answer in 
    answer ;; 

    let hashtbl_for_directly_compiled_files = 
      (Hashtbl.create 20: (t,string list) Hashtbl.t) ;; 

    let directly_compiled_files snap = 
       match Hashtbl.find_opt hashtbl_for_directly_compiled_files snap with 
      (Some old_answer) -> old_answer 
      | None ->
      let answer = Stdl_ordered.to_list(str_sort
        (Stdl_image.image
           Stdl_cee_compilation_command.short_name_from_separate
           (separate_commands snap))) in 
      let _ = Hashtbl.add hashtbl_for_directly_compiled_files snap answer in 
      answer ;; 

   
    let modify_file snap fn new_content =
      let dest_dir = Stdl_directory_name.connectable_to_subpath (destination snap) in
      let ap = Stdl_absolute_path.of_string (dest_dir ^ fn) in
      Stdl_io.overwrite_with ap new_content
    ;;

    let create_file_in_a_list snap fn 
      ?new_content_description ~is_temporary new_content index_msg=
      let containing_dir = ( 
         if is_temporary 
         then Stdl_directory_name.connectable_to_subpath (source snap) 
         else Stdl_directory_name.connectable_to_subpath (destination snap) 
      ) in
      let ap = Stdl_absolute_path.create_file_if_absent (containing_dir ^ fn) in
      let _ = Stdl_io.overwrite_with ap new_content in
      let end_of_msg =
        match new_content_description with
        | None -> ""
        | Some descr -> ", with content " ^ descr
      in
      let durability =(
         if is_temporary then "temporary" else "persistent") in 
      Stdl_basic.announce ("Created "^durability^" file  " ^ fn ^ end_of_msg ^ index_msg)
    ;;

    let create_file snap fn ?new_content_description ~is_temporary new_content =
     create_file_in_a_list snap fn 
      ?new_content_description ~is_temporary new_content ""
    ;;
   
  
    


  
   
 
  let reinitialize_destination_directory snap =
    let src = Stdl_directory_name.connectable_to_subpath (source snap)
    and slashed_dest = Stdl_directory_name.connectable_to_subpath (destination snap) in
    let dest = Stdl_cull_string.coending 1 slashed_dest in
    let _ = Stdl_unix_command.conditional_multiple_uc
      [ "rm -rf " ^ dest ^ "/*"
      ; "cp -R " ^ src ^ "/* " ^ dest ^ "/"
      ; "cp " ^ src ^ "/.gdbinit " ^ dest ^ "/"
      ] in 
    ()
    ;;
  
  
  let parameters (Sn params) = params ;;

  let take_possession params = Sn params ;;



end;;






(* Contents of ~/Teuliou/OCaml/skeptical_duck/lib/Cee_language/cee_snapshot.mli:

 
  type t 

  val commands : t -> Cee_compilation_command_t.t list
  val destination : t -> Directory_name_t.t
  val parameters : t -> Cee_snapshot_parameters_t.t 
  val source : t -> Directory_name_t.t
  val take_possession : Cee_snapshot_parameters_t.t -> t  

  val all_h_or_c_files : t -> string list
  val directly_compiled_files : t -> string list
  val separate_commands : t -> Cee_compilation_command_t.separate_t list
  
  val create_file : t -> string -> ?new_content_description:string -> is_temporary:bool -> string -> unit
  val read_file : t -> string -> string
  val modify_file : t -> string -> string -> unit
  val reinitialize_destination_directory : t -> unit  


*) 

