(*

#directory "+str";;
#load "str.cma";;
#directory "+unix";;
#load "unix.cma";;
#use"Teuliou/OCaml/skeptical_duck/watched/watched_and_githubbed/Standalone_old_files/standalone_java_analysis.ml";;

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
   


end ;;    

let assoc_right_opt y l = 
    Option.map fst (List.find_opt (fun p->snd(p)=y) l);;

(*
assoc_right_opt 3 (Int_range.scale (fun t->(t*t,t)) 1 7) ;;
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






module Stdl_jvsp_types=struct

(*

#use"lib/Java_analysis/jvsp_types.ml";;

*)


type token =
  IDENTIFIER of string
(* Literals *)
|BOOLEAN_LITERAL of bool
|CHARACTER_LITERAL of string
|FLOATING_POINT_LITERAL of string
|INTEGER_LITERAL of string
|NULL_LITERAL
|STRING_LITERAL of string
|TEXT_BLOCK of string
|LOWLEVEL_TYPE of string
(* Separators *)
|LP		(* ( *)
|RP		(* ) *)
|LC		(* { *)
|RC		(* } *)
|LB		(* [ *)
|RB		(* ] *)
|SM		(* ; *)
|CM		(* , *)
|DOT	(* . *)

(* Operators *)
|EQ		  (* = *)
|GT		  (* > *)
|LT		  (* < *)
|NOT		(* ! *)
|COMPL	(* ~ *)
|COND		(* ? *)
|COLON	(* : *)
|EQ_EQ	(* == *)
|LE		  (* <= *)
|GE		  (* >= *)
|NOT_EQ		(* != *)
|AND_AND	(* && *)
|OR_OR	(* || *)
|INCR		(* ++ *)
|DECR		(* -- *)
|PLUS		(* + *)
|MINUS	(* - *)
|TIMES	(* * *)
|DIV		(* / *)
|AND		(* & *)
|OR		  (* | *)
|XOR		(* ^ *)
|MOD		(* % *)
|LS		  (* << *)
|SRS		(* >> *)
|URS		(* >>> *)
|OPERATOR_EQ of string	(* += -= *= /= &= |= ^= %= <<= >>= >>>= *)
|SNAIL (* @ *)
(* Keywords*)
|ABSTRACT |ASSERT |BOOLEAN |BREAK |BYTE |CASE |CATCH |CHAR |CLASS 
|CONST |CONTINUE |DEFAULT |DO |DOUBLE |ELSE |ENUM |EXPORTS |EXTENDS 
|FINAL |FINALLY |FLOAT |FOR |GOTO |IF |IMPLEMENTS |IMPORT |INSTANCEOF 
|INT |INTERFACE |LONG |MODULE |NATIVE |NEW |NONSEALED |OPEN |OPENS 
|PACKAGE |PERMITS |PRIVATE |PROTECTED |PROVIDES |PUBLIC |RECORD |REQUIRES 
|RETURN |SEALED |SHORT |STATIC |STRICTFP |SUPER |SWITCH |SYNCHRONIZED
|THIS |THROW |THROWS |TRANSIENT |TRANSITIVE |TRY |TO |USES 
|VAR |VOID |VOLATILE |WHILE |WITH |YIELD
|EOF 
(* Inactive tokens during parsing *)
|COMMENT of string 
|WHITESPACE of string 
|LINEBREAK of string;;

type positioned_token = { tok:token; start:int; endd:int;} ;;

type token_type =
  IDENTIFIER_T 
|BOOLEAN_LITERAL_T
|CHARACTER_LITERAL_T
|FLOATING_POINT_LITERAL_T
|INTEGER_LITERAL_T
|NULL_LITERAL_T
|STRING_LITERAL_T
|TEXT_BLOCK_T
|LOWLEVEL_TYPE_T
(* Separators *)
|LP_T		(* ( *)
|RP_T		(* ) *)
|LC_T		(* { *)
|RC_T		(* } *)
|LB_T		(* [ *)
|RB_T		(* ] *)
|SM_T		(* ; *)
|CM_T		(* , *)
|DOT_T	(* . *)

(* Operators *)
|EQ_T		  (* = *)
|GT_T		  (* > *)
|LT_T		  (* < *)
|NOT_T		(* ! *)
|COMPL_T	(* ~ *)
|COND_T		(* ? *)
|COLON_T	(* : *)
|EQ_EQ_T	(* == *)
|LE_T		  (* <= *)
|GE_T		  (* >= *)
|NOT_EQ_T		(* != *)
|AND_AND_T	(* && *)
|OR_OR_T	(* || *)
|INCR_T		(* ++ *)
|DECR_T		(* -- *)
|PLUS_T		(* + *)
|MINUS_T	(* - *)
|TIMES_T	(* * *)
|DIV_T		(* / *)
|AND_T		(* & *)
|OR_T		    (* | *)
|XOR_T		(* ^ *)
|MOD_T		(* % *)
|LS_T		  (* << *)
|SRS_T		(* >> *)
|URS_T		(* >>> *)
|OPERATOR_EQ_T 	(* += -= *= /= &= |= ^= %= <<= >>= >>>= *)
|SNAIL_T 
(* Keywords*)
|ABSTRACT_T |ASSERT_T |BOOLEAN_T |BREAK_T |BYTE_T |CASE_T |CATCH_T |CHAR_T |CLASS_T 
|CONST_T |CONTINUE_T |DEFAULT_T |DO_T |DOUBLE_T |ELSE_T |ENUM_T |EXPORTS_T |EXTENDS_T 
|FINAL_T |FINALLY_T |FLOAT_T |FOR_T |GOTO_T |IF_T |IMPLEMENTS_T |IMPORT_T |INSTANCEOF_T 
|INT_T |INTERFACE_T |LONG_T |MODULE_T |NATIVE_T |NEW_T |NONSEALED_T |OPEN_T |OPENS_T 
|PACKAGE_T |PERMITS_T |PRIVATE_T |PROTECTED_T |PROVIDES_T |PUBLIC_T |RECORD_T |REQUIRES_T 
|RETURN_T |SEALED_T |SHORT_T |STATIC_T |STRICTFP_T |SUPER_T |SWITCH_T |SYNCHRONIZED_T
|THIS_T |THROW_T |THROWS_T |TRANSIENT_T |TRANSITIVE_T |TRY_T |TO_T |USES_T 
|VAR_T |VOID_T |VOLATILE_T |WHILE_T |WITH_T |YIELD_T
|EOF_T  
|COMMENT_T 
|WHITESPACE_T 
|LINEBREAK_T ;;


type 'a parser = Parser of ((positioned_token list) -> int -> ('a * int) option) ;;

type token_type_list = TTL of (token_type list) ;;

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






module Stdl_jvsp_util=struct

(*

#use"lib/Java_analysis/jvsp_util.ml";;

*)

open Stdl_jvsp_types ;;

exception Parse_code_sequence_exn of string * string * (string list) ;;   
exception Token_type_sequence_from_codes_in_production_names_exn of string ;;


module Private = struct

let ocaml_name_for_token_type = function 
|IDENTIFIER_T -> "IDENTIFIER_T"
|BOOLEAN_LITERAL_T -> "BOOLEAN_LITERAL_T"
|CHARACTER_LITERAL_T -> "CHARACTER_LITERAL_T"
|FLOATING_POINT_LITERAL_T -> "FLOATING_POINT_LITERAL_T"
|INTEGER_LITERAL_T  -> "INTEGER_LITERAL_T"
|NULL_LITERAL_T  -> "NULL_LITERAL_T"
|STRING_LITERAL_T  -> "STRING_LITERAL_T"
|TEXT_BLOCK_T  -> "TEXT_BLOCK_T"
|LOWLEVEL_TYPE_T  -> "LOWLEVEL_TYPE_T"
|LP_T -> "LP_T"
|RP_T -> "RP_T"
|LC_T -> "LC_T"
|RC_T -> "RC_T"
|LB_T -> "LB_T"
|RB_T -> "RB_T"
|SM_T -> "SM_T"
|CM_T -> "CM_T"
|DOT_T -> "DOT_T"
|EQ_T -> "EQ_T"
|GT_T -> "GT_T"
|LT_T -> "LT_T"
|NOT_T -> "NOT_T"
|COMPL_T -> "COMPL_T"
|COND_T -> "COND_T"
|COLON_T -> "COLON_T"
|EQ_EQ_T -> "EQ_EQ_T"
|LE_T -> "LE_T"
|GE_T -> "GE_T"
|NOT_EQ_T -> "NOT_EQ_T"
|AND_AND_T -> "AND_AND_T"
|OR_OR_T -> "OR_OR_T"
|INCR_T -> "INCR_T"
|DECR_T -> "DECR_T"
|PLUS_T -> "PLUS_T"
|MINUS_T -> "MINUS_T"
|TIMES_T -> "TIMES_T"
|DIV_T -> "DIV_T"
|AND_T -> "AND_T"
|OR_T -> "OR_T"
|XOR_T -> "XOR_T"
|MOD_T -> "MOD_T"
|LS_T -> "LS_T"
|SRS_T -> "SRS_T"
|URS_T -> "URS_T"
|OPERATOR_EQ_T -> "OPERATOR_EQ_T"
|SNAIL_T -> "SNAIL_T"
|ABSTRACT_T -> "ABSTRACT_T"
|ASSERT_T -> "ASSERT_T"
|BOOLEAN_T -> "BOOLEAN_T"
|BREAK_T -> "BREAK_T"
|BYTE_T -> "BYTE_T"
|CASE_T -> "CASE_T"
|CATCH_T -> "CATCH_T"
|CHAR_T -> "CHAR_T"
|CLASS_T -> "CLASS_T"
|CONST_T -> "CONST_T"
|CONTINUE_T -> "CONTINUE_T"
|DEFAULT_T -> "DEFAULT_T"
|DO_T -> "DO_T"
|DOUBLE_T -> "DOUBLE_T"
|ELSE_T -> "ELSE_T"
|ENUM_T -> "ENUM_T"
|EXPORTS_T -> "EXPORTS_T"
|EXTENDS_T -> "EXTENDS_T"
|FINAL_T -> "FINAL_T"
|FINALLY_T -> "FINALLY_T"
|FLOAT_T -> "FLOAT_T"
|FOR_T -> "FOR_T"
|GOTO_T -> "GOTO_T"
|IF_T -> "IF_T"
|IMPLEMENTS_T -> "IMPLEMENTS_T"
|IMPORT_T -> "IMPORT_T"
|INSTANCEOF_T -> "INSTANCEOF_T"
|INT_T -> "INT_T"
|INTERFACE_T -> "INTERFACE_T"
|LONG_T -> "LONG_T"
|MODULE_T -> "MODULE_T"
|NATIVE_T -> "NATIVE_T"
|NEW_T -> "NEW_T"
|NONSEALED_T -> "NONSEALED_T"
|OPEN_T -> "OPEN_T"
|OPENS_T -> "OPENS_T"
|PACKAGE_T -> "PACKAGE_T"
|PERMITS_T -> "PERMITS_T"
|PRIVATE_T -> "PRIVATE_T"
|PROTECTED_T -> "PROTECTED_T"
|PROVIDES_T -> "PROVIDES_T"
|PUBLIC_T -> "PUBLIC_T"
|RECORD_T -> "RECORD_T"
|REQUIRES_T -> "REQUIRES_T"
|RETURN_T -> "RETURN_T"
|SEALED_T -> "SEALED_T"
|SHORT_T -> "SHORT_T"
|STATIC_T -> "STATIC_T"
|STRICTFP_T -> "STRICTFP_T"
|SUPER_T -> "SUPER_T"
|SWITCH_T -> "SWITCH_T"
|SYNCHRONIZED_T -> "SYNCHRONIZED_T"
|THIS_T -> "THIS_T"
|THROW_T -> "THROW_T"
|THROWS_T -> "THROWS_T"
|TRANSIENT_T -> "TRANSIENT_T"
|TRANSITIVE_T -> "TRANSITIVE_T"
|TRY_T -> "TRY_T"
|TO_T -> "TO_T"
|USES_T -> "USES_T"
|VAR_T -> "VAR_T"
|VOID_T -> "VOID_T"
|VOLATILE_T -> "VOLATILE_T"
|WHILE_T -> "WHILE_T"
|WITH_T -> "WITH_T"
|YIELD_T -> "YIELD_T"
|EOF_T -> "EOF_T"
|COMMENT_T -> "COMMENT_T"
|WHITESPACE_T -> "WHITESPACE_T"
|LINEBREAK_T -> "LINEBREAK_T" ;;

let get_token_type = function
|(IDENTIFIER _) -> IDENTIFIER_T
|(BOOLEAN_LITERAL _) -> BOOLEAN_LITERAL_T
|(CHARACTER_LITERAL _) -> CHARACTER_LITERAL_T
|(FLOATING_POINT_LITERAL _) -> FLOATING_POINT_LITERAL_T
|(INTEGER_LITERAL _) -> INTEGER_LITERAL_T
|NULL_LITERAL -> NULL_LITERAL_T
|(STRING_LITERAL _) -> STRING_LITERAL_T
|(TEXT_BLOCK _) -> TEXT_BLOCK_T
|(LOWLEVEL_TYPE _) -> LOWLEVEL_TYPE_T
|LP -> LP_T
|RP -> RP_T
|LC -> LC_T
|RC -> RC_T
|LB -> LB_T
|RB -> RB_T
|SM -> SM_T
|CM -> CM_T
|DOT -> DOT_T
|EQ -> EQ_T
|GT -> GT_T
|LT -> LT_T
|NOT -> NOT_T
|COMPL -> COMPL_T
|COND -> COND_T
|COLON -> COLON_T
|EQ_EQ -> EQ_EQ_T
|LE -> LE_T
|GE -> GE_T
|NOT_EQ -> NOT_EQ_T
|AND_AND -> AND_AND_T
|OR_OR -> OR_OR_T
|INCR -> INCR_T
|DECR -> DECR_T
|PLUS -> PLUS_T
|MINUS -> MINUS_T
|TIMES -> TIMES_T
|DIV -> DIV_T
|AND -> AND_T
|OR -> OR_T
|XOR -> XOR_T
|MOD -> MOD_T
|LS -> LS_T
|SRS -> SRS_T
|URS -> URS_T
|(OPERATOR_EQ _) -> OPERATOR_EQ_T
|SNAIL -> SNAIL_T
|ABSTRACT -> ABSTRACT_T
|ASSERT -> ASSERT_T
|BOOLEAN -> BOOLEAN_T
|BREAK -> BREAK_T
|BYTE -> BYTE_T
|CASE -> CASE_T
|CATCH -> CATCH_T
|CHAR -> CHAR_T
|CLASS -> CLASS_T
|CONST -> CONST_T
|CONTINUE -> CONTINUE_T
|DEFAULT -> DEFAULT_T
|DO -> DO_T
|DOUBLE -> DOUBLE_T
|ELSE -> ELSE_T
|ENUM -> ENUM_T
|EXPORTS -> EXPORTS_T
|EXTENDS -> EXTENDS_T
|FINAL -> FINAL_T
|FINALLY -> FINALLY_T
|FLOAT -> FLOAT_T
|FOR -> FOR_T
|GOTO -> GOTO_T
|IF -> IF_T
|IMPLEMENTS -> IMPLEMENTS_T
|IMPORT -> IMPORT_T
|INSTANCEOF -> INSTANCEOF_T
|INT -> INT_T
|INTERFACE -> INTERFACE_T
|LONG -> LONG_T
|MODULE -> MODULE_T
|NATIVE -> NATIVE_T
|NEW -> NEW_T
|NONSEALED -> NONSEALED_T
|OPEN -> OPEN_T
|OPENS -> OPENS_T
|PACKAGE -> PACKAGE_T
|PERMITS -> PERMITS_T
|PRIVATE -> PRIVATE_T
|PROTECTED -> PROTECTED_T
|PROVIDES -> PROVIDES_T
|PUBLIC -> PUBLIC_T
|RECORD -> RECORD_T
|REQUIRES -> REQUIRES_T
|RETURN -> RETURN_T
|SEALED -> SEALED_T
|SHORT -> SHORT_T
|STATIC -> STATIC_T
|STRICTFP -> STRICTFP_T
|SUPER -> SUPER_T
|SWITCH -> SWITCH_T
|SYNCHRONIZED -> SYNCHRONIZED_T
|THIS -> THIS_T
|THROW -> THROW_T
|THROWS -> THROWS_T
|TRANSIENT -> TRANSIENT_T
|TRANSITIVE -> TRANSITIVE_T
|TRY -> TRY_T
|TO -> TO_T
|USES -> USES_T
|VAR -> VAR_T
|VOID -> VOID_T
|VOLATILE -> VOLATILE_T
|WHILE -> WHILE_T
|WITH -> WITH_T
|YIELD -> YIELD_T
|EOF -> EOF_T
|(COMMENT _) -> COMMENT_T
|(WHITESPACE _) -> WHITESPACE_T 
|(LINEBREAK _) -> LINEBREAK_T ;;


let passive_token_types = [COMMENT_T;WHITESPACE_T;LINEBREAK_T] ;;

let extract_nonpassive_token_type_opt postok =
   let tok =postok.Stdl_jvsp_types.tok in 
   let toktype = get_token_type tok in 
   if List.mem toktype passive_token_types 
   then None 
   else Some toktype ;;     

let summary_of_token_type = function
|IDENTIFIER_T -> "ident"
|BOOLEAN_LITERAL_T -> "bool"
|CHARACTER_LITERAL_T -> "charlit"
|FLOATING_POINT_LITERAL_T -> "float"
|INTEGER_LITERAL_T -> "int"
|STRING_LITERAL_T -> "str"
|TEXT_BLOCK_T -> "text"
|LOWLEVEL_TYPE_T -> "typ"
|OPERATOR_EQ_T -> "opeq"
|COMMENT_T -> "comment"
|WHITESPACE_T -> "white"
|LINEBREAK_T -> "linebreak"
|NULL_LITERAL_T -> "null"
|LP_T -> "("
|RP_T -> ")"
|LC_T -> "{"
|RC_T -> "}"
|LB_T -> "["
|RB_T -> "]"
|SM_T -> ";"
|CM_T -> ","
|DOT_T -> "."
|EQ_T -> "="
|GT_T -> ">"
|LT_T -> "<"
|NOT_T -> "!"
|COMPL_T -> "~"
|COND_T -> "?"
|COLON_T -> ":"
|EQ_EQ_T -> "=="
|LE_T -> "<="
|GE_T -> ">="
|NOT_EQ_T -> "!="
|AND_AND_T -> "&&"
|OR_OR_T -> "||"
|INCR_T -> "++"
|DECR_T -> "--"
|PLUS_T -> "+"
|MINUS_T -> "-"
|TIMES_T -> "*"
|DIV_T -> "/"
|AND_T -> "&"
|OR_T -> "|"
|XOR_T -> "^"
|MOD_T -> "%"
|LS_T -> "<<"
|SRS_T -> ">>"
|URS_T -> ">>>"
|SNAIL_T -> "@"
|ABSTRACT_T -> "abstract"
|ASSERT_T -> "assert"
|BOOLEAN_T -> "boolean"
|BREAK_T -> "break"
|BYTE_T -> "byte"
|CASE_T -> "case"
|CATCH_T -> "catch"
|CHAR_T -> "char"
|CLASS_T -> "class"
|CONST_T -> "const"
|CONTINUE_T -> "continue"
|DEFAULT_T -> "default"
|DO_T -> "do"
|DOUBLE_T -> "double"
|ELSE_T -> "else"
|ENUM_T -> "enum"
|EXPORTS_T -> "exports"
|EXTENDS_T -> "extends"
|FINAL_T -> "final"
|FINALLY_T -> "finally"
|FLOAT_T -> "float"
|FOR_T -> "for"
|GOTO_T -> "goto"
|IF_T -> "if"
|IMPLEMENTS_T -> "implements"
|IMPORT_T -> "import"
|INSTANCEOF_T -> "instanceof"
|INT_T -> "int"
|INTERFACE_T -> "interface"
|LONG_T -> "long"
|MODULE_T -> "module"
|NATIVE_T -> "native"
|NEW_T -> "new"
|NONSEALED_T -> "nonsealed"
|OPEN_T -> "open"
|OPENS_T -> "opens"
|PACKAGE_T -> "package"
|PERMITS_T -> "permits"
|PRIVATE_T -> "private"
|PROTECTED_T -> "protected"
|PROVIDES_T -> "provides"
|PUBLIC_T -> "public"
|RECORD_T -> "record"
|REQUIRES_T -> "requires"
|RETURN_T -> "return"
|SEALED_T -> "sealed"
|SHORT_T -> "short"
|STATIC_T -> "static"
|STRICTFP_T -> "strictfp"
|SUPER_T -> "super"
|SWITCH_T -> "switch"
|SYNCHRONIZED_T -> "synchronized"
|THIS_T -> "this"
|THROW_T -> "throw"
|THROWS_T -> "throws"
|TRANSIENT_T -> "transient"
|TRANSITIVE_T -> "transitive"
|TRY_T -> "try"
|TO_T -> "to"
|USES_T -> "uses"
|VAR_T -> "var"
|VOID_T -> "void"
|VOLATILE_T -> "volatile"
|WHILE_T -> "while"
|WITH_T -> "with"
|YIELD_T -> "yield"
|EOF_T -> "eof" ;;

let has_variable_content = function
|IDENTIFIER_T
|BOOLEAN_LITERAL_T
|CHARACTER_LITERAL_T
|FLOATING_POINT_LITERAL_T
|INTEGER_LITERAL_T
|STRING_LITERAL_T
|TEXT_BLOCK_T
|LOWLEVEL_TYPE_T
|OPERATOR_EQ_T
|COMMENT_T
|WHITESPACE_T
|LINEBREAK_T -> true
|NULL_LITERAL_T
|LP_T
|RP_T
|LC_T
|RC_T
|LB_T
|RB_T
|SM_T
|CM_T
|DOT_T
|EQ_T
|GT_T
|LT_T
|NOT_T
|COMPL_T
|COND_T
|COLON_T
|EQ_EQ_T
|LE_T
|GE_T
|NOT_EQ_T
|AND_AND_T
|OR_OR_T
|INCR_T
|DECR_T
|PLUS_T
|MINUS_T
|TIMES_T
|DIV_T
|AND_T
|OR_T
|XOR_T
|MOD_T
|LS_T
|SRS_T
|URS_T
|SNAIL_T
|ABSTRACT_T
|ASSERT_T
|BOOLEAN_T
|BREAK_T
|BYTE_T
|CASE_T
|CATCH_T
|CHAR_T
|CLASS_T
|CONST_T
|CONTINUE_T
|DEFAULT_T
|DO_T
|DOUBLE_T
|ELSE_T
|ENUM_T
|EXPORTS_T
|EXTENDS_T
|FINAL_T
|FINALLY_T
|FLOAT_T
|FOR_T
|GOTO_T
|IF_T
|IMPLEMENTS_T
|IMPORT_T
|INSTANCEOF_T
|INT_T
|INTERFACE_T
|LONG_T
|MODULE_T
|NATIVE_T
|NEW_T
|NONSEALED_T
|OPEN_T
|OPENS_T
|PACKAGE_T
|PERMITS_T
|PRIVATE_T
|PROTECTED_T
|PROVIDES_T
|PUBLIC_T
|RECORD_T
|REQUIRES_T
|RETURN_T
|SEALED_T
|SHORT_T
|STATIC_T
|STRICTFP_T
|SUPER_T
|SWITCH_T
|SYNCHRONIZED_T
|THIS_T
|THROW_T
|THROWS_T
|TRANSIENT_T
|TRANSITIVE_T
|TRY_T
|TO_T
|USES_T
|VAR_T
|VOID_T
|VOLATILE_T
|WHILE_T
|WITH_T
|YIELD_T
|EOF_T -> false ;;


let token_to_string = function
|(IDENTIFIER txt) -> txt
|(BOOLEAN_LITERAL b) -> string_of_bool b
|(CHARACTER_LITERAL txt) -> txt
|(FLOATING_POINT_LITERAL txt) -> txt
|(INTEGER_LITERAL txt) -> txt
|NULL_LITERAL -> "null"
|(STRING_LITERAL txt) -> txt
|(TEXT_BLOCK txt) -> txt
|(LOWLEVEL_TYPE txt) -> txt
|LP -> "("
|RP -> ")"
|LC -> "{"
|RC -> "}"
|LB -> "["
|RB -> "]"
|SM -> ";"
|CM -> ","
|DOT -> "."
|EQ -> "="
|GT -> ">"
|LT -> "<"
|NOT -> "!"
|COMPL -> "~"
|COND -> "?"
|COLON -> ":"
|EQ_EQ -> "=="
|LE -> "<="
|GE -> ">="
|NOT_EQ -> "!="
|AND_AND -> "&&"
|OR_OR -> "||"
|INCR -> "++"
|DECR -> "--"
|PLUS -> "+"
|MINUS -> "-"
|TIMES -> "*"
|DIV -> "/"
|AND -> "&"
|OR -> "|"
|XOR -> "^"
|MOD -> "%"
|LS -> "<<"
|SRS -> ">>"
|URS -> ">>>"
|(OPERATOR_EQ txt) -> txt
|SNAIL -> "@"
|ABSTRACT -> "abstract"
|ASSERT -> "assert"
|BOOLEAN -> "boolean"
|BREAK -> "break"
|BYTE -> "byte"
|CASE -> "case"
|CATCH -> "catch"
|CHAR -> "char"
|CLASS -> "class"
|CONST -> "const"
|CONTINUE -> "continue"
|DEFAULT -> "default"
|DO -> "do"
|DOUBLE -> "double"
|ELSE -> "else"
|ENUM -> "enum"
|EXPORTS -> "exports"
|EXTENDS -> "extends"
|FINAL -> "final"
|FINALLY -> "finally"
|FLOAT -> "float"
|FOR -> "for"
|GOTO -> "goto"
|IF -> "if"
|IMPLEMENTS -> "implements"
|IMPORT -> "import"
|INSTANCEOF -> "instanceof"
|INT -> "int"
|INTERFACE -> "interface"
|LONG -> "long"
|MODULE -> "module"
|NATIVE -> "native"
|NEW -> "new"
|NONSEALED -> "nonsealed"
|OPEN -> "open"
|OPENS -> "opens"
|PACKAGE -> "package"
|PERMITS -> "permits"
|PRIVATE -> "private"
|PROTECTED -> "protected"
|PROVIDES -> "provides"
|PUBLIC -> "public"
|RECORD -> "record"
|REQUIRES -> "requires"
|RETURN -> "return"
|SEALED -> "sealed"
|SHORT -> "short"
|STATIC -> "static"
|STRICTFP -> "strictfp"
|SUPER -> "super"
|SWITCH -> "switch"
|SYNCHRONIZED -> "synchronized"
|THIS -> "this"
|THROW -> "throw"
|THROWS -> "throws"
|TRANSIENT -> "transient"
|TRANSITIVE -> "transitive"
|TRY -> "try"
|TO -> "to"
|USES -> "uses"
|VAR -> "var"
|VOID -> "void"
|VOLATILE -> "volatile"
|WHILE -> "while"
|WITH -> "with"
|YIELD -> "yield"
|EOF -> "eof"
|(COMMENT txt) -> txt
|(WHITESPACE txt) -> txt 
|(LINEBREAK txt) -> txt;;

let basic_literals= [
   BOOLEAN_LITERAL_T;CHARACTER_LITERAL_T;FLOATING_POINT_LITERAL_T;INTEGER_LITERAL_T
] ;;

let exceptions_for_tokentype_codes_in_production_names = 
   (* ensured that no code is a prefix of another. This is used in
    the token_type_sequence_from_codes_in_production_names below. *)
   [
     AND_AND_T,"StrongAnd";
     DO_T,"Doo";
     EQ_EQ_T,"StrongEq";
     FINALLY_T,"AtLast";
     INTERFACE_T,"Inyourface";
     MODULE_T,"Mudole";
     NOT_EQ_T,"ExclamationEq";
     OPENS_T,"Unlids";
     OR_OR_T,"StrongOr";
     THROWS_T,"Sends";
] ;;

let code_for_token_type_in_production_names toktype = 
   match List.assoc_opt toktype exceptions_for_tokentype_codes_in_production_names with 
   Some answer -> answer 
   |None ->
   let full_name = ocaml_name_for_token_type toktype in 
   let short_name = Stdl_cull_string.two_sided_cutting ("","_T") full_name in 
   let parts = Str.split (Str.regexp_string "_") short_name in 
   let capitalized_lowercase_parts = Stdl_image.image (fun part->String.capitalize_ascii(String.lowercase_ascii part)) parts in 
   let usual = String.concat "" capitalized_lowercase_parts in 
   if List.mem toktype basic_literals 
   then "Lit"^usual 
   else usual;;

let all_token_types = 
[

 ABSTRACT_T;AND_T;AND_AND_T;ASSERT_T;BOOLEAN_T;BOOLEAN_LITERAL_T;BREAK_T;BYTE_T;CASE_T;CATCH_T;
 CHAR_T;CHARACTER_LITERAL_T;CLASS_T;CM_T;COLON_T;COMMENT_T;COMPL_T;COND_T;CONST_T;CONTINUE_T;
 DECR_T;DEFAULT_T;DIV_T;DO_T;DOT_T;DOUBLE_T;ELSE_T;ENUM_T;EOF_T;EQ_T;
 EQ_EQ_T;EXPORTS_T;EXTENDS_T;FINAL_T;FINALLY_T;FLOAT_T;FLOATING_POINT_LITERAL_T;FOR_T;GE_T;GOTO_T;
 GT_T;IDENTIFIER_T;IF_T;IMPLEMENTS_T;IMPORT_T;INCR_T;INSTANCEOF_T;INT_T;INTEGER_LITERAL_T;INTERFACE_T;
 LB_T;LC_T;LE_T;LINEBREAK_T;LONG_T;LOWLEVEL_TYPE_T;LP_T;LS_T;LT_T;MINUS_T;
 MOD_T;MODULE_T;NATIVE_T;NEW_T;NONSEALED_T;NOT_T;NOT_EQ_T;NULL_LITERAL_T;OPEN_T;OPENS_T;
 OPERATOR_EQ_T;OR_T;OR_OR_T;PACKAGE_T;PERMITS_T;PLUS_T;PRIVATE_T;PROTECTED_T;PROVIDES_T;PUBLIC_T;
 RB_T;RC_T;RECORD_T;REQUIRES_T;RETURN_T;RP_T;SEALED_T;SHORT_T;SM_T;SNAIL_T;
 SRS_T;STATIC_T;STRICTFP_T;STRING_LITERAL_T;SUPER_T;SWITCH_T;SYNCHRONIZED_T;TEXT_BLOCK_T;THIS_T;THROW_T;
 THROWS_T;TIMES_T;TO_T;TRANSIENT_T;TRANSITIVE_T;TRY_T;URS_T;USES_T;VAR_T;VOID_T;
 VOLATILE_T;WHILE_T;WHITESPACE_T;WITH_T;XOR_T;YIELD_T;

] ;; 

let all_codes_and_token_types = Stdl_image.image (
  fun tok -> (code_for_token_type_in_production_names tok,tok)
) all_token_types ;;

let token_type_from_code_in_production_names code =
   List.assoc code all_codes_and_token_types ;;



let rec parse_codeseq (treated,original_str,str,n) =
   if n=0 then List.rev treated else 
   let temp = List.filter (fun (code,_tok)->String.starts_with str ~prefix:code) all_codes_and_token_types in 
   if List.length(temp)<>1
   then raise(Parse_code_sequence_exn(original_str,str,Stdl_image.image fst temp))  
   else
   let (code,tok) = List.hd temp in 
   let (str2,n2) = (Stdl_cull_string.two_sided_cutting (code,"") str,n-String.length(code)) in 
   let (str3,n3) = (
      if String.starts_with str2 ~prefix:"_"
      then (Stdl_cull_string.two_sided_cutting ("_","") str2,n2-1)
      else (str2,n2)
      ) in
   parse_codeseq (tok::treated,original_str,str3,n3) ;;  

let token_type_sequence_from_codes_in_production_names codeseq = 
    if String.starts_with codeseq ~prefix:"Molecular"
   then let core = Stdl_cull_string.two_sided_cutting ("Molecular","") codeseq in 
        parse_codeseq ([],core,core,String.length core) 
   else [token_type_from_code_in_production_names codeseq] ;;

end ;;

let all_token_types = Private.all_token_types ;;


let code_for_tokentype_in_production_names = Private.code_for_token_type_in_production_names ;; 
   
let code_for_tokentype_sequence_in_production_names seq =
   let prefix = (
    if List.length(seq)>1
    then "Molecular"
    else ""  
   ) in 
   prefix^(String.concat "_" (Stdl_image.image code_for_tokentype_in_production_names seq)) ;;

let display_optional nm = "\u{3010}"^nm^"\u{3011}"  ;;
let display_star nm = nm ^ "\u{2605}" ;;

let extract_nonpassive_token_types postoks = 
    List.filter_map Private.extract_nonpassive_token_type_opt postoks ;;

let get_token_type = Private.get_token_type ;;


let has_variable_content = Private.has_variable_content ;;

let ocaml_name_for_token_type = Private.ocaml_name_for_token_type ;;

let order_on_token_types = Stdl_total_ordering.from_list all_token_types ;;
let passive_token_types = Private.passive_token_types ;;

let pretty_print_list_of_token_types toktypes =
   let temp = Stdl_image.image Private.summary_of_token_type toktypes in 
   let text = String.concat " " temp in 
   let n = String.length text in 
   if n<=300
   then text
   else (Stdl_cull_string.beginning 300 text)^"(...)" ;;

let summary_of_token_type = Private.summary_of_token_type ;;

let token_to_string = Private.token_to_string ;;
let token_type_from_ocaml_name name = 
  List.find (
   fun tok_type -> ocaml_name_for_token_type tok_type = name
  ) all_token_types ;;

let token_type_sequence_from_codes_in_production_names = Private.token_type_sequence_from_codes_in_production_names ;;

let token_type_sequence_from_codes_in_production_names_opt codeseq = 
   try Some (Private.token_type_sequence_from_codes_in_production_names codeseq) with 
   _ -> None ;;




 

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






module Stdl_functor_for_sets=struct

(*
 
#use"lib/Ordered_Lists/functor_for_sets.ml";;

Here all the possible dependencies are defined. Each particular
instance defines only the value it needs.

*)


type ('a,'b) parameter = (('a Stdl_ordered.ordered_list) -> 'b) * ('b -> ('a Stdl_ordered.ordered_list)) * ('a Stdl_total_ordering_t.t);;


let does_not_intersect ((_co,deco,cmpr):('a,'b) parameter) 
     ox oy= Stdl_ordered.does_not_intersect cmpr (deco ox) (deco oy);;
    
let empty_set ((co,_deco,cmpr):('a,'b) parameter) = co (Stdl_ordered.safe_set cmpr []);;

let fold_merge ((co,deco,cmpr):('a,'b) parameter) 
     l=co (Stdl_ordered.fold_merge cmpr (Stdl_image.image deco l));;

let fold_intersect ((co,deco,cmpr):('a,'b) parameter) 
     l=co (Stdl_ordered.fold_intersect cmpr (Stdl_image.image deco l));;
    
let forget_order ((_co,deco,_cmpr):('a,'b) parameter) =( fun ox->Stdl_ordered.to_list(deco ox));;

let hd ((_co,deco,_cmpr):('a,'b) parameter) ox= List.hd(Stdl_ordered.to_list(deco ox));;

let image ((_co,deco,_cmpr):('a,'b) parameter) f ox= Stdl_image.image f (Stdl_ordered.to_list(deco ox));;

let insert ((co,deco,cmpr):('a,'b) parameter) 
     x oy= co(Stdl_ordered.insert cmpr x (deco oy));;

let intersect ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= co(Stdl_ordered.intersect cmpr (deco ox) (deco oy));;

let intersects ((_co,deco,cmpr):('a,'b) parameter) 
     ox oy= Stdl_ordered.intersects cmpr (deco ox) (deco oy);;

let is_included_in ((_co,deco,cmpr):('a,'b) parameter) 
     ox oy= Stdl_ordered.is_included_in cmpr (deco ox) (deco oy);;

let length ((_co,deco,_cmpr):('a,'b) parameter) ox= List.length(Stdl_ordered.to_list(deco ox));;

let max ((_co,deco,_cmpr):('a,'b) parameter) ox= List.hd(List.rev (Stdl_ordered.to_list(deco ox)));;

let mem ((_co,deco,cmpr):('a,'b) parameter) 
     x oy= Stdl_ordered.mem cmpr x (deco oy);;

let merge ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= co(Stdl_ordered.merge cmpr (deco ox) (deco oy));;

let min ((_co,deco,_cmpr):('a,'b) parameter) ox= List.hd(Stdl_ordered.to_list(deco ox));;

let nmem ((_co,deco,cmpr):('a,'b) parameter) 
     x oy= not(Stdl_ordered.mem cmpr x (deco oy));;

let outsert ((co,deco,cmpr):('a,'b) parameter) 
     x oy= co(Stdl_ordered.outsert cmpr x (deco oy));;

let safe_set ((co,_deco,cmpr):('a,'b) parameter) 
     l= co(Stdl_ordered.safe_set cmpr l);;

let select_minimal_elements_for_inclusion 
  ((co,deco,cmpr):('a,'b) parameter) ll  
   = 
     let old_form = Stdl_image.image deco ll in
     let new_form = Stdl_ordered.select_minimal_elements_for_inclusion cmpr old_form in 
     Stdl_image.image co new_form;;

let setminus ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= co(Stdl_ordered.setminus cmpr (deco ox) (deco oy));;

let singleton ((co,_deco,cmpr):('a,'b) parameter)  x = co(Stdl_ordered.safe_set cmpr[x]);;

let size_of_intersection ((_co,deco,cmpr):('a,'b) parameter) 
     ox oy= List.length(Stdl_ordered.to_list(Stdl_ordered.intersect cmpr (deco ox) (deco oy)));;

let sort ((co,_deco,cmpr):('a,'b) parameter) 
     l= co(Stdl_ordered.sort cmpr l);;

let tl ((co,deco,_cmpr):('a,'b) parameter) ox= co(Stdl_ordered.tl(deco ox));;

let unsafe_set ((co,_deco,_cmpr):('a,'b) parameter) 
     l= co l;;



end;;






module Stdl_set_of_polys_t=struct

(* 

#use"lib/Ordered_Lists/set_of_polys_t.ml";;

*)

type 'a t=S of 'a list;;


end;;






module Stdl_set_of_polys=struct

(* 

#use"lib/Ordered_Lists/set_of_polys.ml";;

*)

let tr = 
  let order = Stdl_total_ordering.standard in 
  ((fun x->Stdl_set_of_polys_t.S(Stdl_ordered.to_list x)),(fun (Stdl_set_of_polys_t.S(x))->Stdl_ordered.safe_set order x),order);;


let does_not_intersect x y= Stdl_functor_for_sets.does_not_intersect tr x y;;
let empty_set = Stdl_functor_for_sets.empty_set tr;;
let fold_merge l= Stdl_functor_for_sets.fold_merge tr l;;
let forget_order x= Stdl_functor_for_sets.forget_order tr x;;
let hd x = Stdl_functor_for_sets.hd tr x;;
let image f x= Stdl_functor_for_sets.image tr f x;;
let insert a x= Stdl_functor_for_sets.insert tr a x;;
let is_included_in x y= Stdl_functor_for_sets.is_included_in tr x y;;
let length x= Stdl_functor_for_sets.length tr x;;
let mem a x= Stdl_functor_for_sets.mem tr a x;;
let merge l= Stdl_functor_for_sets.merge tr l;;
let nmem a x= Stdl_functor_for_sets.nmem tr a x;;
let outsert a x= Stdl_functor_for_sets.outsert tr a x;;
let safe_set l= Stdl_functor_for_sets.safe_set tr l;;
let setminus x y= Stdl_functor_for_sets.setminus tr x y;;
let singleton a= Stdl_functor_for_sets.singleton tr a;;
let sort l= Stdl_functor_for_sets.sort tr l;;
let unsafe_set l= Stdl_functor_for_sets.unsafe_set tr l;;




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






module Stdl_two_winged_bird_on_plank=struct

(*

#use"lib/two_winged_bird_on_plank.ml";;

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



end;;






module Stdl_lower_acyclic_subposet=struct

(*

#use"lib/lower_acyclic_subposet.ml";;

Start with an arbitrary relation R on a finite set L. Consider its transitive closure R'; then
for (x,y) in R, we say that x is a coatom of y, and we denote by coat(y) the set of all coatoms of y.

There are three kind of elements in L :
- The cylic elements, i.e. those elements x such that there are x2,x3,...,xr with xRx2Rx3Rx4...xrRx.
- The "cyclic accomplice" elements, i.e. those elements x not in any cycle but that have cyclic elements below them.
- Finally, the "completely acyclic" elements, i.e. those elements x such that x and the elements below are not cyclic.

The main function here computes the pair (cycles_found_so_far,completely_acyclic_elements) where 
cycles_found_so_far is a (non-canonical,non-exhaustive) set of cycles_found_so_far, and 
completely_acyclic_elements lists the c.a. elements in an order compatible with the initial relation R.
Those c.a. elements are accompanied by the corresponding lower intervals.

*)

module Private = struct

type 'a walker = {
   data_for_completely_acyclic_elements : ('a * (('a list) * ('a Stdl_set_of_polys_t.t))) list ;
   ordered_list_of_completely_acyclic_elements_found_so_far : 'a Stdl_set_of_polys_t.t ;
   cycles_found_so_far : 'a list list ;
   ordered_list_of_cyclic_elements_found_so_far : 'a Stdl_set_of_polys_t.t ;
   between : 'a list ;
   to_be_treated : 'a list ;
   final_answer_opt : ( ('a list list) * ( ('a * 'a list) list) ) option ; 
} ;; 

let extreme_state l opt = {
   data_for_completely_acyclic_elements = [] ;
   ordered_list_of_completely_acyclic_elements_found_so_far = Stdl_set_of_polys.empty_set ;
   cycles_found_so_far = [] ;
   ordered_list_of_cyclic_elements_found_so_far = Stdl_set_of_polys.empty_set ;
   between = [] ;
   to_be_treated = l ;
   final_answer_opt = opt ; 
} ;; 

let final_state w = extreme_state [] (Some(w.cycles_found_so_far,Stdl_list_again.rev_map (fun (z,p)->(z,fst p)) w.data_for_completely_acyclic_elements)) ;;

let initial_state l = extreme_state l None ;; 


let completely_acyclic_case w current_head coatoms unordered_coatoms = 
  let several_coatoms=coatoms::(Stdl_image.image (fun z->snd(List.assoc z w.data_for_completely_acyclic_elements)) 
                      unordered_coatoms) in
  let lower_interval_in_global_order=Stdl_set_of_polys.fold_merge(several_coatoms) in
  let lower_interval_in_reversed_global_order=List.filter_map (
           fun (b,_)->if Stdl_set_of_polys.mem b lower_interval_in_global_order
             then Some(b)
             else None
  ) w.data_for_completely_acyclic_elements in
  let lower_interval_in_local_order=List.rev(lower_interval_in_reversed_global_order) in
  let data_for_head=(lower_interval_in_local_order,lower_interval_in_global_order) in
  {
    w with 
    data_for_completely_acyclic_elements = (current_head,data_for_head):: w.data_for_completely_acyclic_elements ;
    ordered_list_of_completely_acyclic_elements_found_so_far = Stdl_set_of_polys.insert current_head w.ordered_list_of_completely_acyclic_elements_found_so_far ;
    between = [] ;
    to_be_treated = List.filter (fun z->z<>current_head) w.to_be_treated ;
  } ;;

let cyclic_or_cyclic_accomplice_case w current_head = 
  {
    w with 
    ordered_list_of_cyclic_elements_found_so_far = Stdl_set_of_polys.insert current_head w.ordered_list_of_cyclic_elements_found_so_far ;
    between = [] ;
    to_be_treated = List.filter (fun z->z<>current_head) w.to_be_treated ;
  } ;;

let step coat w=
  if w.final_answer_opt<>None 
  then w 
  else
  if (w.between,w.to_be_treated)=([],[]) 
  then final_state w 
  else
  let current_head=
    	  (if w.between=[] 
    	   then List.hd(w.to_be_treated)
           else List.hd(w.between)
          ) in
    let unordered_coatoms=coat(current_head) in
    let coatoms=Stdl_set_of_polys.safe_set(unordered_coatoms) in
    let unclear_coatoms=Stdl_set_of_polys.setminus coatoms w.ordered_list_of_completely_acyclic_elements_found_so_far in
    if Stdl_set_of_polys.length(unclear_coatoms)=0
    then (* if we get here, all the coatoms are completely acyclic ; so is the head *)
          completely_acyclic_case w current_head coatoms unordered_coatoms
    else
    if (Stdl_set_of_polys.mem current_head unclear_coatoms) || (not(Stdl_set_of_polys.does_not_intersect unclear_coatoms w.ordered_list_of_cyclic_elements_found_so_far))
    then (* if we get here the head is a cycle by itself, or a cyclic accomplice *)
         cyclic_or_cyclic_accomplice_case w current_head
    else 
    (*see if we can close the cycle *)
    match List.find_opt(fun (_rev_left,x,_right)->Stdl_set_of_polys.mem x unclear_coatoms) 
     (List.rev(Stdl_two_winged_bird_on_plank.generic w.between)) with
     None->{
           w with 
          between = (
            if w.between=[]
            then [(Stdl_set_of_polys.hd unclear_coatoms);current_head]
            else (Stdl_set_of_polys.hd unclear_coatoms)::w.between
            ) ;
         }
    |Some(rev_left,x,_right)->
        (* let (before,_,_after)=Three_parts.select_center_element_and_reverse_left (fun x->x=p) w.between in
        let temp2=Image.image fst before in
        let new_cycle=(fst p)::(temp2@[current_head]) in *)

        let new_cycle=x::rev_left in 
        let ordered_cycle=Stdl_set_of_polys.sort new_cycle in
        {
           w with 
          cycles_found_so_far = new_cycle:: w.cycles_found_so_far ;
          ordered_list_of_cyclic_elements_found_so_far = Stdl_set_of_polys.merge ordered_cycle w.ordered_list_of_cyclic_elements_found_so_far ;
          between = [] ;
          to_be_treated = List.filter (fun z->Stdl_set_of_polys.nmem z ordered_cycle) w.to_be_treated ;
        } ;;

let compute coat l=
  let rec tempf=(fun walker->
    if walker.final_answer_opt <>None
    then Option.get walker.final_answer_opt
    else tempf(step coat walker)
    ) in
    tempf(initial_state l);;
    
end ;;

let compute = Private.compute ;;


(*

let sugar i=
   if (i<4)||(i>20) then [] else
   if i=11 then [5] else [i+1];;
    
(reconstruct_linear_poset sugar (Int_range.range 1 30))=(
   ([[5; 6; 7; 8; 9; 10; 11]],
 [(1, []); (2, []); (3, []); (21, []); (20, [21]); (19, [21; 20]);
  (18, [21; 20; 19]); (17, [21; 20; 19; 18]); (16, [21; 20; 19; 18; 17]);
  (15, [21; 20; 19; 18; 17; 16]); (14, [21; 20; 19; 18; 17; 16; 15]);
  (13, [21; 20; 19; 18; 17; 16; 15; 14]);
  (12, [21; 20; 19; 18; 17; 16; 15; 14; 13]); (22, []); (23, []); (24, []);
  (25, []); (26, []); (27, []); (28, []); (29, []); (30, [])])

);;  

let some_edges=
  [
    (1,4);(1,16);(2,6);(2,7);(3,16);(7,11);(7,19);(8,11);(8,15);(9,19);
    (10,1);(10,18);(11,2);(12,16);(13,5);(15,13);(16,17);(17,10);(17,14);(19,20);
    (20,21);(21,9)
  
  ];;

let brown j=image fst (List.filter (fun x->snd(x)=j) some_edges);;

(reconstruct_linear_poset brown (Int_range.range 1 21))=(
   ([[2; 11; 7]; [1; 10; 17; 16]],
 [(3, []); (8, []); (15, [8]); (13, [8; 15]); (5, [8; 15; 13]); (12, [])])

);;  

let some_edges=
  [
    (10, 1); (9, 10); (4, 9); (5, 4); (6, 5); (2, 6); (8, 2); (3, 8); (7, 3);(4, 7)
  ];;

let yellow j=image fst (List.filter (fun x->snd(x)=j) some_edges);;

(reconstruct_linear_poset yellow (Int_range.range 1 10))=(
   [[4; 5; 6; 2; 8; 3; 7]], []
);;  

*)
 


end;;






module Stdl_tools_for_debugging=struct

(*

#use"lib/tools_for_debugging.ml";;

*)

let detect_exception f x = try (fun _->false)(f x) with _->true ;;

let extract_from_fold_left f starter pushers=
   let rec tempf = (fun (count_of_pushers_already_applied,current_starter,current_pushers)->
    match current_pushers with 
    [] -> failwith("There is nothing to debug.")
    |pusher :: others ->
     if detect_exception (f current_starter) pusher
     then (count_of_pushers_already_applied,pusher)
     else tempf(count_of_pushers_already_applied+1,f current_starter pusher,others)
   ) in 
   tempf (0,starter,pushers);;

let extract_from_iteration f v0=
   let rec tempf = (fun (j,v)->
     if detect_exception f v 
     then (j,v)
     else tempf(j+1,f v)
   ) in 
   tempf (0,v0);;

let extract_from_list f l =
   let temp1 = Stdl_int_range.index_everything l in 
   let tempf= (fun (_j,elt)->detect_exception f elt) in 
   List.find tempf temp1 ;; 

end;;






module Stdl_jvag_types=struct

(*

#use"lib/Java_analysis/Jvsp_abstract_grammar/jvag_types.ml";;

*)

     
type form = 
    Optional of string 
   |Molecular of Stdl_jvsp_types.token_type list
   |Concat of string list 
   |Disjunction of string list 
   |Star of string 
   |Synonym of string
  ;;

type grammar = AL of  ((string * form) list) ;; 

type location_in_disjunction = I of int | N of string ;;


type local_modification = 
   Lm_remove_left_recursive_lines_in_disjunction
   (*
     remove_left_recursive_lines_in_disjunction gf  make a global transformation on the value associated with
     original_name key in the grammar (this value must be a disjunction, some of which are left recursions,
     i.e. of the form original_name -> original_name Ak for 1<=k<=r). 
     original_name -> original_name A. The left recursions are removed in a standard way, by putting
     original_name = original_name' Starred(A1|A2|...|Ak).
   *)
  |Lm_expand_lines_in_disjunction of location_in_disjunction list
      (* expand_lines_in_disjunction gf (lid,index_in_concat) expands all the lines defined by lids, replacing
      each with one or several new lines. A replaced line must be a Synonym (in which case  it is replaced
      by a single line containing just the synonym) or a Disjunction(in which case it is expanded in the obvious way). *)
  |Lm_expand_point_in_line of location_in_disjunction * int
      (* expand_point_in_line gf (lid,index_in_concat) expands the point at index index_in_concat
      of the line defined by lid. If this point is a Dijsunction, this line will be replaced by
      more lines. If this point is a Concat, a Synonym or a Molecule, it will be replaced by a longer line. *)  
  |Lm_reunite_in_concatenation of location_in_disjunction * (int * int)  
      (*
      implode_molecule gf (lid,(range_start,range_end)) replaces line defined by lid
      with a shorter line, by mergeing all the elements all the elements in the range 
      described by (range_start,range_end) into a Molecular or a Concat. *)  
 |Lm_reunite_in_disjunction of location_in_disjunction * location_in_disjunction
   (*
      reunite_in_disjunction gf (lid_start,lid_end) collapses the lines in the range described
     by (lid_start,lid_end) into a single line, by factoring left and right and mergeing all
     the center elements into an Optional, a Star or a Disjunction.
   *)
  |Lm_replace_self_with_older_synonym
   (*
     one of the simplest operations in this type, only acts on Synonym-s.
   *) 
  |Lm_permute_lines of (int list)
   (*
    if the original ordering of the lines is [l(1),l(2),...l,l(n)] and
    the argument of this constructor is [i(1),...,i(n)], the new ordering is
    [l(i(1)),...,l(i(n))]
   *)
  |Lm_expand_heads_at_lines of (int list) 
  (*
    The argument list is a list of line indices.
  *)
  |Lm_expand_all_disjunctions
  (*
    Expands all the disjunctions and sub-disjunctions recursively. In the uses we
    make of it, the fully expanded results will be a list of concats.
  *)
;;


type modification = 
   Set_production of string * form
  |Create_production of string * form  
  |Rename of string * string 
  |Remove_productions of string list
  |Collapse_synonym_globally of string 
  |Local of string * (local_modification list) ;;

type nonrecursive_grammar = {
   sons_and_fathers : (string * string) list ;
   productions : (string * (form * string list)) list;
} ;;

type link =
    Optional_L 
   |Concat_L
   |Disjunction_L 
   |Star_L
   |Synonym_L
  ;;

type rake_config = {
   main_name : string ;
   irreducibles : string list ;
   original_grammar : grammar
} ;;

type rake = {
  config : rake_config ;
  current_grammar : grammar ;
  terminal_forms : (string list * string list) list ;
  expandables : (string list * string list) list ;
  inner_left_recursions : (string list * string list) list ;
  outer_left_recursions : (string list * string list) list ;
} ;;

end;;






module Stdl_jvag_form=struct

(*

#use"lib/Java_analysis/Jvsp_abstract_grammar/jvag_form.ml";;

*)

open Stdl_jvag_types ;;

exception Get_exn of string ;;
exception Circularity of string * (string list) ;; 
exception Concat_content_exn of form ;;
exception Disjunction_content_exn of form ;;
exception Synonym_content_exn of form ;;

module Private = struct 

  let str_order = Stdl_total_ordering.lex_for_strings ;;
  let str_sort = Stdl_ordered.sort str_order ;;


let to_string = function 
   Optional(nm) -> Stdl_jvsp_util.display_optional nm   
  |Molecular(l) -> (String.concat " " (Stdl_image.image Stdl_jvsp_util.summary_of_token_type l))   
  |Concat(l) ->  String.concat " " l
  |Disjunction(l) ->
     "\n"^(String.concat "\n" (Stdl_image.image (fun elt->
      "|"^elt) l))^"\n"
  |(Star nm) -> Stdl_jvsp_util.display_star nm 
  |Synonym(nm) -> nm;;

let unordered_coatoms form = match form with
    Concat l ->  l
   |Molecular _ -> []
   |Disjunction l -> l
   |Star nm -> [nm]
   |Optional nm -> [nm]
   |Synonym nm -> [nm] ;;


let molecular_content_opt form = match form with 
   (Molecular l) -> Some l    
   |Concat _
   |Disjunction _
   |Star _
   |Optional _ 
   |Synonym _ -> None;;  

end ;;

let coatoms form = Private.str_sort (Private.unordered_coatoms form) ;;

let concat_content form = match form with 
   (Concat l) -> l    
   |Disjunction _
   |Molecular  _
   |Star _
   |Optional _ 
   |Synonym _ -> raise(Concat_content_exn(form));;   
 
let concat_content_opt form = match form with 
   (Concat l) -> Some l    
   |Disjunction _
   |Molecular  _
   |Star _
   |Optional _ 
   |Synonym _ -> None;;   


let disjunction_content form = match form with 
   (Disjunction l) -> l    
   |Concat _
   |Molecular  _
   |Star _
   |Optional _ 
   |Synonym _ -> raise(Disjunction_content_exn(form));;   

let disjunction_content_opt form = match form with 
   (Disjunction l) -> Some l    
   |Concat _
   |Molecular  _
   |Star _
   |Optional _ 
   |Synonym _ -> None;;  



let is_a_token_sequence form = ((Private.molecular_content_opt form)<>None) ;;

let is_directly_contained_in nm form = match form with
    Concat l -> List.mem nm l
   |Molecular _ -> false
   |Disjunction l -> List.mem nm l
   |Star nm2 -> nm2 = nm
   |Optional nm2 -> nm2 = nm
   |Synonym nm2 -> nm2 = nm ;;

let is_contained_in nm form =
   List.exists (fun nm2->is_directly_contained_in nm2 form) [nm;"Optional"^nm;"Starred"^nm] ;;  

let molecular_content_opt = Private.molecular_content_opt ;;   

let number_of_items form = match form with 
   (Disjunction l) -> (List.length l,"Disjunction")    
   |Concat l -> (List.length l,"Concat")
   |Molecular  l-> (List.length l,"Molecule")
   |Star _
   |Optional _ 
   |Synonym _ -> (0,"");;   


let ocaml_name = function 
   Optional(nm) -> "Optional(\""^nm^"\")"
  |Molecular(l) -> "Molecular(["^(String.concat ";" (Stdl_image.image Stdl_jvsp_util.ocaml_name_for_token_type l))^"])"
  |Concat(l) -> "Concat(["^(String.concat ";" (Stdl_image.image (fun s->"\""^s^"\"") l))^"])"
  |Disjunction(l) -> "Disjunction(["^(String.concat ";" (Stdl_image.image (fun s->"\""^s^"\"") l))^"])"
  |Star(nm) -> "Star(\""^nm^"\")"
  |Synonym(nm) -> "Synonym(\""^nm^"\")";;  

let order = (
   (fun form1 form2 ->Stdl_total_ordering.standard form1 form2): 
     form Stdl_total_ordering_t.t ) ;; 

(* This is a registered printer : print_out *)
let print_out (fmt:Format.formatter) form=
   Format.fprintf fmt "@[%s@]" (Stdl_strung.with_size_limit ~size_limit:250 (Private.to_string form));;

let starred_content_opt form = match form with 
   |Star nm -> Some nm
   |Molecular _    
   |Concat _
   |Disjunction _
   |Optional _ 
   |Synonym _ -> None;;  

let synonym_content form = match form with 
   (Synonym nm) -> nm   
   |Disjunction _
   |Concat _
   |Molecular  _
   |Star _
   |Optional _ -> raise(Synonym_content_exn(form));;

let synonym_content_opt form = match form with 
   (Synonym nm) -> Some nm   
   |Disjunction _
   |Concat _
   |Molecular  _
   |Star _
   |Optional _ -> None;;


let to_string = Private.to_string ;; 

let uniform_composition link l = match link with 
    Optional_L -> Optional(List.hd l)
   |Concat_L -> Concat(l)
   |Disjunction_L -> Disjunction(l)
   |Star_L -> Star(List.hd l)
   |Synonym_L -> Synonym(List.hd l);;

let uniform_decomposition_opt form = match form with
   |Molecular _ -> None    
   |Concat l ->  Some(Concat_L,l)
   |Disjunction l -> Some(Disjunction_L,l)
   |Star nm -> Some(Star_L,[nm])
   |Optional nm -> Some(Optional_L,[nm])
   |Synonym nm -> Some(Synonym_L,[nm]) ;;

let unordered_coatoms = Private.unordered_coatoms ;;   

end;;






module Stdl_jvag_grammar=struct

(*

#use"lib/Java_analysis/Jvsp_abstract_grammar/jvag_grammar.ml";;

*)

open Stdl_jvag_types ;;

exception Get_exn of string ;;
exception Circularity of string * (string list) ;; 
exception Name_already_in_use of string ;;

module Private = struct 

  let str_order = Stdl_total_ordering.lex_for_strings ;;
  let str_fold_merge = Stdl_ordered.fold_merge str_order ;;
  let str_insert= Stdl_ordered.insert str_order ;;
  let str_intersect= Stdl_ordered.intersect str_order ;;
  let str_mem= Stdl_ordered.mem str_order ;;
  let str_merge= Stdl_ordered.merge str_order ;;
  let str_safe_set = Stdl_ordered.safe_set str_order ;;
  let str_setminus = Stdl_ordered.setminus str_order ;;
  let str_singleton = Stdl_ordered.singleton  ;;
  let str_sort = Stdl_ordered.sort str_order ;;


let ocaml_name_of_sf (name,frm) =
    "(\""^name^"\","^(Stdl_jvag_form.ocaml_name frm)^")";;

let ocaml_name (AL l)=
  let lines = Stdl_image.image (fun sf->(String.make 3 ' ')^(ocaml_name_of_sf sf)^";") l in 
"AL ([\n\n"^
(String.concat "\n" lines)^
"\n\n])" ;; 

let get_opt (AL l) name = 
  match Stdl_jvsp_util.token_type_sequence_from_codes_in_production_names_opt name with 
  Some answer -> Some (Molecular answer) 
  | None -> 
    if String.starts_with name ~prefix:"Optional"
    then Some(Optional(Stdl_cull_string.two_sided_cutting ("Optional","") name))  
    else
     if String.starts_with name ~prefix:"Starred"
    then Some(Star(Stdl_cull_string.two_sided_cutting ("Starred","") name))  
    else  
    List.assoc_opt name l ;;

let automatic_name_for_molecular_opt = function 
   (Molecular l) -> Some(Stdl_jvsp_util.code_for_tokentype_sequence_in_production_names l)
  |Disjunction(_)
  |Optional(_) 
  |(Concat _) 
  |(Star _) 
  |Synonym(_) -> None;;


let name_for_form_opt (AL l) form = match form with 
  (Molecular l) -> Some(Stdl_jvsp_util.code_for_tokentype_sequence_in_production_names l)
  |(Optional nm) -> Some("Optional"^nm) 
  |(Star nm) -> Some("Starred"^nm)
  |Disjunction(_) 
  |Concat _ 
  |Synonym _ -> Stdl_list_again.assoc_right_opt  form l ;;

let get gram name = match get_opt gram name  with 
  None -> raise(Get_exn(name))
 |Some answer -> answer ;;

let concat_element_to_enhanced_string name form = match form with
   Disjunction(_) -> name
  |Optional(_) 
  |Molecular(_)    
  |(Concat _) 
  |(Star _) 
  |Synonym(_) -> Stdl_jvag_form.to_string form;;

let concat_to_enhanced_string gram l = 
    String.concat " " (Stdl_image.image (fun nm->concat_element_to_enhanced_string nm (get gram nm)) l) ;;

let adhoc_disjunction_to_string l = 
    "\n"^(String.concat "\n" (Stdl_image.image (fun elt->
      "    |"^elt) l)) ;; 

let form_to_enhanced1_string gram form = match form with
   (Concat l) -> concat_to_enhanced_string gram l
   |Disjunction(l) -> adhoc_disjunction_to_string l
  |Optional(_)   
  |Molecular(_)    
  |(Star _) 
  |Synonym(_) -> Stdl_jvag_form.to_string form;;

let form_to_enhanced2_string gram form= match form with
   Disjunction(l) ->
     "\n"^(String.concat "\n" (Stdl_image.image (fun elt->
      let expanded_elt = get gram elt in 
      elt^" : "^(form_to_enhanced1_string gram expanded_elt)) l))^"\n"  
  |Concat(l) ->  concat_to_enhanced_string gram l     
  |Optional(_) 
  |Molecular(_)    
  |(Star _) 
  |Synonym(_) -> Stdl_jvag_form.to_string form;;

let message_about_number_of_items form =
  let (n,name) = Stdl_jvag_form.number_of_items form in 
  if n=0 then "" else 
  (string_of_int n)^" items in "^name^"\n\n";;  

let form_to_enhanced3_string gram name form= 
  "\n\n "^name^" = "^(Stdl_jvag_form.ocaml_name form)^"\n\n"^
  (message_about_number_of_items form)^
  (form_to_enhanced2_string gram form);;


let get_and_display gram name =
   let form = get gram name in 
   let msg = "\n\n\n" ^ (form_to_enhanced3_string gram name form) ^ "\n\n\n" in 
  let _ =(print_string msg;flush stdout) in 
   form ;;

let forced_concat gram name =
  match get gram name with 
  Concat(l) -> l
  |Disjunction(_) 
  |Synonym(_) 
  |Molecular  _
  |Star _
  |Optional _ -> [name];;     

let forced_disjunction_of_concats gram name = 
  match get gram name with 
  Disjunction(dis) -> Stdl_image.image (forced_concat gram) dis 
  |Concat(conc) -> [conc]
  |Molecular  _ 
  |Synonym(_) 
  |Star _
  |Optional _ -> [[name]];;  



let ocaml_name_of_lid = function 
  I(i)->"I "^(string_of_int i)
  |N(s)->"N \""^s^"\"" ;;   
   
let ocaml_name_of_local_modification lmod=
  let soi =string_of_int in 
  match lmod with 
   Lm_remove_left_recursive_lines_in_disjunction ->  
   "Lm_remove_left_recursive_lines_in_disjunction"   
 |(Lm_expand_lines_in_disjunction(lids)) ->  
  "Lm_expand_lines_in_disjunction(["^(String.concat ";" (Stdl_image.image ocaml_name_of_lid lids))^"])"
    |(Lm_expand_point_in_line(lid,index_in_concat)) ->  
  "Lm_expand_point_in_line("^(ocaml_name_of_lid lid)^","^(soi index_in_concat)^")"
 |(Lm_reunite_in_concatenation(lid,(range_start,range_end))) ->  
  "Lm_reunite_in_concatenation("^(ocaml_name_of_lid lid)^",("^(soi range_start)^","^(soi range_end)^"))"    
 |(Lm_reunite_in_disjunction(lid_start,lid_end)) ->
  "Lm_reunite_in_disjunction("^(ocaml_name_of_lid lid_start)^","^(ocaml_name_of_lid lid_end)^")"   
 |Lm_replace_self_with_older_synonym ->  
   "Lm_replace_self_with_older_synonym" 
 |(Lm_permute_lines(ordering)) ->  
  "Lm_permute_lines(["^(String.concat ";" (Stdl_image.image string_of_int ordering))^"])"
 |(Lm_expand_heads_at_lines(line_indices)) ->  
  "Lm_expand_heads_at_lines(["^(String.concat ";" (Stdl_image.image string_of_int line_indices))^"])"  
 |Lm_expand_all_disjunctions ->  
   "Lm_expand_all_disjunctions"     
;;

let ocaml_name_of_modification = function 
    Set_production(name,form) -> "Set_production(\""^name^"\","^(Stdl_jvag_form.ocaml_name form)^")" 
  |Create_production(name,form) -> "Set_production(\""^name^"\","^(Stdl_jvag_form.ocaml_name form)^")"   
  |Rename(old_name,new_name) -> "Rename(\""^old_name^"\",\""^new_name^"\")"     
  |Remove_productions(l) -> "Remove_productions(["^(String.concat ";" (Stdl_image.image (fun s->"\""^s^"\"") l))^"])"   
  |Collapse_synonym_globally(newer_synonym) -> "Collapse_synonym_globally(\""^newer_synonym^"\")" 
  |Local(name,mods)->  "Local(\""^name^"\",["^(String.concat ";" 
     (Stdl_image.image ocaml_name_of_local_modification mods))^"])"
;;

let ocaml_name_of_local_modification_list l = 
  "[\n"^
  (String.concat "\n" (Stdl_image.image (fun modif->"   "^(ocaml_name_of_local_modification modif)^";") l))^
  "\n]" ;;
     

let ocaml_name_of_modification_list l = 
  "[\n"^
  (String.concat "\n" (Stdl_image.image (fun modif->"   "^(ocaml_name_of_modification modif)^";") l))^
  "\n]" ;;
     
let display_local_modification_list l = print_string("\n\n\n"^(ocaml_name_of_local_modification_list l)^"\n\n\n");;  

let display_modification_list l = print_string("\n\n\n"^(ocaml_name_of_modification_list l)^"\n\n\n");;  

let order_on_pairs = Stdl_total_ordering.product str_order Stdl_jvag_form.order ;;


let is_contained_in_pair nm (_name,form) = Stdl_jvag_form.is_contained_in nm form;;

let containing nm (AL l) = List.filter(is_contained_in_pair nm) l;;


let rec helper_for_lower_interval_below (gram,treated,to_be_treated) = 
  match Stdl_ordered.to_list to_be_treated with 
  [] -> treated 
  |name :: other_names ->
     let coats = Stdl_jvag_form.coatoms (get gram name) in 
     let new_coats = str_setminus coats treated in 
     helper_for_lower_interval_below (gram,str_insert name treated,str_merge new_coats (str_safe_set other_names)) 
  ;;

let lower_interval_below gram name = helper_for_lower_interval_below (gram,str_singleton name,str_singleton name) ;;

let just_below gram name = (Stdl_jvag_form.unordered_coatoms (get gram name)) ;;

exception Bad_step_in_ladder_exn of string ;;
exception Strange_step_in_ladder_exn of string ;;
exception Bad_link_in_ladder_exn of string * string * (string list) ;;

let test_disjunction_ladder_link gram (x,y) =
  match get_opt gram x with 
  None -> raise(Bad_step_in_ladder_exn(x))
  |Some form -> 
       match Stdl_jvag_form.disjunction_content_opt form with 
       None -> raise(Strange_step_in_ladder_exn(x))
       |Some l ->
       if not(List.mem y l) 
       then raise(Bad_link_in_ladder_exn(x,y,l))
       else () ;;  


let check_disjunction_ladder gram ladder = 
   let temp1 = Stdl_list_again.universal_delta_list ladder in   
   List.iter (test_disjunction_ladder_link gram) temp1 ;;

let differences (AL l1) (AL l2) =
  let names = str_merge (str_safe_set(Stdl_image.image fst l1)) (str_safe_set(Stdl_image.image fst l2)) in 
  let data = Stdl_ordered.image (
    fun name ->(name,(List.assoc_opt name l1,List.assoc_opt name l2))
  ) names in 
  List.filter (fun (_name,(opt1,opt2))->opt1<>opt2) data ;;

let replace_pair_or_add_if_absent pair (AL l) = 
  let (name,_form) = pair in 
  let new_l = (
    match List.assoc_opt name l with 
    None -> pair :: l 
    |Some _ -> Stdl_image.image (fun pair2->
      if (fst pair2)=name then pair else pair2 ) l
  )  in 
  AL(Stdl_ordered.to_list(Stdl_ordered.sort order_on_pairs new_l));; 

exception Name_is_not_new_exn of string ;;

let create_new_pair pair (AL l) = 
  let (name,_form) = pair in 
  let new_l = (
    match List.assoc_opt name l with 
    None -> pair :: l 
    |Some _ -> raise(Name_is_not_new_exn(name))
  )  in 
  AL(Stdl_ordered.to_list(Stdl_ordered.sort order_on_pairs new_l));; 
 

module Dwarf_count = struct 

let i0 = int_of_char '0'
and i9 = int_of_char '9' ;;

let dwarf_marker = "Dwarf" ;;
let dwarf_marker_length = String.length dwarf_marker ;;

let dwarf_number_in_name name = 
  if not(String.starts_with name ~prefix:dwarf_marker)
  then 0 
  else 
  let n = String.length name in   
  let last_digit_position =(
  match Stdl_string_find_char.from_inclusive_opt (fun c->
    let i=int_of_char c in (i<i0)||(i>i9)
  ) name (dwarf_marker_length+1) with 
  None -> n 
  |Some idx ->idx-1
  )  in
  int_of_string(Stdl_cull_string.interval name (dwarf_marker_length+1) last_digit_position) ;;   


(* dwarf_number_in_name "Dwarf273abc" ;; *)

let recompute_dwarf_count_from_scratch (AL l)=
   snd(Stdl_max.maximize_it (fun (name,_)->dwarf_number_in_name name) l) ;;

let dwarfy_name ~suffix dwarf_number= function 
    Molecular token_types  -> Stdl_jvsp_util.code_for_tokentype_sequence_in_production_names token_types
   |Star nm -> "Starred"^ nm
   |Optional nm -> "Optional"^ nm
   |Concat _ 
   |Disjunction _ 
   |Synonym _  -> dwarf_marker^(string_of_int dwarf_number)^"_"^suffix ;; 

end ;;  



module Mergeable_token_sequences = struct

let merge_tl_sequences_in_concat_perhaps gram l = 
  let temp1 = Stdl_image.image (fun nm->(nm,get gram nm)) l in 
  let temp2 = Stdl_list_again.connected_fibers (fun (_,form)->Stdl_jvag_form.is_a_token_sequence form) temp1 in 
  let watcher = ref(false) in 
  let temp3 = Stdl_image.image (
    fun (_range,segment,is_a_tl_segment) ->
      if is_a_tl_segment && (List.length(segment)>1)
      then let _= (watcher:=true) in
           let seq = List.flatten(Stdl_image.image (fun (_,z)->Option.get(Stdl_jvag_form.molecular_content_opt z)) segment) in
           let seq_name = Stdl_jvsp_util.code_for_tokentype_sequence_in_production_names seq in   
           [seq_name]
      else Stdl_image.image fst segment
  ) temp2 in 
  (List.flatten temp3,!watcher) ;; 


let merge_tl_sequences_in_form_perhaps gram form =
  match form with
    Concat l ->
          let (new_l,action_present)=merge_tl_sequences_in_concat_perhaps gram l in 
            (Concat new_l,action_present)
   |Molecular _   
   |Disjunction _ 
   |Star _
   |Optional _
   |Synonym _ -> (form,false) ;;

let merge_tl_sequences_in_pair_perhaps gram pair =
    let (name,form) = pair in 
    let (new_form,action_present)=merge_tl_sequences_in_form_perhaps gram form in 
    ((name,new_form), action_present) ;; 

let data_about_mergeing_tl_sequences_in_grammar gram =
    let (AL l) = gram in 
    let main = Stdl_image.image  (merge_tl_sequences_in_pair_perhaps gram) l in 
    let new_gram = AL(Stdl_image.image fst main) 
    and needed_modifications = List.filter_map (fun 
      ((name,form),action_present) -> 
        if action_present 
        then Some(Set_production(name,form))
        else None
      ) main in 
    (new_gram,needed_modifications );; 




end ;;   



module Redundant_concats = struct

(* old version ends here *)

let remove_redundant_concats_in_concat_perhaps gram names = 
  let temp1 = Stdl_image.image (fun name->(name,get gram name)) names in 
  let watcher = ref(false) in 
  let new_disjunction = List.flatten(Stdl_image.image (
    fun  (name,form) ->
      match Stdl_jvag_form.concat_content_opt form with 
      None -> [name]
      |Some l -> 
         let _= (watcher:=true) in
          l
  ) temp1) in 
  (new_disjunction,!watcher) ;; 


let remove_redundant_concats_in_form_perhaps gram form =
  match form with
    Concat l ->
          let (new_l,action_present)=remove_redundant_concats_in_concat_perhaps gram l in 
            (Concat new_l,action_present)
   |Molecular _   
   |Disjunction _ 
   |Star _
   |Optional _
   |Synonym _ -> (form,false) ;;

let remove_redundant_concats_in_pair_perhaps gram pair =
    let (name,form) = pair in 
    let (new_form,action_present)=remove_redundant_concats_in_form_perhaps gram form in 
    ((name,new_form), action_present) ;; 

let data_about_removing_redundant_concats_in_grammar gram =
    let (AL l) = gram in 
    let main = Stdl_image.image  (remove_redundant_concats_in_pair_perhaps gram) l in 
    let new_gram = AL(Stdl_image.image fst main) 
    and needed_modifications = List.filter_map (fun 
      ((name,form),action_present) -> 
        if action_present 
        then Some(Set_production(name,form))
        else None
      ) main in 
    (new_gram,needed_modifications );; 


end ;;



module Unused_names = struct 


  
let immediately_unused_names_in_grammar gram =
  let (AL l)= gram in
  let all_names = str_safe_set(Stdl_image.image fst l) in 
  Stdl_ordered.filter (fun name->List.for_all (fun (_,form)->not(Stdl_jvag_form.is_contained_in name form)) l) all_names ;;
   ;;  


let data_about_removing_immediately_unused_names_in_grammar gram ~exceptions=
  let unused_names = immediately_unused_names_in_grammar gram in 
  let to_be_removed =  str_setminus unused_names exceptions in 
  let (AL l)= gram in 
  (AL(List.filter (fun (name,_)->
   not(Stdl_ordered.mem  Stdl_total_ordering.lex_for_strings name to_be_removed)) l),to_be_removed);;


end ;;   

module Sanitize = struct 

let sanitize_superficially gram =
  let (gram2,unused_names) = Unused_names.data_about_removing_immediately_unused_names_in_grammar 
                                    gram ~exceptions:(Stdl_ordered.singleton "OrdinaryCompilationUnit") in 
  let (gram3,mergeable_tokseqs) = Mergeable_token_sequences.data_about_mergeing_tl_sequences_in_grammar gram2 in 
  let (gram4,redundant_concats) = Redundant_concats.data_about_removing_redundant_concats_in_grammar gram3 in 
  ((unused_names,mergeable_tokseqs,redundant_concats),gram4) ;;

let rec helper_for_sanitization (older_data,gram) =
   let (new_data,new_gram) = sanitize_superficially gram in 
   let (nd1,nd2,nd3) = new_data in 
   if (Stdl_ordered.to_list nd1,nd2,nd3)=([],[],[]) 
   then (older_data,gram) 
   else helper_for_sanitization (new_data::older_data,new_gram) ;;

let sanitize_as_many_times_as_needed gram = helper_for_sanitization ([],gram) ;; 


end ;;  

module WriteParser = struct 




exception Find_acyclic_ordering_exn of string * (string list list) ;;

let find_acyclic_ordering unordered_l =
  let ordered_l = Stdl_ordered.sort order_on_pairs unordered_l in  
  let l = Stdl_ordered.to_list ordered_l in 
  let defined_names = str_sort(Stdl_image.image fst l) 
  and referenced_names = str_fold_merge(Stdl_image.image (fun (_,form)->Stdl_jvag_form.coatoms form) l) in 
  let undefined_names = str_setminus referenced_names defined_names in 
  let relative_coatoms = Stdl_memoized.make(fun name ->
     let form = List.assoc name l in 
     Stdl_ordered.to_list(str_intersect(Stdl_jvag_form.coatoms form) defined_names)
  ) in  
  let (cycles,acyclic_ordering) = 
     Stdl_lower_acyclic_subposet.compute relative_coatoms (Stdl_ordered.to_list defined_names) in 
  if cycles<>[]
  then raise(Find_acyclic_ordering_exn("Cycles found : ",cycles)) 
  else       
  let defined_names_in_acylic_order = Stdl_image.image fst acyclic_ordering in 
  let (ghosts,nonghosts) = List.partition (fun name->relative_coatoms name=[]) defined_names_in_acylic_order in 
  let part1 = Stdl_ordered.image (fun name -> (name,None)) undefined_names
  and part2 = Stdl_image.image (fun name -> (name,List.assoc_opt name l)) (ghosts@nonghosts) in 
  part1 @ part2 ;; 

let extract_at_names (AL l) names = 
   List.filter (fun (name,_)->List.mem name names) l ;;  

let uppercase_min = int_of_char 'A' ;;
let uppercase_max = int_of_char 'Z' ;;

let is_uppercase c = 
   let i = int_of_char c in 
   (uppercase_min<=i) && (i<=uppercase_max) ;;

let snake_case_from_camel_case camel_case =
    let n = String.length camel_case in
    let exploded = Stdl_int_range.scale (fun j->(j>1,String.get camel_case (j-1)) ) 1 n in 
    let temp = Stdl_image.image (
      fun (is_not_the_first_char,c) -> 
        let s = String.make 1 c in 
        if (is_uppercase c) && is_not_the_first_char then "_"^s else s 
    ) exploded in 
    let temp2 =String.lowercase_ascii (String.concat "" temp) in 
    Stdl_replace_inside.replace_inside_text ~display_number_of_matches:false ("__","_") temp2;;

(*
    snake_case_from_camel_case "TopLevelClassOrInterfaceDeclaration" ;;
*)
    
let parser_name camel_case = (snake_case_from_camel_case camel_case)^"_prsr" ;;

let prsrtxt_for_concat name l = 
  let sn = string_of_int(List.length l) in 
  "let "^(parser_name name)^" = \n"^
  "   Jvsp"^"_parser.concat"^sn^" \n"^
  "   "^(String.concat " " (Stdl_image.image parser_name l))^" ;;" ;;

let prsrtxt_for_atomic name l = 
  "let "^(parser_name name)^" = \n"^
  "   Jvsp"^"_parser.molecular \n"^
  "   ["^(String.concat ";" (Stdl_image.image 
    (fun tt->"T."^(Stdl_jvsp_util.ocaml_name_for_token_type tt) ) l))^"] ;;" ;;

let prsrtxt_for_disjunction name l = 
  let sn = string_of_int(List.length l) in 
  "let "^(parser_name name)^" = \n"^
  "   Jvsp"^"_parser.dis"^sn^" \n"^
  "   "^(String.concat " " (Stdl_image.image parser_name l))^" ;;" ;;

let prsrtxt_for_star name nm = 
  "let "^(parser_name name)^" = \n"^
  "   Jvsp"^"_parser.star "^(parser_name nm)^" ;;" ;;

let prsrtxt_for_optional name nm = 
  "let "^(parser_name name)^" = \n"^
  "   Jvsp"^"_parser.optional "^(parser_name nm)^" ;;" ;;

let prsrtxt_for_synonym name nm = 
  "let "^(parser_name name)^" = "^(parser_name nm)^" ;;" ;;

let prsrtxt_for_undefined name = 
  "let "^(parser_name name)^" = Jvsp_"^"parser.always_fails ;;" ;;  

let prsrtxt_for_pair (name,form_opt) = match form_opt with 
  None -> prsrtxt_for_undefined name 
  |(Some form) ->
  match form with 
   (Concat l) ->  prsrtxt_for_concat name l
   |Molecular l -> prsrtxt_for_atomic name l
   |Disjunction l -> prsrtxt_for_disjunction name l
   |Star nm -> prsrtxt_for_star name nm
   |Optional nm -> prsrtxt_for_optional name nm
   |Synonym nm -> prsrtxt_for_synonym name nm ;;

let prsrtxt_for_pair_list l = 
  "\n\n\n module T = Jvsp_types ;; \n\n"^(String.concat "\n" (Stdl_image.image prsrtxt_for_pair l))^"\n\n\n" ;;

let ap_for_prsrtxt = Stdl_absolute_path.of_string 
((Sys.getenv "HOME") ^"/Teuliou/OCaml/skeptical_duck/watched/watched_not_githubbed/preparatory_jvsp_parser.ml");;   

let write_prsrtxt l = 
   let acyclic_l =find_acyclic_ordering l in 
   let text = prsrtxt_for_pair_list acyclic_l in 
   Stdl_replace_inside.overwrite_between_markers_inside_file 
   ~overwriter:text  ("(* OCaml-generated parser begins here *)","(* OCaml-generated parser ends here *)") 
   ap_for_prsrtxt ;;

end ;;  

module Nonrecursive_grammar = struct 

let order_on_string_pairs = Stdl_total_ordering.product str_order str_order ;;

let auxiliary_order2 = (
   (fun pair1 pair2 ->Stdl_total_ordering.standard pair1 pair2): 
     (form *(string list)) Stdl_total_ordering_t.t ) ;; 

let order_on_nrg_pairs = Stdl_total_ordering.product str_order auxiliary_order2 ;;

exception Fatherless of string ;;
exception Unproductive_son of string * string;;

let insert_new old_grammar son son_form = 
  match List.assoc_opt son old_grammar.sons_and_fathers with 
  (Some father) -> raise(Unproductive_son(son,father))
  | None ->
  match List.find_opt (fun (_,(form,_)) ->str_mem son (Stdl_jvag_form. coatoms form)) old_grammar.productions with 
  None -> raise(Fatherless(son))
  |(Some (father,(_form,ancestry))) ->
    if List.mem son ancestry 
    then raise(Circularity(son,father::ancestry))
    else     
    let new_item = (son,(son_form,father::ancestry)) in   
    let old_sons_and_fathers = Stdl_ordered.safe_set order_on_string_pairs old_grammar.sons_and_fathers 
    and old_productions = Stdl_ordered.safe_set order_on_nrg_pairs old_grammar.productions in 
  {
   sons_and_fathers  = Stdl_ordered.to_list(Stdl_ordered.insert order_on_string_pairs (son,father) old_sons_and_fathers);
   productions = Stdl_ordered.to_list(Stdl_ordered.insert order_on_nrg_pairs new_item old_productions);
  };; 



exception Expand_a_second_time_exn2 of string * form;;
exception Expand_a_second_time_exn3 of string * string * (string list);;

let get_from_nonrecursive_grammar provider old_grammar name = 
  match List.assoc_opt name old_grammar.productions with 
   (Some (form,_)) -> (form,None) 
  | None->    
  let form = get provider name in 
  let new_grammar = insert_new old_grammar name form in 
  (form, Some new_grammar) ;;

let singleton provider origin = 
  let original_form = get provider origin in 
  {
   sons_and_fathers  = [];
   productions = [origin,(original_form,[])];
  };; 

  

end ;;  


module With_dwarf_count = struct
  
type t = WDC of int * grammar ;;

let make gram =
  let dwarf_count = Dwarf_count.recompute_dwarf_count_from_scratch gram in 
  WDC(dwarf_count,gram) ;;

module Common = struct

let register_with_dwarfy_name_if_needed gram_with_dwc ~suffix form = 
  let (WDC(old_dwarf_count,gram)) = gram_with_dwc in 
   match name_for_form_opt gram form with 
  Some old_name ->(gram_with_dwc,old_name)
  |None -> 
  let new_dwarf_count = old_dwarf_count +1 in 
  let new_name = Dwarf_count.dwarfy_name ~suffix new_dwarf_count form in 
  if get_opt gram new_name <> None then raise(Name_already_in_use(new_name)) else  
  let new_ag = replace_pair_or_add_if_absent (new_name,form) gram in  
  (WDC(new_dwarf_count,new_ag),new_name) ;;  

let rec helper_for_multiple_registration (treated,gram_with_dwc,suffix,to_be_treated) =
  match to_be_treated with 
  [] -> (gram_with_dwc,List.rev treated)
  |form1 :: other_forms ->
    let (gram1_with_dwc,name1) = register_with_dwarfy_name_if_needed gram_with_dwc ~suffix form1 in 
    helper_for_multiple_registration (name1::treated,gram1_with_dwc,suffix,other_forms)
  ;;

let register_several_with_dwarfy_name_if_needed gram ~suffix forms = 
      helper_for_multiple_registration ([],gram,suffix,forms) ;;  

let get (WDC(_dwarf_count,gram)) name = get gram name ;;

let replace_pair_or_add_if_absent pair (WDC(old_dwarf_count,AL l)) = 
  let (name,_form) = pair in 
  let (name_was_already_there,new_l) = (
    match List.assoc_opt name l with 
    None -> (false,pair :: l) 
    |Some _ -> (true,Stdl_image.image (fun pair2->
      if (fst pair2)=name then pair else pair2 ) l)
  )  in 
  let new_dwarf_count = (
    if name_was_already_there 
    then old_dwarf_count 
    else max old_dwarf_count (Dwarf_count.dwarf_number_in_name name)  
  ) in
  WDC(new_dwarf_count,AL(Stdl_ordered.to_list(Stdl_ordered.sort order_on_pairs new_l)));; 


let create_new_pair pair (WDC(old_dwarf_count,AL l)) = 
  let (name,_form) = pair in 
  let new_l = (
    match List.assoc_opt name l with 
    None -> pair :: l 
    |Some _ -> raise(Name_is_not_new_exn(name))
  )  
  and new_dwarf_count = max old_dwarf_count (Dwarf_count.dwarf_number_in_name name) in 
  WDC(new_dwarf_count,AL(Stdl_ordered.to_list(Stdl_ordered.sort order_on_pairs new_l)));; 

end ;;

module Local_Modification = struct 

exception Lm_get_exn of form ;;
exception Bad_index_exn of string * int * string ;;
exception Bad_range_exn of string * int * int * string ;;
exception Bad_form_exn of  string * int * string * form  * string ;;
exception Bad_lid_form_exn of  string * location_in_disjunction * string * form  * string ;;

let lm_get gram name = 
  let form = Common.get gram name in 
  match form with 
  Disjunction(l) -> Stdl_image.image (fun nm->(nm,Common.get gram nm)) l 
  |Concat(_)
  |Synonym(_) ->[name,form]
  |Molecular  _
  |Star _
  |Optional _ ->raise(Lm_get_exn(form));;  
  
let match_named_concat name form = 
    match Stdl_jvag_form.concat_content_opt form with 
  None -> [name]
  |Some(chain)-> chain ;;  

let match_named_concats named_forms = 
   Stdl_image.image (fun (name,form)-> match_named_concat name form) named_forms;; 

let match_lid_concat form (text_for_index,index,caller_name)= 
    match Stdl_jvag_form.concat_content_opt form with 
  None -> raise (Bad_lid_form_exn(text_for_index,index,"concat expected",form,caller_name))
  |Some(chain)-> chain ;;

exception Index_for_lid_exn of location_in_disjunction;;

let index_for_lid indexed_forms lid = match lid with   
  (I i)-> i
  |N name -> match List.find_opt(fun (_,(nm,_)) ->nm=name) indexed_forms with 
     None -> raise(Index_for_lid_exn(lid))
     |Some(idx,_) -> idx
   ;;
   

let extract_lid_range_from_disjunction caller_name named_forms (lid_start,lid_end) =  
  let indexed_forms = Stdl_int_range.index_everything named_forms in 
  let range_start = index_for_lid indexed_forms lid_start 
  and range_end =  index_for_lid indexed_forms lid_end 
  and n=List.length(named_forms) in 
   if (range_start<0)||(range_end<range_start)||(range_end>n)
  then raise (Bad_range_exn("range in disjunction",range_start,range_end,caller_name))
  else Stdl_list_again.two_sided_cutting (range_start-1,n-range_end) named_forms ;;    

let extract_lid_from_disjunction caller_name named_forms lid =  
  let (left,center,right) = extract_lid_range_from_disjunction caller_name named_forms (lid,lid) in 
  (left,List.hd center,right) ;;  


let extract_element_from_concat caller_name chain index_in_concat = 
   if (index_in_concat<0)||(index_in_concat>List.length(chain))
  then raise (Bad_index_exn("index in concat",index_in_concat,caller_name))
  else
  let (before,temp) = Stdl_list_again.cut (index_in_concat-1) chain in 
  let (pivot,after) = Stdl_list_again.head_with_tail temp in 
  (before,pivot,after) ;;

let extract_range_from_concat caller_name chain (range_start,range_end) =  
   if (range_start<0)||(range_end<range_start)||(range_end>List.length(chain))
  then raise (Bad_range_exn("range in concat",range_start,range_end,caller_name))
  else
  let (before,temp) = Stdl_list_again.cut (range_start-1) chain in 
  let d = range_end - range_start +1 in 
  let (between,after) = Stdl_list_again.cut d temp in
  (before,between,after) ;; 


exception Remove_left_recursive_line_in_disjunction_exn of string * string ;;

exception Compute_lump_optional_for_concatenation_of_two_exn ;;
let rewrite_optional_of_a_concatenation_of_two a b =
  if b="Starred"^a then Star(a) else 
  if a="Starred"^b then Star(b) else
  raise Compute_lump_optional_for_concatenation_of_two_exn ;;
    

exception Compute_lump_optional_for_a_concatenation_exn ;;
let rewrite_optional_of_a_concatenation l =
    if List.length(l)=1 
    then Optional(List.hd l)
    else 
    if List.length(l)=2
    then rewrite_optional_of_a_concatenation_of_two (List.nth l 0) (List.nth l 1)
    else raise Compute_lump_optional_for_a_concatenation_exn ;;  

exception Compute_lump_in_disjunction_in_binary_case_exn ;;

let compute_lump_in_disjunction_in_binary_case possibly_large_centers = 
   let l1 = List.nth  possibly_large_centers 0 
   and l2 = List.nth  possibly_large_centers 1 in 
   if l1=[] then rewrite_optional_of_a_concatenation l2 else 
   if l2=[] then rewrite_optional_of_a_concatenation l1 else  
   raise Compute_lump_in_disjunction_in_binary_case_exn ;;


exception Compute_lump_in_disjunction_exn ;;

let compute_lump_in_disjunction possibly_large_centers = 
  if List.for_all (fun x->List.length(x)=1) possibly_large_centers 
  then let centers = Stdl_image.image List.hd possibly_large_centers in 
       Stdl_jvag_types.Disjunction centers
  else 
  if List.length(possibly_large_centers)<>2
  then raise Compute_lump_in_disjunction_exn
  else compute_lump_in_disjunction_in_binary_case possibly_large_centers

let compute_lump_in_concatenation names_between forms_between = 
   let moleculars_opt = Stdl_image.image (Stdl_jvag_form.molecular_content_opt) forms_between in 
   if List.for_all (fun opt->opt<>None) moleculars_opt
   then Molecular(List.flatten(Stdl_image.image Option.get moleculars_opt))
   else Concat names_between ;;

exception Compute_lump_in_nondisjunction_point_expansion_exn of form ;;  

let compute_lump_in_nondisjunction_point_expansion inner_pivot = 
  match inner_pivot with 
   Concat(l)->l
  |Molecular(l)->Stdl_image.image (fun tok->Stdl_jvsp_util.code_for_tokentype_sequence_in_production_names [tok]) l
  |Synonym (older_name)->[older_name]
  |Star _
  |Optional _ 
  |Disjunction _ -> raise(Compute_lump_in_nondisjunction_point_expansion_exn(inner_pivot));; 

exception Expand_disjunction_or_synonym_exn of form ;;

let expand_disjunction_or_synonym form= 
  match form with 
   Disjunction(l)->l 
  |Synonym (older_name)->[older_name] 
  |Concat(_)
  |Molecular(_)
  |Star _
  |Optional _  -> raise(Expand_disjunction_or_synonym_exn(form));; 


let remove_left_recursive_lines_in_disjunction (gram,(name,named_forms)) = 
  let headed_forms = Stdl_image.image (fun pair ->
     let (nm,form)=pair in 
     (pair,Stdl_list_again.head_with_tail(match_named_concat nm form))
  ) named_forms in 
  let (recursive_lines,nonrecursive_lines) = List.partition (
   fun (_pair,(h,_t)) -> h = name
  ) headed_forms in 
  let tails = Stdl_image.image (
   fun (_pair,(_h,t)) -> 
    if List.length(t)=1
    then Common.get gram (List.hd t) 
    else Concat t
  ) recursive_lines in 
  let (gram2,names_for_tails) = Common.register_several_with_dwarfy_name_if_needed gram ~suffix:"" tails in 
  let (gram3,name_for_extndr) = (
    if List.length(names_for_tails)=1 
    then (gram2,List.hd names_for_tails)
    else 
    let extndr = Stdl_jvag_types.Disjunction(names_for_tails) in  
    Common.register_with_dwarfy_name_if_needed gram2 ~suffix:(name^"Extender") extndr 
  ) in  
  let names_for_nonrecursives = Stdl_image.image  (fun ((name,_),_) -> name ) nonrecursive_lines in 
  let (gram4,name_for_core) = (
    if List.length(names_for_nonrecursives)=1 
    then (gram3,List.hd names_for_nonrecursives)
    else 
    let core = Stdl_jvag_types.Disjunction(names_for_nonrecursives) in  
    Common.register_with_dwarfy_name_if_needed gram3 ~suffix:name core 
  ) in  

  (gram4,[name,Stdl_jvag_types.Concat([name_for_core;"Starred"^name_for_extndr])]) ;;



let expand_lines_in_disjunction (gram,(_nahme,named_forms)) lids = 
  let indexed_forms = Stdl_int_range.index_everything named_forms in 
  let blocks = Stdl_image.image (
   fun (idx,(name,form))->
    if (List.mem (I idx) lids)||(List.mem (N name) lids)
    then expand_disjunction_or_synonym form 
    else [name]  
  ) indexed_forms in 
  let new_list_of_names = Stdl_list_again.nonredundant_version(List.flatten blocks) in 
  (gram,Stdl_image.image (fun nm->(nm,Common.get gram nm)) new_list_of_names);;
  
let replacement_content_during_point_expansion (names_before,names_after) inner_pivot =
   match Stdl_jvag_form.disjunction_content_opt inner_pivot with 
  None -> let lump = compute_lump_in_nondisjunction_point_expansion inner_pivot in 
          [Stdl_jvag_types.Concat(names_before@lump@names_after)]
  |Some(inner_disjunction) -> 
       Stdl_image.image (fun elt-> Stdl_jvag_types.Concat(names_before@[elt]@names_after) ) inner_disjunction;;
  

let expand_point_in_line (gram,(name,named_forms)) (lid,index_in_concat) = 
  let (before,outer_pivot,after) = extract_lid_from_disjunction "expand_point_in_line" named_forms lid in  
  let chain = match_lid_concat (snd outer_pivot) ("index in disjunction",lid,"expand_point_in_line") in
  let (names_before,name_there,names_after) = extract_element_from_concat "expand_concat" chain index_in_concat in   
  let inner_pivot = Common.get gram name_there in 
  let new_elements = replacement_content_during_point_expansion (names_before,names_after) inner_pivot in 
  let (gram2,names_for_new_elements) = Common.register_several_with_dwarfy_name_if_needed gram ~suffix:name new_elements in 
  let named_new_elements = List.combine names_for_new_elements new_elements in 
  (gram2,before @ named_new_elements @ after);;


let reunite_in_concatenation (gram,(name,named_forms)) (lid,(range_start,range_end)) = 
  let (before,old_pivot,after) = extract_lid_from_disjunction "reunite_in_concatenation" named_forms lid in  
  let chain = match_lid_concat (snd old_pivot) ("index in disjunction",lid,"reunite_in_concatenation") in
  let (names_before,names_between,names_after) = extract_range_from_concat "reunite_in_concatenation" chain (range_start,range_end) in 
  let forms_between = Stdl_image.image(Common.get gram) names_between in 
  let lump_form = compute_lump_in_concatenation names_between forms_between in 
  let (gram2,name_for_lump_form) = Common.register_with_dwarfy_name_if_needed gram ~suffix:"" lump_form in
  let new_conc_in_disj = Stdl_jvag_types.Concat(names_before@[name_for_lump_form]@names_after) in 
  let (gram3,name_for_new_conc_in_disj) = Common.register_with_dwarfy_name_if_needed gram2 ~suffix:name new_conc_in_disj in 
  (gram3,before @ [(name_for_new_conc_in_disj,new_conc_in_disj)]  @ after);;  

let reunite_in_disjunction (gram,(name,named_forms)) (lid_start,lid_end) = 
  let (before,named_forms_between,after)=extract_lid_range_from_disjunction "reunite_in_disjunction" named_forms (lid_start,lid_end) in 
  let chains_between = match_named_concats named_forms_between  in 
  let (left,centers,right) = Stdl_list_again.two_sided_common_parts chains_between  in 
  let lump_form = compute_lump_in_disjunction centers in 
   let (gram2,name_for_lump_form) = Common.register_with_dwarfy_name_if_needed gram ~suffix:"" lump_form in 
  let new_conc_in_disj = Stdl_jvag_types.Concat(left@[name_for_lump_form]@right) in 
  let (gram3,name_for_conc_in_disj) = Common.register_with_dwarfy_name_if_needed gram2 ~suffix:name new_conc_in_disj in 
  (gram3,before @ [(name_for_conc_in_disj,new_conc_in_disj)]  @ after);;

exception Replace_self_with_older_synonym_exn of string * ((string * form) list) ;;

let replace_self_with_older_synonym  (gram,(name,named_forms)) = 
   if List.length(named_forms)<>1
   then raise( Replace_self_with_older_synonym_exn(name,named_forms))
   else
   let (_,the_synonym_form) = List.hd named_forms in 
   match Stdl_jvag_form.synonym_content_opt the_synonym_form with 
   None -> raise( Replace_self_with_older_synonym_exn(name,named_forms))
   |Some older_synonym ->
    (gram,[name,Common.get gram older_synonym]) ;; 

exception Permute_lines_exn of ((string * form) list)*(int list) ;;

let permute_lines  (gram,(_name,named_forms)) ordering= 
   let n = List.length ordering in  
   if List.length(named_forms)<>n
   then raise(  Permute_lines_exn(named_forms,ordering))
   else
   let permuted_named_forms = Stdl_image.image (fun k->List.nth named_forms (k-1)) ordering in 
   (gram,permuted_named_forms) ;; 

let ref_for_block_sizes = ref [] ;;

let expand_heads_at_lines (gram,(global_name,named_forms)) line_indices = 
  let gram_ref = ref(gram) 
  and _ = (ref_for_block_sizes:=[])in 
  let indexed_forms = Stdl_int_range.index_everything named_forms in 
  let new_blocks = Stdl_image.image (
    fun (idx,(name,form))->
      let old_accu = (!ref_for_block_sizes) in 
      if not(List.mem idx line_indices) 
      then let _ = (ref_for_block_sizes:=1::(old_accu)) in 
           [name,form] 
      else 
      let old_gram = !gram_ref in 
      let (inner_pivot_name,rest_of_form)=(
        match Stdl_jvag_form.concat_content_opt form with 
         None -> (name,[])
        |(Some l)->Stdl_list_again.head_with_tail(l)
      ) in 
      let inner_pivot = Common.get gram inner_pivot_name in 
      let new_elements = replacement_content_during_point_expansion ([],rest_of_form) inner_pivot in 
      let (new_gram,names_for_new_elements) = Common.register_several_with_dwarfy_name_if_needed old_gram ~suffix:global_name new_elements in 
      let _ = (
        ref_for_block_sizes:=(List.length new_elements)::(old_accu);
        gram_ref:=new_gram) in 
      List.combine names_for_new_elements new_elements 
  ) indexed_forms in 
  let _ = (ref_for_block_sizes:=List.rev(!ref_for_block_sizes)) in 
  (!gram_ref,List.flatten new_blocks);;

let rec helper_for_alldis_expansion gram (treated,to_be_treated) = match to_be_treated with 
  [] -> List.rev treated 
  |(name,form)::others ->
    match Stdl_jvag_form.disjunction_content_opt form with 
    None -> helper_for_alldis_expansion gram (name::treated,others)
    |Some parts -> 
      let new_data = Stdl_image.image (fun part->(part,Common.get gram part)) parts in 
      helper_for_alldis_expansion gram (treated,new_data@others) ;;

let expand_all_disjunctions (gram,(global_name,named_forms)) =
    let fully_expanded = helper_for_alldis_expansion gram ([],named_forms) in 
    (gram,[global_name,Disjunction(fully_expanded)]) ;;


let apply name (gram,named_forms) modif=
  let gf = (gram,(name,named_forms)) in 
  match modif with 
   Lm_remove_left_recursive_lines_in_disjunction ->  
    remove_left_recursive_lines_in_disjunction gf 
  |(Lm_expand_lines_in_disjunction(lids)) ->  
     expand_lines_in_disjunction gf lids 
  |(Lm_expand_point_in_line(lid,index_in_concat)) ->  
     expand_point_in_line gf (lid,index_in_concat)  
  |(Lm_reunite_in_concatenation(lid,(range_start,range_end))) -> 
    reunite_in_concatenation gf  (lid,(range_start,range_end))  
  |(Lm_reunite_in_disjunction(lid_start,lid_end)) ->
    reunite_in_disjunction gf  (lid_start,lid_end)  
  |Lm_replace_self_with_older_synonym ->
    replace_self_with_older_synonym gf 
  |Lm_permute_lines ordering ->
    permute_lines gf ordering  
  |(Lm_expand_heads_at_lines(line_indices)) ->  
     expand_heads_at_lines gf line_indices    
  |Lm_expand_all_disjunctions ->  
     expand_all_disjunctions gf   
  ;;

let apply_several name gf mods = List.fold_left (apply name) gf mods ;;

end ;;  

module Modify = struct
  
let expand_form_using_concat (name,chain) form = match form with
  (Concat l) -> Concat(List.flatten(Stdl_image.image (fun name2->if name2=name then chain else [name2]) l))
   |Disjunction _
   |Molecular  _
   |Star _
   |Optional _ 
   |Synonym _ -> form;;   

let expand_pair_using_concat data (name,form) =
    (name,expand_form_using_concat data form) ;;

let expand_grammar_using_concat data (AL l) =
  AL(Stdl_image.image (expand_pair_using_concat data) l);;


let add_pair_and_simplify_some_concats_on_bare_grammar pair gram = 
  let gram2 = replace_pair_or_add_if_absent pair gram in 
  match snd pair with 
  (Concat l) -> expand_grammar_using_concat (fst pair,l) gram2    
   |Disjunction _
   |Molecular  _
   |Star _
   |Optional _ 
   |Synonym _ -> gram2;;   

let add_pair_and_simplify_some_concats pair  (WDC(old_dwarf_count,gram)) = 
   WDC(old_dwarf_count,add_pair_and_simplify_some_concats_on_bare_grammar pair gram) ;;  

let rename_on_name (old_name,new_name) name =
  if name = old_name then new_name else name ;; 
  
let rename_on_form renaming_data form = 
  let rename = rename_on_name renaming_data in 
  match form with
    Concat l -> Concat(Stdl_image.image rename l)
   |Molecular _  -> form
   |Disjunction l -> Disjunction(Stdl_image.image rename l) 
   |Star nm -> Star (rename nm)
   |Optional nm -> Optional (rename nm)
   |Synonym nm -> Synonym (rename nm) ;;  

let rename_on_pair renaming_data (name,form) =
   (rename_on_name renaming_data name,rename_on_form renaming_data form) ;;    

let rename_on_bare_grammar renaming_data (AL l)=
 let unordered_new_l = Stdl_image.image (rename_on_pair renaming_data) l in 
(AL (Stdl_ordered.to_list(Stdl_ordered.sort order_on_pairs (unordered_new_l)))) ;;


let rename_on_grammar renaming_data (WDC(old_dwarf_count,gram)) = 
  let new_gram = rename_on_bare_grammar renaming_data gram in 
  let (old_name,new_name) = renaming_data in 
  let old_v=Dwarf_count.dwarf_number_in_name old_name
  and new_v=Dwarf_count.dwarf_number_in_name new_name in 
  let new_dwarf_count = (
     if (old_v<old_dwarf_count) && (new_v<= old_dwarf_count)  
     then old_dwarf_count
     else Dwarf_count.recompute_dwarf_count_from_scratch new_gram
  ) in 
  WDC(new_dwarf_count,new_gram) ;;

let remove_productions to_be_removed gram_with_dwc =
   let (WDC(old_dwarf_count,AL l)) = gram_with_dwc in 
   WDC(old_dwarf_count,AL(List.filter (fun (name,_)->not(List.mem name to_be_removed)) l)) ;;


let csg_in_form rep_pair form = 
   let (newer_synonym,older_synonym)= rep_pair in 
   let replacer = Stdl_list_again.replace_if_proposed [rep_pair] in 
   match form with 
   (Synonym nm) -> Synonym(replacer nm)    
   |Disjunction l ->
        let new_l=(
          if List.mem older_synonym l 
          then List.filter (fun x->x<>newer_synonym) l
          else Stdl_image.image replacer l) in 
        if List.length(new_l)=1 
        then Synonym older_synonym 
        else Disjunction new_l    
   |Concat l -> Concat(Stdl_image.image replacer l)
   |Star nm -> Star(replacer nm)
   |Optional nm -> Optional (replacer nm)
   |Molecular  _ -> form;;   


let csg_in_pair_opt rep_pair (name,form) = 
  if name = fst(rep_pair)
  then None 
  else Some(name,csg_in_form rep_pair form ) ;; 


let csg_in_grammar newer_synonym gram_with_dwc =
   let (WDC(old_dwarf_count,AL l)) = gram_with_dwc 
   and older_synonym = Stdl_jvag_form.synonym_content(Common.get gram_with_dwc newer_synonym) in 
   WDC(old_dwarf_count,AL(List.filter_map(csg_in_pair_opt (newer_synonym,older_synonym)) l));;   
 

exception Apply_local_modifications_exn of string * (local_modification list);;

let apply_local_modifications gram name mods =
   let start_dis = Local_Modification.lm_get gram name in 
   let (gram2,named_forms) =  
     Local_Modification.apply_several name (gram,start_dis) mods in 
     let final_form = (
   if List.length(named_forms)>1
   then Stdl_jvag_types.Disjunction (Stdl_image.image fst named_forms)
   else   
   let (other_name,other_form) = List.hd named_forms in 
   if other_name<>name 
   then Synonym other_name
   else (* typically, one gets here when the content of the production
           is reset by 'fiat', such as during a remove_left_recursive_lines operation
           or an replace_self_with_older_synonym opration
         *)
        other_form 
     ) in 
    
   add_pair_and_simplify_some_concats (name,final_form) gram2 ;;

let apply gram = function 
   (Set_production(name,form)) -> Common.replace_pair_or_add_if_absent (name,form) gram 
  |Create_production(name,form) ->  Common.create_new_pair (name,form) gram 
  |Rename(old_name,new_name) -> rename_on_grammar (old_name,new_name) gram
  |Remove_productions(to_be_removed) -> remove_productions to_be_removed gram
  |Collapse_synonym_globally(newer_synonym) -> csg_in_grammar newer_synonym gram
  |Local(name,mods)->apply_local_modifications gram name mods;;
 

let apply_several gram modifications = 
   List.fold_left apply gram modifications ;;

let debug_bad_list_of_local_modifications gram name local_modifs=
  let orig = Local_Modification.lm_get gram name in 
  let (count_before_bug,the_problematic_local_modif) = 
    Stdl_tools_for_debugging.extract_from_fold_left (Local_Modification.apply name) (gram,orig) local_modifs in
  let local_modifs_before_bug = Stdl_list_again.long_head count_before_bug local_modifs in 
  (count_before_bug,local_modifs_before_bug,
  (Local_Modification.apply_several name) (gram,orig) local_modifs_before_bug,
  the_problematic_local_modif) ;;


let debug_bad_list_of_modifications gram modifs=
  let (count_before_bug,the_problematic_modif) = Stdl_tools_for_debugging.extract_from_fold_left apply gram modifs in 
  let modifs_before_bug = Stdl_list_again.long_head count_before_bug modifs in 
  (count_before_bug,modifs_before_bug,apply_several gram modifs_before_bug,the_problematic_modif) ;;



end ;;



end ;; 

end ;; 


module Nonrecursive_grammar = struct 

let get = Private.Nonrecursive_grammar.get_from_nonrecursive_grammar ;;  
let singleton = Private.Nonrecursive_grammar.singleton ;;

end ;;  

module Temporary_Storage = struct 

let block_sizes_during_last_expansion_of_line_heads () =
  (!(Private.With_dwarf_count.Local_Modification.ref_for_block_sizes));;

end ;;  

let replace_pair_or_add_if_absent = Private.replace_pair_or_add_if_absent ;;

let check_disjunction_ladder = Private.check_disjunction_ladder ;; 
let containing = Private.containing ;;

let debug_bad_list_of_local_modifications gram name local_modifs=
 let gram_with_dwc = Private.With_dwarf_count.make gram in 
 let  (count_before_bug,local_modifs_before_bug,(gram_with_dwc_before_bug,dis_before_bug),the_problematic_local_modif) =
    Private.With_dwarf_count.Modify.debug_bad_list_of_local_modifications gram_with_dwc name local_modifs in 
 let (Private.With_dwarf_count.WDC(_,gram_before_bug)) = gram_with_dwc_before_bug in 
 (count_before_bug,local_modifs_before_bug,(gram_before_bug,dis_before_bug),the_problematic_local_modif) ;;


let debug_bad_list_of_modifications gram modifs=
 let gram_with_dwc = Private.With_dwarf_count.make gram in 
 let  (count_before_bug,modifs_before_bug,gram_with_dwc_before_bug,the_problematic_modif) =
    Private.With_dwarf_count.Modify.debug_bad_list_of_modifications gram_with_dwc modifs in 
 let (Private.With_dwarf_count.WDC(_,gram_before_bug)) = gram_with_dwc_before_bug in 
 (count_before_bug,modifs_before_bug,gram_before_bug,the_problematic_modif) ;;


let differences = Private.differences ;;
let extract_at_names = Private.WriteParser.extract_at_names ;;
let get = Private.get ;;
let get_and_display = Private.get_and_display ;;
let get_opt = Private.get_opt ;;

let get_simplified_version = Private.forced_disjunction_of_concats ;;

let just_below = Private.just_below ;;
let lower_interval_below = Private.lower_interval_below ;;

let modify gram modifs = 
  let gram_with_dwc = Private.With_dwarf_count.make gram in 
  let  new_gram_with_dwc = Private.With_dwarf_count.Modify.apply_several gram_with_dwc modifs in 
  let (Private.With_dwarf_count.WDC(_,new_gram)) = new_gram_with_dwc in 
  new_gram ;;
  
let name_for_form_opt = Private.name_for_form_opt ;; 
let ocaml_name = Private.ocaml_name ;;
let order_on_pairs = Private.order_on_pairs ;;

(* This is a registered printer : print_out *)

let print_out (fmt:Format.formatter) (AL l)=
   Format.fprintf fmt "@[%s@]" ("("^(string_of_int(List.length l))^" productions)");;


let sanitize = Private.Sanitize.sanitize_as_many_times_as_needed ;;
let singleton name form = AL [name,form] ;;     
let write_parser = Private.WriteParser.write_prsrtxt ;;

end;;






module Stdl_jvsp_token_types_list=struct

(*

#use"lib/Java_analysis/jvsp_token_types_list.ml";;

*)

type t = Stdl_jvsp_types.token_type_list =  TTL of (Stdl_jvsp_types.token_type list) ;;



module Private = struct 

let to_string (TTL l) = 
   Stdl_strung.with_size_limit ~size_limit:250
   (String.concat " " (Stdl_image.image Stdl_jvsp_util.summary_of_token_type l)) ;;

let rec find_and_forget_opt f l = match l with 
 [] -> None 
 | x :: others ->
    if f x 
   then Some l
   else find_and_forget_opt f others ;;


end ;;    


let construct l =(TTL l) ;;

let find_and_forget_opt f (TTL l) = Option.map (fun z->TTL z)(Private.find_and_forget_opt f l);;

let long_tail k (TTL l)= TTL(List.drop k l);;
  
(* This is a registered printer : print_out *)
let print_out (fmt:Format.formatter) ttl=
   Format.fprintf fmt "@[%s@]" (Private.to_string ttl);;

let starts_with (TTL l) prefix = Stdl_list_again.starts_with l prefix;;

let to_string = Private.to_string ;;

let unveil (TTL l)= l;;     

end;;






module Stdl_jvng_duplicated_name=struct

(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_duplicated_name.ml";;

*)


type t = PDN of string ;;

module Private = struct 

let veil str = ((PDN str): t);;

let unveil (PDN str: t) =str ;;

let is_a_digit c= let i = int_of_char c in (48<=i)&&(i<=57) ;;

let is_a_digit_or_dot c = (is_a_digit c) || (c='.') ;; 

let is_not_a_digit_or_dot c = not(is_a_digit_or_dot c) ;;

let core str =
   match Stdl_string_find_char.backwards_from_inclusive_opt is_not_a_digit_or_dot str (String.length str) with 
   None -> ""
   |Some k -> Stdl_cull_string.beginning k str ;;

let make str k = 
   let suffix = (if k=1 then "" else string_of_int k) in 
   veil ((core str)^suffix);;


let name pdn =
   let str = unveil pdn in 
   match Stdl_string_find_char.backwards_from_inclusive_opt is_not_a_digit_or_dot str (String.length str) with 
   None -> ""
   |Some k -> Stdl_cull_string.beginning k str ;;

(*

name (veil "Gabriel234") ;;
name (veil "Gabriel") ;;

*)

let index pdn =
   let str = unveil pdn in 
   match Stdl_string_find_char.backwards_from_inclusive_opt is_not_a_digit_or_dot str (String.length str) with 
   None -> int_of_string str
   |Some k -> 
      if k=String.length str 
      then 1   
      else int_of_string(Stdl_cull_string.cobeginning k str) ;; 

(*

index (veil "Gabriel234") ;;
index (veil "Gabriel") ;;

*)      

let order pdn1 pdn2 = Stdl_total_ordering.standard (unveil pdn1) (unveil pdn2) ;; 

end ;;

let make = Private.make ;;
let index = Private.index ;;
let name = Private.name ;;
let order = (Private.order : t Stdl_total_ordering_t.t) ;;
let of_string str = make str 1 ;;
let starts_with pdn ~prefix = String.starts_with (name pdn) ~prefix ;;

let to_string pdn = 
   let nm=name pdn 
   and i = index pdn in 
   if i=1 then nm else 
   nm^(string_of_int i);;


end;;






module Stdl_jvng_jvag_types=struct

(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_jvag_types.ml";;

*)

type form = 
    Optional of Stdl_jvng_duplicated_name.t 
   |Molecular of Stdl_jvsp_types.token_type list
   |Concat of Stdl_jvng_duplicated_name.t list 
   |Disjunction of Stdl_jvng_duplicated_name.t list 
   |Star of Stdl_jvng_duplicated_name.t 
   |Synonym of Stdl_jvng_duplicated_name.t
  ;;

type grammar = AL of (Stdl_jvng_duplicated_name.t * form) list ;; 


end;;






module Stdl_jvng_jvag_form=struct

(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_jvag_form.ml";;

*)

open Stdl_jvng_jvag_types ;;

let order = (
   (fun form1 form2 ->Stdl_total_ordering.standard form1 form2): 
     form Stdl_total_ordering_t.t ) ;; 

let uniform_composition link l = match link with 
    Stdl_jvag_types.Optional_L -> Optional(List.hd l)
   |Stdl_jvag_types.Concat_L -> Concat(l)
   |Stdl_jvag_types.Disjunction_L -> Disjunction(l)
   |Stdl_jvag_types.Star_L -> Star(List.hd l)
   |Stdl_jvag_types.Synonym_L -> Synonym(List.hd l);;     

let uniform_decomposition_opt form = match form with
   |Molecular _ -> None    
   |Concat l ->  Some(Stdl_jvag_types.Concat_L,l)
   |Disjunction l -> Some(Stdl_jvag_types.Disjunction_L,l)
   |Star nm -> Some(Stdl_jvag_types.Star_L,[nm])
   |Optional nm -> Some(Stdl_jvag_types.Optional_L,[nm])
   |Synonym nm -> Some(Stdl_jvag_types.Synonym_L,[nm]) ;;   

end;;






module Stdl_jvng_jvag_grammar=struct

(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_jvag_grammar.ml";;

*)

open Stdl_jvng_jvag_types ;;

exception Add_pair_fearfully_exn of Stdl_jvng_duplicated_name.t ;;

module Private = struct 

module Modify = struct 

   let order_on_pairs = Stdl_total_ordering.product Stdl_jvng_duplicated_name.order Stdl_jvng_jvag_form.order ;;

   let add_pair_fearfully pair (AL l) = 
  let (name,_form) = pair in 
  let new_l = (
    match List.assoc_opt name l with 
    None -> pair :: l 
    |Some _ -> raise(Add_pair_fearfully_exn(fst pair))
  )  in 
  AL(Stdl_ordered.to_list(Stdl_ordered.sort order_on_pairs new_l));; 

  

end ;;    

end ;;   

let add_pair_fearfully = Private.Modify.add_pair_fearfully ;;

let get_opt (AL l) name = List.assoc_opt name l ;;

let singleton name form = AL [name,form] ;;     


end;;






module Stdl_jvng_types=struct

(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_types.ml";;

*)

type ancestry_data = {
   ancestry : Stdl_jvng_duplicated_name.t list ;
   position_in_birth_list : int ;
} ;;

type ancestry_manager = AM of (Stdl_jvng_duplicated_name.t * ancestry_data) list ;;

type grammar_with_ancestry_manager = {
   source : Stdl_jvag_types.grammar ; 
   receiver : Stdl_jvng_jvag_types.grammar ;
   manager : ancestry_manager ;
} ;;

type stream = {
   cursor : int ;
   remaining_list : Stdl_jvsp_token_types_list.t ;
} ;;

type local_analizer = {
  first_approach : (Stdl_jvsp_types.token_type_list -> Stdl_jvng_duplicated_name.t option) ;
  case_by_case : (((Stdl_jvsp_types.token_type list)*Stdl_jvng_duplicated_name.t) list) ; 
  analysis_is_complete : bool ;
};;

type disjunction_ladder_atomic_finder = DLAF of (Stdl_jvsp_types.token_type_list -> (Stdl_jvng_duplicated_name.t list) option) ;;
  
type disjunction_ladder_finder = DLF of disjunction_ladder_atomic_finder list ;;


type battery_of_analizers = {
  deciders_for_optionals_or_stars : (Stdl_jvng_duplicated_name.t  * local_analizer) list ;  
  choosers_for_disjunctions : (Stdl_jvng_duplicated_name.t * local_analizer) list;
  precomputed_first_tokens : ( Stdl_jvng_duplicated_name.t * ((Stdl_jvsp_types.token_type list) list)) list;
  ladder_finder : disjunction_ladder_finder;
} ;;

type global_analizer = {
  managed_grammar : grammar_with_ancestry_manager ;
  battery : battery_of_analizers ;
  head : Stdl_jvng_duplicated_name.t ;
  tail : Stdl_jvng_duplicated_name.t list ;
  consumable : stream ;
} ;;


end;;






module Stdl_jvng_with_ancestry_manager=struct

(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_with_ancestry_manager.ml";;

*)

open Stdl_jvng_types ;;

exception Get_ancestry_data_for_dname_exn of Stdl_jvng_duplicated_name.t ;;

module Private = struct

let get_ancestry_data_for_dname (AM l) dname = match List.assoc_opt dname l with 
 None -> raise(Get_ancestry_data_for_dname_exn(dname))
 |Some data -> data;;

let order_for_pairs = ((
   Stdl_total_ordering.product Stdl_jvng_duplicated_name.order Stdl_total_ordering.standard
   ): 
   (Stdl_jvng_duplicated_name.t * ancestry_data) Stdl_total_ordering_t.t
) ;; 
let insert_in_ancestry_manager (AM l) pair =
    AM(Stdl_ordered.to_list(Stdl_ordered.insert order_for_pairs pair (Stdl_ordered.safe_set order_for_pairs l)));;

let insert_several_in_ancestry_manager man pairs =
   List.fold_left insert_in_ancestry_manager man pairs ;;

let compute_suitable_duplicate common_ancestry (treated_coatoms,old_manager) (coatom_idx,coatom) =
   let (AM old_ancestry_data)=old_manager in 
   match List.find_opt ( fun (nm,data)->
      (data.ancestry = common_ancestry) 
        && ((Stdl_jvng_duplicated_name.name nm)=coatom)
          && (data.position_in_birth_list = coatom_idx)   
   ) old_ancestry_data with 
   Some(old_answer,_) -> (old_answer::treated_coatoms,AM old_ancestry_data)
   |None ->
   let final_idx = (
     if not(List.exists (fun older_dname ->Stdl_jvng_duplicated_name.name older_dname = coatom) common_ancestry) 
     then 1
     else let older_indices = List.filter_map (fun (nm,_data)->
           if ((Stdl_jvng_duplicated_name.name nm)=coatom) then Some (Stdl_jvng_duplicated_name.index nm) else None
         ) old_ancestry_data  in 
         1+Stdl_max.list older_indices  
   ) in    
   let final_dname = Stdl_jvng_duplicated_name.make coatom final_idx 
   and current_location = {
     ancestry = common_ancestry ;
     position_in_birth_list = coatom_idx ;
   } in 
   let new_pair = (final_dname,current_location) in 
   (final_dname::treated_coatoms,insert_in_ancestry_manager old_manager new_pair ) ;;    


let duplicate_in_list old_ancestry_data common_ancestry coatoms = 
   List.fold_left (compute_suitable_duplicate common_ancestry) ([],old_ancestry_data) 
     (Stdl_int_range.index_everything coatoms) 
    ;;


let duplicate_in_form gram_with_am dname original_form = 
   match Stdl_jvag_form.uniform_decomposition_opt original_form with 
   None -> (Stdl_jvng_jvag_types.Molecular(
      Option.get(Stdl_jvag_form.molecular_content_opt original_form)
   ),gram_with_am.Stdl_jvng_types.manager)
   |Some(link,coatoms) ->
      let old_ancestry_data = gram_with_am.Stdl_jvng_types.manager in 
      let common_ancestry = dname :: ((get_ancestry_data_for_dname old_ancestry_data dname).Stdl_jvng_types.ancestry) in 
      let (new_coatoms,new_ancestry_data) = duplicate_in_list old_ancestry_data common_ancestry coatoms in 
      (Stdl_jvng_jvag_form.uniform_composition link (List.rev new_coatoms),new_ancestry_data) ;;

let get gram_with_am dname =
   let src = gram_with_am.Stdl_jvng_types.source 
   and rcvr = gram_with_am.Stdl_jvng_types.receiver  in 
   match Stdl_jvng_jvag_grammar.get_opt rcvr dname with 
   (Some answer) -> (answer,None)
   | None -> 
      let name = Stdl_jvng_duplicated_name.name dname in 
      let original_form = Stdl_jvag_grammar.get src name in 
      let (adjusted_form,new_ancestry_data) = duplicate_in_form gram_with_am dname original_form in 
      let final_receiver = 
           Stdl_jvng_jvag_grammar.add_pair_fearfully (dname,adjusted_form) rcvr in 
      let final_gram = {
          gram_with_am with 
          receiver = final_receiver ;
          manager = new_ancestry_data 
      } in 
      (adjusted_form,Some final_gram) ;;


let make src origin =
   let first_form_v1 = Stdl_jvag_grammar.get src origin 
   and first_dname = Stdl_jvng_duplicated_name.make origin 1 in 
   let (link,coatoms) = Option.get(Stdl_jvag_form.uniform_decomposition_opt first_form_v1) in 
   let first_coatoms = Stdl_image.image (fun nm -> Stdl_jvng_duplicated_name.make nm 1) coatoms in 
   let first_form_v2 = Stdl_jvng_jvag_form.uniform_composition link (first_coatoms) in 
   let first_nonrec_grammar =  Stdl_jvng_jvag_grammar.singleton first_dname first_form_v2 in 
   let first_data = { ancestry = [] ; position_in_birth_list = 0 } in  
   let indexed_first_coatoms = Stdl_int_range.index_everything first_coatoms in 
   let pairs_from_coatoms = Stdl_image.image ( 
     fun (idx,coatom) -> (coatom, { ancestry = [first_dname] ; position_in_birth_list = idx }) 
   ) indexed_first_coatoms in 
   let first_pairs = (first_dname,first_data) :: pairs_from_coatoms in 
   let first_manager = AM (Stdl_ordered.to_list(Stdl_ordered.sort order_for_pairs first_pairs)) in 
   {
      source =src;   
      receiver = first_nonrec_grammar ;
      manager = first_manager 
   } ;;
     
end ;;

let get = Private.get ;;

let make = Private.make ;; 

end;;






module Stdl_jvng_stream=struct

(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_stream.ml";;

*)

open Stdl_jvng_types ;;

let consume old_stream k = 
  {
   cursor  = old_stream.cursor + k ;
   remaining_list = Stdl_jvsp_token_types_list.long_tail k old_stream.remaining_list ;
} ;;


end;;






module Stdl_jvng_local_analizer=struct

(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_local_analizer.ml";;

*)

open Stdl_jvng_types ;;

module Private = struct 

let no_first_trial cbc is_complete = {
  first_approach = (fun (_tl:Stdl_jvsp_types.token_type_list) -> None);
  case_by_case = cbc ; 
  analysis_is_complete = is_complete ;
};;

end ;;
   
let direct l= Private.no_first_trial (Stdl_image.image (fun x->(x,Stdl_jvng_duplicated_name.of_string "true")) l) true ;;

let first_trial_only f = {
  first_approach = f;
  case_by_case = [] ; 
  analysis_is_complete = false ;
};;

let no_first_trial = Private.no_first_trial ;;

let one_level_above_molecular l= direct [l] ;; 
   
let use anlizr remaining_list = 
    match anlizr.first_approach remaining_list with 
     Some answer1 -> Some answer1 
     | None -> 
    match List.find_opt (fun (prefix,_)->Stdl_jvsp_token_types_list.starts_with remaining_list prefix) anlizr.case_by_case with 
     Some(_,answer2) -> Some answer2
     |None ->
       if anlizr.analysis_is_complete 
       then Some (Stdl_jvng_duplicated_name.of_string "false") 
       else None ;;


       

end;;






module Stdl_jvng_disjunction_ladder_finder=struct

(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_disjunction_ladder_finder.ml";;

*)


module Private = struct 


open Stdl_jvng_types ;;


let apply (DLF atomic_finders) production_name token_types =
   match List.find_map (fun (DLAF tester) ->tester token_types) atomic_finders with 
   None -> None
   |Some ladder ->
     Stdl_list_again.next_in_line_opt production_name ladder ;;
   
let make l = Stdl_jvng_types.DLF(Stdl_image.image (fun f->DLAF f) l);;

end ;; 

let apply = Private.apply ;;

let make = Private.make ;;




end;;






module Stdl_jvng_battery_of_analizers=struct

(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_battery_of_analizers.ml";;

*)

open Stdl_jvng_types ;;

exception Untreated of Stdl_jvng_duplicated_name.t ;;
exception Incomplete of Stdl_jvng_duplicated_name.t ;;

module Private = struct 
let get_local_analizer battery dname =  
  let name = Stdl_jvng_duplicated_name.name dname in
  match Stdl_jvsp_util.token_type_sequence_from_codes_in_production_names_opt name with 
  Some toktypes -> Stdl_jvng_local_analizer.one_level_above_molecular toktypes 
  | None ->
  match List.assoc_opt dname battery.deciders_for_optionals_or_stars with 
        None -> raise(Untreated(dname))
     |(Some analizer) -> analizer;;

end ;;     

let decide battery dname strm=
  let analizer = Private.get_local_analizer battery dname in 
  match (Stdl_jvng_local_analizer.use analizer strm.remaining_list) with 
     None -> raise(Incomplete(dname))
    |Some answer -> bool_of_string (Stdl_jvng_duplicated_name.name answer) ;;    

let choose battery dis_name strm= 
  match Stdl_jvng_disjunction_ladder_finder.apply battery.ladder_finder dis_name strm.remaining_list with 
   Some answer -> answer 
   |None ->
  match List.assoc_opt dis_name battery.choosers_for_disjunctions with 
  None -> raise(Untreated(dis_name))
  |Some(analizer) ->match (Stdl_jvng_local_analizer.use analizer strm.remaining_list) with 
     None -> raise(Incomplete(dis_name))
    |Some answer -> answer ;;    




end;;






module Stdl_jvng_ladder_finder_example=struct

(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_ladder_finder_example.ml";;

*)

module T = Stdl_jvsp_types ;;

exception Malformed_conditional_expression of T.token_type list ;;

module Private = struct 

  let cons_opt x = Option.map (fun l->x::l) ;;

  let left_to_right_associative current_name left_handler separators right_handler name_for_decomposed other_case l=
   match Stdl_list_again.find_pattern_and_remember_opt separators l with 
   None -> cons_opt current_name (other_case l)
   |Some (before,_,after) -> 
     if ((left_handler before)<>None)&&
        ((right_handler after)<>None)
     then Some [current_name;name_for_decomposed]
     else None ;;

  let right_to_left_associative current_name left_handler separators right_handler name_for_decomposed other_case l=
   match Stdl_list_again.find_pattern_and_remember_opt separators l with 
   None -> cons_opt current_name (other_case l)
   |Some (before,_,after) -> 
     if ((left_handler before)<>None)&&
        ((right_handler after)<>None)
     then Some [current_name;name_for_decomposed]
     else None ;;

  let just_one_token name tok l =
    if List.length(l)<>1 then None else 
    let h = List.hd l in 
    if h= tok then Some [name] else None ;;

  let rec recognize_StarredMolecularDot_Identifier l =
  match l with 
  [] -> true 
  |tok1 :: others1 ->
    if tok1<>T.DOT_T then false else 
    match others1 with 
     [] -> false 
    |tok2 :: others2 -> 
      if tok2<>T.IDENTIFIER_T then false else 
      recognize_StarredMolecularDot_Identifier others2 ;;

let recognize_ExpressionName l =
  match l with 
  [] -> false 
  |tok1 :: others1 ->
    if tok1<>T.IDENTIFIER_T then false else
    recognize_StarredMolecularDot_Identifier others1 ;;

     let ladder_for_IntegerLiteral_opt  = just_one_token "IntegerLiteral" T.INTEGER_LITERAL_T ;;
     let ladder_for_FloatingPointLiteral_opt  = just_one_token "FloatingPointLiteral" T.FLOATING_POINT_LITERAL_T ;;
     let ladder_for_BooleanLiteral_opt  = just_one_token "BooleanLiteral" T.BOOLEAN_LITERAL_T ;;
     let ladder_for_CharacterLiteral_opt  = just_one_token "CharacterLiteral" T.CHARACTER_LITERAL_T ;;
     let ladder_for_StringLiteral_opt  = just_one_token "StringLiteral" T.STRING_LITERAL_T ;;
     let ladder_for_TextBlock_opt  = just_one_token "TextBlock" T.TEXT_BLOCK_T ;;
     let ladder_for_NullLiteral_opt  = just_one_token "NullLiteral" T.NULL_LITERAL_T ;;  
    
   
     let ladder_for_Literal_opt l = cons_opt "Literal" (List.find_map (
     fun f-> f l
   ) [
     ladder_for_IntegerLiteral_opt;
     ladder_for_FloatingPointLiteral_opt;
     ladder_for_BooleanLiteral_opt;
     ladder_for_CharacterLiteral_opt;
     ladder_for_StringLiteral_opt;
     ladder_for_TextBlock_opt;
     ladder_for_NullLiteral_opt;
   ] );;   

     let ladder_for_ClassLiteral_opt _l  = None ;;
     let ladder_for_This_opt  = just_one_token "This" T.THIS_T ;;
     let ladder_for_UsingThisPrimaryNoNewArray_opt _l  = None ;;
     let ladder_for_ParenthesedPrimaryNoNewArray_opt _l  = None ;;
     let ladder_for_ClassInstanceCreationExpression_opt _l  = None ;;
     let ladder_for_FieldAccess_opt _l  = None ;;
     let ladder_for_ArrayAccess_opt _l  = None ;;
     let ladder_for_MethodInvocation_opt _l  = None ;;
     let ladder_for_MethodReference_opt _l  = None ;;


   let ladder_for_PrimaryNoNewArray_opt l = cons_opt "PrimaryNoNewArray" (List.find_map (
     fun f-> f l
   ) [
     ladder_for_Literal_opt;
     ladder_for_ClassLiteral_opt;
     ladder_for_This_opt;
     ladder_for_UsingThisPrimaryNoNewArray_opt;
     ladder_for_ParenthesedPrimaryNoNewArray_opt;
     ladder_for_ClassInstanceCreationExpression_opt;
     ladder_for_FieldAccess_opt;
     ladder_for_ArrayAccess_opt;
     ladder_for_MethodInvocation_opt;
     ladder_for_MethodReference_opt;
   ] );; 
     
   let ladder_for_ArrayCreationExpression_opt _l = None ;; 

   let ladder_for_Primary_opt l = cons_opt "Primary" (List.find_map (
     fun f-> f l
   ) [
     ladder_for_PrimaryNoNewArray_opt;
     ladder_for_ArrayCreationExpression_opt;
   ] );; 
   let ladder_for_ExpressionName_opt l = 
      if recognize_ExpressionName l 
      then Some["ExpressionName"]
      else None      ;;
   let ladder_for_PostIncrementExpression_opt _l = None ;;
     
   let ladder_for_PostDecrementExpression_opt _l = None ;; 

   let ladder_for_PostfixExpression_opt l= cons_opt "PostfixExpression" (List.find_map (
     fun f-> f l
   ) [
     ladder_for_Primary_opt;
     ladder_for_ExpressionName_opt;
     ladder_for_PostIncrementExpression_opt;
     ladder_for_PostDecrementExpression_opt;
   ] );; 

   let ladder_for_ParenthesedUnaryExpressionNotPlusMinus_opt _l = None ;; 

   let ladder_for_CastExpression_opt _l = None ;; 

   let ladder_for_UnaryExpressionIII_opt _l = None ;; 

   let ladder_for_UnaryExpressionNotPlusMinus_opt l=match l with 
    [] -> None 
    |toktype :: other_toktypes ->
      if toktype = T.COMPL_T then ladder_for_UnaryExpressionIII_opt other_toktypes else 
      if toktype = T.NOT_T then ladder_for_UnaryExpressionIII_opt other_toktypes else   
      if toktype = T.LP_T then cons_opt "UnaryExpressionNotPlusMinus" (ladder_for_CastExpression_opt l) else  
      if toktype = T.SWITCH_T then cons_opt "UnaryExpressionNotPlusMinus" (ladder_for_ParenthesedUnaryExpressionNotPlusMinus_opt l) else   
      (*        
      match toktype with 
      T.COMPL_T -> ladder_for_UnaryExpressionIII_opt other_toktypes
      |T.NOT_T -> ladder_for_UnaryExpressionIII_opt other_toktypes 
      |T.LP_T -> cons_opt "UnaryExpressionNotPlusMinus" (ladder_for_CastExpression_opt l)
      |T.SWITCH_T -> cons_opt "UnaryExpressionNotPlusMinus" (ladder_for_ParenthesedUnaryExpressionNotPlusMinus_opt l)

      |T.ABSTRACT_T|T.AND_T|T.AND_AND_T|T.ASSERT_T|T.BOOLEAN_T|T.BREAK_T|T.BYTE_T|T.CASE_T|T.CATCH_T|T.CHAR_T|T.CLASS_T|T.CM_T
|T.COLON_T|T.COMMENT_T|T.COND_T|T.CONTINUE_T|T.DECR_T|T.DEFAULT_T|T.DIV_T|T.DO_T|T.DOT_T|T.DOUBLE_T|T.ELSE_T
|T.ENUM_T|T.EQ_T|T.EQ_EQ_T|T.EXPORTS_T|T.EXTENDS_T|T.FINAL_T|T.FINALLY_T|T.FLOAT_T|T.FOR_T|T.GE_T|T.GT_T|T.IDENTIFIER_T
|T.IF_T|T.IMPLEMENTS_T|T.IMPORT_T|T.INCR_T|T.INSTANCEOF_T|T.INT_T|T.INTERFACE_T|T.LB_T|T.LC_T|T.LE_T|T.LINEBREAK_T
|T.LONG_T|T.LS_T|T.LT_T|T.MINUS_T|T.MOD_T|T.MODULE_T|T.NATIVE_T|T.NEW_T|T.NONSEALED_T|T.NOT_EQ_T|T.OPEN_T
|T.OPENS_T|T.OPERATOR_EQ_T|T.OR_T|T.OR_OR_T|T.PACKAGE_T|T.PERMITS_T|T.PLUS_T|T.PRIVATE_T|T.PROTECTED_T|T.PROVIDES_T
|T.PUBLIC_T|T.RB_T|T.RC_T|T.RECORD_T|T.REQUIRES_T|T.RETURN_T|T.RP_T|T.SEALED_T|T.SHORT_T|T.SM_T|T.SNAIL_T|T.SRS_T|T.STATIC_T
|T.STRICTFP_T|T.SUPER_T|T.SYNCHRONIZED_T|T.THIS_T|T.THROW_T|T.THROWS_T|T.TIMES_T|T.TO_T|T.TRANSIENT_T
|T.TRANSITIVE_T|T.TRY_T|T.URS_T|T.USES_T|T.VAR_T|T.VOID_T|T.VOLATILE_T|T.WHILE_T|T.WHITESPACE_T|T.WITH_T|T.XOR_T|T.YIELD_T ->
     
   *)
   cons_opt "UnaryExpressionNotPlusMinus" (ladder_for_PostfixExpression_opt l)  ;;


   let ladder_for_UnaryExpressionII_opt _l = None ;; 

   let ladder_for_UnaryExpression_opt l=match l with 
    [] -> None 
    |toktype :: other_toktypes ->
      if List.mem toktype [T.DECR_T;T.INCR_T;T.MINUS_T;T.PLUS_T]
      then ladder_for_UnaryExpressionII_opt other_toktypes 
      else cons_opt "UnaryExpression" (ladder_for_UnaryExpressionNotPlusMinus_opt l)  ;;

   let ladder_for_MultiplicativeExpressionII_opt _l = None ;; 

    let ladder_for_MultiplicativeExpression_opt =
   left_to_right_associative 
    "MultiplicativeExpression" 
     ladder_for_MultiplicativeExpressionII_opt [[T.DIV_T];[T.MOD_T];[T.TIMES_T]] ladder_for_UnaryExpression_opt 
      "UnaryExpression"  
       ladder_for_UnaryExpression_opt  ;;



   let ladder_for_AdditiveExpressionII_opt _l = None ;; 

   let ladder_for_AdditiveExpression_opt =
   left_to_right_associative 
    "AdditiveExpression" 
     ladder_for_AdditiveExpressionII_opt [[T.MINUS_T];[T.PLUS_T]] ladder_for_MultiplicativeExpression_opt 
      "MultiplicativeExpression"  
       ladder_for_MultiplicativeExpression_opt  ;; 

   let ladder_for_ShiftExpressionII_opt _l = None ;; 

   let ladder_for_ShiftExpression_opt =
   left_to_right_associative 
    "ShiftExpression" 
     ladder_for_ShiftExpressionII_opt [[T.LS_T];[T.SRS_T];[T.URS_T];] ladder_for_AdditiveExpression_opt 
      "AdditiveExpression"  
       ladder_for_AdditiveExpression_opt  ;;

   let ladder_for_RelationalExpressionII_opt _l = None ;; 

   let ladder_for_RelationalExpression_opt =
   left_to_right_associative 
    "RelationalExpression" 
     ladder_for_RelationalExpressionII_opt [[T.LE_T];[T.LT_T];[T.GE_T];[T.GT_T];[T.INSTANCEOF_T]] ladder_for_ShiftExpression_opt 
      "ShiftExpression"  
       ladder_for_ShiftExpression_opt  ;;

   let ladder_for_EqualityExpressionII_opt _l = None ;;  

   let ladder_for_EqualityExpression_opt =
   left_to_right_associative 
    "EqualityExpression" 
     ladder_for_EqualityExpressionII_opt [[T.EQ_EQ_T];[T.NOT_EQ_T]] ladder_for_RelationalExpression_opt 
      "ConditionalAndExpression"  
       ladder_for_RelationalExpression_opt  ;;

   let ladder_for_AndExpressionII_opt _l = None ;; 

   let ladder_for_AndExpression_opt =
   left_to_right_associative 
    "AndExpression" 
     ladder_for_AndExpressionII_opt [[T.AND_T]] ladder_for_EqualityExpression_opt 
      "EqualityExpression"  
       ladder_for_EqualityExpression_opt  ;;

   let ladder_for_ExclusiveExpressionII_opt _l = None ;; 

   let ladder_for_ExclusiveOrExpression_opt =
   left_to_right_associative 
    "ExclusiveOrExpression" 
     ladder_for_ExclusiveExpressionII_opt [[T.XOR_T]] ladder_for_AndExpression_opt 
      "ConditionalAndExpression"  
       ladder_for_AndExpression_opt  ;;

  let ladder_for_InclusiveExpressionII_opt _l = None ;; 

  let ladder_for_InclusiveOrExpression_opt =
   left_to_right_associative 
    "InclusiveOrExpression" 
     ladder_for_InclusiveExpressionII_opt [[T.OR_T]] ladder_for_ExclusiveOrExpression_opt 
      "ExclusiveOrExpression"  
       ladder_for_ExclusiveOrExpression_opt  ;;

  let ladder_for_ConditionalAndExpressionII_opt _l = None ;; 
    
  let ladder_for_ConditionalAndExpression_opt =
   left_to_right_associative 
    "ConditionalAndExpression" 
     ladder_for_ConditionalAndExpressionII_opt [[T.AND_AND_T]] ladder_for_InclusiveOrExpression_opt 
      "ConditionalAndExpression"  
       ladder_for_InclusiveOrExpression_opt  ;;
    
  let ladder_for_ConditionalOrExpressionII_opt _l = None ;;

  let  ladder_for_ConditionalOrExpression_opt =
   left_to_right_associative 
    "ConditionalOrExpression" 
     ladder_for_ConditionalOrExpressionII_opt [[T.OR_OR_T]] ladder_for_ConditionalAndExpression_opt 
      "ConditionalAndExpression"  
       ladder_for_ConditionalAndExpression_opt  ;;

  let ladder_for_ExpressionIII_opt _l = None ;;

  let ladder_for_ConditionalExpressionII_opt _l = None ;;

  let ladder_for_LambdaExpression_opt _l = None ;;

 let ladder_for_ConditionalExpression_opt  l=
 let current_name = "ConditionalExpression" in
   match Stdl_list_again.find_interval_sublist_and_remember_opt [T.COND_T] l with 
   None -> cons_opt current_name (ladder_for_ConditionalOrExpression_opt l)
   |Some (before1,after1) ->
      match Stdl_list_again.find_interval_sublist_and_remember_opt [T.COLON_T] after1 with 
       None -> raise(Malformed_conditional_expression(l))
      |Some (before2,after2) ->   
     if ((ladder_for_ConditionalOrExpression_opt before1)=None)||
        ((ladder_for_ExpressionIII_opt before2)=None)
     then None 
     else 
     if (ladder_for_ConditionalExpressionII_opt after2)<>None 
     then Some [current_name;"LambdalessConditionalAndExpressionConditionalExpression"]
     else 
      if (ladder_for_LambdaExpression_opt after2)<>None 
     then Some [current_name;"LambdafulConditionalAndExpressionConditionalExpression"]
     else None ;;

let ladder_for_LeftHandSide_opt _l = None ;;   

let ladder_for_ExpressionII_opt _l = None ;;  


let ladder_for_AssignmentExpression_opt =
   right_to_left_associative 
    "AssignmentExpression" 
     ladder_for_LeftHandSide_opt [[T.OPERATOR_EQ_T]] ladder_for_ExpressionII_opt 
      "Assignment"  
       ladder_for_ConditionalExpression_opt ;;


let ladder_for_LambdaParameters_opt _l = None ;;   

let ladder_for_LambdaBody_opt _l = None ;;  

let ladder_for_Expression_opt =
   right_to_left_associative 
    "Expression" 
     ladder_for_LambdaParameters_opt [[T.MINUS_T;T.GT_T]] ladder_for_LambdaBody_opt 
      "LambdaExpression"  
       ladder_for_AssignmentExpression_opt ;;
   
  
    
let initial_Expression_followed_by_Semicolon_opt abstract_l =
   let l = Stdl_jvsp_token_types_list.unveil abstract_l in 
   match Stdl_list_again.find_and_remember_opt (fun tok->tok=T.SM_T) l with 
   None -> None
   |(Some(start,_,_)) -> 
    Option.map (Stdl_image.image Stdl_jvng_duplicated_name.of_string)
    (ladder_for_Expression_opt start) ;;


let example =   Stdl_jvng_disjunction_ladder_finder.make [
   initial_Expression_followed_by_Semicolon_opt
] ;;

end ;;

let example = Private.example ;;

end;;

