(*

#directory "+str";;
#load "str.cma";;
#directory "+unix";;
#load "unix.cma";;
#use"Teuliou/OCaml/skeptical_duck/watched/watched_and_githubbed/Standalone_old_files/standalone_lr_parsing.ml";;

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






module Stdl_cartesian=struct

(*

#use"lib/cartesian.ml";;

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

let power x n=general_product (Stdl_int_range.scale (fun _j->x) 1 n);;
             

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






module Stdl_lrp_types=struct

(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_types.ml";;

*)

type production = Prod of string * string list ;;

type grammar = {
  grammar_serial_number : int ;
  productions : production list ; 
} ;;

type item = Item of string * (string list) ;;

type action = 
   Shift of int 
  |Reduce of production 
  |Accept ;;

type lr_pre_table = {
   action_pre_data :  ((int * string) * (action list)) list ;
   goto_pre_data :    ((int * string) * (int list)) list ;
}  ;; 

type lr_table = {
   table_serial_number : int ;
   action_data :  (int * ((string * action) list)) list ;
   goto_data :  (int * ((string * int) list)) list ;
}  ;; 






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






module Stdl_lrp_item=struct

(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_item.ml";;

*)

open Stdl_lrp_types ;;

module Private = struct 

let production_from_item (Item(p,l)) = Prod(p,List.filter(fun x->x<>".") l) ;;   

let index_of_dot (Item(_p,l))= Stdl_list_again.index_of_in "." l;;



let push_dot_one_symbol symb (Item(p,l))=
  let n = List.length l 
  and j = Stdl_list_again.index_of_in "." l in
  if j=n then None else 
  if (List.nth l j)<>symb 
  then None 
  else
  let (head,tail)=Stdl_list_again.cut (j-1) l in 
  Some(Item(p,head@(symb::"."::(List.tl(List.tl tail)))))
  ;;

(*

push_dot_one_symbol "1" (Item("a",[".";"1";"2";"3";"4";"5";"6"])) ;;
push_dot_one_symbol "3" (Item("a",["1";"2";".";"3";"4";"5";"6"])) ;;
push_dot_one_symbol "6" (Item("a",["1";"2";"3";"4";"5";".";"6"])) ;;

*)  

let description_for_item (Item(p,l)) = p ^ " -> " ^ (String.concat "" l) ;;

let description_for_items items=
    String.concat "\n" (Stdl_image.image (fun item->(String.make 3 ' ')^(description_for_item item)) items) ;;

let description_for_indexed_item_set (idx,items)=
 (string_of_int idx)^": \n\n"^(description_for_items items) ;;
 
 let description_for_indexed_item_sets l = 
   "\n\n\n"^(String.concat "\n\n\n" (Stdl_image.image description_for_indexed_item_set l)) ;;
   

end ;;  

let almost_finished_production_opt (Item(p,l)) =
  match List.rev l with 
  [] -> None 
  |head_of_rev::tail_of_rev ->
    if head_of_rev <> "."
    then None  
    else Some(Prod(p,List.rev tail_of_rev)) ;; 
   
let display_indexed_item_sets l = 
  print_string(Private.description_for_indexed_item_sets l);;

let first_item_from_production (Prod(p,l)) = Item(p,"."::l);;

let index_of_dot = Private.index_of_dot ;;   

let items_from_production (Prod(p,l)) =
  let temp = Stdl_two_winged_bird_on_plank.generic l in 
  let temp2 = List.rev_map (fun (left,center,right)->(List.rev left,center::right)) temp in 
  (Stdl_image.image (fun (left,right)->Item(p,left@("."::right))) temp2)@[Item(p,l@["."])] ;; 

(* items_from_production (Prod("x",["1";"2";"3";"4"])) ;; *) 

let last_item_from_production (Prod(p,l)) = Item(p,l@["."]);;

let production_from_item = Private.production_from_item ;;
let push_dot_one_symbol = Private.push_dot_one_symbol ;;




let symbol_after_dot_opt (Item(_,l)) =
    let (_,_,right) = 
      Option.get(Stdl_list_again.find_and_remember_opt (fun s->s=".") l) in 
      if right=[]
      then None  
      else Some(List.hd right) ;;

(*


symbol_after_dot (Item("a",["1";"2";".";"3";"4";"5";"6"])) ;;


*)  

let visualize (Item(p,l)) = String.concat "" (p::" -> "::l) ;;

end;;






module Stdl_lrp_constant=struct

(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_constant.ml";;

*)

let end_marker = "Endmarker" ;;






end;;






module Stdl_lrp_grammar=struct

(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_grammar.ml";;

*)

open Stdl_lrp_types ;;
open Stdl_lrp_constant ;;

module Private = struct 

let str_order = Stdl_total_ordering.lex_for_strings ;;
let str_fold_merge = Stdl_ordered.fold_merge str_order ;;
let str_insert = Stdl_ordered.insert str_order ;;
let str_intersect = Stdl_ordered.intersect str_order ;; 
let str_mem = Stdl_ordered.mem str_order ;; 
let str_merge = Stdl_ordered.merge str_order ;; 
let str_setminus = Stdl_ordered.setminus str_order ;; 
let str_sort = Stdl_ordered.sort str_order ;; 

let productions gram = gram.productions ;;

let instance_counter = ref(0) ;;

let make_from_prods prods = 
  let new_val = (!instance_counter)+1 in 
  let _ = (instance_counter:=new_val) in 
  {
     grammar_serial_number = new_val ;
     productions =  prods ; 
  } ;;

let make prods = 
  make_from_prods (Stdl_image.image (fun (a,b)->Prod(a,b)) prods) ;;


let memoize_constant hashtbl f gram =
   match Hashtbl.find_opt hashtbl gram.grammar_serial_number with 
   Some old_answer -> old_answer 
   | None -> 
     let answer = f gram in 
     let _ = Hashtbl.replace hashtbl gram.grammar_serial_number answer in 
     answer ;;

let memoize_univar hashtbl f gram var1=
   match Hashtbl.find_opt hashtbl (gram.grammar_serial_number,var1) with 
   Some old_answer -> old_answer 
   | None -> 
     let answer = f gram var1 in 
     let _ = Hashtbl.replace hashtbl (gram.grammar_serial_number,var1) answer in 
     answer ;;



let compute_nonterminals_naively gram= 
  str_sort (Stdl_image.image (fun (Prod(a,_))->a) (productions gram)) ;;

let hashtbl_for_nonterminals = Hashtbl.create 100;;

let nonterminals = memoize_constant hashtbl_for_nonterminals compute_nonterminals_naively ;;

let compute_terminals_naively gram= 
   let l = productions gram in 
   let temp = str_sort (List.flatten(Stdl_image.image (fun (Prod(_,b))->b) l)) in 
   str_setminus temp (nonterminals gram) ;; ;;

let hashtbl_for_terminals = Hashtbl.create 100;;

let terminals = memoize_constant hashtbl_for_terminals compute_terminals_naively ;; 

let compute_all_symbols_naively gram= str_merge (terminals gram)  (nonterminals gram);; 

let hashtbl_for_all_symbols = Hashtbl.create 100;;

let all_symbols = memoize_constant hashtbl_for_all_symbols compute_all_symbols_naively ;; 

let rename_in_symbol (old_name,new_name) symb = if symb = old_name then new_name else symb ;;

let rename_in_production rep (Prod(a,b)) = 
  let r = rename_in_symbol rep in (Prod(r a,Stdl_image.image r b)) ;;

let rename_in_grammar rep gram =
  let old_prods = productions gram in 
  let new_prods = Stdl_image.image (rename_in_production rep) old_prods in 
  make_from_prods new_prods ;; 

let start_symbol gram= let (Prod(s,_)) = List.hd (productions gram) in s ;; 

let augment ~earlier_start ~new_name_for_old_start old_gram=
  let old_start = start_symbol old_gram in 
  let new_gram = rename_in_grammar (old_start,new_name_for_old_start) old_gram in 
  let prods1 = productions new_gram in 
  make_from_prods((Prod(earlier_start,[new_name_for_old_start]))::prods1) ;;

let index_of_production_in_grammar item gram =
  let prods = productions gram in 
  Stdl_list_again.index_of_in (Stdl_lrp_item.production_from_item item) prods ;;

let order_on_items gram = ((fun item1 item2 ->
  let trial1 = Stdl_total_ordering.for_integers (Stdl_lrp_item.index_of_dot item1) (Stdl_lrp_item.index_of_dot item2) in 
  if trial1<> Stdl_total_ordering_result_t.Equal then trial1 else 
  Stdl_total_ordering.for_integers 
    (index_of_production_in_grammar item1 gram) 
      (index_of_production_in_grammar item2 gram)  
): item Stdl_total_ordering_t.t);;       

module Emptiable_nonterminals = struct

let initial_data gram =
   let prods1 = productions gram in 
   let prods2= Stdl_image.image (fun (Prod(h,l))->(h,str_sort l)) prods1 in
   let unordered_nonterminals = Stdl_image.image fst prods2 in 
   let nonterminals = Stdl_ordered.to_list(str_sort unordered_nonterminals) in 
   Stdl_image.image (fun h->(h,List.assoc h prods2)) nonterminals ;; 


let pusher (simplified_productions,level) =
    let next_simplified_productions = Stdl_image.image (fun (h,l)->
        (h,str_setminus l level)
      ) simplified_productions in 
    let next_level = List.filter_map(fun (h,l)->if Stdl_ordered.to_list l=[] then Some h else None) next_simplified_productions in 
    (next_simplified_productions,str_sort next_level)  ;;

let rec iterator (preceding_pair,pair) =
   let (_,preceding_level) = preceding_pair 
   and (_,level) = pair in 
   if List.length (Stdl_ordered.to_list preceding_level) = List.length (Stdl_ordered.to_list preceding_level)
   then level 
   else iterator(pair,pusher pair) ;;   

let compute_emptiable_nonterminals_naively gram = 
   let first_pair = (initial_data gram,str_sort []) in 
   let final_level = iterator (first_pair,pusher first_pair) in 
   final_level ;;

let hashtbl_for_emptiable_nonterminals = Hashtbl.create 100;;

let emptiable_nonterminals = memoize_constant hashtbl_for_emptiable_nonterminals compute_emptiable_nonterminals_naively ;; 

let compute_is_emptiable_naively gram symb=str_mem symb (emptiable_nonterminals gram);; 
    

let hashtbl_for_is_emptiable = Hashtbl.create 100;;

let is_emptiable = memoize_univar hashtbl_for_is_emptiable compute_is_emptiable_naively ;; 

end ;;

module Leftmost_descendants = struct

(* Note that a non-terminal is always a leftmost descendant of itself *)   
   
let elements_having_a_wholly_emptiable_subform_on_their_left_side gram form =
   let rec tempf = (
     fun (treated,to_be_treated) -> 
      match to_be_treated with 
      [] -> List.rev(treated)
      |symb::other_symbs ->
         if Emptiable_nonterminals.is_emptiable gram symb 
         then tempf(symb::treated,other_symbs)
         else List.rev(symb::treated)  
   ) in 
   tempf([],form) ;;    
   
let direct_leftmost_descendants gram symb =
    let prods =productions gram in 
    let unordered_descendants = List.flatten(Stdl_image.image (fun (Prod(a,b))->
      if a<>symb then [] else
      elements_having_a_wholly_emptiable_subform_on_their_left_side gram b) prods) in 
    str_sort (symb::unordered_descendants);;

let direct_leftmost_descendants_for_several gram symbs =
  str_fold_merge (Stdl_image.image (direct_leftmost_descendants gram) symbs) ;;

let rec helper gram (current_whole,to_be_treated) =
   let possibly_new = direct_leftmost_descendants_for_several gram (Stdl_ordered.to_list to_be_treated) in 
   let really_new = str_setminus possibly_new current_whole in 
   if Stdl_ordered.to_list really_new = []
   then current_whole 
   else helper gram (str_merge current_whole really_new,really_new) ;;   

let compute_leftmost_descendants_naively gram symb = helper gram (str_sort[],str_sort[symb]) ;;

let hashtbl_for_leftmost_descendants = Hashtbl.create 100;;

let leftmost_descendants = memoize_univar hashtbl_for_leftmost_descendants compute_leftmost_descendants_naively ;; 


end ;;   

module Furst_set = struct 

(*

We compute so-called "FIRST" sets (here renamed "Furst" sets for convenience) 

*)



let hashtbl_for_furst_set = Hashtbl.create 100;;


  
let compute_furst_set_naively gram symb= 
  let termies = str_insert end_marker (terminals gram) in 
  str_intersect termies (Leftmost_descendants.leftmost_descendants gram symb) ;;

let furst_set_for_symbol = memoize_univar hashtbl_for_furst_set compute_furst_set_naively ;; 

let furst_set_for_form gram form = 
    let usable_symbols = 
      Leftmost_descendants.elements_having_a_wholly_emptiable_subform_on_their_left_side gram form in 
    str_fold_merge (Stdl_image.image (furst_set_for_symbol gram) usable_symbols)  ;;



end ;;  

module Rightmost_ancestors = struct 

let direct_rightmost_ancestors gram symb = 
   let productions = productions gram in 
   str_sort(List.filter_map (fun (Prod(a,b))->if List.hd(List.rev b)=symb then Some a else None)  productions) ;;

let direct_rightmost_ancestors_for_several gram symbs =
  str_fold_merge (Stdl_image.image (direct_rightmost_ancestors gram) symbs) ;;

let rec helper gram (current_whole,to_be_treated) =
   let possibly_new = direct_rightmost_ancestors_for_several gram (Stdl_ordered.to_list to_be_treated) in 
   let really_new = str_setminus possibly_new current_whole in 
   if Stdl_ordered.to_list really_new = []
   then current_whole 
   else helper gram (str_merge current_whole really_new,really_new) ;;   

let compute_rightmost_ancestors_naively gram symb = helper gram (str_sort [],str_sort [symb]) ;;

let hashtbl_for_rightmost_ancestors = Hashtbl.create 100;;

let rightmost_ancestors = memoize_univar hashtbl_for_rightmost_ancestors compute_rightmost_ancestors_naively ;; 

end ;;   

module Follow_set = struct 

   let completions_on_the_right_for_in_production symb (Prod(_a,b)) =
      let temp1 = Stdl_two_winged_bird_on_plank.generic b in 
      List.filter_map (
        fun (_rev_left,center,right) ->
          if center <> symb then None else 
          Some right   
      ) temp1 ;;

    let completions_on_the_right_for_in_productions symb productions =
     List.flatten (Stdl_image.image (completions_on_the_right_for_in_production symb) productions) ;;   


let direct_follow_set gram symb =
   let prods = productions gram in 
   let completions = completions_on_the_right_for_in_productions symb prods in 
   let (empty_completions,nonempty_completions) = List.partition (fun x->x=[]) completions in
   let rightmost_contribution = (if empty_completions=[] then [] else [end_marker]) in 
   str_fold_merge ((str_sort rightmost_contribution)::
   (Stdl_image.image (Furst_set.furst_set_for_form gram) nonempty_completions)) ;;

let compute_follow_set_naively gram symb =
   str_fold_merge (Stdl_image.image (direct_follow_set gram)
    (symb::(Stdl_ordered.to_list(Rightmost_ancestors.rightmost_ancestors gram symb)))) ;;

let hashtbl_for_follow_set = Hashtbl.create 100;;

let follow_set = memoize_univar hashtbl_for_follow_set compute_follow_set_naively ;; 

end ;;   


end ;;

let all_symbols = Private.all_symbols ;;
let augment = Private.augment ;;

let first_production gram = List.hd(Private.productions gram);;
let follow_set = Private.Follow_set.follow_set ;;

let furst_set_for_form = Private.Furst_set.furst_set_for_form ;;

let furst_set_for_symbol = Private.Furst_set.furst_set_for_symbol ;;

let is_emptiable = Private.Emptiable_nonterminals.is_emptiable ;;

let items gram =
  let prods = Private.productions gram in 
  let unordered = List.flatten (Stdl_image.image Stdl_lrp_item.items_from_production prods) in 
  Stdl_ordered.sort (Private.order_on_items gram) unordered ;;

let leftmost_descendants = Private.Leftmost_descendants.leftmost_descendants ;;

let make = Private.make ;;  
let make_from_prods = Private.make_from_prods ;;  
let nonterminals = Private.nonterminals ;;

let order_on_items = Private.order_on_items ;;
let productions = Private.productions ;;

let rightmost_ancestors = Private.Rightmost_ancestors.rightmost_ancestors ;;

let sons gram father = List.filter_map (fun (Prod(a,b))->if a=father then Some b else None ) (productions gram) ;;
let start_symbol = Private.start_symbol ;; 
let terminals = Private.terminals ;;

end;;






module Stdl_lrp_pretty_printing=struct

(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_pretty_printing.ml";;

*)

open Stdl_lrp_types ;;

module Private = struct 

let on_index names_for_states idx0=
 snd(List.find (fun ((idx,_),_)->idx=idx0) names_for_states) ;;   

let on_action names_for_states = function 
  (Shift j)->  on_index names_for_states j 
  |Reduce(Prod(a,b)) -> a^" -> "^(String.concat "" b) 
  |Accept -> "Accept" ;;

let on_action_transition names_for_states (symb,act) = 
   symb^"\226\165\184 \226\157\170 "^(on_action names_for_states act)^" \226\157\171";;

let on_action_transitions names_for_states l = 
    String.concat "   " (Stdl_image.image (on_action_transition names_for_states) l);;   

let on_action_indexed_pair names_for_states (idx,transitions) = 
   (on_index names_for_states idx)^" : "^(on_action_transitions names_for_states transitions);;

let on_action_data names_for_states l =
   "Action table :\n\n"^
   (String.concat "\n" (Stdl_image.image (on_action_indexed_pair names_for_states) l))
  ;;  

let on_goto_transition names_for_states (symb,state) = 
   symb^"\226\165\184 \226\157\170 "^(on_index names_for_states state)^" \226\157\171";;

let on_goto_transitions names_for_states l = 
    String.concat "   " (Stdl_image.image (on_goto_transition names_for_states) l);;   

let on_goto_indexed_pair names_for_states (idx,transitions) = 
   (on_index names_for_states idx)^" : "^(on_goto_transitions names_for_states transitions);;

let on_goto_data names_for_states l =
   let effective_l = List.filter (fun (_idx,transitions)->transitions<>[]) l in 
   "Goto table :\n\n"^
   (String.concat "\n" (Stdl_image.image (on_goto_indexed_pair names_for_states) effective_l))
  ;;  

let on_table names_for_states tbl =
   (on_action_data names_for_states tbl.action_data)^
   "\n\n\n"^
   (on_goto_data names_for_states tbl.goto_data)^
   "\n\n\n" ;; 
 
let on_action_conflict names_for_states ((idx,mover),acts) =
    (on_index names_for_states idx)^" \226\157\159 "^mover^" \226\165\184 "^
    (String.concat " \194\166 "(Stdl_image.image (on_action names_for_states) acts));;

let on_action_conflicts names_for_states conflicts =
   if conflicts = [] then "" else 
   "Action conflicts : \n\n"^
    (String.concat "\n"(Stdl_image.image (on_action_conflict names_for_states) conflicts));;    

let on_goto_conflict names_for_states ((idx,mover),destinations) =
    (on_index names_for_states idx)^" \226\157\159 "^mover^" \226\165\184 "^
    (String.concat " \194\166 "(Stdl_image.image (on_index names_for_states) destinations));;

let on_goto_conflicts names_for_states conflicts =
   if conflicts = [] then "" else 
   "Goto conflicts : \n\n"^
    (String.concat "\n"(Stdl_image.image (on_goto_conflict names_for_states) conflicts));;    

let on_both_conflicts names_for_states (action_conflicts,goto_conflicts) =
   let between = (if (action_conflicts<>[])&&(goto_conflicts<>[]) then "\n\n\n" else "") in  
   (on_action_conflicts names_for_states action_conflicts)^between^
   (on_goto_conflicts names_for_states goto_conflicts)^"\n\n" ;;
      

end ;;   

let on_action = Private.on_action ;;

let on_both_conflicts = Private.on_both_conflicts ;;

let on_index = Private.on_index ;;
let on_table = Private.on_table ;;

end;;






module Stdl_lrp_table=struct

(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_table.ml";;

*)

open Stdl_lrp_types ;;
open Stdl_lrp_constant ;;

exception Goto_error_exn of int * string ;;
exception Transition_error_exn of int * string ;;
exception No_steps_after_acceptance_exn ;;
exception Nothing_to_parse_exn ;;

module Private = struct 

let instance_counter = ref(0) ;;

let make l_action l_goto =
   let new_val = (!instance_counter)+1 in 
   let _ = (instance_counter:=new_val) in    
   {
     table_serial_number = new_val ;
     action_data = l_action ;
     goto_data = l_goto ;
   } ;;

let i_sort = Stdl_ordered.sort Stdl_total_ordering.for_integers ;;  
let str_sort = Stdl_ordered.sort Stdl_total_ordering.lex_for_strings ;;  

let from_pre_table_opt pre_tbl=
   let (bad_actions,good_actions)=List.partition (fun (_pair,actions)->
           List.length(actions)<>1
   ) pre_tbl.action_pre_data 
   and (bad_gotos,good_gotos)=List.partition (fun (_pair,gotos)->
           List.length(gotos)<>1
   ) pre_tbl.goto_pre_data in 
   if (bad_actions<>[])||(bad_gotos<>[])
   then (None,Some(bad_actions,bad_gotos))
   else   
   let actions1 = Stdl_image.image (fun (pair,actions)->(pair,List.hd actions)) good_actions 
   and gotos1 = Stdl_image.image (fun (pair,actions)->(pair,List.hd actions)) good_gotos in 
   let indices_from_actions = i_sort(Stdl_image.image (fun ((idx,_),_) -> idx) actions1) 
   and indices_from_gotos = i_sort(Stdl_image.image (fun ((idx,_),_) -> idx) gotos1) in 
   let action_data = Stdl_ordered.image (
     fun idx -> 
      let pairs = List.filter_map(fun ((idx2,mover),act) -> if idx2=idx then Some(mover,act) else None ) actions1 in
      let movers =  str_sort(Stdl_image.image fst pairs) in 
      (idx,Stdl_ordered.image (fun mover ->(mover,List.assoc mover pairs)) movers)
   ) indices_from_actions 
   and goto_data = Stdl_ordered.image (
     fun idx -> 
      let pairs = List.filter_map(fun ((idx2,mover),dest) -> if idx2=idx then Some(mover,dest) else None ) gotos1 in
      let movers =  str_sort(Stdl_image.image fst pairs) in 
      (idx,Stdl_ordered.image (fun mover ->(mover,List.assoc mover pairs)) movers)
   ) indices_from_gotos 
   in
   (Some(make action_data goto_data),None) ;;   
   

   

let hashtbl_for_actions = Hashtbl.create 100 ;;
 

let compute_action_naively tbl state_idx symb = 
   let temp = List.assoc state_idx tbl.action_data  in 
  List.assoc_opt symb temp ;;

let get_action tbl state_idx symb =
   let wrapped_answer = (
   match Hashtbl.find_opt hashtbl_for_actions (tbl.table_serial_number,state_idx,symb) with 
   Some old_answer -> old_answer 
   |None ->
      let answer = compute_action_naively tbl state_idx symb in 
      let _ = Hashtbl.replace hashtbl_for_actions (tbl.table_serial_number,state_idx,symb) answer in 
      answer 
   ) in 
   match wrapped_answer with 
   None -> raise(Transition_error_exn(state_idx,symb))
   |Some(action)->action ;;

let hashtbl_for_gotos = Hashtbl.create 100 ;;   

let compute_goto_naively tbl state_idx symb = 
   let temp = List.assoc state_idx tbl.goto_data  in 
  List.assoc_opt symb temp ;;

let get_goto tbl state_idx symb =
   let wrapped_answer = (
   match Hashtbl.find_opt hashtbl_for_gotos (tbl.table_serial_number,state_idx,symb) with 
   Some old_answer -> old_answer 
   |None ->
      let answer = compute_goto_naively tbl state_idx symb in 
      let _ = Hashtbl.replace hashtbl_for_gotos (tbl.table_serial_number,state_idx,symb) answer in 
      answer 
   ) in 
   match wrapped_answer with 
   None -> raise(Goto_error_exn(state_idx,symb))
   |Some(goto)->goto ;;

let initial_configuration text_to_be_parsed = ([0],text_to_be_parsed@[end_marker]) ;;  

let compute_next_action tbl (state_stack,symbol_stack) =
   (state_stack,symbol_stack,get_action tbl (List.hd state_stack) (List.hd symbol_stack)) ;;

let initial_configuration tbl text_to_be_parsed = 
   compute_next_action tbl ([0],text_to_be_parsed@[end_marker]) ;;     

let step tbl steps = 
   match steps with 
   [] -> []
   |(state_stack,symbol_stack,next_action)::_ ->
      let next_pair = (
       match next_action with 
      Accept -> raise No_steps_after_acceptance_exn 
      |Shift(j) -> (j::state_stack,List.tl symbol_stack)
      |Reduce(Prod(aa,omega)) ->
         let remaining_stack = List.drop (List.length omega) state_stack in 
         let remaining_head = List.hd remaining_stack in 
         let k = get_goto tbl remaining_head aa in 
         (k::remaining_stack,symbol_stack)

      ) in 
      (compute_next_action tbl next_pair)::steps ;;

let rec iterator tbl steps = 
   match steps with 
   [] -> raise(Nothing_to_parse_exn)
   |(_state_stack,_symbol_stack,next_action)::_ ->
      if next_action = Accept 
      then steps   
      else iterator tbl (step tbl steps) ;;      


end ;;   


let from_pre_table_opt = Private.from_pre_table_opt ;;

let make = Private.make ;;

let parsing_details tbl text_to_be_parsed=    
  Private.iterator tbl [Private.initial_configuration tbl text_to_be_parsed] ;;



end;;






module Stdl_lrp_lr_computations=struct

(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_lr_computations.ml";;

*)

open Stdl_lrp_types ;;
open Stdl_lrp_constant ;;

exception Conflict_in_Lr_parser_exn ;;

module type LRK_SEED  =  
sig
    
    type atom 
    type molecule 
    val atoms_inside : molecule -> atom list
    val empty_one : molecule
    val ender_atom : grammar -> atom
    val immediate_closure : grammar -> atom -> atom Stdl_ordered.ordered_list
    val item_component : atom -> item
    val molecule : atom list -> molecule
    val name : string
    val order_on_atoms : grammar -> atom Stdl_total_ordering_t.t
    val push_dot_one_symbol : string -> atom -> atom option
    val starter_atom : grammar -> atom
    val test_for_allowing_reduction : grammar -> atom -> head_of_production:string -> terminal:string -> bool
    val visualize_atom : atom -> string
end ;;

module Make = functor(Seed:LRK_SEED) -> struct


module Private = struct 

   let str_order = Stdl_total_ordering.lex_for_strings ;;

   let str_fold_merge = Stdl_ordered.fold_merge str_order ;; 
   let str_insert = Stdl_ordered.insert str_order ;; 
   let str_intersect = Stdl_ordered.intersect str_order ;; 
   let str_mem = Stdl_ordered.mem str_order ;; 
   let str_merge = Stdl_ordered.merge str_order ;; 
   let str_setminus = Stdl_ordered.setminus str_order ;; 
   let str_sort = Stdl_ordered.sort str_order ;; 


   module Registration = struct 

   let path_order = Stdl_total_ordering.silex_compare Stdl_total_ordering.silex_for_strings ;;

   let path_merge = Stdl_ordered.merge path_order ;;
   let path_sort = Stdl_ordered.sort path_order ;;

   let hashtbl_for_counting_registered_molecules = Hashtbl.create 100 ;;

   let hashtbl_for_indices = Hashtbl.create 100 ;;
   let hashtbl_for_paths = Hashtbl.create 100 ;;
   
   let number_of_registered_molecules gram =
      match Hashtbl.find_opt hashtbl_for_counting_registered_molecules gram.grammar_serial_number with 
      Some old_count -> old_count 
      |None -> let _ =(Hashtbl.replace hashtbl_for_counting_registered_molecules gram.grammar_serial_number 0) in 
               0 ;;

   let get_index gram molecule=
      match Hashtbl.find_opt hashtbl_for_indices (gram.grammar_serial_number,molecule) with 
       Some old_idx -> old_idx
      |None ->
        let new_idx = (number_of_registered_molecules gram)-1 in 
        let _ = (
         Hashtbl.replace hashtbl_for_counting_registered_molecules gram.grammar_serial_number (new_idx+2);
         Hashtbl.replace hashtbl_for_indices (gram.grammar_serial_number,molecule) new_idx;
         Hashtbl.replace hashtbl_for_paths (gram.grammar_serial_number,new_idx) [];
      ) in  
      new_idx ;;      
      
   let get_paths gram molecule=
      let idx = get_index gram molecule in 
      Hashtbl.find hashtbl_for_paths (gram.grammar_serial_number,idx) ;;

   let add_paths gram molecule paths_to_be_added =
       let idx = get_index gram molecule in 
       let old_paths = Hashtbl.find hashtbl_for_paths (gram.grammar_serial_number,idx) in 
       let new_paths = path_merge (path_sort old_paths) (path_sort paths_to_be_added) in 
       Hashtbl.replace hashtbl_for_paths (gram.grammar_serial_number,idx) (Stdl_ordered.to_list new_paths) ;;
     

   end ;;   
   

   let add_new_paths_to_lrk_molecule gram lrk_molecule paths_to_be_added =
     Registration.add_paths gram lrk_molecule paths_to_be_added ;;

   let register_lrk_molecule gram lrk_molecule = 
     let idx = Registration.get_index gram lrk_molecule in 
     (idx,lrk_molecule);; 

   let immediate_closure_for_several gram atoms = 
      Stdl_ordered.fold_merge (Seed.order_on_atoms gram) 
      (Stdl_image.image (fun atm->
      (Seed.immediate_closure gram atm)) atoms) ;; 

   let rec towards_closure gram (whole,_treated,to_be_treated) = 
    let l_to_be_treated = Stdl_ordered.to_list to_be_treated in 
    if l_to_be_treated = [] then Seed.molecule(Stdl_ordered.to_list whole) else 
    let temp = immediate_closure_for_several gram l_to_be_treated in 
    let new_whole = Stdl_ordered.merge (Seed.order_on_atoms gram) temp whole 
    and yet_untreated = Stdl_ordered.setminus (Seed.order_on_atoms gram) temp whole in   
    towards_closure gram (new_whole,whole,yet_untreated) ;;

   let closure gram unordered_items = 
      let items = Stdl_ordered.sort (Seed.order_on_atoms gram) unordered_items in 
      towards_closure gram (items,Stdl_ordered.empty_set,items) ;;  

   let push_dots_one_symbol gram symb lrk_molecule =
      let old_atoms = Seed.atoms_inside lrk_molecule in 
      let unordered_new_atoms = List.filter_map (Seed.push_dot_one_symbol symb) old_atoms in
      let new_atoms = Stdl_ordered.sort (Seed.order_on_atoms gram) unordered_new_atoms in 
      Seed.molecule (Stdl_ordered.to_list new_atoms) ;;

   let ghetto_for_jterm gram lrk_molecule symb = closure gram 
   (Seed.atoms_inside(push_dots_one_symbol gram symb lrk_molecule));; 

let compute_ghetto_naively gram ( (_idx,old_lrk_molecule)) symb = 
  let new_lrk_molecule = ghetto_for_jterm gram old_lrk_molecule symb in 
  let new_rlrk_molecule = register_lrk_molecule gram new_lrk_molecule in 
  let older_paths = Registration.get_paths gram old_lrk_molecule in 
  let paths_to_be_added= Stdl_image.image (fun p->p@[symb]) older_paths in 
  let _ = add_new_paths_to_lrk_molecule gram new_lrk_molecule paths_to_be_added in 
  new_rlrk_molecule  
  ;;

let hashtbl_for_ghettoes = Hashtbl.create 100 ;;

let compute_ghetto gram rlr_state symb =
  let ( (idx,_items)) = rlr_state in  
  let key = (gram.grammar_serial_number,idx,symb) in 
  match Hashtbl.find_opt hashtbl_for_ghettoes key with 
  Some old_answer -> old_answer 
  | None ->
   let new_answer = compute_ghetto_naively gram rlr_state symb in 
   let _ = Hashtbl.replace hashtbl_for_ghettoes key new_answer in 
   new_answer
  ;;

let rlrk_molecule_order = ((fun ( (i1,_))  ( (i2,_))->Stdl_total_ordering.for_integers i1 i2): (int * Seed.molecule)  Stdl_total_ordering_t.t) ;;
let rlrk_molecule_fold_merge = Stdl_ordered.fold_merge rlrk_molecule_order ;;
let rlrk_molecule_merge = Stdl_ordered.merge rlrk_molecule_order ;;
let rlrk_molecule_setminus = Stdl_ordered.setminus rlrk_molecule_order ;;
let rlrk_molecule_sort = Stdl_ordered.sort rlrk_molecule_order ;;


let ghetto_neighbors_for_one gram rlrk_molecule = 
   let all_symbols = Stdl_lrp_grammar.all_symbols gram in 
   rlrk_molecule_sort(Stdl_ordered.image (compute_ghetto gram rlrk_molecule) all_symbols) ;;

let ghetto_neighbors_for_several gram lrk_molecules = rlrk_molecule_fold_merge
 (Stdl_image.image (ghetto_neighbors_for_one gram) lrk_molecules) ;;

let rec towards_ghetto_neighborhood gram (whole,_treated,to_be_treated) = 
  if to_be_treated = [] then whole else 
  let temp = ghetto_neighbors_for_several gram to_be_treated in 
  let new_whole = rlrk_molecule_merge temp whole 
  and yet_untreated = rlrk_molecule_setminus temp whole  in 
 towards_ghetto_neighborhood gram (new_whole,whole,Stdl_ordered.to_list yet_untreated) ;;

let ghetto_neighborhood gram lrk_molecules = towards_ghetto_neighborhood gram 
  (rlrk_molecule_sort lrk_molecules,Stdl_ordered.empty_set,lrk_molecules) ;; 

let starter_lrk_molecule gram = 
   closure gram [Seed.starter_atom gram];; 

let starter_rlrk_molecule gram = 
   let starter_lrk_molecule = starter_lrk_molecule gram in
   let answer = register_lrk_molecule gram starter_lrk_molecule in 
   let _ = add_new_paths_to_lrk_molecule gram starter_lrk_molecule [[]] in 
   answer;;
;;

let compute_all_indexed_lrk_molecules_naively gram = 
   let _ = (register_lrk_molecule gram Seed.empty_one) in 
   let strtr=starter_rlrk_molecule gram in
   ghetto_neighborhood gram [strtr] ;;

let hashtbl_for_all_indexed_lrk_molecules = Hashtbl.create 100 ;;  

let all_indexed_lrk_molecules gram =
  let key = (gram.grammar_serial_number) in 
  match Hashtbl.find_opt hashtbl_for_all_indexed_lrk_molecules key with 
  Some old_answer -> old_answer 
  | None ->
   let new_answer = compute_all_indexed_lrk_molecules_naively gram in 
   let _ = Hashtbl.replace hashtbl_for_all_indexed_lrk_molecules key new_answer in 
   new_answer
  ;;


module Pretty_printing = struct 


let on_molecule molecule =
      let atoms = Seed.atoms_inside molecule in 
      "\n\n"^(String.concat "\n" (Stdl_image.image (fun atom-> (String.make 5 ' ')^(Seed.visualize_atom atom)) atoms))^"\n\n" ;;
      
let on_name_giver ((idx,molecule),surname) =
       "State number "^(string_of_int idx)^", aka "^surname^":\n"^
          (on_molecule molecule) ;;

let on_name_givers triples = "\n\n"^(String.concat "\n" (Stdl_image.image on_name_giver triples))^"\n\n" ;;   
      

end ;;   

module Pretty_display = struct 

   let on_parsing_details names_for_states l =
   List.rev_map (
    fun (state_stack,symbol_stack,next_action) ->
      (
         List.rev_map (Stdl_lrp_pretty_printing.on_index names_for_states) state_stack,
         symbol_stack,
         Stdl_lrp_pretty_printing.on_action names_for_states next_action
      )
   ) l ;;

end ;;   


module Usual_names_for_States = struct 

let compute_usual_names_for_lrk_molecules_naively gram = 
      let temp0 = all_indexed_lrk_molecules gram in 
      let l_temp0 = Stdl_ordered.to_list temp0 in 
      let temp1 = List.tl(List.tl(l_temp0)) in 
      let temp2 = Stdl_image.image (
        fun state ->
         let ((_idx,lrk_molecule)) = state in 
         let paths = Registration.get_paths gram lrk_molecule in
         (state,List.hd(List.rev(List.hd paths))) 
      ) temp1 in 
      (
      (List.nth l_temp0 0,"Death")::
      (List.nth l_temp0 1,"Birth")::
      (Stdl_list_again.rename_according_to_occurrence_rank temp2)
       ) ;;

 let hashtbl_for_usual_names_for_lrk_molecules = Hashtbl.create 100 ;;   

    let usual_names_for_lrk_molecules_without_visualizing gram = 
      match Hashtbl.find_opt hashtbl_for_usual_names_for_lrk_molecules gram.grammar_serial_number  with 
      Some old_answer -> old_answer 
    | None ->
    let new_answer = compute_usual_names_for_lrk_molecules_naively gram in 
    let _ = (Hashtbl.replace hashtbl_for_usual_names_for_lrk_molecules gram.grammar_serial_number new_answer) in 
     new_answer ;;

   
   
   let usual_names_for_lrk_molecules gram =
      let answer =usual_names_for_lrk_molecules_without_visualizing gram in 
      let msg = Pretty_printing.on_name_givers answer in 
      let _ = print_string ("\n\n"^msg^"\n\n") in 
      answer;;

end ;;   


module Table = struct 

 let terminals_after_a_dot_in_lrk_molecule gram lrk_molecule =
    let atoms= Seed.atoms_inside lrk_molecule in 
    let items = Stdl_image.image Seed.item_component atoms in 
    let symbols_after_a_dot = 
      str_sort(List.filter_map Stdl_lrp_item.symbol_after_dot_opt items) in  
    let termies = Stdl_lrp_grammar.terminals gram in 
    str_intersect termies symbols_after_a_dot ;; 

   let shifts_from_lrk_molecule gram lrk_molecule =
      let idx = Registration.get_index gram lrk_molecule  in 
      let terms = terminals_after_a_dot_in_lrk_molecule gram lrk_molecule in 
      Stdl_ordered.image (fun term->
        let  ((new_idx,_))= compute_ghetto gram ((idx,lrk_molecule)) term in 
        (term,Shift(new_idx))
      ) terms ;;

    let full_test_for_allowing_reduction gram atom ~head_of_production ~terminal =
      let productions = Stdl_lrp_grammar.productions gram in 
      let (Prod(early_start,_old_start)) = List.hd(productions)   in 
      if head_of_production = early_start then false else  
      Seed.test_for_allowing_reduction gram atom ~head_of_production ~terminal ;;   

   let reduction_from_terminal_and_atom_opt gram terminal atom =
      match Stdl_lrp_item.almost_finished_production_opt (Seed.item_component atom) with
      None -> None
      |Some(production) ->
         let (Prod(head_of_production,_)) = production in 
         if full_test_for_allowing_reduction gram atom ~head_of_production ~terminal 
         then Some(terminal,Reduce(production))
         else None;;  
         
   let reduction_from_molecule_and_terminal_opt gram lrk_molecule term = 
      let atoms= Seed.atoms_inside lrk_molecule in 
      List.find_map (reduction_from_terminal_and_atom_opt gram term) atoms;;  

   let reductions_from_lrk_molecule gram lrk_molecule = 
      let termies = Stdl_ordered.to_list(str_insert end_marker (Stdl_lrp_grammar.terminals gram)) in 
      List.filter_map (reduction_from_molecule_and_terminal_opt gram lrk_molecule) termies ;;
  

   let acceptations_from_lrk_molecule gram lrk_molecule =
      let atoms= Seed.atoms_inside lrk_molecule in 
      if List.mem (Seed.ender_atom gram) atoms 
      then  [(end_marker,Accept)]
      else [] ;; 
     
   let actions_from_lrk_molecule gram lrk_molecule =
      (shifts_from_lrk_molecule gram lrk_molecule)@
      (reductions_from_lrk_molecule gram lrk_molecule)@
      (acceptations_from_lrk_molecule gram lrk_molecule) ;; 
  
   let action_pairs_from_indexed_lrk_molecule gram (idx,lrk_molecule) = 
      let  termies = str_insert end_marker (Stdl_lrp_grammar.terminals gram) 
      and raw_data = actions_from_lrk_molecule gram lrk_molecule in 
      List.filter_map (fun term->
         let results = List.filter_map (fun (term2,act)->if term2=term then Some act else None ) raw_data in 
         if results=[]
         then None 
         else Some((idx,term),results)
      ) (Stdl_ordered.to_list termies) ;;

   let all_action_pairs gram =
      let indexed_molecules = List.tl(Stdl_ordered.to_list(all_indexed_lrk_molecules gram))  in 
      List.flatten(Stdl_image.image (action_pairs_from_indexed_lrk_molecule gram) indexed_molecules );;

   let all_goto_pairs gram = 
      let states = List.tl(Stdl_ordered.to_list(all_indexed_lrk_molecules gram)) 
      and nonterminals = Stdl_ordered.to_list(Stdl_lrp_grammar.nonterminals gram)  in 
      let base = Stdl_cartesian.product states nonterminals in 
      List.filter_map (
         fun pair ->
            let (state,nonterminal) = pair in 
            let ((idx,_items)) = state in 
            let ((new_idx,_new_items)) = compute_ghetto gram state nonterminal in 
            if new_idx>=0 
            then Some((idx,nonterminal),[new_idx])
            else None
      ) base ;;

   let compute_pre_table_naively gram= {
      action_pre_data = all_action_pairs gram;
      goto_pre_data = all_goto_pairs gram;
   } ;; 


   let hashtbl_for_pre_table = Hashtbl.create 100 ;;   

    let pre_table gram = 
      match Hashtbl.find_opt hashtbl_for_pre_table gram.grammar_serial_number  with 
      Some old_answer -> old_answer 
    | None ->
    let new_answer = compute_pre_table_naively gram in 
    let _ = (Hashtbl.replace hashtbl_for_pre_table gram.grammar_serial_number new_answer) in 
     new_answer ;;

   let towards_table gram =
      let pre_tbl = pre_table gram in 
      let (tbl_opt,conflicts_opt)=Stdl_lrp_table.from_pre_table_opt pre_tbl in 
      match tbl_opt with 
      Some tbl -> tbl 
      | None ->
       let names_for_states = Usual_names_for_States.usual_names_for_lrk_molecules_without_visualizing gram 
       and (action_conflicts,goto_conflicts) = Option.get conflicts_opt in 
       let msg ="\n\nThis grammar is not "^ Seed.name ^".\n"^
       (Stdl_lrp_pretty_printing.on_both_conflicts names_for_states (action_conflicts,goto_conflicts)) in 
       let _ = print_string msg in 
       raise Conflict_in_Lr_parser_exn
      ;;

    
   let table gram = 
      let names_for_states = Usual_names_for_States.usual_names_for_lrk_molecules_without_visualizing gram in 
      let tbl = towards_table gram in 
      let _ = print_string(Stdl_lrp_pretty_printing.on_table names_for_states tbl) in 
      tbl ;; 

end ;;   



end ;;   


let all_indexed_lrk_molecules = Private.all_indexed_lrk_molecules ;;


let parsing_details gram text_to_be_parsed = 
   let names_for_states = Private.Usual_names_for_States.usual_names_for_lrk_molecules gram in 
   let lr_table = Private.Table.table gram in 
   let parse_example = Stdl_lrp_table.parsing_details lr_table text_to_be_parsed in 
   Private.Pretty_display.on_parsing_details names_for_states parse_example ;;   


let table = Private.Table.table ;;   


let usual_names_for_lrk_molecules = Private.Usual_names_for_States.usual_names_for_lrk_molecules ;;

end ;;

end;;






module Stdl_lr1_seed=struct

(*

Lrp is short for "LR Parsing"
Lrk here means LR(k), with k =0 or 1


#use"lib/Lr_parsing/lr1_seed.ml";;

*)



open Stdl_lrp_types ;;
open Stdl_lrp_constant ;;

type atom = Atom of item * string ;;
type molecule = Molecule of atom list ;;

module Private = struct 

let atoms_inside (Molecule atoms)= atoms ;; 

let molecule l = Molecule(l) ;;

let item_component (Atom (item,_lah)) = item ;; 

let make_atom item lah = Atom (item,lah) ;;

let lookahead_component (Atom (_item,lah)) = lah ;; 

let atm_order gram = ((fun atom1 atom2 ->
    let trial1 = Stdl_lrp_grammar.order_on_items gram (item_component atom1) (item_component atom2) in 
    if trial1<>Stdl_total_ordering_result_t.Equal 
    then trial1 
    else Stdl_total_ordering.lex_for_strings (lookahead_component atom1)  (lookahead_component atom2)  
): atom Stdl_total_ordering_t.t);;

let atm_sort gram = Stdl_ordered.sort (atm_order gram) ;;

let immediate_closure gram atom =
  let (Item(_p,l))= item_component atom in 
  let productions =Stdl_lrp_grammar.productions gram in 
  let n = List.length l 
  and j = Stdl_list_again.index_of_in "." l in
  if j=n then atm_sort gram [] else 
  let symb = List.nth l j in 
  let incomplete_ender = List.drop (j+1) l 
  and a = lookahead_component atom in
  let ender = incomplete_ender@[a] in 
  let usable_lookaheads = Stdl_lrp_grammar.furst_set_for_form gram ender in 
  let usable_items = List.filter_map ( fun pr->let (Prod(p2,_))=pr in 
  if p2=symb then Some(Stdl_lrp_item.first_item_from_production pr) else None) productions  in 
  let product = Stdl_cartesian.product usable_items (Stdl_ordered.to_list usable_lookaheads) in 
  atm_sort gram (Stdl_image.image (fun (item,lah)->make_atom item lah) product);;

let push_dot_one_symbol symb atom =
  let item= item_component atom 
  and lah = lookahead_component atom in 
   Option.map (fun new_item ->make_atom new_item lah) (
     Stdl_lrp_item.push_dot_one_symbol symb item 
   );;

let starter_atom bare_grammar = 
  let productions =Stdl_lrp_grammar.productions bare_grammar in 
  make_atom(Stdl_lrp_item.first_item_from_production  (List.hd productions)) end_marker;;

let ender_atom bare_grammar = 
  let productions =Stdl_lrp_grammar.productions bare_grammar in 
  make_atom(Stdl_lrp_item.last_item_from_production  (List.hd productions)) end_marker;;  

 let test_for_allowing_reduction (_gram:grammar) atom ~head_of_production ~terminal =
  let comb = ref(String.length head_of_production) in 
  let _ = (comb:=0) in 
  terminal = lookahead_component atom ;;

let visualize_atom (Atom(item,lah)) = (Stdl_lrp_item.visualize item)^" , "^lah ;;

end ;;

let atoms_inside = Private.atoms_inside ;; 

let empty_one = Private.molecule [] ;;

let ender_atom = Private.ender_atom ;;

let immediate_closure = Private.immediate_closure ;;

let item_component  = Private.item_component ;; 

let lookahead_component  = Private.lookahead_component ;; 

let molecule = Private.molecule ;;

let name = "LR(1)" ;;

let order_on_atoms = Private.atm_order ;;

let push_dot_one_symbol = Private.push_dot_one_symbol ;;

let starter_atom = Private.starter_atom ;;

let test_for_allowing_reduction = Private.test_for_allowing_reduction ;;

let visualize_atom = Private.visualize_atom ;;



end;;

