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
      Image.image (fun (i,j)->if i=j then [i] else [i;j]) l
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
           
let write_using_connected_components l = 
    let temp1 = decompose_into_connected_components l in 
    let temp2 = Private.helper_for_cc_writing ([],temp1) in 
    String.concat "" ( Image.image (
      fun (is_a_long_block,i,j,l) ->
        if is_a_long_block 
        then "["^(string_of_int i)^".."^(string_of_int j)^"]"
        else "{"^(String.concat "," (Image.image string_of_int l))^"}"
    ) temp2) ;;
(*

write_using_connected_components
[1; 3; 4; 6; 8; 9; 11; 12; 13; 14; 15; 17; 18; 19; 22; 24; 25; 27; 29; 30;
   31; 32; 36; 37] ;;

*)

