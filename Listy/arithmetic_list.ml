(*

#use"Listy/arithmetic_list.ml";;

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
    let (d,treated,last_elt,in_progress,to_be_treated) = uple in        
    match to_be_treated with 
    [] ->  List.rev(List.rev(last_elt::in_progress)::treated)
    | _ -> iterator_for_far_apart_components(pusher_for_far_apart_components uple);;
end ;;


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
           

