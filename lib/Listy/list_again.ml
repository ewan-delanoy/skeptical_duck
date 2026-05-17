(*

#use"lib/Listy/list_again.ml";;

*)

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

let rec iterator_for_finding_and_remembering f (treated,to_be_treated) = 
  match to_be_treated with 
  [] -> None 
  |item :: next_items ->
    if f item 
    then Some(List.rev treated,item,next_items)
    else iterator_for_finding_and_remembering f (item::treated,next_items) ;;

let find_and_remember_opt f items = 
    iterator_for_finding_and_remembering f ([],items) ;;
  
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
   let indexed_l = Int_range.index_everything l in 
   let indexed_l_with_images = Image.image (fun (i,x)->(i,x,f x)) indexed_l in    
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

end ;;    

let assoc_right_opt y l = 
    Option.map fst (List.find_opt (fun p->snd(p)=y) l);;

(*
assoc_right_opt 3 (Int_range.scale (fun t->(t*t,t)) 1 7) ;;
*)

let common_initial_sublist l1 l2 = Private.helper_for_common_initial_sublist ([],l1,l2) ;;

(*
common_initial_sublist [1;2;3;7;8] [1;2;3;9] ;; 
*)

let connected_fibers = Private.connected_fibers ;;
(*
connected_fibers (fun x->int_of_float(floor(sqrt(float_of_int x)))) (Int_range.range 1 50) ;;
*)

let find_and_remember_opt = Private.find_and_remember_opt ;;

(*

find_and_remember_opt (fun x->x=7) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] ;;

*)

let find_index_of_in x ll=
    let rec sub_f=(function (j,l)->match l with
    []->(-1)      
    |u::v->if u=x then j else sub_f(j+1,v)) in
    sub_f(1,ll);;

let find_index_of_in_opt x ll=
    let rec sub_f=(function (j,l)->match l with
    []->None      
    |u::v->if u=x then Some j else sub_f(j+1,v)) in
    sub_f(1,ll);;


exception Head_with_tail_exn ;; 

let head_with_tail x=match x with
    []->raise(Head_with_tail_exn)
    |a::b->(a,b);;

let interval = Private.interval ;;

(* interval (Int_range.range 1 7) 2 5 ;; *)

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
    
let long_head r l=if (r>(List.length l)) then l else List.rev(fst(long_head_with_tail(r)(l)));;
    
let long_tail r l=if (r>(List.length l)) then [] else snd(long_head_with_tail(r)(l));;    

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


let power_set l=
  let rec tempf=(function 
     (treated,to_be_treated)->match to_be_treated with
     []->treated
    |a::others->tempf(treated@(Image.image(function y->a::y)(treated)),others)
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

let sublist_with_indices l indices = Image.image (fun k->List.nth l (k-1)) indices ;;

(* sublist_with_indices  ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"] [2;3;7] ;; *)
  
let universal_delta_list l=
  let rec sub_f=(function (accu,a,rl)->match rl with
      []->List.rev(accu)
    |b::x->sub_f((a,b)::accu,b,x)
  ) in
  match l with
  []->[]
  |u::v->sub_f([],u,v);;


