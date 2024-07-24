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

end ;;    

let common_initial_sublist l1 l2 = Private.helper_for_common_initial_sublist ([],l1,l2) ;;

(*
common_initial_sublist [1;2;3;7;8] [1;2;3;9] ;; 
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


