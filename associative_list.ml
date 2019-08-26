
(*

#use"associative_list.ml";;

*)

exception Push_immediately_after_exn;;
exception Reposition_first_key_not_found;;
exception Reposition_second_key_not_found;;



let change_value_for_key l (key1,vaal1)=
   Image.image (fun pair->if fst(pair)=key1 then (key1,vaal1) else pair) l;; 

let remove_key l key1=List.filter (fun (key,_)->key<>key1) l;;

let push_immediately_after l pair2 key1 =
  let rec tempf=(
    fun (treated,to_be_treated)->match to_be_treated with 
     []->raise(Push_immediately_after_exn)
    |pair::other_pairs ->
      if fst(pair)=key1
      then List.rev_append treated (pair::pair2::other_pairs)
      else tempf(pair::treated,other_pairs)
  ) in 
  tempf([],l);; 

(* push_immediately_after [(1,"u");(2,"v");(3,"w");(4,"x")] (7,"e") 2;; *)

let decompose_wrt_key l key1=
  let rec tempf=(
     fun (treated,to_be_treated)->match to_be_treated with 
     []->(List.rev treated,None,[])
    |pair::other_pairs ->
       if fst(pair)=key1
      then (List.rev(treated),Some(pair),other_pairs)
      else tempf(pair::treated,other_pairs)
  ) in 
  tempf([],l);; 

(* decompose_wrt_key [(1,"u");(2,"v");(3,"w");(4,"x");(5,"y");(6,"z")] 2;; *)



let reposition_by_putting_snd_immediately_after_fst l key_i key_j=
    let (left1,opt1,right1)=decompose_wrt_key l key_i in 
    if opt1=None then raise(Reposition_first_key_not_found) else 
    let (left2,opt2,right2)=decompose_wrt_key right1 key_j in 
    if opt2=None then raise(Reposition_second_key_not_found) else
    let pair1=Option.unpack opt1 and pair2=Option.unpack opt2 in 
    left1@(pair1::pair2::(left2@right2));; 
  
(* reposition_by_putting_snd_immediately_after_fst [(1,"u");(2,"v");(3,"w");(4,"x");(5,"y");(6,"z")] 2 5;; *)  

let reorder l key_ordering =Image.image (fun key->(key,List.assoc key l)) key_ordering;;

(* reorder [(1,"u");(2,"v");(3,"w");(4,"x");(5,"y");(6,"z")] [2;5;1;3;4;6];; *) 
 
