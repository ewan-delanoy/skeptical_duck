(*

#use"small_array.ml";;

*)

type 'a t=SA of int*('a list);;

exception Index_out_of_bounds of int*int;;

let size (SA(n,x))=n;;

let get (SA(n,x)) k=
    if (k<1)||(k>n)
    then raise(Index_out_of_bounds(k,n))
    else List.nth x (k-1);;

let set (SA(n,x)) k y=
      if (k<1)||(k>n)
      then raise(Index_out_of_bounds(k,n))
      else let (rleft,right)=Listennou.big_rht (k-1) x in 
           let new_right=y::(List.tl right) in 
           SA(n,List.rev_append rleft new_right);;
      
(* set (SA(5,["i";"j";"k";"l";"m"])) 3 "b";;  *)


let of_list l=SA(List.length l,l);;
let to_list (SA(n,x))=x;;



exception Bad_indices_in_reposition of int*int*int;;

let reposition_by_putting_snd_immediately_after_fst
  (SA(n,x)) i j=
    let (left,ei,middle,ej,right)=
      Listennou.decompose_wrt_two_indices x i j in 
      SA(n,left@(ei::ej::(middle@right)));;
   
exception Overflow;;

let push_right (SA(n,x)) u=(SA(n+1,x@[u]))
    
let push_immediately_after_idx sa u i=
  let interm_sa=push_right sa u in  
  reposition_by_putting_snd_immediately_after_fst interm_sa i (size(sa)+1)  
  ;;

exception Property_not_found;;

let leftmost_index_of_property_in f (SA(_,x))=
    let rec tempf=(fun 
      (nbr_of_items_seen,to_be_treated)->
       match to_be_treated with 
       []->raise(Property_not_found)
       |elt::other_ones->
          let m = nbr_of_items_seen +1 in 
          if f(elt)
          then m
          else tempf(m,other_ones)
    ) in
    tempf (0,x);;

 
let leftmost_index_of_in y sa=
    leftmost_index_of_property_in (fun elt->elt=y) sa;;

let indices_of_property_in f (SA(_,x))=
    let rec tempf=(fun 
      (indices_found,count,to_be_treated)->
       match to_be_treated with 
       []->List.rev indices_found
       |elt::other_ones->
          let m=count+1 in 
          if f(elt)
          then tempf(m::indices_found,m,other_ones)
          else tempf(indices_found,m,other_ones)
    ) in
    tempf ([],0,x);;



let filter_and_unpack f (SA(_,x))=
    Option.filter_and_unpack f x;;

let image f (SA(_,x))=
    Image.image f  x;;
  


exception Remove_item_exn of int*int;;

let remove_item_at_index (SA(n,x)) k=
  if (k<1)||(k>n) then raise(Remove_item_exn(k,n)) else
  let (rleft,right)=Listennou.big_rht (k-1) x in 
  SA(n-1,List.rev_append rleft (List.tl right));;

let apply_transformation_on_interval (SA(n,x)) f i j=
   let (left,middle,right)=Listennou.extract_interval x i j in 
   let new_middle = Image.image f middle in 
   SA(n,left @ new_middle @ right);;

let apply_transformation_on_all (SA(n,x)) f=
  SA(n,Image.image f x);;

let apply_transformation_on_rightmost_interval (SA(n,x)) f i=
  apply_transformation_on_interval (SA(n,x)) f i n;;


let redundant_indices (SA(_,x))=Listennou.redundant_indices x;;

   
let to_concrete_object old_concretizer (SA(n,l))=
    Concrete_object_t.Variant("Small_array.SA",
      [
        Concrete_object_t.Int(n);
        Concrete_object_t.List(Image.image old_concretizer l);
      ]);;


let of_concrete_object old_unconcretizer ccrt_obj =
   let (_,(arg1,arg2,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant ccrt_obj in 
   SA(
      Concrete_object_field.unwrap_int arg1,
      Image.image old_unconcretizer (Concrete_object_field.unwrap_list arg2)
   );;
                     