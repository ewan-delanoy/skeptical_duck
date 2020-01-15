(* 

#use"Hex_analysis/hex_cell_set.ml";;

*)

(* Beginning of directly set-related methods *)

let tr = ((fun x->Hex_cell_set_t.S(x)),(fun (Hex_cell_set_t.S(x))->x),Hex_cell.cmp);;

let does_not_intersect x y= Functor_for_sets.does_not_intersect tr x y;;
let fold_intersect l= Functor_for_sets.fold_intersect tr l;;
let fold_merge l= Functor_for_sets.fold_merge tr l;;
let forget_order x= Functor_for_sets.forget_order tr x;;
let insert a x= Functor_for_sets.insert tr a x;;
let intersect l= Functor_for_sets.intersect tr l;;
let length x= Functor_for_sets.length tr x;;
let mem a x= Functor_for_sets.mem tr a x;;
let merge l= Functor_for_sets.merge tr l;;
let outsert a x= Functor_for_sets.outsert tr a x;;
let safe_set l= Functor_for_sets.safe_set tr l;;
let setminus x y= Functor_for_sets.setminus tr x y;;



(* End of directly set-related methods *)



let  apply_condition opt_condition cell_set=match opt_condition with 
   None -> cell_set 
  |Some(condition_set) -> 
     fold_intersect [cell_set;condition_set];;

let elements_appearing_more_than_once l = 
   let (Hex_cell_set_t.S(temp1)) = fold_merge l in
   let temp2=List.filter (fun elt->
      List.length(List.filter (mem elt) l)>1
   ) temp1 in 
   Hex_cell_set_t.S(temp2);;

let min (Hex_cell_set_t.S(l))=List.hd l;;     
let optional_min (Hex_cell_set_t.S(l))=match l with []->None |a::b->Some(a);;

let seek_nondisjoint_parts l=
   Option.seek (fun (a,b)->not(does_not_intersect a b)) (Uple.list_of_pairs l);;



let of_string enclosed_s =
   let n=String.length enclosed_s in 
   let s=Cull_string.interval enclosed_s 2 (n-1) in 
   let temp1=Cull_string.extract_intervals_in_wrt_separator s "," in 
   safe_set(Image.image Hex_cell.of_string temp1);;

let to_string (Hex_cell_set_t.S(l))=
  let temp1=Image.image Hex_cell.to_string l in 
  "{"^(String.concat "," temp1)^"}";;

(*

let pre_z1="{t5,y4,a2,b3}";;
let z1=of_string pre_z1;;
let z2=to_string z1;;
let check =(of_string(z2)=z1);;

*)

let neighbors dim amoeba =
   let (Hex_cell_set_t.S(l))=amoeba in 
   let temp1 = Image.image (fun cell->safe_set (Hex_cell.neighbors dim cell)) l in 
   setminus (fold_merge temp1) amoeba;;

let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;     

let of_concrete_object crobj=
   let (_,(arg1,_,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
   Hex_cell_set_t.S(
      Concrete_object_field.to_list Hex_cell.of_concrete_object arg1
   );;

let to_concrete_object (Hex_cell_set_t.S(l))=
   Concrete_object_t.Variant("Hex_"^"cell_set_t.S",[Concrete_object_field.of_list Hex_cell.to_concrete_object l]);;


