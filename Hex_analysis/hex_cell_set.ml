(* 

#use"Hex_analysis/hex_cell_set.ml";;

*)


let safe_set l=  Hex_cell_set_t.S (Ordered.diforchan_plaen Hex_cell.cmp l);;

let does_not_intersect (Hex_cell_set_t.S(l1)) (Hex_cell_set_t.S(l2))=
        Ordered.kengeij_goullo Hex_cell.cmp (Ordered.S l1) (Ordered.S l2);;  
let fold_intersect l=  
    let renamed_l=Image.image (fun (Hex_cell_set_t.S(e))->Ordered.S(e) ) l in 
    Hex_cell_set_t.S (Ordered.forget_order(Ordered.big_kengeij Hex_cell.cmp renamed_l));;
let fold_merge l=  
    let renamed_l=Image.image (fun (Hex_cell_set_t.S(e))->Ordered.S(e) ) l in 
    Hex_cell_set_t.S (Ordered.forget_order(Ordered.big_teuzin Hex_cell.cmp renamed_l));;

let is_included_in (Hex_cell_set_t.S(l1)) (Hex_cell_set_t.S(l2))=
        Ordered.ental Hex_cell.cmp (Ordered.S l1) (Ordered.S l2);;    
let insert elt (Hex_cell_set_t.S(l))= Hex_cell_set_t.S (Ordered.insert_plaen Hex_cell.cmp elt l);;
let length (Hex_cell_set_t.S(l))=List.length l;;
let mem elt (Hex_cell_set_t.S(l))=Ordered.mem_silently Hex_cell.cmp elt l;;
let min (Hex_cell_set_t.S(l))=List.hd l;;
let optional_min (Hex_cell_set_t.S(l))=match l with []->None |a::b->Some(a);;
let outsert elt (Hex_cell_set_t.S(l))= Hex_cell_set_t.S (Ordered.lemel_plaen Hex_cell.cmp l [elt]);;
let setminus (Hex_cell_set_t.S(l1)) (Hex_cell_set_t.S(l2))=Hex_cell_set_t.S (Ordered.lemel_plaen Hex_cell.cmp l1 l2);;
let unveil (Hex_cell_set_t.S(l))= l;;
let  apply_condition opt_condition cell_set=match opt_condition with 
   None -> cell_set 
  |Some(condition_set) -> 
     fold_intersect [cell_set;condition_set];;



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

let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;     

let of_concrete_object crobj=
   let (_,(arg1,_,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
   Hex_cell_set_t.S(
      Concrete_object_field.to_list Hex_cell.of_concrete_object arg1
   );;

let to_concrete_object (Hex_cell_set_t.S(l))=
   Concrete_object_t.Variant("Hex_"^"cell_set_t.S",[Concrete_object_field.of_list Hex_cell.to_concrete_object l]);;


