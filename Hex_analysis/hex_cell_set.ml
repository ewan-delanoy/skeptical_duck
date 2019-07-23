(* 

#use"Hex_analysis/hex_cell_set.ml";;

*)


let safe_set l=  Hex_cell_set_t.S (Ordered.diforchan_plaen Hex_cell.cmp l);;

let  apply_condition opt_condition cell_set=match opt_condition with 
   None -> cell_set 
  |Some(condition_set) -> 
     let (Hex_cell_set_t.S(l1))=cell_set 
     and (Hex_cell_set_t.S(l2))=condition_set in 
     Hex_cell_set_t.S(Ordered.kengeij_plaen Hex_cell.cmp l1 l2);;  

let fold_intersect l=  
    let renamed_l=Image.image (fun (Hex_cell_set_t.S(e))->Ordered.S(e) ) l in 
    Hex_cell_set_t.S (Ordered.forget_order(Ordered.big_kengeij Hex_cell.cmp renamed_l));;
let insert elt (Hex_cell_set_t.S(l))= Hex_cell_set_t.S (Ordered.insert_plaen Hex_cell.cmp elt l);;
let optional_min (Hex_cell_set_t.S(l))=match l with []->None |a::b->Some(a);;
let outsert elt (Hex_cell_set_t.S(l))= Hex_cell_set_t.S (Ordered.lemel_plaen Hex_cell.cmp l [elt]);;
let length (Hex_cell_set_t.S(l))=List.length l;;
let mem elt (Hex_cell_set_t.S(l))=Ordered.elfenn_plaen Hex_cell.cmp elt l;;
let min (Hex_cell_set_t.S(l))=List.hd l;;
let setminus (Hex_cell_set_t.S(l1)) (Hex_cell_set_t.S(l2))=Hex_cell_set_t.S (Ordered.lemel_plaen Hex_cell.cmp l1 l2);;
let unveil (Hex_cell_set_t.S(l))= l;;


let to_string (Hex_cell_set_t.S(l))=
  let temp1=Image.image Hex_cell.to_string l in 
  "{"^(String.concat "," temp1)^"}";;

let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;     