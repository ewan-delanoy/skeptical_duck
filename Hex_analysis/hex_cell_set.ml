(* 

#use"Hex_analysis/hex_cell_set.ml";;

*)

let mem elt l=Ordered.elfenn_plaen Hex_cell.cmp elt l;;
let safe_set l=  Hex_cell_set_t.S (Ordered.forget_order(Ordered.safe_set Hex_cell.cmp l));;
let insert elt (Hex_cell_set_t.S(l))= Hex_cell_set_t.S (Ordered.insert_plaen Hex_cell.cmp elt l);;
let outsert elt (Hex_cell_set_t.S(l))= Hex_cell_set_t.S (Ordered.lemel_plaen Hex_cell.cmp l [elt]);;
let fold_intersect l=  Hex_cell_set_t.S (Ordered.forget_order(Ordered.big_kengeij Hex_cell.cmp l));;

let to_string (Hex_cell_set_t.S(l))=
  let temp1=Image.image Hex_cell.to_string l in 
  "{"^(String.concat "," temp1)^"}";;

let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;     