(* 

#use"Hex_analysis/hex_cell_pair_set.ml";;

*)



let constructor l=
   let temp1=Image.image (fun (x,y)->
     if (Hex_cell.cmp x y)=Total_ordering.Greater 
     then (y,x) 
     else (x,y) ) l in 
   let temp2=Ordered.diforchan_plaen Hex_cell.cmp_for_pairs temp1 in 
   Hex_cell_pair_set_t.S(temp2);;


let of_string enclosed_s =
   let n=String.length enclosed_s in 
   let s=Cull_string.interval enclosed_s 2 (n-1) in 
   let temp1=Cull_string.extract_intervals_in_wrt_separator s "," in 
   constructor(Image.image Hex_common.cell_pair_of_string temp1);;

let to_string (Hex_cell_pair_set_t.S(l))=
  let temp1=Image.image Hex_common.cell_pair_to_string l in 
  "{"^(String.concat "," temp1)^"}";;


(*
let pre_z1="{t5 - y4,a2 - b3}";;
let z1=of_string pre_z1;;
let z2=to_string z1;;
let check =(of_string(z2)=z1);;

*)

(*
let of_concrete_object crobj=
   let (_,(arg1,_,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
   Hex_cell_pair_set_t.S(
      Concrete_object_field.to_list Hex_cell.pair_of_concrete_object arg1
   );;
  
let to_concrete_object (Hex_cell_pair_set_t.S(l))= 
    Concrete_object_t.Variant(
      "Hex_"^"cell_pair_set_t.",
      Concrete_object_field.of_list Hex_cell.pair_of_concrete_object arg2
    ) ;;     
*)