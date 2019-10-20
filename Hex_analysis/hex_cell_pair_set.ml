(* 

#use"Hex_analysis/hex_cell_pair_set.ml";;

*)



let constructor l=
   let temp1=Image.image (fun (sx,sy)->
     let x=Hex_cell.of_string sx 
     and y=Hex_cell.of_string sy  in
     if (Hex_cell.cmp x y)=Total_ordering.Greater 
     then (y,x) 
     else (x,y) ) l in 
   let temp2=Ordered.diforchan_plaen Hex_cell.cmp_for_pairs temp1 in 
   Hex_cell_pair_set_t.S(temp2);;

(*
let of_string enclosed_s =
   let n=String.length enclosed_s in 
   let s=Cull_string.interval enclosed_s 2 (n-1) in 
   let temp1=Cull_string.extract_intervals_in_wrt_separator s "," in 
   constructor(Image.image Hex_common. of_string temp1);;

let to_string (Hex_cell_set_t.S(l))=
  let temp1=Image.image Hex_cell.to_string l in 
  "{"^(String.concat "," temp1)^"}";;



let pre_z1="{t5-y4,a2-b3}";;
let z1=of_string pre_z1;;
let z2=to_string z1;;
let check =(of_string(z2)=z1);;

*)