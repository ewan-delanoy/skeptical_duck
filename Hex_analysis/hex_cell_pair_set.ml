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

