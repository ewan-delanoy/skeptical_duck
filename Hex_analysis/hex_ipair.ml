(* 

#use"Hex_analysis/hex_ipair.ml";;

Alternate coordinate system, used in Hex_ascii_grid.

*)

module Private = struct

let ipair_of_string s=
  let j=(int_of_char(String.get s 0))-96
  and i=int_of_string(Cull_string.cobeginning 1 s) in 
  (i,j);;

(* ipair_of_string "b5" = (5,2);; *)

let string_of_ipair (i,j)=
  (String.make 1 (char_of_int(j+96)))^(string_of_int i);;

let core_support_for_upwards_pyramid=
   [
      ((1,1),'b');((1,2),'b');  
      ((1,3),'c');((1,4),'c');
      ((2,1),'d');((2,3),'d');
      ((2,2),'e');((3,1),'e');
      ((3,3),'f');((4,2),'f');
   ]
   @
   [
      ((1,7),'B');((1,8),'B');  
      ((1,5),'C');((1,6),'C');
      ((2,5),'D');((2,7),'D');
      ((2,6),'E');((3,6),'E');
      ((3,4),'F');((4,4),'F');
   ]
   @
   [
      ((3,2),'*');((3,5),'*');  
   ];;

let translator (dx,dy) l=
   Image.image (fun ((a,b),c)->((a+dx,b+dy),c)) l;;

end;;

let of_cell cell= Private.ipair_of_string (Hex_cell.to_string cell);;
let to_cell pair =Hex_cell.of_string(Private.string_of_ipair pair);;

let support_for_upwards_pyramid (x,y)=
   Private.translator (x-4,y-3) Private.core_support_for_upwards_pyramid;;

