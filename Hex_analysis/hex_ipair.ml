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




 
let adhoc_translate core_f l=
   Image.image (fun (x,y,a,b)->
     let (x1,y1)=core_f(x,y)
     and (a1,b1)=core_f(a,b) in 
     (x1,y1,a1,b1)
   ) l;; 

let core_support_for_upwards_pyramid =
  [(1,1,1,2);(1,3,1,4);(2,1,2,3);(2,2,3,1);(3,3,4,2);
(1,7,1,8);(1,5,1,6);(2,5,2,7);(2,6,3,6);(3,4,4,4);
(3,2,3,5)];;    

let core_support_for_downwards_pyramid =
   adhoc_translate (fun (x,y)->(9-x,9-y))  core_support_for_upwards_pyramid;;  

let core_support_for_leftwards_pyramid =
   adhoc_translate (fun (x,y)->(y,x))  core_support_for_upwards_pyramid;; 

let core_support_for_rightwards_pyramid =
   adhoc_translate (fun (x,y)->(9-y,9-x))  core_support_for_upwards_pyramid;; 

let translator (dx,dy) l=adhoc_translate (fun (x,y)->(x+dx,y+dy)) l;;
   

end;;

let add_labels l_fourtuples=
   let temp1=Ennig.index_everything l_fourtuples in 
   let temp2=Image.image (fun (k,(i1,j1,i2,j2))->
      let c=char_of_int(123-k) in 
      [((i1,j1),c);((i2,j2),c)]
   ) temp1 in 
   List.flatten temp2;; 

let of_cell cell= Private.ipair_of_string (Hex_cell.to_string cell);;
let to_cell pair =Hex_cell.of_string(Private.string_of_ipair pair);;

let support_for_downwards_pyramid (x,y)=
   Private.translator (x-5,y-6) Private.core_support_for_downwards_pyramid;;

let support_for_leftwards_pyramid (x,y)=
   Private.translator (x-3,y-4) Private.core_support_for_leftwards_pyramid;;

let support_for_rightwards_pyramid (x,y)=
   Private.translator (x-6,y-5) Private.core_support_for_rightwards_pyramid;;

let support_for_upwards_pyramid (x,y)=
   Private.translator (x-4,y-3) Private.core_support_for_upwards_pyramid;;

