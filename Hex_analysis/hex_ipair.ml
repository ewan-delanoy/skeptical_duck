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

let translator (dx,dy) l=adhoc_translate (fun (x,y)->(x+dx,y+dy)) l;;   

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

let core_support_for_sb_upwards_claw = [(1,1,1,2);(1,3,1,4);(2,1,2,3);(2,1,2,3);(2,2,3,2)];;    

let core_support_for_bs_downwards_claw =
   adhoc_translate (fun (x,y)->(5-x,5-y))  core_support_for_sb_upwards_claw;;  

let core_support_for_sb_leftwards_claw =
   adhoc_translate (fun (x,y)->(y,x))  core_support_for_sb_upwards_claw;; 

let core_support_for_bs_rightwards_claw =
   adhoc_translate (fun (x,y)->(5-y,5-x))  core_support_for_sb_upwards_claw;; 


let core_support_for_bs_upwards_claw = [(1,1,1,2);(1,3,1,4);(2,1,2,3);(2,1,2,3);(2,2,3,1)];;    

let core_support_for_sb_downwards_claw =
   adhoc_translate (fun (x,y)->(5-x,5-y))  core_support_for_bs_upwards_claw;;  

let core_support_for_bs_leftwards_claw =
   adhoc_translate (fun (x,y)->(y,x))  core_support_for_bs_upwards_claw;; 

let core_support_for_sb_rightwards_claw =
   adhoc_translate (fun (x,y)->(5-y,5-x))  core_support_for_bs_upwards_claw;; 

let core_powder_for_left_eyed_upwards_claw =
  (
    (4,2),
    [(1, 1); (1, 2); (1, 3); (1, 4); (1, 5); (1, 6); (1, 7); 
     (2, 1); (2, 2); (2, 3); (2, 4); (2, 5); (2, 6); 
     (3, 1); (3, 2); (3, 3); (3, 4); (3, 5);
     (4, 3)]
  );;

let core_powder_for_right_eyed_upwards_claw =
  (
    (4,3),
    [(1, 1); (1, 2); (1, 3); (1, 4); (1, 5); (1, 6); (1, 7); 
     (2, 1); (2, 2); (2, 3); (2, 4); (2, 5); (2, 6); 
     (3, 1); (3, 2); (3, 3); (3, 4); (3, 5);
     (4, 2)]
  );;

let reflection (x,y) = (y,x);;
let central_sym (x,y)= (7-x,7-y);;

let apply_on_both f (a,b)=(f a,Ordered.sort Total_ordering.standard2 (Image.image f b));;

let reflection_on_both p = apply_on_both reflection p;;
let central_sym_on_both p = apply_on_both central_sym p;;

let adjust_by_translation whole (x1,y1)=
    let ((x0,y0),l)=whole in 
    let temp1=Image.image (fun (x,y)->(x1-x0+x,y1-y0+y)) l in
    Ordered.sort Total_ordering.standard2 temp1;; 


let core_powder_for_high_eyed_leftwards_claw =
    reflection_on_both core_powder_for_left_eyed_upwards_claw;;
  
let core_powder_for_low_eyed_leftwards_claw =
    reflection_on_both core_powder_for_right_eyed_upwards_claw;;

let core_powder_for_left_eyed_downwards_claw =
    central_sym_on_both core_powder_for_right_eyed_upwards_claw;;

let core_powder_for_right_eyed_downwards_claw =
    central_sym_on_both core_powder_for_left_eyed_upwards_claw;;

let core_powder_for_high_eyed_rightwards_claw =
    central_sym_on_both  core_powder_for_low_eyed_leftwards_claw;;

let core_powder_for_low_eyed_rightwards_claw =
    central_sym_on_both  core_powder_for_high_eyed_leftwards_claw;;

end;;

let add_labels l_fourtuples=
   let temp1=Ennig.index_everything l_fourtuples in 
   let temp2=Image.image (fun (k,(i1,j1,i2,j2))->
      let c=char_of_int(123-k) in 
      let s=" "^(String.make 1 c)^" " in
      [((i1,j1),s);((i2,j2),s)]
   ) temp1 in 
   List.flatten temp2;; 

let of_cell cell= Private.ipair_of_string (Hex_cell.to_string cell);;
let to_cell pair =Hex_cell.of_string(Private.string_of_ipair pair);;


let powder_for_left_eyed_upwards_claw =
    Private.adjust_by_translation 
      Private.core_powder_for_left_eyed_upwards_claw;;

let powder_for_right_eyed_upwards_claw =
    Private.adjust_by_translation 
      Private.core_powder_for_right_eyed_upwards_claw;;

let powder_for_high_eyed_leftwards_claw =
    Private.adjust_by_translation 
      Private.core_powder_for_high_eyed_leftwards_claw;;
  
let powder_for_low_eyed_leftwards_claw =
    Private.adjust_by_translation 
      Private.core_powder_for_low_eyed_leftwards_claw;;

let powder_for_left_eyed_downwards_claw =
    Private.adjust_by_translation 
      Private.core_powder_for_left_eyed_downwards_claw;;

let powder_for_right_eyed_downwards_claw =
    Private.adjust_by_translation 
      Private.core_powder_for_right_eyed_downwards_claw;;

let powder_for_high_eyed_rightwards_claw =
     Private.adjust_by_translation 
      Private.core_powder_for_high_eyed_rightwards_claw;;

let powder_for_low_eyed_rightwards_claw =
   Private.adjust_by_translation 
      Private.core_powder_for_low_eyed_rightwards_claw;;


let support_for_downwards_pyramid (x,y)=
   Private.translator (x-5,y-6) Private.core_support_for_downwards_pyramid;;

let support_for_leftwards_pyramid (x,y)=
   Private.translator (x-3,y-4) Private.core_support_for_leftwards_pyramid;;

let support_for_rightwards_pyramid (x,y)=
   Private.translator (x-6,y-5) Private.core_support_for_rightwards_pyramid;;

let support_for_upwards_pyramid (x,y)=
   Private.translator (x-4,y-3) Private.core_support_for_upwards_pyramid;;

let support_for_bs_downwards_claw (x,y)=
   Private.translator (x-2,y-4) Private.core_support_for_bs_downwards_claw;;

let support_for_bs_leftwards_claw (x,y)=
   Private.translator (x-2,y-3) Private.core_support_for_bs_leftwards_claw;;

let support_for_bs_rightwards_claw (x,y)=
   Private.translator (x-4,y-2) Private.core_support_for_bs_rightwards_claw;;

let support_for_bs_upwards_claw (x,y)=
   Private.translator (x-3,y-2) Private.core_support_for_bs_upwards_claw;;

let support_for_sb_downwards_claw (x,y)=
   Private.translator (x-2,y-3) Private.core_support_for_sb_downwards_claw;;

let support_for_sb_leftwards_claw (x,y)=
   Private.translator (x-1,y-3) Private.core_support_for_sb_leftwards_claw;;

let support_for_sb_rightwards_claw (x,y)=
   Private.translator (x-3,y-2) Private.core_support_for_sb_rightwards_claw;;

let support_for_sb_upwards_claw (x,y)=
   Private.translator (x-3,y-1) Private.core_support_for_sb_upwards_claw;;

let support_for_ps1 (x,y)=
   [(11, 1, 11, 2); (10, 2, 10, 4); (4, 3, 4, 4); (5, 3, 3, 5); (6, 3, 5, 4);
   (8, 3, 8, 4); (10, 3, 9, 4); (11, 3, 11, 4); (1, 4, 1, 5); (2, 4, 1, 6);
   (3, 4, 2, 5); (2, 6, 1, 7); (2, 7, 1, 8); (2, 8, 1, 9); (2, 9, 2, 10);
   (1, 10, 1, 11)];;



