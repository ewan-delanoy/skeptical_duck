(* 

#use"Hex_analysis/hex_ipair.ml";;

Alternate coordinate system, used in Hex_ascii_grid.

*)

exception Bad_eyed_claw_specification of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t ;;

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

let down = Hex_cardinal_direction_t.Down;;
let left = Hex_cardinal_direction_t.Left;;
let right = Hex_cardinal_direction_t.Right;;
let up = Hex_cardinal_direction_t.Up;;
let high = up and low = down;;

let powder_for_high_eyed_claw = function 
     Hex_cardinal_direction_t.Left -> adjust_by_translation core_powder_for_high_eyed_leftwards_claw
    |Hex_cardinal_direction_t.Right -> adjust_by_translation core_powder_for_high_eyed_rightwards_claw
    |other -> raise(Bad_eyed_claw_specification(high,other));;

let powder_for_left_eyed_claw = function 
     Hex_cardinal_direction_t.Up -> adjust_by_translation core_powder_for_left_eyed_upwards_claw
    |Hex_cardinal_direction_t.Down -> adjust_by_translation core_powder_for_left_eyed_downwards_claw
    |other -> raise(Bad_eyed_claw_specification(left,other));;

let powder_for_low_eyed_claw = function 
     Hex_cardinal_direction_t.Left -> adjust_by_translation core_powder_for_low_eyed_leftwards_claw
    |Hex_cardinal_direction_t.Right -> adjust_by_translation core_powder_for_low_eyed_rightwards_claw
    |other -> raise(Bad_eyed_claw_specification(low,other));;


let powder_for_right_eyed_claw = function 
     Hex_cardinal_direction_t.Up -> adjust_by_translation core_powder_for_right_eyed_upwards_claw
    |Hex_cardinal_direction_t.Down -> adjust_by_translation core_powder_for_right_eyed_downwards_claw
    |other -> raise(Bad_eyed_claw_specification(right,other));;


let support_for_bs_claw direction (x,y) = match direction with 
     Hex_cardinal_direction_t.Down  ->  translator (x-2,y-4) core_support_for_bs_downwards_claw
    |Hex_cardinal_direction_t.Left  ->  translator (x-2,y-3) core_support_for_bs_leftwards_claw
    |Hex_cardinal_direction_t.Right ->  translator (x-4,y-2) core_support_for_bs_rightwards_claw
    |Hex_cardinal_direction_t.Up    ->  translator (x-3,y-2) core_support_for_bs_upwards_claw;;
            
let support_for_sb_claw direction (x,y) = match direction with 
     Hex_cardinal_direction_t.Down  ->  translator (x-2,y-3) core_support_for_sb_downwards_claw
    |Hex_cardinal_direction_t.Left  ->  translator (x-1,y-3) core_support_for_sb_leftwards_claw
    |Hex_cardinal_direction_t.Right ->  translator (x-3,y-2) core_support_for_sb_rightwards_claw
    |Hex_cardinal_direction_t.Up    ->  translator (x-3,y-1) core_support_for_sb_upwards_claw;;
            


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

let is_valid  (Hex_dimension_t.D dim) (i,j) = (1<=i) && (i<=dim) && (1<=j) && (j<=dim)   ;;


let ipair_powder_for_eyed_claw = function 
     Hex_cardinal_direction_t.Down  -> Private.powder_for_low_eyed_claw
    |Hex_cardinal_direction_t.Left  -> Private.powder_for_left_eyed_claw
    |Hex_cardinal_direction_t.Right -> Private.powder_for_right_eyed_claw
    |Hex_cardinal_direction_t.Up    -> Private.powder_for_high_eyed_claw;;
            
let ipair_support_for_pyramid direction (x,y) = match direction with 
     Hex_cardinal_direction_t.Down  ->  Private.translator (x-5,y-6) Private.core_support_for_downwards_pyramid
    |Hex_cardinal_direction_t.Left  ->  Private.translator (x-3,y-4) Private.core_support_for_leftwards_pyramid
    |Hex_cardinal_direction_t.Right ->  Private.translator (x-6,y-5) Private.core_support_for_rightwards_pyramid
    |Hex_cardinal_direction_t.Up    ->  Private.translator (x-4,y-3) Private.core_support_for_upwards_pyramid;;
            
let ipair_support_for_noneyed_claw qualifier = match qualifier with 
   Hex_double_hump_qualifier_t.Big_followed_by_small -> Private.support_for_bs_claw 
  |Hex_double_hump_qualifier_t.Small_followed_by_big -> Private.support_for_sb_claw ;;

let to_cell pair =Hex_cell.of_string(Private.string_of_ipair pair);;



