(* 

#use"Hex_analysis/hex_unit_side.ml";;


*)

let all = 
   [ 
    Hex_expsv_unit_side_t.North      ;
    Hex_expsv_unit_side_t.North_east ;
    Hex_expsv_unit_side_t.North_west ;
    Hex_expsv_unit_side_t.South      ;
    Hex_expsv_unit_side_t.South_east ;
    Hex_expsv_unit_side_t.South_west ;
   ];;

let oppose = function 
    Hex_expsv_unit_side_t.North      ->  Hex_expsv_unit_side_t.South
   |Hex_expsv_unit_side_t.North_east ->  Hex_expsv_unit_side_t.South_west
   |Hex_expsv_unit_side_t.North_west ->  Hex_expsv_unit_side_t.South_east
   |Hex_expsv_unit_side_t.South      ->  Hex_expsv_unit_side_t.North
   |Hex_expsv_unit_side_t.South_east ->  Hex_expsv_unit_side_t.North_west
   |Hex_expsv_unit_side_t.South_west ->  Hex_expsv_unit_side_t.North_east ;;


let reflect = function 
    Hex_expsv_unit_side_t.North      ->  Hex_expsv_unit_side_t.South_west
   |Hex_expsv_unit_side_t.North_east ->  Hex_expsv_unit_side_t.South
   |Hex_expsv_unit_side_t.North_west ->  Hex_expsv_unit_side_t.North_west
   |Hex_expsv_unit_side_t.South      ->  Hex_expsv_unit_side_t.North_east
   |Hex_expsv_unit_side_t.South_east ->  Hex_expsv_unit_side_t.South_east
   |Hex_expsv_unit_side_t.South_west ->  Hex_expsv_unit_side_t.North ;;

let to_readable_string = function 
    Hex_expsv_unit_side_t.North      ->  "n"
   |Hex_expsv_unit_side_t.North_east ->  "ne"
   |Hex_expsv_unit_side_t.North_west ->  "nw"
   |Hex_expsv_unit_side_t.South      ->  "s"
   |Hex_expsv_unit_side_t.South_east ->  "se"
   |Hex_expsv_unit_side_t.South_west ->  "sw" ;;
