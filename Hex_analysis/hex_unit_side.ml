(* 

#use"Hex_analysis/hex_unit_side.ml";;


*)

let oppose = function 
    Hex_unit_side_t.North      ->  Hex_unit_side_t.South
   |Hex_unit_side_t.North_east ->  Hex_unit_side_t.South_west
   |Hex_unit_side_t.North_west ->  Hex_unit_side_t.South_east
   |Hex_unit_side_t.South      ->  Hex_unit_side_t.North
   |Hex_unit_side_t.South_east ->  Hex_unit_side_t.North_west
   |Hex_unit_side_t.South_west ->  Hex_unit_side_t.North_east ;;


let reflect = function 
    Hex_unit_side_t.North      ->  Hex_unit_side_t.South_west
   |Hex_unit_side_t.North_east ->  Hex_unit_side_t.South
   |Hex_unit_side_t.North_west ->  Hex_unit_side_t.North_west
   |Hex_unit_side_t.South      ->  Hex_unit_side_t.North_east
   |Hex_unit_side_t.South_east ->  Hex_unit_side_t.South_east
   |Hex_unit_side_t.South_west ->  Hex_unit_side_t.North ;;
