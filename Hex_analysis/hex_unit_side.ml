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

let to_readable_string = function 
    Hex_unit_side_t.North      ->  "n"
   |Hex_unit_side_t.North_east ->  "ne"
   |Hex_unit_side_t.North_west ->  "nw"
   |Hex_unit_side_t.South      ->  "s"
   |Hex_unit_side_t.South_east ->  "se"
   |Hex_unit_side_t.South_west ->  "sw" ;;
