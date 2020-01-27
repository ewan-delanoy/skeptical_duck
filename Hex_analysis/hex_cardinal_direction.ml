(* 

#use"Hex_analysis/hex_cardinal_direction.ml";;

*)

module Private = struct 
let correspondences = 
    [
      'd', Hex_cardinal_direction_t.Down  ;
      'l', Hex_cardinal_direction_t.Left  ;
      'r', Hex_cardinal_direction_t.Right ;
      'u', Hex_cardinal_direction_t.Up ;
    ];;

let salt = "Hex_"^"cardinal_direction_t.";;

let crobj_correspondences = 
    [
       Hex_cardinal_direction_t.Down,  salt ^ "Down" ;
       Hex_cardinal_direction_t.Left,  salt ^ "Left"  ;
       Hex_cardinal_direction_t.Right, salt ^ "Right" ;
       Hex_cardinal_direction_t.Up,    salt ^ "Up" ;
    ];;

let char_for_first_direction = function
     Hex_cardinal_direction_t.Down  -> "l"
    |Hex_cardinal_direction_t.Left  -> "l"
    |Hex_cardinal_direction_t.Right -> "r"
    |Hex_cardinal_direction_t.Up    -> "h";;

let char_for_second_direction = function
     Hex_cardinal_direction_t.Down  -> "d"
    |Hex_cardinal_direction_t.Left  -> "l"
    |Hex_cardinal_direction_t.Right -> "r"
    |Hex_cardinal_direction_t.Up    -> "u";;
            
let short_name_for_pair (d1,d2)=
   (char_for_first_direction d1)^(char_for_first_direction d2);;

let test_for_low_parallel    (Hex_dimension_t.D dim) (i,j) d= (i=dim+1-d);;
let test_for_left_parallel                           (i,j) d= (j=d);;
let test_for_right_parallel  (Hex_dimension_t.D dim) (i,j) d= (j=dim+1-d);;
let test_for_upper_parallel                          (i,j) d= (i=d);;    


end ;;

module Parallel_To_Border = struct 

let enumerate d (Hex_dimension_t.D dim) side k= 
    let transfo j = (String.make 1 (char_of_int(j+96))) in 
    match side with 
     Hex_cardinal_direction_t.Down  -> Hex_cell_t.C(transfo k,dim+1-d)
    |Hex_cardinal_direction_t.Left  -> Hex_cell_t.C(transfo d,k)
    |Hex_cardinal_direction_t.Right -> Hex_cell_t.C(transfo (dim+1-d),k)
    |Hex_cardinal_direction_t.Up    -> Hex_cell_t.C(transfo k,d);;

let test d dim side p=
     match side with   
     Hex_cardinal_direction_t.Down  -> Private.test_for_low_parallel dim p d
    |Hex_cardinal_direction_t.Left  -> Private.test_for_left_parallel p d
    |Hex_cardinal_direction_t.Right -> Private.test_for_right_parallel dim p d
    |Hex_cardinal_direction_t.Up    -> Private.test_for_upper_parallel p d;;

end ;;

module Border = struct 

let enumerate = Parallel_To_Border.enumerate 1;;
let test dim side cell = Parallel_To_Border.test 1 dim side (Hex_cell.to_int_pair cell);;

end ;;      

let is_vertical = function 
    Hex_cardinal_direction_t.Down 
   |Hex_cardinal_direction_t.Up -> true 
   | _ -> false ;;  

let sides_for_player = function 
    Hex_player_t.First_player  -> [Hex_cardinal_direction_t.Down;Hex_cardinal_direction_t.Up]
   |Hex_player_t.Second_player -> [Hex_cardinal_direction_t.Left;Hex_cardinal_direction_t.Right];;

let of_concrete_object = Concrete_object_field.unwrap_lonely_variant Private.crobj_correspondences;;

let opposite = function  
     Hex_cardinal_direction_t.Down  -> Hex_cardinal_direction_t.Up
    |Hex_cardinal_direction_t.Left  -> Hex_cardinal_direction_t.Right
    |Hex_cardinal_direction_t.Right -> Hex_cardinal_direction_t.Left
    |Hex_cardinal_direction_t.Up    -> Hex_cardinal_direction_t.Down;;
            

let opt_of_char c =
   Option.find_and_stop (
       fun p->if fst(p)=c then Some(snd p) else None) 
       Private.correspondences ;;
   
let orthogonal_directions = function  
     Hex_cardinal_direction_t.Down  
    |Hex_cardinal_direction_t.Up    -> [Hex_cardinal_direction_t.Left;Hex_cardinal_direction_t.Right]
    |Hex_cardinal_direction_t.Left  
    |Hex_cardinal_direction_t.Right -> [Hex_cardinal_direction_t.Up;Hex_cardinal_direction_t.Down];;
          

let reflect = function 
     Hex_cardinal_direction_t.Down  -> Hex_cardinal_direction_t.Right
    |Hex_cardinal_direction_t.Left  -> Hex_cardinal_direction_t.Up
    |Hex_cardinal_direction_t.Right -> Hex_cardinal_direction_t.Down
    |Hex_cardinal_direction_t.Up    -> Hex_cardinal_direction_t.Left;;

let short_name_for_pair = Private.short_name_for_pair;;
         

let to_concrete_object = Concrete_object_field.wrap_lonely_variant Private.crobj_correspondences;;


let to_int = function 
    (* the correspondance is arbitrary, it just needs to be bijective. *)
     Hex_cardinal_direction_t.Left  -> 1
    |Hex_cardinal_direction_t.Up    -> 2
    |Hex_cardinal_direction_t.Right -> 3
    |Hex_cardinal_direction_t.Down  -> 4 ;;
