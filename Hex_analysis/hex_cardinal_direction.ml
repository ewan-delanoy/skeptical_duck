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

let test_for_low_border    (Hex_dimension_t.D dim) (i,j)= (i=dim);;
let test_for_left_border                           (i,j)= (j=1);;
let test_for_right_border  (Hex_dimension_t.D dim) (i,j)= (j=dim);;
let test_for_upper_border                          (i,j)= (i=1);;
   
    


end ;;

let border_enumerator (Hex_dimension_t.D dim) side k= 
    let transfo j = (String.make 1 (char_of_int(j+96))) in 
    match side with 
     Hex_cardinal_direction_t.Down  -> Hex_cell_t.C(transfo k,dim)
    |Hex_cardinal_direction_t.Left  -> Hex_cell_t.C(transfo 1,k)
    |Hex_cardinal_direction_t.Right -> Hex_cell_t.C(transfo dim,k)
    |Hex_cardinal_direction_t.Up    -> Hex_cell_t.C(transfo k,1);;
            

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

let test_for_border (dim,direction) cell=
     let p = Hex_cell.to_int_pair  cell in
     match direction with   
     Hex_cardinal_direction_t.Down  -> Private.test_for_low_border dim p
    |Hex_cardinal_direction_t.Left  -> Private.test_for_left_border p
    |Hex_cardinal_direction_t.Right -> Private.test_for_right_border dim p
    |Hex_cardinal_direction_t.Up    -> Private.test_for_upper_border p;;
            
let test_for_distance (Hex_dimension_t.D(dim),direction) (x,y) d=
     match direction with   
     Hex_cardinal_direction_t.Down  -> y = dim+1 - d
    |Hex_cardinal_direction_t.Left  -> x = d
    |Hex_cardinal_direction_t.Right -> x = dim+1 - d
    |Hex_cardinal_direction_t.Up    -> y = d;;
            

let to_concrete_object = Concrete_object_field.wrap_lonely_variant Private.crobj_correspondences;;


let to_int = function 
    (* the correspondance is arbitrary, it just needs to be bijective. *)
     Hex_cardinal_direction_t.Left  -> 1
    |Hex_cardinal_direction_t.Up    -> 2
    |Hex_cardinal_direction_t.Right -> 3
    |Hex_cardinal_direction_t.Down  -> 4 ;;
