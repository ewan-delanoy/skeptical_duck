(* 

#use"Hex_analysis/hex_cardinal_direction.ml";;

*)

exception Bad_eyed_claw_specification;;


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

let char_for_eye_description = function
     Hex_cardinal_direction_t.Down  -> "l"
    |Hex_cardinal_direction_t.Left  -> "l"
    |Hex_cardinal_direction_t.Right -> "r"
    |Hex_cardinal_direction_t.Up    -> "h";;

let char_for_ground_description = function
     Hex_cardinal_direction_t.Down  -> "d"
    |Hex_cardinal_direction_t.Left  -> "l"
    |Hex_cardinal_direction_t.Right -> "r"
    |Hex_cardinal_direction_t.Up    -> "u";;
            
let short_name_for_pair (d1,d2)=
   (char_for_eye_description d1)^(char_for_eye_description d2);;

let test_for_low_parallel    (Hex_dimension_t.D dim) (i,j) d= (i=dim+1-d);;
let test_for_left_parallel                           (i,j) d= (j=d);;
let test_for_right_parallel  (Hex_dimension_t.D dim) (i,j) d= (j=dim+1-d);;
let test_for_upper_parallel                          (i,j) d= (i=d);;    

let orthogonal_directions = function  
     Hex_cardinal_direction_t.Down  
    |Hex_cardinal_direction_t.Up    -> [Hex_cardinal_direction_t.Left;Hex_cardinal_direction_t.Right]
    |Hex_cardinal_direction_t.Left  
    |Hex_cardinal_direction_t.Right -> [Hex_cardinal_direction_t.Up;Hex_cardinal_direction_t.Down];;
  

module Parallel_To_Border = struct 

let enumerate d (Hex_dimension_t.D dim) side k= 
    match side with 
     Hex_cardinal_direction_t.Down  -> Hex_cell_t.C(dim+1-d,k)
    |Hex_cardinal_direction_t.Left  -> Hex_cell_t.C(k,d)
    |Hex_cardinal_direction_t.Right -> Hex_cell_t.C(k,dim+1-d)
    |Hex_cardinal_direction_t.Up    -> Hex_cell_t.C(d,k);;

let enumerate_all d (Hex_dimension_t.D dim) side =
   Ennig.doyle (
        enumerate d (Hex_dimension_t.D dim) side
   ) 1 dim;;

let test d dim side p=
     match side with   
     Hex_cardinal_direction_t.Down  -> test_for_low_parallel dim p d
    |Hex_cardinal_direction_t.Left  -> test_for_left_parallel p d
    |Hex_cardinal_direction_t.Right -> test_for_right_parallel dim p d
    |Hex_cardinal_direction_t.Up    -> test_for_upper_parallel p d;;



end ;;

module Immediately_Next_to_Border = struct 

let enumerate1 =  Parallel_To_Border.enumerate 1;;
let enumerate2 dim side k=  
    let offset = (match side with 
      Hex_cardinal_direction_t.Down  
     |Hex_cardinal_direction_t.Right -> 1
     |Hex_cardinal_direction_t.Left  
     |Hex_cardinal_direction_t.Up    -> 0
    ) in  
    Parallel_To_Border.enumerate 2 dim side (k+offset);;

let enumerate dim side k= 
  (enumerate2 dim side k,enumerate1 dim side k,enumerate1 dim side (k+1));;

let enumerate_triangles_for_side formal_dim side = 
   let (Hex_dimension_t.D dim) = formal_dim in 
   Ennig.doyle (enumerate formal_dim side) 1 (dim-1);; 


  

end ;;      


end ;;



module Border = struct 

let corners formal_dim side =
    let (Hex_dimension_t.D dim) = formal_dim in 
    let enm = Private.Parallel_To_Border.enumerate 1 formal_dim side in 
    Image.image (fun x->Hex_cell_set.safe_set [enm x;enm(x+1)]) [1;dim-1];;

let enumerate = Private.Parallel_To_Border.enumerate 1;;
let enumerate_all = Private.Parallel_To_Border.enumerate_all 1;;
let test dim side cell = Private.Parallel_To_Border.test 1 dim side (Hex_cell.to_int_pair cell);;

end ;;      




let all =  
[
 Hex_cardinal_direction_t.Down;
 Hex_cardinal_direction_t.Left;
 Hex_cardinal_direction_t.Right;
 Hex_cardinal_direction_t.Up;
];;

let all_orthogonal_pairs = 
   List.flatten(
   Image.image (fun d->
     Image.image (fun d2->(d,d2)) (Private.orthogonal_directions d)
   ) all);;

let authorized_translations (Hex_dimension_t.D dim) opt = 
   let base = Cartesian.square (Ennig.ennig (1-dim) (dim-1)) in 
   match opt with 
   None -> base 
   |Some(direction) ->
     (
        match direction with 
        Hex_cardinal_direction_t.Down 
       |Hex_cardinal_direction_t.Up -> List.filter (fun (dx,dy)->dx=0) base
       | _ -> List.filter (fun (dx,dy)->dy=0) base
     );;

let bounds_for_authorized_translations (Hex_dimension_t.D dim) opt = 
        match opt with 
        None -> Rectangle_bounds_t.B(1-dim,dim-1,1-dim,dim-1)
        |Some(direction) ->
          (
             match direction with 
             Hex_cardinal_direction_t.Down 
            |Hex_cardinal_direction_t.Up -> Rectangle_bounds_t.B(0,0,1-dim,dim-1)
            | _ -> Rectangle_bounds_t.B(1-dim,dim-1,0,0)
          );;

let enhance dim side cell =
   (* used to avoid ambiguities about which side the cell is on *)
   let opt_side = (if Border.test dim side cell then Some side else None) in 
   (opt_side,cell);; 

let for_eye_description    = Private.char_for_eye_description ;;
let for_ground_description = Private.char_for_ground_description ;;

let is_vertical = function 
    Hex_cardinal_direction_t.Down 
   |Hex_cardinal_direction_t.Up -> true 
   | _ -> false ;;  

let name_for_eyed_claw d1 d2 =
   (Private.char_for_eye_description d1)^"e"^
     (Private.char_for_ground_description d2) ;;


let of_concrete_object = Concrete_object_field.unwrap_lonely_variant Private.crobj_correspondences;;

let oppose = function  
     Hex_cardinal_direction_t.Down  -> Hex_cardinal_direction_t.Up
    |Hex_cardinal_direction_t.Left  -> Hex_cardinal_direction_t.Right
    |Hex_cardinal_direction_t.Right -> Hex_cardinal_direction_t.Left
    |Hex_cardinal_direction_t.Up    -> Hex_cardinal_direction_t.Down;;
            

let opt_of_char c =
   Option.find_and_stop (
       fun p->if fst(p)=c then Some(snd p) else None) 
       Private.correspondences ;;
   
let orthogonal_directions = Private.orthogonal_directions ;;

let player_for_side = function 
     Hex_cardinal_direction_t.Down  
    |Hex_cardinal_direction_t.Up  -> Hex_player_t.First_player
    |Hex_cardinal_direction_t.Left  
    |Hex_cardinal_direction_t.Right -> Hex_player_t.Second_player;;

let reflect = function 
     Hex_cardinal_direction_t.Down  -> Hex_cardinal_direction_t.Right
    |Hex_cardinal_direction_t.Left  -> Hex_cardinal_direction_t.Up
    |Hex_cardinal_direction_t.Right -> Hex_cardinal_direction_t.Down
    |Hex_cardinal_direction_t.Up    -> Hex_cardinal_direction_t.Left;;

let short_name_for_pair = Private.short_name_for_pair;;
         
let sides_for_player = function 
    Hex_player_t.First_player  -> (Hex_cardinal_direction_t.Down,Hex_cardinal_direction_t.Up)
   |Hex_player_t.Second_player -> (Hex_cardinal_direction_t.Left,Hex_cardinal_direction_t.Right);;



let to_concrete_object = Concrete_object_field.wrap_lonely_variant Private.crobj_correspondences;;


let to_int = function 
    (* the correspondance is arbitrary, it just needs to be bijective. *)
     Hex_cardinal_direction_t.Left  -> 1
    |Hex_cardinal_direction_t.Up    -> 2
    |Hex_cardinal_direction_t.Right -> 3
    |Hex_cardinal_direction_t.Down  -> 4 ;;

let triangles_for_side = Private.Immediately_Next_to_Border.enumerate_triangles_for_side ;;