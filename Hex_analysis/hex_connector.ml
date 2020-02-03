(* 

#use"Hex_analysis/hex_connector.ml";;

*)

module Private = struct 

let translate cnnctr (dx,dy)=  
   let trl = (fun z->
      Set_of_poly_pairs.safe_set( Set_of_poly_pairs.image (fun (x,y)->(x+dx,y+dy)) z) 
   )  in 
   let (Hex_island_t.I(opt1,elts1)) = cnnctr.Hex_connector_t.entry 
   and (Hex_island_t.I(opt2,elts2)) = cnnctr.Hex_connector_t.exit in 
   {
    Hex_connector_t.entry =Hex_island_t.I(opt1,trl elts1);
    junction = Image.image (fun (x,y)->(x+dx,y+dy)) (cnnctr.Hex_connector_t.junction) ;
    exit = Hex_island_t.I(opt2,trl elts2);
};;

end ;;

let bring_to_left_upper_corner cnnctr = 
   let (Hex_island_t.I(_,elts1)) = cnnctr.Hex_connector_t.entry 
   and (Hex_island_t.I(_,elts2)) = cnnctr.Hex_connector_t.exit 
   and elts3 = cnnctr.Hex_connector_t.junction in 
   let elts = Set_of_poly_pairs.fold_merge [elts1;elts2;Set_of_poly_pairs.sort elts3] in 
   let abscissas = Set_of_poly_pairs.image fst elts 
   and ordinates = Set_of_poly_pairs.image snd elts in 
   let xmin = Min.list abscissas 
   and ymin = Min.list ordinates in    
   Private.translate cnnctr (1-xmin,1-ymin);; 

let check_entry island cnnctr =
    Hex_island.is_included_in cnnctr.Hex_connector_t.entry island;;

let check_exit island cnnctr =
    Hex_island.is_included_in cnnctr.Hex_connector_t.exit island;;

let oppose dim cnnctr = 
  let on_island = Hex_island.oppose dim
  and on_pairs =  Image.image (Hex_ipair.oppose dim) in 
{
    Hex_connector_t.entry =on_island (cnnctr.Hex_connector_t.entry);
    junction =  on_pairs (cnnctr.Hex_connector_t.junction) ;
    exit = on_island (cnnctr.Hex_connector_t.exit);
};;

let reflect cnnctr = {
    Hex_connector_t.entry = Hex_island.reflect (cnnctr.Hex_connector_t.entry);
    junction = Image.image Hex_ipair.reflect (cnnctr.Hex_connector_t.junction) ;
    exit = Hex_island.reflect (cnnctr.Hex_connector_t.exit);
};;

let reverse cnnctr = {
    cnnctr with  
    Hex_connector_t.entry = (cnnctr.Hex_connector_t.exit);
                    exit  = (cnnctr.Hex_connector_t.entry);
};;



let translates formal_dim cnnctr = 
   let (Hex_dimension_t.D dim) = formal_dim in 
   let (Hex_island_t.I(opt1,elts1)) = cnnctr.Hex_connector_t.entry 
   and (Hex_island_t.I(opt2,elts2)) = cnnctr.Hex_connector_t.exit 
   and elts3 = cnnctr.Hex_connector_t.junction in 
   let opt = (if opt1<>None then opt1 else opt2) in 
   let base = Hex_cardinal_direction.authorized_translations formal_dim opt in 
   let elts = Set_of_poly_pairs.fold_merge [elts1;elts2;Set_of_poly_pairs.sort elts3] in 
   let abscissas = Set_of_poly_pairs.image fst elts 
   and ordinates = Set_of_poly_pairs.image snd elts in 
   let xmin = Min.list abscissas and xmax = Max.list abscissas 
   and ymin = Min.list ordinates and ymax = Max.list ordinates in    
   let cleaned_base =  List.filter ( 
       fun (dx,dy) ->  (1-xmin <= dx) && (dx <= dim-xmax) 
                    && (1-ymin <= dy) && (dy <= dim-ymax) 
    )  base in 
   Image.image (Private.translate cnnctr) cleaned_base ;; 

module Example = struct 
 

let bs_upwards_claw = {
    Hex_connector_t.entry = Hex_island_t.I(None,Set_of_poly_pairs_t.S [(3,1)]);
    junction = [(1, 1); (1, 2); (1, 3); (1, 4); (2, 1); (2, 3); (2, 2); (3, 2)];
    exit = Hex_island_t.I(Some(Hex_cardinal_direction_t.Up),Set_of_poly_pairs.empty_set);
} ;;   



let left_eyed_upwards_claw = {Hex_connector_t.entry =
    Hex_island_t.I (None, Set_of_poly_pairs_t.S [(4, 2)]);
   junction =
    [(1, 1); (1, 2); (1, 3); (1, 4); (1, 5); (1, 6); (1, 7); 
     (2, 1); (2, 2); (2, 3); (2, 4); (2, 5); (2, 6); 
     (3, 1); (3, 2); (3, 3); (3, 4); (3, 5); 
                     (4, 3)];
   exit =
    Hex_island_t.I (Some Hex_cardinal_direction_t.Up, Set_of_poly_pairs_t.S [])};;




let northeast_bridge = {
    Hex_connector_t.entry = Hex_island_t.I(None,Set_of_poly_pairs_t.S [(2,1)]);
    junction = [(1, 2); (2, 2)];
    exit = Hex_island_t.I(None,Set_of_poly_pairs_t.S [(1,3)]);
} ;;   

let northwest_bridge = {
    Hex_connector_t.entry = Hex_island_t.I(None,Set_of_poly_pairs_t.S [(2,2)]);
    junction = [(1, 2); (2, 1)];
    exit = Hex_island_t.I(None,Set_of_poly_pairs_t.S [(1,1)]);
} ;;   

let right_eyed_upwards_claw = {
    Hex_connector_t.entry = Hex_island_t.I(None,Set_of_poly_pairs_t.S [(4,3)]);
    junction = [ 
        (1, 1); (1, 2); (1, 3); (1, 4); (1, 5); (1, 6); (1, 7); 
        (2, 1); (2, 2); (2, 3); (2, 4); (2, 5); (2, 6); 
        (3, 1); (3, 2); (3, 3); (3, 4); (3, 5); 
                (4, 2)
    ];
    exit = Hex_island_t.I(Some(Hex_cardinal_direction_t.Up),Set_of_poly_pairs.empty_set);
} ;;   



let sb_upwards_claw = {Hex_connector_t.entry =
    Hex_island_t.I (None, Set_of_poly_pairs_t.S [(3, 2)]);
   junction = [(1, 1); (1, 2); (1, 3); (1, 4); (2, 1); (2, 3); (2, 2); (3, 1)];
   exit =
    Hex_island_t.I (Some Hex_cardinal_direction_t.Up, Set_of_poly_pairs_t.S [])} ;;

let upwards_pyramid = 
{Hex_connector_t.entry = Hex_island_t.I (None, Set_of_poly_pairs_t.S [(4, 3)]);
   junction =
    [(1, 1); (1, 2);    (1, 3); (1, 4);   (1, 5); (1, 6);   (1, 7); (1, 8); 
     (2, 1); (2, 3);    (2, 5); (2, 7);   (2, 2); (3, 1);   (2, 6); (3, 6); 
     (3, 2); (3, 5);    (3, 3); (4, 2);   (3, 4); (4, 4);
    ];
   exit =
    Hex_island_t.I (Some Hex_cardinal_direction_t.Up, Set_of_poly_pairs_t.S [])};;

let upwards_small_pyramid = 
{Hex_connector_t.entry =
    Hex_island_t.I (None, Set_of_poly_pairs_t.S [(3, 2)]);
   junction =
    [(1, 1) ; (1, 2) ;   (1, 4); (1, 5); 
     (2, 1) ; (2, 4) ;   (2, 2); (3, 1); (2, 3); (3, 3)];
   exit =
    Hex_island_t.I (Some Hex_cardinal_direction_t.Up, Set_of_poly_pairs_t.S [])};;


end ;;
