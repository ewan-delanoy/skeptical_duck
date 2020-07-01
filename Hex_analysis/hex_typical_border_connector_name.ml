(* 

#use"Hex_analysis/hex_typical_border_connector_name.ml";;

*)

exception To_readable_string_exn ;;   

module Private = struct

let prepare_for_journey = 
  [
     Hex_typical_border_connector_name_t.Border_bridge , 
       (Some"bb",None,(2,1),[(1, 1); (1, 2)],[],[]) ;
     Hex_typical_border_connector_name_t.Byssus , 
        (None,Some("sdb","slb","brs","bus"),(3,2),
              [(1, 1); (1, 2);   (1, 3); (1, 4); 
               (2, 1); (2, 3);   (2, 2); (3, 1)],[],[]) ; 
     Hex_typical_border_connector_name_t.Pyramid , 
        (Some "py",None,(4,3),
              [(1, 1); (1, 2); (1, 3); (1, 4);  (1, 5); (1, 6); (1, 7); (1, 8); 
               (2, 1); (2, 3); (2, 5); (2, 7);  (2, 2); (3, 1); (2, 6); (3, 6); 
               (3, 2); (3, 5); (3, 3); (4, 2);  (3, 4); (4, 4)],[],[]) ;
     Hex_typical_border_connector_name_t.Small_pyramid , 
        (Some "sy", None,(3,2),
               [(1, 1); (1, 2);  (1, 4); (1, 5);  
                (2, 1); (2, 4);  (2, 2); (3, 1); 
                (2, 3); (3, 3)],[],[]) ;
     Hex_typical_border_connector_name_t.Sybil , 
        (None,Some("bds","bls","srb","sub"),(3,1),
               [(1, 1); (1, 2); (1, 3); (1, 4); 
                (2, 1); (2, 3); (2, 2); (3, 2)],[],[]) ;
     Hex_typical_border_connector_name_t.Walleye1, 
        (Some "we1",None,(2,4),
               [(1, 1); (1, 2);  (1, 5); (2, 1); 
                (2, 2); (3, 1);  (2, 3); (3, 3)],[3,2],[]) ;
     Hex_typical_border_connector_name_t.Walleye2, 
        (Some "we2",None,(3,1),
               [(1, 1); (1, 2);  (1, 3); (1, 4); 
                (2, 1); (2, 2) ],[2,3],[]) ;  
     Hex_typical_border_connector_name_t.Walleye3, 
        (Some "we3",None,(3,1),
               [(1, 1); (1, 2);  (1, 3); (2, 1); 
                (2, 2); (2, 3) ],[3,2],[]) ;    
     Hex_typical_border_connector_name_t.Hat, 
        (Some "hat",None,(3,2),
               [(1, 2); (2, 3);  (1, 3); (1, 4); 
                (2, 1); (2, 2) ],[],[3,1]) ;  
     Hex_typical_border_connector_name_t.Hat2, 
        (Some "hat2",None,(3,3),
               [(1, 1); (1, 2);  (1, 4); (1, 5); 
                (2, 1); (2, 4);  (2, 2); (3, 1)],[],[3,2]) ;                                        
  ];;

let upwards_version tbc = 
    let (_,_,apex,ju,extra_actives,extra_in_entry) = List.assoc tbc prepare_for_journey in 
   {Hex_connector_t.entry =
    Hex_island_t.I (Hex_anchor_t.No_anchor, Set_of_poly_pairs.safe_set (apex::extra_in_entry));
    junction = ju;
    exit = Hex_island_t.I (Hex_anchor_t.Single_anchor(Hex_cardinal_direction_t.Up), Set_of_poly_pairs_t.S []);
    apex = Some(apex);
    extra_active_cells = extra_actives ;
    };;

let specify_side tbc side = 
     let u = upwards_version tbc in 
     let l = Hex_connector.reflect u in 
      match side with 
     Hex_cardinal_direction_t.Down  -> Hex_connector.oppose Hex_dimension.eleven u
    |Hex_cardinal_direction_t.Left  -> l
    |Hex_cardinal_direction_t.Right -> Hex_connector.oppose Hex_dimension.eleven l
    |Hex_cardinal_direction_t.Up    -> u;; 

end ;;


let for_side side =
Image.vorstellung (
       fun (tbc,data_for_tbc) -> Hex_border_connector_name_t.Typical(tbc,side)
     )   Private.prepare_for_journey ;;
    
let full_constructor tbc side new_apex=
     let default_example = Private.specify_side tbc side in 
     let (x1,y1) = Option.unpack(default_example.Hex_connector_t.apex) 
     and (x2,y2) = new_apex in 
     let dx=x2-x1 and dy=y2-y1 in 
     Hex_connector.translate (dx,dy) default_example;;    
 
let specify_side = Private.specify_side ;;

let to_readable_string tbc side = 
   let (opt_uniform,opt_diverse,_,_,_,_) = List.assoc tbc Private.prepare_for_journey  in 
   match opt_uniform with 
    Some(unf) -> (Hex_cardinal_direction.for_ground_description side)^unf
   |None ->
   match opt_diverse with 
   Some(ad,al,ar,au) -> (
                           match side with 
                           Hex_cardinal_direction_t.Down  -> ad
                          |Hex_cardinal_direction_t.Left  -> al
                          |Hex_cardinal_direction_t.Right -> ar
                          |Hex_cardinal_direction_t.Up    -> au 
                        )
   |None ->  raise (To_readable_string_exn);;     