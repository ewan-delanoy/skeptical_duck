(* 

#use"Hex_analysis/hex_named_connector.ml";;

*)


exception Precomputed_border_connections_exn of Hex_dimension_t.t * Hex_cardinal_direction_t.t ;;
exception Precomputed_inner_connections_exn of Hex_dimension_t.t  ;;



module Private = struct 

 

let expand_name = Hex_connector_constructor.expand_name ;;  


let add_name nm cnnctr = 
   {
     Hex_named_connector_t.name     = nm ;
     entry    = cnnctr.Hex_connector_t.entry ;
     junction = cnnctr.Hex_connector_t.junction ;
     exit     = cnnctr.Hex_connector_t.exit ;
     apex     = cnnctr.Hex_connector_t.apex ;
   };;   

let of_name nm = add_name nm (expand_name nm);; 

let forget_name nc = 
   {
     Hex_connector_t.entry = nc.Hex_named_connector_t.entry ;
     junction = nc.Hex_named_connector_t.junction ;
     exit     = nc.Hex_named_connector_t.exit ;
     apex     = nc.Hex_named_connector_t.apex ;
   };;   

let all_translates dim nc =
   let nm =nc.Hex_named_connector_t.name 
   and cnnctr = forget_name nc in 
   Image.image (add_name nm) (Hex_connector.all_translates dim cnnctr);;

let expand_all dim cnnctrs = 
    List.flatten( Image.image (fun cnnctr ->
    let nc=of_name cnnctr in all_translates dim nc ) cnnctrs);; 



let to_readable_string nc = 
  let apex_part = (match nc.Hex_named_connector_t.apex with 
    None ->""
    |Some(i,j)->"("^(Hex_cell.to_string(Hex_cell.of_int_pair (i,j)))^")"
  ) in 
  (Hex_connector_name.to_readable_string nc.Hex_named_connector_t.name)^apex_part;;


let starters_for_side (dim,side)=
    expand_all dim (Hex_connector_name.starters_for_side side);;
    
let middlers dim= 
    expand_all dim  (Hex_connector_name.middlers);;   

let islanders dim island1 other_islands =
   let temp1 = Image.image ( 
     fun island2 -> 
       let ttemp2 =  Hex_island.common_neighbors dim island1 island2  in 
       let ttemp3 = Set_of_poly_pairs.image Hex_cell.of_int_pair ttemp2 in 
       let ttemp4 = Uple.list_of_pairs ttemp3 in 
       let ttemp5 = List.filter (
           fun (cell1,cell2) -> not(List.mem cell2 (Hex_cell.neighbors dim cell1))
       ) ttemp4 in 
       Image.image (fun (p1,p2)->
          of_name( Hex_connector_name_t.Inner(
             Hex_inner_connector_name_t.Broken_bridge(island1,p1,p2,island2)))
       ) ttemp5
   ) other_islands in 
   List.flatten temp1 ;; 

let namify f nc =
      let nm =nc.Hex_named_connector_t.name 
      and cnnctr = forget_name nc in 
      add_name nm (f cnnctr);;

      let inner_sea nc =
         Hex_cell_set.safe_set 
           (Image.image Hex_cell.of_int_pair 
             nc.Hex_named_connector_t.junction) ;;
      
      

module Precomputed = struct 

let usual_range = Image.image (
   fun d -> (Hex_dimension.eleven,d)
) Hex_cardinal_direction.all;; 

let data_for_starters = 
   Image.image (fun (dim,d)->((dim,d),starters_for_side (dim,d)) ) usual_range;;

let data_for_middlers = 
   Image.image (fun dim->(dim,middlers dim) ) [Hex_dimension.eleven];;


let all_border_connections_for_side dim side=
     match Option.seek (fun p->fst(p)=(dim,side)) data_for_starters with 
     Some(_,vaal)->vaal
    |None -> raise (Precomputed_border_connections_exn(dim,side));;
    
let all_inner_connections dim=
     match Option.seek (fun p->fst(p)=dim) data_for_middlers with 
     Some(_,vaal)->vaal
    |None -> raise (Precomputed_inner_connections_exn(dim));;


    


end ;;


end ;;

let active_part nc =
   Hex_cell_set.fold_merge 
  [ 
    Hex_island.inner_earth nc.Hex_named_connector_t.entry ;
    Hex_island.inner_earth nc.Hex_named_connector_t.exit  ;
  ];;

let all_border_connections_for_side = Private.Precomputed.all_border_connections_for_side ;;

let all_inner_connections = Private.Precomputed.all_inner_connections ;;
  

let check_compatiblity end_of_battle nc = 
   let inner_sea = Hex_cell_set.forget_order (Private.inner_sea nc) 
   and inner_earth = Hex_cell_set.forget_order (Hex_connector.inner_earth (Private.forget_name nc)) in 
   (List.for_all (fun cell -> 
    (Hex_end_of_battle.assess end_of_battle cell)=Hex_eob_result_t.Unoccupied ) inner_sea)
   &&
   (List.for_all (fun cell -> 
    (Hex_end_of_battle.assess end_of_battle cell) <> Hex_eob_result_t.Enemy_territory ) inner_earth)  ;;

let check_disjointness nc1 nc2 =
    Hex_cell_set.does_not_intersect (Private.inner_sea nc1) (Private.inner_sea nc2);; 
   
let check_entry island nc = Hex_connector.check_entry island (Private.forget_name nc);;  

let check_exit nc island = Hex_connector.check_exit island (Private.forget_name nc);;  

let inner_area nc =
      let ju = nc.Hex_named_connector_t.junction in 
      let x_list = Image.image fst ju and y_list = Image.image snd ju in 
      let xmin = Min.list x_list and xmax = Max.list x_list 
      and ymin = Min.list y_list and ymax = Max.list y_list in 
      (xmax-xmin+1)*(ymax-ymin+1);;

let inner_sea = Private.inner_sea ;;

let islanders = Private.islanders ;;


let missing_earth end_of_battle nc = 
   let inner_earth = Hex_cell_set.forget_order 
       (Hex_connector.inner_earth (Private.forget_name nc)) in 
   Hex_cell_set.safe_set(List.filter (fun cell -> 
    (Hex_end_of_battle.assess end_of_battle cell) 
     = Hex_eob_result_t.Unoccupied ) inner_earth)  ;;

let missing_pods end_of_battle nc = 
   let selector = (fun island->
     let inner_earth = Hex_cell_set.forget_order (Hex_island.inner_earth island) in 
     Hex_cell_set.safe_set(List.filter (fun cell -> 
    (Hex_end_of_battle.assess end_of_battle cell) 
     = Hex_eob_result_t.Unoccupied ) inner_earth) 
   ) in 
   (
      selector nc.Hex_named_connector_t.entry,
      selector nc.Hex_named_connector_t.exit
   )  ;;

let of_name = Private.of_name ;;

let oppose dim = Private.namify (Hex_connector.oppose dim) ;;


let print_out (fmt:Format.formatter) nc=
   Format.fprintf fmt "@[%s@]" (Private.to_readable_string nc);;     

let reflect = Private.namify Hex_connector.reflect ;;

let to_molecular_linker nc = 
     match Hex_connector_name.to_nondefault_molecular_linker 
             nc.Hex_named_connector_t.name 
              nc.Hex_named_connector_t.apex
              nc.Hex_named_connector_t.junction with 
     None -> Hex_connector.to_default_molecular_linker (Private.forget_name nc)           
    |Some(mlclr) -> mlclr  ;; 
   


let to_readable_string = Private.to_readable_string ;;

let translate p = Private.namify (Hex_connector.translate p) ;;

let wet_earth nc = Hex_cell_set.fold_merge

  [
     Hex_island.inner_earth nc.Hex_named_connector_t.entry ;
     Hex_island.inner_earth nc.Hex_named_connector_t.exit ;
     (Hex_cell_set.safe_set(Image.image Hex_cell.of_int_pair nc.Hex_named_connector_t.junction)) ;
  ] 
  ;;

let wet_earth_with_entry_unchecked nc = Hex_cell_set.fold_merge

  [
     Hex_island.inner_earth nc.Hex_named_connector_t.exit ;
     (Hex_cell_set.safe_set(Image.image Hex_cell.of_int_pair nc.Hex_named_connector_t.junction)) ;
  ] 
  ;;
