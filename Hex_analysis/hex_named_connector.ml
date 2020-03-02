(* 

#use"Hex_analysis/hex_named_connector.ml";;

*)


exception Precomputed_starter_exn of Hex_dimension_t.t * Hex_cardinal_direction_t.t ;;
exception Precomputed_middler_exn of Hex_dimension_t.t  ;;
exception Precomputed_ender_exn of Hex_dimension_t.t * Hex_cardinal_direction_t.t ;;


module Private = struct 

 

let expand_name = Hex_connector_constructor.expand_name ;;  


let add_name nm cnnctr = 
   {
     Hex_named_connector_t.name     = nm ;
     entry    = cnnctr.Hex_connector_t.entry ;
     junction = cnnctr.Hex_connector_t.junction ;
     exit     = cnnctr.Hex_connector_t.exit ;
     apex     = cnnctr.Hex_connector_t.apex
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

let enders_for_side (dim,side)=
    expand_all dim (Hex_connector_name.enders_for_side side);;    
 
module Precomputed = struct 

let usual_range = Image.image (
   fun d -> (Hex_dimension.eleven,d)
) Hex_cardinal_direction.all;; 

let data_for_starters = 
   Image.image (fun (dim,d)->((dim,d),starters_for_side (dim,d)) ) usual_range;;

let data_for_middlers = 
   Image.image (fun dim->(dim,middlers dim) ) [Hex_dimension.eleven];;


let data_for_enders = 
   Image.image (fun (dim,d)->((dim,d),enders_for_side (dim,d)) ) usual_range;;

let starters_for_side dim side=
     match Option.seek (fun p->fst(p)=(dim,side)) data_for_starters with 
     Some(_,vaal)->vaal
    |None -> raise (Precomputed_starter_exn(dim,side));;
    
let middlers dim=
     match Option.seek (fun p->fst(p)=dim) data_for_middlers with 
     Some(_,vaal)->vaal
    |None -> raise (Precomputed_middler_exn(dim));;

let enders_for_side dim side=
     match Option.seek (fun p->fst(p)=(dim,side)) data_for_enders with 
     Some(_,vaal)->vaal
    |None -> raise (Precomputed_ender_exn(dim,side));;
    


end ;;

let inner_sea nc =
   Hex_cell_set.safe_set 
     (Image.image Hex_cell.of_int_pair nc.Hex_named_connector_t.junction) ;;



end ;;



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



let enders_for_side = Private.Precomputed.enders_for_side ;;

let inner_sea = Private.inner_sea ;;




let middlers = Private.Precomputed.middlers ;;

let of_name = Private.of_name ;;

let print_out (fmt:Format.formatter) nc=
   Format.fprintf fmt "@[%s@]" (Private.to_readable_string nc);;     

let starters_for_side = Private.Precomputed.starters_for_side ;;

let to_molecular_linker nc = 
     match Hex_connector_name.to_nondefault_molecular_linker 
             nc.Hex_named_connector_t.name 
              nc.Hex_named_connector_t.apex
              nc.Hex_named_connector_t.junction with 
     None -> Hex_connector.to_default_molecular_linker (Private.forget_name nc)           
    |Some(mlclr) -> mlclr  ;; 
   

let to_readable_string = Private.to_readable_string ;;