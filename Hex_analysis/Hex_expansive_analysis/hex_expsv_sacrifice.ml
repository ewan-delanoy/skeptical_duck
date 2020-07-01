(* 

#use"Hex_analysis/hex_sacrifice.ml";;

*)



let compatible_sacrifices eob = 
   let temp1 = Hex_expsv_typical_sacrifice.compatible_sacrifices eob in 
   Image.vorstellung ( fun
     (tscr,side,pairs) -> Hex_expsv_sacrifice_t.Scr (tscr,side,pairs)
   )   temp1 ;; 




let data_for_sacrificial_starter end_of_battle 
  (Hex_expsv_sacrifice_t.Scr (_,_,pairs)) = 
  Hex_expsv_typical_sacrifice.data_for_sacrificial_starter end_of_battle pairs;; 

let reconstruct_sacrificial_solutions (Hex_expsv_sacrifice_t.Scr (_,_,pairs)) mlclr=
    Hex_expsv_typical_sacrifice.reconstruct_sacrificial_solutions pairs mlclr;;

