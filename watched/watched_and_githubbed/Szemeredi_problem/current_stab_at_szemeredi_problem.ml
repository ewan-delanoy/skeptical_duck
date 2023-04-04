(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)


open Skeptical_duck_lib ;; 
open Needed_values ;; 
open Sz3_preliminaries ;;
let see0 = Overall.get_status () ;; 
open Fill_Warehouse;;
open Solution_list_upper_half_mode ;;

let vz1 = visualize 1 ;;
let rf1 (B b) (S n) = Fill_Warehouse.simplest_list n;;         
let check_rf1 = partial_check 1 rf1 ;;     

(* RFI BEGIN *)

let rfi (B b) (S n) = 
  if List.mem (n-b,n mod 3) [(2,0);(2,1);(2,2);(3,1);(3,2);(4,2)] 
  then simplest_list n  
  else [(simplest_example (b+2))@(Int_range.range (b+3) n)] ;;    

(* RFI END *)
(* let check_rfi = global_check rfi ;; *)

open Side_effects_after_successful_global_check ;; 

let component = Solution_list ;; 
let (w,s,i,half) = Solution_list_upper_half_mode.current_data() ;; 

let text1 = text_for_new_item (w,s,i,half) component ;;

print_string text1 ;; 

let base_path = Dfa_root.connectable_to_subpath 
(Coma_big_constant.This_World.root) ;; 
let s_this_ap = base_path ^ 
     "lib/Szemeredi/sz3_preliminaries.ml";; 
if not (Sys.file_exists s_this_ap) then () ;; 
let this_ap = Absolute_path.of_string s_this_ap ;; 
let this_text = Io.read_whole_file this_ap ;; 
let wafi_full_text = 
    Cull_string.between_markers markers_for_warehouse_filler
       this_text ;; 
let lines_in_wafi = Lines_in_string.lines wafi_full_text ;;        
let indexed_lines = Int_range.index_everything lines_in_wafi ;; 
let beginnings = List.filter (fun (_,line)->
    Supstring.begins_with line (fst(pre_markers_for_items))
  ) indexed_lines ;; 
let endings = List.filter (fun (_,line)->
  Supstring.begins_with line (snd(pre_markers_for_items))
) indexed_lines ;; 

let line = snd(List.nth beginnings 1) ;; 

let line = 
  "(* Beginning of item at  ( 1 , [ 2, 71 , 83 ],  IMD( 56 ), Superficial_result ,   Upper_half ) *)" ;;

let temp1 = Cull_string.two_sided_cutting (fst(pre_markers_for_items)," *)") line ;;
let temp2 = Cull_string.trim_spaces temp1 ;; 
let i1 = Substring.leftmost_index_of_in_from "," temp2 1 ;; 
let w = int_of_spaced_string(Cull_string.interval temp2 2 (i1-1)) ;; 
let i2 = Substring.leftmost_index_of_in_from "[" temp2 i1 ;; 
let i3 = Substring.leftmost_index_of_in_from "]" temp2 i2 ;; 
let scr = parse_inside_of_intlist(Cull_string.interval temp2 (i2+1) (i3-1)) ;;
let i4 = Substring.leftmost_index_of_in_from "IMD" temp2 i3 ;; 
let i5 = Substring.leftmost_index_of_in_from "(" temp2 i4 ;; 
let i6 = Substring.leftmost_index_of_in_from ")" temp2 i5 ;; 
let imd = int_of_spaced_string(Cull_string.interval temp2 (i5+1) (i6-1)) ;; 
let i7 = Substring.leftmost_index_of_in_from "," temp2 i6 ;; 
let i8 = Substring.leftmost_index_of_in_from "," temp2 (i7+1) ;; 
let i9 = Substring.leftmost_index_of_in_from ")" temp2 i8 ;; 
let component = Kind_of_component.of_string(Cull_string.trim_spaces(Cull_string.interval temp2 (i7+1) (i8-1))) ;; 
let half = Half.of_string(Cull_string.trim_spaces(Cull_string.interval temp2 (i8+1) (i9-1))) ;; 
let answer = (w,scr,imd,component,half) ;; 


