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
if (List.length beginnings)<>(List.length endings) then failwith("zzz") ;; 
let fiftuples = Image.image (fun (_,line)->extract_fiftuple_from_beginning_line  line) beginnings ;; 
let new_fiftuple = (w,s,i,component,half)  ;;

