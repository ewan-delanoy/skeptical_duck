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


(* let inactive1 = Solution_list_upper_half_mode.global_check rfi ;; *)
let c_data = Solution_list_upper_half_mode.current_data () ;; 
(* let inactive2 = Abstract_solution_list_mode.global_check c_data rfi ;; *)
let (w,s,i,half) = c_data ;;
let temp1 = Image.image (
  fun (b,n) -> ((b,n),Solution_list_seed.original (w,s,i) b n,rfi b n)
  ) (Abstract_solution_list_mode.Private.total_range (w,s,i,half)) ;; 
let temp2 = List.filter (fun (_,y1,y2)->y1<>y2) temp1 ;;
let temp3 = 
  (if temp2=[] 
   then [] 
   else
 snd(Min.minimize_it_with_care (fun (pair,_,_)->
    Range.compute_enumerator_index w pair half) temp2));;
let answer =(temp2,temp3)  ;;  


(* let inactive3 = Side_effects_after_successful_global_check.main (w,s,i,Solution_list_seed.current_component,half) ;; *)



(*
open Side_effects_after_successful_global_check ;; 

let component = Solution_list ;; 
let (w,s,i,half) = Solution_list_upper_half_mode.current_data() ;; 

let new_item = text_for_new_item (w,s,i,half) component ;;

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
let ii = compute_insertion_index new_fiftuple fiftuples;;
let max_linedex_before = fst(List.nth endings (ii-1)) ;; 
let (lines_before,lines_after)=List.partition (fun (j,line)->j<=max_linedex_before) indexed_lines ;;  
let before = String.concat "\n" (Image.image snd lines_before) ;; 
let after = String.concat "\n" (Image.image snd lines_after) ;; 
let new_wafi_text = String.concat "\n" [before;new_item;after] ;; 
Replace_inside.overwrite_between_markers_inside_file 
~overwriter:new_wafi_text markers_for_warehouse_filler this_ap ;;
*)