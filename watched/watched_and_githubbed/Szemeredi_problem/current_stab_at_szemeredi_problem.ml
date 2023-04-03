(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)


open Skeptical_duck_lib ;; 
open Needed_values ;; 
open Sz3_preliminaries ;;
let sexam = Fill_Warehouse.simplest_example ;;
let slist = Fill_Warehouse.simplest_list ;;

let see0 = Overall.get_status () ;; 

open Solution_list_upper_half_mode ;;

let vz1 = visualize 1 ;;
let rf1 (B b) (S n) = slist n;;         
let check_rf1 = partial_check 1 rf1 ;;     

let vz2 = visualize 2 ;;
let rf2 (B b) (S n) = 
   if (n mod 3)= 0 
   then [(sexam (b+2))@(Int_range.range (b+3) n)] 
   else slist n;;         
let check_rf2 = partial_check 2 rf2 ;;   

let vz3 = visualize 3 ;;
let rf3 (B b) (S n) = 
   if List.mem (n mod 3) [0;1] 
   then [(sexam (b+2))@(Int_range.range (b+3) n)] 
   else slist n;;         
let check_rf3 = partial_check 3 rf3 ;;   

let vz4 = visualize 4 ;;
let rf4 (B b) (S n) = 
   [(sexam (b+2))@(Int_range.range (b+3) n)] ;;         
let check_rf4 = partial_check 4 rf4 ;;   

(* RFI BEGIN *)

let rfi (B b) (S n) = 
  if List.mem (n-b,n mod 3) [(2,0);(2,1);(2,2);(3,1);(3,2);(4,2)] 
  then slist n  
  else [(sexam (b+2))@(Int_range.range (b+3) n)] ;;   
let check_rfi = global_check rfi ;; 

(* RFI END *)

let string_of_imd (IMD i) =
    if i=0 then "" else 
    "_i"^(string_of_int i)^"_" ;;   

let string_of_intlist l=
 if l=[] then "empty" else 
 "l_"^(String.concat "_" (Image.image string_of_int l))^"_l";;

let name_for_reconstructed_function (w,s,i,half) = 
    "f_"^(string_of_int w)^"_"^ 
     (string_of_intlist s)^ 
     (string_of_imd i)^"_"^(Half.to_string half) ;;

let (w,s,i,half) = Solution_list_upper_half_mode.current_data() ;; 

let base_path = Dfa_root.connectable_to_subpath 
(Coma_big_constant.This_World.root) ;; 
let s_stab_ap = base_path ^ 
  "watched/watched_and_githubbed/Szemeredi_problem/" ^
   "current_stab_at_szemeredi_problem.ml";; 
let s_this_ap = base_path ^ 
     "lib/Szemeredi/sz3_preliminaries.ml";; 
let first_test = (Sys.file_exists s_stab_ap) && (Sys.file_exists s_this_ap) ;; 
if not first_test then () ;; 
let stab_ap = Absolute_path.of_string s_stab_ap 
and this_ap = Absolute_path.of_string s_this_ap ;; 
let text_from_stab = Io.read_whole_file stab_ap ;; 
let original_rfi_code = Cull_string.between_markers 
    ("(* RFI BEGIN *)","(* RFI END *)") text_from_stab ;; 
let f_name = name_for_reconstructed_function (w,s,i,half) ;; 


