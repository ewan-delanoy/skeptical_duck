(*

#use"Van_der_Waerden/Width_up_to_four/vdw_nonempty_index.ml";;

*)

module Private = struct 

let order_for_usual (x1,y1) = function 
(Vdw_nonempty_index_t.Usual(x2,y2)) ->
  Total_ordering.standard2 (x1,y1) (x2,y2) 
  |Solution(_) -> Total_ordering.Lower ;;

let order_for_solution k = function 
  (Vdw_nonempty_index_t.Usual(_,_)) ->
    Total_ordering.Greater
    |Solution(k2) -> Total_ordering.for_integers k k2 ;;  

let full_order idx1 idx2 = match idx1 with 
(Vdw_nonempty_index_t.Usual(x1,y1)) ->
  order_for_usual (x1,y1) idx2
  |Solution(k) -> order_for_solution k idx2 ;; 

end ;;  

let order = (Private.full_order:  Vdw_nonempty_index_t.t Total_ordering.t);;

let to_string = function 
(Vdw_nonempty_index_t.Usual(x1,y1)) ->
  "v_"^(string_of_int x1)^"_"^(string_of_int y1)
  |Solution(k) -> "s_"^(string_of_int k);; 

