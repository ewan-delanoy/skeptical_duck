(*

#use"Old_Van_der_Waerden/udwfw_nonempty_index.ml";;

*)

module Private = struct 

  let order_for_part (x1,y1) = function 
  (Udwfw_nonempty_index_t.Part(x2,y2)) ->
    Total_ordering.standard2 (x1,y1) (x2,y2) 
    |Helper(_,_)
    |Solution(_,_) -> Total_ordering_result_t.Lower ;;
  
  let order_for_helper (x1,y1) = function 
    (Udwfw_nonempty_index_t.Part(_,_)) ->
      Total_ordering_result_t.Greater
    |Helper(x2,y2) -> Total_ordering.standard2 (x1,y1) (x2,y2) 
      |Solution(_,_) -> Total_ordering_result_t.Greater;;  
  
  let order_for_solution (x1,y1) = function 
    (Udwfw_nonempty_index_t.Part(_,_))
    |Helper(_,_) ->
      Total_ordering_result_t.Greater
    |Solution(x2,y2) -> Total_ordering.standard2 (x1,y1) (x2,y2) ;;  
  
  let full_order idx1 idx2 = match idx1 with 
  (Udwfw_nonempty_index_t.Part(x1,y1)) ->
    order_for_part (x1,y1) idx2
    |Helper(x1,y1) -> order_for_helper (x1,y1) idx2 
    |Solution(x1,y1) -> order_for_solution (x1,y1) idx2 ;; 
  
  end ;;  

  let order = (Private.full_order:  Udwfw_nonempty_index_t.t Total_ordering_t.t);;
  
  let to_string = function 
  (Udwfw_nonempty_index_t.Part(x1,y1)) ->
    "v_"^(string_of_int x1)^"_"^(string_of_int y1)
    |Helper(x1,y1) -> "h_"^(string_of_int x1)^"_"^(string_of_int y1) 
    |Solution(x1,y1) -> "s_"^(string_of_int x1)^"_"^(string_of_int y1);; 
  
  