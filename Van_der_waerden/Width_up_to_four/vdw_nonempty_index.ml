(*

#use"Van_der_Waerden/Width_up_to_four/vdw_nonempty_index.ml";;

*)

let order = ((fun 
  (Vdw_nonempty_index_t.NE(x1,y1)) 
  (Vdw_nonempty_index_t.NE(x2,y2)) ->
    Total_ordering.standard2 (x1,y1) (x2,y2)
) :  Vdw_nonempty_index_t.t Total_ordering.t);;

let to_string (Vdw_nonempty_index_t.NE(x,y)) = 
  "v_"^(string_of_int x)^"_"^(string_of_int y) ;;