(* 

#use"Hex_analysis/hex_cell.ml";;

*)

let cmp=((fun (Hex_cell_t.C(s1,i1)) (Hex_cell_t.C(s2,i2)) ->
   (Total_ordering.product 
     Total_ordering.lex_for_strings Total_ordering.standard)
     (s1,i1) (s2,i2)) :> Hex_cell_t.t Total_ordering.t) ;;

let cmp_for_pairs = Total_ordering.product cmp cmp;; 

let of_string s =
  Hex_cell_t.C(
      String.sub s 0 1,
      int_of_string(String.sub s 1 (String.length(s)-1))
  );;

let to_string (Hex_cell_t.C(s1,i1))=
  s1^(string_of_int i1);;

let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;     