(* 

#use"Hex_memory/hex_cell.ml";;

First coordinate is column index, second is row index

*)

let cmp=((fun (Hax_cell_t.C(s1,i1)) (Hax_cell_t.C(s2,i2)) ->
   (Total_ordering.product 
     Total_ordering.lex_for_strings Total_ordering.standard)
     (s1,i1) (s2,i2)) :> Hax_cell_t.t Total_ordering.t) ;;

let of_string s =
  Hax_cell_t.C(
      String.sub s 0 1,
      int_of_string(String.sub s 1 (String.length(s)-1))
  );;

let to_string (Hax_cell_t.C(s1,i1))=
  s1^(string_of_int i1);;

let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;     