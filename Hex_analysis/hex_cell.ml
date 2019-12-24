(* 

#use"Hex_analysis/hex_cell.ml";;

*)

let cmp=((fun (Hex_cell_t.C(s1,i1)) (Hex_cell_t.C(s2,i2)) ->
   (Total_ordering.product 
     Total_ordering.lex_for_strings Total_ordering.standard)
     (s1,i1) (s2,i2)) :> Hex_cell_t.t Total_ordering.t) ;;

let cmp_for_pairs = Total_ordering.product cmp cmp;; 

let neighbors dim (Hex_cell_t.C(s1,i1)) = 
   let j1=(int_of_char(String.get s1 0))-96 in 
   let unchecked = [
                      j1,(i1-1);(j1+1),(i1-1);
       (j1-1),i1               ;(j1+1),i1    ;
       (j1-1),(i1+1); j1,(i1+1);
   ] in 
   let checked =List.filter (
      fun (j,i) -> (1<=i) && (i<=dim) && (1<=j) && (j<=dim)
   ) unchecked in 
   Image.image (fun (j,i)->
     let s = String.make 1 (char_of_int(96+j)) in 
     Hex_cell_t.C(s,i) ) checked;;

let of_string s =
  Hex_cell_t.C(
      String.sub s 0 1,
      int_of_string(String.sub s 1 (String.length(s)-1))
  );;

let to_string (Hex_cell_t.C(s1,i1))=
  s1^(string_of_int i1);;

let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;     

let of_concrete_object crobj = 
    let (_,(arg1,arg2,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
   Hex_cell_t.C(
      Concrete_object_field.unwrap_string arg1,
      Concrete_object_field.unwrap_int arg2
   );;

let to_concrete_object (Hex_cell_t.C(s,i))= 
    Concrete_object_t.Variant("Hex_"^"cell_t.C",[Concrete_object_field.wrap_string(s);Concrete_object_t.Int(i)]);;

let pair_of_concrete_object crobj =
   let (arg1,arg2,_,_,_,_,_)=Concrete_object_field.unwrap_bounded_uple crobj in 
   (of_concrete_object arg1,of_concrete_object arg2);;


let pair_to_concrete_object (cell1,cell2)=
   Concrete_object_t.Uple [
      to_concrete_object(cell1);
      to_concrete_object(cell2)
   ];;
   

