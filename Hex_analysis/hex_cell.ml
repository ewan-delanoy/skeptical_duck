(* 

#use"Hex_analysis/hex_cell.ml";;

*)

module Private = struct 


let to_concrete_object (Hex_cell_t.C(i,j))= 
    Concrete_object_t.Variant("Hex_"^"cell_t.C",
    [Concrete_object_t.Int(i);Concrete_object_t.Int(j)]);;

let of_string untrimmed_s =
  let s=Cull_string.trim_spaces untrimmed_s in 
  let j=(int_of_char(String.get s 0))-96
  and i=int_of_string(Cull_string.cobeginning 1 s) in 
  Hex_cell_t.C(i,j);;   

let to_string (Hex_cell_t.C(i,j))=
  (String.make 1 (char_of_int(j+96)))^(string_of_int i);;


end;;


let abscissa (Hex_cell_t.C(i,j))=i;;

let allowed_range_for_translation_of_cell (Hex_dimension_t.D dim)  (Hex_cell_t.C(i,j))=
   ((1-i,dim-i),(1-j,dim-j));;

let allowed_range_for_translation_of_list formal_dim l =
  let temp1 = Image.image (allowed_range_for_translation_of_cell formal_dim) l in 
  let global_xmin = Max.list (Image.image (fun ((xmin,xmax),(ymin,ymax))->xmin) temp1)
  and global_xmax = Min.list (Image.image (fun ((xmin,xmax),(ymin,ymax))->xmax) temp1)
  and global_ymin = Max.list (Image.image (fun ((xmin,xmax),(ymin,ymax))->ymin) temp1)
  and global_ymax = Min.list (Image.image (fun ((xmin,xmax),(ymin,ymax))->ymax) temp1) in 
  ((global_xmin,global_xmax),(global_ymin,global_ymax));;

let cmp=((fun (Hex_cell_t.C(i1,j1)) (Hex_cell_t.C(i2,j2)) ->
   (Total_ordering.product 
     Total_ordering.standard Total_ordering.standard)
     (i1,j1) (i2,j2)) :> Hex_cell_t.t Total_ordering.t) ;;

let cmp_for_pairs = Total_ordering.product cmp cmp;; 

let neighbors (Hex_dimension_t.D dim) (Hex_cell_t.C(i1,j1)) = 
   let unchecked = [
                      (i1-1),j1;(i1-1),(j1+1);
       i1,(j1-1)               ;    i1,(j1+1);
       (i1+1),(j1-1); (i1+1),j1;
   ] in 
   let checked =List.filter (
      fun (i,j) -> (1<=i) && (i<=dim) && (1<=j) && (j<=dim)
   ) unchecked in 
   Image.image (fun (i,j)-> Hex_cell_t.C(i,j) ) checked;;


let of_concrete_object crobj = 
    let (_,(arg1,arg2,_,_,_,_,_))=Concrete_object.unwrap_bounded_variant crobj in 
   Hex_cell_t.C(
      Crobj_converter.int_of_concrete_object arg1,
      Crobj_converter.int_of_concrete_object arg2
   );;

let of_int_pair (i,j) =Hex_cell_t.C(i,j);;

let of_string= Private.of_string;;

let ordinate (Hex_cell_t.C(i,j))=j;;

let pair_of_concrete_object = 
     Crobj_converter_combinator.to_pair of_concrete_object of_concrete_object ;;


let pair_to_concrete_object =
   Crobj_converter_combinator.of_pair Private.to_concrete_object Private.to_concrete_object ;;

let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (Private.to_string ap);;     


let to_concrete_object = Private.to_concrete_object ;;

let to_int_pair (Hex_cell_t.C(i,j))= (i,j);;

let to_string = Private.to_string;;   

