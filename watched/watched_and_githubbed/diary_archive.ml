(************************************************************************************************************************
Snippet 121 : 
************************************************************************************************************************)
open Skeptical_duck_lib ;; 
open Needed_values ;;


(************************************************************************************************************************
Snippet 120 : Multi-lingual OCR on cropped pngs 
************************************************************************************************************************)
type spanish_or_latin = Spanish | Latin ;; 

let u1 = 
  let bs = home ^ "/Downloads/Building_site/" in 
  if Sys.file_exists bs 
  then Unix_again.quick_beheaded_complete_ls bs
  else [] ;; 


let (u2,u3) = List.partition (fun fn->Supstring.ends_with fn ".png" ) u1 ;; 
exception Bad_length of string list ;;
let check_length l = if not(List.mem (List.length l) [3;4]) then raise(Bad_length(l)) else () ;; 
exception Optional_fourth_elt of string list ;; 
let look_at_optional_fourth_elt l = 
  if List.length(l)>=4 
  then let z = List.nth l 3 in 
       if z<>"sn" then raise(Optional_fourth_elt(l)) else true
  else false ;;
exception Parse_sol_exn of string list ;;   
let parse_sol l = 
  let z = List.nth l 2 in 
  match List.assoc_opt z ["l",Latin;"s",Spanish] with 
  Some sol -> sol 
  | None -> raise(Parse_sol_exn l) ;;  
let sol_to_long_string = function 
  Spanish -> "spa" 
  |Latin -> "lat" ;;   
exception Read_integers_exn of string list ;; 
let read_integers l = 
   try (int_of_string(List.nth l 0),int_of_string(List.nth l 1)) with 
   _-> raise(Read_integers_exn(l)) ;; 

let parse_filename fn =
    let temp1 = Cull_string.coending 4 fn in 
    let temp2 = Str.split (Str.regexp "_") temp1 in 
    let _ = check_length temp2 in 
    (read_integers temp2,
    (parse_sol temp2,
    look_at_optional_fourth_elt temp2)) ;;
(*    
let parse_filename_is_not_ok fn = 
   try (fun _->false)(parse_filename fn) with _ -> true ;;      
let pre_u4 = List.filter parse_filename_is_not_ok u2 ;;    
*)
let u4 = Image.image  parse_filename u2 ;; 
let order_for_ints = Total_ordering.for_integers ;; 
let order_for_int_pairs = Total_ordering.product order_for_ints order_for_ints ;; 
let final_order = Total_ordering.product order_for_int_pairs Total_ordering.standard ;; 
let u5 = Ordered.sort final_order u4 ;; 
let u6 =Int_range.index_everything u5 ;; 

let dir_for_texts = "Text_building_site/"
let num_of_pngs = (string_of_int(List.length u6)) ;; 

let write1 (main_idx,((fn_idx,inner_idx),(sol,is_beginning))) =
   let s_main = string_of_int main_idx 
   and s_idx = string_of_int fn_idx 
   and s_inner = string_of_int inner_idx 
   and long_s_sol = sol_to_long_string sol 
   and optional_ending = (if is_beginning then "_sn" else "") in 
   let full_filename = s_idx^"_"^s_inner^"_"^(Cull_string.beginning 1 long_s_sol)^optional_ending in 
   "tesseract -l "^long_s_sol^" "^full_filename^".png "^full_filename^"\n"^
   "mv "^full_filename^".txt /media/sf_Downloads/"^dir_for_texts^" \n"^
   "echo \""^s_main^" of "^num_of_pngs^":"^full_filename^"\"";;


let text1 = "\n\n\n"^(String.concat "\n" 
 (Image.image write1 u6))^"\n\n\n" ;;   
   
let act1 () = 
  let ap1 = Absolute_path.create_file_if_absent (home^"/Downloads/script.sh") in 
  Io.overwrite_with ap1 text1;;

(* After the OCR has been applied *)
 
let v1 = Unix_again.quick_beheaded_complete_ls 
  (home ^ "/Downloads/Text_building_site/") ;; 
let (v2,v3) = List.partition (fun fn->Supstring.ends_with fn ".txt" ) v1 ;; 
(*    
let parse_filename_is_not_ok fn = 
   try (fun _->false)(parse_filename fn) with _ -> true ;;      
let pre_v4 = List.filter parse_filename_is_not_ok v2 ;;    
*)
let v4 = Image.image  parse_filename v2 ;; 
let v5 = Ordered.sort final_order v4 ;; 
(* By the way, v5 = u5 *)

let pages_concerned = Ordered.sort 
 order_for_ints
 (Image.image (fun (x,_)->fst x) v5);;

let v6 = Image.image (
   fun page_nbr -> 
    (page_nbr,List.filter_map (
     fun ((page_nbr2,inner_idx),(sol,is_beginning)) ->
       if page_nbr2 = page_nbr 
       then Some(inner_idx,sol,is_beginning) 
       else None
    ) v5)
) pages_concerned ;;

let compute_footnote_data triples = 
   let temp1 = Three_parts.generic triples in 
   List.rev_map (
    fun (before,center,after)->
       let (inner_idx,sol,is_beginning) = center in 
       let number_of_beginning_markers_before =
         List.length(List.filter (fun (_,_,is_beginning2)->is_beginning2) before)   
       and is_ending = (
         match after with 
           [] -> true 
         | (_,_,is_beginning3) :: _ -> is_beginning3 
       ) in 
       let current_footnote_idx = (
          if is_beginning 
          then number_of_beginning_markers_before + 1 
          else  number_of_beginning_markers_before  
       ) in   
       (inner_idx,sol,current_footnote_idx,is_beginning,is_ending)
   ) temp1 ;; 

let v7 = Image.image (fun (page_nbr,triples) -> 
  (page_nbr,compute_footnote_data triples)
) v6;;

let v8 = Explicit.image (fun (page_nbr,fiftuples) -> 
  let temp1 = Image.image (
    fun (inner_idx,sol,footnote_idx,is_beginning,is_ending) -> 
      let s_page = string_of_int page_nbr
      and s_inner = string_of_int inner_idx 
      and long_s_sol = sol_to_long_string sol 
      and optional_ending = (if is_beginning then "_sn" else "") in 
  let full_filename = s_page^"_"^s_inner^"_"^
      (Cull_string.beginning 1 long_s_sol)^optional_ending^".txt" in 
  let beginning_part = (
     if is_beginning 
     then "\n\n[size=90][b][color=blue]("^(string_of_int footnote_idx)^")[/color][/b]"
     else "" 
  ) and end_part = (
    if is_ending 
    then "[/size]"
    else "" 
 )  in  
 let ap = Absolute_path.of_string (home ^ "/Downloads/Text_building_site/" ^ full_filename) in 
 let (tag_start,tag_end) = (if sol=Latin then ("[latin]","[/latin]") else ("","")) in 
  beginning_part^tag_start^(Io.read_whole_file ap)^tag_end^end_part
  ) fiftuples in 
  (page_nbr,String.concat "\n" temp1) 
) v7;;

let building_site = home^"/Teuliou/html_files/Translations/Building_site/";;

let old_emptiable_ap = Absolute_path.of_string (building_site^"old_emptiable_cmist.txt") ;;

let emptiable_ap = Absolute_path.of_string (building_site^"emptiable_cmist.txt") ;;

let original_text = Io.read_whole_file old_emptiable_ap ;; 

let original_pages = Percent_pagination.extract_all_pages ~verbose:true original_text ;; 

let ampersands = String.make 100 '@' ;;

let trailer = "\n\n\n"^ampersands^"\n\nREGNUM DEUS INTRA VOS EST\n\n"^ampersands^"\n\n\n" ;; 

let new_pages = Explicit.image (
  fun (page_nbr,old_content) -> 
     match List.assoc_opt page_nbr v8 with 
     None -> (page_nbr,old_content) 
     | Some new_content -> 
      (page_nbr,old_content^trailer^new_content) 
) original_pages ;; 
 
let new_text = Percent_pagination.merge_all_pages new_pages ;; 

let act2 () = Io.overwrite_with emptiable_ap new_text ;; 




(************************************************************************************************************************
Snippet 119 : Preprocessing code for a variant type
************************************************************************************************************************)
let this_root = Coma_big_constant.This_World.root ;;
let s_ap1 = (Dfa_root.connectable_to_subpath this_root) ^ "lib/Szemeredi/sz3p.ml";;
let ap1 = Absolute_path.of_string s_ap1 ;; 

let z1 = Io.read_whole_file ap1 ;; 
let z2 = Lines_in_string.interval z1 60 81 ;; 
let z3 = Lines_in_string.lines z2 ;; 
let z4 = Image.image (
  fun line -> 
    let line2 = Cull_string.trim_spaces line in 
    let line3 = Replace_inside.replace_inside_string ("|","") line2 in 
    line3 
) z3 ;;
let z4 = ["Whole"; "Superficial_result"; "Solution_list"; "Qualified_point_list";
"Qpl_length"; "Qpl_interval"; "Sr_upper_half"; "Sr_lower_half";
"Sl_upper_half"; "Sl_lower_half"; "Qpll_upper_half"; "Qpll_lower_half";
"Qpli_upper_half"; "Qpli_lower_half"; "Sr_upper_half_atomized";
"Sr_lower_half_atomized"; "Sl_upper_half_atomized"; "Sl_lower_half_atomized";
"Qpll_upper_half_atomized"; "Qpll_lower_half_atomized";
"Qpli_upper_half_atomized"; "Qpli_lower_half_atomized"] ;;
let z5 = Image.image (
  fun variant -> 
    let lowercase_variant = String.uncapitalize_ascii variant in 
    "let "^lowercase_variant^" (w,scr) = constructor "^
    "("^variant^",,,,w,scr) ;;" 
) z4 ;;
let z6 = "\n\n\n" ^ (String.concat "\n" z5) ^ "\n\n\n" ;; 

let expand_abbreviation text =
   try List.assoc text [
     "Qpl","Qualified_point_list";
     "Sr","Superficial_result";
     "Sl","Solution_list";
     "Qpll","Qpl_length";
     "Qpli","Qpl_interval";
   ] with Not_found -> text;; 

let main_array =
[
  "_length",("(List_to_length_k,");
  "_interval",("(List_to_range_k,");
  "_upper_half",("(Breadth_n_size_to_upper_half_k,");
  "_lower_half",("(Breadth_n_size_to_upper_half_k,");
  "_atomized",("(Atomize_k,");
] ;; 

let secondary_array = 
[
  "Superficial_result","Bulk_result_to_superficial_result_k";
  "Solution_list","Bulk_result_to_solution_list_k";
  "Qualified_point_list","Bulk_result_to_qualified_point_list_k";
] ;; 

let canonical_decomposition variant = 
  match List.find_map (
    fun (appendix,other_ending) -> 
      if Supstring.ends_with variant appendix 
      then let sub_variant = 
            expand_abbreviation(Cull_string.two_sided_cutting ("",appendix) variant) in 
           Some("("^variant^",Some"^other_ending ^ sub_variant ^ "));" )
      else None  
  ) main_array with 
  Some final_ending -> final_ending 
  | None -> 
    if variant = "Whole" then "(Whole,None);" else
    "("^variant^",Some("^(List.assoc variant secondary_array) ^ ",Whole));" ;;

let z7 = Image.image canonical_decomposition z4 ;;
let z8 = "\n\n\n" ^ (String.concat "\n" z7) ^ "\n\n\n" ;; 
let z9 () = print_string z8 ;; 




(************************************************************************************************************************
Snippet 118 : Doing the accounts 
************************************************************************************************************************)
let data = 
  [
     2574,"Keus";
     8400,"Tren A&M";
     1600,"Tren Ewan";
     1820,"Tren Ewan";
     2500,"Tren Ewan";
     1050,"Pakad Usana Mamm";
     1180,"Pesk";
     1212,"Keus";
     1279,"Kig";
     1184,"Amann";
     818,"Pato";
     1750,"Pesk";
     6274,"Kig";
     2000,"Bara";
     1976,"Pesk";
     2336,"Kig";
     860,"Pesk";
     675,"Chalotez da hadan";
     526,"Kig";
     1727,"Pesk";
     561,"Pato";
     1570,"Edeier";
     1297,"Keus";
     1190,"Kig";
     2659,"Gwin";
     856,"Produioù evit netaat";
     1640,"Kig";
     2263,"Kig";
     1013,"Chalotez da hadan";
     1000,"Pesk";
     1149,"Pesk";
     2656,"Kig";
     835,"Produioù evit netaat";
     2045,"Planioù pato";
     1200,"Pesk";
  ] ;; 

let total = Basic.fold_sum (Image.image fst data) ;;   

let labels = Ordered.sort Total_ordering.lex_for_strings 
(Image.image snd data) ;; 

let data2 = Image.image (
  fun lbl -> (lbl,List.filter_map (fun (x,y)->if y=lbl then Some x else None) data)
) labels ;;

let data3 = Image.image (
  fun (lbl,prices) -> (Basic.fold_sum prices,lbl) )
data2 ;;

let data4 = (List.rev(Ordered.sort Total_ordering.standard2 data3)) ;; 

let total2 = Basic.fold_sum (Image.image fst data3) ;;   

(************************************************************************************************************************
Snippet 117 : Interpolation in {0,1}^n 
************************************************************************************************************************)
(*
#require"zarith";;

#install_printer Z.pp_print ;; 
*)

let i_order = Total_ordering.for_integers ;;
let i_intersect  = Ordered.intersect i_order ;;
let i_merge  = Ordered.merge i_order ;;
let i_setminus  = Ordered.setminus i_order ;;
let i_sort  = Ordered.sort i_order ;;
let i_is_included_in = Ordered.is_included_in i_order;;

let il_order = Total_ordering.silex_compare  i_order ;;
let il_sort  = Ordered.sort il_order ;;

module Z = struct 
  type t = int ;;
  let abs x = x ;;
  let add x y = x + y;;
  let mul x y = x * y;;
  let div x y = x * y;;
  let fac x = x ;;
  let zero = 0 ;; 
  let one = 1 ;;
  let minus_one = (-1) ;; 
  let of_int x = x;;
  let equal x y = (x=y) ;; 
  let lt x y = (x<y) ;; 
  let neg x = x;;
end;;  

let z_order =((fun x y ->
   if Z.lt x y then Total_ordering_result_t.Lower else 
   if Z.lt y x then Total_ordering_result_t.Greater else  
    Total_ordering_result_t.Equal): Z.t Total_ordering_t.t) ;;

let z_intersect  = Ordered.intersect z_order ;;    
let z_sort  = Ordered.sort z_order ;;


let product_on_interval = Memoized.recursive(fun old_f (i,j,accu)->
    if i>j then accu else old_f(i,j-1,Z.mul (Z.of_int j) accu)
) ;;

let early_binomial = Memoized.make(fun (n,p)->
  Z.div(product_on_interval (n-p+1,n,Z.one))(Z.fac p)      
) ;; 

let uncurried_binomial = Memoized.make(fun (n,p)->
  if 2*p<=n 
  then early_binomial (n,p)      
  else early_binomial (n,n-p) 
) ;; 

let binomial n p = uncurried_binomial (n,p) ;; 

let alternating_sign j = if j mod 2 = 0 then Z.one else Z.minus_one ;; 

let z_fold_sum l = List.fold_left Z.add Z.zero l ;; 

let z_minimize_it f=function
[]->failwith("min on empty set undefined")
|x::y->
 let rec minimize_it0=(function
  (current_candidate,current_value,da_ober)->match da_ober with
  []->(current_candidate,current_value)
  |a::peurrest->let va=f(a) in
                if (Z.lt va current_value)
				then minimize_it0(a,va,peurrest)
				else minimize_it0(current_candidate,current_value,peurrest)
 ) 
in
 minimize_it0(x,f(x),y);;

let tf1 n j = 
   Z.mul (alternating_sign(j-1))
     (binomial(n-1)(j-1)) ;; 


let is_uniform l = let h = List.hd l in  List.for_all (fun x-> h=x) l;;

let main_image shed indices =
  (* n = List.length arr *)
   z_fold_sum(Image.image (fun k->List.nth shed (k-1)) indices);; 
 
let neighbors n l =
   let temp1 = Three_parts.complemented_points l 
   and outside_l = i_setminus (Int_range.range 1 n) l in 
   let temp2 = Image.image (
     fun (p,others)->
       Image.image (fun q->i_merge [q] others) outside_l 
   ) temp1 in 
   il_sort(List.flatten temp2) ;; 
   
(* neighbors 25 [1;2;3;4;5] ;; *)

let interesting_modifications (n,shed) (indices,img) =
  let close_neighbors = neighbors n indices in 
  List.filter_map (
     fun indices2 ->
     let new_img = main_image shed indices2 in 
     if Z.lt (Z.abs new_img) (Z.abs img)
     then Some(indices2,new_img)
     else None  
  ) close_neighbors ;;

let rec iterator (n,shed,indices,img) =
    let temp1 = interesting_modifications (n,shed) (indices,img) in 
    if temp1=[] 
    then (indices,img)
    else 
    let ((indices2,_),img2) = z_minimize_it snd temp1 in 
    iterator (n,shed,indices2,img2) ;; 

let tf2 =Memoized.make(fun n -> Int_range.scale (tf1 n) 1 n );; 

let tf3 = Memoized.make(fun (n,d)->
  let temp1 = tf2 n in 
  Int_range.scale (fun k->List.nth temp1 ((k*d) mod n) ) 1 n 
) ;; 

let seed n r d= 
  let temp1 = Int_range.range 1 r 
  and temp2 = tf3(n,d) in 
  (n,temp2,temp1,main_image temp2 temp1);; 

let ff n r d= iterator (seed n r d) ;; 

let u1 = tf2 25 ;; 
let u1_pos = Int_range.scale (fun t->List.nth u1 (2*t)) 0 12 ;;  
let u1_neg =  Int_range.scale (fun t->(Z.neg(List.nth u1 (2*t-1)))) 1 12 ;;  

let s1_pos=z_sort u1_pos ;; 
let s2_pos=z_sort(Image.image (fun (a1,a2)->z_fold_sum [a1;a2])
   (Uple.list_of_pairs u1_pos)) ;;
let s3_pos=z_sort(Image.image (fun (a1,a2,a3)->z_fold_sum [a1;a2;a3])
(Uple.list_of_triples u1_pos)) ;;
let s4_pos=z_sort(Image.image (fun (a1,a2,a3,a4)->z_fold_sum [a1;a2;a3;a4])
(Uple.list_of_fourtuples u1_pos)) ;;


let s1_neg=z_sort u1_neg ;; 
let s2_neg=z_sort(Image.image (fun (a1,a2)->z_fold_sum [a1;a2])
   (Uple.list_of_pairs u1_neg)) ;;
let s3_neg=z_sort(Image.image (fun (a1,a2,a3)->z_fold_sum [a1;a2;a3])
(Uple.list_of_triples u1_neg)) ;;
let s4_neg=z_sort(Image.image (fun (a1,a2,a3,a4)->z_fold_sum [a1;a2;a3;a4])
(Uple.list_of_fourtuples u1_neg)) ;;


let g1 = z_intersect s1_pos s4_neg ;; 
let g2 = z_intersect s2_pos s3_neg ;;
let g3 = z_intersect s3_pos s2_neg ;;
let g4 = z_intersect s4_pos s1_neg ;;

let test (a1,a2,a3) = List.for_all (fun x->List.mem x [0;1])
[a1 + (-3*a2 + 3*a3); 3*a1 + (-8*a2 + 6*a3); 6*a1 + (-15*a2 + 10*a3)];;
let res1 = List.filter test (Cartesian.cube [0;1]) ;; 

let v0 = [1;-24] ;; 
let v1 = v0 @ v0 ;; 
let v2 = Listennou.power_set v1 ;; 
let v3 = i_sort (Image.image Basic.fold_sum v2) ;; 

let v0 = [1;-24;276] ;; 
let v1 = v0 @ v0 ;; 
let v2 = Listennou.power_set v1 ;; 
let v3 = i_sort (Image.image Basic.fold_sum v2) ;; 





(************************************************************************************************************************
Snippet 116 : Interpolation in {0,1}^n 
************************************************************************************************************************)
(*
#require"zarith";;

#install_printer Z.pp_print ;; 
*)

module Container116 = struct
module Z = struct 
  let abs x = x ;;
  let add x y = x + y;;
  let mul x y = x * y;;
  let div x y = x * y;;
  let fac x = x ;;
  let zero = 0 ;; 
  let one = 1 ;;
  let minus_one = (-1) ;; 
  let of_int x = x;;
  let equal x y = (x=y) ;; 
  let lt x y = (x<y) ;; 
end;;  

let product_on_interval = Memoized.recursive(fun old_f (i,j,accu)->
    if i>j then accu else old_f(i,j-1,Z.mul (Z.of_int j) accu)
) ;;

let early_binomial = Memoized.make(fun (n,p)->
  Z.div(product_on_interval (n-p+1,n,Z.one))(Z.fac p)      
) ;; 

let uncurried_binomial = Memoized.make(fun (n,p)->
  if 2*p<=n 
  then early_binomial (n,p)      
  else early_binomial (n,n-p) 
) ;; 

let binomial n p = uncurried_binomial (n,p) ;; 

let alternating_sign j = if j mod 2 = 0 then Z.one else Z.minus_one ;; 

let z_fold_sum l = List.fold_left Z.add Z.zero l ;; 

let z_minimize_it f=function
[]->failwith("min on empty set undefined")
|x::y->
 let rec minimize_it0=(function
  (current_candidate,current_value,da_ober)->match da_ober with
  []->(current_candidate,current_value)
  |a::peurrest->let va=f(a) in
                if (Z.lt va current_value)
				then minimize_it0(a,va,peurrest)
				else minimize_it0(current_candidate,current_value,peurrest)
 ) 
in
 minimize_it0(x,f(x),y);;

let tf1 n j = 
   Z.mul (alternating_sign(j-1))
     (binomial(n-1)(j-1)) ;; 

let tf2 =Memoized.make(fun n -> Int_range.scale (tf1 n) 1 n );; 

let is_uniform l = let h = List.hd l in  List.for_all (fun x-> h=x) l;;

let main_image n arr =
  (* n = List.length arr *)
   let temp1 = List.combine arr (tf2 n) in 
   z_fold_sum(Image.image (fun (v,z_nbr)->
      if v=0 then Z.zero else z_nbr
    ) temp1);;
   
let try_modify_at_index (n,arr,v_arr) j = 
    let indexed_arr = Int_range.index_everything arr in 
    let modified_arr = Image.image (fun (k,v)->
       if k=j then 1-v else v
      ) indexed_arr in 
    if is_uniform modified_arr then None else 
    let new_v_arr= main_image n modified_arr in 
    if Z.lt (Z.abs new_v_arr) (Z.abs v_arr)
    then Some(modified_arr,new_v_arr)
    else None ;;        

let interesting_modifications (n,arr,v_arr) =
   List.filter_map (try_modify_at_index (n,arr,v_arr)) (Int_range.range 1 n) ;;

let rec iterator (n,arr,v_arr) =
    let temp1 = interesting_modifications (n,arr,v_arr) in 
    if temp1=[] 
    then (arr,v_arr)
    else 
    let ((arr2,_),v_arr2) = z_minimize_it snd temp1 in 
    iterator (n,arr2,v_arr2) ;; 

let seed n = 
  let temp1 = Int_range.scale (fun k->if k=(n/2) then 1 else 0) 1 n in 
  (n,temp1,main_image n temp1);; 

let ff n = iterator (seed n) ;; 

end ;;

(************************************************************************************************************************
Snippet 115 : Musings on primes of the form 5p+2
************************************************************************************************************************)
let oi = Total_ordering.for_integers ;; 
let i_sort = Ordered.sort oi ;; 

let u1 = 
[2; 4; 7; 8; 14; 16; 17; 28; 32; 34; 37; 47; 49; 56; 64; 67; 68; 74; 94; 97; 98; 107; 112; 119; 127; 128; 134; 136; 137; 148; 157; 167; 188; 194; 196; 197; 214; 224; 227; 238; 254; 256; 257; 259; 268; 272; 274; 277; 289; 296; 307; 314; 317; 329; 334; 337; 343; 347; 367; 376; 388; 392; 394; 397; 428; 448; 454; 457; 467; 469; 476; 487; 508; 512; 514; 518; 536; 544; 547; 548; 554; 557; 577; 578; 587; 592; 607; 614; 617; 628; 629; 634; 647; 658; 668; 674; 677; 679; 686; 694; 727; 734; 749; 752; 757; 776; 784; 787; 788; 794; 797; 799; 827; 833; 856; 857; 877; 887; 889; 896; 907; 908; 914; 934; 937; 938; 947; 952; 959; 967; 974; 977; 997; 1016; 1024; 1028; 1036; 1072; 1087; 1088; 1094; 1096; 1097; 1099; 1108; 1114; 1117; 1139; 1154; 1156; 1169; 1174; 1184; 1187; 1214; 1217; 1228; 1234; 1237; 1256; 1258; 1268; 1277; 1294; 1297; 1307; 1316; 1327; 1336; 1348; 1354; 1358; 1367; 1369; 1372; 1379; 1388; 1427; 1447; 1454; 1468; 1487; 1498; 1504; 1514; 1552; 1567; 1568; 1574; 1576; 1588; 1589; 1594; 1597; 1598; 1607; 1627; 1637; 1649; 1654; 1657; 1666; 1667; 1697; 1712; 1714; 1739; 1747; 1754; 1774; 1777; 1778; 1787; 1792; 1799; 1813; 1814; 1816; 1819; 1828; 1847; 1867; 1868; 1874; 1876; 1877; 1894; 1904; 1907; 1918; 1934; 1939; 1948; 1954; 1987; 1994; 1997; 2017; 2023; 2027; 2032; 2048; 2056; 2072; 2087; 2137; 2144; 2149; 2159; 2174; 2176; 2188; 2192; 2194; 2198; 2207; 2209; 2216; 2219; 2228; 2234; 2237; 2267; 2278; 2287; 2297; 2303; 2308; 2312; 2329; 2338; 2347; 2348; 2357; 2359; 2368; 2374; 2377; 2401; 2417; 2428; 2429; 2434; 2437; 2447; 2456; 2467; 2468; 2474; 2477; 2479; 2512; 2516; 2536; 2554; 2557; 2569; 2588; 2594; 2614; 2617; 2632; 2647; 2654; 2657; 2669; 2672; 2677; 2687; 2696; 2707; 2708; 2716; 2734; 2738; 2744; 2758; 2767; 2776; 2777; 2779; 2797; 2837; 2839; 2854; 2857; 2887; 2894; 2897; 2908; 2917; 2927; 2936; 2957; 2974; 2996; 3008; 3028; 3037; 3067; 3104; 3134; 3136; 3137; 3148; 3149; 3152; 3167; 3176; 3178; 3187; 3188; 3194; 3196; 3199; 3214; 3217; 3254; 3257; 3269; 3274; 3283; 3298; 3307; 3308; 3314; 3332; 3334; 3347; 3349; 3394; 3407; 3409; 3424; 3428; 3457; 3467; 3478; 3494; 3508; 3517; 3527; 3547; 3548; 3554; 3556; 3557; 3574; 3584; 3589; 3598; 3607; 3617; 3626; 3628; 3632; 3637; 3638; 3656; 3677; 3694; 3697; 3727; 3734; 3736; 3748; 3752; 3754; 3767; 3788; 3797; 3808; 3814; 3829; 3836; 3847; 3859; 3868; 3877; 3878; 3896; 3899; 3907; 3908; 3917; 3947; 3959; 3967; 3974; 3988; 3994; 4007; 4027; 4034; 4039; 4046; 4054; 4057; 4064; 4096; 4109; 4112; 4127; 4144; 4157; 4174; 4177; 4217; 4249; 4274; 4288; 4297; 4298; 4318; 4319; 4327; 4337; 4348; 4352; 4357; 4369; 4376; 4384; 4388; 4396; 4397; 4403; 4414; 4418; 4432; 4438; 4447; 4456; 4457; 4468; 4474; 4489; 4507; 4517; 4529; 4534; 4547; 4556; 4559; 4567; 4574; 4594; 4597; 4606; 4616; 4624; 4637; 4657; 4658; 4676; 4694; 4696; 4699; 4709; 4714] ;;


let u2 = Uple.list_of_pairs u1 ;; 
let unordered_u3 = Image.image (fun (x,y)->y-x) u2 ;; 
let u3 = i_sort unordered_u3 ;; 
let u4 = Explicit.image 
(fun d->(d,List.filter (fun (x,y)->y-x=d) u2)) u3;;
let (u5,u6) = Max.maximize_it_with_care (fun (_,l)->List.length l) u4;;

let all_good x l = List.for_all (fun d->List.mem (x-d) u1) l ;; 


let v2 = List.filter (fun (x,y)->all_good x [840;600]) u2;;
let unordered_v3 = Image.image (fun (x,y)->y-x) v2 ;; 
let v3 = i_sort unordered_v3 ;; 
let v4 = Explicit.image 
(fun d->(d,List.filter (fun (x,y)->y-x=d) v2)) v3;;
let (v5,v6) = Max.maximize_it_with_care 
(fun (_,l)->List.length l) v4;;

(*

let v2 = List.filter (fun (x,y)->List.mem (x-240) u1) u2;;
let unordered_v3 = Image.image (fun (x,y)->y-x) v2 ;; 
let v3 = i_sort unordered_v3 ;; 
let v4 = Explicit.image 
(fun d->(d,List.filter (fun (x,y)->y-x=d) v2)) v3;;
let (v5,v6) = Max.maximize_it_with_care 
(fun (_,l)->List.length l) v4;;

*)

(************************************************************************************************************************
Snippet 114 : Test the Por_space.add_subclass feature 
************************************************************************************************************************)
let print_out_field (fmt:Format.formatter) fd=
   Format.fprintf fmt "@[%s@]" (fd.Por_field_t.field_name);;

(* #install_printer print_out_field ;; *)

let gfd k = {
  Por_field_t.field_name = "fd"^(string_of_int k);
  field_type = "";
  var_name ="";
  default_value ="";
  crobj_converters =None;
} ;;

let gscl (parent_opt,scl_name,field_indices) = {
  Por_subclass_t.subclass_name = scl_name;
   subclass_fields = Image.image gfd field_indices; 
   parent = parent_opt;
   extensions_leading_here = [];
   has_restriction = false;
   has_constructor = false;
} ;; 


let g1= gscl (Some "g7","g1",[1]);;
let g2= gscl (Some "g6","g2",[2]);;
let g3= gscl (Some "g7","g3",[3]);;
let g4= gscl (Some "g1","g4",[4]);;
let g5= gscl (Some "g3","g5",[5]);;
let g6= gscl (Some "g7","g6",[6]);;
let g7 = gscl (None,"g7",[7]);;

let origin = 
  let home = Sys.getenv "HOME" in 
  let file_there = (fun s-> 
    Absolute_path.create_file_if_absent(home^"/Teuliou/OCaml/skeptical_duck/lib/Filewatching/"^s^".ml")) in 
 {
   Por_space_t.main_type_name = "main" ;
   module_name = "main" ;
   subclasses = [] ;
   dependencies = [] ;
   type_signature_file = (file_there "gw_poly_t") ;
   implementation_file = (file_there "gw_poly") ;
   has_crobj_conversion = false ;
   incomplete_extensions = [];
} ;;  

let main_ref = ref origin ;; 
Por_space.add_extension main_ref "g7" g1;;
Por_space.add_extension main_ref "g6" g2;;
Por_space.add_extension main_ref "g7" g3;;
Por_space.add_extension main_ref "g1" g4;;
Por_space.add_extension main_ref "g3" g5;;
Por_space.add_extension main_ref "g7" g6;;

let old_por = (!main_ref) ;;
let new_por = Por_space.Private.add_subclass_on_nonref old_por g7;; 

(*
let scl = g7 ;;
let old_subclasses = old_por.Por_space_t.subclasses 
and scl_name = scl.Por_subclass_t.subclass_name ;;
let initial_complete_subclasses = old_subclasses @ [scl] 
and initial_incomplete_subclasses = old_por.Por_space_t.incomplete_extensions ;;
let bad1 = Por_common.exhaust_possible_linkings
~complete:initial_complete_subclasses
~incomplete:initial_incomplete_subclasses ;;
let v0 = (false,List.rev initial_complete_subclasses,initial_incomplete_subclasses) ;;
let ff = Memoized.small 
Por_common.pusher_for_possible_linkings_exhaustion v0 ;; 
let gg n = let (b,a,c) = ff n in (b,
Image.image (fun sc->sc.Por_subclass_t.subclass_name) a,
Image.image (fun (_,sc)->sc.Por_subclass_t.subclass_name) c
);;
*)

Por_space.add_subclass main_ref g7;;

(************************************************************************************************************************
Snippet 113 : Periodically remove a file in a fixed directory.
************************************************************************************************************************)
let calc () = let _ = Ordered.sort Total_ordering.silex_for_intlists
  (Listennou.power_set (Int_range.range 1 16)) in ();;

Chronometer.it calc () ;; 

let g1 = "rm -f "^home^"/Downloads/Building_site/Older_pages/Untitled*.png" ;;

let circular () = 
  let counter = ref 0 in 
  while true do let _ = Sys.command g1 in 
  calc();
  counter:=((!counter)+1);
   print_string((string_of_int(!counter)));
   print_string("\n");
   flush stdout ;
  done ;;

(************************************************************************************************************************
Snippet 112 : Successive renamings
************************************************************************************************************************)
(*    
let z1 = ae () ;; 
let z2 = Image.image (fun el->
  (Dfa_module.to_line(Dfn_endingless.to_module el),el)) z1 ;; 
let z3 = List.filter (
  fun (mn,el)->
      Supstring.begins_with mn "opor_"
) z2 ;; 
*)

let renamings = ref [
  "Old_polymorphic_ocaml_record_t","Por_types";
  "Opor_public_definition_t","Por_public_definition_t";
  "Opor_common","Por_common";
  "Opor_private_component","Por_private_component";
  "Opor_public_definition","Por_public_definition";
  "Opor_public_component","Por_public_component";

  "fw_poly","gw_poly" ;
  "fw_configuration","gw_configuration" ;
  "file_watcher","gw_life_watcher" ;
  "fw_with_archives","gw_with_archives" ;
  "fw_with_small_details","gw_with_small_details" ;
  "fw_with_dependencies","gw_with_dependencies" ;
  "fw_with_batch_compilation","gw_with_batch_compilation" ;
  "fw_with_githubbing","gw_with_githubbing" ;
  "github_configuration","gw_guthib_configuration";
] ;;

let rename_in_ap ap =
    Replace_inside.replace_several_inside_file (!renamings) ap ;; 

let ap1 = Absolute_path.of_string 
"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_common.ml";;  

let ap2 = Absolute_path.of_string 
"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_private_component.ml";;  

let ap3 = Absolute_path.of_string 
"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_public_definition.ml";;

let ap4 = Absolute_path.of_string 
"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_public_component.ml";;

let ap5 = Absolute_path.of_string 
"watched/watched_and_githubbed/prepare_gw_poly.ml";;

let ap6 = Absolute_path.of_string 
"watched/watched_and_githubbed/prepare_gw_with_dependencies.ml";;

let z4 = 
["opor_public_definition_t"; "opor_common"; "opor_private_component";
 "opor_public_definition"; "opor_public_component"] ;;


dm "opor_public_definition_t" "por_public_definition_t" ;;
dm "opor_common" "por_common" ;;
dm "opor_private_component" "por_private_component" ;;
dm "opor_public_definition" "por_public_definition" ;;
dm "opor_public_component" "por_public_component" ;;

(************************************************************************************************************************
Snippet 111 : Cleanup unused modules
************************************************************************************************************************)
open Needed_values ;; 

let z1 = ae () ;; 
let z2 = Image.image (fun el->
  (Dfa_module.to_line(Dfn_endingless.to_module el),el)) z1 ;; 
let z3 =  (Explicit.image (fun (mn,el)->
  (mn,vfm mn,el)  
)) z2 ;; 
let z4 = List.filter_map (fun (mn,v,el)->
    if v = [] then Some(mn,el) else None
  ) z3 ;; 
let pre_z5 = Image.image fst z4 ;;   
let kept_modules = [
  "yp_token_info"; "yp_flag_parsing_php"; "tested_module_four";
   "tested_module_five"; "sy_token_info_t"; "buenzli_uutf";
   "legendre_symbol"; 
   "self_contained_module_copy"; "node_project"; 
   "tools_for_debugging"; "yp_php_lexer";
   "por_types"
] ;; 
let z6 = List.filter (fun x->not(List.mem x kept_modules)) pre_z5 ;;

let check_z6 = List.filter (fun x->bel(x)<>[]) z6 ;;

let cs1 = Usual_coma_state.Private.main_ref ;;


(************************************************************************************************************************
Snippet 110 : Construct a get_variant_name function from a long type definition 
************************************************************************************************************************)
let ap1 = Absolute_path.of_string "lib/Padioleau/yp_php_lexer.mll" ;; 
let old_text = Io.read_whole_file ap1;;
let (before_u1,u1,after_u1) = Lines_in_string.tripartition_associated_to_interval old_text 1142 1320 ;; 
let u2 = Lines_in_string.lines u1 ;; 
let u3 = Image.image (fun line->
  Option.get(Cull_string.before_and_after " of " line)) u2 ;; 
let u4 = Ordered.sort Total_ordering.silex_for_strings  (Image.image snd u3) ;; 
let u5 = [
  "Yp_token_info_t.t", "(_)"; 
  "Yp_token_info_t.t ", "(_)"; 
  "bool * Yp_token_info_t.t", "(_,_)";
  "string * Yp_token_info_t.t", "(_,_)";
  "int option * Yp_token_info_t.t", "(_,_)";
  "float option * Yp_token_info_t.t", "(_,_)";] ;;
let u6 = Image.image (
   fun (before_ov,after_ov) ->
     let (_,name) = Option.get(Cull_string.before_and_after "| " before_ov) in 
     let circled = List.assoc after_ov u5 in 
     before_ov^circled^" -> \""^name^"\""
) u3 ;;
let u7 = String.concat "\n" u6 ;; 
let new_text = before_u1 ^ u7 ^ after_u1 ;; 
Io.overwrite_with ap1 new_text ;; 

(************************************************************************************************************************
Snippet 110 : PARI-GP code to compute an explicit primitive element for a Galois extension with group S5
************************************************************************************************************************)
open Needed_values ;;


let tf1 (i,j,vn) =
  let si = string_of_int i 
  and sj = string_of_int j in 
  "for_a"^si^"=make_zero(harry[1]["^sj^"],a"^si^");\n"^
  "harry=subst(harry,a"^si^",for_a"^si^");\n"^
  "printf(\" Step \");"^"printf("^si^");printf(\" for "^vn^" done \\n\");" ;;

let tf2 k vn= (tf1(2*k-1,2*k-1,vn))^"\n"^(tf1(2*k-2,2*k,vn)) ;; 

let tf3 a b vn = String.concat "\n" (Int_range.scale (fun j->
  let k = (a+1)-j in tf2 k vn
) 1 ((a+1)-b));; 

let tf4 vn =
   (tf3 60 1 vn)^"\n"^
   "arr_for_"^vn^"=harry;";; 

let z1 = String.concat "\n\n\n" (Int_range.scale (fun j->tf4("x"^(string_of_int(5-j)))) 1 3);;

let ap1 = Absolute_path.of_string
(home^"/Teuliou/Bash_scripts/Pari_Programming/my_pari_code/follenn2.gp");;

Io.append_string_to_file z1 ap1 ;;  

(************************************************************************************************************************
Snippet 108 : Musings on permutations
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let i_intersect  = Ordered.intersect i_order ;;
let i_merge  = Ordered.merge i_order ;;
let i_setminus  = Ordered.setminus i_order ;;
let i_sort  = Ordered.sort i_order ;;
let i_is_included_in = Ordered.is_included_in i_order;;

let il_order = Total_ordering.silex_compare  Total_ordering.for_integers ;;
let il_sort  = Ordered.sort il_order ;;

let current_order = 5 ;;
let base = Permutation.iii current_order ;;

let eval_list_permutation sigma k = List.nth sigma (k-1) ;;

let compose_list_permutations sigma1 sigma2 = 
   Int_range.scale (fun k-> eval_list_permutation sigma1 (eval_list_permutation sigma2 k)) 1 current_order ;;

let inverse_of_list_permutation sigma =
    let temp1 = Int_range.index_everything sigma in 
    let temp2 = Image.image (fun (x,y)->(y,x)) temp1 in 
    Int_range.scale (fun k->List.assoc k temp2) 1 current_order ;;  

let uncurried_compose = Memoized.make(fun (i,j) ->
   let sigma1 = List.nth base (i-1)   
   and sigma2 = List.nth base (j-1) in 
   Listennou.find_index (compose_list_permutations sigma1 sigma2) base
);;     

let compose  i j = uncurried_compose (i,j) ;;

let fold_compose l = List.fold_left compose 1 l ;; 

let inverse = Memoized.make(fun i->
    let sigma = List.nth base (i-1)   in 
    Listennou.find_index (inverse_of_list_permutation sigma) base
  ) ;; 

let uncurried_commutator =  Memoized.make(fun (x,y)->
  let ix = inverse x
  and iy = inverse y   in 
  compose x (compose y (compose ix iy))
) ;;  

let commutator x y = uncurried_commutator (x,y) ;; 

let commutators arbitrary_set =
    let square = Cartesian.square arbitrary_set in 
    Image.image uncurried_commutator square ;;

let uncurried_conjugate =  Memoized.make(fun (x,y)->
  let ix = inverse x  in 
  compose x (compose y ix)
) ;;  

let conjugate x y = uncurried_conjugate (x,y) ;; 

let conjugate_of_set_by ly x= i_sort (Image.image (conjugate x) ly) ;; 

let base_size = List.length base ;;

let visualize_permutation k =
   let sigma = List.nth base (k-1) in 
   String.concat "" (Image.image string_of_int sigma) ;;


let all_conjugates_of_set ly =
   il_sort(Int_range.scale (conjugate_of_set_by ly) 1 base_size) ;; 

let rec helper_for_order (k,small,big) =
   if big = 1 
   then k 
   else helper_for_order (k+1,small,compose small big) ;; 

let order = Memoized.make (fun k->helper_for_order (1,k,k)) ;; 

let rec helper_for_power (remaining,small,big) =
  if remaining = 0 
  then big
  else helper_for_power (remaining-1,small,compose small big) ;; 

let nonnegative_power x k = helper_for_power (k-1,x,x) ;; 

let power x k =
   if k=0 then 1 else 
   if k<0 
   then nonnegative_power (inverse x) (-k)  
   else nonnegative_power x k ;; 
    
let compose_powers l = fold_compose (Image.image (fun (x,k)->power x k) l) ;;  

let order = Memoized.make (fun k->helper_for_order (1,k,k)) ;;

let subset_product l1 l2 =
    let temp1 = Cartesian.product l1 l2 in 
    let temp2 = Image.image uncurried_compose temp1 in 
    i_sort temp2 ;; 

let rec helper_for_generated_subgroup (treated,seed) = 
      let possibly_new = subset_product treated seed in 
      let really_new = i_setminus possibly_new treated in 
      if really_new = [] 
      then treated 
      else let new_whole = i_merge really_new treated in 
            helper_for_generated_subgroup (new_whole,seed) ;;
        
let generated_subgroup seed = helper_for_generated_subgroup ([1],seed) ;; 

let derived_subgroup sg = generated_subgroup (commutators sg) ;; 

let trivial_subgroup = [1] ;;
let full_subgroup = Int_range.range 1 base_size ;;

let level1  = 
  il_sort (Int_range.scale (fun k->generated_subgroup [k]) 2 base_size) ;; 

let pre_level2 = 
    let temp1 = Uple.list_of_pairs level1 in 
    let temp2 = Image.image (fun (a,b)->generated_subgroup(a@b)) temp1 in 
    il_sort temp2 ;;

let (next_to_level2,level2) = List.partition (fun x->List.mem x level1) pre_level2 ;;

let pre_level3 = 
  let temp1 = Cartesian.product level1 level2 in 
  let temp2 = List.filter (fun (x,y)->not(i_is_included_in x y)) temp1 in 
  let temp3 = Image.image (fun (a,b)->generated_subgroup(a@b)) temp2 in 
  il_sort temp3 ;;

let (next_to_level3,level3) = List.partition (fun x->List.mem x level2) pre_level3 ;;  

let all_subgroups = trivial_subgroup :: (level1 @ level2) ;; 

let subgroups_of_given_group sg =
   List.filter (fun sg2->(i_is_included_in sg2 sg)&&(sg2<>sg)) all_subgroups ;; 

let frobenius_test big_sg small_sg = 
   if small_sg = [1] then false else 
    List.for_all (fun g->
      if List.mem g small_sg 
      then true 
      else  
      let csg = conjugate_of_set_by small_sg g in 
      (i_intersect small_sg csg=[1])
    ) big_sg ;;

let frobenius_subgoups big_sg =
    List.filter (frobenius_test big_sg) (subgroups_of_given_group big_sg);;    

type formal_subgroup = FSG of int ;; 

module Formal_subgroup = struct 

let full_group = FSG(List.length all_subgroups) ;;   
let of_list l = FSG(Listennou.find_index l all_subgroups) ;; 
let to_list (FSG k) = List.nth all_subgroups (k-1) ;;
let derived_subgroup = Memoized.make (fun fsg ->
      of_list(derived_subgroup(to_list fsg))
) ;;
let all = Int_range.scale (fun k->FSG k) 1 (List.length all_subgroups) ;; 
let is_solvable = Memoized.recursive (fun old_f fsg ->
  if fsg = FSG 1 then true else 
  let der_fsg = derived_subgroup  fsg in 
  if der_fsg = fsg 
  then false 
  else old_f der_fsg
) ;;

end ;;  

let z1 = Explicit.image Formal_subgroup.derived_subgroup Formal_subgroup.all ;;
let z2 = List.filter Formal_subgroup.is_solvable Formal_subgroup.all ;;

let z3 = Image.image Formal_subgroup.to_list z2 ;; 
let maximalities = Ordered_misc.maximal_elts_wrt_inclusion (il_sort z3) ;; 
let z4 = Explicit.image (fun x->(x,all_conjugates_of_set x)) maximalities ;;
let maximalities_up_to_conjugation = List.filter_map (fun (x,y)->if x=List.hd y then Some x else None) z4;;

let four_cycles = List.filter (fun k->order(k)=4) 
   (Int_range.range 1 base_size) ;;
let z5 = Image.image (
   fun k-> 
     let sg = generated_subgroup (i_sort [34;k]) in 
     (Formal_subgroup.of_list sg,k)
) four_cycles ;;
let z6 = Listennou.partition_according_to_fst z5 ;; 
let z7 = Image.image snd z6 ;; 
let common_cycles = List.hd z7 ;; 



let view_as_product_of_cycles k =
   let sigma = List.nth base (k-1) in 
   Permutation.decompose_into_disjoint_cycles sigma ;; 

let ivp = Image.image view_as_product_of_cycles ;; 

let tf1 sigma k = conjugate (power 34 k) sigma ;; 
let tf2 sigma =
   let sigma_inv = inverse sigma in 
   let temp1 = Int_range.scale (tf1 sigma) 0 4 
   and temp2 = Int_range.scale (tf1 sigma_inv) 0 4 in 
   (ivp temp1,ivp temp2,i_sort(temp1@temp2)) ;; 

let z8 = tf2 11 ;; 
let (z9,z10,z11) = tf2 10 ;; 
let other_cycles = i_setminus common_cycles z11 ;; 
let z12 = tf2 18 ;; 

let sg_t = generated_subgroup [34] ;; 
let base2 = Cartesian.product (Int_range.range 0 4) (Int_range.range 0 3) ;; 

let x0 = 18 and y0 = 10 ;; 


let sg_x0 = generated_subgroup [x0] ;; 
let sel_x0 w =
   List.find (fun (i,j)->compose(power 34 i)(power x0 j)=w) base2 ;;

let z13 = Cartesian.product sg_t sg_x0 ;; 
let z14 = Image.image (fun (u,v)->(compose u v,(u,v))) z13 ;;
let mixed1 = i_sort(Image.image fst z14);;
let z15 = Cartesian.square mixed1 ;; 
let z16 = List.filter (fun (u,v)->compose u v = y0) z15 ;; 
let z17 = Image.image (fun (u,v)->(sel_x0 u,sel_x0 v)) z16;;

let see1 = compose_powers [34,2;10,2;34,1;10,1] ;;
let see2 = compose_powers [34,1;18,1;34,2;18,2] ;;

let z18 = Cartesian.tproduct sg_t common_cycles sg_t ;; 
let z19 = Image.image (fun (u,v,w)->(fold_compose [u;v;w],(u,v,w))) z18 ;;
let z20 = i_sort (Image.image fst z19) ;; 
let z21 = i_setminus z20 common_cycles ;; 
let z22 = List.filter (fun p->fst(p)=25) z19;;

let act_on_int i_sigma k = List.nth (List.nth base (i_sigma-1)) (k-1) ;; 
let act_on_intlist i_sigma l =  i_sort(Image.image (act_on_int i_sigma) l) ;;
let collective_on_intlist i_sigma  ll = il_sort (Image.image (act_on_intlist i_sigma) ll);;

let power5 = il_sort (Listennou.power_set (Int_range.range 1 5) );;
let triples = List.filter (fun x->List.length x=3) power5 ;; 

let frob = generated_subgroup [11;34] ;;
let frob_on_intlist l = il_sort (Image.image (fun f->act_on_intlist f l) frob) ;; 
(************************************************************************************************************************
Snippet 107 : Mass renamings, using the Coherent_pdf module
************************************************************************************************************************)
(*
open Skeptical_duck_lib ;; 
*)

open Needed_values ;;

Coherent_pdf.workspace_directory := (home ^ "/Downloads/Building_site/Older_pages/") ;;
let current_letter = ref "q" 
and current_offset = ref 100 ;; 

(current_letter:="p";current_offset:=100) ;;

(current_letter:="r";current_offset:=200) ;;

(current_letter:="s";current_offset:=300) ;;

let see_commands () = Coherent_pdf.Command.mass_rename 
   ~old_prefix:(!current_letter) ~new_prefix:"p" (!current_offset);; 

let act () = Coherent_pdf.mass_rename 
  ~old_prefix:(!current_letter) ~new_prefix:"p" (!current_offset);; 

let see_indices () = Coherent_pdf.present_indices "p" ;; 

Coherent_pdf.implode ("p","") ;; 
  
Coherent_pdf.workspace_directory := (home ^ "/Downloads/MOM/Suilding_bite") ;;


(************************************************************************************************************************
Snippet 106 : Musing on discrepancy problem
************************************************************************************************************************)
module Container106 = struct 
module Z = struct

type t = Big of int ;;
let of_int x = Big x;;  
let to_int (Big x) = x;;

end ;;  

module Q = struct 

type t = { num : Z.t; den : Z.t; } ;;  

let pp_print (fmt:Format.formatter) (q:t) = () ;;   
let of_int i = {num = (Z.of_int i); den = (Z.of_int 1)} ;;
let div q1 q2 = q1 ;;
let max q1 q2 = q1 ;;
let min q1 q2 = q1 ;;
let (>=) q1 q2 = true ;;

end ;;  

(*
#require"zarith";;
*)

(*
#install_printer Z.pp_print ;; 
*)


let i_order = Total_ordering.for_integers ;; 
let i_setminus = Ordered.setminus Total_ordering.for_integers ;;
let i_sort = Ordered.sort Total_ordering.for_integers ;;

let qfrac_of_pair (i,j) = Q.div (Q.of_int i) (Q.of_int j) ;; 
let qfrac_to_pair q = (Z.to_int q.Q.num,Z.to_int q.Q.den) ;; 

let q1 = qfrac_of_pair (4,7) ;; 

let first_pfrac = (1,2) ;; 
let next_pfrac (k,n) =
   if k=n then (1,n+1) else (k+1,n) ;; 

module Q_interval = struct 

type t = I of Q.t * Q.t ;;

let intersect (I(a1,b1)) (I(a2,b2)) =
   let a = Q.max a1 a2 and b = Q.min b1 b2 in 
   if Q.(>=) a b then None else Some(I(a,b)) ;;       

let of_pfrac (k,n) =
    I(qfrac_of_pair (k-1,n),qfrac_of_pair (k,n)) ;; 

end ;;  


type localizer = L of (int * Q_interval.t) list ;;  

module Localizer = struct 
  
type t = localizer ;;   
  
let rec helper_for_pair_insertion (treated,(x,itv),to_be_treated) =
    match to_be_treated with 
    [] -> Some (List.rev_append treated [x,itv])
    | (x2,itv2) :: others ->
        if x2<x 
        then  helper_for_pair_insertion ((x2,itv2)::treated,(x,itv),others) 
        else
        if x<x2 
        then  Some (List.rev_append treated ((x,itv)::to_be_treated))
        else  (match Q_interval.intersect itv itv2 with 
                None -> None
              | Some itv3 -> Some (List.rev_append treated ((x,itv3)::others))
             ) ;;

let insert_pair pair (L l) =
    match helper_for_pair_insertion ([],pair,l) with 
      None -> None 
      | Some new_l -> Some (L new_l) ;;              

end ;;  

type walker =  W of (((int*int)* int) list)* Localizer.t ;; 

module Walker = struct 

type t = walker ;; 

let impose_value (k,n) j (W(pairs,lclzr)) =
    let itv = Q_interval.of_pfrac (k,n) in 
    match Localizer.insert_pair (j,itv) lclzr with 
    None -> None 
    |Some new_lclzr -> Some(W(((k,n),j)::pairs,new_lclzr)) ;;

let descendants walker =
   let (W(pairs,lclzr)) = walker in 
   let (k0,n0) = (match pairs with 
     [] -> first_pfrac
     |(predecessor,_) :: _ -> next_pfrac predecessor 
   ) in 
   let already_used_indices = i_sort (List.filter_map (
     fun ((_,n),j) -> if n=n0 then Some j else None 
   ) pairs) 
   and whole = Int_range.range 1 n0 in 
   let unused_indices = i_setminus whole already_used_indices in 
   List.filter_map (
    fun j-> impose_value (k0,n0) j  walker
   ) unused_indices ;;  

end ;;

module Walker_list = struct 

let rec descendants = function 
  [] -> []
  | walker :: others ->
      let trial1 = Walker.descendants walker in 
      if trial1 = []
      then descendants others 
      else trial1 @ others ;;      

let origin = [W([],L[])];;

let main = Memoized.small descendants origin ;; 

end ;;  

let ff = Walker_list.main ;; 

let u1 = ff 10000 ;; 
let (W(u2,L u3)) = List.hd u1 ;; 
let u4 = List.rev u2 ;; 
let gg n = List.filter_map 
   (fun ((_,m),j)->if m=n then Some j else None) u4 ;;

let tt n =  (Walker.descendants(List.hd(ff n))=[]) ;;  

let z1 = List.filter tt (Int_range.range 1 2000) ;;

end ;; 

(************************************************************************************************************************
Snippet 105 : Examples of "translating" ppx_deriving into usual OCaml code
************************************************************************************************************************)
(*
let stork1 = {
  FileHandler.logs_folder="suzanne"; 
        truncate=false; 
        file_perms=567; 
        date_prefix = Some "vega"; 
        versioning= Some 71; 
        suffix ="Bart";
} ;;     

let stork2 = 
{
  FileHandler.logs_folder="amy"; 
        truncate=true; 
        file_perms=567; 
        date_prefix = None; 
        versioning= None; 
        suffix ="macdonals";
} ;;     

let bee1 = {file_handlers=stork1} ;; 
let bee2 = {file_handlers=stork2} ;; 

let wasps = Image.image config_to_yojson [bee1;bee2] ;;

let clis = Image.image (fun lev -> {level = lev}) 
[Debug;Trace;Info;NoLevel] ;;

let clos = Image.image cli_json_params_to_yojson clis ;; 

let check6= List.filter (fun cfg->
   (cli_json_params_of_yojson cfg)<>(cjo cfg)
  ) clos ;;

let check5= List.filter (fun cfg->
   (cli_json_params_to_yojson cfg)<>(cjt cfg)
  ) clis ;;

let check4= List.filter (fun cfg->
   (config_of_yojson cfg)<>(config__of_yojson cfg)
  ) wasps ;;

let check3 = List.filter (fun cfg->
   (config_to_yojson cfg)<>(config__to_yojson cfg)
  ) [bee1;bee2] ;;

let check1 = List.filter (fun cfg->
   (config_to_yojson cfg)<>(stork_to_yojson cfg)
  ) [stork1;stork2] ;;

let starks = Image.image config_to_yojson [stork1;stork2] ;;

let check2 = List.filter (fun yj->
  (config_of_yojson yj)<>(stork_of_yojson yj)
 ) starks ;;
*)

(************************************************************************************************************************
Snippet 104 : Local modularization
************************************************************************************************************************)
open Needed_values ;; 
let path1 = home^"/Downloads/OCaml_packages/calendar-3.0.0/src" ;; 

let u1 = rf (path1^"/depend.txt") ;;
let u2 = Str.split (Str.regexp_string " ") u1;;
let u3 = List.filter (fun s->Supstring.ends_with s ".ml") u2;;
let u4 = Image.image (fun s->
    Absolute_path.of_string (path1^"/"^s)
  ) u3;;
let u5 = Modularize.modularize_several "" u4 ;; 
let u6 = "module CalendarLib = struct \n\n" ^ u5 ^ "\n\nend ;;" ;;  

let path2 = home^"/Teuliou/OCaml/Ordinary/Fads/Extract_php_lexer_from_padioleau/More_complete_version";; 
let ap1 = Absolute_path.of_string (path2^"/wrapped_calendarlib.ml") ;;
Io.overwrite_with ap1 u6 ;; 

(************************************************************************************************************************
Snippet 103 : Copy large interval of text from a file to another
************************************************************************************************************************)
let ap1 = Absolute_path.of_string "Githubbed_archive/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;
let ap1 = Absolute_path.of_string "Fads/nap.ml" ;;
Lines_in_string.duplicate_interval_in_file (228,287) ap1 ;; 

let ap2 = Absolute_path.of_string "Fads/pan.ml";; 

Manage_diary.extract_at_index_and_append_to_file 95 ap2 ;;


(************************************************************************************************************************
Snippet 102 : Sorting and comparing two overlapping list fo files
************************************************************************************************************************)
let ap1 = Absolute_path.of_string "~/Downloads/temp.html" ;; 
let text1 = Io.read_whole_file ap1 ;; 


let left_tag = "[{\"text\":\"" ;;
let n1 = String.length left_tag ;; 
let u1 = Substring.occurrences_of_in left_tag text1 ;; 
let u2 = Image.image (
  fun i->
    let j = Substring.leftmost_index_of_in_from "\"" text1 (i+n1) in 
    Cull_string.interval text1 (i+n1) (j-1)
) u1 ;;
let u3 = Ordered.sort Total_ordering.lex_for_strings u2 ;;

let bad_beginnings = [
" ";" "; " "; " ";
"Enregistrer ";"La description";"Le titre "; "Placer ";
"Pour";"Remarque "; "Revenir "; "Si ";"Sous";"Supprimer"; "Tout "; "Traitement";"Vidéo";"Visible"; "VISIONN"; "Voulez-";    
"Fermer";"Vérifiez ";"Vous "] ;;
let bad_contents = ["\\";"Titre";"Terminé";"Sermons";
"Plus";"Diminuer la vitesse de lecture"; "Définir comme miniature de la playlist";"Définir comme série officielle"; "Déplacer vers le haut";"Effectuer des recherches vocales"; "En attente de l'autorisation…";"En attente..."; "En cours de lecture"; "Enregistrer";"Enregistrer dans une playlist"; "Enregistrer dans \\";"Faire un zoom arrière"; "Faire un zoom avant"; "Général";"94";"Lecture"; "Lecture aléatoire"; "Lecture/Pause";"Mettre en ligne une vidéo"; "Micro désactivé. Veuillez réessayer.";"Mise à jour aujourd'hui"; "Non répertoriée";"Panoramique vers la droite"; "Panoramique vers la gauche";
"Panoramique vers le bas"; "Paramètres de la playlist"; "Passer au direct";"Privée"; "Publique"; "Raccourcis clavier"; "Rechercher";
"Abbé Interesse"; "Accueil YouTube"; "Accéder au chapitre précédent";
   "Accéder au chapitre suivant"; "Activer/désactiver le mode plein écran";
   "Activer/Désactiver le lecteur réduit";
   "Activer/Désactiver le mode cinéma"; "Activer/Désactiver le son";
   "Ajouter des nouvelles vidéos au début de la playlist";
   "Ajouter des vidéos"; "Ajouter à la file d'attente";
   "Alterner entre les différents niveaux d'opacité de la fenêtre";
   "Alterner entre les différents niveaux d'opacité du texte";
   "Alterner entre les tailles de police (augmentation)";
   "Alterner entre les tailles de police (diminution)"; "Annuler";
   "Appuyez sur le micro pour réessayer";
   "Atteindre un moment spécifique de la vidéo (7 correspond à 70 % de la durée)";
   "Aucune connexion"; "Aucune description"; "Augmenter la vitesse de lecture";
   "Autoriser l'intégration";
   "Autorisez l'accès au micro pour effectuer des recherches vocales";
   "Avancer de 10 secondes"; "Collaborer";
   "Commande non comprise. Veuillez réessayer."; "Description";
   "Image précédente (en pause)"; "Image suivante (en pause)";
   "Ignorer"; "Ignorer les liens de navigation";
] ;; 
let u4 = List.filter (
  fun w->(List.for_all (
    fun v-> not(Supstring.begins_with w v) 
  ) bad_beginnings)
  &&
  (not(List.mem w bad_contents))
) u3 ;;

let u5 = 
  ["01 1er Dimanche de l’Avent et de l’Année Liturgique - Vendée - 01-12-2013 -";
  "02 IIe  Dimanche de l’Avent Moulins 09 12 1990";
  "03 IIIe  Dimanche de l’Avent Anjou 16 12 1990 La joie dans le Seigneur, moteur de la vie chrétienne.";
  "031 IVe Dimanche de Carême Joie dans la penitence et Montée du Carmel 10 03 91 26min06";
  "032 Ier Dimanche de la Passion RévéIation progressive de la Divinité de NS Moulins 05 04 1992 40min0";
  "035 Pâques Moulins 04 04 1996 44min07";
  "036 Dimanche in albis Quasi modo Les corps glorieux Tours 02 04 1989 19min22";
  "037 IIe Dimanche apres Pâques Miséricorde et Justice de Dieu équilibre de l'esprit chrétien";
  "038 IIIe Dimanche après Pâques Modicum et videbitis Me ND de l'Epine Mayenne 21 04 2013 26min12";
  "040 Ve Dimanche après Pâques Dieu consolé par les Siens Tours 30 04 1989";
  "046 Dimanche dans l'Octave de l'Ascension 2014 Vendée";
  "047 Pentecôte  Moulins 19 05 1991";
  "050 Très Sainte Trinité Moulins 16 Juin 1990";
  "051 Fête du Très Saint Sacrement Moulins 02 06 1991";
  "052 Dimanche dans l’octave du Très Saint Sacrement N.D. de l’Epine 02 06 2013";
  "054 Dimanche dans l’octave du Sacré-Coeur N.D. de l’Epine 09 06 2013";
  "055 IVe Dimanche après la Pentecôte Moulins 16 06 1991";
  "056 Ve Dimanche après la Pentecôte  Sur la réparation et componction Tours 18 06 1989";
  "057 L’Empereur Saint Henri I et la sanctification dans le monde - Moulins 15-07-1990";
  "058. VIIe D. Ap Pent. ND de l’Epine 07 07 2013 Contre les loups déguisés sous des peaux de brebis...";
  "059 Saint Bonaventure et le bonheur en Dieu seul - Moulins - 14-07-1991";
  "06 Vigile Nativité - Choix et goûts de Dieu - Moulins 24-12-1990 min52";
  "060 IXe Dimanche après la Pentecôte - Mayenne ND de l’Epine - 21-07-2013";
  "065 XIIIe Dimanche après la Pentecôte 18 08 2013";
  "066 XIVe Dimanche après la Pentecôte - Saint Louis - 25-08-1991";
  "067 XVe Dimanche après la Pentecôte - Moulins 16-09-1990";
  "069 XVIIe Dimanche après la Pentecôte ND de l’Epine 16 09 2018";
  "070 XVIIIe Dimanche après la Pentecôte ND de l’Epine 8 10 2017";
  "071 XIXe Dimanche après la Pentecôte Sur la colère 24-09-1989 à Tours";
  "072. XXe Dimanche après la Pentecôte 22 10 2017 Notre Dame de l’Epine";
  "073 Fête du Christ-Roi - à Moulins - 27 10 1991";
  "074 XXIe Dimanche après la Pentecôte  En Vendée - 6/11/2011";
  "076 XXIIIe Dimanche après la Pentecôte - sur le Purgatoire - Moulins 11-11-1990";
  "08 Messe de Minuit - Anniversaire naissance de la fille aînée de l'Eglise -Moulins 25-12-1996";
  "081 Solennité de Notre Dame des Victoires du Très Saint Rosaire - Tours 08 10 1989";
  "082 Maternité Divine de Notre Dame 11 10 2015 Mayenne ND de l'Epine";
  "083 Toussaint - Moulins - 01 11 1996";
  "084 Commémoration des fidèles défunts - Vendée 02-11-2012";
  "088 Découverte de la Sainte Croix - ND de l'Epine 3 05 2015";
  "09 Messe du jour de Noël - Moulins 25-12-1996";
  "091 VIe  D. après la Pentecôte Solennité des Saints Apôtres Pierre et Paul  Moulins 30 06 1991";
  "092  Fête de Sainte Anne  Mayenne ND de l'Epine  26 07 2015";
  "094  Fête de Saint Luc - Mayenne ND de l’Epine - 18-10-2015";
  "100 Très Précieux Sang Mayenne ND de l’Epine 1er juillet 2013";
  "101 Nativité de Notre Dame Moulins 08-09-1991";
  "102 Notre-Dame des Sept Douleurs - 15-09-1996";
  "103 Solennité de Sainte Thérèse de l’Enfant Jésus et de la Ste Face - Moulins 30-09-1990";
  "104 Solennité de Saint Michel Archange Moulins 29 09 1991";
  "106 VIe D. ap. Pent - Fructueuse ou infructueuse Communion à N. S. - ND de l’Epine - 12-7-2020";
  "115 VIIIe Dimanche après la Pentecôte - Sur la remise des dettes - 7-8-2011  - Mayenne";
  "116 XVIe Dimanche après la Pentecôte ND de l’Epine 20 09 2020";
  "12 Dimanche dans l'Octave de la Nativité - Dum medium silentium... - Tours 31 12 1989";
  "135 Fête de l'Evangéliste Saint Marc 25 04 2021";
  "139 Pentecôte Vendée 27 05 2018";
  "141 Saint Apôtre Jacques le Majeur - Mayenne ND de l’Epine 25-07-2021";
  "142  Coeur Immaculé de Marie N.D. de l’Epine 22 08 2021";
  "147 XVIe Dimanche après la Pentecôte 25 09 2022 Notre Dame de l’Epine";
  "15 Circoncision - Vendée 01 01 2013";
  "152 Fête de Saint Michel Archange 29 09 2019";
  "16 Saint Nom de Jésus - Vendée 02-01-2013"; "17 Epiphanie - 06-01-1996";
  "19 Dimanche dans l'octave de l'Epiphanie - Manifestation de la Divinité de NS - Moulins 13-01-1991";
  "20 La Sainte Famille Moulins 07 01 1996";
  "21 IIe Dimanche après l'Epiphanie Mayenne ND de l'Epine 19 01 2014";
  "22  IIIe Dimanche après l'Epiphanie - Noli vinci a malo, sed vince in bono malum - Tours 25-01-1990";
  "23 Septuagésime Deux genres de conversion Moulins 27 01 1991";
  "24 Sexagésime - ND de Lourdes -Montée de l'esprit anti-chrétien et apparitions  - Moulins 11-02-1996";
  "25 Quinquagésime Annonce prophétique de la Passion - Moulins 10 02 1991";
  "61 Xe  D. ap. la Pent. Sur le Principe et Fondement des exercices de Saint Ignace Tours - 31-07-1988";
  "62 XIe Dimanche après la Pentecôte - 04-08-1991 Moulins";
  "75 XXIIe Dimanche après la Pentecôte - ND de l’Epine - 21 10 2018";
  "77.  Dernier Dimanche après la Pentecôte  -  Moulins  26-11-1989";
  "78 IVe Dimanche après l’Epiphanie - Tempête apaisée - Moulins 5/11/1989 - (XXVe D. Ap. P.)";
  "78 XXIVe Dimanche après la Pentecôte (IVe ap Eph) - Tempête apaisée - Moulins 1992";
  "79 Ve Dimanche après l’Epiphanie -Sur le bon grain et l’ivraie - Moulins 4/02/1990 -";
  "79 XXVe D. après la Pent. (Ve ap Epiph) -Moulins 04 02 1990- Parabole sur le bon grain et l’ivraie.";
  "80 XXVIe Dimanche après la Pentecôte (VIe ap Epiphanie) - 17 11 1991";
  "86 Fête de l’Immaculée Conception de la Très Sainte Vierge - Moulins 8-12-1989-";
  "87 Présentation de NS au temple et Purification légale de Marie -Mayenne ND de l’Epine - 02-02-2014";
  "93 Saint Laurent, Diacre et Martyr - Vendée - 10-08-2014";
  "96 Assomption Moulins 15 08 1991";
  "99 Saint Joachim, Père de la Très Sainte Vierge Marie - Vendée - 16-08-2015";
  "Dimanche des Rameaux En Vendee 01 04 2012";
  "Ier Dimanche de Carême Sur la Pénitence. Moulins 04/03/1990. 24min53";
  "Ier Dimanche de Carême. Sens mystique des montées vers Jérusalem 16/02/1997 15min18";
  "IIIème dimanche de Carême. Contre le démon muet. Moulins, 10/03/1996, 26min07";
  "IIème Dim. de Carême. Transfiguration. Équilibre entre désolations et consolations.";
  "IVe  Dimanche de l’Avent - Moulins 23-12-1990 -  Comparaison du temps des Patriarches avec le nôtre.";
  "IVe D. après Pâques :  Attachement apostolique à NS et remontée de sa nature humaine à sa Divinté.";
  "IVe Dimanche de Carême - Laetare - Définition de la Charité - Tours - 05-03-1989";
  "Sermon Jeudi Saint 13 04 2017 31min27";
  "Sermon sur l'Ascension à Moulins 24 05 1990";
  "Solennité du Sacré Coeur et Saint Jean Baptiste - Il faut qu'Il croisse et que je diminue 24/6/1990";
  "Veillée pascale Sur l'illogisme de l'attitude actuelle des Juifs talmudistes Moulins 11 04 1998"]
;;
(*
let check_u5 = u5=u4;;
*)

let (already_numbered_titles,nonnumbered_titles) =
  List.partition (
    fun s->let c= int_of_char(String.get s 0) in 
      (48<=c)&&(c<=57)
  ) u5 ;;

exception Ios_exn of string;;

let ios x =
   try   int_of_string x with _ -> 
    if Supstring.ends_with x "."
    then int_of_string(Cull_string.coending 1 x)  
    else raise(Ios_exn(x)) ;; 

let unordered_numbered_titles_part1 = Image.image (
  fun s ->
     let k = Substring.leftmost_index_of_in " " s in
     (ios (Cull_string.beginning (k-1) s),
     Cull_string.cobeginning k s
     )
) already_numbered_titles ;;
let pair_order = Total_ordering.product 
    Total_ordering.for_integers Total_ordering.lex_for_strings ;;
let numbered_titles_part1 = 
   Ordered.sort  pair_order  unordered_numbered_titles_part1 ;;
let indices_in_part1 = Image.image fst numbered_titles_part1 ;; 
let indices_not_in_part1 =
   Ordered.setminus Total_ordering.for_integers
   (Int_range.range 1 143) indices_in_part1 ;;




(*
let dir2 = Directory_name.of_string 
  "/Volumes/Matroska/Video/Abbe_Interesse/";;
let naive_v1 = More_unix.beheaded_simple_ls dir2 ;; 
*)
let v1 = 
  ["001_Ier_D_de_l_Avent_01_12_2013_27min33.mp4";
  "002_IIe_dimanche_de_l_Avent_Moulins_09_12_1990_35min32.mp4";
  "003_IIIe_dimanche_de_l_Avent_Gaudete_En_Anjou_16_12_1990_29min26.mp4";
  "004_IIIe_dim_de_l_Avent_Vendee_16_12_2018.mp4";
  "005_IVe_dimanche_de_l_Avent_Comparaison_du_temps_des_Patriarches_avec_le_notre_Moulins_23_12_1990_28min19_Copie_en_conflit_de_debian_2019_11_13.mp4";
  "006_Vigile_Nativite_Choix_et_gouts_de_Dieu_Moulins_24_12_1990_18min52.mp4";
  "007_Vigile_de_Noel_En_Vendee_24_12_2012_29min31.mp4";
  "008_Messe_de_Minuit_Anniversaire_naissance_de_la_fille_ainee_de_l_Eglise_Moulins_25_12_1996_28min04.mp4";
  "009_Messe_du_jour_de_Noel_Divinite_du_Christ_Seigneur_demontree_par_S_Paul_aux_Hebreux_a_partir_de_l_Ancien_Testament_Moulins_25_12_1996_19min48.mp4";
  "010_Nativite_Messe_de_minuit_En_Vendee_25_12_2012_14min17.mp4";
  "011_Nativite_Messe_du_jour_En_Vendee_25_12_2012_11min26.mp4";
  "012_Dimanche_dans_l_Octave_de_la_Nativite_Dum_medium_silentium_Tours_31_12_1989_21min22.mp4";
  "013_Dimanche_dans_l_octave_de_NoeI_30_12_2012_21min08.mp4";
  "014_Dimanche_dans_l_Octave_de_NoeI_ND_de_l_Epine_29_12_2019_28_min.mp4";
  "015_Circoncision_En_Vendee_01_01_2013_26min22.mp4";
  "016_Saint_Nom_de_Jesus_En_Vendee_02_01_2013_22min24.mp4";
  "017_Epiphanie_06_01_1996_38min27.mp4";
  "018_Epiphanie_07_01_90_29min34.mp4";
  "019_Octave_de_l_Epiphanie_Manifestation_de_la_Divinite_de_NS_Moulins_13_01_1991_38min17.mp4";
  "020_La_Sainte_Famille_Moulins_07_01_1996_31min33.mp4";
  "021_IIe_dimanche_apres_l_Epiphanie_Mayenne_ND_de_l_Epine_19_01_2014_30min50.mp4";
  "022_IIIe_dimanche_apres_l_Epiphanie_Noli_vinci_a_malo_sed_vince_in_bono_malum_25_01_90_34min31.mp4";
  "023_Septuagesime_Deux_genres_de_conversion_Moulins_27_01_1991_28min27.mp4";
  "024_Sexagesime_ND_de_Lourdes_Montee_de_l_esprit_anti_chretien_et_apparitions_de_ND_Moulins_11_02_1996_33min36.mp4";
  "025_Quinquagesime_Annonce_prophetique_de_la_Passion_Moulins_10_02_1991_24min39.mp4";
  "026_Ier_dimanche_de_Careme_Sens_mystique_des_montees_vers_Jerusalem_16_02_1997_15min18.mp4";
  "027_Ier_dimanche_de_Careme_Sur_la_Penitence_Moulins_04_03_1990_24min53.mp4";
  "028_IIe_Dim_de_Careme_Transfiguration_Equilibre_entre_desolations_et_consolations_Moulins_24_02_1991_34min49.mp4";
  "029_IIIe_dimanche_de_Careme_Contre_le_demon_muet_Moulins_10_03_1996_26min07.mp4";
  "030_IIIe_dimanche_de_Careme_Sur_l_Annonciation_Maternite_virginale_et_voeu_de_virginite_22_03_92.mp4";
  "031_IVe_dimanche_de_Careme_Joie_dans_la_penitence_et_Montee_du_Carmel_10_03_91_26min06.mp4";
  "032_Ier_dimanche_de_la_Passion_ReveIation_progressive_de_la_divinite_de_NS_Moulins_05_04_1992_40min02.mp4";
  "033_Dimanche_des_Rameaux_En_Vendee_01_04_2012_16min12.mp4";
  "034_Veillee_pascale_Sur_l_illogisme_de_l_attitude_actuelle_des_Juifs_talmudistes_Moulins_11_04_1998_26min11.mp4";
  "035_Paques_Moulins_04_04_1996_44min07.mp4";
  "036_Dimanche_in_albis_Quasi_modo_Les_corps_glorieux_Tours_02_04_1989_19min22.mp4";
  "037_IIe_dimanche_apres_Paques_Misericorde_et_Justice_de_Dieu_equilibre_de_l_esprit_chretien_Fete_de_saint_Pierre_de_Verone_29_04_1990_30min12.mp4";
  "038_IIIe_dimanche_apres_Paques_Modicum_et_videbitis_Me_ND_de_l_Epine_Mayenne_21_04_2013_26min12.mp4";
  "039_IVe_dimanche_apres_Paques_Attachement_apostolique_a_NS_pour_remonter_de_sa_nature_humaine_a_sa_Divinite_28_04_1991_23min03.mp4";
  "040_Ve_dimanche_apres_Paques_Dieu_console_par_les_siens_Tours_30_04_1989_20min42.mp4";
  "041_Ascension_24_05_1990_25min18.mp4";
  "042_Ascension_Moulins_09_05_1991_27min55.mp4";
  "043_Ascension_2011_Vendee_20min32.mp4";
  "044_Ascension_ND_de_l_Epine_10_05_2018_26min12.mp4";
  "045_Dimanche_dans_l_Octave_de_l_Ascension_12_05_2013_Mayenne_ND_de_l_Epine_15min28.mp4";
  "046_Dimanche_dans_l_Octave_de_l_Ascension_2014_Vendee_19min.mp4";
  "047_Pentecote_Moulins_19_05_1991_29min18.mp4";
  "048_Pentecote_Vendee_19_05_2013_30min45.mp4";
  "049_Pentecote_ND_de_l_Epine_15_05_2016_30min_50.mp4";
  "050_Tres_Sainte_Trinite_Moulins_16_06_1990_33min05.mp4";
  "051_Fete_du_Tres_Saint_Sacrement_Moulins_02_06_1991_28min53.mp4";
  "052_Dimanche_dans_l_octave_du_Saint_Sacrement_Notre_Dame_de_l_Epine_2_06_2013_14min26.mp4";
  "053_Solennite_du_Sacre_Coeur_et_Saint_Jean_Baptiste_Il_faut_qu_Il_croisse_et_que_je_diminue_Moulins_24_06_1990_24min55.mp4";
  "054_Dimanche_dans_l_Octave_du_Sacre_Coeur_29min09.mp4";
  "055_IVe_dimanche_apres_la_Pentecote_Moulins_16_06_1991_31min54.mp4";
  "056_Ve_dimanche_apres_la_Pentecote_Sur_reparation_et_componction_Tours_18_06_1989_34min59.mp4";
  "057_VIe_dimanche_apres_la_Pentecote_Saint_Henri_et_la_sanctification_dans_le_monde_Moulins_15_07_1990_37min39.mp4";
  "058_VIIe_Dimanche_apres_la_Pentecote_Mayenne_ND_de_l_Epine_07_07_2013_36min04.mp4";
  "059_VIIIe_dimanche_apres_la_Pentecote_Saint_Bonaventure_et_le_bonheur_en_Dieu_seul_Moulins_14_07_1991_42min06.mp4";
  "060_IXe_dimanche_apres_la_Pentecote_Mayenne_ND_de_l_Epine_21_07_2013_34min55.mp4";
  "061_Xe_dimanche_apres_la_Pentecote_Sur_le_Principe_et_Fondement_Tours_31_07_1988_22min08.mp4";
  "062_XIe_dimanche_apres_la_Pentecote_04_08_1991_33min37.mp4";
  "063_XIe_Dimanche_apres_Pentecote_12_08_2012_15min44.mp4";
  "064_XIIe_dimanche_apres_la_Pentecote_11_08_2013_27min32.mp4";
  "065_XIIIe_dimanche_apres_la_Pentecote_18_08_2013_27min34.mp4";
  "066_XIVe_dimanche_apres_la_Pentecote_Saint_Louis_25_08_1991.mp4";
  "067_XVe_dimanche_apres_la_Pentecote_Foi_en_la_Divinite_de_NS_Moulins_16_09_90_27min23.mp4";
  "068_XVIe_dimanche_apres_la_Pentecote_03_09_89_24min15.mp4";
  "069_XVIIe_Dimanche_apres_la_Pentecote_ND_de_l_Epine_16_09_2018_40_min.mp4";
  "070_XVIIIe_Dim_apres_la_Pentecote_ND_de_l_Epine_8_10_2017_38min30.mp4";
  "071_XIXe_dimanche_apres_la_Pentecote_Sur_la_colere_24_09_89_22min01.mp4";
  "072_XXe_D_ap_Pent_22_10_2017_ND_de_l_Epine_1h.mp4";
  "073_Fete_du_Christ_Roi_27_10_1991_27min22.mp4";
  "074_XXIe_dimanche_apres_Pentecote_25min24.mp4";
  "075_XXIIe_Dimanche_apres_la_Pentecote_ND_de_l_Epine_21_10_2018_36_min_35.mp4";
  "076_XXIIIe_dimanche_apres_la_Pentecote_Sur_le_Purgatoire_11_11_90.mp4";
  "077_XXIVe_et_dernier_dimanche_apres_la_Pentecote_Moulins_26_11_1989_26min38.mp4";
  "078_XXIVe_ap_Pent_IVe_ap_Eph_Tempete_apaisee_Moulins_29min55.mp4";
  "079_XXVe_dim_ap_Pent_Ve_ap_Epiphanie_Moulins_04_02_1990_20min.mp4";
  "080_XXVIe_dim_apres_la_Pentecote_VIe_ap_Epiphanie_17_11_1991_30min38.mp4";
  "081_Solennite_du_Tres_Saint_Rosaire_Tours_08_10_1989_23min31.mp4";
  "082_Maternite_Divine_de_Notre_Dame_11_10_2015_Mayenne_ND_de_l_Epine_28min20.mp4";
  "083_Toussaint_Moulins_01_11_1996_30min49.mp4";
  "084_Commemoration_des_defunts_Vendee_2_novembre_2012_21min07.mp4";
  "085_Fete_de_la_Dedicace_de_Saint_Jean_de_Latran_09_11_2014_33min47.mp4";
  "086_Solennite_de_l_Immaculee_Conception_Moulins_1989_30min05.mp4";
  "087_Presentation_de_NS_au_temple_et_Purification_de_Marie_Mayenne_ND_de_l_Epine_02_02_2014_23min13.mp4";
  "088_Decouverte_de_la_Sainte_Croix_ND_de_l_Epine_3_05_2015_34min28.mp4";
  "089_St_Philippe_et_st_Jacques_ND_de_l_Epine_11_05_2014_18min42_.mp4";
  "090_Solennite_de_Ste_Jeannes_d_Arc_Moulins_13_05_1990_27min49.mp4";
  "091_VIe_dimanche_apres_la_Pentecote_Solennite_de_St_Pierre_et_St_Paul_Monde_conquis_de_haute_lutte_par_papes_et_martyrs_Moulins_30_06_1991_42min50.mp4";
  "092_Fete_de_Sainte_Anne_Mayenne_ND_de_l_Epine_26_07_2015_42min41.mp4";
  "093_Saint_Laurent_diacre_et_martyr_Vendee_10_08_2014_33min24.mp4";
  "094_Fete_de_saint_Luc_Mayenne_ND_de_l_Epine_18_10_2015_27min09.mp4";
  "095_Sur_la_maniere_de_precher_1988_22min32.mp4";
  "096_Assomption_Moulins_15_08_1991_29min15.mp4"; "097_Assomption_2012.mp4";
  "098_Assomption_2013_Vendee_21min34.mp4";
  "099_Saint_Joachim_Pere_de_la_TS_Vierge_Marie_Vendee_16_08_2015_24min03.mp4";
  "100_Tres_Precieux_Sang_Mayenne_ND_de_l_Epine_1er_juillet_2013_27min03.mp4";
  "101_Nativite_de_Notre_Dame_Moulins_08_09_1991_30min28.mp4";
  "102_Notre_Dame_des_sept_douleurs_15_09_1996_15min51.mp4";
  "103_Solennite_de_Ste_Therese_de_l_Enfant_Jesus_et_de_la_Ste_Face_Moulins_30_09_1990_22min47.mp4";
  "104_Solennite_de_saint_Michel_Archange_Moulins_29_09_1991_32min31.mp4";
  "105_IVe_D_ap_Pentecote_ND_de_l_Epine_28_juin_2020_35min.mp4";
  "106_VIe_D_ap_Pentecote_ND_de_l_Epine_12_7_2020_39min25.mp4";
  "107_refutation_T_de_M_ete_2013_Intro_10min.mp4";
  "108_refutation_T_de_M_ete_2013_partie_1_18min19.mp4";
  "109_refutation_T_de_M_ete_2013_partie_2_18min41.mp4";
  "110_refutation_T_de_M_ete_2013_partie_3_23min22.mp4";
  "111_refutation_T_de_M_ete_2013_partie_4_14min54.mp4";
  "112_refutation_T_de_M_ete_2013_partie_5_21min57.mp4";
  "113_refutation_T_de_M_ete_2013_partie_6_21min51.mp4";
  "114_refutation_T_de_M_ete_2013_partie_7_17min02.mp4";
  "115_VIIIe_D_Ap_Pent_7_8_2011_a_La_Boutouere_Mayenne_21min57.mp4";
  "116_XVIe_Dimanche_apres_la_Pentecote_ND_de_lEpine_20_09_2020_22min14.mp4";
  "117_Christ_Roi_25_10_2020_ND_de_lEpine_45min29.mp4";
  "130_Presentation_des_ouvrages_de_labbe_Interesse_video_013_1h01min40s.mkv";
  "131_debat_celier_zins_30min47.mp4";
  "132_Jeudi_Saint_Vendee_2015_14min53.mp4";
  "133_Jeudi_Saint_2016_5min32.mp4";
  "134_Sermon_Jeudi_Saint_13_04_2017_31min27.mp4";
  "135_Fete_de_lEvangeliste_Saint_Marc_25_04_2021.mp4";
  "136_refutation_T_de_M_ete_2013_version_longue_pars_01_1h10min43s.mp4";
  "137_refutation_T_de_M_ete_2013_version_longue_pars_02_1h15min44s.mp4";
  "138_debat_celier_zins_30min48s.mp4";
  "139_Pentecote_Vendee_27_05_2018_38_min.mp4";
  "140_Fete_de_la_TS_Trinite_ND_de_lEpine_30_mai_2021_38min.mp4";
  "141_Saint_Jacques_le_Majeur_ND_de_lEpine_21_07_2021_23min54.mp4";
  "142_Coeur_Immacule_de_Marie_ND_de_lEpine_55min44_22_08_2021.mp4";
  "143_Assomption_2021_ND_de_lEpine_46min29.mp4";
  "144_IVe_dimanche_de_Careme_Laetare_Definition_de_la_Charite_05_03_89.mp4";
  "146_XVIe_Dimanche_apres_la_Pentecote_ND_de_lEpine_20_09_2020.mp4";
  "147_XVIe_D_ap_Pentecôte_ND_de_lEpine_25_9_2022.mp4";
  "148_35_Christiade_01.mp4"; "149_36_Refutation_fou_these_02.mp4";
  "150_37_Christiade_02.mp4"; "151_38_Refutation_fou_these_03.mp4";
  "152_Fete_de_Saint_Michel_Archange_29_09_2019_31min.mp4";
  "153_Solennite_du_Rosaire_Vendee_06_10_2019_15min15.mp4"]
;;  

(*
let check_v1 = (v1=naive_v1) ;;
*)

let bigger_fountain_of_titles = Image.image (
  fun s ->
     let k = Substring.leftmost_index_of_in "_" s in
     (ios (Cull_string.beginning (k-1) s),
     Cull_string.coending 4 (Cull_string.cobeginning k s)
     )
) v1 ;;
let fountain_of_titles = List.filter (
  fun (j,_)->j<=143
) bigger_fountain_of_titles ;;

let retained_titles = List.filter (
   fun (j,_)-> List.mem j [
      4;5;
   ]
) fountain_of_titles ;;


let unordered_numbered_titles_part2 =   
  [(33, "Dimanche des Rameaux En Vendee 01 04 2012");
  (27, "Ier Dimanche de Carême Sur la Pénitence. Moulins 04/03/1990. 24min53");
  (26,
   "Ier Dimanche de Carême. Sens mystique des montées vers Jérusalem 16/02/1997 15min18");
  (29,
   "IIIème dimanche de Carême. Contre le démon muet. Moulins, 10/03/1996, 26min07");
  (28,
   "IIème Dim. de Carême. Transfiguration. Équilibre entre désolations et consolations.");
  (5,
   "IVe  Dimanche de l’Avent - Moulins 23-12-1990 -  Comparaison du temps des Patriarches avec le nôtre.");
  (39,
   "IVe D. après Pâques :  Attachement apostolique à NS et remontée de sa nature humaine à sa Divinté.");
  (0,
   "IVe Dimanche de Carême - Laetare - Définition de la Charité - Tours - 05-03-1989");
  (134, "Sermon Jeudi Saint 13 04 2017 31min27");
  (0, "Sermon sur l'Ascension à Moulins 24 05 1990");
  (53,
   "Solennité du Sacré Coeur et Saint Jean Baptiste - Il faut qu'Il croisse et que je diminue 24/6/1990");
  (34,
   "Veillée pascale Sur l'illogisme de l'attitude actuelle des Juifs talmudistes Moulins 11 04 1998")]
;;    

let numbered_titles_part2 = 
  Ordered.sort  pair_order  unordered_numbered_titles_part2 ;;

let remaining_titles_in_part2 = List.filter (
  fun (j,_) -> j = 0
) numbered_titles_part2;;

let remaining_indices_not_in_part1 = List.filter (
  fun j -> j > 80
) indices_not_in_part1;;

let data = (remaining_indices_not_in_part1,remaining_titles_in_part2) ;;



(************************************************************************************************************************
Snippet 101 : Remove lines starting with a # in a file (can be used with ocamllex)
************************************************************************************************************************)
let ap3 = Absolute_path.of_string "Fads/jug.ml";; 
let text3 = Io.read_whole_file ap3 ;;
let lines = Lines_in_string.lines text3 ;; 
let good_lines = List.filter (fun line->not(Supstring.begins_with line "#")) lines ;;
let new_text3 = String.concat "\n" good_lines ;; 
Io.overwrite_with ap3 new_text3 ;; 

(************************************************************************************************************************
Snippet 100 : Extract token types from a .mli file 
************************************************************************************************************************)
let ap1 = Absolute_path.of_string "Fads/Extract_php_lexer_from_padioleau/Originals/parser_php.mly" ;; 
let u1 = Io.read_whole_file ap1 ;;
let u2 = Lines_in_string.interval u1 110 236 ;;
let u3 = Outside_comments_and_strings.good_substrings u2 ;; 
let u4 = String.concat " " (Image.image (fun (_,_,s,_)->s) u3) ;;
let u5 = Substring.occurrences_of_in "%token" u4 ;; 
let last_elt_in_u5 = List.hd(List.rev u5) ;; 
let u6 = (Listennou.universal_delta_list u5) @ [last_elt_in_u5,(String.length u4)+1];; 
let u7 = Image.image (fun (i,j)-> Cull_string.interval u4 i (j-1)) u6;; 
let u8 = Image.image (
  fun t->
     let j = Substring.leftmost_index_of_in_from ">" t 9 in 
     (Cull_string.interval t 9 (j-1),Cull_string.cobeginning j t)
) u7 ;;
let u9 = Image.image (fun (typename,l)->(typename,Str.split (Str.regexp"[ \t\r\n]+") l)) u8;;    
let u10 = List.flatten (Image.image (fun (typename,l)->Image.image (fun x->(typename,x)) l) u9) ;;
let u11 = Image.image (fun (typename,tokname)->(tokname,typename)) u10 ;; 
let u12 = Ordered.sort Total_ordering.lex_for_strings  (Image.image fst u11) ;;
let u13 = Image.image (fun t->"| "^ t ^ " of " ^ (List.assoc t u11)) u12 ;;
let u14 = String.concat "\n" u13 ;; 
let u15 () = print_string ("\n\n\n" ^ u14 ^ "\n\n\n") ;;


(************************************************************************************************************************
Snippet 99 : Musings on a random walk (version 2, with stopping times)
************************************************************************************************************************)
let first_base = Memoized.make (fun n->Cartesian.general_product 
 (Int_range.scale (fun k->[-2;1]) 1 n)
) ;;

let rec helper_for_stopping_time (threshhold,current_sum,to_be_treated) = 
   match to_be_treated with 
    [] -> 0
   | (idx,x) :: others ->
      let new_sum = current_sum + x in 
      if new_sum >= threshhold 
      then idx 
      else helper_for_stopping_time (threshhold,new_sum,others) ;;   

let compute_stopping_time  threshhold l =
  helper_for_stopping_time (threshhold,0,Int_range.index_everything l) ;;     

let admissible_cases = Memoized.make(fun n->
    List.filter (fun l->(compute_stopping_time 1 l)=n) (first_base n)
) ;;  

let measure n = 
    let p1 = List.length(admissible_cases n) in 
    ((p1,Basic.power 2 n),(float_of_int p1)*.(0.5**(float_of_int n))) ;; 

let float_fold_sum = List.fold_left (fun x y -> x+.y) (0.) ;;     

let u1 = Int_range.scale (fun j->3*j-2) 1 6 ;;
let u2 = Image.image (fun x->snd(measure x)) u1 ;;
let u3 = Int_range.scale (fun j->float_fold_sum(Listennou.long_head j u2)) 1 6 ;;
let u4 = Image.image (fun x->fst(measure x)) u1 ;;
let u5 = Image.image (fun (a,b)->let g=Gcd.gcd a b in (a/g,b/g)) u4 ;; 


let ac = admissible_cases ;; 


(************************************************************************************************************************
Snippet 98 : Musings on a random walk
************************************************************************************************************************)
type dyadic = D of int * int ;; 
type linear_combination = LC of (dyadic * int ) list ;;

let rec helper_for_nuu (expo,walker) =
    if walker mod 2 = 1 
    then (expo,walker)
    else helper_for_nuu (expo+1,walker/2) ;;   

let nuu n = helper_for_nuu (0,n) ;;


module Dyadic = struct 

let frac x ex = let (d,new_x) = nuu x in D(new_x,d+ex) ;;   

let sum_in_prepared_case (D(a,ea)) (D(b,eb))=  
  (* we assume ea < eb *)
  let new_a = (Basic.power 2 (eb-ea)) * a  in 
  frac (new_a+b) eb ;;

let sum dya dyb =
  let (D(a,ea))=dya and (D(b,eb))=dyb in    
  if ea = eb then frac (a+b) ea else 
  if ea < eb 
  then sum_in_prepared_case dya dyb 
  else sum_in_prepared_case dyb dya ;;
  
let prod (D(a,ea)) (D(b,eb))= D(a*b,ea+eb) ;;      
let half (D(x,ex)) = D(x,ex+1) ;; 

let fold_sum l = List.fold_left sum (D(0,1)) l ;;  


end ;;

module Linear_Combination = struct 

let rec helper_for_sum (treated,to_be_treated1,to_be_treated2) =
  match to_be_treated1 with 
  [] -> List.rev(List.rev_append to_be_treated2 treated)
  | (d1,idx1) :: others1 -> 
     (
      match to_be_treated2 with 
      [] -> List.rev(List.rev_append to_be_treated1 treated)
      | (d2,idx2) :: others2 -> 
          if idx1<idx2 then helper_for_sum ((d1,idx1) ::treated,others1,to_be_treated2) else 
          if idx2<idx1 then helper_for_sum ((d2,idx2) ::treated,to_be_treated1,others2) else   
            helper_for_sum ((Dyadic.sum d1 d2,idx1) ::treated,others1,others2) 
     ) ;;

let sum (LC l1) (LC l2) = (LC(helper_for_sum([],l1,l2))) ;;
let zero = LC [] ;;
let fold_sum l = List.fold_left sum zero l ;;  

end ;;  

let peggy_transform (LC l) = 
  Linear_Combination.fold_sum (Image.image (fun (d,idx)->
      if idx = 0 then LC[d,idx] else 
       LC[Dyadic.half d,idx-1;Dyadic.half d,idx+2] 
    ) l) ;;

let ff = Memoized.small peggy_transform (LC[D(1,0),1]) ;;   

let gg = Memoized.make (fun n->
   let (LC temp1) = ff n in 
   let dys = Dyadic.fold_sum(Image.image fst (List.tl temp1)) in 
   let (D(s,es)) = dys in
   (dys,(float_of_int(s))*.(0.5**(float_of_int es))) 
  ) ;; 

(************************************************************************************************************************
Snippet 97 : Read a file and remove tabs in each line
************************************************************************************************************************)
let ap3 = Absolute_path.of_string "Fads/pan.ml" ;;
let z4 = Io.read_whole_file ap3 ;;
let z5 = Lines_in_string.interval z4 9 21 ;;
let z6 = Lines_in_string.lines z5 ;; 
let z7 = Image.image String.lowercase_ascii z6 ;;
let z8 = Ordered.sort Total_ordering.lex_for_strings z7 ;;
let z9 = Image.image (Str.split (Str.regexp_string "\t")) z6;;
let z10 = Image.image (
   fun l -> let nth  = (fun k->Cull_string.trim_spaces(List.nth l (k-1))) in 
   (int_of_string(nth 1),nth 2)
) z9 ;; 
let z10 = [(53, "Veni de Libano"); (55, "Paulus"); (56, "Pitra"); (57, "Fulgurator");
(59, "Florent-Jean"); (71, "Comte Bavon"); (69, "Charles"); (64, "Francis");
(67, "Amos"); (68, "chaussis"); (79, "HALLELUIA"); (75, "luern");
(80, "Adeodato")] ;;

let z11 = String.concat "," (Image.image snd z10) ;;

(************************************************************************************************************************
Snippet 96 : Musing on the Alon-Knuth theorem (episode 3). Contains some code 
to compute the maximal elts in an upwards filter and the minimal elts outside
it (see the "butterfly" function). It should work even for a filter on a large
base set, as long as the minimal and maximal elts have small size and are not
too numerous.
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let i_fold_intersect = Ordered.fold_intersect i_order ;;
let i_intersects = Ordered.intersects i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_setminus = Ordered.setminus i_order ;;
let i_sort = Ordered.sort i_order ;;


let il_order = Total_ordering.silex_for_intlists ;;
let il_fold_merge = Ordered.fold_merge il_order ;;
let il_setminus = Ordered.setminus il_order ;;
let il_sort = Ordered.sort il_order ;;

let u1 = Int_range.scale (fun t->[-1;1]) 1 5 ;;
let u2 = Cartesian.general_product u1 ;;
let base = Image.image (fun l-> 1 :: l) u2 ;;
let indexed_base = Int_range.index_everything base ;; 
let sp l1 l2 =
   let temp1 = List.combine l1 l2 in 
   Basic.fold_sum(Image.image (fun (x,y)->x*y) temp1) ;;
   

let ker uple =
    List.filter_map (
     fun (j,uple2) -> if sp uple uple2 = 0 then Some j else None
    ) indexed_base;; 

let i_ker_for_individual k = ker (List.nth base (k-1)) ;;      
let i_ker l =
    let temp1 = Image.image i_ker_for_individual l in 
    i_fold_intersect temp1 ;;
              
let unexpanded_data = 
   Image.image (fun (j,uple)->(j,ker uple)) indexed_base ;;

let all_complements n ll=
Image.image (fun z->i_setminus (Int_range.range 1 n) z)  ll ;;

let merged_power_sets ll=
  let temp1 = Image.image (fun l->il_sort(Listennou.power_set l)) ll in 
  il_fold_merge temp1 ;; 

let butterfly_operation ll addedum = 
   let test_f = (
    fun x->let enhanced_x = i_sort (x@addedum) in 
    i_ker (enhanced_x) <> []
   ) in   
  let (inert,ramified) = List.partition test_f ll in 
    let below_ramified = merged_power_sets ramified in 
    let temp4 = List.filter test_f   below_ramified in 
    let temp5 = Ordered_misc.maximal_elts_wrt_inclusion temp4 in  
    let temp6 = Image.image (fun x->i_sort (x@addedum) ) inert 
    and temp7 = Image.image (fun x->i_sort (x@addedum) ) temp5 in 
    let temp8 = List.filter (fun x->not(test_f x))  below_ramified in 
    let temp9 = Ordered_misc.minimal_elts_wrt_inclusion temp8 in  
    let temp10 = Image.image (fun x->i_sort (x@addedum) ) temp9 in 
    let pre_whole = il_sort(ramified@temp6@temp7) in 
    (
      ramified,temp6,temp7, 
      Ordered_misc.maximal_elts_wrt_inclusion pre_whole,
      temp10
    ) ;;

let tt n = List.filter (fun k->i_ker [k;n]=[]) (Int_range.range 1 (n-1)) ;; 


let g_10_part1=[[1;4;6;10];[1;4;7;10];[1;6;7;10];[4;6;7;10]] ;;
let g_10_part2=[[2;3;5;9];[2;3;8;9];[2;5;8;9];[3;5;8;9]] ;;
let g_10 = il_sort(g_10_part1 @ g_10_part2) ;;

let b_10_part1 = Image.image (fun (a,b)->[a;b]) 
(Cartesian.product [1;4;6;7;10] [2;3;5;8;9]) ;;
let b_10_part2 = [[1;4;6;7];[2;3;5;8]] ;;
let b_10 = il_sort(b_10_part1 @ b_10_part2) ;;

let t_10_1 = Ordered_misc.minimal_transversals b_10 ;;
let check_gb_10 = (il_sort(all_complements 10 t_10_1))=g_10 ;;  

butterfly_operation g_10_part1 [11];;

let g_11_part1=[[1;4;6;10];[1;4;6;11];[1;4;7;10];[1;4;7;11];
[1;6;7;10;11];[4;6;7;10;11]] ;;
let g_11_part2=[[2;3;5;9];[2;3;8;9];[2;5;8;9];[3;5;8;9]] ;;
let g_11 = il_sort(g_11_part1 @ g_11_part2) ;;

let b_11_part1 = Image.image (fun (a,b)->i_sort[a;b]) 
(Cartesian.product [1;4;6;7;10;11] [2;3;5;8;9]) ;;
let b_11_part2 = [[1;4;6;7];[2;3;5;8];[1;4;10;11]] ;;
let b_11 = il_sort(b_11_part1 @ b_11_part2) ;;

let t_11_1 = Ordered_misc.minimal_transversals b_11 ;;
let check_gb_11 = (il_sort(all_complements 11 t_11_1))=g_11 ;; 

butterfly_operation g_11_part2 [12];;

let g_12_part1=[[1;4;6;10];[1;4;6;11];[1;4;7;10];[1;4;7;11];
[1;6;7;10;11];[4;6;7;10;11]] ;;
let g_12_part2=[[2;3;5;9];[2;3;5;12];[2;3;8;9];[2;3;8;12];[2;5;8;9;12];[3;5;8;9;12]] ;;
let g_12 = il_sort(g_12_part1 @ g_12_part2) ;;

let b_12_part1 = Image.image (fun (a,b)->i_sort [a;b]) 
(Cartesian.product [1;4;6;7;10;11] [2;3;5;8;9;12]) ;;
let b_12_part2 = [[1;4;6;7];[2;3;5;8];[2;3;9;12];[1;4;10;11]] ;;
let b_12 = il_sort(b_12_part1 @ b_12_part2) ;;

let t_12_1 = Ordered_misc.minimal_transversals b_12 ;;
let check_gb_12 = (il_sort(all_complements 12 t_12_1))=g_12 ;; 

butterfly_operation g_12_part1 [13];;

let g_13_part1=[[1; 4; 6; 10]; [1; 4; 7; 11]; [1; 6; 7; 13]; [1; 10; 11; 13];
[1; 4; 6; 11; 13]; [1; 4; 7; 10; 13]; [1; 6; 7; 10; 11];
[4; 6; 7; 10; 11; 13]] ;;
let g_13_part2=[[2;3;5;9];[2;3;5;12];[2;3;8;9];[2;3;8;12];[2;5;8;9;12];[3;5;8;9;12]] ;;
let g_13 = il_sort(g_13_part1 @ g_13_part2) ;;

let b_13_part1 = Image.image (fun (a,b)->i_sort [a;b]) 
(Cartesian.product [1;4;6;7;10;11;13] [2;3;5;8;9;12]) ;;
let b_13_part2 = [[1;4;6;7];[1;4;10;11];[1;6;10;13];[1;7;11;13];
[2;3;5;8];[2;3;9;12]] ;;
let b_13 = il_sort(b_13_part1 @ b_13_part2) ;;

let t_13_1 = Ordered_misc.minimal_transversals b_13 ;;
let check_gb_13 = (il_sort(all_complements 13 t_13_1))=g_13 ;; 

let (_,_,_,r_14_good,r_14_bad) = butterfly_operation g_13_part2 [14];;

let g_14_part1= g_13_part1 ;;
let g_14_part2= r_14_good ;;
let g_14 = il_sort(g_14_part1 @ g_14_part2) ;;

let b_14_part1 = Image.image (fun (a,b)->i_sort [a;b]) 
(Cartesian.product [1;4;6;7;10;11;13] [2;3;5;8;9;12;14]) ;;
let b_14_part2 = il_sort (b_13_part2 @ r_14_bad) ;;
let b_14 = il_sort(b_14_part1 @ b_14_part2) ;;

let t_14_1 = Ordered_misc.minimal_transversals b_14 ;;
let check_gb_14 = (il_sort(all_complements 14 t_14_1))=g_14 ;; 


(*

let h1 = il_sort(all_complements 13 t_13_1) ;;
let see1 = il_setminus h1 g_13 ;;
let see2 = il_setminus g_13 h1;;

let good1 = [1;6;7;13] ;;
let c_good1 = i_setminus (Int_range.range 1 13) good1 ;; 
let see3 = List.filter (
   fun z-> not(i_intersects z c_good1)
) b_13 ;;
let see4 = List.filter (fun t->i_is_included_in t c_good1) t_13_1 ;; 

let ll = g_12_part1 
and addendum = [13] ;;


let test_f = (
   fun x->let enhanced_x = i_sort (x@addendum) in 
   i_ker (enhanced_x) <> []
) ;;
let (inert,ramified) = List.partition test_f ll ;;
let below_ramified = merged_power_sets ramified ;;
let temp4 = List.filter test_f   below_ramified ;;
let temp5 = Ordered_misc.maximal_elts_wrt_inclusion temp4 ;; 
let temp6 = Image.image (fun x->i_sort (x@addendum) ) inert 
and temp7 = Image.image (fun x->i_sort (x@addendum) ) temp5 ;; 
   let temp8 = List.filter (fun x->not(test_f x))  below_ramified in 
   let temp9 = Ordered_misc.minimal_elts_wrt_inclusion temp8 in  
   let temp10 = Image.image (fun x->i_sort (x@addendum) ) temp9 in 
   let pre_whole = il_sort(ramified@temp6@temp7) in 
   (
     ramified,temp6,temp7, 
     Ordered_misc.maximal_elts_wrt_inclusion pre_whole,
     temp10
   ) ;;

*)   

(************************************************************************************************************************
Snippet 95 : Musing on the Alon-Knuth theorem (episode 2)
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let i_fold_intersect = Ordered.fold_intersect i_order ;;
let i_sort = Ordered.sort i_order ;;


let il_order = Total_ordering.silex_for_intlists ;;
let il_sort = Ordered.sort il_order ;;

let p_order = Total_ordering.product i_order il_order ;;
let p_sort = Ordered.sort p_order ;;

let u1 = Int_range.scale (fun t->[0;1;2]) 1 3 ;;
let u2 = Cartesian.general_product u1 ;;
let u3 = Image.image (fun l->(Basic.fold_sum l,l)) u2 ;;
let u4 = p_sort (List.filter (fun (s,l)->s<=2) u3) ;; 
let u5 = List.rev(Image.image snd (List.tl u4)) ;;
let u6 = Image.image (Image.image (fun t->2*t)) u5;;

let tf1 (a1,a2,a3,b1,b2,b3) =
  (b3*a2 + b2*a3, b3*a1 + b1*a3, b2*a1 + b1*a2) ;;

let tf2 uple =
   let (u,v,w) = tf1 uple in (u*u,v*v,w*w) ;;

let u1 = Int_range.scale (fun t->[-1;1]) 1 6 ;;
let u2 = Cartesian.general_product u1 ;;
let u3 = Image.image (
     fun l->
      let nth = (fun k->List.nth l (k-1)) in 
      (nth 1,nth 2,nth 3,nth 4,nth 5,nth 6)
  ) u2 ;;
let u4 = Image.image tf1 u3 ;; 
let u5 = Image.image (fun (a,b,c)->[a;b;c]) u4 ;;
let zero_vector = [0;0;0] ;;
let whole = il_sort (zero_vector::u5) ;; 
let shadow j = i_sort(Image.image (fun l->List.nth l (j-1)) whole) ;;
let shadows = Int_range.scale shadow 1 3 ;;
let base = Cartesian.general_product shadows ;;
let unattended = List.filter (fun z->not(List.mem z whole)) base ;;
let u6 = Int_range.index_everything unattended 




(************************************************************************************************************************
Snippet 94 : Musing on the Alon-Knuth theorem
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let i_fold_intersect = Ordered.fold_intersect i_order ;;
let i_sort = Ordered.sort i_order ;;


let il_order = Total_ordering.silex_for_intlists ;;
let il_sort = Ordered.sort il_order ;;

let u1 = Int_range.scale (fun t->[-1;1]) 1 3 ;;
let u2 = Cartesian.general_product u1 ;;
let u3 = Image.image (
   fun l->
    let nth = (fun k->List.nth l (k-1)) in 
    (1,nth 1,nth 2,nth 3)
) u2 ;;
let u4 = Int_range.index_everything u3 ;; 
let sp (a1,a2,a3,a4) (b1,b2,b3,b4) = a1*b1 + a2*b2 + a3*b3 + a4 * b4 ;;

let ker uple =
    List.filter_map (
     fun (j,uple2) -> if sp uple uple2 = 0 then Some j else None
    ) u4;; 

let i_ker k = ker (List.nth u3 (k-1)) ;;      
let i_ker_for_several l =
    let temp1 = Image.image i_ker l in 
    i_fold_intersect temp1 ;;
              
let unexpanded_data = 
   Image.image (fun (j,uple)->(j,ker uple)) u4 ;;

let elementary_extension l j = 
    let new_idx = (List.length l)+1 in 
    let naive_possibilities_for_j = List.assoc new_idx unexpanded_data  in 
    if j= new_idx
    then Some(l @ [j,naive_possibilities_for_j])
    else 
    let possibilities_for_j = i_fold_intersect (naive_possibilities_for_j::(List.filter_map (
      fun (k,vals) -> if k=j then Some vals else None
     ) l)) in 
    if possibilities_for_j = [] then None else 
    let new_l = Image.image (fun 
    (k,vals)->if k=j then (k,possibilities_for_j) else (k,vals)) l in 
    Some(new_l @ [j,possibilities_for_j]) ;;     

let all_elementary_extensions  l = 
   let older_indices = i_sort(Image.image fst l) in      
   List.filter_map (
    elementary_extension l
   ) (older_indices@[ (List.length l)+1]) ;;

let involved_possibilities l = 
  let involved_indices = i_sort(Image.image fst l) in  
  Cartesian.general_product (Image.image (fun j->List.assoc j l) involved_indices) ;;

let ff = Memoized.recursive (
   fun old_f d->if d<2 then [[List.hd unexpanded_data]] else 
    List.flatten(Image.image all_elementary_extensions (old_f (d-1)))
)  ;;  

let z1 = List.filter (
   fun l->
    let older_indices = i_sort(Image.image fst l) in    
    List.length(older_indices)<=3
) (ff 7) ;;
let z2 = 
  il_sort(Image.image i_sort
  (List.flatten(Image.image involved_possibilities z1)));;



(************************************************************************************************************************
Snippet 93 : Musing on the simplicity of An
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let i_sort = Ordered.sort i_order ;;

let il_order = Total_ordering.silex_for_intlists ;;
let il_fold_merge = Ordered.fold_merge Total_ordering.silex_for_intlists ;;
let il_sort = Ordered.sort il_order ;;


let pr = Permutation.product ;;
let iv = Permutation.inverse ;;
let conj x g = pr g (pr x (iv g)) ;;
let cube x = pr x (pr x x) ;;
let nonfixed_items perm = 
    let temp1 = Int_range.index_everything perm in 
    List.filter_map (fun (x,y)->if x=y then None else Some x)temp1 ;;
let is_good x= 
 (nonfixed_items(cube x)=[]) && (List.length(nonfixed_items x)=3) ;;



let n0 = 7 ;;
let main = Permutation.alternating_group n0 ;;
let conjugates x = il_sort( Image.image (conj x) main) ;; 
let pikaboo x =
  let temp1 = Permutation.cyclic_subgroup x in 
  let temp2 = Image.image conjugates temp1 in 
  il_fold_merge temp2 ;;

let pookabi = Memoized.make(fun x ->
   let temp1 = pikaboo x in 
   let temp2 = Image.image (Listennou.long_head 3) temp1 in 
   il_sort temp2 );;

let measure z = List.length(List.filter (fun x->x>3) z);;

let test_for_normal_form z =
    let temp1 = List.filter (fun x->x>3) z in 
    let m = List.length temp1 in 
    temp1 = Int_range.range 4 (m+3) ;; 

let prettify_atomic_result z =
  let temp1 = Image.image (fun i->if i<4 then string_of_int i else "*") z in 
   String.concat "" temp1 ;; 

let prettify_individual_result m ll =
  let temp1 =List.filter(fun z->(measure z=m)&&(test_for_normal_form z)) ll in 
  Image.image prettify_atomic_result temp1;;  

let expand_pookabi_result ll = 
  let temp1 = i_sort(Image.image measure ll) in 
  Image.image (fun m->(m,
  prettify_individual_result m ll)) temp1;;

let u1 = Image.image pookabi main ;;   

let boehme_order = Total_ordering.silex_compare Total_ordering.silex_for_intlists ;;
let u2 = Ordered.sort boehme_order u1 ;;
let u3 = Image.image (
  fun y->(y,List.filter (fun x->pookabi x=y) main)
) u2 ;;
let u4 = Image.image (fun (_,z)->List.length z) u3;;
let v1 = List.nth u2 5 ;;
let v2 = expand_pookabi_result v1;;





(************************************************************************************************************************
Snippet 92 : Define a cycle from list of successive elts
************************************************************************************************************************)
let cycle_from_perm perm =
  let n = List.length perm in 
  let idx = (fun x->Listennou.find_index x perm) in 
  let next = (fun x->
     let i = idx x in 
     if i = n then List.hd perm else 
     List.nth perm i  
   ) in 
   Int_range.scale next 1 n ;;

(************************************************************************************************************************
Snippet 91 : An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. (Version 1)
************************************************************************************************************************)
module SN98 = struct 
open Needed_values ;;
module Bart_simpson = struct 
  
  type hook_in_knowledge = 
  Boundary_increment
 | Passive_repeat  
 | Fork ;;

type parametrized_uniform_subrange = {
 usr_positive_exceptions : int list ;
 usr_negative_exceptions : int list ;
 usr_modulus : int ;
 usr_usual :  int list ;
} ;; 

type  parametrized_subrange = {
 ps_exceptions : (int * (int list)) list ;
 ps_usual : parametrized_uniform_subrange ; 
} ;; 

type parametrized_ps_list = {
 pl_exceptions : (int * (int list list)) list ;
 pl_usual : parametrized_subrange list ;
} ;; 

type level_two_t = Quick of int list ;;   

end ;;

open Bart_simpson ;;

let i_order = Total_ordering.for_integers ;;
let i_insert = Ordered.insert i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_setminus = Ordered.setminus i_order ;;

let il_order = Total_ordering.silex_for_intlists ;;
let il_merge = Ordered.merge il_order ;;
let il_sort = Ordered.sort il_order ;;

let concretize (n,scrappers) = i_setminus (Int_range.range 1 n) scrappers ;; 

let test_for_admissibility_up_to_max_with max_width z =
  if max_width<1 then true else 
  Sz_preliminaries.test_for_admissibility (Sz_max_width_t.MW (max_width)) z ;;

let test_for_admissiblity width breadth z =
   (test_for_admissibility_up_to_max_with (width-1) z)
   &&
   (List.for_all (fun t->
    not(i_is_included_in [t;t+width;t+2*width] z)) (Int_range.range 1 breadth))  ;;

let remove_one_element (n,scrappers) k=
  let new_scrappers = i_insert k scrappers in 
  if k <> n then (n,i_insert k scrappers) else 
  let new_z =  concretize (n,new_scrappers) in 
  let new_max = List.hd(List.rev new_z) in 
  (new_max,List.filter (fun t->t<new_max) new_scrappers) ;;



(*

remove_one_element (10,[3;7;8;9]) 10 ;;

*)

module Parametrized = struct 

let eval_uniform_subrange usr n =
  List.filter (
     fun k->
      if i_mem k usr.Bart_simpson.usr_negative_exceptions then false else  
      if i_mem k usr.Bart_simpson.usr_positive_exceptions then true  else 
      i_mem (k mod usr.Bart_simpson.usr_modulus)
      usr.Bart_simpson.usr_usual
  ) (Int_range.range 1 n) ;; 

let eval_subrange sr n =
   match List.assoc_opt n sr.Bart_simpson.ps_exceptions with 
   Some answer -> answer 
   | None ->
    eval_uniform_subrange sr.Bart_simpson.ps_usual n  ;;

let eval_ps_list psl n =
  match List.assoc_opt n psl.Bart_simpson.pl_exceptions with 
  Some answer -> answer 
  | None ->
   Image.image (fun sr->eval_subrange sr n) 
   psl.Bart_simpson.pl_usual ;;    

let eval_level_two (Quick l) scrappers n =
  let z = concretize (n,scrappers) in 
  if (not(i_is_included_in l z))  
  then [z] 
  else 
  let temp1 = List.rev_map (fun t->i_setminus z [t]) l in 
  il_sort temp1 ;;     

end ;;   


module Parametrized_Example = struct 

  let uniform_subrange pe ne mdl usu = {
    Bart_simpson.usr_positive_exceptions = pe ;
    usr_negative_exceptions = ne ; 
    usr_modulus = mdl;
    usr_usual = usu ;
  };; 
  
  let subrange (sr_exns,pe,ne,mdl,usu) = {
    Bart_simpson.ps_exceptions = sr_exns ;
    ps_usual = uniform_subrange pe ne mdl usu ;
  };; 
  
  let ps_list psl_exns psl_usu = {
    Bart_simpson.pl_exceptions = psl_exns ;
    pl_usual = Image.image subrange psl_usu ;
  };; 

  (*
  let example1 = ps_list 
     [
       1,[[1]];
       2,[[1;2]];
     ]
     [
      ([],[],[3],1,[0]);
      ([],[],[2],1,[0]);
      ([],[],[1],1,[0]);
     ] ;;
  *)   

  let example1 = Quick [1;2;3] ;;

  let example2 (* for (1,2,[]) *) = ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
     ]
     [
      ([],[],[3],1,[0]);
      ([],[],[2],1,[0]);
     ] ;;  
     
  let example3 (* for (1,3,[]) *) = ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
       4,[[1;2;4];[1;3;4]];
     ]
     [
      ([],[],[3],1,[0]);
     ] ;;    

  let example4 (* for (1,2,[4]) *)= ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
       4,[[1;2];[1;3];[2;3]];
     ]
     [
      ([],[],[3;4],1,[0]);
      ([],[],[2;4],1,[0]);
      ([],[],[1;4],1,[0]);
     ] ;;   

  let example5 (* for (1,2,[5]) *)= ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
       4,[[1;2;4];[1;3;4]];
     ]
     [
      ([],[],[3;5],1,[0]);
      ([],[],[2;5],1,[0]);
     ] ;;  
     
  let example6 (* for (1,4,[]) *)= ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
       4,[[1;2;4];[1;3;4]];
       5,[[1;2;4;5]];
     ]
     [
      ([],[],[3;6],1,[0]);
      ([],[],[3;5],1,[0]);
      ([],[],[3;4],1,[0]);
      ([],[],[2;5],1,[0]);
      ([],[],[2;4],1,[0]);
      ([],[],[1;4],1,[0]);
     ] ;;   
     
   let example7 (* for (1,5,[]) *)= ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
       4,[[1;2;4];[1;3;4]];
       5,[[1;2;4;5]];
       6,[[1;2;4;5];[1;2;4;6];[1;2;5;6];
          [1;3;4;6];[1;3;5;6];[2;3;5;6]]
     ]
     [
      ([],[],[3;6],1,[0]);
      ([],[],[3;5],1,[0]);
      ([],[],[2;5],1,[0]);
     ] ;;      

    let example8 (* for (1,6,[]) *)= ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
       4,[[1;2;4];[1;3;4]];
       5,[[1;2;4;5]];
       6,[[1;2;4;5];[1;2;4;6];[1;2;5;6];
          [1;3;4;6];[1;3;5;6];[2;3;5;6]];
       7,[[1;2;4;5;7];[1;2;4;6;7];[1;3;4;6;7]]   
     ]
     [
      ([],[],[3;6],1,[0]);
     ] ;;    

  end ;;   
  


let rose_hashtbl = Hashtbl.create 50 ;;
let medium_hashtbl = Hashtbl.create 50 ;;
let low_hashtbl = Hashtbl.create 50 ;;

let access width breadth (n,scrappers) = 
  let z = concretize (n,scrappers) in 
  if ((width,breadth)=(1,0))||(test_for_admissiblity width breadth z) 
  then Some [z] 
  else 
  match Hashtbl.find_opt rose_hashtbl (width,breadth) with 
  Some summary -> Some (Parametrized.eval_level_two summary scrappers n)
  | None ->  
  (match Hashtbl.find_opt medium_hashtbl (width,breadth,scrappers) with 
   Some summary -> Some (Parametrized.eval_ps_list summary n)
   | None -> Hashtbl.find_opt low_hashtbl (width,breadth,n,scrappers)) ;;

exception Boundary_increment_exn1 of int * int * int * (int list) ;;  
exception Boundary_increment_exn2 of int * int * int * (int list) ;; 
exception Boundary_increment_exn3 of int * int * int * (int list) ;; 

exception Passive_repeat_exn1 of int * int * int * (int list) ;; 
exception Passive_repeat_exn2 of int * int * int * (int list) ;; 

exception Fork_exn1 of int * int * int * (int list) ;; 
exception Fork_exn2 of int * int * int * (int list) ;; 
exception Fork_exn3 of int * int * int * (int list) ;; 
  
let compute_from_below (width,breadth,n,scrappers) tool =
     match tool with 
     Boundary_increment -> 
      let opt1 = access width breadth (remove_one_element (n,scrappers) n) in 
      if opt1 = None then raise(Boundary_increment_exn1(width,breadth,n,scrappers)) else  
      let pre1 = Option.get opt1 in 
      if List.mem n scrappers then raise(Boundary_increment_exn2(width,breadth,n,scrappers)) else
      let temp1 = List.filter_map (fun z->
         let new_z = z @ [n] in 
         if test_for_admissiblity width breadth new_z 
          then Some new_z
          else None
      )  pre1 in 
      if temp1=[]  then raise(Boundary_increment_exn3(width,breadth,n,scrappers)) else 
      temp1  
      | Passive_repeat ->
        let opt5 = access width (breadth-1) (n,scrappers)  in 
        if opt5 = None then raise(Passive_repeat_exn1(width,breadth,n,scrappers)) else     
        let pre5 = Option.get opt5 and b = breadth in   
        let temp5 = List.filter (fun z->
          not(i_is_included_in [b;b+width;b+2*width] z) 
       )  pre5 in 
       if temp5=[]  then raise(Boundary_increment_exn2(width,breadth,n,scrappers)) else 
       temp5  
      | Fork -> 
      let tempf = (fun k->
        remove_one_element  (n,scrappers)  (breadth+k*width) 
        )  and tempf2 = access width breadth in 
      let (m0,scr0) = tempf 0 and (m1,scr1) =  tempf 1 and (m2,scr2) =  tempf 2 in 
      let opt2 = tempf2 (m0,scr0)  
      and opt3 = tempf2 (m1,scr1) 
      and opt4 = tempf2 (m2,scr2) in 
      if opt2 = None then raise(Fork_exn1(width,breadth,n,scrappers)) else  
      if opt3 = None then raise(Fork_exn2(width,breadth,n,scrappers)) else    
      if opt4 = None then raise(Fork_exn3(width,breadth,n,scrappers)) else   
      let temp3 = List.flatten (Image.image Option.get [opt2;opt3;opt4]) in 
      let (_,temp4) = Max.maximize_it_with_care List.length temp3 in 
      il_sort temp4 ;; 


let low_add (width,breadth,n,scrappers,tool) =
   let res = compute_from_below (width,breadth,n,scrappers) tool in  
   let _ = Hashtbl.replace low_hashtbl (width,breadth,n,scrappers) res in 
   res ;;

let med_add (width,breadth,scrappers) summary = 
  Hashtbl.replace medium_hashtbl (width,breadth,scrappers) summary ;;

let rose_add (width,breadth) summary = 
    Hashtbl.replace rose_hashtbl (width,breadth) summary ;;  
 

let find_remote_stumbling_block_or_immediate_working_tool width breadth (n,scrappers) = 
  let opt5 = access width (breadth-1) (n,scrappers)  in 
  if opt5 = None then (Some(width,breadth-1,n,scrappers),None) else     
  let pre5 = Option.get opt5 and b = breadth in   
  let temp5 = List.filter (fun z->
      not(i_is_included_in [b;b+width;b+2*width] z) 
  )  pre5 in 
  if temp5<>[]  then (None, Some Passive_repeat) else 

    match access width breadth (n,scrappers) with 
    Some old_answer -> (None,None) 
    | None -> 
      let opt1 = access width breadth (remove_one_element (n,scrappers) n) in 
      if opt1 = None then (Some(width,breadth,n-1,scrappers),None) else  
      let pre1 = Option.get opt1 in 
      let temp1 = List.filter_map (fun z->
         let new_z = z @ [n] in 
         if test_for_admissiblity width breadth new_z 
          then Some new_z
          else None
      )  pre1 in 
      if temp1<>[]  then (None,Some Boundary_increment) else 
      let tempf = (fun k->
        remove_one_element  (n,scrappers)  (breadth+k*width) 
      )  in 
      let (m0,scr0) = tempf 0 and (m1,scr1) =  tempf 1 and (m2,scr2) =  tempf 2 in 
      let tempf2 = access width breadth in 
      let opt2 = tempf2 (m0,scr0)  
      and opt3 = tempf2 (m1,scr1) 
      and opt4 = tempf2 (m2,scr2) in 
      if opt2 = None then (Some(width,breadth,m0,scr0),None) else  
      if opt3 = None then (Some(width,breadth,m1,scr1),None) else    
      if opt4 = None then (Some(width,breadth,m2,scr2),None) else   
      (*
      let temp3 = List.flatten (Image.image Option.unpack [opt2;opt3;opt4]) in 
      let (_,temp4) = Max.maximize_it_with_care List.length temp3 in 
      *)
      (None,Some Fork) ;;

let rec first_needed_step_in_solution_opt (width,breadth,m0,scr0) =
    let (opt_stumbling_block,opt_tool) = 
    find_remote_stumbling_block_or_immediate_working_tool width breadth (m0,scr0) in 
    match opt_tool with 
    Some(tool) ->Some (width,breadth,m0,scr0,tool)
    | None -> (match opt_stumbling_block with 
                Some block ->first_needed_step_in_solution_opt(block) 
                |None -> None 
               )   ;; 
    
exception Quick_compute_exn of int * int * int * (int list) ;; 

let force_access width breadth (n,scrappers) = 
   match access width breadth (n,scrappers) with 
    None -> raise (Quick_compute_exn(width,breadth,n,scrappers)) 
   | Some answer -> answer;;

  
rose_add (1,1) Parametrized_Example.example1 ;; 


med_add (1,2,[]) Parametrized_Example.example2 ;; 
med_add (1,3,[]) Parametrized_Example.example3 ;; 

med_add (1,2,[4]) Parametrized_Example.example4 ;; 
med_add (1,3,[4]) Parametrized_Example.example4 ;; 
med_add (1,4,[4]) Parametrized_Example.example4 ;; 

med_add (1,2,[5]) Parametrized_Example.example5 ;; 
med_add (1,3,[5]) Parametrized_Example.example5 ;; 
med_add (1,4,[5]) Parametrized_Example.example5 ;; 

med_add (1,4,[])  Parametrized_Example.example6 ;; 
med_add (1,5,[])  Parametrized_Example.example7 ;; 
med_add (1,6,[])  Parametrized_Example.example8 ;; 

(*    

#use "Githubbed_archive/Szemeredi_problem/03_third_stab_at_szemeredi_problem.ml" ;;


*)

(*    

STEP 15+N : 

let (c_width,c_breadth,c_strappers) = (1,7,[]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;  
for k = 1 to 8 do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   


let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example8 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)

(*    

STEP 14 : 

let (c_width,c_breadth,c_strappers) = (1,6,[]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;  
for k = 1 to 8 do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   


let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example8 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)

(*    

STEP 13 : 

let (c_width,c_breadth,c_strappers) = (1,5,[]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;  
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   


let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example7 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)

(*    

STEP 12 : 

let (c_width,c_breadth,c_strappers) = (1,4,[]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;  
for k = 1 to 5 do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   
low_add (1, 4, 6, [], Fork) ;;
for k = 7 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Boundary_increment) in ()
done ;;  

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example6 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)


(*    

STEP 10: 

let (c_width,c_breadth,c_strappers) = (1,4,[5]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example5 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)



(*    

STEP 9: 

let (c_width,c_breadth,c_strappers) = (1,3,[5]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example5 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)


(*    

STEP 8: 

let (c_width,c_breadth,c_strappers) = (1,2,[5]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example5 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)



(*    

STEP 7: 

let (c_width,c_breadth,c_strappers) = (1,4,[4]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example4 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)


(*    

STEP 6: 

let (c_width,c_breadth,c_strappers) = (1,4,[4]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example4 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)



(*    

STEP 5: 

let (c_width,c_breadth,c_strappers) = (1,3,[4]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example4 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)


(*    

STEP 4 : 

let (c_width,c_breadth,c_strappers) = (1,2,[4]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example4 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)



(*    

STEP 3 : 

let (c_width,c_breadth,c_strappers) = (1,3,[]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example3 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)


(*    

STEP 2 : 

let (c_width,c_breadth,c_strappers) = (1,2,[]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (1, 2, k, [], Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example2 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)


(*    

STEP 1 :

let (c_width,c_breadth,c_strappers) = (1,1,[]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 

low_add (1, 1, 3, [], Fork) ;; 
let bound = 20 ;;
for k = 4 to bound do 
  let _ = low_add (1, 1, k, [], Boundary_increment) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example1 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)
end ;; 



(************************************************************************************************************************
Snippet 90 : Code that lead to the discovery of a linear algorithm to compute, given any
finite set X of integers, the largest subset Y of X, containing no AP of length 3 and
width <=2. The algorithm uses an automaton with 8 states where the transitions are
the successive differences in Y.
************************************************************************************************************************)
open Needed_values ;;

let i_order = Total_ordering.for_integers ;;
let i_setminus = Ordered.setminus i_order ;;

let il_order = Total_ordering.silex_for_intlists ;;
let il_merge = Ordered.merge il_order ;;
let il_sort = Ordered.sort il_order ;;
let careful_merge x y = Ordered_misc.minimal_elts_wrt_inclusion (il_merge x y) ;;

let current_width = 2 ;;

let is_admissible = Sz_preliminaries.test_for_admissibility 
      (Sz_max_width_t.MW current_width) ;;

let force_insert_in m old_data = List.filter_map (
        fun old_elt ->
          let new_elt = old_elt @ [m] in 
          if is_admissible new_elt 
          then Some new_elt 
          else None 
    ) old_data ;;   
  
exception Middleman_exn of int list ;; 

let middleman = Memoized.recursive (fun old_f x-> 
  match List.rev x with 
  [] -> ([[]],[[]],[],[[]])
  | m :: other_than_m ->
     if other_than_m = [] 
     then ([[m]],[[m]],[[]],[[]])
     else   
     let y = List.rev other_than_m in 
     let (old_sols,old_rules,old_sols1,old_rules1) = old_f y in 
     let trial1 = force_insert_in m old_sols 
     and trial2 = force_insert_in m old_rules in 
     if trial1 <> [] 
     then  (trial1,trial2,
            il_merge (force_insert_in m old_sols1) old_sols,
            careful_merge (force_insert_in m old_rules1) old_rules )
     else
     if trial2 = [] 
     then (il_merge (force_insert_in m old_sols1) old_sols,
           careful_merge (force_insert_in m old_rules1) old_rules,
           old_sols1,[[]]) 
     else  raise (Middleman_exn(x))
) ;;

let measure x =  
    let (sols,rules,_,_) = middleman x in 
    List.hd sols;;      

 

let syndectical_power_set x = 
  let temp1 = Listennou.power_set x in 
  let temp2 = List.filter (fun z->
     if z = [] then false else
     let ttemp3 = (List.hd z) :: (Arithmetic_list.delta z) in 
  List.for_all (fun t->t<=2) ttemp3) temp1 in 
  il_sort temp2 ;;  

let sps = Memoized.make (fun n->syndectical_power_set(Int_range.range 1 n)) ;;

let syndectical_merger x y = 
  let m = List.hd (List.rev x) in 
  x @ (Image.image (fun t->m+t) y) ;;

let relative_measure x small_part_of_x =
    let cropped_x = i_setminus x small_part_of_x in 
    (List.length(measure x)) - (List.length(measure cropped_x)) ;;

(*    
  let d_measure x =
    let almost_x = List.rev(List.tl(List.rev x)) in 
    (List.length(measure x)) - (List.length(measure almost_x)) ;;
*)

module First_attempt = struct 

let d_measure x = 
  let m = List.hd (List.rev x) in 
  relative_measure x [m] ;;

let diff x y = d_measure (syndectical_merger x y) ;; 

let left_base = sps 12 ;;

let small_n = 4 ;;
let right_base = sps small_n ;;
let shadow x = Image.image (diff x) right_base ;;
      
let u3 = Explicit.image  (fun z->(shadow z,z)) left_base ;;
let u4 = Listennou.partition_according_to_fst u3 ;;

let bigger_n = small_n+1 ;;
let bigger_right_base = sps bigger_n ;;
let bigger_shadow x = Image.image (diff x) bigger_right_base ;;
let u5 = Image.image (fun (_,l)->(l,
   Ordered.sort Total_ordering.standard (Image.image bigger_shadow l)
) ) u4;;
let check_u5 = List.filter (fun (l,y)->List.length(y)>1 ) u5;;

let u6 = Int_range.index_everything (Image.image snd u4) ;;
exception Doherty_exn of int list ;;
let doherty_index =Memoized.make(fun x ->
    try fst(List.find (fun (idx,y)->List.mem x y) u6)  with 
    _ -> raise (Doherty_exn(x))
)  ;;   
let waters_index x=
    let tempf = (fun j->doherty_index(syndectical_merger x [j])) in 
    (tempf 1,tempf 2) ;;
let u7 = Image.image (fun (_,l)->
   List.filter (fun z->Max.list(z)<=10) l
  ) u4;;
let u8 = Int_range.index_everything u7 ;;  

let u9 = Image.image (fun (idx,l)->(idx,
    Ordered.sort Total_ordering.standard (Image.image waters_index l)
 ) ) u8;;

let check_u9 = (u9 = [(1, [(2, 3)]); (2, [(4, 3)]); (3, [(5, 4)]); (4, [(1, 1)]); (5, [(6, 3)]);
(6, [(3, 1)])])
;; 
  
let representatives = Image.image (fun (idx,l)->(idx,List.hd l)) u8 ;;

end ;;  

let d_measure x = 
  let m = List.hd (List.rev x) in 
  let y = i_setminus x [m] in 
  (relative_measure x [m],relative_measure y [m-2],relative_measure y [m-4;m-1]) ;;

let diff x y = d_measure (syndectical_merger x y) ;; 

let left_base = sps 12 ;;

let small_n = 2 ;;
let right_base = sps small_n ;;
let shadow x = Image.image (diff x) right_base ;;
      
let u3 = Explicit.image  (fun z->(shadow z,z)) left_base ;;
let u4 = Listennou.partition_according_to_fst u3 ;;

let bigger_n = small_n+1 ;;
let bigger_right_base = sps bigger_n ;;
let bigger_shadow x = Image.image (diff x) bigger_right_base ;;
let u5 = Image.image (fun (_,l)->(l,
   Ordered.sort Total_ordering.standard (Image.image bigger_shadow l)
) ) u4;;
let check_u5 = List.filter (fun (l,y)->List.length(y)>1 ) u5;;


let u6 = Int_range.index_everything (Image.image snd u4) ;;
exception Doherty_exn of int list ;;
let doherty_index =Memoized.make(fun x ->
    try fst(List.find (fun (idx,y)->List.mem x y) u6)  with 
    _ -> raise (Doherty_exn(x))
)  ;;   
let waters_index x=
    let tempf = (fun j->doherty_index(syndectical_merger x [j])) in 
    (tempf 1,tempf 2) ;;
let u7 = Image.image (fun (_,l)->
   List.filter (fun z->Max.list(z)<=10) l
  ) u4;;
let u8 = Int_range.index_everything u7 ;;  

let u9 = Image.image (fun (idx,l)->(idx,
    Ordered.sort Total_ordering.standard (Image.image waters_index l)
 ) ) u8;;

let check_u9 = (u9 = [(1, [(2, 3)]); (2, [(4, 5)]); (3, [(6, 4)]); (4, [(1, 1)]); (5, [(6, 4)]);
(6, [(7, 5)]); (7, [(3, 8)]); (8, [(2, 3)])])
;; 


let representatives = Image.image (fun (idx,l)->(idx,List.hd l)) u8 ;;

let big_representatives = Image.image (fun (idx,l)->
  let temp1 = List.hd (List.rev l) in 
  let a = List.hd temp1 in 
  (idx,Image.image (fun t->t-a+1) temp1)
) u8 ;;

let dougherty_table = [(1, [2; 3]); (2, [4; 5]); (3, [6; 4]); (4, [1; 1]); (5, [6; 4]); (6, [7; 5]);
(7, [3; 8]); (8, [2; 3])] ;;

exception Bad_dougherty_jump of int * int ;; 

let dougherty_jump transition state = 
  if transition >2 then 1 else
  try List.nth (List.assoc state dougherty_table) (transition-1) with 
   _ -> raise (Bad_dougherty_jump(transition,state)) ;;
  
let rec dougherty_iterator (to_be_treated,state) =
   match to_be_treated with 
    [] -> state 
   | transition :: others -> 
    dougherty_iterator (others,dougherty_jump transition state) ;;

let dougherty_index z =
    let temp1 = Arithmetic_list.delta (0::z) in 
    dougherty_iterator (temp1,4) ;;

let rec dougherty_helper (to_be_treated,treated) =
  if List.length(to_be_treated)<3 then to_be_treated@treated else 
  let di = dougherty_index to_be_treated 
  and m = List.hd (List.rev to_be_treated) in 
  if List.mem di [1;2;6;8] 
  then  dougherty_helper ((i_setminus to_be_treated [m-2;m]),m::treated)
  else   
  if List.mem di [3;5] 
  then  dougherty_helper ((i_setminus to_be_treated [m-4;m-1;m]),m::treated)
  else  dougherty_helper ((i_setminus to_be_treated [m]),treated)  ;;
      

(************************************************************************************************************************
Snippet 89 : Compare two copies of the same directory
************************************************************************************************************************)
open Needed_values ;;

let select = List.filter(
  fun s-> (not(List.mem s ["";".gitignore"]))
    &&(not(Supstring.ends_with s ".json"))
    &&(not(Supstring.ends_with s "/"))
    &&(not(Supstring.begins_with s "node_modules/"))
) ;;
 
let dir1 = home^"/Downloads/YC" ;;
let dir2 = home^"/Teuliou/Sites/Mongoose_example/Current_app" ;;

let read_both x = (x,(rf(dir1^"/"^x),rf(dir2^"/"^x) )) ;;

let v1 = 
  Ordered.sort Total_ordering.lex_for_strings
  (select(Unix_again.quick_beheaded_complete_ls dir1));;

let v2 = 
    Ordered.sort Total_ordering.lex_for_strings
    (select(Unix_again.quick_beheaded_complete_ls dir2));;  

let v12 =  Ordered.setminus Total_ordering.lex_for_strings v1 v2 ;;   
let v21 =  Ordered.setminus Total_ordering.lex_for_strings v2 v1 ;;  


let u1 = Image.image read_both v1 ;;
let u2 = List.filter (fun (fn,(x,y))-> x<>y) u1;;
let u3 = Image.image fst u2 ;;
(************************************************************************************************************************
Snippet 88 : Typical combination of the Check_polished_ocr and Incremental_replace_on_a_set_of_files modules
************************************************************************************************************************)
open Needed_values ;;

let building_site = home^"/Teuliou/html_files/Translations/Building_site/";;

let emptiable_ap = Absolute_path.of_string (building_site^"emptiable_cmist.txt") ;;
let polished_ap = Absolute_path.of_string (building_site^"polished_cmist.txt") ;;
let walker_ap = Absolute_path.of_string (building_site^"walker_cmist.txt") ;;

let ref_for_expected_action = ref None ;;

let put_first_page_on_walker ()=
  if (! ref_for_expected_action) = Some "officialize" 
  then failwith("You just pushed a page. You need to officialize it before putting another page") 
  else    
  let (first_page,new_text1) = Percent_pagination.extract_first_page_in_file emptiable_ap in 
  (
    Io.overwrite_with emptiable_ap new_text1 ;
    Io.overwrite_with walker_ap first_page ;
    ref_for_expected_action := Some "officialize" ;
  ) ;;
   
let officialize () = 
  if (! ref_for_expected_action) = Some "push page" 
  then failwith("You just officialized a page. No need to officialize it a second time") 
  else   
  let walker_text = Io.read_whole_file walker_ap in 
  let _ = Check_polished_ocr.check_footnotes_on_page walker_text in 
  let new_polished_text = (Io.read_whole_file polished_ap) ^ "\n\n" ^ walker_text  in 
  (
    Io.overwrite_with polished_ap new_polished_text;
    ref_for_expected_action := Some "push page" ;
  ) ;;

let compress_paragraph_in_walker_interval i j=
   Lines_in_string.findreplace_in_interval_in_file ("\n"," ") walker_ap  i j ;; 

let this_ap = Absolute_path.of_string 
   (home^"/Teuliou/OCaml/skeptical_duck/watched/watched_not_githubbed/pan.ml") ;;

Incremental_replace_on_a_set_of_files.set_replacements_datafile  this_ap ;;

let beginning_marker = "(" ^ "* Replacements begin here *)" ;; 
let end_marker = "(" ^ "* Replacements end here *)" ;; 
Incremental_replace_on_a_set_of_files.set_markers beginning_marker end_marker ;;
Incremental_replace_on_a_set_of_files.set_receiving_files [emptiable_ap;walker_ap] ;;

let check_pages_and_footnotes () = Check_polished_ocr.check_pages_and_footnotes (Io.read_whole_file polished_ap) ;;

(* Replacements begin here *)


let replacements = [
   ("\012","");
   (" /n"," In");
   (" <e"," se");
   (" 1. "," l. ");
   (" 1s"," Is");
   (" cl "," el ");
   (" cn "," en ");
   (" cs "," es ");
   (" cse"," ese");
   (" cst"," est");
   (" ct "," et ");
   (" Ja "," la ");
   (" mo "," no ");
   (" sc "," se ");
   ("esc ","ese ");
   ("nucv","nuev");
   (" cdad"," edad");
   (" clla"," ella");
   (" cra "," era ");
   (" csta"," esta");
   (" Cf, "," Cf. ");
   (" e. "," c. ");
   (" dcbe"," debe");
   (" elc."," etc.");
   (" ficl"," fiel");
   (" imte"," inte");
   (" Jas "," las ");
   (" lgle"," Igle");
   (" pucb"," pueb");
   (" quc "," que ");
   (" sca "," sea ");
   (" veee"," vece");
   ("(1s. ","(Is. ");
   ("cnerg","energ");
   ("mcdio","medio");
   ("mcter","meter");
   ("tcolo","teolo");
   (" cfect"," efect");
   (" clla "," ella ");
   (" cllas"," ellas");
   (" cntre"," entre");
   (" cstas"," estas");
   (" C\195\173. "," Cf. ");
   (" idemt"," ident");
   (" incfa"," inefa");
   (" Mer. "," Mgr. ");
   (" posce"," posee");
   ("cterna","eterna");
   ("cucrpo","cuerpo");
   ("poscsi","posesi");
   ("posec ","posee ");
   ("vuclve","vuelve");
   (" alina "," alma ");
   (" clerna"," eterna");
   (" cllos "," ellos ");
   (" cxiste"," existe");
   (" desco "," deseo ");
   (" elerna"," eterna");
   (" eloria"," gloria");
   (" elorio"," glorio");
   (" eriatu"," criatu");
   (" mucve "," mueve ");
   (" sicte "," siete ");
   (" Samto "," Santo ");
   (" tinicb"," tinieb");
   ("/nstitu","Institu");
   ("inanera","manera");
   ("lelesia","Iglesia");
   ("picrden","pierden");
   ("quictud","quietud");
   ("S, TH.,","S. TH.,");
   ("S. Ti.,","S. TH.,");
   ("virlude","virtude");
   (" anmento"," aumento");
   (" comocer"," conocer");
   (" descos "," deseos ");
   (" eloria "," gloria ");
   (" elorifi"," glorifi");
   (" Ielesia"," Iglesia");
   (" vuesira"," vuestra");
   ("Acust\195\173n","Agust\195\173n");
   ("entre El","entre \195\137l");
   ("mencster","menester");
   ("nucstros","nuestros");
   ("S, Tit.,","S. TH.,");
   (" descar\194\187"," desear\194\187");
   (" eristian"," cristian");
   (" misinas "," mismas ");
   ("cuanto El","cuanto \195\137l");
   ("maturales","naturales");
   ("siendo El","siendo \195\137l");
   (" eriaturas"," criaturas");
   ("Jesueristo","Jesucristo");
   (" eristianos"," cristianos");
   (" estc "," este ");
   (" fu\195\169 "," fue ");
   (" To. "," Io. ");
   (" ul "," ut ");
   ("$","\\$");
   ("$","\194\167");
   ("(1 lo. ","(1 Io. ");
   ("(1o. ","(Io. ");
   ("(lo. ","(Io. ");
   ("(Lo. ","(Io. ");
   ("(To. ","(Io. ");
   ("/nstitu","Institu");
   ("1%","1\194\176");
   ("2%","2\194\176");
   ("3%","3\194\176");
   ("4%","4\194\176");
   ("a El","a \195\137l");
   ("como El","como \195\137l");
   ("con El","con \195\137l");
   ("C\194\163. ","Cf.");
   ("de El","de \195\137l");
   ("en El","en \195\137l");
   ("inficles","infieles");
   ("In loan.","In Ioan.");
   ("mosotros","nostros");
   ("o\\ve","owe");
   ("para El","para \195\137l");
   ("peeadores","pecadores");
   ("por El","por \195\137l");
   ("que El","que \195\137l");
   ("sin El","sin \195\137l");
   ("S. Ac.","S. AG.");
   ("S. Tit.,","S. TH.,");
   ("y El","y \195\137l");
   ("\194\176","\\textdegree");
   ("totalinente","totalmente");
];;


(* Replacements end here *)

Incremental_replace_on_a_set_of_files.initialize_replacements replacements ;; 

(*
On startup, you can make a few clean-up initializations as follows : 
*)

let act1 () = Chronometer.it (Percent_pagination.modify_file_pagewise
  (fun text->
    Remove_hyphens.in_string(Make_paragraphs_one_lined.in_string text)
    )) emptiable_ap ;;

let act2 () = Chronometer.it Incremental_replace_on_a_set_of_files.apply_all () ;;



let p = put_first_page_on_walker ;;

let o = officialize ;;

let c = compress_paragraph_in_walker_interval ;;

let r (a,b) = Incremental_replace_on_a_set_of_files.add_new_replacement (a,b) ;; 

let f =  check_pages_and_footnotes ;;


(************************************************************************************************************************
Snippet 87 : Code to OCR-size PDF's into .html  (see also Snippet 84 for .txt instead of html)
************************************************************************************************************************)
open Needed_values ;; 

let dirname = "Building_site/";;

let first_treated_page = 16 ;;
let num_of_pages = 3 ;;
let last_treated_page = (first_treated_page-1) + num_of_pages ;;


let bare_filename = "bot.pdf"
let write1 k =
   let sk = string_of_int k 
   and sj = string_of_int (k-first_treated_page+1)
   and sn = string_of_int num_of_pages in 
   "pdftoppm "^bare_filename^" p"^sk^" -png -f "^sk^" -singlefile\n"^
   "tesseract -l eng p"^sk^".png p"^sk^"\n"^
   "mv p"^sk^".txt /media/sf_Downloads/"^dirname^" \n"^
   "echo \""^sk^" : "^sj^" of "^sn^"\"";;


let ap_for_script1 = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/script.sh");;


let script1 = "\n\n\n"^(String.concat "\n" 
 (Int_range.scale write1 first_treated_page last_treated_page))^"\n\n\n" ;;   
   
Io.overwrite_with ap_for_script1 script1;;

let partial_texts_for_html = Int_range.scale (fun k->
   let sk = string_of_int k in 
   let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
  let uncompressed_pagetext = rf fn in 
  let pagetext = Make_paragraphs_one_lined.in_string 
  (Remove_hyphens.in_string uncompressed_pagetext) in  
  pagetext)  first_treated_page last_treated_page ;;
 
 
let full_html_ap = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"full.html");;  
 
let html_beginning = String.concat "\n"
 ["<!DOCTYPE html>"; "<html>"; "<head>";
 "\t<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\">";
 "\t<title> Title  </title>"; "</head>";
 "<body background=\"bg03.gif\">"; "<center>";
 "\t<h1> TITLE </h1>"; "</center>"] ;;

let html_ending = String.concat "\n"
 ["</body>"; "</html>"] ;;

 let html_full_text = 
   String.concat "\n"
   [html_beginning;Htmlize.pages partial_texts_for_html;html_ending] ;;
 
  Io.overwrite_with full_html_ap html_full_text;;


(************************************************************************************************************************
Snippet 86 : Code using the Parse_js module 
************************************************************************************************************************)
(*

let ap1 = Absolute_path.of_string 
 (home^"/Teuliou/Sites/Gwerzher_Leoriou/node_modules/async/lib/async.js") ;;
let text1 = Io.read_whole_file ap1 ;; 

let g1 = Parse_js.tokens text1 ;;
let res1 = Parse_js.parse_string text1 ;;
let res2 = Parse_js.parse_program text1 ;;
let res3 = Parse_js.program_of_string text1 ;;

*)


(************************************************************************************************************************
Snippet 85 : Absorbing code from Y. Padioleau's codebase
************************************************************************************************************************)
let (root,backup_dir,githubbing)=Coma_big_constant.Third_World.triple ;;
let fw_config = Fw_configuration.of_root root ;;
let github_config = Fw_poly.construct_github_configuration 
  ~root:root
  ~dir_for_backup:backup_dir
  ~gitpush_after_backup:githubbing
  ~github_url:Coma_big_constant.github_url
  ~encoding_protected_files:[] ;;
let cs_ref=ref(Fw_with_githubbing.plunge_fw_config_with_github_config  fw_config github_config);;
let s_root = Dfa_root.connectable_to_subpath root ;;
let s_above_root = Cull_string.before_rightmost (Dfa_root.without_trailing_slash root) '/';;

let a1 =(Unix_again.create_subdirs_and_fill_files_if_necessary root
       Coma_constant.minimal_set_of_needed_dirs 
           Coma_constant.conventional_files_with_minimal_content) ;;

let a2 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "common.ml";;
let a3 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "common2.ml";;
(* let a4 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "ocaml.ml";; *)
let a5 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "parse_info.ml";;
let a6 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "flag_parsing_js.ml";;
let a7 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "ast_js.ml";;
let a8 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "parser_js.mly";;
let a9 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "lexer_js.mll";;
(* let a10 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "visitor_js.ml";; *)
let a11 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "token_helpers_js.ml";;
let a12 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "parsing_hacks_js.ml";;
let a13 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "parse_js.ml";;
let a14 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "tools_for_absolute_path.ml";;
let a15 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "absolute_path.ml";;
let a16 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "io.ml";;

let raco () = Modify_coma_state.Reference.recompile cs_ref (Some "aaa");;

let v1 = Fw_with_dependencies.list_values_from_module (!cs_ref) "common" ;;

Needed_values.vfm ;;

let z1 = Fw_with_dependencies.all_endinglesses (!cs_ref) ;;

let fg1 = Modify_coma_state.Syntactic_sugar.forget cs_ref ["common2"] ;;

let current_module = ref ("parse_info") ;;
let ap1() = Absolute_path.of_string ("../Cherokee/old_"^(!current_module)^".ml") ;;
let ap2() = Absolute_path.of_string ("../Cherokee/"^(!current_module)^".ml") ;;

let ci i j= Lines_in_string.copy_interval_from_file_to_file (i,j) (ap1()) (ap2()) ;;
let ri i j = Lines_in_string.remove_interval_in_file (ap2()) i j ;;

let act1 () = Replace_inside.replace_several_inside_file 
 [("PI.","Parse_info.")] (ap2()) ;;




let r1 = [("T.","Parser_js.");("TH.","Token_helpers_js.");("Ast.","Ast_js.")]


let (a,b,c) = Lines_in_string.tripartition_associated_to_interval "" 79 123 ;;
let new_b = Replace_inside.replace_inside_string ("-> T","-> Parser_js.T") b ;;
let text2 = String.concat "\n" [a;new_b;c] ;;



(************************************************************************************************************************
Snippet 84 : Code to OCR-size PDF's into .txt 
************************************************************************************************************************)
open Needed_values ;;

let lag = (0) ;;
let num_of_pages = 12 ;;
let dirname = "Building_site/";;
let first_treated_page = 1 ;;

let bare_filename = "brit.pdf"
let write1 k =
   let sk = string_of_int k 
   and sj = string_of_int (k+lag) in 
   "pdftoppm "^bare_filename^" p"^sk^" -png -f "^sj^" -singlefile\n"^
   "tesseract -l eng p"^sk^".png p"^sk^"\n"^
   "mv p"^sk^".txt /media/sf_Downloads/"^dirname^" \n"^
   "echo "^sk;;


let ap1 = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/script.sh");;

let last_treated_page = (first_treated_page-1) + num_of_pages ;;

let text1 = "\n\n\n"^(String.concat "\n" 
 (Int_range.scale write1 first_treated_page last_treated_page))^"\n\n\n" ;;   
   
Io.overwrite_with ap1 text1;;

let partial_texts_for_txt = Int_range.scale (fun k->
  let sk = string_of_int k in 
  let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
  let announcer = "%\n% Page "^sk^" \n%\n" in 
 let uncompressed_pagetext = rf fn in 
 let pagetext = Make_paragraphs_one_lined.in_string 
 (Remove_hyphens.in_string uncompressed_pagetext) in  
 announcer^pagetext)  first_treated_page last_treated_page ;;

 
 let full_ap = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"full.txt");;  
 
 let txt_full_text = String.concat "\n" partial_texts_for_txt ;;
 
 Io.overwrite_with full_ap txt_full_text;;
 


(************************************************************************************************************************
Snippet 83 : Musing on Egyptian fractions
************************************************************************************************************************)
let rec next_gcd_correct_index (l,walker) = 
  if List.for_all (fun t->(Gcd.gcd t walker) =1) l
  then walker
  else next_gcd_correct_index (l,walker+1) ;;  

let reduce_fraction old_a old_b =
  if old_a = 0 
  then (0,1) 
  else let g = Gcd.gcd old_a old_b in 
       (old_a/g,old_b/g) ;; 

let next_state (l,a,b,walker) = 
  (* it is assume a /b >= 1/walker *)
  let i1 = next_gcd_correct_index (l,walker) in 
  let (small_a,small_b)=reduce_fraction (a*i1-b) (b*i1) in 
  (i1::l,small_a,small_b,Basic.frac_ceiling small_b small_a) ;;

let rec finish_solving old_uple =
 let (l,a,b,walker) = old_uple in 
 if a = 1 
 then (List.rev l,b)
 else 
 finish_solving (next_state old_uple) ;;

let solve a b =   finish_solving([],a,b,Basic.frac_ceiling b a) ;;

let v0 = ([],3,7,Basic.frac_ceiling 7 3) ;;
let ff = Memoized.small next_state v0 ;;



(************************************************************************************************************************
Snippet 82 : Linear algebra on variables indexed by Z^2
************************************************************************************************************************)
let w1 n =Int_range.scale (fun y->(n,y-1)) 1 (n+1) ;;
let w2 n =Int_range.scale (fun x->(n-x,n)) 1 (2*n) ;;
let w3 n =Int_range.scale (fun y->(-n,n-y)) 1 (2*n) ;;
let w4 n =Int_range.scale (fun x->(-n+x,-n)) 1 (2*n) ;;
let w5 n =Int_range.scale (fun y->(n,-n+y)) 1 (n-1) ;;
let ww n =
    if n = 0 then [0,0] else 
    List.flatten (Image.image (fun w->w n) [w1;w2;w3;w4;w5]) ;;  

let small_size = 2 ;;    
let base1 = List.flatten (Int_range.scale ww 0 (2*small_size));;
let b_index pair = Listennou.find_index pair base1  ;;

let for_two =((fun pair1 pair2 ->
  Total_ordering.for_integers 
  (b_index pair1) (b_index pair2) ): (int *int) Total_ordering_t.t);;

let for_ttwo = Total_ordering.product for_two for_two ;;

let for_four =((fun 
   (p1,p2)
   (q1,q2)
   -> 
    let mp = max (b_index p1) (b_index p2) 
    and mq = max (b_index q1) (b_index q2)  in
    let trial1 = Total_ordering.standard mp mq in 
    if trial1 <> Total_ordering_result_t.Equal then trial1 else 
      for_ttwo  (p1,p2) (q1,q2)
): ((int *int)*(int *int)) Total_ordering_t.t ) ;;

let m_index l = Max.list (Image.image b_index l) ;; 

let for_list =((fun 
   l1 l2
   -> 
    let trial1 = Total_ordering.standard (m_index l1) (m_index l2) in 
    if trial1 <> Total_ordering_result_t.Equal then trial1 else 
      Total_ordering.standard l1 l2
): ((int *int) list) Total_ordering_t.t ) ;;


let unchecked_base2= Cartesian.square(Cartesian.square(Int_range.range (-small_size) small_size)) ;;
let check_pair (c,d) = if c=0 then d>0 else c>0 ;;
let check_double_pair ((a,b),(c,d)) =
  ((a,b)<>(0,0)) && (check_pair(c,d)) ;;
let base2 = List.filter check_double_pair unchecked_base2 ;;
let base3 = Image.image (fun ((a,b),(c,d))->
  [(a+c,b+d);(a-c,b-d);(a,b);(c,d)]
  ) base2 ;;
let base4 = Ordered.sort for_list base3 ;;
let standardize_pair (x,y)= if x<0 then (-x,-y) else (x,y) ;;
let mima (a,b) = if (for_two a b)=Total_ordering_result_t.Lower then (b,a) else (a,b) ;;
let standardize_list l=
   let temp1 = Image.image standardize_pair l in 
   let tempf = (fun k->List.nth temp1 (k-1)) in 
   (mima (tempf 1,tempf 2),mima (tempf 3,tempf 4)) ;;
let base5 = Image.image standardize_list base4 ;;
let base6 = Listennou.nonredundant_version base5 ;;

let ff k = List.nth base6 (k-1) ;;


(************************************************************************************************************************
Snippet 81 : Short code related to similar matrices exercise
************************************************************************************************************************)
let test1 n = List.exists (fun a->((a*(4-a)+1) mod n)= 0) (Int_range.range 0 (n-1)) ;;
    
let v1 = List.filter test1 (Int_range.range 3 50) ;;

let see1 n = List.filter (fun a->((a*(4-a)+1) mod n)= 0) (Int_range.range 0 (n-1)) ;;

let u1 = Cartesian.square (Int_range.range (-50) 50) ;;
let sign_condition (x,y) =
    if x<0 then false else 
    if x=0 then y>0 else true ;;
let u2 = List.filter (fun (x,y)->(sign_condition (x,y))&&(Gcd.gcd x y=1)) u1 ;;
let u3 = Image.image (fun (x,y)->(max (abs x) (abs y),(x,y)) ) u2 ;;
let (Set_of_poly_pairs_t.S u4) = Set_of_poly_pairs.sort u3 ;;
let u5 = Image.image snd u4 ;;

let tf1 (a,b) = List.find (fun (z2,z4)->
  abs((a*a-4*a-1)*(z2*z2)+2*(a-2)*b*z2*z4+(b*b)*(z4*z4))<b*b 
  ) u5 ;;

let current_b = 29 ;;  
let abs_b = abs current_b ;;
let good_moduli = see1 abs_b ;;
let ff a = tf1(a,current_b) ;;

(************************************************************************************************************************
Snippet 80 : Exercise related to Cantor set 
************************************************************************************************************************)
let read_fraction s = 
  let (a,b) = Cull_string.split_wrt_rightmost s '/' in 
  (int_of_string a,int_of_string b) ;;

let interval_size_is_smaller_than frac1 frac2 (p,q) =
  let (a1,b1) = read_fraction frac1 
  and (a2,b2) = read_fraction frac2 in 
  q * (a2*b1-a1*b2) < p * (b1*b2)  ;;

let interval_size_is_larger_than frac1 frac2 (p,q) =
   let (a1,b1) = read_fraction frac1 
   and (a2,b2) = read_fraction frac2 in 
   q * (a2*b1-a1*b2) > p * (b1*b2)  ;;

let minmax_using_tor total_ordering x y =
if (Listennou.find_index x total_ordering) < (Listennou.find_index y total_ordering) 
then (x,y)
else (y,x) ;;  

let possibly_empty_interval_using_tor total_ordering a b =
  if minmax_using_tor total_ordering a b = (a,b)
  then Some(a,b)
  else None ;;  

let interval_intersection_using_tor total_ordering (a1,b1) (a2,b2) =
  let (_,a3) = minmax_using_tor total_ordering  a1 a2 
  and (b3,_) = minmax_using_tor total_ordering  b1 b2 in 
  possibly_empty_interval_using_tor total_ordering  a3 b3;; 


let current_k = 3 ;; 

let outer_interval_is_too_small  total_ordering =
 let n = List.length total_ordering in 
 let ia = Listennou.find_index "a" total_ordering 
 and ib = Listennou.find_index "b" total_ordering in 
 if List.mem (ia,ib) [1,2;n-1,n] then true else
 if (ia<2)||(ib>=n) then false else 
 let just_below_a = List.nth total_ordering (ia-2) 
 and just_above_b = List.nth total_ordering ib in 
 interval_size_is_smaller_than just_below_a just_above_b (4,Basic.power 3 current_k) ;; 

let inner_interval_is_too_large  total_ordering =
 let ia = Listennou.find_index "a" total_ordering 
 and ib = Listennou.find_index "b" total_ordering in 
 if ib=ia+1 then false else 
 let just_above_a = List.nth total_ordering ia 
 and just_below_b = List.nth total_ordering (ib-2) in 
 interval_size_is_larger_than just_above_a just_below_b (8,Basic.power 3 current_k) ;; 


let intervals_outside = 
[
  "1/9","2/9";"1/3","2/3";"7/9","8/9"
] ;;

let base1 = List.flatten(Image.image (fun (x,y)->[x;y]) intervals_outside);;

let base2 = Listennou.extend_total_ordering_by_adding_two_elements 
  base1 "a" "b" ;;

let (bad1,good1) = List.partition inner_interval_is_too_large base2 ;; 
let (bad2,good2) = List.partition outer_interval_is_too_small good1 ;; 

let u1 = Image.image (
 fun total_ordering ->
   (Image.image (interval_intersection_using_tor total_ordering ("a","b")) 
   intervals_outside,total_ordering)
) good2 ;;

let u2 = Listennou.partition_according_to_fst u1 ;; 
let tf k = List.nth u2 (k-1) ;; 

(************************************************************************************************************************
Snippet 79 : Enumeration of multi-degrees related to symmetric polynomials
************************************************************************************************************************)
let order_for_triples = ((
  fun (x1,x2,x3) (y1,y2,y3) ->
    let sx = x1+x2+x3
    and sy =y1+y2+y3 in 
    let trial1 = Total_ordering.standard sx sy in 
    if trial1 <> Total_ordering_result_t.Equal then trial1 else   
    let mx = Max.list [x1;x2;x3] 
      and my = Max.list [y1;y2;y3] in 
      let trial2 = Total_ordering.standard mx my in 
      if trial2 <> Total_ordering_result_t.Equal then trial2 else 
      Total_ordering.silex_for_intlists [x1;x2;x3] [y1;y2;y3] 
) :> (int*int*int) Total_ordering_t.t );;

let orbit (x1,x2,x3) = Ordered.sort order_for_triples 
   [ (x1,x2,x3);(x1,x3,x2);(x2,x1,x3);(x2,x3,x1);(x3,x1,x2);(x3,x2,x1); ] ;;

let u1 = Ordered.sort order_for_triples  (Cartesian.cube (Int_range.range 0 8)) ;;   

let u2 = Explicit.image (fun tr->(tr,orbit tr)) u1 ;;
let u3 = List.filter (fun (tr,l)->tr=List.hd l) u2 ;; 
let tf k = List.nth u3 (k-1) ;;


(************************************************************************************************************************
Snippet 78 : Reindex pages of a book for printing
************************************************************************************************************************)
let ap1 = Absolute_path.of_string (Needed_values.home^"/Downloads/Gwenn/");;
let s_ap1 = Absolute_path.to_string ap1 ;;

Coherent_pdf.workspace_directory := s_ap1 ;;
Coherent_pdf.explode ("p","") 248;;

let print_transform n =
    let pre_q=(n/8) in 
    let pre_r= n-8*pre_q in 
    let (q,old_r)=(if pre_r=0 then (pre_q-1,8) else (pre_q,pre_r) ) in 
    let new_r = List.nth [4;1;8;5;2;3;6;7] (old_r-1) in 
    8*q+new_r ;; 

let command_for_index i =
    let j = print_transform i in 
    let si = string_of_int i 
    and sj = string_of_int j in 
    "mv "^s_ap1^"p"^sj^".pdf "^s_ap1^"q"^si^".pdf" ;;

let number_of_chunks = 31 ;;
let reindexing_commands = Int_range.scale command_for_index 1 (8*number_of_chunks) ;;
let act () = Image.image Sys.command reindexing_commands ;;

Coherent_pdf.implode ("q","") ;;

(************************************************************************************************************************
Snippet 77 : Musing on Steinhaus triangles
************************************************************************************************************************)
let i_fold_merge = Ordered.fold_merge Total_ordering.for_integers ;;
let i_sort = Ordered.sort Total_ordering.for_integers ;;
let il_sort = Ordered.sort Total_ordering.silex_for_intlists ;;

let index_from_x unadbridged_x_form =
    let x_form = Cull_string.trim_spaces unadbridged_x_form in 
    if not(Supstring.begins_with x_form "x") 
    then None 
    else
    Some(int_of_string(Cull_string.cobeginning 1 x_form));;     

let indices_from_xlist xlist =
  let parts = Str.split (Str.regexp_string "+") xlist in  
  i_sort(List.filter_map index_from_x parts);;

let temporary_store=Absolute_path.of_string (Needed_values.home^"/Downloads/temp.txt") ;;
let transmitter_file = Absolute_path.of_string "Fads/pan.ml";;

let act () = 
  let stored_text = Io.read_whole_file temporary_store in 
  let temp1 = Replace_inside.replace_several_inside_string 
   ["[","";"]","";"(","";")",""] stored_text in
  let xsums = Str.split (Str.regexp_string ",") temp1 in 
  let dim_after = List.length xsums in  
  let indices = i_fold_merge (Image.image indices_from_xlist xsums) in 
  let ocamlese_before = String.concat "," (Image.image ( fun i->
   "x"^(string_of_int i)
  ) indices) in
  let ocamlese_after = String.concat ";" xsums in 
  let ocamlese_uple = String.concat "," (Int_range.scale ( fun i->
  "tf "^(string_of_int i)
  ) 1 (List.length indices))  in 
  let lines_in_preproduced_text =
  [ 
   "let dim_before = "^(string_of_int (List.length indices))^" ;;";
   "let dim_after = "^(string_of_int dim_after)^" ;;";
   "let to_long_list ("^ocamlese_before^") ="; "    Image.image (fun t-> t mod 2)";
   "    [";
   "      "^ocamlese_after;
   "    ] ;;"; "let to_uple l ="; "    let tf = (fun k->List.nth l (k-1)) in ";
   "    ("^ocamlese_uple^") ;;  "; ] in 
  let preproduced_text = "\n\n\n"^(String.concat "\n" lines_in_preproduced_text)^"\n\n\n" in 
  Replace_inside.overwrite_between_markers_inside_file 
      ~overwriter:preproduced_text
      ("(* Pre-"^"processed part starts here *)","(* Pre-"^"processed part ends here *)")
      transmitter_file ;;


(* Pre-processed part starts here *)


let dim_before = 4 ;;
let dim_after = 36 ;;
let to_long_list (x2,x4,x5,x6) =
    Image.image (fun t-> t mod 2)
    [
      0; x2; x2; 1; x2 + 1; 1; x4; x4 + 1; x2 + x4; x2 + x4 + 1; x5; x4 + x5; x5 + 1; x2 + x4 + x5 + 1; x5; x6; x5 + x6; x4 + x6; x4 + x5 + x6 + 1; x2 + x6; x2 + x5 + x6; x2 + x4 + x5 + x6 + 1; x2 + x4 + x5 + 1; x2 + x4 + x6 + 1; x2 + 1; x2 + x4 + x5 + x6; x4 + x5; x2 + x4 + x6; x5 + 1; x2 + x4 + x6; x5 + x6 + 1; x2 + x4 + x5; x4 + x5 + 1; x2 + x6 + 1; x2 + x4 + x5 + x6 + 1; x5 + 1
    ] ;;
let to_uple l =
    let tf = (fun k->List.nth l (k-1)) in 
    (tf 1,tf 2,tf 3,tf 4) ;;  


(* Pre-processed part ends here *)


let test uple =
  let l = to_long_list uple in
  let n = (List.length l)/2 in 
    let l2 = List.filter (fun y->y = 0) l in 
    (List.length l2)=n ;;
  


let test2 l = test (to_uple l) ;;  
  
let base1 = Int_range.scale (fun _->[0;1]) 1 dim_before ;;    
let base2 = Cartesian.general_product base1 ;;
let base3 = List.filter test2 base2 ;; 
let base4 = Image.image (fun u->(u,to_long_list(to_uple u))) base3 ;;

let nonzero_linear_forms = List.tl(Cartesian.product [0;1] base2) ;;
let eval_linear_form (const_part,linear_part) x=
  let temp1 = List.combine (const_part::linear_part) (1::x) in 
  let temp2 = Image.image (fun (a,b)->a*b) temp1 in 
  abs((Basic.fold_sum temp2) mod 2) ;;
let kernel_of_linear_form = Memoized.make(fun lf ->
    List.filter (fun x->eval_linear_form lf x = 0) base3 ) ;;   
let kernel_size = Memoized.make(fun lf->
    List.length(kernel_of_linear_form lf)
  ) ;;
let computation1 = Explicit.image kernel_size nonzero_linear_forms ;;

let res1 = Max.maximize_it_with_care kernel_size nonzero_linear_forms ;;

let defect_at_index =Memoized.make(fun idx -> 
    let (a,b) = List.partition (fun l->List.nth l (idx-1)=0) base3 in 
    (abs(List.length(b)-List.length(a))) );; 

let minimal_defects =
    Min.minimize_it_with_care  defect_at_index 
      (Int_range.range 1 dim_before) ;;   

let big_proj shadow = il_sort(Image.image (fun l->Listennou.sublist_with_indices l shadow) base3) ;;  
let shadows = il_sort (Listennou.power_set (Int_range.range 1 dim_before)) ;;     
let (_,shadowers) = Max.maximize_it_with_care (fun sh->List.length(big_proj sh)) shadows ;;




(************************************************************************************************************************
Snippet 76 : Draft to preprocess a file using data from PARI-GP
************************************************************************************************************************)
open Needed_values ;;

let i_fold_merge = Ordered.fold_merge Total_ordering.for_integers ;;
let i_sort = Ordered.sort Total_ordering.for_integers ;;

let index_from_x unadbridged_x_form =
    let x_form = Cull_string.trim_spaces unadbridged_x_form in 
    if not(Supstring.begins_with x_form "x") 
    then None 
    else
    Some(int_of_string(Cull_string.cobeginning 1 x_form));;     

let indices_from_xlist xlist =
  let parts = Str.split (Str.regexp_string "+") xlist in  
  i_sort(List.filter_map index_from_x parts);;

let temporary_store=Absolute_path.of_string (home^"/Downloads/temp.txt") ;;
let transmitter_file = Absolute_path.of_string "Fads/pan.ml";;


let stored_text = Io.read_whole_file temporary_store ;;
let temp1 = Replace_inside.replace_several_inside_string 
  ["[","";"]","";"(","";")",""] stored_text ;;
let xsums = Str.split (Str.regexp_string ",") temp1 ;;
let dim_after = List.length xsums ;;
let indices = i_fold_merge (Image.image indices_from_xlist xsums) ;;
let dim_before = List.length indices ;;
let ocamlese_before = String.concat "," (Image.image ( fun i->
   "x"^(string_of_int i)
) indices) ;;
let ocamlese_after = String.concat ";" xsums ;;
let ocamlese_uple = String.concat "," (Image.image ( fun i->
  "tf "^(string_of_int i)
) indices) ;;


let lines_in_preproduced_text =
[ "let max_idx = "^(string_of_int dim_after)^" ;;";
   "let to_long_list ("^ocamlese_before^") ="; "    Image.image (fun t-> t mod 2)";
   "    [";
   "      "^ocamlese_after;
   "    ] ;;"; "let to_uple l ="; "    let tf = (fun k->List.nth l (k-1)) in ";
   "    ("^ocamlese_uple^") ;;  "; ] ;;

let preproduced_text = "\n\n\n"^(String.concat "\n" lines_in_preproduced_text)^"\n\n\n" ;;

let prprpr () = 
    Replace_inside.overwrite_between_markers_inside_file 
      ~overwriter:preproduced_text
      ("(* Pre-"^"processed part starts here *)","(* Pre-"^"processed part ends here *)")
      transmitter_file ;;


(************************************************************************************************************************
Snippet 75 : Lower bounds on linear recurrent sequences of order 2
************************************************************************************************************************)
let nachste (x,y) = (y,4*y-5*x) ;;

let nachstee (x,y,l) = (y,4*y-5*x,x::l) ;; 

let precision = ref 1000 ;;

let measure (x,y) =
    let opt_example = ref None 
    and walker = ref (x,y) 
    and bound=abs(x) in 
    for k = 1 to (!precision) 
    do 
       walker := (nachste(!walker));
       if abs(fst(!walker)) <= bound 
       then opt_example := Some k 
    done;
    match (!opt_example) with 
    None -> 2
   |Some kmax -> (kmax)+1 ;;  

let see_measure (x,y) = 
    let n = measure(x,y) 
    and walker = ref (x,y,[]) in
    for k = 3 to n 
    do 
        walker := nachstee (!walker)
    done;
    let (a,b,l)=(!walker) in 
    List.rev(b::a::l) ;;
    
let base1 =Memoized.make(fun n -> List.filter (fun (x,y)->
    let mx = abs x and my =abs y in 
    ((x,y)<>(0,-1)) &&
    (x>=0)&&((Gcd.gcd x y)=1) && (mx<=my) && (max mx my=n)
  ) (Cartesian.square (Int_range.range (-n) n))) ;;

let base_image1 = Memoized.make (fun n->
    Max.maximize_it_with_care measure 
    (base1 n)
) ;;

let rec breaker_tester bound candidate =
  if fst(base_image1 candidate) >= bound 
  then  candidate 
  else  breaker_tester bound (candidate+1) ;;

let next_breaker = Memoized.recursive (fun old_f bound -> 
     if bound<=3 then (1,(3,[1; 1; -1],[])) else 
     let m = breaker_tester bound (fst(old_f(bound-1))) in 
     let (_,l) = base_image1 m in 
     let (a,others) = Listennou.head_with_tail l in 
     let sol = see_measure a in 
     (m,(List.length sol,sol,others))
  ) ;;


let bi = base_image1 ;;
let sm = see_measure ;;
let nb = next_breaker ;;

(************************************************************************************************************************
Snippet 74 : Debugging compiling of mll and mly files
************************************************************************************************************************)
let (root,backup_dir,githubbing)=Coma_big_constant.Third_World.triple ;;
let fw_config = Fw_configuration.of_root root ;;
let github_config = Fw_poly.construct_github_configuration 
  ~root:root
  ~dir_for_backup:backup_dir
  ~gitpush_after_backup:githubbing
  ~github_url:Coma_big_constant.github_url
  ~encoding_protected_files:[] ;;
let cs_ref=ref(Fw_with_githubbing.plunge_fw_config_with_github_config  fw_config github_config);;
let s_root = Dfa_root.connectable_to_subpath root ;;
let s_above_root = Cull_string.before_rightmost (Dfa_root.without_trailing_slash root) '/';;

let a1 =(Unix_again.create_subdirs_and_fill_files_if_necessary root
       Coma_constant.minimal_set_of_needed_dirs 
           Coma_constant.conventional_files_with_minimal_content) ;;



let a1 =(Unix_again.create_subdirs_and_fill_files_if_necessary root
       Coma_constant.minimal_set_of_needed_dirs 
           Coma_constant.conventional_files_with_minimal_content) ;;

let a2 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "cherokee_token.ml";;

let lines = ["cherokee_lexer.mll"];;

let bad1 () = Modify_coma_state.Syntactic_sugar.register_several cs_ref lines ;;

let rootless_paths = Image.image Dfn_rootless.of_line lines ;;

let bad2 () = Modify_coma_state.Reference.register_rootless_paths cs_ref rootless_paths ;;

let cs = (!cs_ref) ;;

let bad3 () = Modify_coma_state.And_save.register_rootless_paths cs rootless_paths ;;

let bad4 () = Fw_with_githubbing.register_rootless_paths cs rootless_paths ;;

let fw_with_bc = Fw_poly.parent cs ;;

let bad5 () = Fw_with_batch_compilation.register_rootless_paths fw_with_bc rootless_paths ;;

let old_fw_with_deps = Fw_poly.parent  fw_with_bc ;;

let (new_fw_with_deps,((ac_paths,uc_paths,nc_paths),_))=
       Fw_with_dependencies.register_rootless_paths old_fw_with_deps rootless_paths ;;

module BCPri = Fw_with_batch_compilation.Private ;;       

let old_list_of_cmpl_results = BCPri.get_cmpl_results fw_with_bc ;; 

let new_list_of_cmpl_results = Image.image (
        fun mn -> 
          match List.assoc_opt mn old_list_of_cmpl_results with 
          None -> (mn,false)
          |Some(old_res) -> (mn,old_res)
     ) (Fw_with_dependencies.dep_ordered_modules new_fw_with_deps) ;; 

let fw_with_bc2 = BCPri.usual_extension new_fw_with_deps new_list_of_cmpl_results ;;    

let unordered_mods = Image.image Dfn_rootless.to_module uc_paths ;;

let bad6 () = BCPri.modern_recompile fw_with_bc2 unordered_mods;;

module BCOtm = BCPri.Ocaml_target_making ;;

let (all_deps,new_deps,changed_modules) = 
        Fw_with_dependencies.below_several new_fw_with_deps unordered_mods ;;
let bad7 ()=
        BCOtm.usual_feydeau fw_with_bc2 all_deps ;;

let cmod = Compilation_mode_t.Usual ;;

let (opt_modnames,opt_rootless_path)= (Some(all_deps),None);;

let bad8 ()=
   BCOtm.feydeau cmod fw_with_bc2 (opt_modnames,opt_rootless_path);;

let bad9 ()=
   BCOtm.shaft_part_of_feydeau cmod fw_with_bc2 (opt_modnames,opt_rootless_path);;   

let bad10 () =
   BCOtm.list_of_commands_for_shaft_part_of_feydeau cmod fw_with_bc2 (opt_modnames,opt_rootless_path) ;;

let l=BCOtm.dependencies_inside_shaft cmod fw_with_bc2 (opt_modnames,opt_rootless_path) ;;

let mn0 = List.hd l ;;

let eless0=Fw_with_dependencies.endingless_at_module fw_with_bc2 mn0 ;;

module BCCmd = BCPri.Command ;;

let bad11 () = BCCmd.module_separate_compilation cmod fw_with_bc2 eless0 ;;

let bad12 () = Commands_for_batch_compilation.module_separate_compilation cmod new_fw_with_deps eless0 ;;

let check = 
   Commands_for_batch_compilation.Private.command_for_cmo_from_mll cmod root fw_with_bc2 eless0;;


(*

let a0 = 
   let i1 = Sys.command ("rm -rf "^s_root^"*") in
   let i2 = Sys.command ("cp "^s_above_root^"/Wyoming/* "^s_root) in 
   (i1,i2) ;; 

#use"Fads/cloth.ml";;   



*)

(************************************************************************************************************************
Snippet 73 : Duplicating a paragraph in a file 
************************************************************************************************************************)
let ap1 = Absolute_path.of_string  
   "Compilation_management/commands_for_batch_compilation.ml" ;;
let text1 = Io.read_whole_file ap1 ;;

let (a,b,c) = Lines_in_string.tripartition_associated_to_interval text1 75 97 ;;

let text2 = String.concat "\n\n" [a;b;b;c] ;;

Io.overwrite_with ap1 text2 ;;


(************************************************************************************************************************
Snippet 72 : Write mathjax text for answer on chain additions
************************************************************************************************************************)
open Needed_values ;;

let i_order = Total_ordering.for_integers ;;
let il_order = Total_ordering.silex_compare i_order ;;

let i_is_included_in = Ordered.is_included_in i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_sort = Ordered.sort i_order ;;

let il_fold_merge = Ordered.fold_merge il_order ;;
let il_sort = Ordered.sort il_order ;;

let decompositions n = Int_range.scale (fun j->(j,n-j)) 1 (n/2) ;;

let try_easier_path old_f l = 
    if List.length (l) < 2 then None else 
    let (a,others) = Listennou.head_with_tail l in   
       let candidate = List.filter (List.mem a) (old_f others ) in 
       if candidate = [] then None else Some candidate ;;

let abauzit_order = 
    let tempf=(
          fun l1 l2->
           let t=Total_ordering.standard(List.length l1)(List.length l2) in
           if t<>Total_ordering_result_t.Equal then t else
           il_order (List.rev l1) (List.rev l2)
    ) in
     (tempf:>( (int list) Total_ordering_t.t));;
      
let abauzit_expansion l = 
    let (n,temp1) = Listennou.head_with_tail (List.rev l) in 
    let temp2 = List.rev temp1 
    and decs = decompositions n in 
    let temp3 = Image.image (fun (a,b)->i_merge (i_sort [a;b]) temp2) decs in   
    Ordered.sort abauzit_order temp3 ;;

let main = Memoized.recursive (fun old_f l->
   if l=[1] then [[1]] else
   match try_easier_path old_f l with 
    Some(easier_answer) -> easier_answer 
   | None ->    
     let n = List.hd(List.rev l) in 
     let temp1 = abauzit_expansion l in 
     let temp2 = Image.image old_f temp1 in 
     let (m,temp3) = Min.minimize_it_with_care (fun res->List.length(List.hd res)) temp2 in
     let temp4 = il_fold_merge temp3 in 
     Image.image (fun sol->sol@[n]) temp4
) ;;

let measure = Memoized.make(fun l->
    let res = main l in List.length(List.hd res)
) ;;

let adrien_analysis (l,bound) = 
    List.find_opt (fun (h,l2)->
      (measure [h]) + (List.length l2) >= bound  
    )  (Three_parts.beheaded_tails l) ;;
    
let singleton_analysis n =
  let temp1 = decompositions n in 
  let temp2 = Image.image (fun (a,b)->(a,b,measure [a],measure [b])) temp1 in 
  let (almost_m,temp3) = Min.minimize_it_with_care (fun (a,b,ma,mb)->max ma mb) temp2 in 
  (measure [n])=(almost_m+1)  ;;    

let singleton_test l = 
    let n = List.hd(List.rev l) in 
    if l <> [n]
    then false 
    else  singleton_analysis n ;;    

let hashtbl_for_impatient_main = Hashtbl.create 100 ;;     
    
let partial_analysis_without_writing l =
    let temp1 = abauzit_expansion l 
     and bound = (measure l)-1 in 
    let temp2 = Image.image (fun l2-> (l2,adrien_analysis (l2,bound)) ) temp1 in 
    let (good_temp2,bad_temp2) = List.partition (fun (l2,opt)->opt<>None) temp2 in 
    let temp3 = Image.image (fun (l2,opt)->(l2,Option.get opt)) good_temp2 in 
    let temp4 = List.filter_map (
      fun (l2,_) -> 
        if Hashtbl.find_opt hashtbl_for_impatient_main l2 = None 
        then Some l2 
        else None 
    ) bad_temp2 in 
    (singleton_test l,temp3,temp4,List.hd(main l)) ;; 
    
let soi = string_of_int ;; 
let soil l = "["^(String.concat ";" (Image.image soi l))^"]";;

let pointed_card_element_to_string (x,(head,passive_part)) =
   "("^(soil x)^",("^(soi head)^","^(soil passive_part)^"))";;

let pointed_card_to_string l = 
   "["^(String.concat ";" (Image.image pointed_card_element_to_string l))^"]";;

let write_about_singleton n solution =
   "simplest_case "^(soi n)^" "^(soil solution)^" ;;";;

let write_about_missing_link missing_link =
  "pa "^(soil missing_link)^" ;;";;

  let write_about_missing_links missing_links =
    String.concat "\n" (Image.image write_about_missing_link missing_links);;

let write_about_pointed_card l pointed_card solution  = 
  "pointed_card "^(soil l)^" "^
  (pointed_card_to_string pointed_card)^" "^(soil solution)^" ;;";;
  ;;

let write_about_partial_analysis l (is_singleton,pointed_card,missing_links,solution) =
  let n = List.hd(List.rev solution) in 
   if is_singleton 
   then write_about_singleton n solution 
   else
   if missing_links <> []
   then write_about_missing_links missing_links         
   else write_about_pointed_card l pointed_card solution ;;

let partial_analysis l =
    let temp = partial_analysis_without_writing l in 
    let text = "\n\n\n"^(write_about_partial_analysis l temp)^"\n\n\n" in 
    let _ = (print_string text;flush stdout) in 
    temp ;;

let check_admissibility x=
  let rec tempf =( 
     fun to_be_treated -> match to_be_treated with 
     [] -> true
     | a:: others -> 
        if others = []
        then true 
        else if List.exists (fun (x,y)->x+y=a) (Uple.inclusive_list_of_pairs others) 
        then tempf(others)
        else false     
  ) in 
  tempf(List.rev x) ;;
  
exception Nonadmissible_set of int list ;;

let check_admissiblity_agressively sol =
    if check_admissibility sol then () else raise(Nonadmissible_set(sol));;

exception Non_inclusion of (int list) * (int list) ;;

let initial_check (x,sol) =
   let _ = check_admissiblity_agressively sol in 
   if not(i_is_included_in x sol)
   then raise(Non_inclusion(x,sol)) 
   else () ;;    



exception Impatient_main_exn of int list ;;

let impatient_main l = match Hashtbl.find_opt hashtbl_for_impatient_main l with 
  Some (answer) -> answer
  |None -> raise(Impatient_main_exn(l));;

let impatient_measure l = List.length (impatient_main l) ;;

Hashtbl.add hashtbl_for_impatient_main [1] [1] ;;

exception Simplest_case_exn of (int list) * int ;; 

let simplest_case n sol = 
    let _ = initial_check ([n],sol) in 
    let temp1 = abauzit_expansion [n] in 
    let tempm = (fun l->Max.list (Image.image (fun t->impatient_measure [t]) l)) in 
    let m = Min.list (Image.image tempm temp1) in     
    if List.length(sol)<>m+1 
    then raise(Simplest_case_exn(sol,m+1))
    else  Hashtbl.add  hashtbl_for_impatient_main [n] sol ;;

exception Bad_pointed_card_element of (int list) * int * (int list) ;; 

let factor (x,y)=
    let rec factor0=(fun
       (graet,da_ober1,da_ober2)->
       if (da_ober1=[])||(da_ober2=[])
       then (List.rev graet,da_ober1,da_ober2)
       else let (a1,peurrest1)=Listennou.head_with_tail da_ober1
            and (a2,peurrest2)=Listennou.head_with_tail da_ober2 in
            if a1=a2
            then factor0(a1::graet,peurrest1,peurrest2)
            else (List.rev graet,da_ober1,da_ober2)
    ) in
    factor0([],x,y);;

let extends l1 l2=
   let (_,_,r2)=factor (l1,l2) in r2=[];;

let check_pointed_card_element (l2,(head,passive_part))=
   if not(extends (List.rev l2) (List.rev(head::passive_part))) 
   then raise(Bad_pointed_card_element(l2,head,passive_part))
   else () ;;
   
let check_pointed_card = List.iter  check_pointed_card_element ;;

exception Pointed_card_exn of ((int list) * int) list ;;

let pointed_card l pointed_card sol =
  let _ = initial_check (l,sol) 
  and _ = check_pointed_card pointed_card in 
  let temp1 = abauzit_expansion l 
  and bound = List.length(sol)-1 in 
  let temp2 = Image.image (
    fun l2 -> match List.assoc_opt l2 pointed_card with 
    Some(head,passive_part) ->(l2,impatient_measure([head])+List.length passive_part)
    |None -> (l2,impatient_measure l2)
  ) temp1 in 
  let bad_ones = List.filter (fun (l2,m_l2)->m_l2<bound) temp2 in 
  if bad_ones <> [] 
  then raise(Pointed_card_exn(bad_ones))
  else Hashtbl.add  hashtbl_for_impatient_main l sol ;;    

let pa = partial_analysis ;;
let pb n = pa [n] ;;

simplest_case 2 [1;2] ;;
simplest_case 3 [1;2;3] ;;
simplest_case 4 [1;2;4] ;;
simplest_case 5 [1;2;3;5] ;;
simplest_case 6 [1;2;3;6] ;;
pointed_card [7] [([3; 4], (3, [4])); ([2; 5], (5, [])); ([1; 6], (6, []))]  [1; 2; 3; 4; 7] ;;
pointed_card [3;8] [([3; 4], (3, [4])); ([3; 5], (3, [5])); ([2; 3; 6], (2, [3; 6]));
([1; 3; 7], (3, [7]))] [1; 2; 3; 4; 8];;
simplest_case 8 [1;2;4;8] ;;
simplest_case 9 [1; 2; 3; 6; 9] ;;
simplest_case 10 [1; 2; 3; 5; 10] ;;
pointed_card [11]  [([5; 6], (5, [6])); ([4; 7], (7, [])); ([2; 9], (9, []));
([1; 10], (10, []))] [1; 2; 3; 4; 7; 11] ;;
pointed_card [5;12] [([5;6],(5,[6]));([5;7],(5,[7]));([4;5;8],(4,[5;8]));([3;5;9],(3,[5;9]));([2;5;10],(5,[10]));([1;5;11],(5,[11]))] [1;2;3;5;6;12] ;;
simplest_case 12 [1; 2; 3; 6; 12] ;;
pointed_card [13] [([6; 7], (6, [7])); ([5; 8], (5, [8])); ([4; 9], (9, []));
([3; 10], (10, [])); ([2; 11], (11, [])); ([1; 12], (12, []))] [1; 2; 3; 5; 8; 13] ;;
pointed_card [14] [([7], (7, [])); ([6; 8], (6, [8])); ([5; 9], (5, [9])); ([4; 10], (10, []));
([3; 11], (11, [])); ([2; 12], (12, [])); ([1; 13], (13, []))]  [1; 2; 3; 4; 7; 14] ;;
simplest_case 15 [1; 2; 3; 5; 10; 15] ;;
pointed_card [3;16] [([3;13],(13,[]));([3;7;9],(3,[7;9]));([3;6;10],(3,[6;10]));([3;5;11],(3,[5;11]));([3;4;12],(3,[4;12]));([2;3;14],(14,[]));([1;3;15],(15,[]))] [1;2;3;4;8;16] ;;
pointed_card [5;16] [([5;8],(5,[8]));([5;11],(5,[11]));([5;7;9],(5,[7;9]));([5;6;10],(5,[6;10]));([4;5;12],(4,[5;12]));([3;5;13],(3,[5;13]));([2;5;14],(5,[14]));([1;5;15],(5,[15]))] [1;2;3;5;8;16] ;;
pointed_card [6;16] [([6;8],(6,[8]));([6;10],(6,[10]));([6;7;9],(6,[7;9]));([5;6;11],(5,[6;11]));([4;6;12],(4,[6;12]));([3;6;13],(3,[6;13]));([2;6;14],(6,[14]));([1;6;15],(6,[15]))] [1;2;3;6;8;16] ;;
pointed_card [7;16] [([7;8],(7,[8]));([7;9],(7,[9]));([6;7;10],(6,[7;10]));([5;7;11],(5,[7;11]));([4;7;12],(7,[12]));([3;7;13],(7,[13]));([2;7;14],(7,[14]));([1;7;15],(7,[15]))] [1;2;3;4;7;8;16] ;;
simplest_case 16 [1; 2; 4; 8; 16] ;;
pointed_card [12;17] [([8;9;12],(8,[9;12]));([7;10;12],(7,[10;12]));([6;11;12],(6,[11;12]));([4;12;13],(12,[13]));([3;12;14],(12,[14]));([2;12;15],(12,[15]));([1;12;16],(12,[16]))] [1;2;3;5;6;12;17] ;;
simplest_case 17 [1; 2; 4; 8; 9; 17] ;;
simplest_case 18 [1; 2; 3; 6; 9; 18] ;;
pointed_card [19] [([9;10],(9,[10]));([8;11],(11,[]));([7;12],(7,[12]));([6;13],(13,[]));([5;14],(14,[]));([4;15],(15,[]));([2;17],(17,[]));([1;18],(18,[]))] [1;2;3;4;8;11;19] ;;
pointed_card [7;20] [([7;10],(7,[10]));([7;13],(7,[13]));([7;9;11],(7,[9;11]));([7;8;12],(7,[8;12]));([6;7;14],(6,[7;14]));([5;7;15],(5,[7;15]));([4;7;16],(7,[16]));([3;7;17],(7,[17]));([2;7;18],(7,[18]));([1;7;19],(7,[19]))] [1;2;3;4;7;10;20] ;;
pointed_card [9;20] [([9;10],(9,[10]));([9;11],(9,[11]));([8;9;12],(8,[9;12]));([7;9;13],(7,[9;13]));([6;9;14],(6,[9;14]));([5;9;15],(5,[9;15]));([4;9;16],(9,[16]));([3;9;17],(9,[17]));([2;9;18],(9,[18]));([1;9;19],(9,[19]))] [1;2;3;6;9;10;20] ;;
simplest_case 20 [1;2;3;5;10;20] ;;
pointed_card [21] [([10;11],(10,[11]));([9;12],(9,[12]));([8;13],(13,[]));([7;14],(7,[14]));([6;15],(15,[]));([4;17],(17,[]));([3;18],(18,[]));([2;19],(19,[]));([1;20],(20,[]))] [1;2;3;4;7;14;21] ;;
pointed_card [22] [([11],(11,[]));([10;12],(10,[12]));([9;13],(9,[13]));([8;14],(14,[]));([7;15],(7,[15]));([5;17],(17,[]));([4;18],(18,[]));([3;19],(19,[]));([2;20],(20,[]));([1;21],(21,[]))] [1;2;3;4;7;11;22] ;;
pointed_card [23] [([11;12],(11,[12]));([10;13],(10,[13]));([9;14],(9,[14]));([8;15],(15,[]));([7;16],(7,[16]));([6;17],(17,[]));([5;18],(18,[]));([4;19],(19,[]));([3;20],(20,[]));([2;21],(21,[]));([1;22],(22,[]))] [1;2;3;5;10;13;23] ;;
pointed_card [5;24] [([5;19],(19,[]));([5;11;13],(5,[11;13]));([5;10;14],(5,[10;14]));([5;9;15],(5,[9;15]));([5;8;16],(5,[8;16]));([5;7;17],(5,[7;17]));([5;6;18],(5,[6;18]));([4;5;20],(20,[]));([3;5;21],(21,[]));([2;5;22],(22,[]));([1;5;23],(23,[]))] [1;2;3;5;6;12;24] ;;
pointed_card [7;24] [([7;12],(7,[12]));([7;17],(7,[17]));([7;11;13],(7,[11;13]));([7;10;14],(7,[10;14]));([7;9;15],(7,[9;15]));([7;8;16],(7,[8;16]));([6;7;18],(6,[7;18]));([5;7;19],(5,[7;19]));([4;7;20],(7,[20]));([3;7;21],(7,[21]));([2;7;22],(7,[22]));([1;7;23],(7,[23]))] [1;2;3;5;7;12;24] ;;
simplest_case 24 [1;2;3;6;12;24] ;;
pointed_card [25] [([12;13],(12,[13]));([11;14],(11,[14]));([10;15],(10,[15]));([9;16],(9,[16]));([8;17],(17,[]));([7;18],(7,[18]));([6;19],(19,[]));([5;20],(20,[]));([4;21],(21,[]));([3;22],(22,[]));([2;23],(23,[]));([1;24],(24,[]))] [1;2;3;5;10;15;25] ;;
pointed_card [26] [([13],(13,[]));([12;14],(12,[14]));([11;15],(11,[15]));([10;16],(10,[16]));([9;17],(9,[17]));([8;18],(18,[]));([7;19],(7,[19]));([6;20],(20,[]));([5;21],(21,[]));([4;22],(22,[]));([3;23],(23,[]));([2;24],(24,[]));([1;25],(25,[]))] [1;2;3;5;8;13;26] ;;
pointed_card [20;27] [([13;14;20],(13,[14;20]));([12;15;20],(12,[15;20]));([11;16;20],(11,[16;20]));([10;17;20],(10,[17;20]));([9;18;20],(9,[18;20]));([8;19;20],(19,[20]));([6;20;21],(20,[21]));([5;20;22],(20,[22]));([4;20;23],(20,[23]));([3;20;24],(20,[24]));([2;20;25],(20,[25]));([1;20;26],(20,[26]))] [1;2;3;4;7;10;20;27] ;;
simplest_case 27 [1;2;3;6;9;18;27] ;;
pointed_card [28] [([14],(14,[]));([13;15],(13,[15]));([12;16],(12,[16]));([11;17],(11,[17]));([10;18],(10,[18]));([9;19],(9,[19]));([8;20],(20,[]));([7;21],(7,[21]));([6;22],(22,[]));([5;23],(23,[]));([4;24],(24,[]));([3;25],(25,[]));([2;26],(26,[]));([1;27],(27,[]))] [1;2;3;4;7;14;28] ;;
pointed_card [29] [([14;15],(14,[15]));([13;16],(13,[16]));([11;18],(11,[18]));([10;19],(19,[]));([8;21],(21,[]));([7;22],(22,[]));([6;23],(23,[]));([4;25],(25,[]));([3;26],(26,[]));([2;27],(27,[]));([1;28],(28,[]))] [1;2;3;4;7;11;18;29] ;;
pointed_card [17;30] [([13;17],(13,[17]));([15;17],(15,[17]));([14;16;17],(14,[16;17]));([12;17;18],(12,[17;18]));([11;17;19],(11,[17;19]));([10;17;20],(10,[17;20]));([9;17;21],(9,[17;21]));([8;17;22],(17,[22]));([7;17;23],(7,[17;23]));([6;17;24],(17,[24]));([5;17;25],(17,[25]));([4;17;26],(17,[26]));([3;17;27],(17,[27]));([2;17;28],(17,[28]));([1;17;29],(17,[29]))] [1;2;3;5;10;15;17;30] ;;
simplest_case 30 [1;2;3;5;10;15;30] ;;
pointed_card [31] [([15;16],(15,[16]));([14;17],(14,[17]));([13;18],(13,[18]));([12;19],(19,[]));([11;20],(11,[20]));([10;21],(21,[]));([9;22],(22,[]));([8;23],(23,[]));([6;25],(25,[]));([5;26],(26,[]));([4;27],(27,[]));([3;28],(28,[]));([2;29],(29,[]));([1;30],(30,[]))] [1;2;3;4;7;14;17;31] ;;
pointed_card [5;16;32] [([5;16;27],(5,[16;27]));([5;15;16;17],(5,[15;16;17]));([5;14;16;18],(5,[14;16;18]));([5;13;16;19],(5,[13;16;19]));([5;12;16;20],(5,[12;16;20]));([5;11;16;21],(5,[11;16;21]));([5;10;16;22],(5,[10;16;22]));([5;9;16;23],(5,[9;16;23]));([5;8;16;24],(5,[8;16;24]));([5;7;16;25],(5,[7;16;25]));([5;6;16;26],(5,[6;16;26]));([4;5;16;28],(4,[5;16;28]));([3;5;16;29],(3,[5;16;29]));([2;5;16;30],(5,[16;30]));([1;5;16;31],(5,[16;31]))] [1;2;3;5;8;16;32] ;;
pointed_card [3;32] [([3;29],(29,[]));([3;15;17],(15,[17]));([3;14;18],(14,[18]));([3;13;19],(13,[19]));([3;12;20],(12,[20]));([3;11;21],(11,[21]));([3;10;22],(10,[22]));([3;9;23],(9,[23]));([3;8;24],(24,[]));([3;7;25],(7,[25]));([3;6;26],(26,[]));([3;5;27],(27,[]));([3;4;28],(28,[]));([2;3;30],(30,[]));([1;3;31],(31,[]))] [1;2;3;4;8;16;32] ;;
pointed_card [5;32] [([5;27],(27,[]));([5;15;17],(5,[15;17]));([5;14;18],(5,[14;18]));([5;13;19],(5,[13;19]));([5;12;20],(5,[12;20]));([5;11;21],(5,[11;21]));([5;10;22],(5,[10;22]));([5;9;23],(5,[9;23]));([5;8;24],(5,[8;24]));([5;7;25],(5,[7;25]));([5;6;26],(5,[6;26]));([4;5;28],(28,[]));([3;5;29],(29,[]));([2;5;30],(30,[]));([1;5;31],(31,[]))] [1;2;3;5;8;16;32] ;;
pointed_card [6;32] [([6;26],(26,[]));([6;15;17],(6,[15;17]));([6;14;18],(6,[14;18]));([6;13;19],(6,[13;19]));([6;12;20],(6,[12;20]));([6;11;21],(6,[11;21]));([6;10;22],(6,[10;22]));([6;9;23],(6,[9;23]));([6;8;24],(6,[8;24]));([6;7;25],(6,[7;25]));([5;6;27],(5,[6;27]));([4;6;28],(28,[]));([3;6;29],(29,[]));([2;6;30],(30,[]));([1;6;31],(31,[]))] [1;2;3;6;8;16;32] ;;
pointed_card [7;32] [([7;25],(25,[]));([7;15;17],(7,[15;17]));([7;14;18],(7,[14;18]));([7;13;19],(7,[13;19]));([7;12;20],(7,[12;20]));([7;11;21],(7,[11;21]));([7;10;22],(7,[10;22]));([7;9;23],(7,[9;23]));([7;8;24],(7,[8;24]));([6;7;26],(26,[]));([5;7;27],(27,[]));([4;7;28],(28,[]));([3;7;29],(29,[]));([2;7;30],(30,[]));([1;7;31],(31,[]))] [1;2;3;4;7;8;16;32] ;;
pointed_card [9;32] [([9;16],(9,[16]));([9;23],(9,[23]));([9;15;17],(9,[15;17]));([9;14;18],(9,[14;18]));([9;13;19],(9,[13;19]));([9;12;20],(9,[12;20]));([9;11;21],(9,[11;21]));([9;10;22],(9,[10;22]));([8;9;24],(8,[9;24]));([7;9;25],(7,[9;25]));([6;9;26],(6,[9;26]));([5;9;27],(5,[9;27]));([4;9;28],(9,[28]));([3;9;29],(9,[29]));([2;9;30],(9,[30]));([1;9;31],(9,[31]))] [1;2;4;8;9;16;32] ;;
pointed_card [10;32] [([10;16],(10,[16]));([10;22],(10,[22]));([10;15;17],(10,[15;17]));([10;14;18],(10,[14;18]));([10;13;19],(10,[13;19]));([10;12;20],(10,[12;20]));([10;11;21],(10,[11;21]));([9;10;23],(9,[10;23]));([8;10;24],(8,[10;24]));([7;10;25],(7,[10;25]));([6;10;26],(6,[10;26]));([5;10;27],(5,[10;27]));([4;10;28],(10,[28]));([3;10;29],(10,[29]));([2;10;30],(10,[30]));([1;10;31],(10,[31]))] [1;2;4;6;10;16;32] ;;
pointed_card [12;32] [([12;16],(12,[16]));([12;20],(12,[20]));([12;15;17],(12,[15;17]));([12;14;18],(12,[14;18]));([12;13;19],(12,[13;19]));([11;12;21],(11,[12;21]));([10;12;22],(10,[12;22]));([9;12;23],(9,[12;23]));([8;12;24],(8,[12;24]));([7;12;25],(7,[12;25]));([6;12;26],(6,[12;26]));([5;12;27],(5,[12;27]));([4;12;28],(12,[28]));([3;12;29],(12,[29]));([2;12;30],(12,[30]));([1;12;31],(12,[31]))] [1;2;4;6;12;16;32] ;;
pointed_card [15;32] [([15;16],(15,[16]));([15;17],(15,[17]));([14;15;18],(14,[15;18]));([13;15;19],(13,[15;19]));([12;15;20],(12,[15;20]));([11;15;21],(11,[15;21]));([10;15;22],(10,[15;22]));([9;15;23],(9,[15;23]));([8;15;24],(15,[24]));([7;15;25],(7,[15;25]));([6;15;26],(15,[26]));([5;15;27],(15,[27]));([4;15;28],(15,[28]));([3;15;29],(15,[29]));([2;15;30],(15,[30]));([1;15;31],(15,[31]))] [1;2;3;5;10;15;16;32] ;;
simplest_case 32 [1;2;4;8;16;32] ;;
pointed_card [14;33] [([14;19],(14,[19]));([14;16;17],(14,[16;17]));([14;15;18],(14,[15;18]));([13;14;20],(13,[14;20]));([12;14;21],(12,[14;21]));([11;14;22],(11,[14;22]));([10;14;23],(10,[14;23]));([9;14;24],(9,[14;24]));([8;14;25],(14,[25]));([7;14;26],(7,[14;26]));([6;14;27],(14,[27]));([5;14;28],(14,[28]));([4;14;29],(14,[29]));([3;14;30],(14,[30]));([2;14;31],(14,[31]));([1;14;32],(14,[32]))] [1;2;3;5;7;14;19;33] ;;
pointed_card [20;33] [([13;20],(13,[20]));([16;17;20],(16,[17;20]));([15;18;20],(15,[18;20]));([14;19;20],(14,[19;20]));([12;20;21],(12,[20;21]));([11;20;22],(11,[20;22]));([10;20;23],(10,[20;23]));([9;20;24],(9,[20;24]));([8;20;25],(20,[25]));([7;20;26],(7,[20;26]));([6;20;27],(20,[27]));([5;20;28],(20,[28]));([4;20;29],(20,[29]));([3;20;30],(20,[30]));([2;20;31],(20,[31]));([1;20;32],(20,[32]))] [1;2;3;5;10;13;20;33] ;;
simplest_case 33 [1;2;4;8;16;17;33] ;;
pointed_card [13;34] [([13;17],(13,[17]));([13;21],(13,[21]));([13;16;18],(13,[16;18]));([13;15;19],(13,[15;19]));([13;14;20],(13,[14;20]));([12;13;22],(12,[13;22]));([11;13;23],(11,[13;23]));([10;13;24],(10,[13;24]));([9;13;25],(9,[13;25]));([8;13;26],(13,[26]));([7;13;27],(7,[13;27]));([6;13;28],(13,[28]));([5;13;29],(13,[29]));([4;13;30],(13,[30]));([3;13;31],(13,[31]));([2;13;32],(13,[32]));([1;13;33],(13,[33]))] [1;2;3;5;8;13;21;34] ;;
simplest_case 34 [1;2;4;8;9;17;34] ;;
pointed_card [35] [([17;18],(17,[18]));([16;19],(19,[]));([15;20],(15,[20]));([14;21],(14,[21]));([13;22],(13,[22]));([12;23],(23,[]));([11;24],(11,[24]));([10;25],(25,[]));([9;26],(26,[]));([8;27],(27,[]));([7;28],(28,[]));([6;29],(29,[]));([5;30],(30,[]));([4;31],(31,[]));([2;33],(33,[]));([1;34],(34,[]))] [1;2;3;4;7;14;21;35] ;;
pointed_card [11;36] [([11;18],(11,[18]));([11;25],(11,[25]));([11;17;19],(11,[17;19]));([11;16;20],(11,[16;20]));([11;15;21],(11,[15;21]));([11;14;22],(11,[14;22]));([11;13;23],(11,[13;23]));([11;12;24],(11,[12;24]));([10;11;26],(10,[11;26]));([9;11;27],(9,[11;27]));([8;11;28],(11,[28]));([7;11;29],(7,[11;29]));([6;11;30],(11,[30]));([5;11;31],(11,[31]));([4;11;32],(11,[32]));([3;11;33],(11,[33]));([2;11;34],(11,[34]));([1;11;35],(11,[35]))] [1;2;3;4;7;11;18;36] ;;
pointed_card [17;36] [([17;18],(17,[18]));([17;19],(17,[19]));([16;17;20],(16,[17;20]));([15;17;21],(15,[17;21]));([14;17;22],(14,[17;22]));([13;17;23],(13,[17;23]));([12;17;24],(12,[17;24]));([11;17;25],(11,[17;25]));([10;17;26],(10,[17;26]));([9;17;27],(9,[17;27]));([8;17;28],(17,[28]));([7;17;29],(7,[17;29]));([6;17;30],(17,[30]));([5;17;31],(17,[31]));([4;17;32],(17,[32]));([3;17;33],(17,[33]));([2;17;34],(17,[34]));([1;17;35],(17,[35]))] [1;2;4;8;9;17;18;36] ;;
simplest_case 36 [1;2;3;6;9;18;36] ;;
pointed_card [37] [([18;19],(18,[19]));([17;20],(17,[20]));([16;21],(21,[]));([15;22],(15,[22]));([14;23],(14,[23]));([13;24],(13,[24]));([12;25],(25,[]));([11;26],(11,[26]));([10;27],(27,[]));([9;28],(28,[]));([8;29],(29,[]));([7;30],(30,[]));([6;31],(31,[]));([4;33],(33,[]));([3;34],(34,[]));([2;35],(35,[]));([1;36],(36,[]))] [1;2;3;5;8;16;21;37] ;;
pointed_card [38] [([19],(19,[]));([18;20],(18,[20]));([17;21],(17,[21]));([16;22],(22,[]));([15;23],(15,[23]));([14;24],(14,[24]));([13;25],(13,[25]));([12;26],(26,[]));([11;27],(11,[27]));([10;28],(28,[]));([9;29],(29,[]));([8;30],(30,[]));([7;31],(31,[]));([5;33],(33,[]));([4;34],(34,[]));([3;35],(35,[]));([2;36],(36,[]));([1;37],(37,[]))] [1;2;3;4;8;11;19;38] ;;
pointed_card [39] [([19;20],(19,[20]));([18;21],(18,[21]));([17;22],(17,[22]));([16;23],(23,[]));([15;24],(15,[24]));([14;25],(14,[25]));([13;26],(13,[26]));([12;27],(27,[]));([11;28],(11,[28]));([10;29],(29,[]));([9;30],(30,[]));([8;31],(31,[]));([6;33],(33,[]));([5;34],(34,[]));([4;35],(35,[]));([3;36],(36,[]));([2;37],(37,[]));([1;38],(38,[]))] [1;2;3;5;8;13;26;39] ;;
pointed_card [7;40] [([7;33],(33,[]));([7;19;21],(7,[19;21]));([7;18;22],(7,[18;22]));([7;17;23],(7,[17;23]));([7;16;24],(7,[16;24]));([7;15;25],(7,[15;25]));([7;14;26],(7,[14;26]));([7;13;27],(7,[13;27]));([7;12;28],(7,[12;28]));([7;11;29],(7,[11;29]));([7;10;30],(7,[10;30]));([7;9;31],(7,[9;31]));([7;8;32],(7,[8;32]));([6;7;34],(34,[]));([5;7;35],(35,[]));([4;7;36],(36,[]));([3;7;37],(37,[]));([2;7;38],(38,[]));([1;7;39],(39,[]))] [1;2;3;4;7;10;20;40] ;;
pointed_card [13;40] [([13;20],(13,[20]));([13;27],(13,[27]));([13;19;21],(13,[19;21]));([13;18;22],(13,[18;22]));([13;17;23],(13,[17;23]));([13;16;24],(13,[16;24]));([13;15;25],(13,[15;25]));([13;14;26],(13,[14;26]));([12;13;28],(12,[13;28]));([11;13;29],(11,[13;29]));([10;13;30],(10,[13;30]));([9;13;31],(9,[13;31]));([8;13;32],(13,[32]));([7;13;33],(7,[13;33]));([6;13;34],(13,[34]));([5;13;35],(13,[35]));([4;13;36],(13,[36]));([3;13;37],(13,[37]));([2;13;38],(13,[38]));([1;13;39],(13,[39]))] [1;2;3;5;10;13;20;40] ;;
pointed_card [15;40] [([15;20],(15,[20]));([15;25],(15,[25]));([15;19;21],(15,[19;21]));([15;18;22],(15,[18;22]));([15;17;23],(15,[17;23]));([15;16;24],(15,[16;24]));([14;15;26],(14,[15;26]));([13;15;27],(13,[15;27]));([12;15;28],(12,[15;28]));([11;15;29],(11,[15;29]));([10;15;30],(10,[15;30]));([9;15;31],(9,[15;31]));([8;15;32],(15,[32]));([7;15;33],(7,[15;33]));([6;15;34],(15,[34]));([5;15;35],(15,[35]));([4;15;36],(15,[36]));([3;15;37],(15,[37]));([2;15;38],(15,[38]));([1;15;39],(15,[39]))] [1;2;3;5;10;15;20;40] ;;
simplest_case 40 [1;2;3;5;10;20;40] ;;
pointed_card [41] [([20;21],(20,[21]));([19;22],(19,[22]));([18;23],(18,[23]));([17;24],(17,[24]));([16;25],(25,[]));([15;26],(15,[26]));([14;27],(14,[27]));([13;28],(13,[28]));([12;29],(29,[]));([11;30],(11,[30]));([10;31],(31,[]));([8;33],(33,[]));([7;34],(34,[]));([6;35],(35,[]));([5;36],(36,[]));([4;37],(37,[]));([3;38],(38,[]));([2;39],(39,[]));([1;40],(40,[]))] [1;2;3;5;10;20;21;41] ;;
pointed_card [42] [([21],(21,[]));([20;22],(20,[22]));([19;23],(19,[23]));([18;24],(18,[24]));([17;25],(17,[25]));([16;26],(26,[]));([15;27],(15,[27]));([14;28],(14,[28]));([13;29],(13,[29]));([12;30],(30,[]));([11;31],(11,[31]));([9;33],(33,[]));([8;34],(34,[]));([7;35],(35,[]));([6;36],(36,[]));([5;37],(37,[]));([4;38],(38,[]));([3;39],(39,[]));([2;40],(40,[]));([1;41],(41,[]))] [1;2;3;4;7;14;21;42] ;;
pointed_card [43] [([21;22],(21,[22]));([20;23],(20,[23]));([19;24],(19,[24]));([18;25],(18,[25]));([17;26],(17,[26]));([16;27],(27,[]));([15;28],(15,[28]));([14;29],(14,[29]));([13;30],(13,[30]));([12;31],(31,[]));([11;32],(11,[32]));([10;33],(33,[]));([9;34],(34,[]));([8;35],(35,[]));([7;36],(36,[]));([6;37],(37,[]));([5;38],(38,[]));([4;39],(39,[]));([3;40],(40,[]));([2;41],(41,[]));([1;42],(42,[]))] [1;2;3;5;10;20;23;43] ;;
pointed_card [44] [([22],(22,[]));([21;23],(21,[23]));([20;24],(20,[24]));([19;25],(19,[25]));([18;26],(18,[26]));([17;27],(17,[27]));([16;28],(28,[]));([15;29],(15,[29]));([14;30],(14,[30]));([13;31],(13,[31]));([11;33],(11,[33]));([10;34],(34,[]));([9;35],(35,[]));([8;36],(36,[]));([7;37],(37,[]));([6;38],(38,[]));([5;39],(39,[]));([4;40],(40,[]));([3;41],(41,[]));([2;42],(42,[]));([1;43],(43,[]))] [1;2;3;4;7;11;22;44] ;;
pointed_card [45] [([22;23],(22,[23]));([21;24],(21,[24]));([20;25],(20,[25]));([19;26],(19,[26]));([18;27],(18,[27]));([17;28],(17,[28]));([16;29],(29,[]));([15;30],(15,[30]));([14;31],(14,[31]));([13;32],(13,[32]));([12;33],(33,[]));([11;34],(11,[34]));([10;35],(35,[]));([9;36],(36,[]));([8;37],(37,[]));([7;38],(38,[]));([6;39],(39,[]));([5;40],(40,[]));([4;41],(41,[]));([3;42],(42,[]));([2;43],(43,[]));([1;44],(44,[]))] [1;2;3;5;10;15;30;45] ;;
pointed_card [46] [([23],(23,[]));([22;24],(22,[24]));([21;25],(21,[25]));([20;26],(20,[26]));([19;27],(19,[27]));([18;28],(18,[28]));([17;29],(17,[29]));([16;30],(30,[]));([15;31],(15,[31]));([14;32],(14,[32]));([13;33],(13,[33]));([12;34],(34,[]));([11;35],(11,[35]));([10;36],(36,[]));([9;37],(37,[]));([8;38],(38,[]));([7;39],(39,[]));([6;40],(40,[]));([5;41],(41,[]));([4;42],(42,[]));([3;43],(43,[]));([2;44],(44,[]));([1;45],(45,[]))] [1;2;3;5;10;13;23;46] ;;
pointed_card [47] [([23;24],(23,[24]));([22;25],(22,[25]));([21;26],(21,[26]));([19;28],(19,[28]));([18;29],(29,[]));([16;31],(31,[]));([12;35],(35,[]));([10;37],(37,[]));([9;38],(38,[]));([8;39],(39,[]));([6;41],(41,[]));([5;42],(42,[]));([4;43],(43,[]));([3;44],(44,[]));([2;45],(45,[]));([1;46],(46,[]))] [1;2;3;4;7;10;20;27;47] ;;
pointed_card [5;48] [([5;43],(43,[]));([5;23;25],(23,[25]));([5;22;26],(22,[26]));([5;21;27],(21,[27]));([5;20;28],(20,[28]));([5;19;29],(19,[29]));([5;18;30],(18,[30]));([5;17;31],(17,[31]));([5;15;33],(15,[33]));([5;14;34],(14,[34]));([5;13;35],(13,[35]));([5;12;36],(36,[]));([5;11;37],(11,[37]));([5;10;38],(38,[]));([5;9;39],(39,[]));([5;8;40],(40,[]));([5;7;41],(41,[]));([5;6;42],(42,[]));([4;5;44],(44,[]));([3;5;45],(45,[]));([2;5;46],(46,[]));([1;5;47],(47,[]))] [1;2;3;5;6;12;24;48] ;;
pointed_card [7;48] [([7;41],(41,[]));([7;23;25],(7,[23;25]));([7;22;26],(7,[22;26]));([7;21;27],(7,[21;27]));([7;20;28],(7,[20;28]));([7;19;29],(7,[19;29]));([7;18;30],(7,[18;30]));([7;17;31],(7,[17;31]));([7;16;32],(7,[16;32]));([7;15;33],(7,[15;33]));([7;14;34],(7,[14;34]));([7;13;35],(7,[13;35]));([7;12;36],(7,[12;36]));([7;11;37],(7,[11;37]));([7;10;38],(7,[10;38]));([7;9;39],(7,[9;39]));([7;8;40],(7,[8;40]));([6;7;42],(42,[]));([5;7;43],(43,[]));([4;7;44],(44,[]));([3;7;45],(45,[]));([2;7;46],(46,[]));([1;7;47],(47,[]))] [1;2;3;5;7;12;24;48] ;;
simplest_case 48 [1;2;3;6;12;24;48] ;;
pointed_card [49] [([24;25],(24,[25]));([23;26],(23,[26]));([22;27],(22,[27]));([21;28],(21,[28]));([20;29],(20,[29]));([19;30],(19,[30]));([18;31],(18,[31]));([17;32],(17,[32]));([16;33],(33,[]));([15;34],(15,[34]));([14;35],(14,[35]));([13;36],(13,[36]));([12;37],(37,[]));([11;38],(11,[38]));([10;39],(39,[]));([9;40],(40,[]));([8;41],(41,[]));([7;42],(42,[]));([6;43],(43,[]));([5;44],(44,[]));([4;45],(45,[]));([3;46],(46,[]));([2;47],(47,[]));([1;48],(48,[]))] [1;2;3;6;12;24;25;49] ;;
pointed_card [50] [([25],(25,[]));([24;26],(24,[26]));([23;27],(23,[27]));([22;28],(22,[28]));([21;29],(21,[29]));([20;30],(20,[30]));([19;31],(19,[31]));([18;32],(18,[32]));([17;33],(17,[33]));([16;34],(34,[]));([15;35],(15,[35]));([14;36],(14,[36]));([13;37],(13,[37]));([12;38],(38,[]));([11;39],(11,[39]));([10;40],(40,[]));([9;41],(41,[]));([8;42],(42,[]));([7;43],(43,[]));([6;44],(44,[]));([5;45],(45,[]));([4;46],(46,[]));([3;47],(47,[]));([2;48],(48,[]));([1;49],(49,[]))] [1;2;3;5;10;15;25;50] ;;
simplest_case 51 [1;2;3;6;12;24;27;51] ;;
pointed_card [52] [([26],(26,[]));([25;27],(25,[27]));([24;28],(24,[28]));([23;29],(23,[29]));([22;30],(22,[30]));([21;31],(21,[31]));([20;32],(20,[32]));([19;33],(19,[33]));([18;34],(18,[34]));([17;35],(17,[35]));([16;36],(36,[]));([15;37],(15,[37]));([14;38],(14,[38]));([13;39],(13,[39]));([12;40],(40,[]));([11;41],(11,[41]));([10;42],(42,[]));([9;43],(43,[]));([8;44],(44,[]));([7;45],(45,[]));([6;46],(46,[]));([5;47],(47,[]));([4;48],(48,[]));([3;49],(49,[]));([2;50],(50,[]));([1;51],(51,[]))] [1;2;3;5;8;13;26;52] ;;
pointed_card [53] [([26;27],(26,[27]));([25;28],(25,[28]));([24;29],(29,[]));([23;30],(23,[30]));([22;31],(22,[31]));([21;32],(21,[32]));([19;34],(19,[34]));([18;35],(35,[]));([16;37],(37,[]));([15;38],(38,[]));([14;39],(39,[]));([12;41],(41,[]));([11;42],(42,[]));([10;43],(43,[]));([9;44],(44,[]));([8;45],(45,[]));([7;46],(46,[]));([6;47],(47,[]));([4;49],(49,[]));([3;50],(50,[]));([2;51],(51,[]));([1;52],(52,[]))] [1;2;3;5;6;12;24;29;53] ;;
simplest_case 54 [1;2;3;6;9;18;27;54] ;;
pointed_card [55] [([27;28],(27,[28]));([26;29],(26,[29]));([25;30],(25,[30]));([24;31],(31,[]));([23;32],(23,[32]));([22;33],(22,[33]));([21;34],(21,[34]));([20;35],(35,[]));([19;36],(19,[36]));([18;37],(37,[]));([17;38],(38,[]));([16;39],(39,[]));([14;41],(41,[]));([13;42],(42,[]));([12;43],(43,[]));([11;44],(44,[]));([10;45],(45,[]));([9;46],(46,[]));([8;47],(47,[]));([6;49],(49,[]));([5;50],(50,[]));([4;51],(51,[]));([3;52],(52,[]));([2;53],(53,[]));([1;54],(54,[]))] [1;2;3;4;7;11;22;33;55] ;;



type abbreviated_move = P of int list |S of int ;;

let all_moves = [S 2; S 3; S 4; S 5; S 6; P [7]; P [3; 8]; S 8; S 9; S 10; P [11]; P [5; 12];
S 12; P [13]; P [14]; S 15; P [3; 16]; P [5; 16]; P [6; 16]; P [7; 16]; 
S 16; P [12; 17]; S 17; S 18; P [19]; P [7; 20]; P [9; 20]; S 20; P [21];
P [22]; P [23]; P [5; 24]; P [7; 24]; S 24; P [25]; P [26]; P [20; 27]; 
S 27; P [28]; P [29]; P [17; 30]; S 30; P [31]; P [5; 16; 32]; P [3; 32];
P [5; 32]; P [6; 32]; P [7; 32]; P [9; 32]; P [10; 32]; P [12; 32];
P [15; 32]; S 32; P [14; 33]; P [20; 33]; S 33; P [13; 34]; S 34; P [35];
P [11; 36]; P [17; 36]; S 36; P [37]; P [38]; P [39]; P [7; 40]; P [13; 40];
P [15; 40]; S 40; P [41]; P [42]; P [43]; P [44]; P [45]; P [46]; P [47];
P [5; 48]; P [7; 48]; S 48; P [49]; P [50]; S 51; P [52]; P [53]; S 54;
P [55]] ;;

type expanded_move = ES of int * int | EP of (int list) * int * ((int * int list) list) * ((int list) list) ;;

let expand_move  = function
  S i->ES(i,impatient_measure [i])
  |(P l)-> 
  let temp1 = abauzit_expansion l 
   and bound = (impatient_measure l)-1 in 
  let temp2 = Image.image (fun l2-> (l2,adrien_analysis (l2,bound)) ) temp1 in 
  let (good_temp2,bad_temp2) = List.partition (fun (l2,opt)->opt<>None) temp2 in 
  let pointed_ones = Image.image (fun (l2,opt)->Option.get opt) good_temp2 in 
  let temp4 = Image.image  fst bad_temp2 in 
  EP(l,bound+1,pointed_ones,temp4) ;; 

let all_expanded_moves = Image.image expand_move all_moves ;;  

let write_es_to_mathjax i m = 
    (soi i)^ " & " ^(soi m)^ " & \\textrm{Simplest case}" ;;
    
let intlist_to_mathjax l = "\\lbrace "^(String.concat "," (Image.image soi l))^" \\rbrace";;

let pointed_one_to_mathjax (head,passive) =
   let passive_component = (
     if passive = [] 
     then "" 
     else "|"^(String.concat "," (Image.image soi passive))) in  
     "["^(soi head)^passive_component^"]" ;; 

let pointed_ones_to_mathjax pointed_ones = 
  String.concat "" (Image.image pointed_one_to_mathjax pointed_ones) ;;

let helpers_to_mathjax helpers = 
  String.concat "" (Image.image intlist_to_mathjax helpers) ;;

let write_ep_to_mathjax l m pointed_ones helpers = 
      (intlist_to_mathjax l)^ " & " ^(soi m)^ " & "^ 
      (pointed_ones_to_mathjax pointed_ones)^(helpers_to_mathjax helpers) ;;    
    
let write_expanded_move_to_mathjax = function 
 ES(i,m) -> write_es_to_mathjax i m 
 |EP(l,m,pointed_ones,helpers) -> write_ep_to_mathjax l m pointed_ones helpers ;;

let backslash = "\\" ;;
let double_backslash = backslash ^ backslash ;;

 let write_expanded_move_to_mathjax_line x = 
    (write_expanded_move_to_mathjax x)^double_backslash^"\n\\hline" ;;

let array_in_mathjax l =
   String.concat "\n"
    (
    "\\begin{array}{|r|c|l|}\n\\hline"::
    (" A & \\mu & \\textrm{Explanations}"^double_backslash^"\n\\hline")::  
     (Image.image write_expanded_move_to_mathjax_line l)@ 
    [
    "\\end{array}"
    ]
    ) ;;

let max_part_size = 15 ;;

let cut_into_small_parts  l ~max_part_size =
  let rec tempf = (
      fun (treated,to_be_treated,remaining_size) -> 
           if remaining_size <= max_part_size 
           then List.rev(to_be_treated::treated) 
           else let (reversed_left,right) = Listennou.long_head_with_tail max_part_size to_be_treated in 
                let left = List.rev reversed_left in 
                tempf(left::treated,right,remaining_size-max_part_size)
  ) in 
  tempf ([],l,List.length l) ;;

(* cut_into_small_parts (Ennig.ennig 1 7) ~max_part_size:3 ;; *)

let parts = cut_into_small_parts 
all_expanded_moves ~max_part_size ;;

let prelude ="/////////////////////////////////////////////////////////\nQuestion : \n/////////////////////////////////////////////////////////\n\n" ;;

let arrays_in_mathjax = prelude ^ (String.concat "\n\n\n" (Image.image array_in_mathjax parts)) ;; 

let the_ap = Absolute_path.of_string "~/Teuliou/Bash_scripts/example_maath.txt";;

let act () = Io.overwrite_with the_ap arrays_in_mathjax  ;;

(*

#use "Fads/nap.ml";;

*)

(************************************************************************************************************************
Snippet 71 : Transfer a large snippet from one file to another
************************************************************************************************************************)
open Needed_values ;;

let z1 = rf "Fads/nap.ml" ;;
let z2 = Lines_in_string.interval z1 209 298 ;;
let ap = Absolute_path.of_string "Fads/pan.ml" ;;
let z3 () = Io.Private.append_string_to_file z2 ap ;;

type ps = P of int list |S of int ;;

let accu = ref [] ;;

let simplest_case i x = (accu:=(S i)::(!accu)) ;;
let pointed_card a b c = (accu:=(P a)::(!accu)) ;;

(************************************************************************************************************************
Snippet 70 : Third stab at boundary operator combinatorics 
************************************************************************************************************************)
open Needed_values ;;

let i_order = Total_ordering.for_integers ;;
let il_order = Total_ordering.silex_compare i_order ;;
let j_order = Total_ordering.product i_order il_order ;;

let i_fold_merge = Ordered.fold_merge i_order ;;
let i_intersection = Ordered.intersect i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_setminus = Ordered.setminus i_order ;;
let i_sort = Ordered.sort i_order ;;

let il_sort = Ordered.sort il_order ;;
let il_setminus = Ordered.setminus il_order ;;


let j_merge = Ordered.merge j_order ;;

type set_index = S of int ;;

let n1 = 4 ;;
let whole = Int_range.range 1 n1 ;;
let u1 = il_sort (Listennou.power_set whole) ;;
let rtl l = List.rev (List.tl l);;
let u2 = rtl (rtl u1);;
let normal_form x = 
     let cx = i_setminus whole x in 
     if (il_order x cx)=Total_ordering_result_t.Lower 
     then x else cx ;;
let u3 = List.filter (fun x->normal_form(x)=x) u2;;      
let u4 = Int_range.index_everything u3 ;;
let u5 = Image.image (fun (j,z)->(z,S(j+2))) u4 ;;
let u6 = Image.image (fun x->let nx=normal_form x in (x,List.assoc nx u5)) u2;;


module Atom = struct 

type t = A of int ;;

let order = ((fun (A i) (A j)->Total_ordering.for_integers i j) : t Total_ordering_t.t) ;; 
  
let table_for_sets_containing_a_given_atom = 
    let temp = Image.image (Image.image (fun i->S i)) 
    (il_sort(Listennou.power_set (Int_range.range 1 9))) in 
    Image.image (fun (j,l)->(A j,l)) (Int_range.index_everything temp) ;;
let all = Image.image fst table_for_sets_containing_a_given_atom ;;

let check_boolean_constraints constraints atm =
    let associated_sets = List.assoc atm table_for_sets_containing_a_given_atom in 
    List.for_all (fun (set,bowl)->(List.mem set associated_sets)=bowl) constraints ;;

let unveil (A i) = i ;;

end ;;  

module Early_atom = struct 

type t = EA of int ;;
  
let order = ((fun (EA i) (EA j)->Total_ordering.for_integers i j) : t Total_ordering_t.t) ;; 
    
let table_for_sets_containing_a_given_early_atom = 
      let temp = Image.image (Image.image (fun i->S i)) 
      (il_sort(Listennou.power_set (Int_range.range 1 2))) in 
      Image.image (fun (j,l)->(EA j,l)) (Int_range.index_everything temp) ;;
let all = Image.image fst table_for_sets_containing_a_given_early_atom ;;

let to_boolean_combination atm = 
  let associated_sets = List.assoc atm table_for_sets_containing_a_given_early_atom in 
   Image.image (fun s->(s,List.mem s associated_sets)) [S 1;S 2] ;;

let unveil (EA i) = i ;;

end ;;  

module Early_union = struct 

let order = Total_ordering.silex_compare  Early_atom.order ;; 
let whole = Int_range.scale (fun j->Early_atom.EA j)  1 4;;
let all = Image.image (Image.image (fun j->Early_atom.EA j)) u2 ;;
let all_pairs = Uple.list_of_pairs all ;;
let nondisjoint_pairs =
    List.filter (fun (a,b)->
       not(Ordered.does_not_intersect Early_atom.order a b)
      ) all_pairs ;;
let normal_form x = 
    let cx = Ordered.setminus Early_atom.order whole x in 
    if (order cx x)=Total_ordering_result_t.Lower 
    then cx else x ;;
let all_normal_forms = Image.image normal_form all ;;    
let table_for_image_sets = Image.image (fun x->
  (x,S(2+(Listennou.find_index (normal_form x) all_normal_forms))) 
) all ;;
let image_set x = List.assoc x table_for_image_sets ;;
let intersection x y = Ordered.intersect Early_atom.order x y;;
let union x y = Ordered.merge Early_atom.order x y;;

let unveil l = Image.image Early_atom.unveil l;;

end ;;  



module Molecule = struct 

type t = M of Atom.t list ;;  

let fold_union pre_ll = 
    let ll =  Image.image (fun (M m)-> m) pre_ll in 
    M (Ordered.fold_merge Atom.order ll);;
let intersection (M x) (M y) = M (Ordered.intersect Atom.order x y);;
let setminus (M x) (M y) = M (Ordered.setminus Atom.order x y);;
let union (M x) (M y) = M (Ordered.merge Atom.order x y);;


let delta mx my = union (setminus mx my) (setminus my mx) ;; 

let of_set set_idx =
    M( List.filter_map (
       fun (atm_idx,l)->if List.mem set_idx l then Some atm_idx else None
    ) Atom.table_for_sets_containing_a_given_atom ) ;;

let complement_of_set set_idx = 
  M( List.filter_map (
    fun (atm_idx,l)->if not(List.mem set_idx l) then Some atm_idx else None
 ) Atom.table_for_sets_containing_a_given_atom ) ;;

let of_boolean_combination constraints =
  M(List.filter_map (
    fun (atm_idx,l)->if Atom.check_boolean_constraints constraints atm_idx then Some atm_idx else None
 ) Atom.table_for_sets_containing_a_given_atom) ;;

let of_early_atom eatm = 
  of_boolean_combination (Early_atom.to_boolean_combination eatm) ;;

let of_early_union eatm_l = fold_union (Image.image of_early_atom eatm_l) ;;
    
let of_early_union_image eatm_l = of_set (Early_union.image_set eatm_l) ;;


let main_test (pre_a,pre_b) =
    let a = of_early_union pre_a 
    and b = of_early_union pre_b in 
    let pre_c = Early_union.intersection pre_a pre_b 
    and c = intersection a b in 
    let hand1 = intersection c (of_early_union_image pre_c)
    and hand2 = intersection c (union (of_early_union_image pre_a)  (of_early_union_image pre_b)) in 
    delta hand1 hand2 ;;

let unveil (M l) = Image.image Atom.unveil l ;; 

end ;;  



let z1 = Early_union.nondisjoint_pairs ;;
let veiled_z2 = Image.image (fun (a,b)->((a,b),Molecule.main_test (a,b)) ) z1 ;;
let z2 =  Image.image (fun ((a,b),m)->(Early_union.unveil a,Early_union.unveil b,Molecule.unveil m) ) veiled_z2 ;;   

let aa = Image.image (fun x->Early_atom.EA x) [1;2] ;;
let bb = Image.image (fun x->Early_atom.EA x) [1;3] ;;
let ab = Early_union.union aa bb ;;

let faa = Molecule.of_early_union_image aa ;;
let fbb = Molecule.of_early_union_image bb ;;
let fab = Molecule.of_early_union_image ab ;;

let veiled_tab = Molecule.setminus faa (Molecule.union (Molecule.of_early_union ab) fab) ;;
let tab = Molecule.unveil veiled_tab ;;
let check_tab  = i_setminus tab (i_fold_merge(Image.image (fun (a,b,m)->m) z2));;

let z3 = Image.image (fun (a,b,m)->(a,b,i_intersection m tab)) z2 ;;
let get (a0,b0) = Option.get(List.find_map (fun (a,b,m)->if (a,b)=(a0,b0) then Some(m) else None) z3) ;;
let part1 = (get ([4],[3;4]));;
let tab2 = i_setminus tab part1 ;;

let z4 = z3 ;;
let v1 = List.tl(il_sort(Image.image (fun (a,b,m)->m) z4)) ;;
let v2 = List.rev v1 ;;

let v3 = List.hd v2 ;;
let v4 = List.filter_map (fun (a,b,m)->if m=v3 then Some(a,b) else None) z4 ;;
(*

let veiled_tab = Molecule.setminus fab (Molecule.union faa fbb) ;;
let tab = Molecule.unveil veiled_tab ;;

let check_tab  = i_setminus tab (i_fold_merge(Image.image (fun (a,b,m)->m) z2));;

let z3 = Image.image (fun (a,b,m)->(a,b,i_intersection m tab)) z2 ;;
let get (a0,b0) = Option.unpack(Option.find_and_stop (fun (a,b,m)->if (a,b)=(a0,b0) then Some(m) else None) z3) ;;
let part1 = i_merge (get ([1;2],[1;2;3])) (get ([1;3],[1;2;3]));;
let tab2 = i_setminus tab part1 ;;

let z4 = Image.image (fun (a,b,m)->(a,b,i_intersection m tab2)) z3 ;;

let v1 = List.tl(il_sort(Image.image (fun (a,b,m)->m) z4)) ;;
let v2 = List.rev v1 ;;

let v3 = List.hd v2 ;;
let v4 = Option.filter_and_unpack (fun (a,b,m)->if m=v3 then Some(a,b) else None) z4 ;;
*)


(************************************************************************************************************************
Snippet 69 : Second stab at boundary operator combinatorics 
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let il_order = Total_ordering.silex_compare i_order ;;
let j_order = Total_ordering.product i_order il_order ;;

let i_fold_merge = Ordered.fold_merge i_order ;;
let i_intersection = Ordered.intersect i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_setminus = Ordered.setminus i_order ;;
let i_sort = Ordered.sort i_order ;;

let il_sort = Ordered.sort il_order ;;
let il_setminus = Ordered.setminus il_order ;;


let j_merge = Ordered.merge j_order ;;

let n1 = 4 ;;
let whole = Int_range.range 1 n1 ;;
let u1 = il_sort (Listennou.power_set whole) ;;
let rtl l = List.rev (List.tl l);;
let u2 = rtl (rtl u1);;
let normal_form x = 
     let cx = i_setminus whole x in 
     if (il_order x cx)=Total_ordering_result_t.Lower 
     then x else cx ;;
let u3 = List.filter (fun x->normal_form(x)=x) u2;;      
let u4 = Int_range.index_everything u3 ;;
let u5 = Image.image (fun (j,z)->(z,[j+4])) u4 ;;
let u6 = Image.image (fun x->let nx=normal_form x in (x,(x,List.assoc nx u5))) u2;;
let u7 = Uple.list_of_pairs u2 ;;
let u8 = List.filter (fun (a,b)->(i_intersection a b)<>[]) u7;;
let u9 = Int_range.index_everything u8 ;;

module Kafka = struct

type t = {
  size : int ; 
  atoms : int list ;
  decompositions : (int * (int list)) list ;
  images : ((int list) * ((int list) * (int list))) list ;
} ;;


let share_with_foreigner kfk x = 
   let n = kfk.size 
   and m = List.length(kfk.atoms) in
   let temp1 = Int_range.index_everything kfk.atoms in  
   let decs1 = Image.image (fun (j,atm)->(atm,[n+j;n+j+m])) temp1
   and expansion_for_x = Int_range.range (n+1) (n+m) in 
   let replacer = (fun z->
      if z=x then expansion_for_x else 
      try List.assoc z decs1 with _ -> [z]
    ) in 
   let replacer2 = (fun lz->
    i_fold_merge (Image.image replacer lz)
    ) in  
   let decs2 = Image.image (fun (w,old_decomposition)->
     (w,replacer2 old_decomposition) 
    ) kfk.decompositions in  
   let new_atoms = Int_range.range (n+1) (n+2*m)
   and new_decompositions = j_merge [x,expansion_for_x] (j_merge decs1 decs2) 
   and new_images = Image.image (
     fun (old_a,(a,b)) -> (old_a,(replacer2 a,replacer2 b))
   ) kfk.images in 
   {
    kfk with 
    atoms = new_atoms ;
    decompositions = new_decompositions ;
    images = new_images ;
  } ;; 

let start =   
  {
    size = 100 ;
    atoms = [1; 2; 3; 4] ;
    decompositions = [] ;
    images = u6 ;
  } ;; 

  let expand kfk l = 
    let replacer = (fun z->
      match List.assoc_opt z kfk.decompositions with 
      Some(old_answer) -> old_answer
      | None -> [z]
    ) in 
    i_fold_merge (Image.image replacer l) ;;


  let get_image kfk l = try snd(List.assoc l kfk.images) with _ -> [] ;;

  let is_fully_decomposed kfk l = i_is_included_in l kfk.atoms ;; 

  let nondecomposed_elements kfk l = i_setminus l kfk.atoms ;; 

let declare_empty kfk zeroes =
  let remaining_atoms = i_setminus kfk.atoms zeroes in 
  let m = List.length remaining_atoms 
  and n = kfk.size in 
  let interval = Int_range.range (n+1) (n+m) in 
  let table = List.combine remaining_atoms interval in 
  let cleanup = (fun x->Image.image (fun t->
      try List.assoc t table with _ ->t) (i_setminus x zeroes)) in   
  let new_decompositions = Image.image (fun (x,dx)->(x,cleanup dx)) kfk.decompositions 
  and new_images = Image.image (
    fun (old_a,(a,b)) -> (old_a,(cleanup a,cleanup b))
  ) kfk.images in 
  {
   kfk with 
   atoms = interval ;
   decompositions = new_decompositions ;
   images = new_images ;
 } ;; 
 

end ;;


module Haddock = struct 

type t = 
   S1 of int * (int list) * (int list) * (int list) * (int list) * (int list) * (int list)  
 | S2 of int * (int list) * (int list) * (int list) * (int list) * (int list) * (int list) 
 | S3 of int * (int list) * (int list) * (int list) * (int list) * (int list) * (int list) 
 | S4 of int * (int list) * (int list) * (int list) * (int list) * (int list) * (int list) * (int list) * (int list)  

;;  

type road =
   Decompose of (int list) * (int list)
  |Declare_empty of (int list) * (int list) ;;

let is_unfinished = function 
 S4(idx,a,b,anb,g_anb,g_a,g_b,d1,d2) -> (d1,d2) <> ([],[])
|_ -> true ;;

let road_to_go  kfk haddock = match haddock with 
  S1(idx,a,b,anb,f_anb,f_a,f_b) ->
       Decompose(
          (Kafka.nondecomposed_elements kfk anb),  
          (Kafka.nondecomposed_elements kfk f_anb)) 
  |S2(idx,a,b,anb,g_anb,f_a,f_b) -> 
    Decompose(Kafka.nondecomposed_elements kfk f_a,[]) 
  |S3(idx,a,b,anb,g_anb,g_a,f_b) -> 
    Decompose(Kafka.nondecomposed_elements kfk f_b,[]) 
  |S4(idx,a,b,anb,g_anb,g_a,g_b,d1,d2) -> Declare_empty(d1,d2) ;;

let improve_a_little_bit  kfk haddock = match haddock with 
  S1(idx,a,b,anb,f_anb,f_a,f_b) ->
       if (Kafka.is_fully_decomposed kfk anb) &&  (Kafka.is_fully_decomposed kfk f_anb) 
       then S2(idx,a,b,anb,i_intersection anb f_anb,f_a,f_b) 
       else haddock
  |S2(idx,a,b,anb,g_anb,f_a,f_b) -> 
    if (Kafka.is_fully_decomposed kfk f_a) 
      then S3(idx,a,b,anb,g_anb,i_intersection anb f_a,f_b) 
      else haddock
  |S3(idx,a,b,anb,g_anb,g_a,f_b) -> 
        if (Kafka.is_fully_decomposed kfk f_b) 
          then let g_b = i_intersection anb f_b in 
               let g_ab = i_merge g_a g_b in 
               S4(idx,a,b,anb,g_anb,g_a,g_b,i_setminus g_anb g_ab,i_setminus g_ab g_anb) 
          else haddock    
  |S4(idx,a,b,anb,g_anb,g_a,g_b,d1,d2) -> haddock ;;

let improve kfk haddock = 
    let rec tempf = (fun old_x ->
       let new_x=improve_a_little_bit  kfk old_x in 
       if new_x = old_x then old_x else tempf new_x
    ) in 
    tempf haddock ;;

let initial_individual kfk (idx,(a,b)) = 
  let c =  i_intersection a b in 
  S1(idx,a,b,Kafka.expand kfk c,Kafka.get_image kfk c,Kafka.get_image kfk a,Kafka.get_image kfk b) ;;
  
let individual kfk (a,b) = 
   let candidate = improve kfk (initial_individual kfk (a,b)) in 
   if is_unfinished candidate 
   then Some (candidate,road_to_go kfk candidate)
   else None ;;

let total kfk = 
   List.find_map (individual kfk) u9 ;;


end ;;  

module This_kafka = struct 

  let main_ref = ref Kafka.start ;;

  let share x = 
     let new_kafka = Kafka.share_with_foreigner (!main_ref) x in 
     let _ = (main_ref:=new_kafka) in 
     new_kafka ;;

  let atoms () = (!main_ref).Kafka.atoms ;;

  let expand  = Kafka.expand (!main_ref) ;;

  let final_haddock () = Haddock.total (!main_ref) ;;

  let declare_empty zeroes = 
    let new_kafka = Kafka.declare_empty (!main_ref) zeroes in 
    let _ = (main_ref:=new_kafka) in 
    new_kafka ;;

  let act = function 
    Haddock.Decompose(d1,d2) -> let d = List.hd (d1@d2) in share d 
    |Declare_empty(e1,e2) -> declare_empty (i_merge e1 e2) ;;

end ;;  

module That_kafka = struct 

let share x = let _ = This_kafka.share x in snd(Option.get(This_kafka.final_haddock ())) ;;
let declare_empty x = let _ = This_kafka.declare_empty x in snd(Option.get(This_kafka.final_haddock ())) ;;

let act road = let _ = This_kafka.act road in  This_kafka.final_haddock () ;;

end ;;  



let rec iterator preceding_moves = match preceding_moves with 
   [] -> failwith("pusher_exn") 
  |(preceding_move,_) :: _->
      (match That_kafka.act preceding_move with 
      None -> List.rev preceding_moves 
    |Some(candidate,road) -> 
      iterator((road,!(This_kafka.main_ref))::preceding_moves) );;
  
let z1 = [(Haddock.Decompose([5],[])),Kafka.start]  ;;
let z2 = iterator z1 ;;   
let (z3,kfk1) = List.hd(List.rev z2) ;;   
let kfk1 = (!(This_kafka.main_ref))
let im = Kafka.get_image kfk1 ;;

let v1 = Uple.list_of_pairs u2 ;;
let v2 = List.filter (
   fun (a,b) ->
     let c = i_merge a b in 
     let d = i_setminus (im c) (i_merge (im a) (im b)) in 
     d<>[]
) v1 ;;



(************************************************************************************************************************
Snippet 68 : First stab at boundary operator combinatorics 
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let il_order = Total_ordering.silex_compare i_order ;;

let i_merge = Ordered.merge i_order ;;
let i_intersection = Ordered.intersect i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;


let il_sort = Ordered.sort il_order ;;


let u1 = il_sort (Listennou.power_set [1;2;3]) ;;
let u2 = Uple.list_of_pairs u1 ;;
let u3 = List.filter (fun (x,y)->i_intersection x y <> []) u2 ;;

let table_for_ff = 
  [[],0; [1],1; [2],2; [3],3; [1; 2],3; [1; 3],2; [2; 3],1; [1; 2; 3],0] ;;
let ff x = List.assoc x table_for_ff  ;;

let u4 = Image.image (fun (a,b)->
  let c= i_intersection a b in 
  (a,b,(c,ff c,ff a, ff b))) u3;;

let tf k = List.nth u4 (k-1) ;;  

let v1 = Cartesian.cube u1 ;;
let haddock k a b c = ((i_intersection k a) = (i_intersection k (i_merge b c)));;
let main_test (x1,x2,x3) =
    (haddock [1] x1 x2 x3) && (haddock [2] x2 x3 x1) && (haddock [3] x3 x1 x2) ;;      
let v2 = List.filter main_test v1 ;;
let v3 = List.filter (fun (x1,x2,x3) -> not(i_is_included_in x3 (i_merge x1 x2))) v2;;



(************************************************************************************************************************
Snippet 67 : Preprocess some PARI/GP code
************************************************************************************************************************)
open Needed_values ;;

let n1 = 5 ;;
let m1 = ((n1-1) * (n1-2)) / 2;;
let u1 = Int_range.scale (fun x->[1;0]) 1 n1 ;;
let u2 = Cartesian.general_product u1 ;;
let u3 = Int_range.index_everything u2 ;;
let ts l= 
   String.concat "+" (Image.image (fun j->"t"^(string_of_int j)) l);;

let s1 i = (ts(List.filter_map (fun (idx,l)->
    if (List.nth l (i-1) = 1) 
    then Some(idx)
    else None    
  ) u3)) ^ "-" ^(string_of_int m1);;
let s2 (i,j) = (ts(List.filter_map (fun (idx,l)->
    if (List.nth l (i-1) = 1) && (List.nth l (j-1) = 1)
    then Some(idx)
    else None    
) u3)) ^ "-" ^(string_of_int (n1-2));;

let part1 = Int_range.scale s1 1 n1 ;;
let part2 = Image.image s2 (Int_uple.list_of_pairs n1);;
let whole = "\n\n\n[" ^ (String.concat "," (part1@part2)) ^ "]\n\n\n"  ;;
let pw () = print_string whole ;;

Ordered.setminus Total_ordering.for_integers (Int_range.range 1 32)
[1; 2; 3; 4; 5; 6; 7; 9; 10; 11; 13; 17; 18; 19; 21; 25; 32] ;;


(************************************************************************************************************************
Snippet 66 : Transform a text in an Ocaml string 
************************************************************************************************************************)
let z1 = Needed_values.rf "Fads/nap.ml"  ;;
let z2 = Lines_in_string.interval z1 12 25 ;;
let z3 = Replace_inside.replace_inside_string ("\"","\\\"") z2;;
let z4 = Lines_in_string.lines z3 ;;
let z5 = Image.image (fun line -> "\"" ^ (Cull_string.trim_spaces line) ^ "\"") z4 ;; 
let z6 = "\n\n\n" ^ (String.concat ";\n" z5) ^ "\n\n\n" ;;
let z7 () = print_string z6 ;;

(************************************************************************************************************************
Snippet 65 : Find and replace on several files 
************************************************************************************************************************)
let reps_ref = ref [ ];;

reps_ref := [
  "Fw_with_githubbing.all_endinglesses","Fw_with_dependencies.all_endinglesses";
  "Fw_with_githubbing.all_ml_absolute_paths","Fw_with_dependencies.all_ml_absolute_paths";
  "Fw_with_githubbing.all_mlx_files","Fw_with_dependencies.all_mlx_files";
  "Fw_with_githubbing.ancestors_for_module","Fw_with_dependencies.ancestors_for_module";
  "Fw_with_githubbing.all_subdirectories","Fw_with_dependencies.all_subdirectories";
  "Fw_with_githubbing.below","Fw_with_dependencies.below";
  "Fw_with_githubbing.check_that_no_change_has_occurred","Fw_with_archives.check_that_no_change_has_occurred";
  "Fw_with_githubbing.configuration","Fw_with_githubbing.to_fw_configuration";
  "Fw_with_githubbing.decipher_module","Fw_with_dependencies.decipher_module";
  "Fw_with_githubbing.decipher_path","Fw_with_dependencies.decipher_path";
  "Fw_with_githubbing.dep_ordered_modules","Fw_with_dependencies.dep_ordered_modules";
  "Fw_with_githubbing.direct_fathers_for_module","Fw_with_dependencies.direct_fathers_for_module";
  "Fw_with_githubbing.directly_below","Fw_with_dependencies.directly_below";
  "Fw_with_githubbing.duplicate_module","Fw_with_dependencies.duplicate_module";
  "Fw_with_githubbing.endingless_at_module","Fw_with_dependencies.endingless_at_module";
  "Fw_with_githubbing.find_subdir_from_suffix","Fw_with_dependencies.find_subdir_from_suffix";
  "Fw_with_githubbing.gitpush_after_backup","Fw_poly.gitpush_after_backup";
  "Fw_with_githubbing.latest_changes","Fw_with_archives.latest_changes";
  "Fw_with_githubbing.modules_using_value","Fw_with_dependencies.modules_using_value";
  "Fw_with_githubbing.noncompilable_files","Fw_with_archives.noncompilable_files";
  "Fw_with_githubbing.of_concrete_object","Fw_poly.of_concrete_object";
  "Fw_with_githubbing.root","Fw_poly.root";
  "Fw_with_githubbing.set_gitpush_after_backup","Fw_poly.set_gitpush_after_backup";
  "Fw_with_githubbing.to_concrete_object","Fw_poly.to_concrete_object";
  "Fw_with_githubbing.usual_compilable_files","Fw_with_archives.usual_compilable_files";
] ;;

let files_ref = ref [] ;;

files_ref := [
  "Compilation_management/create_world_copy.ml";
  "Compilation_management/modify_coma_state.ml";
  "Compilation_management/needed_data_summary.ml";  
  "Compilation_management/usual_coma_state.ml";
  "Compilation_management/other_coma_state.ml";
  "Filewatching/fw_with_persisting.ml";
  "Ocaml_analysis/compute_all_ocaml_items.ml";
  "Ocaml_analysis/read_needed_ocaml_files.ml";
  "self_contained_module_copy.ml";
] ;;

let tr x = Replace_inside.replace_several_inside_file (!reps_ref) (Absolute_path.of_string( "../Idaho/"^x)) ;;

let act () = List.iter tr (!files_ref) ;;

(************************************************************************************************************************
Snippet 64 : Modifying line intervals in a file
************************************************************************************************************************)
let ap1 = Absolute_path.of_string "../Idaho/Filewatching/fw_with_githubbing.ml" ;;
let text1 = Io.read_whole_file ap1 ;;
let (before1,old_center1,after1) = Lines_in_string.tripartition_associated_to_interval 
    text1 257 336 ;;
let new_center1 = Lines_in_string.remove_lines_containing_substring_in_string
  "shrinkable" old_center1 ;;
let new_text1 = String.concat "\n" [before1;new_center1;after1] ;;  
Io.overwrite_with ap1 new_text1 ;;

Lines_in_string.remove_interval_in_file ap1 169 253 ;;

(************************************************************************************************************************
Snippet 63 : Enumerating subgroups of S4 
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let i_sort  = Ordered.sort i_order ;;
let i_is_included_in = Ordered.is_included_in i_order;;

let il_order = Total_ordering.silex_compare  Total_ordering.for_integers ;;
let il_sort  = Ordered.sort il_order ;;

let current_order = 4 ;;
let base = Permutation.iii current_order ;;

let eval_list_permutation sigma k = List.nth sigma (k-1) ;;

let compose_list_permutations sigma1 sigma2 = 
   Int_range.scale (fun k-> eval_list_permutation sigma1 (eval_list_permutation sigma2 k)) 1 current_order ;;

let uncurried_compose = Memoized.make(fun (i,j) ->
   let sigma1 = List.nth base (i-1)   
   and sigma2 = List.nth base (j-1) in 
   Listennou.find_index (compose_list_permutations sigma1 sigma2) base
);;     

let compose  i j = uncurried_compose (i,j) ;;

let base_size = List.length base ;;

let subset_product l1 l2 =
    let temp1 = Cartesian.product l1 l2 in 
    let temp2 = Image.image uncurried_compose temp1 in 
    i_sort temp2 ;; 

let rec helper_for_generated_subgroup (treated,seed) = 
      let possibly_new = subset_product treated seed in 
      let really_new = Ordered.setminus Total_ordering.for_integers possibly_new treated in 
      if really_new = [] 
      then treated 
      else let new_whole = Ordered.merge Total_ordering.for_integers really_new treated in 
            helper_for_generated_subgroup (new_whole,seed) ;;
        
let generated_subgroup seed = helper_for_generated_subgroup ([1],seed) ;; 

let trivial_subgroup = [1] ;;
let full_subgroup = Int_range.range 1 base_size ;;

let level1  = 
  il_sort (Int_range.scale (fun k->generated_subgroup [k]) 2 base_size) ;; 

let pre_level2 = 
    let temp1 = Uple.list_of_pairs level1 in 
    let temp2 = Image.image (fun (a,b)->generated_subgroup(a@b)) temp1 in 
    il_sort temp2 ;;

let (next_to_level2,level2) = List.partition (fun x->List.mem x level1) pre_level2 ;;

let pre_level3 = 
  let temp1 = Cartesian.product level1 level2 in 
  let temp2 = List.filter (fun (x,y)->not(i_is_included_in x y)) temp1 in 
  let temp3 = Image.image (fun (a,b)->generated_subgroup(a@b)) temp2 in 
  il_sort temp3 ;;

let (next_to_level3,level3) = List.partition (fun x->List.mem x level2) pre_level3 ;;  

let halves x =
   let n = (List.length x)/2 in 
   List.filter (fun y->((List.length y)=n)&&(i_is_included_in y x) ) (level1@level2) ;;

let is_transitive sg =
   let temp1 = Image.image (fun sigma -> 
    eval_list_permutation sigma 1
   ) sg in 
   (i_sort temp1) = (Int_range.range 1 current_order) ;;
   

let d4 = List.hd(List.filter (fun x->List.length x=8) level2) ;; 
let halves_for_d4 = halves d4 ;; 

let a4 = List.hd(List.filter (fun x->List.length x=12) level2) ;; 
let halves_for_a4 = halves a4 ;; 

let halves_for_whole = halves full_subgroup ;;

    

(************************************************************************************************************************
Snippet 62 : Finding a polynomial x^4+p*x+q with Galois group A4
************************************************************************************************************************)
let u1 = Int_range.range (-50) 50 ;;
let u2 = Cartesian.square u1 ;;
let u3 = Image.image (fun (x,y)->(max(abs x)(abs y),(x,y)) ) u2 ;;
let u4 = Ordered.sort Total_ordering.standard2 u3 ;;
let unchecked_u5 = Image.image snd u4 ;;
let u5 = List.filter (fun (p,q)->List.for_all (fun z->z*z*z*z+p*z+q<>0) 
(Int_range.range (-1) 1)) unchecked_u5 ;;

let round x=
  let fl = floor x in 
  if (x -. fl) < 0.5 
  then int_of_float fl 
  else (int_of_float fl)+1 ;;  

let is_a_square n = 
    if n< 0 then false else
    let m =round(sqrt(float_of_int n)) in m * m = n;;  

let check = List.filter is_a_square (Int_range.range 0 100) ;;    

let u6 = List.filter (fun (p,q)->is_a_square(-27*p*p*p*p + 256*q*q*q)) u5 ;;

(************************************************************************************************************************
Snippet 61 : Removing indentation in a paragraph in a file  
************************************************************************************************************************)
let ap1 = Absolute_path.of_string "Fads/pan.ml" ;;

let text1 = Io.read_whole_file ap1 ;; 

let (before_text2,text2,after_text2) =
  Lines_in_string.tripartition_associated_to_interval text1 11 60 ;;

let old_lines_in_text2 = Lines_in_string.lines text2 ;;  
let new_lines_in_text2 = Image.image (Cull_string.cobeginning 3) old_lines_in_text2 ;; 

let new_text2 = String.concat "\n" new_lines_in_text2 ;;
let new_text1 = String.concat "\n" [before_text2;new_text2;after_text2] ;;

Io.overwrite_with ap1 new_text1 ;;

(************************************************************************************************************************
Snippet 60 : Intertwining prints for debugging purposes
************************************************************************************************************************)
open Needed_values ;;

let z1 = rf "Fads/pan.ml" ;;
let z2 = Lines_in_string.interval z1 10 58 ;;
let z3 = Lines_in_string.lines z2 ;;
let z4 = List.filter (fun line -> Cull_string.trim_spaces line <> "" ) z3 ;; 
let z5 = Int_range.index_everything z4 ;; 
let z6 = Image.image (
  fun (j,line) ->
    let sj = string_of_int j in 
    line^"\nprint_int "^sj^" ;;"
) z5 ;;
let z7 = "\n\n\n" ^ (String.concat "\n" z6) ^ "\n\n\n" ;;  

(************************************************************************************************************************
Snippet 59 : Problem involving periodicity
************************************************************************************************************************)
let find_periodicity l= 
  let rl = List.rev l in 
  let (a1,after_a1) = Listennou.head_with_tail rl in 
  let j = Listennou.find_index a1 after_a1 in 
  let inverted_motif = Listennou.long_head j rl in 
  let motif = List.rev inverted_motif in 
  let p = List.length motif in 
  let m0 = Min.list motif in 
  let i0 = Listennou.find_index m0 motif in 
  let after_m0 = Listennou.long_tail i0 motif 
  and before_m0 = Listennou.long_head (i0-1) motif in
  (p,m0::(after_m0@before_m0)) ;; 


let current_r = 5 ;;
let current_m = Gcd.lcm_for_many (Int_range.range 2 current_r) ;;

let pusher old_f n = 
  let lower_bound = max 1 (n-current_r) in  
  let temp1 = Int_range.range lower_bound (n-1) in 
  let temp2 = Image.image (fun m->(old_f m)-(current_m/(n-m))) temp1 in 
  let first_trial = Min.list temp2 in 
  if first_trial > 0 then first_trial else 
  let temp3 = Image.image (fun m->(old_f m)+(current_m/(n-m))) temp1 in   
  Max.list temp3 ;;

let ff = Memoized.recursive (fun old_f n->if n<2 then 1 else pusher old_f n) ;;

let z1 = Int_range.scale ff 1 200 ;;
let (period,motif) = find_periodicity z1 ;;
let last_in_motif = List.nth motif (period-1) ;;
let gg n = let r = n mod period in if r = 0 then last_in_motif else List.nth motif (r-1) ;;
let dg t = Min.list (Int_range.scale (fun k->(abs(gg(k+t)-gg(k)))*t ) 1 period) ;;
let (max_dg,dg_sols) = Min.minimize_it_with_care dg (Int_range.range 1 current_r) ;;
let largest_in_motif = Max.list motif ;;
let ratio = (float_of_int(largest_in_motif-List.hd(motif))) /. (float_of_int max_dg);;


(************************************************************************************************************************
Snippet 58 : Musings on the Szemeredi problem, chapter V
************************************************************************************************************************)
let current_width = 3 ;; 
let max_width = Sz_max_width_t.MW current_width ;;
let is_admissible = Sz_preliminaries.test_for_admissibility max_width ;;
let is_not_admissible x= (not(is_admissible x));;
let uncurried_sl  = Memoized.make (fun (x,k)->
  let temp1 = Sz_preliminaries.restricted_power_set (max_width,x) in 
  List.filter (fun z->List.length z=k) temp1 
) ;;  
let sl x k = uncurried_sl (x,k) ;;
let isl n k = uncurried_sl (Int_range.range 1 n,k) ;; 
let meas = Sz_precomputed.measure max_width ;;


let i_does_not_intersect = Ordered.does_not_intersect Total_ordering.for_integers ;;
let i_is_included_in = Ordered.is_included_in Total_ordering.for_integers ;;
let i_merge = Ordered.merge Total_ordering.for_integers ;;
let i_outsert = Ordered.outsert Total_ordering.for_integers ;;
let i_fold_intersect = Ordered.fold_intersect Total_ordering.for_integers ;;
let il_fold_merge = Ordered.fold_merge Total_ordering.silex_for_intlists ;;
let il_mem = Ordered.mem Total_ordering.silex_for_intlists ;;
let il_merge = Ordered.merge Total_ordering.silex_for_intlists ;;
let il_sort = Ordered.safe_set Total_ordering.silex_for_intlists ;;



let original_minimal_carriers carriers sols =
  let indexed_carriers = Int_range.index_everything carriers in 
  let shadow = (
      fun sol ->
         List.filter_map (
          fun (idx,carrier) -> 
             if i_is_included_in carrier sol 
             then Some idx 
            else None 
        ) indexed_carriers 
  )  in     
  let all_shadows = Image.image shadow sols in 
  Ordered_misc.minimal_transversals all_shadows ;;
 
let u_product sheaf1 sheaf2 = 
    let temp1 = Cartesian.product sheaf1 sheaf2 in 
    let temp2 = Image.image (fun (x,y)->i_merge x y) temp1 in 
    let temp3 = Ordered_misc.minimal_elts_wrt_inclusion temp2 in 
    il_sort temp3 ;;

 
let fold_u_product = function 
  [] -> []
  | sheaf :: other_sheaves -> List.fold_left  u_product sheaf other_sheaves ;;   
   

exception Nonunique_set_of_minimal_carriers of int list list list ;;
 
let set_of_minimal_carriers carriers sols =
 let version1 = original_minimal_carriers carriers sols in 
 let m = List.length(List.hd version1) in 
 let version2 = List.filter (fun x->List.length(x)=m) version1 in 
 let visualize = Image.image (fun idx->List.nth carriers (idx-1)) in 
 let version3 = Image.image visualize version2 in 
 if (List.length version3)<>1
 then raise (Nonunique_set_of_minimal_carriers version3)
 else List.hd version3;;
 
let set_of_minimal_carriers_with_extra carriers sols =
 try (Some(set_of_minimal_carriers carriers sols),None) with 
 Nonunique_set_of_minimal_carriers(nonunique) -> (None, Some nonunique)   ;;

let remains_of_obstructions_in_positing_case x=
  List.filter_map (fun j->
      let k=(current_width+1)-j in 
      if x>2*k 
      then  Some [x-2*k;x-k]
      else None
  ) (Int_range.range 1 current_width) ;;

  

let analize_sheaf1 (left,bound,right) =
    let m = List.hd(List.rev right) in 
    let carriers = Sz_preliminaries.force_subset_in_interval    
      max_width right (1,m) in 
      set_of_minimal_carriers_with_extra carriers (sl left bound);;

let ref_for_missing_sheaves = ref [] ;;

exception Troublesome_aftersheaf of int list * int * int list * (int list list list);;
exception Missing_sheaves of (int list * int *  (int list list)) list ;;
  
let analize_sheaf2 (left,bound,right) =
  let (good_opt,bad_opt) = analize_sheaf1(left,bound,right) in 
  (
    match good_opt with 
     None -> raise(Troublesome_aftersheaf(left,bound,right,Option.get bad_opt)) 
    |Some usual -> 
      let _ = (ref_for_missing_sheaves:=[left,bound,usual]) in
      raise(Missing_sheaves [left,bound,usual])
  ) ;;    

let carrier_is_stronger_than_another carrier1 carrier2 =
   List.for_all (fun c1 -> 
    List.exists (fun c2 -> i_is_included_in c2 c1) carrier2) carrier1 ;;

exception Two_carriers_exn of 
  (int list) * int * (int list list) * (int list list) * (int list list list);;

let add_carrier_to_another (x,bound) carrier old_carrier =
   if carrier_is_stronger_than_another carrier old_carrier 
   then [carrier]
   else 
   let carrier2 = u_product carrier old_carrier in 
   let (good_opt,bad_opt) = set_of_minimal_carriers_with_extra carrier2 (sl x bound) in 
   if good_opt<>None
   then [Option.get good_opt]
   else raise (Two_carriers_exn(x,bound,carrier,old_carrier,Option.get bad_opt));; 

exception Add_carrier_exn of (int list) * int * (int list list) * (int list list list) ;;

let add_carrier (x,bound) carrier old_list = 
   match List.length old_list with 
    0 -> [carrier] 
   | 1 -> add_carrier_to_another (x,bound) carrier (List.hd old_list) 
   | _ -> raise (Add_carrier_exn(x,bound,carrier,old_list));;


let hashtbl_for_sheaves = ((Hashtbl.create 100): 
   ((int list) * int, int list list list) Hashtbl.t) ;;


let add_sheaf (x,bound) carrier =
  let new_val = (
   match Hashtbl.find_opt hashtbl_for_sheaves (x,bound)  with 
    Some (old_val) -> add_carrier (x,bound) carrier  old_val
   | None -> [carrier]
   ) in 
   Hashtbl.replace hashtbl_for_sheaves (x,bound) new_val ;;
   
let consult_sheaves (left,bound,right) =
  match Hashtbl.find_opt hashtbl_for_sheaves (left,bound)  with 
     None -> None
   | Some (sheaves) -> 
     List.find_opt (fun sheaf->
       List.for_all (fun z->is_not_admissible (z@right)) sheaf
      ) sheaves
  ;;        

let sheaf_compare new_ll old_ll  =
    List.for_all (fun old_l-> List.exists 
    (fun new_l->i_is_included_in new_l old_l) new_ll ) old_ll  ;;

let sheaf_compare2 new_ll old_lls =
    List.exists (sheaf_compare new_ll) old_lls ;;
       
let sheaf_is_not_already_known_by_sheaves_hashtbl (left,bound,right) = 
  match Hashtbl.find_opt hashtbl_for_sheaves (left,bound)  with 
  None -> true
| Some (old_sheaves)  -> not(sheaf_compare2 right old_sheaves)  ;;       

let hashtbl_for_pre_measure = ((Hashtbl.create 100) :
(int list ,int list) Hashtbl.t) ;;

let sheaf_is_not_already_known_by_measures_hashtbl (left,bound,right) = 
  match Hashtbl.find_opt hashtbl_for_pre_measure left  with 
  None -> if is_admissible left 
          then (List.length left) >= bound
          else true
| Some (old_sol)  -> (List.length old_sol) >= bound  ;;   

let sheaf_is_not_already_known triple = 
   if sheaf_is_not_already_known_by_measures_hashtbl triple 
   then  sheaf_is_not_already_known_by_sheaves_hashtbl triple 
   else false ;; 

exception Borderline_case of ((int list) * int * ((int list) list)) list ;;

let commonest_decomposition (x,bound,carriers) =
    let (m,ry) = Listennou.head_with_tail(List.rev x) in 
    let y = List.rev ry in 
   let rem_obstr1 = remains_of_obstructions_in_positing_case m 
   and (pre_rem_obstr3,rem_obstr2) = List.partition (fun z->List.mem m z) carriers in    
   let rem_obstr3 = Image.image (i_outsert m) pre_rem_obstr3 in       
   let temp = (if List.mem [m] carriers then [] else 
    [
     (y,bound-1,il_fold_merge [rem_obstr1;rem_obstr2;rem_obstr3]);
    ])
   @ 
    [(y,bound,rem_obstr2)] in 
  let (cleaned_temp,dirty_temp) = List.partition sheaf_is_not_already_known_by_measures_hashtbl temp in    
  let (temp_good,temp_bad) = List.partition (fun (_,_,obstr2)->obstr2<>[]) cleaned_temp in   
  if temp_bad <> [] 
  then raise(Borderline_case(temp_bad)) 
  else    
  let temp3 = Image.image (
    fun tr3 ->
       let (y3,bound3,obstr3) = tr3 in 
       (tr3,set_of_minimal_carriers_with_extra obstr3 (sl y3 bound3))
  ) temp_good in 
  let (temp3_good,temp3_bad) = List.partition (fun 
    (_,(good_opt,bad_opt)) -> good_opt <> None
  ) temp3 in 
  (
    Image.image (fun ((y,bound,_),(good_opt,bad_opt))->(y,bound,Option.get good_opt) ) temp3_good,
    Image.image (fun (tr,(good_opt,bad_opt))->tr ) temp3_bad,
    dirty_temp
  );;   



let enhanced_commonest_decomposition triple =
  let (temp1,temp2,temp3) = commonest_decomposition triple in 
  (List.filter sheaf_is_not_already_known temp1, 
   List.filter sheaf_is_not_already_known temp2,
   temp3);;   

exception Add_sheaf_carefully_exn of (int list * int * int list list) list ;;

let add_sheaf_carefully (x,bound) carrier =
  let (temp1,temp2,_) = enhanced_commonest_decomposition (x,bound,carrier) in 
  let temp3 = temp1 @ temp2 in 
  if temp3 <> []
  then raise(Add_sheaf_carefully_exn(temp3))     
  else add_sheaf (x,bound) carrier ;;

let consult_sheaves_and_double_check (left,bound,right) =
  match consult_sheaves (left,bound,right) with 
    Some obstr -> Some obstr 
   | None ->
     let sols = sl left bound in 
     if List.for_all (fun z->is_not_admissible (z@right)) sols
     then analize_sheaf2(left,bound,right) 
     else None ;;
     


let analize_missing_sheaves ()=
  let triple = List.hd(!ref_for_missing_sheaves) in 
  let (temp1,temp2,temp3) = enhanced_commonest_decomposition triple in 
  let _= (ref_for_missing_sheaves:=temp1@temp2) in 
  (temp1,temp2,temp3);;

let default_string_of_intlist l=  
  "["^(String.concat ";" 
  (Image.image string_of_int l)
  )^"]";;  

  let ennified_string_of_intlist l=
    let a = List.hd(l) and b = List.hd(List.rev l) in 
    if (l=Int_range.range a b)
    then  "Ennig.ennig "^(string_of_int a)^" "^(string_of_int b)    
  else default_string_of_intlist l;;

let string_of_intlistlist ll=  
"["^(String.concat ";" 
(Image.image default_string_of_intlist ll)
)^"]";;

let message_for_newcomer (x,bound,carriers) =
 "asc ("^(ennified_string_of_intlist x)^","^(string_of_int bound)^") "^
  (string_of_intlistlist carriers)^" ;;" 
 ;;

let message_for_newcomers l= 
  "\n\n\n"^(String.concat "\n" 
  (Image.image message_for_newcomer l)
  )^"\n\n\n";;

let analize_repeatedly initial_triple =
   let triple1 = enhanced_commonest_decomposition initial_triple in  
   let (bt1,bt2,_) = triple1 in 
   if (bt1,bt2) = ([],[]) 
   then let msg = "\n\n\n"^(message_for_newcomer initial_triple)^"\n\n\n" in 
        let _= (print_string msg;flush stdout) in 
        (initial_triple,triple1)
   else  
    let rec tempf = (fun (t1,t2,t3)->
      let (nt1,nt2,nt3) = enhanced_commonest_decomposition (List.hd(t1@t2)) in 
      if (nt1,nt2) = ([],[])
      then let msg = message_for_newcomers (t1@t2) in 
           let _= (print_string msg;flush stdout) in 
           (initial_triple,(t1,t2,t3))  
      else tempf(nt1,nt2,nt3)
    ) in 
    tempf triple1 ;;  
    

let hashtbl_for_solving = ((Hashtbl.create 100) :
(int list * int * int list, int list) Hashtbl.t) ;;



let solve_in_easy_case (left,bound,right) = 
  let p = List.length left in 
  if p < bound 
  then Some None 
  else 
  if p = bound 
  then let unique = left @ right in 
       if is_admissible unique 
       then Some (Some unique)
       else Some None  
  else None ;;

let quick_way_out1 (left,bound,right) =
    if bound<>List.length(left) then false else 
    not(is_admissible(left@right)) ;;  

let pre_measure_arg whole              = (false,whole,0,[]) ;;
let solve_arg   (left,bound,right) = (true,left,bound,right) ;;
let pre_measure_ret m = (m,None) ;;
let solve_ret opt= ([],opt) ;;


let induction_in_solve_case old_f triple = 
  match solve_in_easy_case triple with 
   Some easy_answer -> solve_ret easy_answer 
   | None ->
  (
   match Hashtbl.find_opt hashtbl_for_solving triple with 
   Some old_answer -> solve_ret (Some old_answer) 
   | None ->
    let old_solve = (fun tr -> snd (old_f(solve_arg(tr)))) in 
    let opt_sol = (
      let (left,bound,right) = triple in  
      let (m,ry) = Listennou.head_with_tail(List.rev left) in 
      let y = List.rev ry in 
      if is_not_admissible (m::right)
      then old_solve(y,bound,right)
      else     
      let temp1 = [(y,bound-1,m::right);(y,bound,right)] in 
      let my = List.length(fst (old_f (pre_measure_arg(y)))) in 
      let temp2 = List.filter (fun (y2,bound2,_)->bound2<=my) temp1 in 
      let temp3 = List.filter (fun tr->not(quick_way_out1 tr)) temp2 in 
      let temp4 = List.filter (
          fun tr -> (consult_sheaves_and_double_check tr) = None
      )  temp3 in  
      let temp5 = List.filter_map old_solve temp4 in 
      if temp5 = []
      then None  
      else Some(List.hd(List.rev temp5))
    ) in 
    let _ = (if opt_sol <>None 
      then Hashtbl.add hashtbl_for_solving triple (Option.get opt_sol)) in 
    solve_ret opt_sol
  );;

let induction_in_pre_measure_case old_f whole = 
   if is_admissible whole 
   then pre_measure_ret(whole) 
   else 
   match Hashtbl.find_opt hashtbl_for_pre_measure whole with 
    Some old_answer -> pre_measure_ret(old_answer) 
   |None -> 
   let new_answer = (
    let (m,ry) = Listennou.head_with_tail(List.rev whole) in 
   let y = List.rev ry in  
   let sy = fst(old_f(false,y,0,[])) in 
   if is_admissible(sy@[m])
   then  pre_measure_ret(sy@[m])
   else   
   (
     match snd(old_f(true,y,List.length sy,[m])) with 
     None -> pre_measure_ret(sy)
     |Some fitting_one -> pre_measure_ret(fitting_one@[m])
   )) in 
  let _ = Hashtbl.add hashtbl_for_pre_measure whole (fst new_answer) in 
  new_answer;;
   

let rec main_iterator (case,left,bound,right) =
  if case 
  then induction_in_solve_case main_iterator (left,bound,right) 
  else induction_in_pre_measure_case main_iterator left ;;  


let pre_measure whole            = fst(main_iterator(pre_measure_arg(whole))) ;; 
let solve (left,bound,right) = snd(main_iterator(solve_arg(left,bound,right))) ;;  

let clear_hashtables () = (
    Hashtbl.clear hashtbl_for_sheaves  ;
    Hashtbl.clear hashtbl_for_solving  ;
    Hashtbl.clear hashtbl_for_pre_measure  ;
) ;;

exception FF_exn of (( int list * int * int list list) *
((int list * int * int list list) list *
(int list * int * int list list) list *
(int list * int * int list list) list)) ;;

let ff n = 
  try pre_measure (Int_range.range 1 n) with 
  Missing_sheaves(l) ->
    let (t1,t2,t3) = List.hd l in 
    let (a,b) = analize_repeatedly(t1,t2,t3) in 
    raise(FF_exn(a,b)) ;;


let ams = analize_missing_sheaves;;
let asc = add_sheaf_carefully ;;

let comp1 = Int_range.scale ff 1 5 ;;

asc (Int_range.range 1 4,3) [[4]] ;; 
ff 6;;
asc (Int_range.range 1 1,1) [[1]] ;; 
asc (Int_range.range 1 2,1) [[1];[2]] ;; 
asc (Int_range.range 1 2,2) [[1]] ;; 
asc (Int_range.range 1 3,2) [[1];[2;3]] ;;
asc (Int_range.range 1 4,3) [[1;4]] ;; 
asc (Int_range.range 1 5,3) [[5];[1;4]] ;;
asc (Int_range.range 1 5,4) [[1;4]] ;;
ff 7;;
asc (Int_range.range 1 2,2) [[2]] ;; 
asc (Int_range.range 1 3,1) [[1];[2];[3]] ;;
asc (Int_range.range 1 3,2) [[2];[1;3]] ;;
asc (Int_range.range 1 4,2) [[2];[1;3];[1;4];[3;4]] ;;
asc (Int_range.range 1 4,3) [[2];[1;3];[3;4]] ;;
asc (Int_range.range 1 5,3) [[1;4];[2;5]] ;;
asc (Int_range.range 1 5,4) [[2;5]] ;;
asc (Int_range.range 1 6,3) [[6];[1;4];[2;5]] ;;
asc (Int_range.range 1 6,4) [[2;5];[4;6]] ;;
Int_range.scale ff 8 10;;
asc (Int_range.range 1 9,5) [[9]] ;; 
Int_range.scale ff 11 13;;
asc (Int_range.range 1 12,7) [[12]] ;; 
ff 14;;
asc (Int_range.range 1 10,5) [[9];[10]] ;;
asc (Int_range.range 1 10,6) [[9]] ;;
asc (Int_range.range 1 11,6) [[9];[10;11]] ;;
asc (Int_range.range 1 12,7) [[9;12]] ;;
asc (Int_range.range 1 13,7) [[13];[9;12]] ;;
asc (Int_range.range 1 13,8) [[9;12]] ;;
ff 15;;
asc (Int_range.range 1 10,6) [[10]] ;;
asc (Int_range.range 1 11,5) [[9];[10];[11]] ;;
asc (Int_range.range 1 11,6) [[10];[9;11]] ;;
asc (Int_range.range 1 12,6) [[10];[9;11];[9;12];[11;12]] ;;
asc (Int_range.range 1 12,7) [[10];[7;10];[9;11];[11;12]] ;;
asc (Int_range.range 1 13,7) [[9;12];[10;13]] ;;
asc (Int_range.range 1 13,8) [[10;13]] ;;
asc (Int_range.range 1 14,7) [[14];[9;12];[10;13]] ;;
asc (Int_range.range 1 14,8) [[10;13];[12;14]] ;;
Int_range.scale ff 16 18;;
asc (Int_range.range 1 17,9) [[17]] ;;
Int_range.scale ff 19 21;;
asc (Int_range.range 1 20,11) [[20]] ;;

(*


let l =  [[1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13], 7, [[13]; [9; 12]]] ;;
let (bt1,bt2,bt3) = List.hd l ;;
let (a,b) = analize_repeatedly(bt1,bt2,bt3) ;;
let triple0 = enhanced_commonest_decomposition (bt1,bt2,bt3) ;;    

let analize_repeatedly (x,bound,carriers) =
   let triple0 = enhanced_commonest_decomposition (x,bound,carriers) in  
    let rec tempf = (fun (t1,t2,t3)->
      let (nt1,nt2,nt3) = enhanced_commonest_decomposition (List.hd(t1@t2)) in 
      if (nt1,nt2) = ([],[])
      then let msg = message_for_newcomers (t1@t2) in 
           let _= (print_string msg;flush stdout) in 
           (triple0,(t1,t2,t3))  
      else tempf(nt1,nt2,nt3)
    ) in 
    tempf triple0 ;;  


asc (Ennig.ennig 1 10,5) [[9];[10]] ;; 
asc (Ennig.ennig 1 5,4) [[2;5]] ;;

let g1 = Hashtbl.find hashtbl_for_sheaves (Ennig.ennig 1 4,3) ;;
asc (Ennig.ennig 1 4, 3)  [[2]; [1; 3]; [3; 4]] ;;

*)

(************************************************************************************************************************
Snippet 57 : Musings on the 1234 problem
************************************************************************************************************************)
open Needed_values ;;

type sensitive_t = {
   unsorted : (int * (int * int) list) list;
   sorted : int list ;
}  ;;  

type increase_t = 
    Inertia of int 
   |Forced of int * ((int*int) list)
   |Redundancy of int * ((int*int) list) 
   |Pass of int * (int list);;


let oi = Total_ordering.for_integers ;;
let oi2 = Total_ordering.product oi oi ;; 

let reorder pairs =
   let temp1 = Listennou.partition_according_to_fst pairs in 
   let temp2 = Image.image (fun (x,ll)->
       (x,Ordered.safe_set oi2 (List.flatten ll))
      ) temp1 in 
   let temp3 = Ordered.sort Total_ordering.for_integers (Image.image fst temp2) in 
   Image.image (fun x->(x,List.assoc x temp2)) temp3 ;;

exception Sore_wound of int list * int * sensitive_t;;

let special_obstructions =
   [
     [2;3;6;8;9]; (* because of [3; 6; 9; 12=2*6] *)
   ]
   @
   (Image.image (fun j->
      [2;j;2*j;2*j+2;3*j] (* because of [j; 2*j; 3*j; 4*j=2*2*j] *)
      
      ) [ 7;11;15]) ;;

let find_initial_obstruction_opt sorted_l =
     let a =List.hd sorted_l and b = List.hd(List.rev sorted_l) in 
     match List.find_opt (fun j->Ordered.is_included_in oi [j;2*j;3*j;4*j] sorted_l) 
        (Int_range.range a (b/4)) with 
     None -> List.find_opt (fun obstr-> Ordered.is_included_in oi obstr sorted_l) special_obstructions
     |Some(j) -> Some [j;2*j;3*j;4*j];;   



module Sensitive = struct 
 
   let check_before_adding x stv = 
      let new_sorted = Ordered.insert oi x stv.sorted in   
      match  find_initial_obstruction_opt new_sorted with 
      None -> (None,Some new_sorted) 
      |Some obstr -> (Some obstr,None) ;;

   let add (x,data_for_x) stv = 
     let (opt_bad,opt_good) = check_before_adding x stv in 
     match opt_bad with 
     Some obstr ->  raise( Sore_wound(obstr,x,stv))
      |None ->
     let new_sorted = Option.get opt_good in   
   {
      unsorted = (x,data_for_x) :: stv.unsorted;
      sorted = new_sorted ;
   }   ;;

   let coming_from_last_element stv last_elt =
       let temp1 = List.filter_map (fun (x,_)-> 
         let y= last_elt -x in 
         if (x>1)&&(x<=y)&&(List.exists (fun (z,_)->z=y) stv.unsorted) 
         then Some(x*y,[x,y])
         else None   
         ) stv.unsorted in 
         reorder temp1 ;; 
    
   let default_increase stv forbidden_indices =
         let part = Ordered.merge oi stv.sorted (Image.image fst forbidden_indices) in 
         let max_val = (if part=[] then 1 else 1+(List.hd(List.rev part))) in 
         let whole = Int_range.range 1 max_val in  
         let possibilities = Ordered.setminus oi whole part in 
         List.hd possibilities ;;   
   
   let empty_one = {unsorted =[] ; sorted =[]} ;;      

   let has_index idx stv= Ordered.mem oi idx stv.sorted ;;

   let incorporate_redundancy (idx,new_decompositions) stv = 
      let new_unsorted = (Image.image (
                  fun old_pair ->
                     let (z,ll) = old_pair in 
                     if z = idx 
                     then (z,ll@new_decompositions)
                     else old_pair   
               ) stv.unsorted ) in 
      {stv with unsorted = new_unsorted ;};;


end ;;   


module Increase = struct 

   type t = increase_t ;;

   

   let string_of_int_pair (x,y)= "("^(string_of_int x)^","^(string_of_int y)^")";;
   let string_of_int_list l =
      "[" ^ (String.concat ";" (Image.image string_of_int l)) ^ "]" ;;
   let string_of_ipair_list l =
      "[" ^ (String.concat ";" (Image.image string_of_int_pair l)) ^ "]" ;;
   
   let message = function 
      Inertia (new_val) -> (string_of_int new_val)^" added by inertia"
      |Forced(new_val,l) -> (string_of_int new_val)^" forced by "^(string_of_ipair_list l)
      |Redundancy(new_val,l) -> "Redundancy : "^(string_of_int new_val)^", with "^(string_of_ipair_list l) 
      |Pass(new_val,l) -> (string_of_int new_val)^" refused because of "^(string_of_int_list l);;
   
   let next (treated,forbidden_indices,to_be_treated)=
      match to_be_treated with 
      [] -> let new_val = Sensitive.default_increase treated forbidden_indices in 
            let (opt_bad,opt_good) = Sensitive.check_before_adding new_val treated in 
            (match opt_bad with 
            Some obstr -> (Pass(new_val,obstr),(treated,forbidden_indices@[new_val,obstr],[])) 
            |None ->   
             (Inertia(new_val),(Sensitive.add (new_val,[]) treated,forbidden_indices,
                     Sensitive.coming_from_last_element treated new_val))) 
      |(new_x,ll) :: others ->
          if Sensitive.has_index new_x treated 
          then let treated2 = Sensitive.incorporate_redundancy (new_x,ll)  treated in 
               (Redundancy(new_x,ll),(treated2,forbidden_indices,others))
          else let addenda = Sensitive.coming_from_last_element treated new_x in 
               (Forced(new_x,ll),(Sensitive.add (new_x,ll) treated,
               forbidden_indices,reorder(others@addenda)));;            

end ;;   

   

let walker = ref (Sensitive.empty_one,[],[]) ;; 

let push () =
    let (incr,next_state) = Increase.next (!walker) in 
    let msg = (Increase.message incr)^"\n" in 
    let _ = (print_string msg;flush stdout) in 
    let _ = (walker:=next_state) in 
    next_state ;;

for j= 1 to 1000 do let _ = push () in () done ;;

let (a,b,c) = (!walker) ;;

let d = List.rev (List.filter_map (fun (x,l)->
    if l=[] then Some x else None) a.unsorted);;


(*
walker:=([],[]) ;;
for j= 1 to 10 do let _ = push () in () done ;;
let z1 = fst (!walker) ;;
let z2 = Option.filter_and_unpack 
(fun (x,ll)->if ll=[] then Some x else None) z1;;
let z3 = Ordered.safe_set oi z2 ;;
let z4 = Basic.delta_list z3 ;;
let meas n = let q = (n/3) and r=(n mod 3) in 2*q+r ;;
let z5 = Image.image meas z3 ;;
let z6 = Basic.delta_list z5 ;;

walker:=([],[]) ;;
*)


(************************************************************************************************************************
Snippet 56 : Removes unnecessary blanks at the beginning of lines in an interval
************************************************************************************************************************)
open Needed_values ;;


let ap = Absolute_path.of_string "Filewatching/fw_with_githubbing.ml";;
let old_text = Io.read_whole_file ap ;;

let part1= Lines_in_string.interval old_text 1 171 ;;
let part2= Lines_in_string.interval old_text 172 254;;

let lines1 = Lines_in_string.lines part2 ;;
let lines2 = Image.image (Cull_string.cobeginning 5) lines1 ;;
let new_part2 = String.concat "\n" lines2 ;;
let new_text = part1 ^ "\n" ^ new_part2 ;;

Io.overwrite_with ap new_text ;;

(************************************************************************************************************************
Snippet 55 : Find all modules whose ml file contains a certain substring
************************************************************************************************************************)
open Needed_values ;;

let z1 = Fw_with_dependencies.all_moduled_mlx_files (!ucs) ;;
let z2 = List.filter (fun mlx -> (Dfn_full.to_ending mlx)= Dfa_ending.ml ) z1 ;;
let z3 = Explicit.filter (
   fun mlx -> 
    let ap = Dfn_full.to_absolute_path mlx in 
    let text = Io.read_whole_file ap in 
    Substring.is_a_substring_of "Automatic" text
) z2 ;;

(************************************************************************************************************************
Snippet 54 : 
************************************************************************************************************************)
open Needed_values ;;
let mixer (a,b,ll)= Image.image (fun l->a@(Image.image (fun t->t+b) l)) ll;;

let upwards_hat (a,n,b) =  
  let q1 = (n-a)/2 in 
  let central_move = (if (n-a) mod 2 = 0 then -1 else 1) in 
  let new_beginning = (a+2*q1)+central_move in 
  let q2 = (new_beginning-b)/2 in 
  (Int_range.scale (fun t->a+2*t) 0 q1)@(Int_range.scale (fun t->new_beginning-2*t) 0 q2) ;;

let downwards_hat (a,n,b) =  
  let q1 = (a-n)/2 in 
  let central_move = (if (a-n) mod 2 = 0 then 1 else -1) in 
  let new_beginning = (a-2*q1)+central_move in 
  let q2 = (b-new_beginning)/2 in 
  (Int_range.scale (fun t->a-2*t) 0 q1)@(Int_range.scale (fun t->new_beginning+2*t) 0 q2) ;;

exception Hat_definition_exn of int * int * int ;;

let hat (a,n,b) =
  if (((b-a) mod 2)=0)
    ||((n<a)&&(n>b))||((n>a)&&(n<b))
  then raise(Hat_definition_exn(a,n,b)) else  
  if a<n 
  then upwards_hat (a,n,b)
  else downwards_hat (a,n,b) ;;  

   
let eu_12  old_f n = mixer([1],1,old_f(n-1,1)) ;;
let eu_132 old_f n = if n=3 then [[1;3;2]] else mixer([1;3;2],3,old_f(n-3,1)) ;;
let eu_134 old_f n = if n=4 then [hat(1,4,2)] else [] ;;
let eu_135 old_f n = [hat(1,n,2)] ;;

let eu_21 old_f n = 
  if n=2
  then [[2;1]]
  else mixer([2;1],2,old_f(n-2,1));;

let eu_case1 i1 old_f n = mixer(hat(i1,1,i1-1),i1,old_f(n-i1,1)) ;;
let eu_case4 i1 old_f n = mixer(hat(i1,n,i1+1),0,old_f(i1-1,i1-1)) ;;  

let main_base n =
    (
      [
          [1;2],(fun old_f n_again ->mixer([1],1,old_f(n-1,1))) ;
          [1;3;2],eu_132 ;
          [1;3;4],eu_134 ; 
          [1;3;5],eu_135 ; 
          [2;1],eu_21;
          [2;3],(fun old_f n_again ->if n=3 then [[2;3;1]] else []); 
          [2;4],eu_case4 2; 
        ]  
    )  
    @
    (List.flatten(
      Int_range.scale (fun x->
        List.filter (fun (l,f)->(List.for_all(fun j->j>0)l)&&(List.hd(List.rev l)<= n)) [
          [x;x-2],eu_case1 x;
          [x;x+2],eu_case4 x; 
        ]
        ) 3 (n-2)
    ))
   @(
     [
      [n-1;n-3],eu_case1 (n-1);
      [n-1;n],(fun old_f n_again ->mixer([n-1;n],0,old_f(n-2,n-2)) ); 
      [n;n-2],(fun old_f n_again ->mixer([n],0,old_f(n-1,n-2)));
      [n;n-1],(fun old_f n_again ->mixer([n],0,old_f(n-1,n-1)));
     ]
   ) ;;

let small_values = [
   (1,1),[[1]];
   (2,1),[[1;2]];
   (2,2),[[2;1]];
   (3,1),[[1;2;3];[1;3;2]];
   (3,2),[[2;1;3];[2;3;1]];
   (3,3),[[3;1;2];[3;2;1]];
   (4,1),[[1;2;3;4];[1;2;4;3];[1;3;2;4];[1;3;4;2]];
   (4,2),[[2;1;3;4];[2;4;3;1]];
   (4,3),[[3;1;2;4];[3;4;2;1]];
   (4,4),[[4;2;1;3];[4;2;3;1];[4;3;1;2];[4;3;2;1]];
]   ;;

exception Main_parameter_exn of int * int ;;

let main = Memoized.recursive (fun old_f (n,i1)->
   match List.assoc_opt (n,i1) small_values with 
   Some(easy_answer) -> easy_answer 
   | None ->
     if (n<5)||(i1<1)||(i1>n) then raise(Main_parameter_exn(n,i1)) else 
     let temp1 = main_base n in 
     let temp2 = Image.image (fun (prefix,f)-> 
        if List.hd(prefix)=i1 
        then f old_f n
        else []  
     ) temp1  in 
     List.flatten temp2 
) ;;

let goal = List.flatten(Int_range.scale (fun m->(Int_range.scale (fun j->(m,j)) 1 m)) 1 25);;
exception Haddock of int * int ;;
let computation = Image.image (fun (n,i)-> try main(n,i) with _->raise(Haddock(n,i))) goal ;;

let whole = Memoized.make (fun n->
    List.flatten (Int_range.scale (fun j->main(n,j)) 1 n)
) ;;

let sizes = 
  let _ = whole 15 in 
  Int_range.scale (fun n->List.length(whole n)) 1 25;;
let check_sizes = (sizes = [1; 2; 6; 12; 20; 34; 56; 88; 136; 208; 314; 470; 700; 1038; 1534; 2262;
3330; 4896; 7192; 10558; 15492; 22724; 33324; 48860; 71630]) ;;

(************************************************************************************************************************
Snippet 53 : Musing on permutations satisfying |i-j|<1 -> |p(i)-p(j)|<=2 
************************************************************************************************************************)
open Needed_values ;;
let ointlist = Total_ordering.silex_compare Total_ordering.for_integers ;;

let extensions1 n l = match l with 
    [] -> Int_range.range 1 n 
   | a :: others ->
      List.filter (fun x->(x>0)&&(x<=n)&&(not(List.mem x l))) [a-2;a-1;a+1;a+2] ;;

let extensions2 n ll =
  List.flatten (Image.image (fun l->
    let temp1 = extensions1 n l in 
    Image.image (fun a-> a::l) temp1
    ) ll) ;;

let main = Memoized.make(fun n->
    let rec tempf = (fun j->
      if j=0 then [[]] else 
      extensions2 n (tempf (j-1))  
    ) in 
    Image.image List.rev (tempf n)
) ;;    

let aa = Memoized.make(fun n->
  List.filter (
   fun l->List.hd(l) = 1
  ) (main (n+1))
) ;;

let factor (x,y)=
    let rec factor0=(fun
       (graet,da_ober1,da_ober2)->
       if (da_ober1=[])||(da_ober2=[])
       then (List.rev graet,da_ober1,da_ober2)
       else let (a1,peurrest1)=Listennou.head_with_tail da_ober1
            and (a2,peurrest2)=Listennou.head_with_tail da_ober2 in
            if a1=a2
            then factor0(a1::graet,peurrest1,peurrest2)
            else (List.rev graet,da_ober1,da_ober2)
    ) in
    factor0([],x,y);;

let extends l1 l2=
   let (_,_,r2)=factor (l1,l2) in r2=[];;

let uu = Memoized.make(fun n->
    List.filter (
     fun l->let rl = List.rev l in
     extends rl [n;n-1]
    ) (main n)
) ;;

let vv = Memoized.make(fun n->
  List.filter (
   fun l->let rl = List.rev l in
     extends rl [n-1;n]
  ) (main n)
) ;;

let ww = Memoized.make(fun n->
  List.filter (
   fun l->
    let gl = (fun k->List.nth l (k-1)) in 
    ((gl 1)=(n-1))&&(gl (n-1)=(n-2))&&(gl n=n)
  ) (main n)
) ;;

let tt = Memoized.make(fun n->
  List.filter (
   fun l->
    let gl = (fun k->List.nth l (k-1)) in 
    ((gl 1)<>(n-1))&&(gl n=(n-2))
  ) (main n)
) ;;

let ss = Memoized.make(fun n->
  List.filter (
   fun l->
    let i = Listennou.find_index (n-1) l in 
    if (List.mem i [1;n-1;n])
    then false 
    else (List.nth l i)=n   
  ) (main n)
) ;;

let s_to_w =Memoized.make(fun n -> Ordered.fold_merge ointlist 
  [ss n;tt n;uu n;vv n;ww n] );;

let reversed_s_to_w =Memoized.make(fun n ->
    Ordered.safe_set ointlist (Image.image List.rev (s_to_w n)));;

let double_s_to_w =Memoized.make(fun n ->
      Ordered.intersect 
      ointlist  (s_to_w n) (reversed_s_to_w n) );;
  

let na n = List.length(aa n);;
let nk n = List.length(main n);;
let ns n = List.length(ss n);;
let nt n = List.length(tt n);;
let nu n = List.length(uu n);;
let nv n = List.length(vv n);;
let nw n = List.length(ww n);;

let zz n=(na n,nu n,nv n,nw n,ns n,nt n,nk n) ;;

let uv n = (uu n,vv(n+1)) ;;

let da n = (na (n+3))-(na(n+2)+na(n)+1) ;; 
let ka n = (nk(n)) -(2*na(n-1)) ;;

let selector = Memoized.make(fun (n,t)->
  List.filter (
   fun l->List.hd(l) = t
  ) (main (n+1))
) ;;

let aa = Memoized.make (fun n->selector(n,1)) ;;
let bb = Memoized.make (fun n->selector(n,2)) ;;
let cc = Memoized.make (fun n->selector(n,3)) ;;
let dd = Memoized.make (fun n->selector(n,4)) ;;
let ee = Memoized.make (fun n->selector(n,5)) ;;

let peggy n = Int_range.scale (fun j->List.length(selector(n,j))) 1 n ;;


let na n = List.length(aa n);;
let nb n = List.length(bb n);;
let nc n = List.length(cc n);;
let nd n = List.length(dd n);;
let ne n = List.length(ee n);;

let zz n=(na n,nb n,nc n,nd n,nk n,ne n);;




(************************************************************************************************************************
Snippet 52 : Rename scanned files
************************************************************************************************************************)
open Needed_values ;;

let ap1 = Absolute_path.of_string (home^"/Downloads/Building_site/");;
let s_ap1 = Absolute_path.to_string ap1 ;;
let u2 = Unix_again.beheaded_simple_ls (Directory_name.of_string s_ap1) ;;
let u3 = Image.image (fun s->(int_of_string(Cull_string.interval s 8 14),s)) u2 ;;
let (Set_of_poly_pairs_t.S u4) = Set_of_poly_pairs.safe_set u3 ;;
let u5 = Int_range.index_everything (Image.image fst u4) ;;
let u6 = Image.image (
  fun (k,old_k)->
    let sk = string_of_int k 
     and sok = string_of_int old_k in 
  "mv "^s_ap1^"korres\\ "^sok^".jpg "^s_ap1^"p"^sk^".jpg"
) u5 ;; 
let u7 = Image.image Sys.command u6 ;;

Coherent_pdf.workspace_directory := s_ap1 ;;
Coherent_pdf.implode ("p","") ;;

(************************************************************************************************************************
Snippet 51 : Write repetitive code for PARI-GP
************************************************************************************************************************)
let s_ap = home^
"/Teuliou/Bash_scripts/Pari_Programming/my_pari_code/follenn2.gp" ;;

let ap = Absolute_path.of_string s_ap ;;

let gtext t1 t2 = 
  String.concat "\n"
  ["c_fa="^(string_of_int t1); 
   "c_t1="^(string_of_int t2); 
   "c_for_t2=big_subst(for_t2,[t1,fa],[c_t1,c_fa])";
 "interm=subst(part3_for_t3,t1,c_t1)";
 "c_for_t3=factor(polresultant(interm,c_for_t2,t2))[2,1]";
 "around_t2=make_zero(special_euclid(interm,c_for_t2,t2,c_for_t3,t3),t2)";
 "listput(accu,[[c_fa,c_t1],around_t2])";
 "printf(Str(c_fa,\",\"c_t1,\" done\\n\"))"] ;;

let u1 = Cartesian.product (Int_range.range 11 50) (Int_range.range 101 140) ;;
let u2 = Image.image (fun (t1,t2)->gtext t1 t2) u1 ;; 
let u3 = String.concat "\n\n\n" u2 ;;

Io.Private.append_string_to_file u3 ap;;

(*

let z1 = home^
"/Teuliou/Bash_scripts/Pari_Programming/my_pari_code/follenn1.gp" ;;

let z2 = Lines_in_string.interval (rf z1) 60 67 ;;

*)

(************************************************************************************************************************
Snippet 50 : A useful shortcut using Lines_in_string.remove_interval_in_file 
************************************************************************************************************************)
let ri fn x y =
     Lines_in_string.remove_interval_in_file 
      (Absolute_path.of_string fn) x y ;;

(************************************************************************************************************************
Snippet 49 : Test the prepare_fw_with_dependencies.ml file
************************************************************************************************************************)
let the_other_one = 
   Absolute_path.of_string "../Idaho/Filewatching/fw_with_dependencies.ml" ;;
 
(*  #use "Githubbed_archive/prepare_fw_with_dependencies.ml";; 
   
 write_all_to_file the_other_one ;;  *)

(************************************************************************************************************************
Snippet 48 : Typical use of marked comments
************************************************************************************************************************)
open Needed_values ;;

let src1 = Absolute_path.of_string "../Idaho/Filewatching/file_watcher.ml";;

let dest1 = Absolute_path.of_string "../Idaho/Filewatching/fw_modular.ml";;

let act1 () = Mark_comments_for_copying_or_deletion.copy src1 dest1 ;;

let act2 () = Shorten_long_blank_intervals.in_file dest1 ;;


(************************************************************************************************************************
Snippet 47 : Replacements on several files
************************************************************************************************************************)
open Needed_values ;;

let aps_ref = ref [];;

 aps_ref := [
   Absolute_path.of_string "../Idaho/Compilation_management/coma_state.ml";
   Absolute_path.of_string "../Idaho/Compilation_management/modify_coma_state.ml";
   Absolute_path.of_string "../Idaho/Ocaml_analysis/read_needed_ocaml_files.ml"
];;

let rep (x,y) = 
   Image.image (
     fun ap -> Replace_inside.replace_inside_file (x,y) ap
   ) (!aps_ref) ;; 

rep ("subdir_at_module","subdir_for_module")   ;;
rep ("principal_ending_at_module","principal_ending_for_module")   ;;
rep ("mli_presence_at_module","mli_presence_for_module")   ;;
rep ("principal_mt_at_module","principal_mt_for_module")   ;;
rep ("mli_mt_at_module","mli_mt_for_module")   ;;
rep ("direct_fathers_at_module","direct_fathers_for_module")   ;;
rep ("ancestors_at_module","ancestors_for_module")   ;;
rep ("needed_libs_at_module","needed_libs_for_module")   ;;
rep ("needed_dirs_at_module","needed_dirs_for_module")   ;;
rep ("product_up_to_date_at_module","last_compilation_result_for_module")   ;;

let fix () =
   let _ = Sys.command (
      "cp Decomposed_filename/dfa_subdirectory.ml "^
      "../Idaho/Decomposed_filename/afd_sybdirectoru.ml") in
   let _ = Sys.command (
         "mv ../Idaho/Decomposed_filename/afd_sybdirectoru.ml "^
         "../Idaho/Decomposed_filename/dfa_subdirectory.ml") in   
   let _ =  Image.image (fun s->
       let ap= Absolute_path.of_string ("../Idaho/"^s) in 
       Replace_inside.replace_inside_file  
       ("Afd_sybdirectoru.","Dfa_subdirectory.") ap
       ) [
      "Decomposed_filename/dfn_rootless.ml";
      "Decomposed_filename/dfn_endingless.ml";
      "Decomposed_filename/dfn_middle.ml";
      "Decomposed_filename/dfn_full.ml";
      "find_suitable_ending.ml";"more_unix.ml";"node_project.ml";"prepare_dircopy_update.ml";
      "Compilation_management/coma_constant.ml";
      "Compilation_management/coma_state.ml";
      "Compilation_management/save_coma_state.ml";
      "Compilation_management/modify_coma_state.ml";
      "Compilation_management/create_world_copy.ml";
      "Filewatching/fw_configuration.ml";
      "Filewatching/file_watcher.ml";
      "Filewatching/fw_with_dependencies.ml";
   ] in    
   ruco () ;;

   let fix () =
      let _ = Sys.command (
         "cp Decomposed_filename/dfa_subdirectory.ml "^
         "../Idaho/Decomposed_filename/afd_sybdirectoru.ml") in
      let _ = Sys.command (
            "mv ../Idaho/Decomposed_filename/afd_sybdirectoru.ml "^
            "../Idaho/Decomposed_filename/dfa_subdirectory.ml") in   
      let _ =  Image.image (fun s->
          let ap= Absolute_path.of_string ("../Idaho/"^s) in 
          Replace_inside.replace_inside_file  
          ("Afd_sybdirectoru.","Dfa_subdirectory.") ap
          ) [
         "Decomposed_filename/dfn_rootless.ml";
         "Decomposed_filename/dfn_endingless.ml";
         "Decomposed_filename/dfn_middle.ml";
         "Decomposed_filename/dfn_full.ml";
         "find_suitable_ending.ml";"more_unix.ml";"node_project.ml";"prepare_dircopy_update.ml";
         "Compilation_management/coma_constant.ml";
         "Compilation_management/coma_state.ml";
         "Compilation_management/save_coma_state.ml";
         "Compilation_management/modify_coma_state.ml";
         "Compilation_management/create_world_copy.ml";
         "Filewatching/fw_configuration.ml";
         "Filewatching/file_watcher.ml";
         "Filewatching/fw_with_dependencies.ml";
      ] in    
      ruco () ;;   

      let fix () =
         let _ = Sys.command (
            "cp Padioleau/lexer_ml.ml "^
            "../Idaho/Padioleau/rexel_ml.ml") in
         let _ = Sys.command (
               "mv ../Idaho/Padioleau/rexel_ml.ml "^
               "../Idaho/Padioleau/lexer_ml.ml") in   
         let _ =  Image.image (fun s->
             let ap= Absolute_path.of_string ("../Idaho/"^s) in 
             Replace_inside.replace_inside_file  
             ("Rexel_ml.","Lexer_ml.") ap
             ) [
            "Padioleau/parse_ml.ml";
         ] in    
         ruco () ;;   


(*   
let aps_ref = ref [];;

 aps_ref := Image.image (fun s->Absolute_path.of_string ("../Idaho/"^s)) [
   "Decomposed_filename/dfn_rootless.ml";
   "Decomposed_filename/dfn_endingless.ml";
   "Decomposed_filename/dfn_middle.ml";
   "Decomposed_filename/dfn_full.ml";
   "find_suitable_ending.ml";"more_unix.ml";"node_project.ml";"prepare_dircopy_update.ml";
   "Compilation_management/coma_constant.ml";
   "Compilation_management/coma_state.ml";
   "Compilation_management/save_coma_state.ml";
   "Compilation_management/modify_coma_state.ml";
   "Compilation_management/create_world_copy.ml";
   "Filewatching/fw_configuration.ml";
   "Filewatching/file_watcher.ml";
   "Filewatching/fw_with_dependencies.ml";
];;

let rep (x,y) = 
   Image.image (
     fun ap -> Replace_inside.replace_inside_file (x,y) ap
   ) (!aps_ref) ;; 

rep ("Afd_sybdirectoru.","Dfa_subdirectory.")   ;;
*)


(************************************************************************************************************************
Snippet 46 : Extract a line interval from a file and treat it
************************************************************************************************************************)
open Needed_values ;;

let z1 = Image.image (
   fun (methname,_,_,_)->"mod_details \""^methname^"\" ([\n   \"\"\n],\"\") ;;"
) [] ;;
let z2=""::(z1@[""]);;
let z3 = String.concat "\n\n" z2 ;;
let z4 () = print_string z3 ;;

let fn = "Filewatching/fw_with_dependencies.ml";;
let fn = "Fads/jug.ml";;
let z5 = Lines_in_string.interval (rf fn) 9 92 ;;
let z6 = Lines_in_string.lines z5;;
let z7 = Image.image (fun line->"   \""^line^"\";") z6;;
let z8 = "\n\n\n"^(String.concat "\n" z7)^"\n\n\n" ;;
let z9  = print_string z8 ;;

let z6 = Image.image (Cull_string.cobeginning 6) z6;;

let g1 = Image.image (fun s->
   let j = Strung.char_finder_from (fun c->c='/') s 1 in 
   Cull_string.beginning j s  ) z6;;
let g2 = Ordered.sort Total_ordering.lex_for_strings g1 ;;
let g3 = String.concat " " g2;;

(************************************************************************************************************************
Snippet 45 : Remove all modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd = Dfa_subdirectory.of_line "Van_der_Waerden";;
let z1 = ae () ;;
let z2 = List.filter (
  fun (Dfn_endingless_t.J(r,s,m)) ->
    Dfa_subdirectory.begins_with s sd
) z1 ;;
let z3 = Image.image (
   fun (Dfn_endingless_t.J(r,s,m)) ->
    Dfa_module.to_line m
  ) z2 ;;
let act () = fgs z3 ;;  

(************************************************************************************************************************
Snippet 44 : Replacing a long interval in a file with another
************************************************************************************************************************)
open Needed_values ;;

let ap1 = Absolute_path.of_string "../Idaho/Compilation_management/coma_state.ml" ;;
let ap1_text = Io.read_whole_file ap1 ;;
let to_be_replaced = Lines_in_string.interval ap1_text 1 676 ;;

let towards_complement = rf "Fads/pan.ml";;
let replacement = Lines_in_string.interval towards_complement 9 240 ;;

let act7 () = Replace_inside.replace_inside_file (to_be_replaced,replacement) ap1;;

(************************************************************************************************************************
Snippet 43 : Visualize Git tree
************************************************************************************************************************)
open Needed_values ;;

let gc = "git -C "^home^"/Teuliou/OCaml/Idaho_backup ";;

let cmd_for_z0 = gc ^ "ls-tree -r HEAD > ~/Downloads/temp.txt";;
let z0 = Sys.command cmd_for_z0 ;;

let z1 = rf "~/Downloads/temp.txt";;
let z2 = Lines_in_string.lines z1 ;;
let z3 = List.filter (fun line->
   Supstring.contains line "depth_one"
  ) z2;;
let z4 = Image.image (Cull_string.cobeginning 53) z3;;


let cmds1 = Image.image (fun x->gc^"rm --cached "^x) z4 ;;
let cmds2 = Image.image (fun x->
  let cx = String.capitalize_ascii x in
  gc^"add "^cx) z4 ;;
let cmds3 = cmds1 @ cmds2 ;;
let anse1 = Image.image Sys.command cmds3 ;;

(************************************************************************************************************************
Snippet 42 : Miscellaneous tests on compilation management
************************************************************************************************************************)
open Needed_values ;;

let u1 = ae();;
let u2 = Image.image (fun el->Dfa_module.to_line(Dfn_endingless.to_module el)) u1 ;;
let u3 = Max.maximize_it_with_care (fun mn->
   min(List.length(abo mn))(List.length(bel mn))
  ) u2;;

let test1 = rv "Dfa_subdirectory.connectable_to_subpath" "cannectoble_to_sabputh" ;;
let exit_test1 = rv "Dfa_subdirectory.cannectoble_to_sabputh" "connectable_to_subpath" ;;

let test2= ren "dfa_subdirectory" "afd_sybdirectoru" ;;
let exit_test2= ren "afd_sybdirectoru" "dfa_subdirectory";;

let test3 = rensub "Depth_one_testdir/Depth_two_testdir" "Dopth_twe_tistder" ;;
let exit_test3 = rensub "Depth_one_testdir/Dopth_twe_tistder" "Depth_two_testdir" ;;

let create_ml_file_with_text (fname,text) =
   let s_ap = "Depth_one_testdir/Depth_two_testdir/adhoc/"^fname^".ml" in 
   let ap = Absolute_path.create_file_if_absent s_ap in 
   let _ = Io.overwrite_with ap text in ap ;;

let tf1 = create_ml_file_with_text("tf_one","let a= 2;;") ;;
let tf2 = create_ml_file_with_text("tf_two","let b= 3;;") ;;
let tf3 = create_ml_file_with_text("tf_three","let c= 7;;") ;;
let tf4 = create_ml_file_with_text("tf_four","let d= Tf_three.c+4;;") ;;
let tf5 = create_ml_file_with_text("tf_five","let e= Tf_four.d+5;;") ;;

regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_one.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_two.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_three.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_four.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_five.ml";;


Io.overwrite_with tf3 "let c=Tf_one.a+Tf_two.b;;" ;;

reco "1";;

fgs ["tf_one";"tf_two";"tf_three";"tf_four";"tf_five"] ;;

(************************************************************************************************************************
Snippet 41 : Extracting lines from a file and modifying them
************************************************************************************************************************)
open Needed_values ;;

let w2 = Image.image fst (vfm "Fw_with_dependencies") ;;
let z1 = rf "Filewatching/fw_with_dependencies.ml";;
let z2 = Lines_in_string.interval z1 1304 1351 ;;

let w2 = Image.image fst (vfm "Fw_with_small_details") ;;
let z1 = rf "Filewatching/fw_with_small_details.ml";;
let z2 = Lines_in_string.interval z1 309 334 ;;

let w2 = Image.image fst (vfm "Fw_with_archives") ;;
let z1 = rf "Filewatching/fw_with_archives.ml";;
let z2 = Lines_in_string.interval z1 285 317 ;;

let w2 = Image.image fst (vfm "File_watcher") ;;
let z1 = rf "Filewatching/file_watcher.ml";;
let z2 = Lines_in_string.interval z1 526 549 ;;

let w2 = Image.image fst (vfm "Fw_configuration") ;;

let z3 = Lines_in_string.lines z2 ;;
let z4 = Image.image (
  fun line->
    let j1=(try String.index_from line 1 ' ' with _->0)
    and j2=String.index line '=' in 
    Cull_string.trim_spaces (Cull_string.interval line (j1+1) (j2-1)) 
) z3 ;;
let z5 = Image.image (
  fun line->
       if not(String.contains line ' ')
       then line 
       else let j3 =  String.index line ' ' in 
             Cull_string.beginning j3 line
) z4 ;;
let bad_in_z5=List.filter (fun x->not(List.mem x w2)) z5;;

let z4=[];;
let check_z4 = Ordered.sort Total_ordering.lex_for_strings (Image.image snd z4) ;;

let write (fun_name,lbl)=
 let addendum=(
   if lbl = "constructor" then " dummy_fw" else
   if lbl = "zerovariate_producer" then " dummy_arg" else ""

) in 
 "    let "^fun_name^" = extract_"^lbl^" All_printables."^fun_name^addendum^" ;;" ;;

let z5 = "\n\n\n" ^(String.concat "\n" (Image.image write z4)) ^ "\n\n\n";; 
let z6 () = print_string z5 ;;


let write2 (fun_name,lbl)=
 "let "^fun_name^" = Private.Exit."^fun_name^" ;;" ;;

let z7 = "\n\n\n" ^(String.concat "\n" (Image.image write2 z4)) ^ "\n\n\n";; 
let z8 () = print_string z7 ;;

(************************************************************************************************************************
Snippet 40 : Get a list of value names from an interval of lines in a file
************************************************************************************************************************)
open Needed_values ;;

let u1 = rf "Filewatching/fw_with_dependencies.ml";;
let u2 = Lines_in_string.interval u1 33 48 ;;
let u3 = Lines_in_string.lines u2 ;;
(*
let compute_names = Image.image (
  fun line ->
     let temp1 = Cull_string.two_sided_cutting ("    let ","") line in 
     let j1 = Strung.char_finder_from (fun c->List.mem c [' ';'\r';'\t']) temp1 1 in
     Cull_string.interval temp1 1 (j1-1)
) u3;;
*)


(************************************************************************************************************************
Snippet 39 : Using intervals of line indices to extract values from a module
************************************************************************************************************************)
open Needed_values ;;

let u1 = Needed_values.rf "Compilation_management/coma_state.ml";;
let u2 = Lines_in_string.indexed_lines u1 ;; 

let extract_interval ((i,j),_) =
   let temp1 = List.filter (fun (k,_)->(i<=k) && (k<=j)) u2 in 
   let temp2 = Image.image snd temp1 in 
   String.concat "\n" temp2 ;;

let ref_for_colombo = ref ([
   ((961, 985),   "compute_principal_ending");
   ((1024, 1031), "registrations_for_lonely_ending");
   ((2267, 2350), "Simplified_ts_creation")
]:(((int * int) * string) list)) ;;
let ref_for_curcuma = ref ([
  ((5, 616), "Automatic"); ((634, 634), "needed_libs_at_module");
   ((636, 636), "ancestor_at_module"); ((637, 637), "needed_dirs_at_module");
   ((659, 659), "ordered_list_of_modules"); ((671, 671), "root");
   ((792, 801), "find_needed_data"); ((925, 959), "PrivateTwo");
   ((1034, 1055), "complete_id_during_new_module_registration");
   ((1394, 1466), "register_mlx_file_on_monitored_modules");
   ((1842, 1874), "Try_to_register")
]:(((int * int) * string) list)) ;;
let ref_for_replacements = ref([]:((string * string) list));;

ref_for_replacements:=[
  "=registrations_for_lonely_ending ","=Colombo.registrations_for_lonely_ending ";
  "=md_compute_modification_times ","=Colombo.md_compute_modification_times ";
  "=md_associated_modification_time ","=Colombo.md_associated_modification_time ";
  "= md_compute_modification_time ","= Colombo.md_compute_modification_time ";
  "=compute_principal_ending ","=Colombo.compute_principal_ending ";
  "= ocamldebug_printersfile_path ","= Colombo.ocamldebug_printersfile_path ";
] ;;


type spice = Colombo | Curcuma ;;

let associated_ref = function 
   Colombo -> ref_for_colombo 
  |Curcuma -> ref_for_curcuma ;; 

let spice_to_string = function 
  Colombo -> "Colombo" 
 |Curcuma -> "Curcuma" ;; 

let haddock_order = 
    let oi = Total_ordering.for_integers 
    and prod = Total_ordering.product in 
    prod (prod oi oi) Total_ordering.lex_for_strings ;; 

 let add_interval ((i,j),name) spice=
  let raf = associated_ref spice in 
    (raf:= Ordered.insert haddock_order ((i,j),name)
      (!raf)) ;;

let copy_whole spice=
   let temp1 = Image.image (extract_interval) (!(associated_ref spice)) in
   let whole = String.concat "\n\n" temp1 
   and s = spice_to_string spice in 
   let corrected_whole = Replace_inside.replace_several_inside_string
    (!ref_for_replacements) whole in 
   Replace_inside.overwrite_between_markers_inside_file
   ~overwriter:corrected_whole
   ("(* Beginning of "^s^" *)\n\n","\n\n(* End of "^s^" *)")
   (Absolute_path.of_string "Fads/pan.ml") ;;

let main ((i,j),name) spice=
   let _= add_interval ((i,j),name) spice in copy_whole spice ;;


(*


*)

(*

main ((961,985),"compute_principal_ending") Colombo;;  
main ((1024,1031),"registrations_for_lonely_ending") Colombo;; 
main ((1655,1658),"ocamldebug_printersfile_path") Colombo;;  
main ((2267,2350),"Simplified_ts_creation") Colombo;;     

main ((5,616),"Automatic") Curcuma;; 
main ((654,654),"set_product_up_to_date_at_module") Curcuma;; 
main ((629,629),"subdir_at_module") Curcuma;; 
main ((630,630),"principal_ending_at_module") Curcuma;; 
main ((631,631),"mli_presence_at_module") Curcuma;; 
main ((634,634),"needed_libs_at_module") Curcuma;; 
main ((636,636),"ancestor_at_module") Curcuma;; 
main ((637,637),"needed_dirs_at_module") Curcuma;; 
main ((649,649),"set_needed_libs") Curcuma;; 
main ((651,651),"set_ancestors_at_module") Curcuma;; 
main ((653,653),"set_needed_dirs") Curcuma;; 
main ((655,656),"set_directories") Curcuma;; 
main ((659,659),"ordered_list_of_modules") Curcuma;; 
main ((660,660),"follows_it") Curcuma;; 
main ((661,661),"all_used_subdirs") Curcuma;; 
main ((671,671),"root") Curcuma;; 
main ((680,685),"endingless_at_module") Curcuma;; 
main ((691,697),"check_ending_in_at_module") Curcuma;; 
main ((720,723),"registered_endings_at_module") Curcuma;; 
main ((781,790),"modules_with_their_ancestors") Curcuma;; 
main ((792,801),"find_needed_data") Curcuma;; 
main ((805,813),"needed_dirs_and_libs_in_command") Curcuma;; 
main ((913,917),"compute_subdirectories_list") Curcuma;; 
main ((919,922),"check_registrations") Curcuma;; 
main ((925,959),"PrivateTwo") Curcuma;; 
main ((987,1007),"complete_info") Curcuma;; 
main ((1034,1055),"complete_id_during_new_module_registration") Curcuma;; 
main ((1378,1389),"printer_equipped_types_from_data") Curcuma;; 
main ((1394,1466),"register_mlx_file_on_monitored_modules") Curcuma;; 
main ((1468,1653),"Modern") Curcuma;; 
main ((1661,1797),"Ocaml_target_making") Curcuma;; 
main ((1842,1874),"Try_to_register") Curcuma;; 

*)

(************************************************************************************************************************
Snippet 38 : Remove all snippets containing a given substring (todo : integrate it
into the Manage_diary module directly)
************************************************************************************************************************)
open Needed_values ;;

let ap_for_diary = Absolute_path.of_string "Githubbed_archive/diary_archive.ml";;
let (g1,Manage_diary.Private.D g2) =  Manage_diary.Private.read_and_parse ap_for_diary ;;
let g3 = Int_range.index_everything g2;;
let g4 = List.filter (fun (j,(x,y))->Substring.is_a_substring_of "Vdw_" y) g3 ;;
let g5 = Image.image fst g4 ;;
let act1 () = Manage_diary.remove_snippets g5;;

let ap1 = Absolute_path.of_string "Fads/cloth.ml" ;;
let act1 () = Manage_diary.extract_at_index_and_append_to_file 
   84 ap1 ;;


let ap1 = Manage_diary.Private.usual_path ;;
let (prologue,diary1) = Manage_diary.Private.read_and_parse ap1 ;;
let (Manage_diary.Private.D l_diary1) = diary1 ;;
let z1 = List.nth l_diary1 (84-1) ;;



(************************************************************************************************************************
Snippet 37 : Search/replace following some module refactoring
************************************************************************************************************************)
open Needed_values ;;

let aps = ref [] ;;
let list_for_reps = ref [] ;;
aps := (Image.image (fun s->Absolute_path.of_string s) 
  [
    "ordered.ml";
    "Van_der_Waerden/Width_up_to_four/vdw_nonempty_index.ml";
    "Van_der_Waerden/vdw_common.ml";
    "Ordered_Lists/functor_for_sets.ml";
    "Ocaml_analysis/follow_ocaml_values.ml";
  ] );;
list_for_reps := [
  "Total_ordering.t)","Total_ordering_t.t)";
  "Total_ordering.t )","Total_ordering_t.t )";
  "Total_ordering.t\r","Total_ordering_t.t\r";
] ;;


let act1 () = List.iter 
  (Replace_inside.replace_several_inside_file 
     (!list_for_reps)) (!aps);

(************************************************************************************************************************
Snippet 36 : Extracting modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd1 = Dfa_subdirectory.of_line "Van_der_Waerden/Width_up_to_four";;

let u1 =ae () ;;

let u2 = List.filter (
  fun eless ->
    Dfa_subdirectory.begins_with (Dfn_endingless.to_subdirectory eless) sd1
) u1;; 

let u3 = Image.image (
   fun eless -> Dfa_module.to_line(Dfn_endingless.to_module eless)
) u2 ;;

(************************************************************************************************************************
Snippet 35 : Remove all "automatic" modules 
************************************************************************************************************************)
open Needed_values ;;

let u1 = ae ();;
let u2 = Image.image (fun eless ->
   Dfa_module.to_line(Dfn_endingless.to_module eless)  
) u1;;
let u3 = List.filter (
  fun x-> Supstring.ends_with x "_automatic"
) u2 ;;

let computed_u3 = ["concrete_object_automatic"; "fw_wrapper_automatic"; "coma_state_automatic";
"fw_nonmodular_wrapper_automatic"; "hex_flattened_end_strategy_automatic"];;

let g1 = vfm "hex_flattened_end_strategy_automatic" ;;
let g2 = Image.image fst g1 ;;

let g3 = Image.image (fun x-> Replace_inside.replace_inside_string ("x",x) "let x = Automatic.x ;;") g2;;
let g4 = String.concat "\n" g3 ;;
let g5 = "\n\n\n" ^ g4 ^ "\n\n\n" ;; 

let h1 = List.flatten (Image.image snd g1) ;;
let h2 = Ordered.sort Total_ordering.standard h1 ;;
let h3 = List.iter (
  fun fn -> Replace_inside.replace_inside_file ("Hex_flattened_end_strategy_automatic.","Hex_flattened_end_strategy.") fn
) h2 ;;


(************************************************************************************************************************
Snippet 34 : Typical use of the Manage_diary module
************************************************************************************************************************)
let act1 () = Manage_diary.fix_indexation ();;

let act2 () = Manage_diary.remove_snippets [ (* put indices here *)];;


let diary_text = Io.read_whole_file ap_for_diary ;;

let (g1,g2) =  Manage_diary.Private.read_and_parse ap_for_diary ;;

(************************************************************************************************************************
Snippet  33 : Deduce the lower measure from the usual measure (related to Vdw)
************************************************************************************************************************)
let measure n =
  if n<1 then 0 else 
  let q=(n/9) in 
  match n mod 9 with
   0 -> 4*q+1 
  |1 -> 4*q+1
  |2 -> 4*q+2  
  |3 -> 4*q+2
  |4 -> 4*q+3
  |5 -> 4*q+4
  |6 -> 4*q+4  
  |7 -> 4*q+4
  |8 -> 4*q+4 
  | _ -> failwith("unforeseen");;   

let lower_measure n =
   if n<1 then 0 else 
   let q=(n/9) in 
   match n mod 9 with
    0 -> 4*q
   |1 -> 4*q
   |2 -> 4*q 
   |3 -> 4*q
   |4 -> 4*q+1
   |5 -> 4*q+1
   |6 -> 4*q+2  
   |7 -> 4*q+2
   |8 -> 4*q+3 
   | _ -> failwith("unforeseen");;  



let compute_lower_measure n = 
  let tempf = (fun t->measure(n+t)-measure(t)) in 
  snd(Min.minimize_it tempf (Int_range.range 1 20)) ;;   


(************************************************************************************************************************
Snippet  32 : Relocate all modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd1 = Dfa_subdirectory.of_line 
   "Van_der_Waerden" ;;

let sd2 = Dfa_subdirectory.of_line 
   "Van_der_Waerden/First_try" ;;

let u1 = ae () ;;   
let u2 = List.filter ( fun
  (Dfn_endingless_t.J(r,sd,m)) -> Dfa_subdirectory.begins_with sd sd1
) u1 ;;
let u3 = Image.image ( fun
(Dfn_endingless_t.J(r,sd,m)) -> Dfa_module.to_line m
) u2 ;;

let act1 () = Explicit.image (fun mn->relo mn sd2) u3 ;;

(************************************************************************************************************************
Snippet  31 : Delete all modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd1 = Dfa_subdirectory.of_line 
   "Ocaml_analysis/Standardized_concrete_ocaml_types" ;;

let u1 = ae () ;;   
let u2 = List.filter ( fun
  (Dfn_endingless_t.J(r,sd,m)) -> Dfa_subdirectory.begins_with sd sd1
) u1 ;;
let u3 = List.rev_map ( fun
(Dfn_endingless_t.J(r,sd,m)) -> Dfa_module.to_line m
) u2 ;;

let act1 () = fgs u3 ;;


(************************************************************************************************************************
Snippet  30 : Code from an abandoned, self-contained module
************************************************************************************************************************)
exception Too_many_arguments of int ;;

let wrap_in_parentheses_if_needed typename =
    if String.contains typename ' '
    then "( "^typename^" )"
    else  typename ;;    

let max_nbr_of_arguments = 7 ;;    

let arguments_in_input argname n=
    if n> max_nbr_of_arguments 
    then raise(Too_many_arguments(n))
    else let temp1 = Int_range.scale (fun k->
          if k<=n then argname^(string_of_int k) else "_") 1 max_nbr_of_arguments in 
         "(" ^ (String.concat "," temp1) ^ ")" ;;       

let listify is_a_list name =
    if not(is_a_list) 
    then name 
    else (wrap_in_parentheses_if_needed name)^" list" ;;     

let add_appendix_to_last_line appendix lines =
      let (last_line,other_lines) = Listennou.head_with_tail (List.rev lines) in 
      List.rev ((last_line^appendix)::other_lines) ;;    

(************************************************************************************************************************
Snippet  29 : Permutations far (wrt Hamming distance) from shift with constants. 
************************************************************************************************************************)
open Needed_values ;;

let hamming_distance perm1 perm2 =
  let temp1 = List.combine perm1 perm2 in 
  List.length(List.filter (fun (x,y)->x<>y) temp1);;

let generic_translate n t  = (Int_range.range t n) @ (Int_range.range 1 (t-1))  ;;

let all_translates =Memoized.make (fun n -> Int_range.scale (generic_translate n) 1 n);;

let measure n perm = snd(Min.minimize_it (hamming_distance perm) (all_translates n)) ;;


let iii = Memoized.make Permutation.iii ;;

let ff = Memoized.make(fun n->
   let whole = iii n 
   and meas = Memoized.make (measure n) in 
   let m = snd(Max.maximize_it meas whole) in 
   Explicit.filter (fun perm->meas(perm)=m) whole);;   


let gg n = Chronometer.it ff n;;

let hh n = (measure n (List.hd(ff n)));;

Int_range.scale (fun x->(x,hh x)) 3 10;;

let hf n = List.hd(ff n) ;;

(************************************************************************************************************************
Snippet  28 : Mass inheritance from a Private submodule 
************************************************************************************************************************)
let z1 = 
  ["conventional_files_with_full_content";
   "conventional_files_with_minimal_content"; "debug_build_subdir";
   "exec_build_subdir"; "full_set_of_needed_dirs"; "git_ignored_subdirectories";
   "minimal_set_of_needed_dirs"; "rootless_path_for_loadingsfile";
   "rootless_path_for_parametersfile"; "rootless_path_for_printersfile";
   "rootless_path_for_targetfile"; "usual_build_subdir"; "utility_files_subdir"] ;;
  
let z2 = Image.image (fun x->" let "^x^" = Private."^x^" ;;") z1;; 
  
let z3 = "\n\n\n" ^ (String.concat "\n" z2) ^ "\n\n\n" ;; 

(************************************************************************************************************************
Snippet  27 : Typical use of the Other_coma_state module 
************************************************************************************************************************)
let act1 () = 
   Other_coma_state.repopulate 
   (Needed_data_summary_t.Everything);;

(* or Other_coma_state.initialize () ;; *)   

let see = Other_coma_state.see_yet_unofficial_changes ();; 
let act2 () = Other_coma_state.officialize_changes ();;

Other_coma_state.Private.ref_for_unofficial_changes :=
(
   Some [ ]
)
;;

Other_coma_state.Private.ref_for_unofficial_changes :=
(
   Some ["Decomposed_filename/dfa_subdirectory.ml";
   "Filewatching/fw_with_batch_compilation.ml";
   "Filewatching/fw_with_githubbing.ml"; "Filewatching/fw_with_persisting.ml";
   "Compilation_management/modify_coma_state.ml";
   "Compilation_management/usual_coma_state.ml";
   "Ocaml_analysis/read_needed_ocaml_files.ml";
   "Ocaml_analysis/compute_all_ocaml_items.ml";
   "Compilation_management/needed_data_summary.ml";
   "Compilation_management/create_world_copy.ml";
   "Compilation_management/other_coma_state.ml"; "needed_values.ml";
   "self_contained_module_copy.ml"]
)
;;




(************************************************************************************************************************
Snippet  26 : Testing freezing and unfreezing of world copies
************************************************************************************************************************)
open Needed_values ;;

let remote_dir = Dfa_root.of_line 
   (home^"/Teuliou/OCaml/Forgotten_projects/Html_scraping_project") ;;

(*

To store a "frozen" copy of the project in a separate directory.
You can combine this with a cp -R (which often will not suffice by itself since you 
also need the dependecies from other subdirectories).

*)

let sd= Dfa_subdirectory.of_line "Text_editing/Html_scraping";;

let g1 = Create_world_copy.frozen_copy (!ucs)
    ~destination:remote_dir 
    (Needed_data_summary_t.Selection([],
    [sd])) ;;
   
(*

Much later, you can "unfreeze" the project as follows

*)    

let g2 = Create_world_copy.unfreeze_copy (!ucs) remote_dir ;;

(*

Then, you can cd to the separate dir, launch utop in it, and enjoy.

*)


(************************************************************************************************************************
Snippet  25 : Remove interval of lines in a file 
************************************************************************************************************************)
let ap = Absolute_path.of_string "Imported/Aantron/aantron_markup.ml";;
let old_text = Io.read_whole_file ap ;;
let v1 = Lines_in_string.indexed_lines old_text ;;
let v2 = List.filter (fun (j,line)->(299<=j)&&(j<=338) ) v1 ;;
let v3 = Image.image (
   fun (j,line)->
      let i1 = Substring.leftmost_index_of_in "val " line 
      and i2 = Substring.leftmost_index_of_in ":" line in 
      Cull_string.trim_spaces(Cull_string.interval line (i1+4) (i2-1))
) v2 ;;
let tab = String.make 5 ' ' ;;
let v3 = Image.image (fun (j,line) -> 
  if Supstring.begins_with line tab
  then Cull_string.two_sided_cutting (tab,"") line
  else line   
    ) v2;;

let tab = String.make 7 ' ' ;;
let v4 = Ordered.sort Total_ordering.lex_for_strings v3;;
let v5 = Image.image (fun name -> tab^"let "^name^" = Aantron_encoding."^name^" ;;") v4;;
let old_snippet = String.concat "\n" (Image.image snd v2) ;;
let new_snippet = String.concat "\n"  v5;;
let act () = Replace_inside.replace_inside_file (old_snippet,new_snippet) ap ;; 

let ap = Absolute_path.of_string "Imported/Aantron/aantron_markup.ml";;
let old_text = Io.read_whole_file ap ;;
let v1 = Lines_in_string.indexed_lines old_text ;;
let v2 = List.filter (fun (j,line)->(299<=j)&&(j<=338) ) v1 ;;
let v3 = Image.image (fun (j,line)->Cull_string.trim_spaces line) v2 ;;
let v4 = List.filter (Substring.is_the_beginning_of "The value ") v3;;
let v5 = Image.image (
   fun line->
      let i1 = Substring.leftmost_index_of_in "`" line 
      and i2 = Substring.leftmost_index_of_in "'" line in 
      Cull_string.trim_spaces(Cull_string.interval line (i1+1) (i2-1))
) v4 ;;
let v6 = Ordered.sort Total_ordering.lex_for_strings v5;;
let v7 = Image.image (fun name -> "let "^name^" = Aantron_utility."^name^" ;;") v6;;
let v8 = "\n\n\n" ^ (String.concat "\n" v7) ^ "\n\n\n";;
let v9 = print_string v8 ;;

(************************************************************************************************************************
Snippet  24 : Removing module wrappers in a set of files
************************************************************************************************************************)
let remove_module_wrapper_in_text text =
  let lines = Lines_in_string.indexed_lines text in 
  let (i1,_)= List.find (fun (_,line)->
    Supstring.begins_with (Cull_string.trim_spaces line) "module "
  ) lines in
  let (i2,_)= List.find (fun (_,line)->
    Supstring.begins_with (Cull_string.trim_spaces line) "end"
  ) (List.rev lines) in 
  let selected_lines = List.filter_map (
    fun (i,line)->if List.mem i [i1;i2] then None else Some line
  ) lines in 
  String.concat "\n" selected_lines ;;

let remove_module_wrapper_in_file ap =
  let old_text = Io.read_whole_file ap in 
  let new_text = remove_module_wrapper_in_text old_text in 
  Io.overwrite_with ap new_text ;;

let the_dir = Directory_name.of_string ((Sys.getcwd())^"/Imported/Aantron/Temp"  ) ;;
let u1 = Unix_again.simple_ls the_dir ;;

let act1 () = List.iter remove_module_wrapper_in_file u1 ;;

(************************************************************************************************************************
Snippet  23 : Sorting names in the dictionary order
************************************************************************************************************************)
let z1 = Ordered.sort Total_ordering.lex_for_strings 
[
  "current_state";
  "emit";
  "push_and_emit";
  "pop";
  "emit_end";
  "initial_state";
  "document_state";
  "doctype_state";
  "root_state";
  "after_root_state";

] ;;

let z2 = "\n\n\n" ^ (String.concat "\n" z1) ^ "\n\n\n" ;;

print_string z2;;

(************************************************************************************************************************
Snippet  22 : Remove phpbb links to footnotes 
************************************************************************************************************************)
let write1 k=
  let sk = string_of_int k in 
  "\n[b][color=blue]("^sk^")[/color][/b]\n" ;;

let reps = Int_range.scale (fun j->(write1 j,"")) 1 43  ;;

let dir = (Sys.getenv "HOME")^"/Teuliou/html_files/Translations/";;  
let ap1 =   Absolute_path.create_file_if_absent (dir^"/notes_to_dot.txt") ;;

let text1= Io.read_whole_file ap1;;
let lines1 = Lines_in_string.indexed_lines text1;;

let act1 () = Replace_inside.replace_several_inside_file reps ap1;;




(************************************************************************************************************************
Snippet  21 : Typical use of Html_to_phpbb.translate
************************************************************************************************************************)
open Needed_values ;;

let u1 = rf (home^"/Teuliou/html_files/Fenton/divine_origin.html");;

let u2 = Html_to_phpbb.translate u1;;

let ap1 = Absolute_path.of_string 
  (home^"/Teuliou/html_files/Translations/divine_origine_translated.txt") ;;

Io.overwrite_with ap1 u2;;  

(************************************************************************************************************************
Snippet  20 : Interaction between "beginning" and "end" of a large tex file
************************************************************************************************************************)
open Needed_values;;

let beg_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/beginning_of_text.txt");; 
 
let end_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/end_of_text.txt");; 

let tex_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/blet_pius_xii.tex");;   

let cmd_for_texshop = "osascript "^home^"/Teuliou/Bash_scripts/Automation/compile_with_texshop.scpt";;

let loop () =
    let beg_part = Io.read_whole_file beg_ap 
    and end_part = Io.read_whole_file end_ap in 
    let whole = beg_part ^ "\n" ^ end_part in 
    let _ =Io.overwrite_with tex_ap whole in 
    Sys.command cmd_for_texshop;;

let tr k = Io_again.transfer_first_lines_of_to k end_ap beg_ap;;

let ll k = let temp = Lines_in_string.interval (Io.read_whole_file end_ap) k k in 
  (temp,Strung.explode temp);;

let rye (a,b) = Replace_inside.replace_inside_file (a,b) end_ap ;; 

let rblap () = Remove_blank_lines_around_percents.in_file end_ap ;;

let rlc pattern = 
   let _ = Lines_in_string.remove_lines_containing_substring_in_file 
   pattern end_ap in rblap ();;

let usual_cleaning () =
   Replace_inside.replace_several_inside_file 
   [
     (".! ",".1 ");
     (".!\n",".1 ");
    ("\012","");
    ("_","");
    ("#","");
    ("\\Vhat","What");
    ("\\Vhen","When");
    ("\\Vhile","While");
    ("\194\162","c");
    ("\226\128\156","\194\171");
    ("\226\128\157","\194\187");
    ("$","\194\167");
    (" & "," \\& ");
    ("&\226\128\153","d'");
    ("\\xii. ","xii. ");
    ("\194\165","V");
    ("\n1}","\n1 ");
    ("\n}","\n1 ");
    ] end_ap ;;


(************************************************************************************************************************
Snippet  19 : Add blank space at the beginning of lines (to make copy&paste easier )
************************************************************************************************************************)
open Needed_values;;

let blanks = String.make 3 ' ';; 

let reform_line x=
  if (x="")||(Supstring.begins_with x blanks) then x else blanks^x;; 

let reform_string s=
  let temp1 = Lines_in_string.lines s in 
  let temp2 = Image.image reform_line temp1 in 
  String.concat "\n" temp2 ;;


let the_ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/PDF_files/Printable/Preparation/greek_in_vl.txt");; 

let old_text = Io.read_whole_file the_ap ;;

let new_text = reform_string old_text ;;

Io.overwrite_with the_ap new_text;; 


(************************************************************************************************************************
Snippet  18 : Delete some HTML footnotes (with their links) and reindex
************************************************************************************************************************)
open Needed_values;;

let ap1 = Absolute_path.of_string (home^"/Teuliou/html_files/Fortescue/papacy_451.html");; 
let old_text = Io.read_whole_file ap1 ;;

let u1 = Enumerate_html_links_to_footnotes.main old_text ;;

let see = Image.image (fun ((i_start,i_end),link_idx)->
    Cull_string.interval old_text i_start i_end) u1 ;; 

let bad_indices = [1;3;4;5;10;11;18;20] ;;

let u2 = List.filter (fun ((i_start,i_end),link_idx)-> 
    (List.mem link_idx bad_indices) ) u1;;
let u3 = Image.image fst u2 ;;
let u4 = Int_range.index_everything u3 ;; 
let u5 = Image.image (
  fun (k,(i_start,i_end))->((i_start,i_end),k)
) u4;;
let u6 = Image.image (fun ((i_start,i_end),link_idx)-> 
    ((i_start,i_end),List.assoc_opt (i_start,i_end) u5) 
) u1;;
let write_link opt = match opt with 
  None -> ""
  |Some(k) -> let sk=string_of_int k in 
              "<span id=\"ln"^sk^"\"><a href=\"#n"^sk^"\">("^sk^")</a></span>";;

let u7 = Image.image ( fun (pair,opt)->(pair,write_link opt) ) u6;;

let new_text = Strung.replace_ranges_in u7 old_text ;;

Io.overwrite_with ap1 new_text ;;

let ap1 = Absolute_path.of_string (home^"/Teuliou/html_files/Fortescue/papacy_451.html");; 
let old_text = Io.read_whole_file ap1 ;;
let v1 = Enumerate_html_footnotes.main old_text ;;
let see = Image.image (fun ((i_start,i_end),_)->
    Cull_string.interval old_text i_start i_end) v1 ;;   
let good_indices = List.filter (fun k->not(List.mem k bad_indices )) (Int_range.range 1 (List.length v1));;
let reindexation = Image.image (fun (i,j)->(j,i)) (Int_range.index_everything good_indices) ;;
let v2 = Image.image (
  fun ((footnote_idx,html_content),(i_start,i_end))->
    ((footnote_idx,html_content),(i_start,i_end),List.assoc_opt footnote_idx reindexation)
) v1;;
let write_reindexed_version ((i_start,i_end),(footnote_idx,html_content),opt_idx)=
   let new_text = (match opt_idx with 
      None -> ""
      |Some(k)->let sk=string_of_int k in 
      "<div id=\"n"^sk^"\"><a href=\"#ln"^sk^"\">("^sk^")</a> "^html_content^"</div>"
   ) in 
   ((i_start,i_end),new_text);;
let v3 = Image.image write_reindexed_version v2;;
let new_text = Strung.replace_ranges_in v3 old_text ;;
Io.overwrite_with ap1 new_text ;;





(************************************************************************************************************************
Snippet  17 : Remove contiguous lines in a file
************************************************************************************************************************)
open Needed_values ;;

let the_ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/Fortescue/papacy_451.html");; 

let old_text = Io.read_whole_file the_ap ;;

let to_be_deleted = Lines_in_string.interval old_text 3387 4948 ;;

Replace_inside.replace_inside_file (to_be_deleted,"") the_ap ;; 


(************************************************************************************************************************
Snippet  16 : Put fillable footnotes in an html draft 
************************************************************************************************************************)
let write1 k=
  let sk = string_of_int k in 
  "<span id=\"ln"^sk^"\"><a href=\"#n"^sk^"\">("^sk^")</a></span>"^
  "\n\n\n"^
  "<div id=\"n"^sk^"\"><a href=\"#ln"^sk^"\">("^sk^")</a>   \n\n "^
  "</div>" ;;

let dir = (Sys.getenv "HOME")^"/Teuliou/html_files/Fortescue";;  
let ap =   Absolute_path.create_file_if_absent (dir^"/pra_filled.html") ;;

let memo = String.concat "\n\n" (Int_range.scale write1 121 170) ;;

Io.overwrite_with ap memo ;; 

(************************************************************************************************************************
Snippet  15 : Aggregate pages
************************************************************************************************************************)
let home = Sys.getenv "HOME" ;;
let workdir = home^"/Downloads/Adrifor";;

Coherent_pdf.workspace_directory := workdir ;;

Coherent_pdf.extract_page_range "main" (5,5) ;;

Coherent_pdf.implode ("p","") ;;

Coherent_pdf.merge ["part1";"part2";"part3"] "whole";;

(************************************************************************************************************************
Snippet  14 : Cleaning up and fixing a chaotic mass download
************************************************************************************************************************)
let downloads_s_dir = home ^ "/Downloads";; 

let u1 = Unix_again.quick_beheaded_complete_ls downloads_s_dir  ;;
let u2 = List.filter (Substring.is_the_beginning_of "iau") u1;;

let p_value s =
     let j1 = Substring.leftmost_index_of_in_from "-" s 5 in 
     let j2 = Substring.leftmost_index_of_in_from "-" s (j1+1) in
     int_of_string(Cull_string.interval s (j1+1) (j2-1));; 

let min_pageNumber = 9 and max_pageNumber = 70 ;; 

let pre_u3 = Image.image (fun s->(s,p_value s)) u2 ;;
let (bad_ones1,u3) = List.partition (fun (s,p)->(p<min_pageNumber) || (p>max_pageNumber)) pre_u3 ;;
let cmds1 = Image.image (fun (s,_)->"rm "^downloads_s_dir^"/"^s) bad_ones1;;
let act1 () = Image.image Sys.command cmds1 ;;

let reached_page_numbers = Ordered.sort Total_ordering.for_integers (Image.image snd u3) ;; 

let u4 = Int_range.scale (
   fun p->(p,List.filter_map (fun (s,q)->if q=p then Some s else None) u3)
) min_pageNumber max_pageNumber;;

let u5 = List.filter (fun (p,representatives) -> List.length(representatives)>1) u4 ;;
let bad_ones2 =List.flatten 
  (Image.image (fun (p,representatives) -> List.tl(representatives)) u5);;
let cmds2 = Image.image (fun s->"rm "^downloads_s_dir^"/"^s) bad_ones2;;
let act2 () = Image.image Sys.command cmds2 ;;

let bad_ones3 = List.filter_map 
  (fun (p,representatives) -> 
     if List.length(representatives)=0 then Some p else None) u4 ;;

    
let cmds3 = Image.image (fun (p,l)->
    let fn = List.hd l in 
    let sk = string_of_int(p-6) in 
    "mv "^downloads_s_dir^"/"^fn^" "^downloads_s_dir^"/p"^sk^".pdf") u4;;
let act3 () = Image.image Sys.command cmds3 ;;

let workdir = home^"/Downloads/";;

Coherent_pdf.workspace_directory := workdir ;;

Coherent_pdf.implode ("p","") ;;



(************************************************************************************************************************
Snippet  13 : Update footnote format in old phpbb text
************************************************************************************************************************)
open Needed_values ;;

let home = Sys.getenv "HOME" ;;

let ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/OCRed_texts/barenton_on_loisy.txt");;

let main_text = Io.read_whole_file ap ;; 

let opening_tag= "[color=blue]";;
let closing_tag = "[/color]";;

let u1 = Substring.occurrences_of_in opening_tag main_text ;;
let u2 = Substring.occurrences_of_in closing_tag main_text ;;

let u3 = List.combine u1 u2;;
let opening_length = String.length opening_tag ;;
let closing_length = String.length closing_tag ;;
let u4 = Image.image (fun (old_a,old_b)->
    let a = old_a+opening_length and b=old_b-1 in 
    ((a,b),Cull_string.interval main_text a b)
    ) u3;;
exception RA of string ;; 
    
let rhine_analysis s=
  let j1 = Substring.leftmost_index_of_in "(" s in 
  let j2 = Substring.leftmost_index_of_in ")" s in 
  if (j1<0)||(j2<0) then raise(RA(s)) else    
  let idx = int_of_string(Cull_string.interval s (j1+1) (j2-1)) in 
  (idx,Cull_string.cobeginning j2 s);;

let u5 = Image.image (
   fun ((a,b),text) -> (((a,b),text),rhine_analysis text)
)  u4 ;; 

let (redundant_u6,redundant_u7)=List.partition (fun (_,(idx,content))->content="") u5;;

let u6 = Image.image (fun (((a,b),text),(idx,content))->
      ((a-opening_length,b+closing_length),idx) ) redundant_u6;;

let u7 = Image.image (fun (((a,b),text),(idx,content))->
        ((a-opening_length,b+closing_length),idx,content) ) redundant_u7;;      

let check1 = ( (List.length u6) = (List.length u7) ) ;;        

let u8 = Image.image (
   fun ((a,b),idx,content) ->
      let s_idx=string_of_int idx in 
      ((a,b),"[size=90][b][color=blue]("^s_idx^")[/color][/b]"^content^"[/size]")
) u7;;
let corrected_text = Strung.replace_ranges_in u8 main_text;;

Io.overwrite_with ap corrected_text ;;



(************************************************************************************************************************
Snippet  12 : Combinatorial musings
************************************************************************************************************************)
exception Hard_computation of string * int ;;

let translate_all t (n,sols)=
  let increment = (if t="1" then 1 else 0) in  
  (n+increment,Image.image (fun u->t^u) sols) ;;

let synthesize_after_translating (n1,sols1) (n2,sols2) =
    if n1 < n2 then (n2,sols2) else 
    if n2 < n1 then (n1,sols1) else (n1,sols1@sols2);; 

let synthesize res1 res2 = 
    synthesize_after_translating 
     (translate_all "0" res1) (translate_all "1" res2);; 


let main_hashtbl = ((Hashtbl.create 50): (string * int, int * (string list)) Hashtbl.t);; 

let am x y = Hashtbl.add main_hashtbl x y;;

let eval_at_one pattern =
     if pattern="" then (1,["1"]) else 
     (if (String.get pattern 0)='F' then  (1,["1"]) else (0,["0"]) );;

let enforce_conditions pattern = 
    let m = String.length pattern in 
    let temp2 = Int_range.scale (fun j->
        if (j<5)&&(j<>2) then "N" else 
        if j>m then "F" else Cull_string.interval pattern j j) 1 (max 4 m) in 
    String.concat "" temp2;;     

(*

enforce_conditions "A";;
enforce_conditions "ANPE";;

*)

let prepare_computation pattern=
   if pattern="" then ("",Some"NFNN") else 
   let tail = Cull_string.cobeginning 1 pattern in 
   (
    if (String.get pattern 0)='F'
    then (tail,Some(enforce_conditions tail))  
    else (tail,None)    
   )      ;;

let left_n_decomposition pattern =
     let j1 = Strung.char_finder_from (fun c->c<>'N') pattern 1 in 
     if j1=0 
     then (String.length pattern,"") 
     else (j1-1,Cull_string.cobeginning (j1-1) pattern);; 

(*

left_n_decomposition "ABC";;
left_n_decomposition "NNABC";;

*)


let eval_quickly pattern n =
    if n=0 then (0,[""]) else
    if n=1 then eval_at_one pattern else
    match Hashtbl.find_opt main_hashtbl (pattern,n) with 
     Some(l,sol)->(l,sol)
     |None -> raise(Hard_computation(pattern,n)) ;;

let eval_using_left_n_decomposition old_pattern n =
    if n=0 then (0,[""]) else
    if n=1 then eval_at_one old_pattern else
    let old_length = String.length old_pattern in 
    let pattern = (if old_length > n 
                   then Cull_string.beginning n old_pattern
                   else old_pattern) in 
    let (number_of_ns,core) = left_n_decomposition pattern in 
    let (size_of_sols,old_sols) = eval_quickly core (n-number_of_ns) in 
    let new_sols =(
       if number_of_ns=0 
      then old_sols 
      else  let offset = String.make number_of_ns '0' in 
      Image.image (fun t->offset^t) old_sols
      ) in 
    (size_of_sols,new_sols);; 


let eval_slowly pattern n =
       try eval_using_left_n_decomposition pattern n with _->
         let (passive_case,opt_active_case) = prepare_computation pattern in 
         let case0 =  translate_all "0" (eval_using_left_n_decomposition passive_case (n-1)) in 
         match opt_active_case with 
         None -> case0
        |Some(active_case) ->
          let case1 =  translate_all "1" (eval_using_left_n_decomposition active_case (n-1)) in 
          synthesize_after_translating case0 case1;;     

let consider pattern n=
   let res = eval_slowly pattern n in 
   let _= (am (pattern,n) res) in res ;;

let ff n = eval_slowly "" n;;

let bf n = Image.image ff (Int_range.range 1 n);;



consider "" 2 ;;
consider "" 3 ;;
consider "FN" 2;;
consider "" 4;;
for k=3 to 30 do let _ = consider "FNN" k in ();let _=consider "" (k+2) in () done ;;


let res1 = Int_range.scale (fun x->fst(ff x)) 1 30;;




(************************************************************************************************************************
Snippet  11 : Massive conversion of audios into videos using ffmepgs
************************************************************************************************************************)
let base1 =
  [
  
  "001_Ier_D_de_l_Avent_01_12_2013_27min33.mp";
  "002_IIe_dimanche_de_l_Avent_Moulins_09_12_1990_35min32.mp";
  "003_IIIe_dimanche_de_l_Avent_Gaudete_En_Anjou_16_12_1990_29min26.mp";
  "004_IIIe_dim_de_l_Avent_Vendee_16_12_2018.mp";
  "005_IVe_dimanche_de_l_Avent_Comparaison_du_temps_des_Patriarches_avec_le_notre_Moulins_23_12_1990_28min19_Copie_en_conflit_de_debian_2019_11_13.mp";
  "006_Vigile_Nativite_Choix_et_gouts_de_Dieu_Moulins_24_12_1990_18min52.mp";
  "007_Vigile_de_Noel_En_Vendee_24_12_2012_29min31.mp";
  "008_Messe_de_Minuit_Anniversaire_naissance_de_la_fille_ainee_de_l_Eglise_Moulins_25_12_1996_28min04.mp";
  "009_Messe_du_jour_de_Noel_Divinite_du_Christ_Seigneur_demontree_par_S_Paul_aux_Hebreux_a_partir_de_l_Ancien_Testament_Moulins_25_12_1996_19min48.mp";
  "010_Nativite_Messe_de_minuit_En_Vendee_25_12_2012_14min17.mp";
  "011_Nativite_Messe_du_jour_En_Vendee_25_12_2012_11min26.mp";
  "012_Dimanche_dans_l_Octave_de_la_Nativite_Dum_medium_silentium_Tours_31_12_1989_21min22.mp";
  "013_Dimanche_dans_l_octave_de_NoeI_30_12_2012_21min08.mp";
  "014_Dimanche_dans_l_Octave_de_NoeI_ND_de_l_Epine_29_12_2019_28_min.mp";
  "015_Circoncision_En_Vendee_01_01_2013_26min22.mp";
  "016_Saint_Nom_de_Jesus_En_Vendee_02_01_2013_22min24.mp";
  "017_Epiphanie_06_01_1996_38min27.mp"; "018_Epiphanie_07_01_90_29min34.mp";
  "019_Octave_de_l_Epiphanie_Manifestation_de_la_Divinite_de_NS_Moulins_13_01_1991_38min17.mp";
  "020_La_Sainte_Famille_Moulins_07_01_1996_31min33.mp";
  "021_IIe_dimanche_apres_l_Epiphanie_Mayenne_ND_de_l_Epine_19_01_2014_30min50.mp";
  "022_IIIe_dimanche_apres_l_Epiphanie_Noli_vinci_a_malo_sed_vince_in_bono_malum_25_01_90_34min31.mp";
  "023_Septuagesime_Deux_genres_de_conversion_Moulins_27_01_1991_28min27.mp";
  "024_Sexagesime_ND_de_Lourdes_Montee_de_l_esprit_anti_chretien_et_apparitions_de_ND_Moulins_11_02_1996_33min36.mp";
  "025_Quinquagesime_Annonce_prophetique_de_la_Passion_Moulins_10_02_1991_24min39.mp";
  "026_Ier_dimanche_de_Careme_Sens_mystique_des_montees_vers_Jerusalem_16_02_1997_15min18.mp";
  "027_Ier_dimanche_de_Careme_Sur_la_Penitence_Moulins_04_03_1990_24min53.mp";
  "028_IIe_Dim_de_Careme_Transfiguration_Equilibre_entre_desolations_et_consolations_Moulins_24_02_1991_34min49.mp";
  "029_IIIe_dimanche_de_Careme_Contre_le_demon_muet_Moulins_10_03_1996_26min07.mp";
  "030_IIIe_dimanche_de_Careme_Sur_l_Annonciation_Maternite_virginale_et_voeu_de_virginite_22_03_92.mp";
  "031_IVe_dimanche_de_Careme_Joie_dans_la_penitence_et_Montee_du_Carmel_10_03_91_26min06.mp";
  "032_Ier_dimanche_de_la_Passion_ReveIation_progressive_de_la_divinite_de_NS_Moulins_05_04_1992_40min02.mp";
  "033_Dimanche_des_Rameaux_En_Vendee_01_04_2012_16min12.mp";
  "034_Veillee_pascale_Sur_l_illogisme_de_l_attitude_actuelle_des_Juifs_talmudistes_Moulins_11_04_1998_26min11.mp";
  "035_Paques_Moulins_04_04_1996_44min07.mp";
  "036_Dimanche_in_albis_Quasi_modo_Les_corps_glorieux_Tours_02_04_1989_19min22.mp";
  "037_IIe_dimanche_apres_Paques_Misericorde_et_Justice_de_Dieu_equilibre_de_l_esprit_chretien_Fete_de_saint_Pierre_de_Verone_29_04_1990_30min12.mp";
  "038_IIIe_dimanche_apres_Paques_Modicum_et_videbitis_Me_ND_de_l_Epine_Mayenne_21_04_2013_26min12.mp";
  "039_IVe_dimanche_apres_Paques_Attachement_apostolique_a_NS_pour_remonter_de_sa_nature_humaine_a_sa_Divinite_28_04_1991_23min03.mp";
  "040_Ve_dimanche_apres_Paques_Dieu_console_par_les_siens_Tours_30_04_1989_20min42.mp";
  "041_Ascension_24_05_1990_25min18.mp";
  "042_Ascension_Moulins_09_05_1991_27min55.mp";
  "043_Ascension_2011_Vendee_20min32.mp";
  "044_Ascension_ND_de_l_Epine_10_05_2018_26min12.mp";
  "045_Dimanche_dans_l_Octave_de_l_Ascension_12_05_2013_Mayenne_ND_de_l_Epine_15min28.mp";
  "046_Dimanche_dans_l_Octave_de_l_Ascension_2014_Vendee_19min.mp";
  "047_Pentecote_Moulins_19_05_1991_29min18.mp";
  "048_Pentecote_Vendee_19_05_2013_30min45.mp";
  "049_Pentecote_ND_de_l_Epine_15_05_2016_30min_50.mp";
  "050_Tres_Sainte_Trinite_Moulins_16_06_1990_33min05.mp";
  "051_Fete_du_Tres_Saint_Sacrement_Moulins_02_06_1991_28min53.mp";
  "052_Dimanche_dans_l_octave_du_Saint_Sacrement_Notre_Dame_de_l_Epine_2_06_2013_14min26.mp";
  "053_Solennite_du_Sacre_Coeur_et_Saint_Jean_Baptiste_Il_faut_qu_Il_croisse_et_que_je_diminue_Moulins_24_06_1990_24min55.mp";
  "054_Dimanche_dans_l_Octave_du_Sacre_Coeur_29min09.mp";
  "055_IVe_dimanche_apres_la_Pentecote_Moulins_16_06_1991_31min54.mp";
  "056_Ve_dimanche_apres_la_Pentecote_Sur_reparation_et_componction_Tours_18_06_1989_34min59.mp";
  "057_VIe_dimanche_apres_la_Pentecote_Saint_Henri_et_la_sanctification_dans_le_monde_Moulins_15_07_1990_37min39.mp";
  "058_VIIe_Dimanche_apres_la_Pentecote_Mayenne_ND_de_l_Epine_07_07_2013_36min04.mp";
  "059_VIIIe_dimanche_apres_la_Pentecote_Saint_Bonaventure_et_le_bonheur_en_Dieu_seul_Moulins_14_07_1991_42min06.mp";
  "060_IXe_dimanche_apres_la_Pentecote_Mayenne_ND_de_l_Epine_21_07_2013_34min55.mp";
  "061_Xe_dimanche_apres_la_Pentecote_Sur_le_Principe_et_Fondement_Tours_31_07_1988_22min08.mp";
  "062_XIe_dimanche_apres_la_Pentecote_04_08_1991_33min37.mp";
  "063_XIe_Dimanche_apres_Pentecote_12_08_2012_15min44.mp";
  "064_XIIe_dimanche_apres_la_Pentecote_11_08_2013_27min32.mp";
  "065_XIIIe_dimanche_apres_la_Pentecote_18_08_2013_27min34.mp";
  "066_XIVe_dimanche_apres_la_Pentecote_Saint_Louis_25_08_1991.mp";
  "067_XVe_dimanche_apres_la_Pentecote_Foi_en_la_Divinite_de_NS_Moulins_16_09_90_27min23.mp";
  "068_XVIe_dimanche_apres_la_Pentecote_03_09_89_24min15.mp";
  "069_XVIIe_Dimanche_apres_la_Pentecote_ND_de_l_Epine_16_09_2018_40_min.mp";
  "070_XVIIIe_Dim_apres_la_Pentecote_ND_de_l_Epine_8_10_2017_38min30.mp";
  "071_XIXe_dimanche_apres_la_Pentecote_Sur_la_colere_24_09_89_22min01.mp";
  "072_XXe_D_ap_Pent_22_10_2017_ND_de_l_Epine_1h.mp";
  "073_Fete_du_Christ_Roi_27_10_1991_27min22.mp";
  "074_XXIe_dimanche_apres_Pentecote_25min24.mp";
  "075_XXIIe_Dimanche_apres_la_Pentecote_ND_de_l_Epine_21_10_2018_36_min_35.mp";
  "076_XXIIIe_dimanche_apres_la_Pentecote_Sur_le_Purgatoire_11_11_90.mp";
  "077_XXIVe_et_dernier_dimanche_apres_la_Pentecote_Moulins_26_11_1989_26min38.mp";
  "078_XXIVe_ap_Pent_IVe_ap_Eph_Tempete_apaisee_Moulins_29min55.mp";
  "079_XXVe_dim_ap_Pent_Ve_ap_Epiphanie_Moulins_04_02_1990_20min.mp";
  "080_XXVIe_dim_apres_la_Pentecote_VIe_ap_Epiphanie_17_11_1991_30min38.mp";
  "081_Solennite_du_Tres_Saint_Rosaire_Tours_08_10_1989_23min31.mp";
  "082_Maternite_Divine_de_Notre_Dame_11_10_2015_Mayenne_ND_de_l_Epine_28min20.mp";
  "083_Toussaint_Moulins_01_11_1996_30min49.mp";
  "084_Commemoration_des_defunts_Vendee_2_novembre_2012_21min07.mp";
  "085_Fete_de_la_Dedicace_de_Saint_Jean_de_Latran_09_11_2014_33min47.mp";
  "086_Solennite_de_l_Immaculee_Conception_Moulins_1989_30min05.mp";
  "087_Presentation_de_NS_au_temple_et_Purification_de_Marie_Mayenne_ND_de_l_Epine_02_02_2014_23min13.mp";
  "088_Decouverte_de_la_Sainte_Croix_ND_de_l_Epine_3_05_2015_34min28.mp";
  "089_St_Philippe_et_st_Jacques_ND_de_l_Epine_11_05_2014_18min42_.mp";
  "090_Solennite_de_Ste_Jeannes_d_Arc_Moulins_13_05_1990_27min49.mp";
  "091_VIe_dimanche_apres_la_Pentecote_Solennite_de_St_Pierre_et_St_Paul_Monde_conquis_de_haute_lutte_par_papes_et_martyrs_Moulins_30_06_1991_42min50.mp";
  "092_Fete_de_Sainte_Anne_Mayenne_ND_de_l_Epine_26_07_2015_42min41.mp";
  "093_Saint_Laurent_diacre_et_martyr_Vendee_10_08_2014_33min24.mp";
  "094_Fete_de_saint_Luc_Mayenne_ND_de_l_Epine_18_10_2015_27min09.mp";
  "095_Sur_la_maniere_de_precher_1988_22min32.mp";
  "096_Assomption_Moulins_15_08_1991_29min15.mp"; "097_Assomption_2012.mp";
  "098_Assomption_2013_Vendee_21min34.mp";
  "099_Saint_Joachim_Pere_de_la_TS_Vierge_Marie_Vendee_16_08_2015_24min03.mp";
  "100_Tres_Precieux_Sang_Mayenne_ND_de_l_Epine_1er_juillet_2013_27min03.mp";
  "101_Nativite_de_Notre_Dame_Moulins_08_09_1991_30min28.mp";
  "102_Notre_Dame_des_sept_douleurs_15_09_1996_15min51.mp";
  "103_Solennite_de_Ste_Therese_de_l_Enfant_Jesus_et_de_la_Ste_Face_Moulins_30_09_1990_22min47.mp";
  "104_Solennite_de_saint_Michel_Archange_Moulins_29_09_1991_32min31.mp";
  "105_IVe_D_ap_Pentecote_ND_de_l_Epine_28_juin_2020_35min.mp";
  "106_VIe_D_ap_Pentecote_ND_de_l_Epine_12_7_2020_39min25.mp";
  "107_refutation_T_de_M_ete_2013_Intro_10min.mp";
  "108_refutation_T_de_M_ete_2013_partie_1_18min19.mp";
  "109_refutation_T_de_M_ete_2013_partie_2_18min41.mp";
  "110_refutation_T_de_M_ete_2013_partie_3_23min22.mp";
  "111_refutation_T_de_M_ete_2013_partie_4_14min54.mp";
  "112_refutation_T_de_M_ete_2013_partie_5_21min57.mp";
  "113_refutation_T_de_M_ete_2013_partie_6_21min51.mp";
  "114_refutation_T_de_M_ete_2013_partie_7_17min02.mp";
  "115_VIIIe_D_Ap_Pent_7_8_2011_a_La_Boutouere_Mayenne_21min57.mp";
  "116_XVIe_Dimanche_apres_la_Pentecote_ND_de_lEpine_20_09_2020_22min14.mp";
  "117_Christ_Roi_25_10_2020_ND_de_lEpine_45min29.mp";
  "118_Presentation_des_ouvrages_de_labbe_Zins_video_001_52min.mp";
  "119_Presentation_des_ouvrages_de_labbe_Zins_video_002_1h.mp";
  "120_Presentation_des_ouvrages_de_labbe_Zins_video_003_55min.mp";
  "121_Presentation_des_ouvrages_de_labbe_Zins_video_004_42min.mp"
  
  ];;

let n1 = List.length base1 ;;

let main_list = String.concat "\n" base1 ;; 




let write1 x =  
    let idx = string_of_int(int_of_string(String.sub x 0 3)) in 
    "wget -c http://larchange.org/audio/"^x^"3\n"^
    "ffmpeg -loop 1 -i STP.jpg -i "^x^"3 -acodec copy -vcodec libx265 -shortest "^x^"4\n"^
    "rm "^x^"3\n"^
    "echo $'\\n\\n\\n\\n\\n\\n Step "^idx^" of 121 finished\\n\\n\\n\\n\\n\\n'" ;; 

let home = Sys.getenv "HOME" ;;    
let ap_for_main_script = Absolute_path.of_string 
    (home^"/Teuliou/Bash_scripts/Convert_audio_to_video/audiotovideo.sh");;    

let main_script = String.concat "\n\n\n" (Image.image write1 base1) ;;      

let fill_main_script () =
   Io.overwrite_with ap_for_main_script main_script ;;

fill_main_script () ;;


let base2 = Int_range.index_everything base1;;
let (pre_part1,remains1) = List.partition (fun (j,x)->j<=40) base2;;
let (pre_part2,pre_part3) = List.partition (fun (j,x)->j<=80) remains1;;
let part1 = Image.image (fun (_,s)->s^"4") pre_part1;;
let part2 = Image.image (fun (_,s)->s^"4") pre_part2;;
let part3 = Image.image (fun (_,s)->s^"4") pre_part3;;

let main_dir = (Sys.getenv "HOME")^"/Teuliou/Bash_scripts/Convert_audio_to_video/Sermons";;
let cmds1 = Image.image (
  fun x->"mv "^main_dir^"/"^x^" "^main_dir^"_1_a_40/"
) part1 ;;
let cmds2 = Image.image (
  fun x->"mv "^main_dir^"/"^x^" "^main_dir^"_41_a_80/"
) part2 ;;
let cmds3 = Image.image (
  fun x->"mv "^main_dir^"/"^x^" "^main_dir^"_81_a_117/"
) part3 ;;
let cmds = cmds1 @ cmds2 @ cmds3 ;;





(************************************************************************************************************************
Snippet  10 : Removing misinterpreted characters from a freshly OCR-ed doc
************************************************************************************************************************)
open Needed_values ;;

let home = Sys.getenv "HOME" ;;

let dirname = "Lossky";;
let num_of_pages = 196 ;;    

let partial_texts = Int_range.scale (fun k->
    let sk = string_of_int k in 
    let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
    let prelude="% Beginning of page "^sk^"\n"
    and postlude="\n% End of page "^sk in 
    prelude^(rf fn)^postlude)  1 num_of_pages ;;
      
let full_ap = Absolute_path.create_file_if_absent  
(home^"/Downloads/"^dirname^"/full.txt");;  
let tex_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/pre_vladimir_lossky.txt");; 
 

let full_text = "\n"^(String.concat "\n" partial_texts)^"\n" ;;


let adjusted_text = Replace_inside.replace_several_inside_string 
   ["&","\\&";
    "$","\\$";
    "#","\\#";
    "_","\\_";
    "\194\162","c";
    "\194\176","o";
    "\226\130\172","E"] full_text ;;   
 
Replace_inside.overwrite_between_markers_inside_file 
  ~overwriter:adjusted_text ("% BEGINNING MARKER","%END MARKER") tex_ap;;


let text1 = Io.read_whole_file tex_ap ;;
let u1 = Substring.occurrences_of_in "% End of page 7\n" text1 ;;
let i1 = List.hd u1 ;;
let u2 = Cull_string.interval text1 (i1-20) i1;;

Replace_inside.replace_several_inside_file
   ["\n\012\n","\n"] tex_ap ;;

(************************************************************************************************************************
Snippet  9 : Typical use of the Trim_text_between_tags module
************************************************************************************************************************)
let home = Sys.getenv "HOME" ;;
let ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/Translations/act_of_body_translated.txt");;

Trim_text_between_tags.in_file [("[i]","[/i]")] ap;;


(************************************************************************************************************************
Snippet  8 : Put fillable footnotes in a phpbb draft 
************************************************************************************************************************)
let write1 k=
  let sk = string_of_int k in 
  "[b][color=blue]("^sk^")[/color][/b]\n\n"^
  "[size=90][b][color=blue]("^sk^")[/color][/b]   [i]   [/i]   [/size]";;

let dir = (Sys.getenv "HOME")^"/Teuliou/html_files/Translations";;  
let ap =   Absolute_path.create_file_if_absent (dir^"/temp.txt") ;;

let memo = String.concat "\n\n" (Int_range.scale write1 1 5) ;;

Io.overwrite_with ap memo ;; 

(************************************************************************************************************************
Snippet  7 : Finding extremal vertices in a polytope
************************************************************************************************************************)
open Needed_values ;;


let small_n=1;;

let u1 = Cartesian.fifth_power (Int_range.range 0 small_n);;

let u2 = List.filter_map (
  fun (a1,a2,a3,a4,a5)->
      let a6 = a3+a4-a5
      and a7 = a2+a4-a5
      and a8 = a1-a4+a5 in
      if List.for_all (fun x->(x>=0)) [a6;a7;a8] 
      then Some(a1,a2,a3,a4,a5,a6,a7,a8)   
    else None
) u1 ;;


let u3 = List.tl u2 ;; 

let shadow (a1,a2,a3,a4,a5,a6,a7,a8) =
    let l = [a1;a2;a3;a4;a5;a6;a7;a8] in 
    Set_of_polys.safe_set(List.filter (fun j->List.nth l (j-1)=0) [1; 2; 3; 4; 5; 6; 7; 8]) ;;

let supporting_rel uple uple2 =
    if uple=uple2 then false else 
    Set_of_polys.is_included_in (shadow uple) (shadow uple2);;  
     
let supporters uple = List.filter (supporting_rel uple ) u3;;

let u4 = List.filter_map (
   fun uple -> let r= supporters uple in 
   if r<>[]
    then Some(uple,r)
  else None
) u3;;

let u5 = List.filter (fun uple->(supporters uple)=[]) u3;;

[[0, 0, 0, 1, 1, 0, 0, 0],
   [0, 0, 1, 0, 0, 1, 0, 0], [0, 0, 1, 1, 1, 1, 0, 0],
   [0, 1, 0, 0, 0, 0, 1, 0], [0, 1, 0, 1, 1, 0, 1, 0],
   [0, 1, 1, 0, 0, 1, 1, 0], [0, 1, 1, 0, 1, 0, 0, 1],
   [0, 1, 1, 1, 1, 1, 1, 0], [1, 0, 0, 0, 0, 0, 0, 1],
   [1, 0, 0, 1, 0, 1, 1, 0], [1, 0, 0, 1, 1, 0, 0, 1],
   [1, 0, 1, 0, 0, 1, 0, 1], [1, 0, 1, 1, 0, 2, 1, 0],
   [1, 0, 1, 1, 1, 1, 0, 1], [1, 1, 0, 0, 0, 0, 1, 1],
   [1, 1, 0, 1, 0, 1, 2, 0], [1, 1, 0, 1, 1, 0, 1, 1],
   [1, 1, 1, 0, 0, 1, 1, 1], [1, 1, 1, 0, 1, 0, 0, 2],
   [1, 1, 1, 1, 0, 2, 2, 0], [1, 1, 1, 1, 1, 1, 1, 1]]

(*

let small_n=2;;

let u1 = Cartesian.fourth_power (Ennig.ennig 0 small_n);;

let u2 = Option.filter_and_unpack (
  fun (a1,a2,a3,a5)->
      let a4 = small_n-(a1+a2+a3) 
      and a6 = small_n-(a1+a2+a5)
      and a7 = small_n-(a1+a3+a5) 
      and a8 = (2*a1+a2+a3+a5)-small_n in
      if List.for_all (fun x->(x>=0)&&(x<=small_n)) [a4;a6;a7;a8] 
      then Some(a1,a2,a3,a4,a5,a6,a7,a8)   
    else None
) u1 ;;

let u3 = List.filter (
   fun (a1,a2,a3,a4,a5,a6,a7,a8) -> 
    List.exists (fun (x,y)->x<>y) [a1,a8;a2,a7;a3,a6;a4,a5]
) u2;;

*)

(************************************************************************************************************************
Snippet  6 : Abandoned code snippet to remove paragraph containing footnotes.
It is much simpler to add html paragraph tags only when the region of text
does not contain footnotes (see the Htmlize module and snippet 4)
************************************************************************************************************************)
exception Unbalanced_html_paragraph_tags of int * int ;;
exception Nested_html_paragraphs of (int * int) * (int * int) ;;

let footnote_marker = ref " nowfeetneto ";;


let html_par_opening_tag = "<p>";;
let html_par_closing_tag = "</p>";;

let op_tag_length = (String.length html_par_opening_tag)-1 ;;
let cl_tag_length = (String.length html_par_closing_tag)-1 ;;

let detect_nested_paragraphs l=
   let temp1 = Listennou.universal_delta_list l in 
   match List.find_opt (fun 
     (((i1,j1),(i2,j2)),((i3,j3),(i4,j4)))->i3<j2
   ) temp1 with 
   None -> ()
   |Some(((i1,j1),(i2,j2)),((i3,j3),(i4,j4))) ->
       raise(Nested_html_paragraphs((i2,j2),(i3,j3)));;

let locate_all_paragraphs_in_html txt=
  (* paragraphs are assumed to be non-nested *)
  let temp1 = Substring.occurrences_of_in html_par_opening_tag txt 
  and temp2 = Substring.occurrences_of_in html_par_closing_tag txt in 
  let o1 = List.length temp1 and c1 = List.length temp2 in 
  if o1<>c1 
  then raise(Unbalanced_html_paragraph_tags(o1,c1))
  else   
  let temp3 = Image.image (fun i->(i,i+op_tag_length)) temp1
  and temp4 = Image.image (fun j->(j,j+cl_tag_length)) temp2 in 
  let temp5 = List.combine temp3 temp4 in 
  let _ = detect_nested_paragraphs temp5 in 
  temp5 ;; 

let remove_paragraphs_containing_footnotes txt l= ();;
        



(************************************************************************************************************************
Snippet  5 : Mass deletion of modules 
************************************************************************************************************************)
open Needed_values;;

let u1 = ae ();;

let u2 = List.filter (fun
   (Dfn_endingless_t.J(r,s,m)) -> Dfa_subdirectory.begins_with s 
   (Dfa_subdirectory_t.SD "Text_editing")
) u1 ;;

let u3 = Image.image (fun
(Dfn_endingless_t.J(r,s,m)) -> match m with 
  (Dfa_module_t.M m0) -> m0
) u2;;

let u4 = List.filter (
  fun m0->not(List.mem m0 ["control_pdf_size";"read_russian"])
) u3;;

let u5 = Image.image (
  fun m0->(m0, bel m0)
) u4;;

let u6 = List.filter (
  fun (m0,b0)->List.for_all (fun m1->(List.mem m1 u4)) b0
) u5;;

let u7=List.rev(Image.image fst u6);;

(************************************************************************************************************************
Snippet  4 : Code to OCR-size PDF's into .txt (and later html)
************************************************************************************************************************)
open Needed_values ;;


let write1 k =
    let sk = string_of_int k in 
    "pdftoppm main.pdf p"^sk^" -png -f "^sk^" -singlefile\n"^
    "tesseract -l fra p"^sk^".png p"^sk;;

let dirname = "Pius_XII";;
let num_of_pages = 326 ;;

let ap1 = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/script.sh");;

let text1 = "\n\n\n"^(String.concat "\n" (Int_range.scale write1 1 num_of_pages))^"\n\n\n" ;;   
    
Io.overwrite_with ap1 text1;;


let partial_texts = Int_range.scale (fun k->
  let sk = string_of_int k in 
  let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
  "%\n% Page "^sk^" \n%\n"^(rf fn))  7 num_of_pages ;;


let full_ap = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/full.txt");;  

let full_text = String.concat "\n" partial_texts ;;
let full_text = Htmlize.pages partial_texts ;;

Io.overwrite_with full_ap full_text;;

let (page1,page2,ranges_for_lfm,ranges_for_fm) =
   Option.get(!(Htmlize.Private.error_handling_ref ));;

(* Re-indexed version *)

let main_list = 
  [51;149;187;189;201;203;231;249;257;261;263;265;269;271;297] ;;

let write1 j =
 let k =List.nth main_list (j-1) in 
 let sj = string_of_int j 
 and sk = string_of_int k in 
 "pdftoppm main.pdf p"^sk^" -png -f "^sj^" -singlefile\n"^
 "tesseract -l fra p"^sk^".png p"^sk;;

let dirname = "Blet_again";;
let num_of_pages = 15 ;;

let ap1 = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/script.sh");;

let text1 = "\n\n\n"^(String.concat "\n" (Int_range.scale write1 1 num_of_pages))^"\n\n\n" ;;   
 
Io.overwrite_with ap1 text1;;

let partial_texts = Int_range.scale (fun k->
  let sk = string_of_int k in 
  let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
  let announcer = "%\n% Page "^sk^" \n%\n" in 
  let uncompressed_pagetext = rf fn in 
  let pagetext = Make_paragraphs_one_lined.in_string 
  (Remove_hyphens.in_string uncompressed_pagetext) in  
  announcer^pagetext)  1 num_of_pages ;;

let full_ap = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/full.txt");;  

let full_text = String.concat "\n" partial_texts ;;
let full_text = Htmlize.pages partial_texts ;;

Io.overwrite_with full_ap full_text;;




(************************************************************************************************************************
Snippet  3 : Typical use of the Read_russian module
************************************************************************************************************************)
open Needed_values ;;

let home = Sys.getenv "HOME";;
let txt1 = rf (home^"/Downloads/temp.txt");;

let z1 = Read_russian.read txt1;;
let z2= Read_russian.prepare_dictation txt1;;

let ap1 = Absolute_path.of_string (home^"/Teuliou/LaTeX/Moullapl/archipelago.tex");;

let act () = 
Replace_inside.overwrite_between_markers_inside_file 
  ~overwriter:z2
  ("\\begin{document}","\\end{document}") ap1;;

(************************************************************************************************************************
Snippet  2 : Convert footnotes between phpBB and HTML
************************************************************************************************************************)
let peggy j =
   let sj=string_of_int j in 
   "<span id=\""^"ln"^sj^"\"><a href=\"#n"^sj^"\">("^sj^")</a></span>";;
 
 let u1 = Int_range.scale peggy 3 43;;  
 
 let u2 ="\n\n\n"^(String.concat "\n\n" u1) ^"\n\n\n";;
 
 
 let peggy j =
   let sj=string_of_int j in 
   "<div id=\""^"n"^sj^"\"><a href=\"#ln"^sj^"\">("^sj^")</a> <i> </i>  </div>";;
 
 let u1 = Int_range.scale peggy 3 43;;  
 
 let u2 ="\n\n\n"^(String.concat "\n\n" u1) ^"\n\n\n";;
 

(************************************************************************************************************************
Snippet  1 : Typical use of the Coherent_pdf module on a freshly scanned doc
************************************************************************************************************************)
let home = Sys.getenv "HOME" ;;
let workdir = home^"/Downloads/Building_Site";;

let workdir = home^"/Downloads/Arno/Towards";;

Coherent_pdf.workspace_directory := workdir ;;

let cmd1 = "cp "^workdir^"/moncunill.pdf "^workdir^"/whole.pdf";;

let act1 () = Sys.command cmd1;;

let act2 () = Coherent_pdf.remove_page_range_in_in_a_total_of 
~range_start:1 ~range_end:1 ~deflated_one:"whole" 
~total_length:28;;

let act3 () = Coherent_pdf.append_on_the_right "whole" "velaza" ;;

let act5 () = Coherent_pdf.transfer_range_to_rightmost
~range_start:91 ~range_end:91 ~deflated_one:"whole" 
~total_length:592 ~receiving_one:"p90" ;;

let act6 ()= Coherent_pdf.insert_in_just_after 
    ~inserted_one: "p90" ~receiving_one: "whole"
     ~page_number:89 ~initial_total_length:591;;

let act7 () = Coherent_pdf.delete_file "p90";;     

let act8 k = Coherent_pdf.replace_page_number_in_by 
~page_number:k ~receiving_one:"whole"  ~inserted_one:("p"^(string_of_int k)) 
~total_length:133;;

Image.image act8 [36;40;42;44;68;70;72;90;100] ;;

let act9 () = Coherent_pdf.extract_page_range "whole" (10,10);;

let act10 () = Coherent_pdf.extract_odd_pages "russia" ;;

let act11 () = Coherent_pdf.intertwine 
~odd_pages:"odd" ~even_pages:"even" 
~num_odd:81  ~num_even:81 ~final_name:"118_to_279" ;;

let phoebe n=
  let q =(n/4) in 
  let r= n - 4*q in 
  (4*q)+List.assoc r [0,(-3);1,2;2,3;3,4;];; 
    
let act12 () =   
   Explicit.image (
     fun k-> 
      let j = phoebe k in 
      Coherent_pdf.rename 
      ("p"^(string_of_int k)) ("q"^(string_of_int j))
   ) (Int_range.range 1 260) ;;

let act13 ()= Coherent_pdf.implode ("q","") ;; 

let workdir = home^"/Downloads";;

Coherent_pdf.workspace_directory := workdir ;;

let act1 () = Coherent_pdf.extract_page_range "mariage" (24,24);;

let act2 ()= Coherent_pdf.insert_in_just_after 
    ~inserted_one: "xx" ~receiving_one: "mariage"
     ~page_number:22 ~initial_total_length:732;;

let act3 () = Coherent_pdf.remove_page_range_in_in_a_total_of 
~range_start:25 ~range_end:25 ~deflated_one:"mariage" 
~total_length:733;;

let act4 ()= Coherent_pdf.insert_in_just_after 
    ~inserted_one: "p272" ~receiving_one: "mariage"
     ~page_number:315 ~initial_total_length:732;;

