(*

#use"lib/Szemeredi/sz3_preliminaries.ml";;

We make an exception to the rule of not having numbers in module names.
Sz3 is short for "third stab at Szemeredi problem".

*)


type breadth = Sz3_types.breadth = B of int ;;
type size = Sz3_types.size = S of int ;;

type point = Sz3_types.point 
  =Empty_point | P of int * int list * breadth * size ;;
type constraint_t = Sz3_types.constraint_t = C of int list ;; 
type extension_data =  int list ;;
type qualified_point = Sz3_types.qualified_point = Q of point * constraint_t list * extension_data ;;
type solution = int list ;;
type mold = Sz3_types.mold = M of solution list * qualified_point list ;;
type superficial_result = Sz3_types.superficial_result = 
     Atomic
   | Decomposable of point * extension_data
   | Contraction of point * constraint_t
   | Fork of (point * extension_data) list ;; 
type bulk_result = Sz3_types.bulk_result = BR of superficial_result * mold ;; 
type half = Sz3_types.half = Lower_half | Upper_half ;;
type kind_of_component = Sz3_types.kind_of_component = 
     Superficial_result
    |Solution_list 
    |Qpl_length 
    |Qpe_core
    |Qpe_constraints
    |Qpe_extension ;; 
type index_of_missing_data = Sz3_types.index_of_missing_data = IMD of int ;;


let i_order = Total_ordering.for_integers ;;
let i_insert = Ordered.insert i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_intersects = Ordered.intersects i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_setminus = Ordered.setminus i_order ;;


let il_order = Total_ordering.silex_for_intlists ;;
let il_fold_merge = Ordered.fold_merge il_order ;;
let il_is_included_in = Ordered.is_included_in il_order ;;
let il_merge = Ordered.merge il_order ;;
let il_sort = Ordered.sort il_order ;;

let t_order = Total_ordering.triple_product 
   i_order i_order (Total_ordering.silex_for_intlists) ;;


module Parameter_pair_for_obstruction = struct 

  let predecessor max_in_set (width,breadth) = 
    if breadth < 1 
    then (if width < 2 then None else Some(width-1,max_in_set-2*(width-1)) )  
    else (Some(width,breadth-1)) ;;
    
  let check_for_meaningful_obstruction (width,breadth) domain =
     if breadth < 1 
     then false 
     else Ordered.is_included_in 
           Total_ordering.for_integers 
         [breadth;breadth+width;breadth+2*width] domain ;;  
  
end ;;  

module Finite_int_set = struct 

  let of_pair ( S n,scrappers) = i_setminus (Int_range.range 1 n) scrappers ;; 
  
  let to_pair domain =
       if domain = [] then (S 0,[]) else 
       let n = List.hd(List.rev domain) in 
       (S n,i_setminus (Int_range.range 1 n) domain) ;;   
  
end ;;    

module Kind_of_component = struct 

let all = [
  Superficial_result;
  Solution_list;
  Qpl_length;
  Qpe_core;
  Qpe_constraints;
  Qpe_extension
] ;; 

let to_capitalized_string = function 
  Superficial_result -> "Superficial_result"
| Solution_list -> "Solution_list"
| Qpl_length -> "Qpl_length"
| Qpe_core -> "Qpe_core"
| Qpe_constraints -> "Qpe_constraints"
| Qpe_extension -> "Qpe_extension" ;; 

let to_uncapitalized_string koc = 
    String.uncapitalize_ascii (to_capitalized_string koc) ;; 

let of_string s = 
  List.assoc s 
  (Image.image (fun k->(to_capitalized_string k,k)) all);;
  
let compare = Total_ordering.from_list all ;;

end ;;   

module Half = struct 

let to_string = function 
    Lower_half -> "lower_half" 
   |Upper_half -> "upper_half" ;; 

let of_string s = 
    List.assoc s 
    [
      "Lower_half",Lower_half;
      "Upper_half",Upper_half;
    ] ;; 

let compare = Total_ordering.from_list [Lower_half;Upper_half] ;; 

end ;;  


module Point = struct 
  
   exception Empty_point_cannot_be_unveiled ;; 
  let unveil =function 
   Empty_point -> raise(Empty_point_cannot_be_unveiled) 
   |P(w,s,b,n) ->  (w,s,b,n) ;; 
  let width p = let (w,_,_,_) = unveil p in w ;; 
  let scrappers p = let (_,s,_,_) = unveil p in s ;;   
  let breadth p = let (_,_,b,_) = unveil p in b ;;  
  let size p = let (_,_,n,_) = unveil p in n ;;   
  let enumerate_supporting_set = function
     Empty_point -> []
    |P(_w,s,_b, n) -> Finite_int_set.of_pair (n,s) ;; 
  let is_in_upper_half p= 
    let (w,_,B(b),S(n)) = unveil p in 
    n >= b + 2*w ;; 


end ;;  

  
module Simplest_reduction = struct 

  module For_nonparametrized_sets = struct 

    module Private = struct 
    
    let rec iterator_for_meaningful_obstruction (domain,max_in_domain,w,b) =
      if Parameter_pair_for_obstruction.check_for_meaningful_obstruction (w,b) domain 
      then Some(w,b)
      else
      match Parameter_pair_for_obstruction.predecessor max_in_domain (w,b) with 
       None -> None  
       |Some(new_w,new_b) -> iterator_for_meaningful_obstruction (domain,max_in_domain,new_w,new_b) ;;
    
    let find_meaningful_obstruction (w,b) domain = 
       if domain = [] then None else 
       let max_in_domain = List.hd(List.rev domain) in 
       iterator_for_meaningful_obstruction (domain,max_in_domain,w,b) ;; 
    
    let inner_test_for_detachability width breadth domain x w = 
        if not(i_is_included_in [x-2*w;x-w] domain)
        then true
        else if w<width 
             then false
             else breadth < (x-2*w)  ;;   
    
    let test_for_detachability width breadth domain x = 
      let idx_range = Int_range.range 1 (min (width)((x-1)/2))  in 
        List.for_all (inner_test_for_detachability width breadth domain x) idx_range ;;
              
    let rec iterator_for_detachment (width,breadth,domain,treated,to_be_treated) = 
        match to_be_treated with 
         [] -> ([],treated)
        | x :: others -> 
           if test_for_detachability width breadth domain x 
           then iterator_for_detachment (width,breadth,domain,x::treated,others)
           else (List.rev to_be_treated,treated);;    
    
    let detach (width,breadth) domain = iterator_for_detachment (width,breadth,domain,[],List.rev domain) ;;
    
    end ;;
    
    let decompose (old_width,old_breadth) domain = 
      if (old_width,old_breadth)=(1,0) then None else 
      match Private.find_meaningful_obstruction (old_width,old_breadth) domain with 
        None -> None
        | Some (width,breadth) -> Some((width,breadth),Private.detach (width,breadth) domain);;  
    
    
    end ;;  


  let decompose pt =
      let (old_width,scrappers,B old_breadth,n) = Point.unveil pt in 
      let domain = Finite_int_set.of_pair (n,scrappers) in 
      match For_nonparametrized_sets.decompose (old_width,old_breadth) domain with
      None -> (Empty_point,domain)
    | (Some((new_width,new_breadth),(new_domain,adjustment))) -> 
       let (new_n,new_scrappers) = Finite_int_set.to_pair new_domain in 
        (P(new_width,new_scrappers,B new_breadth,new_n),adjustment);;
    
(*
   
let check1 = (decompose (P(1,4,6,[])) =  (P (1, 4, 6, []), [])) ;;
let check2 = (decompose (P(1,3,6,[])) =  (P (1, 3, 5, []), [6])) ;;

*)

end ;;  


module Warehouse = struct 
  
  exception Bad_kmp_index of int ;; 

  let access_named_hashtbl (error_handling,hashtbl) pt = 
    let (width,scrappers,breadth,size) = Point.unveil pt in 
    match Hashtbl.find_opt hashtbl (width,scrappers) with 
    None -> (None,Some (error_handling,IMD 0)) 
    | Some f -> (Some(f breadth size),None) ;; 

  let access_parametrized_named_hashtbl (error_handling,hashtbl) k pt = 
    let (width,scrappers,breadth,size) = Point.unveil pt in 
    match Hashtbl.find_opt hashtbl (width,scrappers,k) with 
    None -> (None,Some (error_handling,k)) 
    | Some f -> (Some(f breadth size),None) ;; 
  
    
  
  let hashtbl_for_superficial_result_lower_half = ((Hashtbl.create 50) : (int * int list, breadth -> size -> superficial_result) Hashtbl.t) ;;
  let try_get_superficial_result_lower_half = 
     access_named_hashtbl ((Superficial_result,Lower_half),
        hashtbl_for_superficial_result_lower_half) ;; 
  
  let hashtbl_for_superficial_result_upper_half = ((Hashtbl.create 50) : (int * int list, breadth -> size -> superficial_result) Hashtbl.t) ;;
  let try_get_superficial_result_upper_half = 
      access_named_hashtbl ((Superficial_result,Upper_half),
        hashtbl_for_superficial_result_upper_half) ;; 

  let hashtbl_for_solution_list_lower_half = ((Hashtbl.create 50) : (int * int list, breadth -> size -> solution list) Hashtbl.t) ;;
  let try_get_solution_list_lower_half = 
      access_named_hashtbl ((Solution_list,Lower_half),
        hashtbl_for_solution_list_lower_half) ;; 

  let hashtbl_for_solution_list_upper_half = ((Hashtbl.create 50) : (int * int list, breadth -> size -> solution list) Hashtbl.t) ;;
  let try_get_solution_list_upper_half = 
      access_named_hashtbl ((Solution_list,Upper_half),
        hashtbl_for_solution_list_upper_half) ;; 

  let hashtbl_for_qpl_length_lower_half = ((Hashtbl.create 50) : (int * int list, breadth -> size -> int) Hashtbl.t) ;;
  let try_get_qpl_length_lower_half = 
    access_named_hashtbl ((Qpl_length,Lower_half),
      hashtbl_for_qpl_length_lower_half) ;; 

  let hashtbl_for_qpl_length_upper_half = ((Hashtbl.create 50) : (int * int list, breadth -> size -> int) Hashtbl.t) ;;
  let try_get_qpl_length_upper_half = 
    access_named_hashtbl ((Qpl_length,Upper_half),
      hashtbl_for_qpl_length_upper_half) ;; 
      
  let hashtbl_for_qpe_core_lower_half = ((Hashtbl.create 50) : (int * int list * index_of_missing_data, breadth -> size -> point) Hashtbl.t) ;;
  let try_get_qpe_core_lower_half = 
   access_parametrized_named_hashtbl ((Qpe_core,Lower_half),
      hashtbl_for_qpe_core_lower_half) ;; 

  let hashtbl_for_qpe_core_upper_half = ((Hashtbl.create 50) : (int * int list * index_of_missing_data, breadth -> size -> point) Hashtbl.t) ;;
  let try_get_qpe_core_upper_half = 
    access_parametrized_named_hashtbl ((Qpe_core,Upper_half),
      hashtbl_for_qpe_core_upper_half) ;; 
      
        
  let hashtbl_for_qpe_constraints_lower_half = ((Hashtbl.create 50) : (int * int list * index_of_missing_data, breadth -> size -> constraint_t list) Hashtbl.t) ;;
  let try_get_qpe_constraints_lower_half = 
    access_parametrized_named_hashtbl ((Qpe_constraints,Lower_half),
      hashtbl_for_qpe_constraints_lower_half) ;; 

  let hashtbl_for_qpe_constraints_upper_half = ((Hashtbl.create 50) : (int * int list * index_of_missing_data, breadth -> size -> constraint_t list) Hashtbl.t) ;;
  let try_get_qpe_constraints_upper_half = 
    access_parametrized_named_hashtbl ((Qpe_constraints,Upper_half),
      hashtbl_for_qpe_constraints_upper_half) ;; 

  let hashtbl_for_qpe_extension_lower_half = ((Hashtbl.create 50) : (int * int list * index_of_missing_data, breadth -> size -> extension_data) Hashtbl.t) ;;
  let try_get_qpe_extension_lower_half = 
    access_parametrized_named_hashtbl ((Qpe_extension,Lower_half),
      hashtbl_for_qpe_extension_lower_half) ;; 

  let hashtbl_for_qpe_extension_upper_half = ((Hashtbl.create 50) : (int * int list * index_of_missing_data, breadth -> size -> extension_data) Hashtbl.t) ;;
  let try_get_qpe_extension_upper_half = 
    access_parametrized_named_hashtbl ((Qpe_extension,Upper_half),
      hashtbl_for_qpe_extension_upper_half) ;; 

  let qualified_point_core half k pt = match half with 
    Lower_half -> try_get_qpe_core_lower_half k pt 
   |Upper_half -> try_get_qpe_core_upper_half k pt;;

  let qualified_point_constraints half k pt = match half with 
   Lower_half -> try_get_qpe_constraints_lower_half k pt 
  |Upper_half -> try_get_qpe_constraints_upper_half k pt;;


  let qualified_point_extension half k pt = match half with 
    Lower_half -> try_get_qpe_extension_lower_half k pt 
   |Upper_half -> try_get_qpe_extension_upper_half k pt;;

  let qpl_length half pt = match half with 
    Lower_half -> try_get_qpl_length_lower_half pt 
   |Upper_half -> try_get_qpl_length_upper_half pt;;

  let clear_all_hashtables () = 
(
  Hashtbl.clear hashtbl_for_superficial_result_lower_half;
  Hashtbl.clear hashtbl_for_superficial_result_upper_half; 
  Hashtbl.clear hashtbl_for_solution_list_lower_half; 
  Hashtbl.clear hashtbl_for_solution_list_upper_half; 
  Hashtbl.clear hashtbl_for_qpl_length_lower_half; 
  Hashtbl.clear hashtbl_for_qpl_length_upper_half; 
  Hashtbl.clear hashtbl_for_qpe_core_lower_half; 
  Hashtbl.clear hashtbl_for_qpe_core_upper_half; 
  Hashtbl.clear hashtbl_for_qpe_constraints_lower_half; 
  Hashtbl.clear hashtbl_for_qpe_constraints_upper_half; 
  Hashtbl.clear hashtbl_for_qpe_extension_lower_half ; 
  Hashtbl.clear hashtbl_for_qpe_extension_upper_half ; 
) ;; 

  let qualified_point_element half k pt = 
    let (good_opt1,bad_opt1) = qualified_point_core half k pt in 
    if bad_opt1<>None then (None,bad_opt1) else 
    let (good_opt2,bad_opt2) = qualified_point_constraints half k pt in 
    if bad_opt2<>None then (None,bad_opt2) else   
    let (good_opt3,bad_opt3) = qualified_point_extension half k pt in 
    if bad_opt3<>None then (None,bad_opt3) else     
    let core_r = Option.get good_opt1 
    and constraints_r = Option.get good_opt2 
    and extension_r = Option.get good_opt3 in 
    (Some(Q(core_r,constraints_r,extension_r)),None) ;;   
  
  let superficial_result half pt = match half with 
     Lower_half -> try_get_superficial_result_lower_half pt 
    |Upper_half -> try_get_superficial_result_upper_half pt;;
     
  let solution_list half pt = match half with 
    Lower_half -> try_get_solution_list_lower_half pt 
   |Upper_half -> try_get_solution_list_upper_half pt;;
  
   
  let qualified_point_list half pt =
    let (good_opt1,bad_opt1) =qpl_length half pt in 
    if bad_opt1<>None then (None,bad_opt1) else 
  let length_r = Option.get good_opt1 in 
  let eltwise_results = Int_range.scale (
          fun k-> qualified_point_element half (IMD k) pt
  )  1 length_r in  
  let bad_ones = List.filter (
        fun (good_opt,_bad_opt) -> good_opt = None
  ) eltwise_results in 
  if bad_ones <> [] 
  then (None,snd(List.hd bad_ones)) 
  else 
  let final_result = Image.image (fun (good_opt,_bad_opt) ->Option.get good_opt) eltwise_results in 
  (Some final_result,None) ;;  


  let bulk_result half pt = 
     let (good_opt1,bad_opt1) = superficial_result half pt in 
     if bad_opt1<>None then (None,bad_opt1) else 
     let (good_opt2,bad_opt2) = solution_list half pt in 
     if bad_opt2<>None then (None,bad_opt2) else  
     let (good_opt3,bad_opt3) = qualified_point_list half pt in 
     if bad_opt3<>None then (None,bad_opt3) else  
     let superficial_result_r = Option.get good_opt1 
     and solution_list_r = Option.get good_opt2 
     and qualified_point_list_r = Option.get good_opt3  in     
     (Some(BR(superficial_result_r,M(solution_list_r,qualified_point_list_r))),None)
   ;;   

  let length_watcher pt = 
    if Point.is_in_upper_half pt 
    then Option.get(fst(qpl_length Upper_half pt)) 
    else Option.get(fst(qpl_length Lower_half pt)) ;;    

  let nonhalved_bulk_result pt = 
    if Point.is_in_upper_half pt 
    then bulk_result Upper_half pt 
    else bulk_result Lower_half pt ;;    

  let try_precomputed_results pt = fst(nonhalved_bulk_result pt) ;;

end ;;  


module Tools_for_warehouse = struct 

let simplest_example n = List.filter (fun j->(j mod 3)<>0) (Int_range.range 1 n) ;; 
let simplest_list n = [simplest_example n] ;; 


end ;;  

module Fill_Warehouse = struct 
(* Beginning of warehouse fillings. Do not modify this line *)

open Tools_for_warehouse ;; 

(* Beginning of item at  (1,[],IMD(0),Superficial_result,Lower_half) *)
let f_1_empty_set_superficial_result_lower_half (B _b) (S n) =
    match List.assoc_opt n 
       [1,Atomic;
        2,Atomic;
        3,Fork [(Empty_point, [2; 3]); (Empty_point, [1; 3]); (Empty_point, [1; 2])]]  with    
      Some answer -> answer 
   | None ->
    (match (n mod 3) with 
    0 -> Fork([( P(1,[],B(n-5),S(n-3)),[n-1;n] );
               ( P(1,[],B(n-4),S(n-2)),[n] );
               ( P(1,[],B(n-3),S(n-1)),[] )])
   |1-> Contraction( P(1,[],B(n-3),S(n)),C[n-2;n-1;n] )  
   |2-> Contraction( P(1,[],B(n-3),S(n)),C[n-2;n-1;n] ) 
   |_-> failwith("bad remainder by 3")) ;;       
  
(*
  
  Abstract_superficial_result_mode.global_check 
   (1,[],IMD 0,Lower_half) f_1_empty_set_superficial_result_lower_half ;; 
  
*)

Hashtbl.add 
  Warehouse.hashtbl_for_superficial_result_lower_half (1,[]) 
    f_1_empty_set_superficial_result_lower_half;;
(* End of item at  (1,[],IMD(0),Superficial_result,Lower_half) *)

(* Beginning of item at  (1,[],IMD(0),Superficial_result,Upper_half) *)
let f_1_empty_set_superficial_result_upper_half (B b) (S n) = 
  if b=0 then Atomic else
  if n=3 then Fork [(Empty_point, [2; 3]); (Empty_point, [1; 3]); (Empty_point, [1; 2])] else 
  if n=b+2 
  then (match (n mod 3) with 
            0 -> Fork([( P(1,[],B(n-5),S(n-3)),[n-1;n] );
                       ( P(1,[],B(n-4),S(n-2)),[n] );
                       ( P(1,[],B(n-3),S(n-1)),[] )])
           |1-> Contraction( P(1,[],B(n-3),S(n)),C[n-2;n-1;n] )  
           |2-> Contraction( P(1,[],B(n-3),S(n)),C[n-2;n-1;n] ) 
           |_-> failwith("bad remainder by 3")) 
  else Decomposable (P (1, [], B (b), S (b+2)), Int_range.range (b+3) n) ;;   
    
   
(*
   
   Abstract_superficial_result_mode.global_check 
   (1,[],IMD 0,Upper_half) f_1_empty_set_superficial_result_upper_half ;; 
   
*)
   
Hashtbl.add 
  Warehouse.hashtbl_for_superficial_result_upper_half (1,[]) 
     f_1_empty_set_superficial_result_upper_half;;

(* End of item at  (1,[],IMD(0),Superficial_result,Upper_half) *)     

(* Beginning of item at  (1,[],IMD(0),Solution_list,Lower_half) *)
let f_1_empty_set_solution_list_lower_half (B _b) (S n) = simplest_list n ;;       
    
(*
    
  Abstract_solution_list_mode.global_check 
   (1,[],IMD 0,Lower_half) f_1_empty_set_solution_list_lower_half ;; 
  
    
*)
    
Hashtbl.add 
  Warehouse.hashtbl_for_solution_list_lower_half (1,[]) 
    f_1_empty_set_solution_list_lower_half;;

(* End of item at  (1,[],IMD(0),Solution_list,Lower_half) *)
(* Beginning of item at  (1,[],IMD(0),Solution_list,Upper_half) *)

let f_1_empty_solution_list_upper_half (B b) (S n) = 
  if List.mem (n-b,n mod 3) [(2,0);(2,1);(2,2);(3,1);(3,2);(4,2)] 
  then simplest_list n  
  else [(simplest_example (b+2))@(Int_range.range (b+3) n)] ;;

(* 

   Abstract_solution_list_mode.global_check
    (1,[],IMD(0),Upper_half) f_1_empty_solution_list_upper_half ;; 

*)

Hashtbl.add
 Warehouse.hashtbl_for_solution_list_upper_half
   (1,[]) f_1_empty_solution_list_upper_half ;;

(* End of item at  (1,[],IMD(0),Solution_list,Upper_half) *)


(* Beginning of item at  (1,[],IMD(0),Qpl_length,Lower_half) *)
let f_1_empty_set_qpl_length_lower_half (B _b) (S n) = 
  match List.assoc_opt n [1,0;2,0]  with    
    Some answer -> answer 
  | None ->
    (match (n mod 3) with 
       0 -> 3
      |1-> 2   
      |2-> 1  
      |_-> failwith("bad remainder by 3"));; 


(*

Abstract_qpl_length_mode.global_check 
   (1,[],IMD 0,Lower_half) f_1_empty_set_qpl_length_lower_half ;; 
  

*)

Hashtbl.add 
 Warehouse.hashtbl_for_qpl_length_lower_half (1,[]) 
f_1_empty_set_qpl_length_lower_half;;
(* End of item at  (1,[],IMD(0),Qpl_length,Lower_half) *)
(* Beginning of item at  (1,[],IMD(0),Qpl_length,Upper_half) *)

let f_1_empty_qpl_length_upper_half (B b) (S _n) = 
  if b=0 then 0 else
  List.assoc (b mod 3) [0,1;1,3;2,2] ;;

(* 

   Abstract_qpl_length_mode.global_check
    (1,[],IMD(0),Upper_half) f_1_empty_qpl_length_upper_half ;; 

*)

Hashtbl.add
 Warehouse.hashtbl_for_qpl_length_upper_half
   (1,[]) f_1_empty_qpl_length_upper_half ;;

(* End of item at  (1,[],IMD(0),Qpl_length,Upper_half) *)
(* Beginning of item at  (1,[],IMD(1),Qpe_core,Lower_half) *)

let f_1_empty_i1_qpe_core_lower_half (B _b) (S n) = 
  if n<=5 then Empty_point else
    P(1,[],B(n-5),S(n-3));;

(* 

   Abstract_qpe_core_mode.global_check
    (1,[],IMD(1),Lower_half) f_1_empty_i1_qpe_core_lower_half ;; 

*)

Hashtbl.add
 Warehouse.hashtbl_for_qpe_core_lower_half
   (1,[],IMD(1)) f_1_empty_i1_qpe_core_lower_half ;;

(* End of item at  (1,[],IMD(1),Qpe_core,Lower_half) *)
(* Beginning of item at  (1,[],IMD(1),Qpe_core,Upper_half) *)

let f_1_empty_i1_qpe_core_upper_half (B b) (S _n) = 
  if b<=3 then Empty_point else
    P(1,[],B(b-3),S(b-1))  ;;

(* 

   Abstract_qpe_core_mode.global_check
    (1,[],IMD(1),Upper_half) f_1_empty_i1__qpe_core_upper_half ;; 

*)

Hashtbl.add
 Warehouse.hashtbl_for_qpe_core_upper_half
   (1,[],IMD(1)) f_1_empty_i1_qpe_core_upper_half ;;

(* End of item at  (1,[],IMD(1),Qpe_core,Upper_half) *)
(* Beginning of item at  (1,[],IMD(1),Qpe_constraints,Lower_half) *)

let f_1_empty_i1_qpe_constraints_lower_half (B _b) (S _n) = 
  [];;

(* 

   Abstract_qpe_constraints_mode.global_check
    (1,[],IMD(1),Lower_half) f_1_empty_i1_qpe_constraints_lower_half ;; 

*)

Hashtbl.add
 Warehouse.hashtbl_for_qpe_constraints_lower_half
   (1,[],IMD(1)) f_1_empty_i1_qpe_constraints_lower_half ;;

(* End of item at  (1,[],IMD(1),Qpe_constraints,Lower_half) *)
(* Beginning of item at  (1,[],IMD(1),Qpe_constraints,Upper_half) *)

let f_1_empty_i1_qpe_constraints_upper_half (B _b) (S _n) = [] ;;

(* 

   Abstract_qpe_constraints_mode.global_check
    (1,[],IMD(1),Upper_half) f_1_empty_i1_qpe_constraints_upper_half ;; 

*)

Hashtbl.add
 Warehouse.hashtbl_for_qpe_constraints_upper_half
   (1,[],IMD(1)) f_1_empty_i1_qpe_constraints_upper_half ;;

(* End of item at  (1,[],IMD(1),Qpe_constraints,Upper_half) *)
(* Beginning of item at  (1,[],IMD(1),Qpe_extension,Lower_half) *)

let f_1_empty_i1_qpe_extension_lower_half (B _b) (S n) = 
  match List.assoc_opt n
  [4,[1;3;4];5,[1;2;4;5]] with 
  Some answer -> answer 
  |None -> Int_range.range (n-1) n ;;

(* 

   Abstract_qpe_extension_mode.global_check
    (1,[],IMD(1),Lower_half) f_1_empty_i1_qpe_extension_lower_half ;; 

*)

Hashtbl.add
 Warehouse.hashtbl_for_qpe_extension_lower_half
   (1,[],IMD(1)) f_1_empty_i1_qpe_extension_lower_half ;;

(* End of item at  (1,[],IMD(1),Qpe_extension,Lower_half) *)
(* Beginning of item at  (1,[],IMD(1),Qpe_extension,Upper_half) *)

let f_1_empty_i1_qpe_extension_upper_half (B b) (S n) = 
  match List.assoc_opt b
  [2,1::(Int_range.range 3 (n-b+2));3,[1;2]@(Int_range.range 4 (n-b+3))] with 
  Some answer -> answer 
  |None -> Int_range.range (b+1) n ;;

(* 

   Abstract_qpe_extension_mode.global_check
    (1,[],IMD(1),Upper_half) f_1_empty_i1_qpe_extension_upper_half ;; 

*)

Hashtbl.add
 Warehouse.hashtbl_for_qpe_extension_upper_half
   (1,[],IMD(1)) f_1_empty_i1_qpe_extension_upper_half ;;

(* End of item at  (1,[],IMD(1),Qpe_extension,Upper_half) *)
(* Beginning of item at  (1,[],IMD(2),Qpe_core,Lower_half) *)

let f_1_empty_i2_qpe_core_lower_half (B _b) (S n) = 
  if n<=4 then Empty_point else
    P(1,[],B(n-4),S(n-2));;

(* 

   Abstract_qpe_core_mode.global_check
    (1,[],IMD(2),Lower_half) f_1_empty_i2_qpe_core_lower_half ;; 

*)

Hashtbl.add
 Warehouse.hashtbl_for_qpe_core_lower_half
   (1,[],IMD(2)) f_1_empty_i2_qpe_core_lower_half ;;

(* End of item at  (1,[],IMD(2),Qpe_core,Lower_half) *)
(* Beginning of item at  (1,[],IMD(2),Qpe_core,Upper_half) *)

let f_1_empty_i2_qpe_core_upper_half (B b) (S _n) = 
  if b<=2 then Empty_point else 
    P(1,[],B(b-2),S(b));;

(* 

   Abstract_qpe_core_mode.global_check
    (1,[],IMD(2),Upper_half) f_1_empty_i2_qpe_core_upper_half ;; 

*)

Hashtbl.add
 Warehouse.hashtbl_for_qpe_core_upper_half
   (1,[],IMD(2)) f_1_empty_i2_qpe_core_upper_half ;;

(* End of item at  (1,[],IMD(2),Qpe_core,Upper_half) *)
(* Beginning of item at  (1,[],IMD(2),Qpe_constraints,Upper_half) *)

let f_1_empty_i2_qpe_constraints_upper_half (B _b) (S _n) = [];;

(* 

   Abstract_qpe_constraints_mode.global_check
    (1,[],IMD(2),Upper_half) f_1_empty_i2_qpe_constraints_upper_half ;; 

*)

Hashtbl.add
 Warehouse.hashtbl_for_qpe_constraints_upper_half
   (1,[],IMD(2)) f_1_empty_i2_qpe_constraints_upper_half ;;

(* End of item at  (1,[],IMD(2),Qpe_constraints,Upper_half) *)
(* Beginning of item at  (1,[],IMD(2),Qpe_extension,Upper_half) *)

let f_1_empty_i2_qpe_extension_upper_half (B b) (S n) = 
  match List.assoc_opt b
  [1,1::(Int_range.range 3 (n-b+1));2,[1;2]@(Int_range.range 4 (n-b+2))] with 
  Some answer -> answer 
  |None -> Int_range.range (b+2) n ;;

(* 

   Abstract_qpe_extension_mode.global_check
    (1,[],IMD(2),Upper_half) f_1_empty_i2_qpe_extension_upper_half ;; 

*)

Hashtbl.add
 Warehouse.hashtbl_for_qpe_extension_upper_half
   (1,[],IMD(2)) f_1_empty_i2_qpe_extension_upper_half ;;

(* End of item at  (1,[],IMD(2),Qpe_extension,Upper_half) *)
(* Beginning of item at  (1,[],IMD(3),Qpe_core,Upper_half) *)

let f_1_empty_i3_qpe_core_upper_half (B b) (S _n) = 
  if b=1 then Empty_point else
    P(1,[],B(b-1),S(b+1));;

(* 

   Abstract_qpe_core_mode.global_check
    (1,[],IMD(3),Upper_half) f_1_empty_i3_qpe_core_upper_half ;; 

*)

Hashtbl.add
 Warehouse.hashtbl_for_qpe_core_upper_half
   (1,[],IMD(3)) f_1_empty_i3_qpe_core_upper_half ;;

(* End of item at  (1,[],IMD(3),Qpe_core,Upper_half) *)
(* Beginning of item at  (1,[],IMD(3),Qpe_constraints,Upper_half) *)

let f_1_empty_i3_qpe_constraints_upper_half (B _b) (S _n) = 
  [];;

(* 

   Abstract_qpe_constraints_mode.global_check
    (1,[],IMD(3),Upper_half) f_1_empty_i3_qpe_constraints_upper_half ;; 

*)

Hashtbl.add
 Warehouse.hashtbl_for_qpe_constraints_upper_half
   (1,[],IMD(3)) f_1_empty_i3_qpe_constraints_upper_half ;;

(* End of item at  (1,[],IMD(3),Qpe_constraints,Upper_half) *)
(* Beginning of item at  (1,[],IMD(3),Qpe_extension,Upper_half) *)

let f_1_empty_i3_qpe_extension_upper_half (B b) (S n) = 
  match List.assoc_opt b
  [1,[1;2]@(Int_range.range 4 (n-b+1))] with 
  Some answer -> answer 
  |None -> Int_range.range (b+3) n ;;

(* 

   Abstract_qpe_extension_mode.global_check
    (1,[],IMD(3),Upper_half) f_1_empty_i3_qpe_extension_upper_half ;; 

*)

Hashtbl.add
 Warehouse.hashtbl_for_qpe_extension_upper_half
   (1,[],IMD(3)) f_1_empty_i3_qpe_extension_upper_half ;;

(* End of item at  (1,[],IMD(3),Qpe_extension,Upper_half) *)

(* End of warehouse fillings. Do not modify this line *)
end ;;   

module Constraint = struct  

  let satisfied_by_individual l_constr l =
    List.for_all (fun (C constr)->not(i_is_included_in constr l)) l_constr
  
  let satisfied_by_all_in_list l_constr ll=
    List.for_all (satisfied_by_individual l_constr) ll ;;
  
  let merge_constraints l_constr1 l_constr2 = 
      let simplifier = Image.image (fun (C x)->x) in
      Image.image (fun x->C x)
      (Ordered_misc.minimal_elts_wrt_inclusion (il_merge 
       (simplifier l_constr1) (simplifier l_constr2))) ;;
  
  let insert_new domain (old_constraints,extension) (C new_constraint)= 
    let remaining_constraint = i_setminus new_constraint extension in 
    if remaining_constraint = [] 
    then None 
    else 
    if (i_setminus remaining_constraint domain)<>[] 
    then Some (old_constraints)    
    else Some (merge_constraints [C remaining_constraint] old_constraints) ;;  
     
  let insert_several  domain (old_constraints,extension) new_constraints =
     let rec tempf = (
        fun (constraints_walker,to_be_treated) ->
           match to_be_treated with 
           [] -> Some constraints_walker 
           | new_constraint :: others ->  
          (match  insert_new domain (constraints_walker,extension) new_constraint with    
             None -> None 
            | Some new_walker -> tempf(new_walker,others) 
          )
     ) in 
     tempf(old_constraints,new_constraints);;

  end ;;  


module Qualified_point = struct 

    let extend_with qp extension =  
      match qp with 
      Q(pt,old_constraints,extension2) -> 
      Q(pt,old_constraints,i_merge extension extension2)   ;;
     
    
    let insert_several_constraints new_constraints (Q(pt,old_constraints,extension)) =
      (match Constraint.insert_several (Point.enumerate_supporting_set pt) (old_constraints,extension) new_constraints 
      with
        None -> None 
       |(Some final_constraints) ->  Some((Q(pt,final_constraints,extension)))) ;; 
    
    
end ;;  



module Mold = struct 

  type t = Sz3_types.mold = M of solution list * qualified_point list ;;

  (* it is assumed that compatibility has already been checked *)   
  let extend_with (M(reps,qpoints)) extension =
    M(Image.image (i_merge extension) reps,
    Image.image (fun qpoint->Qualified_point.extend_with qpoint extension) qpoints
    ) ;;  
  
  let insert_several_constraints extra_constraints (M(reps,qpoints)) = 
    M(List.filter (Constraint.satisfied_by_individual extra_constraints) reps,
       List.filter_map (
        Qualified_point.insert_several_constraints extra_constraints
       ) qpoints) ;; 
    
  exception Insert_several_constraints_carefully_exn of constraint_t list * t ;;
  
  let insert_several_constraints_carefully extra_constraints old_mold =
     let new_mold = insert_several_constraints extra_constraints old_mold in 
     let (M(new_reps,new_qpoints)) = new_mold in     
      if new_qpoints = [] 
      then None 
      else
      if new_reps = []
      then raise(Insert_several_constraints_carefully_exn(extra_constraints,old_mold))
      else Some new_mold ;;          
  
  
end ;;
  
module Bulk_result = struct     

  type t = Sz3_types.bulk_result = BR of superficial_result * mold ;;

  let atomic_case pt = BR (Atomic,M([Point.enumerate_supporting_set pt],[])) ;; 
  
  let is_not_atomic (BR(sr,_)) = sr <> Atomic ;; 
  
  let superficial_part (BR(sr,_)) = sr ;; 
  let solution_list (BR(_,md)) = let (M(reps,_qpoints)) = md in reps ;; 
  let qualified_points (BR(_,md)) = let (M(_reps,qpoints)) = md in qpoints ;; 
  let mold (BR(_,md)) = md ;; 
  
  let extend_with pt (BR(old_sr,mold)) extension = 
   let new_sr = (if extension <> []
   then Decomposable(pt,extension)
   else old_sr) in
   BR(new_sr,Mold.extend_with mold extension);;
  
  let extend_with_opt pt bres_opt extension = match bres_opt with 
        None -> None 
        |Some bres -> Some (extend_with pt bres extension) ;;    
  
  let impose_one_more_constraint_opt pt cstr (BR(_sr,mold)) =
      match Mold.insert_several_constraints_carefully [cstr] mold with 
       None -> None
      | Some new_mold -> Some(BR(Contraction(pt,cstr),new_mold)) ;;
       
  
end ;;      
  
module Untamed = struct 
  
  let cil_order = ((fun (C x) (C y)->il_order x y) : constraint_t Total_ordering_t.t) ;;

  let test_for_admissibility_up_to_max_with max_width z =
    if max_width<1 then true else 
    Sz_preliminaries.test_for_admissibility (Sz_max_width_t.MW (max_width)) z ;;
  
  let test_for_admissiblity width breadth z =
     (test_for_admissibility_up_to_max_with (width-1) z)
     &&
     (List.for_all (fun t->
      not(i_is_included_in [t;t+width;t+2*width] z)) (Int_range.range 1 breadth))  ;;
  
  let remove_one_element (S n,scrappers) k=
    let new_scrappers = i_insert k scrappers in 
    if k <> n then (S n,new_scrappers) else 
    let new_z =  Finite_int_set.of_pair (S n,new_scrappers) in 
    let new_max = List.hd(List.rev new_z) in 
    (S new_max,List.filter (fun t->t<new_max) new_scrappers) ;;
  
  
  (*
  
  remove_one_element (10,[3;7;8;9]) 10 ;;
  
  *)
  
  
  
  
  
  let low_hashtbl = Hashtbl.create 50 ;;
    
  let access_opt  pt = 
   let (pt2,adj) = Simplest_reduction.decompose pt in 
   if pt2 = Empty_point 
   then Some (Bulk_result.atomic_case pt)
   else
   let pre_res=Warehouse.try_precomputed_results pt2 in 
   Bulk_result.extend_with_opt pt2 pre_res adj ;;
     
  let superificial_result_in_jump_case  pt_after_jump =
    let (width,scrappers,B _breadth, S n) = Point.unveil pt_after_jump in 
    let pt_before_jump = P(width-1,scrappers,B(n-2*(width-1)),S n) in  
    let (pt2,adj2) = Simplest_reduction.decompose pt_before_jump in 
    ([],Some(Decomposable(pt2,adj2))) ;; 
      
  let access_with_helper_opt pt helper =
      match List.assoc_opt pt helper with 
      Some answer -> Some answer
      | None ->  access_opt pt ;;
  
  let compute_superficial_result_partially pt helper =  
    if pt = Empty_point then ([],Some Atomic) else
    let (width,_scrappers,B breadth,_n) = Point.unveil pt in 
    let (pt2,adj2) = Simplest_reduction.decompose pt in 
    if ((width,breadth)=(1,0))||(pt2=Empty_point)
    then ([],Some Atomic)
    else 
    if adj2<>[]
    then ([],Some(Decomposable(pt2,adj2)))
    else     
    if breadth = 0
    then superificial_result_in_jump_case pt   
    else
    let (width2,scrappers2,B breadth2,n2) = Point.unveil pt2 in   
    let _ = assert(breadth2>0) in 
    let front_constraint = C [breadth2;breadth2+width2;breadth2+2*width2] 
    and preceding_point = P(width2,scrappers2,B(breadth2-1),n2) in 
    match access_with_helper_opt  preceding_point helper with 
      None -> ([preceding_point],None)
     |Some bres ->
         (match Bulk_result.impose_one_more_constraint_opt preceding_point front_constraint bres  with 
         None -> let tooths = Int_range.scale (fun k->
                  let (m,scr) = remove_one_element  (n2,scrappers2)  (breadth2+k*width2) in 
                  let pt3 = P(width2,scr,B(breadth2-1),m) in 
                  Simplest_reduction.decompose(pt3) 
                 ) 0 2  in 
                ([],Some(Fork tooths))
        |Some _bres2 -> ([],Some(Contraction(preceding_point,front_constraint)))) ;; 
  
  let fork_case_in_bulk_result_computation old_f cases helper = 
        let (last_pt,last_adj) = List.nth cases 2 in 
        let partial_res5 = old_f last_pt helper in 
        match snd partial_res5 with 
        None -> (fst partial_res5,None) 
       |Some br5 -> 
         let (BR(_,M(reps,_))) = Bulk_result.extend_with last_pt br5 last_adj in 
         let new_mold = M(reps,Image.image (
          fun (pt6,adj6)-> Q(pt6,[],adj6)
        ) cases) in 
        ([],Some (BR(Fork cases,new_mold))) ;; 
  
  exception Bad_contraction of point * constraint_t ;; 
  
  let rec compute_bulk_result_partially pt helper=  
    if pt = Empty_point then ([],Some(Bulk_result.atomic_case pt)) else 
     let partial_res1 = compute_superficial_result_partially pt helper in 
     match snd partial_res1 with 
      None -> (fst partial_res1,None) 
     |Some sr ->(match sr with 
       Atomic -> ([],Some(Bulk_result.atomic_case pt)) 
     | Decomposable(pt2,adj2) -> 
         let partial_res2 = compute_bulk_result_partially pt2 helper in 
         (
          match snd partial_res2 with 
          None -> (fst partial_res2,None) 
         |Some br2 -> ([],Some (Bulk_result.extend_with pt2 br2 adj2))
         )
     | Contraction (pt5,cstr) ->
      let partial_res4 = compute_bulk_result_partially pt5 helper in 
      (
       match snd partial_res4 with 
       None -> (fst partial_res4,None) 
      |Some br4 -> 
        match Bulk_result.impose_one_more_constraint_opt pt5 cstr br4 with 
          None -> raise(Bad_contraction(pt5,cstr))
          |Some new_br4 ->([],Some new_br4)
      ) 
     | Fork cases ->
      fork_case_in_bulk_result_computation compute_bulk_result_partially cases helper
     ) ;; 
  
     
  let add_if_necessary (a,b) assoc_list = 
    if List.mem_assoc a assoc_list 
    then assoc_list 
    else (a,b) :: assoc_list ;;   
  
  exception Pusher_stop ;;
  
  let pusher_for_needed_subcomputations  
     (treated,to_be_treated) = match to_be_treated with 
           [] -> raise Pusher_stop
           | pt1 :: other_pts ->
             let partial_res1 = compute_bulk_result_partially pt1 treated in 
             match snd partial_res1 with 
              None -> (treated,(fst partial_res1)@to_be_treated)
             |Some answer -> (add_if_necessary (pt1,answer) treated,other_pts) ;;
  
  let rec needed_subcomputations walker =
      let (treated,to_be_treated) = walker in 
      let subcomps =( match to_be_treated with 
      [] -> treated
      | _ -> needed_subcomputations (pusher_for_needed_subcomputations walker) ) in 
      let new_subcomps = List.filter (
      fun (_,bres) -> Bulk_result.is_not_atomic bres
     ) subcomps in 
     let _ = List.iter (fun (pt2,bres)->
      Hashtbl.replace low_hashtbl pt2 bres ) new_subcomps in 
      subcomps ;;
  
  let compute_bulk_result pt =
     let subcomps = needed_subcomputations ([],[pt]) in 
     List.assoc pt subcomps ;;   
    
end ;;  


module Pretty_printer = struct 

module Private = struct 

let for_int_list l = "["^(String.concat ";" (Image.image string_of_int l))^"]" ;;

let for_constraint (C l) = "C"^(for_int_list l);;

let for_solution_list l = "["^(String.concat ";" (Image.image for_int_list l))^"]" ;;

let for_constraint_list l = "["^(String.concat ";" (Image.image for_constraint l))^"]" ;;

let for_point = function 
    Empty_point -> "Empty_point" 
  | P(w,s,B b,S n) -> "P("^(string_of_int w)^","^(for_int_list s)^",B("^(string_of_int b)^"),S("^(string_of_int n)^"))";;

let for_fork_element 
(pt,ext_data) = "( "^(for_point pt)^","^(for_int_list ext_data)^" )" ;; 
  

let for_superficial_result = function
    Atomic -> "Atomic"
  | Decomposable (pt,ext_data) -> "Decomposable( "^(for_point pt)^","^(for_int_list ext_data)^" )"
  | Contraction (pt,cstr) -> "Contraction( "^(for_point pt)^","^(for_constraint cstr)^" )"
  | Fork (l) -> "Fork(["^(String.concat ";" (Image.image for_fork_element l))^"])" ;;

end ;; 

let for_constraint_list = Private.for_constraint_list ;; 
let for_extension_data = Private.for_int_list ;; 
let for_point = Private.for_point ;; 
let for_solution_list = Private.for_solution_list ;; 
let for_superficial_result = Private.for_superficial_result ;; 


end ;;   

module Range = struct 

let bound = 40 ;; 
    
let breadths = Int_range.scale (fun b->B b) 0 bound ;;
let sizes = Int_range.scale (fun n->S n) 1 bound ;;
    
let whole_range = Cartesian.product breadths sizes ;; 
  
let halves = Memoized.make (fun (width,scrappers) -> List.partition (fun 
      (breadth,size) -> let p = P(width,scrappers,breadth,size) in 
         Point.is_in_upper_half p
    ) whole_range );;

let er_range (width,scrappers) = function 
   Lower_half -> snd(halves (width,scrappers)) 
  |Upper_half -> fst(halves (width,scrappers)) ;;

let restricted_range (width,scrappers,IMD ql_idx) half = 
  List.filter (fun (b,n)->Warehouse.length_watcher (P(width,scrappers,b,n))>=ql_idx)
  (er_range (width,scrappers) half);;
let linear_range (w,_scr,d) = function 
  Lower_half -> Int_range.scale (fun n->(B(n-2*w+d),S(n))) (max(2*w-d) 1) (bound-2*w+d) 
  |Upper_half -> Int_range.scale (fun b->(B(b),S(b+2*w-1+d))) (max(2-2*w-d) 0) (bound-2*w+d+1) ;;

let compute_enumerator_index w (B b,S n) = function 
Lower_half ->  b+2*w-n
|Upper_half -> n-b-2*w+1 ;;

end ;;  

module File = struct 

let base_path = Dfa_root.connectable_to_subpath (Coma_big_constant.This_World.root) ;;  
let stab_filename = base_path ^ "watched/watched_and_githubbed/Szemeredi_problem/" ^
   "current_stab_at_szemeredi_problem.ml" ;;
let stab_file = Absolute_path.of_string stab_filename ;;  
let this_filename = base_path ^ "lib/Szemeredi/sz3_preliminaries.ml" ;;
let this_file =  Absolute_path.of_string this_filename ;;  

end ;;   

module Prepared_pages = struct 

  let markers_for_pair (component,half) =
     let s_koc= Kind_of_component.to_capitalized_string component 
     and s_half = String.capitalize_ascii(Half.to_string half) in 
     let common_part = " of prepared page for ("^s_koc^","^s_half^") *)"  in 
     ("(* Beginning"^common_part^"(*","*)(* End"^common_part) ;; 

  let update_prepared_page (component,half) = 
     let outside_content = Io.read_whole_file File.stab_file in 
     let markers = markers_for_pair (component,half) in 
     Replace_inside.overwrite_between_markers_inside_file
     ~overwriter:outside_content markers File.this_file ;;
  
  let get_prepared_page (component,half) = 
      let markers = markers_for_pair (component,half) in 
      let this_content = Io.read_whole_file File.this_file in 
      Cull_string.between_markers markers this_content ;; 

  let use_prepared_page (component,half) = 
      let extracted_content = get_prepared_page (component,half) in 
      if (Cull_string.trim_spaces(extracted_content)) <> ""
      then Io.overwrite_with File.stab_file extracted_content ;; 
  
end ;;  


module Side_effects_after_successful_global_check = struct 

let string_of_imd_inside_name (IMD i) =
    if i=0 then "" else 
    "_i"^(string_of_int i) ;;   

let string_of_intlist_inside_name l=
 if l=[] then "empty" else 
 "l_"^(String.concat "_" (Image.image string_of_int l))^"_l";;

let name_for_reconstructed_function (w,s,i,component,half) = 
    "f_"^(string_of_int w)^"_"^ 
     (string_of_intlist_inside_name s)^ 
     (string_of_imd_inside_name i)^
     "_"^(Kind_of_component.to_uncapitalized_string component)^
     "_"^(Half.to_string half) ;;

let string_of_intlist l=
  "["^(String.concat ";" (Image.image string_of_int l))^"]";;

let string_of_fourtuple (w,s,IMD i,half)=
  "("^(string_of_int w)^","^
      (string_of_intlist s)^","^
      "IMD("^(string_of_int i)^"),"^
      (String.capitalize_ascii(Half.to_string half))^")" ;; 

let string_of_fiftuple (w,s,IMD i,component,half)=
  "("^(string_of_int w)^","^
     (string_of_intlist s)^","^
      "IMD("^(string_of_int i)^"),"^
      (Kind_of_component.to_capitalized_string component)^","^
    (String.capitalize_ascii(Half.to_string half))^")" ;; 

let string_of_imd_inside_wsi (IMD i) =
      if i=0 then "" else ",IMD("^(string_of_int i)^")" ;;  

let compute_wsi_string (w,s,imd) =
  "("^(string_of_int w)^","^(string_of_intlist s)^(string_of_imd_inside_wsi imd)^")" ;;

let pre_markers_for_items=
  ("(* Beginning of item at ","(* End of item at ") ;; 
  
let text_for_new_item (w,s,i,component,half) = 
  let text_from_stab = Io.read_whole_file File.stab_file in 
  let original_rfi_code = Cull_string.between_markers 
    ("(* RFI BEGIN *)","(* RFI END *)") text_from_stab in 
  let f_name = name_for_reconstructed_function (w,s,i,component,half) in 
  let pre_part1 = Replace_inside.replace_inside_string
        (" rfi "," "^f_name^" ") original_rfi_code in 
  let part1 = Cull_string.trim_spaces pre_part1 in 
  let s_component = 
    Kind_of_component.to_uncapitalized_string component in          
  let s_fourtuple = string_of_fourtuple (w,s,i,half) 
  and s_fiftuple = string_of_fiftuple (w,s,i,component,half) in 
  let in_part2=[
  "Abstract_"^s_component^"_mode.global_check";
  " "^s_fourtuple^" "^f_name^" ;;"
  ] in 
  let inside_of_part2 = String.concat "\n" 
  (Image.image (fun x->(String.make 3 ' ')^x) in_part2) in 
  let part2 = "(* \n\n"^inside_of_part2 ^" \n\n*)" in 
  let wsi_string = compute_wsi_string (w,s,i) in 
  let in_part3=[
    "Hashtbl.add";
    " Warehouse.hashtbl_for_"^s_component^"_"^(Half.to_string half);
    "   "^wsi_string^" "^f_name^" ;;"
  ] in          
  let part3 = String.concat "\n" in_part3 in
  let first_line=(fst pre_markers_for_items)^" "^s_fiftuple^" *)"
  and last_line=(snd pre_markers_for_items)^" "^s_fiftuple^" *)" in 
  (String.concat "\n\n" 
    [first_line;part1;part2;part3;last_line],f_name) ;;

let markers_for_warehouse_filler=
 (
    "(* Beginning of warehouse fillings. Do not modify this line *)",
    "(* End of warehouse fillings. Do not modify this line *)"
 ) ;;

 let int_of_spaced_string s = int_of_string(Cull_string.trim_spaces s) ;; 

let parse_inside_of_intlist  comma_separated_ints = 
 let comma_indices = Substring.occurrences_of_in "," comma_separated_ints in 
 let between_commas = Cull_string.complement_union_of_ranges 
    (Image.image (fun i->(i,i)) comma_indices) comma_separated_ints in
 Image.image int_of_spaced_string between_commas ;;
 
let extract_fiftuple_from_beginning_line line =  
 let temp1 = Cull_string.two_sided_cutting (fst(pre_markers_for_items)," *)") line in 
 let temp2 = Cull_string.trim_spaces temp1 in  
 let i1 = Substring.leftmost_index_of_in_from "," temp2 1 in  
 let w = int_of_spaced_string(Cull_string.interval temp2 2 (i1-1)) in  
 let i2 = Substring.leftmost_index_of_in_from "[" temp2 i1 in  
 let i3 = Substring.leftmost_index_of_in_from "]" temp2 i2 in  
 let scr = parse_inside_of_intlist(Cull_string.interval temp2 (i2+1) (i3-1)) in 
 let i4 = Substring.leftmost_index_of_in_from "IMD" temp2 i3 in  
 let i5 = Substring.leftmost_index_of_in_from "(" temp2 i4 in  
 let i6 = Substring.leftmost_index_of_in_from ")" temp2 i5 in  
 let imd = int_of_spaced_string(Cull_string.interval temp2 (i5+1) (i6-1)) in  
 let i7 = Substring.leftmost_index_of_in_from "," temp2 i6 in  
 let i8 = Substring.leftmost_index_of_in_from "," temp2 (i7+1) in  
 let i9 = Substring.leftmost_index_of_in_from ")" temp2 i8 in  
 let component = Kind_of_component.of_string(Cull_string.trim_spaces(Cull_string.interval temp2 (i7+1) (i8-1))) in  
 let half = Half.of_string(Cull_string.trim_spaces(Cull_string.interval temp2 (i8+1) (i9-1))) in  
 (w,scr,IMD(imd),component,half) ;;

let adhoc_order_for_intlists scr1 scr2 =
   match Total_ordering.for_integers (List.length scr1) (List.length scr2) with 
    Total_ordering_result_t.Greater -> Total_ordering_result_t.Lower 
   |Total_ordering_result_t.Lower -> Total_ordering_result_t.Greater 
   |Total_ordering_result_t.Equal -> 
    if scr1=[] then Total_ordering_result_t.Equal else
    let try1 = Total_ordering.for_integers (Max.list scr1) (Max.list scr2) in 
    if try1 <> Total_ordering_result_t.Equal then try1 else       
    Total_ordering.silex_for_intlists scr1 scr2 ;; 

let pre_compare_fiftuples (w1,scr1,IMD(imd1),component1,half1) (w2,scr2,IMD(imd2),component2,half2) =
   let try1 = Total_ordering.for_integers w1 w2 in 
   if try1 <> Total_ordering_result_t.Equal then try1 else 
   let try2 = adhoc_order_for_intlists scr1 scr2 in 
   if try2 <> Total_ordering_result_t.Equal then try2 else  
   let try3 = Total_ordering.for_integers imd1 imd2 in 
   if try3 <> Total_ordering_result_t.Equal then try3 else 
   let try4 = Kind_of_component.compare component1 component2 in 
   if try4 <> Total_ordering_result_t.Equal then try4 else 
  Half.compare half1 half2 ;;
  
let compare_fiftuples = (pre_compare_fiftuples: (int * int list * index_of_missing_data *
kind_of_component * half) Total_ordering_t.t) ;; 

exception Insertion_index of int * int list * index_of_missing_data *
kind_of_component * half ;;

let rec helper_for_insertion_index (counter,new_fiftuple,untreated) =
   match untreated with 
   [] -> counter 
   | fiftuple :: others ->
    (
     match compare_fiftuples new_fiftuple fiftuple with 
     Total_ordering_result_t.Equal -> 
      let (w1,scr1,IMD(imd1),component1,half1) = fiftuple in
      raise(Insertion_index((w1,scr1,IMD(imd1),component1,half1)))
     |Total_ordering_result_t.Lower -> counter 
     |Total_ordering_result_t.Greater -> helper_for_insertion_index (counter+1,new_fiftuple,others) 
    );;

let compute_insertion_index new_fiftuple old_fiftuples =
  helper_for_insertion_index (0,new_fiftuple,old_fiftuples) ;;     

exception Write_new_item_to_this_file_exn ;; 

let write_new_item_to_this_file new_fiftuple new_item = 
  let this_text = Io.read_whole_file File.this_file in  
  let wafi_full_text = Cull_string.between_markers markers_for_warehouse_filler this_text in  
  let lines_in_wafi = Lines_in_string.lines wafi_full_text in         
  let indexed_lines = Int_range.index_everything lines_in_wafi in  
  let beginnings = List.filter (fun (_,line)->
    Supstring.begins_with line (fst(pre_markers_for_items))
  ) indexed_lines in  
  let endings = List.filter (fun (_,line)->
    Supstring.begins_with line (snd(pre_markers_for_items))
  ) indexed_lines in  
  if (List.length beginnings)<>(List.length endings) then raise Write_new_item_to_this_file_exn else 
  let fiftuples = Image.image (fun (_,line)->extract_fiftuple_from_beginning_line  line) beginnings in  
  let ii = compute_insertion_index new_fiftuple fiftuples in 
  let max_linedex_before = fst(List.nth endings (ii-1)) in  
  let (lines_before,lines_after)=
     List.partition (fun (j,_line)->j<=max_linedex_before) indexed_lines in   
  let before = String.concat "\n" (Image.image snd lines_before) in  
  let after = String.concat "\n" (Image.image snd lines_after) in  
  let new_wafi_text = String.concat "\n" [before;new_item;after] in  
  Replace_inside.overwrite_between_markers_inside_file 
    ~overwriter:new_wafi_text markers_for_warehouse_filler File.this_file ;; 

let main new_fiftuple = 
    let (new_item,f_name) = text_for_new_item new_fiftuple in 
    let (_w,_scr,_imd,component,half) = new_fiftuple in
    let _ =
      (write_new_item_to_this_file new_fiftuple new_item;
       Prepared_pages.update_prepared_page (component,half)
      ) in 
    Usual_coma_state.recompile (Some (" add new reconstructed function "^f_name)) ;;

end ;;  




module Tools_for_mode_modules = struct 

let no_extra_condition (_width,_scrappers,_) (B _b,S _n) = true ;;
let check_length (width,scrappers,IMD ql_idx) (b,n) = 
  (Warehouse.length_watcher (P(width,scrappers,b,n)))>=ql_idx;;

end ;;   

module type MODE_SEED = sig 
  type current_t 
  val current_component : kind_of_component
  val original : int * int list * index_of_missing_data -> breadth -> size -> current_t
  val extra_condition_for_range : int * int list  * index_of_missing_data -> breadth * size -> bool
  val current_printer : current_t -> string
end ;; 

module Superficial_result_seed = ((struct

type current_t = superficial_result ;;
let current_component = Superficial_result ;; 
let original (width,scrappers,_) b n =
  Bulk_result.superficial_part( Untamed.compute_bulk_result (P(width,scrappers,b,n))) ;;
let extra_condition_for_range = Tools_for_mode_modules.no_extra_condition ;;
let current_printer = Pretty_printer.for_superficial_result ;; 

end) : MODE_SEED with type current_t = superficial_result) ;;


module Solution_list_seed = ((struct

type current_t = solution list ;;
let current_component = Solution_list ;; 
let original (width,scrappers,_) b n =
  Bulk_result.solution_list( Untamed.compute_bulk_result (P(width,scrappers,b,n))) ;;
  let extra_condition_for_range = Tools_for_mode_modules.no_extra_condition ;;
let current_printer = Pretty_printer.for_solution_list ;; 

end) : MODE_SEED with type current_t = solution list) ;;

module Qpl_length_seed = ((struct

type current_t = int ;;
let current_component = Qpl_length ;; 
let original (width,scrappers,_)  b n =
  let ql = Bulk_result.qualified_points( Untamed.compute_bulk_result (P(width,scrappers,b,n))) in 
  List.length ql;; ;;
  let extra_condition_for_range = Tools_for_mode_modules.no_extra_condition ;;
let current_printer = string_of_int ;; 

end) : MODE_SEED with type current_t = int) ;;

module Qpe_core_seed = ((struct

type current_t = point ;;
let current_component = Qpe_core ;; 
let original (width,scrappers,IMD ql_idx) b n =
  let ql = Bulk_result.qualified_points( Untamed.compute_bulk_result (P(width,scrappers,b,n))) in 
  let (Q(pt,_ql_constraints,_extension)) = List.nth ql (ql_idx-1) in 
  pt ;;
let extra_condition_for_range = Tools_for_mode_modules.check_length ;;  
let current_printer = Pretty_printer.for_point ;; 

end) : MODE_SEED with type current_t = point) ;;

module Qpe_constraints_seed = ((struct

type current_t = constraint_t list ;;
let current_component = Qpe_constraints ;; 
let original (width,scrappers,IMD ql_idx) b n =
  let ql = Bulk_result.qualified_points( Untamed.compute_bulk_result (P(width,scrappers,b,n))) in 
  let (Q(_pt,ql_constraints,_extension)) = List.nth ql (ql_idx-1) in 
  ql_constraints ;;
let extra_condition_for_range = Tools_for_mode_modules.check_length ;;  
let current_printer = Pretty_printer.for_constraint_list ;; 

end) : MODE_SEED with type current_t = constraint_t list) ;;

module Qpe_extension_seed = ((struct

type current_t = extension_data ;;
let current_component = Qpe_extension ;; 
let original (width,scrappers,IMD ql_idx) b n =
  let ql = Bulk_result.qualified_points( Untamed.compute_bulk_result (P(width,scrappers,b,n))) in 
  let (Q(_pt,_ql_constraints,extension)) = List.nth ql (ql_idx-1) in 
  extension ;;
let extra_condition_for_range = Tools_for_mode_modules.check_length ;;  
let current_printer = Pretty_printer.for_extension_data ;; 

end) : MODE_SEED with type current_t = extension_data) ;;

module Mode_grow = functor (Seed:MODE_SEED) -> struct 

module Private = struct  

let print_vr_element ((B b,S n),elt) = 
    "((B "^(string_of_int b)^",S "^(string_of_int n)^"),"^(Seed.current_printer elt)^")" ;;  

let partial_range (w,s,i,half) d = List.filter ( 
      Seed.extra_condition_for_range (w,s,i) )
    (Range.linear_range (w,s,d) half) ;;

let total_range (w,s,i,half)= List.filter (
  Seed.extra_condition_for_range (w,s,i)
) (Range.er_range (w,s) half) ;;

let data_for_visualization (w,s,i,half) d = Image.image (
      fun (b,n) -> ((b,n),Seed.original (w,s,i) b n)
   ) (partial_range (w,s,i,half) d) ;;
   
let pretty_print_visualization_data l = 
       "[\n"^(String.concat ";\n" (Image.image print_vr_element l))^"\n]" ;;

end ;;  

let visualize (w,s,i,half) d = 
  let answer = Private.data_for_visualization (w,s,i,half) d in 
  let msg = "\n\n\n"^(Private.pretty_print_visualization_data answer)^"\n\n\n" in 
  let _ = (print_string msg;flush stdout) in 
  (fun ()->answer) ;; 

let partial_check (w,s,i,half) d f = 
  let temp1 = Image.image (
  fun (b,n) -> ((b,n),Seed.original (w,s,i) b n,f b n)
  ) (Private.partial_range (w,s,i,half) d) in 
  List.filter (fun (_,y1,y2)->y1<>y2) temp1;;   

let global_check (w,s,i,half) g = 
    let temp1 = Image.image (
    fun (b,n) -> ((b,n),Seed.original (w,s,i) b n,g b n)
    ) (Private.total_range (w,s,i,half)) in 
    let temp2 = List.filter (fun (_,y1,y2)->y1<>y2) temp1 in 
    let answer = 
      (if temp2=[] 
       then (0,[]) 
       else Min.minimize_it_with_care (fun (pair,_,_)->
             Range.compute_enumerator_index w pair half) temp2) in 
    let _ = 
    (if temp2=[] 
    then Side_effects_after_successful_global_check.main (w,s,i,Seed.current_component,half))  
    in 
    answer;;   


end ;;  

module Abstract_superficial_result_mode = Mode_grow(Superficial_result_seed);;
module Abstract_solution_list_mode = Mode_grow(Solution_list_seed);; 
module Abstract_qpl_length_mode = Mode_grow(Qpl_length_seed);;
module Abstract_qpe_core_mode = Mode_grow(Qpe_core_seed);;
module Abstract_qpe_constraints_mode = Mode_grow(Qpe_constraints_seed);;
module Abstract_qpe_extension_mode = Mode_grow(Qpe_extension_seed);;

module Overall = struct 

let tour_for_single_pair (width,scrappers) =
    let rec tempf = (fun l->match l with 
      [] -> None 
      | (b,n) :: others ->
         (
          let (good_opt,bad_opt) = Warehouse.nonhalved_bulk_result (P(width,scrappers,b,n)) in 
          if good_opt = None 
          then Some(Option.get bad_opt,(b,n))
          else tempf others     
         )
    ) in
    tempf Range.whole_range ;; 

let rec tour = function 
    [] -> None 
   | ws :: others -> 
      match tour_for_single_pair ws with 
       Some (data,(b,n)) -> Some (data,P(fst ws,snd ws,b,n))   
      | None -> tour others ;; 


let goal = [(1,[]);(2,[]);(3,[]);(4,[]);(5,[])] ;; 

let ref_for_status = ref None ;; 

let get_status () = match (!ref_for_status) with 
   Some status -> status 
   | None -> 
      let opt =  tour goal in 
      let (((koc,half),imd),pt) = Option.get opt in 
      let answer = (koc,half,imd,pt) in 
      let _ = (
         ref_for_status := Some answer; 
         Prepared_pages.use_prepared_page (koc,half)
      ) in 
      let msg = "\n\n\n To get started, do : \n\n"^
                "open "^(Kind_of_component.to_capitalized_string(koc))^"_"^
                        (Half.to_string half)^"_mode ;;\n\n\n" in 
      let _ = (print_string msg;flush stdout) in                   
      answer ;;


end ;; 

module Superficial_result_lower_half_mode = struct 

let current_data () =
    let (_koc,_half,imd,pt) = Overall.get_status () in 
    (Point.width pt,Point.scrappers pt,imd,Lower_half) ;;     

    let visualize d = Abstract_superficial_result_mode.visualize (current_data()) d  ;;
    let partial_check d f = Abstract_superficial_result_mode.partial_check (current_data()) d f ;;
    let global_check g = Abstract_superficial_result_mode.global_check (current_data()) g ;;
end ;;


module Superficial_result_upper_half_mode = struct 

let current_data () =
    let (_koc,_half,imd,pt) = Overall.get_status () in 
    (Point.width pt,Point.scrappers pt,imd,Upper_half) ;;     
  
    let visualize d = Abstract_superficial_result_mode.visualize (current_data()) d  ;;
    let partial_check d f = Abstract_superficial_result_mode.partial_check (current_data()) d f ;;
    let global_check g = Abstract_superficial_result_mode.global_check (current_data()) g ;;
end ;;
  

module Solution_list_lower_half_mode = struct 

  let current_data () =
      let (_koc,_half,imd,pt) = Overall.get_status () in 
      (Point.width pt,Point.scrappers pt,imd,Lower_half) ;;     
  
      let visualize d  = Abstract_solution_list_mode.visualize (current_data()) d ;;
      let partial_check d f = Abstract_solution_list_mode.partial_check (current_data()) d f ;;
      let global_check g = Abstract_solution_list_mode.global_check (current_data()) g ;;
  end ;;
  
  
module Solution_list_upper_half_mode = struct 
  
  let current_data () =
      let (_koc,_half,imd,pt) = Overall.get_status () in 
      (Point.width pt,Point.scrappers pt,imd,Upper_half) ;;     
    
      let visualize d  = Abstract_solution_list_mode.visualize (current_data()) d  ;;
      let partial_check d f = Abstract_solution_list_mode.partial_check (current_data()) d f ;;
      let global_check g = Abstract_solution_list_mode.global_check (current_data()) g ;;
end ;;
  

module Qpl_length_lower_half_mode = struct 

  let current_data () =
      let (_koc,_half,imd,pt) = Overall.get_status () in 
      (Point.width pt,Point.scrappers pt,imd,Lower_half) ;;     
  
      let visualize d = Abstract_qpl_length_mode.visualize (current_data()) d ;;
      let partial_check d f = Abstract_qpl_length_mode.partial_check (current_data()) d f ;;
      let global_check g = Abstract_qpl_length_mode.global_check (current_data()) g ;;
end ;;
  
  
module Qpl_length_upper_half_mode = struct 
  
  let current_data () =
      let (_koc,_half,imd,pt) = Overall.get_status () in 
      (Point.width pt,Point.scrappers pt,imd,Upper_half) ;;     
    
      let visualize d = Abstract_qpl_length_mode.visualize (current_data()) d  ;;
      let partial_check d f = Abstract_qpl_length_mode.partial_check (current_data()) d f ;;
      let global_check g = Abstract_qpl_length_mode.global_check (current_data()) g ;;
end ;;


module Qpe_core_lower_half_mode = struct 

  let current_data () =
      let (_koc,_half,imd,pt) = Overall.get_status () in 
      (Point.width pt,Point.scrappers pt,imd,Lower_half) ;;     
  
      let visualize d = Abstract_qpe_core_mode.visualize (current_data()) d  ;;
      let partial_check d f = Abstract_qpe_core_mode.partial_check (current_data()) d f ;;
      let global_check g = Abstract_qpe_core_mode.global_check (current_data()) g ;;
end ;;
  
  
module Qpe_core_upper_half_mode = struct 
  
  let current_data () =
      let (_koc,_half,imd,pt) = Overall.get_status () in 
      (Point.width pt,Point.scrappers pt,imd,Upper_half) ;;     
    
      let visualize d = Abstract_qpe_core_mode.visualize (current_data()) d ;;
      let partial_check d f = Abstract_qpe_core_mode.partial_check (current_data()) d f ;;
      let global_check g = Abstract_qpe_core_mode.global_check (current_data()) g ;;
end ;;


module Qpe_constraints_lower_half_mode = struct 

  let current_data () =
      let (_koc,_half,imd,pt) = Overall.get_status () in 
      (Point.width pt,Point.scrappers pt,imd,Lower_half) ;;     
  
      let visualize d = Abstract_qpe_constraints_mode.visualize (current_data()) d  ;;
      let partial_check d f = Abstract_qpe_constraints_mode.partial_check (current_data()) d f ;;
      let global_check g = Abstract_qpe_constraints_mode.global_check (current_data()) g ;;
end ;;
  
  
module Qpe_constraints_upper_half_mode = struct 
  
  let current_data () =
      let (_koc,_half,imd,pt) = Overall.get_status () in 
      (Point.width pt,Point.scrappers pt,imd,Upper_half) ;;     
    
      let visualize d = Abstract_qpe_constraints_mode.visualize (current_data()) d ;;
      let partial_check d f = Abstract_qpe_constraints_mode.partial_check (current_data()) d f ;;
      let global_check g = Abstract_qpe_constraints_mode.global_check (current_data()) g ;;
end ;;



module Qpe_extension_lower_half_mode = struct 

  let current_data () =
      let (_koc,_half,imd,pt) = Overall.get_status () in 
      (Point.width pt,Point.scrappers pt,imd,Lower_half) ;;     
  
      let visualize d = Abstract_qpe_extension_mode.visualize (current_data()) d  ;;
      let partial_check d f = Abstract_qpe_extension_mode.partial_check (current_data()) d f ;;
      let global_check g = Abstract_qpe_extension_mode.global_check (current_data()) g ;;
end ;;
  
  
module Qpe_extension_upper_half_mode = struct 
  
  let current_data () =
      let (_koc,_half,imd,pt) = Overall.get_status () in 
      (Point.width pt,Point.scrappers pt,imd,Upper_half) ;;     
    
      let visualize d = Abstract_qpe_extension_mode.visualize (current_data()) d  ;;
      let partial_check d f = Abstract_qpe_extension_mode.partial_check (current_data()) d f ;;
      let global_check g = Abstract_qpe_extension_mode.global_check (current_data()) g ;;
    
end ;;

(* Beginning of prepared page for (Superficial_result,Lower_half) *)(*
*)(* End of prepared page for (Superficial_result,Lower_half) *)
(* Beginning of prepared page for (Superficial_result,Upper_half) *)(*
*)(* End of prepared page for (Superficial_result,Upper_half) *)
(* Beginning of prepared page for (Solution_list,Lower_half) *)(*
*)(* End of prepared page for (Solution_list,Lower_half) *)
(* Beginning of prepared page for (Solution_list,Upper_half) *)(*
*)(* End of prepared page for (Solution_list,Upper_half) *)
(* Beginning of prepared page for (Qpl_length,Lower_half) *)(*
*)(* End of prepared page for (Qpl_length,Lower_half) *)
(* Beginning of prepared page for (Qpl_length,Upper_half) *)(*
*)(* End of prepared page for (Qpl_length,Upper_half) *)
(* Beginning of prepared page for (Qpe_core,Lower_half) *)(*(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)


open Skeptical_duck_lib ;; 
open Needed_values ;; 


open Sz3_preliminaries ;;
open Tools_for_warehouse ;; 
let see0 = Overall.get_status () ;; 


open Qpe_core_lower_half_mode ;;


let vz1 = visualize 1 ;; 
let rf1 (B b) (S _n) = 
  if b=1 then Empty_point else
    P(1,[],B(b-1),S(b+1));;
let check_rf1 = partial_check 1 rf1 ;; 

let vz2 = visualize 2 ;; 
let rf2 (B b) (S n) = 
  if b=1 then Empty_point else
    P(1,[],B(b-1),S(b+1));;
let check_rf2 = partial_check 2 rf2 ;; 

let vz3 = visualize 3 ;; 
let rf3 (B b) (S n) = 
  if b=1 then Empty_point else
    P(1,[],B(b-1),S(b+1));;
let check_rf3 = partial_check 2 rf3 ;; 



(* RFI BEGIN *)

let rfi (B _b) (S n) = 
  if n<=4 then Empty_point else
    P(1,[],B(n-4),S(n-2));; 

(* RFI END *)
let check_rfi = global_check rfi ;; 
*)(* End of prepared page for (Qpe_core,Lower_half) *)
(* Beginning of prepared page for (Qpe_core,Upper_half) *)(*(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)


open Skeptical_duck_lib ;; 
open Needed_values ;; 


open Sz3_preliminaries ;;
open Tools_for_warehouse ;; 
let see0 = Overall.get_status () ;; 


open Qpe_core_upper_half_mode ;;


let vz1 = visualize 1 ;; 
let rf1 (B b) (S _n) = 
  if b=1 then Empty_point else
    P(1,[],B(b-1),S(b+1));;
let check_rf1 = partial_check 1 rf1 ;; 

let vz2 = visualize 2 ;; 
let rf2 (B b) (S n) = 
  if b=1 then Empty_point else
    P(1,[],B(b-1),S(b+1));;
let check_rf2 = partial_check 2 rf2 ;; 

let vz3 = visualize 3 ;; 
let rf3 (B b) (S n) = 
  if b=1 then Empty_point else
    P(1,[],B(b-1),S(b+1));;
let check_rf3 = partial_check 2 rf3 ;; 



(* RFI BEGIN *)

let rfi (B b) (S _n) = 
  if b=1 then Empty_point else
    P(1,[],B(b-1),S(b+1));; 

(* RFI END *)
let check_rfi = global_check rfi ;; 
*)(* End of prepared page for (Qpe_core,Upper_half) *)
(* Beginning of prepared page for (Qpe_constraints,Lower_half) *)(*(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)


open Skeptical_duck_lib ;; 
open Needed_values ;; 


open Sz3_preliminaries ;;
open Tools_for_warehouse ;; 
let see0 = Overall.get_status () ;; 


open Qpe_constraints_lower_half_mode ;;


let vz1 = visualize 1 ;; 
let rf1 (B b) (S _n) = 
   [];;
let check_rf1 = partial_check 1 rf1 ;; 

let vz2 = visualize 2 ;; 
let rf2 (B b) (S n) = 
  [];;
let check_rf2 = partial_check 2 rf2 ;; 

let vz3 = visualize 3 ;; 
let rf3 (B b) (S n) = 
  [];;
let check_rf3 = partial_check 2 rf3 ;; 



(* RFI BEGIN *)

let rfi (B b) (S _n) = 
  [];; 

(* RFI END *)
let check_rfi = global_check rfi ;; 
*)(* End of prepared page for (Qpe_constraints,Lower_half) *)
(* Beginning of prepared page for (Qpe_constraints,Upper_half) *)(*(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)


open Skeptical_duck_lib ;; 
open Needed_values ;; 


open Sz3_preliminaries ;;
open Tools_for_warehouse ;; 
let see0 = Overall.get_status () ;; 


open Qpe_constraints_upper_half_mode ;;


let vz1 = visualize 1 ;; 
let rf1 (B b) (S _n) = 
   [];;
let check_rf1 = partial_check 1 rf1 ;; 

let vz2 = visualize 2 ;; 
let rf2 (B b) (S n) = 
  [];;
let check_rf2 = partial_check 2 rf2 ;; 

let vz3 = visualize 3 ;; 
let rf3 (B b) (S n) = 
  [];;
let check_rf3 = partial_check 2 rf3 ;; 



(* RFI BEGIN *)

let rfi (B b) (S _n) = 
  [];; 

(* RFI END *)
let check_rfi = global_check rfi ;; 
*)(* End of prepared page for (Qpe_constraints,Upper_half) *)
(* Beginning of prepared page for (Qpe_extension,Lower_half) *)(*(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)


open Skeptical_duck_lib ;; 
open Needed_values ;; 


open Sz3_preliminaries ;;
open Tools_for_warehouse ;; 
let see0 = Overall.get_status () ;; 

open Qpe_extension_lower_half_mode ;;


let vz1 = visualize 1 ;; 
let rf1 (B _b) (S n) = 
  match List.assoc_opt n
  [4,1::(Int_range.range 3 4);5,[1;2]@(Int_range.range 4 5)] with 
  Some answer -> answer 
  |None -> Int_range.range (n-1) n ;;
let check_rf1 = partial_check 1 rf1 ;; 

let vz2 = visualize 2 ;; 
let rf2 (B b) (S n) = 
  match List.assoc_opt n
  [4,1::(Int_range.range 3 4);5,[1;2]@(Int_range.range 4 5)] with 
  Some answer -> answer 
  |None -> Int_range.range (n-1) n ;;
let check_rf2 = partial_check 2 rf2 ;; 

let vz3 = visualize 3 ;; 
let rf3 (B b) (S n) = 
  match List.assoc_opt b
  [1,1::(Int_range.range 3 (n-b+1));2,[1;2]@(Int_range.range 4 (n-b+2))] with 
  Some answer -> answer 
  |None -> Int_range.range (b+2) n ;;
let check_rf3 = partial_check 2 rf3 ;; 



(* RFI BEGIN *)

let rfi (B _b) (S n) = 
  match List.assoc_opt n
  [4,[1;3;4];5,[1;2;4;5]] with 
  Some answer -> answer 
  |None -> Int_range.range (n-1) n ;; 

(* RFI END *)
let check_rfi = global_check rfi ;; 
*)(* End of prepared page for (Qpe_extension,Lower_half) *)
(* Beginning of prepared page for (Qpe_extension,Upper_half) *)(*(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)


open Skeptical_duck_lib ;; 
open Needed_values ;; 


open Sz3_preliminaries ;;
open Tools_for_warehouse ;; 
let see0 = Overall.get_status () ;; 


open Qpe_extension_upper_half_mode ;;


let vz1 = visualize 1 ;; 
let rf1 (B b) (S n) = 
  match List.assoc_opt b
  [1,1::(Int_range.range 3 3);2,[1;2]@(Int_range.range 4 4)] with 
  Some answer -> answer 
  |None -> Int_range.range (b+2) n ;;
let check_rf1 = partial_check 1 rf1 ;; 

let vz2 = visualize 2 ;; 
let rf2 (B b) (S n) = 
  match List.assoc_opt b
  [1,1::(Int_range.range 3 (n-b+1));2,[1;2]@(Int_range.range 4 (n-b+2))] with 
  Some answer -> answer 
  |None -> Int_range.range (b+2) n ;;
let check_rf2 = partial_check 2 rf2 ;; 

let vz3 = visualize 3 ;; 
let rf3 (B b) (S n) = 
  match List.assoc_opt b
  [1,1::(Int_range.range 3 (n-b+1));2,[1;2]@(Int_range.range 4 (n-b+2))] with 
  Some answer -> answer 
  |None -> Int_range.range (b+2) n ;;
let check_rf3 = partial_check 2 rf3 ;; 



(* RFI BEGIN *)

let rfi (B b) (S n) = 
  match List.assoc_opt b
  [1,[1;2]@(Int_range.range 4 (n-b+1))] with 
  Some answer -> answer 
  |None -> Int_range.range (b+3) n ;;  

(* RFI END *)
let check_rfi = global_check rfi ;; 
*)(* End of prepared page for (Qpe_extension,Upper_half) *)


