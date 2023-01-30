(*

#use"lib/Szemeredi/sz_preliminaries_for_stab.ml";;

*)

type breadth = Sz_types_for_third_stab.breadth = B of int ;;
type size = Sz_types_for_third_stab.size = S of int ;;

type point = Sz_types_for_third_stab.point = 
   Empty_point 
  |P of int * (int list) * breadth * size   ;;  

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


module Rose = struct 

  type rose_type = ( breadth -> size -> Sz_types_for_third_stab.bulk_result) ;;  
  let rose_hashtbl = ((Hashtbl.create 50) : (int * int list, rose_type) Hashtbl.t) ;;
  let try_precomputed_results pt =
     let (width,scrappers,breadth,n) = Point.unveil pt in 
     match Hashtbl.find_opt rose_hashtbl (width,scrappers) with 
     Some summary_f -> Some (summary_f breadth n)
    | None -> None ;;   
  
  
end ;;  



module Constraint = struct  

  type t = Sz_types_for_third_stab.constraint_t = C of int list ;; 

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

    type point = Sz_types_for_third_stab.point 
     =Empty_point | P of int * int list * breadth * size ;;
    type constraint_t = Sz_types_for_third_stab.constraint_t = C of int list ;; 
    type extension_data =  int list ;;
    type t = Sz_types_for_third_stab.qualified_point = Q of point * constraint_t list * extension_data ;;

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

  type point = Sz_types_for_third_stab.point 
     =Empty_point | P of int * int list * breadth * size ;;
  type constraint_t = Sz_types_for_third_stab.constraint_t = C of int list ;; 
  type extension_data =  int list ;;
  type qualified_point = Sz_types_for_third_stab.qualified_point = Q of point * constraint_t list * extension_data ;;
  type solution = int list ;;
  type t = Sz_types_for_third_stab.mold = M of solution list * qualified_point list ;;

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

  type point = Sz_types_for_third_stab.point 
     =Empty_point | P of int * int list * breadth * size ;;
  type constraint_t = Sz_types_for_third_stab.constraint_t = C of int list ;; 
  type extension_data =  int list ;;
  type qualified_point = Sz_types_for_third_stab.qualified_point = Q of point * constraint_t list * extension_data ;;
  type solution = int list ;;
  type mold = Sz_types_for_third_stab.mold = M of solution list * qualified_point list ;;
  type superficial_result = Sz_types_for_third_stab.superficial_result = 
        Atomic
      | Decomposable of point * extension_data
      | Contraction of point * constraint_t
      | Fork of (point * extension_data) list ;; 
  type t = Sz_types_for_third_stab.bulk_result = BR of superficial_result * mold ;;

  let atomic_case pt = BR (Atomic,M([Point.enumerate_supporting_set pt],[])) ;; 
  
  let is_not_atomic (BR(sr,_)) = sr <> Atomic ;; 
  
  let superficial_part (BR(sr,_)) = sr ;; 
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
  
  

    