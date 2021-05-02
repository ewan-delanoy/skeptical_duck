(*

#use"Van_der_waerden/vdw_memoize.ml";;

*)

let hashtbl_for_vdw_computation = ((Hashtbl.create 100): (int * Set_of_integers_t.t, int * Set_of_integers_t.t) Hashtbl.t) ;;

let ii x y =  (Ennig.ennig x y);;
let iii l =   (List.flatten(Image.image (fun (x,y)->Ennig.ennig x y) l));;

let translate d soi = Set_of_integers.safe_set (Set_of_integers.image (fun x->x+d) soi);;

let test_for_disjointness ll=
   let temp1 = Uple.list_of_pairs ll in 
   List.for_all (fun (x,y)->(Set_of_integers.size_of_intersection x y) = 0) temp1 ;; 

let solution_in_disjoint_case obstructions soi = 
   let temp1 = Image.image Set_of_integers.max obstructions in
   Set_of_integers.setminus soi (Set_of_integers.safe_set temp1) ;;

let set_start_to_one soi =
     let d = (Set_of_integers.min soi)-1 in 
     (d,translate (-d) soi);;

let level1 soi = 
     let l = Set_of_integers.forget_order soi in 
     let intervals = Listennou.decompose_into_connected_components l in 
     let temp1 = List.flatten(Image.image (
       fun (a,b)-> List.filter (fun x->List.mem ((x-a) mod 3)[0;1] ) (Ennig.ennig a b)
     ) intervals) in 
     (List.length temp1,Set_of_integers.safe_set temp1) ;;
    
let partial_level3 n = List.filter (fun x->List.mem(x mod 8)[1;2;4;5]) (Ennig.ennig 1 n);;     

let check_for_precomputed_value (width,soi) =
   let (d,relocated_soi) = set_start_to_one soi in 
   match Hashtbl.find_opt hashtbl_for_vdw_computation (width,relocated_soi) with 
   (Some(optimal_size,sol)) -> Some(optimal_size,translate d sol)
   |None ->
      let obstructions = Vdw_common.look_for_arithmetic_progressions_in_with_width_up_to width soi in 
      if test_for_disjointness obstructions
      then let sol = solution_in_disjoint_case obstructions soi in 
            Some(Set_of_integers.length sol,sol)
      else      
      if width=1 
      then Some(level1 soi) 
      else None;;
 
exception Too_small_for_extension of int;;
exception Unforeseen_value_in_extend of int * Set_of_integers_t.t ;;
exception Extension_not_allowed of int * Set_of_integers_t.t * Set_of_integers_t.t * Set_of_integers_t.t ;;

let extend width ?opt_sol l_soi =
 let soi= Set_of_integers.safe_set l_soi in  
  if width<2 then raise(Too_small_for_extension width) else 
  match  check_for_precomputed_value (width-1,soi) with 
  None -> raise (Unforeseen_value_in_extend(width-1,soi))
  |Some (optimal_size,old_sol) ->
     let sol=(match opt_sol with None ->old_sol |Some(forced_sol) -> forced_sol) in 
     let temp1= Vdw_common.look_for_arithmetic_progressions_in_with_width_up_to width sol in 
     if temp1<>[]
     then raise(Extension_not_allowed(width,soi,sol,List.hd temp1))
     else Hashtbl.add hashtbl_for_vdw_computation (width,soi) (optimal_size,sol) ;;


exception Unforeseen_value_in_decomposition of int * Set_of_integers_t.t * Set_of_integers_t.t ;;
exception Incorrect_input_in_decomposition of int * Set_of_integers_t.t * Set_of_integers_t.t ;;
exception Size_mismatch of int * int * int ;;
     
let get_half width whole half =match check_for_precomputed_value (width,half) with 
     None -> raise(Unforeseen_value_in_decomposition(width,whole,half))
     |Some(optimal_size,_) -> optimal_size ;;

let register_decomposition width l_first_half l_other_half l_sol=
   let first_half = Set_of_integers.safe_set l_first_half 
   and other_half = Set_of_integers.safe_set l_other_half 
   and sol = Set_of_integers.safe_set l_sol in
   let temp1= Vdw_common.look_for_arithmetic_progressions_in_with_width_up_to width sol in 
   if temp1<>[]
   then raise(Incorrect_input_in_decomposition(width,sol,List.hd temp1))
   else 
   let whole = Set_of_integers.merge first_half other_half in 
   let n1 = get_half width whole first_half
   and n2 = get_half width whole other_half 
   and n=Set_of_integers.length sol in
   if n<>n1+n2
   then  raise(Size_mismatch(n,n1,n2))
   else Hashtbl.add hashtbl_for_vdw_computation (width,whole) (n,sol) ;;
 

exception  Unforeseen_value_in_fork of  int * Set_of_integers_t.t * Set_of_integers_t.t ;;
exception  Incorrect_triple of int * (int * int * int) ;;
exception  Foreign_triple of  Set_of_integers_t.t * (int * int * int) ;;

let get_third width whole third =match check_for_precomputed_value (width,third) with 
   None -> raise(Unforeseen_value_in_decomposition(width,whole,third))
   |Some(optimal_size,sol) -> (optimal_size,sol) ;;   

let register_fork width (a,b,c) l_soi=
   let soi = Set_of_integers.safe_set l_soi in
   if (a>=b)||(b>=c)||(b-a<>c-b)||(b-a>width)
   then raise(Incorrect_triple(width,(a,b,c)))
   else 
   let triple = Set_of_integers.safe_set [a;b;c]  in 
   if not(Set_of_integers.is_included_in triple soi) 
   then raise(Foreign_triple(soi,(a,b,c)))
   else 
   let temp1 = Image.image (fun v->
      get_third width soi (Set_of_integers.outsert v soi)) [a;b;c] in 
   let (optimal_size,pairs) = Max.maximize_it_with_care fst temp1 in
   let sols = Image.image snd pairs in 
   let best_sol = Total_ordering.min Vdw_common.silex_order sols in 
   Hashtbl.add hashtbl_for_vdw_computation (width,soi) (optimal_size,best_sol) ;;

exception No_missing_link ;;

let next_missing_link width soi=
   match check_for_precomputed_value (width,soi) with 
   Some(_,_) -> raise No_missing_link
   |None ->
      let w = Vdw_list_of_constraints_t.Defined_by_max_width width in 
      let (_,_,head_constraint) = Vdw_common.first_cut w soi in
      let dog_eared_ones =Set_of_integers.image (fun cell->Set_of_integers.outsert cell soi) 
        head_constraint in 
      let temp1 = Image.image (fun x->(x,check_for_precomputed_value (width,x))) dog_eared_ones in 
      let (temp2,temp3) = List.partition (fun (x,opt)->opt=None) temp1 in 
      let temp4 = Image.image fst temp2 
      and temp5 = Image.image (fun (x,opt)->(Set_of_integers.forget_order x,Option.unpack opt)) temp3 in 
      let m = (if temp4=[] then Set_of_integers.safe_set [] else Total_ordering.min Vdw_common.silex_order temp4) in 
      (m,temp5)   
   ;;

  


