(*

#use"Van_der_Waerden/Fixed_width/vdw_computer.ml";;

*)

type colored_variable = 
   A of int |B of int|C of int|D of int|E of int|F of int| Og of int * int ;;   
type list_of_affinities = Aff of ((int list * colored_variable) * bool)  list ;;    
type partition_watcher = 
   PW of 
   list_of_affinities *
   (((int list * colored_variable) * (colored_variable * colored_variable)) list) *
   ((colored_variable * (int list * colored_variable) list) list) ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;; 
let mea = Vdw_chosen.measure ;;
let lmea = Vdw_chosen.lower_measure ;;
let is_adm = Vdw_chosen.test_for_admissibility ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

let intlist_to_string lamb =
   "["^(String.concat ";" (Image.image string_of_int lamb))^"]" ;;

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  

module Colored = struct 

exception Data_for_variable_exn ;;
let data_for_variable = function 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) |(D k) ->("D",k) 
    |(E k) ->("E",k) |(F k) ->("F",k) 
    |(Og(_,_))->raise(Data_for_variable_exn) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     if lbl = "D" then D k else 
     if lbl = "E" then E k else
     if lbl = "F" then F k else    
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let is_ugly cv = match cv with 
 (Og(_,_)) -> true |
  _ -> let (lbl,k) = data_for_variable cv in 
       (int_of_char(String.get lbl 0))>=68 ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d [0,A 0;1, B 0;2, C 0;3, D 0;4, E 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

let to_ocaml_readable_string cv= match cv with  
   (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
   | _ -> let (lbl,k) = data_for_variable cv in 
          lbl^" "^(string_of_int k) ;; 

let order = ((fun cv1 cv2->
  let (lbl1,k1) = data_for_variable cv1 
  and (lbl2,k2) = data_for_variable cv2 in  
  let trial1 = Total_ordering.standard lbl1 lbl2 in 
  if trial1 <> Total_ordering_result_t.Equal then trial1 else 
   Total_ordering.standard k1 k2 
) : colored_variable Total_ordering_t.t) ;;

let order_for_pairs = ((fun (lamb1,cv1) (lamb2,cv2)->
   let trial1 = order cv1 cv2 in 
   if trial1 <> Total_ordering_result_t.Equal then trial1 else 
    (Total_ordering.silex_compare Total_ordering.standard ) lamb1 lamb2 
 ) : (int list * colored_variable) Total_ordering_t.t) ;;
 

end ;;  

module Affinity = struct

   let shadow (Aff l) cvar = 
      Option.filter_and_unpack (
         fun ((selector,cvar2),is_compatible) ->
           if cvar2 = cvar 
           then Some(selector,is_compatible)   
           else None
      ) l;;
  
  let expand_using_partition aff parti  =
     let (Aff(old_affinities)) = aff 
     and ((selector,partitioned),(part1,part2)) = parti in
     let temp1 = shadow aff partitioned in 
     let temp2 = List.flatten(Image.image (
        fun (selector2,is_compatible) ->
           [((selector2,part1),is_compatible);
            ((selector2,part2),is_compatible)]     
     ) temp1) in
     let new_affinities = old_affinities @ 
         [((selector,part1),true);
         ((selector,part2),false);]@temp2  in 
     Aff(new_affinities) ;;
  
  let insert affty  (Aff old_affinities)  =
     if List.mem affty old_affinities 
     then Aff old_affinities
     else Aff(affty :: old_affinities) ;;
  
  let analyse_combination (Aff affinities) combination =
     let temp0 = List.filter (
       fun (lamb,_)->Vdw_chosen.test_for_admissibility lamb
     ) combination in 
     let temp1 = Image.image (fun 
        pair -> 
         let res = (
          if fst pair = []
          then Some true
          else List.assoc_opt  pair affinities 
         ) in
         (pair,res)
     ) temp0 in 
     let filterer = (fun criterion ->
        Option.filter_and_unpack 
       (fun (pair,opt)->
         if opt=criterion then Some pair else None) temp1
     ) in 
     let bad_ones = filterer None in 
     let checked_version = filterer (Some true) in 
     (Ordered.sort Colored.order_for_pairs bad_ones,checked_version);;
   
  
  
end ;;   
  
module Expansion = struct

let subst_by_in cvar replacement_for_cvar l=
  let temp1 = List.flatten(Image.image (
     fun pair ->
      let (lamb2,cvar2) = pair in 
      if cvar2=cvar 
      then Image.image (
          fun (lamb3,cvar3)->
            (Ordered.merge oi lamb2 lamb3,cvar3)) replacement_for_cvar
      else [pair]      
  ) l) in
  List.filter (fun (lamb4,cvar4)->is_adm lamb4) temp1 ;; 

let origin n d =
   let delt = mea(n)-mea(n-1) in   
   List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
   [
     [],(n-1,d-delt);
     [n],(n-1,d+1-delt)
   ];;   

let allowed_substitution_by_in (n,d) expr = 
   subst_by_in (n,d) (origin n d) expr;;

let special1 n d =
   let temp1 = origin n d in 
   let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d+1) temp2 in
   temp3 ;;

let special2 n d =
   let temp1 = origin n d in 
   let temp2 = allowed_substitution_by_in (n-1,d+1) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d+1) temp2 in
   temp3 ;;
     

let current n d = 
   if (n,d)=(18,1) then special1 n d else 
   if (n,d)=(21,0) then special2 n d else    
   origin n d ;;

end ;;   

let see = Expansion.current ;;

module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (Expansion.current n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;

let direct_descendants (n,d) =
   let temp1 = Image.image snd (Expansion.current n d) in 
   List.filter (fun (nn,dd)->
         ((Hashtbl.find_opt hashtbl_for_oslo (nn,dd)) = None)
         && (nn >= wall_position)   
   ) temp1 ;;      

exception Pusher_for_needed_intermediaries_exn ;;   

let pusher_for_needed_intermediaries (treated,to_be_treated) =
   let oh = Total_ordering.standard2 in (* oh is for "order here" *)
   match (List.rev to_be_treated) with 
     [] -> raise (Pusher_for_needed_intermediaries_exn) 
    |pair :: others -> 
      let new_ones = Ordered.sort oh (direct_descendants pair) in 
   (Ordered.insert oh pair treated,
            Ordered.merge oh new_ones others) ;;  

let needed_intermediaries n d = 
   let rec tempf = (fun (treated,to_be_treated)->
      match (List.rev to_be_treated) with 
       [] -> treated 
      |pair :: others -> 
         let new_step = pusher_for_needed_intermediaries (treated,to_be_treated) in 
         tempf(new_step) 
   ) in 
   tempf([],direct_descendants (n,d) ) ;;      

end ;;

let og = Oslo.get ;;
let ni = Oslo.needed_intermediaries ;;


module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  ((selector,partitioned),(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition 
   (PW(old_affinities,old_parties,ll):t) parti  =
   let new_affinities =  Affinity.expand_using_partition old_affinities parti
   and new_parties = old_parties @ [parti]   in 
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_affinities,new_parties,new_ll):>t) ;;

let insert_affinity (PW(old_affinities,old_parties,ll)) affty  =
   PW(Affinity.insert affty old_affinities,old_parties,ll) ;;        
    

exception Missing_affinities of ( (int list) * colored_variable) list;;   

let check_combination_using_affinities combination (PW(affinities,_,_):t)=
  let (bad_ones,checked_version) = Affinity.analyse_combination affinities combination in 
  if bad_ones <> []
  then raise(Missing_affinities bad_ones)
  else checked_version;;

let expand_using_t combination (pw:t) =
   let (PW(_,_,ll)) = pw in 
   let ref_for_unknowns = ref [] in 
   let combination2 = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in
   let new_combination = check_combination_using_affinities combination2  pw in
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(affinities,parties,ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(affinities,parties,ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(affinities,parties,List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   (intlist_to_string lamb)^","^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW (_,_,ll)):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;

let decompose ((PW (_,_,ll)):t) cvar =
     List.assoc cvar ll ;;

let ugly_part ((PW (_,_,ll)):t) cvar =
     let temp1 = List.assoc cvar ll in 
     List.filter (fun (lamb,cv)->Colored.is_ugly cv) temp1 ;;     

end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
   "D",[List.filter (fun x->(List.length x)=5) wall];
   "E",[List.filter (fun x->(List.length x)=4) wall];
   "F",[List.filter (fun x->(List.length x)=3) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = List.partition (Vdw_chosen.test_joinability selector)  old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    if (a=[])||(b=[])
    then (a,b,None)  
    else
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (a,b,Some(Colored.variable_from_label lbl i,
              Colored.variable_from_label lbl j)) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW(Aff[],[],[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
       (D 0),[[],D 0];
       (E 0),[[],E 0];
       (F 0),[[],F 0];
])) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;

let explain_partition (selector,cv,a,b,opt_cvs) =
   let s_selector = "["^(String.concat ";" (Image.image string_of_int selector))^"]" in  
   let beginning_of_msg = "\n\n"^(Colored.to_string cv)^" "
   and end_of_msg = (
      if a=[] then "is uniformly incompatible with "^s_selector else  
      if b=[] then "is uniformly compatible with "^s_selector else   
      let (cv_i,cv_j) = Option.unpack opt_cvs in       
      " splits as "^(Colored.to_string cv_i)
      ^" + "^(Colored.to_string cv_j)) in 
   let msg = beginning_of_msg ^ end_of_msg in 
   (print_string msg ;flush stdout) ;;             

let partition selector cv =
    let old_state = (!main) in 
    let (a,b,opt_cvs) = Colored_variable_assignment.partition selector cv in 
    let _ = explain_partition (selector,cv,a,b,opt_cvs) in 
    let new_state = (
    match opt_cvs with 
    None -> 
      let affty=((selector,cv),b=[]) in 
      Partition_watcher.insert_affinity old_state affty
    |Some(cv_i,cv_j) -> 
        Partition_watcher.expand_using_partition 
        old_state ((selector,cv),(cv_i,cv_j)) 
    ) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Expansion.current n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

let decompose cvar = Partition_watcher.decompose (!main) cvar;;
let ugly_part cvar = Partition_watcher.ugly_part (!main) cvar;;

end ;;  

let deco = Current_partition_watcher.decompose ;;
let oslo_compute = Current_partition_watcher.oslo_compute ;;
let up = Current_partition_watcher.ugly_part ;;

module Marshall_plan = struct 

let detect_missing_affinities n d=         
   let skeleton = Image.image (
     fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
   ) (Expansion.current n d) in 
   let old_state = (!(Current_partition_watcher.main)) in 
   let (PW(affinities,_,ll)) = old_state in 
   let combination2 = List.flatten(Image.image (fun old_pair->
      let (lamb,cvar) = old_pair in 
      match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
       (Some expansion) -> Image.image (fun
        (lamb2,cvar2)-> 
        (Ordered.merge oi lamb lamb2,cvar2)) expansion
      |None -> [old_pair]
      ) skeleton) in 
      fst(Affinity.analyse_combination affinities combination2)  
    ;;  

let  write_partition_commands_from_list l=
  let temp1 = Image.image (fun (lamb,cv)->
   "let passing = Current_partition_watcher.partition "^
   (intlist_to_string lamb)^" ("^(Colored.to_ocaml_readable_string cv)^") ;;"
   )  l in 
  "\n\n\n" ^ (String.concat "\n" temp1) ^ "\n\n\n" ;;   
  
let write_partition_commands n d =
    let missing_affs = detect_missing_affinities n d in 
    print_string(write_partition_commands_from_list missing_affs) ;;  

let rec compute_affinities_then_oslo (n,d) =
   let missing_affs = detect_missing_affinities n d in 
   if missing_affs = []
   then oslo_compute n d 
   else let _ = Image.image (
         fun (lamb,cv) -> Current_partition_watcher.partition lamb cv
        ) missing_affs in 
        compute_affinities_then_oslo (n,d) ;;

let compute_from_scratch n d =
    let temp1 = (Oslo.needed_intermediaries n d)@[n,d] in 
    let _ = Image.image compute_affinities_then_oslo temp1 in 
    Oslo.get n d;;

end ;;   

let wpc = Marshall_plan.write_partition_commands ;;

Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;
Oslo.add (15,3) (Colored_variable_assignment.get (D 0)) ;;
Oslo.add (15,4) (Colored_variable_assignment.get (E 0)) ;;
Oslo.add (15,5) (Colored_variable_assignment.get (F 0)) ;;

Image.image (fun n->
   Marshall_plan.compute_from_scratch n 0  
)(Ennig.ennig 16 23) ;;

(*
let g1 = Oslo.needed_intermediaries 21 0;;
let g2 = [(15, 5); (16, 2); (16, 3); (16, 4); (17, 1); (17, 2); 
 (17, 3); (18, 2);
 (18, 3); (19, 1); (19, 2); (20, 1); (20, 2); (21, 1); (22, 1); (23, 1)] ;;



let upp l=List.filter(fun (x,(i,j))->(up (Og(i,j)))<>[]) l;;

let n0=21 and d0=0;;
let h1 = Expansion.current n0 d0;;
let h2 = Expansion.allowed_substitution_by_in (n0-1,d0+1) h1;;
let h3 = Expansion.allowed_substitution_by_in (n0-2,d0+1) h2;;
*)

