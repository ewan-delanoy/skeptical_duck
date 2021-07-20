(*

#use"Old_Van_der_Waerden/vdwfw_main_iterator.ml";;

*)

exception Unregistered of int * int ;;

module Private = struct

let main_ref = ref [] ;;


let fsol  n d = Udwfw_nonempty_index_t.Solution (n,d) ;;

let get (n,d) = 
     if List.mem (n,d) (!main_ref)
     then Vdwfw_environment.get (fsol n d)
     else  raise(Unregistered(n,d)) ;; 

let set (n,d) res =
  let _=(main_ref:=(n,d)::(!main_ref)) in 
  Vdwfw_environment.add_new_assignment  (fsol n d,res) ;;

let expand_template_element ((n,d),linker) =
     let _ = get (n,d)  in
     if linker = []
     then Vdwfw_environment.get (fsol n d)    
     else (fst(Vdwfw_environment.homogeneous_translation (fsol n d) linker));;                

let expand_template l=
    Udwfw_combination.fold_union (Image.image expand_template_element l) ;;

let adhoc_decompose n d =
    if (n,d) =(21,0) then Udwfw_current.rightmost_blowup 21 3 else
    if (n,d) =(24,0) then Udwfw_current.rightmost_blowup 24 8 else 
     Udwfw_current.decompose n d ;;  

let compute_and_remember_in_threshhold_case d =
    let t = Udwfw_current.threshhold in   
    let size = (Udwfw_current.measure t) -d in  
    let data = List.filter (fun x->List.length x = size)
       Udwfw_current.base_for_threshhold in   
    let _ =(Udwfw_variable.set (fsol t d) data;
    main_ref:=(t,d)::(!main_ref)) in 
    Vdwfw_environment.get (fsol t d);;

let compute_and_remember (n,d) = 
     try get (n,d) with 
     _->
     let t = Udwfw_current.threshhold in   
     if n=t 
     then compute_and_remember_in_threshhold_case d
     else let answer =expand_template(adhoc_decompose n d) in 
          let _ = set (n,d) answer in 
          answer;;           

let closest_obstruction (n,d) =
     try (fun _->None)(compute_and_remember (n,d)) with 
     Unregistered (x, y) -> Some (x,y) ;;
     
let rec helper_for_zigzagging  (treated,to_be_treated) = 
   match to_be_treated with 
   [] -> List.rev treated 
   | (n1,d1) :: others ->
      match  closest_obstruction (n1,d1) with 
      None ->  helper_for_zigzagging  ((n1,d1)::treated,others)
      |Some(n2,d2) ->  helper_for_zigzagging  (treated,(n2,d2)::to_be_treated) ;;

let reset_all () = 
     (
         Udwfw_variable.reset();
         Vdwfw_environment.reset();
         main_ref:=[] ;
     ) ;;

let naive_zigzag l =
     let _= reset_all () in 
     helper_for_zigzagging([],l) ;; 

let fixer_for_zigzagging =(fun old_f l->
     if List.length(l)<2 then naive_zigzag l else
     let temp1 = List.rev l in 
     let (last_elt,temp2) = Listennou.ht temp1 in 
     let nonlast_elts = List.rev temp2 in       
     let temp3 = old_f nonlast_elts in 
     naive_zigzag (temp3@[last_elt])
) ;;

let hashtbl_for_zigzagging = Hashtbl.create 500 ;;

let rec zigzag l = match Hashtbl.find_opt hashtbl_for_zigzagging l with 
  Some(old_answer) -> old_answer 
  | None -> let answer = fixer_for_zigzagging zigzag l in 
            let _ = (Hashtbl.add hashtbl_for_zigzagging l answer) in 
            answer ;;

end ;; 

let compute_and_remember = Private.compute_and_remember ;;
let zigzag = Private.zigzag;; 