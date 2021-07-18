(*

#use"Van_der_Waerden/Fixed_width/vdwfw_main_iterator.ml";;

*)

exception Unregistered of int * int ;;

module Private = struct

let main_ref = ref [] ;;


let fsol  n d = Vdwfw_nonempty_index_t.Solution (n,d) ;;

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
    Vdwfw_combination.fold_union (Image.image expand_template_element l) ;;

let compute_and_remember_in_threshhold_case d =
    let t = Vdwfw_current.threshhold in   
    let size = (Vdwfw_current.measure t) -d in  
    let data = List.filter (fun x->List.length x = size)
       Vdwfw_current.base_for_threshhold in   
    let _ =(Vdwfw_variable.set (fsol t d) data;
    main_ref:=(t,d)::(!main_ref)) in 
    Vdwfw_environment.get (fsol t d);;

let compute_and_remember (n,d) = 
     try get (n,d) with 
     _->
     let t = Vdwfw_current.threshhold in   
     if n=t 
     then compute_and_remember_in_threshhold_case d
     else let answer =expand_template(Vdwfw_current.decompose n d) in 
          let _ = set (n,d) answer in 
          answer;;           

let closest_obstruction (n,d) =
     try (fun _->None)(compute_and_remember (n,d)) with 
     Unregistered (x, y) -> Some (x,y) ;;
     
let rec zigzag  (treated,to_be_treated) = 
   match to_be_treated with 
   [] -> List.rev treated 
   | (n1,d1) :: others ->
      match  closest_obstruction (n1,d1) with 
      None ->  zigzag  ((n1,d1)::treated,others)
      |Some(n2,d2) ->  zigzag  (treated,(n2,d2)::to_be_treated) ;;

end ;; 

let compute_and_remember = Private.compute_and_remember ;;
let zigzag l= Private.zigzag ([],l);; 