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

let template (n,d) =
     let delt = Vdwfw_current.measure (n+1) - Vdwfw_current.measure (n) in 
     let optional_part=
      (if (delt=1) && (d=0)
       then None 
       else Some(n-1,d-delt)) in 
     (n-1,d+1-delt,optional_part) ;;     

let expand_template (i,j,optional_part) =
     let _ = get (i,j)  in 
     let (partial,_) = Vdwfw_environment.homogeneous_translation (fsol i j) [i+j]   in 
     match optional_part with 
      None -> partial 
     |Some(i2,j2) -> Vdwfw_combination.union partial (get (i2,j2))  ;;

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
     else expand_template(template (n,d)) ;;           

end ;; 

let compute_and_remember = Private.compute_and_remember ;;