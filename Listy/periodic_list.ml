(*

#use"Listy/periodic_list.ml";;

*)

module Private = struct 

let test_for_rightwards_period l n r =
   let tempf = (fun k->List.nth l (n-k)) in 
   List.for_all (fun j->tempf(j)=tempf(j+r)) (Int_range.range 1 r) ;;
   
let rightwards_period_opt l =
  let n = List.length l in  
  let candidates = Int_range.range 2 (n/2) in 
  Option.seek (test_for_rightwards_period l n) candidates;;
  
let duration_of_rightwards_period l period =
    let n = List.length l in  
    let tempf = (fun k->List.nth l (k-1)) in 
    match Option.seek (fun j->tempf(j)<>tempf(j+period)) 
        (List.rev(Int_range.range 1 (n-period))) with 
    None -> n
    | Some change_idx -> n - change_idx;;
    

let seek l =
   match rightwards_period_opt l with 
    None -> None 
   |Some period ->
      let d = duration_of_rightwards_period l period in 
      let (left,right) = Listennou.big_rht ((List.length l)-d) l in 
      Some(List.rev left,Listennou.big_head period right) ;;

end ;;      

(*        
seek [1;2;1;2;3;4;5;1;2;3;4;5;1;2;3;4;5] ;;      

let l0 = [1;2;1;2;3;4;5;1;2;3;4;5;1;2;3;4;5] ;;  
let n0 = List.length l0 ;;
let p0 = Option.unpack (rightwards_period_opt l0) ;; 
*)


let eventual_period = Private.seek ;; 