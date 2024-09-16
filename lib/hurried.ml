(*

#use"lib/hurried.ml";;

*)

module Private = struct

let partition_in_two_parts f l=
  let rec tempf=(fun
   (graet,da_ober)->match da_ober with
     []->(List.rev graet,[])
     |a::peurrest->
        if f(a)
        then tempf(a::graet,peurrest)
        else (List.rev graet,da_ober)
  ) in
  tempf([],l);; 

(* partition_in_two_parts (fun x->(x mod 3)<>0) (Ennig.ennig 1 21) *)
let rec helper_for_reaggregation 
      decomposer (treated,current_level,incomplete_item,to_be_treated) = 
    match to_be_treated with 
    [] -> List.rev((current_level,List.rev incomplete_item)::treated)  
    |pair :: other_pairs ->
        let (new_level,other_data) = decomposer pair in 
        if new_level = current_level 
        then helper_for_reaggregation 
        decomposer (treated,current_level,
                     other_data::incomplete_item,other_pairs)
        else helper_for_reaggregation 
        decomposer ((current_level,List.rev incomplete_item)::treated,
                      new_level,[other_data],other_pairs) ;;            

let reaggregate decomposer l = 
  match l with 
  [] -> []
  |pair :: other_pairs ->
    let (new_level,other_data) = decomposer pair in 
    helper_for_reaggregation 
    decomposer ([],new_level,[other_data],other_pairs) ;;

(*

reaggregate (fun x->(x/10,x mod 10)) (Int_range.range 1 50) ;; 

*)    


end ;; 




let connected_components f l = 
  let rec tempf = (fun 
    (treated,to_be_treated) -> match to_be_treated with 
       [] -> List.rev treated 
     | a :: others ->
        let fa = f a in 
        let (left,right) = Private.partition_in_two_parts (fun x-> f x=fa) others in 
        tempf((a::left)::treated,right)
  ) in 
  tempf([],l) ;;

(* connected_components (fun x->(x mod 3)<>0) (Ennig.ennig 1 21) *)  

let partition_in_two_parts = Private.partition_in_two_parts ;;    

let reaggregate = Private.reaggregate ;;