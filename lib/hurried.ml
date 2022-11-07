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