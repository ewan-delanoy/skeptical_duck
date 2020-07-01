
(*

#use"Arithmetic/smooth_numbers.ml";;

*)

module Private = struct

let rec helper_for_decomposition (exponents,bases,to_be_treated) =
     match Option.seek (fun (j,p)->to_be_treated mod p=0) bases with 
      None -> (Image.vorstellung snd exponents,to_be_treated)
     |Some(j0,p0) ->  
       let new_exponents =Image.vorstellung (fun (j,e)->if j=j0 then (j,e+1) else (j,e)) exponents in 
       helper_for_decomposition (new_exponents,bases,to_be_treated/p0);;

let decompose n bases =
   let temp1 = Ennig.index_everything  bases in 
   let zeroes = Image.vorstellung (fun (j,_)->(j,0)) temp1 in 
   helper_for_decomposition (zeroes,temp1,n);;

let ref_for_smoothness_params = ref [];;

let hashtbl_for_smooth_numbers_enumeration = Hashtbl.create 1000;;

let set_smoothness_params l=
   if l<>(!ref_for_smoothness_params) 
   then (
         Hashtbl.reset hashtbl_for_smooth_numbers_enumeration; 
         ref_for_smoothness_params:=l;
        );;

let rec next_smooth_number n=
    let (exponents,core_of_n) = decompose n (!ref_for_smoothness_params) in 
    if core_of_n=1 then (n,exponents) else 
    next_smooth_number(n+1);;

let first_smooth_number ()= (1,Image.vorstellung (fun j->0) (!ref_for_smoothness_params));;

let rec enumerate n=
   match Hashtbl.find_opt hashtbl_for_smooth_numbers_enumeration n with 
   Some(v)->v
   |None -> let new_v=(if n<2 then first_smooth_number () else next_smooth_number(fst(enumerate(n-1))+1)) in 
            let _=(Hashtbl.add hashtbl_for_smooth_numbers_enumeration n new_v) in 
            new_v ;;

end ;; 

let decompose = Private.decompose ;;
let enumerate = Private.enumerate ;;
let set_smoothness_params = Private.set_smoothness_params ;;


