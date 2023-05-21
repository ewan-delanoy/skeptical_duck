(*

#use"lib/Listy/partition_list.ml";;

*)

module Private = struct 

let rec select_first_colleagues f (treated,img,to_be_treated) =   
    match to_be_treated with 
    [] -> (List.rev treated,to_be_treated)
    | item :: others -> 
        if f item = img 
        then select_first_colleagues f (item::treated,img,others) 
        else (List.rev treated,to_be_treated) ;; 

let according_to_map_assuming_connectedness pairs f=
    let rec tempf = (fun (already_treated,to_be_treated)->
      match to_be_treated with 
      [] -> List.rev already_treated 
    | pair :: others ->
      let img1 = f pair in  
      let (part1,part2) = select_first_colleagues f ([pair],img1,others) in 
               tempf ((img1,part1)::already_treated,part2)     
    ) in 
    tempf ([],pairs) ;;

(*

according_to_map_assuming_connectedness [1,7;1,8;2,9;3,10;3,11;1,12] fst ;; 

*)

let according_to_map_without_assuming_connectedness pairs f=
  let rec tempf = (fun (already_treated,to_be_treated)->
       match to_be_treated with 
        [] -> List.rev already_treated 
       | pair :: _ ->
         let img1 = f pair in  
         let (part1,part2) = List.partition (fun pair2->f pair2=img1) to_be_treated in 
         tempf ((img1,part1)::already_treated,part2)     
   ) in 
   tempf ([],pairs) ;;

(*

according_to_map_without_assuming_connectedness [1,7;1,8;2,9;3,10;3,11;1,12] fst ;; 

*)   

let according_to_map pairs ?(assume_connectedness=false) f  =
   if assume_connectedness 
   then according_to_map_assuming_connectedness pairs f
   else according_to_map_without_assuming_connectedness pairs f ;;  

end ;;  

let according_to_fst ?(assume_connectedness=false) l = 
    let temp = Private.according_to_map l ~assume_connectedness fst in 
    Image.image (fun (img,antecs)->(img,Image.image snd antecs)) temp;; 

let according_to_map = Private.according_to_map ;;


let from_set_of_ranges l n=
    if l=[] then [1,n,false] else 
    let (last_i,last_j)=List.hd(List.rev l) 
    and (first_i,_)=List.hd l in
    let temp2=List_again.universal_delta_list l in  
    let temp3=Image.image (fun ((i1,j1),(i2,_j2))->
      [(i1,j1,true);(j1+1,i2-1,false)]
    ) temp2 in 
    let middle_part=List.flatten temp3 in
    let first_part=(if first_i>1 then [(1,first_i-1,false)] else []) 
    and last_part=(if last_j<n then [(last_j+1,n,false)] else []) in 
    first_part@middle_part@[(last_i,last_j,true)]@last_part;;

(*

from_set_of_ranges [(3,7);(41,52)] 100;;
from_set_of_ranges [(1,7);(41,52)] 100;;

*)

let split_in_half l=
   let temp1=Int_range.index_everything(l) in 
   let (temp2,temp3)=List.partition (fun (j,_)->(j mod 2)=1) temp1 in 
   (Image.image snd temp2,Image.image snd temp3);;

(*

split_list_in_half [1; 2; 3; 4; 5; 6; 7];;
split_list_in_half [1; 2; 3; 4; 5; 6; 7; 8];;

*)   