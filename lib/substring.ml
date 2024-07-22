(*

#use"lib/substring.ml";;

*)



module Private = struct 

   let leftmost_index_of_in_from_opt x y i=
   let lx=String.length(x) in
   let tester=(function j->(String.sub y j lx)=x) in
   match Int_range.find_opt tester (i-1) (String.length(y)-lx) with
      None->None
     |Some(k)->Some(k+1);;

let occurrences_of_in x y=
   let n=String.length y in
   let rec tempf=(fun (j,accu)->
      if j>n then List.rev(accu) else
      match leftmost_index_of_in_from_opt x y j with
      None -> List.rev(accu)
      | Some k -> tempf(k+1,k::accu)
   )  in
   tempf (1,[]);;

let ranges_for_occurrences_of_in x y=
   let m=String.length x in
   let temp1 = occurrences_of_in x y in 
   Image.image (fun i->(i,i+m-1)) temp1;;  


end ;;    

let leftmost_index_of_in_from_opt = Private.leftmost_index_of_in_from_opt ;; 

let decorated_occurrences_of_in x y =
   let ny = String.length y 
   and ranges = Private.ranges_for_occurrences_of_in x y in  
   let arranged_ranges = Image.image (
     fun (old_i,old_j)->
        (max(1)(old_i-5),min(ny)(old_j+150))
   ) ranges in 
   Image.image (fun (a,b)->String.sub y (a-1) (b-a+1)) arranged_ranges ;;

 
 let is_a_substring_located_at y x old_j =
    let j=old_j-1 in
    let ly=String.length(y) in
      if (String.length(x)<j+ly)||(j<0)
      then false
      else (String.sub x j ly)=y;;
      
  let is_a_substring_of x y=
   let lx=String.length(x) in
   let tester=(function j->(String.sub y j lx)=x) in
   Int_range.exists tester 0 (String.length(y)-lx);;               

  let rightmost_index_of_in_opt x y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) 
      and temp1=List.rev(Int_range.range(0)(String.length(y)-lx)) in
      try Some((List.find tester temp1)+1) with
      _->None;;




let leftmost_index_of_pattern_among_in_from_opt patterns whole_string start_idx=  
    let n=String.length(whole_string) in
    let temp1=Int_range.index_everything patterns in 
    let tester =(fun idx->List.find_map (
         fun (patt_nbr,patt)->
           if is_a_substring_located_at patt whole_string idx 
           then Some(patt_nbr,idx)
           else None
       ) temp1) in
    List.find_map tester (Int_range.range start_idx n);;          
      
(*

leftmost_index_of_pattern_among_in_from ["uv";"abc";"abcde"] "123abcde90" 1;;

*)

let occurrences_of_in = Private.occurrences_of_in ;;

let ranges_for_occurrences_of_in = Private.ranges_for_occurrences_of_in ;;   