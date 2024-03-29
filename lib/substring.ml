(*

#use"lib/substring.ml";;

*)

module Private = struct 

   let leftmost_index_of_in_from x y i=
   let lx=String.length(x) in
   let tester=(function j->(String.sub y j lx)=x) in
   match Int_range.find_opt tester (i-1) (String.length(y)-lx) with
      None->(-1)
     |Some(k)->k+1;;

let occurrences_of_in x y=
   let n=String.length y in
   let rec tempf=(fun (j,accu)->
      if j>n then List.rev(accu) else
      let k=leftmost_index_of_in_from x y j in
      if k<0 then List.rev(accu) else
      tempf(k+1,k::accu)
   )  in
   tempf (1,[]);;

let ranges_for_occurrences_of_in x y=
   let m=String.length x in
   let temp1 = occurrences_of_in x y in 
   Image.image (fun i->(i,i+m-1)) temp1;;  


end ;;    

let decorated_occurrences_of_in x y =
   let ny = String.length y 
   and ranges = Private.ranges_for_occurrences_of_in x y in  
   let arranged_ranges = Image.image (
     fun (old_i,old_j)->
        (max(1)(old_i-5),min(ny)(old_j+150))
   ) ranges in 
   Image.image (fun (a,b)->String.sub y (a-1) (b-a+1)) arranged_ranges ;;

 let is_the_beginning_of y x=Supstring.begins_with x y;;     

   
 let is_the_ending_of y x=Supstring.ends_with x y;;  

 let is_a_substring_located_at y x old_j =
    let j=old_j-1 in
    let ly=String.length(y) in
      if (String.length(x)<j+ly)||(j<0)
      then false
      else (String.sub x j ly)=y;;
 
  let is_a_substring_of x y=Supstring.contains y x;; 
      
  let leftmost_index_of_in x y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      try (Option.get(Int_range.find_opt tester 0 (String.length(y)-lx))+1) with
      _->(-1);;
  
  let rightmost_index_of_in x y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) 
      and temp1=List.rev(Int_range.range(0)(String.length(y)-lx)) in
      try ((List.find tester temp1)+1) with
      _->(-1);;
  
   let leftmost_index_of_in_from = Private.leftmost_index_of_in_from ;;
  
module Friend = struct

let number_of_lines_before s i=
   if i<1 then 0 else
   let m=min i (String.length s) in
   List.length(List.filter(fun j->(String.get s (j-1))='\n')(Int_range.range 1 m));;


end;;

let leftmost_linedex_of_in x y=
    let j=leftmost_index_of_in x y in
    if j<0 then (-1) else
    Friend.number_of_lines_before y j;;



let leftmost_linedex_of_in_from x y i=
        let j=leftmost_index_of_in_from x y i in
        if j<0 then (-1) else
        Friend.number_of_lines_before y j;;    



let leftmost_index_of_pattern_among_in_from patterns whole_string start_idx=  
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