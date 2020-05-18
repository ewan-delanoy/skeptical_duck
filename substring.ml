(*

#use"substring.ml";;

*)

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
      try (Option.unpack(Ennig.find_it tester 0 (String.length(y)-lx))+1) with
      _->(-1);;
  
  let rightmost_index_of_in x y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) 
      and temp1=List.rev(Ennig.ennig(0)(String.length(y)-lx)) in
      try ((Listennou.force_find tester temp1)+1) with
      _->(-1);;
  
   let leftmost_index_of_in_from x y i=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      match Ennig.find_it tester (i-1) (String.length(y)-lx) with
         None->(-1)
        |Some(k)->k+1;;
  
module Friend = struct

let number_of_lines_before s i=
   if i<1 then 0 else
   let m=min i (String.length s) in
   List.length(List.filter(fun j->(String.get s (j-1))='\n')(Ennig.ennig 1 m));;


end;;

let leftmost_linedex_of_in x y=
    let j=leftmost_index_of_in x y in
    if j<0 then (-1) else
    Friend.number_of_lines_before y j;;



let leftmost_linedex_of_in_from x y i=
        let j=leftmost_index_of_in_from x y i in
        if j<0 then (-1) else
        Friend.number_of_lines_before y j;;    

 let occurrences_of_in x y=
   let n=String.length y in
   let rec tempf=(fun (j,accu)->
      if j>n then List.rev(accu) else
      let k=leftmost_index_of_in_from x y j in
      if k<0 then List.rev(accu) else
      tempf(k+1,k::accu)
   )  in
   tempf (1,[]);;


let leftmost_index_of_pattern_among_in_from patterns whole_string start_idx=  
    let n=String.length(whole_string) in
    let temp1=Ennig.index_everything patterns in 
    let tester =(fun idx->Option.find_and_stop (
         fun (patt_nbr,patt)->
           if is_a_substring_located_at patt whole_string idx 
           then Some(patt_nbr,idx)
           else None
       ) temp1) in
    Option.find_and_stop tester (Ennig.ennig start_idx n);;          
      
(*

leftmost_index_of_pattern_among_in_from ["uv";"abc";"abcde"] "123abcde90" 1;;

*)

