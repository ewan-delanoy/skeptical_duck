(*

#use"lines_in_string.ml";;

*)


module Private = struct 

let core old_s=
   let left_offset=(if Supstring.begins_with old_s "\n" then "\n" else "")
   and right_offset=(if Supstring.ends_with old_s "\n" then "\n" else "") in
   let s=left_offset^old_s^right_offset in
   let temp1=Str.split (Str.regexp_string "\n") s in
   Ennig.index_everything temp1;;

let rec iterator_for_enchancement (num_of_treated_chars,treated_lines,lines) =
     match lines with 
     [] -> List.rev treated_lines 
     |(line_idx,line) :: other_lines ->
      iterator_for_enchancement 
      (num_of_treated_chars+(String.length line)+1,
       (num_of_treated_chars,line_idx,line)::treated_lines,other_lines)   ;;
      
let enhance indexed_lines =  iterator_for_enchancement (0,[],indexed_lines );;      

end ;;   

let enhanced_core s= Private.enhance (Private.core s);;

(*

enhanced_core "a\nb";;
enhanced_core "\na\nb";;
enhanced_core "a\nb\n";;

*)

let core = Private.core ;;

(*

core "a\nb";;
core "\na\nb";;
core "a\nb\n";;

*)

let lines s= Image.image snd (core s);;

let interval s i j=
    let temp1=core s in
    let temp2=List.filter (fun (k,_)->(i<=k)&&(k<=j)) temp1  in
    let temp3=Image.image snd temp2 in
    String.concat "\n" temp3;; 


exception Lines_in_char_range_exn of int*int;;

let number_of_lines_in_char_interval s  i j=
   try (List.length(List.filter (fun k->
       String.get s (k-1)='\n'
   ) (Ennig.ennig i j))) with
   _->raise(Lines_in_char_range_exn(i,j));; 

let line_index_from_char_index s char_idx=
  1+(number_of_lines_in_char_interval s 1 char_idx);;



let remove_interval s i j=
  let temp1=core s in
  let temp2=List.filter (fun (k,_)->(i>k)||(k>j)) temp1  in
  let temp3=Image.image snd temp2 in
  String.concat "\n" temp3;; 

let remove_interval_in_file fn i j=
    let s1=Io.read_whole_file fn in
    let s2=remove_interval s1 i j  in
   Io.overwrite_with fn s2;;   

let tripartition_associated_to_interval s i j=
   let temp2=lines s in 
   let (temp3,temp4)=Listennou.big_rht (i-1) temp2 in 
   let part1=String.concat "\n" (List.rev temp3) in 
   let (temp5,temp6)=Listennou.big_rht (j-i+1) temp4 in 
   let part2=String.concat "\n" (List.rev temp5) in 
   let part3=String.concat "\n" temp6 in 
   (part1^"\n",part2,"\n"^part3);;

(* tripartition_associated_to_interval "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)
