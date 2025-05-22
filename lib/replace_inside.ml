(*

#use"lib/replace_inside.ml";;

*)

exception Ambiguity of string*int*int;;


module Private = struct 
 
  let substring_leftmost_index_from=(fun x y i0->
    let lx=String.length(x) and ly=String.length(y) in
    let rec tempf=(fun j->
      if j>ly-lx then (-1) else 
      if (String.sub y j lx)=x then j else (tempf(j+1))
    ) in
    tempf i0) ;;
(*

The my_global_replace function below is a replacement for Ocaml's Str.global_replace which has
the disadvantage of applying certain transforms to the replacement string.

*)

let single_char_special_case (single_c,b) s=
  let n=String.length(s) and counter=ref(0) in
  let temp1=Int_range.scale (
     fun j->let c=String.get s j in
            if c=single_c
            then let _=(counter:=(!counter)+1) in b
            else String.make 1 c 
  ) 0 (n-1) in
  (String.concat "" temp1,!counter);;

let global_replace_with_number_of_matches (a,b) s=
let n=String.length(s) and na=String.length(a) in
if na=1 then single_char_special_case (String.get a 0,b) s else
let indices=Substring.occurrences_of_in a s in
if indices=[] then (s,0) else
let delta_indices = List_again.universal_delta_list indices in 
let opt_ambiguity=List.find_opt (fun (start1,start2)->start2<start1+na) delta_indices in 
if  opt_ambiguity<>None
then let (start1,start2)=Option.get opt_ambiguity in 
     raise(Ambiguity(a,start1,start2))
else  
let m=List.length indices in 
let lower_end=(fun j->if j=0 then 1 else List.nth indices (j-1)+na) 
and upper_end=(fun j->if j=m then n else (List.nth indices (j))-1) in 
let unchanged_intervals = Int_range.scale (fun j->(lower_end j,upper_end j)) 0 m in 
let unchanged_substrings=Image.image 
   (fun (x,y)->if x>y then "" else Cull_string.interval s x y) unchanged_intervals in
(String.concat b unchanged_substrings,m);;

let text_for_number_of_replacements k=
  if k = 0 then "No replacement made" else
  if k = 1 then "1 replacement made" else 
  (string_of_int k)^" replacements made" ;;   

let my_global_replace_with_raising display_number_of_matches (a,b) old_s  =
   let (new_s,count) = global_replace_with_number_of_matches (a,b) old_s in 
   let _ =(
      if display_number_of_matches 
      then print_string((text_for_number_of_replacements count)^" for "^a^" -> "^b^"\n"); 
           flush stdout 
   ) in 
   new_s ;; 

let my_global_replace_without_raising display_number_of_matches (a,b) old_s  =
   try my_global_replace_with_raising display_number_of_matches (a,b) old_s with 
   Ambiguity(_,_,_) -> old_s ;; 

let my_global_replace display_number_of_matches silent_on_ambiguity (a,b) old_s  = 
  if silent_on_ambiguity 
  then my_global_replace_without_raising display_number_of_matches (a,b) old_s 
  else my_global_replace_with_raising display_number_of_matches (a,b) old_s ;;   
  

(*  
my_global_replace ("ab","cd") "12345ab6ab78cd91234ab679";; 
my_global_replace ("ab","cd") "ab12345ab6ab78cd91234ab679";; 
my_global_replace ("ab","cd") "12345ab6ab78cd91234ab679ab";;
my_global_replace ("1111","") "abc1111111111def";;
my_global_replace ("ab","cd") "xyz";;
my_global_replace ("a","b") "1aa2";; 
my_global_replace ("uv","w") "1uvuv2";; 

*)  

let pair_for_commenting_or_uncommenting = 
  ("\n(*\n","\n*)\n") ;;

end ;;

let at_char_intervals_inside_text text l=
  if l=[] then text else
  let n=String.length text in
  let temp1=List_again.universal_delta_list l 
  and ((i_first,_),_)=List.hd(l)
  and ((_i_last,j_last),rep_last)=List.hd(List.rev l) in
  let temp2=Image.image (fun (((_i1,j1),rep1),((i2,_j2),_rep2))->
      rep1^(String.sub text j1 (i2-j1-1))
  ) temp1 in
  let first_part=(String.sub text 0 (i_first-1))
  and last_part=rep_last^(String.sub text j_last (n-j_last)) in
  first_part^(String.concat "" temp2)^last_part;;

let at_char_intervals_inside_file 
  fn l=
   let s1=Io.read_whole_file fn in
   let s2=at_char_intervals_inside_text s1 l in
   Io.overwrite_with fn s2;;     

(*    

at_char_intervals_inside_text "12345678901234567890" [(3,5),"right";(12,17),"again"];;

*)         

let comment_out_between_markers_inside_text (bm,em) text=
     let (before,between,after) = Cull_string.tripartition_using_markers (bm,em) text in
     let (opener,closer) = Private.pair_for_commenting_or_uncommenting in 
     before^bm^opener^between^closer^em^after ;; 

let comment_out_between_markers_inside_file (bm,em) fn =
  let old_text=Io.read_whole_file fn in
  let new_text=comment_out_between_markers_inside_text (bm,em) old_text in
  Io.overwrite_with fn new_text;; 

let overwrite_and_dump_markers_inside_text ~overwriter:b (bm,em) text=
     if (bm,em)=("","") then b else
      let (before,_between,after) = Cull_string.tripartition_using_markers (bm,em) text in
      before^bm^b^em^after ;; 
     
let overwrite_and_dump_markers_inside_file 
~overwriter:b (bm,em)
   fn =
    let old_text=Io.read_whole_file fn in
    let new_text=overwrite_and_dump_markers_inside_text ~overwriter:b (bm,em) old_text in
    Io.overwrite_with fn new_text;;      
 
(* 


 overwrite_between_markers_inside_text
  (~overwriter:"456")
  ("aaa","bb")
   "123aaa5678bb78910" ;;    
   
overwrite_and_dump_markers_inside_text
  (~overwriter:"456")
  ("aaa","bb")
   "123aaa5678bb78910" ;;       
   
     
*)

let overwrite_between_markers_inside_text ~overwriter:b (bm,em) text=
     if (bm,em)=("","") then b else
     let (before,_between,after) = Cull_string.tripartition_using_markers (bm,em) text in
     before^bm^b^em^after ;; 
     
let overwrite_between_markers_inside_file 
   ~overwriter:b (bm,em)
   fn =
    let old_text=Io.read_whole_file fn in
    let new_text=overwrite_between_markers_inside_text ~overwriter:b (bm,em) old_text in
    Io.overwrite_with fn new_text;;      


let replace_inside_text ?(display_number_of_matches=true) ?(silent_on_ambiguity=false) (a,b) text=
  Private.my_global_replace display_number_of_matches silent_on_ambiguity (a,b) text ;;
 
let replace_several_inside_text ?(display_number_of_matches=false) ?(silent_on_ambiguity=false) l t=List.fold_left 
(fun s (a,b)->Private.my_global_replace display_number_of_matches silent_on_ambiguity (a,b) s ) t l;;  
 
let replace_inside_file ?(display_number_of_matches=true) ?(silent_on_ambiguity=false) (a,b) fn=
    let old_text=Io.read_whole_file fn in
    let la=String.length(a) in
    if List.exists (fun j->(String.sub old_text j la)=a) (Int_range.range 0 ((String.length old_text)-la))
    then let new_text=replace_inside_text ~display_number_of_matches ~silent_on_ambiguity (a,b) old_text in
         Io.overwrite_with fn new_text
    else ();; 


let replace_several_inside_file ?(display_number_of_matches=false) ?(silent_on_ambiguity=false) l fn=
    let old_text=Io.read_whole_file fn in
    let new_text=replace_several_inside_text ~display_number_of_matches ~silent_on_ambiguity l old_text  in
    Io.overwrite_with fn new_text;; 

let uncomment_between_markers_inside_text (bm,em) text=
      let (before,between,after) = Cull_string.tripartition_using_markers (bm,em) text in
      before^bm^(
       Cull_string.two_sided_cutting Private.pair_for_commenting_or_uncommenting  
      between)^em^after ;; 
 

 let uncomment_between_markers_inside_file (bm,em) fn =
   let old_text=Io.read_whole_file fn in
   let new_text=uncomment_between_markers_inside_text (bm,em) old_text in
   Io.overwrite_with fn new_text;; 



