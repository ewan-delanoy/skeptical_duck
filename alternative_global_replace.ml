(*


#use"alternative_global_replace.ml";;

The my_global_replace is a replacement for Ocaml's Str.global_replace which has
the disadvantage of applying certain transforms to the replacement string.


*)




let single_char_special_case (single_c,b) s=
    let n=String.length(s) in
    let temp1=Ennig.doyle (
       fun j->let c=String.get s j in
              if c=single_c
              then b
              else String.make 1 c 
    ) 0 (n-1) in
    String.concat "" temp1;;

exception Ambiguity of string*int*int;;

let my_global_replace (a,b) s=
  let n=String.length(s) and na=String.length(a) in
  if na=1 then single_char_special_case (String.get a 0,b) s else
  let indices=Substring.occurrences_of_in a s in
  if indices=[] then s else
  let delta_indices = Listennou.universal_delta_list indices in 
  let opt_ambiguity=Option.seek (fun (start1,start2)->start2<start1+na) delta_indices in 
  if  opt_ambiguity<>None
  then let (start1,start2)=Option.unpack opt_ambiguity in 
       raise(Ambiguity(a,start1,start2))
  else  
  let m=List.length indices in 
  let lower_end=(fun j->if j=0 then 1 else List.nth indices (j-1)+na) 
  and upper_end=(fun j->if j=m then n else (List.nth indices (j))-1) in 
  let unchanged_intervals = Ennig.doyle (fun j->(lower_end j,upper_end j)) 0 m in 
  let unchanged_substrings=Image.vorstellung 
     (fun (x,y)->if x>y then "" else Cull_string.interval s x y) unchanged_intervals in
  String.concat b unchanged_substrings;;
  
(*  
my_global_replace ("ab","cd") "12345ab6ab78cd91234ab679";; 
my_global_replace ("ab","cd") "ab12345ab6ab78cd91234ab679";; 
my_global_replace ("ab","cd") "12345ab6ab78cd91234ab679ab";;
my_global_replace ("1111","") "abc1111111111def";;
my_global_replace ("ab","cd") "xyz";;
my_global_replace ("a","b") "1aa2";; 
my_global_replace ("uv","w") "1uvuv2";; 

*)  
  
           