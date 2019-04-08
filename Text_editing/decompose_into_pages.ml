(*


#use"Text_editing/decompose_into_pages.ml";;

*)

let seek_page_number_description_in_line s start_idx =
  let n=String.length s in 
  let opt1=Option.seek (
     fun k->let c=Strung.get s k in 
     not(List.mem c [' ';'\n';'\t';'r'])
  ) (Ennig.ennig start_idx n) in 
  if opt1=None then None else 
  let idx1=Option.unpack opt1 in 
  if (Strung.get s idx1)<>'p' then None else 
  let opt2=Option.seek (
     fun k->let c=Strung.get s k in 
     not(List.mem c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'])
  ) (Ennig.ennig (idx1+1) n) in 
  let end_idx=(match opt2 with 
     None->n
     |Some(idx2)->idx2-1
  ) in    
  let pagenumber_description=Cull_string.interval s (idx1+1) end_idx in 
  let pagenumber=int_of_string pagenumber_description in 
  Some(pagenumber);;

(*

seek_page_number_description_in_line " \t p674(hum)  \n\n" 1;;

*)

let dip s =
  let temp1=Lines_in_string.core s in 
  let temp2=Image.image (
    fun (_,line)->(line,seek_page_number_description_in_line line 1)
  ) temp1 in 
  let (first_one,usual_ones)=Three_parts.decompose_according_to_beginning_markers
    (fun (line,opt_page)->opt_page<>None) temp2 in 
  let tempf1=(fun l->String.concat "\n" (Image.image fst l)) in   
  (tempf1 first_one, Image.image (fun ((_,opt),l)->(Option.unpack(opt),tempf1 l)) usual_ones);;  

(*

dip "abc\nAny more\nI can hear\n\np45\n\n def \n\np47\n\n ij \n\np49\n\n kl";;

*)

