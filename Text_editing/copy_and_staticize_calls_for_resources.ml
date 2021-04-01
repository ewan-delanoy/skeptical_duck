(*

#use "Text_editing/copy_and_staticize_calls_for_resources.ml";;

This module deals with copying data from a remote website to a local version. 



Modify the list_of_allowed_endings value to suit your purposes.

*)

let list_of_allowed_endings = [".php";".png";".jpg";".css";".js";".woff2"] ;;
let proxies = ["proxy.php"] ;;

exception Remove_question_mark_exn of string ;;
exception No_http_in_url of string ;;


module Private = struct 

let remove_question_mark fn = 
    let temp1 = Substring.occurrences_of_in "?" fn in 
    let d = List.length temp1 in 
    if d>1 then raise(Remove_question_mark_exn fn) else 
    if d=0 then fn else 
    let idx = List.hd temp1 in 
    Cull_string.beginning (idx-1) fn ;;  

let check_one_for_admissibility s=
    let k = Substring.rightmost_index_of_in "/" s in 
    let subdir =Cull_string.beginning k s 
    and qualified_name = Cull_string.cobeginning k s in 
    let name =remove_question_mark qualified_name in
    if  List.exists(Supstring.ends_with name) list_of_allowed_endings 
    then Some(subdir,name,qualified_name)
    else None ;;

let check_several_for_admissibility l =
    let temp1 = Image.image (fun s->(s,check_one_for_admissibility s)) l in 
    let (temp2,temp3) = List.partition (fun (s,opt)->opt<>None) temp1 in
    (Image.image (fun (_,opt)->Option.unpack opt) temp2,Image.image fst temp3) ;;  


let enumerate_calls_for_one_starter text starter =
  let temp1 = Substring.occurrences_of_in starter text in 
  let temp2 =  Image.image (fun idx->
    let idx2 = idx + (String.length starter) in 
    let idx3 = Substring.leftmost_index_of_in_from "\"" text idx2 in 
    Cull_string.interval text idx2 (idx3-1)
  ) temp1 in
  let temp3 = Ordered.sort Total_ordering.lex_for_strings temp2 in 
  let (temp4,temp5)=List.partition (Substring.is_the_beginning_of "/") temp3 in
  let (temp6,temp7)=check_several_for_admissibility temp4 in 
  (temp6,temp5,temp7);;

let enumerate_calls_for_several_starters starters text =
     let temp1 = Image.image (enumerate_calls_for_one_starter text) starters in 
     let temp2 = List.flatten (Image.image (fun (a,b,c)->a) temp1) 
     and temp3 = List.flatten (Image.image (fun (a,b,c)->b) temp1) 
     and temp4 = List.flatten (Image.image (fun (a,b,c)->c) temp1) in 
     (temp2,temp3,temp4) ;;
 
 let decode_url =     
  Replace_inside.replace_several_inside_string 
  ["%3A",":";"%2C",",";"&amp;","&";"%2F","/";"%3F","?";"%3D","="] ;;

 let extract_url line =
      let i1 = Substring.leftmost_index_of_in "http" line in 
      if i1 < 0 then raise(No_http_in_url(line)) else
      let pre_i2 = Substring.leftmost_index_of_in_from "&amp;" line i1 in 
      let i2 = (if pre_i2<1 then (String.length line)+1 else pre_i2) in 
      let pre_i3 = Substring.leftmost_index_of_in_from "%3F" line i1 in 
      let i3 = (if pre_i3<1 then (String.length line)+1 else pre_i3) in 
      let i4 = min i2 i3 in 
      let part = Cull_string.interval line i1 (i4-1) in 
      decode_url part ;;

 let command_for_proxy static_subdir_name (a,b,c) = 
     let url = extract_url c in 
     let j1 = Substring.rightmost_index_of_in "/" url in 
     let fn = Cull_string.cobeginning j1 url in 
     "curl -L \""^url^"\" > "^static_subdir_name^"/"^fn ;;  
     
 let command_with_prescribed_ending (website,static_subdir_name) (k,ending) (a,b,c) = 
     let sk = string_of_int k in 
     "curl -L \""^website^"/"^(decode_url c)^"\" > "^static_subdir_name^"/asset"^sk^"."^ending ;; 

 (*   
 let script_for_triples (list_of_proxies,prescribed_endings,website,static_subdir_name) triples =
      let (temp1,temp2) = List.partition (fun ) triples in  
  *)    
 

  end ;;

let enumerate_all_calls = Private.enumerate_calls_for_several_starters ;;
