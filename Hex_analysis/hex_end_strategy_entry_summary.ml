(* 

#use"Hex_analysis/hex_strategy_entry_summary.ml";;

*)


(*
exception Til_of_string_exn of string * (string list);;
exception Arrowed_of_string_exn of string;;
exception Glued_content_of_string_exn of string;;
exception Disjuncted_content_of_string_exn of string;;
exception Entry_of_string_exn of string;;


module Private = struct
(* til is for "triple in linker" *)

let joiner_in_til = " |||\n\n";;

let til_to_string (comment,cell_set,pair_set)= 
   comment^joiner_in_til^(Hex_cell_set.to_string cell_set)^joiner_in_til^(Hex_cell_pair_set.to_string pair_set);;

let til_of_string text =
    let temp1=Cull_string.extract_intervals_in_wrt_separator (" "^text) joiner_in_til in 
    if List.length(temp1)<>3
    then raise(Til_of_string_exn(text,temp1))
    else let tf=(fun j->List.nth temp1 (j-1)) in
    (Cull_string.cobeginning 1 (tf 1),Hex_cell_set.of_string (tf 2),Hex_cell_pair_set.of_string (tf 3));;

let inside_arrowed="->";;
let outside_arrowed=" , ";;

let arrowed_to_string (cell,idx)=(Hex_cell.to_string cell)^inside_arrowed^(string_of_int idx);;
let arrowed_of_string text =
    match  Cull_string.before_and_after inside_arrowed text with 
    None -> raise(Arrowed_of_string_exn(text))
    |Some(before,after) -> (Hex_cell.of_string before,int_of_string after);;


let arrowed_list_to_string (cells,indices)=
   let temp1=List.combine cells indices in 
   let temp2=Image.image arrowed_to_string temp1 in
   String.concat outside_arrowed temp2;;

let arrowed_list_of_string text=
   let temp1=Cull_string.extract_intervals_in_wrt_separator text outside_arrowed in 
   let temp2=Image.image arrowed_of_string temp1 in 
   (Image.image fst temp2,Image.image snd temp2);;

let joiner_for_comment = " cc "

let glued_content_to_string (comment,indices)=
   comment ^ joiner_for_comment ^(Strung.of_intlist indices);;

let glued_content_of_string text =
     match  Cull_string.before_and_after joiner_for_comment text with 
    None -> raise(Glued_content_of_string_exn(text))
    |Some(comments,after) -> 
         let indices=Strung.to_intlist after in 
         (comments,indices);;

let disjuncted_content_to_string (comment,cells,indices)=
   comment ^ joiner_for_comment ^ (arrowed_list_to_string (cells,indices));;

let disjuncted_content_of_string text =
     match  Cull_string.before_and_after joiner_for_comment text with 
    None -> raise(Disjuncted_content_of_string_exn(text))
    |Some(comments,after) -> 
         let (cells,indices)=arrowed_list_of_string after in 
         (comments,cells,indices);;

let announce_basic_linker ="\nBasic linker : \n";;
let announce_gluing ="\nGluing : \n";;
let announce_disjunction="\nDisjunction : \n";;


let entry_to_string (static_constructor,comment,indices)=
   match static_constructor with 
    Hex_strategy_static_constructor_t.Basic_Linker(cell_set,pair_set)->
         announce_basic_linker^(til_to_string (comment,cell_set,pair_set))
  | Gluing -> announce_gluing^(glued_content_to_string (comment,indices)) 
  | Disjunction (cells)->announce_disjunction^(disjuncted_content_to_string (comment,cells,indices));;
;;
    

let basic_linker_of_string text =
   let temp1=Cull_string.two_sided_cutting(announce_basic_linker,"") text in 
   let (comment,cell_set,pair_set)=til_of_string temp1 in 
   (Hex_strategy_static_constructor_t.Basic_Linker(cell_set,pair_set),comment,[]);;

let gluing_of_string text =
   let temp1=Cull_string.two_sided_cutting(announce_gluing,"") text in 
   let (comment,indices)=glued_content_of_string temp1 in 
   (Hex_strategy_static_constructor_t.Gluing,comment,indices);;   

let disjunction_of_string text =
   let temp1=Cull_string.two_sided_cutting(announce_disjunction,"") text in 
   let (comment,cells,indices)=disjuncted_content_of_string temp1 in 
   (Hex_strategy_static_constructor_t.Disjunction(cells),comment,indices);;   

let entry_of_string text=
   if Supstring.begins_with text announce_basic_linker then basic_linker_of_string text else 
   if Supstring.begins_with text announce_gluing       then gluing_of_string text else 
   if Supstring.begins_with text announce_disjunction  then disjunction_of_string text else 
   raise(Entry_of_string_exn(text));;
   
let joiner_in_entry_list="\n ** \n";;

let list_to_string l= String.concat joiner_in_entry_list (Image.image entry_to_string l);;
let list_of_string text =
   let temp1=Cull_string.extract_intervals_in_wrt_separator text joiner_in_entry_list in 
   Image.image entry_of_string temp1;; 
   

end ;;

let list_to_string = Private.list_to_string;;
let list_of_string = Private.list_of_string;;

*)   