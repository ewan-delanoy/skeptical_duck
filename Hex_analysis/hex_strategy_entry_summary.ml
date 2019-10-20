(* 

#use"Hex_analysis/hex_strategy_entry_summary.ml";;

*)

exception Pil_of_string_exn of string;;
exception Arrowed_of_string_exn of string;;
exception Entry_of_string_exn of string;;


module Private = struct
(* pil is for "pair in linker" *)

let joiner_in_pil = " -|- ";;

let pil_to_string (cell_set,pair_set)= 
   (Hex_cell_set.to_string cell_set)^joiner_in_pil^(Hex_cell_pair_set.to_string pair_set);;

let pil_of_string text =
    match  Cull_string.before_and_after joiner_in_pil text with 
    None -> raise(Pil_of_string_exn(text))
    |Some(before,after) -> (Hex_cell_set.of_string before,Hex_cell_pair_set.of_string after);;

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


let announce_basic_linker ="\nBasic linker : \n";;
let announce_gluing ="\nGluing : \n";;
let announce_disjunction="\nDisjunction : \n";;


let entry_to_string (static_constructor,indices)=
   match static_constructor with 
    Hex_strategy_static_constructor_t.Basic_Linker(cell_set,pair_set)->
         announce_basic_linker^(pil_to_string (cell_set,pair_set))
  | Gluing -> announce_gluing^(Strung.of_intlist indices) 
  | Disjunction (cells)->announce_disjunction^(arrowed_list_to_string (cells,indices));;
;;
    

let basic_linker_of_string text =
   let temp1=Cull_string.two_sided_cutting(announce_basic_linker,"") text in 
   let (cell_set,pair_set)=pil_of_string temp1 in 
   (Hex_strategy_static_constructor_t.Basic_Linker(cell_set,pair_set),[]);;

let gluing_of_string text =
   let temp1=Cull_string.two_sided_cutting(announce_gluing,"") text in 
   let indices=Strung.to_intlist temp1 in 
   (Hex_strategy_static_constructor_t.Gluing,indices);;   

let disjunction_of_string text =
   let temp1=Cull_string.two_sided_cutting(announce_disjunction,"") text in 
   let (cells,indices)=arrowed_list_of_string temp1 in 
   (Hex_strategy_static_constructor_t.Disjunction(cells),indices);;   

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

   