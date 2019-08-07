(* 

#use"Concrete_ocaml_objects/concrete_object.ml";;


*)


module Private = struct

let salt = "aY2ukkwDzyKd";;

let array_opener = salt ^ "ao";;
let list_opener = salt ^ "lo";;
let record_opener = salt ^ "ro";;
let string_opener = salt ^ "so";;
let uple_opener = salt ^ "uo";; 
let variant_opener = salt ^ "vo";; 

let array_separator = salt ^ "as";;
let list_separator = salt ^ "ls";;
let record_separator = salt ^ "rs";;
let uple_separator = salt ^ "us";; 
let variant_separator = salt ^ "vs";; 

let array_closer = salt ^ "ac";;
let list_closer = salt ^ "lc";;
let record_closer = salt ^ "rc";;
let string_closer = salt ^ "sc";;
let uple_closer = salt ^ "uc";; 
let variant_closer = salt ^ "vc";; 

let record_arrow = salt ^ "ra";;

let array_triple = (array_opener,array_separator,array_closer);;
let list_triple = (list_opener,list_separator,list_closer);;
let record_triple = (record_opener,record_separator,record_closer);;
let uple_triple = (uple_opener,uple_separator,uple_closer);;
let variant_triple = (variant_opener,variant_separator,variant_closer);;

end;;

module Old = struct 

open Private;;

exception Nonfull_extraction of int*string;;

let force_extraction_to_be_full 
   s (extracted_obj,next_idx)=
     if next_idx<(String.length s)
     then raise(Nonfull_extraction(next_idx,s))
     else extracted_obj;;
     
exception Missing_record_arrow of int*string;;

let parse_record_item older_extractor s=
   let idx1=Substring.leftmost_index_of_in record_arrow s in 
   if idx1<0
   then raise(Missing_record_arrow(idx1,s))
   else 
   let idx2=idx1+String.length(record_arrow) in
   let end_of_s=Cull_string.cobeginning (idx2-1) s in  
   let key=Cull_string.beginning (idx1-1) s in 
   let recorded_obj=force_extraction_to_be_full end_of_s (older_extractor end_of_s) in 
   (key,recorded_obj);;


exception Extract_initial_int_exn of string;;
exception Missing_string_opener of string;;
exception Missing_string_closer of string;;

let extract_initial_int s=
   (* we assume that the first character is ok *)
   let n=String.length s in 
   let next_idx=(
   match Option.seek(
    fun k->let c=Strung.get s k in 
    not(List.mem c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'])
   ) (Ennig.ennig 2 n) with 
   None->n+1
   |Some(idx)->idx) in 
   (Concrete_object_t.Int(int_of_string(Cull_string.beginning (next_idx-1) s)),next_idx);;



let extract_initial_string s=
   if not(Supstring.begins_with s string_opener) 
   then raise(Missing_string_opener(s))
   else 
   let idx1=(String.length string_opener)+1 in 
   let idx2=Substring.leftmost_index_of_in_from string_closer s idx1 in 
   if idx2<0
   then raise(Missing_string_closer(s))
   else
   (Concrete_object_t.String(Cull_string.interval s idx1 (idx2-1)),idx2+(String.length string_closer));;

let extract_initial_array older_extractor s=
   let (items,(i_start,i_end))=Strung.parse_nested_parentheses array_triple s in 
   let temp1=Image.image (fun t->force_extraction_to_be_full t (older_extractor t)) items in 
   (Concrete_object_t.Array(temp1),i_end+1);;

let extract_initial_list older_extractor s=
   let (items,(i_start,i_end))=Strung.parse_nested_parentheses list_triple s in 
   let temp1=Image.image (fun t->force_extraction_to_be_full t (older_extractor t)) items in 
   (Concrete_object_t.List(temp1),i_end+1);;



let extract_initial_record older_extractor s=
   let (items,(i_start,i_end))=Strung.parse_nested_parentheses record_triple s in 
   let temp1=Image.image (parse_record_item older_extractor) items in 
   (Concrete_object_t.Record(temp1),i_end+1);;

let extract_initial_uple older_extractor s=
   let (items,(i_start,i_end))=Strung.parse_nested_parentheses uple_triple s in 
   let temp1=Image.image (fun t->force_extraction_to_be_full t (older_extractor t)) items in 
   (Concrete_object_t.Uple(temp1),i_end+1);;

let extract_initial_variant older_extractor s=
   let (unchecked_items,(i_start,i_end))=Strung.parse_nested_parentheses variant_triple s in 
   let constructor=Cull_string.beginning (i_start-1) s in 
   let items = List.filter (fun u->u<>"") unchecked_items in 
   let temp1=Image.image (fun t->force_extraction_to_be_full t (older_extractor t)) items in 
   (Concrete_object_t.Variant(constructor,temp1),i_end+1);;

exception Empty_argument_in_extraction;;
exception Unknown_beginning of string;;

let fixer_in_initial_object_extraction older_extractor s=
   if s="" then raise(Empty_argument_in_extraction) else 
   let c=String.get s 0 in 
   let ic=int_of_char(c) in 
   if ((48<=ic)&&(ic<=57)) (* char between 0 and 9 *)
      ||
      (c='-')
   then extract_initial_int s 
   else 
   if Supstring.begins_with s string_opener
   then extract_initial_string s
   else 
   if Supstring.begins_with s uple_opener
   then extract_initial_uple older_extractor s
   else 
   if Supstring.begins_with s list_opener
   then extract_initial_list older_extractor s
   else 
   if Supstring.begins_with s array_opener
   then extract_initial_array older_extractor s
   else 
   if Supstring.begins_with s record_opener
   then extract_initial_record older_extractor s
   else 
   if ((65<=ic)&&(ic<=90)) (* char between A and Z *)
   then extract_initial_variant older_extractor s 
   else raise(Unknown_beginning(s));;

let rec extract_initial s=
  fixer_in_initial_object_extraction extract_initial s;;

let of_string s=
  force_extraction_to_be_full s (extract_initial s);;  

let rec to_string = function 
   Concrete_object_t.Int(i)->string_of_int i 
   |String(t)->string_opener^t^string_closer
   |Uple(l)->let temp1=Image.image to_string l in 
             uple_opener^(String.concat uple_separator temp1)^uple_closer
   |List(l)->let temp1=Image.image to_string l in 
             list_opener^(String.concat list_separator temp1)^list_closer 
   |Array(l)->let temp1=Image.image to_string l in 
             array_opener^(String.concat array_separator temp1)^array_closer
   |Record(l)->let temp1=Image.image (fun (key,vaal)->key ^ record_arrow ^ (to_string vaal))  l in 
             record_opener^(String.concat record_separator temp1)^record_closer          
   |Variant(constructor,l)->let temp1=Image.image to_string l in 
             constructor^variant_opener^(String.concat variant_separator temp1)^variant_closer ;; 

end;;

let of_string = Old.of_string;;
let to_string = Old.to_string;;


module New = struct 

exception Missing_string_opener of string;;
exception Missing_string_closer of string;;
open Private;;


let extract_int_at_index s idx=
   (* we assume that the first character is ok *)
   let n=String.length s in 
   let next_idx=(
   match Option.seek(
    fun k->let c=Strung.get s k in 
    not(List.mem c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'])
   ) (Ennig.ennig (idx+1) n) with 
   None->n+1
   |Some(idx2)->idx2) in 
   (Concrete_object_t.Int(int_of_string(Cull_string.beginning (next_idx-1) s)),next_idx);;


let extract_string_at_index s idx=
   if not(Substring.is_a_substring_located_at string_opener s idx) 
   then raise(Missing_string_opener(s))
   else 
   let idx1=idx+(String.length string_opener) in 
   let idx2=Substring.leftmost_index_of_in_from string_closer s idx1 in 
   if idx2<0
   then raise(Missing_string_closer(s))
   else
   (Concrete_object_t.String(Cull_string.interval s idx1 (idx2-1)),idx2+(String.length string_closer));;




end;;



(*

let g1=Concrete_object_t.Variant ("None",[]);;
let sg1=to_string g1;;
let g2=of_string sg1;;


*)