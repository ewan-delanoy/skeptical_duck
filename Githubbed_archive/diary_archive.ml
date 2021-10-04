(************************************************************************************************************************
Snippet 81 : 
************************************************************************************************************************)
open Needed_values ;;

(************************************************************************************************************************
Snippet 80 : Musings on the Vand der Waerden problem, version 20 : add
Constraint.analyze_shadow, and start using it
************************************************************************************************************************)
open Needed_values ;;
type expandable = E of string ;;
type vdw_constraint = C of (int list) list ;; (* The constraint is that 
none of the int lists in the list are contained in the set *)

let mea = Vdw_chosen.measure ;;

let mi ll =
   let temp1 = Image.image Set_of_integers.safe_set ll in 
   let temp2 = Ordered_misc.minimal_elts_wrt_inclusion temp1 in 
   Image.image Set_of_integers.forget_order temp2 ;;

let mt ll =
   let temp1 = Image.image Set_of_integers.safe_set ll in 
   let temp2 = Ordered_misc.minimal_transversals temp1 in 
   Image.image Set_of_integers.forget_order temp2 ;;

let bitmap_to_intlist bm=
  let temp1 = Ennig.index_everything (Strung.explode bm) in 
  Option.filter_and_unpack (
     fun (j,c)->if c='1' then Some j else None
  ) temp1 ;;

let intlist_to_bitmap (n,il) =
   String.concat "" 
    (Ennig.doyle (fun j->if List.mem j il then "1" else "0") 1 n);;  

let rap x y = x ^ (Cull_string.cobeginning (String.length x) y);;

let erap prefix pattern n = rap prefix 
  (intlist_to_bitmap (n,List.filter (fun x->
   List.mem (x mod 9) pattern ) (Ennig.ennig 1 n))) ;;

let brap assocl_for_n assocl_for_r patt  n =
   match List.assoc_opt n assocl_for_n with 
    Some(ready_answer) -> ready_answer 
   |None ->
    let r = n mod 9 in 
    let pattern = Image.image (fun x->(x+r) mod 9) patt in 
    let prefix = List.assoc r assocl_for_r in 
    erap prefix pattern n ;;  

let srap assocl_for_n assocl_for_r n =
    match List.assoc_opt n assocl_for_n with 
     Some(ready_answer) -> ready_answer 
    |None ->
     let r = n mod 9 in 
     List.assoc r assocl_for_r ;; 

let extend_by_one_char c s =
  let possibilities = 
  (if c='F' then [(s,"0");(s,"1")] else [(s,String.make 1 c)]) in 
  Option.filter_and_unpack (fun (s,t)->
    if Vdw_string.admissible_parts_are_joinable s t 
    then Some(s^t) 
    else None 
  ) possibilities ;;  

let extend_several_by_one_char l c=
   List.flatten (Image.image (extend_by_one_char c) l) ;;

(*   
let extend_several_by_several_chars l s=
   let walker = ref l in 
   for k = 0 to (String.length s)-1 do
      walker := extend_several_by_one_char (!walker) (String.get s k)
   done;
   (!walker);;  
*)   

let expand = Memoized.recursive (fun old_f (E s)->
     let n = String.length s in 
     if n<1 then [""] else 
     let t = Cull_string.coending 1 s and c= Strung.get s n in 
     extend_several_by_one_char (old_f (E t)) c) ;;

let expand_several l = 
   let temp1 = Image.image expand l in      
   Ordered.fold_merge Total_ordering.lex_for_strings temp1 ;;

let base = expand (E(String.make 8 'F'));;

let main_hashtbl = Hashtbl.create 2000;; 

let main_algorithm old_f (vdw_s,k) =
   let n = Vdw_string.cardinality vdw_s in 
   if k<1 then (n,"") else 
   let temp1 = [0,"0";1,"1"] in 
   let temp2 = List.filter (fun (j,extension)->
        Vdw_string.admissible_parts_are_joinable vdw_s extension
   ) temp1  in
   let temp3 = Image.image (
     fun (j,extension) -> 
       let new_s = (Vdw_string.tail vdw_s)^extension in 
       let (m,old_sol) = old_f (new_s,k-1) in
       let contribution_of_first_char =(
          if (Strung.get vdw_s 1) = '1' then 1 else 0
       )  in
       (m+contribution_of_first_char,extension^old_sol)
   ) temp2 in 
   let (m,temp4) = Max.maximize_it_with_care fst temp3 in 
   (m,snd(List.hd(temp4)));; 

let rec pre_main (vdw_s,k) =
   (* we assume that vdw_s has length 8 *)
   match Hashtbl.find_opt main_hashtbl (vdw_s,k) with 
   Some(old_answer) -> old_answer 
   | None ->
      let answer=(main_algorithm pre_main (vdw_s,k) ) in 
      let _= Hashtbl.add main_hashtbl (vdw_s,k) answer in 
      answer ;;

let almost_main (s,k) =
    (* we remove any assumption on the length of s *)
    let d = 8 - (String.length s) in 
    if d >0 
    then pre_main((String.make d '0')^s,k) 
    else 
      let before = Cull_string.coending 8 s 
      and after = Cull_string.ending 8 s in 
      let (old_card,old_sol) =pre_main(after,k) in 
      ((Vdw_string.cardinality before)+old_card,old_sol) ;; 

exception Length_mismatch of string * int ;;

let main (s,imposed_prefix,k) =
   let d = k - (String.length imposed_prefix) in 
   if d < 0
   then  raise(Length_mismatch(imposed_prefix,k))      
   else  let (old_card,old_sol) =almost_main(s^imposed_prefix,d) in 
         (old_card,imposed_prefix^old_sol) ;;

let shadow n =
   if n<=8
   then let zeroes = String.make (8-n) '0' in 
        let temp1 = List.filter (Substring.is_the_ending_of zeroes) base in  
        snd (Max.maximize_it_with_care Vdw_string.cardinality temp1)  
   else let m = Vdw_chosen.measure n in 
        List.filter (
          fun prefix -> fst(main("",prefix,n)) = m
        )  base ;;   
        
let analyze_shadow ll=
   let tempf = (fun s->
     List.filter (fun j->
        (Strung.get s j,Strung.get s (2*j))=('1','1')
      ) (Ennig.ennig 1 4)
   ) in 
   mt(Image.image tempf ll);;
    
let gmain ll n =
     let temp1 = Image.image (fun prefix->main("",prefix,n)) ll in 
     let (m,temp2) = Max.maximize_it_with_care fst temp1 in 
     (m,Image.image snd temp2) ;;   


module Constraint = struct 

let order = Total_ordering.silex_compare Total_ordering.for_integers ;;

let make ll = C(Ordered.sort order (mi ll));; 
   
let basic_test patt elt =
   List.exists (fun k->(Strung.get patt k)<>'1') elt ;;

let test (C ll) patt = List.for_all (basic_test patt) ll;;

let representatives cstr= List.filter (test cstr) base;;

let shift (C ll) =(C (Image.image (Image.image (fun x->x-1)) ll));;

let impose_0 (C ll)=
   C([1]::(List.filter (fun x->not(List.mem 1 x)) ll)) ;;

let impose_1 (C ll)=
   if List.mem [1] ll 
   then None
   else
      let (temp1,temp2) = List.partition (List.mem 1) ll in
      let temp3 = Image.image List.tl temp1 in 
      Some(make (temp2@temp3));; 

let corrector_for_extension_by_1 (C ll) =
     make ([[1;2];[2;4];[3;6];[4;8]]@ll);;

let corrector_for_extension_by_0 (C ll) =
      C(List.filter (fun l->l<>[0]) ll);;
 
let corrector_for_extension e cstr=
  if e=1 
  then corrector_for_extension_by_1 cstr 
  else corrector_for_extension_by_0 cstr ;;      

let bare_possible_expansions cstr =
    let case0 = (0,impose_0 cstr) in
    match impose_1 cstr with    
    None -> [case0]
    |Some(cstr1) -> [case0;(1,cstr1)] ;;

let possible_expansions cstr =
   Image.image (fun (e,cstr2)->
      let cstr3 = shift cstr2 in 
     (e,corrector_for_extension e cstr3)   
   ) (bare_possible_expansions cstr)

let doubled l= C(Image.image (fun j->[j;2*j]) l);;    

let analyze_shadow (C ll) sha=
   let temp1 = Ennig.index_everything ll in 
   let tempf = (fun s->
     Option.filter_and_unpack (fun (idx,cstr_elt)->
        if List.for_all (fun j->(Strung.get s j)='1') cstr_elt 
        then Some idx
        else None
      ) temp1
   ) in 
   let temp2 = mt(Image.image tempf sha) in 
   Image.image (Image.image (fun k->List.nth ll (k-1))) temp2 ;;

end ;;   


let reconstructed_main0 =
   brap 
     [1,"1";2,"11";3,"011";9,"101100011";10,"0101100011"]
     [0,"10110001011000";1,"010110001011000";2,"1011000";3,"01011000";
      4,"";5,"";6,"";7,"";8,""]   
      [0;5;6;8]  ;;  

let check_reconstructed_main0 = 
   let temp1 = Image.image (fun j->
       (j,snd(main ("00000000","",j)),reconstructed_main0 j)) (Ennig.ennig 1 100) in 
   List.filter (fun (j,a,b)->a<>b) temp1;;   

let reconstructed_shadow =
   srap
      [
         (1, ["10000000"]); 
         (2, ["11000000"]);
         (3, ["01100000"; "10100000"; "11000000"]); 
         (4, ["10110000"; "11010000"]);
         (7, ["00110110"; "01011010"; "01100110"; "01101100"; "10100110"; 
         "10110100";"11000110"; "11001010"; "11001100"; "11011000"]);
         (8,["00011011"; "00101101"; "00110011"; "00110110"; "01010011"; 
         "01011010";"01100011"; "01100101"; "01100110"; "01101100"; 
         "10001011"; "10001101"; "10011001"; "10100011"; "10100101"; 
         "10100110"; "10110001"; "10110100"; "11000011"; "11000101"; 
         "11000110"; "11001010"; "11001100"; "11010001"; "11011000"]);
         (9,  ["10110001"; "11000101"; "11000110"; "11010001"]);
         (10, ["01011000"; "01100010"; "01100011"; "01101000"; "10000110"; 
         "10001011";"10010100"; "10011001"; "10100010"; "10100011"; 
         "10100110"; "10110000"; "10110001"; "10110100"; "11000010"; 
         "11000011"; "11000101"; "11000110"; "11001010"; "11010000"; 
         "11010001"; "11011000"])
      ]
      [
      0,["10110001"; "10110100"; "11000101"; "11000110"; "11010001"; "11011000"];
      1, ["01011000"; "01011010"; "01100010"; "01100011"; "01101000"; "01101100";
      "10000110"; "10001011"; "10010100"; "10011001"; "10100010"; "10100011";
      "10100110"; "10110000"; "10110001"; "10110100"; "11000010"; "11000011";
      "11000101"; "11000110"; "11001010"; "11001100"; "11010000"; "11010001";
      "11011000"];   
      2,["10110001"; "10110100"; "11000011"; "11000101"; "11001010"; "11010001";
      "11011000"];
      3,["01011000"; "01011010"; "01100001"; "01100010"; "01100101"; "01101000";
      "01101100"; "10100001"; "10100010"; "10100101"; "10100110"; "10110000";
      "10110001"; "10110100"; "11000001"; "11000010"; "11000011"; "11000101";
      "11000110"; "11001010"; "11001100"; "11010000"; "11010001"; "11011000"];
      4,["10110000"; "10110001"; "10110100"; "11010000"; "11010001"; "11011000"];
      5,["11011000"];
      6,["01101100"; "10110100"; "11001100"; "11011000"];
      7,["00110110"; "01011010"; "01100110"; "01101100"; "10100110"; "10110001";
      "10110100"; "11000101"; "11000110"; "11001010"; "11001100"; "11010001";
      "11011000"];
      8,["00011011"; "00101101"; "00110011"; "00110110"; "01010011"; "01011000";
      "01011010"; "01100010"; "01100011"; "01100101"; "01100110"; "01101000";
      "01101100"; "10001011"; "10001101"; "10011001"; "10100010"; "10100011";
      "10100101"; "10100110"; "10110000"; "10110001"; "10110100"; "11000010";
      "11000011"; "11000101"; "11000110"; "11001010"; "11001100"; "11010000";
      "11010001"; "11011000"];

      ] ;;


let check_reconstructed_shadow = 
   let temp1 = Image.image (fun j->
             (j,shadow j,reconstructed_shadow j)) (Ennig.ennig 1 100) in 
   List.filter (fun (j,a,b)->a<>b) temp1;;        


let first_addendum n=
  match (n mod 9) with 
  0 -> if n = 9 then [1;4] else [1;3;4]
 |2 -> if n = 2 then [1] else [1;3;4]
 |5 -> [1]
 |6 -> [1;3]
 |7 -> if n = 7 then [1;2;3] else [1;2;3;4]
 |_ -> [] ;;
 
let expand_first_addendum m=
   let l = first_addendum m in
   if l = [] then [] else 
   let cstr = Constraint.doubled l in 
   Image.image (fun (e,cstr2)->(m-1,mea(m)-1-e,cstr2)) 
   (Constraint.possible_expansions cstr) ;;   
 
let g1 = List.flatten(Ennig.doyle expand_first_addendum 8 100);;
let (g2,g3) = List.partition (fun (n,goal,cstr)->
    goal>=(mea(n))
   ) g1 ;;

let (g4,g5) = List.partition (fun (n,goal,cstr)->goal=(mea n)-1) g3 ;;   
let g5 = Image.image (fun (n,goal,cstr)-> (n,goal,cstr,
 Constraint.analyze_shadow cstr (shadow n)) ) g4;;   
let (g6,empty) = List.partition 
  (fun (n,goal,cstr,res)->List.length res=1) g5 ;; 
let g7 = Image.image (fun (n,goal,cstr,res)->(n,goal,cstr,List.hd res)) g6;;

let g8 = Image.image (fun (n,goal,cstr,res)->n mod 9) g7 ;;   
let g9 = List.filter (fun (n,goal,cstr,res)->(n mod 9)=8) g7 ;;
   


let second_addendum n=
  match (n mod 9) with 
  1 -> if n = 1 then [] else [[1];[2;5];[3;7]]
 |4 -> if n = 4 then [] else [[1]]
 |5 -> if n = 5 then [] else [[2;5]]
 |6 -> if n = 6 then [] else [[1;3];[2;5]]
 |8 -> if n = 8 then [[1];[2;4];[3;6];[3;7];[4;8]] else [[1];[2;5];[3;6];[3;7];[4;8]]
 |_ -> [] ;;
 
let expand_second_addendum m=
   let l = second_addendum m in
   if l = [] then [] else 
   let cstr = Constraint.make l in 
   Image.image (fun (e,cstr2)->(m-1,mea(m)-1-e,cstr2)) 
   (Constraint.possible_expansions cstr) ;;   

let h1 = List.flatten(Ennig.doyle expand_second_addendum 8 100);;
let (h2,h3) = List.partition (fun (n,goal,cstr)->
       goal>=(mea(n))
      ) h1 ;;
let (h4,h5) = List.partition (fun (n,goal,cstr)->goal=(mea n)-1) h3 ;;   
let h5 = Image.image (fun (n,goal,cstr)-> (n,goal,cstr,
    Constraint.analyze_shadow cstr (shadow n)) ) h4;;   
let (h6,empty) = List.partition 
     (fun (n,goal,cstr,res)->List.length res=1) h5 ;; 
let h7 = Image.image (fun (n,goal,cstr,res)->(n,goal,cstr,List.hd res)) h6;;
let h8 = Image.image (fun (n,goal,cstr,res)->n mod 9) h7 ;;   
let h9 = List.filter (fun (n,goal,cstr,res)->(n mod 9)=8) h7 ;;

Ordered_misc.reorder_list_of_pairs_using_list_of_singles ;;


(************************************************************************************************************************
Snippet 79 : Musings on the Vand der Waerden problem, version 19 : add 
Constraint submodule, and start testing it
************************************************************************************************************************)
open Needed_values ;;

type expandable = E of string ;;
type vdw_constraint = C of (int list) list ;; (* The constraint is that 
none of the int lists in the list are contained in the set *)

let mea = Vdw_chosen.measure ;;

let mi ll =
   let temp1 = Image.image Set_of_integers.safe_set ll in 
   let temp2 = Ordered_misc.minimal_elts_wrt_inclusion temp1 in 
   Image.image Set_of_integers.forget_order temp2 ;;

let mt ll =
   let temp1 = Image.image Set_of_integers.safe_set ll in 
   let temp2 = Ordered_misc.minimal_transversals temp1 in 
   Image.image Set_of_integers.forget_order temp2 ;;

let bitmap_to_intlist bm=
  let temp1 = Ennig.index_everything (Strung.explode bm) in 
  Option.filter_and_unpack (
     fun (j,c)->if c='1' then Some j else None
  ) temp1 ;;

let intlist_to_bitmap (n,il) =
   String.concat "" 
    (Ennig.doyle (fun j->if List.mem j il then "1" else "0") 1 n);;  

let rap x y = x ^ (Cull_string.cobeginning (String.length x) y);;

let erap prefix pattern n = rap prefix 
  (intlist_to_bitmap (n,List.filter (fun x->
   List.mem (x mod 9) pattern ) (Ennig.ennig 1 n))) ;;

let brap assocl_for_n assocl_for_r patt  n =
   match List.assoc_opt n assocl_for_n with 
    Some(ready_answer) -> ready_answer 
   |None ->
    let r = n mod 9 in 
    let pattern = Image.image (fun x->(x+r) mod 9) patt in 
    let prefix = List.assoc r assocl_for_r in 
    erap prefix pattern n ;;  

let srap assocl_for_n assocl_for_r n =
    match List.assoc_opt n assocl_for_n with 
     Some(ready_answer) -> ready_answer 
    |None ->
     let r = n mod 9 in 
     List.assoc r assocl_for_r ;; 

let extend_by_one_char c s =
  let possibilities = 
  (if c='F' then [(s,"0");(s,"1")] else [(s,String.make 1 c)]) in 
  Option.filter_and_unpack (fun (s,t)->
    if Vdw_string.admissible_parts_are_joinable s t 
    then Some(s^t) 
    else None 
  ) possibilities ;;  

let extend_several_by_one_char l c=
   List.flatten (Image.image (extend_by_one_char c) l) ;;

(*   
let extend_several_by_several_chars l s=
   let walker = ref l in 
   for k = 0 to (String.length s)-1 do
      walker := extend_several_by_one_char (!walker) (String.get s k)
   done;
   (!walker);;  
*)   

let expand = Memoized.recursive (fun old_f (E s)->
     let n = String.length s in 
     if n<1 then [""] else 
     let t = Cull_string.coending 1 s and c= Strung.get s n in 
     extend_several_by_one_char (old_f (E t)) c) ;;

let expand_several l = 
   let temp1 = Image.image expand l in      
   Ordered.fold_merge Total_ordering.lex_for_strings temp1 ;;

let base = expand (E(String.make 8 'F'));;

let main_hashtbl = Hashtbl.create 2000;; 

let main_algorithm old_f (vdw_s,k) =
   let n = Vdw_string.cardinality vdw_s in 
   if k<1 then (n,"") else 
   let temp1 = [0,"0";1,"1"] in 
   let temp2 = List.filter (fun (j,extension)->
        Vdw_string.admissible_parts_are_joinable vdw_s extension
   ) temp1  in
   let temp3 = Image.image (
     fun (j,extension) -> 
       let new_s = (Vdw_string.tail vdw_s)^extension in 
       let (m,old_sol) = old_f (new_s,k-1) in
       let contribution_of_first_char =(
          if (Strung.get vdw_s 1) = '1' then 1 else 0
       )  in
       (m+contribution_of_first_char,extension^old_sol)
   ) temp2 in 
   let (m,temp4) = Max.maximize_it_with_care fst temp3 in 
   (m,snd(List.hd(temp4)));; 

let rec pre_main (vdw_s,k) =
   (* we assume that vdw_s has length 8 *)
   match Hashtbl.find_opt main_hashtbl (vdw_s,k) with 
   Some(old_answer) -> old_answer 
   | None ->
      let answer=(main_algorithm pre_main (vdw_s,k) ) in 
      let _= Hashtbl.add main_hashtbl (vdw_s,k) answer in 
      answer ;;

let almost_main (s,k) =
    (* we remove any assumption on the length of s *)
    let d = 8 - (String.length s) in 
    if d >0 
    then pre_main((String.make d '0')^s,k) 
    else 
      let before = Cull_string.coending 8 s 
      and after = Cull_string.ending 8 s in 
      let (old_card,old_sol) =pre_main(after,k) in 
      ((Vdw_string.cardinality before)+old_card,old_sol) ;; 

exception Length_mismatch of string * int ;;

let main (s,imposed_prefix,k) =
   let d = k - (String.length imposed_prefix) in 
   if d < 0
   then  raise(Length_mismatch(imposed_prefix,k))      
   else  let (old_card,old_sol) =almost_main(s^imposed_prefix,d) in 
         (old_card,imposed_prefix^old_sol) ;;

let shadow n =
   if n<=8
   then let zeroes = String.make (8-n) '0' in 
        let temp1 = List.filter (Substring.is_the_ending_of zeroes) base in  
        snd (Max.maximize_it_with_care Vdw_string.cardinality temp1)  
   else let m = Vdw_chosen.measure n in 
        List.filter (
          fun prefix -> fst(main("",prefix,n)) = m
        )  base ;;   
        
let analyze_shadow ll=
   let tempf = (fun s->
     List.filter (fun j->
        (Strung.get s j,Strung.get s (2*j))=('1','1')
      ) (Ennig.ennig 1 4)
   ) in 
   mt(Image.image tempf ll);;
    
let gmain ll n =
     let temp1 = Image.image (fun prefix->main("",prefix,n)) ll in 
     let (m,temp2) = Max.maximize_it_with_care fst temp1 in 
     (m,Image.image snd temp2) ;;   


module Constraint = struct 

let order = Total_ordering.silex_compare Total_ordering.for_integers ;;

let make ll = C(Ordered.sort order (mi ll));; 
   
let basic_test patt elt =
   List.exists (fun k->(Strung.get patt k)<>'1') elt ;;

let test (C ll) patt = List.for_all (basic_test patt) ll;;

let representatives cstr= List.filter (test cstr) base;;

let shift (C ll) =(C (Image.image (Image.image (fun x->x-1)) ll));;

let impose_0 (C ll)=
   C([1]::(List.filter (fun x->not(List.mem 1 x)) ll)) ;;

let impose_1 (C ll)=
   if List.mem [1] ll 
   then None
   else
      let (temp1,temp2) = List.partition (List.mem 1) ll in
      let temp3 = Image.image List.tl temp1 in 
      Some(make (temp2@temp3));; 

let possible_expansions cstr =
    let case0 = (0,impose_0 cstr) in
    match impose_1 cstr with    
    None -> [case0]
    |Some(cstr1) -> [case0;(1,cstr1)] ;;

let doubled l= C(Image.image (fun j->[j;2*j]) l);;    

end ;;   


let reconstructed_main0 =
   brap 
     [1,"1";2,"11";3,"011";9,"101100011";10,"0101100011"]
     [0,"10110001011000";1,"010110001011000";2,"1011000";3,"01011000";
      4,"";5,"";6,"";7,"";8,""]   
      [0;5;6;8]  ;;  

let check_reconstructed_main0 = 
   let temp1 = Image.image (fun j->
       (j,snd(main ("00000000","",j)),reconstructed_main0 j)) (Ennig.ennig 1 100) in 
   List.filter (fun (j,a,b)->a<>b) temp1;;   

let reconstructed_shadow =
   srap
      [
         (1, ["10000000"]); 
         (2, ["11000000"]);
         (3, ["01100000"; "10100000"; "11000000"]); 
         (4, ["10110000"; "11010000"]);
         (7, ["00110110"; "01011010"; "01100110"; "01101100"; "10100110"; 
         "10110100";"11000110"; "11001010"; "11001100"; "11011000"]);
         (8,["00011011"; "00101101"; "00110011"; "00110110"; "01010011"; 
         "01011010";"01100011"; "01100101"; "01100110"; "01101100"; 
         "10001011"; "10001101"; "10011001"; "10100011"; "10100101"; 
         "10100110"; "10110001"; "10110100"; "11000011"; "11000101"; 
         "11000110"; "11001010"; "11001100"; "11010001"; "11011000"]);
         (9,  ["10110001"; "11000101"; "11000110"; "11010001"]);
         (10, ["01011000"; "01100010"; "01100011"; "01101000"; "10000110"; 
         "10001011";"10010100"; "10011001"; "10100010"; "10100011"; 
         "10100110"; "10110000"; "10110001"; "10110100"; "11000010"; 
         "11000011"; "11000101"; "11000110"; "11001010"; "11010000"; 
         "11010001"; "11011000"])
      ]
      [
      0,["10110001"; "10110100"; "11000101"; "11000110"; "11010001"; "11011000"];
      1, ["01011000"; "01011010"; "01100010"; "01100011"; "01101000"; "01101100";
      "10000110"; "10001011"; "10010100"; "10011001"; "10100010"; "10100011";
      "10100110"; "10110000"; "10110001"; "10110100"; "11000010"; "11000011";
      "11000101"; "11000110"; "11001010"; "11001100"; "11010000"; "11010001";
      "11011000"];   
      2,["10110001"; "10110100"; "11000011"; "11000101"; "11001010"; "11010001";
      "11011000"];
      3,["01011000"; "01011010"; "01100001"; "01100010"; "01100101"; "01101000";
      "01101100"; "10100001"; "10100010"; "10100101"; "10100110"; "10110000";
      "10110001"; "10110100"; "11000001"; "11000010"; "11000011"; "11000101";
      "11000110"; "11001010"; "11001100"; "11010000"; "11010001"; "11011000"];
      4,["10110000"; "10110001"; "10110100"; "11010000"; "11010001"; "11011000"];
      5,["11011000"];
      6,["01101100"; "10110100"; "11001100"; "11011000"];
      7,["00110110"; "01011010"; "01100110"; "01101100"; "10100110"; "10110001";
      "10110100"; "11000101"; "11000110"; "11001010"; "11001100"; "11010001";
      "11011000"];
      8,["00011011"; "00101101"; "00110011"; "00110110"; "01010011"; "01011000";
      "01011010"; "01100010"; "01100011"; "01100101"; "01100110"; "01101000";
      "01101100"; "10001011"; "10001101"; "10011001"; "10100010"; "10100011";
      "10100101"; "10100110"; "10110000"; "10110001"; "10110100"; "11000010";
      "11000011"; "11000101"; "11000110"; "11001010"; "11001100"; "11010000";
      "11010001"; "11011000"];

      ] ;;


let check_reconstructed_shadow = 
   let temp1 = Image.image (fun j->
             (j,shadow j,reconstructed_shadow j)) (Ennig.ennig 1 100) in 
   List.filter (fun (j,a,b)->a<>b) temp1;;        


let first_addendum n=
  match (n mod 9) with 
  0 -> if n = 9 then [[1;4]] else [[1;3;4]]
 |2 -> if n = 2 then [[1]] else [[1;3;4]]
 |5 -> [[1]]
 |6 -> [[1;3]]
 |7 -> if n = 7 then [[1;2;3]] else [[1;2;3;4]]
 |_ -> [] ;;
 
let expand_first_addendum n=
   let l = first_addendum n in
   if l = [] then (n,0,0) else 
   let cstr = Constraint.make l in 
   let reps = Constraint.representatives cstr in 
   (n,fst(gmain reps n),(mea n)-1) ;;   
 
let g1 = Ennig.doyle expand_first_addendum 8 100;;

(*

let ff n = analyze_shadow (shadow n) ;; 

let mz l= 
 Ordered.sort (Total_ordering.lex_compare Total_ordering.for_integers)
 (mt (Image.image (fun j->[j;2*j]) l)) ;;

let gg r=
  let temp1 = List.filter
  (fun t->shadow(r+9*t)=shadow(r+9*(t+1))) [0;1;2;3;4;5] in 
  let q=List.hd temp1 in 
  (temp1,shadow(r+9*q));;
*)




(************************************************************************************************************************
Snippet 78 : Musings on the Vand der Waerden problem, version 18 : starting 
a different approach
************************************************************************************************************************)
open Needed_values ;;
type expandable = E of string ;;

let bitmap_to_intlist bm=
  let temp1 = Ennig.index_everything (Strung.explode bm) in 
  Option.filter_and_unpack (
     fun (j,c)->if c='1' then Some j else None
  ) temp1 ;;

let intlist_to_bitmap (n,il) =
   String.concat "" 
    (Ennig.doyle (fun j->if List.mem j il then "1" else "0") 1 n);;  

let rap x y = x ^ (Cull_string.cobeginning (String.length x) y);;

let erap prefix pattern n = rap prefix 
  (intlist_to_bitmap (n,List.filter (fun x->
   List.mem (x mod 9) pattern ) (Ennig.ennig 1 n))) ;;

let brap assocl_for_n assocl_for_r patt  n =
   match List.assoc_opt n assocl_for_n with 
    Some(ready_answer) -> ready_answer 
   |None ->
    let r = n mod 9 in 
    let pattern = Image.image (fun x->(x+r) mod 9) patt in 
    let prefix = List.assoc r assocl_for_r in 
    erap prefix pattern n ;;  

let extend_by_one_char c s =
  let possibilities = 
  (if c='F' then [(s,"0");(s,"1")] else [(s,String.make 1 c)]) in 
  Option.filter_and_unpack (fun (s,t)->
    if Vdw_string.admissible_parts_are_joinable s t 
    then Some(s^t) 
    else None 
  ) possibilities ;;  

let extend_several_by_one_char l c=
   List.flatten (Image.image (extend_by_one_char c) l) ;;

(*   
let extend_several_by_several_chars l s=
   let walker = ref l in 
   for k = 0 to (String.length s)-1 do
      walker := extend_several_by_one_char (!walker) (String.get s k)
   done;
   (!walker);;  
*)   

let expand = Memoized.recursive (fun old_f (E s)->
     let n = String.length s in 
     if n<1 then [""] else 
     let t = Cull_string.coending 1 s and c= Strung.get s n in 
     extend_several_by_one_char (old_f (E t)) c) ;;

let expand_several l = 
   let temp1 = Image.image expand l in      
   Ordered.fold_merge Total_ordering.lex_for_strings temp1 ;;

let base = expand (E(String.make 8 'F'));;

let main_hashtbl = Hashtbl.create 2000;; 

let main_algorithm old_f (vdw_s,k) =
   let n = Vdw_string.cardinality vdw_s in 
   if k<1 then (n,"") else 
   let temp1 = [0,"0";1,"1"] in 
   let temp2 = List.filter (fun (j,extension)->
        Vdw_string.admissible_parts_are_joinable vdw_s extension
   ) temp1  in
   let temp3 = Image.image (
     fun (j,extension) -> 
       let new_s = (Vdw_string.tail vdw_s)^extension in 
       let (m,old_sol) = old_f (new_s,k-1) in
       let contribution_of_first_char =(
          if (Strung.get vdw_s 1) = '1' then 1 else 0
       )  in
       (m+contribution_of_first_char,extension^old_sol)
   ) temp2 in 
   let (m,temp4) = Max.maximize_it_with_care fst temp3 in 
   (m,snd(List.hd(temp4)));; 

let rec pre_main (vdw_s,k) =
   (* we assume that vdw_s has length 8 *)
   match Hashtbl.find_opt main_hashtbl (vdw_s,k) with 
   Some(old_answer) -> old_answer 
   | None ->
      let answer=(main_algorithm pre_main (vdw_s,k) ) in 
      let _= Hashtbl.add main_hashtbl (vdw_s,k) answer in 
      answer ;;

let almost_main (s,k) =
    (* we remove any assumption on the length of s *)
    let d = 8 - (String.length s) in 
    if d >0 
    then pre_main((String.make d '0')^s,k) 
    else 
      let before = Cull_string.coending 8 s 
      and after = Cull_string.ending 8 s in 
      let (old_card,old_sol) =pre_main(after,k) in 
      ((Vdw_string.cardinality before)+old_card,old_sol) ;; 

exception Length_mismatch of string * int ;;

let main (s,imposed_prefix,k) =
   let d = k - (String.length imposed_prefix) in 
   if d < 0
   then  raise(Length_mismatch(imposed_prefix,k))      
   else  let (old_card,old_sol) =almost_main(s^imposed_prefix,d) in 
         (old_card,imposed_prefix^old_sol) ;;

let shadow n =
   if n<=8
   then let zeroes = String.make (8-n) '0' in 
        let temp1 = List.filter (Substring.is_the_ending_of zeroes) base in  
        snd (Max.maximize_it_with_care Vdw_string.cardinality temp1)  
   else let m = Vdw_chosen.measure n in 
        List.filter (
          fun prefix -> fst(main("",prefix,n)) = m
        )  base ;;   
        
let reconstructed_main0 =
   brap 
     [1,"1";2,"11";3,"011";9,"101100011";10,"0101100011"]
     [0,"10110001011000";1,"010110001011000";2,"1011000";3,"01011000";
      4,"";5,"";6,"";7,"";8,""]   
      [0;5;6;8]  ;;  


let check_reconstructed_main0 = 
   let temp1 = Image.image (fun j->
       (j,snd(main ("00000000","",j)),reconstructed_main0 j)) (Ennig.ennig 1 100) in 
   List.filter (fun (j,a,b)->a<>b) temp1;;   


(*

Ordered.sort Total_ordering.lex_for_strings base = base ;;

let ff k=
   let _  =Image.image (fun s->main (s,"",k)) base  in 
   main ("00000000","",k) ;;      

let gg k = bitmap_to_intlist(snd(ff k));;   


let z1 = Image.image (fun x->(x mod 9)) (gg(9+9*5));;
let z2 = snd(ff(9+9*5));;
let z3 = Cull_string.beginning 14 z2;;


let uf1 n = erap "010110001011000" [0;1;6;7]  n ;;
let tf1 n = if n=1 then "1" else 
            if n=10 then "0101100011" else uf1 n ;; 
let uf2 n = erap "1011000" [1;2;7;8]  n ;;
let tf2 n = if n=2 then "11" else  uf2 n ;; 
let uf3 n = erap "01011000" [0;2;3;8]  n ;;
let tf3 n = if n=3 then "011" else  uf3 n ;; 
let uf4 n = erap "" [0;1;3;4]  n ;;
let tf4 n = uf4 n ;; 
let uf5 n = erap "" [1;2;4;5]  n ;;
let tf5 n = uf5 n ;; 
let uf6 n = erap "" [2;3;5;6]  n ;;
let tf6 n = uf6 n ;; 
let uf7 n = erap "" [3;4;6;7]  n ;;
let tf7 n = uf7 n ;; 
let uf8 n = erap "" [4;5;7;8]  n ;;
let tf8 n = uf8 n ;; 
let uf9 n = erap "10110001011000" [0;5;6;8]  n ;;
let tf9 n = if n=9 then "101100011" else uf9 n ;; 



let r0 = 9;;
let z4 = Ennig.doyle (fun k->(r0+9*k,snd(ff(r0+9*k)),tf9(r0+9*k))) 0 10;;
let z5 = List.filter (fun (n,a,b)->a<>b) z4 ;;
*)




(************************************************************************************************************************
Snippet 77 : Musings on the Vand der Waerden problem, version 17 : adding the
I variant, and oslo_compute 33 0 
************************************************************************************************************************)
open Needed_values ;;
type colored_variable = 
   A of int |B of int|C of int|D of int|E of int
   |F of int|G of int|H of int|I of int| Og of int * int ;;   
type list_of_affinities = Aff of ((int list * colored_variable) * bool)  list ;;    
type partition_watcher = 
   PW of 
   list_of_affinities *
   (((int list * colored_variable) * (colored_variable * colored_variable)) list) *
   ((colored_variable * (int list * colored_variable) list) list) ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;; 
let mea = Vdw_chosen.measure ;;
let lmea = Vdw_chosen.lower_measure ;;
let is_adm = Vdw_chosen.test_for_admissibility ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

let intlist_to_string lamb =
   "["^(String.concat ";" (Image.image string_of_int lamb))^"]" ;;

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  

module Colored = struct 

exception Data_for_variable_exn of colored_variable;;
let data_for_variable cvar = match cvar with 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) |(D k) ->("D",k) 
    |(E k) ->("E",k) |(F k) ->("F",k) |(G k) ->("G",k) |(H k) ->("H",k)
    |(I k) ->("I",k) 
    |(Og(_,_))->raise(Data_for_variable_exn(cvar)) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     if lbl = "D" then D k else 
     if lbl = "E" then E k else
     if lbl = "F" then F k else  
     if lbl = "G" then G k else   
     if lbl = "H" then H k else  
     if lbl = "I" then I k else       
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let is_ugly cv = match cv with 
 (Og(_,_)) -> true |
  _ -> let (lbl,k) = data_for_variable cv in 
       (int_of_char(String.get lbl 0))>=68 ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d 
       [0,A 0;1, B 0;2, C 0;3, D 0;4, E 0;5, F 0;
        6, G 0;7, H 0;8, I 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

let to_ocaml_readable_string cv= match cv with  
   (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
   | _ -> let (lbl,k) = data_for_variable cv in 
          lbl^" "^(string_of_int k) ;; 

let order = ((fun cv1 cv2->
  let (lbl1,k1) = data_for_variable cv1 
  and (lbl2,k2) = data_for_variable cv2 in  
  let trial1 = Total_ordering.standard lbl1 lbl2 in 
  if trial1 <> Total_ordering_result_t.Equal then trial1 else 
   Total_ordering.standard k1 k2 
) : colored_variable Total_ordering_t.t) ;;

let order_for_pairs = ((fun (lamb1,cv1) (lamb2,cv2)->
   let trial1 = order cv1 cv2 in 
   if trial1 <> Total_ordering_result_t.Equal then trial1 else 
    (Total_ordering.silex_compare Total_ordering.standard ) lamb1 lamb2 
 ) : (int list * colored_variable) Total_ordering_t.t) ;;
 

end ;;  

module Affinity = struct

   let shadow (Aff l) cvar = 
      Option.filter_and_unpack (
         fun ((selector,cvar2),is_compatible) ->
           if cvar2 = cvar 
           then Some(selector,is_compatible)   
           else None
      ) l;;
  
  let expand_using_partition aff parti  =
     let (Aff(old_affinities)) = aff 
     and ((selector,partitioned),(part1,part2)) = parti in
     let temp1 = shadow aff partitioned in 
     let temp2 = List.flatten(Image.image (
        fun (selector2,is_compatible) ->
           [((selector2,part1),is_compatible);
            ((selector2,part2),is_compatible)]     
     ) temp1) in
     let new_affinities = old_affinities @ 
         [((selector,part1),true);
         ((selector,part2),false);]@temp2  in 
     Aff(new_affinities) ;;
  
  let insert affty  (Aff old_affinities)  =
     if List.mem affty old_affinities 
     then Aff old_affinities
     else Aff(affty :: old_affinities) ;;
  
  let upper_bound =
     let (Vdw_max_width_t.MW r) = Vdw_chosen.max_width in 
     wall_position + 2* r;;    

  let compress_pair (lamb,cv) =
     (List.filter (fun t->t<=upper_bound) lamb, cv) ;; 

  let analyse_combination (Aff affinities) combination =
     let temp0 = List.filter (
       fun (lamb,_)->Vdw_chosen.test_for_admissibility lamb
     ) combination in 
     let temp1 = Image.image (fun 
        pair -> 
         let res = (
          if fst pair = []
          then Some true
          else List.assoc_opt (compress_pair pair) affinities 
         ) in
         (pair,res)
     ) temp0 in 
     let filterer = (fun criterion ->
        Option.filter_and_unpack 
       (fun (pair,opt)->
         if opt=criterion then Some pair else None) temp1
     ) in 
     let bad_ones = Option.filter_and_unpack 
     (fun (pair,opt)->
       if opt=None then Some (compress_pair pair) else None) temp1
     in 
     let checked_version = filterer (Some true) in 
     (Ordered.sort Colored.order_for_pairs bad_ones,checked_version);;
   
  
  
end ;;   
  
module Expansion = struct

let subst_by_in cvar replacement_for_cvar l=
  let temp1 = List.flatten(Image.image (
     fun pair ->
      let (lamb2,cvar2) = pair in 
      if cvar2=cvar 
      then Image.image (
          fun (lamb3,cvar3)->
            (Ordered.merge oi lamb2 lamb3,cvar3)) replacement_for_cvar
      else [pair]      
  ) l) in
  List.filter (fun (lamb4,cvar4)->is_adm lamb4) temp1 ;; 

let origin n d =
   let delt = mea(n)-mea(n-1) in   
   List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
   [
     [],(n-1,d-delt);
     [n],(n-1,d+1-delt)
   ];;   

let allowed_substitution_by_in (n,d) expr = 
   subst_by_in (n,d) (origin n d) expr;;

let special1 n d =
   let temp1 = origin n d in 
   let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d+1) temp2 in
   temp3 ;;

let special2 n d =
   let temp1 = origin n d in 
   let temp2 = allowed_substitution_by_in (n-1,d+1) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d+1) temp2 in
   temp3 ;;

let special3 n d =
   let temp1 = origin n d in    
   let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d) temp2 in 
   let temp4 = allowed_substitution_by_in (n-2,d+1) temp3 in 
   let temp5 = allowed_substitution_by_in (n-3,d) temp4 in 
   let temp6 = allowed_substitution_by_in (n-4,d+1) temp5 in 
   let temp7 = allowed_substitution_by_in (n-5,d) temp6 in 
   let temp8 = allowed_substitution_by_in (n-5,d+1) temp7 in 
   let temp9 = allowed_substitution_by_in (n-6,d+1) temp8 in 
   temp9 ;;   
     
let special4 n d = 
      let temp1 = origin n d in 
      let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
      let temp3 = allowed_substitution_by_in (n-2,d-1) temp2 in 
      let temp4 = allowed_substitution_by_in (n-2,d) temp3 in 
      let temp5 = allowed_substitution_by_in (n-3,d-1) temp4 in 
      let temp6 = allowed_substitution_by_in (n-3,d) temp5 in 
      let temp7 = allowed_substitution_by_in (n-4,d-1) temp6 in 
      let temp8 = allowed_substitution_by_in (n-4,d) temp7 in 
      let temp9 = allowed_substitution_by_in (n-5,d) temp8 in 
      let temp10 = allowed_substitution_by_in (n-5,d+1) temp9 in 
      let temp11 = allowed_substitution_by_in (n-6,d-1) temp10 in 
      let temp12 = allowed_substitution_by_in (n-6,d) temp11 in 
      let temp13 = allowed_substitution_by_in (n-7,d) temp12 in 
      temp13 ;;

let special5 n d = 
         let temp1 = origin n d in 
         let temp2 = allowed_substitution_by_in (n-1,d+1) temp1 in 
         let temp3 = allowed_substitution_by_in (n-2,d+2) temp2 in 
         let temp4 = allowed_substitution_by_in (n-3,d+1) temp3 in 
         let temp5 = allowed_substitution_by_in (n-4,d) temp4 in 
         let temp6 = allowed_substitution_by_in (n-4,d+1) temp5 in 
         let temp7 = allowed_substitution_by_in (n-5,d) temp6 in 
         let temp8 = allowed_substitution_by_in (n-5,d+1) temp7 in 
         let temp9 = allowed_substitution_by_in (n-5,d+2) temp8 in 
         let temp10 = allowed_substitution_by_in (n-6,d) temp9 in 
         let temp11 = allowed_substitution_by_in (n-6,d+1) temp10 in 
         let temp12 = allowed_substitution_by_in (n-7,d+1) temp11 in 
         let temp13 = allowed_substitution_by_in (n-8,d) temp12 in 
         let temp14 = allowed_substitution_by_in (n-9,d+1) temp13 in 
         temp14 ;; 

let special6 n d = 
   let temp1 = origin n d in 
   let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d+1) temp2 in 
   let temp4 = allowed_substitution_by_in (n-3,d+1) temp3 in 
   let temp5 = allowed_substitution_by_in (n-4,d+2) temp4 in 
   let temp6 = allowed_substitution_by_in (n-5,d+1) temp5 in 
   let temp7 = allowed_substitution_by_in (n-5,d+2) temp6 in 
   let temp8 = allowed_substitution_by_in (n-6,d) temp7 in 
   let temp9 = allowed_substitution_by_in (n-6,d+1) temp8 in 
   let temp10 = allowed_substitution_by_in (n-7,d) temp9 in 
   let temp11 = allowed_substitution_by_in (n-7,d+1) temp10 in 
   let temp12 = allowed_substitution_by_in (n-8,d) temp11 in 
   let temp13 = allowed_substitution_by_in (n-9,d+1) temp12 in 
   let temp14 = allowed_substitution_by_in (n-10,d) temp13 in 
   let temp15 = allowed_substitution_by_in (n-11,d+1) temp14 in  
   temp15 ;;        

let special7 n d = 
      let temp1 = origin n d in 
      let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
      let temp3 = allowed_substitution_by_in (n-2,d) temp2 in 
      let temp4 = allowed_substitution_by_in (n-2,d+1) temp3 in 
      let temp5 = allowed_substitution_by_in (n-3,d) temp4 in 
      let temp6 = allowed_substitution_by_in (n-4,d+1) temp5 in 
      let temp7 = allowed_substitution_by_in (n-5,d) temp6 in 
      let temp8 = allowed_substitution_by_in (n-5,d+1) temp7 in 
      let temp9 = allowed_substitution_by_in (n-6,d+1) temp8 in 
      let temp10 = allowed_substitution_by_in (n-7,d+1) temp9 in 
      temp10 ;;

let current n d = 
   if (n,d)=(18,1) then special1 n d else 
   if (n,d)=(21,0) then special2 n d else 
   if (n,d)=(22,1) then special3 n d else  
   if (n,d)=(23,2) then special4 n d else  
   if (n,d)=(25,1) then special5 n d else   
   if (n,d)=(27,1) then special6 n d else 
   if (n,d)=(30,0) then special2 n d else 
   if (n,d)=(31,1) then special7 n d else                      
   origin n d ;;

end ;;   

let see = Expansion.current ;;

module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (Expansion.current n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;

let direct_descendants (n,d) =
   let temp1 = Image.image snd (Expansion.current n d) in 
   List.filter (fun (nn,dd)->
         ((Hashtbl.find_opt hashtbl_for_oslo (nn,dd)) = None)
         && (nn >= wall_position)   
   ) temp1 ;;      

let order_for_needed_intermediaries =
   Total_ordering.product 
      (Total_ordering.for_integers)
      (fun x y->Total_ordering.for_integers y x) ;;

exception Pusher_for_needed_intermediaries_exn ;;   

let pusher_for_needed_intermediaries (treated,to_be_treated) =
   let oh = order_for_needed_intermediaries in (* oh is for "order here" *)
   match (List.rev to_be_treated) with 
     [] -> raise (Pusher_for_needed_intermediaries_exn) 
    |pair :: others -> 
      let new_ones = Ordered.sort oh (direct_descendants pair) in 
   (Ordered.insert oh pair treated,
            Ordered.merge oh new_ones others) ;;  

let needed_intermediaries n d = 
   let rec tempf = (fun (treated,to_be_treated)->
      match (List.rev to_be_treated) with 
       [] -> treated 
      |pair :: others -> 
         let new_step = pusher_for_needed_intermediaries (treated,to_be_treated) in 
         tempf(new_step) 
   ) in 
   tempf([],direct_descendants (n,d) ) ;;      

end ;;

let og = Oslo.get ;;
let ni = Oslo.needed_intermediaries ;;


module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  ((selector,partitioned),(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition 
   (PW(old_affinities,old_parties,ll):t) parti  =
   let new_affinities =  Affinity.expand_using_partition old_affinities parti
   and new_parties = old_parties @ [parti]   in 
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_affinities,new_parties,new_ll):>t) ;;

let insert_affinity (PW(old_affinities,old_parties,ll)) affty  =
   PW(Affinity.insert affty old_affinities,old_parties,ll) ;;        
    

exception Missing_affinities of ( (int list) * colored_variable) list;;   

let check_combination_using_affinities combination (PW(affinities,_,_):t)=
  let (bad_ones,checked_version) = Affinity.analyse_combination affinities combination in 
  if bad_ones <> []
  then raise(Missing_affinities bad_ones)
  else checked_version;;

let expand_using_t combination (pw:t) =
   let (PW(_,_,ll)) = pw in 
   let ref_for_unknowns = ref [] in 
   let combination2 = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in
   let new_combination = check_combination_using_affinities combination2  pw in
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(affinities,parties,ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(affinities,parties,ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(affinities,parties,List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   (intlist_to_string lamb)^","^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW (_,_,ll)):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;

let decompose ((PW (_,_,ll)):t) cvar =
     List.assoc cvar ll ;;

let ugly_part ((PW (_,_,ll)):t) cvar =
     let temp1 = List.assoc cvar ll in 
     List.filter (fun (lamb,cv)->Colored.is_ugly cv) temp1 ;;     

let ugly_ones ((PW (_,_,ll)):t) =
   Option.filter_and_unpack(fun (cv2,expansion)->
       let bad_part = List.filter 
       (fun (lamb,cv3)->Colored.is_ugly cv3) expansion in 
       if (bad_part = [])||(Colored.is_ugly cv2)
       then None  
       else Some(cv2,bad_part)
   )  ll ;; 


end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
   "D",[List.filter (fun x->(List.length x)=5) wall];
   "E",[List.filter (fun x->(List.length x)=4) wall];
   "F",[List.filter (fun x->(List.length x)=3) wall];
   "G",[List.filter (fun x->(List.length x)=2) wall];
   "H",[List.filter (fun x->(List.length x)=1) wall];
   "I",[List.filter (fun x->(List.length x)=0) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = List.partition (Vdw_chosen.test_joinability selector)  old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    if (a=[])||(b=[])
    then (a,b,None)  
    else
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (a,b,Some(Colored.variable_from_label lbl i,
              Colored.variable_from_label lbl j)) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW(Aff[],[],[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
       (D 0),[[],D 0];
       (E 0),[[],E 0];
       (F 0),[[],F 0];
       (G 0),[[],G 0];
       (H 0),[[],H 0];
       (I 0),[[],I 0];
])) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;

let explain_partition (selector,cv,a,b,opt_cvs) =
   let s_selector = "["^(String.concat ";" (Image.image string_of_int selector))^"]" in  
   let beginning_of_msg = "\n\n"^(Colored.to_string cv)^" "
   and end_of_msg = (
      if a=[] then "is uniformly incompatible with "^s_selector else  
      if b=[] then "is uniformly compatible with "^s_selector else   
      let (cv_i,cv_j) = Option.unpack opt_cvs in       
      " splits as "^(Colored.to_string cv_i)
      ^" + "^(Colored.to_string cv_j)) in 
   let msg = beginning_of_msg ^ end_of_msg in 
   (print_string msg ;flush stdout) ;;             

let partition selector cv =
    let old_state = (!main) in 
    let (a,b,opt_cvs) = Colored_variable_assignment.partition selector cv in 
    let _ = explain_partition (selector,cv,a,b,opt_cvs) in 
    let new_state = (
    match opt_cvs with 
    None -> 
      let affty=((selector,cv),b=[]) in 
      Partition_watcher.insert_affinity old_state affty
    |Some(cv_i,cv_j) -> 
        Partition_watcher.expand_using_partition 
        old_state ((selector,cv),(cv_i,cv_j)) 
    ) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Expansion.current n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

let decompose cvar = Partition_watcher.decompose (!main) cvar;;
let ugly_part cvar = Partition_watcher.ugly_part (!main) cvar;;
let ugly_ones () = Partition_watcher.ugly_ones (!main) ;;

end ;;  

let deco = Current_partition_watcher.decompose ;;
let oslo_compute = Current_partition_watcher.oslo_compute ;;
let up = Current_partition_watcher.ugly_part ;;

module Marshall_plan = struct 

let detect_missing_affinities n d=         
   let skeleton = Image.image (
     fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
   ) (Expansion.current n d) in 
   let old_state = (!(Current_partition_watcher.main)) in 
   let (PW(affinities,_,ll)) = old_state in 
   let combination2 = List.flatten(Image.image (fun old_pair->
      let (lamb,cvar) = old_pair in 
      match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
       (Some expansion) -> Image.image (fun
        (lamb2,cvar2)-> 
        (Ordered.merge oi lamb lamb2,cvar2)) expansion
      |None -> [old_pair]
      ) skeleton) in 
      fst(Affinity.analyse_combination affinities combination2)  
    ;;  

let  write_partition_commands_from_list l=
  let temp1 = Image.image (fun (lamb,cv)->
   "let passing = Current_partition_watcher.partition "^
   (intlist_to_string lamb)^" ("^(Colored.to_ocaml_readable_string cv)^") ;;"
   )  l in 
  "\n\n\n" ^ (String.concat "\n" temp1) ^ "\n\n\n" ;;   
  
let write_partition_commands n d =
    let missing_affs = detect_missing_affinities n d in 
    print_string(write_partition_commands_from_list missing_affs) ;;  

let rec compute_affinities_then_oslo (n,d) =
   let missing_affs = detect_missing_affinities n d in 
   if missing_affs = []
   then oslo_compute n d 
   else let _ = Image.image (
         fun (lamb,cv) -> Current_partition_watcher.partition lamb cv
        ) missing_affs in 
        compute_affinities_then_oslo (n,d) ;;

let compute_from_scratch n d =
    let temp1 = (Oslo.needed_intermediaries n d)@[n,d] in 
    let _ = Image.image compute_affinities_then_oslo temp1 in 
    Oslo.get n d;;

end ;;   

module Eisenhower = struct 

let rog i j = Colored.usual_rewriting(Og(i,j)) ;;
let order = Total_ordering.product 
     (fun x y->Total_ordering.for_integers y x)
     Total_ordering.for_integers ;;

exception Pusher_to_compute_list_of_adjusters ;;

let pusher_to_compute_list_of_adjusters 
    (n0,d0,treated,to_be_treated)=
    let temp1 = Image.image snd to_be_treated in 
    let temp4 = List.filter (fun (i,j)->
      (Current_partition_watcher.ugly_part 
        (rog i j))<>[]) temp1 in 
    if temp4 = [] then raise(Pusher_to_compute_list_of_adjusters) else
    let temp2 = Ordered.sort order temp4 in 
    let (n1,d1) = List.hd temp2 in 
    let temp3 = Expansion.allowed_substitution_by_in (n1,d1) to_be_treated in 
    (n0,d0,(n0-n1,d1-d0)::treated,temp3) ;;

let pusher_opt uple = 
   try Some(pusher_to_compute_list_of_adjusters uple) with
   _ -> None ;; 


let rec iterator_to_compute_list_of_adjusters uple =
   match pusher_opt uple with 
   Some(next_uple) -> iterator_to_compute_list_of_adjusters next_uple
   |None ->
      let (n0,d0,treated,to_be_treated)= uple in 
      List.rev treated ;;
     
let compute_list_of_adjusters n0 d0 =
   iterator_to_compute_list_of_adjusters 
   (n0,d0,[],Expansion.origin n0 d0) ;;   

let plus_of_int diff_d =
    if diff_d=0 then "" else
    let sd = string_of_int diff_d in    
    if diff_d>0 then "+"^sd else sd ;;  

let write_special_function_from_list l=
   let  temp1 = Ennig.index_everything l in 
   let temp2 = Image.image (fun (idx,(diff_n,diff_d))->
      let si = string_of_int idx 
      and siu = string_of_int (idx+1) 
      and sn = string_of_int(diff_n) 
      and sd = plus_of_int(diff_d) in 
   "   let temp"^siu^" = allowed_substitution_by_in "^
   "(n-"^sn^",d"^sd^") temp"^si^" in "   
   ) temp1 in
   let temp3 =
   "let special n d = "::
   "   let temp1 = origin n d in "::temp2 in 
   print_string("\n\n\n"^(String.concat "\n" temp3)^"\n\n\n");;

let determine_next_special_expansion n d =
   let temp1 = (Oslo.needed_intermediaries n d)@[n,d] in 
   let _ = Marshall_plan.compute_from_scratch n d in 
   let temp2 = List.filter(fun (i,j)->
      (Current_partition_watcher.ugly_part (rog i j))=[]) temp1 in 
   let (n0,d0) = List.hd temp2 in 
   let adjusters = compute_list_of_adjusters n0 d0 in    
   let _ = write_special_function_from_list adjusters in 
   (temp2,adjusters) ;;   

end ;;   

Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;
Oslo.add (15,3) (Colored_variable_assignment.get (D 0)) ;;
Oslo.add (15,4) (Colored_variable_assignment.get (E 0)) ;;
Oslo.add (15,5) (Colored_variable_assignment.get (F 0)) ;;
Oslo.add (15,6) (Colored_variable_assignment.get (G 0)) ;;
Oslo.add (15,7) (Colored_variable_assignment.get (H 0)) ;;
Oslo.add (15,8) (Colored_variable_assignment.get (I 0)) ;;

Image.image (fun n->
   Marshall_plan.compute_from_scratch n 0  
)(Ennig.ennig 16 33) ;;

let check = Current_partition_watcher.ugly_ones () ;;



(*

let z1 = Eisenhower.determine_next_special_expansion 33 0;;



let z1 = Current_partition_watcher.ugly_ones () ;;

let g1 = Oslo.needed_intermediaries 25 0;;
let g2 = [(16, 4); (16, 5); (17, 3); (17, 4); (18, 3); (18, 4); (19, 2); (19, 3);
   (20, 2); (20, 3); (21, 1); (21, 2); (22, 2); (23, 2); (24, 1); (25,0)] ;;
let g3=List.filter(fun (i,j)->(up (Og(i,j)))=[]) g2;;


let upp l=List.filter(fun (x,(i,j))->
   (up (Colored.usual_rewriting(Og(i,j))))<>[]) l;;

*)

(************************************************************************************************************************
Snippet 76 : Musings on the Vand der Waerden problem, version 16 : adding the
H variant, and oslo_compute 26 0 
************************************************************************************************************************)
open Needed_values ;;
type colored_variable = 
   A of int |B of int|C of int|D of int|E of int
   |F of int|G of int|H of int| Og of int * int ;;   
type list_of_affinities = Aff of ((int list * colored_variable) * bool)  list ;;    
type partition_watcher = 
   PW of 
   list_of_affinities *
   (((int list * colored_variable) * (colored_variable * colored_variable)) list) *
   ((colored_variable * (int list * colored_variable) list) list) ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;; 
let mea = Vdw_chosen.measure ;;
let lmea = Vdw_chosen.lower_measure ;;
let is_adm = Vdw_chosen.test_for_admissibility ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

let intlist_to_string lamb =
   "["^(String.concat ";" (Image.image string_of_int lamb))^"]" ;;

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  

module Colored = struct 

exception Data_for_variable_exn of colored_variable;;
let data_for_variable cvar = match cvar with 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) |(D k) ->("D",k) 
    |(E k) ->("E",k) |(F k) ->("F",k) |(G k) ->("G",k) |(H k) ->("H",k) 
    |(Og(_,_))->raise(Data_for_variable_exn(cvar)) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     if lbl = "D" then D k else 
     if lbl = "E" then E k else
     if lbl = "F" then F k else  
     if lbl = "G" then G k else   
     if lbl = "H" then H k else      
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let is_ugly cv = match cv with 
 (Og(_,_)) -> true |
  _ -> let (lbl,k) = data_for_variable cv in 
       (int_of_char(String.get lbl 0))>=68 ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d 
       [0,A 0;1, B 0;2, C 0;3, D 0;4, E 0;5, F 0;
        6, G 0;7, H 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

let to_ocaml_readable_string cv= match cv with  
   (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
   | _ -> let (lbl,k) = data_for_variable cv in 
          lbl^" "^(string_of_int k) ;; 

let order = ((fun cv1 cv2->
  let (lbl1,k1) = data_for_variable cv1 
  and (lbl2,k2) = data_for_variable cv2 in  
  let trial1 = Total_ordering.standard lbl1 lbl2 in 
  if trial1 <> Total_ordering_result_t.Equal then trial1 else 
   Total_ordering.standard k1 k2 
) : colored_variable Total_ordering_t.t) ;;

let order_for_pairs = ((fun (lamb1,cv1) (lamb2,cv2)->
   let trial1 = order cv1 cv2 in 
   if trial1 <> Total_ordering_result_t.Equal then trial1 else 
    (Total_ordering.silex_compare Total_ordering.standard ) lamb1 lamb2 
 ) : (int list * colored_variable) Total_ordering_t.t) ;;
 

end ;;  

module Affinity = struct

   let shadow (Aff l) cvar = 
      Option.filter_and_unpack (
         fun ((selector,cvar2),is_compatible) ->
           if cvar2 = cvar 
           then Some(selector,is_compatible)   
           else None
      ) l;;
  
  let expand_using_partition aff parti  =
     let (Aff(old_affinities)) = aff 
     and ((selector,partitioned),(part1,part2)) = parti in
     let temp1 = shadow aff partitioned in 
     let temp2 = List.flatten(Image.image (
        fun (selector2,is_compatible) ->
           [((selector2,part1),is_compatible);
            ((selector2,part2),is_compatible)]     
     ) temp1) in
     let new_affinities = old_affinities @ 
         [((selector,part1),true);
         ((selector,part2),false);]@temp2  in 
     Aff(new_affinities) ;;
  
  let insert affty  (Aff old_affinities)  =
     if List.mem affty old_affinities 
     then Aff old_affinities
     else Aff(affty :: old_affinities) ;;
  
  let analyse_combination (Aff affinities) combination =
     let temp0 = List.filter (
       fun (lamb,_)->Vdw_chosen.test_for_admissibility lamb
     ) combination in 
     let temp1 = Image.image (fun 
        pair -> 
         let res = (
          if fst pair = []
          then Some true
          else List.assoc_opt  pair affinities 
         ) in
         (pair,res)
     ) temp0 in 
     let filterer = (fun criterion ->
        Option.filter_and_unpack 
       (fun (pair,opt)->
         if opt=criterion then Some pair else None) temp1
     ) in 
     let bad_ones = filterer None in 
     let checked_version = filterer (Some true) in 
     (Ordered.sort Colored.order_for_pairs bad_ones,checked_version);;
   
  
  
end ;;   
  
module Expansion = struct

let subst_by_in cvar replacement_for_cvar l=
  let temp1 = List.flatten(Image.image (
     fun pair ->
      let (lamb2,cvar2) = pair in 
      if cvar2=cvar 
      then Image.image (
          fun (lamb3,cvar3)->
            (Ordered.merge oi lamb2 lamb3,cvar3)) replacement_for_cvar
      else [pair]      
  ) l) in
  List.filter (fun (lamb4,cvar4)->is_adm lamb4) temp1 ;; 

let origin n d =
   let delt = mea(n)-mea(n-1) in   
   List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
   [
     [],(n-1,d-delt);
     [n],(n-1,d+1-delt)
   ];;   

let allowed_substitution_by_in (n,d) expr = 
   subst_by_in (n,d) (origin n d) expr;;

let special1 n d =
   let temp1 = origin n d in 
   let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d+1) temp2 in
   temp3 ;;

let special2 n d =
   let temp1 = origin n d in 
   let temp2 = allowed_substitution_by_in (n-1,d+1) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d+1) temp2 in
   temp3 ;;

let special3 n d =
   let temp1 = origin n d in    
   let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d) temp2 in 
   let temp4 = allowed_substitution_by_in (n-2,d+1) temp3 in 
   let temp5 = allowed_substitution_by_in (n-3,d) temp4 in 
   let temp6 = allowed_substitution_by_in (n-4,d+1) temp5 in 
   let temp7 = allowed_substitution_by_in (n-5,d) temp6 in 
   let temp8 = allowed_substitution_by_in (n-5,d+1) temp7 in 
   let temp9 = allowed_substitution_by_in (n-6,d+1) temp8 in 
   temp9 ;;   
     
let special4 n d = 
      let temp1 = origin n d in 
      let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
      let temp3 = allowed_substitution_by_in (n-2,d-1) temp2 in 
      let temp4 = allowed_substitution_by_in (n-2,d) temp3 in 
      let temp5 = allowed_substitution_by_in (n-3,d-1) temp4 in 
      let temp6 = allowed_substitution_by_in (n-3,d) temp5 in 
      let temp7 = allowed_substitution_by_in (n-4,d-1) temp6 in 
      let temp8 = allowed_substitution_by_in (n-4,d) temp7 in 
      let temp9 = allowed_substitution_by_in (n-5,d) temp8 in 
      let temp10 = allowed_substitution_by_in (n-5,d+1) temp9 in 
      let temp11 = allowed_substitution_by_in (n-6,d-1) temp10 in 
      let temp12 = allowed_substitution_by_in (n-6,d) temp11 in 
      let temp13 = allowed_substitution_by_in (n-7,d) temp12 in 
      temp13 ;;

let special5 n d = 
         let temp1 = origin n d in 
         let temp2 = allowed_substitution_by_in (n-1,d+1) temp1 in 
         let temp3 = allowed_substitution_by_in (n-2,d+2) temp2 in 
         let temp4 = allowed_substitution_by_in (n-3,d+1) temp3 in 
         let temp5 = allowed_substitution_by_in (n-4,d) temp4 in 
         let temp6 = allowed_substitution_by_in (n-4,d+1) temp5 in 
         let temp7 = allowed_substitution_by_in (n-5,d) temp6 in 
         let temp8 = allowed_substitution_by_in (n-5,d+1) temp7 in 
         let temp9 = allowed_substitution_by_in (n-5,d+2) temp8 in 
         let temp10 = allowed_substitution_by_in (n-6,d) temp9 in 
         let temp11 = allowed_substitution_by_in (n-6,d+1) temp10 in 
         let temp12 = allowed_substitution_by_in (n-7,d+1) temp11 in 
         let temp13 = allowed_substitution_by_in (n-8,d) temp12 in 
         let temp14 = allowed_substitution_by_in (n-9,d+1) temp13 in 
         temp14 ;; 

let current n d = 
   if (n,d)=(18,1) then special1 n d else 
   if (n,d)=(21,0) then special2 n d else 
   if (n,d)=(22,1) then special3 n d else  
   if (n,d)=(23,2) then special4 n d else  
   if (n,d)=(25,1) then special5 n d else             
   origin n d ;;

end ;;   

let see = Expansion.current ;;

module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (Expansion.current n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;

let direct_descendants (n,d) =
   let temp1 = Image.image snd (Expansion.current n d) in 
   List.filter (fun (nn,dd)->
         ((Hashtbl.find_opt hashtbl_for_oslo (nn,dd)) = None)
         && (nn >= wall_position)   
   ) temp1 ;;      

let order_for_needed_intermediaries =
   Total_ordering.product 
      (Total_ordering.for_integers)
      (fun x y->Total_ordering.for_integers y x) ;;

exception Pusher_for_needed_intermediaries_exn ;;   

let pusher_for_needed_intermediaries (treated,to_be_treated) =
   let oh = order_for_needed_intermediaries in (* oh is for "order here" *)
   match (List.rev to_be_treated) with 
     [] -> raise (Pusher_for_needed_intermediaries_exn) 
    |pair :: others -> 
      let new_ones = Ordered.sort oh (direct_descendants pair) in 
   (Ordered.insert oh pair treated,
            Ordered.merge oh new_ones others) ;;  

let needed_intermediaries n d = 
   let rec tempf = (fun (treated,to_be_treated)->
      match (List.rev to_be_treated) with 
       [] -> treated 
      |pair :: others -> 
         let new_step = pusher_for_needed_intermediaries (treated,to_be_treated) in 
         tempf(new_step) 
   ) in 
   tempf([],direct_descendants (n,d) ) ;;      

end ;;

let og = Oslo.get ;;
let ni = Oslo.needed_intermediaries ;;


module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  ((selector,partitioned),(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition 
   (PW(old_affinities,old_parties,ll):t) parti  =
   let new_affinities =  Affinity.expand_using_partition old_affinities parti
   and new_parties = old_parties @ [parti]   in 
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_affinities,new_parties,new_ll):>t) ;;

let insert_affinity (PW(old_affinities,old_parties,ll)) affty  =
   PW(Affinity.insert affty old_affinities,old_parties,ll) ;;        
    

exception Missing_affinities of ( (int list) * colored_variable) list;;   

let check_combination_using_affinities combination (PW(affinities,_,_):t)=
  let (bad_ones,checked_version) = Affinity.analyse_combination affinities combination in 
  if bad_ones <> []
  then raise(Missing_affinities bad_ones)
  else checked_version;;

let expand_using_t combination (pw:t) =
   let (PW(_,_,ll)) = pw in 
   let ref_for_unknowns = ref [] in 
   let combination2 = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in
   let new_combination = check_combination_using_affinities combination2  pw in
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(affinities,parties,ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(affinities,parties,ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(affinities,parties,List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   (intlist_to_string lamb)^","^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW (_,_,ll)):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;

let decompose ((PW (_,_,ll)):t) cvar =
     List.assoc cvar ll ;;

let ugly_part ((PW (_,_,ll)):t) cvar =
     let temp1 = List.assoc cvar ll in 
     List.filter (fun (lamb,cv)->Colored.is_ugly cv) temp1 ;;     

let ugly_ones ((PW (_,_,ll)):t) =
   Option.filter_and_unpack(fun (cv2,expansion)->
       let bad_part = List.filter 
       (fun (lamb,cv3)->Colored.is_ugly cv3) expansion in 
       if (bad_part = [])||(Colored.is_ugly cv2)
       then None  
       else Some(cv2,bad_part)
   )  ll ;; 


end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
   "D",[List.filter (fun x->(List.length x)=5) wall];
   "E",[List.filter (fun x->(List.length x)=4) wall];
   "F",[List.filter (fun x->(List.length x)=3) wall];
   "G",[List.filter (fun x->(List.length x)=2) wall];
   "H",[List.filter (fun x->(List.length x)=1) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = List.partition (Vdw_chosen.test_joinability selector)  old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    if (a=[])||(b=[])
    then (a,b,None)  
    else
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (a,b,Some(Colored.variable_from_label lbl i,
              Colored.variable_from_label lbl j)) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW(Aff[],[],[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
       (D 0),[[],D 0];
       (E 0),[[],E 0];
       (F 0),[[],F 0];
       (G 0),[[],G 0];
       (H 0),[[],H 0];
])) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;

let explain_partition (selector,cv,a,b,opt_cvs) =
   let s_selector = "["^(String.concat ";" (Image.image string_of_int selector))^"]" in  
   let beginning_of_msg = "\n\n"^(Colored.to_string cv)^" "
   and end_of_msg = (
      if a=[] then "is uniformly incompatible with "^s_selector else  
      if b=[] then "is uniformly compatible with "^s_selector else   
      let (cv_i,cv_j) = Option.unpack opt_cvs in       
      " splits as "^(Colored.to_string cv_i)
      ^" + "^(Colored.to_string cv_j)) in 
   let msg = beginning_of_msg ^ end_of_msg in 
   (print_string msg ;flush stdout) ;;             

let partition selector cv =
    let old_state = (!main) in 
    let (a,b,opt_cvs) = Colored_variable_assignment.partition selector cv in 
    let _ = explain_partition (selector,cv,a,b,opt_cvs) in 
    let new_state = (
    match opt_cvs with 
    None -> 
      let affty=((selector,cv),b=[]) in 
      Partition_watcher.insert_affinity old_state affty
    |Some(cv_i,cv_j) -> 
        Partition_watcher.expand_using_partition 
        old_state ((selector,cv),(cv_i,cv_j)) 
    ) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Expansion.current n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

let decompose cvar = Partition_watcher.decompose (!main) cvar;;
let ugly_part cvar = Partition_watcher.ugly_part (!main) cvar;;
let ugly_ones () = Partition_watcher.ugly_ones (!main) ;;

end ;;  

let deco = Current_partition_watcher.decompose ;;
let oslo_compute = Current_partition_watcher.oslo_compute ;;
let up = Current_partition_watcher.ugly_part ;;

module Marshall_plan = struct 

let detect_missing_affinities n d=         
   let skeleton = Image.image (
     fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
   ) (Expansion.current n d) in 
   let old_state = (!(Current_partition_watcher.main)) in 
   let (PW(affinities,_,ll)) = old_state in 
   let combination2 = List.flatten(Image.image (fun old_pair->
      let (lamb,cvar) = old_pair in 
      match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
       (Some expansion) -> Image.image (fun
        (lamb2,cvar2)-> 
        (Ordered.merge oi lamb lamb2,cvar2)) expansion
      |None -> [old_pair]
      ) skeleton) in 
      fst(Affinity.analyse_combination affinities combination2)  
    ;;  

let  write_partition_commands_from_list l=
  let temp1 = Image.image (fun (lamb,cv)->
   "let passing = Current_partition_watcher.partition "^
   (intlist_to_string lamb)^" ("^(Colored.to_ocaml_readable_string cv)^") ;;"
   )  l in 
  "\n\n\n" ^ (String.concat "\n" temp1) ^ "\n\n\n" ;;   
  
let write_partition_commands n d =
    let missing_affs = detect_missing_affinities n d in 
    print_string(write_partition_commands_from_list missing_affs) ;;  

let rec compute_affinities_then_oslo (n,d) =
   let missing_affs = detect_missing_affinities n d in 
   if missing_affs = []
   then oslo_compute n d 
   else let _ = Image.image (
         fun (lamb,cv) -> Current_partition_watcher.partition lamb cv
        ) missing_affs in 
        compute_affinities_then_oslo (n,d) ;;

let compute_from_scratch n d =
    let temp1 = (Oslo.needed_intermediaries n d)@[n,d] in 
    let _ = Image.image compute_affinities_then_oslo temp1 in 
    Oslo.get n d;;

end ;;   

module Eisenhower = struct 

let rog i j = Colored.usual_rewriting(Og(i,j)) ;;
let order = Total_ordering.product 
     (fun x y->Total_ordering.for_integers y x)
     Total_ordering.for_integers ;;

exception Pusher_to_compute_list_of_adjusters ;;

let pusher_to_compute_list_of_adjusters 
    (n0,d0,treated,to_be_treated)=
    let temp1 = Image.image snd to_be_treated in 
    let temp4 = List.filter (fun (i,j)->
      (Current_partition_watcher.ugly_part 
        (rog i j))<>[]) temp1 in 
    if temp4 = [] then raise(Pusher_to_compute_list_of_adjusters) else
    let temp2 = Ordered.sort order temp4 in 
    let (n1,d1) = List.hd temp2 in 
    let temp3 = Expansion.allowed_substitution_by_in (n1,d1) to_be_treated in 
    (n0,d0,(n0-n1,d1-d0)::treated,temp3) ;;

let pusher_opt uple = 
   try Some(pusher_to_compute_list_of_adjusters uple) with
   _ -> None ;; 


let rec iterator_to_compute_list_of_adjusters uple =
   match pusher_opt uple with 
   Some(next_uple) -> iterator_to_compute_list_of_adjusters next_uple
   |None ->
      let (n0,d0,treated,to_be_treated)= uple in 
      List.rev treated ;;
     
let compute_list_of_adjusters n0 d0 =
   iterator_to_compute_list_of_adjusters 
   (n0,d0,[],Expansion.origin n0 d0) ;;   

let plus_of_int diff_d =
    if diff_d=0 then "" else
    let sd = string_of_int diff_d in    
    if diff_d>0 then "+"^sd else sd ;;  

let write_special_function_from_list l=
   let  temp1 = Ennig.index_everything l in 
   let temp2 = Image.image (fun (idx,(diff_n,diff_d))->
      let si = string_of_int idx 
      and siu = string_of_int (idx+1) 
      and sn = string_of_int(diff_n) 
      and sd = plus_of_int(diff_d) in 
   "   let temp"^siu^" = allowed_substitution_by_in "^
   "(n-"^sn^",d"^sd^") temp"^si^" in "   
   ) temp1 in
   let temp3 =
   "let special n d = "::
   "   let temp1 = origin n d in "::temp2 in 
   print_string("\n\n\n"^(String.concat "\n" temp3)^"\n\n\n");;

let determine_next_special_expansion n d =
   let temp1 = (Oslo.needed_intermediaries n d)@[n,d] in 
   let _ = Marshall_plan.compute_from_scratch n d in 
   let temp2 = List.filter(fun (i,j)->
      (Current_partition_watcher.ugly_part (rog i j))=[]) temp1 in 
   let (n0,d0) = List.hd temp2 in 
   let adjusters = compute_list_of_adjusters n0 d0 in    
   let _ = write_special_function_from_list adjusters in 
   (temp2,adjusters) ;;   

end ;;   

Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;
Oslo.add (15,3) (Colored_variable_assignment.get (D 0)) ;;
Oslo.add (15,4) (Colored_variable_assignment.get (E 0)) ;;
Oslo.add (15,5) (Colored_variable_assignment.get (F 0)) ;;
Oslo.add (15,6) (Colored_variable_assignment.get (G 0)) ;;
Oslo.add (15,7) (Colored_variable_assignment.get (H 0)) ;;

Image.image (fun n->
   Marshall_plan.compute_from_scratch n 0  
)(Ennig.ennig 16 26) ;;

let check = Current_partition_watcher.ugly_ones () ;;



(*

let z1 = Eisenhower.determine_next_special_expansion 26 0;;



let z1 = Current_partition_watcher.ugly_ones () ;;

let g1 = Oslo.needed_intermediaries 25 0;;
let g2 = [(16, 4); (16, 5); (17, 3); (17, 4); (18, 3); (18, 4); (19, 2); (19, 3);
   (20, 2); (20, 3); (21, 1); (21, 2); (22, 2); (23, 2); (24, 1); (25,0)] ;;
let g3=List.filter(fun (i,j)->(up (Og(i,j)))=[]) g2;;


let upp l=List.filter(fun (x,(i,j))->
   (up (Colored.usual_rewriting(Og(i,j))))<>[]) l;;

*)


(************************************************************************************************************************
Snippet 75 : Musings on the Vand der Waerden problem, version 15 : working
Eisenhower.determine_next_special_expansion, and oslo_compute 25 0
************************************************************************************************************************)
open Needed_values ;;

type colored_variable = 
   A of int |B of int|C of int|D of int|E of int
   |F of int|G of int| Og of int * int ;;   
type list_of_affinities = Aff of ((int list * colored_variable) * bool)  list ;;    
type partition_watcher = 
   PW of 
   list_of_affinities *
   (((int list * colored_variable) * (colored_variable * colored_variable)) list) *
   ((colored_variable * (int list * colored_variable) list) list) ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;; 
let mea = Vdw_chosen.measure ;;
let lmea = Vdw_chosen.lower_measure ;;
let is_adm = Vdw_chosen.test_for_admissibility ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

let intlist_to_string lamb =
   "["^(String.concat ";" (Image.image string_of_int lamb))^"]" ;;

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  

module Colored = struct 

exception Data_for_variable_exn of colored_variable;;
let data_for_variable cvar = match cvar with 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) |(D k) ->("D",k) 
    |(E k) ->("E",k) |(F k) ->("F",k) |(G k) ->("G",k) 
    |(Og(_,_))->raise(Data_for_variable_exn(cvar)) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     if lbl = "D" then D k else 
     if lbl = "E" then E k else
     if lbl = "F" then F k else  
     if lbl = "G" then F k else     
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let is_ugly cv = match cv with 
 (Og(_,_)) -> true |
  _ -> let (lbl,k) = data_for_variable cv in 
       (int_of_char(String.get lbl 0))>=68 ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d 
       [0,A 0;1, B 0;2, C 0;3, D 0;4, E 0;5, F 0;6, G 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

let to_ocaml_readable_string cv= match cv with  
   (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
   | _ -> let (lbl,k) = data_for_variable cv in 
          lbl^" "^(string_of_int k) ;; 

let order = ((fun cv1 cv2->
  let (lbl1,k1) = data_for_variable cv1 
  and (lbl2,k2) = data_for_variable cv2 in  
  let trial1 = Total_ordering.standard lbl1 lbl2 in 
  if trial1 <> Total_ordering_result_t.Equal then trial1 else 
   Total_ordering.standard k1 k2 
) : colored_variable Total_ordering_t.t) ;;

let order_for_pairs = ((fun (lamb1,cv1) (lamb2,cv2)->
   let trial1 = order cv1 cv2 in 
   if trial1 <> Total_ordering_result_t.Equal then trial1 else 
    (Total_ordering.silex_compare Total_ordering.standard ) lamb1 lamb2 
 ) : (int list * colored_variable) Total_ordering_t.t) ;;
 

end ;;  

module Affinity = struct

   let shadow (Aff l) cvar = 
      Option.filter_and_unpack (
         fun ((selector,cvar2),is_compatible) ->
           if cvar2 = cvar 
           then Some(selector,is_compatible)   
           else None
      ) l;;
  
  let expand_using_partition aff parti  =
     let (Aff(old_affinities)) = aff 
     and ((selector,partitioned),(part1,part2)) = parti in
     let temp1 = shadow aff partitioned in 
     let temp2 = List.flatten(Image.image (
        fun (selector2,is_compatible) ->
           [((selector2,part1),is_compatible);
            ((selector2,part2),is_compatible)]     
     ) temp1) in
     let new_affinities = old_affinities @ 
         [((selector,part1),true);
         ((selector,part2),false);]@temp2  in 
     Aff(new_affinities) ;;
  
  let insert affty  (Aff old_affinities)  =
     if List.mem affty old_affinities 
     then Aff old_affinities
     else Aff(affty :: old_affinities) ;;
  
  let analyse_combination (Aff affinities) combination =
     let temp0 = List.filter (
       fun (lamb,_)->Vdw_chosen.test_for_admissibility lamb
     ) combination in 
     let temp1 = Image.image (fun 
        pair -> 
         let res = (
          if fst pair = []
          then Some true
          else List.assoc_opt  pair affinities 
         ) in
         (pair,res)
     ) temp0 in 
     let filterer = (fun criterion ->
        Option.filter_and_unpack 
       (fun (pair,opt)->
         if opt=criterion then Some pair else None) temp1
     ) in 
     let bad_ones = filterer None in 
     let checked_version = filterer (Some true) in 
     (Ordered.sort Colored.order_for_pairs bad_ones,checked_version);;
   
  
  
end ;;   
  
module Expansion = struct

let subst_by_in cvar replacement_for_cvar l=
  let temp1 = List.flatten(Image.image (
     fun pair ->
      let (lamb2,cvar2) = pair in 
      if cvar2=cvar 
      then Image.image (
          fun (lamb3,cvar3)->
            (Ordered.merge oi lamb2 lamb3,cvar3)) replacement_for_cvar
      else [pair]      
  ) l) in
  List.filter (fun (lamb4,cvar4)->is_adm lamb4) temp1 ;; 

let origin n d =
   let delt = mea(n)-mea(n-1) in   
   List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
   [
     [],(n-1,d-delt);
     [n],(n-1,d+1-delt)
   ];;   

let allowed_substitution_by_in (n,d) expr = 
   subst_by_in (n,d) (origin n d) expr;;

let special1 n d =
   let temp1 = origin n d in 
   let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d+1) temp2 in
   temp3 ;;

let special2 n d =
   let temp1 = origin n d in 
   let temp2 = allowed_substitution_by_in (n-1,d+1) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d+1) temp2 in
   temp3 ;;

let special3 n d =
   let temp1 = origin n d in    
   let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d) temp2 in 
   let temp4 = allowed_substitution_by_in (n-2,d+1) temp3 in 
   let temp5 = allowed_substitution_by_in (n-3,d) temp4 in 
   let temp6 = allowed_substitution_by_in (n-4,d+1) temp5 in 
   let temp7 = allowed_substitution_by_in (n-5,d) temp6 in 
   let temp8 = allowed_substitution_by_in (n-5,d+1) temp7 in 
   let temp9 = allowed_substitution_by_in (n-6,d+1) temp8 in 
   temp9 ;;   
     
let special4 n d = 
      let temp1 = origin n d in 
      let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
      let temp3 = allowed_substitution_by_in (n-2,d-1) temp2 in 
      let temp4 = allowed_substitution_by_in (n-2,d) temp3 in 
      let temp5 = allowed_substitution_by_in (n-3,d-1) temp4 in 
      let temp6 = allowed_substitution_by_in (n-3,d) temp5 in 
      let temp7 = allowed_substitution_by_in (n-4,d-1) temp6 in 
      let temp8 = allowed_substitution_by_in (n-4,d) temp7 in 
      let temp9 = allowed_substitution_by_in (n-5,d) temp8 in 
      let temp10 = allowed_substitution_by_in (n-5,d+1) temp9 in 
      let temp11 = allowed_substitution_by_in (n-6,d-1) temp10 in 
      let temp12 = allowed_substitution_by_in (n-6,d) temp11 in 
      let temp13 = allowed_substitution_by_in (n-7,d) temp12 in 
      temp13 ;;

let current n d = 
   if (n,d)=(18,1) then special1 n d else 
   if (n,d)=(21,0) then special2 n d else 
   if (n,d)=(22,1) then special3 n d else  
   if (n,d)=(23,2) then special4 n d else          
   origin n d ;;

end ;;   

let see = Expansion.current ;;

module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (Expansion.current n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;

let direct_descendants (n,d) =
   let temp1 = Image.image snd (Expansion.current n d) in 
   List.filter (fun (nn,dd)->
         ((Hashtbl.find_opt hashtbl_for_oslo (nn,dd)) = None)
         && (nn >= wall_position)   
   ) temp1 ;;      

let order_for_needed_intermediaries =
   Total_ordering.product 
      (Total_ordering.for_integers)
      (fun x y->Total_ordering.for_integers y x) ;;

exception Pusher_for_needed_intermediaries_exn ;;   

let pusher_for_needed_intermediaries (treated,to_be_treated) =
   let oh = order_for_needed_intermediaries in (* oh is for "order here" *)
   match (List.rev to_be_treated) with 
     [] -> raise (Pusher_for_needed_intermediaries_exn) 
    |pair :: others -> 
      let new_ones = Ordered.sort oh (direct_descendants pair) in 
   (Ordered.insert oh pair treated,
            Ordered.merge oh new_ones others) ;;  

let needed_intermediaries n d = 
   let rec tempf = (fun (treated,to_be_treated)->
      match (List.rev to_be_treated) with 
       [] -> treated 
      |pair :: others -> 
         let new_step = pusher_for_needed_intermediaries (treated,to_be_treated) in 
         tempf(new_step) 
   ) in 
   tempf([],direct_descendants (n,d) ) ;;      

end ;;

let og = Oslo.get ;;
let ni = Oslo.needed_intermediaries ;;


module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  ((selector,partitioned),(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition 
   (PW(old_affinities,old_parties,ll):t) parti  =
   let new_affinities =  Affinity.expand_using_partition old_affinities parti
   and new_parties = old_parties @ [parti]   in 
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_affinities,new_parties,new_ll):>t) ;;

let insert_affinity (PW(old_affinities,old_parties,ll)) affty  =
   PW(Affinity.insert affty old_affinities,old_parties,ll) ;;        
    

exception Missing_affinities of ( (int list) * colored_variable) list;;   

let check_combination_using_affinities combination (PW(affinities,_,_):t)=
  let (bad_ones,checked_version) = Affinity.analyse_combination affinities combination in 
  if bad_ones <> []
  then raise(Missing_affinities bad_ones)
  else checked_version;;

let expand_using_t combination (pw:t) =
   let (PW(_,_,ll)) = pw in 
   let ref_for_unknowns = ref [] in 
   let combination2 = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in
   let new_combination = check_combination_using_affinities combination2  pw in
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(affinities,parties,ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(affinities,parties,ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(affinities,parties,List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   (intlist_to_string lamb)^","^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW (_,_,ll)):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;

let decompose ((PW (_,_,ll)):t) cvar =
     List.assoc cvar ll ;;

let ugly_part ((PW (_,_,ll)):t) cvar =
     let temp1 = List.assoc cvar ll in 
     List.filter (fun (lamb,cv)->Colored.is_ugly cv) temp1 ;;     

let ugly_ones ((PW (_,_,ll)):t) =
   Option.filter_and_unpack(fun (cv2,expansion)->
       let bad_part = List.filter 
       (fun (lamb,cv3)->Colored.is_ugly cv3) expansion in 
       if (bad_part = [])||(Colored.is_ugly cv2)
       then None  
       else Some(cv2,bad_part)
   )  ll ;; 


end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
   "D",[List.filter (fun x->(List.length x)=5) wall];
   "E",[List.filter (fun x->(List.length x)=4) wall];
   "F",[List.filter (fun x->(List.length x)=3) wall];
   "G",[List.filter (fun x->(List.length x)=2) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = List.partition (Vdw_chosen.test_joinability selector)  old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    if (a=[])||(b=[])
    then (a,b,None)  
    else
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (a,b,Some(Colored.variable_from_label lbl i,
              Colored.variable_from_label lbl j)) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW(Aff[],[],[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
       (D 0),[[],D 0];
       (E 0),[[],E 0];
       (F 0),[[],F 0];
       (G 0),[[],G 0];
])) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;

let explain_partition (selector,cv,a,b,opt_cvs) =
   let s_selector = "["^(String.concat ";" (Image.image string_of_int selector))^"]" in  
   let beginning_of_msg = "\n\n"^(Colored.to_string cv)^" "
   and end_of_msg = (
      if a=[] then "is uniformly incompatible with "^s_selector else  
      if b=[] then "is uniformly compatible with "^s_selector else   
      let (cv_i,cv_j) = Option.unpack opt_cvs in       
      " splits as "^(Colored.to_string cv_i)
      ^" + "^(Colored.to_string cv_j)) in 
   let msg = beginning_of_msg ^ end_of_msg in 
   (print_string msg ;flush stdout) ;;             

let partition selector cv =
    let old_state = (!main) in 
    let (a,b,opt_cvs) = Colored_variable_assignment.partition selector cv in 
    let _ = explain_partition (selector,cv,a,b,opt_cvs) in 
    let new_state = (
    match opt_cvs with 
    None -> 
      let affty=((selector,cv),b=[]) in 
      Partition_watcher.insert_affinity old_state affty
    |Some(cv_i,cv_j) -> 
        Partition_watcher.expand_using_partition 
        old_state ((selector,cv),(cv_i,cv_j)) 
    ) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Expansion.current n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

let decompose cvar = Partition_watcher.decompose (!main) cvar;;
let ugly_part cvar = Partition_watcher.ugly_part (!main) cvar;;
let ugly_ones () = Partition_watcher.ugly_ones (!main) ;;

end ;;  

let deco = Current_partition_watcher.decompose ;;
let oslo_compute = Current_partition_watcher.oslo_compute ;;
let up = Current_partition_watcher.ugly_part ;;

module Marshall_plan = struct 

let detect_missing_affinities n d=         
   let skeleton = Image.image (
     fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
   ) (Expansion.current n d) in 
   let old_state = (!(Current_partition_watcher.main)) in 
   let (PW(affinities,_,ll)) = old_state in 
   let combination2 = List.flatten(Image.image (fun old_pair->
      let (lamb,cvar) = old_pair in 
      match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
       (Some expansion) -> Image.image (fun
        (lamb2,cvar2)-> 
        (Ordered.merge oi lamb lamb2,cvar2)) expansion
      |None -> [old_pair]
      ) skeleton) in 
      fst(Affinity.analyse_combination affinities combination2)  
    ;;  

let  write_partition_commands_from_list l=
  let temp1 = Image.image (fun (lamb,cv)->
   "let passing = Current_partition_watcher.partition "^
   (intlist_to_string lamb)^" ("^(Colored.to_ocaml_readable_string cv)^") ;;"
   )  l in 
  "\n\n\n" ^ (String.concat "\n" temp1) ^ "\n\n\n" ;;   
  
let write_partition_commands n d =
    let missing_affs = detect_missing_affinities n d in 
    print_string(write_partition_commands_from_list missing_affs) ;;  

let rec compute_affinities_then_oslo (n,d) =
   let missing_affs = detect_missing_affinities n d in 
   if missing_affs = []
   then oslo_compute n d 
   else let _ = Image.image (
         fun (lamb,cv) -> Current_partition_watcher.partition lamb cv
        ) missing_affs in 
        compute_affinities_then_oslo (n,d) ;;

let compute_from_scratch n d =
    let temp1 = (Oslo.needed_intermediaries n d)@[n,d] in 
    let _ = Image.image compute_affinities_then_oslo temp1 in 
    Oslo.get n d;;

end ;;   

module Eisenhower = struct 

let rog i j = Colored.usual_rewriting(Og(i,j)) ;;
let order = Total_ordering.product 
     (fun x y->Total_ordering.for_integers y x)
     Total_ordering.for_integers ;;

exception Pusher_to_compute_list_of_adjusters ;;

let pusher_to_compute_list_of_adjusters 
    (n0,d0,treated,to_be_treated)=
    let temp1 = Image.image snd to_be_treated in 
    let temp4 = List.filter (fun (i,j)->
      (Current_partition_watcher.ugly_part 
        (rog i j))<>[]) temp1 in 
    if temp4 = [] then raise(Pusher_to_compute_list_of_adjusters) else
    let temp2 = Ordered.sort order temp4 in 
    let (n1,d1) = List.hd temp2 in 
    let temp3 = Expansion.allowed_substitution_by_in (n1,d1) to_be_treated in 
    (n0,d0,(n0-n1,d1-d0)::treated,temp3) ;;

let pusher_opt uple = 
   try Some(pusher_to_compute_list_of_adjusters uple) with
   _ -> None ;; 


let rec iterator_to_compute_list_of_adjusters uple =
   match pusher_opt uple with 
   Some(next_uple) -> iterator_to_compute_list_of_adjusters next_uple
   |None ->
      let (n0,d0,treated,to_be_treated)= uple in 
      List.rev treated ;;
     
let compute_list_of_adjusters n0 d0 =
   iterator_to_compute_list_of_adjusters 
   (n0,d0,[],Expansion.origin n0 d0) ;;   

let plus_of_int diff_d =
    if diff_d=0 then "" else
    let sd = string_of_int diff_d in    
    if diff_d>0 then "+"^sd else sd ;;  

let write_special_function_from_list l=
   let  temp1 = Ennig.index_everything l in 
   let temp2 = Image.image (fun (idx,(diff_n,diff_d))->
      let si = string_of_int idx 
      and siu = string_of_int (idx+1) 
      and sn = string_of_int(diff_n) 
      and sd = plus_of_int(diff_d) in 
   "   let temp"^siu^" = allowed_substitution_by_in "^
   "(n-"^sn^",d"^sd^") temp"^si^" in "   
   ) temp1 in
   let temp3 =
   "let special n d = "::
   "   let temp1 = origin n d in "::temp2 in 
   print_string("\n\n\n"^(String.concat "\n" temp3)^"\n\n\n");;

let determine_next_special_expansion n d =
   let temp1 = (Oslo.needed_intermediaries n d)@[n,d] in 
   let _ = Marshall_plan.compute_from_scratch n d in 
   let temp2 = List.filter(fun (i,j)->
      (Current_partition_watcher.ugly_part (rog i j))=[]) temp1 in 
   let (n0,d0) = List.hd temp2 in 
   let adjusters = compute_list_of_adjusters n0 d0 in    
   let _ = write_special_function_from_list adjusters in 
   (temp2,adjusters) ;;   

end ;;   

Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;
Oslo.add (15,3) (Colored_variable_assignment.get (D 0)) ;;
Oslo.add (15,4) (Colored_variable_assignment.get (E 0)) ;;
Oslo.add (15,5) (Colored_variable_assignment.get (F 0)) ;;
Oslo.add (15,6) (Colored_variable_assignment.get (G 0)) ;;

Image.image (fun n->
   Marshall_plan.compute_from_scratch n 0  
)(Ennig.ennig 16 25) ;;

let check = Current_partition_watcher.ugly_ones () ;;



(*

let z1 = Eisenhower.determine_next_special_expansion 25 0;;



let z1 = Eisenhower.determine_next_special_expansion 25 0;;

let z1 = Current_partition_watcher.ugly_ones () ;;

let g1 = Oslo.needed_intermediaries 25 0;;
let g2 = [(16, 4); (16, 5); (17, 3); (17, 4); (18, 3); (18, 4); (19, 2); (19, 3);
   (20, 2); (20, 3); (21, 1); (21, 2); (22, 2); (23, 2); (24, 1); (25,0)] ;;
let g3=List.filter(fun (i,j)->(up (Og(i,j)))=[]) g2;;


let upp l=List.filter(fun (x,(i,j))->
   (up (Colored.usual_rewriting(Og(i,j))))<>[]) l;;

let n0=23 and d0=2;;
let h1 = Expansion.current n0 d0;;
let h2 = Expansion.allowed_substitution_by_in (n0-1,d0) h1;;
let h3 = Expansion.allowed_substitution_by_in (n0-2,d0-1) h2;;
let h4 = Expansion.allowed_substitution_by_in (n0-2,d0) h3;;
let h5 = Expansion.allowed_substitution_by_in (n0-3,d0-1) h4;;
let h6 = Expansion.allowed_substitution_by_in (n0-3,d0) h5;;
let h7 = Expansion.allowed_substitution_by_in (n0-4,d0-1) h6;;
let h8 = Expansion.allowed_substitution_by_in (n0-4,d0) h7;;
let h9 = Expansion.allowed_substitution_by_in (n0-5,d0) h8;;
let h10 = Expansion.allowed_substitution_by_in (n0-5,d0+1) h9;;
let h11 = Expansion.allowed_substitution_by_in (n0-6,d0-1) h10;;
let h12 = Expansion.allowed_substitution_by_in (n0-6,d0) h11;;
let h13 = Expansion.allowed_substitution_by_in (n0-7,d0) h12;;




*)




(************************************************************************************************************************
Snippet 74 : Musings on the Vand der Waerden problem, version 15 : debug 
Eisenhower.determine_next_special_expansion
************************************************************************************************************************)
open Needed_values ;;
type colored_variable = 
   A of int |B of int|C of int|D of int|E of int
   |F of int|G of int| Og of int * int ;;   
type list_of_affinities = Aff of ((int list * colored_variable) * bool)  list ;;    
type partition_watcher = 
   PW of 
   list_of_affinities *
   (((int list * colored_variable) * (colored_variable * colored_variable)) list) *
   ((colored_variable * (int list * colored_variable) list) list) ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;; 
let mea = Vdw_chosen.measure ;;
let lmea = Vdw_chosen.lower_measure ;;
let is_adm = Vdw_chosen.test_for_admissibility ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

let intlist_to_string lamb =
   "["^(String.concat ";" (Image.image string_of_int lamb))^"]" ;;

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  

module Colored = struct 

exception Data_for_variable_exn of colored_variable;;
let data_for_variable cvar = match cvar with 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) |(D k) ->("D",k) 
    |(E k) ->("E",k) |(F k) ->("F",k) |(G k) ->("G",k) 
    |(Og(_,_))->raise(Data_for_variable_exn(cvar)) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     if lbl = "D" then D k else 
     if lbl = "E" then E k else
     if lbl = "F" then F k else  
     if lbl = "G" then F k else     
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let is_ugly cv = match cv with 
 (Og(_,_)) -> true |
  _ -> let (lbl,k) = data_for_variable cv in 
       (int_of_char(String.get lbl 0))>=68 ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d 
       [0,A 0;1, B 0;2, C 0;3, D 0;4, E 0;5, F 0;6, G 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

let to_ocaml_readable_string cv= match cv with  
   (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
   | _ -> let (lbl,k) = data_for_variable cv in 
          lbl^" "^(string_of_int k) ;; 

let order = ((fun cv1 cv2->
  let (lbl1,k1) = data_for_variable cv1 
  and (lbl2,k2) = data_for_variable cv2 in  
  let trial1 = Total_ordering.standard lbl1 lbl2 in 
  if trial1 <> Total_ordering_result_t.Equal then trial1 else 
   Total_ordering.standard k1 k2 
) : colored_variable Total_ordering_t.t) ;;

let order_for_pairs = ((fun (lamb1,cv1) (lamb2,cv2)->
   let trial1 = order cv1 cv2 in 
   if trial1 <> Total_ordering_result_t.Equal then trial1 else 
    (Total_ordering.silex_compare Total_ordering.standard ) lamb1 lamb2 
 ) : (int list * colored_variable) Total_ordering_t.t) ;;
 

end ;;  

module Affinity = struct

   let shadow (Aff l) cvar = 
      Option.filter_and_unpack (
         fun ((selector,cvar2),is_compatible) ->
           if cvar2 = cvar 
           then Some(selector,is_compatible)   
           else None
      ) l;;
  
  let expand_using_partition aff parti  =
     let (Aff(old_affinities)) = aff 
     and ((selector,partitioned),(part1,part2)) = parti in
     let temp1 = shadow aff partitioned in 
     let temp2 = List.flatten(Image.image (
        fun (selector2,is_compatible) ->
           [((selector2,part1),is_compatible);
            ((selector2,part2),is_compatible)]     
     ) temp1) in
     let new_affinities = old_affinities @ 
         [((selector,part1),true);
         ((selector,part2),false);]@temp2  in 
     Aff(new_affinities) ;;
  
  let insert affty  (Aff old_affinities)  =
     if List.mem affty old_affinities 
     then Aff old_affinities
     else Aff(affty :: old_affinities) ;;
  
  let analyse_combination (Aff affinities) combination =
     let temp0 = List.filter (
       fun (lamb,_)->Vdw_chosen.test_for_admissibility lamb
     ) combination in 
     let temp1 = Image.image (fun 
        pair -> 
         let res = (
          if fst pair = []
          then Some true
          else List.assoc_opt  pair affinities 
         ) in
         (pair,res)
     ) temp0 in 
     let filterer = (fun criterion ->
        Option.filter_and_unpack 
       (fun (pair,opt)->
         if opt=criterion then Some pair else None) temp1
     ) in 
     let bad_ones = filterer None in 
     let checked_version = filterer (Some true) in 
     (Ordered.sort Colored.order_for_pairs bad_ones,checked_version);;
   
  
  
end ;;   
  
module Expansion = struct

let subst_by_in cvar replacement_for_cvar l=
  let temp1 = List.flatten(Image.image (
     fun pair ->
      let (lamb2,cvar2) = pair in 
      if cvar2=cvar 
      then Image.image (
          fun (lamb3,cvar3)->
            (Ordered.merge oi lamb2 lamb3,cvar3)) replacement_for_cvar
      else [pair]      
  ) l) in
  List.filter (fun (lamb4,cvar4)->is_adm lamb4) temp1 ;; 

let origin n d =
   let delt = mea(n)-mea(n-1) in   
   List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
   [
     [],(n-1,d-delt);
     [n],(n-1,d+1-delt)
   ];;   

let allowed_substitution_by_in (n,d) expr = 
   subst_by_in (n,d) (origin n d) expr;;

let special1 n d =
   let temp1 = origin n d in 
   let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d+1) temp2 in
   temp3 ;;

let special2 n d =
   let temp1 = origin n d in 
   let temp2 = allowed_substitution_by_in (n-1,d+1) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d+1) temp2 in
   temp3 ;;

let special3 n d =
   let temp1 = origin n d in    
   let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d) temp2 in 
   let temp4 = allowed_substitution_by_in (n-2,d+1) temp3 in 
   let temp5 = allowed_substitution_by_in (n-3,d) temp4 in 
   let temp6 = allowed_substitution_by_in (n-4,d+1) temp5 in 
   let temp7 = allowed_substitution_by_in (n-5,d) temp6 in 
   let temp8 = allowed_substitution_by_in (n-5,d+1) temp7 in 
   let temp9 = allowed_substitution_by_in (n-6,d+1) temp8 in 
   temp9 ;;   
     

let current n d = 
   if (n,d)=(18,1) then special1 n d else 
   if (n,d)=(21,0) then special2 n d else 
   if (n,d)=(22,1) then special1 n d else       
   origin n d ;;

end ;;   

let see = Expansion.current ;;

module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (Expansion.current n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;

let direct_descendants (n,d) =
   let temp1 = Image.image snd (Expansion.current n d) in 
   List.filter (fun (nn,dd)->
         ((Hashtbl.find_opt hashtbl_for_oslo (nn,dd)) = None)
         && (nn >= wall_position)   
   ) temp1 ;;      

let order_for_needed_intermediaries =
   Total_ordering.product 
      (Total_ordering.for_integers)
      (fun x y->Total_ordering.for_integers y x) ;;

exception Pusher_for_needed_intermediaries_exn ;;   

let pusher_for_needed_intermediaries (treated,to_be_treated) =
   let oh = order_for_needed_intermediaries in (* oh is for "order here" *)
   match (List.rev to_be_treated) with 
     [] -> raise (Pusher_for_needed_intermediaries_exn) 
    |pair :: others -> 
      let new_ones = Ordered.sort oh (direct_descendants pair) in 
   (Ordered.insert oh pair treated,
            Ordered.merge oh new_ones others) ;;  

let needed_intermediaries n d = 
   let rec tempf = (fun (treated,to_be_treated)->
      match (List.rev to_be_treated) with 
       [] -> treated 
      |pair :: others -> 
         let new_step = pusher_for_needed_intermediaries (treated,to_be_treated) in 
         tempf(new_step) 
   ) in 
   tempf([],direct_descendants (n,d) ) ;;      

end ;;

let og = Oslo.get ;;
let ni = Oslo.needed_intermediaries ;;


module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  ((selector,partitioned),(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition 
   (PW(old_affinities,old_parties,ll):t) parti  =
   let new_affinities =  Affinity.expand_using_partition old_affinities parti
   and new_parties = old_parties @ [parti]   in 
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_affinities,new_parties,new_ll):>t) ;;

let insert_affinity (PW(old_affinities,old_parties,ll)) affty  =
   PW(Affinity.insert affty old_affinities,old_parties,ll) ;;        
    

exception Missing_affinities of ( (int list) * colored_variable) list;;   

let check_combination_using_affinities combination (PW(affinities,_,_):t)=
  let (bad_ones,checked_version) = Affinity.analyse_combination affinities combination in 
  if bad_ones <> []
  then raise(Missing_affinities bad_ones)
  else checked_version;;

let expand_using_t combination (pw:t) =
   let (PW(_,_,ll)) = pw in 
   let ref_for_unknowns = ref [] in 
   let combination2 = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in
   let new_combination = check_combination_using_affinities combination2  pw in
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(affinities,parties,ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(affinities,parties,ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(affinities,parties,List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   (intlist_to_string lamb)^","^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW (_,_,ll)):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;

let decompose ((PW (_,_,ll)):t) cvar =
     List.assoc cvar ll ;;

let ugly_part ((PW (_,_,ll)):t) cvar =
     let temp1 = List.assoc cvar ll in 
     List.filter (fun (lamb,cv)->Colored.is_ugly cv) temp1 ;;     

let ugly_ones ((PW (_,_,ll)):t) =
   Option.filter_and_unpack(fun (cv2,expansion)->
       let bad_part = List.filter 
       (fun (lamb,cv3)->Colored.is_ugly cv3) expansion in 
       if (bad_part = [])||(Colored.is_ugly cv2)
       then None  
       else Some(cv2,bad_part)
   )  ll ;; 


end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
   "D",[List.filter (fun x->(List.length x)=5) wall];
   "E",[List.filter (fun x->(List.length x)=4) wall];
   "F",[List.filter (fun x->(List.length x)=3) wall];
   "G",[List.filter (fun x->(List.length x)=2) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = List.partition (Vdw_chosen.test_joinability selector)  old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    if (a=[])||(b=[])
    then (a,b,None)  
    else
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (a,b,Some(Colored.variable_from_label lbl i,
              Colored.variable_from_label lbl j)) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW(Aff[],[],[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
       (D 0),[[],D 0];
       (E 0),[[],E 0];
       (F 0),[[],F 0];
       (G 0),[[],G 0];
])) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;

let explain_partition (selector,cv,a,b,opt_cvs) =
   let s_selector = "["^(String.concat ";" (Image.image string_of_int selector))^"]" in  
   let beginning_of_msg = "\n\n"^(Colored.to_string cv)^" "
   and end_of_msg = (
      if a=[] then "is uniformly incompatible with "^s_selector else  
      if b=[] then "is uniformly compatible with "^s_selector else   
      let (cv_i,cv_j) = Option.unpack opt_cvs in       
      " splits as "^(Colored.to_string cv_i)
      ^" + "^(Colored.to_string cv_j)) in 
   let msg = beginning_of_msg ^ end_of_msg in 
   (print_string msg ;flush stdout) ;;             

let partition selector cv =
    let old_state = (!main) in 
    let (a,b,opt_cvs) = Colored_variable_assignment.partition selector cv in 
    let _ = explain_partition (selector,cv,a,b,opt_cvs) in 
    let new_state = (
    match opt_cvs with 
    None -> 
      let affty=((selector,cv),b=[]) in 
      Partition_watcher.insert_affinity old_state affty
    |Some(cv_i,cv_j) -> 
        Partition_watcher.expand_using_partition 
        old_state ((selector,cv),(cv_i,cv_j)) 
    ) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Expansion.current n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

let decompose cvar = Partition_watcher.decompose (!main) cvar;;
let ugly_part cvar = Partition_watcher.ugly_part (!main) cvar;;
let ugly_ones () = Partition_watcher.ugly_ones (!main) ;;

end ;;  

let deco = Current_partition_watcher.decompose ;;
let oslo_compute = Current_partition_watcher.oslo_compute ;;
let up = Current_partition_watcher.ugly_part ;;

module Marshall_plan = struct 

let detect_missing_affinities n d=         
   let skeleton = Image.image (
     fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
   ) (Expansion.current n d) in 
   let old_state = (!(Current_partition_watcher.main)) in 
   let (PW(affinities,_,ll)) = old_state in 
   let combination2 = List.flatten(Image.image (fun old_pair->
      let (lamb,cvar) = old_pair in 
      match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
       (Some expansion) -> Image.image (fun
        (lamb2,cvar2)-> 
        (Ordered.merge oi lamb lamb2,cvar2)) expansion
      |None -> [old_pair]
      ) skeleton) in 
      fst(Affinity.analyse_combination affinities combination2)  
    ;;  

let  write_partition_commands_from_list l=
  let temp1 = Image.image (fun (lamb,cv)->
   "let passing = Current_partition_watcher.partition "^
   (intlist_to_string lamb)^" ("^(Colored.to_ocaml_readable_string cv)^") ;;"
   )  l in 
  "\n\n\n" ^ (String.concat "\n" temp1) ^ "\n\n\n" ;;   
  
let write_partition_commands n d =
    let missing_affs = detect_missing_affinities n d in 
    print_string(write_partition_commands_from_list missing_affs) ;;  

let rec compute_affinities_then_oslo (n,d) =
   let missing_affs = detect_missing_affinities n d in 
   if missing_affs = []
   then oslo_compute n d 
   else let _ = Image.image (
         fun (lamb,cv) -> Current_partition_watcher.partition lamb cv
        ) missing_affs in 
        compute_affinities_then_oslo (n,d) ;;

let compute_from_scratch n d =
    let temp1 = (Oslo.needed_intermediaries n d)@[n,d] in 
    let _ = Image.image compute_affinities_then_oslo temp1 in 
    Oslo.get n d;;

end ;;   

module Eisenhower = struct 

let rog i j = Colored.usual_rewriting(Og(i,j)) ;;
let order = Total_ordering.product 
     (fun x y->Total_ordering.for_integers y x)
     Total_ordering.for_integers ;;

exception Pusher_to_compute_list_of_adjusters ;;

let pusher_to_compute_list_of_adjusters 
    (n0,d0,treated,to_be_treated)=
    let temp1 = Image.image snd to_be_treated in 
    let temp4 = List.filter (fun (i,j)->
      (Current_partition_watcher.ugly_part 
        (rog i j))<>[]) temp1 in 
    if temp4 = [] then raise(Pusher_to_compute_list_of_adjusters) else
    let temp2 = Ordered.sort order temp4 in 
    let (n1,d1) = List.hd temp2 in 
    let temp3 = Expansion.allowed_substitution_by_in (n1,d1) to_be_treated in 
    (n0,d0,(n1-n0,d1-d0)::treated,temp3) ;;

let ref_for_pusher = ref [] ;;

let pusher_to_compute_list_of_adjusters1 x =
    let y = pusher_to_compute_list_of_adjusters x in 
    let _ = (ref_for_pusher:=(x,y)::(!ref_for_pusher)) in 
    y ;; 

let pusher_opt uple = 
   try Some(pusher_to_compute_list_of_adjusters1 uple) with
   _ -> None ;; 


let rec iterator_to_compute_list_of_adjusters uple =
   match pusher_opt uple with 
   Some(next_uple) -> iterator_to_compute_list_of_adjusters next_uple
   |None ->
      let (n0,d0,treated,to_be_treated)= uple in 
      List.rev treated ;;

     
let compute_list_of_adjusters n0 d0 =
   iterator_to_compute_list_of_adjusters 
   (n0,d0,[],Expansion.origin n0 d0) ;;   

let determine_next_special_expansion n d =
   let temp1 = (Oslo.needed_intermediaries n d)@[n,d] in 
   let _ = Marshall_plan.compute_from_scratch n d in 
   let temp2 = List.filter(fun (i,j)->
      (Current_partition_watcher.ugly_part (rog i j))=[]) temp1 in 
   let (n0,d0) = List.hd temp2 in 
   let adjusters = compute_list_of_adjusters n0 d0 in    
   (temp2,adjusters) ;;   

end ;;   

Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;
Oslo.add (15,3) (Colored_variable_assignment.get (D 0)) ;;
Oslo.add (15,4) (Colored_variable_assignment.get (E 0)) ;;
Oslo.add (15,5) (Colored_variable_assignment.get (F 0)) ;;
Oslo.add (15,6) (Colored_variable_assignment.get (G 0)) ;;

Image.image (fun n->
   Marshall_plan.compute_from_scratch n 0  
)(Ennig.ennig 16 24) ;;

let z1 = Eisenhower.determine_next_special_expansion 25 0;;

let z2 = List.rev(!(Eisenhower.ref_for_pusher));;

(*



let z1 = Eisenhower.determine_next_special_expansion 25 0;;

let z1 = Current_partition_watcher.ugly_ones () ;;

let g1 = Oslo.needed_intermediaries 25 0;;
let g2 = [(16, 4); (16, 5); (17, 3); (17, 4); (18, 3); (18, 4); (19, 2); (19, 3);
   (20, 2); (20, 3); (21, 1); (21, 2); (22, 2); (23, 2); (24, 1); (25,0)] ;;
let g3=List.filter(fun (i,j)->(up (Og(i,j)))=[]) g2;;


let upp l=List.filter(fun (x,(i,j))->
   (up (Colored.usual_rewriting(Og(i,j))))<>[]) l;;

let n0=23 and d0=2;;
let h1 = Expansion.current n0 d0;;
let h2 = Expansion.allowed_substitution_by_in (n0-1,d0) h1;;
let h3 = Expansion.allowed_substitution_by_in (n0-2,d0-1) h2;;
let h4 = Expansion.allowed_substitution_by_in (n0-2,d0) h3;;
let h5 = Expansion.allowed_substitution_by_in (n0-3,d0-1) h4;;
let h6 = Expansion.allowed_substitution_by_in (n0-3,d0) h5;;
let h7 = Expansion.allowed_substitution_by_in (n0-4,d0-1) h6;;
let h8 = Expansion.allowed_substitution_by_in (n0-4,d0) h7;;
let h9 = Expansion.allowed_substitution_by_in (n0-5,d0) h8;;
let h10 = Expansion.allowed_substitution_by_in (n0-5,d0+1) h9;;
let h11 = Expansion.allowed_substitution_by_in (n0-6,d0-1) h10;;
let h12 = Expansion.allowed_substitution_by_in (n0-6,d0) h11;;
let h13 = Expansion.allowed_substitution_by_in (n0-7,d0) h12;;



let u0 = (23, 2, [(-1, 0)], [([], (22, 1)); ([23], (21, 1)); ([22; 23], (21, 2))]) ;;
let u1 = Eisenhower.pusher_to_compute_list_of_adjusters u0 ;;
open Eisenhower ;;

let (n0,d0,treated,to_be_treated) = u0 ;;
let temp1 = Image.image snd to_be_treated ;;
let temp4 = List.filter (fun (i,j)->
   (Current_partition_watcher.ugly_part 
     (rog i j))<>[]) temp1 ;;
let temp2 = Ordered.sort order temp4 ;;
let (n1,d1) = List.hd temp2 ;;
let temp3 = Expansion.allowed_substitution_by_in (n1,d1) to_be_treated ;;
let next_u = (n0,d0,(n1-n0,d1-d0)::treated,temp3) ;; 

*)



(************************************************************************************************************************
Snippet 73 : Musings on the Vand der Waerden problem, version 14 : add 
Current_partition_watcher.ugly_part , oslo_compute 24 0
************************************************************************************************************************)
open Needed_values ;;

type colored_variable = 
   A of int |B of int|C of int|D of int|E of int|F of int| Og of int * int ;;   
type list_of_affinities = Aff of ((int list * colored_variable) * bool)  list ;;    
type partition_watcher = 
   PW of 
   list_of_affinities *
   (((int list * colored_variable) * (colored_variable * colored_variable)) list) *
   ((colored_variable * (int list * colored_variable) list) list) ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;; 
let mea = Vdw_chosen.measure ;;
let lmea = Vdw_chosen.lower_measure ;;
let is_adm = Vdw_chosen.test_for_admissibility ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

let intlist_to_string lamb =
   "["^(String.concat ";" (Image.image string_of_int lamb))^"]" ;;

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  

module Colored = struct 

exception Data_for_variable_exn of colored_variable;;
let data_for_variable cvar = match cvar with 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) |(D k) ->("D",k) 
    |(E k) ->("E",k) |(F k) ->("F",k) 
    |(Og(_,_))->raise(Data_for_variable_exn(cvar)) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     if lbl = "D" then D k else 
     if lbl = "E" then E k else
     if lbl = "F" then F k else    
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let is_ugly cv = match cv with 
 (Og(_,_)) -> true |
  _ -> let (lbl,k) = data_for_variable cv in 
       (int_of_char(String.get lbl 0))>=68 ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d [0,A 0;1, B 0;2, C 0;3, D 0;4, E 0;5, F 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

let to_ocaml_readable_string cv= match cv with  
   (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
   | _ -> let (lbl,k) = data_for_variable cv in 
          lbl^" "^(string_of_int k) ;; 

let order = ((fun cv1 cv2->
  let (lbl1,k1) = data_for_variable cv1 
  and (lbl2,k2) = data_for_variable cv2 in  
  let trial1 = Total_ordering.standard lbl1 lbl2 in 
  if trial1 <> Total_ordering_result_t.Equal then trial1 else 
   Total_ordering.standard k1 k2 
) : colored_variable Total_ordering_t.t) ;;

let order_for_pairs = ((fun (lamb1,cv1) (lamb2,cv2)->
   let trial1 = order cv1 cv2 in 
   if trial1 <> Total_ordering_result_t.Equal then trial1 else 
    (Total_ordering.silex_compare Total_ordering.standard ) lamb1 lamb2 
 ) : (int list * colored_variable) Total_ordering_t.t) ;;
 

end ;;  

module Affinity = struct

   let shadow (Aff l) cvar = 
      Option.filter_and_unpack (
         fun ((selector,cvar2),is_compatible) ->
           if cvar2 = cvar 
           then Some(selector,is_compatible)   
           else None
      ) l;;
  
  let expand_using_partition aff parti  =
     let (Aff(old_affinities)) = aff 
     and ((selector,partitioned),(part1,part2)) = parti in
     let temp1 = shadow aff partitioned in 
     let temp2 = List.flatten(Image.image (
        fun (selector2,is_compatible) ->
           [((selector2,part1),is_compatible);
            ((selector2,part2),is_compatible)]     
     ) temp1) in
     let new_affinities = old_affinities @ 
         [((selector,part1),true);
         ((selector,part2),false);]@temp2  in 
     Aff(new_affinities) ;;
  
  let insert affty  (Aff old_affinities)  =
     if List.mem affty old_affinities 
     then Aff old_affinities
     else Aff(affty :: old_affinities) ;;
  
  let analyse_combination (Aff affinities) combination =
     let temp0 = List.filter (
       fun (lamb,_)->Vdw_chosen.test_for_admissibility lamb
     ) combination in 
     let temp1 = Image.image (fun 
        pair -> 
         let res = (
          if fst pair = []
          then Some true
          else List.assoc_opt  pair affinities 
         ) in
         (pair,res)
     ) temp0 in 
     let filterer = (fun criterion ->
        Option.filter_and_unpack 
       (fun (pair,opt)->
         if opt=criterion then Some pair else None) temp1
     ) in 
     let bad_ones = filterer None in 
     let checked_version = filterer (Some true) in 
     (Ordered.sort Colored.order_for_pairs bad_ones,checked_version);;
   
  
  
end ;;   
  
module Expansion = struct

let subst_by_in cvar replacement_for_cvar l=
  let temp1 = List.flatten(Image.image (
     fun pair ->
      let (lamb2,cvar2) = pair in 
      if cvar2=cvar 
      then Image.image (
          fun (lamb3,cvar3)->
            (Ordered.merge oi lamb2 lamb3,cvar3)) replacement_for_cvar
      else [pair]      
  ) l) in
  List.filter (fun (lamb4,cvar4)->is_adm lamb4) temp1 ;; 

let origin n d =
   let delt = mea(n)-mea(n-1) in   
   List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
   [
     [],(n-1,d-delt);
     [n],(n-1,d+1-delt)
   ];;   

let allowed_substitution_by_in (n,d) expr = 
   subst_by_in (n,d) (origin n d) expr;;

let special1 n d =
   let temp1 = origin n d in 
   let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d+1) temp2 in
   temp3 ;;

let special2 n d =
   let temp1 = origin n d in 
   let temp2 = allowed_substitution_by_in (n-1,d+1) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d+1) temp2 in
   temp3 ;;

let special3 n d =
   let temp1 = origin n d in    
   let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d) temp2 in 
   let temp4 = allowed_substitution_by_in (n-2,d+1) temp3 in 
   let temp5 = allowed_substitution_by_in (n-3,d) temp4 in 
   let temp6 = allowed_substitution_by_in (n-4,d+1) temp5 in 
   let temp7 = allowed_substitution_by_in (n-5,d) temp6 in 
   let temp8 = allowed_substitution_by_in (n-5,d+1) temp7 in 
   let temp9 = allowed_substitution_by_in (n-6,d+1) temp8 in 
   temp9 ;;   
     

let current n d = 
   if (n,d)=(18,1) then special1 n d else 
   if (n,d)=(21,0) then special2 n d else 
   if (n,d)=(22,1) then special1 n d else       
   origin n d ;;

end ;;   

let see = Expansion.current ;;

module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (Expansion.current n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;

let direct_descendants (n,d) =
   let temp1 = Image.image snd (Expansion.current n d) in 
   List.filter (fun (nn,dd)->
         ((Hashtbl.find_opt hashtbl_for_oslo (nn,dd)) = None)
         && (nn >= wall_position)   
   ) temp1 ;;      

exception Pusher_for_needed_intermediaries_exn ;;   

let pusher_for_needed_intermediaries (treated,to_be_treated) =
   let oh = Total_ordering.standard2 in (* oh is for "order here" *)
   match (List.rev to_be_treated) with 
     [] -> raise (Pusher_for_needed_intermediaries_exn) 
    |pair :: others -> 
      let new_ones = Ordered.sort oh (direct_descendants pair) in 
   (Ordered.insert oh pair treated,
            Ordered.merge oh new_ones others) ;;  

let needed_intermediaries n d = 
   let rec tempf = (fun (treated,to_be_treated)->
      match (List.rev to_be_treated) with 
       [] -> treated 
      |pair :: others -> 
         let new_step = pusher_for_needed_intermediaries (treated,to_be_treated) in 
         tempf(new_step) 
   ) in 
   tempf([],direct_descendants (n,d) ) ;;      

end ;;

let og = Oslo.get ;;
let ni = Oslo.needed_intermediaries ;;


module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  ((selector,partitioned),(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition 
   (PW(old_affinities,old_parties,ll):t) parti  =
   let new_affinities =  Affinity.expand_using_partition old_affinities parti
   and new_parties = old_parties @ [parti]   in 
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_affinities,new_parties,new_ll):>t) ;;

let insert_affinity (PW(old_affinities,old_parties,ll)) affty  =
   PW(Affinity.insert affty old_affinities,old_parties,ll) ;;        
    

exception Missing_affinities of ( (int list) * colored_variable) list;;   

let check_combination_using_affinities combination (PW(affinities,_,_):t)=
  let (bad_ones,checked_version) = Affinity.analyse_combination affinities combination in 
  if bad_ones <> []
  then raise(Missing_affinities bad_ones)
  else checked_version;;

let expand_using_t combination (pw:t) =
   let (PW(_,_,ll)) = pw in 
   let ref_for_unknowns = ref [] in 
   let combination2 = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in
   let new_combination = check_combination_using_affinities combination2  pw in
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(affinities,parties,ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(affinities,parties,ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(affinities,parties,List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   (intlist_to_string lamb)^","^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW (_,_,ll)):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;

let decompose ((PW (_,_,ll)):t) cvar =
     List.assoc cvar ll ;;

let ugly_part ((PW (_,_,ll)):t) cvar =
     let temp1 = List.assoc cvar ll in 
     List.filter (fun (lamb,cv)->Colored.is_ugly cv) temp1 ;;     

let ugly_ones ((PW (_,_,ll)):t) =
   Option.filter_and_unpack(fun (cv2,expansion)->
       let bad_part = List.filter 
       (fun (lamb,cv3)->Colored.is_ugly cv3) expansion in 
       if bad_part = []
       then None  
       else Some(cv2,bad_part)
   )  ll ;; 


end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
   "D",[List.filter (fun x->(List.length x)=5) wall];
   "E",[List.filter (fun x->(List.length x)=4) wall];
   "F",[List.filter (fun x->(List.length x)=3) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = List.partition (Vdw_chosen.test_joinability selector)  old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    if (a=[])||(b=[])
    then (a,b,None)  
    else
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (a,b,Some(Colored.variable_from_label lbl i,
              Colored.variable_from_label lbl j)) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW(Aff[],[],[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
       (D 0),[[],D 0];
       (E 0),[[],E 0];
       (F 0),[[],F 0];
])) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;

let explain_partition (selector,cv,a,b,opt_cvs) =
   let s_selector = "["^(String.concat ";" (Image.image string_of_int selector))^"]" in  
   let beginning_of_msg = "\n\n"^(Colored.to_string cv)^" "
   and end_of_msg = (
      if a=[] then "is uniformly incompatible with "^s_selector else  
      if b=[] then "is uniformly compatible with "^s_selector else   
      let (cv_i,cv_j) = Option.unpack opt_cvs in       
      " splits as "^(Colored.to_string cv_i)
      ^" + "^(Colored.to_string cv_j)) in 
   let msg = beginning_of_msg ^ end_of_msg in 
   (print_string msg ;flush stdout) ;;             

let partition selector cv =
    let old_state = (!main) in 
    let (a,b,opt_cvs) = Colored_variable_assignment.partition selector cv in 
    let _ = explain_partition (selector,cv,a,b,opt_cvs) in 
    let new_state = (
    match opt_cvs with 
    None -> 
      let affty=((selector,cv),b=[]) in 
      Partition_watcher.insert_affinity old_state affty
    |Some(cv_i,cv_j) -> 
        Partition_watcher.expand_using_partition 
        old_state ((selector,cv),(cv_i,cv_j)) 
    ) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Expansion.current n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

let decompose cvar = Partition_watcher.decompose (!main) cvar;;
let ugly_part cvar = Partition_watcher.ugly_part (!main) cvar;;
let ugly_ones () = Partition_watcher.ugly_ones (!main) ;;

end ;;  

let deco = Current_partition_watcher.decompose ;;
let oslo_compute = Current_partition_watcher.oslo_compute ;;
let up = Current_partition_watcher.ugly_part ;;

module Marshall_plan = struct 

let detect_missing_affinities n d=         
   let skeleton = Image.image (
     fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
   ) (Expansion.current n d) in 
   let old_state = (!(Current_partition_watcher.main)) in 
   let (PW(affinities,_,ll)) = old_state in 
   let combination2 = List.flatten(Image.image (fun old_pair->
      let (lamb,cvar) = old_pair in 
      match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
       (Some expansion) -> Image.image (fun
        (lamb2,cvar2)-> 
        (Ordered.merge oi lamb lamb2,cvar2)) expansion
      |None -> [old_pair]
      ) skeleton) in 
      fst(Affinity.analyse_combination affinities combination2)  
    ;;  

let  write_partition_commands_from_list l=
  let temp1 = Image.image (fun (lamb,cv)->
   "let passing = Current_partition_watcher.partition "^
   (intlist_to_string lamb)^" ("^(Colored.to_ocaml_readable_string cv)^") ;;"
   )  l in 
  "\n\n\n" ^ (String.concat "\n" temp1) ^ "\n\n\n" ;;   
  
let write_partition_commands n d =
    let missing_affs = detect_missing_affinities n d in 
    print_string(write_partition_commands_from_list missing_affs) ;;  

let rec compute_affinities_then_oslo (n,d) =
   let missing_affs = detect_missing_affinities n d in 
   if missing_affs = []
   then oslo_compute n d 
   else let _ = Image.image (
         fun (lamb,cv) -> Current_partition_watcher.partition lamb cv
        ) missing_affs in 
        compute_affinities_then_oslo (n,d) ;;

let compute_from_scratch n d =
    let temp1 = (Oslo.needed_intermediaries n d)@[n,d] in 
    let _ = Image.image compute_affinities_then_oslo temp1 in 
    Oslo.get n d;;

end ;;   

let wpc = Marshall_plan.write_partition_commands ;;

Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;
Oslo.add (15,3) (Colored_variable_assignment.get (D 0)) ;;
Oslo.add (15,4) (Colored_variable_assignment.get (E 0)) ;;
Oslo.add (15,5) (Colored_variable_assignment.get (F 0)) ;;

Image.image (fun n->
   Marshall_plan.compute_from_scratch n 0  
)(Ennig.ennig 16 24) ;;



(*
let g1 = Oslo.needed_intermediaries 24 0;;
let g2 = [(16, 2); (16, 3); (16, 4); (17, 1); (17, 2); (17, 3); (18, 2); (18, 3);
 (19, 1); (19, 2); (20, 1); (20, 2); (21, 1); (22, 1); (23, 1); (24,0)] ;;
let g3=List.filter(fun (i,j)->(up (Og(i,j)))=[]) g2;;


let upp l=List.filter(fun (x,(i,j))->
   (up (Colored.usual_rewriting(Og(i,j))))<>[]) l;;

let n0=23 and d0=1;;
let h1 = Expansion.current n0 d0;;
let h2 = Expansion.allowed_substitution_by_in (n0-1,d0) h1;;
let h3 = Expansion.allowed_substitution_by_in (n0-2,d0) h2;;
let h4 = Expansion.allowed_substitution_by_in (n0-2,d0+1) h3;;
let h5 = Expansion.allowed_substitution_by_in (n0-3,d0) h4;;
let h6 = Expansion.allowed_substitution_by_in (n0-4,d0+1) h5;;
let h7 = Expansion.allowed_substitution_by_in (n0-5,d0) h6;;
let h8 = Expansion.allowed_substitution_by_in (n0-5,d0+1) h7;;
let h9 = Expansion.allowed_substitution_by_in (n0-6,d0+1) h8;;

*)



(************************************************************************************************************************
Snippet 72 : Musings on the Vand der Waerden problem, version 13 : add
Current_partition_watcher.ugly_part and Marshall_plan.compute_from_scratch,
oslo_compute 21 0 has no D or E component
************************************************************************************************************************)
open Needed_values ;;
type colored_variable = 
   A of int |B of int|C of int|D of int|E of int| Og of int * int ;;   
type list_of_affinities = Aff of ((int list * colored_variable) * bool)  list ;;    
type partition_watcher = 
   PW of 
   list_of_affinities *
   (((int list * colored_variable) * (colored_variable * colored_variable)) list) *
   ((colored_variable * (int list * colored_variable) list) list) ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;; 
let mea = Vdw_chosen.measure ;;
let lmea = Vdw_chosen.lower_measure ;;
let is_adm = Vdw_chosen.test_for_admissibility ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

let intlist_to_string lamb =
   "["^(String.concat ";" (Image.image string_of_int lamb))^"]" ;;

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  

module Colored = struct 

exception Data_for_variable_exn ;;
let data_for_variable = function 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) |(D k) ->("D",k) 
    |(E k) ->("E",k) 
    |(Og(_,_))->raise(Data_for_variable_exn) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     if lbl = "D" then D k else 
     if lbl = "E" then E k else   
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let is_ugly cv = match cv with 
 (Og(_,_)) -> true |
  _ -> let (lbl,k) = data_for_variable cv in 
       (int_of_char(String.get lbl 0))>=68 ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d [0,A 0;1, B 0;2, C 0;3, D 0;4, E 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

let to_ocaml_readable_string cv= match cv with  
   (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
   | _ -> let (lbl,k) = data_for_variable cv in 
          lbl^" "^(string_of_int k) ;; 

let order = ((fun cv1 cv2->
  let (lbl1,k1) = data_for_variable cv1 
  and (lbl2,k2) = data_for_variable cv2 in  
  let trial1 = Total_ordering.standard lbl1 lbl2 in 
  if trial1 <> Total_ordering_result_t.Equal then trial1 else 
   Total_ordering.standard k1 k2 
) : colored_variable Total_ordering_t.t) ;;

let order_for_pairs = ((fun (lamb1,cv1) (lamb2,cv2)->
   let trial1 = order cv1 cv2 in 
   if trial1 <> Total_ordering_result_t.Equal then trial1 else 
    (Total_ordering.silex_compare Total_ordering.standard ) lamb1 lamb2 
 ) : (int list * colored_variable) Total_ordering_t.t) ;;
 

end ;;  

module Affinity = struct

   let shadow (Aff l) cvar = 
      Option.filter_and_unpack (
         fun ((selector,cvar2),is_compatible) ->
           if cvar2 = cvar 
           then Some(selector,is_compatible)   
           else None
      ) l;;
  
  let expand_using_partition aff parti  =
     let (Aff(old_affinities)) = aff 
     and ((selector,partitioned),(part1,part2)) = parti in
     let temp1 = shadow aff partitioned in 
     let temp2 = List.flatten(Image.image (
        fun (selector2,is_compatible) ->
           [((selector2,part1),is_compatible);
            ((selector2,part2),is_compatible)]     
     ) temp1) in
     let new_affinities = old_affinities @ 
         [((selector,part1),true);
         ((selector,part2),false);]@temp2  in 
     Aff(new_affinities) ;;
  
  let insert affty  (Aff old_affinities)  =
     if List.mem affty old_affinities 
     then Aff old_affinities
     else Aff(affty :: old_affinities) ;;
  
  let analyse_combination (Aff affinities) combination =
     let temp0 = List.filter (
       fun (lamb,_)->Vdw_chosen.test_for_admissibility lamb
     ) combination in 
     let temp1 = Image.image (fun 
        pair -> 
         let res = (
          if fst pair = []
          then Some true
          else List.assoc_opt  pair affinities 
         ) in
         (pair,res)
     ) temp0 in 
     let filterer = (fun criterion ->
        Option.filter_and_unpack 
       (fun (pair,opt)->
         if opt=criterion then Some pair else None) temp1
     ) in 
     let bad_ones = filterer None in 
     let checked_version = filterer (Some true) in 
     (Ordered.sort Colored.order_for_pairs bad_ones,checked_version);;
   
  
  
end ;;   
  
module Expansion = struct

let subst_by_in cvar replacement_for_cvar l=
  let temp1 = List.flatten(Image.image (
     fun pair ->
      let (lamb2,cvar2) = pair in 
      if cvar2=cvar 
      then Image.image (
          fun (lamb3,cvar3)->
            (Ordered.merge oi lamb2 lamb3,cvar3)) replacement_for_cvar
      else [pair]      
  ) l) in
  List.filter (fun (lamb4,cvar4)->is_adm lamb4) temp1 ;; 

let origin n d =
   let delt = mea(n)-mea(n-1) in   
   List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
   [
     [],(n-1,d-delt);
     [n],(n-1,d+1-delt)
   ];;   

let allowed_substitution_by_in (n,d) expr = 
   subst_by_in (n,d) (origin n d) expr;;

let special1 n d =
   let temp1 = origin n d in 
   let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d+1) temp2 in
   temp3 ;;

let special2 n d =
   let temp1 = origin n d in 
   let temp2 = allowed_substitution_by_in (n-1,d+1) temp1 in 
   let temp3 = allowed_substitution_by_in (n-2,d+1) temp2 in
   temp3 ;;
     

let current n d = 
   if (n,d)=(18,1) then special1 n d else 
   if (n,d)=(21,0) then special2 n d else    
   origin n d ;;

end ;;   

let see = Expansion.current ;;

module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (Expansion.current n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;

let direct_descendants (n,d) =
   let temp1 = Image.image snd (Expansion.current n d) in 
   List.filter (fun (nn,dd)->
         ((Hashtbl.find_opt hashtbl_for_oslo (nn,dd)) = None)
         && (nn >= wall_position)   
   ) temp1 ;;      

exception Pusher_for_needed_intermediaries_exn ;;   

let pusher_for_needed_intermediaries (treated,to_be_treated) =
   let oh = Total_ordering.standard2 in (* oh is for "order here" *)
   match (List.rev to_be_treated) with 
     [] -> raise (Pusher_for_needed_intermediaries_exn) 
    |pair :: others -> 
      let new_ones = Ordered.sort oh (direct_descendants pair) in 
   (Ordered.insert oh pair treated,
            Ordered.merge oh new_ones others) ;;  

let needed_intermediaries n d = 
   let rec tempf = (fun (treated,to_be_treated)->
      match (List.rev to_be_treated) with 
       [] -> treated 
      |pair :: others -> 
         let new_step = pusher_for_needed_intermediaries (treated,to_be_treated) in 
         tempf(new_step) 
   ) in 
   tempf([],direct_descendants (n,d) ) ;;      

end ;;

let og = Oslo.get ;;
let ni = Oslo.needed_intermediaries ;;


module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  ((selector,partitioned),(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition 
   (PW(old_affinities,old_parties,ll):t) parti  =
   let new_affinities =  Affinity.expand_using_partition old_affinities parti
   and new_parties = old_parties @ [parti]   in 
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_affinities,new_parties,new_ll):>t) ;;

let insert_affinity (PW(old_affinities,old_parties,ll)) affty  =
   PW(Affinity.insert affty old_affinities,old_parties,ll) ;;        
    

exception Missing_affinities of ( (int list) * colored_variable) list;;   

let check_combination_using_affinities combination (PW(affinities,_,_):t)=
  let (bad_ones,checked_version) = Affinity.analyse_combination affinities combination in 
  if bad_ones <> []
  then raise(Missing_affinities bad_ones)
  else checked_version;;

let expand_using_t combination (pw:t) =
   let (PW(_,_,ll)) = pw in 
   let ref_for_unknowns = ref [] in 
   let combination2 = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in
   let new_combination = check_combination_using_affinities combination2  pw in
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(affinities,parties,ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(affinities,parties,ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(affinities,parties,List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   (intlist_to_string lamb)^","^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW (_,_,ll)):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;

let decompose ((PW (_,_,ll)):t) cvar =
     List.assoc cvar ll ;;

let ugly_part ((PW (_,_,ll)):t) cvar =
     let temp1 = List.assoc cvar ll in 
     List.filter (fun (lamb,cv)->Colored.is_ugly cv) temp1 ;;     

end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
   "D",[List.filter (fun x->(List.length x)=5) wall];
   "E",[List.filter (fun x->(List.length x)=4) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = List.partition (Vdw_chosen.test_joinability selector)  old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    if (a=[])||(b=[])
    then (a,b,None)  
    else
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (a,b,Some(Colored.variable_from_label lbl i,
              Colored.variable_from_label lbl j)) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW(Aff[],[],[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
       (D 0),[[],D 0];
       (E 0),[[],E 0];
])) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;

let explain_partition (selector,cv,a,b,opt_cvs) =
   let s_selector = "["^(String.concat ";" (Image.image string_of_int selector))^"]" in  
   let beginning_of_msg = "\n\n"^(Colored.to_string cv)^" "
   and end_of_msg = (
      if a=[] then "is uniformly incompatible with "^s_selector else  
      if b=[] then "is uniformly compatible with "^s_selector else   
      let (cv_i,cv_j) = Option.unpack opt_cvs in       
      " splits as "^(Colored.to_string cv_i)
      ^" + "^(Colored.to_string cv_j)) in 
   let msg = beginning_of_msg ^ end_of_msg in 
   (print_string msg ;flush stdout) ;;             

let partition selector cv =
    let old_state = (!main) in 
    let (a,b,opt_cvs) = Colored_variable_assignment.partition selector cv in 
    let _ = explain_partition (selector,cv,a,b,opt_cvs) in 
    let new_state = (
    match opt_cvs with 
    None -> 
      let affty=((selector,cv),b=[]) in 
      Partition_watcher.insert_affinity old_state affty
    |Some(cv_i,cv_j) -> 
        Partition_watcher.expand_using_partition 
        old_state ((selector,cv),(cv_i,cv_j)) 
    ) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Expansion.current n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

let decompose cvar = Partition_watcher.decompose (!main) cvar;;
let ugly_part cvar = Partition_watcher.ugly_part (!main) cvar;;

end ;;  

let deco = Current_partition_watcher.decompose ;;
let oslo_compute = Current_partition_watcher.oslo_compute ;;
let up = Current_partition_watcher.ugly_part ;;

module Marshall_plan = struct 

let detect_missing_affinities n d=         
   let skeleton = Image.image (
     fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
   ) (Expansion.current n d) in 
   let old_state = (!(Current_partition_watcher.main)) in 
   let (PW(affinities,_,ll)) = old_state in 
   let combination2 = List.flatten(Image.image (fun old_pair->
      let (lamb,cvar) = old_pair in 
      match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
       (Some expansion) -> Image.image (fun
        (lamb2,cvar2)-> 
        (Ordered.merge oi lamb lamb2,cvar2)) expansion
      |None -> [old_pair]
      ) skeleton) in 
      fst(Affinity.analyse_combination affinities combination2)  
    ;;  

let  write_partition_commands_from_list l=
  let temp1 = Image.image (fun (lamb,cv)->
   "let passing = Current_partition_watcher.partition "^
   (intlist_to_string lamb)^" ("^(Colored.to_ocaml_readable_string cv)^") ;;"
   )  l in 
  "\n\n\n" ^ (String.concat "\n" temp1) ^ "\n\n\n" ;;   
  
let write_partition_commands n d =
    let missing_affs = detect_missing_affinities n d in 
    print_string(write_partition_commands_from_list missing_affs) ;;  

let rec compute_affinities_then_oslo (n,d) =
   let missing_affs = detect_missing_affinities n d in 
   if missing_affs = []
   then oslo_compute n d 
   else let _ = Image.image (
         fun (lamb,cv) -> Current_partition_watcher.partition lamb cv
        ) missing_affs in 
        compute_affinities_then_oslo (n,d) ;;

let compute_from_scratch n d =
    let temp1 = (Oslo.needed_intermediaries n d)@[n,d] in 
    let _ = Image.image compute_affinities_then_oslo temp1 in 
    Oslo.get n d;;

end ;;   

let wpc = Marshall_plan.write_partition_commands ;;

Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;
Oslo.add (15,3) (Colored_variable_assignment.get (D 0)) ;;
Oslo.add (15,4) (Colored_variable_assignment.get (E 0)) ;;



Image.image (fun n->
   Marshall_plan.compute_from_scratch n 0  
)(Ennig.ennig 16 23) ;;

(*
let g1 = Oslo.needed_intermediaries 24 0;;

let dcg i j = deco(Og(i,j));;
let upp l=List.filter(fun (x,(i,j))->(up (Og(i,j)))<>[]) l;;

let n0=21 and d0=0;;
let h1 = Expansion.current n0 d0;;
let h2 = Expansion.allowed_substitution_by_in (n0-1,d0+1) h1;;
let h3 = Expansion.allowed_substitution_by_in (n0-2,d0+1) h2;;
*)



(************************************************************************************************************************
Snippet 71 : Musings on the Vand der Waerden problem, version 12 : painfully long
computation of oslo_compute 21 0
************************************************************************************************************************)
open Needed_values ;;
type colored_variable = 
   A of int |B of int|C of int|D of int|E of int| Og of int * int ;;   
type list_of_affinities = Aff of ((int list * colored_variable) * bool)  list ;;    
type partition_watcher = 
   PW of 
   list_of_affinities *
   (((int list * colored_variable) * (colored_variable * colored_variable)) list) *
   ((colored_variable * (int list * colored_variable) list) list) ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;; 
let mea = Vdw_chosen.measure ;;
let lmea = Vdw_chosen.lower_measure ;;
let is_adm = Vdw_chosen.test_for_admissibility ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

let intlist_to_string lamb =
   "["^(String.concat ";" (Image.image string_of_int lamb))^"]" ;;

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  

module Colored = struct 

exception Label_for_variable_exn ;;
let data_for_variable = function 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) |(D k) ->("D",k) 
    |(E k) ->("E",k) 
    |(Og(_,_))->raise(Label_for_variable_exn) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     if lbl = "D" then D k else 
     if lbl = "E" then E k else   
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d [0,A 0;1, B 0;2, C 0;3, D 0;4, E 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

let to_ocaml_readable_string cv= match cv with  
   (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
   | _ -> let (lbl,k) = data_for_variable cv in 
          lbl^" "^(string_of_int k) ;; 

let order = ((fun cv1 cv2->
  let (lbl1,k1) = data_for_variable cv1 
  and (lbl2,k2) = data_for_variable cv2 in  
  let trial1 = Total_ordering.standard lbl1 lbl2 in 
  if trial1 <> Total_ordering_result_t.Equal then trial1 else 
   Total_ordering.standard k1 k2 
) : colored_variable Total_ordering_t.t) ;;

let order_for_pairs = ((fun (lamb1,cv1) (lamb2,cv2)->
   let trial1 = order cv1 cv2 in 
   if trial1 <> Total_ordering_result_t.Equal then trial1 else 
    (Total_ordering.silex_compare Total_ordering.standard ) lamb1 lamb2 
 ) : (int list * colored_variable) Total_ordering_t.t) ;;
 

end ;;  

module Affinity = struct

   let shadow (Aff l) cvar = 
      Option.filter_and_unpack (
         fun ((selector,cvar2),is_compatible) ->
           if cvar2 = cvar 
           then Some(selector,is_compatible)   
           else None
      ) l;;
  
  let expand_using_partition aff parti  =
     let (Aff(old_affinities)) = aff 
     and ((selector,partitioned),(part1,part2)) = parti in
     let temp1 = shadow aff partitioned in 
     let temp2 = List.flatten(Image.image (
        fun (selector2,is_compatible) ->
           [((selector2,part1),is_compatible);
            ((selector2,part2),is_compatible)]     
     ) temp1) in
     let new_affinities = old_affinities @ 
         [((selector,part1),true);
         ((selector,part2),false);]@temp2  in 
     Aff(new_affinities) ;;
  
  let insert affty  (Aff old_affinities)  =
     if List.mem affty old_affinities 
     then Aff old_affinities
     else Aff(affty :: old_affinities) ;;
  
  let analyse_combination (Aff affinities) combination =
     let temp0 = List.filter (
       fun (lamb,_)->Vdw_chosen.test_for_admissibility lamb
     ) combination in 
     let temp1 = Image.image (fun 
        pair -> 
         let res = (
          if fst pair = []
          then Some true
          else List.assoc_opt  pair affinities 
         ) in
         (pair,res)
     ) temp0 in 
     let filterer = (fun criterion ->
        Option.filter_and_unpack 
       (fun (pair,opt)->
         if opt=criterion then Some pair else None) temp1
     ) in 
     let bad_ones = filterer None in 
     let checked_version = filterer (Some true) in 
     (Ordered.sort Colored.order_for_pairs bad_ones,checked_version);;
   
  
  
end ;;   
  
module Expansion = struct

let subst_by_in cvar replacement_for_cvar l=
  let temp1 = List.flatten(Image.image (
     fun pair ->
      let (lamb2,cvar2) = pair in 
      if cvar2=cvar 
      then Image.image (
          fun (lamb3,cvar3)->
            (Ordered.merge oi lamb2 lamb3,cvar3)) replacement_for_cvar
      else [pair]      
  ) l) in
  List.filter (fun (lamb4,cvar4)->is_adm lamb4) temp1 ;; 

let origin n d =
   let delt = mea(n)-mea(n-1) in   
   List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
   [
     [],(n-1,d-delt);
     [n],(n-1,d+1-delt)
   ];;   

let allowed_substitution_by_in (n,d) expr = 
   subst_by_in (n,d) (origin n d) expr;;

let special1 n d =
     let temp1 = origin n d in 
     let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
     let temp3 = allowed_substitution_by_in (n-2,d+1) temp2 in
     temp3 ;;

let current n d = 
   if (n,d)=(18,1) then special1 n d else 
   origin n d ;;

end ;;   

let see = Expansion.current ;;

module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (Expansion.current n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;

let direct_descendants (n,d) =
   let temp1 = Image.image snd (Expansion.current n d) in 
   List.filter (fun (nn,dd)->
         ((Hashtbl.find_opt hashtbl_for_oslo (nn,dd)) = None)
         && (nn >= wall_position)   
   ) temp1 ;;      

exception Pusher_for_needed_intermediaries_exn ;;   

let pusher_for_needed_intermediaries (treated,to_be_treated) =
   let oh = Total_ordering.standard2 in (* oh is for "order here" *)
   match (List.rev to_be_treated) with 
     [] -> raise (Pusher_for_needed_intermediaries_exn) 
    |pair :: others -> 
      let new_ones = Ordered.sort oh (direct_descendants pair) in 
   (Ordered.insert oh pair treated,
            Ordered.merge oh new_ones others) ;;  

let needed_intermediaries n d = 
   let rec tempf = (fun (treated,to_be_treated)->
      match (List.rev to_be_treated) with 
       [] -> treated 
      |pair :: others -> 
         let new_step = pusher_for_needed_intermediaries (treated,to_be_treated) in 
         tempf(new_step) 
   ) in 
   tempf([],direct_descendants (n,d) ) ;;      

end ;;

let og = Oslo.get ;;
let ni = Oslo.needed_intermediaries ;;


module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  ((selector,partitioned),(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition 
   (PW(old_affinities,old_parties,ll):t) parti  =
   let new_affinities =  Affinity.expand_using_partition old_affinities parti
   and new_parties = old_parties @ [parti]   in 
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_affinities,new_parties,new_ll):>t) ;;

let insert_affinity (PW(old_affinities,old_parties,ll)) affty  =
   PW(Affinity.insert affty old_affinities,old_parties,ll) ;;        
    

exception Missing_affinities of ( (int list) * colored_variable) list;;   

let check_combination_using_affinities combination (PW(affinities,_,_):t)=
  let (bad_ones,checked_version) = Affinity.analyse_combination affinities combination in 
  if bad_ones <> []
  then raise(Missing_affinities bad_ones)
  else checked_version;;

let expand_using_t combination (pw:t) =
   let (PW(_,_,ll)) = pw in 
   let ref_for_unknowns = ref [] in 
   let combination2 = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in
   let new_combination = check_combination_using_affinities combination2  pw in
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(affinities,parties,ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(affinities,parties,ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(affinities,parties,List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   (intlist_to_string lamb)^","^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW (_,_,ll)):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;

let decompose ((PW (_,_,ll)):t) cvar =
     List.assoc cvar ll ;;

end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
   "D",[List.filter (fun x->(List.length x)=5) wall];
   "E",[List.filter (fun x->(List.length x)=4) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = List.partition (Vdw_chosen.test_joinability selector)  old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    if (a=[])||(b=[])
    then (a,b,None)  
    else
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (a,b,Some(Colored.variable_from_label lbl i,
              Colored.variable_from_label lbl j)) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW(Aff[],[],[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
       (D 0),[[],D 0];
       (E 0),[[],E 0];
])) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;

let explain_partition (selector,cv,a,b,opt_cvs) =
   let s_selector = "["^(String.concat ";" (Image.image string_of_int selector))^"]" in  
   let beginning_of_msg = "\n\n"^(Colored.to_string cv)^" "
   and end_of_msg = (
      if a=[] then "is uniformly incompatible with "^s_selector else  
      if b=[] then "is uniformly compatible with "^s_selector else   
      let (cv_i,cv_j) = Option.unpack opt_cvs in       
      " splits as "^(Colored.to_string cv_i)
      ^" + "^(Colored.to_string cv_j)) in 
   let msg = beginning_of_msg ^ end_of_msg in 
   (print_string msg ;flush stdout) ;;             

let partition selector cv =
    let old_state = (!main) in 
    let (a,b,opt_cvs) = Colored_variable_assignment.partition selector cv in 
    let _ = explain_partition (selector,cv,a,b,opt_cvs) in 
    let new_state = (
    match opt_cvs with 
    None -> 
      let affty=((selector,cv),b=[]) in 
      Partition_watcher.insert_affinity old_state affty
    |Some(cv_i,cv_j) -> 
        Partition_watcher.expand_using_partition 
        old_state ((selector,cv),(cv_i,cv_j)) 
    ) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Expansion.current n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

let decompose cvar = Partition_watcher.decompose (!main) cvar;;

end ;;  

let deco = Current_partition_watcher.decompose ;;
let oslo_compute = Current_partition_watcher.oslo_compute ;;


module Marshall_plan = struct 

let detect_missing_affinities n d=         
   let skeleton = Image.image (
     fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
   ) (Expansion.current n d) in 
   let old_state = (!(Current_partition_watcher.main)) in 
   let (PW(affinities,_,ll)) = old_state in 
   let combination2 = List.flatten(Image.image (fun old_pair->
      let (lamb,cvar) = old_pair in 
      match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
       (Some expansion) -> Image.image (fun
        (lamb2,cvar2)-> 
        (Ordered.merge oi lamb lamb2,cvar2)) expansion
      |None -> [old_pair]
      ) skeleton) in 
      fst(Affinity.analyse_combination affinities combination2)  
    ;;  

let  write_partition_commands_from_list l=
  let temp1 = Image.image (fun (lamb,cv)->
   "let passing = Current_partition_watcher.partition "^
   (intlist_to_string lamb)^" ("^(Colored.to_ocaml_readable_string cv)^") ;;"
   )  l in 
  "\n\n\n" ^ (String.concat "\n" temp1) ^ "\n\n\n" ;;   
  
let write_partition_commands n d =
    let missing_affs = detect_missing_affinities n d in 
    print_string(write_partition_commands_from_list missing_affs) ;;  

end ;;   

let wpc = Marshall_plan.write_partition_commands ;;

Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;
Oslo.add (15,3) (Colored_variable_assignment.get (D 0)) ;;
Oslo.add (15,4) (Colored_variable_assignment.get (E 0)) ;;


let passing = Current_partition_watcher.partition [16] (B 0) ;;
oslo_compute 16 0 ;;

let passing = Current_partition_watcher.partition [16] (C 0) ;;
oslo_compute 16 1 ;;

let passing = Current_partition_watcher.partition [17] (B 1) ;;
let passing = Current_partition_watcher.partition [17] (B 2) ;;
let passing = Current_partition_watcher.partition [16;17] (C 1) ;;

oslo_compute 17 0 ;;

let passing = Current_partition_watcher.partition [18] (A 0) ;;
let passing = Current_partition_watcher.partition [16;18] (B 3) ;;
let passing = Current_partition_watcher.partition [16;18] (B 4) ;;
let passing = Current_partition_watcher.partition [17;18] (B 5) ;;
let passing = Current_partition_watcher.partition [17;18] (B 7) ;;
let passing = Current_partition_watcher.partition [17;18] (B 8) ;;

oslo_compute 18 0 ;;

let passing = Current_partition_watcher.partition [18] (B 6) ;;
let passing = Current_partition_watcher.partition [18] (B 7) ;;
let passing = Current_partition_watcher.partition [18] (B 8) ;;
let passing = Current_partition_watcher.partition [18] (B 9) ;;
let passing = Current_partition_watcher.partition [18] (B 10) ;;
let passing = Current_partition_watcher.partition [18] (B 11) ;;
let passing = Current_partition_watcher.partition [18] (B 12) ;;
let passing = Current_partition_watcher.partition [17;18] (C 2) ;;
let passing = Current_partition_watcher.partition [16;18] (C 3) ;;
let passing = Current_partition_watcher.partition [17;18] (C 3) ;;
let passing = Current_partition_watcher.partition [16;18] (C 4) ;;
let passing = Current_partition_watcher.partition [17;18] (C 4) ;;


let passing = Current_partition_watcher.partition [17;18] (C 7) ;;
let passing = Current_partition_watcher.partition [17;18] (C 8) ;;
let passing = Current_partition_watcher.partition [17;18] (C 11) ;;
let passing = Current_partition_watcher.partition [17;18] (C 12) ;;

oslo_compute 18 1;;

let passing = Current_partition_watcher.partition [19] (A 0) ;;
let passing = Current_partition_watcher.partition [16;19] (B 7) ;;
let passing = Current_partition_watcher.partition [17;19] (B 7) ;;
let passing = Current_partition_watcher.partition [18;19] (B 7) ;;
let passing = Current_partition_watcher.partition [16;19] (B 8) ;;
let passing = Current_partition_watcher.partition [17;19] (B 8) ;;
let passing = Current_partition_watcher.partition [16;19] (B 9) ;;
let passing = Current_partition_watcher.partition [18;19] (B 9) ;;
let passing = Current_partition_watcher.partition [17;19] (B 11) ;;
let passing = Current_partition_watcher.partition [18;19] (B 11) ;;
let passing = Current_partition_watcher.partition [17;19] (B 12) ;;
let passing = Current_partition_watcher.partition [18;19] (B 13) ;;
let passing = Current_partition_watcher.partition [16;19] (B 15) ;;
let passing = Current_partition_watcher.partition [18;19] (B 15) ;;
let passing = Current_partition_watcher.partition [16;19] (B 16) ;;
let passing = Current_partition_watcher.partition [16;17;19] (C 7) ;;
let passing = Current_partition_watcher.partition [16;18;19] (C 7) ;;
let passing = Current_partition_watcher.partition [16;17;19] (C 15) ;;
let passing = Current_partition_watcher.partition [16;17;19] (C 16) ;;
let passing = Current_partition_watcher.partition [16;18;19] (C 17) ;;
let passing = Current_partition_watcher.partition [16;18;19] (C 18) ;;

let passing = Current_partition_watcher.partition [17;19] (B 17) ;;
let passing = Current_partition_watcher.partition [17;19] (B 18) ;;
let passing = Current_partition_watcher.partition [18;19] (B 19) ;;
let passing = Current_partition_watcher.partition [18;19] (B 20) ;;
let passing = Current_partition_watcher.partition [16;18;19] (C 19) ;;
let passing = Current_partition_watcher.partition [16;18;19] (C 20) ;;

oslo_compute 19 0 ;;

let passing = Current_partition_watcher.partition [19;20] (A 1) ;;
let passing = Current_partition_watcher.partition [17;18;20] (B 7) ;;
let passing = Current_partition_watcher.partition [16;19;20] (B 17) ;;
let passing = Current_partition_watcher.partition [17;18;20] (B 19) ;;
let passing = Current_partition_watcher.partition [17;19;20] (B 19) ;;
let passing = Current_partition_watcher.partition [17;18;20] (B 20) ;;
let passing = Current_partition_watcher.partition [17;19;20] (B 21) ;;
let passing = Current_partition_watcher.partition [16;19;20] (B 25) ;;
let passing = Current_partition_watcher.partition [16;17;19;20] (C 16) ;;
let passing = Current_partition_watcher.partition [16;17;19;20] (C 19) ;;
let passing = Current_partition_watcher.partition [16;17;19;20] (C 23) ;;

oslo_compute 20 0 ;;

(* Erasable part begins here *)

let passing = Current_partition_watcher.partition [16] (D 0) ;;
oslo_compute 16 2 ;;
let passing = Current_partition_watcher.partition [16] (E 0) ;;
oslo_compute 16 3 ;;

let passing = Current_partition_watcher.partition [17] (C 5) ;;
let passing = Current_partition_watcher.partition [17] (C 6) ;;
let passing = Current_partition_watcher.partition [17] (C 12) ;;
let passing = Current_partition_watcher.partition [17] (C 16) ;;
let passing = Current_partition_watcher.partition [17] (C 18) ;;
let passing = Current_partition_watcher.partition [17] (C 20) ;;
let passing = Current_partition_watcher.partition [17] (C 23) ;;
let passing = Current_partition_watcher.partition [17] (C 24) ;;
let passing = Current_partition_watcher.partition [17] (C 25) ;;
let passing = Current_partition_watcher.partition [17] (C 26) ;;
let passing = Current_partition_watcher.partition [17] (C 27) ;;
let passing = Current_partition_watcher.partition [17] (C 28) ;;
let passing = Current_partition_watcher.partition [16;17] (D 1) ;;
oslo_compute 17 1 ;;

let passing = Current_partition_watcher.partition [17] (D 2) ;;
let passing = Current_partition_watcher.partition [17] (D 3) ;;
let passing = Current_partition_watcher.partition [17] (D 4) ;;
let passing = Current_partition_watcher.partition [16;17] (E 1) ;;
oslo_compute 17 2 ;;

let passing = Current_partition_watcher.partition [18] (C 5) ;;
let passing = Current_partition_watcher.partition [18] (C 16) ;;
let passing = Current_partition_watcher.partition [18] (C 18) ;;
let passing = Current_partition_watcher.partition [18] (C 20) ;;
let passing = Current_partition_watcher.partition [18] (C 23) ;;
let passing = Current_partition_watcher.partition [18] (C 24) ;;
let passing = Current_partition_watcher.partition [18] (C 25) ;;
let passing = Current_partition_watcher.partition [18] (C 26) ;;
let passing = Current_partition_watcher.partition [18] (C 27) ;;
let passing = Current_partition_watcher.partition [18] (C 28) ;;
let passing = Current_partition_watcher.partition [18] (C 29) ;;
let passing = Current_partition_watcher.partition [18] (C 30) ;;
let passing = Current_partition_watcher.partition [18] (C 31) ;;
let passing = Current_partition_watcher.partition [18] (C 32) ;;
let passing = Current_partition_watcher.partition [16;18] (D 3) ;;
let passing = Current_partition_watcher.partition [17;18] (D 3) ;;
let passing = Current_partition_watcher.partition [17;18] (D 5) ;;
let passing = Current_partition_watcher.partition [16;18] (D 7) ;;
let passing = Current_partition_watcher.partition [17;18] (D 7) ;;
let passing = Current_partition_watcher.partition [16;18] (D 8) ;;
let passing = Current_partition_watcher.partition [17;18] (D 9) ;;
let passing = Current_partition_watcher.partition [17;18] (D 10) ;;
let passing = Current_partition_watcher.partition [17;18] (D 13) ;;
let passing = Current_partition_watcher.partition [17;18] (D 14) ;;
let passing = Current_partition_watcher.partition [17;18] (D 15) ;;
let passing = Current_partition_watcher.partition [17;18] (D 16) ;;
oslo_compute 18 2 ;;


let passing = Current_partition_watcher.partition [19] (B 7) ;;
let passing = Current_partition_watcher.partition [19] (B 9) ;;
let passing = Current_partition_watcher.partition [19] (B 14) ;;
let passing = Current_partition_watcher.partition [19] (B 15) ;;
let passing = Current_partition_watcher.partition [19] (B 17) ;;
let passing = Current_partition_watcher.partition [19] (B 18) ;;
let passing = Current_partition_watcher.partition [19] (B 19) ;;
let passing = Current_partition_watcher.partition [19] (B 20) ;;
let passing = Current_partition_watcher.partition [19] (B 21) ;;
let passing = Current_partition_watcher.partition [19] (B 22) ;;
let passing = Current_partition_watcher.partition [19] (B 23) ;;
let passing = Current_partition_watcher.partition [19] (B 24) ;;
let passing = Current_partition_watcher.partition [19] (B 25) ;;
let passing = Current_partition_watcher.partition [19] (B 26) ;;
let passing = Current_partition_watcher.partition [17;19] (C 5) ;;
let passing = Current_partition_watcher.partition [18;19] (C 5) ;;
let passing = Current_partition_watcher.partition [16;19] (C 16) ;;
let passing = Current_partition_watcher.partition [17;19] (C 16) ;;
let passing = Current_partition_watcher.partition [16;19] (C 18) ;;
let passing = Current_partition_watcher.partition [18;19] (C 18) ;;
let passing = Current_partition_watcher.partition [16;19] (C 20) ;;
let passing = Current_partition_watcher.partition [17;19] (C 20) ;;
let passing = Current_partition_watcher.partition [18;19] (C 20) ;;
let passing = Current_partition_watcher.partition [16;19] (C 23) ;;
let passing = Current_partition_watcher.partition [17;19] (C 23) ;;
let passing = Current_partition_watcher.partition [18;19] (C 23) ;;
let passing = Current_partition_watcher.partition [16;19] (C 24) ;;
let passing = Current_partition_watcher.partition [17;19] (C 24) ;;
let passing = Current_partition_watcher.partition [18;19] (C 24) ;;
let passing = Current_partition_watcher.partition [16;19] (C 25) ;;
let passing = Current_partition_watcher.partition [17;19] (C 25) ;;
let passing = Current_partition_watcher.partition [18;19] (C 25) ;;
let passing = Current_partition_watcher.partition [16;19] (C 26) ;;
let passing = Current_partition_watcher.partition [17;19] (C 26) ;;
let passing = Current_partition_watcher.partition [18;19] (C 26) ;;
let passing = Current_partition_watcher.partition [16;19] (C 27) ;;
let passing = Current_partition_watcher.partition [17;19] (C 27) ;;
let passing = Current_partition_watcher.partition [18;19] (C 27) ;;
let passing = Current_partition_watcher.partition [16;19] (C 28) ;;
let passing = Current_partition_watcher.partition [17;19] (C 28) ;;
let passing = Current_partition_watcher.partition [18;19] (C 28) ;;
let passing = Current_partition_watcher.partition [17;19] (C 29) ;;
let passing = Current_partition_watcher.partition [16;19] (C 31) ;;
let passing = Current_partition_watcher.partition [17;19] (C 31) ;;
let passing = Current_partition_watcher.partition [18;19] (C 33) ;;
let passing = Current_partition_watcher.partition [16;19] (C 35) ;;
let passing = Current_partition_watcher.partition [18;19] (C 35) ;;
let passing = Current_partition_watcher.partition [16;19] (C 36) ;;
let passing = Current_partition_watcher.partition [18;19] (C 37) ;;
let passing = Current_partition_watcher.partition [18;19] (C 38) ;;
let passing = Current_partition_watcher.partition [17;19] (C 41) ;;
let passing = Current_partition_watcher.partition [17;19] (C 42) ;;
let passing = Current_partition_watcher.partition [18;19] (C 45) ;;
let passing = Current_partition_watcher.partition [18;19] (C 46) ;;

let passing = Current_partition_watcher.partition [16;17;19] (D 9) ;;
let passing = Current_partition_watcher.partition [16;18;19] (D 9) ;;
let passing = Current_partition_watcher.partition [16;18;19] (D 13) ;;
let passing = Current_partition_watcher.partition [16;18;19] (D 15) ;;
let passing = Current_partition_watcher.partition [16;18;19] (D 17) ;;
let passing = Current_partition_watcher.partition [16;17;19] (D 19) ;;
let passing = Current_partition_watcher.partition [16;18;19] (D 19) ;;
let passing = Current_partition_watcher.partition [16;17;19] (D 20) ;;
let passing = Current_partition_watcher.partition [16;17;19] (D 21) ;;
let passing = Current_partition_watcher.partition [16;17;19] (D 22) ;;
let passing = Current_partition_watcher.partition [16;18;19] (D 23) ;;
let passing = Current_partition_watcher.partition [16;18;19] (D 24) ;;

oslo_compute 19 1 ;;

let passing = Current_partition_watcher.partition [20] (A 1) ;;
let passing = Current_partition_watcher.partition [20] (A 2) ;;
let passing = Current_partition_watcher.partition [16;20] (B 7) ;;
let passing = Current_partition_watcher.partition [17;20] (B 7) ;;
let passing = Current_partition_watcher.partition [18;20] (B 7) ;;
let passing = Current_partition_watcher.partition [16;20] (B 9) ;;
let passing = Current_partition_watcher.partition [18;20] (B 9) ;;
let passing = Current_partition_watcher.partition [19;20] (B 9) ;;
let passing = Current_partition_watcher.partition [16;20] (B 15) ;;
let passing = Current_partition_watcher.partition [18;20] (B 15) ;;
let passing = Current_partition_watcher.partition [19;20] (B 15) ;;
let passing = Current_partition_watcher.partition [16;20] (B 17) ;;
let passing = Current_partition_watcher.partition [17;20] (B 17) ;;
let passing = Current_partition_watcher.partition [19;20] (B 17) ;;
let passing = Current_partition_watcher.partition [16;20] (B 18) ;;
let passing = Current_partition_watcher.partition [17;20] (B 18) ;;
let passing = Current_partition_watcher.partition [17;20] (B 19) ;;
let passing = Current_partition_watcher.partition [18;20] (B 19) ;;
let passing = Current_partition_watcher.partition [19;20] (B 19) ;;
let passing = Current_partition_watcher.partition [17;20] (B 20) ;;
let passing = Current_partition_watcher.partition [18;20] (B 20) ;;
let passing = Current_partition_watcher.partition [19;20] (B 20) ;;
let passing = Current_partition_watcher.partition [17;20] (B 21) ;;
let passing = Current_partition_watcher.partition [19;20] (B 21) ;;
let passing = Current_partition_watcher.partition [18;20] (B 23) ;;
let passing = Current_partition_watcher.partition [19;20] (B 23) ;;
let passing = Current_partition_watcher.partition [18;20] (B 24) ;;
let passing = Current_partition_watcher.partition [16;20] (B 25) ;;
let passing = Current_partition_watcher.partition [19;20] (B 25) ;;
let passing = Current_partition_watcher.partition [16;20] (B 26) ;;
let passing = Current_partition_watcher.partition [19;20] (B 26) ;;
let passing = Current_partition_watcher.partition [19;20] (B 27) ;;
let passing = Current_partition_watcher.partition [17;20] (B 29) ;;
let passing = Current_partition_watcher.partition [19;20] (B 29) ;;
let passing = Current_partition_watcher.partition [17;20] (B 30) ;;
let passing = Current_partition_watcher.partition [16;17;20] (C 16) ;;
let passing = Current_partition_watcher.partition [16;19;20] (C 16) ;;
let passing = Current_partition_watcher.partition [17;19;20] (C 16) ;;
let passing = Current_partition_watcher.partition [16;17;20] (C 20) ;;
let passing = Current_partition_watcher.partition [17;18;20] (C 20) ;;
let passing = Current_partition_watcher.partition [17;19;20] (C 20) ;;
let passing = Current_partition_watcher.partition [16;17;20] (C 23) ;;
let passing = Current_partition_watcher.partition [16;19;20] (C 23) ;;
let passing = Current_partition_watcher.partition [17;18;20] (C 23) ;;
let passing = Current_partition_watcher.partition [17;19;20] (C 23) ;;
let passing = Current_partition_watcher.partition [16;17;20] (C 24) ;;
let passing = Current_partition_watcher.partition [17;18;20] (C 24) ;;
let passing = Current_partition_watcher.partition [17;19;20] (C 24) ;;
let passing = Current_partition_watcher.partition [16;19;20] (C 25) ;;
let passing = Current_partition_watcher.partition [17;18;20] (C 25) ;;
let passing = Current_partition_watcher.partition [17;18;20] (C 26) ;;
let passing = Current_partition_watcher.partition [16;17;20] (C 27) ;;
let passing = Current_partition_watcher.partition [16;19;20] (C 27) ;;
let passing = Current_partition_watcher.partition [17;18;20] (C 27) ;;
let passing = Current_partition_watcher.partition [17;19;20] (C 27) ;;
let passing = Current_partition_watcher.partition [16;17;20] (C 28) ;;
let passing = Current_partition_watcher.partition [16;19;20] (C 28) ;;
let passing = Current_partition_watcher.partition [17;18;20] (C 28) ;;
let passing = Current_partition_watcher.partition [17;19;20] (C 28) ;;
let passing = Current_partition_watcher.partition [17;18;20] (C 37) ;;
let passing = Current_partition_watcher.partition [17;19;20] (C 37) ;;
let passing = Current_partition_watcher.partition [17;18;20] (C 38) ;;
let passing = Current_partition_watcher.partition [17;19;20] (C 39) ;;
let passing = Current_partition_watcher.partition [16;19;20] (C 41) ;;
let passing = Current_partition_watcher.partition [16;19;20] (C 45) ;;
let passing = Current_partition_watcher.partition [16;19;20] (C 47) ;;
let passing = Current_partition_watcher.partition [16;17;19;20] (D 22) ;;
let passing = Current_partition_watcher.partition [16;17;19;20] (D 23) ;;
let passing = Current_partition_watcher.partition [16;17;19;20] (D 31) ;;

let passing = Current_partition_watcher.partition [17;18;20] (C 49) ;;
let passing = Current_partition_watcher.partition [17;19;20] (C 49) ;;
let passing = Current_partition_watcher.partition [17;18;20] (C 50) ;;
let passing = Current_partition_watcher.partition [17;19;20] (C 50) ;;
let passing = Current_partition_watcher.partition [17;19;20] (C 51) ;;
let passing = Current_partition_watcher.partition [17;19;20] (C 52) ;;

oslo_compute 20 1 ;;

let passing = Current_partition_watcher.partition [19;21] (A 1) ;;
let passing = Current_partition_watcher.partition [20;21] (A 1) ;;
let passing = Current_partition_watcher.partition [20;21] (A 2) ;;
let passing = Current_partition_watcher.partition [16;18;21] (B 7) ;;
let passing = Current_partition_watcher.partition [16;20;21] (B 7) ;;
let passing = Current_partition_watcher.partition [17;18;21] (B 7) ;;
let passing = Current_partition_watcher.partition [17;20;21] (B 7) ;;
let passing = Current_partition_watcher.partition [18;20;21] (B 7) ;;
let passing = Current_partition_watcher.partition [16;18;21] (B 9) ;;
let passing = Current_partition_watcher.partition [16;20;21] (B 9) ;;
let passing = Current_partition_watcher.partition [18;19;21] (B 9) ;;
let passing = Current_partition_watcher.partition [18;20;21] (B 9) ;;
let passing = Current_partition_watcher.partition [16;20;21] (B 15) ;;
let passing = Current_partition_watcher.partition [18;19;21] (B 15) ;;
let passing = Current_partition_watcher.partition [18;20;21] (B 15) ;;
let passing = Current_partition_watcher.partition [16;19;21] (B 17) ;;
let passing = Current_partition_watcher.partition [17;20;21] (B 17) ;;
let passing = Current_partition_watcher.partition [17;20;21] (B 18) ;;
let passing = Current_partition_watcher.partition [17;18;21] (B 19) ;;
let passing = Current_partition_watcher.partition [17;20;21] (B 19) ;;
let passing = Current_partition_watcher.partition [18;19;21] (B 19) ;;
let passing = Current_partition_watcher.partition [18;20;21] (B 19) ;;
let passing = Current_partition_watcher.partition [17;18;21] (B 20) ;;
let passing = Current_partition_watcher.partition [18;19;21] (B 20) ;;
let passing = Current_partition_watcher.partition [18;20;21] (B 20) ;;
let passing = Current_partition_watcher.partition [18;19;21] (B 23) ;;
let passing = Current_partition_watcher.partition [18;20;21] (B 23) ;;
let passing = Current_partition_watcher.partition [18;20;21] (B 24) ;;
let passing = Current_partition_watcher.partition [16;19;21] (B 25) ;;
let passing = Current_partition_watcher.partition [16;20;21] (B 25) ;;
let passing = Current_partition_watcher.partition [17;20;21] (B 30) ;;
let passing = Current_partition_watcher.partition [16;18;19;21] (C 25) ;;
let passing = Current_partition_watcher.partition [17;18;20;21] (C 25) ;;
let passing = Current_partition_watcher.partition [17;18;20;21] (C 26) ;;
let passing = Current_partition_watcher.partition [16;17;20;21] (C 27) ;;
let passing = Current_partition_watcher.partition [16;18;19;21] (C 27) ;;
let passing = Current_partition_watcher.partition [17;18;20;21] (C 27) ;;
let passing = Current_partition_watcher.partition [16;18;19;21] (C 28) ;;
let passing = Current_partition_watcher.partition [17;18;20;21] (C 28) ;;
let passing = Current_partition_watcher.partition [16;17;20;21] (C 49) ;;
let passing = Current_partition_watcher.partition [17;18;20;21] (C 49) ;;
let passing = Current_partition_watcher.partition [17;18;20;21] (C 50) ;;
let passing = Current_partition_watcher.partition [17;18;20;21] (C 51) ;;

let passing = Current_partition_watcher.partition [18;20;21] (B 31) ;;
let passing = Current_partition_watcher.partition [18;20;21] (B 32) ;;

oslo_compute 21 0 ;;

(* Erasable part ends here *)



(*
let z1 = Marshall_plan.write_partition_commands 18 1 ;;

let is_a_d =function (D k)->true |_->false ;;
let z1 = (!(Current_partition_watcher.main)) ;;
let (PW(_,_,z2)) = z1 ;;
let z3 = List.filter (fun (x,y)->
   List.exists (fun (a,b)->is_a_d b) y) z2 ;;
*)

(************************************************************************************************************************
Snippet 70 : Musings on the Van der Waerden problem, version 11 : add the Expansion
submodule and use it to avoid the D variant in the computation of
oslo_compute 18 1, and E variant and compute up to oslo_compute 20 0
************************************************************************************************************************)
open Needed_values ;;
type colored_variable = 
   A of int |B of int|C of int|D of int|E of int| Og of int * int ;;   
type list_of_affinities = Aff of ((int list * colored_variable) * bool)  list ;;    
type partition_watcher = 
   PW of 
   list_of_affinities *
   (((int list * colored_variable) * (colored_variable * colored_variable)) list) *
   ((colored_variable * (int list * colored_variable) list) list) ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;; 
let mea = Vdw_chosen.measure ;;
let lmea = Vdw_chosen.lower_measure ;;
let is_adm = Vdw_chosen.test_for_admissibility ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

let intlist_to_string lamb =
   "["^(String.concat ";" (Image.image string_of_int lamb))^"]" ;;

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  

module Colored = struct 

exception Label_for_variable_exn ;;
let data_for_variable = function 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) |(D k) ->("D",k) 
    |(E k) ->("E",k) 
    |(Og(_,_))->raise(Label_for_variable_exn) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     if lbl = "D" then D k else 
     if lbl = "E" then E k else   
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d [0,A 0;1, B 0;2, C 0;3, D 0;4, E 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

let to_ocaml_readable_string cv= match cv with  
   (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
   | _ -> let (lbl,k) = data_for_variable cv in 
          lbl^" "^(string_of_int k) ;; 

let order = ((fun cv1 cv2->
  let (lbl1,k1) = data_for_variable cv1 
  and (lbl2,k2) = data_for_variable cv2 in  
  let trial1 = Total_ordering.standard lbl1 lbl2 in 
  if trial1 <> Total_ordering_result_t.Equal then trial1 else 
   Total_ordering.standard k1 k2 
) : colored_variable Total_ordering_t.t) ;;

let order_for_pairs = ((fun (lamb1,cv1) (lamb2,cv2)->
   let trial1 = order cv1 cv2 in 
   if trial1 <> Total_ordering_result_t.Equal then trial1 else 
    (Total_ordering.silex_compare Total_ordering.standard ) lamb1 lamb2 
 ) : (int list * colored_variable) Total_ordering_t.t) ;;
 

end ;;  

module Affinity = struct

   let shadow (Aff l) cvar = 
      Option.filter_and_unpack (
         fun ((selector,cvar2),is_compatible) ->
           if cvar2 = cvar 
           then Some(selector,is_compatible)   
           else None
      ) l;;
  
  let expand_using_partition aff parti  =
     let (Aff(old_affinities)) = aff 
     and ((selector,partitioned),(part1,part2)) = parti in
     let temp1 = shadow aff partitioned in 
     let temp2 = List.flatten(Image.image (
        fun (selector2,is_compatible) ->
           [((selector2,part1),is_compatible);
            ((selector2,part2),is_compatible)]     
     ) temp1) in
     let new_affinities = old_affinities @ 
         [((selector,part1),true);
         ((selector,part2),false);]@temp2  in 
     Aff(new_affinities) ;;
  
  let insert affty  (Aff old_affinities)  =
     if List.mem affty old_affinities 
     then Aff old_affinities
     else Aff(affty :: old_affinities) ;;
  
  let analyse_combination (Aff affinities) combination =
     let temp0 = List.filter (
       fun (lamb,_)->Vdw_chosen.test_for_admissibility lamb
     ) combination in 
     let temp1 = Image.image (fun 
        pair -> 
         let res = (
          if fst pair = []
          then Some true
          else List.assoc_opt  pair affinities 
         ) in
         (pair,res)
     ) temp0 in 
     let filterer = (fun criterion ->
        Option.filter_and_unpack 
       (fun (pair,opt)->
         if opt=criterion then Some pair else None) temp1
     ) in 
     let bad_ones = filterer None in 
     let checked_version = filterer (Some true) in 
     (Ordered.sort Colored.order_for_pairs bad_ones,checked_version);;
   
  
  
end ;;   
  
module Expansion = struct

let subst_by_in cvar replacement_for_cvar l=
  let temp1 = List.flatten(Image.image (
     fun pair ->
      let (lamb2,cvar2) = pair in 
      if cvar2=cvar 
      then Image.image (
          fun (lamb3,cvar3)->
            (Ordered.merge oi lamb2 lamb3,cvar3)) replacement_for_cvar
      else [pair]      
  ) l) in
  List.filter (fun (lamb4,cvar4)->is_adm lamb4) temp1 ;; 

let origin n d =
   let delt = mea(n)-mea(n-1) in   
   List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
   [
     [],(n-1,d-delt);
     [n],(n-1,d+1-delt)
   ];;   

let allowed_substitution_by_in (n,d) expr = 
   subst_by_in (n,d) (origin n d) expr;;

let special1 n d =
     let temp1 = origin n d in 
     let temp2 = allowed_substitution_by_in (n-1,d) temp1 in 
     let temp3 = allowed_substitution_by_in (n-2,d+1) temp2 in
     temp3 ;;

let current n d = 
   if (n,d)=(18,1) then special1 n d else 
   origin n d ;;

end ;;   

let see = Expansion.current ;;

module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (Expansion.current n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;

let direct_descendants (n,d) =
   let temp1 = Image.image snd (Expansion.current n d) in 
   List.filter (fun (nn,dd)->
         ((Hashtbl.find_opt hashtbl_for_oslo (nn,dd)) = None)
         && (nn >= wall_position)   
   ) temp1 ;;      

exception Pusher_for_needed_intermediaries_exn ;;   

let pusher_for_needed_intermediaries (treated,to_be_treated) =
   let oh = Total_ordering.standard2 in (* oh is for "order here" *)
   match (List.rev to_be_treated) with 
     [] -> raise (Pusher_for_needed_intermediaries_exn) 
    |pair :: others -> 
      let new_ones = Ordered.sort oh (direct_descendants pair) in 
   (Ordered.insert oh pair treated,
            Ordered.merge oh new_ones others) ;;  

let needed_intermediaries n d = 
   let rec tempf = (fun (treated,to_be_treated)->
      match (List.rev to_be_treated) with 
       [] -> treated 
      |pair :: others -> 
         let new_step = pusher_for_needed_intermediaries (treated,to_be_treated) in 
         tempf(new_step) 
   ) in 
   tempf([],direct_descendants (n,d) ) ;;      

end ;;

let og = Oslo.get ;;
let ni = Oslo.needed_intermediaries ;;


module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  ((selector,partitioned),(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition 
   (PW(old_affinities,old_parties,ll):t) parti  =
   let new_affinities =  Affinity.expand_using_partition old_affinities parti
   and new_parties = old_parties @ [parti]   in 
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_affinities,new_parties,new_ll):>t) ;;

let insert_affinity (PW(old_affinities,old_parties,ll)) affty  =
   PW(Affinity.insert affty old_affinities,old_parties,ll) ;;        
    

exception Missing_affinities of ( (int list) * colored_variable) list;;   

let check_combination_using_affinities combination (PW(affinities,_,_):t)=
  let (bad_ones,checked_version) = Affinity.analyse_combination affinities combination in 
  if bad_ones <> []
  then raise(Missing_affinities bad_ones)
  else checked_version;;

let expand_using_t combination (pw:t) =
   let (PW(_,_,ll)) = pw in 
   let ref_for_unknowns = ref [] in 
   let combination2 = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in
   let new_combination = check_combination_using_affinities combination2  pw in
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(affinities,parties,ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(affinities,parties,ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(affinities,parties,List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   (intlist_to_string lamb)^","^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW (_,_,ll)):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;

let decompose ((PW (_,_,ll)):t) cvar =
     List.assoc cvar ll ;;

end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
   "D",[List.filter (fun x->(List.length x)=5) wall];
   "E",[List.filter (fun x->(List.length x)=4) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = List.partition (Vdw_chosen.test_joinability selector)  old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    if (a=[])||(b=[])
    then (a,b,None)  
    else
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (a,b,Some(Colored.variable_from_label lbl i,
              Colored.variable_from_label lbl j)) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW(Aff[],[],[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
       (D 0),[[],D 0];
       (E 0),[[],E 0];
])) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;

let explain_partition (selector,cv,a,b,opt_cvs) =
   let s_selector = "["^(String.concat ";" (Image.image string_of_int selector))^"]" in  
   let beginning_of_msg = "\n\n"^(Colored.to_string cv)^" "
   and end_of_msg = (
      if a=[] then "is uniformly incompatible with "^s_selector else  
      if b=[] then "is uniformly compatible with "^s_selector else   
      let (cv_i,cv_j) = Option.unpack opt_cvs in       
      " splits as "^(Colored.to_string cv_i)
      ^" + "^(Colored.to_string cv_j)) in 
   let msg = beginning_of_msg ^ end_of_msg in 
   (print_string msg ;flush stdout) ;;             

let partition selector cv =
    let old_state = (!main) in 
    let (a,b,opt_cvs) = Colored_variable_assignment.partition selector cv in 
    let _ = explain_partition (selector,cv,a,b,opt_cvs) in 
    let new_state = (
    match opt_cvs with 
    None -> 
      let affty=((selector,cv),b=[]) in 
      Partition_watcher.insert_affinity old_state affty
    |Some(cv_i,cv_j) -> 
        Partition_watcher.expand_using_partition 
        old_state ((selector,cv),(cv_i,cv_j)) 
    ) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Expansion.current n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

let decompose cvar = Partition_watcher.decompose (!main) cvar;;

end ;;  

let deco = Current_partition_watcher.decompose ;;
let oslo_compute = Current_partition_watcher.oslo_compute ;;


module Marshall_plan = struct 

let detect_missing_affinities n d=         
   let skeleton = Image.image (
     fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
   ) (Expansion.current n d) in 
   let old_state = (!(Current_partition_watcher.main)) in 
   let (PW(affinities,_,ll)) = old_state in 
   let combination2 = List.flatten(Image.image (fun old_pair->
      let (lamb,cvar) = old_pair in 
      match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
       (Some expansion) -> Image.image (fun
        (lamb2,cvar2)-> 
        (Ordered.merge oi lamb lamb2,cvar2)) expansion
      |None -> [old_pair]
      ) skeleton) in 
      fst(Affinity.analyse_combination affinities combination2)  
    ;;  

let  write_partition_commands_from_list l=
  let temp1 = Image.image (fun (lamb,cv)->
   "let passing = Current_partition_watcher.partition "^
   (intlist_to_string lamb)^" ("^(Colored.to_ocaml_readable_string cv)^") ;;"
   )  l in 
  "\n\n\n" ^ (String.concat "\n" temp1) ^ "\n\n\n" ;;   
  
let write_partition_commands n d =
    let missing_affs = detect_missing_affinities n d in 
    print_string(write_partition_commands_from_list missing_affs) ;;  

end ;;   

let wpc = Marshall_plan.write_partition_commands ;;

Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;
Oslo.add (15,3) (Colored_variable_assignment.get (D 0)) ;;
Oslo.add (15,4) (Colored_variable_assignment.get (E 0)) ;;


let passing = Current_partition_watcher.partition [16] (B 0) ;;
oslo_compute 16 0 ;;

let passing = Current_partition_watcher.partition [16] (C 0) ;;
oslo_compute 16 1 ;;

let passing = Current_partition_watcher.partition [17] (B 1) ;;
let passing = Current_partition_watcher.partition [17] (B 2) ;;
let passing = Current_partition_watcher.partition [16;17] (C 1) ;;

oslo_compute 17 0 ;;

let passing = Current_partition_watcher.partition [18] (A 0) ;;
let passing = Current_partition_watcher.partition [16;18] (B 3) ;;
let passing = Current_partition_watcher.partition [16;18] (B 4) ;;
let passing = Current_partition_watcher.partition [17;18] (B 5) ;;
let passing = Current_partition_watcher.partition [17;18] (B 7) ;;
let passing = Current_partition_watcher.partition [17;18] (B 8) ;;

oslo_compute 18 0 ;;

let passing = Current_partition_watcher.partition [18] (B 6) ;;
let passing = Current_partition_watcher.partition [18] (B 7) ;;
let passing = Current_partition_watcher.partition [18] (B 8) ;;
let passing = Current_partition_watcher.partition [18] (B 9) ;;
let passing = Current_partition_watcher.partition [18] (B 10) ;;
let passing = Current_partition_watcher.partition [18] (B 11) ;;
let passing = Current_partition_watcher.partition [18] (B 12) ;;
let passing = Current_partition_watcher.partition [17;18] (C 2) ;;
let passing = Current_partition_watcher.partition [16;18] (C 3) ;;
let passing = Current_partition_watcher.partition [17;18] (C 3) ;;
let passing = Current_partition_watcher.partition [16;18] (C 4) ;;
let passing = Current_partition_watcher.partition [17;18] (C 4) ;;


let passing = Current_partition_watcher.partition [17;18] (C 7) ;;
let passing = Current_partition_watcher.partition [17;18] (C 8) ;;
let passing = Current_partition_watcher.partition [17;18] (C 11) ;;
let passing = Current_partition_watcher.partition [17;18] (C 12) ;;

oslo_compute 18 1;;

let passing = Current_partition_watcher.partition [19] (A 0) ;;
let passing = Current_partition_watcher.partition [16;19] (B 7) ;;
let passing = Current_partition_watcher.partition [17;19] (B 7) ;;
let passing = Current_partition_watcher.partition [18;19] (B 7) ;;
let passing = Current_partition_watcher.partition [16;19] (B 8) ;;
let passing = Current_partition_watcher.partition [17;19] (B 8) ;;
let passing = Current_partition_watcher.partition [16;19] (B 9) ;;
let passing = Current_partition_watcher.partition [18;19] (B 9) ;;
let passing = Current_partition_watcher.partition [17;19] (B 11) ;;
let passing = Current_partition_watcher.partition [18;19] (B 11) ;;
let passing = Current_partition_watcher.partition [17;19] (B 12) ;;
let passing = Current_partition_watcher.partition [18;19] (B 13) ;;
let passing = Current_partition_watcher.partition [16;19] (B 15) ;;
let passing = Current_partition_watcher.partition [18;19] (B 15) ;;
let passing = Current_partition_watcher.partition [16;19] (B 16) ;;
let passing = Current_partition_watcher.partition [16;17;19] (C 7) ;;
let passing = Current_partition_watcher.partition [16;18;19] (C 7) ;;
let passing = Current_partition_watcher.partition [16;17;19] (C 15) ;;
let passing = Current_partition_watcher.partition [16;17;19] (C 16) ;;
let passing = Current_partition_watcher.partition [16;18;19] (C 17) ;;
let passing = Current_partition_watcher.partition [16;18;19] (C 18) ;;

let passing = Current_partition_watcher.partition [17;19] (B 17) ;;
let passing = Current_partition_watcher.partition [17;19] (B 18) ;;
let passing = Current_partition_watcher.partition [18;19] (B 19) ;;
let passing = Current_partition_watcher.partition [18;19] (B 20) ;;
let passing = Current_partition_watcher.partition [16;18;19] (C 19) ;;
let passing = Current_partition_watcher.partition [16;18;19] (C 20) ;;

oslo_compute 19 0 ;;

let passing = Current_partition_watcher.partition [19;20] (A 1) ;;
let passing = Current_partition_watcher.partition [17;18;20] (B 7) ;;
let passing = Current_partition_watcher.partition [16;19;20] (B 17) ;;
let passing = Current_partition_watcher.partition [17;18;20] (B 19) ;;
let passing = Current_partition_watcher.partition [17;19;20] (B 19) ;;
let passing = Current_partition_watcher.partition [17;18;20] (B 20) ;;
let passing = Current_partition_watcher.partition [17;19;20] (B 21) ;;
let passing = Current_partition_watcher.partition [16;19;20] (B 25) ;;
let passing = Current_partition_watcher.partition [16;17;19;20] (C 16) ;;
let passing = Current_partition_watcher.partition [16;17;19;20] (C 19) ;;
let passing = Current_partition_watcher.partition [16;17;19;20] (C 23) ;;

oslo_compute 20 0 ;;

(* Erasable part begins here *)

oslo_compute 16 2 ;;

(* Erasable part ends here *)

(*
let z1 = Marshall_plan.write_partition_commands 18 1 ;;

let is_a_d =function (D k)->true |_->false ;;
let z1 = (!(Current_partition_watcher.main)) ;;
let (PW(_,_,z2)) = z1 ;;
let z3 = List.filter (fun (x,y)->
   List.exists (fun (a,b)->is_a_d b) y) z2 ;;
*)

(************************************************************************************************************************
Snippet 69 : Musings on the Van der Waerden problem, version 10 : add back D variant in the definition of the 
colored_variable type, and add Partition_watcher.decompose and 
Current_partition_watcher.decompose
************************************************************************************************************************)
open Needed_values ;;
type colored_variable = 
   A of int |B of int|C of int|D of int| Og of int * int ;;   
type list_of_affinities = Aff of ((int list * colored_variable) * bool)  list ;;    
type partition_watcher = 
   PW of 
   list_of_affinities *
   (((int list * colored_variable) * (colored_variable * colored_variable)) list) *
   ((colored_variable * (int list * colored_variable) list) list) ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;; 
let mea = Vdw_chosen.measure ;;
let lmea = Vdw_chosen.lower_measure ;;
let is_adm = Vdw_chosen.test_for_admissibility ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

let intlist_to_string lamb =
   "["^(String.concat ";" (Image.image string_of_int lamb))^"]" ;;

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  

module Colored = struct 

exception Label_for_variable_exn ;;
let data_for_variable = function 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) |(D k) ->("D",k) 
    |(Og(_,_))->raise(Label_for_variable_exn) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     if lbl = "D" then D k else  
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d [0,A 0;1, B 0;2, C 0;3, D 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

let to_ocaml_readable_string cv= match cv with  
   (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
   | _ -> let (lbl,k) = data_for_variable cv in 
          lbl^" "^(string_of_int k) ;; 

let order = ((fun cv1 cv2->
  let (lbl1,k1) = data_for_variable cv1 
  and (lbl2,k2) = data_for_variable cv2 in  
  let trial1 = Total_ordering.standard lbl1 lbl2 in 
  if trial1 <> Total_ordering_result_t.Equal then trial1 else 
   Total_ordering.standard k1 k2 
) : colored_variable Total_ordering_t.t) ;;

let order_for_pairs = ((fun (lamb1,cv1) (lamb2,cv2)->
   let trial1 = order cv1 cv2 in 
   if trial1 <> Total_ordering_result_t.Equal then trial1 else 
    (Total_ordering.silex_compare Total_ordering.standard ) lamb1 lamb2 
 ) : (int list * colored_variable) Total_ordering_t.t) ;;
 

end ;;  

module Affinity = struct

   let shadow (Aff l) cvar = 
      Option.filter_and_unpack (
         fun ((selector,cvar2),is_compatible) ->
           if cvar2 = cvar 
           then Some(selector,is_compatible)   
           else None
      ) l;;
  
  let expand_using_partition aff parti  =
     let (Aff(old_affinities)) = aff 
     and ((selector,partitioned),(part1,part2)) = parti in
     let temp1 = shadow aff partitioned in 
     let temp2 = List.flatten(Image.image (
        fun (selector2,is_compatible) ->
           [((selector2,part1),is_compatible);
            ((selector2,part2),is_compatible)]     
     ) temp1) in
     let new_affinities = old_affinities @ 
         [((selector,part1),true);
         ((selector,part2),false);]@temp2  in 
     Aff(new_affinities) ;;
  
  let insert affty  (Aff old_affinities)  =
     if List.mem affty old_affinities 
     then Aff old_affinities
     else Aff(affty :: old_affinities) ;;
  
  let analyse_combination (Aff affinities) combination =
     let temp0 = List.filter (
       fun (lamb,_)->Vdw_chosen.test_for_admissibility lamb
     ) combination in 
     let temp1 = Image.image (fun 
        pair -> 
         let res = (
          if fst pair = []
          then Some true
          else List.assoc_opt  pair affinities 
         ) in
         (pair,res)
     ) temp0 in 
     let filterer = (fun criterion ->
        Option.filter_and_unpack 
       (fun (pair,opt)->
         if opt=criterion then Some pair else None) temp1
     ) in 
     let bad_ones = filterer None in 
     let checked_version = filterer (Some true) in 
     (Ordered.sort Colored.order_for_pairs bad_ones,checked_version);;
   
  
  
end ;;   
  


module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let prepare_computation n d =
        let delt = mea(n)-mea(n-1) in   
        List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
        [
          [],(n-1,d-delt);
          [n],(n-1,d+1-delt)
        ];;   
let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (prepare_computation n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;

let direct_descendants (n,d) =
   let temp1 = Image.image snd (prepare_computation n d) in 
   List.filter (fun (nn,dd)->
         ((Hashtbl.find_opt hashtbl_for_oslo (nn,dd)) = None)
         && (nn >= wall_position)   
   ) temp1 ;;      

exception Pusher_for_needed_intermediaries_exn ;;   

let pusher_for_needed_intermediaries (treated,to_be_treated) =
   let oh = Total_ordering.standard2 in (* oh is for "order here" *)
   match (List.rev to_be_treated) with 
     [] -> raise (Pusher_for_needed_intermediaries_exn) 
    |pair :: others -> 
      let new_ones = Ordered.sort oh (direct_descendants pair) in 
   (Ordered.insert oh pair treated,
            Ordered.merge oh new_ones others) ;;  

let needed_intermediaries n d = 
   let rec tempf = (fun (treated,to_be_treated)->
      match (List.rev to_be_treated) with 
       [] -> treated 
      |pair :: others -> 
         let new_step = pusher_for_needed_intermediaries (treated,to_be_treated) in 
         tempf(new_step) 
   ) in 
   tempf([],direct_descendants (n,d) ) ;;      

end ;;

let og = Oslo.get ;;
let see = Oslo.prepare_computation ;;

module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  ((selector,partitioned),(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition 
   (PW(old_affinities,old_parties,ll):t) parti  =
   let new_affinities =  Affinity.expand_using_partition old_affinities parti
   and new_parties = old_parties @ [parti]   in 
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_affinities,new_parties,new_ll):>t) ;;

let insert_affinity (PW(old_affinities,old_parties,ll)) affty  =
   PW(Affinity.insert affty old_affinities,old_parties,ll) ;;        
    

exception Missing_affinities of ( (int list) * colored_variable) list;;   

let check_combination_using_affinities combination (PW(affinities,_,_):t)=
  let (bad_ones,checked_version) = Affinity.analyse_combination affinities combination in 
  if bad_ones <> []
  then raise(Missing_affinities bad_ones)
  else checked_version;;

let expand_using_t combination (pw:t) =
   let (PW(_,_,ll)) = pw in 
   let ref_for_unknowns = ref [] in 
   let combination2 = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in
   let new_combination = check_combination_using_affinities combination2  pw in
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(affinities,parties,ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(affinities,parties,ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(affinities,parties,List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   (intlist_to_string lamb)^","^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW (_,_,ll)):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;

let decompose ((PW (_,_,ll)):t) cvar =
     List.assoc cvar ll ;;

end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
   "D",[List.filter (fun x->(List.length x)=5) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = List.partition (Vdw_chosen.test_joinability selector)  old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    if (a=[])||(b=[])
    then (a,b,None)  
    else
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (a,b,Some(Colored.variable_from_label lbl i,
              Colored.variable_from_label lbl j)) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW(Aff[],[],[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
       (D 0),[[],D 0];
])) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;

let explain_partition (selector,cv,a,b,opt_cvs) =
   let s_selector = "["^(String.concat ";" (Image.image string_of_int selector))^"]" in  
   let beginning_of_msg = "\n\n"^(Colored.to_string cv)^" "
   and end_of_msg = (
      if a=[] then "is uniformly incompatible with "^s_selector else  
      if b=[] then "is uniformly compatible with "^s_selector else   
      let (cv_i,cv_j) = Option.unpack opt_cvs in       
      " splits as "^(Colored.to_string cv_i)
      ^" + "^(Colored.to_string cv_j)) in 
   let msg = beginning_of_msg ^ end_of_msg in 
   (print_string msg ;flush stdout) ;;             

let partition selector cv =
    let old_state = (!main) in 
    let (a,b,opt_cvs) = Colored_variable_assignment.partition selector cv in 
    let _ = explain_partition (selector,cv,a,b,opt_cvs) in 
    let new_state = (
    match opt_cvs with 
    None -> 
      let affty=((selector,cv),b=[]) in 
      Partition_watcher.insert_affinity old_state affty
    |Some(cv_i,cv_j) -> 
        Partition_watcher.expand_using_partition 
        old_state ((selector,cv),(cv_i,cv_j)) 
    ) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Oslo.prepare_computation n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

let decompose = Partition_watcher.decompose (!main) ;;

end ;;  

let oslo_compute = Current_partition_watcher.oslo_compute ;;


module Marshall_plan = struct 

let detect_missing_affinities n d=         
   let skeleton = Image.image (
     fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
   ) (Oslo.prepare_computation n d) in 
   let old_state = (!(Current_partition_watcher.main)) in 
   let (PW(affinities,_,ll)) = old_state in 
   let combination2 = List.flatten(Image.image (fun old_pair->
      let (lamb,cvar) = old_pair in 
      match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
       (Some expansion) -> Image.image (fun
        (lamb2,cvar2)-> 
        (Ordered.merge oi lamb lamb2,cvar2)) expansion
      |None -> [old_pair]
      ) skeleton) in 
      fst(Affinity.analyse_combination affinities combination2)  
    ;;  

let  write_partition_commands_from_list l=
  let temp1 = Image.image (fun (lamb,cv)->
   "let passing = Current_partition_watcher.partition "^
   (intlist_to_string lamb)^" ("^(Colored.to_ocaml_readable_string cv)^") ;;"
   )  l in 
  "\n\n\n" ^ (String.concat "\n" temp1) ^ "\n\n\n" ;;   
  
let write_partition_commands n d =
    let missing_affs = detect_missing_affinities n d in 
    print_string(write_partition_commands_from_list missing_affs) ;;  

end ;;   

Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;
Oslo.add (15,3) (Colored_variable_assignment.get (D 0)) ;;


let passing = Current_partition_watcher.partition [16] (B 0) ;;
oslo_compute 16 0 ;;

let passing = Current_partition_watcher.partition [16] (C 0) ;;
oslo_compute 16 1 ;;

let passing = Current_partition_watcher.partition [17] (B 1) ;;
let passing = Current_partition_watcher.partition [17] (B 2) ;;
let passing = Current_partition_watcher.partition [16;17] (C 1) ;;

oslo_compute 17 0 ;;

let passing = Current_partition_watcher.partition [18] (A 0) ;;
let passing = Current_partition_watcher.partition [16;18] (B 3) ;;
let passing = Current_partition_watcher.partition [16;18] (B 4) ;;
let passing = Current_partition_watcher.partition [17;18] (B 5) ;;
let passing = Current_partition_watcher.partition [17;18] (B 7) ;;
let passing = Current_partition_watcher.partition [17;18] (B 8) ;;

oslo_compute 18 0 ;;

let passing = Current_partition_watcher.partition [16] (D 0) ;;
oslo_compute 16 2 ;;


let passing = Current_partition_watcher.partition [17] (C 2) ;;
let passing = Current_partition_watcher.partition [17] (C 3) ;;
let passing = Current_partition_watcher.partition [17] (C 4) ;;
let passing = Current_partition_watcher.partition [16;17] (D 1) ;;
oslo_compute 17 1 ;;

let passing = Current_partition_watcher.partition [18] (B 6) ;;
let passing = Current_partition_watcher.partition [18] (B 7) ;;
let passing = Current_partition_watcher.partition [18] (B 8) ;;
let passing = Current_partition_watcher.partition [18] (B 9) ;;
let passing = Current_partition_watcher.partition [18] (B 10) ;;
let passing = Current_partition_watcher.partition [18] (B 11) ;;
let passing = Current_partition_watcher.partition [18] (B 12) ;;
let passing = Current_partition_watcher.partition [16;18] (C 3) ;;
let passing = Current_partition_watcher.partition [17;18] (C 3) ;;
let passing = Current_partition_watcher.partition [17;18] (C 5) ;;
let passing = Current_partition_watcher.partition [16;18] (C 7) ;;
let passing = Current_partition_watcher.partition [17;18] (C 7) ;;
let passing = Current_partition_watcher.partition [16;18] (C 8) ;;

let passing = Current_partition_watcher.partition [17;18] (C 9) ;;
let passing = Current_partition_watcher.partition [17;18] (C 10) ;;
let passing = Current_partition_watcher.partition [17;18] (C 15) ;;
let passing = Current_partition_watcher.partition [17;18] (C 16) ;;

oslo_compute 18 1 ;;

let z1 = Marshall_plan.write_partition_commands 18 1 ;;

(************************************************************************************************************************
Snippet 68 : Musings on the Van der Waerden problem, version 9 : add lmea and
remove D variant in the definition of the colored_variable type
************************************************************************************************************************)
open Needed_values ;;
type colored_variable = 
   A of int |B of int|C of int| Og of int * int ;;   
type list_of_affinities = Aff of ((int list * colored_variable) * bool)  list ;;    
type partition_watcher = 
   PW of 
   list_of_affinities *
   (((int list * colored_variable) * (colored_variable * colored_variable)) list) *
   ((colored_variable * (int list * colored_variable) list) list) ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;; 
let mea = Vdw_chosen.measure ;;
let lmea = Vdw_chosen.lower_measure ;;
let is_adm = Vdw_chosen.test_for_admissibility ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

let intlist_to_string lamb =
   "["^(String.concat ";" (Image.image string_of_int lamb))^"]" ;;

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  

module Colored = struct 

exception Label_for_variable_exn ;;
let data_for_variable = function 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) 
    |(Og(_,_))->raise(Label_for_variable_exn) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d [0,A 0;1, B 0;2, C 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

let to_ocaml_readable_string cv= match cv with  
   (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
   | _ -> let (lbl,k) = data_for_variable cv in 
          lbl^" "^(string_of_int k) ;; 

let order = ((fun cv1 cv2->
  let (lbl1,k1) = data_for_variable cv1 
  and (lbl2,k2) = data_for_variable cv2 in  
  let trial1 = Total_ordering.standard lbl1 lbl2 in 
  if trial1 <> Total_ordering_result_t.Equal then trial1 else 
   Total_ordering.standard k1 k2 
) : colored_variable Total_ordering_t.t) ;;

let order_for_pairs = ((fun (lamb1,cv1) (lamb2,cv2)->
   let trial1 = order cv1 cv2 in 
   if trial1 <> Total_ordering_result_t.Equal then trial1 else 
    (Total_ordering.silex_compare Total_ordering.standard ) lamb1 lamb2 
 ) : (int list * colored_variable) Total_ordering_t.t) ;;
 

end ;;  

module Affinity = struct

   let shadow (Aff l) cvar = 
      Option.filter_and_unpack (
         fun ((selector,cvar2),is_compatible) ->
           if cvar2 = cvar 
           then Some(selector,is_compatible)   
           else None
      ) l;;
  
  let expand_using_partition aff parti  =
     let (Aff(old_affinities)) = aff 
     and ((selector,partitioned),(part1,part2)) = parti in
     let temp1 = shadow aff partitioned in 
     let temp2 = List.flatten(Image.image (
        fun (selector2,is_compatible) ->
           [((selector2,part1),is_compatible);
            ((selector2,part2),is_compatible)]     
     ) temp1) in
     let new_affinities = old_affinities @ 
         [((selector,part1),true);
         ((selector,part2),false);]@temp2  in 
     Aff(new_affinities) ;;
  
  let insert affty  (Aff old_affinities)  =
     if List.mem affty old_affinities 
     then Aff old_affinities
     else Aff(affty :: old_affinities) ;;
  
  let analyse_combination (Aff affinities) combination =
     let temp0 = List.filter (
       fun (lamb,_)->Vdw_chosen.test_for_admissibility lamb
     ) combination in 
     let temp1 = Image.image (fun 
        pair -> 
         let res = (
          if fst pair = []
          then Some true
          else List.assoc_opt  pair affinities 
         ) in
         (pair,res)
     ) temp0 in 
     let filterer = (fun criterion ->
        Option.filter_and_unpack 
       (fun (pair,opt)->
         if opt=criterion then Some pair else None) temp1
     ) in 
     let bad_ones = filterer None in 
     let checked_version = filterer (Some true) in 
     (Ordered.sort Colored.order_for_pairs bad_ones,checked_version);;
   
  
  
end ;;   
  


module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let prepare_computation n d =
        let delt = mea(n)-mea(n-1) in   
        List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
        [
          [],(n-1,d-delt);
          [n],(n-1,d+1-delt)
        ];;   
let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (prepare_computation n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;

let direct_descendants (n,d) =
   let temp1 = Image.image snd (prepare_computation n d) in 
   List.filter (fun (nn,dd)->
         ((Hashtbl.find_opt hashtbl_for_oslo (nn,dd)) = None)
         && (nn >= wall_position)   
   ) temp1 ;;      

exception Pusher_for_needed_intermediaries_exn ;;   

let pusher_for_needed_intermediaries (treated,to_be_treated) =
   let oh = Total_ordering.standard2 in (* oh is for "order here" *)
   match (List.rev to_be_treated) with 
     [] -> raise (Pusher_for_needed_intermediaries_exn) 
    |pair :: others -> 
      let new_ones = Ordered.sort oh (direct_descendants pair) in 
   (Ordered.insert oh pair treated,
            Ordered.merge oh new_ones others) ;;  

let needed_intermediaries n d = 
   let rec tempf = (fun (treated,to_be_treated)->
      match (List.rev to_be_treated) with 
       [] -> treated 
      |pair :: others -> 
         let new_step = pusher_for_needed_intermediaries (treated,to_be_treated) in 
         tempf(new_step) 
   ) in 
   tempf([],direct_descendants (n,d) ) ;;      

end ;;

let og = Oslo.get ;;
let see = Oslo.prepare_computation ;;

module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  ((selector,partitioned),(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition 
   (PW(old_affinities,old_parties,ll):t) parti  =
   let new_affinities =  Affinity.expand_using_partition old_affinities parti
   and new_parties = old_parties @ [parti]   in 
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_affinities,new_parties,new_ll):>t) ;;

let insert_affinity (PW(old_affinities,old_parties,ll)) affty  =
   PW(Affinity.insert affty old_affinities,old_parties,ll) ;;        
    

exception Missing_affinities of ( (int list) * colored_variable) list;;   

let check_combination_using_affinities combination (PW(affinities,_,_):t)=
  let (bad_ones,checked_version) = Affinity.analyse_combination affinities combination in 
  if bad_ones <> []
  then raise(Missing_affinities bad_ones)
  else checked_version;;

let expand_using_t combination (pw:t) =
   let (PW(_,_,ll)) = pw in 
   let ref_for_unknowns = ref [] in 
   let combination2 = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in
   let new_combination = check_combination_using_affinities combination2  pw in
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(affinities,parties,ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(affinities,parties,ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(affinities,parties,List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   (intlist_to_string lamb)^","^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW (_,_,ll)):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;


end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = List.partition (Vdw_chosen.test_joinability selector)  old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    if (a=[])||(b=[])
    then (a,b,None)  
    else
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (a,b,Some(Colored.variable_from_label lbl i,
              Colored.variable_from_label lbl j)) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW(Aff[],[],[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
])) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;

let explain_partition (selector,cv,a,b,opt_cvs) =
   let s_selector = "["^(String.concat ";" (Image.image string_of_int selector))^"]" in  
   let beginning_of_msg = "\n\n"^(Colored.to_string cv)^" "
   and end_of_msg = (
      if a=[] then "is uniformly incompatible with "^s_selector else  
      if b=[] then "is uniformly compatible with "^s_selector else   
      let (cv_i,cv_j) = Option.unpack opt_cvs in       
      " splits as "^(Colored.to_string cv_i)
      ^" + "^(Colored.to_string cv_j)) in 
   let msg = beginning_of_msg ^ end_of_msg in 
   (print_string msg ;flush stdout) ;;             

let partition selector cv =
    let old_state = (!main) in 
    let (a,b,opt_cvs) = Colored_variable_assignment.partition selector cv in 
    let _ = explain_partition (selector,cv,a,b,opt_cvs) in 
    let new_state = (
    match opt_cvs with 
    None -> 
      let affty=((selector,cv),b=[]) in 
      Partition_watcher.insert_affinity old_state affty
    |Some(cv_i,cv_j) -> 
        Partition_watcher.expand_using_partition 
        old_state ((selector,cv),(cv_i,cv_j)) 
    ) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Oslo.prepare_computation n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

end ;;  

let oslo_compute = Current_partition_watcher.oslo_compute ;;


module Marshall_plan = struct 

let detect_missing_affinities n d=         
   let skeleton = Image.image (
     fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
   ) (Oslo.prepare_computation n d) in 
   let old_state = (!(Current_partition_watcher.main)) in 
   let (PW(affinities,_,ll)) = old_state in 
   let combination2 = List.flatten(Image.image (fun old_pair->
      let (lamb,cvar) = old_pair in 
      match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
       (Some expansion) -> Image.image (fun
        (lamb2,cvar2)-> 
        (Ordered.merge oi lamb lamb2,cvar2)) expansion
      |None -> [old_pair]
      ) skeleton) in 
      fst(Affinity.analyse_combination affinities combination2)  
    ;;  

let  write_partition_commands_from_list l=
  let temp1 = Image.image (fun (lamb,cv)->
   "let passing = Current_partition_watcher.partition "^
   (intlist_to_string lamb)^" ("^(Colored.to_ocaml_readable_string cv)^") ;;"
   )  l in 
  "\n\n\n" ^ (String.concat "\n" temp1) ^ "\n\n\n" ;;   
  
let write_partition_commands n d =
    let missing_affs = detect_missing_affinities n d in 
    print_string(write_partition_commands_from_list missing_affs) ;;  

end ;;   

Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;

let passing = Current_partition_watcher.partition [16] (B 0) ;;
oslo_compute 16 0 ;;

let passing = Current_partition_watcher.partition [16] (C 0) ;;
oslo_compute 16 1 ;;

let passing = Current_partition_watcher.partition [17] (B 1) ;;
let passing = Current_partition_watcher.partition [17] (B 2) ;;
let passing = Current_partition_watcher.partition [16;17] (C 1) ;;

oslo_compute 17 0 ;;

let passing = Current_partition_watcher.partition [18] (A 0) ;;
let passing = Current_partition_watcher.partition [16;18] (B 3) ;;
let passing = Current_partition_watcher.partition [16;18] (B 4) ;;
let passing = Current_partition_watcher.partition [17;18] (B 5) ;;
let passing = Current_partition_watcher.partition [17;18] (B 7) ;;
let passing = Current_partition_watcher.partition [17;18] (B 8) ;;

oslo_compute 18 0 ;;

let z1 = Oslo.needed_intermediaries 19 0 ;;
let z2 = Marshall_plan.detect_missing_affinities 19 0 ;;

(************************************************************************************************************************
Snippet 67 : Musings on the Van der Waerden problem, version 8 : adding
Marshall_plan.write_partition_commands
************************************************************************************************************************)
open Needed_values ;;
type colored_variable = 
   A of int |B of int|C of int|D of int| Og of int * int ;;   
type list_of_affinities = Aff of ((int list * colored_variable) * bool)  list ;;    
type partition_watcher = 
   PW of 
   list_of_affinities *
   (((int list * colored_variable) * (colored_variable * colored_variable)) list) *
   ((colored_variable * (int list * colored_variable) list) list) ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;; 
let mea = Vdw_chosen.measure ;;
let is_adm = Vdw_chosen.test_for_admissibility ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

let intlist_to_string lamb =
   "["^(String.concat ";" (Image.image string_of_int lamb))^"]" ;;

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  

module Colored = struct 

exception Label_for_variable_exn ;;
let data_for_variable = function 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) |(D k) ->("D",k) 
    |(Og(_,_))->raise(Label_for_variable_exn) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     if lbl = "D" then D k else  
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d [0,A 0;1, B 0;2, C 0;3, D 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

let to_ocaml_readable_string cv= match cv with  
   (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
   | _ -> let (lbl,k) = data_for_variable cv in 
          lbl^" "^(string_of_int k) ;; 

let order = ((fun cv1 cv2->
  let (lbl1,k1) = data_for_variable cv1 
  and (lbl2,k2) = data_for_variable cv2 in  
  let trial1 = Total_ordering.standard lbl1 lbl2 in 
  if trial1 <> Total_ordering_result_t.Equal then trial1 else 
   Total_ordering.standard k1 k2 
) : colored_variable Total_ordering_t.t) ;;

let order_for_pairs = ((fun (lamb1,cv1) (lamb2,cv2)->
   let trial1 = order cv1 cv2 in 
   if trial1 <> Total_ordering_result_t.Equal then trial1 else 
    (Total_ordering.silex_compare Total_ordering.standard ) lamb1 lamb2 
 ) : (int list * colored_variable) Total_ordering_t.t) ;;
 

end ;;  

module Affinity = struct

   let shadow (Aff l) cvar = 
      Option.filter_and_unpack (
         fun ((selector,cvar2),is_compatible) ->
           if cvar2 = cvar 
           then Some(selector,is_compatible)   
           else None
      ) l;;
  
  let expand_using_partition aff parti  =
     let (Aff(old_affinities)) = aff 
     and ((selector,partitioned),(part1,part2)) = parti in
     let temp1 = shadow aff partitioned in 
     let temp2 = List.flatten(Image.image (
        fun (selector2,is_compatible) ->
           [((selector2,part1),is_compatible);
            ((selector2,part2),is_compatible)]     
     ) temp1) in
     let new_affinities = old_affinities @ 
         [((selector,part1),true);
         ((selector,part2),false);]@temp2  in 
     Aff(new_affinities) ;;
  
  let insert affty  (Aff old_affinities)  =
     if List.mem affty old_affinities 
     then Aff old_affinities
     else Aff(affty :: old_affinities) ;;
  
  let analyse_combination (Aff affinities) combination =
     let temp0 = List.filter (
       fun (lamb,_)->Vdw_chosen.test_for_admissibility lamb
     ) combination in 
     let temp1 = Image.image (fun 
        pair -> 
         let res = (
          if fst pair = []
          then Some true
          else List.assoc_opt  pair affinities 
         ) in
         (pair,res)
     ) temp0 in 
     let filterer = (fun criterion ->
        Option.filter_and_unpack 
       (fun (pair,opt)->
         if opt=criterion then Some pair else None) temp1
     ) in 
     let bad_ones = filterer None in 
     let checked_version = filterer (Some true) in 
     (Ordered.sort Colored.order_for_pairs bad_ones,checked_version);;
   
  
  
end ;;   
  


module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let prepare_computation n d =
        let delt = mea(n)-mea(n-1) in   
        List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
        [
          [],(n-1,d-delt);
          [n],(n-1,d+1-delt)
        ];;   
let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (prepare_computation n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;

let direct_descendants (n,d) =
   let temp1 = Image.image snd (prepare_computation n d) in 
   List.filter (fun (nn,dd)->
         ((Hashtbl.find_opt hashtbl_for_oslo (nn,dd)) = None)
         && (nn >= wall_position)   
   ) temp1 ;;      

exception Pusher_for_needed_intermediaries_exn ;;   

let pusher_for_needed_intermediaries (treated,to_be_treated) =
   let oh = Total_ordering.standard2 in (* oh is for "order here" *)
   match (List.rev to_be_treated) with 
     [] -> raise (Pusher_for_needed_intermediaries_exn) 
    |pair :: others -> 
      let new_ones = Ordered.sort oh (direct_descendants pair) in 
   (Ordered.insert oh pair treated,
            Ordered.merge oh new_ones others) ;;  

let needed_intermediaries n d = 
   let rec tempf = (fun (treated,to_be_treated)->
      match (List.rev to_be_treated) with 
       [] -> treated 
      |pair :: others -> 
         let new_step = pusher_for_needed_intermediaries (treated,to_be_treated) in 
         tempf(new_step) 
   ) in 
   tempf([],direct_descendants (n,d) ) ;;      

end ;;

let og = Oslo.get ;;
let see = Oslo.prepare_computation ;;

module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  ((selector,partitioned),(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition 
   (PW(old_affinities,old_parties,ll):t) parti  =
   let new_affinities =  Affinity.expand_using_partition old_affinities parti
   and new_parties = old_parties @ [parti]   in 
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_affinities,new_parties,new_ll):>t) ;;

let insert_affinity (PW(old_affinities,old_parties,ll)) affty  =
   PW(Affinity.insert affty old_affinities,old_parties,ll) ;;        
    

exception Missing_affinities of ( (int list) * colored_variable) list;;   

let check_combination_using_affinities combination (PW(affinities,_,_):t)=
  let (bad_ones,checked_version) = Affinity.analyse_combination affinities combination in 
  if bad_ones <> []
  then raise(Missing_affinities bad_ones)
  else checked_version;;

let expand_using_t combination (pw:t) =
   let (PW(_,_,ll)) = pw in 
   let ref_for_unknowns = ref [] in 
   let combination2 = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in
   let new_combination = check_combination_using_affinities combination2  pw in
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(affinities,parties,ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(affinities,parties,ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(affinities,parties,List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   (intlist_to_string lamb)^","^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW (_,_,ll)):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;


end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
   "D",[List.filter (fun x->(List.length x)=5) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = List.partition (Vdw_chosen.test_joinability selector)  old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    if (a=[])||(b=[])
    then (a,b,None)  
    else
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (a,b,Some(Colored.variable_from_label lbl i,
              Colored.variable_from_label lbl j)) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW(Aff[],[],[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
       (D 0),[[],D 0];
])) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;

let explain_partition (selector,cv,a,b,opt_cvs) =
   let s_selector = "["^(String.concat ";" (Image.image string_of_int selector))^"]" in  
   let beginning_of_msg = "\n\n"^(Colored.to_string cv)^" "
   and end_of_msg = (
      if a=[] then "is uniformly incompatible with "^s_selector else  
      if b=[] then "is uniformly compatible with "^s_selector else   
      let (cv_i,cv_j) = Option.unpack opt_cvs in       
      " splits as "^(Colored.to_string cv_i)
      ^" + "^(Colored.to_string cv_j)) in 
   let msg = beginning_of_msg ^ end_of_msg in 
   (print_string msg ;flush stdout) ;;             

let partition selector cv =
    let old_state = (!main) in 
    let (a,b,opt_cvs) = Colored_variable_assignment.partition selector cv in 
    let _ = explain_partition (selector,cv,a,b,opt_cvs) in 
    let new_state = (
    match opt_cvs with 
    None -> 
      let affty=((selector,cv),b=[]) in 
      Partition_watcher.insert_affinity old_state affty
    |Some(cv_i,cv_j) -> 
        Partition_watcher.expand_using_partition 
        old_state ((selector,cv),(cv_i,cv_j)) 
    ) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Oslo.prepare_computation n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

end ;;  

let oslo_compute = Current_partition_watcher.oslo_compute ;;


module Marshall_plan = struct 

let detect_missing_affinities n d=         
   let skeleton = Image.image (
     fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
   ) (Oslo.prepare_computation n d) in 
   let old_state = (!(Current_partition_watcher.main)) in 
   let (PW(affinities,_,ll)) = old_state in 
   let combination2 = List.flatten(Image.image (fun old_pair->
      let (lamb,cvar) = old_pair in 
      match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
       (Some expansion) -> Image.image (fun
        (lamb2,cvar2)-> 
        (Ordered.merge oi lamb lamb2,cvar2)) expansion
      |None -> [old_pair]
      ) skeleton) in 
      fst(Affinity.analyse_combination affinities combination2)  
    ;;  

let  write_partition_commands_from_list l=
  let temp1 = Image.image (fun (lamb,cv)->
   "let passing = Current_partition_watcher.partition "^
   (intlist_to_string lamb)^" ("^(Colored.to_ocaml_readable_string cv)^") ;;"
   )  l in 
  "\n\n\n" ^ (String.concat "\n" temp1) ^ "\n\n\n" ;;   
  
let write_partition_commands n d =
    let missing_affs = detect_missing_affinities n d in 
    print_string(write_partition_commands_from_list missing_affs) ;;  

end ;;   

Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;
Oslo.add (15,3) (Colored_variable_assignment.get (D 0)) ;;


let passing = Current_partition_watcher.partition [16] (B 0) ;;
oslo_compute 16 0 ;;

let passing = Current_partition_watcher.partition [16] (C 0) ;;
oslo_compute 16 1 ;;

let passing = Current_partition_watcher.partition [17] (B 1) ;;
let passing = Current_partition_watcher.partition [17] (B 2) ;;
let passing = Current_partition_watcher.partition [16;17] (C 1) ;;

oslo_compute 17 0 ;;

let passing = Current_partition_watcher.partition [18] (A 0) ;;
let passing = Current_partition_watcher.partition [16;18] (B 3) ;;
let passing = Current_partition_watcher.partition [16;18] (B 4) ;;
let passing = Current_partition_watcher.partition [17;18] (B 5) ;;
let passing = Current_partition_watcher.partition [17;18] (B 7) ;;
let passing = Current_partition_watcher.partition [17;18] (B 8) ;;

oslo_compute 18 0 ;;

let passing = Current_partition_watcher.partition [16] (D 0) ;;
oslo_compute 16 2 ;;


let passing = Current_partition_watcher.partition [17] (C 2) ;;
let passing = Current_partition_watcher.partition [17] (C 3) ;;
let passing = Current_partition_watcher.partition [17] (C 4) ;;
let passing = Current_partition_watcher.partition [16;17] (D 1) ;;
oslo_compute 17 1 ;;

let passing = Current_partition_watcher.partition [18] (B 6) ;;
let passing = Current_partition_watcher.partition [18] (B 7) ;;
let passing = Current_partition_watcher.partition [18] (B 8) ;;
let passing = Current_partition_watcher.partition [18] (B 9) ;;
let passing = Current_partition_watcher.partition [18] (B 10) ;;
let passing = Current_partition_watcher.partition [18] (B 11) ;;
let passing = Current_partition_watcher.partition [18] (B 12) ;;
let passing = Current_partition_watcher.partition [16;18] (C 3) ;;
let passing = Current_partition_watcher.partition [17;18] (C 3) ;;
let passing = Current_partition_watcher.partition [17;18] (C 5) ;;
let passing = Current_partition_watcher.partition [16;18] (C 7) ;;
let passing = Current_partition_watcher.partition [17;18] (C 7) ;;
let passing = Current_partition_watcher.partition [16;18] (C 8) ;;

let passing = Current_partition_watcher.partition [17;18] (C 9) ;;
let passing = Current_partition_watcher.partition [17;18] (C 10) ;;
let passing = Current_partition_watcher.partition [17;18] (C 15) ;;
let passing = Current_partition_watcher.partition [17;18] (C 16) ;;

oslo_compute 18 1 ;;
let z1 = Marshall_plan.write_partition_commands 18 1;;


(************************************************************************************************************************
Snippet 66 : Musings on the Van der Waerden problem, version 7 : adding 
Colored.order{_for_pairs} and using it in Affinity.analyse_combination 
************************************************************************************************************************)
open Needed_values ;;

type colored_variable = 
   A of int |B of int|C of int|D of int| Og of int * int ;;   
type list_of_affinities = Aff of ((int list * colored_variable) * bool)  list ;;    
type partition_watcher = 
   PW of 
   list_of_affinities *
   (((int list * colored_variable) * (colored_variable * colored_variable)) list) *
   ((colored_variable * (int list * colored_variable) list) list) ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;; 
let mea = Vdw_chosen.measure ;;
let is_adm = Vdw_chosen.test_for_admissibility ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

exception First_part_is_empty of int list ;;
exception Second_part_is_empty of int list ;;

let nonempty_partition selector ll = 
  let answer = List.partition (Vdw_chosen.test_joinability selector) ll in 
  if (fst answer) = [] then raise(First_part_is_empty selector) else 
  if (snd answer) = [] then raise(Second_part_is_empty selector) else  
  answer ;; 

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  

module Colored = struct 

exception Label_for_variable_exn ;;
let data_for_variable = function 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) |(D k) ->("D",k) 
    |(Og(_,_))->raise(Label_for_variable_exn) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     if lbl = "D" then D k else  
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d [0,A 0;1, B 0;2, C 0;3, D 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

let order = ((fun cv1 cv2->
  let (lbl1,k1) = data_for_variable cv1 
  and (lbl2,k2) = data_for_variable cv2 in  
  let trial1 = Total_ordering.standard lbl1 lbl2 in 
  if trial1 <> Total_ordering_result_t.Equal then trial1 else 
   Total_ordering.standard k1 k2 
) : colored_variable Total_ordering_t.t) ;;

let order_for_pairs = ((fun (lamb1,cv1) (lamb2,cv2)->
   let trial1 = order cv1 cv2 in 
   if trial1 <> Total_ordering_result_t.Equal then trial1 else 
    (Total_ordering.silex_compare Total_ordering.standard ) lamb1 lamb2 
 ) : (int list * colored_variable) Total_ordering_t.t) ;;
 

end ;;  

module Affinity = struct

   let shadow (Aff l) cvar = 
      Option.filter_and_unpack (
         fun ((selector,cvar2),is_compatible) ->
           if cvar2 = cvar 
           then Some(selector,is_compatible)   
           else None
      ) l;;
  
  let expand_using_partition aff parti  =
     let (Aff(old_affinities)) = aff 
     and ((selector,partitioned),(part1,part2)) = parti in
     let temp1 = shadow aff partitioned in 
     let temp2 = List.flatten(Image.image (
        fun (selector2,is_compatible) ->
           [((selector2,part1),is_compatible);
            ((selector2,part2),is_compatible)]     
     ) temp1) in
     let new_affinities = old_affinities @ 
         [((selector,part1),true);
         ((selector,part2),false);]@temp2  in 
     Aff(new_affinities) ;;
  
  let insert affty  (Aff old_affinities)  =
     if List.mem affty old_affinities 
     then Aff old_affinities
     else Aff(affty :: old_affinities) ;;
  
  let analyse_combination (Aff affinities) combination =
     let temp0 = List.filter (
       fun (lamb,_)->Vdw_chosen.test_for_admissibility lamb
     ) combination in 
     let temp1 = Image.image (fun 
        pair -> 
         let res = (
          if fst pair = []
          then Some true
          else List.assoc_opt  pair affinities 
         ) in
         (pair,res)
     ) temp0 in 
     let filterer = (fun criterion ->
        Option.filter_and_unpack 
       (fun (pair,opt)->
         if opt=criterion then Some pair else None) temp1
     ) in 
     let bad_ones = filterer None in 
     let checked_version = filterer (Some true) in 
     (Ordered.sort Colored.order_for_pairs bad_ones,checked_version);;
   
  
  
end ;;   
  


module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let prepare_computation n d =
        let delt = mea(n)-mea(n-1) in   
        List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
        [
          [],(n-1,d-delt);
          [n],(n-1,d+1-delt)
        ];;   
let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (prepare_computation n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;

let direct_descendants (n,d) =
   let temp1 = Image.image snd (prepare_computation n d) in 
   List.filter (fun (nn,dd)->
         ((Hashtbl.find_opt hashtbl_for_oslo (nn,dd)) = None)
         && (nn >= wall_position)   
   ) temp1 ;;      

exception Pusher_for_needed_intermediaries_exn ;;   

let pusher_for_needed_intermediaries (treated,to_be_treated) =
   let oh = Total_ordering.standard2 in (* oh is for "order here" *)
   match (List.rev to_be_treated) with 
     [] -> raise (Pusher_for_needed_intermediaries_exn) 
    |pair :: others -> 
      let new_ones = Ordered.sort oh (direct_descendants pair) in 
   (Ordered.insert oh pair treated,
            Ordered.merge oh new_ones others) ;;  

let needed_intermediaries n d = 
   let rec tempf = (fun (treated,to_be_treated)->
      match (List.rev to_be_treated) with 
       [] -> treated 
      |pair :: others -> 
         let new_step = pusher_for_needed_intermediaries (treated,to_be_treated) in 
         tempf(new_step) 
   ) in 
   tempf([],direct_descendants (n,d) ) ;;      

end ;;

let og = Oslo.get ;;
let see = Oslo.prepare_computation ;;

module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  ((selector,partitioned),(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition 
   (PW(old_affinities,old_parties,ll):t) parti  =
   let new_affinities =  Affinity.expand_using_partition old_affinities parti
   and new_parties = old_parties @ [parti]   in 
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_affinities,new_parties,new_ll):>t) ;;

let insert_affinity (PW(old_affinities,old_parties,ll)) affty  =
   PW(Affinity.insert affty old_affinities,old_parties,ll) ;;        
    

exception Missing_affinities of ( (int list) * colored_variable) list;;   

let check_combination_using_affinities combination (PW(affinities,_,_):t)=
  let (bad_ones,checked_version) = Affinity.analyse_combination affinities combination in 
  if bad_ones <> []
  then raise(Missing_affinities bad_ones)
  else checked_version;;

let expand_using_t combination (pw:t) =
   let (PW(_,_,ll)) = pw in 
   let ref_for_unknowns = ref [] in 
   let combination2 = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in
   let new_combination = check_combination_using_affinities combination2  pw in
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(affinities,parties,ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(affinities,parties,ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(affinities,parties,List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   "["^(String.concat ";" (Image.image string_of_int lamb))
   ^"],"^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW (_,_,ll)):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;


end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
   "D",[List.filter (fun x->(List.length x)=5) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = List.partition (Vdw_chosen.test_joinability selector)  old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    if (a=[])||(b=[])
    then (a,b,None)  
    else
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (a,b,Some(Colored.variable_from_label lbl i,
              Colored.variable_from_label lbl j)) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW(Aff[],[],[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
       (D 0),[[],D 0];
])) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;

let explain_partition (selector,cv,a,b,opt_cvs) =
   let s_selector = "["^(String.concat ";" (Image.image string_of_int selector))^"]" in  
   let beginning_of_msg = "\n\n"^(Colored.to_string cv)^" "
   and end_of_msg = (
      if a=[] then "is uniformly incompatible with "^s_selector else  
      if b=[] then "is uniformly compatible with "^s_selector else   
      let (cv_i,cv_j) = Option.unpack opt_cvs in       
      " splits as "^(Colored.to_string cv_i)
      ^" + "^(Colored.to_string cv_j)) in 
   let msg = beginning_of_msg ^ end_of_msg in 
   (print_string msg ;flush stdout) ;;             

let partition selector cv =
    let old_state = (!main) in 
    let (a,b,opt_cvs) = Colored_variable_assignment.partition selector cv in 
    let _ = explain_partition (selector,cv,a,b,opt_cvs) in 
    let new_state = (
    match opt_cvs with 
    None -> 
      let affty=((selector,cv),b=[]) in 
      Partition_watcher.insert_affinity old_state affty
    |Some(cv_i,cv_j) -> 
        Partition_watcher.expand_using_partition 
        old_state ((selector,cv),(cv_i,cv_j)) 
    ) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Oslo.prepare_computation n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

end ;;  

let oslo_compute = Current_partition_watcher.oslo_compute ;;


module Marshall_plan = struct 

let detect_missing_affinities n d=         
   let skeleton = Image.image (
     fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
   ) (Oslo.prepare_computation n d) in 
   let old_state = (!(Current_partition_watcher.main)) in 
   let (PW(affinities,_,ll)) = old_state in 
   let combination2 = List.flatten(Image.image (fun old_pair->
      let (lamb,cvar) = old_pair in 
      match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
       (Some expansion) -> Image.image (fun
        (lamb2,cvar2)-> 
        (Ordered.merge oi lamb lamb2,cvar2)) expansion
      |None -> [old_pair]
      ) skeleton) in 
      fst(Affinity.analyse_combination affinities combination2)  
    ;;  
   
end ;;   

Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;
Oslo.add (15,3) (Colored_variable_assignment.get (D 0)) ;;


let passing = Current_partition_watcher.partition [16] (B 0) ;;
oslo_compute 16 0 ;;

let passing = Current_partition_watcher.partition [16] (C 0) ;;
oslo_compute 16 1 ;;

let passing = Current_partition_watcher.partition [17] (B 1) ;;
let passing = Current_partition_watcher.partition [17] (B 2) ;;
let passing = Current_partition_watcher.partition [16;17] (C 1) ;;

oslo_compute 17 0 ;;

let passing = Current_partition_watcher.partition [18] (A 0) ;;
let passing = Current_partition_watcher.partition [16;18] (B 3) ;;
let passing = Current_partition_watcher.partition [16;18] (B 4) ;;
let passing = Current_partition_watcher.partition [17;18] (B 5) ;;
let passing = Current_partition_watcher.partition [17;18] (B 7) ;;
let passing = Current_partition_watcher.partition [17;18] (B 8) ;;

oslo_compute 18 0 ;;

let passing = Current_partition_watcher.partition [16] (D 0) ;;
oslo_compute 16 2 ;;


let passing = Current_partition_watcher.partition [17] (C 2) ;;
let passing = Current_partition_watcher.partition [17] (C 3) ;;
let passing = Current_partition_watcher.partition [17] (C 4) ;;
let passing = Current_partition_watcher.partition [16;17] (D 1) ;;
oslo_compute 17 1 ;;

oslo_compute 18 1 ;;
let z1 = Marshall_plan.detect_missing_affinities 18 1;;

(************************************************************************************************************************
Snippet 65 : Musings on the Van der Waerden problem, version 6 : adding
Marshall_plan.detect_missing_affinities
************************************************************************************************************************)
open Needed_values ;;
type colored_variable = 
   A of int |B of int|C of int|D of int| Og of int * int ;;   
type list_of_affinities = Aff of ((int list * colored_variable) * bool)  list ;;    
type partition_watcher = 
   PW of 
   list_of_affinities *
   (((int list * colored_variable) * (colored_variable * colored_variable)) list) *
   ((colored_variable * (int list * colored_variable) list) list) ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;; 
let mea = Vdw_chosen.measure ;;
let is_adm = Vdw_chosen.test_for_admissibility ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

exception First_part_is_empty of int list ;;
exception Second_part_is_empty of int list ;;

let nonempty_partition selector ll = 
  let answer = List.partition (Vdw_chosen.test_joinability selector) ll in 
  if (fst answer) = [] then raise(First_part_is_empty selector) else 
  if (snd answer) = [] then raise(Second_part_is_empty selector) else  
  answer ;; 

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  

module Colored = struct 

exception Label_for_variable_exn ;;
let data_for_variable = function 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) |(D k) ->("D",k) 
    |(Og(_,_))->raise(Label_for_variable_exn) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     if lbl = "D" then D k else  
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d [0,A 0;1, B 0;2, C 0;3, D 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

end ;;  

module Affinity = struct

   let shadow (Aff l) cvar = 
      Option.filter_and_unpack (
         fun ((selector,cvar2),is_compatible) ->
           if cvar2 = cvar 
           then Some(selector,is_compatible)   
           else None
      ) l;;
  
  let expand_using_partition aff parti  =
     let (Aff(old_affinities)) = aff 
     and ((selector,partitioned),(part1,part2)) = parti in
     let temp1 = shadow aff partitioned in 
     let temp2 = List.flatten(Image.image (
        fun (selector2,is_compatible) ->
           [((selector2,part1),is_compatible);
            ((selector2,part2),is_compatible)]     
     ) temp1) in
     let new_affinities = old_affinities @ 
         [((selector,part1),true);
         ((selector,part2),false);]@temp2  in 
     Aff(new_affinities) ;;
  
  let insert affty  (Aff old_affinities)  =
     if List.mem affty old_affinities 
     then Aff old_affinities
     else Aff(affty :: old_affinities) ;;
  
  let analyse_combination (Aff affinities) combination =
     let temp0 = List.filter (
       fun (lamb,_)->Vdw_chosen.test_for_admissibility lamb
     ) combination in 
     let temp1 = Image.image (fun 
        pair -> 
         let res = (
          if fst pair = []
          then Some true
          else List.assoc_opt  pair affinities 
         ) in
         (pair,res)
     ) temp0 in 
     let filterer = (fun criterion ->
        Option.filter_and_unpack 
       (fun (pair,opt)->
         if opt=criterion then Some pair else None) temp1
     ) in 
     let bad_ones = filterer None in 
     let checked_version = filterer (Some true) in 
     (bad_ones,checked_version);;
   
  
  
end ;;   
  


module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let prepare_computation n d =
        let delt = mea(n)-mea(n-1) in   
        List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
        [
          [],(n-1,d-delt);
          [n],(n-1,d+1-delt)
        ];;   
let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (prepare_computation n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;

let direct_descendants (n,d) =
   let temp1 = Image.image snd (prepare_computation n d) in 
   List.filter (fun (nn,dd)->
         ((Hashtbl.find_opt hashtbl_for_oslo (nn,dd)) = None)
         && (nn >= wall_position)   
   ) temp1 ;;      

exception Pusher_for_needed_intermediaries_exn ;;   

let pusher_for_needed_intermediaries (treated,to_be_treated) =
   let oh = Total_ordering.standard2 in (* oh is for "order here" *)
   match (List.rev to_be_treated) with 
     [] -> raise (Pusher_for_needed_intermediaries_exn) 
    |pair :: others -> 
      let new_ones = Ordered.sort oh (direct_descendants pair) in 
   (Ordered.insert oh pair treated,
            Ordered.merge oh new_ones others) ;;  

let needed_intermediaries n d = 
   let rec tempf = (fun (treated,to_be_treated)->
      match (List.rev to_be_treated) with 
       [] -> treated 
      |pair :: others -> 
         let new_step = pusher_for_needed_intermediaries (treated,to_be_treated) in 
         tempf(new_step) 
   ) in 
   tempf([],direct_descendants (n,d) ) ;;      

end ;;

let og = Oslo.get ;;
let see = Oslo.prepare_computation ;;

module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  ((selector,partitioned),(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition 
   (PW(old_affinities,old_parties,ll):t) parti  =
   let new_affinities =  Affinity.expand_using_partition old_affinities parti
   and new_parties = old_parties @ [parti]   in 
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_affinities,new_parties,new_ll):>t) ;;

let insert_affinity (PW(old_affinities,old_parties,ll)) affty  =
   PW(Affinity.insert affty old_affinities,old_parties,ll) ;;        
    

exception Missing_affinities of ( (int list) * colored_variable) list;;   

let check_combination_using_affinities combination (PW(affinities,_,_):t)=
  let (bad_ones,checked_version) = Affinity.analyse_combination affinities combination in 
  if bad_ones <> []
  then raise(Missing_affinities bad_ones)
  else checked_version;;

let expand_using_t combination (pw:t) =
   let (PW(_,_,ll)) = pw in 
   let ref_for_unknowns = ref [] in 
   let combination2 = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in
   let new_combination = check_combination_using_affinities combination2  pw in
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(affinities,parties,ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(affinities,parties,ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(affinities,parties,List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   "["^(String.concat ";" (Image.image string_of_int lamb))
   ^"],"^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW (_,_,ll)):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;


end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
   "D",[List.filter (fun x->(List.length x)=5) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = List.partition (Vdw_chosen.test_joinability selector)  old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    if (a=[])||(b=[])
    then (a,b,None)  
    else
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (a,b,Some(Colored.variable_from_label lbl i,
              Colored.variable_from_label lbl j)) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW(Aff[],[],[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
       (D 0),[[],D 0];
])) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;

let explain_partition (selector,cv,a,b,opt_cvs) =
   let s_selector = "["^(String.concat ";" (Image.image string_of_int selector))^"]" in  
   let beginning_of_msg = "\n\n"^(Colored.to_string cv)^" "
   and end_of_msg = (
      if a=[] then "is uniformly incompatible with "^s_selector else  
      if b=[] then "is uniformly compatible with "^s_selector else   
      let (cv_i,cv_j) = Option.unpack opt_cvs in       
      " splits as "^(Colored.to_string cv_i)
      ^" + "^(Colored.to_string cv_j)) in 
   let msg = beginning_of_msg ^ end_of_msg in 
   (print_string msg ;flush stdout) ;;             

let partition selector cv =
    let old_state = (!main) in 
    let (a,b,opt_cvs) = Colored_variable_assignment.partition selector cv in 
    let _ = explain_partition (selector,cv,a,b,opt_cvs) in 
    let new_state = (
    match opt_cvs with 
    None -> 
      let affty=((selector,cv),b=[]) in 
      Partition_watcher.insert_affinity old_state affty
    |Some(cv_i,cv_j) -> 
        Partition_watcher.expand_using_partition 
        old_state ((selector,cv),(cv_i,cv_j)) 
    ) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Oslo.prepare_computation n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

end ;;  

let oslo_compute = Current_partition_watcher.oslo_compute ;;


module Marshall_plan = struct 

let detect_missing_affinities n d=         
   let skeleton = Image.image (
     fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
   ) (Oslo.prepare_computation n d) in 
   let old_state = (!(Current_partition_watcher.main)) in 
   let (PW(affinities,_,ll)) = old_state in 
   let combination2 = List.flatten(Image.image (fun old_pair->
      let (lamb,cvar) = old_pair in 
      match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
       (Some expansion) -> Image.image (fun
        (lamb2,cvar2)-> 
        (Ordered.merge oi lamb lamb2,cvar2)) expansion
      |None -> [old_pair]
      ) skeleton) in 
      fst(Affinity.analyse_combination affinities combination2)  
    ;;  
   
end ;;   

Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;
Oslo.add (15,3) (Colored_variable_assignment.get (D 0)) ;;


let passing = Current_partition_watcher.partition [16] (B 0) ;;
oslo_compute 16 0 ;;

let passing = Current_partition_watcher.partition [16] (C 0) ;;
oslo_compute 16 1 ;;

let passing = Current_partition_watcher.partition [17] (B 1) ;;
let passing = Current_partition_watcher.partition [17] (B 2) ;;
let passing = Current_partition_watcher.partition [16;17] (C 1) ;;

oslo_compute 17 0 ;;

let passing = Current_partition_watcher.partition [18] (A 0) ;;
let passing = Current_partition_watcher.partition [16;18] (B 3) ;;
let passing = Current_partition_watcher.partition [16;18] (B 4) ;;
let passing = Current_partition_watcher.partition [17;18] (B 5) ;;
let passing = Current_partition_watcher.partition [17;18] (B 7) ;;
let passing = Current_partition_watcher.partition [17;18] (B 8) ;;

oslo_compute 18 0 ;;

let passing = Current_partition_watcher.partition [16] (D 0) ;;
oslo_compute 16 2 ;;


let passing = Current_partition_watcher.partition [17] (C 2) ;;
let passing = Current_partition_watcher.partition [17] (C 3) ;;
let passing = Current_partition_watcher.partition [17] (C 4) ;;
let passing = Current_partition_watcher.partition [16;17] (D 1) ;;
oslo_compute 17 1 ;;

oslo_compute 18 1 ;;
let z1 = Marshall_plan.detect_missing_affinities 18 1;;


(************************************************************************************************************************
Snippet 64 : Musings on the Van der Waerden problem, version 5 : adding 
Oslo.needed_intermediaries, and display partitions as they are made

************************************************************************************************************************)
open Needed_values ;;

type colored_variable = 
   A of int |B of int|C of int| Og of int * int ;;   
type list_of_affinities = Aff of ((int list * colored_variable) * bool)  list ;;    
type partition_watcher = 
   PW of 
   list_of_affinities *
   (((int list * colored_variable) * (colored_variable * colored_variable)) list) *
   ((colored_variable * (int list * colored_variable) list) list) ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;; 
let mea = Vdw_chosen.measure ;;
let is_adm = Vdw_chosen.test_for_admissibility ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

exception First_part_is_empty of int list ;;
exception Second_part_is_empty of int list ;;

let nonempty_partition selector ll = 
  let answer = List.partition (Vdw_chosen.test_joinability selector) ll in 
  if (fst answer) = [] then raise(First_part_is_empty selector) else 
  if (snd answer) = [] then raise(Second_part_is_empty selector) else  
  answer ;; 

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  
module Affinity = struct

 let shadow (Aff l) cvar = 
    Option.filter_and_unpack (
       fun ((selector,cvar2),is_compatible) ->
         if cvar2 = cvar 
         then Some(selector,is_compatible)   
         else None
    ) l;;

let expand_using_partition aff parti  =
   let (Aff(old_affinities)) = aff 
   and ((selector,partitioned),(part1,part2)) = parti in
   let temp1 = shadow aff partitioned in 
   let temp2 = List.flatten(Image.image (
      fun (selector2,is_compatible) ->
         [((selector2,part1),is_compatible);
          ((selector2,part2),is_compatible)]     
   ) temp1) in
   let new_affinities = old_affinities @ 
       [((selector,part1),true);
       ((selector,part2),false);]@temp2  in 
   Aff(new_affinities) ;;

let insert affty  (Aff old_affinities)  =
   if List.mem affty old_affinities 
   then Aff old_affinities
   else Aff(affty :: old_affinities) ;;

let analyse_combination (Aff affinities) combination =
   let temp0 = List.filter (
     fun (lamb,_)->Vdw_chosen.test_for_admissibility lamb
   ) combination in 
   let temp1 = Image.image (fun 
      pair -> 
       let res = (
        if fst pair = []
        then Some true
        else List.assoc_opt  pair affinities 
       ) in
       (pair,res)
   ) temp0 in 
   let filterer = (fun criterion ->
      Option.filter_and_unpack 
     (fun (pair,opt)->
       if opt=criterion then Some pair else None) temp1
   ) in 
   let bad_ones = filterer None in 
   let checked_version = filterer (Some true) in 
   (bad_ones,checked_version);;
 


end ;;   

module Colored = struct 

exception Label_for_variable_exn ;;
let data_for_variable = function 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) 
    |(Og(_,_))->raise(Label_for_variable_exn) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d [0,A 0;1, B 0;2, C 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

end ;;  

module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let prepare_computation n d =
        let delt = mea(n)-mea(n-1) in   
        List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
        [
          [],(n-1,d-delt);
          [n],(n-1,d+1-delt)
        ];;   
let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (prepare_computation n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;

let direct_descendants (n,d) =
   let temp1 = Image.image snd (prepare_computation n d) in 
   List.filter (fun (nn,dd)->
            (Hashtbl.find_opt hashtbl_for_oslo (nn,dd)) = None   
   ) temp1 ;;      
      
let needed_intermediaries n d =
   let oh = Total_ordering.standard2 in (* oh is for "order here" *)
   let rec tempf = (fun (treated,to_be_treated)->
      match (List.rev to_be_treated) with 
       [] -> treated 
      |pair :: others -> 
         let new_ones = Ordered.sort oh (direct_descendants pair) in 
         tempf(Ordered.insert oh pair treated,
               Ordered.merge oh new_ones others) 
   ) in 
   tempf([],direct_descendants (n,d) ) ;;      

end ;;

let og = Oslo.get ;;
let see = Oslo.prepare_computation ;;

module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  ((selector,partitioned),(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition 
   (PW(old_affinities,old_parties,ll):t) parti  =
   let new_affinities =  Affinity.expand_using_partition old_affinities parti
   and new_parties = old_parties @ [parti]   in 
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_affinities,new_parties,new_ll):>t) ;;

let insert_affinity (PW(old_affinities,old_parties,ll)) affty  =
   PW(Affinity.insert affty old_affinities,old_parties,ll) ;;        
    

exception Missing_affinities of ( (int list) * colored_variable) list;;   

let check_combination_using_affinities combination (PW(affinities,_,_):t)=
  let (bad_ones,checked_version) = Affinity.analyse_combination affinities combination in 
  if bad_ones <> []
  then raise(Missing_affinities bad_ones)
  else checked_version;;

     

let expand_using_t combination (pw:t) =
   let (PW(_,_,ll)) = pw in 
   let ref_for_unknowns = ref [] in 
   let combination2 = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in
   let new_combination = check_combination_using_affinities combination2  pw in
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(affinities,parties,ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(affinities,parties,ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(affinities,parties,List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   "["^(String.concat ";" (Image.image string_of_int lamb))
   ^"],"^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW (_,_,ll)):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;


end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = List.partition (Vdw_chosen.test_joinability selector)  old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    if (a=[])||(b=[])
    then (a,b,None)  
    else
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (a,b,Some(Colored.variable_from_label lbl i,
              Colored.variable_from_label lbl j)) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW(Aff[],[],[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
])) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;

let explain_partition (selector,cv,a,b,opt_cvs) =
   let s_selector = "["^(String.concat ";" (Image.image string_of_int selector))^"]" in  
   let beginning_of_msg = "\n\n"^(Colored.to_string cv)^" "
   and end_of_msg = (
      if a=[] then "is uniformly incompatible with "^s_selector else  
      if b=[] then "is uniformly compatible with "^s_selector else   
      let (cv_i,cv_j) = Option.unpack opt_cvs in       
      " splits as "^(Colored.to_string cv_i)
      ^" + "^(Colored.to_string cv_j)) in 
   let msg = beginning_of_msg ^ end_of_msg in 
   (print_string msg ;flush stdout) ;;             

let partition selector cv =
    let old_state = (!main) in 
    let (a,b,opt_cvs) = Colored_variable_assignment.partition selector cv in 
    let _ = explain_partition (selector,cv,a,b,opt_cvs) in 
    let new_state = (
    match opt_cvs with 
    None -> 
      let affty=((selector,cv),b=[]) in 
      Partition_watcher.insert_affinity old_state affty
    |Some(cv_i,cv_j) -> 
        Partition_watcher.expand_using_partition 
        old_state ((selector,cv),(cv_i,cv_j)) 
    ) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Oslo.prepare_computation n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

end ;;  

let oslo_compute = Current_partition_watcher.oslo_compute ;;


Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;


let passing = Current_partition_watcher.partition [16] (B 0) ;;
oslo_compute 16 0 ;;

let passing = Current_partition_watcher.partition [16] (C 0) ;;
oslo_compute 16 1 ;;

let passing = Current_partition_watcher.partition [17] (B 1) ;;
let passing = Current_partition_watcher.partition [17] (B 2) ;;
let passing = Current_partition_watcher.partition [16;17] (C 1) ;;

oslo_compute 17 0 ;;

let passing = Current_partition_watcher.partition [18] (A 0) ;;
let passing = Current_partition_watcher.partition [16;18] (B 3) ;;
let passing = Current_partition_watcher.partition [16;18] (B 4) ;;
let passing = Current_partition_watcher.partition [17;18] (B 5) ;;
let passing = Current_partition_watcher.partition [17;18] (B 7) ;;
let passing = Current_partition_watcher.partition [17;18] (B 8) ;;

oslo_compute 18 0 ;;


(************************************************************************************************************************
Snippet 63 : Musings on the Van der Waerden problem, version 4 : adding 
the Affinity submodule and make each part in a partition inherit the affinities
of the whole
************************************************************************************************************************)
open Needed_values ;;

type colored_variable = 
   A of int |B of int|C of int| Og of int * int ;;   
type list_of_affinities = Aff of ((int list * colored_variable) * bool)  list ;;    
type partition_watcher = 
   PW of 
   list_of_affinities *
   (((int list * colored_variable) * (colored_variable * colored_variable)) list) *
   ((colored_variable * (int list * colored_variable) list) list) ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;; 
let mea = Vdw_chosen.measure ;;
let is_adm = Vdw_chosen.test_for_admissibility ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

exception First_part_is_empty of int list ;;
exception Second_part_is_empty of int list ;;

let nonempty_partition selector ll = 
  let answer = List.partition (Vdw_chosen.test_joinability selector) ll in 
  if (fst answer) = [] then raise(First_part_is_empty selector) else 
  if (snd answer) = [] then raise(Second_part_is_empty selector) else  
  answer ;; 

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  
module Affinity = struct

 let shadow (Aff l) cvar = 
    Option.filter_and_unpack (
       fun ((selector,cvar2),is_compatible) ->
         if cvar2 = cvar 
         then Some(selector,is_compatible)   
         else None
    ) l;;

let expand_using_partition aff parti  =
   let (Aff(old_affinities)) = aff 
   and ((selector,partitioned),(part1,part2)) = parti in
   let temp1 = shadow aff partitioned in 
   let temp2 = List.flatten(Image.image (
      fun (selector2,is_compatible) ->
         [((selector2,part1),is_compatible);
          ((selector2,part2),is_compatible)]     
   ) temp1) in
   let new_affinities = old_affinities @ 
       [((selector,part1),true);
       ((selector,part2),false);]@temp2  in 
   Aff(new_affinities) ;;

let insert affty  (Aff old_affinities)  =
   if List.mem affty old_affinities 
   then Aff old_affinities
   else Aff(affty :: old_affinities) ;;

let analyse_combination (Aff affinities) combination =
   let temp0 = List.filter (
     fun (lamb,_)->Vdw_chosen.test_for_admissibility lamb
   ) combination in 
   let temp1 = Image.image (fun 
      pair -> 
       let res = (
        if fst pair = []
        then Some true
        else List.assoc_opt  pair affinities 
       ) in
       (pair,res)
   ) temp0 in 
   let filterer = (fun criterion ->
      Option.filter_and_unpack 
     (fun (pair,opt)->
       if opt=criterion then Some pair else None) temp1
   ) in 
   let bad_ones = filterer None in 
   let checked_version = filterer (Some true) in 
   (bad_ones,checked_version);;
 


end ;;   

module Colored = struct 

exception Label_for_variable_exn ;;
let data_for_variable = function 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) 
    |(Og(_,_))->raise(Label_for_variable_exn) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d [0,A 0;1, B 0;2, C 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

end ;;  

module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let prepare_computation n d =
        let delt = mea(n)-mea(n-1) in   
        List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
        [
          [],(n-1,d-delt);
          [n],(n-1,d+1-delt)
        ];;   
let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (prepare_computation n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;
end ;;

let og = Oslo.get ;;
let see = Oslo.prepare_computation ;;

module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  ((selector,partitioned),(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition 
   (PW(old_affinities,old_parties,ll):t) parti  =
   let new_affinities =  Affinity.expand_using_partition old_affinities parti
   and new_parties = old_parties @ [parti]   in 
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_affinities,new_parties,new_ll):>t) ;;

let insert_affinity (PW(old_affinities,old_parties,ll)) affty  =
   PW(Affinity.insert affty old_affinities,old_parties,ll) ;;        
    

exception Missing_affinities of ( (int list) * colored_variable) list;;   

let check_combination_using_affinities combination (PW(affinities,_,_):t)=
  let (bad_ones,checked_version) = Affinity.analyse_combination affinities combination in 
  if bad_ones <> []
  then raise(Missing_affinities bad_ones)
  else checked_version;;

     

let expand_using_t combination (pw:t) =
   let (PW(_,_,ll)) = pw in 
   let ref_for_unknowns = ref [] in 
   let combination2 = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in
   let new_combination = check_combination_using_affinities combination2  pw in
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(affinities,parties,ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(affinities,parties,ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(affinities,parties,List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   "["^(String.concat ";" (Image.image string_of_int lamb))
   ^"],"^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW (_,_,ll)):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;


end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = List.partition (Vdw_chosen.test_joinability selector)  old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    if (a=[])||(b=[])
    then (a,b,None)  
    else
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (a,b,Some(Colored.variable_from_label lbl i,
              Colored.variable_from_label lbl j)) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW(Aff[],[],[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
])) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;
   

let partition selector cv =
    let old_state = (!main) in 
    let (a,b,opt_cvs) = Colored_variable_assignment.partition selector cv in 
    let new_state = (
    match opt_cvs with 
    None -> 
      let affty=((selector,cv),b=[]) in 
      Partition_watcher.insert_affinity old_state affty
    |Some(cv_i,cv_j) -> 
        Partition_watcher.expand_using_partition 
        old_state ((selector,cv),(cv_i,cv_j)) 
    ) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Oslo.prepare_computation n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

end ;;  

let oslo_compute = Current_partition_watcher.oslo_compute ;;


Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;


let passing = Current_partition_watcher.partition [16] (B 0) ;;
oslo_compute 16 0 ;;

let passing = Current_partition_watcher.partition [16] (C 0) ;;
oslo_compute 16 1 ;;

let passing = Current_partition_watcher.partition [17] (B 1) ;;
let passing = Current_partition_watcher.partition [17] (B 2) ;;
let passing = Current_partition_watcher.partition [16;17] (C 1) ;;

oslo_compute 17 0 ;;





(************************************************************************************************************************
Snippet 62 : Musings on the Van der Waerden problem, version 3 : adding
affinities and memorize each partition made in the partition watcher
************************************************************************************************************************)
open Needed_values ;;

type colored_variable = 
   A of int |B of int|C of int| Og of int * int ;;    
type partition_watcher = 
   PW of 
   (((int list * colored_variable) * bool)  list)*
   (((int list * colored_variable) * (colored_variable * colored_variable)) list) *
   ((colored_variable * (int list * colored_variable) list) list) ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;; 
let mea = Vdw_chosen.measure ;;
let is_adm = Vdw_chosen.test_for_admissibility ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

exception First_part_is_empty of int list ;;
exception Second_part_is_empty of int list ;;

let nonempty_partition selector ll = 
  let answer = List.partition (Vdw_chosen.test_joinability selector) ll in 
  if (fst answer) = [] then raise(First_part_is_empty selector) else 
  if (snd answer) = [] then raise(Second_part_is_empty selector) else  
  answer ;; 

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  

module Colored = struct 

exception Label_for_variable_exn ;;
let data_for_variable = function 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) 
    |(Og(_,_))->raise(Label_for_variable_exn) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d [0,A 0;1, B 0;2, C 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

end ;;  

module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let prepare_computation n d =
        let delt = mea(n)-mea(n-1) in   
        List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
        [
          [],(n-1,d-delt);
          [n],(n-1,d+1-delt)
        ];;   
let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (prepare_computation n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;
      
end ;;

let og = Oslo.get ;;
let see = Oslo.prepare_computation ;;

module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  ((selector,partitioned),(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition 
   (PW(old_affinities,old_parties,ll):t) parti  =
   let ((selector,partitioned),(part1,part2)) = parti in
   let new_affinities = old_affinities @ 
       [((selector,part1),true);
       ((selector,part2),false);] 
   and new_parties = old_parties @ [parti]   in 
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_affinities,new_parties,new_ll):>t) ;;

let expand_using_affinity pw affty  =
   let (PW(old_affinities,old_parties,ll)) = pw in 
   if List.mem affty old_affinities 
   then pw 
   else PW(affty :: old_affinities,old_parties,ll) ;;        
    

exception Missing_affinities of ( (int list) * colored_variable) list;;   

let check_combination_using_affinities combination (PW(affinities,_,_):t)=
  let temp0 = List.filter (
    fun (lamb,_)->Vdw_chosen.test_for_admissibility lamb
  ) combination in 
  let temp1 = Image.image (fun 
     pair -> 
      let res = (
       if fst pair = []
       then Some true
       else List.assoc_opt  pair affinities 
      ) in
      (pair,res)
  ) temp0 in 
  let temp2 = List.filter (fun (pair,opt)->opt=None) temp1 in 
  if temp2 <> []
  then raise(Missing_affinities(Image.image fst temp2))
  else 
  Option.filter_and_unpack 
  (fun (pair,opt)->if opt=Some true 
                   then Some pair 
                   else None 
   ) temp1 ;;

     

let expand_using_t combination (pw:t) =
   let (PW(_,_,ll)) = pw in 
   let ref_for_unknowns = ref [] in 
   let combination2 = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in
   let new_combination = check_combination_using_affinities combination2  pw in
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(affinities,parties,ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(affinities,parties,ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(affinities,parties,List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   "["^(String.concat ";" (Image.image string_of_int lamb))
   ^"],"^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW (_,_,ll)):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;


end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = List.partition (Vdw_chosen.test_joinability selector)  old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    if (a=[])||(b=[])
    then (a,b,None)  
    else
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (a,b,Some(Colored.variable_from_label lbl i,
              Colored.variable_from_label lbl j)) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW([],[],[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
])) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;
   

let partition selector cv =
    let old_state = (!main) in 
    let (a,b,opt_cvs) = Colored_variable_assignment.partition selector cv in 
    let new_state = (
    match opt_cvs with 
    None -> 
      let affty=((selector,cv),b=[]) in 
      Partition_watcher.expand_using_affinity old_state affty
    |Some(cv_i,cv_j) -> 
        Partition_watcher.expand_using_partition 
        old_state ((selector,cv),(cv_i,cv_j)) 
    ) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Oslo.prepare_computation n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

end ;;  

let oslo_compute = Current_partition_watcher.oslo_compute ;;


Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;


let passing = Current_partition_watcher.partition [16] (B 0) ;;
oslo_compute 16 0 ;;

let passing = Current_partition_watcher.partition [16] (C 0) ;;
oslo_compute 16 1 ;;

let passing = Current_partition_watcher.partition [17] (B 1) ;;
let passing = Current_partition_watcher.partition [17] (B 2) ;;
let passing = Current_partition_watcher.partition [16;17] (C 1) ;;
oslo_compute 17 0 ;;





(************************************************************************************************************************
Snippet 61 : Musings on the Van der Waerden problem, version 2 : adding a partition watcher
************************************************************************************************************************)
open Needed_values ;;

type colored_variable = 
   A of int |B of int|C of int| Og of int * int ;;    
type partition_watcher = 
   PW of (colored_variable * (int list * colored_variable) list) list ;;   

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;;
let mea = Vdw_chosen.measure ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

exception First_part_is_empty of int list ;;
exception Second_part_is_empty of int list ;;

let nonempty_partition selector ll = 
  let answer = List.partition (Vdw_chosen.test_joinability selector) ll in 
  if (fst answer) = [] then raise(First_part_is_empty selector) else 
  if (snd answer) = [] then raise(Second_part_is_empty selector) else  
  answer ;; 

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;
  

module Colored = struct 

exception Label_for_variable_exn ;;
let data_for_variable = function 
    (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) 
    |(Og(_,_))->raise(Label_for_variable_exn) ;;
  
exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
     if lbl = "A" then A k else 
     if lbl = "B" then B k else  
     if lbl = "C" then C k else 
     raise(Variable_from_label_exn) ;;  

let is_an_og = function (Og(_,_)) -> true | _ -> false ;;

let usual_rewriting cv= match cv with  
 (Og(n,d)) ->
     if n <> wall_position then cv else 
     (match List.assoc_opt d [0,A 0;1, B 0;2, C 0] with 
           (Some cv2) -> cv2 
           | None -> cv)  
  | _ ->cv ;;

let to_string cv= match cv with  
  (Og(n,d)) -> "Og("^(string_of_int n)^","^(string_of_int d)^")" 
  | _ -> let (lbl,k) = data_for_variable cv in 
         lbl^(string_of_int k) ;;  

end ;;  

module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let prepare_computation n d =
        let delt = mea(n)-mea(n-1) in   
        List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
        [
          [],(n-1,d-delt);
          [n],(n-1,d+1-delt)
        ];;   
let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (prepare_computation n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;
end ;;

let og = Oslo.get ;;
let see = Oslo.prepare_computation ;;

module Partition_watcher = struct 

type t = partition_watcher ;; 

let expand_using_partition_in_list 
  (partitioned,(part1,part2)) l =
  List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    if cvar = partitioned 
    then [lamb,part1;lamb,part2]
    else [old_pair]
  ) l);;

let expand_using_partition (PW(ll):t) parti  =
   let new_ll = Image.image (fun (x,y)->
       (x,expand_using_partition_in_list parti y)
   ) ll in 
   (PW(new_ll):>t) ;;

let expand_using_t combination (PW(ll):t)   =
   let ref_for_unknowns = ref [] in 
   let new_combination = List.flatten(Image.image (fun old_pair->
    let (lamb,cvar) = old_pair in 
    match List.assoc_opt (Colored.usual_rewriting cvar) ll with 
     (Some expansion) -> Image.image (fun
      (lamb2,cvar2)-> 
      (Ordered.merge oi lamb lamb2,cvar2)) expansion
    |None -> let _ = (ref_for_unknowns:=cvar::(!ref_for_unknowns)) in 
             [old_pair]
    ) combination) in 
   (new_combination,!ref_for_unknowns) ;;


let force_insert_in new_pair (PW(ll):t) =
   if Colored.is_an_og (fst new_pair) 
   then (PW(ll@[new_pair]):>t)
   else  
   let (rev_before,opt_og,incomplete_after) = 
     Three_parts.select_center_element_and_reverse_left
      (fun (expanded_one,_) -> 
        Colored.is_an_og expanded_one
      ) ll in 
   let full_after = (match opt_og with 
      (Some og_elt) -> og_elt :: incomplete_after 
      | None -> incomplete_after 
   ) in
   (PW(List.rev_append rev_before (new_pair :: full_after)):>t);; 

exception Insert_carefully_exn of colored_variable list ;;

let insert_carefully (x,old_expansion_for_x) pw =
   let (new_expansion_for_x,unknowns) = 
     expand_using_t old_expansion_for_x pw in 
   if unknowns <> []   
   then raise(Insert_carefully_exn(unknowns))
   else force_insert_in (x,new_expansion_for_x) pw ;; 

let subitem_to_string (lamb,cv) =
   "["^(String.concat ";" (Image.image string_of_int lamb))
   ^"],"^(Colored.to_string cv);;

let pw_list_to_string expansion =
   "["^(String.concat ";" (Image.image subitem_to_string expansion))^"]";;

let item_to_string (expanded_one,expansion) = 
   (String.make 3 ' ')^"("^
    (Colored.to_string expanded_one)^","^(pw_list_to_string expansion)^")" ;;

let print ((PW ll):t) = 
   let temp1 = String.concat "\n" (Image.image item_to_string ll) in 
   let temp2 = "\n\n\n[\n" ^ temp1 ^ "\n]\n\n\n" in 
   print_string temp2 ;;


end ;;  


module Colored_variable_assignment = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
] ;;


   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = Colored.data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = nonempty_partition selector old_val 
    and (lbl,_) = Colored.data_for_variable colored_var in 
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (Colored.variable_from_label lbl i,
     Colored.variable_from_label lbl j,a,b) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  

module Current_partition_watcher = struct 

let main = ref (PW[
       (A 0),[[],A 0];
       (B 0),[[],B 0];
       (C 0),[[],C 0];
]) ;;

let set_and_show new_state = (
  main := new_state ;
  Partition_watcher.print new_state 
) ;;


let partition selector cv =
    let old_state = (!main) in 
    let (cv_i,cv_j,a,b) = Colored_variable_assignment.partition selector cv in 
    let new_state = Partition_watcher.expand_using_partition 
        old_state (cv,(cv_i,cv_j)) in 
    let _ = set_and_show new_state in 
    (a,b) ;;
        
let oslo_compute n d=         
    let res = Oslo.compute n d in 
    let skeleton = Image.image (
      fun (lamb,(nn,dd))->(lamb,Og(nn,dd))
    ) (Oslo.prepare_computation n d) in 
    let old_state = (!main) in 
    let new_state = Partition_watcher.insert_carefully 
       (Og(n,d),skeleton) old_state in 
    let _ = set_and_show new_state in   
    res ;; 

let print () = Partition_watcher.print (!main) ;;

end ;;  

let oslo_compute = Current_partition_watcher.oslo_compute ;;


Oslo.add (15,0) (Colored_variable_assignment.get (A 0)) ;;
Oslo.add (15,1) (Colored_variable_assignment.get (B 0)) ;;
Oslo.add (15,2) (Colored_variable_assignment.get (C 0)) ;;

let passing = Current_partition_watcher.partition [16] (B 0) ;;
oslo_compute 16 0 ;;

let passing = Current_partition_watcher.partition [16] (C 0) ;;
oslo_compute 16 1 ;;



let passing = Current_partition_watcher.partition [17] (B 1) ;;
let passing = Current_partition_watcher.partition [17] (B 2) ;;
let passing = Current_partition_watcher.partition [16;17] (C 1) ;;
oslo_compute 17 0 ;;




(************************************************************************************************************************
Snippet 60 : Musings on the Van der Waerden problem, version 2 : adding colored variables
************************************************************************************************************************)
open Needed_values ;;

type colored_variable = 
   A of int |B of int|C of int| Og of int * int ;;    

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;;
let mea = Vdw_chosen.measure ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;; 

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

exception First_part_is_empty of int list ;;
exception Second_part_is_empty of int list ;;

let nonempty_partition selector ll = 
  let answer = List.partition (Vdw_chosen.test_joinability selector) ll in 
  if (fst answer) = [] then raise(First_part_is_empty selector) else 
  if (snd answer) = [] then raise(Second_part_is_empty selector) else  
  answer ;; 

module Oslo = struct 

let hashtbl_for_oslo = Hashtbl.create 30;;
let add = Hashtbl.add hashtbl_for_oslo ;;
exception Get_exn of int * int ;;
let get n d =
       let m = Vdw_chosen.measure(n)-d in 
       if (m<0)||(d<0)
       then []
       else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
             _ -> raise (Get_exn(n,d));;

let prepare_computation n d =
        let delt = mea(n)-mea(n-1) in   
        List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
        [
          [],(n-1,d-delt);
          [n],(n-1,d+1-delt)
        ];;   
let compute n d =
      let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,get nn dd)) 
         (prepare_computation n d) in    
      let result = checked_combination temp1 in 
      let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
      result ;;
end ;;

let og = Oslo.get ;;
let see = Oslo.prepare_computation ;;

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
(Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;


module Colored = struct 

type t = colored_variable ;;

let storage = ref [
   "A",[List.filter (fun x->(List.length x)=8) wall];
   "B",[List.filter (fun x->(List.length x)=7) wall];
   "C",[List.filter (fun x->(List.length x)=6) wall];
] ;;

exception Label_for_variable_exn ;;
let data_for_variable = function 
  (A k) ->("A",k) |(B k) ->("B",k) |(C k) ->("C",k) 
  |(Og(_,_))->raise(Label_for_variable_exn) ;;

exception Variable_from_label_exn ;;  
let variable_from_label lbl k=
   if lbl = "A" then A k else 
   if lbl = "B" then B k else  
   if lbl = "C" then C k else 
   raise(Variable_from_label_exn) ;;
   
exception Get_exn of t ;;

let get colored_var =
    match colored_var with 
    Og(n,d) ->Oslo.get n d
    | _ -> let (lbl,k) = data_for_variable colored_var in 
           try List.nth (List.assoc lbl (!storage)) k with 
           _ -> raise(Get_exn(colored_var));; 


let add_new lbl wahl =
   let old_list = List.assoc lbl (!storage) in    
   let new_list = old_list @ [wahl] in 
   let new_storage = Image.image (
     fun old_pair ->
       if (fst old_pair)=lbl 
       then (lbl,new_list)
       else old_pair  
   ) (!storage) in 
   let _=(storage:=new_storage) in 
   List.length old_list (* index of newly created entry *) ;;
   
let partition selector colored_var =
    let old_val = get colored_var in 
    let (a,b) = nonempty_partition selector old_val 
    and (lbl,_) = data_for_variable colored_var in 
    let i = add_new lbl a in 
    let j = add_new lbl b in
    (i,j,a,b) ;; 

let check_partitioning ll=
   let temp1 = Image.image (fun (x,ly)->
     (get x,Image.image (fun (lamb,y)->(lamb,get y)) ly )
    ) ll in 
    check_correct_partitioning temp1 ;;

end ;;  


Oslo.add (15,0) (Colored.get (A 0)) ;;
Oslo.add (15,1) (Colored.get (B 0)) ;;
Oslo.add (15,2) (Colored.get (C 0)) ;;

let passing = Colored.partition [16] (B 0) ;;
Oslo.compute 16 0 ;;

let check1 = Colored.check_partitioning [
     (B 0), [[],B 1;[],B 2];
     Og(16,0), [[], A 0;[16], B 1];
] ;;

let passing = Colored.partition [16] (C 0) ;;
Oslo.compute 16 1 ;;

let check2 = Colored.check_partitioning [
     (B 0), [[],B 1;[],B 2];
     (C 0), [[],C 1;[],C 2];
     Og(16,0), [[],A 0;[16],B 1];
     Og(16,1), [[],B 1;[],B 2;[16],C 1];
] ;;


let passing = Colored.partition [17] (B 1) ;;
let passing = Colored.partition [17] (B 2) ;;
let passing = Colored.partition [16;17] (C 1) ;;
Oslo.compute 17 0 ;;

let check3 = Colored.check_partitioning [
     (B 0), [[],B 3;[],B 4;[],B 5;[],B 6];
     (C 0), [[],C 2;[],C 3;[],C 4];
     Og(16,0), [[],A 0;[16],B 3;[16],B 4];
     Og(16,1), [[],B 3;[],B 4;[],B 5;[],B 6;[16],C 3;[16],C 4];
     Og(17,0), [[],A 0;[16],B 3;[16],B 4;[17],B 3;[17],B 5;[16;17],C 3];
] ;;



(************************************************************************************************************************
Snippet 59 : Musings on the Van der Waerden problem  
************************************************************************************************************************)
open Needed_values ;;

let oi = Vdw_preliminaries.oint ;;
let oil = Vdw_preliminaries.ointlist ;;
let mea = Vdw_chosen.measure ;;

let dot lamb ll = 
    Ordered.safe_set oil
  (Image.image (Ordered.merge oi lamb) ll) ;;

let combination ll =
    let temp1 = Image.image (fun (lamb,sl)->dot lamb sl) ll in 
    Ordered.fold_merge oil temp1 ;;     

let checked_combination ll =
   List.filter Vdw_chosen.test_for_admissibility (combination ll) ;;

let sel selector ll =
    List.partition (Vdw_chosen.test_joinability selector) ll;;    

let check_correct_partitioning =
    List.for_all (
      fun (whole,parts)->whole = combination parts
    ) ;;

let rps n = Vdw_precomputed.restricted_power_set 
(Vdw_max_width_t.MW 4,Ennig.ennig 1 n) ;;

let hashtbl_for_oslo = Hashtbl.create 30;;
let oslo_add = Hashtbl.add hashtbl_for_oslo ;;
exception Oslo_get_exn of int * int ;;
let oslo_get n d =
   let m = Vdw_chosen.measure(n)-d in 
   if (m<0)||(d<0)
   then []
   else  try Hashtbl.find  hashtbl_for_oslo (n,d) with 
         _ -> raise (Oslo_get_exn(n,d));;
let og = oslo_get ;;
let see n d =
    let delt = mea(n)-mea(n-1) in   
    List.filter (fun (lamb,(m,dee))->(dee<=mea(m))&&(dee>=0) )
    [
      [],(n-1,d-delt);
      [n],(n-1,d+1-delt)
    ];;   
let oslo_compute n d =
  let temp1 = Image.image (fun (lamb,(nn,dd))->(lamb,oslo_get nn dd)) (see n d) in    
  let result = checked_combination temp1 in 
  let _ = Hashtbl.add hashtbl_for_oslo (n,d) result in 
  result ;;

    

let computation1 () = Ennig.doyle (fun n->
  List.length(rps n) 
) 1 15;;

let timed_computation1 () = Chronometer.it computation1 ();;

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
(Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;

let aa0 = List.filter (fun x->(List.length x)=8) wall ;;
let bb0 = List.filter (fun x->(List.length x)=7) wall ;;
let cc0 = List.filter (fun x->(List.length x)=6) wall ;;

oslo_add (15,0) aa0 ;;
oslo_add (15,1) bb0 ;;
oslo_add (15,2) cc0 ;;

let (bb1,bb2) = sel [16] bb0 ;;

oslo_compute 16 0 ;;

let check1 = check_correct_partitioning [
     bb0, [[],bb1;[],bb2];
     og 16 0, [[],aa0;[16],bb1];
] ;;

let (cc1,cc2) = sel [16] cc0 ;;

oslo_compute 16 1 ;;

let check2 = check_correct_partitioning [
     bb0, [[],bb1;[],bb2];
     cc0, [[],cc1;[],cc2];
     og 16 0, [[],aa0;[16],bb1];
     og 16 1, [[],bb1;[],bb2;[16],cc1];
] ;;

let (bb3,bb4) = sel [17] bb1 ;;
let (bb5,bb6) = sel [17] bb2 ;;
let (cc3,cc4) = sel [16;17] cc1 ;;

oslo_compute 17 0;;

let check3 = check_correct_partitioning [
     bb0, [[],bb3;[],bb4;[],bb5;[],bb6];
     cc0, [[],cc2;[],cc3;[],cc4];
     og 16 0, [[],aa0;[16],bb3;[16],bb4];
     og 16 1, [[],bb3;[],bb4;[],bb5;[],bb6;[16],cc3;[16],cc4];
     og 17 0, [[],aa0;[16],bb3;[16],bb4;[17],bb3;[17],bb5;[16;17],cc3];
] ;;

(*

([],aa0) = sel [18] aa0 ;;
let (bb7,bb8) = sel [16;18] bb3 ;;
let (bb9,bb10) = sel [16;18] bb4 ;;
let (bb11,bb12) = sel [17;18] bb5 ;;


let check4 = check_correct_partitioning [
     bb0, [[],bb6;[],bb7;[],bb8;[],bb9;[],bb10;[],bb11;[],bb12;];
     cc0, [[],cc2;[],cc3;[],cc4];
     og 16 0, [[],aa0;[16],bb7;[16],bb8;[16],bb9;[16],bb10];
     og 16 1, [[],bb6;[],bb7;[],bb8;[],bb9;[],bb10;[],bb11;[],bb12;[16],cc3;[16],cc4];
     og 17 0, [[],aa0;[16],bb3;[16],bb4;[17],bb3;[17],bb11;[17],bb12;[16;17],cc3];
] ;;

*)

(************************************************************************************************************************
Snippet 58 : Compute summaries for levels, in a Van der Waerden context
************************************************************************************************************************)
open Needed_values ;;


let oi = Vdw_preliminaries.oint ;;
let rps n = Vdw_precomputed.restricted_power_set 
(Vdw_max_width_t.MW 4,Ennig.ennig 1 n) ;;

let computation1 () = Ennig.doyle (fun n->
  List.length(rps n) 
) 1 15;;

let timed_computation1 () = Chronometer.it computation1 ();;

let wall_position = 15 ;;

let wall = Vdw_precomputed.restricted_power_set 
(Vdw_max_width_t.MW 4,Ennig.ennig 1 wall_position) ;;

let aa0 = List.filter (fun x->(List.length x)=8) wall ;;
let bb0 = List.filter (fun x->(List.length x)=7) wall ;;
let cc0 = List.filter (fun x->(List.length x)=6) wall ;;

let result_for_level1=
[ [9; 13]; [10; 13]; [11; 14]; [11; 15]; [12; 15]; [14; 15];  ] ;;
let result_for_level0 = [[10; 11; 13; 14];[10; 12; 13; 15]; [10; 11; 14; 15]; [11; 12; 14; 15]; 
];;

let check_result_for_level1 = 
    List.filter (fun z->List.for_all(fun y->
        not(Ordered.is_included_in oi y z)
      ) result_for_level1) bb0 ;;

let check_result_for_level0 = 
    List.filter (fun z->List.for_all(fun y->
            not(Ordered.is_included_in oi y z)
    ) result_for_level0) aa0 ;;      


let current_d = 0 ;;
let current_m = (Vdw_chosen.measure wall_position)-current_d ;;
let current_wall_part = List.filter (fun x->
  (List.length x)=current_m) wall ;;

let unexpected_impossibilities r = 
   let bound = (Vdw_chosen.measure (wall_position+r)) - current_m in 
   let temp1 = List.filter (fun l->List.length(l)>bound) (rps r) in 
   Image.image (Image.image (fun t->t+wall_position)) temp1;;

let y1 = List.flatten (Ennig.doyle unexpected_impossibilities 1 10);;
let y2 = Ordered.select_minimal_elements_for_inclusion 
           Total_ordering.for_integers y1 ;;
     
let z1 = Vdw_chosen.minimal_obstructions (wall_position+1) 
(Set_of_integers.safe_set [18; 20; 21]) ;;
let z2 = Ennig.index_everything z1 ;;
let shadow x =
    Option.filter_and_unpack (
      fun (j,forb)->
        if Ordered.is_included_in Total_ordering.for_integers forb x
        then Some j 
        else None  
    ) z2 ;;
let z3 = Image.image shadow current_wall_part ;;    
let z4 = Image.image Set_of_integers.safe_set z3 ;;
let z5 = Ordered_misc.minimal_transversals z4 ;;


let base1 = [[14; 15]; [10; 13]];;
let base2 = [[13; 15]; [11; 14]] ;;

let base1 = [ [11; 14; 15]; [10; 13; 15]; [10; 11; 13; 14]];;
let base2 = [ [12; 15]; [10; 14]] ;;

let base1 = [[11; 12; 14; 15]; [10; 11; 14; 15]; [10; 12; 13; 15];
[10; 11; 13; 14]];;


let u1 = Cartesian.product base1 base2 ;;
let u2 = Image.image (fun (x,y)->Ordered.merge Total_ordering.for_integers x y) u1;;
let u3 = Ordered.select_minimal_elements_for_inclusion 
Total_ordering.for_integers u2 ;;


let result1=
[ [9; 13]; [10; 13]; [11; 14]; [11; 15]; [12; 15]; [14; 15];  ] ;;
let result2= [[10; 11; 13; 14];[10; 12; 13; 15]; [10; 11; 14; 15]; [11; 12; 14; 15]; 
];;


(************************************************************************************************************************
Snippet 57 : Remove all modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd = Dfa_subdirectory.of_line "Old_Van_der_Waerden";;
let z1 = ae () ;;
let z2 = List.filter (
  fun (Dfn_endingless_t.J(r,s,m)) ->
    Dfa_subdirectory.begins_with s sd
) z1 ;;
let z3 = Image.image (
   fun (Dfn_endingless_t.J(r,s,m)) ->
    Dfa_module.to_line m
  ) z2 ;;
let act () = fgs z3 ;;  

(************************************************************************************************************************
Snippet 56 : Replacing a long interval in a file with another
************************************************************************************************************************)
open Needed_values ;;

let ap1 = Absolute_path.of_string "../Idaho/Compilation_management/coma_state.ml" ;;
let ap1_text = Io.read_whole_file ap1 ;;
let to_be_replaced = Lines_in_string.interval ap1_text 1 676 ;;

let towards_complement = rf "Fads/pan.ml";;
let replacement = Lines_in_string.interval towards_complement 9 240 ;;

let act7 () = Replace_inside.replace_inside_file (to_be_replaced,replacement) ap1;;

(************************************************************************************************************************
Snippet 55 : Long debugging session on the rename_module functionality (as in snippet 54), with
use of rsh to initialize
************************************************************************************************************************)
open Needed_values ;;

rsh();;

let (case1,case2) = ("dfa_subdirectory","afd_sybdirectoru") ;;
let cases = [ case1 ; case2 ] ;;

let see_cases () = Image.image (fun s->
  Sys.file_exists("Decomposed_filename/"^s^".ml")) cases ;;

let (current_name,other_name) = 
  let sc = see_cases () in 
  if sc = [false;true] then (case2,case1) else 
  if sc = [true;false] then (case1,case2) else 
  failwith("Unforeseen case");;  

let mn = Dfa_module.of_line(String.uncapitalize_ascii current_name) ;;
let old_eless = Coma_state.endingless_at_module (!ucs) mn ;;
let old_middle_name = Dfn_endingless.to_middle old_eless ;; 
let new_nonslashed_name = No_slashes.of_string (String.uncapitalize_ascii other_name) ;;
let tcs = (!ucs) ;;
let msg="rename "^(Dfa_module.to_line(Dfn_middle.to_module old_middle_name))^
  " as "^(No_slashes.to_string new_nonslashed_name) ;;
let old_nm=Dfn_middle.to_module old_middle_name ;;
let new_nm=Dfa_module.of_line (No_slashes.to_string new_nonslashed_name) ;;
let separated_acolytes_below=Option.filter_and_unpack(
      fun mn->
       if List.mem old_nm (Coma_state.ancestors_at_module tcs mn)
      then Some(Image.image (Dfn_full.to_rootless) (Coma_state.acolytes_at_module tcs mn))
      else None
) (Coma_state.ordered_list_of_modules tcs) ;;
let all_acolytes_below=List.flatten separated_acolytes_below ;;
let old_fw = Coma_state.frontier_with_unix_world tcs ;;
let old_pal = Fw_with_dependencies.Private.parent old_fw;;
let (bad1,_) = 
    Fw_with_small_details.rename_module old_pal
     (old_nm,new_nm,all_acolytes_below) ;;
  
module Pri  = Fw_with_small_details.Private ;;
module Auto = Fw_with_small_details.Automatic ;;

let (old_module,new_module,files_to_be_rewritten) = (old_nm,new_nm,all_acolytes_below) ;;

(*
let (bad2,_) = Pri.rename_module_on_filename_level old_pal (old_module,new_module) ;;

let (post_bad2,_) = Pri.rename_module_on_content_level bad2 
   (old_module,new_module) files_to_be_rewritten ;;
*)


(************************************************************************************************************************
Snippet 54 : Long debugging session on the rename_module functionality.
************************************************************************************************************************)
open Needed_values ;;

let (case1,case2) = ("dfa_subdirectory","afd_sybdirectoru") ;;
let cases = [ case1 ; case2 ] ;;

let see_cases () = Image.image (fun s->
  Sys.file_exists("Decomposed_filename/"^s^".ml")) cases ;;

let (current_name,other_name) = 
  let sc = see_cases () in 
  if sc = [false;true] then (case2,case1) else 
  if sc = [true;false] then (case1,case2) else 
  failwith("Unforeseen case");;  

(* let bad0 = ren current_name other_name ;;  *)

(* let bad1 = Usual_coma_state.rename_module current_name other_name ;;  *)

(* let bad2 = Modify_coma_state.Syntactic_sugar.rename_module ucs current_name other_name ;; *)

let mn = Dfa_module.of_line(String.uncapitalize_ascii current_name) ;;
let old_eless = Coma_state.endingless_at_module (!ucs) mn ;;
let old_middle_name = Dfn_endingless.to_middle old_eless ;; 
let new_nonslashed_name = No_slashes.of_string (String.uncapitalize_ascii other_name) ;;


(* let bad3 = Modify_coma_state.Reference.rename_module ucs old_middle_name new_nonslashed_name;; *)

let tcs = (!ucs) ;;

(*

let bad4 = Modify_coma_state.And_save.rename_module tcs old_middle_name new_nonslashed_name;; 

let post_bad4=(ucs:=bad4) ;; 

*)

(*
let bad5 = Modify_coma_state.And_backup.rename_module tcs old_middle_name new_nonslashed_name;; 


let post_bad5=(
  Save_coma_state.save bad5 ;
  ucs:=bad5) ;;
*)  

let msg="rename "^(Dfa_module.to_line(Dfn_middle.to_module old_middle_name))^
  " as "^(No_slashes.to_string new_nonslashed_name) ;;

(*
let bad6=Modify_coma_state.Physical_followed_by_internal.rename_module tcs old_middle_name new_nonslashed_name ;;

let post_bad6=(
  let bud = Coma_state.reflect_latest_changes_in_github bad6 (Some msg) in 
  Save_coma_state.save bud ;
  ucs:=bud) ;;
*)

(*
let (bad7,_)= Modify_coma_state.Physical.rename_module tcs old_middle_name new_nonslashed_name ;;


let post_bad7=(
  let bart=Modify_coma_state.Internal.rename_module bad7 old_middle_name new_nonslashed_name in 
  let bud = Coma_state.reflect_latest_changes_in_github bart (Some msg) in 
  Save_coma_state.save bud ;
  ucs:=bud) ;;
*)
  
let old_nm=Dfn_middle.to_module old_middle_name ;;
let new_nm=Dfa_module.of_line (No_slashes.to_string new_nonslashed_name) ;;
let separated_acolytes_below=Option.filter_and_unpack(
      fun mn->
       if List.mem old_nm (Coma_state.ancestors_at_module tcs mn)
      then Some(Image.image (Dfn_full.to_rootless) (Coma_state.acolytes_at_module tcs mn))
      else None
) (Coma_state.ordered_list_of_modules tcs) ;;
let all_acolytes_below=List.flatten separated_acolytes_below ;;
let old_fw = Coma_state.frontier_with_unix_world tcs ;;

(*
let (bad8,_) = 
  Fw_with_dependencies.rename_module_on_filename_level_and_in_files old_fw 
   (old_nm,new_nm,all_acolytes_below) ;;

let post_bad8=(
    let bag = Coma_state.set_frontier_with_unix_world tcs bad8 in
    let bart=Modify_coma_state.Internal.rename_module bag old_middle_name new_nonslashed_name in 
    let bud = Coma_state.reflect_latest_changes_in_github bart (Some msg) in 
    Save_coma_state.save bud ;
    ucs:=bud) ;;
*)    
  



(************************************************************************************************************************
Snippet 53 : Visualize Git tree
************************************************************************************************************************)
open Needed_values ;;

let gc = "git -C "^home^"/Teuliou/OCaml/Idaho_backup ";;

let cmd_for_z0 = gc ^ "ls-tree -r HEAD > ~/Downloads/temp.txt";;
let z0 = Sys.command cmd_for_z0 ;;

let z1 = rf "~/Downloads/temp.txt";;
let z2 = Lines_in_string.lines z1 ;;
let z3 = List.filter (fun line->
   Supstring.contains line "depth_one"
  ) z2;;
let z4 = Image.image (Cull_string.cobeginning 53) z3;;


let cmds1 = Image.image (fun x->gc^"rm --cached "^x) z4 ;;
let cmds2 = Image.image (fun x->
  let cx = String.capitalize_ascii x in
  gc^"add "^cx) z4 ;;
let cmds3 = cmds1 @ cmds2 ;;
let anse1 = Image.image Sys.command cmds3 ;;

(************************************************************************************************************************
Snippet 52 : Long debugging session on the rename_subdir functionality,
with just one main function ("compressed" version of snippet 51)
************************************************************************************************************************)
open Needed_values ;;

module Pri = Reflect_change_in_github.Private ;;

let special_doyle f i j=
  let temp1 = Ennig.ennig i j in 
  Image.image f temp1 ;;

let peggy () = 
   let _ = rsh() in 
   let (case1,case2) = 
    ("Depth_two_testdir","Dopth_twe_tistder") in 
   let cases = [ case1 ; case2 ] in 
   let sc = Image.image (fun s->
      Sys.file_exists("Depth_one_testdir/"^s)) cases in
   let (current_name,other_name) = (
    if sc = [false;true] then (case2,case1) else 
    if sc = [true;false] then (case1,case2) else 
    failwith("Unforeseen case")) in   
   let old_subdirname = "Depth_one_testdir/"^current_name 
   and new_subdir_short_name = other_name in 
   let old_subdir = Coma_state.find_subdir_from_suffix (!ucs) old_subdirname  in
   let new_subdir = Coma_state.compute_long_subdir_name (!ucs) old_subdir new_subdir_short_name  in
   let tcs = !ucs in 
   let cs2=Modify_coma_state.After_checking.rename_subdirectory tcs old_subdir new_subdir  in
   let msg="rename "^(Dfa_subdirectory.connectable_to_subpath old_subdir)^
   " as "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
   let fw_with_dep = cs2.Coma_state_t.frontier_with_unix_world in 
   let fw_with_sd = fw_with_dep.Fw_with_dependencies_t.parent in 
   let fw_the_first = fw_with_sd.Fw_with_small_details_t.parent in 
   let config = fw_the_first.File_watcher_t.configuration in 
   let diff = fw_the_first.File_watcher_t.last_noticed_changes in 
   let destination_dir = config.Fw_configuration_t.dir_for_backup in
   let (nongit_cmds,git_cmds) = Pri.commands_for_backup config diff in 
   let s_destination=Dfa_root.connectable_to_subpath destination_dir in 
   let _ =Image.image Unix_command.uc nongit_cmds in   
   let cwd=Sys.getcwd() in 
   let final_cmds = 
   (
    [Unix_command.cd s_destination]@   
    git_cmds@   
    [
      "git commit -m \""^msg^"\""
    ]@
    [Unix_command.cd cwd]
    ) in      
  let whole () = Image.image Sys.command final_cmds in 
  let fc_get  = (fun k->List.nth final_cmds (k-1)) in
  let fc k = 
   let cmd = fc_get k in 
   (Sys.command cmd,cmd) in
  let first_half () = special_doyle (fun k->fst(fc k)) 1 9 in
  let second_half () = special_doyle (fun k->fst(fc k)) 10 19 in 
  (first_half,second_half,whole,fc_get,fc) ;;


let (first_half,second_half,whole,fc_get,fc) = peggy ();;

(*
let backup_with_message config  diff msg=
  let destination_dir = config.Fw_configuration_t.dir_for_backup in 
  let (nongit_cmds,git_cmds)=commands_for_backup config diff in
  let s_destination=Dfa_root.connectable_to_subpath destination_dir in
  let _=Image.image Unix_command.uc nongit_cmds in
  let _=(
  if config.Fw_configuration_t.gitpush_after_backup
  then let cwd=Sys.getcwd() in
       Image.image Unix_command.uc
       (
       [Unix_command.cd s_destination]@   
       git_cmds@   
       [
         "git commit -m \""^msg^"\"";
         "git push"
       ]@
       [Unix_command.cd cwd]
       ) 
  else let cwd=Sys.getcwd() in
       Image.image Unix_command.uc
       (
       [Unix_command.cd s_destination]@   
       git_cmds@   
       [
         "git commit -m \""^msg^"\""
       ]@
       [Unix_command.cd cwd]
       ) 
  ) in
  ();;
  *)

(*  
 
git -C /Users/ewandelanoy/Teuliou/OCaml/Idaho_backup/ clean -n 
git -C /Users/ewandelanoy/Teuliou/OCaml/Idaho_backup/ clean -f -d

*)


(*  
 
git -C /Users/ewandelanoy/Teuliou/OCaml/Idaho_backup/ reset HEAD depth_one_testdir/Depth_two_testdir/Depth_three_testdir/example.txt 
git -C /Users/ewandelanoy/Teuliou/OCaml/Idaho_backup/ reset HEAD depth_one_testdir/Depth_two_testdir/Depth_three_testdir/*.ml 
git -C /Users/ewandelanoy/Teuliou/OCaml/Idaho_backup/ reset HEAD depth_one_testdir/Depth_two_testdir/tested_module_five.ml 
git -C /Users/ewandelanoy/Teuliou/OCaml/Idaho_backup/ status

*)



(*
git -C /Users/ewandelanoy/Teuliou/OCaml/Idaho_backup/ rm Depth_one_testdir/Depth_two_testdir/Depth_three_testdir/testable_executable.ml
file /Users/ewandelanoy/Teuliou/OCaml/Idaho_backup/Depth_one_testdir/Depth_two_testdir/Depth_three_testdir/testable_executable.ml
git -C /Users/ewandelanoy/Teuliou/OCaml/Idaho_backup/ add Depth_one_testdir/Depth_two_testdir/Depth_three_testdir/testable_executable.ml

*)

(*
git -C /Users/ewandelanoy/Teuliou/OCaml/Idaho_backup/ rm Depth_one_testdir/Dopth_twe_tistder/Depth_three_testdir/testable_executable.ml
file /Users/ewandelanoy/Teuliou/OCaml/Idaho_backup/Depth_one_testdir/Dopth_twe_tistder/Depth_three_testdir/testable_executable.ml
git -C /Users/ewandelanoy/Teuliou/OCaml/Idaho_backup/ add Depth_one_testdir/Dopth_twe_tistder/Depth_three_testdir/testable_executable.ml

*)

(*

git -C /Users/ewandelanoy/Teuliou/OCaml/Idaho_backup/ config core.longpaths true
git -C /Users/ewandelanoy/Teuliou/OCaml/Idaho_backup/ add .

*)

(************************************************************************************************************************
Snippet 51 : Long debugging session on the rename_subdir functionality
************************************************************************************************************************)
open Needed_values ;;

rsh();;

let (case1,case2) = ("Depth_two_testdir","Dopth_twe_tistder") ;;
let cases = [ case1 ; case2 ] ;;

let see_cases () = Image.image (fun s->
  Sys.file_exists("Depth_one_testdir/"^s)) cases ;;

let (current_name,other_name) = 
  let sc = see_cases () in 
  if sc = [false;true] then (case2,case1) else 
  if sc = [true;false] then (case1,case2) else 
  failwith("Unforeseen case");;    

   
let old_subdirname = "Depth_one_testdir/"^current_name ;;
let new_subdir_short_name = other_name ;;

(*

let bad0 = rensub old_subdirname new_subdir_short_name ;;

*)

let old_subdir = Coma_state.find_subdir_from_suffix (!ucs) old_subdirname  ;;
let new_subdir = Coma_state.compute_long_subdir_name (!ucs) old_subdir new_subdir_short_name  ;;

let tcs = !ucs ;;

(*
let bad1 = Modify_coma_state.And_backup.rename_subdirectory tcs old_subdir new_subdir;;
*)

let cs2=Modify_coma_state.After_checking.rename_subdirectory tcs old_subdir new_subdir  ;;
let msg="rename "^(Dfa_subdirectory.connectable_to_subpath old_subdir)^
" as "^(Dfa_subdirectory.connectable_to_subpath new_subdir) ;;

(* let bad2= Coma_state.reflect_latest_changes_in_github cs2 (Some msg) ;;  *)

let fw_with_dep = cs2.Coma_state_t.frontier_with_unix_world ;;

(* let bad3 = Fw_with_dependencies.reflect_latest_changes_in_github fw_with_dep (Some msg) ;; *)

let fw_with_sd = fw_with_dep.Fw_with_dependencies_t.parent ;;

(* let bad4 = Fw_with_small_details.reflect_latest_changes_in_github fw_with_sd (Some msg) ;; *)

let fw_the_first = fw_with_sd.Fw_with_small_details_t.parent ;;

(* let bad5 = File_watcher.reflect_latest_changes_in_github fw_the_first (Some msg) ;; *)


let config = fw_the_first.File_watcher_t.configuration ;;
let diff = fw_the_first.File_watcher_t.last_noticed_changes ;;

(* let bad6 = Reflect_change_in_github.backup config diff (Some msg) ;; *)

(* let bad7 = Reflect_change_in_github.Private.backup_with_message config diff msg ;; *)

module Pri = Reflect_change_in_github.Private ;;

let destination_dir = config.Fw_configuration_t.dir_for_backup ;;
let (nongit_cmds,git_cmds) = Pri.commands_for_backup config diff ;;
let s_destination=Dfa_root.connectable_to_subpath destination_dir ;;
let act1 =Image.image Unix_command.uc nongit_cmds ;;
let cwd=Sys.getcwd() ;;
let final_cmds = 
(
    [Unix_command.cd s_destination]@   
    git_cmds@   
    [
      "git commit -m \""^msg^"\""
    ]@
    [Unix_command.cd cwd]
) ;;      
let act2 () = Image.image Sys.command final_cmds ;;

let fc k = 
   let cmd = List.nth final_cmds (k-1) in 
   (Sys.command cmd,cmd) ;;

(*
let backup_with_message config  diff msg=
  let destination_dir = config.Fw_configuration_t.dir_for_backup in 
  let (nongit_cmds,git_cmds)=commands_for_backup config diff in
  let s_destination=Dfa_root.connectable_to_subpath destination_dir in
  let _=Image.image Unix_command.uc nongit_cmds in
  let _=(
  if config.Fw_configuration_t.gitpush_after_backup
  then let cwd=Sys.getcwd() in
       Image.image Unix_command.uc
       (
       [Unix_command.cd s_destination]@   
       git_cmds@   
       [
         "git commit -m \""^msg^"\"";
         "git push"
       ]@
       [Unix_command.cd cwd]
       ) 
  else let cwd=Sys.getcwd() in
       Image.image Unix_command.uc
       (
       [Unix_command.cd s_destination]@   
       git_cmds@   
       [
         "git commit -m \""^msg^"\""
       ]@
       [Unix_command.cd cwd]
       ) 
  ) in
  ();;
  *)

(************************************************************************************************************************
Snippet 50 : Miscellaneous tests on compilation management
************************************************************************************************************************)
open Needed_values ;;

let u1 = ae();;
let u2 = Image.image (fun el->Dfa_module.to_line(Dfn_endingless.to_module el)) u1 ;;
let u3 = Max.maximize_it_with_care (fun mn->
   min(List.length(abo mn))(List.length(bel mn))
  ) u2;;

let test1 = rv "Dfa_subdirectory.connectable_to_subpath" "cannectoble_to_sabputh" ;;
let exit_test1 = rv "Dfa_subdirectory.cannectoble_to_sabputh" "connectable_to_subpath" ;;

let test2= ren "dfa_subdirectory" "afd_sybdirectoru" ;;
let exit_test2= ren "afd_sybdirectoru" "dfa_subdirectory";;

let test3 = rensub "Depth_one_testdir/Depth_two_testdir" "Dopth_twe_tistder" ;;
let exit_test3 = rensub "Depth_one_testdir/Dopth_twe_tistder" "Depth_two_testdir" ;;

let create_ml_file_with_text (fname,text) =
   let s_ap = "Depth_one_testdir/Depth_two_testdir/adhoc/"^fname^".ml" in 
   let ap = Absolute_path.create_file_if_absent s_ap in 
   let _ = Io.overwrite_with ap text in ap ;;

let tf1 = create_ml_file_with_text("tf_one","let a= 2;;") ;;
let tf2 = create_ml_file_with_text("tf_two","let b= 3;;") ;;
let tf3 = create_ml_file_with_text("tf_three","let c= 7;;") ;;
let tf4 = create_ml_file_with_text("tf_four","let d= Tf_three.c+4;;") ;;
let tf5 = create_ml_file_with_text("tf_five","let e= Tf_four.d+5;;") ;;

regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_one.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_two.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_three.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_four.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_five.ml";;

Io.overwrite_with tf3 "let c=Tf_one.a+Tf_two.b;;" ;;

reco "1";;

fgs ["tf_one";"tf_two";"tf_three";"tf_four";"tf_five"] ;;

(************************************************************************************************************************
Snippet 49 : Extracting lines from a file and modifying them
************************************************************************************************************************)
open Needed_values ;;

let z1 = rf "Fads/sirloin.ml" ;;
let z2 = Lines_in_string.interval z1 60 75 ;;
let z3 = Lines_in_string.lines z2 ;;
let z4 = Image.image (
  fun line->
    let j1=String.index_from line 8 ' ' 
    and j2=String.index line '=' in 
    let j3=String.index_from line (j2+2) ' ' in 
    (Cull_string.interval line 9 j1,
     Cull_string.interval line (j2+3) j3)
) z3 ;;

let check_z4 = Ordered.sort Total_ordering.lex_for_strings (Image.image snd z4) ;;

let write (fun_name,lbl)=
 let addendum=(
   if lbl = "constructor" then " dummy_fw" else
   if lbl = "zerovariate_producer" then " dummy_arg" else ""

) in 
 "    let "^fun_name^" = extract_"^lbl^" All_printables."^fun_name^addendum^" ;;" ;;

let z5 = "\n\n\n" ^(String.concat "\n" (Image.image write z4)) ^ "\n\n\n";; 
let z6 () = print_string z5 ;;


let write2 (fun_name,lbl)=
 "let "^fun_name^" = Private.Exit."^fun_name^" ;;" ;;

let z7 = "\n\n\n" ^(String.concat "\n" (Image.image write2 z4)) ^ "\n\n\n";; 
let z8 () = print_string z7 ;;

(************************************************************************************************************************
Snippet 48 : Get a list of value names from an interval of lines in a file
************************************************************************************************************************)
open Needed_values ;;

let u1 = rf "Filewatching/fw_with_dependencies.ml";;
let u2 = Lines_in_string.interval u1 33 48 ;;
let u3 = Lines_in_string.lines u2 ;;
(*
let compute_names = Image.image (
  fun line ->
     let temp1 = Cull_string.two_sided_cutting ("    let ","") line in 
     let j1 = Strung.char_finder_from (fun c->List.mem c [' ';'\r';'\t']) temp1 1 in
     Cull_string.interval temp1 1 (j1-1)
) u3;;
*)


(************************************************************************************************************************
Snippet 47 : Primitive version of the Fw_with_dependencies module 
************************************************************************************************************************)
open Needed_values ;;
module Private = struct 

  let index fw = fw.Fw_with_dependencies_t.index_for_caching ;;  
  let parent fw = fw.Fw_with_dependencies_t.parent ;;
  
  let new_state (instance,state) = (instance,Fw_indexer.new_state instance) ;;
  
  let getter f fw = f (parent fw) ;;
  
  let constructor f arg =
    {
       Fw_with_dependencies_t.parent = f arg;
       index_for_caching = Fw_indexer.new_instance ();
    } ;;

  let univar f fw arg=
     let old_parent = parent fw  in 
     let new_parent = f old_parent arg
     and new_index = new_state (index fw) in  
    {
       Fw_with_dependencies_t.parent = new_parent;
       index_for_caching = new_index;
    } ;;

  let zeroplump f fw =
    let old_parent = parent fw  in 
    let (new_parent,additional_data) = f old_parent
    and new_index = new_state (index fw) in  
   ({
      Fw_with_dependencies_t.parent = new_parent;
      index_for_caching = new_index;
   },additional_data) ;;  
  
  let uniplump f fw arg=
    let old_parent = parent fw  in 
    let (new_parent,additional_data) = f old_parent arg
    and new_index = new_state (index fw) in  
   ({
      Fw_with_dependencies_t.parent = new_parent;
      index_for_caching = new_index;
   },additional_data) ;; 

  end ;;   
  
let configuration = Private.getter Fw_with_small_details.configuration ;;
let empty_one = Private.constructor Fw_with_small_details.empty_one;;
let forget_modules = Private.univar Fw_with_small_details.forget_modules ;;
let get_content = Private.getter Fw_with_small_details.get_content ;;  
let get_mtime = Private.getter Fw_with_small_details.get_mtime ;;    
let get_mtime_or_zero_if_file_is_nonregistered = Private.getter Fw_with_small_details.get_mtime_or_zero_if_file_is_nonregistered ;;  
let inspect_and_update = Private.zeroplump Fw_with_small_details.inspect_and_update;;
let last_noticed_changes = Private.getter Fw_with_small_details.last_noticed_changes ;;
let noncompilable_files = Private.getter Fw_with_small_details.noncompilable_files ;;
let of_concrete_object = Private.constructor Fw_with_small_details.of_concrete_object ;;
let of_configuration = Private.constructor Fw_with_small_details.of_configuration ;;
let of_configuration_and_list = Private.constructor Fw_with_small_details.of_configuration_and_list ;;
(*
let overwrite_file_if_it_exists = Private.univar Fw_with_small_details.overwrite_file_if_it_exists;;
let reflect_latest_changes_in_github = Private.univar Fw_with_small_details.reflect_latest_changes_in_github ;;
let register_rootless_paths = Private.uniplump Fw_with_small_details.register_rootless_paths ;;
let relocate_module_to = Private.univar Fw_with_small_details.relocate_module_to ;;
let remove_files = Private.univar Fw_with_small_details.remove_files ;;
let rename_module_on_filename_level_and_in_files = Private.uniplump Fw_with_small_details.rename_module;;
let rename_subdirectory_as = Private.univar Fw_with_small_details.rename_subdirectory_as;;
*)
let replace_string = Private.uniplump Fw_with_small_details.replace_string ;;
let replace_value = Private.uniplump Fw_with_small_details.replace_value ;;
let root = Private.getter Fw_with_small_details.root ;;
let set_gitpush_after_backup = Private.univar Fw_with_small_details.set_gitpush_after_backup ;;
let set_last_noticed_changes = Private.univar Fw_with_small_details.set_last_noticed_changes ;;
let to_concrete_object = Private.getter Fw_with_small_details.to_concrete_object ;;
let usual_compilable_files = Private.getter Fw_with_small_details.usual_compilable_files ;;






(************************************************************************************************************************
Snippet 46 : Using intervals of line indices to extract values from a module
************************************************************************************************************************)
open Needed_values ;;

let u1 = Needed_values.rf "Compilation_management/coma_state.ml";;
let u2 = Lines_in_string.core u1 ;; 

let extract_interval ((i,j),_) =
   let temp1 = List.filter (fun (k,_)->(i<=k) && (k<=j)) u2 in 
   let temp2 = Image.image snd temp1 in 
   String.concat "\n" temp2 ;;

let ref_for_colombo = ref ([
   ((961, 985),   "compute_principal_ending");
   ((1024, 1031), "registrations_for_lonely_ending");
   ((2267, 2350), "Simplified_ts_creation")
]:(((int * int) * string) list)) ;;
let ref_for_curcuma = ref ([
  ((5, 616), "Automatic"); ((634, 634), "needed_libs_at_module");
   ((636, 636), "ancestor_at_module"); ((637, 637), "needed_dirs_at_module");
   ((659, 659), "ordered_list_of_modules"); ((671, 671), "root");
   ((792, 801), "find_needed_data"); ((925, 959), "PrivateTwo");
   ((1034, 1055), "complete_id_during_new_module_registration");
   ((1394, 1466), "register_mlx_file_on_monitored_modules");
   ((1842, 1874), "Try_to_register")
]:(((int * int) * string) list)) ;;
let ref_for_replacements = ref([]:((string * string) list));;

ref_for_replacements:=[
  "=registrations_for_lonely_ending ","=Colombo.registrations_for_lonely_ending ";
  "=md_compute_modification_times ","=Colombo.md_compute_modification_times ";
  "=md_associated_modification_time ","=Colombo.md_associated_modification_time ";
  "= md_compute_modification_time ","= Colombo.md_compute_modification_time ";
  "=compute_principal_ending ","=Colombo.compute_principal_ending ";
  "= ocamldebug_printersfile_path ","= Colombo.ocamldebug_printersfile_path ";
] ;;


type spice = Colombo | Curcuma ;;

let associated_ref = function 
   Colombo -> ref_for_colombo 
  |Curcuma -> ref_for_curcuma ;; 

let spice_to_string = function 
  Colombo -> "Colombo" 
 |Curcuma -> "Curcuma" ;; 

let haddock_order = 
    let oi = Total_ordering.for_integers 
    and prod = Total_ordering.product in 
    prod (prod oi oi) Total_ordering.lex_for_strings ;; 

 let add_interval ((i,j),name) spice=
  let raf = associated_ref spice in 
    (raf:= Ordered.insert haddock_order ((i,j),name)
      (!raf)) ;;

let copy_whole spice=
   let temp1 = Image.image (extract_interval) (!(associated_ref spice)) in
   let whole = String.concat "\n\n" temp1 
   and s = spice_to_string spice in 
   let corrected_whole = Replace_inside.replace_several_inside_string
    (!ref_for_replacements) whole in 
   Replace_inside.overwrite_between_markers_inside_file
   (Overwriter.of_string corrected_whole)
   ("(* Beginning of "^s^" *)\n\n","\n\n(* End of "^s^" *)")
   (Absolute_path.of_string "Fads/pan.ml") ;;

let main ((i,j),name) spice=
   let _= add_interval ((i,j),name) spice in copy_whole spice ;;


(*


*)

(*

main ((961,985),"compute_principal_ending") Colombo;;  
main ((1024,1031),"registrations_for_lonely_ending") Colombo;; 
main ((1655,1658),"ocamldebug_printersfile_path") Colombo;;  
main ((2267,2350),"Simplified_ts_creation") Colombo;;     

main ((5,616),"Automatic") Curcuma;; 
main ((654,654),"set_product_up_to_date_at_module") Curcuma;; 
main ((629,629),"subdir_at_module") Curcuma;; 
main ((630,630),"principal_ending_at_module") Curcuma;; 
main ((631,631),"mli_presence_at_module") Curcuma;; 
main ((634,634),"needed_libs_at_module") Curcuma;; 
main ((636,636),"ancestor_at_module") Curcuma;; 
main ((637,637),"needed_dirs_at_module") Curcuma;; 
main ((649,649),"set_needed_libs") Curcuma;; 
main ((651,651),"set_ancestors_at_module") Curcuma;; 
main ((653,653),"set_needed_dirs") Curcuma;; 
main ((655,656),"set_directories") Curcuma;; 
main ((659,659),"ordered_list_of_modules") Curcuma;; 
main ((660,660),"follows_it") Curcuma;; 
main ((661,661),"all_used_subdirs") Curcuma;; 
main ((671,671),"root") Curcuma;; 
main ((680,685),"endingless_at_module") Curcuma;; 
main ((691,697),"check_ending_in_at_module") Curcuma;; 
main ((720,723),"registered_endings_at_module") Curcuma;; 
main ((781,790),"modules_with_their_ancestors") Curcuma;; 
main ((792,801),"find_needed_data") Curcuma;; 
main ((805,813),"needed_dirs_and_libs_in_command") Curcuma;; 
main ((913,917),"compute_subdirectories_list") Curcuma;; 
main ((919,922),"check_registrations") Curcuma;; 
main ((925,959),"PrivateTwo") Curcuma;; 
main ((987,1007),"complete_info") Curcuma;; 
main ((1034,1055),"complete_id_during_new_module_registration") Curcuma;; 
main ((1378,1389),"printer_equipped_types_from_data") Curcuma;; 
main ((1394,1466),"register_mlx_file_on_monitored_modules") Curcuma;; 
main ((1468,1653),"Modern") Curcuma;; 
main ((1661,1797),"Ocaml_target_making") Curcuma;; 
main ((1842,1874),"Try_to_register") Curcuma;; 

*)

(************************************************************************************************************************
Snippet 45 : Test the Detect_printer_declaration_in_text.detect function
************************************************************************************************************************)
open Needed_values ;;

let z1 = ae () ;;
let z2 = Image.image (
  fun el->
    let full = Dfn_join.to_ending el Dfa_ending.ml in 
    let ap = Dfn_full.to_absolute_path full in 
    (el,ap)
) z1;;
let (z3,z4) = List.partition (fun (el,ap)->
  Sys.file_exists (Absolute_path.to_string ap)
  ) z2 ;;
let z5 = Explicit.image (
  fun (el,ap)->(el,Io.read_whole_file ap)
) z3 ;; 
let z6 = Option.filter_and_unpack (
  fun (el,text) -> 
    let temp1 = Outside_comments_and_strings.good_substrings text in 
    if List.exists (fun (i,j,subtext)->
      (Detect_printer_declaration_in_text.detect subtext)<>None 
      ) temp1 
    then Some (Dfn_endingless.to_module el)
    else None  
) z5 ;;
let z7 = Image.image Dfn_endingless.to_module (Coma_state.printer_equipped_types_from_data (!ucs)) ;;
let check = (z6 = z7) ;;

(************************************************************************************************************************
Snippet 44 : Old version of a Van der Waerden-related module
************************************************************************************************************************)
open Needed_values ;;

let threshhold = 15;;
let m_for_threshhold = Vdw_chosen.measure threshhold ;;

let base = Vdw_precomputed.restricted_power_set (Vdw_chosen.max_width,Ennig.ennig 1 threshhold) ;;

let horizontal = Memoized.make (fun d->
  List.filter(fun x->List.length(x) = m_for_threshhold-d) base) ;;

let translated_horizontal d l =
   let temp1 = 
    Vdw_preliminaries.level_two_translate l (horizontal d) in 
   List.filter Vdw_chosen.test_for_admissibility temp1 ;;

let order_for_s_obstructions =
  Total_ordering.product 
     Vdw_preliminaries.oint Vdw_preliminaries.ointlist ;;

let ref_for_s_admissibility = ref [] ;;

let is_s_admissible (d,l) =
    List.for_all (
      fun (d1,l1)->
        (d1<>d)||(not(Ordered.is_included_in Vdw_preliminaries.oint l1 l))   
    ) (!ref_for_s_admissibility) ;;
   
let add_new_s_obstructions l =
   ref_for_s_admissibility := 
    (Ordered.sort order_for_s_obstructions (l@(!ref_for_s_admissibility)));;

let main_on_small_number n =
  (Vdw_chosen.naive_restricted_power_set (Ennig.ennig 1 n),[]);;

exception Naive_main_on_large_number_exn of ((int * int list) list);;

let naive_main_on_large_number n =
  let m = Vdw_chosen.measure n in 
  let temp1 =  Vdw_chosen.naive_restricted_power_set (Ennig.ennig (threshhold+1) n) in 
  let temp2 = Image.image (fun l->(m_for_threshhold-m+List.length(l),l)) temp1 in 
  let temp3 = List.filter (fun (d,l)->(d>=0) && (is_s_admissible (d,l)) ) temp2 in 
  let temp4 = Image.image (fun (d,l)->
     ((d,l),translated_horizontal d l)
  ) temp3 in 
  let (temp5,temp6) = List.partition (fun ((d,l),res)->res=[]) temp4 in 
  if temp5 <> []
  then raise(Naive_main_on_large_number_exn(Image.image fst temp5))
  else 
  let temp7 = Image.image fst temp6    
  and temp8 = Image.image snd temp6 in 
  (Ordered.fold_merge Vdw_preliminaries.ointlist temp8,temp7);;

let main_on_large_number n =
    let m = Vdw_chosen.measure n in 
    let temp1 =  Vdw_chosen.naive_restricted_power_set (Ennig.ennig (threshhold+1) n) in 
    let temp2 = Image.image (fun l->(m_for_threshhold-m+List.length(l),l)) temp1 in 
    let temp3 = List.filter (fun (d,l)->(d>=0) && (is_s_admissible (d,l)) ) temp2 in 
    let temp4 = Image.image (fun (d,l)->
       ((d,l),translated_horizontal d l)
    ) temp3 in 
    let (temp5,temp6) = List.partition (fun ((d,l),res)->res=[]) temp4 in 
    if temp5 <> []
    then let _ = add_new_s_obstructions (Image.image fst temp5) in 
          naive_main_on_large_number n
    else 
    let temp7 = Image.image fst temp6    
    and temp8 = Image.image snd temp6 in 
    (Ordered.fold_merge Vdw_preliminaries.ointlist temp8,temp7);;

let main = Memoized.make (fun n->
   if n<=threshhold 
   then  main_on_small_number n 
   else  main_on_large_number n
  );;

let ff n =snd(main n);;

let act1 () = 
   let _ =Explicit.image main (Ennig.ennig (threshhold+1) (threshhold+9)) in 
   (!ref_for_s_admissibility);;




(************************************************************************************************************************
Snippet 43 : Remove all snippets containing a given substring (todo : integerate it
in the Manage_diary module directly)
************************************************************************************************************************)
open Needed_values ;;

let ap_for_diary = Absolute_path.of_string "Githubbed_archive/diary_archive.ml";;
let (g1,g2) =  Manage_diary.Private.read_and_parse ap_for_diary ;;
let g3 = Ennig.index_everything g2;;
let g4 = List.filter (fun (j,(x,y))->Substring.is_a_substring_of "Vdw_" y) g3 ;;
let g5 = Image.image fst g4 ;;
let act1 () = Manage_diary.remove_snippets ap_for_diary g5;;


(************************************************************************************************************************
Snippet 42 : Search/replace following some module refactoring
************************************************************************************************************************)
open Needed_values ;;

let aps = ref [] ;;
let list_for_reps = ref [] ;;
aps := (Image.image (fun s->Absolute_path.of_string s) 
  [
    "ordered.ml";
    "Van_der_Waerden/Width_up_to_four/vdw_nonempty_index.ml";
    "Van_der_Waerden/vdw_common.ml";
    "Ordered_Lists/functor_for_sets.ml";
    "Ocaml_analysis/follow_ocaml_values.ml";
  ] );;
list_for_reps := [
  "Total_ordering.t)","Total_ordering_t.t)";
  "Total_ordering.t )","Total_ordering_t.t )";
  "Total_ordering.t\r","Total_ordering_t.t\r";
] ;;


let act1 () = List.iter 
  (Replace_inside.replace_several_inside_file 
     (!list_for_reps)) (!aps);

(************************************************************************************************************************
Snippet 41 : Extracting modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd1 = Dfa_subdirectory.of_line "Van_der_Waerden/Width_up_to_four";;

let u1 =ae () ;;

let u2 = List.filter (
  fun eless ->
    Dfa_subdirectory.begins_with (Dfn_endingless.to_subdirectory eless) sd1
) u1;; 

let u3 = Image.image (
   fun eless -> Dfa_module.to_line(Dfn_endingless.to_module eless)
) u2 ;;

(************************************************************************************************************************
Snippet 40 : Painful debugging session for Needed_values.fg
************************************************************************************************************************)
open Needed_values ;;

let sd= Dfa_subdirectory.of_line "Hex_analysis";;
let u1 = ae ();;
let u2 = List.filter (fun eless ->
   Dfa_subdirectory.begins_with (Dfn_endingless.to_subdirectory eless) sd 
) u1;;
let u3 = Image.image (fun eless ->
  Dfa_module.to_line(Dfn_endingless.to_module eless)  
) u2 ;;
let bad1 () =fgs u3 ;;
let bad2 () = Usual_coma_state.forget_several u3 ;; 


let bad3 () = Modify_coma_state.Syntactic_sugar.forget ucs u3 ;;

let ref_for_modules = ref []
and ref_for_paths = ref [] ;;
let hum =List.iter (
     fun descr ->
       if String.contains descr '.'
       then ref_for_paths:= (Dfn_rootless.of_line descr)::(!ref_for_paths)
       else ref_for_modules:= (Dfa_module.of_line descr) ::(!ref_for_modules)
  ) u3 ;;
let all_paths = List.rev(!ref_for_paths) 
and all_modules =  List.rev(!ref_for_modules) ;;

let bad4 () = Modify_coma_state.Reference.forget_modules ucs all_modules ;;  

let cs = (!ucs) ;;
let bad5 () = Modify_coma_state.And_save.forget_modules cs all_modules ;;  
let bad6 () = Modify_coma_state.And_backup.forget_modules cs all_modules ;;  
let bad7 () = Modify_coma_state.After_checking.forget_modules cs all_modules ;; 
let bad8 () = Modify_coma_state.Physical_followed_by_internal.forget_modules cs all_modules ;; 

let mod_names = all_modules ;;
let check = Coma_state.check_module_sequence_for_forgettability cs mod_names ;;
   
let cs2=Modify_coma_state.Physical.forget_modules cs mod_names ;;
let bad9 () = Modify_coma_state.Internal.forget_modules cs2 mod_names ;;

let mns = mod_names ;;
let old_endinglesses = Image.image (Coma_state.endingless_at_module cs2) mns ;;
let bad10 ()=Coma_state.unregister_modules  cs2 old_endinglesses ;;
let bad11 ()=List.fold_left Coma_state.unregister_module  cs2 old_endinglesses ;;

let one_more_step (ccs,elesses) = 
    let (eless,other_elesses) = Listennou.ht elesses in 
    (Coma_state.unregister_module ccs eless,other_elesses) ;;
let starting_point = (cs2,old_endinglesses) ;;
let iterate = Memoized.small one_more_step starting_point ;;
let bad12 () = iterate (List.length old_endinglesses) ;;
let bad13 () = Tools_for_debugging.extract_from_iteration one_more_step starting_point;;
let bad14 () = one_more_step starting_point ;;




(************************************************************************************************************************
Snippet 39 : Remove all "automatic" modules 
************************************************************************************************************************)
open Needed_values ;;

let u1 = ae ();;
let u2 = Image.image (fun eless ->
   Dfa_module.to_line(Dfn_endingless.to_module eless)  
) u1;;
let u3 = List.filter (
  fun x-> Supstring.ends_with x "_automatic"
) u2 ;;

let computed_u3 = ["concrete_object_automatic"; "fw_wrapper_automatic"; "coma_state_automatic";
"fw_nonmodular_wrapper_automatic"; "hex_flattened_end_strategy_automatic"];;

let g1 = vfm "hex_flattened_end_strategy_automatic" ;;
let g2 = Image.image fst g1 ;;

let g3 = Image.image (fun x-> Replace_inside.replace_inside_string ("x",x) "let x = Automatic.x ;;") g2;;
let g4 = String.concat "\n" g3 ;;
let g5 = "\n\n\n" ^ g4 ^ "\n\n\n" ;; 

let h1 = List.flatten (Image.image snd g1) ;;
let h2 = Ordered.sort Total_ordering.standard h1 ;;
let h3 = List.iter (
  fun fn -> Replace_inside.replace_inside_file ("Hex_flattened_end_strategy_automatic.","Hex_flattened_end_strategy.") fn
) h2 ;;


(************************************************************************************************************************
Snippet 38 : Typical use of the Manage_diary module
************************************************************************************************************************)
let ap_for_diary = Absolute_path.of_string "Githubbed_archive/diary_archive.ml";;

let act1 () = Manage_diary.fix_indexation ap_for_diary ;;

let u1 = Manage_diary.empty_snippets ap_for_diary ;;

let act2 () = Manage_diary.remove_snippets ap_for_diary u1;;

let act3 () = Manage_diary.absorb_new_snippet ap_for_diary ;;

let diary_text = Io.read_whole_file ap_for_diary ;;

let (g1,g2) =  Manage_diary.Private.read_and_parse ap_for_diary ;;

(************************************************************************************************************************
Snippet  37 : Deduce the lower measure from the usual measure (related to Vdw)
************************************************************************************************************************)
let measure n =
  if n<1 then 0 else 
  let q=(n/9) in 
  match n mod 9 with
   0 -> 4*q+1 
  |1 -> 4*q+1
  |2 -> 4*q+2  
  |3 -> 4*q+2
  |4 -> 4*q+3
  |5 -> 4*q+4
  |6 -> 4*q+4  
  |7 -> 4*q+4
  |8 -> 4*q+4 
  | _ -> failwith("unforeseen");;   

let lower_measure n =
   if n<1 then 0 else 
   let q=(n/9) in 
   match n mod 9 with
    0 -> 4*q
   |1 -> 4*q
   |2 -> 4*q 
   |3 -> 4*q
   |4 -> 4*q+1
   |5 -> 4*q+1
   |6 -> 4*q+2  
   |7 -> 4*q+2
   |8 -> 4*q+3 
   | _ -> failwith("unforeseen");;  



let compute_lower_measure n = 
  let tempf = (fun t->measure(n+t)-measure(t)) in 
  snd(Min.minimize_it tempf (Ennig.ennig 1 20)) ;;   


(************************************************************************************************************************
Snippet  36 : Unfinished attempt for automated Crobj converters writing
************************************************************************************************************************)
open Needed_values ;;

module Common = struct 

    let add_indentation k lines =
        let indent = String.make k ' ' in 
        Image.image (fun line->indent^line) lines ;;

    let broken_module_name modname =
        let n = String.length modname in 
        let j = (if n<3 then 1 else 3) in 
        let left_part = Cull_string.beginning j modname 
        and right_part = Cull_string.cobeginning j modname  in 
        "\""^left_part ^ "\" ^ \""^right_part ^ ".\"" ;;    

    let concat_paragraphs pars=
        let temp1 = Image.image (String.concat "\n") pars in 
        String.concat "\n\n\n" temp1 ;;    

    let remove_last_element_if_blank l =
        let (a,b) = Listennou.ht (List.rev l) in 
        if (Cull_string.trim_spaces a) = ""
        then List.rev b 
        else l  ;; 

    let wrap_in_parentheses_if_needed typename =
        if (String.contains typename ' ')||(String.contains typename '*')
        then "( "^typename^" )"
        else  typename ;;         
    
    let unwrap_parentheses_if_needed typename =
            if Supstring.begins_with typename "("
            then let n = String.length typename in
                 Cull_string.interval typename 2 (n-1)
            else  typename ;;   

    let listify is_a_list name =
        if not(is_a_list) 
        then name 
        else (wrap_in_parentheses_if_needed name)^" list" ;;  
    
    exception Arguments_in_input_exn of int * int ;;

    let arguments_in_input argname selected_nbr total_nbr =
        if selected_nbr > total_nbr 
        then raise(Arguments_in_input_exn(selected_nbr,total_nbr))
        else let temp1 = Ennig.doyle (fun k->
              if k<=selected_nbr then argname^(string_of_int k) else "_") 1 total_nbr in 
             "(" ^ (String.concat "," temp1) ^ ")" ;;         
    
    let max_number_of_arguments = 7;;         

    let counter_for_converters = ref 0 ;;   
    let accu_for_helpers = ref [] ;; 

end ;;    

module Level_four = struct 

    type t = 
       Int 
      |Bool 
      |String
      |Modular of string * string
      |Preregistered of string ;;

    exception Constructor_exn of string ;;  

    let module_for_preregistering = "Crobj_converter." ;;

    let constructor cs s = 
        if Supstring.ends_with s "_t.t"
        then let mname = Cull_string.coending 4 s in 
             let modn = Dfa_module.of_line mname in
             let preferred_modn = Coma_state.choose_automatic_if_possible cs modn in
             Modular (mname,String.capitalize_ascii (Dfa_module.to_line preferred_modn))
        else 
        if Supstring.begins_with s module_for_preregistering
        then Preregistered (Cull_string.two_sided_cutting (module_for_preregistering,"") s)
        else   
        match List.assoc_opt s ["bool",Bool;"int",Int;"string",String] with 
        Some(answer) -> answer
        | _ -> raise (Constructor_exn(s)) ;;             


    
    let definition = function 
         Int -> "int"
        |Bool -> "bool"
        |String -> "string"
        |Modular(mname,preferred_mname)->mname^"_t.t"
        |Preregistered(tname)-> module_for_preregistering ^ tname;;

    let get_converters = function 
        Int -> (module_for_preregistering ^ "int_of_concrete_object",
                module_for_preregistering ^ "int_to_concrete_object")
       |Bool -> (module_for_preregistering ^ "bool_of_concrete_object",
                 module_for_preregistering ^ "bool_to_concrete_object")
       |String -> (module_for_preregistering ^ "string_of_concrete_object",
                   module_for_preregistering ^ "string_to_concrete_object")
       |Modular(mname,preferred_mname)->
                        (preferred_mname^".of_concrete_object",
                         preferred_mname^".to_concrete_object")
       |Preregistered(tname)-> (module_for_preregistering ^ tname ^ "of_concrete_object",
                                module_for_preregistering ^ tname ^ "to_concrete_object");;     

    

end ;;    

module Level_three = struct 


    type t = L3 of bool * Level_four.t ;;

    let constructor is_listy l4_t = L3(is_listy,l4_t) ;;

    let definition (L3 (is_listy,l4_t)) = Common.listify is_listy (Level_four.definition l4_t) ;;

    let get_converters (L3 (is_listy,l2_t)) =
        let  (cv_of,cv_to) = Level_four.get_converters l2_t in 
        if is_listy 
        then ("Crobj_converter_combinator.to_list "^cv_of,
              "Crobj_converter_combinator.of_list "^cv_to)   
        else (cv_of,cv_to) ;;     

end ;;

module Level_two = struct 

    type t = L2 of Level_three.t list ;;
    
    let constructor l = L2(l) ;;
    
    let definition (L2 (l)) = String.concat " * " 
          (Image.image (fun l3_t -> 
            Common.wrap_in_parentheses_if_needed
             (Level_three.definition l3_t)) l) ;;

    let new_converter k (L2(l)) = 
       let d = List.length l in 
       let args1 = Common.arguments_in_input "arg" d Common.max_number_of_arguments 
       and args2 = Common.arguments_in_input "x" d d 
       and uple1 = String.concat "," (Image.image (fun (idx,l3_t)->
           let (cv_of,cv_to) = Level_three.get_converters l3_t in 
           cv_of^" arg"^(string_of_int idx) 
        ) (Ennig.index_everything l)) 
       and uple2 = String.concat ";" (Image.image (fun (idx,l3_t)->
            let (cv_of,cv_to) = Level_three.get_converters l3_t in 
            cv_to^" x"^(string_of_int idx) 
       ) (Ennig.index_everything l)) in 
       let sk = string_of_int k in 
        (["let pcv"^sk^"_of_crobj ccrt_obj = "]@
         (
            Common.add_indentation 2 (
              ["let "^args1^" = Concrete_object_automatic.unwrap_bounded_uple ccrt_obj in ";
              "("^uple1^") ;;"]
            )
         )@ 
         ["let pcv"^sk^"_to_crobj "^args2^" = "]@
         (
            Common.add_indentation 2 (
              [" Concrete_object_t.Uple ["^uple2^"] ;;"]
            )
         ),
         ("pcv"^sk^"_of_crobj","pcv"^sk^"_to_crobj")) ;;
    
    let accu_for_converters = ref [] ;;
    
    let get_converters l2_t = 
        let (L2 l) = l2_t in
        if List.length l = 1 
        then Level_three.get_converters (List.hd l)
        else         
        match List.assoc_opt l2_t (!accu_for_converters) with 
        Some(cv_of,cv_to)->(cv_of,cv_to)
        |None -> (* the step below is to insure the converters count
                    will be done in the correct order, so that when we
                    modify Common.counter_for_converters all the preliminary
                    subcomputations have already been carried through *)
                 let _ = Image.image Level_three.get_converters l in 
                 let count = (!(Common.counter_for_converters))+1 in 
                 let (new_helpers,(cv_of,cv_to)) = new_converter count l2_t in 
                 let _ = (
                    Common.counter_for_converters := count ;
                    Common.accu_for_helpers := (!(Common.accu_for_helpers)) @ new_helpers ;
                    accu_for_converters := (l2_t,(cv_of,cv_to)):: (!accu_for_converters) ;
                 ) in 
                 (cv_of,cv_to) ;;
    
end ;;    
    

module Level_one = struct 

    type t = L1 of bool * Level_two.t ;;

    let constructor is_listy l2_t = L1(is_listy,l2_t) ;;

    let definition (L1 (is_listy,l2_t)) = Common.listify is_listy (Level_two.definition l2_t) ;;

    let get_converters (L1 (is_listy,l2_t)) =
        let  (cv_of,cv_to) = Level_two.get_converters l2_t in 
        if is_listy 
        then ("Crobj_converter_combinator.to_list "^cv_of,
              "Crobj_converter_combinator.of_list "^cv_to)   
        else (cv_of,cv_to) ;;

end ;;    


module Reasonable_record = struct 

type t = RR of string * ((string * Level_one.t) list) ;;


let indented_definition k (RR (module_name,l))=
    let indent =  String.make (k+2) ' ' in 
    (
   "{" ::
   (Image.image (fun (field_name,field_type)->
       indent ^ field_name ^ " : " ^ (Level_one.definition field_type) ^ " ;"
    ) l)
   @["}"]);;

let label_definitions (RR (module_name,l))= 
   ("let salt = " ^ (Common.broken_module_name module_name)^ " ;;") ::
   ( Strung.reposition_left_hand_side_according_to_separator "="
  (Image.image (fun (field_name,field_type)->
    "let " ^ (String.lowercase_ascii field_name) ^ "_label = salt ^ \"" ^ field_name ^ "\" ;;"
   ) l))
;; 



let converter_of_crobj reasonable_record =
    let (RR (module_name,l)) = reasonable_record in 
    let converters = Image.image (fun pair->
        let (field_name,field_type) = pair in 
        (pair,Level_one.get_converters field_type) ) l in 
    (
      "let of_concrete_object ccrt_obj = " ::
      (Common.add_indentation 2
        ( "let g = Concrete_object_automatic.get_record ccrt_obj in" ::
          "{"::
           (
            Common.add_indentation 2 
            (
               Image.image (
                fun ((field_name,field_type),(of_crobj,to_crobj))->
                    module_name^"."^field_name ^ " = " ^ 
                    of_crobj^" (g "^(String.lowercase_ascii field_name)^"_label) ;"
               ) converters
            )
           )@
           ["} ;;"]
        )
      )
    );;

let converter_to_crobj reasonable_record =
    let (RR (module_name,l)) = reasonable_record in 
    let converters = Image.image (fun pair->
        let (field_name,field_type) = pair in 
        (pair,Level_one.get_converters field_type) ) l in 
    (
      "let to_concrete_object x = " ::
      (Common.add_indentation 2
        ( "Concrete_object_t.Record [" ::
           (
            Common.add_indentation 2 
            (
               Image.image (
                fun ((field_name,field_type),(of_crobj,to_crobj))->
                    (String.lowercase_ascii field_name)^"_label, "^
                    to_crobj^" x."^module_name^"."^field_name^" ;"
               ) converters
            )
           )@
           ["] ;;"]
        )
      )
    );;

let whole rr =
    let (RR (module_name,l)) = rr in 
    let rr_of_crobj = converter_of_crobj rr 
    and rr_to_crobj = converter_to_crobj rr in 
    Common.concat_paragraphs
    [
        ["module "^module_name^"=struct \n type t=\n"]
        @(indented_definition 2 rr)@[" ;;\n end;;"];
        label_definitions rr;
        (!(Common.accu_for_helpers));
        rr_of_crobj;
        rr_to_crobj;

    ] ;;

end ;;    

module Parsing = struct 

    exception Analize_record_item_exn of string ;;

    let listy_decomposition line =
        if not(Supstring.ends_with line "list")
        then (false,line)
        else 
        let temp1 = Cull_string.trim_spaces (Cull_string.coending 4 line) in 
        if not(Supstring.ends_with temp1 ")")
        then (true,temp1)
        else 
        let temp2 = Cull_string.two_sided_cutting ("(",")") temp1 in 
        (true,Cull_string.trim_spaces temp2) ;;          

    let analize_record_item cs line =
        let j = Substring.leftmost_index_of_in ":" line in 
        if j<0 then raise(Analize_record_item_exn line) else 
        let field_type = Cull_string.trim_spaces (Cull_string.cobeginning j line) in 
        let (is_listy,comp_type2) = listy_decomposition field_type in
        let elements_in_product = Str.split (Str.regexp_string "*") comp_type2 in 
        let elts = Image.image (fun untrimmed_s->
            let pre_s = Cull_string.trim_spaces untrimmed_s in 
            let s = Common.unwrap_parentheses_if_needed pre_s in
            let (is_listy2,comp_type4) = listy_decomposition  s in 
            Level_three.constructor is_listy2 (Level_four.constructor cs comp_type4)) elements_in_product in 
        let comp_type1 = Level_one.constructor is_listy (Level_two.constructor elts) in 
        (Cull_string.trim_spaces (Cull_string.beginning (j-1) line),comp_type1 ) ;;
     
    let parse_record_type_declaration cs text=
       let i1 = (String.index text '{')+1
       and i2 = (String.index text '}')+1 in 
       let temp1 = Cull_string.interval text (i1+1) (i2-1) in 
       let temp2 = Str.split (Str.regexp_string ";") temp1 in 
       let temp3 = Common.remove_last_element_if_blank temp2 in 
       let temp4 = Image.image (analize_record_item cs) temp3 in 
       temp4 ;;  
    
end ;;    

let u1 = rf "Compilation_management/coma_state_t.ml";;
let u2 = Lines_in_string.interval u1 8 23 ;;
let u3 = Parsing.parse_record_type_declaration (!ucs) u2 ;;    


let rr1 = Reasonable_record.RR ("Zorglub", u3);;

let text1 =  "\n\n\n " ^ (Reasonable_record.whole rr1) ^ "\n\n\n" ;;

let ap_for_writing = Absolute_path.of_string "Fads/nap.ml" ;; 

let write () = 
     Replace_inside.overwrite_between_markers_inside_file 
       (Overwriter.of_string text1)
     ("(* Beginning *)","(* End *)") ap_for_writing ;;

(*     
let line = "needed_dirs_for_module : (Dfa_module_t.t * (Dfa_subdirectory_t.t list)) list" ;;
let j = Substring.leftmost_index_of_in ":" line ;;
let field_type = Cull_string.trim_spaces (Cull_string.cobeginning j line) ;;
let (is_listy,comp_type2) = Parsing.listy_decomposition field_type ;;
let elements_in_product = Str.split (Str.regexp_string "*") comp_type2 ;;


        let elements_in_product = Str.split (Str.regexp_string "*") comp_type2 in 
        let elts = Image.image (fun untrimmed_s->
            let s = Cull_string.trim_spaces untrimmed_s in 
            let (is_listy2,comp_type4) = listy_decomposition  s in 
            Level_three.constructor is_listy2 (Level_four.constructor comp_type4)) elements_in_product in 
        let comp_type1 = Level_one.constructor is_listy (Level_two.constructor elts) in 
        (Cull_string.trim_spaces (Cull_string.beginning (j-1) line),comp_type1 ) ;;
     
*)

(************************************************************************************************************************
Snippet  35 : Add a new subdir to a Coma_state_t.t object
************************************************************************************************************************)
open Needed_values ;;

let sd1 = Dfa_subdirectory.of_line "";;
let sd1 = Dfa_subdirectory.of_line "Ocaml_analysis/Concrete_ocaml_types";;
relo "concrete_ocaml_type_t" sd1 ;;

let cs = (!ucs) ;;
let old_sdirs = cs.Coma_state_t.directories ;;
let new_sdirs = old_sdirs @ [Dfa_subdirectory_t.SD "Ocaml_analysis/Concrete_ocaml_types"] ;;
let new_cs = {cs with Coma_state_t.directories = new_sdirs } ;;
Save_coma_state.save new_cs ;;


(************************************************************************************************************************
Snippet  34 : Check and fix initial comments in files
************************************************************************************************************************)
open Needed_values ;;

let z1 = Usual_coma_state.all_principals () ;;
let z2 = Image.image (fun full->
  let rl = Dfn_rootless.to_line (Dfn_full.to_rootless full) in 
  (rl,Dfn_full.to_absolute_path full)) z1 ;;
let z3 = Explicit.image (
   fun (rl,ap) -> 
    try (rl,ap,Put_use_directive_in_initial_comment.detect_initial_comment_in_file ap) with 
    _ ->(rl,ap,None)
) z2 ;;
let z4 = Option.filter_and_unpack (fun (rl,_,opt)->
   if opt = None then Some rl else None
  ) z3;;
let z4 = List.filter (fun s->Supstring.begins_with s "Compi") z4;;  
let z5 = "\n\n\n" ^ (String.concat " "("vopen"::z4)) ^ "\n\n\n";;
let root = Coma_state.root (!ucs) ;;
let z6 = Image.image (
  fun (rl,ap,opt) ->
    let (_,line,_) = Option.unpack opt in 
    (rl,ap,line,Put_use_directive_in_initial_comment.usual root ap)
) z3;;
let z7 = List.filter (
  fun (rl,ap,line1,line2) -> line1 <> line2
) z6;;
let z8 () = Explicit.image (
  fun (rl,ap,line1,line2) -> Put_use_directive_in_initial_comment.put_usual root ap
) z7 ;;


(************************************************************************************************************************
Snippet  33 : Relocate all modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd1 = Dfa_subdirectory.of_line 
   "Van_der_Waerden" ;;

let sd2 = Dfa_subdirectory.of_line 
   "Van_der_Waerden/First_try" ;;

let u1 = ae () ;;   
let u2 = List.filter ( fun
  (Dfn_endingless_t.J(r,sd,m)) -> Dfa_subdirectory.begins_with sd sd1
) u1 ;;
let u3 = Image.image ( fun
(Dfn_endingless_t.J(r,sd,m)) -> Dfa_module.to_line m
) u2 ;;

let act1 () = Explicit.image (fun mn->relo mn sd2) u3 ;;

(************************************************************************************************************************
Snippet  32 : Delete all modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd1 = Dfa_subdirectory.of_line 
   "Ocaml_analysis/Standardized_concrete_ocaml_types" ;;

let u1 = ae () ;;   
let u2 = List.filter ( fun
  (Dfn_endingless_t.J(r,sd,m)) -> Dfa_subdirectory.begins_with sd sd1
) u1 ;;
let u3 = List.rev_map ( fun
(Dfn_endingless_t.J(r,sd,m)) -> Dfa_module.to_line m
) u2 ;;

let act1 () = fgs u3 ;;


(************************************************************************************************************************
Snippet  31 : Code from an abandoned, self-contained module
************************************************************************************************************************)
exception Too_many_arguments of int ;;

let wrap_in_parentheses_if_needed typename =
    if String.contains typename ' '
    then "( "^typename^" )"
    else  typename ;;    

let max_nbr_of_arguments = 7 ;;    

let arguments_in_input argname n=
    if n> max_nbr_of_arguments 
    then raise(Too_many_arguments(n))
    else let temp1 = Ennig.doyle (fun k->
          if k<=n then argname^(string_of_int k) else "_") 1 max_nbr_of_arguments in 
         "(" ^ (String.concat "," temp1) ^ ")" ;;       

let listify is_a_list name =
    if not(is_a_list) 
    then name 
    else (wrap_in_parentheses_if_needed name)^" list" ;;     

let add_appendix_to_last_line appendix lines =
      let (last_line,other_lines) = Listennou.ht (List.rev lines) in 
      List.rev ((last_line^appendix)::other_lines) ;;    

(************************************************************************************************************************
Snippet  30 : Permutations far (wrt Hamming distance) from shift with constants. 
************************************************************************************************************************)
open Needed_values ;;

let hamming_distance perm1 perm2 =
  let temp1 = List.combine perm1 perm2 in 
  List.length(List.filter (fun (x,y)->x<>y) temp1);;

let generic_translate n t  = (Ennig.ennig t n) @ (Ennig.ennig 1 (t-1))  ;;

let all_translates =Memoized.make (fun n -> Ennig.doyle (generic_translate n) 1 n);;

let measure n perm = snd(Min.minimize_it (hamming_distance perm) (all_translates n)) ;;


let iii = Memoized.make Permutation.iii ;;

let ff = Memoized.make(fun n->
   let whole = iii n 
   and meas = Memoized.make (measure n) in 
   let m = snd(Max.maximize_it meas whole) in 
   Explicit.filter (fun perm->meas(perm)=m) whole);;   


let gg n = Chronometer.it ff n;;

let hh n = (measure n (List.hd(ff n)));;

Ennig.doyle (fun x->(x,hh x)) 3 10;;

let hf n = List.hd(ff n) ;;

(************************************************************************************************************************
Snippet  29 : Mass inheritance from a Private submodule 
************************************************************************************************************************)
let z1 = 
  ["conventional_files_with_full_content";
   "conventional_files_with_minimal_content"; "debug_build_subdir";
   "exec_build_subdir"; "full_set_of_needed_dirs"; "git_ignored_subdirectories";
   "minimal_set_of_needed_dirs"; "rootless_path_for_loadingsfile";
   "rootless_path_for_parametersfile"; "rootless_path_for_printersfile";
   "rootless_path_for_targetfile"; "usual_build_subdir"; "utility_files_subdir"] ;;
  
  let z2 = Image.image (fun x->" let "^x^" = Private."^x^" ;;") z1;; 
  
  let z3 = "\n\n\n" ^ (String.concat "\n" z2) ^ "\n\n\n" ;; 

(************************************************************************************************************************
Snippet  28 : Typical use of the Other_coma_state module 
************************************************************************************************************************)
let act1 () = Other_coma_state.repopulate (Needed_data_summary_t.Everything);;
let see = Other_coma_state.see_yet_unofficial_changes ();; 
let act2 () = Other_coma_state.officialize_changes ();;

(************************************************************************************************************************
Snippet  27 : Testing freezing and unfreezing of world copies
************************************************************************************************************************)
open Needed_values ;;

let remote_dir = Dfa_root.of_line 
   (home^"/Teuliou/OCaml/Forgotten_projects/Html_scraping_project") ;;

(*

To store a "frozen" copy of the project in a separate directory.
You can combine this with a cp -R (which often will not suffice by itself since you 
also need the dependecies from other subdirectories).

*)

let sd= Dfa_subdirectory.of_line "Text_editing/Html_scraping";;

let g1 = Create_world_copy.frozen_copy (!ucs)
    ~destination:remote_dir 
    (Needed_data_summary_t.Selection([],
    [sd])) ;;
   
(*

Much later, you can "unfreeze" the project as follows

*)    

let g2 = Create_world_copy.unfreeze_copy (!ucs) remote_dir ;;

(*

Then, you can cd to the separate dir, launch utop in it, and enjoy.

*)


(************************************************************************************************************************
Snippet  26 : Fixing a Coma_state_object.t using Coma_state_automatic.restrict
************************************************************************************************************************)
open Needed_values ;;

let cs = (!ucs) ;;

let bad1 = Coma_state.latest_changes cs;;

let z1 = Coma_state.all_endinglesses cs ;; 
let (bad_in_z1,good_in_z1) = List.partition (
   fun (Dfn_endingless_t.J(r,sd,m)) ->
     List.exists (Dfa_subdirectory.begins_with sd) 
       [Dfa_subdirectory_t.SD"Temporary";
       Dfa_subdirectory_t.SD"Automatically_generated"]
) z1 ;;

let good_modules = Image.image 
  (fun (Dfn_endingless_t.J(r,sd,m)) ->m) good_in_z1;;

let new_cs = Coma_state.Automatic.restrict cs good_modules ;;  
Save_coma_state.save new_cs ;;

(************************************************************************************************************************
Snippet  25 : Remove interval of lines in a file 
************************************************************************************************************************)
let ap = Absolute_path.of_string "Imported/Aantron/aantron_markup.ml";;
let old_text = Io.read_whole_file ap ;;
let v1 = Lines_in_string.core old_text ;;
let v2 = List.filter (fun (j,line)->(299<=j)&&(j<=338) ) v1 ;;
let v3 = Image.image (
   fun (j,line)->
      let i1 = Substring.leftmost_index_of_in "val " line 
      and i2 = Substring.leftmost_index_of_in ":" line in 
      Cull_string.trim_spaces(Cull_string.interval line (i1+4) (i2-1))
) v2 ;;
let tab = String.make 5 ' ' ;;
let v3 = Image.image (fun (j,line) -> 
  if Supstring.begins_with line tab
  then Cull_string.two_sided_cutting (tab,"") line
  else line   
    ) v2;;

let tab = String.make 7 ' ' ;;
let v4 = Ordered.sort Total_ordering.lex_for_strings v3;;
let v5 = Image.image (fun name -> tab^"let "^name^" = Aantron_encoding."^name^" ;;") v4;;
let old_snippet = String.concat "\n" (Image.image snd v2) ;;
let new_snippet = String.concat "\n"  v5;;
let act () = Replace_inside.replace_inside_file (old_snippet,new_snippet) ap ;; 

let ap = Absolute_path.of_string "Imported/Aantron/aantron_markup.ml";;
let old_text = Io.read_whole_file ap ;;
let v1 = Lines_in_string.core old_text ;;
let v2 = List.filter (fun (j,line)->(299<=j)&&(j<=338) ) v1 ;;
let v3 = Image.image (fun (j,line)->Cull_string.trim_spaces line) v2 ;;
let v4 = List.filter (Substring.is_the_beginning_of "The value ") v3;;
let v5 = Image.image (
   fun line->
      let i1 = Substring.leftmost_index_of_in "`" line 
      and i2 = Substring.leftmost_index_of_in "'" line in 
      Cull_string.trim_spaces(Cull_string.interval line (i1+1) (i2-1))
) v4 ;;
let v6 = Ordered.sort Total_ordering.lex_for_strings v5;;
let v7 = Image.image (fun name -> "let "^name^" = Aantron_utility."^name^" ;;") v6;;
let v8 = "\n\n\n" ^ (String.concat "\n" v7) ^ "\n\n\n";;
let v9 = print_string v8 ;;

(************************************************************************************************************************
Snippet  24 : Removing module wrappers in a set of files
************************************************************************************************************************)
let remove_module_wrapper_in_text text =
  let lines = Lines_in_string.core text in 
  let (i1,_)= Listennou.force_find (fun (_,line)->
    Supstring.begins_with (Cull_string.trim_spaces line) "module "
  ) lines in
  let (i2,_)= Listennou.force_find (fun (_,line)->
    Supstring.begins_with (Cull_string.trim_spaces line) "end"
  ) (List.rev lines) in 
  let selected_lines = Option.filter_and_unpack (
    fun (i,line)->if List.mem i [i1;i2] then None else Some line
  ) lines in 
  String.concat "\n" selected_lines ;;

let remove_module_wrapper_in_file ap =
  let old_text = Io.read_whole_file ap in 
  let new_text = remove_module_wrapper_in_text old_text in 
  Io.overwrite_with ap new_text ;;

let the_dir = Directory_name.of_string ((Sys.getcwd())^"/Imported/Aantron/Temp"  ) ;;
let u1 = More_unix.simple_ls the_dir ;;

let act1 () = List.iter remove_module_wrapper_in_file u1 ;;

(************************************************************************************************************************
Snippet  23 : Sorting names in the dictionary order
************************************************************************************************************************)
let z1 = Ordered.sort Total_ordering.lex_for_strings 
[
  "current_state";
  "emit";
  "push_and_emit";
  "pop";
  "emit_end";
  "initial_state";
  "document_state";
  "doctype_state";
  "root_state";
  "after_root_state";

] ;;

let z2 = "\n\n\n" ^ (String.concat "\n" z1) ^ "\n\n\n" ;;

print_string z2;;

(************************************************************************************************************************
Snippet  22 : Remove phpbb links to footnotes 
************************************************************************************************************************)
let write1 k=
  let sk = string_of_int k in 
  "\n[b][color=blue]("^sk^")[/color][/b]\n" ;;

let reps = Ennig.doyle (fun j->(write1 j,"")) 1 43  ;;

let dir = (Sys.getenv "HOME")^"/Teuliou/html_files/Translations/";;  
let ap1 =   Absolute_path.create_file_if_absent (dir^"/notes_to_dot.txt") ;;

let text1= Io.read_whole_file ap1;;
let lines1 = Lines_in_string.core text1;;

let act1 () = Replace_inside.replace_several_inside_file reps ap1;;




(************************************************************************************************************************
Snippet  21 : Typical use of Html_to_phpbb.translate
************************************************************************************************************************)
open Needed_values ;;

let u1 = rf (home^"/Teuliou/html_files/Fenton/divine_origin.html");;

let u2 = Html_to_phpbb.translate u1;;

let ap1 = Absolute_path.of_string 
  (home^"/Teuliou/html_files/Translations/divine_origine_translated.txt") ;;

Io.overwrite_with ap1 u2;;  

(************************************************************************************************************************
Snippet  20 : Interaction between "beginning" and "end" of a large tex file
************************************************************************************************************************)
open Needed_values;;

let beg_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/beginning_of_text.txt");; 
 
let end_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/end_of_text.txt");; 

let tex_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/blet_pius_xii.tex");;   

let cmd_for_texshop = "osascript "^home^"/Teuliou/Bash_scripts/Automation/compile_with_texshop.scpt";;

let loop () =
    let beg_part = Io.read_whole_file beg_ap 
    and end_part = Io.read_whole_file end_ap in 
    let whole = beg_part ^ "\n" ^ end_part in 
    let _ =Io.overwrite_with tex_ap whole in 
    Sys.command cmd_for_texshop;;

let tr k = More_io.transfer_first_lines_of_to k end_ap beg_ap;;

let ll k = let temp = Lines_in_string.interval (Io.read_whole_file end_ap) k k in 
  (temp,Strung.explode temp);;

let rye (a,b) = Replace_inside.replace_inside_file (a,b) end_ap ;; 

let rblap () = Remove_blank_lines_around_percents.in_file end_ap ;;

let rlc pattern = 
   let _ = Lines_in_string.remove_lines_containing_substring_in_file 
   pattern end_ap in rblap ();;

let usual_cleaning () =
   Replace_inside.replace_several_inside_file 
   [
     (".! ",".1 ");
     (".!\n",".1 ");
    ("\012","");
    ("_","");
    ("#","");
    ("\\Vhat","What");
    ("\\Vhen","When");
    ("\\Vhile","While");
    ("\194\162","c");
    ("\226\128\156","\194\171");
    ("\226\128\157","\194\187");
    ("$","\194\167");
    (" & "," \\& ");
    ("&\226\128\153","d'");
    ("\\xii. ","xii. ");
    ("\194\165","V");
    ("\n1}","\n1 ");
    ("\n}","\n1 ");
    ] end_ap ;;


(************************************************************************************************************************
Snippet  19 : Add blank space at the beginning of lines (to make copy&paste easier )
************************************************************************************************************************)
open Needed_values;;

let blanks = String.make 3 ' ';; 

let reform_line x=
  if (x="")||(Supstring.begins_with x blanks) then x else blanks^x;; 

let reform_string s=
  let temp1 = Lines_in_string.lines s in 
  let temp2 = Image.image reform_line temp1 in 
  String.concat "\n" temp2 ;;


let the_ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/PDF_files/Printable/Preparation/greek_in_vl.txt");; 

let old_text = Io.read_whole_file the_ap ;;

let new_text = reform_string old_text ;;

Io.overwrite_with the_ap new_text;; 


(************************************************************************************************************************
Snippet  18 : Delete some HTML footnotes (with their links) and reindex
************************************************************************************************************************)
open Needed_values;;

let ap1 = Absolute_path.of_string (home^"/Teuliou/html_files/Fortescue/papacy_451.html");; 
let old_text = Io.read_whole_file ap1 ;;

let u1 = Enumerate_html_links_to_footnotes.main old_text ;;

let see = Image.image (fun ((i_start,i_end),link_idx)->
    Cull_string.interval old_text i_start i_end) u1 ;; 

let bad_indices = [1;3;4;5;10;11;18;20] ;;

let u2 = List.filter (fun ((i_start,i_end),link_idx)-> 
    (List.mem link_idx bad_indices) ) u1;;
let u3 = Image.image fst u2 ;;
let u4 = Ennig.index_everything u3 ;; 
let u5 = Image.image (
  fun (k,(i_start,i_end))->((i_start,i_end),k)
) u4;;
let u6 = Image.image (fun ((i_start,i_end),link_idx)-> 
    ((i_start,i_end),List.assoc_opt (i_start,i_end) u5) 
) u1;;
let write_link opt = match opt with 
  None -> ""
  |Some(k) -> let sk=string_of_int k in 
              "<span id=\"ln"^sk^"\"><a href=\"#n"^sk^"\">("^sk^")</a></span>";;

let u7 = Image.image ( fun (pair,opt)->(pair,write_link opt) ) u6;;

let new_text = Strung.replace_ranges_in u7 old_text ;;

Io.overwrite_with ap1 new_text ;;

let ap1 = Absolute_path.of_string (home^"/Teuliou/html_files/Fortescue/papacy_451.html");; 
let old_text = Io.read_whole_file ap1 ;;
let v1 = Enumerate_html_footnotes.main old_text ;;
let see = Image.image (fun ((i_start,i_end),_)->
    Cull_string.interval old_text i_start i_end) v1 ;;   
let good_indices = List.filter (fun k->not(List.mem k bad_indices )) (Ennig.ennig 1 (List.length v1));;
let reindexation = Image.image (fun (i,j)->(j,i)) (Ennig.index_everything good_indices) ;;
let v2 = Image.image (
  fun ((footnote_idx,html_content),(i_start,i_end))->
    ((footnote_idx,html_content),(i_start,i_end),List.assoc_opt footnote_idx reindexation)
) v1;;
let write_reindexed_version ((i_start,i_end),(footnote_idx,html_content),opt_idx)=
   let new_text = (match opt_idx with 
      None -> ""
      |Some(k)->let sk=string_of_int k in 
      "<div id=\"n"^sk^"\"><a href=\"#ln"^sk^"\">("^sk^")</a> "^html_content^"</div>"
   ) in 
   ((i_start,i_end),new_text);;
let v3 = Image.image write_reindexed_version v2;;
let new_text = Strung.replace_ranges_in v3 old_text ;;
Io.overwrite_with ap1 new_text ;;





(************************************************************************************************************************
Snippet  17 : Remove contiguous lines in a file
************************************************************************************************************************)
open Needed_values ;;

let the_ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/Fortescue/papacy_451.html");; 

let old_text = Io.read_whole_file the_ap ;;

let to_be_deleted = Lines_in_string.interval old_text 3387 4948 ;;

Replace_inside.replace_inside_file (to_be_deleted,"") the_ap ;; 


(************************************************************************************************************************
Snippet  16 : Put fillable footnotes in an html draft 
************************************************************************************************************************)
let write1 k=
  let sk = string_of_int k in 
  "<span id=\"ln"^sk^"\"><a href=\"#n"^sk^"\">("^sk^")</a></span>"^
  "\n\n\n"^
  "<div id=\"n"^sk^"\"><a href=\"#ln"^sk^"\">("^sk^")</a>   \n\n "^
  "</div>" ;;

let dir = (Sys.getenv "HOME")^"/Teuliou/html_files/Fortescue";;  
let ap =   Absolute_path.create_file_if_absent (dir^"/pra_filled.html") ;;

let memo = String.concat "\n\n" (Ennig.doyle write1 121 170) ;;

Io.overwrite_with ap memo ;; 

(************************************************************************************************************************
Snippet  15 : Aggregate pages
************************************************************************************************************************)
let home = Sys.getenv "HOME" ;;
let workdir = home^"/Downloads/Adrifor";;

Coherent_pdf.workspace_directory := workdir ;;

Coherent_pdf.extract_page_range "main" (5,5) ;;

Coherent_pdf.implode ("p","") ;;

Coherent_pdf.merge ["part1";"part2";"part3"] "whole";;

(************************************************************************************************************************
Snippet  14 : Cleaning up and fixing a chaotic mass download
************************************************************************************************************************)
let downloads_s_dir = home ^ "/Downloads";; 

let u1 = More_unix.quick_beheaded_complete_ls downloads_s_dir  ;;
let u2 = List.filter (Substring.is_the_beginning_of "iau") u1;;

let p_value s =
     let j1 = Substring.leftmost_index_of_in_from "-" s 5 in 
     let j2 = Substring.leftmost_index_of_in_from "-" s (j1+1) in
     int_of_string(Cull_string.interval s (j1+1) (j2-1));; 

let min_pageNumber = 9 and max_pageNumber = 70 ;; 

let pre_u3 = Image.image (fun s->(s,p_value s)) u2 ;;
let (bad_ones1,u3) = List.partition (fun (s,p)->(p<min_pageNumber) || (p>max_pageNumber)) pre_u3 ;;
let cmds1 = Image.image (fun (s,_)->"rm "^downloads_s_dir^"/"^s) bad_ones1;;
let act1 () = Image.image Sys.command cmds1 ;;

let reached_page_numbers = Ordered.sort Total_ordering.for_integers (Image.image snd u3) ;; 

let u4 = Ennig.doyle (
   fun p->(p,Option.filter_and_unpack (fun (s,q)->if q=p then Some s else None) u3)
) min_pageNumber max_pageNumber;;

let u5 = List.filter (fun (p,representatives) -> List.length(representatives)>1) u4 ;;
let bad_ones2 =List.flatten 
  (Image.image (fun (p,representatives) -> List.tl(representatives)) u5);;
let cmds2 = Image.image (fun s->"rm "^downloads_s_dir^"/"^s) bad_ones2;;
let act2 () = Image.image Sys.command cmds2 ;;

let bad_ones3 = Option.filter_and_unpack 
  (fun (p,representatives) -> 
     if List.length(representatives)=0 then Some p else None) u4 ;;

    
let cmds3 = Image.image (fun (p,l)->
    let fn = List.hd l in 
    let sk = string_of_int(p-6) in 
    "mv "^downloads_s_dir^"/"^fn^" "^downloads_s_dir^"/p"^sk^".pdf") u4;;
let act3 () = Image.image Sys.command cmds3 ;;

let workdir = home^"/Downloads/";;

Coherent_pdf.workspace_directory := workdir ;;

Coherent_pdf.implode ("p","") ;;



(************************************************************************************************************************
Snippet  13 : Update footnote format in old phpbb text
************************************************************************************************************************)
open Needed_values ;;

let home = Sys.getenv "HOME" ;;

let ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/OCRed_texts/barenton_on_loisy.txt");;

let main_text = Io.read_whole_file ap ;; 

let opening_tag= "[color=blue]";;
let closing_tag = "[/color]";;

let u1 = Substring.occurrences_of_in opening_tag main_text ;;
let u2 = Substring.occurrences_of_in closing_tag main_text ;;

let u3 = List.combine u1 u2;;
let opening_length = String.length opening_tag ;;
let closing_length = String.length closing_tag ;;
let u4 = Image.image (fun (old_a,old_b)->
    let a = old_a+opening_length and b=old_b-1 in 
    ((a,b),Cull_string.interval main_text a b)
    ) u3;;
exception RA of string ;; 
    
let rhine_analysis s=
  let j1 = Substring.leftmost_index_of_in "(" s in 
  let j2 = Substring.leftmost_index_of_in ")" s in 
  if (j1<0)||(j2<0) then raise(RA(s)) else    
  let idx = int_of_string(Cull_string.interval s (j1+1) (j2-1)) in 
  (idx,Cull_string.cobeginning j2 s);;

let u5 = Image.image (
   fun ((a,b),text) -> (((a,b),text),rhine_analysis text)
)  u4 ;; 

let (redundant_u6,redundant_u7)=List.partition (fun (_,(idx,content))->content="") u5;;

let u6 = Image.image (fun (((a,b),text),(idx,content))->
      ((a-opening_length,b+closing_length),idx) ) redundant_u6;;

let u7 = Image.image (fun (((a,b),text),(idx,content))->
        ((a-opening_length,b+closing_length),idx,content) ) redundant_u7;;      

let check1 = ( (List.length u6) = (List.length u7) ) ;;        

let u8 = Image.image (
   fun ((a,b),idx,content) ->
      let s_idx=string_of_int idx in 
      ((a,b),"[size=90][b][color=blue]("^s_idx^")[/color][/b]"^content^"[/size]")
) u7;;
let corrected_text = Strung.replace_ranges_in u8 main_text;;

Io.overwrite_with ap corrected_text ;;



(************************************************************************************************************************
Snippet  12 : Combinatorial musings
************************************************************************************************************************)
exception Hard_computation of string * int ;;

let translate_all t (n,sols)=
  let increment = (if t="1" then 1 else 0) in  
  (n+increment,Image.image (fun u->t^u) sols) ;;

let synthesize_after_translating (n1,sols1) (n2,sols2) =
    if n1 < n2 then (n2,sols2) else 
    if n2 < n1 then (n1,sols1) else (n1,sols1@sols2);; 

let synthesize res1 res2 = 
    synthesize_after_translating 
     (translate_all "0" res1) (translate_all "1" res2);; 


let main_hashtbl = ((Hashtbl.create 50): (string * int, int * (string list)) Hashtbl.t);; 

let am x y = Hashtbl.add main_hashtbl x y;;

let eval_at_one pattern =
     if pattern="" then (1,["1"]) else 
     (if (String.get pattern 0)='F' then  (1,["1"]) else (0,["0"]) );;

let enforce_conditions pattern = 
    let m = String.length pattern in 
    let temp2 = Ennig.doyle (fun j->
        if (j<5)&&(j<>2) then "N" else 
        if j>m then "F" else Cull_string.interval pattern j j) 1 (max 4 m) in 
    String.concat "" temp2;;     

(*

enforce_conditions "A";;
enforce_conditions "ANPE";;

*)

let prepare_computation pattern=
   if pattern="" then ("",Some"NFNN") else 
   let tail = Cull_string.cobeginning 1 pattern in 
   (
    if (String.get pattern 0)='F'
    then (tail,Some(enforce_conditions tail))  
    else (tail,None)    
   )      ;;

let left_n_decomposition pattern =
     let j1 = Strung.char_finder_from (fun c->c<>'N') pattern 1 in 
     if j1=0 
     then (String.length pattern,"") 
     else (j1-1,Cull_string.cobeginning (j1-1) pattern);; 

(*

left_n_decomposition "ABC";;
left_n_decomposition "NNABC";;

*)


let eval_quickly pattern n =
    if n=0 then (0,[""]) else
    if n=1 then eval_at_one pattern else
    match Hashtbl.find_opt main_hashtbl (pattern,n) with 
     Some(l,sol)->(l,sol)
     |None -> raise(Hard_computation(pattern,n)) ;;

let eval_using_left_n_decomposition old_pattern n =
    if n=0 then (0,[""]) else
    if n=1 then eval_at_one old_pattern else
    let old_length = String.length old_pattern in 
    let pattern = (if old_length > n 
                   then Cull_string.beginning n old_pattern
                   else old_pattern) in 
    let (number_of_ns,core) = left_n_decomposition pattern in 
    let (size_of_sols,old_sols) = eval_quickly core (n-number_of_ns) in 
    let new_sols =(
       if number_of_ns=0 
      then old_sols 
      else  let offset = String.make number_of_ns '0' in 
      Image.image (fun t->offset^t) old_sols
      ) in 
    (size_of_sols,new_sols);; 


let eval_slowly pattern n =
       try eval_using_left_n_decomposition pattern n with _->
         let (passive_case,opt_active_case) = prepare_computation pattern in 
         let case0 =  translate_all "0" (eval_using_left_n_decomposition passive_case (n-1)) in 
         match opt_active_case with 
         None -> case0
        |Some(active_case) ->
          let case1 =  translate_all "1" (eval_using_left_n_decomposition active_case (n-1)) in 
          synthesize_after_translating case0 case1;;     

let consider pattern n=
   let res = eval_slowly pattern n in 
   let _= (am (pattern,n) res) in res ;;

let ff n = eval_slowly "" n;;

let bf n = Image.image ff (Ennig.ennig 1 n);;



consider "" 2 ;;
consider "" 3 ;;
consider "FN" 2;;
consider "" 4;;
for k=3 to 30 do let _ = consider "FNN" k in ();let _=consider "" (k+2) in () done ;;


let res1 = Ennig.doyle (fun x->fst(ff x)) 1 30;;




(************************************************************************************************************************
Snippet  11 : Massive conversion of audios into videos using ffmepgs
************************************************************************************************************************)
let base1 =
  [
  
  "001_Ier_D_de_l_Avent_01_12_2013_27min33.mp";
  "002_IIe_dimanche_de_l_Avent_Moulins_09_12_1990_35min32.mp";
  "003_IIIe_dimanche_de_l_Avent_Gaudete_En_Anjou_16_12_1990_29min26.mp";
  "004_IIIe_dim_de_l_Avent_Vendee_16_12_2018.mp";
  "005_IVe_dimanche_de_l_Avent_Comparaison_du_temps_des_Patriarches_avec_le_notre_Moulins_23_12_1990_28min19_Copie_en_conflit_de_debian_2019_11_13.mp";
  "006_Vigile_Nativite_Choix_et_gouts_de_Dieu_Moulins_24_12_1990_18min52.mp";
  "007_Vigile_de_Noel_En_Vendee_24_12_2012_29min31.mp";
  "008_Messe_de_Minuit_Anniversaire_naissance_de_la_fille_ainee_de_l_Eglise_Moulins_25_12_1996_28min04.mp";
  "009_Messe_du_jour_de_Noel_Divinite_du_Christ_Seigneur_demontree_par_S_Paul_aux_Hebreux_a_partir_de_l_Ancien_Testament_Moulins_25_12_1996_19min48.mp";
  "010_Nativite_Messe_de_minuit_En_Vendee_25_12_2012_14min17.mp";
  "011_Nativite_Messe_du_jour_En_Vendee_25_12_2012_11min26.mp";
  "012_Dimanche_dans_l_Octave_de_la_Nativite_Dum_medium_silentium_Tours_31_12_1989_21min22.mp";
  "013_Dimanche_dans_l_octave_de_NoeI_30_12_2012_21min08.mp";
  "014_Dimanche_dans_l_Octave_de_NoeI_ND_de_l_Epine_29_12_2019_28_min.mp";
  "015_Circoncision_En_Vendee_01_01_2013_26min22.mp";
  "016_Saint_Nom_de_Jesus_En_Vendee_02_01_2013_22min24.mp";
  "017_Epiphanie_06_01_1996_38min27.mp"; "018_Epiphanie_07_01_90_29min34.mp";
  "019_Octave_de_l_Epiphanie_Manifestation_de_la_Divinite_de_NS_Moulins_13_01_1991_38min17.mp";
  "020_La_Sainte_Famille_Moulins_07_01_1996_31min33.mp";
  "021_IIe_dimanche_apres_l_Epiphanie_Mayenne_ND_de_l_Epine_19_01_2014_30min50.mp";
  "022_IIIe_dimanche_apres_l_Epiphanie_Noli_vinci_a_malo_sed_vince_in_bono_malum_25_01_90_34min31.mp";
  "023_Septuagesime_Deux_genres_de_conversion_Moulins_27_01_1991_28min27.mp";
  "024_Sexagesime_ND_de_Lourdes_Montee_de_l_esprit_anti_chretien_et_apparitions_de_ND_Moulins_11_02_1996_33min36.mp";
  "025_Quinquagesime_Annonce_prophetique_de_la_Passion_Moulins_10_02_1991_24min39.mp";
  "026_Ier_dimanche_de_Careme_Sens_mystique_des_montees_vers_Jerusalem_16_02_1997_15min18.mp";
  "027_Ier_dimanche_de_Careme_Sur_la_Penitence_Moulins_04_03_1990_24min53.mp";
  "028_IIe_Dim_de_Careme_Transfiguration_Equilibre_entre_desolations_et_consolations_Moulins_24_02_1991_34min49.mp";
  "029_IIIe_dimanche_de_Careme_Contre_le_demon_muet_Moulins_10_03_1996_26min07.mp";
  "030_IIIe_dimanche_de_Careme_Sur_l_Annonciation_Maternite_virginale_et_voeu_de_virginite_22_03_92.mp";
  "031_IVe_dimanche_de_Careme_Joie_dans_la_penitence_et_Montee_du_Carmel_10_03_91_26min06.mp";
  "032_Ier_dimanche_de_la_Passion_ReveIation_progressive_de_la_divinite_de_NS_Moulins_05_04_1992_40min02.mp";
  "033_Dimanche_des_Rameaux_En_Vendee_01_04_2012_16min12.mp";
  "034_Veillee_pascale_Sur_l_illogisme_de_l_attitude_actuelle_des_Juifs_talmudistes_Moulins_11_04_1998_26min11.mp";
  "035_Paques_Moulins_04_04_1996_44min07.mp";
  "036_Dimanche_in_albis_Quasi_modo_Les_corps_glorieux_Tours_02_04_1989_19min22.mp";
  "037_IIe_dimanche_apres_Paques_Misericorde_et_Justice_de_Dieu_equilibre_de_l_esprit_chretien_Fete_de_saint_Pierre_de_Verone_29_04_1990_30min12.mp";
  "038_IIIe_dimanche_apres_Paques_Modicum_et_videbitis_Me_ND_de_l_Epine_Mayenne_21_04_2013_26min12.mp";
  "039_IVe_dimanche_apres_Paques_Attachement_apostolique_a_NS_pour_remonter_de_sa_nature_humaine_a_sa_Divinite_28_04_1991_23min03.mp";
  "040_Ve_dimanche_apres_Paques_Dieu_console_par_les_siens_Tours_30_04_1989_20min42.mp";
  "041_Ascension_24_05_1990_25min18.mp";
  "042_Ascension_Moulins_09_05_1991_27min55.mp";
  "043_Ascension_2011_Vendee_20min32.mp";
  "044_Ascension_ND_de_l_Epine_10_05_2018_26min12.mp";
  "045_Dimanche_dans_l_Octave_de_l_Ascension_12_05_2013_Mayenne_ND_de_l_Epine_15min28.mp";
  "046_Dimanche_dans_l_Octave_de_l_Ascension_2014_Vendee_19min.mp";
  "047_Pentecote_Moulins_19_05_1991_29min18.mp";
  "048_Pentecote_Vendee_19_05_2013_30min45.mp";
  "049_Pentecote_ND_de_l_Epine_15_05_2016_30min_50.mp";
  "050_Tres_Sainte_Trinite_Moulins_16_06_1990_33min05.mp";
  "051_Fete_du_Tres_Saint_Sacrement_Moulins_02_06_1991_28min53.mp";
  "052_Dimanche_dans_l_octave_du_Saint_Sacrement_Notre_Dame_de_l_Epine_2_06_2013_14min26.mp";
  "053_Solennite_du_Sacre_Coeur_et_Saint_Jean_Baptiste_Il_faut_qu_Il_croisse_et_que_je_diminue_Moulins_24_06_1990_24min55.mp";
  "054_Dimanche_dans_l_Octave_du_Sacre_Coeur_29min09.mp";
  "055_IVe_dimanche_apres_la_Pentecote_Moulins_16_06_1991_31min54.mp";
  "056_Ve_dimanche_apres_la_Pentecote_Sur_reparation_et_componction_Tours_18_06_1989_34min59.mp";
  "057_VIe_dimanche_apres_la_Pentecote_Saint_Henri_et_la_sanctification_dans_le_monde_Moulins_15_07_1990_37min39.mp";
  "058_VIIe_Dimanche_apres_la_Pentecote_Mayenne_ND_de_l_Epine_07_07_2013_36min04.mp";
  "059_VIIIe_dimanche_apres_la_Pentecote_Saint_Bonaventure_et_le_bonheur_en_Dieu_seul_Moulins_14_07_1991_42min06.mp";
  "060_IXe_dimanche_apres_la_Pentecote_Mayenne_ND_de_l_Epine_21_07_2013_34min55.mp";
  "061_Xe_dimanche_apres_la_Pentecote_Sur_le_Principe_et_Fondement_Tours_31_07_1988_22min08.mp";
  "062_XIe_dimanche_apres_la_Pentecote_04_08_1991_33min37.mp";
  "063_XIe_Dimanche_apres_Pentecote_12_08_2012_15min44.mp";
  "064_XIIe_dimanche_apres_la_Pentecote_11_08_2013_27min32.mp";
  "065_XIIIe_dimanche_apres_la_Pentecote_18_08_2013_27min34.mp";
  "066_XIVe_dimanche_apres_la_Pentecote_Saint_Louis_25_08_1991.mp";
  "067_XVe_dimanche_apres_la_Pentecote_Foi_en_la_Divinite_de_NS_Moulins_16_09_90_27min23.mp";
  "068_XVIe_dimanche_apres_la_Pentecote_03_09_89_24min15.mp";
  "069_XVIIe_Dimanche_apres_la_Pentecote_ND_de_l_Epine_16_09_2018_40_min.mp";
  "070_XVIIIe_Dim_apres_la_Pentecote_ND_de_l_Epine_8_10_2017_38min30.mp";
  "071_XIXe_dimanche_apres_la_Pentecote_Sur_la_colere_24_09_89_22min01.mp";
  "072_XXe_D_ap_Pent_22_10_2017_ND_de_l_Epine_1h.mp";
  "073_Fete_du_Christ_Roi_27_10_1991_27min22.mp";
  "074_XXIe_dimanche_apres_Pentecote_25min24.mp";
  "075_XXIIe_Dimanche_apres_la_Pentecote_ND_de_l_Epine_21_10_2018_36_min_35.mp";
  "076_XXIIIe_dimanche_apres_la_Pentecote_Sur_le_Purgatoire_11_11_90.mp";
  "077_XXIVe_et_dernier_dimanche_apres_la_Pentecote_Moulins_26_11_1989_26min38.mp";
  "078_XXIVe_ap_Pent_IVe_ap_Eph_Tempete_apaisee_Moulins_29min55.mp";
  "079_XXVe_dim_ap_Pent_Ve_ap_Epiphanie_Moulins_04_02_1990_20min.mp";
  "080_XXVIe_dim_apres_la_Pentecote_VIe_ap_Epiphanie_17_11_1991_30min38.mp";
  "081_Solennite_du_Tres_Saint_Rosaire_Tours_08_10_1989_23min31.mp";
  "082_Maternite_Divine_de_Notre_Dame_11_10_2015_Mayenne_ND_de_l_Epine_28min20.mp";
  "083_Toussaint_Moulins_01_11_1996_30min49.mp";
  "084_Commemoration_des_defunts_Vendee_2_novembre_2012_21min07.mp";
  "085_Fete_de_la_Dedicace_de_Saint_Jean_de_Latran_09_11_2014_33min47.mp";
  "086_Solennite_de_l_Immaculee_Conception_Moulins_1989_30min05.mp";
  "087_Presentation_de_NS_au_temple_et_Purification_de_Marie_Mayenne_ND_de_l_Epine_02_02_2014_23min13.mp";
  "088_Decouverte_de_la_Sainte_Croix_ND_de_l_Epine_3_05_2015_34min28.mp";
  "089_St_Philippe_et_st_Jacques_ND_de_l_Epine_11_05_2014_18min42_.mp";
  "090_Solennite_de_Ste_Jeannes_d_Arc_Moulins_13_05_1990_27min49.mp";
  "091_VIe_dimanche_apres_la_Pentecote_Solennite_de_St_Pierre_et_St_Paul_Monde_conquis_de_haute_lutte_par_papes_et_martyrs_Moulins_30_06_1991_42min50.mp";
  "092_Fete_de_Sainte_Anne_Mayenne_ND_de_l_Epine_26_07_2015_42min41.mp";
  "093_Saint_Laurent_diacre_et_martyr_Vendee_10_08_2014_33min24.mp";
  "094_Fete_de_saint_Luc_Mayenne_ND_de_l_Epine_18_10_2015_27min09.mp";
  "095_Sur_la_maniere_de_precher_1988_22min32.mp";
  "096_Assomption_Moulins_15_08_1991_29min15.mp"; "097_Assomption_2012.mp";
  "098_Assomption_2013_Vendee_21min34.mp";
  "099_Saint_Joachim_Pere_de_la_TS_Vierge_Marie_Vendee_16_08_2015_24min03.mp";
  "100_Tres_Precieux_Sang_Mayenne_ND_de_l_Epine_1er_juillet_2013_27min03.mp";
  "101_Nativite_de_Notre_Dame_Moulins_08_09_1991_30min28.mp";
  "102_Notre_Dame_des_sept_douleurs_15_09_1996_15min51.mp";
  "103_Solennite_de_Ste_Therese_de_l_Enfant_Jesus_et_de_la_Ste_Face_Moulins_30_09_1990_22min47.mp";
  "104_Solennite_de_saint_Michel_Archange_Moulins_29_09_1991_32min31.mp";
  "105_IVe_D_ap_Pentecote_ND_de_l_Epine_28_juin_2020_35min.mp";
  "106_VIe_D_ap_Pentecote_ND_de_l_Epine_12_7_2020_39min25.mp";
  "107_refutation_T_de_M_ete_2013_Intro_10min.mp";
  "108_refutation_T_de_M_ete_2013_partie_1_18min19.mp";
  "109_refutation_T_de_M_ete_2013_partie_2_18min41.mp";
  "110_refutation_T_de_M_ete_2013_partie_3_23min22.mp";
  "111_refutation_T_de_M_ete_2013_partie_4_14min54.mp";
  "112_refutation_T_de_M_ete_2013_partie_5_21min57.mp";
  "113_refutation_T_de_M_ete_2013_partie_6_21min51.mp";
  "114_refutation_T_de_M_ete_2013_partie_7_17min02.mp";
  "115_VIIIe_D_Ap_Pent_7_8_2011_a_La_Boutouere_Mayenne_21min57.mp";
  "116_XVIe_Dimanche_apres_la_Pentecote_ND_de_lEpine_20_09_2020_22min14.mp";
  "117_Christ_Roi_25_10_2020_ND_de_lEpine_45min29.mp";
  "118_Presentation_des_ouvrages_de_labbe_Zins_video_001_52min.mp";
  "119_Presentation_des_ouvrages_de_labbe_Zins_video_002_1h.mp";
  "120_Presentation_des_ouvrages_de_labbe_Zins_video_003_55min.mp";
  "121_Presentation_des_ouvrages_de_labbe_Zins_video_004_42min.mp"
  
  ];;

let n1 = List.length base1 ;;

let main_list = String.concat "\n" base1 ;; 




let write1 x =  
    let idx = string_of_int(int_of_string(String.sub x 0 3)) in 
    "wget -c http://larchange.org/audio/"^x^"3\n"^
    "ffmpeg -loop 1 -i STP.jpg -i "^x^"3 -acodec copy -vcodec libx265 -shortest "^x^"4\n"^
    "rm "^x^"3\n"^
    "echo $'\\n\\n\\n\\n\\n\\n Step "^idx^" of 121 finished\\n\\n\\n\\n\\n\\n'" ;; 

let home = Sys.getenv "HOME" ;;    
let ap_for_main_script = Absolute_path.of_string 
    (home^"/Teuliou/Bash_scripts/Convert_audio_to_video/audiotovideo.sh");;    

let main_script = String.concat "\n\n\n" (Image.image write1 base1) ;;      

let fill_main_script () =
   Io.overwrite_with ap_for_main_script main_script ;;

fill_main_script () ;;


let base2 = Ennig.index_everything base1;;
let (pre_part1,remains1) = List.partition (fun (j,x)->j<=40) base2;;
let (pre_part2,pre_part3) = List.partition (fun (j,x)->j<=80) remains1;;
let part1 = Image.image (fun (_,s)->s^"4") pre_part1;;
let part2 = Image.image (fun (_,s)->s^"4") pre_part2;;
let part3 = Image.image (fun (_,s)->s^"4") pre_part3;;

let main_dir = (Sys.getenv "HOME")^"/Teuliou/Bash_scripts/Convert_audio_to_video/Sermons";;
let cmds1 = Image.image (
  fun x->"mv "^main_dir^"/"^x^" "^main_dir^"_1_a_40/"
) part1 ;;
let cmds2 = Image.image (
  fun x->"mv "^main_dir^"/"^x^" "^main_dir^"_41_a_80/"
) part2 ;;
let cmds3 = Image.image (
  fun x->"mv "^main_dir^"/"^x^" "^main_dir^"_81_a_117/"
) part3 ;;
let cmds = cmds1 @ cmds2 @ cmds3 ;;





(************************************************************************************************************************
Snippet  10 : Removing misinterpreted characters from a freshly OCR-ed doc
************************************************************************************************************************)
open Needed_values ;;

let home = Sys.getenv "HOME" ;;

let dirname = "Lossky";;
let num_of_pages = 196 ;;    

let partial_texts = Ennig.doyle (fun k->
    let sk = string_of_int k in 
    let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
    let prelude="% Beginning of page "^sk^"\n"
    and postlude="\n% End of page "^sk in 
    prelude^(rf fn)^postlude)  1 num_of_pages ;;
      
let full_ap = Absolute_path.create_file_if_absent  
(home^"/Downloads/"^dirname^"/full.txt");;  
let tex_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/pre_vladimir_lossky.txt");; 
 

let full_text = "\n"^(String.concat "\n" partial_texts)^"\n" ;;


let adjusted_text = Replace_inside.replace_several_inside_string 
   ["&","\\&";
    "$","\\$";
    "#","\\#";
    "_","\\_";
    "\194\162","c";
    "\194\176","o";
    "\226\130\172","E"] full_text ;;   
 
Replace_inside.overwrite_between_markers_inside_file 
  (Overwriter.of_string adjusted_text) ("% BEGINNING MARKER","%END MARKER") tex_ap;;


let text1 = Io.read_whole_file tex_ap ;;
let u1 = Substring.occurrences_of_in "% End of page 7\n" text1 ;;
let i1 = List.hd u1 ;;
let u2 = Cull_string.interval text1 (i1-20) i1;;

Replace_inside.replace_several_inside_file
   ["\n\012\n","\n"] tex_ap ;;

(************************************************************************************************************************
Snippet  9 : Typical use of the Trim_text_between_tags module
************************************************************************************************************************)
let home = Sys.getenv "HOME" ;;
let ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/Translations/act_of_body_translated.txt");;

Trim_text_between_tags.in_file [("[i]","[/i]")] ap;;


(************************************************************************************************************************
Snippet  8 : Put fillable footnotes in a phpbb draft 
************************************************************************************************************************)
let write1 k=
  let sk = string_of_int k in 
  "[b][color=blue]("^sk^")[/color][/b]\n\n"^
  "[size=90][b][color=blue]("^sk^")[/color][/b]   [i]   [/i]   [/size]";;

let dir = (Sys.getenv "HOME")^"/Teuliou/html_files/Translations";;  
let ap =   Absolute_path.create_file_if_absent (dir^"/temp.txt") ;;

let memo = String.concat "\n\n" (Ennig.doyle write1 1 5) ;;

Io.overwrite_with ap memo ;; 

(************************************************************************************************************************
Snippet  7 : Finding extremal vertices in a polytope
************************************************************************************************************************)
open Needed_values ;;


let small_n=1;;

let u1 = Cartesian.fifth_power (Ennig.ennig 0 small_n);;

let u2 = Option.filter_and_unpack (
  fun (a1,a2,a3,a4,a5)->
      let a6 = a3+a4-a5
      and a7 = a2+a4-a5
      and a8 = a1-a4+a5 in
      if List.for_all (fun x->(x>=0)) [a6;a7;a8] 
      then Some(a1,a2,a3,a4,a5,a6,a7,a8)   
    else None
) u1 ;;


let u3 = List.tl u2 ;; 

let shadow (a1,a2,a3,a4,a5,a6,a7,a8) =
    let l = [a1;a2;a3;a4;a5;a6;a7;a8] in 
    Set_of_polys.safe_set(List.filter (fun j->List.nth l (j-1)=0) [1; 2; 3; 4; 5; 6; 7; 8]) ;;

let supporting_rel uple uple2 =
    if uple=uple2 then false else 
    Set_of_polys.is_included_in (shadow uple) (shadow uple2);;  
     
let supporters uple = List.filter (supporting_rel uple ) u3;;

let u4 = Option.filter_and_unpack (
   fun uple -> let r= supporters uple in 
   if r<>[]
    then Some(uple,r)
  else None
) u3;;

let u5 = List.filter (fun uple->(supporters uple)=[]) u3;;

[[0, 0, 0, 1, 1, 0, 0, 0],
   [0, 0, 1, 0, 0, 1, 0, 0], [0, 0, 1, 1, 1, 1, 0, 0],
   [0, 1, 0, 0, 0, 0, 1, 0], [0, 1, 0, 1, 1, 0, 1, 0],
   [0, 1, 1, 0, 0, 1, 1, 0], [0, 1, 1, 0, 1, 0, 0, 1],
   [0, 1, 1, 1, 1, 1, 1, 0], [1, 0, 0, 0, 0, 0, 0, 1],
   [1, 0, 0, 1, 0, 1, 1, 0], [1, 0, 0, 1, 1, 0, 0, 1],
   [1, 0, 1, 0, 0, 1, 0, 1], [1, 0, 1, 1, 0, 2, 1, 0],
   [1, 0, 1, 1, 1, 1, 0, 1], [1, 1, 0, 0, 0, 0, 1, 1],
   [1, 1, 0, 1, 0, 1, 2, 0], [1, 1, 0, 1, 1, 0, 1, 1],
   [1, 1, 1, 0, 0, 1, 1, 1], [1, 1, 1, 0, 1, 0, 0, 2],
   [1, 1, 1, 1, 0, 2, 2, 0], [1, 1, 1, 1, 1, 1, 1, 1]]

(*

let small_n=2;;

let u1 = Cartesian.fourth_power (Ennig.ennig 0 small_n);;

let u2 = Option.filter_and_unpack (
  fun (a1,a2,a3,a5)->
      let a4 = small_n-(a1+a2+a3) 
      and a6 = small_n-(a1+a2+a5)
      and a7 = small_n-(a1+a3+a5) 
      and a8 = (2*a1+a2+a3+a5)-small_n in
      if List.for_all (fun x->(x>=0)&&(x<=small_n)) [a4;a6;a7;a8] 
      then Some(a1,a2,a3,a4,a5,a6,a7,a8)   
    else None
) u1 ;;

let u3 = List.filter (
   fun (a1,a2,a3,a4,a5,a6,a7,a8) -> 
    List.exists (fun (x,y)->x<>y) [a1,a8;a2,a7;a3,a6;a4,a5]
) u2;;

*)

(************************************************************************************************************************
Snippet  6 : Abandoned code snippet to remove paragraph containing footnotes.
It is much simpler to add html paragraph tags only when the region of text
does not contain footnotes (see the Htmlize module and snippet 4)
************************************************************************************************************************)
exception Unbalanced_html_paragraph_tags of int * int ;;
exception Nested_html_paragraphs of (int * int) * (int * int) ;;

let footnote_marker = ref " nowfeetneto ";;


let html_par_opening_tag = "<p>";;
let html_par_closing_tag = "</p>";;

let op_tag_length = (String.length html_par_opening_tag)-1 ;;
let cl_tag_length = (String.length html_par_closing_tag)-1 ;;

let detect_nested_paragraphs l=
   let temp1 = Listennou.universal_delta_list l in 
   match Option.seek (fun 
     (((i1,j1),(i2,j2)),((i3,j3),(i4,j4)))->i3<j2
   ) temp1 with 
   None -> ()
   |Some(((i1,j1),(i2,j2)),((i3,j3),(i4,j4))) ->
       raise(Nested_html_paragraphs((i2,j2),(i3,j3)));;

let locate_all_paragraphs_in_html txt=
  (* paragraphs are assumed to be non-nested *)
  let temp1 = Substring.occurrences_of_in html_par_opening_tag txt 
  and temp2 = Substring.occurrences_of_in html_par_closing_tag txt in 
  let o1 = List.length temp1 and c1 = List.length temp2 in 
  if o1<>c1 
  then raise(Unbalanced_html_paragraph_tags(o1,c1))
  else   
  let temp3 = Image.image (fun i->(i,i+op_tag_length)) temp1
  and temp4 = Image.image (fun j->(j,j+cl_tag_length)) temp2 in 
  let temp5 = List.combine temp3 temp4 in 
  let _ = detect_nested_paragraphs temp5 in 
  temp5 ;; 

let remove_paragraphs_containing_footnotes txt l= ();;
        



(************************************************************************************************************************
Snippet  5 : Mass deletion of modules 
************************************************************************************************************************)
open Needed_values;;

let u1 = ae ();;

let u2 = List.filter (fun
   (Dfn_endingless_t.J(r,s,m)) -> Dfa_subdirectory.begins_with s 
   (Dfa_subdirectory_t.SD "Text_editing")
) u1 ;;

let u3 = Image.image (fun
(Dfn_endingless_t.J(r,s,m)) -> match m with 
  (Dfa_module_t.M m0) -> m0
) u2;;

let u4 = List.filter (
  fun m0->not(List.mem m0 ["control_pdf_size";"read_russian"])
) u3;;

let u5 = Image.image (
  fun m0->(m0, bel m0)
) u4;;

let u6 = List.filter (
  fun (m0,b0)->List.for_all (fun m1->(List.mem m1 u4)) b0
) u5;;

let u7=List.rev(Image.image fst u6);;

(************************************************************************************************************************
Snippet  4 : Code to OCR-size PDF's into .txt (and later html)
************************************************************************************************************************)
open Needed_values ;;


let write1 k =
    let sk = string_of_int k in 
    "pdftoppm main.pdf p"^sk^" -png -f "^sk^" -singlefile\n"^
    "tesseract -l fra p"^sk^".png p"^sk;;

let dirname = "Pius_XII";;
let num_of_pages = 326 ;;

let ap1 = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/script.sh");;

let text1 = "\n\n\n"^(String.concat "\n" (Ennig.doyle write1 1 num_of_pages))^"\n\n\n" ;;   
    
Io.overwrite_with ap1 text1;;


let partial_texts = Ennig.doyle (fun k->
  let sk = string_of_int k in 
  let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
  "%\n% Page "^sk^" \n%\n"^(rf fn))  7 num_of_pages ;;


let full_ap = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/full.txt");;  

let full_text = String.concat "\n" partial_texts ;;
let full_text = Htmlize.pages partial_texts ;;

Io.overwrite_with full_ap full_text;;

let (page1,page2,ranges_for_lfm,ranges_for_fm) =
   Option.unpack(!(Htmlize.Private.error_handling_ref ));;

(* Re-indexed version *)

let main_list = 
  [51;149;187;189;201;203;231;249;257;261;263;265;269;271;297] ;;

let write1 j =
 let k =List.nth main_list (j-1) in 
 let sj = string_of_int j 
 and sk = string_of_int k in 
 "pdftoppm main.pdf p"^sk^" -png -f "^sj^" -singlefile\n"^
 "tesseract -l fra p"^sk^".png p"^sk;;

let dirname = "Blet_again";;
let num_of_pages = 15 ;;

let ap1 = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/script.sh");;

let text1 = "\n\n\n"^(String.concat "\n" (Ennig.doyle write1 1 num_of_pages))^"\n\n\n" ;;   
 
Io.overwrite_with ap1 text1;;

let partial_texts = Ennig.doyle (fun j->
let k =List.nth main_list (j-1) in   
let sk = string_of_int k in 
let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
let announcer = "%\n% Page "^sk^" \n%\n" in 
(announcer,announcer^(rf fn)))  1 num_of_pages ;;

let end_ap = Absolute_path.of_string 
 (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/end_of_text.txt");; 

let act () = Replace_inside.replace_several_inside_file 
 partial_texts end_ap;;  


(************************************************************************************************************************
Snippet  3 : Typical use of the Read_russian module
************************************************************************************************************************)
open Needed_values ;;

let home = Sys.getenv "HOME";;
let txt1 = rf (home^"/Downloads/temp.txt");;

let z1 = Read_russian.read txt1;;
let z2= Read_russian.prepare_dictation txt1;;

let ap1 = Absolute_path.of_string (home^"/Teuliou/LaTeX/Moullapl/archipelago.tex");;

let act () = 
Replace_inside.overwrite_between_markers_inside_file 
  (Overwriter.of_string z2)
  ("\\begin{document}","\\end{document}") ap1;;

(************************************************************************************************************************
Snippet  2 : Convert footnotes between phpBB and HTML
************************************************************************************************************************)
let peggy j =
   let sj=string_of_int j in 
   "<span id=\""^"ln"^sj^"\"><a href=\"#n"^sj^"\">("^sj^")</a></span>";;
 
 let u1 = Ennig.doyle peggy 3 43;;  
 
 let u2 ="\n\n\n"^(String.concat "\n\n" u1) ^"\n\n\n";;
 
 
 let peggy j =
   let sj=string_of_int j in 
   "<div id=\""^"n"^sj^"\"><a href=\"#ln"^sj^"\">("^sj^")</a> <i> </i>  </div>";;
 
 let u1 = Ennig.doyle peggy 3 43;;  
 
 let u2 ="\n\n\n"^(String.concat "\n\n" u1) ^"\n\n\n";;
 

(************************************************************************************************************************
Snippet  1 : Typical use of the Coherent_pdf module on a freshly scanned doc
************************************************************************************************************************)
let home = Sys.getenv "HOME" ;;
let workdir = home^"/Downloads/Building_Site";;

Coherent_pdf.workspace_directory := workdir ;;

let cmd1 = "cp "^workdir^"/moncunill.pdf "^workdir^"/whole.pdf";;

let act1 () = Sys.command cmd1;;

let act2 () = Coherent_pdf.remove_page_range_in_in_a_total_of 
~range_start:1 ~range_end:1 ~deflated_one:"whole" 
~total_length:28;;

let act3 () = Coherent_pdf.append_on_the_right "whole" "velaza" ;;

let act5 () = Coherent_pdf.transfer_range_to_rightmost
~range_start:91 ~range_end:91 ~deflated_one:"whole" 
~total_length:592 ~receiving_one:"p90" ;;

let act6 ()= Coherent_pdf.insert_in_just_after 
    ~inserted_one: "p90" ~receiving_one: "whole"
     ~page_number:89 ~initial_total_length:591;;

let act7 () = Coherent_pdf.delete_file "p90";;     

let act8 k = Coherent_pdf.replace_page_number_in_by 
~page_number:k ~receiving_one:"whole"  ~inserted_one:("p"^(string_of_int k)) 
~total_length:133;;

Image.image act8 [36;40;42;44;68;70;72;90;100] ;;

let act9 () = Coherent_pdf.extract_page_range "whole" (10,10);;

let act10 () = Coherent_pdf.extract_odd_pages "russia" ;;

let act11 () = Coherent_pdf.intertwine 
~odd_pages:"odd" ~even_pages:"even" 
~num_odd:81  ~num_even:81 ~final_name:"118_to_279" ;;

let phoebe n=
  let q =(n/4) in 
  let r= n - 4*q in 
  (4*q)+List.assoc r [0,(-3);1,2;2,3;3,4;];; 
    
let act12 () =   
   Explicit.image (
     fun k-> 
      let j = phoebe k in 
      Coherent_pdf.rename 
      ("p"^(string_of_int k)) ("q"^(string_of_int j))
   ) (Ennig.ennig 1 260) ;;

let act13 ()= Coherent_pdf.implode ("q","") ;; 

let workdir = home^"/Downloads";;

Coherent_pdf.workspace_directory := workdir ;;

let act1 () = Coherent_pdf.extract_page_range "mariage" (24,24);;

let act2 ()= Coherent_pdf.insert_in_just_after 
    ~inserted_one: "xx" ~receiving_one: "mariage"
     ~page_number:22 ~initial_total_length:732;;

let act3 () = Coherent_pdf.remove_page_range_in_in_a_total_of 
~range_start:25 ~range_end:25 ~deflated_one:"mariage" 
~total_length:733;;

let act4 ()= Coherent_pdf.insert_in_just_after 
    ~inserted_one: "p272" ~receiving_one: "mariage"
     ~page_number:315 ~initial_total_length:732;;

