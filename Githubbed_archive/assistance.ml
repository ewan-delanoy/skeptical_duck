(*

#use"Githubbed_archive/assistance.ml";;

In an emergency situation, open a fresh terminal, load this 
file with the above command and call 
Assistance_usual_coma_state.refresh() )

#load"unix.cma";;
#load"str.cma";;
#use"Ordinary/Githubbed_archive/assistance.ml";;

*)




module Assistance_tools_for_absolute_path=struct

(*

#use"tools_for_absolute_path.ml";;

Standardize filename path. Non-directories never 
end with /, directories always do (exceptions : the iterated_container 
functions below, note that 
Sys.getcwd() does not follow this convention ).


*)

let remove_trailing_slash s=
    let n=String.length(s) in
    if ((String.get s (n-1))='/')
    then String.sub s 0 (n-1)
    else s;;

exception Number_of_double_points_exn;;

let number_of_double_points s=
  let n=String.length(s) in
  let rec tempf=(fun j->
     let k=(3*j) in
     if (n<k+2) then j else
     if (String.get s k='.')&&(String.get s (k+1)='.')
     then if n=(k+2) then j+1 else
          if (String.get s (k+2)='/') 
          then tempf(j+1)
          else raise(Number_of_double_points_exn)
     else j
  ) in
  tempf(0);;
  
  
let helper_for_iterated_container j0 s=
   let rec tempf=(fun (j,k)->
     if j<1 then (String.sub s 0 k) else
     let i=String.rindex_from(s)(k-1)('/') in
     tempf(j-1,i)
     ) in
    tempf (j0,String.length s);;
 
exception Too_many_double_points;;  
 
 let iterated_container j0 s=try helper_for_iterated_container j0 s with
   any_exn->raise(Too_many_double_points);;

exception Blank_filename;;

let delete_left_blanks s=
  let n=String.length(s) in
  let rec tempf=(fun j->
    if j>=n then raise(Blank_filename) else
    if String.get(s)(j)=' '
    then tempf(j+1)
    else j
  ) in
  let j0=tempf 0 in
  String.sub s j0 (n-j0);;

let parse_unix_filename_shortcuts_from_dir dir s0=
  let dir_without_the_slash = remove_trailing_slash dir in  
  let s1=delete_left_blanks(s0) in
  let dp1=number_of_double_points(s1) in
  if (dp1>0) 
  then  let smaller_pwd=iterated_container dp1 dir_without_the_slash in
        let j1=(3*dp1)-1 in 
         smaller_pwd^(String.sub s1 j1 ((String.length s1)-j1) )    
  else
  if s1="/" then "/" else
  match String.get(s1)(0) with
  '/'->s1
  |'~'->(Sys.getenv "HOME")^(String.sub s1 1 (String.length(s1)-1))
  |'.'->if s1="." 
        then dir_without_the_slash
        else dir^(String.sub s1 2 (String.length(s1)-2))
  |arall->dir^s1;;

let parse_unix_filename_shortcuts =
  parse_unix_filename_shortcuts_from_dir ((Sys.getcwd())^"/");;
  
 
  
 exception Inexistent_file of string;; 
  
 let opt_of_string s=
  let s0=parse_unix_filename_shortcuts(s) in
  if Sys.file_exists(s0)
  then if s0="/" then Some s0 else
       let s1=remove_trailing_slash s0 in
       if Sys.is_directory s1
       then Some(s1^"/")
       else Some s1
  else None;;
  
 let of_string s=
   match opt_of_string s with 
   Some result -> result  
   |None -> raise(Inexistent_file(s));; 



end;;






module Assistance_absolute_path=struct

(*

#use"absolute_path.ml";;

*)

type t=AP of string;;

let of_string s=AP(Assistance_tools_for_absolute_path.of_string s);;


let to_string (AP s)=s;;

let ocaml_name ap=
 let s=to_string ap in
"Absolute"^"_path"^"."^"of_string(\""^s^"\"";;

let test_equal_paths s1 s2=
((of_string s1)=(of_string s2));;

exception Error_during_file_creation;;
exception Error_during_unix_command of string;;

let uc cmd = 
   let i= Sys.command cmd in 
   if i<>0 then raise(Error_during_unix_command cmd) else ();;


let create_file_if_absent w=
    let cr=(fun w->
      let ld=Unix.openfile w [Unix.O_RDWR;Unix.O_CREAT;Unix.O_EXCL] 0o666 in
       Unix.close ld
    ) in
    if Sys.file_exists w then of_string w else
    if (not(String.contains w '/'))
    then (cr w;of_string w)
    else 
    let i=String.rindex w '/' in
    let basedir=String.sub w 0 i
    and filename=String.sub w (i+1) ((String.length w)-(i+1)) in
    let g1="jnoxgghg_"^filename in
    let _=(uc ("mkdir -p "^basedir);
           uc ("touch "^g1); 
           uc ("mv "^g1^" "^w);
           uc ("rm -f "^g1)) in 
    of_string w;;
    
let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;

           

end;;






module Assistance_ennig=struct

(*

#use"ennig.ml";;

*) 
 let doyle f a b=
 let accu=ref([]) in
 let rec doyle0=(function
 j->if j<a
    then (!accu)
	else let _=(accu:=f(j)::(!accu)) in doyle0(j-1)
 ) in
 doyle0 b;;
 
 let slow_doyle f a b=
 let accu=ref([]) in
 let rec slow_doyle0=(function
 j->if j>b
    then List.rev(!accu)
	else let _=(accu:=f(j)::(!accu)) in slow_doyle0(j+1)
 ) in
 slow_doyle0 a;;
 
 
 let doyle_for_delta f n u0=
 let accu=ref([u0]) and traveler=ref(u0) in
 let rec doyle0=(function
 da_ober->if da_ober<1
          then List.rev(!accu)
	      else let _=(traveler:=f(!traveler);accu:=(!traveler)::(!accu)) in 
	           doyle0(da_ober-1)
 ) in
 doyle0 n;;
  
 
let ennig a b=doyle (function x->x) a b;; 
 
let index_everything l=
  let rec tempf=
   (function (j,graet,da_ober)->
     match da_ober with
      []->graet
     |a::b->tempf(j-1,(j,a)::graet,b)
    )    in
    tempf(List.length(l),[],List.rev(l));;

let for_all f a b=
 let rec for_all0=(function
 j->if j>b
    then true
	else if f(j)
	     then for_all0(j+1)
		 else false
 ) in
 for_all0 a;;

let rec exists f a b=
if (a>b) 
then false
else if f(a)
	 then true
	 else exists f (a+1) b;;	 
 
let rec find_it f a b=
if (a>b) 
then None
else if f(a)
	 then Some(a)
	 else find_it f (a+1) b;;	  

let rec find_and_stop f a b=
 let rec find_and_stop0=(function
  j->if (j>b)
     then None
	 else match f(j) with
		None->find_and_stop0(j+1)
		|Some(x)->Some(x)
 ) in
 find_and_stop0 a;;

let constant_list n x=doyle (function j->x) 1 n;;

let describe_fibers_as_intervals f a b=
  if (a>b) then [] else
  let rec tempf=(function
    (graet,x1,x2,y0)->
       if (x2>=b) then List.rev((x1,x2,y0)::graet) else
       let x3=x2+1 in
       let y3=f(x3) in
       if (y3=y0)
       then tempf(graet,x1,x3,y0)
       else tempf((x1,x2,y0)::graet,x3,x3,y3)
  
  ) in
  tempf([],a,a,f(a));;

let reposition_by_putting_snd_immediately_after_fst i j t=
     if t<=i then t else
     if t=i+1  then j else
     if t<=j  then t-1 else t;;



           

end;;






module Assistance_characters_in_namespace_name=struct

(*

#use"characters_in_namespace_name.ml";;

*)


let chars=
  (Assistance_ennig.doyle char_of_int 65 90)@
  (Assistance_ennig.doyle char_of_int 97 122)@
  (Assistance_ennig.doyle char_of_int 48 57)@
  ['\\';'_'];;
           

end;;






module Assistance_charset=struct

(*

#use"charset.ml";;

*)

let lowercase_letters=    
  ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
   'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
   'u';'v';'w';'x';'y';'z'];;

    
let uppercase_letters= 
   ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
    'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';
    'U';'V';'W';'X';'Y';'Z'];;
    
let anycase_letters=
    lowercase_letters@uppercase_letters;;

let lowercase_identifier_elements=    
    ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
     'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
     'u';'v';'w';'x';'y';'z';'_';'+';'-';'*';
     '0';'1';'2';'3';'4';'5';'6';'7';'8';'9']@uppercase_letters;;
     
let php_label_first_letters =
  [
    'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
    'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
    'u';'v';'w';'x';'y';'z';
    'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
    'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';
    'U';'V';'W';'X';'Y';'Z';
    '_';
    ];;  

 let php_label_nonfirst_letters =
  php_label_first_letters
  @
  [
   '0';'1';'2';'3';'4';'5';'6';'7';'8';'9'
  ];;   

let ocaml_modulename_nonfirst_letters=
  ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o';
 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'; 'A'; 'B'; 'C'; 'D';
 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S';
 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'; '_'; '0'; '1'; '2'; '3'; '4'; '5'; '6';
 '7'; '8'; '9'];;

let alphanumeric_characters =
  php_label_nonfirst_letters @
  [
   '.';'\''
  ];;    

let unix_filename_admissible_characters =
  php_label_nonfirst_letters @
  [
   '.';'/';'!';'~';
  ];;        
    
let list_of_whites=[' ';'\n';'\r';'\t'];; 
  
let classlike_declaration_chars=
    list_of_whites@Assistance_characters_in_namespace_name.chars;;  

let enclosers=[
      '(',')';
      '[',']';
      '{','}';
];;
           

end;;






module Assistance_option=struct

(*

#use"option.ml";;

*) 

exception Unpackable of string;;

module Private = struct 

let unpack_with_error_message s=function
None->raise(Unpackable(s))
|Some(x)->x;;

end ;;


let add_element_on_the_right l x=match x with
  None->l
  |Some(a)->l@[a];;
 
let rec filter_and_unpack f l=
 let rec filter0=(function
  (graet,da_ober)->match da_ober with
   []->List.rev(graet)
   |x::peurrest->match f(x) with
		None->filter0(graet,peurrest)
		|Some(y)->filter0(y::graet,peurrest)
 ) in
 filter0([],l);;

let  find_and_stop f l=
 let rec find_and_stop0=(function
  da_ober->match da_ober with
   []->None
   |a::peurrest->match f(a) with
		None->find_and_stop0(peurrest)
		|Some(x)->Some(x)
 ) in
 find_and_stop0(l);;

let propagate f=function
None->None
|Some(x)->Some(f(x));;

let rec seek f =function
[]->None
|a::b->if f(a) then Some(a) else seek(f)(b);;

let unpack x =Private.unpack_with_error_message "void is not unpackable" x;;





 


end;;






module Assistance_image=struct

(*

#use"image.ml";;
The most used function in all those modules !


*)


let image f l=
  let rec tempf=(fun
   (graet,da_ober)->match da_ober with
   []->List.rev graet
   |a::peurrest->tempf(f(a)::graet,peurrest)
  ) in
  tempf([],l);;



end;;






module Assistance_listennou=struct

(*

#use"listennou.ml";;

*)



exception Ht_exn;;
exception Reposition_first_key_not_found;;
exception Reposition_second_key_not_found;;
exception Push_immediately_after_exn;;


let ht x=match x with
    []->raise(Ht_exn)
    |a::b->(a,b);;

let rec uncurrified_rev_append (x,y)=match x with
[]->y
|a::peurrest->uncurrified_rev_append (peurrest,a::y);;

let rec uncurrified_append (x,y)=uncurrified_rev_append (List.rev x,y);;

let factor (x,y)=
    let rec factor0=(fun
       (graet,da_ober1,da_ober2)->
       if (da_ober1=[])||(da_ober2=[])
       then (List.rev graet,da_ober1,da_ober2)
       else let (a1,peurrest1)=ht da_ober1
            and (a2,peurrest2)=ht da_ober2 in
            if a1=a2
            then factor0(a1::graet,peurrest1,peurrest2)
            else (List.rev graet,da_ober1,da_ober2)
    ) in
    factor0([],x,y);;

let comparable_for_prefix_order  a b=
    let (_,a1,b1)=factor(a,b) in (a1=[])||(b1=[]);;

let extends l1 l2=
   let (_,_,r2)=factor (l1,l2) in r2=[];;


let didrochan x=
let rec didrochan0=
(function (u,accu1,accu2,bowl)->match u with
 []->(accu1,accu2)
 |a::b->if bowl
        then didrochan0(b,a::accu1,accu2,false)
        else didrochan0(b,accu1,a::accu2,true))  
in
didrochan0(x,[],[],true);;

let find_index x ll=
let rec sub_f=
(function (j,l)->match l with
[]->(-1)      
|u::v->if u=x then j else sub_f(j+1,v)) in
sub_f(1,ll);;

exception Force_find_exn ;;

let rec force_find f x=
   match x with 
   [] -> raise(Force_find_exn)
   |a::others -> if f a 
                 then a 
                 else force_find f others ;; 

let morzholan f x=
let rec sub_f=(function (u,v)->if u=v then u else sub_f(v,f(v)))
in sub_f(x,f(x));;

let rec morzhol_bihan f k x=
if k=0 then x else morzhol_bihan f (k-1) (f(x));;

exception Big_rht_exn of int*int;;

let big_rht r l=let rec tempf=
(function (j,kleiz,dehou)->
if j=0 then (kleiz,dehou) else 
match dehou with
[]->raise(Big_rht_exn(r,List.length l))
|a::peurrest->tempf(j-1,a::kleiz,peurrest)
) in
tempf(r,[],l);;

let big_head r l=if (r>(List.length l)) then l else List.rev(fst(big_rht(r)(l)));;

let big_tail r l=if (r>(List.length l)) then [] else snd(big_rht(r)(l));;

let remove_element_at_idx l k=
   let (kleiz,dehou)=big_rht k l in 
   List.rev_append (List.tl kleiz) dehou;;

(* remove_element_at_idx [1; 2; 3; 4; 5; 6; 7] 3;; *)   

let decompose_wrt_two_indices l i j=
   let (r_part1,temp1)=big_rht (i-1) l in 
   let (ei,temp2)=ht temp1 in 
   let (r_part2,temp3)=big_rht (j-i-1) temp2 in 
   let (ej,part3)=ht temp3 in 
   (List.rev r_part1,ei,List.rev r_part2,ej,part3);;

(* decompose_wrt_two_indices [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12] 3 7;; *)

let extract_interval l i j=
  let (r_part1,temp1)=big_rht (i-1) l in 
  let (r_part2,part3)=big_rht (j-i+1) temp1 in 
  (List.rev r_part1,List.rev r_part2,part3);;

(* extract_interval [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12] 3 7;; *)

let decompose_wrt_element l elt1=
  let rec tempf=(
     fun (treated,to_be_treated)->match to_be_treated with 
     []->(List.rev treated,false,[])
    |elt::other_elts ->
       if elt=elt1
      then (List.rev(treated),true,other_elts)
      else tempf(elt::treated,other_elts)
  ) in 
  tempf([],l);; 

(* decompose_wrt_element [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12] 3;; *)



let reposition_by_putting_snd_immediately_after_fst l elt_i elt_j=
    let (left1,found1,right1)=decompose_wrt_element l elt_i in 
    if not found1 then raise(Reposition_first_key_not_found) else 
    let (left2,found2,right2)=decompose_wrt_element right1 elt_j in 
    if not found2 then raise(Reposition_second_key_not_found) else
    left1@(elt_i::elt_j::(left2@right2));; 
  
(* reposition_by_putting_snd_immediately_after_fst [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12] 3 7;; *)  


let power_set l=
let rec tempf=
(function (da_ober,graet)->match da_ober with
[]->graet
|a::peurrest->tempf(peurrest,graet@(Assistance_image.image(function y->a::y)(graet)))
) in
tempf(List.rev(l),[[]]);;


let fold_right f x0 l=List.fold_left(function x->(function a->f a x)) x0 l;;



let universal_delta_list l=
let rec sub_f=
(function (accu,a,rl)->match rl with
[]->List.rev(accu)
|b::x->sub_f((a,b)::accu,b,x)
) in
match l with
[]->[]
|u::v->sub_f([],u,v);;

 
let delete_redundancies r l=
 let rec tempf=(function
   (graet,da_ober)->match da_ober with
   []->List.rev(graet)
   |x::peurrest->
     if List.exists(function y->r y x)(peurrest)
     then tempf(graet,peurrest)
     else let temp1=List.filter(function y->not(r x y))(peurrest) in
          tempf(x::graet,temp1)
 ) in
 tempf([],l);;

let nonredundant_version l=
  let rec tempf=(
    fun (graet,da_ober)->
      match da_ober with
      []->List.rev graet
      |a::peurrest->if List.mem a graet
                    then tempf(graet,peurrest)
                    else tempf(a::graet,peurrest)
  ) in
  tempf([],l);;

let rev_map f l=
   let rec tempf=(
     fun (graet,da_ober)->match da_ober with
     []->graet
     |a::peurrest->tempf((f a)::graet,peurrest)
   ) in
   tempf([],l);;
   
let redundant_indices l=
  let rec tempf=(
    fun (counter,already_known,bad_indices,to_be_treated)->
      match to_be_treated with
      []->List.rev bad_indices
      |a::others->
        let idx=counter+1 in  
        if List.mem a already_known
        then tempf(idx,already_known,idx::bad_indices,others)
        else tempf(idx,a::already_known,bad_indices,others)
  ) in
  tempf(0,[],[],l);;

(*
redundant_indices [1; 2; 1; 4; 5; 6; 3; 8; 9; 10; 11; 12; 13; 6; 15];;
*)

let divide_by_two l=
   let rec tempf=(
     fun (treated,to_be_treated)->match to_be_treated with 
     []->(List.rev treated,None)
     |a1::others1->(
         match others1 with 
         []->(List.rev treated,Some(a1))
         |a2::others->tempf((a1,a2)::treated,others)
      )
   ) in 
   tempf ([],l);;

let push_immediately_after l elt2 elt1 =
  let rec tempf=(
    fun (treated,to_be_treated)->match to_be_treated with 
     []->raise(Push_immediately_after_exn)
    |elt::others ->
      if elt=elt1
      then List.rev_append treated (elt::elt2::others)
      else tempf(elt::treated,others)
  ) in 
  tempf([],l);; 

let hi=List.length;;
let rev=List.rev;;

(*

push_immediately_after [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12] 145 3;;

*)

let partition_from_set_of_ranges l n=
    if l=[] then [1,n,false] else 
    let (last_i,last_j)=List.hd(List.rev l) 
    and (first_i,_)=List.hd l in
    let temp2=universal_delta_list l in  
    let temp3=Assistance_image.image (fun ((i1,j1),(i2,j2))->
      [(i1,j1,true);(j1+1,i2-1,false)]
    ) temp2 in 
    let middle_part=List.flatten temp3 in
    let first_part=(if first_i>1 then [(1,first_i-1,false)] else []) 
    and last_part=(if last_j<n then [(last_j+1,n,false)] else []) in 
    first_part@middle_part@[(last_i,last_j,true)]@last_part;;

(*

partition_from_set_of_ranges [(3,7);(41,52)] 100;;
partition_from_set_of_ranges [(1,7);(41,52)] 100;;

*)

let extract_intervals_in_complement l n =
   let enhanced_l = 0::(l@[n+1]) in 
   let temp1=universal_delta_list enhanced_l in
   let temp2= Assistance_image.image (fun (x,y)->(x+1,y-1)) temp1 in 
   List.filter (fun (a,b)->a<=b) temp2;;

(*

extract_intervals_in_complement [3;7;8;20] 30;;
extract_intervals_in_complement [1;7;8;20] 30;;
extract_intervals_in_complement [1;7;8;30] 30;;

*)   

let complement_union_of_ranges ranges n=
   let rec tempf=(fun 
     (already_treated,a,b,to_be_treated)->
       match to_be_treated with 
       []->List.rev((a,b)::already_treated)
       |(x1,y1)::other_ranges->
         tempf((a,x1-1)::already_treated,y1+1,b,other_ranges)
   ) in 
   let temp1=tempf([],1,n,ranges) in 
   List.filter (fun (x,y)->x<=y) temp1;;

(*

complement_union_of_ranges [3,7;8,20] 30;;
complement_union_of_ranges [3,7;9,20] 30;;
complement_union_of_ranges [1,7;9,20] 30;;
complement_union_of_ranges [1,7;9,30] 30;;
complement_union_of_ranges [1,7;8,30] 30;;

*)


let split_list_in_half l=
   let temp1=Assistance_ennig.index_everything(l) in 
   let (temp2,temp3)=List.partition (fun (j,_)->(j mod 2)=1) temp1 in 
   (Assistance_image.image snd temp2,Assistance_image.image snd temp3);;

(*

split_list_in_half [1; 2; 3; 4; 5; 6; 7];;
split_list_in_half [1; 2; 3; 4; 5; 6; 7; 8];;

*)   



let unequal_combine l1 l2 =
   let rec tempf=(fun
     (treated,to_be_treated1,to_be_treated2)->
       match to_be_treated1 with 
       []->List.rev(treated)
       |a1::others1->(
                       match to_be_treated2 with 
                        []->List.rev(treated)
                        |a2::others2 -> tempf((a1,a2)::treated,others1,others2)
                     )
   ) in 
   tempf([],l1,l2);;

exception Fst_is_largest of int * int;;
  

let unequal_combine_where_fst_is_smallest l1 l2 =
   let n1=List.length(l1) and n2=List.length(l2) in 
   if n1>n2 then raise(Fst_is_largest(n1,n2)) else   
   unequal_combine l1 l2;;


exception  Extract_successive_pairs_exn of int;;

let extract_successive_pairs_from_even_list l=
   let m1 =(List.length l) in 
   if (m1 mod 2)<>0 then raise(Extract_successive_pairs_exn(m1)) else 
   let m2=m1/2 in 
   Assistance_ennig.doyle (fun j->
      (List.nth l (2*j-2),List.nth l (2*j-1)) 
   ) 1 m2;;

let remove_initial_contaminated_elements contamination_test all_elts =
      let rec tempf =(
         fun (beginning,l)-> match l with 
         [] -> (beginning,[])
         |a :: b -> if  contamination_test a 
                    then tempf (a::beginning,b) 
                    else (beginning,l)
      ) in 
      tempf ([],all_elts) ;;

(*

remove_initial_contaminated_elements (fun x->x<=100) [2;3;507;1;4;30];;

*)

let start_separating is_sep is_not_sep elts =
     let (_,temp1) = remove_initial_contaminated_elements is_sep elts in 
     remove_initial_contaminated_elements is_not_sep temp1;; 
      
let separate_according_to elts separators =      
   let is_sep  = (fun x->List.mem x separators) 
   and is_not_sep = (fun x->not(List.mem x separators))  in 
   let rec tempf = (fun (treated,to_be_treated)-> 
       if to_be_treated=[]
       then List.rev treated 
      else let (half1,half2)= start_separating is_sep is_not_sep to_be_treated in 
           let treated2 =(
                if half1=[] 
                then treated 
                else (List.rev half1)::treated 
           ) in 
           tempf(treated2,half2)          
   ) in 
   tempf([],elts);;

(*

separate_according_to  [1;2;3;0;4;0;0;5;6;0;0;0;7;0;8;0] [0];;
separate_according_to  [0;0;1;2;3;0;4;0;0;5;6;0;0;0;7;0;8;0] [0];;
*)


let partition_according_to_fst pairs=
  let rec tempf = (fun (already_treated,to_be_treated)->
       match to_be_treated with 
        [] -> List.rev already_treated 
       |(a0,_) :: _ ->
         let (part1,part2) = List.partition (fun (a,b)->a=a0) to_be_treated in 
         tempf ((a0,Assistance_image.image snd part1)::already_treated,part2)     
   ) in 
   tempf ([],pairs) ;;
  
exception Compute_largest_connected_interval_on_the_left_exn ;;

let compute_largest_connected_interval_on_the_left initial_l =
  let rec tempf = (fun  (a,b,l)->
   match l with 
   [] -> ((a,b),[])
   |head :: tail -> if head = b+1 
                    then tempf(a,b+1,tail)
                    else ((a,b),l)  
  ) in  
  match initial_l with 
  [] -> raise Compute_largest_connected_interval_on_the_left_exn
  | head2 :: tail2 -> tempf (head2,head2,tail2) ;;

let decompose_into_connected_components l=
  let rec tempf = (fun 
     (treated,to_be_treated)->
     if to_be_treated = [] then List.rev treated else 
     let (interval,others) =  compute_largest_connected_interval_on_the_left to_be_treated in 
     tempf (interval::treated,others)
  ) in 
  tempf ([],l);;

(*

decompose_into_connected_components [3; 4; 5; 6; 7; 10; 11; 12; 13; 14; 15; 31; 32; 33; 34; 35; 36; 37; 38; 39; 40;
 41; 42; 43; 44; 45; 46; 47; 48];;

*)


let replace_if_possible l x=
  match List.assoc_opt x l with 
  None -> x 
  |Some y -> y ;;

end;;






module Assistance_prepared=struct

(*

#use"prepared.ml";;

*)

module Private = struct

let filter f=function
[]->[]
|l->
 let rec filter_easily0=(function
 (graet,da_ober)->match da_ober with
   []->List.rev(graet)
   |a::peurrest->if f(a) 
				 then filter_easily0(a::graet,peurrest)
                 else List.rev(graet)
 ) in
 filter_easily0([],l);; 

let partition f=function
[]->[]
|x::y->
 let rec partition_easily0=(function
 (graet,y,la,da_ober)->match da_ober with
   []->List.rev(List.rev(la)::graet)
   |b::peurrest->let z=f(b) in
                 if z=y
				 then partition_easily0(graet,y,b::la,peurrest)
                 else partition_easily0(List.rev(la)::graet,z,[b],peurrest)
 ) in
 partition_easily0([],f(x),[x],y);;  
 

 let write_interval (i,j)=match (j-i) with
  0->string_of_int(i)
  |1->(string_of_int i)^","^(string_of_int j)
  |arall->"["^((string_of_int i)^".."^(string_of_int j))^"]";;
 
 let doyle f a b=
  if (b<a) then [] else
   let rec tempf=(function
  (graet,y,i0,current_i)->
     if (current_i>b)
     then List.rev( (write_interval(i0,current_i-1),y) ::graet)
     else let z=f(current_i) in
          if z=y
          then tempf(graet,y,i0,current_i+1)
          else tempf( (write_interval(i0,current_i-1),y) ::graet, z,current_i,current_i+1)
   ) in
   tempf([],f(a),a,a+1);;
 
 let partition_according_to_fst=function
[]->[]
|(x1,y1)::lost->
 let rec partition_easily0=(function
 (graet,xi,ly,da_ober)->match da_ober with
   []->List.rev((xi,List.rev(ly))::graet)
   |(x,y)::peurrest->
                 if x=xi
				 then partition_easily0(graet,xi,y::ly,peurrest)
                 else partition_easily0((xi,List.rev(ly))::graet,x,[y],peurrest)
 ) in
 partition_easily0([],x1,[y1],lost);;  

end ;; 

let partition_in_two_parts f l=
   let rec tempf=(fun
    (graet,da_ober)->match da_ober with
      []->(List.rev graet,[])
      |a::peurrest->
         if f(a)
         then tempf(a::graet,peurrest)
         else (List.rev graet,da_ober)
   ) in
   tempf([],l);; 
 


let rec wait_and_take_the_rest f l =match l with 
  [] -> []
  |a :: b-> if f a then l else wait_and_take_the_rest f b;;
                
(* wait_and_take_the_rest (fun x->x>=7) (Ennig.ennig 1 20) ;; *)                

end;;






module Assistance_supstring=struct

(*

#use"supstring.ml";;

*)



let begins_with y x=
      let lx=String.length(x) in
      if String.length(y)<lx
      then false
      else (String.sub y 0 lx)=x;;  
   
 let ends_with y x=
      let lx=String.length(x) in
      if String.length(y)<lx
      then false
      else (String.sub y ((String.length y)-lx) lx)=x;;  
   
 
let contains y x=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      Assistance_ennig.exists tester 0 (String.length(y)-lx);;               

end;;






module Assistance_substring=struct

(*

#use"substring.ml";;

*)

 let is_the_beginning_of y x=Assistance_supstring.begins_with x y;;     

   
 let is_the_ending_of y x=Assistance_supstring.ends_with x y;;  

 let is_a_substring_located_at y x old_j =
    let j=old_j-1 in
    let ly=String.length(y) in
      if (String.length(x)<j+ly)||(j<0)
      then false
      else (String.sub x j ly)=y;;
 
  let is_a_substring_of x y=Assistance_supstring.contains y x;; 
      
  let leftmost_index_of_in x y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      try (Assistance_option.unpack(Assistance_ennig.find_it tester 0 (String.length(y)-lx))+1) with
      _->(-1);;
  
  let rightmost_index_of_in x y=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) 
      and temp1=List.rev(Assistance_ennig.ennig(0)(String.length(y)-lx)) in
      try ((Assistance_listennou.force_find tester temp1)+1) with
      _->(-1);;
  
   let leftmost_index_of_in_from x y i=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      match Assistance_ennig.find_it tester (i-1) (String.length(y)-lx) with
         None->(-1)
        |Some(k)->k+1;;
  
module Friend = struct

let number_of_lines_before s i=
   if i<1 then 0 else
   let m=min i (String.length s) in
   List.length(List.filter(fun j->(String.get s (j-1))='\n')(Assistance_ennig.ennig 1 m));;


end;;

let leftmost_linedex_of_in x y=
    let j=leftmost_index_of_in x y in
    if j<0 then (-1) else
    Friend.number_of_lines_before y j;;



let leftmost_linedex_of_in_from x y i=
        let j=leftmost_index_of_in_from x y i in
        if j<0 then (-1) else
        Friend.number_of_lines_before y j;;    



let leftmost_index_of_pattern_among_in_from patterns whole_string start_idx=  
    let n=String.length(whole_string) in
    let temp1=Assistance_ennig.index_everything patterns in 
    let tester =(fun idx->Assistance_option.find_and_stop (
         fun (patt_nbr,patt)->
           if is_a_substring_located_at patt whole_string idx 
           then Some(patt_nbr,idx)
           else None
       ) temp1) in
    Assistance_option.find_and_stop tester (Assistance_ennig.ennig start_idx n);;          
      
(*

leftmost_index_of_pattern_among_in_from ["uv";"abc";"abcde"] "123abcde90" 1;;

*)

let occurrences_of_in x y=
   let n=String.length y in
   let rec tempf=(fun (j,accu)->
      if j>n then List.rev(accu) else
      let k=leftmost_index_of_in_from x y j in
      if k<0 then List.rev(accu) else
      tempf(k+1,k::accu)
   )  in
   tempf (1,[]);;

let ranges_for_occurrences_of_in x y=
   let m=String.length x in
   let temp1 = occurrences_of_in x y in 
   Assistance_image.image (fun i->(i,i+m-1)) temp1;;   

end;;






module Assistance_cull_string=struct

(*

#use"cull_string.ml";;

*)




let interval s a b=String.sub s (a-1) (b-a+1);;

let neighborhood_with_center_and_size s i d=
   let a=max(1)(i-d)
   and b=min(String.length s)(i+d) in
   interval s a b;;

exception Beginning_failure;;

let beginning k s=
   if k<1 then "" else
   let n=String.length(s) in
   if (k>n)
   then raise(Beginning_failure)
   else String.sub s 0 k;;
   
exception Ending_failure;;   
   
 let ending k s=
   if k<1 then "" else
   let n=String.length(s) in
   if (k>n)
   then raise(Ending_failure)
   else String.sub s (n-k) k;;
    
 let cobeginning k s=ending (String.length(s)-k) s;; 
 
 let coending k s=beginning (String.length(s)-k) s;; 
 
 let resize_from_left s p c=
   let d=p-String.length(s) in
   if d>0
   then s^(String.make d c)
   else beginning p s;;
   
  let resize_from_right s p c=
   let d=p-String.length(s) in
   if d>0
   then (String.make d c)^s
   else ending p s;;  
     

let before_and_after w x=
  let j=Assistance_substring.leftmost_index_of_in(w)(x) in
  if j=(-1) then None else 
   Some(  beginning (j-1) x,
    cobeginning (j+String.length(w)-1) x);;

let complement_union_of_ranges ranges s=
   let n=String.length s in 
   let temp1=Assistance_listennou.complement_union_of_ranges ranges n in 
   Assistance_image.image (fun (u,v)->interval s u v) temp1;;

let extract_intervals_in_wrt_separator s sep =
  let d=String.length(sep)-1 in 
  let occurrences = Assistance_substring.occurrences_of_in sep s in 
  let ranges = Assistance_image.image ( fun start ->(start,start + d)) occurrences in 
  complement_union_of_ranges ranges s;;    

(*
extract_intervals_in_wrt_separator "123+ab+++c+d+45+678+" "+" ;;

extract_intervals_in_wrt_separator "123a4ab56ab789ab" "ab" ;;

*)

let remove_chars_in_set_on_the_left l s=
      let n=String.length s in
      match Assistance_option.seek(fun j->
          not(List.mem (String.get s (j-1)) l)
      )(Assistance_ennig.ennig 1 n) with
      None->""
      |Some(d)->cobeginning (d-1) s;;

let remove_chars_in_set_on_the_right l s=
      let n=String.length s in
      match Assistance_option.seek(fun j->
          not(List.mem (String.get s (n-j)) l)
      )(Assistance_ennig.ennig 1 n) with
      None->""
      |Some(d)->coending (d-1) s;;

let trim_spaces_on_the_left =remove_chars_in_set_on_the_left [' ';'\t';'\r';'\n'];;

let trim_spaces_on_the_right = remove_chars_in_set_on_the_right [' ';'\t';'\r';'\n'] ;;

let trim_slashes_on_the_right =remove_chars_in_set_on_the_right ['/'];;
   
              

 let trim_spaces s=
   let n=String.length s in
   let opt1=Assistance_option.seek(fun j->not(List.mem(String.get s (j-1)) [' ';'\r';'\t';'\n']))(Assistance_ennig.ennig 1 n) in
   if opt1=None then "" else
   let i1=Assistance_option.unpack opt1 in
   let k1=Assistance_listennou.force_find(fun j->not(List.mem(String.get s (n-j)) [' ';'\r';'\t';'\n']))(Assistance_ennig.ennig 1 n) in 
   let j1=(n+1)-k1 in
   interval s i1 j1;;

exception Two_sided_cutting_exn of int*int*int;;

let two_sided_cutting (left_part,right_part) s=
   let n=String.length s 
   and l=String.length left_part 
   and r=String.length right_part in 
   let d=n-(l+r) in 
   if n<l+r
   then raise(Two_sided_cutting_exn(n,l,r)) 
   else String.sub s l d;;

(*

two_sided_cutting ("ab","efg") "abcdefg";;

*)      

 let closeup_around_index s j=
   let n=String.length s in
   let temp1=List.filter(fun j->(String.get s (j-1))='\n')(Assistance_ennig.ennig 1 n) in
   let (temp2,temp3)=Assistance_prepared.partition_in_two_parts(fun k->k<j) temp1 in
   let a=(if List.length(temp2)<6 then 1 else List.nth(List.rev temp2)(5))
   and b=(if List.length(temp3)<6 then n else List.nth(temp3)(5)) in
   String.sub s (a-1) (b-a);;
   
exception Absent_beginning_marker of string;;
exception Absent_ending_marker of string;; 
 
let between_markers (bm,em) s=
     if (bm,em)=("","") then s else
     let i1=Assistance_substring.leftmost_index_of_in_from bm s 1  in
     if i1<1 then raise(Absent_beginning_marker(bm)) else
     let j1=i1+(String.length bm) in
     let i2=Assistance_substring.leftmost_index_of_in_from em s (j1+1) in
     if i2<1 then raise(Absent_ending_marker(bm)) else
     interval s j1 (i2-1);; 
 
let optional_between_markers p s=
   try Some(between_markers p s) with _->None;; 
   
(*

between_markers ("aaa","bb") "123aaa45bb678";;

*)     
   
let split_wrt_rightmost s c=
   let i=(try String.rindex(s)(c) with _->(-1)) in
   if i<0
   then ("",s)
   else (String.sub s 0 i,String.sub s (i+1) ((String.length s)-i-1) );;

let before_rightmost s c=fst(split_wrt_rightmost s c);;
let after_rightmost s c=snd(split_wrt_rightmost s c);;

let before_rightmost_possibly_all s c=
   let i=(try String.rindex(s)(c) with _->(-1)) in
   if i<0
   then s
   else String.sub s 0 i;;

let shorten_blanks s=
   let blanks = [' ';'\n';'\r';'\t'] in 
   let n = String.length s in 
   let test_idx = (fun j->
       if j=1 then true else 
       let c = String.get s (j-1)  in 
       if not(List.mem c blanks) then true else 
       let d = String.get s (j-2)  in  
       not(List.mem d blanks) 
      ) in 
   let temp1 = List.filter test_idx (Assistance_ennig.ennig 1 n) in 
   let temp2 = Assistance_image.image (fun j->String.make 1 (String.get s (j-1))) temp1 in 
   let temp3 = String.concat "" temp2 in 
   trim_spaces temp3 ;;

(*

shorten_blanks " \n 123\r \n45 \n\n6\n  7\t 89\n";;

*)    

  
             

end;;






module Assistance_max=struct

(*

#use"max.ml";;

*) 


let list=function 
[]->failwith("max of empty set undefined according to Garfield")
|a::b->List.fold_left(max)(a)(b);;

let maximize_it f=function
[]->failwith("max on empty set undefined")
|x::y->
 let rec maximize_it0=(function
  (current_candidate,current_value,da_ober)->match da_ober with
  []->(current_candidate,current_value)
  |a::peurrest->let va=f(a) in
                if (va>current_value)
				then maximize_it0(a,va,peurrest)
				else maximize_it0(current_candidate,current_value,peurrest)
 ) 
in
 maximize_it0(x,f(x),y);;
 
let maximize_it_if_possible f l=
   let temp1=Assistance_option.filter_and_unpack (function 
     None->None
    |Some(x)->Some(x,f x) ) l in
   if temp1=[]
   then None
   else Some(fst(maximize_it(snd) temp1));;
 

let maximize_it_with_care f=function
[]->failwith("careful max on empty set undefined")
|x::y->
 let rec maximize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                if (va>current_value)
				then maximize_it_with_care0([a],va,peurrest)
				else if (va=current_value)
				     then maximize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else maximize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 maximize_it_with_care0([x],f(x),y);;
           

end;;






module Assistance_min=struct

(*

#use"min.ml";;

*) 


let list=function 
[]->failwith("min of empty set undefined")
|a::b->List.fold_left(min)(a)(b);;

let minimize_it f=function
[]->failwith("min on empty set undefined")
|x::y->
 let rec minimize_it0=(function
  (current_candidate,current_value,da_ober)->match da_ober with
  []->(current_candidate,current_value)
  |a::peurrest->let va=f(a) in
                if (va<current_value)
				then minimize_it0(a,va,peurrest)
				else minimize_it0(current_candidate,current_value,peurrest)
 ) 
in
 minimize_it0(x,f(x),y);;

let minimize_it_if_possible f l=
   let temp1=Assistance_option.filter_and_unpack (function 
     None->None
    |Some(x)->Some(x,f x) ) l in
   if temp1=[]
   then None
   else Some(fst(minimize_it(snd) temp1));;

let minimize_it_with_care f=function
[]->failwith("careful min on empty set undefined")
|x::y->
 let rec minimize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                if (va<current_value)
				then minimize_it_with_care0([a],va,peurrest)
				else if (va=current_value)
				     then minimize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else minimize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 minimize_it_with_care0([x],f(x),y);;
           

end;;






module Assistance_total_ordering=struct

(*

#use"total_ordering.ml";;

*)

type result=Lower |Equal |Greater;;

type 'a t=('a->'a->result);;
let leq (computer:'a t) x y=
   let v=computer(x)(y) in
   (v=Lower)||(v=Equal);;
   
 let lt (computer:'a t) x y=(computer(x)(y)=Lower);;   
 
 let geq (computer:'a t) x y=
   let v=computer(x)(y) in
   (v=Lower)||(v=Equal);;
   
 let gt (computer:'a t) x y=(computer(x)(y)=Greater);;   
 
 let from_lt f=
   let temp1=(fun x y->
     if f(x)(y)
     then Lower
     else if f(y)(x)
          then Greater
          else Equal
   ) in
   (temp1:'a t);;
 
 let standard_completion f g=
  let answer=(fun x y->
   if f(y)(x)
   then Greater
   else if f(x)(y)
        then Lower
        else if g(x)(y)
             then Equal
             else if x<y
                  then Lower
                  else Greater
  ) in
  (answer: 'a t);;
 
 let standard=((fun x y->
    if x=y
    then Equal
    else if x<y
         then Lower
         else Greater
 ): 'a t);;
 
let standard2=((fun (x1,y1) (x2,y2)->
    let t1=standard x1 x2 in 
    if t1<> Equal 
    then t1
    else standard y1 y2
 ): ('a * 'b) t);;

 let completion f (g:'a t)=
  let answer=(fun x y->
   if f(y)(x)
   then Greater
   else if f(x)(y)
        then Lower
         else g(x)(y)
  ) in
  (answer: 'a t);;
 
let combine=((fun ~tried_first ~tried_second->
  (fun x y->
   let first_trial = tried_first x y in 
   if first_trial <> Equal 
   then first_trial
   else tried_second x y
  ) ): 
    tried_first:('a t) -> tried_second:('a t) -> ('a t)
  );;

 let product (f:'a t) (g:'b t)=
  ((fun (x1,y1) (x2,y2)->
     let t=f(x1)(x2) in
     if t<>Equal 
     then t
     else g y1 y2
 ): ('a*'b) t);;
 
 let triple_product (f:'a t) (g:'b t) (h:'c t)=
  ((fun (x1,y1,z1) (x2,y2,z2)->
     let tx=f(x1)(x2) in
     if tx<>Equal 
     then tx
     else let ty=g(y1)(y2) in
          if ty<>Equal 
          then ty
          else h z1 z2
 ): ('a*'b*'c) t);;
 
 let rec lex_compare (f:'a t)=
  let rec tempf=(
    fun l1 l2->
     match l1 with 
     []->(if l2=[] then Equal else Lower)
     |a1::b1->
      (
        match l2 with 
        []->Greater
        |a2::b2->
          let t=f(a1)(a2) in
           if t<>Equal then t else
           tempf b1 b2
      )) in
     (tempf:>( ('a list) t));;
 


let silex_compare (f:'a t)=
  let tempf=(
    fun l1 l2->
     let t=standard(List.length l1)(List.length l2) in
     if t<>Equal then t else
     lex_compare f l1 l2
  ) in
   (tempf:>( ('a list) t));;
 

let from_list (l:'a list)=
  let tempc=(fun x y->
  let rec tempf=(function
   []->(x<y)
   |u::peurrest->if u=x then List.mem(y)(peurrest)
                 else if u=y then false
                 else tempf(peurrest)
  ) in
  tempf l) in
  from_lt tempc;;

let min (f:'a t)=function
 []->failwith("Min of the empty set is undefined")
 |a::b->
   let rec tempf=(fun
    (candidate,l)->match l with
      []->candidate
      |c::peurrest->if f(c)(candidate)=Lower
                    then tempf(c,peurrest)
                    else tempf(candidate,peurrest)
   ) in
   tempf(a,b);;

let max (f:'a t)=function
 []->failwith("Max of the empty set is undefined")
 |a::b->
   let rec tempf=(fun
    (candidate,l)->match l with
      []->candidate
      |c::peurrest->if f(c)(candidate)=Greater
                    then tempf(c,peurrest)
                    else tempf(candidate,peurrest)
   ) in
   tempf(a,b);;
   
let minimize_it_with_care (cf:'a t) 
   f=function
[]->failwith("careful min on empty set undefined")
|x::y->
 let rec minimize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                let howl=cf(va)(current_value) in
                if howl=Lower
				then minimize_it_with_care0([a],va,peurrest)
				else if howl=Equal
				     then minimize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else minimize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 minimize_it_with_care0([x],f(x),y);;


let maximize_it_with_care (cf:'a t) 
   f=function
[]->failwith("careful max on empty set undefined")
|x::y->
 let rec maximize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                let howl=cf(va)(current_value) in
                if howl=Greater
				then maximize_it_with_care0([a],va,peurrest)
				else if howl=Equal
				     then maximize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else maximize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 maximize_it_with_care0([x],f(x),y);;

let modify_locally (f:'a t) l=
  let big_m=max(f)(l) in
  let tempf=(fun x y->
    if List.mem(x)(l)
    then if List.mem(y)(l)
         then if x=y
              then Equal
              else (from_list l x y)
         else f big_m y
    else if List.mem(y)(l)
         then f x big_m
         else f x y
  
  ) in
  (tempf:>( 'a t));;

let list_for_dictionary_order=
  [97; 65; 98; 66; 99; 67; 100; 68; 101; 69; 102; 70; 103; 71; 104; 72; 105;
  73; 106; 74; 107; 75; 108; 76; 109; 77; 110; 78; 111; 79; 112; 80; 113; 81;
  114; 82; 115; 83; 116; 84; 117; 85; 118; 86; 119; 87; 120; 88; 121; 89;
  122; 90; 91; 92; 93; 94; 95; 96];;  

let reindexer_for_dictionary_order i=
    if (i<65)||(i>122) 
    then i 
    else 64+(Assistance_listennou.find_index i list_for_dictionary_order);;


let for_characters=let tempf=(fun x y->
  standard 
        (reindexer_for_dictionary_order(int_of_char x))
        (reindexer_for_dictionary_order(int_of_char y))
  ) in (tempf:>char t);;

let for_integers=let tempf=(fun (x:int) (y:int)-> standard x y 
    ) in (tempf:>int t);;  

let lex_for_strings=
    ((fun s1 s2->
      let m1=String.length s1
      and m2=String.length s2
      in
      let m=Stdlib.min(m1)(m2) in
      match Assistance_option.seek (fun j->(String.get s1 j)<>(String.get s2 j)) (Assistance_ennig.ennig 0 (m-1)) with
      None->standard m1 m2
      |Some(j)->for_characters (String.get s1 j) (String.get s2 j) 
    ) : string t);;

let silex_for_strings=
      ((fun s1 s2->
        let m1=String.length s1
        and m2=String.length s2
        in
        let first_try=standard(m1)(m2) in
        if first_try<>Equal
        then first_try
        else lex_for_strings s1 s2
      ) : string t);;    

let lex_for_string_lists=
  ((fun l1 l2->
      let (_,left_part,right_part)=Assistance_listennou.factor (l1,l2) in
      if left_part=[] 
      then (if right_part=[] 
           then Equal 
           else Lower)
      else if right_part=[] 
           then Greater 
           else lex_for_strings (List.hd left_part) (List.hd right_part)  
  ) : (string list) t);;

let for_longest_match=  
    ((fun s1 s2->
      let m1=String.length s1
      and m2=String.length s2 in
      if (
          if m1>m2 then false else
          (String.sub s2 0 m1)=s1
      ) then Greater else
      if (
          if m2>m1 then false else
          (String.sub s1 0 m2)=s2
      ) then Lower else
      lex_for_strings s1 s2
     ): string t);;


let for_longest_match_pairs=  
((fun (s1,v1) (s2,v2)->
  let first_try=silex_for_strings(s2)(s1) in
  if first_try<>Equal 
  then first_try
  else standard v1 v2
 ): (string*'b) t);;
 
let from_snd (f:'b t)=((fun (x1,y1) (x2,y2)->
  let first_try=f y1 y2 in
  if first_try<>Equal 
  then first_try
  else standard x1 x2
): ('a*'b) t );;

 
 
           

end;;






module Assistance_ordered=struct

(*
 
#use"ordered.ml";;

*)

module Private = struct 

let intersect (cmpr:'a Assistance_total_ordering.t) ox oy=
    let rec tempf=(function (u,v,accu)->
      if u=[] then (List.rev(accu)) else
      if v=[] then (List.rev(accu)) else
      let xu=List.hd(u) and yu=List.tl(u) 
      and xv=List.hd(v) and yv=List.tl(v) in
      match cmpr(xu)(xv) with
       Assistance_total_ordering.Lower->tempf(yu,v,accu)
      |Assistance_total_ordering.Equal->tempf(yu,yv,xu::accu)
      |Assistance_total_ordering.Greater->tempf(u,yv,accu)
    ) in
    tempf(ox,oy,[]);;

let is_increasing (cmpr:'a Assistance_total_ordering.t) l=
  if List.length(l)<2 then true else
  let rec tempf=(function
  (a,to_be_treated)->match to_be_treated with
   []->true
   |b::others->if (cmpr(a)(b)=Assistance_total_ordering.Lower)
                 then tempf(b,others)
                 else false
  ) in
  tempf(List.hd l,List.tl l);;
  

let merge (cmpr:'a Assistance_total_ordering.t) ox oy=
    let rec tempf=(function (u,v,accu)->
      if u=[] then (List.rev_append(accu)(v)) else
      if v=[] then (List.rev_append(accu)(u)) else
      let xu=List.hd(u) and yu=List.tl(u) 
      and xv=List.hd(v) and yv=List.tl(v) in
    match cmpr(xu)(xv) with
      Assistance_total_ordering.Lower->tempf(yu,v,xu::accu)
    |Assistance_total_ordering.Equal->tempf(yu,yv,xu::accu)
    |Assistance_total_ordering.Greater->tempf(u,yv,xv::accu)
    ) in
    tempf(ox,oy,[]);;


let setminus (cmpr:'a Assistance_total_ordering.t) ox oy=
    let rec tempf=
    (function (u,v,accu)->
      if u=[] then (List.rev(accu)) else
      if v=[] then (List.rev_append(accu)(u)) else
      let xu=List.hd(u) and yu=List.tl(u) 
      and xv=List.hd(v) and yv=List.tl(v) in
      match cmpr(xu)(xv) with
         Assistance_total_ordering.Lower->tempf(yu,v,xu::accu)
        |Assistance_total_ordering.Equal->tempf(yu,yv,accu)
        |Assistance_total_ordering.Greater->tempf(u,yv,accu)
   ) in
   tempf(ox,oy,[]);;

let rec sort (cmpr:'a Assistance_total_ordering.t) x=
  if List.length(x)<2
  then x
  else let temp1=Assistance_listennou.split_list_in_half(x) in
       let y1=sort(cmpr)(fst temp1)
       and y2=sort(cmpr)(snd temp1) in
       merge cmpr y1 y2;;


end;;


let diff (cmpr: 'a Assistance_total_ordering.t) =
          let rec tempf=(fun
            (treated_bc,treated_b,treated_c,to_be_treated1,to_be_treated2)->
              match to_be_treated1 with
              []->(treated_bc,treated_b,List.rev_append treated_c to_be_treated2)
              |(a1,b1)::others1->
              (
                match to_be_treated2 with
              []->(treated_bc,List.rev_append treated_b to_be_treated1,treated_c)     
              |(a2,c2)::others2->
                (
                  match cmpr a1 a2 with
                  Assistance_total_ordering.Lower->
                    tempf(treated_bc,(a1,b1)::treated_b,treated_c,others1,to_be_treated2)
                  |Assistance_total_ordering.Greater->
                  tempf(treated_bc,treated_b,(a2,c2)::treated_c,to_be_treated1,others2)
                  |Assistance_total_ordering.Equal->
                  tempf((a1,b1,c2)::treated_bc,treated_b,treated_c,others1,others2)  
                )
              )      
          ) in
          tempf;;   

let does_not_intersect (cmpr:'a Assistance_total_ordering.t) ox oy=
    let rec tempf=(function (u,v)->
        if (u=[])||(v=[]) then true else
        let xu=List.hd(u) and yu=List.tl(u) 
        and xv=List.hd(v) and yv=List.tl(v) in
        match cmpr(xu)(xv) with
          Assistance_total_ordering.Lower->tempf(yu,v)
        |Assistance_total_ordering.Equal->false
        |Assistance_total_ordering.Greater->tempf(u,yv)
    ) in
    tempf(ox,oy);;

exception Empty_intersection_undefined;;    

let fold_intersect cmpr=function
   []->raise(Empty_intersection_undefined)
  |a::b->List.fold_left(Private.intersect cmpr)(a)(b);;

let fold_merge cmpr l=
   let rec tempf=(function
      (already_treated,to_be_treated)->match to_be_treated with 
      []->already_treated
      |a::b->tempf(Private.merge cmpr a already_treated,b)
   ) in 
   tempf([],l);;    

let insert cmpr x oy=Private.merge cmpr [x] oy;; 

let intersect = Private.intersect;;

let intersects cmpr ox oy = not(does_not_intersect cmpr ox oy);;

let is_included_in (cmpr:'a Assistance_total_ordering.t) ox oy=
    let rec tempf=(function (u,v)->
      if u=[] then true else
      if v=[] then false else
      let xu=List.hd(u) and yu=List.tl(u) 
      and xv=List.hd(v) and yv=List.tl(v) in
      match cmpr(xu)(xv) with
        Assistance_total_ordering.Lower->false
      |Assistance_total_ordering.Equal->tempf(yu,yv)
      |Assistance_total_ordering.Greater->tempf(u,yv)
    ) in
    tempf(ox,oy);;


let rec mem (cmpr:'a Assistance_total_ordering.t) x ol=
   let rec tempf=(function
    []->false
    |a::others->match cmpr(x)(a) with
       Assistance_total_ordering.Lower->false
       |Assistance_total_ordering.Equal->true
       |Assistance_total_ordering.Greater->tempf others
   )  in
   tempf ol;;    

let merge = Private.merge;;

let outsert cmpr x oy=Private.setminus cmpr oy [x];;

let safe_set cmpr ox=if Private.is_increasing(cmpr)(ox) 
                     then ox 
                     else Private.sort cmpr ox;;

let setminus = Private.setminus;;

let sort = Private.sort;;




end;;






module Assistance_strung=struct

(*

#use"strung.ml";;

*)


let get s i=String.get s (i-1);;
 
let set s i c=Bytes.set s (i-1) c;;

let enclose s=
  let encloser="\"" in
  encloser^(String.escaped s)^encloser;;


let implode l=
   let n=List.length(l) in
   let by=Bytes.make n ' ' in
   let _=(for i=0 to (n-1) do Bytes.set by i (List.nth l i) done;) in
   Bytes.to_string by;;
  
    
let explode s=
    let n=String.length s in
    Assistance_ennig.doyle (String.get s) 0 (n-1);;
    
 
let char_finder_from f s w0=
   let n=(String.length s) in
   let rec tempf=(fun j->
     if j>=n then 0 else
     if f(String.get s  j) then j+1 else
     tempf(j+1)
   ) in
   tempf(w0-1);;

let backwards_char_finder f s =
    let rec tempf=(fun j->
      if j<0 then 0 else
      if f(String.get s  j) then j+1 else
      tempf(j-1)
    ) in
    tempf((String.length s)-1);;   
 
let show_indices s=
  let n=String.length s in
  Assistance_ennig.doyle (fun i->(i,String.get s (i-1)) ) 1 n;;   
   
let number_of_lines_before = Assistance_substring.Friend.number_of_lines_before;;

exception Negative_offset_for_string;; 

let insert_repetitive_offset_on_the_left c l_max str=
   let d=l_max-(String.length str) in
   if d<0
   then raise(Negative_offset_for_string)
   else
   (String.make d c)^str;;


     
let reverse s=
   implode(List.rev(explode s));; 

let find_one_of_several_in_at_idx candidates s idx =
   let tester=(
      fun candidate ->
       let j=idx-1 and l_cand=String.length(candidate) in 
       if (String.length(s)<j+l_cand)||(j<0)
      then None
      else if (String.sub s j l_cand)=candidate 
           then Some(idx,candidate)
           else None
   ) in 
   Assistance_option.find_and_stop tester candidates;;
  
(*

find_one_of_several_in_at_idx ["ba";"ab"] "123ab67" 4;;

*)

let find_one_of_several_in_from_idx candidates s idx =
  let n=String.length s in 
  Assistance_option.find_and_stop (
    find_one_of_several_in_at_idx candidates s
  ) (Assistance_ennig.ennig idx n);;

(*

find_one_of_several_in_from_idx ["ba";"ab"] "123ab67" 1;;

*)

let remove_newlines s=
   let temp1=List.filter (fun c->c<>'\n') (explode s) in 
   implode temp1;;

exception Not_found_during_succession;;

let find_successively_in_from patterns_in_order s start_idx=
  let rec tempf=(fun 
     (treated,to_be_treated,idx,line_idx)->
       match to_be_treated with 
       []->List.rev treated
       |patt::other_patts->
         match  find_one_of_several_in_from_idx patt s idx with 
         None->raise(Not_found_during_succession)
         |Some(idx2,candidate)->
          let temp1=List.filter(fun k->(get s k)='\n')(Assistance_ennig.ennig idx (idx2-1)) in 
          let line_idx_for_idx2=line_idx+List.length(temp1) in 
          let msg="Found "^(remove_newlines candidate)^" at line number "^(string_of_int line_idx_for_idx2)^"\n" in 
          let _=(print_string msg;flush stdout) in 
          let idx3=idx2+(String.length candidate) in  
          let temp2=List.filter(fun k->(get s k)='\n')(Assistance_ennig.ennig idx2 (idx3-1)) in 
          let line_idx_for_idx3=line_idx_for_idx2+List.length(temp2) in 
          tempf((idx2,idx3-1)::treated,other_patts,idx3,line_idx_for_idx3)    
  ) in 
  let temp3=List.filter(fun k->(get s k)='\n')(Assistance_ennig.ennig 1 (start_idx-1)) in 
  let start_line_idx = 1+(List.length(temp3)) in 
  tempf([],patterns_in_order,start_idx,start_line_idx);;

(*

find_successively_in [["ba";"ab"];["cde";"edc"]] "12\n\n\n\n\n8ab123\n\n67cde12";;

*)



let replace_ranges_in l s=
    if l=[] then s else
    let n=String.length s in
    let ranges=Assistance_image.image fst l in
    let partition=Assistance_listennou.partition_from_set_of_ranges ranges n in 
    let temp1=Assistance_image.image (
      fun (i,j,will_be_replaced)->
        if will_be_replaced 
        then List.assoc (i,j) l
        else String.sub s (i-1) (j-i+1)
    ) partition in
    String.concat "" temp1;;

(*

replace_ranges_in [((3,5),"A");((8,12),"B")] "12345678901234567890";;

*)




let insert_prefixes_at_indices l s=
    if l=[] then s else
    let n=String.length s in
    let temp1=Assistance_image.image (fun (pref,idx)->(idx,pref)) l in
    let temp2=Assistance_image.image fst temp1 in
    let temp3=Assistance_ordered.sort Assistance_total_ordering.standard ((n+1)::temp2) in
    let temp4=Assistance_listennou.universal_delta_list temp3 in
    let temp5=Assistance_image.image(fun (i,j)->
       (List.assoc i temp1)^(String.sub s (i-1) (j-i)) ) temp4 in
    let i1=List.hd temp3 in
    let temp6=(
       if i1=1 then temp5 else (String.sub s 0 (i1-1))::temp5
    )  in 
    String.concat "" temp6;;

(*

insert_prefixes_at_indices ["hap",4;"na",12] "123py678901tion6";;

*)

exception Largest_common_prefix_exn;;

let largest_common_prefix l=
   if l=[] then raise(Largest_common_prefix_exn) else
   let lengths=Assistance_image.image String.length l in
   let m=Assistance_min.list lengths in
   let tester=(fun k->
     let temp1=Assistance_image.image (fun s->String.get s k) l in
     let v=List.hd temp1 in
     List.for_all (fun x->x=v) temp1
   ) in
   let rec tempf=(fun j->
     if j=m then j else 
     if tester(j) then tempf(j+1) else j
   ) in
   let j0=tempf 0 in
   String.sub (List.hd l) 0 j0;;

(*

largest_common_prefix ["abby";"abnormal"];;
largest_common_prefix ["";"call"];;
largest_common_prefix ["sad";"again"];;


*)

let leftmost_difference s1 s2=
   let n1=String.length s1 
   and n2=String.length s2 in
   let n=min(n1)(n2) in 
   match Assistance_option.seek(fun j->
      (get s1 j)<>(get s2 j)
   )(Assistance_ennig.ennig 1 n) with 
   None->None 
   |Some(j0)->
      let common_part=String.sub s1 0 (j0-1) 
      and s1_part=String.sub s1 j0 (n1-j0)
      and s2_part=String.sub s2 j0 (n2-j0) in 
      Some(common_part,get s1 j0,get s2 j0,s1_part,s2_part);;

(*
leftmost_difference "abc1def" "abc257";;
*)




exception Unfinished_expression of int*string;;
exception Unexpected_case_in_triune_analysis;;


module Private = struct

let pusher_inside_nested_parentheses_parsing
    (s,joiners,seeker) state =
     let (opt_result,nbr_of_openers_so_far,items_so_far,current_item_start,world_start,idx)=state in 
     if opt_result<>None then state else 
     let opt = seeker idx in 
     if opt = None 
     then raise(Unfinished_expression(idx,s))
     else 
     let (case,new_idx)=Assistance_option.unpack opt in
     let joiner=List.nth joiners (case-1) in 
     let idx2=new_idx+String.length(joiner) in 
     if case=1
     then (None,nbr_of_openers_so_far+1,items_so_far,current_item_start,world_start,idx2)
     else    
     if case=2
     then (
            if nbr_of_openers_so_far=1
            then let new_item = Assistance_cull_string.interval s current_item_start (new_idx-1) in 
                 (None,nbr_of_openers_so_far,new_item::items_so_far,idx2,world_start,idx2)
            else (None,nbr_of_openers_so_far,items_so_far,current_item_start,world_start,idx2)
          )
     else 
     if case=3
     then (
            if nbr_of_openers_so_far=1
            then let whole_interval=(world_start,idx2-1) in 
                 let last_item = Assistance_cull_string.interval s current_item_start (new_idx-1) in 
                 let items=List.rev(last_item::items_so_far) in
                 let answer=Some(items,whole_interval) in  
                 (answer,0,[],0,0,0)
            else (None,nbr_of_openers_so_far-1,items_so_far,current_item_start,world_start,idx2)
          )
     else raise(Unexpected_case_in_triune_analysis);;
     
let iterator_inside_nested_parentheses_parsing 
    (s,joiners,seeker) =
     let rec tempf=(fun state ->
     let (opt_result,nbr_of_openers_so_far,items_so_far,current_item_start,world_start,idx)=state in 
     match opt_result with 
     Some(result)->result 
     |None -> tempf(pusher_inside_nested_parentheses_parsing (s,joiners,seeker) state )) in 
   tempf;;

end ;;


exception Missing_opener of string*string;;
exception Started_by_nonopener of int*string;;


let parse_nested_parentheses 
  (openr,separatr,closr) s=
    let joiners = [openr;separatr;closr]  in  
    let seeker = Assistance_substring.leftmost_index_of_pattern_among_in_from 
       [openr;separatr;closr] s in 
    
    let opt1=seeker 1 in 
    if opt1=None 
    then raise(Missing_opener(openr,s))
    else  
    let (case1,idx1)=Assistance_option.unpack opt1 in
    if case1<>1
    then raise(Started_by_nonopener(case1,s))
    else 
    let idx2=idx1+(String.length openr) in 
    let initial_vals=(None,1,[],idx2,idx1,idx2) in 
    Private.iterator_inside_nested_parentheses_parsing 
    (s,joiners,seeker) initial_vals;;
    
(*

parse_nested_parentheses ("(",",",")") "f(ab,cde,gh)ijk" ;;

parse_nested_parentheses ("(",",",")") "g(1,f(ab,cde,gh)ijk,2,h(k(u(6,7),v)),3)" ;;

*)  


   
let to_intlist enclosed_s =
   let n=String.length enclosed_s in 
   let s=Assistance_cull_string.interval enclosed_s 2 (n-1) in 
   let temp1=Assistance_cull_string.extract_intervals_in_wrt_separator s ";" in 
   Assistance_image.image int_of_string temp1;;

let of_intlist l=
  let temp1=Assistance_image.image string_of_int l in 
  "["^(String.concat ";" temp1)^"]";;

(*


let z1=[2;7;3;51];;
let z2=of_intlist z1;;
let check =(to_intlist(z2)=z1);;

*)  

let soak (replacee,replacer) s=
   if Assistance_substring.is_the_beginning_of replacee s 
   then Some(replacer^(Assistance_cull_string.two_sided_cutting (replacee,"") s))
   else None ;;

(*

soak ("abc/def","DEF/GHI") "abc/def/klm/pqr" ;;
soak ("abc/def","DEF/GHI") "azc/def/klm/pqr" ;;
soak ("abc/def","DEF/GHI") "azc/def" ;;

*)


let escaped_and_quoted text =
   match Str.split (Str.regexp_string "\n") text with 
   [] -> "\"\""
   |first_line :: other_lines ->
    let quoted_lines = (enclose first_line)::
     (Assistance_image.image (fun line->enclose("\n"^line)) other_lines) in 
    String.concat "^\n" quoted_lines;;


(*

let z1 = "abc\nde\nfghi\njkl";;
print_string(escaped_and_quoted z1);;



*)

let reposition_whole_according_to_separator separator lines =
      let temp1 = Assistance_image.image (fun line->(line,Assistance_substring.leftmost_index_of_in separator line)) lines in 
      let max_idx = snd(Assistance_max.maximize_it snd temp1) in 
      Assistance_image.image (fun (line,idx)->
          let offset = max_idx-idx in 
          (String.make offset ' ')^line
      ) temp1;;

let reposition_left_hand_side_according_to_separator separator lines =
         let temp1 = Assistance_image.image (fun line->
              let j = Assistance_substring.leftmost_index_of_in separator line in 
              ((Assistance_cull_string.beginning (j-1) line,Assistance_cull_string.cobeginning (j-1) line),j)) lines in 
         let max_idx = snd(Assistance_max.maximize_it snd temp1) in 
         Assistance_image.image (fun ((left,right),idx)->
             let offset = max_idx-idx in 
             left^(String.make offset ' ')^right
         ) temp1;;      



end;;






module Assistance_after=struct

(*

#use"after.ml";;

*)

let after_star l s =
  let n=String.length s in
  let rec tempf=(
    fun j->
      if j>n then None else
      if  List.mem (String.get s (j-1)) l
      then tempf(j+1)
      else Some(j)
  ) in
  tempf;;


let after_whites s =after_star Assistance_charset.list_of_whites s;;

  let after_whites_and_comments s=
    let n=String.length s in
    let rec tempf=(
      fun j->
        if j>n then None else
        if List.mem (String.get s (j-1)) Assistance_charset.list_of_whites
        then tempf(j+1)
        else 
        if Assistance_substring.is_a_substring_located_at "/*" s j
        then let k=Assistance_substring.leftmost_index_of_in_from "*/" s (j+2) in
             if k<0
             then None
             else tempf(k+2)
        else Some(j)
    ) in
    tempf;;
  
  (*    
  after_whites_and_comments "\n/* 567 */\t\r\n\n/* 89 ** // 78*/123";;    
  *)
  
exception Unfinished_simple_quoted_string of int;;  

let after_simple_quoted_string s k0=
    let n=String.length s in
    if (Assistance_strung.get s k0)<>'\''
    then k0
    else 
    let rec tempf=(fun k->
       if k>n
       then raise(Unfinished_simple_quoted_string(k0))
       else 
       let c=String.get s (k-1) in
       if c='\\'
       then tempf(k+2)
       else 
       if c='\''
       then k+1
       else tempf(k+1)
    ) in
    tempf (k0+1);;

(*

after_simple_quoted_string "'abc'67" 1;; 

*)    

exception Unfinished_double_quoted_string of int;;  
    
let after_double_quoted_string s k0=
        let n=String.length s in
        if (Assistance_strung.get s k0)<>'"'
        then k0
        else 
        let rec tempf=(fun k->
           if k>n
           then raise(Unfinished_double_quoted_string(k0))
           else 
           let c=String.get s (k-1) in
           if c='\\'
           then tempf(k+2)
           else 
           if c='"'
           then k+1
           else tempf(k+1)
        ) in
        tempf (k0+1);;     



exception Unbalanced_expression of char*char;;

let after_closing_character (lchar,rchar) s=
  let n=String.length s in
  let rec tempf=(
    fun (k,count)->
      if k>n
      then raise(Unbalanced_expression(lchar,rchar))
      else 
      if Assistance_substring.is_a_substring_located_at "/*" s k
      then let j=Assistance_substring.leftmost_index_of_in_from "*/" s (k+2) in
           tempf(j+2,count)
      else 
      if Assistance_substring.is_a_substring_located_at "//" s k
      then let j=Assistance_substring.leftmost_index_of_in_from "\n" s (k+2) in
           tempf(j+1,count)
      else 
      if (Assistance_substring.is_a_substring_located_at "<<<EOF\n" s k)
         ||
         (Assistance_substring.is_a_substring_located_at "<<<'EOF'\n" s k) 
      then let j=Assistance_substring.leftmost_index_of_in_from "\nEOF;\n" s (k+7) in
           tempf(j+6,count)
      else 
      let c=String.get s (k-1) in
      if c=lchar
      then tempf(k+1,count+1)
      else 
      if c='\''
      then let j=after_simple_quoted_string s k in
           tempf(j,count)
      else
      if c='"'
      then let j=after_double_quoted_string s k in
           tempf(j,count)
      else     
      if c<>rchar
      then tempf(k+1,count)
      else 
        if count=1
        then k+1
        else tempf(k+1,count-1)
  ) in
  tempf;;

(*

after_closing_character ('{','}') "{ 345 }89" (1,0);;
after_closing_character ('{','}') "{2{4}6{8{0}2}4}67" (1,0);;
after_closing_character ('{','}') "{\"3}5\"}89" (1,0);;
after_closing_character ('{','}') "{'3}5'}89" (1,0);;
after_closing_character ('{','}') "{/*4}6*/}01" (1,0);;
after_closing_character ('{','}') "{<<<EOF\n}\nEOF;\n}78" (1,0);;
after_closing_character ('{','}') "{<<<'EOF'\n}\nEOF;\n}90" (1,0);;

*)

let next_in_list l s=
  let n=String.length s in
  let rec tempf=(
    fun k->
      if k>n
      then None
      else 
      if Assistance_substring.is_a_substring_located_at "/*" s k
      then let j=Assistance_substring.leftmost_index_of_in_from "*/" s (k+2) in
           tempf(j+2)
      else 
      if Assistance_substring.is_a_substring_located_at "//" s k
      then let j=Assistance_substring.leftmost_index_of_in_from "\n" s (k+2) in
           tempf(j+1)
      else 
      if (Assistance_substring.is_a_substring_located_at "<<<EOF\n" s k)
         ||
         (Assistance_substring.is_a_substring_located_at "<<<'EOF'\n" s k) 
      then let j=Assistance_substring.leftmost_index_of_in_from "\nEOF;\n" s (k+7) in
           tempf(j+6)
      else 
      let c=String.get s (k-1) in
      if c='\''
      then let j=after_simple_quoted_string s k in
           tempf(j)
      else
      if c='"'
      then let j=after_double_quoted_string s k in
           tempf(j)
      else     
      if List.mem c l
      then Some(k)
      else tempf(k+1)
  ) in
  tempf;;


let after_classlike_declaration s i=
    Assistance_option.seek(
     fun j->not(List.mem 
         (String.get s (j-1)) Assistance_charset.classlike_declaration_chars
     )
    )(Assistance_ennig.ennig i (String.length s));;


let after_abstract_class s i0=
  if not(Assistance_substring.is_a_substring_located_at "abstract" s i0)
  then None
  else
  let opt1=after_whites s (i0+8) in
  if opt1=None then None else
  let i1=Assistance_option.unpack opt1 in
  if not(Assistance_substring.is_a_substring_located_at "class" s i1)
  then None
  else 
  let opt2=after_classlike_declaration s (i1+5) in
  if opt2=None then None else
  let i2=Assistance_option.unpack opt2 in
  if (Assistance_strung.get s i2)<>'{' then None else 
  Some(after_closing_character ('{','}') s (i2+1,1));;

(*

after_abstract_class "abstract  class {u\nv}234" 1;;

*)

let after_final_class s i0=
  if not(Assistance_substring.is_a_substring_located_at "final" s i0)
  then None
  else
  let opt1=after_whites s (i0+5) in
  if opt1=None then None else
  let i1=Assistance_option.unpack opt1 in
  if not(Assistance_substring.is_a_substring_located_at "class" s i1)
  then None
  else 
  let opt2=after_classlike_declaration s (i1+5) in
  if opt2=None then None else
  let i2=Assistance_option.unpack opt2 in
  if (Assistance_strung.get s i2)<>'{' then None else 
  Some(after_closing_character ('{','}') s (i2+1,1));;     

(*

after_final_class "final  class {u\nv}901" 1;;

*)

let after_usual_class s i0=
  if not(Assistance_substring.is_a_substring_located_at "class" s i0)
  then None
  else 
  let opt2=after_classlike_declaration s (i0+5) in
  if opt2=None then None else
  let i2=Assistance_option.unpack opt2 in
  if (Assistance_strung.get s i2)<>'{' then None else 
  Some(after_closing_character ('{','}') s (i2+1,1));;     

(*

after_usual_class "class {u\nv}234" 1;;
after_usual_class "class_loader { }" 1;;

*)

let after_interface s i0=
  if not(Assistance_substring.is_a_substring_located_at "interface" s i0)
  then None
  else 
  let opt2=after_classlike_declaration s (i0+5) in
  if opt2=None then None else
  let i2=Assistance_option.unpack opt2 in
  if (Assistance_strung.get s i2)<>'{' then None else 
  Some(after_closing_character ('{','}') s (i2+1,1));;  

(*

after_interface "interface {u\nv}678" 1;;

*)

let after_classlike_block s i=
   Assistance_option.find_and_stop(
     fun f->f s i
   )[
       after_abstract_class;
       after_final_class;
       after_usual_class;
       after_interface;
    ];;


(*

after_classlike_block "abstract  class {u\nv}234" 1;;
after_classlike_block "final  class {u\nv}901" 1;;
after_classlike_block "class {u\nv}234" 1;;
after_classlike_block "interface {u\nv}678" 1;;

*)    

let after_classlike_block_with_linebreak s i=
  let n=String.length s in
  let opt1=after_classlike_block s i in
  if opt1=None then None else
  let i1=Assistance_option.unpack opt1 in
  let opt2=Assistance_option.seek(fun j->
     not(List.mem (Assistance_strung.get s j) [' ';'\r';'\t']) )
  (Assistance_ennig.ennig i1 n) in
  if opt2=None then None else
  let i2=Assistance_option.unpack opt2 in
  if Assistance_strung.get s i2='\n'
  then Some(i2+1)
  else None;;
    
(*

after_classlike_block_with_linebreak "abstract  class {u\nv}  \t \n7" 1;;
after_classlike_block_with_linebreak "final  class {u\nv} \t\t\n3" 1;;
after_classlike_block_with_linebreak "class {u\nv}\n3" 1;;
after_classlike_block_with_linebreak "interface {u\nv} \t\n9" 1;;

*)    

exception End_of_div_not_found;;

let rec main_helper_for_div (s,n,div_count,idx)=
    if idx>n
    then raise(End_of_div_not_found)
    else
    if Assistance_substring.is_a_substring_located_at "</div>" s idx
    then if div_count=1
         then idx+6
         else main_helper_for_div(s,n,div_count-1,idx+6)
    else 
    if not(Assistance_substring.is_a_substring_located_at "<div " s idx)
    then main_helper_for_div(s,n,div_count,idx+1)
    else  
    let jdx=Assistance_substring.leftmost_index_of_in_from ">" s (idx+5) in
    main_helper_for_div(s,n,div_count+1,jdx);;

let after_div s idx=main_helper_for_div(s,String.length s,0,idx);;

(*

after_div "<div val=\"abc\"> xyz </div>789" 1;;

*)

let after_one pattern s idx=
  if Assistance_substring.is_a_substring_located_at pattern s idx
  then Some(idx+String.length pattern)
  else None;;

let after_one_among_several l_patterns s idx=
   Assistance_option.find_and_stop (
     fun pattern->after_one pattern s idx
   ) l_patterns;;

let  after_php_label s idx=
   if not(List.mem (Assistance_strung.get s idx) Assistance_charset.php_label_first_letters)
   then None
   else
   after_star 
     Assistance_charset.php_label_nonfirst_letters s (idx+1);;
     



           

end;;






module Assistance_alternative_global_replace=struct

(*


#use"alternative_global_replace.ml";;

The my_global_replace is a replacement for Ocaml's Str.global_replace which has
the disadvantage of applying certain transforms to the replacement string.


*)




let single_char_special_case (single_c,b) s=
    let n=String.length(s) in
    let temp1=Assistance_ennig.doyle (
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
  let indices=Assistance_substring.occurrences_of_in a s in
  if indices=[] then s else
  let delta_indices = Assistance_listennou.universal_delta_list indices in 
  let opt_ambiguity=Assistance_option.seek (fun (start1,start2)->start2<start1+na) delta_indices in 
  if  opt_ambiguity<>None
  then let (start1,start2)=Assistance_option.unpack opt_ambiguity in 
       raise(Ambiguity(a,start1,start2))
  else  
  let m=List.length indices in 
  let lower_end=(fun j->if j=0 then 1 else List.nth indices (j-1)+na) 
  and upper_end=(fun j->if j=m then n else (List.nth indices (j))-1) in 
  let unchanged_intervals = Assistance_ennig.doyle (fun j->(lower_end j,upper_end j)) 0 m in 
  let unchanged_substrings=Assistance_image.image 
     (fun (x,y)->if x>y then "" else Assistance_cull_string.interval s x y) unchanged_intervals in
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
  
           

end;;






module Assistance_alternative_str=struct

(*

An adptation of OCaml's Str module :

String indices are now from 1 to n instead of 0 to (n-1).
Operations on regexps are encoded as functions in the module.

#use"alternative_str.ml";;

*)

type backtrack_length=int;;

type regexp=M of string*(Str.regexp)*backtrack_length;;

let unveil (M(s,_,b))=s;;
let veil s=M(s,Str.regexp s,0);;

let set_backtrack (b:backtrack_length) (M(s,rgxp,_))=M(s,rgxp,b);;

let regexp_string s=let quote=Str.quote s in veil quote;;

let plus (M (s,_,_))=let new_s="\\("^s^"\\)+" in veil new_s;;
let star (M (s,_,_))=let new_s="\\("^s^"\\)*" in veil new_s;;

let concat (M(s1,_,_)) (M(s2,_,_))=veil (s1^s2);;
   
let big_concat l=
  if l=[]
  then veil ""
  else let temp1=List.map (fun w->"\\("^(unveil w)^"\\)") l in
       let new_s=String.concat "" temp1 in
       veil new_s;;   

let big_or l=
  let temp1=List.map (fun (M(s,_,_))->"\\("^s^"\\)") l in
  let new_s=String.concat "\\|" temp1 in
  veil new_s;;

let ore a b=big_or [a;b];;

let string_match (M(_,rgxp,b)) s i0=
   let bowl=(Str.string_match rgxp s (i0-1)) in
   if bowl
   then Some(i0+(String.length(Str.matched_string s))-b-1)
   else None;;
   
(* Functions from the option or ennig s *)  

let index_everything l=
  let rec tempf=
   (function (j,graet,da_ober)->
     match da_ober with
      []->graet
     |a::b->tempf(j-1,(j,a)::graet,b)
    )    in
    tempf(List.length(l),[],List.rev(l));;

let unpack=function
None->failwith("Void is not unpackable")
|Some(x)->x;;   
   
let rec find_and_stop f l=
 let rec find_and_stop0=(function
  da_ober->match da_ober with
   []->None
   |a::peurrest->match f(a) with
		None->find_and_stop0(peurrest)
		|Some(x)->Some(x)
 ) in
 find_and_stop0(l);;
   
(* End of functions from the option or ennig modules *)     
      
   
type left_regexp=regexp;;
type center_regexp=regexp;;   
type right_regexp=regexp;;   
   
type centered_regexp=left_regexp*center_regexp*right_regexp;;   
 
let create_centered_regexp a b c=((a,b,c):centered_regexp);;   
   
let centered_regexp_match ((a,b,c):centered_regexp) s i0=
  let opt1=string_match a s i0 in
  if opt1=None then None else
  let i1=unpack(opt1)+1 in
  let opt2=string_match b s i1 in
  if opt2=None then None else   
  let i2=unpack(opt2)+1 in 
  let opt3=string_match c s i2 in
  if opt3=None then None else   
  Some(i1,i2-1);;
   
 
let centered_regexp_list_match l s i0=
  let temp1=index_everything l in
  find_and_stop(fun (pattern_idx,rgxp)->
    match centered_regexp_match rgxp s i0 with
     Some(i_start,i_end)->Some(pattern_idx,(i_start,i_end))
    |None->None
  ) temp1;;   
   
 let find_all_occurrences l s i0=
  let n=String.length s in
  let rec tempf=(fun (graet,j)->
    if j>n then List.rev(graet) else
    let opt=centered_regexp_list_match l s j in
    if opt=None then tempf(graet,j+1) else
    let (pattern_idx,(i_start,i_end))=unpack(opt) in
    tempf((pattern_idx,(i_start,i_end))::graet,i_end+1)
  ) in
  tempf([],i0);;

   
   
 (*  
 
 centered_regexp_match (veil "12.",veil"4.6",veil"78") "123456789abcdef" 1;;
 
 let s1="123456789abcdef";;
 let a1=veil "12." and b1=veil"4.6" and c1=veil"78";;
 
 let w1=string_match a1 s1 1;;
 
 
 
 string_match (regexp_string "456") "123456789abcdef" 4;; 

let test_centered_regexp  
 
let search_forward (M(_,rgxp,b)) s i0=
   let j1=(Str.search_forward rgxp s (i0-1))+1 in
   let j2=j1+(String.length(Str.matched_string s))-b-1 in
   (j1,j2);;
  *)            

end;;






module Assistance_alternative_str_example=struct

(*

Concrete values of type My_str.regexp.

#use"alternative_str_example.ml";;

*)

let capital_letter=Assistance_alternative_str.veil "[A-Z]";;
let letters=Assistance_alternative_str.veil "[A-Za-z1-9_']*";;
let nonletter=Assistance_alternative_str.veil "[^A-Za-z1-9_']";;
let white=Assistance_alternative_str.veil "[ \n\r\t]";;
let maybe_whites=Assistance_alternative_str.star white;;
let some_whites=Assistance_alternative_str.plus white;;
let backtracking_nonletter=Assistance_alternative_str.set_backtrack 1 nonletter;;  

let delimited_module_name=Assistance_alternative_str.big_concat
  [
    nonletter;capital_letter;letters;nonletter
  ];;

let bare_module_name=Assistance_alternative_str.big_concat
  [
    capital_letter;letters
  ];;

let include_case=
  let left_part=Assistance_alternative_str.veil "[ \n\r\t]+include[ \n\r\t(]+"
  and center_part=bare_module_name 
  and right_part=backtracking_nonletter in
  Assistance_alternative_str.create_centered_regexp left_part center_part right_part;; 

let open_case=
  let left_part=Assistance_alternative_str.veil "[ \n\r\t]+open[ \n\r\t(]+"
  and center_part=bare_module_name 
  and right_part=backtracking_nonletter in
  Assistance_alternative_str.create_centered_regexp left_part center_part right_part;; 

let moodle_case=
  let left_part=Assistance_alternative_str.big_concat 
  [white;Assistance_alternative_str.veil"module";some_whites;
   bare_module_name;maybe_whites;Assistance_alternative_str.veil"=";maybe_whites]
  and center_part=bare_module_name 
  and right_part=backtracking_nonletter in
  Assistance_alternative_str.create_centered_regexp left_part center_part right_part;; 

let pointed_case=
  let left_part=nonletter
  and center_part=bare_module_name 
  and right_part=Assistance_alternative_str.regexp_string "." in
  Assistance_alternative_str.create_centered_regexp left_part center_part right_part;; 

let moodle_cases=[include_case;open_case;moodle_case;pointed_case];;
let index_for_include_case=1;;
let index_for_open_case=2;;
let index_for_moodle_case=3;;
let index_for_pointed_case=4;;


(*

let f case s=let (i,j)=Option.unpack(Alternative_str.centered_regexp_match case s 1) in 
(i,j,Cull_string.interval s i j);;

f include_case " include Peggy;; ";;
f include_case " include_once;; ";;
f moodle_case " module Amy = Lawson ";;
f pointed_case " 57+Everybody.talking-78 ";;

*)

 let capital_letter=Assistance_alternative_str.veil "[A-Z]";;
 
 let alphanumeric=
    Assistance_alternative_str.big_or
      [ 
     	Assistance_alternative_str.veil "[a-z]";
     	Assistance_alternative_str.veil "[A-Z]";
     	Assistance_alternative_str.veil "[0-9]";
     	Assistance_alternative_str.regexp_string "_";
      ];;
 
 let alphanumerics=Assistance_alternative_str.plus alphanumeric;;
 
 let beginning_of_module_definition=
    Assistance_alternative_str.set_backtrack 1
    (Assistance_alternative_str.big_concat
      [
         white;
         Assistance_alternative_str.regexp_string "module";
         some_whites;
         capital_letter;
         alphanumerics;
         some_whites;
         Assistance_alternative_str.regexp_string "=";
         some_whites;
         Assistance_alternative_str.regexp_string "struct";
         white;
          
      ]);;
      
 let beginning_of_module_reminder=
    Assistance_alternative_str.set_backtrack 1
    (Assistance_alternative_str.big_concat
      [
         white;
         Assistance_alternative_str.regexp_string "module";
         some_whites;
         capital_letter;
         alphanumerics;
         some_whites;
         Assistance_alternative_str.regexp_string ":";
         some_whites;
         Assistance_alternative_str.regexp_string "sig";
         white;
          
      ]);;
      
 let beginning_of_module_type_definition=
    Assistance_alternative_str.set_backtrack 1
    (Assistance_alternative_str.big_concat
      [
         white;
         Assistance_alternative_str.regexp_string "module";
         some_whites;
         Assistance_alternative_str.regexp_string "type";
         some_whites;
         capital_letter;
         alphanumerics;
         some_whites;
         Assistance_alternative_str.regexp_string "=";
         some_whites;
         Assistance_alternative_str.regexp_string "sig";
         white;
          
      ]);;           
      
      
 let the_end=
    Assistance_alternative_str.set_backtrack 1
    (Assistance_alternative_str.big_concat
      [
         white;
         Assistance_alternative_str.regexp_string "end";
         white;
          
      ]);;       
                 

end;;






module Assistance_associative_list=struct

(*

#use"associative_list.ml";;

*)

exception Push_immediately_after_exn;;
exception Reposition_first_key_not_found;;
exception Reposition_second_key_not_found;;


let change_name_for_key l (key1,key2)=
   Assistance_image.image (fun pair->if fst(pair)=key1 then (key2,snd pair) else pair) l;; 

let change_value_for_key l (key1,vaal1)=
   Assistance_image.image (fun pair->if fst(pair)=key1 then (key1,vaal1) else pair) l;; 

let remove_key l key1=List.filter (fun (key,_)->key<>key1) l;;

let push_immediately_after l pair2 key1 =
  let rec tempf=(
    fun (treated,to_be_treated)->match to_be_treated with 
     []->raise(Push_immediately_after_exn)
    |pair::other_pairs ->
      if fst(pair)=key1
      then List.rev_append treated (pair::pair2::other_pairs)
      else tempf(pair::treated,other_pairs)
  ) in 
  tempf([],l);; 

(* push_immediately_after [(1,"u");(2,"v");(3,"w");(4,"x")] (7,"e") 2;; *)

let decompose_wrt_key l key1=
  let rec tempf=(
     fun (treated,to_be_treated)->match to_be_treated with 
     []->(List.rev treated,None,[])
    |pair::other_pairs ->
       if fst(pair)=key1
      then (List.rev(treated),Some(pair),other_pairs)
      else tempf(pair::treated,other_pairs)
  ) in 
  tempf([],l);; 

(* decompose_wrt_key [(1,"u");(2,"v");(3,"w");(4,"x");(5,"y");(6,"z")] 2;; *)



let reposition_by_putting_snd_immediately_after_fst l key_i key_j=
    let (left1,opt1,right1)=decompose_wrt_key l key_i in 
    if opt1=None then raise(Reposition_first_key_not_found) else 
    let (left2,opt2,right2)=decompose_wrt_key right1 key_j in 
    if opt2=None then raise(Reposition_second_key_not_found) else
    let pair1=Assistance_option.unpack opt1 and pair2=Assistance_option.unpack opt2 in 
    left1@(pair1::pair2::(left2@right2));; 
  
(* reposition_by_putting_snd_immediately_after_fst [(1,"u");(2,"v");(3,"w");(4,"x");(5,"y");(6,"z")] 2 5;; *)  

let reorder l key_ordering =Assistance_image.image (fun key->(key,List.assoc key l)) key_ordering;;

(* reorder [(1,"u");(2,"v");(3,"w");(4,"x");(5,"y");(6,"z")] [2;5;1;3;4;6];; *) 
 
let restrict l fewer_keys =List.filter (fun (key,_)->List.mem key fewer_keys) l;;

let override_with overriden_one overrider =
  let unchecked_keys = Assistance_image.image fst (overrider@overriden_one) in 
  let keys = Assistance_listennou.nonredundant_version unchecked_keys in 
  Assistance_image.image (
     fun key->match List.assoc_opt key overrider with 
     None -> (key,List.assoc key overriden_one)
     |Some(vaal)->(key,vaal)
  ) keys;;


end;;






module Assistance_cartesian=struct

(*

#use"cartesian.ml";;

*) 

let product a b=
if (a=[])||(b=[]) then [] else
let rec sub_f=(function
(accu,variable_a,constant_b)->match variable_a with
[]->List.rev(accu)
|u::v->sub_f(List.rev_append(List.rev(List.rev_map(function t->(u,t))(constant_b)))
(accu),v,constant_b)
) in
sub_f([],a,b);;

let square x=product x x;;

let tproduct a b c=List.rev_map(function ((x,y),z)->(x,y,z))
(List.rev(product(product(a)(b))(c)));;

let pproduct a b c d=List.rev_map(function ((x,y,z),t)->(x,y,z,t))
(List.rev(product(tproduct a b c)(d)));;

let qproduct a b c d e=List.rev_map(function ((x,y,z,t),u)->(x,y,z,t,u))
(List.rev(product(pproduct a b c d)(e)));;

let cube x=tproduct x x x;;

let fourth_power x=pproduct x x x x;;

let fifth_power x=qproduct x x x x x;;

let general_product x=
let rec sub_f=(function
([],accu)->accu
|(a::b,accu)->sub_f(b,List.rev_map(function (x,l)->x::l)(List.rev(product(a)(accu)))))
in
sub_f(List.rev(x),[[]]);;

let power x n=general_product (Assistance_ennig.doyle (fun j->x) 1 n);;
             

end;;






module Assistance_encoded_string_t=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/encoded_string_t.ml";;


*)


type t= E of string;;



end;;






module Assistance_concrete_object_t=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/concrete_object_t.ml";;


*)

type t= 
    Int of int 
   |String of Assistance_encoded_string_t.t 
   |Uple of t list
   |List of t list
   |Array of t list
   |Record of (string*t) list
   |Variant of string*(t list);;



end;;






module Assistance_concrete_object_automatic=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/concrete_object_field.ml";;

*)

module Exn = struct

   exception Get_record_absent_key_exn of string;;
   exception Get_record_bad_type_exn of Assistance_concrete_object_t.t;;
   exception Get_pair_exn of Assistance_concrete_object_t.t;;
   exception Unwrap_array_exn of Assistance_concrete_object_t.t;;
   exception Unwrap_int_exn of Assistance_concrete_object_t.t;;
   exception Unwrap_list_exn of Assistance_concrete_object_t.t;;
   exception Unwrap_string_exn of Assistance_concrete_object_t.t;;
   exception Wrap_lonely_variant_exn;;
   exception Unwrap_lonely_variant_exn of Assistance_concrete_object_t.t;;
   
   exception Uple_too_big of Assistance_concrete_object_t.t;;
   exception Uple_too_small of Assistance_concrete_object_t.t;;
   exception Unwrap_bounded_uple_exn of Assistance_concrete_object_t.t;;
   
   exception Variant_too_big of Assistance_concrete_object_t.t;;
   exception Variant_too_small of Assistance_concrete_object_t.t;;
   exception Unwrap_bounded_variant_exn of Assistance_concrete_object_t.t;;
   
   end;;
   
   let wrap_encoded_string encoded_s=Assistance_concrete_object_t.String(encoded_s);;
   
   
   let unwrap_list ccrt_obj=
      match ccrt_obj with 
      Assistance_concrete_object_t.List(l)->l 
      |_->raise(Exn.Unwrap_list_exn(ccrt_obj));;
   
   let unwrap_array ccrt_obj=
      match ccrt_obj with 
      Assistance_concrete_object_t.Array(l)->Array.of_list l 
      |_->raise(Exn.Unwrap_array_exn(ccrt_obj));;
   
   let get_record ccrt_obj field =
      match ccrt_obj with 
      Assistance_concrete_object_t.Record(l)->
           (try List.assoc field l with 
           _ ->raise(Exn.Get_record_absent_key_exn(field)))
      |_->raise(Exn.Get_record_bad_type_exn(ccrt_obj));;
   
   let unwrap_bounded_uple ccrt_obj=
     match ccrt_obj with 
      Assistance_concrete_object_t.Uple(l)->
         let n=List.length(l) in 
         if  n<2 then raise(Exn.Uple_too_small(ccrt_obj)) else 
         if  n>7 then raise(Exn.Uple_too_big(ccrt_obj)) else 
         let i3=(if n<3 then 1 else 3)
         and i4=(if n<4 then 1 else 4)
         and i5=(if n<5 then 1 else 5)
         and i6=(if n<6 then 1 else 6)
         and i7=(if n<7 then 1 else 7) in
         let get=(fun k->List.nth l (k-1)) in 
         (get 1,get 2,get i3,get i4,get i5,get i6,get i7)
      | _-> raise(Exn.Unwrap_bounded_uple_exn(ccrt_obj));;
   
   
   
   let unwrap_bounded_variant ccrt_obj=
     match ccrt_obj with 
      Assistance_concrete_object_t.Variant(constructor,l)->
         let n=List.length(l) in 
         if  n<1 then raise(Exn.Variant_too_small(ccrt_obj)) else 
         if  n>7 then raise(Exn.Variant_too_big(ccrt_obj)) else 
         let i2=(if n<2 then 1 else 2) 
         and i3=(if n<3 then 1 else 3)
         and i4=(if n<4 then 1 else 4)
         and i5=(if n<5 then 1 else 5)
         and i6=(if n<6 then 1 else 6)
         and i7=(if n<7 then 1 else 7) in
         let get=(fun k->List.nth l (k-1)) in 
         (constructor,(get 1,get i2,get i3,get i4,get i5,get i6,get i7))
      | _-> raise(Exn.Unwrap_bounded_variant_exn(ccrt_obj));;
   
   
   let wrap_lonely_variant l_pairs unwrapped=
      match Assistance_option.seek(fun (key,vaal)->key=unwrapped) l_pairs with
         None->raise(Exn.Wrap_lonely_variant_exn)
        |Some(_,constructor)->Assistance_concrete_object_t.Variant(constructor,[]) ;;
   
   
   let unwrap_lonely_variant l_pairs ccrt_obj=
      match ccrt_obj with 
      Assistance_concrete_object_t.Variant(constructor,l)->
         if  l<>[] then raise(Exn.Unwrap_lonely_variant_exn(ccrt_obj)) else 
         (match Assistance_option.seek(fun (_,key)->key=constructor) l_pairs with
         None->raise(Exn.Unwrap_lonely_variant_exn(ccrt_obj))
        |Some(vaal,_)->vaal) 
      |_->raise(Exn.Unwrap_lonely_variant_exn(ccrt_obj));;
   

end;;






module Assistance_io=struct

(*

#use"io.ml";;

*)


exception Open_in_exn of string ;;
exception Open_out_exn of string ;;
exception Dangerous_command_reading of string ;;

let max_size_for_reasonable_in_channel = ref(1000000);;

module Private = struct 

let make_filename_complete s=
  let home=Sys.getenv("HOME") in
  if s="" then Assistance_absolute_path.of_string(home) else
  let c=String.get s 0 in
  if c='/' then Assistance_absolute_path.of_string(s) else
  if c='~' then Assistance_absolute_path.of_string(home^(String.sub s 1 (String.length(s)-1))) else
  Assistance_absolute_path.of_string((Sys.getcwd ())^"/"^s);;

let open_in_locally x=try open_in_bin(x) with 
_->raise(Open_in_exn(x));;

let open_out_locally x=try open_out_bin(x) with 
_->raise(Open_out_exn(x));;  

let put_whole_content_of_file_in_buffer s=
  let x=Assistance_absolute_path.to_string(make_filename_complete(s)) in
  let janet=open_in_locally(x) in
  let n=in_channel_length(janet) in
  let b=Buffer.create(n) in
  let _=Buffer.add_channel(b)(janet)(n) in
  let _=close_in janet in
  b;;
  
  
type filename=string;;
  
let erase_file_and_fill_it_with_contents_of_buffer (fn:filename) b=
   let x=Assistance_absolute_path.to_string(make_filename_complete(fn)) in
  let john=open_out_locally(x) in
  (Buffer.output_buffer(john)(b);close_out john);;
  
let overwrite_with ap s=
   let fn=Assistance_absolute_path.to_string ap in
   let n=String.length(s) in
   let b=Buffer.create(n) in
   let _=Buffer.add_string b s in
   erase_file_and_fill_it_with_contents_of_buffer fn b;;
   
let read_whole_file ap=   
   let s=Assistance_absolute_path.to_string ap in
   let b=put_whole_content_of_file_in_buffer(s) in
   Buffer.contents b;;

let append_string_to_file s ap=
  let new_content=(read_whole_file ap)^s in
  overwrite_with ap new_content;; 
   
let read_reasonable_command cmd =
   let chan = Unix.open_process_in cmd in 
   let max_reasonable_size = (!max_size_for_reasonable_in_channel)+1 in 
   let buf = Bytes.create max_reasonable_size  in 
   let final_size = input chan buf 0 max_reasonable_size  in 
   let _ = Unix.close_process_in chan in 
   if (final_size=0)||(final_size >= max_reasonable_size) 
   then raise(Dangerous_command_reading(cmd))
   else 
   Bytes.sub_string buf 0 final_size ;;

end ;; 

let overwrite_with = Private.overwrite_with ;;
let read_reasonable_command = Private.read_reasonable_command ;;
let read_whole_file = Private.read_whole_file ;;
   
  
             

end;;






module Assistance_overwriter=struct

(*

#use"overwriter.ml";;

*)


type t=Ovw of string;;

let of_string s=Ovw(s);;
let to_string (Ovw s)=s;;
             

end;;






module Assistance_replace_inside=struct

(*

#use"replace_inside.ml";;

*)


let replace_inside_string (a,b) s=
  Assistance_alternative_global_replace.my_global_replace (a,b) s;;
 
let replace_several_inside_string l t=List.fold_left 
(fun s (a,b)->replace_inside_string (a,b) s) t l;;  
 
let replace_inside_file (a,b) fn=
    let s1=Assistance_io.read_whole_file fn in
    let la=String.length(a) in
    if List.exists (fun j->(String.sub s1 j la)=a) (Assistance_ennig.ennig 0 ((String.length s1)-la))
    then let s2=replace_inside_string (a,b) s1 in
         Assistance_io.overwrite_with fn s2
    else ();; 
    
let replace_several_inside_file l fn=
    let s1=Assistance_io.read_whole_file fn in
    let s2=replace_several_inside_string l s1  in
    Assistance_io.overwrite_with fn s2;; 

exception Absent_beginning_marker of string;;
exception Absent_ending_marker of string;; 
 
let overwrite_between_markers_inside_string ovw_b (bm,em)
   s1=
     let b=Assistance_overwriter.to_string ovw_b in
     if (bm,em)=("","") then b else
     let substring_leftmost_index_from=(fun x y i0->
      let lx=String.length(x) and ly=String.length(y) in
      let rec tempf=(fun j->
        if j>ly-lx then (-1) else 
        if (String.sub y j lx)=x then j else (tempf(j+1))
      ) in
      tempf i0) in
     let i1=substring_leftmost_index_from bm s1 0 in
     if i1=(-1) then raise(Absent_beginning_marker(bm)) else
     let j1=i1+(String.length bm)-1 in
     let i2=substring_leftmost_index_from em s1 (j1+1) in
     if i2=(-1) then raise(Absent_ending_marker(em)) else
     let before=String.sub s1 0 (j1+1)
     and after=String.sub s1 i2 (String.length(s1)-i2) 
     in
     before^b^after ;; 
     
let overwrite_between_markers_inside_file 
   ovw_b (bm,em)
   fn =
    let s1=Assistance_io.read_whole_file fn in
    let s2=overwrite_between_markers_inside_string ovw_b (bm,em) s1 in
    Assistance_io.overwrite_with fn s2;;      


let overwrite_and_dump_markers_inside_string ovw_b (bm,em)
   s1=
     let b=Assistance_overwriter.to_string ovw_b in
     if (bm,em)=("","") then b else
     let substring_leftmost_index_from=(fun x y i0->
      let lx=String.length(x) and ly=String.length(y) in
      let rec tempf=(fun j->
        if j>ly-lx then (-1) else 
        if (String.sub y j lx)=x then j else (tempf(j+1))
      ) in
      tempf i0) in
     let i1=substring_leftmost_index_from bm s1 0 in
     if i1=(-1) then raise(Absent_beginning_marker(bm)) else
     let j1=i1+(String.length bm)-1 in
     let i2=substring_leftmost_index_from em s1 (j1+1) in
     if i2=(-1) then raise(Absent_ending_marker(bm)) else
     let corrected_i2=i2+(String.length bm)-1 in
     let before=String.sub s1 0 i1
     and after=String.sub s1 corrected_i2 (String.length(s1)-corrected_i2) 
     in
     before^b^after ;; 
     
let overwrite_and_dump_markers_inside_file 
   ovw_b (bm,em)
   fn =
    let s1=Assistance_io.read_whole_file fn in
    let s2=overwrite_and_dump_markers_inside_string ovw_b (bm,em) s1 in
    Assistance_io.overwrite_with fn s2;;      
 
(* 


 overwrite_between_markers_inside_string
  (Overwriter.of_string "456")
  ("aaa","bb")
   "123aaa5678bb78910" ;;    
   
overwrite_and_dump_markers_inside_string
  (Overwriter.of_string "456")
  ("aaa","bb")
   "123aaa5678bb78910" ;;       
   
     
*)

let at_char_intervals_inside_string s l=
  if l=[] then s else
  let n=String.length s in
  let temp1=Assistance_listennou.universal_delta_list l 
  and ((i_first,_),_)=List.hd(l)
  and ((i_last,j_last),rep_last)=List.hd(List.rev l) in
  let temp2=Assistance_image.image (fun (((i1,j1),rep1),((i2,j2),rep2))->
      rep1^(String.sub s j1 (i2-j1-1))
  ) temp1 in
  let first_part=(String.sub s 0 (i_first-1))
  and last_part=rep_last^(String.sub s j_last (n-j_last)) in
  first_part^(String.concat "" temp2)^last_part;;

let at_char_intervals_inside_file 
  fn l=
   let s1=Assistance_io.read_whole_file fn in
   let s2=at_char_intervals_inside_string s1 l in
   Assistance_io.overwrite_with fn s2;;     

(*    

at_char_intervals_inside_string "12345678901234567890" [(3,5),"right";(12,17),"again"];;

*)         






end;;






module Assistance_encoded_string=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/encoded_string.ml";;

This is a leaky abstraction, as witnessed by the "retrieve" and "store" functions above.


*)

exception Forbidden_substring;;
exception Forbidden_recombination;;

let salt = 
  String.concat "" ["a"; "Y"; "2"; "u"; "k"; "k"; "w"; "D"; "z"; "y"; "K"; "d"];;
let replacement_salt = 
  String.concat "" ["b"; "Z"; "3"; "v"; "l"; "m"; "x"; "E"; "A"; "z"; "L"; "e"];;

let decode (Assistance_encoded_string_t.E(encoded_s))=
   Assistance_replace_inside.replace_inside_string (replacement_salt,salt) encoded_s;;

let encode s=
   if Assistance_substring.is_a_substring_of replacement_salt s 
   then raise(Forbidden_substring)
   else let encoded_s=Assistance_replace_inside.replace_inside_string (salt,replacement_salt) s in 
        if  Assistance_substring.is_a_substring_of salt encoded_s 
        then raise(Forbidden_substring)
        else Assistance_encoded_string_t.E(encoded_s);;

let retrieve encoded_s = (Assistance_encoded_string_t.E(encoded_s));;
let store (Assistance_encoded_string_t.E(encoded_s))= encoded_s;;


end;;






module Assistance_crobj_converter=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_converter.ml";;

*)



exception Unwrap_int_exn of Assistance_concrete_object_t.t;;
exception Unwrap_string_exn of Assistance_concrete_object_t.t;;


let bool_of_concrete_object = Assistance_concrete_object_automatic.unwrap_lonely_variant [true,"True";false,"False"];;
let bool_to_concrete_object bowl = 
    if bowl 
    then Assistance_concrete_object_t.Variant("True",[]) 
    else Assistance_concrete_object_t.Variant("False",[]);;  

let int_of_concrete_object ccrt_obj =
    match ccrt_obj with 
     Assistance_concrete_object_t.Int(i)->i 
    |_->raise(Unwrap_int_exn(ccrt_obj)) ;;
let int_to_concrete_object i = Assistance_concrete_object_t.Int i ;;       

let string_of_concrete_object ccrt_obj =
       match ccrt_obj with 
       Assistance_concrete_object_t.String(encoded_s)->Assistance_encoded_string.decode encoded_s 
       |_->raise(Unwrap_string_exn(ccrt_obj)) ;;
let string_to_concrete_object s = Assistance_concrete_object_t.String(Assistance_encoded_string.encode s) ;;       

end;;






module Assistance_dfa_root_t=struct

(*

Does not end with a slash.

#use"Decomposed_filename/dfa_root_t.ml";;

*)

type t=R of string;;           

end;;






module Assistance_dfa_root=struct

(*

The rightmost trailing slash is removed.

#use"Decomposed_filename/dfa_root.ml";;

*)

let without_trailing_slash (Assistance_dfa_root_t.R s)=s;;
let connectable_to_subpath (Assistance_dfa_root_t.R s)=s^"/";;  

let of_line line = Assistance_dfa_root_t.R(Assistance_tools_for_absolute_path.remove_trailing_slash line);;


let to_concrete_object (Assistance_dfa_root_t.R(line))=
    Assistance_concrete_object_t.Variant("Dfa_"^"root.R",[Assistance_crobj_converter.string_to_concrete_object(line)]);;

let of_concrete_object ccrt_obj =
   let (_,(arg1,_,_,_,_,_,_))=Assistance_concrete_object_automatic.unwrap_bounded_variant ccrt_obj in 
   Assistance_dfa_root_t.R(Assistance_crobj_converter.string_of_concrete_object arg1);;



end;;






module Assistance_crobj_converter_combinator=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_converter_combinator.ml";;

*)

let of_array of_a arr= Assistance_concrete_object_t.Array(Array.to_list(Array.map of_a arr));;
let to_array to_a crobj= Array.map to_a (Assistance_concrete_object_automatic.unwrap_array crobj);;

let of_list of_a l= Assistance_concrete_object_t.List(Assistance_image.image of_a l);;
let to_list to_a crobj= Assistance_image.image to_a (Assistance_concrete_object_automatic.unwrap_list crobj);;
   
   
let of_pair of_a of_b (a,b)=Assistance_concrete_object_t.Uple[of_a a;of_b b];;
let to_pair to_a to_b crobj=
       let (arg1,arg2,_,_,_,_,_)=Assistance_concrete_object_automatic.unwrap_bounded_uple crobj in
       (to_a arg1,to_b arg2);;
   
let of_triple of_a of_b of_c (a,b,c)=Assistance_concrete_object_t.Uple[of_a a;of_b b;of_c c];;
let to_triple to_a to_b to_c crobj=
           let (arg1,arg2,arg3,_,_,_,_)=Assistance_concrete_object_automatic.unwrap_bounded_uple crobj in
           (to_a arg1,to_b arg2,to_c arg3);;
   

let of_pair_list of_a of_b l=of_list (of_pair of_a of_b) l;;
let to_pair_list to_a to_b crobj = to_list (to_pair to_a to_b) crobj;;
   
   


end;;






module Assistance_dfa_ending_t=struct

(*

#use"Decomposed_filename/dfa_ending_t.ml";;

Does not contain a dot.

*)

type t=E of string;;



end;;






module Assistance_dfa_ocaml_ending_t=struct

(*

#use"Decomposed_filename/dfa_ocaml_ending_t.ml";;

*)

type t=Ml |Mli |Mll |Mly;;



end;;






module Assistance_dfa_ending=struct

(*

#use"Decomposed_filename/dfa_ending.ml";;

*)



exception Dot_inside_ending of string;;
exception Not_an_ocaml_ending of string;;
exception Unknown_ending of Assistance_dfa_ending_t.t ;;

let of_line e =
  if String.contains e '.'
  then raise(Dot_inside_ending(e))
  else Assistance_dfa_ending_t.E(e);;

let connectable_to_modulename (Assistance_dfa_ending_t.E(e)) = "." ^ e ;;

let restrict_to_ocaml_ending (Assistance_dfa_ending_t.E(e)) =
   if not(Assistance_supstring.begins_with e "ml")
   then raise(Not_an_ocaml_ending(e))
   else let n=String.length(e) in 
        if n=2 then Assistance_dfa_ocaml_ending_t.Ml else 
        if n>3 then raise(Not_an_ocaml_ending(e)) else 
        match String.get e 2 with 
         'i'->Assistance_dfa_ocaml_ending_t.Mli
        |'l'->Assistance_dfa_ocaml_ending_t.Mll
        |'y'->Assistance_dfa_ocaml_ending_t.Mly
        | _ -> raise(Not_an_ocaml_ending(e));;

let mll =  Assistance_dfa_ending_t.E "mll"
and mly =  Assistance_dfa_ending_t.E "mly"
and ml  =  Assistance_dfa_ending_t.E "ml"
and mli =  Assistance_dfa_ending_t.E "mli" ;; 

let all_ocaml_endings= [mll;mly;ml;mli];;

let all_cee_endings = Assistance_image.image (fun s->Assistance_dfa_ending_t.E s) ["h";"c"];;



let compute_on_all_ocaml_endings f=(f ml,f mli,f mll,f mly);;

let endings_for_compilable_files = 
   (all_ocaml_endings) @ all_cee_endings ;;

let endings_for_noncompilable_readable_files = 
     Assistance_image.image (fun s->Assistance_dfa_ending_t.E s) ["txt";"html";"php";"js";"ejs";"json"];; 

let endings_for_readable_files = 
     endings_for_compilable_files @ endings_for_noncompilable_readable_files ;;

let is_compilable edg =
   if List.mem edg endings_for_compilable_files 
   then true 
   else 
   if List.mem edg endings_for_noncompilable_readable_files 
   then false 
   else raise(Unknown_ending(edg));;



let to_concrete_object (Assistance_dfa_ending_t.E(e)) =
    Assistance_concrete_object_t.Variant ("Dfa_"^"ending_t.E",
    [Assistance_crobj_converter.string_to_concrete_object(e)]);;

let of_concrete_object crobj =
   let (_,(arg1,_,_,_,_,_,_))=Assistance_concrete_object_automatic.unwrap_bounded_variant crobj in 
   Assistance_dfa_ending_t.E(
      Assistance_crobj_converter.string_of_concrete_object arg1
   );;



end;;






module Assistance_dfa_module_t=struct

(*

#use"Decomposed_filename/dfa_module_t.ml";;

A module name, or a candidate for one. Uncapitalized. 
Should contain no slashes.

*)

type t=M of string;;

           

end;;






module Assistance_dfa_module=struct

(*

#use"Decomposed_filename/dfa_module.ml";;

A module name, or a candidate for one. Uncapitalized. Should contain no slashes.

*)


let of_line s=Assistance_dfa_module_t.M (String.uncapitalize_ascii s);; 
let to_line (Assistance_dfa_module_t.M s)=s;;

let add_prefix_and_capitalize prefix (Assistance_dfa_module_t.M name)=
  Assistance_dfa_module_t.M(String.capitalize_ascii(prefix^name));;

  
let capitalized_form (Assistance_dfa_module_t.M name)= String.capitalize_ascii name;;

let to_concrete_object (Assistance_dfa_module_t.M(s))=
    Assistance_concrete_object_t.Variant("Dfa_"^"module_t.M",
     [Assistance_crobj_converter.string_to_concrete_object(s)]);;

let of_concrete_object ccrt_obj =
   let (_,(arg1,_,_,_,_,_,_))=Assistance_concrete_object_automatic.unwrap_bounded_variant ccrt_obj in 
   Assistance_dfa_module_t.M(Assistance_crobj_converter.string_of_concrete_object arg1);;


end;;






module Assistance_dfa_subdirectory_t=struct

(*

Subdirectories name, with the trailing slash removed.

#use"Decomposed_filename/dfa_subdirectory_t.ml";;

*)

type t=SD of string;;



end;;






module Assistance_dfa_subdirectory=struct

(*

Subdirectories name, with the trailing slash removed.

#use"Decomposed_filename/dfa_subdirectory.ml";;


*)

let without_trailing_slash (Assistance_dfa_subdirectory_t.SD s)=s;;

let connectable_to_subpath (Assistance_dfa_subdirectory_t.SD s)=
  if s="" 
  then "" 
  else s^"/";;

let begins_with (Assistance_dfa_subdirectory_t.SD s1) (Assistance_dfa_subdirectory_t.SD s2)=
   Assistance_supstring.begins_with s1 s2;;
    
let extend (Assistance_dfa_subdirectory_t.SD s) subsub = Assistance_dfa_subdirectory_t.SD (s^"/"^subsub);;

let main = Assistance_dfa_subdirectory_t.SD "";;

let of_line s=
  let n = String.length s in 
  let indices = List.rev(Assistance_ennig.ennig 1 n) in 
  let limit_idx=(match Assistance_option.seek(fun j->(Assistance_strung.get s j)<>'/')(indices) with 
     None -> 0 |Some(j)->j
  ) in 
  Assistance_dfa_subdirectory_t.SD (Assistance_cull_string.beginning limit_idx s);;


let rename_endsubdirectory (Assistance_dfa_subdirectory_t.SD(old_subdir),new_esdname) 
   (Assistance_dfa_subdirectory_t.SD s)=
   if Assistance_supstring.begins_with s old_subdir
   then let sub_s=Assistance_cull_string.cobeginning (String.length old_subdir) s in
        let t=Assistance_cull_string.before_rightmost old_subdir '/' in
        let new_t=(if t="" then "" else t^"/") in
        Assistance_dfa_subdirectory_t.SD(new_t^new_esdname^sub_s)
   else Assistance_dfa_subdirectory_t.SD(s);;
   
(*

rename_endsubdirectory (SD("Haag/Huug"),"Java") (SD "Haag/Huug/King/Jordan");;
rename_endsubdirectory (SD("Haag"),"Java") (SD "Haag/Huug/King/Jordan");;

*)              


let soak (Assistance_dfa_subdirectory_t.SD(s1),Assistance_dfa_subdirectory_t.SD(s2)) (Assistance_dfa_subdirectory_t.SD(s))=
   match Assistance_strung.soak (s1,s2) s with 
   Some(t)->Some(Assistance_dfa_subdirectory_t.SD(t))
   |None -> None ;;

let to_concrete_object (Assistance_dfa_subdirectory_t.SD(s))=
    Assistance_concrete_object_t.Variant("Dfa_"^"subdirectory_t.SD",
    [Assistance_crobj_converter.string_to_concrete_object(s)]);;

let of_concrete_object ccrt_obj =
   let (_,(arg1,_,_,_,_,_,_))=Assistance_concrete_object_automatic.unwrap_bounded_variant ccrt_obj in 
   Assistance_dfa_subdirectory_t.SD(Assistance_crobj_converter.string_of_concrete_object arg1);;






end;;






module Assistance_dfn_rootless_t=struct

(*

#use"Decomposed_filename/dfn_rootless_t.ml";;

*)

type t = J of Assistance_dfa_subdirectory_t.t * Assistance_dfa_module_t.t * Assistance_dfa_ending_t.t ;;
          



end;;






module Assistance_dfn_common=struct

(*

#use"Decomposed_filename/dfn_common.ml";;

*)

exception No_dot_in_string_to_rootless of string;;
exception Decompose_absolute_path_using_root_exn of Assistance_absolute_path.t * Assistance_dfa_root_t.t;;

let string_of_sm (s,m)=
   let (Assistance_dfa_subdirectory_t.SD sub)=s 
   and (Assistance_dfa_module_t.M mn)=m in 
   if sub=""
   then mn
   else sub^"/"^mn;;

let string_to_sm s=
   let (sub,mn)= Assistance_cull_string.split_wrt_rightmost s '/' in 
   (Assistance_dfa_subdirectory_t.SD sub,Assistance_dfa_module_t.M mn);;     

let string_to_rootless line=
  if not(String.contains line '.')
  then raise(No_dot_in_string_to_rootless(line)) 
  else 
  let (rest,ending) = Assistance_cull_string.split_wrt_rightmost line '.' in 
  let (s,m) = string_to_sm rest in 
  Assistance_dfn_rootless_t.J(s,m,Assistance_dfa_ending_t.E ending);;

   
let decompose_absolute_path_using_root ap root=
  let s_root=Assistance_dfa_root.without_trailing_slash root  
  and s_ap = Assistance_absolute_path.to_string ap in 
  let ns=String.length(s_root)
  and nw=String.length(s_ap) in
  if (ns+1)>nw then raise(Decompose_absolute_path_using_root_exn(ap,root)) else
  if (String.sub s_ap 0 (ns+1))<>(s_root^"/") then raise(Decompose_absolute_path_using_root_exn(ap,root)) else
  string_to_rootless(String.sub s_ap (ns+1) (nw-ns-1));;

let recompose_potential_absolute_path root 
      (Assistance_dfn_rootless_t.J(Assistance_dfa_subdirectory_t.SD(s),Assistance_dfa_module_t.M(m),Assistance_dfa_ending_t.E(e))) =
    let s_subdir = (if s="" then "" else s^"/") in 
    (Assistance_dfa_root.connectable_to_subpath root)^s_subdir^m^"."^e;;


end;;






module Assistance_dfn_middle_t=struct

(*

#use"Decomposed_filename/dfn_middle_t.ml";;

*)

type t = J of Assistance_dfa_subdirectory_t.t * Assistance_dfa_module_t.t ;;
          



end;;






module Assistance_dfn_rootless=struct

(*

#use"Decomposed_filename/dfn_rootless.ml";;


*)

module Private = struct 

let of_concrete_object crobj =
   let (_,(arg1,arg2,arg3,_,_,_,_))=Assistance_concrete_object_automatic.unwrap_bounded_variant crobj in 
   Assistance_dfn_rootless_t.J(
      Assistance_dfa_subdirectory.of_concrete_object arg1,
      Assistance_dfa_module.of_concrete_object arg2,
      Assistance_dfa_ending.of_concrete_object arg3
   );;

let to_concrete_object (Assistance_dfn_rootless_t.J(s,m,e))=
   Assistance_concrete_object_t.Variant("Dfn_"^"rootless.J",
     [
        
        Assistance_dfa_subdirectory.to_concrete_object s;
        Assistance_dfa_module.to_concrete_object m;
        Assistance_dfa_ending.to_concrete_object e;
     ]
   ) ;;



end ;; 


let is_compilable (Assistance_dfn_rootless_t.J(s,m,e))= Assistance_dfa_ending.is_compilable e;;

let is_in (Assistance_dfn_rootless_t.J(s,m,e)) sd = Assistance_dfa_subdirectory.begins_with s sd;;

let list_of_concrete_object crobj=
  Assistance_crobj_converter_combinator.to_list Private.of_concrete_object crobj ;;

let list_to_concrete_object l=
   Assistance_crobj_converter_combinator.of_list Private.to_concrete_object l;;


let of_concrete_object = Private.of_concrete_object ;;

let of_line line = Assistance_dfn_common.string_to_rootless line;;

let pair_list_of_concrete_object crobj=
  Assistance_crobj_converter_combinator.to_pair_list Private.of_concrete_object Private.of_concrete_object crobj ;;

let pair_list_to_concrete_object l=
  Assistance_crobj_converter_combinator.of_pair_list Private.to_concrete_object Private.to_concrete_object l;;


let relocate_to (Assistance_dfn_rootless_t.J(old_subdir,m,e)) new_subdir=Assistance_dfn_rootless_t.J(new_subdir,m,e);;
     
let rename_subdirectory_as  (old_subdir,new_subdir) old_path=
   let (Assistance_dfn_rootless_t.J(s,m,e))=old_path in 
   if s=old_subdir
   then Assistance_dfn_rootless_t.J(new_subdir,m,e)
   else old_path;;

let soak (old_subdir,new_subdir) (Assistance_dfn_rootless_t.J(s,m,e)) =
   match Assistance_dfa_subdirectory.soak (old_subdir,new_subdir) s with 
   Some(new_s)->Some(Assistance_dfn_rootless_t.J(new_s,m,e))
   |None -> None ;;

let to_concrete_object = Private.to_concrete_object ;;

let to_ending (Assistance_dfn_rootless_t.J(s,m,e))=e;;

let to_line (Assistance_dfn_rootless_t.J(s,m,e))=
   (Assistance_dfa_subdirectory.connectable_to_subpath s)^
   (Assistance_dfa_module.to_line m)^(Assistance_dfa_ending.connectable_to_modulename e);;

let to_middle (Assistance_dfn_rootless_t.J(s,m,e))=Assistance_dfn_middle_t.J(s,m);;

let to_module (Assistance_dfn_rootless_t.J(s,m,e))=m;;

let to_subdirectory (Assistance_dfn_rootless_t.J(s,m,e))=s;;





    


end;;






module Assistance_dircopy_diff_t=struct

(*

#use"dircopy_diff_t.ml";;

*)

type t={
   recently_deleted : Assistance_dfn_rootless_t.t list;
   recently_changed : Assistance_dfn_rootless_t.t list;
   recently_created : Assistance_dfn_rootless_t.t list;
};;



end;;






module Assistance_dfn_endingless_t=struct

(*

#use"Decomposed_filename/dfn_endingless_t.ml";;

*)

type t = J of Assistance_dfa_root_t.t * Assistance_dfa_subdirectory_t.t * Assistance_dfa_module_t.t  ;;



end;;






module Assistance_dfn_full_t=struct

(*

#use"Decomposed_filename/dfn_full_t.ml";;

*)


type t = J of Assistance_dfa_root_t.t * Assistance_dfa_subdirectory_t.t * Assistance_dfa_module_t.t * Assistance_dfa_ending_t.t;;



end;;






module Assistance_dfn_short_t=struct

(*

#use"Decomposed_filename/dfn_short_t.ml";;

*)

type t = J of Assistance_dfa_module_t.t * Assistance_dfa_ending_t.t;;
          



end;;






module Assistance_dfn_join=struct

(*

#use"Decomposed_filename/dfn_join.ml";;

*)

let middle_to_ending (Assistance_dfn_middle_t.J(s,m)) e= (Assistance_dfn_rootless_t.J(s,m,e));;
let root_to_middle r (Assistance_dfn_middle_t.J(s,m))=Assistance_dfn_endingless_t.J(r,s,m);; 
let root_to_rootless r (Assistance_dfn_rootless_t.J(s,m,e))=Assistance_dfn_full_t.J(r,s,m,e);;
let root_to_subdirectory (Assistance_dfa_root_t.R(r)) (Assistance_dfa_subdirectory_t.SD(s))=Assistance_dfa_root_t.R(r^"/"^s);;
let subdirectory_to s (Assistance_dfn_short_t.J(m,e))=(Assistance_dfn_rootless_t.J(s,m,e));;  
let to_ending (Assistance_dfn_endingless_t.J(r,s,m)) e = Assistance_dfn_full_t.J(r,s,m,e);;



end;;






module Assistance_dfn_short=struct

(*

#use"Decomposed_filename/dfn_short.ml";;

*)

exception Of_line_exn of string;;

let of_line line = 
   let (mn,e)=Assistance_cull_string.split_wrt_rightmost line '.' in 
   if mn=""
   then raise(Of_line_exn(line))
   else Assistance_dfn_short_t.J(Assistance_dfa_module.of_line mn,Assistance_dfa_ending.of_line(e));;


let to_line (Assistance_dfn_short_t.J(m,e))=
   (Assistance_dfa_module.to_line m)^(Assistance_dfa_ending.connectable_to_modulename e);;

let to_module (Assistance_dfn_short_t.J(m,e))=m;;

end;;






module Assistance_particular_string=struct

(*

#use"particular_string.ml";;


*)



let double_semicolon=String.make 2 (char_of_int 59);; (* to make life easier for the OCaml mini-parser *)

let triple_blank = String.make 3 ' ';;
           

end;;






module Assistance_coma_constant=struct

(* 

#use"Compilation_management/coma_constant.ml";;

*)

module Private = struct


let utility_files_subdir=
  Assistance_dfa_subdirectory.of_line "Utility_files";;

let fads_subdir=
  Assistance_dfa_subdirectory.of_line "Fads";;

let githubbed_archive_subdir=
  Assistance_dfa_subdirectory.of_line "Githubbed_archive";;

let persistent_data_subdir = 
   Assistance_dfa_subdirectory.extend utility_files_subdir "Persistent_data";;


let build_subdir =   Assistance_dfa_subdirectory.of_line "_build";;
let usual_build_subdir= Assistance_dfa_subdirectory.extend build_subdir "_usual_build";;
let debug_build_subdir= Assistance_dfa_subdirectory.extend build_subdir "_debug_build";;  
let exec_build_subdir=  Assistance_dfa_subdirectory.extend build_subdir "_exec_build";;  
let parameters_subdir= Assistance_dfa_subdirectory.of_line "Compilation_management";;




let short_path_for_loadingsfile= Assistance_dfn_short.of_line"my_loadings.ml";;
let short_path_for_painful_debugging_file=Assistance_dfn_short.of_line"painful_debugging.ml";;
let short_path_for_parametersfile= Assistance_dfn_short.of_line "coma_big_constant.ml";;
let short_path_for_printersfile= Assistance_dfn_short.of_line "my_printers.ml";;
let short_path_for_targetfile= Assistance_dfn_short.of_line "targetfile.ocaml_made";;
 
let rootless_path_for_loadingsfile=
  Assistance_dfn_join.subdirectory_to  utility_files_subdir short_path_for_loadingsfile;;
let rootless_path_for_painful_debugging_file=
  Assistance_dfn_join.subdirectory_to  fads_subdir short_path_for_painful_debugging_file;;
let rootless_path_for_parametersfile=
  Assistance_dfn_join.subdirectory_to  parameters_subdir short_path_for_parametersfile;;
let rootless_path_for_printersfile=
  Assistance_dfn_join.subdirectory_to  utility_files_subdir short_path_for_printersfile;;
let rootless_path_for_targetfile=
  Assistance_dfn_join.subdirectory_to  persistent_data_subdir short_path_for_targetfile;;     

let rootless_path_for_ocamlinit = Assistance_dfn_rootless.of_line ".ocamlinit";;


let git_ignored_subdirectories =
  [
     utility_files_subdir;
     build_subdir;
     githubbed_archive_subdir;
     fads_subdir;
  ];;


let minimalist_text_for_ocamlinit =
   "\n#use\""^(Assistance_dfn_rootless.to_line rootless_path_for_loadingsfile)^"\""^Assistance_particular_string.double_semicolon^
  "\n#use\""^(Assistance_dfn_rootless.to_line rootless_path_for_printersfile)^"\""^Assistance_particular_string.double_semicolon;;

 let full_text_for_ocamlinit = (
      minimalist_text_for_ocamlinit^
      "\nopen Needed_values"^Assistance_particular_string.double_semicolon^
      "\ninitialize_toplevel()"^Assistance_particular_string.double_semicolon
       ) ;; 

let text_for_printersfile = "\n\n (*Registered printers start here *) \n\n (*Registered printers end here *) \n\n" ;;
let text_for_painful_debugging_file  = "\n\n(*\n\n#use\"Temporary/painful_debugging.ml\""^Assistance_particular_string.double_semicolon^"\n\n*)\n\n" ;;

let common_part_in_conventional_files = 
   [
     rootless_path_for_printersfile, text_for_printersfile ; 
     rootless_path_for_loadingsfile, "" ;
     rootless_path_for_targetfile, "";
     
   ] ;;     


let conventional_files_with_full_content =  
   [
     rootless_path_for_ocamlinit, full_text_for_ocamlinit ;
     rootless_path_for_painful_debugging_file, text_for_painful_debugging_file;
   ] @ common_part_in_conventional_files ;;      

let conventional_files_with_minimal_content =    
   [
     rootless_path_for_ocamlinit, minimalist_text_for_ocamlinit ;
   ] @ common_part_in_conventional_files ;;      


let minimal_set_of_needed_dirs = 
  [
    persistent_data_subdir ; 
    usual_build_subdir ;
    utility_files_subdir
  ] ;;  

let full_set_of_needed_dirs = 
  minimal_set_of_needed_dirs @
    [
      fads_subdir 
    ] ;;  

end ;;

 let conventional_files_with_full_content = Private.conventional_files_with_full_content ;;
 let conventional_files_with_minimal_content = Private.conventional_files_with_minimal_content ;;
 let debug_build_subdir = Private.debug_build_subdir ;;
 let exec_build_subdir = Private.exec_build_subdir ;;
 let full_set_of_needed_dirs = Private.full_set_of_needed_dirs ;;
 let git_ignored_subdirectories = Private.git_ignored_subdirectories ;;
 let githubbed_archive_subdir = Private.githubbed_archive_subdir ;;
 let minimal_set_of_needed_dirs = Private.minimal_set_of_needed_dirs ;;
 let rootless_path_for_loadingsfile = Private.rootless_path_for_loadingsfile ;;
 let rootless_path_for_parametersfile = Private.rootless_path_for_parametersfile ;;
 let rootless_path_for_printersfile = Private.rootless_path_for_printersfile ;;
 let rootless_path_for_targetfile = Private.rootless_path_for_targetfile ;;
 let usual_build_subdir = Private.usual_build_subdir ;;
 let utility_files_subdir = Private.utility_files_subdir ;;




end;;






module Assistance_fw_configuration_t=struct

(*

#use"Filewatching/fw_configuration_t.ml";;


In the encoding_protected_files field, the list elements are pair of files ;
the first elt of the pair contains the encoding, and the second elt is the
encoded file. This allows you to vary the encoding depending on the file.

*)

type t ={
  root : Assistance_dfa_root_t.t ;
  dir_for_backup : Assistance_dfa_root_t.t;
  gitpush_after_backup : bool;
  ignored_subdirectories : Assistance_dfa_subdirectory_t.t list;
  ignored_files : Assistance_dfn_rootless_t.t list;
  github_url : string;
  encoding_protected_files : ( Assistance_dfn_rootless_t.t * Assistance_dfn_rootless_t.t) list;
  subdirs_for_archived_mlx_files : Assistance_dfa_subdirectory_t.t list ; 
};;

end;;






module Assistance_fw_configuration=struct

(*

#use"Filewatching/fw_configuration.ml";;

*)

module Private = struct 

let salt = "Fw_"^"configuration_t.";;

let root_label                           = salt ^ "root";;
let dir_for_backup_label                 = salt ^ "dir_for_backup";;
let gitpush_after_backup_label           = salt ^ "gitpush_after_backup";;
let ignored_subdirectories_label         = salt ^ "ignored_subdirectories";;
let ignored_files_label                  = salt ^ "ignored_files";;
let github_url_label                     = salt ^ "github_url";;
let encoding_protected_files_label       = salt ^ "encoding_protected_files";;
let subdirs_for_archived_mlx_files_label = salt ^ "subdirs_for_archived_mlx_files";;

let of_concrete_object ccrt_obj = 
   let g=Assistance_concrete_object_automatic.get_record ccrt_obj in
   {
      Assistance_fw_configuration_t.root = Assistance_dfa_root.of_concrete_object(g root_label);
      dir_for_backup = Assistance_dfa_root.of_concrete_object(g dir_for_backup_label);
      gitpush_after_backup = Assistance_crobj_converter.bool_of_concrete_object (g gitpush_after_backup_label);
      ignored_subdirectories = Assistance_crobj_converter_combinator.to_list Assistance_dfa_subdirectory.of_concrete_object(g ignored_subdirectories_label);
      ignored_files = Assistance_crobj_converter_combinator.to_list Assistance_dfn_rootless.of_concrete_object (g ignored_files_label);
      github_url = Assistance_crobj_converter.string_of_concrete_object (g github_url_label);
      encoding_protected_files = Assistance_dfn_rootless.pair_list_of_concrete_object (g encoding_protected_files_label);
      subdirs_for_archived_mlx_files = Assistance_crobj_converter_combinator.to_list Assistance_dfa_subdirectory.of_concrete_object(g subdirs_for_archived_mlx_files_label);
   };; 

let to_concrete_object config=
   let items= 
   [
    root_label, Assistance_dfa_root.to_concrete_object config.Assistance_fw_configuration_t.root;
    dir_for_backup_label, Assistance_dfa_root.to_concrete_object config.Assistance_fw_configuration_t.dir_for_backup;
    gitpush_after_backup_label, Assistance_crobj_converter.bool_to_concrete_object  config.Assistance_fw_configuration_t.gitpush_after_backup;
    ignored_subdirectories_label, Assistance_crobj_converter_combinator.of_list Assistance_dfa_subdirectory.to_concrete_object config.Assistance_fw_configuration_t.ignored_subdirectories;
    ignored_files_label, Assistance_crobj_converter_combinator.of_list Assistance_dfn_rootless.to_concrete_object config.Assistance_fw_configuration_t.ignored_files;
    github_url_label, Assistance_crobj_converter.string_to_concrete_object config.Assistance_fw_configuration_t.github_url;
    encoding_protected_files_label, Assistance_dfn_rootless.pair_list_to_concrete_object config.Assistance_fw_configuration_t.encoding_protected_files;
    subdirs_for_archived_mlx_files_label, Assistance_crobj_converter_combinator.of_list Assistance_dfa_subdirectory.to_concrete_object config.Assistance_fw_configuration_t.subdirs_for_archived_mlx_files;
   ]  in
   Assistance_concrete_object_t.Record items;;

end ;;

let root config = config.Assistance_fw_configuration_t.root;;
let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;

let constructor (root_dir,backup_dir,g_after_b,git_url,secret_files,subdirs_for_archived) = 
    {
      Assistance_fw_configuration_t.root = root_dir;
      dir_for_backup = backup_dir ;
      gitpush_after_backup = g_after_b ;
      ignored_subdirectories = Assistance_coma_constant.git_ignored_subdirectories;
      ignored_files = [];
      github_url = git_url;
      encoding_protected_files = secret_files;
      subdirs_for_archived_mlx_files = subdirs_for_archived;
    };; 

let test_for_admissibility data rl=
  (List.mem (
    (Assistance_dfn_rootless.to_ending rl)
  ) Assistance_dfa_ending.endings_for_readable_files)
  &&
   (List.for_all (
     fun sd->not(Assistance_dfn_rootless.is_in rl sd)
  ) data.Assistance_fw_configuration_t.ignored_subdirectories
  )  
  &&
  (
    not(List.mem rl data.Assistance_fw_configuration_t.ignored_files)
  )
  ;;



end;;






module Assistance_fw_constant=struct

(*

#use"Filewatching/fw_constant.ml";;

*)

let clone_download_location = (Sys.getenv "HOME")^"/Downloads/Clone";;


end;;






module Assistance_dircopy_diff=struct

(*

#use"dircopy_diff.ml";;

*)

module Private=struct

let summarize_rootless_path rl=
   if List.mem (Assistance_dfn_rootless.to_ending rl) Assistance_dfa_ending.endings_for_compilable_files
   then String.capitalize_ascii(Assistance_cull_string.after_rightmost 
   (Assistance_cull_string.before_rightmost_possibly_all (Assistance_dfn_rootless.to_line rl) '.') '/')
   else Assistance_dfn_rootless.to_line rl;;
 
let summarize_rootless_path_list  l=
    let temp1=Assistance_image.image summarize_rootless_path l in
    Assistance_ordered.sort Assistance_total_ordering.silex_for_strings temp1;;

    

let salt = "Dircopy_"^"diff_t.";;

let recently_deleted_label = salt ^ "recently_deleted";;
let recently_changed_label = salt ^ "recently_changed";;
let recently_created_label = salt ^ "recently_created";;

let of_concrete_object ccrt_obj = 
   let g=Assistance_concrete_object_automatic.get_record ccrt_obj in
   {
      Assistance_dircopy_diff_t.recently_deleted = Assistance_dfn_rootless.list_of_concrete_object (g recently_deleted_label);
      recently_changed = Assistance_dfn_rootless.list_of_concrete_object (g recently_changed_label);
      recently_created = Assistance_dfn_rootless.list_of_concrete_object (g recently_created_label);
   };; 

let to_concrete_object dirdiff=
   let items= 
   [
    recently_deleted_label, Assistance_dfn_rootless.list_to_concrete_object dirdiff.Assistance_dircopy_diff_t.recently_deleted;
    recently_changed_label, Assistance_dfn_rootless.list_to_concrete_object dirdiff.Assistance_dircopy_diff_t.recently_changed;
    recently_created_label, Assistance_dfn_rootless.list_to_concrete_object dirdiff.Assistance_dircopy_diff_t.recently_created;
   ]  in
   Assistance_concrete_object_t.Record items;;

let is_empty x=
  (x.Assistance_dircopy_diff_t.recently_deleted,x.Assistance_dircopy_diff_t.recently_created,x.Assistance_dircopy_diff_t.recently_changed)=
   ([],[],[]);; 

let to_string x=
   if is_empty x then "{}" else 
   let tempf=(fun msg l->
   "\n"::msg::(Assistance_image.image(fun w->"\t\t"^(Assistance_dfn_rootless.to_line w)) l)
   ) in
   let temp1=tempf "Deleted : " (x.Assistance_dircopy_diff_t.recently_deleted)
   and temp2=tempf "Created : " (x.Assistance_dircopy_diff_t.recently_created)
   and temp3=tempf "Changed : " (x.Assistance_dircopy_diff_t.recently_changed) in
   String.concat "\n" (temp1@temp2@temp3) ;;

end;;



let add_changes diff l= 
  {
      diff with 
      Assistance_dircopy_diff_t.recently_changed = (diff.Assistance_dircopy_diff_t.recently_changed)@ l;
   };; 

let constructor a b c={
   Assistance_dircopy_diff_t.recently_deleted =a;
   Assistance_dircopy_diff_t.recently_changed =b;
   Assistance_dircopy_diff_t.recently_created =c;
};;


let create diff created_ones= 
  {
      diff with 
      Assistance_dircopy_diff_t.recently_created = (diff.Assistance_dircopy_diff_t.recently_created)@ created_ones;
   };; 

let destroy diff destroyed_ones= 
  {
      diff with 
      Assistance_dircopy_diff_t.recently_deleted = (diff.Assistance_dircopy_diff_t.recently_deleted)@ destroyed_ones;
   };; 


let empty_one  = 
   {
      Assistance_dircopy_diff_t.recently_deleted = [];
      recently_changed = [];
      recently_created = [];
   };; 


let explain  x=
   let tempf=(fun (msg,l)->
     if l=[]
     then None
     else Some(msg^" "^(String.concat "," l)^".")
   ) in
   let temp1=Assistance_option.filter_and_unpack tempf
   (* we use infinitives for github format *)
   [
     "Delete",Private.summarize_rootless_path_list  (x.Assistance_dircopy_diff_t.recently_deleted);
     "Create",Private.summarize_rootless_path_list  (x.Assistance_dircopy_diff_t.recently_created);
     "Modify",Private.summarize_rootless_path_list  (x.Assistance_dircopy_diff_t.recently_changed);
   ] in
   if temp1=[] then "" else
   let temp2=(String.uncapitalize_ascii (List.hd temp1))::(List.tl temp1) in
   String.concat " " temp2;; 
   


let is_empty = Private.is_empty ;;

let of_concrete_object = Private.of_concrete_object ;;

let print_out (fmt:Format.formatter) x=
   Format.fprintf fmt "@[%s@]" (Private.to_string x);;     


let recently_deleted x=x.Assistance_dircopy_diff_t.recently_deleted;;
let recently_created x=x.Assistance_dircopy_diff_t.recently_created;;
let recently_changed x=x.Assistance_dircopy_diff_t.recently_changed;;



let replace diff replacements= 
   let l_deleted = Assistance_image.image fst replacements 
   and l_created = Assistance_image.image snd replacements  in
  {
      diff with 
      Assistance_dircopy_diff_t.recently_created = (diff.Assistance_dircopy_diff_t.recently_created)@ l_created;
      Assistance_dircopy_diff_t.recently_deleted = (diff.Assistance_dircopy_diff_t.recently_deleted)@ l_deleted;
   };; 


let to_concrete_object = Private.to_concrete_object ;;   
   
   
   
              

end;;






module Assistance_directory_name_t=struct

(*

Directories name, with the trailing slash removed.

#use"directory_name_t.ml";;

*)

type t=D of string;;

           

end;;






module Assistance_directory_name=struct

(*

Directories name, with the trailing slash removed.

#use"directory_name.ml";;

*)



exception Non_directory of string;;
exception File_not_found of string * (Assistance_directory_name_t.t list);;

let find_file_with_directory_list fname l=
  match Assistance_option.find_and_stop (
     fun (Assistance_directory_name_t.D s_dir) ->
      let full_path = s_dir^"/"^fname in 
      if Sys.file_exists full_path 
      then Some(Assistance_absolute_path.of_string full_path)
      else None
  ) l with 
  None -> raise(File_not_found(fname,l))
  |Some(ap) -> ap;;


let of_string s=
  let temp1=Assistance_tools_for_absolute_path.of_string s in
  if Sys.is_directory temp1
  then Assistance_directory_name_t.D(Assistance_tools_for_absolute_path.remove_trailing_slash temp1)
  else raise(Non_directory(s));;

let connectable_to_subpath (Assistance_directory_name_t.D s)=s^"/";;



end;;






module Assistance_dfn_full=struct

(*

#use"Decomposed_filename/dfn_full.ml";;

*)


let to_subdirectory  (Assistance_dfn_full_t.J(r,s,m,e))=s;;
let to_module  (Assistance_dfn_full_t.J(r,s,m,e))=m;;
let to_ending (Assistance_dfn_full_t.J(r,s,m,e))=e;;

let to_rootless (Assistance_dfn_full_t.J(r,s,m,e))=(Assistance_dfn_rootless_t.J(s,m,e));; 
let to_endingless (Assistance_dfn_full_t.J(r,s,m,e))=(Assistance_dfn_endingless_t.J(r,s,m));; 

   
let to_rootless_line (Assistance_dfn_full_t.J(r,s,m,e))=
    (Assistance_dfa_subdirectory.connectable_to_subpath s)^
   (Assistance_dfa_module.to_line m)^(Assistance_dfa_ending.connectable_to_modulename e);;

let to_line (Assistance_dfn_full_t.J(r,s,m,e))=
   (Assistance_dfa_root.connectable_to_subpath r)^
   (Assistance_dfa_subdirectory.connectable_to_subpath s)^
   (Assistance_dfa_module.to_line m)^(Assistance_dfa_ending.connectable_to_modulename e);;

let to_absolute_path mlx=Assistance_absolute_path.of_string(to_line mlx);;  

let from_absolute_path_with_root ap dir=
  let rless = Assistance_dfn_common.decompose_absolute_path_using_root ap dir in 
  Assistance_dfn_join.root_to_rootless dir rless;;

  
let relocate (Assistance_dfn_full_t.J(r,old_subdir,m,e)) new_subdir=
  (Assistance_dfn_full_t.J(r,new_subdir,m,e));;  
 




end;;






module Assistance_chronometer=struct

(*

#use"chronometer.ml";;

*) 

let rewrite_days=function
0->""
|1->"1 day,"
|x->string_of_int(x)^" days,";;

let rewrite_hours=function
0->""
|1->"1 hour,"
|x->string_of_int(x)^" hours,";;

let rewrite_minutes=function
0->""
|1->"1 minute,"
|x->string_of_int(x)^" minutes,";;

let rewrite_seconds=function
0->""
|1->"1 second."
|x->string_of_int(x)^" seconds.";;

let rewrite_float x=
   let i=int_of_float(x) in
   let v_sec=(i mod 60) and q_sec=(i/60) in
   let v_min=(q_sec mod 60) and q_min=(q_sec/60) in
   let v_hour=(q_min mod 24) and q_hour=(q_min/24) in
   let s_day=rewrite_days(q_hour)
   and s_hour=rewrite_hours(v_hour)
   and s_min=rewrite_minutes(v_min)
   and s_sec=rewrite_seconds(v_sec) in
   s_day^s_hour^s_min^s_sec;;
  
let rewrite_duration x=
   if x=0. 
   then "Computation was quick.\n"
   else "Computation lasted "^(rewrite_float x)^"\n";;

 let timer=ref(0.000);;  
 
 let duration_of_computation f x=
   let t0=Unix.time() in
   let _=f(x) in
   let _=(timer:=Unix.time()-.t0) in
   (print_string(rewrite_duration (!timer));flush stdout);;
 
 let duration_of_last_computation ()=
  (print_string(rewrite_duration (!timer));flush stdout);;
   
   
 let  it f x=
  let t0=Unix.time() in
   let y=f(x) in
   let _=(timer:=Unix.time()-.t0) in
   let _=(print_string(rewrite_duration (!timer));flush stdout) in
   y;;
 
   
           

end;;






module Assistance_explicit=struct

(*

#use"explicit.ml";;

*)

module Private = struct 

let iter0 (f:'a->unit) l addenda=
  let n=List.length(l)
  and accu=ref(l) in
  let s0=" of "^string_of_int(n)^" "^addenda^"\n" in
  for j=1 to n
               do
               ( f(List.hd(!accu));
                 accu:=List.tl(!accu);
                 print_string(string_of_int(j)^s0);
                 flush stdout)
               done;;

let iter2 (f:'a->'a) initial_value tester (shower:'a->string)  addenda=
  let accu=ref(initial_value) in
  let _=(while tester(!accu)
               do
               ( 
                 accu:=f(!accu);
                 print_string((shower (!accu))^addenda);
                 flush stdout;
               )
               done) in
  !accu;;

let e_rev l=
   let accu=ref([]) in 
   let f=(fun x->accu:=x::(!accu)) in
   let _=iter0(f)(l)(" (rev part)") in
   !accu;;    

let unchronometered_explore_tree f l=
    let g=(fun (graet,p,q,da_ober)->
       match da_ober with
       []->(graet,0,0,[])
       |a::peurrest->
         let temp1=f a in
         if temp1=[]
         then (a::graet,p-1,q+1,peurrest)
         else (a::graet,p-1+List.length(temp1),q+1,temp1@peurrest)
    ) and 
    tester=(fun (graet,p,q,da_ober)->da_ober<>[]) 
    and
    shower=(
     fun (graet,p,q,da_ober)->
       (string_of_int p)^" to be explored ; "^
       (string_of_int q)^" already explored\n"
    )
    in
    let initial_value=([],List.length l,0,l) in
    let _=(print_string(shower(initial_value));flush stdout) in
    let (ans,_,_,_)=iter2 g initial_value tester shower  "" in
    ans;;



let unchronometered_filter f l=
   let accu=ref([]) in 
   let g=(fun x->if f(x) then accu:=x::(!accu) else ()) in
   let _=iter0(g)(l)(" (filter part)") in
   e_rev(!accu);;    
 
 
 let unchronometered_image f l=
   let accu=ref([]) in 
   let g=(fun x->accu:=f(x)::(!accu)) in
   let _=iter0(g)(l)(" (rev_image part)") in
   e_rev(!accu);;  
   

let unchronometered_image_computed_backwards f l=
   let temp1=e_rev(l) in
    let accu=ref([]) in 
   let g=(fun x->accu:=f(x)::(!accu)) in
   let _=iter0(g)(temp1)(" (image part)") in
   (!accu);;     
 
  

exception Force_find_exn ;;

let rec helper_for_opt_finding (f,sn) (j,x)=
   match x with 
   [] -> None
   |a::others -> if f a 
                 then Some a
                 else let _=(
                        print_string("Item number "^string_of_int(j)^" of "^sn^" found wanting \n");
                        flush stdout) in 
                      helper_for_opt_finding (f,sn) (j+1,others) ;; 

end ;; 

let explore_tree f l=Assistance_chronometer.it (Private.unchronometered_explore_tree f) l;; 
           
let filter f l=Assistance_chronometer.it (Private.unchronometered_filter f) l;; 


let image f l=Assistance_chronometer.it (Private.unchronometered_image f) l;;  

let image_computed_backwards f l=Assistance_chronometer.it 
   	(Private.unchronometered_image_computed_backwards f) l;;               

let opt_find f x = Private.helper_for_opt_finding (f,string_of_int(List.length x)) (1,x) ;;

(* opt_find (fun t->t>4) (Ennig.ennig 1 7);; *)


end;;






module Assistance_functor_for_sets=struct

(*
 
#use"Ordered_Lists/functor_for_sets.ml";;

Here all the possible dependencies are defined. Each particular
instance defines only the value it needs.

*)


type ('a,'b) parameter = (('a list) -> 'b) * ('b -> ('a list)) * ('a Assistance_total_ordering.t);;

let does_not_intersect ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= Assistance_ordered.does_not_intersect cmpr (deco ox) (deco oy);;
    
let empty_set ((co,deco,cmpr):('a,'b) parameter) = co [];;

let fold_merge ((co,deco,cmpr):('a,'b) parameter) 
     l=co (Assistance_ordered.fold_merge cmpr (Assistance_image.image deco l));;

let fold_intersect ((co,deco,cmpr):('a,'b) parameter) 
     l=co (Assistance_ordered.fold_intersect cmpr (Assistance_image.image deco l));;
    
let forget_order ((co,deco,cmpr):('a,'b) parameter) =deco;;

let hd ((co,deco,cmpr):('a,'b) parameter) ox= List.hd(deco ox);;

let image ((co,deco,cmpr):('a,'b) parameter) f ox= Assistance_image.image f (deco ox);;

let insert ((co,deco,cmpr):('a,'b) parameter) 
     x oy= co(Assistance_ordered.insert cmpr x (deco oy));;

let intersect ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= co(Assistance_ordered.intersect cmpr (deco ox) (deco oy));;

let intersects ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= Assistance_ordered.intersects cmpr (deco ox) (deco oy);;

let is_included_in ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= Assistance_ordered.is_included_in cmpr (deco ox) (deco oy);;

let length ((co,deco,cmpr):('a,'b) parameter) ox= List.length(deco ox);;

let max ((co,deco,cmpr):('a,'b) parameter) ox= List.hd(List.rev (deco ox));;

let mem ((co,deco,cmpr):('a,'b) parameter) 
     x oy= Assistance_ordered.mem cmpr x (deco oy);;

let merge ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= co(Assistance_ordered.merge cmpr (deco ox) (deco oy));;

let min ((co,deco,cmpr):('a,'b) parameter) ox= List.hd(deco ox);;

let nmem ((co,deco,cmpr):('a,'b) parameter) 
     x oy= not(Assistance_ordered.mem cmpr x (deco oy));;

let outsert ((co,deco,cmpr):('a,'b) parameter) 
     x oy= co(Assistance_ordered.outsert cmpr x (deco oy));;

let safe_set ((co,deco,cmpr):('a,'b) parameter) 
     l= co(Assistance_ordered.safe_set cmpr l);;

let setminus ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= co(Assistance_ordered.setminus cmpr (deco ox) (deco oy));;

let singleton ((co,deco,cmpr):('a,'b) parameter)  x=co[x];;

let size_of_intersection ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= List.length(Assistance_ordered.intersect cmpr (deco ox) (deco oy));;

let sort ((co,deco,cmpr):('a,'b) parameter) 
     l= co(Assistance_ordered.sort cmpr l);;

let tl ((co,deco,cmpr):('a,'b) parameter) ox= co(List.tl(deco ox));;

let unsafe_set ((co,deco,cmpr):('a,'b) parameter) 
     l= co l;;



end;;






module Assistance_set_of_polys_t=struct

(* 

#use"Ordered_Lists/set_of_polys_t.ml";;

*)

type 'a t=S of 'a list;;


end;;






module Assistance_set_of_polys=struct

(* 

#use"Ordered_Lists/set_of_polys.ml";;

*)

let tr = ((fun x->Assistance_set_of_polys_t.S(x)),(fun (Assistance_set_of_polys_t.S(x))->x),Assistance_total_ordering.standard);;

let does_not_intersect x y= Assistance_functor_for_sets.does_not_intersect tr x y;;
let empty_set = Assistance_functor_for_sets.empty_set tr;;
let fold_merge l= Assistance_functor_for_sets.fold_merge tr l;;
let forget_order x= Assistance_functor_for_sets.forget_order tr x;;
let hd x = Assistance_functor_for_sets.hd tr x;;
let image f x= Assistance_functor_for_sets.image tr f x;;
let insert a x= Assistance_functor_for_sets.insert tr a x;;
let is_included_in x y= Assistance_functor_for_sets.is_included_in tr x y;;
let length x= Assistance_functor_for_sets.length tr x;;
let mem a x= Assistance_functor_for_sets.mem tr a x;;
let merge l= Assistance_functor_for_sets.merge tr l;;
let nmem a x= Assistance_functor_for_sets.nmem tr a x;;
let outsert a x= Assistance_functor_for_sets.outsert tr a x;;
let safe_set l= Assistance_functor_for_sets.safe_set tr l;;
let setminus x y= Assistance_functor_for_sets.setminus tr x y;;
let singleton a= Assistance_functor_for_sets.singleton tr a;;
let sort l= Assistance_functor_for_sets.sort tr l;;
let unsafe_set l= Assistance_functor_for_sets.unsafe_set tr l;;




end;;






module Assistance_stabilize=struct

(*

#use"stabilize.ml";;

*) 

let one_more_time f (an_holl,da_ober)=
 let l_da_ober=Assistance_set_of_polys.forget_order(da_ober) in
 let temp1=List.flatten(List.rev_map(f)(l_da_ober)) in
 let temp2=Assistance_set_of_polys.sort(temp1) in
 let re_nevez=Assistance_set_of_polys.setminus(temp2)(an_holl) in
 let hollad_nevez=Assistance_set_of_polys.merge(an_holl)(re_nevez) in
 (hollad_nevez,re_nevez);; 
  
let rec morzholan f (an_holl,da_ober)=
  if da_ober=Assistance_set_of_polys.empty_set
  then an_holl
  else morzholan f (one_more_time f (an_holl,da_ober));;
  
let father f l=let tl=Assistance_set_of_polys.sort(l) in morzholan f (tl,tl);;

let one_more_time2 f (an_holl,graet,da_ober)=
 let l_graet=Assistance_set_of_polys.forget_order(graet) 
 and l_da_ober=Assistance_set_of_polys.forget_order(da_ober) in
 let zz1=Assistance_cartesian.product(l_graet)(l_da_ober)
 and zz2=Assistance_cartesian.product(l_da_ober)(l_graet) 
 and zz3=Assistance_cartesian.product(l_da_ober)(l_da_ober) in
 let zz=List.flatten [zz1;zz2;zz3] in
 let temp1=List.flatten(List.rev_map (function (x,y)->[f x y]) zz ) in
 let temp2=Assistance_set_of_polys.sort(temp1) in
 let re_nevez=Assistance_set_of_polys.setminus(temp2)(an_holl) in
 let hollad_nevez=Assistance_set_of_polys.merge(an_holl)(re_nevez) in
 (hollad_nevez,an_holl,re_nevez);; 
  
let rec morzholan2 f (an_holl,graet,da_ober)=
  if da_ober=Assistance_set_of_polys.empty_set
  then an_holl
  else morzholan2 f (one_more_time2 f (an_holl,graet,da_ober));; 
  
let binary_operation f l=let tl=Assistance_set_of_polys.sort(l) in morzholan2 f (tl,Assistance_set_of_polys.empty_set,tl);;  

let explore_tree f l0=
 let modified_l0=List.rev_map(function x->(x,0))(l0) in
 let modified_f=(function (x,j)->
   List.rev_map(function y->(y,j+1))(f x)
 ) in
 let rec tempf=(function (j,graet,da_ober)->
 match da_ober with
    []->graet
    |(xa,ia)::peurrest->let temp1=modified_f(xa,ia) in
                  let temp2=(j+1,xa::graet,List.rev_append temp1 peurrest) in
                  tempf(temp2)
 ) in
 tempf(0,[],modified_l0);;
 
 let explore_tree_explicitly f l0=
 let modified_l0=List.rev_map(function x->(x,0))(l0) in
 let modified_f=(function (x,j)->
   List.rev_map(function y->(y,j+1))(f x)
 ) in
 let rec tempf=(function (j,graet,da_ober)->
 match da_ober with
    []->graet
    |(xa,ia)::peurrest->let temp1=modified_f(xa,ia) in
                  let temp2=(j+1,xa::graet,List.rev_append temp1 peurrest) in
                  let _=(print_string("("^string_of_int(ia)^","^string_of_int(j+1)^")\n");
                  flush stdout) in
                  tempf(temp2)
 ) in
 tempf(0,[],modified_l0);;
 
type 'a hierarchize_data=
   Normal of  'a Assistance_set_of_polys_t.t list * 'a Assistance_set_of_polys_t.t * ('a * 'a Assistance_set_of_polys_t.t) list
  |Failed of  'a Assistance_set_of_polys_t.t list * 'a Assistance_set_of_polys_t.t * ('a * 'a Assistance_set_of_polys_t.t) list
  |Succeeded of 'a list list;;
   

let pusher_for_hierarchization (graet,hollad,da_ober)=
  if da_ober=[]
  then Succeeded(List.rev_map Assistance_set_of_polys.forget_order graet)
  else 
  let temp1=Assistance_image.image (fun (x,y)->(x,Assistance_set_of_polys.setminus y hollad)) da_ober in
  let (temp2,temp3)=List.partition (fun (x,z)->Assistance_set_of_polys.length z=0) temp1 in
  if temp2=[]
  then Failed(graet,hollad,da_ober)
  else
  let temp4=Assistance_image.image fst temp2 in
  let o_temp4=Assistance_set_of_polys.sort(temp4) in
  let temp5=Assistance_image.image (fun (x,z)->(x,Assistance_set_of_polys.setminus z o_temp4)) temp3 in
  (Normal(o_temp4::graet,Assistance_set_of_polys.merge hollad o_temp4,temp5));;
  
type 'a list_of_ancestors_map=('a -> 'a list);;  
  
let try_hierarchizing (f: 'a list_of_ancestors_map) l=
  let temp1=Assistance_image.image (fun t->(t,Assistance_set_of_polys.sort(f t))) l in
  let rec tempf=(fun x->
    let y=pusher_for_hierarchization x in
    match y  with
    Normal(a,b,c)->tempf(a,b,c)
    |_->y) in
  tempf ([],Assistance_set_of_polys.empty_set,temp1);;
  
let hierarchize f l=
  match try_hierarchizing f l with
   Succeeded(l)->l
  |_->failwith("Direct hierarchizing fails, there is a cycle. Try hierarchize instead");;
  
  

           

end;;






module Assistance_more_unix=struct

(*

#use"more_unix.ml";;

*)


module Private=struct
 
let naive_extension ap=
   let s=Assistance_absolute_path.to_string ap in
   let i=String.rindex s '.' in
   (Assistance_cull_string.cobeginning (i+1) s);; 
   
let extension x=try (naive_extension x) with 
  any_exception->"";;
  
 let is_a_directory ap=
   let s=Assistance_absolute_path.to_string ap in
   try (function x->true)(Sys.readdir s) with any_exception->false;;
 
 let father ap=
   let s=Assistance_absolute_path.to_string ap in
   let i=String.rindex s '/' in
   if i=0 then Assistance_directory_name.of_string"/" else
   Assistance_directory_name.of_string (Assistance_cull_string.beginning i s);; 
   
 let son dir=
   let s=Assistance_directory_name.connectable_to_subpath dir in
   let i=String.rindex s '/' in
   if i=0 then "" else
   (Assistance_cull_string.cobeginning (i+1) s);; 
  
 let is_a_nondirectory_or_a_nib x=
  if is_a_directory(x)
  then extension(x)="nib"
  else not(Assistance_substring.is_a_substring_of(".nib/")(Assistance_absolute_path.to_string x));;
  
 let naive_ls dir=
   let s=Assistance_directory_name.connectable_to_subpath dir in
   let s_with_slash=(function ()->
    if String.get(s)(String.length(s)-1)='/'
    then s
    else s^"/"
   )() in
   let temp1=Array.to_list(Sys.readdir(s)) in
   let tempf=(function w->try (Some(Assistance_absolute_path.of_string(s_with_slash^w))) with
   any_exception->None) in
   Assistance_option.filter_and_unpack tempf temp1;;
   
 let ls x=try (naive_ls x) with any_exception->[];;  
 
 let test_for_cleaniness=function ap->
  let s=Assistance_absolute_path.to_string ap in
  Assistance_cull_string.after_rightmost(s)('/')<>".DS_Store";;
 
 let cleaned_ls x=
   List.filter test_for_cleaniness (ls x);;
   
let select_by_prefix subdir forbidden_subdirs =
  Assistance_option.filter_and_unpack (
     fun forb_subdir -> 
        if Assistance_supstring.begins_with forb_subdir subdir 
        then Some(Assistance_cull_string.two_sided_cutting (subdir,"") forb_subdir)
        else None
  ) forbidden_subdirs ;; 

let ls_with_ignored_subdirs (dir,forbidden_subdirs)=
   let temp1 = Array.to_list (Sys.readdir dir) in
   let temp2 = Assistance_option.filter_and_unpack (
      fun fname -> if List.for_all (
          fun forb_subdir -> 
           not(Assistance_supstring.begins_with fname forb_subdir)
        )  forbidden_subdirs
           then Some(dir^fname)
           else None
   ) temp1 in 
   let is_a_dir  = (fun s->is_a_directory(Assistance_absolute_path.AP(s))) in 
   let (found_dirs,found_nondirs) = List.partition is_a_dir temp2 in 
   let new_constraints = Assistance_image.image (
     fun full_subdir_path ->
        let subdir = Assistance_cull_string.two_sided_cutting (dir,"") full_subdir_path in 
       (full_subdir_path^"/",select_by_prefix subdir forbidden_subdirs)
   ) found_dirs in 
   (found_nondirs,found_dirs,new_constraints);;

let rec helper_for_complete_ls_with_ignored_subdirs 
  (treated_nondirs,treated_dirs,to_be_treated) = match to_be_treated with 
  [] -> (Assistance_image.image Assistance_absolute_path.of_string treated_nondirs,treated_dirs)
  |(dir,forbidden_subdirs) :: others -> 
    let (found_nondirs,found_dirs,new_constraints) = 
        ls_with_ignored_subdirs (dir,forbidden_subdirs) in 
    let new_treated_nondirs = List.rev_append found_nondirs treated_nondirs 
    and new_treated_dirs =  List.rev_append found_dirs treated_dirs 
    and new_to_be_treated = List.rev_append new_constraints others in 
    let n = string_of_int(List.length new_to_be_treated) in 
    let msg = " "^n^" to go ...\n" in 
    let _= (print_string msg;flush stdout) in 
    helper_for_complete_ls_with_ignored_subdirs 
    (new_treated_nondirs,new_treated_dirs,new_to_be_treated) ;;

let complete_ls_with_ignored_subdirs dir forbidden_subdirs = 
   let s_dir = Assistance_directory_name.connectable_to_subpath dir in 
   let (treated_nondirs,treated_dirs) = 
   helper_for_complete_ls_with_ignored_subdirs 
  ([],[],[s_dir,
         Assistance_image.image Assistance_dfa_subdirectory.without_trailing_slash forbidden_subdirs]) in 
   (treated_nondirs,Assistance_image.image 
     (fun x->Assistance_dfa_subdirectory.of_line(Assistance_cull_string.two_sided_cutting (s_dir,"") x)) 
   treated_dirs);;

let ls_with_directories_only dir=
   let temp1 = cleaned_ls dir in 
   Assistance_option.filter_and_unpack (
     fun ap -> 
       if is_a_directory ap 
       then let s_ap = Assistance_absolute_path.to_string ap in 
            Some(Assistance_directory_name.of_string s_ap)
       else None
   )  temp1 ;;

 let dirty_ones_in_ls x=
   List.filter (function u->not(test_for_cleaniness u) )(ls x);; 
 
 let adhoc_ls ap=
   let s=Assistance_absolute_path.to_string ap in
   if not(is_a_directory ap) 
   then []
   else 
   let dir=Assistance_directory_name.of_string s in
   ls dir;;
 

 
let complete_ls dir=
   let s_dir=Assistance_directory_name.connectable_to_subpath dir in
   let x=Assistance_absolute_path.of_string s_dir in
   Assistance_explicit.explore_tree adhoc_ls [x];;   

let adhoc_ls_with_ignored_subdirs ap=
   let s=Assistance_absolute_path.to_string ap in
   if not(is_a_directory ap) 
   then []
   else 
   let dir=Assistance_directory_name.of_string s in
   ls dir;;

let complete_ls_with_directories_only x=
  Assistance_explicit.explore_tree ls_with_directories_only [x];;
  

 let complete_ls_with_nondirectories_only x=
  List.filter(is_a_nondirectory_or_a_nib)(complete_ls x);;
  
  
 let beheaded_ls_with_nondirectories_only x=
  let n0=String.length(Assistance_absolute_path.to_string x) in
  let temp1=List.filter(is_a_nondirectory_or_a_nib)(adhoc_ls x) in
  let temp2=Assistance_image.image (fun ap->Assistance_cull_string.cobeginning n0 (Assistance_absolute_path.to_string ap)) temp1 in
  temp2;; 
 
 let dir_substructure x=
    let n0=String.length(Assistance_absolute_path.to_string x) in
    let temp1=(Assistance_stabilize.explore_tree adhoc_ls (adhoc_ls x)) in
    let temp2=List.filter(function x->extension(x)<>"nib")(temp1) in
    List.rev_map(function ap->Assistance_cull_string.cobeginning n0 (Assistance_absolute_path.to_string ap))(temp2);;
  
 let endfiles x=
    let n0=String.length(Assistance_absolute_path.to_string x)+1(*because of the slash!*) in
    let temp1=(Assistance_stabilize.explore_tree adhoc_ls (adhoc_ls x)) in
    let temp2=List.filter(is_a_nondirectory_or_a_nib)(temp1) in
    List.rev_map(function ap->Assistance_cull_string.cobeginning n0 (Assistance_absolute_path.to_string ap))(temp2);;
    
let quick_complete_ls s=
  let x=Assistance_directory_name.of_string s in
  let temp1=complete_ls x in
  Assistance_image.image Assistance_absolute_path.to_string temp1;;  
  
 

let quick_beheaded_complete_ls s=
  let x=Assistance_directory_name.of_string s in
  let n=String.length(Assistance_directory_name.connectable_to_subpath x) in
  let temp1=complete_ls x in
  Assistance_image.image (fun ap->Assistance_cull_string.cobeginning n (Assistance_absolute_path.to_string ap)) temp1;; 
  
let beheaded_simple_ls dir=
  let n=String.length(Assistance_directory_name.connectable_to_subpath dir) in
  let temp1=ls dir in
  Assistance_image.image (fun ap->
   Assistance_cull_string.cobeginning n (Assistance_absolute_path.to_string ap)) temp1;; 


let clear_directory_contents root =
    let s_root = Assistance_dfa_root.connectable_to_subpath root in 
    let cmd = "rm -rf "^s_root^"*" in 
    Sys.command cmd;;


let create_subdirs_and_fill_files_if_necessary root subdirs files_with_content =
   let s_root = Assistance_dfa_root.connectable_to_subpath root in 
   let cmds1=Assistance_image.image (
      fun subdir -> 
         "mkdir -p "^s_root^(Assistance_dfa_subdirectory.without_trailing_slash subdir)
   ) subdirs in 
   let _=Assistance_image.image Sys.command cmds1 in 
   let temp1=Assistance_option.filter_and_unpack (
     fun (rootless,content)->
        let full_path = Assistance_dfn_full.to_line(Assistance_dfn_join.root_to_rootless root rootless) in 
        if Sys.file_exists full_path 
        then None 
        else Some(full_path,content)
   ) files_with_content in 
   Assistance_image.image (
      fun (full_path,content) ->
         let _=Sys.command("touch "^full_path) in 
         Assistance_io.overwrite_with (Assistance_absolute_path.of_string full_path) content
   )  temp1;;


let create_subdirs_and_fill_files root subdirs files_with_content =
   let s_root = Assistance_dfa_root.connectable_to_subpath root in 
   let cmds1=Assistance_image.image (
      fun subdir -> 
         "mkdir -p "^s_root^(Assistance_dfa_subdirectory.without_trailing_slash subdir)
   ) subdirs in 
   let _=Assistance_image.image Sys.command cmds1 in 
   Assistance_image.image (
     fun (rootless,content)->
        let full_path = Assistance_dfn_full.to_line(Assistance_dfn_join.root_to_rootless root rootless) in 
         let _=Sys.command("touch "^full_path) in 
         Assistance_io.overwrite_with (Assistance_absolute_path.of_string full_path) content
   ) files_with_content;;

end;;    


let all_files_with_endings dir l_endings=
   let temp1=Private.complete_ls dir in
   let temp2=List.filter(
   fun ap->
     let s_ap=Assistance_absolute_path.to_string ap in
     List.exists( fun ending->
       Assistance_supstring.ends_with s_ap ending)
     l_endings  
   ) temp1 in
   temp2;;  
let beheaded_simple_ls=Private.beheaded_simple_ls;;
let complete_ls=Private.complete_ls;;
let complete_ls_with_directories_only=Private.complete_ls_with_directories_only;;
let complete_ls_with_ignored_subdirs=Private.complete_ls_with_ignored_subdirs;;
let complete_ls_with_nondirectories_only=Private.complete_ls_with_nondirectories_only;;
let clear_directory_contents = Private.clear_directory_contents;;
let create_subdirs_and_fill_files = Private.create_subdirs_and_fill_files;;
let create_subdirs_and_fill_files_if_necessary = Private.create_subdirs_and_fill_files_if_necessary;;
let is_a_directory=Private.is_a_directory;;   
let quick_beheaded_complete_ls=Private.quick_beheaded_complete_ls;;           
let simple_ls=Private.ls;;

end;;






module Assistance_prepare_dircopy_update=struct

(*

#use"prepare_dircopy_update.ml";;

*)

let compute_deleted_in_diff sourcedir destdir=
   let s_sourcedir=Assistance_dfa_root.connectable_to_subpath sourcedir
   and s_destdir=Assistance_dfa_root.connectable_to_subpath destdir in
   let temp1=Assistance_more_unix.quick_beheaded_complete_ls s_destdir in
   Assistance_option.filter_and_unpack(
       fun s->if (s<>"README")
              &&(not(Assistance_supstring.begins_with s ".git/")) 
              &&(not(Sys.file_exists(s_sourcedir^s)))
              then (if Sys.is_directory(s_destdir^s) 
                   then None
                   else Some(Assistance_dfn_rootless.of_line s))
              else None
   ) temp1;;
   
let compute_nondeleted_in_diff (sourcedir,l) destdir=
   let s_sourcedir=Assistance_dfa_root.connectable_to_subpath sourcedir
   and s_destdir=Assistance_dfa_root.connectable_to_subpath destdir in
   let created_accu=ref[]
   and changed_accu=ref[] in
   let _=Assistance_image.image(
   	  fun s->
   	    if (not(Sys.file_exists(s_destdir^s)))
   	    then created_accu:=s::(!created_accu)
   	    else 
   	    (
   	    let txt1=Assistance_io.read_whole_file
   	    (Assistance_absolute_path.of_string(s_sourcedir^s))
   	    and txt2=Assistance_io.read_whole_file
   	    (Assistance_absolute_path.of_string(s_destdir^s)) in
   	    if txt1<>txt2
   	    then changed_accu:=s::(!changed_accu)
   	    )
   ) l in
   (Assistance_image.image Assistance_dfn_rootless.of_line (!created_accu),
    Assistance_image.image Assistance_dfn_rootless.of_line (!changed_accu));;   
   
  
let compute_diff (sourcedir,l) destdir=
   let (created,changed)=compute_nondeleted_in_diff (sourcedir,l) destdir in
   Assistance_dircopy_diff.constructor
   
   	(compute_deleted_in_diff sourcedir destdir)
   	changed
   	created
   ;;
   
   
let greedy_list sourcedir=
   let converted_to_dir=Assistance_directory_name.of_string
      (Assistance_dfa_root.without_trailing_slash sourcedir) in
   let source_paths=Assistance_more_unix.complete_ls_with_nondirectories_only converted_to_dir in
   Assistance_image.image (fun ap->
     let rootless_path = Assistance_dfn_common.decompose_absolute_path_using_root ap sourcedir in 
     Assistance_dfn_rootless.to_line rootless_path ) 
   source_paths;;
      
let compute_greedy_diff sourcedir destdir=
   compute_diff (sourcedir,greedy_list sourcedir) destdir;;      
   
let restricted_list sourcedir (ignored_subdirs,ignored_files)=
   let s_dir =  Assistance_dfa_root.without_trailing_slash sourcedir in 
   let converted_to_dir=Assistance_directory_name.of_string
      (Assistance_dfa_root.without_trailing_slash sourcedir) in
   let absolute_paths1=Assistance_more_unix.complete_ls_with_nondirectories_only converted_to_dir in 
   let ignored_subdirs1=Assistance_image.image Assistance_dfa_subdirectory.without_trailing_slash ignored_subdirs in 
   Assistance_option.filter_and_unpack (fun ap->
     let s_ap = Assistance_absolute_path.to_string ap in 
     let s_rootless = Assistance_cull_string.cobeginning (String.length(s_dir)+1) s_ap in 
     if (List.exists(Assistance_supstring.begins_with s_rootless) ignored_subdirs1) ||
        (List.mem s_rootless ignored_files)
     then None
     else 
     let rootless_path = Assistance_dfn_common.decompose_absolute_path_using_root ap sourcedir in 
     Some(Assistance_dfn_rootless.to_line rootless_path) ) 
   absolute_paths1;;
      

let compute_restricted_diff sourcedir destdir restrictions=
   compute_diff (sourcedir,restricted_list sourcedir restrictions) destdir;;

let commands_for_update (source_dir,destination_dir) diff=
   if Assistance_dircopy_diff.is_empty diff
   then []
   else 
   let s_destination=Assistance_dfa_root.connectable_to_subpath destination_dir in
   let created_ones=Assistance_dircopy_diff.recently_created diff  in
   let temp2=Assistance_option.filter_and_unpack
   (fun rl->
     let fn = Assistance_dfn_rootless.to_line rl in 
     if String.contains fn '/'
     then let dn=Assistance_cull_string.before_rightmost fn '/' in
          Some("mkdir -p "^s_destination^dn)
     else None 
    )
   created_ones in
   let temp3=Assistance_ordered.sort Assistance_total_ordering.silex_for_strings temp2 in
   let s_source=Assistance_dfa_root.connectable_to_subpath source_dir in
   let temp4=Assistance_image.image(
      fun rl->
     let fn = Assistance_dfn_rootless.to_line rl in 
      "cp "^s_source^fn^" "^s_destination^(Assistance_cull_string.before_rightmost fn '/')
   ) created_ones in
   let changed_ones=Assistance_dircopy_diff.recently_changed diff in
   let temp5=Assistance_image.image(
      fun rl->
     let fn = Assistance_dfn_rootless.to_line rl in 
      "cp "^s_source^fn^" "^s_destination^fn
   ) changed_ones in
   let temp7=Assistance_image.image(
      fun rl->
     let fn = Assistance_dfn_rootless.to_line rl in 
      "rm "^s_destination^fn
   ) (Assistance_dircopy_diff.recently_deleted diff) in
   (temp3@temp4@temp5@temp7);;  
   
              

end;;






module Assistance_compact_replacer_t=struct

(*

#use"Text_editing/compact_replacer_t.ml";;

*)

type t= CR of (string*string) list ;;


end;;






module Assistance_compact_replacer=struct

(*

#use"Text_editing/compact_replacer.ml";;

*)


let separator = " \205\140 ";;

let unparse (Assistance_compact_replacer_t.CR(l))=
   let temp1 = List.flatten (Assistance_image.image (fun (x,y)->[x;y]) l)  in 
   String.concat separator temp1 ;;

let parse descr =
   let temp1 = Str.split (Str.regexp_string separator) descr in 
   let m = (List.length temp1)/2 in
   let tg =(fun k->List.nth temp1 (k-1)) in  
   Assistance_compact_replacer_t.CR(Assistance_ennig.doyle (fun j->(tg (2*j-1),tg (2*j)) ) 1 m );;

let replace_inside_string (Assistance_compact_replacer_t.CR(l)) old_text =
   Assistance_replace_inside.replace_several_inside_string l old_text ;;

let replace_inside_file (Assistance_compact_replacer_t.CR(l)) fn =
   Assistance_replace_inside.replace_several_inside_file l fn ;;

let reverse_replace_inside_string (Assistance_compact_replacer_t.CR(old_l)) old_text =
   let l = Assistance_image.image (fun (x,y)->(y,x)) old_l in 
   Assistance_replace_inside.replace_several_inside_string l old_text ;;

let reverse_replace_inside_file (Assistance_compact_replacer_t.CR(old_l)) fn =
   let l = Assistance_image.image (fun (x,y)->(y,x)) old_l in 
   Assistance_replace_inside.replace_several_inside_file l fn ;;

let execute s=
   let temp1 = Str.split (Str.regexp "[ \t]+") s in 
   let temp2 = Assistance_image.image Assistance_absolute_path.of_string  temp1 in 
   let replacements = Assistance_io.read_whole_file (List.nth temp2 0) 
   and recipient = (List.nth temp2 1) in 
   replace_inside_file (parse replacements) recipient ;;

let reverse_execute s=
   let temp1 = Str.split (Str.regexp "[ \t]+") s in 
   let temp2 = Assistance_image.image Assistance_absolute_path.of_string  temp1 in 
   let replacements = Assistance_io.read_whole_file (List.nth temp2 0) 
   and recipient = (List.nth temp2 1) in 
   reverse_replace_inside_file (parse replacements) recipient ;;   

(*

let z1 =  Compact_replacer_t.CR(["abc","def";"12","34"]) ;;  
let z2 = unparse z1;;
let z3 = parse z2;;
let check = (z3=z1);;

*)

end;;






module Assistance_unix_command=struct

(*

Wrapper on the Sys dot command function.

#use"unix_command.ml";;

*)


exception Command_failed of string;;
exception Command_failed_just_now ;;

module Private = struct

let prefix_for_changing_directories         = "cd ";;
let prefix_for_replacing_patterns           = "rp ";;
let prefix_for_reverse_replacing_patterns   = "rvp ";;

let accu=ref([]:string list);;
let remember_commands_mode=ref(false);;
let hardcore_mode=ref(false);;

let command cmd=
   let cd_prefix =prefix_for_changing_directories 
   and rp_prefix =prefix_for_replacing_patterns 
   and rvp_prefix =prefix_for_reverse_replacing_patterns in 
   if Assistance_supstring.begins_with cmd cd_prefix 
   then let  _=Sys.chdir(Assistance_cull_string.cobeginning (String.length cd_prefix) cmd) in 0
   else 
   if Assistance_supstring.begins_with cmd rp_prefix 
   then let  _= Assistance_compact_replacer.execute(Assistance_cull_string.cobeginning (String.length rp_prefix) cmd) in 0 
   else 
   if Assistance_supstring.begins_with cmd rvp_prefix 
   then let  _= Assistance_compact_replacer.reverse_execute(Assistance_cull_string.cobeginning (String.length rvp_prefix) cmd) in 0 
   else 
   Sys.command cmd;;


let mild_uc s=
   let i=command s in
   let _=(
   if i<>0
   then (print_string ("Failed during "^s^"\n");flush stdout)
   else (if (!remember_commands_mode) 
               then accu:=s::(!accu))
   ) in
   i;;

let hardcore_uc s=
   let i=command s in
   if i<>0
   then raise(Command_failed(s))
   else let _=(if (!remember_commands_mode) 
               then accu:=s::(!accu)) in 
        i;;

let uc s=
   if (!hardcore_mode)
   then hardcore_uc s
   else mild_uc s;;

let debug_individual_uc (j,s) =
    let msg = "Trying command number "^(string_of_int j)^" : "^s^"\n" in 
    let _=(print_string msg;flush stdout) in
    let i = command s in 
    if i<>0 
    then raise(Command_failed_just_now)
    else (print_string "Successful.\n";flush stdout);;

let rec helper_for_debug_multiple_uc (j,l)=
   match l with 
   [] -> () 
   |cmd :: other_cmds ->
     let _ = debug_individual_uc (j,cmd) in 
     helper_for_debug_multiple_uc (j+1,other_cmds) ;; 


end;;

let cd dirname = (Private.prefix_for_changing_directories)^dirname;;

let rec conditional_multiple_uc commands=match commands with
  []->true
  |cmd1::other_commands ->
    if (Private.uc cmd1)=0
    then conditional_multiple_uc other_commands 
    else false;;
           
let debug_multiple_uc l = Private.helper_for_debug_multiple_uc (1,l);;           

let hardcore_uc = Private.hardcore_uc ;;

let mv full_path new_location =
   let destination_equals_source=(
     if not(Assistance_supstring.begins_with full_path new_location) then false else 
     let naked_name=Assistance_cull_string.two_sided_cutting (new_location,"") full_path in 
     not(String.contains naked_name '/') 
   ) in 
   if destination_equals_source 
   then None 
   else Some("mv "^full_path^" "^new_location);;

let prefix_for_replacing_patterns           = Private.prefix_for_replacing_patterns;;
let prefix_for_reverse_replacing_patterns   = Private.prefix_for_reverse_replacing_patterns;;


let uc = Private.uc;;



           

end;;






module Assistance_check_ocaml_dircopy=struct

(*

#use"check_ocaml_dircopy.ml";;

Usable on a github clone of the remote master version.

*)


exception Failure_in_clone_directory_creation;;
exception Failure_during_github_cloning;;

let filter_diff_according_to_admissibility data diff=
   let filter_list = List.filter (Assistance_fw_configuration.test_for_admissibility data) in 
   {
    Assistance_dircopy_diff_t.recently_deleted = filter_list diff.Assistance_dircopy_diff_t.recently_deleted;
    Assistance_dircopy_diff_t.recently_changed = filter_list diff.Assistance_dircopy_diff_t.recently_changed;
    Assistance_dircopy_diff_t.recently_created = filter_list diff.Assistance_dircopy_diff_t.recently_created;
  };;


let commands_for_confidentiality encoding_protected_files =
   Assistance_image.image (
     fun (replacer,replacee) ->
       let s_replacer = Assistance_dfn_rootless.to_line  replacer in 
       let s_full_path = Assistance_fw_constant.clone_download_location^"/"^(Assistance_dfn_rootless.to_line replacee) in 
       Assistance_unix_command.prefix_for_reverse_replacing_patterns^s_replacer^" "^s_full_path
   ) encoding_protected_files ;;


let check data =
  let name_of_clone_directory = Assistance_fw_constant.clone_download_location in 
  let i=(
    if Sys.file_exists(name_of_clone_directory)
    then Assistance_unix_command.uc("rm -rf "^name_of_clone_directory) 
    else 0
  ) in
  if i<>0
  then raise(Failure_in_clone_directory_creation)
  else 
  let _=Assistance_unix_command.uc("mkdir -p "^name_of_clone_directory) in
  let remotedir=Assistance_dfa_root.of_line name_of_clone_directory in
  let full_clone_command=
    "git clone "^
    (data.Assistance_fw_configuration_t.github_url)^" "^
    name_of_clone_directory in 
  let j=Assistance_unix_command.uc full_clone_command in
  if j<>0
  then raise(Failure_during_github_cloning)
  else 
  let cmds = commands_for_confidentiality data.Assistance_fw_configuration_t.encoding_protected_files in 
  let _= Assistance_unix_command.conditional_multiple_uc cmds in 
  let root_dir = data.Assistance_fw_configuration_t.root in 
  let diff1=Assistance_prepare_dircopy_update.compute_restricted_diff
     root_dir remotedir (data.Assistance_fw_configuration_t.ignored_subdirectories,
        (Assistance_image.image Assistance_dfn_rootless.to_line data.Assistance_fw_configuration_t.ignored_files) ) in
  filter_diff_according_to_admissibility  data diff1;;
          


end;;






module Assistance_coma_big_constant=struct

(* 
#use"Compilation_management/coma_big_constant.ml";;
*)

let github_url = "https://github.com/ewan-delanoy/skeptical_duck";;
let home = Sys.getenv "HOME" ;;

module This_World=struct

let root=Assistance_dfa_root.of_line (home^"/Teuliou/OCaml/Ordinary");;
let backup_dir=Assistance_dfa_root.of_line (home^"/Teuliou/OCaml/Githubbed_ocaml");;
let githubbing=false;;
let triple = (root,backup_dir,githubbing);;

end;;
module Next_World=struct

let root=Assistance_dfa_root.of_line (home^"/Teuliou/OCaml/Idaho");;
let backup_dir=Assistance_dfa_root.of_line (home^"/Teuliou/OCaml/Idaho_backup") ;;
let githubbing=false;;
let triple = (root,backup_dir,githubbing);;

end;;
module Third_World=struct

let root=Assistance_dfa_root.of_line (home^"/Teuliou/OCaml/Cherokee") ;;
let backup_dir=Assistance_dfa_root.of_line (home^"/Teuliou/OCaml/Cherokee_backup") ;;
let githubbing=false;;
let triple = (root,backup_dir,githubbing);;

end;;





end;;






module Assistance_fw_wrapper_t=struct

(*

#use"Filewatching/fw_wrapper_t.ml";;

Acts on the physical Unix world around, within the limits
defined in  the configuration parameter

*)

type t ={
  configuration         : Assistance_fw_configuration_t.t ;
  compilable_files      : (Assistance_dfn_rootless_t.t * string ) list;
  noncompilable_files   : (Assistance_dfn_rootless_t.t * string ) list;
  last_noticed_changes  : Assistance_dircopy_diff_t.t ;
};;

end;;






module Assistance_ocaml_library_t=struct

(* 

#use"Compilation_management/ocaml_library_t.ml";;

*)


type t=NumLib |StrLib |UnixLib;;

 


end;;






module Assistance_coma_state_t=struct

(* 

#use"Compilation_management/coma_state_t.ml";;

*)


type t={
     frontier_with_unix_world : Assistance_fw_wrapper_t.t;
     modules : Assistance_dfa_module_t.t list ;
     subdir_for_module : (Assistance_dfa_module_t.t * Assistance_dfa_subdirectory_t.t ) list;
     principal_ending_for_module : (Assistance_dfa_module_t.t * Assistance_dfa_ending_t.t ) list;
     mli_presence_for_module : (Assistance_dfa_module_t.t * bool ) list;
     principal_mt_for_module : (Assistance_dfa_module_t.t * string ) list;
     mli_mt_for_module : (Assistance_dfa_module_t.t * string ) list;
     needed_libs_for_module : (Assistance_dfa_module_t.t * Assistance_ocaml_library_t.t list ) list;
     direct_fathers_for_module : (Assistance_dfa_module_t.t * Assistance_dfa_module_t.t list ) list;
     ancestors_for_module : (Assistance_dfa_module_t.t * Assistance_dfa_module_t.t list ) list; 
     needed_dirs_for_module : (Assistance_dfa_module_t.t * (Assistance_dfa_subdirectory_t.t list)) list;
     last_compilation_result_for_module : (Assistance_dfa_module_t.t * bool) list;
     directories : Assistance_dfa_subdirectory_t.t list;
     printer_equipped_types : (Assistance_dfn_endingless_t.t*bool) list;
};;


end;;






module Assistance_dfn_endingless=struct

(*

#use"Decomposed_filename/dfn_endingless.ml";;


*)

let begins_with (Assistance_dfn_endingless_t.J(r,s,m)) subdir=
   Assistance_dfa_subdirectory.begins_with s subdir;;

let to_root (Assistance_dfn_endingless_t.J(r,s,m))=r;;
let to_subdirectory  (Assistance_dfn_endingless_t.J(r,s,m))=s;;
let to_module  (Assistance_dfn_endingless_t.J(r,s,m))=m;;
   
let to_line (Assistance_dfn_endingless_t.J(r,s,m))=
   (Assistance_dfa_root.connectable_to_subpath r)^
   (Assistance_dfa_subdirectory.connectable_to_subpath s)^
   (Assistance_dfa_module.to_line m);;

let to_middle (Assistance_dfn_endingless_t.J(r,s,m)) = Assistance_dfn_middle_t.J(s,m) ;;

let rename_endsubdirectory 
   (old_subdir,new_subdirname) 
      (Assistance_dfn_endingless_t.J(r,s,m))=
   Assistance_dfn_endingless_t.J(
	      r,
   		(Assistance_dfa_subdirectory.rename_endsubdirectory (old_subdir,new_subdirname) s),
         m
	    );;  

let rename_module
   (old_module,new_module) 
      (Assistance_dfn_endingless_t.J(r,s,m))=
        if m =old_module 
        then Assistance_dfn_endingless_t.J(r,s,new_module) 
        else Assistance_dfn_endingless_t.J(r,s,m) ;;     

let replace_subdirectory (old_subdir,new_subdir) eless = 
    match eless with  
      (Assistance_dfn_endingless_t.J(r,s,m)) -> 
       if s <> old_subdir 
       then eless 
       else 
   Assistance_dfn_endingless_t.J(r,new_subdir,m);;  
   
let soak (old_subdir,new_subdir) eless =
   let (Assistance_dfn_endingless_t.J(r,s,m)) = eless in 
   match Assistance_dfa_subdirectory.soak (old_subdir,new_subdir) s with 
   Some(new_s)->Some(Assistance_dfn_endingless_t.J(r,new_s,m))
   |None -> None ;;


let to_concrete_object (Assistance_dfn_endingless_t.J(r,s,m))=
   Assistance_concrete_object_t.Variant("Dfn_"^"endingless.J",
     [
        Assistance_dfa_root.to_concrete_object r;
        Assistance_dfa_subdirectory.to_concrete_object s;
        Assistance_dfa_module.to_concrete_object m;
     ]
   ) ;;

let of_concrete_object crobj =
   let (_,(arg1,arg2,arg3,_,_,_,_))=Assistance_concrete_object_automatic.unwrap_bounded_variant crobj in 
   Assistance_dfn_endingless_t.J(
      Assistance_dfa_root.of_concrete_object arg1,
      Assistance_dfa_subdirectory.of_concrete_object arg2,
      Assistance_dfa_module.of_concrete_object arg3
   );;
    





end;;






module Assistance_fw_wrapper_automatic=struct

(*

#use"Filewatching/fw_wrapper_field.ml";;

Acts on the surrounding physical Unix world, within the limits
defined in  the configuration parameter

*)

exception Rootless_not_found of Assistance_dfn_rootless_t.t;;


module Private = struct 

let pair_of_crobj crobj=
   let (_,(arg1,arg2,_,_,_,_,_))=Assistance_concrete_object_automatic.unwrap_bounded_variant crobj in 
  (
    Assistance_dfn_rootless.of_concrete_object arg1,
    Assistance_crobj_converter.string_of_concrete_object arg2
  );;

let pair_to_crobj (watched_file,modif_date)=
  Assistance_concrete_object_t.Variant("Dfn_"^"rootless.J",
     [
        
        Assistance_dfn_rootless.to_concrete_object watched_file;
        Assistance_crobj_converter.string_to_concrete_object(modif_date);
     ]
   ) ;;

let salt = "Fw_"^"wrapper_t.";;

let configuration_label        = salt ^ "configuration";;
let compilable_files_label     = salt ^ "compilable_files";;
let noncompilable_files_label  = salt ^ "noncompilable_files";;
let last_noticed_changes_label = salt ^ "last_noticed_changes";;

let of_concrete_object ccrt_obj = 
   let g=Assistance_concrete_object_automatic.get_record ccrt_obj in
   {
      Assistance_fw_wrapper_t.configuration = Assistance_fw_configuration.of_concrete_object(g configuration_label);
      compilable_files = Assistance_crobj_converter_combinator.to_list pair_of_crobj (g compilable_files_label);
      noncompilable_files = Assistance_crobj_converter_combinator.to_list pair_of_crobj (g noncompilable_files_label);
      last_noticed_changes = Assistance_dircopy_diff.of_concrete_object (g last_noticed_changes_label);
   };; 

let to_concrete_object fw=
   let items= 
   [
    configuration_label, Assistance_fw_configuration.to_concrete_object fw.Assistance_fw_wrapper_t.configuration;
    compilable_files_label, Assistance_crobj_converter_combinator.of_list pair_to_crobj fw.Assistance_fw_wrapper_t.compilable_files;
    noncompilable_files_label, Assistance_crobj_converter_combinator.of_list pair_to_crobj fw.Assistance_fw_wrapper_t.noncompilable_files;
    last_noticed_changes_label, Assistance_dircopy_diff.to_concrete_object fw.Assistance_fw_wrapper_t.last_noticed_changes
   ]  in
   Assistance_concrete_object_t.Record items;;


end ;;

let reflect_changes_in_diff fw l= {
   fw with 
   Assistance_fw_wrapper_t.last_noticed_changes = 
     Assistance_dircopy_diff.add_changes 
       (fw.Assistance_fw_wrapper_t.last_noticed_changes) l
} ;;

let get_content fw rootless = 
    let root = Assistance_fw_configuration.root (fw.Assistance_fw_wrapper_t.configuration) in 
    let s_ap = Assistance_dfn_common.recompose_potential_absolute_path root rootless in 
    Assistance_io.read_whole_file(Assistance_absolute_path.of_string s_ap);;     
        
let get_mtime_or_zero_if_file_is_nonregistered fw rootless =
   match Assistance_option.seek (fun (rootless1,_)->rootless1=rootless) 
    ((fw.Assistance_fw_wrapper_t.compilable_files)@(fw.Assistance_fw_wrapper_t.noncompilable_files)) with 
   None -> "0."
  |Some(_,mtime)-> mtime  ;; 


let get_mtime fw rootless  =
  match Assistance_option.seek (fun (rootless1,_)->rootless1=rootless) 
    ((fw.Assistance_fw_wrapper_t.compilable_files)@(fw.Assistance_fw_wrapper_t.noncompilable_files)) with 
   None -> raise (Rootless_not_found(rootless))
  |Some(_,mtime)-> mtime  ;; 

let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;

let reflect_creations_in_diff fw created_ones= {
   fw with 
   Assistance_fw_wrapper_t.last_noticed_changes = 
     Assistance_dircopy_diff.create 
       (fw.Assistance_fw_wrapper_t.last_noticed_changes) created_ones
} ;;


let reflect_destructions_in_diff fw destroyed_ones = {
   fw with 
   Assistance_fw_wrapper_t.last_noticed_changes = 
     Assistance_dircopy_diff.destroy  
       (fw.Assistance_fw_wrapper_t.last_noticed_changes) destroyed_ones 
} ;;


let reflect_replacements_in_diff fw reps= {
   fw with 
   Assistance_fw_wrapper_t.last_noticed_changes = 
     Assistance_dircopy_diff.replace 
       (fw.Assistance_fw_wrapper_t.last_noticed_changes) reps
} ;;

let root fw = Assistance_fw_configuration.root (fw.Assistance_fw_wrapper_t.configuration);;



end;;






module Assistance_outside_comments_and_strings=struct

(*

#use"outside_comments_and_strings.ml";;

Detect in a text the parts which can possibly contain module
names, i.e. those parts which are outside comments and outside
strings.

Comments are a little more complicated than strings because they
can be nested. Also, note that we can have strings inside comments :
for example (* a " ( * " b *) is a valid OCaml code snippet.


To keep the automaton simple, changes are notified as soon as
possible. Thus, the automaton toggles string_mode or changes
nesting comment depth as soon as the terminating character is
encountered.

*)

type state={
    depth : int;
    string_mode         : bool;
    lastchar_is_a_left_paren          : bool;
    lastchar_is_a_star                : bool;
    lastchar_is_a_backslash           : bool;
    lastchar_is_a_tick           : bool;
    rightmost_backslash_count_is_even : bool;
    penultchar_is_a_left_paren        : bool;
    interval_start : int;
    accumulator : (int*int*string) list;
};;


let one_more_step s n j c x=
   let d=x.depth in 
   let comment_opened_now=(x.lastchar_is_a_left_paren)&&(c='*')&&(not(x.string_mode))
   and comment_closed_now=
            (not(x.penultchar_is_a_left_paren))
            &&(x.lastchar_is_a_star)
            &&(c=')')
            &&(not(x.string_mode)) in
   let new_depth=(
   		if x.string_mode
        then d
        else
        if comment_opened_now
        then d+1
        else  
        if comment_closed_now
        then max 0 (d-1)
        else  d
   
   ) in
   let string_opened_now=(c='"')&&(not(x.string_mode))&&
       (not(x.lastchar_is_a_backslash))&&
       (not(x.lastchar_is_a_tick))
   and string_closed_now=(c='"')&&(x.string_mode)&&(
      if x.lastchar_is_a_backslash
      then x.rightmost_backslash_count_is_even
      else true
   ) in
   let new_start=
      ((x.depth=0)&&string_closed_now)
      ||
      ((x.depth=1)&&comment_closed_now)
      ||
      ((x.depth=0)&&comment_opened_now) in
    let optional_last_index_for_interval=(
       if x.depth>0
       then None
       else
       if string_opened_now
       then Some(j-1)
       else
       if comment_opened_now
       then Some(j-2)
       else 
       if (j=n)&&(not(x.string_mode))
       then Some(j)
       else None
    ) in
    let old_accu=x.accumulator in  
    let new_accu=(
       match optional_last_index_for_interval with
       None->old_accu
       |Some(upper_bound)->
           let lower_bound=x.interval_start in
           if lower_bound>upper_bound
           then old_accu
           else let new_itv=Assistance_cull_string.interval s lower_bound upper_bound in 
               (lower_bound,upper_bound,new_itv)::old_accu
    ) in  
      
  {
    depth =new_depth;
    string_mode    =(if string_opened_now then true else
                     if string_closed_now then false else
                     x.string_mode);
    lastchar_is_a_left_paren   =(c='(');
    lastchar_is_a_star         =(c='*');
    lastchar_is_a_backslash    =(c='\\');
    lastchar_is_a_tick    =(c='\'');
    rightmost_backslash_count_is_even=(if c<>'\\' 
                                       then true 
                                       else not(x.rightmost_backslash_count_is_even) );
    penultchar_is_a_left_paren =x.lastchar_is_a_left_paren;
    interval_start=(if new_start then j+1 else x.interval_start);
    accumulator=new_accu;
};;

let initial_state=  
 {
    depth =0;
    string_mode    =false;
    lastchar_is_a_left_paren   =false;
    lastchar_is_a_star         =false;
    lastchar_is_a_backslash    =false;
    lastchar_is_a_tick    =false;
    rightmost_backslash_count_is_even=true;
    penultchar_is_a_left_paren =false;
    interval_start=1;
    accumulator=[];
};;

let rec iterator (s,n,j,st)=
    if j>n
    then List.rev(st.accumulator)
    else iterator(s,n,j+1,one_more_step s n j (String.get s (j-1)) st);;
    
let good_substrings s=iterator(s,String.length s,1,initial_state);;    

(*

[
((good_substrings "abcdef")=    [1, 6, "abcdef"]);
((good_substrings "(*abc*)def")=[8, 10, "def"]);
((good_substrings "ab(*cde*)f")=[1, 2, "ab"; 10, 10, "f"]);
((good_substrings "let a=\"\\\"\" in a+1;;")=[1, 6, "let a="; 11, 19, " in a+1;;"] );
((good_substrings "let a='\\\"' in a+2;;")=[1, 19, "let a='\\\"' in a+2;;"]  );
((good_substrings "let a=\"\\\\\" in a+3;;")=[1, 6, "let a="; 11, 19, " in a+3;;"]  );
((good_substrings "let a=\"\\\\\\\" in a+3;;")=[1, 6, "let a="]  );
];;

good_substrings "ab\"cde\"f";;
good_substrings "\"abc\"def";;
good_substrings "ghi(*a(*b*)c*)def";;
good_substrings "ghi(**a(*b*)c**)def";;
good_substrings "ghi(**a\"b\"c**)def";;
good_substrings "123\"(*\"890\"*)\"567";;
good_substrings "123(*67\"90\"23*)67";;
good_substrings "let a=7;; let b='\"';; let c=8;;";;
good_substrings "let a=7;; let b='\"';; (* ahem *) let c=8;;";;

let nachste (s,n,j,st)=(s,n,j+1,one_more_step s n j (String.get s (j-1)) st);;
let s0="123456\"\\\\\"123456789";;
let n0=String.length s0;;
let v0=(s0,n0,1,initial_state);;
let ff=Memoized.small nachste v0;;
let gg n=match ff n with (_,_,_,st)->st;;


*)      

           

end;;






module Assistance_three_parts=struct

(*

#use"three_parts.ml";;

*)




let generic=function
[]->[]
|u::v->
let rec tempf=
(function
((graet,x,da_ober),accu)->
let accu2=(graet,x,da_ober)::accu in
match da_ober with
[]->accu2
|a::b->tempf((x::graet,a,b),accu2)
) in
tempf(([],u,v),[]);;

let complemented_points l=List.rev_map(function (kleiz,x,dehou)->
(x,List.rev_append(kleiz)(dehou)))
(generic l);;

let beheaded_tails l=List.rev_map (function (kleiz,x,dehou)->(x,dehou) )(generic l);;

let select_center_element_and_reverse_left f l=
  let rec tempf=(fun (graet,da_ober)->match da_ober with
  []->(graet,None,[])
  |x::peurrest->if f x 
                then (graet,Some(x),peurrest)
                else tempf(x::graet,peurrest)
  ) in
  tempf([],l);;

let select_center_element f l=
  let (temp1,opt,after)=select_center_element_and_reverse_left f l in 
  (List.rev temp1,opt,after);;

let decompose_according_to_end_markers f l =
  let rec tempf=(
     fun (treated,to_be_treated)->
       let (before,opt,after)=select_center_element f to_be_treated in 
       if opt=None then (List.rev treated,before) else 
       tempf((before,Assistance_option.unpack opt)::treated,after)
  ) in 
  tempf([],l);;


let decompose_according_to_beginning_markers f l=
  let (nonlast_ones,last_one)=decompose_according_to_end_markers f (List.rev l) in
  (List.rev last_one,
    List.rev_map (fun (x,marker)->
      (marker,List.rev x)
    ) nonlast_ones
  );; 
  

(*

decompose_according_to_beginning_markers (fun x->List.mem x [3;7;11]) 
[1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20];;

decompose_according_to_end_markers (fun x->List.mem x [3;7;11]) 
[1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20];;

*)  

let select_left_interval f l=
  (* note that the "interval" is returned in reverse form *)
  let rec tempf=(fun (graet,da_ober)->match da_ober with
  []->(graet,[])
  |x::peurrest->if f x 
                then tempf(x::graet,peurrest) 
                else (graet,da_ober)
  ) in
  tempf([],l);;

let select_center_interval f l=
  let rec tempf=(fun (graet,da_ober)->match da_ober with
  []->(List.rev graet,[],[])
  |x::peurrest->if f x 
                then let (temp1,temp2)=select_left_interval f da_ober in
                     (List.rev graet,List.rev(temp1),temp2)
                else tempf(x::graet,peurrest)
  ) in
  tempf([],l);;


let replace_in_list replacee replacer l=
  let (temp1,opt,temp2)=select_center_element (fun t->t=replacee) l in
  if opt=None then l else List.rev_append temp1 (replacer@temp2);;
   
           

end;;






module Assistance_look_for_module_names=struct

(*

#use"Ocaml_analysis/look_for_module_names.ml";;

*)

exception Unknown_ending_during_modulename_changing of string ;;
exception Change_not_implemented of string ;;

exception Unknown_ending_during_modulename_reading of string ;;
exception Reading_not_implemented of string ;;

module Private = struct 


let indices_in_ml_ocamlcode code=
  let temp1=Assistance_outside_comments_and_strings.good_substrings code in
  let temp2=Assistance_image.image (fun (a,b,t)->
     let ttemp3=Assistance_alternative_str.find_all_occurrences Assistance_alternative_str_example.moodle_cases t 1 in
     Assistance_image.image (fun (case_index,(u,v))->
        (case_index,(u+a-1,v+a-1))
     ) ttemp3
  ) temp1 in
  List.flatten temp2;;

let indices_in_mli_ocamlcode code=  indices_in_ml_ocamlcode code ;; 
  
let indices_in_mlx_file ap=  
    let s_ap = Assistance_absolute_path.to_string ap in 
    let ending = Assistance_cull_string.after_rightmost s_ap '.' in 
    if ending = "ml"  then indices_in_ml_ocamlcode (Assistance_io.read_whole_file ap) else 
    if ending = "mli" then indices_in_mli_ocamlcode (Assistance_io.read_whole_file ap) else   
    if ending = "mll" then raise(Change_not_implemented s_ap) else 
    if ending = "mly" then raise(Change_not_implemented s_ap) else   
    raise(Unknown_ending_during_modulename_reading s_ap);;  


let indices_in_ml_file file=indices_in_ml_ocamlcode(Assistance_io.read_whole_file file);;  

let names_in_mlx_file ap=
  let temp1=indices_in_mlx_file ap in
  let text = Assistance_io.read_whole_file ap in 
  let temp2=Assistance_image.image (fun (_,(a,b))->String.sub text (a-1) (b-a+1) ) temp1 in
  let temp3=Assistance_three_parts.generic temp2 in
  let temp4=List.filter (fun (x,y,z)->not(List.mem y x)) temp3 in
  let temp5=Assistance_image.image (fun (x,y,z)->Assistance_dfa_module.of_line 
      (String.uncapitalize_ascii  y)) temp4 in
  temp5;;


let change_module_name_in_ml_ocamlcode
   old_naked_name
   new_naked_name old_code=
   let old_name=String.capitalize_ascii(Assistance_dfa_module.to_line(old_naked_name))
   and new_name=String.capitalize_ascii(Assistance_dfa_module.to_line(new_naked_name)) in
   let itv=(fun a b->String.sub old_code (a-1) (b-a+1)) in
   let temp1=indices_in_ml_ocamlcode old_code in
   let temp2=List.filter (fun (j,(a,b))->(itv a b)=old_name ) temp1 in
   if temp2=[]
   then old_code
   else
   let temp3 = Assistance_image.image (fun (j,(a,b))->((a,b),new_name) ) temp2 in  
   Assistance_strung.replace_ranges_in temp3 old_code;;
 
  

 let change_module_name_in_ml_file old_name new_name file=
   let s=Assistance_io.read_whole_file file in
   let new_s=change_module_name_in_ml_ocamlcode old_name new_name s in
   Assistance_io.overwrite_with file new_s;;  

 let change_module_name_in_mli_file old_name new_name file=
 change_module_name_in_ml_file old_name new_name file ;;

  let change_module_name_in_mlx_file old_name new_name ap=  
    let s_ap = Assistance_absolute_path.to_string ap in 
    let ending = Assistance_cull_string.after_rightmost s_ap '.' in 
    if ending = "ml"  then change_module_name_in_ml_file old_name new_name ap else 
    if ending = "mli" then change_module_name_in_mli_file old_name new_name ap else   
    if ending = "mll" then raise(Change_not_implemented s_ap) else 
    if ending = "mly" then raise(Change_not_implemented s_ap) else   
    raise(Unknown_ending_during_modulename_changing s_ap);;

let change_several_module_names_in_ml_ocamlcode l_changes s=
    List.fold_left(fun t (u,v)->change_module_name_in_ml_ocamlcode u v t) s l_changes;;

let change_several_module_names_in_ml_file l_changes file=
   let s=Assistance_io.read_whole_file file in
   let new_s=change_several_module_names_in_ml_ocamlcode l_changes s in
   Assistance_io.overwrite_with file new_s;;  

end ;;

let change_module_name_in_mlx_file = Private.change_module_name_in_mlx_file ;;
 let change_module_name_in_ml_ocamlcode = Private.change_module_name_in_ml_ocamlcode ;;
 let change_several_module_names_in_ml_ocamlcode = Private.change_several_module_names_in_ml_ocamlcode ;;
 let indices_in_mlx_file = Private.indices_in_mlx_file ;;
 let names_in_mlx_file = Private.names_in_mlx_file ;;
 

(*   
   
indices_in_string "123 Haag.012 open Garfield;8";;

indices_in_string "(* Haag. *)234 Dog.\"open Garfield;\"67 Corn.4";;

let example = String.concat "\n" [
""; "open Aantron_markup_common"; ""; "module Aantron_peggy = Aantron_kstream";""; "include Aantron_kstream.Foo";"";"val parse :";
"  [< `Document | `Fragment of string ] option ->";
"   Aantron_markup_error.parse_handler ->";
"  (location *  Aantron_html_tokenizer.token)  Aantron_kstream.t *";
"  ( Aantron_html_tokenizer.state -> unit) *";
"  ((unit -> bool) -> unit) ->";
"    (location * signal)  Aantron_kstream.t"; ""] ;;

let see_example = change_module_name_in_ml_ocamlcode
   (Dfa_module_t.M "aantron_kstream") (Dfa_module_t.M "other_kstream") z1 ;;
   
*)              

end;;






module Assistance_lines_in_string=struct

(*

#use"lines_in_string.ml";;

*)


module Private = struct 

  let core old_s=
     let left_offset=(if Assistance_supstring.begins_with old_s "\n" then "\n" else "")
     and right_offset=(if Assistance_supstring.ends_with old_s "\n" then "\n" else "") in
     let s=left_offset^old_s^right_offset in
     let temp1=Str.split (Str.regexp_string "\n") s in
     Assistance_ennig.index_everything temp1;;
  
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
  
  let lines s= Assistance_image.image snd (core s);;
  
  let interval s i j=
      let temp1=core s in
      let temp2=List.filter (fun (k,_)->(i<=k)&&(k<=j)) temp1  in
      let temp3=Assistance_image.image snd temp2 in
      String.concat "\n" temp3;; 
  
  
  exception Lines_in_char_range_exn of int*int;;
  
  let number_of_lines_in_char_interval s  i j=
     try (List.length(List.filter (fun k->
         String.get s (k-1)='\n'
     ) (Assistance_ennig.ennig i j))) with
     _->raise(Lines_in_char_range_exn(i,j));; 
  
  let line_index_from_char_index s char_idx=
    1+(number_of_lines_in_char_interval s 1 char_idx);;
  
  
  
  let remove_interval s i j=
    let temp1=core s in
    let temp2=List.filter (fun (k,_)->(i>k)||(k>j)) temp1  in
    let temp3=Assistance_image.image snd temp2 in
    String.concat "\n" temp3;; 
  
  let remove_interval_in_file fn i j=
      let s1=Assistance_io.read_whole_file fn in
      let s2=remove_interval s1 i j  in
     Assistance_io.overwrite_with fn s2;;   
  
  let remove_lines_containing_substring_in_string pattern text =
     let temp1=core text in
     let temp2=List.filter (fun (_,line)->not(Assistance_substring.is_a_substring_of pattern line)) temp1  in
     let temp3=Assistance_image.image snd temp2 in
     String.concat "\n" temp3;; 
   
   let remove_lines_containing_substring_in_file pattern fn=
       let old_text=Assistance_io.read_whole_file fn in
       let new_text=remove_lines_containing_substring_in_string pattern old_text  in
      Assistance_io.overwrite_with fn new_text;;   
  
  let tripartition_associated_to_interval s i j=
     let temp2=lines s in 
     let (temp3,temp4)=Assistance_listennou.big_rht (i-1) temp2 in 
     let part1=String.concat "\n" (List.rev temp3) in 
     let (temp5,temp6)=Assistance_listennou.big_rht (j-i+1) temp4 in 
     let part2=String.concat "\n" (List.rev temp5) in 
     let part3=String.concat "\n" temp6 in 
     (part1^"\n",part2,"\n"^part3);;
  
  (* tripartition_associated_to_interval "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)
  
let suppress_linebreaks_in_interval s i j=
    let (part1,old_part2,part3) = tripartition_associated_to_interval s i j in 
    let new_part2 = String.concat "" (lines old_part2) in 
    part1^new_part2^part3 ;; 
  
  (* suppress_linebreaks_in_interval "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)
  
let suppress_linebreaks_in_interval_in_file fn i j=
    let s1=Assistance_io.read_whole_file fn in
    let s2=suppress_linebreaks_in_interval s1 i j  in
    Assistance_io.overwrite_with fn s2;;     

let indent_interval_in_string_with (i,j) ~text ~tab_width =
     let old_lines = core text 
     and tab = String.make tab_width ' ' in 
     let new_lines = Assistance_image.image (
         fun (k,line) -> 
           if (k<i)||(k>j)
           then line
          else tab^line
     ) old_lines in 
     String.concat "\n" new_lines ;;

(* ident_interval_in_string_with (2,5) ~text:"1\n2\n3\n4\n5\n6\n7\n" ~tab_width:3;; *)

let indent_interval_in_file_with (i,j) fn ~tab_width=
    let old_text=Assistance_io.read_whole_file fn in
    let new_text=indent_interval_in_string_with (i,j) ~text:old_text ~tab_width  in
    Assistance_io.overwrite_with fn new_text;;   

end;;






module Assistance_put_use_directive_in_initial_comment=struct

(*

#use"Text_editing/put_use_directive_in_initial_comment.ml";;

*)

let detect_initial_comment_in_text text = 
  let lines = Assistance_lines_in_string.core text in 
  let first_line = snd (List.hd lines) in 
  if (Assistance_cull_string.trim_spaces first_line) <> "(*" then None else 
  match Assistance_option.seek (fun (line_idx,line)->
     if (line_idx<=1) then false else
     let trimmed_line = Assistance_cull_string.trim_spaces line in 
     Assistance_supstring.begins_with trimmed_line "#use"
  ) lines with 
  None -> None 
  |Some(i1,line1) ->
 (
  match Assistance_option.seek (fun (line_idx,line)->
    if (line_idx<=i1) then false else
    let trimmed_line = Assistance_cull_string.trim_spaces line in 
    trimmed_line = "*)"
 ) lines with 
  None -> None 
 |Some(i2,_) -> Some(i1,Assistance_cull_string.trim_spaces line1,i2) );;  

let detect_initial_comment_in_file  fn =  detect_initial_comment_in_text  (Assistance_io.read_whole_file fn) ;;

let in_text ~new_directive text =
  match detect_initial_comment_in_text text  with 
  None -> text 
  |Some(i1,line1,i2) ->
    let old_lines = Assistance_lines_in_string.core text in 
    let new_lines = Assistance_image.image (fun (line_idx,line)->
        if line_idx = i1 then new_directive else line
      ) old_lines in 
   String.concat "\n" new_lines ;;   

let in_file ~new_directive fn =   
   let text = Assistance_io.read_whole_file fn in 
   match detect_initial_comment_in_text text  with 
  None -> () 
  |Some(i1,line1,i2) ->
    let old_lines = Assistance_lines_in_string.core text in 
    let new_lines = Assistance_image.image (fun (line_idx,line)->
        if line_idx = i1 then new_directive else line
      ) old_lines in 
    let new_text = String.concat "\n" new_lines in 
    Assistance_io.overwrite_with fn new_text;;

let usual root ap =
    let s_ap=Assistance_absolute_path.to_string ap in 
    let s_cdir=Assistance_dfa_root.connectable_to_subpath root in 
    let shortened_path=Assistance_cull_string.cobeginning (String.length s_cdir) s_ap in 
    "#use\""^shortened_path^"\";"^";" ;;
    
let put_usual root ap =
    let new_directive = usual root ap in 
    in_file ~new_directive ap;;    

end;;






module Assistance_reflect_change_in_github=struct

(* 

#use"reflect_change_in_github.ml";;

*)

module Private = struct

let commands_for_backup config diff=
   if Assistance_dircopy_diff.is_empty diff
   then ([],[])
   else 
   let source_dir = config.Assistance_fw_configuration_t.root 
  and destination_dir = config.Assistance_fw_configuration_t.dir_for_backup in 
   let s_destination=Assistance_dfa_root.connectable_to_subpath destination_dir in
   let created_ones=Assistance_image.image Assistance_dfn_rootless.to_line (Assistance_dircopy_diff.recently_created diff) in
   let temp2=Assistance_option.filter_and_unpack
   (fun fn->
     if String.contains fn '/'
     then let dn=Assistance_cull_string.before_rightmost fn '/' in
          Some("mkdir -p "^s_destination^dn)
     else None 
    ) created_ones in
   let temp3=Assistance_ordered.sort Assistance_total_ordering.silex_for_strings temp2 in
   let s_source=Assistance_dfa_root.connectable_to_subpath source_dir in
   let temp4=Assistance_image.image(
      fun fn->
      "cp "^s_source^fn^" "^s_destination^(Assistance_cull_string.before_rightmost fn '/')
   ) created_ones in
   let changed_ones=Assistance_image.image Assistance_dfn_rootless.to_line (Assistance_dircopy_diff.recently_changed diff) in
   let temp5=Assistance_image.image(
      fun fn->
      "cp "^s_source^fn^" "^s_destination^fn
   ) changed_ones in
   let temp6=Assistance_image.image(
      fun fn->
      "git add "^fn
   ) (created_ones@changed_ones) in
   let temp7=Assistance_image.image(
      fun rl->
      let fn = Assistance_dfn_rootless.to_line rl in 
      "git rm "^fn
   ) (Assistance_dircopy_diff.recently_deleted diff) in
   let temp8= Assistance_image.image (
     fun (replacer,replacee) ->
       let s_replacer = Assistance_dfn_rootless.to_line  replacer 
       and s_backup_dir = Assistance_dfa_root.connectable_to_subpath destination_dir in 
       let s_full_path = s_backup_dir^(Assistance_dfn_rootless.to_line replacee) in 
       Assistance_unix_command.prefix_for_replacing_patterns^s_replacer^" "^s_full_path
   ) config.Assistance_fw_configuration_t.encoding_protected_files in 
   (temp3@temp4@temp5@temp8,temp6@temp7);;

let backup_with_message config  diff msg=
  let destination_dir = config.Assistance_fw_configuration_t.dir_for_backup in 
  let (nongit_cmds,git_cmds)=commands_for_backup config diff in
  let s_destination=Assistance_dfa_root.connectable_to_subpath destination_dir in
  let _=Assistance_image.image Assistance_unix_command.uc nongit_cmds in
  let _=(
  if config.Assistance_fw_configuration_t.gitpush_after_backup
  then let cwd=Sys.getcwd() in
       Assistance_image.image Assistance_unix_command.uc
       (
       [Assistance_unix_command.cd s_destination]@   
       git_cmds@   
       [
         "git commit -m \""^msg^"\"";
         "git push"
       ]@
       [Assistance_unix_command.cd cwd]
       ) 
  else let cwd=Sys.getcwd() in
       Assistance_image.image Assistance_unix_command.uc
       (
       [Assistance_unix_command.cd s_destination]@   
       git_cmds@   
       [
         "git commit -m \""^msg^"\""
       ]@
       [Assistance_unix_command.cd cwd]
       ) 
  ) in
  ();;

let backup config diff opt_msg=
  if Assistance_dircopy_diff.is_empty diff
  then (print_string "No recent changes to commit ...";flush stdout) 
  else 
  let msg=(
   match opt_msg with
    None->Assistance_dircopy_diff.explain diff
   |Some(msg0)->msg0) in
  backup_with_message config diff msg;;
  
end ;; 

let backup config diff opt_msg=
  Private.backup config diff opt_msg;;

  
  

end;;






module Assistance_isolated_occurrences=struct

(*

#use"isolated_occurrences.ml";;

Used to detect mentions of previously defined names in
the same OCaml module.

An occurrence of a substring is isolated when it 
cannot be extended to a meaningful Ocaml name. So we look at
the surrounding characters, on the left and on the right.


*)

module Private=struct

exception Unclear_left_char of char;;
exception Unclear_right_char of char;;

let rejected_left_chars=
  [
   	'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
    'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
    'u';'v';'w';'x';'y';'z';
    'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
    'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';
    'U';'V';'W';'X';'Y';'Z';
    '0';'1';'2';'3';'4';'5';'6';'7';'8';'9';
    '_';
  ];;

let admitted_left_chars=
  [
   	'(' ; ')' ; ';' ; ' ' ;'\n';'\r';'=';'<';'>';'+';'*';'/';'-'; '.'; ',';
  ];;

let rejected_right_chars=
  [
   	'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
    'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
    'u';'v';'w';'x';'y';'z';
    'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
    'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';
    'U';'V';'W';'X';'Y';'Z';
    '0';'1';'2';'3';'4';'5';'6';'7';'8';'9';
    '_';
  ];;

let admitted_right_chars=
  [
   	'(' ; ')' ; ';' ; ' ' ;'\n';'\r';'=';'<';'>';'+';'*';'/';'-'; '.'; ',';
  ];;

let test_for_left_admissiblity c=
   if List.mem c rejected_left_chars then false else
   if List.mem c admitted_left_chars then true else
   raise(Unclear_left_char(c));;
   
let test_for_right_admissiblity c=
   if List.mem c rejected_right_chars then false else
   if List.mem c admitted_right_chars then true else
   raise(Unclear_right_char(c));;   
   
let leftmost_small_test  s j=
   if j=0 
   then true 
   else test_for_left_admissiblity (String.get s (j-1));;

let rightmost_small_test  s j=
   if j=((String.length s)+1) 
   then true 
   else test_for_right_admissiblity (String.get s (j-1));;   
   

end;;

let of_in substr s=
  let l_substr=String.length substr 
  and n=String.length(s) in
  let main_test= (
    fun k->
      if ((String.sub s (k-1) l_substr)<>substr)
      then false
      else 
      ( Private.leftmost_small_test s (k-1) )
      &&
      ( Private.rightmost_small_test s (k+l_substr) )
      
  ) in
  Assistance_option.filter_and_unpack(
     fun k->
       if main_test k
       then Some(k,k+l_substr-1)
       else None
  ) (Assistance_ennig.ennig 1 (n+1-l_substr));;

   
(*   
   
of_in "garfield" 
"let x=garfield in let y=subgarfield and z=garfield2 in";;

of_in "garfield" "garfield is a cat";;

of_in "Boogie.Woogie.c" "48+Boogie.Woogie.c";;


*)              

end;;






module Assistance_ocaml_gsyntax_category=struct

(*

#use"Ocaml_analysis/ocaml_gsyntax_category.ml";;

*)

type t=
     Value
    |Type
    |Exception 
    |Module_opener
    |Module_closer 
    |Module_inclusion;;
    
               

end;;






module Assistance_ocaml_gsyntax_item=struct

(*

#use"Ocaml_analysis/ocaml_gsyntax_item.ml";;

*)

type t={
  category : Assistance_ocaml_gsyntax_category.t;
  name : string;
  interval_for_name : int*int;
  whole : string;
  content : string;
  interval_for_content : int*int;  
  is_an_included_item : bool;
};;

let name x=x.name;;
let content x=x.content;;
let whole x=x.whole;;

let make cat nm nm_itv intr ctnt ctnt_itv incldd_or_not=
    {
  		category =cat;
        name =nm;
        interval_for_name =nm_itv;
        whole =intr;
        content =ctnt;
        interval_for_content =ctnt_itv;  
        is_an_included_item =incldd_or_not;
    };;

let prepend_prefix prefix x=
    {
  		category =x.category;
        name =prefix^"."^x.name;
        interval_for_name =x.interval_for_name;
        whole =x.whole;
        content =x.content;
        interval_for_content =x.interval_for_content;  
        is_an_included_item =x.is_an_included_item;
    };;
    
let include_in_new_scope new_scope x=
    {
  		category =x.category;
        name =new_scope^(Assistance_cull_string.before_rightmost_possibly_all x.name '.');
        interval_for_name =x.interval_for_name;
        whole =x.whole;
        content =x.content;
        interval_for_content =x.interval_for_content;  
        is_an_included_item =true;
    };;    
    
let make_name_coincide_with_content x=
        {
            category =x.category;
            name =x.content;
            interval_for_name =x.interval_for_name;
            whole =x.whole;
            content =x.content;
            interval_for_content =x.interval_for_content;  
            is_an_included_item =x.is_an_included_item;
        };;    
    
    
    
    
    
    
               

end;;






module Assistance_overwrite_at_intervals=struct

(*

#use"overwrite_at_intervals.ml";;

*)



let inside_string replacings s=
  let n=String.length s
  and r=List.length replacings in
  let x_coord=(fun j->
    if j=1 then 1 else
    snd(fst(List.nth replacings ((j-3)/2)))+1
  ) and y_coord=(fun j->
   if j=2*r+1 then n else
    fst(fst(List.nth replacings ((j-1)/2)))-1
  ) in
  let xy_substring=(fun j->
    Assistance_cull_string.interval s (x_coord j) (y_coord j)
  ) in
  let all_parts=Assistance_ennig.doyle (
    fun j->
      if (j mod 2)=1
      then xy_substring j
      else Assistance_overwriter.to_string(snd(List.nth replacings ((j-2)/2)))
  ) 1 (2*r+1) in
  String.concat "" all_parts;;

(*

inside_string
 [(7,12),"garfield";(23,24),"jack";(30,30),"gas"]
 "12345678901234567890123456789012345678901234567890";;
 

*)

let inside_file replacings fn=
  let old_t=Assistance_io.read_whole_file fn in
  let new_t=inside_string replacings old_t in
  Assistance_io.overwrite_with fn new_t;;  
  





           

end;;






module Assistance_gparser=struct

(*

#use"GParser/gparser.ml";;

*)

type t=
     Constant of string
    |Enclosure of string*string
    |Footless_constant of string
    |Sample_char of string
    |Sample_neg of string
    |Sample_star of string
    |Sample_negstar of string
    |Sample_plus of string
    |Race of string*string
    |Comment of string*string*string*string
    |House_with_doors of string*string*((string*string) list)
    |Chain of t list
    |Disjunction of t list
    |Star of t
    |Detailed_star of t
    |One_or_more of t
    |Optional of t
    |Recoiling_ending of t*t
    |Detailed_chain of t list
;;
           

end;;






module Assistance_gparser_result=struct

(*

#use"GParser/gparser_result.ml";;

*)


type t={
   whole_range : int*int ;
   important_ranges : (int*int) list;
   final_cursor_position : int; 
   disjunction_index : int option;
};;

let whole_range x=x.whole_range;;
let important_ranges x=x.important_ranges;;
let final_cursor_position x=x.final_cursor_position;;
let disjunction_index x=x.disjunction_index;;

let veil b c d e={
   whole_range =b;
   important_ranges =c;
   final_cursor_position =d; 
   disjunction_index=e;
};;


           

end;;






module Assistance_gparser_fun=struct

(*

#use"GParser/gparser_fun.ml";;

*)

type t=(string->int->(Assistance_gparser_result.t option));;
           

end;;






module Assistance_gparser_house_with_doors=struct

(*

#use"GParser/gparser_house_with_doors.ml";;

*)

module Private=struct
type mistletoe={
     main_opener:string;
     main_closer:string;
     other_enclosers: (string*string) list;
     processed_argument: string;
     initial_index: int;
};;

type walker={
    current_index : int;
    current_depth : int;
    awaited_closer : string option;
    answer : Assistance_gparser_result.t option;
};;


let new_walker_in_first_case_in_hwd (m,wlkr)=
   let opt1=Assistance_option.seek(fun (opener,closer)->
     Assistance_substring.is_a_substring_located_at opener 
        m.processed_argument wlkr.current_index
   ) m.other_enclosers in
   if opt1<>None
   then let (op1,cl1)=Assistance_option.unpack opt1 in
        {
        	current_index =wlkr.current_index+(String.length op1);
            current_depth =wlkr.current_depth;
            awaited_closer=Some(cl1);
            answer =None;
        }
   else
   if Assistance_substring.is_a_substring_located_at m.main_opener 
        m.processed_argument wlkr.current_index
   then {
        	current_index =wlkr.current_index+(String.length m.main_opener);
            current_depth =wlkr.current_depth+1;
            awaited_closer=None;
            answer =None;
        }
   else   
   if not(Assistance_substring.is_a_substring_located_at m.main_closer 
      m.processed_argument wlkr.current_index)
   then {
        	current_index =wlkr.current_index+1;
            current_depth =wlkr.current_depth;
            awaited_closer=None;
            answer =None;
        }
   else    
   if wlkr.current_depth>1
   then {
        	current_index =wlkr.current_index+(String.length m.main_closer);
            current_depth =wlkr.current_depth-1;
            awaited_closer=None;
            answer =None;
        }
   else  
   let j1=wlkr.current_index+(String.length m.main_closer) in
   let res=Assistance_gparser_result.veil
               (m.initial_index,j1-1)
               []
               j1
               None        in
        {
        	current_index =wlkr.current_index+(String.length m.main_closer);
            current_depth =wlkr.current_depth-1;
            awaited_closer=None;
            answer =Some(res);
        };;
   

let first_case_in_hwd 
   (m,wlkr)=(m,new_walker_in_first_case_in_hwd (m,wlkr));;
  
let second_case_in_hwd (m,wlkr)=
  let rparen=Assistance_option.unpack wlkr.awaited_closer in
  if Assistance_substring.is_a_substring_located_at rparen 
      m.processed_argument wlkr.current_index
  then (m,{
        	current_index =wlkr.current_index+(String.length rparen);
            current_depth =wlkr.current_depth;
            awaited_closer=None;
            answer =None;
        })
  else (m,{
        	current_index =wlkr.current_index+1;
            current_depth =wlkr.current_depth;
            awaited_closer=wlkr.awaited_closer;
            answer =None;
        });;

let pusher_for_hwd w=
  let (m,wlkr)=w in
  if wlkr.answer=None
  then 
       (
         if wlkr.awaited_closer=None
         then first_case_in_hwd w
         else second_case_in_hwd w       
        )
  else w;;  
   
let rec iterator_for_hwd w=
   let (m,wlkr)=w in
   if wlkr.answer<>None
   then wlkr.answer
   else
   if wlkr.current_index>(String.length m.processed_argument)
   then None
   else iterator_for_hwd (pusher_for_hwd w);; 
  
let starter_for_hwd (main_opener,main_closer) other_enclosers s i=
  (
    {
     main_opener=main_opener;
     main_closer=main_closer;
     other_enclosers=other_enclosers;
     processed_argument=s;
     initial_index=i;
    }
  ,
    {
      current_index =i+(String.length main_opener);
      current_depth =1;
      awaited_closer=None;
      answer =None;
    }
  );;
end;;  

let hwd
   (main_opener,main_closer)
     other_enclosers=
   let rec tempf=(fun s i->
        if not(Assistance_substring.is_a_substring_located_at main_opener s i)
        then None 
        else 
          
          Private.iterator_for_hwd 
         (Private.starter_for_hwd (main_opener,main_closer) other_enclosers s i)
   ) in
   (tempf:Assistance_gparser_fun.t);;      
      
(*

hwd ("(*","*)") ["\"","\""] "(* Bye \"*)\" bye bird *)456" 1;;

*)



         
   
           

end;;






module Assistance_gparser_ocaml_comment=struct

(*

#use"GParser/gparser_ocaml_comment.ml";;

*)

module Private=struct
type mistletoe={
     comment_opener:string;
     comment_closer:string;
     quote_opener:string;
     quote_closer:string;
     processed_argument: string;
     initial_index: int;
};;

type walker={
    current_index : int;
    current_depth : int;
    quote_mode : bool;
    answer : Assistance_gparser_result.t option;
    length_of_preceding_backslash_wall :int;
};;

let update_backslash_wall_length (m,wlkr)=
if (Assistance_strung.get m.processed_argument wlkr.current_index)='\\'
then (wlkr.length_of_preceding_backslash_wall)+1
else 0;;


let new_walker_in_first_case_in_hwd (m,wlkr)=
   if Assistance_substring.is_a_substring_located_at
     m.quote_opener m.processed_argument wlkr.current_index
   then {
        	current_index =wlkr.current_index+(String.length m.quote_opener);
            current_depth =wlkr.current_depth;
            quote_mode=true;
            answer =None;
            length_of_preceding_backslash_wall=0;
        }
   else
   if Assistance_substring.is_a_substring_located_at m.comment_opener 
        m.processed_argument wlkr.current_index
   then {
        	current_index =wlkr.current_index+(String.length m.comment_opener);
            current_depth =wlkr.current_depth+1;
            quote_mode=false;
            answer =None;
            length_of_preceding_backslash_wall=0;
        }
   else   
   if not(Assistance_substring.is_a_substring_located_at m.comment_closer 
      m.processed_argument wlkr.current_index)
   then {
        	current_index =wlkr.current_index+1;
            current_depth =wlkr.current_depth;
            quote_mode=false;
            answer =None;
            length_of_preceding_backslash_wall=
               update_backslash_wall_length(m,wlkr);
        }
   else    
   if wlkr.current_depth>1
   then {
        	current_index =wlkr.current_index+(String.length m.comment_closer);
            current_depth =wlkr.current_depth-1;
            quote_mode=false;
            answer =None;
            length_of_preceding_backslash_wall=0;
        }
   else  
   let j1=wlkr.current_index+(String.length m.comment_closer) in
   let res=Assistance_gparser_result.veil
               (m.initial_index,j1-1)
               []
               j1
               None        in
        {
        	current_index =wlkr.current_index+(String.length m.comment_closer);
            current_depth =wlkr.current_depth-1;
            quote_mode=false;
            answer =Some(res);
            length_of_preceding_backslash_wall=0;
        };;
   

let first_case_in_hwd 
   (m,wlkr)=(m,new_walker_in_first_case_in_hwd (m,wlkr));;
  
let second_case_in_hwd (m,wlkr)=
  if (Assistance_substring.is_a_substring_located_at m.quote_closer
      m.processed_argument wlkr.current_index)
     &&
     ((wlkr.length_of_preceding_backslash_wall mod 2)=0) 
  then (m,{
        	current_index =wlkr.current_index+(String.length m.quote_closer);
            current_depth =wlkr.current_depth;
            quote_mode=false;
            answer =None;
            length_of_preceding_backslash_wall=0;
        })
  else (m,{
        	current_index =wlkr.current_index+1;
            current_depth =wlkr.current_depth;
            quote_mode=true;
            answer =None;
            length_of_preceding_backslash_wall=
               update_backslash_wall_length(m,wlkr);
        });;

let pusher_for_hwd w=
  let (m,wlkr)=w in
  if wlkr.answer=None
  then 
       (
         if not(wlkr.quote_mode)
         then first_case_in_hwd w
         else second_case_in_hwd w       
        )
  else w;;  
   
let rec iterator_for_main_prsr w=
   let (m,wlkr)=w in
   if wlkr.answer<>None
   then wlkr.answer
   else
   if wlkr.current_index>(String.length m.processed_argument)
   then None
   else iterator_for_main_prsr (pusher_for_hwd w);; 
  
let starter_for_main_prsr 
(comment_opener,comment_closer) 
 (quote_opener,quote_closer) s i=
  (
    {
     comment_opener=comment_opener;
     comment_closer=comment_closer;
     quote_opener=quote_opener;
     quote_closer=quote_closer;
     processed_argument=s;
     initial_index=i;
    }
  ,
    {
      current_index =i+(String.length comment_opener);
      current_depth =1;
      quote_mode=false;
      answer =None;
      length_of_preceding_backslash_wall=0;
    }
  );;
end;;  

let main_prsr
   (comment_opener,comment_closer)
     (quote_opener,quote_closer)=
   let rec tempf=(fun s i->
        if not(Assistance_substring.is_a_substring_located_at comment_opener s i)
        then None 
        else 
          
          Private.iterator_for_main_prsr 
         (Private.starter_for_main_prsr (comment_opener,comment_closer) 
            (quote_opener,quote_closer) s i)
   ) in
   (tempf:Assistance_gparser_fun.t);;      
      
(*

main_prsr ("(*","*)") ("\"","\"") "(* Bye \"*)\" bye bird *)456" 1;;
main_prsr ("(*","*)") ("\"","\"") "(* Bye \"uu\" bye bird *)456" 1;;
main_prsr ("(*","*)") ("\"","\"") "(* Bye \"uu\\\" *) \"  *)234" 1;;


*)



         
   
           

end;;






module Assistance_gparser_apply=struct

(*

#use"GParser/gparser_apply.ml";;

*)

module Private=struct

let enclosure (left_encloser,right_encloser)=
   let tempf=(fun s i1->
   if (not(Assistance_substring.is_a_substring_located_at left_encloser s i1))
   then None
   else 
   let i2=i1+(String.length left_encloser) in
   let i3=Assistance_substring.leftmost_index_of_in_from right_encloser s i2 in
   if i3<1
   then None 
   else
   let i4=i3+(String.length right_encloser)-1 in
   let res= Assistance_gparser_result.veil
               (i1,i4)
               [i2,i3-1]
               (i4+1)
               None in
   Some(res)) in
   (tempf: Assistance_gparser_fun.t);;
   
let constant t=
   let tempf=(fun s i1->
   if (not(Assistance_substring.is_a_substring_located_at t s i1))
   then None
   else 
   let i2=i1+(String.length t) in
   let res= Assistance_gparser_result.veil
               (i1,i2-1)
               []
               i2
               None in
   Some(res)) in
   (tempf: Assistance_gparser_fun.t);;


let footless_constant t=
   let tempf=(fun s i1->
   if (not(Assistance_substring.is_a_substring_located_at t s i1))
   then None
   else 
   let i2=i1+(String.length t) in
   let res= Assistance_gparser_result.veil
               (i1,i2-1)
               []
               (i2-1)
               None in
   Some(res)) in
   (tempf:Assistance_gparser_fun.t);;

let sample_char t=
   let lc=Assistance_strung.explode t in
   let tempf=(fun s i->
        let c=Assistance_strung.get s i in
        if List.mem c lc
        then Some(Assistance_gparser_result.veil
               (i,i)
               []
               (i+1)
               None)
        else None) in
   (tempf:Assistance_gparser_fun.t);;

let sample_neg t=
   let lc=Assistance_strung.explode t in
   let tempf=(fun s i->
        let c=Assistance_strung.get s i in
        if not(List.mem c lc)
        then Some(Assistance_gparser_result.veil
               (i,i)
               []
               (i+1)
               None)
        else None) in
   (tempf:Assistance_gparser_fun.t);;

let sample_star t=
   let lc=Assistance_strung.explode t in
   let tempf=(fun s i1->
        let j=Assistance_strung.char_finder_from (fun c->not(List.mem c lc)) s i1 in
        let better_j=(if j<1 then (String.length s)+1 else j) in
        let res=Assistance_gparser_result.veil
               (i1,better_j-1)
               []
               better_j
               None in
   Some(res)) in
   (tempf:Assistance_gparser_fun.t);;

let sample_negstar t=
   let lc=Assistance_strung.explode t in
   let tempf=(fun s i1->
        let j=Assistance_strung.char_finder_from (fun c->List.mem c lc) s i1 in
        let better_j=(if j<1 then (String.length s)+1 else j) in
        let res=Assistance_gparser_result.veil
               (i1,better_j-1)
               []
               better_j
               None in
   Some(res)) in
   (tempf:Assistance_gparser_fun.t);;

let sample_plus t=
   let lc=Assistance_strung.explode t in
   let tempf=(fun s i1->
        if i1>(String.length s) then None else
        if (not(List.mem (Assistance_strung.get s i1 ) lc)) then None else
        let j=Assistance_strung.char_finder_from (fun c->not(List.mem c lc)) s i1 in
        let better_j=(if j<1 then (String.length s)+1 else j) in
        let res=Assistance_gparser_result.veil
               (i1,better_j-1)
               []
               better_j
               None in
   Some(res)) in
   (tempf:Assistance_gparser_fun.t);;
   

let race (continuer,finalizer)=
   let rec tempf=(fun (s,i1,k)->
        if k>(String.length s)
        then None
        else
        if Assistance_substring.is_a_substring_located_at continuer s k
        then tempf(s,i1,k+(String.length continuer))
        else
        if (not(Assistance_substring.is_a_substring_located_at finalizer s k))
        then tempf(s,i1,k+1)
        else
        let j1=k+(String.length finalizer) in
        let res=Assistance_gparser_result.veil
               (i1,j1-1)
               []
               (j1-1)
               None in
        Some(res)) in
   ((fun s i->tempf(s,i,i)):Assistance_gparser_fun.t);;   
      
let house_with_doors=Assistance_gparser_house_with_doors.hwd;;


type chain_artefact=
     Usual of (int * int) list * Assistance_gparser_fun.t list * bytes * int * int 
    |Result_found of Assistance_gparser_result.t
    |Failure_found;;

let chain l=
  let main_f=
  	(fun s i->
   		let rec tempf=
   		(
         	fun (imp_ranges,da_ober,s,i0,k)->
      		match da_ober with
      		[]->Some(
           		    	Assistance_gparser_result.veil
               			(i0,k-1)
               			imp_ranges
               			k
               			None
          			)
       		|prsr::rest->   
         		(
           			match prsr s k with
            		None->None
           		  |Some(res)->tempf(
           		       imp_ranges@(Assistance_gparser_result.important_ranges res),
                       rest,s,i0,Assistance_gparser_result.final_cursor_position res)
                )
         )  
    in tempf([],l,s,i,i)
    ) in
  (main_f:Assistance_gparser_fun.t);;

let detailed_chain l=
  let main_f=
  	(fun s i->
   		let rec tempf=
   		(
         	fun (imp_ranges,da_ober,s,i0,k)->
      		match da_ober with
      		[]->Some(
           		    	Assistance_gparser_result.veil
               			(i0,k-1)
               			(List.rev imp_ranges)
               			k
               			None
          			)
       		|prsr::rest->   
         		(
           			match prsr s k with
            		None->None
           		  |Some(res)->tempf(
           		       (Assistance_gparser_result.whole_range res)::imp_ranges,
                       rest,s,i0,Assistance_gparser_result.final_cursor_position res)
                )
         )  
    in tempf([],l,s,i,i)
    ) in
  (main_f:Assistance_gparser_fun.t);;

let debugful_detailed_chain l=
  let main_f=
  	(fun s i->
   		let rec tempf=
   		(
         	fun (imp_ranges,da_ober,s,i0,k,opt)->
      		match da_ober with
      		[]->let sol=Some(
           		    	Assistance_gparser_result.veil
               			(i0,k-1)
               			(List.rev imp_ranges)
               			k
               			None
          			) in
          	     (imp_ranges,da_ober,s,i0,k,sol) 		
       		|prsr::rest->   
         		(
           			match prsr s k with
            		None->(imp_ranges,da_ober,s,i0,k,opt)
           		  |Some(res)->tempf(
           		       (Assistance_gparser_result.whole_range res)::imp_ranges,
                       rest,s,i0,Assistance_gparser_result.final_cursor_position res,None)
                )
         )  
    in tempf([],l,s,i,i,None)
    ) in
  main_f;;

let disjunction l=
   let indexed_l=Assistance_ennig.index_everything l in   
   let rec tempf=(fun
   (da_ober,s,i0)->
      match da_ober with
      []->None 
      |(j,prsr)::rest->
         (
           match prsr s i0 with
             None->tempf(rest,s,i0)
           |Some(res)->
          Some(
             Assistance_gparser_result.veil
               (Assistance_gparser_result.whole_range res)
               (Assistance_gparser_result.important_ranges res)
               (Assistance_gparser_result.final_cursor_position res)
               (Some j)
           )
         )   
   ) in
   ((fun s i->tempf (indexed_l,s,i)):Assistance_gparser_fun.t);;

let star prsr=
   let rec tempf=(fun
   (imp_ranges,s,i0,k)->
      match prsr s k with
       None->Some(
             Assistance_gparser_result.veil
               (i0,k-1)
               (imp_ranges)
               k
               None
            )
      |Some(res)->tempf(imp_ranges@(Assistance_gparser_result.important_ranges res),
                       s,i0,Assistance_gparser_result.final_cursor_position res)
   
   ) in
   ((fun s i->tempf ([],s,i,i)):Assistance_gparser_fun.t);;

let detailed_star prsr=
   let rec tempf=(fun
   (imp_ranges,s,i0,k)->
      match prsr s k with
       None->Some(
             Assistance_gparser_result.veil
               (i0,k-1)
               (List.rev(imp_ranges))
               k
               None
            )
      |Some(res)->tempf((Assistance_gparser_result.whole_range res)::imp_ranges,
                       s,i0,Assistance_gparser_result.final_cursor_position res)
   
   ) in
   ((fun s i->tempf ([],s,i,i)):Assistance_gparser_fun.t);;   
   
   
let one_or_more prsr=chain [prsr;star prsr];;

let optional prsr=
   let rec tempf=(fun s i->
      match prsr s i with
       Some(res)->Some(
            Assistance_gparser_result.veil
               (Assistance_gparser_result.whole_range res)
               (Assistance_gparser_result.important_ranges res)
               (Assistance_gparser_result.final_cursor_position res)
               None
            )
      |None->Some(
            Assistance_gparser_result.veil
               (i,i-1)
               []
               i
               None
            )
   
   ) in
   (tempf:Assistance_gparser_fun.t);;


let recoiling_ending x y=
   let tempf=(fun s i->
      match x s i with
       None->None
      |Some(res)->
                  
                  let j=Assistance_gparser_result.final_cursor_position res in
                  if y s j=None then None else
                  Some(
                  Assistance_gparser_result.veil
                  (i,j-1)
                  (Assistance_gparser_result.important_ranges res)
                  j
                  None
                  )
   ) in
   (tempf:Assistance_gparser_fun.t);;
     
let rec apply=function        
     Assistance_gparser.Constant(s)->constant s
    |Assistance_gparser.Enclosure(s1,s2)->enclosure (s1,s2)
    |Assistance_gparser.Footless_constant(s)->footless_constant s
    |Assistance_gparser.Sample_char(s)->sample_char s
    |Assistance_gparser.Sample_neg(s)->sample_neg s
    |Assistance_gparser.Sample_star(s)->sample_star s
    |Assistance_gparser.Sample_negstar(s)->sample_negstar s
    |Assistance_gparser.Sample_plus(s)->sample_plus s
    |Assistance_gparser.Race(s1,s2)->race(s1,s2)
    |Assistance_gparser.Comment(s1,s2,s3,s4)->Assistance_gparser_ocaml_comment.main_prsr(s1,s2)(s3,s4)
    |Assistance_gparser.House_with_doors(s1,s2,l)->house_with_doors (s1,s2) l
    |Assistance_gparser.Chain(l)->chain(Assistance_image.image apply l)
    |Assistance_gparser.Disjunction(l)->disjunction(Assistance_image.image apply l)
    |Assistance_gparser.Star(x)->star(apply x)
    |Assistance_gparser.Detailed_star(x)->detailed_star(apply x)
    |Assistance_gparser.One_or_more(x)->one_or_more(apply x)
    |Assistance_gparser.Optional(x)->optional(apply x)
    |Assistance_gparser.Recoiling_ending(x,y)->recoiling_ending (apply x) (apply y)
    |Assistance_gparser.Detailed_chain(l)->detailed_chain(Assistance_image.image apply l);;
   
end;;   
   
let apply=Private.apply;;   
   
(*


*)   
   
           

end;;






module Assistance_list_with_indices=struct

(*

#use"list_with_indices.ml";;

*)

exception Bad_set_of_indices;;

let list_with_indices l=
  let n=List.length l in
  let temp1=Assistance_ennig.doyle (fun i->Assistance_option.seek(fun p->fst(p)=i) l) 1 n in
  if List.mem None temp1
  then raise(Bad_set_of_indices)
  else
  Assistance_ennig.doyle (fun
     i->snd(Assistance_listennou.force_find(fun p->fst(p)=i) l)
  ) 1 n;;

(*

list_with_indices [3,"a";1,"b";2,"c"];;

*)  
           

end;;






module Assistance_gparser_for_ocaml_language=struct

(*

#use"GParser/gparser_for_ocaml_language.ml";;

*)

let double_semicolon=Assistance_particular_string.double_semicolon;;

let prsr_for_comment=
  Assistance_gparser.Comment ("(*","*)","\"","\"");;


let prsr_for_sharp_comment=Assistance_gparser.Enclosure ("\n#","\n");;

let prsr_for_space=Assistance_gparser.Constant " ";;
let prsr_for_tab=Assistance_gparser.Constant "\t";;


let prsr_for_space_or_tab=Assistance_gparser.Disjunction [prsr_for_space;prsr_for_tab];;
let prsr_for_linebreak=Assistance_gparser.Constant "\n";;
let prsr_for_newline=Assistance_gparser.Constant "\012";;
let prsr_for_windows_newline=Assistance_gparser.Constant "\r";;
let prsr_for_individual_white=Assistance_gparser.Disjunction 
[prsr_for_space;prsr_for_tab;prsr_for_linebreak;prsr_for_newline;prsr_for_windows_newline];;

let prsr_for_inline_white_maybe=Assistance_gparser.Star prsr_for_space_or_tab;;
let prsr_for_white_maybe=Assistance_gparser.Star prsr_for_individual_white;;
let prsr_for_white=Assistance_gparser.One_or_more prsr_for_individual_white;;

let prsr_for_special_sharp=Assistance_gparser.Chain
   [
     Assistance_gparser.Constant "#";
     prsr_for_inline_white_maybe;
     Assistance_gparser.Sample_star "0123456789";
     prsr_for_inline_white_maybe;
     Assistance_gparser.Constant "\"";
     Assistance_gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ/.";
     Assistance_gparser.Constant "\"";
     prsr_for_inline_white_maybe;
   ];;

let prsr_for_uncapitalized_word=Assistance_gparser.Chain
   [
     Assistance_gparser.Sample_char "abcdefghijklmnopqrstuvwxyz_";
     Assistance_gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
   ];;

let prsr_for_capitalized_word=Assistance_gparser.Chain
   [
     Assistance_gparser.Sample_char "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
     Assistance_gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ012356789";
   ];;

let prsr_for_pointing_module=Assistance_gparser.Chain
   [
     prsr_for_capitalized_word;
     Assistance_gparser.Constant ".";
   ];;

let prsr_for_wholly_lowercase_name=
   Assistance_gparser.Chain
   [
     Assistance_gparser.Sample_char "abcdefghijklmnopqrstuvwxyz_";
     Assistance_gparser.Sample_star "abcdefghijklmnopqrstuvwxyz_";
   ];;


let prsr_for_element_in_uple_in_typedef=
   Assistance_gparser.Chain
   [
     Assistance_gparser.Constant "'";
      prsr_for_uncapitalized_word; 
     prsr_for_white_maybe; 
     Assistance_gparser.Constant ",";
     prsr_for_white_maybe; 
   ];;

let prsr_for_parameters1_in_type=
   Assistance_gparser.Chain
   [
     Assistance_gparser.Constant "'";
      prsr_for_uncapitalized_word; 
     prsr_for_white_maybe; 
   ];;

let prsr_for_parameters2_in_type=
   Assistance_gparser.Chain
   [
     Assistance_gparser.Constant "(";
     prsr_for_white_maybe; 
     Assistance_gparser.Star(prsr_for_element_in_uple_in_typedef);
     prsr_for_white_maybe; 
     Assistance_gparser.Constant "'";
     prsr_for_uncapitalized_word; 
     prsr_for_white_maybe; 
     Assistance_gparser.Constant ")";
     prsr_for_white_maybe; 
   ];;

   

let prsr_for_parameters_in_type=
   Assistance_gparser.Disjunction
   [
     prsr_for_parameters1_in_type;
     prsr_for_parameters2_in_type;
   ];;

let prsr_for_rec_followed_by_white=Assistance_gparser.Chain
   [
     Assistance_gparser.Optional(Assistance_gparser.Constant "rec");
     prsr_for_white;
   ];;
  

module Private=struct
let list_for_value_making=
   [
     Assistance_gparser.Constant "let";
     prsr_for_white;
     Assistance_gparser.Optional(prsr_for_rec_followed_by_white);
     prsr_for_uncapitalized_word;
     prsr_for_white_maybe;
     Assistance_gparser.Enclosure ("","=");
     Assistance_gparser.Enclosure ("",double_semicolon);
   ];;
  
end;;

let index_for_name_in_value_parser=Assistance_listennou.find_index
   prsr_for_uncapitalized_word Private.list_for_value_making;;

let index_for_content_in_value_parser=Assistance_listennou.find_index
   (Assistance_gparser.Enclosure ("",double_semicolon)) Private.list_for_value_making;; 
   

let prsr_for_value_making=Assistance_gparser.Detailed_chain
   Private.list_for_value_making;;

let prsr_for_type_making=Assistance_gparser.Detailed_chain
   [
     Assistance_gparser.Constant "type";
     prsr_for_white;
     Assistance_gparser.Optional(prsr_for_parameters_in_type);
     prsr_for_uncapitalized_word;
     prsr_for_white_maybe;
     Assistance_gparser.Enclosure ("","=");
     Assistance_gparser.Enclosure ("",double_semicolon);
   ];;



let prsr_for_exception_making=Assistance_gparser.Detailed_chain
     [
     Assistance_gparser.Constant "exception";
     prsr_for_white;
     prsr_for_capitalized_word;
     Assistance_gparser.Enclosure ("",double_semicolon);
   ];;

let prsr_for_module_opener=
   Assistance_gparser.Detailed_chain
   [
     Assistance_gparser.Constant "module";
     prsr_for_white;
     prsr_for_capitalized_word;
     prsr_for_white_maybe;
     Assistance_gparser.Constant "=";
     prsr_for_white_maybe;
     Assistance_gparser.Constant "struct";
   ];;

let prsr_for_module_closer=
   Assistance_gparser.Chain
   [
     Assistance_gparser.Constant "end";
     prsr_for_white_maybe;
     Assistance_gparser.Constant double_semicolon;
   ];;

let prsr_for_module_inclusion=
   Assistance_gparser.Detailed_chain
   [
     Assistance_gparser.Constant "include ";
     prsr_for_white_maybe;
     prsr_for_capitalized_word;
     prsr_for_white_maybe;
     Assistance_gparser.Constant double_semicolon;
   ];;

let prsr_for_special_names=
   Assistance_gparser.Disjunction
     [
       Assistance_gparser.Constant "add_to_vvv ";
       Assistance_gparser.Constant "add_data ";
       Assistance_gparser.Constant "add_data\n";
       Assistance_gparser.Constant "add_label ";
       Assistance_gparser.Constant "add_recognizer ";
       Assistance_gparser.Constant "add_shortcut ";
       Assistance_gparser.Constant "define_precedence_set ";
       Assistance_gparser.Constant "get_name_for_set ";
     ];;   
   
let prsr_for_specialities=Assistance_gparser.Chain
   [
     prsr_for_special_names;
     Assistance_gparser.Enclosure ("",double_semicolon);
   ];;   

let index_for_value=1;;
let index_for_type=2;;
let index_for_exception=3;;
let index_for_comment=4;;
let index_for_sharp_comment=5;;
let index_for_special_sharp=6;;
let index_for_module_opener=7;;
let index_for_module_closer=8;;
let index_for_module_inclusion=9;;
let index_for_specialities=10;;
let index_for_white=11;;


let elt_prsr=Assistance_gparser.Disjunction 
  (
     Assistance_list_with_indices.list_with_indices
     [
       index_for_value           ,prsr_for_value_making;
       index_for_type            ,prsr_for_type_making;
       index_for_exception       ,prsr_for_exception_making;
       index_for_comment         ,prsr_for_comment;
       index_for_sharp_comment   ,prsr_for_sharp_comment;
       index_for_special_sharp   ,prsr_for_special_sharp;
       index_for_module_opener   ,prsr_for_module_opener;
       index_for_module_closer   ,prsr_for_module_closer;
       index_for_module_inclusion,prsr_for_module_inclusion;
       index_for_specialities    ,prsr_for_specialities;
       index_for_white           ,prsr_for_white;
     ]
   )
;;


let main_prsr=
   Assistance_gparser.Detailed_star elt_prsr;;



   
           

end;;






module Assistance_pre_read_ocaml_files=struct

(*

#use"Ocaml_analysis/pre_read_ocaml_files.ml";;

Originated as the code shared by modules
read_ocaml_files and read_ocaml_files_without_expanding_inclusions.

*)

exception Pre_read_exn of string;;

module Private=struct
  exception Unreadable of string;;
  
  let accuse_final_excerpt s i=
    let j=min(String.length s)(i+100) in
    raise(Unreadable(Assistance_cull_string.interval s i j));;
  
  let uncatched_read1 s=
    let opt=Assistance_gparser_apply.apply Assistance_gparser_for_ocaml_language.main_prsr s 1 in
    if opt=None then accuse_final_excerpt s 1 else
    let res=Assistance_option.unpack opt in 
    let p=Assistance_gparser_result.final_cursor_position res in
    if p<=(String.length s) 
    then accuse_final_excerpt s p
    else 
    let temp1=Assistance_gparser_result.important_ranges res in
    Assistance_image.image (fun (i,j)->
      let opt=Assistance_gparser_apply.apply Assistance_gparser_for_ocaml_language.elt_prsr s i in
      let res=Assistance_option.unpack opt in
      ((i,j),Assistance_option.unpack(Assistance_gparser_result.disjunction_index res))
    ) temp1;;
  
  exception Read1_exn of string;;
  
  let read1 s= try uncatched_read1 s with Unreadable(t)->raise(Read1_exn(t));;
    
  let describe_value_item s (i,j)=
       let opt=Assistance_gparser_apply.apply Assistance_gparser_for_ocaml_language.prsr_for_value_making s i in
       let res=Assistance_option.unpack opt in
       let (i1,j1)=List.nth(Assistance_gparser_result.important_ranges res) 
            (Assistance_gparser_for_ocaml_language.index_for_name_in_value_parser-1)
       and (i2,j2)=List.nth(Assistance_gparser_result.important_ranges res) 
            (Assistance_gparser_for_ocaml_language.index_for_content_in_value_parser-1) 
       and (i3,j3)=Assistance_gparser_result.whole_range res in
         Assistance_ocaml_gsyntax_item.make
            Assistance_ocaml_gsyntax_category.Value
            (Assistance_cull_string.interval s i1 j1)
            (i1,j1)
            (Assistance_cull_string.interval s i3 j3)
            (* the -2 of because of the 2 characters in the double semicolon *)
            (Assistance_cull_string.interval s i2 (j2-2))
            (i2,j2-2)
            false;;
  
  let describe_type_item s (i,j)=
       let opt=Assistance_gparser_apply.apply Assistance_gparser_for_ocaml_language.prsr_for_type_making s i in
       let res=Assistance_option.unpack opt in
       let (i1,j1)=List.nth(Assistance_gparser_result.important_ranges res) 3
       and (i2,j2)=List.nth(Assistance_gparser_result.important_ranges res) 6 
       and (i3,j3)=Assistance_gparser_result.whole_range res in
         Assistance_ocaml_gsyntax_item.make
            Assistance_ocaml_gsyntax_category.Type
            (Assistance_cull_string.interval s i1 j1)
            (i1,j1)
            (Assistance_cull_string.interval s i3 j3)
            (* the -2 of because of the 2 characters in the double semicolon *)
            (Assistance_cull_string.interval s i2 (j2-2))
            (i2,j2-2)
            false;;
  
  let describe_exception_item s (i,j)=
       let opt=Assistance_gparser_apply.apply Assistance_gparser_for_ocaml_language.prsr_for_exception_making s i in
       let res=Assistance_option.unpack opt in
       let (i1,j1)=List.nth(Assistance_gparser_result.important_ranges res) 2
       and (i2,j2)=List.nth(Assistance_gparser_result.important_ranges res) 3 
       and (i3,j3)=Assistance_gparser_result.whole_range res in
         Assistance_ocaml_gsyntax_item.make
            Assistance_ocaml_gsyntax_category.Exception
            (Assistance_cull_string.interval s i1 j1)
            (i1,j1)
            (Assistance_cull_string.interval s i3 j3)
            (* the -2 of because of the 2 characters in the double semicolon *)
            (Assistance_cull_string.interval s i2 (j2-2))
            (i2,j2-2)
            false;;
  
  let describe_module_opener_item s (i,j)=
       let opt=Assistance_gparser_apply.apply Assistance_gparser_for_ocaml_language.prsr_for_module_opener s i in
       let res=Assistance_option.unpack opt in
       let (i1,j1)=List.nth(Assistance_gparser_result.important_ranges res) 2
       and (i3,j3)=Assistance_gparser_result.whole_range res in 
         Assistance_ocaml_gsyntax_item.make
            Assistance_ocaml_gsyntax_category.Module_opener
            (Assistance_cull_string.interval s i1 j1)
            (i1,j1)
            (Assistance_cull_string.interval s i3 j3)
            ""
            (0,0)
            false;;
  
  
  let describe_module_closer_item=
         Assistance_ocaml_gsyntax_item.make
            Assistance_ocaml_gsyntax_category.Module_closer
            ""
            (0,0)
            ""
            ""
            (0,0)
            false;;
  
  
  let describe_module_inclusion_item s (i,j)=
       let opt=Assistance_gparser_apply.apply Assistance_gparser_for_ocaml_language.prsr_for_module_inclusion s i in
       let res=Assistance_option.unpack opt in
       let (i1,j1)=List.nth(Assistance_gparser_result.important_ranges res) 2 
       and (i3,j3)=Assistance_gparser_result.whole_range res in 
         Assistance_ocaml_gsyntax_item.make
            Assistance_ocaml_gsyntax_category.Module_inclusion
            (Assistance_cull_string.interval s i1 j1)
            (i1,j1)
            (Assistance_cull_string.interval s i3 j3)
            ""
            (0,0)
            false;;
            
   let describe_item s ((i,j),idx)=
     if idx=Assistance_gparser_for_ocaml_language.index_for_value
     then Some(describe_value_item s (i,j))
     else
     if idx=Assistance_gparser_for_ocaml_language.index_for_type
     then Some(describe_type_item s (i,j))
     else
     if idx=Assistance_gparser_for_ocaml_language.index_for_exception
     then Some(describe_exception_item s (i,j))
     else
     if idx=Assistance_gparser_for_ocaml_language.index_for_module_opener
     then Some(describe_module_opener_item s (i,j))
     else
     if idx=Assistance_gparser_for_ocaml_language.index_for_module_closer
     then Some(describe_module_closer_item)
     else          
     if idx=Assistance_gparser_for_ocaml_language.index_for_module_inclusion
     then Some(describe_module_inclusion_item s (i,j))
     else None;;
     
  let uncatched_read2 s=
     Assistance_option.filter_and_unpack (describe_item s) (read1 s);;   
     
  
  
  let pre_read s= try uncatched_read2 s with Read1_exn(t)->raise(Pre_read_exn(t));;
  end;;

  let pre_read =Private.pre_read;;           

end;;






module Assistance_read_ocaml_files=struct

(*

#use"Ocaml_analysis/read_ocaml_files.ml";;

*)

module Private=struct
  

  
  let module_inclusion_in_pusher    
     (graet,current_full_scope,current_names) x=
      let included_module=x.Assistance_ocaml_gsyntax_item.name in
          let full_scope=current_full_scope^"."^included_module in
          let maybe_included_items=List.filter(
             fun y->let nm_y=y.Assistance_ocaml_gsyntax_item.name in
             (Assistance_supstring.begins_with nm_y full_scope)
             ||
             (Assistance_supstring.begins_with nm_y included_module)  
          ) graet in 
          (* local redifinition has priority over an outside definition *)
          let chosen_scope=(if
            List.exists(fun y->
              y.Assistance_ocaml_gsyntax_item.name=included_module
            ) maybe_included_items
            then included_module
            else full_scope
          ) in
           let included_items=List.filter(
             fun y->y.Assistance_ocaml_gsyntax_item.name=chosen_scope
           ) maybe_included_items in
           let renamed_included_items=Assistance_image.image 
           (Assistance_ocaml_gsyntax_item.include_in_new_scope full_scope )
           included_items in
           (List.rev_append renamed_included_items graet,current_full_scope,current_names);;
     
  let first_pusher_for_modulename_prepension_and_inclusion_expansion  
     walker_state x=
     let (graet,current_full_scope,current_names)=walker_state in
    match x.Assistance_ocaml_gsyntax_item.category with
      Assistance_ocaml_gsyntax_category.Value                                                                          
    | Assistance_ocaml_gsyntax_category.Type
    | Assistance_ocaml_gsyntax_category.Exception->
            let new_x=Assistance_ocaml_gsyntax_item.prepend_prefix current_full_scope x in
            (new_x::graet,current_full_scope,current_names)
    | Assistance_ocaml_gsyntax_category.Module_opener->
            let new_name=x.Assistance_ocaml_gsyntax_item.name in
            let new_names=current_names@[new_name] in
            let new_full_scope=String.concat "." new_names in
            (graet,new_full_scope,new_names)
    | Assistance_ocaml_gsyntax_category.Module_closer->
            let new_names=List.rev(List.tl(List.rev(current_names))) in
            let new_full_scope=String.concat "." new_names in
            (graet,new_full_scope,new_names)
    | Assistance_ocaml_gsyntax_category.Module_inclusion->
           module_inclusion_in_pusher (graet,current_full_scope,current_names) x;;
  
  exception Pusher23_exn;;
  
  let pusher_for_modulename_prepension_and_inclusion_expansion (walker_state,da_ober)=
     match da_ober with
     []->raise(Pusher23_exn)
     |x::peurrest->(first_pusher_for_modulename_prepension_and_inclusion_expansion 
     walker_state x,peurrest);;    
  
           
  let rec iterator_for_modulename_prepension_and_inclusion_expansion (walker_state,da_ober)=
     if da_ober=[] 
     then let  (graet,_,_)=walker_state in List.rev graet
     else iterator_for_modulename_prepension_and_inclusion_expansion(
       pusher_for_modulename_prepension_and_inclusion_expansion (walker_state,da_ober));; 
  
  
  let prepend_modulenames_and_expand_inclusions data_before (current_module,l)=
      iterator_for_modulename_prepension_and_inclusion_expansion 
        ((data_before,current_module,String.split_on_char '.' current_module),l);;
  
  end;;
  
  exception Reading_error of Assistance_absolute_path.t * string;;
  
  let read_ocaml_files l_ap=
     let temp1=Assistance_image.image( fun ap->
     let s_ap=Assistance_absolute_path.to_string ap
     and text=Assistance_io.read_whole_file ap in
     let unpointed=Assistance_cull_string.before_rightmost s_ap '.' in
     let module_name=String.capitalize_ascii (Assistance_cull_string.after_rightmost unpointed '/') in
     try (module_name,Assistance_pre_read_ocaml_files.pre_read text)  with
     Assistance_pre_read_ocaml_files.Pre_read_exn(t)->raise(Reading_error(ap,t)) 
     ) l_ap in 
     List.fold_left Private.prepend_modulenames_and_expand_inclusions [] temp1;;
     
     
  (*
  
  let g1=German_wrapper.data();;
  let g2=List.filter Modulesystem_data.ml_present g1;;
  let g3=List.flatten (image Modulesystem_data.acolytes g2);;
  let g4=List.filter (fun mlx->snd(Mlx_filename.decompose mlx)=Ocaml_ending.ml) g3;;
  let g5=image Mlx_filename.to_absolute_path g4;;
  
  let g6=read3 g5;;
  
  
  let g6=image (fun ap->let s=Io.read_whole_file ap in
    (-(String.length s),(ap,s))
  ) g5 ;;
  let g7=image snd (ofo(Tidel2.diforchan g6));;
  let g8=Explicit.image (fun (ap,s)->(ap,read2 s)) g7;;
  let g9=Explicit.image (fun (ap,l)->
    from_level2_to_level3 ([],"Moody") l
  ) g8;;
  
  *)
  
    
  (*  
  
  let s1="let jiving=234  ;;";;
  describe_value_item s1 (1,String.length s1);;
  
  let s2="type ('a,'b) sister=('a list)*'b*string;;";;
  describe_type_item s2 (1,String.length s2);;
  
  let s3="type sister=(int list)*float*string;;";;
  describe_type_item s3 (1,String.length s3);;
  
  let s4="exception Foobar of string*int;;";;
  describe_exception_item s4 (1,String.length s4);;
  
  let s5="exception Foobar;;";;
  describe_exception_item s5 (1,String.length s5);;
  
  let s6="module  Foobar=struct";;
  describe_module_opener_item s6 (1,String.length s6);;
  
  let s7="end\n;;";;
  describe_module_opener_item s7 (1,String.length s7);;
  
  let s8="include Leap\n;;";;
  describe_module_inclusion_item s8 (1,String.length s8);;
     
  *)   
     
       
                

end;;






module Assistance_rename_moduled_value_in_file=struct

(*

#use"Ocaml_analysis/rename_moduled_value_in_file.ml";;

*)

exception No_module_given of string;;
exception No_value_with_name of string;;

let rename_moduled_value_in_file preceding_files old_name new_name path=
   let j=Assistance_substring.leftmost_index_of_in "." old_name in
   if j<0 
   then raise(No_module_given(old_name))
   else 
   let module_name=Assistance_cull_string.beginning (j-1) old_name in
   let temp3=Assistance_read_ocaml_files.read_ocaml_files preceding_files in
   let opt_temp4=Assistance_option.seek (fun itm->
     (itm.Assistance_ocaml_gsyntax_item.name)=old_name
   ) temp3 in
   if opt_temp4=None
   then raise(No_value_with_name(old_name))
   else
   let temp4=Assistance_option.unpack(opt_temp4) in
   let (i1,j1)=temp4.Assistance_ocaml_gsyntax_item.interval_for_name in
   let _=Assistance_overwrite_at_intervals.inside_file [(i1,j1),new_name] path in
   let temp3_again=Assistance_read_ocaml_files.read_ocaml_files preceding_files in
   let beheaded_name=Assistance_cull_string.cobeginning j old_name in
   let s_new_beheaded_name=(fun (fa,nn)->if fa="" then nn else fa^"."^nn)
   (Assistance_cull_string.before_rightmost beheaded_name '.',Assistance_overwriter.to_string new_name) in
   let new_beheaded_name=Assistance_overwriter.of_string s_new_beheaded_name in
   let s_new_full_name=module_name^"."^s_new_beheaded_name in
   let temp4_again=Assistance_listennou.force_find (fun itm->
     (itm.Assistance_ocaml_gsyntax_item.name)=s_new_full_name
   ) temp3_again in
   let k1=Assistance_listennou.find_index temp4_again temp3_again in
   let temp5=Assistance_listennou.big_tail k1 temp3_again in
   let temp6=Assistance_option.filter_and_unpack(
      fun itm->
        let txt=itm.Assistance_ocaml_gsyntax_item.content in
        let ttemp7=Assistance_isolated_occurrences.of_in 
           beheaded_name txt in
        if ttemp7<>[]
        then  let isoc=Assistance_isolated_occurrences.of_in beheaded_name txt in
              let replacings=Assistance_image.image (fun p->(p,new_beheaded_name)) isoc in
              let new_txt=Assistance_overwrite_at_intervals.inside_string
                   replacings txt in
             Some(itm.Assistance_ocaml_gsyntax_item.interval_for_content,
                  Assistance_overwriter.of_string new_txt)
        else None   
   ) temp5 in
   Assistance_overwrite_at_intervals.inside_file temp6 path;;


end;;






module Assistance_fw_wrapper=struct

(*

#use"Filewatching/fw_wrapper.ml";;

*)

exception Register_rootless_path_exn of string list;;

module Private = struct

let mtime file = string_of_float((Unix.stat file).Unix.st_mtime) ;;

let recompute_mtime fw path =
     let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_wrapper_automatic.root fw) 
     and s_path=Assistance_dfn_rootless.to_line path in 
     let file = s_root^s_path in 
     mtime file;;


let recompute_all_info fw path =
     let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_wrapper_automatic.root fw) 
     and s_path=Assistance_dfn_rootless.to_line path in 
     let file = s_root^s_path in 
     (path,mtime file);;

let update_in_list_of_pairs fw  to_be_updated pairs  =
   Assistance_image.image (
      fun pair -> 
        let (rootless,mtime)=pair in 
        if List.mem rootless to_be_updated 
        then recompute_all_info fw rootless 
        else pair
   ) pairs;;

let update_some_files fw (w_files,sw_files) = {
    fw with 
      Assistance_fw_wrapper_t.compilable_files = update_in_list_of_pairs fw w_files (fw.Assistance_fw_wrapper_t.compilable_files) ;
      noncompilable_files = update_in_list_of_pairs fw sw_files (fw.Assistance_fw_wrapper_t.noncompilable_files) ;
} ;;

let remove_nonnoncompilable_files fw rootless_paths =
    let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_wrapper_automatic.root fw) in 
    let removals_to_be_made = Assistance_image.image (
      fun path->" rm -f "^s_root^(Assistance_dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Assistance_unix_command.conditional_multiple_uc removals_to_be_made in 
   {
      fw with 
      Assistance_fw_wrapper_t.compilable_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Assistance_fw_wrapper_t.compilable_files)  
   };;

let remove_noncompilable_files fw rootless_paths=
    let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_wrapper_automatic.root fw) in 
    let removals_to_be_made = Assistance_image.image (
      fun path->" rm -f "^s_root^(Assistance_dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Assistance_unix_command.conditional_multiple_uc removals_to_be_made in 
   {
      fw with 
      Assistance_fw_wrapper_t.noncompilable_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Assistance_fw_wrapper_t.noncompilable_files)  
   };;

let remove_files fw rootless_paths=
    let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_wrapper_automatic.root fw) in 
    let removals_to_be_made = Assistance_image.image (
      fun path->" rm -f "^s_root^(Assistance_dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Assistance_unix_command.conditional_multiple_uc removals_to_be_made in 
    let fw2 ={
      fw with 
      Assistance_fw_wrapper_t.compilable_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Assistance_fw_wrapper_t.compilable_files)  ;
      Assistance_fw_wrapper_t.noncompilable_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Assistance_fw_wrapper_t.noncompilable_files)  
   } in 
   Assistance_fw_wrapper_automatic.reflect_destructions_in_diff fw2 rootless_paths ;;



let forget_modules fw mod_names =
   let the_files = Assistance_option.filter_and_unpack (
      fun (path,_)-> 
        if List.mem (Assistance_dfn_rootless.to_module path) mod_names 
        then Some path
        else None
   ) fw.Assistance_fw_wrapper_t.compilable_files in 
   remove_files fw the_files;;

let register_rootless_paths fw rootless_paths= 
   let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_wrapper_automatic.root fw) in
   let bad_paths = Assistance_option.filter_and_unpack (
     fun rp-> let s_full_path = s_root^(Assistance_dfn_rootless.to_line rp)  in 
     if not(Sys.file_exists s_full_path)
     then Some(s_full_path)
     else None
   ) rootless_paths in 
   if bad_paths<>[]
   then raise(Register_rootless_path_exn(bad_paths))
   else
   let (c_paths,nc_paths) = List.partition Assistance_dfn_rootless.is_compilable rootless_paths in 
   let fw2=  {
      fw with 
      Assistance_fw_wrapper_t.compilable_files =  
        (fw.Assistance_fw_wrapper_t.compilable_files)@
          (Assistance_image.image (recompute_all_info fw) c_paths)  ;
      Assistance_fw_wrapper_t.noncompilable_files =  
        (fw.Assistance_fw_wrapper_t.noncompilable_files)@
         (Assistance_image.image (recompute_all_info fw) nc_paths)  
    }  in 
    (Assistance_fw_wrapper_automatic.reflect_creations_in_diff fw2 rootless_paths,(c_paths,nc_paths));;

let deal_with_initial_comment_if_needed fw rless =
   if (Assistance_dfn_rootless.to_ending rless)<> Assistance_dfa_ending.ml 
   then ()
   else
      let root = Assistance_fw_wrapper_automatic.root fw in 
      let full = Assistance_dfn_join.root_to_rootless root rless in 
      let ap = Assistance_dfn_full.to_absolute_path full in 
      Assistance_put_use_directive_in_initial_comment.put_usual root ap
   ;;    

let relocate_compilable_files_to fw rootless_paths new_subdir=
    let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_wrapper_automatic.root fw) 
    and s_subdir = Assistance_dfa_subdirectory.connectable_to_subpath new_subdir in 
    let displacements_to_be_made = Assistance_image.image (
      fun path->" mv "^s_root^(Assistance_dfn_rootless.to_line path)^" "^
      s_root^s_subdir 
    ) rootless_paths in 
    let _=Assistance_unix_command.conditional_multiple_uc displacements_to_be_made in 
    let reps = Assistance_image.image (fun path->
      (path,Assistance_dfn_rootless.relocate_to path new_subdir)
    ) rootless_paths in 
    let fw2 = {
      fw with 
      Assistance_fw_wrapper_t.compilable_files = Assistance_image.image (fun pair->
         let (path,_)=pair in 
         if(List.mem path rootless_paths) 
         then let new_path = Assistance_dfn_rootless.relocate_to path new_subdir in 
              let _ = deal_with_initial_comment_if_needed fw new_path in 
              (new_path,recompute_mtime fw new_path)
         else pair
      ) (fw.Assistance_fw_wrapper_t.compilable_files)  
   } in 
   Assistance_fw_wrapper_automatic.reflect_replacements_in_diff fw2 reps ;;

let relocate_module_to fw mod_name new_subdir=
   let the_files = Assistance_option.filter_and_unpack (
      fun (path,_)-> 
        if (Assistance_dfn_rootless.to_module path)=mod_name 
        then Some path
        else None
   ) fw.Assistance_fw_wrapper_t.compilable_files in 
   relocate_compilable_files_to fw the_files new_subdir;;

let helper_during_string_replacement fw (old_string,new_string) accu old_list=
    let new_list =Assistance_image.image (
       fun pair->
         let (old_path,_)=pair in 
         if not(Assistance_supstring.contains (Assistance_fw_wrapper_automatic.get_content fw old_path) old_string)
         then pair 
         else 
            let ap = Assistance_dfn_full.to_absolute_path (Assistance_dfn_join.root_to_rootless (Assistance_fw_wrapper_automatic.root fw) old_path) in 
            let _=(
             Assistance_replace_inside.replace_inside_file (old_string,new_string) ap;
             accu:=old_path::(!accu)
            ) in 
            recompute_all_info fw old_path 
         ) old_list in 
    (new_list,List.rev(!accu));;

let replace_string fw (old_string,new_string)=
    let ref_for_usual_ones=ref[] 
    and ref_for_special_ones=ref[] in 
    let (new_usual_files,changed_usual_files)=
        helper_during_string_replacement 
           fw (old_string,new_string) ref_for_usual_ones fw.Assistance_fw_wrapper_t.compilable_files 
    and  (new_special_files,changed_special_files)=
        helper_during_string_replacement 
           fw (old_string,new_string) ref_for_special_ones fw.Assistance_fw_wrapper_t.noncompilable_files   in 
    let new_fw ={
       fw with
       Assistance_fw_wrapper_t.compilable_files         = new_usual_files ;
       Assistance_fw_wrapper_t.noncompilable_files = new_special_files ;
    }  in 
    (new_fw,(changed_usual_files,changed_special_files));;         
       
let rename_value_inside_module fw (old_name,new_name) preceding_files rootless_path=
   let full_path = Assistance_dfn_join.root_to_rootless (Assistance_fw_wrapper_automatic.root fw) rootless_path in 
   let absolute_path=Assistance_dfn_full.to_absolute_path  full_path in 
   let _=Assistance_rename_moduled_value_in_file.rename_moduled_value_in_file 
      preceding_files old_name new_name absolute_path in 
   let new_compilable_files =Assistance_image.image (
       fun pair->
         let (path,_)=pair in 
         if path = rootless_path 
         then recompute_all_info fw path
         else pair 
   ) (fw.Assistance_fw_wrapper_t.compilable_files) in 
   {
      fw with 
       Assistance_fw_wrapper_t.compilable_files = new_compilable_files
   };;  

let ref_for_subdirectory_renaming = ref [];;

let remember_during_subdirectory_renaming pair =
   (ref_for_subdirectory_renaming := pair :: (!ref_for_subdirectory_renaming) );;

let helper1_during_subdirectory_renaming fw (old_subdir,new_subdir) pair=
   let (rootless_path,_)=pair in 
   match Assistance_dfn_rootless.soak (old_subdir,new_subdir) rootless_path with 
   Some(new_rootless_path) -> 
        let _=(
           remember_during_subdirectory_renaming (rootless_path,new_rootless_path)
        ) in 
        recompute_all_info fw new_rootless_path
   |None -> pair;;

let helper2_during_subdirectory_renaming fw (old_subdir,new_subdir) l_pairs =
     let _=(ref_for_subdirectory_renaming := []) in 
     let comp=Assistance_image.image (helper1_during_subdirectory_renaming fw (old_subdir,new_subdir)) l_pairs in 
     (comp,List.rev(!ref_for_subdirectory_renaming));;

let rename_subdirectory_as fw (old_subdir,new_subdir)=
    let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_wrapper_automatic.root fw)  in 
    let s_old_subdir = Assistance_dfa_subdirectory.without_trailing_slash old_subdir 
    and s_new_subdir = Assistance_dfa_subdirectory.without_trailing_slash new_subdir in 
    let old_full_path = s_root^s_old_subdir 
    and new_full_path = s_root^s_new_subdir in 
    let cmd=" mv "^old_full_path^" "^new_full_path in 
        let _=Assistance_unix_command.hardcore_uc cmd in 
    let (c_files,c_reps)   =  helper2_during_subdirectory_renaming fw (old_subdir,new_subdir) (fw.Assistance_fw_wrapper_t.compilable_files) 
    and (nc_files,nc_reps) =  helper2_during_subdirectory_renaming fw (old_subdir,new_subdir) (fw.Assistance_fw_wrapper_t.noncompilable_files) in    
   let fw2 = {
      fw with
      Assistance_fw_wrapper_t.compilable_files = c_files  ;
      Assistance_fw_wrapper_t.noncompilable_files = nc_files  ;
   } in 
   Assistance_fw_wrapper_automatic.reflect_replacements_in_diff fw2 (c_reps@nc_reps);;   

let helper1_during_inspection fw accu pair=
   let (rootless_path,old_mtime)=pair in 
   let new_mtime = recompute_mtime fw rootless_path in 
   if new_mtime <> old_mtime
   then let _=(accu:=rootless_path::(!accu)) in 
        recompute_all_info fw rootless_path
   else pair;;

let helper2_during_inspection fw accu l_pairs =
   let new_l_pairs = Assistance_image.image (helper1_during_inspection fw accu) l_pairs in 
   (new_l_pairs,List.rev(!accu));;

let inspect_and_update fw = 
    let ref_for_compilable_ones=ref[] 
    and ref_for_noncompilable_ones=ref[] in 
    let (new_c_files,changed_c_files)=
        helper2_during_inspection fw ref_for_compilable_ones fw.Assistance_fw_wrapper_t.compilable_files 
    and  (new_nc_files,changed_nc_files)=
        helper2_during_inspection fw ref_for_noncompilable_ones fw.Assistance_fw_wrapper_t.noncompilable_files   in 
    let fw2 ={
       fw with
       Assistance_fw_wrapper_t.compilable_files         = new_c_files ;
       Assistance_fw_wrapper_t.noncompilable_files = new_nc_files ;
    }  in 
    let new_fw = Assistance_fw_wrapper_automatic.reflect_changes_in_diff fw2 (changed_c_files@changed_nc_files) in 
    (new_fw,(changed_c_files,changed_nc_files));;         


let helper1_inside_module_renaming_in_filename fw s_new_module rootless_to_be_renamed =
  let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_wrapper_automatic.root fw) 
   and (Assistance_dfn_rootless_t.J(s,m,e))=rootless_to_be_renamed in 
   let s_old_ap=s_root^(Assistance_dfn_rootless.to_line rootless_to_be_renamed)
   and s_new_ap=s_root^(Assistance_dfa_subdirectory.connectable_to_subpath s)
                ^s_new_module^(Assistance_dfa_ending.connectable_to_modulename e) in 
   "mv "^s_old_ap^" "^s_new_ap;;

let helper2_inside_module_renaming_in_filename fw new_module rootless_to_be_renamed =
  let (Assistance_dfn_rootless_t.J(s,m,e))=rootless_to_be_renamed in 
  (rootless_to_be_renamed,Assistance_dfn_rootless_t.J(s,new_module,e));;

let rename_module_in_filename_only fw rootlesses_to_be_renamed new_module =
   let s_new_module = Assistance_dfa_module.to_line new_module in 
   let l_cmds = Assistance_image.image (helper1_inside_module_renaming_in_filename fw s_new_module) rootlesses_to_be_renamed in 
   let replacements = Assistance_image.image (helper2_inside_module_renaming_in_filename fw new_module) rootlesses_to_be_renamed  in            
   let _ =Assistance_unix_command.conditional_multiple_uc l_cmds in  
   let old_compilable_files = fw.Assistance_fw_wrapper_t.compilable_files  in    
   let new_compilable_files = Assistance_image.image (
     fun pair->
       let (rootless,_)=pair in 
       match Assistance_option.seek (fun (old_one,_)->old_one=rootless) replacements with  
       Some(_,new_rootless_path)-> recompute_all_info fw new_rootless_path  
       |None -> pair 
   )  old_compilable_files in 
   let fw2 ={
      fw with 
      Assistance_fw_wrapper_t.compilable_files = new_compilable_files
   }   in 
   Assistance_fw_wrapper_automatic.reflect_replacements_in_diff fw2 replacements;;
    
let rename_module_in_files fw (old_module,new_module) files_to_be_rewritten =
  let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_wrapper_automatic.root fw) in 
  let _=List.iter (
    fun rootless_path->
      let ap=Assistance_absolute_path.of_string (s_root^(Assistance_dfn_rootless.to_line rootless_path)) in 
      Assistance_look_for_module_names.change_module_name_in_mlx_file old_module new_module ap 
  ) files_to_be_rewritten in 
  let old_compilable_files = fw.Assistance_fw_wrapper_t.compilable_files  in    
  let new_compilable_files = Assistance_image.image (
     fun pair->
       let (rootless,_)=pair in 
       if List.mem rootless files_to_be_rewritten
       then recompute_all_info fw rootless 
       else pair 
   )  old_compilable_files in 
   let fw2 ={
      fw with 
      Assistance_fw_wrapper_t.compilable_files = new_compilable_files
   }    in 
   Assistance_fw_wrapper_automatic.reflect_changes_in_diff fw2 files_to_be_rewritten
    ;;
      
let rename_module_in_special_files fw (old_module,new_module) =
  let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_wrapper_automatic.root fw) in 
  let old_special_files = fw.Assistance_fw_wrapper_t.noncompilable_files   in    
  let new_special_files = Assistance_image.image (
     fun pair->
       let (rootless,mtime)=pair in 
       let ap=Assistance_absolute_path.of_string (s_root^(Assistance_dfn_rootless.to_line rootless)) in 
       if List.mem old_module (Assistance_look_for_module_names.names_in_mlx_file ap)
       then let ap=Assistance_absolute_path.of_string (s_root^(Assistance_dfn_rootless.to_line rootless)) in 
            let _=Assistance_look_for_module_names.change_module_name_in_mlx_file old_module new_module ap in 
            recompute_all_info fw rootless 
       else pair 
   )  old_special_files in 
   {
      fw with 
      Assistance_fw_wrapper_t.noncompilable_files = new_special_files
   }     ;;   

let rename_module_everywhere fw rootlesses_to_be_renamed new_module files_to_be_rewritten=
   let (Assistance_dfn_rootless_t.J(_,old_module,_))=List.hd rootlesses_to_be_renamed in
   let fw2=rename_module_in_filename_only fw rootlesses_to_be_renamed new_module in 
   let fw3=rename_module_in_files fw2 (old_module,new_module) files_to_be_rewritten in 
   fw3;;

let replace_string_in_list_of_pairs fw (replacee,replacer) l=
   let changed_ones=ref[]  in 
   let new_l=Assistance_image.image (
      fun pair ->
        let (rootless,mtime)=pair in 
        let content = Assistance_fw_wrapper_automatic.get_content fw rootless in 
        if Assistance_substring.is_a_substring_of replacee content 
        then let _=(changed_ones:= rootless:: (!changed_ones)) in 
             let s_root = Assistance_dfa_root.connectable_to_subpath (Assistance_fw_wrapper_automatic.root fw) 
             and s_path=Assistance_dfn_rootless.to_line rootless in 
             let file = s_root^s_path in  
             let ap=Assistance_absolute_path.of_string file in 
             let _=(Assistance_replace_inside.replace_inside_file (replacee,replacer) ap;
                   ) in 
             recompute_all_info fw rootless 
        else pair     
   ) l in 
   (new_l,List.rev(!changed_ones));;

let replace_string fw (replacee,replacer) =
   let rep = replace_string_in_list_of_pairs fw (replacee,replacer)  in 
   let (new_c_files,changed_c_files) =  rep fw.Assistance_fw_wrapper_t.compilable_files 
   and (new_nc_files,changed_nc_files) =  rep fw.Assistance_fw_wrapper_t.noncompilable_files in 
   let fw2 ={
       fw with
       Assistance_fw_wrapper_t.compilable_files = new_c_files;
       noncompilable_files = new_nc_files;
   } in 
   let fw3 = Assistance_fw_wrapper_automatic.reflect_changes_in_diff fw2 (changed_c_files @ changed_nc_files) in 
   (fw3,(changed_c_files,changed_nc_files));;

let replace_value fw (preceding_files,path) (replacee,pre_replacer) =
    let replacer=(Assistance_cull_string.before_rightmost replacee '.')^"."^pre_replacer in 
    let _=Assistance_rename_moduled_value_in_file.rename_moduled_value_in_file 
      preceding_files replacee (Assistance_overwriter.of_string pre_replacer) path in 
    let rootless = Assistance_dfn_common.decompose_absolute_path_using_root path (Assistance_fw_wrapper_automatic.root fw)  in 
    let fw2= update_some_files fw ([rootless],[]) in 
    let (fw3,(changed_c_files,changed_nc_files))=replace_string fw2 (replacee,replacer) in 
    let fw4 =  Assistance_fw_wrapper_automatic.reflect_changes_in_diff fw3 (rootless::(changed_c_files@changed_nc_files)) in         
    (fw4,(rootless::changed_c_files,changed_nc_files));;


let compilable_absolute_paths fw= 
   let root = Assistance_fw_wrapper_automatic.root fw in 
   Assistance_image.image (
     fun (rootless,_)-> 
        Assistance_absolute_path.of_string (
           Assistance_dfn_common.recompose_potential_absolute_path root rootless
        )
   ) fw.Assistance_fw_wrapper_t.compilable_files;;
   
let overwrite_compilable_file_if_it_exists fw rootless new_content =
   let root = Assistance_fw_wrapper_automatic.root fw in 
   if List.exists ( fun (r,_)->r=rootless ) fw.Assistance_fw_wrapper_t.compilable_files 
   then let ap = Assistance_absolute_path.of_string (Assistance_dfn_common.recompose_potential_absolute_path root rootless) in 
        let _=Assistance_io.overwrite_with ap new_content in 
        {
           fw with 
           Assistance_fw_wrapper_t.compilable_files = update_in_list_of_pairs fw [rootless] (fw.Assistance_fw_wrapper_t.compilable_files);
        }
   else fw;;


end;;


let empty_one config= {
   Assistance_fw_wrapper_t.configuration = config;
   compilable_files = [];
   noncompilable_files = [];
   last_noticed_changes = Assistance_dircopy_diff.empty_one;
};; 

let forget_modules = Private.forget_modules;;

let inspect_and_update = Private.inspect_and_update;;

let compilable_absolute_paths = Private.compilable_absolute_paths;;

let overwrite_compilable_file_if_it_exists = Private.overwrite_compilable_file_if_it_exists;;

let reflect_latest_changes_in_github fw opt_msg=
   let config = fw.Assistance_fw_wrapper_t.configuration in 
   let _= Assistance_reflect_change_in_github.backup config fw.Assistance_fw_wrapper_t.last_noticed_changes opt_msg in 
   {fw with Assistance_fw_wrapper_t.last_noticed_changes = Assistance_dircopy_diff.empty_one} ;; 


let register_rootless_paths = Private.register_rootless_paths;;

let relocate_module_to = Private.relocate_module_to;;

let remove_files = Private.remove_files;;

let rename_module = Private.rename_module_everywhere;;

let rename_subdirectory_as = Private.rename_subdirectory_as;;

let replace_string = Private.replace_string;;

let replace_value = Private.replace_value;;




end;;






module Assistance_ocaml_library=struct

(* 

#use"Compilation_management/ocaml_library.ml";;

*)


let correspondances=[
   Assistance_ocaml_library_t.NumLib,"num";
   Assistance_ocaml_library_t.StrLib,"str";
   Assistance_ocaml_library_t.UnixLib,"unix"];;
let capitalized_correspondances =Assistance_image.image (
   fun (x,y)->(x,"Ocaml"^"_library_t."^y)
) correspondances;;

exception Unknown_lib of string;;

let of_string s=
  try (fst(Assistance_listennou.force_find (fun (x,y)->y=s) correspondances))
  with _->raise(Unknown_lib(s));;

let to_string lib=snd(Assistance_listennou.force_find (fun (x,y)->x=lib) correspondances);;  


let short_name=function
   Assistance_ocaml_library_t.NumLib->"NumLib" 
  |StrLib->"StrLib" 
  |UnixLib->"UnixLib";;

let ocaml_name lib=
  (*cutting the name as always, to avoid a circular definition *)
  "Ocaml"^"_library."^(short_name lib);;

let file_for_library=function 
  Assistance_ocaml_library_t.NumLib->"nums" |StrLib->"str" |UnixLib->"unix";;  

let modules_telling_a_library_away=function
Assistance_ocaml_library_t.NumLib->["num";"big_int";"arith_status"] 
|StrLib->["str"] 
|UnixLib->["unix"];;    


let all_libraries=[Assistance_ocaml_library_t.NumLib;Assistance_ocaml_library_t.StrLib;Assistance_ocaml_library_t.UnixLib];;  

let compute_needed_libraries_from_uncapitalized_modules_list l=
   List.filter (
      fun lib->List.exists(
        fun z->List.mem z (modules_telling_a_library_away lib)
      ) l
   ) all_libraries;;
           
let of_concrete_object =Assistance_concrete_object_automatic.unwrap_lonely_variant 
  capitalized_correspondances;;
          
let to_concrete_object =Assistance_concrete_object_automatic.wrap_lonely_variant 
  capitalized_correspondances;;    


end;;






module Assistance_coma_state_automatic=struct

(* 

#use"Compilation_management/coma_state_field.ml";;

Here, we put soma wrappers so that all direct manipulations of
the Coma_state_t.t datatype should be done here.

*)

exception Module_not_found of Assistance_dfa_module_t.t ;;

(* Converters *)
let of_t x=x;;
let to_t x=x;;
(*
in debug mode, change the above to
let of_t (Coma_state_t.CS x)=x;;
let to_t x=(Coma_state_t.CS x);;
*)
(* End of converters *)


let frontier_with_unix_world cs = (of_t cs).Assistance_coma_state_t.frontier_with_unix_world;;
let configuration cs=(frontier_with_unix_world cs).Assistance_fw_wrapper_t.configuration;;
let root cs= Assistance_fw_configuration.root (configuration cs);;
let backup_dir cs=(configuration cs).Assistance_fw_configuration_t.dir_for_backup;;
let gitpush_after_backup cs=(configuration cs).Assistance_fw_configuration_t.gitpush_after_backup;;   
let github_url cs=(configuration cs).Assistance_fw_configuration_t.github_url;;
let encoding_protected_files cs=(configuration cs).Assistance_fw_configuration_t.encoding_protected_files;;


let subdir_at_module cs mn=
   try List.assoc mn ( (of_t cs).Assistance_coma_state_t.subdir_for_module) with 
   _ -> raise(Module_not_found(mn));;

let principal_ending_at_module cs mn=
   try List.assoc mn ( (of_t cs).Assistance_coma_state_t.principal_ending_for_module) with 
   _ -> raise(Module_not_found(mn));;

let mli_presence_at_module cs mn=
   try List.assoc mn ( (of_t cs).Assistance_coma_state_t.mli_presence_for_module) with 
   _ -> raise(Module_not_found(mn));;

let principal_mt_at_module cs mn=
   try List.assoc mn ( (of_t cs).Assistance_coma_state_t.principal_mt_for_module) with 
   _ -> raise(Module_not_found(mn));;

let mli_mt_at_module cs mn=
    try List.assoc mn ( (of_t cs).Assistance_coma_state_t.mli_mt_for_module) with 
    _ -> raise(Module_not_found(mn));;

let needed_libs_at_module cs mn=
    try List.assoc mn ( (of_t cs).Assistance_coma_state_t.needed_libs_for_module) with 
    _ -> raise(Module_not_found(mn));;

let direct_fathers_at_module cs mn=
    try  List.assoc mn ( (of_t cs).Assistance_coma_state_t.direct_fathers_for_module) with 
    _ -> raise(Module_not_found(mn));;

let ancestors_at_module cs mn=
    try  List.assoc mn ( (of_t cs).Assistance_coma_state_t.ancestors_for_module) with 
    _ -> raise(Module_not_found(mn));;

let needed_dirs_at_module cs mn=
    try  List.assoc mn  ((of_t cs).Assistance_coma_state_t.needed_dirs_for_module) with 
    _ -> raise(Module_not_found(mn));;

let product_up_to_date_at_module cs mn=
   try  List.assoc mn ((of_t cs).Assistance_coma_state_t.last_compilation_result_for_module) with     
   _ -> raise(Module_not_found(mn));;

let directories cs=(of_t cs).Assistance_coma_state_t.directories;;
let preq_types cs=(of_t cs).Assistance_coma_state_t.printer_equipped_types;;



let ordered_list_of_modules cs=((of_t cs).Assistance_coma_state_t.modules);; 

let test_module_for_registration cs modname=
  List.mem modname (ordered_list_of_modules cs);;

let follows_it_but_does_not_necessarily_depend_on_it cs mn=
    let (_,_,after) = Assistance_three_parts.select_center_element_and_reverse_left (fun x->x=mn)
      (ordered_list_of_modules cs) in 
    after;;


let all_used_subdirs cs =
   let current_assoc = (of_t cs).Assistance_coma_state_t.subdir_for_module in 
   Assistance_image.image snd current_assoc ;;



(* Setters  *)

let set_frontier_with_unix_world cs v= 
   let ccs=of_t cs in 
   to_t({ccs with Assistance_coma_state_t.frontier_with_unix_world=v});;


let set_push_after_backup cs bowl = let ccs=of_t cs in 
     let old_frontier = ccs.Assistance_coma_state_t.frontier_with_unix_world in 
     let old_config = old_frontier.Assistance_fw_wrapper_t.configuration in 
     let new_config = {old_config with Assistance_fw_configuration_t.gitpush_after_backup=bowl } in 
     let new_frontier = {old_frontier with Assistance_fw_wrapper_t.configuration = new_config} in 
     to_t({ccs with Assistance_coma_state_t.frontier_with_unix_world=new_frontier });;



let set_subdir_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Assistance_coma_state_t.subdir_for_module in 
    let new_assocs=Assistance_associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Assistance_coma_state_t.subdir_for_module=new_assocs });;
    

let set_principal_ending_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Assistance_coma_state_t.principal_ending_for_module in 
    let new_assocs=Assistance_associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Assistance_coma_state_t.principal_ending_for_module=new_assocs });;


let set_mli_presence_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Assistance_coma_state_t.mli_presence_for_module in 
    let new_assocs=Assistance_associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Assistance_coma_state_t.mli_presence_for_module=new_assocs });;


let set_principal_mt_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Assistance_coma_state_t.principal_mt_for_module in 
    let new_assocs=Assistance_associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Assistance_coma_state_t.principal_mt_for_module=new_assocs });;

let set_mli_mt_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Assistance_coma_state_t.mli_mt_for_module in 
    let new_assocs=Assistance_associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Assistance_coma_state_t.mli_mt_for_module=new_assocs });;

let set_needed_libs_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Assistance_coma_state_t.needed_libs_for_module in 
    let new_assocs=Assistance_associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Assistance_coma_state_t.needed_libs_for_module=new_assocs });;


let set_direct_fathers_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Assistance_coma_state_t.direct_fathers_for_module in 
    let new_assocs=Assistance_associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Assistance_coma_state_t.direct_fathers_for_module=new_assocs });;



let set_ancestors_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Assistance_coma_state_t.ancestors_for_module in 
    let new_assocs=Assistance_associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Assistance_coma_state_t.ancestors_for_module=new_assocs });;


let set_needed_dirs_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Assistance_coma_state_t.needed_dirs_for_module in 
    let new_assocs=Assistance_associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Assistance_coma_state_t.needed_dirs_for_module=new_assocs });;
    


let set_product_up_to_date_at_module cs mn v=
    let ccs=of_t cs in 
    let old_assocs = ccs.Assistance_coma_state_t.last_compilation_result_for_module in 
    let new_assocs=Assistance_associative_list.change_value_for_key old_assocs (mn,v) in 
    to_t({ccs with Assistance_coma_state_t.last_compilation_result_for_module=new_assocs });;
    


let set_directories cs v = let ccs=of_t cs in 
                            to_t({ccs with Assistance_coma_state_t.directories=v});;


let set_preq_types cs v = let ccs=of_t cs in 
                            to_t({ccs with Assistance_coma_state_t.printer_equipped_types=v});;


(* Adhoc setters *)

exception Impose_last_change_exn of Assistance_dircopy_diff_t.t ;;

let impose_last_changes cs diff =
   let old_fw = frontier_with_unix_world cs in 
   let old_diff = old_fw.Assistance_fw_wrapper_t.last_noticed_changes in 
   if not(Assistance_dircopy_diff.is_empty old_diff)
   then raise(Impose_last_change_exn(old_diff))
   else 
   let new_fw = {
       old_fw with 
       Assistance_fw_wrapper_t.last_noticed_changes = diff 
   } in 
   set_frontier_with_unix_world cs new_fw ;;

let modify_all_subdirs cs f =
   let ccs=of_t cs in 
   let old_subdirs = ((of_t cs).Assistance_coma_state_t.subdir_for_module) in 
   let new_subdirs = Assistance_image.image (fun (key,vaal)->(key,f vaal)) old_subdirs in 
   to_t({ccs with Assistance_coma_state_t.subdir_for_module= new_subdirs });;

let modify_all_needed_dirs cs f =
   let ccs=of_t cs in 
   let old_needed_dirs = ((of_t cs).Assistance_coma_state_t.needed_dirs_for_module) in 
   let new_needed_dirs = Assistance_image.image (fun (key,vaal)->(key,Assistance_image.image f vaal)) old_needed_dirs in 
   to_t({ccs with Assistance_coma_state_t.needed_dirs_for_module= new_needed_dirs });;

(* End of adhoc setters *)



let empty_one config=
    to_t({
     Assistance_coma_state_t.frontier_with_unix_world= Assistance_fw_wrapper.empty_one config;
     modules = [];
     subdir_for_module = [] ;
     principal_ending_for_module = [] ;
     mli_presence_for_module = [] ;
     principal_mt_for_module = [] ;
     mli_mt_for_module = [] ;
     needed_libs_for_module = [] ;
     direct_fathers_for_module = [];
     ancestors_for_module = [] ; 
     needed_dirs_for_module = [];
     last_compilation_result_for_module = [];
     directories =[];
     printer_equipped_types =[];
});;


let change_one_module_name wrapped_cs old_mn new_mn=
    (* note that preq_types are not dealt with here *)
    let cs=of_t wrapped_cs in
    let new_modules = Assistance_image.image (fun x->if x=old_mn then new_mn else x)(ordered_list_of_modules cs) in  
    let rep_pair = (old_mn,new_mn) in 
    let new_subdirs = Assistance_associative_list.change_name_for_key (cs.Assistance_coma_state_t.subdir_for_module) rep_pair
    and new_principal_endings = Assistance_associative_list.change_name_for_key (cs.Assistance_coma_state_t.principal_ending_for_module) rep_pair
    and new_mli_presences = Assistance_associative_list.change_name_for_key (cs.Assistance_coma_state_t.mli_presence_for_module) rep_pair
    and new_principal_mts = Assistance_associative_list.change_name_for_key (cs.Assistance_coma_state_t.principal_mt_for_module) rep_pair
    and new_mli_mts = Assistance_associative_list.change_name_for_key (cs.Assistance_coma_state_t.mli_mt_for_module) rep_pair
    and new_needed_libs = Assistance_associative_list.change_name_for_key (cs.Assistance_coma_state_t.needed_libs_for_module) rep_pair
    and new_direct_fathers = Assistance_associative_list.change_name_for_key (cs.Assistance_coma_state_t.direct_fathers_for_module) rep_pair
    and new_ancestors = Assistance_associative_list.change_name_for_key (cs.Assistance_coma_state_t.ancestors_for_module) rep_pair
    and new_needed_dirs = Assistance_associative_list.change_name_for_key (cs.Assistance_coma_state_t.needed_dirs_for_module) rep_pair  
    and new_products_up_to_date = Assistance_associative_list.change_name_for_key  cs.Assistance_coma_state_t.last_compilation_result_for_module rep_pair  in 
to_t({ cs with 
      Assistance_coma_state_t.modules = new_modules;
      Assistance_coma_state_t.subdir_for_module=  new_subdirs;
      Assistance_coma_state_t.principal_ending_for_module=  new_principal_endings;
      Assistance_coma_state_t.mli_presence_for_module=  new_mli_presences;
      Assistance_coma_state_t.principal_mt_for_module=  new_principal_mts;
      Assistance_coma_state_t.mli_mt_for_module=  new_mli_mts;
      Assistance_coma_state_t.needed_libs_for_module=  new_needed_libs;
      Assistance_coma_state_t.direct_fathers_for_module=  new_direct_fathers;
      Assistance_coma_state_t.ancestors_for_module=  new_ancestors;
      Assistance_coma_state_t.needed_dirs_for_module= new_needed_dirs;
      Assistance_coma_state_t.last_compilation_result_for_module = new_products_up_to_date;
});;

let remove_in_each_at_module wrapped_cs mname=
    let cs=of_t wrapped_cs in
    let new_modules = List.filter (fun x->x<>mname) (ordered_list_of_modules cs) 
    and new_subdirs = Assistance_associative_list.remove_key (cs.Assistance_coma_state_t.subdir_for_module) mname
    and new_principal_endings = Assistance_associative_list.remove_key (cs.Assistance_coma_state_t.principal_ending_for_module) mname
    and new_mli_presences = Assistance_associative_list.remove_key (cs.Assistance_coma_state_t.mli_presence_for_module) mname
    and new_principal_mts = Assistance_associative_list.remove_key (cs.Assistance_coma_state_t.principal_mt_for_module) mname
    and new_mli_mts = Assistance_associative_list.remove_key (cs.Assistance_coma_state_t.mli_mt_for_module) mname
    and new_needed_libs = Assistance_associative_list.remove_key (cs.Assistance_coma_state_t.needed_libs_for_module) mname
    and new_direct_fathers = Assistance_associative_list.remove_key (cs.Assistance_coma_state_t.direct_fathers_for_module) mname
    and new_ancestors = Assistance_associative_list.remove_key (cs.Assistance_coma_state_t.ancestors_for_module) mname
    and new_needed_dirs = Assistance_associative_list.remove_key (cs.Assistance_coma_state_t.needed_dirs_for_module) mname  
    and new_products_up_to_date = Assistance_associative_list.remove_key  cs.Assistance_coma_state_t.last_compilation_result_for_module mname  in 
to_t({ cs with 
      Assistance_coma_state_t.modules = new_modules;
      Assistance_coma_state_t.subdir_for_module=  new_subdirs;
      Assistance_coma_state_t.principal_ending_for_module=  new_principal_endings;
      Assistance_coma_state_t.mli_presence_for_module=  new_mli_presences;
      Assistance_coma_state_t.principal_mt_for_module=  new_principal_mts;
      Assistance_coma_state_t.mli_mt_for_module=  new_mli_mts;
      Assistance_coma_state_t.needed_libs_for_module=  new_needed_libs;
      Assistance_coma_state_t.direct_fathers_for_module=  new_direct_fathers;
      Assistance_coma_state_t.ancestors_for_module=  new_ancestors;
      Assistance_coma_state_t.needed_dirs_for_module= new_needed_dirs;
      Assistance_coma_state_t.last_compilation_result_for_module = new_products_up_to_date;
});;



let push_right_in_each wrapped_cs (hm,pr_end,mlip,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
    let nm=Assistance_dfn_endingless.to_module hm
    and subdir=Assistance_dfn_endingless.to_subdirectory hm 
    and  cs=of_t wrapped_cs in
    let new_modules = (cs.Assistance_coma_state_t.modules)@[nm] 
    and new_subdirs = (  cs.Assistance_coma_state_t.subdir_for_module) @[nm,subdir]
    and new_principal_endings = (  cs.Assistance_coma_state_t.principal_ending_for_module) @[nm,pr_end] 
    and new_mli_presences = (  cs.Assistance_coma_state_t.mli_presence_for_module) @[nm,mlip] 
    and new_principal_mts = (  cs.Assistance_coma_state_t.principal_mt_for_module) @[nm,prmt] 
    and new_mli_mts = (  cs.Assistance_coma_state_t.mli_mt_for_module) @[nm,mlimt] 
    and new_needed_libs = (  cs.Assistance_coma_state_t.needed_libs_for_module) @[nm,libned] 
    and new_direct_fathers = (  cs.Assistance_coma_state_t.direct_fathers_for_module) @[nm,dirfath]
    and new_ancestors = (  cs.Assistance_coma_state_t.ancestors_for_module) @[nm,allanc] 
    and new_needed_dirs = (cs.Assistance_coma_state_t.needed_dirs_for_module)@[nm,dirned] 
    and new_products_up_to_date = (cs.Assistance_coma_state_t.last_compilation_result_for_module)@[nm,upy]  in 
to_t({ cs with 
      Assistance_coma_state_t.modules = new_modules;
      Assistance_coma_state_t.subdir_for_module=  new_subdirs;
      Assistance_coma_state_t.principal_ending_for_module=  new_principal_endings;
      Assistance_coma_state_t.mli_presence_for_module=  new_mli_presences;
      Assistance_coma_state_t.principal_mt_for_module=  new_principal_mts;
      Assistance_coma_state_t.mli_mt_for_module=  new_mli_mts;
      Assistance_coma_state_t.needed_libs_for_module=  new_needed_libs;
      Assistance_coma_state_t.direct_fathers_for_module=  new_direct_fathers;
      Assistance_coma_state_t.ancestors_for_module=  new_ancestors;
      Assistance_coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Assistance_coma_state_t.last_compilation_result_for_module = new_products_up_to_date;
});;

let set_in_each wrapped_cs nm (pr_end,mlip,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
    let cs=of_t wrapped_cs in
    let new_principal_endings = Assistance_associative_list.change_value_for_key (cs.Assistance_coma_state_t.principal_ending_for_module) (nm,pr_end) 
    and new_mli_presences = Assistance_associative_list.change_value_for_key (cs.Assistance_coma_state_t.mli_presence_for_module) (nm, mlip) 
    and new_principal_mts = Assistance_associative_list.change_value_for_key (cs.Assistance_coma_state_t.principal_mt_for_module) (nm,prmt) 
    and new_mli_mts = Assistance_associative_list.change_value_for_key (cs.Assistance_coma_state_t.mli_mt_for_module) (nm,mlimt) 
    and new_needed_libs = Assistance_associative_list.change_value_for_key (cs.Assistance_coma_state_t.needed_libs_for_module) (nm,libned) 
    and new_direct_fathers = Assistance_associative_list.change_value_for_key (cs.Assistance_coma_state_t.direct_fathers_for_module) (nm,dirfath)
    and new_ancestors = Assistance_associative_list.change_value_for_key (cs.Assistance_coma_state_t.ancestors_for_module) (nm,allanc) 
    and new_needed_dirs = Assistance_associative_list.change_value_for_key (cs.Assistance_coma_state_t.needed_dirs_for_module) (nm,dirned) 
    and new_products_up_to_date = Assistance_associative_list.change_value_for_key  cs.Assistance_coma_state_t.last_compilation_result_for_module (nm,upy)  in 
to_t({ cs with 
      (* the "module" and "subdir" fields are not changed *)
      Assistance_coma_state_t.principal_ending_for_module=  new_principal_endings;
      Assistance_coma_state_t.mli_presence_for_module=  new_mli_presences;
      Assistance_coma_state_t.principal_mt_for_module=  new_principal_mts;
      Assistance_coma_state_t.mli_mt_for_module=  new_mli_mts;
      Assistance_coma_state_t.needed_libs_for_module=  new_needed_libs;
      Assistance_coma_state_t.direct_fathers_for_module=  new_direct_fathers;
      Assistance_coma_state_t.ancestors_for_module=  new_ancestors;
      Assistance_coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Assistance_coma_state_t.last_compilation_result_for_module = new_products_up_to_date;
});;
  

    
let reposition_in_each wrapped_cs mn1 mn2=
    let cs=of_t wrapped_cs in
    let l_rep=(fun l->Assistance_associative_list.reposition_by_putting_snd_immediately_after_fst l mn1 mn2 ) in 
    let new_modules = Assistance_listennou.reposition_by_putting_snd_immediately_after_fst (ordered_list_of_modules cs) mn1 mn2 
    and new_subdirs = l_rep (cs.Assistance_coma_state_t.subdir_for_module) 
    and new_principal_endings = l_rep (cs.Assistance_coma_state_t.principal_ending_for_module) 
    and new_mli_presences = l_rep (cs.Assistance_coma_state_t.mli_presence_for_module) 
    and new_principal_mts = l_rep (cs.Assistance_coma_state_t.principal_mt_for_module) 
    and new_mli_mts = l_rep (cs.Assistance_coma_state_t.mli_mt_for_module) 
    and new_needed_libs = l_rep (cs.Assistance_coma_state_t.needed_libs_for_module) 
    and new_direct_fathers = l_rep (cs.Assistance_coma_state_t.direct_fathers_for_module) 
    and new_ancestors = l_rep (cs.Assistance_coma_state_t.ancestors_for_module) 
    and new_needed_dirs = l_rep (cs.Assistance_coma_state_t.needed_dirs_for_module)
    and new_products_up_to_date = l_rep cs.Assistance_coma_state_t.last_compilation_result_for_module in 
to_t({ cs with 
      Assistance_coma_state_t.modules = new_modules;
      Assistance_coma_state_t.subdir_for_module=  new_subdirs;
      Assistance_coma_state_t.principal_ending_for_module=  new_principal_endings;
      Assistance_coma_state_t.mli_presence_for_module=  new_mli_presences;
      Assistance_coma_state_t.principal_mt_for_module=  new_principal_mts;
      Assistance_coma_state_t.mli_mt_for_module=  new_mli_mts;
      Assistance_coma_state_t.needed_libs_for_module=  new_needed_libs;
      Assistance_coma_state_t.direct_fathers_for_module=  new_direct_fathers;
      Assistance_coma_state_t.ancestors_for_module=  new_ancestors;
      Assistance_coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Assistance_coma_state_t.last_compilation_result_for_module = new_products_up_to_date;
});;


let reorder wrapped_cs reordered_list_of_modules =
     let cs=of_t wrapped_cs in 
    let l_rep =(fun l->Assistance_associative_list.reorder l reordered_list_of_modules) in    
    let new_subdirs = l_rep (cs.Assistance_coma_state_t.subdir_for_module) 
    and new_principal_endings = l_rep (cs.Assistance_coma_state_t.principal_ending_for_module) 
    and new_mli_presences = l_rep (cs.Assistance_coma_state_t.mli_presence_for_module) 
    and new_principal_mts = l_rep (cs.Assistance_coma_state_t.principal_mt_for_module) 
    and new_mli_mts = l_rep (cs.Assistance_coma_state_t.mli_mt_for_module) 
    and new_needed_libs = l_rep (cs.Assistance_coma_state_t.needed_libs_for_module) 
    and new_direct_fathers = l_rep (cs.Assistance_coma_state_t.direct_fathers_for_module) 
    and new_ancestors = l_rep (cs.Assistance_coma_state_t.ancestors_for_module) 
    and new_needed_dirs = l_rep (cs.Assistance_coma_state_t.needed_dirs_for_module) 
    and new_products_up_to_date = l_rep cs.Assistance_coma_state_t.last_compilation_result_for_module  in 
to_t({ cs with 
      Assistance_coma_state_t.modules = reordered_list_of_modules;
      Assistance_coma_state_t.subdir_for_module=  new_subdirs;
      Assistance_coma_state_t.principal_ending_for_module=  new_principal_endings;
      Assistance_coma_state_t.mli_presence_for_module=  new_mli_presences;
      Assistance_coma_state_t.principal_mt_for_module=  new_principal_mts;
      Assistance_coma_state_t.mli_mt_for_module=  new_mli_mts;
      Assistance_coma_state_t.needed_libs_for_module=  new_needed_libs;
      Assistance_coma_state_t.direct_fathers_for_module=  new_direct_fathers;
      Assistance_coma_state_t.ancestors_for_module=  new_ancestors;
      Assistance_coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Assistance_coma_state_t.last_compilation_result_for_module = new_products_up_to_date;
});;  

(* For debugging purposes *)

let sizes wrapped_cs =
    let cs=of_t wrapped_cs in
    [ 
      ["modules",List.length(cs.Assistance_coma_state_t.modules)];
      ["subdirs",List.length(cs.Assistance_coma_state_t.subdir_for_module)];
      ["pr_endings",List.length(cs.Assistance_coma_state_t.principal_ending_for_module)];
      ["mlis",List.length(cs.Assistance_coma_state_t.mli_presence_for_module)];
      ["mod_times",List.length(cs.Assistance_coma_state_t.principal_mt_for_module)];
      ["mli_mod_times",List.length(cs.Assistance_coma_state_t.mli_mt_for_module)];
      ["needed_libs",List.length(cs.Assistance_coma_state_t.needed_libs_for_module)];
      ["fathers",List.length(cs.Assistance_coma_state_t.direct_fathers_for_module)];
      ["ancestors",List.length(cs.Assistance_coma_state_t.ancestors_for_module)];
      ["needed_dirs",List.length(cs.Assistance_coma_state_t.needed_dirs_for_module)];
      ["datechecks",List.length(cs.Assistance_coma_state_t.last_compilation_result_for_module)];
  ];;


let push_after_module_in_each wrapped_cs pivot (hm,pr_end,mlip,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
    let nm=Assistance_dfn_endingless.to_module hm
    and subdir=Assistance_dfn_endingless.to_subdirectory hm 
    and  cs=of_t wrapped_cs in
    let new_modules = Assistance_listennou.push_immediately_after (ordered_list_of_modules cs) nm  pivot 
    and new_subdirs = Assistance_associative_list.push_immediately_after (cs.Assistance_coma_state_t.subdir_for_module) (nm,subdir) pivot 
    and new_principal_endings = Assistance_associative_list.push_immediately_after (cs.Assistance_coma_state_t.principal_ending_for_module) (nm,pr_end) pivot 
    and new_mli_presences = Assistance_associative_list.push_immediately_after (cs.Assistance_coma_state_t.mli_presence_for_module) (nm,mlip) pivot 
    and new_principal_mts = Assistance_associative_list.push_immediately_after (cs.Assistance_coma_state_t.principal_mt_for_module) (nm,prmt) pivot 
    and new_mli_mts = Assistance_associative_list.push_immediately_after (cs.Assistance_coma_state_t.mli_mt_for_module) (nm,mlimt) pivot 
    and new_needed_libs = Assistance_associative_list.push_immediately_after (cs.Assistance_coma_state_t.needed_libs_for_module) (nm,libned) pivot 
    and new_direct_fathers = Assistance_associative_list.push_immediately_after (cs.Assistance_coma_state_t.direct_fathers_for_module) (nm,dirfath) pivot 
    and new_ancestors = Assistance_associative_list.push_immediately_after (cs.Assistance_coma_state_t.ancestors_for_module) (nm,allanc) pivot 
    and new_needed_dirs = Assistance_associative_list.push_immediately_after (cs.Assistance_coma_state_t.needed_dirs_for_module) (nm,dirned) pivot
    and new_products_up_to_date = Assistance_associative_list.push_immediately_after cs.Assistance_coma_state_t.last_compilation_result_for_module (nm,upy) pivot  in 
to_t({ cs with 
      Assistance_coma_state_t.modules = new_modules;
      Assistance_coma_state_t.subdir_for_module =  new_subdirs;
      Assistance_coma_state_t.principal_ending_for_module =  new_principal_endings;
      Assistance_coma_state_t.mli_presence_for_module =  new_mli_presences;
      Assistance_coma_state_t.principal_mt_for_module =  new_principal_mts;
      Assistance_coma_state_t.mli_mt_for_module =  new_mli_mts;
      Assistance_coma_state_t.needed_libs_for_module =  new_needed_libs;
      Assistance_coma_state_t.direct_fathers_for_module =  new_direct_fathers;
      Assistance_coma_state_t.ancestors_for_module =  new_ancestors;
      Assistance_coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Assistance_coma_state_t.last_compilation_result_for_module = new_products_up_to_date;
});;
    
let endingless_at_module cs mn=
   Assistance_dfn_endingless_t.J(
        root cs,
        subdir_at_module cs mn,
        mn
    );;

let printer_equipped_types_from_preceding_data  
   (frontier_with_unix_world_field,
      modules_field,
        subdir_for_modules_field,
          principal_ending_at_module_field)=
  let the_root = Assistance_fw_wrapper_automatic.root frontier_with_unix_world_field in         
  Assistance_option.filter_and_unpack (
    fun mn->
    let subdir = List.assoc mn subdir_for_modules_field 
    and pr_end= List.assoc mn principal_ending_at_module_field  in
    let rootless=Assistance_dfn_rootless_t.J(subdir,mn,pr_end) in 
    let text=Assistance_fw_wrapper_automatic.get_content frontier_with_unix_world_field rootless in
    if (Assistance_substring.is_a_substring_of ("let "^"print_out ") text)
    then let eless=Assistance_dfn_endingless_t.J(the_root,subdir,mn) in 
         Some(eless)
    else None
  ) modules_field;;    

let restrict wrapped_cs smaller_list_of_modules =
    let cs=of_t wrapped_cs in 
    let restr =(fun l->Assistance_associative_list.restrict l smaller_list_of_modules) in    
    let temp_direct_fathers = restr (cs.Assistance_coma_state_t.direct_fathers_for_module) 
    and temp_ancestors = restr (cs.Assistance_coma_state_t.ancestors_for_module) in 
    let among_fathers =Assistance_image.image (fun (key,fathers)->
       (key,List.filter (fun father -> List.mem father smaller_list_of_modules) fathers) ) in 
    let new_subdirs = restr (cs.Assistance_coma_state_t.subdir_for_module) 
    and new_principal_endings = restr (cs.Assistance_coma_state_t.principal_ending_for_module) 
    and new_mli_presences = restr (cs.Assistance_coma_state_t.mli_presence_for_module) 
    and new_principal_mts = restr (cs.Assistance_coma_state_t.principal_mt_for_module) 
    and new_mli_mts = restr (cs.Assistance_coma_state_t.mli_mt_for_module) 
    and new_needed_libs = restr (cs.Assistance_coma_state_t.needed_libs_for_module) 
    and new_direct_fathers = among_fathers  temp_direct_fathers  
    and new_ancestors = among_fathers  temp_ancestors
    and new_needed_dirs = restr (cs.Assistance_coma_state_t.needed_dirs_for_module) 
    and new_products_up_to_date = restr cs.Assistance_coma_state_t.last_compilation_result_for_module in  
    let new_preq_types= List.filter (
        fun (eless,_)->
          let middle = Assistance_dfn_endingless.to_middle eless in 
        List.exists (fun (mn,subdir)->middle = Assistance_dfn_middle_t.J(subdir,mn) ) new_subdirs 
        )  cs.Assistance_coma_state_t.printer_equipped_types   in 
    let new_directories = Assistance_ordered.sort Assistance_total_ordering.standard (Assistance_image.image snd new_subdirs) in 
to_t({ cs with 
      Assistance_coma_state_t.modules = smaller_list_of_modules;
      Assistance_coma_state_t.subdir_for_module=  new_subdirs;
      Assistance_coma_state_t.principal_ending_for_module=  new_principal_endings;
      Assistance_coma_state_t.mli_presence_for_module=  new_mli_presences;
      Assistance_coma_state_t.principal_mt_for_module=  new_principal_mts;
      Assistance_coma_state_t.mli_mt_for_module=  new_mli_mts;
      Assistance_coma_state_t.needed_libs_for_module=  new_needed_libs;
      Assistance_coma_state_t.direct_fathers_for_module=  new_direct_fathers;
      Assistance_coma_state_t.ancestors_for_module=  new_ancestors;
      Assistance_coma_state_t.needed_dirs_for_module = new_needed_dirs;
      Assistance_coma_state_t.last_compilation_result_for_module = new_products_up_to_date;
      Assistance_coma_state_t.directories = new_directories;
      Assistance_coma_state_t.printer_equipped_types = new_preq_types;
});;  

let transplant wrapped_cs new_frontier = 
     let cs=of_t wrapped_cs in 
     let new_principal_mts=Assistance_image.image (fun (mn,_)->
          let subdir = List.assoc mn cs.Assistance_coma_state_t.subdir_for_module 
          and pr_end = List.assoc mn cs.Assistance_coma_state_t.principal_ending_for_module in 
          let rootless = (Assistance_dfn_rootless_t.J(subdir,mn,pr_end)) in 
          (mn,Assistance_fw_wrapper_automatic.get_mtime new_frontier rootless)
     ) cs.Assistance_coma_state_t.principal_ending_for_module
     and new_mli_mts=Assistance_image.image (fun (mn,subdir)->
          let rootless = (Assistance_dfn_rootless_t.J(subdir,mn,Assistance_dfa_ending.mli)) in 
          (mn,Assistance_fw_wrapper_automatic.get_mtime_or_zero_if_file_is_nonregistered 
           new_frontier rootless)
     ) cs.Assistance_coma_state_t.subdir_for_module 
     and new_products_up_to_date=Assistance_image.image (fun (mn,_)->(mn,false)
     ) cs.Assistance_coma_state_t.last_compilation_result_for_module
     and new_preq_types=Assistance_image.image (fun (eless,_)->(eless,false)
     ) cs.Assistance_coma_state_t.printer_equipped_types in 
     to_t({
           cs with    
            Assistance_coma_state_t.frontier_with_unix_world= new_frontier;
            principal_mt_for_module = new_principal_mts;
            mli_mt_for_module = new_mli_mts;
            last_compilation_result_for_module = new_products_up_to_date;
            printer_equipped_types = new_preq_types;
     });;

module Private = struct 

let salt = "Coma_"^"state_field.";;

let frontier_with_unix_world_label      = salt ^ "frontier_with_unix_world";;
let dir_for_backup_label                = salt ^ "dir_for_backup";;
let gitpush_after_backup_label          = salt ^ "gitpush_after_backup";;
let modules_label                       = salt ^ "modules";;
let subdir_for_module_label             = salt ^ "subdir_for_module";;
let principal_ending_for_module_label   = salt ^ "principal_ending_for_module";;
let mli_presence_for_module_label       = salt ^ "mli_presence_for_module";;
let principal_mt_for_module_label       = salt ^ "principal_mt_for_module";;
let mli_mt_for_module_label             = salt ^ "mli_mt_for_module";;
let needed_libs_for_module_label        = salt ^ "needed_libs_for_module";;
let direct_fathers_for_module_label     = salt ^ "direct_fathers_for_module";;
let ancestors_for_module_label          = salt ^ "ancestors_for_module";;
let needed_dirs_for_module_label        = salt ^ "needed_dirs_for_module";;
let last_compilation_result_for_module_label = salt ^ "last_compilation_result_for_module";;
let directories_label                   = salt ^ "directories";;
let printer_equipped_types_label        = salt ^ "printer_equipped_types";;

let cr_of_pair f l= Assistance_crobj_converter_combinator.of_pair_list  Assistance_dfa_module.to_concrete_object f l;;
let cr_to_pair f crobj= Assistance_crobj_converter_combinator.to_pair_list  Assistance_dfa_module.of_concrete_object f crobj;;

let of_concrete_object ccrt_obj = 
   let g=Assistance_concrete_object_automatic.get_record ccrt_obj in
   {
      Assistance_coma_state_t.frontier_with_unix_world = Assistance_fw_wrapper_automatic.of_concrete_object (g frontier_with_unix_world_label);
      modules = Assistance_crobj_converter_combinator.to_list Assistance_dfa_module.of_concrete_object (g modules_label);
      subdir_for_module = cr_to_pair Assistance_dfa_subdirectory.of_concrete_object (g subdir_for_module_label);
      principal_ending_for_module = cr_to_pair Assistance_dfa_ending.of_concrete_object (g principal_ending_for_module_label);
      mli_presence_for_module = cr_to_pair Assistance_crobj_converter.bool_of_concrete_object (g mli_presence_for_module_label);
      principal_mt_for_module = cr_to_pair Assistance_crobj_converter.string_of_concrete_object (g principal_mt_for_module_label);
      mli_mt_for_module = cr_to_pair Assistance_crobj_converter.string_of_concrete_object (g mli_mt_for_module_label);
      needed_libs_for_module = cr_to_pair (Assistance_crobj_converter_combinator.to_list Assistance_ocaml_library.of_concrete_object) (g needed_libs_for_module_label);
      direct_fathers_for_module = cr_to_pair (Assistance_crobj_converter_combinator.to_list Assistance_dfa_module.of_concrete_object) (g direct_fathers_for_module_label);
      ancestors_for_module = cr_to_pair (Assistance_crobj_converter_combinator.to_list Assistance_dfa_module.of_concrete_object) (g ancestors_for_module_label); 
      needed_dirs_for_module = cr_to_pair (Assistance_crobj_converter_combinator.to_list Assistance_dfa_subdirectory.of_concrete_object) (g needed_dirs_for_module_label);
      last_compilation_result_for_module = cr_to_pair Assistance_crobj_converter.bool_of_concrete_object (g last_compilation_result_for_module_label);
      directories = (Assistance_crobj_converter_combinator.to_list Assistance_dfa_subdirectory.of_concrete_object)  (g directories_label);
      printer_equipped_types = Assistance_crobj_converter_combinator.to_pair_list 
                                      Assistance_dfn_endingless.of_concrete_object
                                      Assistance_crobj_converter.bool_of_concrete_object (g printer_equipped_types_label);
   };; 

let to_concrete_object cs=
   let items= 
   [
    frontier_with_unix_world_label, Assistance_fw_wrapper_automatic.to_concrete_object cs.Assistance_coma_state_t.frontier_with_unix_world;
    modules_label, Assistance_crobj_converter_combinator.of_list Assistance_dfa_module.to_concrete_object cs.Assistance_coma_state_t.modules;
    subdir_for_module_label, cr_of_pair Assistance_dfa_subdirectory.to_concrete_object cs.Assistance_coma_state_t.subdir_for_module;
    principal_ending_for_module_label, cr_of_pair Assistance_dfa_ending.to_concrete_object cs.Assistance_coma_state_t.principal_ending_for_module;
    mli_presence_for_module_label, cr_of_pair Assistance_crobj_converter.bool_to_concrete_object cs.Assistance_coma_state_t.mli_presence_for_module;  
    principal_mt_for_module_label, cr_of_pair Assistance_crobj_converter.string_to_concrete_object cs.Assistance_coma_state_t.principal_mt_for_module;
    mli_mt_for_module_label, cr_of_pair Assistance_crobj_converter.string_to_concrete_object  cs.Assistance_coma_state_t.mli_mt_for_module;
    needed_libs_for_module_label, cr_of_pair (Assistance_crobj_converter_combinator.of_list Assistance_ocaml_library.to_concrete_object) cs.Assistance_coma_state_t.needed_libs_for_module; 
    direct_fathers_for_module_label, cr_of_pair (Assistance_crobj_converter_combinator.of_list Assistance_dfa_module.to_concrete_object) cs.Assistance_coma_state_t.direct_fathers_for_module;   
    ancestors_for_module_label, cr_of_pair (Assistance_crobj_converter_combinator.of_list Assistance_dfa_module.to_concrete_object) cs.Assistance_coma_state_t.ancestors_for_module;   
    needed_dirs_for_module_label, cr_of_pair (Assistance_crobj_converter_combinator.of_list Assistance_dfa_subdirectory.to_concrete_object)  (cs.Assistance_coma_state_t.needed_dirs_for_module);  
    last_compilation_result_for_module_label, cr_of_pair Assistance_crobj_converter.bool_to_concrete_object cs.Assistance_coma_state_t.last_compilation_result_for_module; 
    directories_label,  (Assistance_crobj_converter_combinator.of_list Assistance_dfa_subdirectory.to_concrete_object) cs.Assistance_coma_state_t.directories; 
    printer_equipped_types_label,  Assistance_crobj_converter_combinator.of_pair_list 
                                      Assistance_dfn_endingless.to_concrete_object
                                      Assistance_crobj_converter.bool_to_concrete_object cs.Assistance_coma_state_t.printer_equipped_types;    
   ]  in
   Assistance_concrete_object_t.Record items;;


end ;;

let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;




end;;






module Assistance_compilation_mode_t=struct

(* 

#use"Compilation_management/compilation_mode_t.ml";;

*)


type t=
   Usual
  |Debug
  |Executable;;

   

           

end;;






module Assistance_compilation_mode=struct

(* 

#use"Compilation_management/compilation_mode.ml";;

*)

exception Ending_for_last_module_exn ;; 
exception Ending_for_nonlast_module_exn ;; 

let workspace = function 
   Assistance_compilation_mode_t.Usual->Assistance_coma_constant.usual_build_subdir
                     |Debug->Assistance_coma_constant.debug_build_subdir
                     |Executable->Assistance_coma_constant.exec_build_subdir;;

let ending_for_last_module = function 
   Assistance_compilation_mode_t.Usual-> raise(Ending_for_last_module_exn)
                     |Debug->".cmo"
                     |Executable->".ml";;

let ending_for_nonlast_module = function 
   Assistance_compilation_mode_t.Usual-> raise(Ending_for_nonlast_module_exn)
                     |Debug->".cmo"
                     |Executable->".cmx";;                     

let executioner = function 
   Assistance_compilation_mode_t.Usual->"ocamlc -bin-annot "
                     |Debug->"ocamlc -g "
                     |Executable->"ocamlopt ";;

let ending_for_final_product = function 
   Assistance_compilation_mode_t.Usual->""
                     |Debug->".ocaml_debuggable "
                     |Executable->".ocaml_executable ";;   
           

end;;






module Assistance_crobj_category_t=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_category_t.ml";;


*)


type t= 
    Uple 
   |List 
   |Array 
   |Record
   |Variant;;



end;;






module Assistance_crobj_opening_t=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_opening_t.ml";;



*)


type t= 
    Uple 
   |List 
   |Array 
   |Record 
   |Variant of string;;



end;;






module Assistance_crobj_basic_increase_t=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_basic_increase_t.ml";;


*)


type t= 
     Push_int of int 
    |Push_string of Assistance_encoded_string_t.t 
    |Push_field_name  of string 
    |Open of Assistance_crobj_opening_t.t 
    |Separate of Assistance_crobj_category_t.t
    |Close of Assistance_crobj_category_t.t;;
    


        





end;;






module Assistance_partial_crobj_t=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/partial_crobj_t.ml";;

For convenience, the list is reversed when the partial object is closed and becomes full
(new objects are therefore appended directly to the left on a partial object).


*)


type t= 
   |Uple of Assistance_concrete_object_t.t list
   |List of Assistance_concrete_object_t.t list
   |Array of Assistance_concrete_object_t.t list
   |Record of ((string*Assistance_concrete_object_t.t) list)
   |RecordPlusFieldName of ((string*Assistance_concrete_object_t.t) list)*string
   |Variant of string*(Assistance_concrete_object_t.t list);;



end;;






module Assistance_double_partial_crobj_t=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/double_partial_crobj_t.ml";;

First argument says if a comma appears last, waiting for another item.

The two last arguments are t * (t list) rather than just t list, to enforce
a non-empty list. 

*)


type t= Double of bool * Assistance_partial_crobj_t.t * (Assistance_partial_crobj_t.t list) ;;



end;;






module Assistance_crobj_parsing_machine_t=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_parsing_machine_t.ml";;


*)

type t= 
    {
       parsed_one    : string;
       current_index : int;
       data : Assistance_double_partial_crobj_t.t;
    };;



end;;






module Assistance_partial_crobj=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/partial_crobj.ml";;


*)

exception Field_With_No_Name of Assistance_concrete_object_t.t;;
exception Unused_Field_Name of string;;
exception Misapplied_Field_Name of string;;
exception Category_Mismatch of Assistance_crobj_category_t.t * Assistance_partial_crobj_t.t;;

let initialize=function 
    Assistance_crobj_opening_t.Uple -> Assistance_partial_crobj_t.Uple[]
   |List -> Assistance_partial_crobj_t.List[]
   |Array -> Assistance_partial_crobj_t.Array[]
   |Record->Assistance_partial_crobj_t.Record([])
   |Variant(constructor)->Assistance_partial_crobj_t.Variant(constructor,[]);;

let category = function 
    Assistance_partial_crobj_t.Uple(l)->Assistance_crobj_category_t.Uple
   |List(_)->Assistance_crobj_category_t.List
   |Array(_)->Assistance_crobj_category_t.Array
   |Record(_)->Assistance_crobj_category_t.Record
   |RecordPlusFieldName(l,rcdname)->Assistance_crobj_category_t.Record
   |Variant(constructor,l)->Assistance_crobj_category_t.Variant;;
 

let push_one_more_item item =function 
    Assistance_partial_crobj_t.Uple(l)->Assistance_partial_crobj_t.Uple(item::l)
   |List(l)->Assistance_partial_crobj_t.List(item::l)
   |Array(l)->Assistance_partial_crobj_t.Array(item::l)
   |Record(_)->raise(Field_With_No_Name(item))
   |RecordPlusFieldName(l,rcdname)->Assistance_partial_crobj_t.Record((rcdname,item)::l)
   |Variant(constructor,l)->Assistance_partial_crobj_t.Variant(constructor,item :: l);;

let push_int i = push_one_more_item (Assistance_concrete_object_t.Int(i));;
let push_string encoded_s = push_one_more_item (Assistance_concrete_object_automatic.wrap_encoded_string encoded_s);;

let push_field_name recdname=function 
    Assistance_partial_crobj_t.Record(l)->Assistance_partial_crobj_t.RecordPlusFieldName(l,recdname)
   |_->raise(Misapplied_Field_Name(recdname));;



let close =function 
    Assistance_partial_crobj_t.Uple(l)->Assistance_concrete_object_t.Uple(List.rev l)
   |List(l)->Assistance_concrete_object_t.List(List.rev l)
   |Array(l)->Assistance_concrete_object_t.Array(List.rev l)
   |Record(l)->Assistance_concrete_object_t.Record(List.rev l)
   |RecordPlusFieldName(_,rcdname)->raise(Unused_Field_Name(rcdname))
   |Variant(constructor,l)->Assistance_concrete_object_t.Variant(constructor,List.rev l);;


let check_category_and_close ctgr pcrobj=
   if category(pcrobj)<>ctgr 
   then raise(Category_Mismatch(ctgr,pcrobj))
   else 
   close pcrobj;;

end;;






module Assistance_double_partial_crobj=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/double_partial_crobj.ml";;

*)

exception Close_on_separator;;
exception Redundant_separator;;
exception Category_Mismatch of Assistance_crobj_category_t.t * Assistance_partial_crobj_t.t;;
exception End_reached of Assistance_concrete_object_t.t ;;

module Private = struct 

let push_int i (Assistance_double_partial_crobj_t.Double(_,last_opened,opened_before))=
  (Assistance_double_partial_crobj_t.Double(false,
    Assistance_partial_crobj.push_int i last_opened,opened_before));;

let push_string encoded_s (Assistance_double_partial_crobj_t.Double(_,last_opened,opened_before))=
  (Assistance_double_partial_crobj_t.Double(false,
    Assistance_partial_crobj.push_string encoded_s last_opened,opened_before));;    

let push_separator ctgr (Assistance_double_partial_crobj_t.Double(separator_present,last_opened,opened_before))=
  if separator_present
  then raise(Redundant_separator)
  else 
        let ctgr2 = Assistance_partial_crobj.category last_opened in 
        if ctgr <> ctgr2
        then raise(Category_Mismatch(ctgr,last_opened))
        else  (Assistance_double_partial_crobj_t.Double(true,last_opened,opened_before));;

let push_field_name record_name (Assistance_double_partial_crobj_t.Double(_,last_opened,opened_before))=
  (Assistance_double_partial_crobj_t.Double(false,
    Assistance_partial_crobj.push_field_name record_name last_opened,opened_before));;    

let open_new opening 
   (Assistance_double_partial_crobj_t.Double(_,last_opened,opened_before))=
    Assistance_double_partial_crobj_t.Double(false,
      Assistance_partial_crobj.initialize opening,last_opened::opened_before);;



let close ctgr
    (Assistance_double_partial_crobj_t.Double(separator_present,last_opened,opened_before))=
    if separator_present 
    then raise(Close_on_separator)
    else 
    let newfound=Assistance_partial_crobj.check_category_and_close ctgr last_opened in 
    match opened_before with 
    []->raise(End_reached(newfound))
    |next_opened_one::others ->
      let new_frontier = Assistance_partial_crobj.push_one_more_item newfound next_opened_one in 
      Assistance_double_partial_crobj_t.Double(false,new_frontier,others);;

end ;; 

let initialize opening = 
    Assistance_double_partial_crobj_t.Double(false,Assistance_partial_crobj.initialize opening,[]);;

let increase = function 
   Assistance_crobj_basic_increase_t.Push_int(i)->Private.push_int i 
    |Push_string(encoded_s)->Private.push_string encoded_s 
    |Push_field_name(rcdname)->Private.push_field_name rcdname
    |Open(opening) -> Private.open_new opening
    |Separate(cat) -> Private.push_separator cat 
    |Close(cat) -> Private.close cat;;
        






end;;






module Assistance_crobj_parsing=struct

(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/crobj_parsing.ml";;


*)

exception Unreadable of int * string ;;

module Private = struct

let salt = Assistance_encoded_string.salt ;; 

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


let array_cat = Assistance_crobj_category_t.Array
and list_cat  = Assistance_crobj_category_t.List
and record_cat = Assistance_crobj_category_t.Record 
and uple_cat = Assistance_crobj_category_t.Uple 
and variant_cat = Assistance_crobj_category_t.Variant;;

let list_for_category_of_lexeme=[
    (array_opener,array_cat); 
    (list_opener,list_cat); 
    (record_opener,record_cat); 
    (uple_opener,uple_cat); 
    (variant_opener,variant_cat); 

    (array_separator,array_cat); 
    (list_separator,list_cat); 
    (record_separator,record_cat); 
    (uple_separator,uple_cat); 
    (variant_separator,variant_cat); 

    (array_closer,array_cat); 
    (list_closer,list_cat); 
    (record_closer,record_cat); 
    (uple_closer,uple_cat); 
    (variant_closer,variant_cat); 

    (record_arrow,record_cat); (* a little bit of convenient convention here *)
];;

let category_of_lexeme lexeme=List.assoc lexeme list_for_category_of_lexeme;;

let list_for_preludeless_increasers=[
    (array_opener,Assistance_crobj_basic_increase_t.Open(Assistance_crobj_opening_t.Array)); 
    (list_opener,Assistance_crobj_basic_increase_t.Open(Assistance_crobj_opening_t.List)); 
    (record_opener,Assistance_crobj_basic_increase_t.Open(Assistance_crobj_opening_t.Record)); 
    (uple_opener,Assistance_crobj_basic_increase_t.Open(Assistance_crobj_opening_t.Uple)); 

    (array_separator,Assistance_crobj_basic_increase_t.Separate(Assistance_crobj_category_t.Array)); 
    (list_separator,Assistance_crobj_basic_increase_t.Separate(Assistance_crobj_category_t.List)); 
    (record_separator,Assistance_crobj_basic_increase_t.Separate(Assistance_crobj_category_t.Record)); 
    (uple_separator,Assistance_crobj_basic_increase_t.Separate(Assistance_crobj_category_t.Uple)); 
    (variant_separator,Assistance_crobj_basic_increase_t.Separate(Assistance_crobj_category_t.Variant)); 

    (array_closer,Assistance_crobj_basic_increase_t.Close(Assistance_crobj_category_t.Array)); 
    (list_closer,Assistance_crobj_basic_increase_t.Close(Assistance_crobj_category_t.List)); 
    (record_closer,Assistance_crobj_basic_increase_t.Close(Assistance_crobj_category_t.Record)); 
    (uple_closer,Assistance_crobj_basic_increase_t.Close(Assistance_crobj_category_t.Uple)); 
    (variant_closer,Assistance_crobj_basic_increase_t.Close(Assistance_crobj_category_t.Variant)); 

];;

exception Unreadable_int of string;;

let parse_int s=try int_of_string s with _->raise(Unreadable_int(s));;

let next_basic_increase_in_variant_opening_case s idx idx1=
   let opening = Assistance_crobj_opening_t.Variant (Assistance_cull_string.interval s idx (idx1-1)) in 
   (Assistance_crobj_basic_increase_t.Open(opening),idx1+(String.length variant_opener));;

let next_basic_increase_in_field_naming_case s idx idx1=
   let name = Assistance_cull_string.interval s idx (idx1-1) in 
   (Assistance_crobj_basic_increase_t.Push_field_name(name),idx1+(String.length record_arrow));;

let next_basic_increase_in_preludy_case s idx idx1=
   if Assistance_substring.is_a_substring_located_at variant_opener s idx1 
   then next_basic_increase_in_variant_opening_case s idx idx1
   else 
   if Assistance_substring.is_a_substring_located_at record_arrow s idx1 
   then next_basic_increase_in_field_naming_case s idx idx1
   else let i=parse_int(Assistance_cull_string.interval s idx (idx1-1)) in 
       (Assistance_crobj_basic_increase_t.Push_int(i),idx1);;


exception Missing_string_closer of int * string;;

let next_basic_increase_in_push_string_case s idx=
   let idx1=idx+(String.length string_opener) in 
   let idx2=Assistance_substring.leftmost_index_of_in_from string_closer s idx1 in 
   if idx2<0
   then raise(Missing_string_closer(idx1,s))
   else
   (* we know that the string is already encoded *)
   let encoded_s = Assistance_encoded_string.retrieve (Assistance_cull_string.interval s idx1 (idx2-1)) in 
   (Assistance_crobj_basic_increase_t.Push_string(encoded_s),idx2+(String.length string_closer));;




exception Unreadable_increase of int * string ;;

let next_basic_increase  s idx=
   let idx1= Assistance_substring.leftmost_index_of_in_from salt s idx in 
   if idx1<0 
   then let i=parse_int (Assistance_cull_string.cobeginning (idx-1) s) in
        (Assistance_crobj_basic_increase_t.Push_int(i),String.length(s)+1)
   else      
   if idx1>idx 
   then next_basic_increase_in_preludy_case s idx idx1
   else 
   if Assistance_substring.is_a_substring_located_at string_opener s idx 
   then next_basic_increase_in_push_string_case s idx
   else 
   match Assistance_option.seek (fun 
      (text,action)->Assistance_substring.is_a_substring_located_at text s idx
   ) list_for_preludeless_increasers with 
   None -> raise(Unreadable_increase(idx,s))
   |Some(text,action)->(action,idx+(String.length text));;

let one_step_more machine =
   let (action,next_idx) =
      next_basic_increase machine.Assistance_crobj_parsing_machine_t.parsed_one 
                            machine.Assistance_crobj_parsing_machine_t.current_index in 
   {
      machine with 
      Assistance_crobj_parsing_machine_t.current_index = next_idx ;
      Assistance_crobj_parsing_machine_t.data = Assistance_double_partial_crobj.increase action machine.Assistance_crobj_parsing_machine_t.data;
   }  ;;

let prudent_push machine = try (None,Some(one_step_more machine)) with 
   Assistance_double_partial_crobj.End_reached(solution) -> (Some solution,None);;

exception First_step_exn of Assistance_crobj_basic_increase_t.t ;; 

let first_step s =
   let (action,next_idx) = next_basic_increase s 1 in 
   match action with 
    Assistance_crobj_basic_increase_t.Push_int(i)->(Some(Assistance_concrete_object_t.Int(i)),None,next_idx)
   |Assistance_crobj_basic_increase_t.Push_string(encoded_s)->(Some(Assistance_concrete_object_automatic.wrap_encoded_string(encoded_s)),None,next_idx)
   |Assistance_crobj_basic_increase_t.Open(opening)->(None,Some(Assistance_double_partial_crobj.initialize(opening)),next_idx)
   |_->raise(First_step_exn(action));;

exception Ends_too_soon of Assistance_concrete_object_t.t * string ;; 

let parse s =
    let (opt_quick_result,opt_start,next_idx) = first_step s in 
    match opt_quick_result with 
    Some (res)-> if next_idx < (String.length s)
                 then raise(Ends_too_soon(res,s)) 
                 else res 
    |None -> let start_partial_obj = Assistance_option.unpack opt_start in   
             let machine = {
                Assistance_crobj_parsing_machine_t.parsed_one = s ;
                Assistance_crobj_parsing_machine_t.current_index = next_idx ;
                Assistance_crobj_parsing_machine_t.data = start_partial_obj;
             } in 
             let rec iterator = (fun mach ->
                let (opt_sol,opt_term) = prudent_push mach in 
                match opt_term with 
                None -> Assistance_option.unpack opt_sol 
                |Some(term)->iterator(term) 
             ) in 
             iterator machine;;

let rec unparse = function 
   Assistance_concrete_object_t.Int(i)->string_of_int i 
   |String(t)->string_opener^(Assistance_encoded_string.store t)^string_closer
   |Uple(l)->let temp1=Assistance_image.image unparse l in 
             uple_opener^(String.concat uple_separator temp1)^uple_closer
   |List(l)->let temp1=Assistance_image.image unparse l in 
             list_opener^(String.concat list_separator temp1)^list_closer 
   |Array(l)->let temp1=Assistance_image.image unparse l in 
             array_opener^(String.concat array_separator temp1)^array_closer
   |Record(l)->let temp1=Assistance_image.image (fun (key,vaal)->key ^ record_arrow ^ (unparse vaal))  l in 
             record_opener^(String.concat record_separator temp1)^record_closer          
   |Variant(constructor,l)->let temp1=Assistance_image.image unparse l in 
             constructor^variant_opener^(String.concat variant_separator temp1)^variant_closer ;; 

end;;

let parse = Private.parse;;
let unparse = Private.unparse;;

end;;






module Assistance_dfn_middle=struct

(*

#use"Decomposed_filename/dfn_middle.ml";;

*)


let to_line (Assistance_dfn_middle_t.J(s,m)) = (Assistance_dfa_subdirectory.connectable_to_subpath s)^ (Assistance_dfa_module.to_line m);;
let to_module (Assistance_dfn_middle_t.J(s,m))=m;;

end;;






module Assistance_find_suitable_ending=struct

(*

#use"find_suitable_ending.ml";;

*)

(*

Note that the order in Ocaml_ending.correspondances is important

*)

exception No_suitable_location of Assistance_dfa_root_t.t*(Assistance_dfa_subdirectory_t.t list)*string;;

let find_file_location dir l_subdir old_x=
  let x=String.uncapitalize_ascii old_x in
  let s_dir=Assistance_dfa_root.connectable_to_subpath(dir) in
  let original_endings=Assistance_image.image Assistance_dfa_ending.connectable_to_modulename Assistance_dfa_ending.all_ocaml_endings in
  let endings=(
     if List.exists (fun edg->Assistance_supstring.ends_with x edg) original_endings
     then [""]
     else original_endings
  ) in
  let temp1=Assistance_cartesian.product(l_subdir) endings in
  let tempf=(fun (sd,edg)->
  	let s1=s_dir^(Assistance_dfa_subdirectory.connectable_to_subpath sd)^x^edg in
  	if Sys.file_exists s1
  	then Some(Assistance_absolute_path.of_string s1)
  	else None
  ) in
  let opt=Assistance_option.find_and_stop tempf temp1 in
  if opt=None
  then raise(No_suitable_location(dir ,l_subdir,x))
  else  Assistance_option.unpack(opt);;           

end;;






module Assistance_memoized=struct

(*

#use"memoized.ml";;

*) 
type ('a,'b) map=('a->'b);;

let make_from (f:'a->'b) (a_hashtbl_for_f:('a,'b) Hashtbl.t)=
  let memoized_f=(fun x->
     if Hashtbl.mem(a_hashtbl_for_f)(x)
     then Hashtbl.find(a_hashtbl_for_f)(x)
     else let y=f(x) in
          let ()=(Hashtbl.add(a_hashtbl_for_f) x y) in
          y
  ) in
  (memoized_f:>('a,'b) map);;

let make (f:'a->'b)=
  let a_hashtbl_for_f=Hashtbl.create(100) in
  make_from f a_hashtbl_for_f;;
  
let recursive_from=((fun (big_f:('a->'b)->'a->'b) (a_hashtbl_for_f:('a,'b) Hashtbl.t)->
  let rec memoized_f=(fun x->
     if Hashtbl.mem(a_hashtbl_for_f)(x)
     then Hashtbl.find(a_hashtbl_for_f)(x)
     else let mf=(memoized_f:>('a->'b)) in
          let y=big_f(mf)(x) in
          let ()=(Hashtbl.add(a_hashtbl_for_f) x y) in
          y
  ) in
  memoized_f):>(('a->'b)-> 'a -> 'b) -> (('a,'b) Hashtbl.t) -> ('a, 'b) map);;

let recursive (big_f:('a->'b)->'a->'b)=
  let a_hashtbl_for_f=Hashtbl.create(100) in
  recursive_from big_f a_hashtbl_for_f;;

let small f initial_value=
  recursive(fun old_f k->if k<1 then initial_value else f(old_f(k-1)));;
  
let reversible (f:'a->'b)=
  let a_hashtbl_for_f=Hashtbl.create(100) 
  and a_hashtbl_for_the_inverse_of_f=Hashtbl.create(100)
  and a_hashtbl_for_the_second_inverse_of_f=Hashtbl.create(100)
  and a_hashtbl_for_the_projector=Hashtbl.create(50) 
  and irreducibles=ref([]) 
  and minimal_reductions=ref([]) in
  let compute_f=(fun x accu->
     let y=f(x) in
     let ()=(Hashtbl.add(a_hashtbl_for_f) x y;accu:=[y]) in
      if Hashtbl.mem(a_hashtbl_for_the_second_inverse_of_f)(y)
     then let old_x=Hashtbl.find(a_hashtbl_for_the_inverse_of_f)(y) in
          Hashtbl.add(a_hashtbl_for_the_projector)(x)(old_x)
     else     
     if Hashtbl.mem(a_hashtbl_for_the_inverse_of_f)(y)
     then let old_x=Hashtbl.find(a_hashtbl_for_the_inverse_of_f)(y) in
          (Hashtbl.add(a_hashtbl_for_the_projector)(x)(old_x);
          Hashtbl.add(a_hashtbl_for_the_second_inverse_of_f)(y)(x);
          minimal_reductions:=(x,old_x)::(!minimal_reductions))
     else (Hashtbl.add(a_hashtbl_for_the_inverse_of_f)(y)(x);
            irreducibles:=x::(!irreducibles))
     
  ) in
  let memoized_f=(fun x->
     if Hashtbl.mem(a_hashtbl_for_f)(x)
     then Hashtbl.find(a_hashtbl_for_f)(x)
     else let accu=ref([]) in
          let _=compute_f(x)(accu) in
          List.hd(!accu)
  ) 
  and memoized_inverse_of_f=Hashtbl.find(a_hashtbl_for_the_inverse_of_f) in
  let memoized_projector=(fun x->
    let ()=compute_f(x)(ref[]) in
    if Hashtbl.mem(a_hashtbl_for_the_projector)(x)
    then Hashtbl.find(a_hashtbl_for_the_projector)(x)
    else x
    ) in
  (memoized_f,memoized_inverse_of_f,memoized_projector,irreducibles,minimal_reductions);;
           

end;;






module Assistance_reconstruct_linear_poset=struct

(*

#use"reconstruct_linear_poset.ml";;

Computes the (canonical) maximal acyclic sub-poset of a given poset, returns
it as a list L where each element of L is a triple (a,anc_a,a_is_clean)
where anc_a is the list of all ancestors of a, ordered as in L, and a_is_clean
is a boolean indicating if a is the ancestor or a descendant of an "active"
element.

Also returns a (non-canonical,non-exhaustive) set of cycles.


*)


let iterator coat 
  (checked,checked_union,cycles,cycles_union,between,not_yet_checked,opt1)=
    (* 
    between is a "chained" list of pairs (x1,x2),(x2,x3), ...
    (stocked in reverse actually)
    that will possibly lead to a cycle
    *)
    if opt1<>None then ([],Assistance_set_of_polys.empty_set,[],Assistance_set_of_polys.empty_set,[],[],opt1) else
    if (between,not_yet_checked)=([],[]) 
    then ([],Assistance_set_of_polys.empty_set,[],Assistance_set_of_polys.empty_set,[],[],
          Some(cycles,Assistance_listennou.rev_map (fun (z,p)->(z,fst p)) checked)) 
    else
    let a=
    	  (if between=[] 
    	   then List.hd(not_yet_checked)
           else snd(List.hd(between))
          ) in
    let not_yet_checked2=List.filter (fun z->z<>a) not_yet_checked in
    let coat_a=coat(a) in
    let coatoms_of_a=Assistance_set_of_polys.safe_set(coat_a) in
    let temp1=Assistance_set_of_polys.setminus coatoms_of_a checked_union in
    if Assistance_set_of_polys.length(temp1)=0
    then let temp3=coatoms_of_a::(Assistance_image.image (fun z->snd(List.assoc z checked)) 
                      (coat_a)) in
         let ordered_set_version=Assistance_set_of_polys.fold_merge(temp3) in
         let temp4=Assistance_option.filter_and_unpack (
           fun (b,_)->if Assistance_set_of_polys.mem b ordered_set_version
             then Some(b)
             else None
         ) checked in
         let list_version=List.rev(temp4) in
         let data_for_a=(list_version,ordered_set_version) in
         ((a,data_for_a)::checked,Assistance_set_of_polys.insert a checked_union,
         cycles,cycles_union,[],not_yet_checked2,None)
    else
    if Assistance_set_of_polys.mem a temp1
    then ([],Assistance_set_of_polys.empty_set,[a]::cycles,Assistance_set_of_polys.insert a cycles_union,
         [],not_yet_checked2,None) 
    else 
    if (not(Assistance_set_of_polys.does_not_intersect temp1 cycles_union))
    then (checked,checked_union,cycles,Assistance_set_of_polys.insert a cycles_union,
         [],not_yet_checked2,None) 
    else 
    (*see if we can close the cycle *)
    match Assistance_option.seek(fun (x,y)->Assistance_set_of_polys.mem x temp1) between with
     None->(checked,checked_union,cycles,cycles_union,
     		(a,Assistance_set_of_polys.hd temp1)::between,not_yet_checked,None)
    |Some(p)->
        let (before,_,after)=Assistance_three_parts.select_center_element_and_reverse_left (fun x->x=p) between in
        let temp2=Assistance_image.image fst before in
        let new_cycle=(fst p)::(temp2@[a]) in
        let ordered_cycle=Assistance_set_of_polys.sort new_cycle in
        let not_yet_checked3=List.filter (fun z->Assistance_set_of_polys.nmem z ordered_cycle) not_yet_checked in
        (checked,checked_union,new_cycle::cycles,
        Assistance_set_of_polys.merge ordered_cycle cycles_union,
        [],not_yet_checked3,None);;

let reconstruct_linear_poset coat l=
  let rec tempf=(fun
  (checked,checked_union,cycles,cycles_union,between,not_yet_checked,opt)->
    if opt<>None
    then Assistance_option.unpack opt
    else tempf(iterator coat 
    (checked,checked_union,cycles,cycles_union,between,not_yet_checked,opt))
    ) in
    tempf([],Assistance_set_of_polys.empty_set,[],Assistance_set_of_polys.empty_set,[],l,None);;
    
(*

let sugar i=
   if (i<4)||(i>20) then [] else
   if i=11 then [5] else [i+1];;
    
reconstruct_linear_poset sugar (ennig 1 30);;  

let some_edges=
  [
    (1,4);(1,16);(2,6);(2,7);(3,16);(7,11);(7,19);(8,11);(8,15);(9,19);
    (10,1);(10,18);(11,2);(12,16);(13,5);(15,13);(16,17);(17,10);(17,14);(19,20);
    (20,21);(21,9)
  
  ];;

let brown j=image fst (List.filter (fun x->snd(x)=j) some_edges);;

reconstruct_linear_poset brown (ennig 1 21);;  


*)
 
    
    
    
               

end;;






module Assistance_set_of_strings_t=struct

(* 

#use"Ordered_Lists/set_of_strings_t.ml";;

*)

type t=S of string list;;


end;;






module Assistance_set_of_strings=struct

(* 

#use"Ordered_Lists/set_of_strings.ml";;

*)

let tr = ((fun x->Assistance_set_of_strings_t.S(x)),(fun (Assistance_set_of_strings_t.S(x))->x),Assistance_total_ordering.silex_for_strings);;


let forget_order x= Assistance_functor_for_sets.forget_order tr x;;
let image f x= Assistance_functor_for_sets.image tr f x;;
let safe_set l= Assistance_functor_for_sets.safe_set tr l;;
let sort l= Assistance_functor_for_sets.sort tr l;;





end;;






module Assistance_coma_state=struct

(* 
#use"Compilation_management/coma_state.ml";;
*)

(* Inherited values *)


let frontier_with_unix_world = Assistance_coma_state_automatic.frontier_with_unix_world;;
let root =Assistance_coma_state_automatic.root;;
let backup_dir =Assistance_coma_state_automatic.backup_dir;;
let gitpush_after_backup =Assistance_coma_state_automatic.gitpush_after_backup;;
let github_url =Assistance_coma_state_automatic.github_url;;
let encoding_protected_files =Assistance_coma_state_automatic.encoding_protected_files;;

let subdir_at_module = Assistance_coma_state_automatic.subdir_at_module ;;
let principal_ending_at_module = Assistance_coma_state_automatic.principal_ending_at_module ;;
let mli_presence_at_module = Assistance_coma_state_automatic.mli_presence_at_module ;;
let principal_mt_at_module = Assistance_coma_state_automatic.principal_mt_at_module ;;
let mli_mt_at_module = Assistance_coma_state_automatic.mli_mt_at_module ;;
let needed_libs_at_module  = Assistance_coma_state_automatic.needed_libs_at_module ;;
let direct_fathers_at_module = Assistance_coma_state_automatic.direct_fathers_at_module ;;
let ancestors_at_module = Assistance_coma_state_automatic.ancestors_at_module ;; 
let needed_dirs_at_module  = Assistance_coma_state_automatic.needed_dirs_at_module ;;
let product_up_to_date_at_module = Assistance_coma_state_automatic.product_up_to_date_at_module ;;
let directories = Assistance_coma_state_automatic.directories;;
let preq_types = Assistance_coma_state_automatic.preq_types;;


let set_frontier_with_unix_world = Assistance_coma_state_automatic.set_frontier_with_unix_world;;
let set_subdir_at_module = Assistance_coma_state_automatic.set_subdir_at_module ;;
let set_principal_ending_at_module = Assistance_coma_state_automatic.set_principal_ending_at_module ;;
let set_mli_presence_at_module = Assistance_coma_state_automatic.set_mli_presence_at_module ;;
let set_principal_mt_at_module = Assistance_coma_state_automatic.set_principal_mt_at_module ;;
let set_mli_mt_at_module = Assistance_coma_state_automatic.set_mli_mt_at_module ;;
let set_needed_libs_at_module  = Assistance_coma_state_automatic.set_needed_libs_at_module ;;
let set_direct_fathers_at_module = Assistance_coma_state_automatic.set_direct_fathers_at_module ;;
let set_ancestors_at_module = Assistance_coma_state_automatic.set_ancestors_at_module ;; 

let set_needed_dirs_at_module  = Assistance_coma_state_automatic.set_needed_dirs_at_module ;;
let set_product_up_to_date_at_module = Assistance_coma_state_automatic.set_product_up_to_date_at_module ;;
let set_directories = Assistance_coma_state_automatic.set_directories;;
let set_preq_types = Assistance_coma_state_automatic.set_preq_types;;


let ordered_list_of_modules = Assistance_coma_state_automatic.ordered_list_of_modules;;
let follows_it = Assistance_coma_state_automatic.follows_it_but_does_not_necessarily_depend_on_it;;
let all_used_subdirs = Assistance_coma_state_automatic.all_used_subdirs;;




(* End of inherited values *)


let endingless_at_module cs mn=
   Assistance_dfn_endingless_t.J(
        root cs,
        subdir_at_module cs mn,
        mn
    );;


let endingless_from_mildly_capitalized_module_name cs mname=
    endingless_at_module cs (Assistance_dfa_module.of_line(String.capitalize_ascii mname));;

let check_ending_in_at_module edg cs mn=
   if edg=principal_ending_at_module cs mn
   then true 
   else 
   if edg=Assistance_dfa_ending.mli
   then mli_presence_at_module cs mn
   else false;;



let acolytes_at_module cs mn=
  let eless = endingless_at_module cs mn in
  Assistance_option.filter_and_unpack (fun 
  edg->
     if check_ending_in_at_module edg cs mn
     then Some(Assistance_dfn_join.to_ending eless edg)
     else None
) Assistance_dfa_ending.all_ocaml_endings;;



let rootless_lines_at_module cs mn=
   Assistance_image.image Assistance_dfn_full.to_rootless_line (acolytes_at_module cs mn);;
  

let rootless_paths_at_module cs mn=
   Assistance_image.image Assistance_dfn_full.to_rootless (acolytes_at_module cs mn);;
  


let registered_endings_at_module cs mn=
  List.filter (fun edg->
  check_ending_in_at_module edg cs mn 
  ) Assistance_dfa_ending.all_ocaml_endings;;



let check_for_single_ending_at_module cs mn=
  if mli_presence_at_module cs mn
  then (principal_ending_at_module cs mn)=(Assistance_dfa_ending.mli)
  else true ;;



let size cs = List.length (ordered_list_of_modules cs);;      


let up_to_date_elesses cs =
   Assistance_option.filter_and_unpack (
     fun mn->
       if product_up_to_date_at_module cs mn
       then Some(endingless_at_module cs mn)
       else None
   )(ordered_list_of_modules cs);;

exception Find_subdir_from_suffix_exn of string * (Assistance_dfa_subdirectory_t.t list) ;;

let find_subdir_from_suffix cs possibly_slashed_suffix =
  let suffix = Assistance_cull_string.trim_slashes_on_the_right possibly_slashed_suffix  in
  let temp1 = List.filter (
    fun subdir -> Assistance_supstring.contains (Assistance_dfa_subdirectory.without_trailing_slash subdir) suffix
  ) (cs.Assistance_coma_state_t.directories) in 
  let test_for_minimality = (fun subdir1->
     List.for_all (fun subdir2 ->
        if subdir2 = subdir1 then true else 
        not(Assistance_dfa_subdirectory.begins_with subdir1 subdir2) 
     ) temp1
  ) in 
  let temp2 = List.filter test_for_minimality temp1 in 
  if List.length(temp2)<>1
  then raise(Find_subdir_from_suffix_exn(suffix,temp2))
  else let (Assistance_dfa_subdirectory_t.SD container) = List.hd temp2 in 
       let j1 = Assistance_substring.leftmost_index_of_in suffix container in 
       let j2 = j1 + (String.length suffix) -1 in 
       Assistance_dfa_subdirectory.of_line(Assistance_cull_string.beginning j2 container);;
  
let compute_long_subdir_name cs old_subdir new_subdir_short_name =
   let temp1 =  Assistance_cull_string.trim_slashes_on_the_right new_subdir_short_name in
   let long_name = (
   if String.contains temp1 '/'
   then temp1 
   else let old_subdir_name = Assistance_dfa_subdirectory.without_trailing_slash old_subdir in 
        let father_name = Assistance_cull_string.before_rightmost old_subdir_name '/' in 
        if father_name = ""
        then temp1
        else father_name^"/"^temp1 ) in 
   Assistance_dfa_subdirectory.of_line long_name ;;       

let modules_with_their_ancestors cs l=
   let temp1=List.filter (
     fun nm->List.mem nm l 
     ) (ordered_list_of_modules cs )   in 
   let temp2=Assistance_image.image (
     fun nm->
       (ancestors_at_module cs nm)@[nm] 
   ) temp1 in 
   let temp3=List.flatten temp2 in 
   Assistance_listennou.nonredundant_version temp3;;

let find_needed_data_for_file cs fn=
      let temp1=Assistance_look_for_module_names.names_in_mlx_file fn in
      List.filter (
         fun mn->List.mem mn temp1  
      )(ordered_list_of_modules cs);;

let  find_needed_data cs rless=
   let full_version = Assistance_dfn_join.root_to_rootless (root cs) rless in 
   let fn=Assistance_dfn_full.to_absolute_path full_version in
      find_needed_data_for_file cs fn;;    

 

let needed_dirs_and_libs_in_command cmod cs mn=
   let extension=(if cmod=Assistance_compilation_mode_t.Executable then ".cmxa" else ".cma") in
   let s_root=Assistance_dfa_root.connectable_to_subpath(root cs) in
   let dirs=
   "-I "^s_root^(Assistance_dfa_subdirectory.connectable_to_subpath(Assistance_compilation_mode.workspace cmod))
  and libs=String.concat(" ")
    (Assistance_image.image(fun z->Assistance_ocaml_library.file_for_library(z)^extension)
    (needed_libs_at_module cs mn)) in
    String.concat " " ["";dirs;libs;""];;

let all_endinglesses cs=
  Assistance_image.image (endingless_at_module cs) (ordered_list_of_modules cs);; 

let get_modification_time cs mn edg=
  if edg=principal_ending_at_module cs mn then principal_mt_at_module cs mn else 
  if edg=Assistance_dfa_ending.mli then mli_mt_at_module cs mn else 
  "0.";;

exception Non_existent_mtime of Assistance_dfn_full_t.t;;

let force_modification_time root_dir cs mlx=
      let edg=Assistance_dfn_full.to_ending mlx in
      let nm=Assistance_dfn_full.to_module mlx in
      let file=Assistance_dfn_full.to_line mlx in 
      let new_val=string_of_float((Unix.stat file).Unix.st_mtime)  in
      let cs2=(
        if edg=principal_ending_at_module cs nm 
        then set_principal_mt_at_module cs nm new_val
        else cs
      ) in
      let cs3=(
        if edg=Assistance_dfa_ending.mli
        then set_mli_mt_at_module cs2 nm new_val
        else cs2
      ) in     
      cs3;;


exception Non_registered_module of Assistance_dfn_endingless_t.t;;  
exception Derelict_children of Assistance_dfa_module_t.t*(Assistance_dfa_module_t.t list);;  
           
            
let unregister_module cs eless=
  let nm=Assistance_dfn_endingless.to_module eless in
  let pre_desc=List.filter(
      fun mn7->
       List.mem nm ( ancestors_at_module cs mn7 )
  ) (ordered_list_of_modules cs) in
   if pre_desc<>[]
   then raise(Derelict_children(nm,pre_desc))
   else
   let cs2=Assistance_coma_state_automatic.remove_in_each_at_module cs nm in
   let old_preqtypes = Assistance_coma_state_automatic.preq_types cs2 in 
   let new_preqtypes = List.filter (fun (eless2,_)->eless2<>eless ) old_preqtypes in 
   let cs3=(
     if new_preqtypes <> old_preqtypes 
     then Assistance_coma_state_automatic.set_preq_types cs2 new_preqtypes
     else cs2
   ) in 
   cs3;;     
                    
let unregister_modules cs elesses = List.fold_left unregister_module cs elesses ;; 


exception Non_registered_file of Assistance_dfn_full_t.t;;  
exception Abandoned_children of Assistance_dfn_full_t.t * (Assistance_dfa_module_t.t list);;
                      
                     
let partially_remove_mlx_file cs mlxfile=
    let eless=Assistance_dfn_full.to_endingless mlxfile
    and nm=Assistance_dfn_full.to_module mlxfile in
    let pre_desc=List.filter(
      fun mn7->
      List.mem nm ( ancestors_at_module cs mn7)
    ) (ordered_list_of_modules cs) in
    if pre_desc<>[]
    then raise(Abandoned_children(mlxfile,pre_desc))
    else
    let edg=Assistance_dfn_full.to_ending mlxfile in
    if (not(check_ending_in_at_module edg cs nm))
    then raise(Non_registered_file(mlxfile))
    else if check_for_single_ending_at_module cs nm
         then let cs5=Assistance_coma_state_automatic.remove_in_each_at_module cs nm in 
              let old_preqtypes = Assistance_coma_state_automatic.preq_types cs5 in 
              let new_preqtypes = List.filter (fun (eless2,_)->eless2<>eless ) old_preqtypes in 
              let cs6=(
                if new_preqtypes <> old_preqtypes 
                then Assistance_coma_state_automatic.set_preq_types cs5 new_preqtypes
                else cs5
              ) in 
              cs6
         else (* if we get here, there are two registered endings, one of which
              is the mli *) 
              if edg=(Assistance_dfa_ending.mli)
              then (
                       let cs3=set_mli_presence_at_module cs nm false in 
                       set_mli_mt_at_module cs3 nm "0."
                   )
               else 
                     let old_mt=principal_mt_at_module cs nm in
                     (
                      let cs4=set_principal_ending_at_module cs nm (Assistance_dfa_ending.mli) in 
                      set_principal_mt_at_module cs4 nm old_mt
                    );;
            


let compute_subdirectories_list cs=
  let temp1=Assistance_image.image Assistance_dfa_subdirectory.without_trailing_slash (all_used_subdirs cs) in
    let temp2=Assistance_set_of_strings.sort temp1 in
    let temp3=Assistance_set_of_strings.forget_order temp2 in
    Assistance_image.image Assistance_dfa_subdirectory.of_line temp3;;

let  check_registrations cs eless=
   let mn=Assistance_dfn_endingless.to_module eless in 
   Assistance_dfa_ending.compute_on_all_ocaml_endings 
      (fun edg->check_ending_in_at_module edg cs mn);;


module PrivateTwo=struct


let find_needed_libraries cs rless ordered_ancestors=
  let full_version=Assistance_dfn_join.root_to_rootless (root cs) rless in
  let fn=Assistance_dfn_full.to_absolute_path full_version in
  let temp1=Assistance_look_for_module_names.names_in_mlx_file fn in
  List.filter
  (
    fun lib->
      if List.exists 
         (fun mdl->List.mem(Assistance_dfa_module.of_line mdl)(temp1))
           (Assistance_ocaml_library.modules_telling_a_library_away lib)
      then true
      else List.exists 
           (fun mn->
            List.mem lib (needed_libs_at_module cs mn) ) 
           ordered_ancestors
  )
  Assistance_ocaml_library.all_libraries;;


let find_needed_directories cs rless ordered_ancestors=
  let temp1=Assistance_image.image (fun mn->
    Assistance_set_of_polys.sort(needed_dirs_at_module cs mn)) ordered_ancestors in
  let subdir_in_mlx=Assistance_dfn_rootless.to_subdirectory rless in
  let temp2=(
      if subdir_in_mlx<>Assistance_dfa_subdirectory.main 
      then Assistance_set_of_polys.singleton(subdir_in_mlx)::temp1
      else temp1
  ) in    
  let temp3=Assistance_set_of_polys.fold_merge temp2 in
  Assistance_set_of_polys.forget_order temp3;;              
                    
end;;  

let compute_principal_ending (mlr,mlir,mllr,mlyr)=
    let temp1=List.combine
      [mlr;mllr;mlyr]
      [Assistance_dfa_ending.ml;Assistance_dfa_ending.mll;Assistance_dfa_ending.mly] in
    let temp2=Assistance_option.filter_and_unpack (
       fun (bowl,ending)->if bowl then Some(ending) else None 
    ) temp1 in
    if temp2=[] then Assistance_dfa_ending.mli else List.hd temp2;;

let md_compute_modification_time hm edg=
  let mlx=Assistance_dfn_join.to_ending hm edg in
  let file=Assistance_dfn_full.to_line mlx in 
  if not(Sys.file_exists file) then "0." else
  let st=Unix.stat file in
  string_of_float(st.Unix.st_mtime);;

let md_compute_modification_times hm=
      Assistance_dfa_ending.compute_on_all_ocaml_endings (md_compute_modification_time hm);;
    
let md_associated_modification_time  (ml_mt,mli_mt,mll_mt,mly_mt) edg=
  match Assistance_dfa_ending.restrict_to_ocaml_ending edg with
     Assistance_dfa_ocaml_ending_t.Ml->ml_mt
    |Mli->mli_mt
    |Mll->mll_mt
    |Mly->mly_mt;;  

let complete_info cs  rless=
  let middle = Assistance_dfn_rootless.to_middle rless in 
  let hm=Assistance_dfn_join.root_to_middle (root cs) middle in
  let modules_written_in_file=find_needed_data cs rless in
  let (mlr,mlir,mllr,mlyr)=check_registrations cs hm
  and (mlmt,mlimt,mllmt,mlymt)=md_compute_modification_times hm in
  let pr_end=compute_principal_ending (mlr,mlir,mllr,mlyr) in
  let prmt=md_associated_modification_time (mlmt,mlimt,mllmt,mlymt) pr_end in
  let temp1=Assistance_image.image 
          (fun mn->
           Assistance_set_of_polys.sort(ancestors_at_module cs mn)) 
          modules_written_in_file in
  let temp2=Assistance_set_of_polys.fold_merge ((Assistance_set_of_polys.sort(modules_written_in_file) )::temp1) in
  let tempf=(fun mn->
              if Assistance_set_of_polys.mem mn temp2
              then Some(mn)
              else None) in
  let allanc=Assistance_option.filter_and_unpack tempf (ordered_list_of_modules cs) in
  let libned=PrivateTwo.find_needed_libraries cs rless modules_written_in_file
  and dirned=PrivateTwo.find_needed_directories cs rless modules_written_in_file in
  (hm,pr_end,mlir,prmt,mlimt,libned,modules_written_in_file,allanc,dirned,false);;

let update_just_one_module cs rootless =
    let mn = Assistance_dfn_rootless.to_module rootless in 
    if not(List.mem mn (ordered_list_of_modules cs))
    then cs 
    else let (_,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,is_updated)=complete_info cs rootless in 
         Assistance_coma_state_automatic.set_in_each cs mn (pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,is_updated);;


  let check_unix_presence eless edg=
    let full_path=Assistance_dfn_join.to_ending eless edg in 
    Sys.file_exists(Assistance_dfn_full.to_line full_path);;

let  check_unix_presences hm=
    Assistance_dfa_ending.compute_on_all_ocaml_endings (fun edg->check_unix_presence hm edg);;  

let registrations_for_lonely_ending old_edg =
   let edg = Assistance_dfa_ending.restrict_to_ocaml_ending old_edg in 
    (
      edg=Assistance_dfa_ocaml_ending_t.Ml,
      edg=Assistance_dfa_ocaml_ending_t.Mli,
      edg=Assistance_dfa_ocaml_ending_t.Mll,
      edg=Assistance_dfa_ocaml_ending_t.Mly
     );;

     
let complete_id_during_new_module_registration cs rless=
    let middle = Assistance_dfn_rootless.to_middle rless in 
    let eless=Assistance_dfn_join.root_to_middle (root cs) middle 
    and edg=Assistance_dfn_rootless.to_ending rless in
    let modules_written_in_file=find_needed_data cs rless in
    let (mlp,mlir,mllr,mlyr)=registrations_for_lonely_ending edg
    and (mlmt,mlimt,mllmt,mlymt)=md_compute_modification_times eless in
    let pr_end=edg in
    let prmt=md_associated_modification_time (mlmt,mlimt,mllmt,mlymt) pr_end in
    let temp1=Assistance_image.image 
          (fun mn->
           Assistance_set_of_polys.sort(ancestors_at_module cs mn)) 
          modules_written_in_file in
    let temp2=Assistance_set_of_polys.fold_merge ((Assistance_set_of_polys.sort(modules_written_in_file) )::temp1) in
    let tempf=(fun mn->
              if Assistance_set_of_polys.mem mn temp2
              then Some(mn)
              else None) in
    let allanc=Assistance_option.filter_and_unpack tempf (ordered_list_of_modules cs) in
    let libned=PrivateTwo.find_needed_libraries cs rless modules_written_in_file
    and dirned=PrivateTwo.find_needed_directories cs rless modules_written_in_file in
    (eless,pr_end,mlir,prmt,mlimt,libned,modules_written_in_file,allanc,dirned,false);;
    

let above cs eless=
  let nm=Assistance_dfn_endingless.to_module eless in
  ancestors_at_module cs nm;;
 

let below cs eless=
        let mn0=Assistance_dfn_endingless.to_module eless  in
        Assistance_option.filter_and_unpack(fun mn->
            if List.mem mn0 (ancestors_at_module cs mn)
            then Some(mn)
            else None) (ordered_list_of_modules cs);;    

let directly_above cs eless=
    let nm=Assistance_dfn_endingless.to_module eless in
     direct_fathers_at_module cs nm;;     

let directly_below cs eless=
        let mn0=Assistance_dfn_endingless.to_module eless  in
        Assistance_option.filter_and_unpack(fun mn->
            if List.mem mn0 (direct_fathers_at_module cs mn)
            then Some(mn)
            else None) (ordered_list_of_modules cs);;        

let ordered_as_in_coma_state cs l=
   List.filter (fun x->List.mem x l) (ordered_list_of_modules cs);;

let above_one_in_several_or_inside cs l=
  let temp1=Assistance_image.image (ancestors_at_module cs) l in
  let temp2=List.flatten (l::temp1) in
  ordered_as_in_coma_state cs  temp2;;

let acolytes_below_module cs mn =
   let temp1 = List.filter(fun mn2->
        List.mem mn (ancestors_at_module cs mn2)) 
    (ordered_list_of_modules cs) in 
   let temp2 = Assistance_image.image (rootless_paths_at_module cs) temp1 in 
   List.flatten temp2 ;; 

let all_mlx_files cs=
  let mods=ordered_list_of_modules cs in
  List.flatten(Assistance_image.image(acolytes_at_module cs) mods);;                
      
let all_mlx_paths cs=Assistance_image.image Assistance_dfn_full.to_absolute_path 
        (all_mlx_files cs);;  

let all_rootless_paths cs=
    let mods=ordered_list_of_modules cs in
    List.flatten(Assistance_image.image(rootless_lines_at_module cs) mods);;  
     

let short_paths_inside_subdirectory cs subdir =
   let s_root = Assistance_dfa_root.connectable_to_subpath (root cs) in 
   let s_subdir_full_name=s_root^(Assistance_dfa_subdirectory.connectable_to_subpath subdir) in 
   let the_subdir=Assistance_directory_name.of_string s_subdir_full_name in 
   let temp1=Assistance_more_unix.complete_ls_with_nondirectories_only the_subdir in 
   let n=String.length s_root in 
   Assistance_image.image (
    fun ap->let s_ap=Assistance_absolute_path.to_string ap in 
    Assistance_cull_string.cobeginning n s_ap
   ) temp1;;


let files_containing_string cs some_string=
let temp1=all_mlx_paths cs in
List.filter (fun ap->Assistance_substring.is_a_substring_of 
  some_string (Assistance_io.read_whole_file ap)) temp1;;


let system_size cs=List.length(ordered_list_of_modules cs);;

exception Inconsistent_constraints of Assistance_dfa_module_t.t*Assistance_dfa_module_t.t;;
exception Bad_upper_constraint of Assistance_dfa_module_t.t;;  


exception Nonregistered_module_during_reposition of Assistance_dfn_endingless_t.t;;  

 
let reposition_module cs eless (l_before,l_after)=
    let l_mods = ordered_list_of_modules cs in 
    let n=List.length(l_mods) in 
    let find_idx=(fun mn->Assistance_listennou.find_index mn l_mods) 
    and get=(fun j->List.nth l_mods (j-1)) in
    let indices_before=Assistance_image.image find_idx l_before
    and indices_after=Assistance_image.image find_idx l_after in
    let max_before=(if indices_before=[] then 1 else Assistance_max.list indices_before)
    and min_after=(if indices_after=[] then n else Assistance_min.list indices_after)
    in
    let pivot=get max_before in 
    if max_before>min_after
    then raise(Inconsistent_constraints(pivot,get min_after))
    else 
    if max_before>(find_idx eless)
    then raise(Bad_upper_constraint(pivot))
    else 
    Assistance_coma_state_automatic.reposition_in_each cs pivot eless;;  

let find_value_definition cs s= 
  if not(String.contains s '.')
  then None
  else
  let j1=String.index(s)('.')+1 in
  let module_name=Assistance_cull_string.beginning (j1-1) s in
  let nm=Assistance_dfa_module.of_line(String.uncapitalize_ascii(module_name)) in
  let hm1=endingless_at_module cs nm in
  let ap1=Assistance_dfn_full.to_absolute_path(Assistance_dfn_join.to_ending hm1 
     Assistance_dfa_ending.ml) in
  let temp1=Assistance_read_ocaml_files.read_ocaml_files [ap1] in	 
  Assistance_option.seek (
     fun itm->Assistance_ocaml_gsyntax_item.name(itm)=s
  ) temp1;;
     

let all_ml_absolute_paths cs=  
Assistance_option.filter_and_unpack (fun mn->
  if not(check_ending_in_at_module Assistance_dfa_ending.ml cs mn)
  then None
  else 
  let hm=endingless_at_module cs mn in
  let mlx=Assistance_dfn_join.to_ending hm Assistance_dfa_ending.ml in
  Some(Assistance_dfn_full.to_absolute_path mlx)
) (ordered_list_of_modules cs);;

let modules_using_value cs value_name =
  Assistance_option.filter_and_unpack (fun mn->
  let eless=endingless_at_module cs mn
  and pr_end=principal_ending_at_module cs mn in
  let mlx=Assistance_dfn_join.to_ending eless pr_end in
   let ap=Assistance_dfn_full.to_absolute_path mlx in
   if Assistance_substring.is_a_substring_of 
       value_name (Assistance_io.read_whole_file ap)
   then Some eless
   else None ) (ordered_list_of_modules cs);;





let update_ancs_libs_and_dirs_at_module cs mn=
  let eless=endingless_at_module cs mn  
  and pr_end=principal_ending_at_module cs mn in
  let rless=Assistance_dfn_full.to_rootless (Assistance_dfn_join.to_ending eless pr_end) in 
  let fathers=direct_fathers_at_module cs mn in
  let separated_ancestors=Assistance_image.image 
  (fun nm2->
    Assistance_set_of_polys.safe_set(ancestors_at_module cs nm2)
  ) fathers in
  let ancestors_with_wrong_order=Assistance_set_of_polys.fold_merge((Assistance_set_of_polys.safe_set fathers)::separated_ancestors) in
  let ordered_ancestors=List.filter (
    fun mn->Assistance_set_of_polys.mem mn ancestors_with_wrong_order
  ) (ordered_list_of_modules cs) in
  let new_libs=PrivateTwo.find_needed_libraries cs rless ordered_ancestors
  and new_dirs=PrivateTwo.find_needed_directories cs rless ordered_ancestors in
  let cs2=set_ancestors_at_module cs mn ordered_ancestors in 
  let cs3=set_needed_libs_at_module cs2 mn new_libs in
  set_needed_dirs_at_module cs3 mn new_dirs;;


let update_ancs_libs_and_dirs cs=
  let cs_walker=ref(cs) in 
  let _=List.iter(fun mn->cs_walker:=update_ancs_libs_and_dirs_at_module (!cs_walker) mn)(ordered_list_of_modules cs) in
  (!cs_walker);;  


module PrivateThree=struct

    let message_about_circular_dependencies printer cycles= 
      if cycles=[]
      then ""
      else
      let temp1=Assistance_image.image(fun cycle->
        let ttemp1=Assistance_image.image printer cycle in
         String.concat " -> " ttemp1 
      ) cycles in
      let temp2=String.concat "\n\n" temp1 in
      temp2;;
    
    exception Circular_dependencies of string;;
    
    let treat_circular_dependencies tolerate_cycles printer cycles=
      if cycles=[]
      then ()
      else let msg=message_about_circular_dependencies printer cycles in  
           if tolerate_cycles
           then (print_string msg;flush stdout)     
           else raise(Circular_dependencies(msg));; 
           
    let message_about_changed_modules changed_modules=
      let temp1=Assistance_image.image Assistance_dfa_module.to_line changed_modules in
      "\n\n\n"^
      "The following modules have been directly changed :\n"^
      (String.concat "\n" temp1)^
      "\n\n\n"
    ;;       

    let message_about_changed_noncompilables changed_noncompilables=
      let temp1=Assistance_image.image Assistance_dfn_rootless.to_line changed_noncompilables in
      "\n\n\n"^
      "The following noncompilables have been directly changed :\n"^
      (String.concat "\n" temp1)^
      "\n\n\n"
    ;;    

    let announce_changed_modules changed_modules=
      if changed_modules=[]
      then ()
      else (print_string(message_about_changed_modules changed_modules);flush stdout);;
             
    let announce_changed_noncompilables changed_noncompilables=
      if changed_noncompilables=[]
      then ()
      else (print_string(message_about_changed_noncompilables changed_noncompilables);flush stdout);;

    let put_md_list_back_in_order tolerate_cycles 
      cs initially_active_nms=
      let md_list=ordered_list_of_modules cs in
      let coat=Assistance_memoized.make (fun nm->direct_fathers_at_module cs nm) in
      let (cycles,reordered_list)=Assistance_reconstruct_linear_poset.reconstruct_linear_poset 
         coat md_list in
      let _=treat_circular_dependencies tolerate_cycles (
        (fun nm->
           let middle = Assistance_dfn_endingless.to_middle ( endingless_at_module cs nm) in 
           Assistance_dfn_middle.to_line middle )
      ) cycles in     
      let cs2=Assistance_coma_state_automatic.reorder cs (Assistance_image.image fst reordered_list) in    
      let cs3=update_ancs_libs_and_dirs cs2 in 
      let active_descendants=Assistance_option.filter_and_unpack (
          fun nm->
            if List.mem nm initially_active_nms
            then Some(nm)
            else
            if List.exists (fun nm2->List.mem nm2 initially_active_nms) 
                 (ancestors_at_module cs nm)
            then Some(nm)
            else None
      ) (ordered_list_of_modules cs) in  
      (cs3,active_descendants);;
     
end;; 
     
let md_recompute_modification_time eless edg=
  let mlx=Assistance_dfn_join.to_ending eless edg in
  let file=Assistance_dfn_full.to_line mlx in
  if not(Sys.file_exists file) then "0." else
  let st=Unix.stat file in
  string_of_float(st.Unix.st_mtime);;
  

let check_for_possible_change cs mn=
  let eless =endingless_at_module cs mn 
  and pr_ending=principal_ending_at_module cs mn in
  let mli_modif_time=md_recompute_modification_time eless Assistance_dfa_ending.mli 
  and pr_modif_time=md_recompute_modification_time eless pr_ending 
  and old_mli_modif_time=mli_mt_at_module cs mn
  and old_pr_modif_time=principal_mt_at_module cs mn 
  in
  let mn = Assistance_dfn_endingless.to_module eless in 
  let no_change_for_mlis =(
     if not(mli_presence_at_module cs mn)
     then true 
    else   mli_modif_time = old_mli_modif_time
  ) in 
  if no_change_for_mlis&&(pr_modif_time=old_pr_modif_time)&&(product_up_to_date_at_module cs mn)
  then None
  else
  let rless=Assistance_dfn_full.to_rootless(Assistance_dfn_join.to_ending eless pr_ending) in
  let direct_fathers=find_needed_data cs rless in
  Some(
    pr_modif_time,
    mli_modif_time,
    direct_fathers
   )   
  ;;
    
let latest_changes_in_compilables cs = 
  let ref_for_changed_modules=ref[] 
  and ref_for_changed_shortpaths=ref[] in
  let declare_changed=(fun nm->
    ref_for_changed_modules:=nm::(!ref_for_changed_modules);
    ref_for_changed_shortpaths:=((!ref_for_changed_shortpaths)@
                        (rootless_lines_at_module cs nm))
    ) in
  let cs_walker=ref(cs) in   
  let _=List.iter (fun mname->
    match check_for_possible_change (!cs_walker) mname with
    None->()
    |_->
    (
    declare_changed(mname);
    )
)(ordered_list_of_modules cs) in
let changed_modules=List.rev(!ref_for_changed_modules) in
if changed_modules=[] then [] else
let _=PrivateThree.announce_changed_modules changed_modules in
(!ref_for_changed_shortpaths);; 

let latest_changes_in_noncompilables cs =
   let fw = frontier_with_unix_world cs in 
   let (_,(_,changed_noncompilables)) = Assistance_fw_wrapper.inspect_and_update fw in 
   Assistance_image.image Assistance_dfn_rootless.to_line changed_noncompilables;;


let latest_changes cs = 
  (latest_changes_in_compilables cs,latest_changes_in_noncompilables cs);;

let printer_equipped_types_from_data cs=
  Assistance_option.filter_and_unpack (
    fun mn->
    let eless=endingless_at_module cs mn
    and pr_end=principal_ending_at_module cs mn in
    let mlx=Assistance_dfn_join.to_ending eless pr_end in
    let ap=Assistance_dfn_full.to_absolute_path mlx in
    let text=Assistance_io.read_whole_file ap in
    if (Assistance_substring.is_a_substring_of ("let "^"print_out ") text)
    then Some(eless)
    else None
  ) (ordered_list_of_modules cs);;
 



exception Already_registered_file of Assistance_dfn_rootless_t.t;;  
exception Overcrowding of Assistance_dfn_rootless_t.t*(Assistance_dfa_ending_t.t list);;
exception Bad_pair of Assistance_dfn_rootless_t.t*Assistance_dfa_ending_t.t;; 


let register_mlx_file_on_monitored_modules cs rless =
          let middle = Assistance_dfn_rootless.to_middle rless
          and ending=Assistance_dfn_rootless.to_ending rless in 
          let nm=Assistance_dfn_rootless.to_module rless in
          if not(Assistance_coma_state_automatic.test_module_for_registration cs nm)
          then  let info=complete_id_during_new_module_registration cs rless in
                Assistance_coma_state_automatic.push_right_in_each cs info 
          else
          let edgs=registered_endings_at_module cs nm in
          if List.length(edgs)>1
          then  raise(Overcrowding(rless,edgs))
          else  
          if List.mem ending edgs
          then raise(Already_registered_file(rless))
          else
          if (not(List.mem Assistance_dfa_ending.mli (ending::edgs)))
          then raise(Bad_pair(rless,List.hd edgs))
          else 
          if ending = Assistance_dfa_ending.mli
          then let old_pr_end = List.hd edgs in
               let old_rless =
                Assistance_dfn_join.middle_to_ending middle old_pr_end in
              let (eless,_,old_mlir,prmt,mlimt,libned,dirfath,allanc,dirned,is_updated)=
                 complete_info cs old_rless in
               let new_mlimt = md_compute_modification_time eless ending in
               let new_dt=(old_pr_end,true,prmt,new_mlimt,libned,dirfath,allanc,dirned,false) in
               Assistance_coma_state_automatic.set_in_each cs nm new_dt
          else
          let new_dt=complete_id_during_new_module_registration cs rless in 
          let (_,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,is_updated)=new_dt in
          let temp3=List.rev(dirfath) in
          if temp3=[]
          then Assistance_coma_state_automatic.set_in_each cs nm (pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,is_updated) 
          else  
          let last_father=List.hd(temp3) in
          let nm=Assistance_dfn_rootless.to_module rless in 
          let cs_walker=ref(cs) in 
          let _=List.iter(
                 fun current_module ->
              let current_anc= ancestors_at_module (!cs_walker) current_module in  
              if not(List.mem nm current_anc)
              then ()
              else  
                   let current_libs= needed_libs_at_module cs current_module in
                   let new_ancestors=Assistance_option.filter_and_unpack(
                      fun nm2->
                      if (List.mem nm2 allanc)||(List.mem nm2 current_anc)
                      then Some(nm2)
                      else None
                    ) (ordered_list_of_modules (!cs_walker)) 
                    and new_libs=List.filter (
                      fun lib->(List.mem lib libned)||(List.mem lib current_libs)
                    ) Assistance_ocaml_library.all_libraries in  
                    let ordered_dirs=Assistance_set_of_polys.merge
                       (Assistance_set_of_polys.safe_set(needed_dirs_at_module (!cs_walker) current_module))
                       (Assistance_set_of_polys.safe_set (dirned)) in
                    let new_dirs=Assistance_set_of_polys.forget_order(ordered_dirs) in
                    cs_walker:=set_ancestors_at_module (!cs_walker) current_module new_ancestors;
                    cs_walker:=set_needed_libs_at_module (!cs_walker) current_module new_libs;
                    cs_walker:=set_needed_dirs_at_module (!cs_walker) current_module new_dirs;
          )(follows_it cs last_father) in 
          let _=
            ( 
              cs_walker:=Assistance_coma_state_automatic.remove_in_each_at_module (!cs_walker) nm;
              cs_walker:=Assistance_coma_state_automatic.push_after_module_in_each (!cs_walker) last_father new_dt;  
            )
          in
          (!cs_walker);;

module Modern = struct 
(*
exception Unregistered_cmi of Dfn_endingless_t.t;;
exception Unregistered_cmo of Dfn_endingless_t.t;;
*)
let command_for_cmi (cmod:Assistance_compilation_mode_t.t) dir cs hm=
    let nm=Assistance_dfn_endingless.to_module hm in
    let s_root=Assistance_dfa_root.connectable_to_subpath(dir) in
    let s_fhm=Assistance_dfn_endingless.to_line hm in
    let mli_reg=check_ending_in_at_module Assistance_dfa_ending.mli cs nm in
    let ending=(if mli_reg then ".mli" else ".ml") in
    let workdir = Assistance_dfa_subdirectory.connectable_to_subpath (Assistance_compilation_mode.workspace cmod ) in 
    let opt_exec_move=(if (cmod=Assistance_compilation_mode_t.Executable)&&(not(mli_reg)) 
                       then Some("mv "^s_fhm^".o "^s_root^workdir) 
                       else None) in 
    let central_cmd=
        (Assistance_compilation_mode.executioner cmod)^
        (needed_dirs_and_libs_in_command cmod cs nm)^
            " -c "^s_fhm^ending in
            let full_mli=s_fhm^".mli" in
            let almost_full_answer=(
            if (not mli_reg)
               &&(Sys.file_exists(full_mli))
            then (* 
                   in this situation the mli file exists but is not registered.
                   So the compilation manager must treat it as though it didn't
                   exist. We temporarily rename it so that ocamlc will ignore it.
                  *)
                  let dummy_mli=s_root^"uvueaoqhkt" in
                  [
                   "mv "^full_mli^" "^dummy_mli;
                   central_cmd;
                   "mv "^s_fhm^".cm* "^s_root^workdir;
                   "mv "^dummy_mli^" "^full_mli
                  ] 
            else  [
                     central_cmd;
                     "mv "^s_fhm^".cm* "^s_root^workdir
                   ]
            ) in 
            Assistance_option.add_element_on_the_right almost_full_answer opt_exec_move;;
   
  let command_for_cmo (cmod:Assistance_compilation_mode_t.t) dir cs eless=
    let nm=Assistance_dfn_endingless.to_module eless in
    let s_root=Assistance_dfa_root.connectable_to_subpath(dir) in
    let s_eless=Assistance_dfn_endingless.to_line eless in
    let dir_and_libs=needed_dirs_and_libs_in_command cmod cs nm in
    let mli_reg=check_ending_in_at_module Assistance_dfa_ending.mli cs nm in 
    let full_mli=s_eless^".mli" in
    let workdir = Assistance_dfa_subdirectory.connectable_to_subpath (Assistance_compilation_mode.workspace cmod ) in 
    let opt_exec_move=(if cmod=Assistance_compilation_mode_t.Executable 
                       then Some("mv "^s_eless^".o "^s_root^workdir) 
                       else None) in 
    let central_cmds=
    [ 
      (Assistance_compilation_mode.executioner cmod)^dir_and_libs^" -c "^s_eless^".ml";
      "mv "^s_eless^".cm* "^s_root^workdir
    ] in 
    let almost_full_answer= 
    (if (not mli_reg) &&(Sys.file_exists(full_mli))
    then 
          (* 
                   in this situation the mli file exists but is not registered.
                   So the compilation manager must treat it as though it didn't
                   exist. We temporarily rename it so that ocamlc will ignore it.
          *)
                  let dummy_mli=s_root^"uvueaoqhkt" in
                  [
                   "mv "^full_mli^" "^dummy_mli
                  ]
                  @ 
                   central_cmds
                  @ 
                  [ 
                   "mv "^dummy_mli^" "^full_mli
                  ] 
    else central_cmds)
    in Assistance_option.add_element_on_the_right almost_full_answer opt_exec_move;; 

exception  Unregistered_element of Assistance_dfn_endingless_t.t;;   

let command_for_module_separate_compilation cmod cs eless=
    let dir = root cs in 
    let nm=Assistance_dfn_endingless.to_module eless in
    let mli_reg=check_ending_in_at_module Assistance_dfa_ending.mli cs nm
    and ml_reg=check_ending_in_at_module Assistance_dfa_ending.ml cs nm in
    let temp2=(
    let co=command_for_cmo cmod dir cs eless in 
    if mli_reg
    then let ci=command_for_cmi cmod dir cs eless in 
         if ml_reg
         then [ci;co]
         else [ci]
    else [co]) in 
    List.flatten temp2;;

exception  Command_for_predebuggable_or_preexecutable_exn;;

let command_for_predebuggable  cs short_path=
    let cmod = Assistance_compilation_mode_t.Debug in 
    let full_path=Assistance_absolute_path.of_string(
        (Assistance_dfa_root.connectable_to_subpath(root cs))^short_path) in 
    let nm_direct_deps = Assistance_look_for_module_names.names_in_mlx_file full_path in 
    let nm_deps =modules_with_their_ancestors cs nm_direct_deps in 
    let nm_deps_with_subdirs = Assistance_image.image (
       fun nm->
               let subdir=subdir_at_module cs nm in 
        (subdir,nm)
    ) nm_deps in 
    let s_root=Assistance_dfa_root.connectable_to_subpath(root cs) in
    let workdir=
      (Assistance_dfa_subdirectory.connectable_to_subpath (Assistance_compilation_mode.workspace cmod)) in
    let unpointed_short_path = Assistance_cull_string.before_rightmost short_path '.' in 
    let libs_for_prow = 
      Assistance_set_of_polys.sort(
      Assistance_ocaml_library.compute_needed_libraries_from_uncapitalized_modules_list
        (Assistance_image.image Assistance_dfa_module.to_line nm_direct_deps)) in 
    let pre_libs1=Assistance_image.image 
     (fun (_,nm) -> Assistance_set_of_polys.sort(needed_libs_at_module cs nm)) nm_deps_with_subdirs in
    let pre_libs2=Assistance_set_of_polys.forget_order (Assistance_set_of_polys.fold_merge (libs_for_prow::pre_libs1)) in 
    let extension=".cma" in
    let libs=String.concat(" ")
      (Assistance_image.image(fun z->Assistance_ocaml_library.file_for_library(z)^extension) pre_libs2) in 
    Assistance_option.add_element_on_the_right   
    [ 
      (Assistance_compilation_mode.executioner cmod)^
      " -I "^s_root^workdir^" "^
      libs^" -c "^s_root^unpointed_short_path^".ml";
    ] 
    (Assistance_unix_command.mv (s_root^unpointed_short_path^".cm*") (s_root^workdir) )
    ;;          




exception  Command_for_debuggable_or_executable_exn;;

let command_for_debuggable_or_executable cmod cs rootless_path=
    if cmod=Assistance_compilation_mode_t.Usual then raise(Command_for_debuggable_or_executable_exn) else 
    let full_path=Assistance_absolute_path.of_string(
        (Assistance_dfa_root.connectable_to_subpath (root cs))^rootless_path) in 
    let nm_direct_deps = Assistance_look_for_module_names.names_in_mlx_file full_path in 
    let nm_deps =modules_with_their_ancestors cs nm_direct_deps in 
    let nm_deps_with_subdirs = Assistance_image.image (
       fun nm->let subdir=subdir_at_module cs nm in 
        (subdir,nm)
    ) nm_deps in 
    let s_root=Assistance_dfa_root.connectable_to_subpath(root cs) in
    let workdir=
      (Assistance_dfa_subdirectory.connectable_to_subpath (Assistance_compilation_mode.workspace cmod)) 
    and ending=Assistance_compilation_mode.ending_for_nonlast_module cmod 
    and last_ending=Assistance_compilation_mode.ending_for_last_module cmod 
    and product_ending=Assistance_compilation_mode.ending_for_final_product cmod  in
    let cm_elements_but_the_last = Assistance_image.image (
      fun (subdir,nm)->(Assistance_dfa_module.to_line nm)^ending
    ) nm_deps_with_subdirs in 
    let unpointed_short_path = Assistance_cull_string.before_rightmost rootless_path '.' in 
    let nm_name = (Assistance_cull_string.after_rightmost unpointed_short_path '/') in 
    let last_cm_element=nm_name^last_ending in 
    let all_cm_elements= cm_elements_but_the_last @ [last_cm_element] in 
    let libs_for_prow = 
      Assistance_set_of_polys.sort(
      Assistance_ocaml_library.compute_needed_libraries_from_uncapitalized_modules_list
        (Assistance_image.image Assistance_dfa_module.to_line nm_direct_deps)) in 
    let pre_libs1=Assistance_image.image 
     (fun (_,nm) -> Assistance_set_of_polys.sort(needed_libs_at_module cs nm)) nm_deps_with_subdirs in
    let pre_libs2=Assistance_set_of_polys.forget_order (Assistance_set_of_polys.fold_merge (libs_for_prow::pre_libs1)) in 
    let extension=(if cmod=Assistance_compilation_mode_t.Executable then ".cmxa" else ".cma") in
    let libs=String.concat(" ")
      (Assistance_image.image(fun z->Assistance_ocaml_library.file_for_library(z)^extension) pre_libs2) in 
    Assistance_option.add_element_on_the_right  
    [ 
      ((Assistance_compilation_mode.executioner cmod)^
       " -I "^s_root^workdir^" "^
       libs^" -o "^nm_name^product_ending^
        (String.concat " " all_cm_elements));
    ]
    (
      Assistance_unix_command.mv ((Sys.getcwd())^"/"^nm_name^product_ending) (s_root^workdir)
    )
    ;;          




end;;

let ocamldebug_printersfile_path root= 
           (Assistance_dfa_root.connectable_to_subpath root)^
           (Assistance_dfa_subdirectory.connectable_to_subpath(Assistance_coma_constant.utility_files_subdir)) ^
             "cmos_for_ocamldebug.txt";;


module Ocaml_target_making=struct




exception Failed_during_compilation of (Assistance_dfa_module_t.t*Assistance_dfn_endingless_t.t*string);;

let rec helper_for_feydeau  (cmod:Assistance_compilation_mode_t.t) cs (rejected,treated,to_be_treated)=
     match to_be_treated with 
     []->(cs,rejected,List.rev treated)
     |triple::other_triples->
       let (nm,eless,cmd)=triple in
       if (Assistance_unix_command.uc cmd)=0
       then 
            let cs2=set_product_up_to_date_at_module cs nm true in 
            helper_for_feydeau cmod cs2 (rejected,(nm,eless)::treated,other_triples)
       else if (cmod<>Assistance_compilation_mode_t.Usual)
            then raise(Failed_during_compilation(triple))
            else 
            let triples_after=snd(Assistance_prepared.partition_in_two_parts (fun (nm2,_,_)->nm2<>nm) other_triples) in 
            let (rejected_siblings_as_triples,survivors)=List.partition
           (
              fun (nm2,_,_)->
                List.mem nm (ancestors_at_module cs nm2)
           ) triples_after in 
           let rejected_siblings_with_redundancies =  
              Assistance_image.image (fun (nm2,eless2,_)->(nm2,eless2) ) rejected_siblings_as_triples in 
           let rejected_siblings = Assistance_listennou.nonredundant_version rejected_siblings_with_redundancies in    
           let newly_rejected = (nm,eless)::rejected_siblings in 
           let cs_walker=ref(cs) in 
           let _=List.iter(
              fun (nm3,hm3)->
                cs_walker:=set_product_up_to_date_at_module (!cs_walker) nm3 false
           ) newly_rejected in 
           helper_for_feydeau cmod (!cs_walker) (rejected@newly_rejected,treated,survivors) ;;
         

let prepare_pretty_printers_for_ocamldebug cs deps = 
  let temp1 = "load_printer str.cma"::(Assistance_image.image (fun mname->
    let s= Assistance_dfa_module.to_line mname in 
    "load_printer "^s^".cmo"
  ) deps) 
  and preq_types = cs.Assistance_coma_state_t.printer_equipped_types  in 
  let printable_deps = List.filter (
    fun mn -> let eless = endingless_at_module cs mn in 
    List.mem (eless,true) preq_types
  ) deps in 
  let temp2 = Assistance_image.image (fun mname->
    let s= Assistance_dfa_module.to_line mname in 
    "install_printer "^(String.capitalize_ascii s)^".print_out"
  ) printable_deps in 
  let full_text = String.concat "\n" (temp1@temp2) in 
  let ppodbg_path = ocamldebug_printersfile_path (root cs) in 
  Assistance_io.overwrite_with (Assistance_absolute_path.of_string ppodbg_path) full_text;;

let dependencies_inside_shaft cmod cs (opt_modnames,opt_rootless_path)=
   match cmod with 
   Assistance_compilation_mode_t.Usual->Assistance_option.unpack opt_modnames
   |_->let rootless_path=Assistance_option.unpack opt_rootless_path in 
       let full_path=Assistance_absolute_path.of_string(
        (Assistance_dfa_root.connectable_to_subpath (root cs))^rootless_path) in 
       let nm_direct_deps = Assistance_look_for_module_names.names_in_mlx_file full_path in 
       let nm_deps=modules_with_their_ancestors cs nm_direct_deps in 
       let deps =List.filter (fun mn->List.mem mn nm_deps) (ordered_list_of_modules cs) in 
       let _=(if cmod = Assistance_compilation_mode_t.Debug 
              then prepare_pretty_printers_for_ocamldebug cs deps) in 
       deps;;



let list_of_commands_for_shaft_part_of_feydeau cmod cs (opt_modulenames,opt_rootless_path)=
   let l=dependencies_inside_shaft cmod cs (opt_modulenames,opt_rootless_path) in 
   let temp1=Assistance_image.image (fun mn->
     let eless=endingless_at_module cs mn in 
     let cmds=Modern.command_for_module_separate_compilation cmod cs eless in 
    Assistance_image.image (fun cmd->(mn,endingless_at_module cs mn,cmd) ) cmds ) l in 
    List.flatten temp1;;



let list_of_commands_for_connecting_part_of_feydeau cmod cs (_,opt_rootless_path)=
   let cmds=(
   match cmod with 
    Assistance_compilation_mode_t.Usual
   |Assistance_compilation_mode_t.Executable ->[] 
   |_->
      let rootless_path=Assistance_option.unpack opt_rootless_path in 
      Modern.command_for_predebuggable cs rootless_path) in 
   cmds;;


let list_of_commands_for_end_part_of_feydeau cmod cs (_,opt_rootless_path)= 
   let cmds=(
   match cmod with 
   Assistance_compilation_mode_t.Usual->[] 
   |_->
      let rootless_path=Assistance_option.unpack opt_rootless_path in 
      Modern.command_for_debuggable_or_executable cmod cs rootless_path) in 
   cmds;;   

let list_of_commands_for_ternary_feydeau cmod cs short_path=
   let pair = (None,Some(short_path)) in 
   let pre_cmds1=list_of_commands_for_shaft_part_of_feydeau cmod cs pair in 
   let cmds1=Assistance_image.image (fun (_,_,cmd)->cmd) pre_cmds1
   and cmds2=list_of_commands_for_connecting_part_of_feydeau cmod cs pair
   and cmds3=list_of_commands_for_end_part_of_feydeau cmod cs pair in 
   cmds1@cmds2@cmds3;;



let shaft_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path)=
  let cmds=list_of_commands_for_shaft_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path) in  
  helper_for_feydeau cmod cs ([],[],cmds);; 


  
let end_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path)=
  match cmod with 
   Assistance_compilation_mode_t.Usual->()
   |_->
     let all_cmds=
       (list_of_commands_for_connecting_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path))@
       (list_of_commands_for_end_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path)) in 
     let _=Assistance_image.image  Assistance_unix_command.hardcore_uc all_cmds in 
     ()



let feydeau cmod cs (opt_modnames,opt_rootless_path)=
  let answer=shaft_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path) in 
  let _=end_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path) in 
  answer;; 


let usual_feydeau cs modnames = feydeau Assistance_compilation_mode_t.Usual cs (Some(modnames),None);;

end;;  


let add_printer_equipped_type cs mn=
  set_preq_types cs ((preq_types cs)@[mn]);;

let remove_printer_equipped_type cs mn=
  set_preq_types cs (List.filter (fun mn2->mn2<>mn) (preq_types cs));;

let uple_form cs=
  (cs,
   directories cs,
   preq_types cs
   );;


let unregister_mlx_file cs mlx=
    let mn=Assistance_dfn_full.to_module mlx in 
    let following = mn::(follows_it cs mn) in  
    let was_lonely=
      (List.length(registered_endings_at_module cs mn)=1) in 
    let _=set_product_up_to_date_at_module cs mn false in 
    let cs2=partially_remove_mlx_file cs mlx in
    let new_dirs=compute_subdirectories_list cs2 in
    let cs3=(if was_lonely 
           then cs2
           else ( fun (cs4,_,_)->cs4)
           (Ocaml_target_making.usual_feydeau 
             cs2 following) ) in 
    set_directories cs3 new_dirs;;   

let unregister_mlx_files cs mlxs = 
  List.fold_left unregister_mlx_file cs mlxs ;; 


exception FileWithDependencies of 
Assistance_dfn_full_t.t*(Assistance_dfa_module_t.t list);;

let read_persistent_version x=
        let full_path=Assistance_dfn_join.root_to_rootless (root x)  Assistance_coma_constant.rootless_path_for_targetfile in
        let ap= Assistance_dfn_full.to_absolute_path full_path in
        let the_archive=Assistance_io.read_whole_file ap in
        let archived_object = Assistance_crobj_parsing.parse the_archive in 
        Assistance_coma_state_automatic.of_concrete_object archived_object;;      

module Try_to_register=struct

  let mlx_file cs mlx_file=
    try(Some(register_mlx_file_on_monitored_modules 
        cs mlx_file)) with _->None;;  

module Private=struct

exception Pusher_exn;;

let pusher  (cs,failures,yet_untreated)=
     match yet_untreated with
      []->raise(Pusher_exn)
      |mlx::others->
      (
        match mlx_file cs mlx with
        None->(cs,mlx::failures,others)
        |Some(nfs)->(nfs,failures,others)
      );; 

let rec iterator x=
   let (cs,failures,yet_untreated)=x in
   match yet_untreated with
      []->(failures,cs)
      |mlx::others->iterator(pusher x);;   

end;;

let mlx_files cs mlx_files=
   Private.iterator(cs,[],mlx_files);;
 

end;;  



module Register_mlx_file=struct

let on_targets (cs,old_dirs) rless=
    let new_dir=Assistance_dfn_rootless.to_subdirectory rless in
   let cs2=register_mlx_file_on_monitored_modules cs rless in
   let new_dirs=
   (if List.mem new_dir old_dirs then old_dirs else old_dirs@[new_dir] )
    in
    let nm=Assistance_dfn_rootless.to_module rless in 
    let (cs3,_,_)=Ocaml_target_making.usual_feydeau cs2 [nm] in 
    (cs3,new_dirs);; 
  

end;;  


let register_mlx_file cs mlx=
          let (cs2,new_dirs)= 
          Register_mlx_file.on_targets (cs,directories cs) mlx in   
           set_directories cs2 new_dirs;;            

let register_mlx_files cs mlxs = List.fold_left register_mlx_file cs mlxs;;

let clean_debug_dir cs=
  let s_root=Assistance_dfa_root.connectable_to_subpath(root cs) in
  let s_debug_dir=s_root^(Assistance_dfa_subdirectory.connectable_to_subpath(Assistance_coma_constant.debug_build_subdir)) in 
  Assistance_unix_command.uc("rm -f "^s_debug_dir^"*.cm*"^" "^s_debug_dir^"*.ocaml_debuggable");;
   
let name_element_for_debugged_file = "debugged" ;;
let debugged_file_path = (Assistance_dfa_subdirectory.connectable_to_subpath(Assistance_coma_constant.utility_files_subdir))
             ^ name_element_for_debugged_file ^ ".ml" ;;  

let start_debugging cs=
  let  _=clean_debug_dir cs in
  let ppodbg_path = ocamldebug_printersfile_path (root cs) in 
  let _= Assistance_io.overwrite_with (Assistance_absolute_path.of_string ppodbg_path) "" in   
  let cmds=Ocaml_target_making.list_of_commands_for_ternary_feydeau Assistance_compilation_mode_t.Debug cs debugged_file_path in 
  let answer=Assistance_unix_command.conditional_multiple_uc cmds in 
	let msg=(
	  if answer
	  then "\n\n Now, start \n\nocamldebug _debug_build/"^name_element_for_debugged_file^
         ".ocaml_debuggable\n\nin another terminal.\n\n"^
         "If you need to use pretty printers, from inside ocamldebug do \n\n"^ 
         "source "^ppodbg_path^" \n\n"
	  else "\n\n Something went wrong, see above. \n\n"
	) in
	let _=(
	  print_string msg;
	  flush stdout
	) in
	answer;;   
   
let clean_exec_dir cs=
  let s_root=Assistance_dfa_root.connectable_to_subpath(root cs) in
  let s_exec_dir=s_root^(Assistance_dfa_subdirectory.connectable_to_subpath(Assistance_coma_constant.exec_build_subdir)) in 
  Assistance_unix_command.uc("rm -f "^s_exec_dir^"*.cm*"^" "^s_exec_dir^"*.ocaml_executable"^" "^s_exec_dir^"*.o");;
   

let start_executing cs short_path=
  let  _=clean_exec_dir cs in
  let cmds=Ocaml_target_making.list_of_commands_for_ternary_feydeau 
    Assistance_compilation_mode_t.Executable cs short_path in 
  Assistance_unix_command.conditional_multiple_uc cmds;;   

let decipher_path cs x=Assistance_find_suitable_ending.find_file_location 
   (root cs) (directories cs) x;;

let forgotten_files_in_build_subdir cs= 
   let s_root=Assistance_dfa_root.connectable_to_subpath (root cs) 
   and s_build=Assistance_dfa_subdirectory.connectable_to_subpath (Assistance_coma_constant.usual_build_subdir) in 
   let dir=Assistance_directory_name.of_string (s_root^s_build) in 
   let temp1=Assistance_more_unix.beheaded_simple_ls dir in 
   List.filter (
      fun s->
       let s_mn=Assistance_cull_string.before_rightmost_possibly_all s '.' in 
       let mn=Assistance_dfa_module.of_line s_mn in 
       not(Assistance_coma_state_automatic.test_module_for_registration cs mn)
       ) temp1;;

exception Absent_module of string;;

let decipher_module cs capitalized_or_not_x=
  let x=String.uncapitalize_ascii capitalized_or_not_x in 
  let s=Assistance_cull_string.before_rightmost_possibly_all x '.' in
  match (Assistance_option.find_and_stop(
      fun edg->
      let t=s^(Assistance_dfa_ending.connectable_to_modulename edg) in 
      try(Some(decipher_path cs t)) with _->None
  ) Assistance_dfa_ending.all_ocaml_endings) with
  None->raise(Absent_module(x))
  |Some(ap)->
    let rootless_path = Assistance_dfn_common.decompose_absolute_path_using_root ap (root cs) in 
    let mlx = Assistance_dfn_join.root_to_rootless (root cs) rootless_path in 
    Assistance_dfn_full.to_endingless mlx ;;

module Local_rename_value_inside_module = struct

exception No_module_given of string;;

exception No_value_with_name of string;;

let get_module_inside_name s=
   let f=Assistance_cull_string.before_rightmost s '/' in
   if f=""
   then raise(No_module_given(s))
   else f;;
   
let rename_value_inside_module cs old_name new_name=
   let j=Assistance_substring.leftmost_index_of_in "." old_name in
   if j<0 
   then raise(No_module_given(old_name))
   else 
   let module_name=Assistance_cull_string.beginning (j-1) old_name in
   let endingless=decipher_module cs  module_name 
   and path=decipher_path cs  module_name in 
   let nm=Assistance_dfn_endingless.to_module endingless in
   let pre_temp2=(ancestors_at_module cs nm)@[nm] in
   let temp2=Assistance_image.image (endingless_at_module cs) pre_temp2 in
   let preceding_files=Assistance_image.image  (fun eless2->
   	 Assistance_dfn_full.to_absolute_path(Assistance_dfn_join.to_ending eless2 Assistance_dfa_ending.ml)
   ) temp2 in
   Assistance_rename_moduled_value_in_file.rename_moduled_value_in_file 
      preceding_files old_name new_name path;;



end;;


module Values_in_modules = struct

let replace_string cs old_string new_string=
  let temp1=files_containing_string cs old_string in
  let m=String.length(Assistance_dfa_root.connectable_to_subpath (root cs)) in
  let temp2=Assistance_image.image (fun ap->
    Assistance_cull_string.cobeginning m (Assistance_absolute_path.to_string ap)) temp1 in
  let temp3=temp2@["";""] in 
  let message="\n\n The following files will be rewritten : \n\n"^
  (String.concat "\n" temp3) in
  let _=(print_string message;flush stdout) in
  List.iter (Assistance_replace_inside.replace_inside_file (old_string,new_string)) temp1;;

(*
if the string argument has a dot inside it, we interpret it
as a value inside a module.
Otherwise we interpret it as a mere string.
*)


let rename_string_or_value cs old_name new_name=
  if not(String.contains old_name '.')
  then replace_string cs old_name new_name
  else 
    let new_full_name=(Assistance_cull_string.before_rightmost old_name '.')^"."^new_name in
    (Local_rename_value_inside_module.rename_value_inside_module 
            cs old_name (Assistance_overwriter.of_string new_name); 
     replace_string cs old_name new_full_name
    );;


let list_values_from_module_in_file module_name file=
   let s=Assistance_io.read_whole_file file in
   let temp1=Assistance_look_for_module_names.indices_in_mlx_file file in
   let temp2=List.filter (fun (t,(i,j))->
     (t=Assistance_alternative_str_example.index_for_pointed_case)&&
     (Assistance_cull_string.interval s i j=(String.capitalize_ascii module_name))
   ) temp1 in
   let temp3=Assistance_image.image(fun (t,(i,j))->
    let opt=Assistance_after.after_star 
     Assistance_charset.ocaml_modulename_nonfirst_letters
     s (j+2) in
    let end_idx=(match opt with Some(k)->k-1 |None->String.length s) in
     Assistance_cull_string.interval s (j+2) end_idx
   ) temp2 in
   Assistance_set_of_strings.sort temp3;;

let list_values_from_module_in_modulesystem cs module_name=
   let temp1=all_mlx_paths cs in
   let temp2=Assistance_image.image (fun ap->
    let ttemp1=list_values_from_module_in_file module_name ap in
    Assistance_set_of_strings.image (fun x->(x,ap) ) ttemp1
    ) temp1 in
   let temp3=List.flatten temp2 in
   let temp4=Assistance_image.image fst temp3 in 
   let temp5=Assistance_ordered.sort Assistance_total_ordering.lex_for_strings temp4 in
   Assistance_image.image (
      fun x->(x,Assistance_option.filter_and_unpack(
        fun (y,ap)->if y=x then Some(ap) else None
      ) temp3)
   ) temp5 ;;
 
let list_value_occurrences_in_file t file=
   let s=Assistance_io.read_whole_file file in
   let temp1=Assistance_substring.occurrences_of_in t s in
   Assistance_image.image (fun j->Assistance_cull_string.closeup_around_index 
      s j
   ) temp1;; 
 

let show_value_occurrences_in_modulesystem cs t=
   let m=String.length(Assistance_dfa_root.connectable_to_subpath (root cs)) in
   let temp1=all_mlx_paths cs in
   let temp2=Assistance_image.image (fun ap->
    let ttemp1=list_value_occurrences_in_file t ap in
    let mname=Assistance_cull_string.cobeginning(m)(Assistance_absolute_path.to_string ap) in
    Assistance_image.image (fun x->mname^":\n"^x ) ttemp1
    ) temp1 in
   let temp3=List.flatten temp2 in
   let temp4=String.concat "\n\n\n" (""::temp3@[""]) in 
   print_string temp4;;

end;;



exception Module_already_exists of string;;

let duplicate_module cs old_t1 old_t2=
   let t1=String.uncapitalize_ascii old_t1
   and t2=String.uncapitalize_ascii old_t2 in 
   let ap1=decipher_path cs t1 in
   let s_ap1=Assistance_absolute_path.to_string ap1 in
   let s_ending = Assistance_cull_string.after_rightmost s_ap1 '.' in 
   let s_ap2=(Assistance_cull_string.before_rightmost_possibly_all s_ap1 '/')^"/"^t2^"."^s_ending in
   if Sys.file_exists s_ap2
   then raise(Module_already_exists(t2))
   else 
   let _=Assistance_unix_command.uc ("cp "^s_ap1^" "^s_ap2) in
   let ap2=Assistance_absolute_path.of_string s_ap2 in
   let _ =  (
     if s_ending = "ml"
     then Assistance_put_use_directive_in_initial_comment.put_usual (root cs) ap2) in 
   Assistance_unix_command.uc ("open -a \"/Applications/Visual Studio Code.app\" "^s_ap2);;             


module Almost_concrete = struct 


let local_above cs capitalized_or_not_module_name=
  let mn = Assistance_dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  let endingless = endingless_at_module cs mn in  
  Assistance_image.image (fun nm-> 
    let mname = Assistance_dfn_endingless.to_module (endingless_at_module cs nm) in 
    Assistance_dfa_module.to_line mname )
  (above cs endingless);;


let local_below cs capitalized_or_not_module_name=
  let mn = Assistance_dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  let endingless = endingless_at_module cs mn in  
  Assistance_image.image (fun nm-> 
    let mname = Assistance_dfn_endingless.to_module (endingless_at_module cs nm) in 
    Assistance_dfa_module.to_line mname )
  (below cs endingless);;

let local_directly_above cs capitalized_or_not_module_name=
  let mn = Assistance_dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  let endingless = endingless_at_module cs mn in   
  Assistance_image.image (fun nm-> 
    let mname = Assistance_dfn_endingless.to_module (endingless_at_module cs nm) in 
    Assistance_dfa_module.to_line mname )
  (directly_above cs endingless);;

let local_directly_below cs capitalized_or_not_module_name=
  let mn = Assistance_dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  let endingless = endingless_at_module cs mn in   
  Assistance_image.image (fun nm-> 
    let mname = Assistance_dfn_endingless.to_module (endingless_at_module cs nm) in 
    Assistance_dfa_module.to_line mname )
  (directly_below cs endingless);; 


end;; 




module Recent_changes = struct
           
    exception Recompilation_needed of Assistance_dfa_module_t.t list;;       

            let check_for_change_at_module_and_ending cs mn edg=
               let hm=endingless_at_module cs mn in 
               (md_recompute_modification_time hm edg)
               <>(get_modification_time cs mn edg);;

            let check_for_change_at_module  cs mn=
               let pr_ending = principal_ending_at_module cs mn in 
               let endings = (
                   if mli_presence_at_module cs mn 
                   then  [Assistance_dfa_ending.mli;pr_ending]
                   else [pr_ending]
               ) in 
            List.exists (check_for_change_at_module_and_ending cs mn) endings ;;
          

            let detect_changes cs =
            Assistance_option.filter_and_unpack (
               fun mn->
               if check_for_change_at_module cs mn 
               then Some(mn)
               else None
            ) (ordered_list_of_modules cs);;

            let check_for_changes cs = 
            let changes = detect_changes cs in 
            if changes<>[]
            then raise(Recompilation_needed(changes))
            else ();;

end;;    

module Late_Recompilation = struct 

let quick_update cs (new_fw,changed_rootlesses)  mn=
  let eless =endingless_at_module cs mn 
  and pr_ending=principal_ending_at_module cs mn in
  let middle = Assistance_dfn_endingless.to_middle eless in 
  let mli_modif_time=Assistance_fw_wrapper_automatic.get_mtime_or_zero_if_file_is_nonregistered new_fw (Assistance_dfn_join.middle_to_ending middle Assistance_dfa_ending.mli) 
  and pr_modif_time=Assistance_fw_wrapper_automatic.get_mtime new_fw (Assistance_dfn_join.middle_to_ending middle pr_ending)  
  and old_mli_modif_time=mli_mt_at_module cs mn
  and old_pr_modif_time=principal_mt_at_module cs mn 
  in
  let new_values=(mli_modif_time,pr_modif_time)
  and old_values=(old_mli_modif_time,old_pr_modif_time) in
  let mn = Assistance_dfn_endingless.to_module eless in 
  if (old_values=new_values)&&(product_up_to_date_at_module cs mn)&&
     (List.for_all (fun rl->(Assistance_dfn_rootless.to_middle rl)<>middle ) changed_rootlesses)  
  then None
  else
  let mlx=Assistance_dfn_join.middle_to_ending middle pr_ending in
  let direct_fathers=find_needed_data cs mlx in
  Some(
    pr_modif_time,
    mli_modif_time,
    direct_fathers
   )   
  ;;


end ;;

let test_for_foreign root ap =
   match (
     try Some(Assistance_dfn_common.decompose_absolute_path_using_root ap root) with 
              _->None 
   ) with 
   None -> true 
   |Some(rootless) ->
      (
       not(List.mem
          (Assistance_dfn_rootless.to_ending rootless) Assistance_dfa_ending.endings_for_readable_files)   
      )
      ;;

let census_of_foreigners cs=
   let config = (cs.Assistance_coma_state_t.frontier_with_unix_world).Assistance_fw_wrapper_t.configuration in 
   let  the_root = config.Assistance_fw_configuration_t.root in 
   let the_dir =  Assistance_directory_name.of_string (Assistance_dfa_root.without_trailing_slash the_root) in 
   let (list1,_) = Assistance_more_unix.complete_ls_with_ignored_subdirs the_dir config.Assistance_fw_configuration_t.ignored_subdirectories in 
   List.filter (test_for_foreign the_root) list1;;

let reflect_latest_changes_in_github cs opt_msg=
  let old_fw = cs.Assistance_coma_state_t.frontier_with_unix_world in 
  let new_fw = Assistance_fw_wrapper.reflect_latest_changes_in_github old_fw opt_msg in 
  {cs with Assistance_coma_state_t.frontier_with_unix_world = new_fw} ;;

  let check_module_sequence_for_forgettability cs l=
  let temp1 = List.rev (Assistance_three_parts.generic l) in 
   Assistance_option.filter_and_unpack (
    fun (to_be_deleted_before_mn,mn,_)->
      let eless = endingless_at_module cs mn in   
      let temp2 = List.filter (fun mn2->
         not(List.mem mn2 to_be_deleted_before_mn) 
      ) (below cs eless) in 
      if temp2 = []
      then None 
      else Some(mn,temp2)
  ) temp1 ;;


let check_rootless_path_sequence_for_forgettability cs old_l =
 let l = List.filter Assistance_dfn_rootless.is_compilable old_l in 
 let temp1 = List.rev (Assistance_three_parts.generic l) in 
 Assistance_option.filter_and_unpack (
    fun (to_be_deleted_before_rp,rp,_)->
      let mn = Assistance_dfn_rootless.to_module rp in 
      let acolytes = rootless_paths_at_module cs mn in  
      let remaining_acolytes = List.filter (
        fun rp2 -> not (List.mem rp2 (rp::to_be_deleted_before_rp))
      ) acolytes in 
      if remaining_acolytes<>[]
      then None
      else 
      let temp2 = List.filter (fun rp2->
         not(List.mem rp2 to_be_deleted_before_rp) 
      ) (acolytes_below_module cs mn) in 
      if temp2 = []
      then None 
      else Some(mn,temp2)
  ) temp1 ;;

exception Empty_acolytes_list ;; 
exception Too_many_acolytes of Assistance_dfn_rootless_t.t list ;;
exception Unknown_first_acolyte_ending  of Assistance_dfn_rootless_t.t ;;
exception Unknown_second_acolyte_ending of Assistance_dfn_rootless_t.t ;;
exception Missing_mli of Assistance_dfn_rootless_t.t * Assistance_dfn_rootless_t.t ;;
exception Incompatible_locations of Assistance_dfn_rootless_t.t * Assistance_dfn_rootless_t.t ;;
exception Circular_dependencies_detected ;;

module Simplified_ts_creation = struct 

let find_the_mli_among_the_two rless1 rless2 = 
    if (Assistance_dfn_rootless.to_ending rless1) = Assistance_dfa_ending.mli 
    then Some(rless1,rless2) 
    else   
    if (Assistance_dfn_rootless.to_ending rless2) = Assistance_dfa_ending.mli 
    then Some(rless2,rless1) 
    else None ;;
    
let check_admissiblity_of_single_acolyte rless =
    if List.mem (Assistance_dfn_rootless.to_ending rless) 
       [Assistance_dfa_ending.mli;Assistance_dfa_ending.ml;Assistance_dfa_ending.mll;Assistance_dfa_ending.mly]
    then (None,rless)
    else raise(Unknown_first_acolyte_ending(rless)) ;; 

let check_admissibility_of_acolytes_list l=
   let n = List.length(l) in 
   if n > 2 then raise(Too_many_acolytes l) else 
   if n = 0 then raise  Empty_acolytes_list else 
   if n = 1 then check_admissiblity_of_single_acolyte(List.hd l) else 
   (* if we get here n=2 *)
   match  find_the_mli_among_the_two (List.nth l 0) (List.nth l 1) with 
   None -> raise(Missing_mli(List.nth l 0,List.nth l 1))
   |Some(rless1,rless2) ->
      let subdir1 =  Assistance_dfn_rootless.to_subdirectory rless1 
      and subdir2 =  Assistance_dfn_rootless.to_subdirectory rless2 in 
      if subdir1 <> subdir2 
      then raise(Incompatible_locations(rless1,rless2))
      else 
      if not(List.mem (Assistance_dfn_rootless.to_ending rless2) 
          [Assistance_dfa_ending.ml;Assistance_dfa_ending.mll;Assistance_dfa_ending.mly])
      then raise(Unknown_second_acolyte_ending(rless2))  
      else (Some rless1,rless2) ;;

let classify_according_to_module root compilable_files =
    let temp1 = Assistance_image.image (fun (rless,_)->
       (Assistance_dfn_rootless.to_module rless,rless)  
    ) compilable_files in 
    let temp2 = Assistance_listennou.partition_according_to_fst temp1 in 
    let ap_from_rootless = (fun rless->
       let full = Assistance_dfn_join.root_to_rootless root rless in 
       Assistance_dfn_full.to_absolute_path full
      ) in 
    Assistance_image.image (fun (mn,l)->
      let (opt_mli,principal)=check_admissibility_of_acolytes_list l in 
      let opt_mli_ap = Assistance_option.propagate ap_from_rootless opt_mli 
      and principal_ap = ap_from_rootless principal in
      (Assistance_dfa_module.to_line mn,(opt_mli,opt_mli_ap,principal,principal_ap))
      ) temp2 ;;

let treat_circular_dependencies cycles= 
      if cycles=[]
      then ()
      else
      let temp1=Assistance_image.image(String.concat " -> ") cycles in
      let temp2="\n\n The following cycles have been detected : "^
        (String.concat "\n\n" temp1) in
      let _ = (print_string temp2;flush stdout) in 
      raise Circular_dependencies_detected ;;

let compute_dependencies  prepared_list_of_modules =
  let lex_order = Assistance_total_ordering.lex_for_strings in 
  let modules_in_lex_order = Assistance_ordered.sort lex_order (Assistance_image.image fst  prepared_list_of_modules) in 
  let coatoms = Assistance_memoized.make (fun mname ->
     let (opt,opt_ap,pr_rless,pr_ap) = List.assoc mname  prepared_list_of_modules in 
     let mli_part = (match opt_ap with None->[] |(Some ap)->Assistance_look_for_module_names.names_in_mlx_file ap)
     and pr_part =  Assistance_look_for_module_names.names_in_mlx_file pr_ap in 
     let temp1 = Assistance_image.image Assistance_dfa_module.to_line (mli_part@pr_part) in 
     List.filter (fun mname -> Assistance_ordered.mem lex_order mname modules_in_lex_order) temp1
  )     in 
  let (cycles,good_list) = Assistance_reconstruct_linear_poset.reconstruct_linear_poset coatoms  modules_in_lex_order in 
  let _ = treat_circular_dependencies cycles in
  Assistance_image.image (fun mname->mname) good_list ;; 

end ;;   

let principal_acolyte cs eless = 
  let mn = Assistance_dfn_endingless.to_module eless in 
  let edg = principal_ending_at_module cs mn in 
  Assistance_dfn_join.to_ending eless edg ;;

let all_principals cs =
    Assistance_image.image (principal_acolyte cs) (all_endinglesses cs) ;;  

exception Module_not_found_while_choosing_automatic of Assistance_dfa_module_t.t ;;

let choose_automatic_if_possible cs modulename =
    let mname = Assistance_dfa_module.to_line modulename 
    and list_of_modules = Assistance_coma_state_automatic.ordered_list_of_modules cs in 
    let auto_version = Assistance_dfa_module.of_line(mname^"_automatic") in 
    if not(List.mem modulename list_of_modules)
    then raise(Module_not_found_while_choosing_automatic modulename)
    else 
    if List.mem auto_version list_of_modules
    then auto_version
    else modulename ;;      



end;;






module Assistance_fw_initialize=struct

(*

#use"Filewatching/fw_initialize.ml";;

*)

module Private = struct 

let indicator_for_git_ignoring = "_gitignored_" ;;

let first_init config =
   let the_root = config.Assistance_fw_configuration_t.root in 
   let the_dir =  Assistance_directory_name.of_string (Assistance_dfa_root.without_trailing_slash the_root) in 
   let (list1,_) = Assistance_more_unix.complete_ls_with_ignored_subdirs the_dir config.Assistance_fw_configuration_t.ignored_subdirectories in 
   let list2 = Assistance_option.filter_and_unpack(
     fun ap-> try Some(Assistance_dfn_common.decompose_absolute_path_using_root ap the_root) with 
              _->None 
   ) list1 in
   let list3 = 
   List.filter (Assistance_fw_configuration.test_for_admissibility config) list2 in 
   List.partition (
      fun rootless -> List.mem
          (Assistance_dfn_rootless.to_ending rootless) Assistance_dfa_ending.endings_for_compilable_files
   ) list3;;

end ;;

let compute_and_store_modification_times config (compilables_to_be_watched,noncompilables) =
    let the_root = config.Assistance_fw_configuration_t.root in  
    let compute_info=( fun path->
      let s_root = Assistance_dfa_root.connectable_to_subpath the_root
      and s_path=Assistance_dfn_rootless.to_line path in 
      let file = s_root^s_path in 
      let mtime = string_of_float((Unix.stat file).Unix.st_mtime) in 
      (path,mtime)
   ) in 
     {
       Assistance_fw_wrapper_t.configuration = config;
       Assistance_fw_wrapper_t.compilable_files = Assistance_image.image compute_info compilables_to_be_watched;
       noncompilable_files = Assistance_image.image compute_info noncompilables;
       last_noticed_changes = Assistance_dircopy_diff.empty_one;
     };;

let init config = 
   let (nonspecials_to_be_watched,specials) = Private.first_init config in 
   compute_and_store_modification_times config (nonspecials_to_be_watched,specials);;



end;;






module Assistance_no_slashes=struct

(*

#use"no_slashes.ml";;

*)

type t=NS of string;;

exception Slash_at of string*int;;

let to_string(NS s)=s;;

let of_string s=
  let n=String.length s in
  let rec tempf=(fun i->
  if i>n
  then NS s
  else if (String.get s (i-1))='/'
       then raise(Slash_at(s,i))
       else tempf(i+1)
  ) in
  tempf 1;;
  
           

end;;






module Assistance_save_coma_state=struct

(* 

#use"Compilation_management/save_coma_state.ml";;

*)

module Private=struct

  let building_site =  Assistance_coma_constant.usual_build_subdir ;;

  let loadings (main_root,rootless_path_for_loadingsfile) (dirs,hms)=
      let path_for_loadingsfile = Assistance_dfn_rootless.to_line rootless_path_for_loadingsfile in 
      let s_root=Assistance_dfa_root.connectable_to_subpath main_root in
      let part1="\n(*\n #use\""^s_root^(path_for_loadingsfile)^"\";"^";\n*)\n\n" in
      let temp5=Assistance_image.image (
        fun sd->
        "#directory\""^s_root^(Assistance_dfa_subdirectory.connectable_to_subpath sd)^"\";"^";"
      ) (building_site::dirs) in
      let part2=String.concat "\n" temp5 
      and part3="\n\n#load\"str.cma\";"^";\n#load\"unix.cma\";"^";\n\n\n" in
      let temp2=Assistance_image.image (
        function hm->
          let nm = Assistance_dfn_endingless.to_module hm in  
          let s=Assistance_cull_string.after_rightmost (Assistance_dfa_module.to_line nm) '/' in
          "#load\""^s^".cmo\";"^";"
      ) hms in
      let temp3="\n\n\n"::temp2 in
      let part4=String.concat "\n" temp3 
      and part5="\n\n\n" in
      part1^part2^part3^part4^part5;; 
          
    
    let instructions_for_merlinfile main_root dirs=
      let s_root=Assistance_dfa_root.connectable_to_subpath main_root in
      let temp1=Assistance_image.image 
        (fun sdir->"S "^s_root^(Assistance_dfa_subdirectory.connectable_to_subpath sdir) )
      (Assistance_coma_constant.utility_files_subdir::dirs) in
      let temp2=("B "^s_root^(Assistance_dfa_subdirectory.connectable_to_subpath building_site))::temp1 in
      "\n\n\n"^(String.concat "\n" temp2)^"\n\n\n";; 

    let instructions_for_printersfile printer_equipped_types=
        let temp2=List.rev_map (
          function (x,compiled_correctly)->
          if compiled_correctly 
          then let modname=Assistance_dfn_endingless.to_module x in 
               "#install_printer "^(Assistance_dfa_module.capitalized_form modname)^".print_out;"^";"
          else ""
        ) printer_equipped_types in
        let temp3="\n\n\n"::(List.rev ("\n\n\n"::temp2)) in
        let part2=String.concat "\n" temp3 in
        part2;;  

    let save_loadingsfile (root,rootless_path_for_loadingsfile) (dirs,hms)=
       let s=loadings (root,rootless_path_for_loadingsfile)
        (dirs,hms)
       and lm=Assistance_dfn_join.root_to_rootless root rootless_path_for_loadingsfile in
       Assistance_io.overwrite_with (Assistance_dfn_full.to_absolute_path lm) s;;
    
    let save_merlinfile (root,rootless_path_for_merlinfile) dirs=
        let s=instructions_for_merlinfile root dirs 
        and lm=Assistance_dfn_join.root_to_rootless root rootless_path_for_merlinfile in
        Assistance_io.overwrite_with (Assistance_dfn_full.to_absolute_path lm) s;;
  
    let save_printersfile (root,rootless_path_for_printersfile) printer_equipped_types=
       let s=instructions_for_printersfile printer_equipped_types
       and lm=Assistance_dfn_join.root_to_rootless root rootless_path_for_printersfile in
       let beg_mark="(*Registered printers start here *)"
       and end_mark="(*Registered printers end here *)" in
       Assistance_replace_inside.overwrite_between_markers_inside_file
       (Assistance_overwriter.of_string s)
       (beg_mark,end_mark)
       (Assistance_dfn_full.to_absolute_path lm);;
    
    
  
    let save_targetfile rootless_path_for_targetfile cs=
      let root_dir = Assistance_coma_state.root cs in 
      let s1=Assistance_crobj_parsing.unparse(Assistance_coma_state_automatic.to_concrete_object cs) in
      let lt=Assistance_dfn_join.root_to_rootless root_dir rootless_path_for_targetfile in
      Assistance_io.overwrite_with (Assistance_dfn_full.to_absolute_path lt) s1;;
    
    
    
    let write_all 
    (
      rootless_path_for_targetfile,
      rootless_path_for_loadingsfile,
      rootless_path_for_printersfile
      )
      uple= 
      let (cs,directories,printer_equipped_types)=uple in
      let root_dir = Assistance_coma_state.root cs in  
      let hms=Assistance_coma_state.up_to_date_elesses cs in 
       (
        save_loadingsfile (root_dir,rootless_path_for_loadingsfile) (directories,hms);
        save_targetfile rootless_path_for_targetfile cs;
        save_printersfile (root_dir,rootless_path_for_printersfile) printer_equipped_types;
       );;
    
    let save_all cs=write_all 
      (
        Assistance_coma_constant.rootless_path_for_targetfile,
        Assistance_coma_constant.rootless_path_for_loadingsfile,
        Assistance_coma_constant.rootless_path_for_printersfile
      )
      (
	      Assistance_coma_state.uple_form cs
      );;

end;;  

let save = Private.save_all;;



end;;






module Assistance_modify_coma_state=struct

(* 

#use"Compilation_management/modify_coma_state.ml";;

*)



module Physical = struct 

let forget_modules cs mod_names=
   let new_fw=
      Assistance_fw_wrapper.forget_modules (cs.Assistance_coma_state_t.frontier_with_unix_world) mod_names in   
   Assistance_coma_state_automatic.set_frontier_with_unix_world cs new_fw;;

let forget_rootless_paths cs rootless_paths=
   let new_fw=Assistance_fw_wrapper.remove_files (cs.Assistance_coma_state_t.frontier_with_unix_world) rootless_paths in   
   Assistance_coma_state_automatic.set_frontier_with_unix_world cs new_fw ;;   


let recompile cs =
   let (new_fw,(changed_compilables,changed_noncompilables))=Assistance_fw_wrapper.inspect_and_update (cs.Assistance_coma_state_t.frontier_with_unix_world) in   
   let new_cs= Assistance_coma_state_automatic.set_frontier_with_unix_world cs new_fw in 
   (new_cs,changed_compilables,changed_noncompilables);;

let refresh config =
   let root = config.Assistance_fw_configuration_t.root in 
   let _=(Assistance_more_unix.create_subdirs_and_fill_files_if_necessary root
    Assistance_coma_constant.minimal_set_of_needed_dirs 
        Assistance_coma_constant.conventional_files_with_minimal_content) in 
   let fw = Assistance_fw_initialize.init config in
   let cs0 = Assistance_coma_state_automatic.empty_one config in  
   Assistance_coma_state_automatic.set_frontier_with_unix_world cs0 fw;;

let register_rootless_paths cs rps=
   let (new_fw,(c_paths,nc_paths))=Assistance_fw_wrapper.register_rootless_paths (cs.Assistance_coma_state_t.frontier_with_unix_world) rps in   
   (Assistance_coma_state_automatic.set_frontier_with_unix_world cs new_fw,c_paths) ;;

let relocate_module_to cs mod_name new_subdir=
   let new_fw=Assistance_fw_wrapper.relocate_module_to (cs.Assistance_coma_state_t.frontier_with_unix_world) mod_name new_subdir in   
   Assistance_coma_state_automatic.set_frontier_with_unix_world cs new_fw ;;

let rename_module cs old_middle_name new_nonslashed_name=
  let old_nm=Assistance_dfn_middle.to_module old_middle_name in
  let new_nm=Assistance_dfa_module.of_line (Assistance_no_slashes.to_string new_nonslashed_name) in  
  let old_acolytes=Assistance_coma_state.acolytes_at_module cs old_nm in
  let separated_acolytes_below=Assistance_option.filter_and_unpack(
    fun mn->
     if List.mem old_nm (Assistance_coma_state.ancestors_at_module cs mn)
    then Some(Assistance_image.image (Assistance_dfn_full.to_rootless) (Assistance_coma_state.acolytes_at_module cs mn))
    else None
) (Assistance_coma_state.ordered_list_of_modules cs) in
  let all_acolytes_below=List.flatten separated_acolytes_below in
  let old_acolyte_paths=Assistance_image.image Assistance_dfn_full.to_rootless old_acolytes in 
  let old_fw = Assistance_coma_state.frontier_with_unix_world cs in 
  let new_fw = Assistance_fw_wrapper.rename_module old_fw old_acolyte_paths new_nm all_acolytes_below in 
  Assistance_coma_state.set_frontier_with_unix_world cs new_fw ;;

let rename_subdirectory cs (old_subdir,new_subdir)=
   let new_fw=Assistance_fw_wrapper.rename_subdirectory_as (cs.Assistance_coma_state_t.frontier_with_unix_world) (old_subdir,new_subdir) in   
   Assistance_coma_state_automatic.set_frontier_with_unix_world cs new_fw ;;


exception Rename_string_or_value_exn of string ;;


let rename_string_or_value cs old_sov new_sov =
   let old_fw = Assistance_coma_state.frontier_with_unix_world cs in 
   let (new_fw,(changed_compilable_files,changed_noncompilable_files))=(
      if not(String.contains old_sov '.')
      then let (fw,(changed_c_files,changed_nc_files))= Assistance_fw_wrapper.replace_string old_fw (old_sov,new_sov) in 
           (fw,(changed_c_files,changed_nc_files))
      else 
           let j=Assistance_substring.leftmost_index_of_in "." old_sov in
           if j<0 
           then raise(Rename_string_or_value_exn(old_sov))
           else let module_name=Assistance_cull_string.beginning (j-1) old_sov in
                let endingless=Assistance_coma_state.decipher_module cs  module_name 
                and path=Assistance_coma_state.decipher_path cs  module_name in 
                let nm=Assistance_dfn_endingless.to_module endingless in
                let pre_temp2=(Assistance_coma_state.ancestors_at_module cs nm)@[nm] in
                let temp2=Assistance_image.image (Assistance_coma_state.endingless_at_module cs) pre_temp2 in
                let preceding_files=Assistance_image.image  (fun eless2->
   	               Assistance_dfn_full.to_absolute_path(Assistance_dfn_join.to_ending eless2 Assistance_dfa_ending.ml)
                ) temp2 in
                Assistance_fw_wrapper.replace_value old_fw (preceding_files,path) (old_sov,new_sov)
   ) in 
   (Assistance_coma_state.set_frontier_with_unix_world cs new_fw,(changed_compilable_files,changed_noncompilable_files));;       



end;;

module Internal = struct

let forget_modules cs mns =
  let old_endinglesses = Assistance_image.image (Assistance_coma_state.endingless_at_module cs) mns in   
  let cs2=Assistance_coma_state.unregister_modules  cs old_endinglesses in
  let new_dirs=Assistance_coma_state.compute_subdirectories_list cs2  in 
  let temp1 = Assistance_image.image Assistance_dfa_module.to_line mns in 
  let temp2 = Assistance_cartesian.product temp1 [".cm*";".d.cm*";".caml_debuggable"] in 
  let _=Assistance_image.image
               (fun (mname,edg)->
                let cmd="rm -f _build/"^mname^edg in
                Assistance_unix_command.uc(cmd))
               temp2 in
  let cs3=Assistance_coma_state.set_directories cs2 new_dirs in 
  cs3;;    


let forget_rootless_paths cs rootless_paths=
   let compilable_paths = List.filter Assistance_dfn_rootless.is_compilable rootless_paths in 
   let the_root = Assistance_coma_state.root cs in 
   let full_paths = Assistance_image.image (Assistance_dfn_join.root_to_rootless the_root) compilable_paths in  
   Assistance_coma_state.unregister_mlx_files cs full_paths ;; 


let recompile (cs,changed_compilables,changed_noncompilables) = 
   let new_fw = cs.Assistance_coma_state_t.frontier_with_unix_world in 
   let ref_for_changed_modules=ref[] 
  and ref_for_changed_shortpaths=ref[] in
  let declare_changed=(fun nm->
    ref_for_changed_modules:=nm::(!ref_for_changed_modules);
    ref_for_changed_shortpaths:=((!ref_for_changed_shortpaths)@
                        (Assistance_coma_state.rootless_lines_at_module cs nm))
    ) in
  let cs_walker=ref(cs) in   
  let _=List.iter (fun mname->
    match Assistance_coma_state.Late_Recompilation.quick_update (!cs_walker) (new_fw,changed_compilables) mname with
    None->()
    |Some(pr_modif_time,mli_modif_time,direct_fathers)->
    (
    declare_changed(mname);
    cs_walker:=Assistance_coma_state.set_principal_mt_at_module (!cs_walker) mname pr_modif_time;
    cs_walker:=Assistance_coma_state.set_mli_mt_at_module (!cs_walker) mname mli_modif_time;
    cs_walker:=Assistance_coma_state.set_direct_fathers_at_module (!cs_walker) mname direct_fathers;
    cs_walker:=Assistance_coma_state.set_product_up_to_date_at_module (!cs_walker) mname false;
    )
)(Assistance_coma_state.ordered_list_of_modules cs) in
let _=Assistance_coma_state.PrivateThree.announce_changed_noncompilables changed_noncompilables in
let changed_modules=List.rev(!ref_for_changed_modules) in 
if changed_modules=[] then (!cs_walker) else
let _=Assistance_coma_state.PrivateThree.announce_changed_modules changed_modules in
let ((cs2,nms_to_be_updated),rootless_paths)= 
 (Assistance_coma_state.PrivateThree.put_md_list_back_in_order false 
  (!cs_walker) changed_modules,
(!ref_for_changed_shortpaths))  in   
   if nms_to_be_updated=[] then cs2 else
let new_dirs=Assistance_coma_state.compute_subdirectories_list cs2  in
let (cs3,rejected_pairs,accepted_pairs)=
       Assistance_coma_state.Ocaml_target_making.usual_feydeau cs2 nms_to_be_updated in 
let rejected_mns=Assistance_image.image snd rejected_pairs in  
let new_preqt=Assistance_image.image(
        fun (mn,_)->(mn,not(List.mem mn rejected_mns))
      )  (Assistance_coma_state_automatic.preq_types cs3) in   
let cs4=Assistance_coma_state_automatic.set_directories cs3 new_dirs in 
let cs5=Assistance_coma_state_automatic.set_preq_types cs4 new_preqt in 
cs5 ;;


let refresh cs = 
        let dir =Assistance_coma_state_automatic.root cs  in 
        let fw1 = cs.Assistance_coma_state_t.frontier_with_unix_world in 
        let temp1=fw1.Assistance_fw_wrapper_t.compilable_files in
        let temp2=Assistance_coma_state.Simplified_ts_creation.classify_according_to_module dir temp1 in
        let temp3=Assistance_coma_state.Simplified_ts_creation.compute_dependencies temp2 in
        let temp4=Assistance_image.image (fun (mname,_)->
           let (opt,opt_ap,pr_rless,pr_ap) = List.assoc mname temp2 in 
            match opt with 
             None -> [pr_rless]
            |Some(mli_rless) -> [mli_rless;pr_rless]
         ) temp3 in 
        let rlesses_in_good_order=List.flatten temp4 in 
        let (failures,cs1)=Assistance_coma_state.Try_to_register.mlx_files cs rlesses_in_good_order in  
        let pre_preqt=Assistance_coma_state.printer_equipped_types_from_data cs1 in
        let l_mod=Assistance_coma_state_automatic.ordered_list_of_modules cs1 in 
        let (cs2,rejected_pairs,_)=
          Assistance_coma_state.Ocaml_target_making.usual_feydeau 
          cs1 l_mod in
        let rejected_endinglesses=Assistance_image.image snd rejected_pairs in 
        let new_ptypes=Assistance_image.image (fun mn->(mn,not(List.mem mn rejected_endinglesses))) pre_preqt in 
        let new_dirs=Assistance_coma_state.compute_subdirectories_list cs2 in
        let cs3=Assistance_coma_state_automatic.set_directories cs2 new_dirs in 
        let cs4=Assistance_coma_state_automatic.set_preq_types cs3 new_ptypes in
        cs4    ;;


let register_rootless_paths cs rootless_paths=
  Assistance_coma_state.register_mlx_files cs rootless_paths ;;

let relocate_module_to cs mn new_subdir=
  let old_endingless = Assistance_coma_state.endingless_at_module cs mn in  
  let old_subdir = Assistance_dfn_endingless.to_subdirectory old_endingless in 
  let mn=Assistance_dfn_endingless.to_module old_endingless in
  let old_acolytes= Assistance_coma_state.acolytes_at_module cs mn in
  let new_acolytes=Assistance_image.image 
    (fun mlx->Assistance_dfn_full.relocate mlx new_subdir) old_acolytes in
  let new_name=Assistance_dfn_full.to_endingless
   (List.hd new_acolytes) in
  let principal_mt=Assistance_coma_state.md_compute_modification_time new_name 
                         (Assistance_coma_state.principal_ending_at_module cs mn)
  and mli_mt=Assistance_coma_state.md_compute_modification_time new_name Assistance_dfa_ending.mli in
  let s_subdir = Assistance_dfa_subdirectory.without_trailing_slash new_subdir in 
  let cs2=Assistance_coma_state.set_subdir_at_module cs mn new_subdir in 
  let cs3=Assistance_coma_state.set_principal_mt_at_module cs2 mn principal_mt in 
  let cs4=Assistance_coma_state.set_mli_mt_at_module cs3 mn mli_mt in 
  let old_preq_types = Assistance_coma_state.preq_types cs4 in 
  let new_preq_types=Assistance_image.image (fun (h,bowl)->
     (Assistance_dfn_endingless.rename_endsubdirectory (old_subdir,s_subdir) h,bowl)) old_preq_types in 
  let cs5=Assistance_coma_state.set_preq_types cs4 new_preq_types in 
  cs5;;   


let rename_module cs2 old_middle_name new_nonslashed_name=
  let root_dir=Assistance_coma_state.root cs2 in 
  let old_nm=Assistance_dfn_middle.to_module old_middle_name in
  let s_root=Assistance_dfa_root.connectable_to_subpath root_dir in   
  let s_build_dir=Assistance_dfa_subdirectory.connectable_to_subpath (Assistance_coma_constant.usual_build_subdir) in  
  let new_nm=Assistance_dfa_module.of_line (Assistance_no_slashes.to_string new_nonslashed_name) in
  let old_acolytes=Assistance_coma_state.acolytes_at_module cs2 old_nm in
  let new_acolytes=Assistance_image.image (
    fun (Assistance_dfn_full_t.J(r,s,m,e))->Assistance_dfn_full_t.J(r,s,new_nm,e)
  ) old_acolytes in 
  let new_eless=Assistance_dfn_full.to_endingless(List.hd new_acolytes) in
  let _=Assistance_unix_command.uc
      ("rm -f "^s_root^s_build_dir^
      (Assistance_dfa_module.to_line old_nm)^
      ".cm* ") in     
  let principal_mt=Assistance_coma_state.md_compute_modification_time new_eless (Assistance_coma_state.principal_ending_at_module cs2 old_nm)
  and mli_mt=Assistance_coma_state.md_compute_modification_time new_eless Assistance_dfa_ending.mli in
  let cs3=Assistance_coma_state_automatic.change_one_module_name cs2 old_nm new_nm in 
  let cs4=Assistance_coma_state.set_principal_mt_at_module cs3 new_nm principal_mt in 
  let cs5=Assistance_coma_state.set_mli_mt_at_module cs4 new_nm mli_mt in 
  let cs6=Assistance_coma_state.set_product_up_to_date_at_module cs5 new_nm false in 
  let replacer=Assistance_image.image(function x->if x=old_nm then new_nm else x) in
  let old_eless = Assistance_dfn_join.root_to_middle root_dir old_middle_name in
  let eless_replacer=(fun x->if x=old_eless then new_eless else x) in 
  let old_preq_types=Assistance_coma_state.preq_types cs6 in 
  let new_preq_types=Assistance_image.image (fun (h,bowl)->(eless_replacer h,bowl)) old_preq_types in 
  let cs7=Assistance_coma_state.set_preq_types cs6 new_preq_types in 
  let cs_walker=ref(cs7) in 
  let _=List.iter(fun mn->
      let old_dirfath=Assistance_coma_state.direct_fathers_at_module (!cs_walker) mn
      and old_ancestors=Assistance_coma_state.ancestors_at_module (!cs_walker) mn in
      (
      cs_walker:=(Assistance_coma_state.set_direct_fathers_at_module (!cs_walker) mn (replacer old_dirfath)) ;
      cs_walker:=(Assistance_coma_state.set_ancestors_at_module (!cs_walker) mn (replacer old_ancestors)); 
      )
  )(Assistance_coma_state.follows_it cs2 old_nm) in
  let cs8=(!cs_walker) in    
  let cs9=recompile (cs8,[],[]) in 
  cs9;;

let rename_subdirectory cs old_subdir new_subdir=
  let rename_in_sd=(fun sd -> 
     match Assistance_dfa_subdirectory.soak (old_subdir,new_subdir) sd with 
     Some(new_sd) -> new_sd 
     |None -> sd
   ) in 
  let cs1=Assistance_coma_state_automatic.modify_all_subdirs cs rename_in_sd in 
  let cs2=Assistance_coma_state_automatic.modify_all_needed_dirs cs1 rename_in_sd in 
   let new_dirs=Assistance_image.image rename_in_sd (Assistance_coma_state.directories cs2)
   and new_peqt=Assistance_image.image (fun (eless,is_compiled_correctly)->
       let final_eless = (
           match Assistance_dfn_endingless.soak (old_subdir,new_subdir) eless with 
        Some(new_eless) -> new_eless
        |None -> eless
       ) in 
       (final_eless,is_compiled_correctly)
   )(Assistance_coma_state.preq_types cs2) in
   let cs3= Assistance_coma_state.set_directories cs2 new_dirs in 
   let cs4= Assistance_coma_state.set_preq_types cs3 new_peqt in 
   cs4;; 


let rename_string_or_value cs = recompile (cs,[],[]);; 

end;;

module Physical_followed_by_internal = struct

exception Forget_modules_exn of (Assistance_dfa_module_t.t * Assistance_dfa_module_t.t list) list ;;

let forget_modules cs mod_names= 
  let check = Assistance_coma_state.check_module_sequence_for_forgettability cs mod_names in 
  if check <> []
  then raise(Forget_modules_exn(check))
  else 
  let cs2=Physical.forget_modules cs mod_names  in
  Internal.forget_modules cs2 mod_names ;;

exception Forget_rootless_paths_exn of (Assistance_dfa_module_t.t * Assistance_dfn_rootless_t.t list) list ;;

let forget_rootless_paths cs rootless_paths= 
  let check = Assistance_coma_state.check_rootless_path_sequence_for_forgettability cs rootless_paths in 
  if check <> []
  then raise(Forget_rootless_paths_exn(check))
  else 
  let cs2=Physical.forget_rootless_paths cs rootless_paths  in
  Internal.forget_rootless_paths cs2 rootless_paths;;


let recompile cs = 
  let (cs2,changed_compilables,changed_noncompilables)=Physical.recompile cs  in
  Internal.recompile (cs2,changed_compilables,changed_noncompilables);;
  
let refresh cs =
   let cs2=Physical.refresh (Assistance_coma_state_automatic.configuration cs)  in
   Internal.refresh cs2;;

let register_rootless_paths cs rootless_paths= 
   let (cs2,c_paths)=Physical.register_rootless_paths cs rootless_paths in
   Internal.register_rootless_paths cs2 c_paths;;

let relocate_module_to cs mod_name new_subdir= 
  let cs2=Physical.relocate_module_to cs mod_name  new_subdir  in
  Internal.relocate_module_to cs2 mod_name  new_subdir;;


let rename_module cs old_middle_name new_nonslashed_name=
   let cs2=Physical.rename_module cs old_middle_name new_nonslashed_name in
   Internal.rename_module cs2 old_middle_name new_nonslashed_name;;

let rename_subdirectory cs old_subdir new_subdir=
   let cs2=Physical.rename_subdirectory cs (old_subdir,new_subdir) in
   Internal.rename_subdirectory cs2 old_subdir new_subdir;;

let rename_string_or_value cs old_sov new_sov =
   let (cs2,_)=Physical.rename_string_or_value cs old_sov new_sov in
   Internal.rename_string_or_value cs2;;

end;;



module After_checking = struct

      let forget_modules cs mod_names=
         let _=Assistance_coma_state.Recent_changes.check_for_changes cs in 
         Physical_followed_by_internal.forget_modules cs mod_names;; 

      let forget_rootless_paths cs rootless_paths=
         let _=Assistance_coma_state.Recent_changes.check_for_changes cs in 
         Physical_followed_by_internal.forget_rootless_paths cs rootless_paths;;    

      (* No check needed before recompiling *)

      (* No check needed before refreshing *)

      let register_rootless_paths cs rootless_paths=
         let _=Assistance_coma_state.Recent_changes.check_for_changes cs in 
         Physical_followed_by_internal.register_rootless_paths cs rootless_paths;; 

      let relocate_module_to cs old_module new_subdir=
         let _=Assistance_coma_state.Recent_changes.check_for_changes cs in 
         Physical_followed_by_internal.relocate_module_to cs old_module new_subdir;; 

      let rename_subdirectory  cs old_subdir new_subdir=
         let _=Assistance_coma_state.Recent_changes.check_for_changes cs in 
         Physical_followed_by_internal.rename_subdirectory  cs old_subdir new_subdir;; 

      let rename_module cs old_middle_name new_nonslashed_name=
         let _=Assistance_coma_state.Recent_changes.check_for_changes cs in 
         Physical_followed_by_internal.rename_module cs old_middle_name new_nonslashed_name;; 

      let rename_string_or_value cs old_sov new_sov=
         let _=Assistance_coma_state.Recent_changes.check_for_changes cs in 
         Physical_followed_by_internal.rename_string_or_value cs old_sov new_sov;; 

end;;

module And_backup = struct

      module Private = struct

            let backup cs diff opt= 
            if not(Assistance_dircopy_diff.is_empty diff) 
            then Assistance_reflect_change_in_github.backup
                  (Assistance_coma_state_automatic.configuration cs) 
                  diff opt
            else (print_string "No recent changes to commit ...";flush stdout);;

      end;;    

      let forget_modules cs mod_names=
         let  cs2=After_checking.forget_modules cs mod_names in 
         Assistance_coma_state.reflect_latest_changes_in_github cs2 None;; 

      let forget_rootless_paths cs rootless_paths=
         let cs2=After_checking.forget_rootless_paths cs rootless_paths in 
         Assistance_coma_state.reflect_latest_changes_in_github cs2 None ;; 


      let recompile cs opt_comment=
         let cs2=Physical_followed_by_internal.recompile cs  in 
         Assistance_coma_state.reflect_latest_changes_in_github cs2 opt_comment ;; 

      (* No backup during refresh *)   

      let register_rootless_paths cs rootless_paths=
         let descr = String.concat " , " (Assistance_image.image Assistance_dfn_rootless.to_line rootless_paths) in 
         let cs2=After_checking.register_rootless_paths cs rootless_paths  in 
         let msg="register "^descr in 
         Assistance_coma_state.reflect_latest_changes_in_github cs2 (Some msg) ;; 

      let relocate_module_to cs old_module new_subdir=
         let cs2=After_checking.relocate_module_to cs old_module new_subdir  in
         let msg="move "^(Assistance_dfa_module.to_line old_module)^" to "^(Assistance_dfa_subdirectory.connectable_to_subpath new_subdir) in 
         Assistance_coma_state.reflect_latest_changes_in_github cs2 (Some msg) ;; 


      let rename_module cs old_middle_name new_nonslashed_name=
         let cs2=After_checking.rename_module cs old_middle_name new_nonslashed_name  in 
         let msg="rename "^(Assistance_dfa_module.to_line(Assistance_dfn_middle.to_module old_middle_name))^
                 " as "^(Assistance_no_slashes.to_string new_nonslashed_name) in       
         Assistance_coma_state.reflect_latest_changes_in_github cs2 (Some msg) ;; 

      let rename_subdirectory  cs old_subdir new_subdir=
         let cs2=After_checking.rename_subdirectory  cs old_subdir new_subdir  in 
         let msg="rename "^(Assistance_dfa_subdirectory.connectable_to_subpath old_subdir)^
                    " as "^(Assistance_dfa_subdirectory.connectable_to_subpath new_subdir) in 
         Assistance_coma_state.reflect_latest_changes_in_github cs2 (Some msg) ;; 

      let rename_string_or_value cs old_sov new_sov=
         let cs2=After_checking.rename_string_or_value cs old_sov new_sov  in 
         let msg="rename "^old_sov^" as "^new_sov in 
         Assistance_coma_state.reflect_latest_changes_in_github cs2 (Some msg) ;; 

end;;



module And_save = struct 

      let forget_modules cs mod_names=
         let cs2=And_backup.forget_modules cs mod_names in 
         let _=Assistance_save_coma_state.save cs2 in 
         cs2;;

      let forget_rootless_paths cs rootless_paths=
         let cs2=And_backup.forget_rootless_paths cs rootless_paths in 
         let _=Assistance_save_coma_state.save cs2 in 
         cs2;;

      let internet_access cs bowl=   
         let cs2=Assistance_coma_state_automatic.set_push_after_backup cs bowl in 
         let _=Assistance_save_coma_state.save cs2 in 
         cs2;;
      
      let recompile cs opt_comment=
         let cs2=And_backup.recompile cs opt_comment in 
         let _=Assistance_save_coma_state.save cs2 in 
         cs2;;
      

      let refresh cs =
         let cs2=Physical_followed_by_internal.refresh cs  in 
         let _=Assistance_save_coma_state.save cs2 in 
         cs2;;  

      let register_rootless_paths cs rootless_path=
         let cs2=And_backup.register_rootless_paths cs rootless_path in 
         let _=Assistance_save_coma_state.save cs2 in 
         cs2;;  

      let relocate_module_to cs old_module new_subdir=
      let cs2 = And_backup.relocate_module_to cs old_module new_subdir in 
      let _=Assistance_save_coma_state.save cs2 in 
      cs2;;   

      let rename_subdirectory cs old_subdir new_subdir=
         let cs2=And_backup.rename_subdirectory cs old_subdir new_subdir in 
         let _=Assistance_save_coma_state.save cs2 in 
         cs2;;  


      let rename_module cs old_middle_name new_nonslashed_name=
         let cs2=And_backup.rename_module cs old_middle_name new_nonslashed_name in 
         let _=Assistance_save_coma_state.save cs2 in 
         cs2;;  


      let rename_string_or_value cs old_sov new_sov=
         let cs2=And_backup.rename_string_or_value cs old_sov new_sov in 
         let _=Assistance_save_coma_state.save cs2 in 
         cs2;;  

end ;;

module Reference = struct 

      let forget_modules pcs mod_names=
         let new_cs = And_save.forget_modules (!pcs) mod_names in 
         pcs:=new_cs;;

      let forget_rootless_paths pcs rootless_paths=
         let new_cs = And_save.forget_rootless_paths (!pcs) rootless_paths in 
         pcs:=new_cs;; 

      let initialize pcs =
      let new_cs = Assistance_coma_state.read_persistent_version (!pcs) in 
      pcs:=new_cs;;

      let initialize_if_empty pcs =
         if Assistance_coma_state.system_size (!pcs)  = 0 
         then initialize pcs;;

      let internet_access pcs bowl=
         let new_cs = And_save.internet_access (!pcs) bowl in 
          pcs:=new_cs;;

      let recompile pcs opt_comment=
         let new_cs = And_save.recompile (!pcs) opt_comment in 
         pcs:=new_cs;;


      let refresh pcs =
         let new_cs = And_save.refresh (!pcs)  in 
         pcs:=new_cs;;

      let register_rootless_paths pcs rootless_paths=
         let new_cs = And_save.register_rootless_paths (!pcs) rootless_paths in 
         pcs:=new_cs;;


      let relocate_module_to pcs old_module new_subdir=
         let new_cs = And_save.relocate_module_to (!pcs) old_module new_subdir in 
         pcs:=new_cs;;  


      let rename_subdirectory pcs old_subdir new_subdir=
         let new_cs = And_save.rename_subdirectory (!pcs) old_subdir new_subdir in 
         pcs:=new_cs;;
         

      let rename_module pcs old_middle_name new_nonslashed_name=
         let new_cs = And_save.rename_module (!pcs) old_middle_name new_nonslashed_name in 
         pcs:=new_cs;;


      let rename_string_or_value pcs old_sov new_sov=
         let new_cs = And_save.rename_string_or_value (!pcs) old_sov new_sov in 
         pcs:=new_cs;;


end ;;


module Syntactic_sugar = struct 

let forget cs_ref data = 
   let ref_for_modules = ref []
   and ref_for_paths = ref [] in 
   let _=List.iter (
      fun descr ->
        if String.contains descr '.'
        then ref_for_paths:= (Assistance_dfn_rootless.of_line descr)::(!ref_for_paths)
        else ref_for_modules:= (Assistance_dfa_module.of_line descr) ::(!ref_for_modules)
   ) data in
   let all_paths = List.rev(!ref_for_paths) 
   and all_modules =  List.rev(!ref_for_modules) in 
   let _=(if all_paths=[] then () else Reference.forget_rootless_paths cs_ref all_paths) in 
   let _=(if all_modules=[] then () else Reference.forget_modules cs_ref all_modules) in 
   ();;


let register_several cs_ref lines =
   let rootless_paths = Assistance_image.image Assistance_dfn_rootless.of_line lines in 
   Reference.register_rootless_paths cs_ref  rootless_paths ;;

let register_one cs_ref line = register_several cs_ref [line];;

let relocate_module_to cs_ref old_module_name new_subdir=
    let mn = Assistance_dfa_module.of_line(String.uncapitalize_ascii old_module_name) in
    Reference.relocate_module_to cs_ref mn new_subdir ;;

let rename_module cs_ref old_module_name new_name=
   let mn = Assistance_dfa_module.of_line(String.uncapitalize_ascii old_module_name) in
   let old_eless = Assistance_coma_state.endingless_at_module (!cs_ref) mn in
   let old_middle_name = Assistance_dfn_endingless.to_middle old_eless in    
   let new_nonslashed_name = Assistance_no_slashes.of_string (String.uncapitalize_ascii new_name) in 
   Reference.rename_module cs_ref old_middle_name new_nonslashed_name;; 


let rename_subdirectory cs_ref old_subdirname new_subdir_short_name=
    let old_subdir = Assistance_coma_state.find_subdir_from_suffix (!cs_ref) old_subdirname  in
    let new_subdir = Assistance_coma_state.compute_long_subdir_name (!cs_ref) old_subdir new_subdir_short_name  in 
    Reference.rename_subdirectory cs_ref old_subdir new_subdir ;;


end;;



end;;






module Assistance_usual_coma_state=struct

(* 

#use"Compilation_management/usual_coma_state.ml";;

*)

exception No_module_with_name of string;;

module Private = struct 

let main_ref=
  let (root,backup_dir,githubbing)=Assistance_coma_big_constant.This_World.triple 
  and url=Assistance_coma_big_constant.github_url in 
  let config = Assistance_fw_configuration.constructor (root,backup_dir,githubbing,url,[],[Assistance_coma_constant.githubbed_archive_subdir]) in 
  ref(Assistance_coma_state_automatic.empty_one  config);;
end;;

let above modname=Assistance_coma_state.Almost_concrete.local_above (!(Private.main_ref)) modname;; 


let add_printer_equipped_type mn=
   let old_cs=(!(Private.main_ref)) in 
   let new_cs=Assistance_coma_state.add_printer_equipped_type old_cs (mn,true) in 
   (Private.main_ref := new_cs ;
    Assistance_save_coma_state.save new_cs);;

let all_endinglesses ()=Assistance_coma_state.all_endinglesses (!(Private.main_ref)) ;; 
let all_principals ()=Assistance_coma_state.all_principals (!(Private.main_ref)) ;; 

let below modname=Assistance_coma_state.Almost_concrete.local_below (!(Private.main_ref)) modname;;

let census_of_foreigners ()= Assistance_coma_state.census_of_foreigners (!(Private.main_ref));;

let clean_debug_dir ()=Assistance_coma_state.clean_debug_dir (!(Private.main_ref));;
let clean_exec_dir ()=Assistance_coma_state.clean_exec_dir (!(Private.main_ref));;

let decipher_path pathname= Assistance_coma_state.decipher_path (!(Private.main_ref)) pathname;;
let decipher_module modname= Assistance_coma_state.decipher_module (!(Private.main_ref)) modname;;

let directly_above modname=Assistance_coma_state.Almost_concrete.local_directly_above (!(Private.main_ref)) modname;;
let directly_below modname=Assistance_coma_state.Almost_concrete.local_directly_below (!(Private.main_ref)) modname;;

let duplicate_module old_t1 old_t2=
  Assistance_coma_state.duplicate_module (!(Private.main_ref)) old_t1 old_t2;;

let find_endingless modname= 
  Assistance_coma_state.endingless_from_mildly_capitalized_module_name (!(Private.main_ref)) modname;;


let fix_lag () =
   let config = Assistance_coma_state_automatic.configuration (!(Private.main_ref)) in 
   let diff = Assistance_check_ocaml_dircopy.check config in 
   let cs2 = Assistance_coma_state_automatic.impose_last_changes (!(Private.main_ref)) diff in 
   let cs3 = Assistance_coma_state.reflect_latest_changes_in_github cs2 (Some"Fix lag") in 
   (Private.main_ref:=cs3;Assistance_save_coma_state.save cs3);;


let forget_one modname=Assistance_modify_coma_state.Syntactic_sugar.forget Private.main_ref [modname];;

let forget_several modnames=Assistance_modify_coma_state.Syntactic_sugar.forget Private.main_ref modnames;;

let initialize ()=Assistance_modify_coma_state.Reference.initialize Private.main_ref ;; 

let initialize_if_empty ()=Assistance_modify_coma_state.Reference.initialize_if_empty Private.main_ref ;;                       

let initialize ()=Assistance_modify_coma_state.Reference.initialize Private.main_ref ;; 

let internet_access bowl=Assistance_modify_coma_state.Reference.internet_access Private.main_ref bowl;;

let latest_changes ()=Assistance_coma_state.latest_changes (!(Private.main_ref));;

let list_values_from_module_in_modulesystem module_name=
   Assistance_coma_state.Values_in_modules.list_values_from_module_in_modulesystem 
   (!(Private.main_ref)) module_name;;

let main_ref=Private.main_ref;;

let modules_using_value x=Assistance_coma_state.modules_using_value (!(Private.main_ref)) x;;

let recompile opt=Assistance_modify_coma_state.Reference.recompile Private.main_ref opt;;
   

let refresh ()=Assistance_modify_coma_state.Reference.refresh Private.main_ref;;

let register_rootless_line x=Assistance_modify_coma_state.Syntactic_sugar.register_one Private.main_ref x;;
  
let register_rootless_lines x=Assistance_modify_coma_state.Syntactic_sugar.register_several Private.main_ref x;;

let relocate_module_to old_module_name new_subdir=
   Assistance_modify_coma_state.Syntactic_sugar.relocate_module_to Private.main_ref old_module_name new_subdir;;

let rename_subdirectory old_subdirname new_subdirname=
    Assistance_modify_coma_state.Syntactic_sugar.rename_subdirectory Private.main_ref old_subdirname new_subdirname;;

let rename_module old_name new_name=
   Assistance_modify_coma_state.Syntactic_sugar.rename_module Private.main_ref old_name new_name;;


let rename_string_or_value old_name new_name=
   Assistance_modify_coma_state.Reference.rename_string_or_value
   (Private.main_ref) old_name new_name;;

let show_value_occurrences_in_modulesystem module_name=
   Assistance_coma_state.Values_in_modules.show_value_occurrences_in_modulesystem
   (!(Private.main_ref)) module_name;;

let start_debugging ()=Assistance_coma_state.start_debugging (!(Private.main_ref));;
let start_executing short_path= Assistance_coma_state.start_executing (!(Private.main_ref)) short_path;;


end;;

