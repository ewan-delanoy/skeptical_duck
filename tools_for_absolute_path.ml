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

