(*

#use"Decomposed_filename/dfn_common.ml";;

*)

exception No_dot_in_string_to_rootless of string;;
exception Decompose_absolute_path_using_root_exn of Absolute_path.t * Dfa_root_t.t;;

let string_of_sm (s,m)=
   let (Dfa_subdirectory_t.SD sub)=s 
   and (Dfa_module_t.M mn)=m in 
   if sub=""
   then mn
   else sub^"/"^mn;;

let string_to_sm s=
   let (sub,mn)= Cull_string.split_wrt_rightmost s '/' in 
   (Dfa_subdirectory_t.SD sub,Dfa_module_t.M mn);;     

let string_to_rootless line=
  let (rest,ending) = Cull_string.split_wrt_rightmost line '.' in 
  if rest="" then raise(No_dot_in_string_to_rootless(line)) else 
  let (s,m) = string_to_sm rest in 
  Dfn_rootless_t.J(s,m,Dfa_ending_t.E ending);;

   
let decompose_absolute_path_using_root ap root=
  let s_root=Dfa_root.without_trailing_slash root  
  and s_ap = Absolute_path.to_string ap in 
  let ns=String.length(s_root)
  and nw=String.length(s_ap) in
  if (ns+1)>nw then raise(Decompose_absolute_path_using_root_exn(ap,root)) else
  if (String.sub s_ap 0 (ns+1))<>(s_root^"/") then raise(Decompose_absolute_path_using_root_exn(ap,root)) else
  string_to_rootless(String.sub s_ap (ns+1) (nw-ns-1));;

