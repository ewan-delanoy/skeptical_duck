(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_battery_example.ml";;

*)

module T= Jvsp_types ;;
open Jvng_types ;;

module Private = struct 

(* The values below are defined by the pt function defined above *)
let list_for_precomputed_first_tokens = 
  let liofli = Image.image (fun tok->[tok]) in 
  Image.image (fun (str,l)->(Jvng_duplicated_name.of_string  str,l)) [
   "FieldModifier",liofli [T.FINAL_T;T.PRIVATE_T;T.PROTECTED_T;T.PUBLIC_T; T.SNAIL_T;T.STATIC_T;T.TRANSIENT_T;T.VOLATILE_T];
   "UnannType",liofli [T.BOOLEAN_T; T.BYTE_T;T.CHAR_T; T.DOUBLE_T;T.FLOAT_T;T.IDENTIFIER_T;T.INT_T; T.LONG_T;T.SHORT_T];
] ;;

let d = Jvng_duplicated_name.of_string ;;

let for_ClassBodyDeclaration rem_list = 
  if Jvsp_token_types_list.starts_with rem_list [T.STATIC_T;T.LC_T] then Some (d "StaticInitilaizer") else 
  let h = List.hd(Jvsp_token_types_list.unveil rem_list) in 
  if h = T.SM_T then Some (d "Sm") else 
  if h = T.LC_T then Some (d "InstanceInitializer") else   
  match Jvsp_token_types_list.find_and_forget_opt (fun tok->
    not(List.mem tok [
        T.PUBLIC_T;T.PRIVATE_T;T.ABSTRACT_T;T.STATIC_T;
        T.FINAL_T;T.SEALED_T;T.NONSEALED_T;T.STRICTFP_T
     ])  
  ) rem_list with 
  None -> None 
  |Some rem_list2 -> 
  List.assoc_opt (List.hd(Jvsp_token_types_list.unveil rem_list2)) [
      T.CLASS_T,d "NormalClassDeclaration";
      T.ENUM_T,d "EnumDeclaration";
      T.RECORD_T,d "RecordDeclaration";
  ] ;; 

let for_ClassDeclaration rem_list = 
  match Jvsp_token_types_list.find_and_forget_opt (fun tok->
    not(List.mem tok [
        T.PUBLIC_T;T.PRIVATE_T;T.ABSTRACT_T;T.STATIC_T;
        T.FINAL_T;T.SEALED_T;T.NONSEALED_T;T.STRICTFP_T
     ])  
  ) rem_list with 
  None -> None 
  |Some rem_list2 -> 
  List.assoc_opt (List.hd(Jvsp_token_types_list.unveil rem_list2)) [
      T.CLASS_T,d "NormalClassDeclaration";
      T.ENUM_T,d "EnumDeclaration";
      T.RECORD_T,d "RecordDeclaration";
  ] ;; 

let for_Dims rem_list = 
   let h = List.hd(Jvsp_token_types_list.unveil rem_list) in 
   if not(List.mem h [T.LB_T;T.SNAIL_T]) then Some (d "false") else None;; 

let for_TypeParameters rem_list = 
   let h = List.hd(Jvsp_token_types_list.unveil rem_list) in 
   if h<>T.LT_T then Some (d "false") else None;; 

let for_TopLevelClassOrInterfaceDeclaration rem_list = 
  match Jvsp_token_types_list.find_and_forget_opt (fun tok->
    not(List.mem tok [
        T.PUBLIC_T;T.PRIVATE_T;T.ABSTRACT_T;T.STATIC_T;
        T.FINAL_T;T.SEALED_T;T.NONSEALED_T;T.STRICTFP_T
     ])  
  ) rem_list with 
  None -> None 
  |Some rem_list2 -> 
  List.assoc_opt (List.hd(Jvsp_token_types_list.unveil rem_list2)) [
      T.CLASS_T,d "ClassDeclaration";
      T.ENUM_T,d "ClassDeclaration";
      T.RECORD_T,d "ClassDeclaration";
      T.INTERFACE_T,d "InterfaceDeclaration";
      T.SNAIL_T,d "InterfaceDeclaration";
      T.SM_T,d "Sm";
  ] ;; 

let for_UnannType rem_list =    
    let l = Jvsp_token_types_list.unveil rem_list in 
    let nt = (fun k->List.nth l (k-1)) in
    if (nt 1=T.IDENTIFIER_T)&&(not(List.mem(nt 2)[T.DOT_T;T.LB_T;T.LT_T;T.SNAIL_T]))
    then Some (d "Identifier") 
    else None;; 

module La = struct 
  
  let direct = Jvng_local_analizer.direct ;;
   
  let first_trial_only = Jvng_local_analizer.first_trial_only ;;

  let make tester cases is_complete= 
  {
   first_approach = tester ;
   case_by_case = Image.image (fun (l,str)->(l,Jvng_duplicated_name.of_string  str)) cases; 
   analysis_is_complete = is_complete ;
  };; 

  let no_first_trial ll = Jvng_local_analizer.no_first_trial (
    Image.image (fun (l,str)->(l,Jvng_duplicated_name.of_string  str)) ll
  ) ;; 

end ;;  


let example = {
  deciders_for_optionals_or_stars = Image.image (fun (str,l)->(Jvng_duplicated_name.of_string  str,l)) [
   ("ClassExtends",La.direct [[T.EXTENDS_T]] ); 
   ("ClassImplements",La.direct [[T.IMPLEMENTS_T]] ); 
   ("ClassModifier",La.direct [[T.SNAIL_T];[T.PUBLIC_T];[T.PROTECTED_T];[T.PRIVATE_T];[T.ABSTRACT_T];
                            [T.STATIC_T];[T.FINAL_T];[T.SEALED_T];[T.NONSEALED_T];[T.STRICTFP_T]]);
   ("ClassPermits",La.direct [[T.PERMITS_T]] ); 
   ("Dims",La.first_trial_only for_Dims); 
   ("ImportDeclaration",La.direct [[T.IMPORT_T]]); 
   ("PackageDeclaration",La.direct [[T.PACKAGE_T]]);
   ("PackageModifier",La.direct [[T.SNAIL_T]]);
   (*
   ("Static",La.one_level_above_molecular [T.STATIC_T]); 
   *)
   ("TypeParameters",La.first_trial_only for_TypeParameters); 
  ] ;  
  choosers_for_disjunctions = Image.image (fun (str,l)->(Jvng_duplicated_name.of_string  str,l)) [
     "ClassModifier", (La.no_first_trial [
       [T.SNAIL_T],"Annotation"; 
       [T.PUBLIC_T],"Public";
       [T.PROTECTED_T],"Protected";
       [T.PRIVATE_T],"Private";
       [T.ABSTRACT_T],"Abstract";
       [T.STATIC_T],"Static"; 
       [T.FINAL_T],"Final";
       [T.SEALED_T],"Sealed";
       [T.NONSEALED_T],"Nonsealed";
       [T.STRICTFP_T],"Strictfp";
     ] true);
     "FieldModifier", (La.no_first_trial [
       [T.SNAIL_T],"Annotation"; 
       [T.PUBLIC_T],"Public";
       [T.PROTECTED_T],"Protected";
       [T.PRIVATE_T],"Private";
       [T.STATIC_T],"Static"; 
       [T.FINAL_T],"Final";
       [T.TRANSIENT_T],"Final";
       [T.VOLATILE_T],"Volatile";
     ] true);
     "TopLevelClassOrInterfaceDeclaration", La.first_trial_only for_TopLevelClassOrInterfaceDeclaration;
     "ClassDeclaration", La.first_trial_only for_ClassDeclaration;
     "ClassBodyDeclaration", La.make for_ClassBodyDeclaration [
       [T.PUBLIC_T;T.STATIC_T;T.FINAL_T;T.IDENTIFIER_T;T.IDENTIFIER_T;T.EQ_T],"FieldDeclaration";
     ] false;
     "UnannType", La.first_trial_only for_UnannType;
  ] ;
  precomputed_first_tokens = list_for_precomputed_first_tokens;
} ;;

end ;;  

let example = Private.example ;;