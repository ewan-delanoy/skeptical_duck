(************************************************************************************************************************
Snippet 46 : 
************************************************************************************************************************)
open Needed_values ;;


(************************************************************************************************************************
Snippet 45 : Test the 
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

