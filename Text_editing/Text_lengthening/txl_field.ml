(*

#use "Text_editing/Text_lengthening/txl_field.ml";;

The arguments for setters are assumed to be already in order. 

*)

let set_decompressions txl v=
  {txl with 
    Text_lengthener_t.adjustable_decompressions =v ;
    Text_lengthener_t.case_insensitive_adjustable_decompressions = 
       French_capitalization.generalize_for_false_three v ;
  };;

let set_expansions txl v=
  {txl with 
    Text_lengthener_t.expansions =v ;
  };;

let set_inert_words txl v=
  {txl with 
    Text_lengthener_t.inert_words =v ;
    Text_lengthener_t.case_insensitive_inert_words = 
       French_capitalization.generalize_for_one v ;
  };;

let set_left_core_abbreviations txl v=
  {txl with 
    Text_lengthener_t.left_core_abbreviations =v ;
    Text_lengthener_t.case_insensitive_left_core_abbreviations = 
       French_capitalization.generalize_for_two v ;
  };;

let set_prefix_abbreviations txl v=
  {txl with 
    Text_lengthener_t.prefix_abbreviations =v ;
    Text_lengthener_t.case_insensitive_prefix_abbreviations = 
       French_capitalization.generalize_for_two v ;
  };;

(* Usual getters *)

let adjustable_decompressions txl=txl.Text_lengthener_t.case_insensitive_adjustable_decompressions;;
let expansions txl=txl.Text_lengthener_t.expansions;;
let inert_words txl=txl.Text_lengthener_t.case_insensitive_inert_words;;
let left_core_abbreviations txl=txl.Text_lengthener_t.case_insensitive_left_core_abbreviations;;
let prefix_abbreviations txl=txl.Text_lengthener_t.case_insensitive_prefix_abbreviations;;

(* Getters for a more internal use *)

let uncapitalized_adjustable_decompressions txl=txl.Text_lengthener_t.adjustable_decompressions;;
let uncapitalized_inert_words txl=txl.Text_lengthener_t.inert_words;;
let uncapitalized_left_core_abbreviations txl=txl.Text_lengthener_t.left_core_abbreviations;;
let uncapitalized_prefix_abbreviations txl=txl.Text_lengthener_t.prefix_abbreviations;;

(* Archiving and unarchiving *)
      



module Private = struct 


let decompression_of_concrete_object crobj=
  let (arg1,arg2,arg3,_,_,_,_)=Concrete_object_field.unwrap_bounded_uple crobj 
  and us=Concrete_object_field.unwrap_string in
  (us arg1,us arg2,Concrete_object_field.to_string_triple_list arg3);;

let decompression_to_concrete_object (s1,s2,l)=
  Concrete_object_t.Uple [
    Concrete_object_field.wrap_string(s1);
    Concrete_object_field.wrap_string(s2);
    Concrete_object_field.of_string_triple_list l];;

let declist_of_concrete_object crobj =
  Image.imagination decompression_of_concrete_object (Concrete_object_field.unwrap_list crobj);;

let declist_to_concrete_object l = 
  Concrete_object_t.List(Image.imagination decompression_to_concrete_object l);; 

let salt = "Text_"^"lengthener_t.";;

let adj_decs_label           = salt ^ "adjustable_decompressions";;
let expansions_label         = salt ^ "expansions";;
let inert_words_label        = salt ^ "inert_words";;
let left_abbrevs_label       = salt ^ "left_core_abbreviations";;
let prefix_abbrevs_label     = salt ^ "prefix_abbreviations";;

let ci_adj_decs_label        = salt ^ "case_insensitive_adjustable_decompressions";;
let ci_expansions_label      = salt ^ "case_insensitive_expansions";;
let ci_inert_words_label     = salt ^ "case_insensitive_inert_words";;
let ci_left_abbrevs_label    = salt ^ "case_insensitive_left_core_abbreviations";;
let ci_prefix_abbrevs_label  = salt ^ "case_insensitive_prefix_abbreviations";;

let of_concrete_object crobj = 
   let g=Concrete_object_field.get_record crobj in
   {
      Text_lengthener_t.adjustable_decompressions=declist_of_concrete_object(g adj_decs_label);
      expansions=Concrete_object_field.to_string_list_list(g expansions_label);
      inert_words=Concrete_object_field.to_string_list(g inert_words_label);
      left_core_abbreviations=Concrete_object_field.to_string_pair_list(g left_abbrevs_label);
      prefix_abbreviations=Concrete_object_field.to_string_pair_list(g prefix_abbrevs_label);
      case_insensitive_adjustable_decompressions=declist_of_concrete_object(g ci_adj_decs_label);
      case_insensitive_inert_words=Concrete_object_field.to_string_list(g ci_inert_words_label);
      case_insensitive_left_core_abbreviations=Concrete_object_field.to_string_pair_list(g ci_left_abbrevs_label);
      case_insensitive_prefix_abbreviations=Concrete_object_field.to_string_pair_list(g prefix_abbrevs_label);
   };;      

let to_concrete_object txl=
   let items= 
   [
      adj_decs_label,   declist_to_concrete_object(txl.Text_lengthener_t.adjustable_decompressions); 
      expansions_label, Concrete_object_field.of_string_list_list(txl.Text_lengthener_t.expansions); 
      inert_words_label, Concrete_object_field.of_string_list(txl.Text_lengthener_t.inert_words); 
      left_abbrevs_label, Concrete_object_field.of_string_pair_list(txl.Text_lengthener_t.left_core_abbreviations); 
      prefix_abbrevs_label, Concrete_object_field.of_string_pair_list(txl.Text_lengthener_t.prefix_abbreviations); 

      ci_adj_decs_label,   declist_to_concrete_object(txl.Text_lengthener_t.case_insensitive_adjustable_decompressions); 
      ci_inert_words_label, Concrete_object_field.of_string_list(txl.Text_lengthener_t.case_insensitive_inert_words); 
      ci_left_abbrevs_label, Concrete_object_field.of_string_pair_list(txl.Text_lengthener_t.case_insensitive_left_core_abbreviations); 
      ci_prefix_abbrevs_label, Concrete_object_field.of_string_pair_list(txl.Text_lengthener_t.case_insensitive_prefix_abbreviations); 

   ]  in
   Concrete_object_t.Record items;;


end ;; 


let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;
