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
      
let decompression=Industrial_separator.text_lengthener_decompression;;
let field=Industrial_separator.text_lengthener_field;;
let level_one_list=Industrial_separator.text_lengthener_level_one_list;;
let level_two_list=Industrial_separator.text_lengthener_level_two_list;;
let times=Industrial_separator.string_times_string;;

module Bitrate = struct

let archive_triple (s1,s2,s3)=s1^times^s2^times^s3;;
let unarchive_triple s=
   let temp1=Str.split_delim (Str.regexp_string times)  s in 
   let tempf=(fun k->List.nth temp1 (k-1)) in 
   (tempf 1,tempf 2,tempf 3);;

let archive_adjustments l=
   String.concat level_one_list (Image.image archive_triple l);;
let unarchive_adjustments s=
  let  temp1=Str.split_delim (Str.regexp_string level_one_list)  s in 
  Image.image unarchive_triple temp1;;

let archive_decompression (u,v,adjustments)=
   u^decompression^v^decompression^(archive_adjustments adjustments);;
let unarchive_decompression s=
   let temp1=Str.split_delim (Str.regexp_string decompression)  s in 
   let tempf=(fun k->List.nth temp1 (k-1)) in 
   (tempf 1,tempf 2,unarchive_adjustments (tempf 3));;

let archive_decompressions l=
  String.concat level_two_list (Image.image archive_decompression l);;
let unarchive_decompressions s=
  let  temp1=Str.split_delim (Str.regexp_string level_two_list)  s in 
  Image.image unarchive_decompression temp1;;



let archive_expansion l=String.concat level_one_list l;;
let unarchive_expansion s= Str.split_delim (Str.regexp_string level_one_list) s;;



let archive_expansions l=
  String.concat level_two_list (Image.image archive_expansion l);;
let unarchive_expansions s=
  let  temp1=Str.split_delim (Str.regexp_string level_two_list)  s in 
  Image.image unarchive_expansion temp1;;


let archive_inert_words=archive_expansion;;
let unarchive_inert_words=unarchive_expansion;;

let archive_cumulable_suffix (s1,s2)=s1^times^s2;;
let unarchive_cumulable_suffix s=
   let temp1=Str.split_delim (Str.regexp_string times)  s in 
   let tempf=(fun k->List.nth temp1 (k-1)) in 
   (tempf 1,tempf 2);;

let archive_cumulable_suffixes l=
  String.concat level_one_list (Image.image archive_cumulable_suffix l);;
let unarchive_cumulable_suffixes s=
  let  temp1=Str.split_delim (Str.regexp_string level_one_list)  s in 
  Image.image unarchive_cumulable_suffix temp1;;

let archive_prefix_abbreviations =  archive_cumulable_suffixes;;
let unarchive_prefix_abbreviations = unarchive_cumulable_suffixes;;

end;;


let archive txl=
        let decs=txl.Text_lengthener_t.adjustable_decompressions
        and exps=txl.Text_lengthener_t.expansions
        and iwds=txl.Text_lengthener_t.inert_words 
        and cusus=txl.Text_lengthener_t.left_core_abbreviations
        and prefs=txl.Text_lengthener_t.prefix_abbreviations
        and ci_decs=txl.Text_lengthener_t.case_insensitive_adjustable_decompressions
        and ci_iwds=txl.Text_lengthener_t.case_insensitive_inert_words 
        and ci_cusus=txl.Text_lengthener_t.case_insensitive_left_core_abbreviations 
        and ci_prefs=txl.Text_lengthener_t.case_insensitive_prefix_abbreviations
        in
        String.concat field
        [
          Bitrate.archive_decompressions decs ;
          Bitrate.archive_expansions exps ;
          Bitrate.archive_inert_words iwds ;
          Bitrate.archive_cumulable_suffixes cusus ;
          Bitrate.archive_prefix_abbreviations prefs ;
          Bitrate.archive_decompressions ci_decs ;
          Bitrate.archive_inert_words ci_iwds ;
          Bitrate.archive_cumulable_suffixes ci_cusus ;
          Bitrate.archive_prefix_abbreviations ci_prefs ;
        ];;
      
        
      
      
           
let unarchive s=
          let temp1=Str.split_delim (Str.regexp_string field) s in
          let tempf=(fun k->List.nth temp1 (k-1)) in 
          let decs = Bitrate.unarchive_decompressions (tempf 1) 
          and exps = Bitrate.unarchive_expansions (tempf 2) 
          and iwds = Bitrate.unarchive_inert_words (tempf 3) 
          and cusus = Bitrate.unarchive_cumulable_suffixes (tempf 4) 
          and prefs = Bitrate.unarchive_prefix_abbreviations (tempf 5) 
          and ci_decs = Bitrate.unarchive_decompressions (tempf 6) 
          and ci_iwds = Bitrate.unarchive_inert_words (tempf 7) 
          and ci_cusus = Bitrate.unarchive_cumulable_suffixes (tempf 8) 
          and ci_prefs = Bitrate.unarchive_prefix_abbreviations (tempf 9) 
          in 
          {
            Text_lengthener_t.adjustable_decompressions=decs;
            expansions=exps;
            inert_words=iwds;
            left_core_abbreviations=cusus;
            prefix_abbreviations=prefs;
            case_insensitive_adjustable_decompressions=ci_decs;
            case_insensitive_left_core_abbreviations=ci_cusus;
            case_insensitive_inert_words=ci_iwds;
            case_insensitive_prefix_abbreviations=ci_prefs;
        } ;; 





