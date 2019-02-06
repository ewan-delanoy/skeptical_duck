(*

#use "Text_editing/Text_lengthening/txl_modify.ml";;

*)

exception Decompression_conflict of string*string*string;;
exception Unknown_decompresser of string;;
exception Unknown_decompression of string*string;;


module Private = struct

let rec recursive_helper_for_i_adjustable_decompression
  (u,v,treated,untreated) = match untreated with 
  []->(List.rev treated,None,[])
  |head::other_ones->
     let (u1,v1,adjustments)=head in 
     (
      match French_order.cmp u1 u with 
       Total_ordering.Lower -> 
         recursive_helper_for_i_adjustable_decompression(u,v,head::treated,untreated)
       |Total_ordering.Equal ->
         (List.rev treated,Some(head),other_ones)  
       |Total_ordering.Greater -> 
         (List.rev treated,None,untreated)   
     );;

let  helper_for_i_adjustable_decompression (u,v) l=
   recursive_helper_for_i_adjustable_decompression (u,v,[],l);;

end;;

(* Reorder everything from scratch ; should be rarely needed *)

let order_fix txl=
  let old_adecs=Txl_field.uncapitalized_adjustable_decompressions txl 
  and old_expios=Txl_field.expansions txl 
  and old_iwords=Txl_field.uncapitalized_inert_words  txl 
  and old_lc_abbs=Txl_field.uncapitalized_left_core_abbreviations  txl 
  and old_px_abbs=Txl_field.uncapitalized_prefix_abbreviations  txl 
  and old_ic_adecs=Txl_field.adjustable_decompressions txl 
  and old_ic_iwords=Txl_field.inert_words  txl 
  and old_ic_lc_abbs=Txl_field.left_core_abbreviations  txl 
  and old_ic_px_abbs=Txl_field.prefix_abbreviations  txl in
  let adecs=Txl_ordering.order_decompressions old_adecs 
  and expios=Txl_ordering.order_expansions old_expios 
  and iwords=Txl_ordering.order_inert_words old_iwords 
  and lc_abbs=Txl_ordering.order_abbreviations old_lc_abbs 
  and px_abbs=Txl_ordering.order_abbreviations old_px_abbs 
  and ic_adecs=Txl_ordering.order_decompressions old_ic_adecs 
  and ic_iwords=Txl_ordering.order_inert_words  old_ic_iwords 
  and ic_lc_abbs=Txl_ordering.order_abbreviations  old_ic_lc_abbs 
  and ic_px_abbs=Txl_ordering.order_abbreviations  old_ic_px_abbs in 
  {
    Text_lengthener_t.adjustable_decompressions = adecs;
    expansions = expios;
    inert_words = iwords;
    left_core_abbreviations = lc_abbs;
    prefix_abbreviations = px_abbs ;
    case_insensitive_adjustable_decompressions = ic_adecs ;
    case_insensitive_left_core_abbreviations = ic_lc_abbs ;
    case_insensitive_inert_words = ic_iwords ;
    case_insensitive_prefix_abbreviations = ic_px_abbs ;
  }   ;;


(* Insert *)

let i_decompression txl (u,v)=
    let decs = Txl_field.adjustable_decompressions txl in 
    let (before_point,point,after_point) = 
      Private.helper_for_i_adjustable_decompression (u,v) decs in 
    match point with
    None -> let new_decs = before_point @ ((u,v,[]):: after_point) in 
            Txl_field.set_decompressions txl new_decs
    | Some(_,v1,_) ->
           if v1=v 
           then txl 
           else raise(Decompression_conflict(u,v1,v));;



let i_adjustment txl (u,v,(ad1,ad2,ad3))=
  let decs = Txl_field.adjustable_decompressions txl in 
    let (before_point,point,after_point) = 
      Private.helper_for_i_adjustable_decompression (u,v) decs in 
    match point with
    None -> raise(Unknown_decompresser(u))
    | Some(_,v1,adjustments) ->
           if v1<>v 
           then raise(Unknown_decompression(u,v))
           else let new_adjustments=
              Txl_ordering.insert_adjustment (ad1,ad2,ad3) adjustments in 
              let new_decs = before_point @ ((u,v,new_adjustments):: after_point) in 
              Txl_field.set_decompressions txl new_decs;;  

let i_expansion txl expansion =
   let exps = Txl_field.expansions txl in
   let new_exps =  Txl_ordering.insert_expansion expansion exps in 
   Txl_field.set_expansions txl new_exps;;

let i_inert_word txl word =
   let iwds = Txl_field.inert_words txl in
   let new_iwds =  Txl_ordering.insert_inert_word word iwds in 
   Txl_field.set_inert_words txl new_iwds;;

let i_left_core_abbreviation txl abbrv =
   let abbrvs = Txl_field.left_core_abbreviations txl in
   let new_abbrvs =  Txl_ordering.insert_abbreviation abbrv abbrvs in 
   Txl_field.set_left_core_abbreviations txl new_abbrvs;;

let i_prefix_abbreviation txl abbrv =
   let abbrvs = Txl_field.prefix_abbreviations txl in
   let new_abbrvs =  Txl_ordering.insert_abbreviation abbrv abbrvs in 
   Txl_field.set_prefix_abbreviations txl new_abbrvs;;   

(* Remove *)

let r_decompression txl (u,v)=
    let decs = Txl_field.adjustable_decompressions txl in 
    let (before_point,point,after_point) = 
      Private.helper_for_i_adjustable_decompression (u,v) decs in 
    match point with
    None -> txl
    | Some(_,v1,_) ->
           if v1<>v 
           then txl 
           else let new_decs = before_point @ after_point in 
                Txl_field.set_decompressions txl new_decs;;


let r_adjustment txl (u,v,(ad1,ad2,ad3))=
  let decs = Txl_field.adjustable_decompressions txl in 
    let (before_point,point,after_point) = 
      Private.helper_for_i_adjustable_decompression (u,v) decs in 
    match point with
    None -> txl
    | Some(_,v1,adjustments) ->
           if v1<>v 
           then txl
           else 
              let ad0=(ad1,ad2,ad3) in 
              let new_adjustments=List.filter (fun ad->ad<>ad0) adjustments in 
              let new_decs = before_point @ ((u,v,new_adjustments):: after_point) in 
              Txl_field.set_decompressions txl new_decs;;  

let r_expansion txl expansion =
   let exps = Txl_field.expansions txl in
   let new_exps =  List.filter(fun expansion2-> expansion2 <> expansion ) exps in 
   Txl_field.set_expansions txl new_exps;;

let r_inert_word txl word =
   let iwds = Txl_field.inert_words txl in
   let new_iwds = List.filter(fun word2-> word2 <> word ) iwds in 
   Txl_field.set_inert_words txl new_iwds;;

let r_left_core_abbreviation txl abbrv =
   let abbrvs = Txl_field.left_core_abbreviations txl in
   let new_abbrvs =  List.filter(fun abbrv2-> abbrv2 <> abbrv ) abbrvs in 
   Txl_field.set_left_core_abbreviations txl new_abbrvs;;

let r_prefix_abbreviation txl abbrv =
   let abbrvs = Txl_field.prefix_abbreviations txl in
   let new_abbrvs =  List.filter(fun abbrv2-> abbrv2 <> abbrv ) abbrvs in 
   Txl_field.set_prefix_abbreviations txl new_abbrvs;;   



