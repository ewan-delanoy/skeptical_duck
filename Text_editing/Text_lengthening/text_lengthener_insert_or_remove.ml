(*

#use "Text_editing/Text_lengthening/text_lengthener_insert_or_remove.ml";;

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

let order_fix x=
  let old_adecs=Txl_field.uncapitalized_adjustable_decompressions x 
  and old_expios=Txl_field.expansions x 
  and old_iwords=Txl_field.uncapitalized_inert_words  x 
  and old_lc_abbs=Txl_field.uncapitalized_left_core_abbreviations  x 
  and old_px_abbs=Txl_field.uncapitalized_prefix_abbreviations  x 
  and old_ic_adecs=Txl_field.adjustable_decompressions x 
  and old_ic_iwords=Txl_field.inert_words  x 
  and old_ic_lc_abbs=Txl_field.left_core_abbreviations  x 
  and old_ic_px_abbs=Txl_field.prefix_abbreviations  x in
  let adecs=Ordering_for_text_lengthener.order_decompressions old_adecs 
  and expios=Ordering_for_text_lengthener.order_expansions old_expios 
  and iwords=Ordering_for_text_lengthener.order_inert_words old_iwords 
  and lc_abbs=Ordering_for_text_lengthener.order_abbreviations old_lc_abbs 
  and px_abbs=Ordering_for_text_lengthener.order_abbreviations old_px_abbs 
  and ic_adecs=Ordering_for_text_lengthener.order_decompressions old_ic_adecs 
  and ic_iwords=Ordering_for_text_lengthener.order_inert_words  old_ic_iwords 
  and ic_lc_abbs=Ordering_for_text_lengthener.order_abbreviations  old_ic_lc_abbs 
  and ic_px_abbs=Ordering_for_text_lengthener.order_abbreviations  old_ic_px_abbs in 
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

let i_decompression x (u,v)=
    let decs = Txl_field.adjustable_decompressions x in 
    let (before_point,point,after_point) = 
      Private.helper_for_i_adjustable_decompression (u,v) decs in 
    match point with
    None -> let new_decs = before_point @ ((u,v,[]):: after_point) in 
            Txl_field.set_decompressions x new_decs
    | Some(_,v1,_) ->
           if v1=v 
           then x 
           else raise(Decompression_conflict(u,v1,v));;



let i_adjustment x (u,v,(ad1,ad2,ad3))=
  let decs = Txl_field.adjustable_decompressions x in 
    let (before_point,point,after_point) = 
      Private.helper_for_i_adjustable_decompression (u,v) decs in 
    match point with
    None -> raise(Unknown_decompresser(u))
    | Some(_,v1,adjustments) ->
           if v1<>v 
           then raise(Unknown_decompression(u,v))
           else let new_adjustments=
              Ordering_for_text_lengthener.insert_adjustment (ad1,ad2,ad3) adjustments in 
              let new_decs = before_point @ ((u,v,new_adjustments):: after_point) in 
              Txl_field.set_decompressions x new_decs;;  

let i_expansion x expansion =
   let exps = Txl_field.expansions x in
   let new_exps =  Ordering_for_text_lengthener.insert_expansion expansion exps in 
   Txl_field.set_expansions x new_exps;;

let i_inert_word x word =
   let iwds = Txl_field.inert_words x in
   let new_iwds =  Ordering_for_text_lengthener.insert_inert_word word iwds in 
   Txl_field.set_inert_words x new_iwds;;

let i_left_core_abbreviation x abbrv =
   let abbrvs = Txl_field.left_core_abbreviations x in
   let new_abbrvs =  Ordering_for_text_lengthener.insert_abbreviation abbrv abbrvs in 
   Txl_field.set_left_core_abbreviations x new_abbrvs;;

let i_prefix_abbreviation x abbrv =
   let abbrvs = Txl_field.prefix_abbreviations x in
   let new_abbrvs =  Ordering_for_text_lengthener.insert_abbreviation abbrv abbrvs in 
   Txl_field.set_prefix_abbreviations x new_abbrvs;;   

(* Remove *)

let r_decompression x (u,v)=
    let decs = Txl_field.adjustable_decompressions x in 
    let (before_point,point,after_point) = 
      Private.helper_for_i_adjustable_decompression (u,v) decs in 
    match point with
    None -> x
    | Some(_,v1,_) ->
           if v1<>v 
           then x 
           else let new_decs = before_point @ after_point in 
                Txl_field.set_decompressions x new_decs;;


let r_adjustment x (u,v,(ad1,ad2,ad3))=
  let decs = Txl_field.adjustable_decompressions x in 
    let (before_point,point,after_point) = 
      Private.helper_for_i_adjustable_decompression (u,v) decs in 
    match point with
    None -> x
    | Some(_,v1,adjustments) ->
           if v1<>v 
           then x
           else 
              let ad0=(ad1,ad2,ad3) in 
              let new_adjustments=List.filter (fun ad->ad<>ad0) adjustments in 
              let new_decs = before_point @ ((u,v,new_adjustments):: after_point) in 
              Txl_field.set_decompressions x new_decs;;  

let r_expansion x expansion =
   let exps = Txl_field.expansions x in
   let new_exps =  List.filter(fun expansion2-> expansion2 <> expansion ) exps in 
   Txl_field.set_expansions x new_exps;;

let r_inert_word x word =
   let iwds = Txl_field.inert_words x in
   let new_iwds = List.filter(fun word2-> word2 <> word ) iwds in 
   Txl_field.set_inert_words x new_iwds;;

let r_left_core_abbreviation x abbrv =
   let abbrvs = Txl_field.left_core_abbreviations x in
   let new_abbrvs =  List.filter(fun abbrv2-> abbrv2 <> abbrv ) abbrvs in 
   Txl_field.set_left_core_abbreviations x new_abbrvs;;

let r_prefix_abbreviation x abbrv =
   let abbrvs = Txl_field.prefix_abbreviations x in
   let new_abbrvs =  List.filter(fun abbrv2-> abbrv2 <> abbrv ) abbrvs in 
   Txl_field.set_prefix_abbreviations x new_abbrvs;;   



