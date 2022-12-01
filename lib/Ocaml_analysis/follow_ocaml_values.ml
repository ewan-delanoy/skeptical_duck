(*

#use"lib/Ocaml_analysis/follow_ocaml_values.ml";;

*)

module Private=struct

let order_for_string_pairs =
  ((Total_ordering.product 
  Total_ordering.lex_for_strings Total_ordering.standard): 
  (string*string) Total_ordering_t.t
  );;

let followed_values_from_items l=
    let temp1=Option.filter_and_unpack (
       fun x->
         match x.Ocaml_gsyntax_item.category with
         Ocaml_gsyntax_category.Value                                                                          
      | Ocaml_gsyntax_category.Type
      | Ocaml_gsyntax_category.Exception->
              Some(x.Ocaml_gsyntax_item.name,x.Ocaml_gsyntax_item.content)
      | Ocaml_gsyntax_category.Module_opener
      | Ocaml_gsyntax_category.Module_closer
      | Ocaml_gsyntax_category.Module_inclusion ->None
  ) l in
  let temp2=Ordered.sort order_for_string_pairs temp1 in
  Followed_ocaml_values.F(temp2);;

end;;




let local_delchacre (Followed_ocaml_values.F l1) (Followed_ocaml_values.F l2)=
  let (common,in_l1_only,in_l2_only)=Ordered.diff  Total_ordering.lex_for_strings 
    ([],[],[],l1,l2) in
  (
    Image.image fst in_l1_only, 
   Option.filter_and_unpack (fun
    (name,content1,content2)->
     if content1=content2 then None else Some(name)
    ) common,
    Image.image fst in_l2_only
  );;
  
let follow_values hm=Private.followed_values_from_items(
  Read_ocaml_file_without_expanding_inclusions.rofwei hm
);;





