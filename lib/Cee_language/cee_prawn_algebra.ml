(*

#use"lib/Cee_language/cee_prawn_algebra.ml";;

*)

module Private = struct 

  let il_order = Total_ordering.lex_compare Total_ordering.for_integers;;
  let il_sort = Ordered.sort il_order ;;
let connected_components l =
     let temp1 = Arithmetic_list.decompose_into_connected_components l in 
     Image.image (fun (i,j)->Int_range.range i j) temp1 ;; 

end ;;

let of_wardrobe 
   (Cee_wardrobe_t.Wr l) =
      let prawns = Image.image
     (fun (_,Cee_shadow_t.Sh (_,Cee_prawn_t.P z)) -> z ) l
    in
   let prawns2 = Private.il_sort prawns in    
   let prawns3 = Ordered_misc.generated_algebra Total_ordering.for_integers prawns2 in 
   let prawns4 = List.flatten (Image.image Private.connected_components prawns3) in 
   let prawns5 = Private.il_sort prawns4 in 
   let prawns6 = Image.image (fun l->Cee_prawn_t.P l) prawns5 in 
   Cee_prawn_algebra_t.A(Int_range.index_everything prawns6) ;;


let decompose_shadow 
  (Cee_prawn_algebra_t.A l_alg) (Cee_shadow_t.Sh(_,Cee_prawn_t.P l_sha))= 
  let present_prawns = List.filter_map (
     fun (prawn_idx,Cee_prawn_t.P x) ->
      (* here we assume that the algebra has been computed correctly*)
      if List.mem (List.hd x) l_sha
      then Some prawn_idx  
    else None
  ) l_alg in 
  (present_prawns,List.length l_alg) ;; 
  