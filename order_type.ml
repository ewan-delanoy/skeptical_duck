(*

#use"order_type.ml";;


*)

let order_type cmp l = 
  let ordered_l = Ordered.sort cmp l in 
  Image.image (fun x->
    Listennou.find_index x ordered_l 
  ) l;;

