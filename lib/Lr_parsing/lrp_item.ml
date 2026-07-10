(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_item.ml";;

*)

open Lrp_types ;;

module Private = struct 

let production_from_item (Item(p,l)) = Prod(p,List.filter(fun x->x<>".") l) ;;   

let index_of_dot (Item(_p,l))= List_again.index_of_in "." l;;

let index_of_production_in_grammar item (BG productions) = 
  List_again.index_of_in (production_from_item item) productions ;;

let order gram = ((fun item1 item2 ->
  let trial1 = Total_ordering.for_integers (index_of_dot item1) (index_of_dot item2) in 
  if trial1<> Total_ordering_result_t.Equal then trial1 else 
  Total_ordering.for_integers 
    (index_of_production_in_grammar item1 gram) 
      (index_of_production_in_grammar item2 gram)  
): item Total_ordering_t.t);;

end ;;  

let first_item_from_production (Prod(p,l)) = Item(p,"."::l);;

let items_from_production (Prod(p,l)) =
  let temp = Three_parts.generic l in 
  let temp2 = List.rev_map (fun (left,center,right)->(List.rev left,center::right)) temp in 
  (Image.image (fun (left,right)->Item(p,left@("."::right))) temp2)@[Item(p,l@["."])] ;; 

(* items_from_production (Prod("x",["1";"2";"3";"4"])) ;; *) 


let fold_merge gram = Ordered.fold_merge (Private.order gram) ;;
let merge gram = Ordered.merge (Private.order gram);;
let setminus gram = Ordered.setminus (Private.order gram) ;;
let sort gram = Ordered.sort (Private.order gram);;

