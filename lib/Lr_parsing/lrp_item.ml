(*

Lrp is short for "LR Parsing"

#use"lib/Lr_parsing/lrp_item.ml";;

*)

open Lrp_types ;;

module Private = struct 

let production_from_item (Item(p,l)) = Prod(p,List.filter(fun x->x<>".") l) ;;   

let index_of_dot (Item(_p,l))= List_again.index_of_in "." l;;



let push_dot_one_symbol symb (Item(p,l))=
  let n = List.length l 
  and j = List_again.index_of_in "." l in
  if j=n then None else 
  if (List.nth l j)<>symb 
  then None 
  else
  let (head,tail)=List_again.long_head_with_tail (j-1) l in 
  Some(Item(p,(List.rev head)@(symb::"."::(List.tl(List.tl tail)))))
  ;;

(*

push_dot_one_symbol "1" (Item("a",[".";"1";"2";"3";"4";"5";"6"])) ;;
push_dot_one_symbol "3" (Item("a",["1";"2";".";"3";"4";"5";"6"])) ;;
push_dot_one_symbol "6" (Item("a",["1";"2";"3";"4";"5";".";"6"])) ;;

*)  

let description_for_item (Item(p,l)) = p ^ " -> " ^ (String.concat "" l) ;;

let description_for_items items=
    String.concat "\n" (Image.image (fun item->(String.make 3 ' ')^(description_for_item item)) items) ;;

let description_for_indexed_item_set (idx,items)=
 (string_of_int idx)^": \n\n"^(description_for_items items) ;;
 
 let description_for_indexed_item_sets l = 
   "\n\n\n"^(String.concat "\n\n\n" (Image.image description_for_indexed_item_set l)) ;;
   

end ;;  

let almost_finished_production_opt (Item(p,l)) =
  match List.rev l with 
  [] -> None 
  |head_of_rev::tail_of_rev ->
    if head_of_rev <> "."
    then None  
    else Some(Prod(p,List.rev tail_of_rev)) ;; 
   
let display_indexed_item_sets l = 
  print_string(Private.description_for_indexed_item_sets l);;

let first_item_from_production (Prod(p,l)) = Item(p,"."::l);;

let index_of_dot = Private.index_of_dot ;;   

let items_from_production (Prod(p,l)) =
  let temp = Two_winged_bird_on_plank.generic l in 
  let temp2 = List.rev_map (fun (left,center,right)->(List.rev left,center::right)) temp in 
  (Image.image (fun (left,right)->Item(p,left@("."::right))) temp2)@[Item(p,l@["."])] ;; 

(* items_from_production (Prod("x",["1";"2";"3";"4"])) ;; *) 

let last_item_from_production (Prod(p,l)) = Item(p,l@["."]);;

let production_from_item = Private.production_from_item ;;
let push_dot_one_symbol = Private.push_dot_one_symbol ;;




let symbol_after_dot_opt (Item(_,l)) =
    let (_,_,right) = 
      Option.get(List_again.find_and_remember_opt (fun s->s=".") l) in 
      if right=[]
      then None  
      else Some(List.hd right) ;;

(*


symbol_after_dot (Item("a",["1";"2";".";"3";"4";"5";"6"])) ;;


*)  
