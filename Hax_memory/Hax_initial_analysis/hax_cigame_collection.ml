(* 

#use"Hex_memory/hex_pgame_collection.ml";;

First coordinate is column index, second is row index

*)

let cmp=((fun 
 (Hax_cigame_collection_t.L(l1)) (Hax_cigame_collection_t.L(l2)) ->
   (Total_ordering.lex_compare 
       Hax_checked_initial_game.cmp l1 l2) :> 
       Hax_cigame_collection_t.t Total_ordering.t) );;

let empty_one = Hax_cigame_collection_t.L [];;
let singleton cell = Hax_cigame_collection_t.L [Hax_checked_initial_game.singleton cell];;

let is_foreseen_in pgame (Hax_cigame_collection_t.L(l))=
   List.exists (fun pgame2->Hax_checked_initial_game.starts_with pgame2 pgame) l;;


let joiner = " \n ";;

let of_string uncorrected_s =
  let s = Cull_string.cobeginning 2 uncorrected_s in 
  let temp1=Str.split (Str.regexp_string joiner) s in 
  let temp2=Image.image Hax_checked_initial_game.of_string temp1 in 
  Hax_cigame_collection_t.L(
     Ordered.diforchan_plaen Hax_checked_initial_game.cmp temp2
  );;

let to_string (Hax_cigame_collection_t.L(l))=
  "\n "^(String.concat joiner (Image.image Hax_checked_initial_game.to_string l));;

let insert_in pgame (Hax_cigame_collection_t.L(l))=
  let temp1=List.filter (fun pgame2->not(Hax_checked_initial_game.starts_with pgame pgame2)) l in 
  let new_l=Ordered.insert_plaen Hax_checked_initial_game.cmp pgame temp1 in 
   Hax_cigame_collection_t.L new_l;;

let cut_by (Hax_cigame_collection_t.L(l)) pgame=
  let new_l=Option.filter_and_unpack(fun pgame2->Hax_checked_initial_game.cut_by pgame2 pgame) l in 
  Hax_cigame_collection_t.L(new_l);; 


let classify_according_to_depth (Hax_cigame_collection_t.L(l)) = 
   let temp1=Image.image (fun pgame->(Hax_checked_initial_game.depth pgame,Hax_checked_initial_game.first_move pgame)) l in
   let temp2=Prepared.partition snd temp1 in 
   let temp3=Image.image (fun l->let mov=snd(List.hd l) and d=snd(Max.maximize_it fst l) in (d,mov)) temp2 in 
   let temp4=Tidel2.diforchan temp3 in 
   Ordered.forget_order temp4;;




  

let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;     