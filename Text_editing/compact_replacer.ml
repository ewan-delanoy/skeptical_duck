(*

#use"Text_editing/compact_replacer.ml";;

*)


let separator = " \205\140 ";;

let unparse (Compact_replacer_t.CR(l))=
   let temp1 = List.flatten (Image.image (fun (x,y)->[x;y]) l)  in 
   String.concat separator temp1 ;;

let parse descr =
   let temp1 = Str.split (Str.regexp_string separator) descr in 
   let m = (List.length temp1)/2 in
   let tg =(fun k->List.nth temp1 (k-1)) in  
   Compact_replacer_t.CR(Ennig.doyle (fun j->(tg (2*j-1),tg (2*j)) ) 1 m );;

let replace_inside_string (Compact_replacer_t.CR(l)) old_text =
   Replace_inside.replace_several_inside_string l old_text ;;

let replace_inside_file (Compact_replacer_t.CR(l)) fn =
   Replace_inside.replace_several_inside_file l fn ;;
   
(*

let z1 =  Compact_replacer_t.CR(["abc","def";"12","34"]) ;;  
let z2 = unparse z1;;
let z3 = parse z2;;
let check = (z3=z1);;

*)