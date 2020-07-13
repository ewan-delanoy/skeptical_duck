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

let replace_inside_file cr fn =
    let s1=Io.read_whole_file fn in
    let s2=replace_inside_string cr s1  in
    Io.overwrite_with fn s2;; 
   
let execute s=
   let temp1 = Str.split (Str.regexp "[ \t]+") s in 
   let temp2 = Image.image Absolute_path.of_string  temp1 in 
   let replacements = Io.read_whole_file (List.nth temp2 0) 
   and recipient = (List.nth temp2 1) in 
   replace_inside_file 
     (parse replacements) recipient ;;

(*

let z1 =  Compact_replacer_t.CR(["abc","def";"12","34"]) ;;  
let z2 = unparse z1;;
let z3 = parse z2;;
let check = (z3=z1);;

*)