(*

#use"lib/list_with_indices.ml";;

*)

exception Bad_set_of_indices;;

let list_with_indices l=
  let n=List.length l in
  let temp1=Int_range.scale (fun i->List.find_opt(fun p->fst(p)=i) l) 1 n in
  if List.mem None temp1
  then raise(Bad_set_of_indices)
  else
  Int_range.scale (fun
     i->snd(List.find(fun p->fst(p)=i) l)
  ) 1 n;;

(*

list_with_indices [3,"a";1,"b";2,"c"];;

*)  
           