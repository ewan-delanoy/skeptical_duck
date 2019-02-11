(*

#use "Text_editing/Text_lengthening/dtu_modify.ml";;

*)

let add_pair dtu (word1,word2)=
   let c=(dtu.Double_tunnel_t.size)+1 in
   (
      Array.set dtu.incoming c (Some word1);
      Array.set dtu.outcoming c (Some(word2));
      dtu.Double_tunnel_t.size <- (c+1);
   );;
 
let remove_last dtu =
   let c=dtu.Double_tunnel_t.size in
   (
      Array.set dtu.incoming c None;
      Array.set dtu.outcoming c None;
      dtu.Double_tunnel_t.size <- (c-1);
   );;
  