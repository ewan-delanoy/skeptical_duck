(*

#use "Text_editing/Text_lengthening/dtu_modify.ml";;

*)

module Private = struct

let recompute_at_index dtu txl k=
      Array.set dtu.Double_tunnel_t.outcoming k
      (Some(Txl_apply.apply txl 
            (Option.unpack(Array.get dtu.Double_tunnel_t.incoming k)) ))
    ;;

end;;

let add_pair dtu (word1,word2)=
   let c=(dtu.Double_tunnel_t.size)+1 in
   (
      Array.set dtu.incoming c (Some word1);
      Array.set dtu.outcoming c (Some(word2));
      dtu.Double_tunnel_t.size <- c;
   );;
 
let remove_last dtu =
   let c=dtu.Double_tunnel_t.size in
   (
      Array.set dtu.incoming c None;
      Array.set dtu.outcoming c None;
      dtu.Double_tunnel_t.size <- (c-1);
   );;
  

let recompute dtu txl =
    let c=dtu.Double_tunnel_t.size in
    for k=1 to c do 
        Private.recompute_at_index dtu txl k
    done;;