(*

#use "Text_editing/Text_lengthening/dtu_modify.ml";;

*)

module Private = struct

let recompute_at_index dtu txl accu k=
      let inc=dtu.Double_tunnel_t.incoming 
      and outc=dtu.Double_tunnel_t.outcoming in 
      let old_x = Option.unpack (Array.get inc k)
      and old_y = Option.unpack (Array.get outc k) in 
      let new_y = Txl_apply.apply txl old_x in 
      if new_y<>old_y
      then  
           accu:=(old_y,new_y)::(!accu);
           Array.set outc k (Some new_y)
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
    let accu=ref [] in
    let _=(for k=1 to c do 
        Private.recompute_at_index dtu txl accu k
    done) in 
    List.rev(!accu);;


    