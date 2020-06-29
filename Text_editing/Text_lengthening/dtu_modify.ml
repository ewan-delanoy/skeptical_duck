(*

#use "Text_editing/Text_lengthening/dtu_modify.ml";;

*)

module Private = struct



let make inc outc=
    {
       Double_tunnel_t.size = List.length inc;
       incoming = inc;
       outcoming = outc;
    };;

let unmake dtu=
   (dtu.Double_tunnel_t.incoming,
    dtu.Double_tunnel_t.outcoming);;

end;;

let add_pair dtu (word1,word2)=
   let (inc,outc)=Private.unmake dtu in
   Private.make(word1::inc) (word2::outc);;
 
let remove_last dtu (word1,word2)=
   let (inc,outc)=Private.unmake dtu in
   Private.make(List.tl inc) (List.tl outc);;

let recompute dtu txl =
    let pairs=Dtu_construct.deconstruct dtu in 
    let accu=ref [] in
    let tempf=(
        fun (x,old_y)->
          let new_y=Txl_apply.apply txl x in 
          if new_y=old_y
          then (x,old_y)
          else let _=(accu:=(x,old_y,new_y)::(!accu)) in 
               (x,new_y)
    ) in
    let new_pairs = Image.image tempf pairs in 
    (Dtu_construct.construct new_pairs,
      Unexpected_change_after_update_t.Ucau(List.rev(!accu)));;


