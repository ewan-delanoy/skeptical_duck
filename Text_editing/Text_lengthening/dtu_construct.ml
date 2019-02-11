(*

#use "Text_editing/Text_lengthening/dtu_construct.ml";;

*)

let max_size = 800000;;

let construct l=
  let n=List.length l in 
  let incoming_arr=Array.make (max_size+1) None 
  and outcoming_arr=Array.make (max_size+1) None in 
  let counter=ref(0) and feeder=ref(l) in  
  let _=(
     while (!feeder)<>[]  do
     let ((inc,outc),others)=Listennou.ht (!feeder) 
     and idx=(!counter)+1 in 
     Array.set incoming_arr idx (Some inc);
     Array.set outcoming_arr idx (Some outc);
     feeder:=others;
     counter:=idx;
     done 
  ) in 
  {
   Double_tunnel_t.size = n ; (* redundant but convenient *)
   incoming = incoming_arr ;
   outcoming = outcoming_arr ; 
  };;

let deconstruct dtu =
  let n=dtu.Double_tunnel_t.size 
  and incoming_arr=dtu.Double_tunnel_t.incoming 
  and outcoming_arr=dtu.Double_tunnel_t.outcoming  in 
  Ennig.doyle (
    fun idx->(Option.unpack(incoming_arr.(idx)),
              Option.unpack(outcoming_arr.(idx)))
  ) 1 n;;

(*

let base = [("ab","cde");("fg","hij")];;
let example = construct base;;
let base2= deconstruct example;;
let check = (base2 = base);;

*)