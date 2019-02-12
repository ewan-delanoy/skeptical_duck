(*

#use "Text_editing/Text_lengthening/dtu_construct.ml";;

*)

let construct l=
  {
   Double_tunnel_t.size = (List.length l) ; (* redundant but convenient *)
   incoming = List.rev_map fst l ;
   outcoming = List.rev_map snd l ; 
  };;

let deconstruct dtu =
  let incoming_part=dtu.Double_tunnel_t.incoming 
  and outcoming_part=dtu.Double_tunnel_t.outcoming  in 
  List.rev(List.combine incoming_part outcoming_part);;

(*

let base = [("ab","cde");("fg","hij")];;
let example = construct base;;
let base2= deconstruct example;;
let check = (base2 = base);;

*)