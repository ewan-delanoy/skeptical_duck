(*

#use"lib/supstring.ml";;

*)

   
 
let contains y x=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      Int_range.exists tester 0 (String.length(y)-lx);;               