(*

#use"supstring.ml";;

*)



let begins_with y x=
      let lx=String.length(x) in
      if String.length(y)<lx
      then false
      else (String.sub y 0 lx)=x;;  
   
 let ends_with y x=
      let lx=String.length(x) in
      if String.length(y)<lx
      then false
      else (String.sub y ((String.length y)-lx) lx)=x;;  
   
 
let contains y x=
      let lx=String.length(x) in
      let tester=(function j->(String.sub y j lx)=x) in
      Ennig.exists tester 0 (String.length(y)-lx);;               