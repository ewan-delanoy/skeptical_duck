(*

#use"Arithmetic/gcd.ml";;

*)

module Private = struct 

let rec helper_for_complete_gcd 
(aa,xa,ya,bb,xb,yb)=
(* we suppose 0<aa<bb and aa=xa*a+ya*b, bb=xb*a+yb*b *)
let q=(bb/aa) and r=(bb mod aa) in
if r=0 
then ( ((1-q)*xa)+xb,((1-q)*ya)+yb,aa)
else helper_for_complete_gcd(r,xb-(q*xa),yb-(q*ya),aa,xa,ya);;

end ;; 

let complete_gcd a b=
if a=0 then if b=0 then (1,1,0) else (0,Basic.sign(b),abs(b)) else
if b=0 then (Basic.sign(a),0,abs(a)) else
(*now a and b are nonzero*)
let aa=abs(a) and bb=abs(b) in
if aa=bb then (Basic.sign(a),0,abs(a)) else
if aa<bb
then Private.helper_for_complete_gcd(aa,Basic.sign(a),0,bb,0,Basic.sign(b))
else Private.helper_for_complete_gcd(bb,0,Basic.sign(b),aa,Basic.sign(a),0);;


let find_bezout_relation a b=(function (e1,e2,e3)->(e1,e2))(complete_gcd(a)(b));;

let gcd a b=(function (e1,e2,e3)->e3)(complete_gcd(a)(b));;

let gcd_for_many=function
[]->0
|a::b->List.fold_left gcd a b;;

let lcm a b=(a*b)/(gcd a b);;

let lcm_for_many=function
[]->1
|a::b->List.fold_left lcm a b;;

