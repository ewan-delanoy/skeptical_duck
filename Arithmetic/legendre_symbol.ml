(*

#use"Arithmetic/legendre_symbol.ml";;

*)

module Private = struct


let curried_product x y=x*y;;

let fold_product=function
[]->1
|a::b->List.fold_left(curried_product)(a)(b);;


let square_free_part n=
let rec tempf=(
function 
(m,accu)->
if m=1 then accu else
let q=Primes.smallest_prime_divisor(m) in 
let m2=(m/q) in
if (m2 mod q)=0
then tempf(m2/q,accu)
else tempf(m2,q*accu)
) in
if n<0 then tempf(-n,-1) else
if n>0 then tempf(n,1) else 
failwith("square-free part of zero undefined");;


let lsm1 p=
if (p mod 4)=1
then 1
else (-1);;

let ls2 p=
if Set_of_integers.mem(p mod 8)(Set_of_integers.safe_set[1;7])
then 1
else (-1);;

let reciprocity_coefficient p q=
if ((p mod 4)=1)|| ((q mod 4)=1) then 1 else (-1);;

let rec naive_legendre_symbol0 accu=
function
[]->accu
|(a,p)::peurrest->(*we take p>a>0, p odd *)
if (a mod p)=0 then 0 else
if a=1 then naive_legendre_symbol0(accu)(peurrest) else
if (a mod 2)=0 then naive_legendre_symbol0(ls2(p)*accu)(((a/2),p)::peurrest) else
let q=Primes.smallest_prime_divisor(a) in
let epsilon=reciprocity_coefficient(p)(q) in
naive_legendre_symbol0(epsilon*accu)(((p mod q),q)::(a/q,p)::peurrest);;

let naive_legendre_symbol a p=
if (a mod p)=0 then 0 else
if a<0
then lsm1(p)*naive_legendre_symbol0(1)([(-a) mod p,p])
else naive_legendre_symbol0(1)([a mod p,p]);;

let phi_set nn=
let n=abs(nn) in
let temp1=Primes.factor(n) in
let temp2=Multiset.to_list(temp1) in
let temp3=Ennig.ennig(1)(n-1) in
List.filter(function x->List.filter(function p->(x mod p)=0)(temp2)=[])(temp3);;

let find_one_prime_equivalent m a=
let rec tempf=(function
accu->if Primes.is_prime(accu) then (a,accu) else tempf(accu+m))
in if a<3 then tempf(a+m) else tempf(a);;

let find_many_prime_equivalents m l=
Image.image(find_one_prime_equivalent(m))(l);;

let good_moduli a=
let aa=abs(a) and ea=Basic.sign(a) in
let temp1=Primes.factor(aa) in
let temp2=Multiset.filter_odd_multiplicities(temp1) in
let ka=ea*fold_product(temp2) in
let big_a=4*abs(ka) in
let temp3=Ennig.ennig(1)(big_a-1) in
let temp4=List.filter(function x->List.filter(function p->(x mod p)=0)(2::temp2)=[])(temp3) in
let temp5=find_many_prime_equivalents(big_a)(temp4) in
let temp6=List.filter(function (z,pz)->naive_legendre_symbol(a)(pz)=1)(temp5) in
(big_a,temp6);;

let gcd_for_three a b c=Gcd.gcd(a)(Gcd.gcd(b)(c));;

let isqrt n=
if n<0 then failwith("negative integers not allowed") else
if n<2 then n else 
let rec tempf=
(function
(j2,j)->if (j2>n) then (j-1) else
tempf(j2+2*j+1,j+1)
) in
tempf(1,1);;

let enumerate_reduced_forms dd=
let r=(dd mod 4) in
if ((r=1)||(r=2)) then ([],[]) else
let temp1=Ennig.ennig(0)(isqrt(dd/3)) in
let temp2=List.filter(function x->(x mod 2)=r)(temp1) in
let temp3=List.flatten(Image.image(function b->let m=(b*b+dd)/4 in
Image.image(function a->(a,b,m/a) )(Set_of_integers.forget_order(Primes.divisors(m))) )(temp2)) in
let temp4=List.filter(function (a,b,c)->(b<=a)&&(a<=c)&&(gcd_for_three(a)(b)(c)=1 ) )(temp3) in
let temp5=List.filter(function (a,b,c)->(0<b)&&(b<a)&&(a<c) )(temp4) in
(temp4,Image.image(function (a,b,c)->(a,-b,c))(temp5));;

let number_of_reduced_forms dd=
let temp=enumerate_reduced_forms(dd) in
List.length(fst(temp))+List.length(snd(temp));;

end ;;

let legendre_symbol = Private.naive_legendre_symbol ;;

