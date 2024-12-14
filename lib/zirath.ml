(*

#use"lib/zirath.ml";;

A simplified version of the Zarith library.

*)

module Private2 = struct 

module Z = struct 

type t = I of int ;;

let of_int k = I k ;;
let of_string s = I(int_of_string s) ;;
let to_string (I k) = string_of_int k ;;

let zero = I 0 ;;
let one = I 1 ;;

let opposite (I x) = (I(-x)) ;;
let add (I x) (I y) = (I(x+y)) ;;
let mul (I x) (I y) = (I(x*y)) ;;
let sub (I x) (I y) = (I(x-y)) ;;
let gcd (I x) (I y) = (I(Gcd.gcd x y)) ;;

let div (I x) (I y) = 
  if x<0 
  then I(-(-x/y)) 
  else I(x/y);;

let equals (I x) (I y) = (x=y) ;; 
let leq (I x) (I y) = (x<=y) ;; 

let geq (I x) (I y) = (x<=y) ;; 

let trinp_out (fmt:Format.formatter) (I(x))=
   Format.fprintf fmt "@[%s@]" (string_of_int x);;  

end ;;

end ;;

module type Z_TYPE =
      sig
        type t 
        val add : t -> t -> t

        val equals : t -> t -> bool
        
        val div : t -> t -> t
        val gcd : t -> t -> t

        val geq : t -> t -> bool
        val leq : t -> t -> bool
        val of_int : int -> t
        val of_string : string -> t
        val one : t
        val opposite : t -> t

        val mul : t -> t -> t

        val trinp_out : Format.formatter -> t -> unit
        val sub : t -> t -> t
        val to_string : t -> string
        val zero : t
        
end ;;

module Z = (Private2.Z:Z_TYPE) ;;


module Private = struct

module Q = struct 

type t = {den: Z.t ; num: Z.t} ;;

let numerator q = q.num ;;
let denominator q = q.den ;;

exception Zero_denominator ;;
let make_with_positive_denominator x y =
   let g = Z.gcd x y in 
   {den = Z.div x g; num = Z.div y g} ;;

let make x y =
  if Z.equals y Z.zero 
  then raise Zero_denominator
  else
  if Z.leq y Z.zero 
  then make_with_positive_denominator (Z.opposite x) (Z.opposite y)  
  else make_with_positive_denominator x y ;;

let of_int k = make (Z.of_int k) Z.one ;;

let of_string s = 
  if String.contains s '/'
  then let (before,after) = 
          Cull_string.split_wrt_rightmost  s '/' in 
       make (Z.of_string before) (Z.of_string after)   
  else make (Z.of_string s) Z.one ;;
let to_string q = 
  if Z.equals q.den Z.one
  then Z.to_string q.num
  else (Z.to_string q.num) ^ "/" ^ (Z.to_string q.den)  ;;

let zero = {den = Z.zero; num = Z.one} ;;
let one = {den = Z.one; num = Z.one} ;;

let opposite q = {q with den = Z.opposite(q.den)} ;;
let add q1 q2 = make 
  (Z.add (Z.mul q1.num q2.den) 
         (Z.mul q1.den q2.num) ) 
  (Z.mul q1.den q2.den) ;;
let mul q1 q2 = make 
  (Z.mul q1.num q2.num) 
  (Z.mul q1.den q2.den) ;;

let div q1 q2 = make 
  (Z.mul q1.num q2.den) 
  (Z.mul q1.den q2.num) ;;  

let inv q = div one q ;;  
let sub q1 q2 = make 
  (Z.sub (Z.mul q1.num q2.den) 
         (Z.mul q1.den q2.num) ) 
  (Z.mul q1.den q2.den) ;;


let equals q1 q2 = Z.equals  
 (Z.mul q1.num q2.den) 
 (Z.mul q1.den q2.num)
 ;; 

let leq q1 q2 = Z.leq  
 (Z.mul q1.num q2.den) 
 (Z.mul q1.den q2.num)
 ;;  

let geq q1 q2 = Z.geq  
 (Z.mul q1.num q2.den) 
 (Z.mul q1.den q2.num)
 ;;  

let floor q = 
  if Z.equals q.den Z.one
  then q.num  
  else 
  if Z.leq q.num Z.zero 
  then let onum = Z.opposite q.num in 
       let ofloor = Z.div onum q.den in 
       Z.opposite(Z.add ofloor Z.one)
  else Z.div q.num q.den ;;
  
let ceil q = 
  if Z.equals q.den Z.one
  then q.num  
  else 
  if Z.leq q.num Z.zero 
  then let onum = Z.opposite q.num in 
       let ofloor = Z.div onum q.den in 
       Z.opposite(ofloor)
  else Z.add (Z.div q.num q.den) Z.one ;;  
  

let trinp_out (fmt:Format.formatter) q=
   Format.fprintf fmt "@[%s@]" (to_string q);;  

end ;;



end ;;


module type Q_TYPE =
      sig
        
        exception Zero_denominator 

        type t = {den: Z.t ; num: Z.t}
        val add : t -> t -> t

        val ceil : t -> Z.t
        val div : t -> t -> t
        val equals : t -> t -> bool
        
        val floor : t -> Z.t 
        val geq : t -> t -> bool

        val inv : t -> t 
        val leq : t -> t -> bool
        val of_int : int -> t
        val of_string : string -> t
        val one : t
        val opposite : t -> t

        val mul : t -> t -> t

        val trinp_out : Format.formatter -> t -> unit
        val sub : t -> t -> t
        val to_string : t -> string
        val zero : t
        
end ;;

module Q = (Private.Q:Q_TYPE) 

