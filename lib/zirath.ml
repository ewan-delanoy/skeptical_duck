(*

#use"lib/zirath.ml";;

A simplified version of the Zarith library.

*)

module Zay = Z ;; 
module Quay = Q ;;

module type Z_TYPE =
  sig
    type t 
    val abs : t -> t
    val add : t -> t -> t
    val ediv : t -> t -> t
    val equals : t -> t -> bool
    val erem : t -> t -> t
    val gcd : t -> t -> t
    val geq : t -> t -> bool
    val gt : t -> t -> bool
    val leq : t -> t -> bool
    val lt : t -> t -> bool
    val mul : t -> t -> t
    val of_int : int -> t
    val of_string : string -> t
    val of_zarith : Zay.t -> t
    val one : t
    val order : t Total_ordering_t.t
    val sub : t -> t -> t
    val to_string : t -> string
    val to_zarith : t -> Zay.t 
    val trinp_out : Format.formatter -> t -> unit
    val zero : t
end ;;

 
module Z = (struct 


  type t = Wrap of Zay.t ;;

  let abs (Wrap x) = (Wrap(Zay.abs x)) ;;
  let add (Wrap x) (Wrap y) = (Wrap(Zay.add x y)) ;;

  let ediv (Wrap x) (Wrap y) = (Wrap(Zay.ediv x y)) ;;
   
  let equals (Wrap x) (Wrap y) = Zay.equal x y ;;

  let erem (Wrap x) (Wrap y) = (Wrap(Zay.erem x y)) ;;

  let gcd (Wrap x) (Wrap y) = (Wrap(Zay.gcd x y)) ;;
 
  let geq (Wrap x) (Wrap y) = Zay.geq x y ;;

  let gt (Wrap x) (Wrap y) = Zay.gt x y ;;
  let leq (Wrap x) (Wrap y) = Zay.leq x y ;;

  let lt (Wrap x) (Wrap y) = Zay.lt x y ;;
  let mul (Wrap x) (Wrap y) = (Wrap(Zay.mul x y)) ;;

  let of_int i = Wrap (Zay.of_int i) ;;
  let of_string i = Wrap (Zay.of_string i) ;;

  let of_zarith z = Wrap z ;;
  let one = Wrap Zay.one ;;

  let order = ((fun x y ->
  if lt x y then Total_ordering_result_t.Lower else 
  if equals x y then Total_ordering_result_t.Equal else 
  Total_ordering_result_t.Greater     
  ): t Total_ordering_t.t) ;;

  let sub (Wrap x) (Wrap y) = (Wrap(Zay.sub x y)) ;;

  let to_string (Wrap x) = Zay.to_string x ;;

  let to_zarith (Wrap x) = x ;;

  let trinp_out (fmt:Format.formatter) x=
   Format.fprintf fmt "@[%s@]" (to_string x);;   
  let zero = Wrap Zay.zero ;;  

end : Z_TYPE) ;;  

module type Q_TYPE =
  sig
    type t 
    val abs : t -> t
    val add : t -> t -> t
    val ceil : t -> Z.t
    val den : t -> Z.t
    val div : t -> t -> t
    val equals : t -> t -> bool
    val floor : t -> Z.t
    val geq : t -> t -> bool
    val gt : t -> t -> bool
    val leq : t -> t -> bool
    val lt : t -> t -> bool
    val make : Z.t -> Z.t -> t
    val mul : t -> t -> t
    val num : t -> Z.t
    val of_int : int -> t
    val of_ints : int -> int -> t
    val of_string : string -> t
    val of_zirath : Z.t -> t
    val one : t
    val order : t Total_ordering_t.t
    val sub : t -> t -> t
    val to_float : t -> float
    val to_string : t -> string
    val trinp_out : Format.formatter -> t -> unit
    val zero : t
  end



module Q = (struct 


  type t = Wrap of Quay.t ;;

  module Private = struct 

  let den (Wrap x) = Z.of_zarith (Quay.den x) ;;

  let num (Wrap x) = Z.of_zarith (Quay.num x) ;;
  
  let floor x= 
    let n = num x and d = den x in 
    Z.ediv n d ;; 

  let ceil x = 
     let d = den x and n = num x in 
     let q = Z.ediv d n in 
     if Z.equals n (Z.mul d q)
     then q
     else Z.add q Z.one;; 

  end ;;

  let abs (Wrap x) = (Wrap(Quay.abs x)) ;;
  let add (Wrap x) (Wrap y) = (Wrap(Quay.add x y)) ;;

  let ceil  = Private.ceil ;;
  let den = Private.den ;;
  let div (Wrap x) (Wrap y) = (Wrap(Quay.div x y)) ;;
   
  let equals (Wrap x) (Wrap y) = Quay.equal x y ;;

  let floor = Private.floor ;;
 
  let geq (Wrap x) (Wrap y) = Quay.geq x y ;;

  let gt (Wrap x) (Wrap y) = Quay.gt x y ;;
  let leq (Wrap x) (Wrap y) = Quay.leq x y ;;

  let lt (Wrap x) (Wrap y) = Quay.lt x y ;;

  let make x y = Wrap(Quay.make (Z.to_zarith x) (Z.to_zarith y));;
  let mul (Wrap x) (Wrap y) = (Wrap(Quay.mul x y)) ;;

  let num = Private.num ;;
  let of_int i = Wrap (Quay.of_int i) ;;

  let of_ints i j = Wrap (Quay.of_ints i j) ;;
  let of_string i = Wrap (Quay.of_string i) ;;

  let of_zirath z = Wrap ({Quay.num=(Z.to_zarith z);Quay.den=Zay.one}) ;;
  let one = Wrap Quay.one ;;

  let order = ((fun x y ->
  if lt x y then Total_ordering_result_t.Lower else 
  if equals x y then Total_ordering_result_t.Equal else 
  Total_ordering_result_t.Greater     
  ): t Total_ordering_t.t) ;;
  let sub (Wrap x) (Wrap y) = (Wrap(Quay.sub x y)) ;;
  let to_float (Wrap x) = Quay.to_float x ;;
  let to_string (Wrap x) = Quay.to_string x ;;

  let trinp_out (fmt:Format.formatter) x=
   Format.fprintf fmt "@[%s@]" (to_string x);;
  let zero = Wrap Quay.zero  ;;

  

end : Q_TYPE) ;;  

