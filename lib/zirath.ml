(*

#use"lib/zirath.ml";;

A simplified version of the Zarith library.

*)

module Zay = Z ;; 
module Quay = Q ;;

module type Z_TYPE =
  sig
    type t = Wrap of Z.t
    val add : t -> t -> t
    val div : t -> t -> t
    val equals : t -> t -> bool
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
    val sub : t -> t -> t
    val to_string : t -> string
    val trinp_out : Format.formatter -> t -> unit
    val zero : t
end ;;


module Z = (struct 


  type t = Wrap of Zay.t ;;

  let add (Wrap x) (Wrap y) = (Wrap(Zay.add x y)) ;;

  let div (Wrap x) (Wrap y) = (Wrap(Zay.div x y)) ;;
   
  let equals (Wrap x) (Wrap y) = Zay.equal x y ;;

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
  let sub (Wrap x) (Wrap y) = (Wrap(Zay.sub x y)) ;;

  let to_string (Wrap x) = Zay.to_string x ;;

  let trinp_out (fmt:Format.formatter) x=
   Format.fprintf fmt "@[%s@]" (to_string x);;   
  let zero = Wrap Zay.zero ;;  

end : Z_TYPE) ;;  

module type Q_TYPE =
  sig
    type t = Wrap of Q.t
    val add : t -> t -> t
    val den : t -> Z.t
    val div : t -> t -> t
    val equals : t -> t -> bool
    val geq : t -> t -> bool
    val gt : t -> t -> bool
    val leq : t -> t -> bool
    val lt : t -> t -> bool
    val mul : t -> t -> t
    val num : t -> Z.t
    val of_int : int -> t
    val of_ints : int -> int -> t
    val of_string : string -> t
    val one : t
    val sub : t -> t -> t
    val to_string : t -> string
    val trinp_out : Format.formatter -> t -> unit
    val zero : t
  end



module Q = (struct 


  type t = Wrap of Quay.t ;;

  let add (Wrap x) (Wrap y) = (Wrap(Quay.add x y)) ;;

  let den (Wrap x) = Z.of_zarith (Q.den x) ;;
  let div (Wrap x) (Wrap y) = (Wrap(Quay.div x y)) ;;
   
  let equals (Wrap x) (Wrap y) = Quay.equal x y ;;

 
  let geq (Wrap x) (Wrap y) = Quay.geq x y ;;

  let gt (Wrap x) (Wrap y) = Quay.gt x y ;;
  let leq (Wrap x) (Wrap y) = Quay.leq x y ;;

  let lt (Wrap x) (Wrap y) = Quay.lt x y ;;
  let mul (Wrap x) (Wrap y) = (Wrap(Quay.mul x y)) ;;

  let num (Wrap x) = Z.of_zarith (Q.den x) ;;
  let of_int i = Wrap (Quay.of_int i) ;;

  let of_ints i j = Wrap (Quay.of_ints i j) ;;
  let of_string i = Wrap (Quay.of_string i) ;;
  let one = Wrap Quay.one ;;
  let sub (Wrap x) (Wrap y) = (Wrap(Quay.sub x y)) ;;
  let to_string (Wrap x) = Quay.to_string x ;;

  let trinp_out (fmt:Format.formatter) x=
   Format.fprintf fmt "@[%s@]" (to_string x);;
  let zero = Wrap Quay.zero  ;;

  

end : Q_TYPE) ;;  

