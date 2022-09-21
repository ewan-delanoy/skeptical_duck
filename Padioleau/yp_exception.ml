(*

#use"Padioleau/yp_exception.ml";;

*)

type t = exn * Printexc.raw_backtrace ;; 

let catch exn = (exn, Printexc.get_raw_backtrace ()) ;; 

let reraise ((exn, trace) : t) =
  Printexc.raise_with_backtrace exn trace ;; 
 