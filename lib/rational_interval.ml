(*

#use"lib/rational_interval.ml";;

*)

module Private = struct 

let intersect 
  (Rational_interval_t.I(a1,b1)) 
     (Rational_interval_t.I(a2,b2)) = 
    let a = Zirath.Q.max a1 a2 
    and b = Zirath.Q.min b1 b2 in 
    if Zirath.Q.gt a b 
    then None   
    else Some(Rational_interval_t.I(a,b));; 

let is_a_singleton 
  (Rational_interval_t.I(a,b)) = 
    Zirath.Q.equals a b ;;

let mem x (Rational_interval_t.I(a,b)) =
  (Zirath.Q.leq a x) && (Zirath.Q.leq x b) ;; 

end ;;  

let divide itv unordered_points=
  let (Rational_interval_t.I(a,b)) = itv in 
  let points = Ordered.sort Zirath.Q.order unordered_points in 
  let good_points = List.filter (fun x->Private.mem x itv) points in 
  let all_points = a :: (good_points@[b]) in 
  let pairs = List_again.universal_delta_list all_points in 
  let nonsingleton_pairs = List.filter(fun (x,y)->
       not(Zirath.Q.equals x y)  
  ) pairs in 
  Image.image (fun (x,y) ->Rational_interval_t.I(x,y) ) nonsingleton_pairs ;;    

let intersect = Private.intersect ;; 

let is_a_singleton = Private.is_a_singleton ;;

let is_included_in 
  (Rational_interval_t.I(a1,b1)) 
     (Rational_interval_t.I(a2,b2)) = 
   (Zirath.Q.leq a2 a1) && (Zirath.Q.leq b1 b2) ;; 

let mem = Private.mem ;;
