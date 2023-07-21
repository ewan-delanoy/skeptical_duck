(*

#use"lib/sudoku.ml";;

*)

type cell = C of int ;;
type box = B of (cell list) * bool ;;
type visible_grid = VG of int array ;; 
type assumption = Assmpt of cell * int ;;

type state = {
   visible : visible_grid;
   assumptions : assumption list;
   prerequisites : (cell * (assumption list));
} ;;

module Cell = struct 

let horizontal_coordinate (C k) = 
    let q = Basic.frac_ceiling k 9 in 
    k-9*(q-1) ;;

let vertical_coordinate (C k) =
     Basic.frac_ceiling k 9 ;;
         
let square_coordinate c =
   let h = horizontal_coordinate c 
   and v = vertical_coordinate c in 
   (Basic.frac_ceiling h 3) + 3* ((Basic.frac_ceiling v 3)-1) ;;

let all = Int_range.scale (fun k-> C k) 1 81 ;; 

end ;;  

module BoxBasics = struct

module Private = struct

let nth_row k =
   B(List.filter (fun c->Cell.vertical_coordinate c = k) Cell.all,true);;
let nth_column k =
    B(List.filter (fun c->Cell.horizontal_coordinate c = (k-9)) Cell.all,true);;
let nth_square k =
    B(List.filter (fun c->Cell.square_coordinate c = (k-18)) Cell.all,true);;
let nth_nondual k = 
   match Basic.frac_ceiling k 9 with 
     1 -> nth_row k
    |2 -> nth_column k
    |3 -> nth_square k
    |_ -> failwith("Bad nondual index")
;;
let nth_dual k = 
    let (B(l,_)) = nth_nondual(k-27) in 
    B(l,false);;
let nth = Memoized.make(fun k->
   if k<=27 
   then nth_nondual(k) 
   else nth_dual k
  );;
let all = Int_range.scale nth 1 54 ;;
end ;;

let nth = Private.nth ;; 

end ;;