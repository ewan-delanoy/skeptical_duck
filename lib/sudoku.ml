(*

#use"lib/sudoku.ml";;

*)

type cell = C of int ;;

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

end ;;  