(*

#use"lib/sudoku.ml";;

*)

(*
#install_printer Display.print_out_grid ;;
*)

type cell = C of int * int ;;

type box =
    Row of int
   |Column of int 
   |Square of int ;; 

type grid = G of (int list) list;;


let i_order = Total_ordering.for_integers ;;
let i_fold_merge = Ordered.fold_merge i_order ;;
let i_outsert = Ordered.outsert i_order ;;

module Cell = struct 

let from_matrix_coordinates i j = C(i,j) ;; 

let horizontal_coordinate (C (_i,j)) = j;; 
 
let vertical_coordinate  (C (i,_j)) = i;; 
 
let square_coordinate (C (i,j)) =
   (Basic.frac_ceiling j 3) + 3* ((Basic.frac_ceiling i 3)-1) ;;

let all = Image.image (fun (i,j)->C(i,j)) (Cartesian.square(Int_range.range 1 9)) ;;

let test_for_neighborhood c1 c2 = 
      ((horizontal_coordinate c1)=(horizontal_coordinate c2))
      ||
      ((vertical_coordinate c1)=(vertical_coordinate c2))
      ||
      ((square_coordinate c1)=(square_coordinate c2))
    ;;


let first_in_given_square square_idx = List.find (fun c->square_coordinate(c)=square_idx) all;;     
let single_index (C(i,j)) = j+9*(i-1) ;;
let at_single_index idx = let q = (idx-1)/9 in C(q+1,idx-9*q);;
let to_short_string (C(i,j)) = "("^(string_of_int i)^","^(string_of_int j)^")" ;;
 

end ;;

module Box = struct 

  let content = function 
   Row (i) -> Int_range.scale (fun j->Cell.from_matrix_coordinates i j) 1 9
  |Column (j) -> Int_range.scale (fun i->Cell.from_matrix_coordinates i j) 1 9
  |Square (k) -> 
     let (C(x0,y0)) = Cell.first_in_given_square k in 
     Image.image (fun (i,j)->Cell.from_matrix_coordinates i j)
     (Cartesian.product [x0;x0+1;x0+2] [y0;y0+1;y0+2]) 
     ;; 
     
  let all =
      (Int_range.scale (fun i->Row i) 1 9) 
      @  
      (Int_range.scale (fun j-> Column j) 1 9)
      @  
      (Int_range.scale (fun k-> Square k) 1 9)
       
  let to_short_string = function 
      Row (i) -> "Row("^(string_of_int i)^")"
     |Column (j) -> "Column("^(string_of_int j)^")"
     |Square (k) -> "Square("^(string_of_int k)^")";; 
           

end ;;   

module Grid = struct 

exception Assign_exn of cell ;;  

module Private = struct

let origin = 
  let small_base = Int_range.range 1 9 in 
  G(Int_range.scale (fun t->small_base) 1 81) ;;

let possibilities_at_cell (G l) cell = List.nth l ((Cell.single_index cell)-1) ;;  

let uncurried_assign (G l) (cell,v) = 
   let possibilities = possibilities_at_cell (G l) cell  in 
   if not(List.mem v possibilities)
   then raise(Assign_exn(cell))
   else 
   let combined_l = List.combine Cell.all l in 
   let new_l = Image.image (
     fun (cell2,poss)->
        if cell2=cell then [v] else 
        if Cell.test_for_neighborhood cell cell2 
        then i_outsert v poss
        else poss       
   ) combined_l in 
   G new_l ;;  

end ;;

let assign gr cell v = Private.uncurried_assign gr (cell,v) ;; 

let assign_several gr assignments = 
     List.fold_left  Private.uncurried_assign gr assignments ;; 

let initialize_with l =
    let temp1 = List.combine Cell.all l in 
    let temp2 = List.filter (fun (cell,v)->v<>0) temp1 in 
    assign_several Private.origin temp2 ;; 

let possibilities_at_cell = Private.possibilities_at_cell ;; 

end ;;

module Display = struct 

module Private = struct 

      let eval_small_grid_using_matrix_coordinates gr (i,j) = 
         let cell = Cell.from_matrix_coordinates i j in 
         let poss = Grid.possibilities_at_cell gr cell in 
         let m = List.length poss in 
         if m = 0 then "B" else 
         if m = 1 then  string_of_int(List.hd poss) else
         " " ;; 

      let eval_large_grid_using_matrix gr large_i large_j =
          let small_i =  List_again.find_index_of_in large_i [2;3;4;6;7;8;10;11;12]
          and small_j =  List_again.find_index_of_in large_j [2;3;4;6;7;8;10;11;12] in 
      if (small_i<0)||(small_j<0)
      then "*"
      else eval_small_grid_using_matrix_coordinates gr (small_i,small_j);;
      
      let large_line bg large_i = String.concat "" 
        (Int_range.scale(eval_large_grid_using_matrix bg large_i) 1 13) ;;
        
      let large_lines bg = Int_range.scale (large_line bg ) 1 13 ;;
  
      let large_grid bg  = (String.concat "\n" (large_lines bg));;   

  let to_string bg = (large_grid bg);;    

  let to_surrounded_string gr = "\n\n\n"^(to_string gr)^"\n\n\n" ;;  
  
  

end ;; 

let print_out_grid (fmt:Format.formatter) gr=
  Format.fprintf fmt "@[%s@]" (Private.to_surrounded_string gr);;

let grid_to_string = Private.to_string ;; 


end ;;  



(*

let cm = Grid.compute_minimizers_inside ;; 


let g0 = Grid.initialize_with 

[
   0;0;0;  0;5;0;  0;0;0;
   8;4;0;  6;0;0;  3;0;0; 
   0;5;0;  0;3;0;  0;7;4; 

   0;0;1;  0;0;9;  7;0;0;
   0;0;2;  0;1;0;  9;0;0;
   0;0;4;  5;0;0;  8;0;0;

   4;2;0;  0;6;0;  0;9;0;
   0;0;9;  0;0;5;  0;2;8;
   0;0;0;  0;2;0;  0;0;0;

];; 

let g0 = Grid.make_all_immediate_deductions original_g0 ;; 

*)

