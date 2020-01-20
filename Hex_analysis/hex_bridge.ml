(* 

#use"Hex_analysis/hex_bridge.ml";;

*)

exception Invalid_bridge of ( int * int );;

module Private = struct 

let ipairs_touching_an_ipair_bridge dim (i1,j1) (i2,j2)=
    let current_diff = (i2-i1,j2-j1) in 
    match Option.seek (
        fun (diff_pair,res)-> current_diff = diff_pair 
    ) [
         (0,1),[(i1-1,j1+1);(i1+1,j1)];
         (-1,1),[(i1-1,j1);(i1,j1+1)];
         (-1,0),[(i1-1,j1+1);(i1,j1-1)];
         (0,-1),[(i1-1,j1);(i1+1,j1-1)];  
         (1,-1),[(i1,j1-1);(i1+1,j1)];
         (1,0),[(i1+1,j1-1);(i1,j1+1)];
      ]  with 
    None -> raise(Invalid_bridge(current_diff))
    |Some(_,res)->
      List.filter (Hex_ipair.is_valid dim) res;;




end ;;

let bridges_touching_a_cell dim cell =
    let (i,j) = Hex_ipair.of_cell cell in 
    let p1=(i,j+1) and p2=(i-1,j+1) and p3=(i-1,j)
    and p4=(i,j-1) and p5=(i+1,j-1) and p6=(i+1,j) in 
   let checker1 = Hex_ipair.is_valid dim   in
   let checker2 = (fun (p,q)-> (checker1 p)&&(checker1 q) ) in 
   let unordered_ipairs = List.filter checker2 [
      p1,p2;p2,p3;p3,p4;p4,p5;p5,p6;p6,p1
   ] in 
   let unordered_cells = Image.image (fun (p,q)->
       let l = Ordered.sort Hex_cell.cmp (Image.image Hex_ipair.to_cell [p;q]) in  
      (List.nth l 0,List.nth l 1)) unordered_ipairs in 
   Ordered.sort Hex_cell.cmp_for_pairs unordered_cells;;
   
let cells_touching_a_bridge dim (cell1,cell2)=
   let p1=Hex_ipair.of_cell cell1 
   and p2=Hex_ipair.of_cell cell2 in 
   let temp1=Private.ipairs_touching_an_ipair_bridge dim p1 p2 in 
   Hex_cell_set.safe_set (Image.image Hex_ipair.to_cell temp1);;