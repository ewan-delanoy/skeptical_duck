(*

#use"lib/sudoku.ml";;

*)

type cell = C of int * int ;;

type box =
    Row of int
   |Column of int 
   |Square of int ;; 
   
type generalized_cell =
   Gc_Cell of cell 
  |Gc_From_DualBox of box * int;; 

type generalized_box =
    Gb_usual of box
   |Gb_dual of box ;;    

type visible_grid = VG of int list ;; 

type assumption_list = AL of (cell * int) list ;;
type prerequisite_list = PL of (cell * (int list)) list ;;

type enhanced_grid = {
   visible : visible_grid;
   assumptions : assumption_list;
   prerequisites : prerequisite_list;
} ;;

type mini_result =
     Contradictions of (generalized_cell * (int list * (int * (cell * int) list) list)) list 
    |Deductions of ((cell * int) * generalized_cell list) list * generalized_cell list
    |Overtures of (generalized_cell * int list) list;;
   

module Cell = struct 

let from_matrix_coordinates i j = C(i,j) ;; 

let horizontal_coordinate (C (_i,j)) = j;; 
 
let vertical_coordinate  (C (i,_j)) = i;; 
 
let square_coordinate (C (i,j)) =
   (Basic.frac_ceiling j 3) + 3* ((Basic.frac_ceiling i 3)-1) ;;

let all = Image.image (fun (i,j)->C(i,j)) (Cartesian.square(Int_range.range 1 9)) ;;

let neighbors =Memoized.make(fun c ->
    List.filter 
     (fun c2->
        ((horizontal_coordinate c)=(horizontal_coordinate c2))
        ||
        ((vertical_coordinate c)=(vertical_coordinate c2))
        ||
        ((square_coordinate c)=(square_coordinate c2))
      )
     all);;
    
let first_in_given_square square_idx = List.find (fun c->square_coordinate(c)=square_idx) all;;     
let single_index (C(i,j)) = j+9*(i-1) ;;
let to_short_string (C(i,j)) = (string_of_int i)^""^(string_of_int j) ;;

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
       

end ;;   





module Visible_Grid = struct 

let eval (VG gr) c = List.nth gr (Cell.single_index(c)-1) ;; 

let current_data vg=
   List.filter_map (fun c->
    let j=eval vg c in
    if j<>0 
    then Some(c,j)
    else None
  ) Cell.all ;; 

let competitors vg (cell1,j1)= 
   List.filter ( fun (cell2,j2) ->
     if (cell1=cell2) then j1<>j2 else  
     if j2<>j1 then false else  
      List.mem cell2 (Cell.neighbors cell1)    
    ) (current_data vg) ;; 

  module Display = struct 

    let eval_small_grid_using_matrix_coordinates vg (i,j) = eval vg (Cell.from_matrix_coordinates i j) ;;
    
    let eval_large_grid_using_matrix vg large_i large_j =
        let small_i =  List_again.find_index_of_in large_i [2;3;4;6;7;8;10;11;12]
        and small_j =  List_again.find_index_of_in large_j [2;3;4;6;7;8;10;11;12] in 
    if (small_i<0)||(small_j<0)
    then "*"
    else string_of_int(eval_small_grid_using_matrix_coordinates vg (small_i,small_j));;
    
    let large_line vg large_i = String.concat "" 
      (Int_range.scale(eval_large_grid_using_matrix vg large_i) 1 13) ;;
      
    let large_lines vg = Int_range.scale (large_line vg) 1 13 ;;

    let large_grid vg = (String.concat "\n" (large_lines vg));;  

  end ;;  

let to_string = Display.large_grid ;; 

exception Set_exn of cell * int ;; 

let set (VG old_gr) c k = 
   if eval (VG old_gr) c <> 0
   then raise(Set_exn(c,k)) 
   else 
   let temp = List.combine Cell.all old_gr in  
   let new_gr = Image.image (fun (c2,v)->if c2=c then k else v ) temp in 
   (VG new_gr);;

let erase (VG old_gr) c=
  let temp = List.combine Cell.all old_gr in  
  let new_gr = Image.image (fun (c2,v)->if c2=c then 0 else v ) temp in 
  (VG new_gr);;

let erase_several vg cells = List.fold_left erase vg cells ;;   

end ;;  

module Generalized_Cell = struct 

  
  let eval vg = function
   Gc_Cell(cell)->Visible_Grid.eval vg cell 
  |Gc_From_DualBox (box,y)->
    let temp1 = Int_range.scale (fun t->
       (Visible_Grid.eval vg (List.nth (Box.content box) (t-1)),t) ) 1 9 in
     match List.assoc_opt y temp1 with 
      Some answer -> answer
    | None -> 0 ;;

   let all = 
      (Image.image (fun cell->Gc_Cell(cell)) Cell.all)
      @
      (
        Image.image (fun (box,y)->Gc_From_DualBox(box,y))
         (Cartesian.product Box.all (Int_range.range 1 9))
      );;

end ;;   


module Possible = struct 

let is_concrete gcell= match gcell with 
  Gc_Cell(_) -> true
  |Gc_From_DualBox(_,_) -> false ;;   
  
let reduce_to_standard (gcell,vaal)= match gcell with 
Gc_Cell(cell) -> (cell,vaal) 
|Gc_From_DualBox(box,vaal2) ->(List.nth (Box.content box) (vaal-1),vaal2) ;; 
  
let possible_fillers vg gcell =
   let temp1 = Int_range.scale (
      fun k->(k,Visible_Grid.competitors vg (reduce_to_standard(gcell,k)))
   ) 1 9  in 
   let (possible_ones,impossible_ones) =
      List.partition (fun (_,competitors)->competitors=[]) temp1 in 
   (gcell,(Image.image fst possible_ones,impossible_ones)) ;;   
 

let compute_all_possible_fillers =Memoized.make(fun vg ->
   Image.image (possible_fillers vg) Generalized_Cell.all );;
  
  
let incoming_contradictions vg = List.filter (
  fun (_g_cell,(possibilities,_))->(possibilities=[])
) (compute_all_possible_fillers vg) ;; 

let immediate_deductions vg = List.filter (
  fun (g_cell,(possibilities,_))->
    (Generalized_Cell.eval vg g_cell=0)&&
    (List.length(possibilities)=1)
) (compute_all_possible_fillers vg) ;; 

let easiest_cells vg =
   let temp1 = List.filter (
    fun (g_cell,_)->Generalized_Cell.eval vg g_cell=0
   ) (compute_all_possible_fillers vg) in
   let (_,temp2)=Min.minimize_it_with_care (
    fun (_g_cell,(possibilities,_))-> List.length(possibilities)
   ) temp1  in 
   Image.image (
    fun (g_cell,(possibilities,_))-> (g_cell,possibilities)
   ) temp2 ;;


end ;;  

module Assumption_list = struct 

let add (AL l) c k = AL(l@[c,k]) ;; 

let cut_using_threshhold (AL assumptions) threshhold=
      let (kept_assumptions,removed_assumptions) = 
          List_again.long_head_with_tail (threshhold-1) assumptions in 
      (Image.image fst removed_assumptions,AL(List.rev kept_assumptions)) ;; 

let to_string (AL l) =
   let temp1 = Image.image (fun (c,k)->(Cell.to_short_string c)^" -> "^(string_of_int k)) l in 
   let temp2 = (if l=[] then "None." else String.concat "," temp1) in
   "Assumptions : "^temp2 ;; 


end ;;  


module Prerequisites = struct 

  let add (PL l) c k = PL(l@[c,k]) ;; 

  let eval (PL pq) cell =
      match List.assoc_opt cell pq with 
       Some(answer) -> answer
       | None -> [] ;; 

  let i_order = Total_ordering.for_integers ;;
  let order = ((fun il1 il2 ->
      if il1 = il2 then Total_ordering_result_t.Equal else 
      if il1 = [] then Total_ordering_result_t.Lower else 
      if il2 = [] then Total_ordering_result_t.Greater else 
      let trial1 = i_order (Max.list il1) (Max.list il2) in 
      if trial1<>Total_ordering_result_t.Equal then trial1 else 
      Total_ordering.silex_for_intlists il1 il2 
  ): (int list) Total_ordering_t.t);;

  let select pq ci_list =
     let temp1 = Image.image (fun (cell,_i)->eval pq cell) ci_list in 
     let m = Ordered.min order temp1 in 
     List.find (fun (cell,_i)->eval pq cell=m) ci_list ;; 

  let cut_using_threshhold (PL pq) idx =
      let (kept,removed)=List.partition (
          fun (_cell,indices)->Max.list(indices)<idx
       ) pq  in
       (Image.image fst removed,PL kept) ;;

  let string_of_intlist l = "["^(String.concat "," (Image.image string_of_int l))^"]" ;;

  let to_string (PL l) =
    let temp1 = Image.image (fun (c,il)->(Cell.to_short_string c)^" -> "^(string_of_intlist il)) l in 
    let temp2 = (if l=[] then "None." else String.concat "," temp1) in
    "Prerequisites : "^temp2 ;;  

end ;;   


module Enhanced_Grid = struct 

  exception Cell_not_deducible of generalized_cell;;

  let details_for_deducible_cell eg gcell = 
    let (_,(possibilities,impossibilities)) = Possible.possible_fillers eg.visible gcell in 
    if List.length(possibilities)<>1 
    then raise(Cell_not_deducible(gcell))
    else  
    let simplified_impossibilities = Image.image 
      (fun (_k,ci_list)->Prerequisites.select eg.prerequisites ci_list) impossibilities in 
    let val_for_gcell =  List.hd possibilities in  
    let (cell,val_for_cell) = Possible.reduce_to_standard (gcell,val_for_gcell) in 
    (cell,val_for_cell,gcell,simplified_impossibilities) ;;     

  let assume old_eg c k= 
  {
    old_eg with 
    visible = Visible_Grid.set (old_eg.visible) c k;
    assumptions =Assumption_list.add (old_eg.assumptions) c k;
  } ;; 

  let immediate_deductions eg = 
      let temp0 = Possible.immediate_deductions eg.visible in 
      let temp1 = Image.image (fun (gcell,_)->
       let (c,val_for_c,gc,_l)=details_for_deducible_cell eg gcell in
        (c,(val_for_c,gc)) 
      ) temp0 in 
      let unordered_cells = Image.image fst temp1 in 
      let ordered_cells = List.filter (fun c->List.mem c unordered_cells) Cell.all in 
      let temp2 = Image.image (fun c->
        let ttemp2 = List.filter_map
        (fun p->if fst(p)=c then Some(snd p) else None) temp1 in 
        let val_for_c = fst(List.hd(ttemp2)) in 
       ((c,val_for_c),Image.image snd ttemp2)) ordered_cells in 
      (temp2,
       Image.image (fun p->List.hd(snd p)) temp2) 
    ;;
  
  let enforce_deduction old_eg gcell =
      let (c,only_possible_value_for_c,_,prerequisitors) = details_for_deducible_cell old_eg gcell in 
      let final_prerequired = Ordered.fold_merge
          Total_ordering.for_integers  
      (Image.image (fun (cc,_i)->
        Prerequisites.eval old_eg.prerequisites cc
         ) prerequisitors) in 
      {
        old_eg with 
        visible = Visible_Grid.set (old_eg.visible) c only_possible_value_for_c;
        prerequisites =Prerequisites.add (old_eg.prerequisites) c final_prerequired;
      } ;; 
  
  exception  Enforce_deduction_at_cell_exn of cell ;;    

  let enforce_deduction_at_cell old_eg cell =
     let (temp1,_) = immediate_deductions old_eg in 
     match List.find_opt (fun ((cell2,_),_) -> cell2=cell) temp1 with 
      None -> raise(Enforce_deduction_at_cell_exn(cell))
      |Some(_,l_gcell) ->  enforce_deduction old_eg (List.hd l_gcell) ;;     

  let enforce_deductions eg gcells =
      List.fold_left enforce_deduction eg gcells ;;

  let enforce_all_current_deductions eg = 
    let (_,gcells) = immediate_deductions eg in 
    enforce_deductions eg gcells ;; 

  let step eg =
      let trial1 = Possible.incoming_contradictions eg.visible in 
      if trial1<>[] 
      then Contradictions(trial1)
      else 
      let (trial2_1,trial2_2) = immediate_deductions eg in 
      if (trial2_1,trial2_2)<>([],[]) 
      then Deductions(trial2_1,trial2_2)
      else Overtures(Possible.easiest_cells eg.visible) ;;      
    

  let to_string eg =

     String.concat "\n"
      ["\n";Visible_Grid.to_string eg.visible;
       Assumption_list.to_string eg.assumptions;
       Prerequisites.to_string eg.prerequisites;"\n"]

  let display eg = (print_string(to_string eg);flush stdout) ;;    

  let cut_using_threshhold old_eg threshhold=
      let (removed_cells1,kept_assumptions) = 
          Assumption_list.cut_using_threshhold old_eg.assumptions threshhold
      and (removed_cells2,kept_prerequisites) = 
         Prerequisites.cut_using_threshhold old_eg.prerequisites threshhold in
      let removed_cells= removed_cells1 @ removed_cells2 in 
      {
        visible = Visible_Grid.erase_several (old_eg.visible) removed_cells;
        assumptions =  kept_assumptions;
        prerequisites = kept_prerequisites;

      } ;;     
 
  let rec helper_for_automatization (eg,deductions) =
      let (temp1,_) = immediate_deductions eg in 
      if temp1 = []
      then (eg,List.rev deductions)
      else 
      let ((c,v),l) = List.hd temp1 in 
      let gcell = List.hd l in
      helper_for_automatization(enforce_deduction eg gcell,(c,v)::deductions)  ;;    
      
  let automatize eg = helper_for_automatization (eg,[]) ;;    

end ;;  

module Walker = struct 

module Private = struct 

let origin = {
   visible =VG (Int_range.scale (fun _->0) 1 81);
   prerequisites = PL[];
   assumptions=AL[];
} ;;

let main_ref = ref origin ;; 

let set_and_show new_state = 
   let _ = (main_ref:=new_state;Enhanced_Grid.display new_state) in
   new_state ;; 

let initialize data = set_and_show({
   visible =VG data;
   prerequisites = PL[];
   assumptions=AL[];
} );;

let step () =
   let _ = Enhanced_Grid.display (!main_ref) in 
   Enhanced_Grid.step (!main_ref) ;; 

let enforce_all_current_deductions () =
    let new_state = Enhanced_Grid.enforce_all_current_deductions(!main_ref) in 
    set_and_show new_state ;;    

let cut_using_threshhold k =
  let new_state = Enhanced_Grid.cut_using_threshhold (!main_ref) k in 
  set_and_show new_state ;;   
  
  
let deduce_and_see i j = 
  let new_state = Enhanced_Grid.enforce_deduction_at_cell (!main_ref) (C(i,j)) in 
  let _ = (main_ref:=new_state;Enhanced_Grid.display new_state) in 
  Enhanced_Grid.step new_state ;; 
     

end ;;  


let c = Private.cut_using_threshhold ;; 
let d = Private.deduce_and_see ;; 
let i = Private.initialize ;;
let s = Private.step ;; 

end ;;  


