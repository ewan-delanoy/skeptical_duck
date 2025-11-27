(************************************************************************************************************************
Snippet 194 : 
************************************************************************************************************************)
open Skeptical_duck_lib ;; 
open Needed_values ;;


(************************************************************************************************************************
Snippet 193 : Code to read/write PARI-GP data between files
************************************************************************************************************************)
module Snip193=struct

let pari_dir = home ^ 
   "/Teuliou/Bash_scripts/Pari_Programming/my_pari_code/" ;;

let thorgal_dir = pari_dir ^ "Thorgal/" ;;

let prelude2_ap = Absolute_path.of_string (
   pari_dir ^ "prelude2.gp"
) ;;

let follenn3_ap = Absolute_path.of_string (
   pari_dir ^ "follenn3.gp"
) ;;

let brouilhed_ap = Absolute_path.of_string (
   pari_dir ^ "brouilhed.gp"
) ;;

let clean_prelude2 () = Lines_in_text.remove_lower_interval_in_file 
 prelude2_ap 18  ;;

let act () = Lines_in_text.remove_interval_in_file 
 follenn3_ap 12 12 ;; 

let cleanup_sheet (prefix,idx) = 
   let name = prefix ^ (string_of_int idx) in 
   let ap_for_reading = Absolute_path.of_string (thorgal_dir ^ name ^ ".gp") in 
   Io.overwrite_with ap_for_reading "" ;;

let transfer_new_data (prefix,idx) = 
   let name = prefix ^ (string_of_int idx) in 
   let ap_for_reading = Absolute_path.of_string (thorgal_dir ^ name ^ ".gp") in 
   let data = Io.read_whole_file ap_for_reading in 
   let text = "\n\n\nraw_"^name^"={"^data^"};\n\n\n" in 
   (Io.append_string_to_file text prelude2_ap;
    (* Io.overwrite_with ap_for_reading ""*) ) ;;

let tfd prefix idx = transfer_new_data (prefix,idx) ;;

(* let refresh_and_transfer_new_data l= 
  let _ = List.iter cleanup_sheet l in 
  List.iter transfer_new_data l ;; *)
  

let main_list = ref [
   "const",1;
   "const",2;
   "const",3;
   "const",4;
   "const",5;
   "const",6;
   "part",1;
   "part",4;
   "part",5;
   "part",10;
   "part",12;
   "part",15;
   "part",17;
   "part",20;
   "part",21;
   "part",22;
   "part",23;
   "part",25;
   "part",29;
   "part",30;
]  ;;

let clean_all () = (clean_prelude2();
List.iter cleanup_sheet (!main_list)) ;;

let transfer_all () = List.iter transfer_new_data (!main_list) ;;   



end ;;


(************************************************************************************************************************
Snippet 192 : Enumerating orbit of monomials, III
************************************************************************************************************************)
type sixtuple = SU of int * int * int * int * int * int ;;

let i_order = Total_ordering.for_integers ;;

let i_merge = Ordered.merge i_order ;;
let i_setminus = Ordered.setminus i_order ;;
let i_sort = Ordered.sort i_order ;;

let first_sixtuple = SU(0,0,0,0,0,0) ;; 

let next_sixtuple (SU(x1,x2,x3,x4,x5,x6)) =
    if x1>0
    then SU(x1-1,x2+1,x3,x4,x5,x6)
    else
    if x2>0
    then SU(x2-1,0,x3+1,x4,x5,x6)
    else 
    if x3>0
    then SU(x3-1,0,0,x4+1,x5,x6)
    else      
    if x4>0
    then SU(x4-1,0,0,0,x5+1,x6)
    else  
    if x5>0
    then SU(x5-1,0,0,0,0,x6+1)
    else SU(x6+1,0,0,0,0,0)      ;; 

let su_degree (SU(x1,x2,x3,x4,x5,x6)) = x1+x2+x3+x4+x5+x6 ;;

 let su_of_list l=
   let p = (fun k->List.nth l (k-1)) in 
   SU(p 1,p 2,p 3,p 4,p 5,p 6) ;;
  
  let su_to_list (SU(x1,x2,x3,x4,x5,x6))=
   [x1;x2;x3;x4;x5;x6] ;; 

let su_order = ((
    fun x y ->
      let (SU(x1,x2,x3,x4,x5,x6))=x 
      and (SU(y1,y2,y3,y4,y5,y6))=y in 
      let dx=su_degree x and dy=su_degree y in 
      let trial1 = Total_ordering.for_integers dx dy in 
      if trial1<>Total_ordering_result_t.Equal then trial1 else 
     (Total_ordering.lex_compare Total_ordering.for_integers)
     [x6;x5;x4;x3;x2;x1] [y6;y5;y4;y3;y2;y1]
  ): sixtuple Total_ordering_t.t);;
  
let su_sort = Ordered.sort su_order ;;

let first_state = (first_sixtuple,[]) ;;    

let next_state (u,l) = (next_sixtuple u,u::l) ;;

let state_ref = ref first_state ;;

let counter = ref 0 ;;

let step () =
   let _ = (
      state_ref:=next_state(!state_ref);
      counter:=(!counter)+1;
   ) in
   let msg=(string_of_int (!counter))^" treated\n" in 
   print_string msg;flush stdout ;; 

let act bound =
   let _ = (state_ref:=first_state) in 
   while (su_degree(fst(!state_ref))<=bound)
   do step () done ;;

Chronometer.it act 15 ;;   

let ball15 = List.rev(snd(!state_ref)) ;;

 let perm_action su original_perm =
    let perm = Permutation.inverse original_perm in 
    let inv_perm = (fun k->List.nth perm (k-1)) in 
    let old_l = su_to_list su in 
    let new_l = Int_range.scale (fun k->
      List.nth old_l ((inv_perm k)-1)  
    ) 1 6 in 
    su_of_list new_l ;; 
  
let is_in_wreath perm =
  let measure = (fun k->((Permutation.eval perm k)mod 2)=(k mod 2)) in 
  let choice = measure 1 in 
  List.for_all (fun k->measure k=choice) (Int_range.range 2 6) ;; 
let all_permutations = Permutation.iii 6 ;;    

let wreath = List.filter is_in_wreath all_permutations ;;

let nth_in_wreath idx = List.nth wreath (idx-1) ;; 

let maxie = Image.image nth_in_wreath [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 25; 26; 27; 28; 29; 30; 31; 32; 33;
   34; 35; 36; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58; 59; 60] ;;


(*[1; 4; 6; 7; 9; 12; 14; 15; 17; 20; 22; 23; 26; 27; 29; 32; 34; 35; 37; 40;
   42; 43; 45; 48; 49; 52; 54; 55; 57; 60; 62; 63; 65; 68; 70; 71] ;;
*)   

(*
[1; 4; 6; 7; 9; 12; 13; 16; 18; 19; 21; 24; 26; 27; 29; 32; 34; 35; 38; 39; 41;
 44; 46; 47; 49; 52; 54; 55; 57; 60; 61; 64; 66; 67; 69; 72]
;;
*)

let wreath_orbit su =
     su_sort(Image.image (perm_action su) wreath);;   
  
let maxie_orbit su =
     su_sort(Image.image (perm_action su) maxie);;      
let ball15_length =   List.length ball15 ;;   
let array_for_wreath_orbit_representatives = Array.make (ball15_length) 0 ;; 

let array_for_maxie_orbit_representatives = Array.make (ball15_length) 0 ;; 


let ivy_get k = List.nth ball15 (k-1) ;;
let ivy_index su = List_again.find_index_of_in su ball15 ;;


let ivy_wreath_orbit =Memoized.make(fun k -> 
   let sixtuple = List.nth ball15 (k-1) in 
   let su_orbit= wreath_orbit sixtuple in 
   i_sort(Image.image ivy_index su_orbit)
);;

let ivy_maxie_orbit =Memoized.make(fun k -> 
   let sixtuple = List.nth ball15 (k-1) in 
   let su_orbit= maxie_orbit sixtuple in 
   i_sort(Image.image ivy_index su_orbit)
);;

let rec helper_for_smallest_untreated_wreath_ivy_index k =
   if k > ball15_length then None else 
   if Array.get array_for_wreath_orbit_representatives (k-1)=0 
   then Some k 
   else helper_for_smallest_untreated_wreath_ivy_index (k+1) ;;   

let rec helper_for_smallest_untreated_maxie_ivy_index k =
   if k > ball15_length then None else 
   if Array.get array_for_maxie_orbit_representatives (k-1)=0 
   then Some k 
   else helper_for_smallest_untreated_maxie_ivy_index (k+1) ;;   


let smallest_untreated_wreath_ivy_index () =
    helper_for_smallest_untreated_wreath_ivy_index 1 ;;

let smallest_untreated_maxie_ivy_index () =
    helper_for_smallest_untreated_maxie_ivy_index 1 ;;


let wreath_step_with_no_message k =
   let orb = ivy_wreath_orbit k in 
   let representative = List.hd(List.rev orb) in 
   List.iter (fun t->Array.set 
     array_for_wreath_orbit_representatives (t-1)  representative
   ) orb ;; 

let maxie_step_with_no_message k =
   let orb = ivy_maxie_orbit k in 
   let representative = List.hd(List.rev orb) in 
   List.iter (fun t->Array.set 
     array_for_maxie_orbit_representatives (t-1)  representative
   ) orb ;;    
   
let wreath_step k =
   let msg = (string_of_int k)^" of "^(string_of_int(ball15_length))^
       " treated [wreath] \n" in 
   (
     wreath_step_with_no_message k;
     print_string msg;
     flush stdout
   )  ;;

let maxie_step k =
   let msg = (string_of_int k)^" of "^(string_of_int(ball15_length))^
       " treated [maxie] \n" in 
   (
     maxie_step_with_no_message k;
     print_string msg;
     flush stdout
   )  ;;


let walker = ref (Some 1) ;;

let wreath_act () = 
   let _ = (walker:=Some 1) in 
   while (!walker)<>None 
   do
     wreath_step (Option.get(!walker));
     walker:=smallest_untreated_wreath_ivy_index ()
done ;;

let maxie_act () = 
   let _ = (walker:=Some 1) in 
   while (!walker)<>None 
   do
     maxie_step (Option.get(!walker));
     walker:=smallest_untreated_maxie_ivy_index ()
done ;;

wreath_act ();;
maxie_act () ;;
let wreath_representatives = 
   i_sort(Array.to_list array_for_wreath_orbit_representatives) ;;

let maxie_representatives = 
   i_sort(Array.to_list array_for_maxie_orbit_representatives) ;;

let specific_maxie_representatives = 
   Explicit.filter 
   (fun m->
      (List.length(ivy_maxie_orbit m))<>(List.length(ivy_wreath_orbit m))
      ) maxie_representatives ;;

let maxie_minimizers =
    let temp = Explicit.image ivy_maxie_orbit 
    specific_maxie_representatives in 
    snd(Min.minimize_it_with_care List.length temp);;

let maxie_companion_in_index_form =
    List.hd maxie_minimizers
   (*[504; 518; 570; 591; 624; 629; 681; 716; 740; 754; 766; 782; 809; 818; 835;
    849; 858; 860] *)  ;; 

let maxie_companion_in_uple_form =
    Image.image (fun k->List.nth ball15 (k-1))
  maxie_companion_in_index_form  ;; 

let u4 = Int_uple.list_of_pairs 6 ;;
   
let u5 =  Cartesian.square u4;;

let u6 = List.filter_map (
 fun ((x1,x2),(x3,x4)) ->
   if List.length(i_sort([x1;x2;x3;x4]))=4
   then Some(x1,x2,x3,x4)
   else None    
) u5 ;;

let u7 = Image.image (
   fun u->
      let (x1,x2,x3,x4) =u in 
      let li = Int_range.scale (fun t->
         if List.mem t [x1;x2] then 2 else 
         if List.mem t [x3;x4] then 1 else 0     
         ) 1 6 in 
      (u,li)   
) u6 ;;

let u8 = List.filter (fun (u,li)->
   List.mem(su_of_list li) maxie_companion_in_uple_form 
   ) u7 ;;

let u9 = Image.image (fun 
    ((x1,x2,x3,x4),_) -> ((x1,x2),(x3,x4))
) u8 ;;

let u10 = Ordered.sort 
(Total_ordering.product i_order i_order)
(Image.image fst u9) ;;

let u11 = Ordered.sort 
(Total_ordering.product i_order i_order)
(Image.image snd u9) ;;

let u12 = Image.image (fun x->
   (x,List.filter (fun y->List.mem (x,y) u9) u11)) u10;;

let v1 = Image.image ivy_get maxie_companion_in_index_form ;;

let v_level v su=
   let l =su_to_list su in 
   List.filter_map (fun (i,v2)->if v2=v then Some i else None)
     (Int_range.index_everything l) ;;

let w_set k= if k mod 2=0 then [1;3;5] else [2;4;6] ;;

let viz_v1 k =
   let temp1 = List.filter (fun su ->
      let l =su_to_list su in List.nth l (k-1)=3
   ) v1 in 
   Image.image (
      fun k2 ->
      let temp2 = List.filter (fun su ->
      let l =su_to_list su in List.nth l (k2-1)=2
      ) temp1 in 
      Image.image (v_level 1) temp2
   )(w_set k) ;;


let v2 = List.filter (fun (SU(x1,x2,x3,x4,x5,x6)) ->x1=3) v1 ;;

(*
let wreath_representatives = i_sort(Array.to_list array_for_wreath_orbit_representatives) ;;

let diff = ball15_length - (List.length u1) ;;
*)



(************************************************************************************************************************
Snippet 191 : Enumerate all subgroups of a small group
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;

let i_merge = Ordered.merge i_order ;;
let i_setminus = Ordered.setminus i_order ;;
let i_sort = Ordered.sort i_order ;;
  
let il_order = Total_ordering.silex_for_intlists ;;

let il_mem = Ordered.mem il_order ;;
let il_merge = Ordered.merge il_order ;;
let il_setminus = Ordered.setminus il_order ;;
let il_sort = Ordered.sort il_order ;;
  
let is_in_wreath perm =
  let measure = (fun k->((Permutation.eval perm k)mod 2)=(k mod 2)) in 
  let choice = measure 1 in 
  List.for_all (fun k->measure k=choice) (Int_range.range 2 6) ;; 
let all_permutations = Permutation.iii 6 ;;    

let wreath = List.filter is_in_wreath all_permutations ;;

let indexed_wreath = Int_range.index_everything wreath ;;
  
let wreath_uncurried_prod =Memoized.make(fun (i,j) ->
   let perm_i = List.nth wreath (i-1) 
   and perm_j = List.nth wreath (j-1) in 
   let perm = Permutation.product perm_i perm_j in 
   List_again.find_index_of_in perm wreath);;

let computation1  = Explicit.image (
   wreath_uncurried_prod
) (Cartesian.square(Int_range.range 1 72)) ;;

let wreath_prod i j = wreath_uncurried_prod (i,j) ;;

let wreath_inverse  =Memoized.make(fun i ->
   let perm_i = List.nth wreath (i-1)  in 
   let perm = Permutation.inverse perm_i  in 
   List_again.find_index_of_in perm wreath);;

let computation2  = Explicit.image (
   wreath_uncurried_prod
) (Cartesian.square(Int_range.range 1 72)) ;;   

let rec helper_for_generated_subgroup (whole_so_far,not_treated_yet,seed) =
  if not_treated_yet=[] then whole_so_far else
  let temp1 = Cartesian.product not_treated_yet seed in 
  let temp2 = Image.image wreath_uncurried_prod temp1 in
  let new_elements = i_sort temp2 in 
  let new_whole = i_merge new_elements whole_so_far 
  and really_new_elements = i_setminus new_elements whole_so_far  in 
  helper_for_generated_subgroup (new_whole,really_new_elements,seed) ;;
  
let hashtbl_for_generating_sets = ((Hashtbl.create 500):
  (int list,int list) Hashtbl.t) ;;
  
let generated_subgroup =Memoized.make(fun seed ->
  let answer = helper_for_generated_subgroup ([1],[1],seed) in 
  let _=match Hashtbl.find_opt hashtbl_for_generating_sets answer with 
      (Some _)->()
      |None ->Hashtbl.add hashtbl_for_generating_sets answer seed
  in 
  answer);;
  
let sphere0=[[1]] ;;
let ball0=[[1]] ;;

let sphere0_with_generators = [[1],[]] ;;

let towards_sphere1=
  Int_range.scale (fun k->(k,generated_subgroup [k])) 2 72 ;;

let sphere1 =
   il_sort (Image.image snd towards_sphere1) ;;   
let ball1 = il_merge ball0 sphere1 ;;   

let distinguished_generator_for_sphere1 sg =
  fst(List.find (fun (k,sg2)->sg2=sg) towards_sphere1) ;;

let distinguished_families1 =
  i_sort (Image.image distinguished_generator_for_sphere1 sphere1) ;;

let sphere1_with_generators = 
  Image.image (fun sg->(sg,[distinguished_generator_for_sphere1 sg])) sphere1 ;;

let pre_distinguished_families2 =  
  let temp = Cartesian.product distinguished_families1
          distinguished_families1 in
  il_sort(List.filter_map (
   fun (x1,x2) ->
    if not(
      (List.mem x1 (generated_subgroup [x2])) ||
      (List.mem x2 (generated_subgroup [x1]))
    )
    then Some (i_sort [x1;x2])
    else None
  )  temp );;       

let towards_sphere2=
  List.filter_map (fun l->
    let sg = generated_subgroup  l in 
    if il_mem sg ball1 
    then None 
    else Some(l,sg)) pre_distinguished_families2 ;;  

let sphere2 =
   il_sort (Image.image snd towards_sphere2) ;;   
let ball2 = il_merge ball1 sphere2 ;;       

let distinguished_generator_for_sphere2 sg =
  fst(List.find (fun (l,sg2)->sg2=sg) towards_sphere2) ;;

let distinguished_families2 =
  il_sort (Image.image distinguished_generator_for_sphere2 sphere2) ;;

let sphere2_with_generators = 
  Image.image (fun sg->(sg,distinguished_generator_for_sphere2 sg)) sphere2 ;;


let pre_distinguished_families3 =  
  let temp = Cartesian.product distinguished_families2
          distinguished_families1 in
  il_sort(List.filter_map (
   fun (old_l,y3) ->
    if List.mem y3 old_l then None else
    let l=i_sort(y3::old_l) in 
    let x1 = List.nth l 0 
    and x2 = List.nth l 1 
    and x3 = List.nth l 2 in 
    if not(
      (List.mem x1 (generated_subgroup [x2;x3])) ||
      (List.mem x2 (generated_subgroup [x1;x3])) ||
      (List.mem x3 (generated_subgroup [x1;x2]))
    )
    then Some ([x1;x2;x3])
    else None
  )  temp );;       

let towards_sphere3=
  List.filter_map (fun l->
    let sg = generated_subgroup  l in 
    if il_mem sg ball2 
    then None 
    else Some(l,sg)) pre_distinguished_families3 ;;  

let sphere3 =
   il_sort (Image.image snd towards_sphere3) ;;   
let ball3 = il_merge ball2 sphere3 ;;        

let distinguished_generator_for_sphere3 sg =
  fst(List.find (fun (l,sg2)->sg2=sg) towards_sphere3) ;;

let distinguished_families3 =
  il_sort (Image.image distinguished_generator_for_sphere3 sphere3) ;;

let sphere3_with_generators = 
  Image.image (fun sg->(sg,distinguished_generator_for_sphere3 sg)) sphere3 ;;


let pre_distinguished_families4 =  
  let temp = Cartesian.product distinguished_families3
          distinguished_families1 in
  il_sort(List.filter_map (
   fun (old_l,y4) ->
    if List.mem y4 old_l then None else
    let l=i_sort(y4::old_l) in 
    let x1 = List.nth l 0 
    and x2 = List.nth l 1 
    and x3 = List.nth l 2 
    and x4 = List.nth l 3 in 
    if not(
      (List.mem x1 (generated_subgroup [x2;x3;x4])) ||
      (List.mem x2 (generated_subgroup [x1;x3;x4])) ||
      (List.mem x3 (generated_subgroup [x1;x2;x4])) ||
      (List.mem x4 (generated_subgroup [x1;x2;x3]))
      )
    then Some ([x1;x2;x3;x4])
    else None
  )  temp );; 

let towards_sphere4=
  List.filter_map (fun l->
    let sg = generated_subgroup  l in 
    if il_mem sg ball3 
    then None 
    else Some(l,sg)) pre_distinguished_families4 ;;    

let with_generators = List.flatten [
   sphere0_with_generators;
   sphere1_with_generators;
   sphere2_with_generators;
   sphere3_with_generators
] ;;

let standard_generating_set sg = List.assoc sg with_generators ;;

let all_subgroups = ball3 ;;    

let nonfull_subgroups = List.filter (fun l->List.length(l)<72) all_subgroups ;;

let maximal_subgroups = 
  Ordered_misc.maximal_elts_wrt_inclusion nonfull_subgroups ;;

let half_subgroups = List.filter (fun l->List.length l=36) 
  maximal_subgroups ;;  

let u1 = Image.image (
  fun sg -> (sg,standard_generating_set sg)
)  half_subgroups ;; 

let u2 = Ordered.fold_intersect i_order half_subgroups ;;

let u3 = standard_generating_set u2 ;;

let perm_in_wreath k=List.nth wreath (k-1) ;;

let describe_permutation_in_wreath k=
  let perm = List.nth wreath (k-1) in 
  (perm,Permutation.decompose_into_disjoint_cycles perm) ;;
   
let maxie = Image.image perm_in_wreath (List.nth half_subgroups 1) ;;

let minnie = Image.image perm_in_wreath (List.nth half_subgroups 2) ;;

let normie = Image.image perm_in_wreath (List.nth half_subgroups 0) ;;

let gpize_int_list l = "["^(String.concat "," 
(Image.image string_of_int l))^"]" ;;

let gpize_int_list_list l = "\n\n\n["^(String.concat "," 
(Image.image gpize_int_list l))^"]\n\n\n" ;;

print_string( gpize_int_list_list wreath);;

print_string( gpize_int_list_list maxie);;

print_string( gpize_int_list_list minnie);;

print_string( gpize_int_list_list normie);;
(************************************************************************************************************************
Snippet 190 : Producing some PARi-GP code enumerating 
some elementary symmetric polynomials
************************************************************************************************************************)

module Snip190=struct

let u1 = List_again.power_set (Int_range.range 1 10) ;;

let u2 = Ordered.sort 
 (Total_ordering.silex_compare Total_ordering.for_integers)
   u1 ;;

let partial_power = Memoized.make(fun k->
    List.filter (fun l->List.length l=k) u2
) ;;   

let soi k=if k=10 then "d" else string_of_int k ;;

let compress l = String.concat "" (Image.image soi l);;


let write_line l= 
 match l with 
 [] -> ""
 |a::others ->
   "h"^(compress l)^" = nz( h"^(soi a)^" * h"^(compress others)^" );";;

let write_set ll =
   let m = string_of_int(List.length(List.hd ll)) in 
   let temp1 = String.concat "\n" 
    (Image.image write_line ll) 
   and temp2 = "f"^m^" = "^(String.concat "+" 
    (Image.image (fun l->"h"^(compress l)) ll) )^" ;" in 
   "\n\n\n"^temp1^"\n\n"^temp2^"\n\n\n";;   

let write_level k = write_set(partial_power k) ;;

let full_text = 
    "\n\n\n"^(String.concat "\n" 
    (Int_range.scale write_level 2 10))^"\n\n\n" ;;

let ap1 = Absolute_path.of_string 
(home^"/Teuliou/Bash_scripts/Pari_Programming/my_pari_code/"^
"follenn1.gp") ;;
    
Io.append_string_to_file full_text ap1 ;;


end ;;


(************************************************************************************************************************
Snippet 189 : Code that reads PARI-GP arrays and writes PARI-GP functions
************************************************************************************************************************)
module Snip189=struct

let main_index = 8 ;;

let prelude_ap = Absolute_path.of_string 
(home^"/Teuliou/Bash_scripts/Pari_Programming/my_pari_code/"^
"prelude1.gp") ;;


(*
let clean_prelude () = Lines_in_text.remove_interval_in_file
  prelude_ap 32 74 ;;

*)  

let ap1 = Absolute_path.of_string 
(home^"/Teuliou/Bash_scripts/Pari_Programming/my_pari_code/"^
"brouilhed"^(string_of_int main_index)^".gp") ;;



let text1 = Io.read_whole_file ap1 ;;

let i1 = (String.index text1 '[')+1 ;;
let i2 = (String.index text1 ']')+1 ;;

let text2 = Cull_string.interval text1 (i1+1) (i2-1) ;;

let u1 = Str.split (Str.regexp ",") text2 ;;

let u2 = Image.image Cull_string.trim_spaces u1 ;;

let extract_from_u2 k =
   let start = 720 * (k-1) in 
   let temp = List_again.long_tail start u2 in 
   List_again.long_head 720 temp ;;

let u3 = Explicit.image extract_from_u2 
  (Int_range.range 1 720) ;;   

let check1 = ((List.flatten u3)=u2);;  
let check2 = (List.for_all (fun x->List.length x=720) u3);;  

let u4 = Ordered.sort Total_ordering.lex_for_strings u2 ;;

let rewrite_leftmost j x = 
  (if x="1" then "" else
   if x="-1" then "-" else  
   if String.contains x ' ' then "("^x^")*"
  else x^"*")^"a["^(string_of_int j)^"]" ;;

let rewrite_non_leftmost (j,x) = 
  (if x="1" then "+" else 
   if x="-1" then "-" else 
   if String.contains x ' ' then "+("^x^")*" else
   if String.starts_with x ~prefix:"-" 
   then x^"*" 
   else "+"^x^"*")^"a["^(string_of_int j)^"]" ;;

let rewrite_line l =
  let indexed_l = Int_range.index_everything l in 
  let temp1 = List.filter (fun (j,x)->x<>"0") indexed_l in
  match temp1 with 
  []->"0"
  |(j1,x1) :: others ->
     String.concat ""
     ((rewrite_leftmost j1 x1) :: 
     (Image.image rewrite_non_leftmost others)) ;; 

let u5 = Image.image rewrite_line u3 ;;     

let u6 = String.concat "," u5 ;;

let acronym = 
  if main_index = 7 then "hh" else 
  if main_index = 8 then "dh" else 
  "g"^(string_of_int main_index);;    

let u7 = "\n\n\ngomer_"^acronym^"_prod(a)=["^u6^"]\n\n\n" ;;

let u8 () = 
  let _ = Io.append_string_to_file u7 prelude_ap in 
  print_string ("\n\n\nmain_index = "^(string_of_int main_index)^"\n\n\n");;


end ;;


(************************************************************************************************************************
Snippet 188 : Enumerating orbits of monomials, II
************************************************************************************************************************)
module Snip188=struct


type sixtuple = SU of int * int * int * int * int * int ;; 

let su_order = ((
    fun (SU(x1,x2,x3,x4,x5,x6)) (SU(y1,y2,y3,y4,y5,y6)) ->
    let lx = [x6;x5;x4;x3;x2;x1]
    and ly = [y6;y5;y4;y3;y2;y1] in
    let trial1 = Total_ordering.for_integers 
    (Basic.fold_sum lx) (Basic.fold_sum ly) in 
    if trial1<>Total_ordering_result_t.Equal 
    then trial1
    else    
     (Total_ordering.lex_compare Total_ordering.for_integers)
     lx ly
  ): sixtuple Total_ordering_t.t);;
  
let su_sort = Ordered.sort su_order ;;

let positive_power_alone g k =
    if k=1 then g else g^"^"^(string_of_int k);;
  
let positive_power_in_group more_than_one (g,k) =
    if (k=1)||(not more_than_one) 
    then positive_power_alone g k
    else "("^(positive_power_alone g k)^")";; 

let su_to_list (SU(x1,x2,x3,x4,x5,x6))=
   [x1;x2;x3;x4;x5;x6] ;; 
  
   
  
  let g_list = Int_range.scale (fun k->
    "g"^(string_of_int k)) 1 6 ;;
  
  let g_monomial_of_su su =
    let temp1 = List.combine g_list (su_to_list su)  in 
    let temp2 = List.filter (fun (g,k)->k>0) temp1 in 
    if temp2=[] then "1" else
    let more_than_one = (List.length(temp2)>1) in 
    let temp3 = Image.image 
      (positive_power_in_group more_than_one) temp2 in 
    String.concat "*" temp3 ;;  
  
let g_monomial_list_of_su_list l =
    let temp1 = Image.image g_monomial_of_su l in 
    "["^(String.concat "," temp1)^"]";;
let ir = Int_range.range 0 ;;
let u1 = Cartesian.tproduct (ir 5) (ir 4) (ir 3) ;;
let u2 = Cartesian.tproduct (ir 2) (ir 1) (ir 0) ;;
let u3 = Cartesian.product u1 u2 ;;

let u4 = Image.image (fun 
  ((x1,x2,x3),(x4,x5,x6)) ->
    SU(x1,x2,x3,x4,x5,x6)
) u3 ;;

let u5 = su_sort u4 ;;

let u6 = g_monomial_list_of_su_list u5 ;;  

let u7 = "\n\n\n"^u6^"\n\n\n" ;;

let u8 () = print_string u7 ;;


end ;;


(************************************************************************************************************************
Snippet 187 : Using Manage_diary.extract_at_index_and_append_to_file
************************************************************************************************************************)
module Snip187=struct

Manage_diary.extract_at_index_and_append_to_file
  192 ~path_in_nongithubbed: "cloth" ;;

Manage_diary.extract_at_index_and_append_to_file
  188 ~path_in_nongithubbed: "jug" ;;  

let act1 () = Manage_diary.copy_file_content_as_new_snippet 
  ~path_in_nongithubbed: "cloth";;

let act2 () = Manage_diary.replace_whole_at_index_with_file_content 
  192 ~path_in_nongithubbed: "cloth";;

let act3 () = Manage_diary.replace_whole_at_index_with_file_content 
  191 ~path_in_nongithubbed: "pan";;  


end ;;


(************************************************************************************************************************
Snippet 186 : Musing over a functional equation
************************************************************************************************************************)
module Snip186=struct

let i_order = Total_ordering.for_integers ;;

let exhaustive_int_order = ((fun x y->
    let trial1 = Total_ordering.for_integers (abs x) (abs y) in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else    
    if x=y then Total_ordering_result_t.Equal else 
    if x<0 
    then Total_ordering_result_t.Greater
    else Total_ordering_result_t.Lower    
) : int Total_ordering_t.t) ;;

let exhaustive_rat_order = 
((fun (x1,x2) (y1,y2)->
    let mx=max(abs x1)(abs x2)
    and my=max(abs y1)(abs y2) in 
    let trial1 = Total_ordering.for_integers mx my in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else    
    let trial2 = Total_ordering.for_integers x2 y2 in 
    if trial2<>Total_ordering_result_t.Equal then trial2 else    
    exhaustive_int_order x1 y1   
) : (int * int) Total_ordering_t.t) ;;

let triple_rat_order = 
((fun (x1,x2,x3) (y1,y2,y3)->
    let mx=Ordered.max exhaustive_rat_order [x1;x2;x3]
    and my=Ordered.max exhaustive_rat_order [y1;y2;y3] in 
    let trial1 = exhaustive_rat_order mx my in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else    
    let trial2 = exhaustive_rat_order x1 y1 in 
    if trial2<>Total_ordering_result_t.Equal then trial2 else    
    let trial3 = exhaustive_rat_order x2 y2 in 
    if trial3<>Total_ordering_result_t.Equal then trial3 else        
    exhaustive_rat_order x3 y3  
) : ((int * int)*(int * int)*(int * int)) Total_ordering_t.t) ;;

let fourtuple_int_order = 
((fun (x1,x2,x3,x4) (y1,y2,y3,y4)->
    let mx=Ordered.max i_order (Image.image abs [x1;x2;x3;x4])
    and my=Ordered.max i_order (Image.image abs [y1;y2;y3;y4]) in 
    let trial1 = i_order mx my in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else    
    let trial2 = exhaustive_int_order x1 y1 in 
    if trial2<>Total_ordering_result_t.Equal then trial2 else    
    let trial3 = exhaustive_int_order x2 y2 in 
    if trial3<>Total_ordering_result_t.Equal then trial3 else        
    let trial4 = exhaustive_int_order x3 y3 in 
    if trial4<>Total_ordering_result_t.Equal then trial4 else            
    exhaustive_int_order x4 y4  
) : ((int * int * int * int)) Total_ordering_t.t) ;;

let bulky_order = Total_ordering.product 
   triple_rat_order fourtuple_int_order ;;


let reduce_fraction old_x old_y =
  let (x1,y1)=
     (if old_y<0 
      then (-old_x,-old_y) 
      else (old_x,old_y)) in 
  let g=Gcd.gcd x1 y1 in 
  (x1/g,y1/g) ;;

let sgn x = if x<0 then (-1) else 1 ;;

let reduce_mfraction old_x old_y =
    let (x1,y1)=reduce_fraction old_x old_y in
    if x1=0 then (x1,y1) else
    if abs(x1)<abs(y1)
    then let s=sgn(x1) in (s*y1,s*x1)
    else (x1,y1) ;;   

let main_map (x1,x2,y1,y2) =
    (
     reduce_mfraction (x1*y1) (x2*y2),
     reduce_mfraction (x1*y1+x2*y2) (x2*y1),
     reduce_mfraction (x1*y1+x2*y2) (x1*y2)
    ) ;;

let small_n = 3 ;;
let u1 = (Int_range.range (-small_n) (-1))@
(Int_range.range 1 small_n);;

let v1 = Cartesian.square u1 ;;

let v2 = List.filter (fun (x,y)->
    ((Gcd.gcd x y)=1)&&(y>0)
    ) v1;;

let u2 = Uple.list_of_pairs v2 ;;
let u3 = Image.image (fun ((a,b),(c,d))->(a,b,c,d)) u2 ;;

let u4 = Image.image (fun x->(main_map x,x)) u3 ;;

let u5 = Ordered.sort bulky_order u4 ;;

let u6 = Ordered.sort triple_rat_order (Image.image fst u5) ;;

let zrat_of_pair (x,y) = Zirath.Q.of_ints x y ;;

let u7 = Explicit.image (
   fun (a,b,c) ->
    let (x1,x2,y1,y2) = List.assoc (a,b,c) u5 in 
    ((zrat_of_pair a,zrat_of_pair b,zrat_of_pair c),
     (zrat_of_pair (x1,x2),zrat_of_pair (y1,y2))
    )
) u6 ;;

let domain1 = 
    Ordered.sort exhaustive_rat_order
    (Int_range.scale (fun x->(x,1)) (-3) 3)@[(-3,2);(3,2)] ;;


let domain_ref = ref domain1 ;;

let is_not_in_domain x= 
  let dom = !domain_ref in 
  let rat_dom = Image.image zrat_of_pair dom in 
  not(List.exists 
  (Zirath.Q.equals x) rat_dom );;

let initial_u8=Explicit.filter (
   fun ((a,b,c),_)->
    List.exists is_not_in_domain [a;b;c]
) u7;;

let u8_ref = ref initial_u8 ;;

let enlarge_domain_without_yielding_head p = 
    let old_dom = !domain_ref 
    and old_u8 = !u8_ref in  
    let new_dom = Ordered.insert exhaustive_rat_order p old_dom in 
    (domain_ref:=new_dom;
     u8_ref:= Explicit.filter (
   fun ((a,b,c),_)->
    List.exists is_not_in_domain [a;b;c]
) old_u8
    ) ;;

let enlarge_domain p = 
    let _= enlarge_domain_without_yielding_head p in 
    List.hd(!u8_ref);;

(*
enlarge_domain (4,1) ;;
enlarge_domain (-4,1) ;;
enlarge_domain (4,3) ;;
enlarge_domain (-4,3) ;;
enlarge_domain (5,2) ;;
enlarge_domain (-5,1) ;;
enlarge_domain (5,1) ;;
enlarge_domain (-5,2) ;;
enlarge_domain (5,3) ;;
enlarge_domain (-5,3) ;;
enlarge_domain (5,4) ;;
enlarge_domain (-5,4) ;;
enlarge_domain (6,1) ;;
enlarge_domain (-6,1) ;;
enlarge_domain (6,5) ;;
enlarge_domain (-6,5) ;;
*)


let uu k = List.nth (!u8_ref) (k-1) ;;


end ;;


(************************************************************************************************************************
Snippet 185 : Enumerating orbits of monomials
************************************************************************************************************************)
module Snip185=struct

  type sixtuple = SU of int * int * int * int * int * int ;; 

  let su_order = ((
    fun (SU(x1,x2,x3,x4,x5,x6)) (SU(y1,y2,y3,y4,y5,y6)) ->
     (Total_ordering.lex_compare Total_ordering.for_integers)
     [x1;x2;x3;x4;x5;x6] [y1;y2;y3;y4;y5;y6]
  ): sixtuple Total_ordering_t.t);;
  
  let su_sort = Ordered.sort su_order ;;
  
  let sul_order = (
    fun l1 l2 ->
      let trial1 = 
        Total_ordering.for_integers 
          (List.length l1) (List.length l2) in 
      if trial1<>Total_ordering_result_t.Equal  
      then trial1 
      else
      let trial2 = su_order (List.hd l1) (List.hd l2) in 
      if trial2<>Total_ordering_result_t.Equal  
      then trial2 
      else Total_ordering.standard l1 l2 
  ) ;;
  
  let sul_sort = Ordered.sort sul_order ;;
  
  let su_of_list l=
   let p = (fun k->List.nth l (k-1)) in 
   SU(p 1,p 2,p 3,p 4,p 5,p 6) ;;
  
  let su_to_list (SU(x1,x2,x3,x4,x5,x6))=
   [x1;x2;x3;x4;x5;x6] ;; 
  
  let positive_power_alone g k =
    if k=1 then g else g^"^"^(string_of_int k);;
  
  let positive_power_in_group more_than_one (g,k) =
    if (k=1)||(not more_than_one) 
    then positive_power_alone g k
    else "("^(positive_power_alone g k)^")";;  
  
  let g_list = Int_range.scale (fun k->
    "g"^(string_of_int k)) 1 6 ;;
  
  let g_monomial_of_su su =
    let temp1 = List.combine g_list (su_to_list su)  in 
    let temp2 = List.filter (fun (g,k)->k>0) temp1 in 
    let more_than_one = (List.length(temp2)>1) in 
    let temp3 = Image.image 
      (positive_power_in_group more_than_one) temp2 in 
    String.concat "*" temp3 ;;  
  
  let g_polynomial_of_su_list l =
    let temp1 = Image.image g_monomial_of_su l in 
    String.concat "+" temp1 ;;
  
  let g_list_of_su_list_list l = 
    "["^(
     String.concat ","  
    (Image.image  g_polynomial_of_su_list l))^"]";;  
  
  
  let tf6 k = [k] ;;
  
  let tf5 k = 
    let temp = Int_range.scale (
      fun j->Image.image (
       fun t6->(j,t6)
      )(tf6 (k-j))
    ) 0 k in 
    List.flatten temp;;
  
  let tf4 k = 
    let temp = Int_range.scale (
        fun j->Image.image (
         fun (t5,t6)->(j,t5,t6)
        )(tf5 (k-j))
    ) 0 k in 
    List.flatten temp;;  
  
  let tf3 k = 
    let temp = Int_range.scale (
          fun j->Image.image (
           fun (t4,t5,t6)->(j,t4,t5,t6)
          )(tf4 (k-j))
      ) 0 k in 
    List.flatten temp;;  
    
  let tf2 k = 
    let temp = Int_range.scale (
      fun j->Image.image (
        fun (t3,t4,t5,t6)->(j,t3,t4,t5,t6)
            )(tf3 (k-j))
    ) 0 k in 
    List.flatten temp;;  
  
  let tf1 k = 
    let temp = Int_range.scale (
        fun j->Image.image (
          fun (t2,t3,t4,t5,t6)->SU(j,t2,t3,t4,t5,t6)
              )(tf2 (k-j))
      ) 0 k in 
    List.flatten temp;;
  
  let all_perms=Permutation.iii 6 ;;
  
  let eq_mod_2 x y = ((abs(x-y)) mod 2=0) ;;
  
  let is_in_wreath perm = 
    let p = (fun k->List.nth perm (k-1)) in 
    let q = (fun k->(abs(p(k)-k) mod 2)) in 
    List.for_all (fun i->q i=q 1) (Int_range.range 1 6);; 
  
  let wreath = List.filter is_in_wreath all_perms ;;  
  
  let perm_action su original_perm =
    let perm = Permutation.inverse original_perm in 
    let inv_perm = (fun k->List.nth perm (k-1)) in 
    let old_l = su_to_list su in 
    let new_l = Int_range.scale (fun k->
      List.nth old_l ((inv_perm k)-1)  
    ) 1 6 in 
    su_of_list new_l ;; 
  
  let orbit su =
     su_sort(Image.image (perm_action su) wreath);;   
  
  let all_orbits =Memoized.make(fun n ->
    let all_monomials = tf1 n in 
    let temp1 = Explicit.image orbit all_monomials in 
    let all_heads = su_sort(Image.image List.hd temp1) in 
    let temp2 = Explicit.image orbit all_heads in
    sul_sort temp2);;
     
  let main n = 
    let temp1 = all_orbits n in 
    let msg = g_list_of_su_list_list temp1 in 
    let wrapped_msg = "\n\n\n" ^ msg ^ "\n\n\n" in 
    print_string wrapped_msg ;;

end ;;


(************************************************************************************************************************
Snippet 184 : Using Coherent_pdf.many_pngs_one_pdf 
************************************************************************************************************************)
module Snip184=struct

  let ap1 = Absolute_path.of_string "~/Downloads/Arno" ;; 
  
  let s_dir1 =  Absolute_path.to_string ap1 ;;
  let dir1 = Directory_name.of_string s_dir1 ;;  
  
  let u1 = Unix_again.beheaded_simple_ls dir1 ;;
  
  let u2 = Image.image (
    fun x ->
      let y = Cull_string.before_rightmost x '.' in 
      "convert "^x^" "^y^".pdf"
  ) u1 ;;
  
  let act0 () = Sys.chdir s_dir1 ;;
  
  let act1 () = Image.image Sys.command u2 ;;
  
  let act2 () = 
    Coherent_pdf.many_pngs_one_pdf 
    dir1 ~outputfile_name:"diglok" ;;
    


end ;;


(************************************************************************************************************************
Snippet 183 : Musings on p-chains problem, II
************************************************************************************************************************)
module Snip183=struct

  let i_order = Total_ordering.for_integers ;;

  let i2_order = 
    Total_ordering.product i_order i_order ;;
  
  let i2_insert = Ordered.insert i2_order ;;
    
  let u_order = ((fun x y ->
  let (x1, x2, x3, x4, (x5, x6), (x7, x8, x9)) = x 
  and (y1, y2, y3, y4, (y5, y6), (y7, y8, y9)) = y in  
  let trial1 = Total_ordering.silex_for_strings x7 y7 in 
  if trial1<>Total_ordering_result_t.Equal then trial1 else
  let trial2 = Total_ordering.for_integers x8 y8 in 
  if trial2<>Total_ordering_result_t.Equal then trial2 else 
  let trial3 = Total_ordering.silex_for_strings x1 y1 in 
  if trial3<>Total_ordering_result_t.Equal then trial3 else 
      Total_ordering.standard x y   
  ): (string * int * int * int * (string * int) * (string * int * int)) Total_ordering_t.t);;
  
  let u_fold_merge = Ordered.fold_merge u_order ;;
  let u_sort = Ordered.sort u_order ;;
  
  exception Inconsistency_exn of (int * int * int *int) list ;;
  exception Extract_from_partial_exn ;;
  exception Data_overflow ;;
  
  type a_or_b = A | B ;; 
  
  type ab_system = {
     depth : int ;
     decided_values : (int * int) list;
  } ;;
  
  type ab_result = 
    Complete of int |Partial of  int*(a_or_b list) ;;
  
  let string_to_ab_list str=
    List.rev_map (fun c->if c='A' then A else B) 
    (Strung.explode str);;
  let string_of_ab_list l=
    String.concat ""
    (List.rev_map (fun c->if c=A then "A" else "B") l);;
  
  
  
  let depth_of_int x = (x+3-(x mod 3))/3 ;;
  let a_eval x = if (x mod 3)=0 then x-2 else x+1 ;;
  let a_inverse x = if (x mod 3)=1 then x+2 else x-1 ;;
  
  let b_eval_opt syst x = List.assoc_opt x syst.decided_values ;;
  
  let rec extract_from_ab_list (x,l)= match l with  
    []->(string_of_ab_list l,x)
    |w::others ->
      if w=B then (string_of_ab_list l,x) else
        extract_from_ab_list (a_inverse x,others)   ;;
  
  let extract_from_partial ab= match ab with  
    Partial(x,l)->((string_of_ab_list l,x),
      extract_from_ab_list (x,l))
    |Complete _ -> raise Extract_from_partial_exn ;;
  
  
  let rec simplify_in_right_first_notation triple=
    let (x,l,y) = triple in 
    match l with 
    [] -> triple 
    |w::others -> 
      if w=B then triple else 
      simplify_in_right_first_notation (x,others,a_inverse y) ;;
  
  let simplify_in_left_first_notation (x,l,y) =
    let (x2,l2,y2)=simplify_in_right_first_notation (x,List.rev l,y) in 
    (x2,List.rev l2,y2) ;;
   
  
  let sift_from_partial ab= match ab with  
    Partial(x,l)->(x,l)
    |Complete _ -> raise Extract_from_partial_exn ;;
  
  
  let rec helper_for_eval syst x l=
    match l with 
    []-> Complete x 
    |w::others -> 
      (
        match w with 
        A ->  helper_for_eval syst (a_eval x) others 
        |B ->
        (
          match b_eval_opt syst x with 
           Some(y)-> helper_for_eval syst y others
           |None -> Partial(x,l)
        )   
      ) ;;
  
  let eval_at_point syst str x = helper_for_eval syst x 
    (string_to_ab_list str) ;;    
  
  let decided_pairs syst str=   
    let n = 3 * syst.depth in 
    let base = Int_range.range 1 n in 
    List.filter_map (
     fun x->match eval_at_point syst str x with 
     Partial(_,_)->None 
     |Complete y -> Some(x,y)
    ) base ;;
  
  let new_deductions_from_actor syst str=   
    let pairs = decided_pairs syst str in 
    let temp2 = Cartesian.square pairs in 
    let temp3 = List.filter_map(
      fun ((x1,y1),(x2,y2))->
        if x2=y1 then Some(x1,x2,y2) else None
    ) temp2 in 
    let temp4 = List.filter_map(
      fun (x1,x2,x3)->
        match List.assoc_opt x3 pairs with 
        None -> None 
        |Some(y1)-> if y1=x1 then None else Some(x1,x2,x3,y1)
    ) temp3 in 
    if temp4<>[]
    then raise(Inconsistency_exn(temp4))
    else
      u_sort(
      List.filter_map(
        fun (x1,x2,x3)->
          match List.assoc_opt x3 pairs with 
          Some(y1)-> None
          |None -> 
            let (x4,l4)= sift_from_partial(eval_at_point syst str x3) in 
            let (x5,l5,y5)=simplify_in_left_first_notation (x4,l4,x1) in  
            Some(str,x1,x2,x3,
              (string_of_ab_list l4,x4),(string_of_ab_list l5,x5,y5)
            ) 
         
      ) temp3) 
    ;;  
  
  let new_deductions_from_actors syst l_str=
  u_fold_merge (Image.image (new_deductions_from_actor syst) l_str) ;;   
  
  
  let impose_new_pair syst x y = 
    let new_depth = Max.list 
     [syst.depth;depth_of_int x;depth_of_int y] 
    and new_pairs = i2_insert (x,y) syst.decided_values in 
    {
      depth=new_depth;
      decided_values=new_pairs;
    }  ;;
  
  let impose_standard syst x =
    impose_new_pair syst x (3*syst.depth+1) ;;
  
  
  
  let triangle_analysis_for_actor_at_point syst actor k=
    match eval_at_point syst actor k with 
    (Partial(_,_)) -> Some[k]
    |Complete k2 ->
      (
        match eval_at_point syst actor k2 with 
        (Partial(_,_)) -> Some[k;k2]
        |Complete _ -> None
      ) ;; 
  
  let rec helper_in_triangle_analysis_for_actor syst actor k =
    if k>(3*syst.depth)  
    then raise Data_overflow (* we do not expect this to happen *)
    else 
    match triangle_analysis_for_actor_at_point syst actor k with 
    Some l -> (k,l)
    |None -> helper_in_triangle_analysis_for_actor syst actor (k+1);;
  
  let triangle_analysis_for_actor syst actor =
      (actor,helper_in_triangle_analysis_for_actor syst actor 1);;
      
  let triangle_analysis_for_actors syst actors =
    let temp1 = Image.image (triangle_analysis_for_actor syst) actors in  
    fst(Min.minimize_it (function (_,(k,_))->k) temp1);;    
  
  let actors =  ["B"; "AB"; "BA"; "AAB"; "ABA"; "ABB"; "BAA"; "BAB"; "BBA"; "AABB";
    "ABBA"; "BAAB"; "BBAA"; "AABAB"; "ABAAB"; "ABABA"; "ABABB"; "ABBAB";
    "BAABA"; "BABAA"; "BABBA"; "BBABA"; "AABABB"; "AABBAB"; "ABAABB"; "ABABBA";
    "ABBAAB"; "ABBABA"; "BAABBA"; "BABBAA"; "BBAABA"; "BBABAA"; "AABAABB";
    "AABBAAB"; "AABBABB"; "ABAABBA"; "ABBAABA"; "ABBAABB"; "ABBABBA"; "BAABBAA";
    "BBAABAA"; "BBAABBA"; "BBABBAA"] ;;      
  
  let origin_syst={
    depth=2;
    decided_values=[1,4];
  }  ;;
  
  let main_ref = ref origin_syst ;;
  
  let deds () = new_deductions_from_actors (!main_ref) actors ;;
  let see () = triangle_analysis_for_actors (!main_ref) actors ;;
  
  let eval str x= eval_at_point (!main_ref) str x ;;
  let set x y = 
    (main_ref:=impose_new_pair (!main_ref) x y);;
  let usual x = 
    (main_ref:=impose_standard (!main_ref) x );;
  
  usual 4 ;; (* to finish B: 1->4 *)
  set 7 1 ;; (* deduced from B: 1->4->7 *)
  usual 5 ;; (* to finish AB: 1->5 *)
  set 11 3 ;; (* deduced from AB: 1->5->11 *)
  usual 2 ;; (* to finish BA: 1 *)
  set 14 9 ;; (* deduced from AB: 7->2->14 *)
  set 15 12 ;; (* deduced from AAB: 11->2->15 *)
  usual 6 ;; (* to finish AAB: 1 -> 6 *)
  set 21 2 ;; (* deduced from AAB: 1->6->21 *)
  set 13 21 ;; (* deduced from AAB: 21->2->13 *)
  usual 8 ;; (* to finish ABB: 1 -> 8 *)
  set 26 6 ;; (* deduced from AB: 4->8->26 *)
  set 12 27 ;; (* deduced from AAB: 26->5->12 *)
  set 19 26 ;; (* deduced from B: 26->6->19 *)
  set 27 15 ;; (* deduced from B: 15->12->27 *)
  usual 25 ;; (* to finish ABB: 1 -> 8 *)
  set 31 8 ;; (* deduced from B: 8->25->31 *)
  set 3 32 ;; (* deduced from AAB: 31->7->13 *)
  set 32 11 ;; (* deduced from B: 11->3->32 *)
  set 33 20 ;; (* deduced from AB: 21->3->33 *)
  usual 10 ;; (* to finish BAB: 1 -> 10 *)
  
  
  (*
  
  
  let syst = (!main_ref) ;;
  
  let bad1 () = eval_at_point syst "AB" 5;;
  
  let bad1 () = helper_for_eval syst 5 [B] ;;
  
  let bad2 () = Image.image (new_deductions_from_actor syst) actors ;;
  
  let bad3 () = new_deductions_from_actor syst "B" ;;
  
  *)

end ;;


(************************************************************************************************************************
Snippet 182 : Computing spheres and balls of strings
************************************************************************************************************************)
module Snip182=struct

  let s_order = Total_ordering.silex_for_strings ;;

let s_fold_merge = Ordered.fold_merge s_order ;;
let s_sort = Ordered.sort s_order ;;

let a_list = ["";"A";"AA"] ;;
let b_list = ["";"B";"BB"] ;;

let alternating n =
   Int_range.scale (fun
    k->if k mod 2=1 then a_list else b_list 
   ) 1 n ;;

let alternating_product n=
  Cartesian.general_product (alternating n) ;;   

let sphere n = s_sort(
   Image.image (String.concat "")
   (alternating_product n)
)  ;;

let ball n =
   let spheres= Int_range.scale sphere 1 n in 
   s_fold_merge spheres ;;

let is_a_square s =
   let n = String.length s in 
   if n mod 2 =1 then false else 
   let m = (n/2) in 
   (Cull_string.beginning m s)=(Cull_string.ending m s)   ;;

let conjugation_relation s t =
   ((String.starts_with s ~prefix:t)
   &&(String.ends_with s ~suffix:(t^t))) ||
   ((String.starts_with s ~prefix:(t^t))
   &&(String.ends_with s ~suffix:t)) ;;

let is_a_conjugate s =
   if String.length(s)<3 then false else 
   List.exists(conjugation_relation s)["A";"B"] ;;

let u1 = ball 5 ;;   

let u2 = List.filter (
  fun x->(x<>"")
  &&(not(Substring.is_a_substring_of "AAA" x))
  &&(not(Substring.is_a_substring_of "BBB" x))
) u1 ;;

let u3 = List.filter (fun x->not(is_a_square x)) u2 ;;

let u4 = List.filter (fun x->not(is_a_conjugate x)) u3 ;;



end ;;


(************************************************************************************************************************
Snippet 181 : Musings on p-chains problem
************************************************************************************************************************)
module Snip181=struct

  let dec = Permutation.decompose_into_disjoint_cycles ;;
  let p = Permutation.product ;;
  
  let fp = Permutation.fold_product ;;
  
  let pow = Permutation.power ;;
  
  let n1 = 25 ;;
  let base = Int_range.range 1 n1 ;;
  
  let measure sigma =
     if sigma = base 
     then n1*1000 
     else
     if List.exists (fun x->List.nth sigma (x-1)=x) base
     then 1 
     else
     let cycles = Permutation.decompose_into_disjoint_cycles sigma in 
     snd(Min.minimize_it List.length cycles) ;;  
  
  
  
  
  let u1 = Int_range.scale 
     (fun k->Int_range.range (5*k-4) (5*k)) 1 5 ;;
  
  let alf = Permutation.product_of_cycles u1 ;;
  
  let u2 = Int_range.scale 
  (fun k->
    if k=2 then [2; 12; 7; 17; 22] else 
    Int_range.scale (fun t->5*t-(5-k)) 1 5 ) 1 5 ;;
  
  let beth = Permutation.product_of_cycles u2 ;;
  
  let c2 = Cartesian.square(Int_range.range 0 4) ;;
  
  let pre_res2 = Explicit.image (
     fun (i,j)->
      let sigma = p (pow alf i) (pow beth j) in 
      ((i,j),measure sigma)
  ) c2 ;;
  
  let res2 = Min.minimize_it_with_care snd pre_res2 ;;
  
  let c3 = Cartesian.cube(Int_range.range 0 4) ;;
  
  let pre_res3 = Explicit.image (
     fun (i1,i2,i3)->
      let sigma = fp [pow alf i1; pow beth i2;pow alf i3] in 
      ((i1,i2,i3),measure sigma)
  ) c3 ;;
  
  let res3 = Min.minimize_it_with_care snd pre_res3;;
  
  let c4 = 
    let temp=Cartesian.square c2 in 
    Image.image (fun ((i1,i2),(i3,i4)) -> 
      ((i1,i2,i3,i4))
      ) temp;;
  
  let pre_res4 = Explicit.image (
     fun (i1,i2,i3,i4)->
      let sigma = fp 
      [pow alf i1; pow beth i2;pow alf i3; pow beth i4] in 
      ((i1,i2,i3,i4),measure sigma)
  ) c4 ;;
  
  let res4 = Min.minimize_it_with_care snd pre_res4;;
  
  let res5 = List.filter (
     fun (i1,i2,i3,i4)->
      let sigma = fp 
      [pow alf i1; pow beth i2;pow alf i3; pow beth i4] in 
      (List.hd(sigma)=1)&&(sigma<>base)
  ) c4 ;;
  
  let ialf= pow alf 4 ;;
  
  let ibeth= pow beth 4 ;;
  
  let gam = fp [alf;beth;ialf;ibeth] ;;


end ;;


(************************************************************************************************************************
Snippet 180 : Musings on permutations, II
************************************************************************************************************************)
module Snip180=struct

  let i_order = Total_ordering.for_integers ;;

  let i_intersect = Ordered.intersect i_order ;;
  let i_merge = Ordered.merge i_order ;;
  let i_setminus = Ordered.setminus i_order ;;
  let i_sort = Ordered.sort i_order ;;
  
  let il_order = Total_ordering.silex_for_intlists ;;
  
  let il_merge = Ordered.merge il_order ;;
  let il_setminus = Ordered.setminus il_order ;;
  let il_sort = Ordered.sort il_order ;;
  
  let n = 8 ;;    
  let pairs = List.filter (fun (x,y)->x<>y) 
    (Cartesian.square(Int_range.range 1 n));;
  
  let (blue_pairs,red_pairs) = 
  List.partition(fun (x,y)->(x-y)mod 2<>0) pairs;;  
  
  let preserves_blue_pairs perm =
      let evp = (fun k->List.nth perm (k-1)) in 
      (List.for_all (fun (x,y)->
         List.mem (evp x,evp y) blue_pairs    
      ) blue_pairs) ;;
  
  let all_permutations = Permutation.iii n ;;    
  
  let all_indices = Int_range.range 1 (List.length all_permutations) ;;
  let indexed_all = List.combine all_indices all_permutations ;;
  
  let indexed_wreath = List.filter (fun
   (idx,perm)->preserves_blue_pairs perm) indexed_all ;;
  
  let wreath = Image.image fst indexed_wreath;;
  
  let uncurried_prod = Memoized.make (fun (i,j)->
      let pi = List.nth all_permutations (i-1)
      and pj = List.nth all_permutations (j-1) in 
      let pk = Permutation.product pi pj in 
      List_again.find_index_of_in pk all_permutations
  );;
  
  let prod i j = uncurried_prod (i,j) ;;
  
  let dot x l = i_sort(Image.image(prod x) l) ;;
  
  let tau_permutation = Int_range.scale 
  (fun k->if k>2 then k else 3-k) 1 n ;;
  
  let tau_index = List_again.find_index_of_in 
    tau_permutation all_permutations ;;
  
  let boar = dot tau_index wreath ;;  
  
  let boar_transform =Memoized.make(fun x -> dot x boar) ;;
  
  let unordered_transforms = Explicit.image boar_transform wreath;;
  
  let all_transforms = il_sort unordered_transforms ;;
  let all_representative_indices = i_sort(Image.image(
     fun bt ->
        List.find (fun w->boar_transform w=bt) wreath 
  ) all_transforms) ;;
  let all_representatives = Image.image(
     fun i ->
        let perm = List.nth all_permutations (i-1) in 
        let cycles = Permutation.decompose_into_disjoint_cycles perm in 
        (perm,cycles) 
  ) all_representative_indices ;;

  let tau_permutation = Int_range.scale 
(fun k->if k>2 then k else 3-k) 1 n ;;

let p_index x = 0;;
let tau_index = p_index tau_permutation ;;
(* let thetaa_permutation = Int_range.scale 
  (fun k->if k<n-1 then k else 2*n-1-k) 1 n ;; *)

let thetaa_permutation = [3;5;2;4;1;7;6;8];;  
let thetaa_index = p_index thetaa_permutation ;;  

let inv x = 0;;

let p_get x= [] ;;
let main_computation = Explicit.image (
   fun beta ->
      let inv_tb = inv (prod tau_index beta) in 
      let alf = prod thetaa_index inv_tb in 
      (alf,(p_get alf,p_get beta)) 
) wreath ;;



let main_result = List.filter_map (
   fun (alf,(perm1,perm2)) ->
      if List.mem alf wreath 
      then Some(perm1,perm2) 
      else None   
) main_computation ;;

let preserves_parity x = false ;;

let main_result2 = List.filter (
   fun (x,y)->
     (preserves_parity x) && 
     (preserves_parity y)
) main_result ;;


let squash l = String.concat "" (Image.image string_of_int l);;
let u1 = Image.image (fun (x,y)->
   (squash x)^","^(squash y)
   ) main_result2;;

let u2 = "\n\n\n"^(String.concat "\n" u1)^"\n\n\n" ;;   

print_string u2;;


end ;;


(************************************************************************************************************************
Snippet 179 : Musings on permutations
************************************************************************************************************************)
module Snip179=struct

  let i_order = Total_ordering.for_integers ;;

  let i_merge = Ordered.merge i_order ;;
  let i_setminus = Ordered.setminus i_order ;;
  let i_sort = Ordered.sort i_order ;;
  
  let il_order = Total_ordering.silex_for_intlists ;;
  
  let il_merge = Ordered.merge il_order ;;
  let il_setminus = Ordered.setminus il_order ;;
  let il_sort = Ordered.sort il_order ;;
  
  let pairs = List.filter (fun (x,y)->x<>y) 
    (Cartesian.square(Int_range.range 1 6));;
  
  let (blue_pairs,red_pairs) = 
  List.partition(fun (x,y)->(x-y)mod 2<>0) pairs;;  
  
  let preserves_blue_pairs perm =
      let evp = (fun k->List.nth perm (k-1)) in 
      (List.for_all (fun (x,y)->
         List.mem (evp x,evp y) blue_pairs    
      ) blue_pairs) ;;
  
  let s6_permutations = Permutation.iii 6 ;;    
  
  let s6 = Int_range.range 1 720 ;;
  let indexed_s6 = Int_range.index_everything s6_permutations ;;
  
  let indexed_wreath = List.filter (fun
   (idx,perm)->preserves_blue_pairs perm) indexed_s6 ;;
  
  let wreath = Image.image fst indexed_wreath;;
  
  let uncurried_s6_prod = Memoized.make (fun (i,j)->
      let pi = List.nth s6_permutations (i-1)
      and pj = List.nth s6_permutations (j-1) in 
      let pk = Permutation.product pi pj in 
      List_again.find_index_of_in pk s6_permutations
  );;
  
  let rec helper_for_generated_subgroup (whole_so_far,not_treated_yet,seed) =
      if not_treated_yet=[] then whole_so_far else
      let temp1 = Cartesian.product not_treated_yet seed in 
      let temp2 = Image.image uncurried_s6_prod temp1 in
      let new_elements = i_sort temp2 in 
      let new_whole = i_merge new_elements whole_so_far 
      and really_new_elements = i_setminus new_elements whole_so_far  in 
      helper_for_generated_subgroup (new_whole,really_new_elements,seed) ;;
  
  let hashtbl_for_generating_sets = ((Hashtbl.create 500):
  (int list,int list) Hashtbl.t) ;;
  
  let generated_subgroup =Memoized.make(fun seed ->
      let answer = helper_for_generated_subgroup ([1],[1],seed) in 
      let _=match Hashtbl.find_opt hashtbl_for_generating_sets answer with 
      (Some _)->()
      |None ->Hashtbl.add hashtbl_for_generating_sets answer seed
       in 
      answer);;
  
  let rec helper_for_greedy_generating_set (sg,accu,generated_by_accu)=
    let others=i_setminus sg generated_by_accu in 
    match others with 
    [] ->accu
    |x:: _ ->
     let new_accu = i_merge accu [x] in 
     let new_subgroup = generated_subgroup new_accu in 
     helper_for_greedy_generating_set (sg,new_accu,new_subgroup);;
  
  let greedy_generating_set sg = 
      helper_for_greedy_generating_set (sg,[1],[1]);;
  
  let generating_set sg = 
      match Hashtbl.find_opt hashtbl_for_generating_sets sg with 
      (Some old_answer)->old_answer
      |None -> greedy_generating_set sg ;;
  
  
  let hashtbl_for_wenerating_sets = ((Hashtbl.create 500):
      (int list,int list) Hashtbl.t) ;;
      
  let wenerated_subgroup =Memoized.make(fun seed ->
     let total_seed = i_merge seed wreath in
     let answer = generated_subgroup total_seed in 
     let _=match Hashtbl.find_opt hashtbl_for_wenerating_sets answer with 
      (Some _)->()
      |None ->Hashtbl.add hashtbl_for_wenerating_sets answer seed
     in 
     answer);;
  
  let outside_wreath= i_setminus s6 wreath;;
  
  let u1 = Explicit.image (fun z->(z,wenerated_subgroup [z])) 
  outside_wreath;;
  
  let u2 = il_sort (Image.image snd u1) ;;
  
  let u3 =Cartesian.square wreath ;;
  
  let s6_prod i j = uncurried_s6_prod (i,j);;
  
  let u4 = Explicit.image (fun (i,j)->
     ((i,j),s6_prod i (s6_prod 121 j))
  ) u3;;
  
  let realizers = Memoized.make(fun k->
     List.filter_map (fun ((i,j),k2)->if k2=k then Some(i,j) else None) u4
  ) ;;
  
  
  
  
  let u5 = List.filter (fun (idx,perm)->
     let get = (fun t->List.nth perm (t-1)) in 
     (get 1,get 2)=(3,5)
     )indexed_s6 ;;
  
  let u6 = Image.image (
     fun (x,y)->(x,y,realizers x)
  ) u5 ;;
   
  let u7 = Image.image (
     fun (x,y)->(x,y,realizers x)
  ) u5 ;;
  
  let tau k = if k>2 then k else 3-k ;;
  
  let v1 = List.filter (fun (x,y)->List.mem (tau x,tau y) red_pairs) red_pairs ;;


end ;;


(************************************************************************************************************************
Snippet 178 : Computing explicit successive minimal polynomials
on a field extension whose Galois group is a wreath
************************************************************************************************************************)
module Snip178=struct


  let completions l =
    let admissibles = (match l with
      [] -> Int_range.range 1 5
      |a::_ ->Int_range.range 1 a
    ) in 
    Image.image(fun i->i::l) admissibles ;;
  
  let sphere = Memoized.recursive(
      fun old_f k->
      if k<1 then [[]] else 
      let temp1=old_f(k-1) in
      let temp2=Image.image completions temp1 in 
      List.flatten temp2
  );;
  
  let ball = Memoized.make(fun 
     k->List.flatten (Int_range.scale sphere 0 k)
  ) ;;
  
  let exponent uple k= List.length(List.filter(fun x->x=k) uple);;
  
  let is_good uple = List.for_all (fun k->
    (exponent uple k)<=6-k    
  ) (Int_range.range 1 5) ;;
  
  let u1 = ball 15;;
  let u2 = List.filter is_good u1;;
  
  let sathenay_order l1 l2 =
      (Total_ordering.lex_compare Total_ordering.for_integers)
      (List.rev l1) (List.rev l2) ;;
  
  let sathenay_base = Ordered.sort sathenay_order u2;;    
  
  let indexed_sathenay_base= Int_range.index_everything sathenay_base;;
  
  let restricted1_base = List.filter (fun (i,l)->
      (exponent l 2)<=2
      ) indexed_sathenay_base ;;
  
  let restricted1_indices = Image.image fst restricted1_base ;;
  
  let restricted1_text = 
      let core = String.concat "," 
      (Image.image string_of_int restricted1_indices) in 
      "\n\n\nrestricted1_indices=["^core^"];\n\n\n" ;;
  
  let restricted2_base = List.filter (fun (i,l)->
      (exponent l 3)<=1
  ) restricted1_base ;;
  
  let restricted2_indices = Image.image fst restricted2_base ;;
  
  let restricted2_text = 
      let core = String.concat "," 
      (Image.image string_of_int restricted2_indices) in 
      "\n\n\nrestricted2_indices=["^core^"];\n\n\n" ;;
  
  let restricted3_base = List.filter (fun (i,l)->
          (exponent l 4)<=1
  ) restricted2_base ;;
      
  let restricted3_indices = Image.image fst restricted3_base ;;
      
  let restricted3_text = 
      let core = String.concat "," 
      (Image.image string_of_int restricted3_indices) in 
      "\n\n\nrestricted3_indices=["^core^"];\n\n\n" ;;    
  
  let squash l=
   if l=[] then "1" else
   "g"^(String.concat "" (Image.image string_of_int l)) ;;
  
  let expression_for_squash = function 
     [] -> "1"
    |a::b ->if b=[] then squash[a] else
      "w_nz("^(squash [a])^" * "^(squash b)^")" ;;
  
  let new_variable_definition_opt l = match l with  
      [] -> ""
    |a::b ->if b=[] then "" else
    (squash l)^" = w_nz("^(squash [a])^" * "^(squash b)^");\n" ;;
       
  
  let line_for_usual_computation (idx,l) = 
     (new_variable_definition_opt l)^ 
     "initial_base["^(string_of_int idx)^"] = "^(squash l)^" ;\n"^
      "printf(\"Step "^(string_of_int idx)^" of 720 finished\\n\");\n" ;;
  
  let text_for_usual_computations =
     let temp1 = Image.image line_for_usual_computation 
     indexed_sathenay_base in
     "\n\n\n"^
     "initial_base=vector(720,k,\"\");\n"^
     (String.concat "\n" temp1)^
     "\n\n\n" ;; 
  
  let line_for_usual_retrieval (idx,l) = 
      (squash l)^" = initial_base["^(string_of_int idx)^"] ;\n" ;;
   
  
  let long_ones = List.filter (fun (_,l)->List.length l>1) 
  indexed_sathenay_base ;;
  
  let text_for_usual_retrievals =
      let temp1 = Image.image line_for_usual_retrieval 
          long_ones in
          "\n\n\n"^
          (String.concat "\n" temp1)^
          "\n\n\n" ;; 
       
  let restricted50_uples = 
      Image.image (fun x->List.assoc x indexed_sathenay_base)
      [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 121; 122; 123; 124; 125; 126; 127; 241];;
          
  let restricted50_monomials = 
      String.concat "," (Image.image squash restricted50_uples) ;;        
  
  
  let ap2 = Absolute_path.of_string (home ^
  "/Teuliou/Bash_scripts/Pari_Programming/my_pari_code/follenn2.gp");;
  
  let ap3 = Absolute_path.of_string (home ^
  "/Teuliou/Bash_scripts/Pari_Programming/my_pari_code/follenn3.gp");; 
  
 (*
  Lines_in_text.remove_interval_in_file ap3 9181 108943 ;;
  
  Lines_in_text.remove_interval_in_file ap2 236 3008 ;;
  
  Io.append_string_to_file text_for_usual_computations ap2 ;;
  
  Io.append_string_to_file text_for_usual_retrievals ap2 ;;
  
  Io.append_string_to_file restricted1_text ap2 ;;
  
  Io.append_string_to_file restricted2_text ap2 ;;
  
  Replace_inside.replace_several_inside_file 
    ["g11111","g22222";"g1111","g2222";
    "g111","g222";"g11","g22";"g1","g2"] ap3 ;;
  
  Replace_inside.replace_several_inside_file 
    ["g11111","g33333";"g1111","g3333";
    "g111","g333";"g11","g33";"g1","g3"] ap3 ;;  
  
  Replace_inside.replace_several_inside_file 
    ["g11111","g44444";"g1111","g4444";
    "g111","g444";"g11","g44";"g1","g4"] ap3 ;;   

*)
end ;;


(************************************************************************************************************************
Snippet 177 : Miscellanea preparing a refactorization
************************************************************************************************************************)
module Snip177=struct


(*
module type EXIT_TYPE = sig

 val forget_modules :
   Fwg_with_dependencies.t ->
   Skeptical_duck_lib.Dfa_module_t.t list ->
   Fwg_with_dependencies.t * Skeptical_duck_lib.Dfn_rootless_t.t list
 val inspect_and_update :
   Fwg_with_dependencies.t ->
   Fwg_with_dependencies.t *
   ((Dfn_rootless_t.t list * Dfn_rootless_t.t list) *
    (Dfn_rootless_t.t * Skeptical_duck_lib.Fw_file_small_details_t.t) list *
    Dfn_rootless_t.t list)
 val of_configuration : Fwg_configuration.t -> Fwg_with_dependencies.t
 val of_configuration_and_list :
   Fwg_configuration.t * Dfn_rootless_t.t list -> Fwg_with_dependencies.t
 val overwrite_file_if_it_exists :
   Fwg_with_dependencies.t ->
   Dfn_rootless_t.t * string ->
   Fwg_with_dependencies.t *
   (Dfn_rootless_t.t * (Dfn_rootless_t.t * Fw_file_small_details_t.t) option)
   option
 val plunge_fw_configuration :
   Fwg_configuration.t -> Fwg_with_dependencies.t
 val register_rootless_paths :
   Fwg_with_dependencies.t ->
   Dfn_rootless_t.t list ->
   Fwg_with_dependencies.t *
   ((Dfn_rootless_t.t list * Dfn_rootless_t.t list * Dfn_rootless_t.t list) *
    (Dfn_rootless_t.t * Fw_file_small_details_t.t) list)
 val relocate_module_to :
   Fwg_with_dependencies.t ->
   Dfa_module_t.t * Skeptical_duck_lib.Dfa_subdirectory_t.t ->
   Fwg_with_dependencies.t *
   ((Dfn_rootless_t.t *
     (Dfn_rootless_t.t * Fw_file_small_details_t.t) option)
    list * (Dfn_rootless_t.t * Dfn_rootless_t.t) list)
 val remove_files :
   Fwg_with_dependencies.t ->
   Dfn_rootless_t.t list ->
   Fwg_with_dependencies.t * (Dfn_rootless_t.t * 'a option) list
 val rename_module_on_filename_level_and_in_files :
   Fwg_with_dependencies.t ->
   Dfa_module_t.t * Dfa_module_t.t * Dfn_rootless_t.t list ->
   Fwg_with_dependencies.t *
   ((Dfn_rootless_t.t *
     (Dfn_rootless_t.t * Fw_file_small_details_t.t) option)
    list *
    ((Dfn_rootless_t.t * Dfn_rootless_t.t) list * Dfn_rootless_t.t list))
 val rename_subdirectory_as :
   Fwg_with_dependencies.t ->
   Dfa_subdirectory_t.t * Dfa_subdirectory_t.t ->
   Fwg_with_dependencies.t *
   ((Dfn_rootless_t.t *
     (Dfn_rootless_t.t * Fw_file_small_details_t.t) option)
    list * (Dfn_rootless_t.t * Dfn_rootless_t.t) list)
 val replace_string :
   Fwg_with_dependencies.t ->
   string * string ->
   Fwg_with_dependencies.t *
   ((Dfn_rootless_t.t *
     (Dfn_rootless_t.t * Fw_file_small_details_t.t) option)
    list * Dfn_rootless_t.t list)
 val replace_value :
   Fwg_with_dependencies.t ->
   (Absolute_path.t list * Absolute_path.t) * (string * string) ->
   Fwg_with_dependencies.t *
   ((Dfn_rootless_t.t *
     (Dfn_rootless_t.t * Fw_file_small_details_t.t) option)
    list * Dfn_rootless_t.t list)

end ;;   

module E1 = (Fwc_with_dependencies.Private.Exit:EXIT_TYPE) ;;
module E2 = (Fwc_with_dependencies.Private.Core:EXIT_TYPE) ;;



module C1 = Fwc_with_dependencies.Private.Exit ;;
module C2 = Fwc_with_dependencies.Private.Core ;;

*)


let ap1 = Absolute_path.of_string 
"lib/Filewatching/Fw_classes/fwc_with_dependencies.ml";;

let txt1 = Io.read_whole_file ap1 ;;

let txt2 = Lines_in_text.interval txt1 1076 1081 ;;

let txt3 = Replace_inside.replace_inside_text ("All_printables","Core")
txt2;;

let act1 () = Lines_in_text.modify_interval_inside_file 
(fun _->txt3) ap1 1064 1064 ;;

let u1 = [
   ("forget_modules",true,false,false,["mods_to_be_erased"]);
   ("inspect_and_update",true,true,false,[]);
   ("of_configuration",false,false,false,["config"]);
   ("of_configuration_and_list",false,false,false,["(config,files)"]); 
   ("overwrite_file_if_it_exists",true,true,false,["(rootless,new_content)"]);
   ("plunge_fw_configuration",false,false,false,["config"]);
   ("register_rootless_paths",true,true,false,["rootlesses"]);
   ("relocate_module_to",true,true,false,["(mod_name,new_subdir)"]);
   ("remove_files",true,true,false,["removed_rootless_paths"]);
   ("rename_module_on_filename_level_and_in_files",true,true,true,["triple"]);
   ("rename_subdirectory_as",true,true,true,["sdir_pair"]);
   ("replace_string",true,true,false,["(replacee,replacer)"]);
   ("replace_value",true,true,false,["(replacee,replacer)"]);
];;

let write (mthd_name,extra_before,extra_after,arg_reused,args) =
   let arg = (match args with [] -> "" |_ ->List.hd args) in
   let old_output = ( if extra_before then "(new_fw_dets,extra)" else "new_fw_dets") 
   and opt_before = (if extra_after then "extra" else "")
   and opt_after = (if arg_reused then arg else "") 
   and final_output = ( if extra_before then ",extra" else "")in 
 "let "^mthd_name^" old_fw "^arg^" =  \n"^
 " let old_fw_dets = parent old_fw \n"^
 " and deps_ref = ref (dependencies old_fw) in \n"^
 " let "^old_output^" = Fwc_with_small_details."^mthd_name^" old_fw_dets "^arg^" in \n"^
 " let _ = Fw_dependencies.ReactOnReference."^mthd_name^" "^opt_before^" new_fw_dets deps_ref "^opt_after^" in \n"^
 " (make new_fw_dets(!deps_ref)"^final_output^");;" ;;

let u2 = Image.image write u1 ;;

let u3 = "\n\n\n"^(String.concat "\n\n" u2)^"\n\n\n" ;;

let act2 () =
    Replace_inside.overwrite_between_markers_inside_file
    ~overwriter:u3 ("(* aaa *)","(* bbb *)") ap1 ;;

let act3 () = Lines_in_text.remove_interval_in_file 
    ap1 99 1061 ;;
    


end ;;


(************************************************************************************************************************
Snippet 176 : Check that double crobj conversion leads back to original
************************************************************************************************************************)
module Snip176=struct

(*

let extract_last = function 
(Fw_final_poly.With_githubbing g) -> g | _ -> failwith("hum") ;;

let fw_with_githubbing = 
  extract_last (!(Usual_coma_state.Private.main_ref)) ;;
let fw_batch=Fwg_with_githubbing.parent fw_with_githubbing;;
let fw_deps=Fwg_with_batch_compilation.parent fw_batch;;

module Pri = Fwc_with_dependencies.Private ;;

let get_tea fw  = {
   Fw_dependencies_t.modularized_details = Pri.Modularized_details.get fw ;
   order = Pri.Order.get fw;
   needed_dirs = Pri.Needed_dirs.get fw;
   needed_libs = Pri.Needed_libs.get fw;
   all_subdirectories = Pri.All_subdirectories.get fw;
   all_printables = Pri.All_printables.get fw;
} ;;

let t1 = get_tea fw_deps ;;

let obj_t1 = Fw_dependencies.Crobj.to_concrete_object t1 ;;

let t2 = Fw_dependencies.Crobj.of_concrete_object obj_t1 ;;

let check = (t1 = t2) ;;

*)

end ;;


(************************************************************************************************************************
Snippet 175 : Using Crobj_conversion_boilerplate.write 
************************************************************************************************************************)
module Snip175=struct

  let ap1 = 
    Absolute_path.of_string 
    "lib/Filewatching/fw_dependencies.ml";;
  
  let u1 = Lines_in_text.extract_field_names_from_type_definition_in_file
     ap1 8 13 ;;   
  let u2 =  Crobj_conversion_boilerplate.write 
   ~var_name:"msd" 
     ~type_name:"Fw_dependencies_t" 
      ~fields:u1 ;;   
  
  let u3 () = print_string u2 ;;



end ;;


(************************************************************************************************************************
Snippet 174 : Musings on twin primes
************************************************************************************************************************)
module Snip174=struct

  let i_order = Total_ordering.for_integers ;;
  let i_sort = Ordered.sort  i_order ;;
  
  
  let is_a_twin_prime =Memoized.make(fun n ->
      (Primes.is_prime n) && (Primes.is_prime(n+2))
  ) ;;
  
  let tp_list = Memoized.make (fun n->
      let candidates = Int_range.range (n+1) (n*n-2) in 
      List.filter is_a_twin_prime candidates
  );;
  
  let upper_bound = 100 ;;
  
  module Crocodile = struct 
  
  type t = {
     elements : (int * int) list ;
     whole : int * int
  } ;;
  
  let enlarge croc (r,m) = {
     elements = (croc.elements) @ [r,m] ;
     whole = Gcd.combine_two_congruences (croc.whole) (r,m)
  } ;;
  
  let constructor elts = {
     elements = elts ;
     whole = Gcd.combine_congruences elts
  } ;;
  
  
  let check croc =
     let n = snd(List.hd(List.rev(croc.elements))) 
     and (r0,m0) = croc.whole in 
     let temp1 = Int_range.scale (
       fun m->(m,List.filter (fun x->x mod m0=r0) (tp_list m))
     ) n upper_bound in 
     List.filter_map (fun (m,l)->
        if l=[] then Some m else None) temp1 ;;
  
  let to_string croc =
     String.concat "-" (Image.image (fun (x,y)->string_of_int x)
       croc.elements) ;;
  
  let image croc m =
     Gcd.congruence_image croc.whole ~other_modulus:m ;;
  
  let descendants_for_one m croc =
     let temp1 = image croc m in 
     let temp2 = Image.image ( 
      fun r->enlarge croc (r,m)
     ) temp1 in  
     List.filter (fun cr->check(cr)=[]) temp2 ;;
  
  let descendants m ll =
     List.flatten(Image.image (descendants_for_one m) ll) ;;    
  
  let print_out (fmt:Format.formatter) ap=
     Format.fprintf fmt "@[%s@]" (to_string ap);;
  
  end ;;   
  
  (* install_printer Crocodile.print_out ;; *)
  
  let croc1 = Crocodile.constructor [1,2;2,3] ;;
  
  let u1 = [croc1] ;;
  
  let u2 = Crocodile.descendants 4 u1 ;;
     
  let u3 = Crocodile.descendants 5 u2;;


end ;;


(************************************************************************************************************************
Snippet 173 : Simple test
************************************************************************************************************************)
module Snip173=struct

  let outside =   ((Sys.getenv "HOME")^"/Teuliou/OCaml/idaho/lib/") ;;

  let script_for_testing () =
     let ap1 = Absolute_path.create_file_if_absent(outside^"za.ml")
     and ap2 = Absolute_path.create_file_if_absent(outside^"zb.ml")
     and ap3 = Absolute_path.create_file_if_absent(outside^"zc.ml") in 
     (
     Io.overwrite_with ap1 "let a = 3 ;;";
     Io.overwrite_with ap2 "let b = Za.a + 4 ;;";
     Io.overwrite_with ap3 "let c = 5 ;;";
     ) ;; 
      


end ;;


(************************************************************************************************************************
Snippet 172 : Step in implementing a certain "Poor man's row polymorphism"
************************************************************************************************************************)
module Snip172=struct

(*

Step by step, we introduce an old module 
into the row-polymorphism family. 

*)

(* Step 1 : Rename and relocate the module with a Fwc_prefix *)

ren "fw_with_dependencies" "fwc_with_dependencies";;
relo "fwc_with_dependencies" 
(Dfa_subdirectory.of_line "lib/Filewatching/Fw_classes") ;; 

(* Step 2 : Create the Fwg corresponding to the Fwc *)

dm "fwc_with_dependencies" "fwg_with_dependencies" ;;

(* Step 3 : register the Fwg file *)

regi "lib/Filewatching/Fw_classes/fwg_with_dependencies.ml" ;;

(* Step 4 : Update the list in the lti file *)

(* Step 5 : Create a new Crobj submodule in Fw_poly *)

let ap1 = Absolute_path.of_string "lib/Filewatching/fw_poly.ml";;

let txt1 = Io.read_whole_file ap1;;

let txt2 = 
  Replace_inside.replace_inside_text ("Crobj ","PartialCrobj ")
  ("\n\n\n"^(Lines_in_text.interval txt1 26 64)^"\n\n\n") ;;

let act1 () = Lines_in_text.modify_interval_inside_file
   (fun _->txt2) ap1 66 66 ;;


(* Step 6 : Fill the Fwg file *)

(* Step 7 : Add crobj conversions in the parent *)

(*
let of_concrete_object = Fw_poly.Private.PartialCrobj.of_concrete_object ;;

let to_concrete_object = Fw_poly.Private.PartialCrobj.to_concrete_object ;;
*)
(* Step 8 : Create a temporary copy of the Fwc file, 
which will soon replace it in the future *)

dm "fwc_with_dependencies" "coming_soon_fwc_with_dependencies" ;;

(* Step 9 : Modify the coming_soon file : replace calls to 
Fw_poly by calls to the Fwg, add a Crobj submodule. 
*)

(* Step 10 : Create a foreign copy *)

let act2() = Fw_usual_manager.create_foreign_copy None;;


(* Step 11 : Copy the coming_soon file *)

print_string
("\n\n\ncp skeptical_duck/lib/Filewatching/Fw_classes/coming_soon_fwc_with_dependencies.ml "^
"idaho/lib/Filewatching/Fw_classes/fwc_with_dependencies.ml\n\n\n");; 

(* Step 12 : Remove some fields in Fw_flattened_t *)

(* Step 13 : Put new Fw_poly.Crobj submodule *)

let foreign_fw_poly_ap = Absolute_path.of_string 
   ((Sys.getenv "HOME")^
   "/Teuliou/OCaml/idaho/lib/Filewatching/fw_poly.ml") ;;
let act3() = 
   Lines_in_text.remove_interval_in_file foreign_fw_poly_ap 27 69 ;;

(* Step 14 : Fix various places in the code until dune build passes *)

(* Step 15 : Apply ti_for_file_watching.toggle() twice *)

(* Step 16 : Test equality after a crobj back and forth *)

let extract_wg g = g ;;


(*function 
(Fw_final_poly.With_githubbing g) -> g | _ -> failwith("hum") ;; *)

let g1 = extract_wg (!(Fw_usual_manager.Private.main_ref)) ;;

let cg1 = Fwc_with_githubbing.Crobj.to_concrete_object g1 ;;

let g2 = Fwc_with_githubbing.Crobj.of_concrete_object cg1 ;;

let test1 = Fwc_with_githubbing.Inherited.test_equality g1 g2 ;;

(* Step 17 : Test if registering works *)

let outside =   ((Sys.getenv "HOME")^"/Teuliou/OCaml/idaho/lib/") ;;

let ap1 = Absolute_path.create_file_if_absent(outside^"za.ml")
and ap2 = Absolute_path.create_file_if_absent(outside^"zb.ml")
and ap3 = Absolute_path.create_file_if_absent(outside^"zc.ml") ;;

let act4 () =
(
   Io.overwrite_with ap1 "let a = 3 ;;";
   Io.overwrite_with ap2 "let b = Za.a + 4 ;;";
   Io.overwrite_with ap3 "let c = 5 ;;";
   ) ;; 

let act5 () =
  Io.overwrite_with ap3 "let c = Za.a + Zb.b + 5 ;;";
;;    

(* Step 18 : Officialize changes *)

let g1 = Fw_usual_manager.changed_files_in_foreign_copy () ;;

Fw_usual_manager.officialize_foreign_changes () ;;

save "use Fwg_with_dependencies in Fwc_with_dependencies";;

(* Step 19 : reset *)

(* dune utop lib *)

Fw_usual_manager.refresh() ;;

(* dune utop lib *)

fg "coming_soon_fwc_with_dependencies" ;;


end ;;


(************************************************************************************************************************
Snippet 171 : Combinatorial musings related to twin primes
************************************************************************************************************************)
module Snip171=struct

  let destroys l x = List.exists(fun y->x mod y=0) l ;;

  let rec muu l x =
    if (destroys l x)||(destroys l (x+2))
    then muu l (x+1)
    else x ;;  
  
  
  
  let q1=11;;
  
  let upper_bound = 1000 ;;
  let tf1 =Memoized.make(fun p -> muu [2;3;q1;p] (p+1));;  
  
  let u1 = Int_range.scale tf1 (q1+1) upper_bound ;;
      
  let u2 = Int_range.scale (fun p-> (tf1 p)-p) (q1+1) upper_bound ;;
    
  let m1 = Max.list u2 ;;
  
  let u3 = List.filter (fun p->tf1(p)-p=m1) (Int_range.range (q1+1) upper_bound) ;;
    
  let u4 = Image.image (fun p->p mod (6*q1)) u3 ;;
    
  (*
  
  let tf1 =Memoized.make(fun p -> muu [2;3;5;p] (p+1));;  
  
  let u1 = Int_range.scale tf1 5 500 ;;
    
  let u2 = Int_range.scale (fun p-> (tf1 p)-p) 5 500 ;;
  
  let u3 = List.filter (fun p->tf1(p)-p=12) (Int_range.range 5 500) ;;
  
  let u4 = Image.image (fun p->p mod 30) u3 ;;
  
  
  let tf1 p = muu [2;3;p] (p+1);;  
  
  let u1 = Int_range.scale tf1 5 50 ;;
  
  let u2 = Int_range.scale (fun p-> (tf1 p)-p) 5 50 ;;
  
  *)


end ;;


(************************************************************************************************************************
Snippet 170 : Comparator for a record type
************************************************************************************************************************)
module Snip170=struct

  let differences fw1 fw2 = 
    let temp =
    [
      ("type_name",(fw1.Fw_flattened_poly_t.type_name=fw2.Fw_flattened_poly_t.type_name));
      ("root",(fw1.Fw_flattened_poly_t.root=fw2.Fw_flattened_poly_t.root));
      ("ignored_subdirectories",(fw1.Fw_flattened_poly_t.ignored_subdirectories=fw2.Fw_flattened_poly_t.ignored_subdirectories));
      ("ignored_files",(fw1.Fw_flattened_poly_t.ignored_files=fw2.Fw_flattened_poly_t.ignored_files));
      ("watched_files",(fw1.Fw_flattened_poly_t.watched_files=fw2.Fw_flattened_poly_t.watched_files));
      ("subdirs_for_archived_mlx_files",(fw1.Fw_flattened_poly_t.subdirs_for_archived_mlx_files=fw2.Fw_flattened_poly_t.subdirs_for_archived_mlx_files));
      ("small_details_in_files",(fw1.Fw_flattened_poly_t.small_details_in_files=fw2.Fw_flattened_poly_t.small_details_in_files));
      ("last_compilation_result_for_module",(fw1.Fw_flattened_poly_t.last_compilation_result_for_module=fw2.Fw_flattened_poly_t.last_compilation_result_for_module));
      ("dir_for_backup",(fw1.Fw_flattened_poly_t.dir_for_backup=fw2.Fw_flattened_poly_t.dir_for_backup));
      ("gitpush_after_backup",(fw1.Fw_flattened_poly_t.gitpush_after_backup=fw2.Fw_flattened_poly_t.gitpush_after_backup));
      ("github_url",(fw1.Fw_flattened_poly_t.github_url=fw2.Fw_flattened_poly_t.github_url));
      ("encoding_protected_files",(fw1.Fw_flattened_poly_t.encoding_protected_files=fw2.Fw_flattened_poly_t.encoding_protected_files));
  
    ] in 
    List.filter_map (fun (fld,is_ok)->if is_ok then None else Some fld) temp;;
  
  let txt1 = rf "lib/Filewatching/fw_flattened_poly_t.ml" ;;
  
  let txt2 = Lines_in_text.interval txt1 9 21 ;;
  
  let u1 = Lines_in_text.lines txt2 ;;
  
  let u2 = Image.image (fun s->Cull_string.before_rightmost s ':') u1 ;;
  
  let u3 = Image.image Cull_string.trim_spaces u2 ;; 
  
  let u4 = Image.image (
    fun s -> "   (\""^s^"\",(fw1.Fw_flattened_poly_t."^s^"=fw2.Fw_flattened_poly_t."^s^"));"
  ) u3 ;;
  
  let u5 = "\n\n\n" ^ (String.concat "\n" u4) ^ "\n\n\n" ;; 
  
  let u6 () = print_string u5 ;;


end ;;


(************************************************************************************************************************
Snippet 169 : Bevy of values
************************************************************************************************************************)
module Snip169=struct

  let designated_parents = [
    "Fw_with_archives" , "File_watcher" ;
    "Fw_with_small_details" , "Fw_with_archives" ;
    "Fw_with_dependencies" , "Fw_with_small_details" ;
    "Fw_with_batch_compilation" , "Fw_with_dependencies" ;
    "Fw_with_githubbing" , "Fw_with_batch_compilation" ;
] ;;


  let f1 = Fwc_with_modular_infrastructure.all_endinglesses ;;
  let f2 = Fwc_with_modular_infrastructure.all_moduled_mlx_files ;;
  let f3 = Fwc_with_modular_infrastructure.all_subdirectories ;;
  let f4 = Fwc_with_modular_infrastructure.ancestors_for_module ;;
  let f5 = Fwc_with_modular_infrastructure.below ;;
  let f6 = Fwc_with_archives.check_that_no_change_has_occurred ;;
  let f9 = Fwc_with_modular_infrastructure.decipher_module ;;
  let f10 = Fwc_with_modular_infrastructure.decipher_path ;;
  let f11 = Fwc_with_modular_infrastructure.dep_ordered_modules ;;
  let f12 = Fwc_with_modular_infrastructure.directly_below ;;
  let f13 = Fwc_with_modular_infrastructure.direct_fathers_for_module ;;
  let f14 = Fwc_with_modular_infrastructure.duplicate_module ;;
  let f15 = Fwc_with_modular_infrastructure.endingless_at_module ;;
  let f16 = Fwc_with_modular_infrastructure.find_subdir_from_suffix ;;
  let f17 = Fwc_with_githubbing.forget_modules ;;
  let f18 = Fwc_with_githubbing.forget_nonmodular_rootlesses ;;
  let f19 = Fwg_github_configuration.gitpush_after_backup ;;
  let f20 = Fwc_configuration.ignored_files ;;
  let f21 = Fwc_configuration.ignored_subdirectories ;;
  let f22 = Fwc_with_archives.latest_changes ;;
  let f23 = Fwc_with_modular_infrastructure.list_values_from_module ;;
  let f25 = Fwc_with_modular_infrastructure.modules_using_value ;;
  let f26 = Fwc_with_archives.noncompilable_files ;;
  let f27 = Fwc_with_modular_infrastructure.number_of_modules ;;
  let f28 = Fwc_with_githubbing.Crobj.of_concrete_object ;;
  let f31 = Fwc_with_githubbing.plunge_fw_config_with_github_config ;;
  let f33 = Fwc_with_githubbing.register_rootless_paths ;;
  let f34 = Fwc_with_githubbing.relocate_module_to ;;
  let f35 = Fwc_with_githubbing.rename_module ;;
  let f36 = Fwc_with_githubbing.rename_subdirectory_as ;;
  let f37 = Fwc_with_githubbing.replace_string ;;
  let f38 = Fwc_with_githubbing.replace_value ;;
  let f39 = Fwc_configuration.root ;;
  let f40 = Fwc_with_githubbing.Inherited.set_gitpush_after_backup ;;
  let f44 = Fwc_configuration.test_for_admissibility ;;
  let f45 = Fwc_with_githubbing.Crobj.to_concrete_object ;;
  let f46 = Fwc_with_githubbing.Inherited.to_fw_configuration ;;
  let f47 = Fwg_with_githubbing.github_configuration ;;
  let f49 = Fwc_with_archives.usual_compilable_files ;;

end ;;


(************************************************************************************************************************
Snippet 168 : Extracting modules and values from an OCaml snippet
************************************************************************************************************************)
module Snip168=struct

(* extract module names *)

let ap1 = Absolute_path.of_string "lib/Filewatching/fw_final_poly.ml" ;;

let text1 = Io.read_whole_file ap1 ;;

let lines_in_text1 = Lines_in_text.indexed_lines text1 ;;

let u1 = Image.image (
  fun (idx,line) -> (idx,Cull_string.trim_spaces line)) lines_in_text1 ;;

let u2 = List.filter (fun (idx,line) -> 
  String.starts_with line ~prefix:"module ") u1 ;;

let u3 = Image.image (fun(idx,line)->(idx,
 Cull_string.two_sided_cutting ("module "," = struct") line)) u2;;  

let u4 =  u3 @ [(165,"...")] ;;

let u5 = List_again.universal_delta_list u4 ;;

let u6 = Image.image (fun ((i1,m1),(i2,_))->(m1,(i1,i2-1))) u5 ;;

let u7 = Image.image (fun (old_mod_name,(i,j))->
  let new_mod_name = (
    if old_mod_name = "Lfw_file_watcher" then "File_watcher" else
    String.capitalize_ascii(Cull_string.cobeginning 1 old_mod_name)) in 
  (new_mod_name,(i,j)
)) u6 ;;  

let extract_values_in_module (i,j) = 
  let subtext = Lines_in_text.interval text1 i j in 
  let lines2 = Lines_in_text.lines subtext in 
  let lines3 = Image.image Cull_string.trim_spaces lines2 in  
  let lines4 = List.filter(fun x->String.starts_with x ~prefix:"let ") lines3 in  
  let lines5 = Image.image Cull_string.trim_spaces lines4 in 
  let lines6 = Image.image (Cull_string.cobeginning 4) lines5 in
  let lines7 = Image.image (fun x->let k=String.index x ' ' in
  Cull_string.beginning k x) lines6 in 
  lines7 ;;

let u8 = Image.image (fun (mod_name,(i,j))->(mod_name,
   extract_values_in_module (i,j)
)) u7 ;;  

let unordered_pairs = List.flatten(Image.image (
   fun (mod_name,vals) -> 
    Image.image (fun vaal->(vaal,mod_name)) vals
) u8);;

let values = Ordered.sort 
  Total_ordering.lex_for_strings (Image.image fst unordered_pairs) ;; 

let u9 = Image.image (
  fun vaal -> (vaal,List.filter_map (fun p->if fst p=vaal then Some(snd p) else None) unordered_pairs)
) values;;

let u10 = Image.image (fun (vaal,l)->(vaal,List.hd l)) u9 ;;

let u11 = Int_range.index_everything u10 ;; 

let u12 = "\n\n\n" ^ (String.concat "\n" 
(Image.image (fun (k,(v,m))->
  "let f"^(string_of_int k)^" = "^m^"."^v^" ;;"  
) u11)) ^ "\n\n\n" ;; 

let ap1 = Absolute_path.of_string "watched/watched_not_githubbed/cloth.ml";;


let act () = Lines_in_text.insert_after_line_inside_file 
ap1 ~line_number:11 ~inserted_snippet:u12 ;;

end ;;


(************************************************************************************************************************
Snippet 167 : Using the Coherent_pdf module
************************************************************************************************************************)
module Snip167=struct


  let sipdf = "~/Teuliou/Heavy/Scanning/Single_Pdf/Colette/" ;;
  let ap1 = Absolute_path.of_string (sipdf^"sk456.pdf") ;;
  let ap2 = Absolute_path.of_string (sipdf^"skeudennou.pdf") ;;
  
  let act1 () = Coherent_pdf.replace_inside 
  ~patient:ap2
  ~replacer:ap1
  ~left_of_cut:2 
  ~right_of_cut:3
  ~outputfile_name:"helena" ;;
  
  let ap3 = Absolute_path.of_string (sipdf^"Erratum/erratum_002.pdf") ;;
  let ap4 = Absolute_path.of_string (sipdf^"p4.pdf") ;;
  
  let act2 () = Coherent_pdf.replace_inside 
  ~patient:ap4
  ~replacer:ap3
  ~left_of_cut:1 
  ~right_of_cut:3 
  ~outputfile_name:"new_p4" ;;
  
  let ap5 = Absolute_path.of_string (sipdf^"colette.pdf") ;;
  
  let act3 () = Coherent_pdf.remove_interval_inside
  ~patient:ap5
  ~first_in_cut:179 
  ~last_in_cut:179 
  ~outputfile_name:"celotte" ;;
  
  let ap5 = Absolute_path.of_string (sipdf^"colette.pdf") ;;
  
  let ap6 = Absolute_path.of_string (sipdf^"rotated.pdf") ;;
  
  let act4 () = Coherent_pdf.replace_inside 
  ~patient:ap5
  ~replacer:ap6
  ~left_of_cut:9 
  ~right_of_cut:14 
  ~outputfile_name:"new_colette" ;;


end ;;


(************************************************************************************************************************
Snippet 166 : Delete all modules in a subdirectory
************************************************************************************************************************)
module Snip166=struct

  let ae = Fw_usual_manager.all_endinglesses () ;;

  let sd1 = Dfa_subdirectory.of_line "lib/Mad_world" ;;
  
  let u1 = List.filter (
    fun rl -> Dfa_subdirectory.begins_with (Dfn_endingless.to_subdirectory rl) sd1
  ) ae ;;
  
  let u2 = Image.image (
    fun rl -> Dfa_module.to_line (Dfn_endingless.to_module rl)
  ) u1 ;;
  
  let act () = fgs ["mw_file_small_details_t"; "mw_file_small_details"; "mw_instance_index_t";
  "mw_state_index_t"; "mw_indexer"; "mw_poly_t"; "mw_poly";
  "mw_configuration"; "mw_file_watcher"; "mw_with_archives";
  "mw_module_small_details_t"; "mw_with_small_details";
  "mw_module_small_details"; "mw_determine_order"; "mw_with_dependencies";
  "mw_transmit_change_to_github"; "mw_commands_for_batch_compilation";
  "mw_with_batch_compilation"; "mw_with_githubbing"; "mw_with_persisting";
  "mw_modify_coma_state"; "mw_needed_data_summary_t";
  "mw_needed_data_summary"; "mw_create_world_copy"; "mw_usual_coma_state";
  "mw_other_coma_state"; "mw_constant"; "mw_check_ocaml_dircopy";
  "mw_needed_values"; "mw_self_contained_module_copy"] ;;
  


end ;;


(************************************************************************************************************************
Snippet 165 : Some old conversion code
************************************************************************************************************************)
module Snip165=struct

(*

module ConvertToMw = struct 

  let from_fw_instance_index (Fw_instance_index_t.I x) = Mw_instance_index_t.I x ;;
  let from_fw_state_index (Fw_state_index_t.I x) = Mw_state_index_t.I x ;;

  let from_fw_file_small_details fsd = 
    {
       Mw_file_small_details_t.used_modules = fsd.Fw_file_small_details_t.used_modules;
       used_libraries = fsd.Fw_file_small_details_t.used_libraries;
       has_printer = fsd.Fw_file_small_details_t.has_printer;
       modification_time = fsd.Fw_file_small_details_t.modification_time;
    };;  

  let from_fw_poly fw = 
    let (instance_idx,state_idx) = fw.Fw_poly_t.index_for_caching in 
    {
      Mw_poly_t.type_name = 
       Replace_inside.replace_inside_text ("Fw_","Mw_")
       fw.Fw_poly_t.type_name ;
      dir_for_backup = fw.Fw_poly_t.dir_for_backup  ;
      encoding_protected_files = fw.Fw_poly_t.encoding_protected_files ;
      github_url = fw.Fw_poly_t.github_url ;
      gitpush_after_backup = fw.Fw_poly_t.gitpush_after_backup ;
      ignored_files = fw.Fw_poly_t.ignored_files ;
      ignored_subdirectories = fw.Fw_poly_t.ignored_subdirectories ;
      index_for_caching = (from_fw_instance_index instance_idx,
                           from_fw_state_index state_idx) ;
      last_compilation_result_for_module = fw.Fw_poly_t.last_compilation_result_for_module ;
      root = fw.Fw_poly_t.root ;
      small_details_in_files = 
       Image.image (
          fun (r,smde) -> (r,from_fw_file_small_details smde)
       ) fw.Fw_poly_t.small_details_in_files;
      subdirs_for_archived_mlx_files = fw.Fw_poly_t.subdirs_for_archived_mlx_files ;
      watched_files = fw.Fw_poly_t.watched_files ;
   } ;;


end ;;  

let old_cs = ConvertToMw.from_fw_poly (!(Usual_coma_state.main_ref)) ;;

(*
let bad1 () = Mw_other_coma_state.repopulate 
Mw_needed_data_summary_t.Everything ;;
*)

let summary = Mw_needed_data_summary_t.Everything ;;

Mw_other_coma_state.Private.ref_for_unofficial_changes:=None ;;

let (next_dest,next_backup,next_gab) = 
   Coma_big_constant.Next_World.triple ;;

Hashtbl.length   Mw_with_dependencies.Private.Modularized_details.the_hashtbl ;;
Hashtbl.length   Mw_with_dependencies.Private.Order.the_hashtbl ;;

Hashtbl.clear   Mw_with_dependencies.Private.Modularized_details.the_hashtbl ;;
Hashtbl.clear   Mw_with_dependencies.Private.Order.the_hashtbl ;;

let act ()=
  Chronometer.it
 (Mw_create_world_copy.fully_developed_copy
   ~destination:next_dest ~destbackupdir:next_backup ~destgab:next_gab
   old_cs) summary
  ;;   



*)


end ;;


(************************************************************************************************************************
Snippet 164 : Combinatorics on Komal problem
************************************************************************************************************************)
module Snip164=struct

  open Zirath ;;

  let param = 5 ;;
  
  (*
  
  
  #install_printer Zirath.Q.trinp_out ;;
  
  *)
  
  let df i j = Zirath.Q.of_ints 1 (abs(j-i)) ;;
  
  let before_crocodile l =
    let get = (fun k->List.nth l (k-1)) in
    let indexed_l = Int_range.index_everything l in 
    let inside_map = (
      fun (idx_for_v,v) ->
        let ttemp2 = List.filter_map (
           fun idx_for_w ->
             let w = get idx_for_w in 
             if List.mem(abs(w-v)) [1;2]
             then Some((idx_for_w,w),df v w) 
             else None
        ) (Int_range.range 1 (idx_for_v-1)) in
        (idx_for_v,(v,ttemp2))
    ) in 
    Image.image inside_map indexed_l ;;
  
  let rec helper_for_crocodile (treated,to_be_treated) = 
    match to_be_treated with
    [] -> List.rev_map snd treated  
    | (idx_for_v,(v,constraints)) :: others ->
      if constraints = [] 
      then helper_for_crocodile ((idx_for_v,(Q.zero,None))::treated,others)
      else
      let pairs_below = Image.image (fun 
      ((idx_for_w,w),d) -> (w,Q.add (fst(List.assoc idx_for_w treated)) d)
      ) constraints in 
      let rationals_below = Image.image snd pairs_below in   
      let final_max = Q.fold_max rationals_below in 
      let (w0,_) = List.find (fun (_,m)->m=final_max) pairs_below in
      helper_for_crocodile ((idx_for_v,(final_max,Some w0))::treated,others) ;;
  
  let crocodile perm = 
     helper_for_crocodile ([],before_crocodile perm) ;;
  
  let rewrite (perm,croc) =
     let n = List.length perm 
     and temp1 = List.combine perm croc in 
     Int_range.scale (fun j->List.assoc j temp1) 1 n ;;
  
  let perms1 = Permutation.iii param ;;
  
  let res1 = Explicit.image (fun perm->(perm,crocodile perm)) perms1 ;;
  
  let u1 = Image.image 
  (fun (perm,croc) -> ((perm,croc),Q.fold_max (Image.image fst croc))) res1 ;;
  
  let minnie1 = Q.fold_min (Image.image snd u1);;
  
  let u2 = List.filter (
    fun (_,m) -> m = minnie1
  ) u1 ;;
  
  let u3 = Image.image (fun (p,_)->rewrite p) u2 ;;
  
  let u4 = Image.image (Image.image (fun (q,data)->
      (Q.div q minnie1,data)
    )) u3 ;;
  
  let u5 = Image.image (Image.image fst) u4 ;;
  
  


end ;;


(************************************************************************************************************************
Snippet 163 : Miscellaneous copy-paste
************************************************************************************************************************)
module Snip163=struct

  let ap1 = Absolute_path.of_string 
  ((Sys.getenv "HOME")^"/Teuliou/OCaml/idaho/lib/Filewatching/fw_final_poly.ml");;
  
  let txt1 = Io.read_whole_file ap1 ;; 
  
  let txt2 = Lines_in_text.interval txt1 62 146 ;;
  
  let u1 = Lines_in_text.lines txt2 ;;
  
  let u2 = Image.image Cull_string.trim_spaces u1 ;;
  
  let u3 = List.filter(fun x->String.starts_with x ~prefix:"let ") u2 ;;
  
  let u4 = Image.image (Cull_string.cobeginning 4) u3 ;;
  
  let u5 = Image.image (fun x->let k=String.index x ' ' in
  Cull_string.beginning k x) u4 ;;
  
  let u6 = u5 @ [
    "root";"ignored_files";"ignored_subdirectories";"test_for_admissibility"
  ];;
  
  let u7 = Ordered.sort Total_ordering.lex_for_strings u6 ;;
  
  let ap2 = Absolute_path.of_string 
  ((Sys.getenv "HOME")^"/Teuliou/OCaml/idaho/draft.ml");;
  
  let txt3 = Io.read_whole_file ap2 ;; 
  
  let txt4 = Lines_in_text.interval txt3 2 3 ;;
  
  let u8 = Image.image (fun x->
    Replace_inside.replace_inside_text ("root",x) txt4 
    ) u7;;
  
  let u9 = String.concat "\n\n" u8;;  
  
  let act () = Lines_in_text.insert_after_line_inside_file
     ap1 ~line_number:15~inserted_snippet:u9 ;;




end ;;


(************************************************************************************************************************
Snippet 162 : Finish an OCR job, and use Coherent_pdf.corep_cuttable_transform
************************************************************************************************************************)
module Snip162=struct

  let dir2 = home ^ "/Teuliou/Heavy/Workshop/Tesserable/Text" ;;

  let v1 = Unix_again.quick_beheaded_complete_ls dir2 ;; 
  
  let v2 = List.filter (fun fn->
    (String.ends_with ~suffix:".txt" fn)&&
    (List.for_all (fun patt->
      not(String.ends_with ~suffix:("_"^patt^".txt") fn)  
    ) ["a"]) ) v1 ;; 
  
  let bad_in_v2 = List.filter (fun fn->String.contains fn '-' ) v2 ;; 
  
  type barley = B of int * bool * int * string ;;
  
  let barley_of_filename fn = 
     let base = Cull_string.coending 4 fn in 
     let (naive_page_number,idx_in_page) = (
     match String.index_opt base '_' with 
     None -> (int_of_string(Cull_string.cobeginning 1 base),1)
     |Some sep ->
       (int_of_string(Cull_string.interval base 2 sep),
        int_of_string(Cull_string.cobeginning (sep+1) base))
     ) in 
     let c = String.get base 0 in 
     let page_number_offset = List.assoc c [
        'u',0;'v',100;'w',200;
        'n',0;'m',100;'l',200;
     ] 
     and is_in_a_footnote = (List.mem c ['n';'m';'l']) in 
     B(naive_page_number+page_number_offset,is_in_a_footnote,idx_in_page,fn) ;;    
  
  let barley_of_filename2 fn =
    try barley_of_filename fn with _ ->failwith fn ;;
  
  let unordered_barleys = Image.image barley_of_filename2 v2 ;; 
  
  let barley_order = ((
    fun (B(p1,is_in_a_footnote1,idx1,_fn1))
     (B(p2,is_in_a_footnote2,idx2,_fn2)) ->
    let trial1 = Total_ordering.for_integers p1 p2 in 
    if trial1<> Total_ordering_result_t.Equal then trial1 else 
    let trial2 = Total_ordering.standard is_in_a_footnote1 is_in_a_footnote2 in 
    if trial2<> Total_ordering_result_t.Equal then trial2 else 
    Total_ordering.for_integers idx1 idx2  
  ) : barley Total_ordering_t.t);;
  
  let barleys = Ordered.sort barley_order unordered_barleys ;;
  
  let page_set = 
    Ordered.sort Total_ordering.for_integers
    (Image.image (fun (B(p,_,_,_))->p) barleys) ;;
  
  let related_barleys page_number = 
     let temp1 = List.filter (
       fun (B(p,_,_,_))->p=page_number
     ) barleys in 
     List.partition (fun (B(_,is_in_footnote,_,_))->is_in_footnote)temp1 ;;
  
  let largest_page_number = List.hd (List.rev page_set) ;;
  
  let related_barleys_for_all_pages = 
     Int_range.scale (fun p->
       (p,related_barleys p)
    ) 1 largest_page_number ;; 
  
  let very_long_footnotes = Max.maximize_it_with_care (fun (b,(l1,l2))->
     List.length l1  
  ) related_barleys_for_all_pages ;; 
  
  let long_footnotes = List.filter (fun (b,(l1,l2))->
     (List.length l1)>1  
  ) related_barleys_for_all_pages ;; 
  
  
  let check1 = Ordered.sort Total_ordering.silex_for_intlists (Image.image (fun (p,(l1,l2))->
      Image.image (fun 
      (B(_,_,idx,_))-> idx 
      ) l1
      ) related_barleys_for_all_pages) ;;
  
  let check2 = Ordered.sort Total_ordering.silex_for_intlists (Image.image (fun (p,(l1,l2))->
      Image.image (fun 
      (B(_,_,idx,_))-> idx 
      ) l2
      ) related_barleys_for_all_pages) ;;
  
  let insert_at_all_lines (left,right) text =
     let old_lines = Lines_in_text.lines text in 
     let young_lines = Image.image (
      fun line -> 
        if List.mem (Cull_string.trim_spaces line) 
          ["";"«";"<";"L";"€"]
        then ""  
        else left ^ line ^ right
     ) old_lines in 
     String.concat "\n" young_lines ;;
  
  let remove_unusable_lines text =
     let old_lines = Lines_in_text.lines text in 
     let young_lines = List.filter (
      fun line -> 
        not(List.mem (Cull_string.trim_spaces line) 
          ["";"«";"<";"L";"€"])
     ) old_lines in 
     String.concat "\n" young_lines ;;   
  
  let geometry_from_page_number page_number =
    if page_number mod 2 = 0 
    then "\\newgeometry{paper=a4paper,left=3.3cm,right=5.6cm,textheight=24cm}\n"
    else "\\newgeometry{paper=a4paper,left=5.6cm,right=3.3cm,textheight=24cm}\n";;    
  
  let pages_without_numbering = [
     1;14;15;53;98;180;210;234;244;245;247
  ] ;;
  
  let lines_for_page_numbering page_number = 
    if List.mem page_number pages_without_numbering
    then ""
    else 
    "\\centerline{-"^(string_of_int page_number) ^"-}\n" ^
    "\\bigskip\n" ^ 
    "\\noindent ";;  
  
  let extract_contents l = String.concat "\n\n\n" (Image.image (fun 
      (B(_,_,_,fn))-> Io.read_whole_file 
      (Absolute_path.of_string(dir2^"/"^fn)) 
      ) l );;
     
  
  let contents_for_all_pages = Image.image (fun (p,(l1,l2))->
     (p,
     let footnote_content = 
      remove_unusable_lines(insert_at_all_lines ("\\indent","\\newline\n") (extract_contents l1))
     and nonfootnote_content = insert_at_all_lines ("","\\linebreak") (extract_contents l2) in 
     (footnote_content,nonfootnote_content)) 
      ) related_barleys_for_all_pages ;;    
  
  let precomputed_pages = [
     49;87;150;240
  ];;
  
  let building_site = home ^ "/Teuliou/html_files/Translations/Building_site2/" ;;
  
  let content_for_precomputed_page page_number = 
    let ap = Absolute_path.of_string (
      building_site ^ 
      "p" ^ (string_of_int page_number) ^ ".txt"
    ) in 
    Io.read_whole_file ap ;;
  
  
  let total_page_content 
    (page_number,(footnote_content,nonfootnote_content)) = 
    if List.mem page_number precomputed_pages
    then content_for_precomputed_page page_number
    else     
    let footnote_part = (
      if footnote_content = ""
      then ""
      else "\n"^
           Check_ocr.separator_announcing_basic_footer^ 
          "\n\\normalsize\n"^ 
          "\\rule{3cm}{0.4pt}\\linebreak\n"^
          "\\bigskip\n\n"^
          footnote_content^
          "\n"
    ) in    
    ("%\n% Page "^(string_of_int page_number)^" \n%\n")^
    (geometry_from_page_number page_number)^
    "\\large\n"^
    "\\newpage\n"^ 
    (lines_for_page_numbering page_number) ^
    nonfootnote_content ^ footnote_part ;;  
  
  let contents_for_restricted_pages = List.filter (
     fun (p,_) -> (p > 57) && (p< 245)
  ) contents_for_all_pages ;;
  
  let total_book_content = 
    String.concat "\n"
     (Image.image total_page_content contents_for_restricted_pages)  ;;  
  
  let emptiable_ap = Absolute_path.of_string (building_site ^ "emptiable_bv.txt") ;;
  
  let write_book () = Io.overwrite_with emptiable_ap total_book_content;;
  
  let building_site = home ^ "/Teuliou/html_files/Translations/Building_site2/" ;;
let tex_site = home ^ "/Teuliou/LaTeX/Brouilhedou/" ;;

let emptiable_ap = Absolute_path.of_string (building_site ^ "emptiable_bv.txt")
let polished_ap = Absolute_path.of_string (tex_site ^ "bonvallet.tex")
let walker_ap = Absolute_path.of_string (tex_site ^ "walker.tex") ;;

let last_walker_copy_ap = Absolute_path.of_string (building_site ^ "walker.txt")
let backup_copy = Absolute_path.of_string (building_site ^ "bonvallet.txt")

let do_backup () =
   Io.overwrite_with backup_copy (Io.read_whole_file polished_ap) ;;

let markers_in_walker_file = 
    ("%Moving part starts here\n\n","\n\n%Moving part ends here") ;;

let write_to_walker text =
   Replace_inside.overwrite_between_markers_inside_file
     ~overwriter:text 
     markers_in_walker_file walker_ap ;;

let read_from_walker () =
    Cull_string.between_markers 
    markers_in_walker_file (Io.read_whole_file walker_ap) ;;

let ref_for_expected_action = ref None ;;

let is_a_digit c = List.mem c 
    ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'];;

let is_not_a_digit c = not(is_a_digit c) ;;

let seek_positive_integer_at_index text idx =
    if (idx<0)||(idx > (String.length text)) then None else 
    let c = String.get text (idx-1) in 
    if is_not_a_digit c then None else  
    let j_opt = Strung.char_finder_from_inclusive_opt is_not_a_digit text idx in 
    let next_idx = (match j_opt with None -> (String.length text)+1 | Some j -> j) in 
    let written_integer = int_of_string(Cull_string.interval text idx (next_idx-1)) in 
    Some(written_integer,next_idx) ;;   

let seek_substring_at_index substr text idx = 
    if Substring.is_a_substring_located_at substr text idx 
    then Some(idx+(String.length substr))
    else None ;; 
    
let seek_naive_reference_at_index text idx =     
  match seek_substring_at_index "(" text idx with 
  None -> None 
  | Some idx2 -> 
     (
      match seek_positive_integer_at_index text idx2 with 
           None -> None 
           | Some(written_integer,idx3) -> 
      (match seek_substring_at_index ")" text idx3 with 
      None -> None 
      | Some idx4 -> Some(written_integer,idx4)
     ));;

let has_naive_reference line =
   match Substring.leftmost_index_of_in_from_opt "(" line 1 with 
   None -> false 
   |Some idx -> (seek_naive_reference_at_index line idx) <> None;;

let idx_of_last_nonempty_line indexed_lines = 
  let temp = List.rev indexed_lines in    
  fst(List.find (fun (_,line) ->Cull_string.trim_spaces line <> "") temp);;

let string_ends_with s t =
    let ls=String.length s 
    and lt=String.length t in 
    if lt > ls then false else 
    (Cull_string.ending lt s) = t;;  

let string_before_line_ender s =
    match List.find_opt(string_ends_with s)
      ["\\linebreak";"\\newline"] with 
    None -> s 
    |Some ender -> Cull_string.coending (String.length ender) s ;;
      
(*

string_before_line_ender "123\\linebreak" ;;
string_before_line_ender "456\\newline" ;;

*)

      
let deal_with_line_ending inserted_ender trimmed_line =
   if List.exists (fun p->String.starts_with trimmed_line ~prefix:p) [
     "%";"\\newgeometry";"\\large";"\\newpage";"\\centerline";
     "\\bigskip";"\\varthreestars"
   ]
   then trimmed_line
   else (string_before_line_ender trimmed_line) ^ inserted_ender ;;
   
let deal_with_line_endings_in_paragraph special_treatment_for_last_line text = 
   let nonempty_lines = Lines_in_text.lines text in 
   if not special_treatment_for_last_line 
   then String.concat "\n" 
         (Image.image (deal_with_line_ending "\\linebreak")  nonempty_lines)
   else 
   let indexed_nonempty_lines = Int_range.index_everything nonempty_lines 
   and m = List.length nonempty_lines in
   let modifier = (
      fun (line_idx,line) -> 
         let ender = if line_idx=m then "\\newline" else "\\linebreak" in 
         deal_with_line_ending ender line
   ) in 
   String.concat "\n" 
   (Image.image modifier  indexed_nonempty_lines) ;;



let rewrite_main_text main_text =
   let indexed_mt_lines = Lines_in_text.indexed_lines main_text in 
   let line_is_a_paragraph_closer =  (
     fun (line_idx,line) -> 
      match List.assoc_opt (line_idx+1) indexed_mt_lines with
       None -> false 
       |Some next_line -> Cull_string.trim_spaces next_line = ""
   )  in  
   let last_line_idx = idx_of_last_nonempty_line indexed_mt_lines in
   let new_mt_lines = Image.image (
     fun pair -> 
      let (line_idx,line) = pair in 
      let trimmed_line = Cull_string.trim_spaces line in
      if trimmed_line = ""
      then line 
      else   
      if line_idx=last_line_idx 
      then let ender = (
             if String.ends_with (string_before_line_ender trimmed_line) ~suffix:"."
             then "\\newline"
             else "\\linebreak" 
           ) in 
           deal_with_line_ending ender line 
      else          
      if (line_is_a_paragraph_closer pair)
      then deal_with_line_ending "\\newline" line 
      else deal_with_line_ending "\\linebreak" line 
   ) indexed_mt_lines in 
   String.concat "\n" new_mt_lines ;;

let rewrite_footer footer =
   let footer_lines = Lines_in_text.indexed_lines footer in 
   let lines_for_new_footnotes = List.filter_map (
     fun (line_idx,line) -> 
      if has_naive_reference line then Some line_idx else None  
   ) footer_lines in  
   let line_is_not_a_paragraph_closer =  (
     fun (line_idx,line) -> 
      match List.assoc_opt (line_idx+1) footer_lines with
       None -> false (* we cannot know a priori if the last line is
                continued on the next page or not,
                but the most common case is that it is not continued
               *)
       |Some next_line -> (Cull_string.trim_spaces next_line) <> ""
   )  in  
   let new_footer_lines = Image.image (
     fun pair -> 
      let (line_idx,line) = pair in
      let possible_actions = [
        not(List.mem line_idx lines_for_new_footnotes),
           ("\\indent",""); 
        (not(List.mem (line_idx+1) lines_for_new_footnotes))
         &&(line_is_not_a_paragraph_closer pair), 
            ("\\newline","\\linebreak");  
      ] in 
      let effective_actions = List.filter_map (
         fun (should_do,action) -> if should_do then Some action else None
      ) possible_actions in  
      Replace_inside.replace_several_inside_text effective_actions line
   ) footer_lines in 
   Replace_inside.replace_several_inside_text 
     [("\\newline\n\\indent","\\newline\n\n\n\\indent");
      ",\\newline",".\\newline"]
   (String.concat "\n" new_footer_lines) ;;

let rewrite_full_text full_text =
   let sep = Check_ocr.separator_announcing_basic_footer in 
   match Substring.leftmost_index_of_in_from_opt 
      sep full_text 1 with 
   None -> rewrite_main_text full_text 
   |Some sep_idx -> 
   let main_text = Cull_string.beginning (sep_idx-1) full_text 
   and footer = Cull_string.cobeginning (sep_idx+(String.length sep)) full_text in 
   let new_main_text = rewrite_main_text main_text 
   and new_footer = rewrite_footer footer in 
   new_main_text^sep^"\n"^new_footer ;;

let put_first_page_on_walker () =
  if !ref_for_expected_action = Some "officialize"
  then
    failwith
      "You just pushed a page. You need to officialize it before putting another page"
  else (
    let (first_page, new_text1) =
      Percent_pagination.extract_first_page_in_file emptiable_ap
    in
    do_backup();
    Io.overwrite_with emptiable_ap new_text1;
    Io.overwrite_with last_walker_copy_ap (Io.read_whole_file walker_ap);
    write_to_walker (rewrite_full_text first_page);
    ref_for_expected_action := Some "officialize")
;;

let markers_in_polished_file = 
    ("%Core starts here","%Core ends here") ;;
let officialize () =
  if !ref_for_expected_action = Some "push page"
  then failwith "You just officialized a page. No need to officialize it a second time"
  else (
    let walker_text = read_from_walker () in
    let _ = Check_ocr.check_basic_footnotes_on_page walker_text in
    let old_core = Cull_string.between_markers
       markers_in_polished_file (Io.read_whole_file polished_ap) in
    let new_core = old_core ^ "\n\n" ^ walker_text in
    Replace_inside.overwrite_between_markers_inside_file
      ~overwriter:new_core markers_in_polished_file polished_ap;
    do_backup();
    ref_for_expected_action := Some "push page")
;;


let this_ap =
  Absolute_path.of_string
    (home ^ "/Teuliou/OCaml/skeptical_duck/watched/watched_not_githubbed/bonvallet.ml")
;;

Incremental_replace_on_a_set_of_files.set_replacements_datafile this_ap

let beginning_marker = "(" ^ "* Replacements begin here *)"
let end_marker = "(" ^ "* Replacements end here *)";;

Incremental_replace_on_a_set_of_files.set_markers beginning_marker end_marker;;
Incremental_replace_on_a_set_of_files.set_receiving_files [ emptiable_ap; walker_ap ]

let check_pages_and_footnotes () = 
   let core = Cull_string.between_markers
       markers_in_polished_file (Io.read_whole_file polished_ap) in 
  Check_ocr.check_basic_footnotes_on_all_pages core
;;



let adjust_linebreaks () = 
   let old_text = read_from_walker () in 
   let new_text = rewrite_full_text old_text in 
   write_to_walker new_text ;;   

let improve_line_endings ?(special_treatment_for_last_line=true) i j =
   Lines_in_text.modify_interval_inside_file
   (deal_with_line_endings_in_paragraph special_treatment_for_last_line) 
    walker_ap i j ;;

let ile = improve_line_endings ;;

let jle = improve_line_endings ~special_treatment_for_last_line:false;;

let kle = List.iter (fun j->ile j j) ;;

(* Replacements begin here *)


let replacements = [
   (", A",". A");
   (", C",". C");
   (", D",". D");
   (", E",". E");
   (", I",". I");
   (", L",". L");
   (", M",". M");
   (", N",". N");
   (", O",". O");
   (", P",". P");
   (", S",". S");
   (", T",". T");
   (", U",". U");
   ("\195\128.","A.");
   ("\226\128\153","'");
   (" ct "," et ");
   ("Tbid","Ibid");
   ("co!l.","col.");
   ("Kbid,","Ibid.,");
   (" col, "," col. ");
   ("A L V,","A. L. V.");
   ("A L V.","A. I. V.");
   ("Fbid.,","Ibid.,");
   ("Ihid.,","Ibid.,");
   (" luite "," lutte ");
   ("A 1, V.","A. I. V.");
   ("A 1. V.","A. I. V.");
   ("A I, V.","A. I. V.");
   ("A T, V.","A. I. V.");
   ("A T. V.","A. I. V.");
   ("A. ! V.","A. I. V.");
   ("A. L V,","A. I. V.");
   ("A. L V.","A. I. V.");
   ("A. T V.","A. I. V.");
   ("A, 1, V.","A. I. V.");
   ("A, 1. V.","A. I. V.");
   ("A. 1. V.","A. I. V.");
   ("A. I, V.","A. I. V.");
   ("A. L, V.","A. I. V.");
   ("A. L. V.","A. I. V.");
   ("A. T, V.","A. I. V.");
   ("A. T. V.","A. I. V.");
   ("Depping. ","Depping, ");
   ("Manuserit","Manuscrit");
   ("Morice. P","Morice, P");
   ("Preuves. ","Preuves, ");
   ("De Bouetiez","Du Bouetiez");
   ("R\195\169billon. ","R\195\169billon, ");
   ("Martini\195\168re. L","Martini\195\168re, L");
   ("\\varthreestars\\newline","\\varthreestars");
];;


(* Replacements end here *)

Incremental_replace_on_a_set_of_files.initialize_replacements replacements

(*
   On startup, you can make a few clean-up initializations as follows :
*)


let p = put_first_page_on_walker
let o = officialize

let op () =
  o ();
  p ()
;;

let r (a, b) = Incremental_replace_on_a_set_of_files.add_new_replacement (a, b)
let f = check_pages_and_footnotes ;;

let enforce_replacements () = Chronometer.it Incremental_replace_on_a_set_of_files.apply_all ()

let after1_ap = Absolute_path.of_string 
(building_site ^ "bonvallet_after_part1.txt") ;;
let act () =
Lines_in_text.remove_interval_in_file after1_ap 5 4933;;


let part1_ap = Absolute_path.of_string 
(building_site ^ "bonvallet_part1.txt") ;;
let act () =
Lines_in_text.remove_interval_in_file part1_ap 2247 2308;;

let part2_ap = Absolute_path.of_string 
(building_site ^ "bonvallet_part2.txt") ;;

let act () =
Lines_in_text.remove_interval_in_file part2_ap 5656 10818;;

let part3_ap = Absolute_path.of_string 
(building_site ^ "bonvallet_part3.txt") ;;
let act () =
Lines_in_text.remove_interval_in_file part3_ap 15 10583;;


end ;;


(************************************************************************************************************************
Snippet 161 : Use the tesseract command on many files
************************************************************************************************************************)
module Snip161=struct

let dir1 = home ^ "/Teuliou/Heavy/Workshop/Tesserable/Action" ;;

let active_mode = ref false ;;

let u1 = 
  if !active_mode 
  then Unix_again.quick_beheaded_complete_ls dir1 
   else [];; 


let u2 = List.filter (fun fn->String.ends_with ~suffix:".png" fn ) u1 ;; 

let commands = Image.image (
  fun fn ->
    let base = Cull_string.coending 4 fn in 
    "tesseract "^fn^" "^base^" "^
    "-l fra --tessdata-dir /usr/share/tesseract-ocr/5/tessdata/"
) u2 ;; 

let go_to_dir1 () = Sys.chdir dir1 ;;

let execute_commands () = 
   Explicit.image Sys.command commands ;; 



end ;;


(************************************************************************************************************************
Snippet 160 : Last use of Cee modules
************************************************************************************************************************)
module Snip160=struct

(*

module Cap = Cee_project_transfer.Capsule ;;

let list28 = (!(Cee_data_archiver.Cache_Content.list28_ref)) ;;
let wardrobe1 = (!(Cee_data_archiver.Cache_Content.wardrobe1_ref)) ;; 


let simplified_dir = Sys.getenv "SHPSRC" ^ "/" ;; 
let half_preprocessed_dir = Sys.getenv "HAHPSRC" ^ "/" ;;

let cpsl0 =
  Cap.make ~source_envname:"SHPSRC" 
  ~destination_envname:"HAHPSRC" list28 
  ~reinitialize_destination:true ;;

let computation1 () = Chronometer.it 
    Cap.shadows_for_dc_files cpsl0 ;; (* 10 minutes,21 seconds *)

let change1 () =
  Chronometer.it
    Cee_project_transfer
    .remove_conditional_directives_in_directly_compiled_files 
     cpsl0 ;; (* 9 seconds *)

let cpsl1 =
   Cap.replicate ~next_envname:"ST01PHPSRC" cpsl0 
   ~reinitialize_destination:true  ;;      

let change2 =
  Cee_project_transfer.standardize_inclusions_in_files cpsl1
    (Cap.directly_compiled_files cpsl1) ;;

let cpsl2 =
  Cap.replicate ~next_envname:"ST02PHPSRC" cpsl1 
   ~reinitialize_destination:true ;; 

let check_change2 () =
  Cee_project_transfer.standardize_inclusions_in_files cpsl2
    (Cap.directly_compiled_files cpsl2) ~dry_run:true ;;  

let change3 =
  Cee_project_transfer.standardize_guards_in_files cpsl2
    (Cap.directly_included_files cpsl2) ;;    


let files = Cap.directly_included_files cpsl2 ;;

let could_fail fn = 
    let old_text = Cap.read_file cpsl2 fn in
    Cee_text.standardize_guard_in_text_opt old_text ;;

let check_failure fn =
   try (fun _->false)(could_fail fn) with _ -> true ;;


let u2 = Explicit.filter check_failure files ;;  

let fn = "ext/fileinfo/libmagic/magic.h" ;;

let old_text = Cap.read_file cpsl2 fn ;;

module Prj = Cee_text.Private ;;

open Prj ;;                                                             
let old_text = rf "~/Teuliou/Experimenting_with_php/step-01-php-src/ext/fileinfo/libmagic/magic.h";;

let old_text = rf "nonml_files/example.h" ;;

let bad1 = Cee_text.standardize_guard_in_text_opt old_text ;;

let bad_text1 = Option.get bad1 ;; 

let bad2 = Prj.test_text_for_guard_pattern old_text ;;
let w0 ()= Walker.make old_text ;;
let w1 = Walker.iterate_for_guard_pattern_detection (w0 ()) ;;

let test_text_for_guard_pattern text =
   Guard_Pattern.test 
   (Walker.iterate_for_guard_pattern_detection (Walker.make text)) ;;

let bad4 = Lines_in_text.lines_inside_or_outside_cee_comments 
     old_text ;;




module Pri = Lines_in_text.Private ;;

open Pri ;;
let bad3 = Walker.make old_text ;;
let w1 = (old_text,String.length old_text,[],1,false) ;;



let rec step 
  (whole_text,total_length,treated_lines,next_idx_to_be_treated,unfinished_comment)= 
  if next_idx_to_be_treated > total_length 
  then failwith("aaa")
  else 
  match next_newline_inside_or_outside_cee_comments_opt  
        whole_text next_idx_to_be_treated unfinished_comment with
  None -> failwith("bbb")
  |Some(newline_idx,unfinished_comment2) ->
     let line= Cull_string.interval whole_text next_idx_to_be_treated (newline_idx-1) in  
  (whole_text,total_length,(line,unfinished_comment)::treated_lines,newline_idx+1,unfinished_comment2);; 

let ff = Memoized.small step w1 ;;

let (whole_text,total_length,treated_lines,next_idx_to_be_treated,unfinished_comment)=
   ff 81 ;;


*)


end ;;


(************************************************************************************************************************
Snippet 159 : Using the Coherent_pdf module
************************************************************************************************************************)
module Snip159=struct

let special_soi j = 
   let s = string_of_int j in 
   if j<10 then "0"^s else s ;;

let u1 = Int_range.scale (
   fun k ->
   let sk=string_of_int k in 
   "convert p"^sk^".png p"^sk^".pdf"
) 1 99 ;;

let u2 = Int_range.scale (
   fun k ->
   let sk=special_soi k in 
   "convert q"^sk^".png p"^(string_of_int(100+k))^".pdf"
) 0 99 ;;

let u3 = Int_range.scale (
   fun k ->
   let sk=special_soi k in 
   "convert r"^sk^".png p"^(string_of_int(200+k))^".pdf"
) 0 48 ;;

let pngs_to_pdfs = u1 @ u2 @ u3 ;;

let act1 () = Sys.chdir (home^"/Pictures/Screenshots") ;;

let act2 () = Image.image Sys.command pngs_to_pdfs ;; 

let cmd ="cpdf intro.pdf "^(
  (String.concat " " (Int_range.scale 
  (fun k->"p"^(string_of_int k)^".pdf") 1 248) )
)^" -o avel3.pdf";;

let act3 () = Sys.command cmd ;;

let ap1 = Absolute_path.of_string 
"~/Teuliou/Heavy/Workshop/contraventions.pdf";;

let ap1 = Absolute_path.of_string 
"~/Teuliou/Heavy/Workshop/first_page.pdf";;

let ap1 = Absolute_path.of_string 
"~/Downloads/Herri_Delfin/herri.pdf";;

let ap1 = Absolute_path.of_string 
"~/Teuliou/Heavy/Workshop/Bonvallet/almost_all_bonvallet.pdf";;

let ap2 = Absolute_path.of_string 
"~/Teuliou/Heavy/Workshop/Bonvallet/pstarter.pdf";;

let ap3 = Absolute_path.of_string 
"~/Teuliou/Heavy/Workshop/Bonvallet/bonvallet.pdf";;

let ap4 = Absolute_path.of_string 
"~/Teuliou/Heavy/Workshop/Bonvallet/bonvallet_with_uniform_pagesize.pdf";;

let ap5 = Absolute_path.of_string 
"~/Teuliou/Heavy/Workshop/Bonvallet/bonvallet_front_page.pdf";;



let act4 () = Coherent_pdf.force_same_size_for_all_pages
ap3 ~outputfile_name:"banvollet"
    ~forced_width:595 ~forced_height:842
;;

let see1 = Coherent_pdf.sizes_for_each_page ap1 ;;

let fl_mean l = (List.fold_left (+.) 0. l) /. (float_of_int(List.length l)) ;;
let act4 () = Coherent_pdf.force_same_size_for_all_pages
ap1 ~outputfile_name:"herri"
    ~forced_width:595 ~forced_height:842
;;

let act5 () = Coherent_pdf.corep_foldable_transform
ap4 ~outputfile_name:"printable_remasterised_bonvallet_without_front_page"
;;

let act6 () = Coherent_pdf.corep_foldable_transform
ap5 ~outputfile_name:"printable_remasterised_bonvallet_front_page"
;;

let ap2 = Absolute_path.of_string 
"~/Teuliou/Heavy/bonvallet_remasterised.pdf";;

let act6 () = Coherent_pdf.corep_cuttable_transform
ap2 ~outputfile_name:"printable_remasterised_bonvallet"
;;

end ;;


(************************************************************************************************************************
Snippet 158 : Function commuting with dynamical system, version 3
************************************************************************************************************************)
module Snip158=struct

module ZZ = Zirath.Z ;;
module ZQ = Zirath.Q ;;

let s_epsilon="1/1"^(String.make 32 '0') ;;

let epsilon = ZQ.of_string s_epsilon ;;

let almost_one = ZQ.sub ZQ.one epsilon;;

let adhoc_printer (fmt:Format.formatter) x=
   let s = (
      if ZQ.equals x almost_one 
      then "1-e"
      else ZQ.to_string x) in 
   Format.fprintf fmt "@[%s@]" s;;

(*
#install_printer adhoc_printer ;;
*)

let a_half = ZQ.of_ints 1 2 ;;
let q29 = ZQ.of_ints 2 9 ;;

let q49 = ZQ.of_ints 4 9 ;;

let unit_interval = Rational_interval_t.I(
   ZQ.zero,ZQ.one
) ;;

let good_interval_for_x = Rational_interval_t.I(
   ZQ.zero,almost_one
) ;;

let good_interval_for_y = Rational_interval_t.I(
   q29,q49
) ;;

let is_x_good itv = Rational_interval.is_included_in itv good_interval_for_x ;;

let is_y_good itv = Rational_interval.is_included_in itv good_interval_for_y ;;



type left_or_right = Left | Right ;; 

module Left_or_right = struct
  
let from_interval (Rational_interval_t.I(a,b)) =
   if ZQ.leq b a_half then Left else 
   if ZQ.leq a_half a then Right else  
   failwith("Error in Left_or_right.from_interval") ;;  

end ;;

type affine_form = Aff of ZQ.t * ZQ.t ;;

module Affine_form = struct
   
module Private = struct 

   let compose (Aff(a1,b1)) (Aff(a2,b2)) = 
   Aff(ZQ.mul a1 a2,ZQ.add (ZQ.mul a1 b2) b1) ;; 

   let of_side = function
     Left -> Aff(ZQ.of_int 2,ZQ.zero)
     |Right -> Aff(ZQ.of_int (-2),ZQ.of_int 2) ;;
     
end ;;   

let apply (Aff(a,b)) x = ZQ.add (ZQ.mul a x) b ;;   
   
let apply_inverse (Aff(a,b)) x = ZQ.div (ZQ.sub x b) a ;;

let apply_on_interval aff (Rational_interval_t.I(u,v)) =
   let (Aff(a,_b)) = aff 
   and u2 = apply aff u 
   and v2 = apply aff v in 
   if ZQ.lt a ZQ.zero 
   then Rational_interval_t.I(v2,u2) 
   else Rational_interval_t.I(u2,v2) ;;  

let compose = Private.compose ;;

let dot dir aff = Private.compose (Private.of_side dir) aff ;;

let inverse_on_interval aff (Rational_interval_t.I(u,v)) =
   let (Aff(a,_b)) = aff 
   and u2 = apply_inverse aff u 
   and v2 = apply_inverse aff v in 
   if ZQ.lt a ZQ.zero 
   then Rational_interval_t.I(v2,u2) 
   else Rational_interval_t.I(u2,v2) ;;  

let inverse_on_intervals aff l =
    let (Aff(a,b)) = aff 
    and temp = Image.image (inverse_on_interval aff) l in 
    if ZQ.lt a ZQ.zero 
    then List.rev temp 
    else temp ;;  

let of_side = Private.of_side ;;

end ;;   

type arrow = {
   interval_for_v: Rational_interval_t.t ;
   path_for_v: left_or_right list ;
   form_for_last_v: affine_form;
   interval_for_penultimate_v: Rational_interval_t.t ;
   interval_for_last_v: Rational_interval_t.t ;
   current_limit: int;
} ;;

module Arrow = struct 

let compute_cases arw l =
    let cases_for_last_v = 
      Rational_interval.divide (arw.interval_for_last_v) l in 
    Affine_form.inverse_on_intervals 
      arw.form_for_last_v cases_for_last_v ;;   
let origin = {
   interval_for_v= unit_interval  ;
   path_for_v= [] ;
   form_for_last_v = Aff(ZQ.one,ZQ.zero);
   interval_for_penultimate_v = unit_interval ;
   interval_for_last_v = unit_interval ;
   current_limit = 0;
} ;;   

let restrict arw smaller_interval = 
   let penultimate_interval =
       Affine_form.apply_on_interval
         arw.form_for_last_v smaller_interval in 
   let new_direction = Left_or_right.from_interval penultimate_interval in 
   let new_form = Affine_form.dot  new_direction arw.form_for_last_v in 

    {
   interval_for_v= smaller_interval  ;
   path_for_v= (arw.path_for_v)@[new_direction] ;
   form_for_last_v = new_form ;
   interval_for_penultimate_v = penultimate_interval; 
   interval_for_last_v = Affine_form.apply_on_interval new_form smaller_interval;
   current_limit = (arw.current_limit)+1;
} ;;   



end ;;

module Y_Arrow = struct 

let current_good_interval_for_y = Rational_interval_t.I(
   ZQ.zero,q49
) ;;

let itv_is_good itv = 
   Rational_interval.is_included_in 
      itv current_good_interval_for_y ;;

let is_good arw = itv_is_good arw.interval_for_penultimate_v ;;
let decompose arw =
    let parts = Arrow.compute_cases arw [q29;q49;a_half] in 
    Image.image (Arrow.restrict arw) parts ;;

let adhoc_order = Total_ordering.product 
   Rational_interval.order Total_ordering.standard ;;

let decompose_several l_arw =
    let new_elts = List.flatten (Image.image decompose l_arw) in      
    let (dead,alive) =List.partition  is_good new_elts in 
    (Image.image (
      fun arw -> 
         let l = arw.path_for_v in 
         (arw.interval_for_v,List.rev(List.tl(List.rev l)))
    ) dead,alive);; 



let main_pusher (all_dead,some_dead,alive)=
  let (newly_dead,still_alive) = decompose_several alive in 
  (Ordered.merge adhoc_order newly_dead all_dead,newly_dead,still_alive) ;;  

let main =
     Memoized.small main_pusher ([],[],[Arrow.origin]) ;; 

end ;;   


type walker_element = {
   arrow_for_x: arrow ;
   arrow_for_y: arrow ;
} ;;

module We = struct 

let current_limit we = we.arrow_for_x.current_limit ;;   

let form_for_last_x we = we.arrow_for_x.form_for_last_v ;;
let form_for_last_y we = we.arrow_for_y.form_for_last_v ;;

let interval_for_last_x we = we.arrow_for_x.interval_for_last_v ;;
let interval_for_last_y we = we.arrow_for_y.interval_for_last_v ;;
let interval_for_x we = we.arrow_for_x.interval_for_v ;;
let interval_for_y we = we.arrow_for_y.interval_for_v ;;

let is_good we =
   (is_x_good we.arrow_for_x.interval_for_penultimate_v)
   &&
   (is_y_good we.arrow_for_y.interval_for_penultimate_v) ;;


let origin = {
   arrow_for_x = Arrow.origin ;
   arrow_for_y = Arrow.origin ;
} ;;

let path_for_x we = we.arrow_for_x.path_for_v ;;
let path_for_y we = we.arrow_for_y.path_for_v ;;
end ;;   




module Walker_element = struct

let restrict we (itv_for_x,itv_for_y) = 
   {
      arrow_for_x = Arrow.restrict we.arrow_for_x itv_for_x ;
      arrow_for_y = Arrow.restrict we.arrow_for_y itv_for_y ;
   }  ;;

let parts_from_explosion we =
    let cases_for_x = Arrow.compute_cases we.arrow_for_x 
             [a_half;almost_one] in 
    let temp = Image.image (
      fun case_for_x ->
         let points_for_y = (
            if Rational_interval.is_included_in case_for_x 
               good_interval_for_x 
            then [q29;q49;a_half] else [a_half]
         ) in 
         let cases_for_y = Arrow.compute_cases 
             we.arrow_for_y points_for_y in 
     Image.image (fun case_for_y ->(case_for_x,case_for_y)) cases_for_y 
    ) cases_for_x in 
    List.flatten temp ;;

let decompose we =
    let parts = parts_from_explosion we in 
    let new_elts = Image.image (restrict we) parts in 
    List.partition  We.is_good new_elts;; 


end ;;    

module Dimension1 = struct 

let main (v,history,frm)=
    let side = (
        if ZQ.leq v a_half 
        then Left  
        else Right 
    ) in 
    let aff = Affine_form.of_side side in 
    let new_v = Affine_form.apply aff v in 
    (new_v,side::history,Affine_form.compose aff frm)

let starter v = (v,[],Aff(ZQ.one,ZQ.zero)) ;;
let iterate v = Memoized.small main (starter v) ;;

let rec iterator_for_measure (j,walker) =
    if j>1000 then failwith("Measure not found") else 
    let (v,_,_) = walker in 
    if Rational_interval.mem v good_interval_for_y 
    then (j,walker)
    else iterator_for_measure (j+1,main walker) ;; 

let measure v = iterator_for_measure (1,main (starter v)) ;; 

end ;;   

module Dimension2 = struct 

let main (vx,vy)=
    (Dimension1.main vx,Dimension1.main vy) ;;

let starter (vx,vy) = 
    (Dimension1.starter vx,Dimension1.starter vy) ;;
let iterate (vx,vy) k 
= (Dimension1.iterate vx k,Dimension1.iterate vy k) ;;

let rec iterator_for_measure (j,walker) =
    if j>1000 then failwith("Measure not found") else 
    let ((vx,_,_),(vy,_,_)) = walker in 
    if (Rational_interval.mem vx good_interval_for_x)
       && 
       (Rational_interval.mem vy good_interval_for_y) 
    then (j,walker)
    else iterator_for_measure (j+1,main walker) ;; 

let measure v = iterator_for_measure (1,main (starter v)) ;; 

end ;;   

Dimension2.measure (ZQ.add a_half epsilon,q29) ;;

Dimension2.measure (ZQ.sub a_half (ZQ.div epsilon (ZQ.of_int 2)) ,q29) ;;

Dimension2.measure 
(ZQ.add a_half (ZQ.mul (ZQ.of_ints 3 8) epsilon ) ,q29) ;;

(*

let ff k =
    let (all_dead,newly_dead,alive) = Y_Arrow.main k in 
    Image.image (fun (
      Rational_interval_t.I(a,b)  
    ,l)->(a,b,l)) newly_dead ;;

let starter = [Arrow.origin] ;;
 
let (dead1,alive1) = Y_Arrow.decompose_several starter ;;



let pivot1 = We.origin ;;

let (dead1,alive1) = Walker_element.decompose pivot1 ;;

let pivot2 = List.nth alive1 1 ;;

let (dead2,alive2) = Walker_element.decompose pivot2 ;;

let pivot3 = List.hd alive2 ;;

let (dead3,alive3) = Walker_element.decompose pivot3 ;;

let pivot4 = List.hd alive3 ;;

let (dead4,alive4) = Walker_element.decompose pivot4 ;;

let pivot5 = List.hd alive4 ;;

let (dead5,alive5) = Walker_element.decompose pivot5 ;;

let pivot6 = List.hd alive5 ;;

let (dead6,alive6) = Walker_element.decompose pivot6 ;;


let z1 = (List.nth dead5 1).arrow_for_x.interval_for_v ;;
let (Rational_interval_t.I(_,z2)) = z1 ;;
let z3 = ZQ.div almost_one z2 ;;

*)


end ;;


(************************************************************************************************************************
Snippet 157 : Function commuting with dynamical system, version 2
************************************************************************************************************************)
module Snip157=struct

module ZZ = Zirath.Z ;;
module ZQ = Zirath.Q ;;

(*
#install_printer ZZ.trinp_out ;;
#install_printer ZQ.trinp_out ;;
*)

let ref_for_unlocalized = ref ZQ.zero ;;   
exception Unlocalized of ZQ.t ;;
let raise_unlocalized q = 
   let _ = (ref_for_unlocalized:=q) in raise(Unlocalized(q)) ;; 

let z_double = ZZ.mul (ZZ.of_int 2) ;;
let z_odouble = ZZ.mul (ZZ.of_int (-2)) ;;

let z_num_alf=ZZ.of_string "366025403784438646763723170752936183471402626905190314027903489725966508454400018540573093378624287837813070707703351514984972547499476239405827756047" ;;
let z_den_alf=ZZ.of_string "500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";;  

let z_square x = ZZ.mul x x ;;

let z_quadratic_form_1 d n = 
   (* is nonnegative when n/d<=sqrt(3), when n,d>0 *)
      ZZ.sub (ZZ.mul (ZZ.of_int 3) (z_square d)) (z_square n) ;; 

let z_quadratic_form_2 d n = 
   (* is nonnegative when n/d<=sqrt(3)-1, when n,d>0 *)
      z_quadratic_form_1 d (ZZ.add d n) ;; 

let should_be_positive = 
   z_quadratic_form_2 z_den_alf (ZZ.add (ZZ.of_int 0) z_num_alf)  ;; 

let should_be_negative = 
   z_quadratic_form_2 z_den_alf (ZZ.add (ZZ.of_int 1) z_num_alf)  ;;    

(*

we deduce that alf is between 
(z_num_alf)/z_den_alf and (z_num_alf+1)/z_den_alf

*)

let lower_bound_for_alf = ZQ.make z_num_alf z_den_alf ;;

let upper_bound_for_alf = ZQ.make (ZZ.add (ZZ.of_int 1) z_num_alf) z_den_alf ;;


type comparator = Greater | Lower ;;

let compare_alf_to q =
   if ZQ.leq q lower_bound_for_alf then Greater else 
   if ZQ.leq upper_bound_for_alf q then Lower else  
   raise_unlocalized q ;;   

type statement_on_alf = 
   Alf_is_below of ZQ.t * float
   |Alf_is_above of ZQ.t * float ;;

type alf_form = AF of ZZ.t * ZZ.t ;;

let decider (AF(a,b)) = 
   let nuum = ZZ.sub ZZ.one (ZZ.mul (ZZ.of_int 2) b)
   and deen = ZZ.mul (ZZ.of_int 2) a in 
   ZQ.make nuum deen ;;
   

let compare_to_one_half frm =
   let (AF(a,b)) = frm in  
   let d = decider frm in 
   let t = compare_alf_to d in 
   if ZZ.leq ZZ.zero a 
   then t 
   else
   match t with     
     Greater -> Lower
    | Lower -> Greater ;;


let left_f (AF(a,b)) = AF(z_double a,z_double b) ;;

let right_f (AF(a,b)) = AF(z_odouble a,ZZ.add (ZZ.of_int 2) (z_odouble b)) ;;

let ff frm = 
   match compare_to_one_half frm with 
    Lower -> left_f frm 
    |Greater -> right_f frm ;;

let affine_interpolation (AF(a1,b1)) (AF(a2,b2)) =
   let multiplier = ZQ.make a2 a1 in 
   let det = ZZ.sub (ZZ.mul a1 b2) (ZZ.mul a2 b1) in 
   (multiplier,ZQ.make det a1) ;;

let to_float (AF(a,b)) =
    let qa = ZQ.of_zirath a and qb = ZQ.of_zirath b in 
    ZQ.to_float (ZQ.add (ZQ.mul qa lower_bound_for_alf) qb) ;; 

let needed_statement_to_compute_ff frm =
   let (AF(a,b)) = frm in  
   let d = decider frm in 
   match compare_alf_to d with  
     Greater -> Alf_is_above(d,ZQ.to_float d)
    | Lower -> Alf_is_below(d,ZQ.to_float d) ;;

let x0 = AF(ZZ.of_int 1,ZZ.of_int 0) ;;
let y0 = AF(ZZ.of_int (-3),ZZ.of_int 3) ;;

let xx = Memoized.small ff x0 ;;

let yy = Memoized.small ff y0 ;;

let gg k=
  let xk= xx k 
  and yk= yy k in 
  (xk,to_float xk,yk,to_float yk,
   affine_interpolation xk yk,
   needed_statement_to_compute_ff (xx(k-1)),
   needed_statement_to_compute_ff (yy(k-1))
  ) ;; 

let v1 = Int_range.scale (fun k->
  affine_interpolation (xx k) (yy k)   
)  1 100 ;;

let v2 = Image.image (fun (x,y) ->
  (int_of_string(ZQ.to_string x),int_of_string(ZQ.to_string y))   
) v1 ;;

let donald_index (c1,c0) =
 let pair = (int_of_string(ZQ.to_string c1),int_of_string(ZQ.to_string c0)) in 
 Option.get(List.find_index (fun t->t=pair) [(-3, 2); (3, -2); (3, 0)])+1 ;;

let v3 = Ordered.sort Total_ordering.standard2 v2 ;; 

let v4 = Int_range.scale (fun k->
  donald_index(affine_interpolation (xx k) (yy k))   
)  1 100 ;;

affine_interpolation (xx 0) (yy 0) ;;     



end ;;


(************************************************************************************************************************
Snippet 156 : Function commuting with dynamical system
************************************************************************************************************************)
module Snip156=struct

module ZZ = Zirath.Z ;;
module ZQ = Zirath.Q ;;

(*
#install_printer ZZ.trinp_out ;;
#install_printer ZQ.trinp_out ;;
*)

let z_num_alf=ZZ.of_string "366025403784438646763723170752936183471402626905190314027903489725966508454400018540573093378624287837813070707703351514984972547499476239405827756047" ;;
let z_den_alf=ZZ.of_string "500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";;  

let z_square x = ZZ.mul x x ;;

let z_quadratic_form_1 d n = 
   (* is nonnegative when n/d<=sqrt(3), when n,d>0 *)
      ZZ.sub (ZZ.mul (ZZ.of_int 3) (z_square d)) (z_square n) ;; 

let z_quadratic_form_2 d n = 
   (* is nonnegative when n/d<=sqrt(3)-1, when n,d>0 *)
      z_quadratic_form_1 d (ZZ.add d n) ;; 

let should_be_positive = 
   z_quadratic_form_2 z_den_alf (ZZ.add (ZZ.of_int 0) z_num_alf)  ;; 

let should_be_negative = 
   z_quadratic_form_2 z_den_alf (ZZ.add (ZZ.of_int 1) z_num_alf)  ;;    

(*

we deduce that alf is between 
(z_num_alf)/z_den_alf and (z_num_alf+1)/z_den_alf

*)

type alf_form = AF of ZZ.t * ZZ.t ;;

(*
let is_below_one_half frm =
    let (AF(a,b)) = *)


let num_alf = ZQ.of_zirath z_num_alf ;;

let den_alf = ZQ.of_zirath z_den_alf ;;


let alf = ZQ.div num_alf den_alf ;;

let beth = ZQ.mul (ZQ.of_int 3) (ZQ.sub ZQ.one alf) ;;

let a_half = ZQ.of_ints 1 2 ;;

let ff r =
    if ZQ.lt r a_half 
    then ZQ.mul (ZQ.of_int 2) r 
    else ZQ.mul (ZQ.of_int 2) (ZQ.sub ZQ.one r) ;;

let alf_seq = Memoized.small ff alf ;; 
let beth_seq = Memoized.small ff beth ;;

let alfbeth = Memoized.make(fun k->(alf_seq k,beth_seq k)) ;;

let t_order = 
    Total_ordering.triple_product 
         ZQ.order ZQ.order Total_ordering.for_integers ;; 

let u1 = Int_range.scale (fun k->(alf_seq k,beth_seq k,k)) 0 100 ;;
     
let u2 = Ordered.sort t_order u1 ;; 



let u3 = 
   let n = List.length u2 
   and get = (fun k->List.nth u2 (k-1)) in 
   Int_range.scale (fun j->(get j,get (j+1),get (j+2))) 1 (n-2) ;;

let saddle_points = 
    List.filter (fun 
      ((a1,b1,k1),(a2,b2,k2),(a3,b3,k3)) ->
         (ZQ.order b1 b2)<>(ZQ.order b2 b3)
    ) u3;;

let tf1 (a,b,k) = (ZQ.to_float a,ZQ.to_float b,k) ;;     

let tf2 (t1,t2) = (tf1 t1,tf1 t2) ;;
let tf3 (t1,t2,t3) = (tf1 t1,tf1 t2,tf1 t3) ;;

let float_saddle_points = Image.image tf3 saddle_points;;

let saddle_indices = 
   Image.image (fun ((a1,b1,k1),(a2,b2,k2),(a3,b3,k3)) ->k2) 
   saddle_points;;

let u4 = 
   let n = List.length u2 
   and get = (fun k->List.nth u2 (k-1)) in 
   Int_range.scale (fun j->(get j,get (j+1))) 1 (n-1) ;;

let preimages_of_one_half = 
   List.filter (fun
     ((a1,b1,k1),(a2,b2,k2)) ->
        (ZQ.leq b1 a_half) && (ZQ.leq a_half b2)
   )  u4;; 

let float_preimages_of_one_half = Image.image tf2 preimages_of_one_half;;

(*


let ab k = (ZQ.to_float(alf_seq k),ZQ.to_float(beth_seq k)) ;; 

let ratio i j =
    let nuum=ZQ.sub(beth_seq i)(beth_seq j)
    and deen=ZQ.sub(alf_seq i)(alf_seq j) in 
    ZQ.abs(ZQ.div nuum deen) ;; 

let dbeth i j =ZQ.abs(ZQ.sub(beth_seq i)(beth_seq j)) ;;     

let minimize_zrational_with_care f=function
[]->failwith("careful min on empty set undefined")
|x::y->
 let rec tempf=(function
  (current_candidates,current_value,to_be_treated)->match to_be_treated with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                if (ZQ.lt va current_value)
				then tempf([a],va,peurrest)
				else if (ZQ.equals va current_value)
				     then tempf(a::current_candidates,current_value,peurrest)
					 else tempf(current_candidates,current_value,peurrest)
 ) 
in
tempf([x],f(x),y);;

let maximize_zrational_with_care f=function
[]->failwith("careful min on empty set undefined")
|x::y->
 let rec tempf=(function
  (current_candidates,current_value,to_be_treated)->match to_be_treated with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                if (ZQ.gt va current_value)
				then tempf([a],va,peurrest)
				else if (ZQ.equals va current_value)
				     then tempf(a::current_candidates,current_value,peurrest)
					 else tempf(current_candidates,current_value,peurrest)
 ) 
in
tempf([x],f(x),y);;


let zrational_order = ((fun x y ->
  if ZQ.lt x y then Total_ordering_result_t.Lower else 
  if ZQ.equals x y then Total_ordering_result_t.Equal else 
  Total_ordering_result_t.Greater     
): ZQ.t Total_ordering_t.t) ;;

let haddock_order = Total_ordering.product 
    zrational_order Total_ordering.for_integers ;; 

let ordering_for_iterates_of_alf n =
    let temp1 = Int_range.scale (fun j->(alf_seq j,j)) 1 n in 
    let temp2 = Ordered.sort haddock_order temp1 in 
    Image.image snd temp2 ;;

let gg n = 
    let temp1 = ordering_for_iterates_of_alf n in
    let temp2 = List_again.universal_delta_list temp1 in 
    let (r,sols)=maximize_zrational_with_care (fun (i,j)->dbeth i j) temp2 in 
    (r,sols) ;;

let see_gg n = 
   let (a,b) = gg n in 
   (ZQ.to_float a,b) ;;

let bound = 100 ;;

let hh n = fst(gg n) ;;

let hh2 n = Ordered.max zrational_order [hh(n);hh(n+1)] ;;

let hh3 n = Ordered.max zrational_order 
  [hh(n);hh(n+1);hh(n+2)] ;;


let u1 = minimize_zrational_with_care hh
  (Int_range.range 2 bound) ;;

let u2 = minimize_zrational_with_care hh2
  (Int_range.range 2 bound) ;;

let u3 = minimize_zrational_with_care hh3
  (Int_range.range 2 bound) ;;  


let generalized_ratio d k =
    snd(Max.maximize_it (fun j->ratio(k+j)) 
     (Int_range.range 0 (d-1)));;    


let bound = 100 ;; 

let measure =Memoized.make(fun d -> minimize_zrational_with_care
  (generalized_ratio d) (Int_range.range 2 (bound-d+1)));;

  let ra k = ZQ.to_float(ratio k);;

*)  


end ;;


(************************************************************************************************************************
Snippet 155 : Unfinished work on functional equation f(xyf(x+y))=f(x)+f(y)
************************************************************************************************************************)
module Snip155=struct


module ZZ = Zirath.Z ;;
module ZQ = Zirath.Q ;;

(*
#install_printer ZZ.trinp_out ;;
#install_printer ZQ.trinp_out ;;
*)
(*

(4,4) -> 1
(3,6) -> 1 
(8,8) -> 2
(12,12) -> 3
(10,15) -> 3
(16,16) -> 4  
(20,20) -> 5

*)

let starter_ref = ref [1;2;3] ;;

starter_ref := [1;2;3;5;7;9;10;11;13;14;17;18;19] ;;

let test_pairs_from l = 
    let temp1 = Image.image ZZ.of_int l in 
    let temp2 = Uple.inclusive_list_of_pairs temp1 in 
    let temp3 = Image.image (
      fun (x,y) -> 
         let qx = ZQ.of_zirath x 
         and qy = ZQ.of_zirath y in 
         let nuum = ZQ.mul qx qy 
         and dane = ZQ.mul (ZQ.of_int 2) (ZQ.add qx qy) in 
         (x,y,ZQ.div nuum dane)
    ) temp2 in 
    List.filter (fun (x,y,q) ->
        ZZ.equals (ZQ.den q) (ZZ.one)
      ) temp3 ;; 

let generic_tt2 starter n = 
    let m = List.hd(List.rev starter)+2 in 
    test_pairs_from (
   starter @ (Int_range.range m n)) ;; 

let next_compatible_value starter =
    let m = List.hd(List.rev starter)+2 in 
    let tempf = Memoized.make(fun n->
      generic_tt2 starter n
    ) in 
    if tempf(m)=[] then m else 
    let tempfg=(fun j->List.length(tempf j)=List.length(tempf (j-1))) 
    and walker = ref(m+1) in
    let _ = (while not(tempfg(!walker)) 
    do walker:=(!walker)+1 done) in 
    !walker ;;

let next_threshhold_values starter =
    let m = List.hd(List.rev starter)+2 in 
    let tempf = Memoized.make(fun n->
      generic_tt2 starter n
    ) in 
    if tempf(m)=[] then m else 
    let tempfg=(fun j->List.length(tempf j)=List.length(tempf (j-1))) 
    and walker = ref(m+1) in
    let _ = (while not(tempfg(!walker)) 
    do walker:=(!walker)+1 done) in 
    !walker ;;



starter_ref := [1;2;3;5;7;9;10;11;13;14;17;18;19] ;;
next_compatible_value(!starter_ref) ;; 
generic_tt2 (!starter_ref) 21 ;;



end ;;


(************************************************************************************************************************
Snippet 154 : Construct a 3-cover of A7
************************************************************************************************************************)
module Snip154=struct


let a_perm = [2;1;4;3;5;6] ;;
let b_perm = [3;4;1;2;5;6] ;;
let c_perm = [1;2;4;3;6;5] ;;
let d_perm = [3;4;5;6;1;2] ;;

let list_for_sg1 = Permutation.generated_subgroup 
 [a_perm;b_perm;c_perm;d_perm]
;;

type sa1_actor = SA1 of int list ;;

module Sa1_actor = struct 

module Private = struct 

let towards_order (SA1 s1) (SA1 s2) = 
   Total_ordering.lex_compare
     Total_ordering.for_integers s1 s2 ;;

end ;;  

let apply_on_list (SA1 s) l= 
  let n = List.length s 
  and inv_s = Permutation.inverse s in 
  Int_range.scale (fun j->List.nth l ((List.nth inv_s j)-1)) 0 (n-1)
;;


let identity = SA1 [1;2;3;4;5;6] ;;

let order = (Private.towards_order: sa1_actor Total_ordering_t.t) ;;

let prod (SA1 s1) (SA1 s2) = SA1(Permutation.product s1 s2) ;;

(*

To check the action is correctly defined :

let l1 = [1;2;3;4;5] ;;
let actor1 = SA1 [2;3;4;1;5] ;;
let actor2 = SA1 [1;3;4;5;2] ;; 
let actor12 = Sa1_actor.prod actor1 actor2 ;; 

let version1 = Sa1_actor.apply_on_list actor12 l1 ;;
let version2 = Sa1_actor.apply_on_list actor1 
   (Sa1_actor.apply_on_list actor2 l1) ;;
*)

let sg1_subgroup = Image.image (fun perm->SA1(perm)) list_for_sg1 ;;

end ;;  

type restricted_w_number = R_U |R_W |R_W2 ;;

module Restricted_w_number = struct 

module Private = struct 
  
let mul_w = function R_U ->R_W | R_W->R_W2 |R_W2 -> R_U ;;
let mul_w2 = function R_U ->R_W2 | R_W->R_U |R_W2 -> R_W ;;

end ;;  

let mul x y = match x with 
  R_U ->y | R_W->Private.mul_w y |R_W2 -> Private.mul_w2 y ;;
 
let order = Total_ordering.from_list [R_U;R_W;R_W2] ;;  

end ;;  


type w_number = WN of int * int ;;

exception Unrecognized of w_number ;; 

module W_number = struct

module Private = struct 

let mul_w (WN(x,y))= WN(-y,x-y);;

let mul_w2 (WN(x,y))= WN(y-x,-x);;

let integer_level x =
  if x=0 then 0 else
  if x<0 then 2 else 1 ;;
    

let adhoc_on_integers = ((fun x y->
   let trial1 = Total_ordering.for_integers (abs x) (abs y) in 
   if trial1 <> Total_ordering_result_t.Equal then trial1 else 
   Total_ordering.for_integers (integer_level x) (integer_level y)
))

let towards_order (WN(x1,y1)) (WN(x2,y2)) = 
  let trial1 = adhoc_on_integers y1 y2 in 
  if trial1 <> Total_ordering_result_t.Equal then trial1 else 
  adhoc_on_integers x1 x2 ;;

let add (WN(x1,y1)) (WN(x2,y2)) = WN(x1+x2,y1+y2) ;; 

let fold_sum l = List.fold_left add (WN(0,0)) l ;;

end ;;  


let fold_sum = Private.fold_sum ;;

let mul (WN(x1,y1)) (WN(x2,y2)) = WN(x1*x2-y1*y2,x1*y2+x2*y1-y1*y2) ;;

let omega = WN(0,1) ;;

let omega_squared = WN(-1,-1) ;;

let one = WN(1,0) ;;

let order = (Private.towards_order : w_number Total_ordering_t.t) ;;

let recognize_restricted x = try List.assoc x 
 [
   WN (1, 0),R_U;
   WN (0, 1),R_W;
   WN (-1, -1),R_W2;
   WN (2, 0),R_U;
   WN (0, 2),R_W;
   WN (-2, -2),R_W2;
 ] with 
 Not_found -> raise(Unrecognized(x));;  

let rotate x y = match x with 
  R_U ->y | R_W->Private.mul_w y |R_W2 -> Private.mul_w2 y ;;

let sesquilinear (WN(x1,x2)) (WN(y1,y2)) = 
    WN(x1*y1+x2*y2-x1*y2,x2*y1-x1*y2) ;;
  

let two = WN(2,0) ;;

let zero = WN(0,0) ;;



end ;;


type w_uple = Wu of w_number * w_number * w_number * w_number * w_number * w_number ;;


module W_uple = struct
  
module Private = struct 
  
let of_list li =
    let th = (fun k->List.nth li (k-1)) in 
    Wu(th 1,th 2,th 3,th 4,th 5,th 6) ;; 
let to_list (Wu(x1,x2,x3,x4,x5,x6))= [x1;x2;x3;x4;x5;x6] ;;

let towards_order x y  =
    Total_ordering.lex_compare
       W_number.order (to_list x) (to_list y) ;;

let nonzero_indices x=
    let li = to_list x
    and zero=WN(0,0) in 
    List.filter (fun k->List.nth li (k-1)<>zero) [1;2;3;4;5;6] ;;


end ;;  

let act sg x=
    let old_li = Private.to_list x in 
    let new_li = Sa1_actor.apply_on_list sg old_li in 
    Private.of_list new_li;;



let dot_product w x=
  Private.of_list(
    Image.image (W_number.rotate w)(Private.to_list x)
  ) ;;
  



let image w =
  let li = Private.to_list w in 
  let idx_set = Private.nonzero_indices w in 
  Image.image (fun k->List.nth li (k-1)) idx_set;;
    
let image_size w =
   List.length(Ordered.sort W_number.order (image w)) ;;  

let nonzero_indices = Private.nonzero_indices ;;

let occurrences w v = 
  let temp1 = Int_range.index_everything (Private.to_list w) in 
  List.filter_map (fun (idx,other_v)->
    if other_v = v then Some idx else None
  ) temp1 ;;

let of_list = Private.of_list ;;

let order = (Private.towards_order: w_uple Total_ordering_t.t) ;;

let repeated_elements w = 
  let im = image w in 
  let sorted_im = Ordered.sort W_number.order im in 
  List.filter (fun v->
    List.length(List.filter(fun z->z=v) im)>1  
  ) sorted_im ;;

let test_orthogonality x y = 
   let temp1 = List.combine(Private.to_list x)(Private.to_list y) in 
   let temp2 = Image.image (fun (u,v)->W_number.sesquilinear u v) temp1 in  
  (W_number.fold_sum temp2)=WN(0,0) ;;

let to_list = Private.to_list ;;
 


end ;;


type direction =
   E of int 
  |F of int * int 
  |G of int * int * int * int ;;

module Direction = struct
  
module Private = struct  

  
let to_w_mapper = function 
E(i) -> (fun t->if t=i then W_number.two else W_number.zero )
|F(j,k)-> (fun t->if (t=j)||(t=k) then W_number.zero else W_number.one )
|G(i,j,k,l) -> (
   fun t -> match List.assoc_opt t 
    [i,W_number.omega;j,W_number.omega_squared;
     k,W_number.one;l,W_number.one
    ] with 
   Some answer -> answer 
   | None -> W_number.zero 
) ;;

let to_w_uple dcn =
  W_uple.of_list(Int_range.scale (to_w_mapper dcn) 1 6) ;;

let level = function 
 E(i) -> 1
|F(j,k)-> 2
|G(i,j,k,l) -> 3 ;;

let unordered_support = function 
 E(i) -> [i]
|F(j,k)-> [j;k]
|G(i,j,k,l) -> [i;j;k;l] ;;

let i_order = Total_ordering.for_integers ;;

let support dcn = Ordered.sort i_order (unordered_support dcn) ;; 

let towards_order x y = 
  let trial1 = Total_ordering.for_integers (level x) (level y) in 
  if trial1<>Total_ordering_result_t.Equal then trial1 else 
  Total_ordering.lex_compare i_order (support x) (support y) ;;  

end ;;

let order = (Private.towards_order: direction Total_ordering_t.t) ;;


let test_orthogonality x y = 
   W_uple.test_orthogonality(Private.to_w_uple x)(Private.to_w_uple y)  ;;

let to_w_uple = Private.to_w_uple ;;

end ;;


type summary=Sy of restricted_w_number *  direction ;;



module Summary = struct 

exception Bad_index_size of w_uple * (int list);;  

module Private = struct
  
let towards_order (Sy(r1,dcn1)) (Sy(r2,dcn2)) = 
  let trial1 = Direction.order dcn1 dcn2 in 
  if trial1<>Total_ordering_result_t.Equal then trial1 else 
  Restricted_w_number.order r1 r2 ;;  

end ;;

let of_w_uple u =
  let li1 = W_uple.to_list u in 
  let idx_set = W_uple.nonzero_indices u in 
  if List.length(idx_set)=1
  then let i = List.hd(idx_set) in 
       let v = List.nth li1 (i-1) in 
       let r = W_number.recognize_restricted v in 
       Sy(r,E(i))
  else 
  if List.length(idx_set)=4
  then if W_uple.image_size(u)=1 
       then let v = List.hd(W_uple.image u) in 
            let zeroes = Ordered.setminus Total_ordering.for_integers
             [1; 2; 3; 4; 5; 6] idx_set in 
            let j1 = List.nth zeroes 0 
            and j2 = List.nth zeroes 1 in  
            let r = W_number.recognize_restricted v in 
            Sy(r,F(j1,j2))
            
       else 
            let r = List.hd(W_uple.repeated_elements u) in 
            let rw = W_number.rotate R_W r  
            and rw2 = W_number.rotate R_W2 r in 
            let doubled = W_uple.occurrences u r in 
            let i = List.hd(W_uple.occurrences u rw)
            and j = List.hd(W_uple.occurrences u rw2)
            and k = List.nth doubled 0 
            and l = List.nth doubled 1 in 
            Sy(W_number.recognize_restricted r,G(i,j,k,l))
  else  raise(Bad_index_size(u,idx_set)) ;; 

let order = (Private.towards_order: summary Total_ordering_t.t) ;;


let test_orthogonality (Sy(_,dcn1)) (Sy(_,dcn2)) = 
   Direction.test_orthogonality dcn1 dcn2 ;;

let to_w_uple (Sy(r,dcn)) =
   W_uple.dot_product r (Direction.to_w_uple dcn) ;;


end ;;  


type restricted_sixtuple = 
  Rs of 
  restricted_w_number * restricted_w_number * restricted_w_number * 
  restricted_w_number * restricted_w_number * restricted_w_number ;; 

module Restricted_sixtuple = struct 
  
module Private = struct 
  
let of_list li =
    let th = (fun k->List.nth li (k-1)) in 
    Rs(th 1,th 2,th 3,th 4,th 5,th 6) ;; 
let to_list (Rs(x1,x2,x3,x4,x5,x6))= [x1;x2;x3;x4;x5;x6] ;;

let towards_order x y =
    Total_ordering.lex_compare
       Restricted_w_number.order 
       (to_list x) (to_list y) ;;


end ;;  

let act sg x=
    let old_li = Private.to_list x in 
    let new_li = Sa1_actor.apply_on_list sg old_li in 
    Private.of_list new_li ;;

let all_ones = Rs(R_U,R_U,R_U,R_U,R_U,R_U);;

let mul x y =
   let temp1 = List.combine(Private.to_list x)(Private.to_list y) in 
   let temp2 = Image.image (fun (u,v)->Restricted_w_number.mul u v) temp1 in  
   Private.of_list temp2 ;;



let of_list = Private.of_list ;;

let order = (Private.towards_order: restricted_sixtuple Total_ordering_t.t) ;;

let to_list = Private.to_list ;;

end ;;  


(* 
Beware the two operations do not commute !
The permutation is applied first, then the multiplication
by a diagonal matrix.
*)
type director = Dr of restricted_sixtuple * sa1_actor;;

module Director = struct 

module Private = struct 

let towards_order (Dr(rs1,sa1)) (Dr(rs2,sa2))=
 (Total_ordering.product 
   Restricted_sixtuple.order Sa1_actor.order) (rs1,sa1) (rs2,sa2) ;;

end ;;  

let act (Dr(restricted_uple,sa)) old_uple = 
  let new_uple = W_uple.act sa old_uple in 
  let temp1 = List.combine(Restricted_sixtuple.to_list restricted_uple)(W_uple.to_list new_uple) in 
  let temp2 = Image.image (fun (r,v)->W_number.rotate r v) temp1 in 
  W_uple.of_list temp2 ;; 
  
let identity = Dr(
  Restricted_sixtuple.all_ones,Sa1_actor.identity);;

let order = (Private.towards_order: director Total_ordering_t.t) ;;

let prod (Dr(rs1,sa1)) (Dr(rs2,sa2))=
  let twisted_rs2 = Restricted_sixtuple.act sa1 rs2 in 
  Dr(Restricted_sixtuple.mul rs1 twisted_rs2,Sa1_actor.prod sa1 sa2) ;;    

(*

To check the action is correctly defined :

let one=WN(1,0);;
let src = Wu(one,one,one,one,one,one) ;;
let actor1 = Dr(Rs(R_U,R_W,R_U,R_W,R_U,R_W),SA1 [2;3;4;1;5;6]) ;;
let actor2 = Dr(Rs(R_W2,R_U,R_W2,R_U,R_W2,R_U),SA1 [1;3;4;5;2;6]) ;; 
let actor12 = Director.prod actor1 actor2 ;; 

let version1 = Director.act actor12 src ;;
let version2 = Director.act actor1 
   (Director.act actor2 src) ;;
*)

end ;;   

module Sg2_subgroup = struct 

module Private = struct 

   let o_order = Director.order ;;

   let o_merge = Ordered.merge o_order ;;
   let o_setminus = Ordered.setminus o_order ;;
   let o_sort = Ordered.sort o_order ;;

  let pusher_for_generated_subgroup l (whole,to_be_treated) =
      let temp1 = Cartesian.product to_be_treated l in
      let unordered_temp2 = Image.image (fun (x,y)->Director.prod x y) temp1 in 
      let temp2 = o_sort unordered_temp2 in 
      let new_elements = o_setminus temp2 whole in 
      let new_whole = o_merge new_elements whole in 
      (new_whole,new_elements) ;;

   let rec iterator_for_generated_subgroup l walker =
       if snd walker = []
       then fst walker
       else iterator_for_generated_subgroup l 
        (pusher_for_generated_subgroup l walker);;  

   let generated_subgroup l = 
      let id = Director.identity in 
       iterator_for_generated_subgroup l ([id],[id]) ;; 

   let starter = 
      (Image.image (fun sg->
        Dr(Restricted_sixtuple.all_ones,sg)) Sa1_actor.sg1_subgroup)
      @[
        Dr(Rs(R_U,R_U,R_U,R_U,R_W2,R_W),
          SA1([2;3;1;4;5;6])
        )
       ];;  

end ;;  

let sg2 = Private.generated_subgroup Private.starter ;;

end ;;  

module Vector_system = struct 



  let wu_order = W_uple.order ;;

  let wu_sort = Ordered.sort wu_order ;;

let z = WN(0,0) ;;

let o = WN(1,0) ;;

let w = WN(0,1) ;;

let w2 = WN(-1,-1) ;;

let u1 = [
   Wu(z,z,o,o,o,o);
   Wu(o,z,o,z,w,w2);
]  ;;  

let u2 = 
  let temp1 = Cartesian.product [R_U;R_W;R_W2] u1 in 
  let temp2 = Image.image (fun (w,u)->
    W_uple.dot_product w u) temp1 in  
  wu_sort temp2 ;;  

let the_45 = 
  let temp1 = Cartesian.product Sa1_actor.sg1_subgroup u2 in 
  let temp2 = Image.image (fun (sa,u)->
    W_uple.act sa u) temp1 in  
  wu_sort temp2 ;; 

let other_seed = Wu(WN(2,0),z,z,z,z,z) ;;

let the_additional_18 = 
  let temp1 = Image.image (fun dr->
    Director.act dr other_seed) Sg2_subgroup.sg2 in  
  wu_sort temp1 ;; 

let the_original_63 = wu_sort (the_45@the_additional_18) ;;

let check1 = (hi the_45,hi the_additional_18,hi the_original_63) ;;

let g1 = Image.image (fun u->(u,Summary.of_w_uple u)) the_original_63 ;;
let check_summaries = List.filter (
  fun (u,s)->(Summary.to_w_uple s)<>u
) g1 ;;

let the_unordered_63 = Image.image snd g1 ;;

let the_63 = Ordered.sort Summary.order the_unordered_63;;

end ;;

(*
module Frames_of_w_uples = struct

let towards_level2 = Uple.list_of_pairs Vector_system.the_original_63 ;;

let level2 = List.filter (
  fun (w1,w2) -> W_uple.test_orthogonality w1 w2 
) towards_level2 ;;

let towards_level3 = Uple.list_of_pairs level2 ;;

let level3 =
   List.filter_map (
    fun ((x1,x2),(y2,y3)) ->
      if (y2=x2) && (W_uple.test_orthogonality x1 y3)
      then Some(x1,x2,y3)
      else None
   ) towards_level3;;

let towards_level4 = Uple.list_of_pairs level3 ;;

let level4 =
   List.filter_map (
    fun ((x1,x2,x3),(y2,y3,y4)) ->
      if (y2=x2) && (y3=x3) && (W_uple.test_orthogonality x1 y4)
      then Some(x1,x2,x3,y4)
      else None
   ) towards_level4;;

let towards_level5 = Uple.list_of_pairs level4 ;;

let level5 =
   List.filter_map (
    fun ((x1,x2,x3,x4),(y2,y3,y4,y5)) ->
      if (y2=x2) && (y3=x3) && (y4=x4) 
        && (W_uple.test_orthogonality x1 y5)
      then Some(x1,x2,x3,x4,y5)
      else None
   ) towards_level5;;   

let towards_level6 = Uple.list_of_pairs level5 ;;

let level6 =
   List.filter_map (
    fun ((x1,x2,x3,x4,x5),(y2,y3,y4,y5,y6)) ->
      if (y2=x2) && (y3=x3) && (y4=x4) && (y5=x5) 
        && (W_uple.test_orthogonality x1 y6)
      then Some(x1,x2,x3,x4,x5,y6)
      else None
   ) towards_level6;;   
(*
let listified_redundant_level6 = Image.image (
  fun (u1,u2,u3,u4,u5,u6) ->
    [u1;u2;u3;u4;u5;u6]
) redundant_level6 ;;  

let listified_level6 = 
  Ordered.sort 
  (Total_ordering.lex_compare W_uple.order) listified_redundant_level6 ;; 
*) 


end ;;  

module Frames_of_summaries = struct

let towards_level2 = Uple.list_of_pairs Vector_system.the_63 ;;

let level2 = List.filter (
  fun (w1,w2) -> Summary.test_orthogonality w1 w2 
) towards_level2 ;;

let towards_level3 = Uple.list_of_pairs level2 ;;

let level3 =
   List.filter_map (
    fun ((x1,x2),(y2,y3)) ->
      if (y2=x2) && (Summary.test_orthogonality x1 y3)
      then Some(x1,x2,y3)
      else None
   ) towards_level3;;

let towards_level4 = Uple.list_of_pairs level3 ;;

let level4 =
   List.filter_map (
    fun ((x1,x2,x3),(y2,y3,y4)) ->
      if (y2=x2) && (y3=x3) && (Summary.test_orthogonality x1 y4)
      then Some(x1,x2,x3,y4)
      else None
   ) towards_level4;;

let towards_level5 = Uple.list_of_pairs level4 ;;

let level5 =
   List.filter_map (
    fun ((x1,x2,x3,x4),(y2,y3,y4,y5)) ->
      if (y2=x2) && (y3=x3) && (y4=x4) 
        && (Summary.test_orthogonality x1 y5)
      then Some(x1,x2,x3,x4,y5)
      else None
   ) towards_level5;;   

let towards_level6 = Uple.list_of_pairs level5 ;;

let level6 =
   List.filter_map (
    fun ((x1,x2,x3,x4,x5),(y2,y3,y4,y5,y6)) ->
      if (y2=x2) && (y3=x3) && (y4=x4) && (y5=x5) 
        && (Summary.test_orthogonality x1 y6)
      then Some(x1,x2,x3,x4,x5,y6)
      else None
   ) towards_level6;;   

end ;;  
*)

module Frames_of_directions = struct

let the_21 =
  Ordered.sort Direction.order
  (Image.image (fun (Sy(_,dcn))->dcn) Vector_system.the_63);;

let towards_level2 = Uple.list_of_pairs the_21 ;;

let level2 = List.filter (
  fun (w1,w2) -> Direction.test_orthogonality w1 w2 
) towards_level2 ;;

let towards_level3 = Uple.list_of_pairs level2 ;;

let level3 =
   List.filter_map (
    fun ((x1,x2),(y2,y3)) ->
      if (y2=x2) && (Direction.test_orthogonality x1 y3)
      then Some(x1,x2,y3)
      else None
   ) towards_level3;;

let towards_level4 = Uple.list_of_pairs level3 ;;

let level4 =
   List.filter_map (
    fun ((x1,x2,x3),(y2,y3,y4)) ->
      if (y2=x2) && (y3=x3) && (Direction.test_orthogonality x1 y4)
      then Some(x1,x2,x3,y4)
      else None
   ) towards_level4;;

let towards_level5 = Uple.list_of_pairs level4 ;;

let level5 =
   List.filter_map (
    fun ((x1,x2,x3,x4),(y2,y3,y4,y5)) ->
      if (y2=x2) && (y3=x3) && (y4=x4) 
        && (Direction.test_orthogonality x1 y5)
      then Some(x1,x2,x3,x4,y5)
      else None
   ) towards_level5;;   

let towards_level6 = Uple.list_of_pairs level5 ;;

let level6 =
   List.filter_map (
    fun ((x1,x2,x3,x4,x5),(y2,y3,y4,y5,y6)) ->
      if (y2=x2) && (y3=x3) && (y4=x4) && (y5=x5) 
        && (Direction.test_orthogonality x1 y6)
      then Some(x1,x2,x3,x4,x5,y6)
      else None
   ) towards_level6;;   


end ;;  

let see1 = (
   (* hi Frames_of_w_uples.level6,
   hi Frames_of_summaries.level6, *)
   hi Frames_of_directions.level6
) ;;


let ze=W_number.zero ;;

let on=W_number.one ;;
let tw =W_number.two ;;

let ww = W_number.omega ;;
let w2 = W_number.omega_squared ;;

let s1 =Summary.of_w_uple (W_uple.of_list [tw;ze;ze;ze;ze;ze]) ;;
let s2 = Summary.of_w_uple (W_uple.of_list [ze;ze;on;on;on;on]) ;;
let s3 =Summary.of_w_uple (W_uple.of_list [ze;on;ze;on;ww;w2]) ;;
let s4 =Summary.of_w_uple (W_uple.of_list [ze;on;on;ze;w2;ww]) ;;
let s5 =Summary.of_w_uple (W_uple.of_list [ze;on;w2;ww;on;ze]) ;;
let s6 =Summary.of_w_uple (W_uple.of_list [ze;on;ww;w2;ze;on]) ;;

let chosen_summaries=[s1;s2;s3;s4;s5;s6] ;;

let chosen_directions = Image.image (
  fun (Sy(r,dcn)) ->dcn
) chosen_summaries ;; 

let pairs_of_directions = Uple.list_of_pairs chosen_directions ;;

let bad_pairs = List.filter ( 
  fun (dcn1,dcn2) ->
    not(Direction.test_orthogonality dcn1 dcn2)
) pairs_of_directions ;;



end ;;


(************************************************************************************************************************
Snippet 153 : Create assistance file on the fly on another computer
************************************************************************************************************************)
module Snip153=struct

let g1=(Needed_values.abo "unix_again")@
["unix_again"];;
let g2=Image.image (
  fun old_s->
   let s= Cull_string.after_rightmost old_s '/' in 
   let full_elt = Dfn_join.to_ending (hmx s) Dfa_ending.ml in 
   Dfn_full.to_absolute_path full_elt 
) g1;;

let g3=Modularize.modularize_several "" g2;;

let ap1 = Absolute_path.create_file_if_absent 
"watched/watched_and_githubbed/painstaking.ml" ;;

Io.overwrite_with ap1 g3 ;;


end ;;


(************************************************************************************************************************
Snippet 152 : Test Cee_text.Private.compute_small_spaces_in_text
************************************************************************************************************************)
module Snip152=struct

(*

#use"watched/watched_not_githubbed/fill_cee_cache.ml";;

*)

module Cap = Cee_project_transfer.Capsule ;;

let list28 = (!(Cee_data_archiver.Cache_Content.list28_ref)) ;;
let wardrobe1 = (!(Cee_data_archiver.Cache_Content.wardrobe1_ref)) ;; 


let simplified_dir = Sys.getenv "SHPSRC" ^ "/" ;; 
let half_preprocessed_dir = Sys.getenv "HAHPSRC" ^ "/" ;;

let cpsl0 =
  Cap.make ~source_envname:"SHPSRC" 
  ~destination_envname:"HAHPSRC" list28 
  (* ~reinitialize_destination:true *) ;;

let cmds = Cap.separate_commands cpsl0 ;; 

let u1 = Image.image (
  fun cmd ->
  (Cee_compilation_command.short_name_from_separate cmd)
) cmds ;;

let could_fail fn = 
   let full_fn = 
    "~/Teuliou/Experimenting_with_php/simplified-php-src/" ^ fn in 
    let text = rf full_fn  in 
    Cee_text.Private.compute_small_spaces_in_text text ;; 

let check_failure fn =
   try (fun _->false)(could_fail fn) with _ -> true ;;


let u2 = Explicit.filter check_failure u1 ;;  

let u3 = Explicit.image could_fail u1 ;;      



let ap1 = 
  Absolute_path.of_string
  ("~/Teuliou/Experimenting_with_php/simplified-php-src/"
   ^"ext/opcache/jit/ir/ir.c"
  );;

let text1 = Io.read_whole_file ap1 ;;  

module Pri = Lines_in_text.Private ;;

open Pri;;

(* let text1 = "			case '\'': fputs(\"'\", f); break;" ;; *)

let bad1 () = Pri.lines_inside_or_outside_cee_comments text1;;

let steppe 
  (whole_text,total_length,treated_lines,next_idx_to_be_treated,unfinished_comment)= 
  if next_idx_to_be_treated > total_length 
  then failwith("aaa")
  else 
  match next_newline_inside_or_outside_cee_comments_opt  
        whole_text next_idx_to_be_treated unfinished_comment with
  None -> 
      failwith("bbb")
  |Some(newline_idx,unfinished_comment2) ->
     let line= Cull_string.interval whole_text next_idx_to_be_treated (newline_idx-1) in 
     
  (whole_text,total_length,(line,unfinished_comment)::treated_lines,newline_idx+1,unfinished_comment2)
;; 

let w0 = (text1,String.length text1,[],1,false) ;;

let ff = Memoized.small steppe w0 ;;




let  (whole_text,total_length,treated_lines,next_idx_to_be_treated,unfinished_comment) =
  ff 84 ;;

let whole_text = "\t\t\tcase '\\'': fputs(\"'\", f); break;\n\t" ;;

let next_idx_to_be_treated = 1 ;;

let unfinished_comment = false ;;

let bad2 () = next_newline_inside_or_outside_cee_comments_opt  
        whole_text next_idx_to_be_treated unfinished_comment ;;   

let w1 = initial_walker 
whole_text next_idx_to_be_treated unfinished_comment ;;

let gg = Memoized.small step w1 ;;       

let see1 = Int_range.index_everything 
   (Strung.explode whole_text) ;;

end ;;


(************************************************************************************************************************
Snippet 151 : Debugging Cee_text.Private.compute_small_spaces_in_text
************************************************************************************************************************)
module Snip151=struct


(*
let text1 = rf 
  "~/Teuliou/Experimenting_with_php/simplified-php-src/ext/fileinfo/libmagic/apprentice.c" ;;
*)

let ap1 = Absolute_path.of_string "nonml_files/example.txt" ;; 
let text1 = Io.read_whole_file ap1 ;; 

(*
let tax1 = rf "nonml_files/original.txt";;
let part1 = Cull_string.interval tax1 1 1511 ;;
let tax2 = Replace_inside.replace_inside_text (part1,"/* COMMENT 1 */") tax1 ;;

let tax3 = Replace_inside.replace_inside_text 
  ("/*\n * apprentice - make one pass through /etc/magic, learning its secrets.\n */","/* COMMENT 2 */") tax2 ;;

let cmt3 = "/*\n * Work around a bug in headers on Digital Unix.\n * At least confirmed for: OSF1 V4.0 878\n */" ;; 
  
let tax4 = Replace_inside.replace_inside_text 
   (cmt3,"/* COMMENT 3 */") tax3 ;;

let cmt4 = Cull_string.interval tax4 5018 5362 ;;

let tax5 = Replace_inside.replace_inside_text 
   (cmt4,"/* COMMENT 4 */") tax4 ;;

let cmt5 = Cull_string.interval tax5 8130 8215 ;;

let tax6 = Replace_inside.replace_inside_text 
   (cmt5,"/* COMMENT 5 */") tax5 ;;

let cmt6 = Cull_string.interval tax6 9232 9663 ;;

let tax7 = Replace_inside.replace_inside_text 
   (cmt6,"/* COMMENT 6 */") tax6 ;;   

let cmt7 = Cull_string.interval tax7 9494 9910 ;;

let tax8 = Replace_inside.replace_inside_text 
   (cmt7,"/* COMMENT 7 */") tax7 ;;    
 
let cmt8 = "/*\n\t\t * \"d\" or \"u\" by itself.\n\t\t */" ;;

let tax9 = Replace_inside.replace_inside_text 
   (cmt8,"/* COMMENT 8 */") tax8 ;;

let cmt9 = "/*\n * Handle one file or directory.\n */" ;;

let tax10 = Replace_inside.replace_inside_text 
   (cmt9,"/* COMMENT 9 */") tax9 ;;

let cmt10 = "/*\n\t * Always leave the database loaded\n\t */" ;;

let tax11 = Replace_inside.replace_inside_text 
   (cmt10,"/* COMMENT 10 */") tax10 ;;   

let cmt11 = Cull_string.interval tax11 16336 16711 ;;

let tax12 = Replace_inside.replace_inside_text 
   (cmt11,"/* COMMENT 11 */") tax11 ;;

let cmt12 = "/*\n * Get weight of this magic entry, for sorting purposes.\n */" ;;

let tax13 = Replace_inside.replace_inside_text 
   (cmt12,"/* COMMENT 12 */") tax12 ;;  

let final_tax = tax13 ;; 

Io.overwrite_with ap1 final_tax ;;

*)

(* let bad1 = New_cee_text.Private.compute_small_spaces_in_text text1 ;; *)

module Old_cee_text = struct 

  (*

#use"lib/Cee_language/cee_text.ml";;

*)

exception Lonely_Elif_exn of int ;;
exception Lonely_Else_exn of int ;;
exception Lonely_Endif_exn of int ;;
exception Double_Else_exn of int ;;
exception Standardize_inclusion_line_exn of string ;;  
exception Add_extra_inclusion_line_exn of string ;;
  
module Private = struct 

  let str_order = Total_ordering.lex_for_strings ;;
  let str_mem = Ordered.mem str_order ;;  

  let i_order = Total_ordering.for_integers ;;
   let i_merge = Ordered.merge i_order ;; 

  let intstr_order = Total_ordering.product i_order str_order ;;

  let intstr_sort = Ordered.sort intstr_order;;

  let lines_outside_cee_comments = Lines_in_text.lines ;;

  let indexed_lines_outside_cee_comments text = 
     Int_range.index_everything (lines_outside_cee_comments text) ;; 


   let sil_merge ox oy=
   let rec tempf=(function (u,v,accu)->
   if u=[] then (List.rev_append(accu)(v)) else
   if v=[] then (List.rev_append(accu)(u)) else
   let (xu,lxu)=List.hd(u) and yu=List.tl(u) 
   and (xv,lxv)=List.hd(v) and yv=List.tl(v) in
   match str_order(xu)(xv) with
     Total_ordering_result_t.Lower->tempf(yu,v,(xu,lxu)::accu)
   |Total_ordering_result_t.Equal->tempf(yu,yv,(xu,i_merge lxu lxv)::accu)
   |Total_ordering_result_t.Greater->tempf(u,yv,(xv,lxv)::accu)
   ) in
   tempf(ox,oy,[]);;
 
 let rec sil_sort  x=
   if List.length(x)<2
   then x
   else 
   let temp1=Partition_list.split_in_half(x) in
   let y1=sil_sort(fst temp1)
   and y2=sil_sort(snd temp1) in
   sil_merge y1 y2;;      
 

type line_beginning = 
  Lb_If
 |Lb_Elif
 |Lb_Else 
 |Lb_Endif 
 |Lb_Usual ;; 

   
 module Line_beginning = struct 

  let data = [
    "if", Lb_If;
    "elif", Lb_Elif;  
    "else", Lb_Else;
    "endif", Lb_Endif
]  ;;  

let compute line =
  if not(String.starts_with line ~prefix:"#")
  then Lb_Usual  
  else  
  match Strung.char_finder_from_inclusive_opt (fun c->
          not(List.mem c [' ';'\t';'\r'])
        ) line 2 with 
  None -> Lb_Usual
  |Some idx1 -> 
    (match List.find_opt (fun (lb_name,_lb)->
      Substring.is_a_substring_located_at lb_name line idx1
      ) data with 
    None -> Lb_Usual
    |Some (_,lb) -> lb 
    );;

end ;;  


 


type small_space_t = {
   namespace : int ;
   start_idx : int ;
   end_idx  : int ;
} ;;

type guard_pattern_indicators_t = {
    first_ivy_index_opt : int option ;
    elsie_or_elif_for_first_index_opt : int option;
    endif_index_for_first_ivy_opt : int option ;
    ivy_index_after_first_ivy_opt : int option;
} ;; 




module Guard_Pattern = struct
let origin = {
  first_ivy_index_opt = None;
  elsie_or_elif_for_first_index_opt = None;
  endif_index_for_first_ivy_opt = None ;
  ivy_index_after_first_ivy_opt = None;
} ;; 

let test gpi=
  (gpi.endif_index_for_first_ivy_opt <> None) &&
  (gpi.elsie_or_elif_for_first_index_opt = None) &&
  (gpi.ivy_index_after_first_ivy_opt = None)  ;;   
  
end ;;

module Walker = struct 

   module Walker_Object = struct 

    type t = {
      lines : (int *string) list ;
      current_namespace : int ;
      preceding_namespaces : int list ;
      smallest_unused_namespace_index : int ;
      small_space_start_index_opt : int option ;
      unfinished_condition : bool;
      last_directive_was_an_else : bool;
      treated : small_space_t list;
      gpi : guard_pattern_indicators_t ;
    } ;;  
  let make text = {
    lines = indexed_lines_outside_cee_comments text ;
    current_namespace = 0 ;
    preceding_namespaces = [] ;
    smallest_unused_namespace_index = 1;
    small_space_start_index_opt = None ;
    unfinished_condition = false;
    last_directive_was_an_else = false;
    treated = [];
    gpi = Guard_Pattern.origin ;
  } ;; 

  let add_to_treated small_space w = {
       w with 
       treated = small_space :: (w.treated)   
    } ;; 

  let condition_is_unfinished w = w.unfinished_condition ;; 
  let get_current_namespace w = w.current_namespace ;; 
  let get_endif_index_for_first_ivy_opt w = w.gpi.endif_index_for_first_ivy_opt ;;
  let get_elsie_or_elif_for_first_index_opt w = w.gpi.elsie_or_elif_for_first_index_opt ;;
  let get_first_ivy_index_opt w = w.gpi.first_ivy_index_opt ;;
  let get_guard_pattern_indicators w = w.gpi ;;
  let get_ivy_index_after_first_ivy_opt w = w.gpi.ivy_index_after_first_ivy_opt ;;
  let get_lines w = w.lines ;;
  let get_preceding_namespaces w = w.preceding_namespaces ;;
  let get_small_space_start_index_opt w = w.small_space_start_index_opt ;;
  let get_smallest_unused_namespace_index w = w.smallest_unused_namespace_index ;;
  let get_treated w = w.treated ;;

  let last_directive_was_an_else w = w.last_directive_was_an_else ;;

  let register_elsie_or_elif_for_first_index w line_idx =
      let new_gpi = {w.gpi with elsie_or_elif_for_first_index_opt = Some line_idx} in 
      {w with gpi = new_gpi} ;;

  let register_first_ivy_index w line_idx =
    let new_gpi = {w.gpi with first_ivy_index_opt = Some line_idx} in 
    {w with gpi = new_gpi} ;;
  
  let register_ivy_index_after_first_ivy w line_idx =  
    let new_gpi = {w.gpi with ivy_index_after_first_ivy_opt = Some line_idx} in 
    {w with gpi = new_gpi} ;;

  let register_small_space_start_index w line_idx =    
      {
        w with 
        small_space_start_index_opt = Some line_idx 
      } ;; 

  let set_condition_to_finished w =    
      {
        w with 
        unfinished_condition = false
      }   ;;
 
  let set_lines w new_set_of_lines=    
      {
        w with 
        lines = new_set_of_lines
      }   ;;    

  let set_namespace_data w 
     ~v_current_namespace
     ~v_smallest_unused_namespace_index
     ~v_preceding_namespaces
      =  {
        w with 
        current_namespace = v_current_namespace ;
        smallest_unused_namespace_index = v_smallest_unused_namespace_index;
        preceding_namespaces = v_preceding_namespaces
      } ;;

  let set_unfinished_condition_data w 
      ~current_cond_is_unfinished
      ~current_line_is_an_elsie
    ={
      w with
        small_space_start_index_opt = None ;
        unfinished_condition = current_cond_is_unfinished;
        last_directive_was_an_else = current_line_is_an_elsie ;
     } ;;



   end ;;

  let register_new_small_space_if_needed old_w w line_idx= 
    match Walker_Object.get_small_space_start_index_opt old_w with 
    None -> w
    |Some start_index ->
      let small_space = {
       namespace = Walker_Object.get_current_namespace w ;
       start_idx = start_index ;
       end_idx  = line_idx -1 ;
      } in 
      Walker_Object.add_to_treated small_space w ;; 

  let deal_with_namespace_data old_w w line_beginning= 
     match line_beginning with 
     Lb_If ->
      let old_idx = Walker_Object.get_smallest_unused_namespace_index old_w in 
      let v_current_namespace=old_idx
      and v_smallest_unused_namespace_index=old_idx + 1
      and v_preceding_namespaces=(Walker_Object.get_current_namespace old_w) :: (Walker_Object.get_preceding_namespaces old_w) in 
      Walker_Object.set_namespace_data w 
        ~v_current_namespace
        ~v_smallest_unused_namespace_index
        ~v_preceding_namespaces
     
    | Lb_Endif ->
      let preceding = Walker_Object.get_preceding_namespaces old_w in 
      let v_current_namespace=List.hd preceding
      and v_smallest_unused_namespace_index=Walker_Object.get_smallest_unused_namespace_index old_w
      and v_preceding_namespaces= List.tl preceding in 
      Walker_Object.set_namespace_data w 
        ~v_current_namespace
        ~v_smallest_unused_namespace_index
        ~v_preceding_namespaces
   | Lb_Elif | Lb_Else | Lb_Usual -> w ;;

   let deal_with_guard_pattern_detection old_w w line_beginning line_idx = 
    match line_beginning with 
    Lb_If ->
      (if ((Walker_Object.get_first_ivy_index_opt old_w) = None)
       then Walker_Object.register_first_ivy_index w line_idx
       else 
       if ((Walker_Object.get_endif_index_for_first_ivy_opt old_w) <> None) &&
          ((Walker_Object.get_ivy_index_after_first_ivy_opt old_w) = None)
       then Walker_Object.register_ivy_index_after_first_ivy w line_idx  
       else  w
       )
   | Lb_Elif | Lb_Else ->
    (if ((Walker_Object.get_first_ivy_index_opt old_w) <> None) &&
         ((Walker_Object.get_current_namespace old_w)=1) &&
         ((Walker_Object.get_elsie_or_elif_for_first_index_opt old_w) = None)
      then Walker_Object.register_elsie_or_elif_for_first_index w line_idx 
      else w  
      )
     
   | Lb_Endif ->
    (if ((Walker_Object.get_first_ivy_index_opt old_w) <> None) &&
        ((Walker_Object.get_current_namespace old_w)=1) 
     then Walker_Object.register_elsie_or_elif_for_first_index w line_idx
     else w  
   )
   | Lb_Usual -> w ;;


  let directive_step old_w line_idx line line_beginning= 
    if ((Walker_Object.get_current_namespace old_w) = 0) && (line_beginning = Lb_Elif) then raise (Lonely_Elif_exn(line_idx)) else 
    if ((Walker_Object.get_current_namespace old_w) = 0) && (line_beginning = Lb_Else) then raise (Lonely_Else_exn(line_idx)) else
    if ((Walker_Object.get_current_namespace old_w) = 0) && (line_beginning = Lb_Endif) then raise (Lonely_Endif_exn(line_idx)) else
    if (Walker_Object.last_directive_was_an_else old_w) && (line_beginning = Lb_Else) then raise (Double_Else_exn(line_idx)) else
    let current_cond_is_unfinished = (
      if List.mem line_beginning [Lb_If;Lb_Elif]
      then String.ends_with line ~suffix:"\\"
      else false 
    ) in 
    let w1 =
      Walker_Object.set_unfinished_condition_data old_w 
       ~current_cond_is_unfinished
       ~current_line_is_an_elsie:(line_beginning = Lb_Else) in 
   let w2 = register_new_small_space_if_needed old_w w1 line_idx in 
   let w3 = deal_with_namespace_data old_w w2 line_beginning in 
   let w4 = deal_with_guard_pattern_detection old_w w3 line_beginning line_idx in
   w4 ;;
  let inner_usual_step w line_idx line = 
    match Walker_Object.get_small_space_start_index_opt w  with 
    (Some _) -> w 
    |None -> 
    if not(Walker_Object.condition_is_unfinished w)
    then Walker_Object.register_small_space_start_index w line_idx 
    else
    if String.ends_with line ~suffix:"\\"
    then w
    else Walker_Object.set_condition_to_finished w ;; 

  let usual_step w line_idx line =
    let w2 = inner_usual_step w line_idx line in 
    if (Walker_Object.get_lines w2) = []
    then register_new_small_space_if_needed w2 w2 (line_idx+1)
    else w2 ;;    
    

  let step old_w = 
    let (line_idx,line) = List.hd (Walker_Object.get_lines old_w) 
    and w = Walker_Object.set_lines old_w  
          (List.tl(Walker_Object.get_lines old_w)) in 
    let line_beginning = Line_beginning.compute line in 
    match line_beginning with 
    Lb_Usual -> usual_step w line_idx line 
   |Lb_If |Lb_Elif |Lb_Else |Lb_Endif -> directive_step w line_idx line line_beginning ;;
  

  let rec iterate w =
    if (Walker_Object.get_lines w) = [] 
    then List.rev (Walker_Object.get_treated w) 
    else iterate (step w) ;;   

  
  let rec iterate_for_guard_pattern_detection w =
    if (Walker_Object.get_lines w) = [] 
    then Walker_Object.get_guard_pattern_indicators w 
    else iterate_for_guard_pattern_detection (step w) ;;   
    
let make = Walker_Object.make ;;

end ;;  


let compute_small_spaces_in_text text =
  Walker.iterate(Walker.make text) ;; 

(*

let text1 = String.concat "\n" 
[
  "#if 1";
  "2";
  "#elif 3";
  "4";
  "#if 5";
  "6";
  "#endif 7";
  "#endif 8";
  "9";
  "#if 10\\";
  "11\\";
  "12\\";
  "13";
  "14";
  "#endif 15"
] ;;

let check1 = compute_small_spaces_in_text text1 ;;

let text2 = String.concat "\n" 
[
  "1";
  "2";
  "#if 3\\";
  "4\\";
  "5";
  "#elif 6\\";
  "7";
  "#if 8";
  "9";
  "#endif 10";
  "11";
  "12";
  "#endif 13";
  "14";
] ;;

let check2 = compute_small_spaces_in_text text2 ;;

*)   

let compute_small_spaces_in_file ap = 
 compute_small_spaces_in_text (Io.read_whole_file ap)  ;;  

let test_text_for_guard_pattern text =
   Guard_Pattern.test 
   (Walker.iterate_for_guard_pattern_detection (Walker.make text)) ;; 

(*

let text1 = String.concat "\n" 
[
  "1";
  "#if 2";
  "3";
  "#endif 4";
  "#if 5";
  "6";
  "#endif 7";
  "8"
] ;;

let check1 = Walker.iterate_for_guard_pattern_detection (Walker.make text1) ;;
let chuck1 = test_text_for_guard_pattern text1 ;;

let text2 = String.concat "\n" 
[
  "1";
  "#if 2";
  "3";
  "4";
  "#elif 5";
  "6";
  "#endif 7";
  "8"
] ;;

let check2 = Walker.iterate_for_guard_pattern_detection (Walker.make text2) ;;
let chuck2 = test_text_for_guard_pattern text2 ;;

let text3 = String.concat "\n" 
[
  "1";
  "#if 2";
  "3";
  "#if 4";
  "5";
  "#elif 6";
  "7";
  "#endif 8";
  "9";
  "#if 10";
  "11";
  "#endif 12";
  "#endif 13";
  "14"
] ;;

let check3 = Walker.iterate_for_guard_pattern_detection (Walker.make text3) ;;
let chuck3 = test_text_for_guard_pattern text3 ;;


*)

let find_directive_from_list_opt line directives=
      if not(String.starts_with line ~prefix:"#")
      then None
      else  
      match Strung.char_finder_from_inclusive_opt (fun c->
              not(List.mem c [' ';'\t';'\r'])
            ) line 2 with 
      None -> None
      |Some idx1 -> 
        List.find_opt (fun drctv ->
          Substring.is_a_substring_located_at drctv line idx1
          ) directives ;;

let text_has_ivy text =
   let lines = lines_outside_cee_comments text in 
   List.exists (
    fun line -> (find_directive_from_list_opt line ["if"])<>None
   ) lines ;;          

exception First_ivy_in_text_exn ;;   
let first_ivy_in_text text =
   let lines = indexed_lines_outside_cee_comments text in 
   match List.find_opt (fun (_line_idx,line)->
      (find_directive_from_list_opt line ["if"])<>None
   ) lines with 
   None -> raise First_ivy_in_text_exn 
   |Some(line_idx,_) -> line_idx ;;
   
exception Last_endif_in_text_exn ;;   
let last_endif_in_text text =
      let lines = List.rev(indexed_lines_outside_cee_comments text) in 
      match List.find_opt (fun (_line_idx,line)->
         (find_directive_from_list_opt line ["endif"])<>None
      ) lines with 
      None -> raise Last_endif_in_text_exn 
      |Some(line_idx,_) -> line_idx ;;   
 
let put_first_ivy_on_first_line text = 
  let line_idx = first_ivy_in_text text in 
  Lines_in_text.put_line_first_in_text line_idx text ;;

let put_last_endif_on_last_line text = 
  let line_idx = last_endif_in_text text in 
  let temp_text = Lines_in_text.put_line_last_in_text line_idx text in 
  let temp_lines = List.rev(lines_outside_cee_comments temp_text) in 
  let (temp_last_line,temp_tl) = List_again.head_with_tail temp_lines in 
  (* Any comments after the #endif must be moved before it *)
  let after= Cull_string.two_sided_cutting  ("#endif","") temp_last_line in 
  let almost_all_lines=(
     match Strung.char_finder_from_inclusive_opt (fun c->
              not(List.mem c [' ';'\t';'\r'])
            ) after 1 with 
      None -> temp_tl
      |Some idx -> (Cull_string.cobeginning (idx-1) after) :: temp_tl
  )in
  let lines = List.rev("#endif" :: almost_all_lines) in 
  String.concat "\n" lines;;  

let emphatize_first_ivy_and_last_endif text = 
  let text2 = put_first_ivy_on_first_line text in 
  put_last_endif_on_last_line text2 ;;

let standardize_guard_in_text_opt text = 
  if test_text_for_guard_pattern text 
  then let new_text = emphatize_first_ivy_and_last_endif text in 
       if new_text = text 
       then None 
       else Some(new_text)
  else None ;;   

 let marker_for_cd_defined_region = "cgmvgtkcxvvxckt" ;;  
 let parametrized_marker_for_cd_defined_region name_for_watermarkable_file k =
   let sk = string_of_int k in 
   marker_for_cd_defined_region^name_for_watermarkable_file^sk^marker_for_cd_defined_region ;;
 
 let parametrized_line_for_cd_defined_region name_for_watermarkable_file cd_idx =  
   "char* unused_string"^(string_of_int cd_idx)^"=\""^
   (parametrized_marker_for_cd_defined_region name_for_watermarkable_file cd_idx)^"\";" ;;
 
 let is_in_interval x (a,b) = (a<=x) && (x<=b) ;; 
 
 let is_in_interval_union x intervals =
    List.exists (is_in_interval x) intervals ;;
 
let marker_for_inclusion_highlighting = "cgmvgtkcxvvxckt" ;;  
let parametrized_marker_for_inclusion_highlighting inclusion_idx verb =
   let s_idx = string_of_int inclusion_idx in 
   "char* unused_string_for_inclusion_highlighting"^s_idx^"_"^verb^
   "=\""^marker_for_inclusion_highlighting^" Inclusion number "^s_idx^
   " "^verb^"s here \";";;
    ;;
 
 let markers_for_inclusion_highlighting inclusion_idx =  
   (
     parametrized_marker_for_inclusion_highlighting inclusion_idx "start",
     parametrized_marker_for_inclusion_highlighting inclusion_idx "end"
   );;


let compute_shadow old_text ~inclusion_index_opt ~name_for_included_file 
  ~preprocessed_includer_text =   
   let ssps = compute_small_spaces_in_text old_text  in 
   let indexed_ssps = Int_range.index_everything ssps in 
   let subtext = (
      match inclusion_index_opt with 
      None -> preprocessed_includer_text 
      |Some inclusion_idx -> 
        let markers = 
          markers_for_inclusion_highlighting inclusion_idx in 
        Cull_string.between_markers 
         markers preprocessed_includer_text
   ) in 
   let accepted_ssps = List.filter(
         fun (ssp_idx,ssp) ->
          if ssp.namespace = 0 then true else 
          Substring.is_a_substring_of 
          (parametrized_marker_for_cd_defined_region name_for_included_file ssp_idx) subtext 
   ) indexed_ssps in 
   Cee_shadow_t.Sh(List.length indexed_ssps,
   Cee_prawn_t.P(Image.image fst accepted_ssps)) ;;

 let crop_using_prawn old_text (Cee_prawn_t.P(accepted_indices)) =
   let lines = indexed_lines_outside_cee_comments old_text 
   and ssps = compute_small_spaces_in_text old_text  in 
   let accepted_ssps = Image.image(
      fun ssp_idx ->
       (ssp_idx,List.nth ssps (ssp_idx-1)) 
   ) accepted_indices in 
   let accepted_intervals = Image.image (
     fun (_,ssp) -> (ssp.start_idx,ssp.end_idx)
   ) accepted_ssps in 
   let accepted_lines = List.filter_map (
      fun (line_idx,line) ->
        if is_in_interval_union line_idx accepted_intervals 
        then Some line 
       else None
   ) lines in 
   String.concat "\n" accepted_lines ;;
 

   let pairs_of_indices_for_watermarking indexed_ssps = 
    List.filter_map (
      fun (ssp_idx,ssp) ->
        if ssp.namespace = 0 then None else 
        Some(ssp.start_idx,ssp_idx) 
    ) indexed_ssps ;;
  
  let tattoo_regions_between_conditional_directives ~name_for_included_file text = 
     let lines = indexed_lines_outside_cee_comments text 
     and ssps = compute_small_spaces_in_text text in 
     let indexed_ssps = Int_range.index_everything ssps in
     let pairs = pairs_of_indices_for_watermarking indexed_ssps in 
     let temp1 = Image.image (
        fun (line_idx,line) ->
          match List.assoc_opt line_idx pairs with 
          None -> [line]
          | (Some ssp_idx) ->
             [parametrized_line_for_cd_defined_region name_for_included_file ssp_idx;line]
     ) lines in  
     (String.concat "\n" (List.flatten temp1)) ;;

(*

let text1 = String.concat "\n" 
[
   "#if 1";
   "2";
   "#elif 3";
   "4";
   "#if 5";
   "6";
   "#endif 7";
   "#endif 8";
   "9";
   "#if 10\\";
   "11\\";
   "12\\";
   "13";
   "14";
   "#endif 15"
] ;;

let text2 = watermark_text text1 ;;

print_string(text2);;

print_string(rewrite_using_watermarks text1 text2);;

let text3 = parametrized_line_for_cd_defined_region 3;;

print_string(rewrite_using_watermarks text1 text3);;

*)

let generic_included_file_opt (opening_char,closing_char) line =
  if not(String.starts_with line ~prefix:"#")
  then None  
  else  
  match Strung.char_finder_from_inclusive_opt (fun c->
          not(List.mem c [' ';'\t';'\r'])
        ) line 2 with 
  None -> None
  |Some idx1 -> 
  if not(Substring.is_a_substring_located_at "include" line idx1)
  then None
  else   
  match Strung.char_finder_from_inclusive_opt (fun c->
        not(List.mem c [' ';'\t';'\r'])
  ) line (idx1+7) with 
  None -> None
  |Some idx2 -> 
  if (Strung.get line idx2)<>opening_char
  then None
  else    
  match Strung.char_finder_from_inclusive_opt (fun c->
           c = closing_char
  ) line (idx2+1) with 
  None -> None
  |Some idx3 ->      
    Some (Cull_string.interval line (idx2+1) (idx3-1))
    ;;

let included_local_file_opt = generic_included_file_opt ('"','"');;
let included_nonlocal_file_opt = generic_included_file_opt ('<','>');;


let included_local_files_in_text text = 
  let temp1 = indexed_lines_outside_cee_comments text in 
  let temp2 = List.filter_map (
    fun (line_idx,line) ->
      Option.map (fun included_fn ->
          (line_idx,included_fn)
      ) (included_local_file_opt line)
  ) temp1 in 
  intstr_sort temp2 ;;

let included_nonlocal_files_in_text text = 
  let temp1 = indexed_lines_outside_cee_comments text in 
  let temp2 = List.filter_map (
      fun (line_idx,line) ->
        Option.map (fun included_fn ->
            (line_idx,included_fn)
        ) (included_nonlocal_file_opt line)
  ) temp1 in 
  intstr_sort temp2 ;;


let add_extra_ending_in_inclusion_line ~extra line = 
  let occs = Substring.occurrences_of_in "\"" line in 
  if List.length(occs)<>2
  then raise(Add_extra_inclusion_line_exn(line))
  else 
  let i1 = List.nth occs 0 
  and i2 = List.nth occs 1 in 
  let included_file = Cull_string.interval line (i1+1) (i2-1) in 
  (Cull_string.beginning i1 line)^
  (Cee_common.add_extra_ending_in_filename ~extra included_file)^
  (Cull_string.interval line i2 (String.length line)) ;;

(*  
  add_extra_ending_in_inclusion_line 
  ~extra:"fat" "# include \"The_brown.cat\"";;
*)

let add_extra_ending_in_inclusions_inside_text ~extra text =
  let lines_before = lines_outside_cee_comments text 
  and counter=ref 0 in 
  let lines_after = Image.image(
    fun line -> 
      if (included_local_file_opt line)<>None
      then let _ =(counter:=(!counter)+1) in 
      add_extra_ending_in_inclusion_line ~extra line 
      else line    
  ) lines_before in 
  (String.concat "\n" lines_after,!counter);;
   
let compute_wardrobe 
  ~preprocessed_includer_text 
  copied_includable_files = 
  Cee_wardrobe_t.Wr(Image.image (
    fun (old_name,old_content,new_name,inclusion_index) ->
      ((inclusion_index,old_name),compute_shadow 
      old_content 
       ~inclusion_index_opt:(Some inclusion_index)
       ~name_for_included_file:new_name 
        ~preprocessed_includer_text
      ) 
  ) copied_includable_files);;

let highlight_inclusions_in_text text = 
  let temp1 = indexed_lines_outside_cee_comments text in 
  let all_lines = Image.image (
    fun (line_idx,line) ->
      ((line_idx,line),(included_local_file_opt line)<>None)
  ) temp1 in 
  let inclusion_lines = List.filter snd all_lines in 
  let indexed_inclusion_lines = Int_range.index_everything inclusion_lines in 
  let line_idx_to_incl_idx = Image.image (
    fun (incl_idx,((line_idx,_),_)) -> (line_idx,incl_idx)
  ) indexed_inclusion_lines in 
  let modified_lines = Image.image (
    fun ((line_idx,line),line_is_an_inclusion) ->
      if not(line_is_an_inclusion) then line else 
      let incl_idx = List.assoc line_idx line_idx_to_incl_idx in 
      let (marker_before,marker_after) = 
       markers_for_inclusion_highlighting incl_idx in 
      "\n"^marker_before ^"\n"^line^"\n"^marker_after 
  ) all_lines in 
  String.concat "\n" modified_lines ;;

(*


let text1 = String.concat "\n" 
[
   "1";
   "2";
   "#include \"abc\"";
   "4";
   "#include \"def\"";
   "#include \"ghi\"";
   "7";
] ;;

let text2 = highlight_inclusions_in_text text1 ;;

print_string text2 ;;


*)

let highlight_and_add_extra_ending_in_inclusions_inside_text 
   ~extra text =
    add_extra_ending_in_inclusions_inside_text ~extra 
     (highlight_inclusions_in_text text)
    ;;


end ;;  

let compute_shadow = Private.compute_shadow ;;
let compute_wardrobe = Private.compute_wardrobe ;;
let crop_using_prawn = Private.crop_using_prawn ;;
let highlight_and_add_extra_ending_in_inclusions_inside_text = Private.highlight_and_add_extra_ending_in_inclusions_inside_text ;;
let included_local_files_in_text = Private.included_local_files_in_text ;;
let included_nonlocal_files_in_text = Private.included_nonlocal_files_in_text ;;
let standardize_guard_in_text_opt = Private.standardize_guard_in_text_opt ;;
let tattoo_regions_between_conditional_directives= Private.tattoo_regions_between_conditional_directives;;


let of_walker_object w=
  (
    List.hd(w.Private.Walker.Walker_Object.lines),
    w.Private.Walker.Walker_Object.current_namespace,
    w.Private.Walker.Walker_Object.preceding_namespaces,
    w.Private.Walker.Walker_Object.smallest_unused_namespace_index,
    w.Private.Walker.Walker_Object.small_space_start_index_opt,
    w.Private.Walker.Walker_Object.unfinished_condition,
    w.Private.Walker.Walker_Object.last_directive_was_an_else,
    Image.image(fun ssp ->
       (ssp.Private.namespace,
        ssp.Private.start_idx,
        ssp.Private.end_idx)  
    )
    (w.Private.Walker.Walker_Object.treated),
    w.Private.Walker.Walker_Object.gpi.Private.first_ivy_index_opt,
    w.Private.Walker.Walker_Object.gpi.Private.elsie_or_elif_for_first_index_opt,
    w.Private.Walker.Walker_Object.gpi.Private.endif_index_for_first_ivy_opt,
    w.Private.Walker.Walker_Object.gpi.Private.ivy_index_after_first_ivy_opt
  ) ;;

end ;;

(* YZYZYZ *)


module New_cee_text = struct 

(*

#use"lib/Cee_language/cee_text.ml";;

*)

exception Lonely_Elif_exn of int ;;
exception Lonely_Else_exn of int ;;
exception Lonely_Endif_exn of int ;;
exception Double_Else_exn of int ;;
exception Standardize_inclusion_line_exn of string ;;  
exception Add_extra_inclusion_line_exn of string ;;
  
module Private = struct 

  let str_order = Total_ordering.lex_for_strings ;;
  let str_mem = Ordered.mem str_order ;;  

  let i_order = Total_ordering.for_integers ;;
   let i_merge = Ordered.merge i_order ;; 

  let intstr_order = Total_ordering.product i_order str_order ;;

  let intstr_sort = Ordered.sort intstr_order;;

  let lines_outside_cee_comments = 
    
    Lines_in_text.lines (* _outside_cee_comments *) ;;

  let indexed_lines_outside_cee_comments text = 
     Int_range.index_everything (lines_outside_cee_comments text) ;; 


   let sil_merge ox oy=
   let rec tempf=(function (u,v,accu)->
   if u=[] then (List.rev_append(accu)(v)) else
   if v=[] then (List.rev_append(accu)(u)) else
   let (xu,lxu)=List.hd(u) and yu=List.tl(u) 
   and (xv,lxv)=List.hd(v) and yv=List.tl(v) in
   match str_order(xu)(xv) with
     Total_ordering_result_t.Lower->tempf(yu,v,(xu,lxu)::accu)
   |Total_ordering_result_t.Equal->tempf(yu,yv,(xu,i_merge lxu lxv)::accu)
   |Total_ordering_result_t.Greater->tempf(u,yv,(xv,lxv)::accu)
   ) in
   tempf(ox,oy,[]);;
 
 let rec sil_sort  x=
   if List.length(x)<2
   then x
   else 
   let temp1=Partition_list.split_in_half(x) in
   let y1=sil_sort(fst temp1)
   and y2=sil_sort(snd temp1) in
   sil_merge y1 y2;;      
 

type line_beginning = 
  Lb_If
 |Lb_Elif
 |Lb_Else 
 |Lb_Endif 
 |Lb_Usual ;; 

   
 module Line_beginning = struct 

  let data = [
    "if", Lb_If;
    "elif", Lb_Elif;  
    "else", Lb_Else;
    "endif", Lb_Endif
]  ;;  

let compute line =
  if not(String.starts_with line ~prefix:"#")
  then Lb_Usual  
  else  
  match Strung.char_finder_from_inclusive_opt (fun c->
          not(List.mem c [' ';'\t';'\r'])
        ) line 2 with 
  None -> Lb_Usual
  |Some idx1 -> 
    (match List.find_opt (fun (lb_name,_lb)->
      Substring.is_a_substring_located_at lb_name line idx1
      ) data with 
    None -> Lb_Usual
    |Some (_,lb) -> lb 
    );;

end ;;  


 


type small_space_t = {
   namespace : int ;
   start_idx : int ;
   end_idx  : int ;
} ;;

type guard_pattern_indicators_t = {
    first_ivy_index_opt : int option ;
    elsie_or_elif_for_first_index_opt : int option;
    endif_index_for_first_ivy_opt : int option ;
    ivy_index_after_first_ivy_opt : int option;
} ;; 




module Guard_Pattern = struct
let origin = {
  first_ivy_index_opt = None;
  elsie_or_elif_for_first_index_opt = None;
  endif_index_for_first_ivy_opt = None ;
  ivy_index_after_first_ivy_opt = None;
} ;; 

let test gpi=
  (gpi.endif_index_for_first_ivy_opt <> None) &&
  (gpi.elsie_or_elif_for_first_index_opt = None) &&
  (gpi.ivy_index_after_first_ivy_opt = None)  ;;   
  
end ;;

module Walker = struct 

   module Walker_Object = struct 

    type t = {
      lines : (int *string) list ;
      current_namespace : int ;
      preceding_namespaces : int list ;
      smallest_unused_namespace_index : int ;
      small_space_start_index_opt : int option ;
      unfinished_condition : bool;
      last_directive_was_an_else : bool;
      treated : small_space_t list;
      gpi : guard_pattern_indicators_t ;
    } ;;  
  let make text = {
    lines = indexed_lines_outside_cee_comments text ;
    current_namespace = 0 ;
    preceding_namespaces = [] ;
    smallest_unused_namespace_index = 1;
    small_space_start_index_opt = None ;
    unfinished_condition = false;
    last_directive_was_an_else = false;
    treated = [];
    gpi = Guard_Pattern.origin ;
  } ;; 

  let add_to_treated small_space w = {
       w with 
       treated = small_space :: (w.treated)   
    } ;; 

  let condition_is_unfinished w = w.unfinished_condition ;; 
  let get_current_namespace w = w.current_namespace ;; 
  let get_endif_index_for_first_ivy_opt w = w.gpi.endif_index_for_first_ivy_opt ;;
  let get_elsie_or_elif_for_first_index_opt w = w.gpi.elsie_or_elif_for_first_index_opt ;;
  let get_first_ivy_index_opt w = w.gpi.first_ivy_index_opt ;;
  let get_guard_pattern_indicators w = w.gpi ;;
  let get_ivy_index_after_first_ivy_opt w = w.gpi.ivy_index_after_first_ivy_opt ;;
  let get_lines w = w.lines ;;
  let get_preceding_namespaces w = w.preceding_namespaces ;;
  let get_small_space_start_index_opt w = w.small_space_start_index_opt ;;
  let get_smallest_unused_namespace_index w = w.smallest_unused_namespace_index ;;
  let get_treated w = w.treated ;;

  let last_directive_was_an_else w = w.last_directive_was_an_else ;;

  let register_elsie_or_elif_for_first_index w line_idx =
      let new_gpi = {w.gpi with elsie_or_elif_for_first_index_opt = Some line_idx} in 
      {w with gpi = new_gpi} ;;

  let register_first_ivy_index w line_idx =
    let new_gpi = {w.gpi with first_ivy_index_opt = Some line_idx} in 
    {w with gpi = new_gpi} ;;
  
  let register_ivy_index_after_first_ivy w line_idx =  
    let new_gpi = {w.gpi with ivy_index_after_first_ivy_opt = Some line_idx} in 
    {w with gpi = new_gpi} ;;

  let register_small_space_start_index w line_idx =    
      {
        w with 
        small_space_start_index_opt = Some line_idx 
      } ;; 

  let set_condition_to_finished w =    
      {
        w with 
        unfinished_condition = false
      }   ;;
 
  let set_lines w new_set_of_lines=    
      {
        w with 
        lines = new_set_of_lines
      }   ;;    

  let set_namespace_data w 
     ~v_current_namespace
     ~v_smallest_unused_namespace_index
     ~v_preceding_namespaces
      =  {
        w with 
        current_namespace = v_current_namespace ;
        smallest_unused_namespace_index = v_smallest_unused_namespace_index;
        preceding_namespaces = v_preceding_namespaces
      } ;;

  let set_unfinished_condition_data w 
      ~current_cond_is_unfinished
      ~current_line_is_an_elsie
    ={
      w with
        small_space_start_index_opt = None ;
        unfinished_condition = current_cond_is_unfinished;
        last_directive_was_an_else = current_line_is_an_elsie ;
     } ;;



   end ;;

  let register_new_small_space_if_needed old_w w line_idx= 
    match Walker_Object.get_small_space_start_index_opt old_w with 
    None -> w
    |Some start_index ->
      let small_space = {
       namespace = Walker_Object.get_current_namespace w ;
       start_idx = start_index ;
       end_idx  = line_idx -1 ;
      } in 
      Walker_Object.add_to_treated small_space w ;; 

  let deal_with_namespace_data old_w w line_beginning= 
     match line_beginning with 
     Lb_If ->
      let old_idx = Walker_Object.get_smallest_unused_namespace_index old_w in 
      let v_current_namespace=old_idx
      and v_smallest_unused_namespace_index=old_idx + 1
      and v_preceding_namespaces=(Walker_Object.get_current_namespace old_w) :: (Walker_Object.get_preceding_namespaces old_w) in 
      Walker_Object.set_namespace_data w 
        ~v_current_namespace
        ~v_smallest_unused_namespace_index
        ~v_preceding_namespaces
     
    | Lb_Endif ->
      let preceding = Walker_Object.get_preceding_namespaces old_w in 
      let v_current_namespace=List.hd preceding
      and v_smallest_unused_namespace_index=Walker_Object.get_smallest_unused_namespace_index old_w
      and v_preceding_namespaces= List.tl preceding in 
      Walker_Object.set_namespace_data w 
        ~v_current_namespace
        ~v_smallest_unused_namespace_index
        ~v_preceding_namespaces
   | Lb_Elif | Lb_Else | Lb_Usual -> w ;;

   let deal_with_guard_pattern_detection old_w w line_beginning line_idx = 
    match line_beginning with 
    Lb_If ->
      (if ((Walker_Object.get_first_ivy_index_opt old_w) = None)
       then Walker_Object.register_first_ivy_index w line_idx
       else 
       if ((Walker_Object.get_endif_index_for_first_ivy_opt old_w) <> None) &&
          ((Walker_Object.get_ivy_index_after_first_ivy_opt old_w) = None)
       then Walker_Object.register_ivy_index_after_first_ivy w line_idx  
       else  w
       )
   | Lb_Elif | Lb_Else ->
    (if ((Walker_Object.get_first_ivy_index_opt old_w) <> None) &&
         ((Walker_Object.get_current_namespace old_w)=1) &&
         ((Walker_Object.get_elsie_or_elif_for_first_index_opt old_w) = None)
      then Walker_Object.register_elsie_or_elif_for_first_index w line_idx 
      else w  
      )
     
   | Lb_Endif ->
    (if ((Walker_Object.get_first_ivy_index_opt old_w) <> None) &&
        ((Walker_Object.get_current_namespace old_w)=1) 
     then Walker_Object.register_elsie_or_elif_for_first_index w line_idx
     else w  
   )
   | Lb_Usual -> w ;;


  let directive_step old_w line_idx line line_beginning= 
    if ((Walker_Object.get_current_namespace old_w) = 0) && (line_beginning = Lb_Elif) then raise (Lonely_Elif_exn(line_idx)) else 
    if ((Walker_Object.get_current_namespace old_w) = 0) && (line_beginning = Lb_Else) then raise (Lonely_Else_exn(line_idx)) else
    if ((Walker_Object.get_current_namespace old_w) = 0) && (line_beginning = Lb_Endif) then raise (Lonely_Endif_exn(line_idx)) else
    if (Walker_Object.last_directive_was_an_else old_w) && (line_beginning = Lb_Else) then raise (Double_Else_exn(line_idx)) else
    let current_cond_is_unfinished = (
      if List.mem line_beginning [Lb_If;Lb_Elif]
      then String.ends_with line ~suffix:"\\"
      else false 
    ) in 
    let w1 =
      Walker_Object.set_unfinished_condition_data old_w 
       ~current_cond_is_unfinished
       ~current_line_is_an_elsie:(line_beginning = Lb_Else) in 
   let w2 = register_new_small_space_if_needed old_w w1 line_idx in 
   let w3 = deal_with_namespace_data old_w w2 line_beginning in 
   let w4 = deal_with_guard_pattern_detection old_w w3 line_beginning line_idx in
   w4 ;;
  let inner_usual_step w line_idx line = 
    match Walker_Object.get_small_space_start_index_opt w  with 
    (Some _) -> w 
    |None -> 
    if not(Walker_Object.condition_is_unfinished w)
    then Walker_Object.register_small_space_start_index w line_idx 
    else
    if String.ends_with line ~suffix:"\\"
    then w
    else Walker_Object.set_condition_to_finished w ;; 

  let usual_step w line_idx line =
    let w2 = inner_usual_step w line_idx line in 
    if (Walker_Object.get_lines w2) = []
    then register_new_small_space_if_needed w2 w2 (line_idx+1)
    else w2 ;;    
    

  let step_naively old_w = 
    let (line_idx,line) = List.hd (Walker_Object.get_lines old_w) 
    and w = Walker_Object.set_lines old_w  
          (List.tl(Walker_Object.get_lines old_w)) in 
    let line_beginning = Line_beginning.compute line in 
    match line_beginning with 
    Lb_Usual -> usual_step w line_idx line 
   |Lb_If |Lb_Elif |Lb_Else |Lb_Endif -> directive_step w line_idx line line_beginning ;;
  
  let ref_for_step_debugging = ref None ;; 

  let step w = 
     try step_naively w with 
     xn ->
       (
         ref_for_step_debugging:=Some(w);
         raise xn
       ) ;; 

  let rec iterate w =
    if (Walker_Object.get_lines w) = [] 
    then List.rev (Walker_Object.get_treated w) 
    else iterate (step w) ;;   

  
  
  
  
  let rec iterate_for_guard_pattern_detection w =
    if (Walker_Object.get_lines w) = [] 
    then Walker_Object.get_guard_pattern_indicators w 
    else iterate_for_guard_pattern_detection (step w) ;;   
    
let make = Walker_Object.make ;;

end ;;  


let compute_small_spaces_in_text text =
  Walker.iterate(Walker.make text) ;; 

(*

let text1 = String.concat "\n" 
[
  "#if 1";
  "2";
  "#elif 3";
  "4";
  "#if 5";
  "6";
  "#endif 7";
  "#endif 8";
  "9";
  "#if 10\\";
  "11\\";
  "12\\";
  "13";
  "14";
  "#endif 15"
] ;;

let check1 = compute_small_spaces_in_text text1 ;;

let text2 = String.concat "\n" 
[
  "1";
  "2";
  "#if 3\\";
  "4\\";
  "5";
  "#elif 6\\";
  "7";
  "#if 8";
  "9";
  "#endif 10";
  "11";
  "12";
  "#endif 13";
  "14";
] ;;

let check2 = compute_small_spaces_in_text text2 ;;

*)   

let compute_small_spaces_in_file ap = 
 compute_small_spaces_in_text (Io.read_whole_file ap)  ;;  

let test_text_for_guard_pattern text =
   Guard_Pattern.test 
   (Walker.iterate_for_guard_pattern_detection (Walker.make text)) ;; 

(*

let text1 = String.concat "\n" 
[
  "1";
  "#if 2";
  "3";
  "#endif 4";
  "#if 5";
  "6";
  "#endif 7";
  "8"
] ;;

let check1 = Walker.iterate_for_guard_pattern_detection (Walker.make text1) ;;
let chuck1 = test_text_for_guard_pattern text1 ;;

let text2 = String.concat "\n" 
[
  "1";
  "#if 2";
  "3";
  "4";
  "#elif 5";
  "6";
  "#endif 7";
  "8"
] ;;

let check2 = Walker.iterate_for_guard_pattern_detection (Walker.make text2) ;;
let chuck2 = test_text_for_guard_pattern text2 ;;

let text3 = String.concat "\n" 
[
  "1";
  "#if 2";
  "3";
  "#if 4";
  "5";
  "#elif 6";
  "7";
  "#endif 8";
  "9";
  "#if 10";
  "11";
  "#endif 12";
  "#endif 13";
  "14"
] ;;

let check3 = Walker.iterate_for_guard_pattern_detection (Walker.make text3) ;;
let chuck3 = test_text_for_guard_pattern text3 ;;


*)

let find_directive_from_list_opt line directives=
      if not(String.starts_with line ~prefix:"#")
      then None
      else  
      match Strung.char_finder_from_inclusive_opt (fun c->
              not(List.mem c [' ';'\t';'\r'])
            ) line 2 with 
      None -> None
      |Some idx1 -> 
        List.find_opt (fun drctv ->
          Substring.is_a_substring_located_at drctv line idx1
          ) directives ;;

let text_has_ivy text =
   let lines = lines_outside_cee_comments text in 
   List.exists (
    fun line -> (find_directive_from_list_opt line ["if"])<>None
   ) lines ;;          

exception First_ivy_in_text_exn ;;   
let first_ivy_in_text text =
   let lines = indexed_lines_outside_cee_comments text in 
   match List.find_opt (fun (_line_idx,line)->
      (find_directive_from_list_opt line ["if"])<>None
   ) lines with 
   None -> raise First_ivy_in_text_exn 
   |Some(line_idx,_) -> line_idx ;;
   
exception Last_endif_in_text_exn ;;   
let last_endif_in_text text =
      let lines = List.rev(indexed_lines_outside_cee_comments text) in 
      match List.find_opt (fun (_line_idx,line)->
         (find_directive_from_list_opt line ["endif"])<>None
      ) lines with 
      None -> raise Last_endif_in_text_exn 
      |Some(line_idx,_) -> line_idx ;;   
 
let put_first_ivy_on_first_line text = 
  let line_idx = first_ivy_in_text text in 
  Lines_in_text.put_line_first_in_text line_idx text ;;

let put_last_endif_on_last_line text = 
  let line_idx = last_endif_in_text text in 
  let temp_text = Lines_in_text.put_line_last_in_text line_idx text in 
  let temp_lines = List.rev(lines_outside_cee_comments temp_text) in 
  let (temp_last_line,temp_tl) = List_again.head_with_tail temp_lines in 
  (* Any comments after the #endif must be moved before it *)
  let after= Cull_string.two_sided_cutting  ("#endif","") temp_last_line in 
  let almost_all_lines=(
     match Strung.char_finder_from_inclusive_opt (fun c->
              not(List.mem c [' ';'\t';'\r'])
            ) after 1 with 
      None -> temp_tl
      |Some idx -> (Cull_string.cobeginning (idx-1) after) :: temp_tl
  )in
  let lines = List.rev("#endif" :: almost_all_lines) in 
  String.concat "\n" lines;;  

let emphatize_first_ivy_and_last_endif text = 
  let text2 = put_first_ivy_on_first_line text in 
  put_last_endif_on_last_line text2 ;;

let standardize_guard_in_text_opt text = 
  if test_text_for_guard_pattern text 
  then let new_text = emphatize_first_ivy_and_last_endif text in 
       if new_text = text 
       then None 
       else Some(new_text)
  else None ;;   

 let marker_for_cd_defined_region = "cgmvgtkcxvvxckt" ;;  
 let parametrized_marker_for_cd_defined_region name_for_watermarkable_file k =
   let sk = string_of_int k in 
   marker_for_cd_defined_region^name_for_watermarkable_file^sk^marker_for_cd_defined_region ;;
 
 let parametrized_line_for_cd_defined_region name_for_watermarkable_file cd_idx =  
   "char* unused_string"^(string_of_int cd_idx)^"=\""^
   (parametrized_marker_for_cd_defined_region name_for_watermarkable_file cd_idx)^"\";" ;;
 
 let is_in_interval x (a,b) = (a<=x) && (x<=b) ;; 
 
 let is_in_interval_union x intervals =
    List.exists (is_in_interval x) intervals ;;
 
let marker_for_inclusion_highlighting = "cgmvgtkcxvvxckt" ;;  
let parametrized_marker_for_inclusion_highlighting inclusion_idx verb =
   let s_idx = string_of_int inclusion_idx in 
   "char* unused_string_for_inclusion_highlighting"^s_idx^"_"^verb^
   "=\""^marker_for_inclusion_highlighting^" Inclusion number "^s_idx^
   " "^verb^"s here \";";;
    ;;
 
 let markers_for_inclusion_highlighting inclusion_idx =  
   (
     parametrized_marker_for_inclusion_highlighting inclusion_idx "start",
     parametrized_marker_for_inclusion_highlighting inclusion_idx "end"
   );;


let compute_shadow old_text ~inclusion_index_opt ~name_for_included_file 
  ~preprocessed_includer_text =   
   let ssps = compute_small_spaces_in_text old_text  in 
   let indexed_ssps = Int_range.index_everything ssps in 
   let subtext = (
      match inclusion_index_opt with 
      None -> preprocessed_includer_text 
      |Some inclusion_idx -> 
        let markers = 
          markers_for_inclusion_highlighting inclusion_idx in 
        Cull_string.between_markers 
         markers preprocessed_includer_text
   ) in 
   let accepted_ssps = List.filter(
         fun (ssp_idx,ssp) ->
          if ssp.namespace = 0 then true else 
          Substring.is_a_substring_of 
          (parametrized_marker_for_cd_defined_region name_for_included_file ssp_idx) subtext 
   ) indexed_ssps in 
   Cee_shadow_t.Sh(List.length indexed_ssps,
   Cee_prawn_t.P(Image.image fst accepted_ssps)) ;;

 let crop_using_prawn old_text (Cee_prawn_t.P(accepted_indices)) =
   let lines = indexed_lines_outside_cee_comments old_text 
   and ssps = compute_small_spaces_in_text old_text  in 
   let accepted_ssps = Image.image(
      fun ssp_idx ->
       (ssp_idx,List.nth ssps (ssp_idx-1)) 
   ) accepted_indices in 
   let accepted_intervals = Image.image (
     fun (_,ssp) -> (ssp.start_idx,ssp.end_idx)
   ) accepted_ssps in 
   let accepted_lines = List.filter_map (
      fun (line_idx,line) ->
        if is_in_interval_union line_idx accepted_intervals 
        then Some line 
       else None
   ) lines in 
   String.concat "\n" accepted_lines ;;
 

   let pairs_of_indices_for_watermarking indexed_ssps = 
    List.filter_map (
      fun (ssp_idx,ssp) ->
        if ssp.namespace = 0 then None else 
        Some(ssp.start_idx,ssp_idx) 
    ) indexed_ssps ;;
  
  let tattoo_regions_between_conditional_directives ~name_for_included_file text = 
     let lines = indexed_lines_outside_cee_comments text 
     and ssps = compute_small_spaces_in_text text in 
     let indexed_ssps = Int_range.index_everything ssps in
     let pairs = pairs_of_indices_for_watermarking indexed_ssps in 
     let temp1 = Image.image (
        fun (line_idx,line) ->
          match List.assoc_opt line_idx pairs with 
          None -> [line]
          | (Some ssp_idx) ->
             [parametrized_line_for_cd_defined_region name_for_included_file ssp_idx;line]
     ) lines in  
     (String.concat "\n" (List.flatten temp1)) ;;

(*

let text1 = String.concat "\n" 
[
   "#if 1";
   "2";
   "#elif 3";
   "4";
   "#if 5";
   "6";
   "#endif 7";
   "#endif 8";
   "9";
   "#if 10\\";
   "11\\";
   "12\\";
   "13";
   "14";
   "#endif 15"
] ;;

let text2 = watermark_text text1 ;;

print_string(text2);;

print_string(rewrite_using_watermarks text1 text2);;

let text3 = parametrized_line_for_cd_defined_region 3;;

print_string(rewrite_using_watermarks text1 text3);;

*)

let generic_included_file_opt (opening_char,closing_char) line =
  if not(String.starts_with line ~prefix:"#")
  then None  
  else  
  match Strung.char_finder_from_inclusive_opt (fun c->
          not(List.mem c [' ';'\t';'\r'])
        ) line 2 with 
  None -> None
  |Some idx1 -> 
  if not(Substring.is_a_substring_located_at "include" line idx1)
  then None
  else   
  match Strung.char_finder_from_inclusive_opt (fun c->
        not(List.mem c [' ';'\t';'\r'])
  ) line (idx1+7) with 
  None -> None
  |Some idx2 -> 
  if (Strung.get line idx2)<>opening_char
  then None
  else    
  match Strung.char_finder_from_inclusive_opt (fun c->
           c = closing_char
  ) line (idx2+1) with 
  None -> None
  |Some idx3 ->      
    Some (Cull_string.interval line (idx2+1) (idx3-1))
    ;;

let included_local_file_opt = generic_included_file_opt ('"','"');;
let included_nonlocal_file_opt = generic_included_file_opt ('<','>');;


let included_local_files_in_text text = 
  let temp1 = indexed_lines_outside_cee_comments text in 
  let temp2 = List.filter_map (
    fun (line_idx,line) ->
      Option.map (fun included_fn ->
          (line_idx,included_fn)
      ) (included_local_file_opt line)
  ) temp1 in 
  intstr_sort temp2 ;;

let included_nonlocal_files_in_text text = 
  let temp1 = indexed_lines_outside_cee_comments text in 
  let temp2 = List.filter_map (
      fun (line_idx,line) ->
        Option.map (fun included_fn ->
            (line_idx,included_fn)
        ) (included_nonlocal_file_opt line)
  ) temp1 in 
  intstr_sort temp2 ;;


let add_extra_ending_in_inclusion_line ~extra line = 
  let occs = Substring.occurrences_of_in "\"" line in 
  if List.length(occs)<>2
  then raise(Add_extra_inclusion_line_exn(line))
  else 
  let i1 = List.nth occs 0 
  and i2 = List.nth occs 1 in 
  let included_file = Cull_string.interval line (i1+1) (i2-1) in 
  (Cull_string.beginning i1 line)^
  (Cee_common.add_extra_ending_in_filename ~extra included_file)^
  (Cull_string.interval line i2 (String.length line)) ;;

(*  
  add_extra_ending_in_inclusion_line 
  ~extra:"fat" "# include \"The_brown.cat\"";;
*)

let add_extra_ending_in_inclusions_inside_text ~extra text =
  let lines_before = lines_outside_cee_comments text 
  and counter=ref 0 in 
  let lines_after = Image.image(
    fun line -> 
      if (included_local_file_opt line)<>None
      then let _ =(counter:=(!counter)+1) in 
      add_extra_ending_in_inclusion_line ~extra line 
      else line    
  ) lines_before in 
  (String.concat "\n" lines_after,!counter);;
   
let compute_wardrobe 
  ~preprocessed_includer_text 
  copied_includable_files = 
  Cee_wardrobe_t.Wr(Image.image (
    fun (old_name,old_content,new_name,inclusion_index) ->
      ((inclusion_index,old_name),compute_shadow 
      old_content 
       ~inclusion_index_opt:(Some inclusion_index)
       ~name_for_included_file:new_name 
        ~preprocessed_includer_text
      ) 
  ) copied_includable_files);;

let highlight_inclusions_in_text text = 
  let temp1 = indexed_lines_outside_cee_comments text in 
  let all_lines = Image.image (
    fun (line_idx,line) ->
      ((line_idx,line),(included_local_file_opt line)<>None)
  ) temp1 in 
  let inclusion_lines = List.filter snd all_lines in 
  let indexed_inclusion_lines = Int_range.index_everything inclusion_lines in 
  let line_idx_to_incl_idx = Image.image (
    fun (incl_idx,((line_idx,_),_)) -> (line_idx,incl_idx)
  ) indexed_inclusion_lines in 
  let modified_lines = Image.image (
    fun ((line_idx,line),line_is_an_inclusion) ->
      if not(line_is_an_inclusion) then line else 
      let incl_idx = List.assoc line_idx line_idx_to_incl_idx in 
      let (marker_before,marker_after) = 
       markers_for_inclusion_highlighting incl_idx in 
      "\n"^marker_before ^"\n"^line^"\n"^marker_after 
  ) all_lines in 
  String.concat "\n" modified_lines ;;

(*


let text1 = String.concat "\n" 
[
   "1";
   "2";
   "#include \"abc\"";
   "4";
   "#include \"def\"";
   "#include \"ghi\"";
   "7";
] ;;

let text2 = highlight_inclusions_in_text text1 ;;

print_string text2 ;;


*)

let highlight_and_add_extra_ending_in_inclusions_inside_text 
   ~extra text =
    add_extra_ending_in_inclusions_inside_text ~extra 
     (highlight_inclusions_in_text text)
    ;;


end ;;  

let compute_shadow = Private.compute_shadow ;;
let compute_wardrobe = Private.compute_wardrobe ;;
let crop_using_prawn = Private.crop_using_prawn ;;
let highlight_and_add_extra_ending_in_inclusions_inside_text = Private.highlight_and_add_extra_ending_in_inclusions_inside_text ;;
let included_local_files_in_text = Private.included_local_files_in_text ;;
let included_nonlocal_files_in_text = Private.included_nonlocal_files_in_text ;;
let standardize_guard_in_text_opt = Private.standardize_guard_in_text_opt ;;
let tattoo_regions_between_conditional_directives= Private.tattoo_regions_between_conditional_directives;;

let last_warrior txt =
    try (function _->[])(Private.compute_small_spaces_in_text text1) with 
    _ ->
      let lw = Option.get(!(Private.Walker.ref_for_step_debugging)) in 
      List.rev_map (
       fun ssp -> (ssp.Private.namespace,ssp.Private.start_idx,ssp.Private.end_idx)
      ) lw.Private.Walker.Walker_Object.treated ;;

let of_walker_object w=
  (
    List.hd(w.Private.Walker.Walker_Object.lines),
    w.Private.Walker.Walker_Object.current_namespace,
    w.Private.Walker.Walker_Object.preceding_namespaces,
    w.Private.Walker.Walker_Object.smallest_unused_namespace_index,
    w.Private.Walker.Walker_Object.small_space_start_index_opt,
    w.Private.Walker.Walker_Object.unfinished_condition,
    w.Private.Walker.Walker_Object.last_directive_was_an_else,
    Image.image(fun ssp ->
       (ssp.Private.namespace,
        ssp.Private.start_idx,
        ssp.Private.end_idx)  
    )
    (w.Private.Walker.Walker_Object.treated),
    w.Private.Walker.Walker_Object.gpi.Private.first_ivy_index_opt,
    w.Private.Walker.Walker_Object.gpi.Private.elsie_or_elif_for_first_index_opt,
    w.Private.Walker.Walker_Object.gpi.Private.endif_index_for_first_ivy_opt,
    w.Private.Walker.Walker_Object.gpi.Private.ivy_index_after_first_ivy_opt
  ) ;;




end ;;  

let diff w1 w2 = 
  let ( line1,
    cuna1,
    prna1,
    smun1,
    smsp1,
    unco1,
    ladi1,
    trea1,
    fiiv1,
    elor1,
    enin1,
    ivin1) =w1 
and  ( line2,
    cuna2,
    prna2,
    smun2,
    smsp2,
    unco2,
    ladi2,
    trea2,
    fiiv2,
    elor2,
    enin2,
    ivin2) =w2    
in
let temp1 = [
    line1<>line2,"lines";
    cuna1<>cuna2,"current_namespace";
    prna1<>prna2,"preceding_namespaces";
    smun1<>smun2,"smallest_unused_namespace_index";
    smsp1<>smsp2,"small_space_start_index_opt";
    unco1<>unco2,"unfinished_condition";
    ladi1<>ladi2,"last_directive_was_an_else";
    trea1<>trea2,"treated";
    fiiv1<>fiiv2,"first_ivy_index_opt";
    elor1<>elor2,"elsie_or_elif_for_first_index_opt";
    enin1<>enin2,"endif_index_for_first_ivy_opt";
    ivin1<>ivin2,"ivy_index_after_first_ivy_opt" 
] in 
List.filter_map (
   fun (x,y) -> if x then Some y else None
) temp1;;

let g1 = 
  Image.image (
       fun ssp -> 
        (ssp.Old_cee_text.Private.namespace,
         ssp.Old_cee_text.Private.start_idx,
         ssp.Old_cee_text.Private.end_idx)
      ) 
  (Old_cee_text.Private.compute_small_spaces_in_text text1) ;; 


let g2 = New_cee_text.last_warrior text1 ;;

let shortened_g1 = List_again.long_head (List.length g2) g1 ;; 

let g12 = Int_range.index_everything (List.combine shortened_g1 g2) ;;

let g3 = List.find (fun (idx,(a,b))->a<>b) g12 ;; 


let (idx1,((_,i1,j1),(_,_,_))) = g3 ;; 

let old_base = Old_cee_text.Private.Walker.make text1 ;; 

let old_ff = Memoized.small Old_cee_text.Private.Walker.step old_base ;;

let old_gg n = 
   let temp1 = old_ff n in 
   let temp2 = temp1.Old_cee_text.Private.Walker.Walker_Object.treated in 
   let ssp = List.hd temp2 in 
   (ssp.Old_cee_text.Private.namespace,
   ssp.Old_cee_text.Private.start_idx,
   ssp.Old_cee_text.Private.end_idx) ;;

let new_base = New_cee_text.Private.Walker.make text1 ;; 

let new_ff = Memoized.small New_cee_text.Private.Walker.step new_base ;;

let new_gg n = 
   let temp1 = new_ff n in 
   let temp2 = temp1.New_cee_text.Private.Walker.Walker_Object.treated in 
   let ssp = List.hd temp2 in 
   (ssp.New_cee_text.Private.namespace,
   ssp.New_cee_text.Private.start_idx,
   ssp.New_cee_text.Private.end_idx) ;;

let g4 = List.find (
  fun k->Old_cee_text.of_walker_object(old_ff k)
  <>New_cee_text.of_walker_object(new_ff k)
) (Int_range.range 1 3476) ;; 

let dff k = diff 
  (Old_cee_text.of_walker_object(old_ff k))
  (New_cee_text.of_walker_object(new_ff k)) ;; 

let old_h1 = old_ff 943 ;; 
let old_h1 = old_ff 943 ;; 


(*
let itv1 = Lines_in_text.interval text1 i1 j1 ;;

let start1 = Option.get(Substring.leftmost_index_of_in_from_opt 
   "/*" itv1 4263) ;;

let end1 = Option.get(Substring.leftmost_index_of_in_from_opt 
   "*/" itv1 start1) ;;   

let comment1 = Cull_string.interval itv1 start1 (end1+1) ;;   

let i2 = Option.get(Substring.leftmost_index_of_in_from_opt 
   comment1 text1 1) ;;

let j2 = i2 + (String.length comment1) - 1 ;;

let check2 = ((Cull_string.interval text1 i2 j2) = comment1) ;;

let z1 = Substring.occurrences_of_in "/*" text1 ;; 
let z2 = Substring.occurrences_of_in "*/" text1 ;; 

let z3 = List.combine z1 z2 ;;

let check_z3 = List.filter (fun (i,j) -> i>=j) z3 ;;

let z4 = List_again.universal_delta_list z3 ;; 

let check_z4 = List.filter (fun ((_,i),(j,_)) -> i>=j) z4 ;;

let z5 = List.filter (fun (i,j)->
   let t = Cull_string.interval text1 i j in 
   String.contains t '\n'
  ) z3 ;;

let the_comment = "/* Don't bother on windows with php_stream_open_wrapper,\n\treturn to give apprentice_load() a chance. *" ;;

let z6 = Image.image (fun (i,j) -> j - i ) z5 ;;

let z7 = Int_range.index_everything z5 ;;

let z8 = Image.image (
   fun (idx,(i,j))->
   let s= string_of_int(idx+12) 
   and t = Cull_string.interval text1 i (j+1) in 
   (t,"/* COMMENT "^s^" */")
) z7 ;;

let check_failure txt =
   try (fun _->false)(New_cee_text.Private.compute_small_spaces_in_text txt) with 
   _ -> true ;;

let ff k =
   let temp1 = List_again.long_head k z8 in 
   let new_text = Replace_inside.replace_several_inside_text
       temp1 text1 in 
   check_failure new_text ;;    

let z9 = List.filter (fun (x,y)->y<>"/* COMMENT 52 */") z8 ;;

let special_text = Replace_inside.replace_several_inside_text
       z9 text1 ;; *)

(*

#use"watched/watched_not_githubbed/ham.ml";;

*)



end ;;


(************************************************************************************************************************
Snippet 150 : Debug Lines_in_text.Private.lines_outside_cee_comments
************************************************************************************************************************)
module Snip150=struct

open Lines_in_text.Private ;;

let txt1 = String.concat "\n" [
   "1 When";"2 The "; "3 /* Saints"; "4 Go" ; "5 Marching */ In"; "6 Oh"
   ]  ;; 

lines_inside_or_outside_cee_comments txt1 ;; 

let txt2 = String.concat "\n" [
   "1 When";"2 The "; "3 '\\' /* Saints */"; "4 Go" ; "5 Marching In"; "6 Oh"
   ]  ;; 

lines_inside_or_outside_cee_comments txt2 ;;

let txt3 = String.concat "\n" [
   "1 When";"2 The "; "3 \"/*\" Saints"; "4 Go" ; "5 Marching \"*/\" In"; "6 Oh"
   ]  ;; 

lines_inside_or_outside_cee_comments txt3 ;; 

let w0 = {
      answer_opt = None ;
      next_idx = 15 ;
      current_state = Outside_comments_or_strings;
      text = txt2 ;
      text_length = String.length txt2;
  } ;;
let ff = Memoized.small step w0;;


end ;;


(************************************************************************************************************************
Snippet 149 : Debugging session
************************************************************************************************************************)
module Snip149=struct


module Cap = Cee_project_transfer.Capsule ;;

let list28 = (!(Cee_data_archiver.Cache_Content.list28_ref)) ;;
let wardrobe1 = (!(Cee_data_archiver.Cache_Content.wardrobe1_ref)) ;; 


let simplified_dir = Sys.getenv "SHPSRC" ^ "/" ;; 
let half_preprocessed_dir = Sys.getenv "HAHPSRC" ^ "/" ;;

let cpsl0 =
  Cap.make ~source_envname:"SHPSRC" 
  ~destination_envname:"HAHPSRC" list28 
  ~reinitialize_destination:true ;;

let change1 () =
  Chronometer.it
    Cee_project_transfer
    .remove_conditional_directives_in_directly_compiled_files 
     cpsl0 ;;

module Pri = Cee_project_transfer.Private ;;
module Pri2 = Cee_project_transfer.Private2 ;;
module PreC = Cee_project_transfer.Private2.PreCapsule ;; 

let bad1 =Cap.shadows_for_dc_files cpsl0 ;;

let unveiled_cpsl0 = Cap.unsafe_unveil cpsl0 ;;
let bad2 = PreC.compute_shadows_for_dc_files unveiled_cpsl0;; 

let cmds = PreC.separate_commands unveiled_cpsl0 ;;

let cmd = List.nth cmds 283 ;; 

let bad3 = Pri2.shadow_for_separate_command 
   (PreC.destination, PreC.read_file, PreC.create_file) 
   unveiled_cpsl0 cmd ;;


let name_for_included_file =
      Cee_compilation_command.short_name_from_separate cmd ;; 

let old_text = PreC.read_file unveiled_cpsl0 name_for_included_file ;;

let bad4 =
      Cee_text.tattoo_regions_between_conditional_directives 
      ~name_for_included_file old_text ;;

let lines = Cee_text.Private.indexed_lines_inside_or_outside_cee_comments old_text ;;

let bad5 = Cee_text.Private.compute_small_spaces_in_text old_text ;; 




end ;;


(************************************************************************************************************************
Snippet 148 : Testing Cee_text.Private.emphatize_first_ivy_and_last_endif
************************************************************************************************************************)
module Snip148=struct

module Pri = Cee_text.Private ;;

let ap1 = Absolute_path.of_string 
  "~/Teuliou/Experimenting_with_php/step-01-php-src/Zend/zend_extensions.h"
;;

let text1 = Io.read_whole_file ap1 ;; 

let text2 = Pri.emphatize_first_ivy_and_last_endif text1 ;; 

let act () = print_string text2 ;; 


end ;;


(************************************************************************************************************************
Snippet 147 : Finite sets stable by (x,y)->|x-y|
************************************************************************************************************************)
module Snip147=struct

let i_order = Total_ordering.for_integers ;; 

let i_merge = Ordered.merge i_order ;; 

let i_setminus = Ordered.setminus i_order ;; 

let i_sort = Ordered.sort i_order ;; 

type crab = Cr of int * ((int list) list) * (int list) ;;

let newly_produced (Cr(n,parts,whole))=
  let temp1 = Uple.list_of_pairs whole in 
  let temp2 = Image.image (fun (i,j)->j-i) temp1 in 
  let temp3 = i_sort temp2 in 
  i_setminus (i_setminus temp3 whole) [n-1] ;;

let rec iterate crrab =
  let (Cr(n,parts,whole)) = crrab in 
  let new_ones = newly_produced crrab in 
  if new_ones = []
  then crrab 
  else 
  let new_crrab = Cr(n,parts@[new_ones],i_merge whole new_ones) in 
  iterate new_crrab ;;

let ff = Memoized.make(fun (v,u)->
   let crrab = Cr(v,[[u;v]],[u;v]) in 
   let (Cr(_n,parts,whole)) = iterate crrab in 
   (parts,i_setminus (Int_range.range 1 (v-2)) whole) 
)  ;;

let gg = Memoized.make(fun t->
   let v=3*t+1 in 
   let temp1 = Int_range.scale (fun j->3*j-2) 1 (t+1) in 
   let crrab = Cr(v,[temp1],temp1) in 
   let (Cr(_n,parts,_whole)) = iterate crrab in 
   parts 
)  ;;

let hh t = 
   let temp1 = Int_range.scale (fun j->("A",3*j-2)) 1 (t+1) in 
   let temp2 = Int_range.scale (fun j->("B",3*j)) 1 (t-1) in
   let temp3 = temp1 @ temp2 in 
   let temp4 = Uple.list_of_pairs temp3 in 
       List.filter (
         fun ((lx,x),(ly,y)) -> abs(y-x) = 2
       ) temp4 ;; 


end ;;


(************************************************************************************************************************
Snippet 146 : Preliminary actions before using the Coherent pdf module
************************************************************************************************************************)
module Snip146=struct
let s_dir1 = home ^ "/Downloads/Pages" ;;

let dir1= Directory_name.of_string s_dir1 ;;

let u1 = Unix_again.beheaded_simple_ls dir1;;

let u2 = List.filter (fun s -> 
   String.starts_with s ~prefix:"q" ) u1 ;;

Sys.chdir s_dir1 ;;

let u3 = Image.image (fun s->
  "mv "^s^" p1"^(Cull_string.cobeginning 1 s)   
) u2 ;;

let act1 () = Image.image Sys.command u3 ;;

let u3 = Unix_again.beheaded_simple_ls dir1;;

let u4 = List.filter (fun s -> 
   String.starts_with s ~prefix:"p" ) u3 ;;

let indices = Ordered.sort Total_ordering.for_integers (Image.image (
   fun s -> int_of_string(Cull_string.two_sided_cutting ("p",".png") s)
) u4);;

let indexed_indices = Int_range.index_everything indices ;;

let u5 = Image.image (fun (i,j)->
  let si=string_of_int i and sj=string_of_int j in
  "mv p"^sj^".png r"^si^".png"   
) indexed_indices ;;

let act2 () = Image.image Sys.command u5 ;;

let n1 = 103 ;; (* For the Baskerville book*)
let n1 = List.length indexed_indices ;;

let u6 = Int_range.scale (fun i->
  let si=string_of_int i in
  "convert r"^si^".png p"^si^".pdf"   
) 1 n1 ;;

let act3 () = Image.image Sys.command u6 ;;

let u7 = "cpdf "^(String.concat " " (Int_range.scale (fun i->
  "p"^(string_of_int i)^".pdf"   
) 1 n1 ))^" -o baskerville_gentleman.pdf";;

let act4 () = Sys.command u7 ;;



end ;;


(************************************************************************************************************************
Snippet 145 : Fix error in an indexed list of files
************************************************************************************************************************)
module Snip145=struct

module Pri = Olavo_s_local_dircopy.Private ;;

let ldc1 = Local_dircopy.initialize Pri.config ;;

let reds1 = Local_dircopy.redundancies Pri.config ;;

let fns1 = Local_dircopy.Private.ordered_filenames Pri.config ;;

Pri.main_ref := Local_dircopy.initialize Pri.config ;;

let u1 = (!(Pri.main_ref)).Local_dircopy_t.remote_files ;;

let u2 = Image.image (fun fn ->
  let s= String.sub fn 1 3 in 
   (int_of_string s,fn)
  ) u1;;

let u3 = Int_range.scale  (fun j->(j,List.assoc j u2) ) 72 98 ;;

let u4 = Image.image (
  fun (j,fn) ->
    let ldc = Pri.config in 
    let rem_loc = ldc.Local_dircopy_config_t.remote_dir in 
    let str = string_of_int (j-1) in 
    let str2 = Strung.insert_repetitive_offset_on_the_left '0' ldc.Local_dircopy_config_t.allowed_number_of_digits str in
    let src = rem_loc^fn 
    and dest = rem_loc^"v"^str2^(Cull_string.cobeginning 4 fn) in 
    Filename.quote_command "mv" [src;dest]
) u3 ;;

let act1 () = Image.image Sys.command u4 ;;


end ;;


(************************************************************************************************************************
Snippet 144 : Code to prepare a phpBB upgrade
************************************************************************************************************************)
module Snip144=struct

let dir1 = Directory_name.of_string 
"~/Downloads//phpBB-3.3.14-files/phpBB-3.3.10_to_3.3.14" ;;

let s1 = Directory_name.connectable_to_subpath dir1 ;;
let u1 = Unix_again.complete_ls_with_nondirectories_only dir1 ;;

let u2 = Image.image Absolute_path.to_string u1 ;;

let u3 = Image.image 
  (Cull_string.cobeginning (String.length s1)) u2 ;;

let u4 = Image.image (fun x->
  let y = Cull_string.before_rightmost x '/' in 
  let z = (if y="" then "/" else "/"^y^"/") in
  "cp $PREP/phpBB_3_3_10_to_3_3_14/"^x^" $MAIN"^z
) u3 ;;  

let u5 = "\n\n\n"^(String.concat "\n" u4)^"\n\n\n" ;;

let s2 = home ^ "/Downloads/Comparison/" ;;

let tt fn = Io.read_whole_file(Absolute_path.of_string(s2^fn)) ;;

let txt1 = tt "common_before.css" ;;
let txt2 = tt "common_now.css" ;;

let txt1 = tt "colours_before.css" ;;
let txt2 = tt "colours_now.css" ;;

let l_txt1 = Lines_in_text.indexed_lines txt1 ;;

let l_txt2 = Lines_in_text.indexed_lines txt2 ;;

let m1 = min(List.length l_txt1)(List.length l_txt2) ;; 

let l_12 = Int_range.scale (fun j->
  (j,List.assoc j l_txt1,List.assoc j l_txt2)) 1 m1 ;;

let u6 = List.filter (fun (j,x,y)->x<>y) l_12 ;;  


end ;;


(************************************************************************************************************************
Snippet 143 : Using the Zarith module
************************************************************************************************************************)
module Snip143=struct

module Z= Zirath.Zay ;;
module Q = Zirath.Quay ;;



end ;;


(************************************************************************************************************************
Snippet 142 : Check a successor function
************************************************************************************************************************)
module Snip142=struct

let order1 x y =
   let trial1= Total_ordering.standard (abs x) (abs y) in 
   if trial1<> Total_ordering_result_t.Equal then trial1 else 
   if x = y then  Total_ordering_result_t.Equal else 
   Total_ordering.standard y x ;; 

let see1 = Ordered.sort order1 (Int_range.range (-5) 5) ;;   

let measure (x1,x2,x3) = Max.list (Image.image abs [x1;x2;x3]) ;;

let order2 tx ty =
   let trial1= Total_ordering.standard (measure tx) (measure ty) in 
   if trial1<> Total_ordering_result_t.Equal then trial1 else 
   let (x1,x2,x3) = tx and (y1,y2,y3) = ty in 
   let trial2= order1 x1 y1 in 
   if trial2<> Total_ordering_result_t.Equal then trial2 else  
   let trial3= order1 x2 y2 in 
   if trial3<> Total_ordering_result_t.Equal then trial3 else   
   order1 x3 y3;; 

let u1 = Cartesian.cube (Int_range.range (-11) 11) ;; 

let u2 =Ordered.sort order2 u1 ;;

let sp k = List.filter (fun t->measure(t)=k) u2 ;;

let ml k = List.hd(List.rev (sp k)) ;; 

let depth (x1,x2,x3) =
   let m = measure (x1,x2,x3) in 
   if abs(x1)=m then 1 else 
   if abs(x2)=m then 2 else  3 ;;

let u3 = List_again.universal_delta_list u2 ;;   

let sp1 k = List.filter (fun (t1,t2)->(measure t1=k)&&(depth t1=1)) u3 ;;

let sp2 k = List.filter_map (fun (t1,t2)->
  let (x1,x2,x3) = t1 in 
  if (measure t1=k)&&(x3=(-k))
  then Some((x1,x2),t2)  
  else None  ) u3 ;;


let successor_for_order1 k = 
   if k=0 then 1 else 
   if k>0 then (-k) else (-k)+1 ;; 



let adjust1 m x1  =
  let y1 = successor_for_order1 x1 in 
  if Max.list [abs y1] = m 
  then 0
  else m ;;  

let adjust2 m x1 x2 =
  let y2 = successor_for_order1 x2 in 
  if Max.list [abs x1;abs y2] = m 
  then 0
  else m ;;  



let successor (x1,x2,x3) =
  let m = measure (x1,x2,x3) in 
  if x3<>(-m) then (x1,x2,successor_for_order1 x3) else 
  if x2<>(-m) then (x1,successor_for_order1 x2,adjust2 m x1 x2) else 
  if x1<>(-m) then (successor_for_order1 x1,0,adjust1 m x1) else   
  (0,0,m+1) ;;  
       
let check_successors = List.filter (
   fun (t1,t2) -> t2 <> successor t1
) u3 ;; 


end ;;


(************************************************************************************************************************
Snippet 141 : Using the Coherent_pdf module
************************************************************************************************************************)
module Snip141=struct

let ap1 = Absolute_path.of_string (
   "~/Teuliou/Printable/numbers.pdf") ;;

let ap1 = Absolute_path.of_string (
   "~/Teuliou/Heavy/Workshop/richelieu.pdf") ;;


let act1 () = Coherent_pdf.corep_foldable_transform 
    ap1 ~outputfile_name:"output";;

let act2 () = Int_range.scale(fun k->
    let sk = string_of_int k in 
    let ap = Absolute_path.of_string 
  ("~/Teuliou/Heavy/planiol_hib_"^sk^".pdf") in 
    Coherent_pdf.corep_foldable_transform ap
    ~outputfile_name:("printable_hib_"^sk)
  ) 1 3 ;;    

let act3 () = Coherent_pdf.corep_foldable_transform 
    ( Absolute_path.of_string (
   "~/Teuliou/Heavy/baskerville_gentleman.pdf")) 
   ~outputfile_name:"output";;

let g3 () = Coherent_pdf.force_same_size_for_all_pages 
  ( Absolute_path.of_string (
   "~/Teuliou/Heavy/baskerville_gentleman.pdf"))  
 ~outputfile_name:"beskarville"
    ~forced_width:1095 ~forced_height:1430;;

let g4 () = Coherent_pdf.force_same_size_for_all_pages 
  ( Absolute_path.of_string (
   "~/Teuliou/Heavy/planiol_hib_3.pdf"))  
 ~outputfile_name:"planiol_hub_3"
    ~forced_width:1120 ~forced_height:740;;   


let ap1 = Absolute_path.of_string (
   "~/Teuliou/Heavy/Workshop/numbers.pdf") ;;

let ap2 = Absolute_path.of_string (
   "~/Teuliou/Heavy/Workshop/page.pdf") ;;   

let act1 () = Coherent_pdf.replace_inside 
  ~patient:ap1 ~replacer:ap2
    ~left_of_cut:7 ~right_of_cut:8 
     ~outputfile_name:"output" ;; 



;;

end ;;


(************************************************************************************************************************
Snippet 140 : Another debugging session on Cee_project_transfer
************************************************************************************************************************)
module Snip140=struct

  module Cap = Cee_project_transfer.Capsule

let list28, wardrobe1 = [],[] ;;
let simplified_dir = Sys.getenv "SHPSRC" ^ "/" ;; 
let half_preprocessed_dir = Sys.getenv "HAHPSRC" ^ "/" ;;

let cpsl0 =
  Cap.make ~source_envname:"SHPSRC" ~destination_envname:"HAHPSRC" list28

let change1 () =
  Chronometer.it
    Cee_project_transfer
    .remove_conditional_directives_in_directly_compiled_files cpsl0

    
let cpsl1 =
   Cap.replicate ~next_envname:"ST01PHPSRC" cpsl0 
   (* ~refill_files:true *) ;; 

let change2 =
  Cee_project_transfer.standardize_inclusions_in_files cpsl1
    (Cap.directly_compiled_files cpsl1) ;;

(* (* DEBUGDEBUGDEBUG BEGIN *)

module Pri = Cee_project_transfer.Private ;;
module Pri2 = Cee_project_transfer.Private2 ;;
module Prec = Pri2.PreCapsule ;;


let u1 = Cap.directly_compiled_files cpsl1 ;;
let fn1 = List.nth u1 382 ;; 

(* let bad1 = 
Cee_project_transfer.standardize_inclusions_in_files cpsl1
    [fn1] ~dry_run:true ;; *)


let bad2 =
    Pri.nonstandard_inclusion_formats_in_individual_includer 
    cpsl1 fn1 ;; 

let includer_fn = fn1 ;;    

let inc_source_dirs =
      Pri2.included_source_dirs_for_file 
      Cap.separate_commands cpsl1 includer_fn ;;

let text = Cap.read_file cpsl1 includer_fn ;;

let old_temp1 = Cee_text.included_local_files_in_text text ;;

let temp1 = [List.nth old_temp1 16] ;;
let lines = Lines_in_text.indexed_lines text ;; 

let bad3 =
      Image.image
        (fun (line_nbr, vague_included_fn) ->
          let iar =
            Pri2.parse_cee_inclusion_line
              Cap.all_h_or_c_files
              cpsl1
              includer_fn
              vague_included_fn
              inc_source_dirs
          in
          ( includer_fn
          , vague_included_fn
          , List.assoc line_nbr lines
          , Pri2.Individual_inclusion_analysis.read iar ))
        temp1 ;;

let (line_nbr, vague_included_fn) = List.nth old_temp1 16 ;;     

let bad4 =
            Pri2.parse_cee_inclusion_line
              Cap.all_h_or_c_files
              cpsl1
              includer_fn
              vague_included_fn
              inc_source_dirs ;;
 
let included_fn = vague_included_fn ;;              

let bad5 =
            Pri2.analize_slashed_included_filename
              Cap.all_h_or_c_files
              cpsl1
              includer_fn
              vague_included_fn
              inc_source_dirs ;;

let bad6 =
            Pri2.analize_slashed_nonpointed_included_filename
              Cap.all_h_or_c_files
              cpsl1
              includer_fn
              vague_included_fn
              inc_source_dirs ;;


(* DEBUGDEBUGDEBUG END *) *)

end ;;


(************************************************************************************************************************
Snippet 139 : Debugging session on Cee_project_transfer
************************************************************************************************************************)
module Snip139=struct

module Cap = Cee_project_transfer.Capsule

let list28, wardrobe1 = [],[](* Large_data.data *) ;;
let simplified_dir = Sys.getenv "SHPSRC" ^ "/" ;; 
let half_preprocessed_dir = Sys.getenv "HAHPSRC" ^ "/" ;;

let cpsl0 =
  Cap.make ~source_envname:"SHPSRC" ~destination_envname:"HAHPSRC" list28

let change1 () =
  Chronometer.it
    Cee_project_transfer
    .remove_conditional_directives_in_directly_compiled_files cpsl0

    
let cpsl1 =
   Cap.replicate ~next_envname:"ST01PHPSRC" cpsl0 
   (* ~refill_files:true *) ;; 

let change2 =
  Cee_project_transfer.standardize_inclusions_in_files cpsl1
    (Cap.directly_compiled_files cpsl1) ;;

(* (* DEBUGDEBUGDEBUG BEGIN *)

module Pri = Cee_project_transfer.Private ;;
module Pri2 = Cee_project_transfer.Private2 ;;
module Prec = Pri2.PreCapsule ;;


let u1 = Cap.directly_compiled_files cpsl1 ;;
let fn1 = List.nth u1 382 ;; 

(* let bad1 = 
Cee_project_transfer.standardize_inclusions_in_files cpsl1
    [fn1] ~dry_run:true ;; *)


let bad2 =
    Pri.nonstandard_inclusion_formats_in_individual_includer 
    cpsl1 fn1 ;;

let includer_fn = fn1 ;;    

let inc_source_dirs =
      Pri2.included_source_dirs_for_file 
      Cap.separate_commands cpsl1 includer_fn ;;

let text = Cap.read_file cpsl1 includer_fn ;;

let lines = Lines_in_text.indexed_lines text ;; 

let temp5 = Cee_text.included_nonlocal_files_in_text text ;;

(* let bad3 =
      List.filter_map
        (fun (line_nbr, included_fn) ->
          let iar =
            Pri2.parse_cee_inclusion_line
              Cap.all_h_or_c_files
              cpsl1
              includer_fn
              included_fn
              inc_source_dirs
          in
          match Pri2.Individual_inclusion_analysis.read iar with
          | None -> None
          | Some answer -> Some (List.assoc line_nbr lines, "#include \"" ^ answer ^ "\""))
        temp5 ;; *)

let (line_nbr, included_fn) = List.nth temp5 12 ;; 

(* let bad4 =
            Pri2.parse_cee_inclusion_line
              Cap.all_h_or_c_files
              cpsl1
              includer_fn
              included_fn
              inc_source_dirs ;; *)

let bad5 =
            Pri2.analize_nonslashed_included_filename
              Cap.all_h_or_c_files
              cpsl1
              includer_fn
              included_fn
              inc_source_dirs ;; 

let current_dir = Cull_string.before_rightmost includer_fn '/' ;;

let all_files = Cap.all_h_or_c_files cpsl1 ;; 



(* DEBUGDEBUGDEBUG END *) *)




end ;;


(************************************************************************************************************************
Snippet 138 : Combinatorics on double transversals
************************************************************************************************************************)
module Snip138=struct

let g1 = Ordered_misc.minimal_transversals ;; 

let i_order = Total_ordering.for_integers ;;
let i_intersection = Ordered.intersect i_order ;;
let i_merge = Ordered.merge i_order ;;

let il_order = Total_ordering.silex_for_intlists ;;
let il_sort = Ordered.sort il_order ;;


let rec helper_for_double_transversals (already_treated,to_be_treated) =
    match to_be_treated with 
    [] -> il_sort already_treated 
    | a :: others ->
      let temp1 = Cartesian.product already_treated (Uple.list_of_pairs a) in 
      let temp2 = Image.image (fun (x,(a,b))->i_merge x [a;b]) temp1 in 
      let temp3 = Ordered_misc.minimal_elts_wrt_inclusion temp2 in 
      helper_for_double_transversals (temp3,others) ;;  

let double_transversals ll = 
   helper_for_double_transversals ([[]],ll);;      

let test_by_one x1 x2 = 
  (List.length(i_intersection x1 x2)>=2) ;;   

let test_by_several lx1 x2 = 
   List.for_all (test_by_one x2) lx1 ;;

let base = il_sort(List_again.power_set (Int_range.range 1 6))    ;;


let example1 = [[1;2;3;4];[1;2;5;6];[3;4;5;6]] ;;

let base1 = List.filter (test_by_several example1) base ;;

let g1 = double_transversals example1 ;;

let other_g1 = Ordered_misc.minimal_elts_wrt_inclusion base1 ;;


end ;;


(************************************************************************************************************************
Snippet 137 : Extract and reorder pages in a PDF
************************************************************************************************************************)
module Snipp145=struct

let s_dir1 = home ^ "/Teuliou/Heavy/Workshop" ;;

let dir1 = Directory_name.of_string s_dir1 ;; 

let command_for_page_extraction k = 
  let sk = string_of_int k in 
  "cpdf whole.pdf "^sk^"-"^sk^" -o p"^sk^".pdf" ;;

let text_for_extraction_script  nbr_of_pages =
 "#! Extract pages from a pdf using cpdf\n\n\n"^
 (String.concat "\n" 
 (Int_range.scale command_for_page_extraction 1 nbr_of_pages))^
 "\n\n\n" ;;

let extractor_file = Absolute_path.of_string 
  (home ^ "/Teuliou/Heavy/Workshop/extract_pages.sh") ;;

let write_extraction_script  nbr_of_pages =
 Io.overwrite_with extractor_file
(text_for_extraction_script nbr_of_pages) ;;

let basic_block_for_page_reunion k = 
  let p = (fun j->"p"^(string_of_int j)^".pdf ") in 
  (p(4*k+4))^(p(4*k+1))^(p(4*k+3))^(p(4*k+2)) ;;

let text_for_reunion_script  nbr_of_pages =
 "#! Reorder pages from a pdf using cpdf\n\n\n"^
 "cpdf "^
 (String.concat "" 
 (Int_range.scale basic_block_for_page_reunion 
 0 ((nbr_of_pages/4)-1)))^
 " -o reordered.pdf"^
 "\n\n\n" ;;

let reuniter_file = Absolute_path.of_string 
  (home ^ "/Teuliou/Heavy/Workshop/reunite_pages.sh") ;;

let write_reunion_script  nbr_of_pages =
 Io.overwrite_with reuniter_file
(text_for_reunion_script nbr_of_pages) ;;



end ;;


(************************************************************************************************************************
Snippet 136 : 
************************************************************************************************************************)
module Snipp144=struct
let heim = Cull_string.two_sided_cutting ("/home/","") 
    Needed_values.home ;;

let s_dir1 = "/media/"^heim^"/HEAVY/French/Cinq_dernieres_minutes/" ;; 
let dir1 = Directory_name.of_string s_dir1 ;; 

let u1 = Unix_again.beheaded_simple_ls dir1 ;; 

let u2 = Image.image (fun s->
  "mv "^s_dir1^s^" "^s_dir1^"CDM_"^s
  ) u1 ;; 

let u3 = Image.image Sys.command u2 ;;   


end ;;


(************************************************************************************************************************
Snippet 135 : Boilerplate code for C project management
************************************************************************************************************************)
module Snipp143=struct

  (*

#use"watched/watched_not_githubbed/large_data.ml";;
#use"watched/watched_not_githubbed/ham.ml";;

module Pri = Cee_project_transfer.Private2 ;;
module Prec = Pri.PreCapsule ;;

let cpsl_ref = Prec.make 
  ~source_envname:"ST01PHPSRC" 
   ~destination_envname:"ST02PHPSRC" 
   list28 ;;
let all_cmds = Prec.separate_commands cpsl_ref ;;

let cmds = List_again.long_head 3 all_cmds ;;

let indexed_cmds = Int_range.index_everything cmds ;;
let s_num_of_cmds = string_of_int (List.length cmds) ;; 

let hard1 = Image.image
        (fun (idx, cmd) ->
          ( Cee_compilation_command.short_name_from_separate cmd
          , Pri.wardrobe_for_indexed_separate_command
              (Prec.destination, Prec.read_file, Prec.create_file, Prec.inclusions_in_dc_files)
              cpsl_ref
              (idx, cmd)
              s_num_of_cmds ))
        indexed_cmds ;;


let cmd = List.hd cmds ;;

let separate_cmd = cmd ;;
let cpsl = cpsl_ref ;;
let s_num_of_cmds = "1" ;;

let (cpsl_destination, cpsl_read_file, cpsl_create_file, cpsl_inclusions_in_dc_files) = 
(Prec.destination, Prec.read_file, Prec.create_file, Prec.inclusions_in_dc_files) ;;

let short_name = Cee_compilation_command.short_name_from_separate separate_cmd
and s_idx = string_of_int 1 ;;

let indexed_name = short_name ^ " (" ^ s_idx ^ " of " ^ s_num_of_cmds ^ ")" ;;
    
let short_filename = short_name ;; 

let temp1 = cpsl_inclusions_in_dc_files cpsl ;;

let dest_dir = Directory_name.connectable_to_subpath 
(cpsl_destination cpsl) ;;


let old_text = cpsl_read_file cpsl short_name ;;

let text_to_be_preprocessed, _nbr_of_inclusions =
      Cee_text.highlight_and_add_extra_ending_in_inclusions_inside_text 
        ~extra:"includable" old_text ;;
    
      
let preprocessed_includer_text =
      Pri.compute_preprocessing_output_for_separate_shadow
        (cpsl_destination, cpsl_create_file)
        cpsl
        separate_cmd
        text_to_be_preprocessed ;; 

let answer =
      Cee_text.compute_wardrobe
        ~preprocessed_includer_text
        copied_includable_files ;;
    in
    let _ =
      if not !keep_temporary_files_mode
      then (
        let _ =
          Image.image
            (fun (_, _, fn,_) -> Unix_command.uc ("rm -f " ^ dest_dir ^ fn))
            copied_includable_files
        in
        ())
    in
    let _ = announce ("Computation of wardrobe finished for " ^ indexed_name ^ ".") in
    answer
  ;;

let bad3 =
      Pri.create_copies_of_included_files_for_wardrobe
        (cpsl_inclusions_in_dc_files, cpsl_read_file, cpsl_create_file)
        cpsl
        short_name ;;


let bad2 =
Pri.wardrobe_for_indexed_separate_command
              (Prec.destination, Prec.read_file, Prec.create_file, Prec.inclusions_in_dc_files)
              cpsl_ref
              (1, cmd)
              "1" ;;

let bad1 = Image.image
        (fun (idx, cmd) ->
          ( Cee_compilation_command.short_name_from_separate cmd
          , Pri.wardrobe_for_indexed_separate_command
              (Prec.destination, Prec.read_file, Prec.create_file, Prec.inclusions_in_dc_files)
              cpsl_ref
              (idx, cmd)
              s_num_of_cmds ))
        indexed_cmds ;;

*)


(* 

#use"watched/watched_not_githubbed/large_data.ml";;
#use"watched/watched_not_githubbed/ham.ml";;

let hard1 = Chronometer.it 
  Cap.wardrobes_for_dc_files cpsl3 ;; 





#use"watched/watched_not_githubbed/large_data.ml";;

Large_data.store_as_wardrobe1 hard1 ;;

#use"watched/watched_not_githubbed/large_data.ml";;


let hurd1 = Large_data.encode_wardrobe hard1 ;;

let check_hurd1 = ((Large_data.decode_wardrobe hurd1) = hard1 ) ;;


 
let check1 = (snd(Large_data.data) = hard1) ;;  

let u1 = Cap.wardrobes_for_di_files cpsl3 ;; 

let u2 = Image.image (fun (included_fn,l)->
   (included_fn,Image.image snd l)  
) u1 ;;


*)



end ;;


(************************************************************************************************************************
Snippet 134 : Use example of the Marshal module
************************************************************************************************************************)
module Snip134=struct

  let hard1 = ([]: int list) ;; 

let chan = home ^"/Teuliou/OCaml/skeptical_duck/"^
 "nonml_files/nongithubbed_nonml_files/horse.marshal" ;;

let outer_chan = open_out_bin chan ;;

Marshal.to_channel outer_chan hard1 [] ;;

close_out outer_chan ;;

let inner_chan = open_in_bin chan ;;

let hard2 = ((Marshal.from_channel inner_chan): 
(string * (string * Cee_shadow_t.t) list) list) ;;

close_in inner_chan ;;


end ;;


(************************************************************************************************************************
Snippet 133 : 
************************************************************************************************************************)
module Snip133=struct

  module XSum = struct 

    type t = XS of bool * bool * bool * bool ;;
    
    let x k = XS(k=1,k=2,k=3,k=4);;
    
    let x_sum l = let m = (fun i->List.mem i l) in 
        XS(m 1,m 2,m 3,m 4);;
    
    let degree (XS(b1,b2,b3,b4)) = 
      match List.find_opt snd [4,b4;3,b3;2,b2;1,b1] with 
      None -> 0
      |Some(d,_) -> d ;;
    
    let cardinality (XS(b1,b2,b3,b4)) = 
       List.length(List.filter (fun b->b) [b1;b2;b3;b4]);;
    
    let naive_base =
      let temp1 = Cartesian.square [false;true] in 
      let temp2 = Cartesian.square temp1 in 
      Image.image (fun ((b1,b2),(b3,b4)) -> 
         XS(b1,b2,b3,b4) ) temp2 ;;
    
    let comparison_at_separation_point 
       (XS(b1,b2,b3,b4)) (XS(c1,c2,c3,c4)) = 
       match List.find_opt (fun (b,c)->b<>c) 
          [b4,c4;b3,c3;b2,c2;b1,c1] with 
       None -> Total_ordering_result_t.Equal 
       |Some(b,c) ->
          if b 
          then Total_ordering_result_t.Greater 
          else Total_ordering_result_t.Lower ;; 
    
    
    let pre_xs_order xs1 xs2 = 
      let trial1 = Total_ordering.standard (cardinality xs1) (cardinality xs2) in 
      if trial1<>Total_ordering_result_t.Equal then trial1 else 
      comparison_at_separation_point xs1 xs2 ;;     
    
    let xs_order = (pre_xs_order: t Total_ordering_t.t) ;; 
    
    let base = List.tl(Ordered.sort xs_order naive_base);; 
    
    let zero = XS(false,false,false,false) ;; 
    
    let rec helper_for_colin_decomposition (treated,to_be_treated) =
      match to_be_treated with 
      [] -> (treated,to_be_treated)
      | xs :: others -> 
        if xs = x(treated+1)
        then helper_for_colin_decomposition (treated+1,others)
        else (treated,to_be_treated) ;;
    
    let colin_decomposition l =  
      helper_for_colin_decomposition (0,l) ;;   
    
    let redundant_patterns = [
        [x 1;x 2;x 3;x_sum [1;3]];
        [x 1;x 2;x 3;x_sum [2;3]];
        [x 1;x 2;x 3;x_sum [1;3];x_sum [2;3]];
        [x 1;x 2;x 3;x_sum [1;2];x_sum [2;3]]; 
        [x 1;x 2;x 3;x_sum [1;2];x_sum [1;3];x_sum [1;2;3]]; (* nontrivial *)
        [x 1;x 2;x 3;x_sum [1;2];x_sum [1;2;3]]; (* nontrivial *)
        [x 1;x 2;x 3;x 4;x_sum [1;3]]; 
        [x 1;x 2;x 3;x 4;x_sum [2;3]]; 
        [x 1;x 2;x 3;x 4;x_sum [1;4]]; 
        [x 1;x 2;x 3;x 4;x_sum [2;4]]; 
        [x 1;x 2;x 3;x 4;x_sum [3;4]]; 
        [x 1;x 2;x 3;x 4;x_sum [1;2;4]]; 
        [x 1;x 2;x 3;x 4;x_sum [1;3;4]]; 
        [x 1;x 2;x 3;x 4;x_sum [2;3;4]];
        [x 1;x 2;x 3;x 4;x_sum [1;2];x_sum [2;3]]; 
        [x 1;x 2;x 3;x 4;x_sum [1;2];x_sum [1;4]]; 
        [x 1;x 2;x 3;x 4;x_sum [1;2];x_sum [2;4]]; 
        [x 1;x 2;x 3;x 4;x_sum [1;2];x_sum [1;2;3]]; (* nontrivial *)
        [x 1;x 2;x 3;x 4;x_sum [1;2];x_sum [1;2;4]];
        [x 1;x 2;x 3;x 4;x_sum [1;2];x_sum [2;3;4]];  
        [x 1;x 2;x 3;x 4;x_sum [1;2];x_sum [1;2;3;4]]; (* nontrivial *)
        [x 1;x 2;x 3;x 4;x_sum [1;2;3];x_sum [1;3;4]];
        [x 1;x 2;x 3;x 4;x_sum [1;2;3];x_sum [2;3;4]];  
        [x 1;x 2;x 3;x 4;x_sum [1;2;3];x_sum [1;2;3;4]]; (* nontrivial *)
        
    ] ;;
    
    
    let test_for_insertability l xs = 
      if List.mem xs l then false else 
      if l = [] then xs = x 1 else  
      let (d,others) = colin_decomposition l in 
      if cardinality(xs)=1
      then (others=[])&&(d<4)&&(xs=x(d+1))    
      else  
      if List.mem (l@[xs]) redundant_patterns then false else  
      let last_elt = List.hd (List.rev l) in 
      (degree(xs)<=d) && ((xs_order last_elt xs)
      <>Total_ordering_result_t.Greater) ;;   
       
    let sons_of_one_father l = 
      List.filter_map (
        fun xs -> 
          if test_for_insertability l xs
          then Some(l@[xs])
          else None  
      ) base ;;
    
    let sons ll = List.flatten (Image.image sons_of_one_father ll) ;;  
    
    let sphere = Memoized.recursive(fun old_f k->
      if k<1 then [[]] else sons (old_f (k-1))
    )
    
    let boolean_add (b1:bool) b2 = (b2<>b1) ;; 
    
    let add (XS(b1,b2,b3,b4))  (XS(c1,c2,c3,c4)) = 
    XS(b1<>c1,b2<>c2,b3<>c3,b4<>c4) ;;
    
    let add_several l = List.fold_left add zero l ;; 
    
    let partial_sum l indices =
       add_several(Image.image (fun k->List.nth l (k-1)) indices) ;;
    
    let all_combinations = Memoized.make(fun n->
      List.tl(Ordered.sort Total_ordering.silex_for_intlists
      (List_again.power_set (Int_range.range 1 n))));;   
    
    let zero_sums l = 
      let n = List.length l in 
       List.filter (fun indices ->
        (partial_sum l indices) = zero  
      )(all_combinations n) ;;
    
    let shadow =Memoized.make(fun l ->
       let n = List.length l in 
       let z_sums = zero_sums l in 
       Image.image (fun t->
         List.length (List.filter(fun z->List.length(z)=t) z_sums)  
      ) (Int_range.range 3 n) );;
    
    let classify_by_shadow ll = 
       let temp1 = Image.image shadow ll in 
       let temp2 = List_again.nonredundant_version temp1 in 
       Image.image (
         fun sha -> (sha,List.filter(fun l->shadow(l)=sha) ll)
       ) temp2 ;;
    
    let naive_cs = Memoized.make(fun n ->
       classify_by_shadow (sphere n) );;
    
    let enhanced_cs = Memoized.make(fun n -> Image.image 
       (fun (x,y)->(x,List.hd y)) (naive_cs n) );;
    
    let to_string (XS(b1,b2,b3,b4)) = 
      if (b1,b2,b3,b4)=(false,false,false,false) 
      then "0"
      else
      let indices = List.filter_map (
        fun (i,b) -> 
          if b 
          then Some ("X"^(string_of_int i)) 
          else None 
      ) [1,b1;2,b2;3,b3;4,b4] in 
      String.concat " + " indices ;;  
    
    let print_out (fmt:Format.formatter) xs=
     Format.fprintf fmt "@[%s@]" (to_string xs);;
    
    
    end ;;
    
    let s = XSum.sphere ;;
    let cs = XSum.enhanced_cs ;; 
    
    let term1 = cs 7 ;; 
    let (_,term2) = List.nth term1 1;;
    let term3 = List_again.long_head 5 term2 ;; 
    let s_term3 = XSum.add_several term3 ;;
    
    let excluded_elements = Ordered.sort XSum.xs_order 
      (XSum.zero::s_term3::term3@(Image.image (XSum.add s_term3) term3)) ;; 
    
    let allowed_elements = Ordered.setminus XSum.xs_order 
       XSum.base excluded_elements ;;
    
    let rings = Image.image (
       fun x -> 
        term3 @ [x;XSum.add s_term3 x]
    ) allowed_elements ;;
    
    let permutation_base = Permutation.permutations (Int_range.range 1 7) ;;
    
    let analize_ring =Memoized.make(fun ring -> 
      (ring,List.filter_map (
        fun perm -> 
           let adjusted_ring = Permutation.product ring perm in 
           if (XSum.partial_sum adjusted_ring [1;2;5]=XSum.zero) &&
              (XSum.partial_sum adjusted_ring [1;3;6]=XSum.zero) &&
              (XSum.partial_sum adjusted_ring [1;4;7]=XSum.zero) 
           then Some(perm,adjusted_ring)
           else None    
      ) permutation_base) 
    ) ;;
    
    let res1 = Image.image analize_ring rings ;;
    
    let res2 = Image.image (
       fun (ring,l) ->
        let temp = Image.image (fun t->List.hd(fst t)) l in 
        (ring,Ordered.sort Total_ordering.for_integers temp ) 
    ) res1 ;;
    
    let example1 = 
      let x= XSum.x and x_sum = XSum.x_sum in 
      [x 1;x 2;x 3;x 4;
       x_sum [1;2];x_sum [1;3];x_sum [1;3;4]] ;;
    
    let example2 = 
      let x= XSum.x and x_sum = XSum.x_sum in 
        [x 1;x 2;x 3;x 4;
         x_sum [1;2];x_sum [1;2;3];x_sum [1;2;3;4]] ;;   
    
    let example3 = 
      let x= XSum.x and x_sum = XSum.x_sum in 
          [x 1;x 2;x 3;x 4;
           x_sum [1;2];x_sum [2;3];x_sum [2;3;4]] ;;
    
    
    let partial = 
      let x= XSum.x and x_sum = XSum.x_sum in 
          [x 1;x 2;x 3;x 4;
           x_sum [1;2];x_sum [3;4]] ;;
    let allowed_elements = Ordered.setminus XSum.xs_order 
        XSum.base partial ;;
    
    analize_ring(partial @ [XSum.x_sum[1;3]]) ;; 
    
    analize_ring(partial @ [XSum.x_sum[1;3;4]]) ;; 
    
    analize_ring(partial @ [XSum.x_sum[1;2;3;4]]) ;; 
    
    let partial2 = 
      let x= XSum.x and x_sum = XSum.x_sum in 
          [x 1;x 2;x 3;x 4;
           x_sum [1;2]] ;;
    let whole = XSum.x_sum[1;2;3;4] ;; 
    
    let excluded_elements = Ordered.sort XSum.xs_order 
      (XSum.zero::whole::partial2@(Image.image (XSum.add whole) partial2)) ;; 
    
    let allowed_elements = Ordered.setminus XSum.xs_order 
       XSum.base excluded_elements ;;
    
    
    
    
       let analize_ring =Memoized.make(fun ring -> 
        (ring,List.filter_map (
          fun perm -> 
             let adjusted_ring = Permutation.product ring perm in 
             if (XSum.partial_sum adjusted_ring [1;2;5]=XSum.zero) &&
                (XSum.partial_sum adjusted_ring [1;3;6]=XSum.zero) &&
                (XSum.partial_sum adjusted_ring [2;4;7]=XSum.zero) 
             then Some(perm,adjusted_ring)
             else None    
        ) permutation_base) 
      ) ;;
    
      let example4 = 
        let x= XSum.x and x_sum = XSum.x_sum in 
            [x 1;x 2;x 3;x 4;
             x_sum [1;2];x_sum [1;3];x_sum [2;3;4]] ;;
      
    let x= XSum.x and x_sum = XSum.x_sum ;;         
    let partial3 = 
      let x= XSum.x and x_sum = XSum.x_sum in 
      [x 1;x 2;x 3;x 4;
         x_sum [1;2];x_sum[1;2;3;4]] ;;
    let excluded_elements = Ordered.sort XSum.xs_order 
      (XSum.zero::partial3@
      [x_sum [3;4];x_sum [1;2;3];x_sum[1;2;4]]) ;; 
            
    let allowed_elements = Ordered.setminus XSum.xs_order 
               XSum.base excluded_elements ;;
    
    
    let analize_ring =Memoized.make(fun ring -> 
      (ring,List.filter_map (
        fun perm -> 
         let adjusted_ring = Permutation.product ring perm in 
         if (XSum.partial_sum adjusted_ring [1;2;5]=XSum.zero) &&
            (XSum.partial_sum adjusted_ring [1;3;6]=XSum.zero) &&
            (XSum.partial_sum adjusted_ring [2;3;4;7]=XSum.zero) 
         then Some(perm,adjusted_ring)
         else None    
      ) permutation_base) 
    ) ;;
    
    analize_ring(partial3 @ [XSum.x_sum[1;3]]) ;; 
    analize_ring(partial3 @ [XSum.x_sum[1;3;4]]) ;; 
    
    let partial4 = 
      [x 1;x 2;x 3;x 4;
         x_sum [1;2];x_sum[1;2;3]] ;;
    
let example5 = 
  [x 1;x 2;x 3;x 4;
    x_sum[1;2;3];x_sum[1;2;4];x_sum[1;2;3;4] ] ;;

let example6 = 
  [x 1;x 2;x 3;x 4;
        x_sum[1;2];x_sum[1;3];x_sum[2;3;4] ] ;;


let analize_ring =Memoized.make(fun ring -> 
      (ring,List.filter_map (
        fun perm -> 
         let adjusted_ring = Permutation.product ring perm in 
         if (XSum.partial_sum adjusted_ring [1;2;5]=XSum.zero) &&
            (XSum.partial_sum adjusted_ring [1;3;6]=XSum.zero) &&
            (XSum.partial_sum adjusted_ring [2;3;4;7]=XSum.zero) 
         then Some(perm,adjusted_ring)
         else None    
      ) permutation_base) 
) ;;


    XSum.zero_sums (partial4 @ [x_sum[1;3]]) ;;
    XSum.zero_sums (partial4 @ [x_sum[2;3]]) ;;
    XSum.zero_sums (partial4 @ [x_sum[1;4]]) ;;
    XSum.zero_sums (partial4 @ [x_sum[2;4]]) ;;
    XSum.zero_sums (partial4 @ [x_sum[3;4]]) ;;
    XSum.zero_sums (partial4 @ [x_sum[1;2;4]]) ;;
    
    let analize_ring =Memoized.make(fun ring -> 
      (ring,List.filter_map (
        fun perm -> 
         let adjusted_ring = Permutation.product ring perm in 
         if (XSum.partial_sum adjusted_ring [1;2;5]=XSum.zero) &&
            (XSum.partial_sum adjusted_ring [1;3;6]=XSum.zero) &&
            (XSum.partial_sum adjusted_ring [2;3;7]=XSum.zero) 
         then Some(perm,adjusted_ring)
         else None    
      ) permutation_base) 
    ) ;;
    
    analize_ring(partial4 @ [x_sum[1;3]]) ;;
    
    let partial5 = 
      [x 1;x 2;x 3;x 4;
         x_sum [1;2];x_sum[1;4]] ;;
    
    XSum.zero_sums (partial5 @ [x_sum[1;3]]) ;;
    XSum.zero_sums (partial5 @ [x_sum[2;3]]) ;;
    XSum.zero_sums (partial5 @ [x_sum[2;4]]) ;;
    XSum.zero_sums (partial5 @ [x_sum[3;4]]) ;;
    
    let partial6 = 
      [x 1;x 2;x 3;x 4;
         x_sum [1;2];x_sum[3;4]] ;;
    
    XSum.zero_sums (partial6 @ [x_sum[1;3]]) ;;
    XSum.zero_sums (partial6 @ [x_sum[2;3]]) ;;
    
    
    (*
    
    #use"watched/watched_not_githubbed/jug.ml";;
    
    #install_printer XSum.print_out ;; 
    
    s 1 ;;
    
    let term1 = cs 7 ;; 
    let (_,term2) = List.nth term1 1;;
    
    
    
    
    
    let term3 = List.nth term2 1 ;;
    
    let term4 = Permutation.permutations term3 ;;
    let term5 = List.filter (
     fun l->
      (XSum.partial_sum l [1;2;4]=XSum.zero) &&
      (XSum.partial_sum l [1;3;5]=XSum.zero) &&
      (0=0)  
    ) term4 ;;
    
    let term7 = Image.image 
      (fun (x,y)->(x,List.hd y)) (cs 7) ;;
    
    
    
    *)


end ;;


(************************************************************************************************************************
Snippet 132 : Snippet from Stackoverflow Code Review
************************************************************************************************************************)
module Snip132=struct

  let string_contains_at_index str substr idx =
    let str_len = String.length str in
    let substr_len = String.length substr in
    idx + substr_len <= str_len &&
    String.sub str idx substr_len = substr ;;
  
  type tok =
    | Text of string
    | Delim of string ;;
  
  let token_seq delims str =
    let str_len = String.length str in
    let rec aux i text_acc () =
      if i >= str_len && text_acc <> "" then
        Seq.Cons (Text text_acc, Seq.empty)
      else if i >= str_len then
        Seq.Nil
      else 
        let check delim = string_contains_at_index str delim i in
        match List.find_opt check delims with
        | None -> 
          aux (i + 1) (text_acc ^ String.sub str i 1) ()
        | Some delim when text_acc = "" -> 
          Seq.Cons (Delim delim, aux (i + String.length delim) "")
        | Some delim -> 
          Seq.Cons (Text text_acc, aux i "")
    in
    aux 0 "" ;;
  
  let sample = 
      "How (much (research effort) is {expected} when) BEGIN posting a"^
      "Code Review ENDquestion? A "^
      "lot. {{(An absurd amount)}}. More BEGIN than  BEGIN you think END"^
      "you ENDare capable of.";;
  
      let delims = [
        ("(", ")");
        ("{", "}");
        ("BEGIN", "END")
      ];;    
  
  type block =
    | Text_block of string 
    | Paren_block of string * string * string ;;
  
  module StrMap = Map.Make (String) ;;
  
  let start_to_end_map = StrMap.of_list delims ;;
  let end_to_start_map = 
      StrMap.of_seq 
      @@ Seq.map (fun (a, b) -> (b, a)) 
      @@ List.to_seq delims  ;;
    
  let decomposed_seq (s_to_e_map, e_to_s_map) chunks =
    let is_start_delim d = StrMap.mem d s_to_e_map in
    let is_end_delim d = StrMap.mem d e_to_s_map in
    let extract_text (Delim t | Text t) = t in
    let rec aux chunks acc delim_stack () =
          match chunks (), acc, delim_stack with
      | Seq.Nil, [], _ -> Seq.Nil
      | Seq.Nil, _, _ -> 
            Seq.Cons (
              Text_block (acc |> List.rev |> List.map extract_text |> String.concat ""), 
              Seq.empty
            )     
      | Seq.Cons ((Delim delim as wd), seq'), [], [] 
            when is_start_delim delim ->
            aux seq' [wd] [delim] ()
      | Seq.Cons ((Delim delim as wd), seq'), _, [] 
            when is_start_delim delim ->
          Seq.Cons (
              Text_block (acc |> List.rev |> List.map extract_text |> String.concat ""),
              aux seq' [wd] [delim]
            )
      | Seq.Cons ((Delim delim as wd), seq'), _, _::_ 
            when is_start_delim delim ->
            aux seq' (wd :: acc) (delim :: delim_stack) ()
      | Seq.Cons ((Delim delim as wd), seq'), _, [] 
            when is_end_delim delim ->
            aux seq' (wd :: acc) [] ()
      | Seq.Cons (Delim delim, seq'), _, [ld]
            when is_end_delim delim 
              && StrMap.find ld s_to_e_map = delim ->
        Seq.Cons (
              Paren_block (
                ld, delim, 
                (acc |> List.rev |> List.tl |> List.map extract_text |> String.concat "")
              ),
              aux seq' [] []
            )
          | Seq.Cons ((Delim delim as wd), seq'), _, d::ds
            when is_end_delim delim 
              && StrMap.find d s_to_e_map = delim ->
            aux seq' (wd :: acc) ds ()
          | Seq.Cons (Text _ as t, seq'), _, _ -> 
            aux seq' (t :: acc) delim_stack ()
          | Seq.Cons (Delim delim, seq'), _, _ -> failwith "This shouldn't happen!"
        in
        aux chunks [] []   ;;  


end ;;


(************************************************************************************************************************
Snippet 131 : add numbering to an interval of lines in a file 
************************************************************************************************************************)
module Snip131=struct


  let ap1 = Absolute_path.of_string (
    home ^ "/Downloads/souplex.txt");;
  
  let text1 = Io.read_whole_file ap1 ;; 
  
  let lines1 = Lines_in_text.indexed_lines text1 ;; 
  
  let (lines2,lines3) = List.partition (
    fun (idx,line) -> idx <= 56 
  ) lines1 ;;
  
  let lines2_v2 = Image.image (
    fun (idx,line) -> 
      let s_idx = string_of_int idx in 
      let t_idx = Strung.insert_repetitive_offset_on_the_left
        '0' 2 s_idx in 
      (idx,"   "^t_idx^". "^(Cull_string.trim_spaces_on_the_left line))
  ) lines2;;
  
  let lines1_v2 = Image.image snd (lines2_v2@lines3) ;; 
  
  let text2 = String.concat "\n" lines1_v2 ;; 
  
  Io.overwrite_with ap1 text2 ;;


end ;;


(************************************************************************************************************************
Snippet 130 : Simple copy/replace in a file
************************************************************************************************************************)
module Snip130=struct

  let ap1 = Absolute_path.of_string (
    home ^ "/Teuliou/html_files/Translations/ws8_translated.txt");;
  
  Replace_inside.replace_inside_file 
    ("oe","œ") ap1;;  


end ;;


(************************************************************************************************************************
Snippet 129 : Compare two filecontents
************************************************************************************************************************)
module Snip129=struct


  let u1 = Io.read_whole_file 
  (Absolute_path.of_string
  (home^"/Teuliou/OCaml/Githubbed_ocaml/lib/Cee_language/cee_text.ml"));;
  
  let u2 = Io.read_whole_file 
  (Absolute_path.of_string
  (home^"/Teuliou/OCaml/skeptical_duck/lib/Cee_language/cee_text.ml"));;
  
  let all_lines1 = Lines_in_text.lines u1 ;;
  
  let all_lines2 = Lines_in_text.lines u2 ;;
  
  let m = min (List.length all_lines1) (List.length all_lines2) ;;
  
  let lines1 = List_again.long_head m all_lines1;;
  
  let lines2 = List_again.long_head m all_lines2;;
  
  let lines_from_both =
       Int_range.index_everything (
         List.combine lines1 lines2
       );;
  
  let different_lines = List.filter 
    (fun (idx,(x,y))->x<>y)
   lines_from_both ;;
  
  

end ;;


(************************************************************************************************************************
Snippet 128 : Generate a random string
************************************************************************************************************************)
module Snip128=struct

  let full_list = 
     ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 
      'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 
      'u'; 'v'; 'w'; 'x'; 'y'; 'z';
      'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 
      'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 
      'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'
      ] ;; 

  let main_list =['a';'e';'i';'o';'u';'y';'A';'E';'I';'O';'U';'Y'];;

  let main_list = 
     [ 'b'; 'c'; 'd';  'f'; 'g'; 'h';  'j'; 
      'k'; 'l'; 'm'; 'n';  'p'; 'q'; 'r'; 's'; 't'; 
       'v'; 'w'; 'x';  'z';
       'B'; 'C'; 'D'; 'F'; 'G'; 'H';  'J'; 
      'K'; 'L'; 'M'; 'N';  'P'; 'Q'; 'R'; 'S'; 'T'; 
       'V'; 'W'; 'X';  'Z'
      ] ;; 

  let n1 = List.length main_list ;;

  let g1 = Int_range.scale (fun x->List.nth main_list (Random.int n1)) 1 20 ;;
  let g2 = String.concat "" (Image.image (String.make 1) g1) ;;
  let n = List.length main_list ;;
  
  Int_range.scale (fun x->((x*x) mod n1)+1) 1 100;;


end ;;


(************************************************************************************************************************
Snippet 127 : Example with an "immutable" Hashtbl field
************************************************************************************************************************)
module Snip127=struct


  type foo = { bar: (int,int) Hashtbl.t} ;; 

  let foo_ref = ref({ bar = Hashtbl.create 1}) ;;
  
  let modify fr = Hashtbl.add (!fr).bar 1 47 ;;
  
  modify foo_ref ;; 
  
  let check = Hashtbl.find_opt (!foo_ref).bar 1 ;;


end ;;


(************************************************************************************************************************
Snippet 126 : Question on integer sequences whose inverse is convex
************************************************************************************************************************)
module Snip126=struct


let unequal_floor x y =
  (* we assume x,y > 0*)
  let r = x mod y in 
  if r = 0
  then (x/y)-1
  else (x-r)/y ;;  


let ff a1 a2 =
  if List.mem a1 [0;-1] then a1 else 
  if List.mem a2 [0;-1] then a2 else  
  if a2>=2*a1 then (-1)  else
  if a2<2 then 0 else 
  unequal_floor (a1*a2) (2*a1-a2) ;;  

let rec helper_for_rubinson_chain (treated,a1,a2,number_of_iterations) =
  if (number_of_iterations < 1)||(List.mem a2 [0;-1]) 
  then List.rev(a1::treated)
  else let a3 = ff a1 a2 in 
        helper_for_rubinson_chain (a1::treated,a2,a3,number_of_iterations-1) ;;   

let rubinson_chain a1 a2 = 
  helper_for_rubinson_chain ([],a1,a2,60) ;;

type end_point = 
  Negative 
  | Positive_but_too_small 
  | Once_down_always_down ;;

type period = Period of int ;;

type rubinson_result = 
   Eventually_arithmetic of int * (int * int) * period
  |Stalls of int * end_point ;;

let rubinson_pattern_opt a1 a2 = 
  let d = (a2-a1) in 
  if d<=0 then None else 
  if a1>=d*(2*d+1)  then Some(Period(d)) else 
  None ;;  

exception Helper_for_rubinson_measure_exn ;;

let rec helper_for_rubinson_measure (a1,a2,m) = 
  if m>1000
  then raise Helper_for_rubinson_measure_exn
  else 
  if a2 = (-1) then Stalls(m,Negative) else
  if a2 = 0  then Stalls(m,Positive_but_too_small) else   
  if a2 <= a1 then Stalls(m,Once_down_always_down) else       
  match rubinson_pattern_opt a1 a2 with 
  (Some p) -> Eventually_arithmetic(m,(a1,a2),p)
  | None ->
  let a3 = ff a1 a2 in 
  helper_for_rubinson_measure (a2,a3,m+1) ;;

exception Rubinson_measure_exn of int * int ;;

let rubinson_measure = Memoized.make(fun (a1,a2)->
  if a1 = (-1) then Stalls(1,Negative) else
  if a1 = 0 then Stalls(1,Positive_but_too_small) else 
  if a2 <= a1 then Stalls(1,Once_down_always_down) else       
   try helper_for_rubinson_measure (a1,a2,2) with 
   Helper_for_rubinson_measure_exn -> 
    raise(Rubinson_measure_exn(a1,a2))
) ;;


let rec helper_for_rubinson_ground (n,(x1,y1,dy1),(x2,y2,dy2)) = 
  if y2>n then x1 else 
    helper_for_rubinson_ground (n,(x2,y2,dy2),(x2+1,y2+dy2,dy2+4)) ;;

let rubinson_ground = Memoized.make(fun n ->
   helper_for_rubinson_ground (n,(0,0,3),(1,3,7)) 
);;

let rubinson_image = Memoized.make(fun 
  a1 -> Int_range.scale (fun a2 -> (a2,rubinson_measure (a1,a2)))
    (a1+(rubinson_ground a1)+1) (2*a1-1)
) ;;

let rubinson_max a1 = 
  let temp1 = rubinson_image a1 in 
  let temp2 = List.filter_map (
      fun (a2,res) -> match res with 
      Eventually_arithmetic(_,_,_) -> None
      |Stalls(m,_) -> Some(a2,m)
  ) temp1 in 
  let (m,sols) = Max.maximize_it_with_care snd temp2 in 
  (m,Image.image fst sols);;

  let rm = rubinson_max ;;

let detailed_ff a1 a2 =
    let b1 = a1 *a2 and b2 = 2*a1 - a2 in 
    let g = Gcd.gcd b1 b2 in 
    let c1 = b1/g and c2 = b2/g in 
    (a1,a2,(c1,c2),ff a1 a2,c1-c2*(ff a1 a2)) ;;

let extract_upper_bound l =  
  if l=[] then 0 else snd(List.hd(l)) ;;

let lower_bound = Memoized.make(fun a1 -> a1+rubinson_ground(a1)+1);;

let range0 a1 = Int_range.range (lower_bound a1) (2*a1-1) ;;

let rme a1 =
   Max.maximize_it_with_care 
   (fun a2 -> List.length(rubinson_chain a1 a2)) (range0 a1) ;;

let upper_bound1 = Memoized.make(fun a1 ->
  match List.assoc_opt a1 [3,0] with 
  Some answer -> answer
  | None ->
      if a1 mod 2 = 0 then (3*a1)/2 else (3*a1-1)/2 
);;

let range1 a1 = Int_range.range (lower_bound a1) (upper_bound1 a1) ;;


let upper_bound2 = Memoized.make(fun a1 ->
  if a1<=5 then 0 else
  match List.assoc_opt a1 [] with 
  Some answer -> answer
  | None ->(4*a1-(a1 mod 3))/3 
);;


let range2 a1 = Int_range.range (lower_bound a1) (upper_bound2 a1) ;;

let upper_bound3 = Memoized.make(fun a1 ->
  if (a1<=6)||(List.mem a1 [10;11]) then 0 else
  match List.assoc_opt a1 [] with 
  Some answer -> answer
  | None ->
    let r = a1 mod 12 in 
    let s =(
       if r<=2 then (-r) else 
       if r<=6 then  4-r else 
       8-r
    ) in 
    (5*a1+s)/4 
);;

let range3 a1 = Int_range.range (lower_bound a1) (upper_bound3 a1) ;;

let upper_bound4 = Memoized.make(fun a1 ->
  if (a1<=6)||(List.mem a1 [10;11]) then 0 else
  match List.assoc_opt a1 [] with 
  Some answer -> answer
  | None ->
    let r = a1 mod 60 in 
    let s =(
       if r<=1 then (-r) else 
       if r<=9 then  5-r else 
       if r<=11 then 10-r else  
       if r<=19 then 15-r else
       if r<=21 then 20-r else
       if r<=29 then 25-r else    
       8-r
    ) in 
    (5*a1+s)/4 
);;

let range4 a1 = Int_range.range (lower_bound a1) (upper_bound4 a1) ;;


let cc0 = Memoized.make(fun a1 ->
  let temp1 = List.filter (fun a2->
    List.mem (ff a1 a2) (range0 a2)
  )(range0 a1) in 
  Arithmetic_list.decompose_into_connected_components temp1
);;

let cc1 = Memoized.make(fun a1 ->
  let temp1 = List.filter (fun a2->
    List.mem (ff a1 a2) (range1 a2)
  )(range0 a1) in 
  Arithmetic_list.decompose_into_connected_components temp1
);;

let cc2 = Memoized.make(fun a1 ->
  let temp1 = List.filter (fun a2->
    List.mem (ff a1 a2) (range2 a2)
  )(range0 a1) in 
  Arithmetic_list.decompose_into_connected_components temp1
);;

let cc3 = Memoized.make(fun a1 ->
  let temp1 = List.filter (fun a2->
    List.mem (ff a1 a2) (range3 a2)
  )(range0 a1) in 
  Arithmetic_list.decompose_into_connected_components temp1
);;

let computed_upper_bound a1 = extract_upper_bound(cc3 a1) ;;

let u1 = Int_range.scale computed_upper_bound 2 1000 ;;
let u2 = Arithmetic_list.delta u1 ;;
let u3 = Int_range.scale (fun x->(x,computed_upper_bound x)) 2 100 ;;
let u4 = List.filter (fun (x,y)->y<>upper_bound3 x) u3 ;;
let u5 = Int_range.scale (fun r->
  let n = 600 + r in
  (r,5*computed_upper_bound(n)-6*n)) 0 59 ;;



(*  

#use"watched/watched_not_githubbed/jug.ml";;



let u1 = Int_range.scale (fun q->(ff (4*q) (5*q)) ) 1 100 ;;
let u2 = Arithmetic_list.delta u1 ;;

let peggy1 = Memoized.make(fun a1 ->
  let temp1 = List.filter (fun a2->
    3 * a1 * a2 < (4*a2 +3 ) * (2 *a1 -a2) 
  )(Int_range.range (a1+1) (bound1 a1)) in 
  Arithmetic_list.decompose_into_connected_components temp1
);;

let peggy2 a1 = snd(List.hd(peggy1 a1)) ;;

let see1 = Int_range.scale peggy2 3 50 ;;

let see2 = Arithmetic_list.delta see1 ;;

let see3 = Int_range.scale (fun a1->4*(peggy2 a1)-5*a1) 3 50 ;;


let peggy1 = Memoized.make(fun a1 ->
  let temp1 = List.filter (fun a2->
    2 * a1 * a2 < (3*a2 +2 ) * (2 *a1 -a2) 
  )(Int_range.range (a1+1) (bound1 a1)) in 
  Arithmetic_list.decompose_into_connected_components temp1
);;

let peggy2 a1 = snd(List.hd(peggy1 a1)) ;;

let see1 = Int_range.scale peggy2 3 50 ;;

let see2 = Arithmetic_list.delta see1 ;;

let see3 = Int_range.scale (fun a1->3*(peggy2 a1)-4*a1) 3 50 ;;


let bound1 a1 = Basic.frac_ceiling (3*a1-1) 2 ;; 

let peggy1 = Memoized.make(fun a1 ->
  let temp1 = List.filter (fun a2->
    2 * a1 * a2 < (3*a2 +1 ) * (2 *a1 -a2) 
  )(Int_range.range (a1+1) (bound1 a1)) in 
  Arithmetic_list.decompose_into_connected_components temp1
);;

let peggy2 a1 = snd(List.hd(peggy1 a1)) ;;

let see1 = Int_range.scale peggy2 3 50 ;;

let see2 = Arithmetic_list.delta see1 ;;

let see3 = Int_range.scale (fun a1->3*(peggy2 a1)-4*a1) 3 50 ;;


let g1 = rubinson_chain 40 45 ;;
let g2 = List_again.universal_delta_list g1 ;; 
let g3 = Image.image (fun (a1,a2) -> 2*a1 - a2) g2;;

let peggy1 = Memoized.make(fun d ->
   let a1 = d * (2*d +1) in 
   List.filter_map (fun a2->
      let l = rubinson_chain a1 a2 in 
      if List.length(l)<>4 then None else 
      let a3 = List.nth l 2 in 
      Some(l,2*a1-a2,2*a2-a3)  
   )(Int_range.range (a1+d+1) (a1+4*d+2)) 
);;


List.filter (fun n->snd(rm n)<>[n+rubinson_ground(n)+1]) (Int_range.range 2 50);;

let hh n = rubinson_chain n (n+rubinson_ground(n)+1) ;;

let pp n = 
  let temp1 = hh n in 
  let temp2 = List_again.universal_delta_list temp1 in  
  let temp3 = Image.image (fun (a1,a2) -> 2*a1 - a2) temp2 in 
  (temp1,temp3,dl temp3)  ;;

  
  (temp1,dl temp1)




let upper_bound a1 = 
  let q=(a1/2) and r=(a1 mod 2) in 
  3*q+r ;;

let cm a1 a2 = let d = a2-a1 in d*(2*d+1)-a1 ;;  
let gg a1 = 
   let a2 = 2*a1 - 5 in 
   let a3 = ff a1 a2 in 
  (a1,a2,a3,[cm a1 a2,cm a2 a3]) ;;


let aa d = ff (2*d*d+d) (2*d*d+2*d+2) ;;

Int_range.scale aa 20 23;;

List.filter (fun d->(aa d)<>2*d*d+3*d+5) (Int_range.range 1 50);;

Int_range.scale (fun d->(d,2*d*d+2*d+2)) 0 7;;
let see d = [2*d*d+d;2*d*d+2*d+1] ;;

let finder1 a1 = List.filter (fun a2 ->
    let a3 = ff a1 a2 in 
    let a4 = ff a2 a3 in 
    a4<(3*a3-3*a2+a1)
 )(Int_range.range (a1+1) (upper_bound a1)) ;;

let finder2 a1 = List.filter (fun a2 ->
  let a3 = ff a1 a2 in 
  let a4 = ff a2 a3 in 
  a2*a3-(2*a2-a3)<(3*a3-3*a2+a1)*(2*a2-a3)
)(Int_range.range (a1+1) (upper_bound a1)) ;;

let finder2 a1 = List.filter (fun a2 ->
  let a3 = ff a1 a2 in 
  let a4 = ff a2 a3 in 
  a2*a3-(2*a2-a3)<(3*a3-3*a2+a1)*(2*a2-a3)
)(Int_range.range (a1+1) (upper_bound a1)) ;;

let finders = List.filter (fun a1->finder(a1)<>[]) (Int_range.range 2 50);;

let ub a1 = List.hd(List.rev(finder a1) ) ;;

let u1 = Int_range.scale ub 2 50 ;;
let u2 = Arithmetic_list.delta u1 ;;

let u3 = List.filter (fun a1->ub(a1)<>old_upper_bound(a1)) (Int_range.range 2 50);;


let peggy1 a1 = Image.image(fun a2 ->
   let a3 = ff a1 a2 in 
   a3-2*a2+a1
)(Int_range.range (a1+1) (upper_bound a1)) ;;

let peggy2 a1 = Ordered.sort Total_ordering.for_integers (peggy1 a1) ;;

let peggy3 a1 = List.filter(fun a2 ->
  let a3 = ff a1 a2 in 
  a3-2*a2+a1 = 1
)(Int_range.range (a1+1) (upper_bound a1)) ;;




let rm = rubinson_max ;;

let gg x = rubinson_measure (x,x+rubinson_ground(x)+1) ;;

let hh x = rubinson_chain x (x+rubinson_ground(x)+1) ;;

let dl = Arithmetic_list.delta ;;

let check = List.filter (fun n->
   snd(rm n)<>[n+rubinson_ground(n)+1]
  ) (Int_range.range 2 50) ;;

let uu d = 
  let a1=2*d*d+d 
  and a2=2*d*d+2*d+1 in 
  (a1,a2,ff a1 a2) ;;

let uu d = 
  let a1=2*d*d+2*d+1 
  and a2=2*d*d+3*d+3 in 
  (a1,a2,ff a1 a2) ;;  

let uu d = 
  let a1=2*d*d+3*d+3 
  and a2=2*d*d+3*d+3 in 
  (a1,a2,ff a1 a2) ;; 


Int_range.scale (fun n->let (_,_,r)=uu n in r) 20 23 ;;  

let check1 = List.filter (fun d->
   ff (2*d*d+d) (2*d*d+2*d+1)<> (2*d*d+3*d+3)
  ) (Int_range.range 1 50) ;;

let check2 = List.filter (fun d->
   ff (2*d*d+2*d+1) (2*d*d+3*d+3) <> (2*d*d+4*d+6) 
  ) (Int_range.range 1 50) ;;

let vv a b = 
   let temp1 = 


*)



end ;;


(************************************************************************************************************************
Snippet 125 : Code to start analizing a snapshot of php-src
************************************************************************************************************************)
module Snip125=struct

List.iter (fun v->Unix.putenv v "" ) [
   "EXTRA_CFLAGS";"LDFLAGS"; 
   "INSTALL_ROOT"; "OPCACHE_SHARED_DEPENDENCIES"; "PROF_FLAGS"
] ;; 

let ap1= Absolute_path.of_string "~/Teuliou/Experimenting_with_php/copiableMakefile";;

let makefile_ref  =  ref(Makefile.parse(Makefile_t.MT(Io.read_whole_file ap1))) ;; 

let prerequisites_and_commands_for_target = Makefile.prerequisites_and_commands_for_target makefile_ref ;;

let prerequisites_for_target = Makefile.prerequisites_for_target makefile_ref ;;

let list_value = Makefile.list_value (!makefile_ref) ;;

let beg_marker = "# Wild target starts here" ;;
let end_marker = "# Wild target ends here" ;;
 
    
let commands_ref = ref ([]: (string * string ) list ) ;;



let act () = 
  let snippet = Makefile.write_rule_without_prerequisites
     ~target_name:"all" ~commands:(!commands_ref) in 
  let _ = Replace_inside.overwrite_between_markers_inside_file
     ~overwriter:snippet (beg_marker,end_marker) ap1 in 
  makefile_ref :=  (Makefile.parse(Makefile_t.MT(Io.read_whole_file ap1)))  ;; 


  let cr0 = 
   [
      ("make $(phplibdir)/opcache.la","");
      ("make $(SAPI_CLI_PATH)","");
    ] ;; 
  
  commands_ref:= cr0 ;;


let see1 = prerequisites_for_target "ext/opcache/opcache.la" ;; 

let see2 = prerequisites_for_target "$(SAPI_CLI_PATH)";;

let part1 = list_value ~variable_name:"shared_objects_opcache" ;; 

let part2 = list_value ~variable_name:"PHP_GLOBAL_OBJS";;

let part3 = list_value ~variable_name:"PHP_BINARY_OBJS";;

let part4 = list_value ~variable_name:"PHP_CLI_OBJS";;

let last_in_part1 = List.hd (List.rev part1) ;;

let unenhanced = part1@["ext/opcache/opcache.la"]@part2@part3@part4@["$(SAPI_CLI_PATH)"] ;; 

let enhanced = Makefile.enhance_target_list makefile_ref unenhanced ;;

let cr1 = Image.image (fun tgt->("make "^tgt,"")) enhanced ;;

commands_ref:= cr1 ;;

let pairs_for_enhanced_ones = 
   List.filter_map (
     fun tgt -> 
      let cmds = snd(prerequisites_and_commands_for_target tgt) in 
      if cmds = [] then None else
      Some(tgt,snd(prerequisites_and_commands_for_target tgt))
   ) enhanced ;;

let check_pairs = List.filter (fun (tgt,l)->List.length(l)<>1) pairs_for_enhanced_ones ;;

let list2 = List.flatten(Image.image (fun (tgt,l_cmd)->
   Image.image (Replace_inside.replace_inside_text ("$@"," "^tgt)) l_cmd    
) pairs_for_enhanced_ones) ;;

let indexed_list2 = Int_range.index_everything list2 ;;

(* let rec helper (redundancies,scanned,to_be_scanned) = match to_be_scanned with 
 [] -> List.rev redundancies 
 | (j,l_j) :: others ->
    match List.find_opt (fun (i,l_i)->l_i = l_j) scanned with 
    (Some(i,_)) -> helper ((i,j)::redundancies,scanned,others) 
    | None -> helper (redundancies,(j,l_j)::scanned,others)  ;;

let find_redundancies indexed_l = helper ([],[],indexed_l) ;;

let res1 = find_redundancies indexed_list2 ;; 
let redundant_indices = Image.image snd res1 ;; *)

let redundant_indices = [338] ;;

let list3 = List.filter_map ( 
   fun (idx,cmd) -> if List.mem idx redundant_indices then None else Some cmd
) indexed_list2 ;;

let build_cli = String.concat " " (Makefile.list_value (!makefile_ref) ~variable_name:"BUILD_CLI") ;;

let list4 = Image.image (fun cmd->if cmd="$(BUILD_CLI)" then build_cli else cmd) list3 ;;


commands_ref := ( Image.image (fun cmd->(cmd,"")) list4)  ;;


let cmd1 = "$(BUILD_CC) -D${IR_TARGET} -DIR_PHP -DIR_PHP_MM=0 -o  ext/opcache/jit/ir/gen_ir_fold_hash /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/ext/opcache/jit/ir/gen_ir_fold_hash.c" ;;
let cmd2 = "ext/opcache/jit/ir/gen_ir_fold_hash < /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/ext/opcache/jit/ir/ir_fold.h > ext/opcache/jit/ir/ir_fold_hash.h" ;;

let list5 = List.filter_map (
   fun cmd ->
       if cmd = cmd1 then None else 
       if cmd = cmd2 then Some "cp ${EXPHP}/reservation-php-src/ir_fold_hash.h ${PHPSRC}/ext/opcache/jit/ir/"  else
       Some cmd
) list4 ;; 

let cmd3 = "$(BUILD_CC) /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/ext/opcache/jit/ir/dynasm/minilua.c -lm -o  ext/opcache/jit/ir/minilua" ;;
let cmd4 = "ext/opcache/jit/ir/minilua /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/ext/opcache/jit/ir/dynasm/dynasm.lua  $(DASM_FLAGS) -o  ext/opcache/jit/ir/ir_emit_$(DASM_ARCH).h /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/ext/opcache/jit/ir/ir_$(DASM_ARCH).dasc" ;;

let list6 = List.filter_map (
   fun cmd ->
       if cmd = cmd3 then Some "cp ${EXPHP}/reservation-php-src/ir_emit_x86.h ${PHPSRC}/ext/opcache/jit/ir/" else 
       if cmd = cmd4 then Some "cp ${EXPHP}/reservation-php-src/ir_x86.dasc   ${PHPSRC}/ext/opcache/jit/ir/"  else
       Some cmd
) list5 ;; 

let cmd5 = "@$(YACC) $(YFLAGS) --defines -l /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/ext/json/json_parser.y -o /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/ext/json/json_parser.tab.c" ;;

let list7 = List.flatten(List.filter_map (
   fun cmd ->
       if cmd = cmd5 then Some 
         [
           "cp ${EXPHP}/reservation-php-src/json_parser.tab.h ${PHPSRC}/ext/json/";
           "cp ${EXPHP}/reservation-php-src/json_parser.tab.c ${PHPSRC}/ext/json/";
         ]
      else 
       Some [cmd]
) list6 );; 

let cmd6 = "@$(RE2C) $(RE2C_FLAGS) -t /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/ext/json/php_json_scanner_defs.h -bci -o /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/ext/json/json_scanner.c /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/ext/json/json_scanner.re" ;;

let list8 = List.flatten(List.filter_map (
   fun cmd ->
       if cmd = cmd6 then Some 
         [
           "cp ${EXPHP}/reservation-php-src/php_json_scanner_defs.h ${PHPSRC}/ext/json/";
           "cp ${EXPHP}/reservation-php-src/json_scanner.c ${PHPSRC}/ext/json/";
         ]
      else 
       Some [cmd]
) list7 );; 

let cmd7 = "@(cd $(top_srcdir);  \tif test -f ./pdo_sql_parser.re; then  \t\t$(RE2C) $(RE2C_FLAGS) -o pdo_sql_parser.c pdo_sql_parser.re;  \telse  \t\t$(RE2C) $(RE2C_FLAGS) -o ext/pdo/pdo_sql_parser.c ext/pdo/pdo_sql_parser.re;  \tfi)" ;;


let list9 = List.flatten(List.filter_map (
   fun cmd ->
       if cmd = cmd7 then Some 
         [
           "cp ${EXPHP}/reservation-php-src/pdo_sql_parser.h ${PHPSRC}/ext/pdo/";
           "cp ${EXPHP}/reservation-php-src/pdo_sql_parser.c ${PHPSRC}/ext/pdo/";
         ]
      else 
       Some [cmd]
) list8 );; 

let cmd8 = "@(cd $(top_srcdir);  \tif test -f ./sqlite_sql_parser.re; then  \t\t$(RE2C) $(RE2C_FLAGS) -o sqlite_sql_parser.c sqlite_sql_parser.re;  \telse  \t\t$(RE2C) $(RE2C_FLAGS) -o ext/pdo_sqlite/sqlite_sql_parser.c ext/pdo_sqlite/sqlite_sql_parser.re;  \tfi)";;

let list10 = List.flatten(List.filter_map (
   fun cmd ->
       if cmd = cmd8 then Some 
         [
           "cp ${EXPHP}/reservation-php-src/sqlite_sql_parser.c ${PHPSRC}/ext/pdo_sqlite/";
         ]
      else 
       Some [cmd]
) list9 );; 

let cmd9 =  "@(cd $(top_srcdir);  \tif test -f ./php_phar.h; then  \t\t$(RE2C) $(RE2C_FLAGS) -b -o phar_path_check.c phar_path_check.re;  \telse  \t\t$(RE2C) $(RE2C_FLAGS) -b -o ext/phar/phar_path_check.c ext/phar/phar_path_check.re;  \tfi)";
;;
let list11 = List.flatten(List.filter_map (
   fun cmd ->
       if cmd = cmd9 then Some 
         [
           "cp ${EXPHP}/reservation-php-src/phar_path_check.c ${PHPSRC}/ext/phar/";
         ]
      else 
       Some [cmd]
) list10 );; 


let cmd10 = "@$(YACC) $(YFLAGS) -v -d /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/Zend/zend_language_parser.y -o  /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/Zend/zend_language_parser.c" ;;

let cmd11 = 
   "@$(SED) -e 's,^int zendparse\\(.*\\),ZEND_API int zendparse\\1,g' <  /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/Zend/zend_language_parser.c  \t>  /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/Zend/zend_language_parser.c.tmp &&  " ^
   "\tmv  /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/Zend/zend_language_parser.c.tmp  /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/Zend/zend_language_parser.c"
 ;;

let cmd12 = 
   "@$(SED) -e 's,^int zendparse\\(.*\\),ZEND_API int zendparse\\1,g' < /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/Zend/zend_language_parser.h  \t> /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/Zend/zend_language_parser.h.tmp &&  \tm" ^
   "v /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/Zend/zend_language_parser.h.tmp /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/Zend/zend_language_parser.h"

;;

let list12 = List.flatten(List.filter_map (
   fun cmd ->
       if cmd = cmd10 then Some 
         [
          "cp ${EXPHP}/reservation-php-src/zend_language_parser.h ${PHPSRC}/Zend/"; 
          "cp ${EXPHP}/reservation-php-src/zend_language_parser.c ${PHPSRC}/Zend/";
         ]
      else 
      if List.mem cmd [cmd11;cmd12] then None else   
       Some [cmd]
) list11 );; 

let cmd13 = 
   "@(cd $(top_srcdir); $(RE2C) $(RE2C_FLAGS) -b -o ext/standard/url_scanner_ex.c\text/standard/url_scanner_ex.re)"
;;

let list13 = List.flatten(List.filter_map (
   fun cmd ->
       if cmd = cmd13 then Some 
         [
          "cp ${EXPHP}/reservation-php-src/url_scanner_ex.h ${PHPSRC}/ext/standard/"; 
          "cp ${EXPHP}/reservation-php-src/url_scanner_ex.c ${PHPSRC}/ext/standard/";
         ]
      else  
       Some [cmd]
) list12 );; 



let cmd14 = 
   "@(cd $(top_srcdir); $(RE2C) $(RE2C_FLAGS) -b -o ext/standard/var_unserializer.c ext/standard/var_unserializer.re)"
;;

let list14 = List.flatten(List.filter_map (
   fun cmd ->
       if cmd = cmd14 then Some 
         [
          "cp ${EXPHP}/reservation-php-src/var_unserializer.c ${PHPSRC}/ext/standard/";
         ]
      else  
       Some [cmd]
) list13 );; 

let cmd15 = 
   "@(cd $(top_srcdir); $(RE2C) $(RE2C_FLAGS) --case-inverted -cbdFt Zend/zend_language_scanner_defs.h -oZend/zend_language_scanner.c Zend/zend_language_scanner.l)"
;;

let list15 = List.flatten(List.filter_map (
   fun cmd ->
       if cmd = cmd15 then Some 
         [
          "cp ${EXPHP}/reservation-php-src/zend_language_scanner_defs.h ${PHPSRC}/Zend/"; 
          "cp ${EXPHP}/reservation-php-src/zend_language_scanner.h ${PHPSRC}/Zend/"; 
          "cp ${EXPHP}/reservation-php-src/zend_language_scanner.c ${PHPSRC}/Zend/";
         ]
      else 
       Some [cmd]
) list14 );; 



let cmd16 = "@if test ! -z \"$(PHP)\"; then  \t\t$(PHP) /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/ext/tokenizer/tokenizer_data_gen.php;  \tfi;";;
let cmd17 = "@if test ! -z \"$(PHP)\"; then  \t\t$(PHP) /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/scripts/gdb/debug_gdb_scripts_gen.php;  \tfi;";;
let cmd18 = "@if test ! -z \"$(PHP)\"; then  \t\t$(PHP) /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/Zend/zend_vm_gen.php;  \tfi;";;

let list16 = List.flatten(List.filter_map (
   fun cmd ->
       if List.mem cmd [cmd16;cmd17;cmd18] 
       then None 
       else Some [cmd]
) list15 );; 


let cmd19 = 
   "@$(YACC) $(YFLAGS) -v -d /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/Zend/zend_ini_parser.y -o  /home/ewandelanoy/Teuliou/Experimenting_with_php/php-src/Zend/zend_ini_parser.c"
;;

let list17 = List.flatten(List.filter_map (
   fun cmd ->
       if cmd = cmd19 then Some 
         [
          "cp ${EXPHP}/reservation-php-src/zend_ini_parser.h ${PHPSRC}/Zend/"; 
          "cp ${EXPHP}/reservation-php-src/zend_ini_parser.c ${PHPSRC}/Zend/";
         ]
      else 
       Some [cmd]
) list16 );; 

let cmd15 = 
   "@(cd $(top_srcdir); $(RE2C) $(RE2C_FLAGS) --case-inverted -cbdFt Zend/zend_ini_scanner_defs.h -oZend/zend_ini_scanner.c Zend/zend_ini_scanner.l)" 
;;


let list18 = List.flatten(List.filter_map (
   fun cmd ->
       if cmd = cmd15 then Some 
         [
          "cp ${EXPHP}/reservation-php-src/zend_ini_scanner_defs.h ${PHPSRC}/Zend/"; 
          "cp ${EXPHP}/reservation-php-src/zend_ini_scanner.h ${PHPSRC}/Zend/"; 
          "cp ${EXPHP}/reservation-php-src/zend_ini_scanner.c ${PHPSRC}/Zend/";
         ]
      else 
       Some [cmd]
) list17 );; 


let list19 = List.filter (fun cmd -> String.starts_with ~prefix:"$(LIBTOOL)" cmd ) list18 ;; 


let old_val_for_libtool = Makefile.single_value (!makefile_ref) ~variable_name:"LIBTOOL" ;;

let new_val_for_libtool = Replace_inside.replace_inside_text ("--silent","--dry-run") old_val_for_libtool ;;

let list20 = Image.image (
   fun (idx,cmd) ->
      let cmd1 = Replace_inside.replace_inside_text ("$(LIBTOOL)",new_val_for_libtool) cmd 
      and s_idx = string_of_int idx in  
      String.concat "\n\t"
      [
       "@echo \"************************************************ Stap "^s_idx^":\" >> ${EXPHP}/what_libtool_did.txt";
        cmd1^" >> ${EXPHP}/what_libtool_did.txt";
        cmd
      ]
) (Int_range.index_everything list19) ;;


let ap2= Absolute_path.of_string "~/Teuliou/Experimenting_with_php/snapshot1_for_libtool.txt";;

let libtool_text  =  (Io.read_whole_file ap2)^"\n***" ;; 

let libtool_lines = Lines_in_text.indexed_lines libtool_text ;;
let table_for_star_index_to_line_index = 
   Int_range.index_everything (List.filter_map (fun 
   (line_idx,line) ->
      if String.starts_with line ~prefix:"*"
      then Some line_idx   
      else None   ) libtool_lines) ;;

let u1 = List_again.universal_delta_list table_for_star_index_to_line_index ;;
let number_of_libtool_lines = List.length libtool_lines ;;

let number_of_starred_lines = List.length table_for_star_index_to_line_index ;;

let compute_star_index_from_line_index line_idx = 
   if line_idx = number_of_libtool_lines then number_of_starred_lines else
   let ((star_idx,_),_) = List.find (fun ((star_idx1,line_idx1),(star_idx2,line_idx2)) ->
         (line_idx1<=line_idx) && (line_idx < line_idx2)
      ) u1 in 
   star_idx;;

let nonstarred_lines = List.filter_map (fun 
(line_idx,line) -> 
   if String.starts_with line ~prefix:"*"
   then None   
   else Some(compute_star_index_from_line_index line_idx,line)
) libtool_lines ;;

let towards1_pre_list21 = Int_range.scale (
  fun star_idx0 ->
    List.filter_map (fun (star_idx,line)->
      if star_idx = star_idx0
      then Some line 
      else None   
      ) nonstarred_lines
) 1 (number_of_starred_lines-1) ;;

let u2 = Image.image (fun l->List.partition(fun line->String.starts_with line ~prefix:"creating ") l ) towards1_pre_list21 ;;
let u3 = List.flatten(Image.image fst u2) ;;

let pre_list21 = Image.image snd u2 ;;
let list21 = Image.image (String.concat "\n\t") pre_list21 ;;

let u4 = Image.image (fun l->List.partition(fun line->String.starts_with line ~prefix:"mkdir ") l ) pre_list21 ;;
let u5 = List.flatten(Image.image fst u4) ;;

let u6 = Image.image (fun s->"mkdir -p ${PHPSRC}/"^(Cull_string.cobeginning 6 s)) u5;;

let u7 = "\n\n\n" ^ (String.concat "\n\t" (""::u6)) ^ "\n\n\n" ;;

let a1 () = print_string u7 ;;

let pre_list22 = Image.image snd u4 ;;
let list22 = Image.image (String.concat "\n\t") pre_list22 ;;

let root_dir = home ^ "/Teuliou/Experimenting_with_php/php-src/";;

let pre_list23 = Image.image (Image.image (Replace_inside.replace_inside_text
~display_number_of_matches:false (root_dir,"") )) pre_list22 ;;
let list23 = Image.image (String.concat "\n\t") pre_list23 ;;

let root_mention = "-I" ^ home ^ "/Teuliou/Experimenting_with_php/php-src";;

let pre_list24 = Image.image (Image.image (Replace_inside.replace_inside_text 
~display_number_of_matches:false (root_mention,"-I.") )) pre_list23 ;;
let list24 = Image.image (String.concat "\n\t") pre_list24 ;;


commands_ref := ( Image.image (fun cmd->(cmd,"")) list24)  ;;

let old_shared_production = List.hd(List.nth pre_list24 28);;

let new_shared_production = Replace_inside.replace_inside_text 
~display_number_of_matches:false ("/.libs/","/")  old_shared_production ;;

let pre_list25 = Image.image (fun l->if List.hd(l)=old_shared_production then [new_shared_production] else l ) pre_list24 ;;
let list25 = Image.image (String.concat "\n\t") pre_list25 ;;

let pre_list26 = Image.image (fun l->
   if List.length(l)<>2  then l else 
   if (List.nth l 1)=""  
   then [List.nth l 0] 
   else [List.nth l 1]) pre_list25 ;;
let list26 = Image.image (String.concat "\n\t") pre_list26 ;;

let pre_list27 = Image.image (Image.image (Replace_inside.replace_inside_text (" >/dev/null 2>&1","") )) pre_list26 ;;
let list27 = Image.image (String.concat "\n\t") pre_list27 ;;


let prepared_dir = (Sys.getenv "AHPSRC") ^ "/";;
let simplified_dir = (Sys.getenv "SHPSRC") ^ "/";;
let half_preprocessed_dir = (Sys.getenv "HAHPSRC") ^ "/";;

let to_be_removed =
   [
      "libs/"; "UPGRADING.INTERNALS"; "tests/"; "codecov.yml"; "build/";
      "CODING_STANDARDS.md";  "config.log"; "EXTENSIONS";
      "README.REDIST.BINS"; "php.ini-development"; "LICENSE"; "travis/";
      "SECURITY.md"; "modules/"; "buildconf.bat";  "UPGRADING";
      "libtool"; "CONTRIBUTING.md"; "win32/"; "php.ini-production"; 
      "Makefile.objects"; "autom4te.cache/"; "configure"; "config.nice";
      "buildconf";  "run-tests.php"; "NEWS"; "pear/"; "docs/";
      "README.md"; "docs-old/"; "benchmark/"; "Makefile.fragments"; "scripts/";
      "configure.ac"; "config.status"; 
   ] ;;
   
let commands_for_removal = Image.image (
   fun x -> "rm -rf "^simplified_dir^"/"^x
) to_be_removed ;;

let act2 () = Image.image Sys.command commands_for_removal ;; 
    
let check_list27 = List.filter (fun cmd ->
  not(Substring.is_a_substring_of " -o " cmd)   
) list27 ;;

let (usual_from_list27,extreme_from_list27) = List.partition (
   Substring.is_a_substring_of " -c "   
 ) list27 ;;

let rewrite_command cmd = 
   let temp1 = Str.split (Str.regexp "[ \t\r]+") cmd in 
   let temp2 = Image.image (
     fun s -> 
      if (String.starts_with s ~prefix:"-I")&&
          (not(String.ends_with s ~suffix:"/"))&&
          (s<>"-I.")
     then s^"/"
     else s 
   ) temp1 in 
   let temp3 = List_again.nonredundant_version temp2 in 
   let temp4 = Int_range.index_everything temp3 in 
   let i1 = fst(List.find (fun (idx,s)->s="-c") temp4)
   and i2 = fst(List.find (fun (idx,s)->s="-o") temp4) in 
   let temp5 = List.filter_map (fun (idx,s)->
      if List.mem idx [i1;i1+1;i2;i2+1] then None else Some s   
   ) temp4
   and src_file = List.assoc (i1+1) temp4
   and dest_file = List.assoc (i2+1) temp4 in 
   let temp6 = temp5 @ ["-c";src_file;"-o";dest_file] in 
   String.concat " " temp6 ;; 

let list28 = Image.image (
      fun cmd ->
         if Substring.is_a_substring_of " -c " cmd 
         then rewrite_command cmd 
        else cmd    
    ) list27 ;;

commands_ref := ( Image.image (fun cmd->(cmd,"")) list28)  ;;

let extract_core_and_filename cmd = 
   let i1 = Option.get(Substring.leftmost_index_of_in_from_opt " -c " cmd 1) in 
   let i2 = Option.get(Substring.leftmost_index_of_in_from_opt " " cmd (i1+4)) in
   (Cull_string.beginning (i1-1) cmd,Cull_string.interval cmd (i1+4) (i2-1));; 
let base1 = List.filter_map (
      fun cmd ->
         if Substring.is_a_substring_of " -c " cmd 
         then Some(extract_core_and_filename cmd) 
        else None  
) list28 ;;

let base2 = List.flatten(Image.image (
      fun (core,short_c_file) ->
     let short_h_file = (Cull_string.coending 2 short_c_file)^".h" in 
     if Sys.file_exists (simplified_dir ^ short_h_file) 
     then [(core,short_h_file);(core,short_c_file)]
     else [core,short_c_file]
) base1);;

let sub_list28 = List.filter(
   Substring.is_a_substring_of " -c "
) list28 ;;

(*
let config1 = Cee_project_transfer.make_capsule 
~source:(Directory_name.of_string simplified_dir)
~destination:(Directory_name.of_string half_preprocessed_dir)
 sub_list28 ;;


let act0 () = Cee_project_transfer.cleanup_temporary_files_after_removing_conditional_directives_in_directly_compiled_files config1  ;; 

let act1 () = Chronometer.it Cee_project_transfer.remove_conditional_directives_in_directly_compiled_files config1  ;; 

 
#use"watched/watched_not_githubbed/ham.ml";;

 let ap1 = Absolute_path.of_string (simplified_dir ^ "ext/opcache/zend_file_cache.c") ;;
let text0 = Io.read_whole_file ap1 ;;

let text1 = Lines_in_text.interval text0 71 106 ;;

let text2 = Cee_conditional_directives.watermark_text text1 ;;

print_string(text2);;

let text3 = List.assoc 28 (Lines_in_text.indexed_lines text2) ;;



print_string(Cee_conditional_directives.rewrite_using_watermarks text1 text3);;




let u2 = List_again.long_head 23 config1.Cee_conditional_directives.commands ;;

let act2 () = Chronometer.it (Cee_conditional_directives.remove_cds_in_files config1) u2 ;; 


let ic1 = List.nth (config1.Cee_conditional_directives.commands) 0 ;; 

let ic2 = List.nth (config1.Cee_conditional_directives.commands) 1 ;; 

let act1 () = Chronometer.it (Cee_conditional_directives.remove_cds_in_files config1) [ic1;ic2] ;; 

let u1 = Unix_again.quick_beheaded_complete_ls simplified_dir ;;  

let u2 = List.filter (fun x->List.exists(
   fun edg->String.ends_with x ~suffix:edg) [".h";".c"] ) u1 ;;

*)



end ;;


(************************************************************************************************************************
Snippet 124 : Code to write a long list of long strings in OCaml
************************************************************************************************************************)
module Snip124=struct

  let hun_decomposition s = 
    let n = String.length(s) in 
    let q = (n/100) in 
    Int_range.scale (
      fun j ->
        if j<=q 
        then Cull_string.interval s (100*(j-1)+1) (100*j)
        else Cull_string.interval s (100*q+1) n
    )  1 (q+1) ;;
  
  let hun_writing s =
     let temp1 = Int_range.index_everything (hun_decomposition s) in 
     let m = List.length temp1 in 
     let temp2 = Image.image (
       fun (idx,line) -> 
        let prefix = (if idx=1 then "(" else " ")
        and suffix = (if idx=m then ")" else "^") in 
        prefix ^ (Strung.enclose line) ^ suffix
     ) temp1 in 
     String.concat "\n" temp2 ;;
  
  let hun_string_list l =
     let temp1 = Image.image hun_writing l in 
    "\n\n\nlet ll = [\n"^
    (String.concat ";\n" temp1)^
    "\n] ;;\n\n\n" ;;
  
  let list_to_be_written = [] ;;  
  let haag3 = hun_string_list list_to_be_written ;;

    let ap1 = Absolute_path.of_string "watched/watched_not_githubbed/cloth.ml" ;;
    
    let write () = Replace_inside.overwrite_between_markers_inside_file
        ~overwriter:haag3 ("(" ^"*B*)","(*"^"E*)") ap1 ;; 

end ;;


(************************************************************************************************************************
Snippet 123 : Makefile code, not using the Makefile_text module
************************************************************************************************************************)
module Snip123=struct

  let ap1= Absolute_path.of_string "~/Teuliou/Experimenting_with_php/copiableMakefile";;

  let beg_marker = "# Wild target starts here" ;;
  let end_marker = "# Wild target ends here" ;;
  
  
  let makefile_lines_for_indexed_command (cmd_idx,(cmd_content,comment,is_commented_out)) = 
     let comment_box = (if is_commented_out then "#" else "") 
     and s_idx = string_of_int cmd_idx in 
     [
     comment_box^
     "\t@echo \"************************************************ Step "^s_idx^":"^comment^"\"";
     comment_box^
     "\t"^cmd_content   
     ] ;; 
  
  let makefile_snippet_for_commands l = 
    let indexed_l = Int_range.index_everything l in 
    "\nfalbala:\n" ^
    (String.concat "\n"
    (List.flatten (Image.image makefile_lines_for_indexed_command indexed_l ))) ^ "\n";;   
  
  
   
      
  let commands_ref = ref ([]: (string * string * bool) list ) ;;
  
  let makefile_ref  =  ref(Io.read_whole_file ap1) ;; 
  
  let act () = 
    let snippet = makefile_snippet_for_commands (!commands_ref) in 
    let _ = Replace_inside.overwrite_between_markers_inside_file
       ~overwriter:snippet (beg_marker,end_marker) ap1 in 
    makefile_ref :=  (Io.read_whole_file ap1)  ;; 
  
  
  
  let add_interval_if_nonempty (treated,text,starter,current_idx)=
    if starter > current_idx 
    then treated 
    else (Cull_string.interval text starter current_idx) :: treated ;;   
    
        
  
  let rec helper_for_ingredients_extractor (treated,text,starter,current_idx,text_length) = 
    if current_idx > text_length 
    then  (List.rev(add_interval_if_nonempty (treated,text,starter,text_length)),text_length + 1)
    else
    let c = Strung.get text current_idx in 
    if not(List.mem c [' ';'\t';'\r';'\n'])   
    then  helper_for_ingredients_extractor (treated,text,starter,current_idx+1,text_length) 
    else 
    let treated2 = add_interval_if_nonempty (treated,text,starter,current_idx-1) in     
    if c <> '\n'
    then  helper_for_ingredients_extractor (treated2,text,current_idx+1,current_idx+1,text_length) 
    else   
    if (Strung.get text (current_idx-1))<>'\\'
    then (List.rev treated2,current_idx+1)  
    else     
    let treated3 = add_interval_if_nonempty (treated,text,starter,current_idx-2) in     
    helper_for_ingredients_extractor (treated3,text,current_idx+1,current_idx+1,text_length) ;;
      
      
    let extract_ingredients text idx = 
        helper_for_ingredients_extractor ([],text,idx,idx,String.length text)  ;;
      
  (*  
    extract_ingredients ("123ab\\\n\tcdef \tghi\njk") 3;;
  *)
  
  let get_ingredients_for_target target_name = 
    let temp1 = Lines_in_text.indexed_lines (!makefile_ref) 
    and prefix = target_name^":" in 
    let (idx1,line1) = List.find (
          fun (_,line) -> String.starts_with ~prefix line 
    )  temp1 in 
    let idx2 = Option.get(Substring.leftmost_index_of_in_from_opt line1 (!makefile_ref) 1) in 
    let left_offset = idx2 + (String.length prefix) in 
    (idx1,extract_ingredients (!makefile_ref) left_offset) ;;
    
  let get_makefile_variable_list_value vname = 
    let temp1 = Lines_in_text.indexed_lines (!makefile_ref) 
    and prefix = vname^" = " in 
    let (idx1,line1) = List.find (
          fun (_,line) -> String.starts_with ~prefix line 
    )  temp1 in 
    let idx2 = Option.get(Substring.leftmost_index_of_in_from_opt line1 (!makefile_ref) 1) in 
    let left_offset = idx2 + (String.length prefix) in 
    (idx1,fst(extract_ingredients (!makefile_ref) left_offset)) ;; 
    
  let get_makefile_variable_single_value vname = 
     let (idx1,temp1) = get_makefile_variable_list_value vname in 
     (idx1,List.hd temp1) ;;
  
  
   let rec helper_for_recipes_extractor (treated,text,starter,current_idx,text_length) = 
       if current_idx > text_length 
       then  List.rev(add_interval_if_nonempty (treated,text,starter,text_length))
       else
       if ((Strung.get text current_idx)<>'\n')||(Strung.get text (current_idx-1)='\\')
       then  helper_for_recipes_extractor (treated,text,starter,current_idx+1,text_length) 
       else
       let treated2 = add_interval_if_nonempty (treated,text,starter,current_idx-1) in   
       if ( (Strung.get text (current_idx+1))<>'\t' ) || (current_idx+2 > text_length)
       then  List.rev treated2
       else 
       helper_for_recipes_extractor (treated2,text,current_idx+2,current_idx+2,text_length) ;;
     
   let extract_recipes text idx = 
       helper_for_recipes_extractor ([],text,idx,idx,String.length text)  ;;
     
     (*  
     extract_recipes ("123ab\n\tcdef\n\tghi\njk") 3;;
     *)
   
  let get_ingredients_and_recipes_for_target target_name = 
    let (_,(ingredients,frontier_idx)) = get_ingredients_for_target target_name in 
    (ingredients,extract_recipes (!makefile_ref) frontier_idx) ;;    
  
  let get_recipe_for_target target_name = 
    snd(get_ingredients_and_recipes_for_target target_name) ;;
  
  
  let (idx1,(list1,jdx1)) = get_ingredients_for_target "ext/opcache/opcache.la" ;; 
  
  let (idx2,list2) = get_makefile_variable_list_value "shared_objects_opcache" ;; 
  
  
  
  let part1_v1 = Image.image (fun tname ->("make "^tname,"",false)) list2 ;; 
  
  let cr1 = 
   (part1_v1)@
   [
      ("make ext/opcache/opcache.la","",false);
      ("$(LIBTOOL) --tag=CC --mode=install cp ext/opcache/opcache.la $(phplibdir)","",false);
      ("make cli","",false); 
      ("make phpdbg","",false); 
      ("make cgi","",false); 
      ("make pharcmd","",false)
    ] ;; 
  
    let comp1 = Image.image (
      fun target ->(target,get_ingredients_and_recipes_for_target target)
   ) list2 ;;
   
   
  
  
  commands_ref:= cr1 ;;
  
   
  


end ;;


(************************************************************************************************************************
Snippet 122 : Code snippet related to recipes in a makefile
************************************************************************************************************************)
module Snip122=struct

let add_interval_if_nonempty (treated,text,starter,current_idx)=
  if starter > current_idx 
  then treated 
  else (Cull_string.interval text starter current_idx) :: treated ;;   


let rec helper_for_nt_analizer (treated,text,starter,current_idx,text_length) = 
    if current_idx > text_length 
    then  List.rev(add_interval_if_nonempty (treated,text,starter,current_idx))
    else
    if (Strung.get text current_idx)<>'\n'
    then  helper_for_nt_analizer (treated,text,starter,current_idx+1,text_length) 
    else
    let treated2 = add_interval_if_nonempty (treated,text,starter,current_idx-1) in   
    if ( (Strung.get text (current_idx+1))<>'\t' ) || (current_idx+2 > text_length)
    then  List.rev treated2
    else 
    helper_for_nt_analizer (treated2,text,current_idx+2,current_idx+2,text_length) ;;
  
let nt_analize text idx = 
    helper_for_nt_analizer ([],text,idx,idx,String.length text)  ;;
  
  (*  
  nt_analize ("123ab\n\tcdef\n\tghi\njk") 3;;
  *)


end ;;


(************************************************************************************************************************
Snippet 121 : Intial code for Localdircopy module, when no type had been created yet
************************************************************************************************************************)
module Snap138=struct

  let allowed_number_of_digits = 3 ;;


  let is_a_digit c = 
     let i = int_of_char c in 
     (48<=i) && (i<=57) ;; 
  
  let is_not_a_digit c = not(is_a_digit c) ;; 
  
  
  let extract_v_number filename = 
     if not(String.starts_with ~prefix:"v" filename )
     then (None,"",filename)
     else 
     let i1_opt = Strung.char_finder_from_inclusive_opt is_not_a_digit filename 2 in 
     if i1_opt = None
     then (None,"",filename)
     else 
    let i1 = Option.get i1_opt in
     if  (Strung.get filename i1)<>'_'
     then (None,"",filename)
     else 
     let str = Cull_string.interval filename 2 (i1-1) in 
     let v_number = int_of_string str in 
     (Some v_number,Cull_string.beginning i1 filename,Cull_string.cobeginning i1 filename) ;;  
  
  
  
  exception Add_next_index_to_name_exn of string ;;
  
  let add_next_index_in_filename next_idx_opt filename = 
     match next_idx_opt with 
     None -> raise (Add_next_index_to_name_exn(filename))
     | (Some next_idx) -> 
        let str = string_of_int next_idx in 
        let str2 = Strung.insert_repetitive_offset_on_the_left '0' allowed_number_of_digits str in 
        "v"^str2^"_"^filename ;;
  
  let is_not_a_filler c = not(List.mem c [' ';'\t';'\r';'\n';'-';'_']) ;;
  
  exception Empty_subpath of string ;;
  
  let standardized_name_opt next_idx_opt filename = 
     if not(String.starts_with ~prefix:"v" filename)
     then Some(add_next_index_in_filename next_idx_opt filename)
     else 
     let i1_opt = Strung.char_finder_from_inclusive_opt is_not_a_digit filename 2 in 
     if i1_opt = None
     then Some(add_next_index_in_filename next_idx_opt filename)
     else 
     let i1 = Option.get i1_opt in
     let v_string = Cull_string.interval filename 2 (i1-1) in 
     let v_number = int_of_string v_string in
     let standardized_v_string = Strung.insert_repetitive_offset_on_the_left '0' allowed_number_of_digits (string_of_int v_number) in 
     let standardized_start = "v"^standardized_v_string^"_" in 
     let i2_opt = Strung.char_finder_from_inclusive_opt is_not_a_filler filename (i1+1) in 
     if i2_opt = None
     then raise(Empty_subpath(filename))
     else 
     let i2 = Option.get i2_opt in   
     let standardized_name = standardized_start ^ (Cull_string.cobeginning (i2-1) filename) in 
     if  filename <> standardized_name
     then Some(standardized_name)
     else None ;;
     
  let myself = Sys.getenv "USER" ;;
  
  let remote_location = "/media/" ^ myself ^ "/HEAVY/Other/OC/" ;;
  let remote_dir = Directory_name.of_string remote_location ;;
     
  let remote_files = Unix_again.beheaded_simple_ls remote_dir ;; 
     
     
  let u1 = Image.image extract_v_number remote_files ;;   
  
  let u1 = Image.image (fun fn ->(fn,standardized_name_opt None fn) ) remote_files ;;
  
  let fixes = List.filter_map (
        fun (old_fn,new_fn_opt) -> match new_fn_opt with 
           None -> None 
         | Some new_fn -> 
           let old_ap = remote_location ^ old_fn 
           and new_ap = remote_location ^ new_fn in 
           Some( Filename.quote_command "mv" [old_ap;new_ap])
  ) u1 ;; 
     
  let fix_act () = Image.image Sys.command fixes ;; 
  
  let ordered_filenames = 
      let temp0 = Image.image (fun s ->
        (Cull_string.cobeginning (allowed_number_of_digits+2) s,s) ) remote_files in 
      let temp1 = Image.image fst temp0 in 
      let temp2 = Ordered.sort Total_ordering.lex_for_strings temp1 in 
      Image.image (fun s->List.assoc s temp0 ) temp2;; 
  
  
  let pass_point = (Sys.getenv "HOME") ^ "/Downloads/OC/Lennet";;
  
  let pass_dir = Directory_name.of_string pass_point ;; 
  
  let files_to_be_transferred = Unix_again.beheaded_simple_ls pass_dir ;;
  
  let dated_files = Image.image (
     fun fn ->
        let full_fn = pass_point ^ "/" ^ fn in 
        ((Unix.stat full_fn).Unix.st_mtime, fn)
  ) files_to_be_transferred ;;
  
  
  let order_for_floats = ((fun (fl1:float) (fl2:float) ->
     Total_ordering.standard fl1 fl2   
  ) : float Total_ordering_t.t) ;; 
  
  let order_for_dated_files = Total_ordering.product 
     order_for_floats Total_ordering.lex_for_strings ;; 
  
  let transfer_commands = 
     let temp1 = Ordered.sort order_for_dated_files dated_files in 
     let temp2 = Image.image snd temp1 in 
     let n = List.length remote_files in 
     let temp3 = Int_range.index_everything temp2 in 
     let temp4 = Image.image (fun (idx,fn) ->
       let new_fn = add_next_index_in_filename (Some(n+idx)) fn in 
       let old_ap = pass_point ^ "/" ^ fn 
       and new_ap = remote_location ^ new_fn in 
        Filename.quote_command "mv" [old_ap;new_ap]
     ) temp3 in 
     temp4 ;; 
  
  let transfer_act () = Image.image Sys.command transfer_commands ;; 
  

end ;;


(************************************************************************************************************************
Snippet 120 : Enhance a Java file with many printouts
************************************************************************************************************************)
module Snep134=struct

let ap1 = Absolute_path.of_string 
  "~/Downloads/StepExploitationPropertiesConfiguration.java";;

let text1 = Io.read_whole_file ap1 ;; 

let lines1 = Lines_in_text.enhanced_indexed_lines text1 ;;  

let prefix1="public String ";;

let indentation = 8;;
let indenter = String.make indentation ' ';;

let extract1 (length_before,line_index,line) =
  let i1_opt = Strung.char_finder_from_inclusive_opt (fun c->
    not(List.mem c [' ';'\t';'\r'])
  ) line 1 in 
  if i1_opt = None then None else
  let i1 = Option.get i1_opt in 
    let temp1 = Cull_string.cobeginning (i1-1) line in 
  if not(String.starts_with ~prefix:prefix1 temp1) 
  then None 
  else 
  let j1 = i1+(String.length prefix1) -1 in 
  let i2 = (String.index_from line j1 '(')+1 in 
  let getter_name = Cull_string.interval line (j1+1) (i2-1) in 
  let i3 = (String.index_from line i2 '{')+1 in
  let before = Cull_string.beginning i3 line 
  and after = Cull_string.cobeginning i3 line in 
  let msg = "\r\n"^indenter^"System.out.println(\" >>@#@<< Entering "
   ^getter_name^"\");" in 
  Some(line_index,before^msg^after) 
  (*
  Some(length_before,line_index,getter_name,before,after) 
  *)  
;; 
  

let newly_created_lines = List.filter_map extract1 lines1 ;; 

let old_lines = Lines_in_text.indexed_lines text1 ;;

let modified_lines = Image.image (
   fun (idx,line) ->
     match List.assoc_opt idx newly_created_lines with 
     None -> line 
     |Some enhanced_line -> enhanced_line
) old_lines ;;

let text2 = String.concat "\n" modified_lines ;;

let act1 () = Io.overwrite_with ap1 text2 ;; 
(*
let v1 = Tools_for_debugging.extract_from_list extract1 lines1 ;; 
*)




end ;;


(************************************************************************************************************************
Snippet 119 : deduce package from path in a Java project
************************************************************************************************************************)
module Snip119=struct

let prefix = home^"/Downloads/my_springs/spring_0/" ;;

 let sources_in_project=Image.image(
    fun s->prefix^s
 )[
   "spring-boot-2.7.x/buildSrc";
   "spring-framework-5.3.x/buildSrc";
   "spring-framework-5.3.x/spring-tx";
   "spring-framework-5.3.x/spring-aop";
   "spring-framework-5.3.x/spring-jcl";
   "spring-framework-5.3.x/spring-jms";
   "spring-framework-5.3.x/spring-orm";
   "spring-framework-5.3.x/spring-oxm";
   "spring-framework-5.3.x/spring-web";
   "spring-framework-5.3.x/spring-core";
   "spring-framework-5.3.x/spring-jdbc";
   "spring-framework-5.3.x/spring-test";
   "spring-framework-5.3.x/spring-beans";
   "spring-framework-5.3.x/spring-r2dbc";
   "spring-framework-5.3.x/spring-webmvc";
   "spring-framework-5.3.x/spring-aspects";
   "spring-framework-5.3.x/spring-context";
   "spring-framework-5.3.x/spring-webflux";
   "spring-framework-5.3.x/spring-messaging";
   "spring-framework-5.3.x/spring-websocket";
   "spring-framework-5.3.x/integration-tests";
   "spring-framework-5.3.x/spring-expression";
   "spring-framework-5.3.x/spring-instrument";
   "spring-framework-5.3.x/spring-context-indexer";
   "spring-framework-5.3.x/spring-context-support";
   "spring-boot-2.7.x/spring-boot-project/spring-boot";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-cli";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-docs";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-test";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-actuator";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-devtools";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-autoconfigure";
   "spring-boot-2.7.x/spring-boot-system-tests/spring-boot-image-tests";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-test-autoconfigure";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-properties-migrator";
   "spring-boot-2.7.x/spring-boot-system-tests/spring-boot-deployment-tests";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-actuator-autoconfigure";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-antlib";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-loader";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-loader-tools";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-test-support";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-gradle-plugin";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-buildpack-platform";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-jarmode-layertools";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-ant";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-aop";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-jpa";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-war";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-xml";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-gradle-test-support";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-amqp";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-test";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-antlib/src/it/sample";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-batch";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-cache";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-jetty";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-kafka";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-flyway";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-jersey";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-quartz";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-secure";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-simple";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-testng";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-tomcat";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-configuration-metadata";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-integration-tests/spring-boot-loader-tests";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-integration-tests/spring-boot-server-tests";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-graphql";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-hateoas";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-jetty10";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-logback";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-profile";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-rsocket";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-servlet";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-web-jsp";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-webflux";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-autoconfigure-processor";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-configuration-processor";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-activemq";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-actuator";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-data-jpa";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-devtools";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-undertow";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-data-jdbc";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-data-ldap";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-data-rest";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-jetty-jsp";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-jetty-ssl";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-liquibase";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-atmosphere";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-data-r2dbc";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-hazelcast3";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-hazelcast4";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-tomcat-jsp";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-tomcat-ssl";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-web-secure";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-web-static";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-actuator-ui";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-hibernate52";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-integration";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-traditional";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-webservices";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-jta-atomikos";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-session-jdbc";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-undertow-ssl";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-web-mustache";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-junit-vintage";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-oauth2-client";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-secure-jersey";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-session-mongo";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-session-redis";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-web-thymeleaf";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-integration-tests/spring-boot-launch-script-tests";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-actuator-noweb";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-parent-context";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-secure-webflux";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-test-nomockito";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-web-freemarker";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-actuator-log4j2";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-animated-banner";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-web-secure-jdbc";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-websocket-jetty";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-websocket-tomcat";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-data-r2dbc-flyway";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-session-hazelcast";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-web-secure-custom";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-websocket-jetty10";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-bootstrap-registry";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-websocket-undertow";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-property-validation";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-web-method-security";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-data-r2dbc-liquibase";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-web-application-type";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-web-groovy-templates";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-session-webflux-mongo";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-session-webflux-redis";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/run";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/war";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-oauth2-resource-server";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-reactive-oauth2-client";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-saml2-service-provider";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-tomcat-multi-connectors";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-integration-tests/spring-boot-configuration-processor-tests";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-actuator-custom-security";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/run-fork";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-info";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/start-stop";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/run-envargs";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/run-exclude";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/run-jvmargs";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/run-devtools";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/run-profiles";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-smoke-tests/spring-boot-smoke-test-reactive-oauth2-resource-server";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/run-arguments";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-create-dir";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-custom-dir";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-executable";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-test-scope";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/run-toolchains";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-layered/jar";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-with-unpack";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/start-stop-skip";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/war-layered/war";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/war-reactor/war";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/war-with-unpack";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-tags";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-system-scope";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/run-disable-fork";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/war-system-scope";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-exclude-entry";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-exclude-group";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/war-exclude-entry";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-integration-tests/spring-boot-loader-tests/spring-boot-loader-tests-app";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-integration-tests/spring-boot-server-tests/spring-boot-server-tests-app";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-caches";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-network";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-publish";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-attach-disabled";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-classifier-main";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-custom-launcher";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-with-zip-layout";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-bindings";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-cmd-line";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-output-timestamp";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/run-jvm-system-props";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/war-output-timestamp";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-classifier-source";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/run-working-directory";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-classifier";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-final-name";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-info-custom-file";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-layered-custom/jar";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/run-use-test-classpath";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/war-layered-custom/war";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-custom-name";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-info-reproducible";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/run-jvmargs-commandline";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-custom-layout/custom";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-custom-layout/layout";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-layered-disabled/jar";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-system-scope-default";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-with-layout-property";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/start-stop-fork-disabled";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/war-layered-disabled/war";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-bad-buildpack";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-builder-error";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-war-packaging";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-zip-packaging";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-custom-layout/default";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/run-arguments-commandline";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-custom-builder";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-with-repackage";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/run-profiles-fork-disabled";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-caches-multiple";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-empty-env-entry";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-multi-module/app";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-info-custom-build-time";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-classifier-source";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-custom-buildpacks";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-info-disable-build-time";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-info-exclude-build-time";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-layered-no-layer-tools/jar";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/war-layered-no-layer-tools/war";
   "spring-boot-2.7.x/spring-boot-tests/spring-boot-integration-tests/spring-boot-launch-script-tests/spring-boot-launch-script-tests-app";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-multi-module/library";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-info-additional-properties";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-lib-name-conflict/test-project";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-info-exclude-build-properties";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-classifier-main-attach-disabled";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-classifier-with-repackage";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/jar-classifier-source-attach-disabled";
   "spring-boot-2.7.x/spring-boot-project/spring-boot-tools/spring-boot-maven-plugin/src/intTest/projects/build-image-classifier-source-with-repackage"
   ] ;;


let current_source = List.nth sources_in_project 12 ;; 

let files_under_source = Memoized.make(fun s_ap ->
   let dir = Directory_name.of_string s_ap in 
   let all_paths = Unix_again.complete_ls dir in 
   List.filter (fun x->not(Unix_again.is_a_directory x))
      all_paths) ;;


let java_files_under_source = 
   Memoized.make(fun src ->
    let all_files = files_under_source src in 
   List.filter (fun 
    ap->
     let s = Absolute_path.to_string ap in 
     String.ends_with ~suffix:".java" s 
   )
   all_files) ;;

let extract_classname_from_filename ap = 
   let s = Absolute_path.to_string ap in 
     let s2 = Cull_string.after_rightmost s '/' in 
     Cull_string.before_rightmost s2 '.' ;; 

let java_classnames_under_source = 
   Memoized.make(fun src ->
    let java_files = java_files_under_source src in 
    let classnames_inpo = 
       Explicit.image extract_classname_from_filename java_files in 
     Ordered.sort Total_ordering.silex_for_strings 
      classnames_inpo
   ) ;;

let paths_for_classname src cname = 
   List.filter (fun ap ->
      extract_classname_from_filename ap = cname
   ) (java_files_under_source src) ;; 


let table_for_classname_paths_under_source = 
   Memoized.make(fun src ->
    let java_classes = java_classnames_under_source src in 
    Image.image (
   fun cname->(cname,paths_for_classname src cname)
) java_classes 
   ) ;;

let all_files_in_project = 
   let main_dir = Directory_name.of_string prefix in 
   let temp1 = Unix_again.complete_ls main_dir in  
   List.filter (fun ap->
    not(Unix_again.is_a_directory ap)
   ) temp1 ;; 

let all_java_files_in_project = 
   List.filter (
    fun ap ->
      let s_ap = Absolute_path.to_string ap in 
      String.ends_with ~suffix:".java" s_ap 
   ) all_files_in_project ;; 


let table_for_filecontents = Chronometer.it (Explicit.image (
  fun fn ->(fn,Io.read_whole_file fn)
)) all_files_in_project ;; 

let file_contents = Memoized.make(fun fn ->
   List.assoc fn table_for_filecontents
) ;;

let local_occurrences cname fname =
   let text = file_contents fname in 
   let temp1 = Substring.occurrences_of_in cname text in 
   Image.image (
     fun i->(Absolute_path.to_string fname,
      (i,i+(String.length cname)-1))
   ) temp1 ;;

let occurrences cname = 
   let temp1 = Explicit.image (local_occurrences cname) all_files_in_project in 
   List.flatten temp1 ;;   

let all_classnames_in_project =
   let temp1 = Image.image java_classnames_under_source sources_in_project in 
   Ordered.fold_merge
    Total_ordering.silex_for_strings
    temp1 ;;   

let package_from_line_opt untrimmed_line = 
  let line = Cull_string.trim_spaces untrimmed_line in 
  let beginning = "package " 
  and ending = ";" in 
  if not(
    (String.starts_with ~prefix:beginning line ) &&
    (String.ends_with ~suffix:ending line) 
  )
  then None 
  else
  let line2 = Cull_string.two_sided_cutting
        (beginning,ending) line in 
  let line3 = Cull_string.trim_spaces line2 in       
  Some(line3);;


exception Extract_package_from_text_exn ;;
   
let extract_package_from_text text = 
    let lines = Lines_in_text.lines text in 
    match List.find_map package_from_line_opt lines with 
     None -> raise Extract_package_from_text_exn 
    |Some (pn) -> pn ;;  

exception Extract_package_from_java_file_exn of Absolute_path.t ;;
 
let extract_package_from_java_file =Memoized.make(fun jf -> 
  try extract_package_from_text (file_contents jf) with 
  Extract_package_from_text_exn ->
   raise(Extract_package_from_java_file_exn(jf) ) );;
   
let import_from_line_opt untrimmed_line = 
  let line = Cull_string.trim_spaces untrimmed_line in 
  let beginning = "import " 
  and ending = ";" in 
  if not(
    (String.starts_with ~prefix:beginning line ) &&
    (String.ends_with ~suffix:ending line) 
  )
  then None 
  else
  let line2 = Cull_string.two_sided_cutting
        (beginning,ending) line in 
  let line3 = Cull_string.trim_spaces line2 in       
  Some(Cull_string.split_wrt_rightmost line3 '.');;      

let extract_imports_from_text text = 
    let lines = Lines_in_text.lines text in 
    List.filter_map import_from_line_opt lines ;;

let extract_imports_from_java_file =Memoized.make(fun jf -> 
  extract_imports_from_text (file_contents jf)
);;
   
let extract_subdir_from_java_file jf = 
  let s = Absolute_path.to_string jf in
  Cull_string.two_sided_cutting (prefix,"") 
  (Cull_string.before_rightmost s '/') ;;   
   
let subdirs_and_packages = 
  Explicit.image 
  (fun jf ->(
     extract_subdir_from_java_file jf
     ,extract_package_from_java_file jf) )
     all_java_files_in_project ;; 

let all_subdirs = 
   Ordered.sort Total_ordering.silex_for_strings 
     (Image.image fst subdirs_and_packages) ;;

exception Java_separator_in_subdirname of string ;;

let java_separator_in_subdirname subdir = 
  match List.find_opt (
    fun s -> Substring.is_a_substring_of s subdir
 )  ["/java/";"/javaTemplate/";"/javaTemplates/"] with 
 None -> raise (Java_separator_in_subdirname(subdir))
 |Some sep -> sep ;; 

let natural_subdir_to_package subdir = 
   let sep = java_separator_in_subdirname subdir in 
   let ocs = Substring.occurrences_of_in sep subdir in 
   let last_oc = List.hd(List.rev ocs) in 
   let offset = String.length(sep)-1 in 
   let after_sep = Cull_string.cobeginning (last_oc+offset) subdir  in 
   Replace_inside.replace_inside_text 
    ~display_number_of_matches:false
     ("/",".") after_sep ;;

let war_reactor_example = 
   "spring-boot-2.7.x/spring-boot-project/"^
    "spring-boot-tools/spring-boot-maven-plugin/src/intTest/"^
    "projects/war-reactor/war/src/main/java/com/example" ;; 


let subdir_to_package subdir = 
   if subdir = war_reactor_example 
   then "org.test"
   else natural_subdir_to_package subdir ;;


(*

let comp1 = Explicit.image files_under_source sources_in_project ;; 
let comp2 = Explicit.image java_files_under_source sources_in_project ;; 
let comp3 = Explicit.image java_classnames_under_source sources_in_project ;; 
let comp4 = Explicit.image table_for_classname_paths_under_source sources_in_project ;; 
let comp5 = Explicit.image occurrences all_classnames_in_project ;;
let comp6 = Explicit.image extract_package_from_java_file all_java_files_in_project ;; 
let comp7 = Explicit.image extract_imports_from_java_file all_java_files_in_project ;; 


let base1 = Image.image(
  fun (sd,pkg) ->
    (sd,pkg,subdir_to_package sd)
) subdirs_and_packages ;; 


let check_base1 = List.filter(
  fun (sd,pkg,pkg2) -> pkg2 <> pkg
) base1 ;;


let debug_base1 = Tools_for_debugging.extract_from_list (
  fun (sd,pkg) ->
    (sd,pkg,subdir_to_package sd)
) subdirs_and_packages ;; 





let check_comp4 = List.filter (
   fun (s,l) -> List.length(l)>1
) (List.flatten comp4);;
let browse_comp7 = List.filter (
   fun ll -> List.exists(fun (a,b)->String.contains b '*') ll 
) (comp7);;

let z1 = List.nth subdirs_and_packages 100 ;; 
let sz1 = subdir_to_package (fst z1);;

let z2 = List.nth subdirs_and_packages 129 ;; 
let sz2 = subdir_to_package (fst z2);;

let z3 = List.nth subdirs_and_packages 200 ;; 
let sz3 = subdir_to_package (fst z2);;






*)


end ;;


(************************************************************************************************************************
Snippet 118 : computation of sources in a Java project
************************************************************************************************************************)
module Snip118=struct

let dir1 = Directory_name.of_string "~/Downloads/myspring" ;; 
let u1 = Unix_again.complete_ls dir1 ;; 


let files_in_project = List.filter (fun ap->
  not(Unix_again.is_a_directory ap)
) u1 ;; 

let java_files_in_project = List.filter (fun ap->
  let s = Absolute_path.to_string ap in 
  String.ends_with ~suffix:".java" s 
) files_in_project ;; 

let extract_classname_from_filename ap = 
   let s = Absolute_path.to_string ap in 
     let s2 = Cull_string.after_rightmost s '/' in 
     Cull_string.before_rightmost s2 '.' ;; 

let extract_source_from_filename ap = 
   let s = Absolute_path.to_string ap in 
   let i1 = Option.get(Substring.rightmost_index_of_in_opt "/src/" s) in
   let s2 = Cull_string.beginning (i1-1) s in 
   (Cull_string.after_rightmost s2 '/',s2) ;;
     
let sources_in_project_in_no_particular_order = 
 Explicit.image extract_source_from_filename 
 java_files_in_project ;;

let sources_in_project = 
   Ordered.sort Total_ordering.standard 
     sources_in_project_in_no_particular_order ;; 

let source_names_in_project = 
   Ordered.sort Total_ordering.silex_for_strings 
     (Image.image fst sources_in_project) ;; 
  

let locations_for_sourcename srcn =
  List.filter (
      fun (a,b) -> a = srcn
  ) sources_in_project ;; 

let list_for_locations = Image.image (
   fun srcn->(srcn,locations_for_sourcename srcn)
) source_names_in_project;;

let repeated_source_names = List.filter (
   fun (srcn,l) -> List.length(l)>1
) list_for_locations ;; 

let source_locations_in_project = 
   Ordered.sort Total_ordering.silex_for_strings 
     (Image.image snd sources_in_project) ;;

let g1 = Image.image (Cull_string.cobeginning 38) 
   source_locations_in_project ;;

let g2 = Image.image (fun s->"   "^(Strung.enclose s)^";" ) g1;;
let g3 = String.concat "\n" g2 ;; 
let g4 = "\n\n\n let reconstructed_source_locations_in_project=[\n"
   ^g3^"] ;;\n\n\n" ;;

let ap1 = Absolute_path.of_string 
"watched/watched_not_githubbed/cloth.ml";;

let act1 () =
Io.append_string_to_file g4 ap1 ;;   

(*

let check_reconstruction = 
 (reconstructed_source_locations_in_project=
  source_locations_in_project 
 ) ;;

*)




  






(*

let java_classes_in_project_in_no_particular_order = 
 Explicit.image extract_classname_from_filename java_files_in_project ;;

let java_classes_in_project = 
   Ordered.sort Total_ordering.silex_for_strings 
     java_classes_in_project_in_no_particular_order ;; 

let associated_files = Memoized.make(fun s->
   List.filter (fun ap ->
      extract_classname_from_filename ap = s
   ) java_files_in_project
) ;; 

let table_for_associated_files = Explicit.image (
   fun s->(s,associated_files s)
) java_classes_in_project ;;

let repeated_classnames = Explicit.filter (
   fun (s,l) -> List.length(l) > 1
) table_for_associated_files ;; 

let see1 = List.nth repeated_classnames 0;;




let u3 = Explicit.image (fun ap->(ap,Io.read_whole_file ap)) u2;;

let see1 = List.find_map (
   fun (ap,text) ->
     if Substring.is_a_substring_of "@Configuration" text 
     then Some ap 
     else None
) u3 ;;

let see2 = List.find_map (
   fun (ap,text) ->
     if (Substring.is_a_substring_of "bean" text)
         ||
        (Substring.is_a_substring_of "Bean" text)  
     then Some ap 
     else None
) u3 ;;

let sourcename_opt ap = 
   let s = Absolute_path.to_string ap in 
   let (path1,end1) = Cull_string.split_wrt_rightmost s '/' in 
   let (path2,end2) = Cull_string.split_wrt_rightmost path1 '/' in 
   let (path3,end3) = Cull_string.split_wrt_rightmost path2 '/' in 
   
     Cull_string.before_rightmost s2 '.' ;; 
let s = "/Users/ewandelanoy/Downloads/myspring/spring-boot-2.7.x/"^
"spring-boot-tests/spring-boot-smoke-tests/"^
"spring-boot-smoke-test-rsocket/src/main/java/smoketest/" ;;

let i1 = Substring.rightmost_index_of_in "/src/" s ;;
let s2 = Cull_string.beginning (i1-1) s ;;
let s3 = Cull_string.after_rightmost s2 '/' ;;
 

*)


end ;;


(************************************************************************************************************************
Snippet 117 : Storing an old version of code for the Szemeredi problem
************************************************************************************************************************)
module Snip117=struct

module Sz3_types = struct 

(*

#use"lib/Szemeredi/sz3_types.ml";;

We make an exception here to the rule of not having numbers in module names.
Sz3 is short for "third stab at Szemeredi problem".


*)

type width = W of int ;; 

type finite_int_set = FIS of int * (int list) ;; 

type constraint_t = C of int list ;; 

type extension_data = int list ;; 

type solution = int list ;; 

type point = P of finite_int_set * width ;; 

type quantify_constraints =
    Some_constraints
    |All_constraints ;; 

type point_with_breadth = 
  No_constraint of finite_int_set
 |Usual of quantify_constraints * point * int ;; 
  

type handle = 
     Has_no_constraints
    |Select of int * int * int   
    |Rightmost_overflow of int * int * int 
    |Rightmost_pivot of width
    |Fork of int * int * int ;;

type helper = 
  Help_with_solution of point_with_breadth * solution 
 |Help_with_links of point_with_breadth * (int list) ;; 

type fan = F of int list list ;; 

type small_mold = SM of (solution list) * fan ;; 

type mold = BM of extension_data * (int * small_mold) list ;;

type common_table = CT of  (point_with_breadth * (handle * mold)) list ;; 
  
type diagnosis =
   Missing_fan of string * point_with_breadth * int * fan 
  |Missing_solution of string * point_with_breadth * solution
  |Missing_subcomputation of string * point_with_breadth 
  |Missing_switch_in_fork of int * point_with_breadth ;;
      
type fan_related_requirement = FRR of (int * fan) list ;;   

type canonized_requirement = CR of point_with_breadth * int * fan ;; 

type point_with_requirements = PWR of point_with_breadth * ((int * fan) list) ;;

end ;;

module Sz3_preliminaries = struct 

(*

#use"lib/Szemeredi/sz3_preliminaries.ml";;

We make an exception to the rule of not having numbers in module names.
Sz3 is short for "third stab at Szemeredi problem".

*)

type width = Sz3_types.width = W of int ;; 

type finite_int_set = Sz3_types.finite_int_set = FIS of int * (int list) ;; 

type constraint_t = Sz3_types.constraint_t = C of int list;; 

type extension_data = Sz3_types.extension_data  ;; 

type solution = Sz3_types.solution ;; 

type point = Sz3_types.point = P of finite_int_set * width ;; 

type quantify_constraints = Sz3_types.quantify_constraints =
    Some_constraints
    |All_constraints ;; 


type point_with_breadth = Sz3_types.point_with_breadth = 
     No_constraint of finite_int_set
    |Usual of quantify_constraints * point * int ;; 

type handle = Sz3_types.handle = 
   Has_no_constraints
  |Select of int * int * int   
  |Rightmost_overflow of int * int * int 
  |Rightmost_pivot of width
  |Fork of int * int * int ;;  

type fan = Sz3_types.fan = F of int list list ;; 

type small_mold = Sz3_types.small_mold = SM of (solution list) * fan ;; 

type mold = Sz3_types.mold = BM of extension_data * (int * small_mold) list ;;

type common_table = Sz3_types.common_table = CT of  (point_with_breadth * (handle * mold)) list ;; 
  
type diagnosis = Sz3_types.diagnosis =
   Missing_fan of string * point_with_breadth * int * fan 
  |Missing_solution of string * point_with_breadth * solution 
  |Missing_subcomputation of string * point_with_breadth 
  |Missing_switch_in_fork of int * point_with_breadth ;;

type point_with_requirements = Sz3_types.point_with_requirements = PWR of point_with_breadth * ( (int * fan) list ) ;;

let i_order = Total_ordering.for_integers ;;
let i_does_not_intersect = Ordered.does_not_intersect i_order ;;
let i_fold_intersect = Ordered.fold_intersect i_order ;;
let i_fold_merge = Ordered.fold_merge i_order ;;
let i_insert = Ordered.insert i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_intersect = Ordered.intersect i_order ;;
let i_intersects = Ordered.intersects i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_length_preserving_sort = Ordered.length_preserving_sort i_order ;;
let i_outsert = Ordered.outsert i_order ;;
let i_setminus = Ordered.setminus i_order ;;
let i_sort = Ordered.sort i_order ;;


let il_order = Total_ordering.silex_for_intlists ;;
let il_fold_merge = Ordered.fold_merge il_order ;;
let il_insert = Ordered.insert il_order ;;
let il_is_included_in = Ordered.is_included_in il_order ;;
let il_min= Ordered.min il_order ;;
let il_merge = Ordered.merge il_order ;;
let il_sort = Ordered.sort il_order ;;




let order_for_triples = ((fun (W w1,scr1,b1) (W w2,scr2,b2) ->
  let trial1 = i_order w1 w2 in 
  if trial1<>Total_ordering_result_t.Equal then trial1 else 
  let trial2 = i_order (List.length scr2) (List.length scr1) in 
  if trial2<>Total_ordering_result_t.Equal then trial2 else
  let trial3 = Total_ordering.silex_for_intlists scr1 scr2 in 
  if trial3<>Total_ordering_result_t.Equal then trial3 else
    Total_ordering.for_integers b1 b2
): (width * int list * int) Total_ordering_t.t);;

module Constraint = struct 

let select_in_list  l_cstr candidates =  
   List.filter (fun candidate->
    List.for_all( fun (C cstr) ->not(i_is_included_in cstr candidate)) l_cstr ) 
     candidates;;


let width (C l) = W((List.nth l 1)-(List.nth l 0)) ;;

end ;;  

module Find_highest_constraint = struct

  let rec for_exact_width (W w) domain to_be_treated =
    match to_be_treated with 
    [] -> None 
    |p::others ->
       if p<=2*w then None else 
       if i_is_included_in [p-2*w;p-w] domain 
       then Some (C[p-2*w;p-w;p])
       else for_exact_width (W w) domain others ;;     
  
  let rec below_maximal_width (W w) domain =
   match for_exact_width (W w) domain (List.rev domain) with 
   Some (cstr) -> Some(cstr)
   |None ->
      if w<2 then None else 
      below_maximal_width (W (w-1)) domain ;;  
  
  let below_width_bound_pair (W w,bound) domain =
    match List.find_opt(fun b->
      i_is_included_in [b;b+(w+1);b+2*(w+1)] domain
      ) (List.rev(Int_range.range 1 bound)) with 
    Some bmax ->  Some (C[bmax;bmax+(w+1);bmax+2*(w+1)])
    | None -> below_maximal_width (W w) domain ;; 

  end ;;

module Finite_int_set = struct 

  exception Translation_goes_negative of int * finite_int_set ;; 

  module Private = struct

  let to_usual_int_list (FIS(n,scrappers)) = i_setminus (Int_range.range 1 n) scrappers ;; 
  
  let of_usual_int_list domain =
       if domain = [] then FIS(0,[]) else 
       let n = List.hd(List.rev domain) in 
       FIS(n,i_setminus (Int_range.range 1 n) domain) ;;   

  let translation_goes_negative d = function 
    [] -> false
    | m :: _ -> m+d<0 ;;      

  end ;;

  let constructor n scrappers =
      let domain = i_setminus (Int_range.range 1 n) scrappers in 
      Private.of_usual_int_list domain ;; 

  let decompose_wrt_translation fis_domain = 
    let domain = Private.to_usual_int_list fis_domain in 
    let (d,core_domain) = (match domain with 
      [] -> (0,[])
      | h :: _ -> (h-1, if h=1 then domain else 
                    Image.image (fun x->x-(h-1)) domain 
                   )
    ) in 
    (d,Private.of_usual_int_list core_domain) ;; 

  let empty_set = FIS(0,[]) ;;

  let max (FIS(n,_)) = n ;; 

  let of_usual_int_list = Private.of_usual_int_list ;; 

  let order = ((fun (FIS(n1,scr1)) (FIS(n2,scr2)) ->
    let trial1 = i_order n1 n2 in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else 
    let trial2 = i_order (List.length scr2) (List.length scr1) in 
    if trial2<>Total_ordering_result_t.Equal then trial2 else
      Total_ordering.silex_for_intlists scr1 scr2
  ): finite_int_set Total_ordering_t.t);;

  let remove_element fis k=
    let (FIS(n,scrappers)) = fis in 
    if (k>n)||(k<1) then fis else 
    let new_scrappers = i_insert k scrappers in 
    if k <> n then FIS(n,new_scrappers) else 
    if scrappers = Int_range.range 1 (n-1)
    then empty_set
    else   
    let new_z =  Private.to_usual_int_list (FIS(n-1,scrappers)) in 
    let new_max = List.hd(List.rev new_z) in 
    FIS(new_max,List.filter (fun t->t<new_max) scrappers) ;;         

  (*
  
  remove_element (FIS(10,[3;7;8;9])) 10 ;;
  remove_element (FIS(3,[])) 3 ;;
  remove_element (FIS(1,[])) 1 ;;
  remove_element (FIS(1,[])) 4 ;;

  *)

  let size (FIS(n,scr)) = n-(List.length scr);; 

  let to_usual_int_list = Private.to_usual_int_list ;; 

  let translate d fis = 
    let domain = Private.to_usual_int_list fis in 
    if Private.translation_goes_negative d domain 
    then raise(Translation_goes_negative(d,fis))   
    else  
    let translated_domain =  Image.image (fun t->t+d) domain in 
    Private.of_usual_int_list translated_domain;; 

end ;;    

module Point = struct 

  let decompose_wrt_translation (P(fis,w)) = 
     let (d,translated_fis) = Finite_int_set.decompose_wrt_translation fis in 
     (d,P(translated_fis,w));;

  let decrement (P(fis,W w)) = P(fis,W(w-1)) ;;    

  let highest_constraint_opt (P(fis,W w)) = 
    if w<1 then None else
    Find_highest_constraint.below_maximal_width 
      (W w) (Finite_int_set.to_usual_int_list fis);;

  let has_no_constraint pt = (highest_constraint_opt(pt)=None) ;; 

  let is_nontrivial (P(fis,w)) =
    let domain = Finite_int_set.to_usual_int_list fis in
    ((Find_highest_constraint.below_maximal_width w domain) <> None);;

  let max (P(fis,_w)) = Finite_int_set.max fis  ;; 

  let order = ((fun (P(fis1,W w1)) (P(fis2,W w2)) ->
    let trial1 = i_order w1 w2 in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else 
    Finite_int_set.order fis1 fis2
  ): point Total_ordering_t.t);;

  let remove_element (P(fis,w)) pivot = 
    let new_fis = Finite_int_set.remove_element fis pivot in 
    let new_w = (
      match Find_highest_constraint.below_maximal_width w (Finite_int_set.to_usual_int_list new_fis) with
      None -> 0
      |Some(C(l))->(List.nth l 1)-(List.nth l 0)
    ) in 
    P(new_fis,W new_w) ;;

  let remove_elements pt pivots = List.fold_left remove_element pt pivots ;;   

  let size (P(fis,_)) = Finite_int_set.size fis ;; 

  let standardize pt =
     let (P(fis,_)) = pt in 
      match highest_constraint_opt pt with  
      None -> P(fis,W 0)
    |Some(C cstr) -> 
      let nth = (fun k->List.nth cstr (k-1)) in 
      let w = W((nth 2)-(nth 1)) in 
      P(fis,w);;
 ;; 

  let supporting_set (P(fis,_)) = Finite_int_set.to_usual_int_list fis ;; 

  let subset_is_admissible (P(_,w)) subset =
      ((Find_highest_constraint.below_maximal_width w subset) =None);;

  let translate d (P(fis,w)) = 
      P(Finite_int_set.translate d fis,w);;

  let width (P(_,w)) = w ;; 


end ;;   



module Point_with_breadth = struct 

exception No_constraint_no_point_no_breadth_exn of finite_int_set ;;   

module Private = struct

let last_step_in_constructor pt b =
  let n = Point.max pt and (W w) = Point.width pt in 
  let qc = (
     if b= n - 2*(w+1)
     then All_constraints
     else Some_constraints
  ) in 
  Usual(qc,pt,b) ;; 

let constructor n scr (W old_w) b =  
  let fis = FIS(n,scr) in
  let pt = P(fis,W old_w) in 
  let support = Point.supporting_set pt in 
  let possible_breadths = List.rev(Int_range.range 1 b) in 
  match List.find_opt (fun t->i_is_included_in [t;t+(old_w+1);t+2*(old_w+1)] support) possible_breadths with
  Some(b0)-> last_step_in_constructor pt b0
  |None ->
  match Point.highest_constraint_opt pt with  
   None -> No_constraint(fis)
  |Some(C cstr) -> 
    let nth = (fun k->List.nth cstr (k-1)) in 
    let new_b = nth 1
    and new_w = (nth 2)-(nth 1)-1 in 
    last_step_in_constructor (P(fis,W new_w)) new_b;;   


let constructor_for_two (P(FIS(n,scr),w)) b = constructor n scr w b ;;    

let constructor_for_three (FIS(n,scr)) w b = constructor n scr w b ;; 

let all_constraints pt = constructor_for_two pt 0 ;; 


let usual_decomposition_opt = function 
    (No_constraint _fis) -> None
   |Usual(_qc,pt,b) ->  
    let (P(_,W w)) = pt in 
    Some(constructor_for_two pt (b-1),C[b;b+(w+1);b+2*(w+1)]);;

  
  let translate d  = function 
     (No_constraint fis) -> (No_constraint (Finite_int_set.translate d fis))
      (*
      when b+d<0, there is no extra constraint, 
      and this is equivalent to setting the new b to 0  
    *) 
    |Usual(_qc,pt,b) ->  
      let new_b = max(0)(b+d) in
      constructor_for_two(Point.translate d pt) new_b ;;

    let support = function 
    (No_constraint fis) -> fis
    |Usual(_qc,P(fis,_w),_b) -> fis  ;;

    let supporting_set pwb = Finite_int_set.to_usual_int_list (support pwb) ;;    
    
    let point_and_breadth = function 
    (No_constraint fis) -> raise(No_constraint_no_point_no_breadth_exn(fis))
    |Usual(_qc,pt,b) -> (pt,b)  ;;

  let has_no_constraint = function 
    (No_constraint _fis) -> true
    |Usual(_qc,_pt,_b) -> false  ;; 

   let complementary_pairs pwb = 
    if has_no_constraint pwb then [] else 
     let (P(FIS(n,_scr),W max_w),b) = point_and_breadth pwb 
     and domain = supporting_set pwb in 
     let candidates = Int_range.range 1 (max_w+1) in 
     List.filter_map (
         fun w->
            let u = n-2*w and v=n-w in 
           if not(i_is_included_in [u;v] domain) then None else
           if w<=max_w then Some(u,v) else 
           if u<=b then Some(u,v) else None  
     ) candidates ;;

   let obstructions pwb =
     if has_no_constraint pwb then [] else 
     let (P(FIS(n,_scr),W wmax),b) = point_and_breadth pwb in 
     let obstructions_for_width = (fun w->Int_range.scale(fun t->[t;t+w;t+2*w]) 1 (n-2*w)) in 
       List.flatten((Int_range.scale obstructions_for_width 1 wmax)@
       [Int_range.scale(fun t->[t;t+(wmax+1);t+2*(wmax+1)]) 1 b]);;

   let solutions pwb offset =
      let temp1 = il_sort(List_again.power_set (supporting_set pwb)) in 
      let obstrs = obstructions pwb in
      let temp2 = List.filter (fun y->List.for_all (fun obs->not(i_is_included_in obs y))obstrs) temp1 in 
      let m = List.length(List.hd(List.rev temp2)) in 
      List.filter (fun y->List.length(y)=m-offset) temp2 ;; 

   let rightmost_largest_width pwb =
      let (P(FIS(n,_scr),W w),b) = point_and_breadth pwb in 
      if b>=(n-2*(w+1))
      then W(w+1)
      else W(w) ;;   

    let inclusion_test_for_non_isolation wmax b domain w candidate =
        if w=wmax+1
        then (i_is_included_in candidate domain) && (List.hd(candidate)<=b)
        else i_is_included_in candidate domain  ;;

   let atomic_test_for_non_isolation wmax b domain x w = 
       if w>wmax+1 then false else 
        let incl_test = inclusion_test_for_non_isolation wmax b domain w in 
       (incl_test [x-2*w;x-w;x])
       || 
       (incl_test [x-w;x;x+w]) 
       ||
       (incl_test [x;x+w;x+2*w]) ;; 


   let individual_test_for_non_isolation wmax b domain x=
       List.exists(atomic_test_for_non_isolation wmax b domain x) (Int_range.range 1 (wmax+1)) ;;

    let nonisolated_version pwb =
       if has_no_constraint pwb 
       then let fis = support pwb in 
            (No_constraint Finite_int_set.empty_set,Finite_int_set.to_usual_int_list fis) 
       else let (P(fis,W wmax),b) = point_and_breadth pwb in 
       let domain = Finite_int_set.to_usual_int_list fis in 
       let (non_isolated,isolated) = List.partition (individual_test_for_non_isolation wmax b domain) domain in 
       let new_fis = Finite_int_set.of_usual_int_list non_isolated in 
       (constructor_for_three new_fis (W wmax) b,isolated);;
    
    let remove_element pwb elt = 
      if has_no_constraint pwb 
      then let fis = support pwb in 
             (No_constraint (Finite_int_set.remove_element fis elt)) 
      else let (P(fis,W wmax),b) = point_and_breadth pwb in 
      let new_fis = Finite_int_set.remove_element fis elt in 
      let new_domain = Finite_int_set.to_usual_int_list new_fis in 
      match Find_highest_constraint.below_width_bound_pair (W wmax,b) new_domain with
      None -> No_constraint(new_fis)
      |Some(C cstr)->
        let nth = (fun k->List.nth cstr (k-1)) in 
        let new_wmax = (nth 2)-(nth 1)-1 in 
        constructor_for_three new_fis (W new_wmax) (nth 1);;

    let compare_pbs pt1 b1 pt2 b2 =
      let trial1 = Point.order pt1 pt2 in 
      if trial1<>Total_ordering_result_t.Equal then trial1 else 
      i_order b1 b2;;

  

    let compare_in_no_constraints_case fis1 = function 
      (No_constraint fis2) -> Finite_int_set.order fis1 fis2
      |Usual(_qc,_pt,_b) -> Total_ordering_result_t.Lower  ;; 

    let compare_in_usual_case pt1 b1 = function 
      (No_constraint _fis2) -> Total_ordering_result_t.Greater
      |Usual(_qc2,pt2,b2) -> compare_pbs pt1 b1 pt2 b2  ;;   

    

    let order = ((fun pwb1 pwb2 ->
          match pwb1 with 
          (No_constraint fis1) -> compare_in_no_constraints_case fis1 pwb2 
          |Usual(_qc1,pt1,b1) -> compare_in_usual_case pt1 b1 pwb2
          ): point_with_breadth Total_ordering_t.t);;    

    let subset_is_admissible pwb subset = 
      if has_no_constraint pwb 
      then true
     else
     let (pt,b) = point_and_breadth pwb in    
      if not(Point.subset_is_admissible pt subset)
      then false 
      else 
        let (P(_,W w)) = pt in 
        List.for_all (fun t->not(i_is_included_in [t;t+(w+1);t+2*(w+1)] subset)) (Int_range.range 1 b);;    

    let decompose_wrt_translation pwb = match pwb with
        (No_constraint fis) -> 
            let (d,translated_fis) = Finite_int_set.decompose_wrt_translation fis in 
            (d,No_constraint translated_fis)
        |Usual(_qc,_pt,_b) -> 
          let (pt,_b) = point_and_breadth pwb in 
          let (d,_) = Point.decompose_wrt_translation pt in 
          (d,translate (-d) pwb);; 


    let left pwb =
        let n = Finite_int_set.max (support pwb) in 
        remove_element pwb n ;;        

    let predecessor_opt pwb = match usual_decomposition_opt pwb with 
      None -> None 
     |Some(prec_pwb,_) -> Some prec_pwb ;;   
       

end ;;  

let all_constraints = Private.all_constraints ;; 
let breadth pwb =snd(Private.point_and_breadth pwb);;
let constructor = Private.constructor ;; 
let complementary_pairs = Private.complementary_pairs ;;
let decompose_wrt_translation = Private.decompose_wrt_translation ;;  
let has_no_constraint pwb = (Private.usual_decomposition_opt pwb=None) ;;
let left = Private.left ;;  
let max pwb = Finite_int_set.max (Private.support pwb) ;;
let nonisolated_version = Private.nonisolated_version ;;
let order = Private.order ;; 
let predecessor_opt = Private.predecessor_opt ;;
let point pwb = fst(Private.point_and_breadth pwb) ;;  
let projection pwb = snd(decompose_wrt_translation pwb);;
let remove_element = Private.remove_element ;;
let rightmost_largest_width = Private.rightmost_largest_width ;;
let size pwb = match pwb with 
   No_constraint(fis)->Finite_int_set.size fis 
  |Usual(_qc,pt,_b)-> Point.size pt ;;
let solutions = Private.solutions ;;  
let subset_is_admissible= Private.subset_is_admissible ;; 
let support  = Private.support ;; 
let supporting_set  = Private.supporting_set ;;
let translate = Private.translate ;; 
let usual_decomposition_opt = Private.usual_decomposition_opt ;; 
let width pwb =
    let (pt,_b) = Private.point_and_breadth pwb in 
    Point.width pt ;;

end ;;  


module Fan = struct 

  exception Impose_exn of fan * (constraint_t list);;
  exception Badly_formed_fan ;;
  exception No_pullback_without_a_constraint_exn;;

  module Private = struct

    let constructor ll =
      if ll= [] then raise Badly_formed_fan else 
      let sorted_ll = il_sort ll in 
      F (Ordered_misc.minimal_elts_wrt_inclusion(sorted_ll));;

  let distribute (F rays) addendum= F(Image.image (i_merge addendum) rays) ;;  

  let combine_two_conditions (F ll1) (F ll2) =
    let temp1 = Cartesian.product ll1 ll2 in 
    constructor( Image.image (fun (x,y)->i_merge x y) temp1 );; 

 let combine_conditions = function 
     [] -> F[]
    |first_fan :: other_fans ->
       List.fold_left combine_two_conditions first_fan other_fans ;; 

  let canonical_container_in_hard_case initial_competing_fans =
    let measure = (fun (F rays)->
      i_length_preserving_sort (Image.image List.length rays)
    ) in 
    let temp1 = Image.image measure initial_competing_fans in 
    let smallest_measure = il_min temp1 in 
    let competing_fans = 
        List.filter(fun mz->measure(mz)=smallest_measure)  
            initial_competing_fans in 
    combine_conditions competing_fans ;; 

  let canonical_container sample (F rays) =
     let indexed_rays = Int_range.index_everything rays in 
     let covering_indices = (fun x->
        List.filter_map (fun (idx,ray)->
           if i_is_included_in ray x 
           then Some idx 
          else None   
        ) indexed_rays
      ) in
      let temp1 = Image.image covering_indices sample in 
      let temp2 = Ordered_misc.minimal_transversals temp1 in 
      let (_,temp3) = Min.minimize_it_with_care List.length temp2 in 
      let return_to_original = (fun l->F(Image.image(fun idx->List.assoc idx indexed_rays) l)) in 
      if List.length temp3 = 1 
      then (true,return_to_original (List.hd temp3)) 
      else      
      let temp4 = Image.image return_to_original temp3 in
      (false,canonical_container_in_hard_case temp4) ;;

    let impose_opt l_cstr (F rays) =  
        let new_rays =Constraint.select_in_list l_cstr rays in 
        if new_rays = []
        then None
        else Some(F new_rays);;  

    let with_or_without (F(ll)) n complements_for_n =
          let rays_for_n=Image.image (fun (i,j)->[i;j]) complements_for_n in   
          let (with_n,without_n)=List.partition (i_mem n) ll in 
          let with_n_removed = Image.image (i_outsert n) with_n in 
          (constructor(with_n_removed@without_n@rays_for_n),F(without_n)) ;;     

    let union (F ll1) (F ll2) = constructor(ll1@ll2) ;; 

   
    let pull_one_level handle destination_pwb (level_in_mold,destination_fan)= 
       (* note that this function does not take into account
          the extra fan-related conditions that come from the handle,
          we cannot do so at this individual level stage   
       *)
        let n = Point_with_breadth.max destination_pwb 
        and complements_for_n = Point_with_breadth.complementary_pairs destination_pwb  in
        let draft= (match handle with  
         Has_no_constraints -> raise(No_pullback_without_a_constraint_exn)
       | Rightmost_pivot(_) -> 
            (* in this case n is involved, the destination level set is included in 
              the union of two translates of source level sets, and measure(dest)=measure(source)+1 *)        
            let (with_n,without_n) = with_or_without destination_fan n complements_for_n in 
            [level_in_mold-1,without_n;level_in_mold,with_n]
       | Rightmost_overflow (_,_,_) -> 
             (* in this case n is involved, the destination level set is included in 
              the union of two translates of source level sets, and measure(dest)=measure(source) *) 
            let (with_n,without_n) = with_or_without destination_fan n complements_for_n in 
            [level_in_mold,without_n;level_in_mold+1,with_n]
       | Select (i,j,k) ->  
            (* in this case n is not involved, the destination level set is included in 
              one source level set, and measure(dest)=measure(source) *) 
            let relaxed_fan = union destination_fan (F[[i;j;k]])   in
            [level_in_mold,relaxed_fan]
       | Fork (i,j,k) -> 
             (* in this case n is not involved, the destination level set is included in 
              one source level set, and measure(dest)=measure(source)-1 *)
            let relaxed_fan = union destination_fan (F[[i;j;k]])   in
            [level_in_mold+1,relaxed_fan]) in
       List.filter (fun (level_in_mold,_)->level_in_mold>=0) draft ;; 

  let insert_smoothly_at_zero fan old_reqs = match old_reqs with 
    [] -> [0,fan]
    | (l1,fan1) :: others ->
       if l1 = 0
       then (0,combine_two_conditions fan fan1) :: others
       else old_reqs ;;  


  let add_handle_related_conditions old_reqs  = function 
    Has_no_constraints -> raise(No_pullback_without_a_constraint_exn)
  | Rightmost_pivot(_) 
  | Select (_,_,_) -> old_reqs  
  | Rightmost_overflow (u,v,_) -> insert_smoothly_at_zero (F[[u;v]]) old_reqs
  | Fork (i,j,k) -> insert_smoothly_at_zero (F[[i;j;k]]) old_reqs ;;  

  let pull_all_levels handle ~destination:destination_pwb old_levelled_fans= 
      let unordered_pulled_levelled_fans = List.flatten(Image.image(pull_one_level handle destination_pwb) old_levelled_fans) in 
      let unordered_levels = Image.image fst unordered_pulled_levelled_fans in 
      let levels = i_sort unordered_levels in 
      let before_handle_related_conditions = List.filter_map (
        fun k ->
          let fans = List.filter_map (fun ((level_in_mold,fan))->
              if level_in_mold=k then Some fan else None) unordered_pulled_levelled_fans in 
          let final_fan = combine_conditions fans in   
          if final_fan = F [[]]
          then None   
          else Some(k,final_fan)     
      ) levels in 
      add_handle_related_conditions  before_handle_related_conditions handle ;;
  


  end ;;  

  let canonical_container = Private.canonical_container ;; 

  let combine_two_conditions = Private.combine_two_conditions ;; 

  let combine_conditions = Private.combine_conditions ;;  

  let constructor = Private.constructor ;;

  let core (F ll) = 
    i_fold_intersect ll ;; 

  let empty_one = F [[]] ;;

  let impose l_cstr fan = 
     match Private.impose_opt l_cstr fan with 
     None -> raise(Impose_exn(fan,l_cstr))
    |Some answer -> answer;;
  
  let impose_and_distribute  (l_cstr,addendum) fan = 
      Private.distribute ( impose l_cstr fan) addendum ;;

  let impose_opt = Private.impose_opt ;; 

  let pull = Private.pull_all_levels ;;  

  let translate d (F rays) = F(Image.image (fun ray->Image.image (fun t->t+d) ray) rays);;

  let union = Private.union ;; 
  


end ;;   

module Small_mold = struct 

  let add_isolated_set (SM(sols,fan)) isolated_set =
    let add = i_merge isolated_set in 
    SM(Image.image add sols,Fan.impose_and_distribute ([],isolated_set) fan) ;;

  let constructor sols forced_elts = SM(sols,Fan.constructor [forced_elts]);;

  let empty_one = SM([],Fan.empty_one);;

  let fan (SM(_sols,fan_inside)) = fan_inside ;;

  let impose l_cstr (SM(sols,fan)) =
     SM(Constraint.select_in_list l_cstr sols,Fan.impose l_cstr fan) ;; 

  let translate d (SM(sols,fan))  = 
    SM(Image.image (Image.image (fun t->t+d)) sols,Fan.translate d fan) ;;  

  let test_for_impossible_constraint (SM(_sols,fan)) c_constraints =
     ((Fan.impose_opt c_constraints fan)=None) ;;

  let typical_selection (complements,addendum_opt) (SM(sols1,fan1)) = 
    let automatically_distributed = Option.to_list addendum_opt in 
    let for_a_solution_set = (fun sols->Image.image (i_merge automatically_distributed)
        (Constraint.select_in_list complements sols))
    and for_a_fan = Fan.impose_and_distribute (complements,automatically_distributed) in 
    SM(for_a_solution_set sols1,for_a_fan fan1)  
       ;;

  let typical_union (complements,n) (SM(sols1,fan1)) (SM(sols2,fan2))= 
    let for_a_solution_set = (fun sols->Image.image (i_insert n)
        (Constraint.select_in_list complements sols))
    and for_a_fan = Fan.impose_and_distribute (complements,[n]) in 
    SM(il_merge (for_a_solution_set sols1) sols2,
       Fan.union (for_a_fan fan1) fan2  
      ) ;;

end ;;
 

module Mold = struct 

  exception Negative_fan_index_exn of int ;;
  exception In_short_sized_case_exn of point_with_breadth ;;

  module Private = struct 
  
  let constructor_opt pwb naive_forced_elts l = 
      let (_,isolated_elts) = Point_with_breadth.nonisolated_version pwb 
      and (SM(_,first_fan)) = List.assoc 0 l in 
      let forced_elts = i_fold_merge [naive_forced_elts;isolated_elts;Fan.core first_fan] in  
      let (SM(sols,_)) = List.assoc 0 l in 
      if sols <> [] 
      then Some(BM(forced_elts,l))
      else None ;;
  
  let small_mold_at_index (BM(_,l)) i = 
      if i<0 then raise (Negative_fan_index_exn i) else
      match List.assoc_opt i l with 
     Some small_mold -> small_mold 
    |None -> SM([],Fan.empty_one) ;; 

  let solutions (BM(_,l)) = 
      let (SM(sols,_)) = List.assoc 0 l in sols ;;     
  
  end ;;  
  
  let add_isolated_set (BM(forced_elts,l)) isolated_set =
     let add = i_merge isolated_set in
     let tip = List.assoc 0 l in  
     BM(add forced_elts,[0,Small_mold.add_isolated_set tip isolated_set]) ;;
  
  let discrete domain = BM(domain,[0,SM([domain],Fan.constructor [domain])]) ;; 
  
  let fan_at_index mold i = Small_mold.fan(Private.small_mold_at_index mold i) ;;

  let forced_elements (BM(ext,_)) = ext ;; 
  
  let fork_opt pwb prec_mold pointed_ones (i,j,k) = 
      let (BM(_prec_ext,prec_l)) = prec_mold in 
      let c_constraints = [C[i;j;k]] 
      and sols = il_fold_merge(Image.image Private.solutions pointed_ones) in 
      let (SM(_,old_fan1)) = Private.small_mold_at_index prec_mold 1 in 
      let first_fan = Fan.impose c_constraints old_fan1 in  
      let new_l= (0,SM(sols,first_fan))::(List.filter_map (
            fun (i,old_indication)->
               if i<=1 then None else
               Some(i-1,Small_mold.impose c_constraints old_indication) 
      )  prec_l) in
     Private.constructor_opt pwb [] new_l ;; 
  
  let from_exhaustive_list_of_solutions all_sols =
     let forced_elts = i_fold_intersect all_sols in 
     BM(forced_elts,[0,Small_mold.constructor all_sols forced_elts]) ;; 

  let from_single_solution sol = BM(sol,[0,Small_mold.constructor [sol] sol]) ;; 

  let in_short_sized_case pwb =
    if ((Point_with_breadth.size pwb)>18)
    then raise(In_short_sized_case_exn(pwb))
    else 
      let all_sols = Point_with_breadth.solutions pwb 0 in   
      from_exhaustive_list_of_solutions all_sols  ;; 

  let rightmost_overflow_opt full_pwb left_mold =
      let (BM(_left_ext,left_data)) = left_mold in  
      let c_pairs = Point_with_breadth.complementary_pairs full_pwb 
      and n = Point_with_breadth.max full_pwb in 
      let c_constraints = Image.image (fun (i,j)->C[i;j]) c_pairs in  
      let old_range = Image.image fst left_data in 
      let new_range = i_merge [0;1] (List.filter (fun i->(i_mem (i+1) old_range)) old_range) in   
      let get = Private.small_mold_at_index left_mold  in 
      let new_l = Image.image (
        fun i->
           (i,Small_mold.typical_union (c_constraints,n) (get(i+1)) (get i)) 
      )  (i_insert 0 new_range) in 
      Private.constructor_opt full_pwb [] new_l ;;     
  
  let rightmost_pivot_opt full_pwb left_mold =
    let (BM(left_ext,left_data)) = left_mold in  
    let c_pairs = Point_with_breadth.complementary_pairs full_pwb 
    and n = Point_with_breadth.max full_pwb in 
    let c_constraints = Image.image (fun (i,j)->C[i;j]) c_pairs in  
    let old_range = Image.image fst left_data in 
    let new_range = i_merge [0;1] (List.filter (fun i->i_mem (i-1) old_range) old_range) in   
    let get = Private.small_mold_at_index left_mold in 
    if Small_mold.test_for_impossible_constraint (get 0) c_constraints
    then None 
    else  
    let new_l = Image.image (
          fun i->
           if i=0
           then (i,Small_mold.typical_selection (c_constraints,Some n) (get i) )
           else (i,Small_mold.typical_union (c_constraints,n) (get i) (get(i-1)) )
            ) 
    new_range in 
    Private.constructor_opt full_pwb (i_insert n left_ext) new_l  ;;         
  
  let select_opt pwb prec_mold (i,j,k) = 
    let (BM(prec_ext,prec_l)) = prec_mold in 
    let get = Private.small_mold_at_index prec_mold in 
    if Small_mold.test_for_impossible_constraint (get 0) [C[i;j;k]]
    then None 
    else  
    let new_l = Image.image (
          fun (t,old_data_for_t)->
           (t,Small_mold.typical_selection ([C[i;j;k]],None) old_data_for_t 
            ) 
    )  prec_l in 
  Private.constructor_opt pwb prec_ext new_l  ;; 
  
        let shallow sols = 
          BM([],[0,SM(sols,Fan.empty_one)])  ;; 
  
     let small_mold_at_index = Private.small_mold_at_index ;; 

       let solutions = Private.solutions ;;        
  
       let translate d (BM(ext,l)) = 
             BM(Image.image(fun t->t+d) ext,
              Image.image (fun (i,data)->(i,Small_mold.translate d data)) l
             );;
  
  
  end ;;    

module Handle = struct 

let translate d handle = 
   match handle with 
  Has_no_constraints
| Rightmost_pivot(_) -> handle 
| Select (i,j,k) -> Select (i+d,j+d,k+d)
| Rightmost_overflow (i,j,k) -> Rightmost_overflow (i+d,j+d,k+d)
| Fork (i,j,k) -> Fork(i+d,j+d,k+d) ;; 

end ;;  



module Common_table = struct 

  module Private = struct
  
    let handle_order = ((fun handle1 handle2 ->Total_ordering.standard handle1 handle2 
    ): handle Total_ordering_t.t);; 
  
    let mold_order = ((fun mold1 mold2 ->Total_ordering.standard mold1 mold2 
    ): mold Total_ordering_t.t);; 
  
    let hm_order = Total_ordering.product handle_order mold_order ;;
   
    let order = Total_ordering.product Point_with_breadth.order hm_order ;; 
   
    let rec get_opt key (CT l) = match l with 
     [] -> None 
     | (key2,val2) :: others ->
        match Point_with_breadth.order key key2 with 
         Total_ordering_result_t.Lower -> None
        |Total_ordering_result_t.Greater -> get_opt key (CT others) 
        |Total_ordering_result_t.Equal -> Some val2 ;;  
      
    let insert new_key data = Ordered.insert order new_key data ;; 
    let merge data1 data2 = Ordered.merge order data1 data2 ;; 
    let sort data = Ordered.sort order data ;; 

    
     end ;;
  
  let add (CT l) pwb pair = 
    CT(Private.insert (pwb,pair) l) ;;
   
  
  let add_if_it_has_constraints flg pwb pair =
    if Point_with_breadth.has_no_constraint pwb 
    then flg
    else add flg pwb pair;;

  let add_several (CT l) pairs=
  CT(Private.merge (Private.sort pairs) l) ;;
   
  
  let get_opt = Private.get_opt ;;   
  

end ;;


module Impatiently = struct 

  module Private = struct 

    type piece_of_help =  {
      beneficiary : point_with_breadth ;
      extra_solutions : (int * solution list) list;
      imposed_fans : (int *fan) list;
      extra_grooves_for_fork : int list;
   } ;; 

    type extra_help  = {
      helpers : piece_of_help list;
      pair_level : ((width * int list) * (int -> int -> handle * mold)) list;
      triple_level : ((width * int list * int) * (int -> handle * mold)) list
    } ;;


    module Piece_of_help = struct 

      module Private = struct 
      
        let order_for_extra_solutions =((fun
          (v1:((int * solution list) list)) (v2:((int * solution list) list)) ->
              Total_ordering.standard v1 v2
        ): ((int * solution list) list) Total_ordering_t.t) ;; 
      
        let order_for_imposed_fans =((fun
          (v1:((int * fan) list)) (v2:((int * fan) list)) ->
              Total_ordering.standard v1 v2
        ): ((int * fan) list) Total_ordering_t.t) ;; 
      
        let order_for_extra_grooves =((fun
          (v1:(int list)) (v2:(int list)) ->
              Total_ordering.standard v1 v2
        ): (int list) Total_ordering_t.t) ;; 
      
        let order_for_fourtuples =
            Total_ordering.quadruple_product 
              Point_with_breadth.order  order_for_extra_solutions  order_for_imposed_fans  order_for_extra_grooves ;; 
        
        let to_uple gr = (gr.beneficiary,gr.extra_solutions,gr.imposed_fans,gr.extra_grooves_for_fork)
      
        let order =((fun
          (v1:piece_of_help) (v2:piece_of_help) ->
            order_for_fourtuples (to_uple v1) (to_uple v2)
        ): piece_of_help Total_ordering_t.t) ;; 
      
      end ;;  
      
      let extra_solutions_at_level help i = 
        match List.assoc_opt i help.extra_solutions with 
        (Some extra_sols) -> extra_sols 
        | None -> [] ;; 
      
      let imposed_fan_at_level help i = List.assoc_opt i  help.imposed_fans ;; 
      
      let order = Private.order ;; 
      
      end ;;  
      
      module Help = struct 
      
      module Private = struct
      
      
      let replace_perhaps original replacer_opt = match replacer_opt with 
        None -> original 
        |Some replacer -> replacer ;;
      
      let apply_individual_help_except_extra_grooves help mold =
          let (BM(ext,old_data)) = mold in 
          let old_range = Image.image fst old_data in
          let new_range = i_merge old_range (Image.image fst help.imposed_fans) in 
          let new_data = Image.image (
            fun i->
               let (SM(old_sols,old_fan)) = Mold.small_mold_at_index mold i in 
               (i,SM(
                 il_merge old_sols (Piece_of_help.extra_solutions_at_level help i ),
                 replace_perhaps old_fan (Piece_of_help.imposed_fan_at_level help i) ))
          ) new_range in 
          BM(ext,new_data) ;; 
          
      let apply_help_except_extra_grooves helpers pwb mold =
         match List.find_opt (fun help->help.beneficiary = pwb) helpers with 
            None -> mold 
           |Some help -> apply_individual_help_except_extra_grooves help mold ;; 
      
      let extra_grooves helpers pwb = 
        match List.find_opt (fun help->help.beneficiary = pwb) helpers with 
            None -> []
           |Some help -> help.extra_grooves_for_fork ;; 
        
      let rec get_opt pwb = function 
        [] -> None 
        | piece :: others ->
          match Point_with_breadth.order pwb piece.beneficiary with 
          Total_ordering_result_t.Lower -> None
         |Total_ordering_result_t.Greater -> get_opt pwb others  
         |Total_ordering_result_t.Equal -> Some piece ;;  
      
      let institute_fan helpers (PWR(pwb,l))  =
          match get_opt pwb helpers with 
          None -> let piece = { 
                    beneficiary = pwb;
                    extra_solutions = [];
                    imposed_fans  = l;
                    extra_grooves_for_fork = [];
                  } in 
                  Ordered.insert Piece_of_help.order piece helpers
         | Some old_piece ->
           let new_piece = {
                old_piece with 
                imposed_fans = l
           } in 
           Image.image (fun piece -> if piece.beneficiary=pwb then new_piece else piece) helpers ;;
      
           
      
      end ;;
      
      let apply_help_except_extra_grooves = Private.apply_help_except_extra_grooves ;; 
      let extra_grooves = Private.extra_grooves ;;
      let institute_fan = Private.institute_fan ;; 
      
      end ;;  

    module Generic_extra_help = struct
  
      module Private = struct 
    
       let empty_one = {
         helpers = [];
         pair_level = [];
        triple_level  = [];
       } ;;
    
      let immediate_eval_opt fgr pwb = 
        if Point_with_breadth.has_no_constraint pwb 
        then let domain = Point_with_breadth.supporting_set pwb in 
             Some(Has_no_constraints,
               Help.apply_help_except_extra_grooves (fgr.helpers) pwb (Mold.discrete domain)) 
        else     
        let (FIS(n,scr)) = Point_with_breadth.support pwb 
        and w = Point_with_breadth.width pwb 
        and b = Point_with_breadth.breadth pwb in 
        let wpair = (w,scr) in
        match List.assoc_opt wpair fgr.pair_level with 
        Some (f) -> let (handle,mold) =f b n in 
                    Some(handle,mold)    
      | None ->
        let wtriple = (w,scr,b) 
        and n =  Point_with_breadth.max  pwb  in 
        match List.assoc_opt wtriple fgr.triple_level with 
          Some (f) -> let (handle,mold) =f n in 
                      Some(handle,mold)    
        | None -> None ;;    
      
        let institute_fan fgr pwb_with_reqs =
          {
            fgr with
            helpers = (Help.institute_fan (fgr.helpers) pwb_with_reqs)
          } ;; 
    
    
      end ;; 
    
     let empty_one =  Private.empty_one ;; 
     let immediate_eval_opt = Private.immediate_eval_opt ;; 
     let institute_fan = Private.institute_fan ;;
    
    end ;;  
    
    
    module Instituted_extra_help = struct
    
      module Private = struct 
      
        let main_ref = ref Generic_extra_help.empty_one ;; 
    
        let institute_fan pwb_with_reqs =
            let old_val = !main_ref in 
            let new_val = Generic_extra_help.institute_fan old_val pwb_with_reqs in 
            let _ = (main_ref:=new_val) in 
            () ;;
    
        institute_fan 
        (PWR(No_constraint(FIS(2,[])),
        [(0, F [[1; 2]]);
         (1, F [[1]; [2]])])) ;; 
    
      end ;; 
    
      let main_ref = Private.main_ref ;;
      
    
    end ;;  
    
    

    let combined_eval_opt low_level pwb = 
      match Generic_extra_help.immediate_eval_opt (!(Instituted_extra_help.main_ref)) pwb  with 
      Some (answer) -> let (handle,mold) =answer in 
                       Some(handle,mold)    
    | None -> 
         (  
          match Common_table.get_opt pwb low_level with 
          Some (answer) -> let (handle,mold) =answer in 
                           Some(handle,mold)    
        | None -> None
           ) ;;    

  let eval_opt low_level pwb =  
    let (d,grounded_pwb) = Point_with_breadth.decompose_wrt_translation pwb in 
     match combined_eval_opt low_level grounded_pwb with 
      None -> None
     |Some(handle,mold) -> Some(Handle.translate d handle,Mold.translate d mold) ;; 

 
  module One_more_step = struct   

    let rec immediate_for_several_opt low_level (treated,to_be_treated) = 
      match to_be_treated with 
      [] -> Some(List.rev treated) 
      | pwb :: others ->
       (
        match eval_opt low_level pwb with 
          None -> None 
          |Some (_,mold) -> immediate_for_several_opt low_level (mold::treated,others)
       ) ;;
  
  
  
  let fork_opt low_level pwb =
    match Point_with_breadth.usual_decomposition_opt pwb with 
    None -> None
   |Some(prec_pwb,C cstr) ->
      match eval_opt low_level prec_pwb with 
      None -> None
    | Some(_,prec_mold) -> 
          let ext = Mold.forced_elements prec_mold in 
          let nth_cstr = (fun k->List.nth cstr (k-1)) in 
          let ijk=(nth_cstr 1,nth_cstr 2,nth_cstr 3) in 
          let (i,j,k) = ijk in 
          if not(i_is_included_in [i;j;k] ext)
          then None
          else
            let fgr = (!(Instituted_extra_help.main_ref)) in 
            let grooves = i_insert k (Help.extra_grooves fgr.helpers pwb) in 
                let pointed_pwbs = Image.image (Point_with_breadth.remove_element pwb) grooves in 
                (match immediate_for_several_opt low_level ([],pointed_pwbs) with 
                  None -> None
                | Some(pointed_molds) -> 
                   (
                    match Mold.fork_opt pwb prec_mold pointed_molds ijk with 
                       None -> None 
                      |Some mold -> Some(Fork(i,j,k),mold) 
                   )
                );;   
  
  
             
  let rightmost_overflow_opt low_level pwb  = 
   let n = Point_with_breadth.max pwb in 
   let left_pwb = Point_with_breadth.remove_element pwb n in 
   match eval_opt low_level left_pwb with 
       None -> None
     | Some(_,left_mold) -> 
    let left_ext = Mold.forced_elements left_mold 
    and complements = Point_with_breadth.complementary_pairs pwb in  
    match List.find_opt  (
                fun (u,v) -> i_is_included_in [u;v] left_ext 
      ) complements with
      None -> None 
     |Some(u,v) ->
        (
          match Mold.rightmost_overflow_opt pwb left_mold with 
             None -> None 
            |Some mold -> Some(Rightmost_overflow(u,v,n),mold) 
         )
      ;;
  
             
  
   let rightmost_pivot_opt low_level pwb  = 
     let n = Point_with_breadth.max pwb in 
     let left_pwb = Point_with_breadth.remove_element pwb n in 
     match eval_opt low_level left_pwb with 
         None -> None
       | Some(_,left_mold) -> 
       (match Mold.rightmost_pivot_opt pwb left_mold with 
         None -> None 
        |Some mold -> Some(Rightmost_pivot(Point_with_breadth.rightmost_largest_width pwb),mold) 
       )    ;;  
   
  let select_opt low_level pwb =
    match Point_with_breadth.usual_decomposition_opt pwb with 
    None -> None
   |Some(prec_pwb,C cstr) ->
      match eval_opt low_level prec_pwb with 
      None -> None
    | Some(_,prec_mold) -> 
               let nth_cstr = (fun k->List.nth cstr (k-1)) in 
               let ijk=(nth_cstr 1,nth_cstr 2,nth_cstr 3) in 
               let (i,j,k) = ijk in 
               (
                   match Mold.select_opt pwb prec_mold ijk with 
                      None -> None 
                     |Some mold -> Some(Select(i,j,k),mold) 
               );;        
  
  let one_more_step_opt low_level pwb =
    match eval_opt low_level pwb with 
    Some(answer0) -> Some answer0
   | None -> 
    if Point_with_breadth.has_no_constraint pwb 
    then Some(Has_no_constraints,Mold.discrete(Point_with_breadth.supporting_set pwb))
    else    
    (match rightmost_pivot_opt low_level pwb with 
     Some(answer1) -> Some answer1
    | None -> 
      (
        match rightmost_overflow_opt low_level pwb with 
          Some(answer2) -> Some answer2
        | None -> 
          (
            match select_opt low_level pwb with 
              Some(answer3) -> Some answer3
            | None -> fork_opt low_level pwb
          )     
      )
     ) ;;
      
    let opt_with_update low_level pwb =  
       match eval_opt low_level pwb with 
       Some pair1 -> (Some pair1,low_level) 
       | None -> 
        (
          match one_more_step_opt low_level pwb with 
         Some pair2 -> (Some pair2,Common_table.add_if_it_has_constraints low_level pwb pair2) 
       | None -> (None,low_level)
        ) ;;  
  
  


  end ;;

  end ;;  

  let eval_opt = Private.eval_opt ;; 
  let one_more_step_opt = Private.One_more_step.opt_with_update ;;

end ;;


module Raw_computer = struct 

  exception Push_exn ;; 

  exception Should_never_happen_in_push_1_exn of point_with_breadth;; 

  exception First_problem of point_with_breadth ;; 

  module Private = struct
    let painstaking_ref = ref (CT[]) ;;

let pusher (low_level,to_be_treated) = match to_be_treated with 
   [] -> raise Push_exn 
  | pwb :: others ->
  let (opt_pair1,low_level1) = Impatiently.one_more_step_opt low_level pwb in 
  if opt_pair1<>None then (low_level1,others) else 
  let (nonisolated_pwb,isolated_elts) = Point_with_breadth.nonisolated_version pwb in 
  if isolated_elts<>[]
  then let (opt_pair6,low_level6) = Impatiently.one_more_step_opt low_level1 nonisolated_pwb in 
       if opt_pair6=None then (low_level6,(Point_with_breadth.projection nonisolated_pwb)::to_be_treated) else 
        let (_handle,nonisolated_mold) = Option.get opt_pair6 in
        let mold = Mold.add_isolated_set nonisolated_mold isolated_elts in 
       (Common_table.add low_level6 pwb (Rightmost_pivot(W 0),mold),others) 
  else
  let opt2 = Point_with_breadth.usual_decomposition_opt pwb in 
  if opt2=None then raise(Should_never_happen_in_push_1_exn(pwb)) else
  let (_,C cstr) = Option.get opt2 in 
  let nth_cstr = (fun k->List.nth cstr (k-1)) in 
  let (i,j,k)=(nth_cstr 1,nth_cstr 2,nth_cstr 3) in 
  let pwb_i = Point_with_breadth.remove_element pwb i 
  and pwb_j = Point_with_breadth.remove_element pwb j 
  and pwb_k = Point_with_breadth.remove_element pwb k in 
  let (opt_pair3,low_level3) = Impatiently.one_more_step_opt low_level1 pwb_i in 
  if opt_pair3=None then (low_level3,(Point_with_breadth.projection pwb_i)::to_be_treated) else
  let (_,mold_i) = Option.get opt_pair3 in 
  let (opt_pair4,low_level4) = Impatiently.one_more_step_opt low_level3 pwb_j in 
  if opt_pair4=None then (low_level4,(Point_with_breadth.projection pwb_j)::to_be_treated) else
  let (_,mold_j) = Option.get opt_pair4 in 
  let (opt_pair5,low_level5) = Impatiently.one_more_step_opt low_level4 pwb_k in 
  if opt_pair5=None then (low_level5,(Point_with_breadth.projection pwb_k)::to_be_treated) else
  let (_,mold_k) = Option.get opt_pair5 in  
  let candidates = il_fold_merge(Image.image Mold.solutions [mold_i;mold_j;mold_k]) in 
  let (_,final_sols) = Max.maximize_it_with_care List.length candidates in 
  let answer=(Fork(i,j,k),Mold.shallow final_sols) in
  (Common_table.add  low_level5 pwb answer,others) ;;

let rec iterator (low_level,to_be_treated) =
    if to_be_treated = [] 
    then low_level
    else iterator(pusher (low_level,to_be_treated)) ;;
    
let eval_in_unblocked_mode  pwb =
    let new_low_level = iterator (!painstaking_ref,[pwb]) in 
    let _ = (painstaking_ref:=new_low_level) in 
    Option.get(Impatiently.eval_opt new_low_level pwb);;    

  let eval_in_blocked_mode pwb = 
    let (low_level,to_be_treated) = pusher (!painstaking_ref,[pwb]) in
    if to_be_treated=[]
    then  Option.get(Impatiently.eval_opt (low_level) pwb)
    else  raise(First_problem(List.hd to_be_treated)) ;; 

  let blocked_mode_ref = ref false ;;    

  let eval pwb =
     if !blocked_mode_ref 
     then eval_in_blocked_mode pwb
     else eval_in_unblocked_mode pwb ;;    

  let store data = 
       (painstaking_ref:=Common_table.add_several (!painstaking_ref) data);;

    

  end ;;

  let current_mode () = (!(Private.blocked_mode_ref)) ;;
  let eval = Private.eval  ;; 
  let measure pwb = let (_,mold) = eval pwb in List.length(List.hd (Mold.solutions mold)) ;; 
  let set_blocked_mode mode = (Private.blocked_mode_ref:=mode) ;; 
  let store = Private.store ;; 
      

end ;;


module Computer_for_extra_constraints = struct
  
  type paint_with_extra_constraints = PWEC of point * constraint_t list ;;
  
  module Private = struct

  let usual_decomposition_for_bare_point_opt_for_pwc pt =
    match Point.highest_constraint_opt pt with 
     None -> None 
    |Some (C l)-> 
      let current_b = List.nth l 0 in 
      let effective_w=(List.nth l 1)-current_b in 
      let candidates=Int_range.descending_scale (
          fun b->C[b;b+effective_w;b+2*effective_w]
      ) 1 (current_b-1) in 
      let (P(fis,_)) = pt in 
      let domain = Finite_int_set.to_usual_int_list fis in 
      let selected_candidates = List.filter (
         fun (C l)->i_is_included_in l domain
      ) candidates in 
      Some(PWEC(P(fis,W(effective_w-1)),selected_candidates), C l)
      ;;  
  
    let usual_decomposition_opt_for_pwc (PWEC(pt,l_cstr)) = 
        match l_cstr with 
      [] -> usual_decomposition_for_bare_point_opt_for_pwc pt 
    |highest :: others -> Some(PWEC(pt,others),highest) ;; 
  
  
  let pwc_has_no_constraint pwc = ((usual_decomposition_opt_for_pwc pwc)=None);;
  
  
  let remove_element_on_pwc (PWEC(pt,l_cstr)) t =
    let smaller_pt = Point.remove_element pt t in 
    PWEC(smaller_pt,List.filter (fun (C l)->not(i_mem t l)) l_cstr) ;; 
  
  let remove_rightmost_element_on_pwc pt_with_constraints =
    let (PWEC(pt,_)) = pt_with_constraints in 
    remove_element_on_pwc  pt_with_constraints (Point.max pt) ;; 
  
  let remove_rightmost_element_but_keep_constraints_on_pwc (PWEC(possibly_nonstandard_pt,l_cstr)) =
     let pt = Point.standardize possibly_nonstandard_pt in 
     let (W w) = Point.width pt and n=Point.max pt in 
     let smaller_pt = Point.remove_element pt n in
     let constraints1 = 
        (Int_range.descending_scale (fun d->[n-(2*d);n-d]) w 1)@
        (Image.image (fun (C l)->i_outsert n l) l_cstr) in
     let constraints2 = Ordered_misc.minimal_elts_wrt_inclusion constraints1 in 
     let (singletons,constraints3) = List.partition (fun cstr->List.length(cstr)=1) constraints2 in
     let removable_subset = List.flatten singletons in 
     let final_pt = Point.remove_elements smaller_pt removable_subset 
     and final_constraints = Image.image (fun l->C l) constraints3 in 
     PWEC(final_pt,final_constraints) ;;  
  
     let easy_measure_opt (PWEC(pt,l_cstr)) = 
       let (_,mold) = Raw_computer.eval (Point_with_breadth.all_constraints pt) in 
       let sols = Mold.solutions mold in 
       let selected_sols = Constraint.select_in_list l_cstr sols in 
       if selected_sols<>[]
       then Some(List.length(List.hd selected_sols))
       else None ;;


     let measure_for_pwc = Memoized.recursive (fun 
     old_f pwc-> 
       let (PWEC(pt,l_cstr)) = pwc in 
       if l_cstr = []
       then Raw_computer.measure(Point_with_breadth.all_constraints pt)
       else 
       let pwc2 = remove_rightmost_element_on_pwc pwc
       and pwc3 = remove_rightmost_element_but_keep_constraints_on_pwc pwc in 
       let m2 = (match easy_measure_opt pwc2 with Some m->m |None->old_f pwc2) 
       and m3 = (match easy_measure_opt pwc3 with Some m->m |None->old_f pwc3) in  
       max(m2)(m3+1)
     );;
  
  let standard_solution_for_pwc  = Memoized.recursive (fun 
    old_f pwc-> 
    let (PWEC(pt,_l_cstr)) = pwc in 
    if pwc_has_no_constraint pwc 
    then Point.supporting_set pt 
    else  
    let pwc2 = remove_rightmost_element_on_pwc pwc
    and pwc3 = remove_rightmost_element_but_keep_constraints_on_pwc pwc in 
    if (measure_for_pwc pwc2)>=((measure_for_pwc pwc3)+1)
    then old_f(pwc2)
    else (old_f(pwc3))@[Point.max pt]  
  );;
  
  let pwb_to_extra_constraints pwb = 
    let pt = Point_with_breadth.point pwb 
    and b = Point_with_breadth.breadth pwb in 
    if b = 0 then PWEC(pt,[]) else 
    let (W w)=Point.width pt 
    and domain = Point.supporting_set pt in 
    let all_constraints = Int_range.descending_scale 
       (fun k->C[k;k+(w+1);k+2*(w+1)]) b 1 in 
    let meaningful_constraints = List.filter(
      fun (C cstr) -> i_is_included_in cstr domain
    )  all_constraints in 
    PWEC(pt,meaningful_constraints) ;;


  end ;;


  let of_point_with_breadth = Private.pwb_to_extra_constraints ;;   
  let measure = Private.measure_for_pwc ;;  
  let remove = Private.remove_element_on_pwc ;; 
  let standard_solution = Private.standard_solution_for_pwc ;;  
  

  end ;;    

module Thorough_computer = struct 

module Private = struct   


  let standard_solution = Memoized.make(fun pwb->
    Computer_for_extra_constraints.standard_solution
       (Computer_for_extra_constraints.of_point_with_breadth pwb)
  )  ;;
  
  
module Decompose = struct 
  
  let test_for_individual_rightmost_overflow left_pwb m (u,v) = 
        List.for_all (fun t->Raw_computer.measure(Point_with_breadth.remove_element left_pwb t)=m-1) [u;v] ;;
    
  let test_for_rightmost_overflow pwb m =
        let pairs =  Point_with_breadth.complementary_pairs pwb 
        and left_pwb = Point_with_breadth.remove_element pwb (Point_with_breadth.max pwb) in 
        List.find_opt (test_for_individual_rightmost_overflow left_pwb m) pairs ;; 
  
  
  let decompose = Memoized.make(fun pwb->
    match Point_with_breadth.usual_decomposition_opt pwb with 
        None -> (Has_no_constraints,None)
        |Some(prec_pwb,C cstr) -> 
          let n = Point_with_breadth.max pwb in
          let left_pwb = Point_with_breadth.remove_element pwb n in 
          let pwc = Computer_for_extra_constraints.of_point_with_breadth pwb in  
          if Computer_for_extra_constraints.measure(Computer_for_extra_constraints.remove pwc n)=
            Computer_for_extra_constraints.measure(pwc)-1   
          then (Rightmost_pivot(Point_with_breadth.rightmost_largest_width pwb),
                Some left_pwb)
          else   
          let m = Raw_computer.measure pwb in   
            ( match test_for_rightmost_overflow pwb m with 
           (Some(u,v))->(Rightmost_overflow(u,v,n), Some left_pwb)
           |None ->   
         
         let nth = (fun k->List.nth cstr (k-1)) in 
         if Raw_computer.measure prec_pwb = m
         then 
              (Select(nth 1,nth 2,nth 3),Some prec_pwb)
         else (Fork(nth 1,nth 2,nth 3),Some prec_pwb)   
  ))  ;;     
  

   let rec helper_for_rail (treated,to_be_treated) =
      let (_handle,pwb_opt) = decompose to_be_treated in 
      let treated2 = to_be_treated::treated in 
      match pwb_opt with
      None -> List.rev treated2
      |Some pwb -> helper_for_rail (treated2,pwb) ;;
      
  let rail pwb = helper_for_rail ([],pwb) ;;    

  end ;;

end ;;

let decompose = Private.Decompose.decompose ;;
let rail = Private.Decompose.rail ;;
let standard_solution = Private.standard_solution ;; 

end ;;



  module Slow_motion = struct 

    exception Next_problem_in_decompose_exn ;;

    let next_problem_in_decompose pwb = 
      let old_mode = Raw_computer.current_mode() in
     try (fun _->
      Raw_computer.set_blocked_mode old_mode;
      raise Next_problem_in_decompose_exn)(
       let _ = Raw_computer.set_blocked_mode true in  
       Thorough_computer.decompose pwb) with 
     Raw_computer.First_problem(pwb2) -> 
      Raw_computer.set_blocked_mode old_mode;
      pwb2 ;;  

    exception Next_problem_in_eval_exn ;;

      let next_problem_in_eval pwb = 
        let old_mode = Raw_computer.current_mode() in
       try (fun _->
        Raw_computer.set_blocked_mode old_mode;
        raise Next_problem_in_eval_exn)(
          let _ = Raw_computer.set_blocked_mode true in    
        Raw_computer.eval pwb) with 
       Raw_computer.First_problem(pwb2) -> 
        Raw_computer.set_blocked_mode old_mode;
        pwb2 ;;     

  end ;;
  

  module Impatient_computer_on_rails = struct 

  exception Eval_exn of point_with_breadth ;;   

  module Private = struct 

  let impatient_on_chains_ref = ref (CT[]);; 

  let eval_opt pwb = 
      let (opt_answer,_new_low_level) 
              = Impatiently.one_more_step_opt (!impatient_on_chains_ref) pwb  in     
      match opt_answer  with 
      None -> None
    |Some (handle,mold) -> 
        let pair = (handle,mold) in 
        let _ = (impatient_on_chains_ref:=
        Common_table.add_if_it_has_constraints (!impatient_on_chains_ref) pwb pair;
        Raw_computer.store [pwb,pair]
        ) in 
        opt_answer ;;

  let eval pwb = match eval_opt pwb with 
    Some answer -> answer 
    |None -> raise(Eval_exn(pwb)) ;;    

  let predecessor_in_chain_opt pwb  = function 
       Has_no_constraints -> None 
      |Rightmost_overflow(_,_,_) 
      |Rightmost_pivot(W _) -> Some(Point_with_breadth.left pwb)
      |Select (_,_,_)
      |Fork (_,_,_) -> Point_with_breadth.predecessor_opt pwb ;; 


  let decompose pwb = 
    let (handle,_) = eval pwb in 
    (handle,predecessor_in_chain_opt pwb handle) ;;
     

  let compute_rail =Memoized.recursive(fun old_f pwb -> 
        let (_,prec_pwb_opt) = decompose pwb in 
        match prec_pwb_opt with 
         None -> [pwb]
        |Some prec_pwb ->
       (old_f prec_pwb)@[pwb] ) ;;  
  
  module Diagnose = struct 

    exception Nothing_to_diagnose_exn ;;
    exception Has_no_constraints_not_diagnosable_exn ;; 
    
    
      let diagnose_rightmost_overflow (u,v,_n)  left_pwb = 
         match eval_opt left_pwb with 
         None -> Missing_subcomputation("rightmost_overflow",left_pwb)
         |Some (_,mold) -> 
          let missing_forced_elts = i_setminus [u;v] (Mold.forced_elements mold) in 
          Missing_fan("rightmost_overflow",left_pwb,0,F[missing_forced_elts]) ;; 
    
     let diagnose_rightmost_pivot pwb left_pwb = 
      match eval_opt left_pwb with 
      None -> Missing_subcomputation("rightmost_pivot",left_pwb)
      |Some (_,_) ->
        let the_sol = Thorough_computer.standard_solution pwb 
        and n = Point_with_breadth.max pwb in
        Missing_solution("rightmost_pivot",left_pwb,i_outsert n the_sol) ;; 
    
      let diagnose_select pwb prec_pwb = 
        match eval_opt prec_pwb with 
      None -> Missing_subcomputation("select",prec_pwb)
      |Some (_,_) ->
          let the_sol = Thorough_computer.standard_solution pwb in
          Missing_solution("select",prec_pwb,the_sol) ;;  
    
      let diagnose_fork (i,j,k) pwb prec_pwb = 
        match eval_opt prec_pwb with 
         None -> Missing_subcomputation("fork",prec_pwb)
         |Some (_,prec_mold) -> 
        let missing_forced_elts = i_setminus [i;j;k] (Mold.forced_elements prec_mold) in 
        if missing_forced_elts <> []
        then Missing_fan("fork",prec_pwb,0,F[missing_forced_elts])   
        else   
        let the_sol = Thorough_computer.standard_solution pwb in 
        let l = List.find (fun t->not(i_mem t the_sol)) [k;j;i] in
        let shorter_pwb = Point_with_breadth.remove_element pwb l in 
        match  eval_opt shorter_pwb with 
         None -> Missing_subcomputation("fork",shorter_pwb)
         |Some (_,mold) -> 
           let sols = Mold.solutions mold in 
           if not(List.mem the_sol sols)
           then Missing_solution("fork",shorter_pwb,the_sol)
           else Missing_switch_in_fork(l,pwb) ;;  
          
    
    let diagnose_precedent pwb =
      let (handle,pwb2_opt) = decompose pwb in 
      let pwb2=(match pwb2_opt with 
         Some pwb3 -> pwb3
         | None -> Point_with_breadth.constructor 0 [] (W 0) 0) in   
        match handle with
         Has_no_constraints -> raise(Has_no_constraints_not_diagnosable_exn)
        |Rightmost_overflow(u,v,n) ->  diagnose_rightmost_overflow (u,v,n) pwb2
        |Rightmost_pivot(_) -> diagnose_rightmost_pivot pwb pwb2
        |Select (_,_,_) -> diagnose_select pwb pwb2 
        |Fork (i,j,k) -> diagnose_fork (i,j,k) pwb pwb2 ;;  
    
  

  end ;;  
  
  let ref_for_problem_during_overchain_declaration = ref None ;; 
    

  (* The overrails declared here are not exact rails but
     merely overrails, i.e. sequences [s(1);s(2);...] where
     for each i, the predecessor of s(i+1) is some s(j) for j<=i 
     (and not necessarily s(i) as it would be for an exact rail)

   *)

  let rec declare_overrail = function 
     [] -> ()
    |pwb :: others -> 
       match eval_opt pwb with 
        None -> 
           let diag = Diagnose.diagnose_precedent pwb in   
          (ref_for_problem_during_overchain_declaration:=Some(pwb,diag);
           print_string("An exception was raised.\n");
           print_string("Type Im"^"patient_computer_on_rails.current_diagnosis() for more details");
          )
        | _ -> declare_overrail others ;; 

  let ref_for_problem_during_iter_eval = ref None ;;

  let rec iter_eval = function 
    [] -> ()
   |pwb :: others ->
     (
      match eval_opt pwb with
       Some(_) -> iter_eval others 
      | None -> 
        (ref_for_problem_during_iter_eval:=Some(pwb);
           print_string("A blocker was encountered.\n");
           print_string("Type Im"^"patient_computer_on_rails.current_blocker() for more details");
          )
     ) ;;
      
  

  end ;;   

  let current_blocker () = Option.get(!(Private.ref_for_problem_during_iter_eval));;
  let current_diagnosis () = Option.get(!(Private.ref_for_problem_during_overchain_declaration));;
  let declare_overrail = Private.declare_overrail ;;
  let decompose = Private.decompose ;; 
  let eval = Private.eval ;; 
  let eval_opt = Private.eval_opt ;; 
  let iter_eval = Private.iter_eval ;; 
  let rail = Private.compute_rail ;;

  end ;; 
  
  
module Point_with_requirements = struct   

exception Pull_exn ;;   

module Private = struct

module Canonized_requirement = struct 

type t = CR of point_with_breadth * int * fan ;; 


let constructor pwb level_in_mold fan =
  let all_sols = Point_with_breadth.solutions pwb level_in_mold in 
  let improved_fan = snd(Fan.canonical_container all_sols fan) in 
  CR(pwb,level_in_mold,improved_fan);;


let pull_list handle source_pwb destination_pwb canonized_reqs = 
  let old_reqs = Image.image (fun (CR(_,level,fan)) -> (level,fan) ) canonized_reqs in
  let new_reqs = Fan.pull handle ~destination:destination_pwb old_reqs in 
  List.filter_map (
      fun (k,fan) ->
        if fan = Fan.empty_one
        then None   
        else Some(constructor source_pwb k fan)     
    ) new_reqs  ;;

let list_of_pwr (PWR(destination_pwb,reqs))= 
  Image.image (fun (level,fans)->CR(destination_pwb,level,fans)) reqs  ;;

exception List_to_pwr_exn ;;

let list_to_pwr common_pwb l = 
  PWR(common_pwb,Image.image (fun (CR(_,level_in_mold,fan))->(level_in_mold,fan)) l) ;;  
 
end ;;   



let pull destination_pwr =
  let (PWR(destination_pwb,_))=  destination_pwr in 
  let (handle,source_pwb_opt) = Thorough_computer.decompose destination_pwb in 
  match source_pwb_opt with 
  None -> raise Pull_exn
  |Some source_pwb ->
  let destination_cr_list = Canonized_requirement.list_of_pwr destination_pwr in 
  let source_cr_list =  Canonized_requirement.pull_list handle source_pwb destination_pwb destination_cr_list in 
  Canonized_requirement.list_to_pwr source_pwb source_cr_list ;;


end ;; 

let constructor pwb = PWR(pwb,[]) ;; 
let pull = Private.pull ;; 

end ;;


module Initialize_overchains = struct 

module Private = struct 
  
  let all_constraints n scr w = Point_with_breadth.all_constraints(P(FIS(n,scr),W w)) ;;
  let level3 n = all_constraints n [] 3 ;;
  
  let special1 q = 
     if q=0 
     then all_constraints 7 [] 2 
     else Point_with_breadth.constructor (8*q+7) [] (W 2) (8*q) ;;

  let chunk q = 
    (Int_range.scale level3 (8*q+1) (8*q+6))@
    [special1 q;level3 (8*q+7)]@
    [Point_with_breadth.constructor (8*q+8) [] (W 2) (8*q+1);level3 (8*q+8)]  ;; 


  let data1 = List.concat_map chunk (Int_range.range 0 4);; 


  let data = [data1] ;;
  
end ;;   



List.iter Impatient_computer_on_rails.declare_overrail Private.data ;;



end ;;   


end ;;


end ;;


(************************************************************************************************************************
Snippet 116 : Code to adapt copy-pasted text to LaTeX
************************************************************************************************************************)
module Snip116=struct


  let ap1 = Absolute_path.of_string 
  (home^"/Downloads/Human_Booster/Dossier_projet/LaTeX/dossier_projet.tex");; 
  
  let text_ref = ref(Io.read_whole_file ap1);;
  
  let replacements = ref [] ;; 
  
  let ur () = (text_ref:=(Io.read_whole_file ap1)) ;;
  let txt () = (!text_ref) ;;
  let rep () = (
    Replace_inside.replace_several_inside_file (!replacements) ap1;
    ur()
  );;
  let sr reps = 
     (
      replacements:=reps;
      rep()
     ) ;; 
  let ch line_nbr = Strung.explode(List.nth (Lines_in_text.lines(txt())) (line_nbr-1)) ;; 
  
  sr [
      ("E\204\129","\195\137");
      ("a\204\128","\195\160");
      ("a\204\130","\195\162");
      ("c\204\167","\195\167");
      ("e\204\128","\195\168");
      ("e\204\129","\195\169");
      ("e\204\130","\195\170");
      ("o\204\130","\195\180");
      ("u\204\128","\195\185");
  ] ;; 
  
  
     
  let pattern = "dans la mesure " ;;
  
  let g1 = txt () ;;
  let occs =  Substring.occurrences_of_in pattern g1;; 
  let n1 = (List.hd occs) + (String.length pattern) ;; 
  let g2 = Cull_string.interval g1 n1 (n1+40) ;; 
  let g3 = Strung.explode g2 ;; 
  
  
  (*
  let ap2 = Absolute_path.of_string 
  (home^"/Teuliou/LaTeX/Brouilhedou/anse.tex");; 
  let text2=Io.read_whole_file ap2 ;; 
  let lines2 = Lines_in_text.indexed_lines text2 ;; 
  let the_line2 = List.assoc 18 lines2 ;; 
  
  let peggy1 i = let si = string_of_int i in "("^si^",\"\\195\\"^si^"\")" ;; 
  let text1 = "\n\n\nlet amy = [" ^ (String.concat ";" (Int_range.scale peggy1 100 200) )^ "]\n\n\n";;
  let act () = print_string text1 ;; 
  
  *)

end ;;


(************************************************************************************************************************
Snippet 115 : Parsing a YAML file
************************************************************************************************************************)
module Snip115=struct

  let ap1 = Absolute_path.of_string (home^"/Downloads/api-v2.txt") ;; 
let u1 = Io.read_whole_file ap1 ;; 
let lines1 = Lines_in_text.indexed_lines u1 ;; 

let the_line = List.assoc 2048 lines1 ;; 



let left_measure s = String.length(s) - String.length(Cull_string.trim_spaces_on_the_left s) ;; 
let the_measure = left_measure the_line ;; 

let lines2 = List.filter (fun (idx,s)->(idx<2048)&&((left_measure s)<the_measure)) lines1 ;;



end ;;


(************************************************************************************************************************
Snippet 114 : Parsing a JSON file with short lines
************************************************************************************************************************)
module Snip114=struct

  let ap1 = Absolute_path.of_string 
  "~/Teuliou/Sites/Angular/External/original_orders.txt" ;; 

let u1 = Io.read_whole_file ap1 ;;  
let lines1 = Lines_in_text.lines u1 ;; 

let improve_line line =
   if not(String.contains line ':')
   then line
   else 
   let b = Option.get(Substring.leftmost_index_of_in_from_opt ":" line 1) in
   let before = Cull_string.beginning (b-1) line
   and after = Cull_string.cobeginning (b-1) line in
   let i= Option.get(Substring.leftmost_index_of_in_from_opt "\"" before 1)
   and j = Option.get(Substring.rightmost_index_of_in_opt "\"" before) in
   let name = Cull_string.interval before (i+1) (j-1) in 
   name^after ;;

let lines2 = Image.image improve_line lines1 ;; 
let res1 = String.concat "\n" lines2 ;;    

let ap2 = Absolute_path.of_string 
  "~/Teuliou/Sites/Angular/External/all_orders.txt" ;; 

let act () = Io.overwrite_with ap2 res1 ;; 

end ;;


(************************************************************************************************************************
Snippet 113 : Parsing a JSON file with long lines
************************************************************************************************************************)
module Snip113=struct

  let ap1 = Absolute_path.of_string 
  "~/Teuliou/Sites/Angular/External/original_products.txt" ;; 

let u1 = Io.read_whole_file ap1 ;;   
let u2 = Substring.occurrences_of_in "{" u1 ;; 
let u3 = Substring.occurrences_of_in "}" u1 ;; 
let ranges = List.combine u2 u3 ;; 
let u4 = Image.image (fun (a,b)->Cull_string.interval u1 a b) ranges ;; 

let fields = [
  "\"productID\":";
  "\"name\":";
  "\"productType\":";
  "\"unitPrice\":";
  "\"description\":";
  "\"stockpiled\":"
] ;;

let analize1 item =
    let ranges = Image.image (fun field->
      let i =List.hd(Substring.occurrences_of_in field item) in 
      (field,i,i+(String.length field)-1)
      ) fields  in 
     let indexed_ranges = Int_range.index_everything ranges 
     and m = List.length ranges in
     let temp1 = Image.image (fun (idx,(field,a,b))->
        if idx = m
        then (field,a,b,String.length item)
        else let (_,a2,b2) = List.nth ranges idx in
             (field,a,b,a2-1)    
     ) indexed_ranges in 
     let temp2 = Image.image (fun
     (field,a,b,c) -> 
       (Cull_string.two_sided_cutting ("\"","\":") field,
        Cull_string.interval item (b+1) c) 
     ) temp1 in
     temp2 ;; 
let u5 = Image.image analize1 u4 ;; 

let analize2 = Image.image ( fun (field,content) ->
   let i= Option.get(Substring.leftmost_index_of_in_from_opt "\n" content 1) in 
   let content2= Cull_string.beginning (i-1) content  in
   let content3=(
     if String.ends_with ~suffix:"," content2
     then Cull_string.coending 1 content2
     else content2  
   ) in
   (field,content3) );; 

let u6 = Image.image analize2 u5;;

let analize3 = Image.image ( fun pair ->
  let (field,content) = pair in 
  if field<> "productType" 
  then pair
  else
      let i= Option.get(Substring.leftmost_index_of_in_from_opt "\"" content  1)
      and j = Option.get(Substring.rightmost_index_of_in_opt "\"" content) in
      let name = Cull_string.interval content (i+1) (j-1) in
      (field," ProductType."^(String.capitalize_ascii name)) 
  );; 

let u7 = Image.image analize3 u6;;

let write_pair (field,content) = field^":"^content ;;   

let write_item pairs =
   "{\n"^(String.concat ",\n" (Image.image write_pair pairs))^"\n}" ;;

let write_items items =
    "\n\n\n[\n"^(String.concat ",\n" (Image.image write_item items))^"\n]\n\n\n" ;;   

let res1 = write_items u7 ;;    

let ap2 = Absolute_path.of_string 
  "~/Teuliou/Sites/Angular/External/all_products.txt" ;; 

let act () = Io.overwrite_with ap2 res1 ;; 


let v1 = List.hd u6 ;; 

let see = analize3 v1;;


end ;;


(************************************************************************************************************************
Snippet 112 : Code used for a Kotlin project
************************************************************************************************************************)
module Snip112=struct

  let dir1 = Directory_name.of_string "~/Downloads/Some_kotlin_files" ;;
let all_files = Unix_again.complete_ls dir1 ;; 
let u1 = Image.image Absolute_path.to_string all_files ;; 
let u2 = List.filter (fun s->String.ends_with ~suffix:".kt" s ) u1;;
let file1 = List.find (fun s->String.ends_with ~suffix:"ExampleJson2KtKotlin.kt" s) u2 ;;
let file2 = List.find (fun s->String.ends_with ~suffix:"Items.kt" s ) u2 ;;
let extracted_files = [file2;file1] ;; 
let reordered_u2 = 
    (List.filter (fun x->not(List.mem x extracted_files)) u2) @ (extracted_files) ;; 
let u3 = Image.image rf reordered_u2 ;;
let u4 = String.concat "\n\n\n(* aaa *)" u3;;
let u5 = Replace_inside.replace_inside_text
   ("\n\n(* aaa *)package com.example.example\n\nimport com.google.gson.annotations.SerializedName",
   "") u4;;
let whole_ap = Absolute_path.of_string "~/Downloads/Some_kotlin_files/whole.txt" ;;
Io.overwrite_with whole_ap u5 ;; 


let select_files sub_path ending =
  let full_path = "~/Downloads/main/"^sub_path in 
  let dir1 = Directory_name.of_string full_path in  
  let all_files = Unix_again.complete_ls dir1 in 
  let temp1 = Image.image (fun ap->(ap,Absolute_path.to_string ap)) all_files in
  List.filter (fun (ap,s)->String.ends_with ~suffix:ending s) temp1 ;;

let g1 = select_files "java/com/example/animeapplication"  ".kt" ;;  
let g2 = select_files "res/layout"  ".xml" ;; 
let g3 = List.filter (
   fun (ap,s) ->
     let text = Io.read_whole_file ap in 
     Substring.is_a_substring_of "hoto" text 
) (g1 @ g2);; 

let g4 = Image.image snd (g1 @ g2) ;;
let g5 = Detect_inside.occurrences_for_several_words_in_several_files 
   ["anga"] g4 ;; 

let g4 = rf "/Users/ewandelanoy/Downloads/main/java/com/example/animeapplication/MangaViewHolder.kt" ;;

Substring.occurrences_of_in "hoto" g4 ;; 
  
let g5 = Cull_string.interval g4 750 1000 ;; 

let z5 = Image.image (fun (ap,s)->Io.read_whole_file ap) [] ;; 
let z6 = String.concat "\n\n\n" z5 ;; 
let ap2 = Absolute_path.of_string "~/Downloads/text.txt";;
Io.overwrite_with ap2 z6 ;; 



end ;;


(************************************************************************************************************************
Snippet 111 : Bijections with finite difference set
************************************************************************************************************************)
module Snip111=struct

  let i_order = Total_ordering.for_integers ;;
  let i_insert = Ordered.insert i_order ;;
  let i_mem = Ordered.mem i_order ;;
  let i_sort = Ordered.sort i_order ;;
  
  type state = S of int * (int list) * (int list);;
  type larger_state = LS of state * ((int list) list) ;; 
  
  let u1 = [6;10;15] ;; 
  let u2 = (List.rev_map (fun x->(-x)) u1) @ u1 ;; 
  
  let insert_opt new_elt (S(a,others,diffs)) =
      if (new_elt<0) || (new_elt=a) || (i_mem new_elt others)
      then None
      else Some(S(new_elt,i_insert a others,(new_elt-a)::diffs)) ;;
      
  let descendants_for_one state =
    let (S(a,others,diffs)) = state in 
    List.filter_map (fun t->insert_opt (a+t) state) u2 ;; 
  
  let descendants_for_several states = List.flatten (Image.image descendants_for_one states) ;;
  
  let rec quest states goal =
     match List.find_opt (fun (S(a,others,diffs))->a=goal) states with 
     Some solution -> solution
     | None -> let new_states = descendants_for_several states in 
               quest new_states goal ;; 
  
  
  
  let rec iterator_for_next_goal i l = match l with
    [] -> i
    | j :: others -> if j=i then iterator_for_next_goal (i+1) others else i ;; 
  
  let next_goal (S(new_elt,others,diffs)) = iterator_for_next_goal 0 (i_insert new_elt others) ;; 
  
  let push_state st = quest [st] (next_goal st) ;; 
  
  let push_larger_state (LS(old_state,addenda)) =
      let new_state = push_state old_state in 
      let (S(_,_,old_diffs)) = old_state 
      and (S(_,_,new_diffs)) = new_state in 
      let d= List.length(new_diffs)-List.length(old_diffs) in 
      LS(new_state,(List_again.long_head d new_diffs) :: addenda);;    
  
  let initial_state = S(0,[],[]) ;;   
  let initial_large_state = LS(initial_state,[]);;  
  
  let ff = Memoized.small push_larger_state initial_large_state ;; 
  
  
  let g1 = descendants_for_several [initial_state] ;;
  let g2 = next_goal initial_state ;; 
  
  let g3 = ff 132 ;; 
  let (LS(_,g4)) = g3 ;; 
  let g5 = Image.image List.length g4 ;;
  
  let nth k = List.nth g4 (k-1) ;; 
  let uu k = (nth(k+1),nth(k+2),nth(k+3),nth(k+4)) ;; 
  let n4 = List.length g4 ;; 
  
  let g5 = List.filter (fun t->if t>n4-1 then false else nth(t+1)=nth(1)) (Int_range.range 1 n4) ;; 
  let g6 = List.filter (fun t->if t>n4-2 then false else nth(t+2)=nth(2)) g5 ;; 
  let g7 = List.filter (fun t->if t>n4-3 then false else nth(t+3)=nth(3)) g6 ;; 


end ;;


(************************************************************************************************************************
Snippet 110 : For sudoku study 
************************************************************************************************************************)
module Snip110=struct

  type cell = C of int * int ;;

  type box =
      Row of int
     |Column of int 
     |Square of int ;; 
     
  type inverse = IV of box * int ;; 
  
  type deductor =
       Direct_ded of cell
      |Inverse_ded of inverse 
      |Inverse_for_inverse_ded of inverse * inverse 
      |Inverse_for_direct_ded of inverse * cell;; 
  
  type cell_state =
      Initialized of int 
     |Assumed of int 
     |Deduced of cell * int * deductor
     |Usual of (cell option) list ;;
  
  type bare_grid = BG of cell_state list ;; 
  
  type minimizer_data = MD of int * (deductor * (cell * int) list) list ;; 
  
  type grid_with_deductions = GWD of bare_grid * (minimizer_data option) ;;
  
  let i_order = Total_ordering.for_integers ;;
  let i_fold_merge = Ordered.fold_merge i_order ;;
  
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
  let fold_merge ll =
      let temp1 = Image.image (Image.image single_index) ll in 
      Image.image at_single_index (i_fold_merge temp1) ;; 
  
  let possibilities  l= 
     let temp1 = Int_range.index_everything l in 
     List.filter_map (
       fun (v,opt)-> match opt with 
        None -> Some v 
       |Some _ -> None
     ) temp1 ;;      
  
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
  
  
  module Inverse = struct 
  
  let to_short_string (IV(box,v))="IV("^(Box.to_short_string box)^","^(string_of_int v)^")";; 
  
  let all = 
    let base = Cartesian.product Box.all (Int_range.range 1 9) in 
    Image.image (fun (box,k)->IV(box,k)) base;;  
  
  end ;;  
  
  module Deductor = struct 
  
  let to_cell_opt = function 
       Direct_ded (cell)-> Some cell
      |Inverse_ded (_) 
      |Inverse_for_inverse_ded(_,_) 
      |Inverse_for_direct_ded(_,_) -> None;; 
  
  let atomic_to_short_string = function
      Direct_ded (cell)-> (Cell.to_short_string cell)
    |Inverse_ded (iv) -> (Inverse.to_short_string iv)
    |Inverse_for_inverse_ded(_,_)
    |Inverse_for_direct_ded(_,_) -> "...";; 
  
  
  let to_short_string = function
  
      Direct_ded (cell)-> "Direct_ded("^(Cell.to_short_string cell)^")"
    |Inverse_ded (iv) -> "Inverse_ded("^(Inverse.to_short_string iv)^")"
    |Inverse_for_inverse_ded(iv1,iv2) ->  
      "Inverse_for_inverse_ded("^(Inverse.to_short_string iv1)^","^(Inverse.to_short_string iv2)^")" 
    |Inverse_for_direct_ded(iv,cell) ->  
        "Inverse_for_direct_ded("^(Inverse.to_short_string iv)^","^(Cell.to_short_string cell)^")";; 
  
  let all_atomic_deductors =
          (Image.image (fun cell->Direct_ded(cell)) Cell.all) 
          @ (Image.image (fun iv->Inverse_ded(iv)) Inverse.all) ;;
      
      
  
  end ;;  
  
  
  module Cell_state = struct 
  
    let display = function
      Initialized(v)->string_of_int v
     |Assumed(v)->string_of_int v
     |Deduced(_,v,_)->string_of_int v
     |Usual(_)->" ";;
  
    let new_state v0 acting_cell state =   
      match state with 
      Initialized _
     |Assumed _ 
     |Deduced(_,_,_)-> state 
     |Usual(l) ->
        let indexed_l = Int_range.index_everything l in 
        Usual(Image.image (
          fun (v1,opt1)->
            if v1<>v0 
            then opt1
            else match opt1 with 
                 Some(_)->opt1
                |None -> Some acting_cell 
        ) indexed_l);;
  
    let possibilities = function 
     Initialized(v2)->[v2]
    |Assumed(v3)->[v3]
    |Deduced(_,v4,_)->[v4]
    |Usual(l)-> Cell.possibilities l ;;  
  
    let to_direct_deduction_opt cell0 = function 
       Initialized(_)
      |Assumed(_)
      |Deduced(_,_,_)-> None
      |Usual(l)->  
        let temp1 = Int_range.index_everything l in 
      let poss=List.filter (fun (_v,opt)-> opt = None) temp1 in 
      if List.length(poss)<>1
      then None
      else 
      let v0=fst(List.hd poss) in 
      Some(cell0,v0);; 
  
    
     let is_yet_undecided = function 
     Initialized(_)
    |Assumed(_)
    |Deduced(_,_,_)-> false
    |Usual(_)->  true;;
  
  end ;;  
  
  module Bare_grid = struct 
  
    module Private = struct 
  
    let constructor l= BG l ;; 
    let states_in_bare_grid (BG l) = l;;
  
    let possibilities_for_cell bg cell= 
      let states= states_in_bare_grid bg in 
       let idx = Cell.single_index cell in 
       Cell_state.possibilities (List.nth states (idx-1));;
  
     let check_before_assignment bg cell v = 
       let possible_values = possibilities_for_cell bg cell in 
       if not(List.mem v  possible_values)
       then let msg = "Incorrect assignment attempt at "^(Cell.to_short_string cell) in 
            let _ = (print_string msg;flush stdout) in 
            false
       else true ;;   
  
     let assign_and_update bg cell0 v0 explanation0 = 
         if not(check_before_assignment bg cell0 v0) 
         then bg
         else
         let old_states = states_in_bare_grid bg in 
         let indexed_old_states = Int_range.index_everything old_states in 
         let new_states = Image.image (
           fun (idx,state) ->
              let cell = Cell.at_single_index idx in 
              if cell=cell0
              then explanation0
              else  
              if not(Cell.test_for_neighborhood cell0 cell)
              then state
              else Cell_state.new_state v0 cell state 
         ) indexed_old_states in
         constructor new_states ;; 
     
      let compute_easy_deductions bg = 
        let base= List.combine Cell.all (states_in_bare_grid bg) in 
         List.filter_map (fun (cell,state)->Cell_state.to_direct_deduction_opt cell state) base ;; 
  
          module Display = struct 
  
            let eval_small_grid_using_matrix_coordinates bg special_cells (i,j) = 
               let states = states_in_bare_grid bg in 
               let cell = Cell.from_matrix_coordinates i j in 
               if List.mem cell special_cells 
               then "#"
               else 
               let idx = Cell.single_index (Cell.from_matrix_coordinates i j) in 
               Cell_state.display(List.nth states (idx-1));;
            
            let eval_large_grid_using_matrix bg special_cells large_i large_j =
                let small_i =  List_again.find_index_of_in large_i [2;3;4;6;7;8;10;11;12]
                and small_j =  List_again.find_index_of_in large_j [2;3;4;6;7;8;10;11;12] in 
            if (small_i<0)||(small_j<0)
            then "*"
            else eval_small_grid_using_matrix_coordinates bg special_cells (small_i,small_j);;
            
            let large_line bg special_cells large_i = String.concat "" 
              (Int_range.scale(eval_large_grid_using_matrix bg special_cells  large_i) 1 13) ;;
              
            let large_lines bg special_cells  = Int_range.scale (large_line bg special_cells ) 1 13 ;;
        
            let large_grid bg special_cells  = (String.concat "\n" (large_lines bg special_cells ));;  
        
          end ;;  
  
        let to_string bg special_cells = (Display.large_grid bg special_cells);;    
  
        let origin = 
            let common = Usual (Int_range.scale (fun _->None) 1 9) in 
            constructor(Int_range.scale (fun _->common) 1 81);;
  
        let get_state bg cell = 
           List.nth (states_in_bare_grid bg) ((Cell.single_index cell)-1);;   
  
      end ;;  
  
      let assign_and_update= Private.assign_and_update ;; 
      let compute_easy_deductions = Private.compute_easy_deductions ;; 
      let get_state = Private.get_state ;; 
      let origin = Private.origin ;; 
      let states_in_bare_grid = Private.states_in_bare_grid ;;
      let to_string = Private.to_string ;; 
  
  end ;;   
  
  module Minimizer_data = struct 
  
    let immediate_deductions (MD(m,messengers)) = 
      if m<>1 then [] else 
      let temp1 = Image.image (fun (ded,l)->
         let (cell,v)=List.hd l in
           (cell,(v,ded))
         ) messengers in 
      List.filter_map (fun cell->
         match List.assoc_opt cell temp1 with 
          None -> None
         |Some (v,ded) -> Some(cell,v,ded)  
      ) Cell.all ;;   
  
    let contradictions (MD(m,messengers)) = 
      if m<>0 then [] else 
      Image.image fst messengers;;  
  
    let minimizing_cells (MD(_m,messengers)) =
       List.filter_map (
          fun (deductor,data)->
             match Deductor.to_cell_opt deductor with 
              None -> None
             |Some(cell)->Some(cell,Image.image snd data)
       ) messengers ;;  
  
  end ;;  
  
  
  module Possibilities = struct 
  
    module Private = struct 
  
    let for_cell bg cell= 
      let states= Bare_grid.states_in_bare_grid bg in 
       let idx = Cell.single_index cell in 
       Cell_state.possibilities (List.nth states (idx-1));;
  
    let for_inverse bg (IV(box,v)) = 
        List.filter (fun cell->List.mem v (for_cell bg cell)) ( Box.content box) ;;
  
    let analysis_for_double_inverse bg iv1 iv2 = 
          let (IV(_box1,v1)) = iv1 in 
          let cases = Image.image (
            fun cell -> 
              let new_bg = Bare_grid.assign_and_update bg cell v1 (Assumed v1) in 
              (cell,for_inverse new_bg iv2)
          ) (for_inverse bg iv1) in 
      (Cell.fold_merge (Image.image snd cases),cases);;
  
    let for_double_inverse bg iv1 iv2 = fst(analysis_for_double_inverse bg iv1 iv2) ;; 
  
    let analysis_for_ifd bg iv1 cell2 = 
      let (IV(_box1,v1)) = iv1 in 
      let cases = Image.image (
        fun cell -> 
          let new_bg = Bare_grid.assign_and_update bg cell v1 (Assumed v1) in 
          (cell,for_cell new_bg cell2)
      ) (for_inverse bg iv1) in 
    (i_fold_merge (Image.image snd cases),cases);;
  
    let for_ifd bg iv1 cell2 = fst(analysis_for_ifd bg iv1 cell2) ;;
  
    let for_deductor bg = function 
      Direct_ded (cell) -> Image.image (fun v->(cell,v)) (for_cell bg cell)
    |Inverse_ded (iv) -> let (IV(_,v))=iv in Image.image (fun cell->(cell,v)) (for_inverse bg iv) 
    |Inverse_for_inverse_ded (iv1,iv2) -> 
      let (IV(_,v2))=iv2 in Image.image (fun cell->(cell,v2)) (for_double_inverse bg iv1 iv2)
    |Inverse_for_direct_ded (iv1,cell2) ->  
      Image.image (fun v2->(cell2,v2)) (for_ifd bg iv1 cell2)
    ;; 
  
    let minimizers bg =
         let temp1 =List.filter_map (
             fun ded ->
              let l=for_deductor bg ded in 
              if List.length(l)<>1
              then Some (ded,l)
              else let (cell,_v) = List.hd l in 
                   if Cell_state.is_yet_undecided(Bare_grid.get_state bg cell)
                   then Some (ded,l)
                   else None
         ) Deductor.all_atomic_deductors in 
         let (m,l)=Min.minimize_it_with_care (fun (_,l)->List.length l) temp1 in
        MD(m,l) ;; 
          
  
  
    end ;;
    
    let for_cell = Private.for_cell ;;
    let for_deductor = Private.for_deductor ;; 
    let minimizers = Private.minimizers ;; 
  
  end ;;  
  
  
  module Grid = struct 
  
      exception Minimizers_exn ;;
  
      module Private = struct 
  
      let grid (GWD(bg,_)) = bg ;;
      let minimizers_opt (GWD(_,md_opt)) = md_opt ;;
      let constructor bg = GWD(bg,None) ;;
  
      let compute_minimizers_if_necessary gwd =
           let (GWD(bg,md_opt)) = gwd in 
            match md_opt with 
            Some(_)->gwd 
           |None ->
             let md=Possibilities.minimizers bg in 
             GWD(bg,Some md) ;;
  
        let minimizers (GWD(_,md_opt)) = match md_opt with 
            None -> raise(Minimizers_exn)
           |Some md -> md ;;
        
        let assign_and_update gwd cell0 v0 explanation0= 
          let old_grid = grid gwd in
          let new_grid=Bare_grid.assign_and_update old_grid cell0 v0 explanation0  in 
          constructor new_grid ;; 
  
  
        let initialize_single_cell bg cell0 v0 = assign_and_update bg cell0 v0 (Initialized v0) ;;
  
        let assume bg cell0 v0 =assign_and_update bg cell0 v0 (Assumed v0) ;;      
  
  
        module Display = struct 
  
          let deduction_to_string (cell,v0,_) = (Cell.to_short_string(cell))^" -> "^(string_of_int v0) ;;
          let deductions_to_string l = String.concat " , " (Image.image deduction_to_string l) ;; 
          let breakdowns_to_string cells =
             if cells = [] then "" else 
             "\n Contradictions : "^(String.concat "," (Image.image Deductor.atomic_to_short_string cells)) ;;    
  
          let minimized_data_to_string md =
              (deductions_to_string(Minimizer_data.immediate_deductions md))^
              (breakdowns_to_string(Minimizer_data.contradictions md));;   
  
          let minimized_data_opt_to_string = function 
            None -> "Minimizers have not been computed for this grid." 
            |Some(md)-> minimized_data_to_string md ;;   
  
        end ;; 
              
  
  
        let to_string gwd special_cells = (Bare_grid.to_string (grid gwd) special_cells)^"\n\n"^
               (Display.minimized_data_opt_to_string(minimizers_opt gwd));;   
        
        let to_surrounded_string gwd special_cells = "\n\n\n"^(to_string gwd special_cells)^"\n\n\n" ;;  
  
        let hidden_print_out (fmt:Format.formatter) bg=
        Format.fprintf fmt "@[%s@]" (to_surrounded_string bg []);;
  
        let origin = constructor  Bare_grid.origin  ;; 
          
  
        let initialize_with l =
           let temp1 = Int_range.index_everything l in 
           let temp2 = List.filter(fun (_idx,v)->(v>=1)&&(v<=9)) temp1
           and walker = ref origin in 
           let apply=(fun (idx,v)->
              let cell = Cell.at_single_index idx in
              walker:=initialize_single_cell (!walker) cell v
            ) in 
           let _ = List.iter apply temp2 in 
           !walker ;;  
           
      let fail_during_deduction gwd ded = 
          let msg = "Incorrect deduction attempt. "^(Deductor.to_short_string ded) in 
          let _ = (print_string msg;flush stdout) in 
          gwd;;   
    
      let deduce gwd ded =
        let old_grid = grid gwd  in 
        let poss = Possibilities.for_deductor old_grid ded  in 
        if List.length(poss)<>1
        then fail_during_deduction gwd ded 
        else 
        let (cell0,v0)=List.hd poss in 
        assign_and_update gwd cell0 v0 (Deduced(cell0,v0,Direct_ded(cell0)))  ;;
      
      let deduce_several gwd deds = List.fold_left deduce gwd deds ;;   
      
      let deduce_several_directly gwd cells =
          let deds = Image.image (fun cell->Direct_ded cell) cells in 
          deduce_several gwd deds ;;
       
      let get_state gwd cell = Bare_grid.get_state (grid gwd) cell ;; 
  
      let immediate_deductions gwd =  Minimizer_data.immediate_deductions (minimizers gwd) ;;
  
      let make_all_immediate_deductions gwd =
         let md = minimizers gwd in
         let imds = Minimizer_data.immediate_deductions md in 
         deduce_several gwd (Image.image (fun (_,_,ded)->ded) imds) ;;
  
  
    end ;; 
      
      let assume = Private.assume ;; 
      let compute_minimizers_inside = Private.compute_minimizers_if_necessary ;;
      let deduce_several = Private.deduce_several ;;
      let deduce_several_directly = Private.deduce_several_directly ;;
      let get_state = Private.get_state ;; 
      let initialize_with = Private.initialize_with ;;
      let make_all_immediate_deductions = Private.make_all_immediate_deductions ;; 
      let minimizers = Private.minimizers ;; 
      let possibilities gwd cell = Possibilities.for_cell (Private.grid gwd) cell ;;
      let possibilities_for_deductor gwd ded= Possibilities.for_deductor (Private.grid gwd) ded ;;
      let hidden_print_out = Private.hidden_print_out ;; 
  
  end ;;   
  
  
  module Helper = struct 
  
  let base1 gwd = List.filter_map
      (fun helper->
        let poss = Grid.possibilities_for_deductor gwd helper in 
        if List.length(poss)<>1
        then Some(helper,poss)
        else let (cell,_) = List.hd poss in 
             if Cell_state.is_yet_undecided(Grid.get_state gwd cell)
             then Some(helper,poss)        
            else None) Deductor.all_atomic_deductors ;;
  
  let level0 gwd = Min.minimize_it_with_care (fun (_,l)->List.length l) (base1 gwd);;
  
  let use_one_shield_step_1 gwd helped (helper,poss)  =
    let cases = Image.image (
      fun (cell,v) -> 
        let new_gwd = Grid.assume gwd cell v  in 
        (cell,Grid.possibilities_for_deductor new_gwd helped)
    ) poss in 
  (helper,Ordered.sort Total_ordering.standard (List.flatten(Image.image snd cases)),cases);;      
  
  let use_one_shield_step_2 (gwd,base1_for_gwd) helped =
     let temp1 = Image.image (use_one_shield_step_1 gwd helped) base1_for_gwd in 
     List.hd(snd(Min.minimize_it_with_care (fun (_,l,_)->List.length l) temp1)) ;;
  
  let base2 gwd = 
     let base1_for_gwd = base1 gwd in 
     Image.image (
       fun (helped,_) -> (helped,use_one_shield_step_2 (gwd,base1_for_gwd) helped)
     ) base1_for_gwd ;;
         
  let level1 gwd = 
      let (m,temp1) = Min.minimize_it_with_care (fun (_,(_,l,_))->List.length l) (base2 gwd) in 
       (m,Image.image (fun (h1,(h2,_,_))->(h1,h2)) temp1);;     
  
  end ;;   
  
  
  (*
  
  open Sudoku ;; 
  open Helper ;;
  
  
  
  let cm = Grid.compute_minimizers_inside ;; 
  
  let original_g0 = cm(Grid.initialize_with 
  
  [
     0;8;0;  1;0;5;  0;2;3;
     0;0;0;  0;2;0;  0;0;0; 
     9;0;0;  0;0;8;  0;4;0; 
  
     0;0;0;  0;8;0;  0;0;2;
     0;6;0;  0;7;0;  0;0;0;
     0;0;4;  2;0;6;  0;3;0;
  
     0;5;0;  6;0;3;  0;0;1;
     0;0;0;  8;5;7;  3;0;0;
     0;0;8;  0;0;0;  5;0;0;
  
  ] );; 
  
  
  
  let g0 = cm(Grid.deduce_several original_g0
       [Inverse_for_inverse_ded(IV(Square 5,3),IV(Square 2,3));
        Direct_ded(C(3,4));
        Inverse_ded(IV(Square 2,6));
        Direct_ded(C(1,3));Direct_ded(C(1,1));Direct_ded(C(1,7));
        Inverse_ded(IV(Column 6,2));Inverse_ded(IV(Column 7,2));
        Inverse_for_direct_ded (IV (Column 6, 1), C (6, 5));
        Inverse_ded(IV(Column 6,9));Inverse_ded(IV(Square 8,9));
        Direct_ded(C(2,4));
        Inverse_ded(IV(Square 8,1));
        Direct_ded(C(7,1));Direct_ded(C(7,3));Direct_ded(C(7,5));Direct_ded(C(7,8));
        Inverse_ded(IV(Column 2,9));Inverse_ded(IV(Column 2,7));
        ] );; 
  
  let g1 = cm(Grid.assume g0 (C(2,2)) 1) ;;
  let g2 = cm(Grid.make_all_immediate_deductions g1) ;;
  let g3 = cm(Grid.make_all_immediate_deductions g2) ;;
  let g4 = cm(Grid.make_all_immediate_deductions g3) ;;
  
  *)
  
  (*
  
  
  let pre_g1 = Grid.assume g0 (C(2,2)) 1 ;;
  let g1 = Grid.deduce_several pre_g1
       [
        Direct_ded(C(3,2));Direct_ded(C(8,2));Direct_ded(C(9,2));
        Direct_ded(C(9,1));Direct_ded(C(9,8));Direct_ded(C(9,9));
        Inverse_ded(IV(Row 3,1));Direct_ded(C(6,7));
        Direct_ded(C(5,7));Direct_ded(C(6,9));Direct_ded(C(3,9));
        Direct_ded(C(8,9));
        ] ;; 
  
  
  Grid.possibilities_for_deductor g0 ded ;; 
  Grid.minimizers g0;;
  Grid.possibilities g0 (C(5,9)) ;; 
  
  
  let pre1_g1 = Grid.assume g0 (C(2,1)) 1;;
  let g1 = Grid.deduce_several_directly pre1_g1
       [C(2,2);C(3,2)] ;; 
  
  let pre1_g2 = Grid.assume g1 (C(2,3)) 5;;
  let g2 = Grid.deduce_several_directly pre1_g2
       [C(3,3);C(3,7);C(3,9)] ;; 
  
  let pre1_g3 = Grid.assume g2 (C(2,4)) 4;;
  let g3 = Grid.deduce_several_directly pre1_g3
            [C(2,6);C(9,4)] ;; 
  
  let pre1_g4 = Grid.assume g3 (C(4,4)) 3;;
  let g4 = Grid.deduce_several_directly pre1_g4
                      [C(5,4)] ;; 
  
  let pre1_g5 = Grid.assume g4 (C(4,6)) 1;;
  let g5 = Grid.deduce_several_directly pre1_g5
              [C(4,3);C(5,6);C(6,5);
              C(4,2);C(5,7);C(9,6);
              C(4,1);C(6,2);C(6,7);
              C(2,7);C(4,8);C(6,1);C(9,2);
              C(4,7);C(8,2);C(9,5);
              C(7,5);C(7,7);C(8,8);C(9,8);
              C(2,8);C(5,8);C(7,1);C(7,3);C(8,3);C(9,9);
              C(2,9);C(5,3);C(5,9);C(8,1);C(8,9);C(9,1);
              C(5,1);
              ] ;; 
  
  
  
  let g2 = Grid.assume g1 (C(1,3)) 6;;
  
  let see1 = Grid.possibilities g0 (C(2,1)) ;; 
  let see2 = Grid.minimizers g3 ;; 
  
  let pre1_g1 = Bare_Grid.assume g0 (C(1,1)) 4;;
  let g1 = Bare_Grid.deduce_several pre1_g1
       [C(1,6);C(2,6)] ;; 
  
  let pre1_g2 = Bare_Grid.assume g1 (C(2,4)) 3;;
  let g2 = Bare_Grid.deduce_several pre1_g2 [C(3,4);C(3,5);C(3,7);C(3,9)] ;; 
  
  let pre1_g3 = Bare_Grid.assume g2 (C(1,3)) 6;;
  let g3 = Bare_Grid.deduce_several pre1_g3 [C(1,7)] ;; 
  
  let g4 = Bare_Grid.assume g3 (C(2,2)) 1;;
  
  let pre1_g5 = Bare_Grid.assume g4 (C(2,1)) 5;;
  let g5 = Bare_Grid.deduce_several pre1_g5 [C(2,3)] ;; 
  
  let pre1_g6 = Bare_Grid.assume g5 (C(3,2)) 2;;
  let g6 = Bare_Grid.deduce_several pre1_g6 [C(3,3)] ;; 
  
  let pre1_g7 = Bare_Grid.assume g6 (C(4,6)) 1;;
  let pre2_g7 = Bare_Grid.deduce_several pre1_g7 [C(5,6);C(6,5);C(9,6)] ;; 
  
  *)
  
  
  (*
     
  let zeroes = 
     
  
  [
     0;0;0;  0;0;0;  0;0;0;
     0;0;0;  0;0;0;  0;0;0; 
     0;0;0;  0;0;0;  0;0;0; 
  
     0;0;0;  0;0;0;  0;0;0;
     0;0;0;  0;0;0;  0;0;0;
     0;0;0;  0;0;0;  0;0;0;
  
     0;0;0;  0;0;0;  0;0;0;
     0;0;0;  0;0;0;  0;0;0;
     0;0;0;  0;0;0;  0;0;0;
  
  ] ;; 
  
  *)
  
  


end ;;


(************************************************************************************************************************
Snippet 109 : Combinatorics on sums of {0,+-1} vectors
************************************************************************************************************************)
module Snip109=struct

  let maxx x =if x=[] then (-1) else Max.list x ;;

  let mil_order = ((fun x y->
    let trial1 = Total_ordering.for_integers (maxx x) (maxx y) in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else
      Total_ordering.silex_for_intlists x y
    ) : int list Total_ordering_t.t) ;; 
  
  let mil_sort = Ordered.sort mil_order ;;
  
  let ip =Memoized.make(fun n->
    mil_sort(List_again.power_set(Int_range.range 1 n)));;
  
  let indexed_sums needed_sum_op needed_zero l=
    let n = List.length l in 
    let indices = ip n in 
    Image.image (fun ind->
      let subset = Image.image (fun t->List.nth l (t-1)) ind in 
      (ind,List.fold_left needed_sum_op needed_zero subset)) indices ;;
  
  let l3_sum (x1,x2,x3) (y1,y2,y3) =  (x1+y1,x2+y2,x3+y3) ;;
  let l3_zero = (0,0,0) ;;
  
  let base3 = [
    (-1,-1,-1);(-1,-1,1); (-1,1,-1);(-1,1,1);
    (1,-1,-1);(1,-1,1); (1,1,-1);(1,1,1);
   ]  ;; 
  
  let l1 = [
    (-1,-1,-1);(-1,-1,1); (-1,1,-1);(-1,1,1);
    (1,-1,0);(1,0,1); (1,1,0);(1,1,0);
   ]  ;; 
  
  
  let pre_res1 = indexed_sums l3_sum l3_zero l1 ;;
  let res1 = List.filter (fun (ind,u)->(ind <> [])&&(u=l3_zero)) pre_res1 ;; 
  
  let l2 = [
    (0,0,-1);(0,-1,1);
    (-1,1,-1);(-1,1,1);
    (1,1,-1);(1,1,1);
   ]  ;; 
  
  let pre_res2 = indexed_sums l3_sum l3_zero l2 ;;
  let res2 = List.filter (fun (ind,u)->(ind <> [])&&(u=l3_zero)) pre_res2 ;; 
  
  let l4_sum (x1,x2,x3,x4) (y1,y2,y3,y4) =  (x1+y1,x2+y2,x3+y3,x4+y4) ;;
  let l4_zero = (0,0,0,0) ;;
  
  let base4 = [
    (-1,-1,-1);(-1,-1,1); (-1,1,-1);(-1,1,1);
    (1,-1,-1);(1,-1,1); (1,1,-1);(1,1,1);
   ]  ;; 
  
  let l3 = [
    (0,1,-1,-1);(0,1,-1,1);(0,1,1,-1);(0,1,1,1);
    (1,-1,-1,-1);(1,-1,-1,1);(1,-1,1,-1);(1,-1,1,1);
    (1,1,-1,-1);(1,1,-1,1);(1,1,1,-1);(1,1,1,1)
   ]  ;; 
  
  let pre_res3 = indexed_sums l4_sum l4_zero l3 ;;
  let res3 = List.filter (fun (ind,u)->(ind <> [])&&(u=l4_zero)) pre_res3 ;; 
  


end ;;


(************************************************************************************************************************
Snippet 108 : Find/replace on two files
************************************************************************************************************************)
module Snip108=struct

  let ap1 = Absolute_path.of_string "lib/Szemeredi/sz3_types.ml";;
  let ap2 = Absolute_path.of_string "lib/Szemeredi/sz3_preliminaries.ml";;
  
  let u1 = Image.image 
  (Replace_inside.replace_several_inside_file 
   ["St_import","Ch_import";
    "St_cumulative","Ch_cumulative";
    "St_fork","Ch_fork";
    ]
  ) [ap1;ap2];;
  


end ;;


(************************************************************************************************************************
Snippet 107 : Looking for a particular non-ASCII charcater
************************************************************************************************************************)
module Snip107=struct

let building_site = home^"/Teuliou/html_files/Translations/Building_site/";;

let emptiable_ap = Absolute_path.of_string (building_site^"emptiable_cmist.txt") ;;
let polished_ap = Absolute_path.of_string (building_site^"polished_cmist.txt") ;;
let walker_ap = Absolute_path.of_string (building_site^"walker_cmist.txt") ;;  

  let u1 = Io.read_whole_file walker_ap ;; 
  let u2 = Lines_in_text.lines u1 ;; 
  let u3 = List.filter (fun line -> line<>"") u2 ;; 
  let u4 = List.filter (fun line -> 
      Substring.is_a_substring_of "Conocimiento obscuro" line 
    ) u3 ;;
  let u5 = List.hd u4 ;; 
  let u6 = Cull_string.cobeginning 126 u5 ;; 
  let u7 = Cull_string.beginning 5 u6 ;; 
  let u8 = Strung.explode u7 ;; 


end ;;


(************************************************************************************************************************
Snippet 106 : Code to add a prefix to each line in a file
************************************************************************************************************************)
module Snip106=struct

  let ap1 = Absolute_path.of_string "~/Downloads/stack.txt";;
  let old_text = Io.read_whole_file ap1 ;; 
  let old_lines = Lines_in_text.lines old_text ;; 
  let new_lines = Image.image (fun line->">! "^line) old_lines ;; 
  let new_text = String.concat "\n" new_lines ;; 
  let act () = Io.overwrite_with ap1 new_text ;; 


end ;;


(************************************************************************************************************************
Snippet 105 : Multi-lingual OCR on cropped pngs 
************************************************************************************************************************)
type spanish_or_latin = Spanish | Latin ;; 

let u1 = 
  let bs = home ^ "/Downloads/Building_site/" in 
  if Sys.file_exists bs 
  then Unix_again.quick_beheaded_complete_ls bs
  else [] ;; 


let (u2,u3) = List.partition (fun fn->String.ends_with ~suffix:".png" fn ) u1 ;; 
exception Bad_length of string list ;;
let check_length l = if not(List.mem (List.length l) [3;4]) then raise(Bad_length(l)) else () ;; 
exception Optional_fourth_elt of string list ;; 
let look_at_optional_fourth_elt l = 
  if List.length(l)>=4 
  then let z = List.nth l 3 in 
       if z<>"sn" then raise(Optional_fourth_elt(l)) else true
  else false ;;
exception Parse_sol_exn of string list ;;   
let parse_sol l = 
  let z = List.nth l 2 in 
  match List.assoc_opt z ["l",Latin;"s",Spanish] with 
  Some sol -> sol 
  | None -> raise(Parse_sol_exn l) ;;  
let sol_to_long_string = function 
  Spanish -> "spa" 
  |Latin -> "lat" ;;   
exception Read_integers_exn of string list ;; 
let read_integers l = 
   try (int_of_string(List.nth l 0),int_of_string(List.nth l 1)) with 
   _-> raise(Read_integers_exn(l)) ;; 

let parse_filename fn =
    let temp1 = Cull_string.coending 4 fn in 
    let temp2 = Str.split (Str.regexp "_") temp1 in 
    let _ = check_length temp2 in 
    (read_integers temp2,
    (parse_sol temp2,
    look_at_optional_fourth_elt temp2)) ;;
(*    
let parse_filename_is_not_ok fn = 
   try (fun _->false)(parse_filename fn) with _ -> true ;;      
let pre_u4 = List.filter parse_filename_is_not_ok u2 ;;    
*)
let u4 = Image.image  parse_filename u2 ;; 
let order_for_ints = Total_ordering.for_integers ;; 
let order_for_int_pairs = Total_ordering.product order_for_ints order_for_ints ;; 
let final_order = Total_ordering.product order_for_int_pairs Total_ordering.standard ;; 
let u5 = Ordered.sort final_order u4 ;; 
let u6 =Int_range.index_everything u5 ;; 

let dir_for_texts = "Text_building_site/"
let num_of_pngs = (string_of_int(List.length u6)) ;; 

let write1 (main_idx,((fn_idx,inner_idx),(sol,is_beginning))) =
   let s_main = string_of_int main_idx 
   and s_idx = string_of_int fn_idx 
   and s_inner = string_of_int inner_idx 
   and long_s_sol = sol_to_long_string sol 
   and optional_ending = (if is_beginning then "_sn" else "") in 
   let full_filename = s_idx^"_"^s_inner^"_"^(Cull_string.beginning 1 long_s_sol)^optional_ending in 
   "tesseract -l "^long_s_sol^" "^full_filename^".png "^full_filename^"\n"^
   "mv "^full_filename^".txt /media/sf_Downloads/"^dir_for_texts^" \n"^
   "echo \""^s_main^" of "^num_of_pngs^":"^full_filename^"\"";;


let text1 = "\n\n\n"^(String.concat "\n" 
 (Image.image write1 u6))^"\n\n\n" ;;   
   
let act1 () = 
  let ap1 = Absolute_path.create_file_if_absent (home^"/Downloads/script.sh") in 
  Io.overwrite_with ap1 text1;;

(* After the OCR has been applied *)
 
let v1 = Unix_again.quick_beheaded_complete_ls 
  (home ^ "/Downloads/Text_building_site/") ;; 
let (v2,v3) = List.partition (fun fn->String.ends_with ~suffix:".txt" fn ) v1 ;; 
(*    
let parse_filename_is_not_ok fn = 
   try (fun _->false)(parse_filename fn) with _ -> true ;;      
let pre_v4 = List.filter parse_filename_is_not_ok v2 ;;    
*)
let v4 = Image.image  parse_filename v2 ;; 
let v5 = Ordered.sort final_order v4 ;; 
(* By the way, v5 = u5 *)

let pages_concerned = Ordered.sort 
 order_for_ints
 (Image.image (fun (x,_)->fst x) v5);;

let v6 = Image.image (
   fun page_nbr -> 
    (page_nbr,List.filter_map (
     fun ((page_nbr2,inner_idx),(sol,is_beginning)) ->
       if page_nbr2 = page_nbr 
       then Some(inner_idx,sol,is_beginning) 
       else None
    ) v5)
) pages_concerned ;;

let compute_footnote_data triples = 
   let temp1 = Three_parts.generic triples in 
   List.rev_map (
    fun (before,center,after)->
       let (inner_idx,sol,is_beginning) = center in 
       let number_of_beginning_markers_before =
         List.length(List.filter (fun (_,_,is_beginning2)->is_beginning2) before)   
       and is_ending = (
         match after with 
           [] -> true 
         | (_,_,is_beginning3) :: _ -> is_beginning3 
       ) in 
       let current_footnote_idx = (
          if is_beginning 
          then number_of_beginning_markers_before + 1 
          else  number_of_beginning_markers_before  
       ) in   
       (inner_idx,sol,current_footnote_idx,is_beginning,is_ending)
   ) temp1 ;; 

let v7 = Image.image (fun (page_nbr,triples) -> 
  (page_nbr,compute_footnote_data triples)
) v6;;

let v8 = Explicit.image (fun (page_nbr,fiftuples) -> 
  let temp1 = Image.image (
    fun (inner_idx,sol,footnote_idx,is_beginning,is_ending) -> 
      let s_page = string_of_int page_nbr
      and s_inner = string_of_int inner_idx 
      and long_s_sol = sol_to_long_string sol 
      and optional_ending = (if is_beginning then "_sn" else "") in 
  let full_filename = s_page^"_"^s_inner^"_"^
      (Cull_string.beginning 1 long_s_sol)^optional_ending^".txt" in 
  let beginning_part = (
     if is_beginning 
     then "\n\n[size=90][b][color=blue]("^(string_of_int footnote_idx)^")[/color][/b]"
     else "" 
  ) and end_part = (
    if is_ending 
    then "[/size]"
    else "" 
 )  in  
 let ap = Absolute_path.of_string (home ^ "/Downloads/Text_building_site/" ^ full_filename) in 
 let (tag_start,tag_end) = (if sol=Latin then ("[latin]","[/latin]") else ("","")) in 
  beginning_part^tag_start^(Io.read_whole_file ap)^tag_end^end_part
  ) fiftuples in 
  (page_nbr,String.concat "\n" temp1) 
) v7;;

let building_site = home^"/Teuliou/html_files/Translations/Building_site/";;

let old_emptiable_ap = Absolute_path.of_string (building_site^"old_emptiable_cmist.txt") ;;

let emptiable_ap = Absolute_path.of_string (building_site^"emptiable_cmist.txt") ;;

let original_text = Io.read_whole_file old_emptiable_ap ;; 

let original_pages = Percent_pagination.extract_all_pages ~verbose:true original_text ;; 

let ampersands = String.make 100 '@' ;;

let trailer = "\n\n\n"^ampersands^"\n\nREGNUM DEUS INTRA VOS EST\n\n"^ampersands^"\n\n\n" ;; 

let new_pages = Explicit.image (
  fun (page_nbr,old_content) -> 
     match List.assoc_opt page_nbr v8 with 
     None -> (page_nbr,old_content) 
     | Some new_content -> 
      (page_nbr,old_content^trailer^new_content) 
) original_pages ;; 
 
let new_text = Percent_pagination.merge_all_pages new_pages ;; 

let act2 () = Io.overwrite_with emptiable_ap new_text ;; 




(************************************************************************************************************************
Snippet 104 : Preprocessing code for a variant type
************************************************************************************************************************)
let this_root = Fw_big_constant.This_World.root ;;

let s_ap1 = (Dfa_root.connectable_to_subpath this_root) ^ "lib/Szemeredi/sz3p.ml";;
let ap1 = Absolute_path.of_string s_ap1 ;; 

let z1 = Io.read_whole_file ap1 ;; 
let z2 = Lines_in_text.interval z1 60 81 ;; 
let z3 = Lines_in_text.lines z2 ;; 
let z4 = Image.image (
  fun line -> 
    let line2 = Cull_string.trim_spaces line in 
    let line3 = Replace_inside.replace_inside_text ("|","") line2 in 
    line3 
) z3 ;;
let z4 = ["Whole"; "Superficial_result"; "Solution_list"; "Qualified_point_list";
"Qpl_length"; "Qpl_interval"; "Sr_upper_half"; "Sr_lower_half";
"Sl_upper_half"; "Sl_lower_half"; "Qpll_upper_half"; "Qpll_lower_half";
"Qpli_upper_half"; "Qpli_lower_half"; "Sr_upper_half_atomized";
"Sr_lower_half_atomized"; "Sl_upper_half_atomized"; "Sl_lower_half_atomized";
"Qpll_upper_half_atomized"; "Qpll_lower_half_atomized";
"Qpli_upper_half_atomized"; "Qpli_lower_half_atomized"] ;;
let z5 = Image.image (
  fun variant -> 
    let lowercase_variant = String.uncapitalize_ascii variant in 
    "let "^lowercase_variant^" (w,scr) = constructor "^
    "("^variant^",,,,w,scr) ;;" 
) z4 ;;
let z6 = "\n\n\n" ^ (String.concat "\n" z5) ^ "\n\n\n" ;; 

let expand_abbreviation text =
   try List.assoc text [
     "Qpl","Qualified_point_list";
     "Sr","Superficial_result";
     "Sl","Solution_list";
     "Qpll","Qpl_length";
     "Qpli","Qpl_interval";
   ] with Not_found -> text;; 

let main_array =
[
  "_length",("(List_to_length_k,");
  "_interval",("(List_to_range_k,");
  "_upper_half",("(Breadth_n_size_to_upper_half_k,");
  "_lower_half",("(Breadth_n_size_to_upper_half_k,");
  "_atomized",("(Atomize_k,");
] ;; 

let secondary_array = 
[
  "Superficial_result","Bulk_result_to_superficial_result_k";
  "Solution_list","Bulk_result_to_solution_list_k";
  "Qualified_point_list","Bulk_result_to_qualified_point_list_k";
] ;; 

let canonical_decomposition variant = 
  match List.find_map (
    fun (appendix,other_ending) -> 
      if String.ends_with ~suffix:appendix variant 
      then let sub_variant = 
            expand_abbreviation(Cull_string.two_sided_cutting ("",appendix) variant) in 
           Some("("^variant^",Some"^other_ending ^ sub_variant ^ "));" )
      else None  
  ) main_array with 
  Some final_ending -> final_ending 
  | None -> 
    if variant = "Whole" then "(Whole,None);" else
    "("^variant^",Some("^(List.assoc variant secondary_array) ^ ",Whole));" ;;

let z7 = Image.image canonical_decomposition z4 ;;
let z8 = "\n\n\n" ^ (String.concat "\n" z7) ^ "\n\n\n" ;; 
let z9 () = print_string z8 ;; 




(************************************************************************************************************************
Snippet 103 : Doing the accounts 
************************************************************************************************************************)
let data = 
  [
     2574,"Keus";
     8400,"Tren A&M";
     1600,"Tren Ewan";
     1820,"Tren Ewan";
     2500,"Tren Ewan";
     1050,"Pakad Usana Mamm";
     1180,"Pesk";
     1212,"Keus";
     1279,"Kig";
     1184,"Amann";
     818,"Pato";
     1750,"Pesk";
     6274,"Kig";
     2000,"Bara";
     1976,"Pesk";
     2336,"Kig";
     860,"Pesk";
     675,"Chalotez da hadan";
     526,"Kig";
     1727,"Pesk";
     561,"Pato";
     1570,"Edeier";
     1297,"Keus";
     1190,"Kig";
     2659,"Gwin";
     856,"Produioù evit netaat";
     1640,"Kig";
     2263,"Kig";
     1013,"Chalotez da hadan";
     1000,"Pesk";
     1149,"Pesk";
     2656,"Kig";
     835,"Produioù evit netaat";
     2045,"Planioù pato";
     1200,"Pesk";
  ] ;; 

let total = Basic.fold_sum (Image.image fst data) ;;   

let labels = Ordered.sort Total_ordering.lex_for_strings 
(Image.image snd data) ;; 

let data2 = Image.image (
  fun lbl -> (lbl,List.filter_map (fun (x,y)->if y=lbl then Some x else None) data)
) labels ;;

let data3 = Image.image (
  fun (lbl,prices) -> (Basic.fold_sum prices,lbl) )
data2 ;;

let data4 = (List.rev(Ordered.sort Total_ordering.standard2 data3)) ;; 

let total2 = Basic.fold_sum (Image.image fst data3) ;;   

(************************************************************************************************************************
Snippet 102 : Interpolation in {0,1}^n 
************************************************************************************************************************)
(*
#require"zarith";;

#install_printer Z.pp_print ;; 
*)

let i_order = Total_ordering.for_integers ;;
let i_intersect  = Ordered.intersect i_order ;;
let i_merge  = Ordered.merge i_order ;;
let i_setminus  = Ordered.setminus i_order ;;
let i_sort  = Ordered.sort i_order ;;
let i_is_included_in = Ordered.is_included_in i_order;;

let il_order = Total_ordering.silex_compare  i_order ;;
let il_sort  = Ordered.sort il_order ;;

module Z = struct 
  type t = int ;;
  let abs x = x ;;
  let add x y = x + y;;
  let mul x y = x * y;;
  let div x y = x * y;;
  let fac x = x ;;
  let zero = 0 ;; 
  let one = 1 ;;
  let minus_one = (-1) ;; 
  let of_int x = x;;
  let equal x y = (x=y) ;; 
  let lt x y = (x<y) ;; 
  let neg x = x;;
end;;  

let z_order =((fun x y ->
   if Z.lt x y then Total_ordering_result_t.Lower else 
   if Z.lt y x then Total_ordering_result_t.Greater else  
    Total_ordering_result_t.Equal): Z.t Total_ordering_t.t) ;;

let z_intersect  = Ordered.intersect z_order ;;    
let z_sort  = Ordered.sort z_order ;;


let product_on_interval = Memoized.recursive(fun old_f (i,j,accu)->
    if i>j then accu else old_f(i,j-1,Z.mul (Z.of_int j) accu)
) ;;

let early_binomial = Memoized.make(fun (n,p)->
  Z.div(product_on_interval (n-p+1,n,Z.one))(Z.fac p)      
) ;; 

let uncurried_binomial = Memoized.make(fun (n,p)->
  if 2*p<=n 
  then early_binomial (n,p)      
  else early_binomial (n,n-p) 
) ;; 

let binomial n p = uncurried_binomial (n,p) ;; 

let alternating_sign j = if j mod 2 = 0 then Z.one else Z.minus_one ;; 

let z_fold_sum l = List.fold_left Z.add Z.zero l ;; 

let z_minimize_it f=function
[]->failwith("min on empty set undefined")
|x::y->
 let rec minimize_it0=(function
  (current_candidate,current_value,da_ober)->match da_ober with
  []->(current_candidate,current_value)
  |a::peurrest->let va=f(a) in
                if (Z.lt va current_value)
				then minimize_it0(a,va,peurrest)
				else minimize_it0(current_candidate,current_value,peurrest)
 ) 
in
 minimize_it0(x,f(x),y);;

let tf1 n j = 
   Z.mul (alternating_sign(j-1))
     (binomial(n-1)(j-1)) ;; 


let is_uniform l = let h = List.hd l in  List.for_all (fun x-> h=x) l;;

let main_image shed indices =
  (* n = List.length arr *)
   z_fold_sum(Image.image (fun k->List.nth shed (k-1)) indices);; 
 
let neighbors n l =
   let temp1 = Three_parts.complemented_points l 
   and outside_l = i_setminus (Int_range.range 1 n) l in 
   let temp2 = Image.image (
     fun (p,others)->
       Image.image (fun q->i_merge [q] others) outside_l 
   ) temp1 in 
   il_sort(List.flatten temp2) ;; 
   
(* neighbors 25 [1;2;3;4;5] ;; *)

let interesting_modifications (n,shed) (indices,img) =
  let close_neighbors = neighbors n indices in 
  List.filter_map (
     fun indices2 ->
     let new_img = main_image shed indices2 in 
     if Z.lt (Z.abs new_img) (Z.abs img)
     then Some(indices2,new_img)
     else None  
  ) close_neighbors ;;

let rec iterator (n,shed,indices,img) =
    let temp1 = interesting_modifications (n,shed) (indices,img) in 
    if temp1=[] 
    then (indices,img)
    else 
    let ((indices2,_),img2) = z_minimize_it snd temp1 in 
    iterator (n,shed,indices2,img2) ;; 

let tf2 =Memoized.make(fun n -> Int_range.scale (tf1 n) 1 n );; 

let tf3 = Memoized.make(fun (n,d)->
  let temp1 = tf2 n in 
  Int_range.scale (fun k->List.nth temp1 ((k*d) mod n) ) 1 n 
) ;; 

let seed n r d= 
  let temp1 = Int_range.range 1 r 
  and temp2 = tf3(n,d) in 
  (n,temp2,temp1,main_image temp2 temp1);; 

let ff n r d= iterator (seed n r d) ;; 

let u1 = tf2 25 ;; 
let u1_pos = Int_range.scale (fun t->List.nth u1 (2*t)) 0 12 ;;  
let u1_neg =  Int_range.scale (fun t->(Z.neg(List.nth u1 (2*t-1)))) 1 12 ;;  

let s1_pos=z_sort u1_pos ;; 
let s2_pos=z_sort(Image.image (fun (a1,a2)->z_fold_sum [a1;a2])
   (Uple.list_of_pairs u1_pos)) ;;
let s3_pos=z_sort(Image.image (fun (a1,a2,a3)->z_fold_sum [a1;a2;a3])
(Uple.list_of_triples u1_pos)) ;;
let s4_pos=z_sort(Image.image (fun (a1,a2,a3,a4)->z_fold_sum [a1;a2;a3;a4])
(Uple.list_of_fourtuples u1_pos)) ;;


let s1_neg=z_sort u1_neg ;; 
let s2_neg=z_sort(Image.image (fun (a1,a2)->z_fold_sum [a1;a2])
   (Uple.list_of_pairs u1_neg)) ;;
let s3_neg=z_sort(Image.image (fun (a1,a2,a3)->z_fold_sum [a1;a2;a3])
(Uple.list_of_triples u1_neg)) ;;
let s4_neg=z_sort(Image.image (fun (a1,a2,a3,a4)->z_fold_sum [a1;a2;a3;a4])
(Uple.list_of_fourtuples u1_neg)) ;;


let g1 = z_intersect s1_pos s4_neg ;; 
let g2 = z_intersect s2_pos s3_neg ;;
let g3 = z_intersect s3_pos s2_neg ;;
let g4 = z_intersect s4_pos s1_neg ;;

let test (a1,a2,a3) = List.for_all (fun x->List.mem x [0;1])
[a1 + (-3*a2 + 3*a3); 3*a1 + (-8*a2 + 6*a3); 6*a1 + (-15*a2 + 10*a3)];;
let res1 = List.filter test (Cartesian.cube [0;1]) ;; 

let v0 = [1;-24] ;; 
let v1 = v0 @ v0 ;; 
let v2 = List_again.power_set v1 ;; 
let v3 = i_sort (Image.image Basic.fold_sum v2) ;; 

let v0 = [1;-24;276] ;; 
let v1 = v0 @ v0 ;; 
let v2 = List_again.power_set v1 ;; 
let v3 = i_sort (Image.image Basic.fold_sum v2) ;; 





(************************************************************************************************************************
Snippet 101 : Interpolation in {0,1}^n 
************************************************************************************************************************)
(*
#require"zarith";;

#install_printer Z.pp_print ;; 
*)

module Container116 = struct
module Z = struct 
  let abs x = x ;;
  let add x y = x + y;;
  let mul x y = x * y;;
  let div x y = x * y;;
  let fac x = x ;;
  let zero = 0 ;; 
  let one = 1 ;;
  let minus_one = (-1) ;; 
  let of_int x = x;;
  let equal x y = (x=y) ;; 
  let lt x y = (x<y) ;; 
end;;  

let product_on_interval = Memoized.recursive(fun old_f (i,j,accu)->
    if i>j then accu else old_f(i,j-1,Z.mul (Z.of_int j) accu)
) ;;

let early_binomial = Memoized.make(fun (n,p)->
  Z.div(product_on_interval (n-p+1,n,Z.one))(Z.fac p)      
) ;; 

let uncurried_binomial = Memoized.make(fun (n,p)->
  if 2*p<=n 
  then early_binomial (n,p)      
  else early_binomial (n,n-p) 
) ;; 

let binomial n p = uncurried_binomial (n,p) ;; 

let alternating_sign j = if j mod 2 = 0 then Z.one else Z.minus_one ;; 

let z_fold_sum l = List.fold_left Z.add Z.zero l ;; 

let z_minimize_it f=function
[]->failwith("min on empty set undefined")
|x::y->
 let rec minimize_it0=(function
  (current_candidate,current_value,da_ober)->match da_ober with
  []->(current_candidate,current_value)
  |a::peurrest->let va=f(a) in
                if (Z.lt va current_value)
				then minimize_it0(a,va,peurrest)
				else minimize_it0(current_candidate,current_value,peurrest)
 ) 
in
 minimize_it0(x,f(x),y);;

let tf1 n j = 
   Z.mul (alternating_sign(j-1))
     (binomial(n-1)(j-1)) ;; 

let tf2 =Memoized.make(fun n -> Int_range.scale (tf1 n) 1 n );; 

let is_uniform l = let h = List.hd l in  List.for_all (fun x-> h=x) l;;

let main_image n arr =
  (* n = List.length arr *)
   let temp1 = List.combine arr (tf2 n) in 
   z_fold_sum(Image.image (fun (v,z_nbr)->
      if v=0 then Z.zero else z_nbr
    ) temp1);;
   
let try_modify_at_index (n,arr,v_arr) j = 
    let indexed_arr = Int_range.index_everything arr in 
    let modified_arr = Image.image (fun (k,v)->
       if k=j then 1-v else v
      ) indexed_arr in 
    if is_uniform modified_arr then None else 
    let new_v_arr= main_image n modified_arr in 
    if Z.lt (Z.abs new_v_arr) (Z.abs v_arr)
    then Some(modified_arr,new_v_arr)
    else None ;;        

let interesting_modifications (n,arr,v_arr) =
   List.filter_map (try_modify_at_index (n,arr,v_arr)) (Int_range.range 1 n) ;;

let rec iterator (n,arr,v_arr) =
    let temp1 = interesting_modifications (n,arr,v_arr) in 
    if temp1=[] 
    then (arr,v_arr)
    else 
    let ((arr2,_),v_arr2) = z_minimize_it snd temp1 in 
    iterator (n,arr2,v_arr2) ;; 

let seed n = 
  let temp1 = Int_range.scale (fun k->if k=(n/2) then 1 else 0) 1 n in 
  (n,temp1,main_image n temp1);; 

let ff n = iterator (seed n) ;; 

end ;;

(************************************************************************************************************************
Snippet 100 : Musings on primes of the form 5p+2
************************************************************************************************************************)
let oi = Total_ordering.for_integers ;; 
let i_sort = Ordered.sort oi ;; 

let u1 = 
[2; 4; 7; 8; 14; 16; 17; 28; 32; 34; 37; 47; 49; 56; 64; 67; 68; 74; 94; 97; 98; 107; 112; 119; 127; 128; 134; 136; 137; 148; 157; 167; 188; 194; 196; 197; 214; 224; 227; 238; 254; 256; 257; 259; 268; 272; 274; 277; 289; 296; 307; 314; 317; 329; 334; 337; 343; 347; 367; 376; 388; 392; 394; 397; 428; 448; 454; 457; 467; 469; 476; 487; 508; 512; 514; 518; 536; 544; 547; 548; 554; 557; 577; 578; 587; 592; 607; 614; 617; 628; 629; 634; 647; 658; 668; 674; 677; 679; 686; 694; 727; 734; 749; 752; 757; 776; 784; 787; 788; 794; 797; 799; 827; 833; 856; 857; 877; 887; 889; 896; 907; 908; 914; 934; 937; 938; 947; 952; 959; 967; 974; 977; 997; 1016; 1024; 1028; 1036; 1072; 1087; 1088; 1094; 1096; 1097; 1099; 1108; 1114; 1117; 1139; 1154; 1156; 1169; 1174; 1184; 1187; 1214; 1217; 1228; 1234; 1237; 1256; 1258; 1268; 1277; 1294; 1297; 1307; 1316; 1327; 1336; 1348; 1354; 1358; 1367; 1369; 1372; 1379; 1388; 1427; 1447; 1454; 1468; 1487; 1498; 1504; 1514; 1552; 1567; 1568; 1574; 1576; 1588; 1589; 1594; 1597; 1598; 1607; 1627; 1637; 1649; 1654; 1657; 1666; 1667; 1697; 1712; 1714; 1739; 1747; 1754; 1774; 1777; 1778; 1787; 1792; 1799; 1813; 1814; 1816; 1819; 1828; 1847; 1867; 1868; 1874; 1876; 1877; 1894; 1904; 1907; 1918; 1934; 1939; 1948; 1954; 1987; 1994; 1997; 2017; 2023; 2027; 2032; 2048; 2056; 2072; 2087; 2137; 2144; 2149; 2159; 2174; 2176; 2188; 2192; 2194; 2198; 2207; 2209; 2216; 2219; 2228; 2234; 2237; 2267; 2278; 2287; 2297; 2303; 2308; 2312; 2329; 2338; 2347; 2348; 2357; 2359; 2368; 2374; 2377; 2401; 2417; 2428; 2429; 2434; 2437; 2447; 2456; 2467; 2468; 2474; 2477; 2479; 2512; 2516; 2536; 2554; 2557; 2569; 2588; 2594; 2614; 2617; 2632; 2647; 2654; 2657; 2669; 2672; 2677; 2687; 2696; 2707; 2708; 2716; 2734; 2738; 2744; 2758; 2767; 2776; 2777; 2779; 2797; 2837; 2839; 2854; 2857; 2887; 2894; 2897; 2908; 2917; 2927; 2936; 2957; 2974; 2996; 3008; 3028; 3037; 3067; 3104; 3134; 3136; 3137; 3148; 3149; 3152; 3167; 3176; 3178; 3187; 3188; 3194; 3196; 3199; 3214; 3217; 3254; 3257; 3269; 3274; 3283; 3298; 3307; 3308; 3314; 3332; 3334; 3347; 3349; 3394; 3407; 3409; 3424; 3428; 3457; 3467; 3478; 3494; 3508; 3517; 3527; 3547; 3548; 3554; 3556; 3557; 3574; 3584; 3589; 3598; 3607; 3617; 3626; 3628; 3632; 3637; 3638; 3656; 3677; 3694; 3697; 3727; 3734; 3736; 3748; 3752; 3754; 3767; 3788; 3797; 3808; 3814; 3829; 3836; 3847; 3859; 3868; 3877; 3878; 3896; 3899; 3907; 3908; 3917; 3947; 3959; 3967; 3974; 3988; 3994; 4007; 4027; 4034; 4039; 4046; 4054; 4057; 4064; 4096; 4109; 4112; 4127; 4144; 4157; 4174; 4177; 4217; 4249; 4274; 4288; 4297; 4298; 4318; 4319; 4327; 4337; 4348; 4352; 4357; 4369; 4376; 4384; 4388; 4396; 4397; 4403; 4414; 4418; 4432; 4438; 4447; 4456; 4457; 4468; 4474; 4489; 4507; 4517; 4529; 4534; 4547; 4556; 4559; 4567; 4574; 4594; 4597; 4606; 4616; 4624; 4637; 4657; 4658; 4676; 4694; 4696; 4699; 4709; 4714] ;;


let u2 = Uple.list_of_pairs u1 ;; 
let unordered_u3 = Image.image (fun (x,y)->y-x) u2 ;; 
let u3 = i_sort unordered_u3 ;; 
let u4 = Explicit.image 
(fun d->(d,List.filter (fun (x,y)->y-x=d) u2)) u3;;
let (u5,u6) = Max.maximize_it_with_care (fun (_,l)->List.length l) u4;;

let all_good x l = List.for_all (fun d->List.mem (x-d) u1) l ;; 


let v2 = List.filter (fun (x,y)->all_good x [840;600]) u2;;
let unordered_v3 = Image.image (fun (x,y)->y-x) v2 ;; 
let v3 = i_sort unordered_v3 ;; 
let v4 = Explicit.image 
(fun d->(d,List.filter (fun (x,y)->y-x=d) v2)) v3;;
let (v5,v6) = Max.maximize_it_with_care 
(fun (_,l)->List.length l) v4;;

(*

let v2 = List.filter (fun (x,y)->List.mem (x-240) u1) u2;;
let unordered_v3 = Image.image (fun (x,y)->y-x) v2 ;; 
let v3 = i_sort unordered_v3 ;; 
let v4 = Explicit.image 
(fun d->(d,List.filter (fun (x,y)->y-x=d) v2)) v3;;
let (v5,v6) = Max.maximize_it_with_care 
(fun (_,l)->List.length l) v4;;

*)

(************************************************************************************************************************
Snippet 99 : Periodically remove a file in a fixed directory.
************************************************************************************************************************)
let calc () = let _ = Ordered.sort Total_ordering.silex_for_intlists
  (List_again.power_set (Int_range.range 1 16)) in ();;

Chronometer.it calc () ;; 

let g1 = "rm -f "^home^"/Downloads/Building_site/Older_pages/Untitled*.png" ;;

let circular () = 
  let counter = ref 0 in 
  while true do let _ = Sys.command g1 in 
  calc();
  counter:=((!counter)+1);
   print_string((string_of_int(!counter)));
   print_string("\n");
   flush stdout ;
  done ;;

(************************************************************************************************************************
Snippet 98 : Successive renamings
************************************************************************************************************************)
(*    
let z1 = ae () ;; 
let z2 = Image.image (fun el->
  (Dfa_module.to_line(Dfn_endingless.to_module el),el)) z1 ;; 

*)

let renamings = ref [
  "Old_polymorphic_ocaml_record_t","Por_types";
  "Opor_public_definition_t","Por_public_definition_t";
  "Opor_common","Por_common";
  "Opor_private_component","Por_private_component";
  "Opor_public_definition","Por_public_definition";
  "Opor_public_component","Por_public_component";

  "fw_poly","gw_poly" ;
  "fw_configuration","gw_configuration" ;
  "file_watcher","gw_life_watcher" ;
  "fw_with_archives","gw_with_archives" ;
  "fw_with_small_details","gw_with_small_details" ;
  "fw_with_dependencies","gw_with_dependencies" ;
  "fw_with_batch_compilation","gw_with_batch_compilation" ;
  "fw_with_githubbing","gw_with_githubbing" ;
  "github_configuration","gw_guthib_configuration";
] ;;

let rename_in_ap ap =
    Replace_inside.replace_several_inside_file (!renamings) ap ;; 

let ap1 = Absolute_path.of_string 
"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_common.ml";;  

let ap2 = Absolute_path.of_string 
"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_private_component.ml";;  

let ap3 = Absolute_path.of_string 
"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_public_definition.ml";;

let ap4 = Absolute_path.of_string 
"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_public_component.ml";;

let ap5 = Absolute_path.of_string 
"watched/watched_and_githubbed/prepare_gw_poly.ml";;

let ap6 = Absolute_path.of_string 
"watched/watched_and_githubbed/prepare_gw_with_dependencies.ml";;

let z4 = 
["opor_public_definition_t"; "opor_common"; "opor_private_component";
 "opor_public_definition"; "opor_public_component"] ;;


dm "opor_public_definition_t" "por_public_definition_t" ;;
dm "opor_common" "por_common" ;;
dm "opor_private_component" "por_private_component" ;;
dm "opor_public_definition" "por_public_definition" ;;
dm "opor_public_component" "por_public_component" ;;

(************************************************************************************************************************
Snippet 97 : Cleanup unused modules
************************************************************************************************************************)
open Needed_values ;; 

let z1 = ae () ;; 
let z2 = Image.image (fun el->
  (Dfa_module.to_line(Dfn_endingless.to_module el),el)) z1 ;; 
let z3 =  (Explicit.image (fun (mn,el)->
  (mn,vfm mn,el)  
)) z2 ;; 
let z4 = List.filter_map (fun (mn,v,el)->
    if v = [] then Some(mn,el) else None
  ) z3 ;; 
let pre_z5 = Image.image fst z4 ;;   
let kept_modules = [
  "yp_token_info"; "yp_flag_parsing_php"; "tested_module_four";
   "tested_module_five"; "sy_token_info_t"; "buenzli_uutf";
   "legendre_symbol"; 
   "self_contained_module_copy"; "node_project"; 
   "tools_for_debugging"; "yp_php_lexer";
   "por_types"
] ;; 
let z6 = List.filter (fun x->not(List.mem x kept_modules)) pre_z5 ;;

let check_z6 = List.filter (fun x->bel(x)<>[]) z6 ;;

let cs1 = Fw_usual_manager.Private.main_ref ;;


(************************************************************************************************************************
Snippet 95 : Construct a get_variant_name function from a long type definition 
************************************************************************************************************************)
let ap1 = Absolute_path.of_string "lib/Padioleau/yp_php_lexer.mll" ;; 
let old_text = Io.read_whole_file ap1;;
let (before_u1,u1,after_u1) = Lines_in_text.tripartition_associated_to_interval old_text 1142 1320 ;; 
let u2 = Lines_in_text.lines u1 ;; 
let u3 = Image.image (fun line->
  Option.get(Cull_string.before_and_after " of " line)) u2 ;; 
let u4 = Ordered.sort Total_ordering.silex_for_strings  (Image.image snd u3) ;; 
let u5 = [
  "Yp_token_info_t.t", "(_)"; 
  "Yp_token_info_t.t ", "(_)"; 
  "bool * Yp_token_info_t.t", "(_,_)";
  "string * Yp_token_info_t.t", "(_,_)";
  "int option * Yp_token_info_t.t", "(_,_)";
  "float option * Yp_token_info_t.t", "(_,_)";] ;;
let u6 = Image.image (
   fun (before_ov,after_ov) ->
     let (_,name) = Option.get(Cull_string.before_and_after "| " before_ov) in 
     let circled = List.assoc after_ov u5 in 
     before_ov^circled^" -> \""^name^"\""
) u3 ;;
let u7 = String.concat "\n" u6 ;; 
let new_text = before_u1 ^ u7 ^ after_u1 ;; 
Io.overwrite_with ap1 new_text ;; 

(************************************************************************************************************************
Snippet 95 : PARI-GP code to compute an explicit primitive element for a Galois extension with group S5
************************************************************************************************************************)
open Needed_values ;;


let tf1 (i,j,vn) =
  let si = string_of_int i 
  and sj = string_of_int j in 
  "for_a"^si^"=make_zero(harry[1]["^sj^"],a"^si^");\n"^
  "harry=subst(harry,a"^si^",for_a"^si^");\n"^
  "printf(\" Step \");"^"printf("^si^");printf(\" for "^vn^" done \\n\");" ;;

let tf2 k vn= (tf1(2*k-1,2*k-1,vn))^"\n"^(tf1(2*k-2,2*k,vn)) ;; 

let tf3 a b vn = String.concat "\n" (Int_range.scale (fun j->
  let k = (a+1)-j in tf2 k vn
) 1 ((a+1)-b));; 

let tf4 vn =
   (tf3 60 1 vn)^"\n"^
   "arr_for_"^vn^"=harry;";; 

let z1 = String.concat "\n\n\n" (Int_range.scale (fun j->tf4("x"^(string_of_int(5-j)))) 1 3);;

let ap1 = Absolute_path.of_string
(home^"/Teuliou/Bash_scripts/Pari_Programming/my_pari_code/follenn2.gp");;

Io.append_string_to_file z1 ap1 ;;  

(************************************************************************************************************************
Snippet 94 : Musings on permutations
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let i_intersect  = Ordered.intersect i_order ;;
let i_merge  = Ordered.merge i_order ;;
let i_setminus  = Ordered.setminus i_order ;;
let i_sort  = Ordered.sort i_order ;;
let i_is_included_in = Ordered.is_included_in i_order;;

let il_order = Total_ordering.silex_compare  Total_ordering.for_integers ;;
let il_sort  = Ordered.sort il_order ;;

let current_order = 5 ;;
let base = Permutation.iii current_order ;;

let eval_list_permutation sigma k = List.nth sigma (k-1) ;;

let compose_list_permutations sigma1 sigma2 = 
   Int_range.scale (fun k-> eval_list_permutation sigma1 (eval_list_permutation sigma2 k)) 1 current_order ;;

let inverse_of_list_permutation sigma =
    let temp1 = Int_range.index_everything sigma in 
    let temp2 = Image.image (fun (x,y)->(y,x)) temp1 in 
    Int_range.scale (fun k->List.assoc k temp2) 1 current_order ;;  

let uncurried_compose = Memoized.make(fun (i,j) ->
   let sigma1 = List.nth base (i-1)   
   and sigma2 = List.nth base (j-1) in 
   List_again.find_index_of_in (compose_list_permutations sigma1 sigma2) base
);;     

let compose  i j = uncurried_compose (i,j) ;;

let fold_compose l = List.fold_left compose 1 l ;; 

let inverse = Memoized.make(fun i->
    let sigma = List.nth base (i-1)   in 
    List_again.find_index_of_in (inverse_of_list_permutation sigma) base
  ) ;; 

let uncurried_commutator =  Memoized.make(fun (x,y)->
  let ix = inverse x
  and iy = inverse y   in 
  compose x (compose y (compose ix iy))
) ;;  

let commutator x y = uncurried_commutator (x,y) ;; 

let commutators arbitrary_set =
    let square = Cartesian.square arbitrary_set in 
    Image.image uncurried_commutator square ;;

let uncurried_conjugate =  Memoized.make(fun (x,y)->
  let ix = inverse x  in 
  compose x (compose y ix)
) ;;  

let conjugate x y = uncurried_conjugate (x,y) ;; 

let conjugate_of_set_by ly x= i_sort (Image.image (conjugate x) ly) ;; 

let base_size = List.length base ;;

let visualize_permutation k =
   let sigma = List.nth base (k-1) in 
   String.concat "" (Image.image string_of_int sigma) ;;


let all_conjugates_of_set ly =
   il_sort(Int_range.scale (conjugate_of_set_by ly) 1 base_size) ;; 

let rec helper_for_order (k,small,big) =
   if big = 1 
   then k 
   else helper_for_order (k+1,small,compose small big) ;; 

let order = Memoized.make (fun k->helper_for_order (1,k,k)) ;; 

let rec helper_for_power (remaining,small,big) =
  if remaining = 0 
  then big
  else helper_for_power (remaining-1,small,compose small big) ;; 

let nonnegative_power x k = helper_for_power (k-1,x,x) ;; 

let power x k =
   if k=0 then 1 else 
   if k<0 
   then nonnegative_power (inverse x) (-k)  
   else nonnegative_power x k ;; 
    
let compose_powers l = fold_compose (Image.image (fun (x,k)->power x k) l) ;;  

let order = Memoized.make (fun k->helper_for_order (1,k,k)) ;;

let subset_product l1 l2 =
    let temp1 = Cartesian.product l1 l2 in 
    let temp2 = Image.image uncurried_compose temp1 in 
    i_sort temp2 ;; 

let rec helper_for_generated_subgroup (treated,seed) = 
      let possibly_new = subset_product treated seed in 
      let really_new = i_setminus possibly_new treated in 
      if really_new = [] 
      then treated 
      else let new_whole = i_merge really_new treated in 
            helper_for_generated_subgroup (new_whole,seed) ;;
        
let generated_subgroup seed = helper_for_generated_subgroup ([1],seed) ;; 

let derived_subgroup sg = generated_subgroup (commutators sg) ;; 

let trivial_subgroup = [1] ;;
let full_subgroup = Int_range.range 1 base_size ;;

let level1  = 
  il_sort (Int_range.scale (fun k->generated_subgroup [k]) 2 base_size) ;; 

let pre_level2 = 
    let temp1 = Uple.list_of_pairs level1 in 
    let temp2 = Image.image (fun (a,b)->generated_subgroup(a@b)) temp1 in 
    il_sort temp2 ;;

let (next_to_level2,level2) = List.partition (fun x->List.mem x level1) pre_level2 ;;

let pre_level3 = 
  let temp1 = Cartesian.product level1 level2 in 
  let temp2 = List.filter (fun (x,y)->not(i_is_included_in x y)) temp1 in 
  let temp3 = Image.image (fun (a,b)->generated_subgroup(a@b)) temp2 in 
  il_sort temp3 ;;

let (next_to_level3,level3) = List.partition (fun x->List.mem x level2) pre_level3 ;;  

let all_subgroups = trivial_subgroup :: (level1 @ level2) ;; 

let subgroups_of_given_group sg =
   List.filter (fun sg2->(i_is_included_in sg2 sg)&&(sg2<>sg)) all_subgroups ;; 

let frobenius_test big_sg small_sg = 
   if small_sg = [1] then false else 
    List.for_all (fun g->
      if List.mem g small_sg 
      then true 
      else  
      let csg = conjugate_of_set_by small_sg g in 
      (i_intersect small_sg csg=[1])
    ) big_sg ;;

let frobenius_subgoups big_sg =
    List.filter (frobenius_test big_sg) (subgroups_of_given_group big_sg);;    

type formal_subgroup = FSG of int ;; 

module Formal_subgroup = struct 

let full_group = FSG(List.length all_subgroups) ;;   
let of_list l = FSG(List_again.find_index_of_in l all_subgroups) ;; 
let to_list (FSG k) = List.nth all_subgroups (k-1) ;;
let derived_subgroup = Memoized.make (fun fsg ->
      of_list(derived_subgroup(to_list fsg))
) ;;
let all = Int_range.scale (fun k->FSG k) 1 (List.length all_subgroups) ;; 
let is_solvable = Memoized.recursive (fun old_f fsg ->
  if fsg = FSG 1 then true else 
  let der_fsg = derived_subgroup  fsg in 
  if der_fsg = fsg 
  then false 
  else old_f der_fsg
) ;;

end ;;  

let z1 = Explicit.image Formal_subgroup.derived_subgroup Formal_subgroup.all ;;
let z2 = List.filter Formal_subgroup.is_solvable Formal_subgroup.all ;;

let z3 = Image.image Formal_subgroup.to_list z2 ;; 
let maximalities = Ordered_misc.maximal_elts_wrt_inclusion (il_sort z3) ;; 
let z4 = Explicit.image (fun x->(x,all_conjugates_of_set x)) maximalities ;;
let maximalities_up_to_conjugation = List.filter_map (fun (x,y)->if x=List.hd y then Some x else None) z4;;

let four_cycles = List.filter (fun k->order(k)=4) 
   (Int_range.range 1 base_size) ;;
let z5 = Image.image (
   fun k-> 
     let sg = generated_subgroup (i_sort [34;k]) in 
     (Formal_subgroup.of_list sg,k)
) four_cycles ;;
let z6 = Partition_list.according_to_fst z5 ;; 
let z7 = Image.image snd z6 ;; 
let common_cycles = List.hd z7 ;; 



let view_as_product_of_cycles k =
   let sigma = List.nth base (k-1) in 
   Permutation.decompose_into_disjoint_cycles sigma ;; 

let ivp = Image.image view_as_product_of_cycles ;; 

let tf1 sigma k = conjugate (power 34 k) sigma ;; 
let tf2 sigma =
   let sigma_inv = inverse sigma in 
   let temp1 = Int_range.scale (tf1 sigma) 0 4 
   and temp2 = Int_range.scale (tf1 sigma_inv) 0 4 in 
   (ivp temp1,ivp temp2,i_sort(temp1@temp2)) ;; 

let z8 = tf2 11 ;; 
let (z9,z10,z11) = tf2 10 ;; 
let other_cycles = i_setminus common_cycles z11 ;; 
let z12 = tf2 18 ;; 

let sg_t = generated_subgroup [34] ;; 
let base2 = Cartesian.product (Int_range.range 0 4) (Int_range.range 0 3) ;; 

let x0 = 18 and y0 = 10 ;; 


let sg_x0 = generated_subgroup [x0] ;; 
let sel_x0 w =
   List.find (fun (i,j)->compose(power 34 i)(power x0 j)=w) base2 ;;

let z13 = Cartesian.product sg_t sg_x0 ;; 
let z14 = Image.image (fun (u,v)->(compose u v,(u,v))) z13 ;;
let mixed1 = i_sort(Image.image fst z14);;
let z15 = Cartesian.square mixed1 ;; 
let z16 = List.filter (fun (u,v)->compose u v = y0) z15 ;; 
let z17 = Image.image (fun (u,v)->(sel_x0 u,sel_x0 v)) z16;;

let see1 = compose_powers [34,2;10,2;34,1;10,1] ;;
let see2 = compose_powers [34,1;18,1;34,2;18,2] ;;

let z18 = Cartesian.tproduct sg_t common_cycles sg_t ;; 
let z19 = Image.image (fun (u,v,w)->(fold_compose [u;v;w],(u,v,w))) z18 ;;
let z20 = i_sort (Image.image fst z19) ;; 
let z21 = i_setminus z20 common_cycles ;; 
let z22 = List.filter (fun p->fst(p)=25) z19;;

let act_on_int i_sigma k = List.nth (List.nth base (i_sigma-1)) (k-1) ;; 
let act_on_intlist i_sigma l =  i_sort(Image.image (act_on_int i_sigma) l) ;;
let collective_on_intlist i_sigma  ll = il_sort (Image.image (act_on_intlist i_sigma) ll);;

let power5 = il_sort (List_again.power_set (Int_range.range 1 5) );;
let triples = List.filter (fun x->List.length x=3) power5 ;; 

let frob = generated_subgroup [11;34] ;;
let frob_on_intlist l = il_sort (Image.image (fun f->act_on_intlist f l) frob) ;; 
(************************************************************************************************************************
Snippet 93 : Musing on discrepancy problem
************************************************************************************************************************)
module Container106 = struct 
module Z = struct

type t = Big of int ;;
let of_int x = Big x;;  
let to_int (Big x) = x;;

end ;;  

module Q = struct 

type t = { num : Z.t; den : Z.t; } ;;  

let pp_print (fmt:Format.formatter) (q:t) = () ;;   
let of_int i = {num = (Z.of_int i); den = (Z.of_int 1)} ;;
let div q1 q2 = q1 ;;
let max q1 q2 = q1 ;;
let min q1 q2 = q1 ;;
let (>=) q1 q2 = true ;;

end ;;  

(*
#require"zarith";;
*)

(*
#install_printer Z.pp_print ;; 
*)


let i_order = Total_ordering.for_integers ;; 
let i_setminus = Ordered.setminus Total_ordering.for_integers ;;
let i_sort = Ordered.sort Total_ordering.for_integers ;;

let qfrac_of_pair (i,j) = Q.div (Q.of_int i) (Q.of_int j) ;; 
let qfrac_to_pair q = (Z.to_int q.Q.num,Z.to_int q.Q.den) ;; 

let q1 = qfrac_of_pair (4,7) ;; 

let first_pfrac = (1,2) ;; 
let next_pfrac (k,n) =
   if k=n then (1,n+1) else (k+1,n) ;; 

module Q_interval = struct 

type t = I of Q.t * Q.t ;;

let intersect (I(a1,b1)) (I(a2,b2)) =
   let a = Q.max a1 a2 and b = Q.min b1 b2 in 
   if Q.(>=) a b then None else Some(I(a,b)) ;;       

let of_pfrac (k,n) =
    I(qfrac_of_pair (k-1,n),qfrac_of_pair (k,n)) ;; 

end ;;  


type localizer = L of (int * Q_interval.t) list ;;  

module Localizer = struct 
  
type t = localizer ;;   
  
let rec helper_for_pair_insertion (treated,(x,itv),to_be_treated) =
    match to_be_treated with 
    [] -> Some (List.rev_append treated [x,itv])
    | (x2,itv2) :: others ->
        if x2<x 
        then  helper_for_pair_insertion ((x2,itv2)::treated,(x,itv),others) 
        else
        if x<x2 
        then  Some (List.rev_append treated ((x,itv)::to_be_treated))
        else  (match Q_interval.intersect itv itv2 with 
                None -> None
              | Some itv3 -> Some (List.rev_append treated ((x,itv3)::others))
             ) ;;

let insert_pair pair (L l) =
    match helper_for_pair_insertion ([],pair,l) with 
      None -> None 
      | Some new_l -> Some (L new_l) ;;              

end ;;  

type walker =  W of (((int*int)* int) list)* Localizer.t ;; 

module Walker = struct 

type t = walker ;; 

let impose_value (k,n) j (W(pairs,lclzr)) =
    let itv = Q_interval.of_pfrac (k,n) in 
    match Localizer.insert_pair (j,itv) lclzr with 
    None -> None 
    |Some new_lclzr -> Some(W(((k,n),j)::pairs,new_lclzr)) ;;

let descendants walker =
   let (W(pairs,lclzr)) = walker in 
   let (k0,n0) = (match pairs with 
     [] -> first_pfrac
     |(predecessor,_) :: _ -> next_pfrac predecessor 
   ) in 
   let already_used_indices = i_sort (List.filter_map (
     fun ((_,n),j) -> if n=n0 then Some j else None 
   ) pairs) 
   and whole = Int_range.range 1 n0 in 
   let unused_indices = i_setminus whole already_used_indices in 
   List.filter_map (
    fun j-> impose_value (k0,n0) j  walker
   ) unused_indices ;;  

end ;;

module Walker_list = struct 

let rec descendants = function 
  [] -> []
  | walker :: others ->
      let trial1 = Walker.descendants walker in 
      if trial1 = []
      then descendants others 
      else trial1 @ others ;;      

let origin = [W([],L[])];;

let main = Memoized.small descendants origin ;; 

end ;;  

let ff = Walker_list.main ;; 

let u1 = ff 10000 ;; 
let (W(u2,L u3)) = List.hd u1 ;; 
let u4 = List.rev u2 ;; 
let gg n = List.filter_map 
   (fun ((_,m),j)->if m=n then Some j else None) u4 ;;

let tt n =  (Walker.descendants(List.hd(ff n))=[]) ;;  

let z1 = List.filter tt (Int_range.range 1 2000) ;;

end ;; 

(************************************************************************************************************************
Snippet 92 : Examples of "translating" ppx_deriving into usual OCaml code
************************************************************************************************************************)
(*
let stork1 = {
  FileHandler.logs_folder="suzanne"; 
        truncate=false; 
        file_perms=567; 
        date_prefix = Some "vega"; 
        versioning= Some 71; 
        suffix ="Bart";
} ;;     

let stork2 = 
{
  FileHandler.logs_folder="amy"; 
        truncate=true; 
        file_perms=567; 
        date_prefix = None; 
        versioning= None; 
        suffix ="macdonals";
} ;;     

let bee1 = {file_handlers=stork1} ;; 
let bee2 = {file_handlers=stork2} ;; 

let wasps = Image.image config_to_yojson [bee1;bee2] ;;

let clis = Image.image (fun lev -> {level = lev}) 
[Debug;Trace;Info;NoLevel] ;;

let clos = Image.image cli_json_params_to_yojson clis ;; 

let check6= List.filter (fun cfg->
   (cli_json_params_of_yojson cfg)<>(cjo cfg)
  ) clos ;;

let check5= List.filter (fun cfg->
   (cli_json_params_to_yojson cfg)<>(cjt cfg)
  ) clis ;;

let check4= List.filter (fun cfg->
   (config_of_yojson cfg)<>(config__of_yojson cfg)
  ) wasps ;;

let check3 = List.filter (fun cfg->
   (config_to_yojson cfg)<>(config__to_yojson cfg)
  ) [bee1;bee2] ;;

let check1 = List.filter (fun cfg->
   (config_to_yojson cfg)<>(stork_to_yojson cfg)
  ) [stork1;stork2] ;;

let starks = Image.image config_to_yojson [stork1;stork2] ;;

let check2 = List.filter (fun yj->
  (config_of_yojson yj)<>(stork_of_yojson yj)
 ) starks ;;
*)

(************************************************************************************************************************
Snippet 91 : Local modularization
************************************************************************************************************************)
open Needed_values ;; 
let path1 = home^"/Downloads/OCaml_packages/calendar-3.0.0/src" ;; 

let u1 = rf (path1^"/depend.txt") ;;
let u2 = Str.split (Str.regexp_string " ") u1;;
let u3 = List.filter (fun s-> String.ends_with ~suffix:".ml" s) u2;;
let u4 = Image.image (fun s->
    Absolute_path.of_string (path1^"/"^s)
  ) u3;;
let u5 = Modularize.modularize_several "" u4 ;; 
let u6 = "module CalendarLib = struct \n\n" ^ u5 ^ "\n\nend ;;" ;;  

let path2 = home^"/Teuliou/OCaml/Ordinary/Fads/Extract_php_lexer_from_padioleau/More_complete_version";; 
let ap1 = Absolute_path.of_string (path2^"/wrapped_calendarlib.ml") ;;
Io.overwrite_with ap1 u6 ;; 

(************************************************************************************************************************
Snippet 90 : Copy large interval of text from a file to another
************************************************************************************************************************)
let ap1 = Absolute_path.of_string "Githubbed_archive/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;
let ap1 = Absolute_path.of_string "Fads/nap.ml" ;;
Lines_in_text.duplicate_interval_in_file (228,287) ap1 ;; 

let ap2 = Absolute_path.of_string "Fads/pan.ml";; 


(************************************************************************************************************************
Snippet 89 : Sorting and comparing two overlapping list fo files
************************************************************************************************************************)
let ap1 = Absolute_path.of_string "~/Downloads/temp.html" ;; 
let text1 = Io.read_whole_file ap1 ;; 


let left_tag = "[{\"text\":\"" ;;
let n1 = String.length left_tag ;; 
let u1 = Substring.occurrences_of_in left_tag text1 ;; 
let u2 = Image.image (
  fun i->
    let j = Option.get(Substring.leftmost_index_of_in_from_opt "\"" text1 (i+n1)) in 
    Cull_string.interval text1 (i+n1) (j-1)
) u1 ;;
let u3 = Ordered.sort Total_ordering.lex_for_strings u2 ;;

let bad_beginnings = [
" ";" "; " "; " ";
"Enregistrer ";"La description";"Le titre "; "Placer ";
"Pour";"Remarque "; "Revenir "; "Si ";"Sous";"Supprimer"; "Tout "; "Traitement";"Vidéo";"Visible"; "VISIONN"; "Voulez-";    
"Fermer";"Vérifiez ";"Vous "] ;;
let bad_contents = ["\\";"Titre";"Terminé";"Sermons";
"Plus";"Diminuer la vitesse de lecture"; "Définir comme miniature de la playlist";"Définir comme série officielle"; "Déplacer vers le haut";"Effectuer des recherches vocales"; "En attente de l'autorisation…";"En attente..."; "En cours de lecture"; "Enregistrer";"Enregistrer dans une playlist"; "Enregistrer dans \\";"Faire un zoom arrière"; "Faire un zoom avant"; "Général";"94";"Lecture"; "Lecture aléatoire"; "Lecture/Pause";"Mettre en ligne une vidéo"; "Micro désactivé. Veuillez réessayer.";"Mise à jour aujourd'hui"; "Non répertoriée";"Panoramique vers la droite"; "Panoramique vers la gauche";
"Panoramique vers le bas"; "Paramètres de la playlist"; "Passer au direct";"Privée"; "Publique"; "Raccourcis clavier"; "Rechercher";
"Abbé Interesse"; "Accueil YouTube"; "Accéder au chapitre précédent";
   "Accéder au chapitre suivant"; "Activer/désactiver le mode plein écran";
   "Activer/Désactiver le lecteur réduit";
   "Activer/Désactiver le mode cinéma"; "Activer/Désactiver le son";
   "Ajouter des nouvelles vidéos au début de la playlist";
   "Ajouter des vidéos"; "Ajouter à la file d'attente";
   "Alterner entre les différents niveaux d'opacité de la fenêtre";
   "Alterner entre les différents niveaux d'opacité du texte";
   "Alterner entre les tailles de police (augmentation)";
   "Alterner entre les tailles de police (diminution)"; "Annuler";
   "Appuyez sur le micro pour réessayer";
   "Atteindre un moment spécifique de la vidéo (7 correspond à 70 % de la durée)";
   "Aucune connexion"; "Aucune description"; "Augmenter la vitesse de lecture";
   "Autoriser l'intégration";
   "Autorisez l'accès au micro pour effectuer des recherches vocales";
   "Avancer de 10 secondes"; "Collaborer";
   "Commande non comprise. Veuillez réessayer."; "Description";
   "Image précédente (en pause)"; "Image suivante (en pause)";
   "Ignorer"; "Ignorer les liens de navigation";
] ;; 
let u4 = List.filter (
  fun w->(List.for_all (
    fun v-> not(String.starts_with ~prefix:v  w) 
  ) bad_beginnings)
  &&
  (not(List.mem w bad_contents))
) u3 ;;

let u5 = 
  ["01 1er Dimanche de l’Avent et de l’Année Liturgique - Vendée - 01-12-2013 -";
  "02 IIe  Dimanche de l’Avent Moulins 09 12 1990";
  "03 IIIe  Dimanche de l’Avent Anjou 16 12 1990 La joie dans le Seigneur, moteur de la vie chrétienne.";
  "031 IVe Dimanche de Carême Joie dans la penitence et Montée du Carmel 10 03 91 26min06";
  "032 Ier Dimanche de la Passion RévéIation progressive de la Divinité de NS Moulins 05 04 1992 40min0";
  "035 Pâques Moulins 04 04 1996 44min07";
  "036 Dimanche in albis Quasi modo Les corps glorieux Tours 02 04 1989 19min22";
  "037 IIe Dimanche apres Pâques Miséricorde et Justice de Dieu équilibre de l'esprit chrétien";
  "038 IIIe Dimanche après Pâques Modicum et videbitis Me ND de l'Epine Mayenne 21 04 2013 26min12";
  "040 Ve Dimanche après Pâques Dieu consolé par les Siens Tours 30 04 1989";
  "046 Dimanche dans l'Octave de l'Ascension 2014 Vendée";
  "047 Pentecôte  Moulins 19 05 1991";
  "050 Très Sainte Trinité Moulins 16 Juin 1990";
  "051 Fête du Très Saint Sacrement Moulins 02 06 1991";
  "052 Dimanche dans l’octave du Très Saint Sacrement N.D. de l’Epine 02 06 2013";
  "054 Dimanche dans l’octave du Sacré-Coeur N.D. de l’Epine 09 06 2013";
  "055 IVe Dimanche après la Pentecôte Moulins 16 06 1991";
  "056 Ve Dimanche après la Pentecôte  Sur la réparation et componction Tours 18 06 1989";
  "057 L’Empereur Saint Henri I et la sanctification dans le monde - Moulins 15-07-1990";
  "058. VIIe D. Ap Pent. ND de l’Epine 07 07 2013 Contre les loups déguisés sous des peaux de brebis...";
  "059 Saint Bonaventure et le bonheur en Dieu seul - Moulins - 14-07-1991";
  "06 Vigile Nativité - Choix et goûts de Dieu - Moulins 24-12-1990 min52";
  "060 IXe Dimanche après la Pentecôte - Mayenne ND de l’Epine - 21-07-2013";
  "065 XIIIe Dimanche après la Pentecôte 18 08 2013";
  "066 XIVe Dimanche après la Pentecôte - Saint Louis - 25-08-1991";
  "067 XVe Dimanche après la Pentecôte - Moulins 16-09-1990";
  "069 XVIIe Dimanche après la Pentecôte ND de l’Epine 16 09 2018";
  "070 XVIIIe Dimanche après la Pentecôte ND de l’Epine 8 10 2017";
  "071 XIXe Dimanche après la Pentecôte Sur la colère 24-09-1989 à Tours";
  "072. XXe Dimanche après la Pentecôte 22 10 2017 Notre Dame de l’Epine";
  "073 Fête du Christ-Roi - à Moulins - 27 10 1991";
  "074 XXIe Dimanche après la Pentecôte  En Vendée - 6/11/2011";
  "076 XXIIIe Dimanche après la Pentecôte - sur le Purgatoire - Moulins 11-11-1990";
  "08 Messe de Minuit - Anniversaire naissance de la fille aînée de l'Eglise -Moulins 25-12-1996";
  "081 Solennité de Notre Dame des Victoires du Très Saint Rosaire - Tours 08 10 1989";
  "082 Maternité Divine de Notre Dame 11 10 2015 Mayenne ND de l'Epine";
  "083 Toussaint - Moulins - 01 11 1996";
  "084 Commémoration des fidèles défunts - Vendée 02-11-2012";
  "088 Découverte de la Sainte Croix - ND de l'Epine 3 05 2015";
  "09 Messe du jour de Noël - Moulins 25-12-1996";
  "091 VIe  D. après la Pentecôte Solennité des Saints Apôtres Pierre et Paul  Moulins 30 06 1991";
  "092  Fête de Sainte Anne  Mayenne ND de l'Epine  26 07 2015";
  "094  Fête de Saint Luc - Mayenne ND de l’Epine - 18-10-2015";
  "100 Très Précieux Sang Mayenne ND de l’Epine 1er juillet 2013";
  "101 Nativité de Notre Dame Moulins 08-09-1991";
  "102 Notre-Dame des Sept Douleurs - 15-09-1996";
  "103 Solennité de Sainte Thérèse de l’Enfant Jésus et de la Ste Face - Moulins 30-09-1990";
  "104 Solennité de Saint Michel Archange Moulins 29 09 1991";
  "106 VIe D. ap. Pent - Fructueuse ou infructueuse Communion à N. S. - ND de l’Epine - 12-7-2020";
  "115 VIIIe Dimanche après la Pentecôte - Sur la remise des dettes - 7-8-2011  - Mayenne";
  "116 XVIe Dimanche après la Pentecôte ND de l’Epine 20 09 2020";
  "12 Dimanche dans l'Octave de la Nativité - Dum medium silentium... - Tours 31 12 1989";
  "135 Fête de l'Evangéliste Saint Marc 25 04 2021";
  "139 Pentecôte Vendée 27 05 2018";
  "141 Saint Apôtre Jacques le Majeur - Mayenne ND de l’Epine 25-07-2021";
  "142  Coeur Immaculé de Marie N.D. de l’Epine 22 08 2021";
  "147 XVIe Dimanche après la Pentecôte 25 09 2022 Notre Dame de l’Epine";
  "15 Circoncision - Vendée 01 01 2013";
  "152 Fête de Saint Michel Archange 29 09 2019";
  "16 Saint Nom de Jésus - Vendée 02-01-2013"; "17 Epiphanie - 06-01-1996";
  "19 Dimanche dans l'octave de l'Epiphanie - Manifestation de la Divinité de NS - Moulins 13-01-1991";
  "20 La Sainte Famille Moulins 07 01 1996";
  "21 IIe Dimanche après l'Epiphanie Mayenne ND de l'Epine 19 01 2014";
  "22  IIIe Dimanche après l'Epiphanie - Noli vinci a malo, sed vince in bono malum - Tours 25-01-1990";
  "23 Septuagésime Deux genres de conversion Moulins 27 01 1991";
  "24 Sexagésime - ND de Lourdes -Montée de l'esprit anti-chrétien et apparitions  - Moulins 11-02-1996";
  "25 Quinquagésime Annonce prophétique de la Passion - Moulins 10 02 1991";
  "61 Xe  D. ap. la Pent. Sur le Principe et Fondement des exercices de Saint Ignace Tours - 31-07-1988";
  "62 XIe Dimanche après la Pentecôte - 04-08-1991 Moulins";
  "75 XXIIe Dimanche après la Pentecôte - ND de l’Epine - 21 10 2018";
  "77.  Dernier Dimanche après la Pentecôte  -  Moulins  26-11-1989";
  "78 IVe Dimanche après l’Epiphanie - Tempête apaisée - Moulins 5/11/1989 - (XXVe D. Ap. P.)";
  "78 XXIVe Dimanche après la Pentecôte (IVe ap Eph) - Tempête apaisée - Moulins 1992";
  "79 Ve Dimanche après l’Epiphanie -Sur le bon grain et l’ivraie - Moulins 4/02/1990 -";
  "79 XXVe D. après la Pent. (Ve ap Epiph) -Moulins 04 02 1990- Parabole sur le bon grain et l’ivraie.";
  "80 XXVIe Dimanche après la Pentecôte (VIe ap Epiphanie) - 17 11 1991";
  "86 Fête de l’Immaculée Conception de la Très Sainte Vierge - Moulins 8-12-1989-";
  "87 Présentation de NS au temple et Purification légale de Marie -Mayenne ND de l’Epine - 02-02-2014";
  "93 Saint Laurent, Diacre et Martyr - Vendée - 10-08-2014";
  "96 Assomption Moulins 15 08 1991";
  "99 Saint Joachim, Père de la Très Sainte Vierge Marie - Vendée - 16-08-2015";
  "Dimanche des Rameaux En Vendee 01 04 2012";
  "Ier Dimanche de Carême Sur la Pénitence. Moulins 04/03/1990. 24min53";
  "Ier Dimanche de Carême. Sens mystique des montées vers Jérusalem 16/02/1997 15min18";
  "IIIème dimanche de Carême. Contre le démon muet. Moulins, 10/03/1996, 26min07";
  "IIème Dim. de Carême. Transfiguration. Équilibre entre désolations et consolations.";
  "IVe  Dimanche de l’Avent - Moulins 23-12-1990 -  Comparaison du temps des Patriarches avec le nôtre.";
  "IVe D. après Pâques :  Attachement apostolique à NS et remontée de sa nature humaine à sa Divinté.";
  "IVe Dimanche de Carême - Laetare - Définition de la Charité - Tours - 05-03-1989";
  "Sermon Jeudi Saint 13 04 2017 31min27";
  "Sermon sur l'Ascension à Moulins 24 05 1990";
  "Solennité du Sacré Coeur et Saint Jean Baptiste - Il faut qu'Il croisse et que je diminue 24/6/1990";
  "Veillée pascale Sur l'illogisme de l'attitude actuelle des Juifs talmudistes Moulins 11 04 1998"]
;;
(*
let check_u5 = u5=u4;;
*)

let (already_numbered_titles,nonnumbered_titles) =
  List.partition (
    fun s->let c= int_of_char(String.get s 0) in 
      (48<=c)&&(c<=57)
  ) u5 ;;

exception Ios_exn of string;;

let ios x =
   try   int_of_string x with _ -> 
    if String.ends_with ~suffix:"." x 
    then int_of_string(Cull_string.coending 1 x)  
    else raise(Ios_exn(x)) ;; 

let unordered_numbered_titles_part1 = Image.image (
  fun s ->
     let k = Option.get(Substring.leftmost_index_of_in_from_opt " " s 1) in
     (ios (Cull_string.beginning (k-1) s),
     Cull_string.cobeginning k s
     )
) already_numbered_titles ;;
let pair_order = Total_ordering.product 
    Total_ordering.for_integers Total_ordering.lex_for_strings ;;
let numbered_titles_part1 = 
   Ordered.sort  pair_order  unordered_numbered_titles_part1 ;;
let indices_in_part1 = Image.image fst numbered_titles_part1 ;; 
let indices_not_in_part1 =
   Ordered.setminus Total_ordering.for_integers
   (Int_range.range 1 143) indices_in_part1 ;;




(*
let dir2 = Directory_name.of_string 
  "/Volumes/Matroska/Video/Abbe_Interesse/";;
let naive_v1 = More_unix.beheaded_simple_ls dir2 ;; 
*)
let v1 = 
  ["001_Ier_D_de_l_Avent_01_12_2013_27min33.mp4";
  "002_IIe_dimanche_de_l_Avent_Moulins_09_12_1990_35min32.mp4";
  "003_IIIe_dimanche_de_l_Avent_Gaudete_En_Anjou_16_12_1990_29min26.mp4";
  "004_IIIe_dim_de_l_Avent_Vendee_16_12_2018.mp4";
  "005_IVe_dimanche_de_l_Avent_Comparaison_du_temps_des_Patriarches_avec_le_notre_Moulins_23_12_1990_28min19_Copie_en_conflit_de_debian_2019_11_13.mp4";
  "006_Vigile_Nativite_Choix_et_gouts_de_Dieu_Moulins_24_12_1990_18min52.mp4";
  "007_Vigile_de_Noel_En_Vendee_24_12_2012_29min31.mp4";
  "008_Messe_de_Minuit_Anniversaire_naissance_de_la_fille_ainee_de_l_Eglise_Moulins_25_12_1996_28min04.mp4";
  "009_Messe_du_jour_de_Noel_Divinite_du_Christ_Seigneur_demontree_par_S_Paul_aux_Hebreux_a_partir_de_l_Ancien_Testament_Moulins_25_12_1996_19min48.mp4";
  "010_Nativite_Messe_de_minuit_En_Vendee_25_12_2012_14min17.mp4";
  "011_Nativite_Messe_du_jour_En_Vendee_25_12_2012_11min26.mp4";
  "012_Dimanche_dans_l_Octave_de_la_Nativite_Dum_medium_silentium_Tours_31_12_1989_21min22.mp4";
  "013_Dimanche_dans_l_octave_de_NoeI_30_12_2012_21min08.mp4";
  "014_Dimanche_dans_l_Octave_de_NoeI_ND_de_l_Epine_29_12_2019_28_min.mp4";
  "015_Circoncision_En_Vendee_01_01_2013_26min22.mp4";
  "016_Saint_Nom_de_Jesus_En_Vendee_02_01_2013_22min24.mp4";
  "017_Epiphanie_06_01_1996_38min27.mp4";
  "018_Epiphanie_07_01_90_29min34.mp4";
  "019_Octave_de_l_Epiphanie_Manifestation_de_la_Divinite_de_NS_Moulins_13_01_1991_38min17.mp4";
  "020_La_Sainte_Famille_Moulins_07_01_1996_31min33.mp4";
  "021_IIe_dimanche_apres_l_Epiphanie_Mayenne_ND_de_l_Epine_19_01_2014_30min50.mp4";
  "022_IIIe_dimanche_apres_l_Epiphanie_Noli_vinci_a_malo_sed_vince_in_bono_malum_25_01_90_34min31.mp4";
  "023_Septuagesime_Deux_genres_de_conversion_Moulins_27_01_1991_28min27.mp4";
  "024_Sexagesime_ND_de_Lourdes_Montee_de_l_esprit_anti_chretien_et_apparitions_de_ND_Moulins_11_02_1996_33min36.mp4";
  "025_Quinquagesime_Annonce_prophetique_de_la_Passion_Moulins_10_02_1991_24min39.mp4";
  "026_Ier_dimanche_de_Careme_Sens_mystique_des_montees_vers_Jerusalem_16_02_1997_15min18.mp4";
  "027_Ier_dimanche_de_Careme_Sur_la_Penitence_Moulins_04_03_1990_24min53.mp4";
  "028_IIe_Dim_de_Careme_Transfiguration_Equilibre_entre_desolations_et_consolations_Moulins_24_02_1991_34min49.mp4";
  "029_IIIe_dimanche_de_Careme_Contre_le_demon_muet_Moulins_10_03_1996_26min07.mp4";
  "030_IIIe_dimanche_de_Careme_Sur_l_Annonciation_Maternite_virginale_et_voeu_de_virginite_22_03_92.mp4";
  "031_IVe_dimanche_de_Careme_Joie_dans_la_penitence_et_Montee_du_Carmel_10_03_91_26min06.mp4";
  "032_Ier_dimanche_de_la_Passion_ReveIation_progressive_de_la_divinite_de_NS_Moulins_05_04_1992_40min02.mp4";
  "033_Dimanche_des_Rameaux_En_Vendee_01_04_2012_16min12.mp4";
  "034_Veillee_pascale_Sur_l_illogisme_de_l_attitude_actuelle_des_Juifs_talmudistes_Moulins_11_04_1998_26min11.mp4";
  "035_Paques_Moulins_04_04_1996_44min07.mp4";
  "036_Dimanche_in_albis_Quasi_modo_Les_corps_glorieux_Tours_02_04_1989_19min22.mp4";
  "037_IIe_dimanche_apres_Paques_Misericorde_et_Justice_de_Dieu_equilibre_de_l_esprit_chretien_Fete_de_saint_Pierre_de_Verone_29_04_1990_30min12.mp4";
  "038_IIIe_dimanche_apres_Paques_Modicum_et_videbitis_Me_ND_de_l_Epine_Mayenne_21_04_2013_26min12.mp4";
  "039_IVe_dimanche_apres_Paques_Attachement_apostolique_a_NS_pour_remonter_de_sa_nature_humaine_a_sa_Divinite_28_04_1991_23min03.mp4";
  "040_Ve_dimanche_apres_Paques_Dieu_console_par_les_siens_Tours_30_04_1989_20min42.mp4";
  "041_Ascension_24_05_1990_25min18.mp4";
  "042_Ascension_Moulins_09_05_1991_27min55.mp4";
  "043_Ascension_2011_Vendee_20min32.mp4";
  "044_Ascension_ND_de_l_Epine_10_05_2018_26min12.mp4";
  "045_Dimanche_dans_l_Octave_de_l_Ascension_12_05_2013_Mayenne_ND_de_l_Epine_15min28.mp4";
  "046_Dimanche_dans_l_Octave_de_l_Ascension_2014_Vendee_19min.mp4";
  "047_Pentecote_Moulins_19_05_1991_29min18.mp4";
  "048_Pentecote_Vendee_19_05_2013_30min45.mp4";
  "049_Pentecote_ND_de_l_Epine_15_05_2016_30min_50.mp4";
  "050_Tres_Sainte_Trinite_Moulins_16_06_1990_33min05.mp4";
  "051_Fete_du_Tres_Saint_Sacrement_Moulins_02_06_1991_28min53.mp4";
  "052_Dimanche_dans_l_octave_du_Saint_Sacrement_Notre_Dame_de_l_Epine_2_06_2013_14min26.mp4";
  "053_Solennite_du_Sacre_Coeur_et_Saint_Jean_Baptiste_Il_faut_qu_Il_croisse_et_que_je_diminue_Moulins_24_06_1990_24min55.mp4";
  "054_Dimanche_dans_l_Octave_du_Sacre_Coeur_29min09.mp4";
  "055_IVe_dimanche_apres_la_Pentecote_Moulins_16_06_1991_31min54.mp4";
  "056_Ve_dimanche_apres_la_Pentecote_Sur_reparation_et_componction_Tours_18_06_1989_34min59.mp4";
  "057_VIe_dimanche_apres_la_Pentecote_Saint_Henri_et_la_sanctification_dans_le_monde_Moulins_15_07_1990_37min39.mp4";
  "058_VIIe_Dimanche_apres_la_Pentecote_Mayenne_ND_de_l_Epine_07_07_2013_36min04.mp4";
  "059_VIIIe_dimanche_apres_la_Pentecote_Saint_Bonaventure_et_le_bonheur_en_Dieu_seul_Moulins_14_07_1991_42min06.mp4";
  "060_IXe_dimanche_apres_la_Pentecote_Mayenne_ND_de_l_Epine_21_07_2013_34min55.mp4";
  "061_Xe_dimanche_apres_la_Pentecote_Sur_le_Principe_et_Fondement_Tours_31_07_1988_22min08.mp4";
  "062_XIe_dimanche_apres_la_Pentecote_04_08_1991_33min37.mp4";
  "063_XIe_Dimanche_apres_Pentecote_12_08_2012_15min44.mp4";
  "064_XIIe_dimanche_apres_la_Pentecote_11_08_2013_27min32.mp4";
  "065_XIIIe_dimanche_apres_la_Pentecote_18_08_2013_27min34.mp4";
  "066_XIVe_dimanche_apres_la_Pentecote_Saint_Louis_25_08_1991.mp4";
  "067_XVe_dimanche_apres_la_Pentecote_Foi_en_la_Divinite_de_NS_Moulins_16_09_90_27min23.mp4";
  "068_XVIe_dimanche_apres_la_Pentecote_03_09_89_24min15.mp4";
  "069_XVIIe_Dimanche_apres_la_Pentecote_ND_de_l_Epine_16_09_2018_40_min.mp4";
  "070_XVIIIe_Dim_apres_la_Pentecote_ND_de_l_Epine_8_10_2017_38min30.mp4";
  "071_XIXe_dimanche_apres_la_Pentecote_Sur_la_colere_24_09_89_22min01.mp4";
  "072_XXe_D_ap_Pent_22_10_2017_ND_de_l_Epine_1h.mp4";
  "073_Fete_du_Christ_Roi_27_10_1991_27min22.mp4";
  "074_XXIe_dimanche_apres_Pentecote_25min24.mp4";
  "075_XXIIe_Dimanche_apres_la_Pentecote_ND_de_l_Epine_21_10_2018_36_min_35.mp4";
  "076_XXIIIe_dimanche_apres_la_Pentecote_Sur_le_Purgatoire_11_11_90.mp4";
  "077_XXIVe_et_dernier_dimanche_apres_la_Pentecote_Moulins_26_11_1989_26min38.mp4";
  "078_XXIVe_ap_Pent_IVe_ap_Eph_Tempete_apaisee_Moulins_29min55.mp4";
  "079_XXVe_dim_ap_Pent_Ve_ap_Epiphanie_Moulins_04_02_1990_20min.mp4";
  "080_XXVIe_dim_apres_la_Pentecote_VIe_ap_Epiphanie_17_11_1991_30min38.mp4";
  "081_Solennite_du_Tres_Saint_Rosaire_Tours_08_10_1989_23min31.mp4";
  "082_Maternite_Divine_de_Notre_Dame_11_10_2015_Mayenne_ND_de_l_Epine_28min20.mp4";
  "083_Toussaint_Moulins_01_11_1996_30min49.mp4";
  "084_Commemoration_des_defunts_Vendee_2_novembre_2012_21min07.mp4";
  "085_Fete_de_la_Dedicace_de_Saint_Jean_de_Latran_09_11_2014_33min47.mp4";
  "086_Solennite_de_l_Immaculee_Conception_Moulins_1989_30min05.mp4";
  "087_Presentation_de_NS_au_temple_et_Purification_de_Marie_Mayenne_ND_de_l_Epine_02_02_2014_23min13.mp4";
  "088_Decouverte_de_la_Sainte_Croix_ND_de_l_Epine_3_05_2015_34min28.mp4";
  "089_St_Philippe_et_st_Jacques_ND_de_l_Epine_11_05_2014_18min42_.mp4";
  "090_Solennite_de_Ste_Jeannes_d_Arc_Moulins_13_05_1990_27min49.mp4";
  "091_VIe_dimanche_apres_la_Pentecote_Solennite_de_St_Pierre_et_St_Paul_Monde_conquis_de_haute_lutte_par_papes_et_martyrs_Moulins_30_06_1991_42min50.mp4";
  "092_Fete_de_Sainte_Anne_Mayenne_ND_de_l_Epine_26_07_2015_42min41.mp4";
  "093_Saint_Laurent_diacre_et_martyr_Vendee_10_08_2014_33min24.mp4";
  "094_Fete_de_saint_Luc_Mayenne_ND_de_l_Epine_18_10_2015_27min09.mp4";
  "095_Sur_la_maniere_de_precher_1988_22min32.mp4";
  "096_Assomption_Moulins_15_08_1991_29min15.mp4"; "097_Assomption_2012.mp4";
  "098_Assomption_2013_Vendee_21min34.mp4";
  "099_Saint_Joachim_Pere_de_la_TS_Vierge_Marie_Vendee_16_08_2015_24min03.mp4";
  "100_Tres_Precieux_Sang_Mayenne_ND_de_l_Epine_1er_juillet_2013_27min03.mp4";
  "101_Nativite_de_Notre_Dame_Moulins_08_09_1991_30min28.mp4";
  "102_Notre_Dame_des_sept_douleurs_15_09_1996_15min51.mp4";
  "103_Solennite_de_Ste_Therese_de_l_Enfant_Jesus_et_de_la_Ste_Face_Moulins_30_09_1990_22min47.mp4";
  "104_Solennite_de_saint_Michel_Archange_Moulins_29_09_1991_32min31.mp4";
  "105_IVe_D_ap_Pentecote_ND_de_l_Epine_28_juin_2020_35min.mp4";
  "106_VIe_D_ap_Pentecote_ND_de_l_Epine_12_7_2020_39min25.mp4";
  "107_refutation_T_de_M_ete_2013_Intro_10min.mp4";
  "108_refutation_T_de_M_ete_2013_partie_1_18min19.mp4";
  "109_refutation_T_de_M_ete_2013_partie_2_18min41.mp4";
  "110_refutation_T_de_M_ete_2013_partie_3_23min22.mp4";
  "111_refutation_T_de_M_ete_2013_partie_4_14min54.mp4";
  "112_refutation_T_de_M_ete_2013_partie_5_21min57.mp4";
  "113_refutation_T_de_M_ete_2013_partie_6_21min51.mp4";
  "114_refutation_T_de_M_ete_2013_partie_7_17min02.mp4";
  "115_VIIIe_D_Ap_Pent_7_8_2011_a_La_Boutouere_Mayenne_21min57.mp4";
  "116_XVIe_Dimanche_apres_la_Pentecote_ND_de_lEpine_20_09_2020_22min14.mp4";
  "117_Christ_Roi_25_10_2020_ND_de_lEpine_45min29.mp4";
  "130_Presentation_des_ouvrages_de_labbe_Interesse_video_013_1h01min40s.mkv";
  "131_debat_celier_zins_30min47.mp4";
  "132_Jeudi_Saint_Vendee_2015_14min53.mp4";
  "133_Jeudi_Saint_2016_5min32.mp4";
  "134_Sermon_Jeudi_Saint_13_04_2017_31min27.mp4";
  "135_Fete_de_lEvangeliste_Saint_Marc_25_04_2021.mp4";
  "136_refutation_T_de_M_ete_2013_version_longue_pars_01_1h10min43s.mp4";
  "137_refutation_T_de_M_ete_2013_version_longue_pars_02_1h15min44s.mp4";
  "138_debat_celier_zins_30min48s.mp4";
  "139_Pentecote_Vendee_27_05_2018_38_min.mp4";
  "140_Fete_de_la_TS_Trinite_ND_de_lEpine_30_mai_2021_38min.mp4";
  "141_Saint_Jacques_le_Majeur_ND_de_lEpine_21_07_2021_23min54.mp4";
  "142_Coeur_Immacule_de_Marie_ND_de_lEpine_55min44_22_08_2021.mp4";
  "143_Assomption_2021_ND_de_lEpine_46min29.mp4";
  "144_IVe_dimanche_de_Careme_Laetare_Definition_de_la_Charite_05_03_89.mp4";
  "146_XVIe_Dimanche_apres_la_Pentecote_ND_de_lEpine_20_09_2020.mp4";
  "147_XVIe_D_ap_Pentecôte_ND_de_lEpine_25_9_2022.mp4";
  "148_35_Christiade_01.mp4"; "149_36_Refutation_fou_these_02.mp4";
  "150_37_Christiade_02.mp4"; "151_38_Refutation_fou_these_03.mp4";
  "152_Fete_de_Saint_Michel_Archange_29_09_2019_31min.mp4";
  "153_Solennite_du_Rosaire_Vendee_06_10_2019_15min15.mp4"]
;;  

(*
let check_v1 = (v1=naive_v1) ;;
*)

let bigger_fountain_of_titles = Image.image (
  fun s ->
     let k = Option.get(Substring.leftmost_index_of_in_from_opt "_" s 1) in
     (ios (Cull_string.beginning (k-1) s),
     Cull_string.coending 4 (Cull_string.cobeginning k s)
     )
) v1 ;;
let fountain_of_titles = List.filter (
  fun (j,_)->j<=143
) bigger_fountain_of_titles ;;

let retained_titles = List.filter (
   fun (j,_)-> List.mem j [
      4;5;
   ]
) fountain_of_titles ;;


let unordered_numbered_titles_part2 =   
  [(33, "Dimanche des Rameaux En Vendee 01 04 2012");
  (27, "Ier Dimanche de Carême Sur la Pénitence. Moulins 04/03/1990. 24min53");
  (26,
   "Ier Dimanche de Carême. Sens mystique des montées vers Jérusalem 16/02/1997 15min18");
  (29,
   "IIIème dimanche de Carême. Contre le démon muet. Moulins, 10/03/1996, 26min07");
  (28,
   "IIème Dim. de Carême. Transfiguration. Équilibre entre désolations et consolations.");
  (5,
   "IVe  Dimanche de l’Avent - Moulins 23-12-1990 -  Comparaison du temps des Patriarches avec le nôtre.");
  (39,
   "IVe D. après Pâques :  Attachement apostolique à NS et remontée de sa nature humaine à sa Divinté.");
  (0,
   "IVe Dimanche de Carême - Laetare - Définition de la Charité - Tours - 05-03-1989");
  (134, "Sermon Jeudi Saint 13 04 2017 31min27");
  (0, "Sermon sur l'Ascension à Moulins 24 05 1990");
  (53,
   "Solennité du Sacré Coeur et Saint Jean Baptiste - Il faut qu'Il croisse et que je diminue 24/6/1990");
  (34,
   "Veillée pascale Sur l'illogisme de l'attitude actuelle des Juifs talmudistes Moulins 11 04 1998")]
;;    

let numbered_titles_part2 = 
  Ordered.sort  pair_order  unordered_numbered_titles_part2 ;;

let remaining_titles_in_part2 = List.filter (
  fun (j,_) -> j = 0
) numbered_titles_part2;;

let remaining_indices_not_in_part1 = List.filter (
  fun j -> j > 80
) indices_not_in_part1;;

let data = (remaining_indices_not_in_part1,remaining_titles_in_part2) ;;



(************************************************************************************************************************
Snippet 88 : Remove lines starting with a # in a file (can be used with ocamllex)
************************************************************************************************************************)
let ap3 = Absolute_path.of_string "Fads/jug.ml";; 
let text3 = Io.read_whole_file ap3 ;;
let lines = Lines_in_text.lines text3 ;; 
let good_lines = List.filter (fun line->not(String.starts_with ~prefix:"#" line )) lines ;;
let new_text3 = String.concat "\n" good_lines ;; 
Io.overwrite_with ap3 new_text3 ;; 

(************************************************************************************************************************
Snippet 87 : Extract token types from a .mli file 
************************************************************************************************************************)
let ap1 = Absolute_path.of_string "Fads/Extract_php_lexer_from_padioleau/Originals/parser_php.mly" ;; 
let u1 = Io.read_whole_file ap1 ;;
let u2 = Lines_in_text.interval u1 110 236 ;;
let u3 = Outside_ocaml_comments_and_strings.good_substrings u2 ;; 
let u4 = String.concat " " (Image.image (fun (_,_,s,_)->s) u3) ;;
let u5 = Substring.occurrences_of_in "%token" u4 ;; 
let last_elt_in_u5 = List.hd(List.rev u5) ;; 
let u6 = (List_again.universal_delta_list u5) @ [last_elt_in_u5,(String.length u4)+1];; 
let u7 = Image.image (fun (i,j)-> Cull_string.interval u4 i (j-1)) u6;; 
let u8 = Image.image (
  fun t->
     let j = Option.get(Substring.leftmost_index_of_in_from_opt ">" t 9) in 
     (Cull_string.interval t 9 (j-1),Cull_string.cobeginning j t)
) u7 ;;
let u9 = Image.image (fun (typename,l)->(typename,Str.split (Str.regexp"[ \t\r\n]+") l)) u8;;    
let u10 = List.flatten (Image.image (fun (typename,l)->Image.image (fun x->(typename,x)) l) u9) ;;
let u11 = Image.image (fun (typename,tokname)->(tokname,typename)) u10 ;; 
let u12 = Ordered.sort Total_ordering.lex_for_strings  (Image.image fst u11) ;;
let u13 = Image.image (fun t->"| "^ t ^ " of " ^ (List.assoc t u11)) u12 ;;
let u14 = String.concat "\n" u13 ;; 
let u15 () = print_string ("\n\n\n" ^ u14 ^ "\n\n\n") ;;


(************************************************************************************************************************
Snippet 86 : Musings on a random walk (version 2, with stopping times)
************************************************************************************************************************)
let first_base = Memoized.make (fun n->Cartesian.general_product 
 (Int_range.scale (fun k->[-2;1]) 1 n)
) ;;

let rec helper_for_stopping_time (threshhold,current_sum,to_be_treated) = 
   match to_be_treated with 
    [] -> 0
   | (idx,x) :: others ->
      let new_sum = current_sum + x in 
      if new_sum >= threshhold 
      then idx 
      else helper_for_stopping_time (threshhold,new_sum,others) ;;   

let compute_stopping_time  threshhold l =
  helper_for_stopping_time (threshhold,0,Int_range.index_everything l) ;;     

let admissible_cases = Memoized.make(fun n->
    List.filter (fun l->(compute_stopping_time 1 l)=n) (first_base n)
) ;;  

let measure n = 
    let p1 = List.length(admissible_cases n) in 
    ((p1,Basic.power 2 n),(float_of_int p1)*.(0.5**(float_of_int n))) ;; 

let float_fold_sum = List.fold_left (fun x y -> x+.y) (0.) ;;     

let u1 = Int_range.scale (fun j->3*j-2) 1 6 ;;
let u2 = Image.image (fun x->snd(measure x)) u1 ;;
let u3 = Int_range.scale (fun j->float_fold_sum(List_again.long_head j u2)) 1 6 ;;
let u4 = Image.image (fun x->fst(measure x)) u1 ;;
let u5 = Image.image (fun (a,b)->let g=Gcd.gcd a b in (a/g,b/g)) u4 ;; 


let ac = admissible_cases ;; 


(************************************************************************************************************************
Snippet 85 : Musings on a random walk
************************************************************************************************************************)
type dyadic = D of int * int ;; 
type linear_combination = LC of (dyadic * int ) list ;;

let rec helper_for_nuu (expo,walker) =
    if walker mod 2 = 1 
    then (expo,walker)
    else helper_for_nuu (expo+1,walker/2) ;;   

let nuu n = helper_for_nuu (0,n) ;;


module Dyadic = struct 

let frac x ex = let (d,new_x) = nuu x in D(new_x,d+ex) ;;   

let sum_in_prepared_case (D(a,ea)) (D(b,eb))=  
  (* we assume ea < eb *)
  let new_a = (Basic.power 2 (eb-ea)) * a  in 
  frac (new_a+b) eb ;;

let sum dya dyb =
  let (D(a,ea))=dya and (D(b,eb))=dyb in    
  if ea = eb then frac (a+b) ea else 
  if ea < eb 
  then sum_in_prepared_case dya dyb 
  else sum_in_prepared_case dyb dya ;;
  
let prod (D(a,ea)) (D(b,eb))= D(a*b,ea+eb) ;;      
let half (D(x,ex)) = D(x,ex+1) ;; 

let fold_sum l = List.fold_left sum (D(0,1)) l ;;  


end ;;

module Linear_Combination = struct 

let rec helper_for_sum (treated,to_be_treated1,to_be_treated2) =
  match to_be_treated1 with 
  [] -> List.rev(List.rev_append to_be_treated2 treated)
  | (d1,idx1) :: others1 -> 
     (
      match to_be_treated2 with 
      [] -> List.rev(List.rev_append to_be_treated1 treated)
      | (d2,idx2) :: others2 -> 
          if idx1<idx2 then helper_for_sum ((d1,idx1) ::treated,others1,to_be_treated2) else 
          if idx2<idx1 then helper_for_sum ((d2,idx2) ::treated,to_be_treated1,others2) else   
            helper_for_sum ((Dyadic.sum d1 d2,idx1) ::treated,others1,others2) 
     ) ;;

let sum (LC l1) (LC l2) = (LC(helper_for_sum([],l1,l2))) ;;
let zero = LC [] ;;
let fold_sum l = List.fold_left sum zero l ;;  

end ;;  

let peggy_transform (LC l) = 
  Linear_Combination.fold_sum (Image.image (fun (d,idx)->
      if idx = 0 then LC[d,idx] else 
       LC[Dyadic.half d,idx-1;Dyadic.half d,idx+2] 
    ) l) ;;

let ff = Memoized.small peggy_transform (LC[D(1,0),1]) ;;   

let gg = Memoized.make (fun n->
   let (LC temp1) = ff n in 
   let dys = Dyadic.fold_sum(Image.image fst (List.tl temp1)) in 
   let (D(s,es)) = dys in
   (dys,(float_of_int(s))*.(0.5**(float_of_int es))) 
  ) ;; 

(************************************************************************************************************************
Snippet 84 : Read a file and remove tabs in each line
************************************************************************************************************************)
let ap3 = Absolute_path.of_string "Fads/pan.ml" ;;
let z4 = Io.read_whole_file ap3 ;;
let z5 = Lines_in_text.interval z4 9 21 ;;
let z6 = Lines_in_text.lines z5 ;; 
let z7 = Image.image String.lowercase_ascii z6 ;;
let z8 = Ordered.sort Total_ordering.lex_for_strings z7 ;;
let z9 = Image.image (Str.split (Str.regexp_string "\t")) z6;;
let z10 = Image.image (
   fun l -> let nth  = (fun k->Cull_string.trim_spaces(List.nth l (k-1))) in 
   (int_of_string(nth 1),nth 2)
) z9 ;; 
let z10 = [(53, "Veni de Libano"); (55, "Paulus"); (56, "Pitra"); (57, "Fulgurator");
(59, "Florent-Jean"); (71, "Comte Bavon"); (69, "Charles"); (64, "Francis");
(67, "Amos"); (68, "chaussis"); (79, "HALLELUIA"); (75, "luern");
(80, "Adeodato")] ;;

let z11 = String.concat "," (Image.image snd z10) ;;

(************************************************************************************************************************
Snippet 83 : Musing on the Alon-Knuth theorem (episode 3). Contains some code 
to compute the maximal elts in an upwards filter and the minimal elts outside
it (see the "butterfly" function). It should work even for a filter on a large
base set, as long as the minimal and maximal elts have small size and are not
too numerous.
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let i_fold_intersect = Ordered.fold_intersect i_order ;;
let i_intersects = Ordered.intersects i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_setminus = Ordered.setminus i_order ;;
let i_sort = Ordered.sort i_order ;;


let il_order = Total_ordering.silex_for_intlists ;;
let il_fold_merge = Ordered.fold_merge il_order ;;
let il_setminus = Ordered.setminus il_order ;;
let il_sort = Ordered.sort il_order ;;

let u1 = Int_range.scale (fun t->[-1;1]) 1 5 ;;
let u2 = Cartesian.general_product u1 ;;
let base = Image.image (fun l-> 1 :: l) u2 ;;
let indexed_base = Int_range.index_everything base ;; 
let sp l1 l2 =
   let temp1 = List.combine l1 l2 in 
   Basic.fold_sum(Image.image (fun (x,y)->x*y) temp1) ;;
   

let ker uple =
    List.filter_map (
     fun (j,uple2) -> if sp uple uple2 = 0 then Some j else None
    ) indexed_base;; 

let i_ker_for_individual k = ker (List.nth base (k-1)) ;;      
let i_ker l =
    let temp1 = Image.image i_ker_for_individual l in 
    i_fold_intersect temp1 ;;
              
let unexpanded_data = 
   Image.image (fun (j,uple)->(j,ker uple)) indexed_base ;;

let all_complements n ll=
Image.image (fun z->i_setminus (Int_range.range 1 n) z)  ll ;;

let merged_power_sets ll=
  let temp1 = Image.image (fun l->il_sort(List_again.power_set l)) ll in 
  il_fold_merge temp1 ;; 

let butterfly_operation ll addedum = 
   let test_f = (
    fun x->let enhanced_x = i_sort (x@addedum) in 
    i_ker (enhanced_x) <> []
   ) in   
  let (inert,ramified) = List.partition test_f ll in 
    let below_ramified = merged_power_sets ramified in 
    let temp4 = List.filter test_f   below_ramified in 
    let temp5 = Ordered_misc.maximal_elts_wrt_inclusion temp4 in  
    let temp6 = Image.image (fun x->i_sort (x@addedum) ) inert 
    and temp7 = Image.image (fun x->i_sort (x@addedum) ) temp5 in 
    let temp8 = List.filter (fun x->not(test_f x))  below_ramified in 
    let temp9 = Ordered_misc.minimal_elts_wrt_inclusion temp8 in  
    let temp10 = Image.image (fun x->i_sort (x@addedum) ) temp9 in 
    let pre_whole = il_sort(ramified@temp6@temp7) in 
    (
      ramified,temp6,temp7, 
      Ordered_misc.maximal_elts_wrt_inclusion pre_whole,
      temp10
    ) ;;

let tt n = List.filter (fun k->i_ker [k;n]=[]) (Int_range.range 1 (n-1)) ;; 


let g_10_part1=[[1;4;6;10];[1;4;7;10];[1;6;7;10];[4;6;7;10]] ;;
let g_10_part2=[[2;3;5;9];[2;3;8;9];[2;5;8;9];[3;5;8;9]] ;;
let g_10 = il_sort(g_10_part1 @ g_10_part2) ;;

let b_10_part1 = Image.image (fun (a,b)->[a;b]) 
(Cartesian.product [1;4;6;7;10] [2;3;5;8;9]) ;;
let b_10_part2 = [[1;4;6;7];[2;3;5;8]] ;;
let b_10 = il_sort(b_10_part1 @ b_10_part2) ;;

let t_10_1 = Ordered_misc.minimal_transversals b_10 ;;
let check_gb_10 = (il_sort(all_complements 10 t_10_1))=g_10 ;;  

butterfly_operation g_10_part1 [11];;

let g_11_part1=[[1;4;6;10];[1;4;6;11];[1;4;7;10];[1;4;7;11];
[1;6;7;10;11];[4;6;7;10;11]] ;;
let g_11_part2=[[2;3;5;9];[2;3;8;9];[2;5;8;9];[3;5;8;9]] ;;
let g_11 = il_sort(g_11_part1 @ g_11_part2) ;;

let b_11_part1 = Image.image (fun (a,b)->i_sort[a;b]) 
(Cartesian.product [1;4;6;7;10;11] [2;3;5;8;9]) ;;
let b_11_part2 = [[1;4;6;7];[2;3;5;8];[1;4;10;11]] ;;
let b_11 = il_sort(b_11_part1 @ b_11_part2) ;;

let t_11_1 = Ordered_misc.minimal_transversals b_11 ;;
let check_gb_11 = (il_sort(all_complements 11 t_11_1))=g_11 ;; 

butterfly_operation g_11_part2 [12];;

let g_12_part1=[[1;4;6;10];[1;4;6;11];[1;4;7;10];[1;4;7;11];
[1;6;7;10;11];[4;6;7;10;11]] ;;
let g_12_part2=[[2;3;5;9];[2;3;5;12];[2;3;8;9];[2;3;8;12];[2;5;8;9;12];[3;5;8;9;12]] ;;
let g_12 = il_sort(g_12_part1 @ g_12_part2) ;;

let b_12_part1 = Image.image (fun (a,b)->i_sort [a;b]) 
(Cartesian.product [1;4;6;7;10;11] [2;3;5;8;9;12]) ;;
let b_12_part2 = [[1;4;6;7];[2;3;5;8];[2;3;9;12];[1;4;10;11]] ;;
let b_12 = il_sort(b_12_part1 @ b_12_part2) ;;

let t_12_1 = Ordered_misc.minimal_transversals b_12 ;;
let check_gb_12 = (il_sort(all_complements 12 t_12_1))=g_12 ;; 

butterfly_operation g_12_part1 [13];;

let g_13_part1=[[1; 4; 6; 10]; [1; 4; 7; 11]; [1; 6; 7; 13]; [1; 10; 11; 13];
[1; 4; 6; 11; 13]; [1; 4; 7; 10; 13]; [1; 6; 7; 10; 11];
[4; 6; 7; 10; 11; 13]] ;;
let g_13_part2=[[2;3;5;9];[2;3;5;12];[2;3;8;9];[2;3;8;12];[2;5;8;9;12];[3;5;8;9;12]] ;;
let g_13 = il_sort(g_13_part1 @ g_13_part2) ;;

let b_13_part1 = Image.image (fun (a,b)->i_sort [a;b]) 
(Cartesian.product [1;4;6;7;10;11;13] [2;3;5;8;9;12]) ;;
let b_13_part2 = [[1;4;6;7];[1;4;10;11];[1;6;10;13];[1;7;11;13];
[2;3;5;8];[2;3;9;12]] ;;
let b_13 = il_sort(b_13_part1 @ b_13_part2) ;;

let t_13_1 = Ordered_misc.minimal_transversals b_13 ;;
let check_gb_13 = (il_sort(all_complements 13 t_13_1))=g_13 ;; 

let (_,_,_,r_14_good,r_14_bad) = butterfly_operation g_13_part2 [14];;

let g_14_part1= g_13_part1 ;;
let g_14_part2= r_14_good ;;
let g_14 = il_sort(g_14_part1 @ g_14_part2) ;;

let b_14_part1 = Image.image (fun (a,b)->i_sort [a;b]) 
(Cartesian.product [1;4;6;7;10;11;13] [2;3;5;8;9;12;14]) ;;
let b_14_part2 = il_sort (b_13_part2 @ r_14_bad) ;;
let b_14 = il_sort(b_14_part1 @ b_14_part2) ;;

let t_14_1 = Ordered_misc.minimal_transversals b_14 ;;
let check_gb_14 = (il_sort(all_complements 14 t_14_1))=g_14 ;; 


(*

let h1 = il_sort(all_complements 13 t_13_1) ;;
let see1 = il_setminus h1 g_13 ;;
let see2 = il_setminus g_13 h1;;

let good1 = [1;6;7;13] ;;
let c_good1 = i_setminus (Int_range.range 1 13) good1 ;; 
let see3 = List.filter (
   fun z-> not(i_intersects z c_good1)
) b_13 ;;
let see4 = List.filter (fun t->i_is_included_in t c_good1) t_13_1 ;; 

let ll = g_12_part1 
and addendum = [13] ;;


let test_f = (
   fun x->let enhanced_x = i_sort (x@addendum) in 
   i_ker (enhanced_x) <> []
) ;;
let (inert,ramified) = List.partition test_f ll ;;
let below_ramified = merged_power_sets ramified ;;
let temp4 = List.filter test_f   below_ramified ;;
let temp5 = Ordered_misc.maximal_elts_wrt_inclusion temp4 ;; 
let temp6 = Image.image (fun x->i_sort (x@addendum) ) inert 
and temp7 = Image.image (fun x->i_sort (x@addendum) ) temp5 ;; 
   let temp8 = List.filter (fun x->not(test_f x))  below_ramified in 
   let temp9 = Ordered_misc.minimal_elts_wrt_inclusion temp8 in  
   let temp10 = Image.image (fun x->i_sort (x@addendum) ) temp9 in 
   let pre_whole = il_sort(ramified@temp6@temp7) in 
   (
     ramified,temp6,temp7, 
     Ordered_misc.maximal_elts_wrt_inclusion pre_whole,
     temp10
   ) ;;

*)   

(************************************************************************************************************************
Snippet 82 : Musing on the Alon-Knuth theorem (episode 2)
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let i_fold_intersect = Ordered.fold_intersect i_order ;;
let i_sort = Ordered.sort i_order ;;


let il_order = Total_ordering.silex_for_intlists ;;
let il_sort = Ordered.sort il_order ;;

let p_order = Total_ordering.product i_order il_order ;;
let p_sort = Ordered.sort p_order ;;

let u1 = Int_range.scale (fun t->[0;1;2]) 1 3 ;;
let u2 = Cartesian.general_product u1 ;;
let u3 = Image.image (fun l->(Basic.fold_sum l,l)) u2 ;;
let u4 = p_sort (List.filter (fun (s,l)->s<=2) u3) ;; 
let u5 = List.rev(Image.image snd (List.tl u4)) ;;
let u6 = Image.image (Image.image (fun t->2*t)) u5;;

let tf1 (a1,a2,a3,b1,b2,b3) =
  (b3*a2 + b2*a3, b3*a1 + b1*a3, b2*a1 + b1*a2) ;;

let tf2 uple =
   let (u,v,w) = tf1 uple in (u*u,v*v,w*w) ;;

let u1 = Int_range.scale (fun t->[-1;1]) 1 6 ;;
let u2 = Cartesian.general_product u1 ;;
let u3 = Image.image (
     fun l->
      let nth = (fun k->List.nth l (k-1)) in 
      (nth 1,nth 2,nth 3,nth 4,nth 5,nth 6)
  ) u2 ;;
let u4 = Image.image tf1 u3 ;; 
let u5 = Image.image (fun (a,b,c)->[a;b;c]) u4 ;;
let zero_vector = [0;0;0] ;;
let whole = il_sort (zero_vector::u5) ;; 
let shadow j = i_sort(Image.image (fun l->List.nth l (j-1)) whole) ;;
let shadows = Int_range.scale shadow 1 3 ;;
let base = Cartesian.general_product shadows ;;
let unattended = List.filter (fun z->not(List.mem z whole)) base ;;
let u6 = Int_range.index_everything unattended 




(************************************************************************************************************************
Snippet 81 : Musing on the Alon-Knuth theorem
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let i_fold_intersect = Ordered.fold_intersect i_order ;;
let i_sort = Ordered.sort i_order ;;


let il_order = Total_ordering.silex_for_intlists ;;
let il_sort = Ordered.sort il_order ;;

let u1 = Int_range.scale (fun t->[-1;1]) 1 3 ;;
let u2 = Cartesian.general_product u1 ;;
let u3 = Image.image (
   fun l->
    let nth = (fun k->List.nth l (k-1)) in 
    (1,nth 1,nth 2,nth 3)
) u2 ;;
let u4 = Int_range.index_everything u3 ;; 
let sp (a1,a2,a3,a4) (b1,b2,b3,b4) = a1*b1 + a2*b2 + a3*b3 + a4 * b4 ;;

let ker uple =
    List.filter_map (
     fun (j,uple2) -> if sp uple uple2 = 0 then Some j else None
    ) u4;; 

let i_ker k = ker (List.nth u3 (k-1)) ;;      
let i_ker_for_several l =
    let temp1 = Image.image i_ker l in 
    i_fold_intersect temp1 ;;
              
let unexpanded_data = 
   Image.image (fun (j,uple)->(j,ker uple)) u4 ;;

let elementary_extension l j = 
    let new_idx = (List.length l)+1 in 
    let naive_possibilities_for_j = List.assoc new_idx unexpanded_data  in 
    if j= new_idx
    then Some(l @ [j,naive_possibilities_for_j])
    else 
    let possibilities_for_j = i_fold_intersect (naive_possibilities_for_j::(List.filter_map (
      fun (k,vals) -> if k=j then Some vals else None
     ) l)) in 
    if possibilities_for_j = [] then None else 
    let new_l = Image.image (fun 
    (k,vals)->if k=j then (k,possibilities_for_j) else (k,vals)) l in 
    Some(new_l @ [j,possibilities_for_j]) ;;     

let all_elementary_extensions  l = 
   let older_indices = i_sort(Image.image fst l) in      
   List.filter_map (
    elementary_extension l
   ) (older_indices@[ (List.length l)+1]) ;;

let involved_possibilities l = 
  let involved_indices = i_sort(Image.image fst l) in  
  Cartesian.general_product (Image.image (fun j->List.assoc j l) involved_indices) ;;

let ff = Memoized.recursive (
   fun old_f d->if d<2 then [[List.hd unexpanded_data]] else 
    List.flatten(Image.image all_elementary_extensions (old_f (d-1)))
)  ;;  

let z1 = List.filter (
   fun l->
    let older_indices = i_sort(Image.image fst l) in    
    List.length(older_indices)<=3
) (ff 7) ;;
let z2 = 
  il_sort(Image.image i_sort
  (List.flatten(Image.image involved_possibilities z1)));;



(************************************************************************************************************************
Snippet 80 : Musing on the simplicity of An
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let i_sort = Ordered.sort i_order ;;

let il_order = Total_ordering.silex_for_intlists ;;
let il_fold_merge = Ordered.fold_merge Total_ordering.silex_for_intlists ;;
let il_sort = Ordered.sort il_order ;;


let pr = Permutation.product ;;
let iv = Permutation.inverse ;;
let conj x g = pr g (pr x (iv g)) ;;
let cube x = pr x (pr x x) ;;
let nonfixed_items perm = 
    let temp1 = Int_range.index_everything perm in 
    List.filter_map (fun (x,y)->if x=y then None else Some x)temp1 ;;
let is_good x= 
 (nonfixed_items(cube x)=[]) && (List.length(nonfixed_items x)=3) ;;



let n0 = 7 ;;
let main = Permutation.alternating_group n0 ;;
let conjugates x = il_sort( Image.image (conj x) main) ;; 
let pikaboo x =
  let temp1 = Permutation.cyclic_subgroup x in 
  let temp2 = Image.image conjugates temp1 in 
  il_fold_merge temp2 ;;

let pookabi = Memoized.make(fun x ->
   let temp1 = pikaboo x in 
   let temp2 = Image.image (List_again.long_head 3) temp1 in 
   il_sort temp2 );;

let measure z = List.length(List.filter (fun x->x>3) z);;

let test_for_normal_form z =
    let temp1 = List.filter (fun x->x>3) z in 
    let m = List.length temp1 in 
    temp1 = Int_range.range 4 (m+3) ;; 

let prettify_atomic_result z =
  let temp1 = Image.image (fun i->if i<4 then string_of_int i else "*") z in 
   String.concat "" temp1 ;; 

let prettify_individual_result m ll =
  let temp1 =List.filter(fun z->(measure z=m)&&(test_for_normal_form z)) ll in 
  Image.image prettify_atomic_result temp1;;  

let expand_pookabi_result ll = 
  let temp1 = i_sort(Image.image measure ll) in 
  Image.image (fun m->(m,
  prettify_individual_result m ll)) temp1;;

let u1 = Image.image pookabi main ;;   

let boehme_order = Total_ordering.silex_compare Total_ordering.silex_for_intlists ;;
let u2 = Ordered.sort boehme_order u1 ;;
let u3 = Image.image (
  fun y->(y,List.filter (fun x->pookabi x=y) main)
) u2 ;;
let u4 = Image.image (fun (_,z)->List.length z) u3;;
let v1 = List.nth u2 5 ;;
let v2 = expand_pookabi_result v1;;





(************************************************************************************************************************
Snippet 79 : Define a cycle from list of successive elts
************************************************************************************************************************)
let cycle_from_perm perm =
  let n = List.length perm in 
  let idx = (fun x->List_again.find_index_of_in x perm) in 
  let next = (fun x->
     let i = idx x in 
     if i = n then List.hd perm else 
     List.nth perm i  
   ) in 
   Int_range.scale next 1 n ;;

(************************************************************************************************************************
Snippet 78 : An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. (Version 1)
************************************************************************************************************************)
module SN98 = struct 
open Needed_values ;;
module Bart_simpson = struct 
  
  type hook_in_knowledge = 
  Boundary_increment
 | Passive_repeat  
 | Fork ;;

type parametrized_uniform_subrange = {
 usr_positive_exceptions : int list ;
 usr_negative_exceptions : int list ;
 usr_modulus : int ;
 usr_usual :  int list ;
} ;; 

type  parametrized_subrange = {
 ps_exceptions : (int * (int list)) list ;
 ps_usual : parametrized_uniform_subrange ; 
} ;; 

type parametrized_ps_list = {
 pl_exceptions : (int * (int list list)) list ;
 pl_usual : parametrized_subrange list ;
} ;; 

type level_two_t = Quick of int list ;;   

end ;;

open Bart_simpson ;;

let i_order = Total_ordering.for_integers ;;
let i_insert = Ordered.insert i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_setminus = Ordered.setminus i_order ;;

let il_order = Total_ordering.silex_for_intlists ;;
let il_merge = Ordered.merge il_order ;;
let il_sort = Ordered.sort il_order ;;

let concretize (n,scrappers) = i_setminus (Int_range.range 1 n) scrappers ;; 

let test_for_admissibility_up_to_max_with max_width z =
  if max_width<1 then true else 
  Sz_preliminaries.test_for_admissibility (Sz_max_width_t.MW (max_width)) z ;;

let test_for_admissiblity width breadth z =
   (test_for_admissibility_up_to_max_with (width-1) z)
   &&
   (List.for_all (fun t->
    not(i_is_included_in [t;t+width;t+2*width] z)) (Int_range.range 1 breadth))  ;;

let remove_one_element (n,scrappers) k=
  let new_scrappers = i_insert k scrappers in 
  if k <> n then (n,i_insert k scrappers) else 
  let new_z =  concretize (n,new_scrappers) in 
  let new_max = List.hd(List.rev new_z) in 
  (new_max,List.filter (fun t->t<new_max) new_scrappers) ;;



(*

remove_one_element (10,[3;7;8;9]) 10 ;;

*)

module Parametrized = struct 

let eval_uniform_subrange usr n =
  List.filter (
     fun k->
      if i_mem k usr.Bart_simpson.usr_negative_exceptions then false else  
      if i_mem k usr.Bart_simpson.usr_positive_exceptions then true  else 
      i_mem (k mod usr.Bart_simpson.usr_modulus)
      usr.Bart_simpson.usr_usual
  ) (Int_range.range 1 n) ;; 

let eval_subrange sr n =
   match List.assoc_opt n sr.Bart_simpson.ps_exceptions with 
   Some answer -> answer 
   | None ->
    eval_uniform_subrange sr.Bart_simpson.ps_usual n  ;;

let eval_ps_list psl n =
  match List.assoc_opt n psl.Bart_simpson.pl_exceptions with 
  Some answer -> answer 
  | None ->
   Image.image (fun sr->eval_subrange sr n) 
   psl.Bart_simpson.pl_usual ;;    

let eval_level_two (Quick l) scrappers n =
  let z = concretize (n,scrappers) in 
  if (not(i_is_included_in l z))  
  then [z] 
  else 
  let temp1 = List.rev_map (fun t->i_setminus z [t]) l in 
  il_sort temp1 ;;     

end ;;   


module Parametrized_Example = struct 

  let uniform_subrange pe ne mdl usu = {
    Bart_simpson.usr_positive_exceptions = pe ;
    usr_negative_exceptions = ne ; 
    usr_modulus = mdl;
    usr_usual = usu ;
  };; 
  
  let subrange (sr_exns,pe,ne,mdl,usu) = {
    Bart_simpson.ps_exceptions = sr_exns ;
    ps_usual = uniform_subrange pe ne mdl usu ;
  };; 
  
  let ps_list psl_exns psl_usu = {
    Bart_simpson.pl_exceptions = psl_exns ;
    pl_usual = Image.image subrange psl_usu ;
  };; 

  (*
  let example1 = ps_list 
     [
       1,[[1]];
       2,[[1;2]];
     ]
     [
      ([],[],[3],1,[0]);
      ([],[],[2],1,[0]);
      ([],[],[1],1,[0]);
     ] ;;
  *)   

  let example1 = Quick [1;2;3] ;;

  let example2 (* for (1,2,[]) *) = ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
     ]
     [
      ([],[],[3],1,[0]);
      ([],[],[2],1,[0]);
     ] ;;  
     
  let example3 (* for (1,3,[]) *) = ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
       4,[[1;2;4];[1;3;4]];
     ]
     [
      ([],[],[3],1,[0]);
     ] ;;    

  let example4 (* for (1,2,[4]) *)= ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
       4,[[1;2];[1;3];[2;3]];
     ]
     [
      ([],[],[3;4],1,[0]);
      ([],[],[2;4],1,[0]);
      ([],[],[1;4],1,[0]);
     ] ;;   

  let example5 (* for (1,2,[5]) *)= ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
       4,[[1;2;4];[1;3;4]];
     ]
     [
      ([],[],[3;5],1,[0]);
      ([],[],[2;5],1,[0]);
     ] ;;  
     
  let example6 (* for (1,4,[]) *)= ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
       4,[[1;2;4];[1;3;4]];
       5,[[1;2;4;5]];
     ]
     [
      ([],[],[3;6],1,[0]);
      ([],[],[3;5],1,[0]);
      ([],[],[3;4],1,[0]);
      ([],[],[2;5],1,[0]);
      ([],[],[2;4],1,[0]);
      ([],[],[1;4],1,[0]);
     ] ;;   
     
   let example7 (* for (1,5,[]) *)= ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
       4,[[1;2;4];[1;3;4]];
       5,[[1;2;4;5]];
       6,[[1;2;4;5];[1;2;4;6];[1;2;5;6];
          [1;3;4;6];[1;3;5;6];[2;3;5;6]]
     ]
     [
      ([],[],[3;6],1,[0]);
      ([],[],[3;5],1,[0]);
      ([],[],[2;5],1,[0]);
     ] ;;      

    let example8 (* for (1,6,[]) *)= ps_list 
     [
       1,[[1]];
       2,[[1;2]];
       3,[[1;2];[1;3];[2;3]];
       4,[[1;2;4];[1;3;4]];
       5,[[1;2;4;5]];
       6,[[1;2;4;5];[1;2;4;6];[1;2;5;6];
          [1;3;4;6];[1;3;5;6];[2;3;5;6]];
       7,[[1;2;4;5;7];[1;2;4;6;7];[1;3;4;6;7]]   
     ]
     [
      ([],[],[3;6],1,[0]);
     ] ;;    

  end ;;   
  


let rose_hashtbl = Hashtbl.create 50 ;;
let medium_hashtbl = Hashtbl.create 50 ;;
let low_hashtbl = Hashtbl.create 50 ;;

let access width breadth (n,scrappers) = 
  let z = concretize (n,scrappers) in 
  if ((width,breadth)=(1,0))||(test_for_admissiblity width breadth z) 
  then Some [z] 
  else 
  match Hashtbl.find_opt rose_hashtbl (width,breadth) with 
  Some summary -> Some (Parametrized.eval_level_two summary scrappers n)
  | None ->  
  (match Hashtbl.find_opt medium_hashtbl (width,breadth,scrappers) with 
   Some summary -> Some (Parametrized.eval_ps_list summary n)
   | None -> Hashtbl.find_opt low_hashtbl (width,breadth,n,scrappers)) ;;

exception Boundary_increment_exn1 of int * int * int * (int list) ;;  
exception Boundary_increment_exn2 of int * int * int * (int list) ;; 
exception Boundary_increment_exn3 of int * int * int * (int list) ;; 

exception Passive_repeat_exn1 of int * int * int * (int list) ;; 
exception Passive_repeat_exn2 of int * int * int * (int list) ;; 

exception Fork_exn1 of int * int * int * (int list) ;; 
exception Fork_exn2 of int * int * int * (int list) ;; 
exception Fork_exn3 of int * int * int * (int list) ;; 
  
let compute_from_below (width,breadth,n,scrappers) tool =
     match tool with 
     Boundary_increment -> 
      let opt1 = access width breadth (remove_one_element (n,scrappers) n) in 
      if opt1 = None then raise(Boundary_increment_exn1(width,breadth,n,scrappers)) else  
      let pre1 = Option.get opt1 in 
      if List.mem n scrappers then raise(Boundary_increment_exn2(width,breadth,n,scrappers)) else
      let temp1 = List.filter_map (fun z->
         let new_z = z @ [n] in 
         if test_for_admissiblity width breadth new_z 
          then Some new_z
          else None
      )  pre1 in 
      if temp1=[]  then raise(Boundary_increment_exn3(width,breadth,n,scrappers)) else 
      temp1  
      | Passive_repeat ->
        let opt5 = access width (breadth-1) (n,scrappers)  in 
        if opt5 = None then raise(Passive_repeat_exn1(width,breadth,n,scrappers)) else     
        let pre5 = Option.get opt5 and b = breadth in   
        let temp5 = List.filter (fun z->
          not(i_is_included_in [b;b+width;b+2*width] z) 
       )  pre5 in 
       if temp5=[]  then raise(Boundary_increment_exn2(width,breadth,n,scrappers)) else 
       temp5  
      | Fork -> 
      let tempf = (fun k->
        remove_one_element  (n,scrappers)  (breadth+k*width) 
        )  and tempf2 = access width breadth in 
      let (m0,scr0) = tempf 0 and (m1,scr1) =  tempf 1 and (m2,scr2) =  tempf 2 in 
      let opt2 = tempf2 (m0,scr0)  
      and opt3 = tempf2 (m1,scr1) 
      and opt4 = tempf2 (m2,scr2) in 
      if opt2 = None then raise(Fork_exn1(width,breadth,n,scrappers)) else  
      if opt3 = None then raise(Fork_exn2(width,breadth,n,scrappers)) else    
      if opt4 = None then raise(Fork_exn3(width,breadth,n,scrappers)) else   
      let temp3 = List.flatten (Image.image Option.get [opt2;opt3;opt4]) in 
      let (_,temp4) = Max.maximize_it_with_care List.length temp3 in 
      il_sort temp4 ;; 


let low_add (width,breadth,n,scrappers,tool) =
   let res = compute_from_below (width,breadth,n,scrappers) tool in  
   let _ = Hashtbl.replace low_hashtbl (width,breadth,n,scrappers) res in 
   res ;;

let med_add (width,breadth,scrappers) summary = 
  Hashtbl.replace medium_hashtbl (width,breadth,scrappers) summary ;;

let rose_add (width,breadth) summary = 
    Hashtbl.replace rose_hashtbl (width,breadth) summary ;;  
 

let find_remote_stumbling_block_or_immediate_working_tool width breadth (n,scrappers) = 
  let opt5 = access width (breadth-1) (n,scrappers)  in 
  if opt5 = None then (Some(width,breadth-1,n,scrappers),None) else     
  let pre5 = Option.get opt5 and b = breadth in   
  let temp5 = List.filter (fun z->
      not(i_is_included_in [b;b+width;b+2*width] z) 
  )  pre5 in 
  if temp5<>[]  then (None, Some Passive_repeat) else 

    match access width breadth (n,scrappers) with 
    Some old_answer -> (None,None) 
    | None -> 
      let opt1 = access width breadth (remove_one_element (n,scrappers) n) in 
      if opt1 = None then (Some(width,breadth,n-1,scrappers),None) else  
      let pre1 = Option.get opt1 in 
      let temp1 = List.filter_map (fun z->
         let new_z = z @ [n] in 
         if test_for_admissiblity width breadth new_z 
          then Some new_z
          else None
      )  pre1 in 
      if temp1<>[]  then (None,Some Boundary_increment) else 
      let tempf = (fun k->
        remove_one_element  (n,scrappers)  (breadth+k*width) 
      )  in 
      let (m0,scr0) = tempf 0 and (m1,scr1) =  tempf 1 and (m2,scr2) =  tempf 2 in 
      let tempf2 = access width breadth in 
      let opt2 = tempf2 (m0,scr0)  
      and opt3 = tempf2 (m1,scr1) 
      and opt4 = tempf2 (m2,scr2) in 
      if opt2 = None then (Some(width,breadth,m0,scr0),None) else  
      if opt3 = None then (Some(width,breadth,m1,scr1),None) else    
      if opt4 = None then (Some(width,breadth,m2,scr2),None) else   
      (*
      let temp3 = List.flatten (Image.image Option.unpack [opt2;opt3;opt4]) in 
      let (_,temp4) = Max.maximize_it_with_care List.length temp3 in 
      *)
      (None,Some Fork) ;;

let rec first_needed_step_in_solution_opt (width,breadth,m0,scr0) =
    let (opt_stumbling_block,opt_tool) = 
    find_remote_stumbling_block_or_immediate_working_tool width breadth (m0,scr0) in 
    match opt_tool with 
    Some(tool) ->Some (width,breadth,m0,scr0,tool)
    | None -> (match opt_stumbling_block with 
                Some block ->first_needed_step_in_solution_opt(block) 
                |None -> None 
               )   ;; 
    
exception Quick_compute_exn of int * int * int * (int list) ;; 

let force_access width breadth (n,scrappers) = 
   match access width breadth (n,scrappers) with 
    None -> raise (Quick_compute_exn(width,breadth,n,scrappers)) 
   | Some answer -> answer;;

  
rose_add (1,1) Parametrized_Example.example1 ;; 


med_add (1,2,[]) Parametrized_Example.example2 ;; 
med_add (1,3,[]) Parametrized_Example.example3 ;; 

med_add (1,2,[4]) Parametrized_Example.example4 ;; 
med_add (1,3,[4]) Parametrized_Example.example4 ;; 
med_add (1,4,[4]) Parametrized_Example.example4 ;; 

med_add (1,2,[5]) Parametrized_Example.example5 ;; 
med_add (1,3,[5]) Parametrized_Example.example5 ;; 
med_add (1,4,[5]) Parametrized_Example.example5 ;; 

med_add (1,4,[])  Parametrized_Example.example6 ;; 
med_add (1,5,[])  Parametrized_Example.example7 ;; 
med_add (1,6,[])  Parametrized_Example.example8 ;; 

(*    

#use "Githubbed_archive/Szemeredi_problem/03_third_stab_at_szemeredi_problem.ml" ;;


*)

(*    

STEP 15+N : 

let (c_width,c_breadth,c_strappers) = (1,7,[]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;  
for k = 1 to 8 do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   


let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example8 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)

(*    

STEP 14 : 

let (c_width,c_breadth,c_strappers) = (1,6,[]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;  
for k = 1 to 8 do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   


let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example8 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)

(*    

STEP 13 : 

let (c_width,c_breadth,c_strappers) = (1,5,[]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;  
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   


let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example7 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)

(*    

STEP 12 : 

let (c_width,c_breadth,c_strappers) = (1,4,[]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;  
for k = 1 to 5 do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   
low_add (1, 4, 6, [], Fork) ;;
for k = 7 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Boundary_increment) in ()
done ;;  

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example6 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)


(*    

STEP 10: 

let (c_width,c_breadth,c_strappers) = (1,4,[5]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example5 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)



(*    

STEP 9: 

let (c_width,c_breadth,c_strappers) = (1,3,[5]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example5 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)


(*    

STEP 8: 

let (c_width,c_breadth,c_strappers) = (1,2,[5]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example5 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)



(*    

STEP 7: 

let (c_width,c_breadth,c_strappers) = (1,4,[4]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example4 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)


(*    

STEP 6: 

let (c_width,c_breadth,c_strappers) = (1,4,[4]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example4 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)



(*    

STEP 5: 

let (c_width,c_breadth,c_strappers) = (1,3,[4]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example4 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)


(*    

STEP 4 : 

let (c_width,c_breadth,c_strappers) = (1,2,[4]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example4 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)



(*    

STEP 3 : 

let (c_width,c_breadth,c_strappers) = (1,3,[]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (c_width, c_breadth, k, c_strappers, Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example3 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)


(*    

STEP 2 : 

let (c_width,c_breadth,c_strappers) = (1,2,[]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 
let bound = 20 ;;
for k = 1 to bound do 
  let _ = low_add (1, 2, k, [], Passive_repeat) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example2 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)


(*    

STEP 1 :

let (c_width,c_breadth,c_strappers) = (1,1,[]) ;;
let ff n = first_needed_step_in_solution_opt 
     (c_width,c_breadth,n,c_strappers) ;;    
let gg n = force_access  c_width c_breadth (n,c_strappers) ;; 

low_add (1, 1, 3, [], Fork) ;; 
let bound = 20 ;;
for k = 4 to bound do 
  let _ = low_add (1, 1, k, [], Boundary_increment) in ()
done ;;   

let u1 = Int_range.scale (fun k->(k,gg k,
  Parametrized.eval_ps_list 
     Parametrized_Example.example1 k
)) 1 bound ;;
let check_u1 = List.filter (fun (k,x,y)->x<> y) u1 ;;

*)
end ;; 



(************************************************************************************************************************
Snippet 77 : Code that lead to the discovery of a linear algorithm to compute, given any
finite set X of integers, the largest subset Y of X, containing no AP of length 3 and
width <=2. The algorithm uses an automaton with 8 states where the transitions are
the successive differences in Y.
************************************************************************************************************************)
open Needed_values ;;

let i_order = Total_ordering.for_integers ;;
let i_setminus = Ordered.setminus i_order ;;

let il_order = Total_ordering.silex_for_intlists ;;
let il_merge = Ordered.merge il_order ;;
let il_sort = Ordered.sort il_order ;;
let careful_merge x y = Ordered_misc.minimal_elts_wrt_inclusion (il_merge x y) ;;

let current_width = 2 ;;

let is_admissible = Sz_preliminaries.test_for_admissibility 
      (Sz_max_width_t.MW current_width) ;;

let force_insert_in m old_data = List.filter_map (
        fun old_elt ->
          let new_elt = old_elt @ [m] in 
          if is_admissible new_elt 
          then Some new_elt 
          else None 
    ) old_data ;;   
  
exception Middleman_exn of int list ;; 

let middleman = Memoized.recursive (fun old_f x-> 
  match List.rev x with 
  [] -> ([[]],[[]],[],[[]])
  | m :: other_than_m ->
     if other_than_m = [] 
     then ([[m]],[[m]],[[]],[[]])
     else   
     let y = List.rev other_than_m in 
     let (old_sols,old_rules,old_sols1,old_rules1) = old_f y in 
     let trial1 = force_insert_in m old_sols 
     and trial2 = force_insert_in m old_rules in 
     if trial1 <> [] 
     then  (trial1,trial2,
            il_merge (force_insert_in m old_sols1) old_sols,
            careful_merge (force_insert_in m old_rules1) old_rules )
     else
     if trial2 = [] 
     then (il_merge (force_insert_in m old_sols1) old_sols,
           careful_merge (force_insert_in m old_rules1) old_rules,
           old_sols1,[[]]) 
     else  raise (Middleman_exn(x))
) ;;

let measure x =  
    let (sols,rules,_,_) = middleman x in 
    List.hd sols;;      

 

let syndectical_power_set x = 
  let temp1 = List_again.power_set x in 
  let temp2 = List.filter (fun z->
     if z = [] then false else
     let ttemp3 = (List.hd z) :: (Arithmetic_list.delta z) in 
  List.for_all (fun t->t<=2) ttemp3) temp1 in 
  il_sort temp2 ;;  

let sps = Memoized.make (fun n->syndectical_power_set(Int_range.range 1 n)) ;;

let syndectical_merger x y = 
  let m = List.hd (List.rev x) in 
  x @ (Image.image (fun t->m+t) y) ;;

let relative_measure x small_part_of_x =
    let cropped_x = i_setminus x small_part_of_x in 
    (List.length(measure x)) - (List.length(measure cropped_x)) ;;

(*    
  let d_measure x =
    let almost_x = List.rev(List.tl(List.rev x)) in 
    (List.length(measure x)) - (List.length(measure almost_x)) ;;
*)

module First_attempt = struct 

let d_measure x = 
  let m = List.hd (List.rev x) in 
  relative_measure x [m] ;;

let diff x y = d_measure (syndectical_merger x y) ;; 

let left_base = sps 12 ;;

let small_n = 4 ;;
let right_base = sps small_n ;;
let shadow x = Image.image (diff x) right_base ;;
      
let u3 = Explicit.image  (fun z->(shadow z,z)) left_base ;;
let u4 = Partition_list.according_to_fst u3 ;;

let bigger_n = small_n+1 ;;
let bigger_right_base = sps bigger_n ;;
let bigger_shadow x = Image.image (diff x) bigger_right_base ;;
let u5 = Image.image (fun (_,l)->(l,
   Ordered.sort Total_ordering.standard (Image.image bigger_shadow l)
) ) u4;;
let check_u5 = List.filter (fun (l,y)->List.length(y)>1 ) u5;;

let u6 = Int_range.index_everything (Image.image snd u4) ;;
exception Doherty_exn of int list ;;
let doherty_index =Memoized.make(fun x ->
    try fst(List.find (fun (idx,y)->List.mem x y) u6)  with 
    _ -> raise (Doherty_exn(x))
)  ;;   
let waters_index x=
    let tempf = (fun j->doherty_index(syndectical_merger x [j])) in 
    (tempf 1,tempf 2) ;;
let u7 = Image.image (fun (_,l)->
   List.filter (fun z->Max.list(z)<=10) l
  ) u4;;
let u8 = Int_range.index_everything u7 ;;  

let u9 = Image.image (fun (idx,l)->(idx,
    Ordered.sort Total_ordering.standard (Image.image waters_index l)
 ) ) u8;;

let check_u9 = (u9 = [(1, [(2, 3)]); (2, [(4, 3)]); (3, [(5, 4)]); (4, [(1, 1)]); (5, [(6, 3)]);
(6, [(3, 1)])])
;; 
  
let representatives = Image.image (fun (idx,l)->(idx,List.hd l)) u8 ;;

end ;;  

let d_measure x = 
  let m = List.hd (List.rev x) in 
  let y = i_setminus x [m] in 
  (relative_measure x [m],relative_measure y [m-2],relative_measure y [m-4;m-1]) ;;

let diff x y = d_measure (syndectical_merger x y) ;; 

let left_base = sps 12 ;;

let small_n = 2 ;;
let right_base = sps small_n ;;
let shadow x = Image.image (diff x) right_base ;;
      
let u3 = Explicit.image  (fun z->(shadow z,z)) left_base ;;
let u4 = Partition_list.according_to_fst u3 ;;

let bigger_n = small_n+1 ;;
let bigger_right_base = sps bigger_n ;;
let bigger_shadow x = Image.image (diff x) bigger_right_base ;;
let u5 = Image.image (fun (_,l)->(l,
   Ordered.sort Total_ordering.standard (Image.image bigger_shadow l)
) ) u4;;
let check_u5 = List.filter (fun (l,y)->List.length(y)>1 ) u5;;


let u6 = Int_range.index_everything (Image.image snd u4) ;;
exception Doherty_exn of int list ;;
let doherty_index =Memoized.make(fun x ->
    try fst(List.find (fun (idx,y)->List.mem x y) u6)  with 
    _ -> raise (Doherty_exn(x))
)  ;;   
let waters_index x=
    let tempf = (fun j->doherty_index(syndectical_merger x [j])) in 
    (tempf 1,tempf 2) ;;
let u7 = Image.image (fun (_,l)->
   List.filter (fun z->Max.list(z)<=10) l
  ) u4;;
let u8 = Int_range.index_everything u7 ;;  

let u9 = Image.image (fun (idx,l)->(idx,
    Ordered.sort Total_ordering.standard (Image.image waters_index l)
 ) ) u8;;

let check_u9 = (u9 = [(1, [(2, 3)]); (2, [(4, 5)]); (3, [(6, 4)]); (4, [(1, 1)]); (5, [(6, 4)]);
(6, [(7, 5)]); (7, [(3, 8)]); (8, [(2, 3)])])
;; 


let representatives = Image.image (fun (idx,l)->(idx,List.hd l)) u8 ;;

let big_representatives = Image.image (fun (idx,l)->
  let temp1 = List.hd (List.rev l) in 
  let a = List.hd temp1 in 
  (idx,Image.image (fun t->t-a+1) temp1)
) u8 ;;

let dougherty_table = [(1, [2; 3]); (2, [4; 5]); (3, [6; 4]); (4, [1; 1]); (5, [6; 4]); (6, [7; 5]);
(7, [3; 8]); (8, [2; 3])] ;;

exception Bad_dougherty_jump of int * int ;; 

let dougherty_jump transition state = 
  if transition >2 then 1 else
  try List.nth (List.assoc state dougherty_table) (transition-1) with 
   _ -> raise (Bad_dougherty_jump(transition,state)) ;;
  
let rec dougherty_iterator (to_be_treated,state) =
   match to_be_treated with 
    [] -> state 
   | transition :: others -> 
    dougherty_iterator (others,dougherty_jump transition state) ;;

let dougherty_index z =
    let temp1 = Arithmetic_list.delta (0::z) in 
    dougherty_iterator (temp1,4) ;;

let rec dougherty_helper (to_be_treated,treated) =
  if List.length(to_be_treated)<3 then to_be_treated@treated else 
  let di = dougherty_index to_be_treated 
  and m = List.hd (List.rev to_be_treated) in 
  if List.mem di [1;2;6;8] 
  then  dougherty_helper ((i_setminus to_be_treated [m-2;m]),m::treated)
  else   
  if List.mem di [3;5] 
  then  dougherty_helper ((i_setminus to_be_treated [m-4;m-1;m]),m::treated)
  else  dougherty_helper ((i_setminus to_be_treated [m]),treated)  ;;
      

(************************************************************************************************************************
Snippet 76 : Compare two copies of the same directory
************************************************************************************************************************)
open Needed_values ;;

let select = List.filter(
  fun s-> (not(List.mem s ["";".gitignore"]))
    &&(not(String.ends_with ~suffix:".json" s ))
    &&(not(String.ends_with ~suffix: "/" s ))
    &&(not(String.starts_with ~prefix:"node_modules/" s ))
) ;;
 
let dir1 = home^"/Downloads/YC" ;;
let dir2 = home^"/Teuliou/Sites/Mongoose_example/Current_app" ;;

let read_both x = (x,(rf(dir1^"/"^x),rf(dir2^"/"^x) )) ;;

let v1 = 
  Ordered.sort Total_ordering.lex_for_strings
  (select(Unix_again.quick_beheaded_complete_ls dir1));;

let v2 = 
    Ordered.sort Total_ordering.lex_for_strings
    (select(Unix_again.quick_beheaded_complete_ls dir2));;  

let v12 =  Ordered.setminus Total_ordering.lex_for_strings v1 v2 ;;   
let v21 =  Ordered.setminus Total_ordering.lex_for_strings v2 v1 ;;  


let u1 = Image.image read_both v1 ;;
let u2 = List.filter (fun (fn,(x,y))-> x<>y) u1;;
let u3 = Image.image fst u2 ;;
(************************************************************************************************************************
Snippet 75 : Typical combination of the Check_polished_ocr and Incremental_replace_on_a_set_of_files modules
************************************************************************************************************************)
open Needed_values ;;

let building_site = home^"/Teuliou/html_files/Translations/Building_site/";;

let emptiable_ap = Absolute_path.of_string (building_site^"emptiable_cmist.txt") ;;
let polished_ap = Absolute_path.of_string (building_site^"polished_cmist.txt") ;;
let walker_ap = Absolute_path.of_string (building_site^"walker_cmist.txt") ;;

let ref_for_expected_action = ref None ;;

let put_first_page_on_walker ()=
  if (! ref_for_expected_action) = Some "officialize" 
  then failwith("You just pushed a page. You need to officialize it before putting another page") 
  else    
  let (first_page,new_text1) = Percent_pagination.extract_first_page_in_file emptiable_ap in 
  (
    Io.overwrite_with emptiable_ap new_text1 ;
    Io.overwrite_with walker_ap first_page ;
    ref_for_expected_action := Some "officialize" ;
  ) ;;
   
let officialize () = 
  if (! ref_for_expected_action) = Some "push page" 
  then failwith("You just officialized a page. No need to officialize it a second time") 
  else   
  let walker_text = Io.read_whole_file walker_ap in 
  let _ = Check_ocr.check_phpbb_footnotes_on_page walker_text in 
  let new_polished_text = (Io.read_whole_file polished_ap) ^ "\n\n" ^ walker_text  in 
  (
    Io.overwrite_with polished_ap new_polished_text;
    ref_for_expected_action := Some "push page" ;
  ) ;;

let compress_paragraph_in_walker_interval i j=
   Lines_in_text.findreplace_in_interval_in_file ("\n"," ") walker_ap  i j ;; 

let this_ap = Absolute_path.of_string 
   (home^"/Teuliou/OCaml/skeptical_duck/watched/watched_not_githubbed/pan.ml") ;;

Incremental_replace_on_a_set_of_files.set_replacements_datafile  this_ap ;;

let beginning_marker = "(" ^ "* Replacements begin here *)" ;; 
let end_marker = "(" ^ "* Replacements end here *)" ;; 
Incremental_replace_on_a_set_of_files.set_markers beginning_marker end_marker ;;
Incremental_replace_on_a_set_of_files.set_receiving_files [emptiable_ap;walker_ap] ;;

let check_pages_and_footnotes () = 
  Check_ocr.check_phpbb_footnotes_on_all_pages (Io.read_whole_file polished_ap) ;;

(* Replacements begin here *)


let replacements = [
   ("\012","");
   (" /n"," In");
   (" <e"," se");
   (" 1. "," l. ");
   (" 1s"," Is");
   (" cl "," el ");
   (" cn "," en ");
   (" cs "," es ");
   (" cse"," ese");
   (" cst"," est");
   (" ct "," et ");
   (" Ja "," la ");
   (" mo "," no ");
   (" sc "," se ");
   ("esc ","ese ");
   ("nucv","nuev");
   (" cdad"," edad");
   (" clla"," ella");
   (" cra "," era ");
   (" csta"," esta");
   (" Cf, "," Cf. ");
   (" e. "," c. ");
   (" dcbe"," debe");
   (" elc."," etc.");
   (" ficl"," fiel");
   (" imte"," inte");
   (" Jas "," las ");
   (" lgle"," Igle");
   (" pucb"," pueb");
   (" quc "," que ");
   (" sca "," sea ");
   (" veee"," vece");
   ("(1s. ","(Is. ");
   ("cnerg","energ");
   ("mcdio","medio");
   ("mcter","meter");
   ("tcolo","teolo");
   (" cfect"," efect");
   (" clla "," ella ");
   (" cllas"," ellas");
   (" cntre"," entre");
   (" cstas"," estas");
   (" C\195\173. "," Cf. ");
   (" idemt"," ident");
   (" incfa"," inefa");
   (" Mer. "," Mgr. ");
   (" posce"," posee");
   ("cterna","eterna");
   ("cucrpo","cuerpo");
   ("poscsi","posesi");
   ("posec ","posee ");
   ("vuclve","vuelve");
   (" alina "," alma ");
   (" clerna"," eterna");
   (" cllos "," ellos ");
   (" cxiste"," existe");
   (" desco "," deseo ");
   (" elerna"," eterna");
   (" eloria"," gloria");
   (" elorio"," glorio");
   (" eriatu"," criatu");
   (" mucve "," mueve ");
   (" sicte "," siete ");
   (" Samto "," Santo ");
   (" tinicb"," tinieb");
   ("/nstitu","Institu");
   ("inanera","manera");
   ("lelesia","Iglesia");
   ("picrden","pierden");
   ("quictud","quietud");
   ("S, TH.,","S. TH.,");
   ("S. Ti.,","S. TH.,");
   ("virlude","virtude");
   (" anmento"," aumento");
   (" comocer"," conocer");
   (" descos "," deseos ");
   (" eloria "," gloria ");
   (" elorifi"," glorifi");
   (" Ielesia"," Iglesia");
   (" vuesira"," vuestra");
   ("Acust\195\173n","Agust\195\173n");
   ("entre El","entre \195\137l");
   ("mencster","menester");
   ("nucstros","nuestros");
   ("S, Tit.,","S. TH.,");
   (" descar\194\187"," desear\194\187");
   (" eristian"," cristian");
   (" misinas "," mismas ");
   ("cuanto El","cuanto \195\137l");
   ("maturales","naturales");
   ("siendo El","siendo \195\137l");
   (" eriaturas"," criaturas");
   ("Jesueristo","Jesucristo");
   (" eristianos"," cristianos");
   (" estc "," este ");
   (" fu\195\169 "," fue ");
   (" To. "," Io. ");
   (" ul "," ut ");
   ("$","\\$");
   ("$","\194\167");
   ("(1 lo. ","(1 Io. ");
   ("(1o. ","(Io. ");
   ("(lo. ","(Io. ");
   ("(Lo. ","(Io. ");
   ("(To. ","(Io. ");
   ("/nstitu","Institu");
   ("1%","1\194\176");
   ("2%","2\194\176");
   ("3%","3\194\176");
   ("4%","4\194\176");
   ("a El","a \195\137l");
   ("como El","como \195\137l");
   ("con El","con \195\137l");
   ("C\194\163. ","Cf.");
   ("de El","de \195\137l");
   ("en El","en \195\137l");
   ("inficles","infieles");
   ("In loan.","In Ioan.");
   ("mosotros","nostros");
   ("o\\ve","owe");
   ("para El","para \195\137l");
   ("peeadores","pecadores");
   ("por El","por \195\137l");
   ("que El","que \195\137l");
   ("sin El","sin \195\137l");
   ("S. Ac.","S. AG.");
   ("S. Tit.,","S. TH.,");
   ("y El","y \195\137l");
   ("\194\176","\\textdegree");
   ("totalinente","totalmente");
];;


(* Replacements end here *)

Incremental_replace_on_a_set_of_files.initialize_replacements replacements ;; 

(*
On startup, you can make a few clean-up initializations as follows : 
*)

let act1 () = Chronometer.it (Percent_pagination.modify_file_pagewise
  (fun text->
    Remove_hyphens.in_text(Make_paragraphs_one_lined.in_text text)
    )) emptiable_ap ;;

let act2 () = Chronometer.it Incremental_replace_on_a_set_of_files.apply_all () ;;



let p = put_first_page_on_walker ;;

let o = officialize ;;

let c = compress_paragraph_in_walker_interval ;;

let r (a,b) = Incremental_replace_on_a_set_of_files.add_new_replacement (a,b) ;; 

let f =  check_pages_and_footnotes ;;


(************************************************************************************************************************
Snippet 74 : Code to OCR-size PDF's into .html  (see also Snippet 72 for .txt instead of html)
************************************************************************************************************************)
open Needed_values ;; 

let dirname = "Building_site/";;

let first_treated_page = 16 ;;
let num_of_pages = 3 ;;
let last_treated_page = (first_treated_page-1) + num_of_pages ;;


let bare_filename = "bot.pdf"
let write1 k =
   let sk = string_of_int k 
   and sj = string_of_int (k-first_treated_page+1)
   and sn = string_of_int num_of_pages in 
   "pdftoppm "^bare_filename^" p"^sk^" -png -f "^sk^" -singlefile\n"^
   "tesseract -l eng p"^sk^".png p"^sk^"\n"^
   "mv p"^sk^".txt /media/sf_Downloads/"^dirname^" \n"^
   "echo \""^sk^" : "^sj^" of "^sn^"\"";;


let ap_for_script1 = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/script.sh");;


let script1 = "\n\n\n"^(String.concat "\n" 
 (Int_range.scale write1 first_treated_page last_treated_page))^"\n\n\n" ;;   
   
Io.overwrite_with ap_for_script1 script1;;

let partial_texts_for_html = Int_range.scale (fun k->
   let sk = string_of_int k in 
   let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
  let uncompressed_pagetext = rf fn in 
  let pagetext = Make_paragraphs_one_lined.in_text 
  (Remove_hyphens.in_text uncompressed_pagetext) in  
  pagetext)  first_treated_page last_treated_page ;;
 
 
let full_html_ap = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"full.html");;  
 
let html_beginning = String.concat "\n"
 ["<!DOCTYPE html>"; "<html>"; "<head>";
 "\t<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\">";
 "\t<title> Title  </title>"; "</head>";
 "<body background=\"bg03.gif\">"; "<center>";
 "\t<h1> TITLE </h1>"; "</center>"] ;;

let html_ending = String.concat "\n"
 ["</body>"; "</html>"] ;;

 let html_full_text = 
   String.concat "\n"
   [html_beginning;Htmlize.pages partial_texts_for_html;html_ending] ;;
 
  Io.overwrite_with full_html_ap html_full_text;;


(************************************************************************************************************************
Snippet 73 : Code using the Parse_js module 
************************************************************************************************************************)
(*

let ap1 = Absolute_path.of_string 
 (home^"/Teuliou/Sites/Gwerzher_Leoriou/node_modules/async/lib/async.js") ;;
let text1 = Io.read_whole_file ap1 ;; 

let g1 = Parse_js.tokens text1 ;;
let res1 = Parse_js.parse_string text1 ;;
let res2 = Parse_js.parse_program text1 ;;
let res3 = Parse_js.program_of_string text1 ;;

*)


(************************************************************************************************************************
Snippet 72 : Code to OCR-size PDF's into .txt 
************************************************************************************************************************)
open Needed_values ;;

let lag = (0) ;;
let num_of_pages = 12 ;;
let dirname = "Building_site/";;
let first_treated_page = 1 ;;

let bare_filename = "brit.pdf"
let write1 k =
   let sk = string_of_int k 
   and sj = string_of_int (k+lag) in 
   "pdftoppm "^bare_filename^" p"^sk^" -png -f "^sj^" -singlefile\n"^
   "tesseract -l eng p"^sk^".png p"^sk^"\n"^
   "mv p"^sk^".txt /media/sf_Downloads/"^dirname^" \n"^
   "echo "^sk;;


let ap1 = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/script.sh");;

let last_treated_page = (first_treated_page-1) + num_of_pages ;;

let text1 = "\n\n\n"^(String.concat "\n" 
 (Int_range.scale write1 first_treated_page last_treated_page))^"\n\n\n" ;;   
   
Io.overwrite_with ap1 text1;;

let partial_texts_for_txt = Int_range.scale (fun k->
  let sk = string_of_int k in 
  let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
  let announcer = "%\n% Page "^sk^" \n%\n" in 
 let uncompressed_pagetext = rf fn in 
 let pagetext = Make_paragraphs_one_lined.in_text 
 (Remove_hyphens.in_text uncompressed_pagetext) in  
 announcer^pagetext)  first_treated_page last_treated_page ;;

 
 let full_ap = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"full.txt");;  
 
 let txt_full_text = String.concat "\n" partial_texts_for_txt ;;
 
 Io.overwrite_with full_ap txt_full_text;;
 


(************************************************************************************************************************
Snippet 71 : Musing on Egyptian fractions
************************************************************************************************************************)
let rec next_gcd_correct_index (l,walker) = 
  if List.for_all (fun t->(Gcd.gcd t walker) =1) l
  then walker
  else next_gcd_correct_index (l,walker+1) ;;  

let reduce_fraction old_a old_b =
  if old_a = 0 
  then (0,1) 
  else let g = Gcd.gcd old_a old_b in 
       (old_a/g,old_b/g) ;; 

let next_state (l,a,b,walker) = 
  (* it is assume a /b >= 1/walker *)
  let i1 = next_gcd_correct_index (l,walker) in 
  let (small_a,small_b)=reduce_fraction (a*i1-b) (b*i1) in 
  (i1::l,small_a,small_b,Basic.frac_ceiling small_b small_a) ;;

let rec finish_solving old_uple =
 let (l,a,b,walker) = old_uple in 
 if a = 1 
 then (List.rev l,b)
 else 
 finish_solving (next_state old_uple) ;;

let solve a b =   finish_solving([],a,b,Basic.frac_ceiling b a) ;;

let v0 = ([],3,7,Basic.frac_ceiling 7 3) ;;
let ff = Memoized.small next_state v0 ;;



(************************************************************************************************************************
Snippet 70 : Linear algebra on variables indexed by Z^2
************************************************************************************************************************)
let w1 n =Int_range.scale (fun y->(n,y-1)) 1 (n+1) ;;
let w2 n =Int_range.scale (fun x->(n-x,n)) 1 (2*n) ;;
let w3 n =Int_range.scale (fun y->(-n,n-y)) 1 (2*n) ;;
let w4 n =Int_range.scale (fun x->(-n+x,-n)) 1 (2*n) ;;
let w5 n =Int_range.scale (fun y->(n,-n+y)) 1 (n-1) ;;
let ww n =
    if n = 0 then [0,0] else 
    List.flatten (Image.image (fun w->w n) [w1;w2;w3;w4;w5]) ;;  

let small_size = 2 ;;    
let base1 = List.flatten (Int_range.scale ww 0 (2*small_size));;
let b_index pair = List_again.find_index_of_in pair base1  ;;

let for_two =((fun pair1 pair2 ->
  Total_ordering.for_integers 
  (b_index pair1) (b_index pair2) ): (int *int) Total_ordering_t.t);;

let for_ttwo = Total_ordering.product for_two for_two ;;

let for_four =((fun 
   (p1,p2)
   (q1,q2)
   -> 
    let mp = max (b_index p1) (b_index p2) 
    and mq = max (b_index q1) (b_index q2)  in
    let trial1 = Total_ordering.standard mp mq in 
    if trial1 <> Total_ordering_result_t.Equal then trial1 else 
      for_ttwo  (p1,p2) (q1,q2)
): ((int *int)*(int *int)) Total_ordering_t.t ) ;;

let m_index l = Max.list (Image.image b_index l) ;; 

let for_list =((fun 
   l1 l2
   -> 
    let trial1 = Total_ordering.standard (m_index l1) (m_index l2) in 
    if trial1 <> Total_ordering_result_t.Equal then trial1 else 
      Total_ordering.standard l1 l2
): ((int *int) list) Total_ordering_t.t ) ;;


let unchecked_base2= Cartesian.square(Cartesian.square(Int_range.range (-small_size) small_size)) ;;
let check_pair (c,d) = if c=0 then d>0 else c>0 ;;
let check_double_pair ((a,b),(c,d)) =
  ((a,b)<>(0,0)) && (check_pair(c,d)) ;;
let base2 = List.filter check_double_pair unchecked_base2 ;;
let base3 = Image.image (fun ((a,b),(c,d))->
  [(a+c,b+d);(a-c,b-d);(a,b);(c,d)]
  ) base2 ;;
let base4 = Ordered.sort for_list base3 ;;
let standardize_pair (x,y)= if x<0 then (-x,-y) else (x,y) ;;
let mima (a,b) = if (for_two a b)=Total_ordering_result_t.Lower then (b,a) else (a,b) ;;
let standardize_list l=
   let temp1 = Image.image standardize_pair l in 
   let tempf = (fun k->List.nth temp1 (k-1)) in 
   (mima (tempf 1,tempf 2),mima (tempf 3,tempf 4)) ;;
let base5 = Image.image standardize_list base4 ;;
let base6 = List_again.nonredundant_version base5 ;;

let ff k = List.nth base6 (k-1) ;;


(************************************************************************************************************************
Snippet 69 : Short code related to similar matrices exercise
************************************************************************************************************************)
let test1 n = List.exists (fun a->((a*(4-a)+1) mod n)= 0) (Int_range.range 0 (n-1)) ;;
    
let v1 = List.filter test1 (Int_range.range 3 50) ;;

let see1 n = List.filter (fun a->((a*(4-a)+1) mod n)= 0) (Int_range.range 0 (n-1)) ;;

let u1 = Cartesian.square (Int_range.range (-50) 50) ;;
let sign_condition (x,y) =
    if x<0 then false else 
    if x=0 then y>0 else true ;;
let u2 = List.filter (fun (x,y)->(sign_condition (x,y))&&(Gcd.gcd x y=1)) u1 ;;
let u3 = Image.image (fun (x,y)->(max (abs x) (abs y),(x,y)) ) u2 ;;
let (Set_of_poly_pairs_t.S u4) = Set_of_poly_pairs.sort u3 ;;
let u5 = Image.image snd u4 ;;

let tf1 (a,b) = List.find (fun (z2,z4)->
  abs((a*a-4*a-1)*(z2*z2)+2*(a-2)*b*z2*z4+(b*b)*(z4*z4))<b*b 
  ) u5 ;;

let current_b = 29 ;;  
let abs_b = abs current_b ;;
let good_moduli = see1 abs_b ;;
let ff a = tf1(a,current_b) ;;

(************************************************************************************************************************
Snippet 68 : Exercise related to Cantor set 
************************************************************************************************************************)
let read_fraction s = 
  let (a,b) = Cull_string.split_wrt_rightmost s '/' in 
  (int_of_string a,int_of_string b) ;;

let interval_size_is_smaller_than frac1 frac2 (p,q) =
  let (a1,b1) = read_fraction frac1 
  and (a2,b2) = read_fraction frac2 in 
  q * (a2*b1-a1*b2) < p * (b1*b2)  ;;

let interval_size_is_larger_than frac1 frac2 (p,q) =
   let (a1,b1) = read_fraction frac1 
   and (a2,b2) = read_fraction frac2 in 
   q * (a2*b1-a1*b2) > p * (b1*b2)  ;;

let minmax_using_tor total_ordering x y =
if (List_again.find_index_of_in x total_ordering) < (List_again.find_index_of_in y total_ordering) 
then (x,y)
else (y,x) ;;  

let possibly_empty_interval_using_tor total_ordering a b =
  if minmax_using_tor total_ordering a b = (a,b)
  then Some(a,b)
  else None ;;  

let interval_intersection_using_tor total_ordering (a1,b1) (a2,b2) =
  let (_,a3) = minmax_using_tor total_ordering  a1 a2 
  and (b3,_) = minmax_using_tor total_ordering  b1 b2 in 
  possibly_empty_interval_using_tor total_ordering  a3 b3;; 


let current_k = 3 ;; 

let outer_interval_is_too_small  total_ordering =
 let n = List.length total_ordering in 
 let ia = List_again.find_index_of_in "a" total_ordering 
 and ib = List_again.find_index_of_in "b" total_ordering in 
 if List.mem (ia,ib) [1,2;n-1,n] then true else
 if (ia<2)||(ib>=n) then false else 
 let just_below_a = List.nth total_ordering (ia-2) 
 and just_above_b = List.nth total_ordering ib in 
 interval_size_is_smaller_than just_below_a just_above_b (4,Basic.power 3 current_k) ;; 

let inner_interval_is_too_large  total_ordering =
 let ia = List_again.find_index_of_in "a" total_ordering 
 and ib = List_again.find_index_of_in "b" total_ordering in 
 if ib=ia+1 then false else 
 let just_above_a = List.nth total_ordering ia 
 and just_below_b = List.nth total_ordering (ib-2) in 
 interval_size_is_larger_than just_above_a just_below_b (8,Basic.power 3 current_k) ;; 


let intervals_outside = 
[
  "1/9","2/9";"1/3","2/3";"7/9","8/9"
] ;;

let base1 = List.flatten(Image.image (fun (x,y)->[x;y]) intervals_outside);;

let insert_two_elements_at_indices l (elt1,elt2) (idx1,idx2) = 
  let (part1,temp1) = List_again.long_head_with_tail (idx1-1) l in 
  let (part2,part3) = List_again.long_head_with_tail (idx2-idx1) temp1 in 
  List.rev_append part1  (elt1 :: (List.rev_append part2  (elt2 :: part3))) ;;  
  
(* insert_two_elements_at_indices [1; 2; 3; 4; 5; 6] (25,35) (3,4) ;;  *)


let extend_total_ordering_by_adding_two_elements old_total_order elt1 elt2 = 
  let n = (List.length old_total_order)+1 in 
  Image.image (
   insert_two_elements_at_indices old_total_order (elt1,elt2)
  ) (Int_uple.inclusive_list_of_pairs n) ;; 


(* extend_total_ordering_by_adding_two_elements  [1; 2; 3; 4; 5; 6] 25 35 ;; *)

let base2 = extend_total_ordering_by_adding_two_elements 
  base1 "a" "b" ;;

let (bad1,good1) = List.partition inner_interval_is_too_large base2 ;; 
let (bad2,good2) = List.partition outer_interval_is_too_small good1 ;; 

let u1 = Image.image (
 fun total_ordering ->
   (Image.image (interval_intersection_using_tor total_ordering ("a","b")) 
   intervals_outside,total_ordering)
) good2 ;;

let u2 = Partition_list.according_to_map u1 fst ;; 
let tf k = List.nth u2 (k-1) ;; 

(************************************************************************************************************************
Snippet 67 : Enumeration of multi-degrees related to symmetric polynomials
************************************************************************************************************************)
let order_for_triples = ((
  fun (x1,x2,x3) (y1,y2,y3) ->
    let sx = x1+x2+x3
    and sy =y1+y2+y3 in 
    let trial1 = Total_ordering.standard sx sy in 
    if trial1 <> Total_ordering_result_t.Equal then trial1 else   
    let mx = Max.list [x1;x2;x3] 
      and my = Max.list [y1;y2;y3] in 
      let trial2 = Total_ordering.standard mx my in 
      if trial2 <> Total_ordering_result_t.Equal then trial2 else 
      Total_ordering.silex_for_intlists [x1;x2;x3] [y1;y2;y3] 
) :> (int*int*int) Total_ordering_t.t );;

let orbit (x1,x2,x3) = Ordered.sort order_for_triples 
   [ (x1,x2,x3);(x1,x3,x2);(x2,x1,x3);(x2,x3,x1);(x3,x1,x2);(x3,x2,x1); ] ;;

let u1 = Ordered.sort order_for_triples  (Cartesian.cube (Int_range.range 0 8)) ;;   

let u2 = Explicit.image (fun tr->(tr,orbit tr)) u1 ;;
let u3 = List.filter (fun (tr,l)->tr=List.hd l) u2 ;; 
let tf k = List.nth u3 (k-1) ;;


(************************************************************************************************************************
Snippet 66 : Musing on Steinhaus triangles
************************************************************************************************************************)
let i_fold_merge = Ordered.fold_merge Total_ordering.for_integers ;;
let i_sort = Ordered.sort Total_ordering.for_integers ;;
let il_sort = Ordered.sort Total_ordering.silex_for_intlists ;;

let index_from_x unadbridged_x_form =
    let x_form = Cull_string.trim_spaces unadbridged_x_form in 
    if not(String.starts_with ~prefix:"x" x_form ) 
    then None 
    else
    Some(int_of_string(Cull_string.cobeginning 1 x_form));;     

let indices_from_xlist xlist =
  let parts = Str.split (Str.regexp_string "+") xlist in  
  i_sort(List.filter_map index_from_x parts);;

let temporary_store=Absolute_path.of_string (Needed_values.home^"/Downloads/temp.txt") ;;
let transmitter_file = Absolute_path.of_string "Fads/pan.ml";;

let act () = 
  let stored_text = Io.read_whole_file temporary_store in 
  let temp1 = Replace_inside.replace_several_inside_text 
   ["[","";"]","";"(","";")",""] stored_text in
  let xsums = Str.split (Str.regexp_string ",") temp1 in 
  let dim_after = List.length xsums in  
  let indices = i_fold_merge (Image.image indices_from_xlist xsums) in 
  let ocamlese_before = String.concat "," (Image.image ( fun i->
   "x"^(string_of_int i)
  ) indices) in
  let ocamlese_after = String.concat ";" xsums in 
  let ocamlese_uple = String.concat "," (Int_range.scale ( fun i->
  "tf "^(string_of_int i)
  ) 1 (List.length indices))  in 
  let lines_in_preproduced_text =
  [ 
   "let dim_before = "^(string_of_int (List.length indices))^" ;;";
   "let dim_after = "^(string_of_int dim_after)^" ;;";
   "let to_long_list ("^ocamlese_before^") ="; "    Image.image (fun t-> t mod 2)";
   "    [";
   "      "^ocamlese_after;
   "    ] ;;"; "let to_uple l ="; "    let tf = (fun k->List.nth l (k-1)) in ";
   "    ("^ocamlese_uple^") ;;  "; ] in 
  let preproduced_text = "\n\n\n"^(String.concat "\n" lines_in_preproduced_text)^"\n\n\n" in 
  Replace_inside.overwrite_between_markers_inside_file 
      ~overwriter:preproduced_text
      ("(* Pre-"^"processed part starts here *)","(* Pre-"^"processed part ends here *)")
      transmitter_file ;;


(* Pre-processed part starts here *)


let dim_before = 4 ;;
let dim_after = 36 ;;
let to_long_list (x2,x4,x5,x6) =
    Image.image (fun t-> t mod 2)
    [
      0; x2; x2; 1; x2 + 1; 1; x4; x4 + 1; x2 + x4; x2 + x4 + 1; x5; x4 + x5; x5 + 1; x2 + x4 + x5 + 1; x5; x6; x5 + x6; x4 + x6; x4 + x5 + x6 + 1; x2 + x6; x2 + x5 + x6; x2 + x4 + x5 + x6 + 1; x2 + x4 + x5 + 1; x2 + x4 + x6 + 1; x2 + 1; x2 + x4 + x5 + x6; x4 + x5; x2 + x4 + x6; x5 + 1; x2 + x4 + x6; x5 + x6 + 1; x2 + x4 + x5; x4 + x5 + 1; x2 + x6 + 1; x2 + x4 + x5 + x6 + 1; x5 + 1
    ] ;;
let to_uple l =
    let tf = (fun k->List.nth l (k-1)) in 
    (tf 1,tf 2,tf 3,tf 4) ;;  


(* Pre-processed part ends here *)


let test uple =
  let l = to_long_list uple in
  let n = (List.length l)/2 in 
    let l2 = List.filter (fun y->y = 0) l in 
    (List.length l2)=n ;;
  


let test2 l = test (to_uple l) ;;  
  
let base1 = Int_range.scale (fun _->[0;1]) 1 dim_before ;;    
let base2 = Cartesian.general_product base1 ;;
let base3 = List.filter test2 base2 ;; 
let base4 = Image.image (fun u->(u,to_long_list(to_uple u))) base3 ;;

let nonzero_linear_forms = List.tl(Cartesian.product [0;1] base2) ;;
let eval_linear_form (const_part,linear_part) x=
  let temp1 = List.combine (const_part::linear_part) (1::x) in 
  let temp2 = Image.image (fun (a,b)->a*b) temp1 in 
  abs((Basic.fold_sum temp2) mod 2) ;;
let kernel_of_linear_form = Memoized.make(fun lf ->
    List.filter (fun x->eval_linear_form lf x = 0) base3 ) ;;   
let kernel_size = Memoized.make(fun lf->
    List.length(kernel_of_linear_form lf)
  ) ;;
let computation1 = Explicit.image kernel_size nonzero_linear_forms ;;

let res1 = Max.maximize_it_with_care kernel_size nonzero_linear_forms ;;

let defect_at_index =Memoized.make(fun idx -> 
    let (a,b) = List.partition (fun l->List.nth l (idx-1)=0) base3 in 
    (abs(List.length(b)-List.length(a))) );; 

let minimal_defects =
    Min.minimize_it_with_care  defect_at_index 
      (Int_range.range 1 dim_before) ;;   

let big_proj shadow = il_sort(Image.image (fun l->List_again.sublist_with_indices l shadow) base3) ;;  
let shadows = il_sort (List_again.power_set (Int_range.range 1 dim_before)) ;;     
let (_,shadowers) = Max.maximize_it_with_care (fun sh->List.length(big_proj sh)) shadows ;;




(************************************************************************************************************************
Snippet 65 : Draft to preprocess a file using data from PARI-GP
************************************************************************************************************************)
open Needed_values ;;

let i_fold_merge = Ordered.fold_merge Total_ordering.for_integers ;;
let i_sort = Ordered.sort Total_ordering.for_integers ;;

let index_from_x unadbridged_x_form =
    let x_form = Cull_string.trim_spaces unadbridged_x_form in 
    if not(String.starts_with ~prefix:"x" x_form) 
    then None 
    else
    Some(int_of_string(Cull_string.cobeginning 1 x_form));;     

let indices_from_xlist xlist =
  let parts = Str.split (Str.regexp_string "+") xlist in  
  i_sort(List.filter_map index_from_x parts);;

let temporary_store=Absolute_path.of_string (home^"/Downloads/temp.txt") ;;
let transmitter_file = Absolute_path.of_string "Fads/pan.ml";;


let stored_text = Io.read_whole_file temporary_store ;;
let temp1 = Replace_inside.replace_several_inside_text 
  ["[","";"]","";"(","";")",""] stored_text ;;
let xsums = Str.split (Str.regexp_string ",") temp1 ;;
let dim_after = List.length xsums ;;
let indices = i_fold_merge (Image.image indices_from_xlist xsums) ;;
let dim_before = List.length indices ;;
let ocamlese_before = String.concat "," (Image.image ( fun i->
   "x"^(string_of_int i)
) indices) ;;
let ocamlese_after = String.concat ";" xsums ;;
let ocamlese_uple = String.concat "," (Image.image ( fun i->
  "tf "^(string_of_int i)
) indices) ;;


let lines_in_preproduced_text =
[ "let max_idx = "^(string_of_int dim_after)^" ;;";
   "let to_long_list ("^ocamlese_before^") ="; "    Image.image (fun t-> t mod 2)";
   "    [";
   "      "^ocamlese_after;
   "    ] ;;"; "let to_uple l ="; "    let tf = (fun k->List.nth l (k-1)) in ";
   "    ("^ocamlese_uple^") ;;  "; ] ;;

let preproduced_text = "\n\n\n"^(String.concat "\n" lines_in_preproduced_text)^"\n\n\n" ;;

let prprpr () = 
    Replace_inside.overwrite_between_markers_inside_file 
      ~overwriter:preproduced_text
      ("(* Pre-"^"processed part starts here *)","(* Pre-"^"processed part ends here *)")
      transmitter_file ;;


(************************************************************************************************************************
Snippet 64 : Lower bounds on linear recurrent sequences of order 2
************************************************************************************************************************)
let nachste (x,y) = (y,4*y-5*x) ;;

let nachstee (x,y,l) = (y,4*y-5*x,x::l) ;; 

let precision = ref 1000 ;;

let measure (x,y) =
    let opt_example = ref None 
    and walker = ref (x,y) 
    and bound=abs(x) in 
    for k = 1 to (!precision) 
    do 
       walker := (nachste(!walker));
       if abs(fst(!walker)) <= bound 
       then opt_example := Some k 
    done;
    match (!opt_example) with 
    None -> 2
   |Some kmax -> (kmax)+1 ;;  

let see_measure (x,y) = 
    let n = measure(x,y) 
    and walker = ref (x,y,[]) in
    for k = 3 to n 
    do 
        walker := nachstee (!walker)
    done;
    let (a,b,l)=(!walker) in 
    List.rev(b::a::l) ;;
    
let base1 =Memoized.make(fun n -> List.filter (fun (x,y)->
    let mx = abs x and my =abs y in 
    ((x,y)<>(0,-1)) &&
    (x>=0)&&((Gcd.gcd x y)=1) && (mx<=my) && (max mx my=n)
  ) (Cartesian.square (Int_range.range (-n) n))) ;;

let base_image1 = Memoized.make (fun n->
    Max.maximize_it_with_care measure 
    (base1 n)
) ;;

let rec breaker_tester bound candidate =
  if fst(base_image1 candidate) >= bound 
  then  candidate 
  else  breaker_tester bound (candidate+1) ;;

let next_breaker = Memoized.recursive (fun old_f bound -> 
     if bound<=3 then (1,(3,[1; 1; -1],[])) else 
     let m = breaker_tester bound (fst(old_f(bound-1))) in 
     let (_,l) = base_image1 m in 
     let (a,others) = List_again.head_with_tail l in 
     let sol = see_measure a in 
     (m,(List.length sol,sol,others))
  ) ;;


let bi = base_image1 ;;
let sm = see_measure ;;
let nb = next_breaker ;;

(************************************************************************************************************************
Snippet 63 : Duplicating a paragraph in a file 
************************************************************************************************************************)
let ap1 = Absolute_path.of_string  
   "Compilation_management/commands_for_batch_compilation.ml" ;;
let text1 = Io.read_whole_file ap1 ;;

let (a,b,c) = Lines_in_text.tripartition_associated_to_interval text1 75 97 ;;

let text2 = String.concat "\n\n" [a;b;b;c] ;;

Io.overwrite_with ap1 text2 ;;


(************************************************************************************************************************
Snippet 62 : Write mathjax text for answer on chain additions
************************************************************************************************************************)
open Needed_values ;;

let i_order = Total_ordering.for_integers ;;
let il_order = Total_ordering.silex_compare i_order ;;

let i_is_included_in = Ordered.is_included_in i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_sort = Ordered.sort i_order ;;

let il_fold_merge = Ordered.fold_merge il_order ;;
let il_sort = Ordered.sort il_order ;;

let decompositions n = Int_range.scale (fun j->(j,n-j)) 1 (n/2) ;;

let try_easier_path old_f l = 
    if List.length (l) < 2 then None else 
    let (a,others) = List_again.head_with_tail l in   
       let candidate = List.filter (List.mem a) (old_f others ) in 
       if candidate = [] then None else Some candidate ;;

let abauzit_order = 
    let tempf=(
          fun l1 l2->
           let t=Total_ordering.standard(List.length l1)(List.length l2) in
           if t<>Total_ordering_result_t.Equal then t else
           il_order (List.rev l1) (List.rev l2)
    ) in
     (tempf:>( (int list) Total_ordering_t.t));;
      
let abauzit_expansion l = 
    let (n,temp1) = List_again.head_with_tail (List.rev l) in 
    let temp2 = List.rev temp1 
    and decs = decompositions n in 
    let temp3 = Image.image (fun (a,b)->i_merge (i_sort [a;b]) temp2) decs in   
    Ordered.sort abauzit_order temp3 ;;

let main = Memoized.recursive (fun old_f l->
   if l=[1] then [[1]] else
   match try_easier_path old_f l with 
    Some(easier_answer) -> easier_answer 
   | None ->    
     let n = List.hd(List.rev l) in 
     let temp1 = abauzit_expansion l in 
     let temp2 = Image.image old_f temp1 in 
     let (m,temp3) = Min.minimize_it_with_care (fun res->List.length(List.hd res)) temp2 in
     let temp4 = il_fold_merge temp3 in 
     Image.image (fun sol->sol@[n]) temp4
) ;;

let measure = Memoized.make(fun l->
    let res = main l in List.length(List.hd res)
) ;;

let adrien_analysis (l,bound) = 
    List.find_opt (fun (h,l2)->
      (measure [h]) + (List.length l2) >= bound  
    )  (Three_parts.beheaded_tails l) ;;
    
let singleton_analysis n =
  let temp1 = decompositions n in 
  let temp2 = Image.image (fun (a,b)->(a,b,measure [a],measure [b])) temp1 in 
  let (almost_m,temp3) = Min.minimize_it_with_care (fun (a,b,ma,mb)->max ma mb) temp2 in 
  (measure [n])=(almost_m+1)  ;;    

let singleton_test l = 
    let n = List.hd(List.rev l) in 
    if l <> [n]
    then false 
    else  singleton_analysis n ;;    

let hashtbl_for_impatient_main = Hashtbl.create 100 ;;     
    
let partial_analysis_without_writing l =
    let temp1 = abauzit_expansion l 
     and bound = (measure l)-1 in 
    let temp2 = Image.image (fun l2-> (l2,adrien_analysis (l2,bound)) ) temp1 in 
    let (good_temp2,bad_temp2) = List.partition (fun (l2,opt)->opt<>None) temp2 in 
    let temp3 = Image.image (fun (l2,opt)->(l2,Option.get opt)) good_temp2 in 
    let temp4 = List.filter_map (
      fun (l2,_) -> 
        if Hashtbl.find_opt hashtbl_for_impatient_main l2 = None 
        then Some l2 
        else None 
    ) bad_temp2 in 
    (singleton_test l,temp3,temp4,List.hd(main l)) ;; 
    
let soi = string_of_int ;; 
let soil l = "["^(String.concat ";" (Image.image soi l))^"]";;

let pointed_card_element_to_string (x,(head,passive_part)) =
   "("^(soil x)^",("^(soi head)^","^(soil passive_part)^"))";;

let pointed_card_to_string l = 
   "["^(String.concat ";" (Image.image pointed_card_element_to_string l))^"]";;

let write_about_singleton n solution =
   "simplest_case "^(soi n)^" "^(soil solution)^" ;;";;

let write_about_missing_link missing_link =
  "pa "^(soil missing_link)^" ;;";;

  let write_about_missing_links missing_links =
    String.concat "\n" (Image.image write_about_missing_link missing_links);;

let write_about_pointed_card l pointed_card solution  = 
  "pointed_card "^(soil l)^" "^
  (pointed_card_to_string pointed_card)^" "^(soil solution)^" ;;";;
  ;;

let write_about_partial_analysis l (is_singleton,pointed_card,missing_links,solution) =
  let n = List.hd(List.rev solution) in 
   if is_singleton 
   then write_about_singleton n solution 
   else
   if missing_links <> []
   then write_about_missing_links missing_links         
   else write_about_pointed_card l pointed_card solution ;;

let partial_analysis l =
    let temp = partial_analysis_without_writing l in 
    let text = "\n\n\n"^(write_about_partial_analysis l temp)^"\n\n\n" in 
    let _ = (print_string text;flush stdout) in 
    temp ;;

let check_admissibility x=
  let rec tempf =( 
     fun to_be_treated -> match to_be_treated with 
     [] -> true
     | a:: others -> 
        if others = []
        then true 
        else if List.exists (fun (x,y)->x+y=a) (Uple.inclusive_list_of_pairs others) 
        then tempf(others)
        else false     
  ) in 
  tempf(List.rev x) ;;
  
exception Nonadmissible_set of int list ;;

let check_admissiblity_agressively sol =
    if check_admissibility sol then () else raise(Nonadmissible_set(sol));;

exception Non_inclusion of (int list) * (int list) ;;

let initial_check (x,sol) =
   let _ = check_admissiblity_agressively sol in 
   if not(i_is_included_in x sol)
   then raise(Non_inclusion(x,sol)) 
   else () ;;    



exception Impatient_main_exn of int list ;;

let impatient_main l = match Hashtbl.find_opt hashtbl_for_impatient_main l with 
  Some (answer) -> answer
  |None -> raise(Impatient_main_exn(l));;

let impatient_measure l = List.length (impatient_main l) ;;

Hashtbl.add hashtbl_for_impatient_main [1] [1] ;;

exception Simplest_case_exn of (int list) * int ;; 

let simplest_case n sol = 
    let _ = initial_check ([n],sol) in 
    let temp1 = abauzit_expansion [n] in 
    let tempm = (fun l->Max.list (Image.image (fun t->impatient_measure [t]) l)) in 
    let m = Min.list (Image.image tempm temp1) in     
    if List.length(sol)<>m+1 
    then raise(Simplest_case_exn(sol,m+1))
    else  Hashtbl.add  hashtbl_for_impatient_main [n] sol ;;

exception Bad_pointed_card_element of (int list) * int * (int list) ;; 

let factor (x,y)=
    let rec factor0=(fun
       (graet,da_ober1,da_ober2)->
       if (da_ober1=[])||(da_ober2=[])
       then (List.rev graet,da_ober1,da_ober2)
       else let (a1,peurrest1)=List_again.head_with_tail da_ober1
            and (a2,peurrest2)=List_again.head_with_tail da_ober2 in
            if a1=a2
            then factor0(a1::graet,peurrest1,peurrest2)
            else (List.rev graet,da_ober1,da_ober2)
    ) in
    factor0([],x,y);;

let extends l1 l2=
   let (_,_,r2)=factor (l1,l2) in r2=[];;

let check_pointed_card_element (l2,(head,passive_part))=
   if not(extends (List.rev l2) (List.rev(head::passive_part))) 
   then raise(Bad_pointed_card_element(l2,head,passive_part))
   else () ;;
   
let check_pointed_card = List.iter  check_pointed_card_element ;;

exception Pointed_card_exn of ((int list) * int) list ;;

let pointed_card l pointed_card sol =
  let _ = initial_check (l,sol) 
  and _ = check_pointed_card pointed_card in 
  let temp1 = abauzit_expansion l 
  and bound = List.length(sol)-1 in 
  let temp2 = Image.image (
    fun l2 -> match List.assoc_opt l2 pointed_card with 
    Some(head,passive_part) ->(l2,impatient_measure([head])+List.length passive_part)
    |None -> (l2,impatient_measure l2)
  ) temp1 in 
  let bad_ones = List.filter (fun (l2,m_l2)->m_l2<bound) temp2 in 
  if bad_ones <> [] 
  then raise(Pointed_card_exn(bad_ones))
  else Hashtbl.add  hashtbl_for_impatient_main l sol ;;    

let pa = partial_analysis ;;
let pb n = pa [n] ;;

simplest_case 2 [1;2] ;;
simplest_case 3 [1;2;3] ;;
simplest_case 4 [1;2;4] ;;
simplest_case 5 [1;2;3;5] ;;
simplest_case 6 [1;2;3;6] ;;
pointed_card [7] [([3; 4], (3, [4])); ([2; 5], (5, [])); ([1; 6], (6, []))]  [1; 2; 3; 4; 7] ;;
pointed_card [3;8] [([3; 4], (3, [4])); ([3; 5], (3, [5])); ([2; 3; 6], (2, [3; 6]));
([1; 3; 7], (3, [7]))] [1; 2; 3; 4; 8];;
simplest_case 8 [1;2;4;8] ;;
simplest_case 9 [1; 2; 3; 6; 9] ;;
simplest_case 10 [1; 2; 3; 5; 10] ;;
pointed_card [11]  [([5; 6], (5, [6])); ([4; 7], (7, [])); ([2; 9], (9, []));
([1; 10], (10, []))] [1; 2; 3; 4; 7; 11] ;;
pointed_card [5;12] [([5;6],(5,[6]));([5;7],(5,[7]));([4;5;8],(4,[5;8]));([3;5;9],(3,[5;9]));([2;5;10],(5,[10]));([1;5;11],(5,[11]))] [1;2;3;5;6;12] ;;
simplest_case 12 [1; 2; 3; 6; 12] ;;
pointed_card [13] [([6; 7], (6, [7])); ([5; 8], (5, [8])); ([4; 9], (9, []));
([3; 10], (10, [])); ([2; 11], (11, [])); ([1; 12], (12, []))] [1; 2; 3; 5; 8; 13] ;;
pointed_card [14] [([7], (7, [])); ([6; 8], (6, [8])); ([5; 9], (5, [9])); ([4; 10], (10, []));
([3; 11], (11, [])); ([2; 12], (12, [])); ([1; 13], (13, []))]  [1; 2; 3; 4; 7; 14] ;;
simplest_case 15 [1; 2; 3; 5; 10; 15] ;;
pointed_card [3;16] [([3;13],(13,[]));([3;7;9],(3,[7;9]));([3;6;10],(3,[6;10]));([3;5;11],(3,[5;11]));([3;4;12],(3,[4;12]));([2;3;14],(14,[]));([1;3;15],(15,[]))] [1;2;3;4;8;16] ;;
pointed_card [5;16] [([5;8],(5,[8]));([5;11],(5,[11]));([5;7;9],(5,[7;9]));([5;6;10],(5,[6;10]));([4;5;12],(4,[5;12]));([3;5;13],(3,[5;13]));([2;5;14],(5,[14]));([1;5;15],(5,[15]))] [1;2;3;5;8;16] ;;
pointed_card [6;16] [([6;8],(6,[8]));([6;10],(6,[10]));([6;7;9],(6,[7;9]));([5;6;11],(5,[6;11]));([4;6;12],(4,[6;12]));([3;6;13],(3,[6;13]));([2;6;14],(6,[14]));([1;6;15],(6,[15]))] [1;2;3;6;8;16] ;;
pointed_card [7;16] [([7;8],(7,[8]));([7;9],(7,[9]));([6;7;10],(6,[7;10]));([5;7;11],(5,[7;11]));([4;7;12],(7,[12]));([3;7;13],(7,[13]));([2;7;14],(7,[14]));([1;7;15],(7,[15]))] [1;2;3;4;7;8;16] ;;
simplest_case 16 [1; 2; 4; 8; 16] ;;
pointed_card [12;17] [([8;9;12],(8,[9;12]));([7;10;12],(7,[10;12]));([6;11;12],(6,[11;12]));([4;12;13],(12,[13]));([3;12;14],(12,[14]));([2;12;15],(12,[15]));([1;12;16],(12,[16]))] [1;2;3;5;6;12;17] ;;
simplest_case 17 [1; 2; 4; 8; 9; 17] ;;
simplest_case 18 [1; 2; 3; 6; 9; 18] ;;
pointed_card [19] [([9;10],(9,[10]));([8;11],(11,[]));([7;12],(7,[12]));([6;13],(13,[]));([5;14],(14,[]));([4;15],(15,[]));([2;17],(17,[]));([1;18],(18,[]))] [1;2;3;4;8;11;19] ;;
pointed_card [7;20] [([7;10],(7,[10]));([7;13],(7,[13]));([7;9;11],(7,[9;11]));([7;8;12],(7,[8;12]));([6;7;14],(6,[7;14]));([5;7;15],(5,[7;15]));([4;7;16],(7,[16]));([3;7;17],(7,[17]));([2;7;18],(7,[18]));([1;7;19],(7,[19]))] [1;2;3;4;7;10;20] ;;
pointed_card [9;20] [([9;10],(9,[10]));([9;11],(9,[11]));([8;9;12],(8,[9;12]));([7;9;13],(7,[9;13]));([6;9;14],(6,[9;14]));([5;9;15],(5,[9;15]));([4;9;16],(9,[16]));([3;9;17],(9,[17]));([2;9;18],(9,[18]));([1;9;19],(9,[19]))] [1;2;3;6;9;10;20] ;;
simplest_case 20 [1;2;3;5;10;20] ;;
pointed_card [21] [([10;11],(10,[11]));([9;12],(9,[12]));([8;13],(13,[]));([7;14],(7,[14]));([6;15],(15,[]));([4;17],(17,[]));([3;18],(18,[]));([2;19],(19,[]));([1;20],(20,[]))] [1;2;3;4;7;14;21] ;;
pointed_card [22] [([11],(11,[]));([10;12],(10,[12]));([9;13],(9,[13]));([8;14],(14,[]));([7;15],(7,[15]));([5;17],(17,[]));([4;18],(18,[]));([3;19],(19,[]));([2;20],(20,[]));([1;21],(21,[]))] [1;2;3;4;7;11;22] ;;
pointed_card [23] [([11;12],(11,[12]));([10;13],(10,[13]));([9;14],(9,[14]));([8;15],(15,[]));([7;16],(7,[16]));([6;17],(17,[]));([5;18],(18,[]));([4;19],(19,[]));([3;20],(20,[]));([2;21],(21,[]));([1;22],(22,[]))] [1;2;3;5;10;13;23] ;;
pointed_card [5;24] [([5;19],(19,[]));([5;11;13],(5,[11;13]));([5;10;14],(5,[10;14]));([5;9;15],(5,[9;15]));([5;8;16],(5,[8;16]));([5;7;17],(5,[7;17]));([5;6;18],(5,[6;18]));([4;5;20],(20,[]));([3;5;21],(21,[]));([2;5;22],(22,[]));([1;5;23],(23,[]))] [1;2;3;5;6;12;24] ;;
pointed_card [7;24] [([7;12],(7,[12]));([7;17],(7,[17]));([7;11;13],(7,[11;13]));([7;10;14],(7,[10;14]));([7;9;15],(7,[9;15]));([7;8;16],(7,[8;16]));([6;7;18],(6,[7;18]));([5;7;19],(5,[7;19]));([4;7;20],(7,[20]));([3;7;21],(7,[21]));([2;7;22],(7,[22]));([1;7;23],(7,[23]))] [1;2;3;5;7;12;24] ;;
simplest_case 24 [1;2;3;6;12;24] ;;
pointed_card [25] [([12;13],(12,[13]));([11;14],(11,[14]));([10;15],(10,[15]));([9;16],(9,[16]));([8;17],(17,[]));([7;18],(7,[18]));([6;19],(19,[]));([5;20],(20,[]));([4;21],(21,[]));([3;22],(22,[]));([2;23],(23,[]));([1;24],(24,[]))] [1;2;3;5;10;15;25] ;;
pointed_card [26] [([13],(13,[]));([12;14],(12,[14]));([11;15],(11,[15]));([10;16],(10,[16]));([9;17],(9,[17]));([8;18],(18,[]));([7;19],(7,[19]));([6;20],(20,[]));([5;21],(21,[]));([4;22],(22,[]));([3;23],(23,[]));([2;24],(24,[]));([1;25],(25,[]))] [1;2;3;5;8;13;26] ;;
pointed_card [20;27] [([13;14;20],(13,[14;20]));([12;15;20],(12,[15;20]));([11;16;20],(11,[16;20]));([10;17;20],(10,[17;20]));([9;18;20],(9,[18;20]));([8;19;20],(19,[20]));([6;20;21],(20,[21]));([5;20;22],(20,[22]));([4;20;23],(20,[23]));([3;20;24],(20,[24]));([2;20;25],(20,[25]));([1;20;26],(20,[26]))] [1;2;3;4;7;10;20;27] ;;
simplest_case 27 [1;2;3;6;9;18;27] ;;
pointed_card [28] [([14],(14,[]));([13;15],(13,[15]));([12;16],(12,[16]));([11;17],(11,[17]));([10;18],(10,[18]));([9;19],(9,[19]));([8;20],(20,[]));([7;21],(7,[21]));([6;22],(22,[]));([5;23],(23,[]));([4;24],(24,[]));([3;25],(25,[]));([2;26],(26,[]));([1;27],(27,[]))] [1;2;3;4;7;14;28] ;;
pointed_card [29] [([14;15],(14,[15]));([13;16],(13,[16]));([11;18],(11,[18]));([10;19],(19,[]));([8;21],(21,[]));([7;22],(22,[]));([6;23],(23,[]));([4;25],(25,[]));([3;26],(26,[]));([2;27],(27,[]));([1;28],(28,[]))] [1;2;3;4;7;11;18;29] ;;
pointed_card [17;30] [([13;17],(13,[17]));([15;17],(15,[17]));([14;16;17],(14,[16;17]));([12;17;18],(12,[17;18]));([11;17;19],(11,[17;19]));([10;17;20],(10,[17;20]));([9;17;21],(9,[17;21]));([8;17;22],(17,[22]));([7;17;23],(7,[17;23]));([6;17;24],(17,[24]));([5;17;25],(17,[25]));([4;17;26],(17,[26]));([3;17;27],(17,[27]));([2;17;28],(17,[28]));([1;17;29],(17,[29]))] [1;2;3;5;10;15;17;30] ;;
simplest_case 30 [1;2;3;5;10;15;30] ;;
pointed_card [31] [([15;16],(15,[16]));([14;17],(14,[17]));([13;18],(13,[18]));([12;19],(19,[]));([11;20],(11,[20]));([10;21],(21,[]));([9;22],(22,[]));([8;23],(23,[]));([6;25],(25,[]));([5;26],(26,[]));([4;27],(27,[]));([3;28],(28,[]));([2;29],(29,[]));([1;30],(30,[]))] [1;2;3;4;7;14;17;31] ;;
pointed_card [5;16;32] [([5;16;27],(5,[16;27]));([5;15;16;17],(5,[15;16;17]));([5;14;16;18],(5,[14;16;18]));([5;13;16;19],(5,[13;16;19]));([5;12;16;20],(5,[12;16;20]));([5;11;16;21],(5,[11;16;21]));([5;10;16;22],(5,[10;16;22]));([5;9;16;23],(5,[9;16;23]));([5;8;16;24],(5,[8;16;24]));([5;7;16;25],(5,[7;16;25]));([5;6;16;26],(5,[6;16;26]));([4;5;16;28],(4,[5;16;28]));([3;5;16;29],(3,[5;16;29]));([2;5;16;30],(5,[16;30]));([1;5;16;31],(5,[16;31]))] [1;2;3;5;8;16;32] ;;
pointed_card [3;32] [([3;29],(29,[]));([3;15;17],(15,[17]));([3;14;18],(14,[18]));([3;13;19],(13,[19]));([3;12;20],(12,[20]));([3;11;21],(11,[21]));([3;10;22],(10,[22]));([3;9;23],(9,[23]));([3;8;24],(24,[]));([3;7;25],(7,[25]));([3;6;26],(26,[]));([3;5;27],(27,[]));([3;4;28],(28,[]));([2;3;30],(30,[]));([1;3;31],(31,[]))] [1;2;3;4;8;16;32] ;;
pointed_card [5;32] [([5;27],(27,[]));([5;15;17],(5,[15;17]));([5;14;18],(5,[14;18]));([5;13;19],(5,[13;19]));([5;12;20],(5,[12;20]));([5;11;21],(5,[11;21]));([5;10;22],(5,[10;22]));([5;9;23],(5,[9;23]));([5;8;24],(5,[8;24]));([5;7;25],(5,[7;25]));([5;6;26],(5,[6;26]));([4;5;28],(28,[]));([3;5;29],(29,[]));([2;5;30],(30,[]));([1;5;31],(31,[]))] [1;2;3;5;8;16;32] ;;
pointed_card [6;32] [([6;26],(26,[]));([6;15;17],(6,[15;17]));([6;14;18],(6,[14;18]));([6;13;19],(6,[13;19]));([6;12;20],(6,[12;20]));([6;11;21],(6,[11;21]));([6;10;22],(6,[10;22]));([6;9;23],(6,[9;23]));([6;8;24],(6,[8;24]));([6;7;25],(6,[7;25]));([5;6;27],(5,[6;27]));([4;6;28],(28,[]));([3;6;29],(29,[]));([2;6;30],(30,[]));([1;6;31],(31,[]))] [1;2;3;6;8;16;32] ;;
pointed_card [7;32] [([7;25],(25,[]));([7;15;17],(7,[15;17]));([7;14;18],(7,[14;18]));([7;13;19],(7,[13;19]));([7;12;20],(7,[12;20]));([7;11;21],(7,[11;21]));([7;10;22],(7,[10;22]));([7;9;23],(7,[9;23]));([7;8;24],(7,[8;24]));([6;7;26],(26,[]));([5;7;27],(27,[]));([4;7;28],(28,[]));([3;7;29],(29,[]));([2;7;30],(30,[]));([1;7;31],(31,[]))] [1;2;3;4;7;8;16;32] ;;
pointed_card [9;32] [([9;16],(9,[16]));([9;23],(9,[23]));([9;15;17],(9,[15;17]));([9;14;18],(9,[14;18]));([9;13;19],(9,[13;19]));([9;12;20],(9,[12;20]));([9;11;21],(9,[11;21]));([9;10;22],(9,[10;22]));([8;9;24],(8,[9;24]));([7;9;25],(7,[9;25]));([6;9;26],(6,[9;26]));([5;9;27],(5,[9;27]));([4;9;28],(9,[28]));([3;9;29],(9,[29]));([2;9;30],(9,[30]));([1;9;31],(9,[31]))] [1;2;4;8;9;16;32] ;;
pointed_card [10;32] [([10;16],(10,[16]));([10;22],(10,[22]));([10;15;17],(10,[15;17]));([10;14;18],(10,[14;18]));([10;13;19],(10,[13;19]));([10;12;20],(10,[12;20]));([10;11;21],(10,[11;21]));([9;10;23],(9,[10;23]));([8;10;24],(8,[10;24]));([7;10;25],(7,[10;25]));([6;10;26],(6,[10;26]));([5;10;27],(5,[10;27]));([4;10;28],(10,[28]));([3;10;29],(10,[29]));([2;10;30],(10,[30]));([1;10;31],(10,[31]))] [1;2;4;6;10;16;32] ;;
pointed_card [12;32] [([12;16],(12,[16]));([12;20],(12,[20]));([12;15;17],(12,[15;17]));([12;14;18],(12,[14;18]));([12;13;19],(12,[13;19]));([11;12;21],(11,[12;21]));([10;12;22],(10,[12;22]));([9;12;23],(9,[12;23]));([8;12;24],(8,[12;24]));([7;12;25],(7,[12;25]));([6;12;26],(6,[12;26]));([5;12;27],(5,[12;27]));([4;12;28],(12,[28]));([3;12;29],(12,[29]));([2;12;30],(12,[30]));([1;12;31],(12,[31]))] [1;2;4;6;12;16;32] ;;
pointed_card [15;32] [([15;16],(15,[16]));([15;17],(15,[17]));([14;15;18],(14,[15;18]));([13;15;19],(13,[15;19]));([12;15;20],(12,[15;20]));([11;15;21],(11,[15;21]));([10;15;22],(10,[15;22]));([9;15;23],(9,[15;23]));([8;15;24],(15,[24]));([7;15;25],(7,[15;25]));([6;15;26],(15,[26]));([5;15;27],(15,[27]));([4;15;28],(15,[28]));([3;15;29],(15,[29]));([2;15;30],(15,[30]));([1;15;31],(15,[31]))] [1;2;3;5;10;15;16;32] ;;
simplest_case 32 [1;2;4;8;16;32] ;;
pointed_card [14;33] [([14;19],(14,[19]));([14;16;17],(14,[16;17]));([14;15;18],(14,[15;18]));([13;14;20],(13,[14;20]));([12;14;21],(12,[14;21]));([11;14;22],(11,[14;22]));([10;14;23],(10,[14;23]));([9;14;24],(9,[14;24]));([8;14;25],(14,[25]));([7;14;26],(7,[14;26]));([6;14;27],(14,[27]));([5;14;28],(14,[28]));([4;14;29],(14,[29]));([3;14;30],(14,[30]));([2;14;31],(14,[31]));([1;14;32],(14,[32]))] [1;2;3;5;7;14;19;33] ;;
pointed_card [20;33] [([13;20],(13,[20]));([16;17;20],(16,[17;20]));([15;18;20],(15,[18;20]));([14;19;20],(14,[19;20]));([12;20;21],(12,[20;21]));([11;20;22],(11,[20;22]));([10;20;23],(10,[20;23]));([9;20;24],(9,[20;24]));([8;20;25],(20,[25]));([7;20;26],(7,[20;26]));([6;20;27],(20,[27]));([5;20;28],(20,[28]));([4;20;29],(20,[29]));([3;20;30],(20,[30]));([2;20;31],(20,[31]));([1;20;32],(20,[32]))] [1;2;3;5;10;13;20;33] ;;
simplest_case 33 [1;2;4;8;16;17;33] ;;
pointed_card [13;34] [([13;17],(13,[17]));([13;21],(13,[21]));([13;16;18],(13,[16;18]));([13;15;19],(13,[15;19]));([13;14;20],(13,[14;20]));([12;13;22],(12,[13;22]));([11;13;23],(11,[13;23]));([10;13;24],(10,[13;24]));([9;13;25],(9,[13;25]));([8;13;26],(13,[26]));([7;13;27],(7,[13;27]));([6;13;28],(13,[28]));([5;13;29],(13,[29]));([4;13;30],(13,[30]));([3;13;31],(13,[31]));([2;13;32],(13,[32]));([1;13;33],(13,[33]))] [1;2;3;5;8;13;21;34] ;;
simplest_case 34 [1;2;4;8;9;17;34] ;;
pointed_card [35] [([17;18],(17,[18]));([16;19],(19,[]));([15;20],(15,[20]));([14;21],(14,[21]));([13;22],(13,[22]));([12;23],(23,[]));([11;24],(11,[24]));([10;25],(25,[]));([9;26],(26,[]));([8;27],(27,[]));([7;28],(28,[]));([6;29],(29,[]));([5;30],(30,[]));([4;31],(31,[]));([2;33],(33,[]));([1;34],(34,[]))] [1;2;3;4;7;14;21;35] ;;
pointed_card [11;36] [([11;18],(11,[18]));([11;25],(11,[25]));([11;17;19],(11,[17;19]));([11;16;20],(11,[16;20]));([11;15;21],(11,[15;21]));([11;14;22],(11,[14;22]));([11;13;23],(11,[13;23]));([11;12;24],(11,[12;24]));([10;11;26],(10,[11;26]));([9;11;27],(9,[11;27]));([8;11;28],(11,[28]));([7;11;29],(7,[11;29]));([6;11;30],(11,[30]));([5;11;31],(11,[31]));([4;11;32],(11,[32]));([3;11;33],(11,[33]));([2;11;34],(11,[34]));([1;11;35],(11,[35]))] [1;2;3;4;7;11;18;36] ;;
pointed_card [17;36] [([17;18],(17,[18]));([17;19],(17,[19]));([16;17;20],(16,[17;20]));([15;17;21],(15,[17;21]));([14;17;22],(14,[17;22]));([13;17;23],(13,[17;23]));([12;17;24],(12,[17;24]));([11;17;25],(11,[17;25]));([10;17;26],(10,[17;26]));([9;17;27],(9,[17;27]));([8;17;28],(17,[28]));([7;17;29],(7,[17;29]));([6;17;30],(17,[30]));([5;17;31],(17,[31]));([4;17;32],(17,[32]));([3;17;33],(17,[33]));([2;17;34],(17,[34]));([1;17;35],(17,[35]))] [1;2;4;8;9;17;18;36] ;;
simplest_case 36 [1;2;3;6;9;18;36] ;;
pointed_card [37] [([18;19],(18,[19]));([17;20],(17,[20]));([16;21],(21,[]));([15;22],(15,[22]));([14;23],(14,[23]));([13;24],(13,[24]));([12;25],(25,[]));([11;26],(11,[26]));([10;27],(27,[]));([9;28],(28,[]));([8;29],(29,[]));([7;30],(30,[]));([6;31],(31,[]));([4;33],(33,[]));([3;34],(34,[]));([2;35],(35,[]));([1;36],(36,[]))] [1;2;3;5;8;16;21;37] ;;
pointed_card [38] [([19],(19,[]));([18;20],(18,[20]));([17;21],(17,[21]));([16;22],(22,[]));([15;23],(15,[23]));([14;24],(14,[24]));([13;25],(13,[25]));([12;26],(26,[]));([11;27],(11,[27]));([10;28],(28,[]));([9;29],(29,[]));([8;30],(30,[]));([7;31],(31,[]));([5;33],(33,[]));([4;34],(34,[]));([3;35],(35,[]));([2;36],(36,[]));([1;37],(37,[]))] [1;2;3;4;8;11;19;38] ;;
pointed_card [39] [([19;20],(19,[20]));([18;21],(18,[21]));([17;22],(17,[22]));([16;23],(23,[]));([15;24],(15,[24]));([14;25],(14,[25]));([13;26],(13,[26]));([12;27],(27,[]));([11;28],(11,[28]));([10;29],(29,[]));([9;30],(30,[]));([8;31],(31,[]));([6;33],(33,[]));([5;34],(34,[]));([4;35],(35,[]));([3;36],(36,[]));([2;37],(37,[]));([1;38],(38,[]))] [1;2;3;5;8;13;26;39] ;;
pointed_card [7;40] [([7;33],(33,[]));([7;19;21],(7,[19;21]));([7;18;22],(7,[18;22]));([7;17;23],(7,[17;23]));([7;16;24],(7,[16;24]));([7;15;25],(7,[15;25]));([7;14;26],(7,[14;26]));([7;13;27],(7,[13;27]));([7;12;28],(7,[12;28]));([7;11;29],(7,[11;29]));([7;10;30],(7,[10;30]));([7;9;31],(7,[9;31]));([7;8;32],(7,[8;32]));([6;7;34],(34,[]));([5;7;35],(35,[]));([4;7;36],(36,[]));([3;7;37],(37,[]));([2;7;38],(38,[]));([1;7;39],(39,[]))] [1;2;3;4;7;10;20;40] ;;
pointed_card [13;40] [([13;20],(13,[20]));([13;27],(13,[27]));([13;19;21],(13,[19;21]));([13;18;22],(13,[18;22]));([13;17;23],(13,[17;23]));([13;16;24],(13,[16;24]));([13;15;25],(13,[15;25]));([13;14;26],(13,[14;26]));([12;13;28],(12,[13;28]));([11;13;29],(11,[13;29]));([10;13;30],(10,[13;30]));([9;13;31],(9,[13;31]));([8;13;32],(13,[32]));([7;13;33],(7,[13;33]));([6;13;34],(13,[34]));([5;13;35],(13,[35]));([4;13;36],(13,[36]));([3;13;37],(13,[37]));([2;13;38],(13,[38]));([1;13;39],(13,[39]))] [1;2;3;5;10;13;20;40] ;;
pointed_card [15;40] [([15;20],(15,[20]));([15;25],(15,[25]));([15;19;21],(15,[19;21]));([15;18;22],(15,[18;22]));([15;17;23],(15,[17;23]));([15;16;24],(15,[16;24]));([14;15;26],(14,[15;26]));([13;15;27],(13,[15;27]));([12;15;28],(12,[15;28]));([11;15;29],(11,[15;29]));([10;15;30],(10,[15;30]));([9;15;31],(9,[15;31]));([8;15;32],(15,[32]));([7;15;33],(7,[15;33]));([6;15;34],(15,[34]));([5;15;35],(15,[35]));([4;15;36],(15,[36]));([3;15;37],(15,[37]));([2;15;38],(15,[38]));([1;15;39],(15,[39]))] [1;2;3;5;10;15;20;40] ;;
simplest_case 40 [1;2;3;5;10;20;40] ;;
pointed_card [41] [([20;21],(20,[21]));([19;22],(19,[22]));([18;23],(18,[23]));([17;24],(17,[24]));([16;25],(25,[]));([15;26],(15,[26]));([14;27],(14,[27]));([13;28],(13,[28]));([12;29],(29,[]));([11;30],(11,[30]));([10;31],(31,[]));([8;33],(33,[]));([7;34],(34,[]));([6;35],(35,[]));([5;36],(36,[]));([4;37],(37,[]));([3;38],(38,[]));([2;39],(39,[]));([1;40],(40,[]))] [1;2;3;5;10;20;21;41] ;;
pointed_card [42] [([21],(21,[]));([20;22],(20,[22]));([19;23],(19,[23]));([18;24],(18,[24]));([17;25],(17,[25]));([16;26],(26,[]));([15;27],(15,[27]));([14;28],(14,[28]));([13;29],(13,[29]));([12;30],(30,[]));([11;31],(11,[31]));([9;33],(33,[]));([8;34],(34,[]));([7;35],(35,[]));([6;36],(36,[]));([5;37],(37,[]));([4;38],(38,[]));([3;39],(39,[]));([2;40],(40,[]));([1;41],(41,[]))] [1;2;3;4;7;14;21;42] ;;
pointed_card [43] [([21;22],(21,[22]));([20;23],(20,[23]));([19;24],(19,[24]));([18;25],(18,[25]));([17;26],(17,[26]));([16;27],(27,[]));([15;28],(15,[28]));([14;29],(14,[29]));([13;30],(13,[30]));([12;31],(31,[]));([11;32],(11,[32]));([10;33],(33,[]));([9;34],(34,[]));([8;35],(35,[]));([7;36],(36,[]));([6;37],(37,[]));([5;38],(38,[]));([4;39],(39,[]));([3;40],(40,[]));([2;41],(41,[]));([1;42],(42,[]))] [1;2;3;5;10;20;23;43] ;;
pointed_card [44] [([22],(22,[]));([21;23],(21,[23]));([20;24],(20,[24]));([19;25],(19,[25]));([18;26],(18,[26]));([17;27],(17,[27]));([16;28],(28,[]));([15;29],(15,[29]));([14;30],(14,[30]));([13;31],(13,[31]));([11;33],(11,[33]));([10;34],(34,[]));([9;35],(35,[]));([8;36],(36,[]));([7;37],(37,[]));([6;38],(38,[]));([5;39],(39,[]));([4;40],(40,[]));([3;41],(41,[]));([2;42],(42,[]));([1;43],(43,[]))] [1;2;3;4;7;11;22;44] ;;
pointed_card [45] [([22;23],(22,[23]));([21;24],(21,[24]));([20;25],(20,[25]));([19;26],(19,[26]));([18;27],(18,[27]));([17;28],(17,[28]));([16;29],(29,[]));([15;30],(15,[30]));([14;31],(14,[31]));([13;32],(13,[32]));([12;33],(33,[]));([11;34],(11,[34]));([10;35],(35,[]));([9;36],(36,[]));([8;37],(37,[]));([7;38],(38,[]));([6;39],(39,[]));([5;40],(40,[]));([4;41],(41,[]));([3;42],(42,[]));([2;43],(43,[]));([1;44],(44,[]))] [1;2;3;5;10;15;30;45] ;;
pointed_card [46] [([23],(23,[]));([22;24],(22,[24]));([21;25],(21,[25]));([20;26],(20,[26]));([19;27],(19,[27]));([18;28],(18,[28]));([17;29],(17,[29]));([16;30],(30,[]));([15;31],(15,[31]));([14;32],(14,[32]));([13;33],(13,[33]));([12;34],(34,[]));([11;35],(11,[35]));([10;36],(36,[]));([9;37],(37,[]));([8;38],(38,[]));([7;39],(39,[]));([6;40],(40,[]));([5;41],(41,[]));([4;42],(42,[]));([3;43],(43,[]));([2;44],(44,[]));([1;45],(45,[]))] [1;2;3;5;10;13;23;46] ;;
pointed_card [47] [([23;24],(23,[24]));([22;25],(22,[25]));([21;26],(21,[26]));([19;28],(19,[28]));([18;29],(29,[]));([16;31],(31,[]));([12;35],(35,[]));([10;37],(37,[]));([9;38],(38,[]));([8;39],(39,[]));([6;41],(41,[]));([5;42],(42,[]));([4;43],(43,[]));([3;44],(44,[]));([2;45],(45,[]));([1;46],(46,[]))] [1;2;3;4;7;10;20;27;47] ;;
pointed_card [5;48] [([5;43],(43,[]));([5;23;25],(23,[25]));([5;22;26],(22,[26]));([5;21;27],(21,[27]));([5;20;28],(20,[28]));([5;19;29],(19,[29]));([5;18;30],(18,[30]));([5;17;31],(17,[31]));([5;15;33],(15,[33]));([5;14;34],(14,[34]));([5;13;35],(13,[35]));([5;12;36],(36,[]));([5;11;37],(11,[37]));([5;10;38],(38,[]));([5;9;39],(39,[]));([5;8;40],(40,[]));([5;7;41],(41,[]));([5;6;42],(42,[]));([4;5;44],(44,[]));([3;5;45],(45,[]));([2;5;46],(46,[]));([1;5;47],(47,[]))] [1;2;3;5;6;12;24;48] ;;
pointed_card [7;48] [([7;41],(41,[]));([7;23;25],(7,[23;25]));([7;22;26],(7,[22;26]));([7;21;27],(7,[21;27]));([7;20;28],(7,[20;28]));([7;19;29],(7,[19;29]));([7;18;30],(7,[18;30]));([7;17;31],(7,[17;31]));([7;16;32],(7,[16;32]));([7;15;33],(7,[15;33]));([7;14;34],(7,[14;34]));([7;13;35],(7,[13;35]));([7;12;36],(7,[12;36]));([7;11;37],(7,[11;37]));([7;10;38],(7,[10;38]));([7;9;39],(7,[9;39]));([7;8;40],(7,[8;40]));([6;7;42],(42,[]));([5;7;43],(43,[]));([4;7;44],(44,[]));([3;7;45],(45,[]));([2;7;46],(46,[]));([1;7;47],(47,[]))] [1;2;3;5;7;12;24;48] ;;
simplest_case 48 [1;2;3;6;12;24;48] ;;
pointed_card [49] [([24;25],(24,[25]));([23;26],(23,[26]));([22;27],(22,[27]));([21;28],(21,[28]));([20;29],(20,[29]));([19;30],(19,[30]));([18;31],(18,[31]));([17;32],(17,[32]));([16;33],(33,[]));([15;34],(15,[34]));([14;35],(14,[35]));([13;36],(13,[36]));([12;37],(37,[]));([11;38],(11,[38]));([10;39],(39,[]));([9;40],(40,[]));([8;41],(41,[]));([7;42],(42,[]));([6;43],(43,[]));([5;44],(44,[]));([4;45],(45,[]));([3;46],(46,[]));([2;47],(47,[]));([1;48],(48,[]))] [1;2;3;6;12;24;25;49] ;;
pointed_card [50] [([25],(25,[]));([24;26],(24,[26]));([23;27],(23,[27]));([22;28],(22,[28]));([21;29],(21,[29]));([20;30],(20,[30]));([19;31],(19,[31]));([18;32],(18,[32]));([17;33],(17,[33]));([16;34],(34,[]));([15;35],(15,[35]));([14;36],(14,[36]));([13;37],(13,[37]));([12;38],(38,[]));([11;39],(11,[39]));([10;40],(40,[]));([9;41],(41,[]));([8;42],(42,[]));([7;43],(43,[]));([6;44],(44,[]));([5;45],(45,[]));([4;46],(46,[]));([3;47],(47,[]));([2;48],(48,[]));([1;49],(49,[]))] [1;2;3;5;10;15;25;50] ;;
simplest_case 51 [1;2;3;6;12;24;27;51] ;;
pointed_card [52] [([26],(26,[]));([25;27],(25,[27]));([24;28],(24,[28]));([23;29],(23,[29]));([22;30],(22,[30]));([21;31],(21,[31]));([20;32],(20,[32]));([19;33],(19,[33]));([18;34],(18,[34]));([17;35],(17,[35]));([16;36],(36,[]));([15;37],(15,[37]));([14;38],(14,[38]));([13;39],(13,[39]));([12;40],(40,[]));([11;41],(11,[41]));([10;42],(42,[]));([9;43],(43,[]));([8;44],(44,[]));([7;45],(45,[]));([6;46],(46,[]));([5;47],(47,[]));([4;48],(48,[]));([3;49],(49,[]));([2;50],(50,[]));([1;51],(51,[]))] [1;2;3;5;8;13;26;52] ;;
pointed_card [53] [([26;27],(26,[27]));([25;28],(25,[28]));([24;29],(29,[]));([23;30],(23,[30]));([22;31],(22,[31]));([21;32],(21,[32]));([19;34],(19,[34]));([18;35],(35,[]));([16;37],(37,[]));([15;38],(38,[]));([14;39],(39,[]));([12;41],(41,[]));([11;42],(42,[]));([10;43],(43,[]));([9;44],(44,[]));([8;45],(45,[]));([7;46],(46,[]));([6;47],(47,[]));([4;49],(49,[]));([3;50],(50,[]));([2;51],(51,[]));([1;52],(52,[]))] [1;2;3;5;6;12;24;29;53] ;;
simplest_case 54 [1;2;3;6;9;18;27;54] ;;
pointed_card [55] [([27;28],(27,[28]));([26;29],(26,[29]));([25;30],(25,[30]));([24;31],(31,[]));([23;32],(23,[32]));([22;33],(22,[33]));([21;34],(21,[34]));([20;35],(35,[]));([19;36],(19,[36]));([18;37],(37,[]));([17;38],(38,[]));([16;39],(39,[]));([14;41],(41,[]));([13;42],(42,[]));([12;43],(43,[]));([11;44],(44,[]));([10;45],(45,[]));([9;46],(46,[]));([8;47],(47,[]));([6;49],(49,[]));([5;50],(50,[]));([4;51],(51,[]));([3;52],(52,[]));([2;53],(53,[]));([1;54],(54,[]))] [1;2;3;4;7;11;22;33;55] ;;



type abbreviated_move = P of int list |S of int ;;

let all_moves = [S 2; S 3; S 4; S 5; S 6; P [7]; P [3; 8]; S 8; S 9; S 10; P [11]; P [5; 12];
S 12; P [13]; P [14]; S 15; P [3; 16]; P [5; 16]; P [6; 16]; P [7; 16]; 
S 16; P [12; 17]; S 17; S 18; P [19]; P [7; 20]; P [9; 20]; S 20; P [21];
P [22]; P [23]; P [5; 24]; P [7; 24]; S 24; P [25]; P [26]; P [20; 27]; 
S 27; P [28]; P [29]; P [17; 30]; S 30; P [31]; P [5; 16; 32]; P [3; 32];
P [5; 32]; P [6; 32]; P [7; 32]; P [9; 32]; P [10; 32]; P [12; 32];
P [15; 32]; S 32; P [14; 33]; P [20; 33]; S 33; P [13; 34]; S 34; P [35];
P [11; 36]; P [17; 36]; S 36; P [37]; P [38]; P [39]; P [7; 40]; P [13; 40];
P [15; 40]; S 40; P [41]; P [42]; P [43]; P [44]; P [45]; P [46]; P [47];
P [5; 48]; P [7; 48]; S 48; P [49]; P [50]; S 51; P [52]; P [53]; S 54;
P [55]] ;;

type expanded_move = ES of int * int | EP of (int list) * int * ((int * int list) list) * ((int list) list) ;;

let expand_move  = function
  S i->ES(i,impatient_measure [i])
  |(P l)-> 
  let temp1 = abauzit_expansion l 
   and bound = (impatient_measure l)-1 in 
  let temp2 = Image.image (fun l2-> (l2,adrien_analysis (l2,bound)) ) temp1 in 
  let (good_temp2,bad_temp2) = List.partition (fun (l2,opt)->opt<>None) temp2 in 
  let pointed_ones = Image.image (fun (l2,opt)->Option.get opt) good_temp2 in 
  let temp4 = Image.image  fst bad_temp2 in 
  EP(l,bound+1,pointed_ones,temp4) ;; 

let all_expanded_moves = Image.image expand_move all_moves ;;  

let write_es_to_mathjax i m = 
    (soi i)^ " & " ^(soi m)^ " & \\textrm{Simplest case}" ;;
    
let intlist_to_mathjax l = "\\lbrace "^(String.concat "," (Image.image soi l))^" \\rbrace";;

let pointed_one_to_mathjax (head,passive) =
   let passive_component = (
     if passive = [] 
     then "" 
     else "|"^(String.concat "," (Image.image soi passive))) in  
     "["^(soi head)^passive_component^"]" ;; 

let pointed_ones_to_mathjax pointed_ones = 
  String.concat "" (Image.image pointed_one_to_mathjax pointed_ones) ;;

let helpers_to_mathjax helpers = 
  String.concat "" (Image.image intlist_to_mathjax helpers) ;;

let write_ep_to_mathjax l m pointed_ones helpers = 
      (intlist_to_mathjax l)^ " & " ^(soi m)^ " & "^ 
      (pointed_ones_to_mathjax pointed_ones)^(helpers_to_mathjax helpers) ;;    
    
let write_expanded_move_to_mathjax = function 
 ES(i,m) -> write_es_to_mathjax i m 
 |EP(l,m,pointed_ones,helpers) -> write_ep_to_mathjax l m pointed_ones helpers ;;

let backslash = "\\" ;;
let double_backslash = backslash ^ backslash ;;

 let write_expanded_move_to_mathjax_line x = 
    (write_expanded_move_to_mathjax x)^double_backslash^"\n\\hline" ;;

let array_in_mathjax l =
   String.concat "\n"
    (
    "\\begin{array}{|r|c|l|}\n\\hline"::
    (" A & \\mu & \\textrm{Explanations}"^double_backslash^"\n\\hline")::  
     (Image.image write_expanded_move_to_mathjax_line l)@ 
    [
    "\\end{array}"
    ]
    ) ;;

let max_part_size = 15 ;;

let cut_into_small_parts  l ~max_part_size =
  let rec tempf = (
      fun (treated,to_be_treated,remaining_size) -> 
           if remaining_size <= max_part_size 
           then List.rev(to_be_treated::treated) 
           else let (reversed_left,right) = List_again.long_head_with_tail max_part_size to_be_treated in 
                let left = List.rev reversed_left in 
                tempf(left::treated,right,remaining_size-max_part_size)
  ) in 
  tempf ([],l,List.length l) ;;

(* cut_into_small_parts (Ennig.ennig 1 7) ~max_part_size:3 ;; *)

let parts = cut_into_small_parts 
all_expanded_moves ~max_part_size ;;

let prelude ="/////////////////////////////////////////////////////////\nQuestion : \n/////////////////////////////////////////////////////////\n\n" ;;

let arrays_in_mathjax = prelude ^ (String.concat "\n\n\n" (Image.image array_in_mathjax parts)) ;; 

let the_ap = Absolute_path.of_string "~/Teuliou/Bash_scripts/example_maath.txt";;

let act () = Io.overwrite_with the_ap arrays_in_mathjax  ;;

(*

#use "Fads/nap.ml";;

*)

(************************************************************************************************************************
Snippet 61 : Transfer a large snippet from one file to another
************************************************************************************************************************)
open Needed_values ;;

let z1 = rf "Fads/nap.ml" ;;
let z2 = Lines_in_text.interval z1 209 298 ;;
let ap = Absolute_path.of_string "Fads/pan.ml" ;;
let z3 () = Io.Private.append_string_to_file z2 ap ;;

type ps = P of int list |S of int ;;

let accu = ref [] ;;

let simplest_case i x = (accu:=(S i)::(!accu)) ;;
let pointed_card a b c = (accu:=(P a)::(!accu)) ;;

(************************************************************************************************************************
Snippet 60 : Third stab at boundary operator combinatorics 
************************************************************************************************************************)
open Needed_values ;;

let i_order = Total_ordering.for_integers ;;
let il_order = Total_ordering.silex_compare i_order ;;
let j_order = Total_ordering.product i_order il_order ;;

let i_fold_merge = Ordered.fold_merge i_order ;;
let i_intersection = Ordered.intersect i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_setminus = Ordered.setminus i_order ;;
let i_sort = Ordered.sort i_order ;;

let il_sort = Ordered.sort il_order ;;
let il_setminus = Ordered.setminus il_order ;;


let j_merge = Ordered.merge j_order ;;

type set_index = S of int ;;

let n1 = 4 ;;
let whole = Int_range.range 1 n1 ;;
let u1 = il_sort (List_again.power_set whole) ;;
let rtl l = List.rev (List.tl l);;
let u2 = rtl (rtl u1);;
let normal_form x = 
     let cx = i_setminus whole x in 
     if (il_order x cx)=Total_ordering_result_t.Lower 
     then x else cx ;;
let u3 = List.filter (fun x->normal_form(x)=x) u2;;      
let u4 = Int_range.index_everything u3 ;;
let u5 = Image.image (fun (j,z)->(z,S(j+2))) u4 ;;
let u6 = Image.image (fun x->let nx=normal_form x in (x,List.assoc nx u5)) u2;;


module Atom = struct 

type t = A of int ;;

let order = ((fun (A i) (A j)->Total_ordering.for_integers i j) : t Total_ordering_t.t) ;; 
  
let table_for_sets_containing_a_given_atom = 
    let temp = Image.image (Image.image (fun i->S i)) 
    (il_sort(List_again.power_set (Int_range.range 1 9))) in 
    Image.image (fun (j,l)->(A j,l)) (Int_range.index_everything temp) ;;
let all = Image.image fst table_for_sets_containing_a_given_atom ;;

let check_boolean_constraints constraints atm =
    let associated_sets = List.assoc atm table_for_sets_containing_a_given_atom in 
    List.for_all (fun (set,bowl)->(List.mem set associated_sets)=bowl) constraints ;;

let unveil (A i) = i ;;

end ;;  

module Early_atom = struct 

type t = EA of int ;;
  
let order = ((fun (EA i) (EA j)->Total_ordering.for_integers i j) : t Total_ordering_t.t) ;; 
    
let table_for_sets_containing_a_given_early_atom = 
      let temp = Image.image (Image.image (fun i->S i)) 
      (il_sort(List_again.power_set (Int_range.range 1 2))) in 
      Image.image (fun (j,l)->(EA j,l)) (Int_range.index_everything temp) ;;
let all = Image.image fst table_for_sets_containing_a_given_early_atom ;;

let to_boolean_combination atm = 
  let associated_sets = List.assoc atm table_for_sets_containing_a_given_early_atom in 
   Image.image (fun s->(s,List.mem s associated_sets)) [S 1;S 2] ;;

let unveil (EA i) = i ;;

end ;;  

module Early_union = struct 

let order = Total_ordering.silex_compare  Early_atom.order ;; 
let whole = Int_range.scale (fun j->Early_atom.EA j)  1 4;;
let all = Image.image (Image.image (fun j->Early_atom.EA j)) u2 ;;
let all_pairs = Uple.list_of_pairs all ;;
let nondisjoint_pairs =
    List.filter (fun (a,b)->
       not(Ordered.does_not_intersect Early_atom.order a b)
      ) all_pairs ;;
let normal_form x = 
    let cx = Ordered.setminus Early_atom.order whole x in 
    if (order cx x)=Total_ordering_result_t.Lower 
    then cx else x ;;
let all_normal_forms = Image.image normal_form all ;;    
let table_for_image_sets = Image.image (fun x->
  (x,S(2+(List_again.find_index_of_in (normal_form x) all_normal_forms))) 
) all ;;
let image_set x = List.assoc x table_for_image_sets ;;
let intersection x y = Ordered.intersect Early_atom.order x y;;
let union x y = Ordered.merge Early_atom.order x y;;

let unveil l = Image.image Early_atom.unveil l;;

end ;;  



module Molecule = struct 

type t = M of Atom.t list ;;  

let fold_union pre_ll = 
    let ll =  Image.image (fun (M m)-> m) pre_ll in 
    M (Ordered.fold_merge Atom.order ll);;
let intersection (M x) (M y) = M (Ordered.intersect Atom.order x y);;
let setminus (M x) (M y) = M (Ordered.setminus Atom.order x y);;
let union (M x) (M y) = M (Ordered.merge Atom.order x y);;


let delta mx my = union (setminus mx my) (setminus my mx) ;; 

let of_set set_idx =
    M( List.filter_map (
       fun (atm_idx,l)->if List.mem set_idx l then Some atm_idx else None
    ) Atom.table_for_sets_containing_a_given_atom ) ;;

let complement_of_set set_idx = 
  M( List.filter_map (
    fun (atm_idx,l)->if not(List.mem set_idx l) then Some atm_idx else None
 ) Atom.table_for_sets_containing_a_given_atom ) ;;

let of_boolean_combination constraints =
  M(List.filter_map (
    fun (atm_idx,l)->if Atom.check_boolean_constraints constraints atm_idx then Some atm_idx else None
 ) Atom.table_for_sets_containing_a_given_atom) ;;

let of_early_atom eatm = 
  of_boolean_combination (Early_atom.to_boolean_combination eatm) ;;

let of_early_union eatm_l = fold_union (Image.image of_early_atom eatm_l) ;;
    
let of_early_union_image eatm_l = of_set (Early_union.image_set eatm_l) ;;


let main_test (pre_a,pre_b) =
    let a = of_early_union pre_a 
    and b = of_early_union pre_b in 
    let pre_c = Early_union.intersection pre_a pre_b 
    and c = intersection a b in 
    let hand1 = intersection c (of_early_union_image pre_c)
    and hand2 = intersection c (union (of_early_union_image pre_a)  (of_early_union_image pre_b)) in 
    delta hand1 hand2 ;;

let unveil (M l) = Image.image Atom.unveil l ;; 

end ;;  



let z1 = Early_union.nondisjoint_pairs ;;
let veiled_z2 = Image.image (fun (a,b)->((a,b),Molecule.main_test (a,b)) ) z1 ;;
let z2 =  Image.image (fun ((a,b),m)->(Early_union.unveil a,Early_union.unveil b,Molecule.unveil m) ) veiled_z2 ;;   

let aa = Image.image (fun x->Early_atom.EA x) [1;2] ;;
let bb = Image.image (fun x->Early_atom.EA x) [1;3] ;;
let ab = Early_union.union aa bb ;;

let faa = Molecule.of_early_union_image aa ;;
let fbb = Molecule.of_early_union_image bb ;;
let fab = Molecule.of_early_union_image ab ;;

let veiled_tab = Molecule.setminus faa (Molecule.union (Molecule.of_early_union ab) fab) ;;
let tab = Molecule.unveil veiled_tab ;;
let check_tab  = i_setminus tab (i_fold_merge(Image.image (fun (a,b,m)->m) z2));;

let z3 = Image.image (fun (a,b,m)->(a,b,i_intersection m tab)) z2 ;;
let get (a0,b0) = Option.get(List.find_map (fun (a,b,m)->if (a,b)=(a0,b0) then Some(m) else None) z3) ;;
let part1 = (get ([4],[3;4]));;
let tab2 = i_setminus tab part1 ;;

let z4 = z3 ;;
let v1 = List.tl(il_sort(Image.image (fun (a,b,m)->m) z4)) ;;
let v2 = List.rev v1 ;;

let v3 = List.hd v2 ;;
let v4 = List.filter_map (fun (a,b,m)->if m=v3 then Some(a,b) else None) z4 ;;
(*

let veiled_tab = Molecule.setminus fab (Molecule.union faa fbb) ;;
let tab = Molecule.unveil veiled_tab ;;

let check_tab  = i_setminus tab (i_fold_merge(Image.image (fun (a,b,m)->m) z2));;

let z3 = Image.image (fun (a,b,m)->(a,b,i_intersection m tab)) z2 ;;
let get (a0,b0) = Option.unpack(Option.find_and_stop (fun (a,b,m)->if (a,b)=(a0,b0) then Some(m) else None) z3) ;;
let part1 = i_merge (get ([1;2],[1;2;3])) (get ([1;3],[1;2;3]));;
let tab2 = i_setminus tab part1 ;;

let z4 = Image.image (fun (a,b,m)->(a,b,i_intersection m tab2)) z3 ;;

let v1 = List.tl(il_sort(Image.image (fun (a,b,m)->m) z4)) ;;
let v2 = List.rev v1 ;;

let v3 = List.hd v2 ;;
let v4 = Option.filter_and_unpack (fun (a,b,m)->if m=v3 then Some(a,b) else None) z4 ;;
*)


(************************************************************************************************************************
Snippet 59 : Second stab at boundary operator combinatorics 
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let il_order = Total_ordering.silex_compare i_order ;;
let j_order = Total_ordering.product i_order il_order ;;

let i_fold_merge = Ordered.fold_merge i_order ;;
let i_intersection = Ordered.intersect i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_setminus = Ordered.setminus i_order ;;
let i_sort = Ordered.sort i_order ;;

let il_sort = Ordered.sort il_order ;;
let il_setminus = Ordered.setminus il_order ;;


let j_merge = Ordered.merge j_order ;;

let n1 = 4 ;;
let whole = Int_range.range 1 n1 ;;
let u1 = il_sort (List_again.power_set whole) ;;
let rtl l = List.rev (List.tl l);;
let u2 = rtl (rtl u1);;
let normal_form x = 
     let cx = i_setminus whole x in 
     if (il_order x cx)=Total_ordering_result_t.Lower 
     then x else cx ;;
let u3 = List.filter (fun x->normal_form(x)=x) u2;;      
let u4 = Int_range.index_everything u3 ;;
let u5 = Image.image (fun (j,z)->(z,[j+4])) u4 ;;
let u6 = Image.image (fun x->let nx=normal_form x in (x,(x,List.assoc nx u5))) u2;;
let u7 = Uple.list_of_pairs u2 ;;
let u8 = List.filter (fun (a,b)->(i_intersection a b)<>[]) u7;;
let u9 = Int_range.index_everything u8 ;;

module Kafka = struct

type t = {
  size : int ; 
  atoms : int list ;
  decompositions : (int * (int list)) list ;
  images : ((int list) * ((int list) * (int list))) list ;
} ;;


let share_with_foreigner kfk x = 
   let n = kfk.size 
   and m = List.length(kfk.atoms) in
   let temp1 = Int_range.index_everything kfk.atoms in  
   let decs1 = Image.image (fun (j,atm)->(atm,[n+j;n+j+m])) temp1
   and expansion_for_x = Int_range.range (n+1) (n+m) in 
   let replacer = (fun z->
      if z=x then expansion_for_x else 
      try List.assoc z decs1 with _ -> [z]
    ) in 
   let replacer2 = (fun lz->
    i_fold_merge (Image.image replacer lz)
    ) in  
   let decs2 = Image.image (fun (w,old_decomposition)->
     (w,replacer2 old_decomposition) 
    ) kfk.decompositions in  
   let new_atoms = Int_range.range (n+1) (n+2*m)
   and new_decompositions = j_merge [x,expansion_for_x] (j_merge decs1 decs2) 
   and new_images = Image.image (
     fun (old_a,(a,b)) -> (old_a,(replacer2 a,replacer2 b))
   ) kfk.images in 
   {
    kfk with 
    atoms = new_atoms ;
    decompositions = new_decompositions ;
    images = new_images ;
  } ;; 

let start =   
  {
    size = 100 ;
    atoms = [1; 2; 3; 4] ;
    decompositions = [] ;
    images = u6 ;
  } ;; 

  let expand kfk l = 
    let replacer = (fun z->
      match List.assoc_opt z kfk.decompositions with 
      Some(old_answer) -> old_answer
      | None -> [z]
    ) in 
    i_fold_merge (Image.image replacer l) ;;


  let get_image kfk l = try snd(List.assoc l kfk.images) with _ -> [] ;;

  let is_fully_decomposed kfk l = i_is_included_in l kfk.atoms ;; 

  let nondecomposed_elements kfk l = i_setminus l kfk.atoms ;; 

let declare_empty kfk zeroes =
  let remaining_atoms = i_setminus kfk.atoms zeroes in 
  let m = List.length remaining_atoms 
  and n = kfk.size in 
  let interval = Int_range.range (n+1) (n+m) in 
  let table = List.combine remaining_atoms interval in 
  let cleanup = (fun x->Image.image (fun t->
      try List.assoc t table with _ ->t) (i_setminus x zeroes)) in   
  let new_decompositions = Image.image (fun (x,dx)->(x,cleanup dx)) kfk.decompositions 
  and new_images = Image.image (
    fun (old_a,(a,b)) -> (old_a,(cleanup a,cleanup b))
  ) kfk.images in 
  {
   kfk with 
   atoms = interval ;
   decompositions = new_decompositions ;
   images = new_images ;
 } ;; 
 

end ;;


module Haddock = struct 

type t = 
   S1 of int * (int list) * (int list) * (int list) * (int list) * (int list) * (int list)  
 | S2 of int * (int list) * (int list) * (int list) * (int list) * (int list) * (int list) 
 | S3 of int * (int list) * (int list) * (int list) * (int list) * (int list) * (int list) 
 | S4 of int * (int list) * (int list) * (int list) * (int list) * (int list) * (int list) * (int list) * (int list)  

;;  

type road =
   Decompose of (int list) * (int list)
  |Declare_empty of (int list) * (int list) ;;

let is_unfinished = function 
 S4(idx,a,b,anb,g_anb,g_a,g_b,d1,d2) -> (d1,d2) <> ([],[])
|_ -> true ;;

let road_to_go  kfk haddock = match haddock with 
  S1(idx,a,b,anb,f_anb,f_a,f_b) ->
       Decompose(
          (Kafka.nondecomposed_elements kfk anb),  
          (Kafka.nondecomposed_elements kfk f_anb)) 
  |S2(idx,a,b,anb,g_anb,f_a,f_b) -> 
    Decompose(Kafka.nondecomposed_elements kfk f_a,[]) 
  |S3(idx,a,b,anb,g_anb,g_a,f_b) -> 
    Decompose(Kafka.nondecomposed_elements kfk f_b,[]) 
  |S4(idx,a,b,anb,g_anb,g_a,g_b,d1,d2) -> Declare_empty(d1,d2) ;;

let improve_a_little_bit  kfk haddock = match haddock with 
  S1(idx,a,b,anb,f_anb,f_a,f_b) ->
       if (Kafka.is_fully_decomposed kfk anb) &&  (Kafka.is_fully_decomposed kfk f_anb) 
       then S2(idx,a,b,anb,i_intersection anb f_anb,f_a,f_b) 
       else haddock
  |S2(idx,a,b,anb,g_anb,f_a,f_b) -> 
    if (Kafka.is_fully_decomposed kfk f_a) 
      then S3(idx,a,b,anb,g_anb,i_intersection anb f_a,f_b) 
      else haddock
  |S3(idx,a,b,anb,g_anb,g_a,f_b) -> 
        if (Kafka.is_fully_decomposed kfk f_b) 
          then let g_b = i_intersection anb f_b in 
               let g_ab = i_merge g_a g_b in 
               S4(idx,a,b,anb,g_anb,g_a,g_b,i_setminus g_anb g_ab,i_setminus g_ab g_anb) 
          else haddock    
  |S4(idx,a,b,anb,g_anb,g_a,g_b,d1,d2) -> haddock ;;

let improve kfk haddock = 
    let rec tempf = (fun old_x ->
       let new_x=improve_a_little_bit  kfk old_x in 
       if new_x = old_x then old_x else tempf new_x
    ) in 
    tempf haddock ;;

let initial_individual kfk (idx,(a,b)) = 
  let c =  i_intersection a b in 
  S1(idx,a,b,Kafka.expand kfk c,Kafka.get_image kfk c,Kafka.get_image kfk a,Kafka.get_image kfk b) ;;
  
let individual kfk (a,b) = 
   let candidate = improve kfk (initial_individual kfk (a,b)) in 
   if is_unfinished candidate 
   then Some (candidate,road_to_go kfk candidate)
   else None ;;

let total kfk = 
   List.find_map (individual kfk) u9 ;;


end ;;  

module This_kafka = struct 

  let main_ref = ref Kafka.start ;;

  let share x = 
     let new_kafka = Kafka.share_with_foreigner (!main_ref) x in 
     let _ = (main_ref:=new_kafka) in 
     new_kafka ;;

  let atoms () = (!main_ref).Kafka.atoms ;;

  let expand  = Kafka.expand (!main_ref) ;;

  let final_haddock () = Haddock.total (!main_ref) ;;

  let declare_empty zeroes = 
    let new_kafka = Kafka.declare_empty (!main_ref) zeroes in 
    let _ = (main_ref:=new_kafka) in 
    new_kafka ;;

  let act = function 
    Haddock.Decompose(d1,d2) -> let d = List.hd (d1@d2) in share d 
    |Declare_empty(e1,e2) -> declare_empty (i_merge e1 e2) ;;

end ;;  

module That_kafka = struct 

let share x = let _ = This_kafka.share x in snd(Option.get(This_kafka.final_haddock ())) ;;
let declare_empty x = let _ = This_kafka.declare_empty x in snd(Option.get(This_kafka.final_haddock ())) ;;

let act road = let _ = This_kafka.act road in  This_kafka.final_haddock () ;;

end ;;  



let rec iterator preceding_moves = match preceding_moves with 
   [] -> failwith("pusher_exn") 
  |(preceding_move,_) :: _->
      (match That_kafka.act preceding_move with 
      None -> List.rev preceding_moves 
    |Some(candidate,road) -> 
      iterator((road,!(This_kafka.main_ref))::preceding_moves) );;
  
let z1 = [(Haddock.Decompose([5],[])),Kafka.start]  ;;
let z2 = iterator z1 ;;   
let (z3,kfk1) = List.hd(List.rev z2) ;;   
let kfk1 = (!(This_kafka.main_ref))
let im = Kafka.get_image kfk1 ;;

let v1 = Uple.list_of_pairs u2 ;;
let v2 = List.filter (
   fun (a,b) ->
     let c = i_merge a b in 
     let d = i_setminus (im c) (i_merge (im a) (im b)) in 
     d<>[]
) v1 ;;



(************************************************************************************************************************
Snippet 58 : First stab at boundary operator combinatorics 
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let il_order = Total_ordering.silex_compare i_order ;;

let i_merge = Ordered.merge i_order ;;
let i_intersection = Ordered.intersect i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;


let il_sort = Ordered.sort il_order ;;


let u1 = il_sort (List_again.power_set [1;2;3]) ;;
let u2 = Uple.list_of_pairs u1 ;;
let u3 = List.filter (fun (x,y)->i_intersection x y <> []) u2 ;;

let table_for_ff = 
  [[],0; [1],1; [2],2; [3],3; [1; 2],3; [1; 3],2; [2; 3],1; [1; 2; 3],0] ;;
let ff x = List.assoc x table_for_ff  ;;

let u4 = Image.image (fun (a,b)->
  let c= i_intersection a b in 
  (a,b,(c,ff c,ff a, ff b))) u3;;

let tf k = List.nth u4 (k-1) ;;  

let v1 = Cartesian.cube u1 ;;
let haddock k a b c = ((i_intersection k a) = (i_intersection k (i_merge b c)));;
let main_test (x1,x2,x3) =
    (haddock [1] x1 x2 x3) && (haddock [2] x2 x3 x1) && (haddock [3] x3 x1 x2) ;;      
let v2 = List.filter main_test v1 ;;
let v3 = List.filter (fun (x1,x2,x3) -> not(i_is_included_in x3 (i_merge x1 x2))) v2;;



(************************************************************************************************************************
Snippet 57 : Preprocess some PARI/GP code
************************************************************************************************************************)
open Needed_values ;;

let n1 = 5 ;;
let m1 = ((n1-1) * (n1-2)) / 2;;
let u1 = Int_range.scale (fun x->[1;0]) 1 n1 ;;
let u2 = Cartesian.general_product u1 ;;
let u3 = Int_range.index_everything u2 ;;
let ts l= 
   String.concat "+" (Image.image (fun j->"t"^(string_of_int j)) l);;

let s1 i = (ts(List.filter_map (fun (idx,l)->
    if (List.nth l (i-1) = 1) 
    then Some(idx)
    else None    
  ) u3)) ^ "-" ^(string_of_int m1);;
let s2 (i,j) = (ts(List.filter_map (fun (idx,l)->
    if (List.nth l (i-1) = 1) && (List.nth l (j-1) = 1)
    then Some(idx)
    else None    
) u3)) ^ "-" ^(string_of_int (n1-2));;

let part1 = Int_range.scale s1 1 n1 ;;
let part2 = Image.image s2 (Int_uple.list_of_pairs n1);;
let whole = "\n\n\n[" ^ (String.concat "," (part1@part2)) ^ "]\n\n\n"  ;;
let pw () = print_string whole ;;

Ordered.setminus Total_ordering.for_integers (Int_range.range 1 32)
[1; 2; 3; 4; 5; 6; 7; 9; 10; 11; 13; 17; 18; 19; 21; 25; 32] ;;


(************************************************************************************************************************
Snippet 56 : Transform a text in an Ocaml string 
************************************************************************************************************************)
let z1 = Needed_values.rf "Fads/nap.ml"  ;;
let z2 = Lines_in_text.interval z1 12 25 ;;
let z3 = Replace_inside.replace_inside_text ("\"","\\\"") z2;;
let z4 = Lines_in_text.lines z3 ;;
let z5 = Image.image (fun line -> "\"" ^ (Cull_string.trim_spaces line) ^ "\"") z4 ;; 
let z6 = "\n\n\n" ^ (String.concat ";\n" z5) ^ "\n\n\n" ;;
let z7 () = print_string z6 ;;

(************************************************************************************************************************
Snippet 55 : Find and replace on several files 
************************************************************************************************************************)
let reps_ref = ref [ ];;

reps_ref := [
  "Fw_with_githubbing.all_endinglesses","Fw_with_dependencies.all_endinglesses";
  "Fw_with_githubbing.all_ml_absolute_paths","Fw_with_dependencies.all_ml_absolute_paths";
  "Fw_with_githubbing.all_mlx_files","Fw_with_dependencies.all_mlx_files";
  "Fw_with_githubbing.ancestors_for_module","Fw_with_dependencies.ancestors_for_module";
  "Fw_with_githubbing.all_subdirectories","Fw_with_dependencies.all_subdirectories";
  "Fw_with_githubbing.below","Fw_with_dependencies.below";
  "Fw_with_githubbing.check_that_no_change_has_occurred","Fw_with_archives.check_that_no_change_has_occurred";
  "Fw_with_githubbing.configuration","Fw_with_githubbing.to_fw_configuration";
  "Fw_with_githubbing.decipher_module","Fw_with_dependencies.decipher_module";
  "Fw_with_githubbing.decipher_path","Fw_with_dependencies.decipher_path";
  "Fw_with_githubbing.dep_ordered_modules","Fw_with_dependencies.dep_ordered_modules";
  "Fw_with_githubbing.direct_fathers_for_module","Fw_with_dependencies.direct_fathers_for_module";
  "Fw_with_githubbing.directly_below","Fw_with_dependencies.directly_below";
  "Fw_with_githubbing.duplicate_module","Fw_with_dependencies.duplicate_module";
  "Fw_with_githubbing.endingless_at_module","Fw_with_dependencies.endingless_at_module";
  "Fw_with_githubbing.find_subdir_from_suffix","Fw_with_dependencies.find_subdir_from_suffix";
  "Fw_with_githubbing.gitpush_after_backup","Fw_poly.gitpush_after_backup";
  "Fw_with_githubbing.latest_changes","Fw_with_archives.latest_changes";
  "Fw_with_githubbing.modules_using_value","Fw_with_dependencies.modules_using_value";
  "Fw_with_githubbing.noncompilable_files","Fw_with_archives.noncompilable_files";
  "Fw_with_githubbing.of_concrete_object","Fw_poly.of_concrete_object";
  "Fw_with_githubbing.root","Fw_poly.root";
  "Fw_with_githubbing.set_gitpush_after_backup","Fw_poly.set_gitpush_after_backup";
  "Fw_with_githubbing.to_concrete_object","Fw_poly.to_concrete_object";
  "Fw_with_githubbing.usual_compilable_files","Fw_with_archives.usual_compilable_files";
] ;;

let files_ref = ref [] ;;

files_ref := [
  "Compilation_management/create_world_copy.ml";
  "Compilation_management/modify_coma_state.ml";
  "Compilation_management/needed_data_summary.ml";  
  "Compilation_management/usual_coma_state.ml";
  "Compilation_management/other_coma_state.ml";
  "Filewatching/fw_with_persisting.ml";
  "Ocaml_analysis/compute_all_ocaml_items.ml";
  "Ocaml_analysis/read_needed_ocaml_files.ml";
  "self_contained_module_copy.ml";
] ;;

let tr x = Replace_inside.replace_several_inside_file (!reps_ref) (Absolute_path.of_string( "../Idaho/"^x)) ;;

let act () = List.iter tr (!files_ref) ;;

(************************************************************************************************************************
Snippet 54 : Modifying line intervals in a file
************************************************************************************************************************)
let ap1 = Absolute_path.of_string "../Idaho/Filewatching/fw_with_githubbing.ml" ;;
let text1 = Io.read_whole_file ap1 ;;
let (before1,old_center1,after1) = Lines_in_text.tripartition_associated_to_interval 
    text1 257 336 ;;
let new_center1 = Lines_in_text.remove_lines_containing_substring_in_text
  "shrinkable" old_center1 ;;
let new_text1 = String.concat "\n" [before1;new_center1;after1] ;;  
Io.overwrite_with ap1 new_text1 ;;

Lines_in_text.remove_interval_in_file ap1 169 253 ;;

(************************************************************************************************************************
Snippet 53 : Enumerating subgroups of S4 
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let i_sort  = Ordered.sort i_order ;;
let i_is_included_in = Ordered.is_included_in i_order;;

let il_order = Total_ordering.silex_compare  Total_ordering.for_integers ;;
let il_sort  = Ordered.sort il_order ;;

let current_order = 4 ;;
let base = Permutation.iii current_order ;;

let eval_list_permutation sigma k = List.nth sigma (k-1) ;;

let compose_list_permutations sigma1 sigma2 = 
   Int_range.scale (fun k-> eval_list_permutation sigma1 (eval_list_permutation sigma2 k)) 1 current_order ;;

let uncurried_compose = Memoized.make(fun (i,j) ->
   let sigma1 = List.nth base (i-1)   
   and sigma2 = List.nth base (j-1) in 
   List_again.find_index_of_in (compose_list_permutations sigma1 sigma2) base
);;     

let compose  i j = uncurried_compose (i,j) ;;

let base_size = List.length base ;;

let subset_product l1 l2 =
    let temp1 = Cartesian.product l1 l2 in 
    let temp2 = Image.image uncurried_compose temp1 in 
    i_sort temp2 ;; 

let rec helper_for_generated_subgroup (treated,seed) = 
      let possibly_new = subset_product treated seed in 
      let really_new = Ordered.setminus Total_ordering.for_integers possibly_new treated in 
      if really_new = [] 
      then treated 
      else let new_whole = Ordered.merge Total_ordering.for_integers really_new treated in 
            helper_for_generated_subgroup (new_whole,seed) ;;
        
let generated_subgroup seed = helper_for_generated_subgroup ([1],seed) ;; 

let trivial_subgroup = [1] ;;
let full_subgroup = Int_range.range 1 base_size ;;

let level1  = 
  il_sort (Int_range.scale (fun k->generated_subgroup [k]) 2 base_size) ;; 

let pre_level2 = 
    let temp1 = Uple.list_of_pairs level1 in 
    let temp2 = Image.image (fun (a,b)->generated_subgroup(a@b)) temp1 in 
    il_sort temp2 ;;

let (next_to_level2,level2) = List.partition (fun x->List.mem x level1) pre_level2 ;;

let pre_level3 = 
  let temp1 = Cartesian.product level1 level2 in 
  let temp2 = List.filter (fun (x,y)->not(i_is_included_in x y)) temp1 in 
  let temp3 = Image.image (fun (a,b)->generated_subgroup(a@b)) temp2 in 
  il_sort temp3 ;;

let (next_to_level3,level3) = List.partition (fun x->List.mem x level2) pre_level3 ;;  

let halves x =
   let n = (List.length x)/2 in 
   List.filter (fun y->((List.length y)=n)&&(i_is_included_in y x) ) (level1@level2) ;;

let is_transitive sg =
   let temp1 = Image.image (fun sigma -> 
    eval_list_permutation sigma 1
   ) sg in 
   (i_sort temp1) = (Int_range.range 1 current_order) ;;
   

let d4 = List.hd(List.filter (fun x->List.length x=8) level2) ;; 
let halves_for_d4 = halves d4 ;; 

let a4 = List.hd(List.filter (fun x->List.length x=12) level2) ;; 
let halves_for_a4 = halves a4 ;; 

let halves_for_whole = halves full_subgroup ;;

    

(************************************************************************************************************************
Snippet 52 : Finding a polynomial x^4+p*x+q with Galois group A4
************************************************************************************************************************)
let u1 = Int_range.range (-50) 50 ;;
let u2 = Cartesian.square u1 ;;
let u3 = Image.image (fun (x,y)->(max(abs x)(abs y),(x,y)) ) u2 ;;
let u4 = Ordered.sort Total_ordering.standard2 u3 ;;
let unchecked_u5 = Image.image snd u4 ;;
let u5 = List.filter (fun (p,q)->List.for_all (fun z->z*z*z*z+p*z+q<>0) 
(Int_range.range (-1) 1)) unchecked_u5 ;;

let round x=
  let fl = floor x in 
  if (x -. fl) < 0.5 
  then int_of_float fl 
  else (int_of_float fl)+1 ;;  

let is_a_square n = 
    if n< 0 then false else
    let m =round(sqrt(float_of_int n)) in m * m = n;;  

let check = List.filter is_a_square (Int_range.range 0 100) ;;    

let u6 = List.filter (fun (p,q)->is_a_square(-27*p*p*p*p + 256*q*q*q)) u5 ;;

(************************************************************************************************************************
Snippet 51 : Removing indentation in a paragraph in a file  
************************************************************************************************************************)
let ap1 = Absolute_path.of_string "Fads/pan.ml" ;;

let act1 () = Lines_in_text.shift_indentation_in_interval_in_file_with 
(41,45) (Absolute_path.of_string "watched/watched_not_githubbed/jug.ml")
 ~shift_amplitude:(-12) ~forced:false ;; 

 let act1 () = Lines_in_text.shift_indentation_in_interval_in_file_with 
(608,635) (Absolute_path.of_string "lib/Szemeredi/sz3_preliminaries.ml")
 ~shift_amplitude:(-8) ~forced:false ;; 


let act2 () = Lines_in_text.remove_interval_in_file 
 (Absolute_path.of_string "lib/Szemeredi/sz3_preliminaries.ml") 346 580 ;; 

(************************************************************************************************************************
Snippet 50 : Intertwining prints for debugging purposes
************************************************************************************************************************)
open Needed_values ;;

let z1 = rf "Fads/pan.ml" ;;
let z2 = Lines_in_text.interval z1 10 58 ;;
let z3 = Lines_in_text.lines z2 ;;
let z4 = List.filter (fun line -> Cull_string.trim_spaces line <> "" ) z3 ;; 
let z5 = Int_range.index_everything z4 ;; 
let z6 = Image.image (
  fun (j,line) ->
    let sj = string_of_int j in 
    line^"\nprint_int "^sj^" ;;"
) z5 ;;
let z7 = "\n\n\n" ^ (String.concat "\n" z6) ^ "\n\n\n" ;;  

(************************************************************************************************************************
Snippet 49 : Problem involving periodicity
************************************************************************************************************************)
let find_periodicity l= 
  let rl = List.rev l in 
  let (a1,after_a1) = List_again.head_with_tail rl in 
  let j = List_again.find_index_of_in a1 after_a1 in 
  let inverted_motif = List_again.long_head j rl in 
  let motif = List.rev inverted_motif in 
  let p = List.length motif in 
  let m0 = Min.list motif in 
  let i0 = List_again.find_index_of_in m0 motif in 
  let after_m0 = List_again.long_tail i0 motif 
  and before_m0 = List_again.long_head (i0-1) motif in
  (p,m0::(after_m0@before_m0)) ;; 


let current_r = 5 ;;
let current_m = Gcd.lcm_for_many (Int_range.range 2 current_r) ;;

let pusher old_f n = 
  let lower_bound = max 1 (n-current_r) in  
  let temp1 = Int_range.range lower_bound (n-1) in 
  let temp2 = Image.image (fun m->(old_f m)-(current_m/(n-m))) temp1 in 
  let first_trial = Min.list temp2 in 
  if first_trial > 0 then first_trial else 
  let temp3 = Image.image (fun m->(old_f m)+(current_m/(n-m))) temp1 in   
  Max.list temp3 ;;

let ff = Memoized.recursive (fun old_f n->if n<2 then 1 else pusher old_f n) ;;

let z1 = Int_range.scale ff 1 200 ;;
let (period,motif) = find_periodicity z1 ;;
let last_in_motif = List.nth motif (period-1) ;;
let gg n = let r = n mod period in if r = 0 then last_in_motif else List.nth motif (r-1) ;;
let dg t = Min.list (Int_range.scale (fun k->(abs(gg(k+t)-gg(k)))*t ) 1 period) ;;
let (max_dg,dg_sols) = Min.minimize_it_with_care dg (Int_range.range 1 current_r) ;;
let largest_in_motif = Max.list motif ;;
let ratio = (float_of_int(largest_in_motif-List.hd(motif))) /. (float_of_int max_dg);;


(************************************************************************************************************************
Snippet 48 : Musings on the Szemeredi problem, chapter V
************************************************************************************************************************)
let current_width = 3 ;; 
let max_width = Sz_max_width_t.MW current_width ;;
let is_admissible = Sz_preliminaries.test_for_admissibility max_width ;;
let is_not_admissible x= (not(is_admissible x));;
let uncurried_sl  = Memoized.make (fun (x,k)->
  let temp1 = Sz_preliminaries.restricted_power_set (max_width,x) in 
  List.filter (fun z->List.length z=k) temp1 
) ;;  
let sl x k = uncurried_sl (x,k) ;;
let isl n k = uncurried_sl (Int_range.range 1 n,k) ;; 
let meas = Sz_precomputed.measure max_width ;;


let i_does_not_intersect = Ordered.does_not_intersect Total_ordering.for_integers ;;
let i_is_included_in = Ordered.is_included_in Total_ordering.for_integers ;;
let i_merge = Ordered.merge Total_ordering.for_integers ;;
let i_outsert = Ordered.outsert Total_ordering.for_integers ;;
let i_fold_intersect = Ordered.fold_intersect Total_ordering.for_integers ;;
let il_fold_merge = Ordered.fold_merge Total_ordering.silex_for_intlists ;;
let il_mem = Ordered.mem Total_ordering.silex_for_intlists ;;
let il_merge = Ordered.merge Total_ordering.silex_for_intlists ;;
let il_sort = Ordered.safe_set Total_ordering.silex_for_intlists ;;



let original_minimal_carriers carriers sols =
  let indexed_carriers = Int_range.index_everything carriers in 
  let shadow = (
      fun sol ->
         List.filter_map (
          fun (idx,carrier) -> 
             if i_is_included_in carrier sol 
             then Some idx 
            else None 
        ) indexed_carriers 
  )  in     
  let all_shadows = Image.image shadow sols in 
  Ordered_misc.minimal_transversals all_shadows ;;
 
let u_product sheaf1 sheaf2 = 
    let temp1 = Cartesian.product sheaf1 sheaf2 in 
    let temp2 = Image.image (fun (x,y)->i_merge x y) temp1 in 
    let temp3 = Ordered_misc.minimal_elts_wrt_inclusion temp2 in 
    il_sort temp3 ;;

 
let fold_u_product = function 
  [] -> []
  | sheaf :: other_sheaves -> List.fold_left  u_product sheaf other_sheaves ;;   
   

exception Nonunique_set_of_minimal_carriers of int list list list ;;
 
let set_of_minimal_carriers carriers sols =
 let version1 = original_minimal_carriers carriers sols in 
 let m = List.length(List.hd version1) in 
 let version2 = List.filter (fun x->List.length(x)=m) version1 in 
 let visualize = Image.image (fun idx->List.nth carriers (idx-1)) in 
 let version3 = Image.image visualize version2 in 
 if (List.length version3)<>1
 then raise (Nonunique_set_of_minimal_carriers version3)
 else List.hd version3;;
 
let set_of_minimal_carriers_with_extra carriers sols =
 try (Some(set_of_minimal_carriers carriers sols),None) with 
 Nonunique_set_of_minimal_carriers(nonunique) -> (None, Some nonunique)   ;;

let remains_of_obstructions_in_positing_case x=
  List.filter_map (fun j->
      let k=(current_width+1)-j in 
      if x>2*k 
      then  Some [x-2*k;x-k]
      else None
  ) (Int_range.range 1 current_width) ;;

  

let analize_sheaf1 (left,bound,right) =
    let m = List.hd(List.rev right) in 
    let carriers = Sz_preliminaries.force_subset_in_interval    
      max_width right (1,m) in 
      set_of_minimal_carriers_with_extra carriers (sl left bound);;

let ref_for_missing_sheaves = ref [] ;;

exception Troublesome_aftersheaf of int list * int * int list * (int list list list);;
exception Missing_sheaves of (int list * int *  (int list list)) list ;;
  
let analize_sheaf2 (left,bound,right) =
  let (good_opt,bad_opt) = analize_sheaf1(left,bound,right) in 
  (
    match good_opt with 
     None -> raise(Troublesome_aftersheaf(left,bound,right,Option.get bad_opt)) 
    |Some usual -> 
      let _ = (ref_for_missing_sheaves:=[left,bound,usual]) in
      raise(Missing_sheaves [left,bound,usual])
  ) ;;    

let carrier_is_stronger_than_another carrier1 carrier2 =
   List.for_all (fun c1 -> 
    List.exists (fun c2 -> i_is_included_in c2 c1) carrier2) carrier1 ;;

exception Two_carriers_exn of 
  (int list) * int * (int list list) * (int list list) * (int list list list);;

let add_carrier_to_another (x,bound) carrier old_carrier =
   if carrier_is_stronger_than_another carrier old_carrier 
   then [carrier]
   else 
   let carrier2 = u_product carrier old_carrier in 
   let (good_opt,bad_opt) = set_of_minimal_carriers_with_extra carrier2 (sl x bound) in 
   if good_opt<>None
   then [Option.get good_opt]
   else raise (Two_carriers_exn(x,bound,carrier,old_carrier,Option.get bad_opt));; 

exception Add_carrier_exn of (int list) * int * (int list list) * (int list list list) ;;

let add_carrier (x,bound) carrier old_list = 
   match List.length old_list with 
    0 -> [carrier] 
   | 1 -> add_carrier_to_another (x,bound) carrier (List.hd old_list) 
   | _ -> raise (Add_carrier_exn(x,bound,carrier,old_list));;


let hashtbl_for_sheaves = ((Hashtbl.create 100): 
   ((int list) * int, int list list list) Hashtbl.t) ;;


let add_sheaf (x,bound) carrier =
  let new_val = (
   match Hashtbl.find_opt hashtbl_for_sheaves (x,bound)  with 
    Some (old_val) -> add_carrier (x,bound) carrier  old_val
   | None -> [carrier]
   ) in 
   Hashtbl.replace hashtbl_for_sheaves (x,bound) new_val ;;
   
let consult_sheaves (left,bound,right) =
  match Hashtbl.find_opt hashtbl_for_sheaves (left,bound)  with 
     None -> None
   | Some (sheaves) -> 
     List.find_opt (fun sheaf->
       List.for_all (fun z->is_not_admissible (z@right)) sheaf
      ) sheaves
  ;;        

let sheaf_compare new_ll old_ll  =
    List.for_all (fun old_l-> List.exists 
    (fun new_l->i_is_included_in new_l old_l) new_ll ) old_ll  ;;

let sheaf_compare2 new_ll old_lls =
    List.exists (sheaf_compare new_ll) old_lls ;;
       
let sheaf_is_not_already_known_by_sheaves_hashtbl (left,bound,right) = 
  match Hashtbl.find_opt hashtbl_for_sheaves (left,bound)  with 
  None -> true
| Some (old_sheaves)  -> not(sheaf_compare2 right old_sheaves)  ;;       

let hashtbl_for_pre_measure = ((Hashtbl.create 100) :
(int list ,int list) Hashtbl.t) ;;

let sheaf_is_not_already_known_by_measures_hashtbl (left,bound,right) = 
  match Hashtbl.find_opt hashtbl_for_pre_measure left  with 
  None -> if is_admissible left 
          then (List.length left) >= bound
          else true
| Some (old_sol)  -> (List.length old_sol) >= bound  ;;   

let sheaf_is_not_already_known triple = 
   if sheaf_is_not_already_known_by_measures_hashtbl triple 
   then  sheaf_is_not_already_known_by_sheaves_hashtbl triple 
   else false ;; 

exception Borderline_case of ((int list) * int * ((int list) list)) list ;;

let commonest_decomposition (x,bound,carriers) =
    let (m,ry) = List_again.head_with_tail(List.rev x) in 
    let y = List.rev ry in 
   let rem_obstr1 = remains_of_obstructions_in_positing_case m 
   and (pre_rem_obstr3,rem_obstr2) = List.partition (fun z->List.mem m z) carriers in    
   let rem_obstr3 = Image.image (i_outsert m) pre_rem_obstr3 in       
   let temp = (if List.mem [m] carriers then [] else 
    [
     (y,bound-1,il_fold_merge [rem_obstr1;rem_obstr2;rem_obstr3]);
    ])
   @ 
    [(y,bound,rem_obstr2)] in 
  let (cleaned_temp,dirty_temp) = List.partition sheaf_is_not_already_known_by_measures_hashtbl temp in    
  let (temp_good,temp_bad) = List.partition (fun (_,_,obstr2)->obstr2<>[]) cleaned_temp in   
  if temp_bad <> [] 
  then raise(Borderline_case(temp_bad)) 
  else    
  let temp3 = Image.image (
    fun tr3 ->
       let (y3,bound3,obstr3) = tr3 in 
       (tr3,set_of_minimal_carriers_with_extra obstr3 (sl y3 bound3))
  ) temp_good in 
  let (temp3_good,temp3_bad) = List.partition (fun 
    (_,(good_opt,bad_opt)) -> good_opt <> None
  ) temp3 in 
  (
    Image.image (fun ((y,bound,_),(good_opt,bad_opt))->(y,bound,Option.get good_opt) ) temp3_good,
    Image.image (fun (tr,(good_opt,bad_opt))->tr ) temp3_bad,
    dirty_temp
  );;   



let enhanced_commonest_decomposition triple =
  let (temp1,temp2,temp3) = commonest_decomposition triple in 
  (List.filter sheaf_is_not_already_known temp1, 
   List.filter sheaf_is_not_already_known temp2,
   temp3);;   

exception Add_sheaf_carefully_exn of (int list * int * int list list) list ;;

let add_sheaf_carefully (x,bound) carrier =
  let (temp1,temp2,_) = enhanced_commonest_decomposition (x,bound,carrier) in 
  let temp3 = temp1 @ temp2 in 
  if temp3 <> []
  then raise(Add_sheaf_carefully_exn(temp3))     
  else add_sheaf (x,bound) carrier ;;

let consult_sheaves_and_double_check (left,bound,right) =
  match consult_sheaves (left,bound,right) with 
    Some obstr -> Some obstr 
   | None ->
     let sols = sl left bound in 
     if List.for_all (fun z->is_not_admissible (z@right)) sols
     then analize_sheaf2(left,bound,right) 
     else None ;;
     


let analize_missing_sheaves ()=
  let triple = List.hd(!ref_for_missing_sheaves) in 
  let (temp1,temp2,temp3) = enhanced_commonest_decomposition triple in 
  let _= (ref_for_missing_sheaves:=temp1@temp2) in 
  (temp1,temp2,temp3);;

let default_string_of_intlist l=  
  "["^(String.concat ";" 
  (Image.image string_of_int l)
  )^"]";;  

  let ennified_string_of_intlist l=
    let a = List.hd(l) and b = List.hd(List.rev l) in 
    if (l=Int_range.range a b)
    then  "Ennig.ennig "^(string_of_int a)^" "^(string_of_int b)    
  else default_string_of_intlist l;;

let string_of_intlistlist ll=  
"["^(String.concat ";" 
(Image.image default_string_of_intlist ll)
)^"]";;

let message_for_newcomer (x,bound,carriers) =
 "asc ("^(ennified_string_of_intlist x)^","^(string_of_int bound)^") "^
  (string_of_intlistlist carriers)^" ;;" 
 ;;

let message_for_newcomers l= 
  "\n\n\n"^(String.concat "\n" 
  (Image.image message_for_newcomer l)
  )^"\n\n\n";;

let analize_repeatedly initial_triple =
   let triple1 = enhanced_commonest_decomposition initial_triple in  
   let (bt1,bt2,_) = triple1 in 
   if (bt1,bt2) = ([],[]) 
   then let msg = "\n\n\n"^(message_for_newcomer initial_triple)^"\n\n\n" in 
        let _= (print_string msg;flush stdout) in 
        (initial_triple,triple1)
   else  
    let rec tempf = (fun (t1,t2,t3)->
      let (nt1,nt2,nt3) = enhanced_commonest_decomposition (List.hd(t1@t2)) in 
      if (nt1,nt2) = ([],[])
      then let msg = message_for_newcomers (t1@t2) in 
           let _= (print_string msg;flush stdout) in 
           (initial_triple,(t1,t2,t3))  
      else tempf(nt1,nt2,nt3)
    ) in 
    tempf triple1 ;;  
    

let hashtbl_for_solving = ((Hashtbl.create 100) :
(int list * int * int list, int list) Hashtbl.t) ;;



let solve_in_easy_case (left,bound,right) = 
  let p = List.length left in 
  if p < bound 
  then Some None 
  else 
  if p = bound 
  then let unique = left @ right in 
       if is_admissible unique 
       then Some (Some unique)
       else Some None  
  else None ;;

let quick_way_out1 (left,bound,right) =
    if bound<>List.length(left) then false else 
    not(is_admissible(left@right)) ;;  

let pre_measure_arg whole              = (false,whole,0,[]) ;;
let solve_arg   (left,bound,right) = (true,left,bound,right) ;;
let pre_measure_ret m = (m,None) ;;
let solve_ret opt= ([],opt) ;;


let induction_in_solve_case old_f triple = 
  match solve_in_easy_case triple with 
   Some easy_answer -> solve_ret easy_answer 
   | None ->
  (
   match Hashtbl.find_opt hashtbl_for_solving triple with 
   Some old_answer -> solve_ret (Some old_answer) 
   | None ->
    let old_solve = (fun tr -> snd (old_f(solve_arg(tr)))) in 
    let opt_sol = (
      let (left,bound,right) = triple in  
      let (m,ry) = List_again.head_with_tail(List.rev left) in 
      let y = List.rev ry in 
      if is_not_admissible (m::right)
      then old_solve(y,bound,right)
      else     
      let temp1 = [(y,bound-1,m::right);(y,bound,right)] in 
      let my = List.length(fst (old_f (pre_measure_arg(y)))) in 
      let temp2 = List.filter (fun (y2,bound2,_)->bound2<=my) temp1 in 
      let temp3 = List.filter (fun tr->not(quick_way_out1 tr)) temp2 in 
      let temp4 = List.filter (
          fun tr -> (consult_sheaves_and_double_check tr) = None
      )  temp3 in  
      let temp5 = List.filter_map old_solve temp4 in 
      if temp5 = []
      then None  
      else Some(List.hd(List.rev temp5))
    ) in 
    let _ = (if opt_sol <>None 
      then Hashtbl.add hashtbl_for_solving triple (Option.get opt_sol)) in 
    solve_ret opt_sol
  );;

let induction_in_pre_measure_case old_f whole = 
   if is_admissible whole 
   then pre_measure_ret(whole) 
   else 
   match Hashtbl.find_opt hashtbl_for_pre_measure whole with 
    Some old_answer -> pre_measure_ret(old_answer) 
   |None -> 
   let new_answer = (
    let (m,ry) = List_again.head_with_tail(List.rev whole) in 
   let y = List.rev ry in  
   let sy = fst(old_f(false,y,0,[])) in 
   if is_admissible(sy@[m])
   then  pre_measure_ret(sy@[m])
   else   
   (
     match snd(old_f(true,y,List.length sy,[m])) with 
     None -> pre_measure_ret(sy)
     |Some fitting_one -> pre_measure_ret(fitting_one@[m])
   )) in 
  let _ = Hashtbl.add hashtbl_for_pre_measure whole (fst new_answer) in 
  new_answer;;
   

let rec main_iterator (case,left,bound,right) =
  if case 
  then induction_in_solve_case main_iterator (left,bound,right) 
  else induction_in_pre_measure_case main_iterator left ;;  


let pre_measure whole            = fst(main_iterator(pre_measure_arg(whole))) ;; 
let solve (left,bound,right) = snd(main_iterator(solve_arg(left,bound,right))) ;;  

let clear_hashtables () = (
    Hashtbl.clear hashtbl_for_sheaves  ;
    Hashtbl.clear hashtbl_for_solving  ;
    Hashtbl.clear hashtbl_for_pre_measure  ;
) ;;

exception FF_exn of (( int list * int * int list list) *
((int list * int * int list list) list *
(int list * int * int list list) list *
(int list * int * int list list) list)) ;;

let ff n = 
  try pre_measure (Int_range.range 1 n) with 
  Missing_sheaves(l) ->
    let (t1,t2,t3) = List.hd l in 
    let (a,b) = analize_repeatedly(t1,t2,t3) in 
    raise(FF_exn(a,b)) ;;


let ams = analize_missing_sheaves;;
let asc = add_sheaf_carefully ;;

let comp1 = Int_range.scale ff 1 5 ;;

asc (Int_range.range 1 4,3) [[4]] ;; 
ff 6;;
asc (Int_range.range 1 1,1) [[1]] ;; 
asc (Int_range.range 1 2,1) [[1];[2]] ;; 
asc (Int_range.range 1 2,2) [[1]] ;; 
asc (Int_range.range 1 3,2) [[1];[2;3]] ;;
asc (Int_range.range 1 4,3) [[1;4]] ;; 
asc (Int_range.range 1 5,3) [[5];[1;4]] ;;
asc (Int_range.range 1 5,4) [[1;4]] ;;
ff 7;;
asc (Int_range.range 1 2,2) [[2]] ;; 
asc (Int_range.range 1 3,1) [[1];[2];[3]] ;;
asc (Int_range.range 1 3,2) [[2];[1;3]] ;;
asc (Int_range.range 1 4,2) [[2];[1;3];[1;4];[3;4]] ;;
asc (Int_range.range 1 4,3) [[2];[1;3];[3;4]] ;;
asc (Int_range.range 1 5,3) [[1;4];[2;5]] ;;
asc (Int_range.range 1 5,4) [[2;5]] ;;
asc (Int_range.range 1 6,3) [[6];[1;4];[2;5]] ;;
asc (Int_range.range 1 6,4) [[2;5];[4;6]] ;;
Int_range.scale ff 8 10;;
asc (Int_range.range 1 9,5) [[9]] ;; 
Int_range.scale ff 11 13;;
asc (Int_range.range 1 12,7) [[12]] ;; 
ff 14;;
asc (Int_range.range 1 10,5) [[9];[10]] ;;
asc (Int_range.range 1 10,6) [[9]] ;;
asc (Int_range.range 1 11,6) [[9];[10;11]] ;;
asc (Int_range.range 1 12,7) [[9;12]] ;;
asc (Int_range.range 1 13,7) [[13];[9;12]] ;;
asc (Int_range.range 1 13,8) [[9;12]] ;;
ff 15;;
asc (Int_range.range 1 10,6) [[10]] ;;
asc (Int_range.range 1 11,5) [[9];[10];[11]] ;;
asc (Int_range.range 1 11,6) [[10];[9;11]] ;;
asc (Int_range.range 1 12,6) [[10];[9;11];[9;12];[11;12]] ;;
asc (Int_range.range 1 12,7) [[10];[7;10];[9;11];[11;12]] ;;
asc (Int_range.range 1 13,7) [[9;12];[10;13]] ;;
asc (Int_range.range 1 13,8) [[10;13]] ;;
asc (Int_range.range 1 14,7) [[14];[9;12];[10;13]] ;;
asc (Int_range.range 1 14,8) [[10;13];[12;14]] ;;
Int_range.scale ff 16 18;;
asc (Int_range.range 1 17,9) [[17]] ;;
Int_range.scale ff 19 21;;
asc (Int_range.range 1 20,11) [[20]] ;;

(*


let l =  [[1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13], 7, [[13]; [9; 12]]] ;;
let (bt1,bt2,bt3) = List.hd l ;;
let (a,b) = analize_repeatedly(bt1,bt2,bt3) ;;
let triple0 = enhanced_commonest_decomposition (bt1,bt2,bt3) ;;    

let analize_repeatedly (x,bound,carriers) =
   let triple0 = enhanced_commonest_decomposition (x,bound,carriers) in  
    let rec tempf = (fun (t1,t2,t3)->
      let (nt1,nt2,nt3) = enhanced_commonest_decomposition (List.hd(t1@t2)) in 
      if (nt1,nt2) = ([],[])
      then let msg = message_for_newcomers (t1@t2) in 
           let _= (print_string msg;flush stdout) in 
           (triple0,(t1,t2,t3))  
      else tempf(nt1,nt2,nt3)
    ) in 
    tempf triple0 ;;  


asc (Ennig.ennig 1 10,5) [[9];[10]] ;; 
asc (Ennig.ennig 1 5,4) [[2;5]] ;;

let g1 = Hashtbl.find hashtbl_for_sheaves (Ennig.ennig 1 4,3) ;;
asc (Ennig.ennig 1 4, 3)  [[2]; [1; 3]; [3; 4]] ;;

*)

(************************************************************************************************************************
Snippet 47 : Musings on the 1234 problem
************************************************************************************************************************)
open Needed_values ;;

type sensitive_t = {
   unsorted : (int * (int * int) list) list;
   sorted : int list ;
}  ;;  

type increase_t = 
    Inertia of int 
   |Forced of int * ((int*int) list)
   |Redundancy of int * ((int*int) list) 
   |Pass of int * (int list);;


let oi = Total_ordering.for_integers ;;
let oi2 = Total_ordering.product oi oi ;; 

let reorder pairs =
   let temp1 = Partition_list.according_to_fst pairs in 
   let temp2 = Image.image (fun (x,ll)->
       (x,Ordered.safe_set oi2 (List.flatten ll))
      ) temp1 in 
   let temp3 = Ordered.sort Total_ordering.for_integers (Image.image fst temp2) in 
   Image.image (fun x->(x,List.assoc x temp2)) temp3 ;;

exception Sore_wound of int list * int * sensitive_t;;

let special_obstructions =
   [
     [2;3;6;8;9]; (* because of [3; 6; 9; 12=2*6] *)
   ]
   @
   (Image.image (fun j->
      [2;j;2*j;2*j+2;3*j] (* because of [j; 2*j; 3*j; 4*j=2*2*j] *)
      
      ) [ 7;11;15]) ;;

let find_initial_obstruction_opt sorted_l =
     let a =List.hd sorted_l and b = List.hd(List.rev sorted_l) in 
     match List.find_opt (fun j->Ordered.is_included_in oi [j;2*j;3*j;4*j] sorted_l) 
        (Int_range.range a (b/4)) with 
     None -> List.find_opt (fun obstr-> Ordered.is_included_in oi obstr sorted_l) special_obstructions
     |Some(j) -> Some [j;2*j;3*j;4*j];;   



module Sensitive = struct 
 
   let check_before_adding x stv = 
      let new_sorted = Ordered.insert oi x stv.sorted in   
      match  find_initial_obstruction_opt new_sorted with 
      None -> (None,Some new_sorted) 
      |Some obstr -> (Some obstr,None) ;;

   let add (x,data_for_x) stv = 
     let (opt_bad,opt_good) = check_before_adding x stv in 
     match opt_bad with 
     Some obstr ->  raise( Sore_wound(obstr,x,stv))
      |None ->
     let new_sorted = Option.get opt_good in   
   {
      unsorted = (x,data_for_x) :: stv.unsorted;
      sorted = new_sorted ;
   }   ;;

   let coming_from_last_element stv last_elt =
       let temp1 = List.filter_map (fun (x,_)-> 
         let y= last_elt -x in 
         if (x>1)&&(x<=y)&&(List.exists (fun (z,_)->z=y) stv.unsorted) 
         then Some(x*y,[x,y])
         else None   
         ) stv.unsorted in 
         reorder temp1 ;; 
    
   let default_increase stv forbidden_indices =
         let part = Ordered.merge oi stv.sorted (Image.image fst forbidden_indices) in 
         let max_val = (if part=[] then 1 else 1+(List.hd(List.rev part))) in 
         let whole = Int_range.range 1 max_val in  
         let possibilities = Ordered.setminus oi whole part in 
         List.hd possibilities ;;   
   
   let empty_one = {unsorted =[] ; sorted =[]} ;;      

   let has_index idx stv= Ordered.mem oi idx stv.sorted ;;

   let incorporate_redundancy (idx,new_decompositions) stv = 
      let new_unsorted = (Image.image (
                  fun old_pair ->
                     let (z,ll) = old_pair in 
                     if z = idx 
                     then (z,ll@new_decompositions)
                     else old_pair   
               ) stv.unsorted ) in 
      {stv with unsorted = new_unsorted ;};;


end ;;   


module Increase = struct 

   type t = increase_t ;;

   

   let string_of_int_pair (x,y)= "("^(string_of_int x)^","^(string_of_int y)^")";;
   let string_of_int_list l =
      "[" ^ (String.concat ";" (Image.image string_of_int l)) ^ "]" ;;
   let string_of_ipair_list l =
      "[" ^ (String.concat ";" (Image.image string_of_int_pair l)) ^ "]" ;;
   
   let message = function 
      Inertia (new_val) -> (string_of_int new_val)^" added by inertia"
      |Forced(new_val,l) -> (string_of_int new_val)^" forced by "^(string_of_ipair_list l)
      |Redundancy(new_val,l) -> "Redundancy : "^(string_of_int new_val)^", with "^(string_of_ipair_list l) 
      |Pass(new_val,l) -> (string_of_int new_val)^" refused because of "^(string_of_int_list l);;
   
   let next (treated,forbidden_indices,to_be_treated)=
      match to_be_treated with 
      [] -> let new_val = Sensitive.default_increase treated forbidden_indices in 
            let (opt_bad,opt_good) = Sensitive.check_before_adding new_val treated in 
            (match opt_bad with 
            Some obstr -> (Pass(new_val,obstr),(treated,forbidden_indices@[new_val,obstr],[])) 
            |None ->   
             (Inertia(new_val),(Sensitive.add (new_val,[]) treated,forbidden_indices,
                     Sensitive.coming_from_last_element treated new_val))) 
      |(new_x,ll) :: others ->
          if Sensitive.has_index new_x treated 
          then let treated2 = Sensitive.incorporate_redundancy (new_x,ll)  treated in 
               (Redundancy(new_x,ll),(treated2,forbidden_indices,others))
          else let addenda = Sensitive.coming_from_last_element treated new_x in 
               (Forced(new_x,ll),(Sensitive.add (new_x,ll) treated,
               forbidden_indices,reorder(others@addenda)));;            

end ;;   

   

let walker = ref (Sensitive.empty_one,[],[]) ;; 

let push () =
    let (incr,next_state) = Increase.next (!walker) in 
    let msg = (Increase.message incr)^"\n" in 
    let _ = (print_string msg;flush stdout) in 
    let _ = (walker:=next_state) in 
    next_state ;;

for j= 1 to 1000 do let _ = push () in () done ;;

let (a,b,c) = (!walker) ;;

let d = List.rev (List.filter_map (fun (x,l)->
    if l=[] then Some x else None) a.unsorted);;


(*
walker:=([],[]) ;;
for j= 1 to 10 do let _ = push () in () done ;;
let z1 = fst (!walker) ;;
let z2 = Option.filter_and_unpack 
(fun (x,ll)->if ll=[] then Some x else None) z1;;
let z3 = Ordered.safe_set oi z2 ;;
let z4 = Basic.delta_list z3 ;;
let meas n = let q = (n/3) and r=(n mod 3) in 2*q+r ;;
let z5 = Image.image meas z3 ;;
let z6 = Basic.delta_list z5 ;;

walker:=([],[]) ;;
*)


(************************************************************************************************************************
Snippet 46 : Removes unnecessary blanks at the beginning of lines in an interval
************************************************************************************************************************)
open Needed_values ;;


let ap = Absolute_path.of_string "Filewatching/fw_with_githubbing.ml";;
let old_text = Io.read_whole_file ap ;;

let part1= Lines_in_text.interval old_text 1 171 ;;
let part2= Lines_in_text.interval old_text 172 254;;

let lines1 = Lines_in_text.lines part2 ;;
let lines2 = Image.image (Cull_string.cobeginning 5) lines1 ;;
let new_part2 = String.concat "\n" lines2 ;;
let new_text = part1 ^ "\n" ^ new_part2 ;;

Io.overwrite_with ap new_text ;;

(************************************************************************************************************************
Snippet 45 : Musing on permutations satisfying |i-j|<1 -> |p(i)-p(j)|<=2 
************************************************************************************************************************)
open Needed_values ;;
let ointlist = Total_ordering.silex_compare Total_ordering.for_integers ;;

let extensions1 n l = match l with 
    [] -> Int_range.range 1 n 
   | a :: others ->
      List.filter (fun x->(x>0)&&(x<=n)&&(not(List.mem x l))) [a-2;a-1;a+1;a+2] ;;

let extensions2 n ll =
  List.flatten (Image.image (fun l->
    let temp1 = extensions1 n l in 
    Image.image (fun a-> a::l) temp1
    ) ll) ;;

let main = Memoized.make(fun n->
    let rec tempf = (fun j->
      if j=0 then [[]] else 
      extensions2 n (tempf (j-1))  
    ) in 
    Image.image List.rev (tempf n)
) ;;    

let aa = Memoized.make(fun n->
  List.filter (
   fun l->List.hd(l) = 1
  ) (main (n+1))
) ;;

let factor (x,y)=
    let rec factor0=(fun
       (graet,da_ober1,da_ober2)->
       if (da_ober1=[])||(da_ober2=[])
       then (List.rev graet,da_ober1,da_ober2)
       else let (a1,peurrest1)=List_again.head_with_tail da_ober1
            and (a2,peurrest2)=List_again.head_with_tail da_ober2 in
            if a1=a2
            then factor0(a1::graet,peurrest1,peurrest2)
            else (List.rev graet,da_ober1,da_ober2)
    ) in
    factor0([],x,y);;

let extends l1 l2=
   let (_,_,r2)=factor (l1,l2) in r2=[];;

let uu = Memoized.make(fun n->
    List.filter (
     fun l->let rl = List.rev l in
     extends rl [n;n-1]
    ) (main n)
) ;;

let vv = Memoized.make(fun n->
  List.filter (
   fun l->let rl = List.rev l in
     extends rl [n-1;n]
  ) (main n)
) ;;

let ww = Memoized.make(fun n->
  List.filter (
   fun l->
    let gl = (fun k->List.nth l (k-1)) in 
    ((gl 1)=(n-1))&&(gl (n-1)=(n-2))&&(gl n=n)
  ) (main n)
) ;;

let tt = Memoized.make(fun n->
  List.filter (
   fun l->
    let gl = (fun k->List.nth l (k-1)) in 
    ((gl 1)<>(n-1))&&(gl n=(n-2))
  ) (main n)
) ;;

let ss = Memoized.make(fun n->
  List.filter (
   fun l->
    let i = List_again.find_index_of_in (n-1) l in 
    if (List.mem i [1;n-1;n])
    then false 
    else (List.nth l i)=n   
  ) (main n)
) ;;

let s_to_w =Memoized.make(fun n -> Ordered.fold_merge ointlist 
  [ss n;tt n;uu n;vv n;ww n] );;

let reversed_s_to_w =Memoized.make(fun n ->
    Ordered.safe_set ointlist (Image.image List.rev (s_to_w n)));;

let double_s_to_w =Memoized.make(fun n ->
      Ordered.intersect 
      ointlist  (s_to_w n) (reversed_s_to_w n) );;
  

let na n = List.length(aa n);;
let nk n = List.length(main n);;
let ns n = List.length(ss n);;
let nt n = List.length(tt n);;
let nu n = List.length(uu n);;
let nv n = List.length(vv n);;
let nw n = List.length(ww n);;

let zz n=(na n,nu n,nv n,nw n,ns n,nt n,nk n) ;;

let uv n = (uu n,vv(n+1)) ;;

let da n = (na (n+3))-(na(n+2)+na(n)+1) ;; 
let ka n = (nk(n)) -(2*na(n-1)) ;;

let selector = Memoized.make(fun (n,t)->
  List.filter (
   fun l->List.hd(l) = t
  ) (main (n+1))
) ;;

let aa = Memoized.make (fun n->selector(n,1)) ;;
let bb = Memoized.make (fun n->selector(n,2)) ;;
let cc = Memoized.make (fun n->selector(n,3)) ;;
let dd = Memoized.make (fun n->selector(n,4)) ;;
let ee = Memoized.make (fun n->selector(n,5)) ;;

let peggy n = Int_range.scale (fun j->List.length(selector(n,j))) 1 n ;;


let na n = List.length(aa n);;
let nb n = List.length(bb n);;
let nc n = List.length(cc n);;
let nd n = List.length(dd n);;
let ne n = List.length(ee n);;

let zz n=(na n,nb n,nc n,nd n,nk n,ne n);;




(************************************************************************************************************************
Snippet 44 : Write repetitive code for PARI-GP
************************************************************************************************************************)
let s_ap = home^
"/Teuliou/Bash_scripts/Pari_Programming/my_pari_code/follenn2.gp" ;;

let ap = Absolute_path.of_string s_ap ;;

let gtext t1 t2 = 
  String.concat "\n"
  ["c_fa="^(string_of_int t1); 
   "c_t1="^(string_of_int t2); 
   "c_for_t2=big_subst(for_t2,[t1,fa],[c_t1,c_fa])";
 "interm=subst(part3_for_t3,t1,c_t1)";
 "c_for_t3=factor(polresultant(interm,c_for_t2,t2))[2,1]";
 "around_t2=make_zero(special_euclid(interm,c_for_t2,t2,c_for_t3,t3),t2)";
 "listput(accu,[[c_fa,c_t1],around_t2])";
 "printf(Str(c_fa,\",\"c_t1,\" done\\n\"))"] ;;

let u1 = Cartesian.product (Int_range.range 11 50) (Int_range.range 101 140) ;;
let u2 = Image.image (fun (t1,t2)->gtext t1 t2) u1 ;; 
let u3 = String.concat "\n\n\n" u2 ;;

Io.Private.append_string_to_file u3 ap;;

(*

let z1 = home^
"/Teuliou/Bash_scripts/Pari_Programming/my_pari_code/follenn1.gp" ;;

let z2 = Lines_in_text.interval (rf z1) 60 67 ;;

*)

(************************************************************************************************************************
Snippet 43 : A useful shortcut using Lines_in_text.remove_interval_in_file 
************************************************************************************************************************)
let ri fn x y =
     Lines_in_text.remove_interval_in_file 
      (Absolute_path.of_string fn) x y ;;

(************************************************************************************************************************
Snippet 42 : Test the prepare_fw_with_dependencies.ml file
************************************************************************************************************************)
let the_other_one = 
   Absolute_path.of_string "../Idaho/Filewatching/fw_with_dependencies.ml" ;;
 
(*  #use "Githubbed_archive/prepare_fw_with_dependencies.ml";; 
   
 write_all_to_file the_other_one ;;  *)

(************************************************************************************************************************
Snippet 41 : Typical use of marked comments
************************************************************************************************************************)
open Needed_values ;;

let src1 = Absolute_path.of_string "../Idaho/Filewatching/file_watcher.ml";;

let dest1 = Absolute_path.of_string "../Idaho/Filewatching/fw_modular.ml";;

let act1 () = Mark_comments_for_copying_or_deletion.copy src1 dest1 ;;

let act2 () = Shorten_long_blank_intervals.in_file dest1 ;;


(************************************************************************************************************************
Snippet 40 : Extract a line interval from a file and treat it
************************************************************************************************************************)
open Needed_values ;;

let z1 = Image.image (
   fun (methname,_,_,_)->"mod_details \""^methname^"\" ([\n   \"\"\n],\"\") ;;"
) [] ;;
let z2=""::(z1@[""]);;
let z3 = String.concat "\n\n" z2 ;;
let z4 () = print_string z3 ;;

let fn = "Filewatching/fw_with_dependencies.ml";;
let fn = "Fads/jug.ml";;
let z5 = Lines_in_text.interval (rf fn) 9 92 ;;
let z6 = Lines_in_text.lines z5;;
let z7 = Image.image (fun line->"   \""^line^"\";") z6;;
let z8 = "\n\n\n"^(String.concat "\n" z7)^"\n\n\n" ;;
let z9  = print_string z8 ;;

let z6 = Image.image (Cull_string.cobeginning 6) z6;;

let g1 = Image.image (fun s->
   let j_opt = Strung.char_finder_from_inclusive_opt (fun c->c='/') s 1 in 
   let j = Option.get j_opt in 
   Cull_string.beginning j s  ) z6;;
let g2 = Ordered.sort Total_ordering.lex_for_strings g1 ;;
let g3 = String.concat " " g2;;

(************************************************************************************************************************
Snippet 39 : Remove all modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd = Dfa_subdirectory.of_line "Van_der_Waerden";;
let z1 = ae () ;;
let z2 = List.filter (
  fun (Dfn_endingless_t.J(r,s,m)) ->
    Dfa_subdirectory.begins_with s sd
) z1 ;;
let z3 = Image.image (
   fun (Dfn_endingless_t.J(r,s,m)) ->
    Dfa_module.to_line m
  ) z2 ;;
let act () = fgs z3 ;;  

(************************************************************************************************************************
Snippet 38 : Replacing a long interval in a file with another
************************************************************************************************************************)
open Needed_values ;;

let ap1 = Absolute_path.of_string "../Idaho/Compilation_management/coma_state.ml" ;;
let ap1_text = Io.read_whole_file ap1 ;;
let to_be_replaced = Lines_in_text.interval ap1_text 1 676 ;;

let towards_complement = rf "Fads/pan.ml";;
let replacement = Lines_in_text.interval towards_complement 9 240 ;;

let act7 () = Replace_inside.replace_inside_file (to_be_replaced,replacement) ap1;;

(************************************************************************************************************************
Snippet 37 : Visualize Git tree
************************************************************************************************************************)
open Needed_values ;;

let gc = "git -C "^home^"/Teuliou/OCaml/Idaho_backup ";;

let cmd_for_z0 = gc ^ "ls-tree -r HEAD > ~/Downloads/temp.txt";;
let z0 = Sys.command cmd_for_z0 ;;

let z1 = rf "~/Downloads/temp.txt";;
let z2 = Lines_in_text.lines z1 ;;
let z3 = List.filter (fun line->
   Substring.is_a_substring_of "depth_one" line 
  ) z2;;
let z4 = Image.image (Cull_string.cobeginning 53) z3;;


let cmds1 = Image.image (fun x->gc^"rm --cached "^x) z4 ;;
let cmds2 = Image.image (fun x->
  let cx = String.capitalize_ascii x in
  gc^"add "^cx) z4 ;;
let cmds3 = cmds1 @ cmds2 ;;
let anse1 = Image.image Sys.command cmds3 ;;

(************************************************************************************************************************
Snippet 36 : Miscellaneous tests on compilation management
************************************************************************************************************************)
open Needed_values ;;

let u1 = ae();;
let u2 = Image.image (fun el->Dfa_module.to_line(Dfn_endingless.to_module el)) u1 ;;
let u3 = Max.maximize_it_with_care (fun mn->
   min(List.length(abo mn))(List.length(bel mn))
  ) u2;;

let test1 = rv "Dfa_subdirectory.connectable_to_subpath" "cannectoble_to_sabputh" ;;
let exit_test1 = rv "Dfa_subdirectory.cannectoble_to_sabputh" "connectable_to_subpath" ;;

let test2= ren "dfa_subdirectory" "afd_sybdirectoru" ;;
let exit_test2= ren "afd_sybdirectoru" "dfa_subdirectory";;

let test3 = rensub "Depth_one_testdir/Depth_two_testdir" "Dopth_twe_tistder" ;;
let exit_test3 = rensub "Depth_one_testdir/Dopth_twe_tistder" "Depth_two_testdir" ;;

let create_ml_file_with_text (fname,text) =
   let s_ap = "Depth_one_testdir/Depth_two_testdir/adhoc/"^fname^".ml" in 
   let ap = Absolute_path.create_file_if_absent s_ap in 
   let _ = Io.overwrite_with ap text in ap ;;

let tf1 = create_ml_file_with_text("tf_one","let a= 2;;") ;;
let tf2 = create_ml_file_with_text("tf_two","let b= 3;;") ;;
let tf3 = create_ml_file_with_text("tf_three","let c= 7;;") ;;
let tf4 = create_ml_file_with_text("tf_four","let d= Tf_three.c+4;;") ;;
let tf5 = create_ml_file_with_text("tf_five","let e= Tf_four.d+5;;") ;;

regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_one.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_two.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_three.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_four.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_five.ml";;


Io.overwrite_with tf3 "let c=Tf_one.a+Tf_two.b;;" ;;

save "1";;

fgs ["tf_one";"tf_two";"tf_three";"tf_four";"tf_five"] ;;

(************************************************************************************************************************
Snippet 35 : Extracting lines from a file and modifying them
************************************************************************************************************************)
open Needed_values ;;

let w2 = Image.image fst (vfm "Fw_with_dependencies") ;;
let z1 = rf "Filewatching/fw_with_dependencies.ml";;
let z2 = Lines_in_text.interval z1 1304 1351 ;;

let w2 = Image.image fst (vfm "Fw_with_small_details") ;;
let z1 = rf "Filewatching/fw_with_small_details.ml";;
let z2 = Lines_in_text.interval z1 309 334 ;;

let w2 = Image.image fst (vfm "Fw_with_archives") ;;
let z1 = rf "Filewatching/fw_with_archives.ml";;
let z2 = Lines_in_text.interval z1 285 317 ;;

let w2 = Image.image fst (vfm "File_watcher") ;;
let z1 = rf "Filewatching/file_watcher.ml";;
let z2 = Lines_in_text.interval z1 526 549 ;;

let w2 = Image.image fst (vfm "Fw_configuration") ;;

let z3 = Lines_in_text.lines z2 ;;
let z4 = Image.image (
  fun line->
    let j1=(try String.index_from line 1 ' ' with _->0)
    and j2=String.index line '=' in 
    Cull_string.trim_spaces (Cull_string.interval line (j1+1) (j2-1)) 
) z3 ;;
let z5 = Image.image (
  fun line->
       if not(String.contains line ' ')
       then line 
       else let j3 =  String.index line ' ' in 
             Cull_string.beginning j3 line
) z4 ;;
let bad_in_z5=List.filter (fun x->not(List.mem x w2)) z5;;

let z4=[];;
let check_z4 = Ordered.sort Total_ordering.lex_for_strings (Image.image snd z4) ;;

let write (fun_name,lbl)=
 let addendum=(
   if lbl = "constructor" then " dummy_fw" else
   if lbl = "zerovariate_producer" then " dummy_arg" else ""

) in 
 "    let "^fun_name^" = extract_"^lbl^" All_printables."^fun_name^addendum^" ;;" ;;

let z5 = "\n\n\n" ^(String.concat "\n" (Image.image write z4)) ^ "\n\n\n";; 
let z6 () = print_string z5 ;;


let write2 (fun_name,lbl)=
 "let "^fun_name^" = Private.Exit."^fun_name^" ;;" ;;

let z7 = "\n\n\n" ^(String.concat "\n" (Image.image write2 z4)) ^ "\n\n\n";; 
let z8 () = print_string z7 ;;

(************************************************************************************************************************
Snippet 34 : Get a list of value names from an interval of lines in a file
************************************************************************************************************************)
open Needed_values ;;

let u1 = rf "Filewatching/fw_with_dependencies.ml";;
let u2 = Lines_in_text.interval u1 33 48 ;;
let u3 = Lines_in_text.lines u2 ;;
(*
let compute_names = Image.image (
  fun line ->
     let temp1 = Cull_string.two_sided_cutting ("    let ","") line in 
     let j1 = Strung.char_finder_from (fun c->List.mem c [' ';'\r';'\t']) temp1 1 in
     Cull_string.interval temp1 1 (j1-1)
) u3;;
*)


(************************************************************************************************************************
Snippet 33 : Using intervals of line indices to extract values from a module
************************************************************************************************************************)
open Needed_values ;;

let u1 = Needed_values.rf "Compilation_management/coma_state.ml";;
let u2 = Lines_in_text.indexed_lines u1 ;; 

let extract_interval ((i,j),_) =
   let temp1 = List.filter (fun (k,_)->(i<=k) && (k<=j)) u2 in 
   let temp2 = Image.image snd temp1 in 
   String.concat "\n" temp2 ;;

let ref_for_colombo = ref ([
   ((961, 985),   "compute_principal_ending");
   ((1024, 1031), "registrations_for_lonely_ending");
   ((2267, 2350), "Simplified_ts_creation")
]:(((int * int) * string) list)) ;;
let ref_for_curcuma = ref ([
  ((5, 616), "Automatic"); ((634, 634), "needed_libs_at_module");
   ((636, 636), "ancestor_at_module"); ((637, 637), "needed_dirs_at_module");
   ((659, 659), "ordered_list_of_modules"); ((671, 671), "root");
   ((792, 801), "find_needed_data"); ((925, 959), "PrivateTwo");
   ((1034, 1055), "complete_id_during_new_module_registration");
   ((1394, 1466), "register_mlx_file_on_monitored_modules");
   ((1842, 1874), "Try_to_register")
]:(((int * int) * string) list)) ;;
let ref_for_replacements = ref([]:((string * string) list));;

ref_for_replacements:=[
  "=registrations_for_lonely_ending ","=Colombo.registrations_for_lonely_ending ";
  "=md_compute_modification_times ","=Colombo.md_compute_modification_times ";
  "=md_associated_modification_time ","=Colombo.md_associated_modification_time ";
  "= md_compute_modification_time ","= Colombo.md_compute_modification_time ";
  "=compute_principal_ending ","=Colombo.compute_principal_ending ";
  "= ocamldebug_printersfile_path ","= Colombo.ocamldebug_printersfile_path ";
] ;;


type spice = Colombo | Curcuma ;;

let associated_ref = function 
   Colombo -> ref_for_colombo 
  |Curcuma -> ref_for_curcuma ;; 

let spice_to_string = function 
  Colombo -> "Colombo" 
 |Curcuma -> "Curcuma" ;; 

let haddock_order = 
    let oi = Total_ordering.for_integers 
    and prod = Total_ordering.product in 
    prod (prod oi oi) Total_ordering.lex_for_strings ;; 

 let add_interval ((i,j),name) spice=
  let raf = associated_ref spice in 
    (raf:= Ordered.insert haddock_order ((i,j),name)
      (!raf)) ;;

let copy_whole spice=
   let temp1 = Image.image (extract_interval) (!(associated_ref spice)) in
   let whole = String.concat "\n\n" temp1 
   and s = spice_to_string spice in 
   let corrected_whole = Replace_inside.replace_several_inside_text
    (!ref_for_replacements) whole in 
   Replace_inside.overwrite_between_markers_inside_file
   ~overwriter:corrected_whole
   ("(* Beginning of "^s^" *)\n\n","\n\n(* End of "^s^" *)")
   (Absolute_path.of_string "Fads/pan.ml") ;;

let main ((i,j),name) spice=
   let _= add_interval ((i,j),name) spice in copy_whole spice ;;


(*


*)

(*
main ((961,985),"compute_principal_ending") Colombo;;  
main ((1024,1031),"registrations_for_lonely_ending") Colombo;; 
main ((1655,1658),"ocamldebug_printersfile_path") Colombo;;  
main ((2267,2350),"Simplified_ts_creation") Colombo;;     

main ((5,616),"Automatic") Curcuma;; 
main ((654,654),"set_product_up_to_date_at_module") Curcuma;; 
main ((629,629),"subdir_at_module") Curcuma;; 
main ((630,630),"principal_ending_at_module") Curcuma;; 
main ((631,631),"mli_presence_at_module") Curcuma;; 
main ((634,634),"needed_libs_at_module") Curcuma;; 
main ((636,636),"ancestor_at_module") Curcuma;; 
main ((637,637),"needed_dirs_at_module") Curcuma;; 
main ((649,649),"set_needed_libs") Curcuma;; 
main ((651,651),"set_ancestors_at_module") Curcuma;; 
main ((653,653),"set_needed_dirs") Curcuma;; 
main ((655,656),"set_directories") Curcuma;; 
main ((659,659),"ordered_list_of_modules") Curcuma;; 
main ((660,660),"follows_it") Curcuma;; 
main ((661,661),"all_used_subdirs") Curcuma;; 
main ((671,671),"root") Curcuma;; 
main ((680,685),"endingless_at_module") Curcuma;; 
main ((691,697),"check_ending_in_at_module") Curcuma;; 
main ((720,723),"registered_endings_at_module") Curcuma;; 
main ((781,790),"modules_with_their_ancestors") Curcuma;; 
main ((792,801),"find_needed_data") Curcuma;; 
main ((805,813),"needed_dirs_and_libs_in_command") Curcuma;; 
main ((913,917),"compute_subdirectories_list") Curcuma;; 
main ((919,922),"check_registrations") Curcuma;; 
main ((925,959),"PrivateTwo") Curcuma;; 
main ((987,1007),"complete_info") Curcuma;; 
main ((1034,1055),"complete_id_during_new_module_registration") Curcuma;; 
main ((1378,1389),"printer_equipped_types_from_data") Curcuma;; 
main ((1394,1466),"register_mlx_file_on_monitored_modules") Curcuma;; 
main ((1468,1653),"Modern") Curcuma;; 
main ((1661,1797),"Ocaml_target_making") Curcuma;; 
main ((1842,1874),"Try_to_register") Curcuma;; 

*)

(************************************************************************************************************************
Snippet 32 : Remove all snippets containing a given substring (todo : integrate it
into the Manage_diary module directly)
************************************************************************************************************************)
open Needed_values ;;

let ap_for_diary = Absolute_path.of_string "Githubbed_archive/diary_archive.ml";;
let (g1,Manage_diary.Private.D g2) =  Manage_diary.Private.read_and_parse ap_for_diary ;;
let g3 = Int_range.index_everything g2;;
let g4 = List.filter (fun (j,(x,y))->Substring.is_a_substring_of "Vdw_" y) g3 ;;
let g5 = Image.image fst g4 ;;
let act1 () = Manage_diary.remove_snippets g5;;

let ap1 = Absolute_path.of_string "Fads/cloth.ml" ;;


let ap1 = Manage_diary.Private.usual_path ;;
let (prologue,diary1) = Manage_diary.Private.read_and_parse ap1 ;;
let (Manage_diary.Private.D l_diary1) = diary1 ;;
let z1 = List.nth l_diary1 (84-1) ;;



(************************************************************************************************************************
Snippet 31 : Search/replace following some module refactoring
************************************************************************************************************************)
open Needed_values ;;

let aps = ref [] ;;
let list_for_reps = ref [] ;;
aps := (Image.image (fun s->Absolute_path.of_string s) 
  [
    "ordered.ml";
    "Van_der_Waerden/Width_up_to_four/vdw_nonempty_index.ml";
    "Van_der_Waerden/vdw_common.ml";
    "Ordered_Lists/functor_for_sets.ml";
    "Ocaml_analysis/follow_ocaml_values.ml";
  ] );;
list_for_reps := [
  "Total_ordering.t)","Total_ordering_t.t)";
  "Total_ordering.t )","Total_ordering_t.t )";
  "Total_ordering.t\r","Total_ordering_t.t\r";
] ;;


let act1 () = List.iter 
  (Replace_inside.replace_several_inside_file 
     (!list_for_reps)) (!aps);

(************************************************************************************************************************
Snippet 30 : Extracting modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd1 = Dfa_subdirectory.of_line "Van_der_Waerden/Width_up_to_four";;

let u1 =ae () ;;

let u2 = List.filter (
  fun eless ->
    Dfa_subdirectory.begins_with (Dfn_endingless.to_subdirectory eless) sd1
) u1;; 

let u3 = Image.image (
   fun eless -> Dfa_module.to_line(Dfn_endingless.to_module eless)
) u2 ;;

(************************************************************************************************************************
Snippet 29 : Remove all "automatic" modules 
************************************************************************************************************************)
open Needed_values ;;

let u1 = ae ();;
let u2 = Image.image (fun eless ->
   Dfa_module.to_line(Dfn_endingless.to_module eless)  
) u1;;
let u3 = List.filter (
  fun x-> String.ends_with ~suffix:"_automatic" x 
) u2 ;;

let computed_u3 = ["concrete_object_automatic"; "fw_wrapper_automatic"; "coma_state_automatic";
"fw_nonmodular_wrapper_automatic"; "hex_flattened_end_strategy_automatic"];;

let g1 = vfm "hex_flattened_end_strategy_automatic" ;;
let g2 = Image.image fst g1 ;;

let g3 = Image.image (fun x-> Replace_inside.replace_inside_text ("x",x) "let x = Automatic.x ;;") g2;;
let g4 = String.concat "\n" g3 ;;
let g5 = "\n\n\n" ^ g4 ^ "\n\n\n" ;; 

let h1 = List.flatten (Image.image snd g1) ;;
let h2 = Ordered.sort Total_ordering.standard h1 ;;
let h3 = List.iter (
  fun fn -> Replace_inside.replace_inside_file ("Hex_flattened_end_strategy_automatic.","Hex_flattened_end_strategy.") fn
) h2 ;;


(************************************************************************************************************************
Snippet 28 : Typical use of the Manage_diary module
************************************************************************************************************************)
let act1 () = Manage_diary.fix_indexation ();;

let act2 () = Manage_diary.remove_snippets [ (* put indices here *)];;


let diary_text = Io.read_whole_file ap_for_diary ;;

let (g1,g2) =  Manage_diary.Private.read_and_parse ap_for_diary ;;

(************************************************************************************************************************
Snippet  27 : Deduce the lower measure from the usual measure (related to Vdw)
************************************************************************************************************************)
let measure n =
  if n<1 then 0 else 
  let q=(n/9) in 
  match n mod 9 with
   0 -> 4*q+1 
  |1 -> 4*q+1
  |2 -> 4*q+2  
  |3 -> 4*q+2
  |4 -> 4*q+3
  |5 -> 4*q+4
  |6 -> 4*q+4  
  |7 -> 4*q+4
  |8 -> 4*q+4 
  | _ -> failwith("unforeseen");;   

let lower_measure n =
   if n<1 then 0 else 
   let q=(n/9) in 
   match n mod 9 with
    0 -> 4*q
   |1 -> 4*q
   |2 -> 4*q 
   |3 -> 4*q
   |4 -> 4*q+1
   |5 -> 4*q+1
   |6 -> 4*q+2  
   |7 -> 4*q+2
   |8 -> 4*q+3 
   | _ -> failwith("unforeseen");;  



let compute_lower_measure n = 
  let tempf = (fun t->measure(n+t)-measure(t)) in 
  snd(Min.minimize_it tempf (Int_range.range 1 20)) ;;   


(************************************************************************************************************************
Snippet  26 : Relocate all modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd1 = Dfa_subdirectory.of_line 
   "Van_der_Waerden" ;;

let sd2 = Dfa_subdirectory.of_line 
   "Van_der_Waerden/First_try" ;;

let u1 = ae () ;;   
let u2 = List.filter ( fun
  (Dfn_endingless_t.J(r,sd,m)) -> Dfa_subdirectory.begins_with sd sd1
) u1 ;;
let u3 = Image.image ( fun
(Dfn_endingless_t.J(r,sd,m)) -> Dfa_module.to_line m
) u2 ;;

let act1 () = Explicit.image (fun mn->relo mn sd2) u3 ;;

(************************************************************************************************************************
Snippet  25 : Delete all modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd1 = Dfa_subdirectory.of_line 
   "Ocaml_analysis/Standardized_concrete_ocaml_types" ;;

let u1 = ae () ;;   
let u2 = List.filter ( fun
  (Dfn_endingless_t.J(r,sd,m)) -> Dfa_subdirectory.begins_with sd sd1
) u1 ;;
let u3 = List.rev_map ( fun
(Dfn_endingless_t.J(r,sd,m)) -> Dfa_module.to_line m
) u2 ;;

let act1 () = fgs u3 ;;


(************************************************************************************************************************
Snippet  24 : Code from an abandoned, self-contained module
************************************************************************************************************************)
exception Too_many_arguments of int ;;

let wrap_in_parentheses_if_needed typename =
    if String.contains typename ' '
    then "( "^typename^" )"
    else  typename ;;    

let max_nbr_of_arguments = 7 ;;    

let arguments_in_input argname n=
    if n> max_nbr_of_arguments 
    then raise(Too_many_arguments(n))
    else let temp1 = Int_range.scale (fun k->
          if k<=n then argname^(string_of_int k) else "_") 1 max_nbr_of_arguments in 
         "(" ^ (String.concat "," temp1) ^ ")" ;;       

let listify is_a_list name =
    if not(is_a_list) 
    then name 
    else (wrap_in_parentheses_if_needed name)^" list" ;;     

let add_appendix_to_last_line appendix lines =
      let (last_line,other_lines) = List_again.head_with_tail (List.rev lines) in 
      List.rev ((last_line^appendix)::other_lines) ;;    

(************************************************************************************************************************
Snippet  23 : Permutations far (wrt Hamming distance) from shift with constants. 
************************************************************************************************************************)
open Needed_values ;;

let hamming_distance perm1 perm2 =
  let temp1 = List.combine perm1 perm2 in 
  List.length(List.filter (fun (x,y)->x<>y) temp1);;

let generic_translate n t  = (Int_range.range t n) @ (Int_range.range 1 (t-1))  ;;

let all_translates =Memoized.make (fun n -> Int_range.scale (generic_translate n) 1 n);;

let measure n perm = snd(Min.minimize_it (hamming_distance perm) (all_translates n)) ;;


let iii = Memoized.make Permutation.iii ;;

let ff = Memoized.make(fun n->
   let whole = iii n 
   and meas = Memoized.make (measure n) in 
   let m = snd(Max.maximize_it meas whole) in 
   Explicit.filter (fun perm->meas(perm)=m) whole);;   


let gg n = Chronometer.it ff n;;

let hh n = (measure n (List.hd(ff n)));;

Int_range.scale (fun x->(x,hh x)) 3 10;;

let hf n = List.hd(ff n) ;;

(************************************************************************************************************************
Snippet  22 : Mass inheritance from a Private submodule 
************************************************************************************************************************)
let z1 = 
  ["conventional_files_with_full_content";
   "conventional_files_with_minimal_content"; "debug_build_subdir";
   "exec_build_subdir"; "full_set_of_needed_dirs"; "git_ignored_subdirectories";
   "minimal_set_of_needed_dirs"; "rootless_path_for_loadingsfile";
   "rootless_path_for_parametersfile"; "rootless_path_for_printersfile";
   "rootless_path_for_targetfile"; "usual_build_subdir"; "utility_files_subdir"] ;;
  
let z2 = Image.image (fun x->" let "^x^" = Private."^x^" ;;") z1;; 
  
let z3 = "\n\n\n" ^ (String.concat "\n" z2) ^ "\n\n\n" ;; 

(************************************************************************************************************************
Snippet  21 : Remove interval of lines in a file 
************************************************************************************************************************)
let ap = Absolute_path.of_string "Imported/Aantron/aantron_markup.ml";;
let old_text = Io.read_whole_file ap ;;
let v1 = Lines_in_text.indexed_lines old_text ;;
let v2 = List.filter (fun (j,line)->(299<=j)&&(j<=338) ) v1 ;;
let v3 = Image.image (
   fun (j,line)->
      let i1 = Option.get(Substring.leftmost_index_of_in_from_opt "val " line 1) 
      and i2 = Option.get(Substring.leftmost_index_of_in_from_opt ":" line 1) in 
      Cull_string.trim_spaces(Cull_string.interval line (i1+4) (i2-1))
) v2 ;;
let tab = String.make 5 ' ' ;;
let v3 = Image.image (fun (j,line) -> 
  if String.starts_with ~prefix:tab line 
  then Cull_string.two_sided_cutting (tab,"") line
  else line   
    ) v2;;

let tab = String.make 7 ' ' ;;
let v4 = Ordered.sort Total_ordering.lex_for_strings v3;;
let v5 = Image.image (fun name -> tab^"let "^name^" = Aantron_encoding."^name^" ;;") v4;;
let old_snippet = String.concat "\n" (Image.image snd v2) ;;
let new_snippet = String.concat "\n"  v5;;
let act () = Replace_inside.replace_inside_file (old_snippet,new_snippet) ap ;; 

let ap = Absolute_path.of_string "Imported/Aantron/aantron_markup.ml";;
let old_text = Io.read_whole_file ap ;;
let v1 = Lines_in_text.indexed_lines old_text ;;
let v2 = List.filter (fun (j,line)->(299<=j)&&(j<=338) ) v1 ;;
let v3 = Image.image (fun (j,line)->Cull_string.trim_spaces line) v2 ;;
let v4 = List.filter (String.starts_with ~prefix:"The value ") v3;;
let v5 = Image.image (
   fun line->
      let i1 = Option.get(Substring.leftmost_index_of_in_from_opt "`" line 1) 
      and i2 = Option.get(Substring.leftmost_index_of_in_from_opt "'" line 1) in 
      Cull_string.trim_spaces(Cull_string.interval line (i1+1) (i2-1))
) v4 ;;
let v6 = Ordered.sort Total_ordering.lex_for_strings v5;;
let v7 = Image.image (fun name -> "let "^name^" = Aantron_utility."^name^" ;;") v6;;
let v8 = "\n\n\n" ^ (String.concat "\n" v7) ^ "\n\n\n";;
let v9 = print_string v8 ;;

(************************************************************************************************************************
Snippet  20 : Removing module wrappers in a set of files
************************************************************************************************************************)
let remove_module_wrapper_in_text text =
  let lines = Lines_in_text.indexed_lines text in 
  let (i1,_)= List.find (fun (_,line)->
    String.starts_with ~prefix:"module " (Cull_string.trim_spaces line) 
  ) lines in
  let (i2,_)= List.find (fun (_,line)->
    String.starts_with ~prefix:"end" (Cull_string.trim_spaces line) 
  ) (List.rev lines) in 
  let selected_lines = List.filter_map (
    fun (i,line)->if List.mem i [i1;i2] then None else Some line
  ) lines in 
  String.concat "\n" selected_lines ;;

let remove_module_wrapper_in_file ap =
  let old_text = Io.read_whole_file ap in 
  let new_text = remove_module_wrapper_in_text old_text in 
  Io.overwrite_with ap new_text ;;

let the_dir = Directory_name.of_string ((Sys.getcwd())^"/Imported/Aantron/Temp"  ) ;;
let u1 = Unix_again.simple_ls the_dir ;;

let act1 () = List.iter remove_module_wrapper_in_file u1 ;;

(************************************************************************************************************************
Snippet  19 : Sorting names in the dictionary order
************************************************************************************************************************)
let z1 = Ordered.sort Total_ordering.lex_for_strings 
[
  "current_state";
  "emit";
  "push_and_emit";
  "pop";
  "emit_end";
  "initial_state";
  "document_state";
  "doctype_state";
  "root_state";
  "after_root_state";

] ;;

let z2 = "\n\n\n" ^ (String.concat "\n" z1) ^ "\n\n\n" ;;

print_string z2;;

(************************************************************************************************************************
Snippet  18 : Remove phpbb links to footnotes 
************************************************************************************************************************)
let write1 k=
  let sk = string_of_int k in 
  "\n[b][color=blue]("^sk^")[/color][/b]\n" ;;

let reps = Int_range.scale (fun j->(write1 j,"")) 1 43  ;;

let dir = (Sys.getenv "HOME")^"/Teuliou/html_files/Translations/";;  
let ap1 =   Absolute_path.create_file_if_absent (dir^"/notes_to_dot.txt") ;;

let text1= Io.read_whole_file ap1;;
let lines1 = Lines_in_text.indexed_lines text1;;

let act1 () = Replace_inside.replace_several_inside_file reps ap1;;




(************************************************************************************************************************
Snippet  17 : Typical use of Html_to_phpbb.translate
************************************************************************************************************************)
open Needed_values ;;

let u1 = rf (home^"/Teuliou/html_files/Fenton/divine_origin.html");;

let u2 = Html_to_phpbb.translate u1;;

let ap1 = Absolute_path.of_string 
  (home^"/Teuliou/html_files/Translations/divine_origine_translated.txt") ;;

Io.overwrite_with ap1 u2;;  

(************************************************************************************************************************
Snippet  16 : Interaction between "beginning" and "end" of a large tex file
************************************************************************************************************************)
open Needed_values;;

let beg_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/beginning_of_text.txt");; 
 
let end_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/end_of_text.txt");; 

let tex_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/blet_pius_xii.tex");;   

let cmd_for_texshop = "osascript "^home^"/Teuliou/Bash_scripts/Automation/compile_with_texshop.scpt";;

let loop () =
    let beg_part = Io.read_whole_file beg_ap 
    and end_part = Io.read_whole_file end_ap in 
    let whole = beg_part ^ "\n" ^ end_part in 
    let _ =Io.overwrite_with tex_ap whole in 
    Sys.command cmd_for_texshop;;

let tr k = Io_again.transfer_first_lines_of_to k end_ap beg_ap;;

let ll k = let temp = Lines_in_text.interval (Io.read_whole_file end_ap) k k in 
  (temp,Strung.explode temp);;

let rye (a,b) = Replace_inside.replace_inside_file (a,b) end_ap ;; 

let rblap () = Remove_blank_lines_around_percents.in_file end_ap ;;

let rlc pattern = 
   let _ = Lines_in_text.remove_lines_containing_substring_in_file 
   pattern end_ap in rblap ();;

let usual_cleaning () =
   Replace_inside.replace_several_inside_file 
   [
     (".! ",".1 ");
     (".!\n",".1 ");
    ("\012","");
    ("_","");
    ("#","");
    ("\\Vhat","What");
    ("\\Vhen","When");
    ("\\Vhile","While");
    ("\194\162","c");
    ("\226\128\156","\194\171");
    ("\226\128\157","\194\187");
    ("$","\194\167");
    (" & "," \\& ");
    ("&\226\128\153","d'");
    ("\\xii. ","xii. ");
    ("\194\165","V");
    ("\n1}","\n1 ");
    ("\n}","\n1 ");
    ] end_ap ;;


(************************************************************************************************************************
Snippet  15 : Add blank space at the beginning of lines (to make copy&paste easier )
************************************************************************************************************************)
open Needed_values;;

let blanks = String.make 3 ' ';; 

let reform_line x=
  if (x="")||(String.starts_with ~prefix:blanks x ) then x else blanks^x;; 

let reform_string s=
  let temp1 = Lines_in_text.lines s in 
  let temp2 = Image.image reform_line temp1 in 
  String.concat "\n" temp2 ;;


let the_ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/PDF_files/Printable/Preparation/greek_in_vl.txt");; 

let old_text = Io.read_whole_file the_ap ;;

let new_text = reform_string old_text ;;

Io.overwrite_with the_ap new_text;; 


(************************************************************************************************************************
Snippet  14 : Delete some HTML footnotes (with their links) and reindex
************************************************************************************************************************)
open Needed_values;;

let ap1 = Absolute_path.of_string (home^"/Teuliou/html_files/Fortescue/papacy_451.html");; 
let old_text = Io.read_whole_file ap1 ;;

let u1 = Enumerate_html_links_to_footnotes.main old_text ;;

let see = Image.image (fun ((i_start,i_end),link_idx)->
    Cull_string.interval old_text i_start i_end) u1 ;; 

let bad_indices = [1;3;4;5;10;11;18;20] ;;

let u2 = List.filter (fun ((i_start,i_end),link_idx)-> 
    (List.mem link_idx bad_indices) ) u1;;
let u3 = Image.image fst u2 ;;
let u4 = Int_range.index_everything u3 ;; 
let u5 = Image.image (
  fun (k,(i_start,i_end))->((i_start,i_end),k)
) u4;;
let u6 = Image.image (fun ((i_start,i_end),link_idx)-> 
    ((i_start,i_end),List.assoc_opt (i_start,i_end) u5) 
) u1;;
let write_link opt = match opt with 
  None -> ""
  |Some(k) -> let sk=string_of_int k in 
              "<span id=\"ln"^sk^"\"><a href=\"#n"^sk^"\">("^sk^")</a></span>";;

let u7 = Image.image ( fun (pair,opt)->(pair,write_link opt) ) u6;;

let new_text = Strung.replace_ranges_in u7 old_text ;;

Io.overwrite_with ap1 new_text ;;

let ap1 = Absolute_path.of_string (home^"/Teuliou/html_files/Fortescue/papacy_451.html");; 
let old_text = Io.read_whole_file ap1 ;;
let v1 = Enumerate_html_footnotes.main old_text ;;
let see = Image.image (fun ((i_start,i_end),_)->
    Cull_string.interval old_text i_start i_end) v1 ;;   
let good_indices = List.filter (fun k->not(List.mem k bad_indices )) (Int_range.range 1 (List.length v1));;
let reindexation = Image.image (fun (i,j)->(j,i)) (Int_range.index_everything good_indices) ;;
let v2 = Image.image (
  fun ((footnote_idx,html_content),(i_start,i_end))->
    ((footnote_idx,html_content),(i_start,i_end),List.assoc_opt footnote_idx reindexation)
) v1;;
let write_reindexed_version ((i_start,i_end),(footnote_idx,html_content),opt_idx)=
   let new_text = (match opt_idx with 
      None -> ""
      |Some(k)->let sk=string_of_int k in 
      "<div id=\"n"^sk^"\"><a href=\"#ln"^sk^"\">("^sk^")</a> "^html_content^"</div>"
   ) in 
   ((i_start,i_end),new_text);;
let v3 = Image.image write_reindexed_version v2;;
let new_text = Strung.replace_ranges_in v3 old_text ;;
Io.overwrite_with ap1 new_text ;;





(************************************************************************************************************************
Snippet  13 : Remove contiguous lines in a file
************************************************************************************************************************)
open Needed_values ;;

let the_ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/Fortescue/papacy_451.html");; 

let old_text = Io.read_whole_file the_ap ;;

let to_be_deleted = Lines_in_text.interval old_text 3387 4948 ;;

Replace_inside.replace_inside_file (to_be_deleted,"") the_ap ;; 


(************************************************************************************************************************
Snippet  12 : Put fillable footnotes in an html draft 
************************************************************************************************************************)
let write1 k=
  let sk = string_of_int k in 
  "<span id=\"ln"^sk^"\"><a href=\"#n"^sk^"\">("^sk^")</a></span>"^
  "\n\n\n"^
  "<div id=\"n"^sk^"\"><a href=\"#ln"^sk^"\">("^sk^")</a>   \n\n "^
  "</div>" ;;

let dir = (Sys.getenv "HOME")^"/Teuliou/html_files/Fortescue";;  
let ap =   Absolute_path.create_file_if_absent (dir^"/pra_filled.html") ;;

let memo = String.concat "\n\n" (Int_range.scale write1 121 170) ;;

Io.overwrite_with ap memo ;; 

(************************************************************************************************************************
Snippet  11 : Combinatorial musings
************************************************************************************************************************)
exception Hard_computation of string * int ;;

let translate_all t (n,sols)=
  let increment = (if t="1" then 1 else 0) in  
  (n+increment,Image.image (fun u->t^u) sols) ;;

let synthesize_after_translating (n1,sols1) (n2,sols2) =
    if n1 < n2 then (n2,sols2) else 
    if n2 < n1 then (n1,sols1) else (n1,sols1@sols2);; 

let synthesize res1 res2 = 
    synthesize_after_translating 
     (translate_all "0" res1) (translate_all "1" res2);; 


let main_hashtbl = ((Hashtbl.create 50): (string * int, int * (string list)) Hashtbl.t);; 

let am x y = Hashtbl.add main_hashtbl x y;;

let eval_at_one pattern =
     if pattern="" then (1,["1"]) else 
     (if (String.get pattern 0)='F' then  (1,["1"]) else (0,["0"]) );;

let enforce_conditions pattern = 
    let m = String.length pattern in 
    let temp2 = Int_range.scale (fun j->
        if (j<5)&&(j<>2) then "N" else 
        if j>m then "F" else Cull_string.interval pattern j j) 1 (max 4 m) in 
    String.concat "" temp2;;     

(*

enforce_conditions "A";;
enforce_conditions "ANPE";;

*)

let prepare_computation pattern=
   if pattern="" then ("",Some"NFNN") else 
   let tail = Cull_string.cobeginning 1 pattern in 
   (
    if (String.get pattern 0)='F'
    then (tail,Some(enforce_conditions tail))  
    else (tail,None)    
   )      ;;

let left_n_decomposition pattern =
     let j1_opt = Strung.char_finder_from_inclusive_opt (fun c->c<>'N') pattern 1 in 
     if j1_opt=None 
     then (String.length pattern,"") 
     else 
      let j1 = Option.get j1_opt in 
      (j1-1,Cull_string.cobeginning (j1-1) pattern);; 

(*

left_n_decomposition "ABC";;
left_n_decomposition "NNABC";;

*)


let eval_quickly pattern n =
    if n=0 then (0,[""]) else
    if n=1 then eval_at_one pattern else
    match Hashtbl.find_opt main_hashtbl (pattern,n) with 
     Some(l,sol)->(l,sol)
     |None -> raise(Hard_computation(pattern,n)) ;;

let eval_using_left_n_decomposition old_pattern n =
    if n=0 then (0,[""]) else
    if n=1 then eval_at_one old_pattern else
    let old_length = String.length old_pattern in 
    let pattern = (if old_length > n 
                   then Cull_string.beginning n old_pattern
                   else old_pattern) in 
    let (number_of_ns,core) = left_n_decomposition pattern in 
    let (size_of_sols,old_sols) = eval_quickly core (n-number_of_ns) in 
    let new_sols =(
       if number_of_ns=0 
      then old_sols 
      else  let offset = String.make number_of_ns '0' in 
      Image.image (fun t->offset^t) old_sols
      ) in 
    (size_of_sols,new_sols);; 


let eval_slowly pattern n =
       try eval_using_left_n_decomposition pattern n with _->
         let (passive_case,opt_active_case) = prepare_computation pattern in 
         let case0 =  translate_all "0" (eval_using_left_n_decomposition passive_case (n-1)) in 
         match opt_active_case with 
         None -> case0
        |Some(active_case) ->
          let case1 =  translate_all "1" (eval_using_left_n_decomposition active_case (n-1)) in 
          synthesize_after_translating case0 case1;;     

let consider pattern n=
   let res = eval_slowly pattern n in 
   let _= (am (pattern,n) res) in res ;;

let ff n = eval_slowly "" n;;

let bf n = Image.image ff (Int_range.range 1 n);;



consider "" 2 ;;
consider "" 3 ;;
consider "FN" 2;;
consider "" 4;;
for k=3 to 30 do let _ = consider "FNN" k in ();let _=consider "" (k+2) in () done ;;


let res1 = Int_range.scale (fun x->fst(ff x)) 1 30;;




(************************************************************************************************************************
Snippet  10 : Massive conversion of audios into videos using ffmepgs
************************************************************************************************************************)
let base1 =
  [
  
  "001_Ier_D_de_l_Avent_01_12_2013_27min33.mp";
  "002_IIe_dimanche_de_l_Avent_Moulins_09_12_1990_35min32.mp";
  "003_IIIe_dimanche_de_l_Avent_Gaudete_En_Anjou_16_12_1990_29min26.mp";
  "004_IIIe_dim_de_l_Avent_Vendee_16_12_2018.mp";
  "005_IVe_dimanche_de_l_Avent_Comparaison_du_temps_des_Patriarches_avec_le_notre_Moulins_23_12_1990_28min19_Copie_en_conflit_de_debian_2019_11_13.mp";
  "006_Vigile_Nativite_Choix_et_gouts_de_Dieu_Moulins_24_12_1990_18min52.mp";
  "007_Vigile_de_Noel_En_Vendee_24_12_2012_29min31.mp";
  "008_Messe_de_Minuit_Anniversaire_naissance_de_la_fille_ainee_de_l_Eglise_Moulins_25_12_1996_28min04.mp";
  "009_Messe_du_jour_de_Noel_Divinite_du_Christ_Seigneur_demontree_par_S_Paul_aux_Hebreux_a_partir_de_l_Ancien_Testament_Moulins_25_12_1996_19min48.mp";
  "010_Nativite_Messe_de_minuit_En_Vendee_25_12_2012_14min17.mp";
  "011_Nativite_Messe_du_jour_En_Vendee_25_12_2012_11min26.mp";
  "012_Dimanche_dans_l_Octave_de_la_Nativite_Dum_medium_silentium_Tours_31_12_1989_21min22.mp";
  "013_Dimanche_dans_l_octave_de_NoeI_30_12_2012_21min08.mp";
  "014_Dimanche_dans_l_Octave_de_NoeI_ND_de_l_Epine_29_12_2019_28_min.mp";
  "015_Circoncision_En_Vendee_01_01_2013_26min22.mp";
  "016_Saint_Nom_de_Jesus_En_Vendee_02_01_2013_22min24.mp";
  "017_Epiphanie_06_01_1996_38min27.mp"; "018_Epiphanie_07_01_90_29min34.mp";
  "019_Octave_de_l_Epiphanie_Manifestation_de_la_Divinite_de_NS_Moulins_13_01_1991_38min17.mp";
  "020_La_Sainte_Famille_Moulins_07_01_1996_31min33.mp";
  "021_IIe_dimanche_apres_l_Epiphanie_Mayenne_ND_de_l_Epine_19_01_2014_30min50.mp";
  "022_IIIe_dimanche_apres_l_Epiphanie_Noli_vinci_a_malo_sed_vince_in_bono_malum_25_01_90_34min31.mp";
  "023_Septuagesime_Deux_genres_de_conversion_Moulins_27_01_1991_28min27.mp";
  "024_Sexagesime_ND_de_Lourdes_Montee_de_l_esprit_anti_chretien_et_apparitions_de_ND_Moulins_11_02_1996_33min36.mp";
  "025_Quinquagesime_Annonce_prophetique_de_la_Passion_Moulins_10_02_1991_24min39.mp";
  "026_Ier_dimanche_de_Careme_Sens_mystique_des_montees_vers_Jerusalem_16_02_1997_15min18.mp";
  "027_Ier_dimanche_de_Careme_Sur_la_Penitence_Moulins_04_03_1990_24min53.mp";
  "028_IIe_Dim_de_Careme_Transfiguration_Equilibre_entre_desolations_et_consolations_Moulins_24_02_1991_34min49.mp";
  "029_IIIe_dimanche_de_Careme_Contre_le_demon_muet_Moulins_10_03_1996_26min07.mp";
  "030_IIIe_dimanche_de_Careme_Sur_l_Annonciation_Maternite_virginale_et_voeu_de_virginite_22_03_92.mp";
  "031_IVe_dimanche_de_Careme_Joie_dans_la_penitence_et_Montee_du_Carmel_10_03_91_26min06.mp";
  "032_Ier_dimanche_de_la_Passion_ReveIation_progressive_de_la_divinite_de_NS_Moulins_05_04_1992_40min02.mp";
  "033_Dimanche_des_Rameaux_En_Vendee_01_04_2012_16min12.mp";
  "034_Veillee_pascale_Sur_l_illogisme_de_l_attitude_actuelle_des_Juifs_talmudistes_Moulins_11_04_1998_26min11.mp";
  "035_Paques_Moulins_04_04_1996_44min07.mp";
  "036_Dimanche_in_albis_Quasi_modo_Les_corps_glorieux_Tours_02_04_1989_19min22.mp";
  "037_IIe_dimanche_apres_Paques_Misericorde_et_Justice_de_Dieu_equilibre_de_l_esprit_chretien_Fete_de_saint_Pierre_de_Verone_29_04_1990_30min12.mp";
  "038_IIIe_dimanche_apres_Paques_Modicum_et_videbitis_Me_ND_de_l_Epine_Mayenne_21_04_2013_26min12.mp";
  "039_IVe_dimanche_apres_Paques_Attachement_apostolique_a_NS_pour_remonter_de_sa_nature_humaine_a_sa_Divinite_28_04_1991_23min03.mp";
  "040_Ve_dimanche_apres_Paques_Dieu_console_par_les_siens_Tours_30_04_1989_20min42.mp";
  "041_Ascension_24_05_1990_25min18.mp";
  "042_Ascension_Moulins_09_05_1991_27min55.mp";
  "043_Ascension_2011_Vendee_20min32.mp";
  "044_Ascension_ND_de_l_Epine_10_05_2018_26min12.mp";
  "045_Dimanche_dans_l_Octave_de_l_Ascension_12_05_2013_Mayenne_ND_de_l_Epine_15min28.mp";
  "046_Dimanche_dans_l_Octave_de_l_Ascension_2014_Vendee_19min.mp";
  "047_Pentecote_Moulins_19_05_1991_29min18.mp";
  "048_Pentecote_Vendee_19_05_2013_30min45.mp";
  "049_Pentecote_ND_de_l_Epine_15_05_2016_30min_50.mp";
  "050_Tres_Sainte_Trinite_Moulins_16_06_1990_33min05.mp";
  "051_Fete_du_Tres_Saint_Sacrement_Moulins_02_06_1991_28min53.mp";
  "052_Dimanche_dans_l_octave_du_Saint_Sacrement_Notre_Dame_de_l_Epine_2_06_2013_14min26.mp";
  "053_Solennite_du_Sacre_Coeur_et_Saint_Jean_Baptiste_Il_faut_qu_Il_croisse_et_que_je_diminue_Moulins_24_06_1990_24min55.mp";
  "054_Dimanche_dans_l_Octave_du_Sacre_Coeur_29min09.mp";
  "055_IVe_dimanche_apres_la_Pentecote_Moulins_16_06_1991_31min54.mp";
  "056_Ve_dimanche_apres_la_Pentecote_Sur_reparation_et_componction_Tours_18_06_1989_34min59.mp";
  "057_VIe_dimanche_apres_la_Pentecote_Saint_Henri_et_la_sanctification_dans_le_monde_Moulins_15_07_1990_37min39.mp";
  "058_VIIe_Dimanche_apres_la_Pentecote_Mayenne_ND_de_l_Epine_07_07_2013_36min04.mp";
  "059_VIIIe_dimanche_apres_la_Pentecote_Saint_Bonaventure_et_le_bonheur_en_Dieu_seul_Moulins_14_07_1991_42min06.mp";
  "060_IXe_dimanche_apres_la_Pentecote_Mayenne_ND_de_l_Epine_21_07_2013_34min55.mp";
  "061_Xe_dimanche_apres_la_Pentecote_Sur_le_Principe_et_Fondement_Tours_31_07_1988_22min08.mp";
  "062_XIe_dimanche_apres_la_Pentecote_04_08_1991_33min37.mp";
  "063_XIe_Dimanche_apres_Pentecote_12_08_2012_15min44.mp";
  "064_XIIe_dimanche_apres_la_Pentecote_11_08_2013_27min32.mp";
  "065_XIIIe_dimanche_apres_la_Pentecote_18_08_2013_27min34.mp";
  "066_XIVe_dimanche_apres_la_Pentecote_Saint_Louis_25_08_1991.mp";
  "067_XVe_dimanche_apres_la_Pentecote_Foi_en_la_Divinite_de_NS_Moulins_16_09_90_27min23.mp";
  "068_XVIe_dimanche_apres_la_Pentecote_03_09_89_24min15.mp";
  "069_XVIIe_Dimanche_apres_la_Pentecote_ND_de_l_Epine_16_09_2018_40_min.mp";
  "070_XVIIIe_Dim_apres_la_Pentecote_ND_de_l_Epine_8_10_2017_38min30.mp";
  "071_XIXe_dimanche_apres_la_Pentecote_Sur_la_colere_24_09_89_22min01.mp";
  "072_XXe_D_ap_Pent_22_10_2017_ND_de_l_Epine_1h.mp";
  "073_Fete_du_Christ_Roi_27_10_1991_27min22.mp";
  "074_XXIe_dimanche_apres_Pentecote_25min24.mp";
  "075_XXIIe_Dimanche_apres_la_Pentecote_ND_de_l_Epine_21_10_2018_36_min_35.mp";
  "076_XXIIIe_dimanche_apres_la_Pentecote_Sur_le_Purgatoire_11_11_90.mp";
  "077_XXIVe_et_dernier_dimanche_apres_la_Pentecote_Moulins_26_11_1989_26min38.mp";
  "078_XXIVe_ap_Pent_IVe_ap_Eph_Tempete_apaisee_Moulins_29min55.mp";
  "079_XXVe_dim_ap_Pent_Ve_ap_Epiphanie_Moulins_04_02_1990_20min.mp";
  "080_XXVIe_dim_apres_la_Pentecote_VIe_ap_Epiphanie_17_11_1991_30min38.mp";
  "081_Solennite_du_Tres_Saint_Rosaire_Tours_08_10_1989_23min31.mp";
  "082_Maternite_Divine_de_Notre_Dame_11_10_2015_Mayenne_ND_de_l_Epine_28min20.mp";
  "083_Toussaint_Moulins_01_11_1996_30min49.mp";
  "084_Commemoration_des_defunts_Vendee_2_novembre_2012_21min07.mp";
  "085_Fete_de_la_Dedicace_de_Saint_Jean_de_Latran_09_11_2014_33min47.mp";
  "086_Solennite_de_l_Immaculee_Conception_Moulins_1989_30min05.mp";
  "087_Presentation_de_NS_au_temple_et_Purification_de_Marie_Mayenne_ND_de_l_Epine_02_02_2014_23min13.mp";
  "088_Decouverte_de_la_Sainte_Croix_ND_de_l_Epine_3_05_2015_34min28.mp";
  "089_St_Philippe_et_st_Jacques_ND_de_l_Epine_11_05_2014_18min42_.mp";
  "090_Solennite_de_Ste_Jeannes_d_Arc_Moulins_13_05_1990_27min49.mp";
  "091_VIe_dimanche_apres_la_Pentecote_Solennite_de_St_Pierre_et_St_Paul_Monde_conquis_de_haute_lutte_par_papes_et_martyrs_Moulins_30_06_1991_42min50.mp";
  "092_Fete_de_Sainte_Anne_Mayenne_ND_de_l_Epine_26_07_2015_42min41.mp";
  "093_Saint_Laurent_diacre_et_martyr_Vendee_10_08_2014_33min24.mp";
  "094_Fete_de_saint_Luc_Mayenne_ND_de_l_Epine_18_10_2015_27min09.mp";
  "095_Sur_la_maniere_de_precher_1988_22min32.mp";
  "096_Assomption_Moulins_15_08_1991_29min15.mp"; "097_Assomption_2012.mp";
  "098_Assomption_2013_Vendee_21min34.mp";
  "099_Saint_Joachim_Pere_de_la_TS_Vierge_Marie_Vendee_16_08_2015_24min03.mp";
  "100_Tres_Precieux_Sang_Mayenne_ND_de_l_Epine_1er_juillet_2013_27min03.mp";
  "101_Nativite_de_Notre_Dame_Moulins_08_09_1991_30min28.mp";
  "102_Notre_Dame_des_sept_douleurs_15_09_1996_15min51.mp";
  "103_Solennite_de_Ste_Therese_de_l_Enfant_Jesus_et_de_la_Ste_Face_Moulins_30_09_1990_22min47.mp";
  "104_Solennite_de_saint_Michel_Archange_Moulins_29_09_1991_32min31.mp";
  "105_IVe_D_ap_Pentecote_ND_de_l_Epine_28_juin_2020_35min.mp";
  "106_VIe_D_ap_Pentecote_ND_de_l_Epine_12_7_2020_39min25.mp";
  "107_refutation_T_de_M_ete_2013_Intro_10min.mp";
  "108_refutation_T_de_M_ete_2013_partie_1_18min19.mp";
  "109_refutation_T_de_M_ete_2013_partie_2_18min41.mp";
  "110_refutation_T_de_M_ete_2013_partie_3_23min22.mp";
  "111_refutation_T_de_M_ete_2013_partie_4_14min54.mp";
  "112_refutation_T_de_M_ete_2013_partie_5_21min57.mp";
  "113_refutation_T_de_M_ete_2013_partie_6_21min51.mp";
  "114_refutation_T_de_M_ete_2013_partie_7_17min02.mp";
  "115_VIIIe_D_Ap_Pent_7_8_2011_a_La_Boutouere_Mayenne_21min57.mp";
  "116_XVIe_Dimanche_apres_la_Pentecote_ND_de_lEpine_20_09_2020_22min14.mp";
  "117_Christ_Roi_25_10_2020_ND_de_lEpine_45min29.mp";
  "118_Presentation_des_ouvrages_de_labbe_Zins_video_001_52min.mp";
  "119_Presentation_des_ouvrages_de_labbe_Zins_video_002_1h.mp";
  "120_Presentation_des_ouvrages_de_labbe_Zins_video_003_55min.mp";
  "121_Presentation_des_ouvrages_de_labbe_Zins_video_004_42min.mp"
  
  ];;

let n1 = List.length base1 ;;

let main_list = String.concat "\n" base1 ;; 




let write1 x =  
    let idx = string_of_int(int_of_string(String.sub x 0 3)) in 
    "wget -c http://larchange.org/audio/"^x^"3\n"^
    "ffmpeg -loop 1 -i STP.jpg -i "^x^"3 -acodec copy -vcodec libx265 -shortest "^x^"4\n"^
    "rm "^x^"3\n"^
    "echo $'\\n\\n\\n\\n\\n\\n Step "^idx^" of 121 finished\\n\\n\\n\\n\\n\\n'" ;; 

let home = Sys.getenv "HOME" ;;    
let ap_for_main_script = Absolute_path.of_string 
    (home^"/Teuliou/Bash_scripts/Convert_audio_to_video/audiotovideo.sh");;    

let main_script = String.concat "\n\n\n" (Image.image write1 base1) ;;      

let fill_main_script () =
   Io.overwrite_with ap_for_main_script main_script ;;

fill_main_script () ;;


let base2 = Int_range.index_everything base1;;
let (pre_part1,remains1) = List.partition (fun (j,x)->j<=40) base2;;
let (pre_part2,pre_part3) = List.partition (fun (j,x)->j<=80) remains1;;
let part1 = Image.image (fun (_,s)->s^"4") pre_part1;;
let part2 = Image.image (fun (_,s)->s^"4") pre_part2;;
let part3 = Image.image (fun (_,s)->s^"4") pre_part3;;

let main_dir = (Sys.getenv "HOME")^"/Teuliou/Bash_scripts/Convert_audio_to_video/Sermons";;
let cmds1 = Image.image (
  fun x->"mv "^main_dir^"/"^x^" "^main_dir^"_1_a_40/"
) part1 ;;
let cmds2 = Image.image (
  fun x->"mv "^main_dir^"/"^x^" "^main_dir^"_41_a_80/"
) part2 ;;
let cmds3 = Image.image (
  fun x->"mv "^main_dir^"/"^x^" "^main_dir^"_81_a_117/"
) part3 ;;
let cmds = cmds1 @ cmds2 @ cmds3 ;;





(************************************************************************************************************************
Snippet  9 : Removing misinterpreted characters from a freshly OCR-ed doc
************************************************************************************************************************)
open Needed_values ;;

let home = Sys.getenv "HOME" ;;

let dirname = "Lossky";;
let num_of_pages = 196 ;;    

let partial_texts = Int_range.scale (fun k->
    let sk = string_of_int k in 
    let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
    let prelude="% Beginning of page "^sk^"\n"
    and postlude="\n% End of page "^sk in 
    prelude^(rf fn)^postlude)  1 num_of_pages ;;
      
let full_ap = Absolute_path.create_file_if_absent  
(home^"/Downloads/"^dirname^"/full.txt");;  
let tex_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/pre_vladimir_lossky.txt");; 
 

let full_text = "\n"^(String.concat "\n" partial_texts)^"\n" ;;


let adjusted_text = Replace_inside.replace_several_inside_text 
   ["&","\\&";
    "$","\\$";
    "#","\\#";
    "_","\\_";
    "\194\162","c";
    "\194\176","o";
    "\226\130\172","E"] full_text ;;   
 
Replace_inside.overwrite_between_markers_inside_file 
  ~overwriter:adjusted_text ("% BEGINNING MARKER","%END MARKER") tex_ap;;


let text1 = Io.read_whole_file tex_ap ;;
let u1 = Substring.occurrences_of_in "% End of page 7\n" text1 ;;
let i1 = List.hd u1 ;;
let u2 = Cull_string.interval text1 (i1-20) i1;;

Replace_inside.replace_several_inside_file
   ["\n\012\n","\n"] tex_ap ;;

(************************************************************************************************************************
Snippet  8 : Typical use of the Trim_text_between_tags module
************************************************************************************************************************)
let home = Sys.getenv "HOME" ;;
let ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/Translations/act_of_body_translated.txt");;

Trim_text_between_tags.in_file [("[i]","[/i]")] ap;;


(************************************************************************************************************************
Snippet  7 : Put fillable footnotes in a phpbb draft 
************************************************************************************************************************)
let write1 k=
  let sk = string_of_int k in 
  "[b][color=blue]("^sk^")[/color][/b]\n\n"^
  "[size=90][b][color=blue]("^sk^")[/color][/b]   [i]   [/i]   [/size]";;

let dir = (Sys.getenv "HOME")^"/Teuliou/html_files/Translations";;  
let ap =   Absolute_path.create_file_if_absent (dir^"/temp.txt") ;;

let memo = String.concat "\n\n" (Int_range.scale write1 1 5) ;;

Io.overwrite_with ap memo ;; 

(************************************************************************************************************************
Snippet  6 : Finding extremal vertices in a polytope
************************************************************************************************************************)
open Needed_values ;;


let small_n=1;;

let u1 = Cartesian.fifth_power (Int_range.range 0 small_n);;

let u2 = List.filter_map (
  fun (a1,a2,a3,a4,a5)->
      let a6 = a3+a4-a5
      and a7 = a2+a4-a5
      and a8 = a1-a4+a5 in
      if List.for_all (fun x->(x>=0)) [a6;a7;a8] 
      then Some(a1,a2,a3,a4,a5,a6,a7,a8)   
    else None
) u1 ;;


let u3 = List.tl u2 ;; 

let shadow (a1,a2,a3,a4,a5,a6,a7,a8) =
    let l = [a1;a2;a3;a4;a5;a6;a7;a8] in 
    Set_of_polys.safe_set(List.filter (fun j->List.nth l (j-1)=0) [1; 2; 3; 4; 5; 6; 7; 8]) ;;

let supporting_rel uple uple2 =
    if uple=uple2 then false else 
    Set_of_polys.is_included_in (shadow uple) (shadow uple2);;  
     
let supporters uple = List.filter (supporting_rel uple ) u3;;

let u4 = List.filter_map (
   fun uple -> let r= supporters uple in 
   if r<>[]
    then Some(uple,r)
  else None
) u3;;

let u5 = List.filter (fun uple->(supporters uple)=[]) u3;;

[[0, 0, 0, 1, 1, 0, 0, 0],
   [0, 0, 1, 0, 0, 1, 0, 0], [0, 0, 1, 1, 1, 1, 0, 0],
   [0, 1, 0, 0, 0, 0, 1, 0], [0, 1, 0, 1, 1, 0, 1, 0],
   [0, 1, 1, 0, 0, 1, 1, 0], [0, 1, 1, 0, 1, 0, 0, 1],
   [0, 1, 1, 1, 1, 1, 1, 0], [1, 0, 0, 0, 0, 0, 0, 1],
   [1, 0, 0, 1, 0, 1, 1, 0], [1, 0, 0, 1, 1, 0, 0, 1],
   [1, 0, 1, 0, 0, 1, 0, 1], [1, 0, 1, 1, 0, 2, 1, 0],
   [1, 0, 1, 1, 1, 1, 0, 1], [1, 1, 0, 0, 0, 0, 1, 1],
   [1, 1, 0, 1, 0, 1, 2, 0], [1, 1, 0, 1, 1, 0, 1, 1],
   [1, 1, 1, 0, 0, 1, 1, 1], [1, 1, 1, 0, 1, 0, 0, 2],
   [1, 1, 1, 1, 0, 2, 2, 0], [1, 1, 1, 1, 1, 1, 1, 1]]

(*

let small_n=2;;

let u1 = Cartesian.fourth_power (Ennig.ennig 0 small_n);;

let u2 = Option.filter_and_unpack (
  fun (a1,a2,a3,a5)->
      let a4 = small_n-(a1+a2+a3) 
      and a6 = small_n-(a1+a2+a5)
      and a7 = small_n-(a1+a3+a5) 
      and a8 = (2*a1+a2+a3+a5)-small_n in
      if List.for_all (fun x->(x>=0)&&(x<=small_n)) [a4;a6;a7;a8] 
      then Some(a1,a2,a3,a4,a5,a6,a7,a8)   
    else None
) u1 ;;

let u3 = List.filter (
   fun (a1,a2,a3,a4,a5,a6,a7,a8) -> 
    List.exists (fun (x,y)->x<>y) [a1,a8;a2,a7;a3,a6;a4,a5]
) u2;;

*)

(************************************************************************************************************************
Snippet  5 : Abandoned code snippet to remove paragraph containing footnotes.
It is much simpler to add html paragraph tags only when the region of text
does not contain footnotes (see the Htmlize module and snippet 3)
************************************************************************************************************************)
exception Unbalanced_html_paragraph_tags of int * int ;;
exception Nested_html_paragraphs of (int * int) * (int * int) ;;

let footnote_marker = ref " nowfeetneto ";;


let html_par_opening_tag = "<p>";;
let html_par_closing_tag = "</p>";;

let op_tag_length = (String.length html_par_opening_tag)-1 ;;
let cl_tag_length = (String.length html_par_closing_tag)-1 ;;

let detect_nested_paragraphs l=
   let temp1 = List_again.universal_delta_list l in 
   match List.find_opt (fun 
     (((i1,j1),(i2,j2)),((i3,j3),(i4,j4)))->i3<j2
   ) temp1 with 
   None -> ()
   |Some(((i1,j1),(i2,j2)),((i3,j3),(i4,j4))) ->
       raise(Nested_html_paragraphs((i2,j2),(i3,j3)));;

let locate_all_paragraphs_in_html txt=
  (* paragraphs are assumed to be non-nested *)
  let temp1 = Substring.occurrences_of_in html_par_opening_tag txt 
  and temp2 = Substring.occurrences_of_in html_par_closing_tag txt in 
  let o1 = List.length temp1 and c1 = List.length temp2 in 
  if o1<>c1 
  then raise(Unbalanced_html_paragraph_tags(o1,c1))
  else   
  let temp3 = Image.image (fun i->(i,i+op_tag_length)) temp1
  and temp4 = Image.image (fun j->(j,j+cl_tag_length)) temp2 in 
  let temp5 = List.combine temp3 temp4 in 
  let _ = detect_nested_paragraphs temp5 in 
  temp5 ;; 

let remove_paragraphs_containing_footnotes txt l= ();;
        



(************************************************************************************************************************
Snippet  4 : Mass deletion of modules 
************************************************************************************************************************)
open Needed_values;;

let u1 = ae ();;

let u2 = List.filter (fun
   (Dfn_endingless_t.J(r,s,m)) -> Dfa_subdirectory.begins_with s 
   (Dfa_subdirectory_t.SD "Text_editing")
) u1 ;;

let u3 = Image.image (fun
(Dfn_endingless_t.J(r,s,m)) -> match m with 
  (Dfa_module_t.M m0) -> m0
) u2;;

let u4 = List.filter (
  fun m0->not(List.mem m0 ["control_pdf_size";"read_russian"])
) u3;;

let u5 = Image.image (
  fun m0->(m0, bel m0)
) u4;;

let u6 = List.filter (
  fun (m0,b0)->List.for_all (fun m1->(List.mem m1 u4)) b0
) u5;;

let u7=List.rev(Image.image fst u6);;

(************************************************************************************************************************
Snippet  3 : Code to OCR-size PDF's into .txt (and later html)
************************************************************************************************************************)
open Needed_values ;;


let write1 k =
    let sk = string_of_int k in 
    "pdftoppm main.pdf p"^sk^" -png -f "^sk^" -singlefile\n"^
    "tesseract -l fra p"^sk^".png p"^sk;;

let dirname = "Pius_XII";;
let num_of_pages = 326 ;;

let ap1 = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/script.sh");;

let text1 = "\n\n\n"^(String.concat "\n" (Int_range.scale write1 1 num_of_pages))^"\n\n\n" ;;   
    
Io.overwrite_with ap1 text1;;


let partial_texts = Int_range.scale (fun k->
  let sk = string_of_int k in 
  let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
  "%\n% Page "^sk^" \n%\n"^(rf fn))  7 num_of_pages ;;


let full_ap = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/full.txt");;  

let full_text = String.concat "\n" partial_texts ;;
let full_text = Htmlize.pages partial_texts ;;

Io.overwrite_with full_ap full_text;;

let (page1,page2,ranges_for_lfm,ranges_for_fm) =
   Option.get(!(Htmlize.Private.error_handling_ref ));;

(* Re-indexed version *)

let main_list = 
  [51;149;187;189;201;203;231;249;257;261;263;265;269;271;297] ;;

let write1 j =
 let k =List.nth main_list (j-1) in 
 let sj = string_of_int j 
 and sk = string_of_int k in 
 "pdftoppm main.pdf p"^sk^" -png -f "^sj^" -singlefile\n"^
 "tesseract -l fra p"^sk^".png p"^sk;;

let dirname = "Blet_again";;
let num_of_pages = 15 ;;

let ap1 = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/script.sh");;

let text1 = "\n\n\n"^(String.concat "\n" (Int_range.scale write1 1 num_of_pages))^"\n\n\n" ;;   
 
Io.overwrite_with ap1 text1;;

let partial_texts = Int_range.scale (fun k->
  let sk = string_of_int k in 
  let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
  let announcer = "%\n% Page "^sk^" \n%\n" in 
  let uncompressed_pagetext = rf fn in 
  let pagetext = Make_paragraphs_one_lined.in_text 
  (Remove_hyphens.in_text uncompressed_pagetext) in  
  announcer^pagetext)  1 num_of_pages ;;

let full_ap = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/full.txt");;  

let full_text = String.concat "\n" partial_texts ;;
let full_text = Htmlize.pages partial_texts ;;

Io.overwrite_with full_ap full_text;;




(************************************************************************************************************************
Snippet  2 : Typical use of the Read_russian module
************************************************************************************************************************)
open Needed_values ;;

let home = Sys.getenv "HOME";;
let txt1 = rf (home^"/Downloads/temp.txt");;

let z1 = Read_russian.read txt1;;
let z2= Read_russian.prepare_dictation txt1;;

let ap1 = Absolute_path.of_string (home^"/Teuliou/LaTeX/Moullapl/archipelago.tex");;

let act () = 
Replace_inside.overwrite_between_markers_inside_file 
  ~overwriter:z2
  ("\\begin{document}","\\end{document}") ap1;;

(************************************************************************************************************************
Snippet  1 : Convert footnotes between phpBB and HTML
************************************************************************************************************************)
let peggy j =
   let sj=string_of_int j in 
   "<span id=\""^"ln"^sj^"\"><a href=\"#n"^sj^"\">("^sj^")</a></span>";;
 
 let u1 = Int_range.scale peggy 3 43;;  
 
 let u2 ="\n\n\n"^(String.concat "\n\n" u1) ^"\n\n\n";;
 
 
 let peggy j =
   let sj=string_of_int j in 
   "<div id=\""^"n"^sj^"\"><a href=\"#ln"^sj^"\">("^sj^")</a> <i> </i>  </div>";;
 
 let u1 = Int_range.scale peggy 3 43;;  
 
 let u2 ="\n\n\n"^(String.concat "\n\n" u1) ^"\n\n\n";;
 
