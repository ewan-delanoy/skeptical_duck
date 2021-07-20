(*

#use"Van_der_Waerden/vdw_common.ml";;

*)

   
let oint = Total_ordering.for_integers ;;   
let ointlist = Total_ordering.silex_compare Total_ordering.for_integers ;;
  
let extract_core_and_simplify ll = 
  if ll = [] then ([],[]) else 
  let core = Ordered.fold_intersect oint ll in 
  (core,Image.image (fun l->Ordered.setminus oint l core) ll) ;;          
  
let diameter soi =
  if List.length(soi)<2 then 0 else 
  (List.hd(List.rev soi)) - (List.hd soi) + 1  ;;  
      
let look_for_arithmetic_progressions_in_with_width_equal_to
         soi width=
  if List.length(soi)<3 then [] else 
  let temp1 = Image.image (fun x->[x;x+width;x+2*width]) soi in 
  List.filter (fun obstruction ->
    Ordered.is_included_in oint obstruction soi) temp1 ;;  


