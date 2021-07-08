(*

#use"Van_der_Waerden/Width_up_to_four/vdw_environment.ml";;

*)


module Private = struct

  let react_to_new_assignment (Vdw_environment_t.L old_list) (x,combination_for_x) =
    Vdw_environment_t.L (Image.image (
      fun (y,combination_for_y) -> 
          (y,Vdw_combination.replace_with_in (x,combination_for_x) combination_for_y)  
     ) old_list );;  
  
  let add_new_assignment old_env assignment =
      let (Vdw_environment_t.L new_list)= react_to_new_assignment old_env assignment in 
      Vdw_environment_t.L (assignment :: new_list) ;;   
  
  let check (Vdw_environment_t.L assignments) = 
      List.filter (
            fun (x,combination_for_x) ->
                let x_content = Vdw_variable.get x 
                and (Vdw_combination_t.C partition_for_x) = combination_for_x in 
                let temp1 = Image.image (
                  fun (translation,core) -> 
                    (translation,Vdw_variable.get core)
                ) partition_for_x in 
                x_content <> Vdw_common.reconstruct temp1
          ) assignments ;;
  
  let get (Vdw_environment_t.L assignments) x =  
     List.assoc x assignments;;  


  exception Empty_result_in_ext_part ;;

let oint = Vdw_common.oint ;;  

let extract old_env obstruction (complement,name_for_x) =
   (*
      we take extra care not to create new names for already named variables
   *)  
   let x = Vdw_variable.get name_for_x 
   and effective_obstruction = Ordered.setminus oint obstruction complement 
   and omerge = Ordered.merge oint in 
   let ((core_for_a,a),(core_for_b,b)) = 
       Vdw_common.extended_partition effective_obstruction x in 
   if b= [] 
   then (old_env,([],Vdw_variable.empty_set()))    
   else 
   if a = []
   then (if core_for_b=[] then (old_env,(complement,name_for_x)) else 
         let zb = Vdw_indexed_namer.register_new b in 
         let new_env = 
          add_new_assignment old_env (name_for_x,
          Vdw_combination.constructor [core_for_b,zb]) in 
         (new_env,(omerge complement core_for_b,zb))
        )
   else       
   let za = Vdw_indexed_namer.register_new  a in
   let zb = Vdw_indexed_namer.register_new b in 
   let new_env = 
     add_new_assignment old_env (name_for_x,
          Vdw_combination.constructor [core_for_a,za;core_for_b,zb]) in 
   (new_env,(omerge complement core_for_b,zb)) ;;   

    let obstructions_at_point m =
      Ennig.doyle (fun t->[m-(2*t);m-t]) 1 4 ;;

   let main_ref = ref (Vdw_environment_t.L []) ;; 
  
  end ;;     
  
  
let add_new_assignment assignment =
    let new_env = Private.add_new_assignment (!Private.main_ref) assignment in 
    let _ = (Private.main_ref:=new_env) in new_env ;;
  
let check () = Private.check  (!Private.main_ref) ;; 

let extract  (complement,name_for_x) obstruction=
  let (new_env,answer) = 
   Private.extract (!Private.main_ref) 
   obstruction (complement,name_for_x) in 
  let _ = (Private.main_ref:=new_env) in answer ;;

let fold_extract pair obstructions =
    List.fold_left extract pair obstructions ;;

let fold_extract_at_point pair m =
  fold_extract pair (Private.obstructions_at_point m) ;;

let get x = Private.get (!Private.main_ref) x ;;

let homogeneous_translation x l=
    Vdw_combination.homogeneous_translation (get x) l;;