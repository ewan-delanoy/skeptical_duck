(*

#use"Van_der_Waerden/vdw_environment.ml";;

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
                  fun (translation,core) -> (translation,Vdw_variable.get core)
                ) partition_for_x in 
                x_content <> Vdw_common.reconstruct temp1
          ) assignments ;;
  
  let main_ref = ref (Vdw_environment_t.L []) ;;
  
  end ;;     
  
  
let add_new_assignment assignment =
    let new_env = Private.add_new_assignment (!Private.main_ref) assignment in 
    let _ = (Private.main_ref:=new_env) in new_env ;;
  
let check () = Private.check  (!Private.main_ref) ;; 