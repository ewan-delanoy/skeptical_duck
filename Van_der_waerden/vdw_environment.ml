(*

#use"Van_der_Waerden/vdw_environment.ml";;

*)


module Private = struct 

let expand env var = 
      let rp = env.Vdw_environment_t.headquarters
      and opt_fan = List.assoc_opt var env.Vdw_environment_t.variables in 
      match opt_fan with 
      None -> []
      |Some fan -> Vdw_fan.expand rp fan;;

let register_fan_if_necessary old_env fan = 
   match Option.seek (fun (_,fan2)->fan2=fan) old_env.Vdw_environment_t.variables with 
    Some(var,_) -> (old_env,var)
   |None -> let old_vars = old_env.Vdw_environment_t.variables in 
            let m = List.length old_vars in 
            let new_var = Vdw_variable_t.V(m+1) in 
            let new_env = {
                 old_env with 
                 Vdw_environment_t.variables = 
                 (Vdw_variable_t.V(m+1),fan)  :: old_vars ;
            } in 
            (new_env,new_var) ;;


let define_partition old_env var criterion=
   let old_rp = old_env.Vdw_environment_t.headquarters
   and old_fan = List.assoc var old_env.Vdw_environment_t.variables  
   and old_vars = old_env.Vdw_environment_t.variables in 
   let (new_rp,changes) = Vdw_fan.prepare_partition (old_rp,old_fan) criterion in 
   let new_vars = Image.image (
      fun (var,fan)->(var,Vdw_fan.apply_changes changes fan)
   ) old_vars in 
   let new_fan = List.assoc var new_vars in 
   let (fan1,fan2)= Vdw_fan.remember_partition (new_rp,new_fan) criterion in
   let env2 = 
        {
         Vdw_environment_t.headquarters = new_rp  ;
         variables = new_vars ;
   } in
   let (env3,var1) =  register_fan_if_necessary env2 fan1 in 
   let (env4,var2) =  register_fan_if_necessary env3 fan2 in 
   (env4,(var1,var2))
   ;;


let define_translate old_env var translation =
    let (Vdw_variable_t.V v) = var in 
    if v < 1 then (old_env,Vdw_variable_t.V 0) else 
    let old_fan = List.assoc var old_env.Vdw_environment_t.variables  
    and old_vars = old_env.Vdw_environment_t.variables in 
    let old_length = List.length old_vars 
    and new_fan = Vdw_fan.translate old_fan  translation in 
    let new_var = Vdw_variable_t.V(old_length+1) in 
    let new_vars = (new_var,new_fan) :: old_vars in
    ({
      old_env with
      Vdw_environment_t.variables = new_vars ;
     },new_var);;

end ;;   

let define_partition env_ref var criterion =
    let (new_env,(var1,var2)) = Private.define_partition (!env_ref) var criterion in 
    let _ = (env_ref:=new_env) in 
    (var1,var2) ;;

let define_translate env_ref var translation =
      let (new_env,new_var) = Private.define_translate (!env_ref) var translation in 
      let _ = (env_ref:=new_env) in 
      new_var ;;    

let start ll=
   {
      Vdw_environment_t.headquarters = Vdw_repeatedly_partitionable.start ll  ;
      variables = [Vdw_variable_t.V 1,Vdw_fan_t.F [Vdw_part_t.P 1,[]]] ;
   };;
   