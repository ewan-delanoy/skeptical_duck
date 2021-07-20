(*

#use"Old_Van_der_Waerden/udwfw_environment.ml";;

*)


module Private = struct
 
  let notify_new_assignment_among_variables (x,combination_for_x) =
    let opt_expansion =(
       try Some(Udwfw_combination.expand_fully combination_for_x) with 
       _ -> None  
    ) in 
    match opt_expansion with 
    None -> ()
    |Some expansion -> Udwfw_variable.set x expansion ;;

  let react_to_new_assignment 
    (Udwfw_environment_t.L old_list) (x,combination_for_x) =
    Udwfw_environment_t.L (Image.image (
      fun (y,combination_for_y) -> 
          (y,Udwfw_combination.replace_with_in (x,combination_for_x) combination_for_y)  
     ) old_list );;  
  
  let add_new_assignment old_env assignment =
      let (Udwfw_environment_t.L new_list)= 
       react_to_new_assignment old_env assignment in 
      let _ =  notify_new_assignment_among_variables assignment in 
      Udwfw_environment_t.L (assignment :: new_list) ;;   
  
  let check (Udwfw_environment_t.L assignments) = 
      List.filter (
            fun (x,combination_for_x) ->
                let x_content = Udwfw_variable.get x 
                and (Udwfw_combination_t.C partition_for_x) = combination_for_x in 
                let temp1 = Image.image (
                  fun (core,translation) -> 
                    (translation,Udwfw_variable.get core)
                ) partition_for_x in 
                x_content <> Udw_common.reconstruct temp1
          ) assignments ;;
  
  let get (Udwfw_environment_t.L assignments) x =  
     match List.assoc_opt x assignments with 
     Some(y)->y 
    |None -> Udwfw_combination.constructor [x,[]];;  


  exception Empty_result_in_ext_part ;;

let oint = Total_ordering.for_integers ;;  

let offset_of = Strung.insert_repetitive_offset_on_the_left ' ';;

let describe_new_extraction (name_for_x,obstr,list_for_c)=
 (offset_of 7 (Udwfw_nonempty_index.to_string name_for_x))^
 " partitioned wrt "^
 (offset_of 7 (Strung.of_intlist obstr))^
 " : "^ 
 (Udwfw_combination.to_string (Udwfw_combination_t.C list_for_c))^"\n" 

let add_new_extraction old_extr_env triple=
 let msg = describe_new_extraction triple in 
 let _= (print_string msg;flush stdout) in
 ( triple :: old_extr_env);;
    
let extract (old_env,old_extr_env) obstruction (complement,name_for_x) =
   (*
      we take extra care not to create new names for already named variables
   *)  
   let x = Udwfw_variable.get name_for_x 
   and effective_obstruction = Ordered.setminus oint obstruction complement 
   and omerge = Ordered.merge oint in 
   let ((core_for_a,a),(core_for_b,b)) = 
       Udw_common.extended_partition effective_obstruction x in 
   if b= [] 
   then ((old_env,old_extr_env),([],None))    
   else 
   if a = []
   then (if core_for_b=[] then ((old_env,old_extr_env),(complement,Some name_for_x)) else 
         let zb = Option.unpack(Udwfw_indexed_namer.register_if_necessary b) in 
         let new_env = 
          add_new_assignment old_env (name_for_x,
          Udwfw_combination.constructor [zb,core_for_b]) 
         and new_extr_env = add_new_extraction
          old_extr_env (name_for_x,effective_obstruction,[zb,core_for_b]) in 
         ((new_env,new_extr_env),(omerge complement core_for_b,Some zb))
        )
   else       
   let za = Option.unpack(Udwfw_indexed_namer.register_if_necessary  a) in
   let zb = Option.unpack(Udwfw_indexed_namer.register_if_necessary b) in 
   let new_env = 
     add_new_assignment old_env (name_for_x,
          Udwfw_combination.constructor [za,core_for_a;zb,core_for_b]) 
   and new_extr_env = add_new_extraction
   old_extr_env (name_for_x,effective_obstruction,[za,core_for_a;zb,core_for_b]) in 
   ((new_env,new_extr_env),(omerge complement core_for_b,Some zb)) ;;   

  let rec extract_several old_envpair (treated,to_be_treated) old_pair =
    match to_be_treated with 
    [] -> (old_envpair,(old_pair,None))
    | obstruction :: other_obstructions ->
      let (new_envpair,(complement_for_y,opt_y)) = 
      extract old_envpair obstruction old_pair in 
     (match opt_y with 
      None -> (old_envpair,(old_pair,Some(treated,obstruction)))
     |Some(name_for_y) ->
      extract_several new_envpair (obstruction::treated,other_obstructions) (complement_for_y,name_for_y)
     )
     ;;

  let prepare_element_for_homogeneous_translation 
    old_envpair (core,translation,bound) =
      let obstructions =
      Udwfw_current.minimal_obstructions_corresponding_to_above 
        bound
        (Set_of_integers.safe_set translation) in
     extract_several old_envpair ([],obstructions) 
       (translation,core)  
    ;;

    let rec prepare_elements_for_homogeneous_translation 
      (treated,to_be_treated) =
      match to_be_treated with 
      [] -> treated 
      | elt :: other_elts ->
        let (new_envpair,_) =
        prepare_element_for_homogeneous_translation  
         treated elt in
        prepare_elements_for_homogeneous_translation 
          (new_envpair,other_elts) ;;

  
  let prepare_homogeneous_translation old_envpair
    (Udwfw_combination_t.C l) translation =
   let temp1 = Image.image (fun (core1,translation1) ->
     (core1,Ordered.merge oint translation1 translation,List.hd translation)   
   ) l  in 
   prepare_elements_for_homogeneous_translation 
     (old_envpair,temp1) ;;
    

   let main_ref = ref (Udwfw_environment_t.L []) ;; 

   let main_extraction_ref = ref ([]) ;; 
  
  end ;;     
  
  
let add_new_assignment assignment =
    let new_env = Private.add_new_assignment (!Private.main_ref) assignment in 
    let _ = (Private.main_ref:=new_env) in new_env ;;
  
let check () = Private.check  (!Private.main_ref) ;; 

let extract  (complement,name_for_x) obstruction=
  let (new_envpair,answer) = 
   Private.extract (!Private.main_ref,!Private.main_extraction_ref) 
   obstruction (complement,name_for_x) in 
  let _ = (
    Private.main_ref:=(fst new_envpair);
    Private.main_extraction_ref:=(snd new_envpair);  
  ) in answer ;;

let extract_several  (complement,name_for_x) obstructions=
  let (new_envpair,answer) = 
   Private.extract_several (!Private.main_ref,!Private.main_extraction_ref)  
   obstructions (complement,name_for_x) in 
   let _ = (
    Private.main_ref:=(fst new_envpair);
    Private.main_extraction_ref:=(snd new_envpair);  
  ) in answer ;;



let get x = Private.get (!Private.main_ref) x ;;

let homogeneous_translation x translation=
   let combination_for_x = get x in 
   let new_envpair = 
    Private.prepare_homogeneous_translation 
    (!Private.main_ref,!Private.main_extraction_ref)  
      combination_for_x translation in 
   let _ = (
    Private.main_ref:=(fst new_envpair);
    Private.main_extraction_ref:=(snd new_envpair);  
   ) in 
  Udwfw_combination.homogeneous_translation (get x) translation;;

let reset () =
  (
  Private.main_ref := (Udwfw_environment_t.L[]) ;
  Private.main_extraction_ref := [] 
  );;