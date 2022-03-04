(*

#use"Ocaml_preprocessing/por_common.ml";;

*)



exception Get_field_exn of string ;;
exception Get_instance_exn of string ;; 
exception Check_inclusion_exn of (string list) * (string list) * (string list) ;;
    

let get_field por fd_name =
  match Option.seek (fun fd->fd.Polymorphic_ocaml_record_t.field_name = fd_name)
          por.Polymorphic_ocaml_record_t.fields with 
    Some answer -> answer 
  | None -> raise ( Get_field_exn(fd_name)) ;;    
    
     
    
let get_instance por inst_name =
  match Option.seek (fun fd->fd.Polymorphic_ocaml_record_t.instance_name = inst_name)
            por.Polymorphic_ocaml_record_t.instances with 
    Some answer -> answer 
  | None -> raise ( Get_instance_exn(inst_name)) ;;    
    
     
let check_inclusion small_list large_list =
    let temp1 = List.filter (fun s->not(List.mem s large_list)) small_list in 
    if temp1 <> []
    then raise(Check_inclusion_exn(temp1,small_list,large_list))
    else () ;;  
    
     