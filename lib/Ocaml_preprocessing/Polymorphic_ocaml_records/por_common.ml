(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_common.ml";;

*)



exception Get_field_exn of string ;;
exception Get_subclass_exn of string ;; 
exception Check_inclusion_exn of (string list) * (string list) * (string list) ;;
    
let check_inclusion small_list large_list =
  let temp1 = List.filter (fun s->not(List.mem s large_list)) small_list in 
  if temp1 <> []
  then raise(Check_inclusion_exn(temp1,small_list,large_list))
  else () ;;  
  
  let indexed_varname_for_field (j,fd)=
  "v"^(string_of_int j)^"_"^(fd.Por_types.var_name) ;;


let extender_name (before_ext,after_ext) = (String.uncapitalize_ascii before_ext)^"_to_"^(String.uncapitalize_ascii after_ext) ;;

let extensions_from_different_sources por =
   let lfs = Total_ordering.lex_for_strings in 
  Ordered.sort (Total_ordering.product lfs lfs)
  (por.Por_space_t.extensions @
  (Image.image (fun (x,y)->(y,x)) por.Por_space_t.designated_parents)) ;; 

let get_field por fd_name =
  match List.find_opt (fun fd->fd.Por_types.field_name = fd_name)
          por.Por_space_t.fields with 
    Some answer -> answer 
  | None -> raise ( Get_field_exn(fd_name)) ;;    

    
let get_subclass por inst_name =
  match List.find_opt (fun fd->fd.Por_subclass_t.subclass_name = inst_name)
            por.Por_space_t.subclasses with 
    Some answer -> answer 
  | None -> raise ( Get_subclass_exn(inst_name)) ;;    
    