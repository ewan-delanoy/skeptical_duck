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
  "v"^(string_of_int j)^"_"^(fd.Por_field_t.var_name) ;;

let field_order = ((fun fld1 fld2 ->
    let trial1 = Total_ordering.lex_for_strings 
       fld1.Por_field_t.field_name fld2.Por_field_t.field_name in 
    if trial1<> Total_ordering_result_t.Equal then trial1 else
       Total_ordering.standard fld1 fld2         
  ) : Por_field_t.t Total_ordering_t.t);;

let all_fields por =
  let to_be_flattened = Image.image (
    fun scl -> scl.Por_subclass_t.subclass_fields
  ) por.Por_space_t.subclasses in 
  let unordered_fields = List.flatten to_be_flattened in 
      Ordered.sort field_order unordered_fields ;; 


let extender_name (before_ext,after_ext) = (String.uncapitalize_ascii before_ext)^"_to_"^(String.uncapitalize_ascii after_ext) ;;

let all_constructors por = 
  List.filter_map (
    fun scl ->
      if scl.Por_subclass_t.has_constructor 
      then Some scl.Por_subclass_t.subclass_name
      else None
) por.Por_space_t.subclasses ;; 

let all_nonparenting_extensions por = 
  List.flatten (Image.image (
    fun scl ->
      let extending_one = scl.Por_subclass_t.subclass_name in 
      Image.image (fun 
        extended_one->(extended_one,extending_one)
      ) scl.Por_subclass_t.extensions_leading_here
  ) por.Por_space_t.subclasses) ;;

let all_parentings por = 
  List.filter_map (
    fun scl ->
      match scl.Por_subclass_t.parent with 
      Some parent_name -> Some (scl.Por_subclass_t.subclass_name,parent_name)
      |None -> None
) por.Por_space_t.subclasses ;;  

let all_restrictions por = 
    List.filter_map (
      fun scl ->
        if scl.Por_subclass_t.has_restriction 
        then Some scl.Por_subclass_t.subclass_name
        else None
  ) por.Por_space_t.subclasses ;;  


let extensions_from_different_sources por =
   let lfs = Total_ordering.lex_for_strings in 
  Ordered.sort (Total_ordering.product lfs lfs)
  ((all_nonparenting_extensions por) @
  (Image.image (fun (x,y)->(y,x)) 
  (all_parentings por))) ;; 

let get_field por fd_name = 
  let fields = all_fields por in 
  match List.find_opt (fun fd->fd.Por_field_t.field_name = fd_name) fields with 
    Some answer -> answer 
  | None -> raise ( Get_field_exn(fd_name)) ;;    

let get_subclass_opt por scl_name =
    List.find_opt (fun fd->fd.Por_subclass_t.subclass_name = scl_name)
              por.Por_space_t.subclasses ;; 
  
let get_subclass por scl_name =
  match get_subclass_opt por scl_name with 
    Some answer -> answer 
  | None -> raise ( Get_subclass_exn(scl_name)) ;;    
    
let link_extension 
  ~parent ~incomplete_child = 
   let old_fields = parent.Por_subclass_t.subclass_fields 
   and new_fields = incomplete_child.Por_subclass_t.subclass_fields in 
  {
    incomplete_child with 
    Por_subclass_t.subclass_fields = old_fields @ new_fields 
  } ;; 

let pusher_for_possible_linkings_exhaustion 
  (_,complete_subclasses,incomplete_subclasses) =
  match List.find_map (
     fun (parent_name,extension) ->
       Option.map (fun parent_scl->(parent_scl,parent_name,extension))
       (List.find_opt (fun scl->scl.Por_subclass_t.subclass_name = parent_name) complete_subclasses)
  ) incomplete_subclasses with 
  None -> (true,complete_subclasses,incomplete_subclasses) 
  |Some(parent_scl,_parent_name,extension)->
    let child_name = extension.Por_subclass_t.subclass_name in 
    let new_complete_subclass = link_extension 
    ~parent:parent_scl ~incomplete_child:extension 
    and fewer_incomplete_subclasses = List.filter (
      fun (_,inc_sc) ->inc_sc.Por_subclass_t.subclass_name <> child_name
    ) incomplete_subclasses in 
    (false,new_complete_subclass::complete_subclasses,fewer_incomplete_subclasses) ;; 

let rec iterator_for_possible_linkings_exhaustion walker =
  let (check_finished,complete_subclasses,incomplete_subclasses) = walker in 
  if check_finished 
  then (List.rev complete_subclasses,incomplete_subclasses)  
  else iterator_for_possible_linkings_exhaustion (pusher_for_possible_linkings_exhaustion walker);;

let exhaust_possible_linkings 
  ~complete:complete_subclasses 
   ~incomplete:incomplete_subclasses =
  iterator_for_possible_linkings_exhaustion 
  (false,List.rev complete_subclasses,incomplete_subclasses) ;;

  


let space_has_dependencies _por =
  false ;; (* for now *)
(*  
  List.exists subclass_has_dependencies  por.Por_space_t.subclasses ;; 
*)

let see_subclasses por =
  Image.image (fun scl->scl.Por_subclass_t.subclass_name) por.Por_space_t.subclasses  ;; 
