(*

#use"lib/Text_editing/Pgfmath_writer/pgfmath_environment.ml";;

*)

open Pgfmath_types ;;

exception Unassigned_variables of variable_name list;;

module type INTERFACE = sig 

type t     

val constructor :
      string -> Absolute_path.t ->
      (variable_name * variable_definition) list ->
      (string*(variable_name * variable_name)) list  ->
      t

val write : t -> unit       

end ;;    


module Private = struct 

module Unconstrained = struct 

type t = { 
   letter_name : string ;
   receiver : Absolute_path.t ;
   values :  (variable_name * (variable_definition * float)) list ;
   points : (string * unnamed_point) list ;
}  ;; 

let empty_one lname rcvr= {
    letter_name = lname ;
    receiver = rcvr ;
    values = [] ;
    points = [] ;
} ;; 

let get_value env vn = 
    match List.assoc_opt vn env.values with 
       None -> raise(Unassigned_variables [vn])
    |Some (_,old_val) -> old_val ;;

let get_two_values env vn1 vn2 = 
    let opts = Image.image (fun vn->(vn,List.assoc_opt vn env.values)) [vn1;vn2] in 
    let bad_opts = List.filter_map (fun (vn,opt)->if opt=None then Some vn else None) opts in
    if bad_opts<>[]
    then raise(Unassigned_variables bad_opts)
    else
    let vals = Image.image  (fun (_vn,opt)->snd(Option.get opt)) opts in 
    (List.nth vals 0,List.nth vals 1) ;;

let get_several_values env lv = 
    let opts = Image.image (fun vn->(vn,List.assoc_opt vn env.values)) lv in 
    let bad_opts = List.filter_map (fun (vn,opt)->if opt=None then Some vn else None) opts in
    if bad_opts<>[]
    then raise(Unassigned_variables bad_opts)
    else
    Image.image  (fun (_vn,opt)->snd(Option.get opt)) opts ;;

let new_value  env defn = match defn with 
    Leaf(fl) -> fl
   |Sum (lv) ->
       let fls = get_several_values env lv in 
       List.fold_left (+.) 0. fls 
   |Substract(vn1,vn2) ->
       let (fl1,fl2) = get_two_values env vn1 vn2 in 
       fl1 -. fl2 
   |Dot(lamb,vn) -> 
       let fl = get_value env vn in 
        lamb *. fl
      ;;

let add_value_definition env (vn,vdefn) =
    let new_val = new_value env vdefn  in
    {
        env with 
        values = (vn,(vdefn,new_val)):: env.values ;
    };;

let add_value_definitions env pairs = List.fold_left add_value_definition env pairs ;;

let add_point env (point_name,(vn1,vn2)) =
    let (fl1,fl2) = get_two_values env vn1 vn2 in 
     {
        env with 
        points = (point_name,UP((vn1,vn2),(fl1,fl2))):: env.points ;
    };;

let add_points env points = List.fold_left add_point env points ;;

let constructor lname rcvr value_defns points = 
    let env1 = add_value_definitions (empty_one lname rcvr) value_defns in 
    add_points env1 points;;

let write_float fl = Printf.sprintf "%.2f" fl ;;

let write_variable_name  (VN vn) =  "\\"^vn ;;

let write_value_defn  defn = match defn with 
    Leaf(fl) -> write_float fl
   |Sum lv -> String.concat "+" (Image.image write_variable_name lv) 
   |Substract(vn1,vn2) -> (write_variable_name vn1) ^ "-" ^ (write_variable_name vn2)
   |Dot(lamb,vn) -> (write_float lamb) ^ "*" ^ (write_variable_name vn);;


let write_value_definition (vn,(vdefn,new_val)) = 
    "\\pgfmathsetmacro{"^(write_variable_name vn)^"}{"^(write_value_defn vdefn)^"} % "^(write_float new_val) ;;

let write_value_definitions l =
    String.concat "\n" (List.rev_map write_value_definition l) ;; 

let write_point (point_name,UP((vn1,vn2),(fl1,fl2))) = 
    "\\coordinate ("^point_name^") at ("
    ^(write_variable_name vn1)^","
    ^(write_variable_name vn2)^"); % ("
    ^(write_float fl1)^","     
    ^(write_float fl2)^")" ;;

let write_points l =
    String.concat "\n" (List.rev_map write_point l) ;; 


    
let write_environment env = String.concat "\n\n\n"
    ["";(write_value_definitions env.values);(write_points env.points);""]    ;;

let write env =
    let text = write_environment env 
    and markers = (
        "%OCaml generated code for letter "^env.letter_name^" starts here",
        "%OCaml generated code for letter "^env.letter_name^" ends here"
    ) in 
    Replace_inside.overwrite_between_markers_inside_file
     ~overwriter:text markers env.receiver ;;
      



end ;; 
   
module Constrained = (Unconstrained:INTERFACE) ;;

end ;;

include Private.Constrained ;;