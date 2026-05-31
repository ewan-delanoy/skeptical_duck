(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_battery_of_analizers.ml";;

*)

open Jvng_types ;;

exception Untreated of Jvng_duplicated_name.t ;;
exception Incomplete of Jvng_duplicated_name.t ;;

module Private = struct 
let get_local_analizer battery dname =  
  let name = Jvng_duplicated_name.name dname in
  match Jvsp_util.token_type_sequence_from_codes_in_production_names_opt name with 
  Some toktypes -> Jvng_local_analizer.one_level_above_molecular toktypes 
  | None ->
  match List.assoc_opt dname battery.deciders_for_optionals_or_stars with 
        None -> raise(Untreated(dname))
     |(Some analizer) -> analizer;;

end ;;     

let decide battery dname strm=
  let analizer = Private.get_local_analizer battery dname in 
  match (Jvng_local_analizer.use analizer strm.remaining_list) with 
     None -> raise(Incomplete(dname))
    |Some answer -> bool_of_string (Jvng_duplicated_name.name answer) ;;    

let choose battery dis_name strm= 
  match List.assoc_opt dis_name battery.choosers_for_disjunctions with 
  None -> raise(Untreated(dis_name))
  |Some(analizer) ->match (Jvng_local_analizer.use analizer strm.remaining_list) with 
     None -> raise(Incomplete(dis_name))
    |Some answer -> answer ;;    


