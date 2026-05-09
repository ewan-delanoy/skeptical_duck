(*

#use"lib/Java_analysis/jvsp_parser.ml";;

*)

module Private = struct
let helper_for_star (Jvsp_types.Parser f) l =
    let rec helper = (
      fun (treated,idx,n) ->
        if idx>n then Some(List.rev treated,idx) else 
        match f l idx with 
        None ->  Some(List.rev treated,idx)
        | (Some (part,new_idx)) -> helper (part::treated,new_idx,n)
    ) in 
    helper ;;

let star prsr = Jvsp_types.Parser (fun tokens idx ->helper_for_star prsr tokens ([],idx,List.length tokens)) ;;

end ;;

let star = Private.star ;;
