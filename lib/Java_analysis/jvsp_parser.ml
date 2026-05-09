(*

#use"lib/Java_analysis/jvsp_parser.ml";;

*)

module Private = struct

let apply (Jvsp_types.Parser f) l idx= f l idx ;;

let helper_for_star prsr l n=
    let rec helper = (
      fun (treated,idx) ->
        if idx>n then Some(List.rev treated,idx) else 
        match apply prsr l idx with 
        None ->  Some(List.rev treated,idx)
        | (Some (part,new_idx)) -> helper (part::treated,new_idx)
    ) in 
    helper ;;

let star prsr = Jvsp_types.Parser (fun tokens idx ->helper_for_star prsr tokens (List.length tokens) ([],idx)) ;;

let map f prsr = Jvsp_types.Parser (fun tokens idx ->
  match (apply prsr tokens idx) with 
  None -> None 
  |(Some(data,new_idx)) -> Some(f data,new_idx)  
) ;;



let helper_for_molecular tokens n =
  let rec helper =(fun (remaining_expectations,treated,idx) ->
    match remaining_expectations with 
  [] -> Some(List.rev treated,idx)
  |tok_type :: other_expectations -> 
      if idx>n then None else 
      let postok = List.nth tokens (idx-1) in 
      let tok = postok.Jvsp_types.tok in 
      let tok_type2 = Jvsp_util.get_token_type(tok) in  
      if List.mem tok_type2 Jvsp_util.passive_token_types 
      then  helper (remaining_expectations,treated,idx+1)
      else
      if tok_type2 <> tok_type 
      then None 
      else let new_treated = (
            if Jvsp_util.has_variable_content tok_type 
            then (Jvsp_util.token_to_string tok) :: treated
            else treated    
           ) in 
           helper (other_expectations,new_treated,idx+1)) in
   helper ;;
      
let molecular molecule = Jvsp_types.Parser (fun tokens idx ->
  helper_for_molecular tokens (List.length tokens) (molecule,[],idx)) ;;     

let helper_for_dis2 prsr1 prsr2 tokens idx=
    match apply prsr1 tokens idx with 
     (Some (res1,new_idx)) -> Some((Some res1,None),new_idx)
     |None ->
    match apply prsr2 tokens idx with 
     (Some (res2,new_idx)) -> Some((None,Some res2),new_idx)
     |None ->
    None ;;

let dis2 prsr1 prsr2 = Jvsp_types.Parser (fun tokens idx ->helper_for_dis2 prsr1 prsr2 tokens idx) ;;




let helper_for_dis3 prsr1 prsr2 prsr3 tokens idx=
    match apply prsr1 tokens idx with 
     (Some (res1,new_idx)) -> Some((Some res1,None,None),new_idx)
     |None ->
    match apply prsr2 tokens idx with 
     (Some (res2,new_idx)) -> Some((None,Some res2,None),new_idx)
     |None ->
    match apply prsr3 tokens idx with 
     (Some (res3,new_idx)) -> Some((None,None,Some res3),new_idx)
     |None ->
    None ;;

let dis3 prsr1 prsr2 prsr3 = Jvsp_types.Parser (fun tokens idx ->helper_for_dis3 prsr1 prsr2 prsr3 tokens idx) ;;


let helper_for_dis4 prsr1 prsr2 prsr3 prsr4 tokens idx=
    match apply prsr1 tokens idx with 
     (Some (res1,new_idx)) -> Some((Some res1,None,None,None),new_idx)
     |None ->
    match apply prsr2 tokens idx with 
     (Some (res2,new_idx)) -> Some((None,Some res2,None,None),new_idx)
     |None ->
    match apply prsr3 tokens idx with 
     (Some (res3,new_idx)) -> Some((None,None,Some res3,None),new_idx)
     |None ->
    match apply prsr4 tokens idx with 
     (Some (res4,new_idx)) -> Some((None,None,None,Some res4),new_idx)
     |None ->
    None ;;

let dis4 prsr1 prsr2 prsr3 prsr4 = Jvsp_types.Parser (fun tokens idx ->helper_for_dis4 prsr1 prsr2 prsr3 prsr4 tokens idx) ;;


let helper_for_dis5 prsr1 prsr2 prsr3 prsr4 prsr5 tokens idx=
    match apply prsr1 tokens idx with 
     (Some (res1,new_idx)) -> Some((Some res1,None,None,None,None),new_idx)
     |None ->
    match apply prsr2 tokens idx with 
     (Some (res2,new_idx)) -> Some((None,Some res2,None,None,None),new_idx)
     |None ->
    match apply prsr3 tokens idx with 
     (Some (res3,new_idx)) -> Some((None,None,Some res3,None,None),new_idx)
     |None ->
    match apply prsr4 tokens idx with 
     (Some (res4,new_idx)) -> Some((None,None,None,Some res4,None),new_idx)
     |None ->
    match apply prsr5 tokens idx with 
     (Some (res5,new_idx)) -> Some((None,None,None,None,Some res5),new_idx)
     |None ->
    None ;;

let dis5 prsr1 prsr2 prsr3 prsr4 prsr5 = Jvsp_types.Parser (fun tokens idx ->helper_for_dis5 prsr1 prsr2 prsr3 prsr4 prsr5 tokens idx) ;;


let helper_for_dis6 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 tokens idx=
    match apply prsr1 tokens idx with 
     (Some (res1,new_idx)) -> Some((Some res1,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr2 tokens idx with 
     (Some (res2,new_idx)) -> Some((None,Some res2,None,None,None,None),new_idx)
     |None ->
    match apply prsr3 tokens idx with 
     (Some (res3,new_idx)) -> Some((None,None,Some res3,None,None,None),new_idx)
     |None ->
    match apply prsr4 tokens idx with 
     (Some (res4,new_idx)) -> Some((None,None,None,Some res4,None,None),new_idx)
     |None ->
    match apply prsr5 tokens idx with 
     (Some (res5,new_idx)) -> Some((None,None,None,None,Some res5,None),new_idx)
     |None ->
    match apply prsr6 tokens idx with 
     (Some (res6,new_idx)) -> Some((None,None,None,None,None,Some res6),new_idx)
     |None ->
    None ;;

let dis6 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 = Jvsp_types.Parser (fun tokens idx ->helper_for_dis6 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 tokens idx) ;;


let helper_for_dis7 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 tokens idx=
    match apply prsr1 tokens idx with 
     (Some (res1,new_idx)) -> Some((Some res1,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr2 tokens idx with 
     (Some (res2,new_idx)) -> Some((None,Some res2,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr3 tokens idx with 
     (Some (res3,new_idx)) -> Some((None,None,Some res3,None,None,None,None),new_idx)
     |None ->
    match apply prsr4 tokens idx with 
     (Some (res4,new_idx)) -> Some((None,None,None,Some res4,None,None,None),new_idx)
     |None ->
    match apply prsr5 tokens idx with 
     (Some (res5,new_idx)) -> Some((None,None,None,None,Some res5,None,None),new_idx)
     |None ->
    match apply prsr6 tokens idx with 
     (Some (res6,new_idx)) -> Some((None,None,None,None,None,Some res6,None),new_idx)
     |None ->
    match apply prsr7 tokens idx with 
     (Some (res7,new_idx)) -> Some((None,None,None,None,None,None,Some res7),new_idx)
     |None ->
    None ;;

let dis7 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 = Jvsp_types.Parser (fun tokens idx ->helper_for_dis7 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 tokens idx) ;;


let helper_for_dis8 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 tokens idx=
    match apply prsr1 tokens idx with 
     (Some (res1,new_idx)) -> Some((Some res1,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr2 tokens idx with 
     (Some (res2,new_idx)) -> Some((None,Some res2,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr3 tokens idx with 
     (Some (res3,new_idx)) -> Some((None,None,Some res3,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr4 tokens idx with 
     (Some (res4,new_idx)) -> Some((None,None,None,Some res4,None,None,None,None),new_idx)
     |None ->
    match apply prsr5 tokens idx with 
     (Some (res5,new_idx)) -> Some((None,None,None,None,Some res5,None,None,None),new_idx)
     |None ->
    match apply prsr6 tokens idx with 
     (Some (res6,new_idx)) -> Some((None,None,None,None,None,Some res6,None,None),new_idx)
     |None ->
    match apply prsr7 tokens idx with 
     (Some (res7,new_idx)) -> Some((None,None,None,None,None,None,Some res7,None),new_idx)
     |None ->
    match apply prsr8 tokens idx with 
     (Some (res8,new_idx)) -> Some((None,None,None,None,None,None,None,Some res8),new_idx)
     |None ->
    None ;;

let dis8 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 = Jvsp_types.Parser (fun tokens idx ->helper_for_dis8 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 tokens idx) ;;


let helper_for_dis9 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 tokens idx=
    match apply prsr1 tokens idx with 
     (Some (res1,new_idx)) -> Some((Some res1,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr2 tokens idx with 
     (Some (res2,new_idx)) -> Some((None,Some res2,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr3 tokens idx with 
     (Some (res3,new_idx)) -> Some((None,None,Some res3,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr4 tokens idx with 
     (Some (res4,new_idx)) -> Some((None,None,None,Some res4,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr5 tokens idx with 
     (Some (res5,new_idx)) -> Some((None,None,None,None,Some res5,None,None,None,None),new_idx)
     |None ->
    match apply prsr6 tokens idx with 
     (Some (res6,new_idx)) -> Some((None,None,None,None,None,Some res6,None,None,None),new_idx)
     |None ->
    match apply prsr7 tokens idx with 
     (Some (res7,new_idx)) -> Some((None,None,None,None,None,None,Some res7,None,None),new_idx)
     |None ->
    match apply prsr8 tokens idx with 
     (Some (res8,new_idx)) -> Some((None,None,None,None,None,None,None,Some res8,None),new_idx)
     |None ->
    match apply prsr9 tokens idx with 
     (Some (res9,new_idx)) -> Some((None,None,None,None,None,None,None,None,Some res9),new_idx)
     |None ->
    None ;;

let dis9 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 = Jvsp_types.Parser (fun tokens idx ->helper_for_dis9 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 tokens idx) ;;


let helper_for_dis10 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 tokens idx=
    match apply prsr1 tokens idx with 
     (Some (res1,new_idx)) -> Some((Some res1,None,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr2 tokens idx with 
     (Some (res2,new_idx)) -> Some((None,Some res2,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr3 tokens idx with 
     (Some (res3,new_idx)) -> Some((None,None,Some res3,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr4 tokens idx with 
     (Some (res4,new_idx)) -> Some((None,None,None,Some res4,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr5 tokens idx with 
     (Some (res5,new_idx)) -> Some((None,None,None,None,Some res5,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr6 tokens idx with 
     (Some (res6,new_idx)) -> Some((None,None,None,None,None,Some res6,None,None,None,None),new_idx)
     |None ->
    match apply prsr7 tokens idx with 
     (Some (res7,new_idx)) -> Some((None,None,None,None,None,None,Some res7,None,None,None),new_idx)
     |None ->
    match apply prsr8 tokens idx with 
     (Some (res8,new_idx)) -> Some((None,None,None,None,None,None,None,Some res8,None,None),new_idx)
     |None ->
    match apply prsr9 tokens idx with 
     (Some (res9,new_idx)) -> Some((None,None,None,None,None,None,None,None,Some res9,None),new_idx)
     |None ->
    match apply prsr10 tokens idx with 
     (Some (res10,new_idx)) -> Some((None,None,None,None,None,None,None,None,None,Some res10),new_idx)
     |None ->
    None ;;

let dis10 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 = Jvsp_types.Parser (fun tokens idx ->helper_for_dis10 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 tokens idx) ;;


let helper_for_dis11 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 tokens idx=
    match apply prsr1 tokens idx with 
     (Some (res1,new_idx)) -> Some((Some res1,None,None,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr2 tokens idx with 
     (Some (res2,new_idx)) -> Some((None,Some res2,None,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr3 tokens idx with 
     (Some (res3,new_idx)) -> Some((None,None,Some res3,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr4 tokens idx with 
     (Some (res4,new_idx)) -> Some((None,None,None,Some res4,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr5 tokens idx with 
     (Some (res5,new_idx)) -> Some((None,None,None,None,Some res5,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr6 tokens idx with 
     (Some (res6,new_idx)) -> Some((None,None,None,None,None,Some res6,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr7 tokens idx with 
     (Some (res7,new_idx)) -> Some((None,None,None,None,None,None,Some res7,None,None,None,None),new_idx)
     |None ->
    match apply prsr8 tokens idx with 
     (Some (res8,new_idx)) -> Some((None,None,None,None,None,None,None,Some res8,None,None,None),new_idx)
     |None ->
    match apply prsr9 tokens idx with 
     (Some (res9,new_idx)) -> Some((None,None,None,None,None,None,None,None,Some res9,None,None),new_idx)
     |None ->
    match apply prsr10 tokens idx with 
     (Some (res10,new_idx)) -> Some((None,None,None,None,None,None,None,None,None,Some res10,None),new_idx)
     |None ->
    match apply prsr11 tokens idx with 
     (Some (res11,new_idx)) -> Some((None,None,None,None,None,None,None,None,None,None,Some res11),new_idx)
     |None ->
    None ;;

let dis11 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 = Jvsp_types.Parser (fun tokens idx ->helper_for_dis11 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 tokens idx) ;;


let helper_for_dis12 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 tokens idx=
    match apply prsr1 tokens idx with 
     (Some (res1,new_idx)) -> Some((Some res1,None,None,None,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr2 tokens idx with 
     (Some (res2,new_idx)) -> Some((None,Some res2,None,None,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr3 tokens idx with 
     (Some (res3,new_idx)) -> Some((None,None,Some res3,None,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr4 tokens idx with 
     (Some (res4,new_idx)) -> Some((None,None,None,Some res4,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr5 tokens idx with 
     (Some (res5,new_idx)) -> Some((None,None,None,None,Some res5,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr6 tokens idx with 
     (Some (res6,new_idx)) -> Some((None,None,None,None,None,Some res6,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr7 tokens idx with 
     (Some (res7,new_idx)) -> Some((None,None,None,None,None,None,Some res7,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr8 tokens idx with 
     (Some (res8,new_idx)) -> Some((None,None,None,None,None,None,None,Some res8,None,None,None,None),new_idx)
     |None ->
    match apply prsr9 tokens idx with 
     (Some (res9,new_idx)) -> Some((None,None,None,None,None,None,None,None,Some res9,None,None,None),new_idx)
     |None ->
    match apply prsr10 tokens idx with 
     (Some (res10,new_idx)) -> Some((None,None,None,None,None,None,None,None,None,Some res10,None,None),new_idx)
     |None ->
    match apply prsr11 tokens idx with 
     (Some (res11,new_idx)) -> Some((None,None,None,None,None,None,None,None,None,None,Some res11,None),new_idx)
     |None ->
    match apply prsr12 tokens idx with 
     (Some (res12,new_idx)) -> Some((None,None,None,None,None,None,None,None,None,None,None,Some res12),new_idx)
     |None ->
    None ;;

let dis12 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 = Jvsp_types.Parser (fun tokens idx ->helper_for_dis12 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 tokens idx) ;;


let helper_for_dis13 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 prsr13 tokens idx=
    match apply prsr1 tokens idx with 
     (Some (res1,new_idx)) -> Some((Some res1,None,None,None,None,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr2 tokens idx with 
     (Some (res2,new_idx)) -> Some((None,Some res2,None,None,None,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr3 tokens idx with 
     (Some (res3,new_idx)) -> Some((None,None,Some res3,None,None,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr4 tokens idx with 
     (Some (res4,new_idx)) -> Some((None,None,None,Some res4,None,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr5 tokens idx with 
     (Some (res5,new_idx)) -> Some((None,None,None,None,Some res5,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr6 tokens idx with 
     (Some (res6,new_idx)) -> Some((None,None,None,None,None,Some res6,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr7 tokens idx with 
     (Some (res7,new_idx)) -> Some((None,None,None,None,None,None,Some res7,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr8 tokens idx with 
     (Some (res8,new_idx)) -> Some((None,None,None,None,None,None,None,Some res8,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr9 tokens idx with 
     (Some (res9,new_idx)) -> Some((None,None,None,None,None,None,None,None,Some res9,None,None,None,None),new_idx)
     |None ->
    match apply prsr10 tokens idx with 
     (Some (res10,new_idx)) -> Some((None,None,None,None,None,None,None,None,None,Some res10,None,None,None),new_idx)
     |None ->
    match apply prsr11 tokens idx with 
     (Some (res11,new_idx)) -> Some((None,None,None,None,None,None,None,None,None,None,Some res11,None,None),new_idx)
     |None ->
    match apply prsr12 tokens idx with 
     (Some (res12,new_idx)) -> Some((None,None,None,None,None,None,None,None,None,None,None,Some res12,None),new_idx)
     |None ->
    match apply prsr13 tokens idx with 
     (Some (res13,new_idx)) -> Some((None,None,None,None,None,None,None,None,None,None,None,None,Some res13),new_idx)
     |None ->
    None ;;

let dis13 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 prsr13 = Jvsp_types.Parser (fun tokens idx ->helper_for_dis13 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 prsr13 tokens idx) ;;


let helper_for_dis14 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 prsr13 prsr14 tokens idx=
    match apply prsr1 tokens idx with 
     (Some (res1,new_idx)) -> Some((Some res1,None,None,None,None,None,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr2 tokens idx with 
     (Some (res2,new_idx)) -> Some((None,Some res2,None,None,None,None,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr3 tokens idx with 
     (Some (res3,new_idx)) -> Some((None,None,Some res3,None,None,None,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr4 tokens idx with 
     (Some (res4,new_idx)) -> Some((None,None,None,Some res4,None,None,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr5 tokens idx with 
     (Some (res5,new_idx)) -> Some((None,None,None,None,Some res5,None,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr6 tokens idx with 
     (Some (res6,new_idx)) -> Some((None,None,None,None,None,Some res6,None,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr7 tokens idx with 
     (Some (res7,new_idx)) -> Some((None,None,None,None,None,None,Some res7,None,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr8 tokens idx with 
     (Some (res8,new_idx)) -> Some((None,None,None,None,None,None,None,Some res8,None,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr9 tokens idx with 
     (Some (res9,new_idx)) -> Some((None,None,None,None,None,None,None,None,Some res9,None,None,None,None,None),new_idx)
     |None ->
    match apply prsr10 tokens idx with 
     (Some (res10,new_idx)) -> Some((None,None,None,None,None,None,None,None,None,Some res10,None,None,None,None),new_idx)
     |None ->
    match apply prsr11 tokens idx with 
     (Some (res11,new_idx)) -> Some((None,None,None,None,None,None,None,None,None,None,Some res11,None,None,None),new_idx)
     |None ->
    match apply prsr12 tokens idx with 
     (Some (res12,new_idx)) -> Some((None,None,None,None,None,None,None,None,None,None,None,Some res12,None,None),new_idx)
     |None ->
    match apply prsr13 tokens idx with 
     (Some (res13,new_idx)) -> Some((None,None,None,None,None,None,None,None,None,None,None,None,Some res13,None),new_idx)
     |None ->
    match apply prsr14 tokens idx with 
     (Some (res14,new_idx)) -> Some((None,None,None,None,None,None,None,None,None,None,None,None,None,Some res14),new_idx)
     |None ->
    None ;;

let dis14 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 prsr13 prsr14 = Jvsp_types.Parser (fun tokens idx ->helper_for_dis14 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 prsr13 prsr14 tokens idx) ;;

let helper_for_concat2 prsr1 prsr2 tokens idx1 = 
    match apply prsr1 tokens idx1 with 
      None -> None
     |(Some (res1,idx2)) ->
    match apply prsr2 tokens idx2 with 
      None -> None
     |(Some (res2,idx3)) ->
    Some((res1,res2),idx3);;

let concat2 prsr1 prsr2 = Jvsp_types.Parser (fun tokens idx ->helper_for_concat2 prsr1 prsr2 tokens idx) ;;




let helper_for_concat3 prsr1 prsr2 prsr3 tokens idx1 = 
    match apply prsr1 tokens idx1 with 
      None -> None
     |(Some (res1,idx2)) ->
    match apply prsr2 tokens idx2 with 
      None -> None
     |(Some (res2,idx3)) ->
    match apply prsr3 tokens idx3 with 
      None -> None
     |(Some (res3,idx4)) ->
    Some((res1,res2,res3),idx4);;

let concat3 prsr1 prsr2 prsr3 = Jvsp_types.Parser (fun tokens idx ->helper_for_concat3 prsr1 prsr2 prsr3 tokens idx) ;;


let helper_for_concat4 prsr1 prsr2 prsr3 prsr4 tokens idx1 = 
    match apply prsr1 tokens idx1 with 
      None -> None
     |(Some (res1,idx2)) ->
    match apply prsr2 tokens idx2 with 
      None -> None
     |(Some (res2,idx3)) ->
    match apply prsr3 tokens idx3 with 
      None -> None
     |(Some (res3,idx4)) ->
    match apply prsr4 tokens idx4 with 
      None -> None
     |(Some (res4,idx5)) ->
    Some((res1,res2,res3,res4),idx5);;

let concat4 prsr1 prsr2 prsr3 prsr4 = Jvsp_types.Parser (fun tokens idx ->helper_for_concat4 prsr1 prsr2 prsr3 prsr4 tokens idx) ;;


let helper_for_concat5 prsr1 prsr2 prsr3 prsr4 prsr5 tokens idx1 = 
    match apply prsr1 tokens idx1 with 
      None -> None
     |(Some (res1,idx2)) ->
    match apply prsr2 tokens idx2 with 
      None -> None
     |(Some (res2,idx3)) ->
    match apply prsr3 tokens idx3 with 
      None -> None
     |(Some (res3,idx4)) ->
    match apply prsr4 tokens idx4 with 
      None -> None
     |(Some (res4,idx5)) ->
    match apply prsr5 tokens idx5 with 
      None -> None
     |(Some (res5,idx6)) ->
    Some((res1,res2,res3,res4,res5),idx6);;

let concat5 prsr1 prsr2 prsr3 prsr4 prsr5 = Jvsp_types.Parser (fun tokens idx ->helper_for_concat5 prsr1 prsr2 prsr3 prsr4 prsr5 tokens idx) ;;


let helper_for_concat6 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 tokens idx1 = 
    match apply prsr1 tokens idx1 with 
      None -> None
     |(Some (res1,idx2)) ->
    match apply prsr2 tokens idx2 with 
      None -> None
     |(Some (res2,idx3)) ->
    match apply prsr3 tokens idx3 with 
      None -> None
     |(Some (res3,idx4)) ->
    match apply prsr4 tokens idx4 with 
      None -> None
     |(Some (res4,idx5)) ->
    match apply prsr5 tokens idx5 with 
      None -> None
     |(Some (res5,idx6)) ->
    match apply prsr6 tokens idx6 with 
      None -> None
     |(Some (res6,idx7)) ->
    Some((res1,res2,res3,res4,res5,res6),idx7);;

let concat6 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 = Jvsp_types.Parser (fun tokens idx ->helper_for_concat6 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 tokens idx) ;;


let helper_for_concat7 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 tokens idx1 = 
    match apply prsr1 tokens idx1 with 
      None -> None
     |(Some (res1,idx2)) ->
    match apply prsr2 tokens idx2 with 
      None -> None
     |(Some (res2,idx3)) ->
    match apply prsr3 tokens idx3 with 
      None -> None
     |(Some (res3,idx4)) ->
    match apply prsr4 tokens idx4 with 
      None -> None
     |(Some (res4,idx5)) ->
    match apply prsr5 tokens idx5 with 
      None -> None
     |(Some (res5,idx6)) ->
    match apply prsr6 tokens idx6 with 
      None -> None
     |(Some (res6,idx7)) ->
    match apply prsr7 tokens idx7 with 
      None -> None
     |(Some (res7,idx8)) ->
    Some((res1,res2,res3,res4,res5,res6,res7),idx8);;

let concat7 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 = Jvsp_types.Parser (fun tokens idx ->helper_for_concat7 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 tokens idx) ;;


let helper_for_concat8 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 tokens idx1 = 
    match apply prsr1 tokens idx1 with 
      None -> None
     |(Some (res1,idx2)) ->
    match apply prsr2 tokens idx2 with 
      None -> None
     |(Some (res2,idx3)) ->
    match apply prsr3 tokens idx3 with 
      None -> None
     |(Some (res3,idx4)) ->
    match apply prsr4 tokens idx4 with 
      None -> None
     |(Some (res4,idx5)) ->
    match apply prsr5 tokens idx5 with 
      None -> None
     |(Some (res5,idx6)) ->
    match apply prsr6 tokens idx6 with 
      None -> None
     |(Some (res6,idx7)) ->
    match apply prsr7 tokens idx7 with 
      None -> None
     |(Some (res7,idx8)) ->
    match apply prsr8 tokens idx8 with 
      None -> None
     |(Some (res8,idx9)) ->
    Some((res1,res2,res3,res4,res5,res6,res7,res8),idx9);;

let concat8 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 = Jvsp_types.Parser (fun tokens idx ->helper_for_concat8 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 tokens idx) ;;


let helper_for_concat9 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 tokens idx1 = 
    match apply prsr1 tokens idx1 with 
      None -> None
     |(Some (res1,idx2)) ->
    match apply prsr2 tokens idx2 with 
      None -> None
     |(Some (res2,idx3)) ->
    match apply prsr3 tokens idx3 with 
      None -> None
     |(Some (res3,idx4)) ->
    match apply prsr4 tokens idx4 with 
      None -> None
     |(Some (res4,idx5)) ->
    match apply prsr5 tokens idx5 with 
      None -> None
     |(Some (res5,idx6)) ->
    match apply prsr6 tokens idx6 with 
      None -> None
     |(Some (res6,idx7)) ->
    match apply prsr7 tokens idx7 with 
      None -> None
     |(Some (res7,idx8)) ->
    match apply prsr8 tokens idx8 with 
      None -> None
     |(Some (res8,idx9)) ->
    match apply prsr9 tokens idx9 with 
      None -> None
     |(Some (res9,idx10)) ->
    Some((res1,res2,res3,res4,res5,res6,res7,res8,res9),idx10);;

let concat9 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 = Jvsp_types.Parser (fun tokens idx ->helper_for_concat9 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 tokens idx) ;;


let helper_for_concat10 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 tokens idx1 = 
    match apply prsr1 tokens idx1 with 
      None -> None
     |(Some (res1,idx2)) ->
    match apply prsr2 tokens idx2 with 
      None -> None
     |(Some (res2,idx3)) ->
    match apply prsr3 tokens idx3 with 
      None -> None
     |(Some (res3,idx4)) ->
    match apply prsr4 tokens idx4 with 
      None -> None
     |(Some (res4,idx5)) ->
    match apply prsr5 tokens idx5 with 
      None -> None
     |(Some (res5,idx6)) ->
    match apply prsr6 tokens idx6 with 
      None -> None
     |(Some (res6,idx7)) ->
    match apply prsr7 tokens idx7 with 
      None -> None
     |(Some (res7,idx8)) ->
    match apply prsr8 tokens idx8 with 
      None -> None
     |(Some (res8,idx9)) ->
    match apply prsr9 tokens idx9 with 
      None -> None
     |(Some (res9,idx10)) ->
    match apply prsr10 tokens idx10 with 
      None -> None
     |(Some (res10,idx11)) ->
    Some((res1,res2,res3,res4,res5,res6,res7,res8,res9,res10),idx11);;

let concat10 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 = Jvsp_types.Parser (fun tokens idx ->helper_for_concat10 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 tokens idx) ;;


let helper_for_concat11 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 tokens idx1 = 
    match apply prsr1 tokens idx1 with 
      None -> None
     |(Some (res1,idx2)) ->
    match apply prsr2 tokens idx2 with 
      None -> None
     |(Some (res2,idx3)) ->
    match apply prsr3 tokens idx3 with 
      None -> None
     |(Some (res3,idx4)) ->
    match apply prsr4 tokens idx4 with 
      None -> None
     |(Some (res4,idx5)) ->
    match apply prsr5 tokens idx5 with 
      None -> None
     |(Some (res5,idx6)) ->
    match apply prsr6 tokens idx6 with 
      None -> None
     |(Some (res6,idx7)) ->
    match apply prsr7 tokens idx7 with 
      None -> None
     |(Some (res7,idx8)) ->
    match apply prsr8 tokens idx8 with 
      None -> None
     |(Some (res8,idx9)) ->
    match apply prsr9 tokens idx9 with 
      None -> None
     |(Some (res9,idx10)) ->
    match apply prsr10 tokens idx10 with 
      None -> None
     |(Some (res10,idx11)) ->
    match apply prsr11 tokens idx11 with 
      None -> None
     |(Some (res11,idx12)) ->
    Some((res1,res2,res3,res4,res5,res6,res7,res8,res9,res10,res11),idx12);;

let concat11 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 = Jvsp_types.Parser (fun tokens idx ->helper_for_concat11 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 tokens idx) ;;


let helper_for_concat12 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 tokens idx1 = 
    match apply prsr1 tokens idx1 with 
      None -> None
     |(Some (res1,idx2)) ->
    match apply prsr2 tokens idx2 with 
      None -> None
     |(Some (res2,idx3)) ->
    match apply prsr3 tokens idx3 with 
      None -> None
     |(Some (res3,idx4)) ->
    match apply prsr4 tokens idx4 with 
      None -> None
     |(Some (res4,idx5)) ->
    match apply prsr5 tokens idx5 with 
      None -> None
     |(Some (res5,idx6)) ->
    match apply prsr6 tokens idx6 with 
      None -> None
     |(Some (res6,idx7)) ->
    match apply prsr7 tokens idx7 with 
      None -> None
     |(Some (res7,idx8)) ->
    match apply prsr8 tokens idx8 with 
      None -> None
     |(Some (res8,idx9)) ->
    match apply prsr9 tokens idx9 with 
      None -> None
     |(Some (res9,idx10)) ->
    match apply prsr10 tokens idx10 with 
      None -> None
     |(Some (res10,idx11)) ->
    match apply prsr11 tokens idx11 with 
      None -> None
     |(Some (res11,idx12)) ->
    match apply prsr12 tokens idx12 with 
      None -> None
     |(Some (res12,idx13)) ->
    Some((res1,res2,res3,res4,res5,res6,res7,res8,res9,res10,res11,res12),idx13);;

let concat12 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 = Jvsp_types.Parser (fun tokens idx ->helper_for_concat12 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 tokens idx) ;;


let helper_for_concat13 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 prsr13 tokens idx1 = 
    match apply prsr1 tokens idx1 with 
      None -> None
     |(Some (res1,idx2)) ->
    match apply prsr2 tokens idx2 with 
      None -> None
     |(Some (res2,idx3)) ->
    match apply prsr3 tokens idx3 with 
      None -> None
     |(Some (res3,idx4)) ->
    match apply prsr4 tokens idx4 with 
      None -> None
     |(Some (res4,idx5)) ->
    match apply prsr5 tokens idx5 with 
      None -> None
     |(Some (res5,idx6)) ->
    match apply prsr6 tokens idx6 with 
      None -> None
     |(Some (res6,idx7)) ->
    match apply prsr7 tokens idx7 with 
      None -> None
     |(Some (res7,idx8)) ->
    match apply prsr8 tokens idx8 with 
      None -> None
     |(Some (res8,idx9)) ->
    match apply prsr9 tokens idx9 with 
      None -> None
     |(Some (res9,idx10)) ->
    match apply prsr10 tokens idx10 with 
      None -> None
     |(Some (res10,idx11)) ->
    match apply prsr11 tokens idx11 with 
      None -> None
     |(Some (res11,idx12)) ->
    match apply prsr12 tokens idx12 with 
      None -> None
     |(Some (res12,idx13)) ->
    match apply prsr13 tokens idx13 with 
      None -> None
     |(Some (res13,idx14)) ->
    Some((res1,res2,res3,res4,res5,res6,res7,res8,res9,res10,res11,res12,res13),idx14);;

let concat13 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 prsr13 = Jvsp_types.Parser (fun tokens idx ->helper_for_concat13 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 prsr13 tokens idx) ;;


let helper_for_concat14 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 prsr13 prsr14 tokens idx1 = 
    match apply prsr1 tokens idx1 with 
      None -> None
     |(Some (res1,idx2)) ->
    match apply prsr2 tokens idx2 with 
      None -> None
     |(Some (res2,idx3)) ->
    match apply prsr3 tokens idx3 with 
      None -> None
     |(Some (res3,idx4)) ->
    match apply prsr4 tokens idx4 with 
      None -> None
     |(Some (res4,idx5)) ->
    match apply prsr5 tokens idx5 with 
      None -> None
     |(Some (res5,idx6)) ->
    match apply prsr6 tokens idx6 with 
      None -> None
     |(Some (res6,idx7)) ->
    match apply prsr7 tokens idx7 with 
      None -> None
     |(Some (res7,idx8)) ->
    match apply prsr8 tokens idx8 with 
      None -> None
     |(Some (res8,idx9)) ->
    match apply prsr9 tokens idx9 with 
      None -> None
     |(Some (res9,idx10)) ->
    match apply prsr10 tokens idx10 with 
      None -> None
     |(Some (res10,idx11)) ->
    match apply prsr11 tokens idx11 with 
      None -> None
     |(Some (res11,idx12)) ->
    match apply prsr12 tokens idx12 with 
      None -> None
     |(Some (res12,idx13)) ->
    match apply prsr13 tokens idx13 with 
      None -> None
     |(Some (res13,idx14)) ->
    match apply prsr14 tokens idx14 with 
      None -> None
     |(Some (res14,idx15)) ->
    Some((res1,res2,res3,res4,res5,res6,res7,res8,res9,res10,res11,res12,res13,res14),idx15);;

let concat14 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 prsr13 prsr14 = Jvsp_types.Parser (fun tokens idx ->helper_for_concat14 prsr1 prsr2 prsr3 prsr4 prsr5 prsr6 prsr7 prsr8 prsr9 prsr10 prsr11 prsr12 prsr13 prsr14 tokens idx) ;;


end ;;

let concat2 = Private.concat2 ;;
let concat3 = Private.concat3 ;;
let concat4 = Private.concat4 ;;
let concat5 = Private.concat5 ;;
let concat6 = Private.concat6 ;;
let concat7 = Private.concat7 ;;
let concat8 = Private.concat8 ;;
let concat9 = Private.concat9 ;;
let concat10 = Private.concat10 ;;
let concat11 = Private.concat11 ;;
let concat12 = Private.concat12 ;;
let concat13 = Private.concat13 ;;
let concat14 = Private.concat14 ;;


let dis2 = Private.dis2 ;;
let dis3 = Private.dis3 ;;
let dis4 = Private.dis4 ;;
let dis5 = Private.dis5 ;;
let dis6 = Private.dis6 ;;
let dis7 = Private.dis7 ;;
let dis8 = Private.dis8 ;;
let dis9 = Private.dis9 ;;
let dis10 = Private.dis10 ;;
let dis11 = Private.dis11 ;;
let dis12 = Private.dis12 ;;
let dis13 = Private.dis13 ;;
let dis14 = Private.dis14 ;;


let map = Private.map ;;

let molecular = Private.molecular ;;
let star = Private.star ;;
