(*

#use"lib/ansi_terminal.ml";;


*)


let remove_last_lines k =  for _ = 1 to k do print_string "\027[A\027[2K" done;;

let ask ~question = 
    let _=(print_string question) in 
    let input_from_user = read_line () in
    let column = (String.length question)+(String.length input_from_user)+1 in
    let _ =Printf.printf "\027[1A\027[%dG " column in 
    input_from_user ;;
