(*

#use"Text_editing/put_use_directive_in_initial_comment.ml";;

*)

let detect_initial_comment_in_text text = 
  let lines = Lines_in_string.indexed_lines text in 
  let first_line = snd (List.hd lines) in 
  if (Cull_string.trim_spaces first_line) <> "(*" then None else 
  match Option.seek (fun (line_idx,line)->
     if (line_idx<=1) then false else
     let trimmed_line = Cull_string.trim_spaces line in 
     Supstring.begins_with trimmed_line "#use"
  ) lines with 
  None -> None 
  |Some(i1,line1) ->
 (
  match Option.seek (fun (line_idx,line)->
    if (line_idx<=i1) then false else
    let trimmed_line = Cull_string.trim_spaces line in 
    trimmed_line = "*)"
 ) lines with 
  None -> None 
 |Some(i2,_) -> Some(i1,Cull_string.trim_spaces line1,i2) );;  

let detect_initial_comment_in_file  fn =  detect_initial_comment_in_text  (Io.read_whole_file fn) ;;

let in_text ~new_directive text =
  match detect_initial_comment_in_text text  with 
  None -> text 
  |Some(i1,line1,i2) ->
    let old_lines = Lines_in_string.indexed_lines text in 
    let new_lines = Image.image (fun (line_idx,line)->
        if line_idx = i1 then new_directive else line
      ) old_lines in 
   String.concat "\n" new_lines ;;   

let in_file ~new_directive fn =   
   let text = Io.read_whole_file fn in 
   match detect_initial_comment_in_text text  with 
  None -> () 
  |Some(i1,line1,i2) ->
    let old_lines = Lines_in_string.indexed_lines text in 
    let new_lines = Image.image (fun (line_idx,line)->
        if line_idx = i1 then new_directive else line
      ) old_lines in 
    let new_text = String.concat "\n" new_lines in 
    Io.overwrite_with fn new_text;;

let usual root ap =
    let s_ap=Absolute_path.to_string ap in 
    let s_cdir=Dfa_root.connectable_to_subpath root in 
    let shortened_path=Cull_string.cobeginning (String.length s_cdir) s_ap in 
    "#use\""^shortened_path^"\";"^";" ;;
    
let put_usual root ap =
    let new_directive = usual root ap in 
    in_file ~new_directive ap;;    