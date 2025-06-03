(*

#use"lib/Text_editing/put_use_directive_in_initial_comment.ml";;

*)

let detect_initial_comment_in_text text = 
  let lines = Lines_in_text.indexed_lines text in 
  let first_line = snd (List.hd lines) in 
  if (Cull_string.trim_spaces first_line) <> "(*" then None else 
  match List.find_opt (fun (line_idx,line)->
     if (line_idx<=1) then false else
     let trimmed_line = Cull_string.trim_spaces line in 
     String.starts_with ~prefix:"#use" trimmed_line 
  ) lines with 
  None -> None 
  |Some(i1,line1) ->
 (
  match List.find_opt (fun (line_idx,line)->
    if (line_idx<=i1) then false else
    let trimmed_line = Cull_string.trim_spaces line in 
    trimmed_line = "*)"
 ) lines with 
  None -> None 
 |Some(i2,_) -> Some(i1,Cull_string.trim_spaces line1,i2) );;  

let detect_initial_comment_in_file  fn =  detect_initial_comment_in_text  (Io.read_whole_file fn) ;;

let compute_text_with_replaced_directive ~directive_line_idx ~new_directive old_text =
  let old_lines = Lines_in_text.indexed_lines old_text in 
  let fresh_lines = Image.image (fun (line_idx,line)->
        if line_idx = directive_line_idx then new_directive else line
      ) old_lines in 
  String.concat "\n" fresh_lines ;; 

let replace_if_present_in_text ~new_directive old_text =
  match detect_initial_comment_in_text old_text  with 
  None -> old_text 
  |Some(i1,_line1,_i2) ->
    compute_text_with_replaced_directive ~directive_line_idx:i1 ~new_directive old_text ;;   

let prepend_in_text  ~new_directive old_text =
  "(*\n\n\n" ^ new_directive ^ "\n\n\n*)\n\n\n" ^old_text ;;

let prepend_or_replace_if_present_in_file ~new_directive fn =   
      let old_text = Io.read_whole_file fn in 
      let new_text = (
      match detect_initial_comment_in_text old_text  with 
     None -> prepend_in_text  ~new_directive old_text
     |Some(i1,_line1,_i2) ->
        compute_text_with_replaced_directive 
        ~directive_line_idx:i1 ~new_directive old_text) in 
       Io.overwrite_with fn new_text;;


let replace_if_present_in_file ~new_directive fn =   
   let old_text = Io.read_whole_file fn in 
   match detect_initial_comment_in_text old_text  with 
  None -> () 
  |Some(i1,_line1,_i2) ->
    let new_text = compute_text_with_replaced_directive ~directive_line_idx:i1 ~new_directive old_text in 
    Io.overwrite_with fn new_text;;

let usual root ap =
    let s_ap=Absolute_path.to_string ap in 
    let s_cdir=Dfa_root.connectable_to_subpath root in 
    let shortened_path=Cull_string.cobeginning (String.length s_cdir) s_ap in 
    "#use\""^shortened_path^"\";"^";" ;;

let prepend_or_replace_with_usual root ap =
      let new_directive = usual root ap in 
      prepend_or_replace_if_present_in_file ~new_directive ap;;       

let replace_with_usual root ap =
    let new_directive = usual root ap in 
    replace_if_present_in_file ~new_directive ap;;    