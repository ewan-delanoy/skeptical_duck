(*

#use"lib/Text_editing/detect_printer_registration_in_text.ml";;

*)

module Private = struct

let starter = "(* This is a registered printer : " ;;

let printer_opt (idx,line) =
  if not(String.starts_with ~prefix:starter line)
  then None 
  else
  let line2 = Cull_string.two_sided_cutting (starter,"*)\n") line in 
  Some (idx,Cull_string.trim_spaces line2) ;;

let printers_in_text text= 
  let indexed_lines = Lines_in_text.indexed_lines text in 
  List.filter_map printer_opt indexed_lines;;

let printers_in_file ap =
   let filename = Absolute_path.to_string ap in 
   let eless = Cull_string.before_rightmost_possibly_all filename '.' in
   let pointed_mod = (String.capitalize_ascii eless)^"." in 
   let full_text = Io.read_whole_file ap in 
   let temp = printers_in_text full_text in 
   Image.image (fun (idx,short_printer_name) ->
    (idx,pointed_mod ^ short_printer_name)  
   ) temp ;;


end ;;

let detect = Private.printers_in_file ;;