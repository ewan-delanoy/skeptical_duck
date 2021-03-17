(*

#use"more_io.ml";;

*)
 

let transfer_first_lines_of_to num_of_lines beg_ap end_ap = 
    let old_beginning = Io.read_whole_file beg_ap in 
    let old_ending = Io.read_whole_file end_ap in 
    let to_be_transferred = Lines_in_string.interval old_ending 1 num_of_lines in 
    let new_beginning = old_beginning ^ to_be_transferred in 
    let new_ending = Cull_string.two_sided_cutting (to_be_transferred,"") old_ending in 
     (Io.overwrite_with beg_ap new_beginning ; Io.overwrite_with end_ap new_ending);;





