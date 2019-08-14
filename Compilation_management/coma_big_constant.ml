
(* 

#use"Compilation_management/coma_big_constant.ml";;

*)

module This_World=struct

let root=Root_directory.of_string "/Users/ewandelanoy/Teuliou/OCaml/Ordinary";;
let backup_dir=Root_directory.of_string "/Users/ewandelanoy/Teuliou/OCaml/Githubbed_ocaml";; 
let githubbing=true;;     
let triple = (root,backup_dir,githubbing);;           

end;;


module Next_World=struct

let root=Root_directory.of_string "/Users/ewandelanoy/Teuliou/OCaml/Idaho";;
let backup_dir=Root_directory.of_string "/Users/ewandelanoy/Teuliou/OCaml/Idaho_backup";;
let githubbing=false;;                
let triple = (root,backup_dir,githubbing);;

end;;

module Third_World=struct

let root=Root_directory.of_string "/Users/ewandelanoy/Teuliou/OCaml/Cherokee";;
let backup_dir=Root_directory.of_string "/Users/ewandelanoy/Teuliou/OCaml/Cherokee_backup";; 
let githubbing=false;;                
let triple = (root,backup_dir,githubbing);;

end;;

                 