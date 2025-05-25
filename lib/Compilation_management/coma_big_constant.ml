(* 
#use"lib/Compilation_management/coma_big_constant.ml";;
*)

let github_url = "https://github.com/ewan-delanoy/skeptical_duck";;
let home = Sys.getenv "HOME" ;;
let root_of_root = home^"/Teuliou/OCaml/" ;;

module This_World=struct

let root=Dfa_root.of_line (root_of_root^"skeptical_duck");;
let backup_dir=Dfa_root.of_line (root_of_root^"Githubbed_ocaml");;
let githubbing=true;;
let triple = (root,backup_dir,githubbing);;

end;;
module Next_World=struct

let root=Dfa_root.of_line (root_of_root^"Idaho");;
let backup_dir=Dfa_root.of_line (root_of_root^"Idaho_backup") ;;
let githubbing=false;;
let triple = (root,backup_dir,githubbing);;

end;;
module Third_World=struct

let root=Dfa_root.of_line (root_of_root^"Cherokee") ;;
let backup_dir=Dfa_root.of_line (root_of_root^"Cherokee_backup") ;;
let githubbing=false;;
let triple = (root,backup_dir,githubbing);;

end;;



