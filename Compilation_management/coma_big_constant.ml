(* 
#use"Compilation_management/coma_big_constant.ml";;
*)

let github_url = "https://github.com/ewan-delanoy/skeptical_duck";;
let home = Sys.getenv "HOME" ;;

module This_World=struct

let root=Dfa_root.of_line (home^"/Teuliou/OCaml/Ordinary");;
let backup_dir=Dfa_root.of_line (home^"/Teuliou/OCaml/Githubbed_ocaml");;
let githubbing=true;;
let triple = (root,backup_dir,githubbing);;

end;;
module Next_World=struct

let root=Dfa_root.of_line (home^"/Teuliou/OCaml/Idaho");;
let backup_dir=Dfa_root.of_line (home^"/Teuliou/OCaml/Idaho_backup") ;;
let githubbing=false;;
let triple = (root,backup_dir,githubbing);;

end;;
module Third_World=struct

let root=Dfa_root.of_line (home^"/Teuliou/OCaml/Cherokee") ;;
let backup_dir=Dfa_root.of_line (home^"/Teuliou/OCaml/Cherokee_backup") ;;
let githubbing=false;;
let triple = (root,backup_dir,githubbing);;

end;;



