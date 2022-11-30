(*

Enumerate the ways in which the "module" keyword can be used in OCaml.

#use"lib/modulekeyword_use_case.ml";;

*)



let all=[
    Modulekeyword_use_case_t.Include;
    Modulekeyword_use_case_t.Open;
    Modulekeyword_use_case_t.Duplicate;
    Modulekeyword_use_case_t.Pointed
];;
  
  
  (*
  let f case s=let (i,j)=Option.unpack(Alternative_str.centered_regexp_match case s 1) in 
  (i,j,Cull_string.interval s i j);;
  
  f include_case " include Peggy;; ";;
  f include_case " include_once;; ";;
  f moodle_case " module Amy = Lawson ";;
  f pointed_case " 57+Everybody.talking-78 ";;
  
  *)
 