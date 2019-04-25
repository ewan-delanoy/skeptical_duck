
(* 


#use"unjoin_path.ml";;



*)

let unjoin_path ap=
  let (t1,t2)=Cull_string.father_and_son (Absolute_path.to_string ap) '/' in
  (Directory_name.of_string(t1),
   No_slashes.of_string(t2));; 
  



   
   
              