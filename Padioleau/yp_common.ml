(*

#use"Padioleau/yp_common.ml";;

*)

let prerr_string s = output_string stderr s ;;

let pr2 s =
  prerr_string s;
  prerr_string "\n";
  flush stderr ;; 

           