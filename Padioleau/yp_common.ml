(*

#use"Padioleau/yp_common.ml";;

*)

let prerr_string s = output_string stderr s ;;

let pr2 s =
  prerr_string s;
  prerr_string "\n";
  flush stderr ;; 

           
let hash_of_list xs =
    let h = Hashtbl.create 101 in
    xs |> List.iter (fun (k, v) -> Hashtbl.replace h k v);
    h  