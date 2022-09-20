(*

#use"Padioleau/yp_common.ml";;

*)

let prerr_string s = output_string stderr s ;;

let pr2 s =
  prerr_string s;
  prerr_string "\n";
  flush stderr ;; 

(*****************************************************************************)
(* Hash *)
(*****************************************************************************)
        
let hash_of_list xs =
    let h = Hashtbl.create 101 in
    xs |> List.iter (fun (k, v) -> Hashtbl.replace h k v);
    h  ;;

let hash_to_list h =
    Hashtbl.fold (fun k v acc -> (k,v)::acc) h []
    |> List.sort compare ;; 
    

let hashset_to_list h = hash_to_list h |> List.map fst ;;

let hkeys h =
      let hkey = Hashtbl.create 101 in
      h |> Hashtbl.iter (fun k _v -> Hashtbl.replace hkey k true);
      hashset_to_list hkey ;;

(*****************************************************************************)
(* Stack *)
(*****************************************************************************)
type 'a stack = 'a list ;;
(* with sexp *)

let (top: 'a stack -> 'a) = List.hd ;; 

let pop2 l =
  let v = List.hd !l in
  begin
    l := List.tl !l;
    v
  end ;;  

let push v l = l := v :: !l ;; 





