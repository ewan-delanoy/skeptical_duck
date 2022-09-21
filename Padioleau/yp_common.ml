(*

#use"Padioleau/yp_common.ml";;

*)

type prof = ProfAll | ProfNone | ProfSome of string list ;;
type timeout_info = {
  name: string;
  max_duration: float;
} ;; 



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

let push v l = (l := v :: !l) ;; 

let optionise f =
  try Some (f ()) with Not_found -> None ;; 

  
let profile = ref ProfNone ;; 
let show_trace_profile = ref false ;;
let spf = Printf.sprintf ;;   

let check_profile category =
    match !profile with
    | ProfAll -> true
    | ProfNone -> false
    | ProfSome l -> List.mem category l ;;

let _profile_table = ref (Hashtbl.create 100) ;; 

let adjust_profile_entry category difftime =
      let (xtime, xcount) =
        (try Hashtbl.find !_profile_table category
         with Not_found ->
           let xtime = ref 0.0 in
           let xcount = ref 0 in
           Hashtbl.add !_profile_table category (xtime, xcount);
           (xtime, xcount)
        ) in
      xtime := !xtime +. difftime;
      xcount := !xcount + 1;
      ()   ;;   

exception Timeout of timeout_info ;;


let profile_code category f =
    if not (check_profile category)
    then f ()
    else begin
      if !show_trace_profile then pr2 (spf "> %s" category);
      let t = Unix.gettimeofday () in
      let res, prefix =
        try Ok (f ()), ""
        with Timeout _ as exn ->
          let e = Yp_exception.catch exn in
          Error e, "*"
      in
      let category = prefix ^ category in (* add a '*' to indicate timeout func *)
      let t' = Unix.gettimeofday () in
  
      if !show_trace_profile then pr2 (spf "< %s" category);
  
      adjust_profile_entry category (t' -. t);
      (match res with
       | Ok res -> res
       | Error e -> Yp_exception.reraise e
      );
    end ;; 
  

let memoized ?(use_cache=true) h k f =
    if not use_cache
    then f ()
    else
      try Hashtbl.find h k
      with Not_found ->
        let v = f () in
        begin
          Hashtbl.add h k v;
          v
        end ;;

let _memo_compiled_regexp = Hashtbl.create 101 ;;
let candidate_match_func s re =
    (* old: Str.string_match (Str.regexp re) s 0 *)
    let compile_re =
      memoized _memo_compiled_regexp re (fun () -> Str.regexp re)
    in
    Str.string_match compile_re s 0 ;; 
  
let match_func s re =
    profile_code "Common.=~" (fun () -> candidate_match_func s re) ;;
  
let (=~) s re =
    match_func s re ;;
    