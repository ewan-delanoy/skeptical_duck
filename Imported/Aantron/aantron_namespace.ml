(*

#use"Imported/Aantron/aantron_namespace.ml";;

*)


(* adapted from Markup.ml/ at https://github.com/aantron/markup.ml *)

  (* open Aantron_markup_common *)

   type 'a cps = 'a Aantron_markup_common.cps;;

   let xmlns_ns = Aantron_markup_common.xmlns_ns ;;
   let xml_ns = Aantron_markup_common.xml_ns ;;

   let list_map_cps : ('a -> 'b cps) -> 'a list -> 'b list cps =
       fun f l throw k ->
   
     let rec loop accumulator = function
       | [] -> k (List.rev accumulator)
       | x::l -> f x throw (fun x' -> loop (x'::accumulator) l)
     in
     loop [] l ;;
   
   module Parsing =
   struct
     type context_entry =
       {f        : string -> string option;
        previous : context_entry} ;;
   
     type context = context_entry ref ;;
   
     let parse qualified_name =
       try
         let colon_index = String.index qualified_name ':' in
         let prefix = String.sub qualified_name 0 colon_index in
         let suffix =
           String.sub qualified_name
             (colon_index + 1)
             (String.length qualified_name - colon_index - 1)
         in
         prefix, suffix
   
       with Not_found -> ("", qualified_name) ;;
   
     let init top_level =
       let f = function
         | "xml" -> Some xml_ns
         | "xmlns" -> Some xmlns_ns
         | s -> top_level s
       in
       let rec entry = {f; previous = entry} in
       ref entry ;;
   
     let expand_element report context raw_element_name throw k =
       let ns, name = parse raw_element_name in
       match !context.f ns with
       | Some uri -> k (uri, name)
       | None ->
         match ns with
         | "" -> k ("", name)
         | prefix ->
           report () (`Bad_namespace prefix) throw (fun () -> k (prefix, name)) ;;
   
     let push report context raw_element_name raw_attributes throw k =
       let parsed_attributes =
         raw_attributes |> List.map (fun (name, value) -> parse name, value) in
   
       let f =
         parsed_attributes |> List.fold_left (fun f -> function
           | ("xmlns", prefix), uri ->
             (fun p -> if p = prefix then Some uri else f p)
           | ("", "xmlns"), uri ->
             (fun p -> if p = "" then Some uri else f p)
           | _ -> f)
           !context.f
       in
   
       let entry = {f; previous = !context} in
       context := entry;
   
       expand_element report context raw_element_name throw
         (fun expanded_element_name ->
       list_map_cps begin fun (name, value) _ k ->
         match name with
         | "", "xmlns" -> k ((xmlns_ns, "xmlns"), value)
         | "", name -> k (("", name), value)
         | ns, name ->
           match f ns with
           | Some uri -> k ((uri, name), value)
           | None ->
             report () (`Bad_namespace ns) throw (fun () -> k ((ns, name), value))
       end parsed_attributes throw (fun expanded_attributes ->
       k (expanded_element_name, expanded_attributes))) ;;
   
     let pop ({contents = {previous}} as context) =
       context := previous ;;
   end ;;
   
   module StringMap = struct

    type key = String.t ;;

    type 'a t =
        Empty
      | Node of {l:'a t; v:key; d:'a; r:'a t; h:int} ;;

    let height = function
        Empty -> 0
      | Node {h} -> h ;;

    let create l x d r =
      let hl = height l and hr = height r in
      Node{l; v=x; d; r; h=(if hl >= hr then hl + 1 else hr + 1)} ;;

    let singleton x d = Node{l=Empty; v=x; d; r=Empty; h=1} ;;

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node {h} -> h in
      let hr = match r with Empty -> 0 | Node {h} -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Map.bal"
        | Node{l=ll; v=lv; d=ld; r=lr} ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Map.bal"
              | Node{l=lrl; v=lrv; d=lrd; r=lrr}->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Map.bal"
        | Node{l=rl; v=rv; d=rd; r=rr} ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Map.bal"
              | Node{l=rll; v=rlv; d=rld; r=rlr} ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        Node{l; v=x; d; r; h=(if hl >= hr then hl + 1 else hr + 1)} ;;

    let empty = Empty ;;

    let is_empty = function Empty -> true | _ -> false ;;

    let rec add x data = function
        Empty ->
          Node{l=Empty; v=x; d=data; r=Empty; h=1}
      | Node {l; v; d; r; h} as m ->
          let c =  String.compare x v in
          if c = 0 then
            if d == data then m else Node{l; v=x; d=data; r; h}
          else if c < 0 then
            let ll = add x data l in
            if l == ll then m else bal ll v d r
          else
            let rr = add x data r in
            if r == rr then m else bal l v d rr ;;

    let rec find x = function
        Empty ->
          raise Not_found
      | Node {l; v; d; r} ->
          let c =  String.compare x v in
          if c = 0 then d
          else find x (if c < 0 then l else r) ;;

    let rec find_first_aux v0 d0 f = function
        Empty ->
          (v0, d0)
      | Node {l; v; d; r} ->
          if f v then
            find_first_aux v d f l
          else
            find_first_aux v0 d0 f r ;;

    let rec find_first f = function
        Empty ->
          raise Not_found
      | Node {l; v; d; r} ->
          if f v then
            find_first_aux v d f l
          else
            find_first f r ;;

    let rec find_first_opt_aux v0 d0 f = function
        Empty ->
          Some (v0, d0)
      | Node {l; v; d; r} ->
          if f v then
            find_first_opt_aux v d f l
          else
            find_first_opt_aux v0 d0 f r ;;

    let rec find_first_opt f = function
        Empty ->
          None
      | Node {l; v; d; r} ->
          if f v then
            find_first_opt_aux v d f l
          else
            find_first_opt f r ;;

    let rec find_last_aux v0 d0 f = function
        Empty ->
          (v0, d0)
      | Node {l; v; d; r} ->
          if f v then
            find_last_aux v d f r
          else
            find_last_aux v0 d0 f l ;;

    let rec find_last f = function
        Empty ->
          raise Not_found
      | Node {l; v; d; r} ->
          if f v then
            find_last_aux v d f r
          else
            find_last f l ;;

    let rec find_last_opt_aux v0 d0 f = function
        Empty ->
          Some (v0, d0)
      | Node {l; v; d; r} ->
          if f v then
            find_last_opt_aux v d f r
          else
            find_last_opt_aux v0 d0 f l ;;

    let rec find_last_opt f = function
        Empty ->
          None
      | Node {l; v; d; r} ->
          if f v then
            find_last_opt_aux v d f r
          else
            find_last_opt f l ;;

    let rec find_opt x = function
        Empty ->
          None
      | Node {l; v; d; r} ->
          let c =  String.compare x v in
          if c = 0 then Some d
          else find_opt x (if c < 0 then l else r) ;;

    let rec mem x = function
        Empty ->
          false
      | Node {l; v; r} ->
          let c =  String.compare x v in
          c = 0 || mem x (if c < 0 then l else r) ;;

    let rec min_binding = function
        Empty -> raise Not_found
      | Node {l=Empty; v; d} -> (v, d)
      | Node {l} -> min_binding l ;;

    let rec min_binding_opt = function
        Empty -> None
      | Node {l=Empty; v; d} -> Some (v, d)
      | Node {l}-> min_binding_opt l ;;

    let rec max_binding = function
        Empty -> raise Not_found
      | Node {v; d; r=Empty} -> (v, d)
      | Node {r} -> max_binding r ;;

    let rec max_binding_opt = function
        Empty -> None
      | Node {v; d; r=Empty} -> Some (v, d)
      | Node {r} -> max_binding_opt r ;;

    let rec remove_min_binding = function
        Empty -> invalid_arg "Map.remove_min_elt"
      | Node {l=Empty; r} -> r
      | Node {l; v; d; r} -> bal (remove_min_binding l) v d r ;;

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          bal t1 x d (remove_min_binding t2) ;;

    let rec remove x = function
        Empty ->
          Empty
      | (Node {l; v; d; r} as m) ->
          let c =  String.compare x v in
          if c = 0 then merge l r
          else if c < 0 then
            let ll = remove x l in if l == ll then m else bal ll v d r
          else
            let rr = remove x r in if r == rr then m else bal l v d rr ;;

    let rec update x f = function
        Empty ->
          begin match f None with
          | None -> Empty
          | Some data -> Node{l=Empty; v=x; d=data; r=Empty; h=1}
          end
      | Node {l; v; d; r; h} as m ->
          let c =  String.compare x v in
          if c = 0 then begin
            match f (Some d) with
            | None -> merge l r
            | Some data ->
                if d == data then m else Node{l; v=x; d=data; r; h}
          end else if c < 0 then
            let ll = update x f l in
            if l == ll then m else bal ll v d r
          else
            let rr = update x f r in
            if r == rr then m else bal l v d rr ;;

    let rec iter f = function
        Empty -> ()
      | Node {l; v; d; r} ->
          iter f l; f v d; iter f r ;;

    let rec map f = function
        Empty ->
          Empty
      | Node {l; v; d; r; h} ->
          let l' = map f l in
          let d' = f d in
          let r' = map f r in
          Node{l=l'; v; d=d'; r=r'; h} ;;

    let rec mapi f = function
        Empty ->
          Empty
      | Node {l; v; d; r; h} ->
          let l' = mapi f l in
          let d' = f v d in
          let r' = mapi f r in
          Node{l=l'; v; d=d'; r=r'; h} ;;

    let rec fold f m accu =
      match m with
        Empty -> accu
      | Node {l; v; d; r} ->
          fold f r (f v d (fold f l accu)) ;;

    let rec for_all p = function
        Empty -> true
      | Node {l; v; d; r} -> p v d && for_all p l && for_all p r ;;

    let rec exists p = function
        Empty -> false
      | Node {l; v; d; r} -> p v d || exists p l || exists p r ;;

    (* Beware: those two functions assume that the added k is *strictly*
       smaller (or bigger) than all the present keys in the tree; it
       does not test for equality with the current min (or max) key.

       Indeed, they are only used during the "join" operation which
       respects this precondition.
    *)

    let rec add_min_binding k x = function
      | Empty -> singleton k x
      | Node {l; v; d; r} ->
        bal (add_min_binding k x l) v d r ;;

    let rec add_max_binding k x = function
      | Empty -> singleton k x
      | Node {l; v; d; r} ->
        bal l v d (add_max_binding k x r) ;;

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)

    let rec join l v d r =
      match (l, r) with
        (Empty, _) -> add_min_binding v d r
      | (_, Empty) -> add_max_binding v d l
      | (Node{l=ll; v=lv; d=ld; r=lr; h=lh},
         Node{l=rl; v=rv; d=rd; r=rr; h=rh}) ->
          if lh > rh + 2 then bal ll lv ld (join lr v d r) else
          if rh > lh + 2 then bal (join l v d rl) rv rd rr else
          create l v d r ;;

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. *)

    let concat t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          join t1 x d (remove_min_binding t2) ;;

    let concat_or_join t1 v d t2 =
      match d with
      | Some d -> join t1 v d t2
      | None -> concat t1 t2 ;;

    let rec split x = function
        Empty ->
          (Empty, None, Empty)
      | Node {l; v; d; r} ->
          let c =  String.compare x v in
          if c = 0 then (l, Some d, r)
          else if c < 0 then
            let (ll, pres, rl) = split x l in (ll, pres, join rl v d r)
          else
            let (lr, pres, rr) = split x r in (join l v d lr, pres, rr) ;;

    let rec merge f s1 s2 =
      match (s1, s2) with
        (Empty, Empty) -> Empty
      | (Node {l=l1; v=v1; d=d1; r=r1; h=h1}, _) when h1 >= height s2 ->
          let (l2, d2, r2) = split v1 s2 in
          concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
      | (_, Node {l=l2; v=v2; d=d2; r=r2}) ->
          let (l1, d1, r1) = split v2 s1 in
          concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
      | _ ->
          assert false ;;

    let rec union f s1 s2 =
      match (s1, s2) with
      | (Empty, s) | (s, Empty) -> s
      | (Node {l=l1; v=v1; d=d1; r=r1; h=h1},
         Node {l=l2; v=v2; d=d2; r=r2; h=h2}) ->
          if h1 >= h2 then
            let (l2, d2, r2) = split v1 s2 in
            let l = union f l1 l2 and r = union f r1 r2 in
            match d2 with
            | None -> join l v1 d1 r
            | Some d2 -> concat_or_join l v1 (f v1 d1 d2) r
          else
            let (l1, d1, r1) = split v2 s1 in
            let l = union f l1 l2 and r = union f r1 r2 in
            match d1 with
            | None -> join l v2 d2 r
            | Some d1 -> concat_or_join l v2 (f v2 d1 d2) r ;;

    let rec filter p = function
        Empty -> Empty
      | Node {l; v; d; r} as m ->
          (* call [p] in the expected left-to-right order *)
          let l' = filter p l in
          let pvd = p v d in
          let r' = filter p r in
          if pvd then if l==l' && r==r' then m else join l' v d r'
          else concat l' r' ;;

    let rec filter_map f = function
        Empty -> Empty
      | Node {l; v; d; r} ->
          (* call [f] in the expected left-to-right order *)
          let l' = filter_map f l in
          let fvd = f v d in
          let r' = filter_map f r in
          begin match fvd with
            | Some d' -> join l' v d' r'
            | None -> concat l' r'
          end ;;

    let rec partition p = function
        Empty -> (Empty, Empty)
      | Node {l; v; d; r} ->
          (* call [p] in the expected left-to-right order *)
          let (lt, lf) = partition p l in
          let pvd = p v d in
          let (rt, rf) = partition p r in
          if pvd
          then (join lt v d rt, concat lf rf)
          else (concat lt rt, join lf v d rf) ;;

    type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration ;;

    let rec cons_enum m e =
      match m with
        Empty -> e
      | Node {l; v; d; r} -> cons_enum l (More(v, d, r, e)) ;;

    let compare cmp m1 m2 =
      let rec compare_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> 0
        | (End, _)  -> -1
        | (_, End) -> 1
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            let c =  String.compare v1 v2 in
            if c <> 0 then c else
            let c = cmp d1 d2 in
            if c <> 0 then c else
            compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in compare_aux (cons_enum m1 End) (cons_enum m2 End) ;;

    let equal cmp m1 m2 =
      let rec equal_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> true
        | (End, _)  -> false
        | (_, End) -> false
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
             String.compare v1 v2 = 0 && cmp d1 d2 &&
            equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in equal_aux (cons_enum m1 End) (cons_enum m2 End) ;;

    let rec cardinal = function
        Empty -> 0
      | Node {l; r} -> cardinal l + 1 + cardinal r ;;

    let rec bindings_aux accu = function
        Empty -> accu
      | Node {l; v; d; r} -> bindings_aux ((v, d) :: bindings_aux accu r) l ;;

    let bindings s =
      bindings_aux [] s ;;

    let choose = min_binding ;;

    let choose_opt = min_binding_opt ;;

    let add_seq i m =
      Seq.fold_left (fun m (k,v) -> add k v m) m i ;;

    let of_seq i = add_seq i empty ;;

    let rec seq_of_enum_ c () = match c with
      | End -> Seq.Nil
      | More (k,v,t,rest) -> Seq.Cons ((k,v), seq_of_enum_ (cons_enum t rest)) ;;

    let to_seq m =
      seq_of_enum_ (cons_enum m End) ;;

    let to_seq_from low m =
      let rec aux low m c = match m with
        | Empty -> c
        | Node {l; v; d; r; _} ->
            begin match  String.compare v low with
              | 0 -> More (v, d, r, c)
              | n when n<0 -> aux low r c
              | _ -> aux low l (More (v, d, r, c))
            end
      in
      seq_of_enum_ (aux low m End) ;;
end ;;

   
   module Writing =
   struct
     type context_entry =
       {namespace_to_prefix : string list StringMap.t;
        prefix_to_namespace : string StringMap.t;
        previous            : context_entry} ;;
   
     type context = context_entry ref * (string -> string option) ;;
   
     let init top_level =
       let namespace_to_prefix =
         StringMap.empty
         |> StringMap.add "" [""]
         |> StringMap.add xml_ns ["xml"]
         |> StringMap.add xmlns_ns ["xmlns"]
       in
   
       let prefix_to_namespace =
         StringMap.empty
         |> StringMap.add "" ""
         |> StringMap.add "xml" xml_ns
         |> StringMap.add "xmlns" xmlns_ns
       in
   
       let rec entry =
         {namespace_to_prefix; prefix_to_namespace; previous = entry} in
   
       ref entry, top_level ;;
   
     let lookup report allow_default context namespace throw k =
       let candidate_prefixes =
         try StringMap.find namespace !(fst context).namespace_to_prefix
         with Not_found -> []
       in
   
       let prefix =
         try
           Some (candidate_prefixes |> List.find (fun prefix ->
             (allow_default || prefix <> "") &&
              begin
               try StringMap.find prefix !(fst context).prefix_to_namespace =
                 namespace
               with Not_found -> false
              end))
         with Not_found -> None
       in
   
       let prefix =
         match prefix with
         | Some _ -> prefix
         | None ->
           match snd context namespace with
           | None -> None
           | Some prefix ->
             if not allow_default && prefix = "" ||
                 StringMap.mem prefix !(fst context).prefix_to_namespace then
               None
             else Some prefix
       in
   
       match prefix with
       | None -> report () (`Bad_namespace namespace) throw (fun () -> k "")
       | Some prefix -> k prefix ;;
   
     let format prefix name =
       match prefix with
       | "" -> name
       | prefix -> prefix ^ ":" ^ name ;;
   
     let unexpand_element report context (namespace, name) throw k =
       lookup report true context namespace throw (fun prefix ->
       k (format prefix name)) ;;
   
     let unexpand_attribute report context ((namespace, name), value) throw k =
       match namespace with
       | "" -> k (name, value)
       | uri ->
         if uri = xmlns_ns && name = "xmlns" then k ("xmlns", value)
         else
           lookup report false context namespace throw (fun prefix ->
             k (format prefix name, value)) ;;
   
     let extend k v map =
       let vs =
         try StringMap.find k map
         with Not_found -> []
       in
       StringMap.add k (v::vs) map ;;
   
     let push report context element_name attributes throw k =
       let namespace_to_prefix, prefix_to_namespace =
         attributes |> List.fold_left (fun (ns_to_prefix, prefix_to_ns) -> function
           | (ns, "xmlns"), uri when ns = xmlns_ns ->
             extend uri "" ns_to_prefix,
             StringMap.add "" uri prefix_to_ns
           | (ns, prefix), uri when ns = xmlns_ns ->
             extend uri prefix ns_to_prefix,
             StringMap.add prefix uri prefix_to_ns
           | _ -> ns_to_prefix, prefix_to_ns)
           (!(fst context).namespace_to_prefix, !(fst context).prefix_to_namespace)
       in
   
       let entry =
         {namespace_to_prefix; prefix_to_namespace; previous = !(fst context)} in
       (fst context) := entry;
   
       unexpand_element report context element_name throw (fun element_name ->
       list_map_cps (unexpand_attribute report context) attributes throw
         (fun attributes ->
       k (element_name, attributes))) ;;
   
     let pop ({contents = {previous}}, _ as context) =
       (fst context) := previous ;;
   end ;;
