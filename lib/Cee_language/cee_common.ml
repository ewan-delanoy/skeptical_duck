(*

#use"lib/Cee_language/cee_common.ml";;

*)

let str_order = Total_ordering.lex_for_strings ;;
let str_mem = Ordered.mem str_order ;; 

let i_order = Total_ordering.for_integers ;;
let i_merge = Ordered.merge i_order ;; 


let included_local_file_opt line =
  if not(String.starts_with line ~prefix:"#")
  then None  
  else  
  match Strung.char_finder_from_inclusive_opt (fun c->
          not(List.mem c [' ';'\t';'\r'])
        ) line 2 with 
  None -> None
  |Some idx1 -> 
  if not(Substring.is_a_substring_located_at "include" line idx1)
  then None
  else   
  match Strung.char_finder_from_inclusive_opt (fun c->
        not(List.mem c [' ';'\t';'\r'])
  ) line (idx1+7) with 
  None -> None
  |Some idx2 -> 
  if (Strung.get line idx2)<>'"'
  then None
  else    
  match Strung.char_finder_from_inclusive_opt (fun c->
           c = '"'
  ) line (idx2+1) with 
  None -> None
  |Some idx3 ->      
    Some (Cull_string.interval line (idx2+1) (idx3-1))
    ;;
let sil_merge ox oy=
  let rec tempf=(function (u,v,accu)->
  if u=[] then (List.rev_append(accu)(v)) else
  if v=[] then (List.rev_append(accu)(u)) else
  let (xu,lxu)=List.hd(u) and yu=List.tl(u) 
  and (xv,lxv)=List.hd(v) and yv=List.tl(v) in
  match str_order(xu)(xv) with
    Total_ordering_result_t.Lower->tempf(yu,v,(xu,lxu)::accu)
  |Total_ordering_result_t.Equal->tempf(yu,yv,(xu,i_merge lxu lxv)::accu)
  |Total_ordering_result_t.Greater->tempf(u,yv,(xv,lxv)::accu)
  ) in
  tempf(ox,oy,[]);;

let rec sil_sort  x=
  if List.length(x)<2
  then x
  else 
  let temp1=Partition_list.split_in_half(x) in
  let y1=sil_sort(fst temp1)
  and y2=sil_sort(snd temp1) in
  sil_merge y1 y2;;      

let included_local_files_in_text text = 
  let temp1 = Lines_in_string.indexed_lines text in 
  let temp2 = List.filter_map (
    fun (line_idx,line) ->
      Option.map (fun included_fn ->
          (included_fn,[line_idx])
      ) (included_local_file_opt line)
  ) temp1 in 
  sil_sort temp2 ;;

let included_local_files_in_file ap =
  included_local_files_in_text (Io.read_whole_file ap) ;; 