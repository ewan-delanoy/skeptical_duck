(*

#use"three_parts.ml";;

*)




let generic=function
[]->[]
|u::v->
let rec tempf=
(function
((graet,x,da_ober),accu)->
let accu2=(graet,x,da_ober)::accu in
match da_ober with
[]->accu2
|a::b->tempf((x::graet,a,b),accu2)
) in
tempf(([],u,v),[]);;

let complemented_points l=List.rev_map(function (kleiz,x,dehou)->
(x,List.rev_append(kleiz)(dehou)))
(generic l);;

let beheaded_tails l=List.rev_map (function (kleiz,x,dehou)->(x,dehou) )(generic l);;

let select_center_element_and_reverse_left f l=
  let rec tempf=(fun (graet,da_ober)->match da_ober with
  []->(graet,None,[])
  |x::peurrest->if f x 
                then (graet,Some(x),peurrest)
                else tempf(x::graet,peurrest)
  ) in
  tempf([],l);;

let select_center_element f l=
  let (temp1,opt,after)=select_center_element_and_reverse_left f l in 
  (List.rev temp1,opt,after);;

let decompose_according_to_markers f l =
  let rec tempf=(
     fun (treated,to_be_treated)->
       let (before,opt,after)=select_center_element f to_be_treated in 
       if opt=None then (List.rev treated,before) else 
       tempf((before,Option.unpack opt)::treated,after)
  ) in 
  tempf([],l);;


(*

decompose_according_to_markers (fun x->List.mem x [3;7;11]) 
[1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20];;

*)  

let select_left_interval f l=
  (* note that the "interval" is returned in reverse form *)
  let rec tempf=(fun (graet,da_ober)->match da_ober with
  []->(graet,[])
  |x::peurrest->if f x 
                then tempf(x::graet,peurrest) 
                else (graet,da_ober)
  ) in
  tempf([],l);;

let select_center_interval f l=
  let rec tempf=(fun (graet,da_ober)->match da_ober with
  []->(List.rev graet,[],[])
  |x::peurrest->if f x 
                then let (temp1,temp2)=select_left_interval f da_ober in
                     (List.rev graet,List.rev(temp1),temp2)
                else tempf(x::graet,peurrest)
  ) in
  tempf([],l);;


let replace_in_list replacee replacer l=
  let (temp1,opt,temp2)=select_center_element (fun t->t=replacee) l in
  if opt=None then l else List.rev_append temp1 (replacer@temp2);;
   
           