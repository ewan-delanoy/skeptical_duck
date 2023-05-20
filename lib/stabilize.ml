(*

#use"lib/stabilize.ml";;

*) 


module Private = struct 

  exception Pusher_for_exploring_enhanced_tree_exn ;;

  let pusher_for_exploring_enhanced_tree f (treated,to_be_treated) =
     match to_be_treated with 
     [] -> raise(Pusher_for_exploring_enhanced_tree_exn)
    |item :: others ->
       let pair = f item in 
       let (_extra_info,descendants) = pair in 
       ((item,pair)::treated,descendants@others) ;;
       
  let rec iterator_for_exploring_enhanced_tree f walker =
     let  (treated,_to_be_treated) = walker in 
     if treated = []
     then List.rev treated 
     else let next_walker = pusher_for_exploring_enhanced_tree f walker in 
          iterator_for_exploring_enhanced_tree f next_walker ;;   
       
  let explore_enhanced_tree f l = iterator_for_exploring_enhanced_tree f ([],l);;
  

end ;;  

let explore_enhanced_tree = Private.explore_enhanced_tree ;; 

let explore_tree f l0=
 let modified_l0=List.rev_map(function x->(x,0))(l0) in
 let modified_f=(function (x,j)->
   List.rev_map(function y->(y,j+1))(f x)
 ) in
 let rec tempf=(function (j,graet,da_ober)->
 match da_ober with
    []->graet
    |(xa,ia)::peurrest->let temp1=modified_f(xa,ia) in
                  let temp2=(j+1,xa::graet,List.rev_append temp1 peurrest) in
                  tempf(temp2)
 ) in
 tempf(0,[],modified_l0);;
 
 