(*

#use"transient_hashtbl.ml";;

Like an ordinary hashtbl, but where few key/value pairs need to be remembered at a given time,
so that when the list of pairs reaches its maximal size and a new element is added, 
the oldest element is thrown out.

*)

module Private = struct 

let salt="Transient_"^"hashtbl.";;
let max_size_label = salt ^ "max_size";;
let pairs_label = salt ^ "pairs";;

let of_concrete_object a_of_crobj b_of_crobj crobj= 
   let g = Concrete_object_field.get_record crobj in 
   {
      Transient_hashtbl_t.max_size = Concrete_object_field.unwrap_int (g max_size_label);
      pairs = Concrete_object_field.to_pair_list a_of_crobj b_of_crobj (g pairs_label)
   };;

let to_concrete_object a_to_crobj b_to_crobj tbl= 
   Concrete_object_t.Record([
     max_size_label, Concrete_object_t.Int(tbl.Transient_hashtbl_t.max_size); 
     pairs_label, Concrete_object_field.of_pair_list a_to_crobj b_to_crobj tbl.Transient_hashtbl_t.pairs;
   ]);;

end ;;

let add tbl key vaal=
   let old_pairs = tbl.Transient_hashtbl_t.pairs in  
   let new_pairs =(
       if List.length(old_pairs)< tbl.Transient_hashtbl_t.max_size 
       then (key,vaal)::old_pairs 
       else (key,vaal)::(List.rev(List.tl(List.rev(old_pairs))))
   ) in 
   tbl.Transient_hashtbl_t.pairs<-new_pairs;;

let create size= {Transient_hashtbl_t.max_size = size;pairs=[]};;

let find tbl key = List.assoc key tbl.Transient_hashtbl_t.pairs ;;   

let mem tbl key = List.exists (fun pair->fst(pair)=key) tbl.Transient_hashtbl_t.pairs ;;

let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;