(*

#use"transient_hashtbl_t.ml";;

Like an ordinary hashtbl, but where few key/value pairs need to be remembered at a given time,
so that when the list of pairs reaches its maximal size and a new element is added, 
the oldest element is thrown out.

*)

type ('a,'b) t= {
    max_size : int ;
    mutable pairs : ('a * 'b) list;
};;

           