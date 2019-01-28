(*

#use"Text_shortening/main_shortener_t.ml";;

*)

type t= {
   exceptional_cases : (string*string) list;
   first_touches : (string*string) list;
   prefix_shortenings : (string*string) list;
   infix_shortenings : (string*string) list;
   suffix_shortenings : (string*string) list;
};;