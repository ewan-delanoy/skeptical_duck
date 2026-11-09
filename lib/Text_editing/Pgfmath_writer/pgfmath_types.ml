(*

#use"lib/Text_editing/Pgfmath_writer/pgfmath_types.ml";;

*)

type variable_name = VN of string ;;

type variable_definition =
    Leaf of float 
   |Sum of variable_name list 
   |Substract of variable_name * variable_name
   |Dot of float * variable_name ;;

type unnamed_point = UP of (variable_name * variable_name) * (float * float) ;;