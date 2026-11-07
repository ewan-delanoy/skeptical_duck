(*

#use"lib/Text_editing/Pgfmath_writer/pgfmath_types.ml";;

*)

type variable_name = VN of variable_name ;;

type variable_definition =
    Leaf of float 
   |Sum of variable_name * variable_name 
   |Substract of variable_name * variable_name
   |Dot of float * variable_name ;;

type environment = DE of (variable_name * (variable_definition * float)) list ;; 
