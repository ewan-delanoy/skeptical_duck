(*

#use"lib/Ocaml_preprocessing/opor_public_definition.ml";;

*)

module Private = struct 

let order = ((fun anndef1 anndef2 ->
      let val_name1 = anndef1.Opor_public_definition_t.value_name 
      and val_name2 = anndef2.Opor_public_definition_t.value_name in 
      let trial1 = Total_ordering.lex_for_strings val_name1 val_name2 in 
      if trial1 <> Total_ordering_result_t.Equal then trial1 else    
                   Total_ordering.standard anndef1 anndef2 
      ) :>  Opor_public_definition_t.t Total_ordering_t.t ) ;;   
                  
      
end ;;      
      
      
let expand anndef =
      String.concat "\n" anndef.Opor_public_definition_t.lines_in_definition ;;
      
let expand_list l =
      let temp1 = Ordered.sort Private.order l in 
      String.concat "\n" (Image.image expand temp1) ;;
      
let order = Private.order ;;
       
      