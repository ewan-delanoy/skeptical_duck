(*

#use"lib/Java_project/jpr_constant.ml";;

*) 

module Private = struct 

let path_for_spring_5_3_with_boot_2_7 = 
   (Sys.getenv "HOME")^"/Downloads/my_springs/spring_0/" ;;


end ;;

let spring_5_3_with_boot_2_7 = 
   Jpr_types.Pr (Private.path_for_spring_5_3_with_boot_2_7) ;;


