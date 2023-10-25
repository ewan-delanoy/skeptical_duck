(*

#use"lib/detect_inside.ml";;

*)

let occurrences_for_several_words_in_several_files words filenames = 
   let contents_of_file = Memoized.make(fun fn->
      Io.read_whole_file (Absolute_path.of_string fn)
    ) in 
   let cart = Cartesian.product words filenames in 
   let temp1 = List.filter_map (
     fun (word,fn) ->
        let text = contents_of_file fn in 
        let temp1 = Substring.decorated_occurrences_of_in word text in 
        if temp1 = []
        then None 
        else Some (Image.image (fun explanation->((word,fn),explanation)) temp1)  
   ) cart in 
   List.flatten temp1 ;; 
