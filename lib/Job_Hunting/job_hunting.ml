(*

#use"lib/Job_Hunting/job_hunting.ml";;


*)

module Private = struct 

let comma = Naive_parser_example.fixed_string "," ;;
let postponed_comma = Naive_parser.postpone comma ;;


let short_company_name = 
  Naive_parser.heavy_map (fun text idx data->
    let (_,(new_idx,_)) = data in 
    Cull_string.interval text idx (new_idx-1)
  )
  (Naive_parser.concat2 
    Naive_parser_example.uppercase_character postponed_comma) ;;
  
(*

Naive_parser.try_parse_at_index short_company_name "Marcia baila, 567" 1 ;;
Naive_parser.try_parse_at_index short_company_name "1Marcia baila, 567" 2 ;;

*)

let linebreak = Naive_parser_example.fixed_string "\n" ;;

let company_name = Naive_parser.map snd 
(Naive_parser.concat2 linebreak short_company_name) ;;

let postponed_company_name = Naive_parser.postpone company_name ;;

(*

Naive_parser.try_parse_at_index postponed_company_name "123\nMarcia baila, 45\nShe did it, 7777\nThiis is the way ," 1 ;;


*)

let company_names = Naive_parser.concat_star_of_postponed_with_stopper 
   company_name Jh_date.parser ;;

(*

Naive_parser.try_parse_at_index company_names 
  "123\nMarcia baila, 45\nShe did it, 1/2/23\nThiis is the way ," 1 ;;


*)

let dated_item = 
  Naive_parser.concat2 Jh_date.parser company_names ;;

(*

Naive_parser.try_parse_at_index dated_item 
  "45/67/98\nMarcia baila, 45\nShe did it, 1/2/23\nThiis is the way ," 1 ;;

*)

let postponed_dated_item = Naive_parser.postpone dated_item ;;

let star_of_dated_items = Naive_parser.star dated_item ;;

let dated_items = 
  Naive_parser.map (
    fun ((_,one),several) -> one :: several
  ) (Naive_parser.concat2
  postponed_dated_item star_of_dated_items) ;;

(*

Naive_parser.try_parse_at_index dated_items 
  "45/67/98\nMarcia baila, 45\nShe did it, 1/2/23\nThiis is the way ," 1 ;;

Naive_parser.try_parse_at_index dated_items 
  ("45/67/98\nMarcia baila, 45\nShe did it, 1/2/23\nThiis is the way , aaa"^
   "84/56/79\nTara baila, 45\nI did it, 12/3/20\nThiis is the way , bbb") 1 ;;  

*)



let order_on_dated_items = 
  Total_ordering.product Jh_date.order Total_ordering.lex_for_strings ;;

let read_list text = 
   match Naive_parser.try_parse_at_index dated_items text 1 with 
   None -> []
   |(Some (l,_)) ->
     let temp1 = Image.image (fun (date,names)->Image.image (fun name->(date,name)) names) l in 
     let temp2 = List.flatten temp1 in 
     let temp3 = Ordered.sort order_on_dated_items temp2 in 
     let temp4 = List.rev_map (
       fun (date,name) -> (name,(date,Jh_date.number_of_days_between (Jh_date.today ()) date))
     ) temp3 in 
     temp4 ;;

let recompute_usual_list_from_scratch () = 
  let ap = Absolute_path.of_string ((Sys.getenv "HOME")^"//Teuliou/html_files/Text_files/candidatures.txt") in 
  let text = Io.read_whole_file ap in 
  read_list text ;;

let ref_for_usual_list = ref None ;;

let usual_list () =
   match !ref_for_usual_list with 
   (Some old_answer) -> old_answer 
    |None ->
      let new_answer = recompute_usual_list_from_scratch () in 
      ref_for_usual_list:=Some(new_answer);
      new_answer ;;

let analize l =
   let ul =usual_list () in 
   let temp1 = Image.image (
    fun name -> (name,List.assoc_opt name ul)
   ) l in 
   let (good_ones1,bad_ones1) = List.partition (
    fun (_,opt) -> opt = None
   ) temp1 in 
   let good_ones2 = Image.image fst good_ones1 in 
   let bad_ones2 = Image.image (fun (name,opt)->(fst(Option.get opt),name))  bad_ones1 in 
   let bad_ones3 = Ordered.sort order_on_dated_items bad_ones2 in 
   let bad_ones4 = Image.image (
       fun (date,name) -> (name,(date,Jh_date.number_of_days_between date (Jh_date.today ())))
     ) bad_ones3 in 
   (good_ones2,bad_ones4) ;;  


  (*
let z1 = recompute_usual_list_from_scratch () ;;

  let ap = Absolute_path.of_string ((Sys.getenv "HOME")^"//Teuliou/html_files/Text_files/candidatures.txt") ;;
  let text = Io.read_whole_file ap ;;

  let z2 = Naive_parser.try_parse_at_index dated_items text 1 ;;

  let (_,guilty_index) = Option.get z2 ;;

  let itv = Cull_string.interval text (guilty_index-10) (guilty_index+500) ;;

  let z3 = Cull_string.cobeginning (guilty_index-1) text ;;

  let z4 = Naive_parser.try_parse_at_index dated_item z3 1;;

  let (z5,i1) = Option.get(Naive_parser.try_parse_at_index Jh_date.parser z3 1);;

  let z6 = Naive_parser.try_parse_at_index company_names z3 i1 ;;

  let z7 = Naive_parser.try_parse_at_index company_name z3 (i1+1) ;;

  let z8 = Naive_parser.try_parse_at_index short_company_name z3 (i1+2) ;;

*)  

end ;;  

let analize = Private.analize ;;
