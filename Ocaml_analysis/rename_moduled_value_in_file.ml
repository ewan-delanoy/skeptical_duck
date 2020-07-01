(*

#use"Ocaml_analysis/rename_moduled_value_in_file.ml";;

*)

exception No_module_given of string;;
exception No_value_with_name of string;;

let rename_moduled_value_in_file preceding_files old_name new_name path=
   let j=Substring.leftmost_index_of_in "." old_name in
   if j<0 
   then raise(No_module_given(old_name))
   else 
   let module_name=Cull_string.beginning (j-1) old_name in
   let temp3=Read_ocaml_files.read_ocaml_files preceding_files in
   let opt_temp4=Option.seek (fun itm->
     (itm.Ocaml_gsyntax_item.name)=old_name
   ) temp3 in
   if opt_temp4=None
   then raise(No_value_with_name(old_name))
   else
   let temp4=Option.unpack(opt_temp4) in
   let (i1,j1)=temp4.Ocaml_gsyntax_item.interval_for_name in
   let _=Overwrite_at_intervals.inside_file [(i1,j1),new_name] path in
   let temp3_again=Read_ocaml_files.read_ocaml_files preceding_files in
   let beheaded_name=Cull_string.cobeginning j old_name in
   let s_new_beheaded_name=(fun (fa,nn)->if fa="" then nn else fa^"."^nn)
   (Cull_string.before_rightmost beheaded_name '.',Overwriter.to_string new_name) in
   let new_beheaded_name=Overwriter.of_string s_new_beheaded_name in
   let s_new_full_name=module_name^"."^s_new_beheaded_name in
   let temp4_again=Listennou.force_find (fun itm->
     (itm.Ocaml_gsyntax_item.name)=s_new_full_name
   ) temp3_again in
   let k1=Listennou.find_index temp4_again temp3_again in
   let temp5=Listennou.big_tail k1 temp3_again in
   let temp6=Option.filter_and_unpack(
      fun itm->
        let txt=itm.Ocaml_gsyntax_item.content in
        let ttemp7=Isolated_occurrences.of_in 
           beheaded_name txt in
        if ttemp7<>[]
        then  let isoc=Isolated_occurrences.of_in beheaded_name txt in
              let replacings=Image.vorstellung (fun p->(p,new_beheaded_name)) isoc in
              let new_txt=Overwrite_at_intervals.inside_string
                   replacings txt in
             Some(itm.Ocaml_gsyntax_item.interval_for_content,
                  Overwriter.of_string new_txt)
        else None   
   ) temp5 in
   Overwrite_at_intervals.inside_file temp6 path;;
