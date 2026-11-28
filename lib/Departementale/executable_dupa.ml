(*

#use"lib/Departementale/executable_dupa.ml";;

*)

module Private = struct

exception Empty_plan ;;

type data_for_incompletely_mastered_deps = {
   number_of_days_before_next_rare_visit : int ;
   number_of_rare_visits_already_made : int ;
} ;;

type mastering_status = 
     Fully_mastered 
    |Not_mastered_yet of data_for_incompletely_mastered_deps ;;

type departement = Dep of int * string ;;

type visit = Vi of departement * bool ;;

type pftf_element = Pftfe of (departement * int);;

type plan_for_the_future = Pftf of pftf_element list ;;

type visit_result =
    Downfall_after_full_mastery 
   |Downfall_after_incomplete_mastery 
   |Next_rare_visit_is_now_possible of int 
   |Effective_repetitive_visit_made of bool * int
   |Effective_rare_visit_made of int
   |Familiarity_reached 
   |Full_mastery_reached;; 

type mastering_item = Msti of departement * mastering_status;; 

type mastering_manager = Mama of mastering_item list ;;

type world = {
  mastering_data : mastering_manager ;
  plan : plan_for_the_future ; 
  timer : int ;
  news_archive : (departement * visit_result) list;
} ;;

let required_number_of_days_between_rare_visits = 15 ;;

let required_number_of_rare_visits_for_mastery = 5 ;; 



module Departement = struct 

let of_int k = 
   let name = List.assoc k Departements.indexed_names in 
   Dep(k,name);;

let to_string (Dep(k,name)) = 
  if k>200
  then name  
  else name ^ "(" ^ (string_of_int k) ^ ")" ;; 

let name (Dep(_k,nm)) = nm ;;

module Crobj = struct 

let of_concrete_object ccrt_obj= 
  let (k,nm) = Crobj_converter_combinator.to_pair Crobj_converter.int_of_concrete_object Crobj_converter.string_of_concrete_object ccrt_obj in
  Dep (k,nm);;

let to_concrete_object (Dep (k,nm))= Crobj_converter_combinator.of_pair 
          Crobj_converter.int_to_concrete_object Crobj_converter.string_to_concrete_object (k,nm);;

end ;;   

end ;;  

module Visit_result = struct 

exception Of_standardized_exn ;;  

let of_standardized (v,k)= match v with 
    1 -> Downfall_after_full_mastery 
   |2 -> Downfall_after_incomplete_mastery 
   |3 -> Next_rare_visit_is_now_possible(k) 
   |4 -> Effective_repetitive_visit_made(true,k) 
   |5 -> Effective_repetitive_visit_made(false,k) 
   |6 -> Effective_rare_visit_made(k) 
   |7 -> Familiarity_reached 
   |8 -> Full_mastery_reached 
   | _ -> raise Of_standardized_exn ;; 

let to_standardized = function 
    Downfall_after_full_mastery -> (1,0)
   |Downfall_after_incomplete_mastery -> (2,0)
   |Next_rare_visit_is_now_possible(k) -> (3,k)
   |Effective_repetitive_visit_made(bowl,k)->((if bowl then 4 else 5),k)
   |Effective_rare_visit_made(k) -> (6,k)
   |Familiarity_reached -> (7,0)
   |Full_mastery_reached -> (8,0);; 

module Crobj = struct 

let of_concrete_object ccrt_obj= 
  let (i,j) = Crobj_converter_combinator.to_pair Crobj_converter.int_of_concrete_object 
  Crobj_converter.int_of_concrete_object ccrt_obj in
  of_standardized (i,j);;

let to_concrete_object item =
   let (i,j) = to_standardized item in 
   Crobj_converter_combinator.of_pair 
          Crobj_converter.int_to_concrete_object Crobj_converter.int_to_concrete_object (i,j);;


end ;;   

end ;;  

module News_archive = struct 

module Crobj = struct 

let list_of_concrete_object ccrt_obj= 
  Crobj_converter_combinator.to_pair_list Departement.Crobj.of_concrete_object 
  Visit_result.Crobj.of_concrete_object ccrt_obj ;;

let list_to_concrete_object pair= 
  Crobj_converter_combinator.of_pair_list Departement.Crobj.to_concrete_object 
  Visit_result.Crobj.to_concrete_object pair ;;


end ;;  

let react_to_recent_news older_news recent_news = 
   let recent_deps = Image.image fst recent_news in 
   let retained_old_news = List.filter (
     fun (dep,_) -> not(List.mem dep recent_deps)
   ) older_news in 
   retained_old_news @ recent_news ;;


end ;;  

module Data_for_incompletely_mastered_deps = struct 

  module Crobj = struct 
  let salt = "Data_for_incompletely_mastered_deps." ;;
  let label_for_nod = salt ^ "number_of_days_before_next_rare_visit" ;;
  let label_for_nor  = salt ^ "number_of_rare_visits_already_made" ;;
  
  let of_concrete_object ccrt_obj = 
    let g=Concrete_object.get_record ccrt_obj in 
    {
      number_of_days_before_next_rare_visit =  Crobj_converter.int_of_concrete_object (g label_for_nod);
      number_of_rare_visits_already_made = Crobj_converter.int_of_concrete_object (g label_for_nor) ; 
    } ;;
  
  let to_concrete_object dt = 
   let items =  
   [
     label_for_nod, Crobj_converter.int_to_concrete_object dt.number_of_days_before_next_rare_visit ;
     label_for_nor, Crobj_converter.int_to_concrete_object dt.number_of_rare_visits_already_made ;
   ] in 
   Concrete_object_t.Record items ;;
  
  
end;; 

module Private = struct 

let write_days = function 
  0 -> "today"
  | 1 -> "tomorrow"
  | d -> "in "^(string_of_int d)^" days" ;;

end ;;  

let starter ={number_of_days_before_next_rare_visit=0 ; number_of_rare_visits_already_made=0;} ;;

let to_string dt = 
  let d = required_number_of_rare_visits_for_mastery - dt.number_of_rare_visits_already_made in 
  (string_of_int d)^" rare visits to go before mastery, starting "^(Private.write_days dt.number_of_days_before_next_rare_visit) ;; 

end ;;  

module Mastering_status = struct 

let of_standardized (is_full,data_for_incomplete_mastery) =
    if is_full then Fully_mastered else Not_mastered_yet (data_for_incomplete_mastery) ;;

let to_standardized = function 
    Fully_mastered -> (true,{number_of_days_before_next_rare_visit =0 ; number_of_rare_visits_already_made= 0 ;})
   |Not_mastered_yet (dt) ->  (false,dt);;


module Crobj = struct 

let of_concrete_object ccrt_obj= 
  let (b,dt) = Crobj_converter_combinator.to_pair Crobj_converter.bool_of_concrete_object 
  Data_for_incompletely_mastered_deps.Crobj.of_concrete_object ccrt_obj in
  of_standardized (b,dt);;

let to_concrete_object item =
   let (b,dt) = to_standardized item in 
   Crobj_converter_combinator.of_pair 
          Crobj_converter.bool_to_concrete_object Data_for_incompletely_mastered_deps.Crobj.to_concrete_object (b,dt);;

end ;;   


let to_string = function 
  Fully_mastered -> "mastered"
  |Not_mastered_yet dt ->  Data_for_incompletely_mastered_deps.to_string dt ;;

end ;;  

module Mastering_item = struct 

module Crobj = struct 

let of_concrete_object ccrt_obj= 
  let (dep,status) = Crobj_converter_combinator.to_pair Departement.Crobj.of_concrete_object Mastering_status.Crobj.of_concrete_object ccrt_obj in
  Msti (dep,status);;

let to_concrete_object (Msti (dep,status))= Crobj_converter_combinator.of_pair 
          Departement.Crobj.to_concrete_object Mastering_status.Crobj.to_concrete_object (dep,status);;

end ;;   

end ;;  


module Mastering_manager = struct 

module Crobj = struct 

let of_concrete_object ccrt_obj= Mama(Crobj_converter_combinator.to_list Mastering_item.Crobj.of_concrete_object ccrt_obj);;

let to_concrete_object  (Mama l)= Crobj_converter_combinator.of_list Mastering_item.Crobj.to_concrete_object l;;

end ;;   

let get (Mama l) dep=Option.get(List.find_map (fun(Msti(dep2,status)) ->if dep2=dep then Some status else None ) l) ;; 

let starter = Mama(Image.image ( 
    fun k->let dep=Departement.of_int k in 
    Msti(dep,Not_mastered_yet(Data_for_incompletely_mastered_deps.starter))
  )  Departements.from_southeast_to_northwest) ;;



end ;;   

module Visit = struct 

 let more_visits k =
   if k = 1 
   then "One more visit"
   else (string_of_int k)^" more visits" ;; 

 let more_rare_visits k =
   if k = 1 
   then "One more rare visit"
   else (string_of_int k)^" more rare visits" ;;   

let visited_again is_first_visit = if is_first_visit then "visited" else "revisited" ;;   

let message  = function
    Downfall_after_full_mastery -> "has fallen from heaven. How are the mighty fallen !"
   |Downfall_after_incomplete_mastery -> "has fallen and returned to zero."
   |Effective_repetitive_visit_made(is_first_visit,k) -> "has just been " ^ (visited_again is_first_visit) ^" successfully. "^(more_visits k)^" to go before familiarity."
   |Familiarity_reached -> "is now fully familiar."
   |Next_rare_visit_is_now_possible (k) -> "is now visitable. "^(more_rare_visits k)^" to go before mastery."
   |Effective_rare_visit_made(k) -> "has just been visited successfully in isolation. "^(more_rare_visits k)^" to go before mastery."
   |Full_mastery_reached -> "is now fully mastered. Congratulations !";; 

let tell (dep,v_result) = print_endline((Departement.to_string dep) ^ " " ^ (message v_result)) ;;

let tell_several l = List.iter tell l ;;
  
end ;;  


module Update_mastering_data = struct 


let start = {
          number_of_days_before_next_rare_visit = 0 ;
          number_of_rare_visits_already_made = 0 ;
        } ;;

let ordinary_day_goes_by dt =    
    let remaining_days = dt.number_of_days_before_next_rare_visit in 
    match remaining_days with 
    0 -> (Not_mastered_yet dt,None)
    | 1 -> (Not_mastered_yet {dt with number_of_days_before_next_rare_visit=0;},Some(Next_rare_visit_is_now_possible(required_number_of_rare_visits_for_mastery-dt.number_of_rare_visits_already_made)))
    |_-> (Not_mastered_yet {dt with number_of_days_before_next_rare_visit=remaining_days-1;},None);; 
   
let register_effective_visit dt = 
  let new_nbr_of_rare_visits = dt.number_of_rare_visits_already_made+1 in
  let remaining=required_number_of_rare_visits_for_mastery-new_nbr_of_rare_visits in  
  if remaining=0 
  then (Fully_mastered,Some Full_mastery_reached) 
  else (Not_mastered_yet({
          number_of_days_before_next_rare_visit = required_number_of_days_between_rare_visits ;
          number_of_rare_visits_already_made = new_nbr_of_rare_visits ;
        }),Some (Effective_rare_visit_made(remaining))) ;;

let react_to_visit_of_self dt visit_was_successful = 
  if not visit_was_successful 
  then let result_opt = (
         if dt = Data_for_incompletely_mastered_deps.starter 
         then None 
         else Some(Downfall_after_incomplete_mastery)
      ) in  
      (Not_mastered_yet start,result_opt)
  else 
  let remaining_days = dt.number_of_days_before_next_rare_visit in 
  if remaining_days > 0 (* the successful visit is too early *) 
  then   ordinary_day_goes_by dt  
  else  register_effective_visit dt ;;

let react_individually_to_visit (Vi(visited_dep,visit_was_successful)) (Msti(dep,mastering_dt))  = 
  match  mastering_dt with 
  Fully_mastered -> 
    if visited_dep <> dep 
    then (Fully_mastered,None)
    else if visit_was_successful 
         then  (Fully_mastered,None)
         else  (Not_mastered_yet(start),Some(Downfall_after_full_mastery))
  |Not_mastered_yet dt ->
    if visited_dep = dep 
    then react_to_visit_of_self dt visit_was_successful
    else ordinary_day_goes_by dt  ;;  

let react_collectively_to_visit visit items  =
  let temp1 = Image.image (fun item->(item,react_individually_to_visit visit item)) items in 
  let new_items = Image.image (fun (Msti(dep,_old_status),(new_status,_result_opt))->
      Msti(dep,new_status)
    ) temp1 
  and news = List.filter_map (fun (Msti(dep,_old_status),(_new_status,result_opt))->Option.map (fun res->(dep,res)) result_opt) temp1 in 
  (new_items,news)  ;;  

end ;;
   




module Pftf_element = struct 

module Crobj = struct 

let of_concrete_object ccrt_obj= 
  let (dep,nm) = Crobj_converter_combinator.to_pair Departement.Crobj.of_concrete_object Crobj_converter.int_of_concrete_object ccrt_obj in
  Pftfe (dep,nm);;

let to_concrete_object (Pftfe (dep,nm))= Crobj_converter_combinator.of_pair 
          Departement.Crobj.to_concrete_object Crobj_converter.int_to_concrete_object (dep,nm);;

end ;;   

end ;;  


module Plan_for_the_future = struct 

let required_number_of_repetitions_for_brainwashing = 5 ;; 

let starter =  Pftf(
  Image.image ( 
    fun k->let dep=Departement.of_int k in 
    Pftfe(dep,required_number_of_repetitions_for_brainwashing)
  )  Departements.from_southeast_to_northwest
) ;;

let react_to_promotion (Pftf l) promoted_dep = 
  Pftf(List.filter(fun (Pftfe(dep,_)) -> dep = promoted_dep) l);;

let react_to_demotion (Pftf l) demoted_dep = 
  let new_l = (
    if List.exists (fun (Pftfe(dep,_remaining_repetitions)) -> dep = demoted_dep) l
    then l 
    else l@[(Pftfe(demoted_dep,required_number_of_repetitions_for_brainwashing))]  
  ) in 
  Pftf(new_l);;

let react_to_visit (Pftf l) (Vi(visited_dep,visit_was_successful)) = 
  match l with 
  [] -> raise Empty_plan
  |(Pftfe(dep,remaining_repetitions))::other_elts ->
     if dep <> visited_dep 
     then (Pftf l,[])  
     else 
     let state_was_initial = (remaining_repetitions=required_number_of_repetitions_for_brainwashing) in 
     if not(visit_was_successful)
     then let new_pftf = Pftf((Pftfe(dep,required_number_of_repetitions_for_brainwashing))::other_elts) in 
          (new_pftf,(if state_was_initial then [] else [dep,Downfall_after_incomplete_mastery]))
     else
     let m=remaining_repetitions-1  in 
     if m=0
     then  (Pftf(other_elts),[dep,Familiarity_reached])
     else  
     let final_pftf = Pftf((Pftfe(dep,m))::other_elts) in  
    (final_pftf,[dep,Effective_repetitive_visit_made(state_was_initial,m)]);; 
   
let next_departement (Pftf l) = let (Pftfe(dep,_))= List.hd l in dep ;; 

let get_status (Pftf l) dep= List.find_map (fun (Pftfe(dep2,k))->if dep2=dep then Some k else None) l ;;

let write_visits = function 1 -> "one visit" |k->(string_of_int k)^" visits" 

let status_to_string = function 
   None -> "familiar"
   |Some k ->(write_visits k)^" before familiarity" ;;
   
let write_status pftf dep = status_to_string(get_status pftf dep) ;;   

module Crobj = struct 

let of_concrete_object ccrt_obj= Pftf (Crobj_converter_combinator.to_list Pftf_element.Crobj.of_concrete_object ccrt_obj);;

let to_concrete_object (Pftf l)= Crobj_converter_combinator.of_list Pftf_element.Crobj.to_concrete_object l;;

end ;;   

end ;;  

let abbreviations =
   (* distinguish Haute-Saône from Haute-Savoie *)
   [("am", "Alpes-Maritimes"); ("br", "Bas-Rhin"); ("bs", "Bro-Spagn");
   ("cm", "Charente-Maritime"); ("ds", "Deux-Sèvres"); ("ha", "Hautes-Alpes");
   ("hg", "Haute-Garonne"); ("hl", "Haute-Loire"); ("hm", "Haute-Marne");
   ("hp", "Hautes-Pyrénées"); ("hr", "Haut-Rhin"); ("hsn", "Haute-Saône");
   ("hsv", "Haute-Savoie"); ("hv", "Haute-Vienne"); ("la", "Loire-Atlantique");
   ("ma", "Mor Atlantel"); ("mk", "Mor Kreizdouarel");
   ("pa", "Pyrénées-Atlantiques"); ("po", "Pyrénées-Orientales");
   ("sm", "Seine-Maritime"); ("bdr", "Bouches-du-Rhône");
   ("cda", "Côtes-d'Armor"); ("cdo", "Côte-d'Or"); ("eel", "Eure-et-Loir");
   ("hds", "Hauts-de-Seine"); ("iel", "Indre-et-Loire");
   ("iev", "Ile-et-Vilaine"); ("lec", "Loir-et-Cher");
   ("leg", "Lot-et-Garonne"); ("mel", "Maine-et-Loire");
   ("mem", "Meurthe-et-Moselle"); ("pdc", "Pas-de-Calais");
   ("pdd", "Puy-de-Dôme"); ("sel", "Saône-et-Loire");
   ("sem", "Seine-et-Marne"); ("ssd", "Seine-Saint-Denis");
   ("tdb", "Territoire de Belfort"); ("teg", "Tarn-et-Garonne");
   ("vdm", "Val-de-Marne"); ("vdo", "Val-d'Oise");
   ("adhp", "Alpes-de-Haute-Provence")] ;;

let expand_abbreviations text =
    match List.assoc_opt text abbreviations with 
    None -> text 
    |Some expanded -> expanded ;;   



let rec helper_for_qa_session total_number_of_questions user_was_always_correct_so_far questions =
  match questions with
  | [] -> let _ = print_endline " Question finished. Press ENTER to go to next question." in 
          let _ = read_line () in
          Ansi_terminal.remove_last_lines (total_number_of_questions+4);
          user_was_always_correct_so_far
  | (question_idx, dep_opt) :: rest ->
      let input_from_user = Ansi_terminal.ask ~question:("Naighbor " ^ (string_of_int question_idx) ^ " : ") in
      let (expected,full_answer) = (match dep_opt with None->("","_(none)") |Some dep ->(Departement.name dep,Departement.to_string dep)) in 
      let user_was_correct = (expand_abbreviations(Cull_string.trim_spaces input_from_user) = expected) in 
      let feedback =(
      if user_was_correct
      then (if dep_opt=None
            then " Correct (the answer is : none)."
            else  "Correct : "^full_answer) 
      else "Wrong, the correct answer is: " ^ expected ^ " ") in   
      print_endline (feedback);
      helper_for_qa_session total_number_of_questions (user_was_always_correct_so_far && user_was_correct) rest  ;; 

let qa_session (w,dep) =
  let (Dep(dep_nbr,_dep_name)) = dep in   
  let temp1 = List.assoc dep_nbr Departements.rotating_neighbors in 
  let temp2 = Image.image (fun dep_nbr ->
     let dep_name = List.assoc dep_nbr Departements.indexed_names in 
     Some(Dep(dep_nbr,dep_name))  
  ) temp1 in 
  let mstatus = Mastering_manager.get w.mastering_data dep 
  and pstatus = Plan_for_the_future.write_status w.plan dep in   
  print_endline ("Current timer : " ^ (string_of_int w.timer));
  print_endline ("Current departement : " ^ (Departement.to_string dep) ^ " [ " ^ pstatus ^ ", " ^ (Mastering_status.to_string mstatus) ^ " ]");
  helper_for_qa_session  (List.length temp2) true (Int_range.index_everything (temp2@[None])) ;;

module World = struct 

   
let next_departement w = 
  let (Mama l)= w.mastering_data in 
  if w.timer mod 2 = 1
  then let m = List.length l in 
       let random_idx = Random.int m in 
       let (Msti(dep,_)) = List.nth l random_idx in 
       dep
  else Plan_for_the_future.next_departement w.plan ;;  

module Crobj = struct 
  let salt = "World." ;;
  let label_for_mastering_data = salt ^ "mastering_data" ;;
  let label_for_plan  = salt ^ "plan" ;;
  let label_for_timer  = salt ^ "timer" ;;
  let label_for_news_archive  = salt ^ "news_archive" ;;
  
  let of_concrete_object ccrt_obj = 
    let g=Concrete_object.get_record ccrt_obj in 
    {
    mastering_data =  Mastering_manager.Crobj.of_concrete_object (g label_for_mastering_data);
    plan = Plan_for_the_future.Crobj.of_concrete_object (g label_for_plan) ; 
    timer = Crobj_converter.int_of_concrete_object (g label_for_timer);
    news_archive = News_archive.Crobj.list_of_concrete_object(g label_for_news_archive);
   } ;;
  
  let to_concrete_object w = 
   let items =  
   [
     label_for_mastering_data, Mastering_manager.Crobj.to_concrete_object w.mastering_data ;
     label_for_plan, Plan_for_the_future.Crobj.to_concrete_object w.plan ;
     label_for_timer, Crobj_converter.int_to_concrete_object ( w.timer ) ;
     label_for_news_archive,News_archive.Crobj.list_to_concrete_object (w.news_archive) ;
   ] in 
   Concrete_object_t.Record items ;;
  
  
end;; 

let ng_nml = (Dfa_root.connectable_to_subpath Fw_big_constant.This_World.root) ^ 
(Dfa_subdirectory.connectable_to_subpath Fw_constant.nongithubbed_nonml_files_subdir) ;; 

let ap_for_persistent_storage = Absolute_path.create_file_if_absent (ng_nml^"departements_q_and_a.txt") ;;
let ap2_for_persistent_storage = Absolute_path.create_file_if_absent (ng_nml^"departements_q_and_a2.txt") ;;

let read () = 
  let text = Io.read_whole_file ap_for_persistent_storage in 
  let answer = Crobj.of_concrete_object(Crobj_parsing.parse(text)) in 
  let _ = Io.append_string_to_file ("Just read timer with value "^(string_of_int answer.timer)^"\n") ap2_for_persistent_storage  in 
  answer ;;

(* let read () = Crobj.of_concrete_object(Crobj_parsing.parse(Io.read_whole_file ap_for_persistent_storage)) ;; *)


let default_starter = {
    mastering_data = Mastering_manager.starter ;
    plan =  Plan_for_the_future.starter ; 
    timer = 0;
    news_archive=[];
   } ;;

let main_ref = ref (try read() with _->default_starter) ;; 


let write () = 
  let text = Crobj_parsing.unparse(Crobj.to_concrete_object (!main_ref)) in 
  let _ = Io.append_string_to_file ("Just wrote timer with value "^(string_of_int (!main_ref).timer)^"\n") ap2_for_persistent_storage  in 
  Io.overwrite_with ap_for_persistent_storage text;;

let react_to_visit w visit =
   let (Mama old_mastering_data) = w.mastering_data in 
   let (new_mastering_data,news1) = Update_mastering_data.react_collectively_to_visit visit old_mastering_data in
   let (new_plan,news2) = Plan_for_the_future.react_to_visit w.plan visit in 
   let recent_news = news1 @ news2 in
   let retained_news = News_archive.react_to_recent_news w.news_archive recent_news in  
   let _ = Ansi_terminal.remove_last_lines (List.length w.news_archive) in 
   let _ = Visit.tell_several (retained_news) in 
   ({
    mastering_data =  Mama new_mastering_data;
    plan = new_plan ; 
    timer = w.timer + 1;
    news_archive = retained_news ;
   },(recent_news,retained_news) );;

let update w =
   let dep = next_departement w in 
   let visit_was_successful = qa_session (w,dep) in 
   let visit = Vi(dep,visit_was_successful) in 
   react_to_visit w visit;;

let update_main_ref () =
    let (new_w,_) = update (!main_ref) in 
    main_ref:=new_w;
    print_endline "Before writing ...";
    write();
    print_endline "After writing ...";   ;; 

let reset () = (main_ref:=default_starter;write()) ;;

let go () = 
   let _ = (main_ref := read() ) in 
   while true do update_main_ref () done ;;



end ;;  


let main (_args:string array) = World.go ();;

end ;;

let main = Private.main ;;
