open Skeptical_duck_lib ;; 
open Needed_values ;;

(************************************************************************************************************************
 Entry 8 : Careful renaming
************************************************************************************************************************)
module Snip8 = struct 

let example_length = 3 ;;
let example_string = "abcdefghijk" ;;

String.take_first example_length example_string = Cull_string.beginning example_length example_string;;
String.drop_first example_length example_string = Cull_string.cobeginning example_length example_string;;
String.take_last example_length example_string = Cull_string.ending example_length example_string;;
String.drop_last example_length example_string = Cull_string.coending example_length example_string;;

let aps1 = sv "Cull_string.cobeginning";;
let act1 () = List.iter (Replace_inside.replace_inside_file 
                         ("Cull_string.cobeginning","String.drop_first")) aps1 ;;

let aps2 = sv "Cull_string.coending";;
let act2 () = List.iter (Replace_inside.replace_inside_file 
                         ("Cull_string.coending","String.drop_last")) aps2 ;;

let old_name3 = "Cull_string.beginning";;
let aps3 = sv old_name3;;
let act3 () = List.iter (Replace_inside.replace_inside_file 
                         (old_name3,"String.take_first")) aps3 ;;

let old_name4 = "Cull_string.ending";;
let aps4 = sv old_name4;;
let act4 () = List.iter (Replace_inside.replace_inside_file 
                         (old_name4,"String.take_last")) aps4 ;;


let ap1 = Absolute_path.of_string "lib/cull_string.ml" ;; 

Replace_inside.replace_inside_file
("Private.String.drop_first","String.drop_first") ap1 ;; 

Replace_inside.replace_inside_file
("Private.String.take_first","String.take_first") ap1 ;; 
end;;

(************************************************************************************************************************
 Entry 7 : Using Fw_usual_manager.standalone_copy_from_subdirectories
************************************************************************************************************************)
module Snip7 = struct 


let dirs1 = Fw_usual_manager.all_subdirectories () ;;

let sd1 = Dfa_subdirectory_t.SD "lib/Cee_language" ;;

let sd1 = Dfa_subdirectory_t.SD "lib/Lr_parsing" ;;
let z1 = Fw_usual_manager.standalone_copy_from_subdirectories [sd1] ~copy_name:"lr_parsing";;
let u1 = Image.image Dfa_module.to_line (fst z1) ;;
let act1 () = fgs u1 ;;



end;;

(************************************************************************************************************************
 Entry 6 :  Change configuration manually
************************************************************************************************************************)
module Snip6 = struct 
   
let old_fw_git = (!(Fw_usual_manager.Private.main_ref)) ;;

let old_fw_config = Fwc_with_githubbing.Inherited.to_fw_configuration old_fw_git ;;

let new_fw_config = {
   old_fw_config with 
   Fwg_configuration.ignored_subdirectories = 
     Fw_constant.git_ignored_subdirectories
} ;;


let new_fw_git = Fwc_with_githubbing.set_fw_configuration old_fw_git new_fw_config ;;

Fw_persisting.persist new_fw_git ;;


end;;

(************************************************************************************************************************
 Entry 5 : Diary template
************************************************************************************************************************)
module Snip5 = struct 

let text1 = rf "watched/watched_and_githubbed/diary_archive.ml" ;;

let lines1 = Lines_in_text.lines text1 ;;

let snip k =
  let sk = string_of_int k in 
  String.concat "\n"
  [
  "(************************************************************************************************************************";
   " Entry "^sk^" : ";
   "************************************************************************************************************************)";
   "module Snip"^sk^" = struct "; ""; "  let var"^sk^" = "^sk^" ;;"; ""; "end;;\n"
  ]  ;;

let text1 = String.concat "\n" ("open Skeptical_duck_lib ;; "::"open Needed_values ;; "::""::(Int_range.scale (fun j->snip(8-j)) 1 7)) ;;  

Io.overwrite_with Manage_diary.Private.Common.usual_container text1 ;;


end;;

(************************************************************************************************************************
 Entry 4 : Debug Manage_diary module
************************************************************************************************************************)
module Snip4 = struct 

let bad1 = Manage_diary.remove_at_indices [2;3] ;;

let fn = Manage_diary.Private.Common.usual_container ;;

let indices = [2;3] ;;

open Manage_diary.Private ;;
let bad2 = With_container.remove_at_indices fn indices ;;

let old_dy = Parse.parse_whole_diary(Io.read_whole_file fn) ;;

let new_dy = Modify.remove_at_indices old_dy indices ;;

let unindexed_retained_ones = List.filter_map (
    fun ent -> 
       if List.mem ent.index indices 
       then None 
       else Some(Modify.unindexed_version ent) 
  ) old_dy.entries ;;





end;;

(************************************************************************************************************************
 Entry 3 : Compare two files line by line
************************************************************************************************************************)
module Snip3 = struct 

let ap1 = Absolute_path.of_string ((Sys.getenv "HOME")^"/Teuliou/html_files/Text_files/candidatures.txt") ;;

let ap3 = Absolute_path.of_string ((Sys.getenv "HOME")^"/Teuliou/html_files/Text_files/candy3.txt") ;;
let text1 = Io.read_whole_file ap1 ;;
let text3 = Io.read_whole_file ap3 ;;

let lines_in_text1 = Lines_in_text.lines text1 ;;

let lines_in_text3 = Lines_in_text.lines text3 ;;

let paired_lines = List_again.best_effort_combine lines_in_text1 lines_in_text3 ;;

let indexed_paired_lines = Int_range.index_everything paired_lines ;;

let bad_paired_lines = List.drop_while (fun (idx,(line1,line2))->line1=line2) indexed_paired_lines ;;

let v1 = List.take 20 bad_paired_lines ;;

let look1 () = print_string( "\n\n\n" ^(String.concat "\n" (Image.image (fun (idx,(line1,line2))->line1) v1)) ^ "\n\n\n") ;;

let look2 () = print_string( "\n\n\n" ^(String.concat "\n" (Image.image (fun (idx,(line1,line2))->line2) v1)) ^ "\n\n\n") ;;


(*
open Job_hunting.Private ;;

let (chaotic_beginning_of_list,naive_list) = extract_naive_list_of_dated_items text1 ;;

let regular_start_of_list = List.take_while (fun (_date,company_items)->List.length(company_items)<=2) naive_list
and irregular_end_of_list = List.drop_while (fun (_date,company_items)->List.length(company_items)<=2) naive_list ;;


*)



end;;

(************************************************************************************************************************
 Entry 2 : Another failed attempt to simplify the Departement graph
************************************************************************************************************************)
module Snip2 = struct 



let p_order = Total_ordering.product Total_ordering.for_integers Total_ordering.lex_for_strings ;;

let p_forget_order = Ordered.to_list  ;;
let p_intersect = Ordered.intersect p_order ;;

let p_sort = Ordered.sort p_order ;;


let vg1 = Departements.graph ;; 

let vg2 = Undirected_graph.merge_group_of_vertices vg1 
   [ (75, "Paris");(92, "Hauts-de-Seine"); (93, "Seine-Saint-Denis"); (94, "Val-de-Marne")]
    (75, "Paris") ;;

let vg3 = Undirected_graph.rename_vertices vg2 (fun (idx,nm)->(10*idx,nm)) ;; 

let vg4 = Undirected_graph.split_vertex vg3 ~original:(300, "Gard") (301, "Gard1") (302, "Gard2") ;;

let vg5 = Undirected_graph.remove_edges vg4 
  [
    ((301, "Gard1"),(70, "Ardèche"));((301, "Gard1"), (840, "Vaucluse"));
    ((302, "Gard2"),(120, "Aveyron"));((302, "Gard2"),(340, "Hérault"));((302, "Gard2"),(2070, "Mor Kreizdouarel"));
  ] ;;
let check1_on_vg5 =
   let ng = Undirected_graph.neighbors vg5 in 
   (ng (301, "Gard1"),ng (302, "Gard2"))  ;;

let vg6 = Undirected_graph.split_vertex vg5 ~original:(380, "Isère") (381, "Isère1") (382, "Isère2") ;;

let vg7 = Undirected_graph.remove_edges vg6 
  [
    ((381, "Isère1"),(10, "Ain"));((381, "Isère1"),(50, "Hautes-Alpes"));((381, "Isère1"),(730, "Savoie") );
    ((382, "Isère2"),(70, "Ardèche"));((382, "Isère2"),(420, "Loire"));
  ] ;;
let check1_on_vg7 =
   let ng = Undirected_graph.neighbors vg7 in 
   (ng (381, "Isère1"),ng (382, "Isère2"))  ;;   

let vg8 = Undirected_graph.split_vertex vg7 ~original:(840, "Vaucluse") (841, "Vaucluse1") (842, "Vaucluse2") ;;

let vg9 = Undirected_graph.remove_edges vg8 
  [
    ((841, "Vaucluse1"), (40, "Alpes-de-Haute-Provence")); ((841, "Vaucluse1"),(130, "Bouches-du-Rhône") );
    ((842, "Vaucluse2"),(70, "Ardèche"));
  ] ;;
let check1_on_vg9 =
   let ng = Undirected_graph.neighbors vg9 in 
   (ng (841, "Vaucluse1"),ng (842, "Vaucluse2"))  ;;

let vg10 = Undirected_graph.split_vertex vg9 ~original:(40, "Alpes-de-Haute-Provence") (41, "Alpes-de-Haute-Provence1") (42, "Alpes-de-Haute-Provence2") ;;

let vg11 = Undirected_graph.remove_edges vg10 
  [
    ((41, "Alpes-de-Haute-Provence1"),(830, "Var") );
    ((42, "Alpes-de-Haute-Provence2"),(260, "Drôme"));
  ] ;;
let check1_on_vg11 =
   let ng = Undirected_graph.neighbors vg11 in 
   (ng (41, "Alpes-de-Haute-Provence1"),ng (42, "Alpes-de-Haute-Provence2"))  ;;

let vg12 = Undirected_graph.split_vertex vg11 ~original:(270, "Eure") (271, "Eure1") (274, "Eure4") ;;

let vg13 = Undirected_graph.split_vertex vg12 ~original:(274, "Eure4") (272, "Eure2") (273, "Eure3") ;;

let vg14 = Undirected_graph.remove_edges vg13 
  [
    ((271, "Eure1"),(140, "Calvados"));((271, "Eure1"),(280, "Eure-et-Loir"));((271, "Eure1"),(610, "Orne"));
    ((272, "Eure2"),(140, "Calvados"));((272, "Eure2"),(600, "Oise"));((272, "Eure2"),(760, "Seine-Maritime"));((272, "Eure2"),(950, "Val-d'Oise"));
    ((273, "Eure3"),(280, "Eure-et-Loir"));((273, "Eure3"),(600, "Oise"));((273, "Eure3"),(950, "Val-d'Oise"));

    ((271, "Eure1"),(272, "Eure2"));

  ] ;;
let check1_on_vg14 =
   let ng = Undirected_graph.neighbors vg14 in 
   (ng (271, "Eure1"),ng (272, "Eure2"),ng (273, "Eure3"))  ;;

let vg15 = Undirected_graph.split_vertex vg14 ~original:(490, "Maine-et-Loire") (491, "Maine-et-Loire1") (492, "Maine-et-Loire2") ;;

let vg16 = Undirected_graph.remove_edges vg15 
  [
    ((491, "Maine-et-Loire1"),(350, "Ile-et-Vilaine"));((491, "Maine-et-Loire1"),(440, "Loire-Atlantique"));((491, "Maine-et-Loire1"),(530, "Mayenne"));((491, "Maine-et-Loire1"),(720, "Sarthe"));
    ((491, "Maine-et-Loire1"),(850, "Vendée"));
    ((492, "Maine-et-Loire2"),(860, "Vienne"));
  ] ;;
let check1_on_vg16 =
   let ng = Undirected_graph.neighbors vg16 in 
   (ng (491, "Maine-et-Loire1"),ng (492, "Maine-et-Loire2"))  ;;   



let ug1 = vg16 ;;

let mainland_deps  = List.filter (fun (k,nm)->
  not(List.mem nm ["Corse"; "Suisse"; "Deutschland"; "Luxembourg"; "Belgique"; "Mor Atlantel";
 "Bro-Spagn"; "Mor Kreizdouarel"; "Italia"])
  ) (Undirected_graph.all_vertices ug1) ;; 

let ug2 = Undirected_graph.restrict ug1 mainland_deps ;;

let nline7 = ["Ardennes"; "Marne"; "Aube"; "Yonne"; "Nièvre"; "Allier"; "Puy-de-Dôme";
 "Corrèze"; "Lot"; "Tarn-et-Garonne"; "Haute-Garonne"] ;;

let line7 = 
  let vs = Undirected_graph.all_vertices ug2 in 
  Image.image (fun nm->List.find (fun p->snd(p)=nm) vs) nline7 ;;


let ug3 = Undirected_graph.remove_vertices ug2 line7 ;;

let components1 = Undirected_graph.connected_components ug3 ;;

let strict_south_west_departements = List.nth components1 1 ;;
let strict_north_east_departements = List.nth components1 0 ;;

let south_west_departements = p_sort (line7 @ strict_south_west_departements) ;;
let north_east_departements = p_sort (line7 @ strict_north_east_departements) ;;

let south_west_ug = Undirected_graph.restrict ug2 (p_forget_order south_west_departements) ;; 

let north_east_ug = Undirected_graph.restrict ug2 (p_forget_order north_east_departements) ;; 

let pre_line8 = Undirected_graph.set_level_neighborhood north_east_ug line7 ;;

let check1_on_line8 = Undirected_graph.vertices_with_degree_at_least_three north_east_ug pre_line8 ;;

let comps2 = Undirected_graph.decompose_into_cycles north_east_ug pre_line8 ;;

let line8 = List.rev(List.nth comps2 0) ;;

let check2_on_line8 = (List.length line8)=(List.length pre_line8) ;;

let north_east_ug2 = Undirected_graph.remove_vertices north_east_ug line7 ;; 

let pre_line9 = Undirected_graph.set_level_neighborhood north_east_ug2 line8 ;;

let check1_on_line9 = Undirected_graph.vertices_with_degree_at_least_three north_east_ug2 pre_line9 ;;

let comps3 = Undirected_graph.decompose_into_cycles north_east_ug pre_line9 ;;

let line9 = (List.rev(List.nth comps3 0)) @ (List.nth comps3 1) ;;

let check2_on_line9 = (List.length line9)=(List.length pre_line9) ;;

let north_east_ug3 = Undirected_graph.remove_vertices north_east_ug2 line8 ;; 

let pre_line10 = Undirected_graph.set_level_neighborhood north_east_ug3 line9 ;;

let check1_on_line10 = Undirected_graph.vertices_with_degree_at_least_three north_east_ug3 pre_line10 ;;

let comps4 = Undirected_graph.decompose_into_cycles north_east_ug pre_line10 ;;

let line10 = (List.rev(List.nth comps4 1)) @ (List.rev(List.nth comps4 0)) ;;

let check2_on_line10 = (List.length line10)=(List.length pre_line10) ;;

let north_east_ug4 = Undirected_graph.remove_vertices north_east_ug3 line9 ;; 

let pre_line11 = Undirected_graph.set_level_neighborhood north_east_ug4 line10 ;;

let check1_on_line11 = Undirected_graph.vertices_with_degree_at_least_three north_east_ug4 pre_line11 ;;

let comps5 = Undirected_graph.decompose_into_cycles north_east_ug pre_line11 ;;

let line11 = List.nth comps5 0 ;;

let check2_on_line11 = (List.length line11)=(List.length pre_line11) ;;

let north_east_ug5 = Undirected_graph.remove_vertices north_east_ug4 line10 ;; 

let pre_line12 = Undirected_graph.set_level_neighborhood north_east_ug5 line11 ;;

let check1_on_line12 = Undirected_graph.vertices_with_degree_at_least_three north_east_ug5 pre_line12 ;;

let line12 = pre_line12 ;;

let north_east_lines = [line7;line8;line9;line10;line11;line12] ;;

let north_east1 = p_sort(Undirected_graph.all_vertices north_east_ug) ;;

let north_east2 = p_sort(List.flatten north_east_lines) ;;

let check1_north_east = (north_east1=north_east2) ;;
let check2_north_east = (Ordered.length north_east2 = (Basic.fold_sum (Image.image List.length north_east_lines) )) ;;

let pre_line6 = Undirected_graph.set_level_neighborhood south_west_ug line7 ;;

let check1_on_line6 = Undirected_graph.vertices_with_degree_at_least_three south_west_ug pre_line6 ;;

let camps2 = Undirected_graph.decompose_into_cycles south_west_ug pre_line6 ;;

let line6 = (List.nth camps2 0) ;;
let check2_on_line6 = (List.length line6)=(List.length pre_line6) ;;

let south_west_ug2 = Undirected_graph.remove_vertices south_west_ug line7 ;; 

let pre_line5 = Undirected_graph.set_level_neighborhood south_west_ug2 line6 ;;

let check1_on_line5 = Undirected_graph.vertices_with_degree_at_least_three south_west_ug pre_line5 ;;

let camps3 = Undirected_graph.decompose_into_cycles south_west_ug pre_line5 ;;

let line5 = (List.nth camps3 0) ;;
let check2_on_line5 = (List.length line5)=(List.length pre_line5) ;;

let south_west_ug3 = Undirected_graph.remove_vertices south_west_ug2 line6 ;; 

let pre_line4 = Undirected_graph.set_level_neighborhood south_west_ug3 line5 ;;

let check1_on_line4 = Undirected_graph.vertices_with_degree_at_least_three south_west_ug pre_line4 ;;

let camps4 = Undirected_graph.decompose_into_cycles south_west_ug pre_line4 ;;

let line4 = (List.nth camps4 0) ;;
let check2_on_line4 = (List.length line4)=(List.length pre_line4) ;;

let south_west_ug4 = Undirected_graph.remove_vertices south_west_ug3 line5 ;; 

let pre_line3 = Undirected_graph.set_level_neighborhood south_west_ug4 line4 ;;

let check1_on_line3 = Undirected_graph.vertices_with_degree_at_least_three south_west_ug pre_line3 ;;

let camps5 = Undirected_graph.decompose_into_cycles south_west_ug pre_line3 ;;

let line3 = (List.nth camps5 0) ;;
let check2_on_line3 = (List.length line3)=(List.length pre_line3) ;;

let south_west_ug5 = Undirected_graph.remove_vertices south_west_ug4 line4 ;; 

let pre_line2 = Undirected_graph.set_level_neighborhood south_west_ug5 line3 ;;

let check1_on_line2 = Undirected_graph.vertices_with_degree_at_least_three south_west_ug pre_line2 ;;

let camps6 = Undirected_graph.decompose_into_cycles south_west_ug pre_line2 ;;

let line2 = (List.nth camps6 0) ;;
let check2_on_line2 = (List.length line2)=(List.length pre_line2) ;;

let south_west_ug6 = Undirected_graph.remove_vertices south_west_ug5 line3 ;;

let pre_line1 = Undirected_graph.set_level_neighborhood south_west_ug6 line2 ;;

let check1_on_line1 = Undirected_graph.vertices_with_degree_at_least_three south_west_ug pre_line1 ;;

let line1 = List.rev pre_line1 ;;

let camps7 = Undirected_graph.decompose_into_cycles south_west_ug pre_line1 ;;

let line1 = (List.nth camps7 0) ;;
let check2_on_line1 = (List.length line1)=(List.length pre_line1) ;;

let south_west_nigels = [line7;line6;line5;line4;line3;line2;line1] ;;

let south_west1 = p_sort(Undirected_graph.all_vertices south_west_ug) ;;

let south_west2 = p_sort(List.flatten south_west_nigels) ;;

let check1_south_west = (south_west1=south_west2) ;;
let check2_south_west = (Ordered.length south_west2 = (Basic.fold_sum (Image.image List.length south_west_nigels) )) ;;

let all_lines= Image.image (fun l->
   let temp = Undirected_graph.decompose_into_cycles ug2 l in 
   List_again.pepper [0,"Blank"] temp
  )
[line1;line2;line3;line4;line5;line6;line7;line8;line9;line10;line11;line12];;

let w1 = [] :: (all_lines) @ [[]] ;;

let w2 = List_again.universal_tridelta_list w1 ;;

let relative_measure x plank =
  let nes = Undirected_graph.neighbors ug2 x in 
  let m = List.length(List.filter(fun p->List.mem p nes) plank) in 
  if m<2 then 1 else m-1 ;;

let two_sided_analysis left x right =
   (relative_measure x left,x,relative_measure x right) ;;

let w3 = Image.image (fun (left,deps,right)->
  let core =Image.image (fun dep->two_sided_analysis left dep right) deps in 
  (core,(Image.image(fun (x,y,z)->x) core,Image.image(fun (x,y,z)->z) core))
) w2 ;;

let ww k = List.nth w3 (k-1) ;; 

(*

let ap1 = Absolute_path.of_string "watched/watched_not_githubbed/pan.ml";;

let act1 () = Replace_inside.replace_several_inside_file [
   "nigel7","virginia1";
   "nigel6","virginia2";
   "nigel5","virginia3";
   "nigel4","virginia4";
   "nigel3","virginia5";
   "nigel2","virginia6";
   "line1","virginia7";
   "line2","virginia8";
   "line3","virginia9";
   "line4","virginia10";
   "line5","virginia11";
   "line6","virginia12";
] ap1 ;; 

let act2 () = Replace_inside.replace_several_inside_file [
   "virginia","line";
] ap1 ;; 

let u1 = Int_range.scale (fun k->"line"^(string_of_int k)) 1 12 ;;

let u2 = "\n\n\n["^(String.concat ";" u1)^"];;\n\n\n" ;;

let w3 = Image.image (fun (left,deps,right)->
  
  
) w2 ;;

#use"watched/watched_not_githubbed/pan.ml";;

 (Undirected_graph.decompose_into_cycles ug2) 

*)


end;;

(************************************************************************************************************************
 Entry 1 : Mock hash tables for debugging purposes
************************************************************************************************************************)
module Snip1 = struct 

  module Hushtbl = struct 

type ('a,'b) t =  HH of 'a * ((('a* 'b) list) ref) ;;

let create m forb= HH(forb,ref []) ;;

let add (HH (forb,l)) x y = 
  if x=forb
  then failwith("badbadbad !!!")  
  else (l:=(x,y)::(!l))  ;;

let find_opt (HH (_,l)) x = List.assoc_opt x (!l);;

end ;;  


end;;
