(*

#use"lib/explicit.ml";;

*)

module Private = struct 

let iter0 (f:'a->unit) l (left,right)=
  let n=List.length(l)
  and accu=ref(l) in
  let s0=" of "^string_of_int(n)^" "^right^"\n" in
  for j=1 to n
               do
               ( f(List.hd(!accu));
                 accu:=List.tl(!accu);
                 print_string(left^" : "^string_of_int(j)^s0);
                 flush stdout)
               done;;

let iter2 (f:'a->'a) initial_value tester (shower:'a->string)  addenda=
  let accu=ref(initial_value) in
  let _=(while tester(!accu)
               do
               ( 
                 accu:=f(!accu);
                 print_string((shower (!accu))^addenda);
                 flush stdout;
               )
               done) in
  !accu;;

let e_rev job_description l=
   let accu=ref([]) in 
   let f=(fun x->accu:=x::(!accu)) in
   let _=iter0(f)(l)(job_description," (rev part)") in
   !accu;;    

let unchronometered_explore_tree f l=
    let g=(fun (graet,p,q,da_ober)->
       match da_ober with
       []->(graet,0,0,[])
       |a::peurrest->
         let temp1=f a in
         if temp1=[]
         then (a::graet,p-1,q+1,peurrest)
         else (a::graet,p-1+List.length(temp1),q+1,temp1@peurrest)
    ) and 
    tester=(fun (_graet,_p,_q,da_ober)->da_ober<>[]) 
    and
    shower=(
     fun (_graet,p,q,_da_ober)->
       (string_of_int p)^" to be explored ; "^
       (string_of_int q)^" already explored\n"
    )
    in
    let initial_value=([],List.length l,0,l) in
    let _=(print_string(shower(initial_value));flush stdout) in
    let (ans,_,_,_)=iter2 g initial_value tester shower  "" in
    ans;;



let unchronometered_filter job_description f l=
   let accu=ref([]) in 
   let g=(fun x->if f(x) then accu:=x::(!accu) else ()) in
   let _=iter0(g)(l)(job_description," (filter part)") in
   e_rev job_description (!accu);;    
 
 
 let unchronometered_image job_description f l=
   let accu=ref([]) in 
   let g=(fun x->accu:=f(x)::(!accu)) in
   let _=iter0(g)(l)(job_description," (rev_image part)") in
   e_rev job_description (!accu);;  
   

let unchronometered_image_computed_backwards job_description f l=
   let temp1=e_rev job_description l in
    let accu=ref([]) in 
   let g=(fun x->accu:=f(x)::(!accu)) in
   let _=iter0(g)(temp1)(job_description," (image part)") in
   (!accu);;     
 
  

exception Force_find_exn ;;

let rec helper_for_opt_finding (f,sn) (j,x)=
   match x with 
   [] -> None
   |a::others -> if f a 
                 then Some a
                 else let _=(
                        print_string("Item number "^string_of_int(j)^" of "^sn^" found wanting \n");
                        flush stdout) in 
                      helper_for_opt_finding (f,sn) (j+1,others) ;; 

end ;; 

let explore_tree f l=Chronometer.it (Private.unchronometered_explore_tree f) l;; 
           
let filter ?(job_description="") f l=Chronometer.it (Private.unchronometered_filter job_description f) l;; 


let image ?(job_description="") f l=Chronometer.it (Private.unchronometered_image job_description f) l;;  

let image_computed_backwards ?(job_description="") f l=Chronometer.it 
   	(Private.unchronometered_image_computed_backwards job_description f) l;;               

let opt_find f x = Private.helper_for_opt_finding (f,string_of_int(List.length x)) (1,x) ;;

(* opt_find (fun t->t>4) (Ennig.ennig 1 7);; *)
