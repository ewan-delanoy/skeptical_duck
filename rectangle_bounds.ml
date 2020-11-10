(*

#use"rectangle_bounds.ml";;

*)

let combine 
  (Rectangle_bounds_t.B(xmin1,xmax1,ymin1,ymax1))
    (Rectangle_bounds_t.B(xmin2,xmax2,ymin2,ymax2)) = 
    (Rectangle_bounds_t.B(
        max xmin1 xmin2,
        min xmax1 xmax2,
        max ymin1 ymin2,
        min ymax1 ymax2)) ;;

let enumerate (Rectangle_bounds_t.B(xmin,xmax,ymin,ymax)) = 
     Cartesian.product (Ennig.ennig xmin xmax) (Ennig.ennig ymin ymax) ;; 

let test  (Rectangle_bounds_t.B(xmin,xmax,ymin,ymax)) (x,y)=
  (xmin<=x) && (x<=xmax) && (ymin <=y) && (y<= ymax) ;;


