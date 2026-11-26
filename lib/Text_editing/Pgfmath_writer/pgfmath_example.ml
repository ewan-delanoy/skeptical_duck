(*

#use"lib/Text_editing/Pgfmath_writer/pgfmath_example.ml";;

*)

open Pgfmath_types ;;

let ap = Absolute_path.of_string "~/Teuliou/LaTeX/Skeudennou_gant_Tikz/Brouilhedou/Lizherennou/lizherenneg.tex" ;;

let env_for_a = Pgfmath_environment.constructor 
   "A"
   ap 
   [
      (VN"zero",Leaf 0.);
      (VN"pedestalHeight",Leaf 2.);
      (VN"wingInnerHorizontalDim",Leaf 6.);
      (VN"wingHorizontalMove",Leaf 30.);
      (VN"wingVerticalMove",Leaf 80.);
      (VN"leftPedestalWidth",Leaf 25.);
      (VN"leftStartOfWingOnPedestal",Leaf 8.);
      (VN"leftEndOfWingOnPedestal",Sum[VN"leftStartOfWingOnPedestal";VN"wingInnerHorizontalDim"]);
      (VN"hatStart",Sum[VN"leftStartOfWingOnPedestal";VN"wingHorizontalMove"]);
      (VN"hatEnd",Sum[VN"leftEndOfWingOnPedestal";VN"wingHorizontalMove"]);
      (VN"hatY",Sum[VN"pedestalHeight";VN"wingVerticalMove"]);
      (VN"rightStartOfWingOnPedestal",Sum[VN"hatStart";VN"wingHorizontalMove"]);
      (VN"rightEndOfWingOnPedestal",Sum[VN"rightStartOfWingOnPedestal";VN"wingInnerHorizontalDim"]);
      (VN"rightInnerSpaceOnPedestal",Leaf 3.);
      (VN"shadowInnerHorizontalDim",Leaf 4.);
      (VN"shadowEndOnPedestal",Substract(VN"rightStartOfWingOnPedestal",VN"rightInnerSpaceOnPedestal"));
      (VN"shadowStartOnPedestal",Substract(VN"shadowEndOnPedestal",VN"shadowInnerHorizontalDim"));
      (VN"rightBeforeShadowOnPedestal",Leaf 10.);
      (VN"rightAfterWingOnPedestal",Leaf 9.);
      (VN"rightStartOfPedestal",Substract(VN"shadowStartOnPedestal",VN"rightBeforeShadowOnPedestal"));
      (VN"rightEndOfPedestal",Sum[VN"rightEndOfWingOnPedestal";VN"rightAfterWingOnPedestal"]);
      (VN"rightPedestalWidth",Sum[VN"rightBeforeShadowOnPedestal";VN"shadowInnerHorizontalDim";
                                  VN"rightEndOfWingOnPedestal";VN"wingInnerHorizontalDim";
                                  VN"rightAfterWingOnPedestal"]);
      (VN"shadowHorizontalMove",Dot(0.85,VN"wingHorizontalMove"));  
      (VN"shadowVerticalMove",Dot(0.85,VN"wingVerticalMove"));         
      (VN"shadowHatStart",Substract(VN"shadowStartOnPedestal",VN"shadowHorizontalMove")); 
      (VN"shadowHatEnd",Substract(VN"shadowEndOnPedestal",VN"shadowHorizontalMove"));    
      (VN"shadowY",Sum[VN"pedestalHeight";VN"shadowVerticalMove"]);   
      (VN"subwingHorizontalMove",Dot(0.26,VN"wingHorizontalMove"));     
      (VN"subwingVerticalMove",Dot(0.26,VN"wingVerticalMove"));    
      (VN"lowerBarMinimalX",Sum[VN"leftEndOfWingOnPedestal";VN"subwingHorizontalMove"]); 
      (VN"lowerBarY",Sum[VN"pedestalHeight";VN"subwingVerticalMove"]); 
      (VN"lowerBarMaximalX",Substract(VN"shadowStartOnPedestal",VN"subwingHorizontalMove")); 
      (VN"supwingHorizontalMove",Dot(0.31,VN"wingHorizontalMove"));     
      (VN"supwingVerticalMove",Dot(0.31,VN"wingVerticalMove"));    
      (VN"upperBarMinimalX",Sum[VN"leftEndOfWingOnPedestal";VN"supwingHorizontalMove"]); 
      (VN"upperBarY",Sum[VN"pedestalHeight";VN"supwingVerticalMove"]); 
      (VN"upperBarMaximalX",Substract(VN"shadowStartOnPedestal",VN"supwingHorizontalMove")); 
   ]  
   [
      ("A1",(VN"zero",VN"pedestalHeight"));
      ("A2",(VN"leftStartOfWingOnPedestal",VN"pedestalHeight"));
      ("A3",(VN"leftEndOfWingOnPedestal",VN"pedestalHeight"));
      ("A4",(VN"leftPedestalWidth",VN"pedestalHeight"));
      ("A5",(VN"leftPedestalWidth",VN"zero"));
      ("A6",(VN"zero",VN"zero"));
      ("A8",(VN"hatStart",VN"hatY"));
      ("A9",(VN"hatEnd",VN"hatY"));
      ("A10",(VN"rightStartOfPedestal",VN"pedestalHeight"));
      ("A11",(VN"shadowStartOnPedestal",VN"pedestalHeight"));
      ("A12",(VN"shadowEndOnPedestal",VN"pedestalHeight"));
      ("A13",(VN"rightStartOfWingOnPedestal",VN"pedestalHeight"));
      ("A14",(VN"rightEndOfWingOnPedestal",VN"pedestalHeight"));
      ("A15",(VN"rightEndOfPedestal",VN"pedestalHeight"));
      ("A16",(VN"rightEndOfPedestal",VN"zero"));
      ("A17",(VN"rightStartOfPedestal",VN"zero"));
      ("A18",(VN"shadowHatEnd",VN"shadowY"));
      ("A19",(VN"shadowHatStart",VN"shadowY"));
      ("A20",(VN"lowerBarMinimalX",VN"lowerBarY"));
      ("A21",(VN"lowerBarMaximalX",VN"lowerBarY"));
      ("A22",(VN"upperBarMinimalX",VN"upperBarY"));
      ("A23",(VN"upperBarMaximalX",VN"upperBarY"));

   ] ;;

let env_for_b = Pgfmath_environment.constructor 
   "B"
   ap 
   [
    (VN"zero",Leaf 0.);
    (VN"tipHeight",Leaf 0.42);
    (VN"tipWidth",Leaf 0.98);
    (VN"leftHingeWidth",Leaf 0.35);
    (VN"interHingeWidth",Leaf 0.36);
    (VN"middleHingeWidth",Leaf 0.61);
    (VN"upperPlankHeight",Leaf 0.43);
    (VN"upperInterPlankHeight",Leaf 3.11);
    (VN"middlePlankHeight",Leaf 0.43);
    (VN"lowerInterPlankHeight",Leaf 3.51);
    (VN"lowerPlankHeight",Leaf 0.32);
    (VN"upperPlankWidth",Leaf 0.81);
    (VN"middlePlankWidth",Leaf 1.38);
    (VN"lowerPlankWidth",Leaf 1.15);
    (VN"upperInterScytheWidth",Leaf 0.84);
    (VN"upperInterScytheHeight",Leaf 0.32);
    (VN"middleInterScytheWidth",Leaf 0.65);
    (VN"xb1",Leaf 0.);
    (VN"yb1",Leaf 0.);
    (VN"xb2",Leaf 0.);
    (VN"yb2",Leaf 0.);
    (VN"xb3",Leaf 0.);
    (VN"yb3",Leaf 0.);
    (VN"xb4",Leaf 0.);
    (VN"yb4",Leaf 0.);
    (VN"xb5",Leaf 0.);
    (VN"yb5",Leaf 0.);
    (VN"xb6",Leaf 0.);
    (VN"yb6",Leaf 0.);
    (VN"xb7",Leaf 0.);
    (VN"yb7",Leaf 0.);
    (VN"xb8",Leaf 0.);
    (VN"yb8",Leaf 0.);
    (VN"xb9",Leaf 0.);
    (VN"yb9",Leaf 0.);
    (VN"xb10",Leaf 0.);
    (VN"yb10",Leaf 0.);
    (VN"xb11",Leaf 0.);
    (VN"yb11",Leaf 0.);
    (VN"xb12",Leaf 0.);
    (VN"yb12",Leaf 0.);
    (VN"xb13",Leaf 0.);
    (VN"yb13",Leaf 0.);
    (VN"xb14",Leaf 0.);
    (VN"yb14",Leaf 0.);
    (VN"xb15",Leaf 0.);
    (VN"yb15",Leaf 0.);
    (VN"xb16",Leaf 0.);
    (VN"yb16",Leaf 0.);
    (VN"xb17",Leaf 0.);
    (VN"yb17",Leaf 0.);
    (VN"xb18",Leaf 0.);
    (VN"yb18",Leaf 0.);
    (VN"xb19",Leaf 0.);
    (VN"yb19",Leaf 0.);
    (VN"xb20",Leaf 0.);
    (VN"yb20",Leaf 0.);
    (VN"xb21",Leaf 0.);
    (VN"yb21",Leaf 0.);
    (VN"xb22",Leaf 0.);
    (VN"yb22",Leaf 0.);
    (VN"xb23",Leaf 0.);
    (VN"yb23",Leaf 0.);
    (VN"xb24",Leaf 0.);
    (VN"yb24",Leaf 0.);
    (VN"xb25",Leaf 0.);
    (VN"yb25",Leaf 0.);
    (VN"xb26",Leaf 0.);
    (VN"yb26",Leaf 0.);
    (VN"xb27",Leaf 0.);
    (VN"yb27",Leaf 0.);
    (VN"xb28",Leaf 0.);
    (VN"yb28",Leaf 0.);
    (VN"xb29",Leaf 0.);
    (VN"yb29",Leaf 0.);
    (VN"xb30",Leaf 0.);
    (VN"yb30",Leaf 0.);
    (VN"xb31",Leaf 0.);
    (VN"yb31",Leaf 0.); 

   ]
   [
      ("B1",(VN"xb1",VN"yb1"));
      ("B2",(VN"xb2",VN"yb2"));
      ("B3",(VN"xb3",VN"yb3"));
      ("B4",(VN"xb4",VN"yb4"));
      ("B5",(VN"xb5",VN"yb5"));
      ("B6",(VN"xb6",VN"yb6"));
      ("B7",(VN"xb7",VN"yb7"));
      ("B8",(VN"xb8",VN"yb8"));
      ("B9",(VN"xb9",VN"yb9"));
      ("B10",(VN"xb10",VN"yb10"));
      ("B11",(VN"xb11",VN"yb11"));
      ("B12",(VN"xb12",VN"yb12"));
      ("B13",(VN"xb13",VN"yb13"));
      ("B14",(VN"xb14",VN"yb14"));
      ("B15",(VN"xb15",VN"yb15"));
      ("B16",(VN"xb16",VN"yb16"));
      ("B17",(VN"xb17",VN"yb17"));
      ("B18",(VN"xb18",VN"yb18"));
      ("B19",(VN"xb19",VN"yb19"));
      ("B20",(VN"xb20",VN"yb20"));
      ("B21",(VN"xb21",VN"yb21"));
      ("B22",(VN"xb22",VN"yb22"));
      ("B23",(VN"xb23",VN"yb23"));
      ("B24",(VN"xb24",VN"yb24"));
      ("B25",(VN"xb25",VN"yb25"));
      ("B26",(VN"xb26",VN"yb26"));
      ("B27",(VN"xb27",VN"yb27"));
      ("B28",(VN"xb28",VN"yb28"));
      ("B29",(VN"xb29",VN"yb29"));
      ("B30",(VN"xb30",VN"yb30"));
      ("B31",(VN"xb31",VN"yb31")); 

   ] ;;   


let env_for_nl = Pgfmath_environment.constructor 
   ""
   ap 
   [
   ]
   [
   ] ;;

(*


Pgfmath_environment.write env_for_a ;;

*)