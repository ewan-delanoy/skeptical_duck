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
    (VN"xb2",Sum[VN"xb1";VN"tipWidth";VN"leftHingeWidth"]);
    (VN"yb2",Sum[VN"yb1"]);
    (VN"xb3",Sum[VN"xb2"]);
    (VN"yb3",Sum[VN"xb1";VN"upperPlankHeight";VN"upperInterPlankHeight";VN"middlePlankHeight";VN"lowerInterPlankHeight";VN"lowerPlankHeight"]);
    (VN"xb4",Sum[VN"xb1"]);
    (VN"yb4",Sum[VN"yb3"]);
    (VN"xb5",Sum[VN"xb4"]);
    (VN"yb5",Substract(VN"yb4",VN"tipHeight"));
    (VN"xb6",Sum[VN"xb5";VN"tipWidth"]);
    (VN"yb6",Sum[VN"yb5"]);
    (VN"xb7",Sum[VN"xb1";VN"tipWidth"]);
    (VN"yb7",Sum[VN"yb1";VN"tipHeight"]);
    (VN"xb8",Sum[VN"xb1"]);
    (VN"yb8",Sum[VN"yb1";VN"tipHeight"]);
    (VN"xb9",Sum[VN"xb2";VN"interHingeWidth"]);
    (VN"yb9",Sum[VN"yb2"]);
    (VN"xb10",Sum[VN"xb9"]);
    (VN"yb10",Sum[VN"yb9";VN"tipHeight"]);
    (VN"xb11",Sum[VN"xb2"]);
    (VN"yb11",Sum[VN"yb7"]);
    (VN"xb12",Sum[VN"xb3"]);
    (VN"yb12",Substract(VN"yb3",VN"tipHeight"));
    (VN"xb13",Sum[VN"xb10"]);
    (VN"yb13",Sum[VN"yb12"]);
    (VN"xb14",Sum[VN"xb13"]);
    (VN"yb14",Sum[VN"yb3"]);
    (VN"xb15",Sum[VN"xb2";VN"middleHingeWidth"]);
    (VN"yb15",Sum[VN"yb9"]);
    (VN"xb16",Sum[VN"xb15"]);
    (VN"yb16",Sum[VN"yb14"]);
    (VN"xb17",Sum[VN"xb15";VN"lowerPlankWidth"]);
    (VN"yb17",Sum[VN"yb15"]);
    (VN"xb18",Sum[VN"xb17"]);
    (VN"yb18",Sum[VN"yb17";VN"lowerPlankHeight"]);
    (VN"xb19",Sum[VN"xb15"]);
    (VN"yb19",Sum[VN"yb18"]);
    (VN"xb20",Sum[VN"xb19"]);
    (VN"yb20",Sum[VN"yb19";VN"lowerInterPlankHeight"]);
    (VN"xb21",Sum[VN"xb20";VN"middlePlankWidth"]);
    (VN"yb21",Sum[VN"yb20"]);
    (VN"xb22",Sum[VN"xb21"]);
    (VN"yb22",Sum[VN"yb21";VN"middlePlankHeight"]);
    (VN"xb23",Sum[VN"xb20"]);
    (VN"yb23",Sum[VN"yb22"]);
    (VN"xb24",Sum[VN"xb16"]);
    (VN"yb24",Substract(VN"yb16",VN"upperPlankHeight"));
    (VN"xb25",Sum[VN"xb14";VN"upperPlankWidth"]);
    (VN"yb25",Sum[VN"yb24"]);
    (VN"xb26",Sum[VN"xb25"]);
    (VN"yb26",Sum[VN"yb16"]);
    (VN"xb27",Sum[VN"xb26"]);
    (VN"yb27",Substract(VN"yb26",VN"upperInterScytheHeight"));
    (VN"xb28",Sum[VN"xb27";VN"upperInterScytheWidth"]);
    (VN"yb28",Sum[VN"yb27"]);
    (VN"xb29",Sum[VN"xb28"]);
    (VN"yb29",Sum[VN"yb26"]);
    (VN"xb30",Sum[VN"xb21";VN"middleInterScytheWidth"]);
    (VN"yb30",Sum[VN"yb21"]);
    (VN"xb31",Sum[VN"xb30"]);
    (VN"yb31",Sum[VN"yb22"]); 

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