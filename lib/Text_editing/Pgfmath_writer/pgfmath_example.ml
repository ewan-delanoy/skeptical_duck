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
   ]
   [
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