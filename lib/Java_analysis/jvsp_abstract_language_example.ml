(*

#use"lib/Java_analysis/jvsp_abstract_language_example.ml";;

*)

module Private = struct 

type token_type = Jvsp_types.token_type =
  IDENTIFIER_T 
|BOOLEAN_LITERAL_T
|CHARACTER_LITERAL_T
|FLOATING_POINT_LITERAL_T
|INTEGER_LITERAL_T
|NULL_LITERAL_T
|STRING_LITERAL_T
|TEXT_BLOCK_T
|LOWLEVEL_TYPE_T
(* Separators *)
|LP_T		(* ( *)
|RP_T		(* ) *)
|LC_T		(* { *)
|RC_T		(* } *)
|LB_T		(* [ *)
|RB_T		(* ] *)
|SM_T		(* ; *)
|CM_T		(* , *)
|DOT_T	(* . *)

(* Operators *)
|EQ_T		  (* = *)
|GT_T		  (* > *)
|LT_T		  (* < *)
|NOT_T		(* ! *)
|COMPL_T	(* ~ *)
|COND_T		(* ? *)
|COLON_T	(* : *)
|EQ_EQ_T	(* == *)
|LE_T		  (* <= *)
|GE_T		  (* >= *)
|NOT_EQ_T		(* != *)
|AND_AND_T	(* && *)
|OR_OR_T	(* || *)
|INCR_T		(* ++ *)
|DECR_T		(* -- *)
|PLUS_T		(* + *)
|MINUS_T	(* - *)
|TIMES_T	(* * *)
|DIV_T		(* / *)
|AND_T		(* & *)
|OR_T		    (* | *)
|XOR_T		(* ^ *)
|MOD_T		(* % *)
|LS_T		  (* << *)
|SRS_T		(* >> *)
|URS_T		(* >>> *)
|OPERATOR_EQ_T 	(* += -= *= /= &= |= ^= %= <<= >>= >>>= *)
|SNAIL_T 
(* Keywords*)
|ABSTRACT_T |ASSERT_T |BOOLEAN_T |BREAK_T |BYTE_T |CASE_T |CATCH_T |CHAR_T |CLASS_T 
|CONST_T |CONTINUE_T |DEFAULT_T |DO_T |DOUBLE_T |ELSE_T |ENUM_T |EXPORTS_T |EXTENDS_T 
|FINAL_T |FINALLY_T |FLOAT_T |FOR_T |GOTO_T |IF_T |IMPLEMENTS_T |IMPORT_T |INSTANCEOF_T 
|INT_T |INTERFACE_T |LONG_T |MODULE_T |NATIVE_T |NEW_T |NONSEALED_T |OPEN_T |OPENS_T 
|PACKAGE_T |PERMITS_T |PRIVATE_T |PROTECTED_T |PROVIDES_T |PUBLIC_T |RECORD_T |REQUIRES_T 
|RETURN_T |SEALED_T |SHORT_T |STATIC_T |STRICTFP_T |SUPER_T |SWITCH_T |SYNCHRONIZED_T
|THIS_T |THROW_T |THROWS_T |TRANSIENT_T |TRANSITIVE_T |TRY_T |TO_T |USES_T 
|VAR_T |VOID_T |VOLATILE_T |WHILE_T |WITH_T |YIELD_T
|EOF_T  
|COMMENT_T 
|WHITESPACE_T 
|LINEBREAK_T;;


     
type form =  Jvsp_abstract_language_t.form = 
    Just_an_optional of string
   |Just_atomic of Jvsp_types.token_type list
   |Just_a_concat of string list 
   |Just_a_disjunction of string list 
   |Just_a_star of string 
   |Synonym of string;;

type t =  Jvsp_abstract_language_t.t = AL of (string * form) list ;; 

type modification = Jvsp_abstract_language_t.modification = 
   Set_production of string * form 
  |Rename of string * string  
  |Remove_productions of string list 
  |Register_molecular of Jvsp_types.token_type list ;;

(* Java grammar begins here *)


 let original_java_grammar = 

AL ([

   ("AdditionalBound",Just_a_concat(["AtomicAnd";"InterfaceType"]));
   ("AdditiveExpression",Just_a_disjunction(["MultiplicativeExpression";"UsingPlusAdditiveExpression";"UsingMinusAdditiveExpression"]));
   ("AmbiguousName",Just_a_disjunction(["Identifier";"CompoundAmbiguousName"]));
   ("AndExpression",Just_a_disjunction(["EqualityExpression";"CompoundAndExpression"]));
   ("AnnotatedIdentifierrPrecededByDot",Just_a_concat(["AtomicDot";"StarredAnnotation";"Identifier"]));
   ("Annotation",Just_a_disjunction(["NormalAnnotation";"MarkerAnnotation";"SingleElementAnnotation"]));
   ("AnnotationInterfaceDeclaration",Just_a_concat(["StarredInterfaceModifier";"MolecularSnail_Interface_Identifier_Lb";"StarredAnnotationInterfaceMemberDeclaration";"AtomicRb"]));
   ("AnnotationInterfaceElementDeclaration",Just_a_concat(["StarredAnnotationInterfaceElementModifier";"UnannType";"MolecularIdentifier_Lp_Rp";"OptionalDims";"OptionalDefaultValue";"AtomicSm"]));
   ("AnnotationInterfaceElementModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicAbstract"]));
   ("AnnotationInterfaceMemberDeclaration",Just_a_disjunction(["AnnotationInterfaceElementDeclaration";"ConstantDeclaration";"ClassDeclaration";"InterfaceDeclaration";"AtomicSm"]));
   ("ArgumentList",Just_a_concat(["Expression";"StarredExpressionPrecededByComma"]));
   ("ArrayAccess",Just_a_disjunction(["ShortArrayAccess";"LongArrayAccess"]));
   ("ArrayCreationExpression",Just_a_disjunction(["HaddockArrayCreationExpression";"MackerelCreationExpressionArrayCreationExpression";"SalmonCreationExpressionArrayCreationExpression";"TunaCreationExpressionArrayCreationExpression"]));
   ("ArrayInitializer",Just_a_concat(["AtomicLb";"OptionalVariableInitializerList";"OptionalCm";"AtomicRb"]));
   ("ArrayType",Just_a_disjunction(["PrimitiveArrayType";"GlassArrayType";"PlasticArrayType"]));
   ("ArrayTypeyMethodReference",Just_a_concat(["ArrayType";"MolecularColon_Colon_New"]));
   ("AssertStatement",Just_a_disjunction(["WithoutColonAssertStatement";"WithColonAssertStatement"]));
   ("Assignment",Just_a_concat(["LeftHandSide";"AtomicOperatorEq";"Expression"]));
   ("AssignmentExpression",Just_a_disjunction(["ConditionalExpression";"Assignment"]));
   ("AtomicAbstract",Just_atomic([ABSTRACT_T]));
   ("AtomicAnd",Just_atomic([AND_T]));
   ("AtomicAndAnd",Just_atomic([AND_AND_T]));
   ("AtomicAssert",Just_atomic([ASSERT_T]));
   ("AtomicBoolean",Just_atomic([BOOLEAN_T]));
   ("AtomicBreak",Just_atomic([BREAK_T]));
   ("AtomicByte",Just_atomic([BYTE_T]));
   ("AtomicCase",Just_atomic([CASE_T]));
   ("AtomicChar",Just_atomic([CHAR_T]));
   ("AtomicCm",Just_atomic([CM_T]));
   ("AtomicColon",Just_atomic([COLON_T]));
   ("AtomicCompl",Just_atomic([COMPL_T]));
   ("AtomicCond",Just_atomic([COND_T]));
   ("AtomicContinue",Just_atomic([CONTINUE_T]));
   ("AtomicDecr",Just_atomic([DECR_T]));
   ("AtomicDefault",Just_atomic([DEFAULT_T]));
   ("AtomicDiv",Just_atomic([DIV_T]));
   ("AtomicDo",Just_atomic([DO_T]));
   ("AtomicDot",Just_atomic([DOT_T]));
   ("AtomicDouble",Just_atomic([DOUBLE_T]));
   ("AtomicElse",Just_atomic([ELSE_T]));
   ("AtomicEq",Just_atomic([EQ_T]));
   ("AtomicEqEq",Just_atomic([EQ_EQ_T]));
   ("AtomicExtends",Just_atomic([EXTENDS_T]));
   ("AtomicFinal",Just_atomic([FINAL_T]));
   ("AtomicFloat",Just_atomic([FLOAT_T]));
   ("AtomicGe",Just_atomic([GE_T]));
   ("AtomicGt",Just_atomic([GT_T]));
   ("AtomicImplements",Just_atomic([IMPLEMENTS_T]));
   ("AtomicImport",Just_atomic([IMPORT_T]));
   ("AtomicIncr",Just_atomic([INCR_T]));
   ("AtomicInstanceof",Just_atomic([INSTANCEOF_T]));
   ("AtomicInt",Just_atomic([INT_T]));
   ("AtomicLb",Just_atomic([LB_T]));
   ("AtomicLc",Just_atomic([LC_T]));
   ("AtomicLe",Just_atomic([LE_T]));
   ("AtomicLong",Just_atomic([LONG_T]));
   ("AtomicLp",Just_atomic([LP_T]));
   ("AtomicLs",Just_atomic([LS_T]));
   ("AtomicLt",Just_atomic([LT_T]));
   ("AtomicMinus",Just_atomic([MINUS_T]));
   ("AtomicMod",Just_atomic([MOD_T]));
   ("AtomicNative",Just_atomic([NATIVE_T]));
   ("AtomicNew",Just_atomic([NEW_T]));
   ("AtomicNonsealed",Just_atomic([NONSEALED_T]));
   ("AtomicNot",Just_atomic([NOT_T]));
   ("AtomicNotEq",Just_atomic([NOT_EQ_T]));
   ("AtomicOperatorEq",Just_atomic([OPERATOR_EQ_T]));
   ("AtomicOr",Just_atomic([OR_T]));
   ("AtomicOrOr",Just_atomic([OR_OR_T]));
   ("AtomicPermits",Just_atomic([PERMITS_T]));
   ("AtomicPlus",Just_atomic([PLUS_T]));
   ("AtomicPrivate",Just_atomic([PRIVATE_T]));
   ("AtomicProtected",Just_atomic([PROTECTED_T]));
   ("AtomicPublic",Just_atomic([PUBLIC_T]));
   ("AtomicRb",Just_atomic([RB_T]));
   ("AtomicRc",Just_atomic([RC_T]));
   ("AtomicReturn",Just_atomic([RETURN_T]));
   ("AtomicRp",Just_atomic([RP_T]));
   ("AtomicSealed",Just_atomic([SEALED_T]));
   ("AtomicShort",Just_atomic([SHORT_T]));
   ("AtomicSm",Just_atomic([SM_T]));
   ("AtomicSnail",Just_atomic([SNAIL_T]));
   ("AtomicSrs",Just_atomic([SRS_T]));
   ("AtomicStatic",Just_atomic([STATIC_T]));
   ("AtomicStrictfp",Just_atomic([STRICTFP_T]));
   ("AtomicSuper",Just_atomic([SUPER_T]));
   ("AtomicSynchronized",Just_atomic([SYNCHRONIZED_T]));
   ("AtomicThis",Just_atomic([THIS_T]));
   ("AtomicThrow",Just_atomic([THROW_T]));
   ("AtomicThrows",Just_atomic([THROWS_T]));
   ("AtomicTimes",Just_atomic([TIMES_T]));
   ("AtomicTransient",Just_atomic([TRANSIENT_T]));
   ("AtomicUrs",Just_atomic([URS_T]));
   ("AtomicVar",Just_atomic([VAR_T]));
   ("AtomicVoid",Just_atomic([VOID_T]));
   ("AtomicVolatile",Just_atomic([VOLATILE_T]));
   ("AtomicXor",Just_atomic([XOR_T]));
   ("AtomicYield",Just_atomic([YIELD_T]));
   ("BasicCastExpression",Just_a_concat(["AtomicLp";"PrimitiveType";"AtomicRp";"UnaryExpression"]));
   ("BasicForStatement",Just_a_concat(["MolecularFor_Lp";"OptionalForInit";"AtomicSm";"OptionalExpression";"AtomicSm";"OptionalForUpdate";"AtomicRp";"Statement"]));
   ("BasicForStatementNoShortIf",Just_a_concat(["MolecularFor_Lp";"OptionalForInit";"AtomicSm";"OptionalExpression";"AtomicSm";"OptionalForUpdate";"AtomicRp";"StatementNoShortIf"]));
   ("BasicMethodHeader",Just_a_concat(["Result";"MolecularIdentifier_Lp";"OptionalReceiverParameterFollowedByComma";"OptionalFormalParameterList";"AtomicRp";"OptionalDims";"OptionalThrows"]));
   ("BasicMethodInvocation",Just_a_concat(["MethodName";"AtomicLp";"OptionalArgumentList";"AtomicRp"]));
   ("BasicRecordComponent",Just_a_concat(["StarredRecordComponentModifier";"UnannType";"Identifier"]));
   ("BasicSwitchBlock",Just_a_concat(["AtomicLc";"SwitchRule";"StarredSwitchRule";"AtomicRc"]));
   ("BasicSwitchRule",Just_a_concat(["SwitchLabel";"MolecularMinus_Gt";"Expression";"AtomicSm"]));
   ("Block",Just_a_concat(["AtomicLb";"OptionalBlockStatements";"AtomicRb"]));
   ("BlockStatement",Just_a_disjunction(["LocalClassOrInterfaceDeclaration";"LocalVariableDeclarationStatement";"Statement"]));
   ("BlockStatements",Just_a_concat(["BlockStatement";"StarredBlockStatement"]));
   ("BooleanLiteral",Just_atomic([BOOLEAN_LITERAL_T]));
   ("BoolyClassLiteral",Just_a_concat(["AtomicBoolean";"StarredOpenSquare";"MolecularDot_Class"]));
   ("BoolyPrimitiveType",Just_a_concat(["StarredAnnotation";"AtomicBoolean"]));
   ("BreakStatement",Just_a_concat(["AtomicBreak";"OptionalIdentifier";"AtomicSm"]));
   ("CaseConstant",Synonym("ConditionalExpression"));
   ("CaseConstantPrecededByComma",Just_a_concat(["AtomicCm";"CaseConstant"]));
   ("CastExpression",Just_a_disjunction(["BasicCastExpression";"NonmappyCastExpression";"MappyCastExpression"]));
   ("CatchClause",Just_a_concat(["MolecularCatch_Lp";"StarredVariableModifier";"UnannClassType";"StarredClassTypePrecededByVerticalBar";"Identifier";"OptionalDims";"MolecularRp_Lb";"OptionalBlockStatements";"AtomicRb"]));
   ("Catches",Just_a_concat(["MolecularCatch_Lp";"StarredVariableModifier";"UnannClassType";"StarredClassTypePrecededByVerticalBar";"Identifier";"OptionalDims";"MolecularRp_Lb";"OptionalBlockStatements";"AtomicRb";"StarredCatchClause"]));
   ("CharacterLiteral",Just_atomic([CHARACTER_LITERAL_T]));
   ("ClassBody",Just_a_concat(["AtomicLb";"StarredClassBodyDeclaration";"AtomicRb"]));
   ("ClassBodyDeclaration",Just_a_disjunction(["ClassMemberDeclaration";"InstanceInitializer";"StaticInitializer";"ConstructorDeclaration"]));
   ("ClassDeclaration",Just_a_disjunction(["NormalClassDeclaration";"EnumDeclaration";"RecordDeclaration"]));
   ("ClassExtends",Just_a_concat(["AtomicExtends";"ClassType"]));
   ("ClassImplements",Just_a_concat(["AtomicImplements";"InterfaceType";"StarredInterfaceTypePrecededByComma"]));
   ("ClassInstanceCreationExpression",Just_a_disjunction(["HaddockClassInstanceCreationExpression";"MackerelClassInstanceCreationExpression";"SalmonClassInstanceCreationExpression"]));
   ("ClassLiteral",Just_a_disjunction(["UserDefinedClassLiteral";"NumericClassLiteral";"BoolyClassLiteral";"MolecularVoid_Dot_Class"]));
   ("ClassMemberDeclaration",Just_a_disjunction(["FieldDeclaration";"MethodDeclaration";"ClassDeclaration";"InterfaceDeclaration";"AtomicSm"]));
   ("ClassModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicProtected";"AtomicPrivate";"AtomicAbstract";"AtomicStatic";"AtomicFinal";"AtomicSealed";"AtomicNonsealed";"AtomicStrictfp"]));
   ("ClassOrInterfaceType",Just_a_disjunction(["ClassType";"InterfaceType"]));
   ("ClassPermits",Just_a_concat(["AtomicPermits";"TypeName";"StarredTypeNamePrecededByComma"]));
   ("ClassType",Just_a_disjunction(["ShortClassType";"UsualClassType";"LongClassType"]));
   ("ClassTypePrecededByVerticalBar",Just_a_concat(["AtomicOr";"ClassType"]));
   ("ClassTypeyMethodReference",Just_a_concat(["ClassType";"MolecularColon_Colon";"OptionalTypeArguments";"AtomicNew"]));
   ("CompactConstructorDeclaration",Just_a_concat(["StarredConstructorModifier";"SimpleTypeName";"AtomicLb";"OptionalExplicitConstructorInvocation";"OptionalBlockStatements";"AtomicRb"]));
   ("CompoundAmbiguousName",Just_a_concat(["AmbiguousName";"MolecularDot_Identifier"]));
   ("CompoundAndExpression",Just_a_concat(["AndExpression";"AtomicAnd";"EqualityExpression"]));
   ("CompoundConditionalAndExpression",Just_a_concat(["ConditionalAndExpression";"AtomicAndAnd";"InclusiveOrExpression"]));
   ("CompoundConditionalOrExpression",Just_a_concat(["ConditionalOrExpression";"AtomicOrOr";"ConditionalAndExpression"]));
   ("CompoundExclusiveOrExpression",Just_a_concat(["ExclusiveOrExpression";"AtomicXor";"AndExpression"]));
   ("CompoundInclusiveOrExpression",Just_a_concat(["InclusiveOrExpression";"AtomicOr";"ExclusiveOrExpression"]));
   ("CompoundModuleName",Just_a_concat(["ModuleName";"MolecularDot_Identifier"]));
   ("CompoundPackageName",Just_a_concat(["PackageName";"MolecularDot_Identifier"]));
   ("CompoundPackageOrTypeName",Just_a_concat(["PackageOrTypeName";"MolecularDot_Identifier"]));
   ("CompoundSwitchBlock",Just_a_concat(["AtomicLc";"StarredSwitchBlockStatementGroup";"StarredSwitchLabelFollowedByColon";"AtomicRc"]));
   ("CompoundSwitchLabel",Just_a_concat(["AtomicCase";"CaseConstant";"StarredCaseConstantPrecededByComma"]));
   ("CompoundTypeName",Just_a_concat(["PackageOrTypeName";"AtomicDot";"TypeIdentifier"]));
   ("ConditionalAndExpression",Just_a_disjunction(["InclusiveOrExpression";"CompoundConditionalAndExpression"]));
   ("ConditionalExpression",Just_a_disjunction(["ConditionalOrExpression";"LambdalessConditionalAndExpressionConditionalExpression";"LambdafulConditionalAndExpressionConditionalExpression"]));
   ("ConditionalOrExpression",Just_a_disjunction(["ConditionalAndExpression";"CompoundConditionalOrExpression"]));
   ("ConstantDeclaration",Just_a_concat(["StarredConstantModifier";"UnannType";"Identifier";"OptionalDims";"OptionalEqualsVariableInitializer";"StarredVariableDeclaratorPrecededByComma";"AtomicSm"]));
   ("ConstantModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicStatic";"AtomicFinal"]));
   ("ConstructorDeclaration",Just_a_concat(["StarredConstructorModifier";"OptionalTypeParameters";"SimpleTypeName";"AtomicLp";"OptionalReceiverParameterFollowedByComma";"OptionalFormalParameterList";"AtomicRp";"OptionalThrows";"AtomicLb";"OptionalExplicitConstructorInvocation";"OptionalBlockStatements";"AtomicRb"]));
   ("ConstructorModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicProtected";"AtomicPrivate"]));
   ("ContinueStatement",Just_a_concat(["AtomicContinue";"OptionalIdentifier";"AtomicSm"]));
   ("DefaultValue",Just_a_concat(["AtomicDefault";"ElementValue"]));
   ("DiamondlyMethodHeader",Just_a_concat(["AtomicLt";"StarredTypeParameterModifier";"Identifier";"OptionalTypeBound";"StarredTypeParameterPrecededByComma";"AtomicGt";"StarredAnnotation";"Result";"MolecularIdentifier_Lp";"OptionalReceiverParameterFollowedByComma";"OptionalFormalParameterList";"AtomicRp";"OptionalDims";"OptionalThrows"]));
   ("DimExpr",Just_a_concat(["StarredAnnotation";"AtomicLb";"Expression";"AtomicRb"]));
   ("Dims",Just_a_concat(["StarredAnnotation";"MolecularLb_Rb";"StarredDimsElement"]));
   ("DimsElement",Just_a_concat(["StarredAnnotation";"MolecularLb_Rb"]));
   ("DoStatement",Just_a_concat(["AtomicDo";"Statement";"MolecularWhile_Lp";"Expression";"MolecularRp_Sm"]));
   ("ElementValue",Just_a_disjunction(["ConditionalExpression";"ElementValueArrayInitializer";"Annotation"]));
   ("ElementValueArrayInitializer",Just_a_concat(["AtomicLb";"OptionalElementValueList";"OptionalCm";"AtomicRb"]));
   ("ElementValueList",Just_a_concat(["ElementValue";"StarredElementValuePrecededByComma"]));
   ("ElementValuePairList",Just_a_concat(["MolecularIdentifier_Eq";"ElementValue";"StarredElementValuePairPrecededByComma"]));
   ("ElementValuePairPrecededByComma",Just_a_concat(["MolecularCm_Identifier_Eq";"ElementValue"]));
   ("ElementValuePrecededByComma",Just_a_concat(["AtomicCm";"ElementValue"]));
   ("EmptyStatement",Synonym("AtomicSm"));
   ("EnhancedForStatement",Just_a_concat(["MolecularFor_Lp";"StarredVariableModifier";"LocalVariableType";"Identifier";"OptionalDims";"OptionalEqualsVariableInitializer";"StarredVariableDeclaratorPrecededByComma";"AtomicColon";"Expression";"AtomicRp";"Statement"]));
   ("EnhancedForStatementNoShortIf",Just_a_concat(["MolecularFor_Lp";"StarredVariableModifier";"LocalVariableType";"Identifier";"OptionalDims";"OptionalEqualsVariableInitializer";"StarredVariableDeclaratorPrecededByComma";"AtomicColon";"Expression";"AtomicRp";"StatementNoShortIf"]));
   ("EnumBodyDeclarations",Just_a_concat(["AtomicSm";"StarredClassBodyDeclaration"]));
   ("EnumConstantList",Just_a_concat(["StarredEnumConstantModifier";"Identifier";"OptionalParenthesedArgumentList";"OptionalClassBody";"StarredEnumConstantPrecededByComma"]));
   ("EnumConstantModifier",Synonym("Annotation"));
   ("EnumConstantPrecededByComma",Just_a_concat(["AtomicCm";"StarredEnumConstantModifier";"Identifier";"OptionalParenthesedArgumentList";"OptionalClassBody"]));
   ("EnumDeclaration",Just_a_concat(["StarredClassModifier";"MolecularEnum_Identifier";"OptionalClassImplements";"AtomicLb";"OptionalEnumConstantList";"OptionalCm";"OptionalEnumBodyDeclarations";"AtomicRb"]));
   ("EqualityExpression",Just_a_disjunction(["RelationalExpression";"WithEqEqEqualityExpression";"WithNotEqEqualityExpression"]));
   ("EqualsVariableInitializer",Just_a_concat(["AtomicEq";"VariableInitializer"]));
   ("ExceptionType",Just_a_disjunction(["ClassType";"TypeVariable"]));
   ("ExceptionTypePrecededByComma",Just_a_concat(["AtomicCm";"ExceptionType"]));
   ("ExclusiveOrExpression",Just_a_disjunction(["AndExpression";"CompoundExclusiveOrExpression"]));
   ("ExplicitCallToSuperFieldAccess",Just_a_concat(["TypeName";"MolecularDot_Super_Dot_Identifier"]));
   ("ExplicitConstructorInvocation",Just_a_disjunction(["HaddockExplicitConstructorInvocation";"MackerelExplicitConstructorInvocation";"SalmonExplicitConstructorInvocation";"TunaExplicitConstructorInvocation"]));
   ("Expression",Just_a_disjunction(["LambdaExpression";"AssignmentExpression"]));
   ("ExpressionName",Just_a_disjunction(["Identifier";"CompoundAmbiguousName"]));
   ("ExpressionPrecededByComma",Just_a_concat(["AtomicCm";"Expression"]));
   ("ExpressionStatement",Just_a_concat(["StatementExpression";"AtomicSm"]));
   ("ExpressionyMethodInvocation",Just_a_concat(["ExpressionName";"AtomicDot";"OptionalTypeArguments";"MolecularIdentifier_Lp";"OptionalArgumentList";"AtomicRp"]));
   ("ExpressionyMethodReference",Just_a_concat(["ExpressionName";"MolecularColon_Colon";"OptionalTypeArguments";"Identifier"]));
   ("FieldAccess",Just_a_disjunction(["SimplestFieldAccess";"MolecularSuper_Dot_Identifier";"ExplicitCallToSuperFieldAccess"]));
   ("FieldDeclaration",Just_a_concat(["StarredFieldModifier";"UnannType";"Identifier";"OptionalDims";"OptionalEqualsVariableInitializer";"StarredVariableDeclaratorPrecededByComma";"AtomicSm"]));
   ("FieldModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicProtected";"AtomicPrivate";"AtomicStatic";"AtomicFinal";"AtomicTransient";"AtomicVolatile"]));
   ("Finally",Just_a_concat(["MolecularFinally_Lb";"OptionalBlockStatements";"AtomicRb"]));
   ("FloatingPointLiteral",Just_atomic([FLOATING_POINT_LITERAL_T]));
   ("FloatingPointType",Just_a_disjunction(["AtomicFloat";"AtomicDouble"]));
   ("ForInit",Just_a_disjunction(["StatementExpressionList";"LocalVariableDeclaration"]));
   ("FormalParameter",Just_a_disjunction(["StrictFormalParameter";"UsualParameter"]));
   ("FormalParameterList",Just_a_concat(["FormalParameter";"StarredFormalParameterPrecededByComma"]));
   ("FormalParameterPrecededByComma",Just_a_concat(["AtomicCm";"FormalParameter"]));
   ("ForStatement",Just_a_disjunction(["BasicForStatement";"EnhancedForStatement"]));
   ("ForStatementNoShortIf",Just_a_disjunction(["BasicForStatementNoShortIf";"EnhancedForStatementNoShortIf"]));
   ("ForUpdate",Synonym("StatementExpressionList"));
   ("GlassArrayType",Just_a_concat(["ClassOrInterfaceType";"StarredAnnotation";"MolecularLb_Rb";"StarredDimsElement"]));
   ("GlassTypeBound",Just_a_concat(["AtomicExtends";"ClassOrInterfaceType";"StarredAdditionalBound"]));
   ("GlassUnannArrayType",Just_a_concat(["UnannClassOrInterfaceType";"StarredAnnotation";"MolecularLb_Rb";"StarredDimsElement"]));
   ("HaddockArrayCreationExpression",Just_a_concat(["AtomicNew";"PrimitiveType";"StarredAnnotation";"AtomicLb";"Expression";"AtomicRb";"StarredDimExpr";"OptionalDims"]));
   ("HaddockClassInstanceCreationExpression",Just_a_concat(["AtomicNew";"OptionalTypeArguments";"StarredAnnotation";"Identifier";"StarredAnnotatedIdentifierrPrecededByDot";"OptionalTypeArgumentsOrDiamond";"AtomicLp";"OptionalArgumentList";"AtomicRp";"OptionalClassBody"]));
   ("HaddockExplicitConstructorInvocation",Just_a_concat(["OptionalTypeArguments";"MolecularThis_Lp";"OptionalArgumentList";"MolecularRp_Sm"]));
   ("HeavyLambdaParameter",Just_a_concat(["StarredVariableModifier";"LambdaParameterType";"Identifier";"OptionalDims"]));
   ("Identifier",Just_atomic([IDENTIFIER_T]));
   ("IdentifierFollowedByDot",Just_a_concat(["MolecularIdentifier_Dot"]));
   ("IdentifierPrecededByComma",Just_a_concat(["MolecularCm_Identifier"]));
   ("IdentifieryLambdaParameterList",Just_a_concat(["Identifier";"StarredIdentifierPrecededByComma"]));
   ("IdentifieryTypeBound",Just_a_concat(["AtomicExtends";"StarredAnnotation";"Identifier"]));
   ("IfThenElseStatement",Just_a_concat(["MolecularIf_Lp";"Expression";"AtomicRp";"StatementNoShortIf";"AtomicElse";"Statement"]));
   ("IfThenElseStatementNoShortIf",Just_a_concat(["MolecularIf_Lp";"Expression";"AtomicRp";"StatementNoShortIf";"AtomicElse";"StatementNoShortIf"]));
   ("IfThenStatement",Just_a_concat(["MolecularIf_Lp";"Expression";"AtomicRp";"Statement"]));
   ("ImportDeclaration",Just_a_disjunction(["SingleTypeImportDeclaration";"TypeImportOnDemandDeclaration";"SingleStaticImportDeclaration";"StaticImportOnDemandDeclaration"]));
   ("InclusiveOrExpression",Just_a_disjunction(["ExclusiveOrExpression";"CompoundInclusiveOrExpression"]));
   ("InstanceInitializer",Synonym("Block"));
   ("InstanceofExpression",Just_a_disjunction(["ShortInstanceofExpression";"LongInstanceofExpression"]));
   ("IntegerLiteral",Just_atomic([INTEGER_LITERAL_T]));
   ("IntegralType",Just_a_disjunction(["AtomicByte";"AtomicShort";"AtomicInt";"AtomicLong";"AtomicChar"]));
   ("InterfaceDeclaration",Just_a_disjunction(["NormalInterfaceDeclaration";"AnnotationInterfaceDeclaration"]));
   ("InterfaceExtends",Just_a_concat(["AtomicExtends";"InterfaceType";"StarredInterfaceTypePrecededByComma"]));
   ("InterfaceMemberDeclaration",Just_a_disjunction(["ConstantDeclaration";"InterfaceMethodDeclaration";"ClassDeclaration";"InterfaceDeclaration";"AtomicSm"]));
   ("InterfaceMethodDeclaration",Just_a_concat(["StarredInterfaceMethodModifier";"MethodHeader";"MethodBody"]));
   ("InterfaceMethodModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicPrivate";"AtomicAbstract";"AtomicDefault";"AtomicStatic";"AtomicStrictfp"]));
   ("InterfaceModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicProtected";"AtomicPrivate";"AtomicAbstract";"AtomicStatic";"AtomicSealed";"AtomicNonsealed";"AtomicStrictfp"]));
   ("InterfacePermits",Just_a_concat(["AtomicPermits";"TypeName";"StarredTypeNamePrecededByComma"]));
   ("InterfaceType",Synonym("ClassType"));
   ("InterfaceTypePrecededByComma",Just_a_concat(["AtomicCm";"InterfaceType"]));
   ("LabeledStatement",Just_a_concat(["MolecularIdentifier_Colon";"Statement"]));
   ("LabeledStatementNoShortIf",Just_a_concat(["MolecularIdentifier_Colon";"StatementNoShortIf"]));
   ("LambdaBody",Just_a_disjunction(["Expression";"Block"]));
   ("LambdaExpression",Just_a_concat(["LambdaParameters";"MolecularMinus_Gt";"LambdaBody"]));
   ("LambdafulConditionalAndExpressionConditionalExpression",Just_a_concat(["ConditionalOrExpression";"AtomicCond";"Expression";"AtomicColon";"LambdaParameters";"MolecularMinus_Gt";"LambdaBody"]));
   ("LambdalessConditionalAndExpressionConditionalExpression",Just_a_concat(["ConditionalOrExpression";"AtomicCond";"Expression";"AtomicColon";"ConditionalExpression"]));
   ("LambdaParameter",Just_a_disjunction(["HeavyLambdaParameter";"UsualParameter"]));
   ("LambdaParameterList",Just_a_disjunction(["LambdayLambdaParameterList";"IdentifieryLambdaParameterList"]));
   ("LambdaParameterPrecededByComma",Just_a_concat(["AtomicCm";"LambdaParameter"]));
   ("LambdaParameters",Just_a_disjunction(["ListyLambdaParameters";"Identifier"]));
   ("LambdaParameterType",Just_a_disjunction(["UnannType";"AtomicVar"]));
   ("LambdayLambdaParameterList",Just_a_concat(["LambdaParameter";"StarredLambdaParameterPrecededByComma"]));
   ("LeftHandSide",Just_a_disjunction(["ExpressionName";"FieldAccess";"ArrayAccess"]));
   ("ListyLambdaParameters",Just_a_concat(["AtomicLp";"OptionalLambdaParameterList";"AtomicRp"]));
   ("Literal",Just_a_disjunction(["IntegerLiteral";"FloatingPointLiteral";"BooleanLiteral";"CharacterLiteral";"StringLiteral";"TextBlock";"NullLiteral"]));
   ("LocalClassOrInterfaceDeclaration",Just_a_disjunction(["ClassDeclaration";"NormalInterfaceDeclaration"]));
   ("LocalVariableDeclaration",Just_a_concat(["StarredVariableModifier";"LocalVariableType";"Identifier";"OptionalDims";"OptionalEqualsVariableInitializer";"StarredVariableDeclaratorPrecededByComma"]));
   ("LocalVariableDeclarationStatement",Just_a_concat(["StarredVariableModifier";"LocalVariableType";"Identifier";"OptionalDims";"OptionalEqualsVariableInitializer";"StarredVariableDeclaratorPrecededByComma";"AtomicSm"]));
   ("LocalVariableType",Just_a_disjunction(["UnannType";"AtomicVar"]));
   ("LongArrayAccess",Just_a_concat(["PrimaryNoNewArray";"AtomicLb";"Expression";"AtomicRb"]));
   ("LongClassType",Just_a_concat(["ClassOrInterfaceType";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"]));
   ("LongInstanceofExpression",Just_a_concat(["RelationalExpression";"AtomicInstanceof";"Pattern"]));
   ("LongTryStatement",Just_a_concat(["MolecularTry_Lb";"OptionalBlockStatements";"MolecularRb_Catch_Lp";"StarredVariableModifier";"UnannClassType";"StarredClassTypePrecededByVerticalBar";"Identifier";"OptionalDims";"MolecularRp_Lb";"OptionalBlockStatements";"AtomicRb";"StarredCatchClause"]));
   ("LongUnannClassType",Just_a_concat(["UnannClassOrInterfaceType";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"]));
   ("MackerelClassInstanceCreationExpression",Just_a_concat(["ExpressionName";"MolecularDot_New";"OptionalTypeArguments";"StarredAnnotation";"Identifier";"StarredAnnotatedIdentifierrPrecededByDot";"OptionalTypeArgumentsOrDiamond";"AtomicLp";"OptionalArgumentList";"AtomicRp";"OptionalClassBody"]));
   ("MackerelCreationExpressionArrayCreationExpression",Just_a_concat(["AtomicNew";"ClassOrInterfaceType";"StarredAnnotation";"AtomicLb";"Expression";"AtomicRb";"StarredDimExpr";"OptionalDims"]));
   ("MackerelExplicitConstructorInvocation",Just_a_concat(["OptionalTypeArguments";"MolecularSuper_Lp";"OptionalArgumentList";"MolecularRp_Sm"]));
   ("MappyCastExpression",Just_a_concat(["AtomicLp";"ReferenceType";"StarredAdditionalBound";"AtomicRp";"LambdaParameters";"MolecularMinus_Gt";"LambdaBody"]));
   ("MarkerAnnotation",Just_a_concat(["AtomicSnail";"TypeName"]));
   ("MethodBody",Just_a_disjunction(["Block";"AtomicSm"]));
   ("MethodDeclaration",Just_a_concat(["StarredMethodModifier";"MethodHeader";"MethodBody"]));
   ("MethodHeader",Just_a_disjunction(["BasicMethodHeader";"DiamondlyMethodHeader"]));
   ("MethodInvocation",Just_a_disjunction(["BasicMethodInvocation";"TypeyMethodInvocation";"ExpressionyMethodInvocation";"PrimaryyMethodInvocation";"ThisSuperMethodInvocation";"OtherSuperMethodInvocation"]));
   ("MethodModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicProtected";"AtomicPrivate";"AtomicAbstract";"AtomicStatic";"AtomicFinal";"AtomicSynchronized";"AtomicNative";"AtomicStrictfp"]));
   ("MethodName",Synonym("Identifier"));
   ("MethodReference",Just_a_disjunction(["ExpressionyMethodReference";"PrimaryyMethodReference";"ReferenceTypeyMethodReference";"ThisSuperMethodReference";"TypeNameyMethodReference";"ClassTypeyMethodReference";"ArrayTypeyMethodReference"]));
   ("ModuleName",Just_a_disjunction(["Identifier";"CompoundModuleName"]));
   ("MolecularCatch_Lp",Just_atomic([CATCH_T;LP_T]));
   ("MolecularClass_Identifier",Just_atomic([CLASS_T;IDENTIFIER_T]));
   ("MolecularCm_Identifier",Just_atomic([CM_T;IDENTIFIER_T]));
   ("MolecularCm_Identifier_Eq",Just_atomic([CM_T;IDENTIFIER_T;EQ_T]));
   ("MolecularColon_Colon",Just_atomic([COLON_T;COLON_T]));
   ("MolecularColon_Colon_New",Just_atomic([COLON_T;COLON_T;NEW_T]));
   ("MolecularDot_Class",Just_atomic([DOT_T;CLASS_T]));
   ("MolecularDot_Dot_Dot_Identifier",Just_atomic([DOT_T;DOT_T;DOT_T;IDENTIFIER_T]));
   ("MolecularDot_Identifier",Just_atomic([DOT_T;IDENTIFIER_T]));
   ("MolecularDot_Identifier_Sm",Just_atomic([DOT_T;IDENTIFIER_T;SM_T]));
   ("MolecularDot_New",Just_atomic([DOT_T;NEW_T]));
   ("MolecularDot_Super_Colon_Colon",Just_atomic([DOT_T;SUPER_T;COLON_T;COLON_T]));
   ("MolecularDot_Super_Dot",Just_atomic([DOT_T;SUPER_T;DOT_T]));
   ("MolecularDot_Super_Dot_Identifier",Just_atomic([DOT_T;SUPER_T;DOT_T;IDENTIFIER_T]));
   ("MolecularDot_This",Just_atomic([DOT_T;THIS_T]));
   ("MolecularDot_Times_Sm",Just_atomic([DOT_T;TIMES_T;SM_T]));
   ("MolecularEnum_Identifier",Just_atomic([ENUM_T;IDENTIFIER_T]));
   ("MolecularFinally_Lb",Just_atomic([FINALLY_T;LB_T]));
   ("MolecularFor_Lp",Just_atomic([FOR_T;LP_T]));
   ("MolecularIdentifier_Colon",Just_atomic([IDENTIFIER_T;COLON_T]));
   ("MolecularIdentifier_Dot",Just_atomic([IDENTIFIER_T;DOT_T]));
   ("MolecularIdentifier_Eq",Just_atomic([IDENTIFIER_T;EQ_T]));
   ("MolecularIdentifier_Lp",Just_atomic([IDENTIFIER_T;LP_T]));
   ("MolecularIdentifier_Lp_Rp",Just_atomic([IDENTIFIER_T;LP_T;RP_T]));
   ("MolecularIf_Lp",Just_atomic([IF_T;LP_T]));
   ("MolecularImport_Static",Just_atomic([IMPORT_T;STATIC_T]));
   ("MolecularInterface_Identifier",Just_atomic([INTERFACE_T;IDENTIFIER_T]));
   ("MolecularLb_Rb",Just_atomic([LB_T;RB_T]));
   ("MolecularLt_Gt",Just_atomic([LT_T;GT_T]));
   ("MolecularMinus_Gt",Just_atomic([MINUS_T;GT_T]));
   ("MolecularMinus_Gt_Lb",Just_atomic([MINUS_T;GT_T;LB_T]));
   ("MolecularMinus_Gt_Throw",Just_atomic([MINUS_T;GT_T;THROW_T]));
   ("MolecularPackage_Identifier",Just_atomic([PACKAGE_T;IDENTIFIER_T]));
   ("MolecularRb_Catch_Lp",Just_atomic([RB_T;CATCH_T;LP_T]));
   ("MolecularRecord_Identifier",Just_atomic([RECORD_T;IDENTIFIER_T]));
   ("MolecularRp_Lb",Just_atomic([RP_T;LB_T]));
   ("MolecularRp_Sm",Just_atomic([RP_T;SM_T]));
   ("MolecularSnail_Interface_Identifier_Lb",Just_atomic([SNAIL_T;INTERFACE_T;IDENTIFIER_T;LB_T]));
   ("MolecularStatic_Lb",Just_atomic([STATIC_T;LB_T]));
   ("MolecularSuper_Colon_Colon",Just_atomic([SUPER_T;COLON_T;COLON_T]));
   ("MolecularSuper_Dot",Just_atomic([SUPER_T;DOT_T]));
   ("MolecularSuper_Dot_Identifier",Just_atomic([SUPER_T;DOT_T;IDENTIFIER_T]));
   ("MolecularSuper_Lp",Just_atomic([SUPER_T;LP_T]));
   ("MolecularSwitch_Lp",Just_atomic([SWITCH_T;LP_T]));
   ("MolecularSynchronized_Lp",Just_atomic([SYNCHRONIZED_T;LP_T]));
   ("MolecularThis_Cm",Just_atomic([THIS_T;CM_T]));
   ("MolecularThis_Lp",Just_atomic([THIS_T;LP_T]));
   ("MolecularTry_Lb",Just_atomic([TRY_T;LB_T]));
   ("MolecularTry_Lp",Just_atomic([TRY_T;LP_T]));
   ("MolecularVoid_Dot_Class",Just_atomic([VOID_T;DOT_T;CLASS_T]));
   ("MolecularWhile_Lp",Just_atomic([WHILE_T;LP_T]));
   ("MultiplicativeExpression",Just_a_disjunction(["UnaryExpression";"UsingTimesMultiplicativeExpression";"UsingDivMultiplicativeExpression";"UsingModMultiplicativeExpression"]));
   ("NonEmptyTypeArgumentsOrDiamond",Just_a_concat(["AtomicLt";"TypeArgument";"StarredTypeArgumentPrecededByComma";"AtomicGt"]));
   ("NonmappyCastExpression",Just_a_concat(["AtomicLp";"ReferenceType";"StarredAdditionalBound";"AtomicRp";"UnaryExpressionNotPlusMinus"]));
   ("NormalAnnotation",Just_a_concat(["AtomicSnail";"TypeName";"AtomicLp";"OptionalElementValuePairList";"AtomicRp"]));
   ("NormalClassDeclaration",Just_a_concat(["StarredClassModifier";"MolecularClass_Identifier";"OptionalTypeParameters";"OptionalClassExtends";"OptionalClassImplements";"OptionalClassPermits";"AtomicLb";"StarredClassBodyDeclaration";"AtomicRb"]));
   ("NormalInterfaceDeclaration",Just_a_concat(["StarredInterfaceModifier";"MolecularInterface_Identifier";"OptionalTypeParameters";"OptionalInterfaceExtends";"OptionalInterfacePermits";"AtomicLb";"StarredInterfaceMemberDeclaration";"AtomicRb"]));
   ("NullLiteral",Just_atomic([NULL_LITERAL_T]));
   ("NumericClassLiteral",Just_a_concat(["NumericType";"StarredOpenSquare";"MolecularDot_Class"]));
   ("NumericType",Just_a_disjunction(["IntegralType";"FloatingPointType"]));
   ("NumericyPrimitiveType",Just_a_concat(["StarredAnnotation";"NumericType"]));
   ("OpenSquare",Just_a_concat(["MolecularLb_Rb"]));
   ("OptionalArgumentList",Just_an_optional("ArgumentList"));
   ("OptionalBlockStatements",Just_an_optional("BlockStatements"));
   ("OptionalCatches",Just_an_optional("Catches"));
   ("OptionalClassBody",Just_an_optional("ClassBody"));
   ("OptionalClassExtends",Just_an_optional("ClassExtends"));
   ("OptionalClassImplements",Just_an_optional("ClassImplements"));
   ("OptionalClassPermits",Just_an_optional("ClassPermits"));
   ("OptionalCm",Just_an_optional("AtomicCm"));
   ("OptionalDefaultValue",Just_an_optional("DefaultValue"));
   ("OptionalDims",Just_an_optional("Dims"));
   ("OptionalElementValueList",Just_an_optional("ElementValueList"));
   ("OptionalElementValuePairList",Just_an_optional("ElementValuePairList"));
   ("OptionalEnumBodyDeclarations",Just_an_optional("EnumBodyDeclarations"));
   ("OptionalEnumConstantList",Just_an_optional("EnumConstantList"));
   ("OptionalEqualsVariableInitializer",Just_an_optional("EqualsVariableInitializer"));
   ("OptionalExplicitConstructorInvocation",Just_an_optional("ExplicitConstructorInvocation"));
   ("OptionalExpression",Just_an_optional("Expression"));
   ("OptionalFinally",Just_an_optional("Finally"));
   ("OptionalForInit",Just_an_optional("ForInit"));
   ("OptionalFormalParameterList",Just_an_optional("FormalParameterList"));
   ("OptionalForUpdate",Just_an_optional("ForUpdate"));
   ("OptionalIdentifier",Just_an_optional("Identifier"));
   ("OptionalIdentifierFollowedByDot",Just_an_optional("IdentifierFollowedByDot"));
   ("OptionalInterfaceExtends",Just_an_optional("InterfaceExtends"));
   ("OptionalInterfacePermits",Just_an_optional("InterfacePermits"));
   ("OptionalLambdaParameterList",Just_an_optional("LambdaParameterList"));
   ("OptionalPackageDeclaration",Just_an_optional("PackageDeclaration"));
   ("OptionalParenthesedArgumentList",Just_an_optional("ParenthesedArgumentList"));
   ("OptionalReceiverParameterFollowedByComma",Just_an_optional("ReceiverParameterFollowedByComma"));
   ("OptionalRecordComponentList",Just_an_optional("RecordComponentList"));
   ("OptionalSm",Just_an_optional("AtomicSm"));
   ("OptionalThrows",Just_an_optional("Throws"));
   ("OptionalTypeArguments",Just_an_optional("TypeArguments"));
   ("OptionalTypeArgumentsOrDiamond",Just_an_optional("TypeArgumentsOrDiamond"));
   ("OptionalTypeBound",Just_an_optional("TypeBound"));
   ("OptionalTypeParameters",Just_an_optional("TypeParameters"));
   ("OptionalVariableInitializerList",Just_an_optional("VariableInitializerList"));
   ("OptionalWildcardBounds",Just_an_optional("WildcardBounds"));
   ("OrdinaryCompilationUnit",Just_a_concat(["OptionalPackageDeclaration";"StarredImportDeclaration";"StarredTopLevelClassOrInterfaceDeclaration"]));
   ("OtherSuperMethodInvocation",Just_a_concat(["TypeName";"MolecularDot_Super_Dot";"OptionalTypeArguments";"MolecularIdentifier_Lp";"OptionalArgumentList";"AtomicRp"]));
   ("PackageDeclaration",Just_a_concat(["StarredPackageModifier";"MolecularPackage_Identifier";"StarredMolecularDot_Identifier";"AtomicSm"]));
   ("PackageModifier",Synonym("Annotation"));
   ("PackageName",Just_a_disjunction(["Identifier";"CompoundPackageName"]));
   ("PackageOrTypeName",Just_a_disjunction(["Identifier";"CompoundPackageOrTypeName"]));
   ("ParenthesedArgumentList",Just_a_concat(["AtomicLp";"OptionalArgumentList";"AtomicRp"]));
   ("ParenthesedPrimaryNoNewArray",Just_a_concat(["AtomicLp";"Expression";"AtomicRp"]));
   ("ParenthesedTryStatement",Just_a_concat(["MolecularTry_Lp";"Resource";"StarredResourcePrecededBySemiColon";"OptionalSm";"MolecularRp_Lb";"OptionalBlockStatements";"AtomicRb";"OptionalCatches";"OptionalFinally"]));
   ("ParenthesedUnaryExpressionNotPlusMinus",Just_a_concat(["MolecularSwitch_Lp";"Expression";"AtomicRp";"SwitchBlock"]));
   ("Pattern",Synonym("TypePattern"));
   ("PlasticArrayType",Just_a_concat(["StarredAnnotation";"Identifier";"StarredAnnotation";"MolecularLb_Rb";"StarredDimsElement"]));
   ("PostDecrementExpression",Just_a_concat(["PostfixExpression";"AtomicDecr"]));
   ("PostfixExpression",Just_a_disjunction(["Primary";"ExpressionName";"PostIncrementExpression";"PostDecrementExpression"]));
   ("PostIncrementExpression",Just_a_concat(["PostfixExpression";"AtomicIncr"]));
   ("PreDecrementExpression",Just_a_concat(["AtomicDecr";"UnaryExpression"]));
   ("PreIncrementExpression",Just_a_concat(["AtomicIncr";"UnaryExpression"]));
   ("Primary",Just_a_disjunction(["PrimaryNoNewArray";"ArrayCreationExpression"]));
   ("PrimaryNoNewArray",Just_a_disjunction(["Literal";"ClassLiteral";"AtomicThis";"UsingThisPrimaryNoNewArray";"ParenthesedPrimaryNoNewArray";"ClassInstanceCreationExpression";"FieldAccess";"ArrayAccess";"MethodInvocation";"MethodReference"]));
   ("PrimaryyMethodInvocation",Just_a_concat(["Primary";"AtomicDot";"OptionalTypeArguments";"MolecularIdentifier_Lp";"OptionalArgumentList";"AtomicRp"]));
   ("PrimaryyMethodReference",Just_a_concat(["Primary";"MolecularColon_Colon";"OptionalTypeArguments";"Identifier"]));
   ("PrimitiveArrayType",Just_a_concat(["PrimitiveType";"StarredAnnotation";"MolecularLb_Rb";"StarredDimsElement"]));
   ("PrimitiveType",Just_a_disjunction(["NumericyPrimitiveType";"BoolyPrimitiveType"]));
   ("PrimitiveyUnannArrayType",Just_a_concat(["UnannPrimitiveType";"StarredAnnotation";"MolecularLb_Rb";"StarredDimsElement"]));
   ("ReceiverParameterFollowedByComma",Just_a_concat(["StarredAnnotation";"UnannType";"OptionalIdentifierFollowedByDot";"MolecularThis_Cm"]));
   ("RecordBodyDeclaration",Just_a_disjunction(["ClassBodyDeclaration";"CompactConstructorDeclaration"]));
   ("RecordComponent",Just_a_disjunction(["BasicRecordComponent";"UsingThreeDotsRecordComponent"]));
   ("RecordComponentList",Just_a_concat(["RecordComponent";"StarredRecordComponentPrecededByComma"]));
   ("RecordComponentModifier",Synonym("Annotation"));
   ("RecordComponentPrecededByComma",Just_a_concat(["AtomicCm";"RecordComponent"]));
   ("RecordDeclaration",Just_a_concat(["StarredClassModifier";"MolecularRecord_Identifier";"OptionalTypeParameters";"AtomicLp";"OptionalRecordComponentList";"AtomicRp";"OptionalClassImplements";"AtomicLb";"StarredRecordBodyDeclaration";"AtomicRb"]));
   ("ReferenceType",Just_a_disjunction(["ClassOrInterfaceType";"TypeVariable";"ArrayType"]));
   ("ReferenceTypeyMethodReference",Just_a_concat(["ReferenceType";"MolecularColon_Colon";"OptionalTypeArguments";"Identifier"]));
   ("RelationalExpression",Just_a_disjunction(["ShiftExpression";"UsingLtRelationalExpression";"UsingGtRelationalExpression";"UsingLeRelationalExpression";"UsingGeRelationalExpression";"InstanceofExpression"]));
   ("Resource",Just_a_disjunction(["LocalVariableDeclaration";"VariableAccess"]));
   ("ResourcePrecededBySemiColon",Just_a_concat(["AtomicSm";"Resource"]));
   ("Result",Just_a_disjunction(["UnannType";"AtomicVoid"]));
   ("ReturnStatement",Just_a_concat(["AtomicReturn";"OptionalExpression";"AtomicSm"]));
   ("SalmonClassInstanceCreationExpression",Just_a_concat(["Primary";"MolecularDot_New";"OptionalTypeArguments";"StarredAnnotation";"Identifier";"StarredAnnotatedIdentifierrPrecededByDot";"OptionalTypeArgumentsOrDiamond";"AtomicLp";"OptionalArgumentList";"AtomicRp";"OptionalClassBody"]));
   ("SalmonCreationExpressionArrayCreationExpression",Just_a_concat(["AtomicNew";"PrimitiveType";"StarredAnnotation";"MolecularLb_Rb";"StarredDimsElement";"AtomicLb";"OptionalVariableInitializerList";"OptionalCm";"AtomicRb"]));
   ("SalmonExplicitConstructorInvocation",Just_a_concat(["ExpressionName";"AtomicDot";"OptionalTypeArguments";"MolecularSuper_Lp";"OptionalArgumentList";"MolecularRp_Sm"]));
   ("ShiftExpression",Just_a_disjunction(["AdditiveExpression";"UsingLsShiftExpression";"UsingSrsShiftExpression";"UsingUrsShiftExpression"]));
   ("ShortArrayAccess",Just_a_concat(["ExpressionName";"AtomicLb";"Expression";"AtomicRb"]));
   ("ShortClassType",Just_a_concat(["StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"]));
   ("ShortInstanceofExpression",Just_a_concat(["RelationalExpression";"AtomicInstanceof";"ReferenceType"]));
   ("ShortTryStatement",Just_a_concat(["MolecularTry_Lb";"OptionalBlockStatements";"AtomicRb";"OptionalCatches";"MolecularFinally_Lb";"OptionalBlockStatements";"AtomicRb"]));
   ("ShortUnannClassType",Just_a_concat(["TypeIdentifier";"OptionalTypeArguments"]));
   ("SimplestFieldAccess",Just_a_concat(["Primary";"MolecularDot_Identifier"]));
   ("SimpleTypeName",Synonym("Identifier"));
   ("SingleElementAnnotation",Just_a_concat(["AtomicSnail";"TypeName";"AtomicLp";"ElementValue";"AtomicRp"]));
   ("SingleStaticImportDeclaration",Just_a_concat(["MolecularImport_Static";"TypeName";"MolecularDot_Identifier_Sm"]));
   ("SingleTypeImportDeclaration",Just_a_concat(["AtomicImport";"TypeName";"AtomicSm"]));
   ("StarredAdditionalBound",Just_a_star("AdditionalBound"));
   ("StarredAnnotatedIdentifierrPrecededByDot",Just_a_star("AnnotatedIdentifierrPrecededByDot"));
   ("StarredAnnotation",Just_a_star("Annotation"));
   ("StarredAnnotationInterfaceElementModifier",Just_a_star("AnnotationInterfaceElementModifier"));
   ("StarredAnnotationInterfaceMemberDeclaration",Just_a_star("AnnotationInterfaceMemberDeclaration"));
   ("StarredBlockStatement",Just_a_star("BlockStatement"));
   ("StarredCaseConstantPrecededByComma",Just_a_star("CaseConstantPrecededByComma"));
   ("StarredCatchClause",Just_a_star("CatchClause"));
   ("StarredClassBodyDeclaration",Just_a_star("ClassBodyDeclaration"));
   ("StarredClassModifier",Just_a_star("ClassModifier"));
   ("StarredClassTypePrecededByVerticalBar",Just_a_star("ClassTypePrecededByVerticalBar"));
   ("StarredConstantModifier",Just_a_star("ConstantModifier"));
   ("StarredConstructorModifier",Just_a_star("ConstructorModifier"));
   ("StarredDimExpr",Just_a_star("DimExpr"));
   ("StarredDimsElement",Just_a_star("DimsElement"));
   ("StarredElementValuePairPrecededByComma",Just_a_star("ElementValuePairPrecededByComma"));
   ("StarredElementValuePrecededByComma",Just_a_star("ElementValuePrecededByComma"));
   ("StarredEnumConstantModifier",Just_a_star("EnumConstantModifier"));
   ("StarredEnumConstantPrecededByComma",Just_a_star("EnumConstantPrecededByComma"));
   ("StarredExceptionTypePrecededByComma",Just_a_star("ExceptionTypePrecededByComma"));
   ("StarredExpressionPrecededByComma",Just_a_star("ExpressionPrecededByComma"));
   ("StarredFieldModifier",Just_a_star("FieldModifier"));
   ("StarredFormalParameterPrecededByComma",Just_a_star("FormalParameterPrecededByComma"));
   ("StarredIdentifierPrecededByComma",Just_a_star("IdentifierPrecededByComma"));
   ("StarredImportDeclaration",Just_a_star("ImportDeclaration"));
   ("StarredInterfaceMemberDeclaration",Just_a_star("InterfaceMemberDeclaration"));
   ("StarredInterfaceMethodModifier",Just_a_star("InterfaceMethodModifier"));
   ("StarredInterfaceModifier",Just_a_star("InterfaceModifier"));
   ("StarredInterfaceTypePrecededByComma",Just_a_star("InterfaceTypePrecededByComma"));
   ("StarredLambdaParameterPrecededByComma",Just_a_star("LambdaParameterPrecededByComma"));
   ("StarredMethodModifier",Just_a_star("MethodModifier"));
   ("StarredMolecularDot_Identifier",Just_a_star("MolecularDot_Identifier"));
   ("StarredOpenSquare",Just_a_star("OpenSquare"));
   ("StarredPackageModifier",Just_a_star("PackageModifier"));
   ("StarredRecordBodyDeclaration",Just_a_star("RecordBodyDeclaration"));
   ("StarredRecordComponentModifier",Just_a_star("RecordComponentModifier"));
   ("StarredRecordComponentPrecededByComma",Just_a_star("RecordComponentPrecededByComma"));
   ("StarredResourcePrecededBySemiColon",Just_a_star("ResourcePrecededBySemiColon"));
   ("StarredStatementExpressionPrecededByComma",Just_a_star("StatementExpressionPrecededByComma"));
   ("StarredSwitchBlockStatementGroup",Just_a_star("SwitchBlockStatementGroup"));
   ("StarredSwitchLabelFollowedByColon",Just_a_star("SwitchLabelFollowedByColon"));
   ("StarredSwitchRule",Just_a_star("SwitchRule"));
   ("StarredTopLevelClassOrInterfaceDeclaration",Just_a_star("TopLevelClassOrInterfaceDeclaration"));
   ("StarredTypeArgumentPrecededByComma",Just_a_star("TypeArgumentPrecededByComma"));
   ("StarredTypeNamePrecededByComma",Just_a_star("TypeNamePrecededByComma"));
   ("StarredTypeParameterModifier",Just_a_star("TypeParameterModifier"));
   ("StarredTypeParameterPrecededByComma",Just_a_star("TypeParameterPrecededByComma"));
   ("StarredVariableDeclaratorPrecededByComma",Just_a_star("VariableDeclaratorPrecededByComma"));
   ("StarredVariableInitializerPrecededByComma",Just_a_star("VariableInitializerPrecededByComma"));
   ("StarredVariableModifier",Just_a_star("VariableModifier"));
   ("Statement",Just_a_disjunction(["StatementWithoutTrailingSubstatement";"LabeledStatement";"IfThenStatement";"IfThenElseStatement";"WhileStatement";"ForStatement"]));
   ("StatementExpression",Just_a_disjunction(["Assignment";"PreIncrementExpression";"PreDecrementExpression";"PostIncrementExpression";"PostDecrementExpression";"MethodInvocation";"ClassInstanceCreationExpression"]));
   ("StatementExpressionList",Just_a_concat(["StatementExpression";"StarredStatementExpressionPrecededByComma"]));
   ("StatementExpressionPrecededByComma",Just_a_concat(["AtomicCm";"StatementExpression"]));
   ("StatementNoShortIf",Just_a_disjunction(["StatementWithoutTrailingSubstatement";"LabeledStatementNoShortIf";"IfThenElseStatementNoShortIf";"WhileStatementNoShortIf";"ForStatementNoShortIf"]));
   ("StatementWithoutTrailingSubstatement",Just_a_disjunction(["Block";"EmptyStatement";"ExpressionStatement";"AssertStatement";"SwitchStatement";"DoStatement";"BreakStatement";"ContinueStatement";"ReturnStatement";"SynchronizedStatement";"ThrowStatement";"TryStatement";"YieldStatement"]));
   ("StaticImportOnDemandDeclaration",Just_a_concat(["MolecularImport_Static";"TypeName";"MolecularDot_Times_Sm"]));
   ("StaticInitializer",Just_a_concat(["MolecularStatic_Lb";"OptionalBlockStatements";"AtomicRb"]));
   ("StrictFormalParameter",Just_a_concat(["StarredVariableModifier";"UnannType";"Identifier";"OptionalDims"]));
   ("StringLiteral",Just_atomic([STRING_LITERAL_T]));
   ("SwitchBlock",Just_a_disjunction(["BasicSwitchBlock";"CompoundSwitchBlock"]));
   ("SwitchBlockStatementGroup",Just_a_concat(["SwitchLabel";"AtomicColon";"StarredSwitchLabelFollowedByColon";"BlockStatement";"StarredBlockStatement"]));
   ("SwitchLabel",Just_a_disjunction(["CompoundSwitchLabel";"AtomicDefault"]));
   ("SwitchLabelFollowedByColon",Just_a_concat(["SwitchLabel";"AtomicColon"]));
   ("SwitchRule",Just_a_disjunction(["BasicSwitchRule";"UsingBracketsSwitchRule";"UsingAThrowSwitchRule"]));
   ("SwitchStatement",Just_a_concat(["MolecularSwitch_Lp";"Expression";"AtomicRp";"SwitchBlock"]));
   ("SynchronizedStatement",Just_a_concat(["MolecularSynchronized_Lp";"Expression";"MolecularRp_Lb";"OptionalBlockStatements";"AtomicRb"]));
   ("TextBlock",Just_atomic([TEXT_BLOCK_T]));
   ("ThisSuperMethodInvocation",Just_a_concat(["MolecularSuper_Dot";"OptionalTypeArguments";"MolecularIdentifier_Lp";"OptionalArgumentList";"AtomicRp"]));
   ("ThisSuperMethodReference",Just_a_concat(["MolecularSuper_Colon_Colon";"OptionalTypeArguments";"Identifier"]));
   ("Throws",Just_a_concat(["AtomicThrows";"ExceptionType";"StarredExceptionTypePrecededByComma"]));
   ("ThrowStatement",Just_a_concat(["AtomicThrow";"Expression";"AtomicSm"]));
   ("TopLevelClassOrInterfaceDeclaration",Just_a_disjunction(["ClassDeclaration";"InterfaceDeclaration";"AtomicSm"]));
   ("TryStatement",Just_a_disjunction(["LongTryStatement";"ShortTryStatement";"ParenthesedTryStatement"]));
   ("TunaCreationExpressionArrayCreationExpression",Just_a_concat(["AtomicNew";"ClassOrInterfaceType";"StarredAnnotation";"MolecularLb_Rb";"StarredDimsElement";"AtomicLb";"OptionalVariableInitializerList";"OptionalCm";"AtomicRb"]));
   ("TunaExplicitConstructorInvocation",Just_a_concat(["Primary";"AtomicDot";"OptionalTypeArguments";"MolecularSuper_Lp";"OptionalArgumentList";"MolecularRp_Sm"]));
   ("TypeArgument",Just_a_disjunction(["ReferenceType";"Wildcard"]));
   ("TypeArgumentPrecededByComma",Just_a_concat(["AtomicCm";"TypeArgument"]));
   ("TypeArguments",Just_a_concat(["AtomicLt";"TypeArgument";"StarredTypeArgumentPrecededByComma";"AtomicGt"]));
   ("TypeArgumentsOrDiamond",Just_a_disjunction(["NonEmptyTypeArgumentsOrDiamond";"MolecularLt_Gt"]));
   ("TypeBound",Just_a_disjunction(["IdentifieryTypeBound";"GlassTypeBound"]));
   ("TypeIdentifier",Synonym("Identifier"));
   ("TypeImportOnDemandDeclaration",Just_a_concat(["AtomicImport";"PackageOrTypeName";"MolecularDot_Times_Sm"]));
   ("TypeName",Just_a_disjunction(["TypeIdentifier";"CompoundTypeName"]));
   ("TypeNamePrecededByComma",Just_a_concat(["AtomicCm";"TypeName"]));
   ("TypeNameyMethodReference",Just_a_concat(["TypeName";"MolecularDot_Super_Colon_Colon";"OptionalTypeArguments";"Identifier"]));
   ("TypeParameterModifier",Synonym("Annotation"));
   ("TypeParameterPrecededByComma",Just_a_concat(["AtomicCm";"StarredTypeParameterModifier";"Identifier";"OptionalTypeBound"]));
   ("TypeParameters",Just_a_concat(["AtomicLt";"StarredTypeParameterModifier";"Identifier";"OptionalTypeBound";"StarredTypeParameterPrecededByComma";"AtomicGt"]));
   ("TypePattern",Synonym("LocalVariableDeclaration"));
   ("TypeVariable",Just_a_concat(["StarredAnnotation";"Identifier"]));
   ("TypeyMethodInvocation",Just_a_concat(["TypeName";"AtomicDot";"OptionalTypeArguments";"MolecularIdentifier_Lp";"OptionalArgumentList";"AtomicRp"]));
   ("TypeyUnannArrayType",Just_a_concat(["UnannTypeVariable";"StarredAnnotation";"MolecularLb_Rb";"StarredDimsElement"]));
   ("UnannArrayType",Just_a_disjunction(["PrimitiveyUnannArrayType";"GlassUnannArrayType";"TypeyUnannArrayType"]));
   ("UnannClassOrInterfaceType",Just_a_disjunction(["UnannClassType";"UnannInterfaceType"]));
   ("UnannClassType",Just_a_disjunction(["ShortUnannClassType";"UsualClassType";"LongUnannClassType"]));
   ("UnannInterfaceType",Synonym("UnannClassType"));
   ("UnannPrimitiveType",Just_a_disjunction(["NumericType";"AtomicBoolean"]));
   ("UnannReferenceType",Just_a_disjunction(["UnannClassOrInterfaceType";"UnannTypeVariable";"UnannArrayType"]));
   ("UnannType",Just_a_disjunction(["UnannPrimitiveType";"UnannReferenceType"]));
   ("UnannTypeVariable",Synonym("Identifier"));
   ("UnaryExpression",Just_a_disjunction(["UsingIncrUnaryExpression";"UsingDecrUnaryExpression";"UsingPlusUnaryExpression";"UsingMinusUnaryExpression";"UnaryExpressionNotPlusMinus"]));
   ("UnaryExpressionNotPlusMinus",Just_a_disjunction(["PostfixExpression";"UsingComplUnaryExpressionNotPlusMinus";"UsingNotUnaryExpressionNotPlusMinus";"CastExpression";"ParenthesedUnaryExpressionNotPlusMinus"]));
   ("UserDefinedClassLiteral",Just_a_concat(["TypeName";"StarredOpenSquare";"MolecularDot_Class"]));
   ("UsingAThrowSwitchRule",Just_a_concat(["SwitchLabel";"MolecularMinus_Gt_Throw";"Expression";"AtomicSm"]));
   ("UsingBracketsSwitchRule",Just_a_concat(["SwitchLabel";"MolecularMinus_Gt_Lb";"OptionalBlockStatements";"AtomicRb"]));
   ("UsingComplUnaryExpressionNotPlusMinus",Just_a_concat(["AtomicCompl";"UnaryExpression"]));
   ("UsingDecrUnaryExpression",Just_a_concat(["AtomicDecr";"UnaryExpression"]));
   ("UsingDivMultiplicativeExpression",Just_a_concat(["MultiplicativeExpression";"AtomicDiv";"UnaryExpression"]));
   ("UsingExtendsWildcardBounds",Just_a_concat(["AtomicExtends";"ReferenceType"]));
   ("UsingGeRelationalExpression",Just_a_concat(["RelationalExpression";"AtomicGe";"ShiftExpression"]));
   ("UsingGtRelationalExpression",Just_a_concat(["RelationalExpression";"AtomicGt";"ShiftExpression"]));
   ("UsingIncrUnaryExpression",Just_a_concat(["AtomicIncr";"UnaryExpression"]));
   ("UsingLeRelationalExpression",Just_a_concat(["RelationalExpression";"AtomicLe";"ShiftExpression"]));
   ("UsingLsShiftExpression",Just_a_concat(["ShiftExpression";"AtomicLs";"AdditiveExpression"]));
   ("UsingLtRelationalExpression",Just_a_concat(["RelationalExpression";"AtomicLt";"ShiftExpression"]));
   ("UsingMinusAdditiveExpression",Just_a_concat(["AdditiveExpression";"AtomicMinus";"MultiplicativeExpression"]));
   ("UsingMinusUnaryExpression",Just_a_concat(["AtomicMinus";"UnaryExpression"]));
   ("UsingModMultiplicativeExpression",Just_a_concat(["MultiplicativeExpression";"AtomicMod";"UnaryExpression"]));
   ("UsingNotUnaryExpressionNotPlusMinus",Just_a_concat(["AtomicNot";"UnaryExpression"]));
   ("UsingPlusAdditiveExpression",Just_a_concat(["AdditiveExpression";"AtomicPlus";"MultiplicativeExpression"]));
   ("UsingPlusUnaryExpression",Just_a_concat(["AtomicPlus";"UnaryExpression"]));
   ("UsingSrsShiftExpression",Just_a_concat(["ShiftExpression";"AtomicSrs";"AdditiveExpression"]));
   ("UsingSuperWildcardBounds",Just_a_concat(["AtomicSuper";"ReferenceType"]));
   ("UsingThisPrimaryNoNewArray",Just_a_concat(["TypeName";"MolecularDot_This"]));
   ("UsingThreeDotsRecordComponent",Just_a_concat(["StarredRecordComponentModifier";"UnannType";"StarredAnnotation";"MolecularDot_Dot_Dot_Identifier"]));
   ("UsingTimesMultiplicativeExpression",Just_a_concat(["MultiplicativeExpression";"AtomicTimes";"UnaryExpression"]));
   ("UsingUrsShiftExpression",Just_a_concat(["ShiftExpression";"AtomicUrs";"AdditiveExpression"]));
   ("UsualClassType",Just_a_concat(["PackageName";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"]));
   ("UsualParameter",Just_a_concat(["StarredVariableModifier";"UnannType";"StarredAnnotation";"MolecularDot_Dot_Dot_Identifier"]));
   ("VariableAccess",Just_a_disjunction(["ExpressionName";"FieldAccess"]));
   ("VariableDeclaratorPrecededByComma",Just_a_concat(["MolecularCm_Identifier";"OptionalDims";"OptionalEqualsVariableInitializer"]));
   ("VariableInitializer",Just_a_disjunction(["Expression";"ArrayInitializer"]));
   ("VariableInitializerList",Just_a_concat(["VariableInitializer";"StarredVariableInitializerPrecededByComma"]));
   ("VariableInitializerPrecededByComma",Just_a_concat(["AtomicCm";"VariableInitializer"]));
   ("VariableModifier",Just_a_disjunction(["Annotation";"AtomicFinal"]));
   ("WhileStatement",Just_a_concat(["MolecularWhile_Lp";"Expression";"AtomicRp";"Statement"]));
   ("WhileStatementNoShortIf",Just_a_concat(["MolecularWhile_Lp";"Expression";"AtomicRp";"StatementNoShortIf"]));
   ("Wildcard",Just_a_concat(["StarredAnnotation";"AtomicCond";"OptionalWildcardBounds"]));
   ("WildcardBounds",Just_a_disjunction(["UsingExtendsWildcardBounds";"UsingSuperWildcardBounds"]));
   ("WithColonAssertStatement",Just_a_concat(["AtomicAssert";"Expression";"AtomicColon";"Expression";"AtomicSm"]));
   ("WithEqEqEqualityExpression",Just_a_concat(["EqualityExpression";"AtomicEqEq";"RelationalExpression"]));
   ("WithNotEqEqualityExpression",Just_a_concat(["EqualityExpression";"AtomicNotEq";"RelationalExpression"]));
   ("WithoutColonAssertStatement",Just_a_concat(["AtomicAssert";"Expression";"AtomicSm"]));
   ("YieldStatement",Just_a_concat(["AtomicYield";"Expression";"AtomicSm"]));

]);;


(* Java grammar ends here *)




let modifications_to_original_java_grammar = 
    [
      (* The following four actions remove left recursions and are fully equivalent to the original *)
      (*
      Set_production("AmbiguousName",Just_a_concat["Identifier";"StarredMolecularDot_Identifier"]);
      Set_production("ModuleName",Just_a_concat["Identifier";"StarredMolecularDot_Identifier"]);
      Set_production("PackageName",Just_a_concat["Identifier";"StarredMolecularDot_Identifier"]);
      Set_production("PackageOrTypeName",Just_a_concat["Identifier";"StarredMolecularDot_Identifier"]);
      *)
      (* A consequence of the preceding four actions *) 
      Set_production("ExpressionName",Just_a_concat["Identifier";"StarredMolecularDot_Identifier"]);
      Set_production("CompoundTypeName",Just_a_concat["Identifier";"StarredMolecularDot_Identifier"]);

      Set_production("ExpressionyMethodInvocation",Just_a_concat(["Identifier";"StarredMolecularDot_Identifier";"AtomicDot";"OptionalTypeArguments";"MolecularIdentifier_Lp";"OptionalArgumentList";"AtomicRp"]));
      Set_production("ExpressionyMethodReference",Just_a_concat(["Identifier";"StarredMolecularDot_Identifier";"MolecularColon_Colon";"OptionalTypeArguments";"Identifier"]));
      Set_production("MackerelClassInstanceCreationExpression",Just_a_concat(["Identifier";"StarredMolecularDot_Identifier";"MolecularDot_New";"OptionalTypeArguments";"StarredAnnotation";"Identifier";"StarredAnnotatedIdentifierrPrecededByDot";"OptionalTypeArgumentsOrDiamond";"AtomicLp";"OptionalArgumentList";"AtomicRp";"OptionalClassBody"]));
      Set_production("SalmonExplicitConstructorInvocation",Just_a_concat(["Identifier";"StarredMolecularDot_Identifier";"AtomicDot";"OptionalTypeArguments";"MolecularSuper_Lp";"OptionalArgumentList";"MolecularRp_Sm"]));
      Set_production("ShortArrayAccess",Just_a_concat(["Identifier";"StarredMolecularDot_Identifier";"AtomicLb";"Expression";"AtomicRb"]));
      Set_production("TypeImportOnDemandDeclaration",Just_a_concat(["AtomicImport";"Identifier";"StarredMolecularDot_Identifier";"MolecularDot_Times_Sm"]));
      Set_production("UsualClassType",Just_a_concat(["Identifier";"StarredMolecularDot_Identifier";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"]));
      Set_production("MolecularImport_Identifier",Just_atomic([IMPORT_T;IDENTIFIER_T]));
      Set_production("TypeImportOnDemandDeclaration",Just_a_concat(["MolecularImport_Identifier";"StarredMolecularDot_Identifier";"MolecularDot_Times_Sm"]));


      Remove_productions( [
      "AmbiguousName"; "CompoundAmbiguousName";  
      "CompoundModuleName"; "CompoundPackageName"; "CompoundPackageOrTypeName";"ModuleName";
      "PackageName"; "PackageOrTypeName"]);
    ] ;;

let java_grammar = 
    Jvsp_abstract_language.modify original_java_grammar 
     modifications_to_original_java_grammar ;;

 end ;;


let java_grammar = Private.java_grammar ;;
