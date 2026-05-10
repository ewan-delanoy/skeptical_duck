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


type element_in_concat = Jvsp_abstract_language_t.element_in_concat = 
   Ref of string ;;


type element_in_disjunction = Jvsp_abstract_language_t.element_in_disjunction = 
    Concat of string list
   |Cancot of element_in_concat list ;;
     
type form =  Jvsp_abstract_language_t.form = 
   Disjunction of element_in_disjunction list 
   |Just_an_optional of string
   |Just_atomic of Jvsp_types.token_type list
   |Just_a_star of string ;;

type t =  Jvsp_abstract_language_t.t = AL of (string * form) list ;; 

(* Java grammar begins here *)


 let java_grammar = 

AL ([

   ("",Disjunction([]));
   ("AdditionalBound",Disjunction([Cancot([Ref("AtomicAnd");Ref("InterfaceType")])]));
   ("AdditiveExpression",Disjunction([Cancot([Ref("MultiplicativeExpression")]);Cancot([Ref("AdditiveExpression");Ref("AtomicPlus");Ref("MultiplicativeExpression")]);Cancot([Ref("AdditiveExpression");Ref("AtomicMinus");Ref("MultiplicativeExpression")])]));
   ("AmbiguousName",Disjunction([Cancot([Ref("Identifier")]);Cancot([Ref("AmbiguousName");Ref("IdentifierPrecededByDot")])]));
   ("AndExpression",Disjunction([Cancot([Ref("EqualityExpression")]);Cancot([Ref("AndExpression");Ref("AtomicAnd");Ref("EqualityExpression")])]));
   ("AnnotatedIdentifierrPrecededByDot",Disjunction([Cancot([Ref("AtomicDot");Ref("StarredAnnotation");Ref("Identifier")])]));
   ("Annotation",Disjunction([Cancot([Ref("NormalAnnotation")]);Cancot([Ref("MarkerAnnotation")]);Cancot([Ref("SingleElementAnnotation")])]));
   ("AnnotationInterfaceBody",Disjunction([Cancot([Ref("AtomicLc");Ref("StarredAnnotationInterfaceMemberDeclaration");Ref("AtomicRc")])]));
   ("AnnotationInterfaceDeclaration",Disjunction([Cancot([Ref("StarredInterfaceModifier");Ref("AtomicSnail");Ref("AtomicInterface");Ref("TypeIdentifier");Ref("AnnotationInterfaceBody")])]));
   ("AnnotationInterfaceElementDeclaration",Disjunction([Cancot([Ref("StarredAnnotationInterfaceElementModifier");Ref("UnannType");Ref("Identifier");Ref("AtomicLp");Ref("AtomicRp");Ref("OptionalDims");Ref("OptionalDefaultValue");Ref("AtomicSm")])]));
   ("AnnotationInterfaceElementModifier",Disjunction([Cancot([Ref("Annotation")]);Cancot([Ref("AtomicPublic")]);Cancot([Ref("AtomicAbstract")])]));
   ("AnnotationInterfaceMemberDeclaration",Disjunction([Cancot([Ref("AnnotationInterfaceElementDeclaration")]);Cancot([Ref("ConstantDeclaration")]);Cancot([Ref("ClassDeclaration")]);Cancot([Ref("InterfaceDeclaration")]);Cancot([Ref("AtomicSm")])]));
   ("ArgumentList",Disjunction([Cancot([Ref("Expression");Ref("StarredExpressionPrecededByComma")])]));
   ("ArrayAccess",Disjunction([Cancot([Ref("ExpressionName");Ref("AtomicLb");Ref("Expression");Ref("AtomicRb")]);Cancot([Ref("PrimaryNoNewArray");Ref("AtomicLb");Ref("Expression");Ref("AtomicRb")])]));
   ("ArrayCreationExpression",Disjunction([Cancot([Ref("AtomicNew");Ref("PrimitiveType");Ref("DimExprs");Ref("OptionalDims")]);Cancot([Ref("AtomicNew");Ref("ClassOrInterfaceType");Ref("DimExprs");Ref("OptionalDims")]);Cancot([Ref("AtomicNew");Ref("PrimitiveType");Ref("Dims");Ref("ArrayInitializer")]);Cancot([Ref("AtomicNew");Ref("ClassOrInterfaceType");Ref("Dims");Ref("ArrayInitializer")])]));
   ("ArrayInitializer",Disjunction([Cancot([Ref("AtomicLc");Ref("OptionalVariableInitializerList");Ref("OptionalCm");Ref("AtomicRc")])]));
   ("ArrayType",Disjunction([Cancot([Ref("PrimitiveType");Ref("Dims")]);Cancot([Ref("ClassOrInterfaceType");Ref("Dims")]);Cancot([Ref("TypeVariable");Ref("Dims")])]));
   ("AssertStatement",Disjunction([Cancot([Ref("AtomicAssert");Ref("Expression");Ref("AtomicSm")]);Cancot([Ref("AtomicAssert");Ref("Expression");Ref("AtomicColon");Ref("Expression");Ref("AtomicSm")])]));
   ("Assignment",Disjunction([Cancot([Ref("LeftHandSide");Ref("AtomicOperatorEq");Ref("Expression")])]));
   ("AssignmentExpression",Disjunction([Cancot([Ref("ConditionalExpression")]);Cancot([Ref("Assignment")])]));
   ("AtomicAbstract",Just_atomic([ABSTRACT_T]));
   ("AtomicAnd",Just_atomic([AND_T]));
   ("AtomicAndAnd",Just_atomic([AND_AND_T]));
   ("AtomicAssert",Just_atomic([ASSERT_T]));
   ("AtomicBoolean",Just_atomic([BOOLEAN_T]));
   ("AtomicBreak",Just_atomic([BREAK_T]));
   ("AtomicByte",Just_atomic([BYTE_T]));
   ("AtomicCase",Just_atomic([CASE_T]));
   ("AtomicCatch",Just_atomic([CATCH_T]));
   ("AtomicChar",Just_atomic([CHAR_T]));
   ("AtomicClass",Just_atomic([CLASS_T]));
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
   ("AtomicEnum",Just_atomic([ENUM_T]));
   ("AtomicEq",Just_atomic([EQ_T]));
   ("AtomicEqEq",Just_atomic([EQ_EQ_T]));
   ("AtomicExports",Just_atomic([EXPORTS_T]));
   ("AtomicExtends",Just_atomic([EXTENDS_T]));
   ("AtomicFinal",Just_atomic([FINAL_T]));
   ("AtomicFinally",Just_atomic([FINALLY_T]));
   ("AtomicFloat",Just_atomic([FLOAT_T]));
   ("AtomicFor",Just_atomic([FOR_T]));
   ("AtomicGe",Just_atomic([GE_T]));
   ("AtomicGt",Just_atomic([GT_T]));
   ("AtomicIf",Just_atomic([IF_T]));
   ("AtomicImplements",Just_atomic([IMPLEMENTS_T]));
   ("AtomicImport",Just_atomic([IMPORT_T]));
   ("AtomicIncr",Just_atomic([INCR_T]));
   ("AtomicInstanceof",Just_atomic([INSTANCEOF_T]));
   ("AtomicInt",Just_atomic([INT_T]));
   ("AtomicInterface",Just_atomic([INTERFACE_T]));
   ("AtomicLb",Just_atomic([LB_T]));
   ("AtomicLc",Just_atomic([LC_T]));
   ("AtomicLe",Just_atomic([LE_T]));
   ("AtomicLong",Just_atomic([LONG_T]));
   ("AtomicLp",Just_atomic([LP_T]));
   ("AtomicLs",Just_atomic([LS_T]));
   ("AtomicLt",Just_atomic([LT_T]));
   ("AtomicMinus",Just_atomic([MINUS_T]));
   ("AtomicMod",Just_atomic([MOD_T]));
   ("AtomicModule",Just_atomic([MODULE_T]));
   ("AtomicNative",Just_atomic([NATIVE_T]));
   ("AtomicNew",Just_atomic([NEW_T]));
   ("AtomicNonsealed",Just_atomic([NONSEALED_T]));
   ("AtomicNot",Just_atomic([NOT_T]));
   ("AtomicNotEq",Just_atomic([NOT_EQ_T]));
   ("AtomicOpens",Just_atomic([OPENS_T]));
   ("AtomicOperatorEq",Just_atomic([OPERATOR_EQ_T]));
   ("AtomicOr",Just_atomic([OR_T]));
   ("AtomicOrOr",Just_atomic([OR_OR_T]));
   ("AtomicPackage",Just_atomic([PACKAGE_T]));
   ("AtomicPermits",Just_atomic([PERMITS_T]));
   ("AtomicPlus",Just_atomic([PLUS_T]));
   ("AtomicPrivate",Just_atomic([PRIVATE_T]));
   ("AtomicProtected",Just_atomic([PROTECTED_T]));
   ("AtomicProvides",Just_atomic([PROVIDES_T]));
   ("AtomicPublic",Just_atomic([PUBLIC_T]));
   ("AtomicRb",Just_atomic([RB_T]));
   ("AtomicRc",Just_atomic([RC_T]));
   ("AtomicRecord",Just_atomic([RECORD_T]));
   ("AtomicRequires",Just_atomic([REQUIRES_T]));
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
   ("AtomicSwitch",Just_atomic([SWITCH_T]));
   ("AtomicSynchronized",Just_atomic([SYNCHRONIZED_T]));
   ("AtomicThis",Just_atomic([THIS_T]));
   ("AtomicThrow",Just_atomic([THROW_T]));
   ("AtomicThrows",Just_atomic([THROWS_T]));
   ("AtomicTimes",Just_atomic([TIMES_T]));
   ("AtomicTo",Just_atomic([TO_T]));
   ("AtomicTransient",Just_atomic([TRANSIENT_T]));
   ("AtomicTransitive",Just_atomic([TRANSITIVE_T]));
   ("AtomicTry",Just_atomic([TRY_T]));
   ("AtomicUrs",Just_atomic([URS_T]));
   ("AtomicUses",Just_atomic([USES_T]));
   ("AtomicVar",Just_atomic([VAR_T]));
   ("AtomicVoid",Just_atomic([VOID_T]));
   ("AtomicVolatile",Just_atomic([VOLATILE_T]));
   ("AtomicWhile",Just_atomic([WHILE_T]));
   ("AtomicWith",Just_atomic([WITH_T]));
   ("AtomicXor",Just_atomic([XOR_T]));
   ("AtomicYield",Just_atomic([YIELD_T]));
   ("BasicForStatement",Disjunction([Cancot([Ref("AtomicFor");Ref("AtomicLp");Ref("OptionalForInit");Ref("AtomicSm");Ref("OptionalExpression");Ref("AtomicSm");Ref("OptionalForUpdate");Ref("AtomicRp");Ref("Statement")])]));
   ("BasicForStatementNoShortIf",Disjunction([Cancot([Ref("AtomicFor");Ref("AtomicLp");Ref("OptionalForInit");Ref("AtomicSm");Ref("OptionalExpression");Ref("AtomicSm");Ref("OptionalForUpdate");Ref("AtomicRp");Ref("StatementNoShortIf")])]));
   ("Block",Disjunction([Cancot([Ref("AtomicLc");Ref("OptionalBlockStatements");Ref("AtomicRc")])]));
   ("BlockStatement",Disjunction([Cancot([Ref("LocalClassOrInterfaceDeclaration")]);Cancot([Ref("LocalVariableDeclarationStatement")]);Cancot([Ref("Statement")])]));
   ("BlockStatements",Disjunction([Cancot([Ref("BlockStatement");Ref("StarredBlockStatement")])]));
   ("BreakStatement",Disjunction([Cancot([Ref("AtomicBreak");Ref("OptionalIdentifier");Ref("AtomicSm")])]));
   ("CaseConstant",Disjunction([Cancot([Ref("ConditionalExpression")])]));
   ("CaseConstantPrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("CaseConstant")])]));
   ("CastExpression",Disjunction([Cancot([Ref("AtomicLp");Ref("PrimitiveType");Ref("AtomicRp");Ref("UnaryExpression")]);Cancot([Ref("AtomicLp");Ref("ReferenceType");Ref("StarredAdditionalBound");Ref("AtomicRp");Ref("UnaryExpressionNotPlusMinus")]);Cancot([Ref("AtomicLp");Ref("ReferenceType");Ref("StarredAdditionalBound");Ref("AtomicRp");Ref("LambdaExpression")])]));
   ("CatchClause",Disjunction([Cancot([Ref("AtomicCatch");Ref("AtomicLp");Ref("CatchFormalParameter");Ref("AtomicRp");Ref("Block")])]));
   ("Catches",Disjunction([Cancot([Ref("CatchClause");Ref("StarredCatchClause")])]));
   ("CatchFormalParameter",Disjunction([Cancot([Ref("StarredVariableModifier");Ref("CatchType");Ref("VariableDeclaratorId")])]));
   ("CatchType",Disjunction([Cancot([Ref("UnannClassType");Ref("StarredClassTypePrecededByVerticalBar")])]));
   ("ClassBody",Disjunction([Cancot([Ref("AtomicLc");Ref("StarredClassBodyDeclaration");Ref("AtomicRc")])]));
   ("ClassBodyDeclaration",Disjunction([Cancot([Ref("ClassMemberDeclaration")]);Cancot([Ref("InstanceInitializer")]);Cancot([Ref("StaticInitializer")]);Cancot([Ref("ConstructorDeclaration")])]));
   ("ClassDeclaration",Disjunction([Cancot([Ref("NormalClassDeclaration")]);Cancot([Ref("EnumDeclaration")]);Cancot([Ref("RecordDeclaration")])]));
   ("ClassExtends",Disjunction([Cancot([Ref("AtomicExtends");Ref("ClassType")])]));
   ("ClassImplements",Disjunction([Cancot([Ref("AtomicImplements");Ref("InterfaceTypeList")])]));
   ("ClassInstanceCreationExpression",Disjunction([Cancot([Ref("UnqualifiedClassInstanceCreationExpression")]);Cancot([Ref("ExpressionName");Ref("AtomicDot");Ref("UnqualifiedClassInstanceCreationExpression")]);Cancot([Ref("Primary");Ref("AtomicDot");Ref("UnqualifiedClassInstanceCreationExpression")])]));
   ("ClassLiteral",Disjunction([Cancot([Ref("TypeName");Ref("StarredOpenSquare");Ref("AtomicDot");Ref("AtomicClass")]);Cancot([Ref("NumericType");Ref("StarredOpenSquare");Ref("AtomicDot");Ref("AtomicClass")]);Cancot([Ref("AtomicBoolean");Ref("StarredOpenSquare");Ref("AtomicDot");Ref("AtomicClass")]);Cancot([Ref("AtomicVoid");Ref("AtomicDot");Ref("AtomicClass")])]));
   ("ClassMemberDeclaration",Disjunction([Cancot([Ref("FieldDeclaration")]);Cancot([Ref("MethodDeclaration")]);Cancot([Ref("ClassDeclaration")]);Cancot([Ref("InterfaceDeclaration")]);Cancot([Ref("AtomicSm")])]));
   ("ClassModifier",Disjunction([Cancot([Ref("Annotation")]);Cancot([Ref("AtomicPublic")]);Cancot([Ref("AtomicProtected")]);Cancot([Ref("AtomicPrivate")]);Cancot([Ref("AtomicAbstract")]);Cancot([Ref("AtomicStatic")]);Cancot([Ref("AtomicFinal")]);Cancot([Ref("AtomicSealed")]);Cancot([Ref("AtomicNonsealed")]);Cancot([Ref("AtomicStrictfp")])]));
   ("ClassOrInterfaceType",Disjunction([Cancot([Ref("ClassType")]);Cancot([Ref("InterfaceType")])]));
   ("ClassOrInterfaceTypeToInstantiate",Disjunction([Cancot([Ref("StarredAnnotation");Ref("Identifier");Ref("StarredAnnotatedIdentifierrPrecededByDot");Ref("OptionalTypeArgumentsOrDiamond")])]));
   ("ClassPermits",Disjunction([Cancot([Ref("AtomicPermits");Ref("TypeName");Ref("StarredTypeNamePrecededByComma")])]));
   ("ClassType",Disjunction([Cancot([Ref("StarredAnnotation");Ref("TypeIdentifier");Ref("OptionalTypeArguments")]);Cancot([Ref("PackageName");Ref("AtomicDot");Ref("StarredAnnotation");Ref("TypeIdentifier");Ref("OptionalTypeArguments")]);Cancot([Ref("ClassOrInterfaceType");Ref("AtomicDot");Ref("StarredAnnotation");Ref("TypeIdentifier");Ref("OptionalTypeArguments")])]));
   ("ClassTypePrecededByVerticalBar",Disjunction([Cancot([Ref("AtomicOr");Ref("ClassType")])]));
   ("CompactConstructorDeclaration",Disjunction([Cancot([Ref("StarredConstructorModifier");Ref("SimpleTypeName");Ref("ConstructorBody")])]));
   ("CompilationUnit",Disjunction([Cancot([Ref("OrdinaryCompilationUnit")]);Cancot([Ref("ModularCompilationUnit")])]));
   ("ConditionalAndExpression",Disjunction([Cancot([Ref("InclusiveOrExpression")]);Cancot([Ref("ConditionalAndExpression");Ref("AtomicAndAnd");Ref("InclusiveOrExpression")])]));
   ("ConditionalExpression",Disjunction([Cancot([Ref("ConditionalOrExpression")]);Cancot([Ref("ConditionalOrExpression");Ref("AtomicCond");Ref("Expression");Ref("AtomicColon");Ref("ConditionalExpression")]);Cancot([Ref("ConditionalOrExpression");Ref("AtomicCond");Ref("Expression");Ref("AtomicColon");Ref("LambdaExpression")])]));
   ("ConditionalOrExpression",Disjunction([Cancot([Ref("ConditionalAndExpression")]);Cancot([Ref("ConditionalOrExpression");Ref("AtomicOrOr");Ref("ConditionalAndExpression")])]));
   ("ConstantDeclaration",Disjunction([Cancot([Ref("StarredConstantModifier");Ref("UnannType");Ref("VariableDeclaratorList");Ref("AtomicSm")])]));
   ("ConstantExpression",Disjunction([Cancot([Ref("Expression")])]));
   ("ConstantModifier",Disjunction([Cancot([Ref("Annotation")]);Cancot([Ref("AtomicPublic")]);Cancot([Ref("AtomicStatic")]);Cancot([Ref("AtomicFinal")])]));
   ("ConstructorBody",Disjunction([Cancot([Ref("AtomicLc");Ref("OptionalExplicitConstructorInvocation");Ref("OptionalBlockStatements");Ref("AtomicRc")])]));
   ("ConstructorDeclaration",Disjunction([Cancot([Ref("StarredConstructorModifier");Ref("ConstructorDeclarator");Ref("OptionalThrows");Ref("ConstructorBody")])]));
   ("ConstructorDeclarator",Disjunction([Cancot([Ref("OptionalTypeParameters");Ref("SimpleTypeName");Ref("AtomicLp");Ref("OptionalReceiverParameterFollowedByComma");Ref("OptionalFormalParameterList");Ref("AtomicRp")])]));
   ("ConstructorModifier",Disjunction([Cancot([Ref("Annotation")]);Cancot([Ref("AtomicPublic")]);Cancot([Ref("AtomicProtected")]);Cancot([Ref("AtomicPrivate")])]));
   ("ContinueStatement",Disjunction([Cancot([Ref("AtomicContinue");Ref("OptionalIdentifier");Ref("AtomicSm")])]));
   ("DefaultValue",Disjunction([Cancot([Ref("AtomicDefault");Ref("ElementValue")])]));
   ("DimExpr",Disjunction([Cancot([Ref("StarredAnnotation");Ref("AtomicLb");Ref("Expression");Ref("AtomicRb")])]));
   ("DimExprs",Disjunction([Cancot([Ref("DimExpr");Ref("StarredDimExpr")])]));
   ("Dims",Disjunction([Cancot([Ref("DimsElement");Ref("StarredDimsElement")])]));
   ("DimsElement",Disjunction([Cancot([Ref("StarredAnnotation");Ref("AtomicLb");Ref("AtomicRb")])]));
   ("DoStatement",Disjunction([Cancot([Ref("AtomicDo");Ref("Statement");Ref("AtomicWhile");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("AtomicSm")])]));
   ("ElementValue",Disjunction([Cancot([Ref("ConditionalExpression")]);Cancot([Ref("ElementValueArrayInitializer")]);Cancot([Ref("Annotation")])]));
   ("ElementValueArrayInitializer",Disjunction([Cancot([Ref("AtomicLc");Ref("OptionalElementValueList");Ref("OptionalCm");Ref("AtomicRc")])]));
   ("ElementValueList",Disjunction([Cancot([Ref("ElementValue");Ref("StarredElementValuePrecededByComma")])]));
   ("ElementValuePair",Disjunction([Cancot([Ref("Identifier");Ref("AtomicEq");Ref("ElementValue")])]));
   ("ElementValuePairList",Disjunction([Cancot([Ref("ElementValuePair");Ref("StarredElementValuePrecededByCommaPair")])]));
   ("ElementValuePairPrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("ElementValuePair")])]));
   ("ElementValuePrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("ElementValue")])]));
   ("EmptyStatement",Disjunction([Cancot([Ref("AtomicSm")])]));
   ("EnhancedForStatement",Disjunction([Cancot([Ref("AtomicFor");Ref("AtomicLp");Ref("LocalVariableDeclaration");Ref("AtomicColon");Ref("Expression");Ref("AtomicRp");Ref("Statement")])]));
   ("EnhancedForStatementNoShortIf",Disjunction([Cancot([Ref("AtomicFor");Ref("AtomicLp");Ref("LocalVariableDeclaration");Ref("AtomicColon");Ref("Expression");Ref("AtomicRp");Ref("StatementNoShortIf")])]));
   ("EnumBody",Disjunction([Cancot([Ref("AtomicLc");Ref("OptionalEnumConstantList");Ref("OptionalCm");Ref("OptionalEnumBodyDeclarations");Ref("AtomicRc")])]));
   ("EnumBodyDeclarations",Disjunction([Cancot([Ref("AtomicSm");Ref("StarredClassBodyDeclaration")])]));
   ("EnumConstant",Disjunction([Cancot([Ref("StarredEnumConstantModifier");Ref("Identifier");Ref("OptionalParenthesedArgumentList");Ref("OptionalClassBody")])]));
   ("EnumConstantList",Disjunction([Cancot([Ref("EnumConstant");Ref("StarredEnumConstantPrecededByComma")])]));
   ("EnumConstantModifier",Disjunction([Cancot([Ref("Annotation")])]));
   ("EnumConstantPrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("EnumConstant")])]));
   ("EnumDeclaration",Disjunction([Cancot([Ref("StarredClassModifier");Ref("AtomicEnum");Ref("TypeIdentifier");Ref("OptionalClassImplements");Ref("EnumBody")])]));
   ("EqualityExpression",Disjunction([Cancot([Ref("RelationalExpression")]);Cancot([Ref("EqualityExpression");Ref("AtomicEqEq");Ref("RelationalExpression")]);Cancot([Ref("EqualityExpression");Ref("AtomicNotEq");Ref("RelationalExpression")])]));
   ("EqualsVariableInitializer",Disjunction([Cancot([Ref("AtomicEq");Ref("VariableInitializer")])]));
   ("ExceptionType",Disjunction([Cancot([Ref("ClassType")]);Cancot([Ref("TypeVariable")])]));
   ("ExceptionTypeList",Disjunction([Cancot([Ref("ExceptionType");Ref("StarredExceptionTypePrecededByComma")])]));
   ("ExceptionTypePrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("ExceptionType")])]));
   ("ExclusiveOrExpression",Disjunction([Cancot([Ref("AndExpression")]);Cancot([Ref("ExclusiveOrExpression");Ref("AtomicXor");Ref("AndExpression")])]));
   ("ExplicitConstructorInvocation",Disjunction([Cancot([Ref("OptionalTypeArguments");Ref("AtomicThis");Ref("ParenthesedArgumentList");Ref("AtomicSm")]);Cancot([Ref("OptionalTypeArguments");Ref("AtomicSuper");Ref("ParenthesedArgumentList");Ref("AtomicSm")]);Cancot([Ref("ExpressionName");Ref("AtomicDot");Ref("OptionalTypeArguments");Ref("AtomicSuper");Ref("ParenthesedArgumentList");Ref("AtomicSm")]);Cancot([Ref("Primary");Ref("AtomicDot");Ref("OptionalTypeArguments");Ref("AtomicSuper");Ref("ParenthesedArgumentList");Ref("AtomicSm")])]));
   ("Expression",Disjunction([Cancot([Ref("LambdaExpression")]);Cancot([Ref("AssignmentExpression")])]));
   ("ExpressionName",Disjunction([Cancot([Ref("Identifier")]);Cancot([Ref("AmbiguousName");Ref("IdentifierPrecededByDot")])]));
   ("ExpressionPrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("Expression")])]));
   ("ExpressionStatement",Disjunction([Cancot([Ref("StatementExpression");Ref("AtomicSm")])]));
   ("FieldAccess",Disjunction([Cancot([Ref("Primary");Ref("IdentifierPrecededByDot")]);Cancot([Ref("AtomicSuper");Ref("IdentifierPrecededByDot")]);Cancot([Ref("TypeName");Ref("AtomicDot");Ref("AtomicSuper");Ref("IdentifierPrecededByDot")])]));
   ("FieldDeclaration",Disjunction([Cancot([Ref("StarredFieldModifier");Ref("UnannType");Ref("VariableDeclaratorList");Ref("AtomicSm")])]));
   ("FieldModifier",Disjunction([Cancot([Ref("Annotation")]);Cancot([Ref("AtomicPublic")]);Cancot([Ref("AtomicProtected")]);Cancot([Ref("AtomicPrivate")]);Cancot([Ref("AtomicStatic")]);Cancot([Ref("AtomicFinal")]);Cancot([Ref("AtomicTransient")]);Cancot([Ref("AtomicVolatile")])]));
   ("Finally",Disjunction([Cancot([Ref("AtomicFinally");Ref("Block")])]));
   ("FloatingPointType",Disjunction([Cancot([Ref("AtomicFloat")]);Cancot([Ref("AtomicDouble")])]));
   ("ForInit",Disjunction([Cancot([Ref("StatementExpressionList")]);Cancot([Ref("LocalVariableDeclaration")])]));
   ("FormalParameter",Disjunction([Cancot([Ref("StarredVariableModifier");Ref("UnannType");Ref("VariableDeclaratorId")]);Cancot([Ref("VariableArityParameter")])]));
   ("FormalParameterList",Disjunction([Cancot([Ref("FormalParameter");Ref("StarredFormalParameterPrecededByComma")])]));
   ("FormalParameterPrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("FormalParameter")])]));
   ("ForStatement",Disjunction([Cancot([Ref("BasicForStatement")]);Cancot([Ref("EnhancedForStatement")])]));
   ("ForStatementNoShortIf",Disjunction([Cancot([Ref("BasicForStatementNoShortIf")]);Cancot([Ref("EnhancedForStatementNoShortIf")])]));
   ("ForUpdate",Disjunction([Cancot([Ref("StatementExpressionList")])]));
   ("IdentifierFollowedByDot",Disjunction([Cancot([Ref("Identifier");Ref("AtomicDot")])]));
   ("IdentifierPrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("Identifier")])]));
   ("IdentifierPrecededByDot",Disjunction([Cancot([Ref("AtomicDot");Ref("Identifier")])]));
   ("IfThenElseStatement",Disjunction([Cancot([Ref("AtomicIf");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("StatementNoShortIf");Ref("AtomicElse");Ref("Statement")])]));
   ("IfThenElseStatementNoShortIf",Disjunction([Cancot([Ref("AtomicIf");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("StatementNoShortIf");Ref("AtomicElse");Ref("StatementNoShortIf")])]));
   ("IfThenStatement",Disjunction([Cancot([Ref("AtomicIf");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("Statement")])]));
   ("ImportDeclaration",Disjunction([Cancot([Ref("SingleTypeImportDeclaration")]);Cancot([Ref("TypeImportOnDemandDeclaration")]);Cancot([Ref("SingleStaticImportDeclaration")]);Cancot([Ref("StaticImportOnDemandDeclaration")])]));
   ("InclusiveOrExpression",Disjunction([Cancot([Ref("ExclusiveOrExpression")]);Cancot([Ref("InclusiveOrExpression");Ref("AtomicOr");Ref("ExclusiveOrExpression")])]));
   ("InstanceInitializer",Disjunction([Cancot([Ref("Block")])]));
   ("InstanceofExpression",Disjunction([Cancot([Ref("RelationalExpression");Ref("AtomicInstanceof");Ref("ReferenceType")]);Cancot([Ref("RelationalExpression");Ref("AtomicInstanceof");Ref("Pattern")])]));
   ("IntegralType",Disjunction([Cancot([Ref("AtomicByte")]);Cancot([Ref("AtomicShort")]);Cancot([Ref("AtomicInt")]);Cancot([Ref("AtomicLong")]);Cancot([Ref("AtomicChar")])]));
   ("InterfaceBody",Disjunction([Cancot([Ref("AtomicLc");Ref("StarredInterfaceMemberDeclaration");Ref("AtomicRc")])]));
   ("InterfaceDeclaration",Disjunction([Cancot([Ref("NormalInterfaceDeclaration")]);Cancot([Ref("AnnotationInterfaceDeclaration")])]));
   ("InterfaceExtends",Disjunction([Cancot([Ref("AtomicExtends");Ref("InterfaceTypeList")])]));
   ("InterfaceMemberDeclaration",Disjunction([Cancot([Ref("ConstantDeclaration")]);Cancot([Ref("InterfaceMethodDeclaration")]);Cancot([Ref("ClassDeclaration")]);Cancot([Ref("InterfaceDeclaration")]);Cancot([Ref("AtomicSm")])]));
   ("InterfaceMethodDeclaration",Disjunction([Cancot([Ref("StarredInterfaceMethodModifier");Ref("MethodHeader");Ref("MethodBody")])]));
   ("InterfaceMethodModifier",Disjunction([Cancot([Ref("Annotation")]);Cancot([Ref("AtomicPublic")]);Cancot([Ref("AtomicPrivate")]);Cancot([Ref("AtomicAbstract")]);Cancot([Ref("AtomicDefault")]);Cancot([Ref("AtomicStatic")]);Cancot([Ref("AtomicStrictfp")])]));
   ("InterfaceModifier",Disjunction([Cancot([Ref("Annotation")]);Cancot([Ref("AtomicPublic")]);Cancot([Ref("AtomicProtected")]);Cancot([Ref("AtomicPrivate")]);Cancot([Ref("AtomicAbstract")]);Cancot([Ref("AtomicStatic")]);Cancot([Ref("AtomicSealed")]);Cancot([Ref("AtomicNonsealed")]);Cancot([Ref("AtomicStrictfp")])]));
   ("InterfacePermits",Disjunction([Cancot([Ref("AtomicPermits");Ref("TypeName");Ref("StarredTypeNamePrecededByComma")])]));
   ("InterfaceType",Disjunction([Cancot([Ref("ClassType")])]));
   ("InterfaceTypeList",Disjunction([Cancot([Ref("InterfaceType");Ref("StarredInterfaceTypePrecededByComma")])]));
   ("InterfaceTypePrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("InterfaceType")])]));
   ("LabeledStatement",Disjunction([Cancot([Ref("Identifier");Ref("AtomicColon");Ref("Statement")])]));
   ("LabeledStatementNoShortIf",Disjunction([Cancot([Ref("Identifier");Ref("AtomicColon");Ref("StatementNoShortIf")])]));
   ("LambdaBody",Disjunction([Cancot([Ref("Expression")]);Cancot([Ref("Block")])]));
   ("LambdaExpression",Disjunction([Cancot([Ref("LambdaParameters");Ref("AtomicMinus");Ref("AtomicGt");Ref("LambdaBody")])]));
   ("LambdaParameter",Disjunction([Cancot([Ref("StarredVariableModifier");Ref("LambdaParameterType");Ref("VariableDeclaratorId")]);Cancot([Ref("VariableArityParameter")])]));
   ("LambdaParameterList",Disjunction([Cancot([Ref("LambdaParameter");Ref("StarredLambdaParameterPrecededByComma")]);Cancot([Ref("Identifier");Ref("StarredIdentifierPrecededByComma")])]));
   ("LambdaParameterPrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("LambdaParameter")])]));
   ("LambdaParameters",Disjunction([Cancot([Ref("AtomicLp");Ref("OptionalLambdaParameterList");Ref("AtomicRp")]);Cancot([Ref("Identifier")])]));
   ("LambdaParameterType",Disjunction([Cancot([Ref("UnannType")]);Cancot([Ref("AtomicVar")])]));
   ("LeftHandSide",Disjunction([Cancot([Ref("ExpressionName")]);Cancot([Ref("FieldAccess")]);Cancot([Ref("ArrayAccess")])]));
   ("Literal",Disjunction([Cancot([Ref("IntegerLiteral")]);Cancot([Ref("FloatingPointLiteral")]);Cancot([Ref("BooleanLiteral")]);Cancot([Ref("CharacterLiteral")]);Cancot([Ref("StringLiteral")]);Cancot([Ref("TextBlock")]);Cancot([Ref("NullLiteral")])]));
   ("LocalClassOrInterfaceDeclaration",Disjunction([Cancot([Ref("ClassDeclaration")]);Cancot([Ref("NormalInterfaceDeclaration")])]));
   ("LocalVariableDeclaration",Disjunction([Cancot([Ref("StarredVariableModifier");Ref("LocalVariableType");Ref("VariableDeclaratorList")])]));
   ("LocalVariableDeclarationStatement",Disjunction([Cancot([Ref("LocalVariableDeclaration");Ref("AtomicSm")])]));
   ("LocalVariableType",Disjunction([Cancot([Ref("UnannType")]);Cancot([Ref("AtomicVar")])]));
   ("MarkerAnnotation",Disjunction([Cancot([Ref("AtomicSnail");Ref("TypeName")])]));
   ("MethodBody",Disjunction([Cancot([Ref("Block")]);Cancot([Ref("AtomicSm")])]));
   ("MethodDeclaration",Disjunction([Cancot([Ref("StarredMethodModifier");Ref("MethodHeader");Ref("MethodBody")])]));
   ("MethodDeclarator",Disjunction([Cancot([Ref("Identifier");Ref("AtomicLp");Ref("OptionalReceiverParameterFollowedByComma");Ref("OptionalFormalParameterList");Ref("AtomicRp");Ref("OptionalDims")])]));
   ("MethodHeader",Disjunction([Cancot([Ref("Result");Ref("MethodDeclarator");Ref("OptionalThrows")]);Cancot([Ref("TypeParameters");Ref("StarredAnnotation");Ref("Result");Ref("MethodDeclarator");Ref("OptionalThrows")])]));
   ("MethodInvocation",Disjunction([Cancot([Ref("MethodName");Ref("ParenthesedArgumentList")]);Cancot([Ref("TypeName");Ref("AtomicDot");Ref("OptionalTypeArguments");Ref("Identifier");Ref("ParenthesedArgumentList")]);Cancot([Ref("ExpressionName");Ref("AtomicDot");Ref("OptionalTypeArguments");Ref("Identifier");Ref("ParenthesedArgumentList")]);Cancot([Ref("Primary");Ref("AtomicDot");Ref("OptionalTypeArguments");Ref("Identifier");Ref("ParenthesedArgumentList")]);Cancot([Ref("AtomicSuper");Ref("AtomicDot");Ref("OptionalTypeArguments");Ref("Identifier");Ref("ParenthesedArgumentList")]);Cancot([Ref("TypeName");Ref("AtomicDot");Ref("AtomicSuper");Ref("AtomicDot");Ref("OptionalTypeArguments");Ref("Identifier");Ref("ParenthesedArgumentList")])]));
   ("MethodModifier",Disjunction([Cancot([Ref("Annotation")]);Cancot([Ref("AtomicPublic")]);Cancot([Ref("AtomicProtected")]);Cancot([Ref("AtomicPrivate")]);Cancot([Ref("AtomicAbstract")]);Cancot([Ref("AtomicStatic")]);Cancot([Ref("AtomicFinal")]);Cancot([Ref("AtomicSynchronized")]);Cancot([Ref("AtomicNative")]);Cancot([Ref("AtomicStrictfp")])]));
   ("MethodName",Disjunction([Cancot([Ref("UnqualifiedMethodIdentifier")])]));
   ("MethodReference",Disjunction([Cancot([Ref("ExpressionName");Ref("AtomicColon");Ref("AtomicColon");Ref("OptionalTypeArguments");Ref("Identifier")]);Cancot([Ref("Primary");Ref("AtomicColon");Ref("AtomicColon");Ref("OptionalTypeArguments");Ref("Identifier")]);Cancot([Ref("ReferenceType");Ref("AtomicColon");Ref("AtomicColon");Ref("OptionalTypeArguments");Ref("Identifier")]);Cancot([Ref("AtomicSuper");Ref("AtomicColon");Ref("AtomicColon");Ref("OptionalTypeArguments");Ref("Identifier")]);Cancot([Ref("TypeName");Ref("AtomicDot");Ref("AtomicSuper");Ref("AtomicColon");Ref("AtomicColon");Ref("OptionalTypeArguments");Ref("Identifier")]);Cancot([Ref("ClassType");Ref("AtomicColon");Ref("AtomicColon");Ref("OptionalTypeArguments");Ref("AtomicNew")]);Cancot([Ref("ArrayType");Ref("AtomicColon");Ref("AtomicColon");Ref("AtomicNew")])]));
   ("ModularCompilationUnit",Disjunction([Cancot([Ref("StarredImportDeclaration");Ref("ModuleDeclaration")])]));
   ("ModuleDeclaration",Disjunction([Cancot([Ref("StarredAnnotation");Ref("Optionalopen");Ref("AtomicModule");Ref("Identifier");Ref("StarredIdentifierPrecededByDot");Ref("AtomicLc");Ref("StarredModuleDirective");Ref("AtomicRc")])]));
   ("ModuleDirective",Disjunction([Cancot([Ref("AtomicRequires");Ref("StarredRequiresModifier");Ref("ModuleName");Ref("AtomicSm")]);Cancot([Ref("AtomicExports");Ref("PackageName");Ref("OptionalToModuleList");Ref("AtomicSm")]);Cancot([Ref("AtomicOpens");Ref("PackageName");Ref("OptionalToModuleList");Ref("AtomicSm")]);Cancot([Ref("AtomicUses");Ref("TypeName");Ref("AtomicSm")]);Cancot([Ref("AtomicProvides");Ref("TypeName");Ref("AtomicWith");Ref("TypeName");Ref("StarredTypeNamePrecededByComma");Ref("AtomicSm")])]));
   ("ModuleName",Disjunction([Cancot([Ref("Identifier")]);Cancot([Ref("ModuleName");Ref("IdentifierPrecededByDot")])]));
   ("ModuleNamePrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("ModuleName")])]));
   ("MultiplicativeExpression",Disjunction([Cancot([Ref("UnaryExpression")]);Cancot([Ref("MultiplicativeExpression");Ref("AtomicTimes");Ref("UnaryExpression")]);Cancot([Ref("MultiplicativeExpression");Ref("AtomicDiv");Ref("UnaryExpression")]);Cancot([Ref("MultiplicativeExpression");Ref("AtomicMod");Ref("UnaryExpression")])]));
   ("NormalAnnotation",Disjunction([Cancot([Ref("AtomicSnail");Ref("TypeName");Ref("AtomicLp");Ref("OptionalElementValuePairList");Ref("AtomicRp")])]));
   ("NormalClassDeclaration",Disjunction([Cancot([Ref("StarredClassModifier");Ref("AtomicClass");Ref("TypeIdentifier");Ref("OptionalTypeParameters");Ref("OptionalClassExtends");Ref("OptionalClassImplements");Ref("OptionalClassPermits");Ref("ClassBody")])]));
   ("NormalInterfaceDeclaration",Disjunction([Cancot([Ref("StarredInterfaceModifier");Ref("AtomicInterface");Ref("TypeIdentifier");Ref("OptionalTypeParameters");Ref("OptionalInterfaceExtends");Ref("OptionalInterfacePermits");Ref("InterfaceBody")])]));
   ("NumericType",Disjunction([Cancot([Ref("IntegralType")]);Cancot([Ref("FloatingPointType")])]));
   ("OpenSquare",Disjunction([Cancot([Ref("AtomicLb");Ref("AtomicRb")])]));
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
   ("Optionalopen",Just_an_optional("open"));
   ("OptionalPackageDeclaration",Just_an_optional("PackageDeclaration"));
   ("OptionalParenthesedArgumentList",Just_an_optional("ParenthesedArgumentList"));
   ("OptionalReceiverParameterFollowedByComma",Just_an_optional("ReceiverParameterFollowedByComma"));
   ("OptionalRecordComponentList",Just_an_optional("RecordComponentList"));
   ("OptionalThrows",Just_an_optional("Throws"));
   ("OptionalToModuleList",Just_an_optional("ToModuleList"));
   ("OptionalTypeArguments",Just_an_optional("TypeArguments"));
   ("OptionalTypeArgumentsOrDiamond",Just_an_optional("TypeArgumentsOrDiamond"));
   ("OptionalTypeBound",Just_an_optional("TypeBound"));
   ("OptionalTypeParameters",Just_an_optional("TypeParameters"));
   ("OptionalVariableInitializerList",Just_an_optional("VariableInitializerList"));
   ("OptionalWildcardBounds",Just_an_optional("WildcardBounds"));
   ("OrdinaryCompilationUnit",Disjunction([Cancot([Ref("OptionalPackageDeclaration");Ref("StarredImportDeclaration");Ref("StarredTopLevelClassOrInterfaceDeclaration")])]));
   ("PackageDeclaration",Disjunction([Cancot([Ref("StarredPackageModifier");Ref("AtomicPackage");Ref("Identifier");Ref("StarredIdentifierPrecededByDot");Ref("AtomicSm")])]));
   ("PackageModifier",Disjunction([Cancot([Ref("Annotation")])]));
   ("PackageName",Disjunction([Cancot([Ref("Identifier")]);Cancot([Ref("PackageName");Ref("IdentifierPrecededByDot")])]));
   ("PackageOrTypeName",Disjunction([Cancot([Ref("Identifier")]);Cancot([Ref("PackageOrTypeName");Ref("IdentifierPrecededByDot")])]));
   ("ParenthesedArgumentList",Disjunction([Cancot([Ref("AtomicLp");Ref("OptionalArgumentList");Ref("AtomicRp")])]));
   ("Pattern",Disjunction([Cancot([Ref("TypePattern")])]));
   ("PostDecrementExpression",Disjunction([Cancot([Ref("PostfixExpression");Ref("AtomicDecr")])]));
   ("PostfixExpression",Disjunction([Cancot([Ref("Primary")]);Cancot([Ref("ExpressionName")]);Cancot([Ref("PostIncrementExpression")]);Cancot([Ref("PostDecrementExpression")])]));
   ("PostIncrementExpression",Disjunction([Cancot([Ref("PostfixExpression");Ref("AtomicIncr")])]));
   ("PreDecrementExpression",Disjunction([Cancot([Ref("AtomicDecr");Ref("UnaryExpression")])]));
   ("PreIncrementExpression",Disjunction([Cancot([Ref("AtomicIncr");Ref("UnaryExpression")])]));
   ("Primary",Disjunction([Cancot([Ref("PrimaryNoNewArray")]);Cancot([Ref("ArrayCreationExpression")])]));
   ("PrimaryNoNewArray",Disjunction([Cancot([Ref("Literal")]);Cancot([Ref("ClassLiteral")]);Cancot([Ref("AtomicThis")]);Cancot([Ref("TypeName");Ref("AtomicDot");Ref("AtomicThis")]);Cancot([Ref("AtomicLp");Ref("Expression");Ref("AtomicRp")]);Cancot([Ref("ClassInstanceCreationExpression")]);Cancot([Ref("FieldAccess")]);Cancot([Ref("ArrayAccess")]);Cancot([Ref("MethodInvocation")]);Cancot([Ref("Methodref_in_diserence")])]));
   ("PrimitiveType",Disjunction([Cancot([Ref("StarredAnnotation");Ref("NumericType")]);Cancot([Ref("StarredAnnotation");Ref("AtomicBoolean")])]));
   ("ReceiverParameter",Disjunction([Cancot([Ref("StarredAnnotation");Ref("UnannType");Ref("OptionalIdentifierFollowedByDot");Ref("AtomicThis")])]));
   ("ReceiverParameterFollowedByComma",Disjunction([Cancot([Ref("ReceiverParameter");Ref("AtomicCm")])]));
   ("RecordBody",Disjunction([Cancot([Ref("AtomicLc");Ref("StarredRecordBodyDeclaration");Ref("AtomicRc")])]));
   ("RecordBodyDeclaration",Disjunction([Cancot([Ref("ClassBodyDeclaration")]);Cancot([Ref("CompactConstructorDeclaration")])]));
   ("RecordComponent",Disjunction([Cancot([Ref("StarredRecordComponentModifier");Ref("UnannType");Ref("Identifier")]);Cancot([Ref("VariableArityRecordComponent")])]));
   ("RecordComponentList",Disjunction([Cancot([Ref("RecordComponent");Ref("StarredRecordComponentPrecededByComma")])]));
   ("RecordComponentModifier",Disjunction([Cancot([Ref("Annotation")])]));
   ("RecordComponentPrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("RecordComponent")])]));
   ("RecordDeclaration",Disjunction([Cancot([Ref("StarredClassModifier");Ref("AtomicRecord");Ref("TypeIdentifier");Ref("OptionalTypeParameters");Ref("RecordHeader");Ref("OptionalClassImplements");Ref("RecordBody")])]));
   ("RecordHeader",Disjunction([Cancot([Ref("AtomicLp");Ref("OptionalRecordComponentList");Ref("AtomicRp")])]));
   ("ReferenceType",Disjunction([Cancot([Ref("ClassOrInterfaceType")]);Cancot([Ref("TypeVariable")]);Cancot([Ref("ArrayType")])]));
   ("RelationalExpression",Disjunction([Cancot([Ref("ShiftExpression")]);Cancot([Ref("RelationalExpression");Ref("AtomicLt");Ref("ShiftExpression")]);Cancot([Ref("RelationalExpression");Ref("AtomicGt");Ref("ShiftExpression")]);Cancot([Ref("RelationalExpression");Ref("AtomicLe");Ref("ShiftExpression")]);Cancot([Ref("RelationalExpression");Ref("AtomicGe");Ref("ShiftExpression")]);Cancot([Ref("InstanceofExpression")])]));
   ("RequiresModifier",Disjunction([Cancot([Ref("AtomicTransitive")]);Cancot([Ref("AtomicStatic")])]));
   ("Resource",Disjunction([Cancot([Ref("LocalVariableDeclaration")]);Cancot([Ref("VariableAccess")])]));
   ("ResourceList",Disjunction([Cancot([Ref("Resource");Ref("StarredResourcePrecededBySemiColon")])]));
   ("ResourcePrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("Resource")])]));
   ("ResourcePrecededBySemiColon",Disjunction([Cancot([Ref("AtomicSm");Ref("Resource")])]));
   ("ResourceSpecification",Disjunction([Cancot([Ref("AtomicLp");Ref("ResourceList");Ref("AtomicSm");Ref("AtomicRp")])]));
   ("Result",Disjunction([Cancot([Ref("UnannType")]);Cancot([Ref("AtomicVoid")])]));
   ("ReturnStatement",Disjunction([Cancot([Ref("AtomicReturn");Ref("OptionalExpression");Ref("AtomicSm")])]));
   ("ShiftExpression",Disjunction([Cancot([Ref("AdditiveExpression")]);Cancot([Ref("ShiftExpression");Ref("AtomicLs");Ref("AdditiveExpression")]);Cancot([Ref("ShiftExpression");Ref("AtomicSrs");Ref("AdditiveExpression")]);Cancot([Ref("ShiftExpression");Ref("AtomicUrs");Ref("AdditiveExpression")])]));
   ("SimpleTypeName",Disjunction([Cancot([Ref("TypeIdentifier")])]));
   ("SingleElementAnnotation",Disjunction([Cancot([Ref("AtomicSnail");Ref("TypeName");Ref("AtomicLp");Ref("ElementValue");Ref("AtomicRp")])]));
   ("SingleStaticImportDeclaration",Disjunction([Cancot([Ref("AtomicImport");Ref("AtomicStatic");Ref("TypeName");Ref("IdentifierPrecededByDot");Ref("AtomicSm")])]));
   ("SingleTypeImportDeclaration",Disjunction([Cancot([Ref("AtomicImport");Ref("TypeName");Ref("AtomicSm")])]));
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
   ("StarredElementValuePrecededByComma",Just_a_star("ElementValuePrecededByComma"));
   ("StarredElementValuePrecededByCommaPair",Just_a_star("ElementValuePrecededByCommaPair"));
   ("StarredEnumConstantModifier",Just_a_star("EnumConstantModifier"));
   ("StarredEnumConstantPrecededByComma",Just_a_star("EnumConstantPrecededByComma"));
   ("StarredExceptionTypePrecededByComma",Just_a_star("ExceptionTypePrecededByComma"));
   ("StarredExpressionPrecededByComma",Just_a_star("ExpressionPrecededByComma"));
   ("StarredFieldModifier",Just_a_star("FieldModifier"));
   ("StarredFormalParameterPrecededByComma",Just_a_star("FormalParameterPrecededByComma"));
   ("StarredIdentifierPrecededByComma",Just_a_star("IdentifierPrecededByComma"));
   ("StarredIdentifierPrecededByDot",Just_a_star("IdentifierPrecededByDot"));
   ("StarredImportDeclaration",Just_a_star("ImportDeclaration"));
   ("StarredInterfaceMemberDeclaration",Just_a_star("InterfaceMemberDeclaration"));
   ("StarredInterfaceMethodModifier",Just_a_star("InterfaceMethodModifier"));
   ("StarredInterfaceModifier",Just_a_star("InterfaceModifier"));
   ("StarredInterfaceTypePrecededByComma",Just_a_star("InterfaceTypePrecededByComma"));
   ("StarredLambdaParameterPrecededByComma",Just_a_star("LambdaParameterPrecededByComma"));
   ("StarredMethodModifier",Just_a_star("MethodModifier"));
   ("StarredModuleDirective",Just_a_star("ModuleDirective"));
   ("StarredModuleNamePrecededByComma",Just_a_star("ModuleNamePrecededByComma"));
   ("StarredOpenSquare",Just_a_star("OpenSquare"));
   ("StarredPackageModifier",Just_a_star("PackageModifier"));
   ("StarredRecordBodyDeclaration",Just_a_star("RecordBodyDeclaration"));
   ("StarredRecordComponentModifier",Just_a_star("RecordComponentModifier"));
   ("StarredRecordComponentPrecededByComma",Just_a_star("RecordComponentPrecededByComma"));
   ("StarredRequiresModifier",Just_a_star("RequiresModifier"));
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
   ("Statement",Disjunction([Cancot([Ref("StatementWithoutTrailingSubstatement")]);Cancot([Ref("LabeledStatement")]);Cancot([Ref("IfThenStatement")]);Cancot([Ref("IfThenElseStatement")]);Cancot([Ref("WhileStatement")]);Cancot([Ref("ForStatement")])]));
   ("StatementExpression",Disjunction([Cancot([Ref("Assignment")]);Cancot([Ref("PreIncrementExpression")]);Cancot([Ref("PreDecrementExpression")]);Cancot([Ref("PostIncrementExpression")]);Cancot([Ref("PostDecrementExpression")]);Cancot([Ref("MethodInvocation")]);Cancot([Ref("ClassInstanceCreationExpression")])]));
   ("StatementExpressionList",Disjunction([Cancot([Ref("StatementExpression");Ref("StarredStatementExpressionPrecededByComma")])]));
   ("StatementExpressionPrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("StatementExpression")])]));
   ("StatementNoShortIf",Disjunction([Cancot([Ref("StatementWithoutTrailingSubstatement")]);Cancot([Ref("LabeledStatementNoShortIf")]);Cancot([Ref("IfThenElseStatementNoShortIf")]);Cancot([Ref("WhileStatementNoShortIf")]);Cancot([Ref("ForStatementNoShortIf")])]));
   ("StatementWithoutTrailingSubstatement",Disjunction([Cancot([Ref("Block")]);Cancot([Ref("EmptyStatement")]);Cancot([Ref("ExpressionStatement")]);Cancot([Ref("AssertStatement")]);Cancot([Ref("SwitchStatement")]);Cancot([Ref("DoStatement")]);Cancot([Ref("BreakStatement")]);Cancot([Ref("ContinueStatement")]);Cancot([Ref("ReturnStatement")]);Cancot([Ref("SynchronizedStatement")]);Cancot([Ref("ThrowStatement")]);Cancot([Ref("TryStatement")]);Cancot([Ref("YieldStatement")])]));
   ("StaticImportOnDemandDeclaration",Disjunction([Cancot([Ref("AtomicImport");Ref("AtomicStatic");Ref("TypeName");Ref("AtomicDot");Ref("AtomicTimes");Ref("AtomicSm")])]));
   ("StaticInitializer",Disjunction([Cancot([Ref("AtomicStatic");Ref("Block")])]));
   ("SwitchBlock",Disjunction([Cancot([Ref("AtomicLc");Ref("SwitchRule");Ref("StarredSwitchRule");Ref("AtomicRc")]);Cancot([Ref("AtomicLc");Ref("StarredSwitchBlockStatementGroup");Ref("StarredSwitchLabelFollowedByColon");Ref("AtomicRc")])]));
   ("SwitchBlockStatementGroup",Disjunction([Cancot([Ref("SwitchLabelFollowedByColon");Ref("StarredSwitchLabelFollowedByColon");Ref("BlockStatements")])]));
   ("SwitchExpression",Disjunction([Cancot([Ref("AtomicSwitch");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("SwitchBlock")])]));
   ("SwitchLabel",Disjunction([Cancot([Ref("AtomicCase");Ref("CaseConstant");Ref("StarredCaseConstantPrecededByComma")]);Cancot([Ref("AtomicDefault")])]));
   ("SwitchLabelFollowedByColon",Disjunction([Cancot([Ref("SwitchLabel");Ref("AtomicColon")])]));
   ("SwitchRule",Disjunction([Cancot([Ref("SwitchLabel");Ref("AtomicMinus");Ref("AtomicGt");Ref("Expression");Ref("AtomicSm")]);Cancot([Ref("SwitchLabel");Ref("AtomicMinus");Ref("AtomicGt");Ref("Block")]);Cancot([Ref("SwitchLabel");Ref("AtomicMinus");Ref("AtomicGt");Ref("ThrowStatement")])]));
   ("SwitchStatement",Disjunction([Cancot([Ref("AtomicSwitch");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("SwitchBlock")])]));
   ("SynchronizedStatement",Disjunction([Cancot([Ref("AtomicSynchronized");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("Block")])]));
   ("Throws",Disjunction([Cancot([Ref("AtomicThrows");Ref("ExceptionTypeList")])]));
   ("ThrowStatement",Disjunction([Cancot([Ref("AtomicThrow");Ref("Expression");Ref("AtomicSm")])]));
   ("ToModuleList",Disjunction([Cancot([Ref("AtomicTo");Ref("ModuleName");Ref("StarredModuleNamePrecededByComma")])]));
   ("TopLevelClassOrInterfaceDeclaration",Disjunction([Cancot([Ref("ClassDeclaration")]);Cancot([Ref("InterfaceDeclaration")]);Cancot([Ref("AtomicSm")])]));
   ("TryStatement",Disjunction([Cancot([Ref("AtomicTry");Ref("Block");Ref("Catches")]);Cancot([Ref("AtomicTry");Ref("Block");Ref("OptionalCatches");Ref("Finally")]);Cancot([Ref("TryWithResourcesStatement")])]));
   ("TryWithResourcesStatement",Disjunction([Cancot([Ref("AtomicTry");Ref("ResourceSpecification");Ref("Block");Ref("OptionalCatches");Ref("OptionalFinally")])]));
   ("Type",Disjunction([Cancot([Ref("PrimitiveType")]);Cancot([Ref("ref_in_diserenceType")])]));
   ("TypeArgument",Disjunction([Cancot([Ref("ref_in_diserenceType")]);Cancot([Ref("Wildcard")])]));
   ("TypeArgumentList",Disjunction([Cancot([Ref("TypeArgument");Ref("StarredTypeArgumentPrecededByComma")])]));
   ("TypeArgumentPrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("TypeArgument")])]));
   ("TypeArguments",Disjunction([Cancot([Ref("AtomicLt");Ref("TypeArgumentList");Ref("AtomicGt")])]));
   ("TypeArgumentsOrDiamond",Disjunction([Cancot([Ref("TypeArguments")]);Cancot([Ref("AtomicLt");Ref("AtomicGt")])]));
   ("TypeBound",Disjunction([Cancot([Ref("AtomicExtends");Ref("TypeVariable")]);Cancot([Ref("AtomicExtends");Ref("ClassOrInterfaceType");Ref("StarredAdditionalBound")])]));
   ("TypeImportOnDemandDeclaration",Disjunction([Cancot([Ref("AtomicImport");Ref("PackageOrTypeName");Ref("AtomicDot");Ref("AtomicTimes");Ref("AtomicSm")])]));
   ("TypeName",Disjunction([Cancot([Ref("TypeIdentifier")]);Cancot([Ref("PackageOrTypeName");Ref("AtomicDot");Ref("TypeIdentifier")])]));
   ("TypeNamePrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("TypeName")])]));
   ("TypeParameter",Disjunction([Cancot([Ref("StarredTypeParameterModifier");Ref("TypeIdentifier");Ref("OptionalTypeBound")])]));
   ("TypeParameterList",Disjunction([Cancot([Ref("TypeParameter");Ref("StarredTypeParameterPrecededByComma")])]));
   ("TypeParameterModifier",Disjunction([Cancot([Ref("Annotation")])]));
   ("TypeParameterPrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("TypeParameter")])]));
   ("TypeParameters",Disjunction([Cancot([Ref("AtomicLt");Ref("TypeParameterList");Ref("AtomicGt")])]));
   ("TypePattern",Disjunction([Cancot([Ref("LocalVariableDeclaration")])]));
   ("TypeVariable",Disjunction([Cancot([Ref("StarredAnnotation");Ref("TypeIdentifier")])]));
   ("UnannArrayType",Disjunction([Cancot([Ref("UnannPrimitiveType");Ref("Dims")]);Cancot([Ref("UnannClassOrInterfaceType");Ref("Dims")]);Cancot([Ref("UnannTypeVariable");Ref("Dims")])]));
   ("UnannClassOrInterfaceType",Disjunction([Cancot([Ref("UnannClassType")]);Cancot([Ref("UnannInterfaceType")])]));
   ("UnannClassType",Disjunction([Cancot([Ref("TypeIdentifier");Ref("OptionalTypeArguments")]);Cancot([Ref("PackageName");Ref("AtomicDot");Ref("StarredAnnotation");Ref("TypeIdentifier");Ref("OptionalTypeArguments")]);Cancot([Ref("UnannClassOrInterfaceType");Ref("AtomicDot");Ref("StarredAnnotation");Ref("TypeIdentifier");Ref("OptionalTypeArguments")])]));
   ("UnannInterfaceType",Disjunction([Cancot([Ref("UnannClassType")])]));
   ("UnannPrimitiveType",Disjunction([Cancot([Ref("NumericType")]);Cancot([Ref("AtomicBoolean")])]));
   ("UnannReferenceType",Disjunction([Cancot([Ref("UnannClassOrInterfaceType")]);Cancot([Ref("UnannTypeVariable")]);Cancot([Ref("UnannArrayType")])]));
   ("UnannType",Disjunction([Cancot([Ref("UnannPrimitiveType")]);Cancot([Ref("Unannref_in_diserenceType")])]));
   ("UnannTypeVariable",Disjunction([Cancot([Ref("TypeIdentifier")])]));
   ("UnaryExpression",Disjunction([Cancot([Ref("PreIncrementExpression")]);Cancot([Ref("PreDecrementExpression")]);Cancot([Ref("AtomicPlus");Ref("UnaryExpression")]);Cancot([Ref("AtomicMinus");Ref("UnaryExpression")]);Cancot([Ref("UnaryExpressionNotPlusMinus")])]));
   ("UnaryExpressionNotPlusMinus",Disjunction([Cancot([Ref("PostfixExpression")]);Cancot([Ref("AtomicCompl");Ref("UnaryExpression")]);Cancot([Ref("AtomicNot");Ref("UnaryExpression")]);Cancot([Ref("CastExpression")]);Cancot([Ref("SwitchExpression")])]));
   ("UnqualifiedClassInstanceCreationExpression",Disjunction([Cancot([Ref("AtomicNew");Ref("OptionalTypeArguments");Ref("ClassOrInterfaceTypeToInstantiate");Ref("ParenthesedArgumentList");Ref("OptionalClassBody")])]));
   ("VariableArityParameter",Disjunction([Cancot([Ref("StarredVariableModifier");Ref("UnannType");Ref("StarredAnnotation");Ref("AtomicDot");Ref("AtomicDot");Ref("AtomicDot");Ref("Identifier")])]));
   ("VariableArityRecordComponent",Disjunction([Cancot([Ref("StarredRecordComponentModifier");Ref("UnannType");Ref("StarredAnnotation");Ref("AtomicDot");Ref("AtomicDot");Ref("AtomicDot");Ref("Identifier")])]));
   ("VariableDeclarator",Disjunction([Cancot([Ref("VariableDeclaratorId");Ref("OptionalEqualsVariableInitializer")])]));
   ("VariableDeclaratorId",Disjunction([Cancot([Ref("Identifier");Ref("OptionalDims")])]));
   ("VariableDeclaratorList",Disjunction([Cancot([Ref("VariableDeclarator");Ref("StarredVariableDeclaratorPrecededByComma")])]));
   ("VariableDeclaratorPrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("VariableDeclarator")])]));
   ("VariableInitializer",Disjunction([Cancot([Ref("Expression")]);Cancot([Ref("ArrayInitializer")])]));
   ("VariableInitializerList",Disjunction([Cancot([Ref("VariableInitializer");Ref("StarredVariableInitializerPrecededByComma")])]));
   ("VariableInitializerPrecededByComma",Disjunction([Cancot([Ref("AtomicCm");Ref("VariableInitializer")])]));
   ("VariableModifier",Disjunction([Cancot([Ref("Annotation")]);Cancot([Ref("AtomicFinal")])]));
   ("WhileStatement",Disjunction([Cancot([Ref("AtomicWhile");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("Statement")])]));
   ("WhileStatementNoShortIf",Disjunction([Cancot([Ref("AtomicWhile");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("StatementNoShortIf")])]));
   ("Wildcard",Disjunction([Cancot([Ref("StarredAnnotation");Ref("AtomicCond");Ref("OptionalWildcardBounds")])]));
   ("WildcardBounds",Disjunction([Cancot([Ref("AtomicExtends");Ref("ReferenceType")]);Cancot([Ref("AtomicSuper");Ref("ReferenceType")])]));
   ("YieldStatement",Disjunction([Cancot([Ref("AtomicYield");Ref("Expression");Ref("AtomicSm")])]));

]);;


(* Java grammar ends here *)

 end ;;
let java_grammar = Private.java_grammar ;;
