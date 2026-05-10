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
   ("AdditionalBound",Disjunction([Concat(["AtomicAnd";"InterfaceType"])]));
   ("AdditiveExpression",Disjunction([Concat(["MultiplicativeExpression"]);Concat(["AdditiveExpression";"AtomicPlus";"MultiplicativeExpression"]);Concat(["AdditiveExpression";"AtomicMinus";"MultiplicativeExpression"])]));
   ("AmbiguousName",Disjunction([Concat(["Identifier"]);Concat(["AmbiguousName";"IdentifierPrecededByDot"])]));
   ("AndExpression",Disjunction([Concat(["EqualityExpression"]);Concat(["AndExpression";"AtomicAnd";"EqualityExpression"])]));
   ("AnnotatedIdentifierrPrecededByDot",Disjunction([Concat(["AtomicDot";"StarredAnnotation";"Identifier"])]));
   ("Annotation",Disjunction([Concat(["NormalAnnotation"]);Concat(["MarkerAnnotation"]);Concat(["SingleElementAnnotation"])]));
   ("AnnotationInterfaceBody",Disjunction([Concat(["AtomicLc";"StarredAnnotationInterfaceMemberDeclaration";"AtomicRc"])]));
   ("AnnotationInterfaceDeclaration",Disjunction([Concat(["StarredInterfaceModifier";"AtomicSnail";"AtomicInterface";"TypeIdentifier";"AnnotationInterfaceBody"])]));
   ("AnnotationInterfaceElementDeclaration",Disjunction([Concat(["StarredAnnotationInterfaceElementModifier";"UnannType";"Identifier";"AtomicLp";"AtomicRp";"OptionalDims";"OptionalDefaultValue";"AtomicSm"])]));
   ("AnnotationInterfaceElementModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicPublic"]);Concat(["AtomicAbstract"])]));
   ("AnnotationInterfaceMemberDeclaration",Disjunction([Concat(["AnnotationInterfaceElementDeclaration"]);Concat(["ConstantDeclaration"]);Concat(["ClassDeclaration"]);Concat(["InterfaceDeclaration"]);Concat(["AtomicSm"])]));
   ("ArgumentList",Disjunction([Concat(["Expression";"StarredExpressionPrecededByComma"])]));
   ("ArrayAccess",Disjunction([Concat(["ExpressionName";"AtomicLb";"Expression";"AtomicRb"]);Concat(["PrimaryNoNewArray";"AtomicLb";"Expression";"AtomicRb"])]));
   ("ArrayCreationExpression",Disjunction([Concat(["AtomicNew";"PrimitiveType";"DimExprs";"OptionalDims"]);Concat(["AtomicNew";"ClassOrInterfaceType";"DimExprs";"OptionalDims"]);Concat(["AtomicNew";"PrimitiveType";"Dims";"ArrayInitializer"]);Concat(["AtomicNew";"ClassOrInterfaceType";"Dims";"ArrayInitializer"])]));
   ("ArrayInitializer",Disjunction([Concat(["AtomicLc";"OptionalVariableInitializerList";"OptionalCm";"AtomicRc"])]));
   ("ArrayType",Disjunction([Concat(["PrimitiveType";"Dims"]);Concat(["ClassOrInterfaceType";"Dims"]);Concat(["TypeVariable";"Dims"])]));
   ("AssertStatement",Disjunction([Concat(["AtomicAssert";"Expression";"AtomicSm"]);Concat(["AtomicAssert";"Expression";"AtomicColon";"Expression";"AtomicSm"])]));
   ("Assignment",Disjunction([Concat(["LeftHandSide";"AtomicOperatorEq";"Expression"])]));
   ("AssignmentExpression",Disjunction([Concat(["ConditionalExpression"]);Concat(["Assignment"])]));
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
   ("BasicForStatement",Disjunction([Concat(["AtomicFor";"AtomicLp";"OptionalForInit";"AtomicSm";"OptionalExpression";"AtomicSm";"OptionalForUpdate";"AtomicRp";"Statement"])]));
   ("BasicForStatementNoShortIf",Disjunction([Concat(["AtomicFor";"AtomicLp";"OptionalForInit";"AtomicSm";"OptionalExpression";"AtomicSm";"OptionalForUpdate";"AtomicRp";"StatementNoShortIf"])]));
   ("Block",Disjunction([Concat(["AtomicLc";"OptionalBlockStatements";"AtomicRc"])]));
   ("BlockStatement",Disjunction([Concat(["LocalClassOrInterfaceDeclaration"]);Concat(["LocalVariableDeclarationStatement"]);Concat(["Statement"])]));
   ("BlockStatements",Disjunction([Concat(["BlockStatement";"StarredBlockStatement"])]));
   ("BreakStatement",Disjunction([Concat(["AtomicBreak";"OptionalIdentifier";"AtomicSm"])]));
   ("CaseConstant",Disjunction([Concat(["ConditionalExpression"])]));
   ("CaseConstantPrecededByComma",Disjunction([Concat(["AtomicCm";"CaseConstant"])]));
   ("CastExpression",Disjunction([Concat(["AtomicLp";"PrimitiveType";"AtomicRp";"UnaryExpression"]);Concat(["AtomicLp";"ReferenceType";"StarredAdditionalBound";"AtomicRp";"UnaryExpressionNotPlusMinus"]);Concat(["AtomicLp";"ReferenceType";"StarredAdditionalBound";"AtomicRp";"LambdaExpression"])]));
   ("CatchClause",Disjunction([Concat(["AtomicCatch";"AtomicLp";"CatchFormalParameter";"AtomicRp";"Block"])]));
   ("Catches",Disjunction([Concat(["CatchClause";"StarredCatchClause"])]));
   ("CatchFormalParameter",Disjunction([Concat(["StarredVariableModifier";"CatchType";"VariableDeclaratorId"])]));
   ("CatchType",Disjunction([Concat(["UnannClassType";"StarredClassTypePrecededByVerticalBar"])]));
   ("ClassBody",Disjunction([Concat(["AtomicLc";"StarredClassBodyDeclaration";"AtomicRc"])]));
   ("ClassBodyDeclaration",Disjunction([Concat(["ClassMemberDeclaration"]);Concat(["InstanceInitializer"]);Concat(["StaticInitializer"]);Concat(["ConstructorDeclaration"])]));
   ("ClassDeclaration",Disjunction([Concat(["NormalClassDeclaration"]);Concat(["EnumDeclaration"]);Concat(["RecordDeclaration"])]));
   ("ClassExtends",Disjunction([Concat(["AtomicExtends";"ClassType"])]));
   ("ClassImplements",Disjunction([Concat(["AtomicImplements";"InterfaceTypeList"])]));
   ("ClassInstanceCreationExpression",Disjunction([Concat(["UnqualifiedClassInstanceCreationExpression"]);Concat(["ExpressionName";"AtomicDot";"UnqualifiedClassInstanceCreationExpression"]);Concat(["Primary";"AtomicDot";"UnqualifiedClassInstanceCreationExpression"])]));
   ("ClassLiteral",Disjunction([Concat(["TypeName";"StarredOpenSquare";"AtomicDot";"AtomicClass"]);Concat(["NumericType";"StarredOpenSquare";"AtomicDot";"AtomicClass"]);Concat(["AtomicBoolean";"StarredOpenSquare";"AtomicDot";"AtomicClass"]);Concat(["AtomicVoid";"AtomicDot";"AtomicClass"])]));
   ("ClassMemberDeclaration",Disjunction([Concat(["FieldDeclaration"]);Concat(["MethodDeclaration"]);Concat(["ClassDeclaration"]);Concat(["InterfaceDeclaration"]);Concat(["AtomicSm"])]));
   ("ClassModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicPublic"]);Concat(["AtomicProtected"]);Concat(["AtomicPrivate"]);Concat(["AtomicAbstract"]);Concat(["AtomicStatic"]);Concat(["AtomicFinal"]);Concat(["AtomicSealed"]);Concat(["AtomicNonsealed"]);Concat(["AtomicStrictfp"])]));
   ("ClassOrInterfaceType",Disjunction([Concat(["ClassType"]);Concat(["InterfaceType"])]));
   ("ClassOrInterfaceTypeToInstantiate",Disjunction([Concat(["StarredAnnotation";"Identifier";"StarredAnnotatedIdentifierrPrecededByDot";"OptionalTypeArgumentsOrDiamond"])]));
   ("ClassPermits",Disjunction([Concat(["AtomicPermits";"TypeName";"StarredTypeNamePrecededByComma"])]));
   ("ClassType",Disjunction([Concat(["StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"]);Concat(["PackageName";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"]);Concat(["ClassOrInterfaceType";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"])]));
   ("ClassTypePrecededByVerticalBar",Disjunction([Concat(["AtomicOr";"ClassType"])]));
   ("CompactConstructorDeclaration",Disjunction([Concat(["StarredConstructorModifier";"SimpleTypeName";"ConstructorBody"])]));
   ("CompilationUnit",Disjunction([Concat(["OrdinaryCompilationUnit"]);Concat(["ModularCompilationUnit"])]));
   ("ConditionalAndExpression",Disjunction([Concat(["InclusiveOrExpression"]);Concat(["ConditionalAndExpression";"AtomicAndAnd";"InclusiveOrExpression"])]));
   ("ConditionalExpression",Disjunction([Concat(["ConditionalOrExpression"]);Concat(["ConditionalOrExpression";"AtomicCond";"Expression";"AtomicColon";"ConditionalExpression"]);Concat(["ConditionalOrExpression";"AtomicCond";"Expression";"AtomicColon";"LambdaExpression"])]));
   ("ConditionalOrExpression",Disjunction([Concat(["ConditionalAndExpression"]);Concat(["ConditionalOrExpression";"AtomicOrOr";"ConditionalAndExpression"])]));
   ("ConstantDeclaration",Disjunction([Concat(["StarredConstantModifier";"UnannType";"VariableDeclaratorList";"AtomicSm"])]));
   ("ConstantExpression",Disjunction([Concat(["Expression"])]));
   ("ConstantModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicPublic"]);Concat(["AtomicStatic"]);Concat(["AtomicFinal"])]));
   ("ConstructorBody",Disjunction([Concat(["AtomicLc";"OptionalExplicitConstructorInvocation";"OptionalBlockStatements";"AtomicRc"])]));
   ("ConstructorDeclaration",Disjunction([Concat(["StarredConstructorModifier";"ConstructorDeclarator";"OptionalThrows";"ConstructorBody"])]));
   ("ConstructorDeclarator",Disjunction([Concat(["OptionalTypeParameters";"SimpleTypeName";"AtomicLp";"OptionalReceiverParameterFollowedByComma";"OptionalFormalParameterList";"AtomicRp"])]));
   ("ConstructorModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicPublic"]);Concat(["AtomicProtected"]);Concat(["AtomicPrivate"])]));
   ("ContinueStatement",Disjunction([Concat(["AtomicContinue";"OptionalIdentifier";"AtomicSm"])]));
   ("DefaultValue",Disjunction([Concat(["AtomicDefault";"ElementValue"])]));
   ("DimExpr",Disjunction([Concat(["StarredAnnotation";"AtomicLb";"Expression";"AtomicRb"])]));
   ("DimExprs",Disjunction([Concat(["DimExpr";"StarredDimExpr"])]));
   ("Dims",Disjunction([Concat(["DimsElement";"StarredDimsElement"])]));
   ("DimsElement",Disjunction([Concat(["StarredAnnotation";"AtomicLb";"AtomicRb"])]));
   ("DoStatement",Disjunction([Concat(["AtomicDo";"Statement";"AtomicWhile";"AtomicLp";"Expression";"AtomicRp";"AtomicSm"])]));
   ("ElementValue",Disjunction([Concat(["ConditionalExpression"]);Concat(["ElementValueArrayInitializer"]);Concat(["Annotation"])]));
   ("ElementValueArrayInitializer",Disjunction([Concat(["AtomicLc";"OptionalElementValueList";"OptionalCm";"AtomicRc"])]));
   ("ElementValueList",Disjunction([Concat(["ElementValue";"StarredElementValuePrecededByComma"])]));
   ("ElementValuePair",Disjunction([Concat(["Identifier";"AtomicEq";"ElementValue"])]));
   ("ElementValuePairList",Disjunction([Concat(["ElementValuePair";"StarredElementValuePrecededByCommaPair"])]));
   ("ElementValuePairPrecededByComma",Disjunction([Concat(["AtomicCm";"ElementValuePair"])]));
   ("ElementValuePrecededByComma",Disjunction([Concat(["AtomicCm";"ElementValue"])]));
   ("EmptyStatement",Disjunction([Concat(["AtomicSm"])]));
   ("EnhancedForStatement",Disjunction([Concat(["AtomicFor";"AtomicLp";"LocalVariableDeclaration";"AtomicColon";"Expression";"AtomicRp";"Statement"])]));
   ("EnhancedForStatementNoShortIf",Disjunction([Concat(["AtomicFor";"AtomicLp";"LocalVariableDeclaration";"AtomicColon";"Expression";"AtomicRp";"StatementNoShortIf"])]));
   ("EnumBody",Disjunction([Concat(["AtomicLc";"OptionalEnumConstantList";"OptionalCm";"OptionalEnumBodyDeclarations";"AtomicRc"])]));
   ("EnumBodyDeclarations",Disjunction([Concat(["AtomicSm";"StarredClassBodyDeclaration"])]));
   ("EnumConstant",Disjunction([Concat(["StarredEnumConstantModifier";"Identifier";"OptionalParenthesedArgumentList";"OptionalClassBody"])]));
   ("EnumConstantList",Disjunction([Concat(["EnumConstant";"StarredEnumConstantPrecededByComma"])]));
   ("EnumConstantModifier",Disjunction([Concat(["Annotation"])]));
   ("EnumConstantPrecededByComma",Disjunction([Concat(["AtomicCm";"EnumConstant"])]));
   ("EnumDeclaration",Disjunction([Concat(["StarredClassModifier";"AtomicEnum";"TypeIdentifier";"OptionalClassImplements";"EnumBody"])]));
   ("EqualityExpression",Disjunction([Concat(["RelationalExpression"]);Concat(["EqualityExpression";"AtomicEqEq";"RelationalExpression"]);Concat(["EqualityExpression";"AtomicNotEq";"RelationalExpression"])]));
   ("EqualsVariableInitializer",Disjunction([Concat(["AtomicEq";"VariableInitializer"])]));
   ("ExceptionType",Disjunction([Concat(["ClassType"]);Concat(["TypeVariable"])]));
   ("ExceptionTypeList",Disjunction([Concat(["ExceptionType";"StarredExceptionTypePrecededByComma"])]));
   ("ExceptionTypePrecededByComma",Disjunction([Concat(["AtomicCm";"ExceptionType"])]));
   ("ExclusiveOrExpression",Disjunction([Concat(["AndExpression"]);Concat(["ExclusiveOrExpression";"AtomicXor";"AndExpression"])]));
   ("ExplicitConstructorInvocation",Disjunction([Concat(["OptionalTypeArguments";"AtomicThis";"ParenthesedArgumentList";"AtomicSm"]);Concat(["OptionalTypeArguments";"AtomicSuper";"ParenthesedArgumentList";"AtomicSm"]);Concat(["ExpressionName";"AtomicDot";"OptionalTypeArguments";"AtomicSuper";"ParenthesedArgumentList";"AtomicSm"]);Concat(["Primary";"AtomicDot";"OptionalTypeArguments";"AtomicSuper";"ParenthesedArgumentList";"AtomicSm"])]));
   ("Expression",Disjunction([Concat(["LambdaExpression"]);Concat(["AssignmentExpression"])]));
   ("ExpressionName",Disjunction([Concat(["Identifier"]);Concat(["AmbiguousName";"IdentifierPrecededByDot"])]));
   ("ExpressionPrecededByComma",Disjunction([Concat(["AtomicCm";"Expression"])]));
   ("ExpressionStatement",Disjunction([Concat(["StatementExpression";"AtomicSm"])]));
   ("FieldAccess",Disjunction([Concat(["Primary";"IdentifierPrecededByDot"]);Concat(["AtomicSuper";"IdentifierPrecededByDot"]);Concat(["TypeName";"AtomicDot";"AtomicSuper";"IdentifierPrecededByDot"])]));
   ("FieldDeclaration",Disjunction([Concat(["StarredFieldModifier";"UnannType";"VariableDeclaratorList";"AtomicSm"])]));
   ("FieldModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicPublic"]);Concat(["AtomicProtected"]);Concat(["AtomicPrivate"]);Concat(["AtomicStatic"]);Concat(["AtomicFinal"]);Concat(["AtomicTransient"]);Concat(["AtomicVolatile"])]));
   ("Finally",Disjunction([Concat(["AtomicFinally";"Block"])]));
   ("FloatingPointType",Disjunction([Concat(["AtomicFloat"]);Concat(["AtomicDouble"])]));
   ("ForInit",Disjunction([Concat(["StatementExpressionList"]);Concat(["LocalVariableDeclaration"])]));
   ("FormalParameter",Disjunction([Concat(["StarredVariableModifier";"UnannType";"VariableDeclaratorId"]);Concat(["VariableArityParameter"])]));
   ("FormalParameterList",Disjunction([Concat(["FormalParameter";"StarredFormalParameterPrecededByComma"])]));
   ("FormalParameterPrecededByComma",Disjunction([Concat(["AtomicCm";"FormalParameter"])]));
   ("ForStatement",Disjunction([Concat(["BasicForStatement"]);Concat(["EnhancedForStatement"])]));
   ("ForStatementNoShortIf",Disjunction([Concat(["BasicForStatementNoShortIf"]);Concat(["EnhancedForStatementNoShortIf"])]));
   ("ForUpdate",Disjunction([Concat(["StatementExpressionList"])]));
   ("IdentifierFollowedByDot",Disjunction([Concat(["Identifier";"AtomicDot"])]));
   ("IdentifierPrecededByComma",Disjunction([Concat(["AtomicCm";"Identifier"])]));
   ("IdentifierPrecededByDot",Disjunction([Concat(["AtomicDot";"Identifier"])]));
   ("IfThenElseStatement",Disjunction([Concat(["AtomicIf";"AtomicLp";"Expression";"AtomicRp";"StatementNoShortIf";"AtomicElse";"Statement"])]));
   ("IfThenElseStatementNoShortIf",Disjunction([Concat(["AtomicIf";"AtomicLp";"Expression";"AtomicRp";"StatementNoShortIf";"AtomicElse";"StatementNoShortIf"])]));
   ("IfThenStatement",Disjunction([Concat(["AtomicIf";"AtomicLp";"Expression";"AtomicRp";"Statement"])]));
   ("ImportDeclaration",Disjunction([Concat(["SingleTypeImportDeclaration"]);Concat(["TypeImportOnDemandDeclaration"]);Concat(["SingleStaticImportDeclaration"]);Concat(["StaticImportOnDemandDeclaration"])]));
   ("InclusiveOrExpression",Disjunction([Concat(["ExclusiveOrExpression"]);Concat(["InclusiveOrExpression";"AtomicOr";"ExclusiveOrExpression"])]));
   ("InstanceInitializer",Disjunction([Concat(["Block"])]));
   ("InstanceofExpression",Disjunction([Concat(["RelationalExpression";"AtomicInstanceof";"ReferenceType"]);Concat(["RelationalExpression";"AtomicInstanceof";"Pattern"])]));
   ("IntegralType",Disjunction([Concat(["AtomicByte"]);Concat(["AtomicShort"]);Concat(["AtomicInt"]);Concat(["AtomicLong"]);Concat(["AtomicChar"])]));
   ("InterfaceBody",Disjunction([Concat(["AtomicLc";"StarredInterfaceMemberDeclaration";"AtomicRc"])]));
   ("InterfaceDeclaration",Disjunction([Concat(["NormalInterfaceDeclaration"]);Concat(["AnnotationInterfaceDeclaration"])]));
   ("InterfaceExtends",Disjunction([Concat(["AtomicExtends";"InterfaceTypeList"])]));
   ("InterfaceMemberDeclaration",Disjunction([Concat(["ConstantDeclaration"]);Concat(["InterfaceMethodDeclaration"]);Concat(["ClassDeclaration"]);Concat(["InterfaceDeclaration"]);Concat(["AtomicSm"])]));
   ("InterfaceMethodDeclaration",Disjunction([Concat(["StarredInterfaceMethodModifier";"MethodHeader";"MethodBody"])]));
   ("InterfaceMethodModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicPublic"]);Concat(["AtomicPrivate"]);Concat(["AtomicAbstract"]);Concat(["AtomicDefault"]);Concat(["AtomicStatic"]);Concat(["AtomicStrictfp"])]));
   ("InterfaceModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicPublic"]);Concat(["AtomicProtected"]);Concat(["AtomicPrivate"]);Concat(["AtomicAbstract"]);Concat(["AtomicStatic"]);Concat(["AtomicSealed"]);Concat(["AtomicNonsealed"]);Concat(["AtomicStrictfp"])]));
   ("InterfacePermits",Disjunction([Concat(["AtomicPermits";"TypeName";"StarredTypeNamePrecededByComma"])]));
   ("InterfaceType",Disjunction([Concat(["ClassType"])]));
   ("InterfaceTypeList",Disjunction([Concat(["InterfaceType";"StarredInterfaceTypePrecededByComma"])]));
   ("InterfaceTypePrecededByComma",Disjunction([Concat(["AtomicCm";"InterfaceType"])]));
   ("LabeledStatement",Disjunction([Concat(["Identifier";"AtomicColon";"Statement"])]));
   ("LabeledStatementNoShortIf",Disjunction([Concat(["Identifier";"AtomicColon";"StatementNoShortIf"])]));
   ("LambdaBody",Disjunction([Concat(["Expression"]);Concat(["Block"])]));
   ("LambdaExpression",Disjunction([Concat(["LambdaParameters";"AtomicMinus";"AtomicGt";"LambdaBody"])]));
   ("LambdaParameter",Disjunction([Concat(["StarredVariableModifier";"LambdaParameterType";"VariableDeclaratorId"]);Concat(["VariableArityParameter"])]));
   ("LambdaParameterList",Disjunction([Concat(["LambdaParameter";"StarredLambdaParameterPrecededByComma"]);Concat(["Identifier";"StarredIdentifierPrecededByComma"])]));
   ("LambdaParameterPrecededByComma",Disjunction([Concat(["AtomicCm";"LambdaParameter"])]));
   ("LambdaParameters",Disjunction([Concat(["AtomicLp";"OptionalLambdaParameterList";"AtomicRp"]);Concat(["Identifier"])]));
   ("LambdaParameterType",Disjunction([Concat(["UnannType"]);Concat(["AtomicVar"])]));
   ("LeftHandSide",Disjunction([Concat(["ExpressionName"]);Concat(["FieldAccess"]);Concat(["ArrayAccess"])]));
   ("Literal",Disjunction([Concat(["IntegerLiteral"]);Concat(["FloatingPointLiteral"]);Concat(["BooleanLiteral"]);Concat(["CharacterLiteral"]);Concat(["StringLiteral"]);Concat(["TextBlock"]);Concat(["NullLiteral"])]));
   ("LocalClassOrInterfaceDeclaration",Disjunction([Concat(["ClassDeclaration"]);Concat(["NormalInterfaceDeclaration"])]));
   ("LocalVariableDeclaration",Disjunction([Concat(["StarredVariableModifier";"LocalVariableType";"VariableDeclaratorList"])]));
   ("LocalVariableDeclarationStatement",Disjunction([Concat(["LocalVariableDeclaration";"AtomicSm"])]));
   ("LocalVariableType",Disjunction([Concat(["UnannType"]);Concat(["AtomicVar"])]));
   ("MarkerAnnotation",Disjunction([Concat(["AtomicSnail";"TypeName"])]));
   ("MethodBody",Disjunction([Concat(["Block"]);Concat(["AtomicSm"])]));
   ("MethodDeclaration",Disjunction([Concat(["StarredMethodModifier";"MethodHeader";"MethodBody"])]));
   ("MethodDeclarator",Disjunction([Concat(["Identifier";"AtomicLp";"OptionalReceiverParameterFollowedByComma";"OptionalFormalParameterList";"AtomicRp";"OptionalDims"])]));
   ("MethodHeader",Disjunction([Concat(["Result";"MethodDeclarator";"OptionalThrows"]);Concat(["TypeParameters";"StarredAnnotation";"Result";"MethodDeclarator";"OptionalThrows"])]));
   ("MethodInvocation",Disjunction([Concat(["MethodName";"ParenthesedArgumentList"]);Concat(["TypeName";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"]);Concat(["ExpressionName";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"]);Concat(["Primary";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"]);Concat(["AtomicSuper";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"]);Concat(["TypeName";"AtomicDot";"AtomicSuper";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"])]));
   ("MethodModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicPublic"]);Concat(["AtomicProtected"]);Concat(["AtomicPrivate"]);Concat(["AtomicAbstract"]);Concat(["AtomicStatic"]);Concat(["AtomicFinal"]);Concat(["AtomicSynchronized"]);Concat(["AtomicNative"]);Concat(["AtomicStrictfp"])]));
   ("MethodName",Disjunction([Concat(["UnqualifiedMethodIdentifier"])]));
   ("MethodReference",Disjunction([Concat(["ExpressionName";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["Primary";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["ReferenceType";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["AtomicSuper";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["TypeName";"AtomicDot";"AtomicSuper";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["ClassType";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"AtomicNew"]);Concat(["ArrayType";"AtomicColon";"AtomicColon";"AtomicNew"])]));
   ("ModularCompilationUnit",Disjunction([Concat(["StarredImportDeclaration";"ModuleDeclaration"])]));
   ("ModuleDeclaration",Disjunction([Concat(["StarredAnnotation";"Optionalopen";"AtomicModule";"Identifier";"StarredIdentifierPrecededByDot";"AtomicLc";"StarredModuleDirective";"AtomicRc"])]));
   ("ModuleDirective",Disjunction([Concat(["AtomicRequires";"StarredRequiresModifier";"ModuleName";"AtomicSm"]);Concat(["AtomicExports";"PackageName";"OptionalToModuleList";"AtomicSm"]);Concat(["AtomicOpens";"PackageName";"OptionalToModuleList";"AtomicSm"]);Concat(["AtomicUses";"TypeName";"AtomicSm"]);Concat(["AtomicProvides";"TypeName";"AtomicWith";"TypeName";"StarredTypeNamePrecededByComma";"AtomicSm"])]));
   ("ModuleName",Disjunction([Concat(["Identifier"]);Concat(["ModuleName";"IdentifierPrecededByDot"])]));
   ("ModuleNamePrecededByComma",Disjunction([Concat(["AtomicCm";"ModuleName"])]));
   ("MultiplicativeExpression",Disjunction([Concat(["UnaryExpression"]);Concat(["MultiplicativeExpression";"AtomicTimes";"UnaryExpression"]);Concat(["MultiplicativeExpression";"AtomicDiv";"UnaryExpression"]);Concat(["MultiplicativeExpression";"AtomicMod";"UnaryExpression"])]));
   ("NormalAnnotation",Disjunction([Concat(["AtomicSnail";"TypeName";"AtomicLp";"OptionalElementValuePairList";"AtomicRp"])]));
   ("NormalClassDeclaration",Disjunction([Concat(["StarredClassModifier";"AtomicClass";"TypeIdentifier";"OptionalTypeParameters";"OptionalClassExtends";"OptionalClassImplements";"OptionalClassPermits";"ClassBody"])]));
   ("NormalInterfaceDeclaration",Disjunction([Concat(["StarredInterfaceModifier";"AtomicInterface";"TypeIdentifier";"OptionalTypeParameters";"OptionalInterfaceExtends";"OptionalInterfacePermits";"InterfaceBody"])]));
   ("NumericType",Disjunction([Concat(["IntegralType"]);Concat(["FloatingPointType"])]));
   ("OpenSquare",Disjunction([Concat(["AtomicLb";"AtomicRb"])]));
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
   ("OrdinaryCompilationUnit",Disjunction([Concat(["OptionalPackageDeclaration";"StarredImportDeclaration";"StarredTopLevelClassOrInterfaceDeclaration"])]));
   ("PackageDeclaration",Disjunction([Concat(["StarredPackageModifier";"AtomicPackage";"Identifier";"StarredIdentifierPrecededByDot";"AtomicSm"])]));
   ("PackageModifier",Disjunction([Concat(["Annotation"])]));
   ("PackageName",Disjunction([Concat(["Identifier"]);Concat(["PackageName";"IdentifierPrecededByDot"])]));
   ("PackageOrTypeName",Disjunction([Concat(["Identifier"]);Concat(["PackageOrTypeName";"IdentifierPrecededByDot"])]));
   ("ParenthesedArgumentList",Disjunction([Concat(["AtomicLp";"OptionalArgumentList";"AtomicRp"])]));
   ("Pattern",Disjunction([Concat(["TypePattern"])]));
   ("PostDecrementExpression",Disjunction([Concat(["PostfixExpression";"AtomicDecr"])]));
   ("PostfixExpression",Disjunction([Concat(["Primary"]);Concat(["ExpressionName"]);Concat(["PostIncrementExpression"]);Concat(["PostDecrementExpression"])]));
   ("PostIncrementExpression",Disjunction([Concat(["PostfixExpression";"AtomicIncr"])]));
   ("PreDecrementExpression",Disjunction([Concat(["AtomicDecr";"UnaryExpression"])]));
   ("PreIncrementExpression",Disjunction([Concat(["AtomicIncr";"UnaryExpression"])]));
   ("Primary",Disjunction([Concat(["PrimaryNoNewArray"]);Concat(["ArrayCreationExpression"])]));
   ("PrimaryNoNewArray",Disjunction([Concat(["Literal"]);Concat(["ClassLiteral"]);Concat(["AtomicThis"]);Concat(["TypeName";"AtomicDot";"AtomicThis"]);Concat(["AtomicLp";"Expression";"AtomicRp"]);Concat(["ClassInstanceCreationExpression"]);Concat(["FieldAccess"]);Concat(["ArrayAccess"]);Concat(["MethodInvocation"]);Concat(["Methodref_in_diserence"])]));
   ("PrimitiveType",Disjunction([Concat(["StarredAnnotation";"NumericType"]);Concat(["StarredAnnotation";"AtomicBoolean"])]));
   ("ReceiverParameter",Disjunction([Concat(["StarredAnnotation";"UnannType";"OptionalIdentifierFollowedByDot";"AtomicThis"])]));
   ("ReceiverParameterFollowedByComma",Disjunction([Concat(["ReceiverParameter";"AtomicCm"])]));
   ("RecordBody",Disjunction([Concat(["AtomicLc";"StarredRecordBodyDeclaration";"AtomicRc"])]));
   ("RecordBodyDeclaration",Disjunction([Concat(["ClassBodyDeclaration"]);Concat(["CompactConstructorDeclaration"])]));
   ("RecordComponent",Disjunction([Concat(["StarredRecordComponentModifier";"UnannType";"Identifier"]);Concat(["VariableArityRecordComponent"])]));
   ("RecordComponentList",Disjunction([Concat(["RecordComponent";"StarredRecordComponentPrecededByComma"])]));
   ("RecordComponentModifier",Disjunction([Concat(["Annotation"])]));
   ("RecordComponentPrecededByComma",Disjunction([Concat(["AtomicCm";"RecordComponent"])]));
   ("RecordDeclaration",Disjunction([Concat(["StarredClassModifier";"AtomicRecord";"TypeIdentifier";"OptionalTypeParameters";"RecordHeader";"OptionalClassImplements";"RecordBody"])]));
   ("RecordHeader",Disjunction([Concat(["AtomicLp";"OptionalRecordComponentList";"AtomicRp"])]));
   ("ReferenceType",Disjunction([Concat(["ClassOrInterfaceType"]);Concat(["TypeVariable"]);Concat(["ArrayType"])]));
   ("RelationalExpression",Disjunction([Concat(["ShiftExpression"]);Concat(["RelationalExpression";"AtomicLt";"ShiftExpression"]);Concat(["RelationalExpression";"AtomicGt";"ShiftExpression"]);Concat(["RelationalExpression";"AtomicLe";"ShiftExpression"]);Concat(["RelationalExpression";"AtomicGe";"ShiftExpression"]);Concat(["InstanceofExpression"])]));
   ("RequiresModifier",Disjunction([Concat(["AtomicTransitive"]);Concat(["AtomicStatic"])]));
   ("Resource",Disjunction([Concat(["LocalVariableDeclaration"]);Concat(["VariableAccess"])]));
   ("ResourceList",Disjunction([Concat(["Resource";"StarredResourcePrecededBySemiColon"])]));
   ("ResourcePrecededByComma",Disjunction([Concat(["AtomicCm";"Resource"])]));
   ("ResourcePrecededBySemiColon",Disjunction([Concat(["AtomicSm";"Resource"])]));
   ("ResourceSpecification",Disjunction([Concat(["AtomicLp";"ResourceList";"AtomicSm";"AtomicRp"])]));
   ("Result",Disjunction([Concat(["UnannType"]);Concat(["AtomicVoid"])]));
   ("ReturnStatement",Disjunction([Concat(["AtomicReturn";"OptionalExpression";"AtomicSm"])]));
   ("ShiftExpression",Disjunction([Concat(["AdditiveExpression"]);Concat(["ShiftExpression";"AtomicLs";"AdditiveExpression"]);Concat(["ShiftExpression";"AtomicSrs";"AdditiveExpression"]);Concat(["ShiftExpression";"AtomicUrs";"AdditiveExpression"])]));
   ("SimpleTypeName",Disjunction([Concat(["TypeIdentifier"])]));
   ("SingleElementAnnotation",Disjunction([Concat(["AtomicSnail";"TypeName";"AtomicLp";"ElementValue";"AtomicRp"])]));
   ("SingleStaticImportDeclaration",Disjunction([Concat(["AtomicImport";"AtomicStatic";"TypeName";"IdentifierPrecededByDot";"AtomicSm"])]));
   ("SingleTypeImportDeclaration",Disjunction([Concat(["AtomicImport";"TypeName";"AtomicSm"])]));
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
   ("Statement",Disjunction([Concat(["StatementWithoutTrailingSubstatement"]);Concat(["LabeledStatement"]);Concat(["IfThenStatement"]);Concat(["IfThenElseStatement"]);Concat(["WhileStatement"]);Concat(["ForStatement"])]));
   ("StatementExpression",Disjunction([Concat(["Assignment"]);Concat(["PreIncrementExpression"]);Concat(["PreDecrementExpression"]);Concat(["PostIncrementExpression"]);Concat(["PostDecrementExpression"]);Concat(["MethodInvocation"]);Concat(["ClassInstanceCreationExpression"])]));
   ("StatementExpressionList",Disjunction([Concat(["StatementExpression";"StarredStatementExpressionPrecededByComma"])]));
   ("StatementExpressionPrecededByComma",Disjunction([Concat(["AtomicCm";"StatementExpression"])]));
   ("StatementNoShortIf",Disjunction([Concat(["StatementWithoutTrailingSubstatement"]);Concat(["LabeledStatementNoShortIf"]);Concat(["IfThenElseStatementNoShortIf"]);Concat(["WhileStatementNoShortIf"]);Concat(["ForStatementNoShortIf"])]));
   ("StatementWithoutTrailingSubstatement",Disjunction([Concat(["Block"]);Concat(["EmptyStatement"]);Concat(["ExpressionStatement"]);Concat(["AssertStatement"]);Concat(["SwitchStatement"]);Concat(["DoStatement"]);Concat(["BreakStatement"]);Concat(["ContinueStatement"]);Concat(["ReturnStatement"]);Concat(["SynchronizedStatement"]);Concat(["ThrowStatement"]);Concat(["TryStatement"]);Concat(["YieldStatement"])]));
   ("StaticImportOnDemandDeclaration",Disjunction([Concat(["AtomicImport";"AtomicStatic";"TypeName";"AtomicDot";"AtomicTimes";"AtomicSm"])]));
   ("StaticInitializer",Disjunction([Concat(["AtomicStatic";"Block"])]));
   ("SwitchBlock",Disjunction([Concat(["AtomicLc";"SwitchRule";"StarredSwitchRule";"AtomicRc"]);Concat(["AtomicLc";"StarredSwitchBlockStatementGroup";"StarredSwitchLabelFollowedByColon";"AtomicRc"])]));
   ("SwitchBlockStatementGroup",Disjunction([Concat(["SwitchLabelFollowedByColon";"StarredSwitchLabelFollowedByColon";"BlockStatements"])]));
   ("SwitchExpression",Disjunction([Concat(["AtomicSwitch";"AtomicLp";"Expression";"AtomicRp";"SwitchBlock"])]));
   ("SwitchLabel",Disjunction([Concat(["AtomicCase";"CaseConstant";"StarredCaseConstantPrecededByComma"]);Concat(["AtomicDefault"])]));
   ("SwitchLabelFollowedByColon",Disjunction([Concat(["SwitchLabel";"AtomicColon"])]));
   ("SwitchRule",Disjunction([Concat(["SwitchLabel";"AtomicMinus";"AtomicGt";"Expression";"AtomicSm"]);Concat(["SwitchLabel";"AtomicMinus";"AtomicGt";"Block"]);Concat(["SwitchLabel";"AtomicMinus";"AtomicGt";"ThrowStatement"])]));
   ("SwitchStatement",Disjunction([Concat(["AtomicSwitch";"AtomicLp";"Expression";"AtomicRp";"SwitchBlock"])]));
   ("SynchronizedStatement",Disjunction([Concat(["AtomicSynchronized";"AtomicLp";"Expression";"AtomicRp";"Block"])]));
   ("Throws",Disjunction([Concat(["AtomicThrows";"ExceptionTypeList"])]));
   ("ThrowStatement",Disjunction([Concat(["AtomicThrow";"Expression";"AtomicSm"])]));
   ("ToModuleList",Disjunction([Concat(["AtomicTo";"ModuleName";"StarredModuleNamePrecededByComma"])]));
   ("TopLevelClassOrInterfaceDeclaration",Disjunction([Concat(["ClassDeclaration"]);Concat(["InterfaceDeclaration"]);Concat(["AtomicSm"])]));
   ("TryStatement",Disjunction([Concat(["AtomicTry";"Block";"Catches"]);Concat(["AtomicTry";"Block";"OptionalCatches";"Finally"]);Concat(["TryWithResourcesStatement"])]));
   ("TryWithResourcesStatement",Disjunction([Concat(["AtomicTry";"ResourceSpecification";"Block";"OptionalCatches";"OptionalFinally"])]));
   ("Type",Disjunction([Concat(["PrimitiveType"]);Concat(["ref_in_diserenceType"])]));
   ("TypeArgument",Disjunction([Concat(["ref_in_diserenceType"]);Concat(["Wildcard"])]));
   ("TypeArgumentList",Disjunction([Concat(["TypeArgument";"StarredTypeArgumentPrecededByComma"])]));
   ("TypeArgumentPrecededByComma",Disjunction([Concat(["AtomicCm";"TypeArgument"])]));
   ("TypeArguments",Disjunction([Concat(["AtomicLt";"TypeArgumentList";"AtomicGt"])]));
   ("TypeArgumentsOrDiamond",Disjunction([Concat(["TypeArguments"]);Concat(["AtomicLt";"AtomicGt"])]));
   ("TypeBound",Disjunction([Concat(["AtomicExtends";"TypeVariable"]);Concat(["AtomicExtends";"ClassOrInterfaceType";"StarredAdditionalBound"])]));
   ("TypeImportOnDemandDeclaration",Disjunction([Concat(["AtomicImport";"PackageOrTypeName";"AtomicDot";"AtomicTimes";"AtomicSm"])]));
   ("TypeName",Disjunction([Concat(["TypeIdentifier"]);Concat(["PackageOrTypeName";"AtomicDot";"TypeIdentifier"])]));
   ("TypeNamePrecededByComma",Disjunction([Concat(["AtomicCm";"TypeName"])]));
   ("TypeParameter",Disjunction([Concat(["StarredTypeParameterModifier";"TypeIdentifier";"OptionalTypeBound"])]));
   ("TypeParameterList",Disjunction([Concat(["TypeParameter";"StarredTypeParameterPrecededByComma"])]));
   ("TypeParameterModifier",Disjunction([Concat(["Annotation"])]));
   ("TypeParameterPrecededByComma",Disjunction([Concat(["AtomicCm";"TypeParameter"])]));
   ("TypeParameters",Disjunction([Concat(["AtomicLt";"TypeParameterList";"AtomicGt"])]));
   ("TypePattern",Disjunction([Concat(["LocalVariableDeclaration"])]));
   ("TypeVariable",Disjunction([Concat(["StarredAnnotation";"TypeIdentifier"])]));
   ("UnannArrayType",Disjunction([Concat(["UnannPrimitiveType";"Dims"]);Concat(["UnannClassOrInterfaceType";"Dims"]);Concat(["UnannTypeVariable";"Dims"])]));
   ("UnannClassOrInterfaceType",Disjunction([Concat(["UnannClassType"]);Concat(["UnannInterfaceType"])]));
   ("UnannClassType",Disjunction([Concat(["TypeIdentifier";"OptionalTypeArguments"]);Concat(["PackageName";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"]);Concat(["UnannClassOrInterfaceType";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"])]));
   ("UnannInterfaceType",Disjunction([Concat(["UnannClassType"])]));
   ("UnannPrimitiveType",Disjunction([Concat(["NumericType"]);Concat(["AtomicBoolean"])]));
   ("UnannReferenceType",Disjunction([Concat(["UnannClassOrInterfaceType"]);Concat(["UnannTypeVariable"]);Concat(["UnannArrayType"])]));
   ("UnannType",Disjunction([Concat(["UnannPrimitiveType"]);Concat(["Unannref_in_diserenceType"])]));
   ("UnannTypeVariable",Disjunction([Concat(["TypeIdentifier"])]));
   ("UnaryExpression",Disjunction([Concat(["PreIncrementExpression"]);Concat(["PreDecrementExpression"]);Concat(["AtomicPlus";"UnaryExpression"]);Concat(["AtomicMinus";"UnaryExpression"]);Concat(["UnaryExpressionNotPlusMinus"])]));
   ("UnaryExpressionNotPlusMinus",Disjunction([Concat(["PostfixExpression"]);Concat(["AtomicCompl";"UnaryExpression"]);Concat(["AtomicNot";"UnaryExpression"]);Concat(["CastExpression"]);Concat(["SwitchExpression"])]));
   ("UnqualifiedClassInstanceCreationExpression",Disjunction([Concat(["AtomicNew";"OptionalTypeArguments";"ClassOrInterfaceTypeToInstantiate";"ParenthesedArgumentList";"OptionalClassBody"])]));
   ("VariableArityParameter",Disjunction([Concat(["StarredVariableModifier";"UnannType";"StarredAnnotation";"AtomicDot";"AtomicDot";"AtomicDot";"Identifier"])]));
   ("VariableArityRecordComponent",Disjunction([Concat(["StarredRecordComponentModifier";"UnannType";"StarredAnnotation";"AtomicDot";"AtomicDot";"AtomicDot";"Identifier"])]));
   ("VariableDeclarator",Disjunction([Concat(["VariableDeclaratorId";"OptionalEqualsVariableInitializer"])]));
   ("VariableDeclaratorId",Disjunction([Concat(["Identifier";"OptionalDims"])]));
   ("VariableDeclaratorList",Disjunction([Concat(["VariableDeclarator";"StarredVariableDeclaratorPrecededByComma"])]));
   ("VariableDeclaratorPrecededByComma",Disjunction([Concat(["AtomicCm";"VariableDeclarator"])]));
   ("VariableInitializer",Disjunction([Concat(["Expression"]);Concat(["ArrayInitializer"])]));
   ("VariableInitializerList",Disjunction([Concat(["VariableInitializer";"StarredVariableInitializerPrecededByComma"])]));
   ("VariableInitializerPrecededByComma",Disjunction([Concat(["AtomicCm";"VariableInitializer"])]));
   ("VariableModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicFinal"])]));
   ("WhileStatement",Disjunction([Concat(["AtomicWhile";"AtomicLp";"Expression";"AtomicRp";"Statement"])]));
   ("WhileStatementNoShortIf",Disjunction([Concat(["AtomicWhile";"AtomicLp";"Expression";"AtomicRp";"StatementNoShortIf"])]));
   ("Wildcard",Disjunction([Concat(["StarredAnnotation";"AtomicCond";"OptionalWildcardBounds"])]));
   ("WildcardBounds",Disjunction([Concat(["AtomicExtends";"ReferenceType"]);Concat(["AtomicSuper";"ReferenceType"])]));
   ("YieldStatement",Disjunction([Concat(["AtomicYield";"Expression";"AtomicSm"])]));

]);;


(* Java grammar ends here *)

 end ;;
let java_grammar = Private.java_grammar ;;
