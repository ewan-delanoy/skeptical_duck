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


type element_in_disjunction = Jvsp_abstract_language_t.element_in_disjunction = 
   Concat of string list ;;
     
type form =  Jvsp_abstract_language_t.form = 
   Disjunction of element_in_disjunction list 
   |Just_an_optional of string
   |Just_atomic of Jvsp_types.token_type list
   |Just_a_concat of string list 
   |Just_a_disjunction of string list 
   |Just_a_star of string 
   |Synonym of string;;

type t =  Jvsp_abstract_language_t.t = AL of (string * form) list ;; 

(* Java grammar begins here *)


 let java_grammar = 

AL ([

   ("AdditionalBound",Just_a_concat(["AtomicAnd";"InterfaceType"]));
   ("AdditiveExpression",Disjunction([Concat(["MultiplicativeExpression"]);Concat(["AdditiveExpression";"AtomicPlus";"MultiplicativeExpression"]);Concat(["AdditiveExpression";"AtomicMinus";"MultiplicativeExpression"])]));
   ("AmbiguousName",Disjunction([Concat(["Identifier"]);Concat(["AmbiguousName";"IdentifierPrecededByDot"])]));
   ("AndExpression",Disjunction([Concat(["EqualityExpression"]);Concat(["AndExpression";"AtomicAnd";"EqualityExpression"])]));
   ("AnnotatedIdentifierrPrecededByDot",Just_a_concat(["AtomicDot";"StarredAnnotation";"Identifier"]));
   ("Annotation",Disjunction([Concat(["NormalAnnotation"]);Concat(["MarkerAnnotation"]);Concat(["SingleElementAnnotation"])]));
   ("AnnotationInterfaceBody",Just_a_concat(["AtomicLc";"StarredAnnotationInterfaceMemberDeclaration";"AtomicRc"]));
   ("AnnotationInterfaceDeclaration",Just_a_concat(["StarredInterfaceModifier";"AtomicSnail";"AtomicInterface";"TypeIdentifier";"AnnotationInterfaceBody"]));
   ("AnnotationInterfaceElementDeclaration",Just_a_concat(["StarredAnnotationInterfaceElementModifier";"UnannType";"Identifier";"AtomicLp";"AtomicRp";"OptionalDims";"OptionalDefaultValue";"AtomicSm"]));
   ("AnnotationInterfaceElementModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicPublic"]);Concat(["AtomicAbstract"])]));
   ("AnnotationInterfaceMemberDeclaration",Disjunction([Concat(["AnnotationInterfaceElementDeclaration"]);Concat(["ConstantDeclaration"]);Concat(["ClassDeclaration"]);Concat(["InterfaceDeclaration"]);Concat(["AtomicSm"])]));
   ("ArgumentList",Just_a_concat(["Expression";"StarredExpressionPrecededByComma"]));
   ("ArrayAccess",Disjunction([Concat(["ExpressionName";"AtomicLb";"Expression";"AtomicRb"]);Concat(["PrimaryNoNewArray";"AtomicLb";"Expression";"AtomicRb"])]));
   ("ArrayCreationExpression",Disjunction([Concat(["AtomicNew";"PrimitiveType";"DimExprs";"OptionalDims"]);Concat(["AtomicNew";"ClassOrInterfaceType";"DimExprs";"OptionalDims"]);Concat(["AtomicNew";"PrimitiveType";"Dims";"ArrayInitializer"]);Concat(["AtomicNew";"ClassOrInterfaceType";"Dims";"ArrayInitializer"])]));
   ("ArrayInitializer",Just_a_concat(["AtomicLc";"OptionalVariableInitializerList";"OptionalCm";"AtomicRc"]));
   ("ArrayType",Disjunction([Concat(["PrimitiveType";"Dims"]);Concat(["ClassOrInterfaceType";"Dims"]);Concat(["TypeVariable";"Dims"])]));
   ("AssertStatement",Disjunction([Concat(["AtomicAssert";"Expression";"AtomicSm"]);Concat(["AtomicAssert";"Expression";"AtomicColon";"Expression";"AtomicSm"])]));
   ("Assignment",Just_a_concat(["LeftHandSide";"AtomicOperatorEq";"Expression"]));
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
   ("BasicForStatement",Just_a_concat(["AtomicFor";"AtomicLp";"OptionalForInit";"AtomicSm";"OptionalExpression";"AtomicSm";"OptionalForUpdate";"AtomicRp";"Statement"]));
   ("BasicForStatementNoShortIf",Just_a_concat(["AtomicFor";"AtomicLp";"OptionalForInit";"AtomicSm";"OptionalExpression";"AtomicSm";"OptionalForUpdate";"AtomicRp";"StatementNoShortIf"]));
   ("Block",Just_a_concat(["AtomicLc";"OptionalBlockStatements";"AtomicRc"]));
   ("BlockStatement",Disjunction([Concat(["LocalClassOrInterfaceDeclaration"]);Concat(["LocalVariableDeclarationStatement"]);Concat(["Statement"])]));
   ("BlockStatements",Just_a_concat(["BlockStatement";"StarredBlockStatement"]));
   ("BreakStatement",Just_a_concat(["AtomicBreak";"OptionalIdentifier";"AtomicSm"]));
   ("CaseConstant",Synonym("ConditionalExpression"));
   ("CaseConstantPrecededByComma",Just_a_concat(["AtomicCm";"CaseConstant"]));
   ("CastExpression",Disjunction([Concat(["AtomicLp";"PrimitiveType";"AtomicRp";"UnaryExpression"]);Concat(["AtomicLp";"ReferenceType";"StarredAdditionalBound";"AtomicRp";"UnaryExpressionNotPlusMinus"]);Concat(["AtomicLp";"ReferenceType";"StarredAdditionalBound";"AtomicRp";"LambdaExpression"])]));
   ("CatchClause",Just_a_concat(["AtomicCatch";"AtomicLp";"CatchFormalParameter";"AtomicRp";"Block"]));
   ("Catches",Just_a_concat(["CatchClause";"StarredCatchClause"]));
   ("CatchFormalParameter",Just_a_concat(["StarredVariableModifier";"CatchType";"VariableDeclaratorId"]));
   ("CatchType",Just_a_concat(["UnannClassType";"StarredClassTypePrecededByVerticalBar"]));
   ("ClassBody",Just_a_concat(["AtomicLc";"StarredClassBodyDeclaration";"AtomicRc"]));
   ("ClassBodyDeclaration",Disjunction([Concat(["ClassMemberDeclaration"]);Concat(["InstanceInitializer"]);Concat(["StaticInitializer"]);Concat(["ConstructorDeclaration"])]));
   ("ClassDeclaration",Disjunction([Concat(["NormalClassDeclaration"]);Concat(["EnumDeclaration"]);Concat(["RecordDeclaration"])]));
   ("ClassExtends",Just_a_concat(["AtomicExtends";"ClassType"]));
   ("ClassImplements",Just_a_concat(["AtomicImplements";"InterfaceTypeList"]));
   ("ClassInstanceCreationExpression",Disjunction([Concat(["UnqualifiedClassInstanceCreationExpression"]);Concat(["ExpressionName";"AtomicDot";"UnqualifiedClassInstanceCreationExpression"]);Concat(["Primary";"AtomicDot";"UnqualifiedClassInstanceCreationExpression"])]));
   ("ClassLiteral",Disjunction([Concat(["TypeName";"StarredOpenSquare";"AtomicDot";"AtomicClass"]);Concat(["NumericType";"StarredOpenSquare";"AtomicDot";"AtomicClass"]);Concat(["AtomicBoolean";"StarredOpenSquare";"AtomicDot";"AtomicClass"]);Concat(["AtomicVoid";"AtomicDot";"AtomicClass"])]));
   ("ClassMemberDeclaration",Disjunction([Concat(["FieldDeclaration"]);Concat(["MethodDeclaration"]);Concat(["ClassDeclaration"]);Concat(["InterfaceDeclaration"]);Concat(["AtomicSm"])]));
   ("ClassModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicPublic"]);Concat(["AtomicProtected"]);Concat(["AtomicPrivate"]);Concat(["AtomicAbstract"]);Concat(["AtomicStatic"]);Concat(["AtomicFinal"]);Concat(["AtomicSealed"]);Concat(["AtomicNonsealed"]);Concat(["AtomicStrictfp"])]));
   ("ClassOrInterfaceType",Disjunction([Concat(["ClassType"]);Concat(["InterfaceType"])]));
   ("ClassOrInterfaceTypeToInstantiate",Just_a_concat(["StarredAnnotation";"Identifier";"StarredAnnotatedIdentifierrPrecededByDot";"OptionalTypeArgumentsOrDiamond"]));
   ("ClassPermits",Just_a_concat(["AtomicPermits";"TypeName";"StarredTypeNamePrecededByComma"]));
   ("ClassType",Disjunction([Concat(["StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"]);Concat(["PackageName";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"]);Concat(["ClassOrInterfaceType";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"])]));
   ("ClassTypePrecededByVerticalBar",Just_a_concat(["AtomicOr";"ClassType"]));
   ("CompactConstructorDeclaration",Just_a_concat(["StarredConstructorModifier";"SimpleTypeName";"ConstructorBody"]));
   ("CompilationUnit",Disjunction([Concat(["OrdinaryCompilationUnit"]);Concat(["ModularCompilationUnit"])]));
   ("ConditionalAndExpression",Disjunction([Concat(["InclusiveOrExpression"]);Concat(["ConditionalAndExpression";"AtomicAndAnd";"InclusiveOrExpression"])]));
   ("ConditionalExpression",Disjunction([Concat(["ConditionalOrExpression"]);Concat(["ConditionalOrExpression";"AtomicCond";"Expression";"AtomicColon";"ConditionalExpression"]);Concat(["ConditionalOrExpression";"AtomicCond";"Expression";"AtomicColon";"LambdaExpression"])]));
   ("ConditionalOrExpression",Disjunction([Concat(["ConditionalAndExpression"]);Concat(["ConditionalOrExpression";"AtomicOrOr";"ConditionalAndExpression"])]));
   ("ConstantDeclaration",Just_a_concat(["StarredConstantModifier";"UnannType";"VariableDeclaratorList";"AtomicSm"]));
   ("ConstantExpression",Synonym("Expression"));
   ("ConstantModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicPublic"]);Concat(["AtomicStatic"]);Concat(["AtomicFinal"])]));
   ("ConstructorBody",Just_a_concat(["AtomicLc";"OptionalExplicitConstructorInvocation";"OptionalBlockStatements";"AtomicRc"]));
   ("ConstructorDeclaration",Just_a_concat(["StarredConstructorModifier";"ConstructorDeclarator";"OptionalThrows";"ConstructorBody"]));
   ("ConstructorDeclarator",Just_a_concat(["OptionalTypeParameters";"SimpleTypeName";"AtomicLp";"OptionalReceiverParameterFollowedByComma";"OptionalFormalParameterList";"AtomicRp"]));
   ("ConstructorModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicPublic"]);Concat(["AtomicProtected"]);Concat(["AtomicPrivate"])]));
   ("ContinueStatement",Just_a_concat(["AtomicContinue";"OptionalIdentifier";"AtomicSm"]));
   ("DefaultValue",Just_a_concat(["AtomicDefault";"ElementValue"]));
   ("DimExpr",Just_a_concat(["StarredAnnotation";"AtomicLb";"Expression";"AtomicRb"]));
   ("DimExprs",Just_a_concat(["DimExpr";"StarredDimExpr"]));
   ("Dims",Just_a_concat(["DimsElement";"StarredDimsElement"]));
   ("DimsElement",Just_a_concat(["StarredAnnotation";"AtomicLb";"AtomicRb"]));
   ("DoStatement",Just_a_concat(["AtomicDo";"Statement";"AtomicWhile";"AtomicLp";"Expression";"AtomicRp";"AtomicSm"]));
   ("ElementValue",Disjunction([Concat(["ConditionalExpression"]);Concat(["ElementValueArrayInitializer"]);Concat(["Annotation"])]));
   ("ElementValueArrayInitializer",Just_a_concat(["AtomicLc";"OptionalElementValueList";"OptionalCm";"AtomicRc"]));
   ("ElementValueList",Just_a_concat(["ElementValue";"StarredElementValuePrecededByComma"]));
   ("ElementValuePair",Just_a_concat(["Identifier";"AtomicEq";"ElementValue"]));
   ("ElementValuePairList",Just_a_concat(["ElementValuePair";"StarredElementValuePrecededByCommaPair"]));
   ("ElementValuePairPrecededByComma",Just_a_concat(["AtomicCm";"ElementValuePair"]));
   ("ElementValuePrecededByComma",Just_a_concat(["AtomicCm";"ElementValue"]));
   ("EmptyStatement",Synonym("AtomicSm"));
   ("EnhancedForStatement",Just_a_concat(["AtomicFor";"AtomicLp";"LocalVariableDeclaration";"AtomicColon";"Expression";"AtomicRp";"Statement"]));
   ("EnhancedForStatementNoShortIf",Just_a_concat(["AtomicFor";"AtomicLp";"LocalVariableDeclaration";"AtomicColon";"Expression";"AtomicRp";"StatementNoShortIf"]));
   ("EnumBody",Just_a_concat(["AtomicLc";"OptionalEnumConstantList";"OptionalCm";"OptionalEnumBodyDeclarations";"AtomicRc"]));
   ("EnumBodyDeclarations",Just_a_concat(["AtomicSm";"StarredClassBodyDeclaration"]));
   ("EnumConstant",Just_a_concat(["StarredEnumConstantModifier";"Identifier";"OptionalParenthesedArgumentList";"OptionalClassBody"]));
   ("EnumConstantList",Just_a_concat(["EnumConstant";"StarredEnumConstantPrecededByComma"]));
   ("EnumConstantModifier",Synonym("Annotation"));
   ("EnumConstantPrecededByComma",Just_a_concat(["AtomicCm";"EnumConstant"]));
   ("EnumDeclaration",Just_a_concat(["StarredClassModifier";"AtomicEnum";"TypeIdentifier";"OptionalClassImplements";"EnumBody"]));
   ("EqualityExpression",Disjunction([Concat(["RelationalExpression"]);Concat(["EqualityExpression";"AtomicEqEq";"RelationalExpression"]);Concat(["EqualityExpression";"AtomicNotEq";"RelationalExpression"])]));
   ("EqualsVariableInitializer",Just_a_concat(["AtomicEq";"VariableInitializer"]));
   ("ExceptionType",Disjunction([Concat(["ClassType"]);Concat(["TypeVariable"])]));
   ("ExceptionTypeList",Just_a_concat(["ExceptionType";"StarredExceptionTypePrecededByComma"]));
   ("ExceptionTypePrecededByComma",Just_a_concat(["AtomicCm";"ExceptionType"]));
   ("ExclusiveOrExpression",Disjunction([Concat(["AndExpression"]);Concat(["ExclusiveOrExpression";"AtomicXor";"AndExpression"])]));
   ("ExplicitConstructorInvocation",Disjunction([Concat(["OptionalTypeArguments";"AtomicThis";"ParenthesedArgumentList";"AtomicSm"]);Concat(["OptionalTypeArguments";"AtomicSuper";"ParenthesedArgumentList";"AtomicSm"]);Concat(["ExpressionName";"AtomicDot";"OptionalTypeArguments";"AtomicSuper";"ParenthesedArgumentList";"AtomicSm"]);Concat(["Primary";"AtomicDot";"OptionalTypeArguments";"AtomicSuper";"ParenthesedArgumentList";"AtomicSm"])]));
   ("Expression",Disjunction([Concat(["LambdaExpression"]);Concat(["AssignmentExpression"])]));
   ("ExpressionName",Disjunction([Concat(["Identifier"]);Concat(["AmbiguousName";"IdentifierPrecededByDot"])]));
   ("ExpressionPrecededByComma",Just_a_concat(["AtomicCm";"Expression"]));
   ("ExpressionStatement",Just_a_concat(["StatementExpression";"AtomicSm"]));
   ("FieldAccess",Disjunction([Concat(["Primary";"IdentifierPrecededByDot"]);Concat(["AtomicSuper";"IdentifierPrecededByDot"]);Concat(["TypeName";"AtomicDot";"AtomicSuper";"IdentifierPrecededByDot"])]));
   ("FieldDeclaration",Just_a_concat(["StarredFieldModifier";"UnannType";"VariableDeclaratorList";"AtomicSm"]));
   ("FieldModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicPublic"]);Concat(["AtomicProtected"]);Concat(["AtomicPrivate"]);Concat(["AtomicStatic"]);Concat(["AtomicFinal"]);Concat(["AtomicTransient"]);Concat(["AtomicVolatile"])]));
   ("Finally",Just_a_concat(["AtomicFinally";"Block"]));
   ("FloatingPointType",Disjunction([Concat(["AtomicFloat"]);Concat(["AtomicDouble"])]));
   ("ForInit",Disjunction([Concat(["StatementExpressionList"]);Concat(["LocalVariableDeclaration"])]));
   ("FormalParameter",Disjunction([Concat(["StarredVariableModifier";"UnannType";"VariableDeclaratorId"]);Concat(["VariableArityParameter"])]));
   ("FormalParameterList",Just_a_concat(["FormalParameter";"StarredFormalParameterPrecededByComma"]));
   ("FormalParameterPrecededByComma",Just_a_concat(["AtomicCm";"FormalParameter"]));
   ("ForStatement",Disjunction([Concat(["BasicForStatement"]);Concat(["EnhancedForStatement"])]));
   ("ForStatementNoShortIf",Disjunction([Concat(["BasicForStatementNoShortIf"]);Concat(["EnhancedForStatementNoShortIf"])]));
   ("ForUpdate",Synonym("StatementExpressionList"));
   ("IdentifierFollowedByDot",Just_a_concat(["Identifier";"AtomicDot"]));
   ("IdentifierPrecededByComma",Just_a_concat(["AtomicCm";"Identifier"]));
   ("IdentifierPrecededByDot",Just_a_concat(["AtomicDot";"Identifier"]));
   ("IfThenElseStatement",Just_a_concat(["AtomicIf";"AtomicLp";"Expression";"AtomicRp";"StatementNoShortIf";"AtomicElse";"Statement"]));
   ("IfThenElseStatementNoShortIf",Just_a_concat(["AtomicIf";"AtomicLp";"Expression";"AtomicRp";"StatementNoShortIf";"AtomicElse";"StatementNoShortIf"]));
   ("IfThenStatement",Just_a_concat(["AtomicIf";"AtomicLp";"Expression";"AtomicRp";"Statement"]));
   ("ImportDeclaration",Disjunction([Concat(["SingleTypeImportDeclaration"]);Concat(["TypeImportOnDemandDeclaration"]);Concat(["SingleStaticImportDeclaration"]);Concat(["StaticImportOnDemandDeclaration"])]));
   ("InclusiveOrExpression",Disjunction([Concat(["ExclusiveOrExpression"]);Concat(["InclusiveOrExpression";"AtomicOr";"ExclusiveOrExpression"])]));
   ("InstanceInitializer",Synonym("Block"));
   ("InstanceofExpression",Disjunction([Concat(["RelationalExpression";"AtomicInstanceof";"ReferenceType"]);Concat(["RelationalExpression";"AtomicInstanceof";"Pattern"])]));
   ("IntegralType",Disjunction([Concat(["AtomicByte"]);Concat(["AtomicShort"]);Concat(["AtomicInt"]);Concat(["AtomicLong"]);Concat(["AtomicChar"])]));
   ("InterfaceBody",Just_a_concat(["AtomicLc";"StarredInterfaceMemberDeclaration";"AtomicRc"]));
   ("InterfaceDeclaration",Disjunction([Concat(["NormalInterfaceDeclaration"]);Concat(["AnnotationInterfaceDeclaration"])]));
   ("InterfaceExtends",Just_a_concat(["AtomicExtends";"InterfaceTypeList"]));
   ("InterfaceMemberDeclaration",Disjunction([Concat(["ConstantDeclaration"]);Concat(["InterfaceMethodDeclaration"]);Concat(["ClassDeclaration"]);Concat(["InterfaceDeclaration"]);Concat(["AtomicSm"])]));
   ("InterfaceMethodDeclaration",Just_a_concat(["StarredInterfaceMethodModifier";"MethodHeader";"MethodBody"]));
   ("InterfaceMethodModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicPublic"]);Concat(["AtomicPrivate"]);Concat(["AtomicAbstract"]);Concat(["AtomicDefault"]);Concat(["AtomicStatic"]);Concat(["AtomicStrictfp"])]));
   ("InterfaceModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicPublic"]);Concat(["AtomicProtected"]);Concat(["AtomicPrivate"]);Concat(["AtomicAbstract"]);Concat(["AtomicStatic"]);Concat(["AtomicSealed"]);Concat(["AtomicNonsealed"]);Concat(["AtomicStrictfp"])]));
   ("InterfacePermits",Just_a_concat(["AtomicPermits";"TypeName";"StarredTypeNamePrecededByComma"]));
   ("InterfaceType",Synonym("ClassType"));
   ("InterfaceTypeList",Just_a_concat(["InterfaceType";"StarredInterfaceTypePrecededByComma"]));
   ("InterfaceTypePrecededByComma",Just_a_concat(["AtomicCm";"InterfaceType"]));
   ("LabeledStatement",Just_a_concat(["Identifier";"AtomicColon";"Statement"]));
   ("LabeledStatementNoShortIf",Just_a_concat(["Identifier";"AtomicColon";"StatementNoShortIf"]));
   ("LambdaBody",Disjunction([Concat(["Expression"]);Concat(["Block"])]));
   ("LambdaExpression",Just_a_concat(["LambdaParameters";"AtomicMinus";"AtomicGt";"LambdaBody"]));
   ("LambdaParameter",Disjunction([Concat(["StarredVariableModifier";"LambdaParameterType";"VariableDeclaratorId"]);Concat(["VariableArityParameter"])]));
   ("LambdaParameterList",Disjunction([Concat(["LambdaParameter";"StarredLambdaParameterPrecededByComma"]);Concat(["Identifier";"StarredIdentifierPrecededByComma"])]));
   ("LambdaParameterPrecededByComma",Just_a_concat(["AtomicCm";"LambdaParameter"]));
   ("LambdaParameters",Disjunction([Concat(["AtomicLp";"OptionalLambdaParameterList";"AtomicRp"]);Concat(["Identifier"])]));
   ("LambdaParameterType",Disjunction([Concat(["UnannType"]);Concat(["AtomicVar"])]));
   ("LeftHandSide",Disjunction([Concat(["ExpressionName"]);Concat(["FieldAccess"]);Concat(["ArrayAccess"])]));
   ("Literal",Disjunction([Concat(["IntegerLiteral"]);Concat(["FloatingPointLiteral"]);Concat(["BooleanLiteral"]);Concat(["CharacterLiteral"]);Concat(["StringLiteral"]);Concat(["TextBlock"]);Concat(["NullLiteral"])]));
   ("LocalClassOrInterfaceDeclaration",Disjunction([Concat(["ClassDeclaration"]);Concat(["NormalInterfaceDeclaration"])]));
   ("LocalVariableDeclaration",Just_a_concat(["StarredVariableModifier";"LocalVariableType";"VariableDeclaratorList"]));
   ("LocalVariableDeclarationStatement",Just_a_concat(["LocalVariableDeclaration";"AtomicSm"]));
   ("LocalVariableType",Disjunction([Concat(["UnannType"]);Concat(["AtomicVar"])]));
   ("MarkerAnnotation",Just_a_concat(["AtomicSnail";"TypeName"]));
   ("MethodBody",Disjunction([Concat(["Block"]);Concat(["AtomicSm"])]));
   ("MethodDeclaration",Just_a_concat(["StarredMethodModifier";"MethodHeader";"MethodBody"]));
   ("MethodDeclarator",Just_a_concat(["Identifier";"AtomicLp";"OptionalReceiverParameterFollowedByComma";"OptionalFormalParameterList";"AtomicRp";"OptionalDims"]));
   ("MethodHeader",Disjunction([Concat(["Result";"MethodDeclarator";"OptionalThrows"]);Concat(["TypeParameters";"StarredAnnotation";"Result";"MethodDeclarator";"OptionalThrows"])]));
   ("MethodInvocation",Disjunction([Concat(["MethodName";"ParenthesedArgumentList"]);Concat(["TypeName";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"]);Concat(["ExpressionName";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"]);Concat(["Primary";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"]);Concat(["AtomicSuper";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"]);Concat(["TypeName";"AtomicDot";"AtomicSuper";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"])]));
   ("MethodModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicPublic"]);Concat(["AtomicProtected"]);Concat(["AtomicPrivate"]);Concat(["AtomicAbstract"]);Concat(["AtomicStatic"]);Concat(["AtomicFinal"]);Concat(["AtomicSynchronized"]);Concat(["AtomicNative"]);Concat(["AtomicStrictfp"])]));
   ("MethodName",Synonym("UnqualifiedMethodIdentifier"));
   ("MethodReference",Disjunction([Concat(["ExpressionName";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["Primary";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["ReferenceType";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["AtomicSuper";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["TypeName";"AtomicDot";"AtomicSuper";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["ClassType";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"AtomicNew"]);Concat(["ArrayType";"AtomicColon";"AtomicColon";"AtomicNew"])]));
   ("ModularCompilationUnit",Just_a_concat(["StarredImportDeclaration";"ModuleDeclaration"]));
   ("ModuleDeclaration",Just_a_concat(["StarredAnnotation";"Optionalopen";"AtomicModule";"Identifier";"StarredIdentifierPrecededByDot";"AtomicLc";"StarredModuleDirective";"AtomicRc"]));
   ("ModuleDirective",Disjunction([Concat(["AtomicRequires";"StarredRequiresModifier";"ModuleName";"AtomicSm"]);Concat(["AtomicExports";"PackageName";"OptionalToModuleList";"AtomicSm"]);Concat(["AtomicOpens";"PackageName";"OptionalToModuleList";"AtomicSm"]);Concat(["AtomicUses";"TypeName";"AtomicSm"]);Concat(["AtomicProvides";"TypeName";"AtomicWith";"TypeName";"StarredTypeNamePrecededByComma";"AtomicSm"])]));
   ("ModuleName",Disjunction([Concat(["Identifier"]);Concat(["ModuleName";"IdentifierPrecededByDot"])]));
   ("ModuleNamePrecededByComma",Just_a_concat(["AtomicCm";"ModuleName"]));
   ("MultiplicativeExpression",Disjunction([Concat(["UnaryExpression"]);Concat(["MultiplicativeExpression";"AtomicTimes";"UnaryExpression"]);Concat(["MultiplicativeExpression";"AtomicDiv";"UnaryExpression"]);Concat(["MultiplicativeExpression";"AtomicMod";"UnaryExpression"])]));
   ("NormalAnnotation",Just_a_concat(["AtomicSnail";"TypeName";"AtomicLp";"OptionalElementValuePairList";"AtomicRp"]));
   ("NormalClassDeclaration",Just_a_concat(["StarredClassModifier";"AtomicClass";"TypeIdentifier";"OptionalTypeParameters";"OptionalClassExtends";"OptionalClassImplements";"OptionalClassPermits";"ClassBody"]));
   ("NormalInterfaceDeclaration",Just_a_concat(["StarredInterfaceModifier";"AtomicInterface";"TypeIdentifier";"OptionalTypeParameters";"OptionalInterfaceExtends";"OptionalInterfacePermits";"InterfaceBody"]));
   ("NumericType",Disjunction([Concat(["IntegralType"]);Concat(["FloatingPointType"])]));
   ("OpenSquare",Just_a_concat(["AtomicLb";"AtomicRb"]));
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
   ("OrdinaryCompilationUnit",Just_a_concat(["OptionalPackageDeclaration";"StarredImportDeclaration";"StarredTopLevelClassOrInterfaceDeclaration"]));
   ("PackageDeclaration",Just_a_concat(["StarredPackageModifier";"AtomicPackage";"Identifier";"StarredIdentifierPrecededByDot";"AtomicSm"]));
   ("PackageModifier",Synonym("Annotation"));
   ("PackageName",Disjunction([Concat(["Identifier"]);Concat(["PackageName";"IdentifierPrecededByDot"])]));
   ("PackageOrTypeName",Disjunction([Concat(["Identifier"]);Concat(["PackageOrTypeName";"IdentifierPrecededByDot"])]));
   ("ParenthesedArgumentList",Just_a_concat(["AtomicLp";"OptionalArgumentList";"AtomicRp"]));
   ("Pattern",Synonym("TypePattern"));
   ("PostDecrementExpression",Just_a_concat(["PostfixExpression";"AtomicDecr"]));
   ("PostfixExpression",Disjunction([Concat(["Primary"]);Concat(["ExpressionName"]);Concat(["PostIncrementExpression"]);Concat(["PostDecrementExpression"])]));
   ("PostIncrementExpression",Just_a_concat(["PostfixExpression";"AtomicIncr"]));
   ("PreDecrementExpression",Just_a_concat(["AtomicDecr";"UnaryExpression"]));
   ("PreIncrementExpression",Just_a_concat(["AtomicIncr";"UnaryExpression"]));
   ("Primary",Disjunction([Concat(["PrimaryNoNewArray"]);Concat(["ArrayCreationExpression"])]));
   ("PrimaryNoNewArray",Disjunction([Concat(["Literal"]);Concat(["ClassLiteral"]);Concat(["AtomicThis"]);Concat(["TypeName";"AtomicDot";"AtomicThis"]);Concat(["AtomicLp";"Expression";"AtomicRp"]);Concat(["ClassInstanceCreationExpression"]);Concat(["FieldAccess"]);Concat(["ArrayAccess"]);Concat(["MethodInvocation"]);Concat(["Methodref_in_diserence"])]));
   ("PrimitiveType",Disjunction([Concat(["StarredAnnotation";"NumericType"]);Concat(["StarredAnnotation";"AtomicBoolean"])]));
   ("ReceiverParameter",Just_a_concat(["StarredAnnotation";"UnannType";"OptionalIdentifierFollowedByDot";"AtomicThis"]));
   ("ReceiverParameterFollowedByComma",Just_a_concat(["ReceiverParameter";"AtomicCm"]));
   ("RecordBody",Just_a_concat(["AtomicLc";"StarredRecordBodyDeclaration";"AtomicRc"]));
   ("RecordBodyDeclaration",Disjunction([Concat(["ClassBodyDeclaration"]);Concat(["CompactConstructorDeclaration"])]));
   ("RecordComponent",Disjunction([Concat(["StarredRecordComponentModifier";"UnannType";"Identifier"]);Concat(["VariableArityRecordComponent"])]));
   ("RecordComponentList",Just_a_concat(["RecordComponent";"StarredRecordComponentPrecededByComma"]));
   ("RecordComponentModifier",Synonym("Annotation"));
   ("RecordComponentPrecededByComma",Just_a_concat(["AtomicCm";"RecordComponent"]));
   ("RecordDeclaration",Just_a_concat(["StarredClassModifier";"AtomicRecord";"TypeIdentifier";"OptionalTypeParameters";"RecordHeader";"OptionalClassImplements";"RecordBody"]));
   ("RecordHeader",Just_a_concat(["AtomicLp";"OptionalRecordComponentList";"AtomicRp"]));
   ("ReferenceType",Disjunction([Concat(["ClassOrInterfaceType"]);Concat(["TypeVariable"]);Concat(["ArrayType"])]));
   ("RelationalExpression",Disjunction([Concat(["ShiftExpression"]);Concat(["RelationalExpression";"AtomicLt";"ShiftExpression"]);Concat(["RelationalExpression";"AtomicGt";"ShiftExpression"]);Concat(["RelationalExpression";"AtomicLe";"ShiftExpression"]);Concat(["RelationalExpression";"AtomicGe";"ShiftExpression"]);Concat(["InstanceofExpression"])]));
   ("RequiresModifier",Disjunction([Concat(["AtomicTransitive"]);Concat(["AtomicStatic"])]));
   ("Resource",Disjunction([Concat(["LocalVariableDeclaration"]);Concat(["VariableAccess"])]));
   ("ResourceList",Just_a_concat(["Resource";"StarredResourcePrecededBySemiColon"]));
   ("ResourcePrecededByComma",Just_a_concat(["AtomicCm";"Resource"]));
   ("ResourcePrecededBySemiColon",Just_a_concat(["AtomicSm";"Resource"]));
   ("ResourceSpecification",Just_a_concat(["AtomicLp";"ResourceList";"AtomicSm";"AtomicRp"]));
   ("Result",Disjunction([Concat(["UnannType"]);Concat(["AtomicVoid"])]));
   ("ReturnStatement",Just_a_concat(["AtomicReturn";"OptionalExpression";"AtomicSm"]));
   ("ShiftExpression",Disjunction([Concat(["AdditiveExpression"]);Concat(["ShiftExpression";"AtomicLs";"AdditiveExpression"]);Concat(["ShiftExpression";"AtomicSrs";"AdditiveExpression"]);Concat(["ShiftExpression";"AtomicUrs";"AdditiveExpression"])]));
   ("SimpleTypeName",Synonym("TypeIdentifier"));
   ("SingleElementAnnotation",Just_a_concat(["AtomicSnail";"TypeName";"AtomicLp";"ElementValue";"AtomicRp"]));
   ("SingleStaticImportDeclaration",Just_a_concat(["AtomicImport";"AtomicStatic";"TypeName";"IdentifierPrecededByDot";"AtomicSm"]));
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
   ("StatementExpressionList",Just_a_concat(["StatementExpression";"StarredStatementExpressionPrecededByComma"]));
   ("StatementExpressionPrecededByComma",Just_a_concat(["AtomicCm";"StatementExpression"]));
   ("StatementNoShortIf",Disjunction([Concat(["StatementWithoutTrailingSubstatement"]);Concat(["LabeledStatementNoShortIf"]);Concat(["IfThenElseStatementNoShortIf"]);Concat(["WhileStatementNoShortIf"]);Concat(["ForStatementNoShortIf"])]));
   ("StatementWithoutTrailingSubstatement",Disjunction([Concat(["Block"]);Concat(["EmptyStatement"]);Concat(["ExpressionStatement"]);Concat(["AssertStatement"]);Concat(["SwitchStatement"]);Concat(["DoStatement"]);Concat(["BreakStatement"]);Concat(["ContinueStatement"]);Concat(["ReturnStatement"]);Concat(["SynchronizedStatement"]);Concat(["ThrowStatement"]);Concat(["TryStatement"]);Concat(["YieldStatement"])]));
   ("StaticImportOnDemandDeclaration",Just_a_concat(["AtomicImport";"AtomicStatic";"TypeName";"AtomicDot";"AtomicTimes";"AtomicSm"]));
   ("StaticInitializer",Just_a_concat(["AtomicStatic";"Block"]));
   ("SwitchBlock",Disjunction([Concat(["AtomicLc";"SwitchRule";"StarredSwitchRule";"AtomicRc"]);Concat(["AtomicLc";"StarredSwitchBlockStatementGroup";"StarredSwitchLabelFollowedByColon";"AtomicRc"])]));
   ("SwitchBlockStatementGroup",Just_a_concat(["SwitchLabelFollowedByColon";"StarredSwitchLabelFollowedByColon";"BlockStatements"]));
   ("SwitchExpression",Just_a_concat(["AtomicSwitch";"AtomicLp";"Expression";"AtomicRp";"SwitchBlock"]));
   ("SwitchLabel",Disjunction([Concat(["AtomicCase";"CaseConstant";"StarredCaseConstantPrecededByComma"]);Concat(["AtomicDefault"])]));
   ("SwitchLabelFollowedByColon",Just_a_concat(["SwitchLabel";"AtomicColon"]));
   ("SwitchRule",Disjunction([Concat(["SwitchLabel";"AtomicMinus";"AtomicGt";"Expression";"AtomicSm"]);Concat(["SwitchLabel";"AtomicMinus";"AtomicGt";"Block"]);Concat(["SwitchLabel";"AtomicMinus";"AtomicGt";"ThrowStatement"])]));
   ("SwitchStatement",Just_a_concat(["AtomicSwitch";"AtomicLp";"Expression";"AtomicRp";"SwitchBlock"]));
   ("SynchronizedStatement",Just_a_concat(["AtomicSynchronized";"AtomicLp";"Expression";"AtomicRp";"Block"]));
   ("Throws",Just_a_concat(["AtomicThrows";"ExceptionTypeList"]));
   ("ThrowStatement",Just_a_concat(["AtomicThrow";"Expression";"AtomicSm"]));
   ("ToModuleList",Just_a_concat(["AtomicTo";"ModuleName";"StarredModuleNamePrecededByComma"]));
   ("TopLevelClassOrInterfaceDeclaration",Disjunction([Concat(["ClassDeclaration"]);Concat(["InterfaceDeclaration"]);Concat(["AtomicSm"])]));
   ("TryStatement",Disjunction([Concat(["AtomicTry";"Block";"Catches"]);Concat(["AtomicTry";"Block";"OptionalCatches";"Finally"]);Concat(["TryWithResourcesStatement"])]));
   ("TryWithResourcesStatement",Just_a_concat(["AtomicTry";"ResourceSpecification";"Block";"OptionalCatches";"OptionalFinally"]));
   ("Type",Disjunction([Concat(["PrimitiveType"]);Concat(["ref_in_diserenceType"])]));
   ("TypeArgument",Disjunction([Concat(["ref_in_diserenceType"]);Concat(["Wildcard"])]));
   ("TypeArgumentList",Just_a_concat(["TypeArgument";"StarredTypeArgumentPrecededByComma"]));
   ("TypeArgumentPrecededByComma",Just_a_concat(["AtomicCm";"TypeArgument"]));
   ("TypeArguments",Just_a_concat(["AtomicLt";"TypeArgumentList";"AtomicGt"]));
   ("TypeArgumentsOrDiamond",Disjunction([Concat(["TypeArguments"]);Concat(["AtomicLt";"AtomicGt"])]));
   ("TypeBound",Disjunction([Concat(["AtomicExtends";"TypeVariable"]);Concat(["AtomicExtends";"ClassOrInterfaceType";"StarredAdditionalBound"])]));
   ("TypeImportOnDemandDeclaration",Just_a_concat(["AtomicImport";"PackageOrTypeName";"AtomicDot";"AtomicTimes";"AtomicSm"]));
   ("TypeName",Disjunction([Concat(["TypeIdentifier"]);Concat(["PackageOrTypeName";"AtomicDot";"TypeIdentifier"])]));
   ("TypeNamePrecededByComma",Just_a_concat(["AtomicCm";"TypeName"]));
   ("TypeParameter",Just_a_concat(["StarredTypeParameterModifier";"TypeIdentifier";"OptionalTypeBound"]));
   ("TypeParameterList",Just_a_concat(["TypeParameter";"StarredTypeParameterPrecededByComma"]));
   ("TypeParameterModifier",Synonym("Annotation"));
   ("TypeParameterPrecededByComma",Just_a_concat(["AtomicCm";"TypeParameter"]));
   ("TypeParameters",Just_a_concat(["AtomicLt";"TypeParameterList";"AtomicGt"]));
   ("TypePattern",Synonym("LocalVariableDeclaration"));
   ("TypeVariable",Just_a_concat(["StarredAnnotation";"TypeIdentifier"]));
   ("UnannArrayType",Disjunction([Concat(["UnannPrimitiveType";"Dims"]);Concat(["UnannClassOrInterfaceType";"Dims"]);Concat(["UnannTypeVariable";"Dims"])]));
   ("UnannClassOrInterfaceType",Disjunction([Concat(["UnannClassType"]);Concat(["UnannInterfaceType"])]));
   ("UnannClassType",Disjunction([Concat(["TypeIdentifier";"OptionalTypeArguments"]);Concat(["PackageName";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"]);Concat(["UnannClassOrInterfaceType";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"])]));
   ("UnannInterfaceType",Synonym("UnannClassType"));
   ("UnannPrimitiveType",Disjunction([Concat(["NumericType"]);Concat(["AtomicBoolean"])]));
   ("UnannReferenceType",Disjunction([Concat(["UnannClassOrInterfaceType"]);Concat(["UnannTypeVariable"]);Concat(["UnannArrayType"])]));
   ("UnannType",Disjunction([Concat(["UnannPrimitiveType"]);Concat(["Unannref_in_diserenceType"])]));
   ("UnannTypeVariable",Synonym("TypeIdentifier"));
   ("UnaryExpression",Disjunction([Concat(["PreIncrementExpression"]);Concat(["PreDecrementExpression"]);Concat(["AtomicPlus";"UnaryExpression"]);Concat(["AtomicMinus";"UnaryExpression"]);Concat(["UnaryExpressionNotPlusMinus"])]));
   ("UnaryExpressionNotPlusMinus",Disjunction([Concat(["PostfixExpression"]);Concat(["AtomicCompl";"UnaryExpression"]);Concat(["AtomicNot";"UnaryExpression"]);Concat(["CastExpression"]);Concat(["SwitchExpression"])]));
   ("UnqualifiedClassInstanceCreationExpression",Just_a_concat(["AtomicNew";"OptionalTypeArguments";"ClassOrInterfaceTypeToInstantiate";"ParenthesedArgumentList";"OptionalClassBody"]));
   ("VariableArityParameter",Just_a_concat(["StarredVariableModifier";"UnannType";"StarredAnnotation";"AtomicDot";"AtomicDot";"AtomicDot";"Identifier"]));
   ("VariableArityRecordComponent",Just_a_concat(["StarredRecordComponentModifier";"UnannType";"StarredAnnotation";"AtomicDot";"AtomicDot";"AtomicDot";"Identifier"]));
   ("VariableDeclarator",Just_a_concat(["VariableDeclaratorId";"OptionalEqualsVariableInitializer"]));
   ("VariableDeclaratorId",Just_a_concat(["Identifier";"OptionalDims"]));
   ("VariableDeclaratorList",Just_a_concat(["VariableDeclarator";"StarredVariableDeclaratorPrecededByComma"]));
   ("VariableDeclaratorPrecededByComma",Just_a_concat(["AtomicCm";"VariableDeclarator"]));
   ("VariableInitializer",Disjunction([Concat(["Expression"]);Concat(["ArrayInitializer"])]));
   ("VariableInitializerList",Just_a_concat(["VariableInitializer";"StarredVariableInitializerPrecededByComma"]));
   ("VariableInitializerPrecededByComma",Just_a_concat(["AtomicCm";"VariableInitializer"]));
   ("VariableModifier",Disjunction([Concat(["Annotation"]);Concat(["AtomicFinal"])]));
   ("WhileStatement",Just_a_concat(["AtomicWhile";"AtomicLp";"Expression";"AtomicRp";"Statement"]));
   ("WhileStatementNoShortIf",Just_a_concat(["AtomicWhile";"AtomicLp";"Expression";"AtomicRp";"StatementNoShortIf"]));
   ("Wildcard",Just_a_concat(["StarredAnnotation";"AtomicCond";"OptionalWildcardBounds"]));
   ("WildcardBounds",Disjunction([Concat(["AtomicExtends";"ReferenceType"]);Concat(["AtomicSuper";"ReferenceType"])]));
   ("YieldStatement",Just_a_concat(["AtomicYield";"Expression";"AtomicSm"]));

]);;


(* Java grammar ends here *)

 end ;;
let java_grammar = Private.java_grammar ;;
