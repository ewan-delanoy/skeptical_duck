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
   ("AnnotatedIdentifierrPrecededByDot",Just_a_concat(["AtomicDot";"StarredAnnotation";"AtomicIdentifier"]));
   ("Annotation",Just_a_disjunction(["NormalAnnotation";"MarkerAnnotation";"SingleElementAnnotation"]));
   ("AnnotationInterfaceBody",Just_a_concat(["AtomicLb";"StarredAnnotationInterfaceMemberDeclaration";"AtomicRb"]));
   ("AnnotationInterfaceDeclaration",Just_a_concat(["StarredInterfaceModifier";"AtomicSnail";"AtomicInterface";"AtomicIdentifier";"AnnotationInterfaceBody"]));
   ("AnnotationInterfaceElementDeclaration",Just_a_concat(["StarredAnnotationInterfaceElementModifier";"UnannType";"AtomicIdentifier";"AtomicLp";"AtomicRp";"OptionalDims";"OptionalDefaultValue";"AtomicSm"]));
   ("AnnotationInterfaceElementModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicAbstract"]));
   ("AnnotationInterfaceMemberDeclaration",Just_a_disjunction(["AnnotationInterfaceElementDeclaration";"ConstantDeclaration";"ClassDeclaration";"InterfaceDeclaration";"AtomicSm"]));
   ("ArgumentList",Just_a_concat(["Expression";"StarredExpressionPrecededByComma"]));
   ("ArrayAccess",Disjunction([Concat(["ExpressionName";"AtomicLb";"Expression";"AtomicRb"]);Concat(["PrimaryNoNewArray";"AtomicLb";"Expression";"AtomicRb"])]));
   ("ArrayCreationExpression",Disjunction([Concat(["AtomicNew";"PrimitiveType";"DimExprs";"OptionalDims"]);Concat(["AtomicNew";"ClassOrInterfaceType";"DimExprs";"OptionalDims"]);Concat(["AtomicNew";"PrimitiveType";"Dims";"ArrayInitializer"]);Concat(["AtomicNew";"ClassOrInterfaceType";"Dims";"ArrayInitializer"])]));
   ("ArrayInitializer",Just_a_concat(["AtomicLb";"OptionalVariableInitializerList";"OptionalCm";"AtomicRb"]));
   ("ArrayType",Disjunction([Concat(["PrimitiveType";"Dims"]);Concat(["ClassOrInterfaceType";"Dims"]);Concat(["TypeVariable";"Dims"])]));
   ("AssertStatement",Disjunction([Concat(["AtomicAssert";"Expression";"AtomicSm"]);Concat(["AtomicAssert";"Expression";"AtomicColon";"Expression";"AtomicSm"])]));
   ("Assignment",Just_a_concat(["LeftHandSide";"AtomicOperatorEq";"Expression"]));
   ("AssignmentExpression",Just_a_disjunction(["ConditionalExpression";"Assignment"]));
   ("BasicForStatement",Just_a_concat(["AtomicFor";"AtomicLp";"OptionalForInit";"AtomicSm";"OptionalExpression";"AtomicSm";"OptionalForUpdate";"AtomicRp";"Statement"]));
   ("BasicForStatementNoShortIf",Just_a_concat(["AtomicFor";"AtomicLp";"OptionalForInit";"AtomicSm";"OptionalExpression";"AtomicSm";"OptionalForUpdate";"AtomicRp";"StatementNoShortIf"]));
   ("Block",Just_a_concat(["AtomicLb";"OptionalBlockStatements";"AtomicRb"]));
   ("BlockStatement",Just_a_disjunction(["LocalClassOrInterfaceDeclaration";"LocalVariableDeclarationStatement";"Statement"]));
   ("BlockStatements",Just_a_concat(["BlockStatement";"StarredBlockStatement"]));
   ("BreakStatement",Just_a_concat(["AtomicBreak";"OptionalIdentifier";"AtomicSm"]));
   ("CaseConstant",Synonym("ConditionalExpression"));
   ("CaseConstantPrecededByComma",Just_a_concat(["AtomicCm";"CaseConstant"]));
   ("CastExpression",Disjunction([Concat(["AtomicLp";"PrimitiveType";"AtomicRp";"UnaryExpression"]);Concat(["AtomicLp";"ReferenceType";"StarredAdditionalBound";"AtomicRp";"UnaryExpressionNotPlusMinus"]);Concat(["AtomicLp";"ReferenceType";"StarredAdditionalBound";"AtomicRp";"LambdaExpression"])]));
   ("CatchClause",Just_a_concat(["AtomicCatch";"AtomicLp";"CatchFormalParameter";"AtomicRp";"Block"]));
   ("Catches",Just_a_concat(["CatchClause";"StarredCatchClause"]));
   ("CatchFormalParameter",Just_a_concat(["StarredVariableModifier";"CatchType";"VariableDeclaratorId"]));
   ("CatchType",Just_a_concat(["UnannClassType";"StarredClassTypePrecededByVerticalBar"]));
   ("ClassBody",Just_a_concat(["AtomicLb";"StarredClassBodyDeclaration";"AtomicRb"]));
   ("ClassBodyDeclaration",Just_a_disjunction(["ClassMemberDeclaration";"InstanceInitializer";"StaticInitializer";"ConstructorDeclaration"]));
   ("ClassDeclaration",Just_a_disjunction(["NormalClassDeclaration";"EnumDeclaration";"RecordDeclaration"]));
   ("ClassExtends",Just_a_concat(["AtomicExtends";"ClassType"]));
   ("ClassImplements",Just_a_concat(["AtomicImplements";"InterfaceTypeList"]));
   ("ClassInstanceCreationExpression",Disjunction([Concat(["UnqualifiedClassInstanceCreationExpression"]);Concat(["ExpressionName";"AtomicDot";"UnqualifiedClassInstanceCreationExpression"]);Concat(["Primary";"AtomicDot";"UnqualifiedClassInstanceCreationExpression"])]));
   ("ClassLiteral",Disjunction([Concat(["TypeName";"StarredOpenSquare";"AtomicDot";"AtomicClass"]);Concat(["NumericType";"StarredOpenSquare";"AtomicDot";"AtomicClass"]);Concat(["AtomicBoolean";"StarredOpenSquare";"AtomicDot";"AtomicClass"]);Concat(["AtomicVoid";"AtomicDot";"AtomicClass"])]));
   ("ClassMemberDeclaration",Just_a_disjunction(["FieldDeclaration";"MethodDeclaration";"ClassDeclaration";"InterfaceDeclaration";"AtomicSm"]));
   ("ClassModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicProtected";"AtomicPrivate";"AtomicAbstract";"AtomicStatic";"AtomicFinal";"AtomicSealed";"AtomicNonsealed";"AtomicStrictfp"]));
   ("ClassOrInterfaceType",Just_a_disjunction(["ClassType";"InterfaceType"]));
   ("ClassOrInterfaceTypeToInstantiate",Just_a_concat(["StarredAnnotation";"AtomicIdentifier";"StarredAnnotatedIdentifierrPrecededByDot";"OptionalTypeArgumentsOrDiamond"]));
   ("ClassPermits",Just_a_concat(["AtomicPermits";"TypeName";"StarredTypeNamePrecededByComma"]));
   ("ClassType",Disjunction([Concat(["StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"]);Concat(["PackageName";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"]);Concat(["ClassOrInterfaceType";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"])]));
   ("ClassTypePrecededByVerticalBar",Just_a_concat(["AtomicOr";"ClassType"]));
   ("CompactConstructorDeclaration",Just_a_concat(["StarredConstructorModifier";"SimpleTypeName";"ConstructorBody"]));
   ("CompilationUnit",Just_a_disjunction(["OrdinaryCompilationUnit";"ModularCompilationUnit"]));
   ("ConditionalAndExpression",Disjunction([Concat(["InclusiveOrExpression"]);Concat(["ConditionalAndExpression";"AtomicAndAnd";"InclusiveOrExpression"])]));
   ("ConditionalExpression",Disjunction([Concat(["ConditionalOrExpression"]);Concat(["ConditionalOrExpression";"AtomicCond";"Expression";"AtomicColon";"ConditionalExpression"]);Concat(["ConditionalOrExpression";"AtomicCond";"Expression";"AtomicColon";"LambdaExpression"])]));
   ("ConditionalOrExpression",Disjunction([Concat(["ConditionalAndExpression"]);Concat(["ConditionalOrExpression";"AtomicOrOr";"ConditionalAndExpression"])]));
   ("ConstantDeclaration",Just_a_concat(["StarredConstantModifier";"UnannType";"VariableDeclaratorList";"AtomicSm"]));
   ("ConstantExpression",Synonym("Expression"));
   ("ConstantModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicStatic";"AtomicFinal"]));
   ("ConstructorBody",Just_a_concat(["AtomicLb";"OptionalExplicitConstructorInvocation";"OptionalBlockStatements";"AtomicRb"]));
   ("ConstructorDeclaration",Just_a_concat(["StarredConstructorModifier";"ConstructorDeclarator";"OptionalThrows";"ConstructorBody"]));
   ("ConstructorDeclarator",Just_a_concat(["OptionalTypeParameters";"SimpleTypeName";"AtomicLp";"OptionalReceiverParameterFollowedByComma";"OptionalFormalParameterList";"AtomicRp"]));
   ("ConstructorModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicProtected";"AtomicPrivate"]));
   ("ContinueStatement",Just_a_concat(["AtomicContinue";"OptionalIdentifier";"AtomicSm"]));
   ("DefaultValue",Just_a_concat(["AtomicDefault";"ElementValue"]));
   ("DimExpr",Just_a_concat(["StarredAnnotation";"AtomicLb";"Expression";"AtomicRb"]));
   ("DimExprs",Just_a_concat(["DimExpr";"StarredDimExpr"]));
   ("Dims",Just_a_concat(["DimsElement";"StarredDimsElement"]));
   ("DimsElement",Just_a_concat(["StarredAnnotation";"AtomicLb";"AtomicRb"]));
   ("DoStatement",Just_a_concat(["AtomicDo";"Statement";"AtomicWhile";"AtomicLp";"Expression";"AtomicRp";"AtomicSm"]));
   ("ElementValue",Just_a_disjunction(["ConditionalExpression";"ElementValueArrayInitializer";"Annotation"]));
   ("ElementValueArrayInitializer",Just_a_concat(["AtomicLb";"OptionalElementValueList";"OptionalCm";"AtomicRb"]));
   ("ElementValueList",Just_a_concat(["ElementValue";"StarredElementValuePrecededByComma"]));
   ("ElementValuePair",Just_a_concat(["AtomicIdentifier";"AtomicEq";"ElementValue"]));
   ("ElementValuePairList",Just_a_concat(["ElementValuePair";"StarredElementValuePairPrecededByComma"]));
   ("ElementValuePairPrecededByComma",Just_a_concat(["AtomicCm";"ElementValuePair"]));
   ("ElementValuePrecededByComma",Just_a_concat(["AtomicCm";"ElementValue"]));
   ("EmptyStatement",Synonym("AtomicSm"));
   ("EnhancedForStatement",Just_a_concat(["AtomicFor";"AtomicLp";"LocalVariableDeclaration";"AtomicColon";"Expression";"AtomicRp";"Statement"]));
   ("EnhancedForStatementNoShortIf",Just_a_concat(["AtomicFor";"AtomicLp";"LocalVariableDeclaration";"AtomicColon";"Expression";"AtomicRp";"StatementNoShortIf"]));
   ("EnumBody",Just_a_concat(["AtomicLb";"OptionalEnumConstantList";"OptionalCm";"OptionalEnumBodyDeclarations";"AtomicRb"]));
   ("EnumBodyDeclarations",Just_a_concat(["AtomicSm";"StarredClassBodyDeclaration"]));
   ("EnumConstant",Just_a_concat(["StarredEnumConstantModifier";"AtomicIdentifier";"OptionalParenthesedArgumentList";"OptionalClassBody"]));
   ("EnumConstantList",Just_a_concat(["EnumConstant";"StarredEnumConstantPrecededByComma"]));
   ("EnumConstantModifier",Synonym("Annotation"));
   ("EnumConstantPrecededByComma",Just_a_concat(["AtomicCm";"EnumConstant"]));
   ("EnumDeclaration",Just_a_concat(["StarredClassModifier";"AtomicEnum";"AtomicIdentifier";"OptionalClassImplements";"EnumBody"]));
   ("EqualityExpression",Disjunction([Concat(["RelationalExpression"]);Concat(["EqualityExpression";"AtomicEqEq";"RelationalExpression"]);Concat(["EqualityExpression";"AtomicNotEq";"RelationalExpression"])]));
   ("EqualsVariableInitializer",Just_a_concat(["AtomicEq";"VariableInitializer"]));
   ("ExceptionType",Just_a_disjunction(["ClassType";"TypeVariable"]));
   ("ExceptionTypeList",Just_a_concat(["ExceptionType";"StarredExceptionTypePrecededByComma"]));
   ("ExceptionTypePrecededByComma",Just_a_concat(["AtomicCm";"ExceptionType"]));
   ("ExclusiveOrExpression",Disjunction([Concat(["AndExpression"]);Concat(["ExclusiveOrExpression";"AtomicXor";"AndExpression"])]));
   ("ExplicitConstructorInvocation",Disjunction([Concat(["OptionalTypeArguments";"AtomicThis";"ParenthesedArgumentList";"AtomicSm"]);Concat(["OptionalTypeArguments";"AtomicSuper";"ParenthesedArgumentList";"AtomicSm"]);Concat(["ExpressionName";"AtomicDot";"OptionalTypeArguments";"AtomicSuper";"ParenthesedArgumentList";"AtomicSm"]);Concat(["Primary";"AtomicDot";"OptionalTypeArguments";"AtomicSuper";"ParenthesedArgumentList";"AtomicSm"])]));
   ("Expression",Just_a_disjunction(["LambdaExpression";"AssignmentExpression"]));
   ("ExpressionName",Disjunction([Concat(["Identifier"]);Concat(["AmbiguousName";"IdentifierPrecededByDot"])]));
   ("ExpressionPrecededByComma",Just_a_concat(["AtomicCm";"Expression"]));
   ("ExpressionStatement",Just_a_concat(["StatementExpression";"AtomicSm"]));
   ("FieldAccess",Disjunction([Concat(["Primary";"IdentifierPrecededByDot"]);Concat(["AtomicSuper";"IdentifierPrecededByDot"]);Concat(["TypeName";"AtomicDot";"AtomicSuper";"IdentifierPrecededByDot"])]));
   ("FieldDeclaration",Just_a_concat(["StarredFieldModifier";"UnannType";"VariableDeclaratorList";"AtomicSm"]));
   ("FieldModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicProtected";"AtomicPrivate";"AtomicStatic";"AtomicFinal";"AtomicTransient";"AtomicVolatile"]));
   ("Finally",Just_a_concat(["AtomicFinally";"Block"]));
   ("FloatingPointType",Just_a_disjunction(["AtomicFloat";"AtomicDouble"]));
   ("ForInit",Just_a_disjunction(["StatementExpressionList";"LocalVariableDeclaration"]));
   ("FormalParameter",Disjunction([Concat(["StarredVariableModifier";"UnannType";"VariableDeclaratorId"]);Concat(["VariableArityParameter"])]));
   ("FormalParameterList",Just_a_concat(["FormalParameter";"StarredFormalParameterPrecededByComma"]));
   ("FormalParameterPrecededByComma",Just_a_concat(["AtomicCm";"FormalParameter"]));
   ("ForStatement",Just_a_disjunction(["BasicForStatement";"EnhancedForStatement"]));
   ("ForStatementNoShortIf",Just_a_disjunction(["BasicForStatementNoShortIf";"EnhancedForStatementNoShortIf"]));
   ("ForUpdate",Synonym("StatementExpressionList"));
   ("IdentifierFollowedByDot",Just_a_concat(["AtomicIdentifier";"AtomicDot"]));
   ("IdentifierPrecededByComma",Just_a_concat(["AtomicCm";"AtomicIdentifier"]));
   ("IdentifierPrecededByDot",Just_a_concat(["AtomicDot";"AtomicIdentifier"]));
   ("IfThenElseStatement",Just_a_concat(["AtomicIf";"AtomicLp";"Expression";"AtomicRp";"StatementNoShortIf";"AtomicElse";"Statement"]));
   ("IfThenElseStatementNoShortIf",Just_a_concat(["AtomicIf";"AtomicLp";"Expression";"AtomicRp";"StatementNoShortIf";"AtomicElse";"StatementNoShortIf"]));
   ("IfThenStatement",Just_a_concat(["AtomicIf";"AtomicLp";"Expression";"AtomicRp";"Statement"]));
   ("ImportDeclaration",Just_a_disjunction(["SingleTypeImportDeclaration";"TypeImportOnDemandDeclaration";"SingleStaticImportDeclaration";"StaticImportOnDemandDeclaration"]));
   ("InclusiveOrExpression",Disjunction([Concat(["ExclusiveOrExpression"]);Concat(["InclusiveOrExpression";"AtomicOr";"ExclusiveOrExpression"])]));
   ("InstanceInitializer",Synonym("Block"));
   ("InstanceofExpression",Disjunction([Concat(["RelationalExpression";"AtomicInstanceof";"ReferenceType"]);Concat(["RelationalExpression";"AtomicInstanceof";"Pattern"])]));
   ("IntegralType",Just_a_disjunction(["AtomicByte";"AtomicShort";"AtomicInt";"AtomicLong";"AtomicChar"]));
   ("InterfaceBody",Just_a_concat(["AtomicLb";"StarredInterfaceMemberDeclaration";"AtomicRb"]));
   ("InterfaceDeclaration",Just_a_disjunction(["NormalInterfaceDeclaration";"AnnotationInterfaceDeclaration"]));
   ("InterfaceExtends",Just_a_concat(["AtomicExtends";"InterfaceTypeList"]));
   ("InterfaceMemberDeclaration",Just_a_disjunction(["ConstantDeclaration";"InterfaceMethodDeclaration";"ClassDeclaration";"InterfaceDeclaration";"AtomicSm"]));
   ("InterfaceMethodDeclaration",Just_a_concat(["StarredInterfaceMethodModifier";"MethodHeader";"MethodBody"]));
   ("InterfaceMethodModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicPrivate";"AtomicAbstract";"AtomicDefault";"AtomicStatic";"AtomicStrictfp"]));
   ("InterfaceModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicProtected";"AtomicPrivate";"AtomicAbstract";"AtomicStatic";"AtomicSealed";"AtomicNonsealed";"AtomicStrictfp"]));
   ("InterfacePermits",Just_a_concat(["AtomicPermits";"TypeName";"StarredTypeNamePrecededByComma"]));
   ("InterfaceType",Synonym("ClassType"));
   ("InterfaceTypeList",Just_a_concat(["InterfaceType";"StarredInterfaceTypePrecededByComma"]));
   ("InterfaceTypePrecededByComma",Just_a_concat(["AtomicCm";"InterfaceType"]));
   ("LabeledStatement",Just_a_concat(["AtomicIdentifier";"AtomicColon";"Statement"]));
   ("LabeledStatementNoShortIf",Just_a_concat(["AtomicIdentifier";"AtomicColon";"StatementNoShortIf"]));
   ("LambdaBody",Just_a_disjunction(["Expression";"Block"]));
   ("LambdaExpression",Just_a_concat(["LambdaParameters";"AtomicMinus";"AtomicGt";"LambdaBody"]));
   ("LambdaParameter",Disjunction([Concat(["StarredVariableModifier";"LambdaParameterType";"VariableDeclaratorId"]);Concat(["VariableArityParameter"])]));
   ("LambdaParameterList",Disjunction([Concat(["LambdaParameter";"StarredLambdaParameterPrecededByComma"]);Concat(["Identifier";"StarredIdentifierPrecededByComma"])]));
   ("LambdaParameterPrecededByComma",Just_a_concat(["AtomicCm";"LambdaParameter"]));
   ("LambdaParameters",Disjunction([Concat(["AtomicLp";"OptionalLambdaParameterList";"AtomicRp"]);Concat(["Identifier"])]));
   ("LambdaParameterType",Just_a_disjunction(["UnannType";"AtomicVar"]));
   ("LeftHandSide",Just_a_disjunction(["ExpressionName";"FieldAccess";"ArrayAccess"]));
   ("Literal",Just_a_disjunction(["IntegerLiteral";"FloatingPointLiteral";"BooleanLiteral";"CharacterLiteral";"StringLiteral";"TextBlock";"NullLiteral"]));
   ("LocalClassOrInterfaceDeclaration",Just_a_disjunction(["ClassDeclaration";"NormalInterfaceDeclaration"]));
   ("LocalVariableDeclaration",Just_a_concat(["StarredVariableModifier";"LocalVariableType";"VariableDeclaratorList"]));
   ("LocalVariableDeclarationStatement",Just_a_concat(["LocalVariableDeclaration";"AtomicSm"]));
   ("LocalVariableType",Just_a_disjunction(["UnannType";"AtomicVar"]));
   ("MarkerAnnotation",Just_a_concat(["AtomicSnail";"TypeName"]));
   ("MethodBody",Just_a_disjunction(["Block";"AtomicSm"]));
   ("MethodDeclaration",Just_a_concat(["StarredMethodModifier";"MethodHeader";"MethodBody"]));
   ("MethodDeclarator",Just_a_concat(["AtomicIdentifier";"AtomicLp";"OptionalReceiverParameterFollowedByComma";"OptionalFormalParameterList";"AtomicRp";"OptionalDims"]));
   ("MethodHeader",Disjunction([Concat(["Result";"MethodDeclarator";"OptionalThrows"]);Concat(["TypeParameters";"StarredAnnotation";"Result";"MethodDeclarator";"OptionalThrows"])]));
   ("MethodInvocation",Disjunction([Concat(["MethodName";"ParenthesedArgumentList"]);Concat(["TypeName";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"]);Concat(["ExpressionName";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"]);Concat(["Primary";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"]);Concat(["AtomicSuper";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"]);Concat(["TypeName";"AtomicDot";"AtomicSuper";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"])]));
   ("MethodModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicProtected";"AtomicPrivate";"AtomicAbstract";"AtomicStatic";"AtomicFinal";"AtomicSynchronized";"AtomicNative";"AtomicStrictfp"]));
   ("MethodName",Synonym("AtomicIdentifier"));
   ("MethodReference",Disjunction([Concat(["ExpressionName";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["Primary";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["ReferenceType";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["AtomicSuper";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["TypeName";"AtomicDot";"AtomicSuper";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["ClassType";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"AtomicNew"]);Concat(["ArrayType";"AtomicColon";"AtomicColon";"AtomicNew"])]));
   ("ModularCompilationUnit",Just_a_concat(["StarredImportDeclaration";"ModuleDeclaration"]));
   ("ModuleDeclaration",Just_a_concat(["StarredAnnotation";"OptionalOpen";"AtomicModule";"AtomicIdentifier";"StarredIdentifierPrecededByDot";"AtomicLb";"StarredModuleDirective";"AtomicRb"]));
   ("ModuleDirective",Disjunction([Concat(["AtomicRequires";"StarredRequiresModifier";"ModuleName";"AtomicSm"]);Concat(["AtomicExports";"PackageName";"OptionalToModuleList";"AtomicSm"]);Concat(["AtomicOpens";"PackageName";"OptionalToModuleList";"AtomicSm"]);Concat(["AtomicUses";"TypeName";"AtomicSm"]);Concat(["AtomicProvides";"TypeName";"AtomicWith";"TypeName";"StarredTypeNamePrecededByComma";"AtomicSm"])]));
   ("ModuleName",Disjunction([Concat(["Identifier"]);Concat(["ModuleName";"IdentifierPrecededByDot"])]));
   ("ModuleNamePrecededByComma",Just_a_concat(["AtomicCm";"ModuleName"]));
   ("MultiplicativeExpression",Disjunction([Concat(["UnaryExpression"]);Concat(["MultiplicativeExpression";"AtomicTimes";"UnaryExpression"]);Concat(["MultiplicativeExpression";"AtomicDiv";"UnaryExpression"]);Concat(["MultiplicativeExpression";"AtomicMod";"UnaryExpression"])]));
   ("NormalAnnotation",Just_a_concat(["AtomicSnail";"TypeName";"AtomicLp";"OptionalElementValuePairList";"AtomicRp"]));
   ("NormalClassDeclaration",Just_a_concat(["StarredClassModifier";"AtomicClass";"AtomicIdentifier";"OptionalTypeParameters";"OptionalClassExtends";"OptionalClassImplements";"OptionalClassPermits";"ClassBody"]));
   ("NormalInterfaceDeclaration",Just_a_concat(["StarredInterfaceModifier";"AtomicInterface";"AtomicIdentifier";"OptionalTypeParameters";"OptionalInterfaceExtends";"OptionalInterfacePermits";"InterfaceBody"]));
   ("NumericType",Just_a_disjunction(["IntegralType";"FloatingPointType"]));
   ("OpenSquare",Just_a_concat(["AtomicLb";"AtomicRb"]));
   ("OrdinaryCompilationUnit",Just_a_concat(["OptionalPackageDeclaration";"StarredImportDeclaration";"StarredTopLevelClassOrInterfaceDeclaration"]));
   ("PackageDeclaration",Just_a_concat(["StarredPackageModifier";"AtomicPackage";"AtomicIdentifier";"StarredIdentifierPrecededByDot";"AtomicSm"]));
   ("PackageModifier",Synonym("Annotation"));
   ("PackageName",Disjunction([Concat(["Identifier"]);Concat(["PackageName";"IdentifierPrecededByDot"])]));
   ("PackageOrTypeName",Disjunction([Concat(["Identifier"]);Concat(["PackageOrTypeName";"IdentifierPrecededByDot"])]));
   ("ParenthesedArgumentList",Just_a_concat(["AtomicLp";"OptionalArgumentList";"AtomicRp"]));
   ("Pattern",Synonym("TypePattern"));
   ("PostDecrementExpression",Just_a_concat(["PostfixExpression";"AtomicDecr"]));
   ("PostfixExpression",Just_a_disjunction(["Primary";"ExpressionName";"PostIncrementExpression";"PostDecrementExpression"]));
   ("PostIncrementExpression",Just_a_concat(["PostfixExpression";"AtomicIncr"]));
   ("PreDecrementExpression",Just_a_concat(["AtomicDecr";"UnaryExpression"]));
   ("PreIncrementExpression",Just_a_concat(["AtomicIncr";"UnaryExpression"]));
   ("Primary",Just_a_disjunction(["PrimaryNoNewArray";"ArrayCreationExpression"]));
   ("PrimaryNoNewArray",Disjunction([Concat(["Literal"]);Concat(["ClassLiteral"]);Concat(["AtomicThis"]);Concat(["TypeName";"AtomicDot";"AtomicThis"]);Concat(["AtomicLp";"Expression";"AtomicRp"]);Concat(["ClassInstanceCreationExpression"]);Concat(["FieldAccess"]);Concat(["ArrayAccess"]);Concat(["MethodInvocation"]);Concat(["Methodref_in_diserence"])]));
   ("PrimitiveType",Disjunction([Concat(["StarredAnnotation";"NumericType"]);Concat(["StarredAnnotation";"AtomicBoolean"])]));
   ("ReceiverParameter",Just_a_concat(["StarredAnnotation";"UnannType";"OptionalIdentifierFollowedByDot";"AtomicThis"]));
   ("ReceiverParameterFollowedByComma",Just_a_concat(["ReceiverParameter";"AtomicCm"]));
   ("RecordBody",Just_a_concat(["AtomicLb";"StarredRecordBodyDeclaration";"AtomicRb"]));
   ("RecordBodyDeclaration",Just_a_disjunction(["ClassBodyDeclaration";"CompactConstructorDeclaration"]));
   ("RecordComponent",Disjunction([Concat(["StarredRecordComponentModifier";"UnannType";"Identifier"]);Concat(["VariableArityRecordComponent"])]));
   ("RecordComponentList",Just_a_concat(["RecordComponent";"StarredRecordComponentPrecededByComma"]));
   ("RecordComponentModifier",Synonym("Annotation"));
   ("RecordComponentPrecededByComma",Just_a_concat(["AtomicCm";"RecordComponent"]));
   ("RecordDeclaration",Just_a_concat(["StarredClassModifier";"AtomicRecord";"AtomicIdentifier";"OptionalTypeParameters";"RecordHeader";"OptionalClassImplements";"RecordBody"]));
   ("RecordHeader",Just_a_concat(["AtomicLp";"OptionalRecordComponentList";"AtomicRp"]));
   ("ReferenceType",Just_a_disjunction(["ClassOrInterfaceType";"TypeVariable";"ArrayType"]));
   ("RelationalExpression",Disjunction([Concat(["ShiftExpression"]);Concat(["RelationalExpression";"AtomicLt";"ShiftExpression"]);Concat(["RelationalExpression";"AtomicGt";"ShiftExpression"]);Concat(["RelationalExpression";"AtomicLe";"ShiftExpression"]);Concat(["RelationalExpression";"AtomicGe";"ShiftExpression"]);Concat(["InstanceofExpression"])]));
   ("RequiresModifier",Just_a_disjunction(["AtomicTransitive";"AtomicStatic"]));
   ("Resource",Just_a_disjunction(["LocalVariableDeclaration";"VariableAccess"]));
   ("ResourceList",Just_a_concat(["Resource";"StarredResourcePrecededBySemiColon"]));
   ("ResourcePrecededByComma",Just_a_concat(["AtomicCm";"Resource"]));
   ("ResourcePrecededBySemiColon",Just_a_concat(["AtomicSm";"Resource"]));
   ("ResourceSpecification",Just_a_concat(["AtomicLp";"ResourceList";"OptionalSm";"AtomicRp"]));
   ("Result",Just_a_disjunction(["UnannType";"AtomicVoid"]));
   ("ReturnStatement",Just_a_concat(["AtomicReturn";"OptionalExpression";"AtomicSm"]));
   ("ShiftExpression",Disjunction([Concat(["AdditiveExpression"]);Concat(["ShiftExpression";"AtomicLs";"AdditiveExpression"]);Concat(["ShiftExpression";"AtomicSrs";"AdditiveExpression"]);Concat(["ShiftExpression";"AtomicUrs";"AdditiveExpression"])]));
   ("SimpleTypeName",Synonym("AtomicIdentifier"));
   ("SingleElementAnnotation",Just_a_concat(["AtomicSnail";"TypeName";"AtomicLp";"ElementValue";"AtomicRp"]));
   ("SingleStaticImportDeclaration",Just_a_concat(["AtomicImport";"AtomicStatic";"TypeName";"IdentifierPrecededByDot";"AtomicSm"]));
   ("SingleTypeImportDeclaration",Just_a_concat(["AtomicImport";"TypeName";"AtomicSm"]));
   ("Statement",Just_a_disjunction(["StatementWithoutTrailingSubstatement";"LabeledStatement";"IfThenStatement";"IfThenElseStatement";"WhileStatement";"ForStatement"]));
   ("StatementExpression",Just_a_disjunction(["Assignment";"PreIncrementExpression";"PreDecrementExpression";"PostIncrementExpression";"PostDecrementExpression";"MethodInvocation";"ClassInstanceCreationExpression"]));
   ("StatementExpressionList",Just_a_concat(["StatementExpression";"StarredStatementExpressionPrecededByComma"]));
   ("StatementExpressionPrecededByComma",Just_a_concat(["AtomicCm";"StatementExpression"]));
   ("StatementNoShortIf",Just_a_disjunction(["StatementWithoutTrailingSubstatement";"LabeledStatementNoShortIf";"IfThenElseStatementNoShortIf";"WhileStatementNoShortIf";"ForStatementNoShortIf"]));
   ("StatementWithoutTrailingSubstatement",Just_a_disjunction(["Block";"EmptyStatement";"ExpressionStatement";"AssertStatement";"SwitchStatement";"DoStatement";"BreakStatement";"ContinueStatement";"ReturnStatement";"SynchronizedStatement";"ThrowStatement";"TryStatement";"YieldStatement"]));
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
   ("TopLevelClassOrInterfaceDeclaration",Just_a_disjunction(["ClassDeclaration";"InterfaceDeclaration";"AtomicSm"]));
   ("TryStatement",Disjunction([Concat(["AtomicTry";"Block";"Catches"]);Concat(["AtomicTry";"Block";"OptionalCatches";"Finally"]);Concat(["TryWithResourcesStatement"])]));
   ("TryWithResourcesStatement",Just_a_concat(["AtomicTry";"ResourceSpecification";"Block";"OptionalCatches";"OptionalFinally"]));
   ("Type",Just_a_disjunction(["PrimitiveType";"ref_in_diserenceType"]));
   ("TypeArgument",Just_a_disjunction(["ref_in_diserenceType";"Wildcard"]));
   ("TypeArgumentList",Just_a_concat(["TypeArgument";"StarredTypeArgumentPrecededByComma"]));
   ("TypeArgumentPrecededByComma",Just_a_concat(["AtomicCm";"TypeArgument"]));
   ("TypeArguments",Just_a_concat(["AtomicLt";"TypeArgumentList";"AtomicGt"]));
   ("TypeArgumentsOrDiamond",Disjunction([Concat(["TypeArguments"]);Concat(["AtomicLt";"AtomicGt"])]));
   ("TypeBound",Disjunction([Concat(["AtomicExtends";"TypeVariable"]);Concat(["AtomicExtends";"ClassOrInterfaceType";"StarredAdditionalBound"])]));
   ("TypeImportOnDemandDeclaration",Just_a_concat(["AtomicImport";"PackageOrTypeName";"AtomicDot";"AtomicTimes";"AtomicSm"]));
   ("TypeName",Disjunction([Concat(["TypeIdentifier"]);Concat(["PackageOrTypeName";"AtomicDot";"TypeIdentifier"])]));
   ("TypeNamePrecededByComma",Just_a_concat(["AtomicCm";"TypeName"]));
   ("TypeParameter",Just_a_concat(["StarredTypeParameterModifier";"AtomicIdentifier";"OptionalTypeBound"]));
   ("TypeParameterList",Just_a_concat(["TypeParameter";"StarredTypeParameterPrecededByComma"]));
   ("TypeParameterModifier",Synonym("Annotation"));
   ("TypeParameterPrecededByComma",Just_a_concat(["AtomicCm";"TypeParameter"]));
   ("TypeParameters",Just_a_concat(["AtomicLt";"TypeParameterList";"AtomicGt"]));
   ("TypePattern",Synonym("LocalVariableDeclaration"));
   ("TypeVariable",Just_a_concat(["StarredAnnotation";"AtomicIdentifier"]));
   ("UnannArrayType",Disjunction([Concat(["UnannPrimitiveType";"Dims"]);Concat(["UnannClassOrInterfaceType";"Dims"]);Concat(["UnannTypeVariable";"Dims"])]));
   ("UnannClassOrInterfaceType",Just_a_disjunction(["UnannClassType";"UnannInterfaceType"]));
   ("UnannClassType",Disjunction([Concat(["TypeIdentifier";"OptionalTypeArguments"]);Concat(["PackageName";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"]);Concat(["UnannClassOrInterfaceType";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"])]));
   ("UnannInterfaceType",Synonym("UnannClassType"));
   ("UnannPrimitiveType",Just_a_disjunction(["NumericType";"AtomicBoolean"]));
   ("UnannReferenceType",Just_a_disjunction(["UnannClassOrInterfaceType";"UnannTypeVariable";"UnannArrayType"]));
   ("UnannType",Just_a_disjunction(["UnannPrimitiveType";"Unannref_in_diserenceType"]));
   ("UnannTypeVariable",Synonym("AtomicIdentifier"));
   ("UnaryExpression",Disjunction([Concat(["PreIncrementExpression"]);Concat(["PreDecrementExpression"]);Concat(["AtomicPlus";"UnaryExpression"]);Concat(["AtomicMinus";"UnaryExpression"]);Concat(["UnaryExpressionNotPlusMinus"])]));
   ("UnaryExpressionNotPlusMinus",Disjunction([Concat(["PostfixExpression"]);Concat(["AtomicCompl";"UnaryExpression"]);Concat(["AtomicNot";"UnaryExpression"]);Concat(["CastExpression"]);Concat(["SwitchExpression"])]));
   ("UnqualifiedClassInstanceCreationExpression",Just_a_concat(["AtomicNew";"OptionalTypeArguments";"ClassOrInterfaceTypeToInstantiate";"ParenthesedArgumentList";"OptionalClassBody"]));
   ("VariableArityParameter",Just_a_concat(["StarredVariableModifier";"UnannType";"StarredAnnotation";"AtomicDot";"AtomicDot";"AtomicDot";"AtomicIdentifier"]));
   ("VariableArityRecordComponent",Just_a_concat(["StarredRecordComponentModifier";"UnannType";"StarredAnnotation";"AtomicDot";"AtomicDot";"AtomicDot";"AtomicIdentifier"]));
   ("VariableDeclarator",Just_a_concat(["VariableDeclaratorId";"OptionalEqualsVariableInitializer"]));
   ("VariableDeclaratorId",Just_a_concat(["AtomicIdentifier";"OptionalDims"]));
   ("VariableDeclaratorList",Just_a_concat(["VariableDeclarator";"StarredVariableDeclaratorPrecededByComma"]));
   ("VariableDeclaratorPrecededByComma",Just_a_concat(["AtomicCm";"VariableDeclarator"]));
   ("VariableInitializer",Just_a_disjunction(["Expression";"ArrayInitializer"]));
   ("VariableInitializerList",Just_a_concat(["VariableInitializer";"StarredVariableInitializerPrecededByComma"]));
   ("VariableInitializerPrecededByComma",Just_a_concat(["AtomicCm";"VariableInitializer"]));
   ("VariableModifier",Just_a_disjunction(["Annotation";"AtomicFinal"]));
   ("WhileStatement",Just_a_concat(["AtomicWhile";"AtomicLp";"Expression";"AtomicRp";"Statement"]));
   ("WhileStatementNoShortIf",Just_a_concat(["AtomicWhile";"AtomicLp";"Expression";"AtomicRp";"StatementNoShortIf"]));
   ("Wildcard",Just_a_concat(["StarredAnnotation";"AtomicCond";"OptionalWildcardBounds"]));
   ("WildcardBounds",Disjunction([Concat(["AtomicExtends";"ReferenceType"]);Concat(["AtomicSuper";"ReferenceType"])]));
   ("YieldStatement",Just_a_concat(["AtomicYield";"Expression";"AtomicSm"]));

]);;


(* Java grammar ends here *)

 end ;;
let java_grammar = Private.java_grammar ;;
