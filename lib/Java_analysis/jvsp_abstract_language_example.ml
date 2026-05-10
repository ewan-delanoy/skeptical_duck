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

   ("AdditiveExpression",Disjunction([Concat(["MultiplicativeExpression"]);Concat(["AdditiveExpression";"AtomicPlus";"MultiplicativeExpression"]);Concat(["AdditiveExpression";"AtomicMinus";"MultiplicativeExpression"])]));
   ("AmbiguousName",Disjunction([Concat(["Identifier"]);Concat(["AmbiguousName";"IdentifierPrecededByDot"])]));
   ("AndExpression",Disjunction([Concat(["EqualityExpression"]);Concat(["AndExpression";"AtomicAnd";"EqualityExpression"])]));
   ("Annotation",Just_a_disjunction(["NormalAnnotation";"MarkerAnnotation";"SingleElementAnnotation"]));
   ("AnnotationInterfaceElementModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicAbstract"]));
   ("AnnotationInterfaceMemberDeclaration",Just_a_disjunction(["AnnotationInterfaceElementDeclaration";"ConstantDeclaration";"ClassDeclaration";"InterfaceDeclaration";"AtomicSm"]));
   ("ArrayAccess",Disjunction([Concat(["ExpressionName";"AtomicLb";"Expression";"AtomicRb"]);Concat(["PrimaryNoNewArray";"AtomicLb";"Expression";"AtomicRb"])]));
   ("ArrayCreationExpression",Disjunction([Concat(["AtomicNew";"PrimitiveType";"DimExprs";"OptionalDims"]);Concat(["AtomicNew";"ClassOrInterfaceType";"DimExprs";"OptionalDims"]);Concat(["AtomicNew";"PrimitiveType";"Dims";"ArrayInitializer"]);Concat(["AtomicNew";"ClassOrInterfaceType";"Dims";"ArrayInitializer"])]));
   ("ArrayType",Disjunction([Concat(["PrimitiveType";"Dims"]);Concat(["ClassOrInterfaceType";"Dims"]);Concat(["TypeVariable";"Dims"])]));
   ("AssertStatement",Disjunction([Concat(["AtomicAssert";"Expression";"AtomicSm"]);Concat(["AtomicAssert";"Expression";"AtomicColon";"Expression";"AtomicSm"])]));
   ("AssignmentExpression",Just_a_disjunction(["ConditionalExpression";"Assignment"]));
   ("BlockStatement",Just_a_disjunction(["LocalClassOrInterfaceDeclaration";"LocalVariableDeclarationStatement";"Statement"]));
   ("CastExpression",Disjunction([Concat(["AtomicLp";"PrimitiveType";"AtomicRp";"UnaryExpression"]);Concat(["AtomicLp";"ReferenceType";"StarredAdditionalBound";"AtomicRp";"UnaryExpressionNotPlusMinus"]);Concat(["AtomicLp";"ReferenceType";"StarredAdditionalBound";"AtomicRp";"LambdaExpression"])]));
   ("ClassBodyDeclaration",Just_a_disjunction(["ClassMemberDeclaration";"InstanceInitializer";"StaticInitializer";"ConstructorDeclaration"]));
   ("ClassDeclaration",Just_a_disjunction(["NormalClassDeclaration";"EnumDeclaration";"RecordDeclaration"]));
   ("ClassInstanceCreationExpression",Disjunction([Concat(["UnqualifiedClassInstanceCreationExpression"]);Concat(["ExpressionName";"AtomicDot";"UnqualifiedClassInstanceCreationExpression"]);Concat(["Primary";"AtomicDot";"UnqualifiedClassInstanceCreationExpression"])]));
   ("ClassLiteral",Disjunction([Concat(["TypeName";"StarredOpenSquare";"AtomicDot";"AtomicClass"]);Concat(["NumericType";"StarredOpenSquare";"AtomicDot";"AtomicClass"]);Concat(["AtomicBoolean";"StarredOpenSquare";"AtomicDot";"AtomicClass"]);Concat(["AtomicVoid";"AtomicDot";"AtomicClass"])]));
   ("ClassMemberDeclaration",Just_a_disjunction(["FieldDeclaration";"MethodDeclaration";"ClassDeclaration";"InterfaceDeclaration";"AtomicSm"]));
   ("ClassModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicProtected";"AtomicPrivate";"AtomicAbstract";"AtomicStatic";"AtomicFinal";"AtomicSealed";"AtomicNonsealed";"AtomicStrictfp"]));
   ("ClassOrInterfaceType",Just_a_disjunction(["ClassType";"InterfaceType"]));
   ("ClassType",Disjunction([Concat(["StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"]);Concat(["PackageName";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"]);Concat(["ClassOrInterfaceType";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"])]));
   ("CompilationUnit",Just_a_disjunction(["OrdinaryCompilationUnit";"ModularCompilationUnit"]));
   ("ConditionalAndExpression",Disjunction([Concat(["InclusiveOrExpression"]);Concat(["ConditionalAndExpression";"AtomicAndAnd";"InclusiveOrExpression"])]));
   ("ConditionalExpression",Disjunction([Concat(["ConditionalOrExpression"]);Concat(["ConditionalOrExpression";"AtomicCond";"Expression";"AtomicColon";"ConditionalExpression"]);Concat(["ConditionalOrExpression";"AtomicCond";"Expression";"AtomicColon";"LambdaExpression"])]));
   ("ConditionalOrExpression",Disjunction([Concat(["ConditionalAndExpression"]);Concat(["ConditionalOrExpression";"AtomicOrOr";"ConditionalAndExpression"])]));
   ("ConstantModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicStatic";"AtomicFinal"]));
   ("ConstructorModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicProtected";"AtomicPrivate"]));
   ("ElementValue",Just_a_disjunction(["ConditionalExpression";"ElementValueArrayInitializer";"Annotation"]));
   ("EqualityExpression",Disjunction([Concat(["RelationalExpression"]);Concat(["EqualityExpression";"AtomicEqEq";"RelationalExpression"]);Concat(["EqualityExpression";"AtomicNotEq";"RelationalExpression"])]));
   ("ExceptionType",Just_a_disjunction(["ClassType";"TypeVariable"]));
   ("ExclusiveOrExpression",Disjunction([Concat(["AndExpression"]);Concat(["ExclusiveOrExpression";"AtomicXor";"AndExpression"])]));
   ("ExplicitConstructorInvocation",Disjunction([Concat(["OptionalTypeArguments";"AtomicThis";"ParenthesedArgumentList";"AtomicSm"]);Concat(["OptionalTypeArguments";"AtomicSuper";"ParenthesedArgumentList";"AtomicSm"]);Concat(["ExpressionName";"AtomicDot";"OptionalTypeArguments";"AtomicSuper";"ParenthesedArgumentList";"AtomicSm"]);Concat(["Primary";"AtomicDot";"OptionalTypeArguments";"AtomicSuper";"ParenthesedArgumentList";"AtomicSm"])]));
   ("Expression",Just_a_disjunction(["LambdaExpression";"AssignmentExpression"]));
   ("ExpressionName",Disjunction([Concat(["Identifier"]);Concat(["AmbiguousName";"IdentifierPrecededByDot"])]));
   ("FieldAccess",Disjunction([Concat(["Primary";"IdentifierPrecededByDot"]);Concat(["AtomicSuper";"IdentifierPrecededByDot"]);Concat(["TypeName";"AtomicDot";"AtomicSuper";"IdentifierPrecededByDot"])]));
   ("FieldModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicProtected";"AtomicPrivate";"AtomicStatic";"AtomicFinal";"AtomicTransient";"AtomicVolatile"]));
   ("FloatingPointType",Just_a_disjunction(["AtomicFloat";"AtomicDouble"]));
   ("ForInit",Just_a_disjunction(["StatementExpressionList";"LocalVariableDeclaration"]));
   ("FormalParameter",Disjunction([Concat(["StarredVariableModifier";"UnannType";"VariableDeclaratorId"]);Concat(["VariableArityParameter"])]));
   ("ForStatement",Just_a_disjunction(["BasicForStatement";"EnhancedForStatement"]));
   ("ForStatementNoShortIf",Just_a_disjunction(["BasicForStatementNoShortIf";"EnhancedForStatementNoShortIf"]));
   ("ImportDeclaration",Just_a_disjunction(["SingleTypeImportDeclaration";"TypeImportOnDemandDeclaration";"SingleStaticImportDeclaration";"StaticImportOnDemandDeclaration"]));
   ("InclusiveOrExpression",Disjunction([Concat(["ExclusiveOrExpression"]);Concat(["InclusiveOrExpression";"AtomicOr";"ExclusiveOrExpression"])]));
   ("InstanceofExpression",Disjunction([Concat(["RelationalExpression";"AtomicInstanceof";"ReferenceType"]);Concat(["RelationalExpression";"AtomicInstanceof";"Pattern"])]));
   ("IntegralType",Just_a_disjunction(["AtomicByte";"AtomicShort";"AtomicInt";"AtomicLong";"AtomicChar"]));
   ("InterfaceDeclaration",Just_a_disjunction(["NormalInterfaceDeclaration";"AnnotationInterfaceDeclaration"]));
   ("InterfaceMemberDeclaration",Just_a_disjunction(["ConstantDeclaration";"InterfaceMethodDeclaration";"ClassDeclaration";"InterfaceDeclaration";"AtomicSm"]));
   ("InterfaceMethodModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicPrivate";"AtomicAbstract";"AtomicDefault";"AtomicStatic";"AtomicStrictfp"]));
   ("InterfaceModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicProtected";"AtomicPrivate";"AtomicAbstract";"AtomicStatic";"AtomicSealed";"AtomicNonsealed";"AtomicStrictfp"]));
   ("LambdaBody",Just_a_disjunction(["Expression";"Block"]));
   ("LambdaParameter",Disjunction([Concat(["StarredVariableModifier";"LambdaParameterType";"VariableDeclaratorId"]);Concat(["VariableArityParameter"])]));
   ("LambdaParameterList",Disjunction([Concat(["LambdaParameter";"StarredLambdaParameterPrecededByComma"]);Concat(["Identifier";"StarredIdentifierPrecededByComma"])]));
   ("LambdaParameters",Disjunction([Concat(["AtomicLp";"OptionalLambdaParameterList";"AtomicRp"]);Concat(["Identifier"])]));
   ("LambdaParameterType",Just_a_disjunction(["UnannType";"AtomicVar"]));
   ("LeftHandSide",Just_a_disjunction(["ExpressionName";"FieldAccess";"ArrayAccess"]));
   ("Literal",Just_a_disjunction(["IntegerLiteral";"FloatingPointLiteral";"BooleanLiteral";"CharacterLiteral";"StringLiteral";"TextBlock";"NullLiteral"]));
   ("LocalClassOrInterfaceDeclaration",Just_a_disjunction(["ClassDeclaration";"NormalInterfaceDeclaration"]));
   ("LocalVariableType",Just_a_disjunction(["UnannType";"AtomicVar"]));
   ("MethodBody",Just_a_disjunction(["Block";"AtomicSm"]));
   ("MethodHeader",Disjunction([Concat(["Result";"MethodDeclarator";"OptionalThrows"]);Concat(["TypeParameters";"StarredAnnotation";"Result";"MethodDeclarator";"OptionalThrows"])]));
   ("MethodInvocation",Disjunction([Concat(["MethodName";"ParenthesedArgumentList"]);Concat(["TypeName";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"]);Concat(["ExpressionName";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"]);Concat(["Primary";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"]);Concat(["AtomicSuper";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"]);Concat(["TypeName";"AtomicDot";"AtomicSuper";"AtomicDot";"OptionalTypeArguments";"Identifier";"ParenthesedArgumentList"])]));
   ("MethodModifier",Just_a_disjunction(["Annotation";"AtomicPublic";"AtomicProtected";"AtomicPrivate";"AtomicAbstract";"AtomicStatic";"AtomicFinal";"AtomicSynchronized";"AtomicNative";"AtomicStrictfp"]));
   ("MethodReference",Disjunction([Concat(["ExpressionName";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["Primary";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["ReferenceType";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["AtomicSuper";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["TypeName";"AtomicDot";"AtomicSuper";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"Identifier"]);Concat(["ClassType";"AtomicColon";"AtomicColon";"OptionalTypeArguments";"AtomicNew"]);Concat(["ArrayType";"AtomicColon";"AtomicColon";"AtomicNew"])]));
   ("ModuleDirective",Disjunction([Concat(["AtomicRequires";"StarredRequiresModifier";"ModuleName";"AtomicSm"]);Concat(["AtomicExports";"PackageName";"OptionalToModuleList";"AtomicSm"]);Concat(["AtomicOpens";"PackageName";"OptionalToModuleList";"AtomicSm"]);Concat(["AtomicUses";"TypeName";"AtomicSm"]);Concat(["AtomicProvides";"TypeName";"AtomicWith";"TypeName";"StarredTypeNamePrecededByComma";"AtomicSm"])]));
   ("ModuleName",Disjunction([Concat(["Identifier"]);Concat(["ModuleName";"IdentifierPrecededByDot"])]));
   ("MultiplicativeExpression",Disjunction([Concat(["UnaryExpression"]);Concat(["MultiplicativeExpression";"AtomicTimes";"UnaryExpression"]);Concat(["MultiplicativeExpression";"AtomicDiv";"UnaryExpression"]);Concat(["MultiplicativeExpression";"AtomicMod";"UnaryExpression"])]));
   ("NumericType",Just_a_disjunction(["IntegralType";"FloatingPointType"]));
   ("PackageName",Disjunction([Concat(["Identifier"]);Concat(["PackageName";"IdentifierPrecededByDot"])]));
   ("PackageOrTypeName",Disjunction([Concat(["Identifier"]);Concat(["PackageOrTypeName";"IdentifierPrecededByDot"])]));
   ("PostfixExpression",Just_a_disjunction(["Primary";"ExpressionName";"PostIncrementExpression";"PostDecrementExpression"]));
   ("Primary",Just_a_disjunction(["PrimaryNoNewArray";"ArrayCreationExpression"]));
   ("PrimaryNoNewArray",Disjunction([Concat(["Literal"]);Concat(["ClassLiteral"]);Concat(["AtomicThis"]);Concat(["TypeName";"AtomicDot";"AtomicThis"]);Concat(["AtomicLp";"Expression";"AtomicRp"]);Concat(["ClassInstanceCreationExpression"]);Concat(["FieldAccess"]);Concat(["ArrayAccess"]);Concat(["MethodInvocation"]);Concat(["Methodref_in_diserence"])]));
   ("PrimitiveType",Disjunction([Concat(["StarredAnnotation";"NumericType"]);Concat(["StarredAnnotation";"AtomicBoolean"])]));
   ("RecordBodyDeclaration",Just_a_disjunction(["ClassBodyDeclaration";"CompactConstructorDeclaration"]));
   ("RecordComponent",Disjunction([Concat(["StarredRecordComponentModifier";"UnannType";"Identifier"]);Concat(["VariableArityRecordComponent"])]));
   ("ReferenceType",Just_a_disjunction(["ClassOrInterfaceType";"TypeVariable";"ArrayType"]));
   ("RelationalExpression",Disjunction([Concat(["ShiftExpression"]);Concat(["RelationalExpression";"AtomicLt";"ShiftExpression"]);Concat(["RelationalExpression";"AtomicGt";"ShiftExpression"]);Concat(["RelationalExpression";"AtomicLe";"ShiftExpression"]);Concat(["RelationalExpression";"AtomicGe";"ShiftExpression"]);Concat(["InstanceofExpression"])]));
   ("RequiresModifier",Just_a_disjunction(["AtomicTransitive";"AtomicStatic"]));
   ("Resource",Just_a_disjunction(["LocalVariableDeclaration";"VariableAccess"]));
   ("Result",Just_a_disjunction(["UnannType";"AtomicVoid"]));
   ("ShiftExpression",Disjunction([Concat(["AdditiveExpression"]);Concat(["ShiftExpression";"AtomicLs";"AdditiveExpression"]);Concat(["ShiftExpression";"AtomicSrs";"AdditiveExpression"]);Concat(["ShiftExpression";"AtomicUrs";"AdditiveExpression"])]));
   ("Statement",Just_a_disjunction(["StatementWithoutTrailingSubstatement";"LabeledStatement";"IfThenStatement";"IfThenElseStatement";"WhileStatement";"ForStatement"]));
   ("StatementExpression",Just_a_disjunction(["Assignment";"PreIncrementExpression";"PreDecrementExpression";"PostIncrementExpression";"PostDecrementExpression";"MethodInvocation";"ClassInstanceCreationExpression"]));
   ("StatementNoShortIf",Just_a_disjunction(["StatementWithoutTrailingSubstatement";"LabeledStatementNoShortIf";"IfThenElseStatementNoShortIf";"WhileStatementNoShortIf";"ForStatementNoShortIf"]));
   ("StatementWithoutTrailingSubstatement",Just_a_disjunction(["Block";"EmptyStatement";"ExpressionStatement";"AssertStatement";"SwitchStatement";"DoStatement";"BreakStatement";"ContinueStatement";"ReturnStatement";"SynchronizedStatement";"ThrowStatement";"TryStatement";"YieldStatement"]));
   ("SwitchBlock",Disjunction([Concat(["AtomicLc";"SwitchRule";"StarredSwitchRule";"AtomicRc"]);Concat(["AtomicLc";"StarredSwitchBlockStatementGroup";"StarredSwitchLabelFollowedByColon";"AtomicRc"])]));
   ("SwitchLabel",Disjunction([Concat(["AtomicCase";"CaseConstant";"StarredCaseConstantPrecededByComma"]);Concat(["AtomicDefault"])]));
   ("SwitchRule",Disjunction([Concat(["SwitchLabel";"AtomicMinus";"AtomicGt";"Expression";"AtomicSm"]);Concat(["SwitchLabel";"AtomicMinus";"AtomicGt";"Block"]);Concat(["SwitchLabel";"AtomicMinus";"AtomicGt";"ThrowStatement"])]));
   ("TopLevelClassOrInterfaceDeclaration",Just_a_disjunction(["ClassDeclaration";"InterfaceDeclaration";"AtomicSm"]));
   ("TryStatement",Disjunction([Concat(["AtomicTry";"Block";"Catches"]);Concat(["AtomicTry";"Block";"OptionalCatches";"Finally"]);Concat(["TryWithResourcesStatement"])]));
   ("Type",Just_a_disjunction(["PrimitiveType";"ref_in_diserenceType"]));
   ("TypeArgument",Just_a_disjunction(["ref_in_diserenceType";"Wildcard"]));
   ("TypeArgumentsOrDiamond",Disjunction([Concat(["TypeArguments"]);Concat(["AtomicLt";"AtomicGt"])]));
   ("TypeBound",Disjunction([Concat(["AtomicExtends";"TypeVariable"]);Concat(["AtomicExtends";"ClassOrInterfaceType";"StarredAdditionalBound"])]));
   ("TypeName",Disjunction([Concat(["TypeIdentifier"]);Concat(["PackageOrTypeName";"AtomicDot";"TypeIdentifier"])]));
   ("UnannArrayType",Disjunction([Concat(["UnannPrimitiveType";"Dims"]);Concat(["UnannClassOrInterfaceType";"Dims"]);Concat(["UnannTypeVariable";"Dims"])]));
   ("UnannClassOrInterfaceType",Just_a_disjunction(["UnannClassType";"UnannInterfaceType"]));
   ("UnannClassType",Disjunction([Concat(["TypeIdentifier";"OptionalTypeArguments"]);Concat(["PackageName";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"]);Concat(["UnannClassOrInterfaceType";"AtomicDot";"StarredAnnotation";"TypeIdentifier";"OptionalTypeArguments"])]));
   ("UnannPrimitiveType",Just_a_disjunction(["NumericType";"AtomicBoolean"]));
   ("UnannReferenceType",Just_a_disjunction(["UnannClassOrInterfaceType";"UnannTypeVariable";"UnannArrayType"]));
   ("UnannType",Just_a_disjunction(["UnannPrimitiveType";"Unannref_in_diserenceType"]));
   ("UnaryExpression",Disjunction([Concat(["PreIncrementExpression"]);Concat(["PreDecrementExpression"]);Concat(["AtomicPlus";"UnaryExpression"]);Concat(["AtomicMinus";"UnaryExpression"]);Concat(["UnaryExpressionNotPlusMinus"])]));
   ("UnaryExpressionNotPlusMinus",Disjunction([Concat(["PostfixExpression"]);Concat(["AtomicCompl";"UnaryExpression"]);Concat(["AtomicNot";"UnaryExpression"]);Concat(["CastExpression"]);Concat(["SwitchExpression"])]));
   ("VariableInitializer",Just_a_disjunction(["Expression";"ArrayInitializer"]));
   ("VariableModifier",Just_a_disjunction(["Annotation";"AtomicFinal"]));
   ("WildcardBounds",Disjunction([Concat(["AtomicExtends";"ReferenceType"]);Concat(["AtomicSuper";"ReferenceType"])]));

]);;


(* Java grammar ends here *)

 end ;;
let java_grammar = Private.java_grammar ;;
