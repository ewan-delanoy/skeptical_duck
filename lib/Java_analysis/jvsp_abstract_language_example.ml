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
   Ref of string  |Optional of string ;;

type element_in_disjunction = Jvsp_abstract_language_t.element_in_disjunction = 
   Concat of element_in_concat list ;;
     
type form =  Jvsp_abstract_language_t.form = 
   Disjunction of element_in_disjunction list 
   |Just_atomic of Jvsp_types.token_type list
   |Just_a_star of string ;;

type t =  Jvsp_abstract_language_t.t = AL of (string * form) list ;; 

(* Java grammar begins here *)


 let java_grammar = 

AL ([

   ("AtomicAbstract",Just_atomic([ABSTRACT_T]));
   ("AtomicAndAnd",Just_atomic([AND_AND_T]));
   ("AtomicAnd",Just_atomic([AND_T]));
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
   ("AtomicDot",Just_atomic([DOT_T]));
   ("AtomicDouble",Just_atomic([DOUBLE_T]));
   ("AtomicDo",Just_atomic([DO_T]));
   ("AtomicElse",Just_atomic([ELSE_T]));
   ("AtomicEnum",Just_atomic([ENUM_T]));
   ("AtomicEqEq",Just_atomic([EQ_EQ_T]));
   ("AtomicEq",Just_atomic([EQ_T]));
   ("AtomicExports",Just_atomic([EXPORTS_T]));
   ("AtomicExtends",Just_atomic([EXTENDS_T]));
   ("AtomicFinally",Just_atomic([FINALLY_T]));
   ("AtomicFinal",Just_atomic([FINAL_T]));
   ("AtomicFloat",Just_atomic([FLOAT_T]));
   ("AtomicFor",Just_atomic([FOR_T]));
   ("AtomicGe",Just_atomic([GE_T]));
   ("AtomicGt",Just_atomic([GT_T]));
   ("AtomicIf",Just_atomic([IF_T]));
   ("AtomicImplements",Just_atomic([IMPLEMENTS_T]));
   ("AtomicImport",Just_atomic([IMPORT_T]));
   ("AtomicIncr",Just_atomic([INCR_T]));
   ("AtomicInstanceof",Just_atomic([INSTANCEOF_T]));
   ("AtomicInterface",Just_atomic([INTERFACE_T]));
   ("AtomicInt",Just_atomic([INT_T]));
   ("AtomicLe",Just_atomic([LE_T]));
   ("AtomicLong",Just_atomic([LONG_T]));
   ("AtomicLp",Just_atomic([LP_T]));
   ("AtomicLs",Just_atomic([LS_T]));
   ("AtomicLt",Just_atomic([LT_T]));
   ("AtomicMinus",Just_atomic([MINUS_T]));
   ("AtomicModule",Just_atomic([MODULE_T]));
   ("AtomicMod",Just_atomic([MOD_T]));
   ("AtomicNative",Just_atomic([NATIVE_T]));
   ("AtomicNew",Just_atomic([NEW_T]));
   ("AtomicNonsealed",Just_atomic([NONSEALED_T]));
   ("AtomicNotEq",Just_atomic([NOT_EQ_T]));
   ("AtomicNot",Just_atomic([NOT_T]));
   ("AtomicOpens",Just_atomic([OPENS_T]));
   ("AtomicOperatorEq",Just_atomic([OPERATOR_EQ_T]));
   ("AtomicOrOr",Just_atomic([OR_OR_T]));
   ("AtomicOr",Just_atomic([OR_T]));
   ("AtomicPackage",Just_atomic([PACKAGE_T]));
   ("AtomicPermits",Just_atomic([PERMITS_T]));
   ("AtomicPlus",Just_atomic([PLUS_T]));
   ("AtomicPrivate",Just_atomic([PRIVATE_T]));
   ("AtomicProtected",Just_atomic([PROTECTED_T]));
   ("AtomicProvides",Just_atomic([PROVIDES_T]));
   ("AtomicPublic",Just_atomic([PUBLIC_T]));
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
   ("AtomicThrows",Just_atomic([THROWS_T]));
   ("AtomicThrow",Just_atomic([THROW_T]));
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
   ("Literal",Disjunction([Concat([Ref("IntegerLiteral")]);Concat([Ref("FloatingPointLiteral")]);Concat([Ref("BooleanLiteral")]);Concat([Ref("CharacterLiteral")]);Concat([Ref("StringLiteral")]);Concat([Ref("TextBlock")]);Concat([Ref("NullLiteral")])]));
   ("Type",Disjunction([Concat([Ref("PrimitiveType")]);Concat([Ref("ref_in_diserenceType")])]));
   ("PrimitiveType",Disjunction([Concat([Ref("StarredAnnotation");Ref("NumericType")]);Concat([Ref("StarredAnnotation");Ref("AtomicBoolean")])]));
   ("NumericType",Disjunction([Concat([Ref("IntegralType")]);Concat([Ref("FloatingPointType")])]));
   ("IntegralType",Disjunction([Concat([Ref("AtomicByte")]);Concat([Ref("AtomicShort")]);Concat([Ref("AtomicInt")]);Concat([Ref("AtomicLong")]);Concat([Ref("AtomicChar")])]));
   ("FloatingPointType",Disjunction([Concat([Ref("AtomicFloat")]);Concat([Ref("AtomicDouble")])]));
   ("ReferenceType",Disjunction([Concat([Ref("ClassOrInterfaceType")]);Concat([Ref("TypeVariable")]);Concat([Ref("ArrayType")])]));
   ("ClassOrInterfaceType",Disjunction([Concat([Ref("ClassType")]);Concat([Ref("InterfaceType")])]));
   ("ClassType",Disjunction([Concat([Ref("StarredAnnotation");Ref("TypeIdentifier");Optional("TypeArguments")]);Concat([Ref("PackageName");Ref("AtomicDot");Ref("StarredAnnotation");Ref("TypeIdentifier");Optional("TypeArguments")]);Concat([Ref("ClassOrInterfaceType");Ref("AtomicDot");Ref("StarredAnnotation");Ref("TypeIdentifier");Optional("TypeArguments")])]));
   ("InterfaceType",Disjunction([Concat([Ref("ClassType")])]));
   ("TypeVariable",Disjunction([Concat([Ref("StarredAnnotation");Ref("TypeIdentifier")])]));
   ("ArrayType",Disjunction([Concat([Ref("PrimitiveType");Ref("Dims")]);Concat([Ref("ClassOrInterfaceType");Ref("Dims")]);Concat([Ref("TypeVariable");Ref("Dims")])]));
   ("Dims",Disjunction([Concat([Ref("DimsElement");Ref("StarredDimsElement")])]));
   ("TypeParameter",Disjunction([Concat([Ref("StarredTypeParameterModifier");Ref("TypeIdentifier");Optional("TypeBound")])]));
   ("TypeParameterModifier",Disjunction([Concat([Ref("Annotation")])]));
   ("TypeBound",Disjunction([Concat([Ref("AtomicExtends");Ref("TypeVariable")]);Concat([Ref("AtomicExtends");Ref("ClassOrInterfaceType");Ref("StarredAdditionalBound")])]));
   ("AdditionalBound",Disjunction([Concat([Ref("AtomicAnd");Ref("InterfaceType")])]));
   ("TypeArguments",Disjunction([Concat([Ref("AtomicLt");Ref("TypeArgumentList");Ref("AtomicGt")])]));
   ("TypeArgumentList",Disjunction([Concat([Ref("TypeArgument");Ref("StarredTypeArgumentPrecededByComma")])]));
   ("TypeArgument",Disjunction([Concat([Ref("ref_in_diserenceType")]);Concat([Ref("Wildcard")])]));
   ("Wildcard",Disjunction([Concat([Ref("StarredAnnotation");Ref("AtomicCond");Optional("WildcardBounds")])]));
   ("WildcardBounds",Disjunction([Concat([Ref("AtomicExtends");Ref("ReferenceType")]);Concat([Ref("AtomicSuper");Ref("ReferenceType")])]));
   ("ModuleName",Disjunction([Concat([Ref("Identifier")]);Concat([Ref("ModuleName");Ref("IdentifierPrecededByDot")])]));
   ("PackageName",Disjunction([Concat([Ref("Identifier")]);Concat([Ref("PackageName");Ref("IdentifierPrecededByDot")])]));
   ("TypeName",Disjunction([Concat([Ref("TypeIdentifier")]);Concat([Ref("PackageOrTypeName");Ref("AtomicDot");Ref("TypeIdentifier")])]));
   ("ExpressionName",Disjunction([Concat([Ref("Identifier")]);Concat([Ref("AmbiguousName");Ref("IdentifierPrecededByDot")])]));
   ("MethodName",Disjunction([Concat([Ref("UnqualifiedMethodIdentifier")])]));
   ("PackageOrTypeName",Disjunction([Concat([Ref("Identifier")]);Concat([Ref("PackageOrTypeName");Ref("IdentifierPrecededByDot")])]));
   ("AmbiguousName",Disjunction([Concat([Ref("Identifier")]);Concat([Ref("AmbiguousName");Ref("IdentifierPrecededByDot")])]));
   ("CompilationUnit",Disjunction([Concat([Ref("OrdinaryCompilationUnit")]);Concat([Ref("ModularCompilationUnit")])]));
   ("OrdinaryCompilationUnit",Disjunction([Concat([Optional("PackageDeclaration");Ref("StarredImportDeclaration");Ref("StarredTopLevelClassOrInterfaceDeclaration")])]));
   ("ModularCompilationUnit",Disjunction([Concat([Ref("StarredImportDeclaration");Ref("ModuleDeclaration")])]));
   ("PackageDeclaration",Disjunction([Concat([Ref("StarredPackageModifier");Ref("AtomicPackage");Ref("Identifier");Ref("StarredIdentifierPrecededByDot");Ref("AtomicSm")])]));
   ("PackageModifier",Disjunction([Concat([Ref("Annotation")])]));
   ("ImportDeclaration",Disjunction([Concat([Ref("SingleTypeImportDeclaration")]);Concat([Ref("TypeImportOnDemandDeclaration")]);Concat([Ref("SingleStaticImportDeclaration")]);Concat([Ref("StaticImportOnDemandDeclaration")])]));
   ("SingleTypeImportDeclaration",Disjunction([Concat([Ref("AtomicImport");Ref("TypeName");Ref("AtomicSm")])]));
   ("TypeImportOnDemandDeclaration",Disjunction([Concat([Ref("AtomicImport");Ref("PackageOrTypeName");Ref("AtomicDot");Ref("AtomicTimes");Ref("AtomicSm")])]));
   ("SingleStaticImportDeclaration",Disjunction([Concat([Ref("AtomicImport");Ref("AtomicStatic");Ref("TypeName");Ref("IdentifierPrecededByDot");Ref("AtomicSm")])]));
   ("StaticImportOnDemandDeclaration",Disjunction([Concat([Ref("AtomicImport");Ref("AtomicStatic");Ref("TypeName");Ref("AtomicDot");Ref("AtomicTimes");Ref("AtomicSm")])]));
   ("TopLevelClassOrInterfaceDeclaration",Disjunction([Concat([Ref("ClassDeclaration")]);Concat([Ref("InterfaceDeclaration")]);Concat([Ref("AtomicSm")])]));
   ("ModuleDeclaration",Disjunction([Concat([Ref("StarredAnnotation");Optional("open");Ref("AtomicModule");Ref("Identifier");Ref("StarredIdentifierPrecededByDot");Ref("{");Ref("StarredModuleDirective");Ref("}")])]));
   ("ModuleDirective",Disjunction([Concat([Ref("AtomicRequires");Ref("StarredRequiresModifier");Ref("ModuleName");Ref("AtomicSm")]);Concat([Ref("AtomicExports");Ref("PackageName");Optional("ToModuleList");Ref("AtomicSm")]);Concat([Ref("AtomicOpens");Ref("PackageName");Optional("ToModuleList");Ref("AtomicSm")]);Concat([Ref("AtomicUses");Ref("TypeName");Ref("AtomicSm")]);Concat([Ref("AtomicProvides");Ref("TypeName");Ref("AtomicWith");Ref("TypeName");Ref("StarredTypeNamePrecededByComma");Ref("AtomicSm")])]));
   ("RequiresModifier",Disjunction([Concat([Ref("AtomicTransitive")]);Concat([Ref("AtomicStatic")])]));
   ("ClassDeclaration",Disjunction([Concat([Ref("NormalClassDeclaration")]);Concat([Ref("EnumDeclaration")]);Concat([Ref("RecordDeclaration")])]));
   ("NormalClassDeclaration",Disjunction([Concat([Ref("StarredClassModifier");Ref("AtomicClass");Ref("TypeIdentifier");Optional("TypeParameters");Optional("ClassExtends");Optional("ClassImplements");Optional("ClassPermits");Ref("ClassBody")])]));
   ("ClassModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Ref("AtomicPublic")]);Concat([Ref("AtomicProtected")]);Concat([Ref("AtomicPrivate")]);Concat([Ref("AtomicAbstract")]);Concat([Ref("AtomicStatic")]);Concat([Ref("AtomicFinal")]);Concat([Ref("AtomicSealed")]);Concat([Ref("AtomicNonsealed")]);Concat([Ref("AtomicStrictfp")])]));
   ("TypeParameters",Disjunction([Concat([Ref("AtomicLt");Ref("TypeParameterList");Ref("AtomicGt")])]));
   ("TypeParameterList",Disjunction([Concat([Ref("TypeParameter");Ref("StarredTypeParameterPrecededByComma")])]));
   ("ClassExtends",Disjunction([Concat([Ref("AtomicExtends");Ref("ClassType")])]));
   ("ClassImplements",Disjunction([Concat([Ref("AtomicImplements");Ref("InterfaceTypeList")])]));
   ("InterfaceTypeList",Disjunction([Concat([Ref("InterfaceType");Ref("StarredInterfaceTypePrecededByComma")])]));
   ("ClassPermits",Disjunction([Concat([Ref("AtomicPermits");Ref("TypeName");Ref("StarredTypeNamePrecededByComma")])]));
   ("ClassBody",Disjunction([Concat([Ref("{");Ref("StarredClassBodyDeclaration");Ref("}")])]));
   ("ClassBodyDeclaration",Disjunction([Concat([Ref("ClassMemberDeclaration")]);Concat([Ref("InstanceInitializer")]);Concat([Ref("StaticInitializer")]);Concat([Ref("ConstructorDeclaration")])]));
   ("ClassMemberDeclaration",Disjunction([Concat([Ref("FieldDeclaration")]);Concat([Ref("MethodDeclaration")]);Concat([Ref("ClassDeclaration")]);Concat([Ref("InterfaceDeclaration")]);Concat([Ref("AtomicSm")])]));
   ("FieldDeclaration",Disjunction([Concat([Ref("StarredFieldModifier");Ref("UnannType");Ref("VariableDeclaratorList");Ref("AtomicSm")])]));
   ("FieldModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Ref("AtomicPublic")]);Concat([Ref("AtomicProtected")]);Concat([Ref("AtomicPrivate")]);Concat([Ref("AtomicStatic")]);Concat([Ref("AtomicFinal")]);Concat([Ref("AtomicTransient")]);Concat([Ref("AtomicVolatile")])]));
   ("VariableDeclaratorList",Disjunction([Concat([Ref("VariableDeclarator");Ref("StarredVariableDeclaratorPrecededByComma")])]));
   ("VariableDeclarator",Disjunction([Concat([Ref("VariableDeclaratorId");Optional("EqualsVariableInitializer")])]));
   ("VariableDeclaratorId",Disjunction([Concat([Ref("Identifier");Optional("Dims")])]));
   ("VariableInitializer",Disjunction([Concat([Ref("Expression")]);Concat([Ref("ArrayInitializer")])]));
   ("UnannType",Disjunction([Concat([Ref("UnannPrimitiveType")]);Concat([Ref("Unannref_in_diserenceType")])]));
   ("UnannPrimitiveType",Disjunction([Concat([Ref("NumericType")]);Concat([Ref("AtomicBoolean")])]));
   ("UnannReferenceType",Disjunction([Concat([Ref("UnannClassOrInterfaceType")]);Concat([Ref("UnannTypeVariable")]);Concat([Ref("UnannArrayType")])]));
   ("UnannClassOrInterfaceType",Disjunction([Concat([Ref("UnannClassType")]);Concat([Ref("UnannInterfaceType")])]));
   ("UnannClassType",Disjunction([Concat([Ref("TypeIdentifier");Optional("TypeArguments")]);Concat([Ref("PackageName");Ref("AtomicDot");Ref("StarredAnnotation");Ref("TypeIdentifier");Optional("TypeArguments")]);Concat([Ref("UnannClassOrInterfaceType");Ref("AtomicDot");Ref("StarredAnnotation");Ref("TypeIdentifier");Optional("TypeArguments")])]));
   ("UnannInterfaceType",Disjunction([Concat([Ref("UnannClassType")])]));
   ("UnannTypeVariable",Disjunction([Concat([Ref("TypeIdentifier")])]));
   ("UnannArrayType",Disjunction([Concat([Ref("UnannPrimitiveType");Ref("Dims")]);Concat([Ref("UnannClassOrInterfaceType");Ref("Dims")]);Concat([Ref("UnannTypeVariable");Ref("Dims")])]));
   ("MethodDeclaration",Disjunction([Concat([Ref("StarredMethodModifier");Ref("MethodHeader");Ref("MethodBody")])]));
   ("MethodModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Ref("AtomicPublic")]);Concat([Ref("AtomicProtected")]);Concat([Ref("AtomicPrivate")]);Concat([Ref("AtomicAbstract")]);Concat([Ref("AtomicStatic")]);Concat([Ref("AtomicFinal")]);Concat([Ref("AtomicSynchronized")]);Concat([Ref("AtomicNative")]);Concat([Ref("AtomicStrictfp")])]));
   ("MethodHeader",Disjunction([Concat([Ref("Result");Ref("MethodDeclarator");Optional("Throws")]);Concat([Ref("TypeParameters");Ref("StarredAnnotation");Ref("Result");Ref("MethodDeclarator");Optional("Throws")])]));
   ("Result",Disjunction([Concat([Ref("UnannType")]);Concat([Ref("AtomicVoid")])]));
   ("MethodDeclarator",Disjunction([Concat([Ref("Identifier");Ref("AtomicLp");Optional("ReceiverParameterFollowedByComma");Optional("FormalParameterList");Ref("AtomicRp");Optional("Dims")])]));
   ("ReceiverParameter",Disjunction([Concat([Ref("StarredAnnotation");Ref("UnannType");Optional("IdentifierFollowedByDot");Ref("AtomicThis")])]));
   ("FormalParameterList",Disjunction([Concat([Ref("FormalParameter");Ref("StarredFormalParameterPrecededByComma")])]));
   ("FormalParameter",Disjunction([Concat([Ref("StarredVariableModifier");Ref("UnannType");Ref("VariableDeclaratorId")]);Concat([Ref("VariableArityParameter")])]));
   ("VariableArityParameter",Disjunction([Concat([Ref("StarredVariableModifier");Ref("UnannType");Ref("StarredAnnotation");Ref("AtomicDot");Ref("AtomicDot");Ref("AtomicDot");Ref("Identifier")])]));
   ("VariableModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Ref("AtomicFinal")])]));
   ("Throws",Disjunction([Concat([Ref("AtomicThrows");Ref("ExceptionTypeList")])]));
   ("ExceptionTypeList",Disjunction([Concat([Ref("ExceptionType");Ref("StarredExceptionTypePrecededByComma")])]));
   ("ExceptionType",Disjunction([Concat([Ref("ClassType")]);Concat([Ref("TypeVariable")])]));
   ("MethodBody",Disjunction([Concat([Ref("Block")]);Concat([Ref("AtomicSm")])]));
   ("InstanceInitializer",Disjunction([Concat([Ref("Block")])]));
   ("StaticInitializer",Disjunction([Concat([Ref("AtomicStatic");Ref("Block")])]));
   ("ConstructorDeclaration",Disjunction([Concat([Ref("StarredConstructorModifier");Ref("ConstructorDeclarator");Optional("Throws");Ref("ConstructorBody")])]));
   ("ConstructorModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Ref("AtomicPublic")]);Concat([Ref("AtomicProtected")]);Concat([Ref("AtomicPrivate")])]));
   ("ConstructorDeclarator",Disjunction([Concat([Optional("TypeParameters");Ref("SimpleTypeName");Ref("AtomicLp");Optional("ReceiverParameterFollowedByComma");Optional("FormalParameterList");Ref("AtomicRp")])]));
   ("SimpleTypeName",Disjunction([Concat([Ref("TypeIdentifier")])]));
   ("ConstructorBody",Disjunction([Concat([Ref("{");Optional("ExplicitConstructorInvocation");Optional("BlockStatements");Ref("}")])]));
   ("ExplicitConstructorInvocation",Disjunction([Concat([Optional("TypeArguments");Ref("AtomicThis");Ref("ParenthesedArgumentList");Ref("AtomicSm")]);Concat([Optional("TypeArguments");Ref("AtomicSuper");Ref("ParenthesedArgumentList");Ref("AtomicSm")]);Concat([Ref("ExpressionName");Ref("AtomicDot");Optional("TypeArguments");Ref("AtomicSuper");Ref("ParenthesedArgumentList");Ref("AtomicSm")]);Concat([Ref("Primary");Ref("AtomicDot");Optional("TypeArguments");Ref("AtomicSuper");Ref("ParenthesedArgumentList");Ref("AtomicSm")])]));
   ("EnumDeclaration",Disjunction([Concat([Ref("StarredClassModifier");Ref("AtomicEnum");Ref("TypeIdentifier");Optional("ClassImplements");Ref("EnumBody")])]));
   ("EnumBody",Disjunction([Concat([Ref("{");Optional("EnumConstantList");Optional(",");Optional("EnumBodyDeclarations");Ref("}")])]));
   ("EnumConstantList",Disjunction([Concat([Ref("EnumConstant");Ref("StarredEnumConstantPrecededByComma")])]));
   ("EnumConstant",Disjunction([Concat([Ref("StarredEnumConstantModifier");Ref("Identifier");Optional("ParenthesedArgumentList");Optional("ClassBody")])]));
   ("EnumConstantModifier",Disjunction([Concat([Ref("Annotation")])]));
   ("EnumBodyDeclarations",Disjunction([Concat([Ref("AtomicSm");Ref("StarredClassBodyDeclaration")])]));
   ("RecordDeclaration",Disjunction([Concat([Ref("StarredClassModifier");Ref("AtomicRecord");Ref("TypeIdentifier");Optional("TypeParameters");Ref("RecordHeader");Optional("ClassImplements");Ref("RecordBody")])]));
   ("RecordHeader",Disjunction([Concat([Ref("AtomicLp");Optional("RecordComponentList");Ref("AtomicRp")])]));
   ("RecordComponentList",Disjunction([Concat([Ref("RecordComponent");Ref("StarredRecordComponentPrecededByComma")])]));
   ("RecordComponent",Disjunction([Concat([Ref("StarredRecordComponentModifier");Ref("UnannType");Ref("Identifier")]);Concat([Ref("VariableArityRecordComponent")])]));
   ("VariableArityRecordComponent",Disjunction([Concat([Ref("StarredRecordComponentModifier");Ref("UnannType");Ref("StarredAnnotation");Ref("AtomicDot");Ref("AtomicDot");Ref("AtomicDot");Ref("Identifier")])]));
   ("RecordComponentModifier",Disjunction([Concat([Ref("Annotation")])]));
   ("RecordBody",Disjunction([Concat([Ref("{");Ref("StarredRecordBodyDeclaration");Ref("}")])]));
   ("RecordBodyDeclaration",Disjunction([Concat([Ref("ClassBodyDeclaration")]);Concat([Ref("CompactConstructorDeclaration")])]));
   ("CompactConstructorDeclaration",Disjunction([Concat([Ref("StarredConstructorModifier");Ref("SimpleTypeName");Ref("ConstructorBody")])]));
   ("InterfaceDeclaration",Disjunction([Concat([Ref("NormalInterfaceDeclaration")]);Concat([Ref("AnnotationInterfaceDeclaration")])]));
   ("NormalInterfaceDeclaration",Disjunction([Concat([Ref("StarredInterfaceModifier");Ref("AtomicInterface");Ref("TypeIdentifier");Optional("TypeParameters");Optional("InterfaceExtends");Optional("InterfacePermits");Ref("InterfaceBody")])]));
   ("InterfaceModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Ref("AtomicPublic")]);Concat([Ref("AtomicProtected")]);Concat([Ref("AtomicPrivate")]);Concat([Ref("AtomicAbstract")]);Concat([Ref("AtomicStatic")]);Concat([Ref("AtomicSealed")]);Concat([Ref("AtomicNonsealed")]);Concat([Ref("AtomicStrictfp")])]));
   ("InterfaceExtends",Disjunction([Concat([Ref("AtomicExtends");Ref("InterfaceTypeList")])]));
   ("InterfacePermits",Disjunction([Concat([Ref("AtomicPermits");Ref("TypeName");Ref("StarredTypeNamePrecededByComma")])]));
   ("InterfaceBody",Disjunction([Concat([Ref("{");Ref("StarredInterfaceMemberDeclaration");Ref("}")])]));
   ("InterfaceMemberDeclaration",Disjunction([Concat([Ref("ConstantDeclaration")]);Concat([Ref("InterfaceMethodDeclaration")]);Concat([Ref("ClassDeclaration")]);Concat([Ref("InterfaceDeclaration")]);Concat([Ref("AtomicSm")])]));
   ("ConstantDeclaration",Disjunction([Concat([Ref("StarredConstantModifier");Ref("UnannType");Ref("VariableDeclaratorList");Ref("AtomicSm")])]));
   ("ConstantModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Ref("AtomicPublic")]);Concat([Ref("AtomicStatic")]);Concat([Ref("AtomicFinal")])]));
   ("InterfaceMethodDeclaration",Disjunction([Concat([Ref("StarredInterfaceMethodModifier");Ref("MethodHeader");Ref("MethodBody")])]));
   ("InterfaceMethodModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Ref("AtomicPublic")]);Concat([Ref("AtomicPrivate")]);Concat([Ref("AtomicAbstract")]);Concat([Ref("AtomicDefault")]);Concat([Ref("AtomicStatic")]);Concat([Ref("AtomicStrictfp")])]));
   ("AnnotationInterfaceDeclaration",Disjunction([Concat([Ref("StarredInterfaceModifier");Ref("AtomicSnail");Ref("AtomicInterface");Ref("TypeIdentifier");Ref("AnnotationInterfaceBody")])]));
   ("AnnotationInterfaceBody",Disjunction([Concat([Ref("{");Ref("StarredAnnotationInterfaceMemberDeclaration");Ref("}")])]));
   ("AnnotationInterfaceMemberDeclaration",Disjunction([Concat([Ref("AnnotationInterfaceElementDeclaration")]);Concat([Ref("ConstantDeclaration")]);Concat([Ref("ClassDeclaration")]);Concat([Ref("InterfaceDeclaration")]);Concat([Ref("AtomicSm")])]));
   ("AnnotationInterfaceElementDeclaration",Disjunction([Concat([Ref("StarredAnnotationInterfaceElementModifier");Ref("UnannType");Ref("Identifier");Ref("AtomicLp");Ref("AtomicRp");Optional("Dims");Optional("DefaultValue");Ref("AtomicSm")])]));
   ("AnnotationInterfaceElementModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Ref("AtomicPublic")]);Concat([Ref("AtomicAbstract")])]));
   ("DefaultValue",Disjunction([Concat([Ref("AtomicDefault");Ref("ElementValue")])]));
   ("Annotation",Disjunction([Concat([Ref("NormalAnnotation")]);Concat([Ref("MarkerAnnotation")]);Concat([Ref("SingleElementAnnotation")])]));
   ("NormalAnnotation",Disjunction([Concat([Ref("AtomicSnail");Ref("TypeName");Ref("AtomicLp");Optional("ElementValuePairList");Ref("AtomicRp")])]));
   ("ElementValuePairList",Disjunction([Concat([Ref("ElementValuePair");Ref("StarredElementValuePrecededByCommaPair")])]));
   ("ElementValuePair",Disjunction([Concat([Ref("Identifier");Ref("AtomicEq");Ref("ElementValue")])]));
   ("ElementValue",Disjunction([Concat([Ref("ConditionalExpression")]);Concat([Ref("ElementValueArrayInitializer")]);Concat([Ref("Annotation")])]));
   ("ElementValueArrayInitializer",Disjunction([Concat([Ref("{");Optional("ElementValueList");Optional(",");Ref("}")])]));
   ("ElementValueList",Disjunction([Concat([Ref("ElementValue");Ref("StarredElementValuePrecededByComma")])]));
   ("MarkerAnnotation",Disjunction([Concat([Ref("AtomicSnail");Ref("TypeName")])]));
   ("SingleElementAnnotation",Disjunction([Concat([Ref("AtomicSnail");Ref("TypeName");Ref("AtomicLp");Ref("ElementValue");Ref("AtomicRp")])]));
   ("ArrayInitializer",Disjunction([Concat([Ref("{");Optional("VariableInitializerList");Optional(",");Ref("}")])]));
   ("VariableInitializerList",Disjunction([Concat([Ref("VariableInitializer");Ref("StarredVariableInitializerPrecededByComma")])]));
   ("Block",Disjunction([Concat([Ref("{");Optional("BlockStatements");Ref("}")])]));
   ("BlockStatements",Disjunction([Concat([Ref("BlockStatement");Ref("StarredBlockStatement")])]));
   ("BlockStatement",Disjunction([Concat([Ref("LocalClassOrInterfaceDeclaration")]);Concat([Ref("LocalVariableDeclarationStatement")]);Concat([Ref("Statement")])]));
   ("LocalClassOrInterfaceDeclaration",Disjunction([Concat([Ref("ClassDeclaration")]);Concat([Ref("NormalInterfaceDeclaration")])]));
   ("LocalVariableDeclarationStatement",Disjunction([Concat([Ref("LocalVariableDeclaration");Ref("AtomicSm")])]));
   ("LocalVariableDeclaration",Disjunction([Concat([Ref("StarredVariableModifier");Ref("LocalVariableType");Ref("VariableDeclaratorList")])]));
   ("LocalVariableType",Disjunction([Concat([Ref("UnannType")]);Concat([Ref("AtomicVar")])]));
   ("Statement",Disjunction([Concat([Ref("StatementWithoutTrailingSubstatement")]);Concat([Ref("LabeledStatement")]);Concat([Ref("IfThenStatement")]);Concat([Ref("IfThenElseStatement")]);Concat([Ref("WhileStatement")]);Concat([Ref("ForStatement")])]));
   ("StatementNoShortIf",Disjunction([Concat([Ref("StatementWithoutTrailingSubstatement")]);Concat([Ref("LabeledStatementNoShortIf")]);Concat([Ref("IfThenElseStatementNoShortIf")]);Concat([Ref("WhileStatementNoShortIf")]);Concat([Ref("ForStatementNoShortIf")])]));
   ("StatementWithoutTrailingSubstatement",Disjunction([Concat([Ref("Block")]);Concat([Ref("EmptyStatement")]);Concat([Ref("ExpressionStatement")]);Concat([Ref("AssertStatement")]);Concat([Ref("SwitchStatement")]);Concat([Ref("DoStatement")]);Concat([Ref("BreakStatement")]);Concat([Ref("ContinueStatement")]);Concat([Ref("ReturnStatement")]);Concat([Ref("SynchronizedStatement")]);Concat([Ref("ThrowStatement")]);Concat([Ref("TryStatement")]);Concat([Ref("YieldStatement")])]));
   ("EmptyStatement",Disjunction([Concat([Ref(";")])]));
   ("LabeledStatement",Disjunction([Concat([Ref("Identifier");Ref("AtomicColon");Ref("Statement")])]));
   ("LabeledStatementNoShortIf",Disjunction([Concat([Ref("Identifier");Ref("AtomicColon");Ref("StatementNoShortIf")])]));
   ("ExpressionStatement",Disjunction([Concat([Ref("StatementExpression");Ref("AtomicSm")])]));
   ("StatementExpression",Disjunction([Concat([Ref("Assignment")]);Concat([Ref("PreIncrementExpression")]);Concat([Ref("PreDecrementExpression")]);Concat([Ref("PostIncrementExpression")]);Concat([Ref("PostDecrementExpression")]);Concat([Ref("MethodInvocation")]);Concat([Ref("ClassInstanceCreationExpression")])]));
   ("IfThenStatement",Disjunction([Concat([Ref("AtomicIf");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("Statement")])]));
   ("IfThenElseStatement",Disjunction([Concat([Ref("AtomicIf");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("StatementNoShortIf");Ref("AtomicElse");Ref("Statement")])]));
   ("IfThenElseStatementNoShortIf",Disjunction([Concat([Ref("AtomicIf");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("StatementNoShortIf");Ref("AtomicElse");Ref("StatementNoShortIf")])]));
   ("AssertStatement",Disjunction([Concat([Ref("AtomicAssert");Ref("Expression");Ref("AtomicSm")]);Concat([Ref("AtomicAssert");Ref("Expression");Ref("AtomicColon");Ref("Expression");Ref("AtomicSm")])]));
   ("SwitchStatement",Disjunction([Concat([Ref("AtomicSwitch");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("SwitchBlock")])]));
   ("SwitchBlock",Disjunction([Concat([Ref("{");Ref("SwitchRule");Ref("StarredSwitchRule");Ref("}")]);Concat([Ref("{");Ref("StarredSwitchBlockStatementGroup");Ref("StarredSwitchLabelFollowedByColon");Ref("}")])]));
   ("SwitchRule",Disjunction([Concat([Ref("SwitchLabel");Ref("AtomicMinus");Ref("AtomicGt");Ref("Expression");Ref("AtomicSm")]);Concat([Ref("SwitchLabel");Ref("AtomicMinus");Ref("AtomicGt");Ref("Block")]);Concat([Ref("SwitchLabel");Ref("AtomicMinus");Ref("AtomicGt");Ref("ThrowStatement")])]));
   ("SwitchBlockStatementGroup",Disjunction([Concat([Ref("SwitchLabelFollowedByColon");Ref("StarredSwitchLabelFollowedByColon");Ref("BlockStatements")])]));
   ("SwitchLabel",Disjunction([Concat([Ref("AtomicCase");Ref("CaseConstant");Ref("StarredCaseConstantPrecededByComma")]);Concat([Ref("AtomicDefault")])]));
   ("CaseConstant",Disjunction([Concat([Ref("ConditionalExpression")])]));
   ("WhileStatement",Disjunction([Concat([Ref("AtomicWhile");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("Statement")])]));
   ("WhileStatementNoShortIf",Disjunction([Concat([Ref("AtomicWhile");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("StatementNoShortIf")])]));
   ("DoStatement",Disjunction([Concat([Ref("AtomicDo");Ref("Statement");Ref("AtomicWhile");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("AtomicSm")])]));
   ("ForStatement",Disjunction([Concat([Ref("BasicForStatement")]);Concat([Ref("EnhancedForStatement")])]));
   ("ForStatementNoShortIf",Disjunction([Concat([Ref("BasicForStatementNoShortIf")]);Concat([Ref("EnhancedForStatementNoShortIf")])]));
   ("BasicForStatement",Disjunction([Concat([Ref("AtomicFor");Ref("AtomicLp");Optional("ForInit");Ref("AtomicSm");Optional("Expression");Ref("AtomicSm");Optional("ForUpdate");Ref("AtomicRp");Ref("Statement")])]));
   ("BasicForStatementNoShortIf",Disjunction([Concat([Ref("AtomicFor");Ref("AtomicLp");Optional("ForInit");Ref("AtomicSm");Optional("Expression");Ref("AtomicSm");Optional("ForUpdate");Ref("AtomicRp");Ref("StatementNoShortIf")])]));
   ("ForInit",Disjunction([Concat([Ref("StatementExpressionList")]);Concat([Ref("LocalVariableDeclaration")])]));
   ("ForUpdate",Disjunction([Concat([Ref("StatementExpressionList")])]));
   ("StatementExpressionList",Disjunction([Concat([Ref("StatementExpression");Ref("StarredStatementExpressionPrecededByComma")])]));
   ("EnhancedForStatement",Disjunction([Concat([Ref("AtomicFor");Ref("AtomicLp");Ref("LocalVariableDeclaration");Ref("AtomicColon");Ref("Expression");Ref("AtomicRp");Ref("Statement")])]));
   ("EnhancedForStatementNoShortIf",Disjunction([Concat([Ref("AtomicFor");Ref("AtomicLp");Ref("LocalVariableDeclaration");Ref("AtomicColon");Ref("Expression");Ref("AtomicRp");Ref("StatementNoShortIf")])]));
   ("BreakStatement",Disjunction([Concat([Ref("AtomicBreak");Optional("Identifier");Ref("AtomicSm")])]));
   ("YieldStatement",Disjunction([Concat([Ref("AtomicYield");Ref("Expression");Ref("AtomicSm")])]));
   ("ContinueStatement",Disjunction([Concat([Ref("AtomicContinue");Optional("Identifier");Ref("AtomicSm")])]));
   ("ReturnStatement",Disjunction([Concat([Ref("AtomicReturn");Optional("Expression");Ref("AtomicSm")])]));
   ("ThrowStatement",Disjunction([Concat([Ref("AtomicThrow");Ref("Expression");Ref("AtomicSm")])]));
   ("SynchronizedStatement",Disjunction([Concat([Ref("AtomicSynchronized");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("Block")])]));
   ("TryStatement",Disjunction([Concat([Ref("AtomicTry");Ref("Block");Ref("Catches")]);Concat([Ref("AtomicTry");Ref("Block");Optional("Catches");Ref("Finally")]);Concat([Ref("TryWithResourcesStatement")])]));
   ("Catches",Disjunction([Concat([Ref("CatchClause");Ref("StarredCatchClause")])]));
   ("CatchClause",Disjunction([Concat([Ref("AtomicCatch");Ref("AtomicLp");Ref("CatchFormalParameter");Ref("AtomicRp");Ref("Block")])]));
   ("CatchFormalParameter",Disjunction([Concat([Ref("StarredVariableModifier");Ref("CatchType");Ref("VariableDeclaratorId")])]));
   ("CatchType",Disjunction([Concat([Ref("UnannClassType");Ref("StarredClassTypePrecededByVerticalBar")])]));
   ("Finally",Disjunction([Concat([Ref("AtomicFinally");Ref("Block")])]));
   ("TryWithResourcesStatement",Disjunction([Concat([Ref("AtomicTry");Ref("ResourceSpecification");Ref("Block");Optional("Catches");Optional("Finally")])]));
   ("ResourceSpecification",Disjunction([Concat([Ref("AtomicLp");Ref("ResourceList");Optional(";");Ref("AtomicRp")])]));
   ("ResourceList",Disjunction([Concat([Ref("Resource");Ref("StarredResourcePrecededBySemiColon")])]));
   ("Resource",Disjunction([Concat([Ref("LocalVariableDeclaration")]);Concat([Ref("VariableAccess")])]));
   ("Pattern",Disjunction([Concat([Ref("TypePattern")])]));
   ("TypePattern",Disjunction([Concat([Ref("LocalVariableDeclaration")])]));
   ("Primary",Disjunction([Concat([Ref("PrimaryNoNewArray")]);Concat([Ref("ArrayCreationExpression")])]));
   ("PrimaryNoNewArray",Disjunction([Concat([Ref("Literal")]);Concat([Ref("ClassLiteral")]);Concat([Ref("AtomicThis")]);Concat([Ref("TypeName");Ref("AtomicDot");Ref("AtomicThis")]);Concat([Ref("AtomicLp");Ref("Expression");Ref("AtomicRp")]);Concat([Ref("ClassInstanceCreationExpression")]);Concat([Ref("FieldAccess")]);Concat([Ref("ArrayAccess")]);Concat([Ref("MethodInvocation")]);Concat([Ref("Methodref_in_diserence")])]));
   ("ClassLiteral",Disjunction([Concat([Ref("TypeName");Ref("StarredOpenSquare");Ref("AtomicDot");Ref("AtomicClass")]);Concat([Ref("NumericType");Ref("StarredOpenSquare");Ref("AtomicDot");Ref("AtomicClass")]);Concat([Ref("AtomicBoolean");Ref("StarredOpenSquare");Ref("AtomicDot");Ref("AtomicClass")]);Concat([Ref("AtomicVoid");Ref("AtomicDot");Ref("AtomicClass")])]));
   ("ClassInstanceCreationExpression",Disjunction([Concat([Ref("UnqualifiedClassInstanceCreationExpression")]);Concat([Ref("ExpressionName");Ref("AtomicDot");Ref("UnqualifiedClassInstanceCreationExpression")]);Concat([Ref("Primary");Ref("AtomicDot");Ref("UnqualifiedClassInstanceCreationExpression")])]));
   ("UnqualifiedClassInstanceCreationExpression",Disjunction([Concat([Ref("AtomicNew");Optional("TypeArguments");Ref("ClassOrInterfaceTypeToInstantiate");Ref("ParenthesedArgumentList");Optional("ClassBody")])]));
   ("ClassOrInterfaceTypeToInstantiate",Disjunction([Concat([Ref("StarredAnnotation");Ref("Identifier");Ref("StarredAnnotatedIdentifierrPrecededByDot");Optional("TypeArgumentsOrDiamond")])]));
   ("TypeArgumentsOrDiamond",Disjunction([Concat([Ref("TypeArguments")]);Concat([Ref("AtomicLt");Ref("AtomicGt")])]));
   ("FieldAccess",Disjunction([Concat([Ref("Primary");Ref("IdentifierPrecededByDot")]);Concat([Ref("AtomicSuper");Ref("IdentifierPrecededByDot")]);Concat([Ref("TypeName");Ref("AtomicDot");Ref("AtomicSuper");Ref("IdentifierPrecededByDot")])]));
   ("ArrayAccess",Disjunction([Concat([Ref("ExpressionName");Ref("[");Ref("Expression");Ref("]")]);Concat([Ref("PrimaryNoNewArray");Ref("[");Ref("Expression");Ref("]")])]));
   ("MethodInvocation",Disjunction([Concat([Ref("MethodName");Ref("ParenthesedArgumentList")]);Concat([Ref("TypeName");Ref("AtomicDot");Optional("TypeArguments");Ref("Identifier");Ref("ParenthesedArgumentList")]);Concat([Ref("ExpressionName");Ref("AtomicDot");Optional("TypeArguments");Ref("Identifier");Ref("ParenthesedArgumentList")]);Concat([Ref("Primary");Ref("AtomicDot");Optional("TypeArguments");Ref("Identifier");Ref("ParenthesedArgumentList")]);Concat([Ref("AtomicSuper");Ref("AtomicDot");Optional("TypeArguments");Ref("Identifier");Ref("ParenthesedArgumentList")]);Concat([Ref("TypeName");Ref("AtomicDot");Ref("AtomicSuper");Ref("AtomicDot");Optional("TypeArguments");Ref("Identifier");Ref("ParenthesedArgumentList")])]));
   ("ArgumentList",Disjunction([Concat([Ref("Expression");Ref("StarredExpressionPrecededByComma")])]));
   ("MethodReference",Disjunction([Concat([Ref("ExpressionName");Ref("AtomicColon");Ref("AtomicColon");Optional("TypeArguments");Ref("Identifier")]);Concat([Ref("Primary");Ref("AtomicColon");Ref("AtomicColon");Optional("TypeArguments");Ref("Identifier")]);Concat([Ref("ReferenceType");Ref("AtomicColon");Ref("AtomicColon");Optional("TypeArguments");Ref("Identifier")]);Concat([Ref("AtomicSuper");Ref("AtomicColon");Ref("AtomicColon");Optional("TypeArguments");Ref("Identifier")]);Concat([Ref("TypeName");Ref("AtomicDot");Ref("AtomicSuper");Ref("AtomicColon");Ref("AtomicColon");Optional("TypeArguments");Ref("Identifier")]);Concat([Ref("ClassType");Ref("AtomicColon");Ref("AtomicColon");Optional("TypeArguments");Ref("AtomicNew")]);Concat([Ref("ArrayType");Ref("AtomicColon");Ref("AtomicColon");Ref("AtomicNew")])]));
   ("ArrayCreationExpression",Disjunction([Concat([Ref("AtomicNew");Ref("PrimitiveType");Ref("DimExprs");Optional("Dims")]);Concat([Ref("AtomicNew");Ref("ClassOrInterfaceType");Ref("DimExprs");Optional("Dims")]);Concat([Ref("AtomicNew");Ref("PrimitiveType");Ref("Dims");Ref("ArrayInitializer")]);Concat([Ref("AtomicNew");Ref("ClassOrInterfaceType");Ref("Dims");Ref("ArrayInitializer")])]));
   ("DimExprs",Disjunction([Concat([Ref("DimExpr");Ref("StarredDimExpr")])]));
   ("DimExpr",Disjunction([Concat([Ref("StarredAnnotation");Ref("[");Ref("Expression");Ref("]")])]));
   ("Expression",Disjunction([Concat([Ref("LambdaExpression")]);Concat([Ref("AssignmentExpression")])]));
   ("LambdaExpression",Disjunction([Concat([Ref("LambdaParameters");Ref("AtomicMinus");Ref("AtomicGt");Ref("LambdaBody")])]));
   ("LambdaParameters",Disjunction([Concat([Ref("AtomicLp");Optional("LambdaParameterList");Ref("AtomicRp")]);Concat([Ref("Identifier")])]));
   ("LambdaParameterList",Disjunction([Concat([Ref("LambdaParameter");Ref("StarredLambdaParameterPrecededByComma")]);Concat([Ref("Identifier");Ref("StarredIdentifierPrecededByComma")])]));
   ("LambdaParameter",Disjunction([Concat([Ref("StarredVariableModifier");Ref("LambdaParameterType");Ref("VariableDeclaratorId")]);Concat([Ref("VariableArityParameter")])]));
   ("LambdaParameterType",Disjunction([Concat([Ref("UnannType")]);Concat([Ref("AtomicVar")])]));
   ("LambdaBody",Disjunction([Concat([Ref("Expression")]);Concat([Ref("Block")])]));
   ("AssignmentExpression",Disjunction([Concat([Ref("ConditionalExpression")]);Concat([Ref("Assignment")])]));
   ("Assignment",Disjunction([Concat([Ref("LeftHandSide");Ref("AtomicOperatorEq");Ref("Expression")])]));
   ("LeftHandSide",Disjunction([Concat([Ref("ExpressionName")]);Concat([Ref("FieldAccess")]);Concat([Ref("ArrayAccess")])]));
   ("",Disjunction([]));
   ("ConditionalExpression",Disjunction([Concat([Ref("ConditionalOrExpression")]);Concat([Ref("ConditionalOrExpression");Ref("AtomicCond");Ref("Expression");Ref("AtomicColon");Ref("ConditionalExpression")]);Concat([Ref("ConditionalOrExpression");Ref("AtomicCond");Ref("Expression");Ref("AtomicColon");Ref("LambdaExpression")])]));
   ("ConditionalOrExpression",Disjunction([Concat([Ref("ConditionalAndExpression")]);Concat([Ref("ConditionalOrExpression");Ref("AtomicOrOr");Ref("ConditionalAndExpression")])]));
   ("ConditionalAndExpression",Disjunction([Concat([Ref("InclusiveOrExpression")]);Concat([Ref("ConditionalAndExpression");Ref("AtomicAndAnd");Ref("InclusiveOrExpression")])]));
   ("InclusiveOrExpression",Disjunction([Concat([Ref("ExclusiveOrExpression")]);Concat([Ref("InclusiveOrExpression");Ref("AtomicOr");Ref("ExclusiveOrExpression")])]));
   ("ExclusiveOrExpression",Disjunction([Concat([Ref("AndExpression")]);Concat([Ref("ExclusiveOrExpression");Ref("AtomicXor");Ref("AndExpression")])]));
   ("AndExpression",Disjunction([Concat([Ref("EqualityExpression")]);Concat([Ref("AndExpression");Ref("AtomicAnd");Ref("EqualityExpression")])]));
   ("EqualityExpression",Disjunction([Concat([Ref("RelationalExpression")]);Concat([Ref("EqualityExpression");Ref("AtomicEqEq");Ref("RelationalExpression")]);Concat([Ref("EqualityExpression");Ref("AtomicNotEq");Ref("RelationalExpression")])]));
   ("RelationalExpression",Disjunction([Concat([Ref("ShiftExpression")]);Concat([Ref("RelationalExpression");Ref("AtomicLt");Ref("ShiftExpression")]);Concat([Ref("RelationalExpression");Ref("AtomicGt");Ref("ShiftExpression")]);Concat([Ref("RelationalExpression");Ref("AtomicLe");Ref("ShiftExpression")]);Concat([Ref("RelationalExpression");Ref("AtomicGe");Ref("ShiftExpression")]);Concat([Ref("InstanceofExpression")])]));
   ("InstanceofExpression",Disjunction([Concat([Ref("RelationalExpression");Ref("AtomicInstanceof");Ref("ReferenceType")]);Concat([Ref("RelationalExpression");Ref("AtomicInstanceof");Ref("Pattern")])]));
   ("ShiftExpression",Disjunction([Concat([Ref("AdditiveExpression")]);Concat([Ref("ShiftExpression");Ref("AtomicLs");Ref("AdditiveExpression")]);Concat([Ref("ShiftExpression");Ref("AtomicSrs");Ref("AdditiveExpression")]);Concat([Ref("ShiftExpression");Ref("AtomicUrs");Ref("AdditiveExpression")])]));
   ("AdditiveExpression",Disjunction([Concat([Ref("MultiplicativeExpression")]);Concat([Ref("AdditiveExpression");Ref("AtomicPlus");Ref("MultiplicativeExpression")]);Concat([Ref("AdditiveExpression");Ref("AtomicMinus");Ref("MultiplicativeExpression")])]));
   ("MultiplicativeExpression",Disjunction([Concat([Ref("UnaryExpression")]);Concat([Ref("MultiplicativeExpression");Ref("AtomicTimes");Ref("UnaryExpression")]);Concat([Ref("MultiplicativeExpression");Ref("AtomicDiv");Ref("UnaryExpression")]);Concat([Ref("MultiplicativeExpression");Ref("AtomicMod");Ref("UnaryExpression")])]));
   ("UnaryExpression",Disjunction([Concat([Ref("PreIncrementExpression")]);Concat([Ref("PreDecrementExpression")]);Concat([Ref("AtomicPlus");Ref("UnaryExpression")]);Concat([Ref("AtomicMinus");Ref("UnaryExpression")]);Concat([Ref("UnaryExpressionNotPlusMinus")])]));
   ("PreIncrementExpression",Disjunction([Concat([Ref("AtomicIncr");Ref("UnaryExpression")])]));
   ("PreDecrementExpression",Disjunction([Concat([Ref("AtomicDecr");Ref("UnaryExpression")])]));
   ("UnaryExpressionNotPlusMinus",Disjunction([Concat([Ref("PostfixExpression")]);Concat([Ref("AtomicCompl");Ref("UnaryExpression")]);Concat([Ref("AtomicNot");Ref("UnaryExpression")]);Concat([Ref("CastExpression")]);Concat([Ref("SwitchExpression")])]));
   ("PostfixExpression",Disjunction([Concat([Ref("Primary")]);Concat([Ref("ExpressionName")]);Concat([Ref("PostIncrementExpression")]);Concat([Ref("PostDecrementExpression")])]));
   ("PostIncrementExpression",Disjunction([Concat([Ref("PostfixExpression");Ref("AtomicIncr")])]));
   ("PostDecrementExpression",Disjunction([Concat([Ref("PostfixExpression");Ref("AtomicDecr")])]));
   ("CastExpression",Disjunction([Concat([Ref("AtomicLp");Ref("PrimitiveType");Ref("AtomicRp");Ref("UnaryExpression")]);Concat([Ref("AtomicLp");Ref("ReferenceType");Ref("StarredAdditionalBound");Ref("AtomicRp");Ref("UnaryExpressionNotPlusMinus")]);Concat([Ref("AtomicLp");Ref("ReferenceType");Ref("StarredAdditionalBound");Ref("AtomicRp");Ref("LambdaExpression")])]));
   ("SwitchExpression",Disjunction([Concat([Ref("AtomicSwitch");Ref("AtomicLp");Ref("Expression");Ref("AtomicRp");Ref("SwitchBlock")])]));
   ("ConstantExpression",Disjunction([Concat([Ref("Expression")])]));
   ("AnnotatedIdentifierrPrecededByDot",Disjunction([Concat([Ref("AtomicDot");Ref("StarredAnnotation");Ref("Identifier")])]));
   ("ClassTypePrecededByVerticalBar",Disjunction([Concat([Ref("AtomicOr");Ref("ClassType")])]));
   ("DimsElement",Disjunction([Concat([Ref("StarredAnnotation");Ref("[");Ref("]")])]));
   ("EqualsVariableInitializer",Disjunction([Concat([Ref("AtomicEq");Ref("VariableInitializer")])]));
   ("IdentifierFollowedByDot",Disjunction([Concat([Ref("Identifier");Ref("AtomicDot")])]));
   ("IdentifierPrecededByDot",Disjunction([Concat([Ref("AtomicDot");Ref("Identifier")])]));
   ("OpenSquare",Disjunction([Concat([Ref("[");Ref("]")])]));
   ("ParenthesedArgumentList",Disjunction([Concat([Ref("AtomicLp");Optional("ArgumentList");Ref("AtomicRp")])]));
   ("ToModuleList",Disjunction([Concat([Ref("AtomicTo");Ref("ModuleName");Ref("StarredModuleNamePrecededByComma")])]));
   ("ReceiverParameterFollowedByComma",Disjunction([Concat([Ref("ReceiverParameter");Ref("AtomicCm")])]));
   ("ResourcePrecededBySemiColon",Disjunction([Concat([Ref("AtomicSm");Ref("Resource")])]));
   ("SwitchLabelFollowedByColon",Disjunction([Concat([Ref("SwitchLabel");Ref("AtomicColon")])]));
   ("CaseConstantPrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("CaseConstant")])]));
   ("ElementValuePrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("ElementValue")])]));
   ("ElementValuePairPrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("ElementValuePair")])]));
   ("EnumConstantPrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("EnumConstant")])]));
   ("ExceptionTypePrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("ExceptionType")])]));
   ("ExpressionPrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("Expression")])]));
   ("FormalParameterPrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("FormalParameter")])]));
   ("IdentifierPrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("Identifier")])]));
   ("InterfaceTypePrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("InterfaceType")])]));
   ("LambdaParameterPrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("LambdaParameter")])]));
   ("ModuleNamePrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("ModuleName")])]));
   ("RecordComponentPrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("RecordComponent")])]));
   ("ResourcePrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("Resource")])]));
   ("StatementExpressionPrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("StatementExpression")])]));
   ("TypeArgumentPrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("TypeArgument")])]));
   ("TypeNamePrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("TypeName")])]));
   ("TypeParameterPrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("TypeParameter")])]));
   ("VariableDeclaratorPrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("VariableDeclarator")])]));
   ("VariableInitializerPrecededByComma",Disjunction([Concat([Ref("AtomicCm");Ref("VariableInitializer")])]));
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

]);;


(* Java grammar ends here *)

 end ;;
let java_grammar = Private.java_grammar ;;
