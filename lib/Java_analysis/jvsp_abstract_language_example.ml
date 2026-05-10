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
   Ref of string |Atomic of token_type  |Optional of string ;;

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

   ("Literal",Disjunction([Concat([Ref("IntegerLiteral")]);Concat([Ref("FloatingPointLiteral")]);Concat([Ref("BooleanLiteral")]);Concat([Ref("CharacterLiteral")]);Concat([Ref("StringLiteral")]);Concat([Ref("TextBlock")]);Concat([Ref("NullLiteral")])]));
   ("Type",Disjunction([Concat([Ref("PrimitiveType")]);Concat([Ref("ref_in_diserenceType")])]));
   ("PrimitiveType",Disjunction([Concat([Ref("StarredAnnotation");Ref("NumericType")]);Concat([Ref("StarredAnnotation");Atomic(BOOLEAN_T)])]));
   ("NumericType",Disjunction([Concat([Ref("IntegralType")]);Concat([Ref("FloatingPointType")])]));
   ("IntegralType",Disjunction([Concat([Atomic(BYTE_T)]);Concat([Atomic(SHORT_T)]);Concat([Atomic(INT_T)]);Concat([Atomic(LONG_T)]);Concat([Atomic(CHAR_T)])]));
   ("FloatingPointType",Disjunction([Concat([Atomic(FLOAT_T)]);Concat([Atomic(DOUBLE_T)])]));
   ("ReferenceType",Disjunction([Concat([Ref("ClassOrInterfaceType")]);Concat([Ref("TypeVariable")]);Concat([Ref("ArrayType")])]));
   ("ClassOrInterfaceType",Disjunction([Concat([Ref("ClassType")]);Concat([Ref("InterfaceType")])]));
   ("ClassType",Disjunction([Concat([Ref("StarredAnnotation");Ref("TypeIdentifier");Optional("TypeArguments")]);Concat([Ref("PackageName");Atomic(DOT_T);Ref("StarredAnnotation");Ref("TypeIdentifier");Optional("TypeArguments")]);Concat([Ref("ClassOrInterfaceType");Atomic(DOT_T);Ref("StarredAnnotation");Ref("TypeIdentifier");Optional("TypeArguments")])]));
   ("InterfaceType",Disjunction([Concat([Ref("ClassType")])]));
   ("TypeVariable",Disjunction([Concat([Ref("StarredAnnotation");Ref("TypeIdentifier")])]));
   ("ArrayType",Disjunction([Concat([Ref("PrimitiveType");Ref("Dims")]);Concat([Ref("ClassOrInterfaceType");Ref("Dims")]);Concat([Ref("TypeVariable");Ref("Dims")])]));
   ("Dims",Disjunction([Concat([Ref("DimsElement");Ref("StarredDimsElement")])]));
   ("TypeParameter",Disjunction([Concat([Ref("StarredTypeParameterModifier");Ref("TypeIdentifier");Optional("TypeBound")])]));
   ("TypeParameterModifier",Disjunction([Concat([Ref("Annotation")])]));
   ("TypeBound",Disjunction([Concat([Atomic(EXTENDS_T);Ref("TypeVariable")]);Concat([Atomic(EXTENDS_T);Ref("ClassOrInterfaceType");Ref("StarredAdditionalBound")])]));
   ("AdditionalBound",Disjunction([Concat([Atomic(AND_T);Ref("InterfaceType")])]));
   ("TypeArguments",Disjunction([Concat([Atomic(LT_T);Ref("TypeArgumentList");Atomic(GT_T)])]));
   ("TypeArgumentList",Disjunction([Concat([Ref("TypeArgument");Ref("StarredTypeArgumentPrecededByComma")])]));
   ("TypeArgument",Disjunction([Concat([Ref("ref_in_diserenceType")]);Concat([Ref("Wildcard")])]));
   ("Wildcard",Disjunction([Concat([Ref("StarredAnnotation");Atomic(COND_T);Optional("WildcardBounds")])]));
   ("WildcardBounds",Disjunction([Concat([Atomic(EXTENDS_T);Ref("ReferenceType")]);Concat([Atomic(SUPER_T);Ref("ReferenceType")])]));
   ("ModuleName",Disjunction([Concat([Ref("Identifier")]);Concat([Ref("ModuleName");Ref("IdentifierPrecededByDot")])]));
   ("PackageName",Disjunction([Concat([Ref("Identifier")]);Concat([Ref("PackageName");Ref("IdentifierPrecededByDot")])]));
   ("TypeName",Disjunction([Concat([Ref("TypeIdentifier")]);Concat([Ref("PackageOrTypeName");Atomic(DOT_T);Ref("TypeIdentifier")])]));
   ("ExpressionName",Disjunction([Concat([Ref("Identifier")]);Concat([Ref("AmbiguousName");Ref("IdentifierPrecededByDot")])]));
   ("MethodName",Disjunction([Concat([Ref("UnqualifiedMethodIdentifier")])]));
   ("PackageOrTypeName",Disjunction([Concat([Ref("Identifier")]);Concat([Ref("PackageOrTypeName");Ref("IdentifierPrecededByDot")])]));
   ("AmbiguousName",Disjunction([Concat([Ref("Identifier")]);Concat([Ref("AmbiguousName");Ref("IdentifierPrecededByDot")])]));
   ("CompilationUnit",Disjunction([Concat([Ref("OrdinaryCompilationUnit")]);Concat([Ref("ModularCompilationUnit")])]));
   ("OrdinaryCompilationUnit",Disjunction([Concat([Optional("PackageDeclaration");Ref("StarredImportDeclaration");Ref("StarredTopLevelClassOrInterfaceDeclaration")])]));
   ("ModularCompilationUnit",Disjunction([Concat([Ref("StarredImportDeclaration");Ref("ModuleDeclaration")])]));
   ("PackageDeclaration",Disjunction([Concat([Ref("StarredPackageModifier");Atomic(PACKAGE_T);Ref("Identifier");Ref("StarredIdentifierPrecededByDot");Atomic(SM_T)])]));
   ("PackageModifier",Disjunction([Concat([Ref("Annotation")])]));
   ("ImportDeclaration",Disjunction([Concat([Ref("SingleTypeImportDeclaration")]);Concat([Ref("TypeImportOnDemandDeclaration")]);Concat([Ref("SingleStaticImportDeclaration")]);Concat([Ref("StaticImportOnDemandDeclaration")])]));
   ("SingleTypeImportDeclaration",Disjunction([Concat([Atomic(IMPORT_T);Ref("TypeName");Atomic(SM_T)])]));
   ("TypeImportOnDemandDeclaration",Disjunction([Concat([Atomic(IMPORT_T);Ref("PackageOrTypeName");Atomic(DOT_T);Atomic(TIMES_T);Atomic(SM_T)])]));
   ("SingleStaticImportDeclaration",Disjunction([Concat([Atomic(IMPORT_T);Atomic(STATIC_T);Ref("TypeName");Ref("IdentifierPrecededByDot");Atomic(SM_T)])]));
   ("StaticImportOnDemandDeclaration",Disjunction([Concat([Atomic(IMPORT_T);Atomic(STATIC_T);Ref("TypeName");Atomic(DOT_T);Atomic(TIMES_T);Atomic(SM_T)])]));
   ("TopLevelClassOrInterfaceDeclaration",Disjunction([Concat([Ref("ClassDeclaration")]);Concat([Ref("InterfaceDeclaration")]);Concat([Atomic(SM_T)])]));
   ("ModuleDeclaration",Disjunction([Concat([Ref("StarredAnnotation");Optional("open");Atomic(MODULE_T);Ref("Identifier");Ref("StarredIdentifierPrecededByDot");Ref("{");Ref("StarredModuleDirective");Ref("}")])]));
   ("ModuleDirective",Disjunction([Concat([Atomic(REQUIRES_T);Ref("StarredRequiresModifier");Ref("ModuleName");Atomic(SM_T)]);Concat([Atomic(EXPORTS_T);Ref("PackageName");Optional("ToModuleList");Atomic(SM_T)]);Concat([Atomic(OPENS_T);Ref("PackageName");Optional("ToModuleList");Atomic(SM_T)]);Concat([Atomic(USES_T);Ref("TypeName");Atomic(SM_T)]);Concat([Atomic(PROVIDES_T);Ref("TypeName");Atomic(WITH_T);Ref("TypeName");Ref("StarredTypeNamePrecededByComma");Atomic(SM_T)])]));
   ("RequiresModifier",Disjunction([Concat([Atomic(TRANSITIVE_T)]);Concat([Atomic(STATIC_T)])]));
   ("ClassDeclaration",Disjunction([Concat([Ref("NormalClassDeclaration")]);Concat([Ref("EnumDeclaration")]);Concat([Ref("RecordDeclaration")])]));
   ("NormalClassDeclaration",Disjunction([Concat([Ref("StarredClassModifier");Atomic(CLASS_T);Ref("TypeIdentifier");Optional("TypeParameters");Optional("ClassExtends");Optional("ClassImplements");Optional("ClassPermits");Ref("ClassBody")])]));
   ("ClassModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Atomic(PUBLIC_T)]);Concat([Atomic(PROTECTED_T)]);Concat([Atomic(PRIVATE_T)]);Concat([Atomic(ABSTRACT_T)]);Concat([Atomic(STATIC_T)]);Concat([Atomic(FINAL_T)]);Concat([Atomic(SEALED_T)]);Concat([Atomic(NONSEALED_T)]);Concat([Atomic(STRICTFP_T)])]));
   ("TypeParameters",Disjunction([Concat([Atomic(LT_T);Ref("TypeParameterList");Atomic(GT_T)])]));
   ("TypeParameterList",Disjunction([Concat([Ref("TypeParameter");Ref("StarredTypeParameterPrecededByComma")])]));
   ("ClassExtends",Disjunction([Concat([Atomic(EXTENDS_T);Ref("ClassType")])]));
   ("ClassImplements",Disjunction([Concat([Atomic(IMPLEMENTS_T);Ref("InterfaceTypeList")])]));
   ("InterfaceTypeList",Disjunction([Concat([Ref("InterfaceType");Ref("StarredInterfaceTypePrecededByComma")])]));
   ("ClassPermits",Disjunction([Concat([Atomic(PERMITS_T);Ref("TypeName");Ref("StarredTypeNamePrecededByComma")])]));
   ("ClassBody",Disjunction([Concat([Ref("{");Ref("StarredClassBodyDeclaration");Ref("}")])]));
   ("ClassBodyDeclaration",Disjunction([Concat([Ref("ClassMemberDeclaration")]);Concat([Ref("InstanceInitializer")]);Concat([Ref("StaticInitializer")]);Concat([Ref("ConstructorDeclaration")])]));
   ("ClassMemberDeclaration",Disjunction([Concat([Ref("FieldDeclaration")]);Concat([Ref("MethodDeclaration")]);Concat([Ref("ClassDeclaration")]);Concat([Ref("InterfaceDeclaration")]);Concat([Atomic(SM_T)])]));
   ("FieldDeclaration",Disjunction([Concat([Ref("StarredFieldModifier");Ref("UnannType");Ref("VariableDeclaratorList");Atomic(SM_T)])]));
   ("FieldModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Atomic(PUBLIC_T)]);Concat([Atomic(PROTECTED_T)]);Concat([Atomic(PRIVATE_T)]);Concat([Atomic(STATIC_T)]);Concat([Atomic(FINAL_T)]);Concat([Atomic(TRANSIENT_T)]);Concat([Atomic(VOLATILE_T)])]));
   ("VariableDeclaratorList",Disjunction([Concat([Ref("VariableDeclarator");Ref("StarredVariableDeclaratorPrecededByComma")])]));
   ("VariableDeclarator",Disjunction([Concat([Ref("VariableDeclaratorId");Optional("EqualsVariableInitializer")])]));
   ("VariableDeclaratorId",Disjunction([Concat([Ref("Identifier");Optional("Dims")])]));
   ("VariableInitializer",Disjunction([Concat([Ref("Expression")]);Concat([Ref("ArrayInitializer")])]));
   ("UnannType",Disjunction([Concat([Ref("UnannPrimitiveType")]);Concat([Ref("Unannref_in_diserenceType")])]));
   ("UnannPrimitiveType",Disjunction([Concat([Ref("NumericType")]);Concat([Atomic(BOOLEAN_T)])]));
   ("UnannReferenceType",Disjunction([Concat([Ref("UnannClassOrInterfaceType")]);Concat([Ref("UnannTypeVariable")]);Concat([Ref("UnannArrayType")])]));
   ("UnannClassOrInterfaceType",Disjunction([Concat([Ref("UnannClassType")]);Concat([Ref("UnannInterfaceType")])]));
   ("UnannClassType",Disjunction([Concat([Ref("TypeIdentifier");Optional("TypeArguments")]);Concat([Ref("PackageName");Atomic(DOT_T);Ref("StarredAnnotation");Ref("TypeIdentifier");Optional("TypeArguments")]);Concat([Ref("UnannClassOrInterfaceType");Atomic(DOT_T);Ref("StarredAnnotation");Ref("TypeIdentifier");Optional("TypeArguments")])]));
   ("UnannInterfaceType",Disjunction([Concat([Ref("UnannClassType")])]));
   ("UnannTypeVariable",Disjunction([Concat([Ref("TypeIdentifier")])]));
   ("UnannArrayType",Disjunction([Concat([Ref("UnannPrimitiveType");Ref("Dims")]);Concat([Ref("UnannClassOrInterfaceType");Ref("Dims")]);Concat([Ref("UnannTypeVariable");Ref("Dims")])]));
   ("MethodDeclaration",Disjunction([Concat([Ref("StarredMethodModifier");Ref("MethodHeader");Ref("MethodBody")])]));
   ("MethodModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Atomic(PUBLIC_T)]);Concat([Atomic(PROTECTED_T)]);Concat([Atomic(PRIVATE_T)]);Concat([Atomic(ABSTRACT_T)]);Concat([Atomic(STATIC_T)]);Concat([Atomic(FINAL_T)]);Concat([Atomic(SYNCHRONIZED_T)]);Concat([Atomic(NATIVE_T)]);Concat([Atomic(STRICTFP_T)])]));
   ("MethodHeader",Disjunction([Concat([Ref("Result");Ref("MethodDeclarator");Optional("Throws")]);Concat([Ref("TypeParameters");Ref("StarredAnnotation");Ref("Result");Ref("MethodDeclarator");Optional("Throws")])]));
   ("Result",Disjunction([Concat([Ref("UnannType")]);Concat([Atomic(VOID_T)])]));
   ("MethodDeclarator",Disjunction([Concat([Ref("Identifier");Atomic(LP_T);Optional("ReceiverParameterFollowedByComma");Optional("FormalParameterList");Atomic(RP_T);Optional("Dims")])]));
   ("ReceiverParameter",Disjunction([Concat([Ref("StarredAnnotation");Ref("UnannType");Optional("IdentifierFollowedByDot");Atomic(THIS_T)])]));
   ("FormalParameterList",Disjunction([Concat([Ref("FormalParameter");Ref("StarredFormalParameterPrecededByComma")])]));
   ("FormalParameter",Disjunction([Concat([Ref("StarredVariableModifier");Ref("UnannType");Ref("VariableDeclaratorId")]);Concat([Ref("VariableArityParameter")])]));
   ("VariableArityParameter",Disjunction([Concat([Ref("StarredVariableModifier");Ref("UnannType");Ref("StarredAnnotation");Atomic(DOT_T);Atomic(DOT_T);Atomic(DOT_T);Ref("Identifier")])]));
   ("VariableModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Atomic(FINAL_T)])]));
   ("Throws",Disjunction([Concat([Atomic(THROWS_T);Ref("ExceptionTypeList")])]));
   ("ExceptionTypeList",Disjunction([Concat([Ref("ExceptionType");Ref("StarredExceptionTypePrecededByComma")])]));
   ("ExceptionType",Disjunction([Concat([Ref("ClassType")]);Concat([Ref("TypeVariable")])]));
   ("MethodBody",Disjunction([Concat([Ref("Block")]);Concat([Atomic(SM_T)])]));
   ("InstanceInitializer",Disjunction([Concat([Ref("Block")])]));
   ("StaticInitializer",Disjunction([Concat([Atomic(STATIC_T);Ref("Block")])]));
   ("ConstructorDeclaration",Disjunction([Concat([Ref("StarredConstructorModifier");Ref("ConstructorDeclarator");Optional("Throws");Ref("ConstructorBody")])]));
   ("ConstructorModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Atomic(PUBLIC_T)]);Concat([Atomic(PROTECTED_T)]);Concat([Atomic(PRIVATE_T)])]));
   ("ConstructorDeclarator",Disjunction([Concat([Optional("TypeParameters");Ref("SimpleTypeName");Atomic(LP_T);Optional("ReceiverParameterFollowedByComma");Optional("FormalParameterList");Atomic(RP_T)])]));
   ("SimpleTypeName",Disjunction([Concat([Ref("TypeIdentifier")])]));
   ("ConstructorBody",Disjunction([Concat([Ref("{");Optional("ExplicitConstructorInvocation");Optional("BlockStatements");Ref("}")])]));
   ("ExplicitConstructorInvocation",Disjunction([Concat([Optional("TypeArguments");Atomic(THIS_T);Ref("ParenthesedArgumentList");Atomic(SM_T)]);Concat([Optional("TypeArguments");Atomic(SUPER_T);Ref("ParenthesedArgumentList");Atomic(SM_T)]);Concat([Ref("ExpressionName");Atomic(DOT_T);Optional("TypeArguments");Atomic(SUPER_T);Ref("ParenthesedArgumentList");Atomic(SM_T)]);Concat([Ref("Primary");Atomic(DOT_T);Optional("TypeArguments");Atomic(SUPER_T);Ref("ParenthesedArgumentList");Atomic(SM_T)])]));
   ("EnumDeclaration",Disjunction([Concat([Ref("StarredClassModifier");Atomic(ENUM_T);Ref("TypeIdentifier");Optional("ClassImplements");Ref("EnumBody")])]));
   ("EnumBody",Disjunction([Concat([Ref("{");Optional("EnumConstantList");Optional(",");Optional("EnumBodyDeclarations");Ref("}")])]));
   ("EnumConstantList",Disjunction([Concat([Ref("EnumConstant");Ref("StarredEnumConstantPrecededByComma")])]));
   ("EnumConstant",Disjunction([Concat([Ref("StarredEnumConstantModifier");Ref("Identifier");Optional("ParenthesedArgumentList");Optional("ClassBody")])]));
   ("EnumConstantModifier",Disjunction([Concat([Ref("Annotation")])]));
   ("EnumBodyDeclarations",Disjunction([Concat([Atomic(SM_T);Ref("StarredClassBodyDeclaration")])]));
   ("RecordDeclaration",Disjunction([Concat([Ref("StarredClassModifier");Atomic(RECORD_T);Ref("TypeIdentifier");Optional("TypeParameters");Ref("RecordHeader");Optional("ClassImplements");Ref("RecordBody")])]));
   ("RecordHeader",Disjunction([Concat([Atomic(LP_T);Optional("RecordComponentList");Atomic(RP_T)])]));
   ("RecordComponentList",Disjunction([Concat([Ref("RecordComponent");Ref("StarredRecordComponentPrecededByComma")])]));
   ("RecordComponent",Disjunction([Concat([Ref("StarredRecordComponentModifier");Ref("UnannType");Ref("Identifier")]);Concat([Ref("VariableArityRecordComponent")])]));
   ("VariableArityRecordComponent",Disjunction([Concat([Ref("StarredRecordComponentModifier");Ref("UnannType");Ref("StarredAnnotation");Atomic(DOT_T);Atomic(DOT_T);Atomic(DOT_T);Ref("Identifier")])]));
   ("RecordComponentModifier",Disjunction([Concat([Ref("Annotation")])]));
   ("RecordBody",Disjunction([Concat([Ref("{");Ref("StarredRecordBodyDeclaration");Ref("}")])]));
   ("RecordBodyDeclaration",Disjunction([Concat([Ref("ClassBodyDeclaration")]);Concat([Ref("CompactConstructorDeclaration")])]));
   ("CompactConstructorDeclaration",Disjunction([Concat([Ref("StarredConstructorModifier");Ref("SimpleTypeName");Ref("ConstructorBody")])]));
   ("InterfaceDeclaration",Disjunction([Concat([Ref("NormalInterfaceDeclaration")]);Concat([Ref("AnnotationInterfaceDeclaration")])]));
   ("NormalInterfaceDeclaration",Disjunction([Concat([Ref("StarredInterfaceModifier");Atomic(INTERFACE_T);Ref("TypeIdentifier");Optional("TypeParameters");Optional("InterfaceExtends");Optional("InterfacePermits");Ref("InterfaceBody")])]));
   ("InterfaceModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Atomic(PUBLIC_T)]);Concat([Atomic(PROTECTED_T)]);Concat([Atomic(PRIVATE_T)]);Concat([Atomic(ABSTRACT_T)]);Concat([Atomic(STATIC_T)]);Concat([Atomic(SEALED_T)]);Concat([Atomic(NONSEALED_T)]);Concat([Atomic(STRICTFP_T)])]));
   ("InterfaceExtends",Disjunction([Concat([Atomic(EXTENDS_T);Ref("InterfaceTypeList")])]));
   ("InterfacePermits",Disjunction([Concat([Atomic(PERMITS_T);Ref("TypeName");Ref("StarredTypeNamePrecededByComma")])]));
   ("InterfaceBody",Disjunction([Concat([Ref("{");Ref("StarredInterfaceMemberDeclaration");Ref("}")])]));
   ("InterfaceMemberDeclaration",Disjunction([Concat([Ref("ConstantDeclaration")]);Concat([Ref("InterfaceMethodDeclaration")]);Concat([Ref("ClassDeclaration")]);Concat([Ref("InterfaceDeclaration")]);Concat([Atomic(SM_T)])]));
   ("ConstantDeclaration",Disjunction([Concat([Ref("StarredConstantModifier");Ref("UnannType");Ref("VariableDeclaratorList");Atomic(SM_T)])]));
   ("ConstantModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Atomic(PUBLIC_T)]);Concat([Atomic(STATIC_T)]);Concat([Atomic(FINAL_T)])]));
   ("InterfaceMethodDeclaration",Disjunction([Concat([Ref("StarredInterfaceMethodModifier");Ref("MethodHeader");Ref("MethodBody")])]));
   ("InterfaceMethodModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Atomic(PUBLIC_T)]);Concat([Atomic(PRIVATE_T)]);Concat([Atomic(ABSTRACT_T)]);Concat([Atomic(DEFAULT_T)]);Concat([Atomic(STATIC_T)]);Concat([Atomic(STRICTFP_T)])]));
   ("AnnotationInterfaceDeclaration",Disjunction([Concat([Ref("StarredInterfaceModifier");Atomic(SNAIL_T);Atomic(INTERFACE_T);Ref("TypeIdentifier");Ref("AnnotationInterfaceBody")])]));
   ("AnnotationInterfaceBody",Disjunction([Concat([Ref("{");Ref("StarredAnnotationInterfaceMemberDeclaration");Ref("}")])]));
   ("AnnotationInterfaceMemberDeclaration",Disjunction([Concat([Ref("AnnotationInterfaceElementDeclaration")]);Concat([Ref("ConstantDeclaration")]);Concat([Ref("ClassDeclaration")]);Concat([Ref("InterfaceDeclaration")]);Concat([Atomic(SM_T)])]));
   ("AnnotationInterfaceElementDeclaration",Disjunction([Concat([Ref("StarredAnnotationInterfaceElementModifier");Ref("UnannType");Ref("Identifier");Atomic(LP_T);Atomic(RP_T);Optional("Dims");Optional("DefaultValue");Atomic(SM_T)])]));
   ("AnnotationInterfaceElementModifier",Disjunction([Concat([Ref("Annotation")]);Concat([Atomic(PUBLIC_T)]);Concat([Atomic(ABSTRACT_T)])]));
   ("DefaultValue",Disjunction([Concat([Atomic(DEFAULT_T);Ref("ElementValue")])]));
   ("Annotation",Disjunction([Concat([Ref("NormalAnnotation")]);Concat([Ref("MarkerAnnotation")]);Concat([Ref("SingleElementAnnotation")])]));
   ("NormalAnnotation",Disjunction([Concat([Atomic(SNAIL_T);Ref("TypeName");Atomic(LP_T);Optional("ElementValuePairList");Atomic(RP_T)])]));
   ("ElementValuePairList",Disjunction([Concat([Ref("ElementValuePair");Ref("StarredElementValuePrecededByCommaPair")])]));
   ("ElementValuePair",Disjunction([Concat([Ref("Identifier");Atomic(EQ_T);Ref("ElementValue")])]));
   ("ElementValue",Disjunction([Concat([Ref("ConditionalExpression")]);Concat([Ref("ElementValueArrayInitializer")]);Concat([Ref("Annotation")])]));
   ("ElementValueArrayInitializer",Disjunction([Concat([Ref("{");Optional("ElementValueList");Optional(",");Ref("}")])]));
   ("ElementValueList",Disjunction([Concat([Ref("ElementValue");Ref("StarredElementValuePrecededByComma")])]));
   ("MarkerAnnotation",Disjunction([Concat([Atomic(SNAIL_T);Ref("TypeName")])]));
   ("SingleElementAnnotation",Disjunction([Concat([Atomic(SNAIL_T);Ref("TypeName");Atomic(LP_T);Ref("ElementValue");Atomic(RP_T)])]));
   ("ArrayInitializer",Disjunction([Concat([Ref("{");Optional("VariableInitializerList");Optional(",");Ref("}")])]));
   ("VariableInitializerList",Disjunction([Concat([Ref("VariableInitializer");Ref("StarredVariableInitializerPrecededByComma")])]));
   ("Block",Disjunction([Concat([Ref("{");Optional("BlockStatements");Ref("}")])]));
   ("BlockStatements",Disjunction([Concat([Ref("BlockStatement");Ref("StarredBlockStatement")])]));
   ("BlockStatement",Disjunction([Concat([Ref("LocalClassOrInterfaceDeclaration")]);Concat([Ref("LocalVariableDeclarationStatement")]);Concat([Ref("Statement")])]));
   ("LocalClassOrInterfaceDeclaration",Disjunction([Concat([Ref("ClassDeclaration")]);Concat([Ref("NormalInterfaceDeclaration")])]));
   ("LocalVariableDeclarationStatement",Disjunction([Concat([Ref("LocalVariableDeclaration");Atomic(SM_T)])]));
   ("LocalVariableDeclaration",Disjunction([Concat([Ref("StarredVariableModifier");Ref("LocalVariableType");Ref("VariableDeclaratorList")])]));
   ("LocalVariableType",Disjunction([Concat([Ref("UnannType")]);Concat([Atomic(VAR_T)])]));
   ("Statement",Disjunction([Concat([Ref("StatementWithoutTrailingSubstatement")]);Concat([Ref("LabeledStatement")]);Concat([Ref("IfThenStatement")]);Concat([Ref("IfThenElseStatement")]);Concat([Ref("WhileStatement")]);Concat([Ref("ForStatement")])]));
   ("StatementNoShortIf",Disjunction([Concat([Ref("StatementWithoutTrailingSubstatement")]);Concat([Ref("LabeledStatementNoShortIf")]);Concat([Ref("IfThenElseStatementNoShortIf")]);Concat([Ref("WhileStatementNoShortIf")]);Concat([Ref("ForStatementNoShortIf")])]));
   ("StatementWithoutTrailingSubstatement",Disjunction([Concat([Ref("Block")]);Concat([Ref("EmptyStatement")]);Concat([Ref("ExpressionStatement")]);Concat([Ref("AssertStatement")]);Concat([Ref("SwitchStatement")]);Concat([Ref("DoStatement")]);Concat([Ref("BreakStatement")]);Concat([Ref("ContinueStatement")]);Concat([Ref("ReturnStatement")]);Concat([Ref("SynchronizedStatement")]);Concat([Ref("ThrowStatement")]);Concat([Ref("TryStatement")]);Concat([Ref("YieldStatement")])]));
   ("EmptyStatement",Disjunction([Concat([Ref(";")])]));
   ("LabeledStatement",Disjunction([Concat([Ref("Identifier");Atomic(COLON_T);Ref("Statement")])]));
   ("LabeledStatementNoShortIf",Disjunction([Concat([Ref("Identifier");Atomic(COLON_T);Ref("StatementNoShortIf")])]));
   ("ExpressionStatement",Disjunction([Concat([Ref("StatementExpression");Atomic(SM_T)])]));
   ("StatementExpression",Disjunction([Concat([Ref("Assignment")]);Concat([Ref("PreIncrementExpression")]);Concat([Ref("PreDecrementExpression")]);Concat([Ref("PostIncrementExpression")]);Concat([Ref("PostDecrementExpression")]);Concat([Ref("MethodInvocation")]);Concat([Ref("ClassInstanceCreationExpression")])]));
   ("IfThenStatement",Disjunction([Concat([Atomic(IF_T);Atomic(LP_T);Ref("Expression");Atomic(RP_T);Ref("Statement")])]));
   ("IfThenElseStatement",Disjunction([Concat([Atomic(IF_T);Atomic(LP_T);Ref("Expression");Atomic(RP_T);Ref("StatementNoShortIf");Atomic(ELSE_T);Ref("Statement")])]));
   ("IfThenElseStatementNoShortIf",Disjunction([Concat([Atomic(IF_T);Atomic(LP_T);Ref("Expression");Atomic(RP_T);Ref("StatementNoShortIf");Atomic(ELSE_T);Ref("StatementNoShortIf")])]));
   ("AssertStatement",Disjunction([Concat([Atomic(ASSERT_T);Ref("Expression");Atomic(SM_T)]);Concat([Atomic(ASSERT_T);Ref("Expression");Atomic(COLON_T);Ref("Expression");Atomic(SM_T)])]));
   ("SwitchStatement",Disjunction([Concat([Atomic(SWITCH_T);Atomic(LP_T);Ref("Expression");Atomic(RP_T);Ref("SwitchBlock")])]));
   ("SwitchBlock",Disjunction([Concat([Ref("{");Ref("SwitchRule");Ref("StarredSwitchRule");Ref("}")]);Concat([Ref("{");Ref("StarredSwitchBlockStatementGroup");Ref("StarredSwitchLabelFollowedByColon");Ref("}")])]));
   ("SwitchRule",Disjunction([Concat([Ref("SwitchLabel");Atomic(MINUS_T);Atomic(GT_T);Ref("Expression");Atomic(SM_T)]);Concat([Ref("SwitchLabel");Atomic(MINUS_T);Atomic(GT_T);Ref("Block")]);Concat([Ref("SwitchLabel");Atomic(MINUS_T);Atomic(GT_T);Ref("ThrowStatement")])]));
   ("SwitchBlockStatementGroup",Disjunction([Concat([Ref("SwitchLabelFollowedByColon");Ref("StarredSwitchLabelFollowedByColon");Ref("BlockStatements")])]));
   ("SwitchLabel",Disjunction([Concat([Atomic(CASE_T);Ref("CaseConstant");Ref("StarredCaseConstantPrecededByComma")]);Concat([Atomic(DEFAULT_T)])]));
   ("CaseConstant",Disjunction([Concat([Ref("ConditionalExpression")])]));
   ("WhileStatement",Disjunction([Concat([Atomic(WHILE_T);Atomic(LP_T);Ref("Expression");Atomic(RP_T);Ref("Statement")])]));
   ("WhileStatementNoShortIf",Disjunction([Concat([Atomic(WHILE_T);Atomic(LP_T);Ref("Expression");Atomic(RP_T);Ref("StatementNoShortIf")])]));
   ("DoStatement",Disjunction([Concat([Atomic(DO_T);Ref("Statement");Atomic(WHILE_T);Atomic(LP_T);Ref("Expression");Atomic(RP_T);Atomic(SM_T)])]));
   ("ForStatement",Disjunction([Concat([Ref("BasicForStatement")]);Concat([Ref("EnhancedForStatement")])]));
   ("ForStatementNoShortIf",Disjunction([Concat([Ref("BasicForStatementNoShortIf")]);Concat([Ref("EnhancedForStatementNoShortIf")])]));
   ("BasicForStatement",Disjunction([Concat([Atomic(FOR_T);Atomic(LP_T);Optional("ForInit");Atomic(SM_T);Optional("Expression");Atomic(SM_T);Optional("ForUpdate");Atomic(RP_T);Ref("Statement")])]));
   ("BasicForStatementNoShortIf",Disjunction([Concat([Atomic(FOR_T);Atomic(LP_T);Optional("ForInit");Atomic(SM_T);Optional("Expression");Atomic(SM_T);Optional("ForUpdate");Atomic(RP_T);Ref("StatementNoShortIf")])]));
   ("ForInit",Disjunction([Concat([Ref("StatementExpressionList")]);Concat([Ref("LocalVariableDeclaration")])]));
   ("ForUpdate",Disjunction([Concat([Ref("StatementExpressionList")])]));
   ("StatementExpressionList",Disjunction([Concat([Ref("StatementExpression");Ref("StarredStatementExpressionPrecededByComma")])]));
   ("EnhancedForStatement",Disjunction([Concat([Atomic(FOR_T);Atomic(LP_T);Ref("LocalVariableDeclaration");Atomic(COLON_T);Ref("Expression");Atomic(RP_T);Ref("Statement")])]));
   ("EnhancedForStatementNoShortIf",Disjunction([Concat([Atomic(FOR_T);Atomic(LP_T);Ref("LocalVariableDeclaration");Atomic(COLON_T);Ref("Expression");Atomic(RP_T);Ref("StatementNoShortIf")])]));
   ("BreakStatement",Disjunction([Concat([Atomic(BREAK_T);Optional("Identifier");Atomic(SM_T)])]));
   ("YieldStatement",Disjunction([Concat([Atomic(YIELD_T);Ref("Expression");Atomic(SM_T)])]));
   ("ContinueStatement",Disjunction([Concat([Atomic(CONTINUE_T);Optional("Identifier");Atomic(SM_T)])]));
   ("ReturnStatement",Disjunction([Concat([Atomic(RETURN_T);Optional("Expression");Atomic(SM_T)])]));
   ("ThrowStatement",Disjunction([Concat([Atomic(THROW_T);Ref("Expression");Atomic(SM_T)])]));
   ("SynchronizedStatement",Disjunction([Concat([Atomic(SYNCHRONIZED_T);Atomic(LP_T);Ref("Expression");Atomic(RP_T);Ref("Block")])]));
   ("TryStatement",Disjunction([Concat([Atomic(TRY_T);Ref("Block");Ref("Catches")]);Concat([Atomic(TRY_T);Ref("Block");Optional("Catches");Ref("Finally")]);Concat([Ref("TryWithResourcesStatement")])]));
   ("Catches",Disjunction([Concat([Ref("CatchClause");Ref("StarredCatchClause")])]));
   ("CatchClause",Disjunction([Concat([Atomic(CATCH_T);Atomic(LP_T);Ref("CatchFormalParameter");Atomic(RP_T);Ref("Block")])]));
   ("CatchFormalParameter",Disjunction([Concat([Ref("StarredVariableModifier");Ref("CatchType");Ref("VariableDeclaratorId")])]));
   ("CatchType",Disjunction([Concat([Ref("UnannClassType");Ref("StarredClassTypePrecededByVerticalBar")])]));
   ("Finally",Disjunction([Concat([Atomic(FINALLY_T);Ref("Block")])]));
   ("TryWithResourcesStatement",Disjunction([Concat([Atomic(TRY_T);Ref("ResourceSpecification");Ref("Block");Optional("Catches");Optional("Finally")])]));
   ("ResourceSpecification",Disjunction([Concat([Atomic(LP_T);Ref("ResourceList");Optional(";");Atomic(RP_T)])]));
   ("ResourceList",Disjunction([Concat([Ref("Resource");Ref("StarredResourcePrecededBySemiColon")])]));
   ("Resource",Disjunction([Concat([Ref("LocalVariableDeclaration")]);Concat([Ref("VariableAccess")])]));
   ("Pattern",Disjunction([Concat([Ref("TypePattern")])]));
   ("TypePattern",Disjunction([Concat([Ref("LocalVariableDeclaration")])]));
   ("Primary",Disjunction([Concat([Ref("PrimaryNoNewArray")]);Concat([Ref("ArrayCreationExpression")])]));
   ("PrimaryNoNewArray",Disjunction([Concat([Ref("Literal")]);Concat([Ref("ClassLiteral")]);Concat([Atomic(THIS_T)]);Concat([Ref("TypeName");Atomic(DOT_T);Atomic(THIS_T)]);Concat([Atomic(LP_T);Ref("Expression");Atomic(RP_T)]);Concat([Ref("ClassInstanceCreationExpression")]);Concat([Ref("FieldAccess")]);Concat([Ref("ArrayAccess")]);Concat([Ref("MethodInvocation")]);Concat([Ref("Methodref_in_diserence")])]));
   ("ClassLiteral",Disjunction([Concat([Ref("TypeName");Ref("StarredOpenSquare");Atomic(DOT_T);Atomic(CLASS_T)]);Concat([Ref("NumericType");Ref("StarredOpenSquare");Atomic(DOT_T);Atomic(CLASS_T)]);Concat([Atomic(BOOLEAN_T);Ref("StarredOpenSquare");Atomic(DOT_T);Atomic(CLASS_T)]);Concat([Atomic(VOID_T);Atomic(DOT_T);Atomic(CLASS_T)])]));
   ("ClassInstanceCreationExpression",Disjunction([Concat([Ref("UnqualifiedClassInstanceCreationExpression")]);Concat([Ref("ExpressionName");Atomic(DOT_T);Ref("UnqualifiedClassInstanceCreationExpression")]);Concat([Ref("Primary");Atomic(DOT_T);Ref("UnqualifiedClassInstanceCreationExpression")])]));
   ("UnqualifiedClassInstanceCreationExpression",Disjunction([Concat([Atomic(NEW_T);Optional("TypeArguments");Ref("ClassOrInterfaceTypeToInstantiate");Ref("ParenthesedArgumentList");Optional("ClassBody")])]));
   ("ClassOrInterfaceTypeToInstantiate",Disjunction([Concat([Ref("StarredAnnotation");Ref("Identifier");Ref("StarredAnnotatedIdentifierrPrecededByDot");Optional("TypeArgumentsOrDiamond")])]));
   ("TypeArgumentsOrDiamond",Disjunction([Concat([Ref("TypeArguments")]);Concat([Atomic(LT_T);Atomic(GT_T)])]));
   ("FieldAccess",Disjunction([Concat([Ref("Primary");Ref("IdentifierPrecededByDot")]);Concat([Atomic(SUPER_T);Ref("IdentifierPrecededByDot")]);Concat([Ref("TypeName");Atomic(DOT_T);Atomic(SUPER_T);Ref("IdentifierPrecededByDot")])]));
   ("ArrayAccess",Disjunction([Concat([Ref("ExpressionName");Ref("[");Ref("Expression");Ref("]")]);Concat([Ref("PrimaryNoNewArray");Ref("[");Ref("Expression");Ref("]")])]));
   ("MethodInvocation",Disjunction([Concat([Ref("MethodName");Ref("ParenthesedArgumentList")]);Concat([Ref("TypeName");Atomic(DOT_T);Optional("TypeArguments");Ref("Identifier");Ref("ParenthesedArgumentList")]);Concat([Ref("ExpressionName");Atomic(DOT_T);Optional("TypeArguments");Ref("Identifier");Ref("ParenthesedArgumentList")]);Concat([Ref("Primary");Atomic(DOT_T);Optional("TypeArguments");Ref("Identifier");Ref("ParenthesedArgumentList")]);Concat([Atomic(SUPER_T);Atomic(DOT_T);Optional("TypeArguments");Ref("Identifier");Ref("ParenthesedArgumentList")]);Concat([Ref("TypeName");Atomic(DOT_T);Atomic(SUPER_T);Atomic(DOT_T);Optional("TypeArguments");Ref("Identifier");Ref("ParenthesedArgumentList")])]));
   ("ArgumentList",Disjunction([Concat([Ref("Expression");Ref("StarredExpressionPrecededByComma")])]));
   ("MethodReference",Disjunction([Concat([Ref("ExpressionName");Atomic(COLON_T);Atomic(COLON_T);Optional("TypeArguments");Ref("Identifier")]);Concat([Ref("Primary");Atomic(COLON_T);Atomic(COLON_T);Optional("TypeArguments");Ref("Identifier")]);Concat([Ref("ReferenceType");Atomic(COLON_T);Atomic(COLON_T);Optional("TypeArguments");Ref("Identifier")]);Concat([Atomic(SUPER_T);Atomic(COLON_T);Atomic(COLON_T);Optional("TypeArguments");Ref("Identifier")]);Concat([Ref("TypeName");Atomic(DOT_T);Atomic(SUPER_T);Atomic(COLON_T);Atomic(COLON_T);Optional("TypeArguments");Ref("Identifier")]);Concat([Ref("ClassType");Atomic(COLON_T);Atomic(COLON_T);Optional("TypeArguments");Atomic(NEW_T)]);Concat([Ref("ArrayType");Atomic(COLON_T);Atomic(COLON_T);Atomic(NEW_T)])]));
   ("ArrayCreationExpression",Disjunction([Concat([Atomic(NEW_T);Ref("PrimitiveType");Ref("DimExprs");Optional("Dims")]);Concat([Atomic(NEW_T);Ref("ClassOrInterfaceType");Ref("DimExprs");Optional("Dims")]);Concat([Atomic(NEW_T);Ref("PrimitiveType");Ref("Dims");Ref("ArrayInitializer")]);Concat([Atomic(NEW_T);Ref("ClassOrInterfaceType");Ref("Dims");Ref("ArrayInitializer")])]));
   ("DimExprs",Disjunction([Concat([Ref("DimExpr");Ref("StarredDimExpr")])]));
   ("DimExpr",Disjunction([Concat([Ref("StarredAnnotation");Ref("[");Ref("Expression");Ref("]")])]));
   ("Expression",Disjunction([Concat([Ref("LambdaExpression")]);Concat([Ref("AssignmentExpression")])]));
   ("LambdaExpression",Disjunction([Concat([Ref("LambdaParameters");Atomic(MINUS_T);Atomic(GT_T);Ref("LambdaBody")])]));
   ("LambdaParameters",Disjunction([Concat([Atomic(LP_T);Optional("LambdaParameterList");Atomic(RP_T)]);Concat([Ref("Identifier")])]));
   ("LambdaParameterList",Disjunction([Concat([Ref("LambdaParameter");Ref("StarredLambdaParameterPrecededByComma")]);Concat([Ref("Identifier");Ref("StarredIdentifierPrecededByComma")])]));
   ("LambdaParameter",Disjunction([Concat([Ref("StarredVariableModifier");Ref("LambdaParameterType");Ref("VariableDeclaratorId")]);Concat([Ref("VariableArityParameter")])]));
   ("LambdaParameterType",Disjunction([Concat([Ref("UnannType")]);Concat([Atomic(VAR_T)])]));
   ("LambdaBody",Disjunction([Concat([Ref("Expression")]);Concat([Ref("Block")])]));
   ("AssignmentExpression",Disjunction([Concat([Ref("ConditionalExpression")]);Concat([Ref("Assignment")])]));
   ("Assignment",Disjunction([Concat([Ref("LeftHandSide");Atomic(OPERATOR_EQ_T);Ref("Expression")])]));
   ("LeftHandSide",Disjunction([Concat([Ref("ExpressionName")]);Concat([Ref("FieldAccess")]);Concat([Ref("ArrayAccess")])]));
   ("",Disjunction([]));
   ("ConditionalExpression",Disjunction([Concat([Ref("ConditionalOrExpression")]);Concat([Ref("ConditionalOrExpression");Atomic(COND_T);Ref("Expression");Atomic(COLON_T);Ref("ConditionalExpression")]);Concat([Ref("ConditionalOrExpression");Atomic(COND_T);Ref("Expression");Atomic(COLON_T);Ref("LambdaExpression")])]));
   ("ConditionalOrExpression",Disjunction([Concat([Ref("ConditionalAndExpression")]);Concat([Ref("ConditionalOrExpression");Atomic(OR_OR_T);Ref("ConditionalAndExpression")])]));
   ("ConditionalAndExpression",Disjunction([Concat([Ref("InclusiveOrExpression")]);Concat([Ref("ConditionalAndExpression");Atomic(AND_AND_T);Ref("InclusiveOrExpression")])]));
   ("InclusiveOrExpression",Disjunction([Concat([Ref("ExclusiveOrExpression")]);Concat([Ref("InclusiveOrExpression");Atomic(OR_T);Ref("ExclusiveOrExpression")])]));
   ("ExclusiveOrExpression",Disjunction([Concat([Ref("AndExpression")]);Concat([Ref("ExclusiveOrExpression");Atomic(XOR_T);Ref("AndExpression")])]));
   ("AndExpression",Disjunction([Concat([Ref("EqualityExpression")]);Concat([Ref("AndExpression");Atomic(AND_T);Ref("EqualityExpression")])]));
   ("EqualityExpression",Disjunction([Concat([Ref("RelationalExpression")]);Concat([Ref("EqualityExpression");Atomic(EQ_EQ_T);Ref("RelationalExpression")]);Concat([Ref("EqualityExpression");Atomic(NOT_EQ_T);Ref("RelationalExpression")])]));
   ("RelationalExpression",Disjunction([Concat([Ref("ShiftExpression")]);Concat([Ref("RelationalExpression");Atomic(LT_T);Ref("ShiftExpression")]);Concat([Ref("RelationalExpression");Atomic(GT_T);Ref("ShiftExpression")]);Concat([Ref("RelationalExpression");Atomic(LE_T);Ref("ShiftExpression")]);Concat([Ref("RelationalExpression");Atomic(GE_T);Ref("ShiftExpression")]);Concat([Ref("InstanceofExpression")])]));
   ("InstanceofExpression",Disjunction([Concat([Ref("RelationalExpression");Atomic(INSTANCEOF_T);Ref("ReferenceType")]);Concat([Ref("RelationalExpression");Atomic(INSTANCEOF_T);Ref("Pattern")])]));
   ("ShiftExpression",Disjunction([Concat([Ref("AdditiveExpression")]);Concat([Ref("ShiftExpression");Atomic(LS_T);Ref("AdditiveExpression")]);Concat([Ref("ShiftExpression");Atomic(SRS_T);Ref("AdditiveExpression")]);Concat([Ref("ShiftExpression");Atomic(URS_T);Ref("AdditiveExpression")])]));
   ("AdditiveExpression",Disjunction([Concat([Ref("MultiplicativeExpression")]);Concat([Ref("AdditiveExpression");Atomic(PLUS_T);Ref("MultiplicativeExpression")]);Concat([Ref("AdditiveExpression");Atomic(MINUS_T);Ref("MultiplicativeExpression")])]));
   ("MultiplicativeExpression",Disjunction([Concat([Ref("UnaryExpression")]);Concat([Ref("MultiplicativeExpression");Atomic(TIMES_T);Ref("UnaryExpression")]);Concat([Ref("MultiplicativeExpression");Atomic(DIV_T);Ref("UnaryExpression")]);Concat([Ref("MultiplicativeExpression");Atomic(MOD_T);Ref("UnaryExpression")])]));
   ("UnaryExpression",Disjunction([Concat([Ref("PreIncrementExpression")]);Concat([Ref("PreDecrementExpression")]);Concat([Atomic(PLUS_T);Ref("UnaryExpression")]);Concat([Atomic(MINUS_T);Ref("UnaryExpression")]);Concat([Ref("UnaryExpressionNotPlusMinus")])]));
   ("PreIncrementExpression",Disjunction([Concat([Atomic(INCR_T);Ref("UnaryExpression")])]));
   ("PreDecrementExpression",Disjunction([Concat([Atomic(DECR_T);Ref("UnaryExpression")])]));
   ("UnaryExpressionNotPlusMinus",Disjunction([Concat([Ref("PostfixExpression")]);Concat([Atomic(COMPL_T);Ref("UnaryExpression")]);Concat([Atomic(NOT_T);Ref("UnaryExpression")]);Concat([Ref("CastExpression")]);Concat([Ref("SwitchExpression")])]));
   ("PostfixExpression",Disjunction([Concat([Ref("Primary")]);Concat([Ref("ExpressionName")]);Concat([Ref("PostIncrementExpression")]);Concat([Ref("PostDecrementExpression")])]));
   ("PostIncrementExpression",Disjunction([Concat([Ref("PostfixExpression");Atomic(INCR_T)])]));
   ("PostDecrementExpression",Disjunction([Concat([Ref("PostfixExpression");Atomic(DECR_T)])]));
   ("CastExpression",Disjunction([Concat([Atomic(LP_T);Ref("PrimitiveType");Atomic(RP_T);Ref("UnaryExpression")]);Concat([Atomic(LP_T);Ref("ReferenceType");Ref("StarredAdditionalBound");Atomic(RP_T);Ref("UnaryExpressionNotPlusMinus")]);Concat([Atomic(LP_T);Ref("ReferenceType");Ref("StarredAdditionalBound");Atomic(RP_T);Ref("LambdaExpression")])]));
   ("SwitchExpression",Disjunction([Concat([Atomic(SWITCH_T);Atomic(LP_T);Ref("Expression");Atomic(RP_T);Ref("SwitchBlock")])]));
   ("ConstantExpression",Disjunction([Concat([Ref("Expression")])]));
   ("AnnotatedIdentifierrPrecededByDot",Disjunction([Concat([Atomic(DOT_T);Ref("StarredAnnotation");Ref("Identifier")])]));
   ("ClassTypePrecededByVerticalBar",Disjunction([Concat([Atomic(OR_T);Ref("ClassType")])]));
   ("DimsElement",Disjunction([Concat([Ref("StarredAnnotation");Ref("[");Ref("]")])]));
   ("EqualsVariableInitializer",Disjunction([Concat([Atomic(EQ_T);Ref("VariableInitializer")])]));
   ("IdentifierFollowedByDot",Disjunction([Concat([Ref("Identifier");Atomic(DOT_T)])]));
   ("IdentifierPrecededByDot",Disjunction([Concat([Atomic(DOT_T);Ref("Identifier")])]));
   ("OpenSquare",Disjunction([Concat([Ref("[");Ref("]")])]));
   ("ParenthesedArgumentList",Disjunction([Concat([Atomic(LP_T);Optional("ArgumentList");Atomic(RP_T)])]));
   ("ToModuleList",Disjunction([Concat([Atomic(TO_T);Ref("ModuleName");Ref("StarredModuleNamePrecededByComma")])]));
   ("ReceiverParameterFollowedByComma",Disjunction([Concat([Ref("ReceiverParameter");Atomic(CM_T)])]));
   ("ResourcePrecededBySemiColon",Disjunction([Concat([Atomic(SM_T);Ref("Resource")])]));
   ("SwitchLabelFollowedByColon",Disjunction([Concat([Ref("SwitchLabel");Atomic(COLON_T)])]));
   ("CaseConstantPrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("CaseConstant")])]));
   ("ElementValuePrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("ElementValue")])]));
   ("ElementValuePairPrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("ElementValuePair")])]));
   ("EnumConstantPrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("EnumConstant")])]));
   ("ExceptionTypePrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("ExceptionType")])]));
   ("ExpressionPrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("Expression")])]));
   ("FormalParameterPrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("FormalParameter")])]));
   ("IdentifierPrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("Identifier")])]));
   ("InterfaceTypePrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("InterfaceType")])]));
   ("LambdaParameterPrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("LambdaParameter")])]));
   ("ModuleNamePrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("ModuleName")])]));
   ("RecordComponentPrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("RecordComponent")])]));
   ("ResourcePrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("Resource")])]));
   ("StatementExpressionPrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("StatementExpression")])]));
   ("TypeArgumentPrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("TypeArgument")])]));
   ("TypeNamePrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("TypeName")])]));
   ("TypeParameterPrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("TypeParameter")])]));
   ("VariableDeclaratorPrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("VariableDeclarator")])]));
   ("VariableInitializerPrecededByComma",Disjunction([Concat([Atomic(CM_T);Ref("VariableInitializer")])]));
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

])


(* Java grammar ends here *)

 end ;;
let java_grammar = Private.java_grammar ;;
