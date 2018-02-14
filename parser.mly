%{
open List
open Syntax
%}

%token <Syntax.ident> IDENTIFIER
%token <int> INT_LITERAL

/* Separators */

%token LP		/* ( */
%token RP		/* ) */
%token LC		/* { */
%token RC		/* } */
%token SM		/* ; */
%token CM		/* , */
%token DOT		/* . */
%token QM       /* ? */
%token CL       /* : */

/* Operators */

%token EQ		/* = */
%token GT		/* > */
%token LT		/* < */
%token NOT		/* ! */
%token EQ_EQ		/* == */
%token LE		/* <= */
%token GE		/* >= */
%token NOT_EQ		/* != */
%token AND_AND		/* && */
%token OR_OR		/* || */
%token PLUS		/* + */
%token MINUS		/* - */
%token TIMES		/* * */
%token DIV		/* / */
%token MOD		/* % */

%token BOOLEAN CLASS ELSE EXTENDS FALSE IF INT NEW NULL RETURN
%token SUPER THIS TRUE WHILE

%token EOF

%start goal
%type <Syntax.jProgram> goal

%%

goal:
	Classes Exprs EOF { Program ($1, $2) }
;

Classes:
	/* empty */ { [] }
|	Class Classes { $1 :: $2 }
;

Class:
	CLASS Identifier EXTENDS Identifier LC ClassBody RC { let (v, c, m) = $6 in Class ($2, $4, v, List.hd c, m) }
;

ClassBody:
	/* empty */ { ([], [], []) }
|	Var ClassBody { let (v, c, m) = $2 in ($1 :: v, c, m) }
|	Constructor Methods { ([], [$1], $2) }
;

Identifier:
	IDENTIFIER  { $1 }
;

Var:
	VarDecl SM { $1 }
;

VarDecl:
	TypeIdentifier Identifier { Var ($1, $2) }
;

TypeIdentifier:
	Int { $1 }
|	Boolean { $1 }
|	Identifier { Named $1 }
;

Int:
	INT { Int }
;

Boolean:
	BOOLEAN { Boolean }
;

Constructor:
	Identifier LP Args RP LC SuperCall ConstructorAsgns RC { Constructor ($1, $3, $6, $7) }
;

SuperCall:
	SUPER LP Idents RP SM { $3 }
;

Idents:
	/* empty */ { [] }
|	NonEmptyIdents { $1 }
;

NonEmptyIdents:
	Identifier { [$1] }
|	Identifier CM NonEmptyIdents { $1 :: $3 }
;

ConstructorAsgns:
	/* empty */ { [] }
|	THIS DOT Identifier EQ Identifier SM ConstructorAsgns { if $3 = $5 then $3 :: $7 else failwith ("Incorrect assignment: this." ^ $3.Syntax.id ^ " = " ^ $5.Syntax.id) }
;

Methods:
	/* empty */ { [] }
|	Method Methods { $1 :: $2 }
;

Method:
	TypeIdentifier Identifier LP Args RP LC RETURN Expr SM RC { Method ($1, $2, $4, $8) }

Args:
	/* empty */ { [] }
|	NonEmptyArgs { $1 }
;

NonEmptyArgs:
	VarDecl { [$1] }
|	VarDecl CM NonEmptyArgs { $1 :: $3 }
;

Exprs:
	/* empty */ { [] }
|	Expr SM Exprs { $1 :: $3 }
;

Expr:
	IfExpr { $1 }
;

Primary:
	THIS { This }
|	Identifier { Val $1 }
|	LP Expr RP { $2 }
|	NEW Identifier LP ArgVals RP { New ($2, $4) }
|	FieldAccess { $1 }
|	MethodInvocation { $1 }
|	IntLiteral { Num $1 }
|	TRUE { BConst JTrue }
|	FALSE { BConst JFalse }
|   NULL { Null }
;

FieldAccess:
	Primary DOT Identifier { Field ($1, $3) }
;

MethodInvocation:
	Primary DOT Identifier LP ArgVals RP { Call ($1, $3, $5) }
;

PostfixExpr:
	Primary { $1 }
;

UnaryExpr:
	MINUS PostfixExpr { UnaryMinus $2 }
|	UnaryExprNotPlusMinus { $1 }
;

UnaryExprNotPlusMinus:
	PostfixExpr { $1 }
|	NOT PostfixExpr { Not $2 }
|	CastExpr { $1 }
;

CastExpr:
	LP Expr RP UnaryExprNotPlusMinus { Cast (val_to_type $2, $4) }
;

MultiplicativeExpr:
	UnaryExpr { $1 }
|	MultiplicativeExpr TIMES UnaryExpr { Aop (Times, $1, $3) }
|	MultiplicativeExpr DIV UnaryExpr { Aop (Div, $1, $3) }
|	MultiplicativeExpr MOD UnaryExpr { Aop (Mod, $1, $3) }
;

AdditiveExpr:
	MultiplicativeExpr { $1 }
|	AdditiveExpr PLUS MultiplicativeExpr { Aop (Plus, $1, $3) }
|	AdditiveExpr MINUS MultiplicativeExpr { Aop (Minus, $1, $3) }
;

RelationalExpr:
	AdditiveExpr { $1 }
|	RelationalExpr LT AdditiveExpr { Cop (Lt, $1, $3) }
|	RelationalExpr GT AdditiveExpr { Cop (Gt, $1, $3) }
|	RelationalExpr LE AdditiveExpr { Cop (Leq, $1, $3) }
|	RelationalExpr GE AdditiveExpr { Cop (Geq, $1, $3) }
;

EqualityExpr:
	RelationalExpr { $1 }
|	EqualityExpr EQ_EQ RelationalExpr { OCop (Eq, $1, $3) }
|	EqualityExpr NOT_EQ RelationalExpr { OCop (Neq, $1, $3) }
;

ConditionalAndExpr:
	EqualityExpr { $1 }
|	ConditionalAndExpr AND_AND EqualityExpr { Bop (And, $1, $3) }
;

ConditionalOrExpr:
	ConditionalAndExpr { $1 }
|	ConditionalOrExpr OR_OR ConditionalAndExpr { Bop (Or, $1, $3) }
;

IfExpr:
	ConditionalOrExpr { $1 }
|	Expr QM IfExpr CL IfExpr { If ($1, $3, $5) }
;

ArgVals:
	/* empty */ { [] }
|	NonEmptyArgVals { $1 }
;

NonEmptyArgVals:
	Expr { [$1] }
|	Expr CM NonEmptyArgVals { $1 :: $3 }
;

IntLiteral:
	INT_LITERAL { $1 }
;
