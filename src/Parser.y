{
module Parser where
import Lexer
import Util
}

%name parsePScript
%tokentype { Token }
%error { parseError }

%token
	for		{ Token _ TokenFor }
	in		{ Token _ TokenIn }
	let		{ Token _ TokenVar }
	if		{ Token _ TokenIf }
	else		{ Token _ TokenElse }
	while		{ Token _ TokenWhile }
	return		{ Token _ TokenReturn }
	extern		{ Token _ TokenExtern }
	model		{ Token _ TokenModel }
	extend		{ Token _ TokenExtend }
	with		{ Token _ TokenWith }
	new		{ Token _ TokenNew }
	operator	{ Token _ TokenOperator }
	struct		{ Token _ TokenStruct }
	enum		{ Token _ TokenEnum }
	const		{ Token _ TokenConst }
	as		{ Token _ TokenAs }
	match		{ Token _ TokenMatch }
	true		{ Token _ TokenTrue }
	false		{ Token _ TokenFalse }
	module		{ Token _ TokenModule }
	import		{ Token _ TokenImport }
	int		{ Token _ (TokenInt $$) }
	str		{ Token _ (TokenString $$) }
	var		{ Token _ (TokenVarname $$) }
	'='		{ Token _ TokenEq }
	'+'		{ Token _ TokenPlus }
	'-'		{ Token _ TokenMinus }
	'*'		{ Token _ TokenTimes }
	'/'		{ Token _ TokenDiv }
	'('		{ Token _ TokenOP }
	')'		{ Token _ TokenCP }
	'['		{ Token _ TokenOB }
	']'		{ Token _ TokenCB }
	'{'		{ Token _ TokenOW }
	'}'		{ Token _ TokenCW }
	'<'		{ Token _ TokenLt }
	'>'		{ Token _ TokenGt }
	','		{ Token _ TokenC }
	';'		{ Token _ TokenSC }
	':'		{ Token _ TokenColon }
	'.'		{ Token _ TokenDot }
	'$'		{ Token _ TokenDollar }
	'&'		{ Token _ TokenAmp }
	'@'		{ Token _ TokenAt }
	'|'		{ Token _ TokenPipe }
	'%'		{ Token _ TokenPercent }
	'!'		{ Token _ TokenEM }
	arrow		{ Token _ TokenArrow }
	eq		{ Token _ TokenEqEq }
	neq		{ Token _ TokenNeq }
	le		{ Token _ TokenLe }
	ge		{ Token _ TokenGe }
	and		{ Token _ TokenAnd }
	or		{ Token _ TokenOr }
	dotdot		{ Token _ TokenDotDot }
	field		{ Token _ TokenField }
	plusplus	{ Token _ TokenPlusPlus }

%%

Prog	: module var ';' Imports Decls	{ ModuleDecl $2 $4 $5 }
	| module var ';' Decls		{ ModuleDecl $2 [] $4 }

Imports	: import var ';' Imports	{ ($2 : $4) }
	| import var ';'		{ [$2] }

Decls	: Decl Decls			{ ($1 : $2) }
	| Decl				{ [$1] }

Decl	: Func				{ Func $1 }
	| extern ExternFunc		{ Func $2 }
	| Model				{ Mdl $1 }
	| Extend			{ Ext $1 }
	| Struct			{ Stc $1 }
	| Enum				{ Enm $1 }

Model	: model var '{' EFuncs '}'				{ Model $2 [] [] $4 }
	| model var '<' TParams '>' '{' EFuncs '}' 		{ Model $2 $4 [] $7 }
	| model var ':' DtList '{' EFuncs '}'			{ Model $2 [] $4 $6 }
	| model var '<' TParams '>' ':' DtList  '{' EFuncs '}' 	{ Model $2 $4 $7 $9 }

TParams	: '@' var ',' TParams		{ ($2 : $4) }
	| '@' var			{ [$2] }

Extend	: extend var with Datatype '{' Funcs '}'			{ Extend $2 [] $4 $6 }
	| extend var '<' TParams '>' with Datatype '{' Funcs '}'	{ Extend $2 $4 $7 $9 }

Funcs	: Func Funcs			{ ($1 : $2) }
	| Func				{ [$1] }

EFuncs	: ExternFunc EFuncs		{ ($1 : $2) }
	| ExternFunc			{ [$1] }

Func	: Datatype var '(' Params ')' Stmt				{ Function $2 [] $4 $1 $6 }
	| Datatype var '(' ')' Stmt					{ Function $2 [] [] $1 $5 }
	| Datatype var '<' TParams '>' '(' Params ')' Stmt		{ Function $2 $4 $7 $1 $9 }
	| Datatype var '<' TParams '>' '(' ')' Stmt			{ Function $2 $4 [] $1 $8 }
	| Datatype operator Op1 '(' Parameter ')' Stmt			{ Function $3 [] [$5] $1 $7 }
	| Datatype operator Op2 '(' Parameter ',' Parameter ')' Stmt	{ Function $3 [] [$5, $7] $1 $9 }

ExternFunc
	: Datatype var '(' Params ')' ';'				{ Function $2 [] $4 $1 Extern }
	| Datatype var '(' ')' ';'					{ Function $2 [] [] $1 Extern }
	| Datatype operator Op1 '(' Parameter ')' ';'			{ Function $3 [] [$5] $1 Extern }
	| Datatype operator Op2 '(' Parameter ',' Parameter ')' ';'	{ Function $3 [] [$5, $7] $1 Extern }

Op1	: '[' ']'			{ "op_get" }
	| plusplus			{ "op_append" }
	| '+'				{ "op_add" }
	| '-'				{ "op_sub" }
	| '*'				{ "op_mul" }
	| '/'				{ "op_div" }
	| '%'				{ "op_mod" }
	| eq				{ "op_eq" }
	| neq				{ "op_neq" }
	| '<'				{ "op_lt" }
	| '>'				{ "op_gt" }
	| le				{ "op_le" }
	| ge				{ "op_ge" }
	| and				{ "op_and" }
	| or				{ "op_or" }
	| '|'				{ "op_pipe" }
Op2	: '[' ']' '='			{ "op_set" }

Params	: Parameter ',' Params		{ ($1 : $3) }
	| Parameter			{ [$1] }

Parameter : Datatype var		{ ($2, $1) }

Struct	: struct var '{' Fields '}'			{ Struct $2 [] $4 False }
	| struct var '<' TParams '>' '{' Fields '}'	{ Struct $2 $4 $7 False }
	| struct var '<' TParams '>' '{' '}'		{ Struct $2 $4 [] False }
	| struct var '{' '}'				{ Struct $2 [] [] False }
	| const struct var '{' Fields '}'			{ Struct $3 [] $5 True }
	| const struct var '<' TParams '>' '{' Fields '}'	{ Struct $3 $5 $8 True }
	| const struct var '<' TParams '>' '{' '}'		{ Struct $3 $5 [] True }
	| const struct var '{' '}'				{ Struct $3 [] [] True }

Fields	: Parameter ';' Fields		{ ($1 : $3) }
	| Parameter ';'			{ [$1] }

Enum	: enum var '{' CaseList '}'			{ EnumStruct $2 [] $4 }
	| enum var '<' TParams '>' '{' CaseList '}'	{ EnumStruct $2 $4 $7 }

CaseList: EnumCase ',' CaseList		{ ($1 : $3) }
	| EnumCase			{ [$1] }

EnumCase: var				{ ($1, []) }
	| var '(' DtList ')'		{ ($1, $3) }

Datatype: PrimDT			{ $1 }
	| '$'				{ DollarType }
	| '@' var			{ Typeparam $2 }
	| PrimDT '&' SumDT		{ SumType ($1:$3) }
	| Datatype '[' ']'		{ Typename "Array" [$1] }
	| Datatype '*'			{ Typename "Pointer" [$1] }
	| Datatype arrow Datatype	{ Typename "Func" [$3, $1] }
	| '(' DtList ')' arrow Datatype	{ Typename "Func" ($5:$2) }
	| '(' ')' arrow Datatype	{ Typename "Func" [$4] }

SumDT	: PrimDT '&' SumDT		{ ($1 : $3) }
	| PrimDT			{ [$1] }

PrimDT	: var				{ Typename $1 [] }
	| var '<' DtList '>'		{ Typename $1 $3 }

DtList	: Datatype ',' DtList		{ ($1 : $3) }
	| Datatype			{ [$1] }

Stmt	: Call ';'			{ Expr $1 }
	| for '(' var in Exp ')' Stmt	{ For $3 $5 $7 }
	| if '(' Exp ')' Stmt		{ If $3 $5 Nothing }
	| if '(' Exp ')' Stmt else Stmt	{ If $3 $5 (Just $7) }
	| while '(' Exp ')' Stmt	{ While $3 $5 }
	| let var '=' Exp ';'		{ Create $2 $4 }
	| var '=' Exp ';'		{ Assign $1 $3 }
	| return Exp ';'		{ Return $2 }
	| '{' Stmts '}'			{ Block $2 }
	| Pipe '|' Preprim ';'		{ Expr $ MethodCall $3 "op_pipe" [$1] }
	| match '(' Exp ')' '{' Matches '}'	{ Match $3 $6 }
	| match '(' Exp ')' '{' '}'	{ Match $3 [] }

Matches	: MatchCase Matches		{ ($1 : $2) }
	| MatchCase			{ [$1] }

MatchCase: MatchCond arrow Stmt		{ ($1, $3) }
MatchCond
	: var				{ MatchCond $1 [] }
	| var '(' MatchConds ')'	{ MatchCond $1 $3 }

MatchConds
	: MatchCond ',' MatchConds	{ ($1 : $3) }
	| MatchCond			{ [$1] }

Stmts	: Stmt Stmts			{ ($1 : $2) }
	| Stmt				{ [$1] }

Call	: var '(' Args ')'		{ Call $1 $3 }
	| var '(' ')'			{ Call $1 [] }
	| Preprim '[' Exp ']'		{ MethodCall $1 "op_get" [$3] }
	| Preprim '[' Exp ']' '=' Exp	{ MethodCall $1 "op_set" [$3, $6] }
	| Preprim '.' var '(' Args ')'	{ MethodCall $1 $3 $5 }
	| Preprim '.' var '(' ')'	{ MethodCall $1 $3 [] }
	| Preprim '.' var		{ FieldGet $1 $3 }
	| Preprim '.' var '=' Exp	{ FieldSet $1 $3 $5 }
	

Args	: Exp ',' Args			{ ($1 : $3) }
	| Exp				{ [$1] }

Exp	: Logic1			{ $1 }

Logic1	: Logic1 or Logic2		{ MethodCall $1 "op_or" [$3] }
	| Logic2			{ $1 }

Logic2	: Logic2 and Cmp1		{ MethodCall $1 "op_and" [$3] }
	| Cmp1				{ $1 }

Cmp1	: Cmp1 eq Cmp2			{ MethodCall $1 "op_eq" [$3] }
	| Cmp1 neq Cmp2			{ MethodCall $1 "op_neq" [$3] }
	| Cmp2				{ $1 }

Cmp2	: Cmp2 '<' Append		{ MethodCall $1 "op_lt" [$3] }
	| Cmp2 '>' Append		{ MethodCall $1 "op_gt" [$3] }
	| Cmp2 le Append		{ MethodCall $1 "op_le" [$3] }
	| Cmp2 ge Append		{ MethodCall $1 "op_ge" [$3] }
	| Append			{ $1 }

Append	: Append plusplus Sum		{ MethodCall $1 "op_append" [$3] }
	| Sum				{ $1 }

Sum	: Sum '+' Term			{ MethodCall $1 "op_add" [$3] }
	| Sum '-' Term			{ MethodCall $1 "op_sub" [$3] }
	| Term				{ $1 }

Term	: Term '*' Pipe			{ MethodCall $1 "op_mul" [$3] }
	| Term '/' Pipe			{ MethodCall $1 "op_div" [$3] }
	| Term '%' Pipe			{ MethodCall $1 "op_mod" [$3] }
	| Pipe				{ $1 }

Pipe	: Pipe '|' Preprim		{ MethodCall $3 "op_pipe" [$1] }
	| Preprim			{ $1 }

Preprim	: Call				{ $1 }
	| Prim				{ $1 }

Prim	: int					{ Int $1 }
	| str					{ Str $1 }
	| var					{ Var $1 }
	| '!' Preprim				{ MethodCall $2 "op_not" [] }
	| '-' Preprim				{ MethodCall $2 "op_neg" [] }
	| '(' Exp ')'				{ $2 }
	| '[' Args ']'				{ List $2 }
	| '[' Exp dotdot Exp ']'		{ Range $2 $4 }
	| '(' Params ')' arrow Datatype '{' Stmts '}'	{ Lambda $2 $5 $ Block $7 }
	| new Datatype '[' Exp ']'		{ NewList $2 $4 }
	| new Datatype '{' Args '}'		{ NewStruct $2 $4 }
	| new Datatype '{' '}'			{ NewStruct $2 [] }
	| new Datatype field var '(' Args ')'	{ NewEnumStruct $2 $4 $6 }
	| new Datatype field var '(' ')'	{ NewEnumStruct $2 $4 [] }
	| new Datatype '*' '(' Exp ')'		{ NewPtrList $2 $5 }
	| Preprim as Datatype			{ Cast $3 $1 }
	| true					{ TrueConstant }
	| false					{ FalseConstant }

{
parseError :: [Token] -> a
parseError ((Token ln t):ts)
	= error ("[Line " ++ show ln ++ "] Parse error on " ++ show t)
parseError [] = error "Parser error on EOF"

data ModuleDecl = ModuleDecl String [String] [Declaration]

data Declaration =
	Func Function
	| Mdl Model
	| Ext Extend
	| Stc Struct
	| Enm EnumStruct
	deriving Show

data Model = Model {
	modelName :: String,
	typeparameters :: [String],
	prerequisites :: [Datatype],
	methods :: [Function]
} deriving Show

data Extend = Extend {
	dtName :: String,
	eTypeparameters :: [String],
	model :: Datatype,
	eMethods :: [Function]
} deriving Show

data Struct = Struct {
	stcName :: String,
	stcTypeparameters :: [String],
	stcFields :: [(String, Datatype)],
	isConst :: Bool
} deriving Show

data EnumStruct = EnumStruct {
	enmName :: String,
	enmTypeparameters :: [String],
	enmCases :: [(String, [Datatype])]
} deriving Show

data Function = Function {
	name :: String,
	funcTypeparameters :: [String],
	parameters :: [(String,Datatype)],
	returnType :: Datatype,
	body :: Statement
} deriving Show

data Datatype
	= Typename String [Datatype]
	| SumType [Datatype]
	| Typeparam String
	| DollarType

instance Show Datatype where
    show (Typename a []) = '#' : a
    show (Typename a as) = '#' : a ++ "<" ++ joinComma (map show as) ++ ">"
    show (SumType dts) = '#' : joinChar '&' (map show dts)
    show DollarType = "#$"
    show (Typeparam p) = "#@" ++ p

data Statement
	= Create String Expression
	| Assign String Expression
	| Expr Expression
	| Block [Statement]
	| For String Expression Statement
	| If Expression Statement (Maybe Statement)
	| While Expression Statement
	| Return Expression
	| Match Expression [(MatchCondition, Statement)]
	| Extern
	deriving Show

data MatchCondition
	= MatchCond String [MatchCondition]
	deriving Show

data Expression
	= MethodCall Expression String [Expression]
	| Int Int
	| Str String
	| Var String
	| List [Expression]
	| Range Expression Expression
	| Call String [Expression]
	| NewList Datatype Expression
	| NewStruct Datatype [Expression]
	| NewEnumStruct Datatype String [Expression]
	| NewPtrList Datatype Expression
	| FieldGet Expression String
	| FieldSet Expression String Expression
	| Cast Datatype Expression
	| Lambda [(String,Datatype)] Datatype Statement
	| TrueConstant | FalseConstant
	deriving Show
}

