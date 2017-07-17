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
	break		{ Token _ TokenBreak }
	continue	{ Token _ TokenContinue }
	pass		{ Token _ TokenPass }
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
	'?'		{ Token _ TokenQM }
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
	| extern import str ';'		{ ExIm $3 }

Model	: AnnsMdl var '{' EFuncs '}'				{ Model $2 [] [] $4 $1 }
	| AnnsMdl var '<' TParams '>' '{' EFuncs '}' 		{ Model $2 $4 [] $7 $1 }
	| AnnsMdl var ':' DtList '{' EFuncs '}'			{ Model $2 [] $4 $6 $1 }
	| AnnsMdl var '<' TParams '>' ':' DtList  '{' EFuncs '}' 	{ Model $2 $4 $7 $9 $1 }

AnnsMdl	: Ann AnnsMdl			{ ($1 : $2) }
	| model				{ [] }

TParams	: '@' var ',' TParams		{ ($2 : $4) }
	| '@' var			{ [$2] }

Extend	: extend var ':' Datatype '{' Funcs '}'			{ Extend $2 [] $4 $6 }
	| extend var '<' TParams '>' ':' Datatype '{' Funcs '}'	{ Extend $2 $4 $7 $9 }

Funcs	: Func Funcs			{ ($1 : $2) }
	| Func				{ [$1] }

EFuncs	: ExternFunc EFuncs		{ ($1 : $2) }
	| ExternFunc			{ [$1] }

Func	: AnnsRT var '(' Params ')' Stmt				{ Function $2 [] $4 (fst $1) $6 (snd $1) }
	| AnnsRT var '(' ')' Stmt					{ Function $2 [] [] (fst $1) $5 (snd $1) }
	| AnnsRT var '<' TParams '>' '(' Params ')' Stmt		{ Function $2 $4 $7 (fst $1) $9 (snd $1) }
	| AnnsRT var '<' TParams '>' '(' ')' Stmt			{ Function $2 $4 [] (fst $1) $8 (snd $1) }
	| AnnsRT operator Op1 '(' Parameter ')' Stmt			{ Function $3 [] [$5] (fst $1) $7 (snd $1) }
	| AnnsRT operator Op2 '(' Parameter ',' Parameter ')' Stmt	{ Function $3 [] [$5, $7] (fst $1) $9 (snd $1) }

ExternFunc
	: AnnsRT var '(' Params ')' ';'					{ Function $2 [] $4 (fst $1) Extern (snd $1) }
	| AnnsRT var '(' ')' ';'					{ Function $2 [] [] (fst $1) Extern (snd $1) }
	| AnnsRT operator Op1 '(' Parameter ')' ';'			{ Function $3 [] [$5] (fst $1) Extern (snd $1) }
	| AnnsRT operator Op2 '(' Parameter ',' Parameter ')' ';'	{ Function $3 [] [$5, $7] (fst $1) Extern (snd $1) }

AnnsRT	: Ann AnnsRT			{ (fst $2, $1 : snd $2) }
	| Datatype			{ ($1, []) }

Ann	: '[' var ']'			{ (,) $2 [] }
	| '[' var '(' ')' ']'		{ (,) $2 [] }
	| '[' var '(' AnnArgs ')' ']'	{ (,) $2 $4 }

AnnArgs	: str ',' AnnArgs		{ ($1 : $3) }
	| str				{ [$1] }

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

Struct	: AnnsStc var '{' Fields '}'			{ Struct $2 [] $4 False $1 False }
	| AnnsStc var '<' TParams '>' '{' Fields '}'	{ Struct $2 $4 $7 False $1 False }
	| AnnsStc var '<' TParams '>' '{' '}'		{ Struct $2 $4 [] False $1 False }
	| AnnsStc var '{' '}'				{ Struct $2 [] [] False $1 False }
	| AnnsCns struct var '{' Fields '}'			{ Struct $3 [] $5 True $1 False }
	| AnnsCns struct var '<' TParams '>' '{' Fields '}'	{ Struct $3 $5 $8 True $1 False }
	| AnnsCns struct var '<' TParams '>' '{' '}'		{ Struct $3 $5 [] True $1 False }
	| AnnsCns struct var '{' '}'				{ Struct $3 [] [] True $1 False }
	| extern struct var '{' Fields '}'			{ Struct $3 [] $5 False [] True }
	| extern const struct var '{' Fields '}'		{ Struct $4 [] $6 True [] True }

AnnsStc	: Ann AnnsStc			{ ($1 : $2) }
	| struct			{ [] }

AnnsCns	: Ann AnnsCns			{ ($1 : $2) }
	| const				{ [] }

Fields	: Parameter ';' Fields		{ ($1 : $3) }
	| Parameter ';'			{ [$1] }

Enum	: AnnsEnm var '{' CaseList '}'			{ EnumStruct $2 [] $4 $1 }
	| AnnsEnm var '<' TParams '>' '{' CaseList '}'	{ EnumStruct $2 $4 $7 $1 }

AnnsEnm	: Ann AnnsEnm			{ ($1 : $2) }
	| enum				{ [] }

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
	| Datatype '?'			{ Typename "Maybe" [$1] }
	| '<' Datatype '>'		{ $2 }

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
	| break ';'			{ Break }
	| continue ';'			{ Continue }

Matches	: MatchCase Matches		{ ($1 : $2) }
	| MatchCase			{ [$1] }

MatchCase: MatchCond arrow Stmt		{ ($1, $3) }
	| MatchCond arrow pass ';'	{ ($1, Block []) }
MatchCond
	: var				{ MatchCond $1 [] }
	| var '(' MatchConds ')'	{ MatchCond $1 $3 }
	| str				{ MatchStr $1 }
	| int				{ MatchInt $1 }

MatchConds
	: MatchCond ',' MatchConds	{ ($1 : $3) }
	| MatchCond			{ [$1] }

Stmts	: Stmt Stmts			{ ($1 : $2) }
	| Stmt				{ [$1] }

Call	: var '(' Args ')'			{ Call $1 [] $3 }
	| var '(' ')'				{ Call $1 [] [] }
	| var field '<' DtList '>' '(' Args ')'	{ Call $1 $4 $7 }
	| var field '<' DtList '>' '(' ')'	{ Call $1 $4 [] }
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
	| '&' var				{ Ref $2 }
	| '!' Preprim				{ MethodCall $2 "op_not" [] }
	| '-' Preprim				{ MethodCall $2 "op_neg" [] }
	| '(' Exp ')'				{ $2 }
	| '[' Args ']'				{ List $2 }
	| '[' ']'				{ EmptyList }
	| '[' Exp dotdot Exp ']'		{ Range $2 $4 }
	| '(' LParams ')' arrow Datatype '{' Stmts '}'	{ Lambda $2 $5 $ Block $7 }
	| '(' ')' arrow Datatype '{' Stmts '}'	{ Lambda [] $4 $ Block $6 }
	| '(' LParams ')' arrow '{' Stmts '}'	{ Lambda $2 AutoType $ Block $6 }
	| '(' ')' arrow '{' Stmts '}'		{ Lambda [] AutoType $ Block $5 }
	| '(' LParams ')' arrow Exp		{ Lambda $2 AutoType $ Block [Return $5] }
	| '(' ')' arrow Exp			{ Lambda [] AutoType $ Block [Return $4] }
	| new Datatype '[' Exp ']'		{ NewList $2 $4 }
	| new Datatype '{' Args '}'		{ NewStruct $2 $4 }
	| new Datatype '{' '}'			{ NewStruct $2 [] }
	| new Datatype field var '(' Args ')'	{ NewEnumStruct $2 $4 $6 }
	| new Datatype field var '(' ')'	{ NewEnumStruct $2 $4 [] }
	| new Datatype '*' '(' Exp ')'		{ NewPtrList $2 $5 }
	| Preprim as Datatype			{ Cast $3 $1 }
	| true					{ TrueConstant }
	| false					{ FalseConstant }

LParams	: LParameter ',' LParams	{ ($1 : $3) }
	| LParameter			{ [$1] }

LParameter : Datatype var		{ ($2, $1) }
	| var				{ ($1, AutoType) }

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
	| ExIm String

data Model = Model {
	modelName :: String,
	typeparameters :: [String],
	prerequisites :: [Datatype],
	methods :: [Function],
	modelAnnotations :: [(String, [String])]
}

data Extend = Extend {
	dtName :: String,
	eTypeparameters :: [String],
	model :: Datatype,
	eMethods :: [Function]
}

data Struct = Struct {
	stcName :: String,
	stcTypeparameters :: [String],
	stcFields :: [(String, Datatype)],
	isConst :: Bool,
	stcAnnotations :: [(String, [String])],
	isExtern :: Bool
}

data EnumStruct = EnumStruct {
	enmName :: String,
	enmTypeparameters :: [String],
	enmCases :: [(String, [Datatype])],
	enmAnnotations :: [(String, [String])]
}

data Function = Function {
	name :: String,
	funcTypeparameters :: [String],
	parameters :: [(String,Datatype)],
	returnType :: Datatype,
	body :: Statement,
	annotations :: [(String, [String])]
}

data Datatype
	= Typename String [Datatype]
	| SumType [Datatype]
	| Typeparam String
	| DollarType
	| AutoType

instance Show Datatype where
    show (Typename a []) = a
    show (Typename a as) = a ++ "<" ++ joinComma (map show as) ++ ">"
    show (SumType dts) = joinChar '&' (map show dts)
    show DollarType = "$"
    show AutoType = "<auto>"
    show (Typeparam p) = "@" ++ p

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
	| Break
	| Continue
	| Extern

data MatchCondition
	= MatchCond String [MatchCondition]
	| MatchStr String
	| MatchInt Int

data Expression
	= MethodCall Expression String [Expression]
	| Int Int
	| Str String
	| Var String
	| Ref String
	| List [Expression]
	| EmptyList
	| Range Expression Expression
	| Call String [Datatype] [Expression]
	| NewList Datatype Expression
	| NewStruct Datatype [Expression]
	| NewEnumStruct Datatype String [Expression]
	| NewPtrList Datatype Expression
	| FieldGet Expression String
	| FieldSet Expression String Expression
	| Cast Datatype Expression
	| Lambda [(String,Datatype)] Datatype Statement
	| TrueConstant | FalseConstant
}

