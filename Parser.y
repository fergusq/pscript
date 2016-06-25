{
module Parser where
import Lexer
}

%name parsePScript
%tokentype { Token }
%error { parseError }

%token
	for		{ TokenFor }
	in		{ TokenIn }
	let		{ TokenVar }
	if		{ TokenIf }
	else		{ TokenElse }
	return		{ TokenReturn }
	int		{ TokenInt $$ }
	str		{ TokenString $$ }
	var		{ TokenVarname $$ }
	'='		{ TokenEq }
	'+'		{ TokenPlus }
	'-'		{ TokenMinus }
	'*'		{ TokenTimes }
	'/'		{ TokenDiv }
	'('		{ TokenOP }
	')'		{ TokenCP }
	'['		{ TokenOB }
	']'		{ TokenCB }
	'{'		{ TokenOW }
	'}'		{ TokenCW }
	'<'		{ TokenLt }
	'>'		{ TokenGt }
	','		{ TokenC }
	';'		{ TokenSC }
	arrow		{ TokenArrow }
	eq		{ TokenEqEq }
	neq		{ TokenNeq }
	le		{ TokenLe }
	ge		{ TokenGe }

%%

Prog	: Decl Prog			{ ($1 : $2) }
	| Decl				{ [$1] }

Decl	: Func				{ $1 }

Func	: Datatype var '(' Params ')' Stmt { Function { name = $2, parameters = $4, returnType = $1, body = $6 } }
Func	: Datatype var '(' ')' Stmt	{ Function { name = $2, parameters = [], returnType = $1, body = $5 } }

Params	: Parameter ',' Params		{ ($1 : $3) }
	| Parameter			{ [$1] }

Parameter : Datatype var		{ ($2, $1) }

Datatype: var				{ Typename $1 [] }
	| Datatype '[' ']'		{ Typename "List" [$1] }
	| Datatype arrow Datatype	{ Typename "Func" [$3, $1] }
	| '(' DtList ')' arrow Datatype	{ Typename "Func" ($5:$2) }
	| '(' ')' arrow Datatype	{ Typename "Func" [$4] }

DtList	: Datatype DtList		{ ($1 : $2) }
	| Datatype			{ [$1] }

Stmt	: Call ';'			{ Expr $1 }
	| for '(' var in Exp ')' Stmt	{ For $3 $5 $7 }
	| if '(' Exp ')' Stmt		{ If $3 $5 Nothing }
	| if '(' Exp ')' Stmt else Stmt	{ If $3 $5 (Just $7) }
	| let var '=' Exp ';'		{ Create $2 $4 }
	| var '=' Exp ';'		{ Assign $1 $3 }
	| return Exp ';'		{ Return $2 }
	| '{' Stmts '}'			{ Block $2 }

Stmts	: Stmt Stmts			{ ($1 : $2) }
	| Stmt				{ [$1] }

Call	: Preprim '(' Args ')'		{ Call $1 $3 }
	| Preprim '(' ')'		{ Call $1 [] }
	| Preprim '[' Exp ']'		{ MethodCall $1 "[]" [$3] }
	| Preprim '[' Exp ']' '=' Exp	{ MethodCall $1 "[]=" [$3, $6] }
	

Args	: Exp ',' Args			{ ($1 : $3) }
	| Exp				{ [$1] }

Exp	: Cmp1				{ $1 }

Cmp1	: Cmp1 eq Cmp2			{ MethodCall $1 "==" [$3] }
	| Cmp1 neq Cmp2			{ MethodCall $1 "!=" [$3] }
	| Cmp2				{ $1 }

Cmp2	: Cmp2 '<' Sum			{ MethodCall $1 "<" [$3] }
	| Cmp2 '>' Sum			{ MethodCall $1 ">" [$3] }
	| Cmp2 le Sum			{ MethodCall $1 "<=" [$3] }
	| Cmp2 ge Sum			{ MethodCall $1 ">=" [$3] }
	| Sum				{ $1 }

Sum	: Sum '+' Term			{ MethodCall $1 "+" [$3] }
	| Sum '-' Term			{ MethodCall $1 "-" [$3] }
	| Term				{ $1 }

Term	: Term '*' Preprim		{ MethodCall $1 "*" [$3] }
	| Term '/' Preprim		{ MethodCall $1 "/" [$3] }
	| Preprim			{ $1 }

Preprim	: Call				{ $1 }
	| Prim				{ $1 }

Prim	: int				{ Int $1 }
	| str				{ Str $1 }
	| var				{ Var $1 }
	| '(' Exp ')'			{ $2 }
	| '[' Args ']'			{ List $2 }

{
parseError :: [Token] -> a
parseError t = error ("Parse error on " ++ case t of (t:ts) -> show t
                                                     _      -> "EOF") 
data Declaration = Function {
		name :: String,
		parameters :: [(String,Datatype)],
		returnType :: Datatype,
		body :: Statement
	} deriving Show

data Datatype = Typename String [Datatype] deriving Show

data Statement
	= Create String Expression
	| Assign String Expression
	| Expr Expression
	| Block [Statement]
	| For String Expression Statement
	| If Expression Statement (Maybe Statement)
	| Return Expression
	deriving Show

data Expression
	= MethodCall Expression String [Expression]
	| Int Int
	| Str String
	| Var String
	| List [Expression]
	| Call Expression [Expression]
	deriving Show
}

