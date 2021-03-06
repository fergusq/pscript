module Lexer where
import Data.Char

data Token = Token Int TokenClass
data TokenClass
	= TokenVar
	| TokenFor
	| TokenIn
	| TokenIf
	| TokenElse
	| TokenWhile
	| TokenReturn
	| TokenExtern
	| TokenModel
	| TokenExtend
	| TokenNew
	| TokenOperator
	| TokenStruct
	| TokenEnum
	| TokenConst
	| TokenAs
	| TokenMatch
	| TokenTrue
	| TokenFalse
	| TokenModule
	| TokenImport
	| TokenBreak
	| TokenContinue
	| TokenPass
	| TokenSub
	| TokenSuper
	| TokenInt Int
	| TokenString String
	| TokenVarname String
	| TokenEq
	| TokenPlus
	| TokenMinus
	| TokenTimes
	| TokenDiv
	| TokenOP
	| TokenCP
	| TokenOB
	| TokenCB
	| TokenOW
	| TokenCW
	| TokenC
	| TokenSC
	| TokenColon
	| TokenAmp
	| TokenPipe
	| TokenArrow
	| TokenEqEq
	| TokenNeq
	| TokenLt
	| TokenGt
	| TokenLe
	| TokenGe
	| TokenAnd
	| TokenOr
	| TokenDot
	| TokenDotDot
	| TokenDollar
	| TokenAt
	| TokenField
	| TokenPercent
	| TokenEM
	| TokenQM
	| TokenPlusPlus
	deriving Show

lexer :: Int -> String -> [Token]
lexer ln [] = []
lexer ln ('/':'/':cs) = lexLineComment ln cs
lexer ln ('/':'*':cs) = lexBlockComment ln cs
lexer ln ('=':'=':cs) = Token ln TokenEqEq : lexer ln cs
lexer ln ('!':'=':cs) = Token ln TokenNeq : lexer ln cs
lexer ln ('=':cs) = Token ln TokenEq : lexer ln cs
lexer ln ('+':'+':cs) = Token ln TokenPlusPlus : lexer ln cs
lexer ln ('+':cs) = Token ln TokenPlus : lexer ln cs
lexer ln ('-':'>':cs) = Token ln TokenArrow : lexer ln cs
lexer ln ('-':cs) = Token ln TokenMinus : lexer ln cs
lexer ln ('*':cs) = Token ln TokenTimes : lexer ln cs
lexer ln ('/':cs) = Token ln TokenDiv : lexer ln cs
lexer ln ('(':cs) = Token ln TokenOP : lexer ln cs
lexer ln (')':cs) = Token ln TokenCP : lexer ln cs
lexer ln ('[':cs) = Token ln TokenOB : lexer ln cs
lexer ln (']':cs) = Token ln TokenCB : lexer ln cs
lexer ln ('{':cs) = Token ln TokenOW : lexer ln cs
lexer ln ('}':cs) = Token ln TokenCW : lexer ln cs
lexer ln ('<':'=':cs) = Token ln TokenLe : lexer ln cs
lexer ln ('>':'=':cs) = Token ln TokenGe : lexer ln cs
lexer ln ('<':cs) = Token ln TokenLt : lexer ln cs
lexer ln ('>':cs) = Token ln TokenGt : lexer ln cs
lexer ln (',':cs) = Token ln TokenC : lexer ln cs
lexer ln (';':cs) = Token ln TokenSC : lexer ln cs
lexer ln ('&':'&':cs) = Token ln TokenAnd : lexer ln cs
lexer ln ('|':'|':cs) = Token ln TokenOr : lexer ln cs
lexer ln ('&':cs) = Token ln TokenAmp : lexer ln cs
lexer ln ('|':cs) = Token ln TokenPipe : lexer ln cs
lexer ln ('.':'.':cs) = Token ln TokenDotDot : lexer ln cs
lexer ln ('.':cs) = Token ln TokenDot : lexer ln cs
lexer ln ('$':cs) = Token ln TokenDollar : lexer ln cs
lexer ln ('@':cs) = Token ln TokenAt : lexer ln cs
lexer ln (':':':':cs) = Token ln TokenField : lexer ln cs
lexer ln (':':cs) = Token ln TokenColon : lexer ln cs
lexer ln ('%':cs) = Token ln TokenPercent : lexer ln cs
lexer ln ('!':cs) = Token ln TokenEM : lexer ln cs
lexer ln ('?':cs) = Token ln TokenQM : lexer ln cs
lexer ln ('"':cs) = lexString ln "" cs
lexer ln ('\n':cs) = lexer (ln+1) cs
lexer ln (c:cs)
	| isSpace c = lexer ln cs
	| isAlpha c || c == '_' = lexVar ln (c:cs)
	| isDigit c = lexNum ln (c:cs)
	| otherwise = error ("Illegal character: " ++ [c])

lexLineComment ln ('\n':cs) = lexer (ln+1) cs
lexLineComment ln (c:cs) = lexLineComment ln cs
lexLineComment ln [] = []

lexBlockComment ln ('\n':cs) = lexBlockComment (ln+1) cs
lexBlockComment ln ('*':'/':cs) = lexer ln cs
lexBlockComment ln (c:cs) = lexBlockComment ln cs
lexBlockComment ln [] = error ("Unclosed comment on line " ++ show ln)

lexString ln s ('"':cs) = Token ln (TokenString s) : lexer ln cs
lexString ln s ('\\':'n':cs) = lexString ln (s ++ "\n") cs
lexString ln s ('\\':'t':cs) = lexString ln (s ++ "\t") cs
lexString ln s ('\\':'\\':cs) = lexString ln (s ++ "\\") cs
lexString ln s ('\\':'"':cs) = lexString ln (s ++ "\"") cs
lexString ln s ('\n':cs) = lexString (ln+1) (s ++ "\n") cs
lexString ln s (c:cs) = lexString ln (s ++ [c]) cs
lexString ln s [] = error ("Unclosed string on line " ++ show ln ++ ": " ++ s)

lexNum ln cs = Token ln (TokenInt (read num)) : lexer ln rest
	where (num,rest) = span isDigit cs

lexVar ln cs =
	case span (\c -> isAlphaNum c || c == '_') cs of
		("for",rest)     -> Token ln TokenFor : lexer ln rest
		("in",rest)      -> Token ln TokenIn : lexer ln rest
		("var",rest)     -> Token ln TokenVar : lexer ln rest
		("if",rest)      -> Token ln TokenIf : lexer ln rest
		("else",rest)    -> Token ln TokenElse : lexer ln rest
		("while",rest)   -> Token ln TokenWhile : lexer ln rest
		("return",rest)  -> Token ln TokenReturn : lexer ln rest
		("extern",rest)  -> Token ln TokenExtern : lexer ln rest
		("model",rest)   -> Token ln TokenModel : lexer ln rest
		("extend",rest)  -> Token ln TokenExtend : lexer ln rest
		("new",rest)     -> Token ln TokenNew : lexer ln rest
		("operator",rest)-> Token ln TokenOperator : lexer ln rest
		("struct",rest)  -> Token ln TokenStruct : lexer ln rest
		("const",rest)   -> Token ln TokenConst : lexer ln rest
		("as",rest)      -> Token ln TokenAs : lexer ln rest
		("sub",rest)     -> Token ln TokenSub : lexer ln rest
		("super",rest)   -> Token ln TokenSuper : lexer ln rest
		("enum",rest)    -> Token ln TokenEnum : lexer ln rest
		("match",rest)   -> Token ln TokenMatch : lexer ln rest
		("true",rest)    -> Token ln TokenTrue : lexer ln rest
		("false",rest)   -> Token ln TokenFalse : lexer ln rest
		("module",rest)  -> Token ln TokenModule : lexer ln rest
		("import",rest)  -> Token ln TokenImport : lexer ln rest
		("break",rest)  -> Token ln TokenBreak : lexer ln rest
		("continue",rest)  -> Token ln TokenContinue : lexer ln rest
		("pass",rest)  -> Token ln TokenPass : lexer ln rest
		(var,rest)       -> Token ln (TokenVarname var) : lexer ln rest
