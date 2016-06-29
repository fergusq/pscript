module Lexer where
import Data.Char

data Token
	= TokenVar
	| TokenFor
	| TokenIn
	| TokenIf
	| TokenElse
	| TokenReturn
	| TokenExtern
	| TokenModel
	| TokenExtend
	| TokenWith
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
	deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer ('=':'=':cs) = TokenEqEq : lexer cs
lexer ('!':'=':cs) = TokenNeq : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('[':cs) = TokenOB : lexer cs
lexer (']':cs) = TokenCB : lexer cs
lexer ('{':cs) = TokenOW : lexer cs
lexer ('}':cs) = TokenCW : lexer cs
lexer ('<':'=':cs) = TokenLe : lexer cs
lexer ('>':'=':cs) = TokenGe : lexer cs
lexer ('<':cs) = TokenLt : lexer cs
lexer ('>':cs) = TokenGt : lexer cs
lexer (',':cs) = TokenC : lexer cs
lexer (';':cs) = TokenSC : lexer cs
lexer ('&':'&':cs) = TokenAnd : lexer cs
lexer ('|':'|':cs) = TokenOr : lexer cs
lexer ('|':cs) = TokenPipe : lexer cs
lexer ('.':cs) = TokenDot : lexer cs
lexer ('"':cs) = lexString "" cs
lexer (c:cs)
	| isSpace c = lexer cs
	| isAlpha c = lexVar (c:cs)
	| isDigit c = lexNum (c:cs)
	| otherwise = error ("Illegal character: " ++ [c])

lexString s ('"':cs) = TokenString s : lexer cs
lexString s ('\\':'n':cs) = lexString (s ++ "\n") cs
lexString s ('\\':'t':cs) = lexString (s ++ "\t") cs
lexString s ('\\':'\\':cs) = lexString (s ++ "\\") cs
lexString s (c:cs) = lexString (s ++ [c]) cs
lexString s [] = error ("Unclosed string: " ++ s)

lexNum cs = TokenInt (read num) : lexer rest
	where (num,rest) = span isDigit cs

lexVar cs =
	case span isAlphaNum cs of
		("for",rest)    -> TokenFor : lexer rest
		("in",rest)     -> TokenIn : lexer rest
		("var",rest)    -> TokenVar : lexer rest
		("if",rest)     -> TokenIf : lexer rest
		("else",rest)   -> TokenElse : lexer rest
		("return",rest) -> TokenReturn : lexer rest
		("extern",rest) -> TokenExtern : lexer rest
		("model",rest) -> TokenModel : lexer rest
		("extend",rest) -> TokenExtend : lexer rest
		("with",rest) -> TokenWith : lexer rest
		(var,rest)      -> TokenVarname var : lexer rest
