{
module Language.Mira.RegExpParser where

import Data.Char
import Language.Mira.RegExp
}
%name regex
%tokentype	{ Token }
%error		{ parseError }

%token
'@'	{ TokenEpsilon }
Char    { TokenChar $$ }
'*'     { TokenStar }
'|'     { TokenOr }
'&'     { TokenAnd }
'.'     { TokenConcat }
'('     { TokenOpenParen }
')'     { TokenCloseParen }

%%
Reg :: { Reg }
Reg : '@'	        { Epsilon }
  | Char                { Literal $1 }
  | Reg '*'             { (Star $1) }
  | '(' Reg ')' '*'     { (Star $2) }
  | '(' Reg '|' Reg ')' { (Or $2 $4) }
  | '(' Reg '&' Reg ')' { (And $2 $4) }
  | '(' Reg '.' Reg ')' { (Then $2 $4) }

{
data Token = TokenEpsilon
  | TokenChar Char
  | TokenStar
  | TokenOr
  | TokenAnd
  | TokenConcat
  | TokenOpenParen
  | TokenCloseParen

lexer :: String -> [Token]
lexer [] = []
lexer ('@':cs) = TokenEpsilon : lexer cs
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = TokenChar c : lexer cs
lexer ('*':cs) = TokenStar : lexer cs
lexer ('|':cs) = TokenOr : lexer cs
lexer ('&':cs) = TokenAnd : lexer cs
lexer ('.':cs) = TokenConcat : lexer cs
lexer ('(':cs) = TokenOpenParen : lexer cs
lexer (')':cs) = TokenCloseParen : lexer cs
    
parseError :: [Token] -> a
parseError _ = error "Parse error"

}
