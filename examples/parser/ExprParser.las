> module ExprParser where

> import Data.Char

> import Control.Arrow
> import Control.Arrow.Transformer.Error

> import Parser

Expressions

> data ESym = LPar | RPar | Plus | Minus | Mult | Div | Number | Unknown | EOF
>	deriving (Show, Eq, Ord)

> instance Symbol ESym where
>	eof = EOF

> type ExprParser = Parser ESym String (->)
> type ExprSym = Sym ESym String

The grammar

> expr :: ExprParser () Int
> expr = proc () -> do
>		x <- term -< ()
>		expr' -< x

> expr' :: ExprParser Int Int
> expr' = proc x -> do
>		returnA -< x
>	<+> do
>		symbol Plus -< ()
>		y <- term -< ()
>		expr' -< x + y
>	<+> do
>		symbol Minus -< ()
>		y <- term -< ()
>		expr' -< x - y

> term :: ExprParser () Int
> term = proc () -> do
>		x <- factor -< ()
>		term' -< x

> term' :: ExprParser Int Int
> term' = proc x -> do
>		returnA -< x
>	<+> do
>		symbol Mult -< ()
>		y <- factor -< ()
>		term' -< x * y
>	<+> do
>		symbol Div -< ()
>		y <- factor -< ()
>		term' -< x `div` y

> factor :: ExprParser () Int
> factor = proc () -> do
>		v <- symbol Number -< ()
>		returnA -< read v::Int
>	<+> do
>		symbol Minus -< ()
>		v <- factor -< ()
>		returnA -< -v
>	<+> do
>		symbol LPar -< ()
>		v <- expr -< ()
>		symbol RPar -< ()
>		returnA -< v

Lexical analysis

> lexer :: String -> [ExprSym]
> lexer [] = []
> lexer ('(':cs) = Sym LPar "(":lexer cs
> lexer (')':cs) = Sym RPar ")":lexer cs
> lexer ('+':cs) = Sym Plus "+":lexer cs
> lexer ('-':cs) = Sym Minus "-":lexer cs
> lexer ('*':cs) = Sym Mult "*":lexer cs
> lexer ('/':cs) = Sym Div "/":lexer cs
> lexer (c:cs)
>	| isSpace c = lexer cs
>	| isDigit c = Sym Number (c:w):lexer cs'
>	| otherwise = Sym Unknown [c]:lexer cs
>		where (w,cs') = span isDigit cs

> run parser = runError (runParser parser)
>	(\(_, err) -> error ("parse error: " ++ err)) . lexer

> t = run expr
