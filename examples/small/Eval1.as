module Eval1 ehere

-- Toy lambda-calculus interpreter from John Hughes's arrows paper (s5)

import Control.Arrow
import Data.Maybe(fromJust)

type Id = String
data Val a = Num Int | Bl Bool | Fun (a (Val a) (Val a))
data Exp = Var Id | Add Exp Exp | If Exp Exp Exp | Lam Id Exp | App Exp Exp

eval :: (ArrowChoice a, ArrowApply a) => Exp -> a [(Id, Val a)] (Val a)
eval (Var s) = proc env ->
		returnA -< fromJust (lookup s env)
eval (Add e1 e2) = proc env ->
		(eval e1 -< env) `bind` \ ~(Num u) ->
		(eval e2 -< env) `bind` \ ~(Num v) ->
		returnA -< Num (u + v)
eval (If e1 e2 e3) = proc env ->
		(eval e1 -< env) `bind` \ ~(Bl b) ->
		if b then eval e2 -< env
			else eval e3 -< env
eval (Lam x e) = proc env ->
		returnA -< Fun (proc v -> eval e -< (x,v):env)
eval (App e1 e2) = proc env ->
		(eval e1 -< env) `bind` \ ~(Fun f) ->
		(eval e2 -< env) `bind` \ v ->
		f -< v

bind :: Arrow a => a b c -> a (b,c) d -> a b d
e `bind` f = returnA &&& e >>> f

-- some tests

i = Lam "x" (Var "x")
k = Lam "x" (Lam "y" (Var "x"))
double = Lam "x" (Add (Var "x") (Var "x"))

t = n
	where Num n = eval (If (Var "b")
			(App (App k (App double (Var "x"))) (Var "x"))
			(Add (Var "x") (Add (Var "x") (Var "x"))))
		[("b", Bl True), ("x", Num 5)]
