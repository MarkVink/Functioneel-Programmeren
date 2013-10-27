definition module RefactorX

import StdEnv

::	Expr			= NR   Int
					| VAR  Name
					| OP   Expr Operator Expr
					| LET  Name     Expr Expr
::	Name		  :== String
::	Operator		= PLUS | MIN | MUL | DIV
::	Val				= Result Int | Undef

instance toString Expr
free				:: Expr -> [Name]
remove_unused_lets	:: Expr -> Expr
eval				:: Expr -> Val
