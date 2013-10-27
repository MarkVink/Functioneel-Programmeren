implementation module RefactorX

import StdEnv

//Start = map toString [E1,E2,E3,E4,E5]

/*
E1 = (let x = 42 - 3 in x / 0) + (let y = 6 in y * y)
E2 = let x = 42 in x + (let x = 58 in x)
E3 = let x = 1 in let y = 2 in let x = 3 in 4
E4 = let x = 1 in x + y
E5 = (let x = 1 in x) * x
*/

E1 = (OP (LET "x" (OP (NR 42) MIN (NR 3)) (OP (VAR "x") DIV (NR 0))) PLUS (LET "y" (NR 6) (OP (VAR "y") MUL (VAR "y"))))
E2 = (LET "x" (NR 42) (OP (VAR "x") PLUS (LET "x" (NR 58) (VAR "x"))))
E3 = (LET "x" (NR 1) (LET "y" (NR 2) (LET "x" (NR 3) (NR 4))))
E4 = (LET "x" (NR 1) (OP (VAR "x") PLUS (VAR "y")))
E5 = (OP (LET "x" (NR 1) (VAR "x")) MUL (VAR "x"))

::	Expr							= NR   Int
									| VAR  Name
									| OP   Expr Operator Expr
									| LET  Name     Expr Expr
::	Name							:== String
::	Operator						= PLUS | MIN | MUL | DIV
::	Val								= Result Int | Undef

//  expressies afdrukken:
instance toString Expr where
	toString (NR i) 			= toString i
	toString (VAR c)			= toString c
	toString (OP lhs op rhs)	= toString lhs +++ " " +++ toString op +++ " " +++ toString rhs		
	toString (LET na lhs rhs) 	= "(let " +++ na +++ " = " +++ toString lhs +++ " in " +++ toString rhs +++ ")"
	
instance toString Operator where
	toString PLUS			= "+"
	toString MIN			= "-"
	toString MUL			= "*"
	toString DIV			= "/"


//	vrije variabelen:
free 								:: Expr -> [Name]
free (VAR c) 			= [c]
free (LET na _ rhs) 	= filter ((<>) na) (free rhs)
free (OP lhs op rhs) 	= free lhs ++ free rhs
free a					= []

//	verwijder deelexpressies met ongebruikte let-variabelen:
remove_unused_lets					:: Expr -> Expr
remove_unused_lets (LET na lhs rhs)   
| exists na (free rhs)	= (LET na lhs (remove_unused_lets rhs))
| otherwise 			= remove_unused_lets rhs
	where
		exists :: a [a] -> Bool | Eq a
		exists a [] = False
		exists a [x : xs]
		| x == a 	= True
		| otherwise = exists a xs
remove_unused_lets a	= a
	
eval								:: Expr -> Val
eval a	
| free a == [] 	= eval` (remove_unused_lets a)
| otherwise		= Undef
where
	eval`								:: Expr -> Val
	eval` (NR i)	= (Result i)
	eval` (OP lhs op rhs) 
	| undef (eval lhs) 				= Undef
	| undef (eval rhs) 				= Undef
	| otherwise						= operate op (eval lhs) (eval rhs)
	where
		undef :: Val -> Bool
		undef (Result i) 	= False
		undef _ 			= True
		operate :: Operator Val Val -> Val
		operate PLUS (Result a) (Result b) 	= Result (a + b)
		operate MIN (Result a) (Result b) 	= Result (a - b)
		operate MUL (Result a) (Result b) 	= Result (a * b)
		operate DIV (Result a) (Result b)
		| b == 0							= Undef
		| otherwise							= Result (a / b)
	eval` (LET na lhs rhs) = eval` (replace (VAR na) lhs rhs)
		where
		replace :: Expr Expr Expr -> Expr
		replace (VAR na) rep (VAR ori) 
		| na == ori  		= rep
		| otherwise			= (VAR ori)
		replace (VAR na) rep (OP lhs op (VAR rhs))
		| na == rhs 		= (OP (replace (VAR na) rep lhs) op rep)
		replace (VAR na) rep (OP (VAR lhs) op rhs)
		| na == lhs 		= (OP rep op (replace (VAR na) rep rhs))
		replace _ _ ori 	= ori
