implementation module RefactorXX

import StdClass, StdInt, StdList, StdOverloaded, StdString

//	Voor de [] instance van MonadFail:
//Start :: [[Int]]
//	Voor de Val instance van MonadFail:
Start :: [Val Int]
Start = [eval E1, eval E2, eval E3, eval E4, eval E5]

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

::	Expr		= NR   Int
				| VAR  Name
				| OP   Expr Operator Expr
				| LET  Name     Expr Expr
::	Name	  :== String
::	Operator	= PLUS | MIN | MUL | DIV
::	Val a		= Result a | Undef

class fail          c :: c a
class return        c :: a -> c a
class (>>=) infix 0 c :: (c a) (a -> c b) -> c b
class Monad         c | return, >>= c
class MonadFail     c | Monad, fail c

instance fail Val
	where fail 				= Undef
instance return Val
	where return a 			= Result a
instance >>= Val
	where >>= (Result a) f 	= f a
		  >>= Undef f 		= Undef
		  
instance fail [] 
	where fail 				= []
instance return [] 
	where return x 			= [x]
instance >>= []
	where >>= xs f 			= [y \\ x <- xs, y <- f x]

eval :: Expr -> c Int | MonadFail c
eval a	
| free a == [] 				= eval` (remove a)
| otherwise					= fail
where
	free :: Expr -> [Name]
	free (VAR a) 			= [a]
	free (LET name _ e2) 	= filter ((<>) name) (free e2)
	free (OP e1 op e2) 		= free e1 ++ free e2
	free a					= []
	
	remove :: Expr -> Expr
	remove (LET name e1 e2)   
	| exists name (free e2)	= (LET name e1 (remove e2))
	| otherwise 			= remove e2
	where
		exists :: a [a] -> Bool | Eq a
		exists a [] 		= False
		exists a [x : xs]
		| x == a 			= True
		| otherwise 		= exists a xs
	remove a				= a
	
	eval` :: Expr -> c Int | MonadFail c
	eval` (NR n) = return n
	eval` (OP e1 op e2) 	= eval e1 >>= \v1 
							= eval e2 >>= \v2 
							= if (valid op v1 v2) (return (apply op v1 v2)) fail
	where
		apply :: Operator Int Int -> Int
		apply PLUS x y 		= x + y
		apply MIN x y 		= x - y
		apply MUL x y 		= x * y
		apply DIV x y 		= x / y 
		
		valid :: Operator Int Int -> Bool
		valid PLUS _ _ 		= True
		valid MIN _ _ 		= True
		valid MUL _ _ 		= True
		valid DIV x y 		= y <> 0
		
	eval` (LET name e1 e2) 	= eval` (replace (VAR name) e1 e2)
	where
		replace :: Expr Expr Expr -> Expr
		replace (VAR name) rep (VAR ori) 
		| name == ori  		= rep
		| otherwise			= (VAR ori)
		replace (VAR name) rep (OP e1 op (VAR e2))
		| name == e2 		= (OP (replace (VAR name) rep e1) op rep)
		replace (VAR na) rep (OP (VAR e1) op e2)
		| name == e1 		= (OP rep op (replace (VAR name) rep e2))
		replace _ _ ori 	= ori

