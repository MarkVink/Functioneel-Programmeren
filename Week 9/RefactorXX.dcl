definition module RefactorXX

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

instance fail   [], Val
instance return [], Val
instance >>=    [], Val

eval			:: Expr -> c Int | MonadFail c
