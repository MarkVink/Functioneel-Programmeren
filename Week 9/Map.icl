implementation module Map

import BinTree		// voor het type Tree en voorbeelden t0 t/m t7
import Maybe        // voor het type Maybe
import StdList      // voor de standaard map functie

//class Map (a -> b) [a] -> [b]    

//class mapMaybe	m::(a -> b) (Maybe a) -> Maybe b

class Map m :: (a -> b) (m a) -> (m b)

instance Map []
	where Map f x 		= map f x
instance Map Maybe
	where Map f x 		= mapMaybe f x
instance Map Tree
	where Map f x 		= mapTree f x

/*
:: Void a = Void
:: List2 a = Single a | Double a a

instance Map Void
	where Map f x = Void
instance Map List2 
	where 
		Map f (Single a) = Single (f a)
		Map f (Double a b) = Double (f a) (f b)  
*/

// voorgegeven functie, specifiek voor Maybe:
mapMaybe 		        :: (a -> b) (Maybe a) -> Maybe b
mapMaybe f Nothing	    = Nothing
mapMaybe f (Just x)	    = Just (f x)

// voorgegeven functie, specifiek voor Tree:
mapTree 			    :: (a -> b) (Tree a) -> Tree b
mapTree f Leaf		    = Leaf
mapTree f (Node x l r)	= Node (f x) (mapTree f l) (mapTree f r)

