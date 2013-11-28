Zij gegeven:

(++) :: [a] [a] -> [a]
(++) []     xs = xs                (1)
(++) [y:ys] xs = [y : ys ++ xs]    (2)

map :: (a -> b) [a] -> [b]
map f []       = []                (3)
map f [x:xs]   = [f x : map f xs]  (4)

flatten :: [[a]] -> [a]
flatten []     = []                (5)
flatten [x:xs] = x ++ (flatten xs) (6)

1. 
Te bewijzen: 
	voor iedere functie f, eindige lijst as en bs:
		
		map f (as ++ bs) = (map f as) ++ (map f bs)

Bewijs:
	Met inductie naar de lengte van as.
	Basis:
	Aanname: as = [].
	
		map f ([] ++ bs)				// basis aanname
	=	map f (bs)						// (1)
	
	=	[] ++ map f bs					// (1) <=
	=	(map f []) ++ (map f bs)		// (3) <=
	= 	(map f as) ++ (map f bs)		// basis aanname <=
	
	
	Inductiestap:
	Aanname: stelling geldt voor zekere as, ofwel: 
	
		map f (as ++ bs) = (map f as) ++ (map f bs)		// (IH)
	
	Te bewijzen: stelling geldt ook voor [a : as], ofwel:
	map f ([a:as] ++ bs) = (map f [a:as]) ++ (map f bs)
	
	Bewijs:
		map f ([a : as] ++ bs)			// basis aanname
	= 	map f [a : as ++ bs]			// (2)
	= 	[f a : map f (as ++ bs)]		// (4)
	
	=	[f a : (map f as) ++ (map f bs)]// IH
	= 	[f a : map f as] ++ (map f bs) 	// (2) <=
	= 	(map f [a : as]) ++ (map f bs)	// (4) <=

	Dus: basis + inductiestap => stelling bewezen.
	
2. 
Te bewijzen:
	voor iedere functie f, voor iedere eindige lijst xs:
	
		flatten (map (map f) xs) = map f (flatten xs)

Bewijs:
