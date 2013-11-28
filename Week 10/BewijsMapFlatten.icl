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
	Met inductie naar de lengte van xs.
	Basis:
	Aanname: xs = [].
	
		flatten (map (map f) [])		// basis aanname
	=	flatten []						// (3)
	= 	[]								// (5)
	=	map f [] 						// (3) <=
	= 	map f (flatten [])				// (5) <=
	= 	map f (flatten xs)				//  basis aanname <=

	Inductiestap:
	Aanname: stelling geldt voor zekere xs, ofwel:
	
		flatten (map (map f) xs) = map f (flatten xs) 	// (IH)
		
	Te bewijzen: stelling geldt voor ook voor [x : xs], ofwel:
	
		flatten (map (map f) [x : xs]) = map f (flatten [x : xs])
		
	Bewijs:
		flatten (map (map f) [x : xs])			// basis aanname
	=	flatten [(map f) x : map (map f) xs]	// (4)
	=	(map f x) ++ flatten (map (map f) xs)	// (6)
	
	=	(map f x) ++ map f (flatten xs)			// (IH)
	=	map f (x ++ (flatten xs))				// 9.4.1 <= 
	=	map f (flatten [x : xs])				// (6) <=

	Dus: basis + inductiestap => stelling bewezen.
	
	