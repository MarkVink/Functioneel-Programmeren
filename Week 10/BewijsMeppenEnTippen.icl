Zij gegeven:

:: BTree a              = Tip a | Bin (BTree a) (BTree a)

map                     :: (a -> b) [a] -> [b]
map f []                = []                                    (1.)
map f [x:xs]            = [f x : map f xs]                      (2.)

mapbtree                :: (a -> b) (BTree a) -> BTree b
mapbtree f (Tip a)      = Tip (f a)                             (3.)
mapbtree f (Bin t1 t2)  = Bin (mapbtree f t1) (mapbtree f t2)   (4.)

foldbtree               :: (a a -> a) (BTree a) -> a
foldbtree f (Tip x)     = x                                     (5.)
foldbtree f (Bin t1 t2) = f (foldbtree f t1) (foldbtree f t2)   (6.)

tips                    :: (BTree a) -> [a]
tips t                  = foldbtree (++) (mapbtree unit t)      (7.)

unit                    :: a -> [a]
unit x                  = [x]                                   (8.)

Leg uit wat de functies foldbtree en tips doen.
 -	foldbtree: Loopt recursief door de gegeven boom, bij het vinden 
 	van een Tip wordt deze waarde gerouteerd. 
 -	tips: Loopt recursief door de gegeven boom en retourneerd alle 
 	Tippen samengevoegd als lijst.


Te bewijzen:
    voor alle functies f, voor alle eindige bomen t:
    
        map f (tips t) = tips (mapbtree f t)

Bewijs:
	Met inductie voor alle eindige bomen in t
	Basis:
	Aanname: t = Tip a
	
		map f (tips (Tip a)) 								// basisaanname
	=	map f (foldbtree (++) (mapbtree unit (Tip a))) 		// (7)
	=	map f (foldbtree (++) (Tip (unit a))) 				// (3)
	=	map f (foldbtree (++) (Tip [a])) 					// (8)
	=	map f [a] 											// (5)
	=	[f a : map f []]									// (2)
	=	[f a : []]											// (2)
	=	[f a]
	=	foldbtree (++) (Tip [f a])) 						// (5) <=
	=	foldbtree (++) (Tip (unit (f a))) 					// (8) <=
	=	foldbtree (++) (mapbtree unit (Tip (f a))) 			// (3) <=
	=	tips (Tip (f a)) 									// (7) <=
	=	tips (mapbtree f (Tip a)) 							// (3) <=
	=	tips (mapbtree f t) 								// basisaanname <=
	
	
	Inductiestap:
	Aanname: stelling geldt voor zekere bomen, ofwel:
	
		map f (tips t) = tips (mapbtree f t)				// (IH)
		
	Te bewijzen: stelling geldt ook voor t3 = Bin t1 t2, ofwel:
	
		map f (tips (Bin t1 t2)) = tips (mapbtree f (Bin t1 t2))
				
	Bewijs:

		map f (tips (Bin t1 t2)) 																	// basisaanname
	=	map f (foldbtree (++) (mapbtree unit (Bin t1 t2))) 											// (7.)
	=	map f (foldbtree (++) (Bin (mapbtree unit t1) (mapbtree unit t2))) 							// (4.)
	=	map f ((++) (foldbtree (++) (mapbtree unit t1)) (foldbtree (++) (mapbtree unit t2))) 		// (6.)
	= 	(map f (foldbtree (++) (mapbtree unit t1))) ++ (map f (foldbtree (++) (mapbtree unit t2)))	// (9.4.1)
	= 	(map f (tips t1)) ++ (map f (foldbtree (++) (mapbtree unit t2))) 							// (7.) <=
	= 	(map f (tips t1)) ++ (map f (tips t2)) 														// (7.) <=
	= 	(tips (mapbtree f t1)) ++ (map f (tips t2)) 												// (IH)
	= 	(tips (mapbtree f t1)) ++ (tips (mapbtree f t2)) 											// (IH)
	= 	(++) (foldbtree (++) (mapbtree unit t1)) ++ (tips (mapbtree f t2)) 							// (7.)
	= 	(++) (foldbtree (++) (mapbtree unit t1)) (foldbtree (++) (mapbtree unit t2)) 				// (7.)
	= 	foldbtree (++) (Bin (mapbtree unit t1) (mapbtree unit t2)) 									// (6.) <=
	= 	foldbtree (++) (mapbtree unit (Bin t1 t2)) 													// (4.) <=
	= 	tips (Bin t1 t2) 																			// (7.) <=
	=	tips (mapbtree f (Bin t1 t2)) 																// (4.) <=
	
	Dus: basis + inductiestap => stelling bewezen.	