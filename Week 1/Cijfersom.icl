module Cijfersom

import StdEnv

Start = (cijfersom 9876543, cijfersom 1000, cijfersom 5)



eerstecijfer 	:: Int -> Int
eerstecijfer i 	= toInt ((toString i) % (0, 0))

restcijfers 	:: Int -> Int
restcijfers 0	= 0
restcijfers i 	= toInt ((toString i) % (1, size (toString i)))



cijfersom 		:: Int -> Int
cijfersom i 	
| i < 10		= i
| otherwise		= (eerstecijfer i) + cijfersom (restcijfers i)
