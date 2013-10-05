module NotatieHOF

import StdEnv

f1 :: a b -> a b// a is een functie, b een variabele
f1 a b = a b

f2 :: a b c -> a b (b b)// a, b en c zijn functies
f2 a b c = a c (b c)

f3 :: a b -> a b// a is een functie, b is een variabele
f3 a b = a (a b)

f4 :: a b b -> b // a is een functie, b en c zijn variabelen
f4 a b c = [x \\ x <- [b .. c] | a x]

f5 :: a b cd -> (c,d) // a en b zijn functies, (c,d) is een tuple
f5 a b (c,d) = (a c,b d)

f6 :: a // a is een functie
f6 = f5

f7 :: Char -> a// a is een instance 
f7 "-" = -
f7 "+" = +
f7 "*" = *
f7 "/" = /

Start = 0
