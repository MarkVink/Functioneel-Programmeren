module NotatieADT

import StdEnv

:: Dag      = Maandag | Dinsdag | Woensdag | Donderdag | Vrijdag | Zaterdag | Zondag
:: Nat      = Nat Int
:: Getal    = Geheel Nat | Decimaal Real
:: Functies = F0 Int | F1 (Int -> Int) | F2 (Int Int -> Int)
:: Void     = Void

//Start = Maandag
//Start = Dinsdag
//Start = Woensdag

//Start = Nat 0
//Start = Nat 1
//Start = Nat 2

//Start = Geheel (Nat 0)
//Start = Geheel (Nat 1)
//Start = Decimaal 1.1

//Start = F0 0
Sample4F1 :: Int -> Int 
Sample4F1 a = a 
//Start = F1 Sample4F1 
Sample4F2 :: Int Int -> Int 
Sample4F2 a b = a * b 
//Start = F2 Sample4F2 

//Start = Void