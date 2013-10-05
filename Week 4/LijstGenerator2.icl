implementation module LijstGenerator2

import StdEnv

allemaal				:: a -> [a]
allemaal x				= [x \\ i <- [0 ..]]

vanaf					:: a -> [a] | + a & one a
vanaf x					= [x : vanaf (x + one)]

vanaf_met_stap			:: a a -> [a] | + a
vanaf_met_stap x z		= [x : vanaf_met_stap (x + z) z]

vanaf_tot				:: a a -> [a] | + a & one a & Ord a
vanaf_tot x y			= [x \\ x <- (x + one) | x < y]

/*
vanaf_tot_met_stap		:: // meest algemene type
vanaf_tot_met_stap x y z= ...

Start					= ( take 10 (allemaal 'H')
						  , '\n'
						  , take 26 (vanaf 'a')
						  , '\n'
						  , take 10 (vanaf_met_stap 0 -2)
						  , '\n'
						  , vanaf_tot 0 10
						  , '\n'
						  , vanaf_tot_met_stap 0 -10 -2
						  )
*/

Start 		= vanaf_tot 1 10