implementation module Nim

import StdEnv, RandomGetallen

// Definitions

:: Player = PLAYER_CPU | PLAYER_HUMAN

:: State = STATE_INIT | STATE_RUNNING | STATE_END

:: GameState = { state :: State,  board :: Board, turn :: Player }
	
:: Board = { a :: !Int, b :: !Int, c :: !Int, d :: !Int, e :: !Int }

Start :: *World -> *World
Start world
#(world, gs) = initGameState world 12
#(world, gs) = startGame world gs
= world

startGame :: *World GameState -> (*World, GameState)
startGame world gs
#world = write (drawBoard gs.board)  world
#(world, gs) = turn world gs
= startGame world gs

turn :: *World GameState -> (*World, GameState)
turn world gs
#(world, gs, row) = askRow world gs
#(world, gs) = askStone world gs row
= (world, gs)
where
	askRow :: *World GameState -> (*World, GameState, String)
	askRow world gs
	#(gs, rows) = possibleRows gs
	#(s, world) = readln ("Which row? [" +++ rows +++ "]") world
	#(gs, max) = maxStones gs s
	| s == "A" && max > 0 = (world, gs, s)
	| s == "B" && max > 0 = (world, gs, s)
	| s == "C" && max > 0 = (world, gs, s)
	| s == "D" && max > 0 = (world, gs, s)
	| s == "E" && max > 0 = (world, gs, s)
	| otherwise			= askRow world gs
	possibleRows :: GameState -> (GameState, String)
	possibleRows gs
	#s = " "
	#s = if(gs.board.a > 0) (s +++ "A ") s
	#s = if(gs.board.b > 0) (s +++ "B ") s
	#s = if(gs.board.c > 0) (s +++ "C ") s
	#s = if(gs.board.d > 0) (s +++ "D ") s
	#s = if(gs.board.e > 0) (s +++ "E ") s
	= (gs, s)
	askStone :: *World GameState String -> (*World, GameState)
	askStone world gs row
	#(gs, max) = maxStones gs row
	#(s, world) = readln ("How many stones do you want? [ 1 .. " +++ (toString max) +++ " ]") world
	#stones = (toInt s)
	| stones > 0 && stones <= max	= (world, (takeStone gs row stones))
	| otherwise			= askStone world gs row
	maxStones :: GameState String -> (GameState, Int)
	maxStones gs row
	| row == "A"		= (gs, gs.board.a)
	| row == "B"		= (gs, gs.board.b)
	| row == "C"		= (gs, gs.board.c)
	| row == "D"		= (gs, gs.board.d)
	| row == "E"		= (gs, gs.board.e)	
	| otherwise			= (gs, 0)	
	takeStone :: GameState String Int -> GameState
	takeStone gs row stones
	| row == "A"		= {gs & board.a = (gs.board.a - stones)}
	| row == "B"		= {gs & board.b = (gs.board.b - stones)}
	| row == "C"		= {gs & board.c = (gs.board.c - stones)}
	| row == "D"		= {gs & board.d = (gs.board.d - stones)}
	| row == "E"		= {gs & board.e = (gs.board.e - stones)}


	
//changeState gs = {gs & currentWord = "Bier"}	
	
initGameState :: *World Int -> (*World, GameState)
initGameState world max 
#(world, player)	= determineStartPlayer world
#(rs, world)		= getNewRandomSeed world
= (world, { GameState |
	state	= STATE_INIT,
	board 	= initBoard rs max,
	turn 	= player
	})
where
	determineStartPlayer :: *World -> (*World, Player)
	determineStartPlayer world
	#(input, world) = readln "Would you like to start self? [y/n]" world
	| input == "y"		= (world, PLAYER_HUMAN)
	| input == "n"		= (world, PLAYER_CPU)
	| otherwise			= determineStartPlayer world
	
initBoard :: RandomSeed Int -> Board
initBoard rs max 
#i = randomMax max rs
= { Board |
	a = last (take 1 i),
	b = last (take 2 i),
	c = last (take 3 i),
	d = last (take 4 i),
	e = last (take 5 i)
	}
where
	randomMax :: Int RandomSeed -> [Int]
	randomMax max rs	= (shuffle [3..max] rs)
	last :: [a] -> a
	last []				= abort "List is empty."
	last [a] 			= a
	last [a : xs] 		= last xs
	
drawBoard :: Board -> String
drawBoard board
#s =	   "A: " +++ points(board.a) +++ "\n"
#s = s +++ "B: " +++ points(board.b) +++ "\n"
#s = s +++ "C: " +++ points(board.c) +++ "\n"
#s = s +++ "D: " +++ points(board.d) +++ "\n"
#s = s +++ "E: " +++ points(board.e) +++ "\n"
= s
where 
	points :: Int -> String
	points i 
	| i == 0 	= ""
	| otherwise	= "0" +++ (points (i-1))


// Console functions	
write :: String *env -> *env | FileSystem env
write message env
#(console, env) = stdio env
#console = fwrites message console
#(ok, env) = fclose console env
= env

writeln :: String *env -> *env | FileSystem env
writeln message env = write (message +++ "\n") env

readln :: String *env -> (String, *env) | FileSystem env
readln message env
#(console, env) = stdio env
#(input, console) = freadline (console <<< message)
#input = rmnln input
#(ok, env) = fclose console env
= (input, env)

// Deze is dubbel van FileIO, kan beter in een .dcl
rmnln :: !String -> String
rmnln "\n" = ""
rmnln str
#last = size str - 1
| last <= 0 || str.[last] <> '\n' = str
| otherwise = str % (0, last - 1)
