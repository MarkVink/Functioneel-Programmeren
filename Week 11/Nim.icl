implementation module Nim

import StdEnv, RandomGetallen

// Definitions

instance toString Player where
         toString PLAYER_CPU     = "CPU"
         toString PLAYER_HUMAN	= "Human"

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
#(world, gs) = turn world gs
= startGame world gs

turn :: *World GameState -> (*World, GameState)
turn world gs
#gs = checkGameOver gs
| toString gs.turn == "CPU"		= turnCPU world gs
| otherwise						= turnHuman world gs
where
	turnHuman :: *World GameState -> (*World, GameState)
	turnHuman world gs
	#(world, gs) = showTurn world gs
	#(world, gs, row) = askRow world gs
	#(world, gs) = askStone world gs row
	#gs = {gs & turn = PLAYER_CPU}
	= (world, gs)
	turnCPU :: *World GameState -> (*World, GameState)
	turnCPU world gs
	#(world, gs) = showTurn world gs
	#(world, gs, row) = choseRow world gs
	#(world, gs) = choseStone world gs row
	#gs = {gs & turn = PLAYER_HUMAN}
	= (world, gs)
	showTurn :: *World GameState -> (*World, GameState)
	showTurn world gs
	= (writeln ((drawBoard gs.board) +++ (toString gs.turn) +++ "s turn:") world, gs)
	askRow :: *World GameState -> (*World, GameState, String)
	askRow world gs
	#(gs, rows) = possibleRows gs
	#(s, world) = readln ("Which row? [" +++ (implode "/" rows) +++ "]") world
	#(gs, max) = maxStones gs s
	| s == "A" && max > 0 = (world, gs, s)
	| s == "B" && max > 0 = (world, gs, s)
	| s == "C" && max > 0 = (world, gs, s)
	| s == "D" && max > 0 = (world, gs, s)
	| s == "E" && max > 0 = (world, gs, s)
	| otherwise			= askRow world gs
	choseRow :: *World GameState -> (*World, GameState, String)
	choseRow world gs
	#(gs, rows) = possibleRows gs
	#(rs, world) = getNewRandomSeed world
	#s = last (shuffle rows rs)
	#world = write ("CPU choose row: " +++ s) world
	#(gs, max) = maxStones gs s
	| s == "A" && max > 0 = (world, gs, s)
	| s == "B" && max > 0 = (world, gs, s)
	| s == "C" && max > 0 = (world, gs, s)
	| s == "D" && max > 0 = (world, gs, s)
	| s == "E" && max > 0 = (world, gs, s)
	| otherwise			= choseRow world gs
	possibleRows :: GameState -> (GameState, [String])
	possibleRows gs
	#s = []
	#s = if(gs.board.a > 0) (s ++ ["A"]) s
	#s = if(gs.board.b > 0) (s ++ ["B"]) s
	#s = if(gs.board.c > 0) (s ++ ["C"]) s
	#s = if(gs.board.d > 0) (s ++ ["D"]) s
	#s = if(gs.board.e > 0) (s ++ ["E"]) s
	= (gs, s)
	askStone :: *World GameState String -> (*World, GameState)
	askStone world gs row
	#(gs, max) = maxStones gs row
	#(s, world) = readln ("How many stones do you want? [ 1 .. " +++ (toString max) +++ " ]") world
	#stones = (toInt s)
	| stones > 0 && stones <= max	= (world, (takeStone gs row stones))
	| otherwise			= askStone world gs row
	choseStone :: *World GameState String -> (*World, GameState)
	choseStone world gs row
	#(gs, max) = maxStones gs row
	#(rs, world) = getNewRandomSeed world
	#stones = last (shuffle [1 .. max] rs)
	#world = writeln (" with " +++ (toString stones) +++ " stone(s)") world
	= (world, (takeStone gs row stones))
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

checkGameOver :: GameState -> GameState
checkGameOver gs
| gameOver gs.board 	= abort ("Game over; " +++ (toString gs.turn) +++ " wins!")
| otherwise 			= gs 
where
	gameOver :: Board -> Bool
	gameOver board
	| sumStones board == 0 	= True
	| otherwise 			= False 
	sumStones :: Board -> Int
	sumStones board	= board.a + board.b + board.c + board.d + board.e
	
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

// Util functions
implode :: String [String] -> String
implode s [] 		= ""
implode s [x] 		= x
implode s [x:xs] 	= x +++ s +++ (implode s xs)

// Console functions	
write :: String *world -> *world | FileSystem world
write message world
#(console, world) = stdio world
#console = fwrites message console
#(ok, world) = fclose console world
= world

writeln :: String *world -> *world | FileSystem world
writeln message world = write (message +++ "\n") world

readln :: String *world -> (String, *world) | FileSystem world
readln message world
#(console, world) = stdio world
#(s, console) = freadline (console <<< message)
#s = rmnln s
#(ok, world) = fclose console world
= (s, world)

rmnln :: !String -> String
rmnln "\n" = ""
rmnln str
#last = size str - 1
| last <= 0 || str.[last] <> '\n' = str
| otherwise = str % (0, last - 1)
