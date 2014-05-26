import Data.Char (chr, ord)
import System.IO
import Debug.Trace
import System.Environment
import Control.Monad

data BFCommand = GoRight | GoLeft | Increment | Decrement | Print | Read | While | Wend
	| Loop BFSource | IncrementN Int | GoRightN Int | GoLeftN Int | SetZero
	deriving (Eq, Show)

type BFSource = [BFCommand]

mapMaybes :: (a -> Maybe b) -> [a] -> [b]
mapMaybes _ [] = []
mapMaybes f (x: xs) =
	let rs = mapMaybes f xs in
		case f x of
			Just e -> e: rs
			Nothing -> rs

parseBrainfuck :: String -> BFSource
parseBrainfuck = mapMaybes charToBF
	where charToBF c = case c of
		'>' -> Just GoRight
		'<' -> Just GoLeft
		'+' -> Just Increment
		'-' -> Just Decrement
		'.' -> Just Print
		',' -> Just Read
		'[' -> Just While
		']' -> Just Wend
		_ -> Nothing

outputBrainfuck :: BFSource -> String
outputBrainfuck [] = []
outputBrainfuck (c: cs) = case c of
	GoRight -> '>': outputBrainfuck cs
	GoLeft -> '<': outputBrainfuck cs
	Increment -> '+': outputBrainfuck cs
	Decrement -> '-': outputBrainfuck cs
	Print -> '.': outputBrainfuck cs
	Read -> ',': outputBrainfuck cs
	While -> '[': outputBrainfuck cs
	Wend -> ']': outputBrainfuck cs
	IncrementN i -> show (abs i) ++ ((if i > 0 then '+' else '-'): outputBrainfuck cs)
	GoRightN i -> show i ++ ('>': outputBrainfuck cs)
	GoLeftN i -> show i ++ ('<': outputBrainfuck cs)
	SetZero -> '=':'0': outputBrainfuck cs
	Loop cs' -> ('{' : outputBrainfuck cs') ++ "}" ++ outputBrainfuck cs

data Tape a = Tape [a] a [a]

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
	where zeros = repeat 0

moveRight :: Tape a -> Tape a
moveRight (Tape ls p (r: rs)) = Tape (p: ls) r rs
moveRight (Tape _ _ []) = error "Nothing on the right hand side"

moveRightN :: Int -> Tape a -> Tape a
moveRightN 0 t = t
moveRightN i t = moveRightN (i - 1) (moveRight t)

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l: ls) p rs) = Tape ls l (p: rs)
moveLeft (Tape [] _ _) = error "Nothing on the left hand side"

moveLeftN :: Int -> Tape a -> Tape a
moveLeftN 0 t = t
moveLeftN i t = moveLeftN (i - 1) (moveLeft t)

runBrainfuck :: Bool -> BFSource -> IO ()
runBrainfuck debug bfs = run emptyTape (transformBrainfuck debug bfs) >> return ()

runBrainfuckInteract :: Bool -> BFSource -> String -> (String, Tape Int, String)
runBrainfuckInteract debug bfs input = runI emptyTape (transformBrainfuck debug bfs) input

transformBrainfuck :: Bool -> BFSource -> BFSource
transformBrainfuck debug bfs =
	let tbfs = gatherLoops bfs in if debug
		then trace (outputBrainfuck tbfs) tbfs
		else tbfs

gatherLoops :: BFSource -> BFSource
gatherLoops [] = []
gatherLoops (c: cs) = case c of
	While -> let (loop, rest) = takeWhileIsSameLoop cs in
		Loop (gatherLoops loop): gatherLoops rest
	Wend -> error "Unexpected closing bracket"
	_ -> c: gatherLoops cs

takeWhileIsSameLoop :: BFSource -> (BFSource, BFSource)
takeWhileIsSameLoop cs = inner 0 cs where
	inner :: Int -> BFSource -> (BFSource, BFSource)
	inner _ [] = error "Missing closing braket"
	inner 0 (Wend: cs') = ([], cs')
	inner b (c: cs') = let { (loop, rest) = inner (case c of
		While -> b + 1
		Wend -> b - 1
		_ -> b) cs' } in (c: loop, rest)

optimize :: BFSource -> BFSource
optimize (Increment:Increment: cs) = optimizeHelper IncrementN Increment cs
optimize (Decrement:Decrement: cs) = optimizeHelper (\i -> IncrementN (-i)) Decrement cs
optimize (GoRight:GoRight: cs) = optimizeHelper GoRightN GoRight cs
optimize (GoLeft:GoLeft: cs) = optimizeHelper GoLeftN GoLeft cs
optimize (While:Decrement:Wend: cs) = SetZero: optimize cs
optimize (c: cs) = c: optimize cs
optimize [] = []

optimizeHelper :: (Int -> BFCommand) -> BFCommand -> BFSource -> BFSource
optimizeHelper commandN cmd cs = commandN (2 + length (takeWhile (== cmd) cs)): optimize (dropWhile (== cmd) cs)

run :: Tape Int -> BFSource -> IO (Tape Int)
run t [] = return t

run dataT@(Tape _ p _) (Print: rest) =
	putChar (chr p) >> run dataT rest

run (Tape l p r) (Read: rest) = do
	maybeC <- catch (liftM Just getChar) (const $ return $ Nothing)
	run (Tape l (case maybeC of Just c -> ord c; Nothing -> p) r) rest

run dataT@(Tape _ p _) source@((Loop loop): rest)
	| p == 0 = run dataT rest -- skip the loop
	| otherwise = do
		dataT' <- run dataT loop
		run dataT' source

run dataT@(Tape l p r) (c: rest) =
	run (case c of
		GoRight -> moveRight dataT
		GoRightN i -> moveRightN i dataT
		GoLeft -> moveLeft dataT
		GoLeftN i -> moveLeftN i dataT
		Increment -> Tape l (p + 1) r
		Decrement -> Tape l (p - 1) r
		IncrementN i -> Tape l (p + i) r
		SetZero -> Tape l 0 r
		While -> error "While found"
		Wend -> error "Wend found"
		_ -> error "logic error in run"
		) rest

runI :: Tape Int -> BFSource -> String -> (String, Tape Int, String)
runI t [] i = ([], t, i)

runI dataT@(Tape _ p _) (Print: rest) input =
	let (nextOutput, nextTape, nextInput) = runI dataT rest input in
	(chr p: nextOutput, nextTape, nextInput)

runI dataT@(Tape l _ r) (Read: rest) input = case input of
	[] -> runI dataT rest []
	(c: cs) -> runI (Tape l (ord c) r) rest cs

runI dataT@(Tape _ p _) source@((Loop loop): rest) input
	| p == 0 = runI dataT rest input
	| otherwise = let
		(loopOutput, loopTape, loopInput) = runI dataT loop input
		(nextOutput, nextTape, nextInput) = runI loopTape source loopInput in
		(loopOutput ++ nextOutput, nextTape, nextInput)

runI dataT@(Tape l p r) (c: rest) input =
	runI (case c of
		GoRight -> moveRight dataT
		GoRightN i -> moveRightN i dataT
		GoLeft -> moveLeft dataT
		GoLeftN i -> moveLeftN i dataT
		Increment -> Tape l (p + 1) r
		Decrement -> Tape l (p - 1) r
		IncrementN i -> Tape l (p + i) r
		SetZero -> Tape l 0 r
		While -> error "While found"
		Wend -> error "Wend found"
		_ -> error "logic error in run"
		) rest input

getOutput :: (String -> (String, Tape a, String)) -> String -> String
getOutput f i = let (o, _, _) = f i in o

main :: IO ()
main = do
	hSetBuffering stdout NoBuffering
	args <- getArgs
	readFile (case args of
		[] -> error "Usage: bf2 <program.bf> [-O]"
		(filename: _) -> filename
		) >>= (if "-i" `elem` args then runBFI args else runBF args)
	where
		debug args = "-d" `elem` args
		optimize' args = if "-O" `elem` args then optimize else id
		runBF args = runBrainfuck (debug args) . (optimize' args) . parseBrainfuck
		runBFI args source = interact $ getOutput $ (runBrainfuckInteract (debug args) . (optimize' args) . parseBrainfuck) source
