type bFCommand = GoRight | GoLeft | Increment | Decrement | Print | Read | While | Wend
	| Loop of bFProgram | IncrementN of int | GoRightN of int | GoLeftN of int | SetZero
and bFProgram = bFCommand array

let rec mapOptions f = function
	| [] -> []
	| x :: xs -> let rest = mapOptions f xs in
		match f x with
		| None -> rest
		| Some e -> e :: rest

and charToBF = function
	| '>' -> Some GoRight
	| '<' -> Some GoLeft
	| '+' -> Some Increment
	| '-' -> Some Decrement
	| '.' -> Some Print
	| ',' -> Some Read
	| '[' -> Some While
	| ']' -> Some Wend
	| _ -> None

and parseBrainfuck source =
	let result = ref [] in
	for i = String.length source - 1 downto 0 do
		result := source.[i] :: !result
	done;
	mapOptions charToBF !result

and optimize = function
	| Increment :: Increment :: rest -> optimizeHelper (fun i -> IncrementN i) Increment rest
	| Decrement :: Decrement :: rest -> optimizeHelper (fun i -> IncrementN (-i)) Decrement rest
	| GoRight :: GoRight :: rest -> optimizeHelper (fun i -> GoRightN i) GoRight rest
	| GoLeft :: GoLeft :: rest -> optimizeHelper (fun i -> GoLeftN i) GoLeft rest
	| While :: Decrement :: Wend :: rest -> SetZero :: optimize rest
	| x :: rest -> x :: optimize rest
	| [] -> []

and optimizeHelper commandN cmd rest =
	let count, rest' = countAtStart cmd rest in
	commandN (2 + count) :: optimize rest'

and countAtStart cmd = function
	| [] -> 0, []
	| x :: xs as all ->
		if x = cmd then
			let count, xs' = countAtStart cmd xs in
			count + 1, xs'
		else
			0, all

(* and gatherLoops source = *)


type tape = {t_a: int array; mutable t_p: int}

let emptyTape () = {t_a = Array.make (1024*256) 0; t_p = 0}

let moveRight t = t.t_p <- t.t_p + 1
and moveLeft t = t.t_p <- t.t_p - 1

let moveRightN t i = t.t_p <- t.t_p + i
and moveLeftN t i = t.t_p <- t.t_p - i

