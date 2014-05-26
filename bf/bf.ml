type bFCommand = GoRight | GoLeft | Increment | Decrement | Print | Read | While | Wend
	| Loop BFSource | IncrementN of int | GoRightN of int | GoLeftN of int | SetZero
and bFSource = bFCommand list

let rec mapOptions f = function
	| [] -> []
	| x :: xs -> let rest = mapOptions f in
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
