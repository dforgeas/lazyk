type bFCommand = GoRight | GoLeft | Increment | Decrement | Print | Read | While | Wend
	| Loop of bFSource | IncrementN of int | GoRightN of int | GoLeftN of int | SetZero
and bFSource = bFCommand array

let stringMap f str =
	let len = String.length str in
	let res = Array.make len (f '\x00') in
	for i = 0 to len - 1 do
		res.(i) <- f str.[i]
	done;
	res

let arrayDestructiveFilter p t arr =
	let i_res = ref 0 in
	for i_src = 0 to Array.length arr - 1 do
		if p arr.(i_src) then begin
			arr.(!i_res) <- arr.(i_src);
			incr i_res
		end
	done;
	Array.map t (Array.sub arr 0 !i_res) (* truncate and transform the result *)

let charToBF = function
	| '>' -> Some GoRight
	| '<' -> Some GoLeft
	| '+' -> Some Increment
	| '-' -> Some Decrement
	| '.' -> Some Print
	| ',' -> Some Read
	| '[' -> Some While
	| ']' -> Some Wend
	| _ -> None

let parseBrainfuck source =
	let notNone = function
		| None -> false
		| Some _ -> true
	and extractSome = function
		| Some x -> x
		| None -> failwith "Unexpected None value in extractSome function"
	in
	arrayDestructiveFilter notNone extractSome (stringMap charToBF source)
