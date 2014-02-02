
type atom = A of atom * atom
	| K | K1 of atom
	| S | S1 of atom | S2 of atom * atom
	| I
	| Inc | N of int
	| L of atom Lazy.t

and 'a cons_t = Nil | Cons of 'a * 'a llist_t
and 'a llist_t = 'a cons_t Lazy.t

let rec repeat thing () =
	Cons (thing, Lazy.from_fun (repeat thing))

and lazy_input () =
	try
		let b = input_byte stdin in
		Cons (b, Lazy.from_fun lazy_input)
	with End_of_file -> Nil
(*in
	match lazy_input () with
	| Nil -> print_endline ":nil:"
	| Cons (b, rest) -> output_byte stdout b;
		begin match Lazy.force rest with
		| Nil -> print_endline ":nil:"
		| Cons (b1, _) -> output_byte stdout b1
		end*)

(* SII(SII(S(S(KS)K)I)) *)
let n256 = A (S2 (I, I), A (S2 (I, I), S2 (S2 (K1 S, K), I)))

and church_numbers = Array.make 256 I

let () = (* imperative in order to share previous numbers easily *)
	church_numbers.(0) <- K1 I; (* 1 is I *)
	for i = 2 to 255 do
		(* n = S(S(KS)K)[n-1] *)
		church_numbers.(i) <- S2 (S2 (K1 S, K), church_numbers.(i-1))
	done

let rec int_to_church i =
	try church_numbers.(i)
	with Invalid_argument _ -> n256

and church_input = function
	| Nil -> Lazy.from_fun (repeat n256)
	| Cons (i, rest) -> lazy (
		Cons (int_to_church i, church_input (Lazy.force rest))
	)

(* S(SI(K[x]))(K[xs]) *)
and atom_input = function
	| Nil -> K1 K (* end of list, should never happen *)
	| Cons (church, rest) -> L (lazy (
		S2 (S2 (I, K1 church), K1 (atom_input (Lazy.force rest)))
	))

let rec execute a0 =
	match a0 with
	| A (K, a) -> K1 a
	| A (K1 a, b) -> a
	| A (S, a) -> S1 a
	| A (S1 a, b) -> S2 (a, b)
	| A (S2 (a, b), c) -> execute (A (A (a, c), A (b, c)))
	| A (I, a) -> a
	| A (Inc, b) -> begin
		match execute b with
		| N i -> N (i + 1)
		| _ -> failwith "can only increment a number"
		end
	| A (N _, _) -> failwith "cannot apply a number"
	| A (a, b) -> execute (A (execute a, b))
	| L a -> execute (Lazy.force a)
	| a -> a

and test_number n target =
	match execute (A (A (n, Inc), N 0)) with
	| N i -> i = target
	| _ -> false

let () =
	for i = 0 to 256 do
		assert (test_number (int_to_church i) i)
	done

let rec iparse s = let rest = String.sub s 1 ((String.length s) - 1) in
	match s.[0] with
	| 's' -> (S, rest)
	| 'k' -> (K, rest)
	| 'i' -> (I, rest)
	| '`' -> let la, lrest = iparse rest in
	         let ra, rrest = iparse lrest in
	         (A (la, ra), rrest)
	| c -> failwith (Printf.sprintf "unexpected char %C" c)
and parse s = let a, rest = iparse s in
	assert (rest = "");
	a
;;

(* 3 *)
let test1 = parse "``s``s`ksk``s``s`kski"
(* (car (list-of 3)) *)
and test2 = parse "````sii``s`k``s`k`s``si`k``s``s`ksk``s``s`kskik``siik"
and runTest t = match execute (A (A (t, Inc), N 0)) with
	| N i -> print_endline (string_of_int i)
	| _ -> failwith "not a number"
in print_newline (); runTest test1; runTest test2
