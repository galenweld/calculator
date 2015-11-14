open Bytes
open Expr
open Eval


let rec text_to_char_list t =
	match t with
	| "" -> []
	| _  -> (get t 0)::(text_to_char_list (sub t 1 ((length t) -1)))


type token = Int of int | Plus | Minus | Times | OParen | CParen


let char_list_to_token_list (s : char list) : token list =
	let rec fold_helper l c = 
		match c with
		| ' ' -> l@ []
		| '(' -> l@ [OParen]
		| ')' -> l@ [CParen]
		| '+' -> l@ [Plus]
		| '-' -> l@ [Minus]
		| '*' -> l@ [Times]
		|  x  -> l@ [Int (int_of_string (Bytes.make 1 x))]
	in List.fold_left fold_helper [] s


let parse (s:bytes) : expr =
	blah