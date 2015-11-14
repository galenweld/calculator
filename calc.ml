type expr =
	| Val   of int
	| Plus  of expr * expr
	| Minus of expr * expr
	| Times of expr * expr

type token = Int of int | Plus | Minus | Times | OParen | CParen

type token_stream = token list

let lex (input : bytes) : token_stream = 
	let rec text_to_char_list (t : bytes) : char list =
		match t with
		| "" -> []
		| _  -> (Bytes.get t 0)::(text_to_char_list (sub t 1 ((Bytes.length t) -1)))
		in


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
		in


	let rec fix_ints (s : token list) : token list =
		match s with
		|[] -> []
		|Int a::Int b::tl -> fix_ints ((Int (10*a+b))::tl)
		|hd::tl -> hd::(fix_ints tl)
		in
fix_ints (char_list_to_token_list (text_to_char_list input))


let has_sub_expression (s : token_stream) : bool =
	List.exists (fun x -> x = OParen) s
	&&
	List.exists (fun x -> x = CParen) s


let sub_expression (s : token_stream) : token_stream =
	if not (has_sub_expression s) then s else
		failwith "this is the hard bit"



let rec parse (s : token_stream) : expr =
	match s with
	|[] -> failwith "no expression"
	|[Int x] -> Val x


let rec eval (e:expr) : int =
	match e with
	| Val     x      -> x
	| Plus   (e1,e2) -> (eval e1) + (eval e2)
	| Minus  (e1,e2) -> (eval e1) - (eval e2)
	| Times  (e1,e2) -> (eval e1) * (eval e2)