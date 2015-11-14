type expr =
	| Val   of int
	| Plus  of expr * expr
	| Minus of expr * expr
	| Times of expr * expr