open Types
open Parse

let rec eval (e:expr) : int =
	match e with
	| Val     x      -> x
	| Plus   (e1,e2) -> (eval e1) + (eval e2)
	| Minus  (e1,e2) -> (eval e1) - (eval e2)
	| Times  (e1,e2) -> (eval e1) * (eval e2)