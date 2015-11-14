open Bytes
open Expr
open Eval


let text_to_word_list (s : bytes) : bytes list =
	let rec text_to_word_list_helper t = 
		if contains t ' ' then
			let hd = sub t 0 (index t ' ') in
			let tl = sub t ((index t ' ') +1) ((length t) - ((index t ' ') +1)) in
			if hd = "" then text_to_word_list_helper tl else
				hd::(text_to_word_list_helper tl)
		else [t]
	in text_to_word_list_helper (trim s)


let parse (s:bytes) : expr =
	blah