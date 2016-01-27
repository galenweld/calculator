type expr =
  | Val   of int
  | Plus  of expr * expr
  | Minus of expr * expr
  | Times of expr * expr

type token = Int of int | Plus | Minus | Times | OParen | CParen

type token_stream = token list

let lex (input:bytes) : token_stream =
  let rec text_to_char_list (t : bytes) : char list =
    match t with
    | "" -> []
    | _  -> (Bytes.get t 0)::(text_to_char_list (Bytes.sub t 1
                                                   ((Bytes.length t) - 1)))
  in


  let char_list_to_token_list (s:char list) : token list =
    let rec fold_helper l c =
      match c with
      | ' ' -> l
      | '(' -> l @ [OParen]
      | ')' -> l @ [CParen]
      | '+' -> l @ [Plus]
      | '-' -> l @ [Minus]
      | '*' -> l @ [Times]
      |  x  -> l @ [Int (int_of_string (Bytes.make 1 x))]
    in List.fold_left fold_helper [] s
  in


  let rec fix_ints (s:token list) : token list =
    match s with
    | [] -> []
    | Int a::Int b::tl -> fix_ints ((Int (10 * a + b))::tl)
    | hd::tl -> hd::(fix_ints tl)
  in
  fix_ints (char_list_to_token_list (text_to_char_list input))


let rec parse (s:token_stream) : expr =
  let (expr, s') = parse_expr s in
  if s' = [] then expr else failwith "Invalid token stream."

and parse_expr (s:token_stream) : (expr * token_stream) =
  let (term, s') = parse_term s in

  let rec termer (t:expr) (s':token_stream) : (expr * token_stream) =
    match s' with
    | Plus::tl ->
      begin
        let (t', tl') = parse_term tl in
        termer (Plus (t, t')) tl'
      end
    | Minus::tl ->
      begin
        let (t', tl') = parse_term tl in
        termer (Minus (t, t')) tl'
      end
    | _ -> (t, s')
  in

  termer term s'

and parse_term (s:token_stream) : (expr * token_stream) =
  let (factor, s') = parse_factor s in

  let rec factorer (f:expr) (s':token_stream) : (expr * token_stream) =
    match s' with
    | Times::tl ->
      begin
        let (f', tl') = parse_factor tl in
        factorer (Times (f, f')) tl'
      end
    | _ -> (f, s')
  in

  factorer factor s'

and parse_factor (s:token_stream) : (expr * token_stream) =
  match s with
  | Int n::tl -> (Val n, tl)
  | OParen::tl ->
    begin
      let (expr, s') = parse_expr s in
      match s' with
      | CParen::tl' -> (expr, tl')
      | _ -> failwith "Missing )."
    end
  | _ -> failwith "Not a factor."


let rec eval (e:expr) : int =
  match e with
  | Val x -> x
  | Plus (e1,e2) -> (eval e1) + (eval e2)
  | Minus (e1,e2) -> (eval e1) - (eval e2)
  | Times (e1,e2) -> (eval e1) * (eval e2)


let calc (input:bytes) : int =
  let tokens = lex input in
  let expr = parse tokens in
  eval expr

