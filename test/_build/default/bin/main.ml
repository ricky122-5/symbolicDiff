open Str

type expr =
  | Const of float
  | Var of string
  | Sum of expr * expr
  | Prod of expr * expr
  | Power of expr * float
  | Log of expr
  | Sin of expr
  | Cos of expr
  | Exp of expr

let rec to_string expr =
  match expr with
  | Const c -> string_of_float c
  | Var v -> v
  | Sum (e1, e2) -> "(" ^ (to_string e1) ^ " + " ^ (to_string e2) ^ ")"
  | Prod (e1, e2) -> "(" ^ (to_string e1) ^ " * " ^ (to_string e2) ^ ")"
  | Power (e, exp) -> "(" ^ (to_string e) ^ " ^ " ^ (string_of_float exp) ^ ")"
  | Log e -> "log(" ^ (to_string e) ^ ")"
  | Sin e -> "sin(" ^ (to_string e) ^ ")"
  | Cos e -> "cos(" ^ (to_string e) ^ ")"
  | Exp e -> "exp(" ^ (to_string e) ^ ")"

let rec differentiate expr var =
  Printf.printf "Differentiating: %s with respect to %s\n" (to_string expr) var;
  let result = 
    match expr with
    | Const _ -> Const 0.0
    | Var v -> if v = var then Const 1.0 else Const 0.0
    | Sum (e1, e2) -> Sum (differentiate e1 var, differentiate e2 var)
    | Prod (e1, e2) ->
        Sum (Prod (differentiate e1 var, e2), Prod (e1, differentiate e2 var))
    | Power (base, exp) ->
        Prod (Prod (Const exp, Power (base, exp -. 1.0)), differentiate base var)
    | Log e -> Prod (differentiate e var, Power (e, -1.0))
    | Sin e -> Prod (Cos e, differentiate e var)
    | Cos e -> Prod (Const (-1.0), Prod (Sin e, differentiate e var))
    | Exp e -> Prod (Exp e, differentiate e var)
  in
  Printf.printf "Result of differentiation: %s\n" (to_string result);
  result

let rec simplify expr =
  Printf.printf "Simplifying: %s\n" (to_string expr);
  let result = 
    match expr with
    | Sum (Const a, Const b) -> Const (a +. b)
    | Sum (Const 0.0, e) -> simplify e
    | Sum (e, Const 0.0) -> simplify e
    | Sum (e1, e2) -> 
        let se1 = simplify e1 in
        let se2 = simplify e2 in
        (match (se1, se2) with
         | (Const a, Const b) -> Const (a +. b)
         | _ -> Sum (se1, se2))
    | Prod (Const a, Const b) -> Const (a *. b)
    | Prod (Const 0.0, _) -> Const 0.0
    | Prod (_, Const 0.0) -> Const 0.0
    | Prod (Const 1.0, e) -> simplify e
    | Prod (e, Const 1.0) -> simplify e
    | Prod (e1, e2) -> 
        let se1 = simplify e1 in
        let se2 = simplify e2 in
        (match (se1, se2) with
         | (Const a, Const b) -> Const (a *. b)
         | _ -> Prod (se1, se2))
    | Power (e, exp) -> Power (simplify e, exp)
    | Log e -> Log (simplify e)
    | Sin e -> Sin (simplify e)
    | Cos e -> Cos (simplify e)
    | Exp e -> Exp (simplify e)
    | e -> e
  in
  Printf.printf "Result of simplification: %s\n" (to_string result);
  result

let parse_expr tokens =
  let rec parse_sum tokens =
    let e1, tokens = parse_prod tokens in
    match tokens with
    | "+" :: rest ->
        let e2, tokens = parse_sum rest in
        (Sum (e1, e2), tokens)
    | "-" :: rest ->
        let e2, tokens = parse_sum rest in
        (Sum (e1, Prod (Const (-1.0), e2)), tokens)
    | _ -> (e1, tokens)
  and parse_prod tokens =
    let e1, tokens = parse_power tokens in
    match tokens with
    | "*" :: rest ->
        let e2, tokens = parse_prod rest in
        (Prod (e1, e2), tokens)
    | "/" :: rest ->
        let e2, tokens = parse_prod rest in
        (Prod (e1, Power (e2, -1.0)), tokens)
    | _ -> (e1, tokens)
  and parse_power tokens =
    let e1, tokens = parse_factor tokens in
    match tokens with
    | "^" :: rest ->
        let e2, tokens = parse_factor rest in
        (match e2 with
         | Const c -> (Power (e1, c), tokens)
         | _ -> failwith "Exponent must be a constant")
    | _ -> (e1, tokens)
  and parse_factor tokens =
    match tokens with
    | [] -> failwith "Unexpected end of input"
    | "(" :: rest ->
        let e, tokens = parse_sum rest in
        (match tokens with
         | ")" :: rest -> (e, rest)
         | _ -> failwith "Expected closing parenthesis")
    | "log" :: "(" :: rest ->
        let e, tokens = parse_sum rest in
        (match tokens with
         | ")" :: rest -> (Log e, rest)
         | _ -> failwith "Expected closing parenthesis")
    | "sin" :: "(" :: rest ->
        let e, tokens = parse_sum rest in
        (match tokens with
         | ")" :: rest -> (Sin e, rest)
         | _ -> failwith "Expected closing parenthesis")
    | "cos" :: "(" :: rest ->
        let e, tokens = parse_sum rest in
        (match tokens with
         | ")" :: rest -> (Cos e, rest)
         | _ -> failwith "Expected closing parenthesis")
    | "exp" :: "(" :: rest ->
        let e, tokens = parse_sum rest in
        (match tokens with
         | ")" :: rest -> (Exp e, rest)
         | _ -> failwith "Expected closing parenthesis")
    | token :: rest ->
        if string_match (regexp "^[0-9]+\\(\\.[0-9]+\\)?$") token 0 then
          (Const (float_of_string token), rest)
        else if string_match (regexp "^[a-zA-Z]+$") token 0 then
          (Var token, rest)
        else
          failwith ("Unexpected token: " ^ token)
  in
  parse_sum tokens

let tokenize str =
  let re = regexp "\\([0-9]+\\(\\.[0-9]+\\)?\\|[a-zA-Z]+\\|[()+\\-*/^]\\)" in
  let rec aux pos =
    if pos >= String.length str then
      []
    else if string_match re str pos then
      let token = matched_string str in
      token :: aux (match_end ())
    else
      aux (pos + 1)
  in
  aux 0

let () =
  Printf.printf "Enter an expression: ";
  let expr_str = read_line () in
  Printf.printf "Tokenizing expression: %s\n" expr_str;
  let tokens = tokenize expr_str in
  Printf.printf "Tokens: %s\n" (String.concat " " tokens);
  Printf.printf "Enter the variable to differentiate with respect to: ";
  let var = read_line () in
  Printf.printf "Parsing expression...\n";
  let expr, _ = parse_expr tokens in
  Printf.printf "Parsed expression: %s\n" (to_string expr);
  Printf.printf "Differentiating expression...\n";
  let derivative = differentiate expr var in
  Printf.printf "Differentiated expression: %s\n" (to_string derivative);
  Printf.printf "Simplifying expression...\n";
  let simplified_derivative = simplify derivative in
  Printf.printf "Simplified derivative: %s\n" (to_string simplified_derivative);
  Printf.printf "Original expression: %s\n" (to_string expr);
  Printf.printf "Derivative: %s\n" (to_string simplified_derivative)
