open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result =
  let (toks1, e1) = and_expr toks in
      match lookahead toks1 with
      |Tok_Or -> let toks2 = match_token toks1 Tok_Or in
                  let (toks3, e2) = parse_expr toks2 in
                    (toks3, Or(e1, e2))
      |_ -> (toks1, e1)
  and and_expr toks =
    let (toks1, e1) = equality_expr toks in
      match lookahead toks1 with
      |Tok_And -> let toks2 = match_token toks1 Tok_And in
                    let (toks3, e2) = and_expr toks2 in
                      (toks3, And(e1,e2))
      |_ -> (toks1, e1)
  and equality_expr toks =
    let (toks1, e1) = relational_expr toks in
      match lookahead toks1 with
      |Tok_Equal -> let toks2 = match_token toks1 Tok_Equal in
                      let (toks3, e2) = equality_expr toks2 in
                        (toks3, Equal(e1,e2))
      |Tok_NotEqual -> let toks2 = match_token toks1 Tok_NotEqual in
                      let (toks3, e2) = equality_expr toks2 in
                        (toks3, NotEqual(e1,e2))
      |_ -> (toks1, e1)
  and relational_expr toks =
    let (toks1, e1) = additive_expr toks in
      match lookahead toks1 with
      |Tok_Greater -> let toks2 = match_token toks1 Tok_Greater in
                        let (toks3, e2) = relational_expr toks2 in
                          (toks3, Greater(e1,e2))
      |Tok_Less -> let toks2 = match_token toks1 Tok_Less in
                        let (toks3, e2) = relational_expr toks2 in
                          (toks3, Less(e1,e2))
      |Tok_GreaterEqual ->let toks2 = match_token toks1 Tok_GreaterEqual in
                            let (toks3, e2) = relational_expr toks2 in
                              (toks3, GreaterEqual(e1,e2))
      |Tok_LessEqual -> let toks2 = match_token toks1 Tok_LessEqual in
                          let (toks3, e2) = relational_expr toks2 in
                            (toks3, LessEqual(e1,e2))
      |_ -> (toks1, e1)
  and additive_expr toks =
    let (toks1, e1) = multiplicative_expr toks in
      match lookahead toks1 with
      |Tok_Add -> let toks2 = match_token toks1 Tok_Add in
                      let (toks3, e2) = additive_expr toks2 in
                        (toks3, Add(e1,e2))
      |Tok_Sub -> let toks2 = match_token toks1 Tok_Sub in
                      let (toks3, e2) = additive_expr toks2 in
                        (toks3, Sub(e1,e2))
      |_ -> (toks1, e1)
  and multiplicative_expr toks =
    let (toks1, e1) = power_expr toks in
      match lookahead toks1 with
      |Tok_Mult -> let toks2 = match_token toks1 Tok_Mult in
                      let (toks3, e2) = multiplicative_expr toks2 in
                        (toks3, Mult(e1,e2))
      |Tok_Div -> let toks2 = match_token toks1 Tok_Div in
                      let (toks3, e2) = multiplicative_expr toks2 in
                        (toks3, Div(e1,e2))
      |_ -> (toks1, e1)
  and power_expr toks =
    let (toks1, e1) = unary_expr toks in
      match lookahead toks1 with
      |Tok_Pow -> let toks2 = match_token toks1 Tok_Pow in
                    let (toks3, e2) = power_expr toks2 in
                      (toks3, Pow(e1,e2))
      |_ -> (toks1, e1)
  and unary_expr toks =
    match lookahead toks with
    |Tok_Not -> let (toks1,e1) = unary_expr (match_token toks Tok_Not) in
                  (toks1, Not(e1))
    |_ -> primary_expr toks
  and primary_expr toks =
    match lookahead toks with
    |Tok_Int x -> let toks1 = match_token toks (Tok_Int x) in (toks1, Int x)
    |Tok_ID x -> let toks1 = match_token toks (Tok_ID x) in (toks1, ID x)
    |Tok_Bool x -> let toks1 = match_token toks (Tok_Bool x) in (toks1, Bool x)
    |Tok_LParen -> let toks1 = match_token toks Tok_LParen in
                      let (toks2, e1) = parse_expr toks1 in
                        let toks3 = match_token toks2 Tok_RParen in
                          (toks3, e1)
    |_ -> raise (InvalidInputException "Something goes wrong")

let rec parse_stmt toks : stmt_result =
  match lookahead toks with
    |EOF -> (toks, NoOp)
    |Tok_RBrace -> (toks, NoOp)
    |_ -> let (toks1, e1) = stmt_options toks in
            let (toks2, e2) = parse_stmt toks1 in 
              (toks2, Seq(e1, e2))
  and stmt_options toks =
    match lookahead toks with
    |EOF -> (toks, NoOp)
    |Tok_Int_Type -> let toks1 = match_token toks Tok_Int_Type in
                          let (toks2, e1) = (match lookahead toks1 with 
                                        |Tok_ID x -> let toks3 = match_token toks1 (Tok_ID x) in (toks3, x)
                                        |_ -> raise (InvalidInputException "Something goes wrong")) in
                              let toks4 = match_token toks2 Tok_Semi in
                                (toks4, Declare(Int_Type, e1))
    |Tok_Bool_Type -> let toks1 = match_token toks Tok_Bool_Type in
                          let (toks2, e1) = (match lookahead toks1 with 
                                        |Tok_ID x -> let toks3 = match_token toks1 (Tok_ID x) in (toks3, x)
                                        |_ -> raise (InvalidInputException "Something goes wrong")) in
                            let toks4 = match_token toks2 Tok_Semi in
                              (toks4, Declare(Bool_Type, e1))
    |Tok_ID x -> let toks1 = match_token toks (Tok_ID x) in
                    let toks2 = match_token toks1 Tok_Assign in
                        let (toks3, e1) = parse_expr toks2 in
                            let toks4 = match_token toks3 Tok_Semi in
                                (toks4, Assign(x, e1))
    |Tok_Print -> let toks1 = match_token toks Tok_Print in
                    let toks2 = match_token toks1 Tok_LParen in
                        let (toks3, e1) = parse_expr toks2 in
                            let toks4 = match_token toks3 Tok_RParen in
                                let toks5 = match_token toks4 Tok_Semi in
                                    (toks5, Print(e1))
    |Tok_If -> let toks1 = match_token toks Tok_If in
                    let toks2 = match_token toks1 Tok_LParen in
                        let (toks3, e1) = parse_expr toks2 in
                            let toks4 = match_token toks3 Tok_RParen in
                                let toks5 = match_token toks4 Tok_LBrace in
                                   let (toks6, e2) = parse_stmt toks5 in
                                      let toks7 = match_token toks6 Tok_RBrace in
                                         let (toks8, e3) = match lookahead toks7 with
                                                    |Tok_Else -> let toks8 = match_token toks7 Tok_Else in
                                                                    let toks9 = match_token toks8 Tok_LBrace in
                                                                      let (toks10,e4) = parse_stmt toks9 in
                                                                        let toks11 = match_token toks10 Tok_RBrace in (toks11, e4)
                                                    |_ -> (toks7, NoOp)
                                            in (toks8, If(e1,e2,e3))    
      |Tok_For -> let toks1 = match_token toks Tok_For in
                    let toks2 = match_token toks1 Tok_LParen in
                        let (toks3, e1) = match lookahead toks2 with 
                                        |Tok_ID x -> let toks4 = match_token toks2 (Tok_ID x) in toks4, x
                                        |_ -> raise (InvalidInputException "Something goes wrong") in
                          let toks5 = match_token toks3 Tok_From in
                            let (toks6, e2) = parse_expr toks5 in
                              let toks7 = match_token toks6 Tok_To in
                                let (toks8, e3) = parse_expr toks7 in
                                  let toks9 = match_token toks8 Tok_RParen in
                                    let toks10 = match_token toks9 Tok_LBrace in
                                      let (toks11, e4) = parse_stmt toks10 in
                                        let toks12 = match_token toks11 Tok_RBrace in
                                          (toks12, For(e1, e2, e3, e4))
      |Tok_While -> let toks1 = match_token toks Tok_While in
                      let toks2 = match_token toks1 Tok_LParen in
                          let (toks3, e1) = parse_expr toks2 in
                              let toks4 = match_token toks3 Tok_RParen in
                                  let toks5 = match_token toks4 Tok_LBrace in
                                     let (toks6, e2) = parse_stmt toks5 in
                                        let toks7 = match_token toks6 Tok_RBrace in
                                            (toks7, While(e1, e2))
      |Tok_RBrace -> (toks, NoOp)
      |_ -> raise (InvalidInputException "Something goes wrong")

let parse_main toks : stmt =
  let toks1 = match_token toks Tok_Int_Type in
    let toks2 = match_token toks1 Tok_Main in
      let toks3 = match_token toks2 Tok_LParen in
        let toks4 = match_token toks3 Tok_RParen in
          let toks5 = match_token toks4 Tok_LBrace in
            let (toks6, stmt) = parse_stmt toks5 in
              let remaining_toks = match_token toks6 Tok_RBrace in
                if remaining_toks <> [EOF] then
                  raise (InvalidInputException "Something goes wrong")
                else
                  stmt
