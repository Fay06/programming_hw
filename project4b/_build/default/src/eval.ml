open SmallCTypes
open EvalUtils
open TokenTypes


exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec eval_expr env t =
  match t with
  |Int i -> Int_Val i
  |Bool b -> Bool_Val b
  |ID i -> (let rec find_env env x = 
              match env with
              |[] -> raise (DeclareError ("Something goes wrong"))
              |(a,b)::t -> if a = x then b else find_env t x
            in find_env env i)
  |Add (e1, e2) -> (let n1 = eval_expr env e1 in
                      let n2 = eval_expr env e2 in
                        match (n1, n2) with
                        |(Int_Val x, Int_Val y) -> Int_Val (x + y)
                        |_ -> raise (TypeError("Something goes wrong")))
  |Sub (e1, e2) -> (let n1 = eval_expr env e1 in
                      let n2 = eval_expr env e2 in
                        match (n1, n2) with
                        |(Int_Val x, Int_Val y) -> Int_Val (x - y)
                        |_ -> raise (TypeError("Something goes wrong")))
  |Mult (e1, e2) -> (let n1 = eval_expr env e1 in
                      let n2 = eval_expr env e2 in
                        match (n1, n2) with
                        |(Int_Val x, Int_Val y) -> Int_Val (x * y)
                        |_ -> raise (TypeError("Something goes wrong")))
  |Div (e1, e2) -> (let n1 = eval_expr env e1 in
                      let n2 = eval_expr env e2 in
                        match (n1, n2) with
                        |(Int_Val x, Int_Val y) -> if y = 0 then raise DivByZeroError else Int_Val (x / y)
                        |_ -> raise (TypeError("Something goes wrong")))            
  |Pow (e1, e2) -> (let n1 = eval_expr env e1 in
                      let n2 = eval_expr env e2 in
                        match (n1, n2) with
                        |(Int_Val x, Int_Val y) -> Int_Val (int_of_float ((float_of_int x) ** (float_of_int y)))
                        |_ -> raise (TypeError("Something goes wrong")))
  |Or (e1, e2) -> (let n1 = eval_expr env e1 in
                    let n2 = eval_expr env e2 in
                      match (n1, n2) with 
                      |(Bool_Val x, Bool_Val y) -> Bool_Val (x || y)
                      |_ -> raise (TypeError("Something goes wrong")))
  |And (e1, e2) -> (let n1 = eval_expr env e1 in
                      let n2 = eval_expr env e2 in
                        match (n1, n2) with 
                        |(Bool_Val x, Bool_Val y) -> Bool_Val (x && y)
                        |_ -> raise (TypeError("Something goes wrong")))
  |Not e -> (let n = eval_expr env e in
                match n with
                |Bool_Val x -> Bool_Val (not x)
                |_ -> raise (TypeError("Something goes wrong")))
  |Greater (e1, e2) -> (let n1 = eval_expr env e1 in
                        let n2 = eval_expr env e2 in
                          match (n1, n2) with
                          |(Int_Val x, Int_Val y) -> Bool_Val (x > y)
                          |_ -> raise (TypeError("Something goes wrong")))
  |Less (e1, e2) -> (let n1 = eval_expr env e1 in
                      let n2 = eval_expr env e2 in
                        match (n1, n2) with
                        |(Int_Val x, Int_Val y) -> Bool_Val (x < y)
                        |_ -> raise (TypeError("Something goes wrong")))
  |GreaterEqual (e1, e2) -> (let n1 = eval_expr env e1 in
                              let n2 = eval_expr env e2 in
                                match (n1, n2) with
                                |(Int_Val x, Int_Val y) -> Bool_Val (x >= y)
                                |_ -> raise (TypeError("Something goes wrong")))
  |LessEqual (e1, e2) -> (let n1 = eval_expr env e1 in
                            let n2 = eval_expr env e2 in
                              match (n1, n2) with
                              |(Int_Val x, Int_Val y) -> Bool_Val (x <= y)
                              |_ -> raise (TypeError("Something goes wrong")))
  |Equal (e1, e2) -> (let n1 = eval_expr env e1 in
                        let n2 = eval_expr env e2 in
                          match (n1, n2) with 
                          |(Int_Val x, Int_Val y) -> Bool_Val(x = y)
                          |(Bool_Val x, Bool_Val y) -> Bool_Val(x = y)
                          |_ -> raise (TypeError("Something goes wrong")))
  |NotEqual (e1, e2) -> (let n1 = eval_expr env e1 in
                          let n2 = eval_expr env e2 in
                            match (n1, n2) with 
                            |(Int_Val x, Int_Val y) -> Bool_Val(x <> y)
                            |(Bool_Val x, Bool_Val y) -> Bool_Val(x <> y)
                            |_ -> raise (TypeError("Something goes wrong")))


let rec eval_stmt env s =
  match s with
  |NoOp -> env
  |Seq (e1, e2) -> (let e = eval_stmt env e1 in
                      eval_stmt e e2)
  |Declare (e1, e2) -> (if (List.exists (fun (a, b) -> a = e2) env) then 
                          raise (DeclareError ("Something goes wrong"))
                        else 
                          match e1 with
                            |Int_Type -> (e2, Int_Val(0)) :: env
                            |Bool_Type -> (e2, Bool_Val(false)) :: env)
  |Assign (e1, e2) -> (if (List.mem_assoc e1 env) then
                        match ((eval_expr env e2), (List.assoc e1 env)) with
                        |Int_Val x, Int_Val y -> 
                          (e1, eval_expr env e2) :: (List.remove_assq e1 env)
                        |Bool_Val x, Bool_Val y -> 
                          (e1, eval_expr env e2) :: (List.remove_assq e1 env)
                        |_ -> raise (TypeError("Something goes wrong"))
                      else
                        raise (DeclareError ("Something goes wrong")))
  |If (e1, e2, e3) -> (match (eval_expr env e1) with
                      |Bool_Val true -> eval_stmt env e2
                      |Bool_Val false -> eval_stmt env e3
                      |_ -> raise (TypeError("Something goes wrong")))
  |While (e1, e2) -> (match (eval_expr env e1) with
                      |Bool_Val true -> eval_stmt (eval_stmt env e2) s
                      |Bool_Val false -> env
                      |_ -> raise (TypeError("Something goes wrong")))
  |For (e1, e2, e3, e4) -> (match ((eval_expr env e2), (eval_expr env e3)) with
                              |Int_Val x, Int_Val y -> (
                                let new_env = eval_stmt env (Assign (e1, e2)) in
                                  eval_stmt new_env (While (And (LessEqual ((ID e1), e3), GreaterEqual ((ID e1), e2)), Seq (e4, Assign (e1, Add((ID e1), (Int 1)))))))
                              |_ -> raise (TypeError("Something goes wrong")))
  |Print e -> (match eval_expr env e with
                |Int_Val x -> print_output_int x; print_output_newline();env
                |Bool_Val x -> print_output_bool x; print_output_newline();env)
