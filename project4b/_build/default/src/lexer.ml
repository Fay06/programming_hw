open TokenTypes

let tokenize input =
  let length = String.length input in
  let rec helper pos =
    if pos >= length then 
      [EOF]
    else if (Str.string_match (Str.regexp "-?[0-9]+") input pos) then
      let matched_int = Str.matched_string input in
        Tok_Int (int_of_string matched_int) :: (helper (pos + (String.length matched_int)))
    else if (Str.string_match (Str.regexp "(") input pos) then 
      Tok_LParen :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp ")") input pos) then 
      Tok_RParen :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "{") input pos) then 
      Tok_LBrace :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "}") input pos) then 
      Tok_RBrace :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "==") input pos) then 
      Tok_Equal :: (helper (pos + 2))
    else if (Str.string_match (Str.regexp "!=") input pos) then 
      Tok_NotEqual :: (helper (pos + 2))
    else if (Str.string_match (Str.regexp "=") input pos) then 
      Tok_Assign :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp ">=") input pos) then 
      Tok_GreaterEqual :: (helper (pos + 2))
    else if (Str.string_match (Str.regexp "<=") input pos) then 
      Tok_LessEqual :: (helper (pos + 2))
    else if (Str.string_match (Str.regexp ">") input pos) then 
      Tok_Greater :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "<") input pos) then 
      Tok_Less :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "||") input pos) then 
      Tok_Or :: (helper (pos + 2))
    else if (Str.string_match (Str.regexp "&&") input pos) then 
      Tok_And :: (helper (pos + 2))
    else if (Str.string_match (Str.regexp "!") input pos) then 
      Tok_Not :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp ";") input pos) then 
      Tok_Semi :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "+") input pos) then 
      Tok_Add :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "-") input pos) then 
      Tok_Sub :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "*") input pos) then 
      Tok_Mult :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "/") input pos) then 
      Tok_Div :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "\\^") input pos) then 
      Tok_Pow :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos) then
      let matched_string = Str.matched_string input in
      let check_string str =
        if (str = "int") then 
          Tok_Int_Type :: (helper (pos + 3))
        else if (str = "bool") then 
          Tok_Bool_Type :: (helper (pos + 4))
        else if (str = "printf") then 
          Tok_Print :: (helper (pos + 6))
        else if (str = "main") then 
          Tok_Main :: (helper (pos + 4))
        else if (str = "if") then 
          Tok_If :: (helper (pos + 2))
        else if (str = "else") then 
          Tok_Else :: (helper (pos + 4))
        else if (str = "for") then 
          Tok_For :: (helper (pos + 3))
        else if (str = "from") then 
          Tok_From :: (helper (pos + 4))
        else if (str = "to") then 
          Tok_To :: (helper (pos + 2))
        else if (str = "while") then 
          Tok_While :: (helper (pos + 5))
        else if (str = "true") then
          Tok_Bool true :: (helper (pos + 4))
        else if (str = "false") then
          Tok_Bool false :: (helper (pos + 5))
        else
          Tok_ID str :: (helper (pos + (String.length str)))
      in 
      check_string matched_string
    else
      helper (pos + 1)
  in
  helper 0

