let code = "(); abc abc ; 21312"

type token =
  | Semicolon
  | LeftParen
  | RightParen
  | Identifier of string
  | Integer of int

type lexer = { input : string; ch : char option; position : int }

let print_opt = function
  | Some value -> print_endline value
  | None -> print_endline "None"

let print_token = function
  | Semicolon -> print_endline "Semicolon"
  | LeftParen -> print_endline "LeftParen"
  | RightParen -> print_endline "RightParen"
  | Identifier i -> Printf.printf "Identifier: %s\n" i
  | Integer i -> Printf.printf "Integer: %d\n" i

let rec skip_whitespaces lexer = read_while lexer is_whitespace

and seek lexer condition =
  let rec loop lexer =
    match lexer.ch with
    | Some ch when condition ch -> loop (advance lexer)
    | _ -> lexer
  in

  loop lexer

and read_while lexer predicate =
  let pos_start = lexer.position in

  let lexer = seek lexer predicate in

  let pos_end = lexer.position + 1 in

  let slice = String.sub lexer.input pos_start (pos_end - pos_start) in

  (lexer, slice)

and is_whitespace ch = ch = ' ' || ch = '\n' || ch = '\t'

and is_alpha ch =
  match ch with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

and is_identifier ch = is_alpha ch
and is_number ch = Option.is_some @@ int_of_string_opt @@ Char.escaped ch

and read_identifier lexer =
  let lexer, slice = read_while lexer is_identifier in
  (lexer, Identifier slice)

and read_number lexer =
  let lexer, slice = read_while lexer is_number in

  (* convering might raise an error *)
  let number = int_of_string slice in

  (lexer, Integer number)

and advance lexer =
  if lexer.position >= String.length lexer.input - 1 then
    { lexer with ch = None }
  else
    let position = lexer.position + 1 in
    { lexer with ch = Some (String.get lexer.input position); position }

let next_token lexer =
  let lexer, _ = skip_whitespaces lexer in

  match lexer.ch with
  | None -> (advance lexer, None)
  | Some ch ->
      let lexer, token =
        match ch with
        | ';' -> (advance lexer, Semicolon)
        | '(' -> (advance lexer, LeftParen)
        | ')' -> (advance lexer, RightParen)
        | ch when is_identifier ch -> read_identifier lexer
        | ch when is_number ch -> read_number lexer
        | ch -> failwith @@ Printf.sprintf "Unexpected character: %c" ch
      in

      (lexer, Some token)

let init input =
  if String.length input > 0 then
    { input; ch = Some (String.get input 0); position = 0 }
  else { input; ch = None; position = 0 }

let rec run_lexer lexer =
  let rec loop lexer tokens =
    let lexer, token = next_token lexer in

    match token with
    | Some token -> loop lexer (token :: tokens)
    | None -> tokens
  in

  loop lexer []

let () =
  let tokens = init code |> run_lexer in

  List.iter print_token @@ List.rev tokens
