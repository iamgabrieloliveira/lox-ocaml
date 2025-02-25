let str = "let a = 10"

(* todo: do it better *)
let is_numeric char = Option.is_some @@ int_of_string_opt char

type token_kind =
  | T_left_paren
  | T_right_paren
  | T_equal
  | T_string of string
  | T_int of int

type state = { input : string; rest : string; pos : int }
        
and read_number state = (state, T_int 1)
and read_string state = (state, T_string "")

and advance state steps =
  {
    input = state.input;
    rest = Str.string_after state.rest steps;
    pos = state.pos + steps;
  }

and read_char state =
  let char = Str.string_before state.rest 1 in

  (char, advance state 1)

let rec tokenize state =
  if state.pos > String.length state.input then state
  else
    let c, state = read_char state in

    let () = print_endline c in

    let state, _ =
      match c with
      | "=" -> (state, T_equal)
      | "(" -> (state, T_left_paren)
      | ")" -> (state, T_right_paren)
      | c when is_numeric c -> read_number state
      | _ -> read_string state
    in

    tokenize state

let () = let _ = tokenize { input = str; rest = str; pos = 1 } in

         ()
