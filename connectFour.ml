let rec create_board rows columns =
  let rec create_row = function
    | 0 -> []
    | n -> 0 :: create_row (n - 1)
  in
  let row = create_row columns in
  match rows with
  | 0 -> []
  | n -> row :: create_board (rows - 1) columns

let rec print_board board =
  let rec print_row = function
    | [] -> print_endline ""
    | h :: t ->
        let _ =
          if h == 1 then print_string "\027[91m\u{25CF} "
          else if h == 2 then print_string "\027[94m\u{25CF} "
          else print_string "\u{25CF} "
        in
        print_row t
  in
  match board with
  | [] -> print_string ""
  | h :: t ->
      let _ = print_row h in
      print_board t

let board = create_board 6 7
let _ = print_board board