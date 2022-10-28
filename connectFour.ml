let rec create_board rows columns =
  let rec create_column = function
    | 0 -> []
    | n -> 0 :: create_column (n - 1)
  in
  let column = create_column rows in
  match columns with
  | 0 -> []
  | n -> column :: create_board rows (columns - 1)

let rec print_board_grid board row =
  let rec print_row board row =
    match board with
    | [] -> print_endline ""
    | h :: t ->
        let _ =
          if List.nth h row == 1 then print_string "\027[91m\u{25CF} "
          else if List.nth h row == 2 then print_string "\027[94m\u{25CF} "
          else print_string "\u{25CF} "
        in
        print_row t row
  in
  match row with
  | n when n = List.length (List.hd board) -> print_string ""
  | n ->
      let _ = print_row board 0 in
      print_board_grid board (n + 1)

let rec print_numbers columns = function
  | n when n = columns -> print_endline ""
  | n ->
      let _ = print_string (string_of_int n ^ " ") in
      print_numbers columns (n + 1)

let print_board board =
  let _ = print_board_grid board 0 in
  print_numbers (List.length board) 0

let board = create_board 6 7
let _ = print_board board