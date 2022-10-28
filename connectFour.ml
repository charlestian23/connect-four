let rec create_board rows columns =
  let rec create_column = function
    | 0 -> []
    | n -> 0 :: create_column (n - 1)
  in
  let column = create_column rows in
  match columns with
  | 0 -> []
  | n -> column :: create_board rows (columns - 1)

let rec print_board_grid board rows =
  let rec print_row board row =
    match board with
    | [] -> print_endline ""
    | h :: t ->
        let _ =
          if List.nth h row == 1 then print_string "\027[91m\u{25CF} "
          else if List.nth h row == 2 then print_string "\027[94m\u{25CF} "
          else print_string "\027[30m\u{25CF} "
        in
        print_row t row
  in
  match rows with
  | -1 -> print_string "\027[0m"
  | n ->
      let _ = print_row board n in
      print_board_grid board (n - 1)

let rec print_numbers columns = function
  | n when n = columns -> print_endline ""
  | n ->
      let _ = print_string (string_of_int n ^ " ") in
      print_numbers columns (n + 1)

let print_board board =
  let _ = print_board_grid board (List.length (List.hd board) - 1) in
  print_numbers (List.length board) 0

(* Reference: https://stackoverflow.com/questions/37091784 ocaml-function-replace-a-element-in-a-list *)
let rec place_piece board column move_number counter =
  let piece_number = (move_number mod 2) + 1 in
  let rec place_piece_in_column column_list piece_number =
    match column_list with
    | [] -> []
    | h :: t -> if h = 0 then [ piece_number ] @ t else h :: place_piece_in_column t piece_number
  in
  match board with
  | [] -> []
  | h :: t when column = counter ->
      let new_column = place_piece_in_column h piece_number in
      new_column :: place_piece t column piece_number (counter + 1)
  | h :: t -> h :: place_piece t column piece_number (counter + 1)

let board = create_board 6 7
let _ = print_board board
let _ = print_endline ""
let board = place_piece board 3 1 0
let board = place_piece board 3 2 0
let board = place_piece board 3 3 0
let _ = print_board board