let player1_color = "\027[91m"
let player2_color = "\027[94m"
let empty_color = "\027[30m"
let default_color = "\027[0m"

let rec create_board rows columns =
  let rec create_column = function
    | 0 -> []
    | n -> 0 :: create_column (n - 1)
  in
  let column = create_column rows in
  match columns with
  | 0 -> []
  | n -> column :: create_board rows (columns - 1)

let is_valid_move board column =
  if column < 0 || column >= List.length board then false
  else
    let list = List.nth board column in
    List.nth list (List.length list - 1) = 0

let rec print_board_grid board rows =
  let rec print_row board row =
    match board with
    | [] -> print_endline ""
    | h :: t ->
        let _ =
          if List.nth h row == 1 then print_string (player1_color ^ "\u{25CF} ")
          else if List.nth h row == 2 then print_string (player2_color ^ "\u{25CF} ")
          else print_string "\027[30m\u{25CF} "
        in
        print_row t row
  in
  match rows with
  | -1 -> print_string "\027[0m"
  | n ->
      let _ = print_row board n in
      print_board_grid board (n - 1)

let rec print_numbers board columns = function
  | n when n = columns -> print_endline ""
  | n ->
      let _ =
        if is_valid_move board n then print_string (string_of_int (n + 1) ^ " ")
        else print_string "  "
      in
      print_numbers board columns (n + 1)

let print_board board =
  let _ = print_board_grid board (List.length (List.hd board) - 1) in
  print_numbers board (List.length board) 0

let rec place_piece_helper board column move_number counter =
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
      new_column :: place_piece_helper t column piece_number (counter + 1)
  | h :: t -> h :: place_piece_helper t column piece_number (counter + 1)

let rec place_piece board column move_number counter =
  let _ =
    if Bool.not (is_valid_move board column) then
      raise (Invalid_argument "Cannot place a move in this column")
  in
  place_piece_helper board column move_number counter

let rec play_game board move_number =
  if move_number > List.length board * List.length (List.hd board) then
    let _ = print_board board in
    print_endline "Tie game."
  else
    let _ = print_board board in
    let _ =
      print_string ("Player " ^ string_of_int ((move_number mod 2) + 1) ^ ", please enter a move: ")
    in
    let user_input = read_int () in
    try
      let new_board = place_piece board (user_input - 1) move_number 0 in
      let _ = print_endline "" in
      play_game new_board (move_number + 1)
    with Invalid_argument n ->
      let _ = print_endline "That is not a valid column to make a move. Please try again..." in
      play_game board move_number

let rec is_vertical_win_helper column_list piece counter =
  if counter >= 4 then counter
  else
    match column_list with
    | [] -> counter
    | h :: t ->
        if h = piece then is_vertical_win_helper t piece (counter + 1)
        else is_vertical_win_helper t piece 0

let is_vertical_win board column piece =
  let num_in_a_row = is_vertical_win_helper (List.nth board column) piece 0 in
  if num_in_a_row >= 4 then true else false

let is_win board column piece = if is_vertical_win board column piece then true else false
let board = create_board 6 7
let _ = play_game board 1