module type Game_type = sig
  val create_board : int -> int -> int list list
  val print_board : int list list -> unit
  val start_game : int list list -> unit
end

module Game : Game_type = struct
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

  let rec place_piece board column piece counter =
    let rec place_piece_helper board column piece counter =
      let rec place_piece_in_column column_list piece_number =
        match column_list with
        | [] -> []
        | h :: t -> if h = 0 then [ piece ] @ t else h :: place_piece_in_column t piece
      in
      match board with
      | [] -> []
      | h :: t when column = counter ->
          let new_column = place_piece_in_column h piece in
          new_column :: place_piece_helper t column piece (counter + 1)
      | h :: t -> h :: place_piece_helper t column piece (counter + 1)
    in
    let _ =
      if Bool.not (is_valid_move board column) then
        raise (Invalid_argument "Cannot place a move in this column")
    in
    place_piece_helper board column piece counter

  let rec vertical_in_a_row column_list piece counter =
    if counter >= 4 then counter
    else
      match column_list with
      | [] -> counter
      | h :: t ->
          if h = piece then vertical_in_a_row t piece (counter + 1)
          else if h = 0 then counter
          else vertical_in_a_row t piece 0

  let rec print_list = function
    | [] -> ()
    | e :: l ->
        print_int e;
        print_string " ";
        print_list l

  let rec find_row column_list =
    match column_list with
    | [] -> -1
    | h :: t -> if h = 0 then -1 else 1 + find_row t

  let rec horizontal_in_a_row board column piece =
    let rec count board row piece counter =
      if counter >= 4 then counter
      else
        match board with
        | [] -> counter
        | h :: t ->
            if List.nth h row = piece then count t row piece (counter + 1) else count t row piece 0
    in
    let row = find_row (List.nth board column) in
    count board row piece 0

  let left_slant_diagonal_in_a_row board column piece =
    let rows = List.length (List.hd board) in
    let columns = List.length board in
    let rec count_up board row column piece counter =
      if counter >= 4 then counter
      else if row >= rows || column < 0 then counter
      else
        let current_piece = List.nth (List.nth board column) row in
        if current_piece = piece then count_up board (row + 1) (column - 1) piece (counter + 1)
        else counter
    in
    let rec count_down board row column piece counter =
      if counter >= 4 then counter
      else if row < 0 || column >= columns then counter
      else
        let current_piece = List.nth (List.nth board column) row in
        if current_piece = piece then count_down board (row - 1) (column + 1) piece (counter + 1)
        else counter
    in
    let row = find_row (List.nth board column) in
    count_up board row column piece 0 + count_down board row column piece 0 - 1

  let right_slant_diagonal_in_a_row board column piece =
    let rows = List.length (List.hd board) in
    let columns = List.length board in
    let rec count_up board row column piece counter =
      if counter >= 4 then counter
      else if row < 0 || column < 0 then counter
      else
        let current_piece = List.nth (List.nth board column) row in
        if current_piece = piece then count_up board (row - 1) (column - 1) piece (counter + 1)
        else counter
    in
    let rec count_down board row column piece counter =
      if counter >= 4 then counter
      else if row >= rows || column >= columns then counter
      else
        let current_piece = List.nth (List.nth board column) row in
        if current_piece = piece then count_down board (row + 1) (column + 1) piece (counter + 1)
        else counter
    in
    let row = find_row (List.nth board column) in
    count_up board row column piece 0 + count_down board row column piece 0 - 1

  let is_win board column piece =
    if vertical_in_a_row (List.nth board column) piece 0 >= 4 then true
    else if horizontal_in_a_row board column piece >= 4 then true
    else if left_slant_diagonal_in_a_row board column piece >= 4 then true
    else if right_slant_diagonal_in_a_row board column piece >= 4 then true
    else false

  let rec play_game board move_number =
    if move_number > List.length board * List.length (List.hd board) then
      let _ = print_board board in
      print_endline "Tie game."
    else
      let _ = print_board board in
      let player_number = (move_number mod 2) + 1 in
      let _ = print_string ("Player " ^ string_of_int player_number ^ ", please enter a move: ") in
      let user_input = read_int () in
      try
        let new_board = place_piece board (user_input - 1) player_number 0 in
        let _ = print_endline "" in
        if is_win new_board (user_input - 1) player_number then
          let _ = print_board new_board in
          print_endline ("Player " ^ string_of_int player_number ^ " wins!")
        else play_game new_board (move_number + 1)
      with Invalid_argument n ->
        let _ = print_endline "That is not a valid column to make a move. Please try again..." in
        play_game board move_number

  let start_game board = play_game board 0
end

module GameController = struct
  let start_game =
    let board = Game.create_board 6 7 in
    Game.start_game board
end

let _ = GameController.start_game