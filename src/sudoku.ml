(* Implementation of http://norvig.com/sudoku.html. See this page for the definition of the terms used here *)
open Core.Std
open Batteries


exception Invalid_grid of string

(***********************************************************************************************************************
 Useful definitions
***********************************************************************************************************************)
(* Cartesian product *)
let cross (a: 'a list) (b : 'b list): ('a * 'b) list = List.flatten (List.map (fun ia -> List.map (fun ib -> (ia, ib)) b) a)

(* Valid rows *)
let rows = ['a';'b';'c';'d';'e';'f';'g';'h';'i']
(* Valid column *)
let cols = [1;2;3;4;5;6;7;8;9]

(* All valid squares *)
let squares = cross rows cols

(* Boxes *)
let group_row = [['a';'b';'c'] ; ['d';'e';'f'] ; ['g';'h';'i']]
let group_col = [[1;2;3] ; [4;5;6] ; [7;8;9]]

(* All units *)
let unitlist =
  List.map (fun el -> cross rows [el]) cols @ (* All columns *)
  List.map (fun el -> cross [el] cols) rows @ (* All rows *)
  List.flatten (List.map (fun e1 -> List.map (fun e2 -> cross e1 e2) group_col) group_row) (* All boxes *)

(* all the units a givn square belong to *)
let units (coord: char * int): (char * int) list list =
  List.filter (fun el -> match List.index_of coord el with None -> false | Some _ -> true) unitlist

(* All the peers of a given square *)
let peers (coord: char * int): (char * int) list =
  let this_units = units coord in
  List.unique (List.filter (fun el -> el <> coord) (List.flatten this_units))

(* Values of a square *)
type square_value = int list

(***********************************************************************************************************************
 Helper functions
***********************************************************************************************************************)
let is_solved arr: bool =
  let array_all = (fun fn arr -> Array.fold_left (fun acc e -> acc && (fn e)) true arr) in
  array_all (fun e -> (List.length e) = 1) arr (* Each square has only one value: problem solved *)

let rec list_first_that_returns (fn: 'a -> 'b option) (l: 'a list): 'b option = match l with
  | [] -> None
  | hd::tl -> let t = (fn hd) in match t with
    | None -> list_first_that_returns fn tl
    | Some x -> Some x

let sq_value_to_string (sq: int list): string = List.fold_left (fun acc s -> acc ^ (string_of_int s)) "" sq

let is_in (i:int) (sq: square_value) = let x = List.index_of i sq in match x with None -> false | _ -> true

(* Coordinate to Grid-as-array index*)
let coord_to_index (coord: char * int): int =
  let row, col = coord in
  let row = match row with
  | 'a' -> 0 | 'b' -> 1 | 'c' -> 2 | 'd' -> 3 | 'e' -> 4 | 'f' -> 5 | 'g' -> 6 | 'h' -> 7 | 'i' -> 8
  | _ -> raise (Invalid_argument "Index out of bound") in
  let col = col - 1 in
  row * 9 + col

(* Index to coordinate *)
let index_to_coord (index:int): (char * int) =
  let row = index / 9 in
  let row = match row with
  | 0 -> 'a' | 1 -> 'b' | 2 -> 'c' | 3 -> 'd' | 4 -> 'e' | 5 -> 'f' | 6 -> 'g' | 7 -> 'h' | 8 -> 'i'
  | _ -> raise (Invalid_argument "Index out of bound") in
  let col = (index mod 9) + 1 in
  (row, col)

(* Read the puzzle and generate the grid *)
let grid_values (grid:string): int option array =
  let valid_symbols = "1234567890." in
  let filtered = String.filter (fun el -> String.contains valid_symbols el) grid in
  let _ =
    if (String.length filtered) <> 81
    then raise (Invalid_grid "Grid size must be 81")
    else ()
  in
  let g = List.map (fun el ->
    if el = '.' || el = '0'
    then None
    else Some (int_of_string (BatString.of_char el))
  ) (String.to_list filtered) in
  Array.of_list g

(* Add space at the end of a string to make it of size i *)
let rec pad str i c = if (String.length str) = i then str else pad (str ^ c) i c
let display_grid arr: unit =
  let width_column = Array.fold_left (fun acc e1 -> if acc >= List.length e1 then acc else List.length e1) 0 arr in
  let arr' = Array.map (fun s -> sq_value_to_string s) arr in
  let () = Array.iteri (fun i _ ->
    if (i mod (9*3)) = 0 then print_string ("\n" ^ (pad "-" ((width_column * 9) + 2) "-") ^ "\n")
    else if (i mod 9) = 0 then print_string "\n"
    else if (i mod 3) = 0 then print_string "|" else ();
    print_string (pad arr'.(i) width_column " ")
  ) arr' in
  print_string "\n"

(* Square containing more than one value are undecided: retrieve the undecided square with the least number of values *)
let get_smallest_indecision_square arr: int option =
  let arr' = Array.mapi (fun i e -> (i, List.length e)) arr in
  let (is, _) = Array.fold_left (fun acc e ->
    let sq, len = acc in (* int option * int *)
    let sq',len' = e in (* int * int *)
    match sq with
    | None -> if (len' > 1) then (Some sq', len') else acc
    | Some x -> if (len' > 1) && (len' < len) then (Some sq', len') else acc
  ) (None, 0) arr' in
  is

(***********************************************************************************************************************
  Problem solving starts here
***********************************************************************************************************************)
let rec eliminate (i:int) (d: int) (arr: square_value array): unit =
  if not (is_in d arr.(i))
  then () (* Nothing to eliminate *)
  else
    let () = arr.(i) <- List.filter (fun el -> el <> d) arr.(i) in (* Leave all values but the one to eliminate *)
    let vals = arr.(i) in match vals with
    (* No more possible values something went wrong *)
    | [] -> raise (Invalid_grid "Eliminated last possible value for a square: grid can not be solved")
    (* One value left, awesome, let's eliminate it from its peers *)
    | d2 :: [] -> peers (index_to_coord i)  (* List of coord, all the value that must differ from the previous one *)
                  |>
                  List.map coord_to_index (* same, as index in the grid-as-array *)
                  |>
                  List.iter (fun s2 -> eliminate s2 d2 arr) (* (try to) eliminate the value d2 from all the peers *)
    (* Iterate over the values left in the unit, see if we can solve a bit more *)
    | _ -> let unts = units (index_to_coord i) in (* List of all the units (as list of square), indexed  *)
           List.iter (fun unt ->
             let dplaces = List.filter (fun u -> is_in d arr.(coord_to_index u)) unt in (* [s for s in u if d in values[s]] *)
             let () = match dplaces with
             | [] -> raise (Invalid_grid "No possible value left for a square in a unit, giving up")
             | dp :: [] -> assign (coord_to_index dp) d arr
             | _ -> ()
             in ()
           ) unts
and assign (i:int) (d: int) (arr: square_value array): unit =
  let other_values = List.filter (fun el -> el <> d) arr.(i) in
  List.iter (fun d2 ->
    eliminate i d2 arr
  ) other_values

let parse_grid (grid:string): int list array =
  let res = Array.create 81 [1;2;3;4;5;6;7;8;9] in (* At the beginning, each square can be of any value *)
  let grid = grid_values grid in
  let assign = (fun i d -> assign i d res) in (* Update res *)
  let _ = Array.iteri (fun i d -> match d with
    | None -> ()
    | Some x -> assign i x
  ) grid in
  res

let rec search arr: int list array =
  if is_solved arr then arr
  else
    (* Get the square with the least number of indecided values, pick the first one and see what happens *)
    let is = get_smallest_indecision_square arr in
    let is = match is with
      | None -> raise (Invalid_grid "It shouldn't happen") (* if it happens, it means there are no undecided square
                                                              left, meaning the problem is solved, meaning we exited
                                                              earlier *)
      | Some x -> x
    in
    let choices = arr.(is) in (* The value we could assign to this square, we'll test them all *)
    let response = list_first_that_returns (fun v -> (* Stops when an valid answer has been found *)
      let arr_copy = Array.copy arr in (* Copy the array *)
      try
        let () = assign is v arr_copy in (* Try the assign/eliminate dance (raises an exception if a value is invalid) *)
        let res = search arr_copy in (* Move to the next value to pivot *)
        Some res
      with
        Invalid_grid _ -> None
    ) choices in
    match response with
    | None -> raise (Invalid_grid "This grid admits no solution")
    | Some x -> x

let solve grid = search (parse_grid grid)

let () =
  let hardest = [
  "85...24..72......9..4.........1.7..23.5...9...4...........8..7..17..........36.4." ;
  "..53.....8......2..7..1.5..4....53...1..7...6..32...8..6.5....9..4....3......97.." ;
  "12..4......5.69.1...9...5.........7.7...52.9..3......2.9.6...5.4..9..8.1..3...9.4" ;
  "...57..3.1......2.7...234......8...4..7..4...49....6.5.42...3.....7..9....18....." ;
  "7..1523........92....3.....1....47.8.......6............9...5.6.4.9.7...8....6.1." ;
  "1....7.9..3..2...8..96..5....53..9...1..8...26....4...3......1..4......7..7...3.." ;
  "1...34.8....8..5....4.6..21.18......3..1.2..6......81.52..7.9....6..9....9.64...2" ;
  "...92......68.3...19..7...623..4.1....1...7....8.3..297...8..91...5.72......64..." ;
  ".6.5.4.3.1...9...8.........9...5...6.4.6.2.7.7...4...5.........4...8...1.5.2.3.4." ;
  "7.....4...2..7..8...3..8.799..5..3...6..2..9...1.97..6...3..9...3..4..6...9..1.35" ;
  "....7..2.8.......6.1.2.5...9.54....8.........3....85.1...3.2.8.4.......9.7..6...." ;
  ] in
  let top95 = [
  "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......" ;
  "52...6.........7.13...........4..8..6......5...........418.........3..2...87....." ;
  "6.....8.3.4.7.................5.4.7.3..2.....1.6.......2.....5.....8.6......1...." ;
  "48.3............71.2.......7.5....6....2..8.............1.76...3.....4......5...." ;
  "....14....3....2...7..........9...3.6.1.............8.2.....1.4....5.6.....7.8..." ;
  "......52..8.4......3...9...5.1...6..2..7........3.....6...1..........7.4.......3." ;
  "6.2.5.........3.4..........43...8....1....2........7..5..27...........81...6....." ;
  ".524.........7.1..............8.2...3.....6...9.5.....1.6.3...........897........" ;
  "6.2.5.........4.3..........43...8....1....2........7..5..27...........81...6....." ;
  ".923.........8.1...........1.7.4...........658.........6.5.2...4.....7.....9....." ;
  "6..3.2....5.....1..........7.26............543.........8.15........4.2........7.." ;
  ".6.5.1.9.1...9..539....7....4.8...7.......5.8.817.5.3.....5.2............76..8..." ;
  "..5...987.4..5...1..7......2...48....9.1.....6..2.....3..6..2.......9.7.......5.." ;
  "3.6.7...........518.........1.4.5...7.....6.....2......2.....4.....8.3.....5....." ;
  "1.....3.8.7.4..............2.3.1...........958.........5.6...7.....8.2...4......." ;
  "6..3.2....4.....1..........7.26............543.........8.15........4.2........7.." ;
  "....3..9....2....1.5.9..............1.2.8.4.6.8.5...2..75......4.1..6..3.....4.6." ;
  "45.....3....8.1....9...........5..9.2..7.....8.........1..4..........7.2...6..8.." ;
  ".237....68...6.59.9.....7......4.97.3.7.96..2.........5..47.........2....8......." ;
  "..84...3....3.....9....157479...8........7..514.....2...9.6...2.5....4......9..56" ;
  ".98.1....2......6.............3.2.5..84.........6.........4.8.93..5...........1.." ;
  "..247..58..............1.4.....2...9528.9.4....9...1.........3.3....75..685..2..." ;
  "4.....8.5.3..........7......2.....6.....5.4......1.......6.3.7.5..2.....1.9......" ;
  ".2.3......63.....58.......15....9.3....7........1....8.879..26......6.7...6..7..4" ;
  "1.....7.9.4...72..8.........7..1..6.3.......5.6..4..2.........8..53...7.7.2....46" ;
  "4.....3.....8.2......7........1...8734.......6........5...6........1.4...82......" ;
  ".......71.2.8........4.3...7...6..5....2..3..9........6...7.....8....4......5...." ;
  "6..3.2....4.....8..........7.26............543.........8.15........8.2........7.." ;
  ".47.8...1............6..7..6....357......5....1..6....28..4.....9.1...4.....2.69." ;
  "......8.17..2........5.6......7...5..1....3...8.......5......2..4..8....6...3...." ;
  "38.6.......9.......2..3.51......5....3..1..6....4......17.5..8.......9.......7.32" ;
  "...5...........5.697.....2...48.2...25.1...3..8..3.........4.7..13.5..9..2...31.." ;
  ".2.......3.5.62..9.68...3...5..........64.8.2..47..9....3.....1.....6...17.43...." ;
  ".8..4....3......1........2...5...4.69..1..8..2...........3.9....6....5.....2....." ;
  "..8.9.1...6.5...2......6....3.1.7.5.........9..4...3...5....2...7...3.8.2..7....4" ;
  "4.....5.8.3..........7......2.....6.....5.8......1.......6.3.7.5..2.....1.8......" ;
  "1.....3.8.6.4..............2.3.1...........958.........5.6...7.....8.2...4......." ;
  "1....6.8..64..........4...7....9.6...7.4..5..5...7.1...5....32.3....8...4........" ;
  "249.6...3.3....2..8.......5.....6......2......1..4.82..9.5..7....4.....1.7...3..." ;
  "...8....9.873...4.6..7.......85..97...........43..75.......3....3...145.4....2..1" ;
  "...5.1....9....8...6.......4.1..........7..9........3.8.....1.5...2..4.....36...." ;
  "......8.16..2........7.5......6...2..1....3...8.......2......7..3..8....5...4...." ;
  ".476...5.8.3.....2.....9......8.5..6...1.....6.24......78...51...6....4..9...4..7" ;
  ".....7.95.....1...86..2.....2..73..85......6...3..49..3.5...41724................" ;
  ".4.5.....8...9..3..76.2.....146..........9..7.....36....1..4.5..6......3..71..2.." ;
  ".834.........7..5...........4.1.8..........27...3.....2.6.5....5.....8........1.." ;
  "..9.....3.....9...7.....5.6..65..4.....3......28......3..75.6..6...........12.3.8" ;
  ".26.39......6....19.....7.......4..9.5....2....85.....3..2..9..4....762.........4" ;
  "2.3.8....8..7...........1...6.5.7...4......3....1............82.5....6...1......." ;
  "6..3.2....1.....5..........7.26............843.........8.15........8.2........7.." ;
  "1.....9...64..1.7..7..4.......3.....3.89..5....7....2.....6.7.9.....4.1....129.3." ;
  ".........9......84.623...5....6...453...1...6...9...7....1.....4.5..2....3.8....9" ;
  ".2....5938..5..46.94..6...8..2.3.....6..8.73.7..2.........4.38..7....6..........5" ;
  "9.4..5...25.6..1..31......8.7...9...4..26......147....7.......2...3..8.6.4.....9." ;
  "...52.....9...3..4......7...1.....4..8..453..6...1...87.2........8....32.4..8..1." ;
  "53..2.9...24.3..5...9..........1.827...7.........981.............64....91.2.5.43." ;
  "1....786...7..8.1.8..2....9........24...1......9..5...6.8..........5.9.......93.4" ;
  "....5...11......7..6.....8......4.....9.1.3.....596.2..8..62..7..7......3.5.7.2.." ;
  ".47.2....8....1....3....9.2.....5...6..81..5.....4.....7....3.4...9...1.4..27.8.." ;
  "......94.....9...53....5.7..8.4..1..463...........7.8.8..7.....7......28.5.26...." ;
  ".2......6....41.....78....1......7....37.....6..412....1..74..5..8.5..7......39.." ;
  "1.....3.8.6.4..............2.3.1...........758.........7.5...6.....8.2...4......." ;
  "2....1.9..1..3.7..9..8...2.......85..6.4.........7...3.2.3...6....5.....1.9...2.5" ;
  "..7..8.....6.2.3...3......9.1..5..6.....1.....7.9....2........4.83..4...26....51." ;
  "...36....85.......9.4..8........68.........17..9..45...1.5...6.4....9..2.....3..." ;
  "34.6.......7.......2..8.57......5....7..1..2....4......36.2..1.......9.......7.82" ;
  "......4.18..2........6.7......8...6..4....3...1.......6......2..5..1....7...3...." ;
  ".4..5..67...1...4....2.....1..8..3........2...6...........4..5.3.....8..2........" ;
  ".......4...2..4..1.7..5..9...3..7....4..6....6..1..8...2....1..85.9...6.....8...3" ;
  "8..7....4.5....6............3.97...8....43..5....2.9....6......2...6...7.71..83.2" ;
  ".8...4.5....7..3............1..85...6.....2......4....3.26............417........" ;
  "....7..8...6...5...2...3.61.1...7..2..8..534.2..9.......2......58...6.3.4...1...." ;
  "......8.16..2........7.5......6...2..1....3...8.......2......7..4..8....5...3...." ;
  ".2..........6....3.74.8.........3..2.8..4..1.6..5.........1.78.5....9..........4." ;
  ".52..68.......7.2.......6....48..9..2..41......1.....8..61..38.....9...63..6..1.9" ;
  "....1.78.5....9..........4..2..........6....3.74.8.........3..2.8..4..1.6..5....." ;
  "1.......3.6.3..7...7...5..121.7...9...7........8.1..2....8.64....9.2..6....4....." ;
  "4...7.1....19.46.5.....1......7....2..2.3....847..6....14...8.6.2....3..6...9...." ;
  "......8.17..2........5.6......7...5..1....3...8.......5......2..3..8....6...4...." ;
  "963......1....8......2.5....4.8......1....7......3..257......3...9.2.4.7......9.." ;
  "15.3......7..4.2....4.72.....8.........9..1.8.1..8.79......38...........6....7423" ;
  "..........5724...98....947...9..3...5..9..12...3.1.9...6....25....56.....7......6" ;
  "....75....1..2.....4...3...5.....3.2...8...1.......6.....1..48.2........7........" ;
  "6.....7.3.4.8.................5.4.8.7..2.....1.3.......2.....5.....7.9......1...." ;
  "....6...4..6.3....1..4..5.77.....8.5...8.....6.8....9...2.9....4....32....97..1.." ;
  ".32.....58..3.....9.428...1...4...39...6...5.....1.....2...67.8.....4....95....6." ;
  "...5.3.......6.7..5.8....1636..2.......4.1.......3...567....2.8..4.7.......2..5.." ;
  ".5.3.7.4.1.........3.......5.8.3.61....8..5.9.6..1........4...6...6927....2...9.." ;
  "..5..8..18......9.......78....4.....64....9......53..2.6.........138..5....9.714." ;
  "..........72.6.1....51...82.8...13..4.........37.9..1.....238..5.4..9.........79." ;
  "...658.....4......12............96.7...3..5....2.8...3..19..8..3.6.....4....473.." ;
  ".2.3.......6..8.9.83.5........2...8.7.9..5........6..4.......1...1...4.22..7..8.9" ;
  ".5..9....1.....6.....3.8.....8.4...9514.......3....2..........4.8...6..77..15..6." ;
  ".....2.......7...17..3...9.8..7......2.89.6...13..6....9..5.824.....891.........." ;
  "3...8.......7....51..............36...2..4....7...........6.13..452...........8.." ;
  ] in
  let all_problems = top95 @ hardest in
  let begin_time = Time.to_epoch (Time.now ()) in
  let () = List.iter (fun p -> display_grid (solve p)) all_problems in
  let total_time = (Time.to_epoch (Time.now ())) -. begin_time in
  printf "Done. %d problems in %fs, avg. %f s/problem\n"
          (List.length all_problems)
          total_time
          (total_time /. (float_of_int (List.length all_problems)))
