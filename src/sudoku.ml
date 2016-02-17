(* Implementation of http://norvig.com/sudoku.html. See this page for the definition of the terms used here *)
open Core.Std
open Batteries


exception Invalid_grid of string

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

let rec eliminate (i:int) (d: int) (arr: square_value array): unit =
  if not (is_in d arr.(i))
  then let () = printf "Eliminating %d in position %d: wasn't there\n" d i in () (* Nothing to eliminate *)
  else
    let () = arr.(i) <- List.filter (fun el -> el <> d) arr.(i) in
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
             let dplaces = List.filter (fun u -> is_in d arr.(i)) unt in (* [s for s in u if d in values[s]] *)
             let () = match dplaces with
             | [] -> raise (Invalid_grid "No possible value left for a square in a unit, giving up")
             | dp :: [] -> assign (coord_to_index dp) d arr
             | _ -> ()
             in ()
           ) unts
and assign (i:int) (d: int) (arr: square_value array): unit =
  let () = printf "Assigning %d in position %d\n" d i in
  let other_values = List.filter (fun el -> el <> d) arr.(i) in
  let () = printf "Other values left %s\n" (sq_value_to_string other_values) in
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

(* Add space at the end of a string to make it of size i *)
let rec pad str i c = if (String.length str) = i then str else pad (str ^ c) i c

let display_grid arr: unit =
  let width_column = Array.fold_left (fun acc e1 -> if acc >= List.length e1 then acc else List.length e1) 0 arr in
  let arr' = Array.map (fun s -> sq_value_to_string s) arr in
  for i = 0 to 81 do
    if (i mod (9*3)) = 0 then print_string ("\n" ^ (pad "-" ((width_column * 9) + 2) "-") ^ "\n")
    else if (i mod 9) = 0 then print_string "\n"
    else if (i mod 3) = 0 then print_string "|" else ();
    print_string (pad arr'.(i) width_column " ")
  done


let () =
  let g = "003020600900305001001806400008102900700000008006708200002609500800203009005010300" in
  display_grid (parse_grid g)
