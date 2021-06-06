(* Eight queens problem. (medium)
 *
 * This is a classical problem in computer science. The objective is to place
 *  eight queens on a chessboard so that no two queens are attacking each other;
 *  i.e., no two queens are in the same row, the same column, or on the same
 *  diagonal.
 *)
let are_valid_queens l =
  let abs n = if n < 0 then (-n) else n in
  let rec aux' k offset = function
    | [] -> true
    | h::tl ->
      if h = k || abs (h - k) = offset then false
      else aux' k (offset+1) tl in
  let rec aux queens =
    match queens with
    | [] -> true
    | h::tl -> if aux' h 1 tl then aux tl else false in
  aux l
let queens_positions n =
  let rec range lower upper =
    if lower = upper then [lower]
    else lower::(range (lower + 1) upper) in
  let rec gen_queens k =
    if k = 1 then List.map (fun x -> [x]) (range 1 n)
    else
      let rest = gen_queens (k-1) in
      List.filter are_valid_queens
      (List.flatten
        (List.map (fun q ->
             (List.map (fun qs -> q::qs) rest))
            (range 1 n)))
  in
  gen_queens n

(* Knight's tour. (medium)
 *
 * Another famous problem is this one: How can a knight jump on an N×N
 *  chessboard in such a way that it visits every square exactly once? *)
open Option
let knights_tour n =
  let rec up_to_n i = if i = n then [i] else i::(up_to_n (i+1)) in
  let nums = up_to_n 1 in
  let n_total = (n * (n+1)) / 2 * n in
  let valid_pos (x, y) visited =
    0 < x && x <= n && 0 < y && y <= n && not (List.mem (x,y) visited) in
  let jump (x, y) seen =
    let xs = List.map (fun offset -> x + offset) [1;2;2;1;-1;-2;-2;-1] in
    let ys = List.map (fun offset -> y + offset) [2;1;-1;-2;-2;-1;1;2] in
    let coords = List.map2 (fun x y -> (x,y)) xs ys in
    let next = List.filter (fun coord -> valid_pos coord seen) coords in
    if List.length next = 0 then None else Some next
  in
  let tour_finished visited =
    let a, b = List.fold_left (fun (x', y') (x, y) -> (x+x', y+y')) (0,0) visited in
    a = b && a = n_total
  in
  let rec try_tour (x, y) visited =
    if tour_finished visited then Some visited else
    let moves = jump (x, y) visited in
    match moves with
    | None -> None
    | Some moves' ->
      let next_moves = (List.filter_map
        (fun (x', y') ->
           let visited' = (x', y')::visited in
           try_tour (x', y') visited') moves') in
      if List.length next_moves > 0 then Some (List.hd next_moves) else None
  in
  List.filter (is_some) (List.map2 (fun x y -> try_tour (x, y) [(x, y)]) nums nums)

(* An arithmetic puzzle
 *
 * Given a list of integer numbers, find a correct way of inserting arithmetic
 *  signs (operators) such that the result is a correct equation. Example: With
 *  the list of numbers [2; 3; 5; 7; 11] we can form the equations
 *  2 - 3 + 5 + 7 = 11 or 2 = (3 * 5 + 7) / 11 (and ten others!).
 *)
exception Solution_found of string
exception No_solution
let equation_finder nums =
  let rec solve_with_ans soFar repr nums ans =
    match nums with
    | [] -> if soFar = ans then raise (Solution_found repr) else ()
    | h::tl ->
      solve_with_ans (soFar + h) (repr ^ "+" ^ string_of_int h) tl ans;
      solve_with_ans (soFar - h) (repr ^ "-" ^ string_of_int h) tl ans;
      solve_with_ans (soFar * h) (repr ^ "*" ^ string_of_int h) tl ans;
      solve_with_ans (soFar / h) (repr ^ "/" ^ string_of_int h) tl ans;
  in
  let ans1 = List.hd nums in
  let soFar = (List.hd (List.tl nums)) in
  try solve_with_ans soFar (string_of_int soFar) (List.tl (List.tl nums)) ans1 with
  | Solution_found s -> print_endline (string_of_int ans1 ^ "=" ^ s)
  | _ ->
    let rev_nums = List.rev nums in
    let ans2 = List.hd rev_nums in
    let rev_nums' = List.rev (List.tl nums) in
    let soFar' = List.hd (List.tl nums) in
    try solve_with_ans soFar' (string_of_int soFar') (List.tl rev_nums') ans2 with
    | Solution_found s -> print_endline (s ^ "=" ^ (string_of_int ans2))
    | _ -> raise No_solution

(* English number words. (medium)
 *
 * On financial documents, like cheques, numbers must sometimes be written in
 *  full words. Example: 175 must be written as one-seven-five. Write a function
 *  full_words to print (non-negative) integer numbers in full words. *)
let full_words n =
  let get_word n =
    match n with
    | 0 -> "zero"
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four"
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine"
  in
  let rec aux n =
    if n > 10 then
     aux (n / 10) ^ "-" ^ (get_word (n mod 10))
    else get_word n
  in
  if n = 0 then "zero" else aux n
