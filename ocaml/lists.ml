(* Write a function
 *  last : 'a list -> 'a option
 *  that returns the last element of a list. (easy) *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | h::tl -> last tl

(* Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec last_two = function
  | [] -> None
  | [x] -> None
  | [x ; y] -> Some (x, y)
  | h::tl -> last_two tl

(* Find the K'th element of a list. (easy) *)
let at k l =
  let rec at' n = function
    | [] -> None
    | h::tl -> if n = k then Some h else at' (n+1) tl
  in
  at' 0 l

(* Find the number of elements of a list. (easy) *)
let rec length = function
  | [] -> 0
  | h::tl -> 1 + length tl

(* Reverse a list. (easy) *)
let rev l =
  let rec rev' acc = function
    | [] -> acc
    | h::tl -> rev' (h::acc) tl
  in
  rev' [] l

(*  Find out whether a list is a palindrome. (easy) *)
let is_palindrome l =
  let rec list_equals l1 l2 =
    match (l1, l2) with
    | [], [] -> true
    | [], _ -> false
    | _ , [] -> false
    | x1::tl1, x2::tl2 -> x1 = x2 && list_equals tl1 tl2
  in
  list_equals (rev l) l

type 'a node =
  | One of 'a
  | Many of 'a node list

(* Flatten a nested list structure. (medium) *)
let rec flatten = function
  | [] -> []
  | One x :: t -> x::(flatten t)
  | Many ls :: t -> (flatten ls) @ (flatten t)

(* Eliminate consecutive duplicates of list elements. (medium) *)
let rec compress = function
  | [] -> []
  | [x] -> [x]
  | x1::x2::tl -> if x1 = x2 then compress (x1::tl)
    else x1 :: (compress (x2::tl))

(* Pack consecutive duplicates of list elements into sublists. (medium) *)
let pack l =
  let rec pack' acc = function
    | [] -> []
    | [x] -> (x::acc)::[]
    | x1::x2::tl -> if x1 = x2 then pack' (x2::acc) (x1::tl)
          else (x1::acc)::(pack' [] (x2::tl))
  in
  pack' [] l
