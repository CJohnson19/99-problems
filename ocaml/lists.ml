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

(* Run length encoding of a list. (easy) *)
let encode l =
  let rec aux n a = function
    | [] -> [(n, a)]
    | h::tl -> if h = a then aux (n+1) a tl
          else (n, a)::(aux 1 h tl)
  in
  match l with
  | [] -> []
  | h::tl -> aux 1 h tl

(* Modified run-length encoding. (easy) *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode' l =
  let form_one n a =
    if n = 1 then One a
        else Many (n, a)
  in
  let rec aux n a = function
    | [] -> [form_one n a]
    | h::tl -> if h = a then aux (n+1) a tl
      else (form_one n a)::(aux 1 h tl)
  in
  match l with
  | [] -> []
  | h::tl -> aux 1 h tl

(* Decode a run-length encoded list. (medium) *)
let decode l =
  let rec aux' n a =
    if n = 0 then [] else a::(aux' (n-1) a)
  in
  let aux = function
    | One x -> aux' 1 x
    | Many (n, x) -> aux' n x
  in
  List.flatten (List.map aux l)

(* Run-length encoding of a list (direct solution). (medium) *)
(* Already implemented by my above solution *)

(* Duplicate the elements of a list. (easy) *)
let rec duplicate = function
  | [] -> []
  | h::tl -> h::h::(duplicate tl)

(* Replicate the elements of a list a given number of times. (medium) *)
let replicate n l =
  let rec aux n x =
    if n = 0 then []
    else x::(aux (n-1) x)
  in
  List.flatten (List.map (aux n) l)

(* Drop every N'th element from a list. (medium) *)
let drop l n =
  let rec aux i = function
    | [] -> []
    | h::tl when i = n -> aux 1 tl
    | h::tl -> h::(aux (i+1) tl)
  in
  aux 1 l

(* Split a list into two parts, the length of the first part is given. (easy) *)
let split l n =
  let rec aux so_far rest n =
    match rest with
    | [] -> (List.rev so_far, [])
    | h::tl when n > 0 -> aux (h::so_far) tl (n-1)
    | _ -> (List.rev so_far, rest)
  in
  aux [] l n

(* Extract a slice from a list. (medium) *)
let slice l left right =
  let rec aux i = function
    | [] -> []
    | h::tl when left <= i && i <= right -> h::(aux (i+1) tl)
    | _::tl -> aux (i+1) tl
  in
  aux 0 l

(* Rotate a list N places to the left. (medium) *)
let rotate l n =
  let len = List.length l in
  let n' = (n mod len + len) mod len in
  let a, b = split l n' in
  b @ a

(* Remove the K'th element from a list. (easy) *)
let remove_at k l =
  let rec aux i = function
    | [] -> []
    | _::tl when i = k -> tl
    | h::tl -> h::(aux (i+1) tl)
  in
  aux 0 l

(* Insert an element at a given position into a list. (easy) *)
let insert_at elem k l =
  let rec aux i = function
    | [] when i = k -> [elem]
    | [] -> []
    | h::tl when i = k -> elem::h::tl
    | h::tl -> h::(aux (i+1) tl)
  in
  aux 0 l

(* Create a list containing all integers within a given range. (easy) *)
let range first last =
  let step = if first <= last then 1 else (-1) in
  let rec gen_range curr =
    if curr = last then [curr]
    else curr::(gen_range (curr+step))
  in
  gen_range first

(* Extract a given number of randomly selected elements from a list. (medium) *)
exception Invalid_index
let rand_select l n =
  let rec extract so_far n = function
    | [] -> raise Invalid_index
    | h::tl when n = 0 -> (h, so_far @ tl)
    | h::tl -> extract (h::so_far) (n-1) tl
  in
  let extract_one l =
    extract [] (Random.int (List.length l)) l
  in
  let rec aux l n =
    if n = 0 then []
    else
      let o, l' = extract_one l in
      o::(aux l' (n-1))
  in
  aux l n

(* Lotto: Draw N different random numbers from the set 1..M. (easy) *)
let lotto_select n m =
  let rec aux n =
    if n = 0 then []
    else (Random.int m + 1)::(aux (n-1))
  in
  aux n

(* Generate a random permutation of the elements of a list. (easy) *)
let permutation l =
  rand_select l (List.length l)

(* Generate the combinations of K distinct objects chosen from the N elements
 * of a list. (medium) *)
let rec extract k l =
  if k <= 0 then [[]]
  else match l with
    | [] -> []
    | h::tl ->
      let with_h = List.map (fun l -> h::l) (extract (k-1) tl) in
      let without_h = extract k tl in
      with_h @ without_h

(* Group the elements of a set into disjoint subsets. (medium) *)
(* TODO *)

(* Sorting a list of lists according to length of sublists. (medium) *)
let length_sort ls =
  let compare_lens l1 l2 = compare (List.length l1) (List.length l2)
  in
  List.sort (compare_lens) ls

(* Part 2: this time the objective is to sort the elements of this list
 *  according to their length frequency. i.e., in the default, where sorting is
 *  done ascendingly, lists with rare lengths are placed first, others with a
 *  more frequent length come later.
 *)
let frequency_sort ls =
  let tbl = Hashtbl.create 100 in
  let add_len len =
    if Hashtbl.mem tbl len then
      Hashtbl.replace tbl len ((Hashtbl.find tbl len) + 1)
    else
      Hashtbl.add tbl len 1
  in
  let () = List.iter (add_len) (List.map (List.length) ls) in
  let compare_len_freq l1 l2 =
    compare (Hashtbl.find tbl (List.length l1)) (Hashtbl.find tbl (List.length l2))
  in
  List.sort (compare_len_freq) ls
