(* Let us define a small "language" for boolean expressions containing variables: *)
type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr

(* Helper that will evaluate a bool_expr with a given state *)
let eval state expr =
  let rec eval' expr =
    match expr with
    | Var s -> List.assoc s state
    | Not expr -> not (eval' expr)
    | And (e1, e2) -> (eval' e1) && (eval' e2)
    | Or (e1, e2) -> (eval' e1) || (eval' e2)
  in
  eval' expr

(* Truth tables for logical expressions (2 variables). (medium) *)
let table2 v1 v2 expr =
  let v1_states = [(v1, true); (v1, false)] in
  let v2_states = [(v2, true); (v2, false)] in
  let rec combinations s1 s2 =
    match s1 with
    | [] -> []
    | h::tl -> (List.map (fun s -> h::s::[]) s2)@(combinations tl s2)
  in
  let get_val (_, s) = s in
  List.map
    (fun states -> (List.map get_val states)@[(eval states expr)])
    (combinations v1_states v2_states)

(* Truth tables for logical expressions. (medium) *)
let table vars expr =
  let add_var_to_state var state =
    List.map (fun o -> (var, true)::o) state @
    List.map (fun o -> (var, false)::o) state
  in
  let rec state_combinations = function
    | [] -> [[]]
    | var::tl -> add_var_to_state var (state_combinations tl)
  in
  List.map (fun s -> (s, eval s expr)) (state_combinations vars)

(* Gray code. (medium)
 *
 * An n-bit Gray code is a sequence of n-bit strings constructed according to
 * certain rules. For example,
 * n = 1: C(1) = ['0', '1'].
 * n = 2: C(2) = ['00', '01', '11', '10'].
 * n = 3: C(3) = ['000', '001', '011', '010', '110', '111', '101', '100'].
 *
 * Find out the construction rules and write a function with the following
 * specification: gray n returns the n-bit Gray code.
 *
 * TODO *)

(* Huffman code (hard) *)
module PQ =
struct
  type priority = int
  type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
  let empty = Empty
  let rec insert queue prio elt =
    match queue with
    | Empty -> Node(prio, elt, Empty, Empty)
    | Node (p, e, left, right) ->
      if prio <= p
      then Node(prio, elt, insert right p e, left)
      else Node(p, e, insert right prio elt, left)
  exception Empty_queue
  let rec pop = function
    | Empty -> raise Empty_queue
    | Node(prio, elt, left, Empty) -> left
    | Node(prio, elt, Empty, right) -> right
    | Node(prio, elt, (Node(lprio, lelt, _, _) as left),
           (Node(rprio, relt, _, _) as right)) ->
      if lprio <= rprio
      then Node(lprio, lelt, pop left, right)
      else Node(rprio, relt, left, pop right)
  let extract = function
    | Empty -> raise Empty_queue
    | Node(prio, elt, _, _) as queue -> (prio, elt, pop queue)
  let rec size = function
    | Empty -> 0
    | Node(_, _, left, right) -> 1 + size left + size right
end;;

type 'a huffman_node =
  | Leaf of 'a
  | Node of 'a huffman_node * 'a huffman_node

let huffman fs =
  let rec aux queue =
    if PQ.size queue > 1 then
      let (p1, e1, queue') = PQ.extract queue in
      let (p2, e2, queue'') = PQ.extract queue' in
      aux (PQ.insert queue'' (p1 + p2) (Node (e1, e2)))
    else
      let (_, v, _) = PQ.extract queue in
      v
  in
  let rec to_code soFar = function
    | Leaf v -> [(v, soFar)]
    | Node (l, r) -> (to_code (soFar ^ "0") l) @ (to_code (soFar ^ "1") r)
  in
  let q = List.fold_left (fun q (v, freq) -> PQ.insert q freq (Leaf v)) PQ.empty fs in
  to_code "" (aux q)
