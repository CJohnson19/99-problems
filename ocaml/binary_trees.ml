type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree

(* Construct completely balanced binary trees. (medium)
 *
 *  a completely balanced binary tree, the following property holds for every
 *  node: The number of nodes in its left subtree and the number of nodes in its
 *  right subtree are almost equal, which means their difference is not greater
 *  than one. *)
let add_trees left right all =
  let rec add_right_tree all l =
    List.fold_left (fun a r -> Node('x', l, r)::a) all right in
  List.fold_left add_right_tree all left

let rec cbal_tree n =
  if n = 0 then [Empty]
  else if n mod 2 = 1 then
    let tree = cbal_tree (n / 2) in
    add_trees tree tree []
  else
    let left = cbal_tree (n / 2) in
    let right = cbal_tree (n / 2 - 1) in
    add_trees left right (add_trees right left [])

(* Symmetric binary trees. (medium) *)
let is_symmetric tree =
  let rec is_mirror t1 t2 =
    match (t1, t2) with
    | Empty, Empty -> true
    | Node(_, left1, right1), Node(_, left2, right2) ->
      is_mirror left1 right2 && is_mirror right1 left2
    | _ -> false
  in
  match tree with
  | Empty -> true
  | Node (_, left, right) -> is_mirror left right

(* Binary search trees (dictionaries). (medium) *)
let construct l =
  let rec insert tree v =
    match tree with
    | Empty -> Node (v, Empty, Empty)
    | Node (v', l, r) ->
      if v < v' then Node(v', insert l v, r)
      else Node(v', l, insert r v)
  in
  List.fold_left insert Empty l

(* Generate-and-test paradigm. (medium) *)
let sym_cbal_trees n =
  List.filter is_symmetric (cbal_tree n)

(* Construct height-balanced binary trees. (medium) *)
let rec hbal_tree h =
  if h = 0 then [Empty]
  else if h = 1 then [Node('x', Empty, Empty)]
  else
    let t1 = hbal_tree (h-1) in
    let t2 = hbal_tree (h-2)in
    add_trees t1 t1 (add_trees t1 t2 (add_trees t2 t1 []))

(* Construct height-balanced binary trees with a given number of nodes. (medium)
 *
 * Try to find a recursive statement and turn it into a function min_nodes
 *  defined as follows: min_nodes h returns the minimum number of nodes in a
 *  height-balanced binary tree of height h. *)
let rec min_nodes h =
  if h = 0 then 0
  else if h = 1 then 1
  else min_nodes (h-1) + min_nodes (h-2) + 1

(* what are the minimum (resp. maximum) height H a height-balanced binary tree
 *  with N nodes can have? min_height (resp. max_height n) returns the minimum
 *  (resp. maximum) height of a height-balanced binary tree with n nodes. *)
let rec min_height n =
  if n = 0 then 0
  else if n = 1 then 1
  else min_height (n / 2) + 1

(* construct all the height-balanced binary trees with a given number of nodes.
 *  hbal_tree_nodes n returns a list of all height-balanced binary tree with n
 *  nodes.
 * TODO *)

(* Count the leaves of a binary tree. (easy) *)
let rec count_leaves = function
  | Empty -> 0
  | Node(_, Empty, Empty) -> 1
  | Node(_, left, right) -> count_leaves left + count_leaves right

(* Collect the leaves of a binary tree in a list. (easy) *)
let rec leaves = function
  | Empty -> []
  | Node(v, Empty, Empty) -> [v]
  | Node(_, left, right) -> leaves left @ leaves right

let leaves_acc tree =
  let rec aux acc = function
    | Empty -> acc
    | Node(v, Empty, Empty) -> v::acc
    | Node(_, left, right) -> aux (aux acc right) left
  in
  aux [] tree

(* Collect the internal nodes of a binary tree in a list. (easy) *)
let internals tree =
  let rec aux acc = function
    | Empty -> acc
    | Node(_, Empty, Empty) -> acc
    | Node(v, left, right) -> aux (aux (v::acc) right) left
  in
  aux [] tree

(* Collect the nodes at a given level in a list. (easy) *)
let at_level tree n =
  let rec aux tree acc level =
    match tree with
    | Empty -> acc
    | Node(v, l, r) when level = n -> v::acc
    | Node(v, l, r) -> aux l (aux r acc (level + 1)) (level + 1)
  in
  aux tree [] 1
