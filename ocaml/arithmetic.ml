(* Determine whether a given integer number is prime. (medium) *)
let is_prime n =
  let rec pow a = function
    | 0 -> 1
    | 1 -> a
    | n -> let b = pow a (n / 2) in
      b * b * (if n mod 2 = 0 then 1 else a)
  in
  let rec aux i =
    if pow i 2 <= n then
      (if n mod i = 0 || n mod (i + 2) = 0 then false
       else aux (i+6))
    else true
  in
  if n <= 3 then n > 1
  else if (n mod 2 = 0 || n mod 3 = 0) then false
  else aux 5

(* Determine the greatest common divisor of two positive integer numbers.
 *  (medium) *)
let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b)

(* Determine whether two positive integer numbers are coprime. (easy) *)
let coprime a b = (gcd a b) = 1

(* Calculate Euler's totient function φ(m). (medium) *)
let phi m =
  let rec count_coprimes acc i =
    if i = m then acc
    else if coprime i m then count_coprimes (acc + 1) (i + 1)
    else count_coprimes acc (i + 1)
  in
  count_coprimes 0 1

(* Determine the prime factors of a given positive integer. (medium) *)
let factors n =
  let rec aux d n =
    if n = 1 then []
    else if n mod d = 0 then d::(aux d (n/d)) else aux (d+1) n
  in
  aux 2 n

(* Determine the prime factors of a given positive integer (2). (medium)
 * Construct a list containing the prime factors and their multiplicity *)
let factors' n =
  let rec aux d n =
    if n = 1 then []
    else if n mod d = 0 then
      match aux d (n/d) with
      | [] -> [(d, 1)]
      | (h, k)::tl when h = d -> (h, k+1)::tl
      | xs -> (d, 1)::xs
    else
      aux (d+1) n
  in
  aux 2 n

(* Calculate Euler's totient function φ(m) (improved). (medium) *)

(* If the list of the prime factors of a number m is known in the form of the
 *  previous problem then the function phi(m) can be efficiently calculated as
 *  follows:
 *    Let [(p1, m1); (p2, m2); (p3, m3); ...] be the list of prime factors (and
 *      their multiplicities) of a given number m.
 *    Then φ(m) can be calculated with the following formula:
 * φ(m) = (p1 - 1) × p1^(m1 - 1) ×
 *        (p2 - 1) × p2^(m2 - 1) ×
 *        (p3 - 1) × p3^(m3 - 1) ×
 *        ⋯ *)
let phi_improved m =
  let rec pow a = function
    | 0 -> 1
    | 1 -> a
    | n -> let b = pow a (n / 2) in
      b * b * (if n mod 2 = 0 then 1 else a)
  in
  let rec aux = function
    | [] -> 1
    | (p, m)::tl -> (p - 1) * (pow p (m - 1)) * aux tl
  in
  aux (factors' m)

(* Compare the two methods of calculating Euler's totient function. (easy) *)
let timeit f a =
  let open Unix in
  let start_time = Unix.gettimeofday () in
  let () = ignore (f a) in
  let end_time = Unix.gettimeofday () in
  end_time -. start_time;;

timeit phi 10090;;
timeit phi_improved 10090;;

(* A list of prime numbers. (easy)
 *
 * Given a range of integers by its lower and upper limit, construct a list of
 *  all prime numbers in that range. *)
let rec all_primes lower upper =
  if lower > upper then []
  else let rest = all_primes (lower + 1) upper in
    if is_prime lower then lower::rest else rest;;

(* Goldbach's conjecture. (medium)
 *
 * Goldbach's conjecture says that every positive even number greater than 2 is
 * the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most
 * famous facts in number theory that has not been proved to be correct in the
 * general case. It has been numerically confirmed up to very large numbers.
 * Write a function to find the two prime numbers that sum up to a given even
 * integer. *)
exception Impossible
let goldbach n =
  let rec aux k =
    if k >= n then raise Impossible
    else if is_prime k && is_prime (n - k) then (k, n-k)
    else aux (k+1)
  in
  aux 2

(* A list of Goldbach compositions. (medium)
 *
 * Given a range of integers by its lower and upper limit, print a list of all
 * even numbers and their Goldbach composition. *)
let goldbach_list lower upper =
  let rec range acc l h =
    if l < h then range (l::acc) (l+1) h
    else List.rev acc
  in
  let evens = List.filter (fun x -> x mod 2 = 0) (range [] lower upper) in
  List.map (fun x -> (x, goldbach x)) evens

(* In most cases, if an even number is written as the sum of two prime numbers,
 * one of them is very small. Very rarely, the primes are both bigger than say
 * 50. Try to find out how many such cases there are in the range 2..3000. *)
let goldbach_limit lower upper bound =
  let outside_limit (_, (s1, s2)) = s1 > bound && s2 > bound in
  List.filter (outside_limit) (goldbach_list (if lower > 2 then lower else 3) upper)
