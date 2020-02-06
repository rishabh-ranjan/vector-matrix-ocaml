(* TODO: define tests, document efficiency and correctness *)

type vector = float list
type matrix = float list list

exception InvalidInput
exception NotMatrix
exception UnequalVectorSize
exception UnequalMatrixShape
exception IncompatibleMatrixShape
exception SingularMatrix

let zero = 0.0
let one = 0.0

(*
 * Returns dimension of vector v.
 * Dimension of vector equals length of its representing list.
 * Time complexity: O(length of list).
 *)
let rec vdim (v:vector): int = List.length v

(*
 * Returns a zero vector of dimension n.
 * Time complexity: O(length of list).
 * Uses tail recursive auxiliary function, so efficient.
 *)
let rec mkzerov (n:int): vector =
    let rec aux acc n =
        if n = 0 then acc else aux (zero::acc) (n-1)
    in aux [] n

(*
 * Checks if v is a zero vector.
 * Returns false as soon as a non-zero element is found,
 * otherwise returns true, preventing unnecessary checks.
 * Time complexity: O(length of list).
 *)
let rec iszerov (v:vector): bool = match v with
| [] -> true
| h::t -> h = zero && iszero t (* short-circuit evaluation *)

(*
 * map2 f [x1; x2; ...] [y1; y2; ...] = [f x1 y1; f x2 y2; ...]
 * Vectors must have equal length.
 * Time complexity: O(length of vectors).
 * Tail-recursive auxiliary function.
 *)
let map2 f v1 v2 =
    let rec aux acc = function
    | [], [] -> List.rev acc
    | h1::t1, h2::t2 -> aux ((f h1 h2)::acc) (t1, t2)
    | _ -> raise UnequalVectorSize
    in aux [] (v1, v2)

(*
 * Adds two vectors.
 * Special case of map2.
 *)
let rec addv (v1:vector) (v2:vector): vector = map2 ( +. ) v1 v2

(*
 * Performs scalar multiplication.
 * Special case of List.map.
 *)
let rec scalarmultv (c:float) (v:vector): vector = List.map (( *. ) c) v

let rec dotprodv (v1:vector) (v2:vector): float = List.fold_left ( +. ) zero (map2 ( *. ) v1 v2)

(* TODO: implement the n dimensional generalisation! *)
(* https://math.stackexchange.com/questions/185991/is-the-vector-cross-product-only-defined-for-3d *)
(*
let rec crossprodv (v1:vector) (v2:vector): vector =
    *)

(*
 * Return dimensions (rows, columns) of m.
 * Verifies that each row vector has equal size,
 * raising NotMatrix exception if this is not the case.
 *)
let rec mdim (m:matrix): int*int =
    let f e v =
        if e = -1 || List.length v = e then List.length v
        else raise NotMatrix
    in
    (List.length m, List.fold_left f -1 m)

(* --- *)

(*
 * Returns an m x n zero matrix.
 * Time complexity: O(m*n).
 *)
let rec mkzerom (m_:int) (n_:int): matrix =
    let row = mkzerov n in
    let rec aux acc m = if m = 0 then acc else aux (row::acc) (m-1)
    in aux [] m_

(*
 * Checkes if m is a zero matrix.
 * Time complexity: O(rows * columns)
 *)
let rec iszerom (m:matrix): bool = match m with
| [] -> true (* empty matrix is considered a zero matrix *)
| h::t ->
    (* ensure that m is a matrix *)
    let cols = List.length h in
    if List.exists (fun x -> List.length x != cols) t then
        raise NotMatrix
    else
        let rec aux = function
        | [] -> true
        | h::t -> (iszerov h) && (aux t) (* short circuit eval *)
        in aux m

(*
 * Returns a m_*m_ unit matrix.
 * Time complexity: O(m_*m_).
 *)
let rec mkunitm (m_:int): matrix =
    (* Makes a row with a 1 at index c from back with nc cols *)
    let rec mkr acc nc c =
        if nc = 0 then acc
        else mkr ((if c = 0 then one else zero)::acc) (nc-1) (c-1)
    in
    let rec aux acc nr =
        if nr = 0 then acc
        else aux ((mkr [] n (n-nr))::acc) (nr-1)
    in aux [] n

(*
 * Checks if m is a unit square matrix.
 * Time complexity: O(m*m).
 *)
let rec isunitm (m:matrix): bool =
    (* Checks if square *)
    match mdim m with (r, c) -> if r <> c then false else
    (* Checks if row has 1 at index r, i tracks iteration index *)
    let rec isunitr i r = function
    | [] -> true
    | h::t -> h = (if i = r then one else zero) && isunitr (i+1) r t
    in
    let rec aux i = function
    | [] -> true
    | h::t -> (isunitr 0 i h) && (aux (i+1) t)
    in aux 0 m

(*
 * Returns sum of matrices m1 and m2.
 * Error if sizes don't match.
 * Time complexity: O(rows * columns).
 *)
let rec addm (m1:matrix) (m2:matrix): matrix =
    if (shape m1) <> (shape m2) then raise UnequalMatrixShape else
    let rec aux acc m1 m2 = match (m1, m2) with
    | ([], []) -> List.rev acc
    | (h1::t1, h2::t2) -> aux ((addv h1 h2)::acc) t1 t2
    | _ -> raise UnequalMatrixShape
    in aux [] m1 m2

(*
 * Performs scalar multiplication on a matrix.
 * Time complexity: O(rows * columns).
 *)
let rec scalarmultm (c:float) (m:matrix): matrix =
    List.map (scalarmultv c) m

(*
 * Returns the transpose of matrix m.
 * Time complexity: O(rows * columns).
 * Logic: in one pass over the rows of m,
 * the head of each row is put into accr,
 * which accumulates a row for the transpose matrix.
 *)
let rec transm (m:matrix): matrix =
    (* handle empty matrix case *)
    if m = [] then [] else
    (* 
     * accm accumulates the enire matrix; accr, the current row
     * top stores the part of m that has been processed,
     * with the heads removed.
     *)
    let rec aux accm accr top = function
    | [] -> (
        (* push the accumulated row into accm *)
        match List.hd top with (* top and bottom can't both be [] *)
        | [] -> List.rev ((List.rev accr)::accm)
        | _ -> aux ((List.rev accr)::accm) [] [] (List.rev top)
    )
    | (hh::tt)::t -> aux accm (hh::accr) (tt::top) t
    | []::t -> raise NotMatrix
    in aux [] [] [] m

(*
 * Returns product of m1 and m2.
 * Error if m1 and m2 are not compatible for matrix multiplication.
 * Time complexity: if m1 is a x b and m2 is b x c then O(a*b*c)
 * Logic: use transpose of m2,
 * Then result at i,j = dot product of ith and jth rows of matrices.
 * Accumulate rows and from rows the matrix suitably.
 *)
let rec multm (m1:matrix) (m2:matrix): matrix =
    let rec auxm accm m1 m2 = match m1 with
    | [] -> List.rev accm
    | h1::t1 ->
        let rec auxr accr = function
        | [] -> List.rev accr
        | h2::t2 -> auxr ((dotprodv h1 h2)::accr) t2
        in auxm ((auxr [] m2)::accm) t1 m2
    in auxm [] m1 (transm m2)

(*
 * Helper function for detm and inv functions.
 * The row which is used to zero out the other rows in
 * triangulization may have zero where non-zero is required.
 * If possible swap with a suitable row,
 * otherwise the matrix is singular.
 *)
let pivot lst =
    (*
     * sel = selected row,
     * rem = list of remaining rows,
     * fnd = bool checking if a 'non-singular' row has been found.
     *)
    let rec aux sel rem fnd = function
    | [] -> sel, rem, fnd
    | (hh::_, _ as h)::t ->
        if not fnd && hh <> zero then aux h rem true t
        else aux sel (h::rem) fnd t
    in aux ([],[]) [] false lst

(*
 * Returns determinant of m.
 * Logic: Triangularize m, the take the product of diagonal elements.
 * Time complexity: O(n^3), where n is the number of rows in m.
 *)
let rec detm (m:matrix): float =
    (* res stores final determinant value *)
    let rec aux n res = function
    | [] -> res
    | (hh::_ as h)::t as l ->
        let top, rem, fnd = pivot l in
        if not fnd then zero else
        let htop::ttop = top in
        (* prepare matrix for next round of processing *)
        let rec pmat acc = function
        | [] -> acc
        | (hh::th)::t ->
            let mf = hh /. htop in
            (* prepare row for next round of processing *)
            let rec prow acc lst top = match lst, top with
            | [], [] -> acc
            | hl::tl, ht::tt -> prow ((hl -. ht *. mf)::acc) tl tt
            in pmat ((prow [] th ttop)::acc) t
        in
        (* adjust sign of res for swaps and reversal of row order *)
        let neg = if ((n-1)/2 mod 2 = 1) <> (hh = zero) then - one else one in
        aux (n-1) (neg *. res *. htop) (pmat [] rem)
    in aux (List.length m) one m

    (*
let rec invm (m:matrix): matrix =
    *)
