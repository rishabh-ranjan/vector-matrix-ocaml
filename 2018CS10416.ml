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
let one = 1.0

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
| h::t -> h = zero && iszerov t (* short-circuit evaluation *)

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

(*
 * Returns the dot product of v1 and v2.
 * Efficient O(n) implementation using List.fold_left.
 *)
let rec dotprodv (v1:vector) (v2:vector): float = List.fold_left ( +. ) zero (map2 ( *. ) v1 v2)

(*
 * Returns the cross product of 2 3-dimensional vectors v1 and v2.
 * Error if dimension is not 3.
 * Direct formula is used.
 *)
let rec crossprodv (v1:vector) (v2:vector): vector =
    if ((vdim v1) <> 3) || ((vdim v2) <> 3) then raise InvalidInput else
    (* assuming 3d vectors; using standard formula *)
    match v1, v2 with [x1; y1; z1], [x2; y2; z2] ->
        [
            y1 *. z2 -. y2 *. z1;
            z1 *. x2 -. z2 *. x1;
            x1 *. y2 -. x2 *. y1;
        ]
    | _ -> raise InvalidInput

(*
 * NOTE: generalised cross product is implemented after
 * the definition of detm.
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
    (List.length m, List.fold_left f (-1) m)

(*
 * Returns an m x n zero matrix.
 * Time complexity: O(m*n).
 *)
let rec mkzerom (m_:int) (n_:int): matrix =
    let row = mkzerov n_ in
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
        else aux ((mkr [] m_ (m_-nr))::acc) (nr-1)
    in aux [] m_

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
    if (mdim m1) <> (mdim m2) then raise UnequalMatrixShape else
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
    if (snd (mdim m1)) <> (fst (mdim m2)) then
        raise IncompatibleMatrixShape
    else
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
 *
 * NOTE: where equality with 0.0 is being tested,
 * it would be better to check some epsilon range.
 * A very small positive number will not give
 * SingularMatrix exception in invm, but the inverse
 * computation would mathematically involve very large
 * numbers, which could possibly overflow.
 *)
(* pivotdet and pivotinv have only minor context related differences. *)
let pivotdet lst =
    (*
     * sel = selected row,
     * rem = list of remaining rows,
     * fnd = bool checking if a 'non-singular' row has been found.
     *)
    let rec aux sel rem fnd = function
    | [] -> sel, rem, fnd
    | (hh::_ as h)::t ->
        if not fnd && hh <> zero then aux h rem true t
        else aux sel (h::rem) fnd t
    | _ -> raise InvalidInput
    in aux [] [] false lst

let pivotinv lst =
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
    | _ -> raise InvalidInput
    in aux ([],[]) [] false lst

(*
 * Returns determinant of m.
 * Logic: Triangularize m, the take the product of diagonal elements.
 * Time complexity: O(n^3), where n is the number of rows in m.
 * NOTE: runtime of (detm (mkunitm 500)) is ~ 5s on my PC.
 * (see timed_detm function in testing section)
 *)
let rec detm (m:matrix): float =
    (* res stores final determinant value *)
    let rec aux n res = function
    | [] -> res
    | (hh::_)::t as l ->
        let top, rem, fnd = pivotdet l in
        if not fnd then zero else
        let htop, ttop = List.hd top, List.tl top in (* top is guaranteed to be non-[] here *)
        (* prepare matrix for next round of processing *)
        let rec pmat acc = function
        | [] -> List.rev acc
        | (hh::th)::t ->
            let mf = hh /. htop in
            (* prepare row for next round of processing *)
            let rec prow acc lst top = match lst, top with
            | [], [] -> acc
            | hl::tl, ht::tt -> prow ((hl -. ht *. mf)::acc) tl tt
            | _ -> raise InvalidInput
            in pmat ((prow [] th ttop)::acc) t
        | _ -> raise InvalidInput
        in
        (* adjust sign of res for pivoting *)
        let neg = if hh = zero then -. one else one in
        aux (n-1) (neg *. res *. htop) (pmat [] rem)
    | _ -> raise InvalidInput
    in aux (List.length m) one m

(*
 * Returns the inverse of matrix m.
 * Error if matrix is singular.
 * Logic: Gauss Jordan elimination is used.
 * Time complexity: O(n^3).
 * NOTE: runtime of (invm (mkunitm 500)) is ~ 36s on my PC.
 * (see timed_invm function in testing section)
 *)
let rec invm (m:matrix): matrix =
    (*
     * m is split into top and bottom parts,
     * rows are moved into the top part as they are processed;
     * finally, bottom part is empty and top contains the inverse.
     *)
    let rec aux top = function
    | [] -> List.rev (List.map (fun (x, y) -> y) top)
    | _ as bot ->
        let sel, rem, fnd = pivotinv bot in
        if not fnd then raise SingularMatrix else
        let hts, ls = sel in (* hts is never [] *)
        let hs, ts = List.hd hts, List.tl hts in
        let f h x y = x -. y *. h /. hs in
        let prep (ht, l) = match ht with
        | [] -> raise InvalidInput
        | h::t -> (map2 (f h) t ts), (map2 (f h) l ls) in
        let nxt = List.map prep in
        let fs x = x /. hs in
        let sel' = (List.map fs ts), (List.map fs ls) in
        aux (sel'::(nxt top)) (nxt rem)
    (* m is augmented by pairing each row with a unit matrix row *)
    in aux [] (map2 (fun x y -> (x, y)) m (mkunitm (List.length m)))

(*
 * m is an (n-1) x n matrix.
 * Returns an n-dimensional vector representing the generalized
 * cross product of the (n-1) row vectors of m.
 * Time complexity: O(n^4).
 * Logic: Each part of resulting vector is a cofactor,
 * which can be easily computed using the detm function
 * in O(n^3) giving a total of O(n^4).
 *)
let rec crossprodvgen (m:matrix): vector =
    let r, c = mdim m in
    if r <> c - 1 then raise InvalidInput else
    let cofactor i =
        let rec partial_copy acc i = function
        | [] -> List.rev acc
        | h::t -> partial_copy (if i = 0 then acc else (h::acc)) (i-1) t 
        in
        (if i mod 2 = 0 then 1.0 else -1.0) *. (detm (List.map (partial_copy [] i) m))
    in
    let rec aux acc i =
        if i = (List.length m) + 1 then List.rev acc
        else aux ((cofactor i)::acc) (i+1)
    in aux [] 0

(* === testing === *)
(* The code has been thoroughly tested.
 * These functions were used to generate random tests.
 * Testing of detm, multm, invm, transm, etc. were done
 * using the mathematical properties relating them,
 * such as,
 * 1. det(A * B) = det(A) * det(B)
 * 2. det(inv(A)) = 1.0 / det(A)
 * 3. det(trans(A)) = det(A)
 * 4. inv(trans(A)) = trans(inv(A))
 * 5. det(unit matrix) = 1.0
 * 6. det(zero matrix) = 0.0
 * etc.
 *
 * NOTE: direct equality may not hold in the above due
 * to precision issues of floating point numbers.
 * Manual inspection or checking equality in an epsilon
 * range is a good enough test.
 *
 * The testing was done in the ocaml toplevel as follows:
 * 1. '#use "<filename>.ml";;' loads functions defined here.
 * 2. define some matrices.
 * 3. test using above properties.
 * 4. perform some direct tests.
 * etc.
 *)

let max = 1000.0

let rec genv n =
    let rec aux acc n =
        if n = 0 then acc
        else aux ((if (Random.int 2) mod 2 = 0 then Random.float max else Random.float (-.max))::acc) (n-1)
    in aux [] n

let rec genm m n =
    let rec aux acc m =
        if m = 0 then acc
        else aux ((genv n)::acc) (m-1)
    in aux [] m

(*
 * Prints time taken for determinant of matrix m,
 * and returns value returned by detm m.
 *)
let timed_detm m =
    let init = Sys.time() in
    let r = detm m in
    Printf.printf "Time taken: %fs\n" (Sys.time() -. init); r

(*
 * Prints time taken for inverse of matrix m,
 * and returns value returned by invm m.
 *)
let timed_invm m =
    let init = Sys.time() in
    let r = invm m in
    Printf.printf "Time taken: %fs\n" (Sys.time() -. init); r
