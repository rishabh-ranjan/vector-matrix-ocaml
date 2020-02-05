(* TODO: define tests, document efficiency and correctness *)

type vector = float list
type matrix = float list list

exception InvalidInput
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
| h::t -> h = 0.0 && iszero t (* short-circuit evaluation *)

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
(* ---- *)

let rec dotprodv (v1:vector) (v2:vector): float = List.fold_left ( +. ) 0.0 (opv ( *. ) v1 v2)

(* TODO: implement the n dimensional generalisation! *)
(* https://math.stackexchange.com/questions/185991/is-the-vector-cross-product-only-defined-for-3d *)
(*
let rec crossprodv (v1:vector) (v2:vector): vector =
    *)

let rec mdim (m:matrix): int*int = match m with
    [] -> (0, 0) (* could be 0*c or r*0 for any r, c actually *)
    | h :: _ -> (List.length m, List.length h)

let rec mkzerom (m_:int) (n_:int): matrix =
    let rec mkrow acc n = if n = 0 then acc else mkrow (0.0 :: acc) (n - 1)
    in
    let row = mkrow [] n_ in
    let rec aux acc m = if m = 0 then acc else aux (row :: acc) (m - 1) in
    aux [] m_

(* TODO: check if it is a matrix first *)
let rec iszerom (m:matrix): bool =
    let rec iszeror = function
        [] -> true
        | h :: t -> if h = 0.0 then iszeror t else false
    in
    match m with
    [] -> true
    | h :: t -> if iszeror h then iszerom t else false

let rec mkunitm (m_:int): matrix =
    let rec mkrow acc i r = if i = m_ then acc else mkrow ((if i = r then 1.0 else 0.0) :: acc) (i + 1) r
    in
    let rec aux acc i = if i = m_ then acc else aux ((mkrow [] 0 i) :: acc) (i + 1)
    in
    aux [] 0

(* TODO: check if it is a square matrix first *)
let rec isunitm (m:matrix): bool =
    let rec isunitr i r = function
        [] -> true
        | h :: t ->
                if i = r then
                    if h = 1.0 then
                        isunitr (i + 1) r t
                    else
                        false
                else
                    if h = 0.0 then
                        isunitr (i + 1) r t
                    else
                        false
    in
    let rec aux i = function
        [] -> true
        | h :: t -> if isunitr 0 i h then aux (i + 1) t else false
    in
    aux 0 m
(* === *)

    (* shape and size checking deferred to later 
let shapem = function
    | [] -> (0, 0) (* could be 0*c or r*0 for any r, c *)
    | h :: t ->
            let m = List.length h in
            let rec aux m
            *)

let rec addm (m1:matrix) (m2:matrix): matrix =
    let rec addr acc r1 r2 = match (r1, r2) with
    | ([], []) -> List.rev acc
    | (h1::t1, h2::t2) -> addr ((h1 +. h2)::acc) t1 t2
    | _ -> raise InvalidInput
    in
    let rec aux acc m1 m2 = match (m1, m2) with
    | ([], []) -> List.rev acc
    | (h1::t1, h2::t2) -> aux ((addr [] h1 h2)::acc) t1 t2
    | _ -> raise InvalidInput
    in aux [] m1 m2

let rec scalarmultm (c:float) (m:matrix): matrix = List.map (List.map (( *. ) c)) m

let rec transm (m:matrix): matrix =
    let rec aux accm accr top = function
    | [] -> (
        match List.hd top with (* empty list implies zero size matrix *)
        | [] -> List.rev ((List.rev accr)::accm)
        | _ -> aux ((List.rev accr)::accm) [] [] (List.rev top)
    )
    | (hh::tt)::t -> aux accm (hh::accr) (tt::top) t
    | []::t -> raise InvalidInput (* TODO: handle zero size matrices (?) *)
    in aux [] [] [] m

let rec multm (m1:matrix) (m2:matrix): matrix =
    let rec dot acc v1 v2 = match v1, v2 with
    | [], [] -> acc
    | h1::t1, h2::t2 -> dot (h1 *. h2 +. acc) t1 t2
    | _ -> raise InvalidInput
    in
    let rec auxm accm m1 m2 = match m1 with
    | [] -> List.rev accm
    | h1::t1 ->
        let rec auxr accr = function
        | [] -> List.rev accr
        | h2::t2 -> auxr ((dot 0.0 h1 h2)::accr) t2
        in auxm ((auxr [] m2)::accm) t1 m2
    in auxm [] m1 (transm m2)

let rec detm (m:matrix): float =
    let rec aux res = function
    | [] -> res
    | (hh::_ as h)::t ->
        let rec pivot acc lead flag = function
        | [] -> lead, acc
        | (hh::_ as h)::t ->
            if flag = false && hh <> 0 then pivot acc h true
            else pivot (h::acc) lead flag
        in
        let hd, tl =
            if hh = 0 then
                pivot [] 0.0 false
            else
                h, t
        in
        let el = List.hd hd in
        let rec prep acc rel = function
        | [], [] -> acc
        | (hh::tt)::t, hl::tl ->
            prep ((List.map (fun x -> x -. hl *. (rel /. el)) tt)::acc) (List.hd (List.hd t)) t tl
        in
        aux (res * el) (prep [] (List.hd (List.hd tl)))
    in aux 1.0 m

    (*
let rec invm (m:matrix): matrix =
    *)
