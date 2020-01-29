type vector = float list
type matrix = float list list

exception InvalidInput
exception UnequalVectorSize
exception UnequalMatrixShape
exception IncompatibleMatrixShape
exception SingularMatrix

let rec vdim (v:vector): int = List.length v

let rec mkzerov (n:int): vector =
    let rec aux acc n =
        if n = 0 then
            acc
        else
            aux (0.0 :: acc) (n - 1)
    in
    aux [] n

let rec iszerov (v:vector): bool = match v with
    [] -> true
    | h :: t ->
            if h = 0.0 then
                iszerov t
            else
                false

let rec opv f v1 v2 =
    let rec aux acc v1 v2 = match (v1, v2) with
        ([], []) -> acc
        | ([], _) -> raise UnequalVectorSize
        | (_, []) -> raise UnequalVectorSize
        | (h1 :: t1, h2 :: t2) ->
                aux ((f h1 h2) :: acc) t1 t2
    in
    List.rev (aux [] v1 v2)

let rec addv (v1:vector) (v2:vector): vector = opv ( +. ) v1 v2

let rec scalarmultv (c:float) (v:vector): vector = List.map (fun x -> c *. x) v

let rec dotprodv (v1:vector) (v2:vector): float = List.fold_left ( +. ) 0.0 (opv ( *. ) v1 v2)

(* TODO: implement the n dimensional generalisation! *)
(* https://math.stackexchange.com/questions/185991/is-the-vector-cross-product-only-defined-for-3d *)
(*
let rec crossprodv (v1:vector) (v2:vector): vector =

let rec mdim (m:matrix): int*int =
let rec mkzerom (m_:int) (n_:int): matrix =
let rec iszerom (m:matrix): bool =
let rec mkunitm (m_:int): matrix =
let rec isunitm (m:matrix): bool =
let rec addm (m1:matrix) (m2:matrix): matrix =
let rec scalarmultm (c:float) (m:matrix): matrix = 
let rec multm (m1:matrix) (m2:matrix): matrix =
let rec transm (m:matrix): matrix = 
let rec detm (m:matrix): float =
let rec invm (m:matrix): matrix =
*)
