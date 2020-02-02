exception SingularMatrix

let zero = 0.0
let one = 1.0

let pivot l =
    let rec aux sel rem fnd = function
    | [] -> sel, rem, fnd
    | (hh::_, _ as h)::t ->
        if not fnd && hh <> zero then aux h rem true t
        else aux sel (h::rem) fnd t
    in aux [] [] false l

let map2 f l1 l2 =
    let rec aux acc = function
    | [], [] -> List.rev acc
    | h1::t1, h2::t2 -> aux ((f h1 h2)::acc) (t1, t2)
    in aux [] l1 l2

let zip m1 m2 = map2 (fun l1 l2 -> (l1, l2)) m1 m2

(* works *)
let rec mkunitm n =
    let rec mkr acc n i =
        if n = 0 then acc
        else mkr ((if i = 0 then one else zero)::acc) (n-1) (i-1)
    in
    let rec aux acc m =
        if m = 0 then acc
        else aux ((mkr [] n (n-m))::acc) (m-1)
    in aux [] n

let inv m =
    let rec aux top = function 
    | [] -> top
    | _ as l ->
        let sel, rem, fnd = pivot l in
        if not fnd then raise SingularMatrix else
        let htop::ttop, atop = top in
        let rec prep acc = function
        | [] -> acc
        | (hh::th, a)::t ->
            let f x y = x -. y *. hh /. htop in
            prep (((map2 f th ttop), (map2 f a atop))::acc) t
        in aux (List.rev (sel::(prep [] top)) (List.rev (prep [] rem))
        in aux [] m 
