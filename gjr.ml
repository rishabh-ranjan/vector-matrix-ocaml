exception SingularMatrix

let zero, one = 0.0, 1.0

let pivot lst =
    let rec aux sel rem fnd = function
    | [] -> sel, rem, fnd
    | (hh::_, _ as h)::t ->
        if not fnd && hh <> zero then aux h rem true t
        else aux sel (h::rem) fnd t
    in aux ([],[]) [] false lst

let map2 f l1 l2 =
    let rec aux acc = function
    | [], [] -> List.rev acc
    | h1::t1, h2::t2 -> aux ((f h1 h2)::acc) (t1, t2)
    in aux [] (l1, l2)

let zip m1 m2 = map2 (fun l1 l2 -> (l1, l2)) m1 m2

let unzipr m = List.map snd m

let mkunitm n =
    let rec mkr acc nc c =
        if nc = 0 then acc
        else mkr ((if c = 0 then one else zero)::acc) (nc-1) (c-1)
    in
    let rec aux acc nr =
        if nr = 0 then acc
        else aux ((mkr [] n (n-nr))::acc) (nr-1)
    in aux [] n

(*let prep f (h::t, l) = (List.map f t), (List.map f l)*)
(*let prep f = function
| h::t, l -> (List.map f t), (List.map f l)*)

(* TODO: handle exceptions *)
let inv m =
    let rec aux top = function
    | [] -> List.rev (unzipr top)
    | _ as bot ->
        let sel, rem, fnd = pivot bot in
        if not fnd then raise SingularMatrix else
        let hs::ts, ls = sel in
        let f h x y = x -. y *. h /. hs in
        let prep (h::t, l) = (map2 (f h) t ts), (map2 (f h) l ls) in
        let nxt = List.map prep in
        let fs x = x /. hs in
        let sel' = (List.map fs ts), (List.map fs ls) in
        aux (sel'::(nxt top)) (nxt rem)
    in aux [] (zip m (mkunitm (List.length m)))

