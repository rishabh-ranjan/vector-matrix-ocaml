let det m =
    let rec aux n res = function
    | [] -> res
    | (hh::_ as h)::t as l ->
        let top, rem, fnd = if hh <> 0.0 then h, t, true else
            let rec pivot top rem fnd = function
            | [] -> top, rem, fnd
            | (hh::_ as h)::t ->
                if not fnd && hh <> 0.0 then pivot h rem true t
                else pivot top (h::rem) fnd t
            in pivot [] [] false l
        in
        if not fnd then 0.0 else
        let htop::ttop = top in
        let rec pmat acc = function
        | [] -> acc
        | (hh::th)::t ->
            let mf = hh /. htop in
            let rec prow acc lst top = match lst, top with
            | [], [] -> acc
            | hl::tl, ht::tt -> prow ((hl -. ht *. mf)::acc) tl tt
            in pmat ((prow [] th ttop)::acc) t
        in
        let neg = if ((n - 1)/2 mod 2 = 1) <> (hh = 0.0) then -1.0 else 1.0 in
        aux (n - 1) (neg *. res *. htop) (pmat [] rem)
    in aux (List.length m) 1.0 m

