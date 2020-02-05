let det m =
    let rec aux res = function
    | [] -> res
    | (hh::_ as h)::t as l ->
        let top, rem, fnd = if hh <> 0.0 then h, t, true else
            let rec pivot top rem fnd = function
            | [] -> top, rem, fnd
            | (hh::_ as h)::t ->
                if not fnd && hh <> 0.0 then pivot h rem true t
                else pivot top (h::rem) fnd t
            (*| _ -> raise InvalidInput*)
            in pivot [] [] false l 
        in
        if not fnd then 0.0 else
        let tel = List.hd top in
        let rec prep acc = function
        | [] -> acc
        | h::t ->
            let mf = 1.0 in
            let rec prow acc = function
            | [], [] -> acc
            | vh::vt, ht::tt -> prow ((vh -. ht *. mf)::acc) (vt, tt)
            in prep ((prow [] ([1.], top))::acc) t
        in aux (res *. tel) (prep [] [[1.]])
    in aux 1.0 m

    (*
        let rec prep acc = function
        | [] -> acc
        | h::t ->
            let mf = 1.0 in
            let rec prow acc = function
            | [], [] -> acc
            | h::t, ht::tt -> prow ((h -. ht *. mf)::acc) (t, tt)
            in prep ((prow [] (h, top))::acc) t
            *)
