type vector = float list
type matrix = float list list

exception InvalidInput
exception UnequalVectorSize
exception UnequalMatrixShape
exception IncompatibleMatrixShape
exception SingularMatrix

let rec vdim (v:vector): int =
let rec mkzerov (n:int): vector =
let rec iszerov (v:vector): bool =
let rec addv (v1:vector) (v2:vector): vector =
let rec scalarmultv (c:float) (v:vector): vector = 
let rec dotprodv (v1:vector) (v2:vector): float =
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

