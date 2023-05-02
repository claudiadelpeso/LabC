let combine 't (op: t -> t -> t) (x: (t, bool)) (y: (t, bool)): (t, bool) =
  let (v1, f1) = x
  let (v2, f2) = y
  let v = if f2 then v2 else op v1 v2
  let f = f1 || f2
  in (v, f)


def segscan [n] 't (op: t -> t -> t) (ne: t) (arr: [n](t, bool)): *[n]t =
  let accumulated = scan (combine op) (ne,false) arr
  let res = map (\(v,_) -> v) accumulated
  in res



-- Test block for segscan functions.
-- ==
-- entry: segscan
-- compiled input @ test.in
-- output { [1,3,3,7,5,11] }

def segreduce [n] 't (op: t -> t -> t) (ne: t) (arr: [n](t, bool)): *[]t = 
    let scanned = segscan op ne arr 
    let mapIdx = map (\((v1,f1),i) -> if f1 && i>0 then i-1 else -1) (zip arr (iota n)) ++ [n-1]
    let idx = filter (\x -> x==(-1) ) mapIdx
    let endArr = replicate (length idx) 0 
    let scattered = scatter endArr idx scanned
    in scattered

-- [(1,true), (2,true), (3,true), (4, false), (5,true)]