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

entry segscan_addition_zip [n] (vals: [n]i32) (flags: [n]bool): *[n]i32 =
  let arr = zip vals flags
  in segscan (+) 0 arr

-- Test block for segscan function.
-- ==
-- entry: segscan_addition_zip
-- input { [1, 2, 3, 4, 5, 6] [true, false, true, false, true, false] }
-- output { [1, 3, 3, 7, 5, 11] }

def segreduce [n] 't (op: t -> t -> t) (ne: t) (arr: [n](t, bool)): *[]t = 
    let scanned = segscan op ne arr 
    let flags = map (\(_, flag) -> flag) arr
    let flags' = rotate 1 flags
    let flags_int = map i64.bool flags'
    let offsets = scan (+) 0 flags_int
    let put_in i f = if f then i-1 else -1
    let is = map2 put_in offsets flags'
    let as' = scatter (copy scanned) is scanned
    in take (offsets[n-1]) as'

entry segreduce_addition [n] (vals: [n]i32) (flags: [n]bool): *[]i32 =
  let arr = zip vals flags
  in segreduce (+) 0 arr


-- Test block for segreduce function.
-- ==
-- entry: segreduce_addition
-- input { [1, 2, 3, 4, 5, 6] [true, false, true, false, true, false] }
-- output { [3, 7, 11] }


-- Benchmarking scan function 
-- ==
-- entry: segscan_addition_zip 
-- compiled input @ seg_1000000.in
-- compiled input @ seg_5000000.in
-- compiled input @ seg_10000000.in

-- Benchmarking reduce function 
-- ==
-- entry: segreduce_addition 
-- compiled input @ seg_1000000.in
-- compiled input @ seg_5000000.in
-- compiled input @ seg_10000000.in


entry regreduce [n] (vals: [n]i32) : i32 = reduce (+) 0 vals
entry regscan [n] (vals: [n]i32) : *[]i32 = scan (+) 0 vals


-- Benchmarking regular reduce function 
-- ==
-- entry: regreduce
-- compiled input @ reg_1000000.in
-- compiled input @ reg_5000000.in
-- compiled input @ reg_10000000.in

-- Benchmarking regscan function 
-- ==
-- entry: regscan
-- compiled input @ reg_1000000.in
-- compiled input @ reg_5000000.in
-- compiled input @ reg_10000000.in



