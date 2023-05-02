-- val processes [n] : (xs : [n]i32) -> (ys: [n]i32) -> i32 
-- map2 applies a binary function to corresponding elements of two input arrays to produce a new array (in parallel)
-- reduce applies a binary function (in parallel) to pairs of adjacent elements in an array to produce a single value. It has accumulator

def max(x: i32, y:i32) : i32 = 
    if x>=y then x else y 

def abs(x:i32) : i32 = 
    if x>=0 then x else -x 

entry process [n] (xs: [n]i32) (ys: [n]i32) : i32 =
    let diffs = map2 (\x y -> abs(x - y)) xs ys
    let max_diff = reduce (\x y -> max(x, y)) 0 diffs
    let result =
        if n == 0 then 0 else max_diff
    in
        result

-- Test block for process function.
-- ==
-- entry: process
-- input { [23,45,-23,44,23,54,23,12,34,54,7,2,4,67] 
--         [-2,3,4,57,34,2,5,56,56,3,3,5,77,89] }
-- output { 73 }


-- Benchmarking process function 
-- ==
-- entry: process
-- compiled input @ i32_100.in
-- compiled input @ i32_1000.in
-- compiled input @ i32_10000.in
-- compiled input @ i32_100000.in
-- compiled input @ i32_1000000.in
-- compiled input @ i32_5000000.in
-- compiled input @ i32_10000000.in


let max_idx (d1: i32, i1: i64) (d2: i32, i2: i64): (i32,i64) =
    if d1 >= d2 then (d1,i1) else (d2,i2)


def process_idx [n] (xs: [n]i32)  (ys: [n]i32) : (i32, i64) = 
    let diffs =
        reduce_comm max_idx( 0 , -1)
                    (zip (map i32.abs ( map2 (-) xs ys ))
                    (iota n))
        let result =
            if n == 0 then (0,-1) else diffs
        in
            result


entry segscan [n] 't (op: t -> t -> t) (ne: t)
                   (arr: [n](t, bool)): *[n]t = 
  let combine_segments (x: (t, bool)) (y: (t, bool)): (t, bool) =
    let (v1, f1) = x
    let (v2, f2) = y
    in (if f2 then v2 else op v1 v2, f1 || f2)

  let combined_scan = scan combine_segments (ne, false) arr    
  let result = map (\(v, _) -> v) combined_scan
  in result

-- Test block for segscan functions.
-- ==
-- entry: segscan
-- input { (+) 
           0 
           [(1,true),(2,false),(3,true),(4,false),(5,true),(6,false)] }
-- output { [1,3,3,7,5,11] }

