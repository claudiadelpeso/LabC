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
-- output { 52 }