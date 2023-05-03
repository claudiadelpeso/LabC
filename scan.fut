import "lib/github.com/diku-dk/sorts/radix_sort"

-- Exercise 2.2
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


def segreduce [n] 't (op: t -> t -> t) (ne: t) (arr: [n](t, bool)): *[]t = 
-- def segreduce [n] (op: i32 -> i32 -> i32) (ne: i32) (arr: [n](i32, bool)): *[]i32 = 
    let scanned = segscan op ne arr 
    let mapIdx = map (\((_,f1),i) -> if f1 && i>0 then i-1 else -1) (zip arr (iota n)) ++ [n-1]
    let idx = filter (\x -> x!=(-1)) mapIdx
    let values = map (\i -> scanned[i]) idx
    -- let endArr = replicate n ne
    -- let scattered = scatter endArr idx values
    -- let res = filter (\x -> x != ne ) scattered
    -- let res = filter (\x -> neq x  ne ) scattered
    in values


entry segreduce_test [n] (vals: [n]i32) (flags: [n]bool): *[]i32 =
  let arr = zip vals flags
  in segreduce (+) 0 arr


-- Test block for entry 
-- == 
-- entry: segreduce_test
-- input {[1, 2, 3, 4, 5, 6] [true, false, true, false, true, false]}
-- output {[3,7,11]}

-- Benchmarking segmentation reduction function 
-- ==
-- entry: segreduce_test
-- compiled input @ segmentation_100.in
-- compiled input @ segmentation_1000.in
-- compiled input @ segmentation_10000.in
-- compiled input @ segmentation_100000.in
-- compiled input @ segmentation_1000000.in

def reduce_normal [n] 't (op: t -> t -> t) (ne: t) (arr: [n]t): t =
  reduce op ne arr

entry reduce_normal_test [n] (vals: [n]i32): i32 =
    reduce_normal (+) 0 vals

-- Benchmarking normal reduction function 
-- ==
-- entry: reduce_normal_test
-- compiled input @ normal_100.in
-- compiled input @ normal_1000.in
-- compiled input @ normal_10000.in
-- compiled input @ normal_100000.in
-- compiled input @ normal_1000000.in

-- Exercise 2.3 


def hist 'a [n] (op : a -> a -> a) (ne : a) (k: i64) (is : [n]i64) (as : [n]a) : [k]a = 
    let sorted_is = radix_sort_by_key (\i -> i) 64 (\_ x -> x) as  -- In this case, the key function (\_ x -> 0) ignores the first argument _, which is the index of the bit being considered, and always returns 0 as the key for all elements. This means that all elements are treated as if they had the same key, so they are sorted based on their original order.
    let flags = map2 (\i x -> if i == 0 || x != sorted_is[i-1] then true else false) (iota n) sorted_is
    let combined_arr = map (\i -> (sorted_is[i], flags[i])) (iota n)
    let res = segreduce op ne combined_arr
    in res


    -- let flags = map2 (\a b -> if a != b then 1 else 0) sorted_is (rotate 1 sorted_is)
