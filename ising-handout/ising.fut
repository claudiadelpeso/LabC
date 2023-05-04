-- We represent a spin as a single byte.  In principle, we need only
-- two values (-1 or 1), but Futhark represents booleans a a full byte
-- entirely, so using an i8 instead takes no more space, and makes the
-- arithmetic simpler.
type spin = i8

import "lib/github.com/diku-dk/cpprandom/random"

-- Pick an RNG engine and define random distributions for specific types.
module rng_engine = minstd_rand
module rand_f32 = uniform_real_distribution f32 rng_engine
module rand_i8 = uniform_int_distribution i8 rng_engine

-- We can create an few RNG state with 'rng_engine.rng_from_seed [x]',
-- where 'x' is some seed.  We can split one RNG state into many with
-- 'rng_engine.split_rng'.
--
-- For an RNG state 'r', we can generate random integers that are
-- either 0 or 1 by calling 'rand_i8.rand (0i8, 1i8) r'.
--
-- For an RNG state 'r', we can generate random floats in the range
-- (0,1) by calling 'rand_f32.rand (0f32, 1f32) r'.
--
-- Remember to consult
-- https://futhark-lang.org/pkgs/github.com/diku-dk/cpprandom/latest/

def rand = rand_f32.rand (0f32, 1f32)

-- Create a new grid of a given size.  Also produce an identically
-- sized array of RNG states.
def random_grid (seed: i32) (h: i64) (w: i64)
              : ([h][w]rng_engine.rng, [h][w]spin) =
  let flat_size = h*w
  let new_state = rng_engine.rng_from_seed [seed]
  let split = rng_engine.split_rng flat_size new_state
  let rgrid = unflatten h w split
  let empty = replicate h (replicate w 0i8)
  in (rgrid,empty)


-- [[1, 2, 3],
--  [4, 5, 6],
--  [7, 8, 9]]

--Left
-- [[3, 1, 2], 
--  [6, 4, 5], 
--  [9, 7, 8]]

-- Right
-- [[2, 3, 1], 
--  [5, 6, 4], 
--  [8, 9, 7]]

-- Top 
-- [[7, 8, 9], 
--  [1, 2, 3], 
--  [4, 5, 6]]

-- Bottom
-- [[4, 5, 6], 
--  [7, 8, 9], 
--  [1, 2, 3]]

-- Compute ∆e for each spin in the grid, using wraparound at the edges.
entry deltas [h][w] (spins: [h][w]spin): [h][w]i8 =
  let left = map (rotate (-1)) spins
  let right = map (rotate 1) spins  
  let up = rotate (-1) spins        
  let down = rotate 1 spins         
  let final = map ( \i -> map5 (\c l r u d -> 2*c *(l+r+u+d)) spins[i] left[i] right[i] up[i] down[i])  (iota h)
  in final 

-- The sum of all deltas of a grid.  The result is a measure of how
-- ordered the grid is.
def delta_sum [h][w] (spins: [w][h]spin): i32 =
  let delta_sum = reduce (+) 0 (flatten (deltas spins))
  in i32.i8 delta_sum



-- def compute_c  (delta_e:i8) (c:spin) (t: f32) (p: f32) ((rng: rng_engine.rng)): spin = 
def compute_c  (delta_e:i8) (c:i8) (t: f32) (p: f32) ((rng: rng_engine.rng)): spin = 
    let splitted = rng_engine.split_rng 2 rng
    let splitValues = map (\x -> rand_f32.rand (0f32, 1f32) x) splitted

    let (_,a) = splitValues[0]
    let (_,b) = splitValues[1]

    in if a < p && (delta_e < (-delta_e) || b < f32.exp ((f32.i8 (-delta_e))/t)) then (-c)
    else c

-- Take one step in the Ising 2D simulation.
def step [h][w] (abs_temp: f32) (samplerate: f32)
                (rngs: [h][w]rng_engine.rng) (spins: [h][w]spin)
              : ([h][w]rng_engine.rng, [h][w]spin) = 
    let flat_size = h*w
    let flatR = flatten rngs :> [flat_size]rng_engine.rng
    let flatS = flatten spins :> [flat_size]spin
    let flatD = flatten (deltas spins) :> [flat_size]i8
    let zipped = zip3 flatR flatS flatD 
    let flatRes = map (\(r,c,d) -> compute_c d c abs_temp samplerate r ) zipped
    let res = unflatten h w flatRes
    in (rngs,res)
    
  
-- | Just for benchmarking.
def main (abs_temp: f32) (samplerate: f32)
         (h: i64) (w: i64) (n: i32): [h][w]spin =
  (loop (rngs, spins) = random_grid 1337 h w for _i < n do
     step abs_temp samplerate rngs spins).1

-- ==
-- entry: main
-- input { 0.5f32 0.1f32 10i64 10i64 2 } auto output

-- The following definitions are for the visualisation and need not be modified.

type~ state = {cells: [][](rng_engine.rng, spin)}

entry tui_init seed h w : state =
  let (rngs, spins) = random_grid seed h w
  in {cells=map (uncurry zip) (zip rngs spins)}

entry tui_render (s: state) = map (map (.1)) s.cells

entry tui_step (abs_temp: f32) (samplerate: f32) (s: state) : state =
  let rngs = (map (map (.0)) s.cells)
  let spins = map (map (.1)) s.cells
  let (rngs', spins') = step abs_temp samplerate rngs spins
  in {cells=map (uncurry zip) (zip rngs' spins')}