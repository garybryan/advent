module Advent2022.Day10.Base
  ( Instr,
    execInstrs,
    valuesWithTicks,
    noop,
    addx,
    parseLine,
  )
where

-- Represent an instruction as a function to modify the register and a number
-- of remaining ticks. Once the ticks are at zero, the function is executed.
data Instr = Instr Int (Int -> Int)

noop :: Instr
noop = Instr 1 id

addx :: Int -> Instr
addx x = Instr 2 (+ x)

-- Execute a list of instructions, given a starting register value, and give
-- the register value at each tick.
-- When an instruction is on its last tick, pass the resulting register value
-- to the next iteration, to give the effect of executing /after/ the tick.
execInstrs :: Int -> [Instr] -> [Int]
execInstrs n [] = [n]
execInstrs n (Instr 1 f : is) = n : execInstrs (f n) is
execInstrs n (Instr t f : is) = n : execInstrs n (Instr (t - 1) f : is)

valuesWithTicks :: [Instr] -> [(Int, Int)]
valuesWithTicks = zip [1 ..] . execInstrs 1

parseLine :: String -> Instr
parseLine "noop" = noop
parseLine s = addx (read (drop 5 s))
