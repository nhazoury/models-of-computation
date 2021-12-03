module NumericalCoding where

import Text.Show.Functions
import Data.Sequence

-- DATA TYPES --

data Pair = SinglePair Int Int | DoublePair Int Int deriving (Eq, Show)
data Instruction = Add Int Int | Sub Int Int Int | HALT deriving (Eq, Show)

first :: Pair -> Int
first (SinglePair x _) = x
first (DoublePair x _) = x

second :: Pair -> Int
second (SinglePair _ y) = y
second (DoublePair _ y) = y

-- BASIC ENCODING/DECODING --

doubleEncode :: Int -> Int -> Int
doubleEncode x y = 2 ^ x * (2 * y + 1)

singleEncode :: Int -> Int -> Int
singleEncode x y = doubleEncode x y - 1

doubleDecode :: Int -> Pair
doubleDecode val = DoublePair x y
    where
        x  = divsByTwo val 0
        x' = div val (2 ^ x)
        y  = div (x' - 1) 2

singleDecode :: Int -> Pair
singleDecode val = SinglePair x y
    where
        val' = val + 1
        x  = divsByTwo val' 0
        x' = div val' (2 ^ x)
        y  = div (x' - 1) 2


divsByTwo :: Int -> Int -> Int
divsByTwo x count
    | x `mod` 2 == 1 = count
    | otherwise      = divsByTwo (div x 2) (count + 1)

-- PROGRAMS --

decodeInstruction :: Int -> Instruction
decodeInstruction 0 = HALT
decodeInstruction x
    | even a = Add p b
    | odd  a = Sub p r s
        where
            x' = doubleDecode x
            a = first x'
            b = second x'
            p = div a 2
            q = singleDecode b
            r = first q
            s = second q

decodeProgram :: Int -> [Instruction]
decodeProgram 0 = []
decodeProgram x = y' : decodeProgram z
    where
        x' = doubleDecode x
        y = first x'
        y' = decodeInstruction y
        z = second x'

encodeInstruction :: Instruction -> Int
encodeInstruction HALT = 0
encodeInstruction (Add x y) = doubleEncode (2 * x) y
encodeInstruction (Sub x y z) = doubleEncode (2 * x + 1) (singleEncode y z)

-- EXECUTION --

incrementInList :: [Int] -> Int -> [Int]
incrementInList (x : xs) 0 = (x + 1) : xs
incrementInList (x : xs) n = x : (incrementInList xs (n - 1))

decrementInList :: [Int] -> Int -> [Int]
decrementInList (x : xs) 0 = (x - 1) : xs
decrementInList (x : xs) n = x : (decrementInList xs (n - 1))

executeProgram :: [Instruction] -> Int -> [Int] -> [Int]
executeProgram is n rs = if end then rs' else executeProgram is n' rs'
    where
        (n', rs', end) = executeInstr i rs
        i = is !! n
        executeInstr :: Instruction -> [Int] -> (Int, [Int], Bool)
        executeInstr HALT regs = (0, regs, True)
        executeInstr (Add x y) regs = (y, incrementInList regs x, False)
        executeInstr (Sub x y z) regs = if regs !! x > 0 then (y, decrementInList regs x, False) else (z, regs, False)

execute :: Int -> [Int]
execute val = executeProgram prog 0 setup
    where
        prog = decodeProgram val
        setup = [0, 0, 0, 0, 0, 0]

executeProg :: [Instruction] -> [Int]
executeProg is = executeProgram is 0 setup
    where
        setup = [0, 0, 0, 0, 0, 0]