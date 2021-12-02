module NumericalCoding where

import Text.Show.Functions

data Pair = SinglePair Int Int | DoublePair Int Int deriving (Eq, Show)
data Instruction = Add Int Int | Sub Int Int Int | HALT deriving (Eq, Show)


first :: Pair -> Int
first (SinglePair x _) = x
first (DoublePair x _) = x

second :: Pair -> Int
second (SinglePair _ y) = y
second (DoublePair _ y) = y

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