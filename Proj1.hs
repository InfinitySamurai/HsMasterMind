--Written by Russell Long: 576494
--Purpose: for Project one in the subjec declarative programming.
--This program plays out an AI for a game similar to the board game "Mastermind"

module Proj1 (initialGuess, nextGuess, GameState) where
import Data.List


type Pitch = String
type Guess = [Pitch]
type GameState = [[String]]

pitches = 
	["A1", "A2", "A3"
	,"B1", "B2", "B3"
	,"C1", "C2", "C3"
	,"D1", "D2", "D3"
	,"E1", "E2", "E3"
	,"F1", "F2", "F3"
	,"G1", "G2", "G3"]


initialGuess :: ([String], GameState)
initialGuess = (["A1", "B2", "C3"], (sampleSpace pitches pitches pitches))


nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess (prevGuess, oldState) newResponse = (newGuess, newState)
		where 
			newState = cullList prevGuess newResponse oldState
			newGuess = head newState




--I essentially wrote the permutations function not realising that it existed.
--This one is better for this application though because it doesn't have equivalent
--repeats e.g. ["A1", "G3", "F2"] == ["G3", "F2", "A1"]
sampleSpace :: [String] -> [String] -> [String] -> GameState
sampleSpace [] _ _ = []
sampleSpace (a:as) [] [] = sampleSpace as as as
sampleSpace (a:as) (b:bs) [] = sampleSpace (a:as) bs (bs)
sampleSpace (a:as) [] (c:cs) = sampleSpace as pitches pitches
sampleSpace (a:as) (b:bs) (c:cs)
	| a == b 	= sampleSpace (a:as) bs cs
	| b == c 	= sampleSpace (a:as) (b:bs) cs
	| a == c 	= sampleSpace (a:as) (b:bs) cs
	| otherwise	=  [[a] ++ [b] ++ [c]] ++ sampleSpace (a:as) (b:bs) cs


cullList :: Guess -> (Int, Int, Int) -> GameState -> GameState
cullList _ _ [] = []
cullList guess response (s:ss)
	| equalTuple (compareGuess guess s) response 	= s:(cullList guess response ss)
	| otherwise 									= (cullList guess response ss)

-- inspired by Proj1Test.hs from the supplied files
-- I used the function to finish the project, and then the announcement
-- came out to not use it, so i deleted all the code after the where statement
-- to re write it.
compareGuess :: [String] -> [String] -> (Int,Int,Int)
compareGuess target guess = (right, rightNote, rightOctave)
	where 
		right = length(intersect target guess)
		rightNote = checkCorrect (map (!!0) target) (map (!!0) guess) - right 
		rightOctave = checkCorrect (map (!!1) target) (map (!!1) guess) - right 



checkCorrect :: String -> String -> Int
checkCorrect a b
	| (sort a) == (sort b)				= 3
	| length(nub a) <= length(nub b)	= length(intersect (nub b) (nub a))
	| otherwise							= length(intersect (nub a) (nub b))




--checks if two triple value tuples are equal
equalTuple :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
equalTuple (a, b, c) (d, e, f) = a == d && b ==e && c == f



