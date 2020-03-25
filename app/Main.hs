-- Tic-tac-toe example from chapter 11 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

-- Basic declarations
module Main where

import Data.Char
import Data.List
import System.IO
import Control.Parallel
import Control.Parallel.Strategies
import qualified Data.Array.Repa as R
import Control.Monad.State

size :: Int
size = 9

data Game = Game { o'pos :: Pos,
                   x'pos :: Pos,
                   o'wall :: Int,
                   x'wall :: Int,
                   walls :: [Wall_Pos],
                   turn :: Player
                 }

type Pos = (Int, Int)
  
type Wall_Pos = (Dir, Pos)

type Board = [[Maybe Player]]

type Walls = [[String]]

data Dir = H | V
  deriving Show

data Player = O | X
  deriving Show


initGame :: Game
initGame = Game (0, 4) (8, 4) 10 10 [] O

emptyBoard :: Board
emptyBoard = replicate size $ replicate size Nothing

emptyWalls :: Walls
emptyWalls = replicate (size-1) $ replicate size "."

-- Display a Board

insertPlayer :: Board -> Pos -> Player -> Board
insertPlayer b (y,x) p = xs ++ [(xs' ++ [Just p] ++ ys')] ++ (drop 1 ys)
  where (xs,ys) = splitAt y b
        (xs',Nothing:ys') = splitAt x (head ys) 

showBoard :: Board -> IO ()
showBoard b = putStrLn $ unlines $ map concat $ map (interleave "|") xs
  where xs = take size (map (map showPlayer) b)
        
vWallsInsert :: Walls -> [Wall_Pos] -> Walls
vWallsInsert w [] = w
vWallsInsert w ((H,_):xs) = vWallsInsert w xs
vWallsInsert w ((V,(y,x)):xs) = vWallsInsert (xs' ++ [xs''++ ["|","|"]++ys''] ++ (drop 1 ys')) xs
  where (xs',ys') = splitAt x w
        (xs'',".":".":ys'') = splitAt y (head ys')

convertWalls :: Walls -> [String]
convertWalls w = foldr f [] w
  where f [] y = y
        f (x:xs) y = x:y


        
  
-- Displaying a Game
-- putGame :: Game -> IO ()
-- putGame =
-- putStrLn $ unlines $ (replicate 3 $ concat $ interleave "." $ replicate 9 "   ") ++ [concat $ replicate 9 "...."]


-- State Pos Pos Player

-- type Grid = [[Player]]

-- data Player = O | B | X
--               deriving (Eq, Ord, Show)

-- genInitState :: State
-- genInitState = State (0,4) (8,4) O

-- empty :: Grid 
-- empty = (insertPlayer O blankLine) : foldr (:) [insertPlayer X blankLine]  (replicate (size-1) blankLine)
--   where blankLine = replicate size B

-- insertPlayer :: Player -> [Player] -> [Player]
-- insertPlayer p xs = xs' ++ [p] ++ ys
--   where spAt = length xs `div` 2
--         xs' = take spAt xs
--         B:ys = drop spAt xs

-- next :: Player -> Player
-- next O = X
-- next B = B
-- next X = O

-- -- Grid utilities

-- full :: Grid -> Bool
-- full = all (/= B) . concat

-- turn :: Grid -> Player
-- turn g = if os <= xs then O else X
--          where
--             os = length (filter (== O) ps)
--             xs = length (filter (== X) ps)
--             ps = concat g

-- wins :: Player -> Grid -> Bool
-- wins O g = any (== O) $ head g
-- wins X g = any (== X) $ last g
-- line (rows ++ cols ++ dias)
--            where
--               line = all (== p)
--               rows = g
--               cols = transpose g
--               dias = [diag g, diag (map reverse g)]

-- diag :: Grid -> [Player]
-- diag g = [g !! n !! n | n <- [0..size-1]]

-- won :: Grid -> Bool
-- won g = wins O g || wins X g

-- -- Displaying a grid

-- putGrid :: Grid -> IO ()
-- putGrid =
--    putStrLn . unlines . concat . interleave bar . map showRow
--    where bar = [replicate ((size*4)-1) '.']

-- showRow :: [Player] -> [String]
-- showRow = beside . interleave bar . map showPlayer
--           where
--              beside = foldr1 (zipWith (++))
--              bar    = replicate 3 "."

showPlayer :: Maybe Player -> String
showPlayer (Just p) = " " ++ show p ++ " "
showPlayer Nothing = "   "

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

-- -- -- Making a move

-- commands :: [Char]
-- commands = ['h', 'j', 'k', 'l']

-- valid :: State -> Char -> Player -> Bool
-- valid g c = any (==c) commands && 0 <= i && i < size^2 && concat g !! i == B
--   where 

-- move:: Grid -> Char -> Player -> [Grid]
-- move g c p =
--    if valid g c p then [chop size (xs ++ [p] ++ ys)] else []
--    where (xs,B:ys) = splitAt (i*size+j) (concat g)

-- chop :: Int -> [a] -> [[a]]
-- chop n [] = []
-- chop n xs = take n xs : chop n (drop n xs)

-- -- -- Reading a natural number

-- getCommand :: String -> IO Int
-- getCommand prompt = do putStr prompt
--                        xs <- getLine
--                        if xs /= [] && all isAlpha xs then
--                          return (read xs)
--                        else
--                          do putStrLn "ERROR: Invalid number"
--                             getCommand prompt

-- -- -- Human vs human

-- -- tictactoe :: IO ()
-- -- tictactoe = run empty O 

main :: IO ()
main = print 1--run empty O

-- run :: Grid -> Player -> IO ()
-- run g p = do cls
--              goto (1,1)
--              putGrid g
--              run' g p

-- run' :: Grid -> Player -> IO ()
-- run' g p | wins O g  = putStrLn "Player O wins!\n"
--          | wins X g  = putStrLn "Player X wins!\n"
--          | otherwise =
--               do i <- getNat (prompt p)
--                  case move g i p of
--                     []   -> do putStrLn "ERROR: Invalid move"
--                                run' g p
--                     [g'] -> run g' (next p)

-- prompt :: Player -> String
-- prompt p = "Player " ++ show p ++ ", enter your move: "

-- cls :: IO ()
-- cls = putStr "\ESC[2J"

-- goto :: (Int,Int) -> IO ()
-- goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

  
-- -- Game trees

-- data Tree a = Node a [Tree a]
--               deriving Show

-- gametree :: Grid -> Player -> Tree Grid
-- gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

-- countStates :: Tree Grid -> Int
-- countStates (Node g []) = 1
-- countStates (Node g ts) = foldr (+) 1 $ map countStates ts
                              

-- moves :: Grid -> Player -> [Grid]
-- moves g p | won g     = []
--           | full g    = []
--           | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

-- prune :: Int -> Tree a -> Tree a
-- prune 0 (Node x _)  = Node x []
-- prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

-- depth :: Int
-- depth = 9

-- -- Minimax

-- minimax :: Tree Grid -> Tree (Grid,Player)
-- minimax (Node g [])
--    | wins O g  = Node (g,O) []
--    | wins X g  = Node (g,X) []
--    | otherwise = Node (g,B) []
-- minimax (Node g ts) 
--    | turn g == O = Node (g, minimum ps) ts'
--    | turn g == X = Node (g, maximum ps) ts'
--                    where
--                       ts' = map minimax ts
--                       ps  = [p | Node (_,p) _ <- ts']

-- bestmove :: Grid -> Player -> Grid
-- bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
--                where 
--                   tree = prune depth (gametree g p)
--                   Node (_,best) ts = minimax tree

-- -- Human vs computer

-- main :: IO ()
-- main = do hSetBuffering stdout NoBuffering
--           play empty O

-- main :: IO ()
-- main = do
--   print 1

-- play :: Grid -> Player -> IO ()
-- play g p = do cls
--               goto (1,1)
--               putGrid g
--               play' g p

-- play' :: Grid -> Player -> IO ()
-- play' g p
--    | wins O g = putStrLn "Player O wins!\n"
--    | wins X g = putStrLn "Player X wins!\n"
--    | full g   = putStrLn "It's a draw!\n"
--    | p == O   = do i <- getNat (prompt p)
--                    case move g i p of
--                       []   -> do putStrLn "ERROR: Invalid move"
--                                  play' g p
--                       [g'] -> play g' (next p)
--    | p == X   = do putStr "Player X is thinking... "
--                    (play $! (bestmove g p)) (next p)


