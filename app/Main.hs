module Main where

import           Control.Monad.State
import           Data.Char
import qualified Data.Map            as M
import           Data.Maybe          (fromJust)


-- Instructions

data BF = Next    -- >
        | Prev    -- <
        | Inc     -- +
        | Dec     -- -
        | Out     -- .
        | In      -- ,
        | LStart  -- [
        | LEnd    -- ]
        | Other
        deriving (Show)


-- | Env

type Heap = [Int]
type Ptr = Int
type Idx = Int
data Env = Env {
  heap :: Heap,
  ptr  :: Ptr,
  idx  :: Idx
} deriving (Show)

prev, next :: Env -> Env
prev env = env { ptr = ptr env - 1 }
next env = env { ptr = ptr env + 1 }

appCur :: (Int -> Int) -> Env -> Env
appCur f env = env { heap = update env (f $ cur env)}

cur :: Env -> Int
cur env = heap env !! ptr env

update :: Env -> Int -> Heap
update env n = take ptr' heap' ++ [n] ++ drop (ptr' + 1) heap'
  where
    ptr' = ptr env
    heap' = heap env


-- | BraceMap

type Start = Int
type End = Int
type BraceMap = M.Map Start End
type Stack = [Int]

braceMap :: String -> BraceMap
braceMap input = makeStack input [] 0 M.empty
  where
    makeStack :: [Char] -> Stack -> Int -> BraceMap -> BraceMap
    makeStack [] _ _ bm = bm
    makeStack (x:xs) stack idx bm
      | x == '[' = makeStack xs (push idx stack) (idx+1) bm
      | x == ']' = makeStack xs (snd a) (idx+1) (M.insert idx (fst a) (M.insert (fst a) idx bm) )
      | otherwise = makeStack xs stack (idx+1) bm
        where
          a = pop stack

    pop :: Stack -> (Int, Stack )
    pop  (x:xs) = (x, xs)

    push :: Int -> Stack -> Stack
    push a stack = stack ++ [a]


-- | Parser

-- >>> parser "<]>.+"
-- [Prev,LEnd,Next,Out,Inc]
parser :: String -> [BF]
parser []     = []
parser (s:ss) = lexer s: parser ss
  where
    lexer :: Char -> BF
    lexer '>' = Next
    lexer '<' = Prev
    lexer '+' = Inc
    lexer '-' = Dec
    lexer '.' = Out
    lexer ',' = In
    lexer '[' = LStart
    lexer ']' = LEnd
    lexer _   = Other


-- | Eval

addIdx :: StateT Env IO ()
addIdx = do
  env <- get
  put env { idx = idx env + 1}

modifyIndex ::  (Env -> Env) -> StateT Env IO ()
modifyIndex f = do
  modify f
  addIdx

eval :: BF -> BraceMap -> StateT Env IO ()
eval Next _    = modifyIndex next
eval Prev _    = modifyIndex prev
eval Inc  _    = modifyIndex $ appCur (+1)
eval Dec  _    = modifyIndex $ appCur (subtract 1)
eval Out  _    = do
  liftIO . putChar . toEnum . cur =<< get
  addIdx
eval In   _    = do
  input <- lift getChar
  modifyIndex $ appCur (\_ -> ord input)
eval LStart bm = do
  env <- get
  case cur env of
    0 -> put env { idx = fromJust $ M.lookup (idx env) bm }
    _ -> addIdx
eval LEnd bm   = do
  env <- get
  case cur env of
    0 -> addIdx
    _ -> put env { idx = fromJust $ M.lookup (idx env) bm }
eval Other _   = return ()


-- | Main

main :: IO ((), Env)
main = do
  input <- readFile "example.bf"
  let ast = parser input
  -- let env = Env { heap = replicate 20 0, ptr = 0, idx = 0} -- for debug
  let env = Env { heap = [0,0..], ptr = 0, idx = 0}
  let bm = braceMap input
  runStateT (run ast bm) env


run :: [BF] -> BraceMap -> StateT Env IO ()
run input bm = do
  env <- get
  let bf = input !! idx env
  if length input == idx env
    then put env
    else do
      eval bf bm
      run input bm


-- | Debug

debug :: String -> StateT Env IO ()
debug s = do
  env <- get
  liftIO $ print s
  liftIO $ print env

