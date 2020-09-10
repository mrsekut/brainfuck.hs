module Main where

import           Data.Char
import qualified Data.Map   as M
import           Data.Maybe (fromJust)

type Heap = [Int]
type Ptr = Int
type Idx = Int
type Env = (Heap, Ptr, Idx)



main :: IO ()
main = do
  input <- readFile "example.bf"
  let env = ([], 0, 0)
  let bm = braceMap input
  run input (return env) bm
  print '\n'

run :: [Char] -> IO Env -> BraceMap -> IO Env
run input env bm = do
  e <- env
  if length input == thd e
    then return e
    else run input (cmd e (input !! thd e) bm) bm



type Start = Int
type End = Int
type BraceMap = M.Map Start End
type Stack = [Int]


thd :: (a, b, c) -> c
thd (a,b,c) = c


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


cmd :: Env -> Char -> BraceMap -> IO Env
cmd (heap, ptr, idx) '>' bm = return (heap, ptr+1, idx+1)
cmd (heap, ptr, idx) '<' bm = return (heap, ptr-1, idx+1)
cmd (heap, ptr, idx) '+' bm = return (incH heap ptr, ptr, idx+1)
cmd (heap, ptr, idx) '-' bm = return (decH heap ptr, ptr, idx+1)
cmd (heap, ptr, idx) '.' bm = do
  putChar (toEnum (heap !! ptr) :: Char)
  return (heap, ptr, idx+1)
cmd (heap, ptr, idx) ',' bm = do
  input <- getChar
  return $ (update heap ptr (ord $ input), ptr, idx+1)
cmd (heap, ptr, idx) '[' bm
  | heap !! ptr == 0 = return (heap, ptr, fromJust $ M.lookup idx bm)
  | otherwise = return (heap, ptr, idx+1)
cmd (heap, ptr, idx) ']' bm
  | heap !! ptr == 0 = return (heap, ptr, idx+1)
  | otherwise = return (heap, ptr, fromJust $ M.lookup idx bm)
cmd (heap, ptr, idx)  _  bm = return (heap, ptr+1, idx+1)

-- utils


incH :: Heap -> Ptr -> Heap
incH [] ptr
  | ptr == 0 = [1]
  | otherwise = 0: incH [] (ptr - 1)
incH (h:hs) ptr
  | ptr == 0  = (h+1):hs
  | otherwise = h: incH hs (ptr - 1)


decH :: Heap -> Ptr -> Heap
decH [] ptr
  | ptr == 0 = [-1]
  | otherwise = -1: incH [] (ptr - 1)
decH heap ptr = update heap ptr ((heap !! ptr) - 1)


update :: Heap -> Ptr -> Int -> Heap
update heap ptr a = take ptr heap ++ [a] ++ drop (ptr + 1) heap
