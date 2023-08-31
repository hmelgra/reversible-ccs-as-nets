module Main (main) where

import System.Environment

import Net (enabled, fire, Net(netMarking) )
import Encoding ( enc )
import Ccs ( Action(In, Out), CCS(..) )
import Data.List ( sort )
import ReversibleNet ( rev )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ccs'] -> do
      let x = (read ccs' :: CCS Int)
      start ccs

start :: CCS Int -> IO()
start ccs = do
    simulate $ rev (enc ccs)


simulate :: (Show s, Show t, Ord t, Eq s) => Net s t -> IO ()
simulate net = do
    putStrLn $ "\nMarking: " ++ show (netMarking net)
    let ready = sort (enabled net)
    if null ready
      then putStrLn "No enabled transition. Terminating!"
      else do
        putStrLn "Enabled transitions: "
        putStr $ unlines  (zipWith (\n line -> show n ++ ")  " ++ show line)  [1..] ready)
        putStr "transition to fire? (0 to exit): "
        numberString <- getLine
        let input = reads numberString :: [(Int,String)]
        if null input || fst (head input) > length ready
          then do
            putStrLn "\n*********Invalid transition********\n"
            simulate net
          else do
            let number = fst (head input)
            if number > 0
              then do
                let new = fire (ready!!(number-1)) net
                simulate new
              else
                putStrLn "Bye"


ccs1 :: CCS Int
ccs1 = Rec "X" (In 1 :. Var "X")

ccs2 :: CCS Int
ccs2 = Rec "X" ((Out 2 :. Var "X") :+ (Out 1 :. Nil))

ccs :: CCS Int
ccs = (ccs1 :| ccs2) :\ 1

ccs3 :: CCS Int
ccs3 = In 1 :. Out 2 :. Nil

ccs4 :: CCS Int
ccs4 = Out 1 :. In 2 :. Nil
