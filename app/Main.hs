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
      print x
      start x

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
