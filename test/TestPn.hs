{-# OPTIONS_GHC -F -pgmF htfpp #-}


module TestPn (htf_thisModulesTests) where

import Test.Framework
import qualified Data.List           as L

import Net

--------------
-- EXAMPLES --
--------------
n' :: Net Int Char
n' = Net p t m
  where
    p = const [1, 2, 3, 4, 5]
    t = const [
          Transition 'a' [1,2] [4]
        , Transition 'b' [2,3] [4]
        , Transition 'c' [4]   [5]
        ]
    m = [1, 2, 3]

nAt :: [Int] -> Net Int String
nAt = Net p t
  where
    p [] = []
    p m = [0 .. maximum m + 1]

    t [] = []
    t m  = [Transition (replicate i 'a') [i - 1] [i] |
            i <- [1 .. maximum m + 1]]


-- n :: Net Int String
-- n = Net p t m
--   where
--     p [] = []
--     p m = ([0,1] ++) . map succ . drop 1 . p . push $ m
--     t [] = []
--     t m  = (Transition "a" [0] [1] :) .  map shift . t . push $ m
--     m = [0]
--     push = map pred . filter (/= 0)
--     shift (Transition n pre post) =
--       Transition ('a':n) (map (+1) pre) (map (+1) post)





test_withSource = do
  assertEqual True True
