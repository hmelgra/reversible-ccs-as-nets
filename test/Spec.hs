{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main (main) where

import {-@ HTF_TESTS @-} TestCcs
import                   Test.Framework

--Import {-@ HTF_TESTS @-} TestFsm

main :: IO ()


main = htfMain htf_importedTests
