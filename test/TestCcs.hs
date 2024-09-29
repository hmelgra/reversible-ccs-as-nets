{-# OPTIONS_GHC -F -pgmF htfpp #-}


module TestCcs(
  htf_thisModulesTests
) where

import           Ccs
import           Test.Framework


-- ccs1 :: CCS Int
-- ccs1 = (1:?) :. (2:!) :. Nil
-- ccs2 :: CCS Int
-- ccs2 = (1:!) :. (2:?) :. Nil

-- ccs3 :: CCS Int
-- ccs3 = Rec "X" ((1:?) :. Var "X")

-- ccs4 :: CCS Int
-- ccs4 = Rec "X" (((2:!) :. Var "X") :+ ((1:!) :. Nil))



-- ccs :: CCS Char
-- ccs = (In 'a' :. Out 'a' :. Nil) :| ((Out 'a' :. Nil) :+ (In 'b' :. Nil))

ccs' :: CCS Char
ccs' = Rec (VarName "X") (In 'a' :. Var (VarName "X") )

ccs'' :: CCS Char
ccs'' = (Rec (VarName "X") (In 'a' :. Var (VarName "X")) :| ((Out 'a' :. Nil) :+ (In 'b' :. Nil)))


ccs1 :: CCS Int
ccs1 = Rec (VarName "X") (In 1 :. Var (VarName "X"))

ccs2 :: CCS Int
ccs2 = Rec (VarName "X") ((Out 2 :. Var (VarName "X")) :+ (Out 1 :. Nil))

ccs :: CCS Int
ccs = (ccs1 :| ccs2) :\ 1

ccs3 :: CCS Int
ccs3 = In 1 :. Out 2 :. Nil

ccs4 :: CCS Int
ccs4 = Out 1 :. In 2 :. Nil
