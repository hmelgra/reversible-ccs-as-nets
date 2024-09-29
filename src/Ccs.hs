module Ccs
  (
  -- * Types
    Action (In, Out, Tau)
  , VarName(VarName)
  , CCS ((:.), (:|), (:+), (:\), Nil, Var, Rec)
  -- * Check action duality
  , dual
  -- * Transformations
  , subs
  , unfold
  )
where

import Data.Char (isSpace)

data Action a
  = In a   -- Input
  | Out a  -- Output
  | Tau    -- Internal Action
  deriving (Eq, Ord, Read)

instance (Show a) => Show (Action a)  where
  show (In x) = show x ++ "?"
  show (Out y) = show y ++ "!"
  show Tau      = "0"

-- It checks whether a pair of actions are duals
dual :: Eq a => Action a -> Action a -> Bool
dual (Out x) (In y) = x == y
dual (In x) (Out y) = x == y
dual _ _               =  False


-- Milner's precedence
infixl 4 :\
infixr 5 :.
infixl 6 :|
infixl 7 :+

{- Type for process variable names. Allows for the redefinition of Read -}
newtype VarName = VarName String deriving (Eq, Ord)

instance Read VarName where
    readsPrec _ str = [(VarName parsedStr, rest)] where
        isDelimiter c = isSpace c || c `elem` ".()"
        (parsedStr, rest) = parseStringWithoutQuotes str
        parseStringWithoutQuotes :: String -> (String, String)
        parseStringWithoutQuotes str' =
            let (parsed, rest') = break isDelimiter (dropWhile isSpace str')
            in (parsed, dropWhile isSpace rest')

{- Syntax for infinite CCS Processes -}
data CCS a
  = (Action a) :. (CCS a)  -- Prefix
  | (CCS a) :| (CCS a)     -- Parallel
  | (CCS a) :+ (CCS a)     -- Choice
  | (CCS a) :\ a           -- Restriction
  | Nil                    -- Ended process
  | Var VarName            -- Process variable
  | Rec VarName (CCS a)    -- Recursive process
  deriving (Eq, Ord, Read)

instance (Show a) => Show (CCS a)  where
  show (x :. y)  = show x ++ "." ++ show y
  show (x :| y)  = show x ++ "|" ++ show y
  show (x :+ y)  = show x ++ "+" ++ show y
  show (x :\ y)  = "(" ++ show x ++ ")\\ " ++ show y
  show Nil       = "0"
  show (Var (VarName x))   = x
  show (Rec (VarName x) p) = "Rec " ++ x ++ "." ++ show p

{- Substitution of a process variable by a process -}
subs :: CCS a    -- ^ process over which substitution is applied
     -> String   -- ^ process variable to be substituted
     -> CCS a    -- ^ replacement term
     -> CCS a
subs (a :. p) x r = a :. subs p x r
subs (p :| q) x r = subs p x r :| subs q x r
subs (p :+ q) x r = subs p x r :+ subs q x r
subs (p :\ a) x r = subs p x r :\ a
subs Nil  _ _     = Nil
subs (Var y) x r
  | y == VarName x        = r
  | otherwise     = Var y
subs (Rec y p) x r
  | y == VarName x        = Rec y p
  | otherwise     = Rec y (subs p x r)

{- Unfolding step of a  process -}
unfold :: CCS a -> CCS a
unfold (Rec (VarName x) p) = subs p x (Rec (VarName x) p)
unfold p                   = p
