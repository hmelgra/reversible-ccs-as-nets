{- REVERSIBLE NET -}
module ReversibleNet
  ( Directed (Fwd,Bwd)
  , rev
  )
where

import           Net (Net(Net), Transition (Transition))

data Directed a
  = Fwd a
  | Bwd a deriving (Eq, Ord)

instance (Show a) => Show (Directed a) where
  show (Fwd a) = "->" ++ show a
  show (Bwd a) = "<-" ++ show a

rev :: Net s t -> Net s (Directed t)
rev (Net s t m) =  Net s t' m
  where
    t' = foldr reverse [] . t

    reverse (Transition x y z) =
      (Transition (Fwd x) y z :) . (Transition (Bwd x) z y :)
