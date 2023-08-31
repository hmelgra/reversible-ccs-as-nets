module Net
  (
  -- * Types
    Net (..)
  , Transition (..)
  -- * Query
  , places
  , transitions
  , isTransition
  , isPlace
  , isEnabled
  , enabled
  -- * Transition's firing
  , fire
  -- * Modifiers
  , union
  , (@)
  , (@@)
  )
where

import           Control.Applicative (Applicative (liftA2))
import           Data.Functor        ((<&>))
import qualified Data.List           as L
import           Data.Maybe          (fromMaybe, isJust)

-- Each transition consists of a name, a preset and a postset
data Transition t s = Transition
  { trName :: t
  , trPre  :: [s]
  , trPost :: [s]
  }

instance (Show s, Show t) => Show (Transition t s)  where
  show (Transition t pr ps) =
    "(" ++ show  pr ++ " [" ++ show t ++ "> " ++ show ps ++ ")"

-- The elements of an infinite net depends of the marking.
data Net s t = Net
  { netPlaces        :: [s] -> [s]
  , netTransitions   :: [s] -> [Transition t s]
  , netMarking       :: [s]
  }

instance (Show s, Show t) => Show (Net s t)  where
  show (Net s t m) =
    "Net(" ++ show  (s m) ++ "," ++ show (t m) ++ "," ++ show m ++ ")"

places :: Net s t -> [s]
places n = netPlaces n $ netMarking n

transitions :: Net s t -> [Transition t s ]
transitions n = netTransitions n $ netMarking n

transitionByName :: Eq t => t -> Net s t -> Maybe (Transition t s)
transitionByName t n =  L.find ((== t). trName) (transitions n)

isTransition :: Eq t => t -> Net s t -> Bool
isTransition t n  =  isJust $ transitionByName t n

isPlace :: Eq s => s -> Net s t -> Bool
isPlace s n  = s `elem` places n

pre :: Eq t => t -> Net s t -> [s]
pre t n  =  fromMaybe [] $ transitionByName t n  <&>  trPre

post :: Eq t => t -> Net s t -> [s]
post t n =  fromMaybe [] $ transitionByName t n  <&>  trPost

isEnabled :: (Eq s, Eq t) => t -> Net s t -> Bool
isEnabled t n
  | isTransition t n  = all  (`elem` netMarking n) (pre t n)
  | otherwise = False

(@) :: Net s t -> [s] -> Net s t
(@) (Net s t _) = Net s t

(@@) :: Net s t -> ([s] -> [Transition t s]) -> Net s t
(@@) (Net s _ m) t' = Net s t' m

fire :: (Eq s, Eq t) => t -> Net s t -> Net s t
fire t n@(Net ps ts m)
  | isEnabled t n = Net ps ts ((m  L.\\ pre t n) ++ post t n)
  | otherwise = error "transition not enabled"

enabled :: (Eq s, Eq t) => Net s t -> [t]
enabled n = filter (`isEnabled` n) (map trName $ transitions n)

union :: Net s t -> Net s t -> Net s t
union (Net s1 t1 m1) (Net s2 t2 m2) =
  Net (liftA2 (++) s1 s2) (liftA2 (++) t1 t2) (m1 ++ m2)
