module Encoding
  (
    enc
  )
where

import           Ccs
import           Control.Monad
import           Data.Bifunctor
import qualified Data.List      as L
import           Data.Maybe
import           Net            (Net (Net, netTransitions, netMarking),
                                 Transition (..), union, (@@))

{- ENCODING -}

{- Data Structure for nets names -}

{-- Place's names --}
data PlaceNames a
  = Proc (CCS a)                        -- CCS process
  | PKey (Action a)                     -- key for an action
  | PPref (Action a) (PlaceNames a)     -- prefixed by an executed action
  | PParLeft (PlaceNames a)             -- on the left of a parallel operator
  | PParRight (PlaceNames a)            -- on the right of a parallel operator
  | PSync (TransNames a) (TransNames a) -- key for synchronisation
  | PPlusLeft (PlaceNames a)            -- on the left of a sum operator
  | PPlusRight (PlaceNames a)           -- on the right of sum operator
  | PRest (PlaceNames a) a              -- under restriction
  deriving (Eq, Ord)

instance (Show a) => Show (PlaceNames a) where
  show (Proc p)       = show p
  show (PKey a)       = "K[" ++ show a ++ "]"
  show (PPref a s)    = "^" ++ show a ++ "." ++ show s
  show (PParLeft s)   = "|l:" ++ show s
  show (PParRight s)  = "|r:" ++ show s
  show (PSync t1 t2)  = "K[" ++ show t1 ++ "*" ++ show t2 ++ "]"
  show (PPlusLeft s)  = "+l:" ++ show s
  show (PPlusRight s) = "+r:" ++ show s
  show (PRest s a)    = "(" ++ show s ++ ")\\" ++ show a

isKey :: PlaceNames a -> Bool
isKey (PKey _)       = True
isKey (PPref _ s)    = isKey s
isKey (PParLeft s)   = isKey s
isKey (PParRight s)  = isKey s
isKey (PSync _ _)    = True
isKey (PPlusRight s) = isKey s
isKey (PPlusLeft s)  = isKey s
isKey (PRest s _)    = isKey s
isKey _              = False

isNotKey :: PlaceNames a -> Bool
isNotKey = not . isKey

unwrapPref :: Eq a => Action a -> PlaceNames a -> Maybe (PlaceNames a)
unwrapPref a (PPref b s) | a == b = Just s
unwrapPref _ _ = Nothing

unwrapParLeft :: PlaceNames a -> Maybe (PlaceNames a)
unwrapParLeft (PParLeft s) = Just s
unwrapParLeft _            = Nothing

unwrapParRight :: PlaceNames a -> Maybe (PlaceNames a)
unwrapParRight (PParRight s) = Just s
unwrapParRight _             = Nothing

unwrapPlusLeft :: PlaceNames a -> Maybe (PlaceNames a)
unwrapPlusLeft (PPlusLeft s) = Just s
unwrapPlusLeft _             = Nothing

unwrapPlusRight :: PlaceNames a -> Maybe (PlaceNames a)
unwrapPlusRight (PPlusRight s) = Just s
unwrapPlusRight _              = Nothing

unwrapRest :: Eq a => PlaceNames a -> a -> Maybe (PlaceNames a)
unwrapRest (PRest s a) b | a == b = Just s
unwrapRest _ _ = Nothing

{-- Transition's names --}
data TransNames a
  = Act (Action a)                      -- CCS process
  | TPref (Action a) (TransNames a)     -- prefixed by an executed action
  | TParLeft (TransNames a)             -- on the left of a parallel operator
  | TParRight (TransNames a)            -- on the right of a parallel operator
  | TSync (TransNames a) (TransNames a) -- a synchronisation
  | TPlusLeft (TransNames a)            -- on the left of a sum operator
  | TPlusRight (TransNames a)           -- on the right of sum operator
  | TRest (TransNames a) a              -- under restriction
  deriving (Eq, Ord)

instance (Show a) => Show (TransNames a) where
  show (Act a)        = show a
  show (TPref a t)    = "^" ++ show a ++ "." ++ show t
  show (TParLeft t)   = "|l:" ++ show t
  show (TParRight t)  = "|r:" ++ show t
  show (TSync t1 t2)  = show t1 ++ "*" ++ show t2
  show (TPlusLeft t)  = "+l:" ++ show t
  show (TPlusRight t) = "+r:" ++ show t
  show (TRest t a)    = "(" ++ show t ++ ")\\" ++ show a

label :: TransNames a -> Action a
label (Act l)        = l
label (TPref _ t)    = label t
label (TParRight t)  = label t
label (TParLeft t)   = label t
label (TSync _ _)    = Tau
label (TPlusLeft t)  = label t
label (TPlusRight t) = label t
label (TRest t _)    = label t


-- renameNet :: (s -> Maybe s) -> Net s t -> Net s t
-- renameNet d (Net s t m) = Net (s . mapMaybe d) (t . mapMaybe d) m

rename :: (s -> s') -> (t -> t') -> (s' -> Maybe s) -> Net s t -> Net s' t'
rename fs ft fm (Net s t m) =
  Net (map fs . s . mapMaybe fm) (map (relabel ft fs) . t . mapMaybe fm) (map fs m)
  where
  relabel ::  (t -> t') -> (s -> s') -> Transition t s -> Transition t' s'
  relabel f g (Transition t pre post) = Transition (f t) (map g pre) (map g post)


sync ::
  Eq t
  => (s -> [Transition (TransNames t) (PlaceNames t)])
  -> (s -> [Transition (TransNames t) (PlaceNames t)])
  -> s
  -> [Transition (TransNames t) (PlaceNames t)]
sync f1 f2 = map (uncurry synchronisationOf) . matching
  where
    matching = filter (uncurry dual . join bimap (label . trName)) . kart f1 f2

    kart xs ys zs = [(x, y) | x <- xs zs, y <- ys zs]

    synchronisationOf t1 t2 =
      Transition
      (TSync (trName t1) (trName t2))                                        -- transition name
      (trPre t1 ++ trPre t2)                                                     -- the pre is the union of the pres
      (PSync (trName t1) (trName t2) : filter isNotKey (trPost t1 ++ trPost t2)) -- the post is the key and the places in the posts that are not keys

enc :: (Eq t) => CCS t -> Net (PlaceNames t) (TransNames t)

enc Nil = Net (const [Proc Nil]) (const []) [Proc Nil]

enc (a :. p) = Net s t [Proc (a :. p)]
  where
    Net aSp aTp amp = rename (PPref a) (TPref a) (unwrapPref a) $ enc p
    s m = if null m then [] else [Proc (a :. p), PKey a] ++ aSp m
    t m = if null m then [] else
            Transition (Act a) [Proc (a :. p)] (PKey a : amp) : aTp m

enc (p1 :| p2) = n1 `union` n2 `union` n3
  where
    n1 = rename PParLeft  TParLeft  unwrapParLeft  $ enc p1
    n2 = rename PParRight TParRight unwrapParRight $ enc p2
    n3 = Net (const []) (sync (netTransitions  n1) (netTransitions n2)) []

enc (p1 :+ p2) = connect n1 n2 `union` connect n2 n1
  where
    n1 = rename PPlusLeft  TPlusLeft  unwrapPlusLeft  $ enc p1
    n2 = rename PPlusRight TPlusRight unwrapPlusRight $ enc p2

    connect n n' = n @@ \m -> map (addInitial n n') (netTransitions n m)

    addInitial n n' t  =
      if null (trPre t L.\\ netMarking n)
      then Transition (trName t) (trPre t ++ netMarking n') (trPost t)
      else t

enc (p1 :\ a) = n1 @@ (filter (notOn a) . netTransitions n1)
  where
    n1 = rename (`PRest` a) (`TRest` a) (`unwrapRest` a) $ enc p1
    notOn a = not . (`elem` [In a, Out a]) . label . trName

enc (Rec x p) = enc (unfold (Rec x p))

enc (Var _) = error "Non-closed CCS process"
