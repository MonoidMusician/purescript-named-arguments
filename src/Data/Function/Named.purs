module Data.Function.Named where

import Type.Row (kind RowList, class RowToList, class ListToRow, Nil, Cons, RLProxy(..))

foreign import unsafeMergeImpl :: forall r1 r2 r.
  Record r1 -> Record r2 -> Record r

unsafeMerge :: forall r1 r2 r. Union r1 r2 r =>
  Record r1 -> Record r2 -> Record r
unsafeMerge = unsafeMergeImpl

type Named expected final =
  forall given step.
    CurryNamed expected given final step =>
  Record given -> step

class CurryNamed
  (expected :: # Type)
  (given    :: # Type)
  (final    ::   Type)
  (step     ::   Type)
  | expected given final -> step
  where
    curryNamed ::
      (Record expected -> final) ->
      (Record given -> step)

instance delegateCurryNamed ::
  ( Union remaining given expected
  , RowToList remaining remainingRL
  , ListToRow remainingRL remaining
  , CurryNamedRL remainingRL expected remaining given final step
  ) => CurryNamed expected given final step
  where
    curryNamed = curryNamedRL (RLProxy :: RLProxy remainingRL)

class
  ( Union remaining given expected
  , RowToList remaining remainingRL
  , ListToRow remainingRL remaining
  ) <= CurryNamedRL
  (remainingRL :: RowList)
  (expected    ::  # Type)
  (remaining   ::  # Type)
  (given       ::  # Type)
  (final       ::    Type)
  (step        ::    Type)
  | remainingRL expected given final -> step remaining
  , remaining expected given final -> step remainingRL
  , remainingRL -> remaining
  , remaining -> remainingRL
  where
    curryNamedRL :: RLProxy remainingRL ->
      (Record expected -> final) ->
      (Record given -> step)

instance fullyNamed ::
  CurryNamedRL Nil expected () expected final final
  where
    curryNamedRL _ f g = f g

instance partiallyNamed ::
  ( ListToRow (Cons sym t rl) remaining
  , RowToList remaining (Cons sym t rl)
  , Union remaining given expected
  -- , CurryNamed remaining more final step
  ) => CurryNamedRL
    (Cons sym t rl)
    expected remaining given
    final (Record remaining -> final) -- (Record more -> step)
  where
    curryNamedRL _ f g1 = {-curryNamed-} merging
      where
        merge :: Record remaining -> Record given -> Record expected
        merge = unsafeMerge
        merging :: Record remaining -> final
        merging g2 = f (merge g2 g1)
