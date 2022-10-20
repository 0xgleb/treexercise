module Lib
  ( Tree(..)
  , BinaryTree(..)

  , cpsMempty
  , CpsBinaryTree(..)

  , toList
  )
  where

import Prelude hiding (lookup)
import GHC.Generics
import Test.QuickCheck.Arbitrary.Generic as QC

-- While not strictly required, this evaluation lets us clearly see each candidate's skills.
-- It also gives super-candidates the chance to really shine.
-- That said, we are open to candidates of all skill levels!
-- (We will 100% take into account if you're new to haskell)
-- Just do the best you can with your current abilities and time available.
-- This was written by (and for) devs who enjoy a good programming puzzle. So have fun!

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-------------------------------    TASK #1   -----------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-
  Make a recursive binary tree type and implement 'BinaryTree' for it.

A type that implemnts BinaryTree is very similar to 'Map [Bool] t'
You can lookup a series of bits (represented as [Bool])
And you can return a new BinaryTree with a new value inserted with the given bits.

It should obey the following common sense laws:

(1) lookup bits (insert bits item tree) == (Just item)
(2) inserting what you looked up doesn't change the tree at all
(3) inserting the same thing twice is the same as inserting it once

Tip: A common mistake can happen when inserting bits that start identically
     (insert [True] "bar" (insert [True, True, False] "foo" tree))
-}

class BinaryTree b where
  lookup :: [Bool] -> b t -> Maybe t
  insert :: [Bool] -> t -> b t -> b t

data Tree a
  = Node (Maybe a) (Tree a) (Tree a)
  | Empty
  deriving stock (Eq, Show, Generic)

deriving via (GenericArbitrary (Tree a)) instance Arbitrary a => Arbitrary (Tree a)

instance BinaryTree Tree where
  lookup [] = \case
    Node val _ _ ->
      val

    Empty ->
      Nothing

  lookup (b:bs) = \case
    Node _ subtree _ | b ->
      lookup bs subtree

    Node _ _ subtree ->
      lookup bs subtree

    Empty ->
      Nothing
  
  insert [] val = \case
    Node _ leftSub rightSub ->
      Node (Just val) leftSub rightSub

    Empty ->
      Node (Just val) Empty Empty

  insert (b:bs) newVal = \case
    Node val leftSub rightSub | b ->
      Node val (insert bs newVal leftSub) rightSub

    Node val leftSub rightSub ->
      Node val leftSub (insert bs newVal rightSub)

    Empty | b ->
      Node Nothing (insert bs newVal Empty) Empty

    Empty ->
      Node Nothing Empty (insert bs newVal Empty)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-------------------------------    TASK #2   -----------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
{-
  This doesn't require writing any code, only thinking + some research :)

  Please move as many typeclasses from list #1 into #2 and #3.
  This is tricky if you're new to haskell.
  But please do your best and really consider each typeclass individually.
  If it can be implemented with a certain constraint on 't', just make a note.
  Feel free to explain your reasoning as much as you want.

  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  Typeclasses that I'm not sure if they should be implemented for my binary tree.
  #1 I'M NOT SURE
  - Comonad <- can be implemented but I'm not sure what if it's useful for anything
  - Contravariant <- I guess it could be useful for copying the topology of the
                   | tree without the values but I've never worked with
                   | contravariants so not sure how useful this is

  Typeclasses that should be implemented for my binary tree
  #2 SHOULD IMPLEMENT
  - Eq
  - Ord
  - Functor
  - Foldable
  - Traversable
  - Semigroup
  - Monoid
  - ...

  Typeclasses that should NOT be implemnted for my binary tree
  #3 SHOULD NOT IMPLEMENT
  - Apply       <- I think a lawful instance can be implemented but it's not very useful
  - Applicative <- I think a lawful instance can be implemented but it's not very useful
  - Monad       <- Maybe a lawful instance can be implemented but it's still not very useful
  - ...
  - ...

  -}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
---------------------------    EXTRA CREDIT #1   -------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-
  Make an efficient 'toList' function for your binary tree.

  toList :: YourBinaryTree t -> [t]
  toList = ???

  (1) You can assume the tree is finite.
  (2) Order does not matter.
  (3) Taking into account GHC evaluation, it should have the best O(N) complexity that you can write.

-}

-- I tried a couple of similar versions, some of which enforced strictness, but
-- this version performed best on a simple benchmark I added in Main.hs
-- (assuming -O2 optimizations)
toList :: Tree t -> [t]
toList = toList' []
  where
    toList' :: [t] -> Tree t -> [t]

    toList' accum Empty
      = accum

    toList' accum (Node val leftSub rightSub)
      = toList' (toList' (maybe accum (:accum) val) leftSub) rightSub

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
---------------------------    EXTRA CREDIT #2   -------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-
  This is the same as Task #1, only in Continuation Passing Style :D
  If you enjoy puzzles, try implementing CpsBinaryTree for your binary tree.
  You are only allowed to create a new tree with the provided 'cpsMempty' function.
-}

instance Semigroup (Tree a) where
  Empty <> rhs
    = rhs

  Node x leftSub rightSub <> rhs
    = Node x leftSub (rightSub <> rhs)

instance Monoid (Tree a) where
  mempty = Empty

cpsMempty :: (Monoid bt) => (bt -> r) -> r
cpsMempty cb = cb mempty

class (BinaryTree b) => CpsBinaryTree b where
  cpsInsert :: [Bool] -> t -> (b t -> r) -> r

-- not sure if this counts but it's something
instance CpsBinaryTree Tree where
  cpsInsert :: [Bool] -> t -> (Tree t -> r) -> r

  cpsInsert [] val k
    = k $ toTree val id

  cpsInsert (b:bs) val k
    = k $ cpsInsert bs val
        $ if b then toLeft else toRight

toTree :: a -> (Tree a -> r) -> r
toTree val k
  = k $ Node (Just val) Empty Empty

toLeft :: Tree a -> Tree a
toLeft subtree
  = cpsMempty $ \empty -> Node Nothing subtree empty

toRight :: Tree a -> Tree a
toRight subtree
  = cpsMempty $ \empty -> Node Nothing empty subtree
