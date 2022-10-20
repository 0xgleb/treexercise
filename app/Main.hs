module Main (main) where

import Lib

import Prelude
import Criterion.Main

main :: IO ()
main = do
  let tree3
        = insert [True, False, False, False, True, False, False, False] 532
        $ insert [True, False, True, False, False] 93
        $ insert [True, False, True, True, False, True, False, True] 59
        $ insert [True, False, False, True, True, True, False] 11
        $ insert [True, False, False, False, False, False, False] 532
            tree2

      tree2
        = insert [True, True, False, False] 93
        $ insert [True, True, False, True, False, True] 59
        $ insert [True, False, True, False, True, True, False] 11
        $ insert [False, False, False, False, False, False] 532
        $ insert [False, True, False, False] 93
        $ insert [False, True, False, True, False, True] 59
        $ insert [False, False, True, True, True, False] 11
            tree1

      tree1
        = insert [False, False, False, False, False] 532
        $ insert [True, False, False] 93
        $ insert [True, False, True, False, True] 59
        $ insert [False, True, True, True, False] 11
        $ insert [True, True, True, False, False] (379 :: Int)
            Empty

  defaultMain [
    bgroup "toList"
      [ bench "tree 1"  $ whnf toList tree1
      , bench "tree 2"  $ whnf toList tree2
      , bench "tree 3"  $ whnf toList tree3
      ]
   ]
