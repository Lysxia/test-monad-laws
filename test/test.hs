module Main where

import Test.QuickCheck

import Test.Monad.State.Checkers

main :: IO ()
main = do
  putStrLn "MonadState"
  checkState
  checkState'
