module Main where

import Test.QuickCheck

import Test.Monad.Except.Checkers
import Test.Monad.State.Checkers

main :: IO ()
main = do
  putStrLn "MonadState"
  checkState
  checkState'
  putStrLn "MonadExcept"
  checkExcept
