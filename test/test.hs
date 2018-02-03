module Main where

import Test.QuickCheck

import Test.Monad.Except.Checkers
import Test.Monad.Reader.Checkers
import Test.Monad.State.Checkers

main :: IO ()
main = do
  putStrLn "MonadState"
  checkState
  checkState'
  putStrLn "MonadExcept"
  checkExcept
  checkExcept'
  putStrLn "MonadReader"
  checkReader
  checkReader'
