module Main where

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Monad.Control.Checkers
import Test.Monad.Cont.Checkers
import Test.Monad.Except.Checkers
import Test.Monad.Reader.Checkers
import Test.Monad.State.Checkers
import Test.Monad.Writer.Checkers

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testsState
  , testsCont
  , testsExcept
  , testsReader
  , testsWriter
  , testsControl
  ]

testsState :: TestTree
testsState = testGroup "MonadState"
  [ testProperties "Normal" checkState_
  , testProperties "Mutant" checkState'
  ]

testsCont :: TestTree
testsCont = testGroup "MonadCont"
  [ testProperties "Normal" checkCont_
  ]

testsExcept :: TestTree
testsExcept = testGroup "MonadExcept"
  [ testProperties "Normal" checkExcept_
  , testProperties "Mutant" checkExcept'
  ]

testsReader :: TestTree
testsReader = testGroup "MonadReader"
  [ testProperties "Normal" checkReader_
  , testProperties "Mutant" checkReader'
  ]

testsWriter :: TestTree
testsWriter = testGroup "MonadWriter"
  [ testProperties "Normal" checkWriter_
  , testProperties "Mutant" checkWriter'
  ]

testsControl :: TestTree
testsControl = testGroup "Monad*Control"
  [ testProperties name props | (name, props) <- checkControl ]
