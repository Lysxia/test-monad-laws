{-# LANGUAGE TypeApplications #-}

module Test.Monad.State.Checkers where

import Control.Monad.State
import Test.QuickCheck (Property)
import Test.QuickCheck.HigherOrder (ok, ko)

import Test.Monad.Instances ()
import Test.Monad.State
import Test.Monad.State.Mutants

checkState :: [(String, Property)]
checkState =
  [ ok "get-get" (get_get @(State Int))
  , ok "get-put" (get_put @(State Int))
  , ok "put-get" (put_get @(State Int))
  , ok "put-put" (put_put @(State Int))
  ]

checkState' :: [(String, Property)]
checkState' =
  [ ko "bad-get-put-get"     (bad_get_put_get @(State Int))
  , ko "bad-put-put"         (bad_put_put     @(State Int))
  , ko "mut-put-get"         (put_get         @(MutantState Int))
  , ok "mut-bad-get-put-get" (bad_get_put_get @(MutantState Int))
  , ok "mut-bad-put-put"     (bad_put_put     @(MutantState Int))
  ]

