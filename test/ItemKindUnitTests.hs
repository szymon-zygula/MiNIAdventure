module ItemKindUnitTests (itemKindUnitTests) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import qualified Content.RuleKind
import           Game.LambdaHack.Common.Kind (emptyMultiGroupItem)
import           Game.LambdaHack.Content.ItemKind
import qualified Game.LambdaHack.Content.RuleKind as RK

itemKindUnitTests :: TestTree
itemKindUnitTests = testGroup "itemKindUnitTests" $
  let standardSymbols = RK.ritemSymbols Content.RuleKind.standardRules
  in
  [ testCase "overlonginame_validateSingle_errs" $
      validateSingle standardSymbols
                     emptyMultiGroupItem { iname = "123456789012345678901234" }
      @?= ["iname longer than 23"]
  , testCase "shortEnoughiname_validateSingle_noErr" $
      validateSingle standardSymbols
                     emptyMultiGroupItem
      @?= []
  ]
