{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}

module Test.DefineNotInScope (tests) where

import           Data.List.Extra
import qualified Data.Text                         as T
import           Development.IDE.Types.Location
import           Language.LSP.Test
import           Language.LSP.Types                hiding
                                                   (SemanticTokenAbsolute (length, line),
                                                    SemanticTokenRelative (length),
                                                    SemanticTokensEdit (_start),
                                                    mkRange)
import           Test.Tasty
import           Test.Tasty.HUnit


import           Test.Hls

import qualified Development.IDE.Plugin.CodeAction as Refactor

tests :: TestTree
tests =
  testGroup
    "define not-in-scope datatype"
#if !MIN_VERSION_ghc(9,2,1)
    []
#else
    [ mkGoldenAddArgTest "Simple" (r 0 0 0 50)
    , mkGoldenAddArgTest "WithTypeSig" (r 1 0 1 50)
    ]
  where
    r x y x' y' = Range (Position x y) (Position x' y')

mkGoldenAddArgTest :: FilePath -> Range -> TestTree
mkGoldenAddArgTest testFileName range = mkGoldenAddArgTest' testFileName range "NewData"

-- Make a golden test for the add argument action. Given varName is the name of the variable not yet defined.
mkGoldenAddArgTest' :: FilePath -> Range -> T.Text -> TestTree
mkGoldenAddArgTest' testFileName range varName = do
    let action docB = do
          _ <- waitForDiagnostics
          InR action@CodeAction {_title = actionTitle} : _ <-
            filter (\(InR CodeAction {_title = x}) -> "Define" `isPrefixOf` T.unpack x)
              <$> getCodeActions docB range
          liftIO $ actionTitle @?= ("Define ‘" <> varName <> "’ as datatype")
          executeCodeAction action
    goldenWithHaskellDoc
      (Refactor.bindingsPluginDescriptor mempty "ghcide-code-actions-bindings")
      (testFileName <> " (golden)")
      "test/data/golden/define-datatype"
      testFileName
      "expected"
      "hs"
      action
#endif
