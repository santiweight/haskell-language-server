module Development.IDE.Plugin.Plugins.DefineNotInScope
  (plugin) where

import           Control.Monad.List                        (MonadTrans (..))
import           Data.Bifunctor                            (Bifunctor (..))
import           Data.Either.Extra                         (maybeToEither)
import qualified Data.Text                                 as T
import           Development.IDE
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.ExactPrint     (exactPrint,
                                                            makeDeltaAst)
import           Development.IDE.GHC.ExactPrint            (generatedAnchor,
                                                            mapAnchor,
                                                            modifySmallestDeclWithM_)
import           Development.IDE.Plugin.Plugins.Diagnostic (NotInScope (..),
                                                            extractNotInScopeName,
                                                            unifySpaces)
import           GHC                                       (Anchor (..),
                                                            AnchorOperation (..),
                                                            DeltaPos (..))
import           Ide.PluginUtils                           (makeDiffTextEdit,
                                                            responseError)
import           Language.Haskell.GHC.ExactPrint.Parsers   (parseDecl)
import           Language.Haskell.GHC.ExactPrint.Transform (runTransformT)
import           Language.LSP.Types

plugin :: DynFlags -> ParsedModule -> Diagnostic -> Either ResponseError [(T.Text, [TextEdit])]
plugin dynFlags (ParsedModule _ (makeDeltaAst -> moduleSrc) _ _) Diagnostic {_message, _range} =
  case extractNotInScopeName message of
    Nothing -> pure []
    Just notInScope -> case notInScope of
      NotInScopeDataConstructor dataCon -> fmap fst3 $ runTransformT $ do
        moduleSrc' <- modifySmallestDeclWithM_ spanContainsRangeOrErr (\ decl -> do
          newDataDecl <- lift $ dataConDecl dataCon
          pure [decl, setAtLeastNewlineDp newDataDecl]) moduleSrc
        let diff = makeDiffTextEdit (T.pack $ exactPrint moduleSrc) (T.pack $ exactPrint moduleSrc')
        pure [("Define ‘" <> dataCon <> "’ as datatype", fromLspList diff)]
      _ -> pure []
  where
    dataConDecl :: T.Text -> Either ResponseError (LHsDecl GhcPs)
    dataConDecl dataCon = bimap (mkResponseError . T.pack . show) makeDeltaAst $ parseDecl dynFlags "dummy" ("data " <> T.unpack dataCon <> " = " <> T.unpack dataCon)

    mkResponseError msg = ResponseError InvalidRequest msg Nothing
    fst3 (a, _, _) = a
    message = unifySpaces _message
    spanContainsRangeOrErr = maybeToEither (responseError "SrcSpan was not valid range") . (`spanContainsRange` _range)


fromLspList :: List a -> [a]
fromLspList (List a) = a

-- | Ensures that there is at least one newline's difference between this LHsDecl and the previous decl. If the
-- original decl was on the next line after the anchor, with no padding, we simply return with no modifications
setAtLeastNewlineDp :: LHsDecl GhcPs -> LHsDecl GhcPs
setAtLeastNewlineDp (L srcSpanAnn decl) = L (mapAnchor (maybe nextLineAnchor setAtLeastNewline) srcSpanAnn) decl
  where
    setAtLeastNewline (Anchor realSrcSpan ancOp) =
              let ancOp' = case ancOp of
                    UnchangedAnchor                 -> nextLine2Dp
                    MovedAnchor (SameLine _)        -> nextLine2Dp
                    MovedAnchor (DifferentLine 0 _) -> nextLine2Dp
                    ancOp_                          -> ancOp_
                in Anchor realSrcSpan ancOp'

    nextLine2Dp = MovedAnchor (DifferentLine 2 0)
    nextLineAnchor = generatedAnchor nextLine2Dp
