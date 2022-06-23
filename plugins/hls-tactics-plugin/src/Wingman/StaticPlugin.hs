{-# LANGUAGE CPP #-}

module Wingman.StaticPlugin
  ( staticPlugin
  , metaprogramHoleName
  , enableQuasiQuotes
  , pattern WingmanMetaprogram
  , pattern MetaprogramSyntax
  ) where

import Development.IDE.GHC.Compat
import Development.IDE.GHC.Compat.Util
import GHC.LanguageExtensions.Type (Extension(EmptyCase, QuasiQuotes))

import Ide.Types

#if MIN_VERSION_ghc(9,2,0)
import GHC.Parser.Annotation
#endif
#if __GLASGOW_HASKELL__ >= 808
import Data.Data
import Generics.SYB
#if __GLASGOW_HASKELL__ >= 900
import GHC.Driver.Plugins (purePlugin)
import GHC (EpAnn(EpAnnNotUsed))
#else
import Plugins (purePlugin)
#endif
#endif

staticPlugin :: GhcOptsModifications
staticPlugin = mempty
  { dynFlagsModifyGlobal =
      \df -> allowEmptyCaseButWithWarning
           $ flip gopt_unset Opt_SortBySubsumHoleFits
           $ flip gopt_unset Opt_ShowValidHoleFits
           $ df
             { refLevelHoleFits = Just 0
             , maxRefHoleFits   = Just 0
             , maxValidHoleFits = Just 0
#if MIN_VERSION_ghc(9,2,0)
#else
#if __GLASGOW_HASKELL__ >= 808
             , staticPlugins = staticPlugins df <> [metaprogrammingPlugin]
#endif
#endif
             }
#if __GLASGOW_HASKELL__ >= 808
  , dynFlagsModifyParser = enableQuasiQuotes
#endif
#if MIN_VERSION_ghc(9,2,0)
  , staticPlugins = [metaprogrammingPlugin]
#else
  , staticPlugins = []
#endif
  }


pattern MetaprogramSourceText :: SourceText
pattern MetaprogramSourceText = SourceText "wingman-meta-program"


pattern WingmanMetaprogram :: (XRec p (HsExpr p) ~ GenLocated l (HsExpr p)) => FastString -> HsExpr p
pattern WingmanMetaprogram mp <-
#if __GLASGOW_HASKELL__ >= 900
  HsPragE _ (HsPragSCC _ MetaprogramSourceText (StringLiteral NoSourceText mp Nothing))
      (L _ ( HsVar _ _))
#else
  HsSCC _ MetaprogramSourceText (StringLiteral NoSourceText mp)
      (L _ ( HsVar _ _))
#endif


enableQuasiQuotes :: DynFlags -> DynFlags
enableQuasiQuotes = flip xopt_set QuasiQuotes


-- | Wingman wants to support destructing of empty cases, but these are a parse
-- error by default. So we want to enable 'EmptyCase', but then that leads to
-- silent errors without 'Opt_WarnIncompletePatterns'.
allowEmptyCaseButWithWarning :: DynFlags -> DynFlags
allowEmptyCaseButWithWarning =
  flip xopt_set EmptyCase . flip wopt_set Opt_WarnIncompletePatterns


#if __GLASGOW_HASKELL__ >= 808
metaprogrammingPlugin :: StaticPlugin
metaprogrammingPlugin =
    StaticPlugin $ PluginWithArgs pluginDefinition  []
  where
    pluginDefinition = defaultPlugin
        { parsedResultAction = worker
        , pluginRecompile = purePlugin
        }
    worker :: Monad m => [CommandLineOption] -> ModSummary -> HsParsedModule -> m HsParsedModule
    worker _ _ pm = pure $ pm { hpm_module = addMetaprogrammingSyntax $ hpm_module pm }

mkMetaprogram :: SrcSpan -> FastString -> HsExpr GhcPs
mkMetaprogram ss mp =
#if MIN_VERSION_ghc(9,2,0)
  HsPragE noExtField (HsPragSCC EpAnnNotUsed MetaprogramSourceText (StringLiteral NoSourceText mp Nothing))
    $ L (SrcSpanAnn EpAnnNotUsed ss)
    $ HsVar noExtField
    $ L (SrcSpanAnn EpAnnNotUsed ss)
    $ mkRdrUnqual metaprogramHoleName
addMetaprogrammingSyntax :: Data a => a -> a
addMetaprogrammingSyntax =
  everywhere $ mkT $ \case
    L ss (MetaprogramSyntax mp) ->
      L ss $ mkMetaprogram ss mp
    (x :: GenLocated SrcSpan (HsExpr GhcPs)) -> x
#else
#if __GLASGOW_HASKELL__ >= 900
  HsPragE noExtField (HsPragSCC EpAnnNotUsed MetaprogramSourceText (StringLiteral NoSourceText mp Nothing))
    $ L ss
    $ HsVar noExtField
    $ L ss
    $ mkRdrUnqual metaprogramHoleName
#else
  HsSCC noExtField MetaprogramSourceText (StringLiteral NoSourceText mp)
    $ L ss
    $ HsVar noExtField
    $ L ss
    $ mkRdrUnqual metaprogramHoleName
#endif

addMetaprogrammingSyntax :: Data a => a -> a
addMetaprogrammingSyntax =
  everywhere $ mkT $ \case
    L ss (MetaprogramSyntax mp) ->
      L ss $ mkMetaprogram ss mp
    (x :: LHsExpr GhcPs) -> x
#endif
#endif

metaprogramHoleName :: OccName
metaprogramHoleName = mkVarOcc "_$metaprogram"

pattern MetaprogramSyntax :: FastString -> HsExpr GhcPs
pattern MetaprogramSyntax mp <-
    HsSpliceE _ (HsQuasiQuote _ _ (occNameString . rdrNameOcc -> "wingman") _ mp)
  where
    MetaprogramSyntax mp =
      HsSpliceE EpAnnNotUsed $
        HsQuasiQuote
          noExtField
          (mkRdrUnqual $ mkVarOcc "splice")
          (mkRdrUnqual $ mkVarOcc "wingman")
          noSrcSpan
          mp
