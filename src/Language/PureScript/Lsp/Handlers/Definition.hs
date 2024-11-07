{-# LANGUAGE BlockArguments #-}

module Language.PureScript.Lsp.Handlers.Definition where

import Control.Lens ((^.))
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.PureScript qualified as P
import Language.PureScript.Lsp.AtPosition (getImportRefNameType, spanToRange)
import Language.PureScript.Lsp.Cache (selectExternPathFromModuleName)
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationLocationInModule)
import Language.PureScript.Lsp.Log (debugLsp)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.NameType (LspNameType (..))
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.State (cachedFilePaths, cachedRebuild)
import Language.PureScript.Lsp.Types (OpenFile (OpenFile, ofArtifacts))
import Language.PureScript.Lsp.Util (positionToSourcePos, sourcePosToPosition)
import Language.PureScript.TypeChecker.IdeArtifacts (IdeArtifact (..), IdeArtifactValue (..), getArtifactsAtPosition, smallestArtifact)
import Protolude
import Language.PureScript.Lsp.Docs (readDeclarationDocsSourceSpan)

definitionHandler :: Server.Handlers HandlerM
definitionHandler = Server.requestHandler Message.SMethod_TextDocumentDefinition $ \req res -> do
  let Types.DefinitionParams docIdent pos _prog _prog' = req ^. LSP.params
      filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri

      nullRes = res $ Right $ Types.InR $ Types.InR Types.Null

      locationRes fp range = res $ Right $ Types.InL $ Types.Definition $ Types.InL $ Types.Location (Types.filePathToUri fp) range

      posRes fp srcPos = locationRes fp $ Types.Range (sourcePosToPosition srcPos) (sourcePosToPosition srcPos)

      spanRes span = locationRes (P.spanName span) (spanToRange span)

      forLsp :: Maybe a -> (a -> HandlerM ()) -> HandlerM ()
      forLsp val f = maybe nullRes f val

      respondWithDeclInOtherModule :: LspNameType -> P.ModuleName -> Text -> HandlerM ()
      respondWithDeclInOtherModule nameType modName ident = do
        declSpans <- getAstDeclarationLocationInModule nameType modName ident
        case head declSpans of
          Just sourceSpan ->
            locationRes (P.spanName sourceSpan) (spanToRange sourceSpan)
          Nothing -> do
            debugLsp $ "No definition in DB found for " <> show nameType <> " " <> show ident <> " in " <> show modName 
            docSsMb <- readDeclarationDocsSourceSpan modName ident
            forLsp docSsMb spanRes

      respondWithModule :: P.ModuleName -> HandlerM ()
      respondWithModule modName = do
        modFpMb <- selectExternPathFromModuleName modName
        forLsp modFpMb \modFp -> do
          posRes modFp $ P.SourcePos 1 1

  forLsp filePathMb \filePath -> do
    cacheOpenMb <- cachedRebuild filePath
    when (isNothing cacheOpenMb) do
      debugLsp $ "file path not cached: " <> T.pack filePath
      debugLsp . show =<< cachedFilePaths

    forLsp cacheOpenMb \OpenFile {..} -> do
      let allArtifacts = ofArtifacts
          atPos = getArtifactsAtPosition (positionToSourcePos pos) allArtifacts
      let smallest = smallestArtifact (\a -> (negate $ artifactInterest a, isNothing (iaDefinitionPos a), isNothing (iaDefinitionModule a))) atPos
      case smallest of
        Just (IdeArtifact _ (IaModule modName) _ _ _) -> respondWithModule modName
        Just (IdeArtifact _ (IaImport modName ref) _ _ _) -> do
          let nameType = getImportRefNameType ref
              name = P.declRefName ref
          respondWithDeclInOtherModule nameType modName (printName name)
        Just (IdeArtifact _ (IaExpr _ (Just ident) (Just nameType)) _ (Just modName) _) -> do
          respondWithDeclInOtherModule nameType modName ident
        Just (IdeArtifact _ (IaTypeName name) _ (Just modName) _) -> do
          respondWithDeclInOtherModule TyNameType modName (P.runProperName name)
        Just (IdeArtifact _ (IaClassName name) _ (Just modName) _) -> do
          respondWithDeclInOtherModule TyClassNameType modName (P.runProperName name)
        Just (IdeArtifact _ _ _ _ (Just (Right defSpan))) -> do
          spanRes defSpan
        Just (IdeArtifact _ _ _ (Just modName) (Just (Left defPos))) -> do
          fpMb <- selectExternPathFromModuleName modName
          forLsp fpMb \fp -> posRes fp defPos
        Just (IdeArtifact _ _ _ Nothing (Just (Left defPos))) -> do
          posRes filePath defPos
        _ -> do
          debugLsp "No relevat definition found for artifact"
          nullRes

artifactInterest :: IdeArtifact -> Int
artifactInterest (IdeArtifact {..}) = case iaValue of
  IaBinder {} -> 2
  IaTypeName {} -> 3
  IaClassName {} -> 3
  IaExpr _ (Just "bind") _ -> -10 -- desugared do notation is not interesting
  _ -> 1
