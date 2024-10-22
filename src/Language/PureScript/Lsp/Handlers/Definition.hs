{-# LANGUAGE BlockArguments #-}

module Language.PureScript.Lsp.Handlers.Definition where

import Control.Lens ((^.))
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.PureScript qualified as P
import Language.PureScript.Lsp.Cache (selectExternPathFromModuleName)
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationLocationInModule)
import Language.PureScript.Lsp.Log (debugLsp)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.NameType (LspNameType (..))
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.Util (posInSpan, sourcePosToPosition)
import Protolude
import Language.PureScript.Lsp.AtPosition (atPosition, findDeclRefAtPos, getImportRefNameType, spanToRange)

definitionHandler :: Server.Handlers HandlerM
definitionHandler = Server.requestHandler Message.SMethod_TextDocumentDefinition $ \req res -> do
  let Types.DefinitionParams docIdent pos _prog _prog' = req ^. LSP.params
      filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri

      nullRes = res $ Right $ Types.InR $ Types.InR Types.Null

      locationRes fp range = res $ Right $ Types.InL $ Types.Definition $ Types.InL $ Types.Location (Types.filePathToUri fp) range

      posRes fp srcPos = locationRes fp $ Types.Range (sourcePosToPosition srcPos) (sourcePosToPosition srcPos)

      forLsp :: Maybe a -> (a -> HandlerM ()) -> HandlerM ()
      forLsp val f = maybe nullRes f val

      respondWithDeclInOtherModule :: LspNameType -> P.ModuleName -> Text -> HandlerM ()
      respondWithDeclInOtherModule nameType modName ident = do
        declSpans <- getAstDeclarationLocationInModule nameType modName ident
        forLsp (head declSpans) $ \sourceSpan ->
          locationRes (P.spanName sourceSpan) (spanToRange sourceSpan)

      respondWithModule :: P.SourceSpan -> P.ModuleName -> HandlerM ()
      respondWithModule ss modName =
        if posInSpan pos ss
          then do
            modFpMb <- selectExternPathFromModuleName modName
            forLsp modFpMb \modFp -> do
              posRes modFp $ P.SourcePos 1 1
          else nullRes

      respondWithImports ss importedModuleName imports = do
        case findDeclRefAtPos pos imports of
          Just import' -> do
            let name = P.declRefName import'
                nameType = getImportRefNameType import'
            debugLsp $ "import: " <> show import'
            respondWithDeclInOtherModule nameType importedModuleName (printName name)
          _ -> do
            debugLsp $ "respondWithModule importedModuleName: " <> show importedModuleName
            respondWithModule ss importedModuleName

  forLsp filePathMb \filePath -> do
    atPosition
      nullRes
      respondWithDeclInOtherModule
      respondWithImports
      respondWithModule
      posRes 
      filePath 
      pos
