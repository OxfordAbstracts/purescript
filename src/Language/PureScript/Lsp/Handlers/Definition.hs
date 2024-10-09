{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Language.PureScript.Lsp.Handlers.Definition where

import Control.Lens ((^.))
import Control.Lens.Getter (to)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Message
import Language.LSP.Protocol.Types qualified as Types
import Language.LSP.Server qualified as Server
import Language.LSP.VFS qualified as VFS
import Language.PureScript qualified as P
import Language.PureScript.CoreFn.Expr qualified as CF
import Language.PureScript.Errors qualified as Errors
import Language.PureScript.Lsp.Cache (selectExternModuleNameFromFilePath, selectExternPathFromModuleName)
import Language.PureScript.Lsp.Cache.Query (getAstDeclarationInModule, getCoreFnExprAt, getEfDeclarationInModule)
import Language.PureScript.Lsp.Docs (readQualifiedNameDocsSourceSpan)
import Language.PureScript.Lsp.Monad (HandlerM)
import Language.PureScript.Lsp.Print (printName)
import Language.PureScript.Lsp.ServerConfig (ServerConfig)
import Language.PureScript.Lsp.Util (efDeclSourceSpan, getNamesAtPosition, sourcePosToPosition)
import Protolude hiding (to)

definitionHandler :: Server.Handlers (HandlerM ServerConfig)
definitionHandler = Server.requestHandler Message.SMethod_TextDocumentDefinition $ \req res -> do
  let Types.DefinitionParams docIdent pos _prog _prog' = req ^. LSP.params
      filePathMb = Types.uriToFilePath $ docIdent ^. LSP.uri
      uri :: Types.NormalizedUri
      uri =
        req
          ^. LSP.params
            . LSP.textDocument
            . LSP.uri
            . to Types.toNormalizedUri

      nullRes = res $ Right $ Types.InR $ Types.InR Types.Null

      locationRes fp range = res $ Right $ Types.InL $ Types.Definition $ Types.InL $ Types.Location (Types.filePathToUri fp) range

      forLsp :: Maybe a -> (a -> HandlerM ServerConfig ()) -> HandlerM ServerConfig ()
      forLsp val f = maybe nullRes f val
  forLsp filePathMb \filePath -> do
    vfMb <- Server.getVirtualFile uri
    forLsp vfMb \vf -> do
      mNameMb <- selectExternModuleNameFromFilePath filePath
      forLsp mNameMb \mName -> do
        names <- getNamesAtPosition pos mName (VFS._file_text vf)

        case head names of
          Just name -> do
            spanMb <- readQualifiedNameDocsSourceSpan name
            case spanMb of
              _ -> do
                case name of
                  P.Qualified (P.BySourcePos pos') _ -> do
                    locationRes filePath (Types.Range (sourcePosToPosition pos') (sourcePosToPosition pos'))
                  P.Qualified (P.ByModuleName nameModule) ident -> do
                    declMb <- getAstDeclarationInModule nameModule (printName ident)
                    forLsp declMb \decl -> do
                      modFpMb <- selectExternPathFromModuleName nameModule
                      forLsp modFpMb \modFp -> do
                        let sourceSpan = P.declSourceSpan decl
                        locationRes modFp (spanToRange sourceSpan)
              Just span ->
                locationRes (P.spanName span) (spanToRange span)
          _ -> do
            corefnExprMb <- getCoreFnExprAt filePath pos
            case corefnExprMb of
              Just (CF.Var (_ss, _comments, _meta) (P.Qualified qb ident)) -> do
                let name = P.runIdent ident
                case qb of
                  P.ByModuleName coreMName -> do
                    declMb <- getEfDeclarationInModule coreMName name
                    forLsp declMb \decl -> do
                      modFpMb <- selectExternPathFromModuleName coreMName
                      forLsp modFpMb \modFp -> do
                        let sourceSpan = efDeclSourceSpan decl
                        locationRes modFp (spanToRange sourceSpan)
                  P.BySourcePos srcPos ->
                    locationRes filePath (Types.Range (sourcePosToPosition srcPos) (sourcePosToPosition srcPos))
              _ -> nullRes

spanToRange :: Errors.SourceSpan -> Types.Range
spanToRange (Errors.SourceSpan _ start end) =
  Types.Range
    (sourcePosToPosition start)
    (sourcePosToPosition end)