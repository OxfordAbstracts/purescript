module Language.PureScript.Lsp.Prim where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Version (showVersion)
import Language.PureScript (primEnv)
import Language.PureScript qualified as P
import Language.PureScript.AST.SourcePos (nullSourceSpan)
import Protolude

primExternsMap :: Map P.ModuleName [P.ExternsFile]
primExternsMap =
  primExterns
    <&> (\ef -> (P.efModuleName ef, [ef]))
    & Map.fromListWith (<>)

primExterns :: [P.ExternsFile]
primExterns = Map.toList primEnv <&> toExtern
  where
    toExtern ::
      (P.ModuleName, (P.SourceSpan, P.Imports, P.Exports)) ->
      P.ExternsFile
    toExtern (modName, (srcSpan, P.Imports {..}, P.Exports {..})) =
      P.ExternsFile
        { efVersion = T.pack $ showVersion P.version,
          efModuleName = modName,
          efExports = efExports,
          efImports = efImports,
          efFixities = [],
          efTypeFixities = [],
          efDeclarations = efDeclarations,
          efSourceSpan = srcSpan
        }
      where
        efExports =
          (Map.toList exportedTypes <&> toEfExportType)
            <> (Map.toList exportedTypeClasses <&> toEfExportTypeClass)
            <> (Map.toList exportedValues <&> toEfExportValue)
            <> (Map.toList exportedTypeOps <&> toEfExportTypeOp)
            <> (Map.toList exportedValueOps <&> toEfExportValueOp)

        toEfExportType ::
          ( P.ProperName 'P.TypeName,
            ([P.ProperName 'P.ConstructorName], P.ExportSource)
          ) ->
          P.DeclarationRef
        toEfExportType (name, (ctrs, _src)) = P.TypeRef nullSourceSpan name (Just ctrs)

        toEfExportTypeClass ::
          (P.ProperName 'P.ClassName, P.ExportSource) ->
          P.DeclarationRef
        toEfExportTypeClass (name, _src) = P.TypeClassRef nullSourceSpan name

        toEfExportValue :: (P.Ident, P.ExportSource) -> P.DeclarationRef
        toEfExportValue (ident, _) = P.ValueRef nullSourceSpan ident

        toEfExportTypeOp :: (P.OpName 'P.TypeOpName, P.ExportSource) -> P.DeclarationRef
        toEfExportTypeOp (opName, _) = P.TypeOpRef nullSourceSpan opName

        toEfExportValueOp :: (P.OpName 'P.ValueOpName, P.ExportSource) -> P.DeclarationRef
        toEfExportValueOp (opName, _) = P.ValueOpRef nullSourceSpan opName

        efImports =
          (Map.toList importedTypes >>= toEfImportType)
            <> (Map.toList importedTypeClasses >>= toEfImportTypeClass)
            <> (Map.toList importedValues >>= toEfImportValue)
            <> (Map.toList importedTypeOps >>= toEfImportTypeOp)
            <> (Map.toList importedValueOps >>= toEfImportValueOp)
            <> (Map.toList importedKinds >>= toEfImportKind)
            <> (Set.toList importedModules <&> toEfImportModule)

        toEfImportType ::
          (P.Qualified (P.ProperName 'P.TypeName), [P.ImportRecord (P.ProperName 'P.TypeName)]) ->
          [P.ExternsImport]
        toEfImportType (P.Qualified (P.ByModuleName mn) name, _ctrs) =
          [ P.ExternsImport
              mn
              (P.Explicit [P.TypeRef nullSourceSpan name Nothing])
              Nothing
          ]
        toEfImportType _ = []

        toEfImportTypeClass :: (P.Qualified (P.ProperName 'P.ClassName), [P.ImportRecord (P.ProperName 'P.ClassName)]) -> [P.ExternsImport]
        toEfImportTypeClass (P.Qualified (P.ByModuleName mn) name, _ctrs) =
          [ P.ExternsImport
              mn
              (P.Explicit [P.TypeClassRef nullSourceSpan name])
              Nothing
          ]
        toEfImportTypeClass _ = []

        toEfImportValue :: (P.Qualified P.Ident, [P.ImportRecord P.Ident]) -> [P.ExternsImport]
        toEfImportValue = \case
          (P.Qualified (P.ByModuleName mn) name, _ctrs) ->
            [ P.ExternsImport
                mn
                (P.Explicit [P.ValueRef nullSourceSpan name])
                Nothing
            ]
          _ -> []

        toEfImportTypeOp :: (P.Qualified (P.OpName 'P.TypeOpName), [P.ImportRecord (P.OpName 'P.TypeOpName)]) -> [P.ExternsImport]
        toEfImportTypeOp = \case
          (P.Qualified (P.ByModuleName mn) name, _ctrs) ->
            [ P.ExternsImport
                mn
                (P.Explicit [P.TypeOpRef nullSourceSpan name])
                Nothing
            ]
          _ -> []

        toEfImportValueOp :: (P.Qualified (P.OpName 'P.ValueOpName), [P.ImportRecord (P.OpName 'P.ValueOpName)]) -> [P.ExternsImport]
        toEfImportValueOp = \case
          (P.Qualified (P.ByModuleName mn) name, _ctrs) ->
            [ P.ExternsImport
                mn
                (P.Explicit [P.ValueOpRef nullSourceSpan name])
                Nothing
            ]
          _ -> []

        toEfImportKind :: (P.Qualified (P.ProperName 'P.TypeName), [P.ImportRecord (P.ProperName 'P.TypeName)]) -> [P.ExternsImport]
        toEfImportKind = \case
          (P.Qualified (P.ByModuleName mn) name, _ctrs) ->
            [ P.ExternsImport
                mn
                (P.Explicit [P.TypeRef nullSourceSpan name Nothing])
                Nothing
            ]
          _ -> []

        toEfImportModule :: P.ModuleName -> P.ExternsImport
        toEfImportModule mn = P.ExternsImport mn P.Implicit Nothing

        efDeclarations :: [P.ExternsDeclaration]
        efDeclarations =
          efExports >>= \case
            P.TypeClassRef _ss name -> pure $ P.EDClass name [] [] [] [] False
            P.TypeOpRef _ss name -> pure $ P.EDValue (P.Ident $ P.runOpName name) P.srcREmpty
            P.TypeRef _ss name _ctrs -> pure $ P.EDType name P.srcREmpty (P.DataType P.Data [] [])
            P.ValueRef _ss name -> pure $ P.EDValue name P.srcREmpty
            P.ValueOpRef _ss name -> pure $ P.EDValue (P.Ident $ P.runOpName name) P.srcREmpty
            _ -> []

--  TypeClassRef SourceSpan (ProperName 'ClassName)
-- -- |
-- -- A type operator
-- --

-- | The data which will be serialized to an externs file
-- data ExternsFile = ExternsFile
--   -- NOTE: Make sure to keep `efVersion` as the first field in this
--   -- record, so the derived Serialise instance produces CBOR that can
--   -- be checked for its version independent of the remaining format
--   { efVersion :: Text
--   -- ^ The externs version
--   , efModuleName :: ModuleName
--   -- ^ Module name
--   , efExports :: [DeclarationRef]
--   -- ^ List of module exports
--   , efImports :: [ExternsImport]
--   -- ^ List of module imports
--   , efFixities :: [ExternsFixity]
--   -- ^ List of operators and their fixities
--   , efTypeFixities :: [ExternsTypeFixity]
--   -- ^ List of type operators and their fixities
--   , efDeclarations :: [ExternsDeclaration]
--   -- ^ List of type and value declaration
--   , efSourceSpan :: SourceSpan
--   -- ^ Source span for error reporting
--   } deriving (Show, Generic, NFData)