{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera.Evergreen.MigrationGeneratorHelpers where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Name as N
import Data.Map.Strict (unionWithKey)

import qualified AST.Canonical as Can
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Interface as Interface

import Lamdera
import Lamdera.Types
import Lamdera.Wire3.Helpers (resolveTvar)
import StandaloneInstances

type TypeName = N.Name
type TypeRef = (ModuleName.Canonical, TypeName)
type RecursionSet = Set.Set (ModuleName.Canonical, N.Name)
type TypeIdentifier = (Pkg.Author, Pkg.Project, N.Name, N.Name)
type TvarMap = [(N.Name, Can.Type)] -- @TODO make it an actual map

data TypeDef
  = Alias ModuleName.Canonical TypeName Can.Alias
  | Union ModuleName.Canonical TypeName Can.Union
  deriving (Show)

-- A specialised local migration def, along with any dependant top-level migrations and their related imports
data Migration = MigrationNested
  { migrationDef :: Text
  , migrationImports :: Set.Set ModuleName.Canonical
  , migrationTopLevelDefs :: MigrationDefinitions
  }
  deriving (Show)

xMigrationNested (a,b,c) = MigrationNested a b c

type MigrationDefinitions = Map TypeRef MigrationDefinition

-- Set of top-level migration functions and their required imports
data MigrationDefinition =
  MigrationDefinition
    { imports :: Set.Set ModuleName.Canonical
    , migrations :: Text
    }
  deriving (Show, Eq)


-- Map <import name> <package>
-- @TODO in future we can use this to pin package versions and adjust import routing to those snapshots
type ElmImports = Set.Set ModuleName.Canonical



-- When non-empty, will pretty-print the producer name & definition for the target top-level migration fn name
debugMigrationFn :: Text
debugMigrationFn = ""

-- When non-empty, will pretty-print the producer name & value for an inline migration containing the search text
debugMigrationIncludes :: Text
debugMigrationIncludes = ""

debugMigrationIncludes_ tag migration =
  migration
    & debugHaskellWhen ( debugMigrationIncludes /= ""
      && (
      -- debugMigrationIncludes `T.isInfixOf` migrationDef migration
      -- ||
      (migration
        & migrationTopLevelDefs
        & Map.toList
        & fmap snd
        & filter (\migrationDefinition ->
            migrationDefinition
              & migrations
              & (\v -> debugMigrationIncludes `T.isInfixOf` v)
          )
        & length
        & (\c -> c > 0)
      )
    )
    )
    ("debugMigrationIncludes:" <> tag)



allMigrations :: MigrationDefinitions -> Text
allMigrations efts =
  efts
    & Map.toList
    & fmap (\(file, ef@(MigrationDefinition imports migrations)) -> migrations )
    & List.sort
    & T.intercalate "\n\n"

importsToText :: Set.Set ModuleName.Canonical -> [Text]
importsToText imports =
  imports
    & Set.toList
    & fmap (\(ModuleName.Canonical (Pkg.Name author project) module_) ->
      "import " <> nameToText module_
    )
    & List.sort

allSubDefs :: [Migration] -> MigrationDefinitions
allSubDefs migrations =
  migrations & foldl (\acc migration ->
    acc & Map.union (migrationTopLevelDefs migration)
  ) Map.empty

allImports :: [Migration] -> ElmImports
allImports migrations = migrations
  & fmap (\migration ->
    migrationImports migration <>
      (migrationTopLevelDefs migration & Map.toList & fmap (imports . snd) & Set.unions)
  )
  & Set.unions

mergeAllSubDefs :: [MigrationDefinitions] -> MigrationDefinitions
mergeAllSubDefs ftss = ftss & foldl (\acc fts -> Map.union acc fts) Map.empty

migrationNameUnderscored :: N.Name -> Int -> Int -> N.Name -> Text
migrationNameUnderscored newModule oldVersion newVersion newTypeName =
  newModule
    & N.toText
    & T.replace ("Evergreen.V" <> show_ newVersion <> ".") ""
    & T.replace ("Evergreen.V" <> show_ oldVersion <> ".") ""
    & T.replace "." "_"
    & (\v -> "migrate_" <> v <> "_" <> nameToText newTypeName)

unimplemented :: Text -> Text -> Migration
unimplemented debugIdentifier message =
  let debugIdentifier_ :: Text = ""
        -- & (\v -> debugIdentifier & suffixIfNonempty " ")
  in
  xMigrationNested (T.concat ["(Unimplemented {- ", debugIdentifier_, message, " -})"], Set.empty, Map.empty)


dropCan :: ModuleName.Canonical -> N.Name
dropCan (ModuleName.Canonical (Pkg.Name author pkg) module_) = module_

moduleKey :: TypeIdentifier -> Text
moduleKey identifier@(author, pkg, module_, tipe) =
  if utf8ToText author == "author" then
    -- Internal package, keep as is
    nameToText module_
  else
    -- External package
    utf8ToText author <> "/" <> utf8ToText pkg <> ":" <> nameToText module_

idTypeName :: TypeIdentifier -> Text
idTypeName identifier@(author, pkg, module_, tipe) = N.toText tipe

moduleNameKey :: ModuleName.Canonical -> Text
moduleNameKey moduleName =
  case moduleName of
    (ModuleName.Canonical (Pkg.Name author pkg) module_) ->
      if author == "author" then
        -- Internal package, keep as is
        nameToText module_
      else
        -- External package
        utf8ToText author <> "/" <> utf8ToText pkg <> ":" <> nameToText module_


getModuleNameUnkeyed :: ModuleName.Canonical -> Text
getModuleNameUnkeyed moduleName =
  case moduleName of
    (ModuleName.Canonical (Pkg.Name author pkg) module_) ->
        nameToText module_

typeNameToStringQualified :: ModuleName.Canonical -> N.Name -> [Can.Type] -> Text
typeNameToStringQualified moduleName tipeName params = do
  let coreType = [nameToText tipeName] ++ parenthesize (fmap qualifiedTypeName params) & T.intercalate " "
  case moduleName of
    (ModuleName.Canonical (Pkg.Name author pkg) module_) ->
      case (author, pkg, module_) of
        ("elm", "core", "Basics") -> coreType
        ("elm", "core", "String") -> coreType
        ("elm", "core", "Maybe") -> coreType
        ("elm", "core", "List") -> coreType
        ("elm", "core", "Set") -> coreType
        ("elm", "core", "Array") -> coreType
        ("elm", "core", "Dict") -> coreType
        ("elm", "core", "Result") -> coreType
        _ ->
          T.concat $ [nameToText module_, ".", nameToText tipeName] ++ (fmap qualifiedTypeName params)


parenthesize :: [Text] -> [Text]
parenthesize texts =
  texts & fmap (\v -> T.concat ["(", v, ")"])


asIdentifier :: Can.Type -> TypeIdentifier
asIdentifier tipe =
  case tipe of
    Can.TType moduleName name params -> asIdentifier_ (moduleName, name)
    Can.TAlias moduleName name _ _ ->   asIdentifier_ (moduleName, name)
    Can.TLambda a b        -> ("elm", "core", "Basics", "<function>")
    Can.TVar a             -> ("elm", "core", "Basics", "a")
    Can.TRecord a b        -> ("elm", "core", "Basics", "{}")
    Can.TUnit              -> ("elm", "core", "Basics", "()")
    Can.TTuple a b c       -> ("elm", "core", "Basics", "Tuple")


typeModuleNameCan :: Can.Type -> ModuleName.Canonical
typeModuleNameCan tipe =
  case tipe of
    Can.TType moduleName name params -> moduleName
    Can.TAlias moduleName name _ _ ->   moduleName
    Can.TLambda a b        -> error $ "used typeModuleName on :" <> show ("elm", "core", "Basics", "<function>")
    Can.TVar a             -> error $ "used typeModuleName on :" <> show ("elm", "core", "Basics", "a")
    Can.TRecord a b        -> error $ "used typeModuleName on :" <> show ("elm", "core", "Basics", "{}")
    Can.TUnit              -> error $ "used typeModuleName on :" <> show ("elm", "core", "Basics", "()")
    Can.TTuple a b c       -> error $ "used typeModuleName on :" <> show ("elm", "core", "Basics", "Tuple")


asIdentifier_ :: (ModuleName.Canonical, N.Name) -> TypeIdentifier
asIdentifier_ pair =
  case pair of
    ((ModuleName.Canonical (Pkg.Name author pkg) module_), typeName) ->
          (author, pkg, module_, typeName)


asTypeName :: Can.Type -> Text
asTypeName tipe =
  case tipe of
    Can.TType moduleName name params -> N.toText name
    Can.TAlias moduleName name _ _ -> N.toText name
    Can.TRecord _ _ -> "anonymousRecord_"
    _ -> error $ "unimplemented asTypeName: " <> show tipe


qualifiedTypeName :: Can.Type -> Text
qualifiedTypeName tipe =
  case tipe of
    Can.TType moduleName name params ->         typeNameToStringQualified moduleName name params
    Can.TAlias moduleName name namedParams _ -> typeNameToStringQualified moduleName name (fmap snd namedParams)
    Can.TLambda a b        -> "<function>"
    Can.TVar a             -> "a"
    Can.TRecord a b        -> "{}"
    Can.TUnit              -> "()"
    Can.TTuple a b mc      ->
      case mc of
        Just c -> T.concat ["(", qualifiedTypeName a, ", ", qualifiedTypeName b, ", ", qualifiedTypeName c, ")"]
        Nothing -> T.concat ["(", qualifiedTypeName a, ", ", qualifiedTypeName b, ")"]


isUserType :: TypeIdentifier -> Bool
isUserType (author, pkg, module_, tipe) =
  author == "author" && pkg == "project"


isPackageType :: Can.Type -> Bool
isPackageType = not . isUserDefinedType_

-- Whether this top level wrapping type is defined by the user,
-- i.e. an alias or custom type, meaning we likely need to migrate it
isUserDefinedType_ :: Can.Type -> Bool
isUserDefinedType_ cType =
  -- (\v ->
  --   if v
  --     then v
  --     else debugHaskellPass "isUserDefinedType_" cType v
  -- ) $
  case cType of
    Can.TType moduleName name params ->
      isUserModule moduleName

    Can.TLambda _ _ -> True
    Can.TVar _ -> True
    Can.TRecord _ _ -> True
    Can.TUnit -> False
    Can.TTuple _ _ _ -> False
    Can.TAlias _ _ _ aType ->
      case aType of
        Can.Holey t -> isUserDefinedType_ t
        Can.Filled t -> isUserDefinedType_ t


laterError = error "fail"


-- @ISSUE this fails to detect custom types, which was the whole point
containsUserTypes :: TvarMap -> Can.Type -> Bool
containsUserTypes tvarMap tipe =
  case resolveTvar tvarMap tipe of
    Can.TType moduleName name params ->
      -- debugHaskellPassWhen (length params /= 0) "containsUserTypes:params" (tipe, tvarMap) $
      isUserModule moduleName
      || (params /= [] && (tvarResolveParams params tvarMap & any (containsUserTypes tvarMap)))

    Can.TAlias moduleName name namedParams aType ->
      case aType of
        Can.Holey t ->
          containsUserTypes tvarMap t
          || (namedParams & fmap snd & (\params -> tvarResolveParams params tvarMap) & any (containsUserTypes tvarMap))
        Can.Filled t ->
          containsUserTypes tvarMap t
          || (namedParams & fmap snd & (\params -> tvarResolveParams params tvarMap) & any (containsUserTypes tvarMap))

    Can.TRecord fields isPartial ->
      fields & Can.fieldsToList & fmap snd & any (containsUserTypes tvarMap)

    Can.TTuple a b mc ->
      containsUserTypes tvarMap a || containsUserTypes tvarMap b || case mc of
        Just c -> containsUserTypes tvarMap c
        Nothing -> False

    Can.TVar a ->
      -- We've already tried to resolve tvars, so this is an unfilled type, this should be impossible
      -- error $ concat ["containsUserTypes: unresolved `", show tipe, "`, please report this issue."]
      True
    Can.TUnit              -> False
    Can.TLambda a b        -> False -- not true but unsupported type


isAnonymousRecord :: Can.Type -> Bool
isAnonymousRecord cType =
  case cType of
    Can.TRecord _ _ -> True
    _ -> False

isRecord :: Can.Type -> Bool
isRecord cType =
  case cType of
    Can.TRecord _ _ -> True
    _ -> False

isTvar :: Can.Type -> Bool
isTvar cType =
  case cType of
    Can.TVar _ -> True
    _ -> False




-- Like == but ignores differences in alias module locations when they are pointing to equivalent types
-- Will NOT find custom types to be equivalent
-- Will NOT resolve tvars


isEquivalentElmType__ :: N.Name -> Can.Type -> Can.Type -> Bool
isEquivalentElmType__ debug t1 t2 = do
  case (t1,t2) of
    (Can.TType moduleName name params, Can.TType moduleName2 name2 params2) ->
        moduleName == moduleName2 && name == name2
    (Can.TAlias moduleName name tvarMap_ aliasType, Can.TAlias moduleName2 name2 tvarMap_2 aliasType2) ->
      case (aliasType, aliasType2) of
        (Can.Holey t1, Can.Holey t2) ->
          isEquivalentElmType__ name t1 t2
        (Can.Filled t1, Can.Filled t2) ->
          isEquivalentElmType__ name t1 t2
        _ ->
          False
    (Can.TRecord fields isPartial, Can.TRecord fields2 isPartial2) ->
      let
        fieldTypes1 :: [Can.Type] = fields & Can.fieldsToList & fmap snd
        fieldTypes2 :: [Can.Type] = fields2 & Can.fieldsToList & fmap snd
      in
      (length fieldTypes1 == length fieldTypes2)
        && (zipWith (isEquivalentElmType__ debug) fieldTypes1 fieldTypes2 & all id)

    (Can.TTuple t1 t2 mt3, Can.TTuple t12 t22 mt32) ->
      t1 == t2
    (Can.TUnit, Can.TUnit) ->
      t1 == t2
    (Can.TVar name, Can.TVar name2) ->
      t1 == t2
    (Can.TLambda _ _, Can.TLambda _ _) ->
      -- Skip lambda equality not relevant
      False
    _ ->
      False





-- 1. ✅ need to rename the below isEquivalentElmType to be something like isEquivalentAppliedType
-- 2. rename all usages and check if the logic is right or should use imaginary above function
-- 2. implement the above, which ignores applied types (tvars)




-- Like == but ignores differences in alias module locations when they are pointing to equivalent types
-- Will NOT find identically defined custom types to be equivalent
-- Will NOT resolve tvars
-- WILL consider applied parameter types
isEquivalentAppliedType :: N.Name -> Can.Type -> Can.Type -> Bool
isEquivalentAppliedType debug t1 t2 = do
  -- debugHaskellPass "isEquivalentElmType" (t1,t2) $
  -- if name /= "unchangedAllTypes"
  --   then
  --   else
    -- debugHaskellPassWhen True
    --   -- (case t1 of
    --   --   Can.TType moduleName name params ->
    --   --     name == debug
    --   --   _ -> False
    --   -- )
    -- -- debug == "migrate_External_Paramed")
    --   "thingy" (
    --     (case t1 of
    --     Can.TType moduleName name params ->
    --       name
    --     _ -> ""
    --   )
    --   ) $
      case (t1,t2) of
        (Can.TType moduleName name params, Can.TType moduleName2 name2 params2) ->

          -- debugHaskell "TType" $
            moduleName == moduleName2 && name == name2 && areEquivalentAppliedElmTypes name params params2
        (Can.TAlias moduleName name tvarMap_ aliasType, Can.TAlias moduleName2 name2 tvarMap_2 aliasType2) ->
          case (aliasType, aliasType2) of
            (Can.Holey t1, Can.Holey t2) ->
              -- debugHaskellPass "TAlias:Holey" (moduleName, moduleName2, name, name2, tvarMap_, tvarMap_2) $
              isEquivalentAppliedType name t1 t2 && areEquivalentAppliedTvarMaps name tvarMap_ tvarMap_2
            (Can.Filled t1, Can.Filled t2) ->
              -- debugHaskellPass "TAlias:Filled" (moduleName, moduleName2, name, name2, tvarMap_, tvarMap_2) $
              isEquivalentAppliedType name t1 t2 && areEquivalentAppliedTvarMaps name tvarMap_ tvarMap_2
            _ ->
              False
        (Can.TRecord fields isPartial, Can.TRecord fields2 isPartial2) ->
          -- debugHaskell "TRecord" $
          -- t1 == t2
          let
            fieldsTypes1 :: [Can.Type] = fields & Can.fieldsToList & fmap snd
            fieldTypes2 :: [Can.Type] = fields2 & Can.fieldsToList & fmap snd
          in
          (length fieldsTypes1 == length fieldTypes2)
            && areEquivalentAppliedElmTypes debug fieldsTypes1 fieldTypes2

        (Can.TTuple t1 t2 mt3, Can.TTuple t12 t22 mt32) ->
          -- debugHaskell "TTuple" $
          t1 == t2
        (Can.TUnit, Can.TUnit) ->
          -- debugHaskell "TUnit" $
          t1 == t2
        (Can.TVar name, Can.TVar name2) ->
          -- debugHaskell "TVar" $
          t1 == t2
        (Can.TLambda _ _, Can.TLambda _ _) ->
          -- Skip lambda equality not relevant
          False
        _ ->
          -- debugHaskell "unequal types" $
          False

areEquivalentAppliedElmTypes :: N.Name -> [Can.Type] -> [Can.Type] -> Bool
areEquivalentAppliedElmTypes debug types1 types2 =
  (length types1 == length types2)
    &&
  (zipWith (isEquivalentAppliedType debug) types1 types2 & all id)

areEquivalentAppliedTvarMaps :: N.Name -> [(N.Name, Can.Type)] -> [(N.Name, Can.Type)] -> Bool
areEquivalentAppliedTvarMaps debug tvars1 tvars2 =
  (length tvars1 == length tvars2)
    &&
  (zipWith (\(n1, t1) (n2, t2) -> n1 == n2 && isEquivalentAppliedType debug t1 t2) tvars1 tvars2 & all id)


isUserModule :: ModuleName.Canonical -> Bool
isUserModule moduleName =
    case moduleName of
        (ModuleName.Canonical (Pkg.Name author pkg) module_) ->
            author == "author" && pkg == "project"

migrationWrapperForType :: N.Name -> Text
migrationWrapperForType t =
  case N.toChars t of
    "BackendModel"  -> "ModelMigrated"
    "FrontendModel" -> "ModelMigrated"
    "FrontendMsg"   -> "MsgMigrated"
    "ToBackend"     -> "MsgMigrated"
    "BackendMsg"    -> "MsgMigrated"
    "ToFrontend"    -> "MsgMigrated"

migrationTypeForType :: N.Name -> Text
migrationTypeForType t =
  case N.toChars t of
    "BackendModel"  -> "ModelMigration"
    "FrontendModel" -> "ModelMigration"
    "FrontendMsg"   -> "MsgMigration"
    "ToBackend"     -> "MsgMigration"
    "BackendMsg"    -> "MsgMigration"
    "ToFrontend"    -> "MsgMigration"

msgForType :: N.Name -> Text
msgForType t =
  case N.toChars t of
    "BackendModel"  -> "BackendMsg"
    "FrontendModel" -> "FrontendMsg"
    "FrontendMsg"   -> "FrontendMsg"
    "ToBackend"     -> "BackendMsg"
    "BackendMsg"    -> "BackendMsg"
    "ToFrontend"    -> "FrontendMsg"

unchangedForType :: N.Name -> Text
unchangedForType t =
  case N.toChars t of
    "BackendModel"  -> "ModelUnchanged"
    "FrontendModel" -> "ModelUnchanged"
    "FrontendMsg"   -> "MsgUnchanged"
    "ToBackend"     -> "MsgUnchanged"
    "BackendMsg"    -> "MsgUnchanged"
    "ToFrontend"    -> "MsgUnchanged"


nothingTODO = Nothing


findTypeDef :: Can.Type -> Interfaces -> Maybe TypeDef
findTypeDef tipe interfaces =
  case tipe of
    Can.TType moduleNameCan typeName params -> findDef moduleNameCan typeName interfaces
    Can.TAlias moduleNameCan typeName _ _ -> findDef moduleNameCan typeName interfaces
    _ -> error $ "findTypeDef used on non-concrete type: " <> show tipe


findDef :: ModuleName.Canonical -> N.Name -> Interfaces -> Maybe TypeDef
findDef moduleNameCan typeName interfaces =
  case Map.lookup (dropCan moduleNameCan) interfaces of
    Just moduleInterface ->
      findDef_ moduleNameCan typeName moduleInterface

    Nothing ->
      Nothing
      -- error $ "could not find old module at all: " <> show (module_, tipe)
      -- error $ "could not find '" <> N.toChars oldModuleName <> "' old module at all: " <> show (Map.keys interfaces)

findDef_ :: ModuleName.Canonical -> N.Name -> Interface.Interface -> Maybe TypeDef
findDef_ moduleNameCan typeName moduleInterface =
  -- let
  --   moduleName :: ModuleName.Canonical
  --   moduleName = ModuleName.Canonical (Interface._home moduleInterface) typeName
  -- in
  case Map.lookup typeName $ Interface._aliases moduleInterface of
    Just aliasInterface ->
        case aliasInterface of
          Interface.PublicAlias a -> Just $ Alias moduleNameCan typeName a
          Interface.PrivateAlias a -> Just $ Alias moduleNameCan typeName a

    Nothing ->
      case Map.lookup typeName $ Interface._unions moduleInterface of
        Just unionInterface -> do
          case unionInterface of
            Interface.OpenUnion u -> Just $ Union moduleNameCan typeName u
            Interface.ClosedUnion u -> Just $ Union moduleNameCan typeName u
            Interface.PrivateUnion u -> Just $ Union moduleNameCan typeName u

        Nothing ->
          -- error $ "could not find old type at all: " <> show (module_, tipe)
          Nothing


asOldModuleName :: N.Name -> Int -> Int -> N.Name
asOldModuleName newModule newVersion oldVersion =
  newModule
    & N.toText
    & T.replace ("Evergreen.V" <> show_ newVersion <> ".") ("Evergreen.V" <> show_ oldVersion <> ".")
    & N.fromText


asOldModule :: N.Name -> Int -> Int -> ModuleName.Canonical
asOldModule newModule newVersion oldVersion =
  ModuleName.Canonical (Pkg.Name "author" "project") (asOldModuleName newModule newVersion oldVersion)


loadTvars :: [N.Name] -> [(N.Name, Can.Type)] -> [Can.Type]
loadTvars tvars tvarMap =
  tvars & fmap (loadTvar tvarMap)

loadTvar :: [(N.Name, Can.Type)] -> N.Name -> Can.Type
loadTvar tvarMap name =
  case List.find (\(t,ti) -> t == name) tvarMap of
    Just (_,ti) -> ti
    Nothing -> error $ "could not resolve tvar, is this a problem?"
      -- This would be the alternative:
      -- Can.Tvar name


tvarResolveParams :: [Can.Type] -> [(N.Name, Can.Type)] -> [Can.Type]
tvarResolveParams params tvarMap =
  params & fmap (tvarResolveParam tvarMap)

tvarResolveParam :: [(N.Name, Can.Type)] -> Can.Type -> Can.Type
tvarResolveParam tvarMap param =
  case param of
    Can.TVar a ->
      case List.find (\(t,ti) -> t == a) tvarMap of
        Just (_,ti) -> ti
        Nothing -> param
    _ -> param


suffixIfNonempty :: Text -> Text -> Text
suffixIfNonempty t s =
  if T.length s == 0 then s else s <> t


noMigration :: Migration
noMigration = xMigrationNested ("", Set.empty, Map.empty)


-- For every additional anonymous nesting, bump the reference number
nextUniqueRef :: Text -> Text
nextUniqueRef oldRef =
  let
    readMaybeInt :: Text -> Maybe Int
    readMaybeInt = readMaybeText
  in
  case oldRef of
    "old" -> "rec"
    _ ->
      oldRef
        & T.replace "rec" ""
        & readMaybeInt
        & withDefault (0 :: Int)
        & (+) 1
        & show_
        & (<>) "rec"