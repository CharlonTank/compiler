{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}


module Lamdera.Evergreen.MigrationSpecialCases where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import NeatInterpolation

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Data.Name as N

import Lamdera.Evergreen.MigrationGeneratorHelpers


specialCaseMigration identifier =
    case identifier of
      ("pzp1997", "assoc-list", "AssocList", "Dict") ->
        migration identifier
          [text|
            migrate_AssocList_Dict : (a_old -> a_new) -> (b_old -> b_new) -> AssocList.Dict a_old b_old -> AssocList.Dict a_new b_new
            migrate_AssocList_Dict migrate_a migrate_b old =
                Dict.toList old
                    |> List.map (Tuple.mapBoth migrate_a migrate_b)
                    |> Dict.fromList
          |]

      ("mgold", "elm-nonempty-list", "List.Nonempty", "Nonempty") ->
        migration identifier
          [text|
            migrate_List_Nonempty_Nonempty : (a_old -> a_new) -> List.Nonempty.Nonempty a_old -> List.Nonempty.Nonempty a_new
            migrate_List_Nonempty_Nonempty migrate_a old =
                List.Nonempty.map migrate_a old
          |]

      ("MartinSStewart", "elm-audio", "Audio", "Msg") ->
        migration identifier
          [text|
            migrate_Audio_Msg : (userMsg_old -> userMsg_new) -> Audio.Msg userMsg_old -> Audio.Msg userMsg_new
            migrate_Audio_Msg migrate_userMsg old =
                old
                  |> Audio.migrateMsg (\userMsg_old -> (migrate_userMsg userMsg_old, Cmd.none))
                  |> Tuple.first
          |]

      _ ->
        Nothing

migration :: TypeIdentifier -> Text -> Maybe MigrationDefinition
migration (author, pkg, module_, typeName) def =
  Just $ MigrationDefinition
    { imports = Set.singleton $ ModuleName.Canonical (Pkg.Name author pkg) module_
    , migrations = Map.singleton (T.concat ["migrate_", N.toText module_, "_", N.toText typeName]) def
    }


