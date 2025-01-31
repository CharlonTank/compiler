module Evergreen.Migrate.V2 exposing (backendModel, backendMsg, frontendModel, frontendMsg, toBackend, toFrontend)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.app/docs/evergreen> for more info.

-}

import Array
import AssocList
import Audio
import Dict
import Evergreen.V1.External
import Evergreen.V1.IncludedByParam
import Evergreen.V1.IncludedBySpecialCasedParam
import Evergreen.V1.Types
import Evergreen.V2.External
import Evergreen.V2.IncludedByParam
import Evergreen.V2.IncludedBySpecialCasedParam
import Evergreen.V2.Types
import Lamdera.Migrations exposing (..)
import List
import Maybe
import Result
import Set


frontendModel : Evergreen.V1.Types.FrontendModel -> ModelMigration Evergreen.V2.Types.FrontendModel Evergreen.V2.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V1.Types.BackendModel -> ModelMigration Evergreen.V2.Types.BackendModel Evergreen.V2.Types.BackendMsg
backendModel old =
    ModelMigrated ( migrate_Types_BackendModel old, Cmd.none )


frontendMsg : Evergreen.V1.Types.FrontendMsg -> MsgMigration Evergreen.V2.Types.FrontendMsg Evergreen.V2.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V1.Types.ToBackend -> MsgMigration Evergreen.V2.Types.ToBackend Evergreen.V2.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V1.Types.BackendMsg -> MsgMigration Evergreen.V2.Types.BackendMsg Evergreen.V2.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V1.Types.ToFrontend -> MsgMigration Evergreen.V2.Types.ToFrontend Evergreen.V2.Types.FrontendMsg
toFrontend old =
    MsgUnchanged


migrate_Types_BackendModel : Evergreen.V1.Types.BackendModel -> Evergreen.V2.Types.BackendModel
migrate_Types_BackendModel old =
    { unchangedCore = old.unchangedCore
    , unchangedUser = old.unchangedUser |> migrate_Types_UserType
    , unchangedAllCoreTypes = old.unchangedAllCoreTypes |> migrate_External_AllCoreTypes
    , unchangedResult = old.unchangedResult
    , unchangedDict = old.unchangedDict
    , unchangedAnonymousRecord =
        old.unchangedAnonymousRecord
            |> (\rec ->
                    { name = rec.name
                    , age = rec.age
                    , userType = rec.userType |> migrate_Types_UserType
                    }
               )
    , unchangedAnonymousRecordNested =
        old.unchangedAnonymousRecordNested
            |> (\rec ->
                    { name = rec.name
                    , subrecord =
                        rec.subrecord
                            |> (\rec1 ->
                                    { age = rec1.age
                                    , userType = rec1.userType |> migrate_Types_UserType
                                    }
                               )
                    }
               )
    , unchangedStringAlias = old.unchangedStringAlias
    , withCustomMaybe = old.withCustomMaybe |> Maybe.map migrate_Types_UserType
    , withCustomList = old.withCustomList |> List.map migrate_Types_UserType
    , withCustomSet = old.withCustomSet |> Set.map (Unimplemented {- Type changed from `Int` to `String`. I need you to write this migration. -})
    , withCustomArray = old.withCustomArray |> Array.map migrate_Types_UserType
    , withCustomDict = old.withCustomDict |> Dict.map (\k -> migrate_Types_UserType)
    , withCustomResult = old.withCustomResult |> Result.mapError migrate_Types_UserType >> Result.map migrate_Types_UserType
    , externalUnion = old.externalUnion |> migrate_External_ExternalUnion
    , added = (Unimplemented {- Type `Int` was added in V2. I need you to set a default value. -})
    , unionThatGetsMoved = old.unionThatGetsMoved |> migrate_External_UnionThatGetsMoved
    , aliasThatGetsMoved = old.aliasThatGetsMoved |> migrate_External_AliasThatGetsMoved
    , typeToAlias = old.typeToAlias |> (Unimplemented {- Type changed from `Evergreen.V1.Types.TypeToAlias` to `Evergreen.V2.Types.TypeToAlias`. I need you to write this migration. -})
    , aliasToType = old.aliasToType |> (Unimplemented {- Type changed from `Evergreen.V1.Types.AliasToType` to `Evergreen.V2.Types.AliasToType`. I need you to write this migration. -})
    , time = old.time
    , userCache = old.userCache |> migrate_AssocList_Dict identity migrate_IncludedBySpecialCasedParam_Custom
    , apps = (Unimplemented {- Type `Dict (String) (Evergreen.V2.Types.App)` was added in V2. I need you to set a default value. -})
    , depthTests = (Unimplemented {- Field of type `Dict (String) (Evergreen.V1.Types.Depth)` was removed in V2. I need you to do something with the `old.depthTests` value if you wish to keep the data, then remove this line. -})
    , removed = (Unimplemented {- Field of type `String` was removed in V2. I need you to do something with the `old.removed` value if you wish to keep the data, then remove this line. -})
    , removedRecord = (Unimplemented {- Field of type `Evergreen.V1.External.AllCoreTypes` was removed in V2. I need you to do something with the `old.removedRecord` value if you wish to keep the data, then remove this line. -})
    }


migrate_Types_FrontendModel : Evergreen.V1.Types.FrontendModel -> Evergreen.V2.Types.FrontendModel
migrate_Types_FrontendModel old =
    { basic = old.basic
    , added = (Unimplemented {- Type `Int` was added in V2. I need you to set a default value. -})
    , url = old.url
    , key = old.key
    }


migrate_Types_FrontendMsg : Evergreen.V1.Types.FrontendMsg -> Evergreen.V2.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    old |> migrate_Audio_Msg migrate_Types_FrontendMsg_


migrate_AssocList_Dict : (a_old -> a_new) -> (b_old -> b_new) -> AssocList.Dict a_old b_old -> AssocList.Dict a_new b_new
migrate_AssocList_Dict migrate_a migrate_b old =
    old
        |> AssocList.toList
        |> List.map (Tuple.mapBoth migrate_a migrate_b)
        |> AssocList.fromList


migrate_Audio_Msg : (userMsg_old -> userMsg_new) -> Audio.Msg userMsg_old -> Audio.Msg userMsg_new
migrate_Audio_Msg migrate_userMsg old =
    old
        |> Audio.migrateMsg (\userMsg_old -> ( migrate_userMsg userMsg_old, Cmd.none ))
        |> Tuple.first


migrate_External_AliasThatGetsMoved : Evergreen.V1.Types.AliasThatGetsMoved -> Evergreen.V2.External.AliasThatGetsMoved
migrate_External_AliasThatGetsMoved old =
    old


migrate_External_AllCoreTypes : Evergreen.V1.External.AllCoreTypes -> Evergreen.V2.External.AllCoreTypes
migrate_External_AllCoreTypes old =
    old


migrate_External_ExternalUnion : Evergreen.V1.External.ExternalUnion -> Evergreen.V2.External.ExternalUnion
migrate_External_ExternalUnion old =
    case old of
        Evergreen.V1.External.External1 ->
            Evergreen.V2.External.External1

        Evergreen.V1.External.External2 ->
            Evergreen.V2.External.External2


migrate_External_Paramed : (a_old -> a_new) -> Evergreen.V1.External.Paramed a_old -> Evergreen.V2.External.Paramed a_new
migrate_External_Paramed migrate_a old =
    { subtype = old.subtype |> migrate_a
    , string = old.string
    }


migrate_External_Paramed2 : (a_old -> a_new) -> (b_old -> b_new) -> Evergreen.V1.External.Paramed2 a_old b_old -> Evergreen.V2.External.Paramed2 a_new b_new
migrate_External_Paramed2 migrate_a migrate_b old =
    { subtype = old.subtype |> migrate_a
    , subtype2 = old.subtype2 |> migrate_b
    , string = old.string
    }


migrate_External_ParamedSub : (x_old -> x_new) -> Evergreen.V1.External.ParamedSub x_old -> Evergreen.V2.External.ParamedSub x_new
migrate_External_ParamedSub migrate_x old =
    { subtypeParamed = old.subtypeParamed |> migrate_External_Paramed migrate_x
    , string = old.string
    }


migrate_External_UnionThatGetsMoved : Evergreen.V1.Types.UnionThatGetsMoved -> Evergreen.V2.External.UnionThatGetsMoved
migrate_External_UnionThatGetsMoved old =
    case old of
        Evergreen.V1.Types.UnionThatGetsMoved ->
            Evergreen.V2.External.UnionThatGetsMoved


migrate_IncludedByParam_Custom : Evergreen.V1.IncludedByParam.Custom -> Evergreen.V2.IncludedByParam.Custom
migrate_IncludedByParam_Custom old =
    case old of
        Evergreen.V1.IncludedByParam.Custom ->
            Evergreen.V2.IncludedByParam.Custom


migrate_IncludedBySpecialCasedParam_Custom : Evergreen.V1.IncludedBySpecialCasedParam.Custom -> Evergreen.V2.IncludedBySpecialCasedParam.Custom
migrate_IncludedBySpecialCasedParam_Custom old =
    case old of
        Evergreen.V1.IncludedBySpecialCasedParam.Custom ->
            Evergreen.V2.IncludedBySpecialCasedParam.Custom


migrate_Types_CustomType : Evergreen.V1.Types.CustomType -> Evergreen.V2.Types.CustomType
migrate_Types_CustomType old =
    case old of
        Evergreen.V1.Types.CustomOne ->
            Evergreen.V2.Types.CustomOne

        Evergreen.V1.Types.CustomTwo ->
            Evergreen.V2.Types.CustomTwo


migrate_Types_FrontendMsg_ : Evergreen.V1.Types.FrontendMsg_ -> Evergreen.V2.Types.FrontendMsg_
migrate_Types_FrontendMsg_ old =
    case old of
        Evergreen.V1.Types.Noop ->
            Evergreen.V2.Types.Noop

        Evergreen.V1.Types.AllCoreTypes p0 ->
            Evergreen.V2.Types.AllCoreTypes (p0 |> migrate_External_AllCoreTypes)


migrate_Types_UserType : Evergreen.V1.Types.UserType -> Evergreen.V2.Types.UserType
migrate_Types_UserType old =
    case old of
        Evergreen.V1.Types.UserFirst ->
            Evergreen.V2.Types.UserFirst

        Evergreen.V1.Types.UserSecond ->
            Evergreen.V2.Types.UserSecond

        Evergreen.V1.Types.UserRemoved ->
            (Unimplemented
             {- `UserRemoved` was removed or renamed in V2 so I couldn't figure out how to migrate it.
                I need you to decide what happens to this Evergreen.V1.Types.UserRemoved value in a migration.
                See https://dashboard.lamdera.app/tips/modified-custom-type for more info.
             -}
            )

        Evergreen.V1.Types.UserWithParam p0 ->
            Evergreen.V2.Types.UserWithParam p0

        Evergreen.V1.Types.UserWithParams p0 p1 p2 ->
            Evergreen.V2.Types.UserWithParams p0 p1 p2

        Evergreen.V1.Types.UserWithParamCustom p0 ->
            Evergreen.V2.Types.UserWithParamCustom (p0 |> migrate_Types_CustomType)

        Evergreen.V1.Types.UserResultP1 p0 ->
            Evergreen.V2.Types.UserResultP1 (p0 |> Result.mapError migrate_Types_CustomType)

        Evergreen.V1.Types.UserResultP2 p0 ->
            Evergreen.V2.Types.UserResultP2 (p0 |> Result.map migrate_Types_CustomType)

        Evergreen.V1.Types.UserResultPBoth p0 ->
            Evergreen.V2.Types.UserResultPBoth (p0 |> Result.mapError migrate_Types_CustomType >> Result.map migrate_External_ExternalUnion)

        Evergreen.V1.Types.UserAnonymous p0 ->
            Evergreen.V2.Types.UserAnonymous
                { name = p0.name
                , userType = p0.userType |> migrate_Types_UserType
                }

        Evergreen.V1.Types.UserAnonymousNested p0 ->
            Evergreen.V2.Types.UserAnonymousNested
                { name = p0.name
                , subrecord =
                    p0.subrecord
                        |> (\rec1 ->
                                { userType = rec1.userType |> migrate_Types_UserType
                                }
                           )
                }

        Evergreen.V1.Types.UserAnonymousNestedAdded p0 ->
            Evergreen.V2.Types.UserAnonymousNestedAdded
                { name = p0.name
                , subrecord =
                    p0.subrecord
                        |> (\rec1 ->
                                { userType = rec1.userType |> migrate_Types_UserType
                                , added = (Unimplemented {- Type `Int` was added in V2. I need you to set a default value. -})
                                }
                           )
                }

        Evergreen.V1.Types.UserAnonymousNestedRemoved p0 ->
            Evergreen.V2.Types.UserAnonymousNestedRemoved
                { name = p0.name
                , subrecord =
                    p0.subrecord
                        |> (\rec1 ->
                                { userType = rec1.userType |> migrate_Types_UserType
                                , removed = (Unimplemented {- Field of type `String` was removed in V2. I need you to do something with the `rec1.removed` value if you wish to keep the data, then remove this line. -})
                                }
                           )
                }

        Evergreen.V1.Types.UserAnonymousNestedAddedRemoved p0 ->
            Evergreen.V2.Types.UserAnonymousNestedAddedRemoved
                { name = p0.name
                , subrecord =
                    p0.subrecord
                        |> (\rec1 ->
                                { userType = rec1.userType |> migrate_Types_UserType
                                , added = (Unimplemented {- Type `Int` was added in V2. I need you to set a default value. -})
                                , removed = (Unimplemented {- Field of type `String` was removed in V2. I need you to do something with the `rec1.removed` value if you wish to keep the data, then remove this line. -})
                                }
                           )
                }

        Evergreen.V1.Types.UserTvarAlias p0 ->
            Evergreen.V2.Types.UserTvarAlias (p0 |> migrate_External_Paramed migrate_Types_CustomType)

        Evergreen.V1.Types.UserTvarAlias2 p0 ->
            Evergreen.V2.Types.UserTvarAlias2 (p0 |> migrate_External_Paramed2 migrate_Types_CustomType migrate_External_AllCoreTypes)

        Evergreen.V1.Types.UserTvarAliasSub p0 ->
            Evergreen.V2.Types.UserTvarAliasSub (p0 |> migrate_External_ParamedSub migrate_IncludedByParam_Custom)

        Evergreen.V1.Types.UserExtTime p0 ->
            Evergreen.V2.Types.UserExtTime p0

        Evergreen.V1.Types.UserExtResultTime p0 ->
            Evergreen.V2.Types.UserExtResultTime p0

        notices ->
            {- @NOTICE `UserAdded` was added in V2.
               This is just a reminder in case migrating some subset of the old data to this new value was important.
               See https://dashboard.lamdera.app/tips/modified-custom-type for more info.
            -}
            {- @NOTICE `UserAddedParam Int` was added in V2.
               This is just a reminder in case migrating some subset of the old data to this new value was important.
               See https://dashboard.lamdera.app/tips/modified-custom-type for more info.
            -}
            (Unimplemented {- New constructors were added. I need you to resolve the above notices and then remove this case. -})
