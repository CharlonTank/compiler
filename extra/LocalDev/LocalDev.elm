port module LocalDev exposing (main)

{-

   Hello you curious thing!

   This is the development harness used for local development of Lamdera apps.

   This file should not be used as a reference for building Lamdera apps, see
   https://dashboard.lamdera.app/docs/building instead.

   The features used by this file are subject to change/removal and should not
   be relied on in any way.

-}
-- import Http
-- import LamderaGenerated
-- import LamderaHelpers exposing (..)

import Backend
import Browser
import Bytes exposing (Bytes)
import Env
import Frontend
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Lazy
import Lamdera exposing (ClientId, Key, SessionId, Url)
import Lamdera.Debug as LD
import Lamdera.Json as Json
import Lamdera.Wire3 as Wire exposing (Bytes)
import Process
import Task exposing (Task)
import Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendMsg, ToBackend, ToFrontend)

-- ADD A NEW TYPE FOR OUR HISTORY
type alias FrontendId =
    { sessionId : String
    , clientId : String
    }

type alias HistoryEntry =
    { kind : MsgKind
    , msg : String
    , fem : FrontendModel
    , bem : Maybe BackendModel
    , source : Maybe FrontendId  -- Which frontend sent this message
    , target : Maybe FrontendId  -- For ToFrontend messages, which frontend it's targeting
    , timestamp : Int            -- To order messages across frontends
    }

type MsgKind
    = KindFrontend
    | KindBackend
    | KindToFrontend
    | KindToBackend

type Location
    = TopLeft
    | TopRight
    | BottomRight
    | BottomLeft

next : Location -> Location
next location =
    case location of
        TopLeft ->
            TopRight

        TopRight ->
            BottomRight

        BottomRight ->
            BottomLeft

        BottomLeft ->
            TopLeft

userFrontendApp =
    Frontend.app

userBackendApp =
    Backend.app

type VersionCheck
    = VersionUnchecked
    | VersionCheckFailed LD.Posix
    | VersionCheckSucceeded String LD.Posix

type alias Model =
    { fem : FrontendModel
    , bem : BackendModel
    , bemDirty : Bool
    , originalUrl : Url
    , originalKey : Key
    , sessionId : String
    , clientId : String
    , nodeType : NodeType
    , devbar : DevBar
    , history : List HistoryEntry
    , currentIndex : Int
    , timeTravelOpen : Bool
    , debuggerWindowOpen : Bool  -- Add this field
    }

type alias DevBar =
    { expanded : Bool
    , location : Location
    , freeze : Bool
    , networkDelay : Bool
    , logging : Bool
    , liveStatus : LiveStatus
    , showModeChanger : Bool
    , showResetNotification : Bool
    , versionCheck : VersionCheck
    , qrCodeShow : Bool
    , snapshotFilenames : List String
    }



-- MKRRI


{-| Injected when parsed by Lamdera/CLI/Live.hs
-}
currentVersion =
    ( 0, 0, 0 )


port send_ToBackend : Bytes -> Cmd msg


port receive_ToBackend : (( SessionId, ClientId, Bytes ) -> msg) -> Sub msg


port save_BackendModel : { t : String, f : Bool, b : Bytes } -> Cmd msg


port send_EnvMode : { t : String, v : String } -> Cmd msg


port send_ToFrontend : WireMsg -> Cmd msg


port receive_ToFrontend : (WireMsg -> msg) -> Sub msg


type alias WireMsg =
    { t : String, s : String, c : String, b : Bytes }



-- @TODO this isn't used currently but needs to adapt for state restore functions?


port receive_BackendModel : (Bytes -> msg) -> Sub msg



-- @LEGCACY END


port setNodeTypeLeader : (Bool -> msg) -> Sub msg


port setLiveStatus : (Bool -> msg) -> Sub msg


port setClientId : (String -> msg) -> Sub msg


port rpcIn : (Json.Value -> msg) -> Sub msg


port rpcOut : Json.Value -> Cmd msg


port onConnection : (ConnectionMsg -> msg) -> Sub msg


port onDisconnection : (ConnectionMsg -> msg) -> Sub msg


type alias ConnectionMsg =
    { s : SessionId, c : ClientId }


type Msg
    = FEMsg Types.FrontendMsg
    | BEMsg Types.BackendMsg
    | BEtoFE ClientId Types.ToFrontend
    | BEtoFEDelayed ClientId Types.ToFrontend
    | FEtoBE Types.ToBackend
    | FEtoBEDelayed Types.ToBackend
    | FENewUrl Url
    | OnConnection { s : String, c : String }
    | OnDisconnection { s : String, c : String }
    | ReceivedToBackend ( String, String, Bytes )
    | ReceivedToFrontend { t : String, b : Bytes, s : String, c : String }
    | ReceivedBackendModel Bytes
    | ReceivedFrontendMsg { s : String, c : String, msg : String }  -- New variant
    | RPCIn Json.Value
    | SetNodeTypeLeader Bool
    | SetLiveStatus Bool
    | ReceivedClientId String
    | ExpandedDevbar
    | CollapsedDevbar
    | ResetDebugStoreBoth
    | ResetDebugStoreFE
    | ResetDebugStoreBE
    | ToggledFreezeMode
    | ToggledNetworkDelay
    | ToggledLogging
    | QRCodeShow
    | QRCodeHide
    | ClickedLocation
    | PersistBackend Bool
    | Reload
    | EnvClicked
    | EnvModeSelected String
    | EnvCleared
    | ModelResetCleared
    | VersionCheck LD.Posix
    | VersionCheckResult (Result LD.HttpError VersionCheck)
    -- Time Travel Messages
    | ToggleTimeTravel
      -- Snapshots
    | LoadLatestSnapshotFilename
    | LoadLatestSnapshotFilenamesResult (Result LD.HttpError (List String))
    | LoadSnapshot String
    | LoadSnapshotLegacy String
    | LoadedSnapshot (Result LD.HttpError ( Bytes, Int ))
    | LoadedSnapshotLegacy (Result LD.HttpError ( List Int, Int ))
    | Noop
    | JumpTo Int


type alias Flags =
    { s : String, c : String, nt : String, b : Maybe Bytes }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ensureOutputInclusion =
            shouldProxy

        log t v =
            if devbar.logging then
                Debug.log t v

            else
                v

        ( ifem, iFeCmds ) =
            userFrontendApp.init url key

        ( ibem, iBeCmds ) =
            userBackendApp.init

        ( fem, newFeCmds ) =
            case LD.debugR "fe" ifem of
                Nothing ->
                    ( ifem, iFeCmds )

                Just rfem ->
                    if devbar.freeze then
                        ( rfem, Cmd.none )

                    else
                        ( ifem, iFeCmds )

        ( bem, newBeCmds, didReset ) =
            case flags.b of
                Nothing ->
                    let
                        _ =
                            Debug.log "☀️ Initializing new app" ""
                    in
                    -- No existing model, brand new app
                    ( ibem, iBeCmds, False )

                Just backendModelBytes ->
                    case Wire.bytesDecode Types.w3_decode_BackendModel backendModelBytes of
                        Just restoredBem ->
                            ( restoredBem
                            , Cmd.none
                            , False
                            )

                        Nothing ->
                            -- Prior backend model has failed to restore, notify the user of a resulting reset
                            ( ibem, iBeCmds, True )

        _ =
            case flags.b of
                Nothing ->
                    bem

                Just backendModelBytes ->
                    if Bytes.width backendModelBytes > 1024 then
                        let
                            -- The backend model is really large now, it's not useful to
                            -- log to the console anymore and slows things down
                            _ =
                                log "☀️ Restored BackendModel <print skipped for 1MB+ model size>" ()
                        in
                        bem

                    else
                        log "☀️ Restored BackendModel" bem

        devbarInit =
            { expanded = False
            , location = BottomLeft
            , freeze = False
            , networkDelay = False
            , logging = True
            , liveStatus = Online
            , showModeChanger = False
            , showResetNotification = didReset
            , versionCheck = VersionUnchecked
            , qrCodeShow = False
            , snapshotFilenames = []
            }

        devbar =
            case LD.debugR "d" devbarInit of
                Nothing ->
                    devbarInit

                Just restoredDevbar ->
                    { restoredDevbar
                      -- Avoid scenario where we persisted while expanded and now
                      -- every refresh it's opening up again without cursor
                        | expanded = False

                        -- If we've just loaded the page, then we must have connectivity,
                        -- so avoid an odd scenario where we persisted debvar while disconnected
                        , liveStatus = Online

                        -- Data might have reset since our last refresh
                        , showResetNotification = didReset
                    }

        nodeType =
            case flags.nt of
                "l" ->
                    Leader

                "f" ->
                    Follower

                _ ->
                    let
                        _ =
                            Debug.log "error" ("decodeNodeType saw an unexpected value: " ++ flags.nt)
                    in
                    Follower
    in
    ( { fem = fem
      , bem = bem
      , bemDirty = True
      , originalKey = key
      , originalUrl = url
      , nodeType = nodeType
      , sessionId = flags.s
      , clientId = flags.c
      , devbar = devbar
      , history = []
      , currentIndex = 0
      , timeTravelOpen = False
      , debuggerWindowOpen = False  -- Initialize as closed
      }
    , Cmd.batch
        [ Cmd.map FEMsg newFeCmds
        , if nodeType == Leader then
            Cmd.map BEMsg newBeCmds

          else
            Cmd.none
        , LD.now |> Task.perform VersionCheck
        ]
    )


storeFE m newFem =
    if m.devbar.freeze then
        LD.debugS "fe" newFem

    else
        newFem


type NodeType
    = Follower
    | Leader


nodeTypeToString nodeType =
    case nodeType of
        Follower ->
            "Follower"

        Leader ->
            "Leader"


type LiveStatus
    = Online
    | Offline


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    let
        log t v =
            if m.devbar.logging then
                Debug.log t v
            else
                v

        withDebugger ( model, cmd ) =
            ( model
            , Cmd.batch [ cmd, sendToDebugger model ]
            )
    in
    withDebugger <| case msg of
        FEMsg frontendMsg ->
            let
                x =
                    log "F   " frontendMsg

                ( newFem, newFeCmds ) =
                    userFrontendApp.update frontendMsg m.fem

                -- Record in the history (only Leader keeps full history)
                newHistoryEntry =
                    { kind = KindFrontend
                    , msg = Debug.toString frontendMsg
                    , fem = newFem
                    , bem = if m.nodeType == Leader then Just m.bem else Nothing
                    , source = Just { sessionId = m.sessionId, clientId = m.clientId }
                    , target = Nothing
                    , timestamp = 0
                    }

                updatedHistory =
                    if m.currentIndex < List.length m.history then
                        -- If we're in the middle of history, truncate it
                        List.take m.currentIndex m.history ++ [ newHistoryEntry ]
                    else
                        m.history ++ [ newHistoryEntry ]
            in
            ( { m 
                | fem = storeFE m newFem
                , history = updatedHistory
                , currentIndex = List.length updatedHistory
              }
            , Cmd.map FEMsg newFeCmds
            )

        BEMsg backendMsg ->
            case m.nodeType of
                Follower ->
                    -- Followers don't run BE messages
                    ( m, Cmd.none )

                Leader ->
                    let
                        x =
                            log "  B " backendMsg

                        ( newBem, newBeCmds ) =
                            userBackendApp.update backendMsg m.bem

                        newHistoryEntry =
                            { kind = KindBackend
                            , msg = Debug.toString backendMsg
                            , fem = m.fem
                            , bem = Just newBem
                            , source = Nothing
                            , target = Nothing
                            , timestamp = 0
                            }

                        updatedHistory =
                            if m.currentIndex < List.length m.history then
                                -- If we're in the middle of history, truncate it
                                List.take m.currentIndex m.history ++ [ newHistoryEntry ]
                            else
                                m.history ++ [ newHistoryEntry ]
                    in
                    withDebugger <|
                        ( { m 
                            | bem = newBem
                            , bemDirty = True
                            , history = updatedHistory
                            , currentIndex = List.length updatedHistory
                          }
                        , Cmd.map BEMsg newBeCmds
                        )

        BEtoFE clientId toFrontend ->
            case m.nodeType of
                Follower ->
                    -- Followers don't broadcast ToFrontends
                    ( m, Cmd.none )

                Leader ->
                    let
                        _ =
                            log " ◀️B " toFrontend

                        newHistoryEntry =
                            { kind = KindToFrontend
                            , msg = Debug.toString toFrontend
                            , fem = m.fem
                            , bem = Just m.bem
                            , source = Nothing
                            , target = Just { sessionId = "", clientId = clientId }
                            , timestamp = 0
                            }

                        updatedHistory =
                            if m.currentIndex < List.length m.history then
                                -- If we're in the middle of history, truncate it
                                List.take m.currentIndex m.history ++ [ newHistoryEntry ]
                            else
                                m.history ++ [ newHistoryEntry ]
                    in
                    if m.devbar.networkDelay then
                        ( { m | history = updatedHistory, currentIndex = List.length updatedHistory }
                        , delay 500 (BEtoFEDelayed clientId toFrontend)
                        )
                    else
                        ( { m | history = updatedHistory, currentIndex = List.length updatedHistory }
                        , Cmd.batch
                            [ send_ToFrontend
                                { t = "ToFrontend", b = toFrontend |> Types.w3_encode_ToFrontend |> Wire.bytesEncode, s = "", c = clientId }
                            ]
                        )

        BEtoFEDelayed clientId toFrontend ->
            case m.nodeType of
                Follower ->
                    ( m, Cmd.none )

                Leader ->
                    ( m
                    , Cmd.batch
                        [ send_ToFrontend
                            { t = "ToFrontend", b = toFrontend |> Types.w3_encode_ToFrontend |> Wire.bytesEncode, s = "", c = clientId }
                        ]
                    )

        FEtoBE toBackend ->
            let
                newHistoryEntry =
                    { kind = KindToBackend
                    , msg = Debug.toString toBackend
                    , fem = m.fem
                    , bem = if m.nodeType == Leader then Just m.bem else Nothing
                    , source = Just { sessionId = m.sessionId, clientId = m.clientId }
                    , target = Nothing
                    , timestamp = 0
                    }

                updatedHistory =
                    if m.currentIndex < List.length m.history then
                        -- If we're in the middle of history, truncate it
                        List.take m.currentIndex m.history ++ [ newHistoryEntry ]
                    else
                        m.history ++ [ newHistoryEntry ]
            in
            if m.devbar.networkDelay then
                ( { m | history = updatedHistory, currentIndex = List.length updatedHistory }
                , delay 500 (FEtoBEDelayed toBackend)
                )
            else
                let
                    _ =
                        log "F▶️  " toBackend
                in
                ( { m | history = updatedHistory, currentIndex = List.length updatedHistory }
                , Cmd.batch [ send_ToBackend (Wire.bytesEncode (Types.w3_encode_ToBackend toBackend)) ]
                )

        FEtoBEDelayed toBackend ->
            let
                _ =
                    log "F▶️ ⏱" toBackend
            in
            ( m, Cmd.batch [ send_ToBackend (Wire.bytesEncode (Types.w3_encode_ToBackend toBackend)) ] )

        FENewUrl url ->
            let
                ( newModel, newCmds ) =
                    update (FEMsg (userFrontendApp.onUrlChange url)) m
            in
            ( { newModel | originalUrl = url }, newCmds )

        OnConnection d ->
            ( m, Lamdera.clientConnected_ d.s d.c )

        OnDisconnection d ->
            ( m, Lamdera.clientDisconnected_ d.s d.c )

        ReceivedToBackend ( s, c, bytes ) ->
            case m.nodeType of
                Follower ->
                    -- Followers don't run BE messages
                    ( m, Cmd.none )

                Leader ->
                    case Wire.bytesDecode Types.w3_decode_ToBackend bytes of
                        Just toBackend ->
                            let
                                _ =
                                    log " ▶️B " toBackend

                                ( newBem, newBeCmds ) =
                                    userBackendApp.updateFromFrontend s c toBackend m.bem

                                newHistoryEntry =
                                    { kind = KindToBackend
                                    , msg = Debug.toString toBackend
                                    , fem = m.fem
                                    , bem = Just newBem
                                    , source = Just { sessionId = s, clientId = c }
                                    , target = Nothing
                                    , timestamp = 0
                                    }

                                updatedHistory =
                                    if m.currentIndex < List.length m.history then
                                        -- If we're in the middle of history, truncate it
                                        List.take m.currentIndex m.history ++ [ newHistoryEntry ]
                                    else
                                        m.history ++ [ newHistoryEntry ]
                            in
                            withDebugger <|
                                ( { m 
                                    | bem = newBem
                                    , bemDirty = True
                                    , history = updatedHistory
                                    , currentIndex = List.length updatedHistory
                                  }
                                , Cmd.map BEMsg newBeCmds
                                )

                        Nothing ->
                            let
                                x =
                                    log "❌ ReceivedToBackend" "failed to decode provided msg!"
                            in
                            ( m, Cmd.none )

        ReceivedToFrontend args ->
            case Wire.bytesDecode Types.w3_decode_ToFrontend args.b of
                Just toFrontend ->
                    let
                        x =
                            log "F◀️  " toFrontend

                        ( newFem, newFeCmds ) =
                            userFrontendApp.updateFromBackend toFrontend m.fem

                        newHistoryEntry =
                            { kind = KindToFrontend
                            , msg = Debug.toString toFrontend
                            , fem = newFem
                            , bem = if m.nodeType == Leader then Just m.bem else Nothing
                            , source = Nothing
                            , target = Just { sessionId = args.s, clientId = args.c }
                            , timestamp = 0
                            }

                        updatedHistory =
                            if m.currentIndex < List.length m.history then
                                -- If we're in the middle of history, truncate it
                                List.take m.currentIndex m.history ++ [ newHistoryEntry ]
                            else
                                m.history ++ [ newHistoryEntry ]
                    in
                    withDebugger <|
                        ( { m 
                            | fem = storeFE m newFem
                            , history = updatedHistory
                            , currentIndex = List.length updatedHistory
                          }
                        , Cmd.map FEMsg newFeCmds
                        )

                Nothing ->
                    let
                        x =
                            log "❌ ReceivedToFrontend" "failed to decode provided msg!"
                    in
                    ( m, Cmd.none )

        ReceivedBackendModel bytes ->
            case Wire.bytesDecode Types.w3_decode_BackendModel bytes of
                Just newBem ->
                    let
                        x =
                            log "❇️ ReceivedBackendModel" newBem
                    in
                    withDebugger <|
                        ( { m | bem = newBem }
                        , Cmd.none
                        )

                Nothing ->
                    let
                        x =
                            log "❌ ReceivedBackendModel" "failed to decode provided msg!"
                    in
                    ( m, Cmd.none )

        RPCIn rpcArgsJson ->
            -- MKRRC
            ( m, Cmd.none )

        --}
        SetNodeTypeLeader bool ->
            ( { m
                | nodeType =
                    case bool of
                        True ->
                            Leader

                        False ->
                            Follower
              }
            , Cmd.none
            )

        SetLiveStatus bool ->
            let
                devbar =
                    m.devbar
            in
            ( { m
                | devbar =
                    { devbar
                        | liveStatus =
                            case bool of
                                True ->
                                    Online

                                False ->
                                    Offline
                    }
              }
            , Cmd.none
            )

        ReceivedClientId clientId ->
            ( { m | clientId = clientId }, Cmd.none )

        ExpandedDevbar ->
            let
                devbar =
                    m.devbar
            in
            ( { m | devbar = { devbar | expanded = True } }, Cmd.none )

        CollapsedDevbar ->
            let
                devbar =
                    m.devbar
            in
            ( { m | devbar = { devbar | expanded = False } }, Cmd.none )

        ResetDebugStoreBoth ->
            let
                ( newFem, newFeCmds ) =
                    userFrontendApp.init m.originalUrl m.originalKey

                ( newBem, newBeCmds ) =
                    userBackendApp.init
            in
            withDebugger <|
                ( { m
                    | fem = LD.debugS "fe" newFem
                    , bem = newBem
                    , bemDirty = True
                  }
                , Cmd.batch
                    [ trigger (PersistBackend True)
                    ]
                )

        ResetDebugStoreFE ->
            let
                ( newFem, newFeCmds ) =
                    userFrontendApp.init m.originalUrl m.originalKey
            in
            withDebugger <|
                ( { m
                    | fem = LD.debugS "fe" newFem
                  }
                , Cmd.batch
                    [ Cmd.map FEMsg newFeCmds
                    ]
                )

        ResetDebugStoreBE ->
            let
                ( newBem, newBeCmds ) =
                    userBackendApp.init
            in
            withDebugger <|
                ( { m
                    | bem = newBem
                    , bemDirty = True
                  }
                , Cmd.batch
                    [ Cmd.map BEMsg newBeCmds
                    , trigger (PersistBackend True)
                    ]
                )

        ToggledFreezeMode ->
            let
                devbar =
                    m.devbar

                newDevbar =
                    { devbar | freeze = not m.devbar.freeze }

                newFem =
                    if newDevbar.freeze then
                        LD.debugS "fe" m.fem

                    else
                        m.fem
            in
            withDebugger <|
                ( { m | devbar = LD.debugS "d" newDevbar, fem = newFem }
                , Cmd.none
                )

        ToggledNetworkDelay ->
            let
                devbar =
                    m.devbar

                newDevbar =
                    { devbar | networkDelay = not m.devbar.networkDelay }
            in
            withDebugger <|
                ( { m | devbar = LD.debugS "d" newDevbar }
                , Cmd.none
                )

        ToggledLogging ->
            let
                devbar =
                    m.devbar

                newDevbar =
                    { devbar | logging = not m.devbar.logging }
            in
            withDebugger <|
                ( { m | devbar = LD.debugS "d" newDevbar }
                , Cmd.none
                )

        QRCodeShow ->
            let
                devbar =
                    m.devbar

                newDevbar =
                    { devbar | qrCodeShow = True }
            in
            withDebugger <|
                ( { m | devbar = newDevbar }
                , Cmd.none
                )

        QRCodeHide ->
            let
                devbar =
                    m.devbar

                newDevbar =
                    { devbar | qrCodeShow = False }
            in
            withDebugger <|
                ( { m | devbar = newDevbar }
                , Cmd.none
                )

        ClickedLocation ->
            let
                devbar =
                    m.devbar

                newDevbar =
                    { devbar | location = next m.devbar.location }
            in
            withDebugger <|
                ( { m | devbar = LD.debugS "d" newDevbar }
                , Cmd.none
                )

        PersistBackend reload ->
            let
                persistBeState nodeType bem =
                    case nodeType of
                        Follower ->
                            Cmd.none

                        Leader ->
                            save_BackendModel { t = "p", f = reload, b = Wire.bytesEncode (Types.w3_encode_BackendModel bem) }
            in
            if m.bemDirty then
                withDebugger <|
                    ( { m | bemDirty = False }
                    , Cmd.batch
                        [ persistBeState m.nodeType m.bem
                        , if reload then
                            delay 200 Reload

                          else
                            Cmd.none
                        ]
                    )

            else
                withDebugger <|
                    ( m, Cmd.none )

        Reload ->
            ( m, LD.browserReload )

        EnvClicked ->
            let
                devbar =
                    m.devbar
            in
            ( { m | devbar = { devbar | showModeChanger = not devbar.showModeChanger } }, Cmd.none )

        EnvModeSelected env ->
            ( m, send_EnvMode { t = "env", v = env } )

        EnvCleared ->
            let
                devbar =
                    m.devbar
            in
            ( { m | devbar = { devbar | showModeChanger = False } }, Cmd.none )

        ModelResetCleared ->
            let
                devbar =
                    m.devbar
            in
            ( { m | devbar = { devbar | showResetNotification = False } }, Cmd.none )

        VersionCheck timeCurrent ->
            let
                check =
                    ( m
                    , getLatestVersion timeCurrent
                        |> Task.onError (\err -> Task.succeed <| VersionCheckFailed timeCurrent)
                        |> Task.attempt VersionCheckResult
                    )

                recheckIfLongerThanHours hours timeOld =
                    if (LD.posixToMillis timeCurrent - LD.posixToMillis timeOld) > (hours * 1000 * 60 * 60) then
                        check

                    else
                        ( m, Cmd.none )
            in
            case m.devbar.versionCheck of
                VersionUnchecked ->
                    check

                VersionCheckFailed t ->
                    recheckIfLongerThanHours 1 t

                VersionCheckSucceeded v t ->
                    recheckIfLongerThanHours 24 t

        VersionCheckResult res ->
            case res of
                Ok val ->
                    let
                        devbar =
                            m.devbar
                    in
                    case val of
                        VersionUnchecked ->
                            -- Not possible, not mapped in decoder
                            ( m, Cmd.none )

                        VersionCheckFailed time ->
                            ( { m | devbar = LD.debugS "d" { devbar | versionCheck = val } }, Cmd.none )

                        VersionCheckSucceeded v time ->
                            ( { m | devbar = LD.debugS "d" { devbar | versionCheck = val } }, Cmd.none )

                Err err ->
                    -- Not possible, errors remapped to VersionCheckFailed
                    ( m, Cmd.none )

        LoadLatestSnapshotFilename ->
            ( m
            , getAppSnapshotFilenames "dashboard"
                |> Task.attempt LoadLatestSnapshotFilenamesResult
            )

        LoadLatestSnapshotFilenamesResult res ->
            case res of
                Ok filenames ->
                    ( { m | devbar = m.devbar |> (\d -> { d | snapshotFilenames = filenames |> List.drop 2 }) }
                    , Cmd.none
                    )

                Err error ->
                    let
                        _ =
                            Debug.log "LoadLatestSnapshotFilenamesResult" error
                    in
                    ( m, Cmd.none )

        LoadSnapshot filename ->
            ( m, getAppSnapshot filename |> Task.attempt LoadedSnapshot )

        LoadSnapshotLegacy filename ->
            ( m, getAppSnapshotLegacy "ascii-collab" filename |> Task.attempt LoadedSnapshotLegacy )

        LoadedSnapshot res ->
            Debug.todo "neutered LoadedSnapshot"

        -- let
        --     evergreenResult =
        --         case res of
        --             Ok ( bytes, version ) ->
        --                 let
        --                     x =
        --                         Debug.log
        --                             ("running LamderaGenerated.decodeAndUpgradeBackendModel for v"
        --                                 ++ String.fromInt version
        --                                 ++ " and bytes size"
        --                             )
        --                             ( version, Bytes.width bytes )
        --                     y =
        --                         LamderaGenerated.debug bytes
        --                             |> Debug.log "manual debug result"
        --                 in
        --                 LamderaGenerated.decodeAndUpgradeBackendModel version bytes
        --             Err err ->
        --                 DecoderError <| Debug.toString err
        --     evergreenTest =
        --         case evergreenResult of
        --             AlreadyCurrent ( valueType, cmds ) ->
        --                 "AlreadyCurrent"
        --             Upgraded ( valueType, cmds ) ->
        --                 "Upgraded"
        --             UnknownVersion ( int, string, bytes_ ) ->
        --                 "UnknownVersion: " ++ String.fromInt int ++ ", " ++ string
        --             UnknownType string ->
        --                 "UnknownType: " ++ string
        --             DecoderError string ->
        --                 "DecoderError: " ++ string
        --     _ =
        --         Debug.log "evergreenResult" evergreenTest
        -- in
        -- case evergreenResult of
        --     AlreadyCurrent ( valueType, cmds ) ->
        --         let
        --             _ =
        --                 Debug.log "NON-MIGRATED RESTORE SUCCESS!" ()
        --         in
        --         ( { m | bem = valueType, bemDirty = True }, Cmd.none )
        --     Upgraded ( valueType, cmds ) ->
        --         let
        --             _ =
        --                 Debug.log "MIGRATION SUCCESS!" ()
        --         in
        --         ( { m | bem = valueType, bemDirty = True }, Cmd.none )
        --     _ ->
        --         ( m, Cmd.none )
        LoadedSnapshotLegacy res ->
            Debug.todo "neutered LoadedSnapshotLegacy"

        -- let
        --     evergreenResult =
        --         case res of
        --             Ok ( intList, version ) ->
        --                 LamderaGenerated.decodeAndUpgradeBackendModel version (Wire2.intListToBytes intList)
        --
        --             Err err ->
        --                 DecoderError <| Debug.toString err
        --
        --     evergreenTest =
        --         case evergreenResult of
        --             AlreadyCurrent ( valueType, cmds ) ->
        --                 "AlreadyCurrent"
        --
        --             Upgraded ( valueType, cmds ) ->
        --                 "Upgraded"
        --
        --             UnknownVersion ( int, string, bytes_ ) ->
        --                 "UnknownVersion: " ++ String.fromInt int ++ ", " ++ string
        --
        --             UnknownType string ->
        --                 "UnknownType: " ++ string
        --
        --             DecoderError string ->
        --                 "DecoderError: " ++ string
        --
        --     _ =
        --         Debug.log "evergreenResult" evergreenTest
        -- in
        -- case evergreenResult of
        --     AlreadyCurrent ( valueType, cmds ) ->
        --         ( { m | bem = valueType }, Cmd.none )
        --
        --     Upgraded ( valueType, cmds ) ->
        --         let
        --             _ =
        --                 Debug.log "RESTORING SUCCESS DECODE!!!!!" ()
        --         in
        --         ( { m | bem = valueType }, Cmd.none )
        --
        --     _ ->
        --         ( m, Cmd.none )
        Noop ->
            ( m, Cmd.none )

        ToggleTimeTravel ->
            if m.debuggerWindowOpen then
                ( { m | debuggerWindowOpen = False }
                , closeDebuggerWindow ()
                )
            else
                ( { m | debuggerWindowOpen = True }
                , Cmd.batch
                    [ openDebuggerWindow { width = 800, height = 600 }
                    , updateDebugger 
                        { history = List.map historyToJson m.history
                        , currentIndex = m.currentIndex
                        }
                    ]
                )

        JumpTo index ->
            if index >= 0 && index < List.length m.history then
                case List.drop index m.history |> List.head of
                    Just entry ->
                        ( { m
                            | fem = entry.fem
                            , bem = Maybe.withDefault m.bem entry.bem
                            , currentIndex = index
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( m, Cmd.none )
            else
                ( m, Cmd.none )

        ReceivedFrontendMsg args ->
            case m.nodeType of
                Follower ->
                    -- Only Leader records frontend messages
                    ( m, Cmd.none )

                Leader ->
                    let
                        newHistoryEntry =
                            { kind = KindFrontend
                            , msg = args.msg
                            , fem = m.fem  -- Keep Leader's frontend model
                            , bem = Just m.bem
                            , source = Just { sessionId = args.s, clientId = args.c }
                            , target = Nothing
                            , timestamp = 0
                            }

                        updatedHistory =
                            if m.currentIndex < List.length m.history then
                                -- If we're in the middle of history, truncate it
                                List.take m.currentIndex m.history ++ [ newHistoryEntry ]
                            else
                                m.history ++ [ newHistoryEntry ]
                    in
                    withDebugger <|
                        ( { m 
                            | history = updatedHistory
                            , currentIndex = List.length updatedHistory
                          }
                        , Cmd.none
                        )


subscriptions { nodeType, fem, bem, bemDirty } =
    Sub.batch
        [ Sub.map FEMsg (userFrontendApp.subscriptions fem)
        , if nodeType == Leader then
            Sub.map BEMsg (userBackendApp.subscriptions bem)
          else
            Sub.none
        , if nodeType == Leader && bemDirty then
            LD.every 1000 (always (PersistBackend False))
          else
            Sub.none
        , setNodeTypeLeader SetNodeTypeLeader
        , setLiveStatus SetLiveStatus
        , setClientId ReceivedClientId
        , receive_ToBackend ReceivedToBackend
        , receive_ToFrontend ReceivedToFrontend
        , receive_BackendModel ReceivedBackendModel
        , rpcIn RPCIn
        , onConnection OnConnection
        , onDisconnection OnDisconnection
        , LD.every (10 * 60 * 1000) VersionCheck
        , debuggerWindowClosed (always (ToggleTimeTravel))
        ]


xForLocation location =
    case location of
        TopLeft ->
            style "left" "5px"

        TopRight ->
            style "right" "5px"

        BottomRight ->
            style "right" "5px"

        BottomLeft ->
            style "left" "5px"


yForLocation location =
    case location of
        TopLeft ->
            style "top" "5px"

        TopRight ->
            style "top" "5px"

        BottomRight ->
            style "bottom" "5px"

        BottomLeft ->
            style "bottom" "5px"


lamderaUI :
    DevBar
    -> NodeType
    -> Model
    -> List (Html Msg)
lamderaUI devbar nodeType model =
    case devbar.liveStatus of
        Online ->
            [ Html.Lazy.lazy2 lamderaPane devbar nodeType
            , Html.Lazy.lazy envModeChanger devbar.showModeChanger
            , Html.Lazy.lazy resetNotification devbar.showResetNotification
            , if model.timeTravelOpen then
                renderTimeTravelUi model
              else
                text ""
            ]

        Offline ->
            [ withOverlay Noop
                [ div
                    [ onClick ClickedLocation
                    , style "padding" "10px"
                    , style "display" "flex"
                    , style "justify-content" "center"
                    , style "align-items" "center"
                    , style "color" white
                    , style "background-color" charcoal
                    , style "border-radius" "5px"
                    , style "z-index" "1000"
                    ]
                    [ icon iconWarning 18 yellow
                    , spacer 8
                    , text "lamdera live is not running!"
                    ]
                ]
            ]


envModeChanger showModeChanger =
    if showModeChanger then
        withOverlay EnvCleared
            [ div
                [ style "padding" "10px"
                , style "color" white
                , style "background-color" charcoal
                , style "border-radius" "5px"
                , id "lamdera-env"
                ]
                [ div [] [ Html.node "style" [] [ text """
                    #lamdera-env .lamdera-dev:hover {
                      background-color: #85BC7A20
                    }
                    #lamdera-env .lamdera-preview:hover {
                      background-color: #4196ad20
                    }
                    #lamdera-env .lamdera-prod:hover {
                      background-color: #E06C7520
                    }
                  """ ] ]
                , div [ style "padding" "2px" ] [ text "Select `Env.mode` value:" ]
                , div
                    [ onClick (EnvModeSelected "Development")
                    , style "cursor" "pointer"
                    , style "padding" "6px 6px"
                    , style "margin" "4px 2px"
                    , style "border-left" "3px solid #85BC7A"
                    , class "lamdera-dev"
                    ]
                    [ text "Development" ]

                -- , div
                --     [ onClick (EnvModeSelected "Preview")
                --     , style "cursor" "pointer"
                --     , style "padding" "6px 6px"
                --     , style "margin" "4px 2px"
                --     , style "border-left" "3px solid #4196ad"
                --     , class "lamdera-preview"
                --     ]
                --     [ text "Preview" ]
                , div
                    [ onClick (EnvModeSelected "Production")
                    , style "cursor" "pointer"
                    , style "padding" "6px 6px"
                    , style "margin" "4px 2px"
                    , style "border-left" "3px solid #E06C75"
                    , class "lamdera-prod"
                    ]
                    [ text "Production" ]
                ]
            ]

    else
        text ""


resetNotification showReset =
    if showReset then
        withOverlay ModelResetCleared
            [ div
                [ onClick ClickedLocation
                , style "padding" "10px 20px"
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "color" white
                , style "background-color" charcoal
                , style "border-radius" "5px"
                ]
                [ icon iconWarning 18 yellow
                , spacer 8
                , div [ style "text-align" "center" ]
                    [ div [ style "padding" "5px" ] [ text "It looks like your BackendModel type has changed!" ]
                    , div [ style "padding" "5px" ] [ text "I've reset the BackendModel to its init value." ]
                    , div
                        [ onClick ModelResetCleared
                        , style "padding" "8px 20px"
                        , style "margin" "5px"
                        , style "color" white
                        , style "background-color" grey
                        , style "border-radius" "5px"
                        , style "display" "inline-block"
                        , style "cursor" "pointer"
                        ]
                        [ text "Okay" ]
                    ]
                ]
            ]

    else
        text ""


lamderaPane devbar nodeType =
    div
        [ style "font-family" "system-ui, Helvetica Neue, sans-serif"
        , style "font-size" "12px"
        , style "position" "fixed"
        , xForLocation devbar.location
        , yForLocation devbar.location
        , style "color" white
        , style "background-color" charcoal
        , style "border-radius" "5px"
        , onMouseEnter ExpandedDevbar
        , onMouseLeave CollapsedDevbar
        , style "user-select" "none"
        ]
        (case devbar.location of
            TopLeft ->
                lamderaDevBar True devbar nodeType

            TopRight ->
                lamderaDevBar True devbar nodeType

            BottomRight ->
                lamderaDevBar False devbar nodeType

            BottomLeft ->
                lamderaDevBar False devbar nodeType
        )


withOverlay dismiss html =
    div
        [ style "font-family" "system-ui, Helvetica Neue, sans-serif"
        , style "font-size" "14px"
        , style "display" "block"
        , style "position" "fixed"
        , style "top" "0"
        , style "left" "0"
        , style "height" "100vh"
        , style "width" "100vw"
        , style "background-color" "#2e333588"
        , style "-webkit-backdrop-filter" "blur(3px)"
        , style "backdrop-filter" "blur(3px)"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , onClick dismiss
        ]
        html


envIndicator =
    let
        ( label, color ) =
            envMeta
    in
    div []
        [ div
            [ style "text-align" "center"
            , style "border-top" "1px solid #393939"
            , style "border-radius" "0px 0px 5px 5px"
            , style "font-size" "10px"
            , style "padding" "1px 4px 2px 4px"
            , style "cursor" "pointer"
            , style "color" "#fff"
            , onClick EnvClicked
            ]
            [ text <| "Env: ", span [ style "color" color ] [ text label ] ]
        ]


envMeta =
    case Env.mode of
        Env.Production ->
            ( "Prod", red )

        -- Env.Preview ->
        --     ( "Preview", blue )
        Env.Development ->
            ( "Dev", green )


lamderaDevBar topDown devbar nodeType =
    case topDown of
        True ->
            [ pill devbar nodeType
            , if devbar.expanded then
                div
                    [ style "border-top" "1px solid #393939"
                    ]
                    [ expandedUI topDown devbar nodeType
                    ]

              else
                text ""
            ]

        False ->
            [ if devbar.expanded then
                div
                    [ style "border-bottom" "1px solid #393939"
                    , style "padding-bottom" "5px"
                    ]
                    [ expandedUI topDown devbar nodeType ]

              else
                text ""
            , pill devbar nodeType
            ]


pill devbar nodeType =
    div []
        [ div
            [ style "padding" "5px 7px 2px 5px"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            ]
            [ span
                [ onClick ClickedLocation
                , style "margin" "-2px 5px 0 5px"
                , style "display" "inline-block"
                , style "vertical-align" "middle"
                , style "height" "22px"
                , style "width" "13px"
                , style "cursor" "pointer"
                , style "background-image" """url("data:image/svg+xml;utf8,<svg width='13px' height='23px' viewBox='0 0 23 27' version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'><g stroke='none' stroke-width='1' fill='none' fill-rule='evenodd'><g transform='translate(-272.000000, -129.000000)' fill='white'><g transform='translate(272.000000, 129.000000)'><path d='M22.721133,26.2285714 C22.3975092,26.7428597 21.8359081,27 21.1720266,27 C20.745075,26.9311782 20.4000491,26.7155717 20.1369487,26.3531804 C19.9207409,26.049077 19.4876467,25.1169484 18.8376663,23.5567944 L11.48425,6.00209059 L3.14198812,25.9651568 C2.85432248,26.6550557 2.3569081,27 1.64973006,27 C1.42199476,27 1.20025582,26.9498263 0.984506591,26.8494774 C0.564994195,26.6613231 0.277332868,26.3477374 0.121513978,25.9087108 C-0.0462909803,25.4696842 -0.040298036,25.0306642 0.139492991,24.5916376 L9.99199199,1.03484321 C10.2796576,0.344944286 10.777072,0 11.48425,0 C12.2034142,0 12.7068215,0.344944286 12.9944871,1.03484321 L22.8469861,24.5916376 C23.0867075,25.1561004 23.0447569,25.7017395 22.721133,26.2285714 Z'></path></g></g></g></svg>")"""
                , style "position" "relative"
                ]
                [ case nodeType of
                    Leader ->
                        div
                            [ style "background-color" "#a6f098"
                            , style "height" "4px"
                            , style "width" "4px"
                            , style "border-radius" "10px"
                            , style "position" "absolute"
                            , style "top" "3px"
                            , style "left" "-3px"
                            ]
                            []

                    Follower ->
                        text ""
                ]
            , spacer 5
            , if devbar.freeze then
                summaryIcon iconFreeze blue ToggledFreezeMode

              else
                summaryIcon iconFreeze grey ToggledFreezeMode
            , spacer 5
            , if devbar.networkDelay then
                summaryIcon iconNetwork yellow ToggledNetworkDelay

              else
                summaryIcon iconNetwork grey ToggledNetworkDelay
            , spacer 5
            , if devbar.logging then
                summaryIcon iconLogs white ToggledLogging

              else
                summaryIcon iconLogs grey ToggledLogging
            ]
        , envIndicator
        , case devbar.versionCheck of
            VersionUnchecked ->
                text ""

            VersionCheckFailed time ->
                text ""

            VersionCheckSucceeded version time ->
                let
                    latestVersion =
                        version
                            |> String.split "-"
                            |> (\p ->
                                    case p of
                                        ev :: lv :: _ ->
                                            lv
                                                |> String.split "."
                                                |> List.map String.toInt
                                                |> justs
                                                |> (\parts ->
                                                        case parts of
                                                            v1 :: v2 :: v3 :: [] ->
                                                                ( v1, v2, v3 )

                                                            _ ->
                                                                ( 0, 0, 0 )
                                                   )

                                        _ ->
                                            ( 0, 0, 0 )
                               )

                    newVersionUi =
                        div
                            [ style "text-align" "center"
                            , style "font-size" "10px"
                            , style "background-color" "#8E4CD0"
                            , style "padding" "4px"
                            , style "border-radius" "0 0 5px 5px"
                            , style "box-shadow" "inset 0px 3px 3px -3px rgba(0,0,0,1)"
                            , style "border-top" "1px solid #555"
                            ]
                            [ buttonDevLink "New version!" "https://dashboard.lamdera.app/docs/download" white
                            ]
                in
                if String.contains "wip" version then
                    text ""

                else if latestVersion > currentVersion then
                    newVersionUi

                else
                    text ""
        ]


summaryIcon icon_ color msg =
    span [ onClick msg, style "cursor" "pointer" ] [ icon icon_ 16 color ]


spacer width =
    -- If only we had elm-ui :'(
    span [ style "width" (String.fromInt width ++ "px"), style "display" "inline-block" ] []


expandedUI topDown devbar nodeType =
    let
        modeText =
            case devbar.freeze of
                False ->
                    "Inactive"

                True ->
                    "Active"

        envDocs =
            let
                borderPos =
                    if topDown then
                        "border-top"

                    else
                        "border-bottom"
            in
            div
                [ style "display" "flex"
                , style "justify-content" "space-evenly"
                , style borderPos "1px solid #393939"
                ]
                [ let
                    ( label, color ) =
                        envMeta
                  in
                  buttonDevColored "Env" label color EnvClicked
                , div [ style "height" "30px", style "width" "1px", style "background-color" "#393939" ] []
                , buttonDevLink "Docs" "https://dashboard.lamdera.app/docs" white
                ]

        versionInfo =
            let
                ( borderPos, borderRadius ) =
                    if topDown then
                        ( "border-top", "0 0 5px 5px" )

                    else
                        ( "border-bottom", "5px 5px 0 0" )
            in
            div
                [ style "text-align" "center"
                , style "font-size" "10px"
                , style "background-color" "#222"
                , style "color" "#888"
                , style "border-radius" borderRadius
                , style "padding" "4px"
                , style borderPos "1px solid #393939"
                ]
                [ text <| "Version: " ++ showVersion currentVersion
                ]
    in
    div [ style "width" "175px" ]
        [ if topDown then
            text ""

          else
            div [] [ versionInfo, envDocs ]
        , case nodeType of
            Leader ->
                div []
                    [ case devbar.freeze of
                    False ->
                        buttonDev "Reset Backend" ResetDebugStoreBE

                    True ->
                        buttonDev "Reset Both" ResetDebugStoreBoth
                    , buttonDev "Time Travel" ToggleTimeTravel
                    ]

            Follower ->
                div [ style "padding" "8px 8px", style "text-align" "center" ] [ text "Use leader tab (green dot) for reset options" ]
        , if devbar.freeze then
            buttonDev "Reset Frontend" ResetDebugStoreFE

          else
            text ""
        , case devbar.freeze of
            False ->
                buttonDevOff "Freeze Mode: Off" iconFreeze ToggledFreezeMode

            True ->
                buttonDevColoredIcon "Freeze Mode" "On" blue iconFreeze ToggledFreezeMode

        , case devbar.networkDelay of
            True ->
                buttonDevColoredIcon "Network Delay" "500ms" yellow iconNetwork ToggledNetworkDelay

            False ->
                buttonDevOff "Network Delay: Off" iconNetwork ToggledNetworkDelay
        , case devbar.logging of
            True ->
                buttonDevColoredIcon "Logging" "On" white iconLogs ToggledLogging

            False ->
                buttonDevOff "Logging: Off" iconLogs ToggledLogging
        , if topDown then
            div [] [ envDocs, versionInfo ]

          else
            text ""
        ]


lamderaSnapshots devbar =
    Html.div []
        ([ Html.div [ onClick LoadLatestSnapshotFilename ] [ Html.text "Load latest snapshots" ]
         ]
            ++ List.map (\f -> Html.div [ onClick (LoadSnapshot f) ] [ Html.text f ]) devbar.snapshotFilenames
         -- ++ List.map
         --     (\f ->
         --         Html.div
         --             [ onClick (LoadSnapshotLegacy f)
         --             ]
         --             [ Html.text <| "LEGACY: " ++ f ]
         --     )
         --     [ "appname_v9_1687430149455.bin" ]
        )


buttonDev label msg =
    div
        [ style "color" white
        , style "cursor" "pointer"
        , style "padding" "8px 8px"
        , style "text-align" "center"
        , onClick msg
        ]
        [ text label
        ]


buttonDevColored label value color msg =
    div
        [ style "color" white
        , style "cursor" "pointer"
        , style "padding" "8px 8px"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , onClick msg
        ]
        [ text label
        , text ":"
        , spacer 3
        , span [ style "color" color ] [ text value ]
        ]


buttonDevColoredIcon label value color icon_ msg =
    div
        [ style "color" white
        , style "cursor" "pointer"
        , style "padding" "8px 8px"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , onClick msg
        ]
        [ icon icon_ 16 color
        , spacer 5
        , text label
        , text ":"
        , spacer 3
        , span [ style "color" color ] [ text value ]
        ]


buttonDevOff label icon_ msg =
    div
        [ style "color" grey
        , style "cursor" "pointer"
        , style "padding" "8px 8px"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , onClick msg
        ]
        [ icon icon_ 16 grey
        , spacer 5
        , text label
        ]


buttonDevLink label url color =
    a
        [ style "color" color
        , style "cursor" "pointer"
        , style "text-decoration" "none"
        , style "display" "block"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , href url
        , target "_blank"
        ]
        [ text label
        , spacer 5
        , icon iconExternalLink 12 color
        ]


mapDocument : Model -> (FrontendMsg -> Msg) -> Browser.Document FrontendMsg -> Browser.Document Msg
mapDocument model msg { title, body } =
    { title = title
    , body =
        List.map (Html.map msg) body
            ++ lamderaUI
                model.devbar
                model.nodeType
                model
    }


icon fn size hex =
    div
        [ style "display" "inline-block"
        , style "height" (String.fromInt size ++ "px")
        , style "width" (String.fromInt size ++ "px")
        , style "background-image" (fn size hex)
        ]
        []


iconWarning size hex =
    """url("data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='"""
        ++ String.fromInt size
        ++ """' height='"""
        ++ String.fromInt size
        ++ """' viewBox='0 0 24 24' fill='none' stroke='"""
        ++ String.replace "#" "%23" hex
        ++ """' stroke-width='2' stroke-linecap='round' stroke-linejoin='round' class='feather feather-alert-triangle'><path d='M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z'></path><line x1='12' y1='9' x2='12' y2='13'></line><line x1='12' y1='17' x2='12.01' y2='17'></line></svg>")"""


iconNetwork size hex =
    """url("data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='"""
        ++ String.fromInt size
        ++ """' height='"""
        ++ String.fromInt size
        ++ """' viewBox='0 0 24 24' fill='none' stroke='"""
        ++ String.replace "#" "%23" hex
        ++ """' stroke-width='2' stroke-linecap='round' stroke-linejoin='round' class='feather feather-zap'><polygon points='13 2 3 14 12 14 11 22 21 10 12 10 13 2'></polygon></svg>")"""


iconFreeze size hex =
    """url("data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='"""
        ++ String.fromInt size
        ++ """' height='"""
        ++ String.fromInt size
        ++ """' viewBox='0 0 24 24' fill='none' stroke='"""
        ++ String.replace "#" "%23" hex
        ++ """' stroke-width='2' stroke-linecap='round' stroke-linejoin='round' > <path d='M12 2v6.5M10 4l2 1 2-1M3.3 7L9 10.2m-4.9-.5L6 8.5l.1-2.2M3.3 17L9 13.7m-2.9 4L6 15.5l-1.9-1.2M12 22v-6.5m2 4.5l-2-1-2 1m5-6.2l5.6 3.3m-.7-2.8L18 15.5l-.1 2.2M20.7 7L15 10.3m2.9-4l.1 2.2 1.9 1.2M12 8.5l3 1.8v3.5l-3 1.8-3-1.8v-3.5l3-1.8z' /></svg>")"""


iconLogs size hex =
    """url("data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='"""
        ++ String.fromInt size
        ++ """' height='"""
        ++ String.fromInt size
        ++ """' viewBox='0 0 24 24' fill='none' stroke='"""
        ++ String.replace "#" "%23" hex
        ++ """' stroke-width='2' stroke-linecap='round' stroke-linejoin='round' class='feather feather-align-justify'><line x1='21' y1='10' x2='3' y2='10'></line><line x1='21' y1='6' x2='3' y2='6'></line><line x1='21' y1='14' x2='3' y2='14'></line><line x1='21' y1='18' x2='3' y2='18'></line></svg>")"""


iconExternalLink size hex =
    """url("data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='"""
        ++ String.fromInt size
        ++ """' height='"""
        ++ String.fromInt size
        ++ """' viewBox='0 0 24 24' fill='none' stroke='"""
        ++ String.replace "#" "%23" hex
        ++ """' stroke-width='2' stroke-linecap='round' stroke-linejoin='round' class='feather feather-external-link'><path d='M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6'></path><polyline points='15 3 21 3 21 9'></polyline><line x1='10' y1='14' x2='21' y2='3'></line></svg>")"""


red =
    "#E06C75"


green =
    "#85BC7A"


blue =
    "#4196AD"


yellow =
    "#FFCB64"


darkYellow =
    "#C98F1B"


white =
    "#EEE"


grey =
    "#666"


charcoal =
    "#2e3335"


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view =
            \model ->
                mapDocument model FEMsg (userFrontendApp.view model.fem)
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \ureq -> FEMsg (userFrontendApp.onUrlRequest ureq)
        , onUrlChange = \url -> FENewUrl url
        }


delay time msg =
    Process.sleep time |> Task.perform (always msg)


trigger msg =
    Process.sleep 0 |> Task.perform (always msg)


required : String -> Json.Decoder a -> Json.Decoder (a -> b) -> Json.Decoder b
required key valDecoder decoder =
    custom (Json.field key valDecoder) decoder


custom : Json.Decoder a -> Json.Decoder (a -> b) -> Json.Decoder b
custom =
    Json.map2 (|>)


getLatestVersion : LD.Posix -> Task LD.HttpError VersionCheck
getLatestVersion time =
    LD.task
        { method = "GET"
        , headers = []
        , url = "http://localhost:8001/https://static.lamdera.com/bin/latest-version.json"
        , body = LD.emptyBody
        , resolver = LD.stringResolver <| LD.handleJsonResponse <| decodeVersionCheck time
        , timeout = Nothing
        }


decodeVersionCheck : LD.Posix -> Json.Decoder VersionCheck
decodeVersionCheck time =
    Json.decoderString
        |> Json.map (\v -> VersionCheckSucceeded v time)


getAppSnapshotFilenames : String -> Task LD.HttpError (List String)
getAppSnapshotFilenames appId =
    LD.task
        { method = "GET"
        , headers = []

        -- , url = "http://apps.lamdera.com:8080/v1/app/" ++ appId ++ "/snapshots"
        -- , url = "http://localhost:8080/v1/app/" ++ appId ++ "/snapshots"
        , url = "/_x/list/snapshots"
        , body = LD.emptyBody
        , resolver = LD.stringResolver <| LD.handleJsonResponse <| Json.decoderList Json.decoderString
        , timeout = Nothing
        }
        |> Task.map List.sort


getAppSnapshot : String -> Task LD.HttpError ( Wire.Bytes, Int )
getAppSnapshot snapshot =
    Debug.todo "neutered"



-- let
--     token =
--         "XXXXX"
--     ( appId, version ) =
--         snapshot
--             |> String.split "_"
--             |> (\res ->
--                     case res of
--                         appId_ :: version_ :: _ ->
--                             ( appId_
--                             , version_
--                                 |> String.replace "v" ""
--                                 |> String.toInt
--                                 |> Debug.log "parsed version as:"
--                                 |> Maybe.withDefault -1
--                             )
--                         _ ->
--                             Debug.todo ("impossible... bad snapshot name: " ++ snapshot)
--                )
-- in
-- LD.task
--     { method = "GET"
--     , headers = []
--     -- , url = "http://apps.lamdera.com:8080/v1/app/" ++ appId ++ "/snapshot-retrieve/" ++ snapshot ++ "/" ++ token
--     -- , url = "http://localhost:8080/v1/app/" ++ appId ++ "/snapshot-retrieve/" ++ snapshot ++ "/" ++ token
--     , url = "/_x/read/" ++ "snapshots/" ++ snapshot
--     , body = LD.emptyBody
--     , resolver = Http.bytesResolver handleBytesResponse
--     , timeout = Nothing
--     }
--     |> Task.map (\bytes -> ( bytes, version ))


getAppSnapshotLegacy : String -> String -> Task LD.HttpError ( List Int, Int )
getAppSnapshotLegacy appId snapshot =
    Debug.todo "neutered getAppSnapshotLegacy"



-- let
--     token =
--         "XXXXX"
--
--     version =
--         snapshot
--             |> String.replace (appId ++ "-v") ""
--             |> String.split "-"
--             |> List.head
--             |> Maybe.andThen String.toInt
--             |> Debug.log "parsed version as:"
--             |> Maybe.withDefault -1
-- in
-- LD.task
--     { method = "GET"
--     , headers = []
--     , url = "http://apps.lamdera.com:8080/v1/app/" ++ appId ++ "/snapshot-retrieve-legacy/" ++ snapshot ++ "/" ++ token
--
--     -- , url = "http://localhost:8080/v1/app/" ++ appId ++ "/snapshot-retrieve-legacy/" ++ snapshot ++ "/" ++ token
--     , body = LD.emptyBody
--     , resolver =
--         LD.stringResolver
--             (\response ->
--                 case response of
--                     Http.BadUrl_ urlString ->
--                         Err <| Http.BadUrl urlString
--
--                     Http.Timeout_ ->
--                         Err <| Http.Timeout
--
--                     Http.NetworkError_ ->
--                         Err <| Http.NetworkError
--
--                     Http.BadStatus_ metadata body ->
--                         -- @TODO use metadata better here
--                         Err <| Http.BadStatus metadata.statusCode
--
--                     Http.GoodStatus_ metadata text ->
--                         case Json.decodeString Json.decoderString text of
--                             Ok s ->
--                                 case Json.decodeString (Json.decoderList Json.decoderInt) s of
--                                     Ok x ->
--                                         Ok x
--
--                                     Err err ->
--                                         Err <| Http.BadBody <| "Failed to decode response: " ++ Json.errorToString err
--
--                             Err _ ->
--                                 Err <| Http.BadBody <| "Failed to decode response"
--             )
--     , timeout = Nothing
--     }
--     |> Task.map (\v -> ( v, version ))


{-| Used directly by the core CORS modification to decide which Msg types need
the CORS flag set for subsequent Cmd's they'll initiate
-}
shouldProxy : Msg -> Bool
shouldProxy msg =
    case msg of
        BEMsg _ ->
            True

        FEtoBE _ ->
            True

        FEtoBEDelayed _ ->
            True

        ReceivedToBackend _ ->
            True

        _ ->
            False


showVersion ( major, minor, patch ) =
    [ major, minor, patch ] |> List.map String.fromInt |> String.join "."


justs =
    List.foldr
        (\v acc ->
            case v of
                Just el ->
                    el :: acc

                Nothing ->
                    acc
        )
        []



-- {-| Helper for handling a Bytes response
-- -}
-- handleBytesResponse : Http.Response Bytes -> Result Http.Error Bytes
-- handleBytesResponse response =
--     case response of
--         Http.BadUrl_ url ->
--             Err (Http.BadUrl url)
--         Http.Timeout_ ->
--             Err Http.Timeout
--         Http.BadStatus_ { statusCode } _ ->
--             Err (Http.BadStatus statusCode)
--         Http.NetworkError_ ->
--             Err Http.NetworkError
--         Http.GoodStatus_ metadata text ->
--             Ok text


renderTimeTravelUi : Model -> Html Msg
renderTimeTravelUi model =
    let
        indexedHistory =
            List.indexedMap (\i entry -> ( i, entry )) model.history

        kindColor kind =
            case kind of
                KindFrontend ->
                    green

                KindBackend ->
                    blue

                KindToFrontend ->
                    yellow

                KindToBackend ->
                    red

        frontendInfo entry =
            case ( entry.source, entry.target ) of
                ( Just source, Nothing ) ->
                    " (from " ++ source.clientId ++ ")"

                ( Nothing, Just target ) ->
                    " (to " ++ target.clientId ++ ")"

                ( Just source, Just target ) ->
                    " (from " ++ source.clientId ++ " to " ++ target.clientId ++ ")"

                ( Nothing, Nothing ) ->
                    ""

        historyEntry ( i, entry ) =
            div
                [ style "padding" "4px 8px"
                , style "margin" "2px 0"
                , style "cursor" "pointer"
                , style "background-color" (if i == model.currentIndex then "#444" else "#333")
                , style "border-left" ("3px solid " ++ kindColor entry.kind)
                , onClick (JumpTo i)
                ]
                [ div [ style "font-size" "12px" ]
                    [ text (String.fromInt i ++ ": " ++ entry.msg ++ frontendInfo entry) ]
                ]

        currentEntry =
            List.drop model.currentIndex model.history
                |> List.head
                |> Maybe.withDefault 
                    { kind = KindFrontend
                    , msg = "Initial State"
                    , fem = model.fem
                    , bem = Just model.bem
                    , source = Nothing
                    , target = Nothing
                    , timestamp = 0
                    }

        -- Function to pretty print JSON-like data
        prettyPrint str =
            str
                |> String.lines
                |> List.map String.trim
                |> String.join "\n"
                |> (\s -> 
                    if String.startsWith "{" s then
                        "{\n  " ++ (String.dropLeft 1 (String.dropRight 1 s) |> String.replace "," ",\n  ") ++ "\n}"
                    else
                        s
                   )

        -- Models panel on the right
        modelsPanel =
            let
                -- Get the models after the selected message
                currentModels =
                    if model.currentIndex == -1 then
                        -- Initial state
                        { kind = KindFrontend
                        , msg = "Initial State"
                        , fem = model.fem  -- Use the initial frontend model
                        , bem = Just model.bem
                        , source = Nothing
                        , target = Nothing
                        , timestamp = 0
                        }
                    else
                        -- Get the entry at current index to show post-message state
                        List.drop model.currentIndex model.history
                            |> List.head
                            |> Maybe.withDefault 
                                { kind = KindFrontend
                                , msg = "Initial State"
                                , fem = model.fem
                                , bem = Just model.bem
                                , source = Nothing
                                , target = Nothing
                                , timestamp = 0
                                }

                -- Collect all unique frontend models from history up to current index
                frontendModels =
                    -- Start with the current frontend model
                    [ ( model.clientId, currentModels.fem ) ]
                        -- Then add models from history
                        |> List.append
                            (List.take (model.currentIndex + 1) model.history
                                |> List.filterMap (\entry -> 
                                    case entry.source of
                                        Just source -> Just ( source.clientId, entry.fem )
                                        Nothing -> Nothing
                                )
                                |> List.reverse
                            )
                        -- Keep only the latest model for each clientId
                        |> List.foldl 
                            (\(clientId, fem) acc ->
                                if List.any (\(existingId, _) -> existingId == clientId) acc then
                                    acc
                                else
                                    (clientId, fem) :: acc
                            )
                            []
            in
            div [ style "width" "50%", style "padding-left" "1rem" ]
                [ h4 [ style "margin" "0 0 0.5rem 0" ] [ text "Backend Model" ]
                , pre 
                    [ style "margin" "0 0 1rem 0"
                    , style "background-color" "#333"
                    , style "padding" "0.5rem"
                    , style "border-radius" "4px"
                    ] 
                    [ text (prettyPrint (Debug.toString (Maybe.withDefault model.bem currentModels.bem))) ]
                , h4 [ style "margin" "0 0 0.5rem 0" ] [ text "Frontend Models" ]
                , div []
                    (List.map
                        (\(clientId, fem) ->
                            div [ style "margin-bottom" "1rem" ]
                                [ div [ style "font-size" "0.8em", style "color" "#999", style "margin-bottom" "0.25rem" ]
                                    [ text ("Client: " ++ clientId ++ if clientId == model.clientId then " (current)" else "") ]
                                , pre 
                                    [ style "margin" "0"
                                    , style "background-color" "#333"
                                    , style "padding" "0.5rem"
                                    , style "border-radius" "4px"
                                    ] 
                                    [ text (prettyPrint (Debug.toString fem)) ]
                                ]
                        )
                        frontendModels
                    )
                ]

        -- Add initial state entry at the top of history
        historyWithInitial =
            ( -1
            , { kind = KindFrontend
              , msg = "Initial State"
              , fem = model.fem
              , bem = Just model.bem
              , source = Nothing
              , target = Nothing
              , timestamp = 0
              }
            ) :: indexedHistory
    in
    div
        [ style "position" "absolute"  -- Changed from fixed
        , style "top" "0"
        , style "left" "0"
        , style "right" "0"
        , style "bottom" "0"
        , style "background-color" charcoal
        , style "color" white
        , style "padding" "1rem"
        , style "overflow" "hidden"
        ]
        [ div 
            [ style "display" "flex"
            , style "justify-content" "space-between"
            , style "align-items" "center"
            , style "padding-bottom" "0.5rem"
            , style "margin-bottom" "0.5rem"
            , style "border-bottom" "1px solid #444"
            ]
            [ h3 [ style "margin" "0" ] [ text "Time Travel Debugger" ]
            , button
                [ onClick ToggleTimeTravel
                , style "background" "none"
                , style "border" "none"
                , style "color" white
                , style "cursor" "pointer"
                , style "font-size" "20px"
                , style "padding" "4px 8px"
                , style "border-radius" "4px"
                ]
                [ text "×" ]
            ]
        , div 
            [ style "display" "flex"
            , style "overflow" "hidden"
            , style "height" "calc(100% - 3rem)"
            ] 
            [ div 
                [ style "width" "50%"
                , style "overflow" "hidden"
                , style "display" "flex"
                , style "flex-direction" "column"
                ] 
                [ div 
                    [ style "margin-bottom" "8px"
                    , style "padding" "4px 8px"
                    , style "background-color" "#333"
                    , style "border-radius" "4px"
                    ]
                    [ text ("Current index: " ++ String.fromInt model.currentIndex)
                    , text (" / " ++ String.fromInt (List.length model.history - 1))
                    ]
                , div 
                    [ style "flex" "1"
                    , style "overflow-y" "auto"
                    , style "overflow-x" "hidden"
                    , style "min-height" "0"
                    ] 
                    (List.map historyEntry historyWithInitial)
                ]
            , div 
                [ style "width" "50%"
                , style "padding-left" "1rem"
                , style "overflow" "hidden"
                , style "display" "flex"
                , style "flex-direction" "column"
                ] 
                [ h4 [ style "margin" "0 0 0.5rem 0" ] [ text "Backend Model" ]
                , pre 
                    [ style "margin" "0 0 1rem 0"
                    , style "background-color" "#333"
                    , style "padding" "0.5rem"
                    , style "border-radius" "4px"
                    , style "overflow-x" "auto"
                    ] 
                    [ text (prettyPrint (Debug.toString (Maybe.withDefault model.bem currentEntry.bem))) ]
                , h4 [ style "margin" "0 0 0.5rem 0" ] [ text "Frontend Models" ]
                , div 
                    [ style "flex" "1"
                    , style "overflow-y" "auto"
                    , style "overflow-x" "hidden"
                    , style "min-height" "0"
                    ]
                    (List.map
                        (\(clientId, fem) ->
                            div [ style "margin-bottom" "1rem" ]
                                [ div [ style "font-size" "0.8em", style "color" "#999", style "margin-bottom" "0.25rem" ]
                                    [ text ("Client: " ++ clientId ++ if clientId == model.clientId then " (current)" else "") ]
                                , pre 
                                    [ style "margin" "0"
                                    , style "background-color" "#333"
                                    , style "padding" "0.5rem"
                                    , style "border-radius" "4px"
                                    , style "overflow-x" "auto"
                                    ] 
                                    [ text (prettyPrint (Debug.toString fem)) ]
                                ]
                        )
                        ([ ( model.clientId, currentEntry.fem ) ]
                            |> List.append
                                (List.take (model.currentIndex + 1) model.history
                                    |> List.filterMap (\entry -> 
                                        case entry.source of
                                            Just source -> Just ( source.clientId, entry.fem )
                                            Nothing -> Nothing
                                    )
                                    |> List.reverse
                                )
                            |> List.foldl 
                                (\(clientId, fem) acc ->
                                    if List.any (\(existingId, _) -> existingId == clientId) acc then
                                        acc
                                    else
                                        (clientId, fem) :: acc
                                )
                                []
                        )
                    )
                ]
            ]
        ]

-- Add near the other ports
port openDebuggerWindow : { width : Int, height : Int } -> Cmd msg
port closeDebuggerWindow : () -> Cmd msg
port debuggerWindowClosed : (() -> msg) -> Sub msg
port updateDebugger : { history : List { kind : String, msg : String, source : Maybe { sessionId : String, clientId : String }, target : Maybe { sessionId : String, clientId : String }, timestamp : Int }, currentIndex : Int } -> Cmd msg

-- Helper function to convert HistoryEntry to JSON-compatible format
historyToJson : HistoryEntry -> { kind : String, msg : String, source : Maybe { sessionId : String, clientId : String }, target : Maybe { sessionId : String, clientId : String }, timestamp : Int }
historyToJson entry =
    { kind = case entry.kind of
        KindFrontend -> "frontend"
        KindBackend -> "backend"
        KindToFrontend -> "toFrontend"
        KindToBackend -> "toBackend"
    , msg = entry.msg
    , source = entry.source
    , target = entry.target
    , timestamp = entry.timestamp
    }

-- Helper function to send updates to debugger window
sendToDebugger : Model -> Cmd msg
sendToDebugger model =
    if model.debuggerWindowOpen then
        updateDebugger 
            { history = List.map historyToJson model.history
            , currentIndex = model.currentIndex
            }
    else
        Cmd.none

-- Update the view function to send content to debugger window
view : Model -> Browser.Document Msg
view model =
    let
        { title, body } =
            userFrontendApp.view model.fem
    in
    { title = title
    , body =
        List.map (Html.map FEMsg) body
        ++ [ devBar model ]
    }

devBar : Model -> Html Msg
devBar model =
    div
        [ style "position" "fixed"
        , style "bottom" "0"
        , style "right" "0"
        , style "z-index" "1000"
        ]
        [ div
            [ style "background-color" "#1e1e1e"
            , style "color" "white"
            , style "padding" "8px"
            , style "border-radius" "4px"
            , style "margin" "8px"
            ]
            [ text (nodeTypeToString model.nodeType)
            , text " | "
            , button
                [ onClick ToggleTimeTravel
                , style "background" "none"
                , style "border" "none"
                , style "color" "white"
                , style "cursor" "pointer"
                , style "padding" "4px 8px"
                ]
                [ text "Time Travel" ]
            ]
        ]
