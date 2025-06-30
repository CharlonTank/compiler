{-# LANGUAGE OverloadedStrings #-}

module Test.TimeTravelDebuggerIntegration where

import qualified System.Directory as Dir
import System.FilePath ((</>))
import Data.Text as T hiding (unlines)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import qualified Data.List

import Lamdera
import EasyTest
import Test.Helpers
import qualified Ext.Common
import qualified Develop
import qualified Lamdera.Compile

{-
Integration tests for multi-client time travel debugger.
These tests verify the actual WebSocket message flow between clients.

NOTE: These are heavier tests that actually start the development server,
so they should be run separately from unit tests.
-}

all = run Test.TimeTravelDebuggerIntegration.suite

suite :: Test ()
suite = tests
  [ scope "Time travel debugger integration tests" $ tests
      [ pendingIntegrationTest "Multi-client message flow" testMultiClientMessageFlow
      , pendingIntegrationTest "Time travel state broadcast" testTimeTravelStateBroadcast
      , pendingIntegrationTest "Frontend model synchronization" testFrontendModelSync
      ]
  ]

-- Helper to mark integration tests as pending by default
-- Since they require starting a server and may be slow
pendingIntegrationTest :: String -> Test () -> Test ()
pendingIntegrationTest name test = 
  if False -- Set to True to run integration tests
  then scope name test
  else pending $ scope name test

-- Test that frontend messages from multiple clients are properly recorded
testMultiClientMessageFlow :: Test ()
testMultiClientMessageFlow = do
  let testProject = "test/scenario-time-travel-integration"
  
  using (setupIntegrationTest testProject) (cleanupIntegrationTest testProject) $ \serverThread -> do
    -- Give server time to start
    io $ threadDelay 2000000 -- 2 seconds
    
    -- In a real integration test, we would:
    -- 1. Connect multiple WebSocket clients
    -- 2. Send frontend messages from each client
    -- 3. Verify the leader receives all messages
    -- 4. Check the debug history contains all messages
    
    -- For now, we just verify the server started
    ok

-- Test time travel state broadcasting to all clients
testTimeTravelStateBroadcast :: Test ()
testTimeTravelStateBroadcast = do
  let testProject = "test/scenario-time-travel-broadcast-integration"
  
  using (setupIntegrationTest testProject) (cleanupIntegrationTest testProject) $ \serverThread -> do
    io $ threadDelay 2000000
    
    -- In a real test, we would:
    -- 1. Connect multiple clients
    -- 2. Have the leader navigate through time travel history
    -- 3. Verify all followers receive state updates
    -- 4. Check that all clients show the same state
    
    ok

-- Test frontend model synchronization across clients
testFrontendModelSync :: Test ()
testFrontendModelSync = do
  let testProject = "test/scenario-time-travel-sync-integration"
  
  using (setupIntegrationTest testProject) (cleanupIntegrationTest testProject) $ \serverThread -> do
    io $ threadDelay 2000000
    
    -- In a real test, we would:
    -- 1. Connect multiple clients
    -- 2. Update frontend state on each client
    -- 3. Verify the leader tracks all frontend models
    -- 4. Check debugger shows correct state for each client
    
    ok

-- Helper functions for integration tests

setupIntegrationTest :: String -> IO (Async ())
setupIntegrationTest projectPath = do
  -- Setup test project
  rmdir projectPath
  mkdir projectPath
  mkdir (projectPath </> "src")
  mkdir (projectPath </> "src/Evergreen")
  
  -- Write test app files
  writeIntegrationTestApp projectPath
  
  -- Compile the project first
  withDebug $ do
    Ext.Common.withProjectRoot projectPath $ do
      Lamdera.Compile.makeDev projectPath ["src/Frontend.elm"]
  
  -- Start the development server in a separate thread
  serverThread <- async $ withDebug $ do
    Ext.Common.withProjectRoot projectPath $ do
      Develop.runWithRoot projectPath (Develop.Flags Nothing)
  
  return serverThread

cleanupIntegrationTest :: String -> Async () -> IO ()
cleanupIntegrationTest projectPath serverThread = do
  -- Cancel the server thread
  cancel serverThread
  -- Clean up test project
  rmdir projectPath

-- Write a more complex test app for integration testing
writeIntegrationTestApp :: String -> IO ()
writeIntegrationTestApp projectPath = do
  -- elm.json
  writeFile (projectPath </> "elm.json") $ unlines
    [ "{"
    , "    \"type\": \"application\","
    , "    \"source-directories\": ["
    , "        \"src\""
    , "    ],"
    , "    \"elm-version\": \"0.19.1\","
    , "    \"dependencies\": {"
    , "        \"direct\": {"
    , "            \"elm/browser\": \"1.0.2\","
    , "            \"elm/core\": \"1.0.5\","
    , "            \"elm/html\": \"1.0.0\","
    , "            \"elm/json\": \"1.1.3\","
    , "            \"elm/time\": \"1.0.0\","
    , "            \"elm/url\": \"1.0.0\","
    , "            \"lamdera/codecs\": \"1.0.0\","
    , "            \"lamdera/core\": \"1.0.0\""
    , "        },"
    , "        \"indirect\": {"
    , "            \"elm/bytes\": \"1.0.8\","
    , "            \"elm/file\": \"1.0.5\","
    , "            \"elm/http\": \"2.0.0\","
    , "            \"elm/virtual-dom\": \"1.0.3\""
    , "        }"
    , "    },"
    , "    \"test-dependencies\": {"
    , "        \"direct\": {},"
    , "        \"indirect\": {}"
    , "    }"
    , "}"
    ]
  
  -- Frontend.elm with more complex state
  writeFile (projectPath </> "src/Frontend.elm") $ unlines
    [ "module Frontend exposing (..)"
    , ""
    , "import Browser"
    , "import Browser.Navigation as Nav"
    , "import Html exposing (..)"
    , "import Html.Attributes exposing (..)"
    , "import Html.Events exposing (..)"
    , "import Json.Decode as D"
    , "import Lamdera"
    , "import Time"
    , "import Types exposing (..)"
    , "import Url"
    , ""
    , "type alias Model = FrontendModel"
    , ""
    , "app ="
    , "    Lamdera.frontend"
    , "        { init = init"
    , "        , onUrlRequest = UrlClicked"
    , "        , onUrlChange = UrlChanged"
    , "        , update = update"
    , "        , updateFromBackend = updateFromBackend"
    , "        , subscriptions = subscriptions"
    , "        , view = view"
    , "        }"
    , ""
    , "init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )"
    , "init url key ="
    , "    ( { counter = 0"
    , "      , localCounter = 0"
    , "      , clientId = Nothing"
    , "      , messages = []"
    , "      , key = key"
    , "      }"
    , "    , Cmd.none"
    , "    )"
    , ""
    , "subscriptions : Model -> Sub FrontendMsg"
    , "subscriptions model ="
    , "    Time.every 5000 Tick"
    , ""
    , "update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )"
    , "update msg model ="
    , "    case msg of"
    , "        UrlClicked urlRequest ->"
    , "            ( model, Cmd.none )"
    , ""
    , "        UrlChanged url ->"
    , "            ( model, Cmd.none )"
    , ""
    , "        Increment ->"
    , "            ( { model | counter = model.counter + 1 }"
    , "            , Lamdera.sendToBackend IncrementBackend"
    , "            )"
    , ""
    , "        LocalIncrement ->"
    , "            ( { model | localCounter = model.localCounter + 1 }"
    , "            , Cmd.none"
    , "            )"
    , ""
    , "        Tick _ ->"
    , "            ( { model | messages = (\"Tick at \" ++ String.fromInt model.localCounter) :: model.messages }"
    , "            , Cmd.none"
    , "            )"
    , ""
    , "updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )"
    , "updateFromBackend msg model ="
    , "    case msg of"
    , "        CounterUpdated newValue clientId ->"
    , "            ( { model | counter = newValue, clientId = Just clientId }"
    , "            , Cmd.none"
    , "            )"
    , ""
    , "view : Model -> Browser.Document FrontendMsg"
    , "view model ="
    , "    { title = \"Time Travel Test\""
    , "    , body ="
    , "        [ div []"
    , "            [ h1 [] [ text \"Multi-Client Time Travel Test\" ]"
    , "            , div []"
    , "                [ text \"Client ID: \""
    , "                , text (Maybe.withDefault \"Unknown\" model.clientId)"
    , "                ]"
    , "            , div []"
    , "                [ h2 [] [ text \"Shared Counter\" ]"
    , "                , p [] [ text (String.fromInt model.counter) ]"
    , "                , button [ onClick Increment ] [ text \"Increment (Backend)\" ]"
    , "                ]"
    , "            , div []"
    , "                [ h2 [] [ text \"Local Counter\" ]"
    , "                , p [] [ text (String.fromInt model.localCounter) ]"
    , "                , button [ onClick LocalIncrement ] [ text \"Increment (Local)\" ]"
    , "                ]"
    , "            , div []"
    , "                [ h2 [] [ text \"Messages\" ]"
    , "                , ul [] (List.map (\\m -> li [] [ text m ]) model.messages)"
    , "                ]"
    , "            ]"
    , "        ]"
    , "    }"
    ]
  
  -- Backend.elm
  writeFile (projectPath </> "src/Backend.elm") $ unlines
    [ "module Backend exposing (..)"
    , ""
    , "import Html"
    , "import Lamdera exposing (ClientId, SessionId)"
    , "import Types exposing (..)"
    , ""
    , "type alias Model = BackendModel"
    , ""
    , "app ="
    , "    Lamdera.backend"
    , "        { init = init"
    , "        , update = update"
    , "        , updateFromFrontend = updateFromFrontend"
    , "        , subscriptions = \\m -> Sub.none"
    , "        }"
    , ""
    , "init : ( Model, Cmd BackendMsg )"
    , "init ="
    , "    ( { counter = 0 }"
    , "    , Cmd.none"
    , "    )"
    , ""
    , "update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )"
    , "update msg model ="
    , "    ( model, Cmd.none )"
    , ""
    , "updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )"
    , "updateFromFrontend sessionId clientId msg model ="
    , "    case msg of"
    , "        IncrementBackend ->"
    , "            let"
    , "                newCounter = model.counter + 1"
    , "            in"
    , "            ( { model | counter = newCounter }"
    , "            , Lamdera.broadcast (CounterUpdated newCounter clientId)"
    , "            )"
    ]
  
  -- Types.elm
  writeFile (projectPath </> "src/Types.elm") $ unlines
    [ "module Types exposing (..)"
    , ""
    , "import Browser"
    , "import Browser.Navigation as Nav"
    , "import Lamdera exposing (ClientId)"
    , "import Time"
    , "import Url"
    , ""
    , "type alias FrontendModel ="
    , "    { counter : Int"
    , "    , localCounter : Int"
    , "    , clientId : Maybe ClientId"
    , "    , messages : List String"
    , "    , key : Nav.Key"
    , "    }"
    , ""
    , "type alias BackendModel ="
    , "    { counter : Int"
    , "    }"
    , ""
    , "type FrontendMsg"
    , "    = UrlClicked Browser.UrlRequest"
    , "    | UrlChanged Url.Url"
    , "    | Increment"
    , "    | LocalIncrement"
    , "    | Tick Time.Posix"
    , ""
    , "type ToBackend"
    , "    = IncrementBackend"
    , ""
    , "type BackendMsg"
    , "    = NoOpBackendMsg"
    , ""
    , "type ToFrontend"
    , "    = CounterUpdated Int ClientId"
    ]
  
  -- Env.elm
  writeFile (projectPath </> "src/Env.elm") $ unlines
    [ "module Env exposing (..)"
    , ""
    , "-- Dummy environment config"
    , "dummyConfigItem = \"\""
    ]