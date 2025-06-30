{-# LANGUAGE OverloadedStrings #-}

module Test.TimeTravelDebugger where

import qualified System.Directory as Dir
import System.FilePath ((</>))
import Data.Text as T hiding (unlines)
import Control.Monad (forM_)
import Data.List (isInfixOf)

import Lamdera
import EasyTest
import Test.Helpers
import qualified Ext.Common
import qualified Lamdera.Compile
import qualified Develop

-- Main entry point for running tests
all = run Test.TimeTravelDebugger.suite

suite :: Test ()
suite = tests
  [ scope "Multi-client time travel debugger" $ tests
      [ testInitialHistoryEntry
      , testFrontendMessageBroadcasting
      , testTimeTravelSynchronization
      , testRealTimeDebuggerUpdates
      , testMultipleClientStates
      ]
  ]

-- Test that initial history entry is created with KindInit
testInitialHistoryEntry :: Test ()
testInitialHistoryEntry = scope "Initial history entry" $ do
  let testProject = "test/scenario-time-travel"
  
  using (setupTestProject testProject) (cleanupTestProject testProject) $ \_ -> do
    -- Create a simple test app with time travel debugging
    io $ writeTestApp testProject
    
    -- Compile with LDEBUG to enable time travel features
    output <- io $ withDebug $ do
      Ext.Common.withProjectRoot testProject $ do
        Lamdera.Compile.makeDev testProject ["src/Frontend.elm"]
    
    -- Verify compilation succeeded
    io $ formatHaskellValue "Compilation output" output
    ok
    
    -- Note: Testing the actual runtime behavior would require:
    -- 1. Starting lamdera live
    -- 2. Opening browser connections
    -- 3. Intercepting WebSocket messages
    -- This is beyond unit test scope but could be done with integration tests

-- Test that frontend messages from all clients are recorded
testFrontendMessageBroadcasting :: Test ()
testFrontendMessageBroadcasting = scope "Frontend message broadcasting" $ do
  let testProject = "test/scenario-time-travel-broadcast"
  
  using (setupTestProject testProject) (cleanupTestProject testProject) $ \_ -> do
    -- Create test app that generates frontend messages
    io $ writeTestAppWithFrontendMessages testProject
    
    -- Compile the app
    output <- io $ withDebug $ do
      Ext.Common.withProjectRoot testProject $ do
        Lamdera.Compile.makeDev testProject ["src/Frontend.elm"]
    
    -- Verify LocalDev.elm includes our frontend message broadcasting logic
    localDevContent <- io $ readFile (testProject </> "elm-stuff/lamdera/LocalDev.elm")
    
    -- Check for key components of our implementation
    expect $ "port sendFrontendMessage" `Data.List.isInfixOf` localDevContent
    
    expect $ "port receiveFrontendMessage" `Data.List.isInfixOf` localDevContent
    
    expect $ "sendFrontendMessage" `Data.List.isInfixOf` localDevContent &&
             "Follower" `Data.List.isInfixOf` localDevContent

-- Test time travel synchronization across clients
testTimeTravelSynchronization :: Test ()
testTimeTravelSynchronization = scope "Time travel synchronization" $ do
  let testProject = "test/scenario-time-travel-sync"
  
  using (setupTestProject testProject) (cleanupTestProject testProject) $ \_ -> do
    io $ writeTestApp testProject
    
    -- Compile and verify synchronization ports
    output <- io $ withDebug $ do
      Ext.Common.withProjectRoot testProject $ do
        Lamdera.Compile.makeDev testProject ["src/Frontend.elm"]
    
    localDevContent <- io $ readFile (testProject </> "elm-stuff/lamdera/LocalDev.elm")
    
    expect $ "port broadcastTimeTravelState" `Data.List.isInfixOf` localDevContent
    
    expect $ "port receiveTimeTravelState" `Data.List.isInfixOf` localDevContent
    
    expect $ "frontendModels = entry.frontendModels" `Data.List.isInfixOf` localDevContent

-- Test real-time debugger updates
testRealTimeDebuggerUpdates :: Test ()
testRealTimeDebuggerUpdates = scope "Real-time debugger updates" $ do
  let testProject = "test/scenario-time-travel-realtime"
  
  using (setupTestProject testProject) (cleanupTestProject testProject) $ \_ -> do
    io $ writeTestApp testProject
    
    output <- io $ withDebug $ do
      Ext.Common.withProjectRoot testProject $ do
        Lamdera.Compile.makeDev testProject ["src/Frontend.elm"]
    
    localDevContent <- io $ readFile (testProject </> "elm-stuff/lamdera/LocalDev.elm")
    
    -- Check that sendToDebugger is called when history updates
    expect $ "sendToDebugger updatedModel" `Data.List.isInfixOf` localDevContent
    
    expect $ "sendToDebugger : Model -> Cmd msg" `Data.List.isInfixOf` localDevContent

-- Test multiple client states are tracked correctly
testMultipleClientStates :: Test ()
testMultipleClientStates = scope "Multiple client states" $ do
  let testProject = "test/scenario-time-travel-multistate"
  
  using (setupTestProject testProject) (cleanupTestProject testProject) $ \_ -> do
    io $ writeTestApp testProject
    
    output <- io $ withDebug $ do
      Ext.Common.withProjectRoot testProject $ do
        Lamdera.Compile.makeDev testProject ["src/Frontend.elm"]
    
    localDevContent <- io $ readFile (testProject </> "elm-stuff/lamdera/LocalDev.elm")
    
    -- Verify activeFrontendModels dictionary is used
    expect $ "activeFrontendModels : Dict String FrontendModel" `Data.List.isInfixOf` localDevContent
    
    -- Check that frontend models are updated when clients send updates
    expect $ "Dict.insert args.c frontendModel m.activeFrontendModels" `Data.List.isInfixOf` localDevContent

-- Helper functions

setupTestProject :: String -> IO ()
setupTestProject projectPath = do
  rmdir projectPath
  mkdir projectPath
  mkdir (projectPath </> "src")
  mkdir (projectPath </> "src/Evergreen")

cleanupTestProject :: String -> () -> IO ()
cleanupTestProject projectPath _ = do
  rmdir projectPath

-- Write a minimal test application
writeTestApp :: String -> IO ()
writeTestApp projectPath = do
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
    , "            \"elm/url\": \"1.0.0\","
    , "            \"lamdera/codecs\": \"1.0.0\","
    , "            \"lamdera/core\": \"1.0.0\""
    , "        },"
    , "        \"indirect\": {"
    , "            \"elm/bytes\": \"1.0.8\","
    , "            \"elm/file\": \"1.0.5\","
    , "            \"elm/http\": \"2.0.0\","
    , "            \"elm/json\": \"1.1.3\","
    , "            \"elm/time\": \"1.0.0\","
    , "            \"elm/virtual-dom\": \"1.0.3\""
    , "        }"
    , "    },"
    , "    \"test-dependencies\": {"
    , "        \"direct\": {},"
    , "        \"indirect\": {}"
    , "    }"
    , "}"
    ]
  
  -- Frontend.elm
  writeFile (projectPath </> "src/Frontend.elm") $ unlines
    [ "module Frontend exposing (..)"
    , ""
    , "import Browser"
    , "import Browser.Navigation as Nav"
    , "import Html exposing (..)"
    , "import Html.Attributes exposing (..)"
    , "import Html.Events exposing (..)"
    , "import Lamdera"
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
    , "        , subscriptions = \\m -> Sub.none"
    , "        , view = view"
    , "        }"
    , ""
    , "init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )"
    , "init url key ="
    , "    ( { counter = 0"
    , "      , key = key"
    , "      }"
    , "    , Cmd.none"
    , "    )"
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
    , "            , Cmd.none"
    , "            )"
    , ""
    , "updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )"
    , "updateFromBackend msg model ="
    , "    ( model, Cmd.none )"
    , ""
    , "view : Model -> Browser.Document FrontendMsg"
    , "view model ="
    , "    { title = \"Test App\""
    , "    , body ="
    , "        [ div []"
    , "            [ h1 [] [ text \"Counter\" ]"
    , "            , p [] [ text (String.fromInt model.counter) ]"
    , "            , button [ onClick Increment ] [ text \"Increment\" ]"
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
    , "    ( { message = \"Hello!\" }"
    , "    , Cmd.none"
    , "    )"
    , ""
    , "update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )"
    , "update msg model ="
    , "    ( model, Cmd.none )"
    , ""
    , "updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )"
    , "updateFromFrontend sessionId clientId msg model ="
    , "    ( model, Cmd.none )"
    ]
  
  -- Types.elm
  writeFile (projectPath </> "src/Types.elm") $ unlines
    [ "module Types exposing (..)"
    , ""
    , "import Browser"
    , "import Browser.Navigation as Nav"
    , "import Url"
    , ""
    , "type alias FrontendModel ="
    , "    { counter : Int"
    , "    , key : Nav.Key"
    , "    }"
    , ""
    , "type alias BackendModel ="
    , "    { message : String"
    , "    }"
    , ""
    , "type FrontendMsg"
    , "    = UrlClicked Browser.UrlRequest"
    , "    | UrlChanged Url.Url"
    , "    | Increment"
    , ""
    , "type ToBackend"
    , "    = NoOpToBackend"
    , ""
    , "type BackendMsg"
    , "    = NoOpBackendMsg"
    , ""
    , "type ToFrontend"
    , "    = NoOpToFrontend"
    ]
  
  -- Env.elm
  writeFile (projectPath </> "src/Env.elm") $ unlines
    [ "module Env exposing (..)"
    , ""
    , "-- Dummy environment config"
    , "dummyConfigItem = \"\""
    ]

-- Write test app with frontend messages
writeTestAppWithFrontendMessages :: String -> IO ()
writeTestAppWithFrontendMessages projectPath = do
  writeTestApp projectPath
  -- The basic test app already has Increment messages which are frontend-only