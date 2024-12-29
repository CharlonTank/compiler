module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , FrontendModel
    , FrontendMsg(..)
    , ToBackend(..)
    , ToFrontend(..)
    )

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    }


type alias BackendModel =
    { message : String
    , blob : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend
    | NotifyLeaderFrontendMsg FrontendMsg  -- Used by Followers to notify Leader about their frontend messages


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
