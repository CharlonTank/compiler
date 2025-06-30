module LocalDev.Timeline exposing (view)

{-| Timeline visualization for the multi-client time travel debugger.
Inspired by lamdera-program-test's visualization.
-}

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as D
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Svg.Events as SvgEvents
import Time


type alias Config msg =
    { onSelectMessage : Int -> msg
    , selectedIndex : Int
    , width : Float
    , height : Float
    }


type alias HistoryEntry =
    { kind : MsgKind
    , msg : String
    , frontendModels : List ( String, String ) -- (clientId, model)
    , bem : Maybe String
    , source : Maybe { sessionId : String, clientId : String }
    , target : Maybe { sessionId : String, clientId : String }
    , timestamp : Int
    }


type MsgKind
    = KindInit
    | KindFrontend
    | KindBackend
    | KindToFrontend
    | KindToBackend


type alias ClientLane =
    { clientId : String
    , yPosition : Float
    , color : String
    }


view : Config msg -> List HistoryEntry -> Html msg
view config history =
    let
        -- Constants
        laneHeight = 60
        timelineStartX = 100
        timelineWidth = config.width - 150
        messageSpacing = timelineWidth / max 1 (toFloat (List.length history))
        
        -- Extract unique client IDs
        clientIds =
            history
                |> List.concatMap extractClientIds
                |> List.sort
                |> unique
                
        -- Create lanes for clients + backend
        lanes =
            createLanes clientIds laneHeight
            
        -- Position messages on timeline
        positionedMessages =
            List.indexedMap (positionMessage messageSpacing timelineStartX lanes) history
    in
    Html.div
        [ Attr.style "width" (String.fromFloat config.width ++ "px")
        , Attr.style "height" (String.fromFloat config.height ++ "px")
        , Attr.style "background-color" "#1e1e1e"
        , Attr.style "color" "#ffffff"
        , Attr.style "font-family" "monospace"
        , Attr.style "overflow" "hidden"
        , Attr.style "position" "relative"
        ]
        [ Svg.svg
            [ SvgAttr.width (String.fromFloat config.width)
            , SvgAttr.height (String.fromFloat config.height)
            , SvgAttr.viewBox ("0 0 " ++ String.fromFloat config.width ++ " " ++ String.fromFloat config.height)
            ]
            (List.concat
                [ drawLanes lanes config.width
                , drawMessages positionedMessages config.selectedIndex config.onSelectMessage
                , drawConnections positionedMessages
                ]
            )
        ]


extractClientIds : HistoryEntry -> List String
extractClientIds entry =
    let
        sourceIds =
            case entry.source of
                Just source ->
                    [ source.clientId ]
                Nothing ->
                    []
                    
        targetIds =
            case entry.target of
                Just target ->
                    [ target.clientId ]
                Nothing ->
                    []
                    
        frontendModelIds =
            List.map Tuple.first entry.frontendModels
    in
    sourceIds ++ targetIds ++ frontendModelIds


createLanes : List String -> Float -> List ClientLane
createLanes clientIds laneHeight =
    let
        backendLane =
            { clientId = "backend"
            , yPosition = 50
            , color = "#4a4a4a"
            }
            
        clientLanes =
            List.indexedMap
                (\index clientId ->
                    { clientId = clientId
                    , yPosition = 150 + (toFloat index * laneHeight)
                    , color = laneColor index
                    }
                )
                clientIds
    in
    backendLane :: clientLanes


laneColor : Int -> String
laneColor index =
    case index of
        0 -> "#3498db"  -- Blue
        1 -> "#2ecc71"  -- Green
        2 -> "#f39c12"  -- Orange
        3 -> "#e74c3c"  -- Red
        _ -> "#95a5a6"  -- Gray


type alias PositionedMessage =
    { entry : HistoryEntry
    , index : Int
    , x : Float
    , y : Float
    , lane : ClientLane
    }


positionMessage : Float -> Float -> List ClientLane -> Int -> HistoryEntry -> PositionedMessage
positionMessage spacing startX lanes index entry =
    let
        x = startX + (toFloat index * spacing)
        
        lane =
            case entry.kind of
                KindBackend ->
                    findLane "backend" lanes
                    
                KindFrontend ->
                    case entry.source of
                        Just source ->
                            findLane source.clientId lanes
                        Nothing ->
                            findLane "backend" lanes
                            
                KindToFrontend ->
                    findLane "backend" lanes
                    
                KindToBackend ->
                    case entry.source of
                        Just source ->
                            findLane source.clientId lanes
                        Nothing ->
                            findLane "backend" lanes
                            
                KindInit ->
                    findLane "backend" lanes
    in
    { entry = entry
    , index = index
    , x = x
    , y = lane.yPosition
    , lane = lane
    }


findLane : String -> List ClientLane -> ClientLane
findLane clientId lanes =
    lanes
        |> List.filter (\lane -> lane.clientId == clientId)
        |> List.head
        |> Maybe.withDefault { clientId = "backend", yPosition = 50, color = "#4a4a4a" }


drawLanes : List ClientLane -> Float -> List (Svg msg)
drawLanes lanes width =
    List.map (drawLane width) lanes


drawLane : Float -> ClientLane -> Svg msg
drawLane width lane =
    Svg.g []
        [ -- Lane line
          Svg.line
            [ SvgAttr.x1 "0"
            , SvgAttr.y1 (String.fromFloat lane.yPosition)
            , SvgAttr.x2 (String.fromFloat width)
            , SvgAttr.y2 (String.fromFloat lane.yPosition)
            , SvgAttr.stroke lane.color
            , SvgAttr.strokeWidth "2"
            , SvgAttr.opacity "0.3"
            ]
            []
        , -- Lane label
          Svg.text_
            [ SvgAttr.x "10"
            , SvgAttr.y (String.fromFloat (lane.yPosition - 5))
            , SvgAttr.fill lane.color
            , SvgAttr.fontSize "12"
            , SvgAttr.fontFamily "monospace"
            ]
            [ Svg.text (laneLabel lane.clientId) ]
        ]


laneLabel : String -> String
laneLabel clientId =
    case clientId of
        "backend" -> "Backend"
        _ -> "Client " ++ String.left 8 clientId


drawMessages : List PositionedMessage -> Int -> (Int -> msg) -> List (Svg msg)
drawMessages messages selectedIndex onSelect =
    List.map (drawMessage selectedIndex onSelect) messages


drawMessage : Int -> (Int -> msg) -> PositionedMessage -> Svg msg
drawMessage selectedIndex onSelect positioned =
    let
        isSelected = positioned.index == selectedIndex
        radius = if isSelected then "8" else "6"
        fillColor = msgKindColor positioned.entry.kind
        strokeColor = if isSelected then "#ffffff" else fillColor
        strokeWidth = if isSelected then "3" else "1"
    in
    Svg.g []
        [ -- Message circle
          Svg.circle
            [ SvgAttr.cx (String.fromFloat positioned.x)
            , SvgAttr.cy (String.fromFloat positioned.y)
            , SvgAttr.r radius
            , SvgAttr.fill fillColor
            , SvgAttr.stroke strokeColor
            , SvgAttr.strokeWidth strokeWidth
            , SvgAttr.cursor "pointer"
            , SvgEvents.onClick (onSelect positioned.index)
            ]
            []
        , -- Message label (if selected)
          if isSelected then
            Svg.text_
                [ SvgAttr.x (String.fromFloat positioned.x)
                , SvgAttr.y (String.fromFloat (positioned.y - 15))
                , SvgAttr.fill "#ffffff"
                , SvgAttr.fontSize "10"
                , SvgAttr.textAnchor "middle"
                , SvgAttr.fontFamily "monospace"
                ]
                [ Svg.text (msgKindLabel positioned.entry.kind) ]
          else
            Svg.g [] []
        ]


msgKindColor : MsgKind -> String
msgKindColor kind =
    case kind of
        KindInit -> "#9966cc"      -- Purple
        KindFrontend -> "#85bc7a"   -- Green
        KindBackend -> "#4196ad"    -- Blue
        KindToFrontend -> "#ffcb64" -- Yellow
        KindToBackend -> "#e06c75"  -- Red


msgKindLabel : MsgKind -> String
msgKindLabel kind =
    case kind of
        KindInit -> "Init"
        KindFrontend -> "Frontend"
        KindBackend -> "Backend"
        KindToFrontend -> "ToFrontend"
        KindToBackend -> "ToBackend"


drawConnections : List PositionedMessage -> List (Svg msg)
drawConnections messages =
    messages
        |> List.filterMap (drawConnection messages)


drawConnection : List PositionedMessage -> PositionedMessage -> Maybe (Svg msg)
drawConnection allMessages message =
    case message.entry.kind of
        KindToFrontend ->
            case message.entry.target of
                Just target ->
                    let
                        targetLane = findLane target.clientId (List.map .lane allMessages)
                    in
                    Just (drawArrow message.x message.y message.x targetLane.yPosition "#ffcb64")
                Nothing ->
                    Nothing
                    
        KindToBackend ->
            let
                backendLane = findLane "backend" (List.map .lane allMessages)
            in
            Just (drawArrow message.x message.y message.x backendLane.yPosition "#e06c75")
            
        _ ->
            Nothing


drawArrow : Float -> Float -> Float -> Float -> String -> Svg msg
drawArrow x1 y1 x2 y2 color =
    Svg.g []
        [ Svg.line
            [ SvgAttr.x1 (String.fromFloat x1)
            , SvgAttr.y1 (String.fromFloat y1)
            , SvgAttr.x2 (String.fromFloat x2)
            , SvgAttr.y2 (String.fromFloat y2)
            , SvgAttr.stroke color
            , SvgAttr.strokeWidth "2"
            , SvgAttr.markerEnd "url(#arrowhead)"
            , SvgAttr.opacity "0.6"
            ]
            []
        , -- Arrowhead marker definition
          Svg.defs []
            [ Svg.marker
                [ SvgAttr.id "arrowhead"
                , SvgAttr.markerWidth "10"
                , SvgAttr.markerHeight "7"
                , SvgAttr.refX "9"
                , SvgAttr.refY "3.5"
                , SvgAttr.orient "auto"
                ]
                [ Svg.polygon
                    [ SvgAttr.points "0 0, 10 3.5, 0 7"
                    , SvgAttr.fill color
                    ]
                    []
                ]
            ]
        ]


-- Helper for List.Extra.unique
unique : List comparable -> List comparable
unique list =
    List.foldl
        (\item acc ->
            if List.member item acc then
                acc
            else
                acc ++ [ item ]
        )
        []
        list