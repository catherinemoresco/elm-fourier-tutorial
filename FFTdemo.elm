-- FFTDemo.elm


-- C. Moresco (moresco.cm@gmail.com)

module FFTDemo where

import Graphing exposing (graph, defaultGraph, defaultPlot)
import Signal
import Signal.Extra
import Time
import Text
import Mouse
import Graphics.Element exposing (..)
import Graphics.Input as Input
import Html.Attributes exposing (..)
import Html exposing (Html)

type Action = Done | Next | NoOp
type Phase = Drawing | Result | GameOver

type alias Func = Float -> Float
type alias Coords = (Float, Float)
type alias Pair =   { signal: Func
                    , transform: Func
                    , signalEqn: String
                    , transformEqn: String
                    , title: String}

type alias GameState =  { phase: Phase
                        , pairsVisited: List Pair
                        , pairsToVisit: List Pair
                        , currentPair: Pair
                        , drawing: List Coords
                        , totalScore: Float
                        , scores: List Float}

topMargin = 35 + 30 + 15

-- Initial game state
state : GameState
state = { phase=Drawing
        , pairsVisited=[]
        , pairsToVisit=[]
        , currentPair={
                    signal=sinc
                    , transform=box
                    , signalEqn = "poo"
                    , transformEqn = "poo transform"
                    , title = "poo?" }
        , drawing=[]
        , totalScore=0 
        , scores=[]}

-- Styles
graphStyle = {defaultGraph | 
                width = 400
                , height = 300
                , xInterval = (-5, 5)
                , yInterval = (-1, 2)}

plotStyle = { defaultPlot | plotType=(Graphing.plotType "scatter")
                            }
-- Element compoentns
graphs : GameState ->  Element 
graphs gs = 
        let shownSig = case gs.phase of 
                            Drawing -> Graphing.wrapData[]
                            _ -> Graphing.wrapFunc gs.currentPair.signal 
        in 
        flow right [spacer 30 30 
                    , Html.toElement 400 300 (graph [
                                    (shownSig, defaultPlot)
                                    , (Graphing.wrapData gs.drawing, plotStyle)] graphStyle )
                    , flow down [
                            spacer 50 130
                            , centered <| Text.fromString "&rarr;"
                            ]
                    , Html.toElement 400 300 (graph [(Graphing.wrapFunc gs.currentPair.transform, defaultPlot)] graphStyle )
                    ]

header : Element 
header = flow down [
            spacer 30 30 
            , flow right[ 
                spacer 30 30
                , leftAligned 
                    <| Text.height 35
                    <| Text.typeface ["Patua One", "sans-serif"]
                    <| Text.fromString "So you think you know Fourier transforms?"
                ]
            , spacer 15 15
            ]

doneButton : Element
doneButton = Input.button (Signal.message myMailbox.address Done) "Done"

nextButton : Element
nextButton = Input.button (Signal.message myMailbox.address Next) "Next"

scoreDisplay : GameState -> Element 
scoreDisplay gs =
    flow right [spacer 30 30
                , flow down [leftAligned <| Text.fromString <| toString gs.totalScore]
                ]

-- Update functions 

upstateDrawing : (Float, Float) -> GameState ->  GameState
upstateDrawing coord state = 
        { state | drawing=(coord::state.drawing)}

upStatePhase : Action  -> GameState -> GameState
upStatePhase act gs = 
    case act of 
        Done -> -- Show result and calculate score
            let score = check gs.drawing gs.currentPair.signal in 
            { gs | phase=Result, scores=score::gs.scores, totalScore = gs.totalScore + score} 
        _ -> gs -- Load next 


upState : ((Float, Float), Action) -> GameState -> GameState
upState (dot, act) gs = 
    upStatePhase act (upstateDrawing dot gs)


-- MAIN
main = let stateSignal = Signal.foldp upState state <| Signal.map2 (,) mousePosInSignalFrame myMailbox.signal in 
     Signal.map (flow down) <| Signal.Extra.combine 
        [Signal.constant header
        , Signal.map graphs <| stateSignal
        , Signal.constant <| flow right [spacer 30 30 , doneButton]
        , Signal.constant <| flow right [spacer 30 30, nextButton]
        , Signal.map scoreDisplay <| stateSignal 
        ]

-- SIGNALS =================================================================

-- Mouse

mousePosInSignalFrame : Signal (Float, Float)
mousePosInSignalFrame = 
    Signal.map (\x -> convertCoords (55, 400) (25 + topMargin, 275 + topMargin) graphStyle.xInterval graphStyle.yInterval  x) mouseCoordData

convertCoords : (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Coords -> Coords
convertCoords (xMin, xMax) (yMin, yMax) (newxMin, newxMax) (newyMin, newyMax) (x, y) =
    (lerp xMin x xMax newxMin newxMax, lerp yMax y yMin newyMin newyMax)

lerp : Float -> Float -> Float -> Float -> Float -> Float
lerp x0 x x1 y0 y1 = 
    y0 + (y1 - y0)*((x - x0)/(x1 - x0))

isMouseInDrawSpace : (Float, Float) -> Bool
isMouseInDrawSpace (x, y) = 
    if x > 30 && x < 430 && y > 0 + topMargin && y < 300 + topMargin then True
    else False

mouseCoordData : Signal (Float, Float)
mouseCoordData = 
        Signal.filter isMouseInDrawSpace (0, 0) 
            <| Signal.filterMap filterNothing (-1, -1) maybeMouse

filterNothing : (Bool, (Float, Float)) -> Maybe (Float, Float)
filterNothing (a, coord) = case a of 
    True -> Just coord
    False -> Nothing 

maybeMouse : Signal (Bool, (Float, Float))
maybeMouse =
    Signal.map2 (,) Mouse.isDown
        <| Signal.map (\(x, y) -> (toFloat x, toFloat y)) Mouse.position

-- Mailbox

myMailbox : Signal.Mailbox Action
myMailbox = Signal.mailbox NoOp


-- FUNCTIONS ===============================================================
-- (all with time constant 0)


box : Float -> Float
box x = 
    if x > -1/2.0 && x < 1/2.0 then 1
    else 0

sinc : Float -> Float
sinc x = 
    if x == 0 then 1
    else (1/(pi * x)) * sin (pi*x)


-- UTILITIES ================================================================

check : List Coords -> Func -> Float
check data func = 
    (List.sum <| List.map (\(x, y) -> abs((func x) - y)) data) / (toFloat <| List.length data)



