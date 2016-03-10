-- FFTDemo.elm


-- C. Moresco (moresco.cm@gmail.com)

module FFTDemo where

import Graphing exposing (graph, defaultGraph, defaultPlot)
import Signal
import Signal.Extra exposing (combine)
import Time
import Text
import Mouse
import String
import Color exposing (Color, red, yellow, green)
import Graphics.Element exposing (..)
import Graphics.Input as Input
import Html.Attributes exposing (..)
import Html exposing (Html)

type Action = Done | Next | Reset | StartOver | NoOp
type Phase = Drawing | Result | GameOver 

type alias Func = Float -> Float
type alias Coords = (Float, Float)
type alias Pair =   { signal: Func
                    , transform: Func
                    , signalEqn: String
                    , transformEqn: String
                    , tTitle: String
                    , sTitle: String
                    , sXUnits: String
                    , tXUnits: String}

type alias GameState =  { phase: Phase
                        , pairsVisited: List Pair
                        , pairsToVisit: List Pair
                        , currentPair: Pair
                        , drawing: List Coords
                        , totalScore: Float
                        , scores: List Float
                        , lastAction: Action}

topMargin = 35 + 30 + 15 + 40 + 45 + 10 + 75 + 35 + 28

-- Initial game state
state : GameState
state = { phase=Drawing
        , pairsVisited=[]
        , pairsToVisit=[trianglePair, gaussianPair, impulsePair, cosPair]
        , currentPair=boxPair
        , drawing=[]
        , totalScore=0 
        , scores=[]
        , lastAction=NoOp}

-- Styles
graphStyle = {defaultGraph | 
                width = 400
                , height = 300
                , xInterval = (-5, 5)
                , yInterval = (-1, 2)
                , xTicksEvery = 2}

plotStyle = { defaultPlot | plotType=(Graphing.plotType "line")
                          ,  strokeColor="#ee2560" }
-- Element compoentns
graphs : GameState ->  Element 
graphs gs = 
        case gs.phase of 
            GameOver -> spacer 10 10
            _ -> let (shownSig, shownEq, shownTitle) = case gs.phase of 
                                Drawing -> (Graphing.wrapData[], "", "?")
                                _ -> (Graphing.wrapFunc gs.currentPair.signal, gs.currentPair.signalEqn, gs.currentPair.sTitle)
                in 
                flow right [spacer 30 30 
                            , flow down [ 
                                flow right [spacer 0 110
                                        , flow down [ Graphics.Element.width 400 <| centered <| Text.typeface ["Patua One", "serif"] <| Text.fromString shownTitle
                                                     , spacer 15 15 
                                                     , Graphics.Element.width 400 <| centered <| Text.height 12 <|  Text.fromString shownEq]
                                            ]
                                , Html.toElement 400 300 (graph [(shownSig, defaultPlot), (Graphing.wrapData gs.drawing, plotStyle)] {graphStyle | xUnits = gs.currentPair.sXUnits})]
                            , flow down [
                                    spacer 50 230
                                    , centered <| Text.height 30 <| Text.fromString "&rarr;"
                                    ]
                            , flow down [ 
                                 flow right [spacer 0 110, 
                                   flow down [
                                     Graphics.Element.width 400 <| centered <| Text.typeface ["Patua One", "serif"] <| Text.fromString gs.currentPair.tTitle 
                                    , spacer 15 15
                                    , Graphics.Element.width 400 <| centered <| Text.height 12 <| Text.fromString gs.currentPair.transformEqn]

                                ]
                                , Html.toElement 400 300 (graph [(Graphing.wrapFunc gs.currentPair.transform, defaultPlot)] {graphStyle | xUnits = gs.currentPair.tXUnits} )]
                            ]

header : Element 
header = flow down [
            spacer 30 30 
            , flow right[ 
                spacer 30 30
                , Graphics.Element.width 800 <| leftAligned 
                    <| Text.height 30
                    <| Text.typeface ["Patua One", "sans-serif"]
                    <| Text.fromString "So you think you know "
                ]
            , Graphics.Element.width 800 <| centered 
                    <| Text.height 45
                    <| Text.typeface ["Patua One", "sans-serif"]
                    <| Text.fromString "Fourier Transforms?"
            , flow right[ 
                spacer 30 30
                , Graphics.Element.width 800 <| leftAligned 
                    <| Text.color Color.darkGray
                    <| Text.height 14
                    <| Text.typeface ["Lato", "sans-serif"]
                    <| Text.fromString "Click and drag to draw the signal corresponding to the given frequency-domain representation."
            ]
            , spacer 45 45
            , spacer 15 15
            ]

doneButton : GameState -> Element
doneButton gs = 
    if gs.phase /= Drawing then spacer 0 0 else
    (container 900 60 middle) <| flow down [
            if List.length gs.drawing < 2  || gs.phase/=Drawing then (spacer 0 0 ) else 
                    Input.button (Signal.message myMailbox.address Done) "DONE"
              ]

resetButton : GameState -> Element
resetButton gs = 
    if gs.phase /= Drawing then spacer 0 0 else
    (container 900 60 middle) <| flow down [
        if List.length gs.drawing < 2  || gs.phase/=Drawing then (spacer 0 0 ) else 
            Input.button (Signal.message myMailbox.address Reset) "RESET"
        ]

nextButton : GameState -> Element
nextButton gs = 
    if gs.phase /= Result then spacer 0 0 else
    (container 900 60 middle) <| flow down [ if gs.phase /= Result then spacer 0 0 else
            Input.button (Signal.message myMailbox.address Next) "NEXT"
        ]

startOver : GameState -> Element
startOver gs = 
    Input.button (Signal.message myMailbox.address StartOver) "TRY AGAIN"




scoreDisplay : GameState -> Element 
scoreDisplay gs =
    flow right [spacer 30 30
                , flow down 
                    <| List.append [flow right [
                                Graphics.Element.width  200 <| leftAligned <| Text.fromString <| "Average Score: "
                                , rightAligned <| Text.color (percentToColor gs.totalScore) <| Text.fromString  <| String.concat [String.left 4 <| toString gs.totalScore, "%"]
                            ]]
                            (List.indexedMap scoreLine <| List.map2 (,) gs.scores <| gs.pairsVisited)
                ]

scoreLine : Int -> (Float, Pair) -> Element
scoreLine idx (fl, p) = 
    let color = if idx == 0 then Color.darkGray else Color.gray in 
    flow right [
        container 200 19 midLeft <| leftAligned <| Text.color color <| Text.fromString <| String.concat [p.sTitle, " &rarr; ", p.tTitle]
        , rightAligned <| Text.color color <| Text.fromString <| String.concat [String.left 4 <| toString fl, "%"]
    ]

resultScore : GameState -> Element
resultScore gs = 
    if gs.phase == Result then
        let score = case gs.scores of 
            [] -> 0
            x::xs -> x
        in 
             Graphics.Element.container 900 60 middle  <|  centered 
                <| Text.color (percentToColor score) 
                <| Text.typeface ["Patua One", "serif"]
                <| Text.height 50
                <| Text.fromString  
                <| String.concat [String.left 4 
                <| toString score, "%"]
    else if gs.phase == GameOver then
        let score = gs.totalScore in 
            flow down [
                spacer 25 25 
                   , Graphics.Element.container 950 150 middle  <|  centered 
                    <| Text.color (percentToColor gs.totalScore) 
                    <| Text.typeface ["Patua One", "serif"]
                    <| Text.height  100
                    <| Text.fromString  
                    <| String.concat [String.left 4 
                    <| toString gs.totalScore, "%"]
                , spacer 15 15
                , Graphics.Element.container 950 100 midTop  <|centered
                    <| Text.color Color.darkGray 
                    <| Text.typeface ["Lato", "sans-serif"]
                    <| Text.height 40
                    <| Text.fromString
                    <| percentToMessage gs.totalScore
                , Graphics.Element.container 950 75 middle  <| startOver gs
            ]
    else spacer 0 0 
                

-- Update functions 

upstateDrawing : (Float, Float) -> GameState ->  GameState
upstateDrawing coord state = 
        if state.phase == Result || state.phase == GameOver then state else
        case state.drawing of 
            x::xs -> if coord == x then state else 
                        { state | drawing=(coord::state.drawing)}
            _ -> { state | drawing=(coord::state.drawing)}

upStatePhase : Action  -> GameState -> GameState
upStatePhase act gs = 
    if gs.lastAction == act then gs else
    case act of 
        Done -> -- Show result and calculate score
            if List.length gs.drawing < 2 then gs else 
            if gs.phase /= Drawing then gs else
            let score = check gs.drawing gs.currentPair.signal in 
                { gs | phase=Result
                , scores=score::gs.scores
                , totalScore = ( (toFloat <| List.length gs.pairsVisited) *  gs.totalScore + score) / (toFloat <| List.length gs.pairsVisited + 1) -- running average
                , lastAction = Done
                , pairsVisited=gs.currentPair::gs.pairsVisited
            } 
        Next -> -- Load next 
            if gs.phase /= Result then gs else
            case gs.pairsToVisit of 
                [] -> {gs | phase=GameOver, drawing=[]} 
                x::xs -> {gs | phase=Drawing
                            , currentPair=x
                            , pairsToVisit=xs
                            , drawing=[] 
                            , lastAction = Next}
        Reset -> {gs | drawing=[]
                    , lastAction = Reset}
        StartOver -> { phase=Drawing
        , pairsVisited=[]
        , pairsToVisit=[trianglePair, gaussianPair]
        , currentPair=boxPair
        , drawing=[]
        , totalScore=0 
        , scores=[]
        , lastAction=StartOver}
        _ -> gs

upState : ((Float, Float), Action) -> GameState -> GameState
upState (dot, act) gs = 
    upStatePhase act (upstateDrawing dot gs)


-- MAIN
main = let stateSignal = Signal.foldp upState state <| Signal.map2 (,) mousePosInSignalFrame myMailbox.signal in 
     Signal.map (flow down) <| Signal.Extra.combine 
        [Signal.constant header
        , Signal.map graphs <| stateSignal
        , Signal.constant <| spacer 15 15 
        -- Should really write a function for this
        , Signal.map resultScore stateSignal
        , Signal.map resetButton stateSignal
        , Signal.map doneButton stateSignal
        , Signal.map nextButton stateSignal
        , Signal.constant <| spacer 15 15 
        , Signal.map (container 900 300 midTop) <|  Signal.map scoreDisplay stateSignal
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

sinc2 : Float -> Float
sinc2 x = 
    if x == 0 then 1
    else ((1/(pi * x)) * sin (pi*x))^2

triangle : Float -> Float
triangle x = 
    if abs(x) < 1 then 1 - abs(x)
    else 0

gaussian : Float -> Float 
gaussian x = 
    e^(-1 * x^2 * 1/2)

gaussianTransform : Float -> Float 
gaussianTransform x = 
    e^(-1 * x^2 * 1/2)

cosTransform : Float -> Float
cosTransform x =
    if x == 1 || x == -1 then 1
    else 0

impulse : Float -> Float
impulse x = 
    if x == 0 then snd graphStyle.yInterval else 0

impulseTrain : Float -> Float 
impulseTrain x =
    if abs(x - (toFloat <| round x)) < 0.01 then snd graphStyle.yInterval else 0

const : Float -> Float 
const x = 1

-- PAIRS

--type alias Pair =   { signal: Func
--                    , transform: Func
--                    , signalEqn: String
--                    , transformEqn: String
--                    , title: String}

boxPair = {signal=box 
    , transform=sinc 
    , signalEqn="$f(t) = \\begin{cases}
        1, & |t| < \\tau/2 \\\\
        0, & |t| > \\tau/2  
        \\end{cases} $"
    , transformEqn="$F(f) = \\tau \\text{sinc} (f \\tau)$"
    , tTitle="Sinc"
    , sTitle="Box"
    , sXUnits="T"
    , tXUnits="/T"}

trianglePair = {signal=triangle
    , transform=sinc2
    , signalEqn="$f(t) = \\begin{cases}
        1 - |t|/\\tau, & |t| < \\tau \\\\
        0, & |t| > \\tau
        \\end{cases} $"
    , transformEqn="$F(f) = \\tau \\text{sinc}^2 (f \\tau)$"
    , tTitle="Squared sinc"
    , sTitle = "Triangle"
    , sXUnits = "T"
    , tXUnits = "/T"
    }

gaussianPair = {
    signal=gaussian 
    , transform=gaussianTransform
    , signalEqn="$f(t) = e^{-\\frac{1}{2}t^2} $"
    , transformEqn="$F(f) = \\tau (2 \\pi )^{\\frac{1}{2}} e^{-\\frac{1}{2}t^2}$"
    , tTitle="Gaussian"
    , sTitle="Gaussian"
    , sXUnits = "T"
    , tXUnits = "/T"
    }

impulsePair = {
    signal=impulse 
    , transform=const
    , signalEqn="$f(t) = \\delta(t) $"
    , transformEqn="$F(f) = 1$"
    , tTitle="Constant"
    , sTitle="Impulse"
    , sXUnits = "T"
    , tXUnits = "/T"
    }

cosPair = {
    signal =  (\x -> cos (2 * pi * x))
    , transform = cosTransform
    , signalEqn = "f(t) = \\cos (t0)$"
    , transformEqn = "$F(f) = \\delta\\delta^+(f0)"
    , tTitle = "Impulses"
    , sTitle = "Cosine"
    , sXUnits = "t0"
    , tXUnits = "f0"
    }


-- UTILITIES ================================================================

check : List Coords -> Func -> Float
check data func = 
    --let filteredData = List.filter (\(x, y) -> if abs(func x) > 0.01 && (abs ((func x )- y )) > 0.1 then True else False) data in 
    --if filteredData == [] then case data of 
    --    [] -> 66.3 -- This should never happen, but I don't wannt crash the program so
    --    x::xs -> 100 - (unJustFloat <| List.maximum (List.map2 (\(x, y) z -> abs (x - y)) (List.repeat 11 x) (List.map func [0..10])))
    --else
    let avgDiff = (List.sum <| List.map (\(x, y) -> abs((func x) - y)) data) / (toFloat <| List.length data) in
        100 - 100 * (avgDiff/3)


unJustFloat : Maybe Float -> Float
unJustFloat x = 
    case x of 
        Nothing -> 66.3
        Just a -> a

percentToColor : Float -> Color
percentToColor x =
    if x < 80 then red
    else if x < 95 then yellow
    else green

percentToMessage : Float -> String
percentToMessage x = 
    if x < 70 then "Are you even trying?"
    else if x < 80 then "Well, at least you tried."
    else if x < 95 then "Nice."
    else "Holy cow!"

-- PORTS ========================================================================
-- (just for MathJax refresh)

port refreshMathJax : Signal Float
port refreshMathJax = Time.fps 60

