-- ImpulseDemo.elm
-- A demonstration of the Graphing library

-- C. Moresco (moresco.cm@gmail.com)

module ImpulseDemo where

import Graphing exposing (graph, defaultGraph, defaultPlot, PlotAttributes)
import Svg
import Signal
import Time
import Mouse
import Json.Decode as Json
--import Svg.Attributes exposing (..)
import Html.Attributes exposing (..)
import Html exposing (Html)
import Html.Events as Events

data : Signal Graphing.ToPlot
data = Signal.map Graphing.wrapData  mouseCoordData

function : Signal Graphing.ToPlot
function = Signal.map Graphing.wrapFunc <|
    let sinFromFreqSpace (x, y) = (\z -> y * sin (x * pi * z)) in 
        Signal.map (List.foldr (addFunc) (\_ -> 0 ))
            <| Signal.map (List.map sinFromFreqSpace) mouseCoordData

dataList : List (Float, Float)
dataList = List.concat <| List.map2 (\x y -> [x, y]) 
                                      (List.map (\ x-> (x, abs <| (4/pi)/x)) <| List.map (\x -> 2*x - 1) [1..10]) 
                                      (List.map (\x -> (x, 0)) <| List.map (\x -> 2*x) [1..10])

mouseCoordData : Signal (List (Float, Float))
mouseCoordData = 
    Signal.foldp (updateData graphStyle) dataList 
        <| Signal.filterMap filterNothing (-1, -1) maybeMouse

filterNothing : (Bool, (Float, Float)) -> Maybe (Float, Float)
filterNothing (a, coord) = case a of 
    True -> Just coord
    False -> Nothing 

maybeMouse : Signal (Bool, (Float, Float))
maybeMouse =
    Signal.map2 (,) Mouse.isDown
        <| Signal.map (\(x, y) -> (toFloat x, toFloat y)) Mouse.position

getSector : (Float, Float) -> Graphing.GraphAttributes -> Maybe Int
getSector (x, y) ga = 
    let margin = toFloat ga.margin
        width = toFloat (ga.width - 2 * ga.margin)
        height = toFloat ga.height
        _ = Debug.log "" x
    in
        if x < margin then Nothing
        else if x > width + margin + 15then Nothing
        else if y < margin then Nothing
        else if y > height + margin then Nothing
        else let sectorSize = (width/(toFloat <| List.length dataList)) in 
            Just <| (floor <| (x - margin - 15)/sectorSize - 0.5)

getYValue : (Float, Float) -> Graphing.GraphAttributes -> Maybe Float  
getYValue (x, y) ga =   
    let margin = toFloat ga.margin
        width = toFloat ga.width
        height = toFloat ga.height
        (ymin, ymax) = ga.yInterval
    in
        if y < margin then Nothing
        else if y > height + margin then Nothing
        else Just (lerp (height + margin) y margin ymin ymax)

updateData : Graphing.GraphAttributes -> (Float, Float) -> List (Float, Float) -> List (Float, Float)
updateData ga mousePos data = 
    let foo (sector, yval) data = 
        case data of 
            [] -> []
            x::xs -> if sector == 0 then (fst x, yval) :: xs
                     else x :: (foo (sector - 1, yval) xs)
    in
    case (getSector mousePos ga, getYValue mousePos ga) of 
        (Nothing, _) -> data
        (_, Nothing ) -> data
        (Just sector, Just yval) -> foo (sector, yval) data


graphStyle : Graphing.GraphAttributes
graphStyle = { defaultGraph | width=800, 
                                height=200,
                                yInterval=(0, (4/pi)*1.5), 
                                xInterval=(0, 20), 
                                margin=25,
                                axisColor="#000", 
                                xTicksEvery=1}

graphStyleSin : Graphing.GraphAttributes
graphStyleSin = { defaultGraph | width=800, 
                                height=200,
                                yInterval=(-10, 10), 
                                xInterval=(0, 4), 
                                margin=25,
                                axisColor="#000",
                                yTicksEvery=10}

main : Signal Html
main = Signal.map (Html.div []) <| combine  
                                    [ freq
                                    , Signal.constant <| Html.div [class "spacer"] []
                                    , sinusoid
                                    ]

freq : Signal Html
freq = Signal.map (Html.div [class "white shadow"] ) <| combine
                                        [Signal.constant <| Html.div [class "spacer"] []
                                        , Signal.constant <| Html.div [class "centered header-font"] [Html.text "Frequency Domain Representation"]
                                        , Signal.map (\x -> graph [(x, {defaultPlot | strokeColor="#999", dotColor="#ee2560"})] graphStyle) <| data]

sinusoid : Signal Html
sinusoid = Signal.map (Html.div [class "white shadow"] ) <| combine 
                                    [Signal.constant <| Html.div [class "spacer"] []
                                    ,Signal.constant <| Html.div [class "centered header-font"] [Html.text "Sinusoidal Signal"]
                                    , Signal.map (\x -> graph [(x, {defaultPlot | strokeColor="#ee2560"})] graphStyleSin) <| function
                                    ]

lerp : Float -> Float -> Float -> Float -> Float -> Float
lerp x0 x x1 y0 y1 = 
    y0 + (y1 - y0)*((x - x0)/(x1 - x0))

addFunc : (Float->Float) -> (Float->Float) -> (Float->Float)
addFunc x y = (\z -> x z  + y z)

{-| This was originally part of the signal library, then it wasn't...then it was added again? Not sure.
 I got this from https://groups.google.com/forum/#!topic/elm-discuss/CVRhrF4Wn7k. -}

combine : List (Signal a) -> Signal (List a)
combine l = List.foldr (Signal.map2 (\x y -> [x] ++ y)) (Signal.constant []) l


