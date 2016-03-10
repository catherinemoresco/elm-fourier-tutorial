-- Graphing.elm
-- A simple library for graphing
-- C. Moresco (moresco.cm@gmail.com)
-- 2016

module Graphing 
    (Graph, ToPlot, plotType, GraphAttributes, wrapData, wrapFunc, graph, defaultGraph, defaultPlot) where

import List
import String
import Color
import Svg
import Signal
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Animation exposing (..)
import Time exposing (second)

{-| A graph will be an SVG element, which is backed 
by `Svg.Svg` in  `evancz/elm-svg` -}
type alias Graph = Svg.Svg
type alias Coords = (Float, Float)
type ToPlot = Func (Float -> Float) | Data (List Coords)
type DataPlot = Scatter | Impulse | Line

type alias GraphAttributes = {
                                width:  Int, 
                                height:  Int, 
                                ticks:  Int, 
                                margin: Int,
                                xInterval: (Float, Float),
                                yInterval: (Float, Float),
                                backgroundColor: String,
                                axisColor: String,
                                axisWidth: Int,
                                xTicksEvery: Float,
                                yTicksEvery: Float, 
                                xUnits: String,
                                yUnits: String
                            }

type alias PlotAttributes = {
                                strokeColor: String,
                                dotColor: String,
                                strokeWidth: String, 
                                plotType: DataPlot
                            }

{-| Main function for a simple graph. 

    Takes an x-interval, a y-interval, and a function
    or set of points -}
graph : List (ToPlot, PlotAttributes) ->  GraphAttributes -> Html
graph toGraph graphAttrs =
    let graphSvg toGraph= 
        case toGraph of 
            (Func function, plotAttrs) -> 
                Svg.g [transform <| translate graphAttrs] [funcPlot graphAttrs plotAttrs function]
            (Data coords, plotAttrs) -> 
                Svg.g [transform <| translate graphAttrs] [dataField graphAttrs plotAttrs coords]

    in 
        Svg.svg [width <| toString graphAttrs.width, height <| toString graphAttrs.height]  (List.append [xAxis graphAttrs, yAxis graphAttrs, xTicks graphAttrs, yTicks graphAttrs] (List.map graphSvg toGraph))

translate : GraphAttributes -> String
translate graphAttrs = 
    String.concat [ "translate(", toString graphAttrs.margin,  " ", toString graphAttrs.margin, ")"]


xAxis : GraphAttributes -> Svg.Svg
xAxis graphStyles = 
    let bottom = graphStyles.height - graphStyles.margin
        top = graphStyles.margin
        left = graphStyles.margin
        right = graphStyles.width - graphStyles.margin
    in 
        Svg.line [x1 <| toString graphStyles.margin, 
                y1 <| toString bottom, 
                x2 <| toString right,
                y2 <| toString bottom,
                stroke graphStyles.axisColor,
                strokeWidth <| toString graphStyles.axisWidth
        ] []
    
yAxis : GraphAttributes -> Svg.Svg
yAxis graphStyles = 
    let bottom = graphStyles.height - graphStyles.margin
        top = graphStyles.margin
        left = graphStyles.margin
        right = graphStyles.width - graphStyles.margin
    in 
        Svg.line [x1 <|toString left, 
                y1 <| toString bottom, 
                x2 <| toString left,
                y2 <| toString top,
                stroke graphStyles.axisColor,
                strokeWidth <| toString graphStyles.axisWidth
        ] []

xTicks : GraphAttributes -> Svg.Svg
xTicks ga = 
    let (xmin, xmax) = ga.xInterval in
    let numticks = floor <| (xmax - xmin)/ga.xTicksEvery in 
    let step = (toFloat ga.width - 2 * (toFloat ga.margin))/ (toFloat numticks)
    in
    let bottom = ga.height - ga.margin
        top = ga.margin
        left = ga.margin
        right = ga.width - ga.margin
    in 
    let lines =  List.map (\x -> Svg.line [ x1 <| toString <| x * step + toFloat left
                                       , y1 <| toString (bottom + 5)
                                       , x2 <| toString <| x * step + toFloat left
                                       , y2 <| toString (bottom - 5)
                                       , stroke ga.axisColor
                                       , strokeWidth <| toString ga.axisWidth] []) 
                                <| List.map toFloat [1..numticks]
        labels = List.map (\k -> Svg.text' [ x <| toString <| k * step + toFloat left + 3
                                            , y <| toString (bottom + 15)
                                            , fontFamily "monospace"
                                            ] [Svg.text <| String.concat [toString <| (k * ga.xTicksEvery) + xmin, ga.xUnits]]) 
                                <| List.map toFloat [1..numticks]
    in
    Svg.g [] <| List.append lines labels

yTicks : GraphAttributes -> Svg.Svg
yTicks ga = 
    let (ymin, ymax) = ga.yInterval in
    let numticks = floor <| (ymax - ymin)/ga.yTicksEvery in 
    let step = (toFloat ga.height - 2 * (toFloat ga.margin))/ ((ymax - ymin)/ga.yTicksEvery)
    in
    let bottom = ga.height - ga.margin
        top = ga.margin
        left = ga.margin
        right = ga.width - ga.margin
    in 
    let lines =  List.map (\x -> Svg.line [ x1 <| toString <| toFloat left - 5
                                       , y1 <| toString <| toFloat bottom -  x * step 
                                       , x2 <| toString <| toFloat left + 5
                                       , y2 <| toString <| toFloat bottom -  x * step 
                                       , stroke ga.axisColor
                                       , strokeWidth <| toString ga.axisWidth] []) 
                                <| List.map toFloat [1..numticks]
        labels = List.map (\k -> Svg.text' [ y <| toString <| toFloat bottom + 5  - k * step
                                            , x <| toString (left - 10)
                                            , textAnchor "end"
                                            , fontFamily "monospace"
                                            ] [Svg.text <| toString <| (k * ga.yTicksEvery) + ymin]) 
                                <| List.map toFloat [1..numticks]
    in
    Svg.g [] <| List.append lines labels

{-| Functions for a scatter plot -}
dataField : GraphAttributes -> PlotAttributes -> List Coords -> Svg.Svg
dataField ga pa data = 
    let w = ga.width - 2*ga.margin
        h = ga.height - 2*ga.margin 
        newCoords = List.map (convertCoords ga.xInterval ga.yInterval (w, h)) data in
    let (lines, dots) = case pa.plotType of 
                    Scatter -> ([], (makeDots (w, h) newCoords pa)) 
                    Impulse -> (makeImpulses (w, h) newCoords pa, (makeDots (w, h) newCoords pa))
                    Line -> ([pathFromCoords pa newCoords], [])
    in
    Svg.g [] <| List.concat [lines, dots]

makeDots : (Float, Float) -> List Coords -> PlotAttributes -> List Svg.Svg
makeDots dimensions data pa = 
        let makeDot (x, y) = 
            Svg.circle [cx <| toString x, cy <| toString y, r "5", fill pa.dotColor] []
        in 
        List.map makeDot data

makeImpulses : (Float, Float) -> List Coords -> PlotAttributes -> List Svg.Svg
makeImpulses dimensions data pa = 
        let makeImpulse (x, y) = 
            pathFromCoords pa [(x, y), (x, snd dimensions)]
        in 
        List.map makeImpulse data

{-| Functions for plotting a continuous function -}
funcPlot : GraphAttributes -> PlotAttributes -> (Float -> Float) -> Svg.Svg
funcPlot ga pa func = 
     let w = ga.width - 2*ga.margin
         h = ga.height - 2*ga.margin 
      in 
    Svg.g [] [makePath ga pa (w, h) func]

makePath : GraphAttributes -> PlotAttributes -> (Float, Float) -> (Float -> Float) -> Svg.Svg
makePath ga pa (w, h) func = 
    let 
        samples = linearSpace ga.xInterval 700
    in
    let 
        coordinates = List.map2 (,) samples (List.map func samples)
    in 
    pathFromCoords pa <| List.map (convertCoords ga.xInterval ga.yInterval (w, h)) coordinates


pathFromCoords : PlotAttributes -> List Coords -> Svg.Svg
pathFromCoords pa coords = 
    let addPt samples = case samples of 
        [] -> ""
        (x, y)::xs -> String.concat [" L", toString x, " ", toString y, " ", addPt xs] 
    in 
    let (x, y) = case coords of 
        [] -> (0, 0)
        c::cs -> c in
    Svg.path [d (String.concat ["M", toString x, " ", toString y, addPt (List.drop 1 coords)]), stroke pa.strokeColor, strokeWidth pa.strokeWidth, fill "none"] []


{-| Input an (xmin, xmax), (ymin, ymax), (width, height), and coords for a new set of coords 

Used to convert data coordinates to pixel coordinates-}
convertCoords : (Float, Float) -> (Float, Float) -> (Float, Float) -> Coords -> Coords
convertCoords (xMin, xMax) (yMin, yMax) (width, height) (x, y) =
    (lerp xMin x xMax 0 width, lerp yMax y yMin 0 height)

linearSpace : (Float, Float) -> Float -> List Float
linearSpace (min, max) numPoints = 
    let size = max - min 
        stepSize = size / numPoints in 
    List.map (\x -> x * stepSize + min) [0..(numPoints - 1)]

{-| Linear interpolation -}
lerp : Float -> Float -> Float -> Float -> Float -> Float
lerp x0 x x1 y0 y1 = 
    y0 + (y1 - y0)*((x - x0)/(x1 - x0))

defaultGraph : GraphAttributes
defaultGraph = {width=400, 
                height=400, 
                ticks=0, 
                backgroundColor="#f5f5f5", 
                axisColor="#555",
                margin=25, 
                xInterval=(0, 10), 
                yInterval=(0, 10), 
                axisWidth = 2, 
                xTicksEvery = 1,
                yTicksEvery = 1,
                xUnits="", 
                yUnits=""
            }

defaultPlot : PlotAttributes
defaultPlot = {strokeColor="#000",
               strokeWidth="2px",
               plotType=Impulse,
               dotColor="#000"
            }

wrapData : List Coords -> ToPlot
wrapData data = Data data

wrapFunc : (Float -> Float) -> ToPlot
wrapFunc func = Func func

plotType : String -> DataPlot
plotType x = 
    if x == "impulse" then Impulse
    else if x == "scatter" then Scatter
    else if x == "line" then Line
    else Debug.crash "that's not a plot type!"


{-| Some defualt constants -}
defaultMax : Float
defaultMax = 100


{-| Animations -}
radiusAnimation : Animation
radiusAnimation = animation 0 |> Animation.from 0 |> Animation.to 10 |> duration (4*second)

clock : Signal Time.Time
clock = Signal.foldp (+) 0 (Time.fps 40)

radius : Signal Float
radius = Signal.map (\x -> animate x radiusAnimation) clock