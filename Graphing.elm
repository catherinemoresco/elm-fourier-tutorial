-- Graphing.elm
-- A simple library for graphing
-- C. Moresco (moresco.cm@gmail.com)
-- 2016

module Graphing 
    (Graph, ToPlot, GraphAttributes, wrapData, wrapFunc, graph, defaultGraph, defaultPlot) where

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
type alias GraphAttributes = {
                                width:  Int, 
                                height:  Int, 
                                ticks:  Int, 
                                margin: Int,
                                xInterval: (Float, Float),
                                yInterval: (Float, Float),
                                backgroundColor: String,
                                axisColor: String,
                                axisWidth: Int
                            }
type alias PlotAttributes = {
                                strokeColor: String,
                                strokeWidth: String
                            }

{-| Main function for a simple graph. 

    Takes an x-interval, a y-interval, and a function
    or set of points -}
graph : List (ToPlot, PlotAttributes) ->  GraphAttributes -> Html
graph toGraph graphAttrs =
    let graphSvg toGraph= 
        case toGraph of 
            (Func function, plotAttrs) -> Svg.g [transform <| String.concat [ "translate(", toString graphAttrs.margin,  " ", toString graphAttrs.margin, ")"]] [funcPlot graphAttrs plotAttrs function]
            (Data coords, plotAttrs) -> Svg.g [transform <| String.concat [ "translate(", toString graphAttrs.margin,  " ", toString graphAttrs.margin, ")"]] [dataField graphAttrs plotAttrs coords]
    in 
        Svg.svg [width <| toString graphAttrs.width, height <| toString graphAttrs.height]  (List.append [xAxis graphAttrs, yAxis graphAttrs] (List.map graphSvg toGraph))

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

{-| Functions for a scatter plot -}
dataField : GraphAttributes -> PlotAttributes -> List Coords -> Svg.Svg
dataField ga pa data = 
    let w = ga.width - 2*ga.margin
        h = ga.height - 2*ga.margin 
        newCoords = List.map (convertCoords ga.xInterval ga.yInterval (w, h)) data in 
    Svg.g [] (makeDots (w, h) newCoords)

makeDots : (Float, Float) -> List Coords -> List Svg.Svg
makeDots dimensions data = 
        let makeDot (x, y) = 
            Svg.circle [cx <| toString x, cy <| toString y, r "5", fill "red"] []
        in 
        List.map makeDot data

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
                axisWidth = 2}

defaultPlot : PlotAttributes
defaultPlot = {strokeColor="#000",
               strokeWidth="2px"}

wrapData : List Coords -> ToPlot
wrapData data = Data data

wrapFunc : (Float -> Float) -> ToPlot
wrapFunc func = Func func


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