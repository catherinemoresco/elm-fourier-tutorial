-- GraphDemo.elm
-- A demonstration of the Graphing library

-- C. Moresco (moresco.cm@gmail.com)

module GraphDemo where

import Graphing exposing (graph, defaultGraph, defaultPlot)
import Svg
import Signal
import Time
--import Svg.Attributes exposing (..)
import Html.Attributes exposing (..)
import Html exposing (Html)

data : Graphing.ToPlot
data = Graphing.wrapData [(1, 1), (2, 2), (3, 3), (4, 4), (6, 8), (9, 10)]

func : Graphing.ToPlot
func = Graphing.wrapFunc (\x -> sin x)

func2 : Graphing.ToPlot
func2 = Graphing.wrapFunc (\x -> 2 * cos x)

func3 : Graphing.ToPlot
func3 = Graphing.wrapFunc (\x -> 1 * cos (10*(12/7) *x) +  sin (x/10) - 2 * sin (10*(3/5)*(x)) + 2.5)

graphStyle : Float -> Graphing.GraphAttributes
graphStyle x = { defaultGraph | width=800, 
                                height=100,
                                yInterval=(-2, 10), 
                                xInterval=(0.001 * x, 10 + 0.001 * x), 
                                margin=0,
                                axisColor="#FFF"}

main : Signal Html
main = Signal.map (\x -> graph [(func3, {defaultPlot | strokeColor="#D2527F"})] ( graphStyle  x) ) <| Signal.foldp (+) 0 (Time.fps 60)

