-- ImpulseDemo.elm
-- A demonstration of the Graphing library

-- C. Moresco (moresco.cm@gmail.com)

module SquareFreq where

import Graphing exposing (graph, defaultGraph, defaultPlot, PlotAttributes)
import Svg
import Signal
import Time
import Html.Attributes exposing (..)
import Html exposing (Html)
import Html.Events as Events

data : Graphing.ToPlot
data = Graphing.wrapData  dataList


dataList : List (Float, Float)
dataList = List.map (\ x-> (x, abs <| (4/pi)/x)) <| List.map (\x -> 2*x - 1) [1..20] 

graphStyle : Graphing.GraphAttributes
graphStyle = { defaultGraph | width=800, 
                                height=300,
                                yInterval=(0, 1.5), 
                                xInterval=(0, 40), 
                                margin=25,
                                axisColor="#000",
                                axisWidth=2,
                                xTicksEvery=5,
                                yTicksEvery=1
                            }

main : Html
main = graph [(data, {defaultPlot | strokeColor="#ee2560", dotColor = "#ee2560"})] graphStyle 



