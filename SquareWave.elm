-- SquareWave.elm
-- A plot of a square wave

module SquareWave where

import Graphing exposing (graph, defaultGraph, defaultPlot)
import Svg
import Signal
import Time
import Html.Attributes exposing (..)
import Html exposing (Html)

func3 : Graphing.ToPlot
func3 = Graphing.wrapFunc (\x -> if ((round x)%2) == 0 then 0 else 1)

graphStyle : Float -> Graphing.GraphAttributes
graphStyle x = { defaultGraph | width=1000, 
                                height=100,
                                yInterval=(0, 1.5), 
                                xInterval=(0.001 * x, 10 + 0.001 * x), 
                                margin=3,
                                axisWidth=0
                                }

main : Signal Html
main = Signal.map (\x -> graph [(func3, {defaultPlot | strokeColor="#ee2560"})] ( graphStyle x) ) <| Signal.foldp (+) 0 (Time.fps 40)

