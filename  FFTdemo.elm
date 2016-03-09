-- GraphDemo.elm
-- A demonstration of the Graphing library

-- C. Moresco (moresco.cm@gmail.com)

module GraphDemo where

import Graphing exposing (graph, defaultGraph, defaultPlot)
import Svg
import Signal
import Time
import Html.Attributes exposing (..)
import Html exposing (Html)

func : Graphing.ToPlot
func = Graphing.wrapFunc <| List.foldl (addFunc) (\x -> 0) (List.map (\x y -> sin (x pi y)) [1..10])


graphStyle : Float -> Graphing.GraphAttributes
graphStyle x = { defaultGraph | width=800, 
                                height=100,
                                yInterval=(-2, 10), 
                                xInterval=(0.001 * x, 10 + 0.001 * x), 
                                margin=1,
                                axisColor="#FFF"}

main : Signal Html
main = Signal.map (\x -> graph [(func, {defaultPlot | strokeColor="#ee2560"})] ( graphStyle  x) ) <| Signal.foldp (+) 0 (Time.fps 60)

addFunc : (Float->Float) -> (Float->Float) -> (Float->Float)
addFunc x y = (\z -> x z  + y z)