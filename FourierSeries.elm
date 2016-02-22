-- FourierSeries.elm

module FourierSeries where 

import ParseInt exposing (parseInt)
import Graphing exposing (graph, defaultGraph, defaultPlot)
import Svg
import Signal
import Time
import String
import Graphics.Element as Element
import Html.Attributes exposing (..)
import Html.Events as Events
import Html exposing (Html)
import Html.Lazy exposing (lazy)

squareWave: Graphing.ToPlot
squareWave = Graphing.wrapFunc (\x -> if ((round (x + 0.5)%2)) == 0 then -1 else 1)

sineWavesAtN : Float -> Float -> Graphing.ToPlot
sineWavesAtN period n = Graphing.wrapFunc (List.foldr addFunc (\x -> 0) (List.map (nthSin period ) (List.map (\x -> 2*x - 1) [1..n])))

nthSin : Float -> Float -> Float -> Float 
nthSin period n x = (4/pi) * (1/n) * sin (n*pi*x/period)


graphStyle : Float -> Graphing.GraphAttributes
graphStyle x = { defaultGraph | width=800, 
                                height=300,
                                yInterval=(-1.5, 1.5), 
                                xInterval=(0, 4), 
                                margin=15,
                                axisColor="#fff",
                                axisWidth=0
                            }

componentSinusoids : Float -> Float -> List Graphing.ToPlot
componentSinusoids period n =
    List.map Graphing.wrapFunc <| List.map (nthSin period) [1..n]

mailbox : Signal.Mailbox String
mailbox = Signal.mailbox "1"

view : Float -> Html
view v = let x = v
             event = Events.on "input" Events.targetValue (Signal.message mailbox.address) 
         in 
         Html.div [ class "centered-content"] 
                [ (graph (List.concat [(List.map2 (,) (componentSinusoids 1 x) (List.repeat (round x) {defaultPlot | strokeColor="#eee"})),
                                      [(Graphing.wrapFunc (nthSin 1 x), {defaultPlot | strokeColor="#bbb"})],
                                      [(squareWave, defaultPlot) , (sineWavesAtN 1 x, { defaultPlot | strokeColor="#D2527F" })]]) (graphStyle 0))
           , Html.div [class "centered cm"] [Html.text <| String.append "n = " <|toString x]

           , Html.div [class "spacer"] []
           , Html.div [class "auto-margin limited-width"] [Html.input [ type' "range"
              , Html.Attributes.min "1"
              , Html.Attributes.max "20"
              , value <| toString x
              , event
              , class "slider"
              , style [("width", "100%"),
                        ("max-width", "800px")]
              ] []]
           , Html.div [class "double-spacer"] []
           , Html.div [class "centered limited-width"] [ Html.div [class "inline-block auto-margin"] [(equationImg x)]]
           , Html.div [class "spacer"] []]

main : Signal Html
main = Signal.map (lazy view) <| Signal.map (toFloat << unsafeToInt) mailbox.signal

unsafeToInt : String -> Int 
unsafeToInt x = case parseInt x of 
    Ok x -> x
    _ -> Debug.crash "unsafeToInt called on a bad string"


-- UTILITIES
addFunc : (Float->Float) -> (Float->Float) -> (Float->Float)
addFunc x y = (\z -> x z  + y z)

-- IMAGES
-- for now, we're shamelessly abusing codecogs; will download all of these as SVGs eventually
equationTermImg : Float -> Html
equationTermImg n = Html.img [src <| String.concat ["equations/CodeCogsEqn(", toString n, ").svg"], class "eqnImg middle-term"] []

lastTermImg : Float -> Html
lastTermImg n = Html.img [src <| String.concat ["equations/CodeCogsEqn(", toString n, ").svg"], class "eqnImg last-term"] []

equationImg : Float -> Html
equationImg n = 
  let fx = Html.img [src "equations/CodeCogsEqn(fx).svg", class "eqnImg"][]
      plus = if n > 1 then Html.img [src "equations/CodeCogsEqn(plus).svg", class "plus"] [] else Html.img [] []
      leadingFactor = Html.img [src "equations/CodeCogsEqn(start).svg", class "eqnImg"] [] 
      closeParen = Html.img [src "equations/CodeCogsEqn(end).svg", class "eqnImg closeParen"] [] 
    in
  Html.div [] <| List.concat <|
                    [[fx],
                     [leadingFactor], 
                     List.intersperse plus <| List.map equationTermImg [1..(n-1)],
                     [plus],
                     [lastTermImg n],
                     [closeParen]]
