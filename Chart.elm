module Chart exposing (lineChart)

import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List


lineChart points =
    let
        chartHeight =
            400

        min =
            Maybe.withDefault 0 <| List.minimum <| List.map (\p -> p.overtime - p.compensation) points

        max =
            Maybe.withDefault 0 <| List.maximum <| List.map (\p -> p.overtime - p.compensation) points

        heightScale =
            scale min max 0 chartHeight
    in
        svg [ viewBox ("0 " ++ toString (abs (heightScale min) - 50) ++ " 2100 500") ]
            (List.indexedMap
                (\i p ->
                     g [ class "o-90"]
                        [ rect
                            [ x (toString (i * 40))
                            , y (toString (computeY heightScale chartHeight (p.overtime - p.compensation)))
                            , width "30"
                            , height (toString (abs (heightScale (p.overtime - p.compensation))))
                            , fill "#eee"
                            , stroke "black"
                            , strokeWidth "0.2"
                            , class "dim"
                            ]
                            []
                        , text_
                            [ x (toString ((toFloat i) * 40.0 + 15))
                            , y (toString ((computeY heightScale chartHeight (p.overtime - p.compensation)) + (abs (heightScale (p.overtime - p.compensation)) / 2)))
                            , textAnchor "middle"
                            ]
                            [ text (toString (p.overtime - p.compensation)) ]
                        , text_
                            [ x (toString ((toFloat i) * 40.0 + 15))
                            , y (toString (chartHeight + abs (heightScale min) + 50))
                            , textAnchor "middle"
                            ]
                            [ text (toString i) ]
                        ]
                )
                points
            )


scale : Float -> Float -> Float -> Float -> (Float -> Float)
scale domainMin domainMax rangeMin rangeMax input =
    input * (rangeMax - rangeMin) / (domainMax - domainMin)


computeY heightScale chartHeight overtime =
    if (overtime < 0) then
        chartHeight
    else
        chartHeight - heightScale overtime
