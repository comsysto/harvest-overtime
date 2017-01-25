module Chart exposing (lineChart)

import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List


lineChart : List { a | compensation : Float, overtime : Float } -> Html.Html msg
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

        svgYOffset =
            toString (abs (heightScale min) - 50)
    in
        svg [ viewBox ("0 " ++ svgYOffset ++ " 2100 550") ]
            (List.indexedMap
                (\i p ->
                    g [ class "o-90" ]
                        [ rect
                            [ x (toString (i * 40))
                            , y (toString (computeY heightScale chartHeight (p.overtime - p.compensation)))
                            , width "35"
                            , height (toString (abs (heightScale (p.overtime - p.compensation))))
                            , fill "#eee"
                            , stroke "black"
                            , strokeWidth "0.3"
                            , class "dim"
                            ]
                            []
                        , text_
                            [ x (toString ((toFloat i) * 40 + 17.5))
                            , y (toString ((computeY heightScale chartHeight (p.overtime - p.compensation)) + (abs (heightScale (p.overtime - p.compensation)) / 2) + 5))
                            , textAnchor "middle"
                            ]
                            [ text (toString (p.overtime - p.compensation)) ]
                        , text_
                            [ x (toString ((toFloat i) * 40 + 17.5))
                            , y (toString (chartHeight + abs (heightScale min) + 50))
                            , textAnchor "middle"
                            ]
                            [ text (toString (i + 1)) ]
                        , text_
                            [ x "1050"
                            , y (toString (chartHeight + abs (heightScale min) + 80))
                            , textAnchor "middle"
                            ]
                            [ text "Weeks" ]
                        ]
                )
                points
            )


scale : Float -> Float -> Float -> Float -> (Float -> Float)
scale domainMin domainMax rangeMin rangeMax input =
    input * (rangeMax - rangeMin) / (domainMax - domainMin)


computeY : (Float -> Float) -> Float -> Float -> Float
computeY heightScale chartHeight overtime =
    if (overtime < 0) then
        chartHeight
    else
        chartHeight - heightScale overtime
