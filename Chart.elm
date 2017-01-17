module Chart exposing (lineChart)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html


lineChart : List { a | overtime : Float } -> Html.Html msg
lineChart points =
    svg [ viewBox "0 0 1810 150" ]
        (List.indexedMap
            (\i p ->
                g []
                    [ text_
                        [ x (toString ((toFloat i) * 35.0 + 12.5))
                        , y
                            (if p.overtime < 0 then
                                (toString (150 - p.overtime * 20 - 45 - (abs (p.overtime * 20))))
                             else
                                (toString (150 - p.overtime * 20 - 45))
                            )
                        , textAnchor "middle"
                        ]
                        [ text ((toString p.overtime) ++ "h") ]
                    , rect
                        [ x (toString (i * 35))
                        , y
                            (if p.overtime < 0 then
                                (toString (150 - p.overtime * 20 - 40 - (abs (p.overtime * 20))))
                             else
                                (toString (150 - p.overtime * 20 - 40))
                            )
                        , width "25"
                        , height (toString (abs (p.overtime * 20)))
                        , fill "#eee"
                        , class "dim"
                        ]
                        []
                    , text_
                        [ x (toString ((toFloat i) * 35.0 + 12.5))
                        , y "150"
                        , textAnchor "middle"
                        ]
                        [ text (toString (i + 1)) ]
                    ]
            )
            points
        )
