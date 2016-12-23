module Login exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


-- Model


type alias Model =
    { username : String
    , password : String
    }


init : ( Model, Cmd Msg )
init =
    ( { username = ""
      , password = ""
      }
    , Cmd.none
    )



-- Update


type Msg
    = UsernameInput String
    | PasswordInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsernameInput username ->
            ( { model | username = username }, Cmd.none )

        PasswordInput password ->
            ( { model | password = password }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Cool login page" ]
        , Html.form []
            [ input
                [ type_ "text"
                , onInput UsernameInput
                , placeholder "username"
                ]
                []
            , input
                [ type_ "password"
                , onInput PasswordInput
                , placeholder "password"
                ]
                []
            , input [ type_ "submit" ]
                [ text "Login" ]
            ]
        ]


main : Program Never Model Msg
main =
    program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
