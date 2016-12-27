module Main exposing (..)

import Harvest.Api exposing (..)
import Harvest.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Navigation exposing (Location)


-- Model


type alias Model =
    { location : Location
    , access_token : Maybe String
    , hours : Maybe (List DailyHours)
    , who_am_i : Maybe WhoAmI
    , error : String
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        token =
            getTokenFromHash location.hash

        initCmd =
            case token of
                Just aToken ->
                    Http.send WhoAmI <| getUserInfo aToken

                Nothing ->
                    Cmd.none

        {-
           initCmd2 =
               case token of
                   Just aToken ->
                       Http.send Daily (getDailyForDate aToken "50" "2016")

                   Nothing ->
                       Cmd.none

           initCmd3 =
               case token of
                   Just aToken ->
                       Http.send Hours (getDailyHoursForDateRange "146305" "20161219" "20161225" aToken)

                   Nothing ->
                       Cmd.none
        -}
    in
        ({ location = location
         , access_token = token
         , hours = Nothing
         , who_am_i = Nothing
         , error = ""
         }
            ! [ Navigation.modifyUrl "#", initCmd ]
        )



-- Update


type Msg
    = LocationChange Location
    | Hours (Result Http.Error (List DailyHours))
    | WhoAmI (Result Http.Error WhoAmI)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationChange location ->
            ( { model | location = location }, Cmd.none )

        Hours (Ok hours) ->
            ( { model | hours = Just hours }, Cmd.none )

        Hours (Err err) ->
            ( { model | error = toString err }, Cmd.none )

        WhoAmI (Ok who) ->
            loadHours model who

        WhoAmI (Err err) ->
            ( { model | error = toString err }, Cmd.none )


loadHours : Model -> WhoAmI -> ( Model, Cmd Msg )
loadHours model who =
    case model.access_token of
        Just token ->
            { model | who_am_i = Just who } ! [ Http.send Hours (getDailyHoursForDateRange (toString who.user.id) "20161219" "20161225" token) ]

        Nothing ->
            { model | who_am_i = Just who } ! []



-- View


view : Model -> Html Msg
view model =
    case model.access_token of
        Just token ->
            div []
                [ div [ style [ ( "margin", "1rem" ) ] ] [ renderUserInfo model.who_am_i ]
                , hr [] []
                , div [ style [ ( "margin", "1rem" ) ] ] [ renderHours model.hours ]
                , hr [] []
                , div [ style [ ( "margin", "1rem" ) ] ] [ text model.error ]
                ]

        Nothing ->
            renderLoginButton


renderLoginButton : Html Msg
renderLoginButton =
    div []
        [ a [ href harvestAuthUrl ] [ text "Login with Harvest" ]
        ]


renderUserInfo : Maybe WhoAmI -> Html Msg
renderUserInfo info =
    case info of
        Just userInfo ->
            div []
                [ img [ src <| userInfo.company.base_uri ++ userInfo.user.avatar_url ] []
                , div []
                    [ span [] [ text "ID:" ]
                    , span [] [ text <| toString userInfo.user.id ]
                    ]
                , div []
                    [ span [] [ text "First name: " ]
                    , span [] [ text userInfo.user.first_name ]
                    ]
                , div []
                    [ span [] [ text "Last name: " ]
                    , span [] [ text userInfo.user.last_name ]
                    ]
                , div []
                    [ span [] [ text "Admin: " ]
                    , span [] [ text <| toString userInfo.user.admin ]
                    ]
                ]

        Nothing ->
            div [] []


renderHours : Maybe (List DailyHours) -> Html Msg
renderHours hrs =
    case hrs of
        Just hours ->
            div []
                [ h2 [] [ text "Hours for the week 2016-12-19 till 2016-12-25 (dates hardcoded)" ]
                , List.map renderHour hours |> ul []
                ]

        Nothing ->
            div [] []


renderHour : DailyHours -> Html Msg
renderHour hour =
    li []
        [ div []
            [ span [] [ text "Hours: " ]
            , span [] [ text <| toString hour.hours ]
            ]
        , div []
            [ span [] [ text "Spent at: " ]
            , span [] [ text hour.spent_at ]
            ]
        , div []
            [ span [] [ text "Billed: " ]
            , span [] [ text <| toString hour.is_billed ]
            ]
        , div []
            [ span [] [ text "Closed: " ]
            , span [] [ text <| toString hour.is_closed ]
            ]
        ]


harvestAuthUrl : String
harvestAuthUrl =
    "https://comsysto.harvestapp.com/oauth2/authorize?response_type=token&immediate=true&approval_prompt=auto&client_id=wvIOerEB7xWVfzrSsge3zw&redirect_uri=http%3A%2F%2Flocalhost%3A8000%2FMain.elm"


main : Program Never Model Msg
main =
    Navigation.program LocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
