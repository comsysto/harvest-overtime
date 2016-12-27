module Main exposing (..)

import HarvestAPI exposing (..)
import HarvestTypes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Navigation exposing (Location)


-- Model


type alias Model =
    { location : Location
    , access_token : Maybe String
    , res : Maybe Daily
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
         , res = Nothing
         , hours = Nothing
         , who_am_i = Nothing
         , error = ""
         }
            ! [ Navigation.modifyUrl "#", initCmd ]
        )



-- Update


type Msg
    = LocationChange Location
    | Daily (Result Http.Error HarvestTypes.Daily)
    | Hours (Result Http.Error (List HarvestTypes.DailyHours))
    | WhoAmI (Result Http.Error HarvestTypes.WhoAmI)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LocationChange location ->
            ( { model | location = location }, Cmd.none )

        Daily (Ok daily) ->
            ( { model | res = Just daily }, Cmd.none )

        Daily (Err err) ->
            ( { model | error = toString err }, Cmd.none )

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
                , div [ style [ ( "margin", "1rem" ) ] ] (renderDaily model.res)
                , div [ style [ ( "margin", "1rem" ) ] ] [ text model.error ]
                ]

        Nothing ->
            renderLoginButton


renderDaily : Maybe Daily -> List (Html Msg)
renderDaily daily =
    case daily of
        Just daily ->
            [ h3 [] [ text (toString (.forDay daily)) ] ]
                ++ renderEntries daily
                ++ renderProjectList daily

        Nothing ->
            []


renderEntries : Daily -> List (Html Msg)
renderEntries daily =
    [ h3 [] [ text "Entries" ]
    , ul [] (List.map (\entry -> li [] [ text (.task entry ++ " " ++ toString (.hours entry)) ]) daily.dayEntries)
    ]


renderProjectList : Daily -> List (Html Msg)
renderProjectList daily =
    [ h3 [] [ text "Projects" ]
    , ul [] (List.map (\project -> li [] [ text (.name project) ]) daily.projects)
    ]


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
