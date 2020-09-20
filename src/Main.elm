module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, span, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Set exposing (Set)

---- MODEL ----


type alias Model =
    {
        guesses: Set String,
        phrase: String
    }


init : ( Model, Cmd Msg )
init =
    ( 
        {
            guesses = Set.empty,
            phrase = "Like and subscribe"
        }, 
        Cmd.none 
    )



---- UPDATE ----


type Msg
    = Guess String
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Guess char ->
            ( 
                { model | guesses = Set.insert char model.guesses }, 
                Cmd.none 
            )
        Restart -> init



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        phraseHtml = 
            model.phrase
            |> String.split ""
            |> List.map (\char ->
                if char == " " then
                    " "
                else if Set.member (String.toLower char) model.guesses then
                    char
                else
                    "_"
            )
            |> List.map (\char ->
                span [][text char]
            )
            |> div []
        
        phraseSet = 
            model.phrase 
            |> String.toLower
            |> String.split ""
            |> Set.fromList

        failuresHtml = 
            model.guesses
            |> Set.toList
            |> List.filter (\char ->
                not <| Set.member char phraseSet
            )
            |> List.map (\char ->
                span [][text char]
            )
            |> div []

        buttonsHtml = 
            "abcdefghijklmnopqrstuvwxyz"
            |> String.split ""
            |> List.map (\char ->
                button 
                    [ onClick <| Guess char ] 
                    [text char ]
            )
            |> div []
    in
    div []
        [ 
            phraseHtml,
            buttonsHtml,
            failuresHtml,
            button [onClick Restart ] [ text "Restart" ]
        ]


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
