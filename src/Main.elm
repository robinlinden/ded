module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { contacts : List String, contactInput : String }


init : Model
init =
    { contacts = [], contactInput = "" }


type Msg
    = ContactInputChange String
    | AddContact


update : Msg -> Model -> Model
update msg model =
    case msg of
        ContactInputChange inputChange ->
            { model | contactInput = inputChange }

        AddContact ->
            { model | contacts = model.contacts ++ [ model.contactInput ], contactInput = "" }


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "contact", value model.contactInput, onInput ContactInputChange ] []
        , button [ onClick AddContact ] [ text "Add" ]
        , ul [] (List.map (\l -> li [] [ text l ]) model.contacts)
        ]
