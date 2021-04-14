module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, li, p, text, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { contacts : List String, contactInput : String, selectedContact : String }


init : Model
init =
    { contacts = [], contactInput = "", selectedContact = "" }


type Msg
    = ContactInputChange String
    | AddContact
    | SelectContact String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ContactInputChange inputChange ->
            { model | contactInput = inputChange }

        AddContact ->
            { model | contacts = model.contacts ++ [ model.contactInput ], contactInput = "" }

        SelectContact contact ->
            { model | selectedContact = contact }


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "contact", value model.contactInput, onInput ContactInputChange ] []
        , button [ onClick AddContact ] [ text "Add" ]
        , ul [] (List.map (\l -> li [ onClick (SelectContact l) ] [ text l ]) model.contacts)
        , p [] [ text ("Selected contact: " ++ model.selectedContact) ]
        ]
