module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, li, p, text, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { contacts : List String
    , contactInput : String
    , selectedContact : String
    }


init : Model
init =
    { contacts = []
    , contactInput = ""
    , selectedContact = ""
    }


type Msg
    = ContactAdded
    | ContactInputUpdated String
    | ContactSelected String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ContactAdded ->
            { model
                | contacts = model.contacts ++ [ model.contactInput ]
                , contactInput = ""
            }

        ContactInputUpdated new ->
            { model | contactInput = new }

        ContactSelected contact ->
            { model | selectedContact = contact }


view : Model -> Html Msg
view model =
    div []
        [ input
            [ placeholder "contact"
            , value model.contactInput
            , onInput ContactInputUpdated
            ]
            []
        , button [ onClick ContactAdded ] [ text "add" ]
        , ul [] (List.map (\l -> li [ onClick (ContactSelected l) ] [ text l ]) model.contacts)
        , p [] [ text ("selected contact: " ++ model.selectedContact) ]
        ]
