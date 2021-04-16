module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, li, p, text, ul)
import Html.Attributes exposing (disabled, placeholder, value)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { contacts : List String
    , contactInput : String
    , selectedContact : String
    , messages : List ( String, String )
    , messageInput : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { contacts = []
      , contactInput = ""
      , selectedContact = ""
      , messages = []
      , messageInput = ""
      }
    , Cmd.none
    )


type Msg
    = ContactAdded
    | ContactInputUpdated String
    | ContactSelected String
    | MessageInputUpdated String
    | MessageSent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ContactAdded ->
            ( { model
                | contacts = model.contacts ++ [ model.contactInput ]
                , contactInput = ""
              }
            , Cmd.none
            )

        ContactInputUpdated new ->
            ( { model | contactInput = new }
            , Cmd.none
            )

        ContactSelected contact ->
            ( { model | selectedContact = contact }
            , Cmd.none
            )

        MessageInputUpdated new ->
            ( { model | messageInput = new }
            , Cmd.none
            )

        MessageSent ->
            ( { model
                | messages = model.messages ++ [ ( model.selectedContact, model.messageInput ) ]
                , messageInput = ""
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ input
            [ placeholder "contact"
            , value model.contactInput
            , onInput ContactInputUpdated
            ]
            []
        , button
            [ disabled (String.isEmpty model.contactInput)
            , onClick ContactAdded
            ]
            [ text "add" ]
        , ul [] (List.map (\l -> li [ onClick (ContactSelected l) ] [ text l ]) model.contacts)
        , p [] [ text ("talking to " ++ model.selectedContact) ]
        , ul []
            (model.messages
                |> List.filter (\m -> Tuple.first m == model.selectedContact)
                |> List.map (\l -> li [] [ text (Tuple.second l) ])
            )
        , input [ placeholder "message", value model.messageInput, onInput MessageInputUpdated ] []
        , button
            [ disabled (String.isEmpty model.selectedContact || String.isEmpty model.messageInput)
            , onClick MessageSent
            ]
            [ text "send" ]
        ]
