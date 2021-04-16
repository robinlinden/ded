port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, li, p, text, ul)
import Html.Attributes exposing (disabled, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E


port updateStorage : E.Value -> Cmd msg


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , update = updateAndStore
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


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( case D.decodeValue decoder flags of
        Ok model ->
            model

        Err e ->
            { contacts = []
            , contactInput = ""
            , selectedContact = ""
            , messages = []
            , messageInput = ""
            }
    , Cmd.none
    )


encode : Model -> E.Value
encode model =
    E.object
        [ ( "contacts", E.list E.string model.contacts )
        , ( "contactInput", E.string model.contactInput )
        , ( "selectedContact", E.string model.selectedContact )
        , ( "messages", E.list (tuple2Encoder E.string E.string) model.messages )
        , ( "messageInput", E.string model.messageInput )
        ]


tuple2Encoder : (a -> E.Value) -> (b -> E.Value) -> ( a, b ) -> E.Value
tuple2Encoder aEnc bEnc ( a, b ) =
    E.list identity [ aEnc a, bEnc b ]


decoder : D.Decoder Model
decoder =
    D.map5 Model
        (D.field "contacts" (D.list D.string))
        (D.field "contactInput" D.string)
        (D.field "selectedContact" D.string)
        (D.field "messages" (D.list (tuple2Decoder D.string D.string)))
        (D.field "messageInput" D.string)


tuple2Decoder : D.Decoder a -> D.Decoder b -> D.Decoder ( a, b )
tuple2Decoder a b =
    D.index 0 a |> D.andThen (\aVal -> D.index 1 b |> D.andThen (\bVal -> D.succeed ( aVal, bVal )))


type Msg
    = ContactAdded
    | ContactInputUpdated String
    | ContactSelected String
    | MessageInputUpdated String
    | MessageSent


updateAndStore : Msg -> Model -> ( Model, Cmd Msg )
updateAndStore msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , Cmd.batch [ updateStorage (encode newModel), cmds ]
    )


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
