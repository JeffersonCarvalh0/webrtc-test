port module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, h2, text, video)
import Html.Attributes exposing (autoplay, class, id)
import Json.Decode as Json



---- MAIN ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }



---- PORTS ----


port setMediaStream : MediaStreamConstraints -> Cmd msg


port getMediaError : (Json.Value -> msg) -> Sub msg



---- SUBSCRIPTIONS ----


mediaStreamErrorDecoder : Json.Decoder MediaStreamError
mediaStreamErrorDecoder =
    Json.map3 MediaStreamError
        (Json.field "name" Json.string)
        (Json.field "message" Json.string)
        (Json.field "stack" Json.string)


mediaStreamErrorToMsg : Json.Value -> Msg
mediaStreamErrorToMsg jsonValue =
    case Json.decodeValue mediaStreamErrorDecoder jsonValue of
        Ok error ->
            GotStreamError error

        Err _ ->
            NoOp


subscriptions : Model -> Sub Msg
subscriptions _ =
    getMediaError mediaStreamErrorToMsg



---- UPDATE ----


type Msg
    = NoOp
    | GotStreamError MediaStreamError


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotStreamError error ->
            ( { model | streamError = Just error }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- MODEL ----
-- type MediaStreamErrorType
--     = AbortError String String
--     | NotAllowerError String String
--     | NotFoundError String String
--     | NotReadableError String String
--     | OverconstrainedError String String
--     | SecurityError String String
--     | TypeError String String


type alias MediaStreamError =
    { name : String
    , message : String
    , stack : String
    }


type alias MediaStreamConstraints =
    { audio : Bool
    , video : Bool
    , videoElementId : String
    }


type alias Model =
    { streamError : Maybe MediaStreamError }


init : ( Model, Cmd msg )
init =
    ( { streamError = Nothing }, setMediaStream { audio = True, video = True, videoElementId = "videoEl" } )



---- VIEW ----


view : Model -> Html msg
view model =
    div []
        [ h1 [] [ text "Realtime communication with WebRTC" ]
        , videoView model.streamError
        ]


videoView : Maybe MediaStreamError -> Html msg
videoView streamError =
    case streamError of
        Just error ->
            div [ class "error" ]
                [ h2 [] [ text "We are having trouble with your video stream." ]
                , h2 [] [ text error.name ]
                , h2 [] [ text error.message ]
                ]

        Nothing ->
            video [ id "videoEl", autoplay True ] []
