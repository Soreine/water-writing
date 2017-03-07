module Main exposing (..)

import Html exposing (Html, Attribute, div, span, input, text, br)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, onBlur, defaultOptions)
import Json.Decode as Decode
import AnimationFrame
import Time exposing (Time)
import Dom
import Task


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- TYPES


{-| Integer coordinates
-}
type alias Coord =
    ( Int, Int )



-- MODEL


type alias Model =
    { location : Coord
    , currentLine : String
    , lines : List String
    , time : Time
    }


model : Model
model =
    { location = ( 0, 0 )
    , currentLine = ""
    , lines = []
    , time = 0
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = ClickAt Coord
    | TypeText String
    | BreakLine
    | InputBlurred
    | Tick Time
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickAt ( x, y ) ->
            ( { model | location = ( x, y ) }
            , focusInput
            )

        TypeText text ->
            ( { model | currentLine = text }
            , Cmd.none
            )

        BreakLine ->
            -- Move current text to previous lines
            ( { model
                | currentLine = ""
                , lines = model.lines ++ [ model.currentLine ]
              }
            , Cmd.none
            )

        InputBlurred ->
            ( model, focusInput )

        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


focusInput : Cmd Msg
focusInput =
    Dom.focus "hidden-input"
        |> Task.attempt (always NoOp)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.times Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        written : List (Html Msg)
        written =
            (model.lines ++ [ model.currentLine ])
                |> List.map text
                |> List.intersperse (br [] [])
    in
        div
            [ id "wall"
            , on "click" (Decode.map ClickAt decodeClickLocation)
            ]
            [ span [ class "writing", stylePosition model.location ]
                (written
                    ++ [ cursor, text <| toString <| Time.inSeconds <| model.time ]
                )
            , hiddenInput model.currentLine
            ]



-- ELEMENTS


cursor : Html Msg
cursor =
    span [ class "cursor" ] []


{-| Invisibile input used to capture text input. Always focused
-}
hiddenInput : String -> Html Msg
hiddenInput val =
    input
        [ id "hidden-input"
        , autofocus True
        , value val
        , onInput TypeText
        , onEnter BreakLine
        , onBlur InputBlurred
        ]
        []



-- UTILS


{-| Style to position left and top to the given coords.
-}
stylePosition : Coord -> Attribute msg
stylePosition ( x, y ) =
    style
        [ ( "left", (toString x) ++ "px" )
        , ( "top", (toString y) ++ "px" )
        ]


decodeClickLocation : Decode.Decoder Coord
decodeClickLocation =
    Decode.map2 (,)
        (Decode.map2 (-)
            (Decode.at [ "pageX" ] Decode.int)
            (Decode.at [ "currentTarget", "offsetLeft" ] Decode.int)
        )
        (Decode.map2 (-)
            (Decode.at [ "pageY" ] Decode.int)
            (Decode.at [ "currentTarget", "offsetTop" ] Decode.int)
        )


{-| Detect Enter input, and prevent default behavior.

http://stackoverflow.com/questions/42390708/elm-conditional-preventdefault-with-contenteditable
-}
onEnter : msg -> Attribute msg
onEnter msg =
    let
        options =
            { defaultOptions | preventDefault = True }

        filterKey code =
            -- Enter
            if code == 13 then
                Decode.succeed msg
            else
                Decode.fail "ignored input"

        decoder =
            Html.Events.keyCode
                |> Decode.andThen filterKey
    in
        Html.Events.onWithOptions "keydown" options decoder
