module Main exposing (..)

import AnimationFrame
import Dom
import Html exposing (Html, Attribute, div, span, input, text, br)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, onBlur, defaultOptions)
import Json.Decode as Decode
import Task
import Time exposing (Time, second)
import Debug
import Result exposing (Result)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- TYPES


{-| Characters, or line breaks. All the things that make up the text written by the user, that needs to be displayed in its space.
-}
type Stroke
    = Dated Time String
    | LineBreak



-- MODEL


type alias Model =
    { strokes : List (Stroke)
    , {-
         The character being written in the hidden input. We need to keep it there
         until the next character is typed, to support dead keys
         https://en.wikipedia.org/wiki/Dead_key
      -}
      inputText : String
    , now : Time
    }


model : Model
model =
    { strokes = []
    , inputText = ""
    , now = 0
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = TypeText String
    | BreakLine
    | InputBlurred
    | Tick Time
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TypeText text ->
            ( newInput model text
            , Cmd.none
            )

        BreakLine ->
            ( let
                model2 =
                    (finishStroke model)
              in
                { model2
                    | strokes = model.strokes ++ [ LineBreak ]
                }
            , Cmd.none
            )

        InputBlurred ->
            ( model, focusInput )

        Tick newTime ->
            ( { model | now = newTime }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


{-| Input received new text value.
-}
newInput : Model -> String -> Model
newInput model newText =
    let
        oldText =
            model.inputText

        oldLength =
            String.length oldText

        newLength =
            String.length newText
    in
        if (newLength == 0) then
            model
            -- Do not alter the current state. This has the effect to disallow erasing the input completely
        else if (newLength == 1 && oldLength == 1) then
            -- Allow dead keys to modify current char
            { model | inputText = newText }
        else if (not <| String.startsWith oldText newText) then
            model
            -- Disallow erasing or modifying the input
        else
            let
                lastChar =
                    newText |> String.reverse |> String.left 1

                rest =
                    String.slice 0 -1 newText

                restStrokes =
                    Dated model.now rest
            in
                { model
                    | inputText = lastChar
                    , strokes = model.strokes ++ [ restStrokes ]
                }


{-| Clear and convert current input, if any, to strokes
-}
finishStroke : Model -> Model
finishStroke model =
    case model.inputText of
        "" ->
            model

        str ->
            { model
                | inputText = ""
                , strokes = model.strokes ++ [ Dated model.now str ]
            }


focusInput : Cmd Msg
focusInput =
    Dom.focus "hidden-input"
        |> Task.attempt (logError >> always NoOp)


logError : Result a b -> Result a b
logError result =
    case result of
        Err error ->
            let
                _ =
                    Debug.log (toString error)
            in
                result

        Ok _ ->
            result



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.times Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        allStrokes =
            (model.strokes ++ [ Dated model.now model.inputText ])

        written : List (Html Msg)
        written =
            allStrokes |> List.map (renderStroke model.now)
    in
        div [ id "wall" ]
            [ span [ class "writing" ]
                (written
                    ++ [ cursor ]
                )
            , hiddenInput model.inputText
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


renderStroke : Time -> Stroke -> Html Msg
renderStroke now stroke =
    case stroke of
        LineBreak ->
            br [] []

        Dated time str ->
            let
                age =
                    now - time

                fadingDelay =
                    8 * second

                opacity =
                    -- linear progression
                    1 - (progress 0 fadingDelay age)
            in
                span [ styleOpacity opacity ] [ text str ]



-- UTILS


{-| Returns the progress (within [0, 1]) of a value, relative to a min and max. (Matthieu, better naming for this value?).
-}
progress : Float -> Float -> Float -> Float
progress min max x =
    let
        normX =
            clamp min max x
    in
        (normX - min) / (max - min)


styleOpacity : number -> Attribute msg
styleOpacity opacity =
    style [ ( "opacity", toString opacity ) ]


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
