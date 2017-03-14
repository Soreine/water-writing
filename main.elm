module Main exposing (..)

import AnimationFrame
import Dom
import Html exposing (Html, Attribute, div, span, textarea, text, br)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, onBlur, defaultOptions)
import Json.Decode as Decode
import Task
import Time exposing (Time, second)
import Debug
import Result exposing (Result)
import Constants
import List.Extra


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- CONSTANTS


fadingDelay : Float
fadingDelay =
    8 * second



-- TYPES


{-| A stroke is a character and a timestamp, because it will displayed and will fades with time.
-}
type Stroke
    = Dated Time String



-- MODEL


type alias Model =
    { strokes : List (Stroke)
    , now : Time
    }


model : Model
model =
    { strokes = []
    , now = 0
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = TypeText String
    | InputBlurred
    | Tick Time
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TypeText text ->
            ( model |> (newInput text) |> cleanupStrokes
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


{-| Input received new text value. Update the strokes.
-}
newInput : String -> Model -> Model
newInput newText model =
    let
        newChars =
            toStringList newText

        sizeDiff =
            (List.length newChars - List.length model.strokes)

        -- When using `map2`, the longest list is cropped. We don't want to crop new inputs.
        paddedStrokes =
            model.strokes ++ List.repeat sizeDiff (Dated model.now "")

        updatedStrokes =
            List.map2 (updateStroke model.now) paddedStrokes newChars
    in
        { model | strokes = updatedStrokes }


{-| Update a stroke with a new character. Updates its time too.
-}
updateStroke : Time -> Stroke -> String -> Stroke
updateStroke now stroke string =
    let
        (Dated time str) =
            stroke
    in
        if (str == string) then
            stroke
        else
            Dated now string


{-| Remove all lines of strokes that have completely disappeared.
-}
cleanupStrokes : Model -> Model
cleanupStrokes model =
    let
        isOldLineBreak (Dated time txt) =
            (txt == "\n") && (model.now - time > fadingDelay)

        cleaned =
            -- Assuming strokes are in a chronological sequence.
            model.strokes
                |> List.reverse
                |> List.Extra.takeWhile (not << isOldLineBreak)
                |> List.reverse
    in
        { model | strokes = cleaned }


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
view { now, strokes } =
    let
        written : List (Html Msg)
        written =
            strokes |> List.map (renderStroke now)

        strokesToText : String
        strokesToText =
            strokes |> List.map (\(Dated _ str) -> str) |> String.join ""
    in
        div [ id "wall" ]
            [ span [ class "writing" ]
                (written
                    ++ [ cursor ]
                )
            , hiddenInput strokesToText
            ]



-- ELEMENTS


cursor : Html Msg
cursor =
    span [ class "cursor" ] []


{-| Invisibile input used to capture text input. Always focused
-}
hiddenInput : String -> Html Msg
hiddenInput val =
    textarea
        [ id "hidden-input"
        , autofocus True
        , value val
        , onInput TypeText
        , onKeys
            [ Constants.uparrow
            , Constants.leftarrow
            , Constants.tab
            ]
            (always NoOp)
        , onBlur InputBlurred
        ]
        []


renderStroke : Time -> Stroke -> Html Msg
renderStroke now (Dated time str) =
    let
        age =
            now - time

        opacity =
            -- linear progression
            1 - (progress 0 fadingDelay age)
    in
        if (str == "\n") then
            br [] []
        else
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


{-|
Convert a String to a List of individual characters String
-}
toStringList : String -> List String
toStringList str =
    String.toList str |> List.map String.fromChar


{-| Detect keys input, and prevent default behavior.

Based on http://stackoverflow.com/questions/42390708/elm-conditional-preventdefault-with-contenteditable
-}
onKeys : List Int -> (Int -> msg) -> Attribute msg
onKeys keyCodes onKey =
    let
        options =
            { defaultOptions | preventDefault = True }

        filterKey code =
            -- Enter
            if List.member code keyCodes then
                Decode.succeed (onKey code)
            else
                Decode.fail "ignored input"

        decoder =
            Html.Events.keyCode
                |> Decode.andThen filterKey
    in
        Html.Events.onWithOptions "keydown" options decoder
