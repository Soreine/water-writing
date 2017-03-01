module Main exposing (..)

import Html exposing (Html, Attribute, div, span, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Decode


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { location : Coord
    }


model : Model
model =
    { location = ( 0, 0 ) }



-- Integer coordinates


type alias Coord =
    ( Int, Int )



-- UPDATE


type Msg
    = ClickAt Coord


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickAt ( x, y ) ->
            { model | location = ( x, y ) }



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ id "wall"
        , on "click" (Decode.map ClickAt decodeClickLocation)
        ]
        [ span
            [ class "writing"
            , stylePosition model.location
            ]
            [ cursor, text "oui" ]
        ]



-- ELEMENTS


cursor : Html Msg
cursor =
    span [ class "cursor" ] []



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
