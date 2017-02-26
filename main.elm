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
    { location : ( Int, Int )
    }


model : Model
model =
    { location = ( 0, 0 ) }



-- UPDATE


type Msg
    = ClickAt ( Int, Int )


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickAt ( x, y ) ->
            { model | location = ( x, y ) }



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ on "click" (Decode.map ClickAt decodeClickLocation)
        , id "wall"
        ]
        [ span
            [ class "writing"
            , style
                [ ( "left", (model.location |> Tuple.first |> toString) ++ "px" )
                , ( "top", (model.location |> Tuple.second |> toString) ++ "px" )
                ]
            ]
            [ text <| toString model.location ]
        ]



-- UTILS


decodeClickLocation : Decode.Decoder ( Int, Int )
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
