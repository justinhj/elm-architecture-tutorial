module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RomanConvert exposing (decimalToRoman, romanNumeralsToDecimal)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { decimal : Int
    , roman : String
    }


model : Model
model =
    { decimal = 1
    , roman = "I"
    }



-- UPDATE


type Msg
    = RomanToDecimal
    | DecimalToRoman
    | ChangeRoman String
    | ChangeDecimal String


getInt : String -> Int
getInt s =
    String.toInt s |> Result.toMaybe |> Maybe.withDefault 0


update : Msg -> Model -> Model
update msg model =
    case msg of
        RomanToDecimal ->
            { model | decimal = romanNumeralsToDecimal model.roman }

        DecimalToRoman ->
            { model | roman = decimalToRoman model.decimal }

        ChangeRoman new ->
            { model | roman = new }

        ChangeDecimal new ->
            { model | decimal = getInt new }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Roman Numeral Converter" ]
        , button [ onClick RomanToDecimal ] [ text "RomanToDecimal" ]
        , button [ onClick DecimalToRoman ] [ text "DecimalToRoman" ]
        , div [] [ text (toString model.decimal) ]
        , div [] [ text model.roman ]
        , input [ placeholder "Enter Roman numeral", onInput ChangeRoman ] []
        , input [ placeholder "Enter number", onInput ChangeDecimal ] []
        ]



--        div [] [ input [placeholder = "Enter roman  numeral", onChange ChangeRoman ] ]
--        , div [] [ input [placeholder = "Enter decimal", onChange ChangeDecimal ] ]
