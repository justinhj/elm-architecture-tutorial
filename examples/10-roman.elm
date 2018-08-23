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
    = ChangeRoman String
    | ChangeDecimal String


getInt : String -> Int
getInt s =
    String.toInt s |> Result.toMaybe |> Maybe.withDefault 0


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeRoman updated ->
            let
                updatedToUpper =
                    String.toUpper updated

                newDecimal =
                    romanNumeralsToDecimal updatedToUpper

                newRoman =
                    decimalToRoman newDecimal
            in
                { model | roman = newRoman, decimal = newDecimal }

        ChangeDecimal new ->
            { model | decimal = getInt new, roman = decimalToRoman (getInt new) }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Roman Numeral Converter" ]
        , label [] [ text "Roman numeral" ]
        , input [ placeholder "Enter Roman numeral", onInput ChangeRoman, value model.roman ] []
        , label [] [ text "Decimal number" ]
        , input [ placeholder "Enter number", onInput ChangeDecimal, value (toString model.decimal) ] []
        ]
