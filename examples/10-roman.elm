module Main exposing (Model, Msg(..), getInt, main, model, update, view)

import Browser exposing (sandbox)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RomanConvert exposing (decimalToRoman, romanNumeralsToDecimal)


main =
    Browser.sandbox
        { init = model
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
    String.toInt s |> Maybe.withDefault 0


update : Msg -> Model -> Model
update msg updatedModel =
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
            { updatedModel | roman = newRoman, decimal = newDecimal }

        ChangeDecimal new ->
            { updatedModel | decimal = getInt new, roman = decimalToRoman (getInt new) }



-- VIEW


view : Model -> Html Msg
view updatedModel =
    div []
        [ h2 [] [ text "Roman Numeral Converter" ]
        , label [] [ text "Roman numeral" ]
        , input [ placeholder "Enter Roman numeral", onInput ChangeRoman, value updatedModel.roman ] []
        , label [] [ text "Decimal number" ]
        , input [ placeholder "Enter number", onInput ChangeDecimal, value (String.fromInt updatedModel.decimal) ] []
        ]
