module Main exposing (..)
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Model = Int

init : Model
init =
  0


-- UPDATE

type Msg = Increment | Decrement

-- Lowest Value wins
-- these are stops and their base value
-- modify total value when inspecting the entire chain
-- 
-- multiple fights in a row scale, 
-- after 3 fights on a floor their base bump by 1
-- multiple merchants on one floor cancel eachother out, already spent gold, are missing an oppurtunity to gain gold or an idol


type alias Stop = { base: Int, name: String }

elite = Stop 1 "Elite"
rest = Stop 2 "Rest"
merchant = Stop 3 "Merchant"
unknown = Stop 4 "Unknown"
enemy = Stop 5 "Enemy"

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]