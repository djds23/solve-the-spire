module Main exposing (..)
import Browser
import Html exposing (Html, button, div, p, h1, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Stop = { base: Int, name: String }

type alias Path = List Stop

type alias Act = List Path

type alias Model = { act: Act, keypress: Bool }

headPath act = 
  List.head act

elite = Stop 5 "Elite"
rest = Stop 4 "Rest"
merchant = Stop 3 "Merchant"
unknown = Stop 2 "Unknown"
enemy = Stop 1 "Enemy"

stops = [ 
  elite
  , rest
  , merchant
  , unknown
  , enemy ]

init = { act = [], keypress = False }


-- UPDATE

type Msg = AddStop Stop

addStop model stop =
  case headPath model of
    Just path -> path ++ [ stop ]
    Nothing -> [ stop ]

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddStop stop ->
      { model | act = [ addStop model.act stop ] }


-- VIEW

description = """
Here we attempt suggest the best path in a single "Slay The Spire" run.
This heuristic is very primative. It does not take into account important properties of a run, like health or particular items.
-- these are stops and their base value
-- modify total value when inspecting the entire chain
-- multiple fights in a row scale, 
-- after 3 fights on a floor their base bump by 1
-- multiple merchants on one floor cancel eachother out, already spent gold, are missing an oppurtunity to gain gold or an idol
"""

baseCss = [ style "max-width" "38rem"
  , style "padding" "2rem"
  , style "margin" "auto" ]

buttonFor stop =
  button [ onClick (AddStop stop) ] [ text stop.name ]

buttons =
  div [] (List.map buttonFor stops)

solveAct: Act -> String
solveAct act =
  case headPath act of
    Just path -> solvePath path
    Nothing -> "N/A"

stopValue stop = stop.base

solvePath path = 
  (List.map stopValue path) 
  |> List.foldl (\a b -> a + b) 0 
  |> String.fromInt

view : Model -> Html Msg
view model =
  div baseCss
    [
    h1 [] [ text "Solve The Spire" ]
    , p [] [ text description ]
    , p [] [ text (solveAct model.act) ]
    , buttons
    ]