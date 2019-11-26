module Main exposing (..)

import Browser.Events exposing (onKeyPress)
import Json.Decode as Decode
import Browser exposing (Document)
import Html exposing (Html, button, div, p, h1, text, ol, li)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)


-- MAIN

main : Program () Model Msg
main =
  Browser.document { init = init, subscriptions = subscriptions, update = update, view = view }


-- SUBSCRIPTION

subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyPress keyDecoder

keyDecoder : Decode.Decoder Msg
keyDecoder =
  (Decode.field "key" Decode.string)
    |> Decode.map codeToMessage

codeToMessage code =
  case String.uncons code of 
    Just (char, "") -> charToMessage char
    _ -> Noop

charToMessage : Char -> Msg
charToMessage char = 
  case char of 
    '1' -> AddStop elite
    '2' -> AddStop rest
    '3' -> AddStop merchant
    '4' -> AddStop unknown
    '5' -> AddStop enemy
    _ -> Noop

-- MODEL

type alias Stop = { base: Int
  , name: String
  , icon: String
  , hotKey: Char }

type alias Path = List Stop

type alias Act = List Path

type alias Model = { act: Act }

headPath act = 
  List.head act

elite = Stop 5 "Elite" "🦑" '1'
rest = Stop 4 "Rest" "🛏" '2'
merchant = Stop 3 "Merchant" "💰" '3'
unknown = Stop 2 "Unknown" "🎲" '4'
enemy = Stop 1 "Enemy" "🦐" '5'


stops = [ 
  elite
  , rest
  , merchant
  , unknown
  , enemy ]

init _ = (Model [], Cmd.none)


-- UPDATE

type Msg = AddStop Stop
  | Noop

addStop model stop =
  case headPath model of
    Just path -> path ++ [ stop ]
    Nothing -> [ stop ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddStop stop ->
      ({ model | act = [ addStop model.act stop ] }, Cmd.none)
    Noop ->
      (model, Cmd.none)

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
  button [ onClick (AddStop stop) ] [ text (buttonTextFor stop) ]

buttonTextFor stop =
  "#" ++ (String.fromChar stop.hotKey) ++ " " ++ stop.name 

buttons =
  div [] (List.map buttonFor stops)

renderAct : Act -> Html Msg
renderAct act =
  case headPath act of
    Just path -> ol [] (pathList path)
    Nothing -> ol [] [ li [] [ text "Add stops to create a Path"] ]

pathList : Path -> List (Html Msg)
pathList path = 
  List.map stopToItem path

stopToItem stop =
  li [] [ text (stop.icon ++ " " ++ stop.name) ]

solveAct: Act -> String
solveAct act =
  case headPath act of
    Just path -> solvePath path
    Nothing -> "N/A"

solvePath path = 
  (List.map .base path) 
  |> List.foldl (\a b -> a + b) 0 
  |> String.fromInt

view : Model -> Document Msg
view model =
  Document "Solve The Spire" [ (mainDiv model) ]

mainDiv model = 
  div baseCss
    [
    h1 [] [ text "Solve The Spire" ]
    , p [] [ text description ]
    , p [] [ text (solveAct model.act) ]
    , buttons
    , renderAct model.act
    ]