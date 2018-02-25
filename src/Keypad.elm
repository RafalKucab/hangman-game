module Keypad exposing (keypad, Msg(..))

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import List exposing (map, take, drop, member)
import String exposing (fromChar)

type Msg
  = StartGame
  | ValidKeyPressed Char
  | InvalidKeyPressed Char

keys: List Char
keys = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']

keypad: List Char -> List Char -> Html Msg
keypad validKeys pressedKeys = div [] (
  keys
    |> map (\c -> buildKey c validKeys pressedKeys)
    |> split 5
    |> map (\row -> div [] row)
  )

buildKey: Char -> List Char -> List Char -> Html Msg
buildKey c validKeys pressedKeys = selectStyles c pressedKeys |> selectKey c validKeys

selectKey: Char -> List Char -> (List (String, String) -> Html Msg)
selectKey c validKeys =
  if (member c validKeys) then key c (ValidKeyPressed c) else key c (InvalidKeyPressed c)

selectStyles: Char -> List Char -> List (String, String)
selectStyles c pressedKeys =
  if (member c pressedKeys) then inactiveStyles else activeStyles

split: Int -> List a -> List (List a)
split n list =
  case take n list of
    [] -> []
    head -> head :: split n (drop n list)

key: Char -> msg -> List (String, String) -> Html msg
key char action styles = div
  [ style styles
  , onClick action
  ] [ text (fromChar char) ]

activeStyles: List (String, String)
activeStyles =
  [ ("display", "inline-block")
  , ("margin", "7px 7px 7px 7px")
  , ("padding", "10px 20px 10px 20px")
  , ("font-family", "'Ubuntu Mono', monospace")
  , ("font-size", "26px")
  , ("color", "black")
  , ("border-radius", "17px")
  , ("border", "solid black 3px")
  , ("cursor", "pointer")
  ]

inactiveStyles: List (String, String)
inactiveStyles =
  [ ("display", "inline-block")
  , ("margin", "7px 7px 7px 7px")
  , ("padding", "10px 20px 10px 20px")
  , ("font-family", "'Ubuntu Mono', monospace")
  , ("font-size", "26px")
  , ("color", "#e1e3e8")
  , ("border-radius", "17px")
  , ("border", "solid #e1e3e8 3px")
  , ("cursor", "not-allowed")
  ]
