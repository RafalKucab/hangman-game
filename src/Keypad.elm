module Keypad exposing (keypad, Msg(..))

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import List exposing (map)
import String exposing (fromChar)

type Msg
  = StartGame
  | KeyPressed Char

keypad: Html Msg
keypad =
  div []
    [ row ['A', 'B', 'C', 'D', 'E']
    , row ['F', 'G', 'H', 'I', 'J']
    , row ['K', 'L', 'M', 'N', 'O']
    , row ['P', 'Q', 'R', 'S', 'T']
    , row ['U', 'V', 'W', 'X', 'Y']
    , row ['Z']
    ]

row: List (Char) -> Html Msg
row elements =
  elements
    |> map (\c -> key c)
    |> div []

  -- alternative: div [] (map (\c -> l c) ls)

key: Char -> Html Msg
key char = div
  [ style
    [ ("display", "inline-block")
    , ("margin", "7px 7px 7px 7px")
    , ("padding", "10px 20px 10px 20px")
    , ("font-family", "'Ubuntu Mono', monospace")
    , ("font-size", "26px")
    , ("border-radius", "17px")
    , ("border", "solid #050005 3px")
    , ("cursor", "pointer")
    ]
  , onClick (KeyPressed char)
  ] [ text (fromChar char) ]


{-
Some styling
, ("margin", "7px 7px 7px 7px")
, ("padding", "10px 20px 10px 20px")
, ("font-family", "'Ubuntu Mono', monospace")
, ("font-size", "26px")
, ("border-radius", "17px")
, ("border", "solid #050005 3px")
, ("cursor", "pointer")
-}
