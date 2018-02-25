module Main exposing (..)

import Html exposing (Html, div, text, button, h2)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import String exposing (fromChar, fromList, toList, repeat, length)
import List exposing (map, map2, member, head, drop)
import Keypad exposing (..)

-- MODEL
type GameResult
  = Win
  | Loss

type GameState
  = Welcome
  | InProgress
  | Finished GameResult

type alias Model =
  { gameState: GameState
  , word: List Char
  , answer: List Char
  , attempts: Int
  , pressedKeys: List Char
  }

init: (Model, Cmd Msg)
init = (
    { gameState = Welcome
    , word = []
    , answer = []
    , attempts = 0
    , pressedKeys = []
    }, Cmd.none)

-- VIEW
view: Model -> Html Msg
view model =
  case model.gameState of
    Welcome ->
      div []
        [ h2 [] [ text "Hangman Game"]
        , div [] [ button [ onClick StartGame ] [ text "Start new game" ] ]
        ]

    InProgress ->
      div []
        [ h2 [] [ text ("Hangman Game") ]
        , div [] [ text ("Number of attempts: " ++ (toString model.attempts)) ]
        , div [style [("letter-spacing", "3px"), ("font-size", "30px")]] [ text (fromList model.answer) ]
        , div [] [ keypad model.word model.pressedKeys ]
        ]

    Finished result ->
      div []
        [ h2 [] [ text ("Hangman Game") ]
        , div [style [("letter-spacing", "3px")]] [ text (fromList model.answer) ]
        , div [] [ text ("You " ++ toString result)]
        , div [] [ button [ onClick StartGame ] [ text "Start new game" ] ]
        , div [] [ keypad model.word model.pressedKeys ]
        ]

-- UPDATE
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ValidKeyPressed key ->
      let
        newAnswer = combineAnswer key model.word model.answer
      in
        ({model |
          answer = newAnswer,
          pressedKeys = key :: model.pressedKeys,
          gameState = if (newAnswer == model.word) then Finished Win else model.gameState
        }, Cmd.none)

    InvalidKeyPressed key ->
      ({ model |
        attempts = model.attempts - 1,
        pressedKeys = key :: model.pressedKeys,
        gameState = if (model.attempts == 1) then Finished Loss else model.gameState
      }, Cmd.none)

    StartGame ->
      let
        newWord = toList "HASKELL"
      in
        ({ model |
          gameState = InProgress,
          word = newWord,
          answer = maskWord newWord,
          attempts = 3,
          pressedKeys = []
        }, Cmd.none)

combineAnswer: Char -> List Char -> List Char -> List Char
combineAnswer key word answer = map2 (\w a -> if (key == w) then w else a) word answer

maskWord: List Char  -> List Char
maskWord word = map (always '_') word

-- SUBSCRIPTIONS
subscriptions: Model -> Sub Msg
subscriptions model = Sub.none

main: Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
