module Main exposing (..)

import Html exposing (Html, div, text, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import String exposing (fromChar, fromList, toList)
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
  , word: String
  , answer: String
  , attempts: Int
  , pressedKeys: List (Char)
  }

init: (Model, Cmd Msg)
init = (
    { gameState = Welcome
    , word = ""
    , answer = ""
    , attempts = 0
    , pressedKeys = []
    }, Cmd.none)

-- VIEW
view: Model -> Html Msg
view model =
  case model.gameState of
    Welcome ->
      div []
        [ div [] [ text "Hangman Game"]
        , div [] [ button [ onClick StartGame ] [ text "Start new game" ] ]
        ]

    InProgress ->
      div []
        [ div [] [ text ("Hangman Game") ]
        , div [] [ text ("Number of attempts: " ++ (toString model.attempts)) ]
        , div [style [("letter-spacing", "3px")]] [ text model.answer ]
        , div [] [ keypad ]
        ]

    Finished result ->
      div []
        [ div [style [("letter-spacing", "3px")]] [ text model.answer ]
        , div [] [ text ("You " ++ toString result)]
        , div [] [ button [ onClick StartGame ] [ text "Start new game" ] ]
        ]

-- UPDATE
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyPressed letter ->
      let
        correctAnswer = member letter (toList model.word)
      in
        if (correctAnswer)
        then
          let
            newAnswer = combineAnswer letter model.word model.answer
          in
            ({model |
              answer = newAnswer,
              pressedKeys = letter :: model.pressedKeys,
              gameState = if (newAnswer == model.word) then Finished Win else model.gameState
            }, Cmd.none)
        else
          ({ model |
            attempts = model.attempts - 1,
            pressedKeys = letter :: model.pressedKeys,
            gameState = if (model.attempts == 1) then Finished Loss else model.gameState
          }, Cmd.none)

    StartGame ->
      let
        newWord = "HASKELL"
      in
        ({ model |
          gameState = InProgress,
          word = newWord,
          answer = maskWord newWord,
          attempts = 3,
          pressedKeys = []
        }, Cmd.none)

combineAnswer: Char -> String -> String -> String
combineAnswer letter word answer = fromList (map2 (\w g -> if (letter == w) then w else g) (toList word) (toList answer))

maskWord: String -> String
maskWord word =
  word
    |> toList
    |> map (\_ -> '_')
    |> fromList
-- alternative: fromList (map (\c -> '_') (toList word))

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
