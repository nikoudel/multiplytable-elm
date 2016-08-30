port module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Array exposing (..)
import Model exposing (..)
import AppLogic exposing (..)

view : Model -> Html Msg
view model =
  div []
    [
      div [id "main"]
        [
          div [id "grid"] [viewTable model.cells],
          div [id "input"] [viewInput model]
        ],
      div [id "right"]
        [
          div [id "buttons"]
            [
              img [src (modeImageUrl model), onClick ToggleMode] [],
              img [src (resetImageUrl model),
                onClick ResetModel,
                onMouseDown ResetDown,
                onMouseUp ResetUp,
                onDragEnd ResetUp] [],
              h1 [class (errorCountClass model)] [text (model.errorCount |> toString)],
              div [] (List.indexedMap checkboxToHtml (Array.toList model.checkboxes))
            ],
          div [id "cartoon"] (viewCartoon model)
        ]
    ]

viewTable : Array Cell -> Html Msg
viewTable cells =
  let
    rows =
      split 9 [] (Array.toList cells)

    rowToHtml row =
      tr [] (List.map cellToHtml row)

    cellToHtml cell =
      td [class (cellClass cell)]
        [
          text cell.content,
          img [
            class (if cell.imageHidden then "hidden" else ""),
            src cell.imageSrc,
            onMouseEnter (MouseEnter cell.index),
            onMouseLeave (MouseLeave cell.index)] []
        ]
  in
    table [] [ tbody [] (List.map rowToHtml rows) ]

viewInput : Model -> Html Msg
viewInput model =
  input
    [
      id inputId,
      type' "text",
      autofocus True,
      onBlur Focus,
      disabled (model.selectedCell < 0),
      value model.inputText,
      onKeyDown
    ] []

checkboxToHtml : Int -> Bool -> Html Msg
checkboxToHtml i isChecked =
  div []
    [
      label []
        [
          text (i + 2 |> toString),
          text " ",
          input
            [
              type' "checkbox",
              checked isChecked,
              onClick (ToggleCheckbox i)
            ] []
        ]
    ]

onDragEnd : Msg -> Attribute Msg
onDragEnd msg =
  on "dragend" (Json.succeed msg)

onKeyDown : Attribute Msg
onKeyDown =
  let
    -- Prevent default action in order to allow input validation.
    options = { stopPropagation = False, preventDefault = True }
    tagger code = UserInput code
  in
    onWithOptions "keydown" options (Json.map tagger keyCode)

viewCartoon : Model -> List (Html Msg)
viewCartoon model =
  case getCartoon model of
    Just cartoon ->
      [
        h1 [] [text cartoon.header],
        img [src cartoon.image] []
      ]
    _ ->
      []