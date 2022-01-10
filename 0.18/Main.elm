port module Main exposing (main, update)

import Html.App
import View exposing (view)
import Model exposing (..)
import AppLogic exposing (..)
import Time exposing (Time, second)
import Array exposing (..)

main : Program (Maybe Model)
main = Html.App.programWithFlags
  {
    init = init,
    view = View.view,
    update = update,
    subscriptions = subscriptions
  }

port setStorage : Model -> Cmd msg
port focus : String -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second (\t -> Tick)

init : Maybe Model -> (Model, Cmd Msg)
init savedModel =
  case savedModel of
    Just model ->
      if model.selectedCell < 0
      then model |> selectCell
      else model ! []
    _ ->
      emptyModel |> selectCell

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      model ! []

    Tick ->
      List.foldr updateCells model model.activeCells ! []

    Save ->
      model ! [setStorage model]

    Focus ->
      model ! [focus inputId]

    UserInput keyCode ->
      onUserInput keyCode model

    ToggleMode ->
      toggleMode model ! [] |> save

    ResetModel ->
      selectCell emptyModel |> save

    ResetDown ->
      { model | resetPressed = True } ! []

    ResetUp ->
      { model | resetPressed = False } ! []

    ToggleCheckbox index ->
      toggleCheckbox index model |> save

    SelectCell indexOfNextCellToSelect ->
      model |> selectNewCell indexOfNextCellToSelect

    MouseEnter index ->
      (model |> mouseEnter index) ! []

    MouseLeave index ->
      (model |> mouseLeave index) ! []