module Tests exposing (..)

import ElmTest exposing (..)
import Model exposing (..)
import Main exposing (update)
import Array exposing (..)
import String

main : Program Never
main =
    runSuite tests

tests = suite "All tests"
  [
    test "ToggleMode toggles mode"
      <| assertEqual False
      <| (toggleMode emptyModel).multiplicationMode,

    test "ResetDown changes button color"
      <| assertEqual True
      <| (pushReset True).resetPressed,

    test "ResetUp reverts button color"
      <| assertEqual False
      <| (pushReset False).resetPressed,

    test "ToggleCheckbox unchecks first checkbox" 
      <| assertEqual (Just False)
      <| List.head <| Array.toList <| (toggleCheckbox 0 emptyModel).checkboxes,

    test "ToggleCheckbox checks first checkbox"
      <| assertEqual (Just True)
      <| List.head <| Array.toList <| (emptyModel |> toggleCheckbox 0 |> toggleCheckbox 0).checkboxes,

    test "ToggleCheckbox locks cells"
      <| assertEqual "10 11 12 13 14 15 16 17 18 "
      <| (emptyModel |> toggleCheckbox 0 |> toggleCheckbox 1 |> toggleCheckbox 1 |> getLockedCells),

    test "ResetModel resets model"
      <| assertEqual emptyModel
      <| (emptyModel |> toggleMode |> resetModel),

    test "Invalid user input gets blocked"
      <| assertEqual "22"
      <| (emptyModel |> enterKey 90 |> enterKey 50 |> enterKey 98).inputText, -- 90: z, 50: 2, 98: NumPad 2

    test "Wrong answer"
      <| assertEqual ("visible active " ++ state.sad2 ++ " " ++ image.sad)
      <| (emptyModel |> selectCell 0 |> enterKey 51 |> enterKey 13 |> getState 10),

    test "Wrong answer updates error counter"
      <| assertEqual 1
      <| (emptyModel |> selectCell 0 |> enterKey 51 |> enterKey 13).errorCount,

    test "Correct answer"
      <| assertEqual ("visible active " ++ state.happy2 ++ " " ++ image.happy)
      <| (emptyModel |> selectCell 0 |> enterKey 52 |> enterKey 13 |> getState 10),

    test "Correct answer disables mode changer"
      <| assertEqual True
      <| (emptyModel |> selectCell 0 |> enterKey 52 |> enterKey 13).modeButtonDisabled,

    test "Cell count"
      <| assertEqual 81
      <| Array.length emptyModel.cells,

    test "Cell array has correct structure"
      <| assertEqual (Just (1,1))
      <| (emptyModel.cells |> Array.map (\c -> (c.row, c.col)) |> Array.get 10),

    test "SelectCell sets a real index to model.selectedCell"
      <| assertEqual 10
      <| (selectCell 0 emptyModel).selectedCell,

    test "SelectCell updates class of header cells"
      <| assertEqual "01 10 "
      <| (selectCell 0 emptyModel |> getActiveHeaderCells),

    test "SelectCell clears 'initial' state from a new cell"
      <| assertEqual "11 "
      <| (emptyModel |> selectCell 0 |> getChangedCells),

    test "SelectCell sets the question image"
      <| assertEqual image.question
      <| (emptyModel |> selectCell 0 |> getImageSrc 10),

    test "SelectCell activates the cell"
      <| assertEqual ("hidden active " ++ state.question ++ " " ++ image.question)
      <| (emptyModel |> selectCell 0 |> getState 10),

    test "First Tick makes image visible"
      <| assertEqual ("visible active " ++ state.question ++ " " ++ image.question)
      <| (emptyModel |> selectCell 0 |> tick |> getSelectedCellState),

    test "Second Tick hides image"
      <| assertEqual ("hidden active " ++ state.question ++ " " ++ image.question)
      <| (emptyModel |> selectCell 0 |> tick |> tick |> getSelectedCellState),

    test "Tick changes state from 'sad2' to 'sad1'"
      <| assertEqual ("visible active " ++ state.sad1 ++ " " ++ image.sad)
      <| (emptyModel |> selectCell 0 |> enterKey 51 |> enterKey 13 |> tick |> getState 10),

    test "Tick changes state from 'sad2' to 'question'"
      <| assertEqual ("hidden active " ++ state.question ++ " " ++ image.question)
      <| (emptyModel |> selectCell 0 |> enterKey 51 |> enterKey 13 |> tick |> tick |> getState 10),

    test "Tick changes state from happy2 to 'happy1'"
      <| assertEqual ("visible active " ++ state.happy1 ++ " " ++ image.happy)
      <| (emptyModel |> selectCell 0 |> enterKey 52 |> enterKey 13 |> tick |> getState 10),

    test "Tick changes state from 'happy1' to 'Alex-2-icon.png'"
      <| assertEqual ("visible " ++ state.done ++ " /images/Penguins-icon.png")
      <| (emptyModel |> selectCell 0 |> enterKey 52 |> enterKey 13 |> tick |> tick |> getState 10)
  ]

getModel (model, cmd) = model

toggleMode model =
  update ToggleMode model |> getModel

resetModel model =
  update ResetModel model |> getModel

pushReset isDown =
  if isDown then
    update ResetDown emptyModel |> getModel
  else
    update ResetDown emptyModel
      |> getModel
      |> update ResetUp
      |> getModel

toggleCheckbox index model =
  update (ToggleCheckbox index) model |> getModel

selectCell index model =
  update (SelectCell index) model |> getModel

getActiveHeaderCells model =
  model.cells
    |> Array.filter (\cell -> cell.selected == True && cell.header == True)
    |> Array.map (\cell -> toString cell.row ++ (toString cell.col) ++ " ")
    |> Array.foldr (++) ""

getChangedCells model =
  model.cells
    |> Array.filter (\cell -> not cell.header && not (cell.state == state.initial))
    |> Array.map (\cell -> toString cell.row ++ (toString cell.col) ++ " ")
    |> Array.foldr (++) ""

getImageSrc index model =
  case Array.get index model.cells of
    Just cell -> cell.imageSrc
    _ -> ""

isImageHidden index model =
  case Array.get index model.cells of
    Just cell -> Just cell.imageHidden
    _ -> Nothing

getState index model =
  case Array.get index model.cells of
    Just cell -> (if cell.imageHidden then "hidden" else "visible")
      ++ (if List.any (\i -> i == cell.index) model.activeCells then " active" else "")
      ++ " " ++ cell.state ++ " " ++ cell.imageSrc
    _ -> ""

getSelectedCellState model =
  getState model.selectedCell model

tick model =
  update Tick model |> getModel

enterKey keyCode model =
  update (UserInput keyCode) model |> getModel

getLockedCells model =
  model.cells
    |> Array.filter (\cell -> cell.locked)
    |> Array.map (\cell -> toString cell.row ++ (toString cell.col) ++ " ")
    |> Array.foldr (++) ""