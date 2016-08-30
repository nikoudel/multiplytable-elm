port module AppLogic exposing (..)

import Model exposing (..)
import Array exposing (..)
import String
import Random
import Task
import Debug exposing (log)

updateCells index model =
  case model.cells |> Array.get index of
    Just cell -> onTick cell model
    _ -> model

onTick cell model =
  if cell.state == state.question then
    (model |> updateCell cell.index (\c -> { c | imageHidden = not c.imageHidden }))
  else if cell.state == state.sad2 then
    (model |> updateCell cell.index (\c -> { c | state = state.sad1 }))
  else if cell.state == state.sad1 then
    (model |> updateCell cell.index (\c ->
      { c | state = state.question, imageSrc = image.question, imageHidden = True }))
  else if cell.state == state.happy2 then
    (model |> updateCell cell.index (\c -> { c | state = state.happy1 }))
  else if cell.state == state.happy1 then
    ({ model | activeCells = List.filter (\i -> i /= cell.index) model.activeCells } 
      |> updateCell cell.index (\c ->
        { c | state = state.done, imageSrc = c.cartoon, imageHidden = False }))
  else
    model

getIndex row col =
  row * 9 + col

toggleMode model =
  if model.modeButtonDisabled
  then model
  else { model | multiplicationMode = not model.multiplicationMode }

isCorrectAnswer answer model =
  case model.cells |> Array.get model.selectedCell of
    Just cell ->
      let
        x = cell.col + 1
        y = cell.row + 1
      in
        if model.multiplicationMode
        then Just (answer == x * y)
        else Just (answer == x + y)
    _ ->
      Nothing

getFreeCells model =
  model.cells |> Array.filter (\cell ->
    cell.state == state.initial && not cell.header && not cell.locked)

updateCell index update model =
  case Array.get index model.cells of
    Just cell ->
      { model | cells = model.cells |> Array.set index (update cell) }
    _ ->
      model

onUserInput keyCode model =
  if keyCode == key.enter then
    checkAnswer model |> save
  else if keyCode == key.esc then
    { model | inputText = "" } ! []
  else if keyCode == key.backspace then
    { model | inputText = model.inputText |> String.dropRight 1 } ! []
  else if keyCode >= key.zero && keyCode <= key.nine then
    { model | inputText = model.inputText ++ toString (keyCode - key.zero) } ! []
  else if keyCode >= key.zeroNumPad && keyCode <= key.nineNumPad then
    { model | inputText = model.inputText ++ toString (keyCode - key.zeroNumPad) } ! []
  else
    model ! []

selectCell model =
  let
    maxIndex =
      (getFreeCells model |> Array.length) - 1
  in
    if maxIndex < 0
      then { model | selectedCell = -1 } ! []
      else (model, Random.generate SelectCell (Random.int 0 maxIndex))

checkAnswer model =
  case String.toInt model.inputText of
    Ok n ->
      case isCorrectAnswer n model of
        Just True  -> { model | inputText = "", modeButtonDisabled = True }
          |> updateCell model.selectedCell (\cell ->
            { cell | state = state.happy2, imageSrc = image.happy, imageHidden = False })
          |> deselectCurrentCell |> selectCell
        Just False -> ({ model | inputText = "", errorCount = model.errorCount + 1 }
          |> updateCell model.selectedCell (\cell ->
            { cell | state = state.sad2, imageSrc = image.sad, imageHidden = False })) ! []
        _ -> model ! []
    _ ->
      model ! []

deselectCurrentCell model =
  case model.cells |> Array.get model.selectedCell of
    Just currentlySelectedCell ->
      (model
        |> markHeaders currentlySelectedCell (\cell -> { cell | selected = False })
        |> setSelectedCellIndex -1)
    _ ->
      model

selectNewCell indexOfNextCellToSelect model =
  case model.cells |> Array.get (getRealIndex indexOfNextCellToSelect model) of
    Just newCellToSelect ->
      (model
        |> markHeaders newCellToSelect (\cell -> { cell | selected = True })
        |> updateCell newCellToSelect.index (\cell ->
          { cell | state = state.question, imageSrc = image.question, imageHidden = True })
        |> setSelectedCellIndex newCellToSelect.index
        |> addToActiveCells newCellToSelect.index) ! []
    _ ->
      model ! []

markHeaders cell marker model =
  model
    |> updateCell (getIndex 0 cell.col) marker
    |> updateCell (getIndex cell.row 0) marker

setSelectedCellIndex newSelectedIndex model =
  { model | selectedCell = newSelectedIndex }

addToActiveCells newSelectedIndex model =
  { model | activeCells = newSelectedIndex :: model.activeCells }

getRealIndex cellIndex model =
  case getFreeCells model |> Array.get cellIndex of
    Just cell -> cell.index
    _ -> -1

getCartoon model =
  case model.cells |> Array.get model.selectedCell of
    Just cell ->
      Just { header = getHeader cell model.multiplicationMode, image = cell.cartoon }
    _ ->
      case model.cells |> Array.get model.hoveredCell of
        Just cell ->
          Just { header = "", image = cell.cartoon }
        _ ->
          Nothing

getHeader cell multiplicationMode =
  let
    x = Basics.min cell.row cell.col + 1
    y = Basics.max cell.row cell.col + 1
  in
    if multiplicationMode
    then toString x ++ " * " ++ toString y
    else toString x ++ " + " ++ toString y

toggleCheckbox cbIndex model =
  let
    toggle i flag =
      if i == cbIndex then not flag else flag

    checkboxes = model.checkboxes |> Array.indexedMap toggle

    lock cell =
      let
        rowLocked =
          case checkboxes |> Array.get (cell.row - 1) of
            Just checked -> not checked
            _ -> False
      in
        { cell | locked = rowLocked }

    cells = model.cells |> Array.map lock

    afterToggle =
      if model.selectedCell < 0
      then selectCell
      else \model' -> model' ! []
  in
    { model | checkboxes = checkboxes, cells = cells } |> afterToggle

mouseEnter index model =
  { model | hoveredCell = index }

mouseLeave index model =
  { model | hoveredCell = -1 }

modeImageUrl model =
  if model.multiplicationMode
  then if model.modeButtonDisabled then image.multiplyGray else image.multiply
  else if model.modeButtonDisabled then image.plusGray else image.plus

errorCountClass model =
  if model.errorCount > 0
  then "error"
  else ""

cellClass cell =
  if cell.header
  then if cell.selected
    then if cell.locked then "header selected excluded" else "header selected"
    else if cell.locked then "header excluded" else "header"
  else "cell"
  
resetImageUrl model =
  if model.resetPressed
  then image.restartPressed
  else image.restart

save (model, cmd) =
  let
    saveToLocalStorage = Task.perform (\_ -> NoOp) (\msg -> msg) (Task.succeed Save)
    focus = Task.perform (\_ -> NoOp) (\msg -> msg) (Task.succeed Focus)
  in
    model ! [cmd, saveToLocalStorage, focus]

split : Int -> List (List a) -> List a -> List (List a)
split chunkSize aggregate items =
  if chunkSize <= 0
    then Debug.crash "chunkSize must be > 0"
  else
    case items of
      [] ->
        List.reverse aggregate
      _ ->
        split chunkSize
          ((List.take chunkSize items) :: aggregate)
          (List.drop chunkSize items)