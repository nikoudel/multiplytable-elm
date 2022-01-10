port module Model exposing (..)

import Array exposing (..)
import Images exposing (..)

inputId = "userInput"

image =
  {
    multiply = "/images/multiply.png",
    plus = "/images/plus.png",
    multiplyGray = "/images/multiply_gray.png",
    plusGray = "/images/plus_gray.png",
    restart = "/images/restart.png",
    restartPressed = "/images/restart_pressed.png",
    question = "/images/question.png",
    sad = "/images/sad.png",
    happy = "/images/happy.png"
  }

state =
  {
    initial = "initial",
    question = "question",
    sad1 = "sad1",
    sad2 = "sad2",
    happy1 = "happy1",
    happy2 = "happy2",
    done = "done"
  }

key =
  {
    enter = 13,
    esc = 27,
    backspace = 8,
    zero = 48,
    nine = 57,
    zeroNumPad = 96,
    nineNumPad = 105
  }

type Msg
  = NoOp
  | Tick
  | Save
  | Focus
  | UserInput Int
  | ToggleMode
  | ResetModel
  | ResetDown
  | ResetUp
  | ToggleCheckbox Int
  | SelectCell Int
  | MouseEnter Int
  | MouseLeave Int

type alias Model = {
  cells : (Array Cell),
  inputText : String,
  multiplicationMode : Bool,
  modeButtonDisabled : Bool,
  resetPressed : Bool,
  errorCount : Int,
  checkboxes : Array Bool,
  selectedCell : Int,
  hoveredCell : Int,
  activeCells : List Int
}

type alias Cell = {
  index : Int,
  row : Int,
  col : Int,
  header : Bool,
  content : String,
  imageSrc : String,
  imageHidden : Bool,
  cartoon : String,
  state : String,
  locked : Bool,
  selected : Bool
}

emptyModel : Model
emptyModel =
  {
    cells = Array.initialize 81 createCell,
    inputText = "",
    multiplicationMode = True,
    modeButtonDisabled = False,
    resetPressed = False,
    errorCount = 0,
    checkboxes = Array.repeat 8 True,
    selectedCell = -1,
    hoveredCell = -1,
    activeCells = []
  }

createCell : Int -> Cell
createCell index =
  let
    row = index // 9
    col = index % 9
    isHeader = row == 0 || col == 0
    getContent row col =
      if row == 0 && col > 0 then col + 1 |> toString
      else if col == 0 && row > 0 then row + 1 |> toString
      else ""
  in
    {
      index = index,
      row = row,
      col = col,
      header = isHeader,
      content = getContent row col,
      imageSrc = "",
      imageHidden = True,
      cartoon = getImage row col,
      state = state.initial,
      locked = False,
      selected = False
    }