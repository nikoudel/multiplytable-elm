port module AppLogic exposing (..)

import Array exposing (..)
import Model exposing (..)
import Random
import String


port setStorage : Model -> Cmd msg


port focus : String -> Cmd msg


updateCells : Int -> Model -> Model
updateCells index model =
    case model.cells |> Array.get index of
        Just cell ->
            onTick cell model

        _ ->
            model


onTick : Cell -> Model -> Model
onTick cell model =
    if cell.state == state.question then
        model |> updateCell cell.index (\c -> { c | imageHidden = not c.imageHidden })

    else if cell.state == state.sad2 then
        model |> updateCell cell.index (\c -> { c | state = state.sad1 })

    else if cell.state == state.sad1 then
        model
            |> updateCell cell.index
                (\c ->
                    { c | state = state.question, imageSrc = image.question, imageHidden = True }
                )

    else if cell.state == state.happy2 then
        model |> updateCell cell.index (\c -> { c | state = state.happy1 })

    else if cell.state == state.happy1 then
        { model | activeCells = List.filter (\i -> i /= cell.index) model.activeCells }
            |> updateCell cell.index
                (\c ->
                    { c | state = state.done, imageSrc = image.happy, imageHidden = False }
                )

    else
        model


getIndex : Int -> Int -> Int
getIndex row col =
    row * 9 + col


toggleMode : Model -> Model
toggleMode model =
    if model.modeButtonDisabled then
        model

    else
        { model | multiplicationMode = not model.multiplicationMode }


isCorrectAnswer : Int -> Model -> Maybe Bool
isCorrectAnswer answer model =
    case model.cells |> Array.get model.selectedCell of
        Just cell ->
            let
                x =
                    cell.col + 1

                y =
                    cell.row + 1
            in
            if model.multiplicationMode then
                Just (answer == x * y)

            else
                Just (answer == x + y)

        _ ->
            Nothing


getFreeCells : Model -> Array Cell
getFreeCells model =
    model.cells
        |> Array.filter
            (\cell ->
                cell.state == state.initial && not cell.header && not cell.locked
            )


updateCell : Int -> (Cell -> Cell) -> Model -> Model
updateCell index update model =
    case Array.get index model.cells of
        Just cell ->
            { model | cells = model.cells |> Array.set index (update cell) }

        _ ->
            model


onUserInput : Int -> Model -> ( Model, Cmd Msg )
onUserInput keyCode model =
    if keyCode == key.enter then
        checkAnswer model |> save

    else if keyCode == key.esc then
        ( { model | inputText = "" }, Cmd.none )

    else if keyCode == key.backspace then
        ( { model | inputText = model.inputText |> String.dropRight 1 }, Cmd.none )

    else if keyCode >= key.zero && keyCode <= key.nine then
        ( { model | inputText = model.inputText ++ String.fromInt (keyCode - key.zero) }, Cmd.none )

    else if keyCode >= key.zeroNumPad && keyCode <= key.nineNumPad then
        ( { model | inputText = model.inputText ++ String.fromInt (keyCode - key.zeroNumPad) }, Cmd.none )

    else
        ( model, Cmd.none )


selectCell : Model -> ( Model, Cmd Msg )
selectCell model =
    let
        maxIndex =
            (getFreeCells model |> Array.length) - 1
    in
    if maxIndex < 0 then
        ( { model | selectedCell = -1 }, focus theInputId )

    else
        ( model, Cmd.batch [ Random.generate SelectCell (Random.int 0 maxIndex), focus theInputId ] )


checkAnswer : Model -> ( Model, Cmd Msg )
checkAnswer model =
    case String.toInt model.inputText of
        Just n ->
            case isCorrectAnswer n model of
                Just True ->
                    { model | inputText = "", modeButtonDisabled = True }
                        |> updateCell model.selectedCell
                            (\cell ->
                                { cell | state = state.happy2, imageSrc = image.happy, imageHidden = False }
                            )
                        |> deselectCurrentCell
                        |> selectCell

                Just False ->
                    ( { model | inputText = "", errorCount = model.errorCount + 1 }
                        |> updateCell model.selectedCell
                            (\cell ->
                                { cell | state = state.sad2, imageSrc = image.sad, imageHidden = False }
                            )
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


deselectCurrentCell : Model -> Model
deselectCurrentCell model =
    case model.cells |> Array.get model.selectedCell of
        Just currentlySelectedCell ->
            model
                |> markHeaders currentlySelectedCell (\cell -> { cell | selected = False })
                |> setSelectedCellIndex -1

        _ ->
            model


selectNewCell : Int -> Model -> ( Model, Cmd msg )
selectNewCell indexOfNextCellToSelect model =
    case model.cells |> Array.get (getRealIndex indexOfNextCellToSelect model) of
        Just newCellToSelect ->
            ( model
                |> markHeaders newCellToSelect (\cell -> { cell | selected = True })
                |> updateCell newCellToSelect.index
                    (\cell ->
                        { cell | state = state.question, imageSrc = image.question, imageHidden = True }
                    )
                |> setSelectedCellIndex newCellToSelect.index
                |> addToActiveCells newCellToSelect.index
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


markHeaders : Cell -> (Cell -> Cell) -> Model -> Model
markHeaders cell marker model =
    model
        |> updateCell (getIndex 0 cell.col) marker
        |> updateCell (getIndex cell.row 0) marker


setSelectedCellIndex : Int -> Model -> Model
setSelectedCellIndex newSelectedIndex model =
    { model | selectedCell = newSelectedIndex }


addToActiveCells : Int -> Model -> Model
addToActiveCells newSelectedIndex model =
    { model | activeCells = newSelectedIndex :: model.activeCells }


getRealIndex : Int -> Model -> Int
getRealIndex cellIndex model =
    case getFreeCells model |> Array.get cellIndex of
        Just cell ->
            cell.index

        _ ->
            -1


getHeader : Cell -> Bool -> String
getHeader cell multiplicationMode =
    let
        x =
            Basics.min cell.row cell.col + 1

        y =
            Basics.max cell.row cell.col + 1
    in
    if multiplicationMode then
        String.fromInt x ++ " * " ++ String.fromInt y

    else
        String.fromInt x ++ " + " ++ String.fromInt y


toggleCheckbox : Int -> Model -> ( Model, Cmd Msg )
toggleCheckbox cbIndex model =
    let
        toggle i flag =
            if i == cbIndex then
                not flag

            else
                flag

        checkboxes =
            model.checkboxes |> Array.indexedMap toggle

        lock cell =
            let
                rowLocked =
                    case checkboxes |> Array.get (cell.row - 1) of
                        Just checked ->
                            not checked

                        _ ->
                            False
            in
            { cell | locked = rowLocked }

        cells =
            model.cells |> Array.map lock

        afterToggle =
            if model.selectedCell < 0 then
                selectCell

            else if isCheckboxForCurrentRow cbIndex model then
                \m ->
                    updateCell m.selectedCell
                        (\cell ->
                            { cell | state = state.initial, imageHidden = True }
                        )
                        m
                        |> deselectCurrentCell
                        |> selectCell

            else
                \m -> ( m, Cmd.none )
    in
    { model | checkboxes = checkboxes, cells = cells } |> afterToggle


isCheckboxForCurrentRow : Int -> Model -> Bool
isCheckboxForCurrentRow cbIndex model =
    (model.selectedCell // 9) - 1 == cbIndex


mouseEnter : Int -> Model -> Model
mouseEnter index model =
    { model | hoveredCell = index }


mouseLeave : a -> Model -> Model
mouseLeave _ model =
    { model | hoveredCell = -1 }


modeImageUrl : Model -> String
modeImageUrl model =
    if model.multiplicationMode then
        if model.modeButtonDisabled then
            image.multiplyGray

        else
            image.multiply

    else if model.modeButtonDisabled then
        image.plusGray

    else
        image.plus


errorCountClass : Model -> String
errorCountClass model =
    if model.errorCount > 0 then
        "error"

    else
        ""


cellClass : Cell -> String
cellClass cell =
    if cell.header then
        if cell.selected then
            if cell.locked then
                "header selected excluded"

            else
                "header selected"

        else if cell.locked then
            "header excluded"

        else
            "header"

    else
        "cell"


resetImageUrl : Model -> String
resetImageUrl model =
    if model.resetPressed then
        image.restartPressed

    else
        image.restart


split : Int -> List (List a) -> List a -> List (List a)
split chunkSize aggregate items =
    case items of
        [] ->
            List.reverse aggregate

        _ ->
            split chunkSize
                (List.take chunkSize items :: aggregate)
                (List.drop chunkSize items)


save : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
save ( model, cmd ) =
    ( model, Cmd.batch [ cmd, setStorage model ] )
