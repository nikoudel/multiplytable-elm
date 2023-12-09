module Main exposing (..)

import AppLogic as L
import Browser
import Model as M
import Time
import View


main : Program (Maybe M.Model) M.Model M.Msg
main =
    Browser.element
        { init = init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : M.Model -> Sub M.Msg
subscriptions _ =
    Time.every 1000 (\_ -> M.Tick)


init : Maybe M.Model -> ( M.Model, Cmd M.Msg )
init savedModel =
    case savedModel of
        Just model ->
            if model.selectedCell < 0 then
                model |> L.selectCell

            else
                ( model, L.focus M.theInputId )

        _ ->
            M.emptyModel |> L.selectCell


update : M.Msg -> M.Model -> ( M.Model, Cmd M.Msg )
update msg model =
    case msg of
        M.NoOp ->
            ( model, Cmd.none )

        M.Tick ->
            ( List.foldr L.updateCells model model.activeCells, Cmd.none )

        M.Focus ->
            ( model, Cmd.batch [ L.focus M.theInputId ] )

        M.UserInput keyCode ->
            L.onUserInput keyCode model

        M.ToggleMode ->
            ( L.toggleMode model, Cmd.none ) |> L.save

        M.ResetModel ->
            L.selectCell M.emptyModel |> L.save

        M.ResetDown ->
            ( { model | resetPressed = True }, Cmd.none )

        M.ToggleCheckbox index ->
            L.toggleCheckbox index model |> L.save

        M.SelectCell indexOfNextCellToSelect ->
            model |> L.selectNewCell indexOfNextCellToSelect

        M.MouseEnter index ->
            ( model |> L.mouseEnter index, Cmd.none )

        M.MouseLeave index ->
            ( model |> L.mouseLeave index, Cmd.none )
