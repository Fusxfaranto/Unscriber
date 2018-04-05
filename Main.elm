module House exposing (main)

import Collage exposing (Collage, styled, uniform, rotate, shiftX, shiftY)
import Collage.Events
import Collage.Layout exposing (spacer, stack, vertical)
import Collage.Render
import Color exposing (..)
import Html exposing (Html)


-- Model -----------------------------------------------------------------------


type alias Model =
    { asdf: Int }


init : Model
init =
    { asdf = 0 }



-- Update ----------------------------------------------------------------------


type alias Msg =
    { }


update : Msg -> Model -> Model
update msg model =
    { asdf = 0 }



-- View ------------------------------------------------------------------------


drawGrid : Int -> Collage Msg
drawGrid n =
    let
        spacerLen = 50.0
        gridLen = spacerLen * (toFloat n)
        offsetLen = (spacerLen - gridLen) / 2.0
        l = Collage.traced
            (Collage.dash Collage.thin (uniform gray))
            (Collage.line <| gridLen)
        s = spacer 0 spacerLen
        ls = vertical <| List.intersperse l <| List.repeat n s
    in
    stack [ls, shiftX offsetLen <| shiftY offsetLen <| rotate (degrees 90) ls]


genScene : Model -> Collage Msg
genScene model = styled (uniform red, Collage.defaultLineStyle) (Collage.square 100)


view : Model -> Html Msg
view model =
    --genScene model
    drawGrid 9
        --|> Collage.scale 1
        |> Collage.Render.svg



-- Main ------------------------------------------------------------------------


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }
