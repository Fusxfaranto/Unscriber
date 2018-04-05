module House exposing (main)

import Collage exposing (Collage, styled, uniform, rotate, shift, shiftX, shiftY)
import Collage.Events exposing (onClick)
import Collage.Layout exposing (align, bottomLeft, center, spacer, stack, vertical)
import Collage.Render
import Color exposing (..)
import Html exposing (Html)


-- Model -----------------------------------------------------------------------


type ObjType = Square
             | Circle


type alias Obj =
    { objType: ObjType
    , x: Int
    , y: Int
    , size: Int
    , color: Color
    }


type alias Model =
    { textDisplay: String
    , objs: List Obj
    }


init : Model
init =
    { textDisplay = "init"
    , objs = [ { objType = Circle
               , x = 3
               , y = 7
               , size = 2
               , color = orange
               }
             , { objType = Square
               , x = 4
               , y = 1
               , size = 3
               , color = yellow
               }
          ]
    }



-- Update ----------------------------------------------------------------------


type Msg = ObjSelect Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        ObjSelect i -> {model | textDisplay = toString i}



-- View ------------------------------------------------------------------------


spacerLen : Float
spacerLen = 50.0


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
    align bottomLeft <| stack [ls, shiftX offsetLen <| shiftY offsetLen <| rotate (degrees 90) ls]


drawObj : Int -> Obj -> Collage Msg
drawObj i o =
    let
        dim = (toFloat o.size) * spacerLen
        shape = case o.objType of
                    Square -> (Collage.square dim)
                    Circle -> (Collage.circle <| dim / 2)
        style = styled (uniform o.color, Collage.defaultLineStyle)
        offsets = ((toFloat o.x) * spacerLen, (toFloat o.y) * spacerLen)
    in
    style shape
        |> align bottomLeft
        |> shift offsets
        |> onClick (ObjSelect i)


drawScene : Model -> Collage Msg
drawScene model = stack <| List.indexedMap drawObj model.objs


renderCollage : Model -> Html Msg
renderCollage model =
    stack
    [ drawScene model
    , drawGrid 10
    ]
        --|> Collage.scale 1
        |> Collage.Render.svg


view : Model -> Html Msg
view model =
    Html.table []
        [ Html.tr []
              [ Html.td [] [ renderCollage model ]
              , Html.td []
                  [ Html.p [] [Html.text <| model.textDisplay]
                  ]
              ]
        ]



-- Main ------------------------------------------------------------------------


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }
