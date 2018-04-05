module Unscriber exposing (main)

import Collage exposing (Collage, styled, uniform, rotate, shift, shiftX, shiftY)
import Collage.Events exposing (onClick)
import Collage.Layout exposing (align, bottomLeft, center, spacer, stack, vertical)
import Collage.Render
import Color exposing (Color, green, orange, blue, gray)
import Html exposing (Html)
import Random as Random exposing (Generator, generate)


-- Model -----------------------------------------------------------------------


type ObjType = Square
             | Circle


type ObjColor = Green
              | Orange
              | Blue
               

type alias Obj =
    { objType: ObjType
    , x: Int
    , y: Int
    , size: Int
    , color: ObjColor
    }


type alias Model =
    { textDisplay: String
    , objs: List Obj
    }


init : (Model, Cmd Msg)
init =
    ( { textDisplay = "init"
      , objs = []
      }
    , genObj
    )


-- Update ----------------------------------------------------------------------


type Msg = ObjSelect Int
         | ObjGenerated Obj
         | DecideStopGenerating Float


objGenerator : Generator Obj
objGenerator =
    let
        f t x y s c =
            { objType =
                  case t of
                      0 -> Square
                      1 -> Circle
                      _ -> Debug.crash "unreachable"
            , x = x
            , y = y
            , size = s
            , color =
                  case c of
                      0 -> Green
                      1 -> Orange
                      2 -> Blue
                      _ -> Debug.crash "unreachable"
            }
    in
    Random.map5 f
        (Random.int 0 1)
        (Random.int 0 (gridSize - 1))
        (Random.int 0 (gridSize - 1))
        (Random.int 1 (gridSize // 4))
        (Random.int 0 2)


genObj : Cmd Msg
genObj = generate ObjGenerated objGenerator


isValidObj : Model -> Obj -> Bool
isValidObj model o =
    let
        noOverlap other = o.x + o.size <= other.x ||
                          o.y + o.size <= other.y ||
                          o.x >= other.x + other.size ||
                          o.y >= other.y + other.size
    in
    (o.x + o.size <= gridSize)
    && (o.y + o.size <= gridSize)
    && List.all noOverlap model.objs


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ObjSelect i -> ({model | textDisplay = toString i}, Cmd.none)
        ObjGenerated o ->
            let
                modelp = if isValidObj model o
                         then {model | objs = o::model.objs}
                         else model
                nextAction = if List.length modelp.objs < 5
                             then genObj
                             else Random.generate DecideStopGenerating (Random.float 0 1)
            in
            (modelp, nextAction)
        DecideStopGenerating f ->
            if f > 0.7
            then (model, Cmd.none)
            else (model, genObj)




-- View ------------------------------------------------------------------------


spacerLen : Float
spacerLen = 30.0


gridSize : Int
gridSize = 18


drawGrid : Int -> Collage Msg
drawGrid n =
    let
        gridLen = spacerLen * (toFloat n)
        offsetLen = (spacerLen - gridLen) / 2.0
        l = Collage.traced
            (Collage.dash Collage.thin (uniform gray))
            (Collage.line <| gridLen)
        s = spacer 0 spacerLen
        ls = vertical <| List.intersperse l <| List.repeat n s
    in
    stack [ls, shiftX offsetLen <| shiftY offsetLen <| rotate (degrees 90) ls]
        |> align bottomLeft


objColorToColor : ObjColor -> Color
objColorToColor oc =
    case oc of
        Green -> green
        Orange -> orange
        Blue -> blue


drawObj : Int -> Obj -> Collage Msg
drawObj i o =
    let
        dim = (toFloat o.size) * spacerLen
        shape = case o.objType of
                    Square -> (Collage.square dim)
                    Circle -> (Collage.circle <| dim / 2)
        style = styled (uniform <| objColorToColor o.color, Collage.defaultLineStyle)
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
    , drawGrid gridSize
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
                  , Html.p [] [Html.text <| toString model.objs]
                  ]
              ]
        ]



-- Subscriptions ---------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Main ------------------------------------------------------------------------


main : Program Never Model Msg
main =
  Html.program
      { init = init
      , subscriptions = subscriptions
      , view = view
      , update = update
      }
