module Unscriber exposing (main)

import Collage exposing (Collage, defaultLineStyle, styled, uniform, rotate, shift, shiftX, shiftY)
import Collage.Events exposing (onClick)
import Collage.Layout exposing (align, bottomLeft, center, spacer, stack, vertical)
import Collage.Render
import Color exposing (Color, green, orange, blue, gray, red)
--import Css
--import Html exposing (Html)
import Html.Styled as Html exposing (Html, fromUnstyled)
--import Html.Styled.Attributes exposing (css)
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


type Algorithm = Incremental Float


type alias Model =
    { textDisplay: List String
    , objs: List Obj
    , selectedIndex: Int
    , algorithm: Algorithm
    }


init : (Model, Cmd Msg)
init =
    ( { textDisplay = []
      , objs = []
      , selectedIndex = -1
      , algorithm = Incremental 0.75
      }
    , genObj
    )



-- Update ----------------------------------------------------------------------


type Msg = ObjSelect Int Obj
         | ObjGenerated Obj
         | DecideStopGenerating Float
         | SelectIndex Int


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
        (Random.int 1 (gridSize // 5))
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


type UniProp = USquare
             | UCircle
             | UGreen
             | UBlue
             | UOrange
             | ULeft
             | URight
             | UBottom
             | UTop

uniPropList : List UniProp
uniPropList = [ USquare
              , UCircle
              , UGreen
              , UBlue
              , UOrange
              , ULeft
              , URight
              , UBottom
              , UTop
              ]


type BiProp = BLeft -- TODO


type Prop = UniProp UniProp
          --| BiProp BiProp Obj


getUniProp : Obj -> UniProp -> Float
getUniProp o p =
    case p of
        USquare -> if o.objType == Square then 1 else 0
        UCircle -> if o.objType == Circle then 1 else 0
        UGreen -> if o.color == Green then 1 else 0
        UBlue -> if o.color == Blue then 1 else 0
        UOrange -> if o.color == Orange then 1 else 0
        ULeft -> (toFloat (gridSize - o.x)) / (toFloat gridSize)
        URight -> (toFloat (o.x + o.size)) / (toFloat gridSize)
        UBottom -> (toFloat (gridSize - o.y)) / (toFloat gridSize)
        UTop -> (toFloat (o.y + o.size)) / (toFloat gridSize)


getProp : Obj -> Prop -> Float
getProp o p =
    case p of
        UniProp up -> getUniProp o up


getProps : Obj -> List (Prop, Float)
getProps o = List.map (\x -> (UniProp x, getUniProp o x)) uniPropList


gradeProps : Obj -> List Prop -> Algorithm -> Float
gradeProps o l a =
    let
        scores = List.map (getProp o) l
    in
        --List.foldl (*) 1.0 scores -- TODO t-norm
        case a of
            Incremental t -> if List.all (\x -> x > t) scores then 1.0 else 0.0


selectDescriptors : Obj -> List Obj -> Algorithm -> List Prop
selectDescriptors o allOs a =
    let
        d = List.filter (\x -> x /= o) allOs
    in
    case a of
        Incremental t ->
            let
                select p (d, selected) =
                    if not (List.isEmpty d) && getProp o p > t
                    then
                        let
                            dp = List.filter (\x -> getProp x p > t) d
                        in
                        if dp /= d
                        then (dp, p::selected)
                        else (d, selected)
                    else (d, selected)
            in
                List.foldl select (d, []) (List.map UniProp uniPropList) -- TODO all props
                    |> Tuple.second


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ObjSelect i o -> ({ model
                              | textDisplay =
                                [ toString <| getProps o
                                , toString <| selectDescriptors o model.objs model.algorithm
                                ]
                          }
                         , Cmd.none)
        ObjGenerated o ->
            let
                modelp = if isValidObj model o
                         then {model | objs = o::model.objs}
                         else model
                nextAction = if List.length modelp.objs < 50
                             then genObj
                             else Random.generate DecideStopGenerating (Random.float 0 1)
            in
            (modelp, nextAction)
        DecideStopGenerating f ->
            if f > 0.9
            then (model
                 , Random.generate SelectIndex
                     <| Random.int 0
                     <| List.length model.objs)
            else (model, genObj)
        SelectIndex i -> ({model | selectedIndex = i}, Cmd.none)



-- View ------------------------------------------------------------------------


spacerLen : Float
spacerLen = 15.0


gridSize : Int
gridSize = 40


drawGrid : Int -> Collage Msg
drawGrid n =
    let
        gridLen = spacerLen * (toFloat n)
        offsetLen = (spacerLen - gridLen) / 2.0
        l = Collage.traced
            (Collage.dash Collage.verythin (uniform gray))
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


drawObj : Model -> Int -> Obj -> Collage Msg
drawObj model i o =
    let
        dim = (toFloat o.size) * spacerLen
        shape = case o.objType of
                    Square -> (Collage.square dim)
                    Circle -> (Collage.circle <| dim / 2)
        lineStyle = if i == model.selectedIndex
                    then {defaultLineStyle | fill = (uniform red)}
                    else defaultLineStyle
        style = styled (uniform <| objColorToColor o.color, lineStyle)
        offsets = ((toFloat o.x) * spacerLen, (toFloat o.y) * spacerLen)
    in
    style shape
        |> align bottomLeft
        |> shift offsets
        |> onClick (ObjSelect i o)


drawScene : Model -> Collage Msg
drawScene model = stack <| List.indexedMap (drawObj model) model.objs


renderCollage : Model -> Html Msg
renderCollage model =
    let
        grid = drawGrid gridSize
        base =
            stack
            [ drawScene model
            , grid
            ]
             |> center
    in
        stack
        [ base
        , Collage.outlined
            defaultLineStyle
            (Collage.square (Collage.Layout.width grid + 10))
        , Collage.filled
            (uniform Color.white)
            (Collage.square (Collage.Layout.width grid + 50))
        ]
            --|> Collage.scale 1
            |> Collage.Render.svg
            |> fromUnstyled


view : Model -> Html Msg
view model =
    Html.body []
        -- [ css
        --   [ Css.margin2 (Css.px 40) Css.auto
        --   , Css.maxWidth (Css.px 950)
        --   , Css.fontSize (Css.px 20)
        --   , Css.color (Css.hex "152055")
        --   , Css.backgroundColor (Css.hex "ffefd0")
        --   , Css.padding2 Css.zero (Css.px 40)
        --   ]
        -- ]
        [ Html.table []
              [ Html.tr []
                    [ Html.td [] [ renderCollage model ]
                    , Html.td []
                        (List.map
                             (\t -> Html.p [] [Html.text t])
                             model.textDisplay)
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


-- TODO
-- continuous colors?
-- landmark system?
