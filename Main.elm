module Unscriber exposing (main)

import Collage exposing (Collage, defaultLineStyle, styled, uniform, rotate, shift, shiftX, shiftY)
import Collage.Events exposing (onClick)
import Collage.Layout exposing (align, bottomLeft, center, spacer, stack, vertical)
import Collage.Render
import Color exposing (Color, green, orange, blue, gray, red)
import Css
--import Html exposing (Html)
import Html.Styled as Html exposing (Html, fromUnstyled)
import Html.Styled.Attributes as Attributes exposing (css, type_, value)
import Html.Styled.Events as Events exposing (onInput)
import List.Extra exposing (lift2, maximumBy)
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
               | Fuzzy


type alias Model =
    { textDisplay: List String
    , objs: List Obj
    , selectedIndex: Int
    , algorithm: Algorithm
    , gridSize : Int
    , numObjects : Int
    }


init : (Model, Cmd Msg)
init =
    let
        model =
            { textDisplay = []
            , objs = []
            , selectedIndex = -1
            , algorithm = Fuzzy
            , gridSize = 35
            , numObjects = 30
            }
    in
        (model, genObj model)



-- Update ----------------------------------------------------------------------


type Msg = ObjSelect Int Obj
         | ObjGenerated Obj
         | StartGeneration
         | SelectIndex Int
         | ChooseAlgorithm String
         | SetNumObjects String


maxSize : Model -> Int
maxSize model = model.gridSize // 5


objGenerator : Model -> Generator Obj
objGenerator model =
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
        (Random.int 0 (model.gridSize - 1))
        (Random.int 0 (model.gridSize - 1))
        (Random.int 1 (maxSize model))
        (Random.int 0 2)


genObj : Model -> Cmd Msg
genObj model = generate ObjGenerated (objGenerator model)


isValidObj : Model -> Obj -> Bool
isValidObj model o =
    let
        noOverlap other = o.x + o.size <= other.x ||
                          o.y + o.size <= other.y ||
                          o.x >= other.x + other.size ||
                          o.y >= other.y + other.size
    in
    (o.x + o.size <= model.gridSize)
    && (o.y + o.size <= model.gridSize)
    && List.all noOverlap model.objs


objCenter : Obj -> (Float, Float)
objCenter o = ( toFloat o.x + (toFloat o.size) / 2
              , toFloat o.y + (toFloat o.size) / 2
              )


dist : Obj -> Obj -> Float
dist a b =
    let
        (ax, ay) = objCenter a
        (bx, by) = objCenter b
    in
    (ax - bx) ^ 2 + (ay - by) ^ 2 |> sqrt


-- NB: result is signed
hDist : Obj -> Obj -> Float
hDist a b =
    let
        (ax, ay) = objCenter a
        (bx, by) = objCenter b
    in
    ax - bx


logistic : Float -> Float
logistic f = 1 / (1 + e ^ (-f))


searchThreshold : Float
searchThreshold = 0.6


type BiProp = BLeft
            | BRight
            | BBelow
            | BAbove


biPropList : List BiProp
biPropList = [ BLeft
             , BRight
             , BBelow
             , BAbove
             ]


type UniProp = UBiProp BiProp Obj
             | USquare
             | UCircle
             | UGreen
             | UBlue
             | UOrange
             | USmall
             | ULarge
             | ULeft
             | URight
             | UBottom
             | UTop
             | UMiddle


uniPropList : List UniProp
uniPropList = [ USquare
              , UCircle
              , UGreen
              , UBlue
              , UOrange
              , USmall
              , ULarge
              , ULeft
              , URight
              , UBottom
              , UTop
              , UMiddle
              ]


unconditionalProps : List UniProp
unconditionalProps = [ USquare
                     , UCircle
                     , UGreen
                     , UBlue
                     , UOrange
                     ]


rateDist : (Obj -> Obj -> Float) -> Obj -> Obj -> Float
rateDist f a b = ((f a b) / (dist a b) ^ 2) / 5


getUniProp : Obj -> List Obj -> Model -> UniProp -> Float
getUniProp o os model p =
    let
        g = toFloat model.gridSize
    in
    case p of
        UBiProp bp other ->
            case model.algorithm of
                Incremental t -> 0 -- not really any good way to do bivalence with "pure" incremental alg afaik
                Fuzzy ->
                    let
                        newOs = List.filter (\x -> x /= other) os
                        bScore =
                            (case bp of
                                 BLeft  -> -(rateDist hDist o other) * g
                                 BRight -> rateDist hDist o other * g
                                 BBelow -> -(rateDist hDist o other) * g
                                 BAbove -> rateDist hDist o other * g
                            )
                               |> logistic
                    in
                        if bScore > searchThreshold
                        then bScore *
                            (gradeProps
                                 other
                                 (o::newOs)
                                 (selectDescriptors other newOs model)
                                 model
                            )
                        else 0
        USquare -> if o.objType == Square then 1 else 0
        UCircle -> if o.objType == Circle then 1 else 0
        UGreen  -> if o.color == Green then 1 else 0
        UBlue   -> if o.color == Blue then 1 else 0
        UOrange -> if o.color == Orange then 1 else 0
        USmall  -> (toFloat (maxSize model - o.size)) / (toFloat (maxSize model - 1))
        ULarge  -> (toFloat (o.size - 1)) / (toFloat (maxSize model - 1))
        ULeft   -> (g - (toFloat o.x)) / g
        URight  -> (toFloat (o.x + o.size)) / g
        UBottom -> (g - (toFloat o.y)) / g
        UTop    -> (toFloat (o.y + o.size)) / g
        UMiddle ->
            let
                (x, y) = objCenter o
                gridCenter = (g / 2)
            in
            (x - gridCenter) ^ 2 + (y - gridCenter) ^ 2
                |> sqrt
                |> \x -> 1 - x / g


getUniPropList : List Obj -> List UniProp
getUniPropList os = List.append uniPropList
                    <| lift2 (\x y -> UBiProp x y) biPropList os


getProps : Obj -> List Obj -> List UniProp -> Model -> List (UniProp, Float)
getProps o os l model =
        List.map (\x -> (x, getUniProp o os model x)) l
                |> List.sortBy (negate << Tuple.second)


gradePropsWithoutContext : Obj -> List Obj -> List UniProp -> Model -> Float
gradePropsWithoutContext o os l model =
    let
        scores = List.map Tuple.second <| getProps o os l model
    in
        case model.algorithm of
            Incremental t ->
                if List.all (\x -> x > t) scores -- TODO is this ever false?
                then 1.0
                else 0.0
            Fuzzy ->
                List.foldl (*) 1.0 scores -- TODO better t-norm?


gradeProps : Obj -> List Obj -> List UniProp -> Model -> Float
gradeProps o os l model =
    let
        oScore = gradePropsWithoutContext o os l model
        otherScores = List.map (\x -> gradePropsWithoutContext x os l model) os
                      |> List.sortBy negate
    in
        case model.algorithm of
            Incremental t -> oScore
            Fuzzy -> -- TODO something smarter
                oScore - (List.filter (\x -> x >= oScore) otherScores
                         |> List.length
                         |> toFloat
                         ) * 0.5
                    |> max 0


selectDescriptors : Obj -> List Obj -> Model -> List UniProp
selectDescriptors o os model =
    let
        unconditionals = List.filter
                         (\x -> getUniProp o os model x > 0.5)
                         unconditionalProps
        notUnconditionals = List.filter
                            (\x -> not (List.member x unconditionalProps))
                            uniPropList
    in
    case model.algorithm of
        Incremental t ->
            let
                select p (d, selected) =
                    if not (List.isEmpty d) && getUniProp o d model p > t
                    then
                        let
                            dp = List.filter (\x -> getUniProp x d model p > t) d
                        in
                        if dp /= d
                        then (dp, p::selected)
                        else (d, selected)
                    else (d, selected)
            in
                List.foldl select (os, []) (getUniPropList os)
                    |> Tuple.second
        Fuzzy ->
            let
                filterDs : List Obj -> List UniProp -> List Obj
                filterDs d ps =
                    let
                        currScore = gradePropsWithoutContext o os ps model
                    in
                    List.filter
                    (\x -> gradePropsWithoutContext x os ps model >= currScore) -- TODO threshold?
                    d

                select : List Obj -> List UniProp -> List UniProp -> List UniProp
                select d selected unselected =
                    case d of
                        [] -> selected
                        _  ->
                            let
                                newP =
                                    maximumBy
                                    (\x -> gradeProps o os (x::selected) model)
                                    unselected
                            in
                                case newP of
                                    Nothing -> selected
                                    Just p ->
                                        let
                                            newSelected = p::selected
                                            newUnselected = List.filter (\x -> x /= p) unselected
                                        in
                                        select (filterDs d newSelected) newSelected newUnselected
            in
                select (filterDs os unconditionals) (List.reverse unconditionals) notUnconditionals


type SyntaxTree = Leaf String
                | Branch SyntaxTree SyntaxTree


type BranchType = Left
                | Right


describeProp : UniProp -> List Obj -> Model -> (SyntaxTree, BranchType)
describeProp p os model =
    case p of
        UBiProp bp other ->
            let
                newOs = List.filter (\x -> x /= other) os
                leaf = case bp of
                           BLeft  -> (Leaf "left of the")
                           BRight -> (Leaf "right of the")
                           BBelow -> (Leaf "below the")
                           BAbove -> (Leaf "above the")
            in
                ( Branch
                     leaf
                     (describeTree (selectDescriptors other newOs model) os model)
                , Right
                )
        USquare -> (Leaf "square", Left)
        UCircle -> (Leaf "round",  Left)
        UGreen  -> (Leaf "green",  Left)
        UBlue   -> (Leaf "blue",   Left)
        UOrange -> (Leaf "orange", Left)
        USmall  -> (Leaf "small",  Left)
        ULarge  -> (Leaf "large",  Left)
        ULeft   -> (Leaf "left",   Left)
        URight  -> (Leaf "right",  Left)
        UBottom -> (Leaf "bottom", Left)
        UTop    -> (Leaf "top",    Left)
        UMiddle -> (Leaf "middle", Left)


describeTree : List UniProp -> List Obj -> Model -> SyntaxTree
describeTree ps os model =
    case ps of
        []    -> Leaf "object"
        USquare::[] -> Leaf "square"
        UCircle::[] -> Leaf "circle"
        x::xs -> case describeProp x os model of
                     (t, Left) -> Branch t (describeTree xs os model)
                     (t, Right) -> Branch (describeTree xs os model) t


flattenTree : SyntaxTree -> String
flattenTree t =
    case t of
        Leaf s -> s
        Branch t1 t2 -> (flattenTree t1) ++ " " ++ (flattenTree t2)


describe : List UniProp -> List Obj -> Model -> String
describe ps os model =
    "The " ++ flattenTree (describeTree ps os model)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ObjSelect i o ->
            let
                d = List.filter (\x -> x /= o) model.objs
                descriptors = selectDescriptors o d model
            in
            ({ model
                 | textDisplay =
                   [ toString <| getProps o d (getUniPropList d) model
                   , toString <| descriptors
                   , toString <| gradeProps o d descriptors model
                   , toString <| describeTree descriptors d model
                   , toString <| describe descriptors d model
                   ]
             }
            , Cmd.none)
        StartGeneration ->
            ({model | objs = []}, genObj model)
        ObjGenerated o ->
            let
                modelp = if isValidObj model o
                         then {model | objs = o::model.objs}
                         else model
                nextAction = if List.length modelp.objs < modelp.numObjects
                             then genObj modelp
                             else Random.generate SelectIndex
                                 <| Random.int 0
                                 <| List.length modelp.objs
            in
            (modelp, nextAction)
        SelectIndex i -> ({model | selectedIndex = i}, Cmd.none)
        ChooseAlgorithm s -> ({model | algorithm =
                                   case s of
                                       "Fuzzy" -> Fuzzy
                                       "Incremental" -> Incremental 0.65
                                       _ -> Debug.crash "bad option"
                              }, Cmd.none)
        SetNumObjects s ->
            case String.toInt s of
                Err _ -> (model, Cmd.none)
                Ok n  -> ({model | numObjects = n, objs = []}, genObj model)





-- View ------------------------------------------------------------------------


spacerLen : Float
spacerLen = 15.0


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
        grid = drawGrid model.gridSize
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
                    -- , Html.td []
                    --     (List.map
                    --          (\t -> Html.p [] [Html.text t])
                    --          model.textDisplay)
                    , Html.td []
                        [ Html.div
                              [ css
                                [ Css.overflow Css.scroll
                                , Css.height (Css.px (toFloat model.gridSize * spacerLen))
                                ]
                              ]
                              [ Html.pre []
                                    [ Html.text
                                          (String.join "\n\n" model.textDisplay)
                                    ]
                              ]
                        ]
                    ]
              ]
        --, Html.br [] []
        , Html.div []
            [ Html.table []
                  [ Html.tr []
                        [ Html.td [] []
                        , Html.td [] [Html.text "Algorithm:"]
                        , Html.td [] [Html.text "Number of Objects:"]
                        ]

                  , Html.tr []
                        [ Html.td []
                              [ Html.button
                                    [Events.onClick StartGeneration]
                                    [Html.text "Regenerate"]
                              ]
                        , Html.td []
                              [ Html.select
                                    [ onInput ChooseAlgorithm
                                    ]
                                    (List.map
                                         (\x -> Html.option [value x] [Html.text x])
                                         ["Fuzzy", "Incremental"]
                                    )
                              ]
                        , Html.td []
                              [ Html.select
                                    [ onInput SetNumObjects
                                    ]
                                    (List.map
                                         ((\x ->
                                               Html.option
                                               [ value x
                                               , Attributes.selected (x == "30")
                                               ]
                                               [Html.text x]) << toString)
                                         [5, 10, 20, 30, 40]
                                    )
                              ]
                        ]
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

