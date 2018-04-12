module Unscriber exposing (main)

import Collage exposing (Collage, defaultLineStyle, styled, uniform, rotate, shift, shiftX, shiftY)
import Collage.Events exposing (onClick)
import Collage.Layout exposing (align, bottomLeft, center, spacer, stack, vertical, impose)
import Collage.Render
import Color exposing (Color, green, orange, blue, gray, red)
import Css
--import Html exposing (Html)
import Html.Styled as Html exposing (Html, fromUnstyled)
import Html.Styled.Attributes as Attributes exposing (css, type_, value)
import Html.Styled.Events as Events exposing (onInput)
import List.Extra exposing (lift2, maximumBy, (!!))
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
               | Fuzzy Float


type DisplayMode = DebugView
                 | Demo


type DemoState = DemoStarted
               | DemoWrong
               | DemoRight


type alias Model =
    { textDisplay: List String
    , objs: List Obj
    , objUniScores : List (List UniProp, Float)
    , selectedIndex : Int
    , secretIndex : Int
    , secretDescription : String
    , demoState : DemoState
    , algorithm: Algorithm
    , gridSize : Int
    , numObjects : Int
    , displayMode : DisplayMode
    }


init : (Model, Cmd Msg)
init =
    let
        model =
            { textDisplay = []
            , objs = []
            , objUniScores = []
            , selectedIndex = -1
            , secretIndex = -1
            , secretDescription = ""
            , demoState = DemoStarted
            , algorithm = Fuzzy 0.05
            , gridSize = 44
            , numObjects = 34
            , displayMode = Demo
            }
    in
        update StartGeneration model



-- Update ----------------------------------------------------------------------


type Msg = ObjSelect Int Obj
         | ObjGenerated Obj
         | StartGeneration
         | ChooseAlgorithm String
         | SetNumObjects String
         | ChooseDisplayMode String
         | GenerateSecret Int


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


vDist : Obj -> Obj -> Float
vDist a b =
    let
        (ax, ay) = objCenter a
        (bx, by) = objCenter b
    in
    ay - by


logistic : Float -> Float
logistic f = 1 / (1 + e ^ (-f))


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


type UniProp = UBiProp BiProp Obj (List UniProp, Float)
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
rateDist f a b = ((f a b) / (dist a b) ^ 2)


getUniProp : Obj -> UniProp -> Model -> Float
getUniProp o p model =
    let
        g = toFloat model.gridSize
    in
    case p of
        UBiProp bp other (_, otherScore) ->
            if o == other
            then 0
            else
                let
                    bScore =
                        (case bp of
                             BLeft  -> -(rateDist hDist o other) * g
                             BRight -> rateDist hDist o other * g
                             BBelow -> -(rateDist vDist o other) * g
                             BAbove -> rateDist vDist o other * g
                        )
                           |> logistic
                in
                    otherScore * bScore
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


getUniPropListWithLandmarks : Model -> List UniProp
getUniPropListWithLandmarks model =
    List.map2 (\x (y, z) -> (x, y, z)) model.objs model.objUniScores
        |> lift2 (\bp (o, ups, oScore) -> UBiProp bp o (ups, oScore)) biPropList
        |> List.append uniPropList
        |> Debug.log "props"


getProps : Obj -> List UniProp -> Model -> List (UniProp, Float)
getProps o l model =
        List.map (\x -> (x, getUniProp o x model)) l
                |> List.sortBy (negate << Tuple.second)


fuzzyTNorm : Float -> Float -> Float
fuzzyTNorm a b = a * b -- TODO better t-norm?


gradePropsWithoutContext : Obj -> List UniProp -> Model -> Float
gradePropsWithoutContext o l model =
    let
        scores = List.map Tuple.second <| getProps o l model
    in
        case model.algorithm of
            Incremental t ->
                if List.all (\x -> x > t) scores -- TODO is this ever false?
                then 1.0
                else 0.0
            Fuzzy t ->
                List.foldl fuzzyTNorm 1.0 scores


gradeProps : Obj -> List Obj -> List UniProp -> Model -> Float
gradeProps o os l model =
    let
        oScore = gradePropsWithoutContext o l model
        -- otherScores = List.map (\x -> gradePropsWithoutContext x os l model) os
        --               |> List.sortBy negate
    in
        case model.algorithm of
            Incremental t -> oScore
            Fuzzy t ->
                case List.map
                    (\x -> gradePropsWithoutContext x l model)
                    os
                        |> List.maximum
                of
                    Nothing -> Debug.crash "probably unreachable"
                    Just x  ->
                        oScore - x -- TODO max?


gradeSingleProp : Obj -> List Obj -> UniProp -> Float -> List Float -> Model -> (Float, List Float)
gradeSingleProp o os p oScore osScores model =
    let
        oScoreNew = fuzzyTNorm oScore (getUniProp o p model)
    in
    case model.algorithm of
        Incremental t -> Debug.crash "don't"
        Fuzzy t ->
            let
                osScoresNew = List.map2
                              (\x y -> fuzzyTNorm y (getUniProp x p model))
                              os
                              osScores
            in
            case List.maximum osScoresNew
            of
                Nothing -> Debug.crash "probably unreachable"
                Just x  ->
                    (oScoreNew - x, osScoresNew) -- TODO max?



selectDescriptors : Obj -> List Obj -> List UniProp -> Model -> List UniProp
selectDescriptors o os possProps model =
    let
        unconditionals = List.filter
                         (\x -> getUniProp o x model > 0.5)
                         unconditionalProps
                             |> List.reverse
    in
    case model.algorithm of
        Incremental t ->
            let
                select p (d, selected) =
                    if not (List.isEmpty d) && getUniProp o p model > t
                    then
                        let
                            dp = List.filter (\x -> getUniProp x p model > t) d
                        in
                        if dp /= d
                        then (dp, p::selected)
                        else (d, selected)
                    else (d, selected)
            in
                List.foldl select (os, []) possProps
                    |> Tuple.second
        Fuzzy t ->
            let
                -- filterDs : List Obj -> List UniProp -> List Obj
                -- filterDs d ps =
                --     let
                --         currScore = gradePropsWithoutContext o os ps model
                --     in
                --     List.filter
                --     (\x -> gradePropsWithoutContext x os ps model >= currScore) -- TODO threshold?
                --     d

                -- TODO filter out distractors that hit zero or go below some threshold?
                select : List UniProp -> List UniProp -> Float -> List Float -> Float -> List UniProp
                select selected unselected oScore osScores oScoreC =
                    if oScoreC > t
                    then selected
                    else
                        let
                            newP =
                                maximumBy
                                (\x -> Tuple.first (gradeSingleProp o os x oScore osScores model))
                                unselected
                                --|> Debug.log "newP"
                        in
                            case newP of
                                Nothing -> Debug.crash "this shouldn't ever happen in practice" -- TODO probably remove
                                Just p ->
                                    let
                                        newSelected = p::selected
                                        newUnselected = List.filter (\x -> x /= p) unselected
                                        (oScoreCNew, osScoresNew) = gradeSingleProp o os p oScore osScores model
                                                              --|> Debug.log "(oScoreCNew, osScoresNew)"
                                    in
                                        if oScoreCNew <= oScoreC
                                        then selected
                                        else
                                            select
                                            newSelected
                                            newUnselected
                                            (fuzzyTNorm oScore (getUniProp o p model))
                                            osScoresNew
                                            oScoreCNew
            in
                select
                --(filterDs os unconditionals)
                unconditionals
                (List.filter
                     (\x -> not (List.member x unconditionalProps))
                     possProps
                )
                (gradePropsWithoutContext o unconditionals model)
                (List.map (\x -> gradePropsWithoutContext x unconditionals model) os)
                (gradeProps o os unconditionals model)


type SyntaxTree = Leaf String
                | Branch SyntaxTree SyntaxTree


type BranchType = Left
                | Right


describeProp : UniProp -> (SyntaxTree, BranchType)
describeProp p =
    case p of
        UBiProp bp other (otherProps, _) ->
            let
                leaf = case bp of
                           BLeft  -> (Leaf "left of the")
                           BRight -> (Leaf "right of the")
                           BBelow -> (Leaf "below the")
                           BAbove -> (Leaf "above the")
            in
                ( Branch
                     leaf
                     (describeTree otherProps False)
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


describeTree : List UniProp -> Bool -> SyntaxTree
describeTree ps hasRight =
    case ps of
        []    -> Leaf "object"
        USquare::[] -> Leaf "square"
        UCircle::[] -> Leaf "circle"
        x::xs -> case describeProp x of
                     (t, Left) -> Branch t (describeTree xs hasRight)
                     (t, Right) ->
                         if hasRight
                         then Branch (Branch (describeTree xs True) t) (Leaf "and")
                         else Branch (describeTree xs True) t


flattenTree : SyntaxTree -> String
flattenTree t =
    case t of
        Leaf s -> s
        Branch t1 t2 -> (flattenTree t1) ++ " " ++ (flattenTree t2)


describe : List UniProp -> String
describe ps =
    "the " ++ flattenTree (describeTree ps False)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ObjSelect i o ->
            case model.displayMode of
                DebugView ->
                    let
                        d = List.filter (\x -> x /= o) model.objs
                        possProps = getUniPropListWithLandmarks model
                        descriptors = selectDescriptors o d possProps model
                    in
                    ({ model
                         | textDisplay =
                           [ toString <| getProps o possProps model
                           , toString <| descriptors
                           , toString <| getProps o descriptors model
                           --, toString <| gradePropsWithoutContext o d (List.drop 1 descriptors) model
                           , toString <| gradePropsWithoutContext o descriptors model
                           , toString <| gradeProps o d descriptors model
                           , toString <| describeTree descriptors False
                           , toString <| describe descriptors
                           ]
                         , selectedIndex = i
                     }
                    , Cmd.none)
                Demo ->
                    if model.demoState == DemoStarted
                    then ({model
                              | selectedIndex = i
                              , demoState = if i == model.secretIndex
                                            then DemoRight
                                            else DemoWrong
                          }, Cmd.none)
                    else (model, Cmd.none)
        StartGeneration ->
            ({model
                 | objs = []
                 , selectedIndex = -1
                 , demoState = DemoStarted
             }, genObj model)
        ObjGenerated o ->
            if isValidObj model o
            then
                let
                    modelp = if isValidObj model o
                             then {model | objs = o::model.objs}
                             else model
                in
                if List.length modelp.objs < model.numObjects
                then (modelp, genObj model)
                else ({modelp | objUniScores =
                           List.map
                           (\x ->
                                let
                                    os = (List.filter (\y -> y /= x) modelp.objs)
                                    l = selectDescriptors
                                        x
                                        os
                                        uniPropList
                                        model
                                in
                                (l, gradeProps x os l modelp)
                           )
                           modelp.objs
                           --|> Debug.log "objUniScores"
                      }
                     , generate GenerateSecret (Random.int 0 (model.numObjects - 1)))
            else (model, genObj model)
        ChooseAlgorithm s ->
            update (GenerateSecret model.secretIndex)
            {model | algorithm =
                 case s of
                     "Fuzzy (partial specificity)" -> Fuzzy 0.05
                     "Fuzzy (full specificity)" -> Fuzzy 100
                     "Incremental (low threshold)" -> Incremental 0.55
                     "Incremental (high threshold)" -> Incremental 0.8
                     _ -> Debug.crash "bad option"
            }
        SetNumObjects s ->
            case String.toInt s of
                Err _ -> (model, Cmd.none)
                Ok n  -> update StartGeneration {model | numObjects = n}
        ChooseDisplayMode s -> ({model
                                    | displayMode =
                                        case s of
                                            "Debug" -> DebugView
                                            "Demo"  -> Demo
                                            _ -> Debug.crash "bad option"
                                     , selectedIndex = -1
                                }, Cmd.none)
        GenerateSecret i ->
            let
                o = case model.objs !! i of
                        Just x  -> x
                        Nothing -> Debug.crash "should be unreachable"
                d = List.filter (\x -> x /= o) model.objs
                possProps = getUniPropListWithLandmarks model
                descriptors = selectDescriptors o d possProps model
            in
            ({model
                 | secretIndex = i
                 , secretDescription = describe descriptors
             }, Cmd.none)





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
                    then {defaultLineStyle
                             | fill = (uniform red)
                             , thickness = Collage.semithick}
                    else if i == model.secretIndex &&
                            model.demoState == DemoWrong &&
                            model.displayMode == Demo
                         then {defaultLineStyle
                                  | dashPattern = [(6, 3)]
                                  , thickness = Collage.thick}
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
            impose
            (drawScene model)
            grid

             |> center
        background =
            stack
            [ Collage.outlined
                  defaultLineStyle
                  (Collage.square (Collage.Layout.width grid + 10))
            , Collage.filled
                  (uniform Color.white)
                  (Collage.square (Collage.Layout.width grid + 50))
            ]
    in
        impose base background
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
                    , Html.td [] [Html.div [css [Css.width (Css.px (spacerLen * 2))]] []]
                    , Html.td
                        [ css
                          [ Css.verticalAlign Css.top
                          ]
                        ]
                        [ case model.displayMode of
                              DebugView ->
                                  Html.div
                                  [ css
                                    [ Css.overflow Css.scroll
                                    , Css.height (Css.px (toFloat model.gridSize * spacerLen))
                                    , Css.width (Css.px (toFloat model.gridSize * spacerLen))
                                    ]
                                  ]
                                  [ Html.pre []
                                        [ Html.text
                                              (String.join "\n\n" model.textDisplay)
                                        ]
                                  ]
                              Demo ->
                                  Html.div
                                  [ css
                                    [ Css.fontSize (Css.px 36)
                                    , Css.verticalAlign Css.top
                                    ]
                                  ]
                                  [ Html.p [] [Html.text <| "Click on " ++ model.secretDescription ++ "."]
                                  , Html.p [] [
                                         Html.text <|
                                             case model.demoState of
                                                 DemoStarted -> ""
                                                 DemoRight -> "Hooray!  You got it!  Click the \"Regenerate\" button to play again."
                                                 DemoWrong -> "Oops, not quite...   Click the \"Regenerate\" button to play again."
                                        ]
                                  ]
                        ]
                    ]
              ]
        --, Html.br [] []
        , Html.div []
            [ Html.table
                  [ css
                    [ Css.borderSpacing (Css.px 10)
                    ]
                  ]
                  [ Html.tr []
                        [ Html.td [] []
                        , Html.td [] [Html.text "Algorithm:"]
                        , Html.td [] [Html.text "Number of Objects:"]
                        , Html.td [] [Html.text "Display Mode:"]
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
                                         [ "Fuzzy (partial specificity)"
                                         , "Fuzzy (full specificity)"
                                         , "Incremental (low threshold)"
                                         , "Incremental (high threshold)"
                                         ]
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
                                               , Attributes.selected (x == "34")
                                               ]
                                               [Html.text x]) << toString)
                                         [2, 3, 5, 8, 13, 21, 34, 55, 89]
                                    )
                              ]
                        , Html.td []
                              [ Html.select
                                    [ onInput ChooseDisplayMode
                                    ]
                                    (List.map
                                         (\x -> Html.option [value x] [Html.text x])
                                         ["Demo", "Debug"]
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

