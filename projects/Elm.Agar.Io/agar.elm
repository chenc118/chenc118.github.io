module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick,onInput)
import Html.Attributes exposing (..)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Keyboard as Key
import Mouse as Mouse
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random exposing (..)
import Window exposing (..)
import Task exposing (..)
import AnimationFrame as Anim

--a highly simplified version of agar.io, not going to bother with the time needed to make the background scroll and other stuff

--to make the type definitions a lot more easier
type alias FeedBit = List Feed
type alias Feed = {x:Int,y:Int,value:Float,color:String}

type Either a b = RS a | LS b -- lazy implementation of Haskell's Either

type alias Display = Either PImage String -- can be one or the other, need to extract on display
type alias PImage = {source:String} -- basic stuff needed to model the player image

type Control = Mouse | Keys

type alias RNG = {range:Int,regChance:Int,superChance:Int,limit:Int}-- various stuffs that control the RNG aspects of the game

type alias Model = {x:Float, y:Float,
                    mx:Int,my:Int,
                    winH: Int, winW:Int,
                    name:String,
                    feed: FeedBit,
                    size:Float,
                    display:Display,
                    control:Control,
                    rng:RNG,
                    fps:Bool,
                    minimap:Bool,
                    tickFrames:{first:Float,count:Int},--stuff to get the FPS aka ticks/second
                    tickRate:Float,
                    inGame:Bool} -- list of records representing the different dots etc
type alias DUpdate = Either String String --either holds info on whether it came for URL input or radio button input

type Msg = KeyMsg Key.KeyCode 
            | RandResult (Int,Int)
            | DispUpdate DUpdate 
            | NameUpdate String 
            | StartG 
            | UpdateWinSize Window.Size
            | Tick Float
            | MouseMsg Mouse.Position
            | Toggle String


init : (Model, Cmd.Cmd Msg)
init = ({x=(toFloat svWidth)/2,
        y=(toFloat svHeight)/2,
        mx = 0,my=0,
        winH = 0, winW = 0,
        name="",--default name in agar.io, aka nothing
        feed=[],
        size=25,-- set size to a really big number to see - infinity
        control=Mouse,
        display = LS "#ff0000",
        rng = {range = 50, regChance=1, superChance=0,limit=100},
        fps = True,
        minimap = True,
        tickFrames={first=0,count=0},-- debug for animation tickRate
        tickRate=0,
        inGame=False
    }
    , Task.perform UpdateWinSize Window.size)


extractMod : (Model, Cmd.Cmd Msg) -> Model
extractMod (model,_)=model

--resets the game model
resetGame : Model -> Model
resetGame model = let 
        initial = extractMod init

    in {model | x=initial.x, y = initial.y, feed = initial.feed, size = initial.size,inGame=False}



incNum : Float
incNum = 10

pow : Float -> Int -> Float
pow x num = case num of
                0 -> 1
                _ -> x*(pow x (num-1))

-- tuple of the center of the screen,mousePos,maxSpeed returns dx,dy
mouseSpeed : (Float,Float) ->(Int,Int)->Float -> (Float,Float)
mouseSpeed (x,y) (ma,mb) speed = let
        mx = Basics.toFloat ma
        my = Basics.toFloat mb
        rx = 0.01*(mx-x)
        ry = 0.01*(my-y)
        r = Basics.sqrt ((pow rx 2)+(pow ry 2))
        ds = if r > speed || Basics.isNaN r then speed else r
        theta = (Basics.atan2 ry rx)
        dx = ds*(Basics.cos theta)
        dy = ds*(Basics.sin theta)
    in (dx,dy)
--gets center of screen
scCenter : Model -> (Float,Float)
scCenter model = let
        x = toFloat model.winW
        y = toFloat model.winH
    in ((x/2),(y/2))
{-
--takes boundary, position
boundsCheck : Int -> Float ->Float -> Float
boundsCheck b pos rad = let 
                            bounds = Basics.toFloat b
                        in if pos>=bounds+rad then
                            pos-bounds-(2*rad)
                        else if pos <= -rad then
                            pos + bounds+(2*rad)
                        else
                            pos
-}
--alt boundary check
boundsCheck : Int -> Float -> Float -> Float
boundsCheck b pos rad = let
                            bounds = toFloat b
                        in if pos>bounds then 
                            bounds
                        else if pos<0 then 
                            0
                        else 
                            pos



genViewBox : Float -> (Float,Float)->Svg.Attribute msg
genViewBox rad (x,y)= let
        (vx,vy,vw,vh) = genVBox rad (x,y)
    in Svg.Attributes.viewBox (vx++" "++vy++" "++vw++" "++vh)

genVBox : Float -> (Float,Float) -> (String,String,String,String)
genVBox rad (x,y) =let
        factor = (2*rad)+26
    in ((x-factor|>toString),(y-factor|>toString),(factor*2|>toString),(factor*2|>toString))

svWidth : Int 
svWidth = 1300

svHeight : Int
svHeight = 600

--compute the radius based on the size
mr :Float -> Float 
mr i = Basics.sqrt <| (i/Basics.pi)*2

--wrapping
bCheckx : Float -> Float ->Float
bCheckx pos size = boundsCheck svWidth pos (mr size)

bChecky : Float -> Float -> Float
bChecky pos size = boundsCheck svHeight pos (mr size)

--render feed bits
buildFeeds: FeedBit -> Float -> List (Svg.Svg msg)
buildFeeds feed mult = case feed of -- literally just a bunch of tiny circles
                    (f::fs) -> (Svg.circle [cx (toString f.x),cy (toString f.y), r (toString <| mult*(mr f.value)), fill f.color][])::(buildFeeds fs mult)
                    []      -> []-- return empty list on list end



drawLines : List (Svg.Svg msg)
drawLines = (drawXLines 10 svHeight 0 svWidth [])++(drawYLines 10 svWidth 0 svHeight [])


drawXLines : Int-> Int-> Int -> Int -> List (Svg.Svg msg) -> List (Svg.Svg msg)
drawXLines inc len lx total lines= if lx>total then lines
                        else drawXLines inc len (lx+inc) total 
                                (line[x1 (Basics.toString lx),
                                    x2 (Basics.toString lx), 
                                    y1 "0",
                                    y2 (Basics.toString len), 
                                    Svg.Attributes.strokeWidth "1px", 
                                    Svg.Attributes.stroke "lightgrey"][]::lines)

drawYLines : Int-> Int-> Int -> Int -> List (Svg.Svg msg) -> List (Svg.Svg msg)
drawYLines inc len ly total lines= if ly>total then lines
                        else drawYLines inc len (ly+inc) total 
                                (line[y1 (Basics.toString ly),
                                y2 (Basics.toString ly), 
                                x1 "0",
                                x2 (Basics.toString len), 
                                Svg.Attributes.strokeWidth "1px", 
                                Svg.Attributes.stroke "lightgrey"][]::lines)

--update model consuming feeds that overlap with circle
testConsume: Model -> Model
testConsume model = let
        feeds = model.feed
        consumed = List.map (canConsume (mr model.size) model.x model.y) feeds -- messy mapping stuff to produce a boolean list
        both = wrap consumed feeds --wrap the two together lazily in a tuple
    in {model | feed = List.reverse <|filterOut both [], size = shrink <|consume both model.size}--update model

shrink:Float -> Float
--shrink size = size - (sqrt ((size-25) /10000000) ) -- basically shrink faster if larger, though at a slower rate, pretty much grow forever like this
--shrink size = size - (((size*size)-625) / 10000000) --shrink really fast when big, constant gen max ~ 2236 (can get higher if you game it), rng god gen max about 70711
shrink size = size - (((size*size)-625) / 50000000) --shrink really fast when big, constant gen max ~ 2236 (can get higher if you game it), rng god gen max about 70711

--lazy wrapping cause seriously no way to do this kind of thing without going into a bunch of messy case x of and Maybes
--Also the two lists should be the same size in the usage scenario
wrap : List a -> List b -> List (a,b)
wrap a b = case a of
        (n::ns) -> case b of 
                       (b::bs) -> (n,b)::(wrap ns bs)
                       []      -> []
        []      -> []

filterOut : List (Bool,Feed) -> List Feed -> List Feed
filterOut both feeds = case both of 
        ((True,_)::fs)  -> filterOut fs feeds -- consumed skip that feed item
        ((False,f)::fs) -> filterOut fs (f::feeds) -- merge 
        []              -> feeds
--list of whether can consumeor not > feed list to extract size > current size
consume: List (Bool,Feed) -> Float -> Float 
consume feeds size = case feeds of
        ((True,f)::fs)  -> consume fs (size+f.value)
        ((False,_)::fs) -> consume fs size
        []              -> size

-- ints are radius and position
canConsume: Float -> Float -> Float -> Feed -> Bool
canConsume r x y f= let
            distance = Basics.sqrt ((((toFloat f.x)-x)*((toFloat f.x)-x))+(((toFloat f.y)-y)*((toFloat f.y)-y)))
        in (distance<r + (mr f.value))


genFeed: RNG -> Int -> Int -> Model -> Model
genFeed rng a b model = if a<=rng.regChance
            then {model | feed = addFeed model.feed 5 (b%svWidth) (round <| toFloat b/(toFloat svWidth))} 
            else if a<=rng.regChance+rng.superChance then {model | feed = addFeed model.feed 10 (b%svWidth) (round<| toFloat b/(toFloat svWidth))}
            else model -- only gen on rand = 1
addFeed: FeedBit -> Float -> Int -> Int -> FeedBit
addFeed f v x y = {x=x, y=y,value=v,color=genColor (x+y)}::f

--semi random based on position
genColor : Int -> String
genColor x = let 
            y= x%14 --change to suit colors
        in 
            case y of 
                0 -> "red"
                1 -> "blue"
                2 -> "green"
                3 -> "yellow"
                4 -> "purple"
                5 -> "magenta"
                6 -> "orange"
                7 -> "lime"
                8 -> "cyan"
                9 -> "black"
                10 -> "teal"
                11 -> "grey"
                12 -> "brown"
                _ -> "white" -- the devil's dot, aka it can't be seen

tick : Float -> Model -> Model
tick tick model = let
        frames = model.tickFrames 

    in  if frames.count == 0 then 
            {model | tickFrames = {frames | first=tick,count=frames.count+1}}
        else if frames.count >= 20 then
            {model | tickRate = 20000 / (tick-frames.first), tickFrames = {frames | count=0}}
        else 
            {model | tickFrames = {frames | count=frames.count+1}}

updatePlayerDisplay: DUpdate -> Display
updatePlayerDisplay du = case du of 
                    (LS s) -> LS s
                    (RS s) -> RS {source=s}

update : Msg -> Model -> (Model, Cmd.Cmd Msg)
update msg model = let
            rng = model.rng
            rand = genRand rng
        in case msg of --wasd
            (KeyMsg k) -> if model.control==Keys then case k of 
                                87 -> (testConsume {model|y = bChecky (model.y-incNum) model.size},rand)
                                65 -> (testConsume {model|x = bCheckx (model.x-incNum) model.size},rand)
                                83 -> (testConsume {model|y = bChecky (model.y+incNum) model.size},rand)
                                68 -> (testConsume {model|x = bCheckx (model.x+incNum) model.size},rand)
                                --arrow keys
                                38 -> (testConsume {model|y = bChecky (model.y-incNum) model.size},rand)
                                37 -> (testConsume {model|x = bCheckx (model.x-incNum) model.size},rand)
                                40 -> (testConsume {model|y = bChecky (model.y+incNum) model.size},rand)
                                39 -> (testConsume {model|x = bCheckx (model.x+incNum) model.size},rand)
                                --reset the game if escape key is pressed
                                27 -> (resetGame model, Cmd.none)
                                _  -> (model,Cmd.none)
                            else case k of
                                27 -> (resetGame model,Cmd.none)
                                _  -> (model,Cmd.none)
            (Tick t) -> if model.control==Mouse then let
                                                    (dx,dy) = mouseSpeed (scCenter model) (model.mx,model.my) 2
                                                in (tick t <|testConsume {model| 
                                                    x = (bCheckx (model.x+dx) model.size), 
                                                    y = (bChecky (model.y+dy) model.size)},rand)
                                            else (tick t model,Cmd.none)
            (MouseMsg pos) -> {model| mx = pos.x,my=pos.y}![]
            (DispUpdate u) -> ({model | display = updatePlayerDisplay u},Cmd.none)
            (NameUpdate s) -> ({model | name = s},Cmd.none)
            (StartG)       -> ({model | inGame = True},Cmd.none)
            (Toggle s)     ->  case s of 
                                    "FPS"     -> {model | fps = Basics.not model.fps} ![]
                                    "minimap" -> {model | minimap = Basics.not model.minimap} ![]
                                    _         -> model ![]
            (UpdateWinSize s) -> ({model | winH = s.height, winW = s.width},Cmd.none)
            (RandResult (a,b)) -> if (List.length model.feed < rng.limit)  --limit so there's an upper bound to the size, over 1000 it starts getting slow
                                then (genFeed rng a b model,Cmd.none) 
                                else (model,Cmd.none)
            --_ -> (model,Cmd.none)

--genRand : RNG -> Cmd.Cmd Msg﻿
genRand rng = generate RandResult (Random.pair (int 1 rng.range) (int 1 (svWidth*svHeight)))

gameView : Model -> Html Msg -- view used for when the game is being played
gameView model = 
    let     
        posX = (toString model.x)
        posY = (toString model.y)
        feeds = buildFeeds model.feed
        pfill = case model.display of 
            (LS c) -> c
            (RS _) -> "url(#player)" -- use the pattern image
        pImage = case model.display of
            (LS _) -> image [][]
            (RS r) -> image [x "0%",y "0%",Svg.Attributes.height "5000",Svg.Attributes.width "5000",Svg.Attributes.xlinkHref r.source][]
        vBox = genViewBox (mr model.size) (model.x,model.y)
        gridlines = drawLines
        (vx1,vy1,vw,vh) = genVBox (mr model.size) (model.x,model.y)
    in div[Html.Attributes.style[("margin","0"),("padding","0"),("overflow","hidden")]][-- fixed positioning of SVG https://stackoverflow.com/questions/5643254/how-to-scale-svg-image-to-fill-browser-window
        svg[Html.Attributes.style[("position","fixed"),("top","0"),("left","0"),("height","100%"),("width","100%")],vBox](
            [--image in svg based on this https://stackoverflow.com/questions/29442833/svg-image-inside-circle
            defs [][
                Svg.pattern[Svg.Attributes.id "player", x "0%", y "0%", Svg.Attributes.height "100%", Svg.Attributes.width "100%", viewBox "0 0 5000 5000"][
                    pImage-- um... somehow if you set viewbox = image height/width it automatically sizes it for you... *throws away a bunch of code that hasn't been coded yet*
                    ]
                ]
            ]
            ++gridlines
            ++(feeds 1)
            ++[Svg.circle [cx posX,cy posY, r (toString <| mr model.size),fill pfill, stroke "black", Svg.Attributes.strokeWidth "1px"] []]
            ++[Svg.text_ [x posX, y posY, Svg.Attributes.textAnchor "middle",Svg.Attributes.alignmentBaseline "middle", Html.Attributes.style [
                    ("font-size",(toString <|( mr model.size)/2)++"px")-- dynamic scaling of text wrt size
                    ,("font-weight","bold")
                    ,("fill","white")
                    ,("fill-opacity","1")
                    ,("stroke","#000")
                    ,("stroke-width",(toString <| (mr model.size)/50)++"px")-- dynamic scaling of text wrt to size
                    ,("stroke-linecap","butt")
                    ,("stroke-linejoin","miter")
                    ,("stroke-opacity","1")
                    ,("font-family","Sans-Serif")]
                ][Svg.text model.name]]-- outlined text https://stackoverflow.com/questions/442164/how-to-get-an-outline-effect-on-text-in-svg
            --viewbox testing
            --++[Svg.rect [fill "none",Svg.Attributes.x vx1,Svg.Attributes.y vy1,Svg.Attributes.height vh,Svg.Attributes.width vh, Svg.Attributes.strokeWidth ".5px",Svg.Attributes.stroke "red"][]]
            )
        ,div[Html.Attributes.style[("position","fixed"),("bottom","1%"),("left","1%"),("color","white"),("background-color","darkgrey"),("opacity","0.7")]][
            strong[][Html.text ("Score: "++(toString <|round<| model.size-25))]
            ]
        ,div[Html.Attributes.hidden (not model.fps), Html.Attributes.style[("position","fixed"),("top","1%"),("left","1%"),("color","white"),("background-color","darkgrey"),("opacity","0.7")]][
            strong[][Html.text ("FPS: "++(toString <|round<| model.tickRate))]
            ]
        ,div[Html.Attributes.hidden (not model.minimap), Html.Attributes.style [("position","fixed"),("bottom","1%"),("right","1%"),("height","10%"),("width","10%")]][
            Svg.svg[Svg.Attributes.viewBox ("0 0 "++(toString svWidth)++" "++(toString svHeight))]([
                Svg.rect[Svg.Attributes.x "0",Svg.Attributes.y "0", Svg.Attributes.height <| toString svHeight,Svg.Attributes.width <| toString svWidth,Svg.Attributes.fill "white",Svg.Attributes.strokeWidth "1%", Svg.Attributes.stroke "black"][]
                ,Svg.rect [Svg.Attributes.x vx1, Svg.Attributes.y vy1, Svg.Attributes.width vw, Svg.Attributes.height vh,Svg.Attributes.fill pfill][]
                ]
                ++(feeds 5)
                )
            ]
        ]



radioStyle = Html.Attributes.style[("clear","both"),("margin","auto"),("text-align","unset")]

radioButton : String -> String -> String ->  Html Msg
radioButton color current display= label[radioStyle][
            input[Html.Attributes.checked (color==current)
                ,Html.Attributes.type_ "radio"
                ,Html.Attributes.name "colorPick"
                ,onClick (DispUpdate (LS color))][]
            ,Html.text display
        ]

preView : Model -> Html Msg
preView model = 
    let 
        current = case model.display of
                (LS s) -> s
                (RS s) -> ""
        img = case model.display of
                (LS s) -> ""
                (RS s) -> s.source
    in div [Html.Attributes.style[("margin","auto"),("text-align","center"),("height","80%"),("width","80%"),("background-color","lightgrey")]][
        div[][Html.text "Enter your name"]
        ,div[][Html.input [Html.Attributes.placeholder "name", Html.Events.onInput (\inp -> NameUpdate inp),radioStyle, Html.Attributes.value model.name][]]
        ,div [][Html.text "Enter the url for the image or choose a color from below"]
        ,div[][input [Html.Attributes.placeholder "Input Image URL", onInput (\inp ->DispUpdate (RS inp)),radioStyle, Html.Attributes.value img][]]
        ,div[][--div holding a bunch of stuff relating to the color
            radioButton "#ff0000" current "red",
            radioButton "#0000ff" current "blue",
            radioButton "#008000" current "green"
            ]
        ,div [][
            label [][
                Html.text "Color Picker:"
                ,input [Html.Attributes.type_ "color", Html.Attributes.value current, Html.Events.onInput (\inp -> DispUpdate (LS inp))][]
                
                ]
            ]
        ,div[][
            button[Html.Attributes.style [("background-color","green"),("color","white")],Html.Events.onClick StartG][Html.text "Start"]
            ]
        ,div[][
            strong[][Html.text "Instructions:"]
            ,p[][Html.text "Use the mouse move the circle around, press ESC to quit the game at any time"]
            ,p[][Html.text "The goal is eat smaller dots and grow. Try to get to 2000 points, and as a difficult challenge 2200"]
            ]
        ,div[][
            label [][input [Html.Attributes.type_ "checkbox", Html.Attributes.checked model.fps, onClick (Toggle "FPS")][], Html.text "FPS"]
            ,label [][input [Html.Attributes.type_ "checkbox", Html.Attributes.checked model.minimap, onClick (Toggle "minimap")][], Html.text "minimap"]
            ]
    ]

view : Model -> Html Msg
view model = case model.inGame of
    True -> gameView model
    False -> preView model

subscriptions : Model -> Sub.Sub Msg
subscriptions model = let
        mice = if model.control==Mouse then 
                    [Mouse.moves MouseMsg] 
               else 
                    []
        ani = if model.control==Mouse || model.fps then 
                    [Anim.times Tick] 
              else 
                    []
    in case model.inGame of
    True -> Sub.batch([Key.downs KeyMsg,Window.resizes UpdateWinSize]++ani++mice) -- only need keys during game
    False -> Sub.none --- listens to nothing due to nothing going on pre game

main : Program Never Model Msg
main = program
        {init = init,
        view = view,
        update = update,
        subscriptions = subscriptions}

--bookmark in case I need it to draw the border for custom img https://raw.githubusercontent.com/lokesh/color-thief/master/src/color-thief.js