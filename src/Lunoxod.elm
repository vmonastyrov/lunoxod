port module Lunoxod exposing (..)

import Html exposing (..)
import Html.Attributes as H exposing (..)
import Html.Events as E exposing (..)
import Debug
import Json.Decode as Json
import String exposing (split)
import Time exposing (Time, second)
import Lunomodel exposing (..)


main =
  Html.program { init = init
               , view = view
               , update = update
               , subscriptions = subscriptions}

-- MODEL

type alias Model = {
      started: Bool,
      uncons:   Float,
      planet:  Planet,
      ship:    Spaceship,  
      time:    Float,
      h: Float,
      u: Float,
      acc: Float,
      engine: Engine
    }

init : (Model, Cmd Msg)
init = (Model False 0 moon (Spaceship 2000 3660 0 []) 0 0 0 0 (Engine 0 1 False 0 False), Cmd.none)

-- UPDATE

run mm q time =
  let gp  = freeFall mm.planet mm.h
      r   = if (mm.engine.revers) then -1 else 1
      newAcc = q * mm.ship.c / totalMass mm.ship
      newU   = mm.u + (r * newAcc - gp) * time     
      newH   = mm.h + (mm.u + newU) * time / 2     
      newTime = mm.time + time
      newShip = takeFuel mm.ship (q * time)         
      engine  = mm.engine
      newEngine = if (newShip.fuel > mm.engine.mass)
                  then engine
                  else {engine | mass = newShip.fuel }
      newModel  = { mm | ship = newShip, h = newH, u = newU, time = newTime, acc  = newAcc, engine = newEngine }          
  in if ( mm.h == 0 && newH < mm.h)
       then {mm | ship = newShip, engine = newEngine}
       else newModel

type alias InitData = {fuel:Float, maxAcc: Float, weight: Float}
port dialog : Int -> Cmd msg
port finish : String -> Cmd msg

type Msg = ChangeTime String | Start | ChangeFuel String | ChangeRevers String
           | IncTime | DecTime| IncFuel | DecFuel | StartGame InitData | Tick Time

type State = NotStarted | Landed | Unconscious Float | Flying

getState: Model -> State
getState model = let pilot    = Maybe.withDefault (Cosmonaut "" "" 150 Pilot (3 * 9.81)) (List.head <| getPilot model.ship)
                     maxAcc   = pilot.maxAcceleration
                     divAcc   = model.acc - maxAcc  
                 in if (model.started == False) then NotStarted
                    else if (model.h < 0) then Landed
                        else if (divAcc > 0 ) then Unconscious divAcc
                             else Flying
                                  
calcNewModel model =
  let oEngine  = model.engine
      stTime   = toFloat oEngine.startedTime
      timeDiff = oEngine.time - stTime
      q        = oEngine.mass / oEngine.time
      exTime   = if ( timeDiff > 1 ) then 1 else timeDiff
  in if (oEngine.started /= True || stTime + 1 > oEngine.time) then run model 0 1
                     else let firstRun  = run model q exTime
                              secondRun = if (exTime /= 1) then run firstRun 0 (1 - exTime) else firstRun
                          in {secondRun | acc = firstRun.acc}

calcNewEngine model oEngine =
  let newModelEngine = model.engine
      stTime   = toFloat oEngine.startedTime
  in if (newModelEngine.started /= True) then model.engine
  else if (stTime + 1 < newModelEngine.time)
       then {newModelEngine | startedTime = oEngine.startedTime + 1}
       else {newModelEngine | startedTime = 0, started = False}

calcLanding model oEngine =
  let r  = if (oEngine.revers) then -1 else 1
      gp = freeFall model.planet 0
      q  = oEngine.mass / oEngine.time
      t  = 2 * model.h / (sqrt (model.u ^ 2 + 2 * model.h * (gp - model.acc * r)) - model.u)
  in run {model | ship = tank model.ship (abs <| t / oEngine.time * oEngine.mass)} q t

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =  
  let oEngine = model.engine
  in case msg of
        Tick time ->
            let newModel   = calcNewModel model
                newEngine  = calcNewEngine newModel oEngine
            in case (getState newModel) of
                 NotStarted -> (model, Cmd.none)
                 Landed     -> let finModel = calcLanding newModel oEngine    
                               in ({ finModel | h = 0, acc = 0, started = False }, finish (round2 finModel.u))
                 Unconscious divAcc -> ({newModel | engine = newEngine, uncons = divAcc}, dialog 1)
                 Flying -> if (model.uncons == 0) then ({newModel | engine = newEngine}, Cmd.none)
                           else if (model.uncons - 1 > 0)
                                then ({newModel | engine = newEngine, uncons = model.uncons - 1}, dialog 1)
                                else ({newModel | engine = newEngine, uncons = 0}, dialog 0)

        StartGame initData -> let cosmonaut = Cosmonaut "Vasilij" "Pupkin" initData.weight Pilot  initData.maxAcc
                                  newEngine = {oEngine | time = 1, mass = 0, started = False, revers = False, startedTime = 0}
                                  ship = takeFuel (addCosmonaut (unboard model.ship) cosmonaut) 3500
                              in  ({model | started = False, ship = tank ship initData.fuel, u = 0, h = 0, time = 0, acc = 0 }, Cmd.none)

        ChangeFuel value -> let val =  Result.withDefault model.ship.fuel <| String.toFloat value
                                newEngine = if (val > model.ship.fuel)
                                            then {oEngine | mass = model.ship.fuel}
                                            else {oEngine | mass = val}
                            in ({ model  | engine = newEngine}, Cmd.none)

        IncFuel -> let fuelMax = if (model.ship.fuel > 100) then 100 else model.ship.fuel
                       newEngine  = if (model.engine.mass + 1 > fuelMax)
                                    then {oEngine | mass = fuelMax }
                                    else {oEngine | mass = (model.engine.mass + 1)}
                   in ({ model  | engine = newEngine}, Cmd.none)

        DecFuel -> let newEngine = if (model.engine.mass - 1 < 0)
                                   then {oEngine | mass = 0}
                                   else {oEngine | mass = (model.engine.mass - 1)}
                   in ({ model  | engine = newEngine}, Cmd.none)

        IncTime -> let newEngine =  if (model.engine.time + 1 > 60)
                                    then {oEngine | time = 100}
                                    else {oEngine | time = (oEngine.time + 1)}
                   in ({ model  | engine = newEngine}, Cmd.none)

        DecTime -> let newEngine = if (model.engine.time - 1 < 0.7)
                                   then {oEngine | time = 0.7}
                                   else {oEngine | time = (model.engine.time - 1)}
                   in ({ model  | engine = newEngine}, Cmd.none)

        ChangeTime value -> let val = Result.withDefault 0.7 (String.toFloat value)
                                newEngine = {oEngine | time = val}
                            in  ({ model  | engine = newEngine}, Cmd.none)

        ChangeRevers value -> let newEngine =  if ( model.engine.revers ) 
                                               then {oEngine | revers = False }
                                               else {oEngine | revers = True }
                              in ({ model  | engine = newEngine}, Cmd.none)
        Start -> let newEngine = {oEngine | started = True}
                 in ({model | started = True, engine = newEngine}, Cmd.none)

-- VIEW


infoView model =
  let revers = if(model.engine.revers) then "revers" else ""
      srcImg = if (model.acc == 0 ) then "img/ship" ++ revers else "img/shipEngine" ++ revers 
  in div [class "col s4"] [
        div [class "row"] [ div [class "col s6"] [text "Время полета" ]
                ,div [class "col s6"] [b [] [text (round2 model.time)] , text " c." ]],
        div [class "row"] [ div [class "col s6"] [text "Топливо" ],
                div [class "col s6"] [b [] [text (round2 model.ship.fuel)] , text " кг." ]],
        div [class "row"] [ div [class "col s6"] [text "Высота" ],
                div [class "col s6"] [b [] [text (round2 model.h)] , text " м."] ],
        div [class "row"] [ div [class "col s6"] [text "Скорость" ],
                div [class "col s6"] [b [] [text (round2 model.u)] , text " м/с"] ],
        div [class "row"] [ div [class "col s12 offset-s6"] [img [src <| srcImg ++ ".png"] []]]
                
    ]
massEngineView model =
  let newModel = Debug.log "massEngineView" model 
  in  div [class "row valign-wrapper"] [
         div [class "col s2 valign chip"]
             [ text "Расход: "
             , b [class "large"] [text (toString model.engine.mass)]
             , text " кг."]
       , div [ class "col s10 valign"]
             [ div [class "row valign-wrapper"]
                   [ div [class "col s1 valign"]
                         [a [class "btn-floating", onClick DecFuel] [i [class "material-icons"][text "fast_rewind"]]]
                   , div [class "col s1 valign"] [text "0"]
                   , div [class "col s8 valign"]
                         [ p [class "range-field"]
                             [input [ type_ "range"
                                        , H.min "0"
                                        , H.max "100"
                                        , value <| toString model.engine.mass
                                        , on "change" (Json.map ChangeFuel targetValue) 
                                    ] []
                             ]
                         ]
                  , div [class "col s1 valign"] [text "100"]
                  , div [class "col s1 valign"][a [class "btn-floating", onClick IncFuel] [i [class "material-icons"][text "fast_forward"]]]
                  ]
           ]
   ]

timeEngineView model = div [class "row valign-wrapper"] [
  div [class "col s2 valign chip"]
    [text "За время: "
    ,b [] [text (toString model.engine.time)]
    ,text " c."]
  , div [class "col s10 valign"] [
      div [class "row valign-wrapper"]
        [ div [class "col s1 valign"]
              [a [class "btn-floating", onClick DecTime] [i [class "material-icons"][text "fast_rewind"]]]
          , div [class "col s1 valign"] [text "0.7"]
          , div[class "col s8 valign"]
            [p [class "range-field"]
                [ input [ type_ "range"
                        , H.min "0.7"
                        , H.max "60"
                        , H.step "0.1"
                        , value <| toString model.engine.time
                        , on "change" (Json.map ChangeTime targetValue) 
                    ] []
                ]
            ]
          , div [class "col s1 valign"] [text "60"]
          , div [class "col s1 valign"][a [class "btn-floating", onClick IncTime] [i [class "material-icons"][text "fast_forward"]]]
       ]
    ]
 ]

reversEngineView model = div [class "row"] [
    div [class "col s2 chip"] [text "Реверс тяги"]
  , div [class "col s10"]
    [ div [class "switch"]
      [ label []
         [   text "Выкл."
           , input
               [  type_ "checkbox"
                , checked model.engine.revers
                , on "change" (Json.map ChangeRevers targetValue)]
               []
        , span [class "lever"] []
        , text "Вкл."]
     ]
    ]
  ]
  
engineNotActive model = (model.started == False && model.u /= 0) || (model.started == False &&  model.engine.mass == 0) || model.acc /= 0 
  
startEngineView model =
  let defaultClass = "waves-effect waves-light btn-large red"
      btnClass = if (engineNotActive model) then defaultClass ++ " disabled" else defaultClass
  in                          
    div [class "row"]
    [ div [class "col s12"]
        [button
            [ onClick Start
             , class btnClass
             , disabled <| engineNotActive model
            ]
            [ text "ПУСК" ]
        ]
    ]
  
view : Model -> Html Msg
view model =
  div [class "row"] [
     infoView model
     , div [class "col s8"]
       [ massEngineView model
       , timeEngineView model
       , reversEngineView model
       , startEngineView model
       ]
  ] 

-- SUBSCRIPTIONS

port startGame : (InitData -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [ startGame StartGame
                                  , Time.every second Tick
                                ]
