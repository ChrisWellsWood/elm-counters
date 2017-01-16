import Html exposing (..)
import Html.Events exposing (onClick)

import ReusableCounter exposing (..)

main : Program Never Model Msg
main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = (\_ -> Sub.none)
  }

init : ( Model, Cmd Msg )
init = ( emptyModel, Cmd.none )

-- MODEL

type alias Model =
  { counterList : List CounterModel
  , currentCounterID : CounterID
  }

emptyModel : Model
emptyModel =
  { counterList = []
  , currentCounterID = 0
  }


-- UPDATE


type Msg 
  = AddCounter
  | ModifyCounter CounterModifier CounterID
  | RemoveCounter CounterID

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    AddCounter ->
      let
        nextID = model.currentCounterID + 1
        newModel =
          { counterList = ( newCounter nextID ) :: model.counterList
          , currentCounterID = nextID
          }
      in
        ( newModel, Cmd.none )
    
    ModifyCounter modifier counterID ->
      let
        counterList = List.map (updateCounterList modifier counterID) model.counterList
      in
        ( { model | counterList = counterList }, Cmd.none )
    
    RemoveCounter counterID ->
      let
        counterList = List.filter (\cntr -> cntr.refID /= counterID) model.counterList
      in
        ( { model | counterList = counterList }, Cmd.none )

updateCounterList : CounterModifier -> CounterID -> CounterModel -> CounterModel
updateCounterList modifier targetID counter =
  if targetID == counter.refID then
    modifyCounter modifier counter
  else
    counter

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [] [ button [ onClick AddCounter ] [ text "Add Counter" ] ]
    , div [] (List.map makeView model.counterList)
    ]

counterConfig : Config Msg
counterConfig =
  config
    { modifyMsg = ModifyCounter
    , removeMsg = RemoveCounter
    }

makeView : CounterModel -> Html Msg
makeView counterModel = viewCounter counterConfig counterModel
