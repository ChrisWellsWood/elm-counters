import Dict
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
  { counterDict : CounterDict
  , currentCounterID : CounterID
  }

type alias CounterDict = Dict.Dict CounterID CounterModel

type alias CounterID = Int

emptyModel : Model
emptyModel =
  { counterDict = Dict.empty
  , currentCounterID = 0
  }


-- UPDATE


type Msg 
  = AddCounter
  | ModifyCounter CounterID CounterModifier
  | RemoveCounter CounterID

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    AddCounter ->
      let
        nextID = model.currentCounterID + 1
        newModel =
          { counterDict = Dict.insert nextID 0 model.counterDict
          , currentCounterID = nextID
          }
      in
        ( newModel, Cmd.none )
    
    ModifyCounter counterID modifier ->
      let
        clickedCounter = Dict.get counterID model.counterDict
      in
        case clickedCounter of
          Just counter ->
            ( { model | counterDict =
              Dict.insert counterID (modifyCounter modifier counter) model.counterDict }
            , Cmd.none )
          Nothing ->
            ( model, Cmd.none )
    
    RemoveCounter counterID ->
        ( { model | counterDict = Dict.remove counterID model.counterDict }, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [] [ button [ onClick AddCounter ] [ text "Add Counter" ] ]
    , div [] (List.map makeView (Dict.toList model.counterDict))
    ]

makeView : (CounterID, CounterModel) -> Html Msg
makeView (refID, counterModel) = 
  let
    counterConfig =
      config
        { modifyMsg = ModifyCounter refID
        , removeMsg = RemoveCounter refID
        }
  in
    viewCounter counterConfig counterModel
