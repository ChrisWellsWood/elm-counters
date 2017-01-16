module ReusableCounter exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


-- MODEL

type alias CounterModel =
  { currentCount : Int
  , refID : CounterID
  }

type alias CounterID = Int

newCounter : CounterID -> CounterModel
newCounter refID = CounterModel 0 refID

-- CONFIG

type Config msg =
  Config
    { modifyMsg : (CounterModifier -> CounterID -> msg)
    , removeMsg : (CounterID -> msg)
    }

config
  : { modifyMsg : (CounterModifier -> CounterID -> msg)
    , removeMsg : (CounterID -> msg)
    }
  -> Config msg
config { modifyMsg, removeMsg } =
  Config
    { modifyMsg = modifyMsg
    , removeMsg = removeMsg
    }

-- UPDATE

type CounterModifier = Increment | Decrement | Clear

modifyCounter : CounterModifier -> CounterModel -> CounterModel
modifyCounter counterModifier counterModel =
  case counterModifier of
    Increment -> { counterModel | currentCount = counterModel.currentCount + 1 }
    Decrement -> { counterModel | currentCount = counterModel.currentCount - 1 }
    Clear -> { counterModel | currentCount = 0 }


-- VIEW

viewCounter : Config msg -> CounterModel -> Html msg
viewCounter (Config { modifyMsg, removeMsg }) counterModel =
  div []
    [ button [ onClick (modifyMsg Decrement counterModel.refID) ] [ text "-" ]
    , div [ countStyle ] [ text (toString counterModel.currentCount) ]
    , button [ onClick (modifyMsg Increment counterModel.refID) ] [ text "+" ]
    , button [ onClick (modifyMsg Clear counterModel.refID) ] [ text "Clear" ]
    , button [ onClick (removeMsg counterModel.refID) ] [ text "Remove" ]
    ]

countStyle : Attribute msg
countStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    ]
