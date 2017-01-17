module ReusableCounter exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


-- MODEL

type alias CounterModel = Int

-- CONFIG

type Config msg =
  Config
    { modifyMsg : (CounterModifier -> msg)
    , removeMsg : msg
    }

config
  : { modifyMsg : (CounterModifier -> msg)
    , removeMsg : msg
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
    Increment -> counterModel + 1
    Decrement -> counterModel - 1
    Clear -> 0


-- VIEW

viewCounter : Config msg -> CounterModel -> Html msg
viewCounter (Config { modifyMsg, removeMsg }) counterModel =
  div []
    [ button [ onClick (modifyMsg Decrement) ] [ text "-" ]
    , div [ countStyle ] [ text (toString counterModel) ]
    , button [ onClick (modifyMsg Increment) ] [ text "+" ]
    , button [ onClick (modifyMsg Clear) ] [ text "Clear" ]
    , button [ onClick (removeMsg) ] [ text "Remove" ]
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
