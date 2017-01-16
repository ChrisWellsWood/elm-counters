import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = (\_ -> Sub.none)
  }

init : ( Model, Cmd Msg )
init = ( 0, Cmd.none )

-- MODEL

type alias Model = Int


-- UPDATE


type Msg = Increment | Decrement | Clear

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    Increment ->
      ( model + 1, Cmd.none )

    Decrement ->
      ( model - 1, Cmd.none )
    
    Clear ->
      ( 0, Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [ countStyle ] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    , button [ onClick Clear ] [ text "Clear" ]
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
