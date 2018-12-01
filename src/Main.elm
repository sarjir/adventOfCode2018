import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Frequency exposing (frequencyInput)

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model = Int

init : Model
init = 
  0

-- UPDATE
 
type Msg = Increment | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1
    
    Decrement ->
      model - 1 


-- VIEW
calculateFrequency : List Int -> Int
calculateFrequency frequency =
  List.sum frequency

createText : Int -> Html Msg
createText finalNumber = 
  div [] [ text (String.fromInt finalNumber) ]

view : Model -> Html Msg
view model =
  div [] [ createText ( calculateFrequency frequencyInput) ]