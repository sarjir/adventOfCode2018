import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Frequency exposing (frequencyInput)

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model =
  { content: String
  }

init : Model
init = 
  { content = "" }

-- UPDATE
 
type Msg
  = Change String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }


-- VIEW

calculateFirstDuplicatedFrequency : List Int -> List Int -> Int -> Int
calculateFirstDuplicatedFrequency frequencyCombos frequenciesModifiers currentFrequency =
  case frequenciesModifiers of
    [] ->  0
    [x] ->
       calculateFirstDuplicatedFrequency (frequencyCombos ++ [(currentFrequency + x)]) [3, 3, 4, -2, -4] (currentFrequency + x)
    (x::xs) -> 
      if List.member (currentFrequency + x) frequencyCombos then
        currentFrequency + x

      else
        calculateFirstDuplicatedFrequency (frequencyCombos ++ [currentFrequency + x]) xs (currentFrequency + x)
        -- let _ = Debug.log "x is" x



calculateFrequency : List Int -> Int
calculateFrequency frequencies =
  List.sum frequencies

createText : Int -> Html Msg
createText finalNumber = 
  div [] [ text (String.fromInt finalNumber) ]

view : Model -> Html Msg
view model =
  div []
    [ text "First puzzle"
    , createText (calculateFrequency frequencyInput)
    , text "Second puzzle"
    , createText (calculateFirstDuplicatedFrequency [] [3, 3, 4, -2, -4] 0)
    ]