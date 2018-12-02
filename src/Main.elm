import Browser
import Set
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

calculateFirstDuplicatedFrequency : Set.Set Int -> List Int -> Int -> Int
calculateFirstDuplicatedFrequency frequencyCombos frequenciesModifiers currentFrequency =
  case frequenciesModifiers of
    [] ->  0
    [x] ->
       Debug.log "one item" calculateFirstDuplicatedFrequency (Set.insert (currentFrequency + x) frequencyCombos) frequencyInput (currentFrequency + x)
    (x::xs) -> 
      if Set.member (currentFrequency + x) frequencyCombos then
        Debug.log "in if - currentFrequency" (currentFrequency + x)

      else
        Debug.log "in else" calculateFirstDuplicatedFrequency (Debug.log "acc" (Set.insert (currentFrequency + x) frequencyCombos)) xs (currentFrequency + x)
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
    , createText (calculateFirstDuplicatedFrequency Set.empty frequencyInput 0)
    ]