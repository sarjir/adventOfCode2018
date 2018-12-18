import Browser
import Set exposing (Set)
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
-- starting : List Int -> (Set Int -> Int -> Int)
-- starting list
--   calculateFirstDuplicatedFrequency list

addFrequency : Int -> Int -> Int
addFrequency currentFrequency frequency =
  currentFrequency + frequency

updateFrequencySet : Set Int -> Int -> Set Int
updateFrequencySet frequencyCombos newFreq =
  Set.insert newFreq frequencyCombos

checkCombos : Set Int -> Int -> Bool
checkCombos frequencyCombos frequency =
  Set.member frequency frequencyCombos

calculateFirstDuplicatedFrequency : List Int -> Set Int -> Int -> Int
calculateFirstDuplicatedFrequency frequenciesModifiers frequencyCombos currentFrequency =
  let 
    createNewFreq = addFrequency currentFrequency
    updateCombos = updateFrequencySet frequencyCombos
    checkFrequencyCombos = checkCombos frequencyCombos
  in 
    case frequenciesModifiers of
      [] ->  0
      [x] ->
        calculateFirstDuplicatedFrequency frequencyInput (updateCombos (createNewFreq x)) (createNewFreq x)
      (x::xs) ->
        if checkFrequencyCombos (createNewFreq x) then
          createNewFreq x

        else
          calculateFirstDuplicatedFrequency xs (updateCombos (createNewFreq x)) (createNewFreq x)
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
    , createText (calculateFirstDuplicatedFrequency frequencyInput Set.empty 0)
    ]